use std::rc::Rc;
use crate::encoder::{Encoder, EventRef};
use crate::encoder::pending::PendingValue;
use crate::options::Alignment;
use crate::qname::QName;
use crate::schema::{SchemaInfo, TypeDefinition};
use crate::string_table::{ValueResult, bits_for_value};
use crate::{Error, Result};

impl Encoder {

    /// Encodiert einen String (Spec 7.1.10).
    ///
    /// Bei ByteAlignment ist der String bereits byte-aligned (Unsigned Integers sind octet-basiert).
    pub(super) fn encode_string(&mut self, value: &str) {
        crate::string::encode(&mut self.writer, value);
    }

    /// Encodiert einen String mit Längen-Offset (Spec 7.3.3).
    pub(super) fn encode_string_with_length_offset(&mut self, value: &str, offset: u64) {
        if value.is_ascii() {
            // ASCII: len() == char count, Bulk-Write statt per-Codepoint
            crate::unsigned_integer::encode(&mut self.writer, value.len() as u64 + offset);
            self.writer.write_bytes_aligned(value.as_bytes());
        } else {
            crate::string::encode_non_ascii(&mut self.writer, value, offset);
        }
    }

    /// Encodiert seltene Events (CM, PI, DT, ER) ohne String-Table-Integration.
    ///
    /// Konvertiert EventRef zu ExiEvent und delegiert an encode_event (Spec 7).
    /// Wird nur fuer Fidelity-Events aufgerufen, nie fuer SE/EE/AT/CH/NS.
    pub(super) fn encode_rare_event_content(&mut self, event: &EventRef<'_>) {
        let exi_event = event.to_exi_event_for_content();
        let ctx = crate::event_content::ContentContext::new(
            self.options.preserve.prefixes,
            0,
            self.options.effective_alignment() != Alignment::BitPacked,
        );
        crate::event_content::encode_event(&mut self.writer, &exi_event, &ctx);
    }

    /// Encodiert einen Value mit String Table (Spec 7.3.3).
    ///
    /// Value Encoding Format:
    /// - 0 = Local Hit → n-bit Compact ID (lokale Partition)
    /// - 1 = Global Hit → m-bit Compact ID (globale Partition)
    /// - length+2 = Miss → String Characters
    ///
    /// Bei PreCompression (Spec 9): Values werden gepuffert und am Ende
    /// in Value Channels geschrieben.
    /// Encodiert einen Value mit optionaler TypeDefinition.
    ///
    /// # Parameter
    ///
    /// - `qname`: QName des Elements/Attributs (für String Table Partitionierung)
    /// - `value`: Der zu encodierende Wert
    /// - `type_def`: Optionale TypeDefinition für Typed Value Encoding
    ///
    /// # Spec-Referenz
    ///
    /// - Spec 7.1: Typed Values werden mit spezifischem Encoding encodiert
    /// - Spec 7.3: String Table nur für String-Werte (Schema-less oder String-Typ)
    ///
    /// # Errors
    ///
    /// Bei `strict=true` und ungültigem Wert wird `Error::InvalidValue` zurückgegeben.
    pub(super) fn encode_value(&mut self, qname: &QName, value: &str, type_def: Option<Rc<TypeDefinition>>) -> Result<()> {
        // Spec 8.5.4.4 + 9.2.1: xsi:type Value ist IMMER als QName kodiert
        // und bleibt im Structure Channel.
        if qname.is_xsi_type() {
            return self.encode_xsi_type_value(value);
        }

        // Bei Compression/PreCompression: Value puffern statt inline schreiben (Spec 9)
        // Channel-Index statt voller QName: spart ~52 Bytes pro Value bei ~800K Values.
        if self.options.compression
            || matches!(self.options.alignment, Alignment::PreCompression)
        {
            let channel_idx = {
                let next_idx = self.pending_channel_qnames.len() as u16;
                *self.pending_channel_index
                    .entry(qname.identity_hash())
                    .or_insert_with(|| {
                        self.pending_channel_qnames.push(qname.clone());
                        next_idx
                    })
            };
            let str_offset = self.pending_string_buffer.len();
            self.pending_string_buffer.extend_from_slice(value.as_bytes());
            // Auch im Compression/PreCompression Pfad MemoryMonitor nutzen,
            // damit sehr grosse Pending-Buffers frueh abgebrochen werden.
            self.string_table.check_memory(value.len() as u64)?;
            let str_len = value.len() as u32;
            let type_idx = if let Some(td) = type_def {
                let ptr = Rc::as_ptr(&td) as usize;
                let next_idx = (self.pending_type_defs.len() + 1) as u16;
                *self.pending_type_ptr_index
                    .entry(ptr)
                    .or_insert_with(|| {
                        self.pending_type_defs.push(td);
                        next_idx
                    })
            } else {
                0
            };
            self.pending_values.push(PendingValue {
                str_offset, str_len, channel_idx, type_idx,
            });
            self.current_block_value_count += 1;

            // Spec 9.1: Multi-Block - Block-Grenze NACH dem Event setzen, das den
            // blockSize-ten Value produziert. So gehören alle Events bis einschließlich
            // diesem Event zum aktuellen Block, und das nächste Event startet einen
            // neuen Block (egal ob mit oder ohne Value).
            if self.current_block_value_count >= self.options.block_size as usize
                && self.options.block_size > 0
            {
                // Block-Grenze speichern: (structure_byte_pos, values_start_index)
                // Position ist RELATIV zum Header-Ende (nicht absolut), damit sie
                // direkt als Index in structure_data verwendet werden kann.
                let absolute_pos = self.writer.bit_position().div_ceil(8);
                let structure_pos = absolute_pos - self.header_end_byte;
                let values_start = self.pending_values.len();
                self.block_boundaries.push((structure_pos, values_start));
                self.current_block_value_count = 0;
            }
            return Ok(());
        }

        self.encode_value_typed_or_string(qname, value, type_def.as_deref())
    }

    /// Encodiert einen Value als Typed Value (wenn TypeDefinition vorhanden) oder String.
    ///
    /// # Errors
    ///
    /// Bei `strict=true` und ungültigem Wert wird `Error::InvalidValue` zurückgegeben.
    /// Bei `strict=false` wird bei ungültigem Wert zu String-Encoding gewechselt.
    pub(super) fn encode_value_typed_or_string(
        &mut self,
        qname: &QName,
        value: &str,
        type_def: Option<&TypeDefinition>,
    ) -> Result<()> {
        // Byte-Alignment: Werte werden auf Byte-Grenze ausgerichtet (Spec 7.1)
        if matches!(
            self.options.effective_alignment(),
            Alignment::ByteAlignment | Alignment::PreCompression
        ) {
            self.writer.align_to_byte();
        }

        if let Some(td) = type_def {
            // DTRM-Auflösung VOR String/Typed-Verzweigung (Spec 7.4)
            let dtrm = &self.options.datatype_representation_map;
            if !dtrm.is_empty()
                && !self.options.preserve.lexical_values
                && self.schema.is_some()
            {
                use crate::typed_value::EffectiveRepresentation;
                let schema_ref = self.schema.as_deref();
                if let Some(eff) = crate::typed_value::resolve_effective_repr(td, dtrm, schema_ref) {
                    return match eff {
                        EffectiveRepresentation::String => {
                            self.encode_value_inline(qname, value)
                        }
                        EffectiveRepresentation::Typed(base) => {
                            let strict = self.options.strict;
                            let alignment = self.options.effective_alignment();
                            if strict {
                                crate::typed_value::encode_by_base_type(
                                    &mut self.writer, value, base, strict, alignment,
                                )
                            } else {
                                let checkpoint = self.writer.save_checkpoint();
                                if crate::typed_value::encode_by_base_type(
                                    &mut self.writer, value, base, strict, alignment,
                                ).is_err() {
                                    self.writer.restore_checkpoint(checkpoint);
                                    self.encode_value_inline(qname, value)?;
                                } else {
                                    self.writer.discard_checkpoint();
                                }
                                Ok(())
                            }
                        }
                        EffectiveRepresentation::UserDefined(qn) => {
                            let alignment = self.options.effective_alignment();
                            if let Some((encode, _)) = crate::typed_value::get_user_defined_representation(&qn) {
                                encode(&mut self.writer, value, alignment)
                            } else {
                                Err(Error::UnsupportedDatatypeRepresentation(qn.to_string()))
                            }
                        }
                    };
                }
            }

            // Spec 7.2: Enumeration-Typen als n-Bit Index encodieren
            // (ausser Union, List, QName, Notation — eigene Codecs;
            //  ausser Enums mit > 4096 Werten — Fallthrough zu normalem Typed Value;
            //  ausser bei Preserve.lexicalValues — dann als String für lexikalische Form)
            let enum_vals = td.enumeration_values();
            let base_type = crate::typed_value::resolve_base_type(td);
            let is_enum_type = !enum_vals.is_empty()
                && enum_vals.len() <= 4096
                && !self.options.preserve.lexical_values
                && !td.is_union()
                && !matches!(td, TypeDefinition::Simple {
                    variety: crate::schema::SimpleTypeVariety::List { .. }, ..
                })
                && !matches!(base_type, Some("QName") | Some("NOTATION"));

            if is_enum_type {
                let norm_value = crate::typed_value::normalize_enum_value(value, base_type);
                if let Some(idx) = enum_vals.iter().position(|v| {
                    crate::typed_value::normalize_enum_value(v, base_type) == norm_value
                }) {
                    crate::enumeration::encode(&mut self.writer, idx, enum_vals.len())?;
                    return Ok(());
                }
                // Wert nicht in Enum-Liste: im strict-Modus Fehler, sonst String-Fallback
                if self.options.strict {
                    return Err(Error::InvalidValue(format!(
                        "Wert '{}' nicht in Enumeration ({} Werte)", value, enum_vals.len()
                    )));
                }
                if self.trace.event_code {
                    eprintln!(
                        "encoder: enum fallback für {:?} nicht in Enum-Liste ({} Werte), encodiere als String",
                        value, enum_vals.len()
                    );
                }
                self.encode_value_inline(qname, value)?;
                return Ok(());
            }

            // Bestehende Logik (kein DTRM-Override, kein Enumeration)
            if crate::typed_value::is_string_base_type(base_type) {
                // String-Typ: String Table mit Length+2 bei Miss
                self.encode_value_inline(qname, value)?;
            } else {
                // Nicht-String-Typ: Typed Value Encoding (Integer, Boolean, etc.)
                // Spec 7.1: Wenn der Wert nicht zum deklarierten Typ passt,
                // wird er als String encodiert (Deviated Type Representation).
                let strict = self.options.strict;
                let alignment = self.options.effective_alignment();
                if strict {
                    crate::typed_value::encode_typed_value(&mut self.writer, value, td, strict, alignment)?;
                } else {
                    let checkpoint = self.writer.save_checkpoint();
                    if crate::typed_value::encode_typed_value(&mut self.writer, value, td, strict, alignment).is_err() {
                        self.writer.restore_checkpoint(checkpoint);
                        self.encode_value_inline(qname, value)?;
                    } else {
                        self.writer.discard_checkpoint();
                    }
                }
            }
        } else {
            // Schema-less: String Table Encoding
            self.encode_value_inline(qname, value)?;
        }
        Ok(())
    }

    /// Parst einen xsi:type-Wert zu einem QName.
    ///
    /// Unterstuetzte Formate:
    /// - `{URI}localname` (Clark-Notation, vom XML-Parser voraufgeloest)
    /// - `prefix:localname` (Prefix wird via in-scope NS-Bindings aufgeloest)
    /// - `localname` (URI wird via Default-Namespace oder Schema aufgeloest)
    ///
    /// `ns_bindings`: In-scope Namespace-Bindings (prefix, URI) aus NS-Events.
    pub(super) fn parse_xsi_type_qname(
        value: &str,
        schema: Option<&SchemaInfo>,
        ns_bindings: &[(Rc<str>, Rc<str>)],
    ) -> QName {
        // Clark-Notation: {URI}localname (vom XML-Parser voraufgeloest)
        if value.starts_with('{')
            && let Some(end) = value.find('}') {
                return QName::new(&value[1..end], &value[end + 1..]);
            }

        // Parse "prefix:localname" oder "localname"
        let (prefix, local_name) = if let Some(pos) = value.find(':') {
            (Some(&value[..pos]), &value[pos + 1..])
        } else {
            (None, value)
        };

        // 1. URI via in-scope NS-Bindings auflösen (wie XML-Namespace-Spec).
        //    Unpräfixierte QName-Werte nutzen den Default-Namespace (prefix="").
        //    Präfixierte QName-Werte nutzen den entsprechenden Prefix-Binding.
        let lookup_prefix = prefix.unwrap_or("");
        if let Some((_, uri)) = ns_bindings.iter().find(|(p, _)| &**p == lookup_prefix) {
            return match prefix {
                Some(pfx) => QName::with_prefix(Rc::clone(uri), local_name, pfx),
                None => QName::new(Rc::clone(uri), local_name),
            };
        }

        // 2. Fallback: nur fuer unpraefixierte Werte — URI aus Schema-Typdefinitionen
        //    aufloesen. Bei explizitem Prefix, das nicht in NS-Bindings aufgeloest
        //    werden konnte, ist der Prefix ungueltig → uri="", localName=voller Wert
        //    (Spec: unbound prefix wird nicht aufgeloest).
        if prefix.is_none() {
            let resolved_uri = schema.and_then(|s| {
                s.type_definitions()
                    .keys()
                    .find(|tq| &*tq.local_name == local_name)
                    .map(|tq| tq.uri.clone())
            });

            if let Some(uri) = resolved_uri {
                return QName::new(uri, local_name);
            }
        }

        // Unbekannter Typ oder ungeloester Prefix → uri="", localName = voller Wert
        QName::new("", value)
    }

    /// Encodiert einen Value direkt (inline).
    pub(super) fn encode_value_inline(&mut self, qname: &QName, value: &str) -> Result<()> {

        let (result, global_size, local_size) = self.string_table.encode_value(qname, value)?;

        match result {
            ValueResult::HitLocal(id) => {
                // 0 als Unsigned Integer, dann n-bit Compact ID
                crate::unsigned_integer::encode(&mut self.writer, 0);
                let n = bits_for_value(local_size);
                self.encode_n_bit(id as u64, n);
            }
            ValueResult::HitGlobal(id) => {
                // 1 als Unsigned Integer, dann m-bit Compact ID
                crate::unsigned_integer::encode(&mut self.writer, 1);
                let n = bits_for_value(global_size);
                self.encode_n_bit(id as u64, n);
            }
            ValueResult::Miss => {
                // length+2 als Unsigned Integer, dann String Characters
                self.encode_string_with_length_offset(value, 2);
            }
        }
        Ok(())
    }

    pub(super) fn encode_xsi_nil_value(&mut self, value: &str) -> Result<()> {
        let bool_value = match value {
            "true" | "1" => true,
            "false" | "0" => false,
            _ => {
                return Err(Error::InvalidValue(format!(
                    "Invalid xsi:nil value: {value}"
                )))
            }
        };

        match self.options.effective_alignment() {
            Alignment::ByteAlignment | Alignment::PreCompression => {
                self.writer.align_to_byte();
                crate::boolean::encode_byte_aligned(&mut self.writer, bool_value);
            }
            Alignment::BitPacked => {
                crate::boolean::encode(&mut self.writer, bool_value);
            }
        }

        Ok(())
    }
}
