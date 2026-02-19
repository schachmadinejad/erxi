use std::rc::Rc;
use crate::decoder::Decoder;
use crate::options::Alignment;
use crate::qname::QName;
use crate::schema::TypeDefinition;
use crate::string_table::bits_for_value;
use crate::{Error, Result};

impl<'a> Decoder<'a> {

    /// Decodiert einen String mit bekannter Länge (in Unicode Code Points).
    ///
    /// ASCII-Fast-Path: Wenn byte-aligned und alle nächsten `length` Bytes MSB=0,
    /// direkte Konvertierung ohne per-Codepoint Decode.
    pub(super) fn decode_string_of_length(&mut self, length: usize) -> Result<String> {
        // ASCII Fast-Path: direkte Konvertierung wenn byte-aligned und alle Bytes < 128
        if let Some(bytes) = self.reader.peek_aligned_bytes(length) {
            if bytes.iter().all(|&b| b & 0x80 == 0) {
                let s = std::str::from_utf8(bytes)
                    .expect("ASCII-Bytes sind valides UTF-8")
                    .to_string();
                self.reader.skip_aligned_bytes(length);
                return Ok(s);
            }
        }
        // Fallback: Codepoint-by-Codepoint mit Inline Single-Byte Fast-Path
        // Kapazität: length * 2 für typische BMP-Texte (1-3 UTF-8 Bytes/Codepoint),
        // begrenzt durch verbleibende Bytes (Schutz gegen korrupte length-Werte).
        let remaining = self.reader.remaining_bits() / 8;
        let mut s = String::with_capacity(length.saturating_mul(2).min(remaining));
        for _ in 0..length {
            // Inline Single-Byte: spart Funktionsaufruf-Overhead pro Zeichen
            let byte = self.reader.read_byte_aligned()?;
            if byte & 0x80 == 0 {
                s.push(byte as char);
            } else {
                // Multi-Byte Unsigned Integer: restliche Bytes lesen
                let mut result = u64::from(byte & 0x7F);
                let mut shift: u32 = 7;
                loop {
                    let b = self.reader.read_byte_aligned()?;
                    let data = u64::from(b & 0x7F);
                    if shift == 63 && (data > 1 || b & 0x80 != 0) {
                        return Err(Error::IntegerOverflow);
                    }
                    result |= data << shift;
                    if b & 0x80 == 0 {
                        break;
                    }
                    shift += 7;
                }
                let ch = u32::try_from(result)
                    .ok()
                    .and_then(char::from_u32)
                    .ok_or_else(|| Error::InvalidCodePoint(result))?;
                s.push(ch);
            }
        }
        Ok(s)
    }

    /// Decodiert einen Value als Typed Value (wenn TypeDefinition vorhanden) oder String.
    ///
    /// # Spec-Referenz
    /// - Spec 7.1: Bei byte-aligned werden Werte auf Byte-Grenze ausgerichtet
    pub(super) fn decode_value_typed_or_string(&mut self, qname: &QName, type_def: Option<&TypeDefinition>) -> Result<Rc<str>> {
        // Byte-Alignment: Werte werden auf Byte-Grenze ausgerichtet (Spec 7.1)
        if matches!(
            self.options.effective_alignment(),
            Alignment::ByteAlignment | Alignment::PreCompression
        ) {
            self.reader.align_to_byte();
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
                            self.decode_value(qname)
                        }
                        EffectiveRepresentation::Typed(base) => {
                            let alignment = self.options.effective_alignment();
                            if self.options.strict {
                                crate::typed_value::decode_by_base_type(
                                    &mut self.reader, base, alignment,
                                ).map(Into::into)
                            } else {
                                let saved_reader = self.reader;
                                match crate::typed_value::decode_by_base_type(
                                    &mut self.reader, base, alignment,
                                ) {
                                    Ok(val) => Ok(val.into()),
                                    Err(_) => {
                                        self.reader = saved_reader;
                                        self.decode_value(qname)
                                    }
                                }
                            }
                        }
                        EffectiveRepresentation::UserDefined(qn) => {
                            let alignment = self.options.effective_alignment();
                            if let Some((_, decode)) = crate::typed_value::get_user_defined_representation(&qn) {
                                decode(&mut self.reader, alignment).map(Into::into)
                            } else {
                                Err(Error::UnsupportedDatatypeRepresentation(qn.to_string()))
                            }
                        }
                    };
                }
            }

            // Spec 7.2: Enumeration-Typen als n-Bit Index decodieren
            // (ausser Union, List, QName, Notation — eigene Codecs;
            //  ausser Enums mit > 4096 Werten;
            //  ausser bei Preserve.lexicalValues — dann als String)
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
                if self.options.strict {
                    let idx = crate::enumeration::decode(&mut self.reader, enum_vals.len())?;
                    return Ok(enum_vals[idx].clone().into());
                }
                let saved_reader = self.reader;
                match crate::enumeration::decode(&mut self.reader, enum_vals.len()) {
                    Ok(idx) => return Ok(enum_vals[idx].clone().into()),
                    Err(_) => {
                        self.reader = saved_reader;
                        return self.decode_value(qname);
                    }
                }
            }

            // Bestehende Logik (kein Enumeration, kein DTRM-Override)
            if crate::typed_value::is_string_base_type(base_type) {
                // String-Typ: String Table mit Length+2 bei Miss
                self.decode_value(qname)
            } else {
                // Nicht-String-Typ: Typed Value Decoding (Integer, Boolean, etc.)
                // Spec 7.1: Wenn der Wert nicht zum deklarierten Typ passt,
                // ist er als String encodiert (Deviated Type Representation).
                let alignment = self.options.effective_alignment();
                if self.options.strict {
                    crate::typed_value::decode_typed_value(&mut self.reader, td, alignment)
                        .map(Into::into)
                } else {
                    let saved_reader = self.reader; // BitReader ist Copy
                    match crate::typed_value::decode_typed_value(&mut self.reader, td, alignment) {
                        Ok(val) => Ok(val.into()),
                        Err(_) => {
                            self.reader = saved_reader;
                            self.decode_value(qname)
                        }
                    }
                }
            }
        } else {
            // Schema-less: String Table Decoding
            self.decode_value(qname)
        }
    }

    /// Decodiert einen Value mit String Table (Spec 7.3.3).
    ///
    /// Value Encoding Format:
    /// - 0 = Local Hit → n-bit Compact ID (lokale Partition)
    /// - 1 = Global Hit → m-bit Compact ID (globale Partition)
    /// - length+2 = Miss → String Characters
    pub(super) fn decode_value(&mut self, qname: &QName) -> Result<Rc<str>> {
        let trace_attr = self.trace.attr;
        let before = if trace_attr {
            Some(self.bit_position())
        } else {
            None
        };
        let indicator = crate::unsigned_integer::decode(&mut self.reader)?;

        match indicator {
            0 => {
                // Local Hit: n-bit Compact ID
                // size_for_bits statt count wegen möglicher Evictions bei bounded Partitions
                let local_size = self.string_table.local_value_size_for_bits(qname);
                let n = bits_for_value(local_size);
                let compact_id = self.decode_n_bit(n)? as usize;
                if trace_attr {
                    eprintln!(
                        "decode_value: qname={:?} indicator=0 local_size={} n_bits={} compact_id={} bits {} -> {}",
                        qname,
                        local_size,
                        n,
                        compact_id,
                        before.unwrap_or(0),
                        self.bit_position()
                    );
                }
                let value = self
                    .string_table
                    .decode_value_hit_local_rc(qname, compact_id)?;
                Ok(value)
            }
            1 => {
                // Global Hit: m-bit Compact ID
                // size_for_bits statt count wegen möglicher Evictions bei bounded Partitions
                let global_size = self.string_table.global_value_size_for_bits();
                let n = bits_for_value(global_size);
                let compact_id = self.decode_n_bit(n)? as usize;
                if trace_attr {
                    eprintln!(
                        "decode_value: qname={:?} indicator=1 global_size={} n_bits={} compact_id={} bits {} -> {}",
                        qname,
                        global_size,
                        n,
                        compact_id,
                        before.unwrap_or(0),
                        self.bit_position()
                    );
                }
                let value = self.string_table.decode_value_hit_global_rc(compact_id)?;
                Ok(value)
            }
            n => {
                // Miss: length = n - 2, dann String Characters
                let length = (n - 2) as usize;
                if trace_attr {
                    eprintln!(
                        "decode_value: qname={:?} indicator={} length={} bits {} -> {} (before chars)",
                        qname,
                        n,
                        length,
                        before.unwrap_or(0),
                        self.bit_position()
                    );
                }
                let value = self.decode_string_of_length(length)?;
                if trace_attr {
                    eprintln!(
                        "decode_value: qname={:?} length={} bits {} -> {} (after chars)",
                        qname,
                        length,
                        before.unwrap_or(0),
                        self.bit_position()
                    );
                }
                self.string_table.decode_value_miss(qname, &value)?;
                Ok(value.into())
            }
        }
    }

    /// Dekodiert den Wert eines xsi:nil Attributs (Spec 8.5.4.4.2).
    ///
    /// Der Wert ist als Boolean (7.1.2) codiert, nicht als String.
    /// Gibt "true" oder "false" zurück.
    pub(super) fn decode_xsi_nil_value(&mut self) -> Result<Rc<str>> {
        let trace = self.trace.xsi_nil;
        let before = if trace { Some(self.bit_position()) } else { None };
        // Boolean dekodieren (Spec 7.1.2, alignment-abhängig)
        let nil_value = match self.options.effective_alignment() {
            Alignment::ByteAlignment | Alignment::PreCompression => {
                crate::boolean::decode_byte_aligned(&mut self.reader)?
            }
            Alignment::BitPacked => crate::boolean::decode(&mut self.reader)?,
        };
        if let Some(start) = before {
            eprintln!(
                "decode_xsi_nil: bits {} -> {} value={}",
                start,
                self.bit_position(),
                nil_value
            );
        }
        Ok(if nil_value { Rc::from("true") } else { Rc::from("false") })
    }
}
