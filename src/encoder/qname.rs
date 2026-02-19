use crate::encoder::{Encoder, EventRef};
use crate::grammar::{AttributeKind, GrammarType, StartElementKind, Terminal};
use crate::options::Alignment;
use crate::qname::QName;
use crate::string_table::{CompactIdResult, StringLiteralResult, bits_for_value};
use crate::Result;

impl Encoder {

    /// Encodiert den QName-Anteil eines SE/AT-Events (Spec 7.1.7, 8.4.3, 8.5.1).
    ///
    /// Bei Wildcard-Match: vollstaendiger QName (oder nur local-name bei uri:*).
    /// Bei Nicht-Wildcard + preserve.prefixes: nur den Prefix encodieren.
    /// Bei Nicht-Wildcard ohne preserve.prefixes: nichts encodieren (QName implizit).
    pub(super) fn encode_qname_for_event(
        &mut self,
        qname: &QName,
        terminal: &Terminal,
        was_wildcard: bool,
    ) -> Result<()> {
        if was_wildcard {
            // Namespace-URI aus dem Terminal extrahieren (SE(uri:*) oder AT(uri:*))
            let implied_uri_id = match terminal {
                Terminal::StartElement(StartElementKind::NamespaceWildcard(uri))
                | Terminal::Attribute(AttributeKind::NamespaceWildcard(uri)) => Some(*uri),
                _ => None,
            };
            if let Some(uri_id) = implied_uri_id {
                let uri_str = self.interner.resolve(uri_id).to_owned();
                self.encode_qname_with_implied_uri(qname, &uri_str)?;
            } else {
                self.encode_qname(qname)?;
            }
        } else if self.options.preserve.prefixes
            && let Some(uri_id) = self.string_table.lookup_uri(&qname.uri) {
                self.encode_prefix(uri_id, qname);
            }
        Ok(())
    }

    /// Encodiert den Content eines Events (Document-Kontext).
    ///
    /// # Spec-Referenz
    /// - Spec 8.5.1: Schema-informed Document Grammar hat SE(qname) Productions
    /// - Bei SE(qname) (nicht SE(*)) ist der QName implizit durch den Event Code
    pub(super) fn encode_event_content_document_ref(
        &mut self,
        event: &EventRef<'_>,
        terminal: &Terminal,
        was_wildcard: bool,
    ) -> Result<()> {
        if event.is_start_element() {
            let qname = event.qname().unwrap();
            self.encode_qname_for_event(qname, terminal, was_wildcard)?;
        } else if event.is_attribute() {
            let qname = event.qname().unwrap();
            let value = event.value().unwrap();
            self.encode_qname_for_event(qname, terminal, was_wildcard)?;
            let in_element_fragment =
                self.document_grammar.grammar_type() == GrammarType::ElementFragment;
            let mut attr_type = None;
            if in_element_fragment
                && let Some(schema) = self.schema.as_ref() {
                    if schema.is_element_fragment_relaxed_attribute(qname) {
                        attr_type = None;
                    } else if let Some(td) =
                        schema.element_fragment_attribute_type(qname)
                    {
                        attr_type = Some(td.clone());
                    }
                }
            let type_def = if terminal.is_untyped() { None } else { attr_type };
            self.encode_value(qname, value, type_def)?;
        } else if event.is_characters() {
            let value = event.value().unwrap();
            let elem_qname = self.current_element_qname()?;
            // CH im Document-Kontext - kein Typed Value (Mixed Content vor erstem Element)
            self.encode_value(&elem_qname, value, None)?;
        } else {
            self.encode_rare_event_content(event);
        }
        Ok(())
    }

    /// Encodiert den Content eines Events (Element-Kontext).
    ///
    /// Spec 8.4.3: Bei gelerntem SE(qname)/AT(qname) wird kein QName encodiert -
    /// er ist implizit durch den Event Code bekannt. Nur bei SE(*)/AT(*)
    /// Wildcard-Match wird der QName encodiert.
    pub(super) fn encode_event_content_element_ref(
        &mut self,
        event: &EventRef<'_>,
        terminal: &Terminal,
        was_wildcard: bool,
    ) -> Result<()> {
        if event.is_start_element() {
            let qname = event.qname().unwrap();
            self.encode_qname_for_event(qname, terminal, was_wildcard)?;
        } else if event.is_attribute() {
            let qname = event.qname().unwrap();
            let value = event.value().unwrap();
            self.encode_qname_for_event(qname, terminal, was_wildcard)?;
            // Value wird IMMER encodiert (auch bei AT(qname))
            // AT: Typed Value wenn Attribut-Typ im Element-Typ definiert
            if qname.is_xsi_nil()
                && self.schema.is_some()
                && !matches!(
                    terminal,
                    Terminal::Attribute(AttributeKind::WildcardUntyped | AttributeKind::QNameUntyped(_))
                )
            {
                self.encode_xsi_nil_value(value)?;
            } else {
                let in_element_fragment = self
                    .element_stack
                    .last()
                    .and_then(|elem| self.element_grammars.get(&elem.grammar_key))
                    .map(|g| g.grammar_type() == GrammarType::ElementFragment)
                    .unwrap_or(false);
                let mut attr_type = if in_element_fragment {
                    None
                } else {
                    self.current_element_type
                        .as_ref()
                        .and_then(|et| et.get_attribute_type(qname).cloned())
                };
                if attr_type.is_none()
                    && let Some(schema) = self.schema.as_ref() {
                        if in_element_fragment {
                            if schema.is_element_fragment_relaxed_attribute(qname) {
                                attr_type = None;
                            } else if let Some(td) =
                                schema.element_fragment_attribute_type(qname)
                            {
                                attr_type = Some(td.clone());
                            }
                        } else {
                            attr_type =
                                schema.get_global_attribute_type(qname).cloned();
                        }
                    }
                let type_def = if terminal.is_untyped() { None } else { attr_type };
                self.encode_value(qname, value, type_def)?;
            }

            // Spec 8.5.4.4.2: xsi:nil Handling
            if qname.is_xsi_nil() {
                self.handle_xsi_nil(value)?;
            }

            // Spec 8.5.4.4: xsi:type Handling
            if qname.is_xsi_type() {
                self.handle_xsi_type(value)?;
            }
        } else if event.is_characters() {
            let value = event.value().unwrap();
            let elem_qname = self.current_element_qname()?;
            // CH: Typed Value wenn current_element_type vorhanden
            let type_def = if terminal.is_untyped() { None } else { self.current_element_type.clone() };
            self.encode_value(&elem_qname, value, type_def)?;
        } else if let Some(ns_content) = event.ns_content() {
            // NS Content: uri + prefix + local-element-ns (Table 4-2, Spec 7.3.2)
            self.encode_namespace_declaration(ns_content)?;
            // NS-Binding merken für xsi:type-Value-Auflösung (Spec 8.5.4.4).
            if let Some(elem) = self.element_stack.last_mut() {
                elem.ns_bindings.get_or_insert_with(Vec::new).push((ns_content.prefix.clone(), ns_content.uri.clone()));
            }
        } else {
            self.encode_rare_event_content(event);
        }
        Ok(())
    }

    /// Encodiert einen QName mit String Table (Spec 7.1.7, 7.3.2, 7.3.3).
    ///
    /// Encoding-Reihenfolge: URI -> LocalName -> Prefix (wenn preserve.prefixes)
    pub(super) fn encode_qname(&mut self, qname: &QName) -> Result<()> {
        // 1. URI encodieren
        let (uri_result, uri_size) = self.string_table.encode_uri(&qname.uri);
        let uri_id = self.encode_uri_result(uri_result, uri_size, &qname.uri);

        // 2. LocalName encodieren
        let (ln_result, ln_size) = self
            .string_table
            .encode_local_name(uri_id, &qname.local_name);
        self.encode_local_name_result(ln_result, ln_size, &qname.local_name);

        // 3. Prefix encodieren (wenn preserve.prefixes)
        if self.options.preserve.prefixes {
            self.encode_prefix(uri_id, qname);
        }

        Ok(())
    }

    /// Encodiert einen QName mit impliziter URI (SE/AT(uri:*)).
    ///
    /// Spec 7.1.7: Bei SE(uri:*)/AT(uri:*) ist die URI implizit, es wird nur der
    /// LocalName (und optional Prefix) encodiert.
    pub(super) fn encode_qname_with_implied_uri(&mut self, qname: &QName, uri: &str) -> Result<()> {
        // URI ist implizit (nicht im Stream), daher bei Bedarf lokal hinzufügen.
        let uri_id = self
            .string_table
            .lookup_uri(uri)
            .unwrap_or_else(|| self.string_table.add_uri(uri));

        let (ln_result, ln_size) = self
            .string_table
            .encode_local_name(uri_id, &qname.local_name);
        self.encode_local_name_result(ln_result, ln_size, &qname.local_name);

        if self.options.preserve.prefixes {
            self.encode_prefix(uri_id, qname);
        }

        Ok(())
    }

    /// Encodiert das URI-Ergebnis (Spec 7.3.2).
    ///
    /// Spec 7.3.2 - Compact Identifier Partition:
    /// - Hit: n-bit(i+1), wobei n = ceil(log2(m+1))
    /// - Miss: n-bit(0) + String Literal
    ///
    /// Returns: Die URI-ID fuer LocalName-Lookup.
    pub(super) fn encode_uri_result(
        &mut self,
        result: CompactIdResult,
        size: usize,
        uri: &str,
    ) -> usize {
        // n = ceil(log2(m+1)), wobei m = size (Anzahl Eintraege vor dieser Operation)
        let n = bits_for_value(size + 1);

        match result {
            CompactIdResult::Hit(id) => {
                // Hit: n-bit(i+1)
                self.encode_n_bit((id + 1) as u64, n);
                id
            }
            CompactIdResult::Miss(id) => {
                // Miss: n-bit(0) + String Literal
                self.encode_n_bit(0, n);
                self.encode_string(uri);
                id
            }
        }
    }

    /// Encodiert das LocalName-Ergebnis (Spec 7.3.3).
    ///
    /// Spec 7.3.3 - Partitions Optimized for Frequent use of String Literals:
    /// - Hit: 0 (Unsigned Integer) + n-bit Compact ID, wobei n = ceil(log2(m))
    /// - Miss: String Literal mit length+1 als Laenge
    pub(super) fn encode_local_name_result(
        &mut self,
        result: StringLiteralResult,
        size: usize,
        local_name: &str,
    ) {
        match result {
            StringLiteralResult::Hit(id) => {
                // Hit: 0 als Unsigned Integer, dann Compact ID als n-bit
                crate::unsigned_integer::encode(&mut self.writer, 0);
                let n = bits_for_value(size);
                self.encode_n_bit(id as u64, n);
            }
            StringLiteralResult::Miss => {
                // Miss: String mit length+1 (unterscheidet Miss von Hit mit id=0)
                self.encode_string_with_length_offset(local_name, 1);
            }
        }
    }

    /// Encodiert den Prefix (wenn preserve.prefixes, Spec 7.1.7, 7.3.2).
    ///
    /// Spec 7.3.2 - Compact Identifier Partition:
    /// - Hit: n-bit(i+1), wobei n = ceil(log2(m+1))
    /// - Miss: n-bit(0) + String Literal
    /// Encodiert den Prefix-Anteil eines QName (Spec 7.1.7).
    ///
    /// Anders als NS-Event-Prefix (Table 4-2, mit Miss-Option und n=ceil(log2(m+1)))
    /// hat der QName-Prefix keine Miss-Option: n = ceil(log2(m)), Wert = Compact-ID.
    /// Bei m=0 (kein Prefix in Scope) wird der Prefix "elided" (nicht encodiert).
    pub(super) fn encode_prefix(&mut self, uri_id: usize, qname: &QName) {
        // Spec 7.1.7: Wenn m=0 (keine Prefix-Einträge), wird der Prefix
        // "elided" — nichts encodiert und nichts zur Tabelle hinzugefügt.
        // Der Decoder überspringt den Prefix ebenfalls bei m=0.
        let prefix_count = self.string_table.prefix_count(uri_id);
        if prefix_count == 0 {
            return;
        }

        let prefix_str = qname.prefix.as_deref().unwrap_or("");

        // Spec 7.1.7: QName-Prefix hat KEINE Miss-Option — der Wert ist
        // immer ein Compact Identifier aus den vorhandenen m Einträgen.
        // Prefixe werden nur über NS-Events zur Tabelle hinzugefügt.
        // Falls der Prefix nicht gefunden wird, verwende den ersten Eintrag (id=0).
        let n = bits_for_value(prefix_count);
        if let Some(id) = self.string_table.lookup_prefix(uri_id, prefix_str) {
            self.encode_n_bit(id as u64, n);
        } else {
            // Prefix nicht in Tabelle — Fallback auf Eintrag 0
            self.encode_n_bit(0, n);
        }
    }

    /// Encodiert den Wert eines xsi:type Attributs als QName (Spec 8.5.4.4, 7.1.7).
    ///
    /// Der Wert kommt entweder als Clark-Notation `{URI}localname` (vom XML-Parser
    /// voraufgeloest) oder als `prefix:localname` / `localname`.
    pub(super) fn encode_xsi_type_value(&mut self, value: &str) -> Result<()> {
        // Byte-Alignment: Werte werden auf Byte-Grenze ausgerichtet (Spec 7.1)
        if matches!(
            self.options.effective_alignment(),
            Alignment::ByteAlignment | Alignment::PreCompression
        ) {
            self.writer.align_to_byte();
        }

        let ns_bindings = self.element_stack.last()
            .and_then(|e| e.ns_bindings.as_deref())
            .unwrap_or(&[]);
        let qname = Self::parse_xsi_type_qname(value, self.schema.as_deref(), ns_bindings);
        self.encode_qname(&qname)
    }

    /// Encodiert NS-Event Content mit String Table (Spec Table 4-2, 7.3.2).
    ///
    /// - uri: URI-Partition (Compact ID, Spec 7.3.2)
    /// - prefix: Prefix-Partition mit Miss-Option (Spec 7.3.2)
    /// - local-element-ns: Boolean (1 Bit)
    pub(super) fn encode_namespace_declaration(&mut self, content: &crate::event::NsContent) -> Result<()> {
        // 1. URI encodieren (String Table URI Partition)
        let (uri_result, uri_size) = self.string_table.encode_uri(&content.uri);
        let uri_id = self.encode_uri_result(uri_result, uri_size, &content.uri);

        // 2. Prefix encodieren (String Table Prefix Partition mit Miss-Option)
        let prefix_str = &content.prefix;
        let (result, size_before) = self.string_table.encode_prefix(uri_id, prefix_str);
        let n = bits_for_value(size_before + 1);
        match result {
            CompactIdResult::Hit(id) => {
                self.encode_n_bit((id + 1) as u64, n);
            }
            CompactIdResult::Miss(_) => {
                self.encode_n_bit(0, n);
                self.encode_string(prefix_str);
            }
        }

        // 3. local-element-ns als 1-Bit Boolean (Spec Table 4-2)
        self.encode_n_bit(if content.local_element_ns { 1 } else { 0 }, 1);

        Ok(())
    }
}
