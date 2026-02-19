use std::rc::Rc;
use crate::decoder::Decoder;
use crate::options::Alignment;
use crate::qname::{InternedStr, QName};
use crate::Result;

impl<'a> Decoder<'a> {


    /// Löst den QName aus Terminal oder Stream auf.
    /// Decodiert einen QName mit String Table (Spec 7.1.7, 7.3.2, 7.3.3).
    ///
    /// Decoding-Reihenfolge: URI -> LocalName -> Prefix (wenn preserve.prefixes)
    pub(super) fn decode_qname(&mut self) -> Result<QName> {
        // 1. URI decodieren
        let (uri, uri_id) = self.decode_uri()?;

        // 2. LocalName decodieren
        let local_name = self.decode_local_name(uri_id)?;

        // 3. Prefix decodieren (Spec 7.1.7)
        // Prefix wird nur gelesen, wenn:
        // - preserve.prefixes=true UND
        // - Der URI "in scope" ist (mindestens ein Prefix existiert für diesen URI)
        // Sonst wird der Prefix "elided" (nicht im Stream encodiert).
        let prefix: Option<Rc<str>> = if self.options.preserve.prefixes && self.string_table.prefix_count(uri_id) > 0
        {
            Some(self.decode_prefix(uri_id)?)
        } else {
            None
        };

        Ok(QName::with_optional_prefix(uri, local_name, prefix))
    }

    /// Decodiert einen QName mit impliziter URI (SE/AT(uri:*)).
    ///
    /// Spec 7.1.7: Bei SE(uri:*)/AT(uri:*) ist die URI implizit durch das Event
    /// Code bekannt; nur LocalName (und optional Prefix) werden gelesen.
    pub(super) fn decode_qname_with_implied_uri(&mut self, uri_interned: InternedStr) -> Result<QName> {
        let uri = self.interner.resolve(uri_interned);
        // URI ist implizit (nicht im Stream), daher bei Bedarf lokal hinzufügen.
        let uri_id = match self.string_table.lookup_uri(uri) {
            Some(id) => id,
            None => self.string_table.add_uri(uri),
        };
        let uri_arc = Rc::from(uri);
        let local_name = self.decode_local_name(uri_id)?;
        let prefix: Option<Rc<str>> = if self.options.preserve.prefixes && self.string_table.prefix_count(uri_id) > 0
        {
            Some(self.decode_prefix(uri_id)?)
        } else {
            None
        };
        Ok(QName::with_optional_prefix(uri_arc, local_name, prefix))
    }

    /// Dekodiert den Wert eines xsi:type Attributs (Spec 8.5.4.4.1).
    ///
    /// Der Wert ist als QName (7.1.7) codiert, nicht als String.
    /// Gibt den QName als lexikalische Repräsentation zurück (z.B. "prefix:localname").
    pub(super) fn decode_xsi_type_value(&mut self) -> Result<Rc<str>> {
        // Byte-Alignment: Werte werden auf Byte-Grenze ausgerichtet (Spec 7.1)
        if matches!(
            self.options.effective_alignment(),
            Alignment::ByteAlignment | Alignment::PreCompression
        ) {
            self.reader.align_to_byte();
        }

        // QName dekodieren (URI, LocalName, optional Prefix)
        let type_qname = self.decode_qname()?;
        self.xsi_type_qname = Some(type_qname.clone());

        // Zu lexikalischer Form konvertieren
        // Spec: "If there is no namespace in scope for the specified qname prefix,
        // set the uri of target-type to empty ("") and the localName to the full
        // lexical value of the QName, including the prefix."
        match &type_qname.prefix {
            Some(pfx) if !pfx.is_empty() => {
                Ok(format!("{}:{}", pfx, type_qname.local_name).into())
            }
            _ => Ok(Rc::clone(&type_qname.local_name)),
        }
    }
}
