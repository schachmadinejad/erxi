//! EXI Event Content Encoding (Spec Section 7, Table 4-1, Table 4-2).
//!
//! Encodes and decodes the content items of EXI events.
//!
//! # Content Items (Zusammenfassung aus Table 4-1 und 4-2)
//!
//! | Event | Content Items | Datatype Representation |
//! |-------|---------------|-------------------------|
//! | SD, ED, EE, SC | – | kein Content |
//! | SE | qname | QName (7.1.7) |
//! | AT | qname + value | QName + typed/String |
//! | CH | value | typed/String |
//! | NS | uri + prefix + local-element-ns | String + String + Boolean (7.1.2) |
//! | CM | text | String (7.1.10) |
//! | PI | name + text | String + String |
//! | DT | name + public + system + text | 4× String |
//! | ER | name | String |
//!
//! # Encoding Modes (Spec 7)
//!
//! **Schema-less (untyped):** Alle `value`-Items (AT, CH) werden als String
//! (7.1.10) encodiert. Dies ist der aktuelle Implementierungsstand.
//!
//! **Schema-informed (typed):** `value`-Items werden nach ihrem Schema-Datatype
//! encodiert (Integer, Boolean, DateTime, etc.). Wird in Issue #37 (Schema-informed
//! Encoder/Decoder) implementiert.
//!
//! **lexicalValues=true (Spec 5.4, 7):** Wenn diese Preserve-Option gesetzt ist,
//! werden typed values stattdessen als String mit Restricted Character Sets
//! (Table 7-2) encodiert. Dies ermöglicht Round-Trip-Fidelity für lexikalische
//! Formen. Erst bei schema-informed encoding relevant.

use std::rc::Rc;

use crate::bitstream::{BitReader, BitWriter};
use crate::event::{
    AtContent, ChContent, CmContent, DtContent, ErContent, ExiEvent, NsContent, PiContent,
};
use crate::qname::QName;
use crate::{Result, boolean, qname, string};

// ============================================================================
// High-Level API: encode_event
// ============================================================================

/// Encodiert den Content eines ExiEvent (Spec Table 4-2).
///
/// **Hinweis:** Diese Funktion encodiert nur den Content des Events, nicht den
/// Event Code. Der Event Code wird separat im Grammar-System behandelt (Issue #21:
/// Event Codes – Berechnung und Encoding).
///
/// **Kein decode_event:** Eine entsprechende `decode_event`-Funktion existiert nicht,
/// da der Event-Typ beim Decoding erst aus der Grammar bekannt sein muss, bevor
/// der Content decodiert werden kann. Stattdessen die individuellen `decode_*`-
/// Funktionen verwenden.
///
/// Für typed values (AT, CH) wird aktuell immer String-Encoding verwendet.
/// Schema-informed typed encoding wird in Issue #37 (Schema-informed Encoder/Decoder)
/// implementiert.
pub fn encode_event(writer: &mut BitWriter, event: &ExiEvent, ctx: &ContentContext) {
    match event {
        ExiEvent::StartDocument => encode_start_document(writer),
        ExiEvent::EndDocument => encode_end_document(writer),
        ExiEvent::StartElement(qname_val) => encode_start_element(writer, qname_val, ctx),
        ExiEvent::EndElement => encode_end_element(writer),
        ExiEvent::Attribute(content) => encode_attribute(writer, content, ctx),
        ExiEvent::Characters(content) => encode_characters(writer, content),
        ExiEvent::NamespaceDeclaration(content) => encode_namespace_declaration(writer, content, ctx),
        ExiEvent::Comment(content) => encode_comment(writer, content),
        ExiEvent::ProcessingInstruction(content) => encode_processing_instruction(writer, content),
        ExiEvent::DocType(content) => encode_doctype(writer, content),
        ExiEvent::EntityReference(content) => encode_entity_reference(writer, content),
        ExiEvent::SelfContained => encode_self_contained(writer),
    }
}

/// Encoding-Kontext fuer Event Content.
///
/// Enthaelt alle Optionen die das Content-Encoding beeinflussen.
#[derive(Debug, Clone, Default)]
pub struct ContentContext {
    preserve_prefixes: bool,
    prefix_count: usize,
    byte_aligned: bool,
}

impl ContentContext {
    /// Erstellt einen neuen ContentContext.
    pub fn new(preserve_prefixes: bool, prefix_count: usize, byte_aligned: bool) -> Self {
        Self { preserve_prefixes, prefix_count, byte_aligned }
    }

    /// Preserve.prefixes Option (Spec 5.4).
    pub fn preserve_prefixes(&self) -> bool { self.preserve_prefixes }
    /// Anzahl bekannter Prefixe fuer die aktuelle URI.
    pub fn prefix_count(&self) -> usize { self.prefix_count }
    /// Byte-aligned Encoding (Spec 7.1.9).
    pub fn byte_aligned(&self) -> bool { self.byte_aligned }
}

// ============================================================================
// Events ohne Content: SD, ED, EE, SC
// ============================================================================

/// SD (Start Document) hat keinen Content (Spec Table 4-2).
pub fn encode_start_document(_writer: &mut BitWriter) {}

/// SD (Start Document) hat keinen Content (Spec Table 4-2).
pub fn decode_start_document(_reader: &mut BitReader) -> Result<()> {
    Ok(())
}

/// ED (End Document) hat keinen Content (Spec Table 4-2).
pub fn encode_end_document(_writer: &mut BitWriter) {}

/// ED (End Document) hat keinen Content (Spec Table 4-2).
pub fn decode_end_document(_reader: &mut BitReader) -> Result<()> {
    Ok(())
}

/// EE (End Element) hat keinen Content (Spec Table 4-2).
pub fn encode_end_element(_writer: &mut BitWriter) {}

/// EE (End Element) hat keinen Content (Spec Table 4-2).
pub fn decode_end_element(_reader: &mut BitReader) -> Result<()> {
    Ok(())
}

/// SC (Self Contained) hat keinen Content (Spec Table 4-2).
pub fn encode_self_contained(_writer: &mut BitWriter) {}

/// SC (Self Contained) hat keinen Content (Spec Table 4-2).
pub fn decode_self_contained(_reader: &mut BitReader) -> Result<()> {
    Ok(())
}

// ============================================================================
// SE (Start Element): qname
// ============================================================================

/// Encodiert SE Content: qname (Spec 7.1.7, Table 4-2).
pub fn encode_start_element(writer: &mut BitWriter, qname_val: &QName, ctx: &ContentContext) {
    qname::encode(writer, qname_val, ctx.preserve_prefixes, ctx.prefix_count);
}

/// Decodiert SE Content: qname (Spec 7.1.7, Table 4-2).
pub fn decode_start_element(
    reader: &mut BitReader,
    ctx: &ContentContext,
    prefix_resolver: impl Fn(u64) -> Option<Rc<str>>,
) -> Result<QName> {
    qname::decode(
        reader,
        ctx.preserve_prefixes,
        ctx.prefix_count,
        prefix_resolver,
    )
}

// ============================================================================
// AT (Attribute): qname + value
// ============================================================================

/// Encodiert AT Content: qname + value (Spec 7.1.7 + 7.1.10, Table 4-2).
///
/// Im schema-less Modus wird der Value immer als String encodiert.
/// Schema-informed typed encoding wird in Issue #37 implementiert.
pub fn encode_attribute(writer: &mut BitWriter, content: &AtContent, ctx: &ContentContext) {
    qname::encode(
        writer,
        &content.qname,
        ctx.preserve_prefixes,
        ctx.prefix_count,
    );
    string::encode(writer, &content.value);
}

/// Decodiert AT Content: qname + value (Spec 7.1.7 + 7.1.10, Table 4-2).
pub fn decode_attribute(
    reader: &mut BitReader,
    ctx: &ContentContext,
    prefix_resolver: impl Fn(u64) -> Option<Rc<str>>,
) -> Result<AtContent> {
    let qname_val = qname::decode(
        reader,
        ctx.preserve_prefixes,
        ctx.prefix_count,
        prefix_resolver,
    )?;
    let value = string::decode(reader)?.into();
    Ok(AtContent {
        qname: Rc::new(qname_val),
        value,
    })
}

// ============================================================================
// CH (Characters): value
// ============================================================================

/// Encodiert CH Content: value (Spec 7.1.10, Table 4-2).
///
/// Im schema-less Modus wird der Value immer als String encodiert.
/// Schema-informed typed encoding wird in Issue #37 implementiert.
pub fn encode_characters(writer: &mut BitWriter, content: &ChContent) {
    string::encode(writer, &content.value);
}

/// Decodiert CH Content: value (Spec 7.1.10, Table 4-2).
pub fn decode_characters(reader: &mut BitReader) -> Result<ChContent> {
    let value = string::decode(reader)?.into();
    Ok(ChContent { value })
}

// ============================================================================
// NS (Namespace Declaration): uri + prefix + local-element-ns
// ============================================================================

/// Encodiert NS Content: uri + prefix + local-element-ns (Spec Table 4-2).
///
/// - uri: String (7.1.10)
/// - prefix: String (7.1.10)
/// - local-element-ns: Boolean (7.1.2)
///
/// Bei byte-aligned Encoding (compression/byte-alignment/pre-compression) wird
/// local-element-ns als 1 Byte encodiert (Spec 7.1.9).
pub fn encode_namespace_declaration(writer: &mut BitWriter, content: &NsContent, ctx: &ContentContext) {
    string::encode(writer, &content.uri);
    string::encode(writer, &content.prefix);
    if ctx.byte_aligned {
        boolean::encode_byte_aligned(writer, content.local_element_ns);
    } else {
        boolean::encode(writer, content.local_element_ns);
    }
}

/// Decodiert NS Content: uri + prefix + local-element-ns (Spec Table 4-2).
///
/// Bei byte-aligned Encoding wird local-element-ns als 1 Byte decodiert.
pub fn decode_namespace_declaration(reader: &mut BitReader, ctx: &ContentContext) -> Result<NsContent> {
    let uri = string::decode(reader)?.into();
    let prefix = string::decode(reader)?.into();
    let local_element_ns = if ctx.byte_aligned {
        boolean::decode_byte_aligned(reader)?
    } else {
        boolean::decode(reader)?
    };
    Ok(NsContent {
        uri,
        prefix,
        local_element_ns,
    })
}

// ============================================================================
// CM (Comment): text
// ============================================================================

/// Encodiert CM Content: text (Spec 7.1.10, Table 4-2).
pub fn encode_comment(writer: &mut BitWriter, content: &CmContent) {
    string::encode(writer, &content.text);
}

/// Decodiert CM Content: text (Spec 7.1.10, Table 4-2).
pub fn decode_comment(reader: &mut BitReader) -> Result<CmContent> {
    let trace = std::env::var("ERXI_TRACE_CM_PI").is_ok();
    let before = if trace { Some(reader.bit_position()) } else { None };
    let text: Rc<str> = string::decode(reader)?.into();
    if let Some(start) = before {
        eprintln!(
            "decode_comment: bits {} -> {} text_len={}",
            start,
            reader.bit_position(),
            text.chars().count()
        );
    }
    Ok(CmContent { text })
}

// ============================================================================
// PI (Processing Instruction): name + text
// ============================================================================

/// Encodiert PI Content: name + text (Spec 7.1.10, Table 4-2).
pub fn encode_processing_instruction(writer: &mut BitWriter, content: &PiContent) {
    string::encode(writer, &content.name);
    string::encode(writer, &content.text);
}

/// Decodiert PI Content: name + text (Spec 7.1.10, Table 4-2).
pub fn decode_processing_instruction(reader: &mut BitReader) -> Result<PiContent> {
    let trace = std::env::var("ERXI_TRACE_CM_PI").is_ok();
    let before = if trace { Some(reader.bit_position()) } else { None };
    let name: Rc<str> = string::decode(reader)?.into();
    let text: Rc<str> = string::decode(reader)?.into();
    if let Some(start) = before {
        eprintln!(
            "decode_pi: bits {} -> {} name_len={} text_len={}",
            start,
            reader.bit_position(),
            name.chars().count(),
            text.chars().count()
        );
    }
    Ok(PiContent { name, text })
}

// ============================================================================
// DT (DOCTYPE): name + public + system + text
// ============================================================================

/// Encodiert DT Content: name + public + system + text (Spec 7.1.10, Table 4-2).
pub fn encode_doctype(writer: &mut BitWriter, content: &DtContent) {
    string::encode(writer, &content.name);
    string::encode(writer, &content.public);
    string::encode(writer, &content.system);
    string::encode(writer, &content.text);
}

/// Decodiert DT Content: name + public + system + text (Spec 7.1.10, Table 4-2).
pub fn decode_doctype(reader: &mut BitReader) -> Result<DtContent> {
    let name = string::decode(reader)?.into();
    let public = string::decode(reader)?.into();
    let system = string::decode(reader)?.into();
    let text = string::decode(reader)?.into();
    Ok(DtContent {
        name,
        public,
        system,
        text,
    })
}

// ============================================================================
// ER (Entity Reference): name
// ============================================================================

/// Encodiert ER Content: name (Spec 7.1.10, Table 4-2).
pub fn encode_entity_reference(writer: &mut BitWriter, content: &ErContent) {
    string::encode(writer, &content.name);
}

/// Decodiert ER Content: name (Spec 7.1.10, Table 4-2).
pub fn decode_entity_reference(reader: &mut BitReader) -> Result<ErContent> {
    let name = string::decode(reader)?.into();
    Ok(ErContent { name })
}

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // Hilfsfunktionen
    // ========================================================================

    fn default_ctx() -> ContentContext {
        ContentContext::default()
    }

    fn ctx_with_prefixes(prefix_count: usize) -> ContentContext {
        ContentContext {
            preserve_prefixes: true,
            prefix_count,
            byte_aligned: false,
        }
    }

    fn no_prefix_resolver(_idx: u64) -> Option<Rc<str>> {
        None
    }

    /// Hilfsfunktion: Testet dass encode keine Bytes schreibt und decode Ok(()) zurückgibt.
    fn assert_empty_content(
        encode_fn: fn(&mut BitWriter),
        decode_fn: fn(&mut BitReader) -> Result<()>,
        event_name: &str,
    ) {
        let mut writer = BitWriter::new();
        encode_fn(&mut writer);
        let data = writer.into_vec();
        assert!(data.is_empty(), "{event_name} sollte keine Bytes schreiben");

        let mut reader = BitReader::new(&[]);
        assert!(decode_fn(&mut reader).is_ok());
    }

    /// Hilfsfunktion: Testet dass decode bei leerem Stream einen Fehler zurückgibt.
    fn assert_decode_empty_fails<T>(decode_fn: fn(&mut BitReader) -> Result<T>) {
        let mut reader = BitReader::new(&[]);
        assert!(decode_fn(&mut reader).is_err());
    }

    // ========================================================================
    // SD, ED, EE, SC (Events ohne Content) Tests
    // ========================================================================

    /// Spec Table 4-2: SD hat keinen Content.
    #[test]
    fn sd_kein_content() {
        assert_empty_content(encode_start_document, decode_start_document, "SD");
    }

    /// Spec Table 4-2: ED hat keinen Content.
    #[test]
    fn ed_kein_content() {
        assert_empty_content(encode_end_document, decode_end_document, "ED");
    }

    /// Spec Table 4-2: EE hat keinen Content.
    #[test]
    fn ee_kein_content() {
        assert_empty_content(encode_end_element, decode_end_element, "EE");
    }

    /// Spec Table 4-2: SC hat keinen Content.
    #[test]
    fn sc_kein_content() {
        assert_empty_content(encode_self_contained, decode_self_contained, "SC");
    }

    // ========================================================================
    // SE (Start Element) Tests
    // ========================================================================

    /// Spec Table 4-2: SE Content ist qname (7.1.7).
    #[test]
    fn se_qname_roundtrip() {
        let qname_val = QName::new("http://example.org", "element");
        let ctx = default_ctx();

        let mut writer = BitWriter::new();
        encode_start_element(&mut writer, &qname_val, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_start_element(&mut reader, &ctx, no_prefix_resolver).unwrap();

        assert_eq!(&*decoded.uri, "http://example.org");
        assert_eq!(&*decoded.local_name, "element");
    }

    /// Spec 7.1.7: QName mit Prefix wenn preserve.prefixes=true.
    #[test]
    fn se_qname_mit_prefix() {
        let qname_val = QName::with_prefix("http://example.org", "element", "ex");
        let ctx = ctx_with_prefixes(2);

        let mut writer = BitWriter::new();
        encode_start_element(&mut writer, &qname_val, &ctx);
        let data = writer.into_vec();

        // Mit prefix_count=2 wird 1 Bit für den Prefix-Index geschrieben
        assert!(!data.is_empty());

        let mut reader = BitReader::new(&data);
        let decoded = decode_start_element(&mut reader, &ctx, |_| Some(Rc::from("ex"))).unwrap();

        assert_eq!(&*decoded.uri, "http://example.org");
        assert_eq!(&*decoded.local_name, "element");
        assert_eq!(decoded.prefix.as_deref(), Some("ex"));
    }

    /// Spec 7.1.7: QName ohne Namespace (leere URI).
    #[test]
    fn se_qname_ohne_namespace() {
        let qname_val = QName::new("", "root");
        let ctx = default_ctx();

        let mut writer = BitWriter::new();
        encode_start_element(&mut writer, &qname_val, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_start_element(&mut reader, &ctx, no_prefix_resolver).unwrap();

        assert!(decoded.uri.is_empty());
        assert_eq!(&*decoded.local_name, "root");
    }

    // ========================================================================
    // AT (Attribute) Tests
    // ========================================================================

    /// Spec Table 4-2: AT Content ist qname + value.
    #[test]
    fn at_qname_value_roundtrip() {
        let content = AtContent {
            qname: Rc::new(QName::new("", "id")),
            value: "123".into(),
        };
        let ctx = default_ctx();

        let mut writer = BitWriter::new();
        encode_attribute(&mut writer, &content, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_attribute(&mut reader, &ctx, no_prefix_resolver).unwrap();

        assert_eq!(&*decoded.qname.local_name, "id");
        assert_eq!(&*decoded.value, "123");
    }

    /// Spec Table 4-2: AT mit Namespace-Attribut.
    #[test]
    fn at_mit_namespace() {
        let content = AtContent {
            qname: Rc::new(QName::new("http://www.w3.org/XML/1998/namespace", "lang")),
            value: "de".into(),
        };
        let ctx = default_ctx();

        let mut writer = BitWriter::new();
        encode_attribute(&mut writer, &content, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_attribute(&mut reader, &ctx, no_prefix_resolver).unwrap();

        assert_eq!(&*decoded.qname.uri, "http://www.w3.org/XML/1998/namespace");
        assert_eq!(&*decoded.qname.local_name, "lang");
        assert_eq!(&*decoded.value, "de");
    }

    /// Spec 7.1.10: AT value mit Unicode-Zeichen.
    #[test]
    fn at_unicode_value() {
        let content = AtContent {
            qname: Rc::new(QName::new("", "title")),
            value: "日本語 🎉".into(),
        };
        let ctx = default_ctx();

        let mut writer = BitWriter::new();
        encode_attribute(&mut writer, &content, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_attribute(&mut reader, &ctx, no_prefix_resolver).unwrap();

        assert_eq!(&*decoded.value, "日本語 🎉");
    }

    // ========================================================================
    // CH (Characters) Tests
    // ========================================================================

    /// Spec Table 4-2: CH Content ist value (7.1.10).
    #[test]
    fn ch_value_roundtrip() {
        let content = ChContent {
            value: "Hello, World!".into(),
        };

        let mut writer = BitWriter::new();
        encode_characters(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_characters(&mut reader).unwrap();

        assert_eq!(&*decoded.value, "Hello, World!");
    }

    /// Spec 7.1.10: CH mit leerem String.
    #[test]
    fn ch_leerer_string() {
        let content = ChContent {
            value: "".into(),
        };

        let mut writer = BitWriter::new();
        encode_characters(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_characters(&mut reader).unwrap();

        assert!(decoded.value.is_empty());
    }

    /// Spec 7.1.10: CH mit Whitespace.
    #[test]
    fn ch_whitespace() {
        let content = ChContent {
            value: "  \t\n  ".into(),
        };

        let mut writer = BitWriter::new();
        encode_characters(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_characters(&mut reader).unwrap();

        assert_eq!(&*decoded.value, "  \t\n  ");
    }

    // ========================================================================
    // NS (Namespace Declaration) Tests
    // ========================================================================

    /// Spec Table 4-2: NS Content ist uri + prefix + local-element-ns.
    #[test]
    fn ns_roundtrip() {
        let content = NsContent {
            uri: "http://www.w3.org/1999/xhtml".into(),
            prefix: "html".into(),
            local_element_ns: false,
        };

        let mut writer = BitWriter::new();
        encode_namespace_declaration(&mut writer, &content, &ContentContext::default());
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_namespace_declaration(&mut reader, &ContentContext::default()).unwrap();

        assert_eq!(&*decoded.uri, "http://www.w3.org/1999/xhtml");
        assert_eq!(&*decoded.prefix, "html");
        assert!(!decoded.local_element_ns);
    }

    /// Spec Table 4-2, 7.1.9: NS Content mit byte-aligned Boolean für local_element_ns.
    #[test]
    fn ns_roundtrip_byte_aligned() {
        let ctx = ContentContext {
            byte_aligned: true,
            ..ContentContext::default()
        };

        for local_element_ns in [true, false] {
            let content = NsContent {
                uri: "http://example.org".into(),
                prefix: "ex".into(),
                local_element_ns,
            };

            let mut writer = BitWriter::new();
            encode_namespace_declaration(&mut writer, &content, &ctx);
            let data = writer.into_vec();

            let mut reader = BitReader::new(&data);
            let decoded = decode_namespace_declaration(&mut reader, &ctx).unwrap();

            assert_eq!(&*decoded.uri, "http://example.org");
            assert_eq!(&*decoded.prefix, "ex");
            assert_eq!(decoded.local_element_ns, local_element_ns);
        }
    }

    /// Spec 4: NS mit local_element_ns=true zeigt Element-Namespace an.
    #[test]
    fn ns_local_element_ns_true() {
        let content = NsContent {
            uri: "http://example.org".into(),
            prefix: "ex".into(),
            local_element_ns: true,
        };

        let mut writer = BitWriter::new();
        encode_namespace_declaration(&mut writer, &content, &ContentContext::default());
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_namespace_declaration(&mut reader, &ContentContext::default()).unwrap();

        assert!(decoded.local_element_ns);
    }

    /// Spec 4: NS mit leerem Prefix = Default Namespace.
    #[test]
    fn ns_default_namespace() {
        let content = NsContent {
            uri: "http://example.org".into(),
            prefix: "".into(),
            local_element_ns: false,
        };

        let mut writer = BitWriter::new();
        encode_namespace_declaration(&mut writer, &content, &ContentContext::default());
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_namespace_declaration(&mut reader, &ContentContext::default()).unwrap();

        assert!(decoded.prefix.is_empty());
    }

    /// Spec 4: NS mit leerer URI = Namespace undeclaration.
    #[test]
    fn ns_undeclare() {
        let content = NsContent {
            uri: "".into(),
            prefix: "ex".into(),
            local_element_ns: false,
        };

        let mut writer = BitWriter::new();
        encode_namespace_declaration(&mut writer, &content, &ContentContext::default());
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_namespace_declaration(&mut reader, &ContentContext::default()).unwrap();

        assert!(decoded.uri.is_empty());
        assert_eq!(&*decoded.prefix, "ex");
    }

    // ========================================================================
    // CM (Comment) Tests
    // ========================================================================

    /// Spec Table 4-2: CM Content ist text (7.1.10).
    #[test]
    fn cm_roundtrip() {
        let content = CmContent {
            text: "This is a comment".into(),
        };

        let mut writer = BitWriter::new();
        encode_comment(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_comment(&mut reader).unwrap();

        assert_eq!(&*decoded.text, "This is a comment");
    }

    /// Spec 7.1.10: CM mit leerem Kommentar.
    #[test]
    fn cm_leer() {
        let content = CmContent {
            text: "".into(),
        };

        let mut writer = BitWriter::new();
        encode_comment(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_comment(&mut reader).unwrap();

        assert!(decoded.text.is_empty());
    }

    // ========================================================================
    // PI (Processing Instruction) Tests
    // ========================================================================

    /// Spec Table 4-2: PI Content ist name + text (7.1.10).
    #[test]
    fn pi_roundtrip() {
        let content = PiContent {
            name: "xml-stylesheet".into(),
            text: "type=\"text/xsl\" href=\"style.xsl\"".into(),
        };

        let mut writer = BitWriter::new();
        encode_processing_instruction(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_processing_instruction(&mut reader).unwrap();

        assert_eq!(&*decoded.name, "xml-stylesheet");
        assert_eq!(&*decoded.text, "type=\"text/xsl\" href=\"style.xsl\"");
    }

    /// Spec 7.1.10: PI ohne Daten (nur Target).
    #[test]
    fn pi_ohne_daten() {
        let content = PiContent {
            name: "php".into(),
            text: "".into(),
        };

        let mut writer = BitWriter::new();
        encode_processing_instruction(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_processing_instruction(&mut reader).unwrap();

        assert_eq!(&*decoded.name, "php");
        assert!(decoded.text.is_empty());
    }

    // ========================================================================
    // DT (DOCTYPE) Tests
    // ========================================================================

    /// Spec Table 4-2: DT Content ist name + public + system + text (7.1.10).
    #[test]
    fn dt_roundtrip() {
        let content = DtContent {
            name: "html".into(),
            public: "-//W3C//DTD XHTML 1.0 Strict//EN".into(),
            system: "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd".into(),
            text: "".into(),
        };

        let mut writer = BitWriter::new();
        encode_doctype(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_doctype(&mut reader).unwrap();

        assert_eq!(&*decoded.name, "html");
        assert_eq!(&*decoded.public, "-//W3C//DTD XHTML 1.0 Strict//EN");
        assert_eq!(
            &*decoded.system,
            "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
        );
        assert!(decoded.text.is_empty());
    }

    /// Spec 7.1.10: DT mit internem Subset.
    #[test]
    fn dt_mit_internal_subset() {
        let content = DtContent {
            name: "doc".into(),
            public: "".into(),
            system: "".into(),
            text: "<!ENTITY copyright \"(c) 2024\">".into(),
        };

        let mut writer = BitWriter::new();
        encode_doctype(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_doctype(&mut reader).unwrap();

        assert_eq!(&*decoded.name, "doc");
        assert!(decoded.public.is_empty());
        assert!(decoded.system.is_empty());
        assert_eq!(&*decoded.text, "<!ENTITY copyright \"(c) 2024\">");
    }

    /// Spec 7.1.10: DT minimal (nur Name).
    #[test]
    fn dt_minimal() {
        let content = DtContent {
            name: "root".into(),
            public: "".into(),
            system: "".into(),
            text: "".into(),
        };

        let mut writer = BitWriter::new();
        encode_doctype(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_doctype(&mut reader).unwrap();

        assert_eq!(&*decoded.name, "root");
    }

    // ========================================================================
    // ER (Entity Reference) Tests
    // ========================================================================

    /// Spec Table 4-2: ER Content ist name (7.1.10).
    #[test]
    fn er_roundtrip() {
        let content = ErContent {
            name: "nbsp".into(),
        };

        let mut writer = BitWriter::new();
        encode_entity_reference(&mut writer, &content);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_entity_reference(&mut reader).unwrap();

        assert_eq!(&*decoded.name, "nbsp");
    }

    /// Spec 7.1.10: ER mit XML Built-in Entity.
    #[test]
    fn er_builtin_entity() {
        for entity in ["amp", "lt", "gt", "apos", "quot"] {
            let content = ErContent {
                name: entity.into(),
            };

            let mut writer = BitWriter::new();
            encode_entity_reference(&mut writer, &content);
            let data = writer.into_vec();

            let mut reader = BitReader::new(&data);
            let decoded = decode_entity_reference(&mut reader).unwrap();

            assert_eq!(&*decoded.name, entity);
        }
    }

    // ========================================================================
    // Integration Tests
    // ========================================================================

    /// Mehrere Events hintereinander encodieren/decodieren.
    #[test]
    fn mehrere_events_sequenziell() {
        let mut writer = BitWriter::new();
        let ctx = default_ctx();

        // SD
        encode_start_document(&mut writer);

        // SE
        let se_qname = QName::new("", "root");
        encode_start_element(&mut writer, &se_qname, &ctx);

        // AT
        let at = AtContent {
            qname: Rc::new(QName::new("", "id")),
            value: "1".into(),
        };
        encode_attribute(&mut writer, &at, &ctx);

        // CH
        let ch = ChContent {
            value: "Hello".into(),
        };
        encode_characters(&mut writer, &ch);

        // EE
        encode_end_element(&mut writer);

        // ED
        encode_end_document(&mut writer);

        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);

        // Decode in gleicher Reihenfolge
        decode_start_document(&mut reader).unwrap();
        let se = decode_start_element(&mut reader, &ctx, no_prefix_resolver).unwrap();
        assert_eq!(&*se.local_name, "root");
        let at = decode_attribute(&mut reader, &ctx, no_prefix_resolver).unwrap();
        assert_eq!(&*at.value, "1");
        let ch = decode_characters(&mut reader).unwrap();
        assert_eq!(&*ch.value, "Hello");
        decode_end_element(&mut reader).unwrap();
        decode_end_document(&mut reader).unwrap();
    }

    // ========================================================================
    // High-Level API Tests (encode_event)
    // ========================================================================

    /// Spec Table 4-2: encode_event delegiert korrekt an spezifische Funktionen.
    #[test]
    fn encode_event_sd_ed() {
        let mut writer = BitWriter::new();
        let ctx = default_ctx();

        encode_event(&mut writer, &ExiEvent::StartDocument, &ctx);
        encode_event(&mut writer, &ExiEvent::EndDocument, &ctx);

        let data = writer.into_vec();
        assert!(data.is_empty(), "SD und ED sollten keine Bytes schreiben");
    }

    /// Spec Table 4-2: encode_event für SE.
    #[test]
    fn encode_event_se() {
        let qname_val = QName::new("http://example.org", "root");
        let event = ExiEvent::StartElement(Rc::new(qname_val.clone()));
        let ctx = default_ctx();

        let mut writer = BitWriter::new();
        encode_event(&mut writer, &event, &ctx);
        let data = writer.into_vec();

        // Verify via decode
        let mut reader = BitReader::new(&data);
        let decoded = decode_start_element(&mut reader, &ctx, no_prefix_resolver).unwrap();
        assert_eq!(&*decoded.uri, "http://example.org");
        assert_eq!(&*decoded.local_name, "root");
    }

    /// Spec Table 4-2: encode_event für AT.
    #[test]
    fn encode_event_at() {
        let content = AtContent {
            qname: Rc::new(QName::new("", "class")),
            value: "container".into(),
        };
        let event = ExiEvent::Attribute(content.clone());
        let ctx = default_ctx();

        let mut writer = BitWriter::new();
        encode_event(&mut writer, &event, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_attribute(&mut reader, &ctx, no_prefix_resolver).unwrap();
        assert_eq!(&*decoded.qname.local_name, "class");
        assert_eq!(&*decoded.value, "container");
    }

    /// Spec Table 4-2: encode_event für CH.
    #[test]
    fn encode_event_ch() {
        let content = ChContent {
            value: "Text content".into(),
        };
        let event = ExiEvent::Characters(content.clone());
        let ctx = default_ctx();

        let mut writer = BitWriter::new();
        encode_event(&mut writer, &event, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_characters(&mut reader).unwrap();
        assert_eq!(&*decoded.value, "Text content");
    }

    /// Spec Table 4-2: encode_event für NS.
    #[test]
    fn encode_event_ns() {
        let content = NsContent {
            uri: "http://example.org".into(),
            prefix: "ex".into(),
            local_element_ns: true,
        };
        let event = ExiEvent::NamespaceDeclaration(content.clone());
        let ctx = default_ctx();

        let mut writer = BitWriter::new();
        encode_event(&mut writer, &event, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let decoded = decode_namespace_declaration(&mut reader, &ContentContext::default()).unwrap();
        assert_eq!(&*decoded.uri, "http://example.org");
        assert_eq!(&*decoded.prefix, "ex");
        assert!(decoded.local_element_ns);
    }

    /// Spec Table 4-2: encode_event für alle Event-Typen ohne panic.
    #[test]
    fn encode_event_alle_typen() {
        let ctx = default_ctx();
        let events = [
            ExiEvent::StartDocument,
            ExiEvent::EndDocument,
            ExiEvent::StartElement(Rc::new(QName::new("", "test"))),
            ExiEvent::EndElement,
            ExiEvent::Attribute(AtContent {
                qname: Rc::new(QName::new("", "a")),
                value: "v".into(),
            }),
            ExiEvent::Characters(ChContent {
                value: "c".into(),
            }),
            ExiEvent::NamespaceDeclaration(NsContent {
                uri: "u".into(),
                prefix: "p".into(),
                local_element_ns: false,
            }),
            ExiEvent::Comment(CmContent {
                text: "cm".into(),
            }),
            ExiEvent::ProcessingInstruction(PiContent {
                name: "pi".into(),
                text: "".into(),
            }),
            ExiEvent::DocType(DtContent {
                name: "dt".into(),
                public: "".into(),
                system: "".into(),
                text: "".into(),
            }),
            ExiEvent::EntityReference(ErContent {
                name: "er".into(),
            }),
            ExiEvent::SelfContained,
        ];

        for event in &events {
            let mut writer = BitWriter::new();
            encode_event(&mut writer, event, &ctx);
            // Sollte nicht paniken
        }
    }

    // ========================================================================
    // Negativtests: EOF / Truncated Streams
    // ========================================================================

    /// Spec 7: Truncated Stream bei SE decode → PrematureEndOfStream.
    #[test]
    fn se_truncated_stream() {
        let ctx = default_ctx();
        // Leerer Stream
        let mut reader = BitReader::new(&[]);
        let result = decode_start_element(&mut reader, &ctx, no_prefix_resolver);
        assert!(result.is_err());

        // Partieller Stream (nur URI-Länge, kein Inhalt)
        let mut reader = BitReader::new(&[0x05]); // Länge 5, aber keine Daten
        let result = decode_start_element(&mut reader, &ctx, no_prefix_resolver);
        assert!(result.is_err());
    }

    /// Spec 7: Truncated Stream bei AT decode → PrematureEndOfStream.
    #[test]
    fn at_truncated_stream() {
        let ctx = default_ctx();
        let mut reader = BitReader::new(&[]);
        let result = decode_attribute(&mut reader, &ctx, no_prefix_resolver);
        assert!(result.is_err());
    }

    /// Spec 7: Truncated Stream bei CH decode → PrematureEndOfStream.
    #[test]
    fn ch_truncated_stream() {
        assert_decode_empty_fails(decode_characters);
    }

    /// Spec 7: Truncated Stream bei NS decode → PrematureEndOfStream.
    #[test]
    fn ns_truncated_stream() {
        // Empty stream → PrematureEndOfStream
        let mut reader = BitReader::new(&[]);
        assert_eq!(
            decode_namespace_declaration(&mut reader, &ContentContext::default()).unwrap_err(),
            crate::Error::PrematureEndOfStream
        );

        // Partieller Stream (URI ok, aber prefix fehlt)
        let mut writer = BitWriter::new();
        string::encode(&mut writer, "http://example.org");
        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);
        assert!(decode_namespace_declaration(&mut reader, &ContentContext::default()).is_err());
    }

    /// Spec 7: Truncated Stream bei CM decode → PrematureEndOfStream.
    #[test]
    fn cm_truncated_stream() {
        assert_decode_empty_fails(decode_comment);
    }

    /// Spec 7: Truncated Stream bei PI decode → PrematureEndOfStream.
    #[test]
    fn pi_truncated_stream() {
        assert_decode_empty_fails(decode_processing_instruction);
    }

    /// Spec 7: Truncated Stream bei DT decode → PrematureEndOfStream.
    #[test]
    fn dt_truncated_stream() {
        assert_decode_empty_fails(decode_doctype);

        // Partieller Stream (name ok, aber public fehlt)
        let mut writer = BitWriter::new();
        string::encode(&mut writer, "html");
        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);
        assert!(decode_doctype(&mut reader).is_err());
    }

    /// Spec 7: Truncated Stream bei ER decode → PrematureEndOfStream.
    #[test]
    fn er_truncated_stream() {
        assert_decode_empty_fails(decode_entity_reference);
    }

    // ========================================================================
    // Negativtests: UnresolvedPrefix
    // ========================================================================

    /// Spec 7.1.7: UnresolvedPrefix wenn prefix_resolver None zurückgibt.
    #[test]
    fn se_unresolved_prefix() {
        let qname_val = QName::with_prefix("http://example.org", "element", "ex");
        let ctx = ctx_with_prefixes(2);

        let mut writer = BitWriter::new();
        encode_start_element(&mut writer, &qname_val, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let result = decode_start_element(&mut reader, &ctx, no_prefix_resolver);
        assert!(result.is_err());
    }

    /// Spec 7.1.7: UnresolvedPrefix bei AT decode.
    #[test]
    fn at_unresolved_prefix() {
        let content = AtContent {
            qname: Rc::new(QName::with_prefix("http://example.org", "attr", "ex")),
            value: "val".into(),
        };
        let ctx = ctx_with_prefixes(2);

        let mut writer = BitWriter::new();
        encode_attribute(&mut writer, &content, &ctx);
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let result = decode_attribute(&mut reader, &ctx, no_prefix_resolver);
        assert!(result.is_err());
    }
}
