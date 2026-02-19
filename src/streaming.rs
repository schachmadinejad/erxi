//! Streaming XML → EXI Encoding mit begrenztem Speicherverbrauch
//! (kein vollständiges Einlesen der XML-Eingabe).
//!
//! Nicht kompatibel mit Compression/PreCompression (Spec 9 erfordert Buffering).
//! String Table und Element-Grammars wachsen weiterhin mit der Dokumentstruktur.

use std::io::{Read, Write};
use std::path::Path;

use crate::encoder::{Encoder, EncoderConfig, auto_config, ensure_streaming_compatible};
use crate::memory_monitor::MemoryMonitor;
use crate::options::ExiOptions;
use crate::schema::SchemaInfo;
use crate::xml::{ParseFlags, emit_xml_events};
use crate::{Error, Result};

/// Schwellenwert für periodisches Flushing.
/// 64 KB als Kompromiss zwischen IO-Systemcalls (nicht pro Event) und Speicherbegrenzung.
const FLUSH_THRESHOLD: usize = 64 * 1024;

/// Encodiert XML streaming zu EXI mit begrenztem Speicherverbrauch:
/// XML-Input wird nicht vollständig in den Speicher geladen und der Bit-Buffer
/// wird periodisch geflusht. String Table und Element-Grammars wachsen weiterhin.
///
/// Nicht kompatibel mit Compression/PreCompression (Spec 9).
/// Internal Subset Entities (`<!ENTITY name "value">`) werden direkt aufgelöst.
/// Externe Entity-Deklarationen (`<!ENTITY name SYSTEM "datei">`) erfordern
/// `base_path` — ohne ihn wird `Error::DtdRequiresBatchApi` zurückgegeben.
pub fn encode_xml_stream(
    xml_reader: impl Read,
    exi_writer: impl Write,
    options: &ExiOptions,
    schema: Option<&SchemaInfo>,
) -> Result<()> {
    encode_xml_stream_with_config(xml_reader, exi_writer, options, schema, auto_config(options), None, None)
}

/// Encodiert XML streaming zu EXI mit expliziter Konfiguration.
///
/// `base_path` wird für die Auflösung externer Entity-Deklarationen
/// (`<!ENTITY name SYSTEM "datei">`) im Internal Subset benötigt.
/// Ohne base_path werden nur Inline-Entities aufgelöst.
pub fn encode_xml_stream_with_config(
    xml_reader: impl Read,
    exi_writer: impl Write,
    options: &ExiOptions,
    schema: Option<&SchemaInfo>,
    config: EncoderConfig,
    base_path: Option<&Path>,
    monitor: Option<MemoryMonitor>,
) -> Result<()> {
    encode_xml_stream_with_config_and_dtd_guard(
        xml_reader,
        exi_writer,
        options,
        schema,
        config,
        base_path,
        monitor,
        true,
    )
}

/// Wie `encode_xml_stream_with_config`, aber mit steuerbarem DTD-Guard.
///
/// `streaming_dtd_guard=false` entspricht dem alten stdin-Verhalten:
/// keine Aufloesung externer Entities und kein DTD-Fallback-Fehler.
pub fn encode_xml_stream_with_config_and_dtd_guard(
    xml_reader: impl Read,
    mut exi_writer: impl Write,
    options: &ExiOptions,
    schema: Option<&SchemaInfo>,
    config: EncoderConfig,
    base_path: Option<&Path>,
    monitor: Option<MemoryMonitor>,
    streaming_dtd_guard: bool,
) -> Result<()> {
    ensure_streaming_compatible(options)?;

    let mut encoder = if let Some(schema) = schema {
        Encoder::with_schema(options.clone(), config, schema.clone())?
    } else {
        Encoder::new(options.clone(), config)?
    };

    if let Some(monitor) = monitor {
        encoder.set_memory_monitor(monitor);
    }

    let flags = ParseFlags::from_options(options);

    emit_xml_events(xml_reader, &flags, streaming_dtd_guard, base_path, |event| {
        encoder.encode_xml_event(&event)?;
        if encoder.buf_len() >= FLUSH_THRESHOLD {
            encoder.flush_to(&mut exi_writer)?;
        }
        Ok(())
    })?;

    encoder.finish_to(&mut exi_writer)
}

/// Encodiert eine XML-Datei streaming zu einer EXI-Datei.
pub fn encode_xml_file(
    xml_path: &Path,
    exi_path: &Path,
    options: &ExiOptions,
    schema: Option<&SchemaInfo>,
) -> Result<()> {
    let xml_file = std::fs::File::open(xml_path)
        .map_err(|e| Error::IoError(format!("XML-Datei oeffnen: {e}")))?;
    let exi_file = std::fs::File::create(exi_path)
        .map_err(|e| Error::IoError(format!("EXI-Datei erstellen: {e}")))?;
    let exi_writer = std::io::BufWriter::new(exi_file);
    encode_xml_stream_with_config(xml_file, exi_writer, options, schema, auto_config(options), xml_path.parent(), None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decoder::decode;
    use crate::encoder::encode;
    use crate::event::ExiEvent;
    use crate::options::Alignment;

    /// Byte-Parität: Streaming und Batch müssen identische EXI-Bytes erzeugen.
    #[test]
    fn streaming_matches_batch_simple() {
        let xml = r#"<?xml version="1.0"?><root><child>Hello</child></root>"#;
        let opts = ExiOptions::default();

        // Batch
        let events = crate::xml::parse_xml_events_from_str(xml, &opts).unwrap();
        let batch_bytes = encode(&events, &opts).unwrap();

        // Streaming
        let mut stream_bytes = Vec::new();
        encode_xml_stream(
            std::io::Cursor::new(xml.as_bytes()),
            &mut stream_bytes,
            &opts,
            None,
        ).unwrap();

        assert_eq!(batch_bytes, stream_bytes, "Streaming und Batch EXI-Bytes unterscheiden sich");
    }

    /// Byte-Parität mit ByteAlignment.
    #[test]
    fn streaming_matches_batch_byte_aligned() {
        let xml = r#"<?xml version="1.0"?><root attr="value"><child>Text</child></root>"#;
        let mut opts = ExiOptions::default();
        opts.set_alignment(Alignment::ByteAlignment);

        let events = crate::xml::parse_xml_events_from_str(xml, &opts).unwrap();
        let batch_bytes = crate::encoder::encode_with_config(
            &events, &opts,
            EncoderConfig { include_options: true, ..EncoderConfig::default() },
        ).unwrap();

        let mut stream_bytes = Vec::new();
        encode_xml_stream(
            std::io::Cursor::new(xml.as_bytes()),
            &mut stream_bytes,
            &opts,
            None,
        ).unwrap();

        assert_eq!(batch_bytes, stream_bytes);
    }

    /// Streaming Round-Trip: XML → EXI → Decode → Events vergleichen.
    #[test]
    fn streaming_round_trip() {
        let xml = r#"<?xml version="1.0"?><doc><p>Hallo Welt</p><p>Zweiter Absatz</p></doc>"#;
        let opts = ExiOptions::default();

        let mut exi_bytes = Vec::new();
        encode_xml_stream(
            std::io::Cursor::new(xml.as_bytes()),
            &mut exi_bytes,
            &opts,
            None,
        ).unwrap();

        let (events, _) = decode(&exi_bytes).unwrap();
        // Prüfe Grundstruktur
        assert!(matches!(events[0], ExiEvent::StartDocument));
        assert!(matches!(events.last().unwrap(), ExiEvent::EndDocument));
        // Mindestens SD, SE(doc), SE(p), CH, EE, SE(p), CH, EE, EE, ED
        assert!(events.len() >= 10);
    }

    /// Compression → sofortiger Fehler.
    #[test]
    fn streaming_compression_error() {
        let xml = "<root/>";
        let mut opts = ExiOptions::default();
        opts.set_compression(true);

        let mut out = Vec::new();
        let err = encode_xml_stream(
            std::io::Cursor::new(xml.as_bytes()),
            &mut out,
            &opts,
            None,
        ).unwrap_err();
        assert!(err.to_string().contains("Compression"), "{err}");
    }

    /// PreCompression → sofortiger Fehler.
    #[test]
    fn streaming_precompression_error() {
        let xml = "<root/>";
        let mut opts = ExiOptions::default();
        opts.set_alignment(Alignment::PreCompression);

        let mut out = Vec::new();
        let err = encode_xml_stream(
            std::io::Cursor::new(xml.as_bytes()),
            &mut out,
            &opts,
            None,
        ).unwrap_err();
        assert!(err.to_string().contains("PreCompression"), "{err}");
    }

    /// DTD mit Internal Subset Entities → im Streaming aufgelöst.
    /// Byte-Parität mit datei-basiertem Batch-Pfad.
    #[test]
    fn streaming_dtd_internal_entities_resolved() {
        let xml = r#"<?xml version="1.0"?>
<!DOCTYPE root [
  <!ENTITY hello "world">
]>
<root>&hello;</root>"#;

        let dir = std::env::temp_dir();
        let xml_path = dir.join("erxi_dtd_entity_test.xml");
        std::fs::write(&xml_path, xml).unwrap();

        let opts = ExiOptions::default();

        // Batch (datei-basiert, löst DTD-Entities auf)
        let events = crate::xml::parse_xml_events_with_options(&xml_path, &opts).unwrap();
        let batch_bytes = encode(&events, &opts).unwrap();

        // Streaming (löst Internal Subset Entities auf)
        let mut stream_bytes = Vec::new();
        encode_xml_stream(
            std::io::Cursor::new(xml.as_bytes()),
            &mut stream_bytes,
            &opts,
            None,
        ).unwrap();

        assert_eq!(batch_bytes, stream_bytes,
            "DTD-Entity-Auflösung im Streaming weicht von Batch ab");

        // Round-Trip: decodiertes Ergebnis enthält "world"
        let (decoded, _) = decode(&stream_bytes).unwrap();
        let ch_values: Vec<_> = decoded.iter().filter_map(|e| {
            if let ExiEvent::Characters(ch) = e { Some(ch.value.as_ref()) } else { None }
        }).collect();
        assert!(ch_values.iter().any(|v| v.contains("world")),
            "Entity &hello; wurde nicht zu 'world' aufgelöst: {ch_values:?}");

        let _ = std::fs::remove_file(&xml_path);
    }

    /// Einfacher DOCTYPE ohne Entities → kein Fehler im Streaming.
    #[test]
    fn streaming_simple_doctype_ok() {
        let xml = r#"<?xml version="1.0"?>
<!DOCTYPE root SYSTEM "root.dtd">
<root/>"#;
        let opts = ExiOptions::default();

        let mut out = Vec::new();
        encode_xml_stream(
            std::io::Cursor::new(xml.as_bytes()),
            &mut out,
            &opts,
            None,
        ).unwrap();
        assert!(!out.is_empty());
    }

    /// CH-Coalescing mit Entity-Referenzen: a&amp;b → "a&b" als ein CH-Event.
    /// Byte-Parität mit Batch.
    #[test]
    fn streaming_entity_coalescing_parity() {
        let xml = r#"<?xml version="1.0"?><root>a&amp;b</root>"#;
        let opts = ExiOptions::default();

        let events = crate::xml::parse_xml_events_from_str(xml, &opts).unwrap();
        let batch_bytes = encode(&events, &opts).unwrap();

        let mut stream_bytes = Vec::new();
        encode_xml_stream(
            std::io::Cursor::new(xml.as_bytes()),
            &mut stream_bytes,
            &opts,
            None,
        ).unwrap();

        assert_eq!(batch_bytes, stream_bytes,
            "Entity-Coalescing im Streaming weicht von Batch ab");
    }

    /// finish_to() mit offenen Elementen → Fehler.
    #[test]
    fn finish_to_with_open_elements() {
        let opts = ExiOptions::default();
        let config = EncoderConfig::default();
        let mut encoder = crate::encoder::Encoder::new(opts, config).unwrap();

        // SD + SE ohne EE → offenes Element
        encoder.encode_event(&ExiEvent::StartDocument).unwrap();
        encoder.encode_event(&ExiEvent::StartElement(
            std::rc::Rc::new(crate::qname::QName::new("", "root")),
        )).unwrap();

        let mut out = Vec::new();
        let err = encoder.finish_to(&mut out).unwrap_err();
        assert!(err.to_string().contains("offene Elemente"), "{err}");
    }

    /// SYSTEM-Entity im Internal Subset mit base_path, aber Datei fehlt → Fehler.
    #[test]
    fn streaming_external_entity_file_missing() {
        let xml = r#"<?xml version="1.0"?>
<!DOCTYPE root [
  <!ENTITY ext SYSTEM "nonexistent.ent">
]>
<root>&ext;</root>"#;
        let opts = ExiOptions::default();
        let config = EncoderConfig::default();

        let mut out = Vec::new();
        let err = encode_xml_stream_with_config(
            std::io::Cursor::new(xml.as_bytes()),
            &mut out,
            &opts,
            None,
            config,
            Some(Path::new("/tmp")),
            None,
        ).unwrap_err();
        assert!(err.to_string().contains("nicht lesbar"), "{err}");
    }

    /// SYSTEM-Entity im Internal Subset ohne base_path → DtdRequiresBatchApi.
    #[test]
    fn streaming_external_entity_without_base_path() {
        let xml = r#"<?xml version="1.0"?>
<!DOCTYPE root [
  <!ENTITY ext SYSTEM "ext.ent">
]>
<root>&ext;</root>"#;
        let opts = ExiOptions::default();

        let mut out = Vec::new();
        let err = encode_xml_stream(
            std::io::Cursor::new(xml.as_bytes()),
            &mut out,
            &opts,
            None,
        ).unwrap_err();
        assert!(matches!(err, crate::Error::DtdRequiresBatchApi), "{err}");
    }

    /// encode_xml_file Round-Trip.
    #[test]
    fn encode_xml_file_round_trip() {
        let dir = std::env::temp_dir();
        let xml_path = dir.join("erxi_streaming_test.xml");
        let exi_path = dir.join("erxi_streaming_test.exi");

        std::fs::write(&xml_path, "<root><item>Test</item></root>").unwrap();

        let opts = ExiOptions::default();
        encode_xml_file(&xml_path, &exi_path, &opts, None).unwrap();

        let exi_bytes = std::fs::read(&exi_path).unwrap();
        let (events, _) = decode(&exi_bytes).unwrap();
        assert!(matches!(events[0], ExiEvent::StartDocument));

        // Aufräumen
        let _ = std::fs::remove_file(&xml_path);
        let _ = std::fs::remove_file(&exi_path);
    }
}
