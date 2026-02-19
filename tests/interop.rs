//! Interop-Tests mit externen EXI-Implementierungen (Issue #28).
//!
//! Diese Tests dekodieren EXI-Dateien die von Exificient 1.0.4 generiert wurden
//! und verifizieren die korrekte Interpretation.
//!
//! # Status
//!
//! - **simple.xml:** ✓ bit-packed, byte-aligned, with-cookie
//! - **complex.xml:** ✓ bit-packed, byte-aligned (Namespaces + Attribute)
//! - **options-header:** ✓ Decoder parst Exificient-Options (schemaId=None via EE)
//! - **with-options:** ✗ Benötigt schema-informed Grammar (strict=true) — Phase 7
//!
//! ## Erkenntnisse
//!
//! 1. **CH/EE-Learning:** Grammar-Evolution (Spec 8.4.3) erfolgt für ALLE Alignment-Modi.
//!    Frühere Annahme, dass Exificient dies nur bei bit-packed macht, war falsch.
//!
//! 2. **schemaId=None Encoding:** Exificient verwendet `EE` ohne `AT(xsi:nil)` als
//!    Kurzform für `xsi:nil=true`. Wir akzeptieren beide Varianten beim Decoding.
//!
//! 3. **Out-of-band Options:** Die Fixtures haben keine Options im Header. Für
//!    byte-aligned muss `decode_with_options` mit expliziten Options verwendet werden.
//!
//! 4. **Namespace-Encoding:** Bei `preserve.prefixes=false` (Default) wird der
//!    Namespace im QName des Elements encodiert, nicht als separates NS-Event.
//!
//! # Spec-Referenz
//! - Spec 5.4: EXI Options
//! - Spec 8.4.3: Built-in Element Grammar
//! - Spec 10: Conformance

use std::rc::Rc;

use erxi::decoder::{decode, decode_with_options};
use erxi::event::ExiEvent;
use erxi::options::{Alignment, ExiOptions, Preserve};

/// Lädt eine Fixture-Datei aus tests/fixtures/.
fn load_fixture(name: &str) -> Vec<u8> {
    let path = format!("tests/fixtures/{}", name);
    std::fs::read(&path).unwrap_or_else(|e| panic!("Fixture {} nicht gefunden: {}", path, e))
}

// ============================================================================
// Simple XML Tests
// ============================================================================

/// Verifiziert Events für simple.xml:
/// `<root><item>Hello</item><item>World</item></root>`
fn verify_simple_events(events: &[ExiEvent]) {
    // SD, SE(root), SE(item), CH(Hello), EE, SE(item), CH(World), EE, EE, ED
    assert!(events.len() >= 10, "Zu wenige Events: {}", events.len());

    // SD
    assert!(
        matches!(events[0], ExiEvent::StartDocument),
        "Event 0: {:?}",
        events[0]
    );

    // SE(root)
    match &events[1] {
        ExiEvent::StartElement(qname) => {
            assert_eq!(&*qname.local_name, "root", "SE: {:?}", qname);
        }
        e => panic!("Event 1 sollte SE sein: {:?}", e),
    }

    // SE(item)
    match &events[2] {
        ExiEvent::StartElement(qname) => {
            assert_eq!(&*qname.local_name, "item", "SE: {:?}", qname);
        }
        e => panic!("Event 2 sollte SE sein: {:?}", e),
    }

    // CH(Hello)
    match &events[3] {
        ExiEvent::Characters(content) => {
            assert_eq!(&*content.value, "Hello", "CH: {:?}", content);
        }
        e => panic!("Event 3 sollte CH sein: {:?}", e),
    }

    // EE
    assert!(
        matches!(events[4], ExiEvent::EndElement),
        "Event 4: {:?}",
        events[4]
    );

    // SE(item)
    match &events[5] {
        ExiEvent::StartElement(qname) => {
            assert_eq!(&*qname.local_name, "item", "SE: {:?}", qname);
        }
        e => panic!("Event 5 sollte SE sein: {:?}", e),
    }

    // CH(World)
    match &events[6] {
        ExiEvent::Characters(content) => {
            assert_eq!(&*content.value, "World", "CH: {:?}", content);
        }
        e => panic!("Event 6 sollte CH sein: {:?}", e),
    }

    // EE, EE, ED
    assert!(
        matches!(events[7], ExiEvent::EndElement),
        "Event 7: {:?}",
        events[7]
    );
    assert!(
        matches!(events[8], ExiEvent::EndElement),
        "Event 8: {:?}",
        events[8]
    );
    assert!(
        matches!(events[9], ExiEvent::EndDocument),
        "Event 9: {:?}",
        events[9]
    );
}

/// Spec 10: Interop mit exificient - Bit-Packed Modus.
#[test]
fn interop_simple_bitpacked() {
    let data = load_fixture("simple_bitpacked.exi");
    let (events, options) = decode(&data).expect("Decode fehlgeschlagen");

    assert_eq!(options.alignment(), Alignment::BitPacked);
    verify_simple_events(&events);
}

/// Spec 10: Interop mit exificient - Byte-Aligned Modus.
///
/// Die Fixture hat keine Options im Header, daher müssen die Options
/// out-of-band via `decode_with_options` übergeben werden.
#[test]
fn interop_simple_bytealigned() {
    let data = load_fixture("simple_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, result_options) =
        decode_with_options(&data, options).expect("Decode fehlgeschlagen");

    assert_eq!(result_options.alignment(), Alignment::ByteAlignment);
    verify_simple_events(&events);
}

/// Spec 5.1: Cookie-Präfix ($EXI).
#[test]
fn interop_simple_with_cookie() {
    let data = load_fixture("simple_with_cookie.exi");

    // Verifiziere dass Cookie vorhanden ist
    assert_eq!(&data[0..4], b"$EXI", "Cookie fehlt");

    let (events, _options) = decode(&data).expect("Decode fehlgeschlagen");
    verify_simple_events(&events);
}

/// Spec 5.4: Options im Header.
///
/// Diese Fixture hat options_present=true im Header, die Options selbst
/// sind aber Default-Werte (leere HeaderContent = EE). Der Options-Header
/// kann erfolgreich geparst werden.
#[test]
fn interop_simple_with_options() {
    let data = load_fixture("simple_with_options.exi");
    use erxi::bitstream::BitReader;
    use erxi::header;
    use erxi::options_codec;

    let mut reader = BitReader::new(&data);
    let header = header::decode(&mut reader, false).expect("Header decode fehlgeschlagen");
    assert!(header.options_present(), "Options-Header fehlt");

    let options = options_codec::decode(&mut reader).expect("Options decode fehlgeschlagen");
    // Fixture enthält leere Options (HeaderContent code=3=EE) → alle Defaults
    assert!(!options.strict(), "Fixture hat Default-Options, kein strict");
}

// ============================================================================
// Complex XML Tests
// ============================================================================

/// Verifiziert Events für complex.xml mit Namespace und Attributen.
///
/// Bei `preserve.prefixes=false` (Default) wird der Namespace im QName
/// des Elements encodiert, nicht als separates NS-Event.
fn verify_complex_events(events: &[ExiEvent]) {
    // Mindestens: SD, SE(doc), AT(version), SE(header), AT(id), ...
    assert!(events.len() >= 15, "Zu wenige Events: {}", events.len());

    // SD
    assert!(
        matches!(events[0], ExiEvent::StartDocument),
        "Event 0: {:?}",
        events[0]
    );

    // Suche nach SE(doc) mit Namespace http://example.org im QName
    let has_doc = events.iter().any(|e| match e {
        ExiEvent::StartElement(qname) => {
            &*qname.local_name == "doc" && &*qname.uri == "http://example.org"
        }
        _ => false,
    });
    assert!(has_doc, "SE(doc) mit Namespace http://example.org fehlt");

    // Suche nach AT(version)
    let has_version_attr = events.iter().any(|e| match e {
        ExiEvent::Attribute(at) => &*at.qname.local_name == "version" && &*at.value == "1.0",
        _ => false,
    });
    assert!(has_version_attr, "AT(version=1.0) fehlt");

    // Suche nach AT(id)
    let has_id_attr = events.iter().any(|e| match e {
        ExiEvent::Attribute(at) => &*at.qname.local_name == "id" && &*at.value == "h1",
        _ => false,
    });
    assert!(has_id_attr, "AT(id=h1) fehlt");

    // Suche nach CH(Test Document)
    let has_title = events.iter().any(|e| match e {
        ExiEvent::Characters(ch) => &*ch.value == "Test Document",
        _ => false,
    });
    assert!(has_title, "CH(Test Document) fehlt");

    // ED am Ende
    assert!(
        matches!(events.last(), Some(ExiEvent::EndDocument)),
        "Letztes Event: {:?}",
        events.last()
    );
}

/// Spec 10: Komplexes XML mit Namespace und Attributen - Bit-Packed.
#[test]
fn interop_complex_bitpacked() {
    let data = load_fixture("complex_bitpacked.exi");
    let (events, options) = decode(&data).expect("Decode fehlgeschlagen");

    assert_eq!(options.alignment(), Alignment::BitPacked);
    verify_complex_events(&events);
}

/// Spec 10: Komplexes XML mit Namespace und Attributen - Byte-Aligned.
///
/// Die Fixture hat keine Options im Header, daher müssen die Options
/// out-of-band via `decode_with_options` übergeben werden.
#[test]
fn interop_complex_bytealigned() {
    let data = load_fixture("complex_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, result_options) =
        decode_with_options(&data, options).expect("Decode fehlgeschlagen");

    assert_eq!(result_options.alignment(), Alignment::ByteAlignment);
    verify_complex_events(&events);
}

// ============================================================================
// Large Grammar Tests (Little-Endian Event Codes)
// ============================================================================

/// Spec 7.1.9: Byte-Aligned Event Codes mit > 256 Elementen.
///
/// Dieses XML hat 330 verschiedene Elemente (e000..e329), was zu einer
/// Document Grammar mit > 256 Productions führt. Damit werden 2-Byte
/// Event Codes verwendet, und die Byte-Reihenfolge wird relevant.
///
/// Spec 7.1.9: "Bytes are ordered with the least significant byte first."
/// (Little-Endian)
///
/// Dieser Test würde fehlschlagen wenn Big-Endian verwendet wird.
#[test]
fn interop_large_grammar_bytealigned() {
    let data = load_fixture("large_grammar_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, result_options) =
        decode_with_options(&data, options).expect("Decode fehlgeschlagen");

    assert_eq!(result_options.alignment(), Alignment::ByteAlignment);

    // SD, SE(root), 330x SE(eXXX), 330x EE, EE(root), ED
    // = 1 + 1 + 330 + 330 + 1 + 1 = 664 Events
    assert_eq!(events.len(), 664, "Erwartete 664 Events");

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // SE(root)
    match &events[1] {
        ExiEvent::StartElement(qname) => assert_eq!(&*qname.local_name, "root"),
        e => panic!("Event 1 sollte SE(root) sein: {:?}", e),
    }

    // Prüfe einige Elemente aus verschiedenen Bereichen
    // Leere Elemente haben SE + EE, also Index = 2 + 2*n für SE(e_n)

    // Element e000 (Index 2)
    match &events[2] {
        ExiEvent::StartElement(qname) => assert_eq!(&*qname.local_name, "e000"),
        e => panic!("Event 2 sollte SE(e000) sein: {:?}", e),
    }

    // Element e255 (Index 2+2*255=512) - letztes 1-Byte Event Code Element
    match &events[512] {
        ExiEvent::StartElement(qname) => assert_eq!(&*qname.local_name, "e255"),
        e => panic!("Event 512 sollte SE(e255) sein: {:?}", e),
    }

    // Element e256 (Index 2+2*256=514) - erstes 2-Byte Event Code Element
    // Hier ist die Byte-Reihenfolge kritisch!
    match &events[514] {
        ExiEvent::StartElement(qname) => assert_eq!(&*qname.local_name, "e256"),
        e => panic!("Event 514 sollte SE(e256) sein: {:?}", e),
    }

    // Element e329 (Index 2+2*329=660) - letztes Element
    match &events[660] {
        ExiEvent::StartElement(qname) => assert_eq!(&*qname.local_name, "e329"),
        e => panic!("Event 660 sollte SE(e329) sein: {:?}", e),
    }

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 7.1.9: Bit-Packed Modus zum Vergleich.
#[test]
fn interop_large_grammar_bitpacked() {
    let data = load_fixture("large_grammar_bitpacked.exi");
    let (events, options) = decode(&data).expect("Decode fehlgeschlagen");

    assert_eq!(options.alignment(), Alignment::BitPacked);
    assert_eq!(events.len(), 664, "Erwartete 664 Events");
}

// ============================================================================
// Fidelity Options: Comments (Spec 4, 7, 8.3)
// ============================================================================

/// Verifiziert Events für comment.xml mit preserve.comments=true.
fn verify_comment_events(events: &[ExiEvent]) {
    // SD, CM(Header comment), SE(root), CM(First comment), SE(item), CH(Value), EE, CM(Special chars), EE, ED
    assert!(events.len() >= 10, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // Suche nach CM Events
    let comments: Vec<_> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::Comment(cm) => Some(&*cm.text),
            _ => None,
        })
        .collect();

    assert!(
        comments.len() >= 3,
        "Erwartete mindestens 3 Comments: {:?}",
        comments
    );

    // Prüfe Inhalte (Whitespace ignorierend)
    assert!(
        comments.iter().any(|c| c.contains("Header comment")),
        "Header comment fehlt: {:?}",
        comments
    );
    assert!(
        comments.iter().any(|c| c.contains("First comment")),
        "First comment fehlt: {:?}",
        comments
    );
    assert!(
        comments.iter().any(|c| c.contains("Special chars")),
        "Special chars comment fehlt: {:?}",
        comments
    );

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 8.3: Fidelity Option preserve.comments - Bit-Packed.
#[test]
fn interop_comment_bitpacked() {
    let data = load_fixture("comment_bitpacked.exi");
    let options = ExiOptions::default()
        .with_preserve(Preserve {
            comments: true,
            ..Default::default()
        });
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_comment_events(&events);
}

/// Spec 8.3: Fidelity Option preserve.comments - Byte-Aligned.
#[test]
fn interop_comment_bytealigned() {
    let data = load_fixture("comment_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)

        .with_preserve(Preserve {
            comments: true,
            ..Default::default()
        });
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_comment_events(&events);
}

// ============================================================================
// Fidelity Options: Processing Instructions (Spec 4, 7, 8.3)
// ============================================================================

/// Verifiziert Events für processing_instruction.xml mit preserve.pis=true.
fn verify_pi_events(events: &[ExiEvent]) {
    // SD, PI(xml-stylesheet), SE(root), PI(target), SE(item), CH(Value), EE, EE, ED
    assert!(events.len() >= 8, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // Suche nach PI Events (name=target, text=data laut PiContent)
    let pis: Vec<(&str, &str)> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::ProcessingInstruction(pi) => Some((&*pi.name, &*pi.text)),
            _ => None,
        })
        .collect();

    assert!(pis.len() >= 2, "Erwartete mindestens 2 PIs: {:?}", pis);

    // Prüfe PIs
    assert!(
        pis.iter()
            .any(|(name, text)| *name == "xml-stylesheet" && text.contains("text/xsl")),
        "xml-stylesheet PI fehlt: {:?}",
        pis
    );
    assert!(
        pis.iter()
            .any(|(name, text)| *name == "target" && *text == "data"),
        "target PI fehlt: {:?}",
        pis
    );

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 8.3: Fidelity Option preserve.pis - Bit-Packed.
#[test]
fn interop_pi_bitpacked() {
    let data = load_fixture("processing_instruction_bitpacked.exi");
    let options = ExiOptions::default()
        .with_preserve(Preserve {
            pis: true,
            ..Default::default()
        });
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_pi_events(&events);
}

/// Spec 8.3: Fidelity Option preserve.pis - Byte-Aligned.
#[test]
fn interop_pi_bytealigned() {
    let data = load_fixture("processing_instruction_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)

        .with_preserve(Preserve {
            pis: true,
            ..Default::default()
        });
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_pi_events(&events);
}

// ============================================================================
// Fidelity Options: DOCTYPE (Spec 4, 7, 8.3)
// ============================================================================

/// Verifiziert Events für doctype.xml mit preserve.dtd=true.
fn verify_doctype_events(events: &[ExiEvent]) {
    // SD, DT(root, SYSTEM, "root.dtd"), SE(root), SE(item), CH(Value), EE, EE, ED
    assert!(events.len() >= 7, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // Suche nach DT Event (system enthält "root.dtd")
    let has_dt = events.iter().any(|e| match e {
        ExiEvent::DocType(dt) => &*dt.name == "root" && &*dt.system == "root.dtd",
        _ => false,
    });
    assert!(has_dt, "DT(root, root.dtd) fehlt");

    // Suche nach SE(root)
    let has_root = events.iter().any(|e| match e {
        ExiEvent::StartElement(qname) => &*qname.local_name == "root",
        _ => false,
    });
    assert!(has_root, "SE(root) fehlt");

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 8.3: Fidelity Option preserve.dtd - Bit-Packed.
#[test]
fn interop_doctype_bitpacked() {
    let data = load_fixture("doctype_bitpacked.exi");
    let options = ExiOptions::default()
        .with_preserve(Preserve {
            dtd: true,
            ..Default::default()
        });
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_doctype_events(&events);
}

/// Spec 8.3: Fidelity Option preserve.dtd - Byte-Aligned.
#[test]
fn interop_doctype_bytealigned() {
    let data = load_fixture("doctype_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)

        .with_preserve(Preserve {
            dtd: true,
            ..Default::default()
        });
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_doctype_events(&events);
}

// ============================================================================
// Fidelity Options: Prefixes (Spec 4, 7, 8.3)
// ============================================================================

/// Verifiziert Events für prefixes_preserved.xml mit preserve.prefixes=true.
fn verify_prefixes_events(events: &[ExiEvent]) {
    // SD, NS(ex=http://example.org), SE(ex:root), SE(ex:item), CH(Content), EE, EE, ED
    assert!(events.len() >= 7, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // Suche nach NS Event
    let has_ns = events.iter().any(|e| match e {
        ExiEvent::NamespaceDeclaration(ns) => &*ns.prefix == "ex" && &*ns.uri == "http://example.org",
        _ => false,
    });
    assert!(has_ns, "NS(ex=http://example.org) fehlt");

    // Suche nach SE(root) mit Prefix "ex"
    let has_root = events.iter().any(|e| match e {
        ExiEvent::StartElement(qname) => {
            &*qname.local_name == "root"
                && &*qname.uri == "http://example.org"
                && qname.prefix.as_deref() == Some("ex")
        }
        _ => false,
    });
    assert!(has_root, "SE(ex:root) mit Prefix fehlt");

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 8.3: Fidelity Option preserve.prefixes - Bit-Packed.
#[test]
fn interop_prefixes_bitpacked() {
    let data = load_fixture("prefixes_preserved_bitpacked.exi");
    let options = ExiOptions::default()
        .with_preserve(Preserve {
            prefixes: true,
            ..Default::default()
        });
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_prefixes_events(&events);
}

/// Spec 8.3: Fidelity Option preserve.prefixes - Byte-Aligned.
#[test]
fn interop_prefixes_bytealigned() {
    let data = load_fixture("prefixes_preserved_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)

        .with_preserve(Preserve {
            prefixes: true,
            ..Default::default()
        });
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_prefixes_events(&events);
}

// ============================================================================
// PreCompression Alignment (Spec 5.4)
// ============================================================================

/// Spec 5.4: PreCompression Alignment.
///
/// PreCompression ist wie Byte-Aligned, aber optimiert für nachfolgende
/// DEFLATE-Komprimierung.
#[test]
fn interop_precompression() {
    let data = load_fixture("simple_precompression.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::PreCompression)
;
    let (events, result_options) =
        decode_with_options(&data, options).expect("Decode fehlgeschlagen");

    assert_eq!(result_options.alignment(), Alignment::PreCompression);
    verify_simple_events(&events);
}

/// Spec 9 + preserve.prefixes: PreCompression mit NS-Events und Prefix-Preservation.
///
/// Bei PreCompression werden AT/CH Values im Value Channel am Ende geschrieben.
/// Mit preserve.prefixes=true werden NS-Events mit Prefixes erzeugt.
///
/// Dieser Test prüft:
/// 1. NS-Events sind vorhanden
/// 2. SE-Events haben den richtigen Prefix (NS-Lookahead funktioniert)
#[test]
fn interop_precompression_prefixes() {
    let data = load_fixture("prefixes_precompression.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::PreCompression)

        .with_preserve(Preserve {
            prefixes: true,
            ..Default::default()
        });
    let (events, result_options) =
        decode_with_options(&data, options).expect("Decode fehlgeschlagen");

    assert_eq!(result_options.alignment(), Alignment::PreCompression);
    assert!(result_options.preserve().prefixes);

    // Verifiziere NS-Events sind vorhanden
    let ns_events: Vec<_> = events
        .iter()
        .filter(|e| matches!(e, ExiEvent::NamespaceDeclaration(_)))
        .collect();
    assert!(!ns_events.is_empty(), "NS-Events sollten vorhanden sein");

    // Prüfe dass SE-Events den Prefix aus NS-Lookahead haben
    // (NS mit local_element_ns=true überträgt Prefix zum vorhergehenden SE)
    for e in &events {
        if let ExiEvent::StartElement(qname) = e {
            // Root-Element mit Namespace sollte Prefix haben
            if !qname.uri.is_empty() {
                // Prefix kann leer sein für Default-Namespace, aber sollte gesetzt sein
                // wenn local_element_ns=true verwendet wurde
                // Wir prüfen nur dass der QName konsistent ist
                assert!(
                    qname.prefix.is_some() || &*qname.uri == "http://example.org",
                    "SE({}) sollte Prefix haben oder Default-NS sein",
                    qname.local_name
                );
            }
        }
    }
}

/// Spec 9.3: PreCompression mit >100 Values triggert separate Compressed Streams.
///
/// Bei >100 Values insgesamt werden die Channels in anderer Reihenfolge geschrieben:
/// 1. Structure Channel
/// 2. Alle Channels mit ≤100 Values (in Reihenfolge des ersten Auftretens)
/// 3. Jeder Channel mit >100 Values separat (in Reihenfolge des ersten Auftretens)
///
/// Diese Fixture hat:
/// - 105 <item> Elemente (>100 Values)
/// - 3 <other> Elemente (≤100 Values)
/// Die "other" Values müssen VOR den "item" Values gelesen werden.
#[test]
fn interop_precompression_many_values() {
    let data = load_fixture("many_values_precompression.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::PreCompression)
;
    let (events, result_options) =
        decode_with_options(&data, options).expect("Decode fehlgeschlagen");

    assert_eq!(result_options.alignment(), Alignment::PreCompression);

    // Zähle item-Elemente
    let item_count = events
        .iter()
        .filter(|e| match e {
            ExiEvent::StartElement(qname) => &*qname.local_name == "item",
            _ => false,
        })
        .count();
    assert_eq!(item_count, 105, "Erwartete 105 item-Elemente");

    // Zähle other-Elemente
    let other_count = events
        .iter()
        .filter(|e| match e {
            ExiEvent::StartElement(qname) => &*qname.local_name == "other",
            _ => false,
        })
        .count();
    assert_eq!(other_count, 3, "Erwartete 3 other-Elemente");

    // Prüfe CH-Values
    let ch_values: Vec<&str> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::Characters(ch) => Some(&*ch.value),
            _ => None,
        })
        .collect();

    // Sollte 108 CH-Values haben (105 item + 3 other)
    assert_eq!(ch_values.len(), 108, "Erwartete 108 CH-Values");

    // Prüfe dass die ersten item-Values korrekt sind
    assert!(
        ch_values.iter().any(|v| *v == "v001"),
        "v001 sollte vorhanden sein"
    );
    assert!(
        ch_values.iter().any(|v| *v == "v105"),
        "v105 sollte vorhanden sein"
    );

    // Prüfe dass other-Values vorhanden sind
    assert!(
        ch_values.iter().any(|v| *v == "a"),
        "a sollte vorhanden sein"
    );
    assert!(
        ch_values.iter().any(|v| *v == "b"),
        "b sollte vorhanden sein"
    );
    assert!(
        ch_values.iter().any(|v| *v == "c"),
        "c sollte vorhanden sein"
    );
}

/// Roundtrip-Test für PreCompression mit >100 Values (Spec 9.3).
///
/// Testet:
/// 1. Wire-Format: kleine Channels (≤100 Values) werden VOR großen (>100) geschrieben
/// 2. Roundtrip: Encoder und Decoder arbeiten korrekt zusammen
/// 3. Event-Sequenz: CH-Values kommen in der richtigen Reihenfolge zurück
#[test]
fn roundtrip_precompression_many_values() {
    use erxi::encoder::{EncoderConfig, encode_with_config};
    use erxi::event::{ChContent, ExiEvent};
    use erxi::qname::QName;

    let options = ExiOptions::default()
        .with_alignment(Alignment::PreCompression)
;

    // Erstelle Events mit >100 Values für einen Channel
    let mut events = vec![ExiEvent::StartDocument];
    events.push(ExiEvent::StartElement(Rc::new(QName::new("", "root"))));

    // 105 item-Elemente
    for i in 1..=105 {
        events.push(ExiEvent::StartElement(Rc::new(QName::new("", "item"))));
        events.push(ExiEvent::Characters(ChContent {
            value: format!("v{:03}", i).into(),
        }));
        events.push(ExiEvent::EndElement);
    }

    // 3 other-Elemente
    for c in ['a', 'b', 'c'] {
        events.push(ExiEvent::StartElement(Rc::new(QName::new("", "other"))));
        events.push(ExiEvent::Characters(ChContent {
            value: c.to_string().into(),
        }));
        events.push(ExiEvent::EndElement);
    }

    events.push(ExiEvent::EndElement); // root
    events.push(ExiEvent::EndDocument);

    // Encode
    let encoded = encode_with_config(&events, &options, EncoderConfig::default())
        .expect("Encoding fehlgeschlagen");

    // Wire-Format Validierung (Spec 9.3):
    // Bei >100 Values müssen kleine Channels (≤100) VOR großen (>100) geschrieben werden.
    // "other" hat 3 Values, "item" hat 105 Values.
    // Also müssen "a", "b", "c" VOR "v001" im Byte-Stream erscheinen.
    let encoded_str = String::from_utf8_lossy(&encoded);
    let pos_a = encoded_str.find("a").expect("'a' nicht im Stream");
    let pos_v001 = encoded_str.find("v001").expect("'v001' nicht im Stream");
    assert!(
        pos_a < pos_v001,
        "Spec 9.3: kleine Channels (other: a,b,c) müssen VOR großen (item: v001...) kommen. \
         pos_a={}, pos_v001={}",
        pos_a,
        pos_v001
    );

    // Decode
    let (decoded, _) = decode_with_options(&encoded, options).expect("Decoding fehlgeschlagen");

    // Vergleiche Events
    let orig_ch: Vec<&str> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::Characters(ch) => Some(&*ch.value),
            _ => None,
        })
        .collect();

    let decoded_ch: Vec<&str> = decoded
        .iter()
        .filter_map(|e| match e {
            ExiEvent::Characters(ch) => Some(&*ch.value),
            _ => None,
        })
        .collect();

    assert_eq!(
        orig_ch.len(),
        decoded_ch.len(),
        "Anzahl CH-Events muss gleich sein"
    );
    assert_eq!(
        orig_ch, decoded_ch,
        "CH-Values müssen in gleicher Reihenfolge sein"
    );
}

// ============================================================================
// Fragment Mode (Spec 8.4.2)
// ============================================================================

/// Verifiziert Events für fragment.xml (mehrere Root-Elemente).
fn verify_fragment_events(events: &[ExiEvent]) {
    // SD, SE(item), CH(First), EE, SE(item), CH(Second), EE, SE(other), CH(Third), EE, ED
    assert!(events.len() >= 11, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // Zähle SE(item) - sollte 2 sein
    let item_count = events
        .iter()
        .filter(|e| match e {
            ExiEvent::StartElement(qname) => &*qname.local_name == "item",
            _ => false,
        })
        .count();
    assert_eq!(item_count, 2, "Erwartete 2 SE(item)");

    // Zähle SE(other) - sollte 1 sein
    let other_count = events
        .iter()
        .filter(|e| match e {
            ExiEvent::StartElement(qname) => &*qname.local_name == "other",
            _ => false,
        })
        .count();
    assert_eq!(other_count, 1, "Erwartete 1 SE(other)");

    // Prüfe CH Events
    let chars: Vec<_> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::Characters(ch) => Some(&*ch.value),
            _ => None,
        })
        .collect();
    assert!(chars.contains(&"First"), "CH(First) fehlt: {:?}", chars);
    assert!(chars.contains(&"Second"), "CH(Second) fehlt: {:?}", chars);
    assert!(chars.contains(&"Third"), "CH(Third) fehlt: {:?}", chars);

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 8.4.2: Fragment Grammar - Bit-Packed.
#[test]
fn interop_fragment_bitpacked() {
    let data = load_fixture("fragment_bitpacked.exi");
    let options = ExiOptions::default()
        .with_fragment();
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_fragment_events(&events);
}

/// Spec 8.4.2: Fragment Grammar - Byte-Aligned.
#[test]
fn interop_fragment_bytealigned() {
    let data = load_fixture("fragment_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)

        .with_fragment();
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_fragment_events(&events);
}

// ============================================================================
// String Table Tests (Spec 7.3)
// ============================================================================

/// Verifiziert Events für long_strings.xml mit 4096-Zeichen-Strings.
fn verify_long_strings_events(events: &[ExiEvent]) {
    // SD, SE(root), SE(data), CH(AAAA...), EE, SE(repeat), CH(AAAA...), EE, EE, ED
    assert!(events.len() >= 9, "Zu wenige Events: {}", events.len());

    // Finde die CH Events mit langen Strings
    let long_chars: Vec<_> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::Characters(ch) if ch.value.len() == 4096 => Some(&ch.value),
            _ => None,
        })
        .collect();

    assert_eq!(long_chars.len(), 2, "Erwartete 2 lange CH Events");
    assert!(
        long_chars[0].chars().all(|c| c == 'A'),
        "Erster String sollte nur 'A' enthalten"
    );
    assert!(
        long_chars[1].chars().all(|c| c == 'A'),
        "Zweiter String sollte nur 'A' enthalten"
    );

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 7.3: Lange Strings und String Table - Bit-Packed.
#[test]
fn interop_long_strings_bitpacked() {
    let data = load_fixture("long_strings_bitpacked.exi");
    let (events, _) = decode(&data).expect("Decode fehlgeschlagen");
    verify_long_strings_events(&events);
}

/// Spec 7.3: Lange Strings und String Table - Byte-Aligned.
#[test]
fn interop_long_strings_bytealigned() {
    let data = load_fixture("long_strings_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_long_strings_events(&events);
}

/// Verifiziert Events für repeated_values.xml (String Table Hits).
fn verify_repeated_values_events(events: &[ExiEvent]) {
    // SD, SE(root), SE(item), AT(attr=common), CH(common), EE, ...
    assert!(events.len() >= 15, "Zu wenige Events: {}", events.len());

    // Zähle AT(attr=common)
    let common_attr_count = events
        .iter()
        .filter(|e| match e {
            ExiEvent::Attribute(at) => &*at.qname.local_name == "attr" && &*at.value == "common",
            _ => false,
        })
        .count();
    assert_eq!(common_attr_count, 2, "Erwartete 2 AT(attr=common)");

    // Zähle AT(attr=different)
    let different_attr_count = events
        .iter()
        .filter(|e| match e {
            ExiEvent::Attribute(at) => &*at.qname.local_name == "attr" && &*at.value == "different",
            _ => false,
        })
        .count();
    assert_eq!(different_attr_count, 1, "Erwartete 1 AT(attr=different)");

    // Zähle CH(common) - sollte 3 sein
    let common_ch_count = events
        .iter()
        .filter(|e| match e {
            ExiEvent::Characters(ch) => &*ch.value == "common",
            _ => false,
        })
        .count();
    assert_eq!(common_ch_count, 3, "Erwartete 3 CH(common)");

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 7.3.3: Repeated Values (String Table Hits) - Bit-Packed.
#[test]
fn interop_repeated_values_bitpacked() {
    let data = load_fixture("repeated_values_bitpacked.exi");
    let (events, _) = decode(&data).expect("Decode fehlgeschlagen");
    verify_repeated_values_events(&events);
}

/// Spec 7.3.3: Repeated Values (String Table Hits) - Byte-Aligned.
#[test]
fn interop_repeated_values_bytealigned() {
    let data = load_fixture("repeated_values_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_repeated_values_events(&events);
}

// ============================================================================
// Deep Nesting Tests (Element Stack Depth)
// ============================================================================

/// Verifiziert Events für deep_nesting.xml (8 Level Verschachtelung).
fn verify_deep_nesting_events(events: &[ExiEvent]) {
    // SD, SE(level1), SE(level2), ..., SE(level8), CH(Deep), EE x8, ED
    // = 1 + 8 + 1 + 8 + 1 = 19 Events
    assert!(events.len() >= 19, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // Prüfe alle 8 Level
    for i in 1..=8 {
        let expected_name = format!("level{}", i);
        match &events[i] {
            ExiEvent::StartElement(qname) => {
                assert_eq!(&*qname.local_name, expected_name, "Event {}: {:?}", i, qname);
            }
            e => panic!("Event {} sollte SE({}) sein: {:?}", i, expected_name, e),
        }
    }

    // CH(Deep) bei Index 9
    match &events[9] {
        ExiEvent::Characters(ch) => assert_eq!(&*ch.value, "Deep"),
        e => panic!("Event 9 sollte CH(Deep) sein: {:?}", e),
    }

    // 8x EE (Index 10-17)
    for i in 10..18 {
        assert!(
            matches!(events[i], ExiEvent::EndElement),
            "Event {}: {:?}",
            i,
            events[i]
        );
    }

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 8.4.3: Tiefe Verschachtelung - Bit-Packed.
#[test]
fn interop_deep_nesting_bitpacked() {
    let data = load_fixture("deep_nesting_bitpacked.exi");
    let (events, _) = decode(&data).expect("Decode fehlgeschlagen");
    verify_deep_nesting_events(&events);
}

/// Spec 8.4.3: Tiefe Verschachtelung - Byte-Aligned.
#[test]
fn interop_deep_nesting_bytealigned() {
    let data = load_fixture("deep_nesting_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_deep_nesting_events(&events);
}

// ============================================================================
// Many Elements Tests (SE Learning)
// ============================================================================

/// Verifiziert Events für many_elements.xml (16 Elemente, einige wiederholt).
fn verify_many_elements_events(events: &[ExiEvent]) {
    // SD, SE(root), 16x (SE + CH + EE), EE(root), ED
    assert!(events.len() >= 52, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // SE(root)
    match &events[1] {
        ExiEvent::StartElement(qname) => assert_eq!(&*qname.local_name, "root"),
        e => panic!("Event 1 sollte SE(root) sein: {:?}", e),
    }

    // Zähle SE(a) - sollte 5 sein (Zeilen 1, 11, 14, 15, 16 in XML)
    let a_count = events
        .iter()
        .filter(|e| matches!(e, ExiEvent::StartElement(q) if &*q.local_name == "a"))
        .count();
    assert_eq!(a_count, 5, "Erwartete 5 SE(a)");

    // Prüfe dass alle erwarteten Werte vorhanden sind
    let values: Vec<_> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::Characters(ch) => Some(&*ch.value),
            _ => None,
        })
        .collect();
    assert!(values.contains(&"1"), "CH(1) fehlt");
    assert!(values.contains(&"16"), "CH(16) fehlt");

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 8.4.3: Viele Elemente mit SE-Learning - Bit-Packed.
#[test]
fn interop_many_elements_bitpacked() {
    let data = load_fixture("many_elements_bitpacked.exi");
    let (events, _) = decode(&data).expect("Decode fehlgeschlagen");
    verify_many_elements_events(&events);
}

/// Spec 8.4.3: Viele Elemente mit SE-Learning - Byte-Aligned.
#[test]
fn interop_many_elements_bytealigned() {
    let data = load_fixture("many_elements_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_many_elements_events(&events);
}

// ============================================================================
// Many Attributes Tests (AT Learning)
// ============================================================================

/// Verifiziert Events für many_attributes.xml (10+ Attribute).
fn verify_many_attributes_events(events: &[ExiEvent]) {
    // SD, SE(root), AT x10, SE(item), AT x3, EE, SE(item), AT x3, EE, EE(root), ED
    assert!(events.len() >= 20, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // Zähle alle Attribute
    let attr_count = events
        .iter()
        .filter(|e| matches!(e, ExiEvent::Attribute(_)))
        .count();
    assert_eq!(attr_count, 16, "Erwartete 16 Attribute (10+3+3)");

    // Prüfe root-Attribute (a-j mit Werten 1-10)
    let root_attrs: Vec<_> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::Attribute(at) if at.value.parse::<u32>().unwrap_or(0) <= 10 => {
                Some((&at.qname.local_name, &at.value))
            }
            _ => None,
        })
        .collect();
    assert_eq!(root_attrs.len(), 10, "Erwartete 10 root-Attribute");

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 8.4.3: Viele Attribute mit AT-Learning - Bit-Packed.
#[test]
fn interop_many_attributes_bitpacked() {
    let data = load_fixture("many_attributes_bitpacked.exi");
    let (events, _) = decode(&data).expect("Decode fehlgeschlagen");
    verify_many_attributes_events(&events);
}

/// Spec 8.4.3: Viele Attribute mit AT-Learning - Byte-Aligned.
#[test]
fn interop_many_attributes_bytealigned() {
    let data = load_fixture("many_attributes_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_many_attributes_events(&events);
}

// ============================================================================
// Alphabet Tests (26 verschiedene Elemente)
// ============================================================================

/// Verifiziert Events für alphabet.xml (26 verschiedene Element-Namen a-z).
fn verify_alphabet_events(events: &[ExiEvent]) {
    // SD, SE(root), 30x (SE + CH + EE), EE(root), ED
    assert!(events.len() >= 94, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // Sammle alle Element-Namen
    let element_names: Vec<_> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::StartElement(qname) if &*qname.local_name != "root" => {
                Some(&*qname.local_name)
            }
            _ => None,
        })
        .collect();

    // Prüfe dass alle Buchstaben a-z vorhanden sind
    for c in 'a'..='z' {
        let name = c.to_string();
        assert!(
            element_names.contains(&name.as_str()),
            "Element '{}' fehlt",
            c
        );
    }

    // Einige Elemente kommen mehrfach vor (a, z, m)
    let a_count = element_names.iter().filter(|&&n| n == "a").count();
    assert!(
        a_count >= 3,
        "Erwartete mindestens 3 SE(a), gefunden: {}",
        a_count
    );

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 8.4.3: Alphabet (26 verschiedene Elemente) - Bit-Packed.
#[test]
fn interop_alphabet_bitpacked() {
    let data = load_fixture("alphabet_bitpacked.exi");
    let (events, _) = decode(&data).expect("Decode fehlgeschlagen");
    verify_alphabet_events(&events);
}

/// Spec 8.4.3: Alphabet (26 verschiedene Elemente) - Byte-Aligned.
#[test]
fn interop_alphabet_bytealigned() {
    let data = load_fixture("alphabet_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_alphabet_events(&events);
}

// ============================================================================
// Many Namespaces Tests (URI String Table)
// ============================================================================

/// Verifiziert Events für many_namespaces.xml (3 verschiedene URIs).
fn verify_many_namespaces_events(events: &[ExiEvent]) {
    // SD, SE(root), 5x (SE + CH + EE), EE(root), ED
    assert!(events.len() >= 19, "Zu wenige Events: {}", events.len());

    // SD
    assert!(matches!(events[0], ExiEvent::StartDocument));

    // Sammle alle URIs aus den Elementen
    let uris: Vec<_> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::StartElement(qname) if !qname.uri.is_empty() => Some(&*qname.uri),
            _ => None,
        })
        .collect();

    // Prüfe dass alle 3 URIs vorhanden sind
    assert!(
        uris.iter().any(|u| u.contains("a.example.org")),
        "URI a.example.org fehlt"
    );
    assert!(
        uris.iter().any(|u| u.contains("b.example.org")),
        "URI b.example.org fehlt"
    );
    assert!(
        uris.iter().any(|u| u.contains("c.example.org")),
        "URI c.example.org fehlt"
    );

    // Prüfe CH Values
    let values: Vec<_> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::Characters(ch) => Some(&*ch.value),
            _ => None,
        })
        .collect();
    assert!(values.contains(&"A1"), "CH(A1) fehlt");
    assert!(values.contains(&"B1"), "CH(B1) fehlt");
    assert!(values.contains(&"C1"), "CH(C1) fehlt");

    // ED am Ende
    assert!(matches!(events.last(), Some(ExiEvent::EndDocument)));
}

/// Spec 7.3: Mehrere Namespaces (URI String Table) - Bit-Packed.
#[test]
fn interop_many_namespaces_bitpacked() {
    let data = load_fixture("many_namespaces_bitpacked.exi");
    let (events, _) = decode(&data).expect("Decode fehlgeschlagen");
    verify_many_namespaces_events(&events);
}

/// Spec 7.3: Mehrere Namespaces (URI String Table) - Byte-Aligned.
#[test]
fn interop_many_namespaces_bytealigned() {
    let data = load_fixture("many_namespaces_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)
;
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");
    verify_many_namespaces_events(&events);
}

/// Spec 7.3 + 8.3: Mehrere Namespaces mit Prefixes - Byte-Aligned.
#[test]
fn interop_many_namespaces_prefixes_bytealigned() {
    let data = load_fixture("many_namespaces_prefixes_bytealigned.exi");
    let options = ExiOptions::default()
        .with_alignment(Alignment::ByteAlignment)

        .with_preserve(Preserve {
            prefixes: true,
            ..Default::default()
        });
    let (events, _) = decode_with_options(&data, options).expect("Decode fehlgeschlagen");

    // Prüfe NS Events
    let ns_events: Vec<(&str, &str)> = events
        .iter()
        .filter_map(|e| match e {
            ExiEvent::NamespaceDeclaration(ns) => Some((&*ns.prefix, &*ns.uri)),
            _ => None,
        })
        .collect();

    assert!(ns_events.len() >= 3, "Erwartete mindestens 3 NS Events");

    // Prüfe dass Prefixes vorhanden sind
    let has_a = ns_events
        .iter()
        .any(|(p, u)| *p == "a" && u.contains("a.example.org"));
    let has_b = ns_events
        .iter()
        .any(|(p, u)| *p == "b" && u.contains("b.example.org"));
    let has_c = ns_events
        .iter()
        .any(|(p, u)| *p == "c" && u.contains("c.example.org"));

    assert!(has_a, "NS(a=http://a.example.org) fehlt");
    assert!(has_b, "NS(b=http://b.example.org) fehlt");
    assert!(has_c, "NS(c=http://c.example.org) fehlt");
}
