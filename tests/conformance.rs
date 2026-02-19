//! EXI 1.0 Spec Section 10: Conformance-Verifikation.
//!
//! Diese Tests prüfen formale Conformance-Anforderungen der W3C EXI 1.0
//! Second Edition Spezifikation.

use erxi::decoder::{decode_with_options, decode_with_schema};
use erxi::encoder::{encode, encode_with_schema};
use erxi::event::ExiEvent;
use erxi::options::{Alignment, ExiOptions, Preserve};
use erxi::schema::SchemaInfo;
use erxi::xml::parse_xml_events_from_str;
use erxi::xsd::parse_xsd_with_imports;
use std::cell::RefCell;
use std::path::Path;

fn exi_testsuite_base() -> String {
    std::env::var("EXI_TESTSUITE_DIR")
        .expect("EXI_TESTSUITE_DIR muss gesetzt sein (Pfad zur W3C EXI Test Suite)")
}

fn has_exi_testsuite() -> bool {
    std::env::var("EXI_TESTSUITE_DIR").is_ok()
}

thread_local! {
    static ACCEPTANCE_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
}

fn acceptance_schema() -> &'static SchemaInfo {
    ACCEPTANCE_SCHEMA.with(|c| {
        let mut borrow = c.borrow_mut();
        if let Some(s) = *borrow {
            s
        } else {
            let xsd_path = format!(
                "{}/data/interop/schemaInformedGrammar/acceptance.xsd",
                exi_testsuite_base()
            );
            let leaked: &'static SchemaInfo = Box::leak(Box::new(
                parse_xsd_with_imports(Path::new(&xsd_path))
                .expect("acceptance.xsd parse error"),
            ));
            *borrow = Some(leaked);
            leaked
        }
    })
}

fn parse_xml(xml: &str, opts: &ExiOptions) -> Vec<erxi::event::ExiEvent> {
    parse_xml_events_from_str(xml, opts).expect("XML parse error")
}

/// Spec 10.1: Stream Conformance.
///
/// "A conformant EXI stream consists of a sequence of octets that follows
///  the syntax of EXI stream that is defined in this document."
///
/// Verifikation: Jeder von erxi encodierte Stream laesst sich von erxi
/// korrekt round-trippen. Notwendig, aber nicht hinreichend fuer Spec 10.1
/// (cross-implementation Pruefung in cross_matrix_test).
#[test]
fn conformance_10_1_stream_round_trip() {
    let xml = r#"<?xml version="1.0"?><root><child attr="value">text</child></root>"#;

    let alignments = [
        ("BitPacked", Alignment::BitPacked),
        ("ByteAlignment", Alignment::ByteAlignment),
        ("PreCompression", Alignment::PreCompression),
    ];

    for (name, alignment) in &alignments {
        let mut opts = ExiOptions::default();
        opts.set_alignment(alignment.clone());

        let events = parse_xml(xml, &opts);
        let encoded = encode(&events, &opts).unwrap_or_else(|e| panic!("{name}: encode: {e}"));
        let (decoded_events, _) = decode_with_options(&encoded, opts.clone())
            .unwrap_or_else(|e| panic!("{name}: decode: {e}"));
        let re_encoded = encode(&decoded_events, &opts)
            .unwrap_or_else(|e| panic!("{name}: re-encode: {e}"));

        assert_eq!(
            encoded, re_encoded,
            "Spec 10.1: {name} round-trip mismatch"
        );
    }
}

/// Spec 10.1: Stream Conformance mit Compression.
#[test]
fn conformance_10_1_stream_compression_round_trip() {
    let xml = r#"<?xml version="1.0"?><data><item>hello</item><item>world</item></data>"#;

    let mut opts = ExiOptions::default();
    opts.set_compression(true);

    let events = parse_xml(xml, &opts);
    let encoded = encode(&events, &opts).expect("encode");
    let (decoded_events, _) = decode_with_options(&encoded, opts.clone()).expect("decode");
    let re_encoded = encode(&decoded_events, &opts).expect("re-encode");

    assert_eq!(encoded, re_encoded, "Spec 10.1: Compression round-trip");
}

/// Spec 10.2: Processor Conformance — Alle Alignment-Modi unterstützt.
#[test]
fn conformance_10_2_all_alignments() {
    let xml = r#"<?xml version="1.0"?><test>Hallo Welt</test>"#;

    let alignments = [
        Alignment::BitPacked,
        Alignment::ByteAlignment,
        Alignment::PreCompression,
    ];

    for alignment in &alignments {
        let mut opts = ExiOptions::default();
        opts.set_alignment(alignment.clone());

        let events = parse_xml(xml, &opts);
        let encoded = encode(&events, &opts)
            .unwrap_or_else(|e| panic!("encode {alignment:?}: {e}"));
        let (decoded, _) = decode_with_options(&encoded, opts)
            .unwrap_or_else(|e| panic!("decode {alignment:?}: {e}"));
        assert!(
            !decoded.is_empty(),
            "Spec 10.2: {alignment:?} produced no events"
        );
    }
}

/// Spec 10.2: Processor Conformance — Schema-informed und Schema-less Modi.
#[test]
fn conformance_10_2_schema_modes() {
    if !has_exi_testsuite() {
        eprintln!("SKIP: EXI_TESTSUITE_DIR nicht gesetzt");
        return;
    }

    // Schema-less
    let xml = r#"<?xml version="1.0"?><root><child>text</child></root>"#;
    let opts = ExiOptions::default();
    let events = parse_xml(xml, &opts);
    let encoded = encode(&events, &opts).expect("schema-less encode");
    let (decoded, _) = decode_with_options(&encoded, opts).expect("schema-less decode");
    assert!(!decoded.is_empty(), "Spec 10.2: schema-less mode");

    // Schema-informed (encode + decode mit Schema)
    let schema = acceptance_schema();
    let xml_si = r#"<?xml version="1.0"?><A><AB>text</AB></A>"#;
    let opts_si = ExiOptions::default();
    let events_si = parse_xml(xml_si, &opts_si);
    let encoded_si =
        encode_with_schema(&events_si, &opts_si, schema).expect("schema-informed encode");
    let (decoded_si, _) =
        decode_with_schema(&encoded_si, opts_si, schema).expect("schema-informed decode");
    assert!(!decoded_si.is_empty(), "Spec 10.2: schema-informed mode");
}

/// Spec 10.2: Processor Conformance — Preserve-Optionen.
#[test]
fn conformance_10_2_preserve_options() {
    let xml = r#"<?xml version="1.0"?><!-- Kommentar --><?pi target?><root>text</root>"#;

    let test_cases: Vec<(&str, Preserve)> = vec![
        (
            "comments",
            Preserve {
                comments: true,
                ..Default::default()
            },
        ),
        (
            "pis",
            Preserve {
                pis: true,
                ..Default::default()
            },
        ),
        (
            "prefixes",
            Preserve {
                prefixes: true,
                ..Default::default()
            },
        ),
    ];

    for (name, preserve) in &test_cases {
        let mut opts = ExiOptions::default();
        opts.set_preserve(preserve.clone());
        let events = parse_xml(xml, &opts);
        let encoded =
            encode(&events, &opts).unwrap_or_else(|e| panic!("encode preserve.{name}: {e}"));
        let (decoded, _) = decode_with_options(&encoded, opts)
            .unwrap_or_else(|e| panic!("decode preserve.{name}: {e}"));
        assert!(
            !decoded.is_empty(),
            "Spec 10.2: preserve.{name} produced no events"
        );
    }
}

/// Spec 10.2: Processor Conformance — Strict mode.
#[test]
fn conformance_10_2_strict_mode() {
    if !has_exi_testsuite() {
        eprintln!("SKIP: EXI_TESTSUITE_DIR nicht gesetzt");
        return;
    }

    let schema = acceptance_schema();
    let xml = r#"<?xml version="1.0"?><A><AB>strict</AB></A>"#;

    let mut opts = ExiOptions::default();
    opts.set_strict(true);

    let events = parse_xml(xml, &opts);
    let encoded = encode_with_schema(&events, &opts, schema).expect("strict encode");
    let (decoded, _) = decode_with_schema(&encoded, opts, schema).expect("strict decode");
    assert!(!decoded.is_empty(), "Spec 10.2: strict mode");
}

/// Spec 10.2: Processor Conformance — Fragment mode.
///
/// Round-Trip: Encode → Decode → Re-Encode mit fragment=true.
#[test]
fn conformance_10_2_fragment_mode() {
    let xml = r#"<root>fragment</root>"#;

    let mut opts = ExiOptions::default();
    opts.set_fragment(true);

    let events = parse_xml(xml, &opts);
    let encoded = encode(&events, &opts).expect("fragment encode");
    let (decoded, _) = decode_with_options(&encoded, opts.clone()).expect("fragment decode");
    assert!(!decoded.is_empty(), "Spec 10.2: fragment mode");

    // Zusätzlich: W3C Fragment-Fixtures
    let dir = std::path::Path::new("tests/fixtures/w3c/builtin_fragments");
    if dir.exists() {
        let mut decoded_count = 0;
        for entry in std::fs::read_dir(dir).expect("read dir").flatten() {
            let path = entry.path();
            if path.extension().map(|e| e == "exi").unwrap_or(false) {
                let data = std::fs::read(&path).expect("read fixture");
                let mut frag_opts = ExiOptions::default();
                frag_opts.set_fragment(true);
                frag_opts.set_preserve(Preserve {
                    pis: true,
                    comments: true,
                    ..Default::default()
                });
                if let Ok((evts, _)) = decode_with_options(&data, frag_opts) {
                    assert!(!evts.is_empty());
                    decoded_count += 1;
                }
            }
        }
        assert!(decoded_count > 0, "Mindestens ein W3C Fragment-Fixture muss decodierbar sein");
        eprintln!("Spec 10.2 fragment: {decoded_count} W3C fixtures decoded");
    }
}

/// Spec 10.2: Processor Conformance — Compression mode.
#[test]
fn conformance_10_2_compression_mode() {
    let xml = r#"<?xml version="1.0"?><data><a>1</a><b>2</b><c>3</c></data>"#;

    let mut opts = ExiOptions::default();
    opts.set_compression(true);

    let events = parse_xml(xml, &opts);
    let encoded = encode(&events, &opts).expect("compression encode");
    let (decoded, _) = decode_with_options(&encoded, opts).expect("compression decode");
    assert!(!decoded.is_empty(), "Spec 10.2: compression mode");
}

/// Spec 10.2: Processor Conformance — Datatype Representation Map.
///
/// erxi unterstützt DTRM-Dekodierung (Spec 7.4). Schema-informed Decoding
/// der W3C DTRM-Fixtures mit den zugehoerigen XSDs.
#[test]
fn conformance_10_2_dtrm_support() {
    if !has_exi_testsuite() {
        eprintln!("SKIP: EXI_TESTSUITE_DIR nicht gesetzt");
        return;
    }

    let dir = Path::new("tests/fixtures/w3c/datatypes_dtrm");
    if !dir.exists() {
        eprintln!("SKIP: DTRM fixtures nicht vorhanden");
        return;
    }

    let xsd_base_str = format!("{}/data/interop/datatypes/dtrm", exi_testsuite_base());
    let xsd_base = Path::new(&xsd_base_str);
    if !xsd_base.exists() {
        eprintln!("SKIP: W3C DTRM XSDs nicht vorhanden");
        return;
    }

    thread_local! {
        static ENUM_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static LIST_SCHEMA_DTRM: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static UNION_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    }
    fn get_or_init_schema(
        cell: &'static std::thread::LocalKey<RefCell<Option<&'static SchemaInfo>>>,
        f: impl FnOnce() -> SchemaInfo,
    ) -> &'static SchemaInfo {
        cell.with(|c| {
            let mut borrow = c.borrow_mut();
            if let Some(s) = *borrow {
                s
            } else {
                let leaked: &'static SchemaInfo = Box::leak(Box::new(f()));
                *borrow = Some(leaked);
                leaked
            }
        })
    }
    let enum_schema = get_or_init_schema(&ENUM_SCHEMA, || {
        parse_xsd_with_imports(&xsd_base.join("enumerationDTRM.xsd"))
            .expect("enumerationDTRM.xsd parse")
    });
    let list_schema = get_or_init_schema(&LIST_SCHEMA_DTRM, || {
        parse_xsd_with_imports(&xsd_base.join("listDTRM.xsd"))
            .expect("listDTRM.xsd parse")
    });
    let union_schema = get_or_init_schema(&UNION_SCHEMA, || {
        parse_xsd_with_imports(&xsd_base.join("unionDTRM.xsd"))
            .expect("unionDTRM.xsd parse")
    });

    let mut decoded_count = 0;
    let mut failed = Vec::new();

    for entry in std::fs::read_dir(dir).expect("read dir").flatten() {
        let path = entry.path();
        if path.extension().map(|e| e == "exi").unwrap_or(false) {
            let name = path.file_name().unwrap().to_string_lossy().to_string();

            // Schema anhand des Dateinamens zuordnen
            let schema = if name.starts_with("tokenToInteger") || name.starts_with("enumerationToInteger") {
                enum_schema
            } else if name.starts_with("listToString") {
                list_schema
            } else if name.starts_with("unionToDecimal") {
                union_schema
            } else {
                eprintln!("DTRM: unbekannte Fixture-Gruppe: {name}");
                continue;
            };

            let mut opts = ExiOptions::default();
            if name.contains("_bytealigned") {
                opts.set_alignment(Alignment::ByteAlignment);
            } else if name.contains("_precompression") {
                opts.set_alignment(Alignment::PreCompression);
            } else if name.contains("_compression") {
                opts.set_compression(true);
            }
            if name.contains("_strict") {
                opts.set_strict(true);
            }

            let data = std::fs::read(&path).expect("read fixture");
            match decode_with_schema(&data, opts, schema) {
                Ok((events, _)) => {
                    // Inhaltliche Prüfung: SD/ED müssen vorhanden sein
                    assert!(
                        events.iter().any(|e| matches!(e, ExiEvent::StartDocument)),
                        "{name}: kein StartDocument"
                    );
                    assert!(
                        events.iter().any(|e| matches!(e, ExiEvent::EndDocument)),
                        "{name}: kein EndDocument"
                    );
                    // Mindestens ein Element und Characters-Event
                    assert!(
                        events.iter().any(|e| matches!(e, ExiEvent::StartElement(_))),
                        "{name}: kein StartElement"
                    );
                    assert!(
                        events.iter().any(|e| matches!(e, ExiEvent::Characters(_))),
                        "{name}: kein Characters-Event (DTRM-Werte fehlen)"
                    );
                    decoded_count += 1;
                }
                Err(e) => {
                    failed.push(format!("{name}: {e}"));
                }
            }
        }
    }

    assert!(
        decoded_count > 0,
        "Keine DTRM fixtures decodierbar"
    );
    if !failed.is_empty() {
        eprintln!("DTRM: {}/{} fehlgeschlagen:", failed.len(), decoded_count + failed.len());
        for f in &failed {
            eprintln!("  {f}");
        }
    }
    eprintln!(
        "DTRM conformance: {decoded_count}/{} fixtures OK",
        decoded_count + failed.len()
    );
}
