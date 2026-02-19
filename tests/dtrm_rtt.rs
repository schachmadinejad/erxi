//! DTRM (Datatype Representation Map) Self-Round-Trip Tests (Spec 7.4).
//!
//! Fuer jede DTRM-Fixture + Alignment:
//! 1. XML parsen → Events
//! 2. erxi encode (mit Schema + DTRM-Options)
//! 3. erxi decode → Events
//! 4. Vergleiche: Original-Events == Decoded-Events

use erxi::decoder::decode_with_schema;
use erxi::encoder::{encode_with_schema_and_config, EncoderConfig};
use erxi::event::ExiEvent;
use erxi::options::{Alignment, DatatypeRepresentationMapping, ExiOptions};
use erxi::qname::QName;
use erxi::xml::parse_xml_events_with_options;
use erxi::xsd::parse_xsd_with_imports;
use std::path::Path;
use std::path::PathBuf;

const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema";
const EXI_NS: &str = "http://www.w3.org/2009/exi";
const DTRM_DIR: &str = "tests/fixtures/dtrm";
const SCHEMA_PATH: &str = "tests/fixtures/dtrm/dtrm.xsd";

const ALIGNMENTS: &[(&str, fn(&mut ExiOptions))] = &[
    ("bitpacked", |_| {}),
    ("bytealigned", |o| o.set_alignment(Alignment::ByteAlignment)),
    ("precompression", |o| o.set_alignment(Alignment::PreCompression)),
    ("compression", |o| o.set_compression(true)),
];

/// DTRM-Konfigurationen pro Fixture.
fn dtrm_for_fixture(name: &str) -> Vec<DatatypeRepresentationMapping> {
    match name {
        "dtrm-01" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "decimal"),
            representation_qname: QName::new(EXI_NS, "string"),
        }],
        "dtrm-02" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "integer"),
            representation_qname: QName::new(EXI_NS, "string"),
        }],
        "dtrm-03" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "boolean"),
            representation_qname: QName::new(EXI_NS, "string"),
        }],
        "dtrm-04" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "decimal"),
            representation_qname: QName::new(EXI_NS, "integer"),
        }],
        "dtrm-05" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "double"),
            representation_qname: QName::new(EXI_NS, "string"),
        }],
        "dtrm-06" => vec![
            // Closest Ancestor: decimal→string erbt auf myDecimal
            DatatypeRepresentationMapping {
                type_qname: QName::new(XSD_NS, "decimal"),
                representation_qname: QName::new(EXI_NS, "string"),
            },
        ],
        "dtrm-07" => vec![
            DatatypeRepresentationMapping {
                type_qname: QName::new(XSD_NS, "decimal"),
                representation_qname: QName::new(EXI_NS, "string"),
            },
            DatatypeRepresentationMapping {
                type_qname: QName::new(XSD_NS, "boolean"),
                representation_qname: QName::new(EXI_NS, "string"),
            },
            DatatypeRepresentationMapping {
                type_qname: QName::new(XSD_NS, "double"),
                representation_qname: QName::new(EXI_NS, "integer"),
            },
        ],
        "dtrm-08" => vec![
            // DTRM auf Attributen: decimal→string
            DatatypeRepresentationMapping {
                type_qname: QName::new(XSD_NS, "decimal"),
                representation_qname: QName::new(EXI_NS, "string"),
            },
        ],
        "dtrm-09" => vec![
            // Nur decimal gemappt, boolean und string bleiben normal
            DatatypeRepresentationMapping {
                type_qname: QName::new(XSD_NS, "decimal"),
                representation_qname: QName::new(EXI_NS, "string"),
            },
        ],
        "dtrm-10" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "date"),
            representation_qname: QName::new(EXI_NS, "string"),
        }],
        _ => vec![],
    }
}

fn self_round_trip(xml_path: &Path, fixture_name: &str, alignment_name: &str, apply_alignment: fn(&mut ExiOptions)) {
    let schema_path = PathBuf::from(SCHEMA_PATH);
    let schema = parse_xsd_with_imports(&schema_path).unwrap_or_else(|e| {
        panic!("{fixture_name}_{alignment_name}: Schema-Parse-Fehler: {e}");
    });

    let mut opts = ExiOptions::default();
    apply_alignment(&mut opts);
    opts.set_datatype_representation_map(dtrm_for_fixture(fixture_name));

    let events = parse_xml_events_with_options(xml_path, &opts).unwrap_or_else(|e| {
        panic!("{fixture_name}_{alignment_name}: XML-Parse-Fehler: {e}");
    });

    let config = EncoderConfig { include_options: true, include_cookie: false };
    let exi_bytes = encode_with_schema_and_config(&events, &opts, &schema, config).unwrap_or_else(|e| {
        panic!("{fixture_name}_{alignment_name}: Encode-Fehler: {e}");
    });

    // Decoder bekommt Default-Options (ohne DTRM) — muss DTRM aus dem Header lesen
    let mut decode_opts = ExiOptions::default();
    apply_alignment(&mut decode_opts);

    let (decoded, decoded_opts) = decode_with_schema(&exi_bytes, decode_opts, &schema).unwrap_or_else(|e| {
        panic!(
            "{fixture_name}_{alignment_name}: Decode-Fehler: {e} (EXI: {} bytes)",
            exi_bytes.len()
        );
    });

    // Verifiziere: DTRM aus Header gelesen
    let expected_dtrm = dtrm_for_fixture(fixture_name);
    assert_eq!(
        decoded_opts.datatype_representation_map().len(),
        expected_dtrm.len(),
        "{fixture_name}_{alignment_name}: DTRM-Anzahl aus Header stimmt nicht"
    );
    for (i, (exp, dec)) in expected_dtrm.iter().zip(decoded_opts.datatype_representation_map().iter()).enumerate() {
        assert_eq!(
            exp.type_qname, dec.type_qname,
            "{fixture_name}_{alignment_name}: DTRM[{i}] type_qname unterschiedlich"
        );
        assert_eq!(
            exp.representation_qname, dec.representation_qname,
            "{fixture_name}_{alignment_name}: DTRM[{i}] representation_qname unterschiedlich"
        );
    }

    // Filtere Whitespace-only CH-Events und SD/ED (werden bei schema-informed gefiltert)
    let is_significant = |e: &&ExiEvent| {
        match e {
            ExiEvent::StartDocument | ExiEvent::EndDocument => false,
            ExiEvent::Characters(s) => !s.value.trim().is_empty(),
            _ => true,
        }
    };
    let original: Vec<_> = events.iter().filter(is_significant).collect();
    let decoded_filtered: Vec<_> = decoded.iter().filter(is_significant).collect();

    assert_eq!(
        original.len(),
        decoded_filtered.len(),
        "{fixture_name}_{alignment_name}: Event-Anzahl unterschiedlich (original={}, decoded={})",
        original.len(),
        decoded_filtered.len()
    );

    for (i, (orig, dec)) in original.iter().zip(decoded_filtered.iter()).enumerate() {
        // Typed Values werden von EXI kanonisiert, daher semantischer Vergleich
        match (orig, dec) {
            (ExiEvent::Characters(a), ExiEvent::Characters(b)) => {
                if a.value != b.value {
                    // Erlaube EXI-Kanonisierung: "1.0" → "1", "1.0E2" → "100", "0.0" → "0"
                    let a_clean = a.value.trim();
                    let b_clean = b.value.trim();
                    let a_f = a_clean.parse::<f64>();
                    let b_f = b_clean.parse::<f64>();
                    if let (Ok(af), Ok(bf)) = (a_f, b_f) {
                        assert!(
                            (af - bf).abs() < 1e-10 || (af == bf),
                            "{fixture_name}_{alignment_name}: Event {i} numerisch unterschiedlich\n  original: {:?}\n  decoded:  {:?}",
                            a.value, b.value,
                        );
                    } else {
                        assert_eq!(
                            a.value, b.value,
                            "{fixture_name}_{alignment_name}: Event {i} unterschiedlich"
                        );
                    }
                }
            }
            _ => {
                assert_eq!(
                    orig, dec,
                    "{fixture_name}_{alignment_name}: Event {i} unterschiedlich\n  original: {orig:?}\n  decoded:  {dec:?}"
                );
            }
        }
    }
}

#[test]
fn dtrm_self_round_trip_all() {
    let fixtures: Vec<String> = (1..=10)
        .map(|i| format!("dtrm-{:02}", i))
        .collect();

    let mut failures = Vec::new();

    for fixture in &fixtures {
        let xml_path = Path::new(DTRM_DIR).join(format!("{}.xml", fixture));
        if !xml_path.exists() {
            failures.push(format!("{}: Datei nicht gefunden", fixture));
            continue;
        }

        for &(alignment_name, apply_alignment) in ALIGNMENTS {
            let result = std::panic::catch_unwind(|| {
                self_round_trip(&xml_path, fixture, alignment_name, apply_alignment);
            });
            if let Err(e) = result {
                let msg = if let Some(s) = e.downcast_ref::<String>() {
                    s.clone()
                } else if let Some(s) = e.downcast_ref::<&str>() {
                    s.to_string()
                } else {
                    "unbekannter Fehler".to_string()
                };
                failures.push(format!("{}_{}: {}", fixture, alignment_name, msg));
            }
        }
    }

    if !failures.is_empty() {
        let total = fixtures.len() * ALIGNMENTS.len();
        let passed = total - failures.len();
        panic!(
            "\nDTRM Self-RTT: {}/{} bestanden, {} fehlgeschlagen:\n{}",
            passed,
            total,
            failures.len(),
            failures.join("\n")
        );
    }

    eprintln!(
        "DTRM Self-RTT: alle {}/{} bestanden",
        fixtures.len() * ALIGNMENTS.len(),
        fixtures.len() * ALIGNMENTS.len()
    );
}
