//! DTRM Cross-RTT-Test: erxi vs Exificient.
//!
//! Argumente: <fixture> <alignment>
//! - fixture: z.B. `dtrm-01`
//! - alignment: `bitpacked`, `bytealigned`, `precompression`, `compression`
//!
//! Ausgabe: TSV auf stdout (fixture\ttest_id\tresult\tdetail)

use erxi::decoder::decode_with_schema;
use erxi::encoder::{encode_with_schema_and_config, EncoderConfig};
use erxi::event::ExiEvent;
use erxi::options::{Alignment, DatatypeRepresentationMapping, ExiOptions};
use erxi::qname::QName;
use erxi::xml::parse_xml_events_with_options;
use erxi::xsd::parse_xsd_with_imports;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema";
const EXI_NS: &str = "http://www.w3.org/2009/exi";
const DTRM_DIR: &str = "tests/fixtures/dtrm";
const SCHEMA_PATH: &str = "tests/fixtures/dtrm/dtrm.xsd";
fn exificient_jar() -> String {
    std::env::var("EXIFICIENT_JAR")
        .expect("EXIFICIENT_JAR muss gesetzt sein (Pfad zur Exificient JAR)")
}

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
        "dtrm-06" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "decimal"),
            representation_qname: QName::new(EXI_NS, "string"),
        }],
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
        "dtrm-08" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "decimal"),
            representation_qname: QName::new(EXI_NS, "string"),
        }],
        "dtrm-09" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "decimal"),
            representation_qname: QName::new(EXI_NS, "string"),
        }],
        "dtrm-10" => vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XSD_NS, "date"),
            representation_qname: QName::new(EXI_NS, "string"),
        }],
        _ => vec![],
    }
}

fn dtrm_to_exificient_arg(dtrm: &[DatatypeRepresentationMapping]) -> String {
    // Exificient Format: qnameType,qnameRepresentation,qnameType2,qnameRepresentation2,...
    // QName Format: {uri}localname
    dtrm.iter()
        .flat_map(|m| {
            vec![
                format!("{{{}}}{}", m.type_qname.uri, m.type_qname.local_name),
                format!(
                    "{{{}}}{}",
                    m.representation_qname.uri, m.representation_qname.local_name
                ),
            ]
        })
        .collect::<Vec<_>>()
        .join(",")
}

fn options_for_alignment(alignment: &str, fixture: &str) -> ExiOptions {
    let mut opts = ExiOptions::default();
    match alignment {
        "bytealigned" => opts.set_alignment(Alignment::ByteAlignment),
        "precompression" => opts.set_alignment(Alignment::PreCompression),
        "compression" => opts.set_compression(true),
        _ => {}
    }
    opts.set_datatype_representation_map(dtrm_for_fixture(fixture));
    opts
}

fn exificient_args(opts: &ExiOptions) -> Vec<String> {
    let mut args = vec!["-schema".to_string(), SCHEMA_PATH.to_string()];

    match opts.alignment() {
        Alignment::ByteAlignment => args.push("-bytePacked".to_string()),
        Alignment::PreCompression => args.push("-preCompression".to_string()),
        _ => {}
    }
    if opts.compression() {
        args.push("-compression".to_string());
    }

    if !opts.datatype_representation_map().is_empty() {
        args.push("-datatypeRepresentationMap".to_string());
        args.push(dtrm_to_exificient_arg(opts.datatype_representation_map()));
    }

    args
}

fn exificient_encode(xml_path: &Path, opts: &ExiOptions, out_path: &Path) -> Result<(), String> {
    let mut args = vec![
        "-jar".to_string(),
        exificient_jar(),
        "-encode".to_string(),
        "-i".to_string(),
        xml_path.to_string_lossy().to_string(),
        "-o".to_string(),
        out_path.to_string_lossy().to_string(),
    ];
    args.extend(exificient_args(opts));

    let output = Command::new("java")
        .args(&args)
        .output()
        .map_err(|e| format!("exificient encode start: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("exificient encode failed: {}", stderr.trim()));
    }
    Ok(())
}

fn exificient_decode(exi_path: &Path, opts: &ExiOptions, out_path: &Path) -> Result<(), String> {
    let mut args = vec![
        "-jar".to_string(),
        exificient_jar(),
        "-decode".to_string(),
        "-i".to_string(),
        exi_path.to_string_lossy().to_string(),
        "-o".to_string(),
        out_path.to_string_lossy().to_string(),
    ];
    args.extend(exificient_args(opts));

    let output = Command::new("java")
        .args(&args)
        .output()
        .map_err(|e| format!("exificient decode start: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("exificient decode failed: {}", stderr.trim()));
    }
    Ok(())
}

/// Kanonischer XML-Vergleich: Parst beide XML-Dateien in Events und vergleicht
/// semantisch (Namespace-Prefixes und Whitespace ignorierend).
fn xml_files_semantically_equal(a: &Path, b: &Path) -> Result<bool, String> {
    let opts = ExiOptions::default();
    let events_a = parse_xml_events_with_options(a, &opts)
        .map_err(|e| format!("parse {}: {e}", a.display()))?;
    let events_b = parse_xml_events_with_options(b, &opts)
        .map_err(|e| format!("parse {}: {e}", b.display()))?;
    let sig_a = significant_events(&events_a);
    let sig_b = significant_events(&events_b);
    Ok(events_semantically_equal(&sig_a, &sig_b))
}

fn emit(fixture_tag: &str, test_id: &str, result: &str, detail: &str) {
    println!("{}\t{}\t{}\t{}", fixture_tag, test_id, result, detail);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: dtrm_cross_test <fixture> <alignment>");
        std::process::exit(1);
    }
    let fixture = &args[1];
    let alignment = &args[2];
    let fixture_tag = format!("{}_{}", fixture, alignment);

    let xml_path = PathBuf::from(DTRM_DIR).join(format!("{}.xml", fixture));
    if !xml_path.exists() {
        emit(&fixture_tag, "erxi_self_rtt", "SKIP", "xml-missing");
        emit(&fixture_tag, "exif_self_rtt", "SKIP", "xml-missing");
        emit(&fixture_tag, "erxi_to_exif", "SKIP", "xml-missing");
        emit(&fixture_tag, "bytes_erxi_vs_exif", "SKIP", "xml-missing");
        return;
    }

    let schema_path = PathBuf::from(SCHEMA_PATH);
    let schema = match parse_xsd_with_imports(&schema_path) {
        Ok(s) => s,
        Err(e) => {
            emit(&fixture_tag, "erxi_self_rtt", "SKIP", &format!("schema: {e}"));
            return;
        }
    };

    let opts = options_for_alignment(alignment, fixture);

    let events = match parse_xml_events_with_options(&xml_path, &opts) {
        Ok(ev) => ev,
        Err(e) => {
            emit(&fixture_tag, "erxi_self_rtt", "FAIL", &format!("xml-parse: {e}"));
            return;
        }
    };

    let work_dir = PathBuf::from("target/dtrm_cross");
    fs::create_dir_all(&work_dir).unwrap();

    // === TEST 1: erxi Self-RTT ===
    let config = EncoderConfig {
        include_options: false,
        include_cookie: false,
    };
    let exi_erxi = match encode_with_schema_and_config(&events, &opts, &schema, config) {
        Ok(data) => data,
        Err(e) => {
            emit(&fixture_tag, "erxi_self_rtt", "FAIL", &format!("encode: {e}"));
            return;
        }
    };

    let erxi_exi_path = work_dir.join(format!("{fixture_tag}_erxi.exi"));
    fs::write(&erxi_exi_path, &exi_erxi).unwrap();

    match decode_with_schema(&exi_erxi, opts.clone(), &schema) {
        Ok((decoded, _)) => {
            let orig_sig = significant_events(&events);
            let dec_sig = significant_events(&decoded);
            if events_semantically_equal(&orig_sig, &dec_sig) {
                emit(
                    &fixture_tag,
                    "erxi_self_rtt",
                    "OK",
                    &format!("size={}", exi_erxi.len()),
                );
            } else {
                emit(&fixture_tag, "erxi_self_rtt", "FAIL", "events-differ");
            }
        }
        Err(e) => {
            emit(&fixture_tag, "erxi_self_rtt", "FAIL", &format!("decode: {e}"));
        }
    }

    // === TEST 2: Exificient Self-RTT ===
    let exif_exi_path = work_dir.join(format!("{fixture_tag}_exif.exi"));
    let exif_rtt_path = work_dir.join(format!("{fixture_tag}_exif_rtt.xml"));

    match exificient_encode(&xml_path, &opts, &exif_exi_path) {
        Ok(()) => {
            let exif_size = fs::read(&exif_exi_path).map(|d| d.len()).unwrap_or(0);
            match exificient_decode(&exif_exi_path, &opts, &exif_rtt_path) {
                Ok(()) => {
                    match xml_files_semantically_equal(&xml_path, &exif_rtt_path) {
                        Ok(true) => emit(
                            &fixture_tag,
                            "exif_self_rtt",
                            "OK",
                            &format!("size={}", exif_size),
                        ),
                        Ok(false) => emit(&fixture_tag, "exif_self_rtt", "FAIL", "events-differ"),
                        Err(e) => emit(&fixture_tag, "exif_self_rtt", "FAIL", &format!("compare: {e}")),
                    }
                }
                Err(e) => emit(&fixture_tag, "exif_self_rtt", "FAIL", &format!("decode: {e}")),
            }
        }
        Err(e) => emit(&fixture_tag, "exif_self_rtt", "FAIL", &format!("encode: {e}")),
    }

    // === TEST 3: erxi encode → Exificient decode ===
    let erxi_to_exif_path = work_dir.join(format!("{fixture_tag}_erxi_to_exif.xml"));
    match exificient_decode(&erxi_exi_path, &opts, &erxi_to_exif_path) {
        Ok(()) => {
            match xml_files_semantically_equal(&xml_path, &erxi_to_exif_path) {
                Ok(true) => emit(&fixture_tag, "erxi_to_exif", "OK", "events-match"),
                Ok(false) => emit(&fixture_tag, "erxi_to_exif", "FAIL", "events-differ"),
                Err(e) => emit(&fixture_tag, "erxi_to_exif", "FAIL", &format!("compare: {e}")),
            }
        }
        Err(e) => emit(&fixture_tag, "erxi_to_exif", "FAIL", &format!("{e}")),
    }

    // === TEST 4: Exificient encode → erxi decode ===
    let exif_exi_data = fs::read(&exif_exi_path).unwrap_or_default();
    if !exif_exi_data.is_empty() {
        match decode_with_schema(&exif_exi_data, opts.clone(), &schema) {
            Ok((decoded, _)) => {
                let orig_sig = significant_events(&events);
                let dec_sig = significant_events(&decoded);
                if events_semantically_equal(&orig_sig, &dec_sig) {
                    emit(&fixture_tag, "exif_to_erxi", "OK", "events-match");
                } else {
                    emit(&fixture_tag, "exif_to_erxi", "FAIL", "events-differ");
                }
            }
            Err(e) => emit(&fixture_tag, "exif_to_erxi", "FAIL", &format!("decode: {e}")),
        }
    } else {
        emit(&fixture_tag, "exif_to_erxi", "SKIP", "no-exif-exi");
    }

    // === TEST 5: Byte-Vergleich erxi vs Exificient ===
    let erxi_bytes = fs::read(&erxi_exi_path).unwrap_or_default();
    let exif_bytes = fs::read(&exif_exi_path).unwrap_or_default();
    if !erxi_bytes.is_empty() && !exif_bytes.is_empty() {
        if erxi_bytes == exif_bytes {
            emit(
                &fixture_tag,
                "bytes_erxi_vs_exif",
                "OK",
                &format!("identical={}", erxi_bytes.len()),
            );
        } else {
            let diff = erxi_bytes.len() as isize - exif_bytes.len() as isize;
            emit(
                &fixture_tag,
                "bytes_erxi_vs_exif",
                "FAIL",
                &format!(
                    "erxi={} exif={} diff={}",
                    erxi_bytes.len(),
                    exif_bytes.len(),
                    diff
                ),
            );
        }
    } else {
        emit(&fixture_tag, "bytes_erxi_vs_exif", "SKIP", "missing-data");
    }
}

fn significant_events(events: &[ExiEvent]) -> Vec<&ExiEvent> {
    events
        .iter()
        .filter(|e| match e {
            ExiEvent::StartDocument | ExiEvent::EndDocument => false,
            ExiEvent::Characters(s) => !s.value.trim().is_empty(),
            _ => true,
        })
        .collect()
}

fn events_semantically_equal(a: &[&ExiEvent], b: &[&ExiEvent]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    for (ae, be) in a.iter().zip(b.iter()) {
        match (ae, be) {
            (ExiEvent::Characters(ca), ExiEvent::Characters(cb)) => {
                if ca.value != cb.value {
                    let af = ca.value.trim().parse::<f64>();
                    let bf = cb.value.trim().parse::<f64>();
                    match (af, bf) {
                        (Ok(a), Ok(b)) => {
                            if (a - b).abs() > 1e-10 && a != b {
                                return false;
                            }
                        }
                        _ => return false,
                    }
                }
            }
            _ => {
                if ae != be {
                    return false;
                }
            }
        }
    }
    true
}
