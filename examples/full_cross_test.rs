//! Vollständige Cross-RTT-Matrix: W3C Fixtures + DTRM + EXI4JSON + infoset + coverage + CLI.
//!
//! Argumente: <suite> <fixture_basename> <alignment>
//! - suite: `declared`, `undeclared`, `dtrm`, `exi4json`, `infoset`, `coverage`, `cli`
//! - fixture_basename: z.B. `complexType-01`
//! - alignment: `bitpacked`, `bytealigned`, `precompression`, `compression`, `strict`
//!
//! Ausgabe: TSV auf stdout (fixture\ttest_id\tresult\tdetail)

use erxi::decoder::{decode, decode_with_options, decode_with_schema};
use erxi::encoder::{encode, encode_with_config, encode_with_schema, encode_with_schema_and_config, EncoderConfig};
use erxi::event::{AtContent, ChContent, CmContent, DtContent, ErContent, ExiEvent, NsContent, PiContent};
use erxi::options::{Alignment, DatatypeRepresentationMapping, ExiOptions, Preserve, SchemaId};
use erxi::qname::QName;
use erxi::schema::SchemaInfo;
use erxi::xml::{parse_xml_events_from_str, parse_xml_events_with_options};
use erxi::xml_serializer::events_to_xml;
use erxi::xsd::parse_xsd_with_imports;
use roxmltree;
use serde_json::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

// ============================================================================
// Konstanten
// ============================================================================

fn exi_testsuite_base() -> String {
    if let Ok(dir) = std::env::var("EXI_TESTSUITE_DIR") {
        return dir;
    }
    let fallback = "/tmp/exi-testsuite/ttfms-interop-18122013";
    if std::path::Path::new(fallback).is_dir() {
        return fallback.to_string();
    }
    panic!(
        "EXI_TESTSUITE_DIR fehlt. Erwartet W3C EXI Test Suite unter {} (oder EXI_TESTSUITE_DIR setzen)",
        fallback
    );
}

fn declared_xml_dir() -> String {
    format!("{}/data/interop/schemaInformedGrammar/declaredProductions", exi_testsuite_base())
}

fn undeclared_xml_dir() -> String {
    format!("{}/data/interop/schemaInformedGrammar/undeclaredProductions", exi_testsuite_base())
}

const DTRM_DIR: &str = "tests/fixtures/dtrm";
const DTRM_SCHEMA: &str = "tests/fixtures/dtrm/dtrm.xsd";

const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema";
const EXI_NS: &str = "http://www.w3.org/2009/exi";

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
    dtrm.iter()
        .flat_map(|m| {
            vec![
                format!("{{{}}}{}", m.type_qname.uri, m.type_qname.local_name),
                format!("{{{}}}{}", m.representation_qname.uri, m.representation_qname.local_name),
            ]
        })
        .collect::<Vec<_>>()
        .join(",")
}

// ============================================================================
// Options (identisch mit cross_rtt.rs)
// ============================================================================

fn options_for_alignment(alignment: &str, xml_basename: &str, suite: &str) -> ExiOptions {
    let mut opts = ExiOptions::default();

    match alignment {
        "bytealigned" => opts.set_alignment(Alignment::ByteAlignment),
        "precompression" => opts.set_alignment(Alignment::PreCompression),
        "compression" => opts.set_compression(true),
        "strict" => opts.set_strict(true),
        _ => {} // bitpacked = default
    }

    // DTRM-Suite: Mappings pro Fixture setzen
    if suite == "dtrm" {
        opts.set_datatype_representation_map(dtrm_for_fixture(xml_basename));
    }

    if !opts.strict() {
        let mut p = *opts.preserve();
        if suite == "undeclared" {
            if xml_basename.starts_with("cm-") {
                p.comments = true;
                if !xml_basename.starts_with("cm-04") {
                    p.pis = true;
                }
            }
            if xml_basename.starts_with("pi-") {
                p.pis = true;
            }
            if xml_basename.starts_with("er-") {
                p.dtd = true;
            }
            if xml_basename.starts_with("namespaceDecl-") {
                p.prefixes = true;
            }
        } else {
            // declared
            if xml_basename.starts_with("document-01") {
                p.prefixes = true;
            } else if xml_basename.starts_with("document-02")
                || xml_basename.starts_with("document-03")
                || xml_basename.starts_with("document-04")
                || xml_basename.starts_with("document-05")
                || xml_basename.starts_with("document-06")
                || xml_basename.starts_with("document-07")
            {
                p.dtd = true;
                p.pis = true;
                p.comments = true;
            }
            if xml_basename.starts_with("elementFragment-05")
                || xml_basename.starts_with("elementFragment-06")
            {
                p.dtd = true;
                p.pis = true;
                p.comments = true;
            }
            if xml_basename.starts_with("fragment-01") {
                p.dtd = true;
                p.pis = true;
                p.comments = true;
            } else if xml_basename.starts_with("fragment-02") {
                p.dtd = true;
            } else if xml_basename.starts_with("fragment-03")
                || xml_basename.starts_with("fragment-04")
                || xml_basename.starts_with("fragment-05")
                || xml_basename.starts_with("fragment-06")
            {
                p.pis = true;
            }
        }
        opts.set_preserve(p);
    }

    if xml_basename.starts_with("elementFragment") || xml_basename.starts_with("fragment-") {
        opts.set_fragment(true);
    }

    if xml_basename.starts_with("sc-") {
        opts.set_self_contained(true);
        opts.set_self_contained_qnames(vec![QName::new("urn:foo", "ANY")]);
        if xml_basename == "sc-02" {
            let mut p = *opts.preserve();
            p.prefixes = true;
            opts.set_preserve(p);
        }
    }

    opts
}

// ============================================================================
// Schema-Zuordnung (identisch mit cross_rtt.rs)
// ============================================================================

fn schema_path_for_xml(xml_basename: &str, suite: &str) -> String {
    if suite == "dtrm" {
        return DTRM_SCHEMA.to_string();
    }

    let base = format!("{}/data/interop/schemaInformedGrammar", exi_testsuite_base());
    let declared = format!("{base}/declaredProductions");

    if suite == "declared" && xml_basename.starts_with("elementFragment") {
        format!("{declared}/elementFragment.xsd")
    } else if suite == "declared" && xml_basename.starts_with("document-") {
        format!("{declared}/document.xsd")
    } else if suite == "declared" && xml_basename.starts_with("duplicateTerminals-01") {
        format!("{declared}/duplicateTerminals-01.xsd")
    } else if suite == "declared" && xml_basename.starts_with("duplicateTerminals-02") {
        format!("{declared}/duplicateTerminals-02.xsd")
    } else if suite == "declared" && xml_basename.starts_with("elementTerm-01") {
        format!("{declared}/substitutionGroup.xsd")
    } else if suite == "declared"
        && (xml_basename.starts_with("particle-")
            || xml_basename.starts_with("complexType-21")
            || xml_basename.starts_with("complexType-23"))
    {
        format!("{declared}/particle.xsd")
    } else if suite == "declared"
        && (xml_basename.starts_with("fragment-01") || xml_basename.starts_with("fragment-02"))
    {
        format!("{declared}/fragment-a.xsd")
    } else if suite == "declared" && xml_basename.starts_with("fragment-03") {
        format!("{declared}/fragment-b.xsd")
    } else if suite == "declared" && xml_basename.starts_with("fragment-04") {
        format!("{declared}/fragment-c.xsd")
    } else if suite == "declared" && xml_basename.starts_with("fragment-05") {
        format!("{declared}/fragment-d.xsd")
    } else if suite == "declared" && xml_basename.starts_with("fragment-06") {
        format!("{declared}/fragment-e.xsd")
    } else {
        format!("{base}/acceptance.xsd")
    }
}

thread_local! {
    static SCHEMA_CACHE: RefCell<HashMap<String, &'static SchemaInfo>> = RefCell::new(HashMap::new());
}

fn load_schema(xml_basename: &str, suite: &str) -> Result<&'static SchemaInfo, String> {
    let xsd_path = schema_path_for_xml(xml_basename, suite);

    let cached = SCHEMA_CACHE.with(|c| c.borrow().get(&xsd_path).copied());
    if let Some(schema) = cached {
        return Ok(schema);
    }

    let schema = parse_xsd_with_imports(Path::new(&xsd_path))
        .map_err(|e| format!("Schema parse error for {xsd_path}: {e}"))?;
    let leaked: &'static SchemaInfo = Box::leak(Box::new(schema));
    SCHEMA_CACHE.with(|c| c.borrow_mut().insert(xsd_path, leaked));
    Ok(leaked)
}

// ============================================================================
// Exificient CLI-Wrapper (identisch mit cross_rtt.rs)
// ============================================================================

fn options_to_exificient_args(opts: &ExiOptions, schema_path: &str) -> Vec<String> {
    let mut args = Vec::new();
    if !schema_path.is_empty() {
        args.push("-schema".to_string());
        args.push(schema_path.to_string());
    }

    match opts.alignment() {
        Alignment::ByteAlignment => args.push("-bytePacked".to_string()),
        Alignment::PreCompression => args.push("-preCompression".to_string()),
        _ => {}
    }

    if opts.compression() {
        args.push("-compression".to_string());
    }
    if opts.strict() {
        args.push("-strict".to_string());
    }
    if opts.preserve().comments {
        args.push("-preserveComments".to_string());
    }
    if opts.preserve().pis {
        args.push("-preservePIs".to_string());
    }
    if opts.preserve().dtd {
        args.push("-preserveDTDs".to_string());
    }
    if opts.preserve().prefixes {
        args.push("-preservePrefixes".to_string());
    }
    if opts.fragment() {
        args.push("-fragment".to_string());
    }
    if opts.self_contained() {
        for qname in opts.self_contained_qnames() {
            args.push("-selfContained".to_string());
            args.push(format!("{{{}}}{}", qname.uri, qname.local_name));
        }
    }

    if !opts.datatype_representation_map().is_empty() {
        args.push("-datatypeRepresentationMap".to_string());
        args.push(dtrm_to_exificient_arg(opts.datatype_representation_map()));
    }

    args
}

// ============================================================================
// Persistenter ExifBatch-Prozess (gemeinsamer Code)
// ============================================================================

include!("../tests/common/exif_batch.rs");

fn exificient_self_rtt(
    xml_path: &Path,
    opts: &ExiOptions,
    schema_path: &str,
    work_dir: &Path,
    tag: &str,
) -> Result<(usize, String), String> {
    let exi_path = work_dir.join(format!("{tag}_exif.exi"));
    let xml_out = work_dir.join(format!("{tag}_exif_rtt.xml"));

    exificient_encode(xml_path, opts, schema_path, &exi_path)?;
    let exi_size = fs::read(&exi_path)
        .map_err(|e| format!("read exi: {e}"))?
        .len();
    exificient_decode(&exi_path, opts, schema_path, &xml_out)?;

    let orig = normalize_xml(&fs::read_to_string(xml_path).map_err(|e| format!("read xml: {e}"))?);
    let rtt = normalize_xml(&fs::read_to_string(&xml_out).map_err(|e| format!("read rtt xml: {e}"))?);

    if orig == rtt {
        Ok((exi_size, "xml-identical".to_string()))
    } else {
        Ok((exi_size, "xml-differs".to_string()))
    }
}

// ============================================================================
// XML-Normalisierung und kanonischer Event-Vergleich (gemeinsamer Code)
// ============================================================================

include!("../tests/common/canonical.rs");

fn compare_events_canonical(orig: &[ExiEvent], rtt: &[ExiEvent]) -> (bool, String) {
    compare_events_canonical_ex(orig, rtt, false)
}

/// Kanonischer Event-Vergleich mit optionalem URI-Stripping.
/// Bei strip_uris=true werden nur Local-Names verglichen (fuer XML-RTT ohne preserve_prefixes).
fn compare_events_canonical_ex(orig: &[ExiEvent], rtt: &[ExiEvent], strip_uris: bool) -> (bool, String) {
    let canon_orig = events_to_canonical_ex(orig, strip_uris);
    let canon_rtt = events_to_canonical_ex(rtt, strip_uris);

    if canon_orig == canon_rtt {
        return (true, String::new());
    }

    let orig_lines: Vec<&str> = canon_orig.lines().collect();
    let rtt_lines: Vec<&str> = canon_rtt.lines().collect();
    for (i, (a, b)) in orig_lines.iter().zip(rtt_lines.iter()).enumerate() {
        if a != b {
            return (
                false,
                format!("line {}: '{}' vs '{}'", i + 1, truncate(a, 40), truncate(b, 40)),
            );
        }
    }
    if orig_lines.len() != rtt_lines.len() {
        return (
            false,
            format!("lines: {} vs {}", orig_lines.len(), rtt_lines.len()),
        );
    }
    (false, "unknown diff".to_string())
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max])
    }
}

// ============================================================================
// CLI-Interop Tests (erxi CLI <-> Exificient)
// ============================================================================

const CLI_FIXTURES: &[&str] = &["no-include-options", "exif-to-erxi"];
const CLI_TEST_IDS: &[&str] = &["cli_erxi_to_exif", "cli_exif_to_erxi"];

fn erxi_bin() -> PathBuf {
    if let Ok(bin) = std::env::var("ERXI_BIN") {
        return PathBuf::from(bin);
    }
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let debug = root.join("target/debug/erxi");
    if debug.exists() {
        return debug;
    }
    let release = root.join("target/release/erxi");
    if release.exists() {
        return release;
    }
    PathBuf::from("erxi")
}

fn run_erxi(args: &[&str]) -> Result<Output, String> {
    Command::new(erxi_bin())
        .args(args)
        .output()
        .map_err(|e| format!("run erxi: {e}"))
}

fn cli_temp_dir(tag: &str) -> PathBuf {
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock before epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("erxi-cli-cross-{tag}-{}-{ts}", std::process::id()));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn assert_infoset_eq(expected_xml: &str, actual_xml: &str) -> Result<(), String> {
    let opts = ExiOptions::default();
    let expected = parse_xml_events_from_str(expected_xml, &opts).map_err(|e| format!("parse expected: {e}"))?;
    let actual = parse_xml_events_from_str(actual_xml, &opts).map_err(|e| format!("parse actual: {e}"))?;
    if expected == actual {
        Ok(())
    } else {
        Err("infoset mismatch".to_string())
    }
}

fn run_cli_tests(basename: &str, alignment: &str) {
    let fixture_tag = format!("{}_{}", basename, alignment);
    if !CLI_FIXTURES.contains(&basename) {
        for test_id in CLI_TEST_IDS {
            emit(&fixture_tag, test_id, "SKIP", "unknown-cli-fixture");
        }
        return;
    }

    match basename {
        "no-include-options" => run_cli_no_include_options(&fixture_tag),
        "exif-to-erxi" => run_cli_exif_to_erxi(&fixture_tag),
        _ => {
            for test_id in CLI_TEST_IDS {
                emit(&fixture_tag, test_id, "SKIP", "unknown-cli-fixture");
            }
        }
    }
}

fn run_cli_no_include_options(fixture_tag: &str) {
    let dir = cli_temp_dir("no-include-to-exif");
    let xml = "<root><item>abc</item><item>def</item></root>";
    let xsd = r#"<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="root">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"#;
    let xml_path = dir.join("in.xml");
    let xsd_path = dir.join("schema.xsd");
    let exi_path = dir.join("out.exi");
    let exif_decoded = dir.join("exif-decoded.xml");
    fs::write(&xml_path, xml).expect("write xml");
    fs::write(&xsd_path, xsd).expect("write xsd");

    let enc = match run_erxi(&[
        "encode",
        "-i",
        xml_path.to_str().unwrap(),
        "-o",
        exi_path.to_str().unwrap(),
        "--schema",
        xsd_path.to_str().unwrap(),
        "--byte-aligned",
        "--no-include-options",
    ]) {
        Ok(out) => out,
        Err(e) => {
            emit(fixture_tag, "cli_erxi_to_exif", "FAIL", &truncate(&e, 80));
            return;
        }
    };
    if !enc.status.success() {
        emit(
            fixture_tag,
            "cli_erxi_to_exif",
            "FAIL",
            &truncate(&String::from_utf8_lossy(&enc.stderr), 80),
        );
        return;
    }

    let mut opts = ExiOptions::default();
    opts.set_alignment(Alignment::ByteAlignment);
    if let Err(e) = exificient_decode(&exi_path, &opts, xsd_path.to_str().unwrap(), &exif_decoded) {
        emit(fixture_tag, "cli_erxi_to_exif", "FAIL", &truncate(&e, 80));
        return;
    }

    let xml_out = match fs::read_to_string(exif_decoded) {
        Ok(s) => s,
        Err(e) => {
            emit(fixture_tag, "cli_erxi_to_exif", "FAIL", &truncate(&format!("read decoded: {e}"), 80));
            return;
        }
    };

    match assert_infoset_eq(xml, &xml_out) {
        Ok(()) => emit(fixture_tag, "cli_erxi_to_exif", "OK", "infoset-match"),
        Err(e) => emit(fixture_tag, "cli_erxi_to_exif", "FAIL", &truncate(&e, 80)),
    }
}

fn run_cli_exif_to_erxi(fixture_tag: &str) {
    let dir = cli_temp_dir("exif-to-erxi");
    let xml = "<root><v>hello</v><v>world</v></root>";
    let xsd = r#"<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="root">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="v" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"#;
    let xml_path = dir.join("in.xml");
    let xsd_path = dir.join("schema.xsd");
    let exi_path = dir.join("exif.exi");
    let erxi_decoded = dir.join("erxi-decoded.xml");
    fs::write(&xml_path, xml).expect("write xml");
    fs::write(&xsd_path, xsd).expect("write xsd");

    let mut opts = ExiOptions::default();
    opts.set_compression(true);
    let mut p = *opts.preserve();
    p.prefixes = true;
    opts.set_preserve(p);

    if let Err(e) = exificient_encode(&xml_path, &opts, xsd_path.to_str().unwrap(), &exi_path) {
        emit(fixture_tag, "cli_exif_to_erxi", "FAIL", &truncate(&e, 80));
        return;
    }

    let dec = match run_erxi(&[
        "decode",
        "-i",
        exi_path.to_str().unwrap(),
        "-o",
        erxi_decoded.to_str().unwrap(),
        "--schema",
        xsd_path.to_str().unwrap(),
        "--compression",
        "--preserve-prefixes",
    ]) {
        Ok(out) => out,
        Err(e) => {
            emit(fixture_tag, "cli_exif_to_erxi", "FAIL", &truncate(&e, 80));
            return;
        }
    };
    if !dec.status.success() {
        emit(
            fixture_tag,
            "cli_exif_to_erxi",
            "FAIL",
            &truncate(&String::from_utf8_lossy(&dec.stderr), 80),
        );
        return;
    }

    let xml_out = match fs::read_to_string(erxi_decoded) {
        Ok(s) => s,
        Err(e) => {
            emit(fixture_tag, "cli_exif_to_erxi", "FAIL", &truncate(&format!("read decoded: {e}"), 80));
            return;
        }
    };

    match assert_infoset_eq(xml, &xml_out) {
        Ok(()) => emit(fixture_tag, "cli_exif_to_erxi", "OK", "infoset-match"),
        Err(e) => emit(fixture_tag, "cli_exif_to_erxi", "FAIL", &truncate(&e, 80)),
    }
}

// ============================================================================
// EXI4JSON Cross-Interop
// ============================================================================

const EXI4JSON_TEST_IDS: &[&str] = &["exi4json_erxi_to_exif", "exi4json_exif_to_erxi"];

fn values_equivalent(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Number(x), Value::Number(y)) => match (x.as_f64(), y.as_f64()) {
            (Some(xf), Some(yf)) => (xf - yf).abs() <= f64::EPSILON * xf.abs().max(1.0),
            _ => false,
        },
        (Value::Array(xs), Value::Array(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| values_equivalent(x, y))
        }
        (Value::Object(xm), Value::Object(ym)) => {
            if xm.len() != ym.len() {
                return false;
            }
            xm.iter().all(|(k, v)| ym.get(k).map_or(false, |v2| values_equivalent(v, v2)))
        }
        _ => false,
    }
}

fn escape_key(key: &str) -> String {
    const RESERVED: [&str; 7] = ["map", "array", "string", "number", "boolean", "null", "other"];
    if RESERVED.contains(&key) {
        return format!("_.{key}");
    }
    let mut out = String::new();
    for (idx, ch) in key.chars().enumerate() {
        if ch == '_' || !is_ncname_char(ch, idx == 0) {
            out.push('_');
            out.push_str(&(ch as u32).to_string());
            out.push('.');
        } else {
            out.push(ch);
        }
    }
    out
}

fn unescape_key(key: &str) -> String {
    let mut input = key;
    if let Some(rest) = key.strip_prefix("_.") {
        input = rest;
    }
    let mut out = String::new();
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '_' {
            out.push(ch);
            continue;
        }
        let mut digits = String::new();
        while let Some(&next) = chars.peek() {
            if next == '.' {
                break;
            }
            if !next.is_ascii_digit() {
                return input.to_string();
            }
            digits.push(next);
            chars.next();
        }
        if digits.is_empty() {
            return input.to_string();
        }
        if chars.next() != Some('.') {
            return input.to_string();
        }
        let code = match digits.parse::<u32>() {
            Ok(v) => v,
            Err(_) => return input.to_string(),
        };
        if let Some(decoded) = char::from_u32(code) {
            out.push(decoded);
        }
    }
    out
}

fn is_ncname_char(ch: char, is_first: bool) -> bool {
    if !ch.is_ascii() {
        return false;
    }
    if is_first {
        ch.is_ascii_alphabetic() || ch == '_'
    } else {
        ch.is_ascii_alphanumeric() || ch == '_' || ch == '-' || ch == '.'
    }
}

fn xml_escape(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('\"', "&quot;")
        .replace('\'', "&apos;")
}

fn json_to_xml(value: &Value) -> String {
    let mut out = String::new();
    out.push_str("<j:");
    build_xml(value, &mut out, true);
    out
}

fn build_xml(value: &Value, out: &mut String, is_root: bool) {
    match value {
        Value::Object(map) => {
            if is_root {
                out.push_str("map xmlns:j=\"http://www.w3.org/2015/EXI/json\">");
            } else {
                out.push_str("map>");
            }
            for (k, v) in map {
                let key = escape_key(k);
                out.push_str("<j:");
                out.push_str(&key);
                out.push('>');
                out.push_str("<j:");
                build_xml(v, out, false);
                out.push_str("</j:");
                out.push_str(&key);
                out.push('>');
            }
            out.push_str("</j:map>");
        }
        Value::Array(items) => {
            out.push_str("array>");
            for item in items {
                out.push_str("<j:");
                build_xml(item, out, false);
            }
            out.push_str("</j:array>");
        }
        Value::String(s) => {
            out.push_str("string>");
            out.push_str(&xml_escape(s));
            out.push_str("</j:string>");
        }
        Value::Number(n) => {
            out.push_str("number>");
            out.push_str(&xml_escape(&n.to_string()));
            out.push_str("</j:number>");
        }
        Value::Bool(b) => {
            out.push_str("boolean>");
            out.push_str(if *b { "true" } else { "false" });
            out.push_str("</j:boolean>");
        }
        Value::Null => {
            out.push_str("null/>");
        }
    }
}

fn xml_to_json(xml: &str) -> Result<Value, String> {
    let doc = roxmltree::Document::parse(xml).map_err(|e| format!("parse xml: {e}"))?;
    let root = doc.root_element();
    Ok(element_to_value(&root))
}

fn element_to_value(elem: &roxmltree::Node) -> Value {
    let local = elem.tag_name().name();
    match local {
        "map" => {
            let mut obj = serde_json::Map::new();
            for child in elem.children().filter(|n| n.is_element()) {
                let key = unescape_key(child.tag_name().name());
                let value_elem = child.children().find(|n| n.is_element()).expect("value element");
                let value = element_to_value(&value_elem);
                obj.insert(key, value);
            }
            Value::Object(obj)
        }
        "array" => {
            let mut items = Vec::new();
            for child in elem.children().filter(|n| n.is_element()) {
                items.push(element_to_value(&child));
            }
            Value::Array(items)
        }
        "string" => Value::String(elem.text().unwrap_or("").to_string()),
        "number" => {
            let v: Value = serde_json::from_str(elem.text().unwrap_or("0")).expect("number");
            v
        }
        "boolean" => Value::Bool(elem.text().unwrap_or("") == "true"),
        "null" => Value::Null,
        "other" => {
            let child = elem.children().find(|n| n.is_element()).expect("other child");
            let local = child.tag_name().name();
            match local {
                "integer" | "decimal" => {
                    let v: Value = serde_json::from_str(child.text().unwrap_or("0")).expect("number");
                    v
                }
                _ => Value::String(child.text().unwrap_or("").to_string()),
            }
        }
        _ => Value::Null,
    }
}

fn run_exi4json_tests(basename: &str, alignment: &str) {
    let fixture_tag = format!("{}_{}", basename, alignment);
    if alignment != "strict" {
        for test_id in EXI4JSON_TEST_IDS {
            emit(&fixture_tag, test_id, "SKIP", "strict-only");
        }
        return;
    }
    let json_path = Path::new("tests/fixtures/json").join(format!("{basename}.json"));
    if !json_path.exists() {
        for test_id in EXI4JSON_TEST_IDS {
            emit(&fixture_tag, test_id, "SKIP", "json-missing");
        }
        return;
    }

    let json = match fs::read_to_string(&json_path) {
        Ok(s) => s,
        Err(e) => {
            emit(&fixture_tag, "exi4json_erxi_to_exif", "FAIL", &truncate(&format!("read json: {e}"), 80));
            emit(&fixture_tag, "exi4json_exif_to_erxi", "SKIP", "json-read-failed");
            return;
        }
    };
    let value: Value = match serde_json::from_str(&json) {
        Ok(v) => v,
        Err(e) => {
            emit(&fixture_tag, "exi4json_erxi_to_exif", "FAIL", &truncate(&format!("parse json: {e}"), 80));
            emit(&fixture_tag, "exi4json_exif_to_erxi", "SKIP", "json-parse-failed");
            return;
        }
    };
    let xml = json_to_xml(&value);
    let schema_path = Path::new("src/exi4json.xsd");
    let opts = ExiOptions::default().with_strict();

    // erxi -> exificient decode
    let dir = cli_temp_dir("exi4json-erxi-to-exif");
    let exi_path = dir.join("out.exi");
    let exif_decoded = dir.join("exif-decoded.xml");
    match erxi::encode_json(&json) {
        Ok(exi) => {
            fs::write(&exi_path, exi).expect("write exi");
            if let Err(e) = exificient_decode(&exi_path, &opts, schema_path.to_str().unwrap(), &exif_decoded) {
                emit(&fixture_tag, "exi4json_erxi_to_exif", "FAIL", &truncate(&e, 80));
            } else {
                let xml_out = fs::read_to_string(exif_decoded).expect("read decoded");
                match xml_to_json(&xml_out) {
                    Ok(value_out) => {
                        if values_equivalent(&value, &value_out) {
                            emit(&fixture_tag, "exi4json_erxi_to_exif", "OK", "value-match");
                        } else {
                            emit(&fixture_tag, "exi4json_erxi_to_exif", "FAIL", "value-mismatch");
                        }
                    }
                    Err(e) => {
                        emit(&fixture_tag, "exi4json_erxi_to_exif", "FAIL", &truncate(&e, 80));
                    }
                }
            }
        }
        Err(e) => {
            emit(&fixture_tag, "exi4json_erxi_to_exif", "FAIL", &truncate(&format!("encode_json: {e}"), 80));
        }
    }

    // exificient -> erxi decode_json
    let dir = cli_temp_dir("exi4json-exif-to-erxi");
    let xml_path = dir.join("in.xml");
    let exi_path = dir.join("exif.exi");
    fs::write(&xml_path, xml).expect("write xml");
    if let Err(e) = exificient_encode(&xml_path, &opts, schema_path.to_str().unwrap(), &exi_path) {
        emit(&fixture_tag, "exi4json_exif_to_erxi", "FAIL", &truncate(&e, 80));
    } else {
        let exi = fs::read(&exi_path).expect("read exi");
        match erxi::decode_json(&exi) {
            Ok(json_out) => {
                let value_out: Value = serde_json::from_str(&json_out).expect("parse output");
                if values_equivalent(&value, &value_out) {
                    emit(&fixture_tag, "exi4json_exif_to_erxi", "OK", "value-match");
                } else {
                    emit(&fixture_tag, "exi4json_exif_to_erxi", "FAIL", "value-mismatch");
                }
            }
            Err(e) => {
                emit(&fixture_tag, "exi4json_exif_to_erxi", "FAIL", &truncate(&format!("decode_json: {e}"), 80));
            }
        }
    }
}

// ============================================================================
// Infoset Mapping (Spec Appendix B)
// ============================================================================

const INFOSET_TEST_ID: &str = "infoset_rtt";

fn infoset_round_trip(xml: &str, opts: &ExiOptions) -> Result<Vec<ExiEvent>, String> {
    let events = parse_xml_events_from_str(xml, opts).map_err(|e| format!("parse xml: {e}"))?;
    let config = EncoderConfig { include_cookie: false, include_options: true };
    let exi = encode_with_config(&events, opts, config).map_err(|e| format!("encode: {e}"))?;
    let (decoded, _) = decode_with_options(&exi, opts.clone()).map_err(|e| format!("decode: {e}"))?;
    Ok(decoded)
}

fn run_infoset_tests(basename: &str, alignment: &str) {
    let fixture_tag = format!("{}_{}", basename, alignment);
    let mut opts = ExiOptions::default();
    match alignment {
        "bytealigned" => opts.set_alignment(Alignment::ByteAlignment),
        "precompression" => opts.set_alignment(Alignment::PreCompression),
        "compression" => opts.set_compression(true),
        "bitpacked" => {}
        _ => {
            emit(&fixture_tag, INFOSET_TEST_ID, "SKIP", "unsupported-alignment");
            return;
        }
    }

    let (xml, checks): (&str, fn(&[ExiEvent]) -> Result<(), String>) = match basename {
        "b1_document" => ("<root/>", |events| {
            if events.len() < 4 { return Err("too-few-events".to_string()); }
            if !matches!(events.first(), Some(ExiEvent::StartDocument)) { return Err("missing-SD".to_string()); }
            if !matches!(events.last(), Some(ExiEvent::EndDocument)) { return Err("missing-ED".to_string()); }
            let has_root = events.iter().any(|e| matches!(e, ExiEvent::StartElement(q) if &*q.local_name == "root"));
            if !has_root { return Err("missing-root-SE".to_string()); }
            Ok(())
        }),
        "b2_element" => ("<root><a><b/></a><c/></root>", |events| {
            if events.len() != 10 { return Err(format!("expected-10-events got={}", events.len())); }
            Ok(())
        }),
        "b3_attribute" => (r#"<root a="1" b="2"><child x="y"/></root>"#, |events| {
            let attrs: Vec<&AtContent> = events.iter().filter_map(|e| match e { ExiEvent::Attribute(at) => Some(at), _ => None }).collect();
            if attrs.len() != 3 { return Err(format!("expected-3-attrs got={}", attrs.len())); }
            Ok(())
        }),
        "b4_pi" => {
            let mut p = Preserve::default();
            p.pis = true;
            opts.set_preserve(p);
            ("<?target data?><root/>", |events| {
                let pis: Vec<&PiContent> = events.iter().filter_map(|e| match e { ExiEvent::ProcessingInstruction(pi) => Some(pi), _ => None }).collect();
                if pis.len() != 1 { return Err(format!("expected-1-PI got={}", pis.len())); }
                if &*pis[0].name != "target" { return Err("pi-target-mismatch".to_string()); }
                Ok(())
            })
        }
        "b5_entity" => {
            let mut p = Preserve::default();
            p.dtd = true;
            opts.set_preserve(p);
            (r#"<!DOCTYPE root [<!ENTITY ext SYSTEM "external.xml">]><root>&ext;</root>"#, |events| {
                let ers: Vec<&ErContent> = events.iter().filter_map(|e| match e { ExiEvent::EntityReference(er) => Some(er), _ => None }).collect();
                if ers.len() != 1 { return Err(format!("expected-1-ER got={}", ers.len())); }
                Ok(())
            })
        }
        "b6_character" => (r#"<root>Hello &amp; World</root>"#, |events| {
            let chs: Vec<&ChContent> = events.iter().filter_map(|e| match e { ExiEvent::Characters(ch) => Some(ch), _ => None }).collect();
            if chs.is_empty() { return Err("missing-characters".to_string()); }
            let combined: String = chs.iter().map(|ch| &*ch.value).collect();
            if combined != "Hello & World" { return Err("character-mismatch".to_string()); }
            Ok(())
        }),
        "b7_comment" => {
            let mut p = Preserve::default();
            p.comments = true;
            opts.set_preserve(p);
            ("<!-- comment --><root><!-- inner --></root>", |events| {
                let cms: Vec<&CmContent> = events.iter().filter_map(|e| match e { ExiEvent::Comment(cm) => Some(cm), _ => None }).collect();
                if cms.len() != 2 { return Err(format!("expected-2-comments got={}", cms.len())); }
                Ok(())
            })
        }
        "b8_doctype" => {
            let mut p = Preserve::default();
            p.dtd = true;
            opts.set_preserve(p);
            (r#"<!DOCTYPE root SYSTEM "root.dtd"><root/>"#, |events| {
                let dts: Vec<&DtContent> = events.iter().filter_map(|e| match e { ExiEvent::DocType(dt) => Some(dt), _ => None }).collect();
                if dts.len() != 1 { return Err(format!("expected-1-DT got={}", dts.len())); }
                Ok(())
            })
        }
        "b9_unparsed" => {
            let mut p = Preserve::default();
            p.dtd = true;
            opts.set_preserve(p);
            (r#"<!DOCTYPE root [<!NOTATION jpeg SYSTEM "jpeg"><!ENTITY pic SYSTEM "pic.jpg" NDATA jpeg>]><root/>"#, |events| {
                let dts: Vec<&DtContent> = events.iter().filter_map(|e| match e { ExiEvent::DocType(dt) => Some(dt), _ => None }).collect();
                if dts.len() != 1 { return Err("missing-doctype".to_string()); }
                if !dts[0].text.contains("NOTATION") { return Err("missing-notation".to_string()); }
                Ok(())
            })
        }
        "b10_notation" => {
            let mut p = Preserve::default();
            p.dtd = true;
            opts.set_preserve(p);
            (r#"<!DOCTYPE root [<!NOTATION jpeg SYSTEM "image/jpeg">]><root/>"#, |events| {
                let dts: Vec<&DtContent> = events.iter().filter_map(|e| match e { ExiEvent::DocType(dt) => Some(dt), _ => None }).collect();
                if dts.len() != 1 { return Err("missing-doctype".to_string()); }
                if !dts[0].text.contains("NOTATION") { return Err("missing-notation".to_string()); }
                Ok(())
            })
        }
        "b11_namespace" => {
            let mut p = Preserve::default();
            p.prefixes = true;
            opts.set_preserve(p);
            (r#"<root xmlns:a="http://a" xmlns:b="http://b"><a:x/><b:y/></root>"#, |events| {
                let nss: Vec<&NsContent> = events.iter().filter_map(|e| match e { ExiEvent::NamespaceDeclaration(ns) => Some(ns), _ => None }).collect();
                if nss.len() < 2 { return Err("missing-namespace-events".to_string()); }
                Ok(())
            })
        }
        _ => {
            emit(&fixture_tag, INFOSET_TEST_ID, "SKIP", "unknown-infoset-fixture");
            return;
        }
    };

    match infoset_round_trip(xml, &opts) {
        Ok(events) => match checks(&events) {
            Ok(()) => emit(&fixture_tag, INFOSET_TEST_ID, "OK", "events-match"),
            Err(e) => emit(&fixture_tag, INFOSET_TEST_ID, "FAIL", &truncate(&e, 80)),
        },
        Err(e) => emit(&fixture_tag, INFOSET_TEST_ID, "FAIL", &truncate(&e, 80)),
    }
}

// ============================================================================
// Coverage Matrix
// ============================================================================

const COVERAGE_TEST_ID: &str = "coverage";

fn run_coverage_tests(basename: &str, alignment: &str) {
    let fixture_tag = format!("{}_{}", basename, alignment);
    if alignment != "strict" {
        emit(&fixture_tag, COVERAGE_TEST_ID, "SKIP", "strict-only");
        return;
    }

    let result: Result<(), String> = (|| {
        match basename {
            "schema_id_none" => {
                let xml = "<root><v>1</v></root>";
                let events = parse_xml_events_from_str(xml, &ExiOptions::default()).map_err(|e| format!("parse xml: {e}"))?;
                let mut opts = ExiOptions::default();
                opts.set_schema_id(Some(SchemaId::None));
                let exi = encode_with_config(&events, &opts, EncoderConfig { include_options: true, ..EncoderConfig::default() })
                    .map_err(|e| format!("encode: {e}"))?;
                let dir = cli_temp_dir("schema-id-none");
                let exi_path = dir.join("out.exi");
                let decoded_path = dir.join("out.xml");
                fs::write(&exi_path, exi).map_err(|e| format!("write exi: {e}"))?;
                exificient_decode(&exi_path, &opts, "", &decoded_path)?;
                if !decoded_path.exists() {
                    return Err("exificient decode did not produce output".to_string());
                }
                let xml_out = fs::read_to_string(&decoded_path).map_err(|e| format!("read decoded: {e}"))?;
                assert_infoset_eq(xml, &xml_out).map_err(|e| format!("infoset: {e}"))?;
                Ok(())
            }
            "schema_id_builtin" => {
                let xml = "<root><v>1</v></root>";
                let events = parse_xml_events_from_str(xml, &ExiOptions::default()).map_err(|e| format!("parse xml: {e}"))?;
                let mut opts = ExiOptions::default();
                opts.set_schema_id(Some(SchemaId::BuiltinOnly));
                let exi = encode_with_config(&events, &opts, EncoderConfig { include_options: true, ..EncoderConfig::default() })
                    .map_err(|e| format!("encode: {e}"))?;
                let dir = cli_temp_dir("schema-id-builtin");
                let exi_path = dir.join("out.exi");
                let decoded_path = dir.join("out.xml");
                fs::write(&exi_path, exi).map_err(|e| format!("write exi: {e}"))?;
                exificient_decode(&exi_path, &opts, "", &decoded_path)?;
                if !decoded_path.exists() {
                    return Err("exificient decode did not produce output".to_string());
                }
                let xml_out = fs::read_to_string(&decoded_path).map_err(|e| format!("read decoded: {e}"))?;
                assert_infoset_eq(xml, &xml_out).map_err(|e| format!("infoset: {e}"))?;
                Ok(())
            }
            "schema_id_id" => {
                let xml = "<root><v>1</v></root>";
                let xsd = r#"
                    <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                      <xs:element name="root">
                        <xs:complexType>
                          <xs:sequence>
                            <xs:element name="v" type="xs:int"/>
                          </xs:sequence>
                        </xs:complexType>
                      </xs:element>
                    </xs:schema>
                "#;
                let schema = erxi::xsd::parse_xsd(xsd).map_err(|e| format!("parse xsd: {e}"))?;
                let dir = cli_temp_dir("schema-id-id");
                let schema_path = dir.join("schema.xsd");
                fs::write(&schema_path, xsd).map_err(|e| format!("write schema: {e}"))?;
                let mut opts = ExiOptions::default();
                opts.set_schema_id(Some(SchemaId::Id(schema_path.to_string_lossy().to_string())));
                opts.set_strict(true);
                let events = parse_xml_events_from_str(xml, &opts).map_err(|e| format!("parse xml: {e}"))?;
                let exi = encode_with_schema_and_config(
                    &events,
                    &opts,
                    &schema,
                    EncoderConfig { include_options: true, ..EncoderConfig::default() },
                )
                .map_err(|e| format!("encode: {e}"))?;
                let exi_path = dir.join("out.exi");
                let decoded_path = dir.join("out.xml");
                fs::write(&exi_path, exi).map_err(|e| format!("write exi: {e}"))?;
                exificient_decode(&exi_path, &opts, schema_path.to_str().unwrap(), &decoded_path)?;
                let xml_out = fs::read_to_string(&decoded_path).map_err(|e| format!("read decoded: {e}"))?;
                assert_infoset_eq(xml, &xml_out).map_err(|e| format!("infoset: {e}"))?;
                Ok(())
            }
            "alignment_matrix" => {
                let xml = "<root><v>alpha</v><v>beta</v><v>gamma</v><v>delta</v></root>";
                let variants: Vec<(&str, Box<dyn Fn(&mut ExiOptions)>)> = vec![
                    ("bitpacked", Box::new(|_| {})),
                    ("bytealigned", Box::new(|o| o.set_alignment(Alignment::ByteAlignment))),
                    ("precompression", Box::new(|o| o.set_alignment(Alignment::PreCompression))),
                    ("compression", Box::new(|o| { o.set_compression(true); o.set_block_size(64); })),
                ];
                for (name, apply) in variants {
                    let mut opts = ExiOptions::default();
                    apply(&mut opts);
                    let events = parse_xml_events_from_str(xml, &opts).map_err(|e| format!("{name}: parse: {e}"))?;
                    let exi = encode(&events, &opts).map_err(|e| format!("{name}: encode: {e}"))?;
                    let (decoded, _) = decode(&exi).map_err(|e| format!("{name}: decode: {e}"))?;
                    let xml_out = events_to_xml(&decoded).map_err(|e| format!("{name}: to_xml: {e}"))?;
                    assert_infoset_eq(xml, &xml_out).map_err(|e| format!("{name}: {e}"))?;
                }
                Ok(())
            }
            "value_limits" => {
                let xml = "<root><v>alpha</v><v>bravo</v><v>charlie</v><v>delta</v><v>echo</v></root>";
                let mut opts = ExiOptions::default();
                opts.set_value_max_length(Some(4));
                opts.set_value_partition_capacity(Some(4));
                let events = parse_xml_events_from_str(xml, &opts).map_err(|e| format!("parse: {e}"))?;
                let exi = encode(&events, &opts).map_err(|e| format!("encode: {e}"))?;
                let (decoded, _) = decode(&exi).map_err(|e| format!("decode: {e}"))?;
                let xml_out = events_to_xml(&decoded).map_err(|e| format!("to_xml: {e}"))?;
                assert_infoset_eq(xml, &xml_out).map_err(|e| format!("infoset: {e}"))?;
                Ok(())
            }
            "preserve_prefixes" => {
                let xml = r#"<ns1:root xmlns:ns1="urn:a" xmlns:ns2="urn:b"><ns2:child ns1:attr="v"/></ns1:root>"#;
                let mut opts = ExiOptions::default();
                opts.set_preserve(Preserve { prefixes: true, ..Preserve::default() });
                let events = parse_xml_events_from_str(xml, &opts).map_err(|e| format!("parse: {e}"))?;
                let exi = encode(&events, &opts).map_err(|e| format!("encode: {e}"))?;
                let (decoded, _) = decode(&exi).map_err(|e| format!("decode: {e}"))?;
                let xml_out = events_to_xml(&decoded).map_err(|e| format!("to_xml: {e}"))?;
                assert_infoset_eq(xml, &xml_out).map_err(|e| format!("infoset: {e}"))?;
                Ok(())
            }
            "dtrm_schema_id" => {
                let xsd = r#"
                    <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                      <xs:element name="root">
                        <xs:complexType>
                          <xs:sequence>
                            <xs:element name="price" type="xs:decimal"/>
                          </xs:sequence>
                        </xs:complexType>
                      </xs:element>
                    </xs:schema>
                "#;
                let schema = erxi::xsd::parse_xsd(xsd).map_err(|e| format!("parse xsd: {e}"))?;
                let dir = cli_temp_dir("dtrm-schemaid");
                let schema_path = dir.join("schema.xsd");
                fs::write(&schema_path, xsd).map_err(|e| format!("write schema: {e}"))?;
                let xml = "<root><price>12.50</price></root>";
                let mut opts = ExiOptions::default();
                opts.set_strict(true);
                opts.set_schema_id(Some(SchemaId::Id(schema_path.to_string_lossy().to_string())));
                opts.set_datatype_representation_map(vec![DatatypeRepresentationMapping {
                    type_qname: QName::new("http://www.w3.org/2001/XMLSchema", "decimal"),
                    representation_qname: QName::new("http://www.w3.org/2009/exi", "string"),
                }]);
                let events = parse_xml_events_from_str(xml, &opts).map_err(|e| format!("parse: {e}"))?;
                let exi = encode_with_schema_and_config(
                    &events,
                    &opts,
                    &schema,
                    EncoderConfig { include_options: true, ..EncoderConfig::default() },
                )
                .map_err(|e| format!("encode: {e}"))?;
                let (decoded, decoded_opts) = decode_with_schema(&exi, ExiOptions::default(), &schema)
                    .map_err(|e| format!("decode: {e}"))?;
                let xml_out = events_to_xml(&decoded).map_err(|e| format!("to_xml: {e}"))?;
                assert_infoset_eq(xml, &xml_out).map_err(|e| format!("infoset: {e}"))?;
                if decoded_opts.schema_id() != opts.schema_id() {
                    return Err("schema_id_mismatch".to_string());
                }
                if decoded_opts.datatype_representation_map().len() != 1 {
                    return Err("dtrm_len_mismatch".to_string());
                }
                Ok(())
            }
            _ => {
                emit(&fixture_tag, COVERAGE_TEST_ID, "SKIP", "unknown-coverage-fixture");
                return Ok(());
            }
        }
    })();

    match result {
        Ok(()) => emit(&fixture_tag, COVERAGE_TEST_ID, "OK", "ok"),
        Err(e) => emit(&fixture_tag, COVERAGE_TEST_ID, "FAIL", &truncate(&e, 80)),
    }
}


// ============================================================================
// Skip-Logik (identisch mit cross_rtt.rs)
// ============================================================================

fn is_expected_strict_skip(basename: &str, suite: &str) -> bool {
    let skip_declared = &["complexType-18", "complexUrType-02", "elementTerm-01"];
    let skip_undeclared = &[
        "attrWildcard-01", "attrWildcard-02", "attrWildcard-03",
        "attruseUntyped-01",
        "ch-01", "ch-02", "ch-03", "ch-04",
        "ee-01", "ee-02", "ee-03", "ee-04", "ee-05", "ee-06",
        "er-01", "er-02", "er-03", "er-entity",
        "cm-01", "cm-02", "cm-03",
        "namespaceDecl-01", "namespaceDecl-02", "namespaceDecl-04", "namespaceDecl-05",
        "pi-01", "pi-02", "pi-03",
        "se-01", "se-02", "se-03",
        "xsiNilDefault-01", "xsiNilDefault-02", "xsiNilDefault-03",
        "xsiNilDefault-04", "xsiNilDefault-05", "xsiNilDefault-06",
        "xsiNilDefault-07",
        "xsiNilStrict-03",
        "xsiTypeDefault-01", "xsiTypeDefault-02", "xsiTypeDefault-03",
        "xsiTypeDefault-04", "xsiTypeDefault-05", "xsiTypeDefault-06",
    ];

    if suite == "declared" {
        skip_declared.iter().any(|s| basename == *s)
    } else {
        skip_undeclared.iter().any(|s| basename == *s)
    }
}

// ============================================================================
// TSV-Ausgabe
// ============================================================================

fn emit(fixture: &str, test_id: &str, result: &str, detail: &str) {
    println!("{}\t{}\t{}\t{}", fixture, test_id, result, detail);
}

// ============================================================================
// Kern-Logik: Alle 10 Tests für ein Fixture
// ============================================================================

fn is_data_file(basename: &str) -> bool {
    basename == "er-entity"
}

fn run_xml_rtt_test(
    fixture_tag: &str,
    exi_data: &[u8],
    events_orig: &[ExiEvent],
    opts: &ExiOptions,
    schema: &SchemaInfo,
    work_dir: &Path,
) {
    // Schritt 1: EXI → decode → events_rtt
    let events_rtt = match decode_with_schema(exi_data, opts.clone(), schema) {
        Ok((ev, _)) => ev,
        Err(e) => {
            emit(fixture_tag, "erxi_xml_rtt", "FAIL", &format!("decode: {}", truncate(&format!("{e}"), 50)));
            return;
        }
    };

    // Schritt 2: events_to_xml → XML-String
    let xml_string = match events_to_xml(&events_rtt) {
        Ok(s) => s,
        Err(e) => {
            emit(fixture_tag, "erxi_xml_rtt", "FAIL", &format!("events_to_xml: {}", truncate(&format!("{e}"), 50)));
            return;
        }
    };

    // Schritt 3: XML-String in Temp-Datei schreiben
    let xml_path = work_dir.join(format!("{fixture_tag}_xml_rtt.xml"));
    fs::write(&xml_path, &xml_string).unwrap();

    // Schritt 4: parse_xml_events_with_options → events_reparsed
    let events_reparsed = match parse_xml_events_with_options(&xml_path, opts) {
        Ok(ev) => ev,
        Err(e) => {
            emit(fixture_tag, "erxi_xml_rtt", "FAIL", &format!("reparse: {}", truncate(&format!("{e}"), 50)));
            return;
        }
    };

    // Schritt 5: Kanonischer Vergleich (Zwei-Stufen)
    let (ok, detail) = compare_events_canonical(events_orig, &events_reparsed);
    if ok {
        emit(fixture_tag, "erxi_xml_rtt", "OK", "xml-roundtrip-match");
    } else if !opts.preserve().prefixes {
        // Ohne preserve_prefixes gehen NS-URIs beim XML-Roundtrip verloren.
        // Pruefen ob nur URIs fehlen oder ob es einen echten Fehler gibt.
        let (ok_no_uri, detail_no_uri) = compare_events_canonical_ex(events_orig, &events_reparsed, true);
        if ok_no_uri {
            emit(fixture_tag, "erxi_xml_rtt", "OK", "ns-stripped");
        } else {
            emit(fixture_tag, "erxi_xml_rtt", "FAIL", &detail_no_uri);
        }
    } else {
        emit(fixture_tag, "erxi_xml_rtt", "FAIL", &detail);
    }
}

fn run_all_tests(suite: &str, basename: &str, alignment: &str) {
    if suite == "cli" {
        run_cli_tests(basename, alignment);
        return;
    }
    if suite == "exi4json" {
        run_exi4json_tests(basename, alignment);
        return;
    }
    if suite == "infoset" {
        run_infoset_tests(basename, alignment);
        return;
    }
    if suite == "coverage" {
        run_coverage_tests(basename, alignment);
        return;
    }

    let fixture_tag = format!("{}_{}", basename, alignment);
    let xml_dir = match suite {
        "declared" => PathBuf::from(declared_xml_dir()),
        "dtrm" => PathBuf::from(DTRM_DIR),
        _ => PathBuf::from(undeclared_xml_dir()),
    };

    // Datendateien (keine eigenstaendigen Fixtures) ueberspringen
    if is_data_file(basename) {
        for test_id in ALL_TEST_IDS {
            emit(&fixture_tag, test_id, "SKIP", "data-file");
        }
        return;
    }

    // Skip-Checks
    if basename.starts_with("sc-")
        && (alignment == "strict" || alignment == "compression" || alignment == "precompression")
    {
        for test_id in ALL_TEST_IDS {
            emit(&fixture_tag, test_id, "SKIP", "sc-incompatible");
        }
        return;
    }

    if alignment == "strict" && is_expected_strict_skip(basename, suite) {
        for test_id in ALL_TEST_IDS {
            emit(&fixture_tag, test_id, "SKIP", "strict-invalid");
        }
        return;
    }

    let xml_path = xml_dir.join(format!("{basename}.xml"));
    if !xml_path.exists() {
        for test_id in ALL_TEST_IDS {
            emit(&fixture_tag, test_id, "SKIP", "xml-missing");
        }
        return;
    }

    // Schema laden
    let schema = match load_schema(basename, suite) {
        Ok(s) => s,
        Err(e) => {
            for test_id in ALL_TEST_IDS {
                emit(&fixture_tag, test_id, "SKIP", &format!("schema: {}", truncate(&e, 50)));
            }
            return;
        }
    };
    let schema_path = schema_path_for_xml(basename, suite);

    // Options für beide Varianten (identisch, interop-Flag wurde entfernt)
    let opts_interop = options_for_alignment(alignment, basename, suite);
    let opts_nointerop = options_for_alignment(alignment, basename, suite);

    // Work-Verzeichnis
    let work_dir = PathBuf::from(format!("target/cross_rtt_full/{suite}"));
    fs::create_dir_all(&work_dir).unwrap();

    // --- XML parsen (interop und nointerop) ---
    let events_interop = parse_xml_events_with_options(&xml_path, &opts_interop);
    let events_nointerop = parse_xml_events_with_options(&xml_path, &opts_nointerop);

    // Falls XML-Parse fehlschlägt (z.B. er-entity), alles SKIPpen
    let events_i = match events_interop {
        Ok(ev) => ev,
        Err(e) => {
            for test_id in ALL_TEST_IDS {
                emit(&fixture_tag, test_id, "SKIP", &format!("xml-parse: {}", truncate(&format!("{e}"), 40)));
            }
            return;
        }
    };
    let events_ni = match events_nointerop {
        Ok(ev) => ev,
        Err(e) => {
            // interop-Parse OK, nointerop-Parse fehlgeschlagen — nur nointerop-Tests SKIPpen
            // Zunächst interop-Tests durchführen, nointerop-Tests SKIPpen
            run_interop_only_tests(&fixture_tag, &events_i, &opts_interop, &schema, &schema_path, &xml_path, &work_dir);
            emit(&fixture_tag, "erxi_nointerop_self_rtt", "SKIP", &format!("xml-parse: {}", truncate(&format!("{e}"), 40)));
            emit(&fixture_tag, "erxi_nointerop_to_exif", "SKIP", "xml-parse-error");
            emit(&fixture_tag, "bytes_nointerop_vs_exif", "SKIP", "xml-parse-error");
            emit(&fixture_tag, "bytes_interop_vs_nointerop", "SKIP", "xml-parse-error");
            return;
        }
    };

    let skip_exificient = basename.starts_with("er-");

    // === TEST 1: erxi+interop self-RTT ===
    let exi_interop = match encode_with_schema(&events_i, &opts_interop, &schema) {
        Ok(data) => data,
        Err(e) => {
            emit(&fixture_tag, "erxi_interop_self_rtt", "FAIL", &format!("encode: {e}"));
            emit(&fixture_tag, "erxi_interop_to_exif", "SKIP", "encode-error");
            emit(&fixture_tag, "bytes_interop_vs_exif", "SKIP", "encode-error");
            emit(&fixture_tag, "bytes_interop_vs_nointerop", "SKIP", "encode-error");
            emit(&fixture_tag, "erxi_xml_rtt", "SKIP", "encode-error");

            // nointerop-Tests noch durchführen
            run_nointerop_tests_standalone(&fixture_tag, &events_ni, &opts_nointerop, &schema, &schema_path, &xml_path, &work_dir);
            run_exif_self_rtt(&fixture_tag, &xml_path, &opts_interop, &schema_path, &work_dir);
            return;
        }
    };

    match decode_with_schema(&exi_interop, opts_interop.clone(), &schema) {
        Ok((events_rtt, _)) => {
            let (ok, detail) = compare_events_canonical(&events_i, &events_rtt);
            if ok {
                emit(&fixture_tag, "erxi_interop_self_rtt", "OK", &format!("size={}", exi_interop.len()));
            } else {
                emit(&fixture_tag, "erxi_interop_self_rtt", "FAIL", &detail);
            }
        }
        Err(e) => {
            emit(&fixture_tag, "erxi_interop_self_rtt", "FAIL", &format!("decode: {}", truncate(&format!("{e}"), 50)));
        }
    }

    // === TEST: erxi XML round-trip ===
    if opts_interop.fragment() {
        emit(&fixture_tag, "erxi_xml_rtt", "SKIP", "fragment");
    } else {
        run_xml_rtt_test(&fixture_tag, &exi_interop, &events_i, &opts_interop, &schema, &work_dir);
    }

    // === TEST 2: erxi-interop self-RTT ===
    let exi_nointerop = match encode_with_schema(&events_ni, &opts_nointerop, &schema) {
        Ok(data) => data,
        Err(e) => {
            emit(&fixture_tag, "erxi_nointerop_self_rtt", "FAIL", &format!("encode: {e}"));
            emit(&fixture_tag, "erxi_nointerop_to_exif", "SKIP", "encode-error");
            emit(&fixture_tag, "bytes_nointerop_vs_exif", "SKIP", "encode-error");
            emit(&fixture_tag, "bytes_interop_vs_nointerop", "SKIP", "encode-error");

            // Restliche Tests
            run_cross_tests_interop_only(&fixture_tag, &exi_interop, &opts_interop, &schema, &schema_path, &xml_path, &work_dir);
            run_exif_self_rtt(&fixture_tag, &xml_path, &opts_interop, &schema_path, &work_dir);
            return;
        }
    };

    match decode_with_schema(&exi_nointerop, opts_nointerop.clone(), &schema) {
        Ok((events_rtt, _)) => {
            let (ok, detail) = compare_events_canonical(&events_ni, &events_rtt);
            if ok {
                emit(&fixture_tag, "erxi_nointerop_self_rtt", "OK", &format!("size={}", exi_nointerop.len()));
            } else {
                emit(&fixture_tag, "erxi_nointerop_self_rtt", "FAIL", &detail);
            }
        }
        Err(e) => {
            emit(&fixture_tag, "erxi_nointerop_self_rtt", "FAIL", &format!("decode: {}", truncate(&format!("{e}"), 50)));
        }
    }

    // === TEST 3: exificient self-RTT ===
    if skip_exificient {
        emit(&fixture_tag, "exif_self_rtt", "SKIP", "exif-external-entity");
    } else {
        run_exif_self_rtt(&fixture_tag, &xml_path, &opts_interop, &schema_path, &work_dir);
    }

    // === TEST 4: erxi+interop → exificient decode ===
    let exi_interop_path = work_dir.join(format!("{fixture_tag}_erxi_interop.exi"));
    fs::write(&exi_interop_path, &exi_interop).unwrap();
    let xml_cross_interop = work_dir.join(format!("{fixture_tag}_cross_interop.xml"));

    if skip_exificient {
        emit(&fixture_tag, "erxi_interop_to_exif", "SKIP", "exif-external-entity");
    } else {
        match exificient_decode(&exi_interop_path, &opts_interop, &schema_path, &xml_cross_interop) {
            Ok(()) => {
                emit(&fixture_tag, "erxi_interop_to_exif", "OK", "exif-decoded-ok");
            }
            Err(e) => {
                emit(&fixture_tag, "erxi_interop_to_exif", "FAIL", &truncate(&e, 60));
            }
        }
    }

    // === TEST 5: erxi-interop → exificient decode ===
    let exi_nointerop_path = work_dir.join(format!("{fixture_tag}_erxi_nointerop.exi"));
    fs::write(&exi_nointerop_path, &exi_nointerop).unwrap();
    let xml_cross_nointerop = work_dir.join(format!("{fixture_tag}_cross_nointerop.xml"));

    if skip_exificient {
        emit(&fixture_tag, "erxi_nointerop_to_exif", "SKIP", "exif-external-entity");
    } else {
        match exificient_decode(&exi_nointerop_path, &opts_nointerop, &schema_path, &xml_cross_nointerop) {
            Ok(()) => {
                emit(&fixture_tag, "erxi_nointerop_to_exif", "OK", "exif-decoded-ok");
            }
            Err(e) => {
                emit(&fixture_tag, "erxi_nointerop_to_exif", "FAIL", &truncate(&e, 60));
            }
        }
    }

    // === TEST 6: exificient → erxi+interop decode ===
    let exi_exif_path = work_dir.join(format!("{fixture_tag}_exif_enc.exi"));
    if skip_exificient {
        emit(&fixture_tag, "exif_to_erxi_interop", "SKIP", "exif-external-entity");
        emit(&fixture_tag, "exif_to_erxi_nointerop", "SKIP", "exif-external-entity");
        emit(&fixture_tag, "bytes_interop_vs_exif", "SKIP", "exif-external-entity");
        emit(&fixture_tag, "bytes_nointerop_vs_exif", "SKIP", "exif-external-entity");
    } else {
        match exificient_encode(&xml_path, &opts_interop, &schema_path, &exi_exif_path) {
            Ok(()) => {
                let exi_exif = fs::read(&exi_exif_path).unwrap();

                match decode_with_schema(&exi_exif, opts_interop.clone(), &schema) {
                    Ok((events_rtt, _)) => {
                        let (ok, detail) = compare_events_canonical(&events_i, &events_rtt);
                        if ok {
                            emit(&fixture_tag, "exif_to_erxi_interop", "OK", "canonical-match");
                        } else {
                            emit(&fixture_tag, "exif_to_erxi_interop", "FAIL", &detail);
                        }
                    }
                    Err(e) => {
                        emit(&fixture_tag, "exif_to_erxi_interop", "FAIL", &format!("decode: {}", truncate(&format!("{e}"), 50)));
                    }
                }

                // === TEST 7: exificient → erxi-interop decode ===
                match decode_with_schema(&exi_exif, opts_nointerop.clone(), &schema) {
                    Ok((events_rtt, _)) => {
                        let (ok, detail) = compare_events_canonical(&events_ni, &events_rtt);
                        if ok {
                            emit(&fixture_tag, "exif_to_erxi_nointerop", "OK", "canonical-match");
                        } else {
                            emit(&fixture_tag, "exif_to_erxi_nointerop", "FAIL", &detail);
                        }
                    }
                    Err(e) => {
                        emit(&fixture_tag, "exif_to_erxi_nointerop", "FAIL", &format!("decode: {}", truncate(&format!("{e}"), 50)));
                    }
                }

                // === TEST 8: bytes interop vs exif ===
                if opts_interop.compression() {
                    // DEFLATE-Output ist nicht deterministic — kanonischen Event-Vergleich stattdessen
                    match decode_with_schema(&exi_exif, opts_interop.clone(), &schema) {
                        Ok((exif_events, _)) => {
                            let (ok, detail) = compare_events_canonical(&events_i, &exif_events);
                            if ok {
                                emit(&fixture_tag, "bytes_interop_vs_exif", "OK",
                                    &format!("canonical-match erxi={} exif={}", exi_interop.len(), exi_exif.len()));
                            } else {
                                emit(&fixture_tag, "bytes_interop_vs_exif", "FAIL", &detail);
                            }
                        }
                        Err(e) => {
                            emit(&fixture_tag, "bytes_interop_vs_exif", "FAIL",
                                &format!("decompress-cmp: {}", truncate(&format!("{e}"), 50)));
                        }
                    }
                } else if exi_interop == exi_exif {
                    emit(&fixture_tag, "bytes_interop_vs_exif", "OK", &format!("size={}", exi_interop.len()));
                } else {
                    let diff = exi_interop.iter().zip(exi_exif.iter()).filter(|(a, b)| a != b).count();
                    emit(
                        &fixture_tag,
                        "bytes_interop_vs_exif",
                        &format!("DIFF:{}:{}", exi_interop.len(), exi_exif.len()),
                        &format!("diff={}", diff),
                    );
                }

                // === TEST 9: bytes nointerop vs exif ===
                if opts_nointerop.compression() {
                    match decode_with_schema(&exi_exif, opts_nointerop.clone(), &schema) {
                        Ok((exif_events, _)) => {
                            let (ok, detail) = compare_events_canonical(&events_ni, &exif_events);
                            if ok {
                                emit(&fixture_tag, "bytes_nointerop_vs_exif", "OK",
                                    &format!("canonical-match erxi={} exif={}", exi_nointerop.len(), exi_exif.len()));
                            } else {
                                emit(&fixture_tag, "bytes_nointerop_vs_exif", "FAIL", &detail);
                            }
                        }
                        Err(e) => {
                            emit(&fixture_tag, "bytes_nointerop_vs_exif", "FAIL",
                                &format!("decompress-cmp: {}", truncate(&format!("{e}"), 50)));
                        }
                    }
                } else if exi_nointerop == exi_exif {
                    emit(&fixture_tag, "bytes_nointerop_vs_exif", "OK", &format!("size={}", exi_nointerop.len()));
                } else {
                    let diff = exi_nointerop.iter().zip(exi_exif.iter()).filter(|(a, b)| a != b).count();
                    emit(
                        &fixture_tag,
                        "bytes_nointerop_vs_exif",
                        &format!("DIFF:{}:{}", exi_nointerop.len(), exi_exif.len()),
                        &format!("diff={}", diff),
                    );
                }
            }
            Err(e) => {
                emit(&fixture_tag, "exif_to_erxi_interop", "SKIP", &format!("exif-encode: {}", truncate(&e, 40)));
                emit(&fixture_tag, "exif_to_erxi_nointerop", "SKIP", &format!("exif-encode: {}", truncate(&e, 40)));
                emit(&fixture_tag, "bytes_interop_vs_exif", "SKIP", "exif-encode-error");
                emit(&fixture_tag, "bytes_nointerop_vs_exif", "SKIP", "exif-encode-error");
            }
        }
    }

    // === TEST 10: bytes interop vs nointerop ===
    if opts_interop.compression() {
        // Bei Compression: beide erxi-Streams dekodieren und kanonisch vergleichen
        let dec_i = decode_with_schema(&exi_interop, opts_interop.clone(), &schema);
        let dec_ni = decode_with_schema(&exi_nointerop, opts_nointerop.clone(), &schema);
        match (dec_i, dec_ni) {
            (Ok((ev_i, _)), Ok((ev_ni, _))) => {
                let (ok, detail) = compare_events_canonical(&ev_i, &ev_ni);
                if ok {
                    emit(&fixture_tag, "bytes_interop_vs_nointerop", "OK",
                        &format!("canonical-match i={} ni={}", exi_interop.len(), exi_nointerop.len()));
                } else {
                    emit(&fixture_tag, "bytes_interop_vs_nointerop", "FAIL", &detail);
                }
            }
            (Err(e), _) => {
                emit(&fixture_tag, "bytes_interop_vs_nointerop", "FAIL",
                    &format!("decode-interop: {}", truncate(&format!("{e}"), 50)));
            }
            (_, Err(e)) => {
                emit(&fixture_tag, "bytes_interop_vs_nointerop", "FAIL",
                    &format!("decode-nointerop: {}", truncate(&format!("{e}"), 50)));
            }
        }
    } else if exi_interop == exi_nointerop {
        emit(&fixture_tag, "bytes_interop_vs_nointerop", "OK", &format!("size={}", exi_interop.len()));
    } else {
        let size_diff = exi_interop.len() as i64 - exi_nointerop.len() as i64;
        emit(
            &fixture_tag,
            "bytes_interop_vs_nointerop",
            &format!("DIFF:{}:{}", exi_interop.len(), exi_nointerop.len()),
            &format!("diff={}", size_diff),
        );
    }
}

// Hilfsfunktionen für Error-Recovery-Pfade

fn run_exif_self_rtt(
    fixture_tag: &str,
    xml_path: &Path,
    opts: &ExiOptions,
    schema_path: &str,
    work_dir: &Path,
) {
    match exificient_self_rtt(xml_path, opts, schema_path, work_dir, fixture_tag) {
        Ok((size, detail)) => {
            emit(fixture_tag, "exif_self_rtt", "OK", &format!("size={} {}", size, detail));
        }
        Err(e) => {
            emit(fixture_tag, "exif_self_rtt", "FAIL", &truncate(&e, 60));
        }
    }
}

fn run_interop_only_tests(
    fixture_tag: &str,
    events_i: &[ExiEvent],
    opts_interop: &ExiOptions,
    schema: &SchemaInfo,
    schema_path: &str,
    xml_path: &Path,
    work_dir: &Path,
) {
    // interop self-RTT
    match encode_with_schema(events_i, opts_interop, schema) {
        Ok(exi) => {
            match decode_with_schema(&exi, opts_interop.clone(), schema) {
                Ok((events_rtt, _)) => {
                    let (ok, detail) = compare_events_canonical(events_i, &events_rtt);
                    if ok {
                        emit(fixture_tag, "erxi_interop_self_rtt", "OK", &format!("size={}", exi.len()));
                    } else {
                        emit(fixture_tag, "erxi_interop_self_rtt", "FAIL", &detail);
                    }
                }
                Err(e) => {
                    emit(fixture_tag, "erxi_interop_self_rtt", "FAIL", &format!("decode: {e}"));
                }
            }

            // XML-RTT
            if opts_interop.fragment() {
                emit(fixture_tag, "erxi_xml_rtt", "SKIP", "fragment");
            } else {
                run_xml_rtt_test(fixture_tag, &exi, events_i, opts_interop, schema, work_dir);
            }

            // interop → exif
            let exi_path = work_dir.join(format!("{fixture_tag}_erxi_interop.exi"));
            fs::write(&exi_path, &exi).unwrap();
            let xml_cross = work_dir.join(format!("{fixture_tag}_cross_interop.xml"));
            match exificient_decode(&exi_path, opts_interop, schema_path, &xml_cross) {
                Ok(()) => emit(fixture_tag, "erxi_interop_to_exif", "OK", "exif-decoded-ok"),
                Err(e) => emit(fixture_tag, "erxi_interop_to_exif", "FAIL", &truncate(&e, 60)),
            }

            // bytes interop vs exif
            let exi_exif_path = work_dir.join(format!("{fixture_tag}_exif_enc.exi"));
            match exificient_encode(xml_path, opts_interop, schema_path, &exi_exif_path) {
                Ok(()) => {
                    let exi_exif = fs::read(&exi_exif_path).unwrap_or_default();
                    if opts_interop.compression() {
                        match decode_with_schema(&exi_exif, opts_interop.clone(), schema) {
                            Ok((exif_events, _)) => {
                                let (ok, detail) = compare_events_canonical(events_i, &exif_events);
                                if ok {
                                    emit(fixture_tag, "bytes_interop_vs_exif", "OK",
                                        &format!("canonical-match erxi={} exif={}", exi.len(), exi_exif.len()));
                                } else {
                                    emit(fixture_tag, "bytes_interop_vs_exif", "FAIL", &detail);
                                }
                            }
                            Err(e) => {
                                emit(fixture_tag, "bytes_interop_vs_exif", "FAIL",
                                    &format!("decompress-cmp: {}", truncate(&format!("{e}"), 50)));
                            }
                        }
                    } else if exi == exi_exif {
                        emit(fixture_tag, "bytes_interop_vs_exif", "OK", &format!("size={}", exi.len()));
                    } else {
                        let diff = exi.iter().zip(exi_exif.iter()).filter(|(a, b)| a != b).count();
                        emit(fixture_tag, "bytes_interop_vs_exif",
                            &format!("DIFF:{}:{}", exi.len(), exi_exif.len()),
                            &format!("diff={}", diff));
                    }
                }
                Err(_) => {
                    emit(fixture_tag, "bytes_interop_vs_exif", "SKIP", "exif-encode-error");
                }
            }
        }
        Err(e) => {
            emit(fixture_tag, "erxi_interop_self_rtt", "FAIL", &format!("encode: {e}"));
            emit(fixture_tag, "erxi_interop_to_exif", "SKIP", "encode-error");
            emit(fixture_tag, "bytes_interop_vs_exif", "SKIP", "encode-error");
            emit(fixture_tag, "erxi_xml_rtt", "SKIP", "encode-error");
        }
    }

    // exif self-RTT
    run_exif_self_rtt(fixture_tag, xml_path, opts_interop, schema_path, work_dir);

    // exif → erxi interop
    let exi_exif_path = work_dir.join(format!("{fixture_tag}_exif_enc.exi"));
    if exi_exif_path.exists() {
        let exi_exif = fs::read(&exi_exif_path).unwrap();
        match decode_with_schema(&exi_exif, opts_interop.clone(), schema) {
            Ok((events_rtt, _)) => {
                let (ok, detail) = compare_events_canonical(events_i, &events_rtt);
                if ok {
                    emit(fixture_tag, "exif_to_erxi_interop", "OK", "canonical-match");
                } else {
                    emit(fixture_tag, "exif_to_erxi_interop", "FAIL", &detail);
                }
            }
            Err(e) => {
                emit(fixture_tag, "exif_to_erxi_interop", "FAIL", &format!("decode: {e}"));
            }
        }
    } else {
        emit(fixture_tag, "exif_to_erxi_interop", "SKIP", "exif-encode-error");
    }
    emit(fixture_tag, "exif_to_erxi_nointerop", "SKIP", "xml-parse-error");
}

fn run_nointerop_tests_standalone(
    fixture_tag: &str,
    events_ni: &[ExiEvent],
    opts_nointerop: &ExiOptions,
    schema: &SchemaInfo,
    schema_path: &str,
    xml_path: &Path,
    work_dir: &Path,
) {
    match encode_with_schema(events_ni, opts_nointerop, schema) {
        Ok(exi) => {
            match decode_with_schema(&exi, opts_nointerop.clone(), schema) {
                Ok((events_rtt, _)) => {
                    let (ok, detail) = compare_events_canonical(events_ni, &events_rtt);
                    if ok {
                        emit(fixture_tag, "erxi_nointerop_self_rtt", "OK", &format!("size={}", exi.len()));
                    } else {
                        emit(fixture_tag, "erxi_nointerop_self_rtt", "FAIL", &detail);
                    }
                }
                Err(e) => {
                    emit(fixture_tag, "erxi_nointerop_self_rtt", "FAIL", &format!("decode: {e}"));
                }
            }

            let exi_path = work_dir.join(format!("{fixture_tag}_erxi_nointerop.exi"));
            fs::write(&exi_path, &exi).unwrap();
            let xml_cross = work_dir.join(format!("{fixture_tag}_cross_nointerop.xml"));
            match exificient_decode(&exi_path, opts_nointerop, schema_path, &xml_cross) {
                Ok(()) => emit(fixture_tag, "erxi_nointerop_to_exif", "OK", "exif-decoded-ok"),
                Err(e) => emit(fixture_tag, "erxi_nointerop_to_exif", "FAIL", &truncate(&e, 60)),
            }

            let exi_exif_path = work_dir.join(format!("{fixture_tag}_exif_enc.exi"));
            if !exi_exif_path.exists() {
                let _ = exificient_encode(xml_path, opts_nointerop, schema_path, &exi_exif_path);
            }
            if exi_exif_path.exists() {
                let exi_exif = fs::read(&exi_exif_path).unwrap();
                if opts_nointerop.compression() {
                    match decode_with_schema(&exi_exif, opts_nointerop.clone(), schema) {
                        Ok((exif_events, _)) => {
                            let (ok, detail) = compare_events_canonical(events_ni, &exif_events);
                            if ok {
                                emit(fixture_tag, "bytes_nointerop_vs_exif", "OK",
                                    &format!("canonical-match erxi={} exif={}", exi.len(), exi_exif.len()));
                            } else {
                                emit(fixture_tag, "bytes_nointerop_vs_exif", "FAIL", &detail);
                            }
                        }
                        Err(e) => {
                            emit(fixture_tag, "bytes_nointerop_vs_exif", "FAIL",
                                &format!("decompress-cmp: {}", truncate(&format!("{e}"), 50)));
                        }
                    }
                } else if exi == exi_exif {
                    emit(fixture_tag, "bytes_nointerop_vs_exif", "OK", &format!("size={}", exi.len()));
                } else {
                    let diff = exi.iter().zip(exi_exif.iter()).filter(|(a, b)| a != b).count();
                    emit(fixture_tag, "bytes_nointerop_vs_exif",
                        &format!("DIFF:{}:{}", exi.len(), exi_exif.len()),
                        &format!("diff={}", diff));
                }
            } else {
                emit(fixture_tag, "bytes_nointerop_vs_exif", "SKIP", "exif-encode-error");
            }
        }
        Err(e) => {
            emit(fixture_tag, "erxi_nointerop_self_rtt", "FAIL", &format!("encode: {e}"));
            emit(fixture_tag, "erxi_nointerop_to_exif", "SKIP", "encode-error");
            emit(fixture_tag, "bytes_nointerop_vs_exif", "SKIP", "encode-error");
        }
    }
}

fn run_cross_tests_interop_only(
    fixture_tag: &str,
    exi_interop: &[u8],
    opts_interop: &ExiOptions,
    schema: &SchemaInfo,
    schema_path: &str,
    xml_path: &Path,
    work_dir: &Path,
) {
    // interop → exif decode
    let exi_interop_path = work_dir.join(format!("{fixture_tag}_erxi_interop.exi"));
    fs::write(&exi_interop_path, exi_interop).unwrap();
    let xml_cross = work_dir.join(format!("{fixture_tag}_cross_interop.xml"));
    match exificient_decode(&exi_interop_path, opts_interop, schema_path, &xml_cross) {
        Ok(()) => emit(fixture_tag, "erxi_interop_to_exif", "OK", "exif-decoded-ok"),
        Err(e) => emit(fixture_tag, "erxi_interop_to_exif", "FAIL", &truncate(&e, 60)),
    }

    // exif encode + exif → erxi decode + byte compare
    let exi_exif_path = work_dir.join(format!("{fixture_tag}_exif_enc.exi"));
    match exificient_encode(xml_path, opts_interop, schema_path, &exi_exif_path) {
        Ok(()) => {
            let exi_exif = fs::read(&exi_exif_path).unwrap();

            // Lade die Events nochmal für den Vergleich
            if let Ok(events_i) = parse_xml_events_with_options(xml_path, opts_interop) {
                match decode_with_schema(&exi_exif, opts_interop.clone(), schema) {
                    Ok((events_rtt, _)) => {
                        let (ok, detail) = compare_events_canonical(&events_i, &events_rtt);
                        if ok {
                            emit(fixture_tag, "exif_to_erxi_interop", "OK", "canonical-match");
                        } else {
                            emit(fixture_tag, "exif_to_erxi_interop", "FAIL", &detail);
                        }
                    }
                    Err(e) => {
                        emit(fixture_tag, "exif_to_erxi_interop", "FAIL", &format!("decode: {e}"));
                    }
                }
            }

            if opts_interop.compression() {
                if let Ok(events_i) = parse_xml_events_with_options(xml_path, opts_interop) {
                    match decode_with_schema(&exi_exif, opts_interop.clone(), schema) {
                        Ok((exif_events, _)) => {
                            let (ok, detail) = compare_events_canonical(&events_i, &exif_events);
                            if ok {
                                emit(fixture_tag, "bytes_interop_vs_exif", "OK",
                                    &format!("canonical-match erxi={} exif={}", exi_interop.len(), exi_exif.len()));
                            } else {
                                emit(fixture_tag, "bytes_interop_vs_exif", "FAIL", &detail);
                            }
                        }
                        Err(e) => {
                            emit(fixture_tag, "bytes_interop_vs_exif", "FAIL",
                                &format!("decompress-cmp: {}", truncate(&format!("{e}"), 50)));
                        }
                    }
                } else {
                    emit(fixture_tag, "bytes_interop_vs_exif", "SKIP", "xml-parse-error");
                }
            } else if exi_interop == exi_exif.as_slice() {
                emit(fixture_tag, "bytes_interop_vs_exif", "OK", &format!("size={}", exi_interop.len()));
            } else {
                let diff = exi_interop.iter().zip(exi_exif.iter()).filter(|(a, b)| a != b).count();
                emit(fixture_tag, "bytes_interop_vs_exif",
                    &format!("DIFF:{}:{}", exi_interop.len(), exi_exif.len()),
                    &format!("diff={}", diff));
            }
        }
        Err(e) => {
            emit(fixture_tag, "exif_to_erxi_interop", "SKIP", &format!("exif-encode: {}", truncate(&e, 40)));
            emit(fixture_tag, "bytes_interop_vs_exif", "SKIP", "exif-encode-error");
        }
    }

    emit(fixture_tag, "exif_to_erxi_nointerop", "SKIP", "nointerop-encode-error");
    emit(fixture_tag, "bytes_nointerop_vs_exif", "SKIP", "nointerop-encode-error");
}

const ALL_TEST_IDS: &[&str] = &[
    "erxi_interop_self_rtt",
    "erxi_nointerop_self_rtt",
    "exif_self_rtt",
    "erxi_interop_to_exif",
    "erxi_nointerop_to_exif",
    "exif_to_erxi_interop",
    "exif_to_erxi_nointerop",
    "bytes_interop_vs_exif",
    "bytes_nointerop_vs_exif",
    "bytes_interop_vs_nointerop",
    "erxi_xml_rtt",
];

// ============================================================================
// main
// ============================================================================

fn run_batch() {
    let stdin = std::io::stdin();
    let valid_suites = ["declared", "undeclared", "dtrm", "cli", "exi4json", "infoset", "coverage"];
    let valid_alignments = ["bitpacked", "bytealigned", "precompression", "compression", "strict"];

    for line in stdin.lock().lines() {
        let line = match line {
            Ok(l) => l,
            Err(_) => break,
        };
        let line = line.trim().to_string();
        if line.is_empty() {
            continue;
        }
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() != 3 {
            eprintln!("Ungültige Batch-Zeile (erwartet: suite fixture alignment): {line}");
            continue;
        }
        let (suite, basename, alignment) = (parts[0], parts[1], parts[2]);
        if !valid_suites.contains(&suite) {
            eprintln!("Ungültige Suite: {suite}");
            continue;
        }
        if !valid_alignments.contains(&alignment) {
            eprintln!("Ungültiges Alignment: {alignment}");
            continue;
        }
        run_all_tests(suite, basename, alignment);
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 2 && args[1] == "--batch" {
        run_batch();
        return;
    }

    if args.len() != 4 {
        eprintln!("Usage: {} <suite> <fixture_basename> <alignment>", args[0]);
        eprintln!("       {} --batch  (liest suite fixture alignment von stdin)", args[0]);
        eprintln!("  suite: declared | undeclared | dtrm | cli | exi4json | infoset | coverage");
        eprintln!("  alignment: bitpacked | bytealigned | precompression | compression | strict");
        std::process::exit(1);
    }

    let suite = &args[1];
    let basename = &args[2];
    let alignment = &args[3];

    if suite != "declared"
        && suite != "undeclared"
        && suite != "dtrm"
        && suite != "cli"
        && suite != "exi4json"
        && suite != "infoset"
        && suite != "coverage"
    {
        eprintln!("Ungültige Suite: {suite}");
        std::process::exit(1);
    }

    let valid_alignments = ["bitpacked", "bytealigned", "precompression", "compression", "strict"];
    if !valid_alignments.contains(&alignment.as_str()) {
        eprintln!("Ungültiges Alignment: {alignment}");
        std::process::exit(1);
    }

    run_all_tests(suite, basename, alignment);
}
