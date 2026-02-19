//! Vollständige Cross-RTT-Matrix: Alle 10 Test-Kombinationen für ein Fixture.
//!
//! Argumente: <suite> <fixture_basename> <alignment>
//! - suite: `declared` oder `undeclared`
//! - fixture_basename: z.B. `complexType-01`
//! - alignment: `bitpacked`, `bytealigned`, `precompression`, `compression`, `strict`
//!
//! Ausgabe: TSV auf stdout (fixture\ttest_id\tresult\tdetail)

use erxi::decoder::decode_with_schema;
use erxi::encoder::encode_with_schema;
use erxi::event::ExiEvent;
use erxi::options::{Alignment, DatatypeRepresentationMapping, ExiOptions};
use erxi::qname::QName;
use erxi::schema::SchemaInfo;
use erxi::xml::parse_xml_events_with_options;
use erxi::xml_serializer::events_to_xml;
use erxi::xsd::parse_xsd_with_imports;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

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
    let mut args = vec!["-schema".to_string(), schema_path.to_string()];

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
    let valid_suites = ["declared", "undeclared", "dtrm"];
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
        eprintln!("  suite: declared | undeclared | dtrm");
        eprintln!("  alignment: bitpacked | bytealigned | precompression | compression | strict");
        std::process::exit(1);
    }

    let suite = &args[1];
    let basename = &args[2];
    let alignment = &args[3];

    if suite != "declared" && suite != "undeclared" && suite != "dtrm" {
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
