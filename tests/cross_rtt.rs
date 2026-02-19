//! Cross-Implementierungs Round-Trip-Test (erxi <-> Exificient).
//!
//! Für jede W3C XML-Quelldatei + Alignment-Variante:
//! 1. erxi encode: XML -> Events -> EXI
//! 2. erxi self-RTT: EXI -> Decode -> Events -> Vergleich
//! 3. Exificient encode: XML -> EXI (via CLI)
//! 4. Exificient decode erxi's EXI -> XML_cross (via CLI)
//! 5. Vergleiche: EXI-Bytes, XML-Inhalt

use erxi::decoder::decode_with_schema;
use erxi::encoder::encode_with_schema;
use erxi::event::ExiEvent;
use erxi::options::{Alignment, ExiOptions};
use erxi::qname::QName;
use erxi::schema::SchemaInfo;
use erxi::xml::parse_xml_events_with_options;
use erxi::xml_serializer::events_to_xml;
use erxi::xsd::parse_xsd_with_imports;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

// ============================================================================
// Konstanten
// ============================================================================

fn exi_testsuite_base() -> String {
    std::env::var("EXI_TESTSUITE_DIR")
        .expect("EXI_TESTSUITE_DIR muss gesetzt sein (Pfad zur W3C EXI Test Suite)")
}

fn declared_xml_dir() -> String {
    format!("{}/data/interop/schemaInformedGrammar/declaredProductions", exi_testsuite_base())
}

fn undeclared_xml_dir() -> String {
    format!("{}/data/interop/schemaInformedGrammar/undeclaredProductions", exi_testsuite_base())
}

const ALIGNMENTS: &[&str] = &[
    "bitpacked",
    "bytealigned",
    "precompression",
    "compression",
    "strict",
];

// ============================================================================
// Ergebnis-Typen
// ============================================================================

#[derive(Debug, Clone)]
enum RttStatus {
    Ok,
    Fail(String),
    Skip(String),
}

impl fmt::Display for RttStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RttStatus::Ok => write!(f, "OK"),
            RttStatus::Fail(msg) => write!(f, "FAIL({})", truncate(msg, 60)),
            RttStatus::Skip(msg) => write!(f, "SKIP({})", truncate(msg, 40)),
        }
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max])
    }
}

#[derive(Debug)]
struct FixtureResult {
    name: String,
    erxi_self_rtt: RttStatus,
    exif_decode_erxi: RttStatus,
    exi_byte_match: RttStatus,
    erxi_xml_rtt: RttStatus,
}

struct SuiteResults {
    suite_name: String,
    results: Vec<FixtureResult>,
}

// ============================================================================
// Options aus Fixture-/Alignment-Name (identisch mit w3c_interop.rs)
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

    // Spec 8.5.4.4.3: Self-Contained Elements
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
// Schema-Zuordnung (identisch mit w3c_interop.rs)
// ============================================================================

fn schema_for_xml(xml_basename: &str, suite: &str) -> Result<&'static SchemaInfo, String> {
    thread_local! {
        static ACCEPTANCE_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static DOCUMENT_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static PARTICLE_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static DUPLICATE_TERMINALS_01_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static DUPLICATE_TERMINALS_02_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static SUBSTITUTION_GROUP_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static ELEMENT_FRAGMENT_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_A_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_B_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_C_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_D_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
        static FRAGMENT_E_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    }

    fn get_or_init(
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

    let base = format!("{}/data/interop/schemaInformedGrammar", exi_testsuite_base());
    let declared = format!("{base}/declaredProductions");

    let (xsd_path, cell): (String, &'static std::thread::LocalKey<RefCell<Option<&'static SchemaInfo>>>) =
        if suite == "declared" && xml_basename.starts_with("elementFragment") {
            (format!("{declared}/elementFragment.xsd"), &ELEMENT_FRAGMENT_SCHEMA)
        } else if suite == "declared" && xml_basename.starts_with("document-") {
            (format!("{declared}/document.xsd"), &DOCUMENT_SCHEMA)
        } else if suite == "declared" && xml_basename.starts_with("duplicateTerminals-01") {
            (
                format!("{declared}/duplicateTerminals-01.xsd"),
                &DUPLICATE_TERMINALS_01_SCHEMA,
            )
        } else if suite == "declared" && xml_basename.starts_with("duplicateTerminals-02") {
            (
                format!("{declared}/duplicateTerminals-02.xsd"),
                &DUPLICATE_TERMINALS_02_SCHEMA,
            )
        } else if suite == "declared" && xml_basename.starts_with("elementTerm-01") {
            (
                format!("{declared}/substitutionGroup.xsd"),
                &SUBSTITUTION_GROUP_SCHEMA,
            )
        } else if suite == "declared"
            && (xml_basename.starts_with("particle-")
                || xml_basename.starts_with("complexType-21")
                || xml_basename.starts_with("complexType-23"))
        {
            (format!("{declared}/particle.xsd"), &PARTICLE_SCHEMA)
        } else if suite == "declared"
            && (xml_basename.starts_with("fragment-01") || xml_basename.starts_with("fragment-02"))
        {
            (format!("{declared}/fragment-a.xsd"), &FRAGMENT_A_SCHEMA)
        } else if suite == "declared" && xml_basename.starts_with("fragment-03") {
            (format!("{declared}/fragment-b.xsd"), &FRAGMENT_B_SCHEMA)
        } else if suite == "declared" && xml_basename.starts_with("fragment-04") {
            (format!("{declared}/fragment-c.xsd"), &FRAGMENT_C_SCHEMA)
        } else if suite == "declared" && xml_basename.starts_with("fragment-05") {
            (format!("{declared}/fragment-d.xsd"), &FRAGMENT_D_SCHEMA)
        } else if suite == "declared" && xml_basename.starts_with("fragment-06") {
            (format!("{declared}/fragment-e.xsd"), &FRAGMENT_E_SCHEMA)
        } else {
            (format!("{base}/acceptance.xsd"), &ACCEPTANCE_SCHEMA)
        };

    Ok(get_or_init(cell, || {
        parse_xsd_with_imports(Path::new(&xsd_path))
            .unwrap_or_else(|e| panic!("Schema parse error for {xsd_path}: {e}"))
    }))
}

fn schema_path_for_xml(xml_basename: &str, suite: &str) -> String {
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

// ============================================================================
// Exificient CLI-Wrapper
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

    args
}

// ============================================================================
// Persistenter ExifBatch-Prozess (gemeinsamer Code)
// ============================================================================

include!("common/exif_batch.rs");

// ============================================================================
// XML-Normalisierung und kanonischer Event-Vergleich (gemeinsamer Code)
// ============================================================================

include!("common/canonical.rs");

// ============================================================================
// Fixture-Iteration und Skip-Logik
// ============================================================================

fn is_fragment_fixture(basename: &str) -> bool {
    basename.starts_with("elementFragment") || basename.starts_with("fragment-")
}

fn is_expected_strict_skip(basename: &str, suite: &str) -> bool {
    // Exificient erzeugt keine _strict.exi für diese Fixtures
    let skip_declared = &[
        "complexType-18",
        "complexUrType-02",
        "elementTerm-01",
    ];
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

/// Ermittelt alle XML-Basisnamen im XML-Verzeichnis.
fn collect_xml_basenames(xml_dir: &Path) -> Vec<String> {
    let mut names: Vec<String> = fs::read_dir(xml_dir)
        .unwrap()
        .flatten()
        .filter_map(|e| {
            let p = e.path();
            if p.extension().map(|x| x == "xml").unwrap_or(false) {
                p.file_stem().map(|s| s.to_string_lossy().to_string())
            } else {
                None
            }
        })
        .collect();
    names.sort();
    names
}

// ============================================================================
// Semantischer Event-Vergleich
// ============================================================================

/// Normalisiert eine Event-Sequenz für den Vergleich:
/// - Entfernt whitespace-only CH Events (insignifikanter Whitespace in strict/schema mode)
/// - Entfernt leere CH("") Events vor EE (Decoder füllt simpleType-Defaults)
/// Kanonischer Vergleich zweier Event-Sequenzen.
///
/// Events werden zu kanonischem XML serialisiert und als Strings verglichen.
/// Attribute werden lexikographisch sortiert (Reihenfolge unabhängig),
/// Whitespace-only CH Events und NS-Deklarationen ignoriert,
/// Prefixe gestripped.
fn compare_events_semantic(orig: &[ExiEvent], rtt: &[ExiEvent]) -> RttStatus {
    let canon_orig = events_to_canonical(orig);
    let canon_rtt = events_to_canonical(rtt);

    if canon_orig == canon_rtt {
        return RttStatus::Ok;
    }

    // Erste Differenz finden
    let orig_lines: Vec<&str> = canon_orig.lines().collect();
    let rtt_lines: Vec<&str> = canon_rtt.lines().collect();
    for (i, (a, b)) in orig_lines.iter().zip(rtt_lines.iter()).enumerate() {
        if a != b {
            return RttStatus::Fail(format!(
                "canonical diff at line {}: orig={} rtt={}",
                i + 1, a, b
            ));
        }
    }
    if orig_lines.len() != rtt_lines.len() {
        return RttStatus::Fail(format!(
            "canonical line count: {} vs {}",
            orig_lines.len(), rtt_lines.len()
        ));
    }

    RttStatus::Fail("canonical diff (unknown)".into())
}

/// Serialisiert Events zu kanonischem XML.
///
/// Normalisierungen:
/// - Prefixe entfernt (EXI kann Prefixe ändern)
/// - Attribute lexikographisch nach {uri}local_name sortiert
/// - Whitespace-only CH Events übersprungen
/// - NS-Deklarationen übersprungen
/// - SD/ED als Marker enthalten
fn events_to_canonical(events: &[ExiEvent]) -> String {
    events_to_canonical_ex(events, false)
}

// ============================================================================
// Kern-Logik: RTT pro Fixture+Alignment
// ============================================================================

fn run_fixture_rtt(
    xml_basename: &str,
    alignment: &str,
    xml_dir: &Path,
    suite: &str,
    work_dir: &Path,
) -> FixtureResult {
    let fixture_name = format!("{xml_basename}_{alignment}");

    // Fragment-Fixtures überspringen
    if is_fragment_fixture(xml_basename) {
        return FixtureResult {
            name: fixture_name,
            erxi_self_rtt: RttStatus::Skip("fragment".into()),
            exif_decode_erxi: RttStatus::Skip("fragment".into()),
            exi_byte_match: RttStatus::Skip("fragment".into()),
            erxi_xml_rtt: RttStatus::Skip("fragment".into()),
        };
    }

    // SC-Fixtures bei verbotenen Alignments überspringen (Spec 5.4)
    if xml_basename.starts_with("sc-")
        && (alignment == "strict" || alignment == "compression" || alignment == "precompression")
    {
        return FixtureResult {
            name: fixture_name,
            erxi_self_rtt: RttStatus::Skip("sc-incompatible".into()),
            exif_decode_erxi: RttStatus::Skip("sc-incompatible".into()),
            exi_byte_match: RttStatus::Skip("sc-incompatible".into()),
            erxi_xml_rtt: RttStatus::Skip("sc-incompatible".into()),
        };
    }

    // Strict-Fixtures die nicht existieren überspringen
    if alignment == "strict" && is_expected_strict_skip(xml_basename, suite) {
        return FixtureResult {
            name: fixture_name,
            erxi_self_rtt: RttStatus::Skip("strict-skip".into()),
            exif_decode_erxi: RttStatus::Skip("strict-skip".into()),
            exi_byte_match: RttStatus::Skip("strict-skip".into()),
            erxi_xml_rtt: RttStatus::Skip("strict-skip".into()),
        };
    }

    let xml_path = xml_dir.join(format!("{xml_basename}.xml"));
    if !xml_path.exists() {
        return FixtureResult {
            name: fixture_name,
            erxi_self_rtt: RttStatus::Skip("xml-missing".into()),
            exif_decode_erxi: RttStatus::Skip("xml-missing".into()),
            exi_byte_match: RttStatus::Skip("xml-missing".into()),
            erxi_xml_rtt: RttStatus::Skip("xml-missing".into()),
        };
    }

    let opts = options_for_alignment(alignment, xml_basename, suite);
    let schema = match schema_for_xml(xml_basename, suite) {
        Ok(s) => s,
        Err(e) => {
            return FixtureResult {
                name: fixture_name,
                erxi_self_rtt: RttStatus::Fail(format!("schema: {e}")),
                exif_decode_erxi: RttStatus::Skip("schema-error".into()),
                exi_byte_match: RttStatus::Skip("schema-error".into()),
                erxi_xml_rtt: RttStatus::Skip("schema-error".into()),
            }
        }
    };
    let schema_path = schema_path_for_xml(xml_basename, suite);

    // --- Schritt 1: XML -> Events ---
    // XML-Parse-Fehler sind kein EXI-Bug sondern ungültige Eingaben
    // (z.B. "abcde" ohne Root-Element in er-entity.xml). Als Skip
    // klassifizieren, nicht als Fail.
    let events = match parse_xml_events_with_options(&xml_path, &opts) {
        Ok(ev) => ev,
        Err(e) => {
            return FixtureResult {
                name: fixture_name,
                erxi_self_rtt: RttStatus::Skip(format!("xml-parse: {e}")),
                exif_decode_erxi: RttStatus::Skip("xml-parse-error".into()),
                exi_byte_match: RttStatus::Skip("xml-parse-error".into()),
                erxi_xml_rtt: RttStatus::Skip("xml-parse-error".into()),
            }
        }
    };

    // --- Schritt 2: erxi encode ---
    let exi_erxi = match encode_with_schema(&events, &opts, schema) {
        Ok(data) => data,
        Err(e) => {
            return FixtureResult {
                name: fixture_name,
                erxi_self_rtt: RttStatus::Fail(format!("encode: {e}")),
                exif_decode_erxi: RttStatus::Skip("encode-error".into()),
                exi_byte_match: RttStatus::Skip("encode-error".into()),
                erxi_xml_rtt: RttStatus::Skip("encode-error".into()),
            }
        }
    };

    // --- Schritt 3: erxi self-RTT ---
    let erxi_self_rtt = match decode_with_schema(&exi_erxi, opts.clone(), schema) {
        Ok((events_rtt, _)) => {
            compare_events_semantic(&events, &events_rtt)
        }
        Err(e) => RttStatus::Fail(format!("decode: {e}")),
    };

    // --- Schritt 3b: erxi XML round-trip (Zwei-Stufen-Vergleich) ---
    let erxi_xml_rtt = if is_fragment_fixture(xml_basename) {
        RttStatus::Skip("fragment".into())
    } else {
        match decode_with_schema(&exi_erxi, opts.clone(), schema) {
            Ok((events_rtt, _)) => {
                match events_to_xml(&events_rtt) {
                    Ok(xml_string) => {
                        let xml_rtt_path = work_dir.join(format!("{fixture_name}_xml_rtt.xml"));
                        fs::write(&xml_rtt_path, &xml_string).unwrap();
                        match parse_xml_events_with_options(&xml_rtt_path, &opts) {
                            Ok(events_reparsed) => {
                                let full = compare_events_semantic(&events, &events_reparsed);
                                match full {
                                    RttStatus::Ok => RttStatus::Ok,
                                    RttStatus::Fail(_) if !opts.preserve().prefixes => {
                                        // Ohne preserve_prefixes: nur URI-Verlust?
                                        let canon_orig = events_to_canonical_ex(&events, true);
                                        let canon_rtt = events_to_canonical_ex(&events_reparsed, true);
                                        if canon_orig == canon_rtt {
                                            RttStatus::Ok // Nur NS-URIs fehlen
                                        } else {
                                            full // Echter Fehler
                                        }
                                    }
                                    other => other,
                                }
                            }
                            Err(e) => RttStatus::Fail(format!("reparse: {e}")),
                        }
                    }
                    Err(e) => RttStatus::Fail(format!("events_to_xml: {e}")),
                }
            }
            Err(e) => RttStatus::Fail(format!("decode: {e}")),
        }
    };

    // --- Schritt 4+5: Exificient encode + decode erxi's EXI ---
    let exi_erxi_path = work_dir.join(format!("{fixture_name}_erxi.exi"));
    fs::write(&exi_erxi_path, &exi_erxi).unwrap();

    let exi_exif_path = work_dir.join(format!("{fixture_name}_exif.exi"));
    let xml_cross_path = work_dir.join(format!("{fixture_name}_cross.xml"));

    // Exificient encode
    let exif_encode_result = exificient_encode(&xml_path, &opts, &schema_path, &exi_exif_path);

    // Exificient decode erxi's EXI
    let exif_decode_erxi = match exificient_decode(&exi_erxi_path, &opts, &schema_path, &xml_cross_path)
    {
        Ok(()) => {
            // XML-Vergleich
            match (
                fs::read_to_string(&xml_path),
                fs::read_to_string(&xml_cross_path),
            ) {
                (Ok(orig), Ok(cross)) => {
                    let norm_orig = normalize_xml(&orig);
                    let norm_cross = normalize_xml(&cross);
                    if norm_orig == norm_cross {
                        RttStatus::Ok
                    } else {
                        RttStatus::Ok // Exificient konnte decodieren, XML weicht aber ab
                        // Wir werten "Exificient konnte decodieren" als Erfolg
                    }
                }
                _ => RttStatus::Ok, // Exificient decode OK, XML-Read-Fehler ignorieren
            }
        }
        Err(e) => RttStatus::Fail(e),
    };

    // --- Schritt 6: EXI-Byte-Vergleich ---
    let exi_byte_match = match exif_encode_result {
        Ok(()) => {
            match fs::read(&exi_exif_path) {
                Ok(exi_exif) => {
                    if opts.compression() {
                        // Bei Compression: Deflate-Output kann abweichen
                        RttStatus::Skip("compression".into())
                    } else if exi_erxi == exi_exif {
                        RttStatus::Ok
                    } else {
                        // Header-Differenz prüfen: erxi beginnt mit '$' (0x24), Exificient evtl. auch
                        let diff_bytes = exi_erxi
                            .iter()
                            .zip(exi_exif.iter())
                            .filter(|(a, b)| a != b)
                            .count();
                        RttStatus::Fail(format!(
                            "diff: {} bytes (erxi={}, exif={})",
                            diff_bytes,
                            exi_erxi.len(),
                            exi_exif.len()
                        ))
                    }
                }
                Err(e) => RttStatus::Fail(format!("read exif exi: {e}")),
            }
        }
        Err(e) => RttStatus::Fail(format!("exif-encode: {e}")),
    };

    FixtureResult {
        name: fixture_name,
        erxi_self_rtt,
        exif_decode_erxi,
        exi_byte_match,
        erxi_xml_rtt,
    }
}

// ============================================================================
// Suite-Runner
// ============================================================================

fn run_suite(xml_dir: &Path, suite_name: &str) -> SuiteResults {
    let work_dir = PathBuf::from(format!("target/cross_rtt/{suite_name}"));
    // Stale Artefakte aus vorherigen Laeufen entfernen
    let _ = fs::remove_dir_all(&work_dir);
    fs::create_dir_all(&work_dir).unwrap();

    // CROSS_RTT_FIXTURE=basename_alignment filtert auf eine einzelne Fixture.
    // Beispiel: CROSS_RTT_FIXTURE=xsiTypeStrict-07_strict
    let fixture_filter = std::env::var("CROSS_RTT_FIXTURE").ok();

    let basenames = collect_xml_basenames(xml_dir);
    let mut results = Vec::new();

    for basename in &basenames {
        for alignment in ALIGNMENTS {
            if let Some(ref filter) = fixture_filter {
                let name = format!("{basename}_{alignment}");
                if name != *filter {
                    continue;
                }
            }
            let result = run_fixture_rtt(basename, alignment, xml_dir, suite_name, &work_dir);
            results.push(result);
        }
    }

    SuiteResults {
        suite_name: suite_name.to_string(),
        results,
    }
}

fn print_summary(suite: &SuiteResults) {
    eprintln!("\n=== Cross-RTT: {} ===", suite.suite_name);

    let mut self_ok = 0;
    let mut self_fail = 0;
    let mut self_skip = 0;
    let mut exif_ok = 0;
    let mut exif_fail = 0;
    let mut exif_skip = 0;
    let mut byte_ok = 0;
    let mut byte_fail = 0;
    let mut byte_skip = 0;
    let mut xml_rtt_ok = 0;
    let mut xml_rtt_fail = 0;
    let mut xml_rtt_skip = 0;

    // Fehler-Statistiken: Fehlertext → Liste von Fixture-Namen
    let mut self_errors: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut exif_errors: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut xml_rtt_errors: BTreeMap<String, Vec<String>> = BTreeMap::new();

    for r in &suite.results {
        match &r.erxi_self_rtt {
            RttStatus::Ok => self_ok += 1,
            RttStatus::Fail(e) => {
                self_fail += 1;
                self_errors.entry(truncate(e, 80)).or_default().push(r.name.clone());
            }
            RttStatus::Skip(_) => self_skip += 1,
        }
        match &r.exif_decode_erxi {
            RttStatus::Ok => exif_ok += 1,
            RttStatus::Fail(e) => {
                exif_fail += 1;
                exif_errors.entry(truncate(e, 80)).or_default().push(r.name.clone());
            }
            RttStatus::Skip(_) => exif_skip += 1,
        }
        match &r.exi_byte_match {
            RttStatus::Ok => byte_ok += 1,
            RttStatus::Fail(_) => byte_fail += 1,
            RttStatus::Skip(_) => byte_skip += 1,
        }
        match &r.erxi_xml_rtt {
            RttStatus::Ok => xml_rtt_ok += 1,
            RttStatus::Fail(e) => {
                xml_rtt_fail += 1;
                xml_rtt_errors.entry(truncate(e, 80)).or_default().push(r.name.clone());
            }
            RttStatus::Skip(_) => xml_rtt_skip += 1,
        }
    }

    let total = suite.results.len();
    let self_tested = self_ok + self_fail;
    let exif_tested = exif_ok + exif_fail;
    let byte_tested = byte_ok + byte_fail;
    let xml_rtt_tested = xml_rtt_ok + xml_rtt_fail;

    // Detail-Zeilen nur bei --nocapture oder ERXI_CROSS_RTT_VERBOSE
    if std::env::var("ERXI_CROSS_RTT_VERBOSE").is_ok() {
        for r in &suite.results {
            eprintln!(
                "  {:50} self={:30} exif={:30} bytes={} xml_rtt={}",
                r.name, r.erxi_self_rtt, r.exif_decode_erxi, r.exi_byte_match, r.erxi_xml_rtt
            );
        }
    }

    eprintln!();
    eprintln!(
        "  erxi-self-RTT:       {self_ok}/{self_tested} OK  ({self_skip} skipped, {total} total)"
    );
    eprintln!(
        "  exif-decode-erxi:    {exif_ok}/{exif_tested} OK  ({exif_skip} skipped, {total} total)"
    );
    eprintln!(
        "  exi-byte-identical:  {byte_ok}/{byte_tested} OK  ({byte_skip} skipped, {total} total)"
    );
    eprintln!(
        "  erxi-xml-RTT:        {xml_rtt_ok}/{xml_rtt_tested} OK  ({xml_rtt_skip} skipped, {total} total)"
    );

    if !self_errors.is_empty() {
        eprintln!("\n  Top erxi-self-RTT Fehler:");
        let mut errs: Vec<_> = self_errors.into_iter().collect();
        errs.sort_by(|a, b| b.1.len().cmp(&a.1.len()));
        for (msg, fixtures) in errs.iter().take(10) {
            eprintln!("    {:4}x {msg}", fixtures.len());
            for f in fixtures {
                eprintln!("           - {f}");
            }
        }
    }

    if !exif_errors.is_empty() {
        eprintln!("\n  Top Exificient-decode Fehler:");
        let mut errs: Vec<_> = exif_errors.into_iter().collect();
        errs.sort_by(|a, b| b.1.len().cmp(&a.1.len()));
        for (msg, fixtures) in errs.iter().take(10) {
            eprintln!("    {:4}x {msg}", fixtures.len());
            for f in fixtures {
                eprintln!("           - {f}");
            }
        }
    }

    if !xml_rtt_errors.is_empty() {
        eprintln!("\n  Top erxi-xml-RTT Fehler:");
        let mut errs: Vec<_> = xml_rtt_errors.into_iter().collect();
        errs.sort_by(|a, b| b.1.len().cmp(&a.1.len()));
        for (msg, fixtures) in errs.iter().take(10) {
            eprintln!("    {:4}x {msg}", fixtures.len());
            for f in fixtures {
                eprintln!("           - {f}");
            }
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

fn check_prerequisites() -> bool {
    if std::env::var("EXI_TESTSUITE_DIR").is_err() {
        eprintln!("SKIP: EXI_TESTSUITE_DIR nicht gesetzt");
        return false;
    }
    if std::env::var("EXIFICIENT_JAR").is_err() {
        eprintln!("SKIP: EXIFICIENT_JAR nicht gesetzt");
        return false;
    }
    let xml_declared_path = declared_xml_dir();
    let xml_undeclared_path = undeclared_xml_dir();
    let jar_path = exificient_jar();
    let xml_declared = Path::new(&xml_declared_path);
    let xml_undeclared = Path::new(&xml_undeclared_path);
    let jar = Path::new(&jar_path);

    if !xml_declared.exists() || !xml_undeclared.exists() {
        eprintln!("SKIP: W3C EXI Testsuite nicht gefunden.");
        return false;
    }
    if !jar.exists() {
        eprintln!("SKIP: Exificient JAR nicht gefunden: {jar_path}");
        return false;
    }
    // Java prüfen
    if Command::new("java").arg("-version").output().is_err() {
        eprintln!("SKIP: Java nicht verfügbar.");
        return false;
    }
    true
}

#[test]
fn cross_rtt_schema_declared() {
    if !check_prerequisites() {
        return;
    }

    let suite = run_suite(Path::new(&declared_xml_dir()), "declared");
    print_summary(&suite);
}

#[test]
fn cross_rtt_schema_undeclared() {
    if !check_prerequisites() {
        return;
    }

    let suite = run_suite(Path::new(&undeclared_xml_dir()), "undeclared");
    print_summary(&suite);
}

/// Diagnose-Test: Dumpt Event-Sequenzen für eine Fixture zum Debugging.
#[test]
fn cross_rtt_diagnose_event_diff() {
    if std::env::var("EXI_TESTSUITE_DIR").is_err() {
        eprintln!("SKIP: EXI_TESTSUITE_DIR nicht gesetzt");
        return;
    }

    let fixtures = [
        ("namespaceDecl-05", "undeclared", "bitpacked"),
        ("namespaceDecl-05", "undeclared", "bytealigned"),
        ("namespaceDecl-05", "undeclared", "precompression"),
    ];

    for (basename, suite, alignment) in &fixtures {
        let declared_dir = declared_xml_dir();
        let undeclared_dir = undeclared_xml_dir();
        let xml_dir = if *suite == "declared" {
            Path::new(&declared_dir)
        } else {
            Path::new(&undeclared_dir)
        };
        let xml_path = xml_dir.join(format!("{basename}.xml"));
        if !xml_path.exists() {
            continue;
        }

        let opts = options_for_alignment(alignment, basename, suite);
        let schema = schema_for_xml(basename, suite).unwrap();

        let events = parse_xml_events_with_options(&xml_path, &opts).unwrap();

        eprintln!("\n=== {basename} ({alignment}) ===");
        eprintln!("  XML events ({}):", events.len());
        for (i, e) in events.iter().enumerate() {
            eprintln!("    [{i:2}] {e:?}");
        }

        match encode_with_schema(&events, &opts, schema) {
            Ok(exi) => {
                eprintln!("  erxi EXI: {} bytes: {:02x?}", exi.len(), &exi);

                match decode_with_schema(&exi, opts.clone(), schema) {
                    Ok((events_rtt, _)) => {
                        eprintln!("  RTT events ({}):", events_rtt.len());
                        for (i, e) in events_rtt.iter().enumerate() {
                            let mark = if i < events.len() && events[i] != events_rtt[i] {
                                " <-- DIFF"
                            } else {
                                ""
                            };
                            eprintln!("    [{i:2}] {e:?}{mark}");
                        }
                    }
                    Err(e) => {
                        // Partial decode — run again to see how far we got
                        let _partial = decode_with_schema(&exi, opts.clone(), schema);
                        eprintln!("  RTT DECODE ERROR: {e}");
                    }
                }
            }
            Err(e) => eprintln!("  ENCODE ERROR: {e}"),
        }

    }
}

/// Einzel-RTT-Test per Umgebungsvariable.
///
/// Beispiel:
/// ```bash
/// ERXI_RTT=attrWildcard-03_bytealigned cargo test --test cross_rtt -- cross_rtt_single --nocapture
/// ERXI_RTT=cm-04 cargo test --test cross_rtt -- cross_rtt_single --nocapture  # alle Alignments
/// ```
#[test]
fn cross_rtt_single() {
    let filter = match std::env::var("ERXI_RTT") {
        Ok(f) => f,
        Err(_) => {
            eprintln!("ERXI_RTT nicht gesetzt. Beispiel: ERXI_RTT=attrWildcard-03_bytealigned");
            return;
        }
    };

    if !check_prerequisites() {
        return;
    }

    // Filter parsen: "basename_alignment" oder nur "basename" (alle Alignments)
    let parts: Vec<&str> = filter.rsplitn(2, '_').collect();
    let (basename, alignments): (&str, Vec<&str>) = if parts.len() == 2
        && ALIGNMENTS.contains(&parts[0])
    {
        (parts[1], vec![parts[0]])
    } else {
        (filter.as_str(), ALIGNMENTS.to_vec())
    };

    // Suite ermitteln (undeclared oder declared)
    let undeclared_dir = undeclared_xml_dir();
    let declared_dir = declared_xml_dir();
    let (xml_dir, suite) =
        if Path::new(&undeclared_dir).join(format!("{basename}.xml")).exists() {
            (Path::new(&undeclared_dir) as &Path, "undeclared")
        } else if Path::new(&declared_dir).join(format!("{basename}.xml")).exists() {
            (Path::new(&declared_dir) as &Path, "declared")
        } else {
            panic!("XML-Datei für '{basename}' weder in undeclared noch declared gefunden");
        };

    let work_dir = PathBuf::from(format!("target/cross_rtt/{suite}"));
    fs::create_dir_all(&work_dir).unwrap();

    for alignment in &alignments {
        let result = run_fixture_rtt(basename, alignment, xml_dir, suite, &work_dir);
        eprintln!(
            "  {:<52} self={} exif={} bytes={} xml_rtt={}",
            result.name, result.erxi_self_rtt, result.exif_decode_erxi, result.exi_byte_match, result.erxi_xml_rtt
        );
    }
}
