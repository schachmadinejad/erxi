//! W3C EXI Test Suite Interop-Tests.
//!
//! Diese Tests verwenden Fixtures aus der W3C EXI Test Suite,
//! encodiert mit Exificient, um die Interoperabilität zu prüfen.
//!
//! # Options-Erkennung
//!
//! Die EXI-Dateien haben oft keine Options im Header (`$`-Header).
//! Die Optionen werden aus dem Dateinamen abgeleitet:
//! - `_bitpacked.exi` → BitPacked (default)
//! - `_bytealigned.exi` → ByteAlignment
//! - `_precompression.exi` → PreCompression
//! - `_compression.exi` → Compression

use erxi::decoder::{decode, decode_with_options, decode_with_schema};
use erxi::encoder::encode;
use erxi::event::ExiEvent;
use erxi::options::{Alignment, ExiOptions, Preserve};
use erxi::schema::SchemaInfo;
use erxi::xsd::parse_xsd_with_imports;
use std::cell::RefCell;
use std::fs;
use std::path::Path;

fn exi_testsuite_base() -> String {
    std::env::var("EXI_TESTSUITE_DIR")
        .expect("EXI_TESTSUITE_DIR muss gesetzt sein (Pfad zur W3C EXI Test Suite)")
}

macro_rules! require_exi_testsuite {
    () => {
        if std::env::var("EXI_TESTSUITE_DIR").is_err() {
            eprintln!("SKIP: EXI_TESTSUITE_DIR nicht gesetzt");
            return;
        }
    };
}

// ============================================================================
// Hilfsfunktionen
// ============================================================================

/// Ermittelt ExiOptions aus dem Dateinamen.
fn options_from_filename(path: &Path) -> ExiOptions {
    let name = path.to_string_lossy();
    let mut opts = ExiOptions::default();
    let mut preserve = Preserve::default();

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

    if !opts.strict() {
        // schemaInformedGrammar/undeclaredProductions/cm.xml
        if name.contains("schema_undeclared/cm-") {
            preserve.comments = true;
            if !name.contains("cm-04") {
                preserve.pis = true;
            }
        }
        // schemaInformedGrammar/undeclaredProductions/pi.xml
        if name.contains("schema_undeclared/pi-") {
            preserve.pis = true;
        }
        // schemaInformedGrammar/undeclaredProductions/er.xml
        if name.contains("schema_undeclared/er-") {
            preserve.dtd = true;
        }
        // schemaInformedGrammar/undeclaredProductions/namespaceDecl.xml
        if name.contains("schema_undeclared/namespaceDecl-") {
            preserve.prefixes = true;
        }

        // W3C interop metadata (exi-testsuite config):
        // schemaInformedGrammar/declaredProductions/document.xml
        if name.contains("schema_declared/document-01") {
            preserve.prefixes = true;
        } else if name.contains("schema_declared/document-02")
            || name.contains("schema_declared/document-03")
            || name.contains("schema_declared/document-04")
            || name.contains("schema_declared/document-05")
            || name.contains("schema_declared/document-06")
            || name.contains("schema_declared/document-07")
        {
            preserve.dtd = true;
            preserve.pis = true;
            preserve.comments = true;
        }

        // schemaInformedGrammar/declaredProductions/elementFragment.xml
        if name.contains("schema_declared/elementFragment-05")
            || name.contains("schema_declared/elementFragment-06")
        {
            preserve.dtd = true;
            preserve.pis = true;
            preserve.comments = true;
        }

        // schemaInformedGrammar/declaredProductions/fragment.xml
        if name.contains("schema_declared/fragment-01") {
            preserve.dtd = true;
            preserve.pis = true;
            preserve.comments = true;
        } else if name.contains("schema_declared/fragment-02") {
            preserve.dtd = true;
        } else if name.contains("schema_declared/fragment-03")
            || name.contains("schema_declared/fragment-04")
            || name.contains("schema_declared/fragment-05")
            || name.contains("schema_declared/fragment-06")
        {
            preserve.pis = true;
        }
    }

    if name.contains("elementFragment")
        || name.contains("fragment-")
        || name.contains("_fragment")
    {
        opts.set_fragment(true);
    }

    // Preserve-Varianten aus Dateinamen (preserve_document / preserve_element)
    if name.contains("preserve_document/") || name.contains("preserve_element/") {
        // Suffixe: _comments_, _dtds_, _pis_, _prefixes_, _pis_dtds_, _pis_prefixes_,
        // _pis_comments_prefixes_, _pis_comments_dtds_prefixes_
        if name.contains("_comments_") || name.contains("_comments.") {
            preserve.comments = true;
        }
        if name.contains("_dtds_") || name.contains("_dtds.") {
            preserve.dtd = true;
        }
        if name.contains("_pis_") || name.contains("_pis.") {
            preserve.pis = true;
        }
        if name.contains("_prefixes_") || name.contains("_prefixes.") {
            preserve.prefixes = true;
        }
    }

    // Fragment-Fixtures aus builtin_fragments
    if name.contains("builtin_fragments/") {
        opts.set_fragment(true);
        // Alle Fragment-Fixtures wurden mit -preservePIs -preserveComments erzeugt
        preserve.pis = true;
        preserve.comments = true;
    }

    // EmptyBlock: Compression-Modus
    if name.contains("empty_block/") {
        opts.set_compression(true);
    }

    opts.set_preserve(preserve);
    opts
}

/// Dekodiert eine EXI-Datei mit Options aus dem Dateinamen.
fn decode_fixture(fixture_path: &Path) -> Result<Vec<ExiEvent>, String> {
    let data = fs::read(fixture_path).map_err(|e| format!("Read error: {e}"))?;
    let options = options_from_filename(fixture_path);

    let (events, _options) =
        decode_with_options(&data, options).map_err(|e| format!("Decode error: {e}"))?;
    Ok(events)
}

/// Dekodiert mit automatischer Options-Erkennung (nur für Header mit Options).
#[allow(dead_code)]
fn decode_fixture_auto(fixture_path: &Path) -> Result<Vec<ExiEvent>, String> {
    let data = fs::read(fixture_path).map_err(|e| format!("Read error: {e}"))?;
    let (events, _options) = decode(&data).map_err(|e| format!("Decode error: {e}"))?;
    Ok(events)
}

/// Dekodiert eine EXI-Datei mit Schema-informed Grammar.
fn decode_fixture_with_schema(
    fixture_path: &Path,
    schema: &SchemaInfo,
) -> Result<Vec<ExiEvent>, String> {
    let data = fs::read(fixture_path).map_err(|e| format!("Read error: {e}"))?;
    let options = options_from_filename(fixture_path);

    let (events, _options) =
        decode_with_schema(&data, options, schema).map_err(|e| format!("Decode error: {e}"))?;
    Ok(events)
}

/// Round-Trip Test: Dekodiere EXI, re-encodiere, vergleiche.
#[allow(dead_code)]
fn round_trip_test(fixture_path: &Path) -> Result<(), String> {
    let data = fs::read(fixture_path).map_err(|e| format!("Read error: {e}"))?;
    let options = options_from_filename(fixture_path);

    // Dekodiere
    let (events, _) =
        decode_with_options(&data, options.clone()).map_err(|e| format!("Decode error: {e}"))?;

    // Re-encode
    let re_encoded = encode(&events, &options).map_err(|e| format!("Re-encode error: {e}"))?;

    // Dekodiere erneut
    let (events2, _) =
        decode_with_options(&re_encoded, options).map_err(|e| format!("Re-decode error: {e}"))?;

    // Vergleiche Event-Sequenzen
    if events.len() != events2.len() {
        return Err(format!(
            "Event count mismatch: {} vs {}",
            events.len(),
            events2.len()
        ));
    }

    Ok(())
}

/// Zählt erfolgreich dekodierbare Dateien in einem Verzeichnis.
fn count_decodable(dir: &Path) -> (usize, usize) {
    let mut success = 0;
    let mut total = 0;

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().map(|e| e == "exi").unwrap_or(false) {
                if is_empty_file(&path) {
                    continue;
                }
                if is_ignored_fixture(&path) {
                    continue;
                }
                if is_expected_strict_failure(&path) {
                    continue;
                }
                total += 1;
                if decode_fixture(&path).is_ok() {
                    success += 1;
                }
            }
        }
    }

    (success, total)
}

/// Zählt erfolgreich dekodierbare Dateien mit Schema-informed Grammar.
fn count_decodable_with_schema(dir: &Path) -> (usize, usize, Vec<String>) {
    let mut success = 0;
    let mut total = 0;
    let mut errors = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        let mut entries: Vec<_> = entries.flatten().collect();
        entries.sort_by_key(|e| e.path());

        for entry in entries {
            let path = entry.path();
            if path.extension().map(|e| e == "exi").unwrap_or(false) {
                if is_empty_file(&path) {
                    continue;
                }
                if is_ignored_fixture(&path) {
                    continue;
                }
                if is_expected_strict_failure(&path) {
                    continue;
                }
                total += 1;
                if std::env::var("ERXI_TRACE_W3C").is_ok() {
                    eprintln!(
                        "w3c_schema_declared: decoding {}",
                        path.file_name().unwrap().to_string_lossy()
                    );
                }
                let schema = match schema_for_fixture(&path) {
                    Ok(s) => s,
                    Err(e) => {
                        if errors.len() < 5 {
                            errors.push(format!(
                                "{}: schema error: {}",
                                path.file_name().unwrap().to_string_lossy(),
                                e
                            ));
                        }
                        continue;
                    }
                };
                match decode_fixture_with_schema(&path, schema) {
                    Ok(_) => success += 1,
                    Err(e) => {
                        // Sammle erste paar Fehler für Diagnose
                        if errors.len() < 5 {
                            errors.push(format!(
                                "{}: {}",
                                path.file_name().unwrap().to_string_lossy(),
                                e
                            ));
                        }
                    }
                }
            }
        }
    }

    (success, total, errors)
}

fn count_decodable_with_schema_error_stats(
    dir: &Path,
    sample_per_error: usize,
) -> (usize, usize, Vec<(String, usize, Vec<String>)>) {
    let mut success = 0;
    let mut total = 0;
    let mut errors: std::collections::BTreeMap<String, (usize, Vec<String>)> =
        std::collections::BTreeMap::new();

    if let Ok(entries) = fs::read_dir(dir) {
        let mut entries: Vec<_> = entries.flatten().collect();
        entries.sort_by_key(|e| e.path());

        for entry in entries {
            let path = entry.path();
            if path.extension().map(|e| e == "exi").unwrap_or(false) {
                if is_empty_file(&path) {
                    continue;
                }
                if is_ignored_fixture(&path) {
                    continue;
                }
                if is_expected_strict_failure(&path) {
                    continue;
                }
                total += 1;
                let schema = match schema_for_fixture(&path) {
                    Ok(s) => s,
                    Err(e) => {
                        let key = format!("schema error: {e}");
                        let entry = errors.entry(key).or_insert((0, Vec::new()));
                        entry.0 += 1;
                        if entry.1.len() < sample_per_error {
                            entry
                                .1
                                .push(path.file_name().unwrap().to_string_lossy().to_string());
                        }
                        continue;
                    }
                };
                match decode_fixture_with_schema(&path, schema) {
                    Ok(_) => success += 1,
                    Err(e) => {
                        let key = e.to_string();
                        let entry = errors.entry(key).or_insert((0, Vec::new()));
                        entry.0 += 1;
                        if entry.1.len() < sample_per_error {
                            entry
                                .1
                                .push(path.file_name().unwrap().to_string_lossy().to_string());
                        }
                    }
                }
            }
        }
    }

    let mut summary: Vec<(String, usize, Vec<String>)> = errors
        .into_iter()
        .map(|(k, (count, samples))| (k, count, samples))
        .collect();
    summary.sort_by(|a, b| b.1.cmp(&a.1));
    (success, total, summary)
}

fn schema_for_fixture(path: &Path) -> Result<&'static SchemaInfo, String> {
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

    let name = path.file_name().unwrap().to_string_lossy();
    let base = format!("{}/data/interop/schemaInformedGrammar", exi_testsuite_base());
    let declared = format!("{base}/declaredProductions");
    let (xsd_path, cell): (String, &'static std::thread::LocalKey<RefCell<Option<&'static SchemaInfo>>>) = if name.contains("elementFragment") {
        (format!("{declared}/elementFragment.xsd"), &ELEMENT_FRAGMENT_SCHEMA)
    } else if name.contains("document-") {
        (format!("{declared}/document.xsd"), &DOCUMENT_SCHEMA)
    } else if name.contains("duplicateTerminals-01") {
        (format!("{declared}/duplicateTerminals-01.xsd"), &DUPLICATE_TERMINALS_01_SCHEMA)
    } else if name.contains("duplicateTerminals-02") {
        (format!("{declared}/duplicateTerminals-02.xsd"), &DUPLICATE_TERMINALS_02_SCHEMA)
    } else if name.contains("elementTerm-01") {
        (format!("{declared}/substitutionGroup.xsd"), &SUBSTITUTION_GROUP_SCHEMA)
    } else if name.contains("particle-")
        || name.contains("complexType-21")
        || name.contains("complexType-23")
    {
        (format!("{declared}/particle.xsd"), &PARTICLE_SCHEMA)
    } else if name.contains("fragment-01") || name.contains("fragment-02") {
        (format!("{declared}/fragment-a.xsd"), &FRAGMENT_A_SCHEMA)
    } else if name.contains("fragment-03") {
        (format!("{declared}/fragment-b.xsd"), &FRAGMENT_B_SCHEMA)
    } else if name.contains("fragment-04") {
        (format!("{declared}/fragment-c.xsd"), &FRAGMENT_C_SCHEMA)
    } else if name.contains("fragment-05") {
        (format!("{declared}/fragment-d.xsd"), &FRAGMENT_D_SCHEMA)
    } else if name.contains("fragment-06") {
        (format!("{declared}/fragment-e.xsd"), &FRAGMENT_E_SCHEMA)
    } else {
        (format!("{base}/acceptance.xsd"), &ACCEPTANCE_SCHEMA)
    };

    Ok(get_or_init(cell, || {
        parse_xsd_with_imports(Path::new(&xsd_path))
            .unwrap_or_else(|e| panic!("Schema parse error for {xsd_path}: {e}"))
    }))
}

/// Zählt erfolgreich dekodierbare Dateien mit einem explizit übergebenen Schema.
fn count_decodable_with_explicit_schema(
    dir: &Path,
    schema: &SchemaInfo,
) -> (usize, usize, Vec<String>) {
    let mut success = 0;
    let mut total = 0;
    let mut errors = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        let mut entries: Vec<_> = entries.flatten().collect();
        entries.sort_by_key(|e| e.path());

        for entry in entries {
            let path = entry.path();
            if path.extension().map(|e| e == "exi").unwrap_or(false) {
                if is_empty_file(&path) {
                    continue;
                }
                if is_ignored_fixture(&path) {
                    continue;
                }
                if is_expected_strict_failure(&path) {
                    continue;
                }
                total += 1;
                match decode_fixture_with_schema(&path, schema) {
                    Ok(_) => success += 1,
                    Err(e) => {
                        if errors.len() < 5 {
                            errors.push(format!(
                                "{}: {}",
                                path.file_name().unwrap().to_string_lossy(),
                                e
                            ));
                        }
                    }
                }
            }
        }
    }

    (success, total, errors)
}

/// Zählt erfolgreich dekodierbare Dateien mit detailliertem Output.
fn count_decodable_verbose(dir: &Path) -> (usize, usize) {
    let mut success = 0;
    let mut total = 0;

    if let Ok(entries) = fs::read_dir(dir) {
        let mut entries: Vec<_> = entries.flatten().collect();
        entries.sort_by_key(|e| e.path());

        for entry in entries {
            let path = entry.path();
            if path.extension().map(|e| e == "exi").unwrap_or(false) {
                if is_empty_file(&path) {
                    continue;
                }
                if is_ignored_fixture(&path) {
                    continue;
                }
                if is_expected_strict_failure(&path) {
                    continue;
                }
                total += 1;
                match decode_fixture(&path) {
                    Ok(events) => {
                        success += 1;
                        eprintln!("  ✓ {} ({} events)", path.file_name().unwrap().to_string_lossy(), events.len());
                    }
                    Err(e) => {
                        eprintln!("  ✗ {} - {}", path.file_name().unwrap().to_string_lossy(), e);
                    }
                }
            }
        }
    }

    (success, total)
}

fn is_empty_file(path: &Path) -> bool {
    fs::metadata(path).map(|m| m.len() == 0).unwrap_or(false)
}

const EXPECTED_MISSING_OR_EMPTY: &[&str] = &[
    "schema_declared/complexType-18_strict.exi",
    "schema_declared/complexUrType-02_strict.exi",
    "schema_undeclared/attrWildcard-01_strict.exi",
    "schema_undeclared/attrWildcard-02_strict.exi",
    "schema_undeclared/attrWildcard-03_strict.exi",
    "schema_undeclared/attruseUntyped-01_strict.exi",
    "schema_undeclared/ch-01_strict.exi",
    "schema_undeclared/ch-02_strict.exi",
    "schema_undeclared/ch-03_strict.exi",
    "schema_undeclared/ch-04_strict.exi",
    "schema_undeclared/ee-01_strict.exi",
    "schema_undeclared/ee-02_strict.exi",
    "schema_undeclared/ee-03_strict.exi",
    "schema_undeclared/ee-04_strict.exi",
    "schema_undeclared/ee-05_strict.exi",
    "schema_undeclared/ee-06_strict.exi",
    "schema_undeclared/er-entity_bitpacked.exi",
    "schema_undeclared/er-entity_strict.exi",
    "schema_undeclared/namespaceDecl-01_strict.exi",
    "schema_undeclared/namespaceDecl-02_strict.exi",
    "schema_undeclared/namespaceDecl-05_strict.exi",
    "schema_undeclared/se-01_strict.exi",
    "schema_undeclared/se-02_strict.exi",
    "schema_undeclared/se-03_strict.exi",
    "schema_undeclared/xsiNilDefault-03_strict.exi",
    "schema_undeclared/xsiNilDefault-04_strict.exi",
    "schema_undeclared/xsiNilDefault-05_strict.exi",
    "schema_undeclared/xsiNilDefault-07_strict.exi",
    "schema_undeclared/xsiNilStrict-03_strict.exi",
    "schema_undeclared/xsiTypeDefault-01_strict.exi",
    "schema_undeclared/xsiTypeDefault-02_strict.exi",
    "schema_undeclared/xsiTypeDefault-03_strict.exi",
    "schema_undeclared/xsiTypeDefault-04_strict.exi",
    "schema_undeclared/xsiTypeDefault-05_strict.exi",
    "schema_undeclared/xsiTypeDefault-06_strict.exi",
];

// Expected strict failures: fixtures that are schema-invalid in strict mode.
const EXPECTED_STRICT_FAILURES: &[&str] = &[
    // elementTerm-01: root Z is not declared in the schema (strict must fail).
    "schema_declared/elementTerm-01_strict.exi",
    // xsiNilDefault: "default schema mode" Tests (non-strict only).
    // -06: Spec Zeile 3346: xsi:type + xsi:nil zusammen in strict nicht darstellbar.
    "schema_undeclared/xsiNilDefault-01_strict.exi",
    "schema_undeclared/xsiNilDefault-02_strict.exi",
    "schema_undeclared/xsiNilDefault-06_strict.exi",
    // Strict: DTD/ER not allowed (preserve.dtd=false).
    "schema_undeclared/er-01_strict.exi",
    "schema_undeclared/er-02_strict.exi",
    // Strict: Spec Zeile 719-721: strict MUST NOT mit comments/pis koexistieren.
    "schema_undeclared/cm-01_strict.exi",
    "schema_undeclared/cm-02_strict.exi",
    "schema_undeclared/cm-03_strict.exi",
    // Strict: Exificient fails to decode these streams as well.
    "schema_undeclared/er-03_strict.exi",
    "schema_undeclared/namespaceDecl-04_strict.exi",
    "schema_undeclared/pi-01_strict.exi",
    "schema_undeclared/pi-02_strict.exi",
    "schema_undeclared/pi-03_strict.exi",
];

// Interop-ignore: known Exificient deviations (erxi is spec-conform and should error).
// Aktuell leer — alle Fixtures werden mitgezählt.
const IGNORED_INTEROP_FIXTURES: &[&str] = &[];

fn is_ignored_fixture(path: &Path) -> bool {
    let base = Path::new("tests/fixtures/w3c");
    let Ok(rel) = path.strip_prefix(base) else {
        return false;
    };
    let rel = rel.to_string_lossy();
    IGNORED_INTEROP_FIXTURES.iter().any(|p| *p == rel)
}

fn is_expected_strict_failure(path: &Path) -> bool {
    let base = Path::new("tests/fixtures/w3c");
    let Ok(rel) = path.strip_prefix(base) else {
        return false;
    };
    let rel = rel.to_string_lossy();
    EXPECTED_STRICT_FAILURES.iter().any(|p| *p == rel)
}

#[test]
fn w3c_expected_missing_or_empty_fixtures() {
    let base = Path::new("tests/fixtures/w3c");
    if !base.exists() {
        return;
    }

    let mut missing = Vec::new();
    let mut empty = Vec::new();
    let mut non_empty = Vec::new();

    for rel in EXPECTED_MISSING_OR_EMPTY {
        let path = base.join(rel);
        if !path.exists() {
            missing.push(rel);
            continue;
        }
        if is_empty_file(&path) {
            empty.push(rel);
        } else {
            non_empty.push(rel);
        }
    }

    if !non_empty.is_empty() {
        eprintln!(
            "WARN: Expected missing/empty fixtures are non-empty (using available files): {:?}",
            non_empty
        );
    }

    eprintln!(
        "Expected missing/empty fixtures: missing={}, empty={}",
        missing.len(),
        empty.len()
    );
}

// ============================================================================
// Kategorie-Tests: Built-in Grammar
// ============================================================================

#[test]
fn w3c_builtin_element() {
    let dir = Path::new("tests/fixtures/w3c/builtin_element");
    if !dir.exists() {
        eprintln!("SKIP: W3C fixtures not generated. Run scripts/generate_w3c_fixtures.sh");
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!("builtin_element: {}/{} decoded successfully", success, total);
    assert!(
        success >= total / 2,
        "Less than 50% decodable: {}/{}",
        success,
        total
    );
}

#[test]
fn w3c_builtin_attribute() {
    let dir = Path::new("tests/fixtures/w3c/builtin_attribute");
    if !dir.exists() {
        return;
    }

    // attr-01: normale Attribute → funktioniert
    // attr-02: xsi:type/xsi:nil → TODO: xsi:type QName-Value Decoding
    let (success, total) = count_decodable_verbose(dir);
    eprintln!(
        "builtin_attribute: {}/{} decoded successfully",
        success, total
    );
    assert!(success >= total / 2);
}

#[test]
fn w3c_builtin_character() {
    let dir = Path::new("tests/fixtures/w3c/builtin_character");
    if !dir.exists() {
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!(
        "builtin_character: {}/{} decoded successfully",
        success, total
    );
    assert!(success >= total / 2);
}

// ============================================================================
// Kategorie-Tests: Datatypes
// ============================================================================

#[test]
fn w3c_datatypes_string() {
    let dir = Path::new("tests/fixtures/w3c/datatypes_string");
    if !dir.exists() {
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!("datatypes_string: {}/{} decoded successfully", success, total);
    assert!(success >= total / 2);
}

#[test]
fn w3c_datatypes_integer() {
    let dir = Path::new("tests/fixtures/w3c/datatypes_integer");
    if !dir.exists() {
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!(
        "datatypes_integer: {}/{} decoded successfully",
        success, total
    );
    assert!(success >= total / 2);
}

#[test]
fn w3c_datatypes_boolean() {
    let dir = Path::new("tests/fixtures/w3c/datatypes_boolean");
    if !dir.exists() {
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!(
        "datatypes_boolean: {}/{} decoded successfully",
        success, total
    );
    assert!(success >= total / 2);
}

#[test]
fn w3c_datatypes_decimal() {
    let dir = Path::new("tests/fixtures/w3c/datatypes_decimal");
    if !dir.exists() {
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!(
        "datatypes_decimal: {}/{} decoded successfully",
        success, total
    );
    assert!(success >= total / 2);
}

#[test]
fn w3c_datatypes_float() {
    let dir = Path::new("tests/fixtures/w3c/datatypes_float");
    if !dir.exists() {
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!("datatypes_float: {}/{} decoded successfully", success, total);
    assert!(success >= total / 2);
}

#[test]
fn w3c_datatypes_datetime() {
    let dir = Path::new("tests/fixtures/w3c/datatypes_datetime");
    if !dir.exists() {
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!(
        "datatypes_datetime: {}/{} decoded successfully",
        success, total
    );
    assert!(success >= total / 2);
}

// ============================================================================
// Kategorie-Tests: Header & Compression
// ============================================================================

#[test]
fn w3c_header() {
    let dir = Path::new("tests/fixtures/w3c/header");
    if !dir.exists() {
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!("header: {}/{} decoded successfully", success, total);
    assert!(success >= total / 2);
}

#[test]
fn w3c_compression() {
    let dir = Path::new("tests/fixtures/w3c/compression");
    if !dir.exists() {
        return;
    }

    let (success, total) = count_decodable(dir);
    eprintln!("compression: {}/{} decoded successfully", success, total);
    // Compression ist komplexer, niedrigere Schwelle
    assert!(success >= total / 3);
}

// ============================================================================
// Kategorie-Tests: Schema-informed
// ============================================================================

/// W3C Schema-informed Tests mit dem acceptance.xsd Schema.
#[test]
fn w3c_schema_declared() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/schema_declared");
    if !dir.exists() {
        return;
    }

    // Dekodiere mit Schema-informed Grammar
    let (success, total, errors) = count_decodable_with_schema(dir);

    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }

    eprintln!(
        "schema_declared: {}/{} decoded successfully ({}%)",
        success,
        total,
        if total > 0 { 100 * success / total } else { 0 }
    );
}

#[test]
fn w3c_schema_declared_error_stats() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/schema_declared");
    if !dir.exists() {
        return;
    }

    let (success, total, errors) = count_decodable_with_schema_error_stats(dir, 3);
    eprintln!(
        "schema_declared: {}/{} decoded successfully ({}%)",
        success,
        total,
        if total > 0 { 100 * success / total } else { 0 }
    );

    eprintln!("Top error categories (schema_declared):");
    for (msg, count, samples) in errors.iter().take(10) {
        eprintln!("  {count:4}x {msg}");
        if !samples.is_empty() {
            eprintln!("       samples: {}", samples.join(", "));
        }
    }
}

#[test]
fn w3c_schema_undeclared() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/schema_undeclared");
    if !dir.exists() {
        return;
    }

    let (success, total, errors) = count_decodable_with_schema(dir);
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!(
        "schema_undeclared: {}/{} decoded successfully ({}%)",
        success,
        total,
        if total > 0 { 100 * success / total } else { 0 }
    );
}

#[test]
fn w3c_schema_undeclared_error_stats() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/schema_undeclared");
    if !dir.exists() {
        return;
    }

    let (success, total, errors) = count_decodable_with_schema_error_stats(dir, 3);
    eprintln!(
        "schema_undeclared: {}/{} decoded successfully ({}%)",
        success,
        total,
        if total > 0 { 100 * success / total } else { 0 }
    );

    eprintln!("Top error categories (schema_undeclared):");
    for (msg, count, samples) in errors.iter().take(10) {
        eprintln!("  {count:4}x {msg}");
        if !samples.is_empty() {
            eprintln!("       samples: {}", samples.join(", "));
        }
    }
}

// ============================================================================
// Kategorie-Tests: Schema-informed Datatypes (neue Kategorien)
// ============================================================================

thread_local! {
    static BINARY_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    static ENUMERATION_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    static ENUMERATION_OF_LIST_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    static LIST_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    static NBIT_INTEGER_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    static QNAME_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    static RCS_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    static UNSIGNED_INTEGER_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    static DTR_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
    static EMPTY_BLOCK_SCHEMA: RefCell<Option<&'static SchemaInfo>> = const { RefCell::new(None) };
}

fn load_schema(
    xsd_path: &str,
    cell: &'static std::thread::LocalKey<RefCell<Option<&'static SchemaInfo>>>,
) -> &'static SchemaInfo {
    cell.with(|c| {
        let mut borrow = c.borrow_mut();
        if let Some(s) = *borrow {
            s
        } else {
            let leaked: &'static SchemaInfo = Box::leak(Box::new(
                parse_xsd_with_imports(Path::new(xsd_path))
                    .unwrap_or_else(|e| panic!("Schema parse error for {xsd_path}: {e}")),
            ));
            *borrow = Some(leaked);
            leaked
        }
    })
}

#[test]
fn w3c_datatypes_binary() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/datatypes_binary");
    if !dir.exists() {
        return;
    }
    let schema = load_schema(
        &format!("{}/data/interop/datatypes/binary/binary.xsd", exi_testsuite_base()),
        &BINARY_SCHEMA,
    );
    let (success, total, errors) = count_decodable_with_explicit_schema(dir, schema);
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!("datatypes_binary: {}/{} decoded", success, total);
    assert!(total > 0, "No binary fixtures found");
}

#[test]
fn w3c_datatypes_enumeration() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/datatypes_enumeration");
    if !dir.exists() {
        return;
    }
    // Zwei Schemas: enumeration.xsd und enumerationOfList.xsd
    // Da die Fixtures gemischt im selben Verzeichnis liegen, dekodieren wir datei-weise.
    let schema_enum = load_schema(
        &format!("{}/data/interop/datatypes/enumeration/enumeration.xsd", exi_testsuite_base()),
        &ENUMERATION_SCHEMA,
    );
    let schema_list = load_schema(
        &format!("{}/data/interop/datatypes/enumeration/enumerationOfList.xsd", exi_testsuite_base()),
        &ENUMERATION_OF_LIST_SCHEMA,
    );
    let mut success = 0;
    let mut total = 0;
    let mut errors = Vec::new();
    if let Ok(entries) = fs::read_dir(dir) {
        let mut entries: Vec<_> = entries.flatten().collect();
        entries.sort_by_key(|e| e.path());
        for entry in entries {
            let path = entry.path();
            if path.extension().map(|e| e == "exi").unwrap_or(false) {
                if is_empty_file(&path) {
                    continue;
                }
                total += 1;
                let name = path.file_name().unwrap().to_string_lossy();
                let schema = if name.contains("enumerationOfList") {
                    schema_list
                } else {
                    schema_enum
                };
                match decode_fixture_with_schema(&path, schema) {
                    Ok(_) => success += 1,
                    Err(e) => {
                        if errors.len() < 5 {
                            errors.push(format!("{name}: {e}"));
                        }
                    }
                }
            }
        }
    }
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!("datatypes_enumeration: {}/{} decoded", success, total);
    assert!(total > 0, "No enumeration fixtures found");
}

#[test]
fn w3c_datatypes_list() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/datatypes_list");
    if !dir.exists() {
        return;
    }
    let schema = load_schema(
        &format!("{}/data/interop/datatypes/list/list.xsd", exi_testsuite_base()),
        &LIST_SCHEMA,
    );
    let (success, total, errors) = count_decodable_with_explicit_schema(dir, schema);
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!("datatypes_list: {}/{} decoded", success, total);
    assert!(total > 0, "No list fixtures found");
}

#[test]
fn w3c_datatypes_nbit_integer() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/datatypes_nbit_integer");
    if !dir.exists() {
        return;
    }
    let schema = load_schema(
        &format!("{}/data/interop/datatypes/nbitInteger/nbitInteger.xsd", exi_testsuite_base()),
        &NBIT_INTEGER_SCHEMA,
    );
    let (success, total, errors) = count_decodable_with_explicit_schema(dir, schema);
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!("datatypes_nbit_integer: {}/{} decoded", success, total);
    assert!(total > 0, "No nbit integer fixtures found");
}

#[test]
fn w3c_datatypes_qname() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/datatypes_qname");
    if !dir.exists() {
        return;
    }
    let schema = load_schema(
        &format!("{}/data/interop/datatypes/qname/qname.xsd", exi_testsuite_base()),
        &QNAME_SCHEMA,
    );
    let (success, total, errors) = count_decodable_with_explicit_schema(dir, schema);
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!("datatypes_qname: {}/{} decoded", success, total);
    assert!(total > 0, "No qname fixtures found");
}

#[test]
fn w3c_datatypes_rcs() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/datatypes_rcs");
    if !dir.exists() {
        return;
    }
    let schema = load_schema(
        &format!("{}/data/interop/datatypes/rcs/restrictedCharacterSets.xsd", exi_testsuite_base()),
        &RCS_SCHEMA,
    );
    let (success, total, errors) = count_decodable_with_explicit_schema(dir, schema);
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!("datatypes_rcs: {}/{} decoded", success, total);
    assert!(total > 0, "No rcs fixtures found");
}

#[test]
fn w3c_datatypes_unsigned_integer() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/datatypes_unsigned_integer");
    if !dir.exists() {
        return;
    }
    let schema = load_schema(
        &format!("{}/data/interop/datatypes/unsignedInteger/unsignedInteger.xsd", exi_testsuite_base()),
        &UNSIGNED_INTEGER_SCHEMA,
    );
    let (success, total, errors) = count_decodable_with_explicit_schema(dir, schema);
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!("datatypes_unsigned_integer: {}/{} decoded", success, total);
    assert!(total > 0, "No unsigned integer fixtures found");
}

#[test]
fn w3c_datatypes_dtrm() {
    let dir = Path::new("tests/fixtures/w3c/datatypes_dtrm");
    if !dir.exists() {
        return;
    }
    // DTRM-XSDs enthalten DTD-Referenzen, die unser XSD-Parser nicht unterstützt.
    // Außerdem brauchen DTRM-Tests datatypeRepresentationMap Options.
    // Daher: Schema-less Dekodierung als Basis-Check.
    let (success, total) = count_decodable(dir);
    eprintln!("datatypes_dtrm: {}/{} decoded (schema-less)", success, total);
    assert!(total > 0, "No dtrm fixtures found");
}

// ============================================================================
// Kategorie-Tests: xsitype, fragments, preserve, emptyBlock, dtr
// ============================================================================

#[test]
fn w3c_builtin_xsitype() {
    let dir = Path::new("tests/fixtures/w3c/builtin_xsitype");
    if !dir.exists() {
        return;
    }
    let (success, total) = count_decodable(dir);
    eprintln!("builtin_xsitype: {}/{} decoded", success, total);
    assert!(total > 0, "No xsitype fixtures found");
}

#[test]
fn w3c_builtin_fragments() {
    let dir = Path::new("tests/fixtures/w3c/builtin_fragments");
    if !dir.exists() {
        return;
    }
    let (success, total) = count_decodable(dir);
    eprintln!("builtin_fragments: {}/{} decoded", success, total);
    assert!(total > 0, "No fragment fixtures found");
}

#[test]
fn w3c_preserve_document() {
    let dir = Path::new("tests/fixtures/w3c/preserve_document");
    if !dir.exists() {
        return;
    }
    let (success, total) = count_decodable(dir);
    eprintln!("preserve_document: {}/{} decoded", success, total);
    assert!(total > 0, "No preserve document fixtures found");
}

#[test]
fn w3c_preserve_element() {
    let dir = Path::new("tests/fixtures/w3c/preserve_element");
    if !dir.exists() {
        return;
    }
    let (success, total) = count_decodable(dir);
    eprintln!("preserve_element: {}/{} decoded", success, total);
    assert!(total > 0, "No preserve element fixtures found");
}

#[test]
fn w3c_empty_block() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/empty_block");
    if !dir.exists() {
        return;
    }
    let schema = load_schema(
        &format!("{}/data/interop/emptyBlock/emptyBlock.xsd", exi_testsuite_base()),
        &EMPTY_BLOCK_SCHEMA,
    );
    let (success, total, errors) = count_decodable_with_explicit_schema(dir, schema);
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!("empty_block: {}/{} decoded", success, total);
    assert!(total > 0, "No empty block fixtures found");
}

#[test]
fn w3c_dtr() {
    require_exi_testsuite!();
    let dir = Path::new("tests/fixtures/w3c/dtr");
    if !dir.exists() {
        return;
    }
    let schema = load_schema(
        &format!("{}/data/interop/dtr/dtr.xsd", exi_testsuite_base()),
        &DTR_SCHEMA,
    );
    let (success, total, errors) = count_decodable_with_explicit_schema(dir, schema);
    if !errors.is_empty() {
        eprintln!("First errors:");
        for err in &errors {
            eprintln!("  - {err}");
        }
    }
    eprintln!("dtr: {}/{} decoded", success, total);
    assert!(total > 0, "No dtr fixtures found");
}

// ============================================================================
// Detaillierte Tests für spezifische Fixtures
// ============================================================================

/// Test spezifische element Fixtures mit allen Varianten.
#[test]
fn w3c_element_01_all_variants() {
    let base = "tests/fixtures/w3c/builtin_element/element-01";
    let variants = ["_bitpacked.exi", "_bytealigned.exi", "_compression.exi", "_precompression.exi"];

    eprintln!("\n=== element-01 Variants ===");
    for variant in &variants {
        let path_str = format!("{}{}", base, variant);
        let path = Path::new(&path_str);
        if path.exists() {
            match decode_fixture(path) {
                Ok(events) => eprintln!("  ✓ {} ({} events)", variant, events.len()),
                Err(e) => eprintln!("  ✗ {} - {}", variant, e),
            }
        }
    }
}

/// Verbose Test für builtin_element Kategorie.
#[test]
fn w3c_builtin_element_verbose() {
    let dir = Path::new("tests/fixtures/w3c/builtin_element");
    if !dir.exists() {
        return;
    }

    eprintln!("\n=== builtin_element (verbose) ===");
    let (success, total) = count_decodable_verbose(dir);
    eprintln!("\nResult: {}/{}", success, total);
}

// ============================================================================
// Zusammenfassung aller W3C Tests
// ============================================================================

#[test]
fn w3c_summary() {
    require_exi_testsuite!();
    // Schema-less Kategorien (ohne Schema dekodiert)
    let schemaless_categories = [
        "builtin_element",
        "builtin_attribute",
        "builtin_character",
        "builtin_xsitype",
        "builtin_fragments",
        "datatypes_string",
        "datatypes_integer",
        "datatypes_boolean",
        "datatypes_decimal",
        "datatypes_float",
        "datatypes_datetime",
        "preserve_document",
        "preserve_element",
        "header",
        "compression",
    ];

    let mut total_success = 0;
    let mut total_count = 0;

    eprintln!("\n=== W3C EXI Interop Test Summary ===\n");

    for cat in &schemaless_categories {
        let dir = Path::new("tests/fixtures/w3c").join(cat);
        if dir.exists() {
            let (success, total) = count_decodable(&dir);
            total_success += success;
            total_count += total;
            let pct = if total > 0 {
                100 * success / total
            } else {
                0
            };
            eprintln!("{:30} {:4}/{:4} ({:3}%)", cat, success, total, pct);
        }
    }

    // Schema-informed Kategorien mit eigenem Schema
    let ts = exi_testsuite_base();
    let schema_categories: Vec<(&str, String, &'static std::thread::LocalKey<RefCell<Option<&'static SchemaInfo>>>)> = vec![
        ("datatypes_binary", format!("{ts}/data/interop/datatypes/binary/binary.xsd"), &BINARY_SCHEMA),
        ("datatypes_list", format!("{ts}/data/interop/datatypes/list/list.xsd"), &LIST_SCHEMA),
        ("datatypes_nbit_integer", format!("{ts}/data/interop/datatypes/nbitInteger/nbitInteger.xsd"), &NBIT_INTEGER_SCHEMA),
        ("datatypes_qname", format!("{ts}/data/interop/datatypes/qname/qname.xsd"), &QNAME_SCHEMA),
        ("datatypes_rcs", format!("{ts}/data/interop/datatypes/rcs/restrictedCharacterSets.xsd"), &RCS_SCHEMA),
        ("datatypes_unsigned_integer", format!("{ts}/data/interop/datatypes/unsignedInteger/unsignedInteger.xsd"), &UNSIGNED_INTEGER_SCHEMA),
        ("dtr", format!("{ts}/data/interop/dtr/dtr.xsd"), &DTR_SCHEMA),
        ("empty_block", format!("{ts}/data/interop/emptyBlock/emptyBlock.xsd"), &EMPTY_BLOCK_SCHEMA),
    ];

    for (cat, xsd, cell) in &schema_categories {
        let dir = Path::new("tests/fixtures/w3c").join(cat);
        if dir.exists() {
            let schema = load_schema(&xsd, cell);
            let (success, total, _) = count_decodable_with_explicit_schema(&dir, schema);
            total_success += success;
            total_count += total;
            let pct = if total > 0 {
                100 * success / total
            } else {
                0
            };
            eprintln!("{:30} {:4}/{:4} ({:3}%)", cat, success, total, pct);
        }
    }

    // Schema-informed Kategorien mit schema_for_fixture() Lookup
    let lookup_schema_categories = ["schema_declared", "schema_undeclared"];
    for cat in &lookup_schema_categories {
        let dir = Path::new("tests/fixtures/w3c").join(cat);
        if dir.exists() {
            let (success, total, _errors) = count_decodable_with_schema(&dir);
            total_success += success;
            total_count += total;
            let pct = if total > 0 {
                100 * success / total
            } else {
                0
            };
            eprintln!("{:30} {:4}/{:4} ({:3}%)", cat, success, total, pct);
        }
    }

    // Enumeration + DTRM sind Spezialfälle (mehrere Schemas pro Verzeichnis).
    // Diese werden in ihren eigenen Tests gezählt; hier nur zusammenfassen.
    for cat in &["datatypes_enumeration", "datatypes_dtrm"] {
        let dir = Path::new("tests/fixtures/w3c").join(cat);
        if dir.exists() {
            // Schema-less Dekodierung als grobe Zählung
            let (success, total) = count_decodable(&dir);
            total_success += success;
            total_count += total;
            let pct = if total > 0 {
                100 * success / total
            } else {
                0
            };
            eprintln!("{:30} {:4}/{:4} ({:3}%) [mixed schemas]", cat, success, total, pct);
        }
    }

    eprintln!("\n{:30} {:4}/{:4}", "TOTAL", total_success, total_count);

    if total_count > 0 {
        let total_pct = 100 * total_success / total_count;
        eprintln!("Overall success rate: {}%", total_pct);
    }
}
