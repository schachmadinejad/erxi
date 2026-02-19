use erxi::options::{Alignment, ExiOptions};
use serde_json::Value;
use std::cell::RefCell;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

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
    args
}

include!("common/exif_batch.rs");

fn ensure_exificient_env() -> bool {
    if Command::new("java").arg("-version").output().is_err() {
        eprintln!("SKIP: Java nicht verfuegbar");
        return false;
    }

    let jar_dir = Path::new("tests/fixtures/exificient");
    let jars = [
        jar_dir.join("exificient-1.0.4.jar"),
        jar_dir.join("exificient-core-1.0.4.jar"),
        jar_dir.join("exificient-grammars-1.0.4.jar"),
        jar_dir.join("xercesImpl-2.12.0.jar"),
    ];
    if jars.iter().any(|p| !p.exists()) {
        eprintln!("SKIP: Exificient JARs fehlen in tests/fixtures/exificient");
        return false;
    }

    let cp = jars
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect::<Vec<_>>()
        .join(":");
    unsafe {
        std::env::set_var("EXIFICIENT_JAR", cp);
    }

    let exifbatch_java = Path::new("tools/ExifBatch.java");
    let exifbatch_class = Path::new("tools/ExifBatch.class");
    if !exifbatch_class.exists() {
        let cp = std::env::var("EXIFICIENT_JAR").expect("EXIFICIENT_JAR missing");
        let status = Command::new("javac")
            .args(["-cp", &cp, exifbatch_java.to_str().expect("java path")])
            .status();
        match status {
            Ok(s) if s.success() => {}
            _ => {
                eprintln!("SKIP: javac fehlgeschlagen fuer tools/ExifBatch.java");
                return false;
            }
        }
    }

    true
}

fn test_temp_dir(tag: &str) -> PathBuf {
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock before epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("erxi-exi4json-cross-{tag}-{}-{ts}", std::process::id()));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn fixture_paths() -> Vec<PathBuf> {
    let dir = Path::new("tests/fixtures/json");
    let mut entries: Vec<_> = fs::read_dir(dir)
        .expect("fixtures dir")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("json"))
        .collect();
    entries.sort();
    entries
}

fn values_equivalent(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Number(x), Value::Number(y)) => {
            match (x.as_f64(), y.as_f64()) {
                (Some(xf), Some(yf)) => (xf - yf).abs() <= f64::EPSILON * xf.abs().max(1.0),
                _ => false,
            }
        }
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
        .replace('"', "&quot;")
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
    let doc = roxmltree::Document::parse(xml)
        .map_err(|e| format!("parse xml: {e}"))?;
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

#[test]
fn cross_exi4json_erxi_encode_exificient_decode() {
    if !ensure_exificient_env() {
        return;
    }
    let schema_path = Path::new("src/exi4json.xsd");
    let opts = ExiOptions::default().with_strict();

    for path in fixture_paths() {
        let json = fs::read_to_string(&path).expect("read json");
        let value: Value = serde_json::from_str(&json).expect("parse json");
        let xml = json_to_xml(&value);

        let dir = test_temp_dir("erxi-to-exif");
        let exi_path = dir.join("out.exi");
        let exif_decoded = dir.join("exif-decoded.xml");

        let exi = erxi::encode_json(&json).expect("encode_json");
        fs::write(&exi_path, exi).expect("write exi");

        exificient_decode(
            &exi_path,
            &opts,
            schema_path.to_str().unwrap(),
            &exif_decoded,
        )
        .expect("exificient decode failed");

        let xml_out = fs::read_to_string(exif_decoded).expect("read decoded");
        let value_out = match xml_to_json(&xml_out) {
            Ok(v) => v,
            Err(e) => {
                panic!(
                    "xml_to_json failed: {e}\noutput size: {}\noutput: {}",
                    xml_out.len(),
                    xml_out.chars().take(200).collect::<String>()
                );
            }
        };

        assert!(values_equivalent(&value, &value_out), "mismatch for {:?}", path);
        // sanity: ensure mapping xml parse is consistent
        let value_xml = xml_to_json(&xml)
            .expect("xml mapping parse failed");
        assert!(values_equivalent(&value, &value_xml), "xml mapping mismatch for {:?}", path);
    }
}

#[test]
fn cross_exi4json_exificient_encode_erxi_decode() {
    if !ensure_exificient_env() {
        return;
    }
    let schema_path = Path::new("src/exi4json.xsd");
    let opts = ExiOptions::default().with_strict();

    for path in fixture_paths() {
        let json = fs::read_to_string(&path).expect("read json");
        let value: Value = serde_json::from_str(&json).expect("parse json");
        let xml = json_to_xml(&value);

        let dir = test_temp_dir("exif-to-erxi");
        let xml_path = dir.join("in.xml");
        let exi_path = dir.join("exif.exi");

        fs::write(&xml_path, xml).expect("write xml");

        exificient_encode(
            &xml_path,
            &opts,
            schema_path.to_str().unwrap(),
            &exi_path,
        )
        .expect("exificient encode failed");

        let exi = fs::read(&exi_path).expect("read exi");
        let json_out = erxi::decode_json(&exi).expect("decode_json");
        let value_out: Value = serde_json::from_str(&json_out).expect("parse output");

        assert!(values_equivalent(&value, &value_out), "mismatch for {:?}", path);
    }
}
