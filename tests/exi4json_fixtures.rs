use std::fs;
use std::path::Path;

use erxi::{decode_json, encode_json};
use serde_json::Value;

fn fixture_paths() -> Vec<std::path::PathBuf> {
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

#[test]
fn json_fixtures_roundtrip() {
    for path in fixture_paths() {
        let input = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read {:?}: {e}", path));
        let exi = encode_json(&input)
            .unwrap_or_else(|e| panic!("encode {:?}: {e}", path));
        let output = decode_json(&exi)
            .unwrap_or_else(|e| panic!("decode {:?}: {e}", path));

        let v_in: Value = serde_json::from_str(&input)
            .unwrap_or_else(|e| panic!("parse input {:?}: {e}", path));
        let v_out: Value = serde_json::from_str(&output)
            .unwrap_or_else(|e| panic!("parse output {:?}: {e}", path));

        assert!(
            values_equivalent(&v_in, &v_out),
            "roundtrip mismatch for {:?}\nleft: {:?}\nright: {:?}",
            path,
            v_in,
            v_out
        );
    }
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
