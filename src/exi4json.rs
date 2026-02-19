//! EXI for JSON (EXI4JSON) adapter layer.
//!
//! Implements the W3C EXI for JSON Working Group Note (2018-07-26).

use crate::datetime::DateTimeType;
use crate::event::{ChContent, ExiEvent};
use crate::options::{ExiOptions, SchemaId};
use crate::qname::QName;
use crate::schema::SchemaInfo;
use crate::{Error, Result};
use base64::Engine;
use serde_json::Value;
use std::cell::OnceCell;
use std::rc::Rc;

const EXI4JSON_NS: &str = "http://www.w3.org/2015/EXI/json";
const EXI4JSON_SCHEMA_ID: &str = "exi4json";
const EXI4JSON_XSD: &str = include_str!("exi4json.xsd");

const RESERVED_KEYS: [&str; 7] = [
    "map", "array", "string", "number", "boolean", "null", "other",
];

/// EXI4JSON encoder options (heuristics for <other> types).
#[derive(Debug, Clone, Copy)]
pub struct Exi4JsonOptions {
    /// Enable mapping to <other> types (base64/date/time/integer/decimal).
    pub use_other_types: bool,
}

impl Default for Exi4JsonOptions {
    fn default() -> Self {
        Self { use_other_types: false }
    }
}

/// Encode a JSON document (string) to EXI4JSON bytes.
pub fn encode_json(json: &str) -> Result<Vec<u8>> {
    encode_json_with_options(json, Exi4JsonOptions::default())
}

/// Encode a JSON document (string) to EXI4JSON bytes with options.
pub fn encode_json_with_options(json: &str, exi4json_opts: Exi4JsonOptions) -> Result<Vec<u8>> {
    let value: Value = serde_json::from_str(json)
        .map_err(|e| Error::InvalidValue(format!("JSON parse error: {e}")))?;

    let schema = exi4json_schema()?;
    let exi_opts = exi4json_options();

    let mut events = Vec::with_capacity(32);
    events.push(ExiEvent::StartDocument);
    encode_value(&value, &mut events, &exi4json_opts)?;
    events.push(ExiEvent::EndDocument);

    crate::encoder::encode_with_schema(&events, &exi_opts, &schema)
}

/// Decode EXI4JSON bytes to a JSON document string.
pub fn decode_json(exi: &[u8]) -> Result<String> {
    let schema = exi4json_schema()?;
    let options = exi4json_options();
    let (events, _opts) = crate::decoder::decode_with_schema(exi, options, &schema)?;

    let mut idx = 0usize;
    if matches!(events.get(idx), Some(ExiEvent::StartDocument)) {
        idx += 1;
    }

    let value = decode_value(&events, &mut idx)?;

    if matches!(events.get(idx), Some(ExiEvent::EndDocument)) {
        idx += 1;
    }

    if idx != events.len() {
        return Err(Error::InvalidValue("EXI4JSON: trailing events".into()));
    }

    serde_json::to_string(&value)
        .map_err(|e| Error::InvalidValue(format!("JSON encode error: {e}")))
}

fn exi4json_schema() -> Result<SchemaInfo> {
    thread_local! {
        static SCHEMA: OnceCell<SchemaInfo> = OnceCell::new();
    }
    SCHEMA.with(|cell| {
        if let Some(schema) = cell.get() {
            return Ok(schema.clone());
        }
        let parsed = crate::xsd::parse_xsd(EXI4JSON_XSD)?;
        let _ = cell.set(parsed.clone());
        Ok(parsed)
    })
}

fn exi4json_options() -> ExiOptions {
    ExiOptions::default()
        .with_strict()
        .with_schema_id(SchemaId::Id(EXI4JSON_SCHEMA_ID.to_string()))
}

fn encode_value(value: &Value, events: &mut Vec<ExiEvent>, options: &Exi4JsonOptions) -> Result<()> {
    match value {
        Value::Object(map) => {
            push_start(events, "map");
            for (key, value) in map {
                let escaped = escape_key(key)?;
                push_start(events, &escaped);
                encode_value(value, events, options)?;
                push_end(events);
            }
            push_end(events);
        }
        Value::Array(items) => {
            push_start(events, "array");
            for item in items {
                encode_value(item, events, options)?;
            }
            push_end(events);
        }
        Value::String(s) => {
            if options.use_other_types {
                if let Some(other) = encode_other_string(s) {
                    return encode_other(events, other, s);
                }
            }
            push_start(events, "string");
            push_chars(events, s);
            push_end(events);
        }
        Value::Number(num) => {
            if options.use_other_types {
                if let Some(other) = encode_other_number(num) {
                    return encode_other(events, other, &num.to_string());
                }
            }
            push_start(events, "number");
            push_chars(events, &num.to_string());
            push_end(events);
        }
        Value::Bool(b) => {
            push_start(events, "boolean");
            push_chars(events, if *b { "true" } else { "false" });
            push_end(events);
        }
        Value::Null => {
            push_start(events, "null");
            push_end(events);
        }
    }
    Ok(())
}

fn encode_other(events: &mut Vec<ExiEvent>, other_type: &str, value: &str) -> Result<()> {
    push_start(events, "other");
    push_start(events, other_type);
    push_chars(events, value);
    push_end(events);
    push_end(events);
    Ok(())
}

fn encode_other_string(value: &str) -> Option<&'static str> {
    if crate::typed_value::is_valid_datetime(value, DateTimeType::DateTime) {
        return Some("dateTime");
    }
    if crate::typed_value::is_valid_datetime(value, DateTimeType::Date) {
        return Some("date");
    }
    if crate::typed_value::is_valid_datetime(value, DateTimeType::Time) {
        return Some("time");
    }
    if is_base64(value) {
        return Some("base64Binary");
    }
    None
}

fn encode_other_number(value: &serde_json::Number) -> Option<&'static str> {
    if value.is_i64() || value.is_u64() {
        Some("integer")
    } else {
        Some("decimal")
    }
}

fn is_base64(value: &str) -> bool {
    if value.is_empty() || value.len() % 4 != 0 {
        return false;
    }
    if !value.chars().all(|c| c.is_ascii_alphanumeric() || c == '+' || c == '/' || c == '=') {
        return false;
    }
    let engine = base64::engine::general_purpose::STANDARD;
    let Ok(bytes) = engine.decode(value) else {
        return false;
    };
    let encoded = engine.encode(bytes);
    encoded == value
}

fn decode_value(events: &[ExiEvent], idx: &mut usize) -> Result<Value> {
    let qname = expect_start_element(events, idx)?;
    if qname.uri.as_ref() != EXI4JSON_NS {
        return Err(Error::InvalidValue("EXI4JSON: unexpected namespace".into()));
    }

    match qname.local_name.as_ref() {
        "map" => decode_map(events, idx),
        "array" => decode_array(events, idx),
        "string" => decode_string(events, idx),
        "number" => decode_number(events, idx),
        "boolean" => decode_boolean(events, idx),
        "null" => decode_null(events, idx),
        "other" => decode_other(events, idx),
        other => Err(Error::InvalidValue(format!("EXI4JSON: unexpected element '{other}'"))),
    }
}

fn decode_map(events: &[ExiEvent], idx: &mut usize) -> Result<Value> {
    let mut map = serde_json::Map::new();
    loop {
        match events.get(*idx) {
            Some(ExiEvent::EndElement) => {
                *idx += 1;
                break;
            }
            Some(ExiEvent::StartElement(q)) => {
                if q.uri.as_ref() != EXI4JSON_NS {
                    return Err(Error::InvalidValue("EXI4JSON: unexpected namespace".into()));
                }
                let key = unescape_key(q.local_name.as_ref())?;
                *idx += 1; // consume key SE
                let value = decode_value(events, idx)?;
                expect_end_element(events, idx)?; // end of key
                map.insert(key, value);
            }
            Some(_) => {
                return Err(Error::InvalidValue("EXI4JSON: unexpected event in map".into()));
            }
            None => return Err(Error::PrematureEndOfStream),
        }
    }
    Ok(Value::Object(map))
}

fn decode_array(events: &[ExiEvent], idx: &mut usize) -> Result<Value> {
    let mut items = Vec::new();
    loop {
        match events.get(*idx) {
            Some(ExiEvent::EndElement) => {
                *idx += 1;
                break;
            }
            Some(ExiEvent::StartElement(_)) => {
                let value = decode_value(events, idx)?;
                items.push(value);
            }
            Some(_) => {
                return Err(Error::InvalidValue("EXI4JSON: unexpected event in array".into()));
            }
            None => return Err(Error::PrematureEndOfStream),
        }
    }
    Ok(Value::Array(items))
}

fn decode_string(events: &[ExiEvent], idx: &mut usize) -> Result<Value> {
    let value = if let Some(ExiEvent::Characters(ch)) = events.get(*idx) {
        *idx += 1;
        ch.value.as_ref().to_string()
    } else {
        String::new()
    };
    expect_end_element(events, idx)?;
    Ok(Value::String(value))
}

fn decode_number(events: &[ExiEvent], idx: &mut usize) -> Result<Value> {
    let Some(ExiEvent::Characters(ch)) = events.get(*idx) else {
        return Err(Error::InvalidValue("EXI4JSON: number without value".into()));
    };
    *idx += 1;
    expect_end_element(events, idx)?;

    let value: Value = serde_json::from_str(ch.value.as_ref())
        .map_err(|_| Error::InvalidValue("EXI4JSON: invalid JSON number".into()))?;
    match value {
        Value::Number(num) => Ok(Value::Number(num)),
        _ => Err(Error::InvalidValue("EXI4JSON: invalid JSON number".into())),
    }
}

fn decode_boolean(events: &[ExiEvent], idx: &mut usize) -> Result<Value> {
    let Some(ExiEvent::Characters(ch)) = events.get(*idx) else {
        return Err(Error::InvalidValue("EXI4JSON: boolean without value".into()));
    };
    *idx += 1;
    expect_end_element(events, idx)?;

    match ch.value.as_ref() {
        "true" => Ok(Value::Bool(true)),
        "false" => Ok(Value::Bool(false)),
        _ => Err(Error::InvalidValue("EXI4JSON: invalid boolean".into())),
    }
}

fn decode_null(events: &[ExiEvent], idx: &mut usize) -> Result<Value> {
    match events.get(*idx) {
        Some(ExiEvent::EndElement) => {
            *idx += 1;
            Ok(Value::Null)
        }
        Some(_) => Err(Error::InvalidValue("EXI4JSON: null must be empty".into())),
        None => Err(Error::PrematureEndOfStream),
    }
}

fn decode_other(events: &[ExiEvent], idx: &mut usize) -> Result<Value> {
    let qname = expect_start_element(events, idx)?;
    if qname.uri.as_ref() != EXI4JSON_NS {
        return Err(Error::InvalidValue("EXI4JSON: unexpected namespace".into()));
    }

    let local = qname.local_name.as_ref();
    match local {
        "base64Binary" | "dateTime" | "time" | "date" => {
            let value = decode_string(events, idx)?;
            expect_end_element(events, idx)?; // end of <other>
            Ok(value)
        }
        "integer" | "decimal" => {
            let value = decode_number(events, idx)?;
            expect_end_element(events, idx)?; // end of <other>
            Ok(value)
        }
        _ => Err(Error::InvalidValue("EXI4JSON: unknown <other> type".into())),
    }
}

fn push_start(events: &mut Vec<ExiEvent>, local: &str) {
    events.push(ExiEvent::StartElement(Rc::new(QName::new(EXI4JSON_NS, local))));
}

fn push_end(events: &mut Vec<ExiEvent>) {
    events.push(ExiEvent::EndElement);
}

fn push_chars(events: &mut Vec<ExiEvent>, value: &str) {
    events.push(ExiEvent::Characters(ChContent { value: Rc::from(value) }));
}

fn expect_start_element<'a>(events: &'a [ExiEvent], idx: &mut usize) -> Result<&'a QName> {
    match events.get(*idx) {
        Some(ExiEvent::StartElement(q)) => {
            *idx += 1;
            Ok(q.as_ref())
        }
        Some(_) => Err(Error::InvalidValue("EXI4JSON: expected StartElement".into())),
        None => Err(Error::PrematureEndOfStream),
    }
}

fn expect_end_element(events: &[ExiEvent], idx: &mut usize) -> Result<()> {
    match events.get(*idx) {
        Some(ExiEvent::EndElement) => {
            *idx += 1;
            Ok(())
        }
        Some(_) => Err(Error::InvalidValue("EXI4JSON: expected EndElement".into())),
        None => Err(Error::PrematureEndOfStream),
    }
}

fn escape_key(key: &str) -> Result<String> {
    if key.is_empty() {
        return Err(Error::InvalidValue("EXI4JSON: empty object key".into()));
    }
    if RESERVED_KEYS.contains(&key) {
        return Ok(format!("_.{key}"));
    }

    let mut out = String::new();
    for (idx, ch) in key.chars().enumerate() {
        if ch == '_' || !is_ncname_char(ch, idx == 0) {
            push_escape(&mut out, ch);
        } else {
            out.push(ch);
        }
    }
    Ok(out)
}

fn unescape_key(key: &str) -> Result<String> {
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
                return Err(Error::InvalidValue("EXI4JSON: invalid escape".into()));
            }
            digits.push(next);
            chars.next();
        }
        if digits.is_empty() {
            return Err(Error::InvalidValue("EXI4JSON: invalid escape".into()));
        }
        match chars.next() {
            Some('.') => {},
            _ => return Err(Error::InvalidValue("EXI4JSON: invalid escape".into())),
        }
        let code = digits.parse::<u32>()
            .map_err(|_| Error::InvalidValue("EXI4JSON: invalid escape".into()))?;
        let decoded = char::from_u32(code)
            .ok_or_else(|| Error::InvalidValue("EXI4JSON: invalid code point".into()))?;
        out.push(decoded);
    }

    Ok(out)
}

fn push_escape(out: &mut String, ch: char) {
    out.push('_');
    out.push_str(&(ch as u32).to_string());
    out.push('.');
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escape_reserved_names() {
        assert_eq!(escape_key("map").unwrap(), "_.map");
        assert_eq!(escape_key("array").unwrap(), "_.array");
    }

    #[test]
    fn escape_and_unescape_roundtrip() {
        let key = "1 key";
        let escaped = escape_key(key).unwrap();
        assert_eq!(escaped, "_49._32.key");
        assert_eq!(unescape_key(&escaped).unwrap(), key);
    }

    #[test]
    fn escape_underscore() {
        let key = "a_b";
        let escaped = escape_key(key).unwrap();
        assert_eq!(escaped, "a_95.b");
        assert_eq!(unescape_key(&escaped).unwrap(), key);
    }

    #[test]
    fn roundtrip_simple_example() {
        let json = r#"{"keyNumber":123,"keyArrayStrings":["s1","s2"]}"#;
        let exi = encode_json(json).unwrap();
        let decoded = decode_json(&exi).unwrap();
        let v_in: Value = serde_json::from_str(json).unwrap();
        let v_out: Value = serde_json::from_str(&decoded).unwrap();
        assert_eq!(v_in, v_out);
    }

    #[test]
    fn roundtrip_escaping_example() {
        let json = r#"{"1 key":"value","map":"x","a_b":"y"}"#;
        let exi = encode_json(json).unwrap();
        let decoded = decode_json(&exi).unwrap();
        let v_in: Value = serde_json::from_str(json).unwrap();
        let v_out: Value = serde_json::from_str(&decoded).unwrap();
        assert_eq!(v_in, v_out);
    }
}
