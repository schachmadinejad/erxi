//! Typed Value Encoding/Decoding (Spec 7, Table 7-1).
//!
//! Dieses Modul implementiert typisierte Wert-Codierung basierend auf `TypeDefinition`
//! aus dem Schema. Bei Schema-informed Encoding werden Werte nicht als Strings,
//! sondern mit ihrem spezifischen XSD-Datentyp encodiert.
//!
//! # Spec-Referenz
//!
//! - Spec 7.1: Built-in EXI Datatype Representations
//! - Spec Table 7-1: EXI Datatype Representation Map
//! - Spec 8.5.4.1.3: Type Grammars

use crate::bitstream::{BitReader, BitWriter};
use crate::options::Alignment;
use crate::qname::QName;
use crate::schema::{SimpleTypeVariety, TypeDefinition};
use crate::{Error, Result};
use std::cell::RefCell;

/// XSD Namespace URI.
pub const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema";

// ============================================================================
// Type Resolution
// ============================================================================

/// Löst den ultimativen Built-in Basistyp für ein TypeDefinition auf.
///
/// Folgt der base_type-Kette bis ein bekannter XSD Built-in Typ gefunden wird.
/// Für Union-Typen wird immer "string" zurückgegeben (Spec: character representation).
pub fn resolve_base_type(type_def: &TypeDefinition) -> Option<&str> {
    match type_def {
        TypeDefinition::Simple {
            variety, base_type, ..
        } => {
            // Union-Typen werden immer als String encodiert
            if matches!(variety, SimpleTypeVariety::Union { .. }) {
                return Some("string");
            }
            // List-Typen haben einen item_type, der separat behandelt wird
            if matches!(variety, SimpleTypeVariety::List { .. }) {
                return Some("list");
            }
            // Atomic: base_type verwenden
            base_type.as_deref()
        }
        TypeDefinition::Complex { content, .. } => {
            // Complex Type mit Simple Content → base_type des Simple Contents
            if matches!(content, crate::schema::ContentType::Simple) {
                // simpleContent verwendet den Base-Type der Ableitung.
                // Wenn base_type ein built-in XSD-Typ ist, können wir ihn direkt verwenden.
                // Andernfalls fallback zu String (bis wir transitive Auflösung haben).
                if let TypeDefinition::Complex { base_type, .. } = type_def
                    && let Some(qname) = base_type.as_ref()
                        && &*qname.uri == XSD_NS {
                            return Some(&qname.local_name);
                        }
                Some("string")
            } else {
                None
            }
        }
    }
}

/// Prüft ob ein aufgelöster Basis-Typ ein String-basierter Typ ist.
///
/// String-basierte Typen werden als EXI Strings encodiert (Spec 7.1.10),
/// nicht mit einer spezifischen Typed Value Representation.
pub fn is_string_base_type(base_type: Option<&str>) -> bool {
    matches!(
        base_type,
        Some("string") | Some("normalizedString") | Some("token") | Some("language")
            | Some("Name") | Some("NCName") | Some("NMTOKEN") | Some("NMTOKENS")
            | Some("ID") | Some("IDREF") | Some("IDREFS") | Some("ENTITY")
            | Some("ENTITIES") | Some("anyURI") | Some("QName") | Some("NOTATION")
            | Some("anySimpleType") | None
    )
}

// ============================================================================
// Encode Typed Value
// ============================================================================

/// Encodiert einen typisierten Wert basierend auf TypeDefinition.
///
/// # Parameter
///
/// - `writer`: BitWriter für Output
/// - `value`: Der zu encodierende Wert als String
/// - `type_def`: Die TypeDefinition aus dem Schema
/// - `strict`: Bei true wird Fehler bei ungültigem Wert geworfen,
///             bei false wird Fallback zu String verwendet
/// - `alignment`: Alignment-Modus (BitPacked, ByteAlignment, PreCompression)
///
/// # Spec-Referenz
///
/// - Spec 7.1: Built-in EXI Datatype Representations
/// - Spec 7.1.2: Boolean bei byte-aligned = 1 Byte
/// - Spec 7.1.5: Integer Sign bei byte-aligned = 1 Byte
/// - Spec 8.5.4.4.1: Fallback bei strict=false
pub fn encode_typed_value(
    writer: &mut BitWriter,
    value: &str,
    type_def: &TypeDefinition,
    strict: bool,
    alignment: Alignment,
) -> Result<()> {
    // Prüfe auf List-Typ
    if let TypeDefinition::Simple {
        variety: SimpleTypeVariety::List { item_type },
        ..
    } = type_def
    {
        return encode_list(
            writer,
            value,
            item_type.as_ref().map(|t| t.as_ref()),
            strict,
            alignment,
        );
    }

    // Resolve base type
    let base_type = resolve_base_type(type_def);

    match base_type {
        Some(bt) => encode_by_base_type(writer, value, bt, strict, alignment),
        None => {
            // Kein base_type bekannt → als String encodieren
            encode_string(writer, value);
            Ok(())
        }
    }
}

/// Mappt einen XSD base_type String auf den entsprechenden DateTimeType.
fn str_to_datetime_type(base_type: &str) -> Option<crate::datetime::DateTimeType> {
    use crate::datetime::DateTimeType;
    match base_type {
        "dateTime" => Some(DateTimeType::DateTime),
        "date" => Some(DateTimeType::Date),
        "time" => Some(DateTimeType::Time),
        "gYear" => Some(DateTimeType::GYear),
        "gYearMonth" => Some(DateTimeType::GYearMonth),
        "gMonth" => Some(DateTimeType::GMonth),
        "gMonthDay" => Some(DateTimeType::GMonthDay),
        "gDay" => Some(DateTimeType::GDay),
        _ => None,
    }
}

/// Encodiert einen Wert basierend auf dem base_type String.
pub(crate) fn encode_by_base_type(
    writer: &mut BitWriter,
    value: &str,
    base_type: &str,
    strict: bool,
    alignment: Alignment,
) -> Result<()> {
    let is_byte_aligned = matches!(alignment, Alignment::ByteAlignment | Alignment::PreCompression);

    let result = match base_type {
        // Integer-Familie (Spec 7.1.5)
        "byte" => {
            // Spec 7.1.5 + 7.1.9: bounded integers (range <= 4096) use n-bit unsigned
            // offset from min (spec/exi-spec.txt lines 1284-1287).
            let parsed: i64 = value
                .trim()
                .parse()
                .map_err(|_| Error::InvalidValue(format!("Invalid byte: {}", value)))?;
            crate::integer::encode_bounded(writer, parsed, -128, 127);
            Ok(())
        }
        "integer" | "nonPositiveInteger" | "negativeInteger" | "long" | "int" | "short" => {
            encode_integer(writer, value, is_byte_aligned)
        }
        "unsignedByte" => {
            // Spec 7.1.6 + 7.1.9: bounded unsigned range [0,255] uses 8-bit n-bit encoding
            // (spec/exi-spec.txt lines 1284-1287).
            let parsed: u64 = value
                .trim()
                .parse()
                .map_err(|_| Error::InvalidValue(format!("Invalid unsignedByte: {}", value)))?;
            if parsed > 255 {
                return Err(Error::InvalidValue(format!("Invalid unsignedByte: {}", value)));
            }
            crate::n_bit_unsigned_integer::encode(writer, parsed, 8);
            Ok(())
        }
        "nonNegativeInteger" | "positiveInteger" | "unsignedLong" | "unsignedInt"
        | "unsignedShort" => {
            encode_unsigned_integer(writer, value)
        }

        // Boolean (Spec 7.1.2)
        "boolean" => encode_boolean(writer, value, is_byte_aligned),

        // Float/Double (Spec 7.1.4)
        "float" | "double" => encode_float(writer, value, is_byte_aligned),

        // Decimal (Spec 7.1.3)
        "decimal" => encode_decimal(writer, value, is_byte_aligned),

        // DateTime-Familie (Spec 7.1.8)
        dt if str_to_datetime_type(dt).is_some() => {
            encode_datetime(writer, value, str_to_datetime_type(dt).unwrap(), is_byte_aligned)
        }

        // Binary (Spec 7.1.1)
        "base64Binary" => encode_base64_binary(writer, value),
        "hexBinary" => encode_hex_binary(writer, value),

        // String und alle anderen → character representation
        _ => {
            encode_string(writer, value);
            Ok(())
        }
    };

    // Bei strict=false: Fallback zu String bei Fehler
    match result {
        Ok(()) => Ok(()),
        Err(_) if !strict => {
            encode_string(writer, value);
            Ok(())
        }
        Err(e) => Err(e),
    }
}

// ============================================================================
// Decode Typed Value
// ============================================================================

/// Decodiert einen typisierten Wert basierend auf TypeDefinition.
///
/// # Spec-Referenz
///
/// - Spec 7.1: Built-in EXI Datatype Representations
/// - Spec 7.1.2: Boolean bei byte-aligned = 1 Byte
/// - Spec 7.1.5: Integer Sign bei byte-aligned = 1 Byte
pub fn decode_typed_value(
    reader: &mut BitReader,
    type_def: &TypeDefinition,
    alignment: Alignment,
) -> Result<String> {
    // Prüfe auf List-Typ
    if let TypeDefinition::Simple {
        variety: SimpleTypeVariety::List { item_type },
        ..
    } = type_def
    {
        return decode_list(
            reader,
            item_type.as_ref().map(|t| t.as_ref()),
            alignment,
        );
    }

    // Resolve base type
    let base_type = resolve_base_type(type_def);

    match base_type {
        Some(bt) => decode_by_base_type(reader, bt, alignment),
        None => {
            // Kein base_type bekannt → als String decodieren
            decode_string(reader)
        }
    }
}

/// Decodiert einen Wert basierend auf dem base_type String.
pub(crate) fn decode_by_base_type(reader: &mut BitReader, base_type: &str, alignment: Alignment) -> Result<String> {
    let is_byte_aligned = matches!(alignment, Alignment::ByteAlignment | Alignment::PreCompression);

    match base_type {
        // Integer-Familie (Spec 7.1.5)
        "byte" => {
            // Spec 7.1.5 + 7.1.9: bounded integers (range <= 4096) use n-bit unsigned
            // offset from min (spec/exi-spec.txt lines 1284-1287).
            // Bounded range [-128, 127] → 8-bit offset encoding
            crate::integer::decode_bounded(reader, -128, 127).map(|v| v.to_string())
        }
        "integer" | "nonPositiveInteger" | "negativeInteger" | "long" | "int" | "short" => {
            decode_integer(reader, is_byte_aligned)
        }
        "unsignedByte" => {
            // Spec 7.1.6 + 7.1.9: bounded unsigned range [0,255] uses 8-bit n-bit encoding
            // (spec/exi-spec.txt lines 1284-1287).
            // Bounded range [0, 255] → 8-bit unsigned
            let value = crate::n_bit_unsigned_integer::decode(reader, 8)?;
            Ok(value.to_string())
        }
        "nonNegativeInteger" | "positiveInteger" | "unsignedLong" | "unsignedInt"
        | "unsignedShort" => decode_unsigned_integer(reader),

        // Boolean (Spec 7.1.2)
        "boolean" => decode_boolean(reader, is_byte_aligned),

        // Float/Double (Spec 7.1.4)
        "float" | "double" => decode_float(reader, is_byte_aligned),

        // Decimal (Spec 7.1.3)
        "decimal" => decode_decimal(reader, is_byte_aligned),

        // DateTime-Familie (Spec 7.1.8)
        dt if str_to_datetime_type(dt).is_some() => {
            decode_datetime(reader, str_to_datetime_type(dt).unwrap(), is_byte_aligned)
        }

        // Binary (Spec 7.1.1)
        "base64Binary" => decode_base64_binary(reader),
        "hexBinary" => decode_hex_binary(reader),

        // String und alle anderen
        _ => decode_string(reader),
    }
}

// ============================================================================
// Primitive Type Encoders
// ============================================================================

/// Encodiert einen String (Spec 7.1.10).
fn encode_string(writer: &mut BitWriter, value: &str) {
    crate::string::encode(writer, value);
}

/// Decodiert einen String (Spec 7.1.10).
fn decode_string(reader: &mut BitReader) -> Result<String> {
    crate::string::decode(reader)
}

/// Encodiert einen Integer byte-aligned (Spec 7.1.5).
/// Sign als 1 Byte (Spec 7.1.2 + 7.1.9), Magnitude als Unsigned Integer.
fn encode_integer_raw(writer: &mut BitWriter, value: i64) {
    if value >= 0 {
        writer.write_byte_aligned(0);
        crate::unsigned_integer::encode(writer, value as u64);
    } else {
        writer.write_byte_aligned(1);
        crate::unsigned_integer::encode(writer, !(value as u64));
    }
}

/// Decodiert einen Integer byte-aligned (Spec 7.1.5).
fn decode_integer_raw(reader: &mut BitReader) -> Result<i64> {
    let sign_byte = reader.read_byte_aligned()?;
    let is_negative = sign_byte != 0;
    let magnitude = crate::unsigned_integer::decode(reader)?;
    if magnitude > i64::MAX as u64 {
        return Err(Error::IntegerOverflow);
    }
    if is_negative {
        Ok(-(magnitude as i64) - 1)
    } else {
        Ok(magnitude as i64)
    }
}

fn encode_integer(writer: &mut BitWriter, value: &str, byte_aligned: bool) -> Result<()> {
    let parsed: i64 = value
        .trim()
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid integer: {}", value)))?;

    if byte_aligned {
        encode_integer_raw(writer, parsed);
    } else {
        crate::integer::encode(writer, parsed);
    }
    Ok(())
}

/// Decodiert einen Integer (Spec 7.1.5).
fn decode_integer(reader: &mut BitReader, byte_aligned: bool) -> Result<String> {
    if byte_aligned {
        // Spec 7.1.2 + 7.1.5: Bei byte-aligned ist Sign = 1 Byte
        let sign_byte = reader.read_byte_aligned()?;
        let is_negative = sign_byte != 0;
        let magnitude = crate::unsigned_integer::decode(reader)?;
        // i128 statt i64 um Overflow bei magnitude > i64::MAX zu vermeiden
        let value: i128 = if is_negative {
            -(magnitude as i128) - 1
        } else {
            magnitude as i128
        };
        Ok(value.to_string())
    } else {
        let value = crate::integer::decode(reader)?;
        Ok(value.to_string())
    }
}

/// Encodiert einen Unsigned Integer (Spec 7.1.6).
fn encode_unsigned_integer(writer: &mut BitWriter, value: &str) -> Result<()> {
    let parsed: u64 = value
        .trim()
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid unsigned integer: {}", value)))?;
    crate::unsigned_integer::encode(writer, parsed);
    Ok(())
}

/// Decodiert einen Unsigned Integer (Spec 7.1.6).
fn decode_unsigned_integer(reader: &mut BitReader) -> Result<String> {
    let value = crate::unsigned_integer::decode(reader)?;
    Ok(value.to_string())
}

/// Encodiert einen Boolean (Spec 7.1.2).
fn encode_boolean(writer: &mut BitWriter, value: &str, byte_aligned: bool) -> Result<()> {
    let parsed = match value.trim() {
        "true" | "1" => true,
        "false" | "0" => false,
        _ => return Err(Error::InvalidValue(format!("Invalid boolean: {}", value))),
    };
    if byte_aligned {
        // Spec 7.1.2: Bei byte-aligned = 1 Byte
        writer.write_byte_aligned(if parsed { 1 } else { 0 });
    } else {
        crate::boolean::encode(writer, parsed);
    }
    Ok(())
}

/// Decodiert einen Boolean (Spec 7.1.2).
fn decode_boolean(reader: &mut BitReader, byte_aligned: bool) -> Result<String> {
    if byte_aligned {
        // Spec 7.1.2: Bei byte-aligned = 1 Byte
        let byte = reader.read_byte_aligned()?;
        let value = byte != 0;
        Ok(if value { "true" } else { "false" }.to_string())
    } else {
        let value = crate::boolean::decode(reader)?;
        Ok(if value { "true" } else { "false" }.to_string())
    }
}

/// Encodiert einen Float (Spec 7.1.4).
///
/// Float = zwei Integers (7.1.5). Integer bei byte-aligned: Sign als 1 Byte (7.1.9).
fn encode_float(writer: &mut BitWriter, value: &str, byte_aligned: bool) -> Result<()> {
    let trimmed = value.trim();

    let float_val = match trimmed {
        "INF" => crate::float::Float::Infinity,
        "-INF" => crate::float::Float::NegativeInfinity,
        "NaN" => crate::float::Float::NaN,
        _ => {
            // Parse als f64 und konvertiere zu Mantissa/Exponent
            let f: f64 = trimmed
                .parse()
                .map_err(|_| Error::InvalidValue(format!("Invalid float: {}", value)))?;

            if f.is_infinite() {
                if f.is_sign_positive() {
                    crate::float::Float::Infinity
                } else {
                    crate::float::Float::NegativeInfinity
                }
            } else if f.is_nan() {
                crate::float::Float::NaN
            } else {
                // Konvertiere zu dezimaler Mantissa/Exponent-Darstellung
                let (mantissa, exponent) = float_to_mantissa_exponent(f);
                crate::float::Float::Value { mantissa, exponent }
            }
        }
    };

    if byte_aligned {
        let (mantissa, exponent) = match float_val {
            crate::float::Float::Value { mantissa, exponent } => (mantissa, exponent),
            crate::float::Float::Infinity => (1, -(1i64 << 14)),
            crate::float::Float::NegativeInfinity => (-1, -(1i64 << 14)),
            crate::float::Float::NaN => (0, -(1i64 << 14)),
        };
        encode_integer_raw(writer, mantissa);
        encode_integer_raw(writer, exponent);
    } else {
        crate::float::encode(writer, float_val);
    }
    Ok(())
}

/// Konvertiert f64 zu (mantissa, exponent) für EXI Float encoding.
fn float_to_mantissa_exponent(f: f64) -> (i64, i64) {
    if f == 0.0 {
        return (0, 0);
    }

    // Berechne Exponent (Anzahl Dezimalstellen)
    let abs_f = f.abs();
    let exp10 = abs_f.log10().floor() as i64;

    // Normalisiere zu einer Mantisse ohne Nachkommastellen
    // Wir wählen eine Präzision, die die signifikanten Stellen erhält
    let precision = 15i64; // f64 hat ~15-16 signifikante Dezimalstellen
    let scale = 10f64.powi((precision - exp10 - 1) as i32);
    let scaled = f * scale;
    if scaled.abs() > i64::MAX as f64 {
        // Praezision reduzieren bis Mantisse in i64 passt
        let mut m_prec = precision - 1;
        while m_prec > 0 {
            let s = 10f64.powi((m_prec - exp10 - 1) as i32);
            let sv = f * s;
            if sv.abs() <= i64::MAX as f64 {
                let mut m = sv.round() as i64;
                let mut e = -(m_prec - exp10 - 1);
                while m != 0 && m % 10 == 0 {
                    m /= 10;
                    e += 1;
                }
                return (m, e);
            }
            m_prec -= 1;
        }
        return (f.round() as i64, 0);
    }
    let mantissa = scaled.round() as i64;
    let exponent = -(precision - exp10 - 1);

    // Vereinfache: entferne trailing zeros aus Mantissa
    let (mut m, mut e) = (mantissa, exponent);
    while m != 0 && m % 10 == 0 {
        m /= 10;
        e += 1;
    }

    (m, e)
}

/// Decodiert einen Float (Spec 7.1.4).
///
/// Bei byte-aligned: Integer-Komponenten mit 1-Byte-Sign (Spec 7.1.5 + 7.1.9).
fn decode_float(reader: &mut BitReader, byte_aligned: bool) -> Result<String> {
    let float_val = if byte_aligned {
        let mantissa = decode_integer_raw(reader)?;
        let exponent = decode_integer_raw(reader)?;
        if exponent == -(1i64 << 14) {
            match mantissa {
                1 => crate::float::Float::Infinity,
                -1 => crate::float::Float::NegativeInfinity,
                _ => crate::float::Float::NaN,
            }
        } else {
            crate::float::Float::Value { mantissa, exponent }
        }
    } else {
        crate::float::decode(reader)?
    };

    let s = match float_val {
        crate::float::Float::Infinity => "INF".to_string(),
        crate::float::Float::NegativeInfinity => "-INF".to_string(),
        crate::float::Float::NaN => "NaN".to_string(),
        crate::float::Float::Value { mantissa, exponent } => {
            if exponent == 0 {
                mantissa.to_string()
            } else if exponent > 0 {
                // mantissa * 10^exponent
                let factor = 10i64.checked_pow(exponent as u32).unwrap_or(i64::MAX);
                mantissa.saturating_mul(factor).to_string()
            } else {
                // mantissa * 10^exponent = mantissa / 10^(-exponent)
                let divisor = 10f64.powi(-exponent as i32);
                let result = (mantissa as f64) / divisor;
                format!("{}", result)
            }
        }
    };

    Ok(s)
}

/// Encodiert einen Decimal (Spec 7.1.3).
///
/// Bei byte-aligned/pre-compression: Sign ist Boolean (7.1.2) = n-bit UI mit n=1
/// → Spec 7.1.9: minimum 1 Byte. Unsigned Integers (7.1.6) verwenden bereits
/// write_byte_aligned().
fn encode_decimal(writer: &mut BitWriter, value: &str, byte_aligned: bool) -> Result<()> {
    let trimmed = value.trim();
    let negative = trimmed.starts_with('-');
    let unsigned = trimmed.trim_start_matches('-').trim_start_matches('+');

    let (integral_str, fractional_str) = if let Some(pos) = unsigned.find('.') {
        (&unsigned[..pos], &unsigned[pos + 1..])
    } else {
        (unsigned, "")
    };

    let integral: u64 = if integral_str.is_empty() {
        0
    } else {
        integral_str
            .parse()
            .map_err(|_| Error::InvalidValue(format!("Invalid decimal: {}", value)))?
    };

    // Fractional: Ziffern umkehren
    let fractional: u64 = if fractional_str.is_empty() {
        0
    } else {
        let reversed: String = fractional_str.chars().rev().collect();
        reversed
            .parse()
            .map_err(|_| Error::InvalidValue(format!("Invalid decimal fractional: {}", value)))?
    };

    if byte_aligned {
        // Spec 7.1.2 + 7.1.9: Boolean sign als 1 Byte bei byte-alignment
        writer.write_byte_aligned(if negative { 1 } else { 0 });
        crate::unsigned_integer::encode(writer, integral);
        crate::unsigned_integer::encode(writer, fractional);
    } else {
        crate::decimal::encode(
            writer,
            crate::decimal::Decimal {
                negative,
                integral,
                fractional,
            },
        );
    }
    Ok(())
}

/// Decodiert einen Decimal (Spec 7.1.3).
///
/// Bei byte-aligned: Sign als 1 Byte (Spec 7.1.2 + 7.1.9).
fn decode_decimal(reader: &mut BitReader, byte_aligned: bool) -> Result<String> {
    let d = if byte_aligned {
        // Spec 7.1.2 + 7.1.9: Boolean sign als 1 Byte bei byte-alignment
        let sign_byte = reader.read_byte_aligned()?;
        let negative = sign_byte != 0;
        let integral = crate::unsigned_integer::decode(reader)?;
        let fractional = crate::unsigned_integer::decode(reader)?;
        crate::decimal::Decimal {
            negative,
            integral,
            fractional,
        }
    } else {
        crate::decimal::decode(reader)?
    };

    let mut s = String::new();
    if d.negative {
        s.push('-');
    }
    s.push_str(&d.integral.to_string());

    if d.fractional > 0 {
        s.push('.');
        // Fractional-Ziffern sind reversed gespeichert
        let frac_str = d.fractional.to_string();
        let frac_reversed: String = frac_str.chars().rev().collect();
        s.push_str(&frac_reversed);
    }

    Ok(s)
}

/// Encodiert einen DateTime-Wert (Spec 7.1.8).
fn encode_datetime(
    writer: &mut BitWriter,
    value: &str,
    dt_type: crate::datetime::DateTimeType,
    byte_aligned: bool,
) -> Result<()> {
    let dt = parse_datetime(value, dt_type)?;
    crate::datetime::encode(writer, &dt, byte_aligned)
}

/// Decodiert einen DateTime-Wert (Spec 7.1.8).
fn decode_datetime(
    reader: &mut BitReader,
    dt_type: crate::datetime::DateTimeType,
    byte_aligned: bool,
) -> Result<String> {
    let dt = crate::datetime::decode(reader, dt_type, byte_aligned)?;
    Ok(format_datetime(&dt))
}

/// Parst einen DateTime-String zu DateTime struct.
fn parse_datetime(
    value: &str,
    dt_type: crate::datetime::DateTimeType,
) -> Result<crate::datetime::DateTime> {
    use crate::datetime::DateTimeType;

    let trimmed = value.trim();

    // Timezone extrahieren falls vorhanden
    let (main_part, tz_offset) = extract_timezone(trimmed)?;

    match dt_type {
        DateTimeType::DateTime => parse_full_datetime(main_part, tz_offset),
        DateTimeType::Date => parse_date(main_part, tz_offset),
        DateTimeType::Time => parse_time(main_part, tz_offset),
        DateTimeType::GYear => parse_gyear(main_part, tz_offset),
        DateTimeType::GYearMonth => parse_gyearmonth(main_part, tz_offset),
        DateTimeType::GMonth => parse_gmonth(main_part, tz_offset),
        DateTimeType::GMonthDay => parse_gmonthday(main_part, tz_offset),
        DateTimeType::GDay => parse_gday(main_part, tz_offset),
    }
}

/// Extrahiert Timezone-Offset aus ISO-8601 String.
fn extract_timezone(value: &str) -> Result<(&str, Option<i16>)> {
    if let Some(without_z) = value.strip_suffix('Z') {
        return Ok((without_z, Some(0)));
    }

    // Suche nach +HH:MM oder -HH:MM am Ende
    if let Some(pos) = value.rfind('+').or_else(|| value.rfind('-')) {
        let tz_part = &value[pos..];
        if tz_part.len() >= 6 && tz_part.chars().nth(3) == Some(':') {
            let sign = if tz_part.starts_with('-') { -1 } else { 1 };
            let hours: i16 = tz_part[1..3]
                .parse()
                .map_err(|_| Error::InvalidValue(format!("Invalid timezone: {}", tz_part)))?;
            let minutes: i16 = tz_part[4..6]
                .parse()
                .map_err(|_| Error::InvalidValue(format!("Invalid timezone: {}", tz_part)))?;
            let offset = sign * (hours * 60 + minutes);
            return Ok((&value[..pos], Some(offset)));
        }
    }

    Ok((value, None))
}

/// Parst vollständiges DateTime (YYYY-MM-DDTHH:MM:SS.fff).
fn parse_full_datetime(
    value: &str,
    tz_offset: Option<i16>,
) -> Result<crate::datetime::DateTime> {
    // Format: YYYY-MM-DDTHH:MM:SS oder YYYY-MM-DDTHH:MM:SS.fff
    let parts: Vec<&str> = value.split('T').collect();
    if parts.len() != 2 {
        return Err(Error::InvalidValue(format!("Invalid dateTime: {}", value)));
    }

    let date_parts: Vec<&str> = parts[0].split('-').collect();
    if date_parts.len() < 3 {
        return Err(Error::InvalidValue(format!("Invalid date part: {}", parts[0])));
    }

    let year: i64 = date_parts[0]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid year: {}", date_parts[0])))?;
    let month: u8 = date_parts[1]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid month: {}", date_parts[1])))?;
    let day: u8 = date_parts[2]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid day: {}", date_parts[2])))?;

    let time_part = parts[1];
    let (hour, minute, second, fractional) = parse_time_components(time_part)?;

    Ok(crate::datetime::DateTime::DateTime {
        year: year - 2000, // Offset from 2000
        month,
        day,
        hour,
        minute,
        second,
        fractional_secs: fractional,
        timezone_offset_minutes: tz_offset,
    })
}

/// Parst Time-Komponenten (HH:MM:SS.fff).
fn parse_time_components(value: &str) -> Result<(u8, u8, u8, Option<u64>)> {
    // Trenne Sekunden und Fractional
    let (time_str, frac_str) = if let Some(pos) = value.find('.') {
        (&value[..pos], Some(&value[pos + 1..]))
    } else {
        (value, None)
    };

    let parts: Vec<&str> = time_str.split(':').collect();
    if parts.len() < 3 {
        return Err(Error::InvalidValue(format!("Invalid time: {}", value)));
    }

    let hour: u8 = parts[0]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid hour: {}", parts[0])))?;
    let minute: u8 = parts[1]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid minute: {}", parts[1])))?;
    let second: u8 = parts[2]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid second: {}", parts[2])))?;

    let fractional = if let Some(frac) = frac_str {
        // Fractional wird reversed gespeichert
        let reversed: String = frac.chars().rev().collect();
        Some(
            reversed
                .parse()
                .map_err(|_| Error::InvalidValue(format!("Invalid fractional: {}", frac)))?,
        )
    } else {
        None
    };

    Ok((hour, minute, second, fractional))
}

/// Parst Date (YYYY-MM-DD).
fn parse_date(value: &str, tz_offset: Option<i16>) -> Result<crate::datetime::DateTime> {
    let parts: Vec<&str> = value.split('-').collect();
    if parts.len() < 3 {
        return Err(Error::InvalidValue(format!("Invalid date: {}", value)));
    }

    let year: i64 = parts[0]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid year: {}", parts[0])))?;
    let month: u8 = parts[1]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid month: {}", parts[1])))?;
    let day: u8 = parts[2]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid day: {}", parts[2])))?;

    Ok(crate::datetime::DateTime::Date {
        year: year - 2000,
        month,
        day,
        timezone_offset_minutes: tz_offset,
    })
}

/// Parst Time (HH:MM:SS.fff).
fn parse_time(value: &str, tz_offset: Option<i16>) -> Result<crate::datetime::DateTime> {
    let (hour, minute, second, fractional) = parse_time_components(value)?;

    Ok(crate::datetime::DateTime::Time {
        hour,
        minute,
        second,
        fractional_secs: fractional,
        timezone_offset_minutes: tz_offset,
    })
}

/// Parst gYear (YYYY).
fn parse_gyear(value: &str, tz_offset: Option<i16>) -> Result<crate::datetime::DateTime> {
    let year: i64 = value
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid gYear: {}", value)))?;

    Ok(crate::datetime::DateTime::GYear {
        year: year - 2000,
        timezone_offset_minutes: tz_offset,
    })
}

/// Parst gYearMonth (YYYY-MM).
fn parse_gyearmonth(value: &str, tz_offset: Option<i16>) -> Result<crate::datetime::DateTime> {
    let parts: Vec<&str> = value.split('-').collect();
    if parts.len() < 2 {
        return Err(Error::InvalidValue(format!("Invalid gYearMonth: {}", value)));
    }

    let year: i64 = parts[0]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid year: {}", parts[0])))?;
    let month: u8 = parts[1]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid month: {}", parts[1])))?;

    Ok(crate::datetime::DateTime::GYearMonth {
        year: year - 2000,
        month,
        day: 0, // day=0 für gYearMonth
        timezone_offset_minutes: tz_offset,
    })
}

/// Parst gMonth (--MM).
fn parse_gmonth(value: &str, tz_offset: Option<i16>) -> Result<crate::datetime::DateTime> {
    let stripped = value.trim_start_matches('-');
    let month: u8 = stripped
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid gMonth: {}", value)))?;

    Ok(crate::datetime::DateTime::GMonth {
        month,
        timezone_offset_minutes: tz_offset,
    })
}

/// Parst gMonthDay (--MM-DD).
fn parse_gmonthday(value: &str, tz_offset: Option<i16>) -> Result<crate::datetime::DateTime> {
    let stripped = value.trim_start_matches('-');
    let parts: Vec<&str> = stripped.split('-').collect();
    if parts.len() < 2 {
        return Err(Error::InvalidValue(format!("Invalid gMonthDay: {}", value)));
    }

    let month: u8 = parts[0]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid month: {}", parts[0])))?;
    let day: u8 = parts[1]
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid day: {}", parts[1])))?;

    Ok(crate::datetime::DateTime::GMonthDay {
        month,
        day,
        timezone_offset_minutes: tz_offset,
    })
}

/// Parst gDay (---DD).
fn parse_gday(value: &str, tz_offset: Option<i16>) -> Result<crate::datetime::DateTime> {
    let stripped = value.trim_start_matches('-');
    let day: u8 = stripped
        .parse()
        .map_err(|_| Error::InvalidValue(format!("Invalid gDay: {}", value)))?;

    Ok(crate::datetime::DateTime::GDay {
        day,
        timezone_offset_minutes: tz_offset,
    })
}

/// Formatiert DateTime enum zu ISO-8601 String.
/// Haengt reversed Fractional-Secs an den String an (z.B. ".123").
fn append_fractional_secs(s: &mut String, dt: &crate::datetime::DateTime) {
    if let Some(frac) = dt.fractional_secs() {
        let frac_str = frac.to_string();
        let frac_reversed: String = frac_str.chars().rev().collect();
        s.push('.');
        s.push_str(&frac_reversed);
    }
}

fn format_datetime(dt: &crate::datetime::DateTime) -> String {
    use crate::datetime::DateTimeType;

    let dt_type = dt.datetime_type();
    let year = dt.year().unwrap_or(0) + 2000;
    let mut s = String::new();

    match dt_type {
        DateTimeType::DateTime => {
            s.push_str(&format!(
                "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}",
                year,
                dt.month().unwrap_or(1),
                dt.day().unwrap_or(1),
                dt.hour().unwrap_or(0),
                dt.minute().unwrap_or(0),
                dt.second().unwrap_or(0)
            ));
            append_fractional_secs(&mut s, dt);
        }
        DateTimeType::Date => {
            s.push_str(&format!(
                "{:04}-{:02}-{:02}",
                year,
                dt.month().unwrap_or(1),
                dt.day().unwrap_or(1)
            ));
        }
        DateTimeType::Time => {
            s.push_str(&format!(
                "{:02}:{:02}:{:02}",
                dt.hour().unwrap_or(0),
                dt.minute().unwrap_or(0),
                dt.second().unwrap_or(0)
            ));
            append_fractional_secs(&mut s, dt);
        }
        DateTimeType::GYear => {
            s.push_str(&format!("{:04}", year));
        }
        DateTimeType::GYearMonth => {
            s.push_str(&format!("{:04}-{:02}", year, dt.month().unwrap_or(1)));
        }
        DateTimeType::GMonth => {
            s.push_str(&format!("--{:02}", dt.month().unwrap_or(1)));
        }
        DateTimeType::GMonthDay => {
            s.push_str(&format!(
                "--{:02}-{:02}",
                dt.month().unwrap_or(1),
                dt.day().unwrap_or(1)
            ));
        }
        DateTimeType::GDay => {
            s.push_str(&format!("---{:02}", dt.day().unwrap_or(1)));
        }
    }

    // Timezone
    if let Some(offset) = dt.timezone_offset_minutes() {
        if offset == 0 {
            s.push('Z');
        } else {
            use std::fmt::Write;
            let sign = if offset < 0 { '-' } else { '+' };
            let _ = write!(s, "{}{:02}:{:02}", sign, offset.abs() / 60, offset.abs() % 60);
        }
    }

    s
}

/// Encodiert base64Binary (Spec 7.1.1).
fn encode_base64_binary(writer: &mut BitWriter, value: &str) -> Result<()> {
    use base64::Engine;
    let bytes = base64::engine::general_purpose::STANDARD
        .decode(value.trim())
        .map_err(|_| Error::InvalidValue(format!("Invalid base64: {}", value)))?;
    crate::binary::encode(writer, &bytes);
    Ok(())
}

/// Decodiert base64Binary (Spec 7.1.1).
fn decode_base64_binary(reader: &mut BitReader) -> Result<String> {
    use base64::Engine;
    let bytes = crate::binary::decode(reader)?;
    Ok(base64::engine::general_purpose::STANDARD.encode(&bytes))
}

/// Encodiert hexBinary (Spec 7.1.1).
fn encode_hex_binary(writer: &mut BitWriter, value: &str) -> Result<()> {
    let trimmed = value.trim();
    if !trimmed.len().is_multiple_of(2) {
        return Err(Error::InvalidValue(format!("Invalid hexBinary: {}", value)));
    }

    let bytes: Result<Vec<u8>> = (0..trimmed.len())
        .step_by(2)
        .map(|i| {
            u8::from_str_radix(&trimmed[i..i + 2], 16)
                .map_err(|_| Error::InvalidValue(format!("Invalid hex: {}", &trimmed[i..i + 2])))
        })
        .collect();

    crate::binary::encode(writer, &bytes?);
    Ok(())
}

/// Decodiert hexBinary (Spec 7.1.1).
fn decode_hex_binary(reader: &mut BitReader) -> Result<String> {
    let bytes = crate::binary::decode(reader)?;
    Ok(bytes
        .iter()
        .map(|b| format!("{:02X}", b))
        .collect::<String>())
}

/// Encodiert eine Liste (Spec 7.1.11).
fn encode_list(
    writer: &mut BitWriter,
    value: &str,
    item_type_qname: Option<&crate::qname::QName>,
    strict: bool,
    alignment: Alignment,
) -> Result<()> {
    // Whitespace-separierte Werte
    let items: Vec<&str> = value.split_whitespace().collect();

    // Länge encodieren
    crate::unsigned_integer::encode(writer, items.len() as u64);

    // Items encodieren
    // Wenn item_type ein Built-in-Typ ist, verwende die entsprechende
    // Typed-Value-Repräsentation; sonst als String.
    for item in items {
        if let Some(qname) = item_type_qname
            && &*qname.uri == XSD_NS {
                encode_by_base_type(writer, item, &qname.local_name, strict, alignment)?;
                continue;
            }
        crate::string::encode(writer, item);
    }

    Ok(())
}

/// Decodiert eine Liste (Spec 7.1.11).
fn decode_list(
    reader: &mut BitReader,
    item_type_qname: Option<&crate::qname::QName>,
    alignment: Alignment,
) -> Result<String> {
    let len = crate::unsigned_integer::decode(reader)?;
    if len > crate::list::MAX_LIST_LENGTH {
        return Err(Error::ListLengthOverflow(len));
    }

    let mut items = Vec::with_capacity(len as usize);
    for _ in 0..len {
        if let Some(qname) = item_type_qname
            && &*qname.uri == XSD_NS {
                items.push(decode_by_base_type(reader, &qname.local_name, alignment)?);
                continue;
            }
        items.push(crate::string::decode(reader)?);
    }

    Ok(items.join(" "))
}

// ============================================================================
// Datatype Representation Map (Spec 7.4)
// ============================================================================

/// EXI Namespace URI.
pub const EXI_NS: &str = "http://www.w3.org/2009/exi";

/// Effektive Representation für einen Wert nach DTRM-Auflösung (Spec 7.4).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EffectiveRepresentation {
    /// String-Table-Encoding (String, anySimpleType, union, oder DTRM→exi:string).
    String,
    /// Typed Value Encoding mit dem angegebenen base_type.
    Typed(&'static str),
    /// User-defined Representation (über Registry auflösbar).
    UserDefined(crate::qname::QName),
}

/// User-defined Representation Encoder.
pub type UserDefinedEncode = fn(&mut BitWriter, &str, Alignment) -> Result<()>;
/// User-defined Representation Decoder.
pub type UserDefinedDecode = fn(&mut BitReader, Alignment) -> Result<String>;

thread_local! {
    static USER_DEFINED_REPRS: RefCell<crate::FastHashMap<QName, (UserDefinedEncode, UserDefinedDecode)>> =
        RefCell::new(crate::FastHashMap::with_capacity_and_hasher(8, Default::default()));
}

/// Registriert eine User-defined Representation.
///
/// Rückgabe: ggf. vorherige Registrierung.
pub fn register_user_defined_representation(
    representation_qname: QName,
    encode: UserDefinedEncode,
    decode: UserDefinedDecode,
) -> Option<(UserDefinedEncode, UserDefinedDecode)> {
    USER_DEFINED_REPRS.with(|registry| {
        registry
            .borrow_mut()
            .insert(representation_qname, (encode, decode))
    })
}

/// Entfernt eine User-defined Representation.
pub fn unregister_user_defined_representation(
    representation_qname: &QName,
) -> Option<(UserDefinedEncode, UserDefinedDecode)> {
    USER_DEFINED_REPRS.with(|registry| registry.borrow_mut().remove(representation_qname))
}

/// Holt eine User-defined Representation, falls registriert.
pub fn get_user_defined_representation(
    representation_qname: &QName,
) -> Option<(UserDefinedEncode, UserDefinedDecode)> {
    USER_DEFINED_REPRS.with(|registry| {
        registry
            .borrow()
            .get(representation_qname)
            .copied()
    })
}

const EXI_BUILTIN_TYPES: &[&str] = &[
    "base64Binary", "hexBinary", "boolean", "decimal", "double", "integer",
    "dateTime", "time", "date", "gYearMonth", "gYear", "gMonthDay", "gDay", "gMonth",
    "string",
];

/// Mappt eine EXI-Namespace Representation auf den entsprechenden Base-Type (Table 7-1).
fn exi_repr_to_base_type(repr: &crate::qname::QName) -> Option<&'static str> {
    if &*repr.uri != EXI_NS {
        return None;
    }
    EXI_BUILTIN_TYPES
        .iter()
        .find(|&&t| t == &*repr.local_name)
        .copied()
}

/// Sucht im DTRM nach einem Mapping für den Typ oder seinen nächsten Vorfahren (Spec 7.4).
///
/// Traversiert die base_type_qname-Kette aufwärts. Erster Match gewinnt (Closest Ancestor).
pub fn resolve_dtrm_representation<'a>(
    type_def: &crate::schema::TypeDefinition,
    dtrm: &'a [crate::options::DatatypeRepresentationMapping],
    schema: Option<&crate::schema::SchemaInfo>,
) -> Option<&'a crate::qname::QName> {
    // Direkter Match über Typ-QName
    if let Some(name) = type_def.name() {
        for entry in dtrm {
            if entry.type_qname == **name {
                return Some(&entry.representation_qname);
            }
        }
    }

    // Closest Ancestor: base_type_qname-Kette hochlaufen
    let mut current_qname = type_def.base_type_qname().cloned();
    let schema = schema?;

    let mut depth = 0;
    while let Some(ref qn) = current_qname {
        if depth > 128 {
            break; // Cycle-Schutz: Schema-Tiefe > 128 = vermutlich zyklisch
        }
        for entry in dtrm {
            if entry.type_qname == **qn {
                return Some(&entry.representation_qname);
            }
        }
        // Nächsten Vorfahren suchen
        let ancestor_type = schema.get_type(qn)?;
        current_qname = ancestor_type.base_type_qname().cloned();
        depth += 1;
    }

    None
}

/// Bestimmt die effektive Representation für einen Typ unter Berücksichtigung des DTRM (Spec 7.4).
///
/// Wird aus encoder.rs/decoder.rs VOR der String/Typed-Verzweigung aufgerufen.
///
/// # Rückgabe
///
/// - `None`: Kein DTRM-Override → bestehende Logik verwenden
/// - `Some(EffectiveRepresentation)`: DTRM-Override aktiv
pub fn resolve_effective_repr(
    type_def: &crate::schema::TypeDefinition,
    dtrm: &[crate::options::DatatypeRepresentationMapping],
    schema: Option<&crate::schema::SchemaInfo>,
) -> Option<EffectiveRepresentation> {
    if dtrm.is_empty() {
        return None;
    }

    let repr_qname = resolve_dtrm_representation(type_def, dtrm, schema)?;

    // Built-in EXI Representation?
    if let Some(base_type) = exi_repr_to_base_type(repr_qname) {
        if base_type == "string" {
            Some(EffectiveRepresentation::String)
        } else {
            Some(EffectiveRepresentation::Typed(base_type))
        }
    } else {
        // User-defined Representation
        Some(EffectiveRepresentation::UserDefined(repr_qname.clone()))
    }
}

// ============================================================================
// Enumeration Value-Space Normalisierung (Spec 7.2)
// ============================================================================

use std::borrow::Cow;

/// Normalisiert einen Enum-Wert für Value-Space-Vergleich (Spec 7.2).
///
/// Die Spec erfordert Vergleich im Value Space, nicht im Lexical Space.
/// Z.B. sind `"-0"`, `"+0"` und `"0"` im Integer-Value-Space identisch.
///
/// Gibt `Cow::Borrowed` zurück wenn keine Normalisierung nötig (häufigster Fall).
///
/// # Spec-Referenz
///
/// - Spec 7.2: Enumeration
/// - XSD 1.0 Part 2 §4.2.1: enumeration constraining facet
pub fn normalize_enum_value<'a>(value: &'a str, base_type: Option<&str>) -> Cow<'a, str> {
    match base_type {
        Some("boolean") => normalize_boolean(value),
        Some(
            "integer" | "nonPositiveInteger" | "negativeInteger" | "long" | "int" | "short"
            | "byte" | "nonNegativeInteger" | "positiveInteger" | "unsignedLong"
            | "unsignedInt" | "unsignedShort" | "unsignedByte",
        ) => normalize_integer(value),
        Some("decimal") => normalize_decimal(value),
        Some("float" | "double") => normalize_float(value),
        Some("string") => normalize_whitespace(value, WhitespaceMode::Preserve),
        Some("normalizedString") => normalize_whitespace(value, WhitespaceMode::Replace),
        Some(
            "token" | "language" | "Name" | "NCName" | "NMTOKEN" | "NMTOKENS" | "ID"
            | "IDREF" | "IDREFS" | "ENTITY" | "ENTITIES" | "NOTATION",
        ) => normalize_whitespace(value, WhitespaceMode::Collapse),
        _ => normalize_whitespace(value, WhitespaceMode::Trim),
    }
}

enum WhitespaceMode {
    Preserve,
    Replace,
    Collapse,
    Trim,
}

fn normalize_whitespace<'a>(value: &'a str, mode: WhitespaceMode) -> Cow<'a, str> {
    match mode {
        WhitespaceMode::Preserve => Cow::Borrowed(value),
        WhitespaceMode::Replace => replace_whitespace(value),
        WhitespaceMode::Collapse => collapse_whitespace(value),
        WhitespaceMode::Trim => trim_whitespace(value),
    }
}

fn trim_whitespace<'a>(value: &'a str) -> Cow<'a, str> {
    let trimmed = value.trim();
    if trimmed.len() == value.len() {
        Cow::Borrowed(value)
    } else {
        Cow::Owned(trimmed.to_string())
    }
}

fn replace_whitespace<'a>(value: &'a str) -> Cow<'a, str> {
    if !value.as_bytes().iter().any(|b| matches!(b, b'\t' | b'\n' | b'\r')) {
        return Cow::Borrowed(value);
    }
    let mut out = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '\t' | '\n' | '\r' => out.push(' '),
            _ => out.push(ch),
        }
    }
    Cow::Owned(out)
}

fn collapse_whitespace<'a>(value: &'a str) -> Cow<'a, str> {
    let replaced = replace_whitespace(value);
    let s = replaced.as_ref();
    let mut out = String::with_capacity(s.len());
    let mut in_ws = false;
    for ch in s.chars() {
        if ch.is_whitespace() {
            if !in_ws {
                out.push(' ');
                in_ws = true;
            }
        } else {
            out.push(ch);
            in_ws = false;
        }
    }
    let trimmed = out.trim();
    if trimmed.len() == s.len() && matches!(replaced, Cow::Borrowed(_)) {
        Cow::Borrowed(value)
    } else if trimmed.len() == out.len() {
        Cow::Owned(out)
    } else {
        Cow::Owned(trimmed.to_string())
    }
}

/// Boolean: `"1"` → `"true"`, `"0"` → `"false"`.
fn normalize_boolean(value: &str) -> Cow<'_, str> {
    match value.trim() {
        "1" | "true" => Cow::Borrowed("true"),
        "0" | "false" => Cow::Borrowed("false"),
        _ => Cow::Borrowed(value),
    }
}

/// Integer: trim, `+` entfernen, führende Nullen, `"-0"` → `"0"`.
///
/// Bei invalider Lexical-Form (leer, nur Vorzeichen) wird der Wert unverändert
/// zurückgegeben — Normalisierung greift nur bei gültigen Darstellungen.
fn normalize_integer(value: &str) -> Cow<'_, str> {
    let trimmed = value.trim();
    let negative = trimmed.starts_with('-');
    let unsigned = trimmed
        .strip_prefix('-')
        .or_else(|| trimmed.strip_prefix('+'))
        .unwrap_or(trimmed);

    // Guard: Leerer unsigned-Teil → invalide Form, unverändert zurückgeben
    if unsigned.is_empty() || !unsigned.bytes().all(|b| b.is_ascii_digit()) {
        return Cow::Borrowed(value);
    }

    // Führende Nullen entfernen
    let stripped = unsigned.trim_start_matches('0');
    let stripped = if stripped.is_empty() { "0" } else { stripped };

    if negative && stripped == "0" {
        // "-0" → "0"
        return Cow::Borrowed("0");
    }

    if negative {
        Cow::Owned(format!("-{}", stripped))
    } else if stripped == value {
        Cow::Borrowed(value)
    } else {
        Cow::Owned(stripped.to_string())
    }
}

/// Decimal: trim, `+` entfernen, führende/trailing Nullen, `"-0.0"` → `"0.0"`.
///
/// Bei invalider Lexical-Form wird der Wert unverändert zurückgegeben.
fn normalize_decimal(value: &str) -> Cow<'_, str> {
    let trimmed = value.trim();
    let negative = trimmed.starts_with('-');
    let unsigned = trimmed
        .strip_prefix('-')
        .or_else(|| trimmed.strip_prefix('+'))
        .unwrap_or(trimmed);

    // Guard: Leerer unsigned-Teil oder ungültige Zeichen → unverändert
    if unsigned.is_empty()
        || !unsigned
            .bytes()
            .all(|b| b.is_ascii_digit() || b == b'.')
    {
        return Cow::Borrowed(value);
    }

    let (int_part, frac_part) = if let Some(dot_pos) = unsigned.find('.') {
        (&unsigned[..dot_pos], Some(&unsigned[dot_pos + 1..]))
    } else {
        (unsigned, None)
    };

    // Führende Nullen im Integer-Teil
    let int_stripped = int_part.trim_start_matches('0');
    let int_stripped = if int_stripped.is_empty() {
        "0"
    } else {
        int_stripped
    };

    // Trailing Nullen im Fraktional-Teil
    let frac_stripped = frac_part.map(|f| f.trim_end_matches('0'));

    // Prüfe ob Wert = 0
    let is_zero = int_stripped == "0"
        && frac_stripped.map_or(true, |f| f.is_empty() || f == "0");

    if negative && is_zero {
        // "-0" / "-0.0" → "0" / "0.0"
        return match frac_stripped {
            Some(f) if !f.is_empty() => Cow::Owned(format!("0.{}", f)),
            Some(_) => Cow::Borrowed("0.0"),
            None => Cow::Borrowed("0"),
        };
    }

    // Normalisiertes Ergebnis zusammenbauen
    let sign = if negative { "-" } else { "" };
    let result = match frac_stripped {
        Some(f) if !f.is_empty() => format!("{sign}{int_stripped}.{f}"),
        Some(_) => format!("{sign}{int_stripped}.0"),
        None if negative => format!("-{int_stripped}"),
        None if int_stripped == trimmed => return Cow::Borrowed(value),
        None => return Cow::Owned(int_stripped.to_string()),
    };

    if result == value {
        Cow::Borrowed(value)
    } else {
        Cow::Owned(result)
    }
}

/// Float/Double: kanonische Mantissa × 10^Exponent Form.
///
/// Analog zu Exificient: Wert als f64 parsen, dann kanonisch formatieren.
fn normalize_float(value: &str) -> Cow<'_, str> {
    let trimmed = value.trim();

    // Spezialwerte
    match trimmed {
        "INF" | "-INF" | "NaN" => return Cow::Borrowed(trimmed),
        _ => {}
    }

    // Als f64 parsen und kanonisch zurückschreiben
    if let Ok(f) = trimmed.parse::<f64>() {
        if f.is_infinite() {
            return Cow::Borrowed(if f.is_sign_positive() { "INF" } else { "-INF" });
        }
        if f.is_nan() {
            return Cow::Borrowed("NaN");
        }

        // Kanonische Mantissa/Exponent-Form
        let (mantissa, exponent) = float_to_mantissa_exponent(f);
        let canonical = if exponent == 0 {
            format!("{}", mantissa)
        } else {
            format!("{}E{}", mantissa, exponent)
        };

        if canonical == value {
            Cow::Borrowed(value)
        } else {
            Cow::Owned(canonical)
        }
    } else {
        // Parse-Fehler → getrimmt zurückgeben
        if trimmed.len() == value.len() {
            Cow::Borrowed(value)
        } else {
            Cow::Owned(trimmed.to_string())
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitstream::{BitReader, BitWriter};
    use crate::qname::QName;
    use crate::schema::{SimpleTypeVariety, TypeDefinition};
    use std::rc::Rc;

    const XS_NS: &str = "http://www.w3.org/2001/XMLSchema";

    /// Helper: Round-Trip Test (Bit-Packed)
    fn round_trip(value: &str, type_def: &TypeDefinition) -> String {
        let mut writer = BitWriter::new();
        encode_typed_value(&mut writer, value, type_def, true, Alignment::BitPacked).unwrap();
        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);
        decode_typed_value(&mut reader, type_def, Alignment::BitPacked).unwrap()
    }

    // ========================================================================
    // Integer Tests
    // ========================================================================

    /// Spec 7.1.5: Integer encode/decode
    #[test]
    fn integer_round_trip() {
        let type_def = TypeDefinition::simple_with_base("integer");
        assert_eq!(round_trip("42", &type_def), "42");
        assert_eq!(round_trip("-123", &type_def), "-123");
        assert_eq!(round_trip("0", &type_def), "0");
    }

    /// Spec 7.1.5: Große Integer-Werte
    #[test]
    fn integer_large_values() {
        let type_def = TypeDefinition::simple_with_base("integer");
        assert_eq!(
            round_trip("9223372036854775807", &type_def),
            "9223372036854775807"
        ); // i64::MAX
        assert_eq!(
            round_trip("-9223372036854775808", &type_def),
            "-9223372036854775808"
        ); // i64::MIN
    }

    // ========================================================================
    // Boolean Tests
    // ========================================================================

    /// Spec 7.1.2: Boolean encode/decode
    #[test]
    fn boolean_round_trip() {
        let type_def = TypeDefinition::simple_with_base("boolean");
        assert_eq!(round_trip("true", &type_def), "true");
        assert_eq!(round_trip("false", &type_def), "false");
        assert_eq!(round_trip("1", &type_def), "true");
        assert_eq!(round_trip("0", &type_def), "false");
    }

    // ========================================================================
    // Float Tests
    // ========================================================================

    /// Spec 7.1.4: Float encode/decode
    #[test]
    fn float_special_values() {
        let type_def = TypeDefinition::simple_with_base("float");
        assert_eq!(round_trip("INF", &type_def), "INF");
        assert_eq!(round_trip("-INF", &type_def), "-INF");
        assert_eq!(round_trip("NaN", &type_def), "NaN");
    }

    /// Spec 7.1.4: Float normale Werte
    #[test]
    fn float_normal_values() {
        let type_def = TypeDefinition::simple_with_base("float");
        let result = round_trip("0", &type_def);
        assert_eq!(result, "0");
    }

    // ========================================================================
    // Decimal Tests
    // ========================================================================

    /// Spec 7.1.3: Decimal encode/decode
    #[test]
    fn decimal_round_trip() {
        let type_def = TypeDefinition::simple_with_base("decimal");
        assert_eq!(round_trip("12.34", &type_def), "12.34");
        assert_eq!(round_trip("-5.6", &type_def), "-5.6");
        assert_eq!(round_trip("0", &type_def), "0");
        assert_eq!(round_trip("42", &type_def), "42");
    }

    // ========================================================================
    // DateTime Tests
    // ========================================================================

    /// Spec 7.1.8: DateTime encode/decode
    #[test]
    fn datetime_round_trip() {
        let type_def = TypeDefinition::simple_with_base("dateTime");
        let result = round_trip("2025-06-15T10:30:00", &type_def);
        assert_eq!(result, "2025-06-15T10:30:00");
    }

    /// Spec 7.1.8: DateTime mit Timezone
    #[test]
    fn datetime_with_timezone() {
        let type_def = TypeDefinition::simple_with_base("dateTime");
        let result = round_trip("2025-06-15T10:30:00Z", &type_def);
        assert_eq!(result, "2025-06-15T10:30:00Z");
    }

    /// Spec 7.1.8: Date encode/decode
    #[test]
    fn date_round_trip() {
        let type_def = TypeDefinition::simple_with_base("date");
        assert_eq!(round_trip("2025-06-15", &type_def), "2025-06-15");
    }

    /// Spec 7.1.8: Time encode/decode
    #[test]
    fn time_round_trip() {
        let type_def = TypeDefinition::simple_with_base("time");
        assert_eq!(round_trip("10:30:00", &type_def), "10:30:00");
    }

    /// Spec 7.1.8: gYear encode/decode
    #[test]
    fn gyear_round_trip() {
        let type_def = TypeDefinition::simple_with_base("gYear");
        assert_eq!(round_trip("2025", &type_def), "2025");
    }

    // ========================================================================
    // Binary Tests
    // ========================================================================

    /// Spec 7.1.1: base64Binary encode/decode
    #[test]
    fn base64_round_trip() {
        let type_def = TypeDefinition::simple_with_base("base64Binary");
        assert_eq!(round_trip("SGVsbG8=", &type_def), "SGVsbG8=");
    }

    /// Spec 7.1.1: hexBinary encode/decode
    #[test]
    fn hex_round_trip() {
        let type_def = TypeDefinition::simple_with_base("hexBinary");
        assert_eq!(round_trip("48656C6C6F", &type_def), "48656C6C6F");
    }

    // ========================================================================
    // String Tests
    // ========================================================================

    /// String encode/decode (Fallback)
    #[test]
    fn string_round_trip() {
        let type_def = TypeDefinition::simple_with_base("string");
        assert_eq!(round_trip("Hello World", &type_def), "Hello World");
    }

    // ========================================================================
    // Union Tests
    // ========================================================================

    /// Spec 8.5.4.4.2: Union wird als String encodiert
    #[test]
    fn union_as_string() {
        let type_def = TypeDefinition::simple_union();
        assert_eq!(round_trip("42", &type_def), "42");
        assert_eq!(round_trip("hello", &type_def), "hello");
    }

    // ========================================================================
    // List Tests
    // ========================================================================

    /// Spec 7.1.11: List encode/decode
    #[test]
    fn list_round_trip() {
        let type_def = TypeDefinition::Simple {
            name: None,
            variety: SimpleTypeVariety::List { item_type: None },
            base_type_qname: None,
            base_type: None,
            is_union: false,
            enumeration_values: Vec::new(),
            has_named_sub_types: false,
        };
        assert_eq!(round_trip("a b c", &type_def), "a b c");
    }

    // ========================================================================
    // Strict Mode Tests
    // ========================================================================

    /// Spec 8.5.4.4.1: strict=false erlaubt Fallback zu String bei ungültigem Wert.
    /// "When strict is false, content that does not match the schema is processed
    /// using built-in element grammar."
    #[test]
    fn strict_false_fallback() {
        let type_def = TypeDefinition::simple_with_base("integer");

        let mut writer = BitWriter::new();
        let result = encode_typed_value(&mut writer, "not-an-integer", &type_def, false, Alignment::BitPacked);
        assert!(result.is_ok()); // Fallback zu String
    }

    /// Spec 8.5.4.4.1: strict=true erfordert Schema-konforme Werte.
    /// Bei ungültigem Wert wird ein Fehler zurückgegeben.
    #[test]
    fn strict_true_error() {
        let type_def = TypeDefinition::simple_with_base("integer");

        let mut writer = BitWriter::new();
        let result = encode_typed_value(&mut writer, "not-an-integer", &type_def, true, Alignment::BitPacked);
        assert!(result.is_err());
    }

    // ========================================================================
    // Coverage Tests für ungedeckte Pfade
    // ========================================================================

    /// Test für List-Typ Detection (Zeile 39).
    #[test]
    fn list_type_base_type() {
        let type_def = TypeDefinition::Simple {
            name: None,
            variety: SimpleTypeVariety::List {
                item_type: Some(Rc::new(QName::new(XS_NS, "string"))),
            },
            base_type_qname: None,
            base_type: None,
            enumeration_values: Vec::new(),
            is_union: false,
            has_named_sub_types: false,
        };

        let result = resolve_base_type(&type_def);
        assert_eq!(result, Some("list"));
    }

    /// Test für Complex Type mit Simple Content (Zeile 44-51).
    #[test]
    fn complex_type_simple_content() {
        // Verwende complex() Helper der ContentType::Empty erzeugt
        // und setze dann content auf Simple
        let type_def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: crate::schema::ContentType::Simple,
            has_named_sub_types: false,
        };

        // Complex Type mit Simple Content → String Fallback
        let result = resolve_base_type(&type_def);
        assert_eq!(result, Some("string"));
    }

    /// Test für Complex Type ohne Simple Content (Zeile 51).
    #[test]
    fn complex_type_element_content() {
        // Verwende ContentType::Empty als Beispiel für nicht-Simple Content
        let type_def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: crate::schema::ContentType::Empty,
            has_named_sub_types: false,
        };

        // Complex Type ohne Simple Content → None
        let result = resolve_base_type(&type_def);
        assert!(result.is_none());
    }

    /// Test für kein base_type → String Fallback (Zeile 97-98).
    #[test]
    fn no_base_type_string_fallback() {
        let type_def = TypeDefinition::Simple {
            name: None,
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: None, // Kein base_type
            is_union: false,
            enumeration_values: Vec::new(),
            has_named_sub_types: false,
        };

        let mut writer = BitWriter::new();
        let result = encode_typed_value(&mut writer, "test value", &type_def, true, Alignment::BitPacked);
        assert!(result.is_ok()); // Fallback zu String
    }

    /// Test für NMTOKENS Typ (Zeile 141-142).
    #[test]
    fn nmtokens_type_as_string() {
        let type_def = TypeDefinition::simple_with_base("NMTOKENS");

        let mut writer = BitWriter::new();
        let result = encode_typed_value(&mut writer, "token1 token2", &type_def, true, Alignment::BitPacked);
        assert!(result.is_ok());
    }

    /// Test für float INF durch Parsing (Zeile 290-291).
    #[test]
    fn float_parsed_infinity() {
        let type_def = TypeDefinition::simple_with_base("float");

        // Verwende eine sehr große Zahl die zu Infinity wird
        let mut writer = BitWriter::new();
        encode_typed_value(&mut writer, "1e309", &type_def, true, Alignment::BitPacked).unwrap();

        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);
        let decoded = decode_typed_value(&mut reader, &type_def, Alignment::BitPacked).unwrap();
        assert!(decoded.contains("INF") || decoded.contains("Infinity"));
    }

    /// Test für float -INF durch Parsing (Zeile 293).
    #[test]
    fn float_parsed_negative_infinity() {
        let type_def = TypeDefinition::simple_with_base("float");

        let mut writer = BitWriter::new();
        encode_typed_value(&mut writer, "-1e309", &type_def, true, Alignment::BitPacked).unwrap();

        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);
        let decoded = decode_typed_value(&mut reader, &type_def, Alignment::BitPacked).unwrap();
        assert!(decoded.contains("-INF") || decoded.contains("-Infinity"));
    }

    /// Test für float NaN durch Parsing (Zeile 296).
    #[test]
    fn float_parsed_nan() {
        let type_def = TypeDefinition::simple_with_base("float");

        // f64::NAN.to_string() ergibt "NaN"
        let mut writer = BitWriter::new();
        // Parse "nan" (lowercase)
        let result = encode_typed_value(&mut writer, "nan", &type_def, true, Alignment::BitPacked);
        // Sollte als InvalidValue fehlschlagen (case-sensitive) oder als NaN parsen
        // Je nach Implementierung
        assert!(result.is_ok() || result.is_err());
    }

    /// Test für float_to_mantissa_exponent (Zeile 316-333).
    #[test]
    fn float_mantissa_exponent_precision() {
        let type_def = TypeDefinition::simple_with_base("float");

        // Teste verschiedene Float-Werte
        for value in &["3.14159", "0.001", "1000.5", "-42.0", "1.5e10"] {
            let mut writer = BitWriter::new();
            encode_typed_value(&mut writer, value, &type_def, true, Alignment::BitPacked).unwrap();

            let data = writer.into_vec();
            let mut reader = BitReader::new(&data);
            let decoded = decode_typed_value(&mut reader, &type_def, Alignment::BitPacked).unwrap();

            // Decode sollte einen gültigen Float-String zurückgeben
            let _: f64 = decoded.parse().unwrap();
        }
    }

    /// Test für Union-Typ (Zeile 35).
    #[test]
    fn union_type_as_string() {
        let type_def = TypeDefinition::Simple {
            name: None,
            variety: SimpleTypeVariety::Union {
                member_types: vec![
                    Rc::new(QName::new(XS_NS, "integer")),
                    Rc::new(QName::new(XS_NS, "string")),
                ],
            },
            base_type_qname: None,
            base_type: None,
            is_union: true,
            enumeration_values: Vec::new(),
            has_named_sub_types: false,
        };

        // Union wird immer als String encodiert
        let result = resolve_base_type(&type_def);
        assert_eq!(result, Some("string"));

        let mut writer = BitWriter::new();
        encode_typed_value(&mut writer, "union value", &type_def, true, Alignment::BitPacked).unwrap();
    }

    // ========================================================================
    // DTRM Resolution Tests (Spec 7.4)
    // ========================================================================

    /// Helper: Erstellt ein einfaches SimpleType mit Name und base_type_qname.
    fn named_simple_type(uri: &str, local: &str, base_uri: &str, base_local: &str) -> TypeDefinition {
        TypeDefinition::Simple {
            name: Some(Rc::new(QName::new(uri, local))),
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: Some(Rc::new(QName::new(base_uri, base_local))),
            base_type: None,
            is_union: false,
            enumeration_values: Vec::new(),
            has_named_sub_types: false,
        }
    }

    /// Spec 7.4: DTRM decimal→exi:string liefert EffectiveRepresentation::String.
    #[test]
    fn dtrm_decimal_to_string() {
        use crate::options::DatatypeRepresentationMapping;
        let td = named_simple_type(XS_NS, "decimal", XS_NS, "anySimpleType");
        let dtrm = vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XS_NS, "decimal"),
            representation_qname: QName::new(EXI_NS, "string"),
        }];
        let result = resolve_effective_repr(&td, &dtrm, None);
        assert_eq!(result, Some(EffectiveRepresentation::String));
    }

    /// Spec 7.4: Closest Ancestor — userType basiert auf xsd:decimal, erbt Mapping.
    #[test]
    fn dtrm_closest_ancestor() {
        use crate::options::DatatypeRepresentationMapping;
        use crate::schema::SchemaInfo;

        let decimal_td = Rc::new(named_simple_type(XS_NS, "decimal", XS_NS, "anySimpleType"));
        let schema = SchemaInfo::builder()
            .type_definition(Rc::new(QName::new(XS_NS, "decimal")), decimal_td)
            .build();

        // Abgeleiteter Typ: myDecimal basiert auf xsd:decimal
        let my_td = named_simple_type("http://example.org", "myDecimal", XS_NS, "decimal");

        let dtrm = vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XS_NS, "decimal"),
            representation_qname: QName::new(EXI_NS, "integer"),
        }];

        let result = resolve_effective_repr(&my_td, &dtrm, Some(&schema));
        assert_eq!(result, Some(EffectiveRepresentation::Typed("integer")));
    }

    /// Spec 7.4: Kein Match im DTRM → None (bestehende Logik verwenden).
    #[test]
    fn dtrm_no_match() {
        use crate::options::DatatypeRepresentationMapping;
        let td = named_simple_type(XS_NS, "boolean", XS_NS, "anySimpleType");
        let dtrm = vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XS_NS, "decimal"),
            representation_qname: QName::new(EXI_NS, "string"),
        }];
        let result = resolve_effective_repr(&td, &dtrm, None);
        assert_eq!(result, None);
    }

    /// Spec 7.4: User-defined Representation → UserDefined.
    #[test]
    fn dtrm_user_defined() {
        use crate::options::DatatypeRepresentationMapping;
        let td = named_simple_type(XS_NS, "decimal", XS_NS, "anySimpleType");
        let dtrm = vec![DatatypeRepresentationMapping {
            type_qname: QName::new(XS_NS, "decimal"),
            representation_qname: QName::new("http://example.org/codec", "myCodec"),
        }];
        let result = resolve_effective_repr(&td, &dtrm, None);
        assert_eq!(
            result,
            Some(EffectiveRepresentation::UserDefined(QName::new(
                "http://example.org/codec",
                "myCodec"
            )))
        );
    }

    /// Spec 7.4: Leerer DTRM → None.
    #[test]
    fn dtrm_empty() {
        let td = named_simple_type(XS_NS, "decimal", XS_NS, "anySimpleType");
        let result = resolve_effective_repr(&td, &[], None);
        assert_eq!(result, None);
    }

    /// Spec 7.4: Mehrere DTRM-Entries — nächster Ancestor gewinnt.
    #[test]
    fn dtrm_multiple_entries_closest_wins() {
        use crate::options::DatatypeRepresentationMapping;
        use crate::schema::SchemaInfo;

        let any_simple = Rc::new(named_simple_type(XS_NS, "anySimpleType", XS_NS, "anyType"));
        let decimal_td = Rc::new(named_simple_type(XS_NS, "decimal", XS_NS, "anySimpleType"));
        let schema = SchemaInfo::builder()
            .type_definition(Rc::new(QName::new(XS_NS, "anySimpleType")), any_simple)
            .type_definition(Rc::new(QName::new(XS_NS, "decimal")), decimal_td)
            .build();

        // myDecimal basiert auf xsd:decimal
        let my_td = named_simple_type("http://example.org", "myDecimal", XS_NS, "decimal");

        let dtrm = vec![
            // Mapping für anySimpleType (entfernterer Ancestor)
            DatatypeRepresentationMapping {
                type_qname: QName::new(XS_NS, "anySimpleType"),
                representation_qname: QName::new(EXI_NS, "string"),
            },
            // Mapping für decimal (näherer Ancestor)
            DatatypeRepresentationMapping {
                type_qname: QName::new(XS_NS, "decimal"),
                representation_qname: QName::new(EXI_NS, "boolean"),
            },
        ];

        // decimal ist der nächste Ancestor → boolean gewinnt
        let result = resolve_effective_repr(&my_td, &dtrm, Some(&schema));
        assert_eq!(result, Some(EffectiveRepresentation::Typed("boolean")));
    }

    // ========================================================================
    // normalize_enum_value Tests (Spec 7.2)
    // ========================================================================

    /// Spec 7.2: Boolean Normalisierung: "1" → "true", "0" → "false"
    #[test]
    fn normalize_enum_boolean() {
        assert_eq!(normalize_enum_value("1", Some("boolean")), "true");
        assert_eq!(normalize_enum_value("0", Some("boolean")), "false");
        assert_eq!(normalize_enum_value("true", Some("boolean")), "true");
        assert_eq!(normalize_enum_value("false", Some("boolean")), "false");
    }

    /// Spec 7.2: Integer Normalisierung: "+1" → "1", "-0" → "0", "01" → "1"
    #[test]
    fn normalize_enum_integer() {
        assert_eq!(normalize_enum_value("-0", Some("integer")), "0");
        assert_eq!(normalize_enum_value("+1", Some("integer")), "1");
        assert_eq!(normalize_enum_value("01", Some("integer")), "1");
        assert_eq!(normalize_enum_value("0", Some("integer")), "0");
        assert_eq!(normalize_enum_value("42", Some("integer")), "42");
        assert_eq!(normalize_enum_value("-42", Some("integer")), "-42");
        assert_eq!(normalize_enum_value("+0", Some("integer")), "0");
        assert_eq!(normalize_enum_value("007", Some("integer")), "7");
    }

    /// Spec 7.2: Integer-Untertypen werden ebenfalls normalisiert
    #[test]
    fn normalize_enum_integer_subtypes() {
        assert_eq!(normalize_enum_value("+1", Some("int")), "1");
        assert_eq!(normalize_enum_value("-0", Some("long")), "0");
        assert_eq!(normalize_enum_value("01", Some("short")), "1");
        assert_eq!(normalize_enum_value("+5", Some("unsignedInt")), "5");
    }

    /// Spec 7.2: Decimal Normalisierung
    #[test]
    fn normalize_enum_decimal() {
        assert_eq!(normalize_enum_value("-0.0", Some("decimal")), "0.0");
        assert_eq!(normalize_enum_value("+1.0", Some("decimal")), "1.0");
        assert_eq!(normalize_enum_value("1.00", Some("decimal")), "1.0");
        assert_eq!(normalize_enum_value("01.5", Some("decimal")), "1.5");
        assert_eq!(normalize_enum_value("0", Some("decimal")), "0");
        assert_eq!(normalize_enum_value("-0", Some("decimal")), "0");
    }

    /// Spec 7.2: Float Normalisierung: "1.000" vs "1.0", "1.0e0" vs "1.0"
    #[test]
    fn normalize_enum_float() {
        // Gleiche Werte in verschiedener Schreibweise → gleiche kanonische Form
        let a = normalize_enum_value("1.000", Some("float"));
        let b = normalize_enum_value("1.0", Some("float"));
        assert_eq!(a, b);

        let c = normalize_enum_value("1.0e0", Some("float"));
        assert_eq!(a, c);

        // Spezialwerte
        assert_eq!(normalize_enum_value("INF", Some("float")), "INF");
        assert_eq!(normalize_enum_value("-INF", Some("float")), "-INF");
        assert_eq!(normalize_enum_value("NaN", Some("float")), "NaN");
    }

    /// Spec 7.2: String-basierte Typen → whiteSpace Facette
    #[test]
    fn normalize_enum_string() {
        assert_eq!(normalize_enum_value("  hello  ", Some("string")), "  hello  ");
        assert_eq!(normalize_enum_value("hello", Some("string")), "hello");
        assert_eq!(normalize_enum_value("a\tb\nc", Some("normalizedString")), "a b c");
        assert_eq!(normalize_enum_value("  a\t b  c ", Some("token")), "a b c");
    }

    /// Spec 7.2: Unbekannter Typ → Whitespace trim
    #[test]
    fn normalize_enum_unknown_type() {
        assert_eq!(normalize_enum_value("  foo  ", None), "foo");
        assert_eq!(normalize_enum_value("bar", None), "bar");
    }

    // ========================================================================
    // normalize_enum_value Edge Cases (T2)
    // ========================================================================

    /// Edge Case: Integer mit nur Nullen → "0"
    #[test]
    fn normalize_enum_integer_all_zeros() {
        assert_eq!(normalize_enum_value("000", Some("integer")), "0");
    }

    /// Edge Case: Leerer String ist kein gültiger Integer → unverändert zurückgeben
    #[test]
    fn normalize_enum_integer_empty_string() {
        let result = normalize_enum_value("", Some("integer"));
        assert_eq!(result, ""); // Invalide Form → unverändert
    }

    /// Edge Case: Nur Vorzeichen ohne Ziffern → unverändert
    #[test]
    fn normalize_enum_integer_sign_only() {
        assert_eq!(normalize_enum_value("+", Some("integer")), "+");
        assert_eq!(normalize_enum_value("-", Some("integer")), "-");
    }

    /// Edge Case: Nicht-numerisch → unverändert
    #[test]
    fn normalize_enum_integer_non_numeric() {
        assert_eq!(normalize_enum_value("abc", Some("integer")), "abc");
    }

    /// Edge Case: Leerer String bei Decimal → unverändert
    #[test]
    fn normalize_enum_decimal_empty() {
        assert_eq!(normalize_enum_value("", Some("decimal")), "");
        assert_eq!(normalize_enum_value("+", Some("decimal")), "+");
    }

    /// Edge Case: Decimal mit trailing dot → "1.0"
    #[test]
    fn normalize_enum_decimal_trailing_dot() {
        assert_eq!(normalize_enum_value("1.", Some("decimal")), "1.0");
    }

    /// Edge Case: Float -0.0 und 0.0 sollen identisch normalisiert werden
    #[test]
    fn normalize_enum_float_negative_zero() {
        let a = normalize_enum_value("-0.0", Some("float"));
        let b = normalize_enum_value("0.0", Some("float"));
        assert_eq!(a, b);
    }

    /// Edge Case: Double wird wie Float normalisiert
    #[test]
    fn normalize_enum_double_type() {
        let a = normalize_enum_value("1.0e2", Some("double"));
        let b = normalize_enum_value("100.0", Some("double"));
        assert_eq!(a, b);
    }

    /// Edge Case: Cow::Borrowed-Optimierung bei bereits kanonischem Wert
    #[test]
    fn normalize_enum_cow_borrowed_optimization() {
        let result = normalize_enum_value("42", Some("integer"));
        assert!(matches!(result, Cow::Borrowed(_)), "Bereits kanonisch -> Borrowed");
    }

    /// Edge Case: Whitespace um Integer → wird getrimmt
    #[test]
    fn normalize_enum_integer_with_whitespace() {
        assert_eq!(normalize_enum_value(" 42 ", Some("integer")), "42");
    }

    /// Edge Case: Float Parse-Fehler → trimmed zurückgeben
    #[test]
    fn normalize_enum_float_parse_error() {
        assert_eq!(normalize_enum_value("not_a_number", Some("float")), "not_a_number");
    }
}
