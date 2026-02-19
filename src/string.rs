//! String encoding (Spec 7.1.10).
//!
//! When no restricted character set is in effect, the string is encoded as a
//! length-prefixed sequence of Unicode code points. The length (number of
//! characters) is encoded as an Unsigned Integer (Spec 7.1.6), followed by
//! each character's Unicode code point encoded as an Unsigned Integer.
//!
//! Restricted Character Sets (Spec 7.1.10.1) and String Tables (Spec 7.3)
//! are not handled here.

use crate::bitstream::{BitReader, BitWriter};
use crate::{Error, Result, unsigned_integer};

/// Versucht `len` Unicode Code Points als ASCII-Bytes zu dekodieren (Fast-Path).
///
/// Gibt `Some(String)` zurück wenn byte-aligned und alle `len` Bytes MSB=0 sind
/// (also Single-Byte Unsigned Integers = ASCII). Avanziert den Lesezeiger.
/// Gibt `None` zurück wenn der Fast-Path nicht anwendbar ist (nicht aligned,
/// Bytes >= 128, oder nicht genug Daten).
pub(crate) fn try_decode_ascii_fast(reader: &mut BitReader, len: usize) -> Option<String> {
    let bytes = reader.peek_aligned_bytes(len)?;
    if bytes.iter().all(|&b| b & 0x80 == 0) {
        // Alle Bytes < 128 -> Single-Byte Unsigned Integers -> ASCII -> valid UTF-8
        let s = std::str::from_utf8(bytes)
            .expect("ASCII-Bytes sind valides UTF-8")
            .to_string();
        reader.skip_aligned_bytes(len);
        Some(s)
    } else {
        None
    }
}

/// Encodes a string as a length-prefixed sequence of Unicode code points (Spec 7.1.10).
///
/// Uses the default (non-restricted) encoding path. The length is the number of
/// characters (not bytes), encoded as an Unsigned Integer. Each character is
/// encoded as its Unicode code point via Unsigned Integer.
///
/// ASCII-Fast-Path: Wenn der String nur ASCII enthält (Codepoints 0-127), wird
/// die Länge direkt aus `str::len()` genommen und die Bytes per Bulk-Write
/// geschrieben. Jeder ASCII-Codepoint erzeugt in Unsigned Integer genau 1 Byte
/// ohne Continuation-Bit — identisch mit dem Roh-Byte.
pub fn encode(writer: &mut BitWriter, value: &str) {
    if value.is_ascii() {
        // ASCII: len() == char count, Bulk-Write statt per-Codepoint
        unsigned_integer::encode(writer, value.len() as u64);
        writer.write_bytes_aligned(value.as_bytes());
    } else {
        encode_non_ascii(writer, value, 0);
    }
}

/// Non-ASCII Encoding mit optionalem Length-Offset (Spec 7.1.10).
///
/// Optimiert: Für kleine Strings ein Pass mit Buffer, sonst zwei Passes ohne Zusatz-Vec.
pub(crate) fn encode_non_ascii(writer: &mut BitWriter, value: &str, offset: u64) {
    const MAX_BUFFERED_CODEPOINTS: usize = 4096;
    let mut buf: Vec<u32> = Vec::new();
    let mut count: u64 = 0;
    let mut buffered = true;

    for ch in value.chars() {
        count += 1;
        if buffered {
            if buf.len() < MAX_BUFFERED_CODEPOINTS {
                buf.push(ch as u32);
            } else {
                buffered = false;
            }
        }
    }

    unsigned_integer::encode(writer, count + offset);

    if buffered {
        for cp in buf {
            unsigned_integer::encode(writer, cp as u64);
        }
    } else {
        for ch in value.chars() {
            unsigned_integer::encode(writer, ch as u64);
        }
    }
}

/// Decodes a string from a length-prefixed sequence of Unicode code points (Spec 7.1.10).
///
/// Returns [`Error::InvalidCodePoint`] if a decoded code point is a surrogate
/// (U+D800..U+DFFF) or exceeds U+10FFFF.
///
/// ASCII-Fast-Path: Wenn byte-aligned und alle nächsten `len` Bytes MSB=0 haben,
/// sind es Single-Byte Unsigned Integers (= ASCII Codepoints). Direkte Konvertierung
/// zu String ohne per-Codepoint Decode.
pub fn decode(reader: &mut BitReader) -> Result<String> {
    let len = unsigned_integer::decode(reader)?;
    if let Ok(len_usize) = usize::try_from(len) {
        if let Some(s) = try_decode_ascii_fast(reader, len_usize) {
            return Ok(s);
        }
    }
    // Fallback: Codepoint-by-Codepoint
    let cap = usize::try_from(len).unwrap_or(0).min(16 * 1024 * 1024);
    let mut s = String::with_capacity(cap);
    for _ in 0..len {
        let cp = unsigned_integer::decode(reader)?;
        let ch = u32::try_from(cp)
            .ok()
            .and_then(char::from_u32)
            .ok_or_else(|| Error::InvalidCodePoint(cp))?;
        s.push(ch);
    }
    Ok(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trip(value: &str) -> String {
        let mut w = BitWriter::new();
        encode(&mut w, value);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r).unwrap()
    }

    /// Spec 7.1.10: empty string — length=0, no characters
    #[test]
    fn empty_string() {
        assert_eq!(round_trip(""), "");
        let mut w = BitWriter::new();
        encode(&mut w, "");
        let data = w.into_vec();
        // length=0 encoded as single byte 0x00
        assert_eq!(data, vec![0x00]);
    }

    /// Spec 7.1.10: ASCII string round-trip
    #[test]
    fn ascii_string() {
        assert_eq!(round_trip("hello"), "hello");
    }

    /// Spec 7.1.10: length is character count, not byte count
    #[test]
    fn length_is_char_count() {
        let mut w = BitWriter::new();
        encode(&mut w, "aé");
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        // length should be 2 (two characters), not 3 (byte length of UTF-8)
        let len = unsigned_integer::decode(&mut r).unwrap();
        assert_eq!(len, 2);
    }

    /// Spec 7.1.10: Unicode emoji (U+1F600, outside BMP)
    #[test]
    fn unicode_emoji() {
        assert_eq!(round_trip("😀"), "😀");
    }

    /// Spec 7.1.10: CJK characters
    #[test]
    fn unicode_cjk() {
        assert_eq!(round_trip("漢字"), "漢字");
    }

    /// Spec 7.1.10: mixed ASCII and multi-byte Unicode
    #[test]
    fn mixed_unicode() {
        let s = "Hello, 世界! 🌍";
        assert_eq!(round_trip(s), s);
    }

    /// Spec 7.1.10: single character string
    #[test]
    fn single_char() {
        assert_eq!(round_trip("A"), "A");
    }

    /// Spec 7.1.10: decode EOF on length
    #[test]
    fn decode_eof_on_length() {
        let mut r = BitReader::new(&[]);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.10: decode EOF on first character
    #[test]
    fn decode_eof_on_char() {
        // Encode length=1 but provide no character data
        let mut w = BitWriter::new();
        unsigned_integer::encode(&mut w, 1);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.10: decode EOF mid-string (length=3 but only 2 chars provided)
    #[test]
    fn decode_eof_mid_string() {
        let mut w = BitWriter::new();
        unsigned_integer::encode(&mut w, 3);
        unsigned_integer::encode(&mut w, 'A' as u64);
        unsigned_integer::encode(&mut w, 'B' as u64);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.10: surrogate code point (U+D800) → InvalidCodePoint
    #[test]
    fn decode_surrogate_range_start() {
        let mut w = BitWriter::new();
        unsigned_integer::encode(&mut w, 1); // length=1
        unsigned_integer::encode(&mut w, 0xD800); // surrogate
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::InvalidCodePoint(0xD800));
    }

    /// Spec 7.1.10: surrogate code point (U+DFFF) → InvalidCodePoint
    #[test]
    fn decode_surrogate_range_end() {
        let mut w = BitWriter::new();
        unsigned_integer::encode(&mut w, 1);
        unsigned_integer::encode(&mut w, 0xDFFF);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::InvalidCodePoint(0xDFFF));
    }

    /// Spec 7.1.10: code point > U+10FFFF → InvalidCodePoint
    #[test]
    fn decode_codepoint_too_large() {
        let mut w = BitWriter::new();
        unsigned_integer::encode(&mut w, 1);
        unsigned_integer::encode(&mut w, 0x110000);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode(&mut r).unwrap_err(),
            Error::InvalidCodePoint(0x110000)
        );
    }

    /// Spec 7.1.10: very large code point value → InvalidCodePoint
    #[test]
    fn decode_codepoint_u64_max() {
        let mut w = BitWriter::new();
        unsigned_integer::encode(&mut w, 1);
        unsigned_integer::encode(&mut w, u64::MAX);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode(&mut r).unwrap_err(),
            Error::InvalidCodePoint(u64::MAX)
        );
    }

    /// Spec 7.1.10: sequential strings in a stream
    #[test]
    fn sequential_strings() {
        let mut w = BitWriter::new();
        encode(&mut w, "abc");
        encode(&mut w, "");
        encode(&mut w, "日本");
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap(), "abc");
        assert_eq!(decode(&mut r).unwrap(), "");
        assert_eq!(decode(&mut r).unwrap(), "日本");
    }

    /// Spec 7.1.10: boundary code points (U+0000, U+FFFF, U+10000, U+10FFFF)
    #[test]
    fn boundary_codepoints() {
        // NUL, last BMP, first supplementary, last valid
        let s: String = ['\0', '\u{FFFF}', '\u{10000}', '\u{10FFFF}']
            .iter()
            .collect();
        assert_eq!(round_trip(&s), s);
    }

    // --- ASCII-Fast-Path Tests ---

    /// ASCII-Fast-Path: Encode erzeugt identische Bytes wie per-Codepoint-Pfad
    #[test]
    fn ascii_fast_path_byte_identical() {
        // Manuell per-Codepoint encodieren (Slow-Path simulieren)
        let value = "Hello, World!";
        let mut slow = BitWriter::new();
        unsigned_integer::encode(&mut slow, value.len() as u64);
        for ch in value.chars() {
            unsigned_integer::encode(&mut slow, ch as u64);
        }
        // Fast-Path via encode()
        let mut fast = BitWriter::new();
        encode(&mut fast, value);
        assert_eq!(fast.into_vec(), slow.into_vec());
    }

    /// ASCII-Fast-Path: langer ASCII-String (> 127 Zeichen, 2-Byte Länge)
    #[test]
    fn ascii_fast_path_long_string() {
        let s: String = (0..200).map(|i| (b'A' + (i % 26) as u8) as char).collect();
        assert_eq!(round_trip(&s), s);
    }

    /// ASCII-Fast-Path: NUL-Bytes (0x00) sind gültige ASCII-Codepoints
    #[test]
    fn ascii_fast_path_with_nul() {
        let s = "a\0b\0c";
        assert_eq!(round_trip(s), s);
    }

    /// ASCII-Fast-Path: DEL (0x7F) ist der höchste ASCII-Codepoint
    #[test]
    fn ascii_fast_path_del_boundary() {
        let s = "\x7F";
        assert_eq!(round_trip(s), s);
        // Byte-identisch prüfen
        let mut w = BitWriter::new();
        encode(&mut w, s);
        let data = w.into_vec();
        // Länge=1 (0x01), Codepoint=127 (0x7F)
        assert_eq!(data, vec![0x01, 0x7F]);
    }

    /// Kein Fast-Path für Strings mit Nicht-ASCII-Zeichen (Codepoint >= 128)
    #[test]
    fn no_fast_path_for_non_ascii() {
        // "é" = U+00E9, braucht 2 Bytes in Unsigned Integer
        let s = "café";
        assert_eq!(round_trip(s), s);
        // Byte-identisch mit manuellem Encode
        let mut slow = BitWriter::new();
        unsigned_integer::encode(&mut slow, 4); // 4 chars
        for ch in s.chars() {
            unsigned_integer::encode(&mut slow, ch as u64);
        }
        let mut fast = BitWriter::new();
        encode(&mut fast, s);
        assert_eq!(fast.into_vec(), slow.into_vec());
    }

    /// ASCII nach Non-ASCII sequentiell decodieren (Fast→Slow→Fast)
    #[test]
    fn sequential_ascii_and_non_ascii() {
        let mut w = BitWriter::new();
        encode(&mut w, "fast");      // ASCII → Fast-Path
        encode(&mut w, "日本語");    // Non-ASCII → Slow-Path
        encode(&mut w, "back");      // ASCII → Fast-Path
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap(), "fast");
        assert_eq!(decode(&mut r).unwrap(), "日本語");
        assert_eq!(decode(&mut r).unwrap(), "back");
    }
}
