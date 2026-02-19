//! Restricted Character Sets (Spec 7.1.10.1).
//!
//! Strings mit Pattern-Facets können effizienter encodiert werden:
//! - n-bit pro Zeichen statt variable-length Unsigned Integer
//! - n = ⌈log₂(N+1)⌉ wo N = Anzahl Zeichen im Set
//! - Escape: n-bit N gefolgt von Unicode Code Point als Unsigned Integer
//!
//! **Bedingungen für RCS (Spec 7.1.10.1):**
//! 1. < 256 Zeichen im Set
//! 2. Nur BMP-Zeichen (U+0000..U+FFFF)

mod parser;

use crate::bitstream::{BitReader, BitWriter};
use crate::{Error, Result, enumeration, n_bit_unsigned_integer, unsigned_integer};

/// Restricted Character Set (Spec 7.1.10.1).
///
/// Zeichen im Set werden als n-bit Unsigned Integer encodiert.
/// Zeichen außerhalb des Sets werden als Escape (n-bit N + Unicode Code Point) encodiert.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RestrictedCharacterSet {
    /// Sortiert nach Unicode Code Point
    chars: Vec<char>,
    /// Bitbreite = ⌈log₂(N+1)⌉
    n: u8,
}

impl RestrictedCharacterSet {
    /// Erstellt ein neues RCS aus einem Vektor von Zeichen (Spec 7.1.10.1).
    ///
    /// # Errors
    ///
    /// - `InvalidValue` wenn chars leer ist (N=0)
    /// - `InvalidValue` wenn Nicht-BMP Zeichen (> U+FFFF) enthalten sind
    /// - `InvalidValue` wenn nach Deduplizierung > 255 Zeichen verbleiben
    pub fn new(mut chars: Vec<char>) -> Result<Self> {
        if chars.is_empty() {
            return Err(Error::InvalidValue("RCS: empty character set".into()));
        }
        if chars.iter().any(|&ch| ch as u32 > 0xFFFF) {
            return Err(Error::InvalidValue("RCS: non-BMP character (> U+FFFF)".into()));
        }
        // Sortieren und Duplikate entfernen
        chars.sort_unstable();
        chars.dedup();
        // Nach dedup prüfen: > 255 Zeichen
        if chars.len() > 255 {
            return Err(Error::InvalidValue(
                format!("RCS: {} chars after dedup (max 255)", chars.len()),
            ));
        }
        // n = ⌈log₂(N+1)⌉ (N+1 weil Escape-Sentinel N auch encodiert werden muss)
        let n = enumeration::bit_width(chars.len() + 1);
        Ok(Self { chars, n })
    }

    /// Erstellt ein RCS aus einem XML Schema Regex Pattern (Appendix E).
    ///
    /// # Errors
    ///
    /// - `InvalidValue` wenn das Pattern nicht parsebar ist
    /// - `InvalidValue` wenn die resultierende Zeichenmenge leer oder > 255 ist
    /// - `InvalidValue` wenn Nicht-BMP Zeichen enthalten sind
    pub fn from_pattern(pattern: &str) -> Result<Self> {
        let chars = parser::parse_pattern(pattern)
            .ok_or_else(|| Error::InvalidValue(format!("RCS: unparseable pattern '{pattern}'")))?;
        Self::new(chars)
    }

    /// Anzahl der Zeichen im Set (immer 1..=255).
    #[inline]
    pub fn len(&self) -> usize {
        self.chars.len()
    }

    /// Gibt zurueck ob das Set leer ist (immer false, da leere Sets nicht erlaubt sind).
    #[inline]
    pub fn is_empty(&self) -> bool {
        false // Invariante: chars.len() >= 1
    }

    /// Bitbreite für die Zeichenkodierung.
    #[inline]
    pub fn bit_width(&self) -> u8 {
        self.n
    }

    /// Gibt die Zeichen im Set als Slice zurück.
    #[inline]
    pub fn chars(&self) -> &[char] {
        &self.chars
    }

    /// O(log N) Lookup via binary search.
    #[inline]
    fn char_to_index(&self, ch: char) -> Option<usize> {
        self.chars.binary_search(&ch).ok()
    }

    /// Encodiert ein einzelnes Zeichen (Spec 7.1.10.1).
    ///
    /// - Zeichen im Set: n-bit Index
    /// - Zeichen nicht im Set: n-bit N (Escape) + Unicode Code Point als Unsigned Integer
    pub fn encode_char(&self, writer: &mut BitWriter, ch: char) {
        match self.char_to_index(ch) {
            Some(index) => {
                // Zeichen im Set: n-bit Index
                n_bit_unsigned_integer::encode(writer, index as u64, self.n);
            }
            None => {
                // Escape: n-bit N + Unicode Code Point
                n_bit_unsigned_integer::encode(writer, self.chars.len() as u64, self.n);
                unsigned_integer::encode(writer, ch as u64);
            }
        }
    }

    /// Decodiert ein einzelnes Zeichen (Spec 7.1.10.1).
    pub fn decode_char(&self, reader: &mut BitReader) -> Result<char> {
        let index = n_bit_unsigned_integer::decode(reader, self.n)? as usize;
        if index < self.chars.len() {
            // Zeichen im Set: Index 0..N-1
            Ok(self.chars[index])
        } else if index == self.chars.len() {
            // Escape: n-bit N gefolgt von Unicode Code Point
            let code_point = unsigned_integer::decode(reader)?;
            char::from_u32(code_point as u32).ok_or(crate::Error::InvalidCodePoint(code_point))
        } else {
            // Ungültiger Index > N (korrupte Daten)
            Err(crate::Error::InvalidEnumerationIndex {
                index,
                enum_count: self.chars.len() + 1,
            })
        }
    }

    /// Encodiert einen String (ohne Length-Prefix, Spec 7.1.10.1).
    ///
    /// Length wird extern encodiert (via `unsigned_integer::encode`).
    pub fn encode_string(&self, writer: &mut BitWriter, value: &str) {
        for ch in value.chars() {
            self.encode_char(writer, ch);
        }
    }

    /// Decodiert einen String (Spec 7.1.10.1).
    ///
    /// `len` ist die Anzahl der Zeichen (extern via `unsigned_integer::decode` gelesen).
    pub fn decode_string(&self, reader: &mut BitReader, len: u64) -> Result<String> {
        let mut result = String::with_capacity(len as usize);
        for _ in 0..len {
            result.push(self.decode_char(reader)?);
        }
        Ok(result)
    }
}

// =============================================================================
// Vordefinierte Sets (Table 7-2)
// =============================================================================

/// Helper: Erstellt ein RCS aus Whitespace + zusaetzlichen Zeichen und Ranges.
fn build_rcs(
    extra_chars: &[char],
    ranges: &[std::ops::RangeInclusive<char>],
) -> RestrictedCharacterSet {
    let mut chars = vec!['\t', '\n', '\r', ' ']; // Whitespace immer enthalten
    chars.extend_from_slice(extra_chars);
    for range in ranges {
        chars.extend(range.clone());
    }
    RestrictedCharacterSet::new(chars).expect("vordefiniertes RCS muss gueltig sein")
}

/// base64Binary: { \t, \n, \r, ' ', +, /, 0-9, =, A-Z, a-z }
pub fn base64_binary() -> RestrictedCharacterSet {
    build_rcs(&['+', '/', '='], &['0'..='9', 'A'..='Z', 'a'..='z'])
}

/// hexBinary: { \t, \n, \r, ' ', 0-9, A-F, a-f }
pub fn hex_binary() -> RestrictedCharacterSet {
    build_rcs(&[], &['0'..='9', 'A'..='F', 'a'..='f'])
}

/// boolean: { \t, \n, \r, ' ', 0, 1, a, e, f, l, r, s, t, u }
pub fn boolean() -> RestrictedCharacterSet {
    build_rcs(&['0', '1', 'a', 'e', 'f', 'l', 'r', 's', 't', 'u'], &[])
}

/// dateTime/time/date/gYearMonth/gYear/gMonthDay/gDay/gMonth:
/// { \t, \n, \r, ' ', +, -, ., 0-9, :, T, Z }
pub fn date_time() -> RestrictedCharacterSet {
    build_rcs(&['+', '-', '.', ':', 'T', 'Z'], &['0'..='9'])
}

/// decimal: { \t, \n, \r, ' ', +, -, ., 0-9 }
pub fn decimal() -> RestrictedCharacterSet {
    build_rcs(&['+', '-', '.'], &['0'..='9'])
}

/// double: { \t, \n, \r, ' ', +, -, ., 0-9, E, F, I, N, a, e }
pub fn double() -> RestrictedCharacterSet {
    build_rcs(&['+', '-', '.', 'E', 'F', 'I', 'N', 'a', 'e'], &['0'..='9'])
}

/// integer: { \t, \n, \r, ' ', +, -, 0-9 }
pub fn integer() -> RestrictedCharacterSet {
    build_rcs(&['+', '-'], &['0'..='9'])
}

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // Vordefinierte Sets (Table 7-2)
    // =========================================================================

    // Spec 7.1.10.1, Table 7-2: base64Binary
    #[test]
    fn base64_binary_set() {
        let rcs = base64_binary();
        // { \t, \n, \r, ' ', +, /, 0-9, =, A-Z, a-z }
        // 4 + 1 + 1 + 10 + 1 + 26 + 26 = 69 Zeichen
        assert_eq!(rcs.len(), 69);
        // Prüfe einige Zeichen
        assert!(rcs.chars().contains(&'\t'));
        assert!(rcs.chars().contains(&'+'));
        assert!(rcs.chars().contains(&'/'));
        assert!(rcs.chars().contains(&'0'));
        assert!(rcs.chars().contains(&'9'));
        assert!(rcs.chars().contains(&'='));
        assert!(rcs.chars().contains(&'A'));
        assert!(rcs.chars().contains(&'Z'));
        assert!(rcs.chars().contains(&'a'));
        assert!(rcs.chars().contains(&'z'));
        // Nicht enthalten
        assert!(!rcs.chars().contains(&'@'));
        // n = ceil(log2(69+1)) = ceil(log2(70)) = 7
        assert_eq!(rcs.bit_width(), 7);
    }

    // Spec 7.1.10.1, Table 7-2: hexBinary
    #[test]
    fn hex_binary_set() {
        let rcs = hex_binary();
        // { \t, \n, \r, ' ', 0-9, A-F, a-f }
        // 4 + 10 + 6 + 6 = 26 Zeichen
        assert_eq!(rcs.len(), 26);
        assert!(rcs.chars().contains(&'A'));
        assert!(rcs.chars().contains(&'F'));
        assert!(rcs.chars().contains(&'a'));
        assert!(rcs.chars().contains(&'f'));
        assert!(!rcs.chars().contains(&'G'));
        // n = ceil(log2(26+1)) = ceil(log2(27)) = 5
        assert_eq!(rcs.bit_width(), 5);
    }

    // Spec 7.1.10.1, Table 7-2: boolean
    #[test]
    fn boolean_set() {
        let rcs = boolean();
        // { \t, \n, \r, ' ', 0, 1, a, e, f, l, r, s, t, u }
        // 4 + 2 + 8 = 14 Zeichen
        assert_eq!(rcs.len(), 14);
        // Für "true", "false", "0", "1"
        assert!(rcs.chars().contains(&'t'));
        assert!(rcs.chars().contains(&'r'));
        assert!(rcs.chars().contains(&'u'));
        assert!(rcs.chars().contains(&'e'));
        assert!(rcs.chars().contains(&'f'));
        assert!(rcs.chars().contains(&'a'));
        assert!(rcs.chars().contains(&'l'));
        assert!(rcs.chars().contains(&'s'));
        // n = ceil(log2(14+1)) = ceil(log2(15)) = 4
        assert_eq!(rcs.bit_width(), 4);
    }

    // Spec 7.1.10.1, Table 7-2: dateTime
    #[test]
    fn date_time_set() {
        let rcs = date_time();
        // { \t, \n, \r, ' ', +, -, ., 0-9, :, T, Z }
        // 4 + 3 + 10 + 3 = 20 Zeichen
        assert_eq!(rcs.len(), 20);
        assert!(rcs.chars().contains(&'T'));
        assert!(rcs.chars().contains(&'Z'));
        assert!(rcs.chars().contains(&':'));
        assert!(rcs.chars().contains(&'-'));
        // n = ceil(log2(20+1)) = ceil(log2(21)) = 5
        assert_eq!(rcs.bit_width(), 5);
    }

    // Spec 7.1.10.1, Table 7-2: decimal
    #[test]
    fn decimal_set() {
        let rcs = decimal();
        // { \t, \n, \r, ' ', +, -, ., 0-9 }
        // 4 + 3 + 10 = 17 Zeichen
        assert_eq!(rcs.len(), 17);
        assert!(rcs.chars().contains(&'.'));
        assert!(rcs.chars().contains(&'-'));
        // n = ceil(log2(17+1)) = ceil(log2(18)) = 5
        assert_eq!(rcs.bit_width(), 5);
    }

    // Spec 7.1.10.1, Table 7-2: double
    #[test]
    fn double_set() {
        let rcs = double();
        // { \t, \n, \r, ' ', +, -, ., 0-9, E, F, I, N, a, e }
        // 4 + 3 + 10 + 6 = 23 Zeichen
        assert_eq!(rcs.len(), 23);
        // Für INF, -INF, NaN
        assert!(rcs.chars().contains(&'I'));
        assert!(rcs.chars().contains(&'N'));
        assert!(rcs.chars().contains(&'F'));
        assert!(rcs.chars().contains(&'E'));
        assert!(rcs.chars().contains(&'a'));
        assert!(rcs.chars().contains(&'e'));
        // n = ceil(log2(23+1)) = ceil(log2(24)) = 5
        assert_eq!(rcs.bit_width(), 5);
    }

    // Spec 7.1.10.1, Table 7-2: integer
    #[test]
    fn integer_set() {
        let rcs = integer();
        // { \t, \n, \r, ' ', +, -, 0-9 }
        // 4 + 2 + 10 = 16 Zeichen
        assert_eq!(rcs.len(), 16);
        assert!(rcs.chars().contains(&'+'));
        assert!(rcs.chars().contains(&'-'));
        // n = ceil(log2(16+1)) = ceil(log2(17)) = 5
        assert_eq!(rcs.bit_width(), 5);
    }

    // Spec 7.1.10.1: Alle Sets sind sortiert
    #[test]
    fn vordefinierte_sets_sortiert() {
        for rcs in [
            base64_binary(),
            hex_binary(),
            boolean(),
            date_time(),
            decimal(),
            double(),
            integer(),
        ] {
            let chars = rcs.chars();
            let mut sorted = chars.to_vec();
            sorted.sort();
            assert_eq!(chars, &sorted[..], "Set muss sortiert sein");
        }
    }

    // =========================================================================
    // RestrictedCharacterSet::new() Edge Cases
    // =========================================================================

    // Spec 7.1.10.1: Leeres Set nicht erlaubt
    #[test]
    fn new_leeres_set() {
        assert!(RestrictedCharacterSet::new(vec![]).is_err());
    }

    // Spec 7.1.10.1: > 255 Zeichen nicht erlaubt
    #[test]
    fn new_zu_viele_zeichen() {
        let chars: Vec<char> = (0u32..=255).map(|c| char::from_u32(c).unwrap()).collect();
        assert_eq!(chars.len(), 256);
        assert!(RestrictedCharacterSet::new(chars).is_err());
    }

    // Spec 7.1.10.1: Genau 255 Zeichen ist OK
    #[test]
    fn new_255_zeichen() {
        let chars: Vec<char> = (0u32..255).map(|c| char::from_u32(c).unwrap()).collect();
        assert_eq!(chars.len(), 255);
        let rcs = RestrictedCharacterSet::new(chars).unwrap();
        assert_eq!(rcs.len(), 255);
        // n = ceil(log2(255+1)) = ceil(log2(256)) = 8
        assert_eq!(rcs.bit_width(), 8);
    }

    // Spec 7.1.10.1: Nicht-BMP Zeichen nicht erlaubt
    #[test]
    fn new_nicht_bmp_zeichen() {
        let chars = vec!['a', 'b', '😀']; // U+1F600 ist außerhalb BMP
        assert!(RestrictedCharacterSet::new(chars).is_err());
    }

    // Spec 7.1.10.1: N=1 (ein Zeichen + Escape-Sentinel)
    #[test]
    fn new_ein_zeichen() {
        let rcs = RestrictedCharacterSet::new(vec!['a']).unwrap();
        assert_eq!(rcs.len(), 1);
        // n = ceil(log2(1+1)) = ceil(log2(2)) = 1
        assert_eq!(rcs.bit_width(), 1);
    }

    // Duplikate werden entfernt
    #[test]
    fn new_duplikate() {
        let rcs = RestrictedCharacterSet::new(vec!['a', 'b', 'a', 'c', 'b']).unwrap();
        assert_eq!(rcs.len(), 3);
        assert_eq!(rcs.chars(), &['a', 'b', 'c']);
    }

    // Unsortierte Eingabe wird sortiert
    #[test]
    fn new_unsortiert() {
        let rcs = RestrictedCharacterSet::new(vec!['z', 'a', 'm']).unwrap();
        assert_eq!(rcs.chars(), &['a', 'm', 'z']);
    }

    // Nach dedup > 255 Zeichen
    #[test]
    fn new_nach_dedup_zu_viele() {
        // 256 unterschiedliche Zeichen, aber mit Duplikaten am Anfang
        let mut chars: Vec<char> = (0u32..256).map(|c| char::from_u32(c).unwrap()).collect();
        chars.push('\0'); // Duplikat
        assert_eq!(chars.len(), 257); // Vor dedup: 257
        // Nach dedup: 256 → zu viel
        assert!(RestrictedCharacterSet::new(chars).is_err());
    }

    // Nach dedup leer (nur Duplikate)
    #[test]
    fn new_nach_dedup_leer() {
        // Nur ein Zeichen, aber nach dedup immer noch eines
        let rcs = RestrictedCharacterSet::new(vec!['a', 'a', 'a']).unwrap();
        assert_eq!(rcs.len(), 1);
    }

    // Viele Duplikate, aber < 256 einzigartige → OK
    #[test]
    fn new_viele_duplikate_ok() {
        // 300 Zeichen mit nur 100 einzigartigen
        let mut chars: Vec<char> = Vec::new();
        for _ in 0..3 {
            chars.extend((0x20u32..0x84).map(|c| char::from_u32(c).unwrap()));
        }
        assert_eq!(chars.len(), 300); // Vor dedup: 300
        let rcs = RestrictedCharacterSet::new(chars).unwrap();
        assert_eq!(rcs.len(), 100); // Nach dedup: 100 → OK
    }

    // =========================================================================
    // Encoding/Decoding (Spec 7.1.10.1)
    // =========================================================================

    fn round_trip_char(rcs: &RestrictedCharacterSet, ch: char) -> char {
        let mut w = BitWriter::new();
        rcs.encode_char(&mut w, ch);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        rcs.decode_char(&mut r).unwrap()
    }

    fn round_trip_string(rcs: &RestrictedCharacterSet, s: &str) -> String {
        let mut w = BitWriter::new();
        unsigned_integer::encode(&mut w, s.chars().count() as u64);
        rcs.encode_string(&mut w, s);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        let len = unsigned_integer::decode(&mut r).unwrap();
        rcs.decode_string(&mut r, len).unwrap()
    }

    // Spec 7.1.10.1: Zeichen im Set
    #[test]
    fn encode_decode_zeichen_im_set() {
        let rcs = RestrictedCharacterSet::new(vec!['a', 'b', 'c']).unwrap();
        assert_eq!(round_trip_char(&rcs, 'a'), 'a');
        assert_eq!(round_trip_char(&rcs, 'b'), 'b');
        assert_eq!(round_trip_char(&rcs, 'c'), 'c');
    }

    // Spec 7.1.10.1: Zeichen außerhalb des Sets (Escape-Mechanismus)
    #[test]
    fn encode_decode_escape() {
        let rcs = RestrictedCharacterSet::new(vec!['a', 'b', 'c']).unwrap();
        // 'x' ist nicht im Set, wird als Escape encodiert
        assert_eq!(round_trip_char(&rcs, 'x'), 'x');
        assert_eq!(round_trip_char(&rcs, 'z'), 'z');
        assert_eq!(round_trip_char(&rcs, '0'), '0');
    }

    // Spec 7.1.10.1: String-Encoding
    #[test]
    fn encode_decode_string_im_set() {
        let rcs = base64_binary();
        assert_eq!(round_trip_string(&rcs, "ABC123"), "ABC123");
        assert_eq!(round_trip_string(&rcs, "a+b/c="), "a+b/c=");
    }

    // Spec 7.1.10.1: Gemischter String (im Set + escaped)
    #[test]
    fn encode_decode_gemischter_string() {
        let rcs = RestrictedCharacterSet::new(vec!['a', 'b', 'c']).unwrap();
        assert_eq!(round_trip_string(&rcs, "abc"), "abc");
        assert_eq!(round_trip_string(&rcs, "axbycz"), "axbycz");
    }

    // Spec 7.1.10.1: Leerer String
    #[test]
    fn encode_decode_leerer_string() {
        let rcs = base64_binary();
        assert_eq!(round_trip_string(&rcs, ""), "");
    }

    // Spec 7.1.10.1: Bitbreite ist korrekt
    #[test]
    fn encoding_bitbreite() {
        // N=3, n=ceil(log2(4))=2
        let rcs = RestrictedCharacterSet::new(vec!['a', 'b', 'c']).unwrap();
        assert_eq!(rcs.bit_width(), 2);

        let mut w = BitWriter::new();
        rcs.encode_char(&mut w, 'a'); // Index 0: 2 bits
        assert_eq!(w.bit_position(), 2);

        let mut w = BitWriter::new();
        rcs.encode_char(&mut w, 'x'); // Escape: 2 bits + Unsigned Integer für 'x'
        // Escape (Index 3) = 2 bits, 'x' = 0x78 = 120, encoded als 1 Byte = 8 bits
        assert_eq!(w.bit_position(), 2 + 8);
    }

    // Spec 7.1.10.1: Alle vordefinierten Sets round-trippen korrekt
    #[test]
    fn vordefinierte_sets_roundtrip() {
        let test_cases = [
            (base64_binary(), "SGVsbG8gV29ybGQh"),
            (hex_binary(), "DEADBEEF"),
            (boolean(), "true"),
            (boolean(), "false"),
            (boolean(), "1"),
            (boolean(), "0"),
            (date_time(), "2024-01-15T10:30:00Z"),
            (decimal(), "-123.456"),
            (double(), "-1.23E10"),
            (double(), "INF"),
            (double(), "-INF"),
            (double(), "NaN"),
            (integer(), "-42"),
        ];
        for (rcs, value) in test_cases {
            assert_eq!(round_trip_string(&rcs, value), value, "Failed for: {value}");
        }
    }

    // =========================================================================
    // Regex Parser (Appendix E)
    // =========================================================================

    // Appendix E: Einfache Literale
    #[test]
    fn from_pattern_literale() {
        let rcs = RestrictedCharacterSet::from_pattern("abc").unwrap();
        assert_eq!(rcs.chars(), &['a', 'b', 'c']);
    }

    // Appendix E: Ranges [a-z]
    #[test]
    fn from_pattern_range() {
        let rcs = RestrictedCharacterSet::from_pattern("[a-z]").unwrap();
        assert_eq!(rcs.len(), 26);
        assert_eq!(rcs.chars().first(), Some(&'a'));
        assert_eq!(rcs.chars().last(), Some(&'z'));
    }

    // Appendix E: Alternativen a|b|c
    #[test]
    fn from_pattern_alternativen() {
        let rcs = RestrictedCharacterSet::from_pattern("a|b|c").unwrap();
        assert_eq!(rcs.chars(), &['a', 'b', 'c']);
    }

    // Appendix E: Gruppen (a|b)c
    #[test]
    fn from_pattern_gruppen() {
        let rcs = RestrictedCharacterSet::from_pattern("(a|b)c").unwrap();
        assert_eq!(rcs.chars(), &['a', 'b', 'c']);
    }

    // Appendix E: Character Class [abc]
    #[test]
    fn from_pattern_char_class() {
        let rcs = RestrictedCharacterSet::from_pattern("[abc]").unwrap();
        assert_eq!(rcs.chars(), &['a', 'b', 'c']);
    }

    // Appendix E: Character Class mit Range [a-zA-Z0-9]
    #[test]
    fn from_pattern_char_class_multi_range() {
        let rcs = RestrictedCharacterSet::from_pattern("[a-zA-Z0-9]").unwrap();
        assert_eq!(rcs.len(), 62); // 26 + 26 + 10
    }

    // Appendix E: Subtraction [a-z-[aeiou]]
    #[test]
    fn from_pattern_subtraction() {
        let rcs = RestrictedCharacterSet::from_pattern("[a-z-[aeiou]]").unwrap();
        // 26 - 5 Vokale = 21 Konsonanten
        assert_eq!(rcs.len(), 21);
        assert!(!rcs.chars().contains(&'a'));
        assert!(!rcs.chars().contains(&'e'));
        assert!(rcs.chars().contains(&'b'));
        assert!(rcs.chars().contains(&'z'));
    }

    // Appendix E: \s → { \t, \n, \r, ' ' }
    #[test]
    fn from_pattern_whitespace_escape() {
        let rcs = RestrictedCharacterSet::from_pattern(r"\s").unwrap();
        assert_eq!(rcs.len(), 4);
        assert!(rcs.chars().contains(&'\t'));
        assert!(rcs.chars().contains(&'\n'));
        assert!(rcs.chars().contains(&'\r'));
        assert!(rcs.chars().contains(&' '));
    }

    // Appendix E: Escaped Metazeichen \+, \-, \[, etc.
    #[test]
    fn from_pattern_escaped_meta() {
        let rcs = RestrictedCharacterSet::from_pattern(r"\+\-\.\*").unwrap();
        assert_eq!(rcs.chars(), &['*', '+', '-', '.']);
    }

    // Appendix E: Escape-Sequenzen \n, \r, \t
    #[test]
    fn from_pattern_escape_sequenzen() {
        let rcs = RestrictedCharacterSet::from_pattern(r"\n\r\t").unwrap();
        assert_eq!(rcs.chars(), &['\t', '\n', '\r']);
    }

    // Appendix E: Quantifier werden ignoriert (nur Zeichenmenge relevant)
    #[test]
    fn from_pattern_quantifier() {
        let rcs1 = RestrictedCharacterSet::from_pattern("a").unwrap();
        let rcs2 = RestrictedCharacterSet::from_pattern("a+").unwrap();
        let rcs3 = RestrictedCharacterSet::from_pattern("a*").unwrap();
        let rcs4 = RestrictedCharacterSet::from_pattern("a?").unwrap();
        let rcs5 = RestrictedCharacterSet::from_pattern("a{2,5}").unwrap();
        assert_eq!(rcs1.chars(), rcs2.chars());
        assert_eq!(rcs1.chars(), rcs3.chars());
        assert_eq!(rcs1.chars(), rcs4.chars());
        assert_eq!(rcs1.chars(), rcs5.chars());
    }

    // Appendix E: \d, \D, \w, \W, etc. → None (nicht unterstützt)
    #[test]
    fn from_pattern_nicht_unterstuetzt() {
        assert!(RestrictedCharacterSet::from_pattern(r"\d").is_err());
        assert!(RestrictedCharacterSet::from_pattern(r"\D").is_err());
        assert!(RestrictedCharacterSet::from_pattern(r"\w").is_err());
        assert!(RestrictedCharacterSet::from_pattern(r"\W").is_err());
        assert!(RestrictedCharacterSet::from_pattern(r"\i").is_err());
        assert!(RestrictedCharacterSet::from_pattern(r"\I").is_err());
        assert!(RestrictedCharacterSet::from_pattern(r"\c").is_err());
        assert!(RestrictedCharacterSet::from_pattern(r"\C").is_err());
    }

    // Appendix E: Category Escapes \p{Lu}, \P{Lu} → None
    #[test]
    fn from_pattern_category_escapes() {
        assert!(RestrictedCharacterSet::from_pattern(r"\p{Lu}").is_err());
        assert!(RestrictedCharacterSet::from_pattern(r"\P{Lu}").is_err());
        assert!(RestrictedCharacterSet::from_pattern(r"\p{L}").is_err());
    }

    // Appendix E: . Wildcard → None (zu viele Zeichen)
    #[test]
    fn from_pattern_wildcard() {
        assert!(RestrictedCharacterSet::from_pattern(".").is_err());
    }

    // Spec 7.1.10.1: Negierte Klasse [^a] → None (BMP_SIZE - 1 > 255)
    #[test]
    fn from_pattern_negierte_klasse() {
        assert!(RestrictedCharacterSet::from_pattern("[^a]").is_err());
        assert!(RestrictedCharacterSet::from_pattern("[^abc]").is_err());
    }

    // Spec 7.1.10.1: Leeres Pattern → None
    #[test]
    fn from_pattern_leer() {
        assert!(RestrictedCharacterSet::from_pattern("").is_err());
    }

    // Spec 7.1.10.1: > 255 Zeichen → None
    #[test]
    fn from_pattern_zu_viele_zeichen() {
        // Pattern für alle ASCII-Zeichen + mehr
        assert!(RestrictedCharacterSet::from_pattern(r"[\x00-\xFF]").is_err());
    }

    // =========================================================================
    // Grenzfälle (Codex-Review)
    // =========================================================================

    // N=1: Ein Zeichen + Escape-Sentinel (n=1)
    #[test]
    fn grenzfall_n_eins() {
        let rcs = RestrictedCharacterSet::new(vec!['x']).unwrap();
        assert_eq!(rcs.bit_width(), 1); // ceil(log2(2)) = 1

        // 'x' im Set: Index 0
        let mut w = BitWriter::new();
        rcs.encode_char(&mut w, 'x');
        assert_eq!(w.bit_position(), 1);

        // 'y' nicht im Set: Escape (Index 1) + Code Point
        let mut w = BitWriter::new();
        rcs.encode_char(&mut w, 'y');
        assert!(w.bit_position() > 1);

        assert_eq!(round_trip_char(&rcs, 'x'), 'x');
        assert_eq!(round_trip_char(&rcs, 'y'), 'y');
    }

    // N=255: Maximal erlaubte Größe (n=8)
    #[test]
    fn grenzfall_n_255() {
        let chars: Vec<char> = (0x20u32..0x20 + 255)
            .map(|c| char::from_u32(c).unwrap())
            .collect();
        let rcs = RestrictedCharacterSet::new(chars).unwrap();
        assert_eq!(rcs.len(), 255);
        assert_eq!(rcs.bit_width(), 8); // ceil(log2(256)) = 8

        // Erstes Zeichen im Set
        assert_eq!(round_trip_char(&rcs, ' '), ' ');
        // Letztes Zeichen im Set
        let last = char::from_u32(0x20 + 254).unwrap();
        assert_eq!(round_trip_char(&rcs, last), last);
        // Zeichen außerhalb des Sets
        assert_eq!(round_trip_char(&rcs, '\t'), '\t');
    }

    // Surrogates in Ranges werden ausgespart (Spec 7.1.10.1)
    #[test]
    fn grenzfall_surrogates() {
        // Range die Surrogates enthält: U+D700..U+D7FE (255 Zeichen vor Surrogates)
        let rcs = RestrictedCharacterSet::from_pattern(r"[\uD700-\uD7FE]").unwrap();
        assert_eq!(rcs.len(), 255);
        // Kein Zeichen im Surrogate-Bereich
        for ch in rcs.chars() {
            let cp = *ch as u32;
            assert!(cp < 0xD800 || cp > 0xDFFF, "Surrogate gefunden: U+{cp:04X}");
        }
    }

    // =========================================================================
    // Decode Errors
    // =========================================================================

    // Spec 7.1.10.1: EOF beim Decodieren des Index
    #[test]
    fn decode_eof() {
        let rcs = RestrictedCharacterSet::new(vec!['a', 'b']).unwrap();
        let mut r = BitReader::new(&[]);
        assert!(rcs.decode_char(&mut r).is_err());
    }

    // Spec 7.1.10.1: EOF beim Decodieren des Escape-Code-Points
    #[test]
    fn decode_eof_bei_escape_code_point() {
        let rcs = RestrictedCharacterSet::new(vec!['a']).unwrap();
        // Escape-Index (1) encodieren, aber kein Code Point danach
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 1, rcs.bit_width());
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let err = rcs.decode_char(&mut r).unwrap_err();
        assert_eq!(err, crate::Error::PrematureEndOfStream);
    }

    // Spec 7.1.10.1: EOF mitten im String
    #[test]
    fn decode_string_eof_mitten_im_string() {
        let rcs = RestrictedCharacterSet::new(vec!['a', 'b', 'c']).unwrap();
        let mut w = BitWriter::new();
        // Nur 2 Zeichen encodieren
        rcs.encode_char(&mut w, 'a');
        rcs.encode_char(&mut w, 'b');
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        // Versuche 5 Zeichen zu lesen
        let err = rcs.decode_string(&mut r, 5).unwrap_err();
        assert_eq!(err, crate::Error::PrematureEndOfStream);
    }

    // Spec 7.1.10.1: Ungültiger Index > N (korrupte Daten)
    #[test]
    fn decode_ungueltiger_index() {
        // N=2, n=2 bits. Index 3 ist > N, sollte Fehler sein.
        let rcs = RestrictedCharacterSet::new(vec!['a', 'b']).unwrap();
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 3, rcs.bit_width()); // Index 3 > N=2
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let err = rcs.decode_char(&mut r).unwrap_err();
        assert_eq!(
            err,
            crate::Error::InvalidEnumerationIndex {
                index: 3,
                enum_count: 3
            }
        );
    }

    // Spec 7.1.10.1: Ungültiger Code Point beim Escape
    #[test]
    fn decode_ungueltig_code_point() {
        let rcs = RestrictedCharacterSet::new(vec!['a']).unwrap();
        // Manuell Escape (Index 1) + ungültiger Surrogate encodieren
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 1, rcs.bit_width()); // Escape
        unsigned_integer::encode(&mut w, 0xD800); // Surrogate (ungültig)
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let err = rcs.decode_char(&mut r).unwrap_err();
        assert_eq!(err, crate::Error::InvalidCodePoint(0xD800));
    }

    /// RCS is_empty() ist immer false (Invariante: mindestens 1 Zeichen)
    #[test]
    fn rcs_is_empty() {
        let rcs = RestrictedCharacterSet::new(vec!['a']).unwrap();
        assert!(!rcs.is_empty()); // Niemals leer
    }
}
