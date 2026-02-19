//! Boolean encoding (Spec 7.1.2).
//!
//! Two encoding variants (context-dependent):
//! 1. Default: 1-bit unsigned integer (0=false, 1=true)
//! 2. When pattern facets available: 2-bit unsigned integer (0="false", 1="0", 2="true", 3="1")

use crate::bitstream::{BitReader, BitWriter};
use crate::{Result, n_bit_unsigned_integer};

/// The four lexical values of a boolean with pattern facets (Spec 7.1.2).
///
/// When an xsd:boolean has pattern facets defined in the schema, all four
/// lexical representations ("false", "0", "true", "1") must be preserved
/// to maintain round-trip fidelity with the original XML document.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BooleanValue {
    /// Lexical "false" (encoded as 0).
    False = 0,
    /// Lexical "0" (encoded as 1).
    Zero = 1,
    /// Lexical "true" (encoded as 2).
    True = 2,
    /// Lexical "1" (encoded as 3).
    One = 3,
}

/// Encodes a boolean as a 1-bit unsigned integer (Spec 7.1.2).
///
/// Use this when the schema datatype does NOT have pattern facets.
/// For booleans with pattern facets, use [`encode_with_pattern`].
pub fn encode(writer: &mut BitWriter, value: bool) {
    n_bit_unsigned_integer::encode(writer, value as u64, 1);
}

/// Decodes a boolean from a 1-bit unsigned integer (Spec 7.1.2).
///
/// Use this when the schema datatype does NOT have pattern facets.
/// For booleans with pattern facets, use [`decode_with_pattern`].
pub fn decode(reader: &mut BitReader) -> Result<bool> {
    let bit = n_bit_unsigned_integer::decode(reader, 1)?;
    Ok(bit == 1)
}

/// Encodes a boolean with pattern facets as a 2-bit unsigned integer (Spec 7.1.2).
///
/// Use this when the schema datatype HAS pattern facets.
/// For standard booleans, use [`encode`].
pub fn encode_with_pattern(writer: &mut BitWriter, value: BooleanValue) {
    n_bit_unsigned_integer::encode(writer, value as u64, 2);
}

/// Decodes a boolean with pattern facets from a 2-bit unsigned integer (Spec 7.1.2).
///
/// Use this when the schema datatype HAS pattern facets.
/// For standard booleans, use [`decode`].
pub fn decode_with_pattern(reader: &mut BitReader) -> Result<BooleanValue> {
    let bits = n_bit_unsigned_integer::decode(reader, 2)?;
    let value = match bits {
        0 => BooleanValue::False,
        1 => BooleanValue::Zero,
        2 => BooleanValue::True,
        3 => BooleanValue::One,
        _ => unreachable!("2-bit decode yielded value > 3"),
    };
    Ok(value)
}

/// Encodes a boolean byte-aligned (Spec 7.1.2, 7.1.9).
///
/// Bei byte-alignment/pre-compression/compression wird ein 1-bit Boolean
/// als 1 Byte encodiert (minimum number of bytes required to store 1 bit).
pub fn encode_byte_aligned(writer: &mut BitWriter, value: bool) {
    writer.write_byte_aligned(value as u8);
}

/// Decodes a boolean byte-aligned (Spec 7.1.2, 7.1.9).
///
/// Bei byte-alignment/pre-compression/compression wird ein 1-bit Boolean
/// als 1 Byte decodiert.
pub fn decode_byte_aligned(reader: &mut BitReader) -> Result<bool> {
    let byte = reader.read_byte_aligned()?;
    Ok(byte != 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trip(value: bool) -> bool {
        let mut w = BitWriter::new();
        encode(&mut w, value);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r).unwrap()
    }

    fn round_trip_with_pattern(value: BooleanValue) -> BooleanValue {
        let mut w = BitWriter::new();
        encode_with_pattern(&mut w, value);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode_with_pattern(&mut r).unwrap()
    }

    // --- 1-bit Boolean tests ---

    /// Spec 7.1.2: false encoded as 1-bit value 0
    #[test]
    fn encode_false() {
        assert_eq!(round_trip(false), false);
        let mut w = BitWriter::new();
        encode(&mut w, false);
        assert_eq!(w.bit_position(), 1);
    }

    /// Spec 7.1.2: true encoded as 1-bit value 1
    #[test]
    fn encode_true() {
        assert_eq!(round_trip(true), true);
        let mut w = BitWriter::new();
        encode(&mut w, true);
        assert_eq!(w.bit_position(), 1);
    }

    /// Spec 7.1.2: verify exact byte encoding (MSB-first)
    #[test]
    fn encode_byte_patterns() {
        // false → bit 0 → 0b0_0000000 = 0x00
        let mut w = BitWriter::new();
        encode(&mut w, false);
        assert_eq!(w.into_vec(), vec![0x00]);

        // true → bit 1 → 0b1_0000000 = 0x80
        let mut w = BitWriter::new();
        encode(&mut w, true);
        assert_eq!(w.into_vec(), vec![0x80]);
    }

    /// Spec 7.1.2: decode EOF
    #[test]
    fn decode_eof() {
        let mut r = BitReader::new(&[]);
        assert_eq!(
            decode(&mut r).unwrap_err(),
            crate::Error::PrematureEndOfStream
        );
    }

    // --- 2-bit Boolean with pattern facets tests ---

    /// Spec 7.1.2: all four pattern values round-trip
    #[test]
    fn pattern_all_values() {
        assert_eq!(
            round_trip_with_pattern(BooleanValue::False),
            BooleanValue::False
        );
        assert_eq!(
            round_trip_with_pattern(BooleanValue::Zero),
            BooleanValue::Zero
        );
        assert_eq!(
            round_trip_with_pattern(BooleanValue::True),
            BooleanValue::True
        );
        assert_eq!(
            round_trip_with_pattern(BooleanValue::One),
            BooleanValue::One
        );
    }

    /// Spec 7.1.2: pattern variant uses exactly 2 bits
    #[test]
    fn pattern_bit_width() {
        let mut w = BitWriter::new();
        encode_with_pattern(&mut w, BooleanValue::True);
        assert_eq!(w.bit_position(), 2);
    }

    /// Spec 7.1.2: pattern decode EOF
    #[test]
    fn pattern_decode_eof() {
        let mut r = BitReader::new(&[]);
        assert_eq!(
            decode_with_pattern(&mut r).unwrap_err(),
            crate::Error::PrematureEndOfStream
        );
    }

    /// Spec 7.1.2: verify encoded bit patterns
    #[test]
    fn pattern_encoded_values() {
        // "false" = 0, "0" = 1, "true" = 2, "1" = 3
        for (val, expected) in [
            (BooleanValue::False, 0u64),
            (BooleanValue::Zero, 1),
            (BooleanValue::True, 2),
            (BooleanValue::One, 3),
        ] {
            let mut w = BitWriter::new();
            encode_with_pattern(&mut w, val);
            let data = w.into_vec();
            let mut r = BitReader::new(&data);
            assert_eq!(n_bit_unsigned_integer::decode(&mut r, 2).unwrap(), expected);
        }
    }

    /// Spec 7.1.2: sequential booleans in a stream
    #[test]
    fn sequential_booleans() {
        let mut w = BitWriter::new();
        encode(&mut w, true);
        encode(&mut w, false);
        encode_with_pattern(&mut w, BooleanValue::One);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap(), true);
        assert_eq!(decode(&mut r).unwrap(), false);
        assert_eq!(decode_with_pattern(&mut r).unwrap(), BooleanValue::One);
    }

    /// Spec 7.1.2: 1-bit decode with no bits remaining
    #[test]
    fn decode_partial_exhausted() {
        let mut r = BitReader::new(&[0x80]);
        assert_eq!(decode(&mut r).unwrap(), true); // consumes 1 bit
        // 7 padding bits remain but next byte boundary exhausted after 7 more reads
        for _ in 0..7 {
            let _ = decode(&mut r).unwrap();
        }
        assert_eq!(
            decode(&mut r).unwrap_err(),
            crate::Error::PrematureEndOfStream
        );
    }

    /// Spec 7.1.2: 2-bit pattern decode with only 1 bit remaining
    #[test]
    fn pattern_decode_partial_eof() {
        let mut r = BitReader::new(&[0xFF]);
        // consume 7 bits, leaving 1
        for _ in 0..7 {
            let _ = decode(&mut r).unwrap();
        }
        assert_eq!(
            decode_with_pattern(&mut r).unwrap_err(),
            crate::Error::PrematureEndOfStream
        );
    }

    /// BooleanValue enum discriminants match spec values
    #[test]
    fn enum_discriminants() {
        assert_eq!(BooleanValue::False as u64, 0);
        assert_eq!(BooleanValue::Zero as u64, 1);
        assert_eq!(BooleanValue::True as u64, 2);
        assert_eq!(BooleanValue::One as u64, 3);
    }

    // --- Byte-aligned Boolean tests ---

    /// Spec 7.1.2, 7.1.9: byte-aligned Boolean encode
    #[test]
    fn byte_aligned_encode() {
        let mut w = BitWriter::new();
        encode_byte_aligned(&mut w, false);
        assert_eq!(w.into_vec(), vec![0x00]);

        let mut w = BitWriter::new();
        encode_byte_aligned(&mut w, true);
        assert_eq!(w.into_vec(), vec![0x01]);
    }

    /// Spec 7.1.2, 7.1.9: byte-aligned Boolean decode
    #[test]
    fn byte_aligned_decode() {
        let mut r = BitReader::new(&[0x00]);
        assert!(!decode_byte_aligned(&mut r).unwrap());

        let mut r = BitReader::new(&[0x01]);
        assert!(decode_byte_aligned(&mut r).unwrap());

        // Any non-zero value is true
        let mut r = BitReader::new(&[0xFF]);
        assert!(decode_byte_aligned(&mut r).unwrap());
    }

    /// Spec 7.1.2, 7.1.9: byte-aligned round-trip
    #[test]
    fn byte_aligned_round_trip() {
        for value in [true, false] {
            let mut w = BitWriter::new();
            encode_byte_aligned(&mut w, value);
            let mut r = BitReader::new(w.bytes());
            assert_eq!(decode_byte_aligned(&mut r).unwrap(), value);
        }
    }
}
