//! n-bit unsigned integer encoding (Spec 7.1.9).
//!
//! Represents an unsigned integer value using exactly `n` bits in the EXI
//! bit-packed stream. When `n` is 0 the value is omitted entirely (Spec 7.1.9).

use crate::Result;
use crate::bitstream::{BitReader, BitWriter};

/// Encodes an unsigned integer using exactly `n` bits (Spec 7.1.9).
///
/// # Panics
///
/// Panics if `n > 64` or if `value` does not fit in `n` bits.
#[inline]
pub fn encode(writer: &mut BitWriter, value: u64, n: u8) {
    assert!(n <= 64, "bit width must be 0..=64, got {n}");
    assert!(
        n == 64 || value < (1u64 << n),
        "value {value} does not fit in {n} bits"
    );
    writer.write_bits(value, n);
}

/// Decodes an unsigned integer from exactly `n` bits (Spec 7.1.9).
///
/// # Panics
///
/// Panics if `n > 64`.
#[inline]
pub fn decode(reader: &mut BitReader, n: u8) -> Result<u64> {
    reader.read_bits(n)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trip(value: u64, n: u8) -> u64 {
        let mut w = BitWriter::new();
        encode(&mut w, value, n);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r, n).unwrap()
    }

    // Spec 7.1.9: n=0, value is omitted
    #[test]
    fn zero_bits_omitted() {
        let mut w = BitWriter::new();
        encode(&mut w, 0, 0);
        assert_eq!(w.bit_position(), 0);
        let data = w.into_vec();
        assert!(data.is_empty());

        let mut r = BitReader::new(&[]);
        assert_eq!(decode(&mut r, 0).unwrap(), 0);
    }

    // Spec 7.1.9: n=1
    #[test]
    fn one_bit_values() {
        assert_eq!(round_trip(0, 1), 0);
        assert_eq!(round_trip(1, 1), 1);
    }

    // Spec 7.1.9: n=3
    #[test]
    fn three_bit_values() {
        for val in 0..8u64 {
            assert_eq!(round_trip(val, 3), val, "failed for {val}");
        }
    }

    // Spec 7.1.9: n=8 (full byte)
    #[test]
    fn eight_bit_values() {
        assert_eq!(round_trip(0, 8), 0);
        assert_eq!(round_trip(255, 8), 255);
        assert_eq!(round_trip(0xAB, 8), 0xAB);
    }

    // Spec 7.1.9: n=9 (crosses byte boundary)
    #[test]
    fn nine_bit_values() {
        assert_eq!(round_trip(0, 9), 0);
        assert_eq!(round_trip(511, 9), 511); // max 9-bit value
        assert_eq!(round_trip(256, 9), 256);
    }

    // Spec 7.1.9: n=17 (crosses two byte boundaries)
    #[test]
    fn seventeen_bit_values() {
        let max_17 = (1u64 << 17) - 1;
        assert_eq!(round_trip(0, 17), 0);
        assert_eq!(round_trip(max_17, 17), max_17);
        assert_eq!(round_trip(65536, 17), 65536);
    }

    // Spec 7.1.9: n=64 (maximum)
    #[test]
    fn sixty_four_bit_values() {
        assert_eq!(round_trip(0, 64), 0);
        assert_eq!(round_trip(u64::MAX, 64), u64::MAX);
    }

    // Decode EOF
    #[test]
    fn decode_premature_end_of_stream() {
        let mut r = BitReader::new(&[]);
        assert_eq!(
            decode(&mut r, 8).unwrap_err(),
            crate::Error::PrematureEndOfStream
        );
    }

    // Spec 7.1.9: partial byte available but n requires more bits
    #[test]
    fn decode_partial_eof() {
        let mut r = BitReader::new(&[0xFF]); // 8 bits available
        assert_eq!(
            decode(&mut r, 9).unwrap_err(),
            crate::Error::PrematureEndOfStream
        );
    }

    // Encode panics on overflow
    #[test]
    #[should_panic(expected = "does not fit")]
    fn encode_overflow_panics() {
        let mut w = BitWriter::new();
        encode(&mut w, 8, 3); // 8 doesn't fit in 3 bits
    }

    // n > 64 panics
    #[test]
    #[should_panic(expected = "bit width must be 0..=64")]
    fn encode_n_too_large_panics() {
        let mut w = BitWriter::new();
        encode(&mut w, 0, 65);
    }

    #[test]
    #[should_panic(expected = "bit count must be 0..=64")]
    fn decode_n_too_large_panics() {
        let mut r = BitReader::new(&[0xFF; 9]);
        let _ = decode(&mut r, 65);
    }

    // Multiple n-bit integers in sequence
    #[test]
    fn sequential_mixed_widths() {
        let mut w = BitWriter::new();
        encode(&mut w, 0b101, 3);
        encode(&mut w, 0xAB, 8);
        encode(&mut w, 1, 1);
        encode(&mut w, 0, 0); // omitted
        encode(&mut w, 0x1FF, 9);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r, 3).unwrap(), 0b101);
        assert_eq!(decode(&mut r, 8).unwrap(), 0xAB);
        assert_eq!(decode(&mut r, 1).unwrap(), 1);
        assert_eq!(decode(&mut r, 0).unwrap(), 0);
        assert_eq!(decode(&mut r, 9).unwrap(), 0x1FF);
    }
}
