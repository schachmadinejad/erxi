//! Signed integer encoding (Spec 7.1.5).
//!
//! Three encoding variants depending on schema-derived bounds:
//! 1. Bounded (range ≤ 4096): n-bit unsigned offset from minimum
//! 2. Non-negative (min ≥ 0): use `unsigned_integer` directly (no wrapper here)
//! 3. Unbounded signed: 1-bit sign + unsigned integer magnitude

use crate::bitstream::{BitReader, BitWriter};
use crate::{Error, Result, n_bit_unsigned_integer, unsigned_integer};

/// Encodes a signed integer (Spec 7.1.5, unbounded signed case).
///
/// Sign bit (0 = non-negative, 1 = negative) followed by the magnitude as an
/// unsigned integer. For negative values the magnitude is `(-value - 1)`.
pub fn encode(writer: &mut BitWriter, value: i64) {
    if value >= 0 {
        writer.write_bit(false);
        unsigned_integer::encode(writer, value as u64);
    } else {
        writer.write_bit(true);
        // (-value - 1) computed via unsigned arithmetic to avoid overflow at i64::MIN
        unsigned_integer::encode(writer, !(value as u64));
    }
}

/// Decodes a signed integer (Spec 7.1.5, unbounded signed case).
pub fn decode(reader: &mut BitReader) -> Result<i64> {
    let sign = reader.read_bit()?;
    let magnitude = unsigned_integer::decode(reader)?;
    if magnitude > i64::MAX as u64 {
        return Err(Error::IntegerOverflow);
    }
    if sign {
        Ok(-(magnitude as i64) - 1)
    } else {
        Ok(magnitude as i64)
    }
}

/// Encodes a bounded integer (Spec 7.1.5, bounded case).
///
/// The value is encoded as an n-bit unsigned offset from `min`, where
/// `n = ⌈log₂(max - min + 1)⌉`.
///
/// # Panics
///
/// Panics if `max < min`, if `value` is not in `[min, max]`, or if `max - min + 1 > 4096`.
pub fn encode_bounded(writer: &mut BitWriter, value: i64, min: i64, max: i64) {
    let range = bounded_range(min, max);
    assert!(
        value >= min && value <= max,
        "value {value} not in [{min}, {max}]"
    );
    let n = ceiling_log2(range);
    n_bit_unsigned_integer::encode(writer, (value - min) as u64, n);
}

/// Decodes a bounded integer (Spec 7.1.5, bounded case).
///
/// # Panics
///
/// Panics if `max < min` or if `max - min + 1 > 4096`.
pub fn decode_bounded(reader: &mut BitReader, min: i64, max: i64) -> Result<i64> {
    let range = bounded_range(min, max);
    let n = ceiling_log2(range);
    let offset = n_bit_unsigned_integer::decode(reader, n)?;
    if offset >= range {
        return Err(Error::IntegerOverflow);
    }
    Ok(min + offset as i64)
}

/// Computes `max - min + 1` as u64, panicking if range exceeds 4096.
fn bounded_range(min: i64, max: i64) -> u64 {
    assert!(max >= min, "max ({max}) < min ({min})");
    let range = max as i128 - min as i128 + 1;
    assert!(range <= 4096, "range {range} exceeds 4096");
    range as u64
}

/// Ceiling log₂: number of bits needed to represent values 0..range-1.
fn ceiling_log2(range: u64) -> u8 {
    if range <= 1 {
        0
    } else {
        (range - 1).ilog2() as u8 + 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn signed_round_trip(value: i64) -> i64 {
        let mut w = BitWriter::new();
        encode(&mut w, value);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r).unwrap()
    }

    fn bounded_round_trip(value: i64, min: i64, max: i64) -> i64 {
        let mut w = BitWriter::new();
        encode_bounded(&mut w, value, min, max);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode_bounded(&mut r, min, max).unwrap()
    }

    // --- Signed (unbounded signed case) tests ---

    /// Spec 7.1.5: encode/decode 0 — sign=0, magnitude=0
    #[test]
    fn signed_zero() {
        assert_eq!(signed_round_trip(0), 0);
    }

    /// Spec 7.1.5: encode/decode 1 — sign=0, magnitude=1
    #[test]
    fn signed_one() {
        assert_eq!(signed_round_trip(1), 1);
    }

    /// Spec 7.1.5: encode/decode -1 — sign=1, magnitude=0
    #[test]
    fn signed_minus_one() {
        assert_eq!(signed_round_trip(-1), -1);
        // Verify encoding: sign=1, magnitude=0 → 1 bit + 1 byte (0x00)
        let mut w = BitWriter::new();
        encode(&mut w, -1);
        let data = w.into_vec();
        // sign bit 1, then unsigned_integer(0) = 0x00
        // bit layout: 1_0000000 0 = 0x80 0x00
        assert_eq!(data, vec![0x80, 0x00]);
    }

    /// Spec 7.1.5: encode/decode -2 — sign=1, magnitude=1
    #[test]
    fn signed_minus_two() {
        assert_eq!(signed_round_trip(-2), -2);
    }

    /// Spec 7.1.5: large positive value
    #[test]
    fn signed_large_positive() {
        let val = i64::MAX / 2;
        assert_eq!(signed_round_trip(val), val);
    }

    /// Spec 7.1.5: large negative value (i64::MIN)
    #[test]
    fn signed_large_negative() {
        assert_eq!(signed_round_trip(i64::MIN), i64::MIN);
    }

    /// Spec 7.1.5: round-trip diverse values
    #[test]
    fn signed_round_trip_diverse() {
        for &val in &[0, 1, -1, 127, -128, i64::MAX, i64::MIN] {
            assert_eq!(signed_round_trip(val), val, "failed for {val}");
        }
    }

    /// Spec 7.1.5: decode EOF → PrematureEndOfStream
    #[test]
    fn signed_decode_eof() {
        let mut r = BitReader::new(&[]);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.5: decode overflow — positive magnitude too large for i64
    #[test]
    fn signed_decode_overflow_positive() {
        let mut w = BitWriter::new();
        w.write_bit(false); // sign = positive
        unsigned_integer::encode(&mut w, u64::MAX);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::IntegerOverflow);
    }

    /// Spec 7.1.5: decode overflow — negative magnitude too large for i64
    #[test]
    fn signed_decode_overflow_negative() {
        let mut w = BitWriter::new();
        w.write_bit(true); // sign = negative
        // magnitude = i64::MAX as u64 + 1 → result would be -(2^63 + 1), overflows
        unsigned_integer::encode(&mut w, i64::MAX as u64 + 1);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::IntegerOverflow);
    }

    // --- Bounded (bounded case) tests ---

    /// Spec 7.1.5: bounded 0..255, n=8
    #[test]
    fn bounded_0_to_255() {
        assert_eq!(bounded_round_trip(0, 0, 255), 0);
        assert_eq!(bounded_round_trip(255, 0, 255), 255);
        assert_eq!(bounded_round_trip(128, 0, 255), 128);
    }

    /// Spec 7.1.5: bounded -5..5, range=11, n=4
    #[test]
    fn bounded_minus5_to_5() {
        assert_eq!(ceiling_log2(11), 4);
        for val in -5..=5 {
            assert_eq!(bounded_round_trip(val, -5, 5), val, "failed for {val}");
        }
    }

    /// Spec 7.1.5: bounded 0..0, range=1, n=0 (single value, omitted)
    #[test]
    fn bounded_single_value() {
        assert_eq!(ceiling_log2(1), 0);
        let mut w = BitWriter::new();
        encode_bounded(&mut w, 0, 0, 0);
        assert_eq!(w.bit_position(), 0);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode_bounded(&mut r, 0, 0).unwrap(), 0);
    }

    /// Spec 7.1.5: bounded 0..4095, n=12 (max allowed range)
    #[test]
    fn bounded_0_to_4095() {
        assert_eq!(bounded_round_trip(0, 0, 4095), 0);
        assert_eq!(bounded_round_trip(4095, 0, 4095), 4095);
        assert_eq!(bounded_round_trip(2048, 0, 4095), 2048);
    }

    /// Spec 7.1.5: range=1 means n=0, value is omitted
    #[test]
    fn bounded_range_1() {
        assert_eq!(bounded_round_trip(42, 42, 42), 42);
    }

    /// Spec 7.1.5: value out of range panics
    #[test]
    #[should_panic(expected = "not in")]
    fn bounded_value_out_of_range() {
        let mut w = BitWriter::new();
        encode_bounded(&mut w, 10, 0, 5);
    }

    /// Spec 7.1.5: range > 4096 panics
    #[test]
    #[should_panic(expected = "exceeds 4096")]
    fn bounded_range_too_large() {
        let mut w = BitWriter::new();
        encode_bounded(&mut w, 0, 0, 4096);
    }

    /// Spec 7.1.5: extreme bounds (i64::MIN..i64::MAX) must panic, not wrap
    #[test]
    #[should_panic(expected = "exceeds 4096")]
    fn bounded_range_extreme_bounds() {
        let mut w = BitWriter::new();
        encode_bounded(&mut w, 0, i64::MIN, i64::MAX);
    }

    /// Spec 7.1.5: bounded decode with corrupted offset exceeding range
    #[test]
    fn bounded_decode_offset_out_of_range() {
        // range=3 (min=0, max=2), n=2 bits. Write offset=3 which is >= range.
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 3, 2);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode_bounded(&mut r, 0, 2).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    /// Spec 7.1.5: bounded round-trip diverse values
    #[test]
    fn bounded_round_trip_diverse() {
        // Range -100..100 (range=201, n=8)
        for val in [-100, -50, -1, 0, 1, 50, 100] {
            assert_eq!(bounded_round_trip(val, -100, 100), val, "failed for {val}");
        }
    }
}
