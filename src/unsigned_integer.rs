//! Variable-length 7-bit unsigned integer encoding (Spec 7.1.6).
//!
//! Each octet has a continuation bit (MSB) and 7 data bits. The least
//! significant group is written first. The last octet has continuation = 0.

use crate::bitstream::{BitReader, BitWriter};
use crate::{Error, Result};

/// Encodes a `u64` as a variable-length unsigned integer (Spec 7.1.6).
#[inline]
pub fn encode(writer: &mut BitWriter, value: u64) {
    if value < 128 {
        // Fast-Path: Single-Byte (häufigster Fall — ASCII Codepoints, kleine Längen)
        writer.write_byte_aligned(value as u8);
        return;
    }
    let mut v = value;
    loop {
        let low7 = (v & 0x7F) as u8;
        v >>= 7;
        if v == 0 {
            writer.write_byte_aligned(low7);
            break;
        }
        writer.write_byte_aligned(0x80 | low7);
    }
}

/// Decodes a variable-length unsigned integer from the stream (Spec 7.1.6).
#[inline]
pub fn decode(reader: &mut BitReader) -> Result<u64> {
    let byte = reader.read_byte_aligned()?;
    if byte & 0x80 == 0 {
        // Fast-Path: Single-Byte (häufigster Fall — ASCII Codepoints, kleine Längen)
        return Ok(u64::from(byte));
    }
    // Multi-Byte: erstes Byte bereits gelesen
    let mut result = u64::from(byte & 0x7F);
    let mut shift: u32 = 7;
    loop {
        let byte = reader.read_byte_aligned()?;
        let data = u64::from(byte & 0x7F);
        // Overflow-Prüfung (Spec 7.1.6): Bei shift 63 (10. Byte) ist nur
        // Daten-Bit 0 gültig (u64 hat 64 Bits), und kein Continuation-Byte.
        if shift == 63 && (data > 1 || byte & 0x80 != 0) {
            return Err(Error::IntegerOverflow);
        }
        result |= data << shift;
        if byte & 0x80 == 0 {
            return Ok(result);
        }
        shift += 7;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trip(value: u64) -> u64 {
        let mut w = BitWriter::new();
        encode(&mut w, value);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r).unwrap()
    }

    // Spec 7.1.6: smallest value, single byte
    #[test]
    fn encode_decode_0() {
        assert_eq!(round_trip(0), 0);
    }

    #[test]
    fn encode_decode_1() {
        assert_eq!(round_trip(1), 1);
    }

    // Spec 7.1.6: max single-byte value (7 data bits)
    #[test]
    fn encode_decode_127() {
        assert_eq!(round_trip(127), 127);
        // Verify single byte encoding
        let mut w = BitWriter::new();
        encode(&mut w, 127);
        let data = w.into_vec();
        assert_eq!(data.len(), 1);
        assert_eq!(data[0], 0x7F); // 0_1111111
    }

    // Spec 7.1.6: min two-byte value
    #[test]
    fn encode_decode_128() {
        assert_eq!(round_trip(128), 128);
        let mut w = BitWriter::new();
        encode(&mut w, 128);
        let data = w.into_vec();
        assert_eq!(data.len(), 2);
        // 128 = 0b1_0000000 → low7=0x00 with cont=1, then 0x01 with cont=0
        assert_eq!(data[0], 0x80); // 1_0000000
        assert_eq!(data[1], 0x01); // 0_0000001
    }

    // Spec 7.1.6: max two-byte value
    #[test]
    fn encode_decode_16383() {
        assert_eq!(round_trip(16383), 16383);
        let mut w = BitWriter::new();
        encode(&mut w, 16383);
        let data = w.into_vec();
        assert_eq!(data.len(), 2);
        // 16383 = 0x3FFF = 0b11_1111111111111
        // low7 = 0x7F with cont=1, high7 = 0x7F with cont=0
        assert_eq!(data[0], 0xFF); // 1_1111111
        assert_eq!(data[1], 0x7F); // 0_1111111
    }

    #[test]
    fn encode_decode_2_pow_31_minus_1() {
        let val = (1u64 << 31) - 1;
        assert_eq!(round_trip(val), val);
    }

    #[test]
    fn encode_decode_large_values() {
        let val = u64::MAX / 2;
        assert_eq!(round_trip(val), val);
        assert_eq!(round_trip(u64::MAX), u64::MAX);
    }

    // Example 7-1 from the spec: value 10 encodes as single byte 0x0A
    #[test]
    fn spec_example_7_1_value_10() {
        let mut w = BitWriter::new();
        encode(&mut w, 10);
        let data = w.into_vec();
        assert_eq!(data, vec![0x0A]);
    }

    // Example 7-1 from the spec: value 201 = 0b11001001
    // low7 = 0b1001001 = 0x49, cont=1 → 0xC9
    // high7 = 0b0000001 = 0x01, cont=0 → 0x01
    #[test]
    fn spec_example_7_1_value_201() {
        let mut w = BitWriter::new();
        encode(&mut w, 201);
        let data = w.into_vec();
        assert_eq!(data, vec![0xC9, 0x01]);
    }

    #[test]
    fn decode_premature_end_of_stream() {
        // Empty stream
        let mut r = BitReader::new(&[]);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);

        // Continuation bit set but no more bytes
        let mut r = BitReader::new(&[0x80]);
        let err = decode(&mut r).unwrap_err();
        assert_eq!(err, Error::PrematureEndOfStream);
    }

    #[test]
    fn round_trip_diverse_values() {
        for &val in &[
            0,
            1,
            2,
            63,
            64,
            127,
            128,
            255,
            256,
            16383,
            16384,
            1_000_000,
            u64::MAX,
        ] {
            assert_eq!(round_trip(val), val, "round-trip failed for {val}");
        }
    }

    #[test]
    fn decode_overflow_too_many_bytes() {
        // 10 continuation bytes (shift reaches 70) then a final byte
        let mut data = vec![0x80; 10];
        data.push(0x01);
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::IntegerOverflow);
    }

    // Spec 7.1.6: at shift==63 only data 0 or 1 is valid, and no continuation
    #[test]
    fn decode_overflow_shift63_continuation() {
        // 9 continuation bytes (shift 0..56), 10th byte with data=1 + continuation
        let mut data = vec![0x80; 9];
        data.push(0x81); // data=1, continuation=1 → overflow
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::IntegerOverflow);
    }

    #[test]
    fn decode_overflow_shift63_data_too_large() {
        // 9 continuation bytes, 10th byte with data=2, no continuation
        let mut data = vec![0x80; 9];
        data.push(0x02); // data=2 at shift=63 → overflow
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::IntegerOverflow);
    }
}
