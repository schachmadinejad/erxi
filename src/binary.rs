//! Binary encoding (Spec 7.1.1).
//!
//! The Binary datatype representation is a length-prefixed sequence of octets.
//! The length is represented as an Unsigned Integer (Spec 7.1.6).

use crate::bitstream::{BitReader, BitWriter};
use crate::{Result, unsigned_integer};

/// Encodes binary data as a length-prefixed sequence of octets (Spec 7.1.1).
pub fn encode(writer: &mut BitWriter, value: &[u8]) {
    unsigned_integer::encode(writer, value.len() as u64);
    for &byte in value {
        writer.write_byte_aligned(byte);
    }
}

/// Decodes binary data from a length-prefixed sequence of octets (Spec 7.1.1).
pub fn decode(reader: &mut BitReader) -> Result<Vec<u8>> {
    let len = unsigned_integer::decode(reader)?;
    let mut buf = Vec::new();
    for _ in 0..len {
        buf.push(reader.read_byte_aligned()?);
    }
    Ok(buf)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Error;

    fn round_trip(value: &[u8]) -> Vec<u8> {
        let mut w = BitWriter::new();
        encode(&mut w, value);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r).unwrap()
    }

    /// Spec 7.1.1: empty binary — length=0, no octets
    #[test]
    fn empty_binary() {
        assert_eq!(round_trip(&[]), Vec::<u8>::new());
        let mut w = BitWriter::new();
        encode(&mut w, &[]);
        let data = w.into_vec();
        assert_eq!(data, vec![0x00]); // length=0
    }

    /// Spec 7.1.1: single octet
    #[test]
    fn single_octet() {
        assert_eq!(round_trip(&[0xAB]), vec![0xAB]);
    }

    /// Spec 7.1.1: multiple octets
    #[test]
    fn multiple_octets() {
        let input = vec![0xDE, 0xAD, 0xBE, 0xEF];
        assert_eq!(round_trip(&input), input);
    }

    /// Spec 7.1.1: all zero bytes
    #[test]
    fn all_zeros() {
        let input = vec![0x00; 10];
        assert_eq!(round_trip(&input), input);
    }

    /// Spec 7.1.1: all 0xFF bytes
    #[test]
    fn all_ones() {
        let input = vec![0xFF; 10];
        assert_eq!(round_trip(&input), input);
    }

    /// Spec 7.1.1: decode EOF on length
    #[test]
    fn decode_eof_on_length() {
        let mut r = BitReader::new(&[]);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.1: decode EOF on octet data
    #[test]
    fn decode_eof_on_data() {
        let mut w = BitWriter::new();
        unsigned_integer::encode(&mut w, 5); // length=5
        w.write_byte_aligned(0xAA); // only 1 octet
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.1: sequential binary values in a stream
    #[test]
    fn sequential_binaries() {
        let mut w = BitWriter::new();
        encode(&mut w, &[0x01, 0x02]);
        encode(&mut w, &[]);
        encode(&mut w, &[0xFF]);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap(), vec![0x01, 0x02]);
        assert_eq!(decode(&mut r).unwrap(), Vec::<u8>::new());
        assert_eq!(decode(&mut r).unwrap(), vec![0xFF]);
    }

    /// Spec 7.1.1: length is octet count
    #[test]
    fn length_is_octet_count() {
        let mut w = BitWriter::new();
        encode(&mut w, &[0x01, 0x02, 0x03]);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        let len = unsigned_integer::decode(&mut r).unwrap();
        assert_eq!(len, 3);
    }
}
