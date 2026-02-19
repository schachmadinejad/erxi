//! List encoding (Spec 7.1.11).
//!
//! Values of type List are encoded as a length-prefixed sequence of values.
//! The length is an Unsigned Integer (Spec 7.1.6) and each value is encoded
//! according to its type.

use crate::bitstream::{BitReader, BitWriter};
use crate::{Error, Result, unsigned_integer};

/// Maximum list length to prevent OOM/DoS attacks from corrupted streams.
/// This is a conservative limit; can be adjusted based on use case.
pub const MAX_LIST_LENGTH: u64 = 1 << 24; // 16 million elements

/// Encodes a list as length-prefixed sequence of values (Spec 7.1.11).
///
/// The `encode_item` closure encodes each item according to its type.
pub fn encode<T, F>(writer: &mut BitWriter, items: &[T], mut encode_item: F)
where
    F: FnMut(&mut BitWriter, &T),
{
    unsigned_integer::encode(writer, items.len() as u64);
    for item in items {
        encode_item(writer, item);
    }
}

/// Decodes a list from a length-prefixed sequence of values (Spec 7.1.11).
///
/// The `decode_item` closure decodes each item according to its type.
///
/// Returns `Error::ListLengthOverflow` if the decoded length exceeds
/// `MAX_LIST_LENGTH` to prevent OOM/DoS from corrupted streams.
pub fn decode<T, F>(reader: &mut BitReader, mut decode_item: F) -> Result<Vec<T>>
where
    F: FnMut(&mut BitReader) -> Result<T>,
{
    let len = unsigned_integer::decode(reader)?;
    if len > MAX_LIST_LENGTH {
        return Err(Error::ListLengthOverflow(len));
    }
    let mut items = Vec::with_capacity(len as usize);
    for _ in 0..len {
        items.push(decode_item(reader)?);
    }
    Ok(items)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Spec 7.1.11: leere Liste — length=0, keine Werte
    #[test]
    fn leere_liste() {
        let mut w = BitWriter::new();
        encode(&mut w, &[] as &[u8], |w, &v| w.write_byte_aligned(v));
        let data = w.into_vec();
        assert_eq!(data, vec![0x00]); // nur length=0

        let mut r = BitReader::new(&data);
        let result: Vec<u8> = decode(&mut r, |r| r.read_byte_aligned()).unwrap();
        assert!(result.is_empty());
    }

    /// Spec 7.1.11: Liste mit String-Werten
    #[test]
    fn string_liste() {
        use crate::string;

        let items = vec!["foo".to_string(), "bar".to_string(), "baz".to_string()];
        let mut w = BitWriter::new();
        encode(&mut w, &items, |w, s| string::encode(w, s));
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result: Vec<String> = decode(&mut r, |r| string::decode(r)).unwrap();
        assert_eq!(result, items);
    }

    /// Spec 7.1.11: Liste mit Integer-Werten (als Unsigned Integer)
    #[test]
    fn integer_liste() {
        let items: Vec<u64> = vec![0, 127, 128, 16383, 16384];
        let mut w = BitWriter::new();
        encode(&mut w, &items, |w, &v| unsigned_integer::encode(w, v));
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result: Vec<u64> = decode(&mut r, |r| unsigned_integer::decode(r)).unwrap();
        assert_eq!(result, items);
    }

    /// Spec 7.1.11: einzelnes Element
    #[test]
    fn einzelnes_element() {
        let items: Vec<u8> = vec![0xAB];
        let mut w = BitWriter::new();
        encode(&mut w, &items, |w, &v| w.write_byte_aligned(v));
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result: Vec<u8> = decode(&mut r, |r| r.read_byte_aligned()).unwrap();
        assert_eq!(result, items);
    }

    /// Spec 7.1.11: mehrere Elemente
    #[test]
    fn mehrere_elemente() {
        let items: Vec<u8> = vec![0x01, 0x02, 0x03, 0x04, 0x05];
        let mut w = BitWriter::new();
        encode(&mut w, &items, |w, &v| w.write_byte_aligned(v));
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result: Vec<u8> = decode(&mut r, |r| r.read_byte_aligned()).unwrap();
        assert_eq!(result, items);
    }

    /// Spec 7.1.11: decode EOF bei length
    #[test]
    fn decode_eof_bei_length() {
        let mut r = BitReader::new(&[]);
        let result: Result<Vec<u8>> = decode(&mut r, |r| r.read_byte_aligned());
        assert_eq!(result.unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.11: decode EOF bei Elementen
    #[test]
    fn decode_eof_bei_elementen() {
        let mut w = BitWriter::new();
        unsigned_integer::encode(&mut w, 3); // length=3
        w.write_byte_aligned(0x01); // nur 1 Element
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result: Result<Vec<u8>> = decode(&mut r, |r| r.read_byte_aligned());
        assert_eq!(result.unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.11: sequentielle Listen im Stream
    #[test]
    fn sequentielle_listen() {
        let mut w = BitWriter::new();
        encode(&mut w, &[1u8, 2], |w, &v| w.write_byte_aligned(v));
        encode(&mut w, &[] as &[u8], |w, &v| w.write_byte_aligned(v));
        encode(&mut w, &[0xFFu8], |w, &v| w.write_byte_aligned(v));
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(
            decode(&mut r, |r| r.read_byte_aligned()).unwrap(),
            vec![1u8, 2]
        );
        assert_eq!(
            decode(&mut r, |r| r.read_byte_aligned()).unwrap(),
            Vec::<u8>::new()
        );
        assert_eq!(
            decode(&mut r, |r| r.read_byte_aligned()).unwrap(),
            vec![0xFFu8]
        );
    }

    /// Spec 7.1.11: length entspricht Elementanzahl
    #[test]
    fn length_ist_elementanzahl() {
        let items: Vec<u64> = vec![100, 200, 300];
        let mut w = BitWriter::new();
        encode(&mut w, &items, |w, &v| unsigned_integer::encode(w, v));
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let len = unsigned_integer::decode(&mut r).unwrap();
        assert_eq!(len, 3);
    }

    /// Spec 7.1.11: Längenüberlauf bei korruptem Stream (DoS-Schutz)
    #[test]
    fn decode_laenge_overflow() {
        let mut w = BitWriter::new();
        // Länge größer als MAX_LIST_LENGTH encodieren
        unsigned_integer::encode(&mut w, super::MAX_LIST_LENGTH + 1);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result: Result<Vec<u8>> = decode(&mut r, |r| r.read_byte_aligned());
        assert_eq!(
            result.unwrap_err(),
            Error::ListLengthOverflow(super::MAX_LIST_LENGTH + 1)
        );
    }
}
