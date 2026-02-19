//! Enumeration encoding (Spec 7.2).
//!
//! Enumerated values are encoded as n-bit Unsigned Integers where n = ⌈log₂(m)⌉
//! and m is the number of items in the enumerated type. The unsigned integer
//! value corresponds to the ordinal position (0-based) in schema-order.
//!
//! **Ausnahmen (Spec 7.2):** union, list, QName, Notation und davon abgeleitete
//! Typen werden NICHT als Enumerations encodiert, sondern nach ihrem jeweiligen
//! built-in EXI datatype representation. Diese Entscheidung liegt beim EXI-Prozessor.

use crate::bitstream::{BitReader, BitWriter};
use crate::{Error, Result, n_bit_unsigned_integer};

/// Berechnet die Bitbreite für eine Enumeration mit `m` Werten (Spec 7.2).
///
/// n = ⌈log₂(m)⌉, wobei für m=0 und m=1 gilt: n=0 (keine Bits nötig).
#[inline]
pub fn bit_width(m: usize) -> u8 {
    crate::bit_width::for_count(m)
}

/// Encodes an enumeration index as n-bit Unsigned Integer (Spec 7.2).
///
/// Returns `Error::InvalidEnumerationIndex` if `index >= enum_count`.
pub fn encode(writer: &mut BitWriter, index: usize, enum_count: usize) -> Result<()> {
    if index >= enum_count {
        return Err(Error::InvalidEnumerationIndex { index, enum_count });
    }
    let n = bit_width(enum_count);
    n_bit_unsigned_integer::encode(writer, index as u64, n);
    Ok(())
}

/// Decodes an enumeration index from n-bit Unsigned Integer (Spec 7.2).
///
/// Returns `Error::InvalidEnumerationIndex` if the decoded index >= enum_count
/// (can happen with corrupted streams when enum_count is not a power of two).
pub fn decode(reader: &mut BitReader, enum_count: usize) -> Result<usize> {
    let n = bit_width(enum_count);
    let index = n_bit_unsigned_integer::decode(reader, n)? as usize;
    if index >= enum_count {
        return Err(Error::InvalidEnumerationIndex { index, enum_count });
    }
    Ok(index)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trip(index: usize, enum_count: usize) -> usize {
        let mut w = BitWriter::new();
        encode(&mut w, index, enum_count).unwrap();
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r, enum_count).unwrap()
    }

    // Spec 7.2: n = ceil(log2(m))
    #[test]
    fn bit_width_berechnung() {
        // m=0: n=0 (Sonderfall, keine gültigen Werte)
        assert_eq!(bit_width(0), 0);
        // m=1: n=0 (nur Index 0 möglich, keine Bits nötig)
        assert_eq!(bit_width(1), 0);
        // m=2: n=1 (0..1)
        assert_eq!(bit_width(2), 1);
        // m=3: n=2 (0..2)
        assert_eq!(bit_width(3), 2);
        // m=4: n=2 (0..3)
        assert_eq!(bit_width(4), 2);
        // m=5: n=3 (0..4)
        assert_eq!(bit_width(5), 3);
        // m=8: n=3 (0..7)
        assert_eq!(bit_width(8), 3);
        // m=9: n=4 (0..8)
        assert_eq!(bit_width(9), 4);
        // m=16: n=4 (0..15)
        assert_eq!(bit_width(16), 4);
        // m=17: n=5 (0..16)
        assert_eq!(bit_width(17), 5);
        // m=256: n=8 (0..255)
        assert_eq!(bit_width(256), 8);
        // m=257: n=9 (0..256)
        assert_eq!(bit_width(257), 9);
    }

    // Spec 7.2: kleine Enumeration (2 Werte, 1 Bit)
    #[test]
    fn kleine_enum_zwei_werte() {
        assert_eq!(round_trip(0, 2), 0);
        assert_eq!(round_trip(1, 2), 1);

        // Prüfe dass wirklich nur 1 Bit verwendet wird
        let mut w = BitWriter::new();
        encode(&mut w, 0, 2).unwrap();
        assert_eq!(w.bit_position(), 1);
    }

    // Spec 7.2: kleine Enumeration (4 Werte, 2 Bits)
    #[test]
    fn kleine_enum_vier_werte() {
        for i in 0..4 {
            assert_eq!(round_trip(i, 4), i);
        }

        let mut w = BitWriter::new();
        encode(&mut w, 0, 4).unwrap();
        assert_eq!(w.bit_position(), 2);
    }

    // Spec 7.2: Enumeration mit einem Wert (0 Bits, Wert wird weggelassen)
    #[test]
    fn enum_ein_wert() {
        let mut w = BitWriter::new();
        encode(&mut w, 0, 1).unwrap();
        assert_eq!(w.bit_position(), 0);
        let data = w.into_vec();
        assert!(data.is_empty());

        let mut r = BitReader::new(&[]);
        assert_eq!(decode(&mut r, 1).unwrap(), 0);
    }

    // Spec 7.2: große Enumeration (256 Werte, 8 Bits)
    #[test]
    fn grosse_enum_256_werte() {
        assert_eq!(round_trip(0, 256), 0);
        assert_eq!(round_trip(255, 256), 255);
        assert_eq!(round_trip(128, 256), 128);

        let mut w = BitWriter::new();
        encode(&mut w, 0, 256).unwrap();
        assert_eq!(w.bit_position(), 8);
    }

    // Spec 7.2: große Enumeration (1000 Werte, 10 Bits)
    #[test]
    fn grosse_enum_1000_werte() {
        assert_eq!(round_trip(0, 1000), 0);
        assert_eq!(round_trip(999, 1000), 999);
        assert_eq!(round_trip(500, 1000), 500);

        let mut w = BitWriter::new();
        encode(&mut w, 0, 1000).unwrap();
        assert_eq!(w.bit_position(), 10);
    }

    // Spec 7.2: decode EOF
    #[test]
    fn decode_eof() {
        let mut r = BitReader::new(&[]);
        assert_eq!(decode(&mut r, 4).unwrap_err(), Error::PrematureEndOfStream);
    }

    // Spec 7.2: sequentielle Enum-Werte im Stream
    #[test]
    fn sequentielle_enums() {
        let mut w = BitWriter::new();
        encode(&mut w, 0, 4).unwrap(); // 2 bits
        encode(&mut w, 3, 4).unwrap(); // 2 bits
        encode(&mut w, 1, 2).unwrap(); // 1 bit
        encode(&mut w, 7, 8).unwrap(); // 3 bits (total: 8 bits = 1 byte)
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r, 4).unwrap(), 0);
        assert_eq!(decode(&mut r, 4).unwrap(), 3);
        assert_eq!(decode(&mut r, 2).unwrap(), 1);
        assert_eq!(decode(&mut r, 8).unwrap(), 7);
    }

    // Spec 7.2: encode gibt Error bei ungültigem Index zurück
    #[test]
    fn encode_index_out_of_bounds() {
        let mut w = BitWriter::new();
        let err = encode(&mut w, 4, 4).unwrap_err(); // 4 ist nicht gültig für enum mit 4 Werten (0..3)
        assert_eq!(
            err,
            Error::InvalidEnumerationIndex {
                index: 4,
                enum_count: 4
            }
        );
    }

    // Spec 7.2: encode gibt Error bei enum_count=0 zurück
    #[test]
    fn encode_enum_count_null() {
        let mut w = BitWriter::new();
        let err = encode(&mut w, 0, 0).unwrap_err();
        assert_eq!(
            err,
            Error::InvalidEnumerationIndex {
                index: 0,
                enum_count: 0
            }
        );
    }

    // Spec 7.2: "When there are more than one item that represent the same value..."
    // Diese Regel betrifft den EXI-Prozessor, nicht das Encoding selbst.
    // Der Test dokumentiert, dass mehrere Indices den gleichen Wert repräsentieren können.
    #[test]
    fn mehrere_items_gleicher_wert() {
        // Wenn in einer Enumeration [A, B, A] der Wert A sowohl Index 0 als auch 2 hat,
        // kann der Encoder entweder 0 oder 2 verwenden. Beide sind gültig.
        assert_eq!(round_trip(0, 3), 0);
        assert_eq!(round_trip(2, 3), 2);
        // Der Decoder muss beide akzeptieren und zurückgeben.
    }

    // Spec 7.2: Ungültiger Index bei nicht-Zweierpotenz
    // Bei enum_count=3 ist n=2 Bits, was Werte 0-3 erlaubt. Index 3 ist aber ungültig.
    #[test]
    fn decode_ungueltiger_index() {
        // enum_count=3, n=2 bits, aber Wert 3 im Stream
        let mut w = BitWriter::new();
        crate::n_bit_unsigned_integer::encode(&mut w, 3, 2); // ungültiger Index 3
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let err = decode(&mut r, 3).unwrap_err();
        assert_eq!(
            err,
            Error::InvalidEnumerationIndex {
                index: 3,
                enum_count: 3
            }
        );
    }
}
