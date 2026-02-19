//! Decimal encoding (Spec 7.1.3).
//!
//! The Decimal datatype representation is a Boolean sign (Spec 7.1.2) followed
//! by two Unsigned Integers (Spec 7.1.6): the integral portion and the
//! fractional portion. The fractional digits are stored in reverse order to
//! preserve leading zeros. A sign of 0 (false) represents positive, 1 (true)
//! represents negative. Minus zero is representable (sign=1, integral=0,
//! fractional=0).

use crate::bitstream::{BitReader, BitWriter};
use crate::{Result, boolean, unsigned_integer};

/// A decoded EXI decimal value (Spec 7.1.3).
///
/// The fractional part stores digits in reverse order as they appear in the
/// EXI stream. For example, the decimal "12.340" has `integral=12` and
/// `fractional=43` (the fractional digits "3","4","0" are reversed to
/// "0","4","3", which as a u64 is 43; the original trailing zero is lost).
///
/// To reconstruct the original fractional string, reverse the digits of
/// `fractional` and prepend zeros up to the original digit count (which
/// must be tracked externally if leading zeros matter).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Decimal {
    /// True if the value is negative (including minus zero).
    pub negative: bool,
    /// The integral portion of the decimal value.
    pub integral: u64,
    /// The fractional portion with digits in reverse order.
    pub fractional: u64,
}

/// Encodes a decimal value (Spec 7.1.3).
pub fn encode(writer: &mut BitWriter, value: Decimal) {
    boolean::encode(writer, value.negative);
    unsigned_integer::encode(writer, value.integral);
    unsigned_integer::encode(writer, value.fractional);
}

/// Decodes a decimal value (Spec 7.1.3).
pub fn decode(reader: &mut BitReader) -> Result<Decimal> {
    let negative = boolean::decode(reader)?;
    let integral = unsigned_integer::decode(reader)?;
    let fractional = unsigned_integer::decode(reader)?;
    Ok(Decimal {
        negative,
        integral,
        fractional,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Error;

    fn round_trip(value: Decimal) -> Decimal {
        let mut w = BitWriter::new();
        encode(&mut w, value);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r).unwrap()
    }

    /// Spec 7.1.3: positive decimal (12.34 → sign=0, integral=12, fractional=43)
    #[test]
    fn positive_decimal() {
        let d = Decimal {
            negative: false,
            integral: 12,
            fractional: 43, // "34" reversed = "43"
        };
        assert_eq!(round_trip(d), d);
    }

    /// Spec 7.1.3: negative decimal (-5.6 → sign=1, integral=5, fractional=6)
    #[test]
    fn negative_decimal() {
        let d = Decimal {
            negative: true,
            integral: 5,
            fractional: 6,
        };
        assert_eq!(round_trip(d), d);
    }

    /// Spec 7.1.3: minus zero is representable
    #[test]
    fn minus_zero() {
        let d = Decimal {
            negative: true,
            integral: 0,
            fractional: 0,
        };
        assert_eq!(round_trip(d), d);
        // Verify it's distinct from positive zero
        let pos_zero = Decimal {
            negative: false,
            integral: 0,
            fractional: 0,
        };
        assert_ne!(d, pos_zero);
    }

    /// Spec 7.1.3: positive zero
    #[test]
    fn positive_zero() {
        let d = Decimal {
            negative: false,
            integral: 0,
            fractional: 0,
        };
        assert_eq!(round_trip(d), d);
    }

    /// Spec 7.1.3: integer-only decimal (no fractional part)
    #[test]
    fn integer_only() {
        let d = Decimal {
            negative: false,
            integral: 42,
            fractional: 0,
        };
        assert_eq!(round_trip(d), d);
    }

    /// Spec 7.1.3: fractional-only decimal (0.123 → integral=0, fractional=321)
    #[test]
    fn fractional_only() {
        let d = Decimal {
            negative: false,
            integral: 0,
            fractional: 321, // "123" reversed
        };
        assert_eq!(round_trip(d), d);
    }

    /// Spec 7.1.3: large values
    #[test]
    fn large_values() {
        let d = Decimal {
            negative: false,
            integral: u64::MAX,
            fractional: u64::MAX,
        };
        assert_eq!(round_trip(d), d);
    }

    /// Spec 7.1.3: decode EOF on sign
    #[test]
    fn decode_eof_on_sign() {
        let mut r = BitReader::new(&[]);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.3: decode EOF on integral
    #[test]
    fn decode_eof_on_integral() {
        let mut w = BitWriter::new();
        boolean::encode(&mut w, false); // sign
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.3: decode EOF on fractional
    #[test]
    fn decode_eof_on_fractional() {
        let mut w = BitWriter::new();
        boolean::encode(&mut w, false);
        unsigned_integer::encode(&mut w, 0);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.3: sequential decimals in a stream
    #[test]
    fn sequential_decimals() {
        let d1 = Decimal {
            negative: false,
            integral: 1,
            fractional: 2,
        };
        let d2 = Decimal {
            negative: true,
            integral: 0,
            fractional: 0,
        };
        let d3 = Decimal {
            negative: false,
            integral: 999,
            fractional: 999,
        };

        let mut w = BitWriter::new();
        encode(&mut w, d1);
        encode(&mut w, d2);
        encode(&mut w, d3);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap(), d1);
        assert_eq!(decode(&mut r).unwrap(), d2);
        assert_eq!(decode(&mut r).unwrap(), d3);
    }

    /// Spec 7.1.3: sign bit uses exactly 1 bit (Boolean encoding)
    #[test]
    fn sign_uses_one_bit() {
        let mut w = BitWriter::new();
        boolean::encode(&mut w, false);
        assert_eq!(w.bit_position(), 1);
    }
}
