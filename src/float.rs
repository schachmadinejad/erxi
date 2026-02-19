//! Float encoding (Spec 7.1.4).
//!
//! The Float datatype representation is two consecutive Integers (Spec 7.1.5):
//! a mantissa and a base-10 exponent. The mantissa range is -(2^63) to 2^63-1,
//! and the exponent range is -(2^14-1) to 2^14-1.
//!
//! The special exponent value -(2^14) encodes infinity, negative infinity and
//! NaN: mantissa 1 = INF, mantissa -1 = -INF, any other mantissa = NaN.
//!
//! Mantissa or exponent values outside the accepted range MUST NOT be used
//! (Spec 7.1.4).

use crate::bitstream::{BitReader, BitWriter};
use crate::{Error, Result, integer};

/// Normal exponent range: -(2^14-1) to 2^14-1.
const EXPONENT_MIN: i64 = -(1 << 14) + 1; // -16383
const EXPONENT_MAX: i64 = (1 << 14) - 1; // 16383

/// Special exponent value for INF, -INF, NaN.
const SPECIAL_EXPONENT: i64 = -(1 << 14); // -16384

/// A decoded EXI float value (Spec 7.1.4).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Float {
    /// A finite value: m × 10^e.
    Value { mantissa: i64, exponent: i64 },
    /// Positive infinity (INF).
    Infinity,
    /// Negative infinity (-INF).
    NegativeInfinity,
    /// Not-a-Number (NaN).
    NaN,
}

/// Encodes a float value (Spec 7.1.4).
///
/// # Panics
///
/// Panics if a `Value` has exponent outside the accepted range.
pub fn encode(writer: &mut BitWriter, value: Float) {
    match value {
        Float::Value { mantissa, exponent } => {
            assert!(
                (EXPONENT_MIN..=EXPONENT_MAX).contains(&exponent),
                "exponent {exponent} out of range [{EXPONENT_MIN}, {EXPONENT_MAX}]"
            );
            integer::encode(writer, mantissa);
            integer::encode(writer, exponent);
        }
        Float::Infinity => {
            integer::encode(writer, 1);
            integer::encode(writer, SPECIAL_EXPONENT);
        }
        Float::NegativeInfinity => {
            integer::encode(writer, -1);
            integer::encode(writer, SPECIAL_EXPONENT);
        }
        Float::NaN => {
            integer::encode(writer, 0);
            integer::encode(writer, SPECIAL_EXPONENT);
        }
    }
}

/// Decodes a float value (Spec 7.1.4).
///
/// Returns [`Error::FloatOutOfRange`] if the exponent exceeds the accepted
/// range. Returns [`Error::IntegerOverflow`] if the mantissa exceeds i64 range.
pub fn decode(reader: &mut BitReader) -> Result<Float> {
    let mantissa = integer::decode(reader)?;
    let exponent = integer::decode(reader)?;

    if exponent == SPECIAL_EXPONENT {
        return Ok(match mantissa {
            1 => Float::Infinity,
            -1 => Float::NegativeInfinity,
            _ => Float::NaN,
        });
    }

    // Mantissa is represented as i64, which inherently covers the spec range -(2^63) to 2^63-1.
    // Exponent must be in -(2^14-1)..2^14-1 for normal values.
    if !(EXPONENT_MIN..=EXPONENT_MAX).contains(&exponent) {
        return Err(Error::FloatOutOfRange);
    }

    Ok(Float::Value { mantissa, exponent })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unsigned_integer;

    fn round_trip(value: Float) -> Float {
        let mut w = BitWriter::new();
        encode(&mut w, value);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r).unwrap()
    }

    /// Spec 7.1.4: basic float value 1.5 = 15 × 10^(-1)
    #[test]
    fn basic_value() {
        let f = Float::Value {
            mantissa: 15,
            exponent: -1,
        };
        assert_eq!(round_trip(f), f);
    }

    /// Spec 7.1.4: zero
    #[test]
    fn zero() {
        let f = Float::Value {
            mantissa: 0,
            exponent: 0,
        };
        assert_eq!(round_trip(f), f);
    }

    /// Spec 7.1.4: negative mantissa
    #[test]
    fn negative_mantissa() {
        let f = Float::Value {
            mantissa: -42,
            exponent: 3,
        };
        assert_eq!(round_trip(f), f);
    }

    /// Spec 7.1.4: negative exponent
    #[test]
    fn negative_exponent() {
        let f = Float::Value {
            mantissa: 123,
            exponent: -100,
        };
        assert_eq!(round_trip(f), f);
    }

    /// Spec 7.1.4: mantissa boundary values
    #[test]
    fn mantissa_boundaries() {
        let f_min = Float::Value {
            mantissa: i64::MIN,
            exponent: 0,
        };
        assert_eq!(round_trip(f_min), f_min);

        let f_max = Float::Value {
            mantissa: i64::MAX,
            exponent: 0,
        };
        assert_eq!(round_trip(f_max), f_max);
    }

    /// Spec 7.1.4: exponent boundary values
    #[test]
    fn exponent_boundaries() {
        let f_min = Float::Value {
            mantissa: 0,
            exponent: EXPONENT_MIN,
        };
        assert_eq!(round_trip(f_min), f_min);

        let f_max = Float::Value {
            mantissa: 0,
            exponent: EXPONENT_MAX,
        };
        assert_eq!(round_trip(f_max), f_max);
    }

    /// Spec 7.1.4: positive infinity
    #[test]
    fn infinity() {
        assert_eq!(round_trip(Float::Infinity), Float::Infinity);
    }

    /// Spec 7.1.4: negative infinity
    #[test]
    fn negative_infinity() {
        assert_eq!(round_trip(Float::NegativeInfinity), Float::NegativeInfinity);
    }

    /// Spec 7.1.4: NaN
    #[test]
    fn nan() {
        assert_eq!(round_trip(Float::NaN), Float::NaN);
    }

    /// Spec 7.1.4: NaN with mantissa=0 (our encoding choice)
    #[test]
    fn nan_encoding_uses_mantissa_zero() {
        let mut w = BitWriter::new();
        encode(&mut w, Float::NaN);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        let mantissa = integer::decode(&mut r).unwrap();
        let exponent = integer::decode(&mut r).unwrap();
        assert_eq!(mantissa, 0);
        assert_eq!(exponent, SPECIAL_EXPONENT);
    }

    /// Spec 7.1.4: any mantissa other than 1/-1 with special exponent → NaN
    #[test]
    fn special_exponent_other_mantissa_is_nan() {
        for &m in &[0, 2, -2, 42, -42, i64::MAX, i64::MIN] {
            let mut w = BitWriter::new();
            integer::encode(&mut w, m);
            integer::encode(&mut w, SPECIAL_EXPONENT);
            let data = w.into_vec();
            let mut r = BitReader::new(&data);
            assert_eq!(decode(&mut r).unwrap(), Float::NaN, "mantissa={m}");
        }
    }

    /// Spec 7.1.4: decode EOF
    #[test]
    fn decode_eof() {
        let mut r = BitReader::new(&[]);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.4: mantissa overflow → IntegerOverflow (not FloatOutOfRange)
    #[test]
    fn decode_mantissa_overflow() {
        // Encode a positive sign (0) + magnitude > i64::MAX via unsigned_integer
        let mut w = BitWriter::new();
        w.write_bit(false); // sign = positive
        unsigned_integer::encode(&mut w, u64::MAX); // magnitude > i64::MAX
        integer::encode(&mut w, 0); // exponent
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::IntegerOverflow);
    }

    /// Spec 7.1.4: decode EOF on exponent
    #[test]
    fn decode_eof_on_exponent() {
        let mut w = BitWriter::new();
        integer::encode(&mut w, 0); // mantissa only
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Spec 7.1.4: sequential floats in a stream
    #[test]
    fn sequential_floats() {
        let values = [
            Float::Value {
                mantissa: 1,
                exponent: 0,
            },
            Float::Infinity,
            Float::NaN,
            Float::Value {
                mantissa: -99,
                exponent: -5,
            },
        ];

        let mut w = BitWriter::new();
        for &v in &values {
            encode(&mut w, v);
        }
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        for &expected in &values {
            assert_eq!(decode(&mut r).unwrap(), expected);
        }
    }

    /// Spec 7.1.4: exponent out of range on decode → FloatOutOfRange
    #[test]
    fn decode_exponent_out_of_range() {
        // Exponent = EXPONENT_MAX + 1 = 16384, which is outside normal range
        // but not the special exponent (-16384)
        let mut w = BitWriter::new();
        integer::encode(&mut w, 0);
        integer::encode(&mut w, EXPONENT_MAX + 1);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::FloatOutOfRange);
    }

    /// Spec 7.1.4: exponent below range on decode → FloatOutOfRange
    #[test]
    fn decode_exponent_below_range() {
        // Exponent = EXPONENT_MIN - 2 = -16385 (EXPONENT_MIN - 1 is SPECIAL_EXPONENT)
        let mut w = BitWriter::new();
        integer::encode(&mut w, 0);
        integer::encode(&mut w, EXPONENT_MIN - 2);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r).unwrap_err(), Error::FloatOutOfRange);
    }

    /// Spec 7.1.4: special exponent as Value panics on encode (boundary)
    #[test]
    #[should_panic(expected = "exponent")]
    fn encode_value_with_special_exponent_panics() {
        let mut w = BitWriter::new();
        encode(
            &mut w,
            Float::Value {
                mantissa: 0,
                exponent: SPECIAL_EXPONENT,
            },
        );
    }

    /// Spec 7.1.4: exponent out of range panics on encode
    #[test]
    #[should_panic(expected = "exponent")]
    fn encode_exponent_out_of_range_high() {
        let mut w = BitWriter::new();
        encode(
            &mut w,
            Float::Value {
                mantissa: 0,
                exponent: EXPONENT_MAX + 1,
            },
        );
    }

    /// Spec 7.1.4: exponent below range panics on encode
    #[test]
    #[should_panic(expected = "exponent")]
    fn encode_exponent_out_of_range_low() {
        let mut w = BitWriter::new();
        // EXPONENT_MIN - 1 = -16384, but that's the SPECIAL_EXPONENT
        // EXPONENT_MIN - 2 = -16385
        encode(
            &mut w,
            Float::Value {
                mantissa: 0,
                exponent: EXPONENT_MIN - 2,
            },
        );
    }
}
