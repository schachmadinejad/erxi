//! DateTime encoding (Spec 7.1.8).
//!
//! Supports all eight XML Schema date-time types as defined in Table 7-4.
//! Each type encodes a subset of the components Year, MonthDay, Time,
//! FractionalSecs, and TimeZone according to Table 7-3.

use crate::bitstream::{BitReader, BitWriter};
use crate::{Error, Result, boolean, integer, n_bit_unsigned_integer, unsigned_integer};

/// The XML Schema date-time type that determines which components are encoded.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DateTimeType {
    GYear,
    GYearMonth,
    Date,
    DateTime,
    GMonth,
    GMonthDay,
    GDay,
    Time,
}

impl DateTimeType {
    fn has_year(self) -> bool {
        matches!(
            self,
            Self::GYear | Self::GYearMonth | Self::Date | Self::DateTime
        )
    }

    fn has_month_day(self) -> bool {
        matches!(
            self,
            Self::GYearMonth
                | Self::Date
                | Self::DateTime
                | Self::GMonth
                | Self::GMonthDay
                | Self::GDay
        )
    }

    fn has_time(self) -> bool {
        matches!(self, Self::DateTime | Self::Time)
    }

    fn has_fractional_secs(self) -> bool {
        matches!(self, Self::DateTime | Self::Time)
    }
}

/// A decoded EXI date-time value (Spec 7.1.8).
///
/// Sum-Type mit 8 Varianten für die 8 XML Schema date-time Typen.
/// Jede Variante enthält nur die für diesen Typ relevanten Felder.
/// Year ist immer als Offset von 2000 gespeichert (z.B. 25 = Jahr 2025).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DateTime {
    /// gYear: year + optional timezone
    GYear {
        year: i64,
        timezone_offset_minutes: Option<i16>,
    },
    /// gYearMonth: year + month + day (wire-format, day aus MonthDay-Encoding) + optional timezone
    GYearMonth {
        year: i64,
        month: u8,
        day: u8,
        timezone_offset_minutes: Option<i16>,
    },
    /// date: year + month + day + optional timezone
    Date {
        year: i64,
        month: u8,
        day: u8,
        timezone_offset_minutes: Option<i16>,
    },
    /// dateTime: volle Datum/Zeit + optional timezone
    DateTime {
        year: i64,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
        /// Fractional seconds mit Ziffern in umgekehrter Reihenfolge.
        fractional_secs: Option<u64>,
        timezone_offset_minutes: Option<i16>,
    },
    /// gMonth: month + optional timezone
    GMonth {
        month: u8,
        timezone_offset_minutes: Option<i16>,
    },
    /// gMonthDay: month + day + optional timezone
    GMonthDay {
        month: u8,
        day: u8,
        timezone_offset_minutes: Option<i16>,
    },
    /// gDay: day + optional timezone
    GDay {
        day: u8,
        timezone_offset_minutes: Option<i16>,
    },
    /// time: Uhrzeit + optional timezone
    Time {
        hour: u8,
        minute: u8,
        second: u8,
        /// Fractional seconds mit Ziffern in umgekehrter Reihenfolge.
        fractional_secs: Option<u64>,
        timezone_offset_minutes: Option<i16>,
    },
}

impl DateTime {
    /// Gibt den `DateTimeType` für diese Variante zurück.
    #[inline]
    pub fn datetime_type(&self) -> DateTimeType {
        match self {
            Self::GYear { .. } => DateTimeType::GYear,
            Self::GYearMonth { .. } => DateTimeType::GYearMonth,
            Self::Date { .. } => DateTimeType::Date,
            Self::DateTime { .. } => DateTimeType::DateTime,
            Self::GMonth { .. } => DateTimeType::GMonth,
            Self::GMonthDay { .. } => DateTimeType::GMonthDay,
            Self::GDay { .. } => DateTimeType::GDay,
            Self::Time { .. } => DateTimeType::Time,
        }
    }

    /// Gibt den Timezone-Offset in Minuten zurück (falls vorhanden).
    #[inline]
    pub fn timezone_offset_minutes(&self) -> Option<i16> {
        match self {
            Self::GYear { timezone_offset_minutes, .. }
            | Self::GYearMonth { timezone_offset_minutes, .. }
            | Self::Date { timezone_offset_minutes, .. }
            | Self::DateTime { timezone_offset_minutes, .. }
            | Self::GMonth { timezone_offset_minutes, .. }
            | Self::GMonthDay { timezone_offset_minutes, .. }
            | Self::GDay { timezone_offset_minutes, .. }
            | Self::Time { timezone_offset_minutes, .. } => *timezone_offset_minutes,
        }
    }

    /// Gibt das Jahr zurück (Offset von 2000).
    #[inline]
    pub fn year(&self) -> Option<i64> {
        match self {
            Self::GYear { year, .. }
            | Self::GYearMonth { year, .. }
            | Self::Date { year, .. }
            | Self::DateTime { year, .. } => Some(*year),
            _ => None,
        }
    }

    /// Gibt den Monat zurück (1-12).
    #[inline]
    pub fn month(&self) -> Option<u8> {
        match self {
            Self::GYearMonth { month, .. }
            | Self::Date { month, .. }
            | Self::DateTime { month, .. }
            | Self::GMonth { month, .. }
            | Self::GMonthDay { month, .. } => Some(*month),
            _ => None,
        }
    }

    /// Gibt den Tag zurück (1-31).
    #[inline]
    pub fn day(&self) -> Option<u8> {
        match self {
            Self::GYearMonth { day, .. }
            | Self::Date { day, .. }
            | Self::DateTime { day, .. }
            | Self::GMonthDay { day, .. }
            | Self::GDay { day, .. } => Some(*day),
            _ => None,
        }
    }

    /// Gibt die Stunde zurück (0-24).
    #[inline]
    pub fn hour(&self) -> Option<u8> {
        match self {
            Self::DateTime { hour, .. } | Self::Time { hour, .. } => Some(*hour),
            _ => None,
        }
    }

    /// Gibt die Minuten zurück (0-59).
    #[inline]
    pub fn minute(&self) -> Option<u8> {
        match self {
            Self::DateTime { minute, .. } | Self::Time { minute, .. } => Some(*minute),
            _ => None,
        }
    }

    /// Gibt die Sekunden zurück (0-60, 60 für Schaltsekunde).
    #[inline]
    pub fn second(&self) -> Option<u8> {
        match self {
            Self::DateTime { second, .. } | Self::Time { second, .. } => Some(*second),
            _ => None,
        }
    }

    /// Gibt die Fractional Seconds zurück (reversed digit encoding).
    #[inline]
    pub fn fractional_secs(&self) -> Option<u64> {
        match self {
            Self::DateTime { fractional_secs, .. } | Self::Time { fractional_secs, .. } => {
                *fractional_secs
            }
            _ => None,
        }
    }
}

/// Encodes a date-time value (Spec 7.1.8).
///
/// Der `DateTimeType` wird aus der Enum-Variante abgeleitet.
/// Werte außerhalb der Spec-Bereiche erzeugen einen Fehler.
pub fn encode(
    writer: &mut BitWriter,
    value: &DateTime,
    byte_aligned: bool,
) -> Result<()> {
    let dt_type = value.datetime_type();

    // Year
    if let Some(year) = value.year() {
        if byte_aligned {
            encode_year_byte_aligned(writer, year);
        } else {
            integer::encode(writer, year);
        }
    }

    // MonthDay (GDay: month=0, GMonth: day=0 per Getter-Default)
    if dt_type.has_month_day() {
        encode_month_day(writer, value.month().unwrap_or(0), value.day().unwrap_or(0), byte_aligned)?;
    }

    // Time
    if let (Some(h), Some(m), Some(s)) = (value.hour(), value.minute(), value.second()) {
        encode_time(writer, h, m, s, byte_aligned)?;
    }

    // FractionalSecs
    if dt_type.has_fractional_secs() {
        let frac = value.fractional_secs();
        encode_bool(writer, frac.is_some(), byte_aligned);
        if let Some(f) = frac {
            unsigned_integer::encode(writer, f);
        }
    }

    // Timezone
    let tz = value.timezone_offset_minutes();
    encode_bool(writer, tz.is_some(), byte_aligned);
    if let Some(offset) = tz {
        encode_timezone(writer, offset, byte_aligned)?;
    }

    Ok(())
}

/// Decodes a date-time value (Spec 7.1.8).
pub fn decode(reader: &mut BitReader, dt_type: DateTimeType, byte_aligned: bool) -> Result<DateTime> {
    // Year (für GYear, GYearMonth, Date, DateTime)
    let year = if dt_type.has_year() {
        if byte_aligned {
            decode_year_byte_aligned(reader)?
        } else {
            integer::decode(reader)?
        }
    } else {
        0 // Platzhalter, wird nicht verwendet
    };

    // MonthDay (für alle außer GYear und Time)
    let (month, day) = if dt_type.has_month_day() {
        let (m, d) = decode_month_day(reader, byte_aligned)?;
        match dt_type {
            DateTimeType::GDay => {
                // gDay: month=0 ist Sentinel, day ist der echte Wert
                (0u8, d)
            }
            DateTimeType::GMonth => {
                // gMonth: day=0 ist Sentinel, month ist der echte Wert
                (m, 0u8)
            }
            _ if m == 0 || d == 0 => return Err(Error::IntegerOverflow),
            _ => (m, d),
        }
    } else {
        (0, 0) // Platzhalter
    };

    // Time (für DateTime und Time)
    let (hour, minute, second) = if dt_type.has_time() {
        decode_time(reader, byte_aligned)?
    } else {
        (0, 0, 0) // Platzhalter
    };

    // FractionalSecs (für DateTime und Time)
    let fractional_secs = if dt_type.has_fractional_secs() {
        if decode_bool(reader, byte_aligned)? {
            Some(unsigned_integer::decode(reader)?)
        } else {
            None
        }
    } else {
        None
    };

    // Timezone (für alle Typen möglich)
    let has_timezone = decode_bool(reader, byte_aligned)?;
    let tz = if has_timezone {
        Some(decode_timezone(reader, byte_aligned)?)
    } else {
        None
    };

    // Enum-Variante konstruieren
    Ok(match dt_type {
        DateTimeType::GYear => DateTime::GYear {
            year,
            timezone_offset_minutes: tz,
        },
        DateTimeType::GYearMonth => DateTime::GYearMonth {
            year,
            month,
            day,
            timezone_offset_minutes: tz,
        },
        DateTimeType::Date => DateTime::Date {
            year,
            month,
            day,
            timezone_offset_minutes: tz,
        },
        DateTimeType::DateTime => DateTime::DateTime {
            year,
            month,
            day,
            hour,
            minute,
            second,
            fractional_secs,
            timezone_offset_minutes: tz,
        },
        DateTimeType::GMonth => DateTime::GMonth {
            month,
            timezone_offset_minutes: tz,
        },
        DateTimeType::GMonthDay => DateTime::GMonthDay {
            month,
            day,
            timezone_offset_minutes: tz,
        },
        DateTimeType::GDay => DateTime::GDay {
            day,
            timezone_offset_minutes: tz,
        },
        DateTimeType::Time => DateTime::Time {
            hour,
            minute,
            second,
            fractional_secs,
            timezone_offset_minutes: tz,
        },
    })
}

fn encode_month_day(writer: &mut BitWriter, month: u8, day: u8, byte_aligned: bool) -> Result<()> {
    if month > 12 {
        return Err(Error::InvalidValue(format!("month {month} out of range 0-12")));
    }
    if day > 31 {
        return Err(Error::InvalidValue(format!("day {day} out of range 0-31")));
    }
    let value = month as u64 * 32 + day as u64;
    encode_n_bit(writer, value, 9, byte_aligned);
    Ok(())
}

fn decode_month_day(reader: &mut BitReader, byte_aligned: bool) -> Result<(u8, u8)> {
    let value = decode_n_bit(reader, 9, byte_aligned)?;
    let month = (value / 32) as u8;
    let day = (value % 32) as u8;
    if month > 12 || day > 31 {
        return Err(Error::IntegerOverflow);
    }
    Ok((month, day))
}

fn encode_time(writer: &mut BitWriter, hour: u8, minute: u8, second: u8, byte_aligned: bool) -> Result<()> {
    if hour > 24 {
        return Err(Error::InvalidValue(format!("hour {hour} out of range 0-24")));
    }
    if minute > 59 {
        return Err(Error::InvalidValue(format!("minute {minute} out of range 0-59")));
    }
    if second > 60 {
        return Err(Error::InvalidValue(format!("second {second} out of range 0-60")));
    }
    if hour == 24 && (minute != 0 || second != 0) {
        return Err(Error::InvalidValue("hour=24 requires minute=0 and second=0".into()));
    }
    let value = ((hour as u64) * 64 + minute as u64) * 64 + second as u64;
    encode_n_bit(writer, value, 17, byte_aligned);
    Ok(())
}

fn decode_time(reader: &mut BitReader, byte_aligned: bool) -> Result<(u8, u8, u8)> {
    let value = decode_n_bit(reader, 17, byte_aligned)?;
    let second = (value % 64) as u8;
    let remainder = value / 64;
    let minute = (remainder % 64) as u8;
    let hour = (remainder / 64) as u8;
    if hour > 24 || minute > 59 || second > 60 {
        return Err(Error::IntegerOverflow);
    }
    if hour == 24 && (minute != 0 || second != 0) {
        return Err(Error::IntegerOverflow);
    }
    Ok((hour, minute, second))
}

fn encode_timezone(writer: &mut BitWriter, offset_minutes: i16, byte_aligned: bool) -> Result<()> {
    let tz_hours = offset_minutes / 60;
    let tz_minutes = offset_minutes % 60;
    if !(-14..=14).contains(&tz_hours) {
        return Err(Error::InvalidValue(format!("timezone hours {tz_hours} out of range -14..14")));
    }
    // tz_minutes is offset_minutes % 60, so always in -59..59 — no check needed.
    // 896 = 14 * 64, per Spec 7.1.8 Table 7-3
    let raw = (tz_hours as i32 * 64 + tz_minutes as i32 + 896) as u64;
    encode_n_bit(writer, raw, 11, byte_aligned);
    Ok(())
}

fn decode_timezone(reader: &mut BitReader, byte_aligned: bool) -> Result<i16> {
    let raw = decode_n_bit(reader, 11, byte_aligned)? as i32;
    // 896 = 14 * 64, per Spec 7.1.8 Table 7-3
    let adjusted = raw - 896;
    let tz_hours = adjusted / 64;
    let tz_minutes = adjusted % 64;
    if !(-14..=14).contains(&tz_hours) || !(-59..=59).contains(&tz_minutes) {
        return Err(Error::IntegerOverflow);
    }
    Ok((tz_hours * 60 + tz_minutes) as i16)
}

/// Encodiert einen Boolean-Wert (bit-packed oder byte-aligned).
fn encode_bool(writer: &mut BitWriter, value: bool, byte_aligned: bool) {
    if byte_aligned {
        boolean::encode_byte_aligned(writer, value);
    } else {
        boolean::encode(writer, value);
    }
}

/// Decodiert einen Boolean-Wert (bit-packed oder byte-aligned).
fn decode_bool(reader: &mut BitReader, byte_aligned: bool) -> Result<bool> {
    if byte_aligned {
        boolean::decode_byte_aligned(reader)
    } else {
        boolean::decode(reader)
    }
}

fn encode_n_bit(writer: &mut BitWriter, value: u64, n: u8, byte_aligned: bool) {
    if byte_aligned {
        let num_bytes = n.div_ceil(8);
        for i in 0..num_bytes {
            let byte = ((value >> (i * 8)) & 0xFF) as u8;
            writer.write_byte_aligned(byte);
        }
    } else {
        n_bit_unsigned_integer::encode(writer, value, n);
    }
}

fn decode_n_bit(reader: &mut BitReader, n: u8, byte_aligned: bool) -> Result<u64> {
    if byte_aligned {
        let num_bytes = n.div_ceil(8);
        let mut value = 0u64;
        for i in 0..num_bytes {
            let byte = reader.read_byte_aligned()?;
            value |= (byte as u64) << (i * 8);
        }
        Ok(value)
    } else {
        n_bit_unsigned_integer::decode(reader, n)
    }
}

fn encode_year_byte_aligned(writer: &mut BitWriter, year: i64) {
    if year >= 0 {
        writer.write_byte_aligned(0);
        unsigned_integer::encode(writer, year as u64);
    } else {
        writer.write_byte_aligned(1);
        unsigned_integer::encode(writer, !(year as u64));
    }
}

fn decode_year_byte_aligned(reader: &mut BitReader) -> Result<i64> {
    let sign = reader.read_byte_aligned()? != 0;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trip(dt_type: DateTimeType, value: DateTime) -> DateTime {
        let mut w = BitWriter::new();
        encode(&mut w, &value, false).unwrap();
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r, dt_type, false).unwrap()
    }

    // --- Component tests ---

    /// Spec 7.1.8, Table 7-3: Year offset from 2000
    #[test]
    fn year_encode_decode() {
        for &(year, _label) in &[
            (0i64, "2000"),
            (-1, "1999"),
            (25, "2025"),
            (-2000, "year 0"),
        ] {
            let mut w = BitWriter::new();
            integer::encode(&mut w, year);
            let data = w.into_vec();
            let mut r = BitReader::new(&data);
            assert_eq!(integer::decode(&mut r).unwrap(), year);
        }
    }

    /// Spec 7.1.8, Table 7-3: MonthDay = month*32 + day, 9-bit
    #[test]
    fn month_day_encode_decode() {
        for &(month, day) in &[(1u8, 1u8), (12, 31), (6, 15), (0, 5), (3, 0)] {
            let mut w = BitWriter::new();
            encode_month_day(&mut w, month, day, false).unwrap();
            let data = w.into_vec();
            let mut r = BitReader::new(&data);
            let (m, d) = decode_month_day(&mut r, false).unwrap();
            assert_eq!((m, d), (month, day), "failed for month={month}, day={day}");
        }
    }

    /// Spec 7.1.8, Table 7-3: Time = ((hour*64)+min)*64+sec, 17-bit
    #[test]
    fn time_encode_decode() {
        for &(h, m, s) in &[(0u8, 0u8, 0u8), (24, 0, 0), (12, 30, 45), (0, 0, 60)] {
            let mut w = BitWriter::new();
            encode_time(&mut w, h, m, s, false).unwrap();
            let data = w.into_vec();
            let mut r = BitReader::new(&data);
            let (rh, rm, rs) = decode_time(&mut r, false).unwrap();
            assert_eq!((rh, rm, rs), (h, m, s), "failed for {h}:{m}:{s}");
        }
    }

    /// Spec 7.1.8, Table 7-3: FractionalSecs as unsigned integer
    #[test]
    fn fractional_secs_round_trip() {
        for &frac in &[0u64, 123, 999999] {
            let mut w = BitWriter::new();
            unsigned_integer::encode(&mut w, frac);
            let data = w.into_vec();
            let mut r = BitReader::new(&data);
            assert_eq!(unsigned_integer::decode(&mut r).unwrap(), frac);
        }
    }

    /// Spec 7.1.8, Table 7-3: TimeZone = (hours*64+minutes+896), 11-bit
    #[test]
    fn timezone_encode_decode() {
        for &offset in &[0i16, 840, -840, 330, -330, 60, -60] {
            let mut w = BitWriter::new();
            encode_timezone(&mut w, offset, false).unwrap();
            let data = w.into_vec();
            let mut r = BitReader::new(&data);
            assert_eq!(
                decode_timezone(&mut r, false).unwrap(),
                offset,
                "failed for offset={offset}"
            );
        }
    }

    // --- Type round-trip tests ---

    /// Spec 7.1.8, Table 7-4: gYear round-trip
    #[test]
    fn gyear_round_trip() {
        let dt = DateTime::GYear { year: 25, timezone_offset_minutes: None };
        assert_eq!(round_trip(DateTimeType::GYear, dt), dt);

        let dt_tz = DateTime::GYear { year: 25, timezone_offset_minutes: Some(330) };
        assert_eq!(round_trip(DateTimeType::GYear, dt_tz), dt_tz);
    }

    /// Spec 7.1.8, Table 7-4: gYearMonth round-trip.
    /// gYearMonth uses the MonthDay encoding component, which requires both
    /// month and day to be present in the wire format even though XML Schema
    /// gYearMonth has no day semantics.
    #[test]
    fn gyear_month_round_trip() {
        let dt = DateTime::GYearMonth { year: -1, month: 12, day: 1, timezone_offset_minutes: Some(-300) };
        assert_eq!(round_trip(DateTimeType::GYearMonth, dt), dt);
    }

    /// Spec 7.1.8, Table 7-4: date round-trip
    #[test]
    fn date_round_trip() {
        let dt = DateTime::Date { year: 0, month: 1, day: 31, timezone_offset_minutes: None };
        assert_eq!(round_trip(DateTimeType::Date, dt), dt);
    }

    /// Spec 7.1.8, Table 7-4: dateTime round-trip
    #[test]
    fn datetime_round_trip() {
        // Without fractional secs, without timezone
        let dt = DateTime::DateTime {
            year: 25, month: 6, day: 15,
            hour: 14, minute: 30, second: 0,
            fractional_secs: None, timezone_offset_minutes: None,
        };
        assert_eq!(round_trip(DateTimeType::DateTime, dt), dt);

        // With fractional secs and timezone
        let dt2 = DateTime::DateTime {
            year: 25, month: 12, day: 31,
            hour: 23, minute: 59, second: 59,
            fractional_secs: Some(123), timezone_offset_minutes: Some(0),
        };
        assert_eq!(round_trip(DateTimeType::DateTime, dt2), dt2);
    }

    /// Spec 7.1.8, Table 7-4: gMonth round-trip (day=0 in MonthDay)
    #[test]
    fn gmonth_round_trip() {
        let dt = DateTime::GMonth { month: 7, timezone_offset_minutes: None };
        assert_eq!(round_trip(DateTimeType::GMonth, dt), dt);
    }

    /// Spec 7.1.8, Table 7-4: gMonthDay round-trip
    #[test]
    fn gmonth_day_round_trip() {
        let dt = DateTime::GMonthDay { month: 2, day: 29, timezone_offset_minutes: Some(60) };
        assert_eq!(round_trip(DateTimeType::GMonthDay, dt), dt);
    }

    /// Spec 7.1.8, Table 7-4: gDay round-trip (month=0 in MonthDay)
    #[test]
    fn gday_round_trip() {
        let dt = DateTime::GDay { day: 15, timezone_offset_minutes: Some(-840) };
        assert_eq!(round_trip(DateTimeType::GDay, dt), dt);
    }

    /// Spec 7.1.8, Table 7-4: time round-trip
    #[test]
    fn time_round_trip() {
        // Without fractional secs
        let dt = DateTime::Time {
            hour: 8, minute: 0, second: 0,
            fractional_secs: None, timezone_offset_minutes: None,
        };
        assert_eq!(round_trip(DateTimeType::Time, dt), dt);

        // With fractional secs and timezone
        let dt2 = DateTime::Time {
            hour: 12, minute: 30, second: 45,
            fractional_secs: Some(5), timezone_offset_minutes: Some(330),
        };
        assert_eq!(round_trip(DateTimeType::Time, dt2), dt2);
    }

    // --- Edge case tests ---

    /// Spec 7.1.8: leap second (second=60)
    #[test]
    fn leap_second() {
        let dt = DateTime::DateTime {
            year: 25, month: 6, day: 30,
            hour: 23, minute: 59, second: 60,
            fractional_secs: None, timezone_offset_minutes: Some(0),
        };
        assert_eq!(round_trip(DateTimeType::DateTime, dt), dt);
    }

    /// Spec 7.1.8: hour=24 (end of day per XML Schema)
    #[test]
    fn hour_24() {
        let dt = DateTime::DateTime {
            year: 25, month: 1, day: 1,
            hour: 24, minute: 0, second: 0,
            fractional_secs: None, timezone_offset_minutes: None,
        };
        assert_eq!(round_trip(DateTimeType::DateTime, dt), dt);
    }

    /// Spec 7.1.8: negative year offset
    #[test]
    fn negative_year() {
        let dt = DateTime::Date { year: -2000, month: 1, day: 1, timezone_offset_minutes: None };
        assert_eq!(round_trip(DateTimeType::Date, dt), dt);
    }

    /// Spec 7.1.8: all 8 types sequential in one stream
    #[test]
    fn all_types_sequential() {
        let types_and_values = [
            (
                DateTimeType::GYear,
                DateTime::GYear { year: 25, timezone_offset_minutes: None },
            ),
            (
                DateTimeType::GYearMonth,
                DateTime::GYearMonth { year: 0, month: 6, day: 1, timezone_offset_minutes: None },
            ),
            (
                DateTimeType::Date,
                DateTime::Date { year: -1, month: 12, day: 31, timezone_offset_minutes: Some(0) },
            ),
            (
                DateTimeType::DateTime,
                DateTime::DateTime {
                    year: 25, month: 3, day: 14,
                    hour: 15, minute: 9, second: 26,
                    fractional_secs: Some(535), timezone_offset_minutes: Some(60),
                },
            ),
            (
                DateTimeType::GMonth,
                DateTime::GMonth { month: 11, timezone_offset_minutes: None },
            ),
            (
                DateTimeType::GMonthDay,
                DateTime::GMonthDay { month: 2, day: 29, timezone_offset_minutes: None },
            ),
            (
                DateTimeType::GDay,
                DateTime::GDay { day: 1, timezone_offset_minutes: None },
            ),
            (
                DateTimeType::Time,
                DateTime::Time {
                    hour: 0, minute: 0, second: 0,
                    fractional_secs: None, timezone_offset_minutes: Some(-330),
                },
            ),
        ];

        let mut w = BitWriter::new();
        for (_dt_type, value) in &types_and_values {
            encode(&mut w, value, false).unwrap();
        }
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        for (dt_type, expected) in &types_and_values {
            let decoded = decode(&mut r, *dt_type, false).unwrap();
            assert_eq!(&decoded, expected, "failed for {dt_type:?}");
        }
    }

    /// Spec 7.1.8: EOF on various types
    #[test]
    fn eof_gyear() {
        let mut r = BitReader::new(&[]);
        assert_eq!(
            decode(&mut r, DateTimeType::GYear, false).unwrap_err(),
            Error::PrematureEndOfStream
        );
    }

    #[test]
    fn eof_datetime() {
        let mut r = BitReader::new(&[]);
        assert_eq!(
            decode(&mut r, DateTimeType::DateTime, false).unwrap_err(),
            Error::PrematureEndOfStream
        );
    }

    #[test]
    fn eof_time() {
        let mut r = BitReader::new(&[]);
        assert_eq!(
            decode(&mut r, DateTimeType::Time, false).unwrap_err(),
            Error::PrematureEndOfStream
        );
    }

    /// Spec 7.1.8: gDay encodes month=0 in MonthDay
    #[test]
    fn gday_month_zero() {
        let dt = DateTime::GDay { day: 25, timezone_offset_minutes: None };
        // Verify the raw encoding: month=0, so MonthDay = 0*32 + 25 = 25
        let mut w = BitWriter::new();
        encode(&mut w, &dt, false).unwrap();
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        let (m, d) = decode_month_day(&mut r, false).unwrap();
        assert_eq!(m, 0);
        assert_eq!(d, 25);
    }

    /// Spec 7.1.8: gMonth encodes day=0 in MonthDay
    #[test]
    fn gmonth_day_zero() {
        let dt = DateTime::GMonth { month: 10, timezone_offset_minutes: None };
        // Verify the raw encoding: day=0, so MonthDay = 10*32 + 0 = 320
        let mut w = BitWriter::new();
        encode(&mut w, &dt, false).unwrap();
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        let (m, d) = decode_month_day(&mut r, false).unwrap();
        assert_eq!(m, 10);
        assert_eq!(d, 0);
    }

    // --- Error tests ---

    /// Spec 7.1.8: error when hour=24 with minute!=0
    #[test]
    fn err_hour24_invalid() {
        let mut w = BitWriter::new();
        let result = encode_time(&mut w, 24, 1, 0, false);
        assert!(result.is_err());
    }

    /// Spec 7.1.8: error on month out of range
    #[test]
    fn err_month_out_of_range() {
        let mut w = BitWriter::new();
        let result = encode_month_day(&mut w, 13, 1, false);
        assert!(result.is_err());
    }

    /// Spec 7.1.8: error on day out of range
    #[test]
    fn err_day_out_of_range() {
        let mut w = BitWriter::new();
        let result = encode_month_day(&mut w, 1, 32, false);
        assert!(result.is_err());
    }

    /// Spec 7.1.8: error on hour out of range
    #[test]
    fn err_hour_out_of_range() {
        let mut w = BitWriter::new();
        let result = encode_time(&mut w, 25, 0, 0, false);
        assert!(result.is_err());
    }

    /// Spec 7.1.8: error on minute out of range
    #[test]
    fn err_minute_out_of_range() {
        let mut w = BitWriter::new();
        let result = encode_time(&mut w, 0, 60, 0, false);
        assert!(result.is_err());
    }

    /// Spec 7.1.8: error on second out of range
    #[test]
    fn err_second_out_of_range() {
        let mut w = BitWriter::new();
        let result = encode_time(&mut w, 0, 0, 61, false);
        assert!(result.is_err());
    }

    /// Spec 7.1.8: error on timezone hours out of range
    #[test]
    fn err_timezone_hours_out_of_range() {
        let mut w = BitWriter::new();
        let result = encode_timezone(&mut w, 15 * 60, false); // 15 hours = 900 minutes
        assert!(result.is_err());
    }

    /// Spec 7.1.8: error when hour=24 with second!=0
    #[test]
    fn err_hour24_second_nonzero() {
        let mut w = BitWriter::new();
        let result = encode_time(&mut w, 24, 0, 1, false);
        assert!(result.is_err());
    }

    // --- Decode validation tests ---

    /// Spec 7.1.8: decode MonthDay with month > 12 returns error
    #[test]
    fn decode_month_day_invalid_month() {
        // month=13, day=0 → value = 13*32 + 0 = 416
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 416, 9);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode_month_day(&mut r, false).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    /// Spec 7.1.8: decode Time with hour > 24 returns error
    #[test]
    fn decode_time_invalid_hour() {
        // hour=25, minute=0, second=0 → ((25*64)+0)*64+0 = 102400
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 102400, 17);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode_time(&mut r, false).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    /// Spec 7.1.8: decode Time with minute > 59 returns error
    #[test]
    fn decode_time_invalid_minute() {
        // hour=0, minute=60, second=0 → ((0*64)+60)*64+0 = 3840
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 3840, 17);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode_time(&mut r, false).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    /// Spec 7.1.8: decode Time with second > 60 returns error
    #[test]
    fn decode_time_invalid_second() {
        // hour=0, minute=0, second=61 → 61
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 61, 17);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode_time(&mut r, false).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    /// Spec 7.1.8: decode Time with hour=24, minute=0, second=1 returns error
    #[test]
    fn decode_time_hour24_second_nonzero() {
        // hour=24, minute=0, second=1 → ((24*64)+0)*64+1 = 98305
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 98305, 17);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode_time(&mut r, false).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    /// Spec 7.1.8: decode TimeZone with hours > 14 returns error
    #[test]
    fn decode_timezone_out_of_range() {
        // tz_hours=15, tz_minutes=0 → raw = 15*64 + 0 + 896 = 1856
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 1856, 11);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode_timezone(&mut r, false).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    /// Spec 7.1.8: decode TimeZone with minutes > 59 returns error
    #[test]
    fn decode_timezone_invalid_minutes() {
        // tz_hours=0, tz_minutes=60 → raw = 0*64 + 60 + 896 = 956
        let mut w = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut w, 956, 11);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode_timezone(&mut r, false).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    /// Spec 7.1.8: decode MonthDay with month=0 for Date returns error
    #[test]
    fn decode_date_month_zero() {
        // month=0, day=1 → MonthDay = 0*32 + 1 = 1 (invalid for Date)
        let mut w = BitWriter::new();
        // Year = 0 (offset from 2000)
        integer::encode(&mut w, 0);
        // MonthDay = 1 (month=0, day=1)
        n_bit_unsigned_integer::encode(&mut w, 1, 9);
        // presence(timezone) = false
        boolean::encode(&mut w, false);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode(&mut r, DateTimeType::Date, false).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    /// Spec 7.1.8: decode MonthDay with day=0 for DateTime returns error
    #[test]
    fn decode_datetime_day_zero() {
        let mut w = BitWriter::new();
        // Year = 0
        integer::encode(&mut w, 0);
        // MonthDay = 1*32 + 0 = 32 (month=1, day=0 — invalid for DateTime)
        n_bit_unsigned_integer::encode(&mut w, 32, 9);
        // Time is required but we'll error before reaching it
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        assert_eq!(
            decode(&mut r, DateTimeType::DateTime, false).unwrap_err(),
            Error::IntegerOverflow
        );
    }

    // --- Getter None-Tests ---

    /// Getter geben None zurück wenn das Feld in der Variante nicht existiert.
    #[test]
    fn getter_none_for_missing_fields() {
        let gyear = DateTime::GYear { year: 25, timezone_offset_minutes: None };
        assert_eq!(gyear.month(), None);
        assert_eq!(gyear.day(), None);
        assert_eq!(gyear.hour(), None);
        assert_eq!(gyear.minute(), None);
        assert_eq!(gyear.second(), None);
        assert_eq!(gyear.fractional_secs(), None);

        let time = DateTime::Time {
            hour: 12, minute: 0, second: 0,
            fractional_secs: None, timezone_offset_minutes: None,
        };
        assert_eq!(time.year(), None);
        assert_eq!(time.month(), None);
        assert_eq!(time.day(), None);

        let gmonth = DateTime::GMonth { month: 6, timezone_offset_minutes: None };
        assert_eq!(gmonth.year(), None);
        assert_eq!(gmonth.day(), None);
        assert_eq!(gmonth.hour(), None);

        let gday = DateTime::GDay { day: 15, timezone_offset_minutes: None };
        assert_eq!(gday.year(), None);
        assert_eq!(gday.month(), None);
        assert_eq!(gday.hour(), None);
    }
}
