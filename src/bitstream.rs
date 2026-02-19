//! Bit-level stream reader and writer for EXI encoding.
//!
//! EXI uses MSB-first bit packing (Spec 7.1). Bits within each byte are
//! numbered 7 (most significant, written/read first) down to 0.

use std::io::Write;

use crate::{Error, Result};

/// Writes individual bits into a growable byte buffer, MSB first (Spec 7.1).
///
/// Intern wird ein u64-Akkumulator verwendet: Bits werden zuerst in `accum`
/// gesammelt und erst bei >= 8 akkumulierten Bits als volle Bytes in `buf`
/// geflusht. Das reduziert Vec-Zugriffe bei kleinen Writes (1-3 Bits) drastisch.
pub struct BitWriter {
    buf: Vec<u8>,
    /// Akkumulator: enthält die nächsten `accum_bits` Bits (MSB = ältestes Bit).
    accum: u64,
    /// Anzahl gültiger Bits im Akkumulator (0..7 nach Flush).
    accum_bits: u8,
    /// Anzahl aktiver Checkpoints (für drain_to()-Safety).
    active_checkpoints: usize,
}

impl BitWriter {
    /// Creates a new empty `BitWriter`.
    pub fn new() -> Self {
        Self {
            buf: Vec::new(),
            accum: 0,
            accum_bits: 0,
            active_checkpoints: 0,
        }
    }

    /// Flusht volle Bytes aus dem Akkumulator in den Buffer.
    #[inline(always)]
    fn flush_to_buf(&mut self) {
        while self.accum_bits >= 8 {
            self.accum_bits -= 8;
            self.buf.push((self.accum >> self.accum_bits) as u8);
        }
        if self.accum_bits > 0 {
            self.accum &= (1u64 << self.accum_bits) - 1;
        } else {
            self.accum = 0;
        }
    }

    /// Writes a single bit. `true` = 1, `false` = 0.
    #[inline(always)]
    pub fn write_bit(&mut self, val: bool) {
        self.accum = (self.accum << 1) | u64::from(val);
        self.accum_bits += 1;
        if self.accum_bits >= 8 {
            self.flush_to_buf();
        }
    }

    /// Schreibt die unteren 2 Bits von `val` (MSB first). Optimiert für Event-Code-Teile.
    #[inline(always)]
    pub fn write_bits_2(&mut self, val: u8) {
        debug_assert!(val < 4, "write_bits_2: val={val} >= 4");
        self.accum = (self.accum << 2) | u64::from(val);
        self.accum_bits += 2;
        if self.accum_bits >= 8 {
            self.flush_to_buf();
        }
    }

    /// Schreibt die unteren 3 Bits von `val` (MSB first). Optimiert für Event-Code-Teile.
    #[inline(always)]
    pub fn write_bits_3(&mut self, val: u8) {
        debug_assert!(val < 8, "write_bits_3: val={val} >= 8");
        self.accum = (self.accum << 3) | u64::from(val);
        self.accum_bits += 3;
        if self.accum_bits >= 8 {
            self.flush_to_buf();
        }
    }

    /// Writes the lower `n` bits of `val`, MSB first.
    /// When `n` is 0 this is a no-op (Spec 6.2 zero-length event code parts).
    ///
    /// # Panics
    ///
    /// Panics if `n > 64`.
    #[inline]
    pub fn write_bits(&mut self, val: u64, n: u8) {
        debug_assert!(n <= 64, "bit count must be 0..=64, got {n}");
        if n == 0 {
            return;
        }
        let total = self.accum_bits as u16 + n as u16;
        if total <= 64 {
            // Fast path: alles passt in den Akkumulator
            if n < 64 {
                self.accum = (self.accum << n) | (val & ((1u64 << n) - 1));
            } else {
                // n == 64, accum_bits muss 0 sein
                self.accum = val;
            }
            self.accum_bits = total as u8;
        } else {
            // Slow path: n > 57, muss splitten (extrem selten)
            let first = 64 - self.accum_bits; // 57..63
            let rest = n - first; // 1..7
            self.accum = (self.accum << first) | ((val >> rest) & ((1u64 << first) - 1));
            self.accum_bits = 64;
            self.flush_to_buf();
            self.accum = val & ((1u64 << rest) - 1);
            self.accum_bits = rest;
        }
        if self.accum_bits >= 8 {
            self.flush_to_buf();
        }
    }

    /// Pads with zero bits until the current position is byte-aligned (Spec 5.2 header padding).
    /// No-op if already aligned.
    pub fn align_to_byte(&mut self) {
        if self.accum_bits > 0 {
            self.buf
                .push((self.accum << (8 - self.accum_bits)) as u8);
            self.accum = 0;
            self.accum_bits = 0;
        }
    }

    /// Writes a single byte when already byte-aligned. Falls back to `write_bits(8)`
    /// when not aligned. Avoids per-bit overhead for byte-oriented codecs (Spec 7.1.6).
    #[inline(always)]
    pub fn write_byte_aligned(&mut self, val: u8) {
        if self.accum_bits == 0 {
            self.buf.push(val);
        } else {
            self.write_bits(u64::from(val), 8);
        }
    }

    /// Schreibt ein Byte-Slice. Bei byte-aligned: direkt `extend_from_slice` (O(1) amortized).
    /// Bei nicht-aligned: Loop über Bytes mit `write_bits(8)`.
    pub fn write_bytes_aligned(&mut self, bytes: &[u8]) {
        if bytes.is_empty() {
            return;
        }
        if self.accum_bits == 0 {
            self.buf.extend_from_slice(bytes);
        } else {
            for &b in bytes {
                self.write_bits(u64::from(b), 8);
            }
        }
    }

    /// Returns the current bit position (number of bits written so far).
    pub fn bit_position(&self) -> usize {
        self.buf.len() * 8 + self.accum_bits as usize
    }

    /// Returns a reference to the written bytes so far.
    ///
    /// Flusht den Akkumulator: falls nicht byte-aligned, enthält das letzte Byte
    /// partielle Daten mit Null-Padding.
    pub fn bytes(&mut self) -> &[u8] {
        self.align_to_byte();
        &self.buf
    }

    /// Speichert die aktuelle Schreibposition für möglichen Rollback (Spec 7.1
    /// Typed-Value-Fallback: bei ungültigem Wert wird auf String zurückgefallen).
    pub fn save_checkpoint(&mut self) -> BitWriterCheckpoint {
        self.active_checkpoints += 1;
        BitWriterCheckpoint {
            buf_len: self.buf.len(),
            accum: self.accum,
            accum_bits: self.accum_bits,
        }
    }

    /// Stellt eine zuvor gespeicherte Position wieder her und verwirft alle
    /// danach geschriebenen Daten.
    pub fn restore_checkpoint(&mut self, checkpoint: BitWriterCheckpoint) {
        debug_assert!(
            self.active_checkpoints > 0,
            "restore_checkpoint() ohne aktiven Checkpoint"
        );
        self.active_checkpoints -= 1;
        self.buf.truncate(checkpoint.buf_len);
        self.accum = checkpoint.accum;
        self.accum_bits = checkpoint.accum_bits;
    }

    /// Löst einen Checkpoint auf ohne Rollback (Erfolgsfall: typed-value-Encoding war gültig).
    pub fn discard_checkpoint(&mut self) {
        debug_assert!(
            self.active_checkpoints > 0,
            "discard_checkpoint() ohne aktiven Checkpoint"
        );
        self.active_checkpoints -= 1;
    }

    /// Schreibt alle vollständigen Bytes in den Writer und entfernt sie aus dem Buffer.
    /// Der Akkumulator (partielle Bits) bleibt erhalten.
    ///
    /// # Panics
    ///
    /// Panikt wenn ein aktiver Checkpoint existiert — drain_to() darf nur
    /// zwischen encode_event()-Aufrufen verwendet werden, nie innerhalb.
    pub fn drain_to(&mut self, writer: &mut impl Write) -> std::io::Result<()> {
        assert_eq!(
            self.active_checkpoints, 0,
            "drain_to() bei aktivem Checkpoint nicht erlaubt"
        );
        if self.buf.is_empty() {
            return Ok(());
        }
        writer.write_all(&self.buf)?;
        self.buf.clear();
        Ok(())
    }

    /// Gibt die aktuelle Buffer-Größe in Bytes zurück.
    pub fn buf_len(&self) -> usize {
        self.buf.len()
    }

    /// Finalises the writer, padding the last byte with zero bits, and returns the buffer.
    pub fn into_vec(mut self) -> Vec<u8> {
        self.align_to_byte();
        self.buf
    }
}

/// Checkpoint für BitWriter-Rollback (Spec 7.1 Typed-Value-Fallback).
pub struct BitWriterCheckpoint {
    buf_len: usize,
    accum: u64,
    accum_bits: u8,
}

/// Checkpoint für BitReader-Rollback (Cookie-Erkennung, Typed-Value-Fallback).
#[derive(Clone, Copy)]
pub struct BitReaderCheckpoint {
    byte_pos: usize,
    accum: u64,
    accum_bits: u8,
}

impl Default for BitWriter {
    fn default() -> Self {
        Self::new()
    }
}

/// Reads individual bits from a byte slice, MSB first (Spec 7.1).
///
/// Verwendet einen u64-Akkumulator für schnellen Bit-Zugriff.
/// Bits werden batch-weise aus `data` in den Akkumulator geladen
/// und per Shift/Mask extrahiert. Dies reduziert Byte-Array-Zugriffe
/// und Boundary-Checks drastisch (nur beim Refill statt bei jedem Bit).
#[derive(Clone, Copy)]
pub struct BitReader<'a> {
    data: &'a [u8],
    /// Nächstes ungelesenes Byte in data.
    byte_pos: usize,
    /// Akkumulator: enthält `accum_bits` gültige Bits, linksbündig (Bit 63 = ältestes).
    /// Die rechten (64 - accum_bits) Bits sind immer 0.
    accum: u64,
    /// Anzahl gültiger Bits im Akkumulator (0..=64).
    accum_bits: u8,
}

impl<'a> BitReader<'a> {
    /// Creates a new `BitReader` over the given byte slice.
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, byte_pos: 0, accum: 0, accum_bits: 0 }
    }

    /// Füllt den Akkumulator byteweise nach, solange accum_bits <= 56 (Ergebnis: 57..=64 Bits,
    /// falls genug Daten).
    #[inline(always)]
    fn refill(&mut self) {
        while self.accum_bits <= 56 && self.byte_pos < self.data.len() {
            self.accum |= (self.data[self.byte_pos] as u64) << (56 - self.accum_bits);
            self.byte_pos += 1;
            self.accum_bits += 8;
        }
    }

    /// Reads a single bit. Returns `true` for 1, `false` for 0.
    #[inline(always)]
    pub fn read_bit(&mut self) -> Result<bool> {
        self.refill();
        if self.accum_bits == 0 {
            return Err(Error::PrematureEndOfStream);
        }
        let val = (self.accum >> 63) != 0;
        self.accum <<= 1;
        self.accum_bits -= 1;
        Ok(val)
    }

    /// Liest 2 Bits und gibt sie als u8 zurück. Optimiert für Event-Code-Teile.
    #[inline(always)]
    pub fn read_bits_2(&mut self) -> Result<u8> {
        self.refill();
        if self.accum_bits < 2 {
            return Err(Error::PrematureEndOfStream);
        }
        let val = (self.accum >> 62) as u8;
        self.accum <<= 2;
        self.accum_bits -= 2;
        Ok(val)
    }

    /// Liest 3 Bits und gibt sie als u8 zurück. Optimiert für Event-Code-Teile.
    #[inline(always)]
    pub fn read_bits_3(&mut self) -> Result<u8> {
        self.refill();
        if self.accum_bits < 3 {
            return Err(Error::PrematureEndOfStream);
        }
        let val = (self.accum >> 61) as u8;
        self.accum <<= 3;
        self.accum_bits -= 3;
        Ok(val)
    }

    /// Reads `n` bits and returns them as a `u64`, MSB first.
    /// When `n` is 0 this is a no-op returning 0.
    ///
    /// # Panics
    ///
    /// Panics if `n > 64`.
    #[inline]
    pub fn read_bits(&mut self, n: u8) -> Result<u64> {
        debug_assert!(n <= 64, "bit count must be 0..=64, got {n}");
        if n == 0 {
            return Ok(0);
        }
        // Upfront-Check: genug Bits verfügbar? (State bleibt unverändert bei Fehler)
        let available = self.accum_bits as usize + (self.data.len() - self.byte_pos) * 8;
        if (n as usize) > available {
            return Err(Error::PrematureEndOfStream);
        }

        if n <= 56 {
            self.refill();
            // n <= 56 und available >= n → nach Refill: accum_bits >= n
            let val = self.accum >> (64 - n);
            self.accum <<= n;
            self.accum_bits -= n;
            Ok(val)
        } else {
            // n > 56 (selten: nur bei read_bits(57..64) für sehr große unsigned_integer)
            self.refill();
            if self.accum_bits >= n {
                let val = self.accum >> (64 - n);
                self.accum = if n < 64 { self.accum << n } else { 0 };
                self.accum_bits -= n;
                Ok(val)
            } else {
                // Zweistufig: erst Akkumulator leeren, dann nachladen
                let first = self.accum_bits;
                debug_assert!(first > 0, "refill hätte mindestens 1 Byte laden müssen");
                let val_high = self.accum >> (64 - first);
                self.accum = 0;
                self.accum_bits = 0;

                let remaining = n - first;
                self.refill();
                let val_low = self.accum >> (64 - remaining);
                self.accum <<= remaining;
                self.accum_bits -= remaining;

                Ok((val_high << remaining) | val_low)
            }
        }
    }

    /// Discards unread bits up to the next byte boundary. No-op if already aligned.
    pub fn align_to_byte(&mut self) {
        let discard = self.accum_bits % 8;
        if discard > 0 {
            self.accum <<= discard;
            self.accum_bits -= discard;
        }
    }

    /// Liest ein Byte. Fast-Path: direkt aus Akkumulator (>= 8 Bits) oder direkt aus
    /// data (leerer Akkumulator). Fallback via `read_bits(8)` bei < 8 Rest-Bits.
    #[inline(always)]
    pub fn read_byte_aligned(&mut self) -> Result<u8> {
        if self.accum_bits >= 8 {
            let val = (self.accum >> 56) as u8;
            self.accum <<= 8;
            self.accum_bits -= 8;
            Ok(val)
        } else if self.accum_bits == 0 {
            if self.byte_pos >= self.data.len() {
                return Err(Error::PrematureEndOfStream);
            }
            let val = self.data[self.byte_pos];
            self.byte_pos += 1;
            Ok(val)
        } else {
            Ok(self.read_bits(8)? as u8)
        }
    }

    /// Liest `n` Bytes in den Buffer. Bei leerem Akkumulator: direkter Slice-Zugriff.
    /// Sonst Loop über `read_bits(8)`.
    pub fn read_bytes_aligned(&mut self, buf: &mut [u8]) -> Result<()> {
        if buf.is_empty() {
            return Ok(());
        }
        if self.accum_bits == 0 {
            let end = self.byte_pos + buf.len();
            if end > self.data.len() {
                return Err(Error::PrematureEndOfStream);
            }
            buf.copy_from_slice(&self.data[self.byte_pos..end]);
            self.byte_pos += buf.len();
        } else {
            for slot in buf.iter_mut() {
                *slot = self.read_bits(8)? as u8;
            }
        }
        Ok(())
    }

    /// Returns the current bit position.
    pub fn bit_position(&self) -> usize {
        self.byte_pos * 8 - self.accum_bits as usize
    }

    /// Gibt einen Slice der nächsten `n` Bytes zurück, falls byte-aligned
    /// (accum_bits % 8 == 0) und verfügbar. Berücksichtigt bereits in den Akkumulator
    /// geladene Bytes. Avanziert den Lesezeiger NICHT.
    pub fn peek_aligned_bytes(&self, n: usize) -> Option<&'a [u8]> {
        if n == 0 || self.accum_bits % 8 != 0 {
            return None;
        }
        let accum_bytes = (self.accum_bits / 8) as usize;
        debug_assert!(
            self.byte_pos >= accum_bytes,
            "byte_pos ({}) < accum_bytes ({}): Akkumulator-Invariante verletzt",
            self.byte_pos, accum_bytes
        );
        let logical_byte_pos = self.byte_pos - accum_bytes;
        let end = logical_byte_pos.checked_add(n)?;
        if end > self.data.len() {
            return None;
        }
        Some(&self.data[logical_byte_pos..end])
    }

    /// Überspringt `n` Bytes. Nur verwenden nach erfolgreichem `peek_aligned_bytes`.
    pub fn skip_aligned_bytes(&mut self, n: usize) {
        debug_assert!(self.accum_bits % 8 == 0);
        let logical_byte_pos = self.byte_pos - (self.accum_bits / 8) as usize;
        let new_byte_pos = logical_byte_pos + n;
        assert!(
            new_byte_pos <= self.data.len(),
            "skip_aligned_bytes({n}): byte_pos {} überschreitet data.len() {}",
            new_byte_pos, self.data.len()
        );
        self.byte_pos = new_byte_pos;
        self.accum = 0;
        self.accum_bits = 0;
    }

    /// Debug helper: dump raw bytes around the current bit position.
    #[cfg(debug_assertions)]
    pub fn debug_dump_bytes(&self, label: &str, before: usize, after: usize) {
        let bit_pos = self.bit_position();
        let byte_pos = bit_pos / 8;
        let start = byte_pos.saturating_sub(before);
        let end = (byte_pos + after).min(self.data.len());
        eprintln!(
            "dump_bytes({label}): bit_pos={} byte_pos={} range=[{}..{}) bytes={:02X?}",
            bit_pos,
            byte_pos,
            start,
            end,
            &self.data[start..end]
        );
    }

    /// Returns the number of bits remaining to be read.
    pub fn remaining_bits(&self) -> usize {
        (self.data.len() - self.byte_pos) * 8 + self.accum_bits as usize
    }

    /// Returns the total number of bits in the stream.
    pub fn total_bits(&self) -> usize {
        self.data.len() * 8
    }

    /// Speichert die aktuelle Leseposition für möglichen Rollback.
    ///
    /// Analog zu `BitWriter::save_checkpoint()` — ermöglicht effizientes Backtracking
    /// ohne `set_bit_position()` (das eine absolute Position im Slice braucht).
    pub fn save_checkpoint(&self) -> BitReaderCheckpoint {
        BitReaderCheckpoint {
            byte_pos: self.byte_pos,
            accum: self.accum,
            accum_bits: self.accum_bits,
        }
    }

    /// Stellt eine zuvor gespeicherte Leseposition wieder her.
    pub fn restore_checkpoint(&mut self, cp: BitReaderCheckpoint) {
        self.byte_pos = cp.byte_pos;
        self.accum = cp.accum;
        self.accum_bits = cp.accum_bits;
    }

    /// Sets the bit position for seeking/backtracking.
    ///
    /// # Panics
    ///
    /// Panics if `pos` exceeds the data length in bits.
    pub fn set_bit_position(&mut self, pos: usize) {
        assert!(
            pos <= self.data.len() * 8,
            "bit position {pos} exceeds data length {} bits",
            self.data.len() * 8
        );
        self.byte_pos = pos / 8;
        self.accum = 0;
        self.accum_bits = 0;
        let partial = (pos % 8) as u8;
        if partial > 0 {
            let byte = self.data[self.byte_pos];
            self.byte_pos += 1;
            let remaining = 8 - partial;
            let mask = (1u8 << remaining) - 1;
            let bits = byte & mask;
            self.accum = (bits as u64) << (64 - remaining);
            self.accum_bits = remaining;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn writer_default() {
        let w = BitWriter::default();
        assert_eq!(w.bit_position(), 0);
        assert_eq!(w.into_vec(), vec![]);
    }

    // --- Test 1: Single bit write/read (Spec 7.1 MSB first) ---

    #[test]
    fn write_read_single_bit_true() {
        let mut w = BitWriter::new();
        w.write_bit(true);
        let data = w.into_vec();
        assert_eq!(data, vec![0b1000_0000]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bit().unwrap(), true);
    }

    #[test]
    fn write_read_single_bit_false() {
        let mut w = BitWriter::new();
        w.write_bit(false);
        let data = w.into_vec();
        assert_eq!(data, vec![0b0000_0000]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bit().unwrap(), false);
    }

    // --- Test 2: n-bit values (Spec 7.1) ---

    #[test]
    fn write_read_3_bits() {
        let mut w = BitWriter::new();
        w.write_bits(0b101, 3);
        let data = w.into_vec();
        // 101 followed by 5 padding zeros = 0b1010_0000
        assert_eq!(data, vec![0b1010_0000]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(3).unwrap(), 0b101);
    }

    #[test]
    fn write_read_8_bits() {
        let mut w = BitWriter::new();
        w.write_bits(0xAB, 8);
        let data = w.into_vec();
        assert_eq!(data, vec![0xAB]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(8).unwrap(), 0xAB);
    }

    #[test]
    fn write_read_16_bits() {
        let mut w = BitWriter::new();
        w.write_bits(0xCAFE, 16);
        let data = w.into_vec();
        assert_eq!(data, vec![0xCA, 0xFE]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(16).unwrap(), 0xCAFE);
    }

    #[test]
    fn write_read_64_bits() {
        let val: u64 = 0xDEAD_BEEF_CAFE_BABE;
        let mut w = BitWriter::new();
        w.write_bits(val, 64);
        let data = w.into_vec();
        assert_eq!(data.len(), 8);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(64).unwrap(), val);
    }

    #[test]
    fn write_read_64_bits_unaligned() {
        let val: u64 = 0xDEAD_BEEF_CAFE_BABE;
        let mut w = BitWriter::new();
        w.write_bits(0b101, 3); // misalign by 3 bits
        w.write_bits(val, 64);
        let data = w.into_vec();
        // 3 + 64 = 67 bits → 9 bytes
        assert_eq!(data.len(), 9);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(3).unwrap(), 0b101);
        assert_eq!(r.read_bits(64).unwrap(), val);
    }

    // --- Test 3: Values crossing byte boundaries ---

    #[test]
    fn cross_byte_boundary() {
        let mut w = BitWriter::new();
        w.write_bits(0b11, 2); // 2 bits
        w.write_bits(0b1010_1010_10, 10); // 10 bits crossing byte boundary
        let data = w.into_vec();
        // 11_1010_1010_10 + 4 padding = 11_1010_10 | 10_10_0000
        assert_eq!(data, vec![0b1110_1010, 0b1010_0000]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(2).unwrap(), 0b11);
        assert_eq!(r.read_bits(10).unwrap(), 0b1010_1010_10);
    }

    // --- Test 4: 0-bit write/read is no-op (Spec 6.2) ---

    #[test]
    fn zero_bit_write_is_noop() {
        let mut w = BitWriter::new();
        w.write_bits(0xFF, 0);
        assert_eq!(w.bit_position(), 0);
        assert_eq!(w.into_vec(), vec![]);
    }

    #[test]
    fn zero_bit_read_is_noop() {
        let mut r = BitReader::new(&[]);
        assert_eq!(r.read_bits(0).unwrap(), 0);
        assert_eq!(r.bit_position(), 0);
    }

    // --- Test 5: align_to_byte at various positions (Spec 5.2 header padding) ---

    #[test]
    fn align_to_byte_from_partial() {
        let mut w = BitWriter::new();
        w.write_bits(0b111, 3);
        assert_eq!(w.bit_position(), 3);
        w.align_to_byte();
        assert_eq!(w.bit_position(), 8);
        assert_eq!(w.into_vec(), vec![0b1110_0000]);
    }

    #[test]
    fn align_to_byte_from_1_bit() {
        let mut w = BitWriter::new();
        w.write_bit(true);
        w.align_to_byte();
        assert_eq!(w.bit_position(), 8);
        assert_eq!(w.into_vec(), vec![0b1000_0000]);
    }

    #[test]
    fn align_to_byte_from_7_bits() {
        let mut w = BitWriter::new();
        w.write_bits(0b1111111, 7);
        w.align_to_byte();
        assert_eq!(w.bit_position(), 8);
        assert_eq!(w.into_vec(), vec![0b1111_1110]);
    }

    // --- Test 6: align_to_byte when already aligned (no-op) ---

    #[test]
    fn writer_align_already_aligned() {
        let mut w = BitWriter::new();
        w.write_bits(0xFF, 8);
        let pos_before = w.bit_position();
        w.align_to_byte();
        assert_eq!(w.bit_position(), pos_before);
    }

    #[test]
    fn writer_align_at_zero() {
        let mut w = BitWriter::new();
        w.align_to_byte();
        assert_eq!(w.bit_position(), 0);
        assert_eq!(w.into_vec(), vec![]);
    }

    #[test]
    fn reader_align_already_aligned() {
        let mut r = BitReader::new(&[0xFF]);
        r.read_bits(8).unwrap();
        let pos_before = r.bit_position();
        r.align_to_byte();
        assert_eq!(r.bit_position(), pos_before);
    }

    #[test]
    fn reader_align_at_zero() {
        let mut r = BitReader::new(&[0xFF]);
        r.align_to_byte();
        assert_eq!(r.bit_position(), 0);
    }

    // --- Test 7: bit_position tracking ---

    #[test]
    fn writer_position_tracking() {
        let mut w = BitWriter::new();
        assert_eq!(w.bit_position(), 0);
        w.write_bit(true);
        assert_eq!(w.bit_position(), 1);
        w.write_bits(0, 5);
        assert_eq!(w.bit_position(), 6);
        w.align_to_byte();
        assert_eq!(w.bit_position(), 8);
        w.write_bits(0, 16);
        assert_eq!(w.bit_position(), 24);
    }

    #[test]
    fn reader_position_tracking() {
        let data = [0xFF, 0xFF, 0xFF];
        let mut r = BitReader::new(&data);
        assert_eq!(r.bit_position(), 0);
        r.read_bit().unwrap();
        assert_eq!(r.bit_position(), 1);
        r.read_bits(5).unwrap();
        assert_eq!(r.bit_position(), 6);
        r.align_to_byte();
        assert_eq!(r.bit_position(), 8);
        r.read_bits(16).unwrap();
        assert_eq!(r.bit_position(), 24);
    }

    // --- Test 8: Round-trip writer → reader ---

    #[test]
    fn round_trip_mixed_values() {
        let mut w = BitWriter::new();
        w.write_bit(true);
        w.write_bits(42, 7);
        w.write_bits(0xBEEF, 16);
        w.write_bit(false);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bit().unwrap(), true);
        assert_eq!(r.read_bits(7).unwrap(), 42);
        assert_eq!(r.read_bits(16).unwrap(), 0xBEEF);
        assert_eq!(r.read_bit().unwrap(), false);
    }

    #[test]
    fn round_trip_with_alignment() {
        let mut w = BitWriter::new();
        w.write_bits(0b101, 3);
        w.align_to_byte();
        w.write_bits(0xAB, 8);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(3).unwrap(), 0b101);
        r.align_to_byte();
        assert_eq!(r.read_bits(8).unwrap(), 0xAB);
    }

    // --- Test 9: EOF → PrematureEndOfStream ---

    #[test]
    fn read_bit_eof() {
        let mut r = BitReader::new(&[]);
        let err = r.read_bit().unwrap_err();
        assert_eq!(err, Error::PrematureEndOfStream);
    }

    #[test]
    fn read_bits_eof() {
        let mut r = BitReader::new(&[]);
        let err = r.read_bits(8).unwrap_err();
        assert_eq!(err, Error::PrematureEndOfStream);
    }

    // --- Test 10: Partial EOF (bit_pos must not advance on error) ---

    #[test]
    fn read_bits_partial_eof() {
        let mut r = BitReader::new(&[0xFF]);
        // Read 4 bits successfully, then try to read 8 more (only 4 remain)
        assert_eq!(r.read_bits(4).unwrap(), 0xF);
        let pos_before = r.bit_position();
        let err = r.read_bits(8).unwrap_err();
        assert_eq!(err, Error::PrematureEndOfStream);
        // bit_pos must be unchanged after failed read_bits
        assert_eq!(r.bit_position(), pos_before);
    }

    #[test]
    fn read_bits_exact_boundary_then_eof() {
        let mut r = BitReader::new(&[0xAB]);
        assert_eq!(r.read_bits(8).unwrap(), 0xAB);
        let err = r.read_bit().unwrap_err();
        assert_eq!(err, Error::PrematureEndOfStream);
    }

    // --- Test 11: write_bits uses only the lower n bits (Spec 7.1) ---

    #[test]
    fn write_bits_ignores_higher_bits() {
        // 0b1011_0000 has bits set above position 2, but n=3 means only lower 3 bits (0b000) are written.
        let mut w = BitWriter::new();
        w.write_bits(0b1011_0000, 3);
        let data = w.into_vec();
        // Lower 3 bits of 0b1011_0000 = 0b000 → 0b0000_0000
        assert_eq!(data, vec![0b0000_0000]);
    }

    #[test]
    fn write_bits_lower_bits_only() {
        // val=0xFF, n=4 → lower 4 bits = 0b1111
        let mut w = BitWriter::new();
        w.write_bits(0xFF, 4);
        let data = w.into_vec();
        assert_eq!(data, vec![0b1111_0000]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(4).unwrap(), 0b1111);
    }

    // --- Test 12: Defensive branch coverage for write_bits/align_to_byte ---
    // These test internal states where bit_pos points past the buffer,
    // exercising defensive push-on-demand branches.

    #[test]
    fn write_bits_with_accum_state() {
        // Akkumulator hat Bits, danach weitere Bits schreiben
        let mut w = BitWriter::new();
        w.write_bit(false); // accum_bits=1, accum=0
        w.write_bits(0b101, 3); // accum_bits=4, accum=0b0101
        assert_eq!(w.bit_position(), 4);
        let data = w.into_vec();
        // Bits: 0_101_0000
        assert_eq!(data, vec![0b0101_0000]);
    }

    #[test]
    fn align_to_byte_from_accum() {
        // Akkumulator hat 3 Bits, align_to_byte flusht mit Padding
        let mut w = BitWriter::new();
        w.write_bits(0b000, 3); // accum_bits=3
        w.align_to_byte();
        assert_eq!(w.bit_position(), 8);
        let data = w.into_vec();
        assert_eq!(data, vec![0x00]);
    }

    // --- Test 13: write_byte_aligned ---

    #[test]
    fn write_byte_aligned_fast_path() {
        let mut w = BitWriter::new();
        w.write_byte_aligned(0xAB);
        w.write_byte_aligned(0xCD);
        let data = w.into_vec();
        assert_eq!(data, vec![0xAB, 0xCD]);
    }

    #[test]
    fn write_byte_aligned_unaligned_fallback() {
        let mut w = BitWriter::new();
        w.write_bit(true); // misalign
        w.write_byte_aligned(0xFF);
        let data = w.into_vec();
        // 1_11111111_000000 padded = 0b1_1111111 0b1_0000000
        assert_eq!(data, vec![0xFF, 0x80]);
    }

    // --- Test 14: read_byte_aligned ---

    #[test]
    fn read_byte_aligned_fast_path() {
        let mut r = BitReader::new(&[0xAB, 0xCD]);
        assert_eq!(r.read_byte_aligned().unwrap(), 0xAB);
        assert_eq!(r.read_byte_aligned().unwrap(), 0xCD);
    }

    #[test]
    fn read_byte_aligned_unaligned_fallback() {
        let mut r = BitReader::new(&[0xFF, 0x80]);
        r.read_bits(1).unwrap(); // misalign
        assert_eq!(r.read_byte_aligned().unwrap(), 0xFF); // falls back to read_bits(8)
    }

    #[test]
    fn read_byte_aligned_eof_empty() {
        let mut r = BitReader::new(&[]);
        assert_eq!(
            r.read_byte_aligned().unwrap_err(),
            Error::PrematureEndOfStream
        );
    }

    #[test]
    fn read_byte_aligned_eof_after_last() {
        let mut r = BitReader::new(&[0xAB]);
        assert_eq!(r.read_byte_aligned().unwrap(), 0xAB);
        assert_eq!(
            r.read_byte_aligned().unwrap_err(),
            Error::PrematureEndOfStream
        );
    }

    // --- Test 14a: write_bits_2/3, read_bits_2/3 ---

    #[test]
    fn write_read_bits_2_aligned() {
        let mut w = BitWriter::new();
        w.write_bits_2(0b11);
        w.write_bits_2(0b01);
        w.write_bits_2(0b10);
        w.write_bits_2(0b00);
        let data = w.into_vec();
        assert_eq!(data, vec![0b11_01_10_00]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits_2().unwrap(), 0b11);
        assert_eq!(r.read_bits_2().unwrap(), 0b01);
        assert_eq!(r.read_bits_2().unwrap(), 0b10);
        assert_eq!(r.read_bits_2().unwrap(), 0b00);
    }

    #[test]
    fn write_read_bits_2_misaligned() {
        let mut w = BitWriter::new();
        w.write_bit(true); // offset=1
        w.write_bits_2(0b11);
        w.write_bits_2(0b01);
        // 1_11_01_000 = 0b1_11_01_000 = 0xE8
        let data = w.into_vec();
        assert_eq!(data, vec![0xE8]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bit().unwrap(), true);
        assert_eq!(r.read_bits_2().unwrap(), 0b11);
        assert_eq!(r.read_bits_2().unwrap(), 0b01);
    }

    #[test]
    fn write_read_bits_2_spanning_boundary() {
        let mut w = BitWriter::new();
        w.write_bits(0b1111111, 7); // offset=7
        w.write_bits_2(0b10); // spans byte boundary
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(7).unwrap(), 0b1111111);
        assert_eq!(r.read_bits_2().unwrap(), 0b10);
    }

    #[test]
    fn write_read_bits_3_aligned() {
        let mut w = BitWriter::new();
        w.write_bits_3(0b101);
        w.write_bits_3(0b011);
        // 101_011_00 = 0xAC
        let data = w.into_vec();
        assert_eq!(data, vec![0xAC]);

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits_3().unwrap(), 0b101);
        assert_eq!(r.read_bits_3().unwrap(), 0b011);
    }

    #[test]
    fn write_read_bits_3_spanning_boundary() {
        let mut w = BitWriter::new();
        w.write_bits(0b111111, 6); // offset=6
        w.write_bits_3(0b101); // spans byte boundary
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(6).unwrap(), 0b111111);
        assert_eq!(r.read_bits_3().unwrap(), 0b101);
    }

    #[test]
    fn read_bits_2_eof() {
        let mut r = BitReader::new(&[]);
        assert_eq!(r.read_bits_2().unwrap_err(), Error::PrematureEndOfStream);
    }

    #[test]
    fn read_bits_3_eof() {
        let mut r = BitReader::new(&[]);
        assert_eq!(r.read_bits_3().unwrap_err(), Error::PrematureEndOfStream);
    }

    // --- Test 14b: write_bytes_aligned / read_bytes_aligned ---

    #[test]
    fn write_bytes_aligned_fast_path() {
        let mut w = BitWriter::new();
        w.write_bytes_aligned(&[0xAB, 0xCD, 0xEF]);
        let data = w.into_vec();
        assert_eq!(data, vec![0xAB, 0xCD, 0xEF]);
    }

    #[test]
    fn write_bytes_aligned_unaligned_fallback() {
        let mut w = BitWriter::new();
        w.write_bit(true); // misalign
        w.write_bytes_aligned(&[0xFF, 0x00]);
        let data = w.into_vec();
        // 1_11111111_00000000_0000000 = 0xFF 0x80 0x00
        assert_eq!(data, vec![0xFF, 0x80, 0x00]);
    }

    #[test]
    fn write_bytes_aligned_empty() {
        let mut w = BitWriter::new();
        w.write_bytes_aligned(&[]);
        assert_eq!(w.bit_position(), 0);
        assert_eq!(w.into_vec(), vec![]);
    }

    #[test]
    fn read_bytes_aligned_fast_path() {
        let mut r = BitReader::new(&[0xAB, 0xCD, 0xEF]);
        let mut buf = [0u8; 3];
        r.read_bytes_aligned(&mut buf).unwrap();
        assert_eq!(buf, [0xAB, 0xCD, 0xEF]);
    }

    #[test]
    fn read_bytes_aligned_unaligned_fallback() {
        let mut r = BitReader::new(&[0xFF, 0x80, 0x00]);
        r.read_bits(1).unwrap(); // misalign
        let mut buf = [0u8; 2];
        r.read_bytes_aligned(&mut buf).unwrap();
        assert_eq!(buf, [0xFF, 0x00]);
    }

    #[test]
    fn read_bytes_aligned_eof() {
        let mut r = BitReader::new(&[0xAB]);
        let mut buf = [0u8; 2];
        assert_eq!(
            r.read_bytes_aligned(&mut buf).unwrap_err(),
            Error::PrematureEndOfStream
        );
    }

    #[test]
    fn read_bytes_aligned_empty() {
        let mut r = BitReader::new(&[]);
        let mut buf = [0u8; 0];
        r.read_bytes_aligned(&mut buf).unwrap();
    }

    #[test]
    fn write_read_bytes_aligned_round_trip() {
        let original = b"Hello, EXI!";
        let mut w = BitWriter::new();
        w.write_bytes_aligned(original);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let mut buf = [0u8; 11];
        r.read_bytes_aligned(&mut buf).unwrap();
        assert_eq!(&buf, original);
    }

    // --- Test 15: Mixed sequence with alignment ---

    #[test]
    fn mixed_sequence_bits_align_bits() {
        let mut w = BitWriter::new();
        w.write_bits(0b110, 3); // 3 bits
        w.write_bit(true); // 1 bit (pos=4)
        w.align_to_byte(); // pad to 8
        w.write_bits(0xDE, 8); // 8 bits
        w.write_bits(0b10, 2); // 2 bits
        w.align_to_byte(); // pad to 24
        w.write_bit(false); // 1 bit
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(3).unwrap(), 0b110);
        assert_eq!(r.read_bit().unwrap(), true);
        r.align_to_byte();
        assert_eq!(r.read_bits(8).unwrap(), 0xDE);
        assert_eq!(r.read_bits(2).unwrap(), 0b10);
        r.align_to_byte();
        assert_eq!(r.read_bit().unwrap(), false);
    }

    // --- Test 16: set_bit_position ---

    #[test]
    fn set_bit_position_basic() {
        let mut r = BitReader::new(&[0xAB, 0xCD]);
        r.read_bits(8).unwrap();
        assert_eq!(r.bit_position(), 8);
        r.set_bit_position(0);
        assert_eq!(r.bit_position(), 0);
        assert_eq!(r.read_bits(8).unwrap(), 0xAB);
    }

    #[test]
    fn set_bit_position_mid_byte() {
        let mut r = BitReader::new(&[0xFF]);
        r.set_bit_position(4);
        assert_eq!(r.read_bits(4).unwrap(), 0xF);
    }

    #[test]
    fn set_bit_position_to_end() {
        let mut r = BitReader::new(&[0xAB]);
        r.set_bit_position(8);
        assert_eq!(r.bit_position(), 8);
        assert_eq!(r.read_bit().unwrap_err(), Error::PrematureEndOfStream);
    }

    #[test]
    #[should_panic(expected = "exceeds data length")]
    fn set_bit_position_beyond_end_panics() {
        let mut r = BitReader::new(&[0xAB]);
        r.set_bit_position(9);
    }

    // --- Test 16a: Akkumulator-spezifische Tests ---

    #[test]
    fn read_bits_57_boundary() {
        // Testet den n > 56 Zweistufig-Pfad mit n = 57
        let mut w = BitWriter::new();
        w.write_bits(0x01_DEAD_BEEF_CAFE_BA, 57);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(57).unwrap(), 0x01_DEAD_BEEF_CAFE_BA);
    }

    #[test]
    fn read_bits_57_unaligned() {
        // n > 56 nach vorherigem Misalignment
        let mut w = BitWriter::new();
        w.write_bits(0b11, 2);
        w.write_bits(0x01_DEAD_BEEF_CAFE_BA, 57);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(r.read_bits(2).unwrap(), 0b11);
        assert_eq!(r.read_bits(57).unwrap(), 0x01_DEAD_BEEF_CAFE_BA);
    }

    #[test]
    fn peek_skip_with_accumulator_content() {
        // peek/skip wenn Akkumulator Bytes enthält (nach align_to_byte)
        let data = [0xFF, 0xAA, 0xBB, 0xCC, 0xDD];
        let mut r = BitReader::new(&data);
        // Lese 3 Bits → Akkumulator hat ~5 Bytes geladen
        assert_eq!(r.read_bits(3).unwrap(), 0b111);
        // Align → Akkumulator hat ganze Bytes (5 Bits verworfen)
        r.align_to_byte();
        assert_eq!(r.bit_position(), 8);
        // peek sollte ab logischer Byte-Position funktionieren
        let peeked = r.peek_aligned_bytes(4).unwrap();
        assert_eq!(peeked, &[0xAA, 0xBB, 0xCC, 0xDD]);
        // skip und weiter lesen
        r.skip_aligned_bytes(4);
        assert_eq!(r.bit_position(), 40);
        assert_eq!(r.remaining_bits(), 0);
    }

    #[test]
    fn set_bit_position_then_peek() {
        let data = [0x11, 0x22, 0x33, 0x44];
        let mut r = BitReader::new(&data);
        // Lese etwas um Akkumulator zu füllen
        r.read_bits(16).unwrap();
        assert_eq!(r.bit_position(), 16);
        // Springe zurück
        r.set_bit_position(8);
        assert_eq!(r.bit_position(), 8);
        // peek ab Position 8 (Byte 1)
        let peeked = r.peek_aligned_bytes(3).unwrap();
        assert_eq!(peeked, &[0x22, 0x33, 0x44]);
    }

    // --- BitReader Checkpoint Tests ---

    #[test]
    fn reader_checkpoint_basic_rollback() {
        let mut r = BitReader::new(&[0xAB, 0xCD]);
        let cp = r.save_checkpoint();
        assert_eq!(r.read_bits(8).unwrap(), 0xAB);
        assert_eq!(r.bit_position(), 8);
        r.restore_checkpoint(cp);
        assert_eq!(r.bit_position(), 0);
        assert_eq!(r.read_bits(8).unwrap(), 0xAB);
    }

    #[test]
    fn reader_checkpoint_mid_byte() {
        let mut r = BitReader::new(&[0xFF, 0x00]);
        assert_eq!(r.read_bits(3).unwrap(), 0b111);
        let cp = r.save_checkpoint();
        assert_eq!(r.read_bits(5).unwrap(), 0b11111);
        assert_eq!(r.read_bits(8).unwrap(), 0x00);
        r.restore_checkpoint(cp);
        assert_eq!(r.bit_position(), 3);
        assert_eq!(r.read_bits(5).unwrap(), 0b11111);
    }

    #[test]
    fn reader_checkpoint_no_rollback() {
        let mut r = BitReader::new(&[0xAB, 0xCD]);
        let _cp = r.save_checkpoint();
        assert_eq!(r.read_bits(16).unwrap(), 0xABCD);
        // Checkpoint nicht restore'd — einfach weiter
        assert_eq!(r.remaining_bits(), 0);
    }

    // --- drain_to() Tests ---

    #[test]
    fn drain_to_byte_aligned() {
        let mut w = BitWriter::new();
        w.write_bits(0xAB, 8);
        w.write_bits(0xCD, 8);
        let mut out = Vec::new();
        w.drain_to(&mut out).unwrap();
        assert_eq!(out, vec![0xAB, 0xCD]);
        assert_eq!(w.bit_position(), 0);
        assert_eq!(w.buf_len(), 0);
        // Weitere Daten schreiben nach drain
        w.write_bits(0xEF, 8);
        let rest = w.into_vec();
        assert_eq!(rest, vec![0xEF]);
    }

    #[test]
    fn drain_to_with_partial_byte() {
        let mut w = BitWriter::new();
        w.write_bits(0xAB, 8);
        w.write_bits(0b101, 3); // 3 Bits = partielles Byte
        let mut out = Vec::new();
        w.drain_to(&mut out).unwrap();
        assert_eq!(out, vec![0xAB]); // Nur vollständiges Byte
        assert_eq!(w.bit_position(), 3); // 3 Bits bleiben
        // Weiter schreiben und finalisieren
        w.write_bits(0b01010, 5); // Byte komplett
        w.write_bits(0xEF, 8);
        let rest = w.into_vec();
        assert_eq!(rest, vec![0b1010_1010, 0xEF]);
    }

    #[test]
    fn drain_to_empty_noop() {
        let mut w = BitWriter::new();
        let mut out = Vec::new();
        w.drain_to(&mut out).unwrap();
        assert!(out.is_empty());
    }

    #[test]
    fn drain_to_result_matches_into_vec() {
        // Gleiche Bits, einmal mit drain, einmal ohne → identisches Ergebnis
        let mut w1 = BitWriter::new();
        w1.write_bits(0xDE, 8);
        w1.write_bits(0xAD, 8);
        w1.write_bits(0b1011, 4);
        w1.write_bits(0xBE, 8);
        w1.write_bits(0xEF, 8);
        let expected = w1.into_vec();

        let mut w2 = BitWriter::new();
        let mut out = Vec::new();
        w2.write_bits(0xDE, 8);
        w2.write_bits(0xAD, 8);
        w2.drain_to(&mut out).unwrap();
        w2.write_bits(0b1011, 4);
        w2.write_bits(0xBE, 8);
        w2.drain_to(&mut out).unwrap();
        w2.write_bits(0xEF, 8);
        w2.align_to_byte();
        out.extend_from_slice(w2.bytes());
        assert_eq!(out, expected);
    }

    #[test]
    #[should_panic(expected = "aktivem Checkpoint")]
    fn drain_to_panics_with_active_checkpoint() {
        let mut w = BitWriter::new();
        w.write_bits(0xAB, 8);
        let _cp = w.save_checkpoint();
        let mut out = Vec::new();
        w.drain_to(&mut out).unwrap(); // Muss paniken
    }

    #[test]
    fn drain_to_after_checkpoint_restore() {
        let mut w = BitWriter::new();
        w.write_bits(0xAB, 8);
        let cp = w.save_checkpoint();
        w.write_bits(0xCD, 8);
        w.restore_checkpoint(cp);
        // Checkpoint ist jetzt aufgelöst, drain_to() sollte funktionieren
        let mut out = Vec::new();
        w.drain_to(&mut out).unwrap();
        assert_eq!(out, vec![0xAB]);
    }

    #[test]
    fn drain_to_after_checkpoint_discard() {
        let mut w = BitWriter::new();
        w.write_bits(0xAB, 8);
        let _cp = w.save_checkpoint();
        w.write_bits(0xCD, 8);
        w.discard_checkpoint();
        // Checkpoint verworfen, drain_to() sollte funktionieren
        let mut out = Vec::new();
        w.drain_to(&mut out).unwrap();
        assert_eq!(out, vec![0xAB, 0xCD]);
    }
}
