//! EXI Header Encoding/Decoding (Spec 5, 5.1, 5.2, 5.3).
//!
//! Der EXI Header hat folgende Struktur:
//! - [EXI Cookie] (optional): `$EXI` als 4 ASCII-Bytes (Spec 5.1)
//! - Distinguishing Bits (required): `10` als 2 Bits (Spec 5.2)
//! - Presence Bit (required): 1 Bit, 1=Options vorhanden (Spec 5)
//! - EXI Format Version (required): Preview-Bit + 4-Bit-Chunks (Spec 5.3)
//! - [EXI Options] (optional): Siehe Issue #20
//! - [Padding Bits] (optional): Bei byte-alignment/compression (Spec 5)
//!
//! Die EXI Options werden in einem separaten Modul behandelt (Issue #20).
//! Dieses Modul behandelt nur den Header ohne Options-Inhalt.
//!
//! # Beispiel
//!
//! ```
//! use erxi::header::ExiHeader;
//!
//! let header = ExiHeader::default();
//! assert!(!header.cookie());
//! assert!(!header.preview());
//! assert_eq!(header.version(), 1);
//! ```

use std::num::NonZeroU16;

use crate::bitstream::{BitReader, BitWriter};
use crate::n_bit_unsigned_integer;
use crate::{Error, Result};

/// EXI Cookie als ASCII-Bytes: "$EXI"
const EXI_COOKIE: [u8; 4] = [0x24, 0x45, 0x58, 0x49]; // $, E, X, I

/// Distinguishing Bits: `10` (Spec 5.2)
const DISTINGUISHING_BITS: u8 = 0b10;

/// EXI Header (Spec 5).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExiHeader {
    cookie: bool,
    preview: bool,
    version: NonZeroU16,
    options_present: bool,
}

impl Default for ExiHeader {
    fn default() -> Self {
        Self {
            cookie: false,
            preview: false,
            // SAFETY: 1 != 0
            version: NonZeroU16::new(1).unwrap(),
            options_present: false,
        }
    }
}

impl ExiHeader {
    /// Erstellt einen Header fuer EXI Final Version 1 ohne Cookie und ohne Options.
    pub fn new() -> Self {
        Self::default()
    }

    // --- Getter ---

    /// EXI Cookie vorhanden (Spec 5.1).
    pub fn cookie(&self) -> bool {
        self.cookie
    }

    /// Preview-Version (true) oder Final-Version (false) (Spec 5.3).
    pub fn preview(&self) -> bool {
        self.preview
    }

    /// Format-Versionsnummer, ab 1 (Spec 5.3).
    pub fn version(&self) -> u16 {
        self.version.get()
    }

    /// EXI Options im Header vorhanden (Spec 5).
    pub fn options_present(&self) -> bool {
        self.options_present
    }

    // --- Builder ---

    /// Setzt das Cookie-Flag.
    pub fn with_cookie(mut self) -> Self {
        self.cookie = true;
        self
    }

    /// Setzt das Options-Presence-Flag.
    pub fn with_options(mut self) -> Self {
        self.options_present = true;
        self
    }

    /// Setzt die Versionsnummer (muss > 0 sein, wird sonst auf 1 gesetzt).
    pub fn with_version(mut self, version: u16) -> Self {
        self.version = NonZeroU16::new(version).unwrap_or(NonZeroU16::new(1).unwrap());
        self
    }

    /// Markiert als Preview-Version.
    pub fn as_preview(mut self) -> Self {
        self.preview = true;
        self
    }
}

/// Encodiert den EXI Header (Spec 5).
///
/// Der Parameter `needs_padding` gibt an, ob Padding-Bits am Ende hinzugefügt
/// werden sollen (bei byte-alignment oder compression).
///
pub fn encode(writer: &mut BitWriter, header: &ExiHeader, needs_padding: bool) -> Result<()> {
    // 1. Optional: EXI Cookie (Spec 5.1)
    if header.cookie {
        for &byte in &EXI_COOKIE {
            writer.write_byte_aligned(byte);
        }
    }

    // 2. Distinguishing Bits: 10 (Spec 5.2)
    writer.write_bits(u64::from(DISTINGUISHING_BITS), 2);

    // 3. Presence Bit für EXI Options (Spec 5)
    writer.write_bit(header.options_present());

    // 4. EXI Format Version (Spec 5.3)
    encode_version(writer, header.preview, header.version.get());

    // 5. Padding Bits (Spec 5)
    if needs_padding {
        writer.align_to_byte();
    }

    Ok(())
}

/// Encodiert die EXI Format Version (Spec 5.3).
///
/// Format:
/// - 1 Bit: Preview (1) oder Final (0)
/// - Sequenz von 4-Bit Unsigned Integers für (version - 1)
/// - Wert 15 signalisiert Fortsetzung, Werte 0-14 terminieren
fn encode_version(writer: &mut BitWriter, preview: bool, version: u16) {
    // Preview-Bit
    writer.write_bit(preview);

    // Version minus 1, dann als 4-Bit-Chunks mit 15 als Fortsetzungs-Marker
    let mut remaining = version - 1;

    loop {
        if remaining <= 14 {
            // Terminierender Wert (0-14)
            n_bit_unsigned_integer::encode(writer, u64::from(remaining), 4);
            break;
        } else {
            // Fortsetzungs-Marker (15)
            n_bit_unsigned_integer::encode(writer, 15, 4);
            remaining -= 15;
        }
    }
}

/// Decodiert den EXI Header (Spec 5).
///
/// Der Parameter `needs_padding` gibt an, ob Padding-Bits am Ende erwartet
/// werden (bei byte-alignment oder compression).
///
/// # Errors
///
/// - [`Error::InvalidDistinguishingBits`] wenn die Bits nicht `10` sind
/// - [`Error::PrematureEndOfStream`] bei unerwartetem Stream-Ende
pub fn decode(reader: &mut BitReader, needs_padding: bool) -> Result<ExiHeader> {
    // 1. Prüfe auf EXI Cookie (Spec 5.1)
    let cookie = try_decode_cookie(reader)?;

    // 2. Distinguishing Bits: 10 (Spec 5.2)
    let dist_bits = n_bit_unsigned_integer::decode(reader, 2)? as u8;
    if dist_bits != DISTINGUISHING_BITS {
        return Err(Error::InvalidDistinguishingBits(dist_bits));
    }

    // 3. Presence Bit für EXI Options (Spec 5)
    let options_present = reader.read_bit()?;

    // 4. EXI Format Version (Spec 5.3)
    let (preview, version) = decode_version(reader)?;

    // Spec 5.3: Nur Final Version 1 ist unterstuetzt
    if preview || version != 1 {
        return Err(Error::UnsupportedVersion);
    }

    // 5. Padding Bits überspringen (Spec 5)
    if needs_padding {
        reader.align_to_byte();
    }

    // Version ist validiert (== 1), also sicher NonZero
    let version = NonZeroU16::new(version).unwrap();

    Ok(ExiHeader {
        cookie,
        preview,
        version,
        options_present,
    })
}

/// Versucht den EXI Cookie zu decodieren (Spec 5.1).
///
/// Prüft ob die nächsten 4 Bytes `$EXI` sind. Falls ja, werden sie konsumiert
/// und `true` zurückgegeben. Falls nein, bleibt der Reader unverändert und
/// `false` wird zurückgegeben.
///
/// Wenn das erste Byte `$` ist aber nicht genug Bytes für den Cookie folgen,
/// wird [`Error::PrematureEndOfStream`] zurückgegeben (truncated Cookie).
fn try_decode_cookie(reader: &mut BitReader) -> Result<bool> {
    let cp = reader.save_checkpoint();

    // Versuche 4 Bytes zu lesen
    let mut bytes = [0u8; 4];
    for (i, byte) in bytes.iter_mut().enumerate() {
        match reader.read_bits(8) {
            Ok(b) => *byte = b as u8,
            Err(e) => {
                // EOF: Wenn erstes Byte '$' war, ist es ein truncated Cookie
                if i > 0 && bytes[0] == EXI_COOKIE[0] {
                    return Err(e);
                }
                // Sonst: Nicht genug Bytes, kein Cookie
                reader.restore_checkpoint(cp);
                return Ok(false);
            }
        }
    }

    if bytes == EXI_COOKIE {
        Ok(true)
    } else {
        // Kein Cookie, Rollback
        reader.restore_checkpoint(cp);
        Ok(false)
    }
}

/// Decodiert die EXI Format Version (Spec 5.3).
fn decode_version(reader: &mut BitReader) -> Result<(bool, u16)> {
    // Preview-Bit
    let preview = reader.read_bit()?;

    // 4-Bit-Chunks summieren bis terminierender Wert (0-14)
    let mut version: u16 = 1;

    loop {
        let chunk = n_bit_unsigned_integer::decode(reader, 4)? as u16;
        version = version.saturating_add(chunk);

        if chunk <= 14 {
            break;
        }
    }

    Ok((preview, version))
}

#[cfg(test)]
mod tests {
    use super::*;

    // === Helper ===

    fn round_trip(header: &ExiHeader, needs_padding: bool) -> ExiHeader {
        let mut writer = BitWriter::new();
        encode(&mut writer, header, needs_padding).unwrap();
        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);
        decode(&mut reader, needs_padding).unwrap()
    }

    fn encode_to_vec(header: &ExiHeader, needs_padding: bool) -> Vec<u8> {
        let mut writer = BitWriter::new();
        encode(&mut writer, header, needs_padding).unwrap();
        writer.into_vec()
    }

    // === Spec 5.1: EXI Cookie ===

    /// Spec 5.1: Cookie ist optional, Header ohne Cookie
    #[test]
    fn header_without_cookie() {
        let header = ExiHeader::new();
        assert!(!header.cookie());
        let decoded = round_trip(&header, false);
        assert!(!decoded.cookie());
    }

    /// Spec 5.1: Cookie "$EXI" als 4 ASCII-Bytes
    #[test]
    fn header_with_cookie() {
        let header = ExiHeader::new().with_cookie();
        assert!(header.cookie());
        let decoded = round_trip(&header, false);
        assert!(decoded.cookie());
    }

    /// Spec 5.1: Cookie-Bytes sind korrekt
    #[test]
    fn cookie_bytes_correct() {
        let header = ExiHeader::new().with_cookie();
        let data = encode_to_vec(&header, false);
        // Erste 4 Bytes müssen "$EXI" sein
        assert_eq!(&data[0..4], b"$EXI");
    }

    /// Spec 5.1/5.2: Stream ohne Cookie, Rollback funktioniert korrekt
    #[test]
    fn non_cookie_stream_rollback() {
        // Valider Header ohne Cookie: 10 00 0000 = Dist-Bits + Presence=0 + Preview=0 + Version=1
        let data = [0x80];
        let mut reader = BitReader::new(&data);
        let header = decode(&mut reader, false).unwrap();
        assert!(!header.cookie());
        assert!(!header.preview());
        assert_eq!(header.version(), 1);
        assert!(!header.options_present());
    }

    // === Spec 5.2: Distinguishing Bits ===

    /// Spec 5.2: Distinguishing Bits sind `10`
    #[test]
    fn distinguishing_bits_correct() {
        let header = ExiHeader::new();
        let data = encode_to_vec(&header, false);
        // Ohne Cookie: erste 2 Bits sind Distinguishing Bits (10)
        // Bit 0 = 1, Bit 1 = 0
        assert_eq!(data[0] & 0b1100_0000, 0b1000_0000);
    }

    /// Spec 5.2: Ungültige Distinguishing Bits werden abgelehnt (00, 01, 11)
    #[test]
    fn invalid_distinguishing_bits_rejected() {
        fn check_invalid_dist_bits(bits: u8) {
            let mut writer = BitWriter::new();
            writer.write_bits(u64::from(bits), 2);
            writer.write_bit(false); // Presence Bit
            writer.write_bit(false); // Preview Bit
            writer.write_bits(0, 4); // Version 1
            let data = writer.into_vec();

            let mut reader = BitReader::new(&data);
            let err = decode(&mut reader, false).unwrap_err();
            assert!(matches!(err, Error::InvalidDistinguishingBits(b) if b == bits));
        }

        check_invalid_dist_bits(0b00);
        check_invalid_dist_bits(0b01);
        check_invalid_dist_bits(0b11);
    }

    // === Spec 5: Presence Bit ===

    /// Spec 5: Presence Bit = 0 bedeutet keine Options
    #[test]
    fn presence_bit_false() {
        let header = ExiHeader::new();
        assert!(!header.options_present());
        let decoded = round_trip(&header, false);
        assert!(!decoded.options_present());
    }

    /// Spec 5: Presence Bit = 1 bedeutet Options vorhanden
    #[test]
    fn presence_bit_true() {
        let header = ExiHeader::new().with_options();
        assert!(header.options_present());
        let decoded = round_trip(&header, false);
        assert!(decoded.options_present());
    }

    // === Spec 5.3: EXI Format Version ===

    /// Hilfsfunktion: Testet encode_version/decode_version Roundtrip direkt.
    fn version_round_trip(preview: bool, version: u16) -> (bool, u16) {
        let mut writer = BitWriter::new();
        encode_version(&mut writer, preview, version);
        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);
        decode_version(&mut reader).unwrap()
    }

    /// Spec 5.3 Example 5-1: Final Version 1 = `0 0000`
    #[test]
    fn version_final_1() {
        let header = ExiHeader::new().with_version(1);
        let decoded = round_trip(&header, false);
        assert!(!decoded.preview());
        assert_eq!(decoded.version(), 1);
    }

    /// Spec 5.3 Example 5-1: Preview Version 1 = `1 0000`
    #[test]
    fn version_preview_1() {
        let (preview, version) = version_round_trip(true, 1);
        assert!(preview);
        assert_eq!(version, 1);
    }

    /// Spec 5.3 Example 5-1: Final Version 15 = `0 1110`
    #[test]
    fn version_final_15() {
        let (preview, version) = version_round_trip(false, 15);
        assert!(!preview);
        assert_eq!(version, 15);
    }

    /// Spec 5.3 Example 5-1: Final Version 16 = `0 1111 0000`
    #[test]
    fn version_final_16() {
        let (preview, version) = version_round_trip(false, 16);
        assert!(!preview);
        assert_eq!(version, 16);
    }

    /// Spec 5.3 Example 5-1: Final Version 17 = `0 1111 0001`
    #[test]
    fn version_final_17() {
        let (preview, version) = version_round_trip(false, 17);
        assert!(!preview);
        assert_eq!(version, 17);
    }

    /// Spec 5.3: Version 30 = `0 1111 1110`
    #[test]
    fn version_final_30() {
        let (_, version) = version_round_trip(false, 30);
        assert_eq!(version, 30);
    }

    /// Spec 5.3: Version 31 = `0 1111 1111 0000`
    #[test]
    fn version_final_31() {
        let (_, version) = version_round_trip(false, 31);
        assert_eq!(version, 31);
    }

    /// Spec 5.3: Große Versionsnummer
    #[test]
    fn version_large() {
        let (_, version) = version_round_trip(false, 100);
        assert_eq!(version, 100);
    }

    /// Spec 5.3: Sehr große Versionsnummer (Grenzfall)
    #[test]
    fn version_very_large() {
        let (_, version) = version_round_trip(false, 1000);
        assert_eq!(version, 1000);
    }

    /// Spec 5.3: Preview-Version wird beim Decode abgelehnt
    #[test]
    fn decode_preview_rejected() {
        let header = ExiHeader::new().with_version(1).as_preview();
        let data = encode_to_vec(&header, false);
        let mut reader = BitReader::new(&data);
        assert_eq!(decode(&mut reader, false).unwrap_err(), Error::UnsupportedVersion);
    }

    /// Spec 5.3: Version != 1 wird beim Decode abgelehnt
    #[test]
    fn decode_unsupported_version_rejected() {
        let header = ExiHeader::new().with_version(2);
        let data = encode_to_vec(&header, false);
        let mut reader = BitReader::new(&data);
        assert_eq!(decode(&mut reader, false).unwrap_err(), Error::UnsupportedVersion);
    }

    // === Spec 5: Padding ===

    /// Spec 5: Padding bei bit-packed nicht vorhanden
    #[test]
    fn no_padding_bit_packed() {
        let header = ExiHeader::new();
        // Ohne Padding: 2 + 1 + 1 + 4 = 8 Bits = 1 Byte
        let data = encode_to_vec(&header, false);
        assert_eq!(data.len(), 1);
    }

    /// Spec 5: Padding bei byte-alignment/compression
    #[test]
    fn padding_byte_alignment() {
        let header = ExiHeader::new().with_cookie();
        // Mit Cookie: 32 + 2 + 1 + 1 + 4 = 40 Bits = 5 Bytes (genau aligned)
        let data = encode_to_vec(&header, true);
        assert_eq!(data.len(), 5);
    }

    /// Spec 5: Padding fügt Bits hinzu bis Byte-Grenze
    #[test]
    fn padding_adds_bits() {
        // Header mit Version 16 braucht mehr Bits: 2 + 1 + 1 + 8 = 12 Bits
        let header = ExiHeader::new().with_version(16);
        // 12 Bits -> 2 Bytes mit Padding
        let data = encode_to_vec(&header, true);
        assert_eq!(data.len(), 2);
    }

    /// Spec 5: Decode mit Padding überspringt Padding-Bits
    #[test]
    fn decode_with_padding() {
        let header = ExiHeader::new().with_version(1);
        let decoded = round_trip(&header, true);
        assert_eq!(decoded.version(), 1);
    }

    // === Kombinationen ===

    /// Alle Features kombiniert (Version 1 Final)
    #[test]
    fn full_header() {
        let header = ExiHeader::new()
            .with_cookie()
            .with_options()
            .with_version(1);

        let decoded = round_trip(&header, true);
        assert!(decoded.cookie());
        assert!(!decoded.preview());
        assert_eq!(decoded.version(), 1);
        assert!(decoded.options_present());
    }

    /// Default-Werte sind korrekt
    #[test]
    fn default_values() {
        let header = ExiHeader::default();
        assert!(!header.cookie());
        assert!(!header.preview());
        assert_eq!(header.version(), 1);
        assert!(!header.options_present());
    }

    // === Error Cases ===

    /// Decode leerer Stream
    #[test]
    fn decode_empty_stream() {
        let mut reader = BitReader::new(&[]);
        let err = decode(&mut reader, false).unwrap_err();
        assert_eq!(err, Error::PrematureEndOfStream);
    }

    /// Decode zu kurzer Stream: Cookie truncated (nur 3 von 4 Bytes, beginnt mit '$')
    #[test]
    fn decode_truncated_cookie() {
        // "$EX" ohne das finale "I" - erstes Byte ist '$', also truncated Cookie
        let data = [0x24, 0x45, 0x58]; // $EX
        let mut reader = BitReader::new(&data);
        let err = decode(&mut reader, false).unwrap_err();
        assert_eq!(err, Error::PrematureEndOfStream);
    }

    /// Decode zu kurzer Stream: Kein Cookie (beginnt nicht mit '$'), dann Dist-Bits falsch
    #[test]
    fn decode_short_non_cookie_stream() {
        // "ABC" - kein Cookie (beginnt nicht mit '$'), Rollback, dann Dist-Bits prüfen
        // 'A' = 0x41 = 0100_0001, erste 2 Bits sind 01 → InvalidDistinguishingBits
        let data = [0x41, 0x42, 0x43]; // ABC
        let mut reader = BitReader::new(&data);
        let err = decode(&mut reader, false).unwrap_err();
        assert!(matches!(err, Error::InvalidDistinguishingBits(1)));
    }

    /// Decode zu kurzer Stream: Version mit Fortsetzung truncated
    #[test]
    fn decode_truncated_version_continuation() {
        // Header mit Version 16 braucht 2 4-Bit-Chunks (1111 + 0000)
        // Wir schreiben nur den ersten Teil
        let mut writer = BitWriter::new();
        writer.write_bits(0b10, 2); // Distinguishing Bits
        writer.write_bit(false); // Presence Bit
        writer.write_bit(false); // Preview Bit
        writer.write_bits(0b1111, 4); // Version chunk 15 (Fortsetzung erwartet)
        // Hier fehlt der zweite Chunk!
        let data = writer.into_vec();

        let mut reader = BitReader::new(&data);
        let err = decode(&mut reader, false).unwrap_err();
        assert_eq!(err, Error::PrematureEndOfStream);
    }

    /// Version 0 wird vom Builder auf 1 normalisiert (NonZeroU16)
    #[test]
    fn version_zero_normalized_to_one() {
        let header = ExiHeader::new().with_version(0);
        assert_eq!(header.version(), 1);
    }

    // === Bit-Level Encoding Verification ===

    /// Spec 5.3: Verify bit encoding of versions 1, 15, 16
    #[test]
    fn version_bit_encoding() {
        // Version 1: Dist(10) + Presence(0) + Preview(0) + Version(0000) = 0x80
        let data = encode_to_vec(&ExiHeader::new(), false);
        assert_eq!(data[0], 0x80);

        // Version 15: Dist(10) + Presence(0) + Preview(0) + Version(1110) = 0x8E
        let data = encode_to_vec(&ExiHeader::new().with_version(15), false);
        assert_eq!(data[0], 0x8E);

        // Version 16: Dist(10) + Presence(0) + Preview(0) + Version(1111 0000)
        let data = encode_to_vec(&ExiHeader::new().with_version(16), false);
        assert_eq!(data[0], 0x8F);
        assert_eq!(data[1] & 0xF0, 0x00);
    }

    /// Mit Cookie und Options
    #[test]
    fn cookie_with_options_encoding() {
        let header = ExiHeader::new().with_cookie().with_options();
        let data = encode_to_vec(&header, false);
        // Cookie: $EXI
        assert_eq!(&data[0..4], b"$EXI");
        // Distinguishing Bits (10) + Presence (1) + Preview (0) + Version (0000)
        // = 1010 0000 = 0xA0
        assert_eq!(data[4], 0xA0);
    }
}
