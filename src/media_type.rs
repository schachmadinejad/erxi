//! Content Coding und Internet Media Type (Spec Appendix F).
//!
//! Definiert die standardisierten Kennungen fuer EXI:
//! - Content Coding: "exi" (Spec F.1)
//! - Media Type: "application/exi" (Spec F.2)
//! - Magic Number: 0x24 0x45 0x58 0x49 = "$EXI" (Spec F.2)
//! - Dateiendung: ".exi" (Spec F.2)

/// Content Coding Label fuer EXI (Spec F.1).
///
/// > Protocols [...] SHOULD use the content coding "exi" (case-insensitive)
/// > to convey the acceptance or actual use of EXI encoding for XML information.
pub const CONTENT_CODING: &str = "exi";

/// Internet Media Type fuer EXI (Spec F.2).
///
/// > A new media type registration "application/exi" [...]
pub const MEDIA_TYPE: &str = "application/exi";

/// Magic Number: die ersten vier Oktette eines EXI-Streams mit Cookie (Spec F.2).
///
/// > The first four octets may be hexadecimal 24 45 58 49 ("$EXI").
pub const MAGIC_NUMBER: [u8; 4] = [0x24, 0x45, 0x58, 0x49];

/// Dateiendung fuer EXI-Dateien (Spec F.2).
pub const FILE_EXTENSION: &str = ".exi";

/// Prueft, ob ein Byte-Slice mit dem EXI Magic Number beginnt (Spec F.2).
///
/// > The first four octets may be hexadecimal 24 45 58 49 ("$EXI").
pub fn has_magic_number(data: &[u8]) -> bool {
    data.len() >= 4 && data[..4] == MAGIC_NUMBER
}

#[cfg(test)]
mod tests {
    use super::*;

    // Spec F.1: Content Coding "exi"
    #[test]
    fn content_coding_ist_exi() {
        assert_eq!(CONTENT_CODING, "exi");
    }

    // Spec F.2: Media Type "application/exi"
    #[test]
    fn media_type_ist_application_exi() {
        assert_eq!(MEDIA_TYPE, "application/exi");
    }

    // Spec F.2: Magic Number 0x24 0x45 0x58 0x49
    #[test]
    fn magic_number_ist_dollar_exi() {
        assert_eq!(MAGIC_NUMBER, [0x24, 0x45, 0x58, 0x49]);
        assert_eq!(&MAGIC_NUMBER, b"$EXI");
    }

    // Spec F.2: Dateiendung ".exi"
    #[test]
    fn dateiendung_ist_dot_exi() {
        assert_eq!(FILE_EXTENSION, ".exi");
    }

    // Spec F.2: Magic Number Detection
    #[test]
    fn has_magic_number_erkennt_exi_cookie() {
        // Mit Cookie: $EXI + Header
        assert!(has_magic_number(&[0x24, 0x45, 0x58, 0x49, 0x80]));
        // Exakt 4 Bytes
        assert!(has_magic_number(&[0x24, 0x45, 0x58, 0x49]));
    }

    #[test]
    fn has_magic_number_ohne_cookie() {
        // Ohne Cookie: Header beginnt direkt mit Distinguishing Bits
        assert!(!has_magic_number(&[0x80]));
        // Zu kurz
        assert!(!has_magic_number(&[0x24, 0x45, 0x58]));
        // Leer
        assert!(!has_magic_number(&[]));
    }

    #[test]
    fn has_magic_number_falsche_bytes() {
        assert!(!has_magic_number(&[0x00, 0x00, 0x00, 0x00]));
        assert!(!has_magic_number(&[0x24, 0x45, 0x58, 0x00]));
    }

    // Konsistenz: Header mit Cookie muss mit Magic Number beginnen
    #[test]
    fn magic_number_konsistent_mit_header() {
        let header = crate::header::ExiHeader::default().with_cookie();
        let mut writer = crate::bitstream::BitWriter::new();
        crate::header::encode(&mut writer, &header, false).unwrap();
        let bytes = writer.into_vec();
        assert!(has_magic_number(&bytes));
    }
}
