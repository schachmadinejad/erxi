//! Zentrale Bitbreiten-Berechnung (Spec 6.2, 7.2, 7.3).
//!
//! Berechnet `⌈log₂(n)⌉` — die Anzahl Bits um `n` unterschiedliche Werte
//! zu codieren. Wird von Event Codes (6.2), Enumerations (7.2) und
//! String Tables (7.3) verwendet.

/// Berechnet die Anzahl Bits fuer `n` unterschiedliche Werte: `⌈log₂(n)⌉`.
///
/// - `n = 0` oder `n = 1`: 0 Bits (kein Bit noetig)
/// - `n = 2`: 1 Bit
/// - `n = 3..4`: 2 Bits
/// - `n = 5..8`: 3 Bits
/// - usw.
///
/// # Spec-Referenz
/// - 6.2 Representing Event Codes
/// - 7.2 Enumeration
/// - 7.3 String Table
#[inline]
pub fn for_count(n: usize) -> u8 {
    if n <= 1 {
        0
    } else {
        (usize::BITS - (n - 1).leading_zeros()) as u8
    }
}

/// Wie [`for_count`], aber fuer `u32`-Eingabe (Event Codes, Spec 6.2).
#[inline]
pub fn for_count_u32(n: u32) -> u8 {
    if n <= 1 {
        0
    } else {
        (u32::BITS - (n - 1).leading_zeros()) as u8
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Spec 6.2, 7.2: ceil(log2(n))
    #[test]
    fn grundwerte() {
        assert_eq!(for_count(0), 0);
        assert_eq!(for_count(1), 0);
        assert_eq!(for_count(2), 1);
        assert_eq!(for_count(3), 2);
        assert_eq!(for_count(4), 2);
        assert_eq!(for_count(5), 3);
        assert_eq!(for_count(8), 3);
        assert_eq!(for_count(9), 4);
        assert_eq!(for_count(16), 4);
        assert_eq!(for_count(17), 5);
        assert_eq!(for_count(256), 8);
        assert_eq!(for_count(257), 9);
    }

    #[test]
    fn for_count_u32_konsistent() {
        for n in 0..=1000u32 {
            assert_eq!(for_count_u32(n), for_count(n as usize));
        }
    }
}
