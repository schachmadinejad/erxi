//! Event Code Berechnung und Encoding (Spec 6.1, 6.2, 8.2)
//!
//! Event Codes identifizieren Events in EXI Streams und bestehen aus 1-3 Teilen.
//! Jeder Teil wird als n-bit unsigned integer encodiert, wobei n von der Anzahl
//! der möglichen Werte (Geschwister) abhängt.
//!
//! # Spec 6.2: Sibling-basierte Bitbreiten
//!
//! Die Bitbreite für Part i hängt von den Geschwister-Event-Codes ab:
//! - Part 1: Alle Event Codes sind Geschwister
//! - Part 2: Nur Event Codes mit gleichem Part1-Wert sind Geschwister
//! - Part 3: Nur Event Codes mit gleichen Part1 und Part2-Werten sind Geschwister

use crate::Result;
use crate::bitstream::{BitReader, BitWriter};
use crate::n_bit_unsigned_integer;

/// Ein Event Code mit 1-3 Teilen.
///
/// Event Codes werden in EXI Grammars verwendet, um Productions zu identifizieren.
/// Die Teile werden hierarchisch interpretiert (z.B. "1.3.0" = Teil1=1, Teil2=3, Teil3=0).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EventCode {
    /// Erster Teil (immer vorhanden)
    part1: u32,
    /// Zweiter Teil (optional)
    part2: Option<u32>,
    /// Dritter Teil (optional)
    part3: Option<u32>,
}

impl EventCode {
    /// Erstellt einen Event Code mit einem Teil.
    pub fn one(part1: u32) -> Self {
        Self {
            part1,
            part2: None,
            part3: None,
        }
    }

    /// Erstellt einen Event Code mit zwei Teilen.
    pub fn two(part1: u32, part2: u32) -> Self {
        Self {
            part1,
            part2: Some(part2),
            part3: None,
        }
    }

    /// Erstellt einen Event Code mit drei Teilen.
    pub fn three(part1: u32, part2: u32, part3: u32) -> Self {
        Self {
            part1,
            part2: Some(part2),
            part3: Some(part3),
        }
    }

    /// Gibt die Anzahl der Teile zurück.
    pub fn num_parts(&self) -> usize {
        1 + self.part2.is_some() as usize + self.part3.is_some() as usize
    }

    /// Gibt die Länge des Event Codes zurück (Anzahl der Teile).
    ///
    /// Alias für `num_parts()`. Die Spec verwendet "event code of length N"
    /// um Event Codes mit N Teilen zu beschreiben.
    ///
    /// # Spec-Referenz
    ///
    /// - 8.4.3: "...with an event code of length 1..."
    #[inline]
    pub fn length(&self) -> usize {
        self.num_parts()
    }

    /// Gibt den ersten Teil zurück.
    pub fn part1(&self) -> u32 {
        self.part1
    }

    /// Gibt den zweiten Teil zurück (falls vorhanden).
    pub fn part2(&self) -> Option<u32> {
        self.part2
    }

    /// Gibt den dritten Teil zurück (falls vorhanden).
    pub fn part3(&self) -> Option<u32> {
        self.part3
    }

    /// Inkrementiert den ersten Teil des Event Codes.
    ///
    /// Wird bei Grammar Evolution verwendet:
    /// - Fragment Grammar (Spec 8.4.2): Wenn SE(qname) gelernt wird
    /// - Element Grammar (Spec 8.4.3): Wenn AT(qname), CH oder EE gelernt wird
    ///
    /// Alle bestehenden Event Codes werden inkrementiert wenn ein neues
    /// Element/Attribut gelernt wird.
    ///
    /// # Beispiele
    ///
    /// - `[n]` → `[n+1]`
    /// - `[n.m]` → `[n+1.m]`
    /// - `[n.m.o]` → `[n+1.m.o]`
    ///
    /// # Spec-Referenz
    ///
    /// - 8.4.2 Built-in Fragment Grammar, SE(*)-Semantik
    /// - 8.4.3 Built-in Element Grammar, AT(*)/CH/EE-Semantik
    pub fn increment_first_part(&self) -> Self {
        Self {
            part1: self.part1 + 1,
            part2: self.part2,
            part3: self.part3,
        }
    }

    /// Gibt einen neuen EventCode mit Part1 + Offset zurück.
    ///
    /// Wird bei Offset-basiertem Prepend verwendet: Gelernte Productions
    /// verschieben die effektiven Event Codes der Original-Productions.
    #[inline]
    pub fn with_part1_offset(&self, offset: u32) -> Self {
        Self {
            part1: self.part1 + offset,
            part2: self.part2,
            part3: self.part3,
        }
    }

    /// Gibt einen neuen EventCode mit Part1 - delta zurück, oder None bei Unterlauf.
    ///
    /// Inverse von `with_part1_offset`: Für Event-Code-Lookup subtrahiert
    /// man den Offset, um den gespeicherten Code zu finden.
    #[inline]
    pub fn with_part1_decremented(&self, delta: u32) -> Option<Self> {
        self.part1.checked_sub(delta).map(|p1| Self {
            part1: p1,
            part2: self.part2,
            part3: self.part3,
        })
    }
}

/// Berechnet die Anzahl der Bits für einen Event Code Teil.
///
/// Gemäß Spec 6.2: n = ⌈log₂(m)⌉, wobei m die Anzahl der distinct values ist.
/// Spezialfall: Wenn m ≤ 1, werden 0 Bits benötigt (Teil wird weggelassen).
///
/// # Spec-Referenz
/// - 6.2 Representing Event Codes
pub fn bits_for_part(num_distinct_values: u32) -> u8 {
    crate::bit_width::for_count_u32(num_distinct_values)
}

/// Berechnet die Anzahl der Bytes für einen Event Code Teil (byte-aligned).
///
/// Gemäß Spec 6.2, Table 6-2: ⌈(log₂(m)) / 8⌉ Bytes pro Teil.
/// Minimum 1 Byte wenn n > 0, sonst 0 Bytes.
///
/// # Spec-Referenz
/// - 6.2 Representing Event Codes, Table 6-2
pub fn bytes_for_part(num_distinct_values: u32) -> u8 {
    let bits = bits_for_part(num_distinct_values);
    if bits == 0 { 0 } else { bits.div_ceil(8) }
}

/// Kontext für Event Code Encoding/Decoding (Spec 6.2).
///
/// Enthält die Anzahl der distinct values für jeden Teil, wobei Part2 und Part3
/// per Sibling-Gruppe gezählt werden (nicht global).
///
/// # Spec 6.2
///
/// "Two event codes are siblings at the i-th part if and only if they share
/// the same values in all preceding parts."
///
/// Das bedeutet:
/// - Part 1: Alle Event Codes sind Geschwister → eine globale Anzahl
/// - Part 2: Pro Part1-Wert eine separate Anzahl
/// - Part 3: Pro (Part1, Part2)-Kombination eine separate Anzahl
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EventCodeContext {
    /// Anzahl distinct values für Teil 1 (alle Event Codes sind Geschwister)
    num_values_part1: u32,
    /// Anzahl distinct values für Teil 2, gruppiert nach Part1-Wert.
    /// Flacher Vec statt BTreeMap — typisch 0-3 Einträge,
    /// linearer Scan schneller als B-Tree, Clone billiger als BTreeMap.
    part2_counts: Vec<(u32, u32)>,
    /// Anzahl distinct values für Teil 3, gruppiert nach (Part1, Part2).
    /// Flacher Vec statt BTreeMap — typisch 0-2 Einträge.
    part3_counts: Vec<((u32, u32), u32)>,
    /// Ob ein 2nd Level (Tier 2) existiert.
    ///
    /// Bei Schema-informed Grammars werden Undeclared Productions dynamisch
    /// behandelt statt statisch in der Grammar. Wenn `has_second_level=true`,
    /// wird Part1 mit einem zusätzlichen "escape"-Wert encodiert:
    /// - Part1 < num_values_part1: 1st Level (Schema-declared)
    /// - Part1 == num_values_part1: "escape" zu 2nd Level (Undeclared)
    ///
    /// Bei Built-in Grammars ist `has_second_level=false`, da alle Productions
    /// statisch in der Grammar sind.
    has_second_level: bool,
    /// Ob EndElement (EE) in 1st Level vorhanden ist.
    ///
    /// Nur relevant wenn `has_second_level=true`. Verhindert doppeltes EE
    /// im 2nd Level (Exificient-Kompatibilität).
    has_end_element_in_first_level: bool,
}

/// Sucht einen Wert in einem flachen Vec<(K, V)> per linearem Scan.
/// Für 1-4 Einträge schneller als HashMap/BTreeMap.
fn flat_vec_get<'a, K: Eq, V>(vec: &'a [(K, V)], key: &K) -> Option<&'a V> {
    vec.iter().find(|(k, _)| k == key).map(|(_, v)| v)
}

/// Fügt einen Wert in einen flachen Vec<(K, V)> ein oder aktualisiert ihn.
fn flat_vec_insert<K: Eq, V>(vec: &mut Vec<(K, V)>, key: K, value: V) {
    if let Some(entry) = vec.iter_mut().find(|(k, _)| *k == key) {
        entry.1 = value;
    } else {
        vec.push((key, value));
    }
}

impl EventCodeContext {
    /// Erstellt einen Kontext für 1-Teil Event Codes.
    ///
    /// Für Built-in Grammars (kein 2-Tier System).
    pub fn one(num_values: u32) -> Self {
        Self {
            num_values_part1: num_values,
            part2_counts: Vec::new(),
            part3_counts: Vec::new(),
            has_second_level: false,
            has_end_element_in_first_level: false,
        }
    }

    /// Erstellt einen Kontext für 2-Teil Event Codes (alle mit gleichem Part1).
    ///
    /// Convenience-Konstruktor für Built-in Grammars, wo alle 2-teil Codes
    /// denselben Part1-Wert haben. Der Part1-Wert wird als `num_values_part1 - 1`
    /// angenommen (der letzte 1-teil Code + 1).
    pub fn two(num_values_part1: u32, num_values_part2: u32) -> Self {
        let part2_counts = if num_values_part2 > 0 && num_values_part1 > 0 {
            vec![(num_values_part1 - 1, num_values_part2)]
        } else {
            Vec::new()
        };
        Self {
            num_values_part1,
            part2_counts,
            part3_counts: Vec::new(),
            has_second_level: false,
            has_end_element_in_first_level: false,
        }
    }

    /// Erstellt einen Kontext für 3-Teil Event Codes (alle mit gleichem Part1, Part2).
    ///
    /// Convenience-Konstruktor für Built-in Grammars. Siehe `two()` für Details.
    pub fn three(num_values_part1: u32, num_values_part2: u32, num_values_part3: u32) -> Self {
        let mut ctx = Self::two(num_values_part1, num_values_part2);
        if num_values_part3 > 0 && num_values_part1 > 0 && num_values_part2 > 0 {
            ctx.part3_counts.push((
                (num_values_part1 - 1, num_values_part2 - 1),
                num_values_part3,
            ));
        }
        ctx
    }

    /// Erstellt einen leeren Kontext (für Builder-Pattern).
    ///
    /// Für Built-in Grammars (kein 2-Tier System).
    pub fn empty() -> Self {
        Self {
            num_values_part1: 0,
            part2_counts: Vec::new(),
            part3_counts: Vec::new(),
            has_second_level: false,
            has_end_element_in_first_level: false,
        }
    }

    /// Setzt die Anzahl Part1-Werte.
    pub fn with_part1_count(mut self, count: u32) -> Self {
        self.num_values_part1 = count;
        self
    }

    /// Fügt einen Part2-Count für einen bestimmten Part1-Wert hinzu.
    pub fn with_part2_count(mut self, part1: u32, count: u32) -> Self {
        if count > 0 {
            flat_vec_insert(&mut self.part2_counts, part1, count);
        }
        self
    }

    /// Fügt einen Part3-Count für eine bestimmte (Part1, Part2)-Kombination hinzu.
    pub fn with_part3_count(mut self, part1: u32, part2: u32, count: u32) -> Self {
        if count > 0 {
            flat_vec_insert(&mut self.part3_counts, (part1, part2), count);
        }
        self
    }

    /// Aktiviert das 2-Tier System (Schema-informed Grammars).
    ///
    /// Bei `has_second_level=true` wird Part1 mit einem zusätzlichen
    /// "escape"-Wert encodiert um zu Tier 2 (Undeclared Productions) zu wechseln.
    pub fn with_second_level(mut self, has_second_level: bool) -> Self {
        self.has_second_level = has_second_level;
        self
    }

    /// Setzt ob EndElement (EE) in Tier 1 vorhanden ist.
    ///
    /// Relevant für Tier 2 EE-Überspringung (verhindert doppeltes EE).
    pub fn with_end_element_in_first_level(mut self, has_ee: bool) -> Self {
        self.has_end_element_in_first_level = has_ee;
        self
    }

    /// Gibt die Anzahl distinct values für Teil 1 zurück.
    pub fn num_values_part1(&self) -> u32 {
        self.num_values_part1
    }

    /// Prüft ob das 2-Tier System aktiv ist.
    ///
    /// Bei `true` wird Part1 mit +1 für "escape" zu Tier 2 encodiert.
    pub fn has_second_level(&self) -> bool {
        self.has_second_level
    }

    /// Prüft ob EndElement (EE) in Tier 1 vorhanden ist.
    pub fn has_end_element_in_first_level(&self) -> bool {
        self.has_end_element_in_first_level
    }

    /// Gibt die Anzahl distinct Part2-Werte für einen bestimmten Part1-Wert zurück.
    ///
    /// Gibt 0 zurück wenn keine 2-teil Codes mit diesem Part1 existieren.
    pub fn num_values_part2_for(&self, part1: u32) -> u32 {
        flat_vec_get(&self.part2_counts, &part1).copied().unwrap_or(0)
    }

    /// Gibt die Anzahl distinct Part3-Werte für eine (Part1, Part2)-Kombination zurück.
    ///
    /// Gibt 0 zurück wenn keine 3-teil Codes mit dieser Kombination existieren.
    pub fn num_values_part3_for(&self, part1: u32, part2: u32) -> u32 {
        flat_vec_get(&self.part3_counts, &(part1, part2)).copied().unwrap_or(0)
    }

    /// Gibt das globale Maximum der Part2-Werte zurück.
    ///
    /// Für Abwärtskompatibilität mit Code der das alte API verwendet.
    /// Bei Built-in Grammars entspricht dies dem einzigen Part2-Count.
    pub fn max_num_values_part2(&self) -> u32 {
        self.part2_counts.iter().map(|(_, v)| *v).max().unwrap_or(0)
    }

    /// Gibt das globale Maximum der Part3-Werte zurück.
    ///
    /// Für Abwärtskompatibilität mit Code der das alte API verwendet.
    /// Bei Built-in Grammars entspricht dies dem einzigen Part3-Count.
    pub fn max_num_values_part3(&self) -> u32 {
        self.part3_counts.iter().map(|(_, v)| *v).max().unwrap_or(0)
    }

    /// Prüft ob 2-teil Codes für einen bestimmten Part1-Wert existieren.
    pub fn has_part2(&self, part1: u32) -> bool {
        self.num_values_part2_for(part1) > 0
    }

    /// Prüft ob 3-teil Codes für eine (Part1, Part2)-Kombination existieren.
    pub fn has_part3(&self, part1: u32, part2: u32) -> bool {
        self.num_values_part3_for(part1, part2) > 0
    }

    /// Gibt die effektive Anzahl Part1-Werte zurück (inkl. Escape bei 2nd Level).
    ///
    /// Bei `has_second_level=true` wird +1 für den "escape" zu Tier 2 berücksichtigt.
    pub fn effective_part1_count(&self) -> u32 {
        if self.has_second_level {
            self.num_values_part1.saturating_add(1)
        } else {
            self.num_values_part1
        }
    }

    /// Inkrementiert num_values_part1 um 1 und verschiebt Part2/Part3-Keys.
    ///
    /// Wird bei `prepend_learned_production()` verwendet: Jede gelernte
    /// Production erhöht die Anzahl der Part1-Werte um 1. Da die gelernte
    /// Production Part1=0 bekommt, rücken alle bestehenden Part1-Werte um 1
    /// nach oben. Die Part2/Part3-Maps müssen entsprechend angepasst werden.
    ///
    /// Komplexität: O(k) wobei k = Anzahl Einträge in part2_counts/part3_counts
    /// (typisch 1-3, nie mehr als ~10).
    pub fn increment_part1_count(&mut self) {
        self.num_values_part1 += 1;
        // Part2-Keys um 1 verschieben (Part1-Werte rücken alle nach oben).
        // In-place statt take+rebuild (alle Keys werden um 1 erhöht).
        for entry in &mut self.part2_counts {
            entry.0 += 1;
        }
        // Part3-Keys um 1 verschieben (Part1-Komponente der (Part1, Part2)-Keys).
        for entry in &mut self.part3_counts {
            (entry.0).0 += 1;
        }
    }

    /// Gibt die Bit-Breite für Part1 zurück.
    ///
    /// Bei `has_second_level=true` wird +1 für den "escape" zu Tier 2 berücksichtigt.
    /// Dies entspricht Exificient's `get1stLevelEventCodeLength()`:
    /// `getCodingLength(grammar.getNumberOfEvents() + (has2ndLevel ? 1 : 0))`
    pub fn bits_for_part1(&self) -> u8 {
        bits_for_part(self.effective_part1_count())
    }

    /// Gibt die Bit-Breite für Part2 bei gegebenem Part1-Wert zurück.
    pub fn bits_for_part2(&self, part1: u32) -> u8 {
        bits_for_part(self.num_values_part2_for(part1))
    }

    /// Gibt die Bit-Breite für Part3 bei gegebenen Part1/Part2-Werten zurück.
    pub fn bits_for_part3(&self, part1: u32, part2: u32) -> u8 {
        bits_for_part(self.num_values_part3_for(part1, part2))
    }

    /// Gibt die Byte-Breite für Part1 zurück (byte-aligned).
    ///
    /// Bei `has_second_level=true` wird +1 für den "escape" zu Tier 2 berücksichtigt.
    pub fn bytes_for_part1(&self) -> u8 {
        bytes_for_part(self.effective_part1_count())
    }

    /// Gibt die Byte-Breite für Part2 bei gegebenem Part1-Wert zurück (byte-aligned).
    pub fn bytes_for_part2(&self, part1: u32) -> u8 {
        bytes_for_part(self.num_values_part2_for(part1))
    }

    /// Gibt die Byte-Breite für Part3 bei gegebenen Part1/Part2-Werten zurück (byte-aligned).
    pub fn bytes_for_part3(&self, part1: u32, part2: u32) -> u8 {
        bytes_for_part(self.num_values_part3_for(part1, part2))
    }
}

/// Encodiert einen Event Code im bit-packed Format.
///
/// Validiert dass alle Teile im gültigen Bereich liegen (Spec 6.2).
///
/// # Spec-Referenz
/// - 6.2 Representing Event Codes, Table 6-1
#[inline]
pub fn encode_bit_packed(
    writer: &mut BitWriter,
    event_code: &EventCode,
    context: &EventCodeContext,
) -> Result<()> {
    let part1 = event_code.part1();
    validate_part1(part1, context)?;

    let bits1 = context.bits_for_part1();
    encode_bit_packed_part(writer, part1, bits1);

    if let Some(part2) = event_code.part2() {
        validate_part2(part1, part2, context)?;

        let bits2 = context.bits_for_part2(part1);
        encode_bit_packed_part(writer, part2, bits2);

        if let Some(part3) = event_code.part3() {
            validate_part3(part1, part2, part3, context)?;

            let bits3 = context.bits_for_part3(part1, part2);
            encode_bit_packed_part(writer, part3, bits3);
        }
    }
    Ok(())
}

/// Validiert Part1 gegen den Context (Spec 6.2).
///
/// Bei `has_second_level`: Part1 darf 0..=num_values_part1 sein (Escape-Wert inklusive).
/// Ohne `has_second_level`: Part1 darf 0..num_values_part1 sein.
fn validate_part1(part1: u32, context: &EventCodeContext) -> Result<()> {
    let max = context.effective_part1_count();
    if max == 0 || part1 < max {
        Ok(())
    } else {
        Err(crate::Error::invalid_event_code(
            part1.to_string(),
            format!("part1 >= {max}"),
        ))
    }
}

/// Validiert Part2 gegen den Context (Spec 6.2).
fn validate_part2(part1: u32, part2: u32, context: &EventCodeContext) -> Result<()> {
    let max = context.num_values_part2_for(part1);
    if max == 0 || part2 < max {
        Ok(())
    } else {
        Err(crate::Error::invalid_event_code(
            format!("{part1}.{part2}"),
            format!("part2 >= {max}"),
        ))
    }
}

/// Validiert Part3 gegen den Context (Spec 6.2).
fn validate_part3(part1: u32, part2: u32, part3: u32, context: &EventCodeContext) -> Result<()> {
    let max = context.num_values_part3_for(part1, part2);
    if max == 0 || part3 < max {
        Ok(())
    } else {
        Err(crate::Error::invalid_event_code(
            format!("{part1}.{part2}.{part3}"),
            format!("part3 >= {max}"),
        ))
    }
}

/// Hilfsfunktion: Encodiert einen Teil im bit-packed Format.
#[inline]
fn encode_bit_packed_part(writer: &mut BitWriter, value: u32, bits: u8) {
    if bits > 0 {
        n_bit_unsigned_integer::encode(writer, value as u64, bits);
    }
}

/// Encodiert einen Event Code im byte-aligned Format.
///
/// Validiert dass alle Teile im gültigen Bereich liegen (Spec 6.2).
///
/// # Spec-Referenz
/// - 6.2 Representing Event Codes, Table 6-2
pub fn encode_byte_aligned(
    writer: &mut BitWriter,
    event_code: &EventCode,
    context: &EventCodeContext,
) -> Result<()> {
    let part1 = event_code.part1();
    validate_part1(part1, context)?;

    let bytes1 = context.bytes_for_part1();
    encode_byte_aligned_part(writer, part1, bytes1);

    if let Some(part2) = event_code.part2() {
        validate_part2(part1, part2, context)?;

        let bytes2 = context.bytes_for_part2(part1);
        encode_byte_aligned_part(writer, part2, bytes2);

        if let Some(part3) = event_code.part3() {
            validate_part3(part1, part2, part3, context)?;

            let bytes3 = context.bytes_for_part3(part1, part2);
            encode_byte_aligned_part(writer, part3, bytes3);
        }
    }
    Ok(())
}

/// Hilfsfunktion: Encodiert einen Wert als ganze Bytes (Little-Endian).
///
/// Spec 7.1.9 (n-bit Unsigned Integer): "Bytes are ordered with the least
/// significant byte first." Event Codes werden als n-bit unsigned integers
/// encodiert (Spec 6.2), daher gilt Little-Endian für byte-aligned.
/// Bei num_bytes == 0 wird nichts geschrieben (0-Byte Omission).
fn encode_byte_aligned_part(writer: &mut BitWriter, value: u32, num_bytes: u8) {
    for i in 0..num_bytes {
        let byte = ((value >> (i * 8)) & 0xFF) as u8;
        writer.write_byte_aligned(byte);
    }
}

/// Decodiert einen Event Code im bit-packed Format.
///
/// Die Bitbreiten werden per Sibling-Gruppe berechnet (Spec 6.2):
/// - Part1 wird mit der globalen Bitbreite gelesen
/// - Part2 Bitbreite hängt vom gelesenen Part1-Wert ab
/// - Part3 Bitbreite hängt von Part1 und Part2 ab
///
/// # Spec-Referenz
/// - 6.2 Representing Event Codes, Table 6-1
pub fn decode_bit_packed(reader: &mut BitReader, context: &EventCodeContext) -> Result<EventCode> {
    // Part1 lesen und validieren
    let bits1 = context.bits_for_part1();
    let part1 = decode_bit_packed_part(reader, bits1)?;
    validate_part1(part1, context)?;

    // Part2 lesen (falls vorhanden für diesen Part1-Wert)
    let num_part2 = context.num_values_part2_for(part1);
    let part2 = if num_part2 > 0 {
        let bits2 = context.bits_for_part2(part1);
        let p2 = decode_bit_packed_part(reader, bits2)?;
        validate_part2(part1, p2, context)?;
        Some(p2)
    } else {
        None
    };

    // Part3 lesen (falls vorhanden für diese Part1/Part2-Kombination)
    let part3 = if let Some(p2) = part2 {
        let num_part3 = context.num_values_part3_for(part1, p2);
        if num_part3 > 0 {
            let bits3 = context.bits_for_part3(part1, p2);
            let p3 = decode_bit_packed_part(reader, bits3)?;
            validate_part3(part1, p2, p3, context)?;
            Some(p3)
        } else {
            None
        }
    } else {
        None
    };

    Ok(EventCode {
        part1,
        part2,
        part3,
    })
}

/// Hilfsfunktion: Decodiert einen Teil im bit-packed Format.
fn decode_bit_packed_part(reader: &mut BitReader, bits: u8) -> Result<u32> {
    if bits > 0 {
        Ok(n_bit_unsigned_integer::decode(reader, bits)? as u32)
    } else {
        Ok(0)
    }
}

/// Decodiert einen Event Code im byte-aligned Format.
///
/// Die Byte-Breiten werden per Sibling-Gruppe berechnet (Spec 6.2):
/// - Part1 wird mit der globalen Byte-Breite gelesen
/// - Part2 Byte-Breite hängt vom gelesenen Part1-Wert ab
/// - Part3 Byte-Breite hängt von Part1 und Part2 ab
///
/// # Spec-Referenz
/// - 6.2 Representing Event Codes, Table 6-2
pub fn decode_byte_aligned(
    reader: &mut BitReader,
    context: &EventCodeContext,
) -> Result<EventCode> {
    // Part1 lesen und validieren
    let bytes1 = context.bytes_for_part1();
    let part1 = decode_byte_aligned_part(reader, bytes1)?;
    validate_part1(part1, context)?;

    // Part2 lesen (falls vorhanden für diesen Part1-Wert)
    let num_part2 = context.num_values_part2_for(part1);
    let part2 = if num_part2 > 0 {
        let bytes2 = context.bytes_for_part2(part1);
        let p2 = decode_byte_aligned_part(reader, bytes2)?;
        validate_part2(part1, p2, context)?;
        Some(p2)
    } else {
        None
    };

    // Part3 lesen (falls vorhanden für diese Part1/Part2-Kombination)
    let part3 = if let Some(p2) = part2 {
        let num_part3 = context.num_values_part3_for(part1, p2);
        if num_part3 > 0 {
            let bytes3 = context.bytes_for_part3(part1, p2);
            let p3 = decode_byte_aligned_part(reader, bytes3)?;
            validate_part3(part1, p2, p3, context)?;
            Some(p3)
        } else {
            None
        }
    } else {
        None
    };

    Ok(EventCode {
        part1,
        part2,
        part3,
    })
}

/// Hilfsfunktion: Decodiert einen Wert aus ganzen Bytes (Little-Endian).
///
/// Spec 7.1.9: "Bytes are ordered with the least significant byte first."
/// Bei num_bytes == 0 wird 0 zurückgegeben (0-Byte Omission).
fn decode_byte_aligned_part(reader: &mut BitReader, num_bytes: u8) -> Result<u32> {
    let mut value = 0u32;
    for i in 0..num_bytes {
        let byte = reader.read_byte_aligned()?;
        value |= (byte as u32) << (i * 8);
    }
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // Tests für bits_for_part (Spec 6.2)
    // ========================================================================

    #[test]
    fn bits_for_part_zero_values() {
        // Spec 6.2: Wenn nur 1 distinct value, 0 Bits
        assert_eq!(bits_for_part(0), 0);
    }

    #[test]
    fn bits_for_part_one_value() {
        // Spec 6.2: Wenn nur 1 distinct value, 0 Bits (Teil wird weggelassen)
        assert_eq!(bits_for_part(1), 0);
    }

    #[test]
    fn bits_for_part_two_values() {
        // ⌈log₂(2)⌉ = 1
        assert_eq!(bits_for_part(2), 1);
    }

    #[test]
    fn bits_for_part_three_values() {
        // ⌈log₂(3)⌉ = 2
        assert_eq!(bits_for_part(3), 2);
    }

    #[test]
    fn bits_for_part_four_values() {
        // ⌈log₂(4)⌉ = 2
        assert_eq!(bits_for_part(4), 2);
    }

    #[test]
    fn bits_for_part_five_values() {
        // ⌈log₂(5)⌉ = 3
        assert_eq!(bits_for_part(5), 3);
    }

    #[test]
    fn bits_for_part_six_values() {
        // Spec 6.2 Beispiel: ⌈log₂(6)⌉ = 3
        assert_eq!(bits_for_part(6), 3);
    }

    #[test]
    fn bits_for_part_eight_values() {
        // ⌈log₂(8)⌉ = 3
        assert_eq!(bits_for_part(8), 3);
    }

    #[test]
    fn bits_for_part_nine_values() {
        // ⌈log₂(9)⌉ = 4
        assert_eq!(bits_for_part(9), 4);
    }

    #[test]
    fn bits_for_part_256_values() {
        // ⌈log₂(256)⌉ = 8
        assert_eq!(bits_for_part(256), 8);
    }

    #[test]
    fn bits_for_part_257_values() {
        // ⌈log₂(257)⌉ = 9
        assert_eq!(bits_for_part(257), 9);
    }

    // ========================================================================
    // Tests für bytes_for_part (Spec 6.2, Table 6-2)
    // ========================================================================

    #[test]
    fn bytes_for_part_zero_values() {
        assert_eq!(bytes_for_part(0), 0);
    }

    #[test]
    fn bytes_for_part_one_value() {
        assert_eq!(bytes_for_part(1), 0);
    }

    #[test]
    fn bytes_for_part_two_values() {
        // 1 Bit → 1 Byte
        assert_eq!(bytes_for_part(2), 1);
    }

    #[test]
    fn bytes_for_part_six_values() {
        // Spec 6.2, Table 6-2: 3 Bits → 1 Byte
        assert_eq!(bytes_for_part(6), 1);
    }

    #[test]
    fn bytes_for_part_256_values() {
        // 8 Bits → 1 Byte
        assert_eq!(bytes_for_part(256), 1);
    }

    #[test]
    fn bytes_for_part_257_values() {
        // 9 Bits → 2 Bytes
        assert_eq!(bytes_for_part(257), 2);
    }

    // ========================================================================
    // Tests für EventCode Konstruktoren
    // ========================================================================

    #[test]
    fn event_code_one_part() {
        let ec = EventCode::one(3);
        assert_eq!(ec.part1(), 3);
        assert_eq!(ec.part2(), None);
        assert_eq!(ec.part3(), None);
        assert_eq!(ec.num_parts(), 1);
    }

    #[test]
    fn event_code_two_parts() {
        let ec = EventCode::two(1, 2);
        assert_eq!(ec.part1(), 1);
        assert_eq!(ec.part2(), Some(2));
        assert_eq!(ec.part3(), None);
        assert_eq!(ec.num_parts(), 2);
    }

    #[test]
    fn event_code_three_parts() {
        let ec = EventCode::three(5, 1, 0);
        assert_eq!(ec.part1(), 5);
        assert_eq!(ec.part2(), Some(1));
        assert_eq!(ec.part3(), Some(0));
        assert_eq!(ec.num_parts(), 3);
    }

    // ========================================================================
    // Tests für Spec Table 6-1 (Bit-packed)
    // ========================================================================

    #[test]
    fn table_6_1_se_a_event_code_0() {
        // SE("A") = 0, 3 Bits, encoding: 000
        let context = EventCodeContext::one(6);
        let ec = EventCode::one(0);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 000 + 5 Padding-Bits = 0b00000000 = 0x00
        assert_eq!(buf, vec![0b00000000]);
    }

    #[test]
    fn table_6_1_se_b_event_code_1() {
        // SE("B") = 1, 3 Bits, encoding: 001
        let context = EventCodeContext::one(6);
        let ec = EventCode::one(1);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 001 + 5 Padding = 0b00100000 = 0x20
        assert_eq!(buf, vec![0b00100000]);
    }

    #[test]
    fn table_6_1_se_c_event_code_2() {
        // SE("C") = 2, 3 Bits, encoding: 010
        let context = EventCodeContext::one(6);
        let ec = EventCode::one(2);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 010 + 5 Padding = 0b01000000 = 0x40
        assert_eq!(buf, vec![0b01000000]);
    }

    #[test]
    fn table_6_1_se_d_event_code_3() {
        // SE("D") = 3, 3 Bits, encoding: 011
        let context = EventCodeContext::one(6);
        let ec = EventCode::one(3);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 011 + 5 Padding = 0b01100000 = 0x60
        assert_eq!(buf, vec![0b01100000]);
    }

    #[test]
    fn table_6_1_se_wildcard_event_code_4() {
        // SE(*) = 4, 3 Bits, encoding: 100
        let context = EventCodeContext::one(6);
        let ec = EventCode::one(4);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 100 + 5 Padding = 0b10000000 = 0x80
        assert_eq!(buf, vec![0b10000000]);
    }

    #[test]
    fn table_6_1_dt_event_code_5_0() {
        // DT = 5.0, Part1=5 (3 Bits), Part2=0 (1 Bit), total 4 Bits
        // Context: 6 distinct values für Part1, 2 für Part2
        let context = EventCodeContext::two(6, 2);
        let ec = EventCode::two(5, 0);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 101 0 + 4 Padding = 0b10100000 = 0xA0
        assert_eq!(buf, vec![0b10100000]);
    }

    #[test]
    fn table_6_1_cm_event_code_5_1_0() {
        // CM = 5.1.0, Part1=5 (3 Bits), Part2=1 (1 Bit), Part3=0 (1 Bit), total 5 Bits
        let context = EventCodeContext::three(6, 2, 2);
        let ec = EventCode::three(5, 1, 0);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 101 1 0 + 3 Padding = 0b10110000 = 0xB0
        assert_eq!(buf, vec![0b10110000]);
    }

    #[test]
    fn table_6_1_pi_event_code_5_1_1() {
        // PI = 5.1.1, Part1=5 (3 Bits), Part2=1 (1 Bit), Part3=1 (1 Bit), total 5 Bits
        let context = EventCodeContext::three(6, 2, 2);
        let ec = EventCode::three(5, 1, 1);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 101 1 1 + 3 Padding = 0b10111000 = 0xB8
        assert_eq!(buf, vec![0b10111000]);
    }

    // ========================================================================
    // Tests für Spec Table 6-2 (Byte-aligned)
    // ========================================================================

    #[test]
    fn table_6_2_se_a_event_code_0() {
        // SE("A") = 0, 1 Byte
        let context = EventCodeContext::one(6);
        let ec = EventCode::one(0);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        assert_eq!(buf, vec![0x00]);
    }

    #[test]
    fn table_6_2_se_b_event_code_1() {
        // SE("B") = 1, 1 Byte
        let context = EventCodeContext::one(6);
        let ec = EventCode::one(1);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        assert_eq!(buf, vec![0x01]);
    }

    #[test]
    fn table_6_2_dt_event_code_5_0() {
        // DT = 5.0, 2 Bytes
        let context = EventCodeContext::two(6, 2);
        let ec = EventCode::two(5, 0);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        assert_eq!(buf, vec![0x05, 0x00]);
    }

    #[test]
    fn table_6_2_cm_event_code_5_1_0() {
        // CM = 5.1.0, 3 Bytes
        let context = EventCodeContext::three(6, 2, 2);
        let ec = EventCode::three(5, 1, 0);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        assert_eq!(buf, vec![0x05, 0x01, 0x00]);
    }

    #[test]
    fn table_6_2_pi_event_code_5_1_1() {
        // PI = 5.1.1, 3 Bytes
        let context = EventCodeContext::three(6, 2, 2);
        let ec = EventCode::three(5, 1, 1);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        assert_eq!(buf, vec![0x05, 0x01, 0x01]);
    }

    // ========================================================================
    // Tests für 0-Bit Omission (Spec 6.2)
    // ========================================================================

    #[test]
    fn zero_bit_omission_single_value() {
        // Wenn nur 1 distinct value, 0 Bits
        let context = EventCodeContext::one(1);
        let ec = EventCode::one(0);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // Nichts geschrieben
        assert!(buf.is_empty());
    }

    #[test]
    fn zero_bit_omission_part2() {
        // Part2 hat nur 1 Wert → 0 Bits für Part2
        let context = EventCodeContext::empty()
            .with_part1_count(4)
            .with_part2_count(2, 1); // Part1=2 hat 1 Part2-Wert
        let ec = EventCode::two(2, 0);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // Nur 2 Bits für Part1 (⌈log₂(4)⌉ = 2)
        // 10 + 6 Padding = 0b10000000
        assert_eq!(buf, vec![0b10000000]);
    }

    // ========================================================================
    // Round-Trip Tests (Bit-packed)
    // ========================================================================

    #[test]
    fn round_trip_bit_packed_one_part() {
        let context = EventCodeContext::one(10);
        let original = EventCode::one(7);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &original, &context).unwrap();
        let buf = writer.into_vec();

        let mut reader = BitReader::new(&buf);
        let decoded = decode_bit_packed(&mut reader, &context).unwrap();

        assert_eq!(original, decoded);
    }

    #[test]
    fn round_trip_bit_packed_two_parts() {
        // Context mit Part2-Counts bei Part1=5
        let context = EventCodeContext::empty()
            .with_part1_count(8)
            .with_part2_count(5, 4); // 4 Part2-Werte bei Part1=5

        let original = EventCode::two(5, 3);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &original, &context).unwrap();
        let buf = writer.into_vec();

        let mut reader = BitReader::new(&buf);
        let decoded = decode_bit_packed(&mut reader, &context).unwrap();

        assert_eq!(original, decoded);
    }

    #[test]
    fn round_trip_bit_packed_three_parts() {
        let context = EventCodeContext::three(6, 2, 2);
        let original = EventCode::three(5, 1, 1);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &original, &context).unwrap();
        let buf = writer.into_vec();

        let mut reader = BitReader::new(&buf);
        let decoded = decode_bit_packed(&mut reader, &context).unwrap();

        assert_eq!(original, decoded);
    }

    // ========================================================================
    // Round-Trip Tests (Byte-aligned)
    // ========================================================================

    #[test]
    fn round_trip_byte_aligned_one_part() {
        let context = EventCodeContext::one(10);
        let original = EventCode::one(7);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &original, &context).unwrap();
        let buf = writer.into_vec();

        let mut reader = BitReader::new(&buf);
        let decoded = decode_byte_aligned(&mut reader, &context).unwrap();

        assert_eq!(original, decoded);
    }

    #[test]
    fn round_trip_byte_aligned_two_parts() {
        // Context mit Part2-Counts bei Part1=5
        let context = EventCodeContext::empty()
            .with_part1_count(8)
            .with_part2_count(5, 4); // 4 Part2-Werte bei Part1=5

        let original = EventCode::two(5, 3);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &original, &context).unwrap();
        let buf = writer.into_vec();

        let mut reader = BitReader::new(&buf);
        let decoded = decode_byte_aligned(&mut reader, &context).unwrap();

        assert_eq!(original, decoded);
    }

    #[test]
    fn round_trip_byte_aligned_three_parts() {
        let context = EventCodeContext::three(6, 2, 2);
        let original = EventCode::three(5, 1, 1);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &original, &context).unwrap();
        let buf = writer.into_vec();

        let mut reader = BitReader::new(&buf);
        let decoded = decode_byte_aligned(&mut reader, &context).unwrap();

        assert_eq!(original, decoded);
    }

    // ========================================================================
    // Edge Cases
    // ========================================================================

    #[test]
    fn large_values_byte_aligned() {
        // Mehr als 256 distinct values → 2 Bytes
        let context = EventCodeContext::one(300);
        let original = EventCode::one(299);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &original, &context).unwrap();
        let buf = writer.into_vec();

        // 299 = 0x012B, Little-Endian (Spec 7.1.9)
        assert_eq!(buf, vec![0x2B, 0x01]);

        let mut reader = BitReader::new(&buf);
        let decoded = decode_byte_aligned(&mut reader, &context).unwrap();
        assert_eq!(original, decoded);
    }

    /// Spec 7.1.9: Expliziter Test für Little-Endian Byte-Reihenfolge.
    ///
    /// Dieser Test würde fehlschlagen wenn Big-Endian verwendet wird.
    /// Wichtig für Interop mit großen Grammars (> 256 Productions).
    #[test]
    fn byte_aligned_little_endian_spec_7_1_9() {
        // 0x1234 = 4660 dezimal
        // Little-Endian: [0x34, 0x12] (LSB first)
        // Big-Endian wäre: [0x12, 0x34] (MSB first)
        let context = EventCodeContext::one(5000);
        let original = EventCode::one(0x1234);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &original, &context).unwrap();
        let buf = writer.into_vec();

        // Spec 7.1.9: "Bytes are ordered with the least significant byte first."
        assert_eq!(
            buf,
            vec![0x34, 0x12],
            "Byte-Reihenfolge muss Little-Endian sein (Spec 7.1.9)"
        );

        // Round-trip muss funktionieren
        let mut reader = BitReader::new(&buf);
        let decoded = decode_byte_aligned(&mut reader, &context).unwrap();
        assert_eq!(original, decoded);
    }

    /// Spec 7.1.9: 3-Byte Wert Little-Endian.
    #[test]
    fn byte_aligned_little_endian_three_bytes() {
        // 0x123456 = 1193046 dezimal
        // Little-Endian: [0x56, 0x34, 0x12]
        let context = EventCodeContext::one(0x200000); // braucht 3 Bytes
        let original = EventCode::one(0x123456);

        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &original, &context).unwrap();
        let buf = writer.into_vec();

        assert_eq!(
            buf,
            vec![0x56, 0x34, 0x12],
            "3-Byte Little-Endian (Spec 7.1.9)"
        );

        let mut reader = BitReader::new(&buf);
        let decoded = decode_byte_aligned(&mut reader, &context).unwrap();
        assert_eq!(original, decoded);
    }

    /// Spec 6.2: Per-sibling Bitbreiten.
    ///
    /// EventCodeContext::three(6, 2, 2) erstellt:
    /// - Part1: 6 Werte (0-5)
    /// - Part2: 2 Werte bei Part1=5
    /// - Part3: 2 Werte bei (Part1=5, Part2=1)
    #[test]
    fn context_per_sibling_bit_widths() {
        let context = EventCodeContext::three(6, 2, 2);

        // Part1 (global): 6 Werte → 3 Bits
        assert_eq!(context.bits_for_part1(), 3);

        // Part2 bei Part1=5: 2 Werte → 1 Bit
        assert_eq!(context.bits_for_part2(5), 1);
        // Part2 bei Part1=0: keine 2-teil Codes → 0 Bits
        assert_eq!(context.bits_for_part2(0), 0);

        // Part3 bei (5, 1): 2 Werte → 1 Bit
        assert_eq!(context.bits_for_part3(5, 1), 1);
        // Part3 bei (5, 0): keine 3-teil Codes → 0 Bits
        assert_eq!(context.bits_for_part3(5, 0), 0);
    }

    /// Spec 6.2: Per-sibling Byte-Breiten (byte-aligned).
    #[test]
    fn context_per_sibling_byte_widths() {
        let context = EventCodeContext::three(6, 2, 2);

        // Part1: 6 Werte → 1 Byte
        assert_eq!(context.bytes_for_part1(), 1);

        // Part2 bei Part1=5: 2 Werte → 1 Byte
        assert_eq!(context.bytes_for_part2(5), 1);
        // Part2 bei Part1=0: keine 2-teil Codes → 0 Bytes
        assert_eq!(context.bytes_for_part2(0), 0);

        // Part3 bei (5, 1): 2 Werte → 1 Byte
        assert_eq!(context.bytes_for_part3(5, 1), 1);
    }

    // ========================================================================
    // Tests für Example 8-5 (Spec 8.2)
    // ========================================================================

    #[test]
    fn example_8_5_ee_event_code_0() {
        // EE = 0
        let context = EventCodeContext::one(2); // EE=0, Rest beginnt mit 1
        let ec = EventCode::one(0);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 1 Bit: 0 + 7 Padding = 0x00
        assert_eq!(buf, vec![0b00000000]);
    }

    #[test]
    fn example_8_5_se_wildcard_event_code_1_0() {
        // SE(*) = 1.0
        // Part1: 2 distinct values (0, 1) → 1 Bit
        // Part2: 4 distinct values (0, 1, 2, 3) → 2 Bits
        let context = EventCodeContext::two(2, 4);
        let ec = EventCode::two(1, 0);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 1 00 + 5 Padding = 0b10000000 = 0x80
        assert_eq!(buf, vec![0b10000000]);
    }

    #[test]
    fn example_8_5_cm_event_code_1_3_0() {
        // CM = 1.3.0
        // Part1: 2 values → 1 Bit
        // Part2: 4 values → 2 Bits
        // Part3: 2 values → 1 Bit
        let context = EventCodeContext::three(2, 4, 2);
        let ec = EventCode::three(1, 3, 0);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 1 11 0 + 4 Padding = 0b11100000 = 0xE0
        assert_eq!(buf, vec![0b11100000]);
    }

    #[test]
    fn example_8_5_pi_event_code_1_3_1() {
        // PI = 1.3.1
        let context = EventCodeContext::three(2, 4, 2);
        let ec = EventCode::three(1, 3, 1);

        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &context).unwrap();
        let buf = writer.into_vec();

        // 1 11 1 + 4 Padding = 0b11110000 = 0xF0
        assert_eq!(buf, vec![0b11110000]);
    }

    // ========================================================================
    // Tests für increment_first_part (Spec 8.4.2)
    // ========================================================================

    /// Spec 8.4.2: Inkrementiert [n] → [n+1]
    #[test]
    fn increment_first_part_one_part() {
        let ec = EventCode::one(0);
        let incremented = ec.increment_first_part();
        assert_eq!(incremented, EventCode::one(1));

        let ec2 = EventCode::one(5);
        let incremented2 = ec2.increment_first_part();
        assert_eq!(incremented2, EventCode::one(6));
    }

    /// Spec 8.4.2: Inkrementiert [n.m] → [n+1.m]
    #[test]
    fn increment_first_part_two_parts() {
        let ec = EventCode::two(1, 0);
        let incremented = ec.increment_first_part();
        assert_eq!(incremented, EventCode::two(2, 0));

        let ec2 = EventCode::two(0, 3);
        let incremented2 = ec2.increment_first_part();
        assert_eq!(incremented2, EventCode::two(1, 3));
    }

    /// Spec 8.4.2: Inkrementiert [n.m.o] → [n+1.m.o]
    #[test]
    fn increment_first_part_three_parts() {
        let ec = EventCode::three(1, 2, 0);
        let incremented = ec.increment_first_part();
        assert_eq!(incremented, EventCode::three(2, 2, 0));

        let ec2 = EventCode::three(0, 1, 1);
        let incremented2 = ec2.increment_first_part();
        assert_eq!(incremented2, EventCode::three(1, 1, 1));
    }

    /// Spec 8.4.2: Fragment Grammar Evolution Szenario
    #[test]
    fn increment_first_part_fragment_evolution_scenario() {
        // Initial Fragment Grammar: SE(*) [0], ED [1], CM [2.0], PI [2.1]
        // Nach Lernen von "book": SE(book) [0], SE(*) [1], ED [2], CM [3.0], PI [3.1]
        let codes = vec![
            EventCode::one(0),    // SE(*)
            EventCode::one(1),    // ED
            EventCode::two(2, 0), // CM
            EventCode::two(2, 1), // PI
        ];

        let incremented: Vec<_> = codes.iter().map(|c| c.increment_first_part()).collect();

        assert_eq!(incremented[0], EventCode::one(1)); // SE(*) → [1]
        assert_eq!(incremented[1], EventCode::one(2)); // ED → [2]
        assert_eq!(incremented[2], EventCode::two(3, 0)); // CM → [3.0]
        assert_eq!(incremented[3], EventCode::two(3, 1)); // PI → [3.1]
    }

    /// Verhalten bei großen Event Code Werten (nahe u32::MAX).
    ///
    /// In der Praxis sind solche Werte unrealistisch (~4 Milliarden gelernte
    /// Elemente), aber der Test dokumentiert das Verhalten.
    #[test]
    fn increment_first_part_large_values() {
        // Nahe Maximum - funktioniert korrekt
        let ec = EventCode::one(u32::MAX - 1);
        let incremented = ec.increment_first_part();
        assert_eq!(incremented.part1(), u32::MAX);

        // Auch mit mehreren Teilen
        let ec2 = EventCode::two(u32::MAX - 1, 5);
        let incremented2 = ec2.increment_first_part();
        assert_eq!(incremented2.part1(), u32::MAX);
        assert_eq!(incremented2.part2(), Some(5));
    }

    // ========================================================================
    // Tests für has_part2/has_part3 (Coverage)
    // ========================================================================

    /// Spec 6.2: has_part2 prüft ob 2-Teil Codes existieren.
    #[test]
    fn has_part2_coverage() {
        // Context mit nur Part1 - keine Part2 Codes
        let ctx = EventCodeContext::one(3);
        assert!(!ctx.has_part2(0));
        assert!(!ctx.has_part2(1));
        assert!(!ctx.has_part2(2));

        // Context mit Part2 über two() Konstruktor
        // two(3, 2) → Part1=2 hat 2 Part2-Werte
        let ctx2 = EventCodeContext::two(3, 2);
        assert!(!ctx2.has_part2(0)); // Part1=0 hat keine Part2
        assert!(!ctx2.has_part2(1)); // Part1=1 hat keine Part2
        assert!(ctx2.has_part2(2)); // Part1=2 hat Part2 (=num_values_part1-1)
    }

    /// Spec 6.2: has_part3 prüft ob 3-Teil Codes existieren.
    #[test]
    fn has_part3_coverage() {
        // three(3, 2, 4) → Part1=2, Part2=1 hat 4 Part3-Werte
        let ctx = EventCodeContext::three(3, 2, 4);

        assert!(!ctx.has_part3(0, 0)); // Part1=0 hat keine Part3
        assert!(!ctx.has_part3(2, 0)); // Part1=2, Part2=0 hat keine Part3
        assert!(ctx.has_part3(2, 1)); // Part1=2, Part2=1 hat Part3
    }

    // ========================================================================
    // Tests für Zero-Bits Decode-Pfade (Coverage)
    // ========================================================================

    /// Spec 6.2: Wenn nur 1 Wert existiert, werden 0 Bits gelesen → immer 0.
    #[test]
    fn decode_zero_bits_returns_zero() {
        // Context mit nur einem Wert für Part1 → 0 Bits
        let ctx = EventCodeContext::one(1);
        let data = vec![0xFF]; // Beliebige Daten
        let mut reader = BitReader::new(&data);

        // Sollte 0 zurückgeben ohne Bits zu lesen
        let decoded = decode_bit_packed(&mut reader, &ctx).unwrap();
        assert_eq!(decoded.part1(), 0);
        assert_eq!(reader.bit_position(), 0); // Keine Bits konsumiert
    }

    /// Spec 6.2: Byte-aligned Decode bei num_values=1 → 0 Bytes, immer 0.
    #[test]
    fn decode_byte_aligned_zero_bytes_returns_zero() {
        // Context mit nur einem Wert für Part1 → 0 Bytes
        let ctx = EventCodeContext::one(1);
        let data = vec![0xFF]; // Beliebige Daten
        let mut reader = BitReader::new(&data);

        let decoded = decode_byte_aligned(&mut reader, &ctx).unwrap();
        assert_eq!(decoded.part1(), 0);
        // Keine Bytes konsumiert (nur wenn auch Part2/Part3 0 Bytes hätten)
    }

    // ========================================================================
    // Tests für Range-Validierung (Spec 6.2, Phase 4b/4c)
    // ========================================================================

    /// Spec 6.2: Encode mit Part1 außerhalb des gültigen Bereichs → Error.
    #[test]
    fn encode_part1_out_of_range() {
        let ctx = EventCodeContext::one(6); // Part1: 0..5
        let ec = EventCode::one(6); // 6 ist ungültig

        let result = encode_bit_packed(&mut BitWriter::new(), &ec, &ctx);
        assert!(result.is_err());

        let result = encode_byte_aligned(&mut BitWriter::new(), &ec, &ctx);
        assert!(result.is_err());
    }

    /// Spec 6.2: Encode mit Part1 == num_values bei has_second_level → OK (Escape).
    #[test]
    fn encode_part1_escape_with_second_level() {
        let ctx = EventCodeContext::empty()
            .with_part1_count(3)
            .with_second_level(true)
            .with_part2_count(3, 2); // Escape-Wert 3 hat Part2
        let ec = EventCode::two(3, 0); // Escape zu Tier 2

        encode_bit_packed(&mut BitWriter::new(), &ec, &ctx).unwrap();
        encode_byte_aligned(&mut BitWriter::new(), &ec, &ctx).unwrap();
    }

    /// Spec 6.2: Encode mit Part2 außerhalb des gültigen Bereichs → Error.
    #[test]
    fn encode_part2_out_of_range() {
        let ctx = EventCodeContext::two(6, 2); // Part2 bei Part1=5: 0..1
        let ec = EventCode::two(5, 2); // Part2=2 ist ungültig

        let result = encode_bit_packed(&mut BitWriter::new(), &ec, &ctx);
        assert!(result.is_err());

        let result = encode_byte_aligned(&mut BitWriter::new(), &ec, &ctx);
        assert!(result.is_err());
    }

    /// Spec 6.2: Encode mit Part3 außerhalb des gültigen Bereichs → Error.
    #[test]
    fn encode_part3_out_of_range() {
        let ctx = EventCodeContext::three(6, 2, 2); // Part3 bei (5,1): 0..1
        let ec = EventCode::three(5, 1, 2); // Part3=2 ist ungültig

        let result = encode_bit_packed(&mut BitWriter::new(), &ec, &ctx);
        assert!(result.is_err());

        let result = encode_byte_aligned(&mut BitWriter::new(), &ec, &ctx);
        assert!(result.is_err());
    }

    /// Spec 6.2: Decode mit ungültigem Part1-Wert im Stream → Error.
    ///
    /// Context hat 6 Part1-Werte (3 Bits). Wert 7 (111) passt in 3 Bits,
    /// liegt aber außerhalb des gültigen Bereichs 0..5.
    #[test]
    fn decode_part1_out_of_range() {
        let ctx = EventCodeContext::one(6); // Part1: 0..5, 3 Bits
        // 111 = 7 (ungültig, da >= 6)
        let data = vec![0b11100000];
        let mut reader = BitReader::new(&data);

        let result = decode_bit_packed(&mut reader, &ctx);
        assert!(result.is_err());
    }

    /// Spec 6.2: Decode mit ungültigem Part1-Wert (byte-aligned) → Error.
    #[test]
    fn decode_byte_aligned_part1_out_of_range() {
        let ctx = EventCodeContext::one(6); // Part1: 0..5, 1 Byte
        let data = vec![0x06]; // 6 ist ungültig
        let mut reader = BitReader::new(&data);

        let result = decode_byte_aligned(&mut reader, &ctx);
        assert!(result.is_err());
    }

    /// Spec 6.2: effective_part1_count mit und ohne second_level.
    #[test]
    fn effective_part1_count_coverage() {
        let ctx = EventCodeContext::one(5);
        assert_eq!(ctx.effective_part1_count(), 5);

        let ctx2 = EventCodeContext::empty()
            .with_part1_count(5)
            .with_second_level(true);
        assert_eq!(ctx2.effective_part1_count(), 6); // +1 für Escape
    }

    /// Spec 6.2: 3-Teil Code mit allen Teilen am Maximum (Boundary-Test).
    #[test]
    fn encode_all_parts_at_limit() {
        let ctx = EventCodeContext::empty()
            .with_part1_count(256)
            .with_part2_count(255, 256)
            .with_part3_count(255, 255, 256);

        let ec = EventCode::three(255, 255, 255);

        // Bit-packed Round-Trip
        let mut writer = BitWriter::new();
        encode_bit_packed(&mut writer, &ec, &ctx).unwrap();
        let buf = writer.into_vec();
        let mut reader = BitReader::new(&buf);
        let decoded = decode_bit_packed(&mut reader, &ctx).unwrap();
        assert_eq!(ec, decoded);

        // Byte-aligned Round-Trip
        let mut writer = BitWriter::new();
        encode_byte_aligned(&mut writer, &ec, &ctx).unwrap();
        let buf = writer.into_vec();
        let mut reader = BitReader::new(&buf);
        let decoded = decode_byte_aligned(&mut reader, &ctx).unwrap();
        assert_eq!(ec, decoded);
    }

    /// Spec 6.2: validate_part1 bei max == 0 (leerer Context).
    #[test]
    fn validate_empty_context() {
        let ctx = EventCodeContext::empty();
        // max == 0 → alles akzeptiert (kein Event möglich, 0 Bits)
        assert!(validate_part1(0, &ctx).is_ok());
    }
}
