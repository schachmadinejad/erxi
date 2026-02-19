//! EXI Grammar und Pruning (Spec 6.3, 8.1, 8.2, 8.3, 8.4)
//!
//! Dieses Modul definiert die Grammar-Datenstrukturen und die Pruning-Logik,
//! die Productions basierend auf Fidelity Options entfernt.
//!
//! # Architektur
//!
//! - `Grammar`: Einfache flache Production-Liste (für einfache Fälle)
//! - `GrammarSystem`: NonTerminal-basiertes System mit Macro-Expansion (Spec 8.4)
//!
//! # Spec-Referenz
//!
//! - 8.1 Grammar Notation (Fixed + Variable Event Codes)
//! - 8.2 Grammar Event Codes
//! - 8.3 Pruning Unneeded Productions
//! - 8.4 Built-in XML Grammars

use crate::FastHashMap;
use std::rc::Rc;

use crate::event_code::{EventCode, EventCodeContext, bits_for_part, bytes_for_part};
use crate::options::ExiOptions;
use crate::qname::{ExpandedNameId, InternedStr, QName, StringInterner};
use crate::string_table::URI_XSI;
use crate::{Error, Result};

// ============================================================================
// Grammar-Template-Cache (für Encoder + Decoder)
// ============================================================================

/// Key für den Grammar-Template-Cache.
///
/// Identifiziert eine Schema-Typ + nillable-Kombination eindeutig.
/// Options (strict, preserve, etc.) sind Session-Konstanten und müssen
/// nicht im Key enthalten sein, da der Cache pro Encoder/Decoder-Instanz lebt.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TemplateKey {
    /// Pointer-Identity des TypeDefinition Arc (0 = kein Typ / built-in).
    type_ptr: usize,
    /// Ob das Element nillable ist (beeinflusst xsi:nil-Production).
    is_nillable: bool,
}

impl TemplateKey {
    /// Erstellt einen TemplateKey aus einer ElementDeclaration.
    pub(crate) fn from_decl(decl: &crate::schema::ElementDeclaration) -> Self {
        Self {
            type_ptr: decl
                .type_definition
                .as_ref()
                .map(|td| Rc::as_ptr(td) as usize)
                .unwrap_or(0),
            is_nillable: decl.nillable,
        }
    }
}

/// Typ-Alias für den Grammar-Template-Cache.
pub(crate) type GrammarTemplateCache = FastHashMap<TemplateKey, Rc<GrammarSystem>>;


// ============================================================================
// ProductionTable (Array-basierter O(1) Event Code Lookup für Decoder)
// ============================================================================

/// Eintrag in der ProductionTable (Terminal + optionaler NonTerminal-Übergang).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ProductionEntry {
    pub terminal: Terminal,
    pub rhs: Option<NonTerminalId>,
}

/// Beschreibt eine Gruppe von Level-2-Einträgen mit demselben part1-Wert.
#[derive(Debug, Clone, Copy)]
struct Level2Group {
    part1: u32,
    count: u32,
    offset: u32,
}

/// Beschreibt eine Gruppe von Level-3-Einträgen mit demselben (part1, part2)-Paar.
#[derive(Debug, Clone, Copy)]
struct Level3Group {
    part1: u32,
    part2: u32,
    count: u32,
    offset: u32,
}

/// Array-basierter Event Code Lookup (ersetzt HashMap-basierte ProductionMap im Decoder).
///
/// Layout: `[level1...][level2_groups...][level3_groups...]`
/// - Level 1: Einträge mit 1-Teil Event Codes (Index = part1)
/// - Level 2: Einträge gruppiert nach part1 (per-part1 Bit-Breiten, Spec 6.2)
/// - Level 3: Einträge gruppiert nach (part1, part2)
#[derive(Debug, Clone)]
pub struct ProductionTable {
    entries: Vec<ProductionEntry>,
    level1_count: u32,
    level2_groups: Vec<Level2Group>,
    level3_groups: Vec<Level3Group>,
    bits_part1: u8,
    bytes_part1: u8,
    has_end_element_in_first_level: bool,
    has_multi_part: bool,
}

impl ProductionTable {
    /// Level-1 Lookup: O(1) Array-Index.
    #[inline]
    pub fn get_level1(&self, part1: u32) -> Option<ProductionEntry> {
        if part1 < self.level1_count {
            Some(self.entries[part1 as usize])
        } else {
            None
        }
    }

    /// Level-2 Lookup: Findet die part1-Gruppe, dann O(1) Array-Index.
    #[inline]
    pub fn get_level2(&self, part1: u32, part2: u32) -> Option<ProductionEntry> {
        for group in &self.level2_groups {
            if group.part1 == part1 {
                if part2 < group.count {
                    return Some(self.entries[group.offset as usize + part2 as usize]);
                } else {
                    return None;
                }
            }
        }
        None
    }

    /// Level-3 Lookup: Findet die (part1, part2)-Gruppe, dann O(1) Array-Index.
    #[inline]
    pub fn get_level3(&self, part1: u32, part2: u32, part3: u32) -> Option<ProductionEntry> {
        for group in &self.level3_groups {
            if group.part1 == part1 && group.part2 == part2 {
                if part3 < group.count {
                    return Some(self.entries[group.offset as usize + part3 as usize]);
                } else {
                    return None;
                }
            }
        }
        None
    }

    /// Anzahl Level-1-Einträge.
    #[inline]
    pub fn level1_count(&self) -> u32 {
        self.level1_count
    }

    /// Vorberechnete Bit-Breite für Part1.
    #[inline]
    pub fn bits_part1(&self) -> u8 {
        self.bits_part1
    }

    /// Vorberechnete Byte-Breite für Part1.
    #[inline]
    pub fn bytes_part1(&self) -> u8 {
        self.bytes_part1
    }

    /// Effektive Anzahl Level-2-Einträge für einen part1-Wert (inkl. Level-3-Marker).
    #[inline]
    fn level2_effective_count(&self, part1: u32) -> u32 {
        for group in &self.level2_groups {
            if group.part1 == part1 {
                let has_l3 = self.level3_groups.iter().any(|g| g.part1 == part1);
                return group.count + has_l3 as u32;
            }
        }
        0
    }

    /// Effektive Anzahl Level-3-Einträge für ein (part1, part2)-Paar.
    #[inline]
    fn level3_effective_count(&self, part1: u32, part2: u32) -> u32 {
        for group in &self.level3_groups {
            if group.part1 == part1 && group.part2 == part2 {
                return group.count;
            }
        }
        0
    }

    /// Bit-Breite für Part2 (per-part1, Spec 6.2 Sibling-Regel).
    #[inline]
    pub fn bits_for_part2(&self, part1: u32) -> u8 {
        bits_for_part(self.level2_effective_count(part1))
    }

    /// Byte-Breite für Part2 (per-part1, Spec 6.2 Sibling-Regel).
    #[inline]
    pub fn bytes_for_part2(&self, part1: u32) -> u8 {
        bytes_for_part(self.level2_effective_count(part1))
    }

    /// Bit-Breite für Part3 (per-(part1, part2), Spec 6.2 Sibling-Regel).
    #[inline]
    pub fn bits_for_part3(&self, part1: u32, part2: u32) -> u8 {
        bits_for_part(self.level3_effective_count(part1, part2))
    }

    /// Byte-Breite für Part3 (per-(part1, part2), Spec 6.2 Sibling-Regel).
    #[inline]
    pub fn bytes_for_part3(&self, part1: u32, part2: u32) -> u8 {
        bytes_for_part(self.level3_effective_count(part1, part2))
    }

    /// Ob EndElement in Level 1 vorkommt.
    #[inline]
    pub fn has_end_element_in_first_level(&self) -> bool {
        self.has_end_element_in_first_level
    }

    /// Ob die Grammatik Multi-Part Event Codes hat.
    #[inline]
    pub fn has_multi_part(&self) -> bool {
        self.has_multi_part
    }

    /// Fügt einen neuen Level-1-Eintrag an Position 0 ein (für Grammar-Learning).
    fn prepend_level1(&mut self, entry: ProductionEntry) {
        self.entries.insert(0, entry);
        self.level1_count += 1;
        // Alle Gruppen-Offsets anpassen (+1 wegen Insert an Position 0)
        for group in &mut self.level2_groups {
            group.offset += 1;
        }
        for group in &mut self.level3_groups {
            group.offset += 1;
        }
        if matches!(entry.terminal, Terminal::EndElement) {
            self.has_end_element_in_first_level = true;
        }
        self.bits_part1 = bits_for_part(self.level1_count + self.has_multi_part as u32);
        self.bytes_part1 = bytes_for_part(self.level1_count + self.has_multi_part as u32);
    }
}

/// Baut eine ProductionTable aus einem NonTerminal.
///
/// Sammelt alle Productions mit effektivem Event Code, gruppiert nach Code-Länge.
/// Level 2 wird nach part1, Level 3 nach (part1, part2) gruppiert (Spec 6.2 Sibling-Regel).
fn build_production_table(nt: &NonTerminal) -> ProductionTable {
    use std::collections::BTreeMap;

    // Level 1: BTreeMap statt Vec, damit bei Duplikat-Event-Codes der letzte Eintrag
    // gewinnt (konsistent mit HashMap::collect im ProductionMap-Build).
    // Duplikate können durch add_namespace_decls_to_start_tag entstehen.
    let mut level1_map: BTreeMap<u32, ProductionEntry> = BTreeMap::new();
    let mut level2_map: BTreeMap<u32, BTreeMap<u32, ProductionEntry>> = BTreeMap::new();
    let mut level3_map: BTreeMap<(u32, u32), BTreeMap<u32, ProductionEntry>> = BTreeMap::new();
    for (idx, prod) in nt.productions().iter().enumerate() {
        if let Some(ec) = nt.effective_event_code(idx) {
            let entry = ProductionEntry {
                terminal: prod.terminal,
                rhs: prod.right_hand_side,
            };
            match ec.length() {
                1 => {
                    level1_map.insert(ec.part1(), entry);
                }
                2 => {
                    level2_map
                        .entry(ec.part1())
                        .or_default()
                        .insert(ec.part2().unwrap(), entry);
                }
                3 => {
                    level3_map
                        .entry((ec.part1(), ec.part2().unwrap()))
                        .or_default()
                        .insert(ec.part3().unwrap(), entry);
                }
                _ => unreachable!("Event Code Länge > 3"),
            }
        }
    }

    let has_multi_part = !level2_map.is_empty() || !level3_map.is_empty();

    let l1 = level1_map.len() as u32;

    // Level-1-Einträge in aufsteigender part1-Reihenfolge (BTreeMap ist sortiert)
    let mut entries: Vec<ProductionEntry> = level1_map.values().copied().collect();
    let has_ee_in_l1 = entries.iter().any(|e| matches!(e.terminal, Terminal::EndElement));

    // Level-2-Gruppen (BTreeMap ist bereits nach part2 sortiert, Duplikate überschrieben)
    let mut level2_groups = Vec::new();
    for (part1, group) in level2_map {
        let offset = entries.len() as u32;
        let count = group.len() as u32;
        entries.extend(group.values());
        level2_groups.push(Level2Group { part1, count, offset });
    }

    // Level-3-Gruppen (BTreeMap ist bereits nach part3 sortiert, Duplikate überschrieben)
    let mut level3_groups = Vec::new();
    for ((part1, part2), group) in level3_map {
        let offset = entries.len() as u32;
        let count = group.len() as u32;
        entries.extend(group.values());
        level3_groups.push(Level3Group { part1, part2, count, offset });
    }

    ProductionTable {
        entries,
        level1_count: l1,
        level2_groups,
        level3_groups,
        bits_part1: bits_for_part(l1 + has_multi_part as u32),
        bytes_part1: bytes_for_part(l1 + has_multi_part as u32),
        has_end_element_in_first_level: has_ee_in_l1,
        has_multi_part,
    }
}

/// Holt eine Grammar aus dem Template-Cache oder erzeugt sie per `schema_informed_element`.
///
/// Das Pattern `get-or-insert` wird sowohl im Encoder als auch im Decoder
/// an mehreren Stellen benötigt. Diese Funktion kapselt es.
pub(crate) fn cached_schema_grammar(
    cache: &mut GrammarTemplateCache,
    decl: &crate::schema::ElementDeclaration,
    options: &crate::options::ExiOptions,
    interner: &mut StringInterner,
) -> Result<Rc<GrammarSystem>> {
    let key = TemplateKey::from_decl(decl);
    if let Some(cached) = cache.get(&key) {
        return Ok(Rc::clone(cached));
    }
    let g = Rc::new(GrammarSystem::schema_informed_element(decl, options, interner)?);
    cache.insert(key, Rc::clone(&g));
    Ok(g)
}

// ============================================================================
// Gemeinsame Hilfsfunktionen
// ============================================================================

/// Konvertiert usize zu u32 mit Überlauf-Prüfung.
///
/// EXI Event Codes sind u32, daher darf die Anzahl der Schema-Elemente
/// u32::MAX nicht überschreiten.
fn checked_u32(value: usize) -> Result<u32> {
    u32::try_from(value).map_err(|_| Error::IntegerOverflow)
}

/// Prüft ob ein Terminal bei gegebenen Options erlaubt ist (für Non-Document Grammars).
///
/// # Spec-Referenz
/// - 6.3 Fidelity Options, Table 6-3
/// - 8.5.4.1.1: Bei strict=true werden Wildcard-Productions aus Element/Type Grammars entfernt
/// - NICHT aus Document/Fragment Grammars (diese sind keine "schema types and elements")
fn is_terminal_allowed(
    terminal: &Terminal,
    options: &ExiOptions,
    grammar_type: GrammarType,
) -> bool {
    let is_document_grammar = matches!(grammar_type, GrammarType::Document | GrammarType::Fragment);
    let is_schema_based = matches!(
        grammar_type,
        GrammarType::SchemaInformedElement | GrammarType::ElementFragment
    );
    let is_built_in_element = matches!(grammar_type, GrammarType::Element);
    match terminal {
        Terminal::Comment => options.preserve.comments,
        Terminal::ProcessingInstr => options.preserve.pis,
        Terminal::DocType | Terminal::EntityRef => options.preserve.dtd,
        Terminal::NamespaceDecl => options.preserve.prefixes,
        Terminal::SelfContained => options.self_contained,
        // Wildcards sind bei schema-definierten any/anyAttribute erlaubt.
        // Ungetypte Wildcards (schema-deviation) sind bei strict=true verboten.
        Terminal::StartElement(StartElementKind::Wildcard) => {
            !options.strict || is_document_grammar || is_schema_based || is_built_in_element
        }
        Terminal::Attribute(AttributeKind::Wildcard) => {
            !options.strict || is_document_grammar || is_schema_based || is_built_in_element
        }
        Terminal::Attribute(AttributeKind::WildcardUntyped) => !options.strict,
        Terminal::Attribute(AttributeKind::QNameUntyped(_)) => !options.strict,
        Terminal::CharactersUntyped => !options.strict,
        // Diese Terminals sind immer erlaubt
        Terminal::StartDocument
        | Terminal::EndDocument
        | Terminal::StartElement(_)
        | Terminal::EndElement
        | Terminal::Attribute(_)
        | Terminal::Characters
        => true,
    }
}

/// Maximale Anzahl gelernter Productions pro NonTerminal (DoS-Schutz).
///
/// Verhindert dass ein bösartiger Stream unbegrenzt viele Productions lernt.
/// Bei Erreichen des Limits wird kein weiteres Lernen durchgeführt,
/// SE(*)/AT(*)/CH bleibt als Fallback erhalten.
const MAX_LEARNED_PRODUCTIONS: usize = 100_000;

/// Berechnet Event Codes nach Pruning neu, erhält die hierarchische Struktur.
///
/// Die Spec (Example 8-6 → 8-7) zeigt:
/// - Vor Pruning: EE [0], SE(*) [1.0], CH [1.1], ER [1.2], CM [1.3.0], PI [1.3.1]
/// - Nach Pruning (ER, CM, PI entfernt): EE [0], SE(*) [1.0], CH [1.1]
///
/// Die hierarchische Struktur bleibt erhalten! 1-teil Codes bleiben 1-teil,
/// 2-teil bleiben 2-teil, etc. Nur innerhalb jeder Sibling-Gruppe werden
/// die Indizes contiguous gemacht.
///
/// # Algorithmus
///
/// 1. 1-teil Codes: Renummeriere part1 als 0, 1, 2, ...
/// 2. 2-teil Codes: part1 beginnt nach dem höchsten 1-teil Code,
///    part2 wird pro Gruppe (gleicher part1) als 0, 1, 2, ... nummeriert
/// 3. 3-teil Codes: Analog, part3 wird pro Gruppe (gleiche part1, part2) nummeriert
///
/// # Spec-Referenz
/// - 8.3 Pruning Unneeded Productions
/// - Example 8-6, 8-7
fn recalculate_event_codes(productions: &mut [Production]) {
    use std::collections::BTreeMap;
    use std::collections::BTreeSet;

    if productions.is_empty() {
        return;
    }

    // None-Productions werden komplett ignoriert (bleiben None).
    // Nur Productions mit existierendem Event Code werden neu berechnet.

    // Hilfsfunktionen arbeiten nur mit Some-Variants
    let get_level = |ec: &EventCode| ec.length();
    let get_part1 = |ec: &EventCode| ec.part1();
    let get_part2 = |ec: &EventCode| ec.part2().unwrap_or(0);

    // Schritt 1: Sammle original part1-Werte für 1-teil Codes (geordnet)
    let mut one_part_values: BTreeSet<u32> = BTreeSet::new();
    for ec in productions.iter().filter_map(|p| p.event_code.as_ref()) {
        if get_level(ec) == 1 {
            one_part_values.insert(get_part1(ec));
        }
    }
    let mut one_part_map: BTreeMap<u32, u32> = BTreeMap::new();
    for (idx, orig) in one_part_values.iter().copied().enumerate() {
        one_part_map.insert(orig, idx as u32);
    }
    let one_part_count = one_part_map.len() as u32;

    // Schritt 2: Sammle original part1-Werte für 2/3-teil Codes (geordnet)
    let mut multi_part_values: BTreeSet<u32> = BTreeSet::new();
    for ec in productions.iter().filter_map(|p| p.event_code.as_ref()) {
        if get_level(ec) >= 2 {
            multi_part_values.insert(get_part1(ec));
        }
    }
    let mut multi_part1_map: BTreeMap<u32, u32> = BTreeMap::new();
    for (idx, orig) in multi_part_values.iter().copied().enumerate() {
        multi_part1_map.insert(orig, one_part_count + idx as u32);
    }

    // Schritt 3: Für jedes part1: sammle alle orig_part2 (aus 2- und 3-teil Codes)
    // und ordne sie nach orig_part2.
    let mut part2_map: BTreeMap<(u32, u32), u32> = BTreeMap::new();
    for &orig_part1 in &multi_part_values {
        let mut part2_values: BTreeSet<u32> = BTreeSet::new();
        for ec in productions.iter().filter_map(|p| p.event_code.as_ref()) {
            if get_level(ec) >= 2 && get_part1(ec) == orig_part1 {
                part2_values.insert(get_part2(ec));
            }
        }
        for (idx, orig_part2) in part2_values.iter().copied().enumerate() {
            part2_map.insert((orig_part1, orig_part2), idx as u32);
        }
    }

    // Schritt 4: Für 3-teil Codes, sammle orig_part3 pro (part1, part2) und ordne sie.
    let mut part3_map: BTreeMap<(u32, u32, u32), u32> = BTreeMap::new();
    for ec in productions.iter().filter_map(|p| p.event_code.as_ref()) {
        if get_level(ec) == 3 {
            let key = (get_part1(ec), get_part2(ec));
            part3_map.entry((key.0, key.1, ec.part3().unwrap_or(0))).or_insert(0);
        }
    }
    // Für jede (part1, part2) Gruppe die orig_part3 Werte ordnen
    let mut grouped_part3: BTreeMap<(u32, u32), Vec<u32>> = BTreeMap::new();
    for (p1, p2, p3) in part3_map.keys().copied() {
        grouped_part3.entry((p1, p2)).or_default().push(p3);
    }
    for ((p1, p2), mut vals) in grouped_part3 {
        vals.sort_unstable();
        for (idx, orig_p3) in vals.into_iter().enumerate() {
            part3_map.insert((p1, p2, orig_p3), idx as u32);
        }
    }

    // Schritt 5: Berechne neue Event Codes basierend auf den Mappings
    for p in productions.iter_mut() {
        let Some(ref ec) = p.event_code else { continue };
        let level = get_level(ec);
        let orig_part1 = get_part1(ec);
        let orig_part2 = get_part2(ec);
        match level {
            1 => {
                let new_part1 = one_part_map[&orig_part1];
                p.event_code = Some(EventCode::one(new_part1));
            }
            2 => {
                let new_part1 = multi_part1_map[&orig_part1];
                let new_part2 = part2_map[&(orig_part1, orig_part2)];
                p.event_code = Some(EventCode::two(new_part1, new_part2));
            }
            3 => {
                let new_part1 = multi_part1_map[&orig_part1];
                let new_part2 = part2_map[&(orig_part1, orig_part2)];
                let new_part3 = part3_map[&(orig_part1, orig_part2, ec.part3().unwrap_or(0))];
                p.event_code = Some(EventCode::three(new_part1, new_part2, new_part3));
            }
            n => unreachable!(
                "Event Code Länge muss 1, 2 oder 3 sein, war {} für {:?}",
                n, p.event_code
            ),
        }
    }
}

/// Berechnet den EventCodeContext aus einer Production-Liste.
///
/// Zählt die distinct values pro Event-Code-Teil basierend
/// auf den aktuellen Productions mit per-sibling Gruppierung (Spec 6.2).
///
/// # Spec 6.2: Sibling-basierte Bitbreiten
///
/// "Two event codes are siblings at the i-th part if and only if they share
/// the same values in all preceding parts."
///
/// Das bedeutet:
/// - Part 1: Alle Event Codes sind Geschwister → globales Maximum
/// - Part 2: Pro Part1-Wert wird separat gezählt
/// - Part 3: Pro (Part1, Part2)-Kombination wird separat gezählt
///
/// # Annahme
///
/// Setzt contiguous Event Codes (0, 1, 2, ...) voraus.
/// Built-in Grammars garantieren dies durch `recalculate_event_codes()`.

/// Baut einen Terminal -> Index Lookup fuer O(1) Zugriff.
///
/// Bei duplizierten Terminals wird der erste Eintrag bevorzugt
/// (vorwaerts-Iteration entspricht Original-Reihenfolge).
fn build_terminal_index(productions: &[Production]) -> FastHashMap<Terminal, usize> {
    let mut map = FastHashMap::default();
    map.reserve(productions.len());
    for (idx, p) in productions.iter().enumerate() {
        debug_assert!(
            !map.contains_key(&p.terminal) || map[&p.terminal] < idx,
            "Duplikat-Terminal in Original-Zone: {:?}", p.terminal
        );
        map.entry(p.terminal.clone()).or_insert(idx);
    }
    map
}

/// # Spec-Referenz
/// - 6.2 Representing Event Codes
fn compute_event_code_context(productions: &[Production]) -> EventCodeContext {
    use std::collections::BTreeMap;

    if productions.is_empty() {
        return EventCodeContext::one(0);
    }

    let mut max_part1 = 0u32;
    // Part2 max pro Part1-Wert
    let mut part2_max: BTreeMap<u32, u32> = BTreeMap::new();
    // Part3 max pro (Part1, Part2)-Kombination
    let mut part3_max: BTreeMap<(u32, u32), u32> = BTreeMap::new();

    for p in productions {
        // Productions müssen Event Codes haben - sonst ist die Grammar unvollständig
        let Some(ref ec) = p.event_code else {
            debug_assert!(
                false,
                "Production ohne Event Code in compute_event_code_context: {:?}. \
                 Rufe recalculate_event_codes() auf bevor du den Context berechnest.",
                p.terminal
            );
            continue;
        };

        let p1 = ec.part1();
        max_part1 = max_part1.max(p1);

        if let Some(p2) = ec.part2() {
            let entry = part2_max.entry(p1).or_insert(0);
            *entry = (*entry).max(p2);

            if let Some(p3) = ec.part3() {
                let entry = part3_max.entry((p1, p2)).or_insert(0);
                *entry = (*entry).max(p3);
            }
        }
    }

    // Context mit per-sibling Counts erstellen
    let mut ctx = EventCodeContext::empty().with_part1_count(max_part1.saturating_add(1));

    for (part1, max_p2) in part2_max {
        ctx = ctx.with_part2_count(part1, max_p2.saturating_add(1));
    }

    for ((part1, part2), max_p3) in part3_max {
        ctx = ctx.with_part3_count(part1, part2, max_p3.saturating_add(1));
    }

    ctx
}

/// Berechnet EventCodeContext nur für Productions mit Event-Code-Länge 1.
///
/// Inline-Filter statt Vec-Clone: Iteriert direkt über die Productions
/// und berücksichtigt nur solche mit einstelligen Event Codes.
fn compute_context_length_one(productions: &[Production]) -> EventCodeContext {
    let mut max_part1 = 0u32;
    let mut count = 0u32;

    for p in productions {
        if let Some(ec) = p.event_code.as_ref() {
            if ec.length() == 1 {
                max_part1 = max_part1.max(ec.part1());
                count += 1;
            }
        }
    }

    if count == 0 {
        return EventCodeContext::one(0);
    }

    EventCodeContext::one(max_part1.saturating_add(1))
}


// ============================================================================
// NonTerminalId (Spec 8.4)
// ============================================================================

/// Identifier für NonTerminal-Symbole in EXI Grammars.
///
/// Jede Built-in Grammar besteht aus NonTerminals mit eindeutigen IDs.
/// Diese IDs werden verwendet um Referenzen zwischen Productions herzustellen.
///
/// # Spec-Referenz
///
/// - 8.4.1 Built-in Document Grammar (Document, DocContent, DocEnd)
/// - 8.4.2 Built-in Fragment Grammar (Fragment, FragmentContent)
/// - 8.4.3 Built-in Element Grammar (StartTagContent, ElementContent)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum NonTerminalId {
    /// Document Grammar Einstiegspunkt (Spec 8.4.1)
    Document,
    /// Document Content nach SD (Spec 8.4.1)
    DocContent,
    /// Document Ende nach Root-Element (Spec 8.4.1)
    DocEnd,
    /// Fragment Grammar Einstiegspunkt (Spec 8.4.2)
    Fragment,
    /// Fragment Content nach SD (Spec 8.4.2)
    FragmentContent,
    /// Element Start-Tag Content (Spec 8.4.3)
    StartTagContent,
    /// Element Content nach Start-Tag (Spec 8.4.3)
    ElementContent,
    /// Element Fragment Grammar 0 - Start mit Attributen (Spec 8.5.3)
    ElementFragment0,
    /// Element Fragment Grammar 1 - Nach Attributen (Spec 8.5.3)
    ElementFragment1,
    /// Element Fragment Grammar 2 - Element Content (Spec 8.5.3)
    ElementFragment2,
    /// Element Fragment TypeEmpty 0 - Start mit Attributen (Spec 8.5.3)
    ElementFragmentTypeEmpty0,
    /// Element Fragment TypeEmpty 1 - Nach Attributen (Spec 8.5.3)
    ElementFragmentTypeEmpty1,
    /// Dynamisch generiertes NonTerminal aus Proto-Grammar (Spec 8.5.4).
    ///
    /// Wird bei Konvertierung von Proto-Grammar zu normalisierten NonTerminals
    /// verwendet. Der Index ist relativ zur Proto-Grammar.
    Dynamic(usize),
    /// Schema-informed Type Grammar NonTerminal (Spec 8.5.4).
    ///
    /// Wird für Schema-informed Element Grammars verwendet. Der Index
    /// entspricht dem NonTerminal-Index in der Type Grammar (0, 1, ...).
    SchemaType(usize),
    /// Content2 Kopie für Undeclared Productions (Spec 8.5.4.4.1).
    ///
    /// Bei der Augmentierung mit Undeclared Productions wird eine Kopie des
    /// Content-NonTerminals erstellt. SE(*), CH[untyped], ER, CM, PI Productions
    /// für j ≤ content zeigen auf dieses Content2 NonTerminal.
    ///
    /// Der usize ist der Original-Content-Index für Debugging.
    /// WICHTIG: Muss NACH SchemaType stehen, damit Ord-Sortierung korrekt ist
    /// (Content2 wird bei Augmentierung am Ende des Vec angehängt).
    Content2(usize),
}

/// Schnelle Bit-Repräsentation einer NonTerminalId für Cache-Slot-Hashing.
/// Vermeidet `std::mem::discriminant` + `AHasher::default()` (teuer).
#[inline]
pub fn compact_nt_bits(nt: NonTerminalId) -> u32 {
    /// XOR-Fold: obere Bits in untere 12 Bit mischen.
    #[inline]
    fn fold(tag: u32, i: usize) -> u32 {
        tag | ((i as u32) ^ ((i >> 12) as u32))
    }
    match nt {
        NonTerminalId::Document => 0x100,
        NonTerminalId::DocContent => 0x200,
        NonTerminalId::DocEnd => 0x300,
        NonTerminalId::Fragment => 0x400,
        NonTerminalId::FragmentContent => 0x500,
        NonTerminalId::StartTagContent => 0x600,
        NonTerminalId::ElementContent => 0x700,
        NonTerminalId::ElementFragment0 => 0x800,
        NonTerminalId::ElementFragment1 => 0x900,
        NonTerminalId::ElementFragment2 => 0xA00,
        NonTerminalId::ElementFragmentTypeEmpty0 => 0xB00,
        NonTerminalId::ElementFragmentTypeEmpty1 => 0xC00,
        NonTerminalId::Dynamic(i) => fold(0xD000, i),
        NonTerminalId::SchemaType(i) => fold(0xE000, i),
        NonTerminalId::Content2(i) => fold(0xF000, i),
    }
}

// ============================================================================
// StartElementKind (Spec 8.4)
// ============================================================================

/// Art des StartElement-Terminals (Spec 8.4).
///
/// Unterscheidet zwischen Wildcard SE(*) und spezifischem SE(qname).
/// Bei Fragment Grammar Evolution (8.4.2) werden SE(qname) Productions
/// dynamisch gelernt.
///
/// # Spec-Referenz
///
/// - 8.4.1: Document Grammar verwendet nur SE(*)
/// - 8.4.2: Fragment Grammar lernt SE(qname) dynamisch
/// - 8.4.3: Element Grammar verwendet SE(*)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StartElementKind {
    /// SE(*) - Wildcard, matched jeden Element-Namen.
    ///
    /// Initial in allen Built-in Grammars vorhanden.
    Wildcard,
    /// SE(uri:*) - Namespace-Wildcard, matched Elemente mit dieser URI (Spec 8.5.4.1.7).
    ///
    /// Verwendet bei Wildcard Terms mit Namespace-Constraint.
    /// Leerer String "" repräsentiert "absent" (kein Namespace).
    /// `InternedStr` für Copy-fähigkeit und O(1) Eq/Hash.
    NamespaceWildcard(InternedStr),
    /// SE(qname) - Spezifisch für einen QName.
    ///
    /// Wird bei Fragment Grammar Evolution gelernt (Spec 8.4.2).
    /// `ExpandedNameId` für Copy-fähigkeit und O(1) Eq/Hash.
    QName(ExpandedNameId),
}

// ============================================================================
// AttributeKind (Spec 8.4)
// ============================================================================

/// Art des Attribute-Terminals (Spec 8.4).
///
/// Unterscheidet zwischen Wildcard AT(*) und spezifischem AT(qname).
///
/// # Spec-Referenz
///
/// - 8.4.3: Element Grammar verwendet nur AT(*)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AttributeKind {
    /// AT(*) - Wildcard, matched jeden Attribut-Namen.
    Wildcard,
    /// AT(*) [untyped] - Wildcard für schema-invalid Attribute (Spec 8.5.4.4).
    WildcardUntyped,
    /// AT(uri:*) - Namespace-Wildcard, matched Attribute mit dieser URI (Spec 8.5.4.1.3.2).
    ///
    /// Verwendet bei Attribute Wildcards mit Namespace-Constraint.
    /// Leerer String "" repräsentiert "absent" (kein Namespace).
    /// `InternedStr` für Copy-fähigkeit und O(1) Eq/Hash.
    NamespaceWildcard(InternedStr),
    /// AT(qname) - Spezifisch für einen QName.
    /// `ExpandedNameId` für Copy-fähigkeit und O(1) Eq/Hash.
    QName(ExpandedNameId),
    /// AT(qname) [untyped] - Spezifisch für einen QName mit untyped value (Spec 8.5.4.4.1).
    QNameUntyped(ExpandedNameId),
}

/// Konstante für SE(*) Wildcard-Terminal.
pub const SE_WILDCARD: Terminal = Terminal::StartElement(StartElementKind::Wildcard);

/// Konstante für AT(*) Wildcard-Terminal.
pub const AT_WILDCARD: Terminal = Terminal::Attribute(AttributeKind::Wildcard);

// ============================================================================
// Terminal (Spec 4, Table 4-1)
// ============================================================================

/// Terminal-Symbol einer Production (Event-Typ).
///
/// Repräsentiert die verschiedenen Event-Typen die in EXI Grammars vorkommen.
///
/// # Spec-Referenz
/// - 4. EXI Streams, Table 4-1
/// - 8.4 Built-in XML Grammars (SE/AT Wildcard vs QName)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Terminal {
    /// Start Document (SD)
    StartDocument,
    /// End Document (ED)
    EndDocument,
    /// Start Element (SE) - Wildcard oder spezifischer QName
    StartElement(StartElementKind),
    /// End Element (EE)
    EndElement,
    /// Attribute (AT) - Wildcard oder spezifischer QName
    Attribute(AttributeKind),
    /// Characters (CH) [schema-typed value]
    Characters,
    /// Characters (CH) [untyped value] (Spec 8.5.4.4.1)
    CharactersUntyped,
    /// Namespace Declaration (NS)
    NamespaceDecl,
    /// Comment (CM)
    Comment,
    /// Processing Instruction (PI)
    ProcessingInstr,
    /// DOCTYPE (DT)
    DocType,
    /// Entity Reference (ER)
    EntityRef,
    /// Self-Contained (SC)
    SelfContained,
}

impl Terminal {
    /// Prueft ob dieses Terminal ein Wildcard-Terminal ist (SE(*), SE(uri:*),
    /// AT(*), AT(*)[untyped], AT(uri:*)).
    ///
    /// Wird im Encoder und Decoder verwendet um zu entscheiden, ob der QName
    /// explizit im Stream encodiert/decodiert werden muss.
    pub fn is_wildcard(&self) -> bool {
        matches!(
            self,
            Terminal::StartElement(
                StartElementKind::Wildcard | StartElementKind::NamespaceWildcard(_)
            ) | Terminal::Attribute(
                AttributeKind::Wildcard
                    | AttributeKind::WildcardUntyped
                    | AttributeKind::NamespaceWildcard(_)
            )
        )
    }

    /// Prueft ob dieses Terminal ein untyped-Terminal ist (AT(*)[untyped],
    /// AT(qname)[untyped], CH[untyped]).
    ///
    /// Bei untyped-Terminals wird der Wert immer als String encodiert,
    /// auch wenn ein Schema-Typ vorliegt (Spec 8.5.4.4.1).
    pub fn is_untyped(&self) -> bool {
        matches!(
            self,
            Terminal::Attribute(AttributeKind::WildcardUntyped | AttributeKind::QNameUntyped(_))
                | Terminal::CharactersUntyped
        )
    }

    /// Erstellt SE(qname) aus QName via Interner.
    pub fn se_qname(qname: &QName, interner: &mut StringInterner) -> Result<Self> {
        let id = interner.intern_expanded(&qname.uri, &qname.local_name)?;
        Ok(Terminal::StartElement(StartElementKind::QName(id)))
    }

    /// Erstellt AT(qname) aus QName via Interner.
    pub fn at_qname(qname: &QName, interner: &mut StringInterner) -> Result<Self> {
        let id = interner.intern_expanded(&qname.uri, &qname.local_name)?;
        Ok(Terminal::Attribute(AttributeKind::QName(id)))
    }

    /// Erzeugt ein konkretes Terminal aus einem Wildcard-Terminal und einem QName.
    ///
    /// SE(*)/SE(uri:*) → SE(qname)
    /// AT(*)/AT(uri:*)/AT(*)[untyped] → AT(qname)
    pub fn learned_from_wildcard(
        &self,
        qname: &QName,
        interner: &mut StringInterner,
    ) -> Result<Option<Self>> {
        match self {
            Terminal::StartElement(
                StartElementKind::Wildcard | StartElementKind::NamespaceWildcard(_),
            ) => Ok(Some(Terminal::se_qname(qname, interner)?)),
            Terminal::Attribute(
                AttributeKind::Wildcard
                | AttributeKind::WildcardUntyped
                | AttributeKind::NamespaceWildcard(_),
            ) => Ok(Some(Terminal::at_qname(qname, interner)?)),
            _ => Ok(None),
        }
    }

    /// Erstellt AT(qname)[untyped] aus QName via Interner.
    pub fn at_qname_untyped(qname: &QName, interner: &mut StringInterner) -> Result<Self> {
        let id = interner.intern_expanded(&qname.uri, &qname.local_name)?;
        Ok(Terminal::Attribute(AttributeKind::QNameUntyped(id)))
    }

    /// Erstellt SE(uri:*) aus URI-String via Interner.
    pub fn se_ns_wildcard(uri: &str, interner: &mut StringInterner) -> Result<Self> {
        let id = interner.intern(uri)?;
        Ok(Terminal::StartElement(StartElementKind::NamespaceWildcard(id)))
    }

    /// Erstellt AT(uri:*) aus URI-String via Interner.
    pub fn at_ns_wildcard(uri: &str, interner: &mut StringInterner) -> Result<Self> {
        let id = interner.intern(uri)?;
        Ok(Terminal::Attribute(AttributeKind::NamespaceWildcard(id)))
    }

    /// Erstellt SE(qname) direkt aus ExpandedNameId (Copy, kein Interner nötig).
    pub const fn se_expanded(id: ExpandedNameId) -> Self {
        Terminal::StartElement(StartElementKind::QName(id))
    }

    /// Erstellt AT(qname) direkt aus ExpandedNameId (Copy, kein Interner nötig).
    pub const fn at_expanded(id: ExpandedNameId) -> Self {
        Terminal::Attribute(AttributeKind::QName(id))
    }

    /// Erstellt AT(qname)[untyped] direkt aus ExpandedNameId (Copy, kein Interner nötig).
    pub const fn at_expanded_untyped(id: ExpandedNameId) -> Self {
        Terminal::Attribute(AttributeKind::QNameUntyped(id))
    }

    /// Löst die ExpandedNameId in ein QName auf (für Event-Ausgabe).
    /// Gibt None zurück wenn kein QName-Terminal.
    pub fn resolve_qname(&self, interner: &StringInterner) -> Option<QName> {
        match self {
            Terminal::StartElement(StartElementKind::QName(id))
            | Terminal::Attribute(AttributeKind::QName(id))
            | Terminal::Attribute(AttributeKind::QNameUntyped(id)) => {
                let (uri, local) = id.resolve(interner);
                Some(QName::new(uri, local))
            }
            _ => None,
        }
    }

    /// Gibt die ExpandedNameId zurück, falls vorhanden.
    pub fn expanded_name_id(&self) -> Option<ExpandedNameId> {
        match self {
            Terminal::StartElement(StartElementKind::QName(id))
            | Terminal::Attribute(AttributeKind::QName(id))
            | Terminal::Attribute(AttributeKind::QNameUntyped(id)) => Some(*id),
            _ => None,
        }
    }

    /// Debug-Name fuer Tracing (Exificient-kompatible Event-Type-Bezeichner).
    pub fn debug_event_type(&self) -> &'static str {
        match self {
            Terminal::StartDocument => "START_DOCUMENT",
            Terminal::EndDocument => "END_DOCUMENT",
            Terminal::StartElement(StartElementKind::QName(_)) => "START_ELEMENT",
            Terminal::StartElement(StartElementKind::NamespaceWildcard(_)) => "START_ELEMENT_NS",
            Terminal::StartElement(StartElementKind::Wildcard) => "START_ELEMENT_GENERIC_UNDECLARED",
            Terminal::Attribute(AttributeKind::QName(_)) => "ATTRIBUTE",
            Terminal::Attribute(AttributeKind::QNameUntyped(_)) => "ATTRIBUTE_INVALID_VALUE",
            Terminal::Attribute(AttributeKind::NamespaceWildcard(_)) => "ATTRIBUTE_NS",
            Terminal::Attribute(AttributeKind::Wildcard) => "ATTRIBUTE_GENERIC_UNDECLARED",
            Terminal::Attribute(AttributeKind::WildcardUntyped) => "ATTRIBUTE_ANY_INVALID_VALUE",
            Terminal::EndElement => "END_ELEMENT",
            Terminal::Characters | Terminal::CharactersUntyped => "CHARACTERS",
            Terminal::Comment => "COMMENT",
            Terminal::ProcessingInstr => "PROCESSING_INSTRUCTION",
            Terminal::NamespaceDecl => "NAMESPACE_DECLARATION",
            Terminal::SelfContained => "SELF_CONTAINED",
            Terminal::DocType => "DOC_TYPE",
            Terminal::EntityRef => "ENTITY_REFERENCE",
        }
    }
}

/// Eine Grammar-Production mit Event Code und optionalem Übergang.
///
/// # Invarianten
///
/// - `right_hand_side = None`: Terminale Production (EE, ED) - keine Weiterverarbeitung
/// - `right_hand_side = Some(id)`: Nach diesem Event wird mit NonTerminal `id` fortgefahren
///
/// # Spec-Referenz
/// - 8. EXI Grammars
/// - 8.1 Grammar Notation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Production {
    /// Das Terminal-Symbol (Event-Typ)
    pub terminal: Terminal,
    /// Der Event Code für diese Production (None vor Event Code Assignment)
    pub event_code: Option<EventCode>,
    /// Das NonTerminal auf der rechten Seite (None = terminal, keine Weiterverarbeitung)
    pub right_hand_side: Option<NonTerminalId>,
}

impl Production {
    /// Erstellt eine neue Production mit Event Code.
    ///
    /// # Parameter
    ///
    /// - `terminal`: Das Event-Terminal
    /// - `event_code`: Der Event Code
    /// - `right_hand_side`: Das nächste NonTerminal (None für terminale Productions wie EE, ED)
    pub fn new(
        terminal: Terminal,
        event_code: EventCode,
        right_hand_side: Option<NonTerminalId>,
    ) -> Self {
        Self {
            terminal,
            event_code: Some(event_code),
            right_hand_side,
        }
    }

    /// Erstellt eine neue Production ohne Event Code.
    ///
    /// Event Code wird später durch `assign_event_codes()` oder `recalculate_event_codes()` gesetzt.
    pub fn without_event_code(
        terminal: Terminal,
        right_hand_side: Option<NonTerminalId>,
    ) -> Self {
        Self {
            terminal,
            event_code: None,
            right_hand_side,
        }
    }

    /// Erstellt eine terminale Production (ohne Übergang).
    ///
    /// Convenience-Methode für Productions wie EE und ED.
    pub fn terminal(terminal: Terminal, event_code: EventCode) -> Self {
        Self::new(terminal, event_code, None)
    }

    /// Erstellt eine terminale Production ohne Event Code.
    pub fn terminal_without_event_code(terminal: Terminal) -> Self {
        Self::without_event_code(terminal, None)
    }

    /// Gibt den Event Code zurück (panics wenn None).
    ///
    /// Nur für Tests und Stellen wo der Event Code garantiert gesetzt ist.
    #[cfg(test)]
    pub fn event_code_unwrap(&self) -> &EventCode {
        self.event_code
            .as_ref()
            .expect("Event Code sollte gesetzt sein")
    }
}

// ============================================================================
// NonTerminal (Spec 8.4)
// ============================================================================

/// Ein NonTerminal mit zugehörigen Productions.
///
/// Ein NonTerminal repräsentiert einen Zustand in der Grammar-Maschine.
/// Von jedem NonTerminal gehen mehrere Productions aus, die verschiedene
/// mögliche Events beschreiben.
///
/// # Spec-Referenz
///
/// - 8.4 Built-in XML Grammars
#[derive(Debug, Clone)]
pub struct NonTerminal {
    /// Eindeutiger Identifier
    id: NonTerminalId,
    /// Liste der Productions (Originals zuerst, gelernte am Ende)
    productions: Vec<Production>,
    /// Anzahl gelernter Productions am Ende des Vec (Offset für Event Codes).
    ///
    /// Layout: `[original_0..original_m, learned_oldest..learned_newest]`
    /// Gelernte Productions (ab Index original_count) haben effektiven Code:
    /// neueste = 0, älteste = learned_count-1.
    /// Original-Productions (Index 0..original_count-1) haben effektiven
    /// Code = gespeicherter Code + learned_count.
    learned_count: u32,
    /// Gecachter EventCodeContext (wird bei jeder Mutation aktualisiert).
    cached_context: EventCodeContext,
    /// Gecachter EventCodeContext nur für Productions mit Event-Code-Länge 1.
    cached_context_l1: EventCodeContext,
    /// Terminal → Productions-Index Lookup (O(1) statt O(n) Scan).
    /// Nur für Original-Productions — gelernte Productions werden zuerst
    /// in der gelernten Zone gesucht (rückwärts, wenige Einträge).
    terminal_index: FastHashMap<Terminal, usize>,
    /// Array-basierte ProductionTable für O(1) Event Code Lookup (Decoder).
    /// `None` während der Konstruktionsphase, `Some` nach `seal_production_table()`.
    production_table: Option<Rc<ProductionTable>>,
    /// Precomputed: EndElement mit Event-Code-Länge 1 vorhanden?
    /// Monoton: wird in `prepend_learned_production()` auf `true` geflipt.
    has_ee_length_one: bool,
}

/// Hilfs-API für Grammar Evolution (zentraler Mutationspfad).
pub struct GrammarEvolution;

impl GrammarEvolution {
    #[inline]
    pub fn learn_fragment_se(nt: &mut NonTerminal, terminal: Terminal) {
        nt.learn_production(terminal, Some(NonTerminalId::FragmentContent));
    }

    #[inline]
    pub fn learn_element_se(nt: &mut NonTerminal, terminal: Terminal) {
        nt.learn_production(terminal, Some(NonTerminalId::ElementContent));
    }

    #[inline]
    pub fn learn_attribute(
        stc: &mut NonTerminal,
        terminal: Terminal,
        interner: &mut StringInterner,
    ) {
        stc.learn_attribute(terminal, Some(NonTerminalId::StartTagContent), interner);
    }

    #[inline]
    pub fn learn_content(
        nt: &mut NonTerminal,
        terminal: Terminal,
        next_nt: Option<NonTerminalId>,
    ) {
        nt.learn_content_production(terminal, next_nt);
    }
}

/// Ab dieser Schwelle wird für Original-Productions der HashMap-Index statt
/// linearem Scan verwendet. Für kleine Grammars (≤ Schwelle) ist linearer Scan
/// schneller (kein Hashing-Overhead, bessere Cache-Lokalität).
const LINEAR_SCAN_THRESHOLD: usize = 16;

impl NonTerminal {
    /// Erstellt ein neues NonTerminal.
    pub(crate) fn new(id: NonTerminalId, productions: Vec<Production>) -> Self {
        let cached_context = compute_event_code_context(&productions);
        let cached_context_l1 = compute_context_length_one(&productions);
        let terminal_index = build_terminal_index(&productions);
        // Bei learned_count=0 genügt ein Index-Lookup in der Original-Zone.
        let has_ee_length_one = terminal_index.get(&Terminal::EndElement)
            .and_then(|&idx| productions[idx].event_code.as_ref())
            .is_some_and(|ec| ec.length() == 1);
        Self {
            id,
            productions,
            learned_count: 0,
            cached_context,
            cached_context_l1,
            terminal_index,
            production_table: None,
            has_ee_length_one,
        }
    }

    /// Gibt den Identifier zurück.
    pub fn id(&self) -> NonTerminalId {
        self.id
    }

    /// Gibt die Productions als Slice zurück.
    pub fn productions(&self) -> &[Production] {
        &self.productions
    }

    /// Gibt die Anzahl gelernter (am Ende gespeicherter) Productions zurück.
    pub fn learned_count(&self) -> u32 {
        self.learned_count
    }

    /// Berechnet den effektiven Event Code für die Production am Index `idx`.
    ///
    /// Layout: `[original_0..original_m, learned_oldest..learned_newest]`
    /// - Original-Productions (idx < original_count): Code = gespeicherter Code + learned_count
    /// - Gelernte Productions (idx >= original_count): Neueste hat Code 0, älteste lc-1
    pub fn effective_event_code(&self, idx: usize) -> Option<EventCode> {
        let lc = self.learned_count;
        let oc = self.productions.len() - lc as usize;
        if idx < oc {
            // Original-Production: gespeicherter Code + Offset
            self.productions[idx].event_code.as_ref().map(|ec| ec.with_part1_offset(lc))
        } else {
            // Gelernte Production: Neueste (am Ende) hat Code 0
            let learned_pos = idx - oc; // 0 = älteste, lc-1 = neueste
            Some(EventCode::one(lc - 1 - learned_pos as u32))
        }
    }

    /// Aktualisiert beide Cache-Felder nach einer Mutation.
    fn recompute_cache(&mut self) {
        self.cached_context = compute_event_code_context(&self.productions);
        self.cached_context_l1 = compute_context_length_one(&self.productions);
        // Terminal-Index nur für Original-Zone neu bauen (gelernte am Ende)
        let oc = self.productions.len() - self.learned_count as usize;
        self.terminal_index = build_terminal_index(&self.productions[..oc]);
        self.has_ee_length_one = self.has_terminal_with_length_one(&Terminal::EndElement);
        self.rebuild_production_table();
    }

    /// Fügt eine Production am Ende hinzu und aktualisiert den Cache.
    pub fn push_production(&mut self, prod: Production) {
        self.productions.push(prod);
        self.recompute_cache();
    }

    /// Entfernt Productions die das Prädikat nicht erfüllen und aktualisiert den Cache.
    pub fn retain_productions<F>(&mut self, f: F)
    where
        F: FnMut(&Production) -> bool,
    {
        self.productions.retain(f);
        self.recompute_cache();
    }

    /// Fügt eine Production an der gegebenen Position ein und aktualisiert den Cache.
    pub fn insert_production(&mut self, index: usize, prod: Production) {
        self.productions.insert(index, prod);
        self.recompute_cache();
    }

    /// Berechnet Event Codes nach retain/insert neu und aktualisiert den Cache.
    pub fn recalculate_event_codes(&mut self) {
        recalculate_event_codes(&mut self.productions);
        self.recompute_cache();
    }

    /// Setzt die RHS aller Wildcard-Attribute auf die gegebene ID.
    ///
    /// RHS-Aenderungen beeinflussen Event Codes nicht, aber die ProductionTable
    /// enthält (Terminal, RHS) Tupel → Rebuild noetig.
    pub fn update_wildcard_rhs(&mut self, id: NonTerminalId) {
        for prod in &mut self.productions {
            if matches!(
                prod.terminal,
                Terminal::Attribute(
                    AttributeKind::Wildcard | AttributeKind::NamespaceWildcard(_)
                )
            ) {
                prod.right_hand_side = Some(id);
            }
        }
        self.rebuild_production_table();
    }

    /// Findet eine Production anhand ihres Terminals.
    ///
    /// Gibt die erste übereinstimmende Production zurück.
    /// **Hinweis:** Der gespeicherte `event_code` ist bei gelernten Grammars
    /// NICHT der effektive Code. Verwende `find_by_terminal_with_code()` wenn
    /// der effektive Event Code benötigt wird.
    pub fn find_by_terminal(&self, terminal: &Terminal) -> Option<&Production> {
        self.productions.iter().find(|p| &p.terminal == terminal)
    }

    /// Findet eine Production anhand ihres Terminals und gibt den effektiven Event Code zurück.
    ///
    /// Berücksichtigt den `learned_count`-Offset:
    /// - Original-Productions (Index < original_count): Code = gespeicherter Code + learned_count
    /// - Gelernte Productions (Index >= original_count): Neueste hat Code 0
    pub fn find_by_terminal_with_code(&self, terminal: &Terminal) -> Option<(&Production, EventCode)> {
        let lc = self.learned_count as usize;
        let oc = self.productions.len() - lc;

        // Phase 1: Gelernte Productions (rückwärts, neueste zuerst).
        // Wenige Einträge — linearer Scan ist hier OK.
        for idx in (oc..self.productions.len()).rev() {
            if &self.productions[idx].terminal == terminal {
                if let Some(ec) = self.effective_event_code(idx) {
                    return Some((&self.productions[idx], ec));
                }
            }
        }

        // Phase 2: Original-Zone.
        // Für kleine Grammars (≤ 16 Productions): linearer Scan statt HashMap —
        // spart Hashing-Overhead und nutzt Cache-Lokalität.
        if oc <= LINEAR_SCAN_THRESHOLD {
            for idx in 0..oc {
                if &self.productions[idx].terminal == terminal {
                    if let Some(ec) = self.effective_event_code(idx) {
                        return Some((&self.productions[idx], ec));
                    }
                }
            }
        } else if let Some(&idx) = self.terminal_index.get(terminal) {
            if let Some(ec) = self.effective_event_code(idx) {
                return Some((&self.productions[idx], ec));
            }
        }

        None
    }

    /// Findet eine Production per Predicate und gibt den effektiven Event Code zurück.
    ///
    /// Berücksichtigt den `learned_count`-Offset:
    /// - Original-Productions (Index < original_count): Code = gespeicherter Code + learned_count
    /// - Gelernte Productions (Index >= original_count): Neueste hat Code 0
    ///
    /// Suche in zwei Phasen:
    /// 1. Gelernte Zone (rückwärts, neueste zuerst) — bevorzugt gelernte
    ///    Productions bei duplizierten Terminals (CH, EE nach `learn_content_production`)
    /// 2. Original-Zone (vorwärts) — unveränderte Reihenfolge
    pub fn find_by_predicate_with_code(
        &self,
        predicate: impl Fn(&Terminal) -> bool,
    ) -> Option<(&Production, EventCode)> {
        let lc = self.learned_count as usize;
        let oc = self.productions.len() - lc;
        // Phase 1: Gelernte Productions (rückwärts, neueste zuerst).
        // Bei lc == 0 ist dieser Bereich leer.
        for idx in (oc..self.productions.len()).rev() {
            if predicate(&self.productions[idx].terminal) {
                if let Some(ec) = self.effective_event_code(idx) {
                    return Some((&self.productions[idx], ec));
                }
            }
        }
        // Phase 2: Original-Productions (vorwärts)
        for (idx, p) in self.productions[..oc].iter().enumerate() {
            if predicate(&p.terminal) {
                if let Some(ec) = self.effective_event_code(idx) {
                    return Some((p, ec));
                }
            }
        }
        None
    }

    /// Findet eine Production anhand ihres Event Codes (offset-aware).
    ///
    /// Layout: `[original_0..original_m, learned_oldest..learned_newest]`
    /// - Gelernte Codes (length==1, part1 < lc): Code k → Index len - 1 - k
    /// - Andere Codes: part1 wird um learned_count reduziert, dann in Originals gesucht
    pub fn find_by_event_code(&self, event_code: &EventCode) -> Option<&Production> {
        let lc = self.learned_count;
        if lc == 0 {
            // Fast path: kein Offset nötig
            return self.productions
                .iter()
                .find(|p| p.event_code.as_ref() == Some(event_code));
        }

        let oc = self.productions.len() - lc as usize;

        // Gelernte Productions: Code k → physischer Index = len - 1 - k
        //   (da oc + lc == len, vereinfacht sich oc + lc - 1 - k zu len - 1 - k)
        if event_code.length() == 1 && event_code.part1() < lc {
            let physical = self.productions.len() - 1 - event_code.part1() as usize;
            return Some(&self.productions[physical]);
        }

        // Original-Productions: Code decrementiert, suche in [0..oc]
        let adjusted = event_code.with_part1_decremented(lc)?;
        self.productions[..oc]
            .iter()
            .find(|p| p.event_code.as_ref() == Some(&adjusted))
    }

    /// Sammelt Schema-deklarierte Attribute (ohne xsi:type/xsi:nil),
    /// sortiert nach (uri, local_name).
    ///
    /// Wird fuer das 3-Part Event-Code-System (Spec 8.5.4.4.1) benoetigt,
    /// um deklarierte Attribute von Wildcard-Attributen zu unterscheiden.
    /// Konsistent mit Exificient `getNumberOfDeclaredAttributes()`.
    pub fn declared_attributes(&self, interner: &StringInterner) -> Vec<(ExpandedNameId, Option<NonTerminalId>)> {
        let mut attrs: Vec<(ExpandedNameId, Option<NonTerminalId>)> = self
            .productions
            .iter()
            .filter_map(|p| {
                if let Terminal::Attribute(AttributeKind::QName(q)) = p.terminal {
                    let (uri, local) = q.resolve(interner);
                    if uri == URI_XSI && (local == "type" || local == "nil") {
                        return None;
                    }
                    Some((q, p.right_hand_side))
                } else {
                    None
                }
            })
            .collect();
        attrs.sort_by(|a, b| {
            let (a_uri, a_local) = a.0.resolve(interner);
            let (b_uri, b_local) = b.0.resolve(interner);
            (a_uri, a_local).cmp(&(b_uri, b_local))
        });
        attrs
    }

    /// Gibt den gecachten EventCodeContext zurück (O(1)).
    ///
    /// # Spec-Referenz
    /// - 6.2 Representing Event Codes
    pub fn event_code_context(&self) -> &EventCodeContext {
        &self.cached_context
    }

    /// Gibt den gecachten EventCodeContext nur für Productions mit Event-Code-Länge 1 zurück (O(1)).
    ///
    /// Einige Streams nutzen Tier2-Escape und rechnen die
    /// Bitbreite von Part1 nur mit schema-deklarierten Productions (Laenge 1).
    pub fn event_code_context_length_one(&self) -> &EventCodeContext {
        &self.cached_context_l1
    }

    /// Ob EndElement mit Event-Code-Länge 1 vorhanden ist (precomputed, O(1)).
    ///
    /// Schneller Spezialfall von `has_terminal_with_length_one(&Terminal::EndElement)`.
    pub fn has_ee_length_one(&self) -> bool {
        self.has_ee_length_one
    }

    /// Ob dieses NonTerminal im Content-Bereich ist (keine Attribute mehr erlaubt).
    ///
    /// Prüft ob in den Productions noch Attribute-Terminals vorhanden sind.
    /// Wird für Tier2-Cache-Key-Berechnung in Encoder/Decoder verwendet.
    pub fn is_content_area(&self) -> bool {
        !self.productions.iter().any(|p| matches!(p.terminal, Terminal::Attribute(_)))
    }

    /// Prüft ob eine Production mit diesem Terminal existiert.
    ///
    /// Wird bei Fragment Grammar Evolution verwendet um zu prüfen,
    /// ob ein Element bereits gelernt wurde.
    ///
    /// # Spec-Referenz
    ///
    /// - 8.4.2: "If [...] this is the first occurrence of the element..."
    pub fn has_terminal(&self, terminal: &Terminal) -> bool {
        let oc = self.productions.len() - self.learned_count as usize;
        // Gelernte Zone: linearer Scan (typisch 1–10 Einträge pro NonTerminal)
        if self.productions[oc..].iter().any(|p| &p.terminal == terminal) {
            return true;
        }
        // Original-Zone: Linearer Scan für kleine Grammars, HashMap für große.
        if oc <= LINEAR_SCAN_THRESHOLD {
            self.productions[..oc].iter().any(|p| &p.terminal == terminal)
        } else {
            self.terminal_index.contains_key(terminal)
        }
    }

    /// Prüft ob eine Production mit Terminal und Event Code Length 1 existiert.
    ///
    /// Wird bei Element Grammar Evolution für CH und EE Lernen verwendet.
    /// Die Spec unterscheidet zwischen "exists" und "exists with event code of length 1".
    ///
    /// # Beispiel
    ///
    /// - StartTagContent hat EE [0.0] (Length 2) → false
    /// - ElementContent hat EE [0] (Length 1) → true
    ///
    /// # Spec-Referenz
    ///
    /// - 8.4.3: "If a production of the form, LeftHandSide : CH RightHandSide
    ///   with an event code of length 1 does not exist..."
    /// - 8.4.3: "If a production of the form, LeftHandSide : EE
    ///   with an event code of length 1 does not exist..."
    pub fn has_terminal_with_length_one(&self, terminal: &Terminal) -> bool {
        // Gelernte Zone: alle gelernten haben Event Code Length 1 per Definition
        let oc = self.productions.len() - self.learned_count as usize;
        if self.productions[oc..].iter().any(|p| &p.terminal == terminal) {
            return true;
        }
        // Original-Zone: Linearer Scan für kleine Grammars, HashMap für große.
        if oc <= LINEAR_SCAN_THRESHOLD {
            self.productions[..oc].iter()
                .find(|p| &p.terminal == terminal)
                .and_then(|p| p.event_code.as_ref())
                .is_some_and(|ec| ec.length() == 1)
        } else if let Some(&idx) = self.terminal_index.get(terminal) {
            self.productions[idx]
                .event_code
                .as_ref()
                .is_some_and(|ec| ec.length() == 1)
        } else {
            false
        }
    }

    /// Baut die eingebettete ProductionTable (am Ende der Grammar-Konstruktion aufrufen).
    fn seal_production_table(&mut self) {
        self.production_table = Some(Rc::new(build_production_table(self)));
    }

    /// Baut die eingebettete ProductionTable nach einer Mutation neu.
    /// Nur wenn bereits gesealt (Konstruktionsphase: kein Rebuild).
    fn rebuild_production_table(&mut self) {
        if self.production_table.is_some() {
            self.production_table = Some(Rc::new(build_production_table(self)));
        }
    }

    /// Inkrementelles Update der ProductionTable beim Lernen einer neuen Production.
    ///
    /// Statt Full-Rebuild: neuen Eintrag an Position 0 einfügen (prepend_level1).
    /// Fallback auf Full-Rebuild bei shared Rc (strong_count > 1).
    fn update_production_table_incremental(
        &mut self,
        new_terminal: Terminal,
        new_rhs: Option<NonTerminalId>,
    ) {
        let Some(ref mut table_rc) = self.production_table else {
            debug_assert!(false, "update_production_table_incremental aufgerufen ohne sealed ProductionTable");
            return;
        };

        let entry = ProductionEntry { terminal: new_terminal, rhs: new_rhs };
        if Rc::strong_count(table_rc) == 1 {
            Rc::make_mut(table_rc).prepend_level1(entry);
        } else {
            self.production_table = Some(Rc::new(build_production_table(self)));
        }
    }

    /// Gibt die eingebettete ProductionTable zurück (nach Seal immer vorhanden).
    pub(crate) fn production_table(&self) -> &Rc<ProductionTable> {
        self.production_table.as_ref().unwrap_or_else(|| {
            panic!(
                "production_table() aufgerufen ohne vorheriges seal_production_table() \
                 für NonTerminal {:?}",
                self.id
            )
        })
    }

    /// Fügt eine gelernte Production am Ende ein (Spec 8.4.2 Schritt 3-5).
    ///
    /// Spec-Semantik:
    /// 1. Erstellt Production mit Event Code 0
    /// 2. Inkrementiert alle bestehenden Event Codes (first part +1)
    /// 3. Fügt Production in die Grammar ein
    ///
    /// Implementierung: Kein expliziter Inkrement-Loop. Der Offset wird
    /// virtuell über `learned_count` verwaltet (O(1) statt O(n)).
    ///
    /// # Layout
    ///
    /// Gelernte Productions werden am **Ende** des Vec gespeichert:
    /// `[original_0, ..., original_m, learned_oldest, ..., learned_newest]`
    /// Die effektive Code-Zuordnung erfolgt über `effective_event_code()`.
    ///
    /// Der Name "prepend" bezieht sich auf die Spec-Semantik (Event Code 0 wird
    /// logisch an erster Stelle eingefügt), nicht auf die physische Speicherung.
    ///
    /// # Wichtig
    ///
    /// Vor dem Aufruf mit `has_terminal()` prüfen, ob das Element bereits
    /// gelernt wurde. Bei bereits gelernten Elementen KEINE Mutation.
    ///
    /// # Spec-Referenz
    ///
    /// - 8.4.2 Built-in Fragment Grammar:
    ///   "3. Create a production [...] with an event code of 0."
    ///   "4. Increment the first part of the event code of each production [...]"
    ///   "5. Add the production [...] to the current grammar."
    pub fn prepend_learned_production(
        &mut self,
        terminal: Terminal,
        right_hand_side: Option<NonTerminalId>,
    ) {
        // Monotonic flip: gelernte Productions haben immer Länge 1
        if terminal == Terminal::EndElement {
            self.has_ee_length_one = true;
        }

        // Schritt 3+5: Neue Production mit Event Code 0 am Ende anfügen (O(1)).
        // KEIN Inkrement-Loop: Offset wird über learned_count verwaltet.
        self.productions
            .push(Production::new(terminal, EventCode::one(0), right_hand_side));
        self.learned_count += 1;

        // Inkrementeller Cache-Update (O(1) statt O(n) recompute_cache):
        self.cached_context.increment_part1_count();
        self.cached_context_l1.increment_part1_count();

        // Inkrementelles ProductionTable-Update NACH dem Push
        if self.production_table.is_some() {
            let new_terminal = self.productions.last().unwrap().terminal;
            self.update_production_table_incremental(new_terminal, right_hand_side);
        }
    }

    /// Lernt eine neue Production, wenn das Terminal noch nicht existiert.
    ///
    /// Convenience-Methode die `has_terminal()` intern prüft und nur bei
    /// neuem Terminal die Grammar mutiert.
    ///
    /// # Rückgabewert
    ///
    /// - `true`: Terminal war neu, Production wurde hinzugefügt
    /// - `false`: Terminal bereits vorhanden, keine Änderung
    ///
    /// # Beispiel
    ///
    /// ```ignore
    /// let book = Terminal::StartElement(StartElementKind::QName(qname));
    /// if nt.learn_production(book, Some(NonTerminalId::FragmentContent)) {
    ///     // Erstes Auftreten von SE(book)
    /// }
    /// ```
    ///
    /// # Spec-Referenz
    ///
    /// - 8.4.2: "If [...] this is the first occurrence of the element..."
    pub fn learn_production(
        &mut self,
        terminal: Terminal,
        right_hand_side: Option<NonTerminalId>,
    ) -> bool {
        if self.has_terminal(&terminal) {
            return false;
        }
        if self.productions.len() >= MAX_LEARNED_PRODUCTIONS {
            return false;
        }
        self.prepend_learned_production(terminal, right_hand_side);
        true
    }

    /// Lernt CH oder EE wenn noch keine mit Code-Length 1 existiert.
    ///
    /// Unterschied zu `learn_production`:
    /// - Prüft auf Code-Length 1, nicht auf Terminal-Existenz
    /// - CH/EE mit Code-Length 2+ triggert trotzdem Lernen
    ///
    /// # Verwendung
    ///
    /// Bei Element Grammar Evolution: Wenn CH oder EE mit 2-Teil-Code matched,
    /// wird eine neue Production mit 1-Teil-Code gelernt.
    ///
    /// # Rückgabewert
    ///
    /// - `true`: Keine Production mit Length 1 existierte, neue wurde hinzugefügt
    /// - `false`: Production mit Length 1 existiert bereits, keine Änderung
    ///
    /// # Spec-Referenz
    ///
    /// - 8.4.3: "If a production of the form, LeftHandSide : CH RightHandSide
    ///   with an event code of length 1 does not exist in the current element
    ///   grammar, create one with event code 0 and increment the first part of
    ///   the event code of each production..."
    /// - 8.4.3: "If a production of the form, LeftHandSide : EE
    ///   with an event code of length 1 does not exist..."
    pub fn learn_content_production(
        &mut self,
        terminal: Terminal,
        right_hand_side: Option<NonTerminalId>,
    ) -> bool {
        if self.has_terminal_with_length_one(&terminal) {
            return false;
        }
        if self.productions.len() >= MAX_LEARNED_PRODUCTIONS {
            return false;
        }
        self.prepend_learned_production(terminal, right_hand_side);
        true
    }

    /// Lernt ein Attribut (AT(qname)) mit xsi:type Sonderbehandlung.
    ///
    /// # Logik
    ///
    /// - Normale Attribute: Nur lernen wenn Terminal noch nicht existiert
    /// - xsi:type: Nur lernen wenn noch keine AT(xsi:type) mit Code-Length 1 existiert
    ///
    /// # Rückgabewert
    ///
    /// - `true`: Attribut wurde gelernt
    /// - `false`: Bereits vorhanden, keine Änderung
    ///
    /// # Spec-Referenz
    ///
    /// - 8.4.3: "If qname is not xsi:type or if a production of the form
    ///   LeftHandSide : AT (xsi:type) with an event code of length 1 does not
    ///   exist in the current element grammar, create a production of the form
    ///   LeftHandSide : AT (qname) RightHandSide with an event code 0..."
    pub fn learn_attribute(
        &mut self,
        terminal: Terminal,
        right_hand_side: Option<NonTerminalId>,
        interner: &StringInterner,
    ) -> bool {
        debug_assert!(
            matches!(terminal, Terminal::Attribute(_)),
            "learn_attribute erwartet Terminal::Attribute, bekam {:?}",
            terminal
        );

        // xsi:type Sonderfall: Prüfe auf Code-Length 1 (Spec 8.4.3)
        if let Terminal::Attribute(AttributeKind::QName(q)) = terminal {
            let (uri, local) = q.resolve(interner);
            if uri == URI_XSI && local == "type" {
                return self.learn_content_production(terminal, right_hand_side);
            }
        }

        // Normale Attribute: Prüfe auf Terminal-Existenz
        self.learn_production(terminal, right_hand_side)
    }
}

// ============================================================================
// EventCodeFormula (Spec 8.1.2)
// ============================================================================

/// Formel für variable Event Codes in Macro-Expansionen.
///
/// In der Spec-Notation werden variable Event Codes mit `n.m`, `n.(m+k)` etc.
/// dargestellt. Diese Formeln werden bei Macro-Expansion ausgewertet.
///
/// # Spec-Referenz
///
/// - 8.1.2 Variable Event Codes
/// - 8.4.3 Built-in Element Grammar (ChildContentItems)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventCodeFormula {
    /// Event Code n.m+offset (2 Teile)
    ///
    /// Evaluiert zu `EventCode::two(n, m + offset)`
    TwoPart { m_offset: u32 },

    /// Event Code n.(m+offset).third (3 Teile)
    ///
    /// Evaluiert zu `EventCode::three(n, m + offset, third)`
    ThreePart { m_offset: u32, third: u32 },
}

impl EventCodeFormula {
    /// Evaluiert die Formel mit gegebenen n und m Werten.
    ///
    /// # Parameter
    ///
    /// - `n`: Wert für ersten Teil (aus dem Kontext)
    /// - `m`: Basis-Wert für zweiten Teil (aus dem Kontext)
    ///
    /// # Rückgabe
    ///
    /// Evaluierter EventCode
    ///
    /// # Spec-Referenz
    ///
    /// - 8.1.2: "Unless otherwise specified, the variable n evaluates to
    ///   the first part of the event code..."
    pub fn evaluate(&self, n: u32, m: u32) -> EventCode {
        match *self {
            Self::TwoPart { m_offset } => EventCode::two(n, m + m_offset),
            Self::ThreePart { m_offset, third } => EventCode::three(n, m + m_offset, third),
        }
    }
}

// ============================================================================
// MacroProduction (Spec 8.1.2)
// ============================================================================

/// Eine Production innerhalb eines Macro-Templates.
///
/// Enthält eine Formel statt eines fixen Event Codes.
#[derive(Debug, Clone)]
pub struct MacroProduction {
    /// Das Terminal-Symbol
    pub terminal: Terminal,
    /// Die Event Code Formel
    pub formula: EventCodeFormula,
    /// Das NonTerminal auf der rechten Seite
    pub right_hand_side: NonTerminalId,
}

impl MacroProduction {
    /// Erstellt eine neue MacroProduction.
    pub fn new(
        terminal: Terminal,
        formula: EventCodeFormula,
        right_hand_side: NonTerminalId,
    ) -> Self {
        Self {
            terminal,
            formula,
            right_hand_side,
        }
    }

    /// Expandiert diese MacroProduction mit gegebenen n und m Werten.
    pub fn expand(&self, n: u32, m: u32) -> Production {
        Production::new(
            self.terminal.clone(),
            self.formula.evaluate(n, m),
            Some(self.right_hand_side),
        )
    }
}

// ============================================================================
// MacroTemplate (Spec 8.1.2)
// ============================================================================

/// Ein Macro-Template für wiederverwendbare Production-Gruppen.
///
/// Macros werden in der Spec als NonTerminal-Symbole mit Variablen verwendet,
/// z.B. `ChildContentItems(n.m)`. Bei der Expansion werden die Variablen
/// durch konkrete Werte ersetzt.
///
/// # Spec-Referenz
///
/// - 8.1.2 Variable Event Codes
/// - 8.4.3 Built-in Element Grammar (ChildContentItems)
#[derive(Debug, Clone)]
pub struct MacroTemplate {
    /// Die Macro-Productions mit Formeln
    productions: Vec<MacroProduction>,
}

impl MacroTemplate {
    /// Erstellt ein neues MacroTemplate.
    pub fn new(productions: Vec<MacroProduction>) -> Self {
        Self { productions }
    }

    /// Erstellt das ChildContentItems Macro (Spec 8.4.3).
    ///
    /// ```text
    /// ChildContentItems(n.m):
    ///   SE(*) ElementContent  n.m
    ///   CH ElementContent     n.(m+1)
    ///   ER ElementContent     n.(m+2)       (nur wenn preserve.dtd=true)
    ///   CM ElementContent     n.(m+3).0     (nur wenn preserve.comments=true)
    ///   PI ElementContent     n.(m+3).1     (nur wenn preserve.pis=true)
    /// ```
    ///
    /// Die Fidelity-abhängigen Productions (ER, CM, PI) werden durch
    /// `GrammarSystem::prune()` entfernt, wenn die entsprechenden Options false sind.
    ///
    /// # Spec-Referenz
    ///
    /// - 8.4.3 Built-in Element Grammar
    pub fn child_content_items() -> Self {
        Self::new(vec![
            MacroProduction::new(
                Terminal::StartElement(StartElementKind::Wildcard),
                EventCodeFormula::TwoPart { m_offset: 0 },
                NonTerminalId::ElementContent,
            ),
            MacroProduction::new(
                Terminal::Characters,
                EventCodeFormula::TwoPart { m_offset: 1 },
                NonTerminalId::ElementContent,
            ),
            MacroProduction::new(
                Terminal::EntityRef,
                EventCodeFormula::TwoPart { m_offset: 2 },
                NonTerminalId::ElementContent,
            ),
            MacroProduction::new(
                Terminal::Comment,
                EventCodeFormula::ThreePart {
                    m_offset: 3,
                    third: 0,
                },
                NonTerminalId::ElementContent,
            ),
            MacroProduction::new(
                Terminal::ProcessingInstr,
                EventCodeFormula::ThreePart {
                    m_offset: 3,
                    third: 1,
                },
                NonTerminalId::ElementContent,
            ),
        ])
    }

    /// Expandiert das Macro mit gegebenen n und m Werten.
    ///
    /// Gibt alle Productions mit evaluierten Event Codes zurück.
    pub fn expand(&self, n: u32, m: u32) -> Vec<Production> {
        self.productions.iter().map(|p| p.expand(n, m)).collect()
    }
}

// ============================================================================
// NtOffsets — O(1) Lookup-Tabelle für NonTerminalId → Vec-Index
// ============================================================================

/// Vorberechnete Offsets für O(1) NonTerminalId → Vec-Index.
///
/// Feste Varianten werden per Ordinal in `fixed` nachgeschlagen (u16::MAX = nicht vorhanden).
/// Sequentielle Bereiche (Dynamic, SchemaType) nutzen `base + n`.
/// Content2 ist sparse (1-2 Einträge pro Grammar) und wird inline gespeichert.
#[derive(Debug, Clone, Copy)]
struct NtOffsets {
    /// Vec-Index für feste Varianten (Document..ElementFragmentTypeEmpty1).
    /// u16::MAX = nicht vorhanden.
    fixed: [u16; 12],
    /// Basis-Index für Dynamic(n).
    dynamic_base: u16,
    /// Anzahl Dynamic-Einträge.
    dynamic_count: u16,
    /// Basis-Index für SchemaType(n).
    schema_type_base: u16,
    /// Anzahl SchemaType-Einträge.
    schema_type_count: u16,
    /// Content2(n) → Vec-Index Paare (sparse, max. 4 pro Grammar).
    content2: [(u16, u16); 4],
    /// Anzahl Content2-Einträge.
    content2_len: u8,
}

impl NtOffsets {
    /// Baut die Offset-Tabelle aus einem sortierten NonTerminal-Vec.
    fn build(non_terminals: &[NonTerminal]) -> Self {
        let mut offsets = NtOffsets {
            fixed: [u16::MAX; 12],
            dynamic_base: 0,
            dynamic_count: 0,
            schema_type_base: 0,
            schema_type_count: 0,
            content2: [(0, 0); 4],
            content2_len: 0,
        };

        for (idx, nt) in non_terminals.iter().enumerate() {
            let idx_u16 = idx as u16;
            match nt.id() {
                NonTerminalId::Document => offsets.fixed[0] = idx_u16,
                NonTerminalId::DocContent => offsets.fixed[1] = idx_u16,
                NonTerminalId::DocEnd => offsets.fixed[2] = idx_u16,
                NonTerminalId::Fragment => offsets.fixed[3] = idx_u16,
                NonTerminalId::FragmentContent => offsets.fixed[4] = idx_u16,
                NonTerminalId::StartTagContent => offsets.fixed[5] = idx_u16,
                NonTerminalId::ElementContent => offsets.fixed[6] = idx_u16,
                NonTerminalId::ElementFragment0 => offsets.fixed[7] = idx_u16,
                NonTerminalId::ElementFragment1 => offsets.fixed[8] = idx_u16,
                NonTerminalId::ElementFragment2 => offsets.fixed[9] = idx_u16,
                NonTerminalId::ElementFragmentTypeEmpty0 => offsets.fixed[10] = idx_u16,
                NonTerminalId::ElementFragmentTypeEmpty1 => offsets.fixed[11] = idx_u16,
                NonTerminalId::Dynamic(n) => {
                    if n == 0 {
                        offsets.dynamic_base = idx_u16;
                    }
                    offsets.dynamic_count = offsets.dynamic_count.max(n as u16 + 1);
                }
                NonTerminalId::SchemaType(n) => {
                    if n == 0 {
                        offsets.schema_type_base = idx_u16;
                    }
                    offsets.schema_type_count = offsets.schema_type_count.max(n as u16 + 1);
                }
                NonTerminalId::Content2(n) => {
                    let len = offsets.content2_len as usize;
                    assert!(len < 4, "NtOffsets: mehr als 4 Content2-Einträge \
                        (Content2({n}) an Index {idx}). Array-Größe erhöhen.");
                    offsets.content2[len] = (n as u16, idx_u16);
                    offsets.content2_len += 1;
                }
            }
        }

        offsets
    }

    /// O(1) Lookup: NonTerminalId → Vec-Index.
    #[inline]
    fn index_of(&self, id: NonTerminalId) -> Option<usize> {
        match id {
            NonTerminalId::Document => Self::fixed_lookup(self.fixed[0]),
            NonTerminalId::DocContent => Self::fixed_lookup(self.fixed[1]),
            NonTerminalId::DocEnd => Self::fixed_lookup(self.fixed[2]),
            NonTerminalId::Fragment => Self::fixed_lookup(self.fixed[3]),
            NonTerminalId::FragmentContent => Self::fixed_lookup(self.fixed[4]),
            NonTerminalId::StartTagContent => Self::fixed_lookup(self.fixed[5]),
            NonTerminalId::ElementContent => Self::fixed_lookup(self.fixed[6]),
            NonTerminalId::ElementFragment0 => Self::fixed_lookup(self.fixed[7]),
            NonTerminalId::ElementFragment1 => Self::fixed_lookup(self.fixed[8]),
            NonTerminalId::ElementFragment2 => Self::fixed_lookup(self.fixed[9]),
            NonTerminalId::ElementFragmentTypeEmpty0 => Self::fixed_lookup(self.fixed[10]),
            NonTerminalId::ElementFragmentTypeEmpty1 => Self::fixed_lookup(self.fixed[11]),
            NonTerminalId::Dynamic(n) => {
                if (n as u16) < self.dynamic_count {
                    Some(self.dynamic_base as usize + n)
                } else {
                    None
                }
            }
            NonTerminalId::SchemaType(n) => {
                if (n as u16) < self.schema_type_count {
                    Some(self.schema_type_base as usize + n)
                } else {
                    None
                }
            }
            NonTerminalId::Content2(n) => {
                let n_u16 = n as u16;
                let len = self.content2_len as usize;
                // Lineare Suche über 1-2 Einträge → effektiv O(1).
                for i in 0..len {
                    if self.content2[i].0 == n_u16 {
                        return Some(self.content2[i].1 as usize);
                    }
                }
                None
            }
        }
    }

    #[inline]
    fn fixed_lookup(val: u16) -> Option<usize> {
        if val == u16::MAX {
            None
        } else {
            Some(val as usize)
        }
    }
}

// ============================================================================
// GrammarSystem (Spec 8.4)
// ============================================================================

/// Vollständiges Grammar-System mit NonTerminals.
///
/// Ein GrammarSystem enthält alle NonTerminals einer Grammar und ermöglicht
/// Lookup, Pruning und Event Code Context Berechnung.
///
/// # Spec-Referenz
///
/// - 8.4 Built-in XML Grammars
#[derive(Debug, Clone)]
pub struct GrammarSystem {
    /// Typ der Grammar
    grammar_type: GrammarType,
    /// NonTerminals (sortiert nach NonTerminalId::Ord)
    non_terminals: Vec<NonTerminal>,
    /// O(1) Lookup-Tabelle für NonTerminalId → Vec-Index.
    nt_offsets: NtOffsets,
    /// Start-NonTerminal
    start: NonTerminalId,
    /// Optionaler Start für TypeEmpty (Schema-informed Complex Types).
    type_empty_start: Option<NonTerminalId>,
    /// SE(qname) → ElementDeclaration Side-Table für O(1) push_element Lookup.
    ///
    /// Nur bei Schema-informed Element Grammars befüllt. Mappt jeden ExpandedNameId
    /// (Head + Substitution Group Members) auf die zugehörige ElementDeclaration.
    se_element_decls: FastHashMap<ExpandedNameId, crate::schema::ElementDeclaration>,
}

impl GrammarSystem {
    /// Erstellt ein neues GrammarSystem.
    pub(crate) fn new(
        grammar_type: GrammarType,
        mut non_terminals: Vec<NonTerminal>,
        start: NonTerminalId,
    ) -> Self {
        non_terminals.sort_by_key(|nt| nt.id());
        let nt_offsets = NtOffsets::build(&non_terminals);
        Self {
            grammar_type,
            non_terminals,
            nt_offsets,
            start,
            type_empty_start: None,
            se_element_decls: FastHashMap::default(),
        }
    }

    /// Gibt den Grammar-Typ zurück.
    pub fn grammar_type(&self) -> GrammarType {
        self.grammar_type
    }

    /// Gibt das Start-NonTerminal zurück.
    pub fn start(&self) -> NonTerminalId {
        self.start
    }

    pub fn type_empty_start(&self) -> Option<NonTerminalId> {
        self.type_empty_start
    }

    pub fn set_type_empty_start(&mut self, start: Option<NonTerminalId>) {
        self.type_empty_start = start;
    }

    /// Setzt die SE-ElementDecl Side-Table.
    pub(crate) fn set_se_element_decls(
        &mut self,
        decls: FastHashMap<ExpandedNameId, crate::schema::ElementDeclaration>,
    ) {
        self.se_element_decls = decls;
    }

    /// Gibt die ElementDeclaration für eine ExpandedNameId zurück (O(1) Lookup).
    ///
    /// Nur bei Schema-informed Element Grammars befüllt.
    pub(crate) fn se_element_decl(
        &self,
        expanded: ExpandedNameId,
    ) -> Option<&crate::schema::ElementDeclaration> {
        self.se_element_decls.get(&expanded)
    }

    /// Baut die ProductionTable für alle NonTerminals (am Ende der Konstruktion aufrufen).
    fn seal_all_production_tables(&mut self) {
        for nt in self.non_terminals.iter_mut() {
            nt.seal_production_table();
        }
    }

    /// Augments an ElementFragment grammar with undeclared productions
    /// (Spec 8.5.4.4), treating it as nillable with named sub-types.
    ///
    /// This is required when an Element Fragment grammar is used in
    /// schema-informed fragment decoding/encoding (Spec 8.5.3 note).
    pub fn augment_element_fragment(&mut self, options: &ExiOptions, interner: &mut StringInterner) -> crate::Result<()> {
        if self.grammar_type != GrammarType::ElementFragment {
            return Ok(());
        }

        let fake_elem = crate::schema::ElementDeclaration::new(std::rc::Rc::new(
            crate::qname::QName::new("", "__element_fragment__"),
        ))
        .with_nillable(true)
        .with_type(std::rc::Rc::new(
            crate::schema::TypeDefinition::complex_with_sub_types(),
        ));

        if options.strict {
            // Strict: Alle NTs zusammen augmentieren (nur xsi:type/xsi:nil
            // auf NT[0], keine Content-Productions).
            crate::undeclared::augment_element_grammar(&mut self.non_terminals, &fake_elem, options, interner)?;
            self.sort_dedup_rebuild();
        } else {
            // Non-strict: Main-NTs und TypeEmpty-NTs getrennt augmentieren.
            // TypeEmpty muss als eigene Grammar augmentiert werden, damit
            // content_idx und Content2-Referenzen korrekt berechnet werden
            // (sonst zeigt Content2-RHS von CH/SE zurück zum AT-NT statt
            // zum Content-NT).
            let split_pos = self.non_terminals.iter().position(|nt|
                matches!(nt.id(), NonTerminalId::ElementFragmentTypeEmpty0 | NonTerminalId::ElementFragmentTypeEmpty1)
            ).unwrap_or(self.non_terminals.len());

            let mut te_nts: Vec<NonTerminal> = self.non_terminals.drain(split_pos..).collect();
            crate::undeclared::augment_element_grammar(&mut self.non_terminals, &fake_elem, options, interner)?;
            crate::undeclared::augment_element_grammar(&mut te_nts, &fake_elem, options, interner)?;
            self.non_terminals.extend(te_nts);
            self.sort_dedup_rebuild();
        }
        self.seal_all_production_tables();
        Ok(())
    }

    /// Gibt ein NonTerminal anhand seiner ID zurück (O(1) via NtOffsets).
    pub fn get(&self, id: NonTerminalId) -> Option<&NonTerminal> {
        let idx = self.nt_offsets.index_of(id)?;
        let nt = self.non_terminals.get(idx)?;
        debug_assert_eq!(nt.id(), id);
        Some(nt)
    }

    /// Gibt eine mutable Referenz auf ein NonTerminal zurück (O(1) via NtOffsets).
    pub fn get_mut(&mut self, id: NonTerminalId) -> Option<&mut NonTerminal> {
        let idx = self.nt_offsets.index_of(id)?;
        let nt = self.non_terminals.get_mut(idx)?;
        debug_assert_eq!(nt.id(), id);
        Some(nt)
    }

    /// Sortiert non_terminals, entfernt Duplikate (last wins) und baut NtOffsets neu auf.
    ///
    /// Content2-IDs können bei getrennter Augmentierung von Main- und TypeEmpty-Gruppen
    /// doppelt entstehen. Die HashMap-Version (HEAD) deduplizierte implizit via
    /// "last insert wins". Diese Methode repliziert das Verhalten für den Vec.
    fn sort_dedup_rebuild(&mut self) {
        self.non_terminals.sort_by_key(|nt| nt.id());
        // Reverse + dedup + reverse: behält den LETZTEN Eintrag bei Duplikaten.
        self.non_terminals.reverse();
        self.non_terminals.dedup_by_key(|nt| nt.id());
        self.non_terminals.reverse();
        self.nt_offsets = NtOffsets::build(&self.non_terminals);
    }

    /// Entfernt xsi:type/xsi:nil-Productions aus der Start-NT.
    ///
    /// Spec 8.5.4.4.2: Type-Grammars bekommen in strict Mode KEINE
    /// xsi:type/xsi:nil-Augmentierung — die Spec definiert diese nur
    /// für "each normalized element grammar Element_i".
    pub fn strip_xsi_type_nil_from_start(&mut self, interner: &StringInterner) {
        let start = self.start;
        if let Some(nt) = self.get_mut(start) {
            nt.retain_productions(|p| {
                if let Terminal::Attribute(AttributeKind::QName(q)) = p.terminal {
                    let (uri, local) = q.resolve(interner);
                    !(uri == URI_XSI && (local == "type" || local == "nil"))
                } else {
                    true
                }
            });
            nt.recalculate_event_codes();
        }
    }

    /// Gibt eine Referenz auf alle NonTerminals zurück (Debug/Inspect).
    pub fn non_terminals(&self) -> &[NonTerminal] {
        &self.non_terminals
    }

    /// Gibt den EventCodeContext für ein NonTerminal zurück.
    pub fn event_code_context(&self, id: NonTerminalId) -> Option<&EventCodeContext> {
        self.get(id).map(|nt| nt.event_code_context())
    }

    /// Built-in Document Grammar (Spec 8.4.1).
    ///
    /// ```text
    /// Document:
    ///   SD DocContent               [0]
    ///
    /// DocContent:
    ///   SE(*) DocEnd                [0]
    ///   DT DocContent               [1.0]
    ///   CM DocContent               [1.1.0]
    ///   PI DocContent               [1.1.1]
    ///
    /// DocEnd:
    ///   ED                          [0]
    ///   CM DocEnd                   [1.0]
    ///   PI DocEnd                   [1.1]
    /// ```
    pub fn built_in_document() -> Self {
        let mut non_terminals = Vec::new();

        // Document: SD → DocContent [0]
        non_terminals.push(NonTerminal::new(
            NonTerminalId::Document,
            vec![Production::new(
                Terminal::StartDocument,
                EventCode::one(0),
                Some(NonTerminalId::DocContent),
            )],
        ));

        // DocContent
        non_terminals.push(NonTerminal::new(
            NonTerminalId::DocContent,
            vec![
                // SE(*) → DocEnd [0]
                Production::new(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(0),
                    Some(NonTerminalId::DocEnd),
                ),
                // DT → DocContent [1.0]
                Production::new(
                    Terminal::DocType,
                    EventCode::two(1, 0),
                    Some(NonTerminalId::DocContent),
                ),
                // CM → DocContent [1.1.0]
                Production::new(
                    Terminal::Comment,
                    EventCode::three(1, 1, 0),
                    Some(NonTerminalId::DocContent),
                ),
                // PI → DocContent [1.1.1]
                Production::new(
                    Terminal::ProcessingInstr,
                    EventCode::three(1, 1, 1),
                    Some(NonTerminalId::DocContent),
                ),
            ],
        ));

        // DocEnd
        non_terminals.push(NonTerminal::new(
            NonTerminalId::DocEnd,
            vec![
                // ED [0]
                Production::terminal(Terminal::EndDocument, EventCode::one(0)),
                // CM → DocEnd [1.0]
                Production::new(
                    Terminal::Comment,
                    EventCode::two(1, 0),
                    Some(NonTerminalId::DocEnd),
                ),
                // PI → DocEnd [1.1]
                Production::new(
                    Terminal::ProcessingInstr,
                    EventCode::two(1, 1),
                    Some(NonTerminalId::DocEnd),
                ),
            ],
        ));

        let mut gs = Self::new(
            GrammarType::Document,
            non_terminals,
            NonTerminalId::Document,
        );
        gs.seal_all_production_tables();
        gs
    }

    /// Built-in Fragment Grammar (Spec 8.4.2).
    ///
    /// ```text
    /// Fragment:
    ///   SD FragmentContent          [0]
    ///
    /// FragmentContent:
    ///   SE(*) FragmentContent       [0]
    ///   ED                          [1]
    ///   CM FragmentContent          [2.0]
    ///   PI FragmentContent          [2.1]
    /// ```
    pub fn built_in_fragment() -> Self {
        let mut non_terminals = Vec::new();

        // Fragment: SD → FragmentContent [0]
        non_terminals.push(NonTerminal::new(
            NonTerminalId::Fragment,
            vec![Production::new(
                Terminal::StartDocument,
                EventCode::one(0),
                Some(NonTerminalId::FragmentContent),
            )],
        ));

        // FragmentContent
        non_terminals.push(NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                // SE(*) → FragmentContent [0]
                Production::new(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                // ED [1]
                Production::terminal(Terminal::EndDocument, EventCode::one(1)),
                // CM → FragmentContent [2.0]
                Production::new(
                    Terminal::Comment,
                    EventCode::two(2, 0),
                    Some(NonTerminalId::FragmentContent),
                ),
                // PI → FragmentContent [2.1]
                Production::new(
                    Terminal::ProcessingInstr,
                    EventCode::two(2, 1),
                    Some(NonTerminalId::FragmentContent),
                ),
            ],
        ));

        let mut gs = Self::new(
            GrammarType::Fragment,
            non_terminals,
            NonTerminalId::Fragment,
        );
        gs.seal_all_production_tables();
        gs
    }

    /// Built-in Element Grammar (Spec 8.4.3).
    ///
    /// ```text
    /// StartTagContent:
    ///   EE                          [0.0]
    ///   AT(*) StartTagContent       [0.1]
    ///   NS StartTagContent          [0.2]
    ///   SC Fragment                 [0.3]      (nur wenn selfContained=true)
    ///   ChildContentItems(0.4)                 (oder 0.3 wenn selfContained=false)
    ///
    /// ElementContent:
    ///   EE                          [0]
    ///   ChildContentItems(1.0)
    /// ```
    ///
    /// # Note (Spec 8.4.3)
    ///
    /// Wenn `selfContained=false`:
    /// - SC-Production fehlt in StartTagContent
    /// - ChildContentItems wird mit (0.3) statt (0.4) expandiert
    ///
    /// # Externe Grammar-Referenz (Spec 8.4.3)
    ///
    /// SC (SelfContained) verweist auf `NonTerminalId::Fragment`, das in diesem
    /// GrammarSystem **nicht enthalten** ist. Dies ist beabsichtigt.
    ///
    /// Bei SC-Verarbeitung muss der Caller:
    /// 1. String Table und Grammars speichern
    /// 2. State auf Anfangszustand zurücksetzen
    /// 3. Fragment Grammar (`built_in_fragment()`) verwenden
    /// 4. Nach ED den gespeicherten State wiederherstellen
    ///
    /// `get(NonTerminalId::Fragment)` gibt hier `None` zurück.
    pub fn built_in_element(options: &ExiOptions) -> Self {
        let mut non_terminals = Vec::new();

        // StartTagContent
        let mut start_tag_prods = vec![
            // EE [0.0]
            Production::terminal(Terminal::EndElement, EventCode::two(0, 0)),
            // AT(*) → StartTagContent [0.1]
            Production::new(
                Terminal::Attribute(AttributeKind::Wildcard),
                EventCode::two(0, 1),
                Some(NonTerminalId::StartTagContent),
            ),
            // NS → StartTagContent [0.2]
            Production::new(
                Terminal::NamespaceDecl,
                EventCode::two(0, 2),
                Some(NonTerminalId::StartTagContent),
            ),
        ];

        let child_content_start = if options.self_contained {
            // SC → Fragment [0.3]
            start_tag_prods.push(Production::new(
                Terminal::SelfContained,
                EventCode::two(0, 3),
                Some(NonTerminalId::Fragment),
            ));
            4u32
        } else {
            3u32
        };

        // ChildContentItems(0.child_content_start)
        let child_items = MacroTemplate::child_content_items();
        start_tag_prods.extend(child_items.expand(0, child_content_start));

        non_terminals.push(NonTerminal::new(NonTerminalId::StartTagContent, start_tag_prods));

        // ElementContent
        let mut element_content_prods = vec![
            // EE [0]
            Production::terminal(Terminal::EndElement, EventCode::one(0)),
        ];

        // ChildContentItems(1.0)
        element_content_prods.extend(child_items.expand(1, 0));

        non_terminals.push(NonTerminal::new(NonTerminalId::ElementContent, element_content_prods));

        let mut gs = Self::new(
            GrammarType::Element,
            non_terminals,
            NonTerminalId::StartTagContent,
        );
        gs.seal_all_production_tables();
        gs
    }

    /// Schema-informed Document Grammar (Spec 8.5.1).
    ///
    /// ```text
    /// Document:
    ///   SD DocContent                    [0]
    ///
    /// DocContent:
    ///   SE (G_0) DocEnd                  [0]
    ///   SE (G_1) DocEnd                  [1]
    ///   ⋮
    ///   SE (G_{n-1}) DocEnd              [n-1]
    ///   SE (*) DocEnd                    [n]
    ///   DT DocContent                    [(n+1).0]
    ///   CM DocContent                    [(n+1).1.0]
    ///   PI DocContent                    [(n+1).1.1]
    ///
    /// DocEnd:
    ///   ED                               [0]
    ///   CM DocEnd                        [1.0]
    ///   PI DocEnd                        [1.1]
    /// ```
    ///
    /// # Note
    ///
    /// - `n` = Anzahl globaler Elemente im Schema
    /// - G_0..G_{n-1} = QNames sortiert: erst local-name, dann URI
    ///
    /// # Errors
    ///
    /// Gibt `Error::IntegerOverflow` zurück wenn das Schema mehr als
    /// `u32::MAX` Elemente enthält.
    pub fn schema_informed_document(schema: &crate::schema::SchemaInfo, interner: &mut StringInterner) -> Result<Self> {
        let mut non_terminals = Vec::new();
        let n = checked_u32(schema.global_elements().len())?;

        // Document: SD → DocContent [0]
        non_terminals.push(NonTerminal::new(
            NonTerminalId::Document,
            vec![Production::new(
                Terminal::StartDocument,
                EventCode::one(0),
                Some(NonTerminalId::DocContent),
            )],
        ));

        // DocContent: n×SE(G_i) + SE(*) + DT + CM + PI = n + 4
        let mut doc_content_prods = Vec::with_capacity(n as usize + 4);

        // SE(G_i) → DocEnd [i] für i in 0..n
        for (i, qname) in schema.global_elements().iter().enumerate() {
            doc_content_prods.push(Production::new(
                Terminal::se_qname(qname, interner)?,
                EventCode::one(i as u32),
                Some(NonTerminalId::DocEnd),
            ));
        }

        // SE(*) → DocEnd [n]
        doc_content_prods.push(Production::new(
            Terminal::StartElement(StartElementKind::Wildcard),
            EventCode::one(n),
            Some(NonTerminalId::DocEnd),
        ));

        // DT → DocContent [(n+1).0]
        doc_content_prods.push(Production::new(
            Terminal::DocType,
            EventCode::two(n + 1, 0),
            Some(NonTerminalId::DocContent),
        ));

        // CM → DocContent [(n+1).1.0]
        doc_content_prods.push(Production::new(
            Terminal::Comment,
            EventCode::three(n + 1, 1, 0),
            Some(NonTerminalId::DocContent),
        ));

        // PI → DocContent [(n+1).1.1]
        doc_content_prods.push(Production::new(
            Terminal::ProcessingInstr,
            EventCode::three(n + 1, 1, 1),
            Some(NonTerminalId::DocContent),
        ));

        non_terminals.push(NonTerminal::new(NonTerminalId::DocContent, doc_content_prods));

        // DocEnd (identisch mit Built-in)
        non_terminals.push(NonTerminal::new(
            NonTerminalId::DocEnd,
            vec![
                // ED [0]
                Production::terminal(Terminal::EndDocument, EventCode::one(0)),
                // CM → DocEnd [1.0]
                Production::new(
                    Terminal::Comment,
                    EventCode::two(1, 0),
                    Some(NonTerminalId::DocEnd),
                ),
                // PI → DocEnd [1.1]
                Production::new(
                    Terminal::ProcessingInstr,
                    EventCode::two(1, 1),
                    Some(NonTerminalId::DocEnd),
                ),
            ],
        ));

        let mut gs = Self::new(
            GrammarType::Document,
            non_terminals,
            NonTerminalId::Document,
        );
        gs.seal_all_production_tables();
        Ok(gs)
    }

    /// Schema-informed Element Grammar für eine ElementDeclaration (Spec 8.5.4).
    ///
    /// # 2-Tier System
    ///
    /// Diese Funktion implementiert das 2-Tier Grammar System:
    /// - **Tier 1:** Nur Schema-declared Productions (in der Grammar)
    /// - **Tier 2:** Undeclared Productions werden dynamisch behandelt
    ///
    /// Im Unterschied zu Built-in Grammars werden hier **keine** Undeclared
    /// Productions statisch hinzugefügt. Der Encoder/Decoder behandelt
    /// diese dynamisch basierend auf dem EventCodeContext.
    ///
    /// # Arguments
    ///
    /// * `elem_decl` - Die ElementDeclaration aus dem Schema
    /// * `options` - EXI Options (für strict-Mode Prüfung)
    ///
    /// # Spec-Referenz
    ///
    /// - 8.5.4 Element and Type Grammars
    /// - 8.5.4.4 Undeclared Productions (Tier 2)
    pub fn schema_informed_element(
        elem_decl: &crate::schema::ElementDeclaration,
        options: &ExiOptions,
        interner: &mut StringInterner,
    ) -> Result<Self> {
        use crate::schema::TypeDefinition;

        let Some(td) = elem_decl.type_definition.as_ref() else {
            // Kein Type definiert: Built-in als Fallback
            let mut grammar = Self::built_in_element(options);
            grammar.prune(options)?;
            return Ok(grammar);
        };

        match td.as_ref() {
            TypeDefinition::Simple { .. } => {
                // Simple Type Element: CH [0] → EE [0]
                Self::schema_informed_simple_type_element(elem_decl, options, interner)
            }
            TypeDefinition::Complex { .. } => {
                // Complex Type Element: Attribute + Content Model
                Self::schema_informed_complex_type_element(elem_decl, td, options, interner)
            }
        }
    }

    /// Schema-informed Element Grammar für Simple Type (Spec 8.5.4.1.3.1).
    ///
    /// ```text
    /// Type_i,0:
    ///   CH [schema-typed value] Type_i,1     [0]
    ///
    /// Type_i,1:
    ///   EE                                   [0]
    /// ```
    ///
    /// Bei strict=false wird EE als Undeclared Production (Tier 2) behandelt
    /// statt in Tier 1 aufgenommen (Spec 8.5.4.4.1).
    fn schema_informed_simple_type_element(
        elem_decl: &crate::schema::ElementDeclaration,
        options: &ExiOptions,
        interner: &mut StringInterner,
    ) -> Result<Self> {
        let mut non_terminals = Vec::new();

        // StartTagContent (SchemaType(0)): CH [0] → SchemaType(1)
        let start_prods = vec![Production::new(
            Terminal::Characters,
            EventCode::one(0),
            Some(NonTerminalId::SchemaType(1)),
        )];

        non_terminals.push(NonTerminal::new(NonTerminalId::SchemaType(0), start_prods));

        // SchemaType(1): EE [0]
        non_terminals.push(NonTerminal::new(
            NonTerminalId::SchemaType(1),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        ));

        let mut grammar = Self::new(
            GrammarType::SchemaInformedElement,
            non_terminals,
            NonTerminalId::SchemaType(0),
        );

        // TypeEmpty für Simple Types: EE-only Grammars, damit xsi:nil="true"
        // korrekt auf leeren Inhalt umschaltet (Spec 8.5.4.4.1/8.5.4.4.2).
        // Wir erzeugen einen parallelen Satz an NTs, um den Offset-Mapping-Mechanismus
        // (current_nt -> empty_offset + current_idx) zu unterstützen.
        {
            let empty_offset = Self::next_schema_type_offset(grammar.non_terminals());
            let mut empty_nts: Vec<NonTerminal> = Vec::new();
            let empty_len = grammar.non_terminals().len();
            for idx in 0..empty_len {
                empty_nts.push(NonTerminal::new(
                    NonTerminalId::SchemaType(empty_offset + idx),
                    vec![Production::terminal(
                        Terminal::EndElement,
                        EventCode::one(0),
                    )],
                ));
            }
            // Undeclared Productions für TypeEmpty (nur bei strict=false).
            if let Some(td) = elem_decl.type_definition.as_ref() {
                crate::undeclared::augment_type_grammar(&mut empty_nts, td, options, Some(elem_decl), interner)?;
            }
            if !empty_nts.is_empty() {
                grammar.non_terminals.extend(empty_nts);
                grammar.sort_dedup_rebuild();
                grammar.set_type_empty_start(Some(NonTerminalId::SchemaType(empty_offset)));
            }
        }

        // Spec 8.5.4 StartTagContent: NS ist erlaubt wenn preserve.prefixes=true.
        // In Schema-informed Grammars ist StartTagContent = SchemaType(0).
        grammar.add_namespace_decls_to_start_tag(options.preserve.prefixes)?;

        // Undeclared Productions nach Spec 8.5.4.4 (strict/non-strict)
        crate::undeclared::augment_element_grammar(&mut grammar.non_terminals, elem_decl, options, interner)?;
        grammar.sort_dedup_rebuild();

        // Pruning für preserve-abhängige Productions (bei Simple Type minimal)
        grammar.prune(options)?;

        Ok(grammar)
    }

    /// Schema-informed Element Grammar für Complex Type (Spec 8.5.4.1, 8.5.4.2).
    ///
    /// Generiert Grammar basierend auf:
    /// - Attribut-Deklarationen → AT(qname) Productions mit Übergang zu nächstem NT
    /// - Content Model (Sequence/Choice/All) → SE(qname) Productions
    ///
    /// Laut Spec 8.5.4.2 und Appendix H:
    /// - Jedes AT führt zu einem NEUEN NonTerminal (nicht zurück zum selben)
    /// - Das letzte AT führt zum Content-NonTerminal
    /// - SE/CH im Content-NT haben Event Code 0
    fn schema_informed_complex_type_element(
        elem_decl: &crate::schema::ElementDeclaration,
        type_def: &crate::schema::TypeDefinition,
        options: &ExiOptions,
        interner: &mut StringInterner,
    ) -> Result<Self> {
        use crate::proto_grammar::ProtoGrammar;
        let attr_order = crate::proto_grammar::AttributeOrder::Lexicographic;
        let (type_grammar, type_empty) = ProtoGrammar::complex_type_with_attr_order(
            type_def,
            attr_order,
            interner,
        )?;
        let mut normalized_type = type_grammar.normalize(interner);
        let mut normalized_empty = type_empty.normalize(interner);

        // Undeclared Productions (Spec 8.5.4.4)
        crate::undeclared::augment_element_grammar(&mut normalized_type, elem_decl, options, interner)?;
        crate::undeclared::augment_type_grammar(&mut normalized_empty, type_def, options, Some(elem_decl), interner)?;
        let has_type_empty = !normalized_empty.is_empty();

        // ProtoGrammar nutzt Dynamic-IDs; Schema-informed Element-Grammars erwarten SchemaType-IDs.
        let mut non_terminals = Self::map_dynamic_to_schema_type_with_offset(normalized_type, 0);
        let empty_offset = Self::next_schema_type_offset(&non_terminals);
        let empty_nts =
            Self::map_dynamic_to_schema_type_with_offset(normalized_empty, empty_offset);
        if !empty_nts.is_empty() {
            non_terminals.extend(empty_nts);
        }

        let mut grammar = Self::new(
            GrammarType::SchemaInformedElement,
            non_terminals,
            NonTerminalId::SchemaType(0),
        );

        // SE-ElementDecl Side-Table für O(1) push_element Lookup
        grammar.set_se_element_decls(crate::schema::collect_se_element_decls(type_def, interner)?);

        if has_type_empty {
            grammar.set_type_empty_start(Some(NonTerminalId::SchemaType(empty_offset)));
        }

        // Spec 8.5.4 StartTagContent: NS ist erlaubt wenn preserve.prefixes=true.
        // Füge NS zu allen StartTag-NonTerminals (Attribute-Kette) hinzu.
        grammar.add_namespace_decls_to_start_tag(options.preserve.prefixes)?;

        // Spec 8.5.4.1.3.2 (Z.2461-2462): AT(*) G_i,0 — Wildcard-Attribute
        // loopen zurück auf die eigene NT (Self-Loop).
        for nt in grammar.non_terminals.iter_mut() {
            if !nt
                .productions()
                .iter()
                .any(|p| matches!(p.terminal, Terminal::Attribute(_)))
            {
                continue;
            }

            nt.update_wildcard_rhs(nt.id());
        }

        grammar.prune(options)?;

        Ok(grammar)
    }

    /// Fügt NS-Productions zu allen StartTag-NonTerminals hinzu.
    ///
    /// StartTag-NonTerminals sind alle NonTerminals, die vom Start über
    /// Attribute-Produktionen erreichbar sind (inkl. Start selbst).
    fn add_namespace_decls_to_start_tag(&mut self, preserve_prefixes: bool) -> Result<()> {
        if !preserve_prefixes {
            return Ok(());
        }
        use std::collections::VecDeque;
        use crate::FastHashSet;

        let start = self.start();
        let mut start_nts = FastHashSet::default();
        let mut queue = VecDeque::new();
        start_nts.insert(start);
        queue.push_back(start);

        while let Some(nt_id) = queue.pop_front() {
            let Some(nt) = self.get(nt_id) else { continue };
            for prod in nt.productions() {
                if matches!(prod.terminal, Terminal::Attribute(_))
                    && let Some(rhs) = prod.right_hand_side
                        && start_nts.insert(rhs) {
                            queue.push_back(rhs);
                        }
            }
        }

        for nt_id in start_nts {
            let Some(nt) = self.get_mut(nt_id) else { continue };

            if nt.find_by_terminal(&Terminal::NamespaceDecl).is_some() {
                continue;
            }

            let insert_pos = nt
                .productions()
                .iter()
                .position(|p| !matches!(p.terminal, Terminal::Attribute(_)))
                .unwrap_or_else(|| nt.productions().len());

            let ec_len = nt
                .productions()
                .iter()
                .find_map(|p| p.event_code.as_ref().map(|ec| ec.length()))
                .unwrap_or(1);

            let ns_code = match ec_len {
                1 => EventCode::one(0),
                2 => EventCode::two(0, 0),
                _ => EventCode::three(0, 0, 0),
            };

            nt.insert_production(
                insert_pos,
                Production::terminal(Terminal::NamespaceDecl, ns_code),
            );
            nt.recalculate_event_codes();
        }

        Ok(())
    }

    /// Decode-only: Attribut-Wildcards in allen StartTag-NonTerminals.
    ///
    /// Spec 8.5.4.1.3.2 ergänzt Wildcards nur in `G_i,0`. Exificient hält AT(*)/AT(uri:*)
    /// jedoch in späteren StartTag-NonTerminals verfügbar, wodurch sich Event-Code-Breiten
    /// ändern können. Diese Methode fügt die Wildcards tolerant in alle StartTag-NTs ein
    /// und recalcult die Event Codes für diese NTs.
    fn map_dynamic_to_schema_type_with_offset(
        normalized: Vec<NonTerminal>,
        offset: usize,
    ) -> Vec<NonTerminal> {
        let mut non_terminals = Vec::with_capacity(normalized.len());

        for nt in normalized.into_iter() {
            let schema_id = match nt.id() {
                NonTerminalId::Dynamic(i) => NonTerminalId::SchemaType(offset + i),
                NonTerminalId::Content2(i) => NonTerminalId::Content2(offset + i),
                other => other,
            };

            let prods = nt
                .productions()
                .iter()
                .map(|p| Production {
                    terminal: p.terminal.clone(),
                    event_code: p.event_code.clone(),
                    right_hand_side: match p.right_hand_side {
                        Some(NonTerminalId::Dynamic(i)) => {
                            Some(NonTerminalId::SchemaType(offset + i))
                        }
                        Some(NonTerminalId::Content2(i)) => {
                            Some(NonTerminalId::Content2(offset + i))
                        }
                        Some(other) => Some(other),
                        None => None,
                    },
                })
                .collect();

            non_terminals.push(NonTerminal::new(schema_id, prods));
        }

        non_terminals
    }

    fn next_schema_type_offset(
        non_terminals: &[NonTerminal],
    ) -> usize {
        non_terminals
            .iter()
            .filter_map(|nt| match nt.id() {
                NonTerminalId::SchemaType(i) => Some(i),
                _ => None,
            })
            .max()
            .map(|i| i + 1)
            .unwrap_or(0)
    }

    /// Schema-informed Fragment Grammar (Spec 8.5.2).
    ///
    /// ```text
    /// Fragment:
    ///   SD FragmentContent               [0]
    ///
    /// FragmentContent:
    ///   SE (F_0) FragmentContent         [0]
    ///   SE (F_1) FragmentContent         [1]
    ///   ⋮
    ///   SE (F_{n-1}) FragmentContent     [n-1]
    ///   SE (*) FragmentContent           [n]
    ///   ED                               [n+1]
    ///   CM FragmentContent               [(n+2).0]
    ///   PI FragmentContent               [(n+2).1]
    /// ```
    ///
    /// # Note
    ///
    /// - `n` = Anzahl **aller** Element-QNames im Schema (nicht nur global)
    /// - F_0..F_{n-1} = QNames sortiert: erst local-name, dann URI
    /// - FragmentContent verweist auf sich selbst (Loop)
    ///
    /// # Errors
    ///
    /// Gibt `Error::IntegerOverflow` zurück wenn das Schema mehr als
    /// `u32::MAX` Elemente enthält.
    pub fn schema_informed_fragment(schema: &crate::schema::SchemaInfo, interner: &mut StringInterner) -> Result<Self> {
        let mut non_terminals = Vec::new();
        let mut qnames: Vec<_> = schema.all_elements().to_vec();
        // Spec 8.5.4.1.6: QNames sortiert nach (local-name, uri)
        qnames.sort_by(|a, b| {
            a.local_name
                .cmp(&b.local_name)
                .then_with(|| a.uri.cmp(&b.uri))
        });

        let n = checked_u32(qnames.len())?;

        // Fragment: SD → FragmentContent [0]
        non_terminals.push(NonTerminal::new(
            NonTerminalId::Fragment,
            vec![Production::new(
                Terminal::StartDocument,
                EventCode::one(0),
                Some(NonTerminalId::FragmentContent),
            )],
        ));

        // FragmentContent: n×SE(F_i) + SE(*) + ED + CM + PI = n + 4
        let mut fc_prods = Vec::with_capacity(n as usize + 4);

        // SE(F_i) → FragmentContent [i] für i in 0..n
        for (i, qname) in qnames.iter().enumerate() {
            fc_prods.push(Production::new(
                Terminal::se_qname(qname, interner)?,
                EventCode::one(i as u32),
                Some(NonTerminalId::FragmentContent), // Loop!
            ));
        }

        // SE(*) → FragmentContent [n]
        fc_prods.push(Production::new(
            Terminal::StartElement(StartElementKind::Wildcard),
            EventCode::one(n),
            Some(NonTerminalId::FragmentContent),
        ));

        // ED [n+1]
        fc_prods.push(Production::terminal(
            Terminal::EndDocument,
            EventCode::one(n + 1),
        ));

        // CM → FragmentContent [(n+2).0]
        fc_prods.push(Production::new(
            Terminal::Comment,
            EventCode::two(n + 2, 0),
            Some(NonTerminalId::FragmentContent),
        ));

        // PI → FragmentContent [(n+2).1]
        fc_prods.push(Production::new(
            Terminal::ProcessingInstr,
            EventCode::two(n + 2, 1),
            Some(NonTerminalId::FragmentContent),
        ));

        non_terminals.push(NonTerminal::new(NonTerminalId::FragmentContent, fc_prods));

        let mut gs = Self::new(
            GrammarType::Fragment,
            non_terminals,
            NonTerminalId::Fragment,
        );
        gs.seal_all_production_tables();
        Ok(gs)
    }

    /// Schema-informed Element Fragment Grammar (Spec 8.5.3).
    ///
    /// Verwendet für Elemente mit gleichem QName aber unterschiedlichen Typen.
    ///
    /// # NonTerminals
    ///
    /// - `ElementFragment_0`: Start mit Attributen
    /// - `ElementFragment_1`: Nach Attributen (kein AT mehr)
    /// - `ElementFragment_2`: Element Content (wie _1)
    /// - `ElementFragmentTypeEmpty_0`: TypeEmpty mit Attributen
    /// - `ElementFragmentTypeEmpty_1`: TypeEmpty nach Attributen (nur EE)
    ///
    /// # Note
    ///
    /// - `n` = Anzahl Attribut-QNames im Schema
    /// - `m` = Anzahl Element-QNames im Schema
    ///
    /// # Errors
    ///
    /// Gibt `Error::IntegerOverflow` zurück wenn das Schema mehr als
    /// `u32::MAX` Elemente oder Attribute enthält.
    pub fn schema_informed_element_fragment(schema: &crate::schema::SchemaInfo, interner: &mut StringInterner) -> Result<Self> {
        let mut non_terminals = Vec::new();
        let mut attrs: Vec<_> = schema.all_attributes().to_vec();
        let mut elems: Vec<_> = schema.all_elements().to_vec();
        // Spec 8.5.4.1.6: QNames sortiert nach (local-name, uri)
        attrs.sort_by(|a, b| {
            a.local_name
                .cmp(&b.local_name)
                .then_with(|| a.uri.cmp(&b.uri))
        });
        elems.sort_by(|a, b| {
            a.local_name
                .cmp(&b.local_name)
                .then_with(|| a.uri.cmp(&b.uri))
        });

        let n = checked_u32(attrs.len())?;
        let m = checked_u32(elems.len())?;

        // Terminals vorab internen (Interner wird nur einmal gebraucht)
        let attr_terminals: Vec<Terminal> = attrs
            .iter()
            .map(|qname| Terminal::at_qname(qname, interner))
            .collect::<Result<_>>()?;
        let elem_terminals: Vec<Terminal> = elems
            .iter()
            .map(|qname| Terminal::se_qname(qname, interner))
            .collect::<Result<_>>()?;

        // Hilfsfunktion: AT-Productions für alle Schema-Attribute + AT(*)
        let make_attr_prods = |start_code: u32, target: NonTerminalId| -> Vec<Production> {
            let mut prods = Vec::with_capacity(n as usize + 1);
            for (i, terminal) in attr_terminals.iter().enumerate() {
                prods.push(Production::new(
                    *terminal,
                    EventCode::one(start_code + i as u32),
                    Some(target),
                ));
            }
            prods.push(Production::new(
                Terminal::Attribute(AttributeKind::Wildcard),
                EventCode::one(start_code + n),
                Some(target),
            ));
            prods
        };

        // Hilfsfunktion: SE-Productions für alle Schema-Elemente + SE(*)
        let make_element_prods = |start_code: u32, target: NonTerminalId| -> Vec<Production> {
            let mut prods = Vec::with_capacity(m as usize + 1);
            for (j, terminal) in elem_terminals.iter().enumerate() {
                prods.push(Production::new(
                    *terminal,
                    EventCode::one(start_code + j as u32),
                    Some(target),
                ));
            }
            prods.push(Production::new(
                Terminal::StartElement(StartElementKind::Wildcard),
                EventCode::one(start_code + m),
                Some(target),
            ));
            prods
        };

        // ElementFragment_0: AT + SE + EE + CH
        let mut ef0_prods = make_attr_prods(0, NonTerminalId::ElementFragment0);
        ef0_prods.extend(make_element_prods(n + 1, NonTerminalId::ElementFragment2));
        ef0_prods.push(Production::terminal(
            Terminal::EndElement,
            EventCode::one(n + m + 2),
        ));
        ef0_prods.push(Production::new(
            Terminal::Characters,
            EventCode::one(n + m + 3),
            Some(NonTerminalId::ElementFragment2),
        ));
        non_terminals.push(NonTerminal::new(NonTerminalId::ElementFragment0, ef0_prods));

        // ElementFragment_1/2: SE + EE + CH (keine Attribute)
        let mut ef1_prods = make_element_prods(0, NonTerminalId::ElementFragment2);
        ef1_prods.push(Production::terminal(
            Terminal::EndElement,
            EventCode::one(m + 1),
        ));
        ef1_prods.push(Production::new(
            Terminal::Characters,
            EventCode::one(m + 2),
            Some(NonTerminalId::ElementFragment2),
        ));
        non_terminals.push(NonTerminal::new(NonTerminalId::ElementFragment1, ef1_prods.clone()));
        non_terminals.push(NonTerminal::new(NonTerminalId::ElementFragment2, ef1_prods));

        // ElementFragmentTypeEmpty_0: AT + EE (keine SE, kein CH)
        let mut te0_prods = make_attr_prods(0, NonTerminalId::ElementFragmentTypeEmpty0);
        te0_prods.push(Production::terminal(
            Terminal::EndElement,
            EventCode::one(n + 1),
        ));
        non_terminals.push(NonTerminal::new(NonTerminalId::ElementFragmentTypeEmpty0, te0_prods));

        // ElementFragmentTypeEmpty_1: nur EE
        non_terminals.push(NonTerminal::new(
            NonTerminalId::ElementFragmentTypeEmpty1,
            vec![Production::terminal(
                Terminal::EndElement,
                EventCode::one(0),
            )],
        ));

        let mut grammar = Self::new(
            GrammarType::ElementFragment,
            non_terminals,
            NonTerminalId::ElementFragment0,
        );
        // Spec 8.5.3: ElementFragmentTypeEmpty dient als TypeEmpty-Grammar
        // (u.a. für xsi:nil Handling).
        grammar.set_type_empty_start(Some(NonTerminalId::ElementFragmentTypeEmpty0));
        grammar.seal_all_production_tables();
        Ok(grammar)
    }

    /// Prunt Productions basierend auf Fidelity Options (Spec 8.3).
    ///
    /// Entfernt nicht-erlaubte Productions aus allen NonTerminals und berechnet
    /// Event Codes neu, sodass sie contiguous bleiben.
    ///
    /// # Errors
    ///
    /// Gibt `Error::InvalidOptionCombination` zurück wenn `options.validate()` fehlschlägt.
    ///
    /// # Reihenfolge (wichtig!)
    ///
    /// 1. Productions in jedem NonTerminal filtern
    /// 2. Event Codes pro NonTerminal neu berechnen
    ///
    /// # Spec-Referenz
    ///
    /// - 6.3 Fidelity Options
    /// - 8.3 Pruning Unneeded Productions
    ///
    /// Ruft am Ende `seal_all_production_tables()` auf.
    pub fn prune(&mut self, options: &ExiOptions) -> crate::Result<()> {
        options.validate()?;

        let grammar_type = self.grammar_type;
        for nt in self.non_terminals.iter_mut() {
            // ProductionTable invalidieren, damit rebuild in
            // retain_productions/recalculate_event_codes ein No-Op ist.
            nt.production_table = None;
            nt.retain_productions(|p| is_terminal_allowed(&p.terminal, options, grammar_type));
            nt.recalculate_event_codes();
        }
        self.seal_all_production_tables();
        Ok(())
    }

    /// Findet das erste NonTerminal ab `current_nt`, das keine Attribute-Productions hat.
    ///
    /// Wird fuer Tier-2 Decoding/Encoding benoetigt, um das Content-NonTerminal
    /// zu bestimmen (nach den Attribut-NonTerminals).
    pub fn find_content_nonterminal(&self, current_nt: NonTerminalId) -> Option<NonTerminalId> {
        use std::collections::VecDeque;
        use crate::FastHashSet;

        let mut visited = FastHashSet::default();
        let mut queue = VecDeque::new();
        queue.push_back(current_nt);

        while let Some(nt_id) = queue.pop_front() {
            if !visited.insert(nt_id) {
                continue;
            }

            let nt = self.get(nt_id)?;
            let has_attribute = nt
                .productions()
                .iter()
                .any(|p| matches!(p.terminal, Terminal::Attribute(_)));
            if !has_attribute {
                return Some(nt_id);
            }

            for prod in nt.productions() {
                if let Some(next) = prod.right_hand_side {
                    queue.push_back(next);
                }
            }
        }

        None
    }
}

// ============================================================================
// GrammarType (Spec 8.4)
// ============================================================================

/// Typ einer Grammar (beeinflusst welche Terminals vorkommen können).
///
/// # Spec-Referenz
/// - 8.4.1 Built-in Document Grammar
/// - 8.4.2 Built-in Fragment Grammar
/// - 8.4.3 Built-in Element Grammar
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GrammarType {
    /// Document Grammar (SD, SE, ED, DT, CM, PI)
    Document,
    /// Fragment Grammar (SD, SE, ED, CM, PI)
    Fragment,
    /// Element Grammar (EE, SE, AT, CH, NS, CM, PI, ER, SC)
    Element,
    /// Element Fragment Grammar (Spec 8.5.3)
    ///
    /// Verwendet für Elemente mit gleichem QName aber unterschiedlichen Typen.
    /// Hat 5 NonTerminals: ElementFragment0/1/2, ElementFragmentTypeEmpty0/1.
    ElementFragment,
    /// Schema-informed Element Grammar (Spec 8.5.4)
    ///
    /// Im Unterschied zu Built-in Element Grammar werden Undeclared Productions
    /// nicht statisch hinzugefügt, sondern dynamisch behandelt (2-Tier System).
    SchemaInformedElement,
}

/// Eine EXI Grammar (Liste von Productions).
///
/// # Spec-Referenz
/// - 8. EXI Grammars
/// - 8.3 Pruning Unneeded Productions
#[derive(Debug, Clone)]
pub struct Grammar {
    grammar_type: GrammarType,
    productions: Vec<Production>,
}

impl Grammar {
    /// Erstellt eine neue Grammar mit gegebenem Typ und Productions.
    pub fn new(grammar_type: GrammarType, productions: Vec<Production>) -> Self {
        Self {
            grammar_type,
            productions,
        }
    }

    /// Gibt den Grammar-Typ zurück.
    pub fn grammar_type(&self) -> GrammarType {
        self.grammar_type
    }

    /// Gibt die Productions als Slice zurück.
    pub fn productions(&self) -> &[Production] {
        &self.productions
    }

    /// Prunt Productions basierend auf Fidelity Options (Spec 8.3).
    ///
    /// Entfernt nicht-erlaubte Productions und berechnet Event Codes
    /// neu, sodass sie contiguous bleiben.
    ///
    /// # Errors
    ///
    /// Gibt `Error::InvalidOptionCombination` zurück wenn `options.validate()` fehlschlägt.
    ///
    /// # Spec-Referenz
    /// - 6.3 Fidelity Options
    /// - 8.3 Pruning Unneeded Productions
    /// - 8.5.4.1.1 Wildcards nur aus Element/Type Grammars entfernen, nicht aus Document
    pub fn prune(&mut self, options: &ExiOptions) -> crate::Result<()> {
        options.validate()?;

        self.productions
            .retain(|p| is_terminal_allowed(&p.terminal, options, self.grammar_type));
        recalculate_event_codes(&mut self.productions);
        Ok(())
    }

    /// Erzeugt den EventCodeContext für diese Grammar.
    ///
    /// # Spec-Referenz
    /// - 6.2 Representing Event Codes
    pub fn event_code_context(&self) -> EventCodeContext {
        compute_event_code_context(&self.productions)
    }
}

// ============================================================================
// GrammarKey + GrammarCache (gemeinsam für Encoder + Decoder)
// ============================================================================

/// Key für Element-Grammars: global (nur ExpandedName) oder lokal (ExpandedName + ID).
///
/// Verwendet [`ExpandedNameId`] statt [`QName`] — Copy-Type, Hash/Eq über u32-Vergleiche
/// statt String-Vergleiche.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct GrammarKey {
    pub(crate) qname: ExpandedNameId,
    pub(crate) local_id: Option<u64>,
}

const GRAMMAR_CACHE_SLOTS: usize = 32;

/// Direct-Mapped Grammar-Cache (32 Slots, Slot = XOR-Hash von URI+LocalName).
/// Vermeidet HashMap-Hash+Probe bei wiederholten Grammar-Lookups.
/// Lazy per-Entry Epoch: statt alle Slots bei Epoch-Wechsel zu nullen,
/// wird pro Eintrag die Epoch gespeichert und bei Lookup geprüft.
#[derive(Clone)]
pub(crate) struct GrammarCache {
    entries: [Option<GrammarCacheEntry>; GRAMMAR_CACHE_SLOTS],
}

#[derive(Clone)]
struct GrammarCacheEntry {
    key: GrammarKey,
    grammar: Rc<GrammarSystem>,
    epoch: u64,
}

impl GrammarCache {
    pub(crate) fn new() -> Self {
        Self {
            entries: [const { None }; GRAMMAR_CACHE_SLOTS],
        }
    }

    /// Lookup mit Epoch-Validierung. Gibt bei Hit `Some(grammar)` zurück,
    /// bei Miss oder Epoch-Mismatch `None`.
    /// Stale Einträge werden lazy beim nächsten `insert` überschrieben.
    #[inline]
    pub(crate) fn lookup(
        &self,
        key: &GrammarKey,
        current_epoch: u64,
    ) -> Option<Rc<GrammarSystem>> {
        let slot = Self::slot(key);
        if let Some(ref entry) = self.entries[slot] {
            if entry.epoch == current_epoch && entry.key == *key {
                return Some(Rc::clone(&entry.grammar));
            }
        }
        None
    }

    /// Speichert ein Grammar im Cache nach einem HashMap-Lookup (Miss-Pfad).
    #[inline]
    pub(crate) fn insert(
        &mut self,
        key: GrammarKey,
        grammar: &Rc<GrammarSystem>,
        current_epoch: u64,
    ) {
        self.entries[Self::slot(&key)] = Some(GrammarCacheEntry {
            key,
            grammar: Rc::clone(grammar),
            epoch: current_epoch,
        });
    }

    #[inline]
    fn slot(key: &GrammarKey) -> usize {
        let mut h = key.qname.uri.0 ^ key.qname.local_name.0;
        if let Some(id) = key.local_id {
            h ^= id as u32;
        }
        h as usize % GRAMMAR_CACHE_SLOTS
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::options::Preserve;

    // ========================================================================
    // Test-Helper für Terminal-Erstellung
    // ========================================================================

    /// Erstellt einen neuen StringInterner für Tests.
    fn test_interner() -> StringInterner {
        StringInterner::new()
    }

    /// Erstellt einen SE(qname)-Terminal für Tests.
    fn se(uri: &str, local: &str, interner: &mut StringInterner) -> Terminal {
        Terminal::se_qname(&QName::new(uri, local), interner).unwrap()
    }

    /// Erstellt einen AT(qname)-Terminal für Tests.
    fn at(uri: &str, local: &str, interner: &mut StringInterner) -> Terminal {
        Terminal::at_qname(&QName::new(uri, local), interner).unwrap()
    }

    /// Erstellt einen AT(xsi:type)-Terminal für Tests.
    fn at_xsi_type(interner: &mut StringInterner) -> Terminal {
        Terminal::at_qname(&QName::xsi_type(), interner).unwrap()
    }

    /// Erstellt einen AT(xsi:nil)-Terminal für Tests.
    fn at_xsi_nil(interner: &mut StringInterner) -> Terminal {
        Terminal::at_qname(&QName::xsi_nil(), interner).unwrap()
    }

    /// Test-Helfer: Delegiert an `NonTerminal::effective_event_code()`.
    fn effective_code(nt: &NonTerminal, index: usize) -> Option<EventCode> {
        nt.effective_event_code(index)
    }

    // ========================================================================
    // Tests: AttributeKind (Spec 8.5.4.1.3.2)
    // ========================================================================

    /// Spec 8.5.4.1.3.2: AT(uri:*) für Namespace-Constraints bei Attribute Wildcards
    #[test]
    fn attribute_kind_namespace_wildcard() {
        let mut interner = test_interner();
        let id = interner.intern("http://example.org").unwrap();
        let attr = AttributeKind::NamespaceWildcard(id);
        match attr {
            AttributeKind::NamespaceWildcard(uri) => {
                assert_eq!(interner.resolve(uri), "http://example.org");
            }
            _ => panic!("Expected NamespaceWildcard"),
        }
    }

    /// Spec 8.5.4.1.3.2: Leerer Namespace (absent) wird als "" dargestellt
    #[test]
    fn attribute_kind_namespace_wildcard_absent() {
        let mut interner = test_interner();
        let id = interner.intern("").unwrap();
        let attr = AttributeKind::NamespaceWildcard(id);
        match attr {
            AttributeKind::NamespaceWildcard(uri) => {
                assert_eq!(interner.resolve(uri), "");
            }
            _ => panic!("Expected NamespaceWildcard"),
        }
    }

    /// AttributeKind-Varianten sind distinct
    #[test]
    fn attribute_kind_variants_distinct() {
        let mut interner = test_interner();
        let wildcard = AttributeKind::Wildcard;
        let ns_id = interner.intern("http://a.org").unwrap();
        let ns_wildcard = AttributeKind::NamespaceWildcard(ns_id);
        let qname = at("http://a.org", "attr", &mut interner);
        let qname_kind = match qname {
            Terminal::Attribute(kind) => kind,
            _ => unreachable!(),
        };

        // Alle sind unterschiedlich
        assert_ne!(
            std::mem::discriminant(&wildcard),
            std::mem::discriminant(&ns_wildcard)
        );
        assert_ne!(
            std::mem::discriminant(&wildcard),
            std::mem::discriminant(&qname_kind)
        );
        assert_ne!(
            std::mem::discriminant(&ns_wildcard),
            std::mem::discriminant(&qname_kind)
        );
    }

    /// NamespaceWildcard mit unterschiedlichen URIs sind ungleich
    #[test]
    fn attribute_kind_namespace_wildcard_different_uris() {
        let mut interner = test_interner();
        let a = AttributeKind::NamespaceWildcard(interner.intern("http://a.org").unwrap());
        let b = AttributeKind::NamespaceWildcard(interner.intern("http://b.org").unwrap());
        assert_ne!(a, b);
    }

    /// NamespaceWildcard mit gleicher URI sind gleich
    #[test]
    fn attribute_kind_namespace_wildcard_same_uri() {
        let mut interner = test_interner();
        let a = AttributeKind::NamespaceWildcard(interner.intern("http://example.org").unwrap());
        let b = AttributeKind::NamespaceWildcard(interner.intern("http://example.org").unwrap());
        assert_eq!(a, b);
    }

    // ========================================================================
    // Tests: NonTerminalId (Spec 8.4)
    // ========================================================================

    /// Spec 8.4.1: Document Grammar NonTerminals existieren
    #[test]
    fn non_terminal_id_document_grammar_variants() {
        let ids = [
            NonTerminalId::Document,
            NonTerminalId::DocContent,
            NonTerminalId::DocEnd,
        ];
        // Alle IDs sind distinct
        for (i, id1) in ids.iter().enumerate() {
            for (j, id2) in ids.iter().enumerate() {
                if i == j {
                    assert_eq!(id1, id2);
                } else {
                    assert_ne!(id1, id2);
                }
            }
        }
    }

    /// Spec 8.4.2: Fragment Grammar NonTerminals existieren
    #[test]
    fn non_terminal_id_fragment_grammar_variants() {
        let ids = [NonTerminalId::Fragment, NonTerminalId::FragmentContent];
        assert_ne!(ids[0], ids[1]);
    }

    /// Spec 8.4.3: Element Grammar NonTerminals existieren
    #[test]
    fn non_terminal_id_element_grammar_variants() {
        let ids = [
            NonTerminalId::StartTagContent,
            NonTerminalId::ElementContent,
        ];
        assert_ne!(ids[0], ids[1]);
    }

    /// NonTerminalId implementiert Copy
    #[test]
    fn non_terminal_id_is_copy() {
        let id = NonTerminalId::Document;
        let copy = id; // Copy, nicht Move
        assert_eq!(id, copy);
    }

    /// NonTerminalId implementiert Hash (für HashMap/IndexMap)
    #[test]
    fn non_terminal_id_is_hashable() {
        use crate::FastHashSet;
        let mut set = FastHashSet::default();
        set.insert(NonTerminalId::Document);
        set.insert(NonTerminalId::DocContent);
        set.insert(NonTerminalId::Document); // Duplikat
        assert_eq!(set.len(), 2);
    }

    /// NonTerminalId Debug-Output enthält Variant-Name
    #[test]
    fn non_terminal_id_debug() {
        let id = NonTerminalId::StartTagContent;
        let debug = format!("{:?}", id);
        assert!(debug.contains("StartTagContent"));
    }

    /// Spec 8.5.4.4.1: Content2 Variant existiert
    #[test]
    fn non_terminal_id_content2_equality() {
        let c2_0 = NonTerminalId::Content2(0);
        let c2_0_copy = NonTerminalId::Content2(0);
        let c2_1 = NonTerminalId::Content2(1);

        assert_eq!(c2_0, c2_0_copy);
        assert_ne!(c2_0, c2_1);
    }

    /// Spec 8.5.4.4.1: Content2 Debug-Output enthält Index
    #[test]
    fn non_terminal_id_content2_debug() {
        let id = NonTerminalId::Content2(42);
        let debug = format!("{:?}", id);
        assert!(debug.contains("Content2"));
        assert!(debug.contains("42"));
    }

    /// Spec 8.5.4.4.1: Content2 ist distinct von Dynamic
    #[test]
    fn non_terminal_id_content2_distinct_from_dynamic() {
        let content2 = NonTerminalId::Content2(5);
        let dynamic = NonTerminalId::Dynamic(5);
        assert_ne!(content2, dynamic);
    }

    // ========================================================================
    // Tests: NonTerminal (Spec 8.4)
    // ========================================================================

    /// NonTerminal::new erstellt NonTerminal mit ID und Productions
    #[test]
    fn non_terminal_new() {
        let nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );
        assert_eq!(nt.id(), NonTerminalId::ElementContent);
        assert_eq!(nt.productions().len(), 2);
    }

    /// NonTerminal::find_by_terminal findet Production
    #[test]
    fn non_terminal_find_by_terminal() {
        let nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                    Some(NonTerminalId::ElementContent),
                ),
                Production::new(
                    Terminal::Characters,
                    EventCode::two(1, 1),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let se = nt.find_by_terminal(&SE_WILDCARD);
        assert!(se.is_some());
        assert_eq!(se.unwrap().event_code, Some(EventCode::two(1, 0)));

        let ch = nt.find_by_terminal(&Terminal::Characters);
        assert!(ch.is_some());

        let cm = nt.find_by_terminal(&Terminal::Comment);
        assert!(cm.is_none());
    }

    /// NonTerminal::find_by_event_code findet Production
    #[test]
    fn non_terminal_find_by_event_code() {
        let nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let prod = nt.find_by_event_code(&EventCode::one(0));
        assert!(prod.is_some());
        assert_eq!(prod.unwrap().terminal, Terminal::EndElement);

        let prod = nt.find_by_event_code(&EventCode::two(1, 0));
        assert!(prod.is_some());
        assert_eq!(prod.unwrap().terminal, SE_WILDCARD);

        let prod = nt.find_by_event_code(&EventCode::one(99));
        assert!(prod.is_none());
    }

    /// NonTerminal::event_code_context berechnet korrekten Context
    #[test]
    fn non_terminal_event_code_context() {
        let nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                    Some(NonTerminalId::ElementContent),
                ),
                Production::new(
                    Terminal::Characters,
                    EventCode::two(1, 1),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let ctx = nt.event_code_context();
        // Part 1: max=1, num_values=2
        assert_eq!(ctx.num_values_part1(), 2);
        // Part 2: max=1, num_values=2
        assert_eq!(ctx.max_num_values_part2(), 2);
        assert_eq!(ctx.max_num_values_part3(), 0);
    }

    /// Leeres NonTerminal ergibt Context mit 0 values
    #[test]
    fn non_terminal_empty_context() {
        let nt = NonTerminal::new(NonTerminalId::DocEnd, vec![]);
        let ctx = nt.event_code_context();
        assert_eq!(ctx.num_values_part1(), 0);
    }

    // ========================================================================
    // Tests: NonTerminal Mutation API (Spec 8.4.2)
    // ========================================================================

    /// Spec 8.4.2: has_terminal findet Wildcard
    #[test]
    fn non_terminal_has_terminal_wildcard() {
        let nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::terminal(Terminal::EndDocument, EventCode::one(1)),
            ],
        );
        assert!(nt.has_terminal(&SE_WILDCARD));
        assert!(nt.has_terminal(&Terminal::EndDocument));
        assert!(!nt.has_terminal(&Terminal::Comment));
    }

    /// Spec 8.4.2: has_terminal unterscheidet SE(*) von SE(qname)
    #[test]
    fn non_terminal_has_terminal_qname_vs_wildcard() {
        let mut interner = test_interner();
        let nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    se("", "book", &mut interner),
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(1),
                    Some(NonTerminalId::FragmentContent),
                ),
            ],
        );

        // SE(book) vorhanden
        assert!(nt.has_terminal(&se("", "book", &mut interner)));

        // SE(*) vorhanden
        assert!(nt.has_terminal(&SE_WILDCARD));

        // SE(chapter) nicht vorhanden
        assert!(!nt.has_terminal(&se("", "chapter", &mut interner)));
    }

    /// Spec 8.4.3: has_terminal unterscheidet AT(*) von AT(qname)
    #[test]
    fn non_terminal_has_terminal_attribute_qname_vs_wildcard() {
        let mut interner = test_interner();
        let nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::new(
                    at("", "id", &mut interner),
                    EventCode::one(0),
                    Some(NonTerminalId::StartTagContent),
                ),
                Production::new(
                    AT_WILDCARD,
                    EventCode::one(1),
                    Some(NonTerminalId::StartTagContent),
                ),
            ],
        );

        // AT(id) vorhanden
        assert!(nt.has_terminal(&at("", "id", &mut interner)));

        // AT(*) vorhanden
        assert!(nt.has_terminal(&AT_WILDCARD));

        // AT(class) nicht vorhanden
        assert!(!nt.has_terminal(&at("", "class", &mut interner)));
    }

    // === has_terminal_with_length_one Tests (Spec 8.4.3) ===

    /// Spec 8.4.3: EE mit 2-Teil-Code hat nicht Length 1
    #[test]
    fn has_terminal_with_length_one_ee_two_part() {
        // StartTagContent: EE [0.0] hat Length 2
        let nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![Production::terminal(
                Terminal::EndElement,
                EventCode::two(0, 0),
            )],
        );
        assert!(!nt.has_terminal_with_length_one(&Terminal::EndElement));
    }

    /// Spec 8.4.3: EE mit 1-Teil-Code hat Length 1
    #[test]
    fn has_terminal_with_length_one_ee_one_part() {
        // ElementContent: EE [0] hat Length 1
        let nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![Production::terminal(
                Terminal::EndElement,
                EventCode::one(0),
            )],
        );
        assert!(nt.has_terminal_with_length_one(&Terminal::EndElement));
    }

    /// Spec 8.4.3: CH mit 2-Teil-Code hat nicht Length 1
    #[test]
    fn has_terminal_with_length_one_ch_two_part() {
        // StartTagContent: CH [0.5] hat Length 2
        let nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![Production::new(
                Terminal::Characters,
                EventCode::two(0, 5),
                Some(NonTerminalId::ElementContent),
            )],
        );
        assert!(!nt.has_terminal_with_length_one(&Terminal::Characters));
    }

    /// Spec 8.4.3: Kein Terminal vorhanden → false
    #[test]
    fn has_terminal_with_length_one_not_present() {
        let nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![Production::terminal(
                Terminal::EndElement,
                EventCode::one(0),
            )],
        );
        // CH nicht vorhanden
        assert!(!nt.has_terminal_with_length_one(&Terminal::Characters));
    }

    /// Spec 8.4.3: prepend_learned_production in leere Grammar
    #[test]
    fn non_terminal_prepend_first_production() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(NonTerminalId::FragmentContent, vec![]);

        nt.prepend_learned_production(se("", "book", &mut interner), Some(NonTerminalId::FragmentContent));

        assert_eq!(nt.productions().len(), 1);
        assert_eq!(nt.productions()[0].event_code, Some(EventCode::one(0)));
    }

    /// Spec 8.4.2: prepend_learned_production inkrementiert bestehende Event Codes
    #[test]
    fn non_terminal_prepend_increments_existing() {
        let mut interner = test_interner();
        // Initiale Fragment Grammar: SE(*) [0], ED [1], CM [2.0], PI [2.1]
        let mut nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::terminal(Terminal::EndDocument, EventCode::one(1)),
                Production::new(
                    Terminal::Comment,
                    EventCode::two(2, 0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::new(
                    Terminal::ProcessingInstr,
                    EventCode::two(2, 1),
                    Some(NonTerminalId::FragmentContent),
                ),
            ],
        );

        // Element "book" lernen
        nt.prepend_learned_production(se("", "book", &mut interner), Some(NonTerminalId::FragmentContent));

        // Physisch: [SE(*), ED, CM, PI, SE(book)]
        // Effektive Codes: SE(*) [1], ED [2], CM [3.0], PI [3.1], SE(book) [0]
        assert_eq!(nt.productions().len(), 5);
        assert_eq!(nt.productions()[0].terminal, SE_WILDCARD);
        assert_eq!(effective_code(&nt, 0), Some(EventCode::one(1)));
        assert_eq!(nt.productions()[1].terminal, Terminal::EndDocument);
        assert_eq!(effective_code(&nt, 1), Some(EventCode::one(2)));
        assert_eq!(nt.productions()[2].terminal, Terminal::Comment);
        assert_eq!(effective_code(&nt, 2), Some(EventCode::two(3, 0)));
        assert_eq!(nt.productions()[3].terminal, Terminal::ProcessingInstr);
        assert_eq!(effective_code(&nt, 3), Some(EventCode::two(3, 1)));
        assert_eq!(nt.productions()[4].terminal, se("", "book", &mut interner));
        assert_eq!(effective_code(&nt, 4), Some(EventCode::one(0)));
    }

    /// Spec 8.4.2: Zweites Element lernen führt zu weiterer Inkrementierung
    #[test]
    fn non_terminal_prepend_second_element() {
        let mut interner = test_interner();
        // Nach erstem Lernen: SE(book) [0], SE(*) [1], ED [2]
        let mut nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    se("", "book", &mut interner),
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(1),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::terminal(Terminal::EndDocument, EventCode::one(2)),
            ],
        );

        // Zweites Element "chapter" lernen
        nt.prepend_learned_production(se("", "chapter", &mut interner), Some(NonTerminalId::FragmentContent));

        // Physisch: [SE(book), SE(*), ED, SE(chapter)]
        // Effektive Codes: SE(book) [1], SE(*) [2], ED [3], SE(chapter) [0]
        assert_eq!(nt.productions().len(), 4);
        assert_eq!(nt.productions()[0].terminal, se("", "book", &mut interner));
        assert_eq!(effective_code(&nt, 0), Some(EventCode::one(1)));
        assert_eq!(nt.productions()[1].terminal, SE_WILDCARD);
        assert_eq!(effective_code(&nt, 1), Some(EventCode::one(2)));
        assert_eq!(nt.productions()[2].terminal, Terminal::EndDocument);
        assert_eq!(effective_code(&nt, 2), Some(EventCode::one(3)));
        assert_eq!(nt.productions()[3].terminal, se("", "chapter", &mut interner));
        assert_eq!(effective_code(&nt, 3), Some(EventCode::one(0)));
    }

    /// Spec 8.4.2: Bereits gelerntes Element führt zu keiner Mutation
    #[test]
    fn non_terminal_no_mutation_for_known_element() {
        let mut interner = test_interner();
        let nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    se("", "book", &mut interner),
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(1),
                    Some(NonTerminalId::FragmentContent),
                ),
            ],
        );

        // Prüfen vor Mutation
        assert!(nt.has_terminal(&se("", "book", &mut interner)));

        // Wichtig: Caller muss has_terminal() prüfen und bei true NICHT mutieren
        // Dieses Verhalten ist nicht in prepend_learned_production eingebaut,
        // da der Caller die Semantik kontrolliert
    }

    /// Spec 8.4.2: learn_production lernt neues Element
    #[test]
    fn non_terminal_learn_production_new_element() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::terminal(Terminal::EndDocument, EventCode::one(1)),
            ],
        );

        // Erstes Auftreten: sollte lernen
        let learned = nt.learn_production(se("", "book", &mut interner), Some(NonTerminalId::FragmentContent));
        assert!(learned);
        assert_eq!(nt.productions().len(), 3);

        // Physisch: [SE(*), ED, SE(book)] — gelernte am Ende
        // Bestehende Productions haben effektiv inkrementierte Codes
        assert_eq!(nt.productions()[0].terminal, SE_WILDCARD);
        assert_eq!(effective_code(&nt, 0), Some(EventCode::one(1))); // war 0, jetzt 1
        assert_eq!(nt.productions()[1].terminal, Terminal::EndDocument);
        assert_eq!(effective_code(&nt, 1), Some(EventCode::one(2))); // war 1, jetzt 2

        // Neue Production hat effektiven Event Code 0 (am Ende)
        assert_eq!(nt.productions()[2].terminal, se("", "book", &mut interner));
        assert_eq!(effective_code(&nt, 2), Some(EventCode::one(0)));
    }

    /// Spec 8.4.2: learn_production ignoriert bekanntes Element
    #[test]
    fn non_terminal_learn_production_known_element() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    se("", "book", &mut interner),
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(1),
                    Some(NonTerminalId::FragmentContent),
                ),
            ],
        );

        // Bekanntes Element: sollte NICHT lernen
        let learned = nt.learn_production(se("", "book", &mut interner), Some(NonTerminalId::FragmentContent));
        assert!(!learned);
        assert_eq!(nt.productions().len(), 2); // Keine Änderung
    }

    // === learn_content_production Tests (Spec 8.4.3) ===

    /// Spec 8.4.3: EE mit 2-Teil-Code → neues EE mit Code 0 lernen
    #[test]
    fn learn_content_production_ee_two_part_learns() {
        // StartTagContent mit EE [0.0]
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::two(0, 0)),
                Production::new(
                    AT_WILDCARD,
                    EventCode::two(0, 1),
                    Some(NonTerminalId::StartTagContent),
                ),
            ],
        );

        let learned = nt.learn_content_production(Terminal::EndElement, None);

        assert!(learned);
        assert_eq!(nt.productions().len(), 3);
        // Physisch: [EE(alt), AT(*), EE(neu)] — gelernte am Ende
        // Altes EE [1.0] (effektiv inkrementiert)
        assert_eq!(nt.productions()[0].terminal, Terminal::EndElement);
        assert_eq!(effective_code(&nt, 0), Some(EventCode::two(1, 0)));
        // AT(*) [1.1]
        assert_eq!(nt.productions()[1].terminal, AT_WILDCARD);
        assert_eq!(effective_code(&nt, 1), Some(EventCode::two(1, 1)));
        // Neues EE [0] am Ende
        assert_eq!(nt.productions()[2].terminal, Terminal::EndElement);
        assert_eq!(effective_code(&nt, 2), Some(EventCode::one(0)));
    }

    /// Spec 8.4.3: EE mit 1-Teil-Code → keine Änderung
    #[test]
    fn learn_content_production_ee_one_part_no_change() {
        // ElementContent mit EE [0]
        let mut nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    SE_WILDCARD,
                    EventCode::two(1, 0),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let learned = nt.learn_content_production(Terminal::EndElement, None);

        assert!(!learned); // Bereits Length 1
        assert_eq!(nt.productions().len(), 2); // Keine Änderung
    }

    /// Spec 8.4.3: CH mit 2-Teil-Code → neues CH mit Code 0 lernen
    #[test]
    fn learn_content_production_ch_two_part_learns() {
        // StartTagContent mit CH [0.5]
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::two(0, 0)),
                Production::new(
                    Terminal::Characters,
                    EventCode::two(0, 5),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let learned =
            nt.learn_content_production(Terminal::Characters, Some(NonTerminalId::ElementContent));

        assert!(learned);
        assert_eq!(nt.productions().len(), 3);
        // Physisch: [EE, CH(alt), CH(neu)] — gelernte am Ende
        // EE [1.0]
        assert_eq!(nt.productions()[0].terminal, Terminal::EndElement);
        assert_eq!(effective_code(&nt, 0), Some(EventCode::two(1, 0)));
        // Altes CH [1.5]
        assert_eq!(nt.productions()[1].terminal, Terminal::Characters);
        assert_eq!(effective_code(&nt, 1), Some(EventCode::two(1, 5)));
        // Neues CH [0]
        assert_eq!(nt.productions()[2].terminal, Terminal::Characters);
        assert_eq!(effective_code(&nt, 2), Some(EventCode::one(0)));
    }

    /// Spec 8.4.3: CH in ElementContent mit 2-Teil-Code → lernen
    #[test]
    fn learn_content_production_ch_element_content() {
        // ElementContent: EE [0], CH [1.1]
        let mut nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    Terminal::Characters,
                    EventCode::two(1, 1),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let learned =
            nt.learn_content_production(Terminal::Characters, Some(NonTerminalId::ElementContent));

        assert!(learned);
        assert_eq!(nt.productions().len(), 3);
        // Physisch: [EE, CH(alt), CH(neu)] — gelernte am Ende
        // EE [1]
        assert_eq!(nt.productions()[0].terminal, Terminal::EndElement);
        assert_eq!(effective_code(&nt, 0), Some(EventCode::one(1)));
        // Altes CH [2.1]
        assert_eq!(nt.productions()[1].terminal, Terminal::Characters);
        assert_eq!(effective_code(&nt, 1), Some(EventCode::two(2, 1)));
        // Neues CH [0]
        assert_eq!(nt.productions()[2].terminal, Terminal::Characters);
        assert_eq!(effective_code(&nt, 2), Some(EventCode::one(0)));
    }

    /// Spec 8.4.3: CH mit 3-Teil-Code → neues CH mit Code 0 lernen
    #[test]
    fn learn_content_production_ch_three_part_learns() {
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    Terminal::Characters,
                    EventCode::three(1, 3, 0),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let learned =
            nt.learn_content_production(Terminal::Characters, Some(NonTerminalId::ElementContent));

        assert!(learned);
        // Physisch: [EE, CH(alt), CH(neu)] — gelernte am Ende
        assert_eq!(nt.productions()[2].terminal, Terminal::Characters);
        assert_eq!(effective_code(&nt, 2), Some(EventCode::one(0)));
        // Altes CH [2.3.0] (first part effektiv inkrementiert)
        assert_eq!(effective_code(&nt, 1), Some(EventCode::three(2, 3, 0)));
    }

    // === Duplikat-Lookup Regressionstests (learned-at-end Layout) ===

    /// Nach learn_content_production(CH) muss find_by_terminal_with_code
    /// die gelernte CH-Production (Code 0) finden, nicht die originale.
    #[test]
    fn find_by_terminal_prefers_learned_ch() {
        // ElementContent: EE [0], CH [1.1]
        let mut nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    Terminal::Characters,
                    EventCode::two(1, 1),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        nt.learn_content_production(Terminal::Characters, Some(NonTerminalId::ElementContent));

        // Lookup muss gelernte CH (Code 0) finden, nicht originale CH (Code 2.1)
        let (_, ec) = nt.find_by_terminal_with_code(&Terminal::Characters).unwrap();
        assert_eq!(ec, EventCode::one(0));
    }

    /// Nach learn_content_production(EE) muss find_by_terminal_with_code
    /// die gelernte EE-Production (Code 0) finden, nicht die originale.
    #[test]
    fn find_by_terminal_prefers_learned_ee() {
        // StartTagContent: EE [0.0], AT(*) [0.1]
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::two(0, 0)),
                Production::new(
                    AT_WILDCARD,
                    EventCode::two(0, 1),
                    Some(NonTerminalId::StartTagContent),
                ),
            ],
        );

        nt.learn_content_production(Terminal::EndElement, None);

        // Lookup muss gelernte EE (Code 0) finden, nicht originale EE (Code 1.0)
        let (_, ec) = nt.find_by_terminal_with_code(&Terminal::EndElement).unwrap();
        assert_eq!(ec, EventCode::one(0));
    }

    // === learn_attribute Tests (Spec 8.4.3) ===

    /// Spec 8.4.3: AT(*) matched mit "foo" → AT(foo) gelernt
    #[test]
    fn learn_attribute_normal() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::two(0, 0)),
                Production::new(
                    AT_WILDCARD,
                    EventCode::two(0, 1),
                    Some(NonTerminalId::StartTagContent),
                ),
            ],
        );

        let learned = nt.learn_attribute(at("", "foo", &mut interner), Some(NonTerminalId::StartTagContent), &interner);

        assert!(learned);
        assert_eq!(nt.productions().len(), 3);
        // AT(foo) am Ende [effektiv 0]
        assert_eq!(nt.productions()[2].terminal, at("", "foo", &mut interner));
        assert_eq!(nt.productions()[2].event_code, Some(EventCode::one(0)));
    }

    /// Spec 8.4.3: AT(*) matched mit "foo" zweites Mal → keine Änderung
    #[test]
    fn learn_attribute_already_known() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::new(
                    at("", "foo", &mut interner),
                    EventCode::one(0),
                    Some(NonTerminalId::StartTagContent),
                ),
                Production::new(
                    AT_WILDCARD,
                    EventCode::one(1),
                    Some(NonTerminalId::StartTagContent),
                ),
            ],
        );

        let learned = nt.learn_attribute(at("", "foo", &mut interner), Some(NonTerminalId::StartTagContent), &interner);

        assert!(!learned); // Bereits vorhanden
        assert_eq!(nt.productions().len(), 2);
    }

    /// Spec 8.4.3: AT(xsi:type) erstes Mal → lernen
    #[test]
    fn learn_attribute_xsi_type_first() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::two(0, 0)),
                Production::new(
                    AT_WILDCARD,
                    EventCode::two(0, 1),
                    Some(NonTerminalId::StartTagContent),
                ),
            ],
        );

        let learned = nt.learn_attribute(at_xsi_type(&mut interner), Some(NonTerminalId::StartTagContent), &interner);

        assert!(learned);
        // AT(xsi:type) am Ende [effektiv 0]
        assert_eq!(nt.productions()[2].terminal, at_xsi_type(&mut interner));
        assert_eq!(nt.productions()[2].event_code, Some(EventCode::one(0)));
    }

    /// Spec 8.4.3: AT(xsi:type) zweites Mal → keine Änderung (bereits Code-Length 1)
    #[test]
    fn learn_attribute_xsi_type_second() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::new(
                    at_xsi_type(&mut interner),
                    EventCode::one(0),
                    Some(NonTerminalId::StartTagContent),
                ),
                Production::new(
                    AT_WILDCARD,
                    EventCode::one(1),
                    Some(NonTerminalId::StartTagContent),
                ),
            ],
        );

        let learned = nt.learn_attribute(at_xsi_type(&mut interner), Some(NonTerminalId::StartTagContent), &interner);

        assert!(!learned); // Bereits mit Code-Length 1 vorhanden
        assert_eq!(nt.productions().len(), 2);
    }

    /// Spec 8.4.3: AT(xsi:type) mit existierendem 2-Teil-Code → neues lernen
    #[test]
    fn learn_attribute_xsi_type_with_two_part_code() {
        let mut interner = test_interner();
        // Szenario: AT(xsi:type) [1.0] existiert (Code-Length 2)
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::two(0, 0)),
                Production::new(
                    at_xsi_type(&mut interner),
                    EventCode::two(1, 0),
                    Some(NonTerminalId::StartTagContent),
                ),
            ],
        );

        // xsi:type mit 2-Teil-Code → sollte lernen (Code-Length != 1)
        let learned = nt.learn_attribute(at_xsi_type(&mut interner), Some(NonTerminalId::StartTagContent), &interner);

        assert!(learned);
        // Gelernte am Ende
        assert_eq!(nt.productions()[2].terminal, at_xsi_type(&mut interner));
        assert_eq!(nt.productions()[2].event_code, Some(EventCode::one(0)));
    }

    /// Spec 8.4.3: AT(xsi:nil) wird normal gelernt (keine Sonderbehandlung)
    #[test]
    fn learn_attribute_xsi_nil_normal() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::StartTagContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::two(0, 0)),
                Production::new(
                    AT_WILDCARD,
                    EventCode::two(0, 1),
                    Some(NonTerminalId::StartTagContent),
                ),
            ],
        );

        let learned = nt.learn_attribute(at_xsi_nil(&mut interner), Some(NonTerminalId::StartTagContent), &interner);

        assert!(learned);
        // Gelernte am Ende
        assert_eq!(nt.productions()[2].terminal, at_xsi_nil(&mut interner));
    }

    // === SE(*) Lernen in Element Grammar (Spec 8.4.3) ===

    /// Spec 8.4.3: SE(*) matched in ElementContent → SE(qname) gelernt
    #[test]
    fn learn_element_in_element_content() {
        let mut interner = test_interner();
        // ElementContent: EE [0], SE(*) [1.0], CH [1.1]
        let mut nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    SE_WILDCARD,
                    EventCode::two(1, 0),
                    Some(NonTerminalId::ElementContent),
                ),
                Production::new(
                    Terminal::Characters,
                    EventCode::two(1, 1),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let se_book = se("", "book", &mut interner);
        let learned = nt.learn_production(se_book.clone(), Some(NonTerminalId::ElementContent));

        assert!(learned);
        assert_eq!(nt.productions().len(), 4);
        // Physisch: [EE, SE(*), CH, SE(book)] — gelernte am Ende
        // EE [1]
        assert_eq!(effective_code(&nt, 0), Some(EventCode::one(1)));
        // SE(*) [2.0]
        assert_eq!(effective_code(&nt, 1), Some(EventCode::two(2, 0)));
        // CH [2.1]
        assert_eq!(effective_code(&nt, 2), Some(EventCode::two(2, 1)));
        // SE(book) [0]
        assert_eq!(nt.productions()[3].terminal, se_book);
        assert_eq!(effective_code(&nt, 3), Some(EventCode::one(0)));
    }

    /// Spec 8.4.3: SE(*) matched zweites Mal → keine Änderung
    #[test]
    fn learn_element_already_known() {
        let mut interner = test_interner();
        let se_book = se("", "book", &mut interner);
        let mut nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::new(
                    se_book.clone(),
                    EventCode::one(0),
                    Some(NonTerminalId::ElementContent),
                ),
                Production::terminal(Terminal::EndElement, EventCode::one(1)),
                Production::new(
                    SE_WILDCARD,
                    EventCode::two(2, 0),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let learned = nt.learn_production(se_book, Some(NonTerminalId::ElementContent));

        assert!(!learned); // Bereits vorhanden
        assert_eq!(nt.productions().len(), 3);
    }

    /// Spec 8.4.3: SE(qname) Learning ist idempotent bei mehrfacher Wiederholung
    #[test]
    fn learn_element_idempotent_multiple_occurrences() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::ElementContent,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::new(
                    SE_WILDCARD,
                    EventCode::two(1, 0),
                    Some(NonTerminalId::ElementContent),
                ),
            ],
        );

        let se_book = se("", "book", &mut interner);

        // Erstes Lernen
        assert!(nt.learn_production(se_book.clone(), Some(NonTerminalId::ElementContent)));
        let len_after_first = nt.productions().len();

        // Zweites, drittes Lernen - keine Änderung
        assert!(!nt.learn_production(se_book.clone(), Some(NonTerminalId::ElementContent)));
        assert!(!nt.learn_production(se_book.clone(), Some(NonTerminalId::ElementContent)));
        assert_eq!(nt.productions().len(), len_after_first);
    }

    // === Element Grammar Evolution Integrationstests (Spec 8.4.3) ===

    /// Spec 8.4.3: Vollständiger Durchlauf mit mehreren ATs und SEs
    #[test]
    fn element_grammar_evolution_integration() {
        let mut interner = test_interner();
        let options = ExiOptions::default();
        let mut gs = GrammarSystem::built_in_element(&options);

        // StartTagContent holen
        let stc = gs.get_mut(NonTerminalId::StartTagContent).unwrap();

        // AT(id) lernen
        let at_id = at("", "id", &mut interner);
        let learned_id = stc.learn_attribute(at_id.clone(), Some(NonTerminalId::StartTagContent), &interner);
        assert!(learned_id);
        // Gelernte am Ende
        let last = stc.productions().len() - 1;
        assert_eq!(stc.productions()[last].terminal, at_id);
        assert_eq!(effective_code(stc, last), Some(EventCode::one(0)));

        // AT(class) lernen
        let at_class = at("", "class", &mut interner);
        let learned_class =
            stc.learn_attribute(at_class.clone(), Some(NonTerminalId::StartTagContent), &interner);
        assert!(learned_class);
        // Neueste gelernte am Ende → AT(class) [0]
        let last = stc.productions().len() - 1;
        assert_eq!(stc.productions()[last].terminal, at_class);
        assert_eq!(effective_code(stc, last), Some(EventCode::one(0)));
        // AT(id) ist jetzt effektiv [1] (vorletzte)
        assert_eq!(stc.productions()[last - 1].terminal, at_id);
        assert_eq!(effective_code(stc, last - 1), Some(EventCode::one(1)));

        // ElementContent holen
        let ec = gs.get_mut(NonTerminalId::ElementContent).unwrap();

        // SE(div) lernen
        let se_div = se("", "div", &mut interner);
        let learned_div = ec.learn_production(se_div.clone(), Some(NonTerminalId::ElementContent));
        assert!(learned_div);
        let last = ec.productions().len() - 1;
        assert_eq!(ec.productions()[last].terminal, se_div);
        assert_eq!(effective_code(ec, last), Some(EventCode::one(0)));

        // CH lernen (hat 2-Teil-Code in ElementContent)
        let learned_ch =
            ec.learn_content_production(Terminal::Characters, Some(NonTerminalId::ElementContent));
        assert!(learned_ch);
        let last = ec.productions().len() - 1;
        assert_eq!(ec.productions()[last].terminal, Terminal::Characters);
        assert_eq!(effective_code(ec, last), Some(EventCode::one(0)));
    }

    /// Spec 8.4.3: Event Codes nach mehreren Lernvorgängen
    #[test]
    fn element_grammar_evolution_event_codes() {
        let mut interner = test_interner();
        let options = ExiOptions::default();
        let mut gs = GrammarSystem::built_in_element(&options);

        let stc = gs.get_mut(NonTerminalId::StartTagContent).unwrap();

        // 5 Attribute lernen
        for name in ["a", "b", "c", "d", "e"] {
            let at_term = at("", name, &mut interner);
            stc.learn_attribute(at_term, Some(NonTerminalId::StartTagContent), &interner);
        }

        // Gelernte Attribute am Ende: älteste zuerst, neueste zuletzt
        // Physisch: [...originals..., AT(a), AT(b), AT(c), AT(d), AT(e)]
        // Effektive Codes: neueste (e) = 0, ..., älteste (a) = 4
        let len = stc.productions().len();
        for (i, name) in ["e", "d", "c", "b", "a"].iter().enumerate() {
            let expected_terminal = at("", name, &mut interner);
            let idx = len - 1 - i; // neueste am Ende
            assert_eq!(stc.productions()[idx].terminal, expected_terminal);
            assert_eq!(effective_code(stc, idx), Some(EventCode::one(i as u32)));
        }
    }

    /// Spec 8.4.2: Event Codes nach Mutation sind contiguous (0, 1, 2, ...)
    #[test]
    fn non_terminal_event_codes_contiguous_after_mutation() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::terminal(Terminal::EndDocument, EventCode::one(1)),
            ],
        );

        // Drei Elemente lernen
        for name in ["a", "b", "c"] {
            let terminal = se("", name, &mut interner);
            nt.prepend_learned_production(terminal, Some(NonTerminalId::FragmentContent));
        }

        // Effektive Event Codes prüfen: Alle Werte 0..4 müssen genau einmal vorkommen
        let mut codes: Vec<u32> = (0..nt.productions().len())
            .map(|i| {
                let ec = effective_code(&nt, i)
                    .expect("Effektiver Event Code sollte existieren");
                assert!(ec.part2().is_none(), "Production {i} sollte 1-Teil Code haben");
                ec.part1()
            })
            .collect();
        codes.sort();
        for (i, code) in codes.iter().enumerate() {
            assert_eq!(
                *code,
                i as u32,
                "Event Code {i} fehlt oder doppelt",
            );
        }
    }

    /// Spec 8.4.2: EventCodeContext nach Mutation ist konsistent
    #[test]
    fn non_terminal_event_code_context_after_mutation() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::terminal(Terminal::EndDocument, EventCode::one(1)),
            ],
        );

        assert_eq!(nt.event_code_context().num_values_part1(), 2);

        // Element lernen
        let terminal = se("", "test", &mut interner);
        nt.prepend_learned_production(terminal, Some(NonTerminalId::FragmentContent));

        let ctx_after = nt.event_code_context();
        assert_eq!(ctx_after.num_values_part1(), 3);
    }

    /// Inkrementelles ProductionTable-Update liefert identische Table wie Full-Rebuild.
    ///
    /// Verifiziert, dass `update_production_table_incremental` (via prepend_level1)
    /// nach N gelernten Productions dasselbe Ergebnis wie `build_production_table` liefert.
    #[test]
    fn incremental_production_table_equals_full_rebuild() {
        let mut interner = test_interner();
        let mut nt = NonTerminal::new(
            NonTerminalId::FragmentContent,
            vec![
                Production::new(
                    SE_WILDCARD,
                    EventCode::one(0),
                    Some(NonTerminalId::FragmentContent),
                ),
                Production::terminal(Terminal::EndDocument, EventCode::one(1)),
            ],
        );
        nt.seal_production_table();

        let terminals = vec![
            se("", "alpha", &mut interner),
            se("", "beta", &mut interner),
            Terminal::EndElement,
            se("http://example.org", "gamma", &mut interner),
            Terminal::Characters,
        ];

        for terminal in &terminals {
            nt.prepend_learned_production(terminal.clone(), Some(NonTerminalId::FragmentContent));

            let incremental = nt.production_table().as_ref().clone();
            let full_rebuild = build_production_table(&nt);

            assert_eq!(
                incremental.level1_count(), full_rebuild.level1_count(),
                "level1_count weicht ab nach {} gelernten Productions",
                nt.learned_count
            );
            assert_eq!(
                incremental.bits_part1(), full_rebuild.bits_part1(),
                "bits_part1 weicht ab nach {} gelernten Productions",
                nt.learned_count
            );
            // Alle Level-1 Einträge vergleichen
            for i in 0..incremental.level1_count() {
                assert_eq!(
                    incremental.get_level1(i), full_rebuild.get_level1(i),
                    "Level-1 Eintrag {} weicht ab (learned_count={})",
                    i, nt.learned_count
                );
            }
        }
    }

    // ========================================================================
    // Tests: Document Grammar ist statisch (Spec 8.4.1)
    // ========================================================================

    /// Spec 8.4.1: Document Grammar hat SE(*) aber keine SE(qname) Productions
    #[test]
    fn document_grammar_has_se_wildcard_only() {
        let doc = GrammarSystem::built_in_document();
        let doc_content = doc.get(NonTerminalId::DocContent).unwrap();

        // SE(*) ist vorhanden
        assert!(doc_content.has_terminal(&SE_WILDCARD));

        // Keine SE(qname) Productions
        for p in doc_content.productions() {
            if let Terminal::StartElement(kind) = &p.terminal {
                assert!(
                    matches!(kind, StartElementKind::Wildcard),
                    "Document Grammar sollte nur SE(*) haben, nicht SE(qname)"
                );
            }
        }
    }

    /// Spec 8.4.1: Document Grammar mutiert nicht (konzeptioneller Test)
    #[test]
    fn document_grammar_conceptually_static() {
        // Dieser Test dokumentiert, dass Document Grammar keine Mutation unterstützt.
        // Es gibt keine SE(qname) Lern-Semantik für Document Grammar.
        // Die API erlaubt technisch Mutation, aber die Spec sagt das nicht vor.
        let doc = GrammarSystem::built_in_document();
        let doc_content = doc.get(NonTerminalId::DocContent).unwrap();

        // Initial 4 Productions: SE(*), DT, CM, PI
        assert_eq!(doc_content.productions().len(), 4);

        // Der Unterschied zu Fragment: Document evaluiert SE(*) und wechselt zu Element Grammar,
        // ohne die Document Grammar selbst zu mutieren.
    }

    // ========================================================================
    // Tests: EventCodeFormula (Spec 8.1.2)
    // ========================================================================

    /// EventCodeFormula::TwoPart mit offset=0 → n.m
    #[test]
    fn event_code_formula_two_part_no_offset() {
        let formula = EventCodeFormula::TwoPart { m_offset: 0 };
        let code = formula.evaluate(2, 3);
        assert_eq!(code, EventCode::two(2, 3));
    }

    /// EventCodeFormula::TwoPart mit offset=1 → n.(m+1)
    #[test]
    fn event_code_formula_two_part_with_offset() {
        let formula = EventCodeFormula::TwoPart { m_offset: 1 };
        let code = formula.evaluate(1, 0);
        assert_eq!(code, EventCode::two(1, 1));
    }

    /// EventCodeFormula::ThreePart → n.(m+offset).third
    #[test]
    fn event_code_formula_three_part() {
        let formula = EventCodeFormula::ThreePart {
            m_offset: 3,
            third: 0,
        };
        let code = formula.evaluate(0, 4);
        assert_eq!(code, EventCode::three(0, 7, 0));
    }

    /// EventCodeFormula für ChildContentItems(0.4) Beispiel
    /// SE(*) → n.m = 0.4
    #[test]
    fn event_code_formula_child_content_items_se() {
        let formula = EventCodeFormula::TwoPart { m_offset: 0 };
        let code = formula.evaluate(0, 4);
        assert_eq!(code, EventCode::two(0, 4));
    }

    /// EventCodeFormula für ChildContentItems(0.4) Beispiel
    /// CH → n.(m+1) = 0.5
    #[test]
    fn event_code_formula_child_content_items_ch() {
        let formula = EventCodeFormula::TwoPart { m_offset: 1 };
        let code = formula.evaluate(0, 4);
        assert_eq!(code, EventCode::two(0, 5));
    }

    /// EventCodeFormula für ChildContentItems(0.4) Beispiel
    /// CM → n.(m+3).0 = 0.7.0
    #[test]
    fn event_code_formula_child_content_items_cm() {
        let formula = EventCodeFormula::ThreePart {
            m_offset: 3,
            third: 0,
        };
        let code = formula.evaluate(0, 4);
        assert_eq!(code, EventCode::three(0, 7, 0));
    }

    /// EventCodeFormula für ChildContentItems(0.4) Beispiel
    /// PI → n.(m+3).1 = 0.7.1
    #[test]
    fn event_code_formula_child_content_items_pi() {
        let formula = EventCodeFormula::ThreePart {
            m_offset: 3,
            third: 1,
        };
        let code = formula.evaluate(0, 4);
        assert_eq!(code, EventCode::three(0, 7, 1));
    }

    /// EventCodeFormula für ChildContentItems(1.0) Beispiel
    /// SE(*) → 1.0, CH → 1.1, ER → 1.2, CM → 1.3.0, PI → 1.3.1
    #[test]
    fn event_code_formula_child_content_items_1_0() {
        // SE(*) → n.m = 1.0
        let se = EventCodeFormula::TwoPart { m_offset: 0 };
        assert_eq!(se.evaluate(1, 0), EventCode::two(1, 0));

        // CH → n.(m+1) = 1.1
        let ch = EventCodeFormula::TwoPart { m_offset: 1 };
        assert_eq!(ch.evaluate(1, 0), EventCode::two(1, 1));

        // ER → n.(m+2) = 1.2
        let er = EventCodeFormula::TwoPart { m_offset: 2 };
        assert_eq!(er.evaluate(1, 0), EventCode::two(1, 2));

        // CM → n.(m+3).0 = 1.3.0
        let cm = EventCodeFormula::ThreePart {
            m_offset: 3,
            third: 0,
        };
        assert_eq!(cm.evaluate(1, 0), EventCode::three(1, 3, 0));

        // PI → n.(m+3).1 = 1.3.1
        let pi = EventCodeFormula::ThreePart {
            m_offset: 3,
            third: 1,
        };
        assert_eq!(pi.evaluate(1, 0), EventCode::three(1, 3, 1));
    }

    // ========================================================================
    // Tests: MacroTemplate (Spec 8.1.2, 8.4.3)
    // ========================================================================

    /// MacroTemplate::child_content_items() erstellt korrektes Macro
    #[test]
    fn macro_template_child_content_items_structure() {
        let macro_t = MacroTemplate::child_content_items();
        assert_eq!(macro_t.productions.len(), 5);
    }

    /// ChildContentItems(0.4) → Codes 0.4, 0.5, 0.6, 0.7.0, 0.7.1
    #[test]
    fn macro_template_child_content_items_0_4() {
        let macro_t = MacroTemplate::child_content_items();
        let expanded = macro_t.expand(0, 4);

        assert_eq!(expanded.len(), 5);

        // SE(*) → 0.4
        assert_eq!(expanded[0].terminal, SE_WILDCARD);
        assert_eq!(expanded[0].event_code, Some(EventCode::two(0, 4)));
        assert_eq!(
            expanded[0].right_hand_side,
            Some(NonTerminalId::ElementContent)
        );

        // CH → 0.5
        assert_eq!(expanded[1].terminal, Terminal::Characters);
        assert_eq!(expanded[1].event_code, Some(EventCode::two(0, 5)));

        // ER → 0.6
        assert_eq!(expanded[2].terminal, Terminal::EntityRef);
        assert_eq!(expanded[2].event_code, Some(EventCode::two(0, 6)));

        // CM → 0.7.0
        assert_eq!(expanded[3].terminal, Terminal::Comment);
        assert_eq!(expanded[3].event_code, Some(EventCode::three(0, 7, 0)));

        // PI → 0.7.1
        assert_eq!(expanded[4].terminal, Terminal::ProcessingInstr);
        assert_eq!(expanded[4].event_code, Some(EventCode::three(0, 7, 1)));
    }

    /// ChildContentItems(1.0) → Codes 1.0, 1.1, 1.2, 1.3.0, 1.3.1
    #[test]
    fn macro_template_child_content_items_1_0() {
        let macro_t = MacroTemplate::child_content_items();
        let expanded = macro_t.expand(1, 0);

        assert_eq!(expanded.len(), 5);

        // SE(*) → 1.0
        assert_eq!(expanded[0].event_code, Some(EventCode::two(1, 0)));

        // CH → 1.1
        assert_eq!(expanded[1].event_code, Some(EventCode::two(1, 1)));

        // ER → 1.2
        assert_eq!(expanded[2].event_code, Some(EventCode::two(1, 2)));

        // CM → 1.3.0
        assert_eq!(expanded[3].event_code, Some(EventCode::three(1, 3, 0)));

        // PI → 1.3.1
        assert_eq!(expanded[4].event_code, Some(EventCode::three(1, 3, 1)));
    }

    /// ChildContentItems(0.3) → Codes 0.3, 0.4, 0.5, 0.6.0, 0.6.1
    /// (für selfContained=false, wo SC fehlt)
    #[test]
    fn macro_template_child_content_items_0_3() {
        let macro_t = MacroTemplate::child_content_items();
        let expanded = macro_t.expand(0, 3);

        // SE(*) → 0.3
        assert_eq!(expanded[0].event_code, Some(EventCode::two(0, 3)));

        // CH → 0.4
        assert_eq!(expanded[1].event_code, Some(EventCode::two(0, 4)));

        // ER → 0.5
        assert_eq!(expanded[2].event_code, Some(EventCode::two(0, 5)));

        // CM → 0.6.0
        assert_eq!(expanded[3].event_code, Some(EventCode::three(0, 6, 0)));

        // PI → 0.6.1
        assert_eq!(expanded[4].event_code, Some(EventCode::three(0, 6, 1)));
    }

    /// MacroProduction::expand generiert korrekte Production
    #[test]
    fn macro_production_expand() {
        let mp = MacroProduction::new(
            Terminal::Characters,
            EventCodeFormula::TwoPart { m_offset: 1 },
            NonTerminalId::ElementContent,
        );

        let prod = mp.expand(2, 5);
        assert_eq!(prod.terminal, Terminal::Characters);
        assert_eq!(prod.event_code, Some(EventCode::two(2, 6)));
        assert_eq!(prod.right_hand_side, Some(NonTerminalId::ElementContent));
    }

    // ========================================================================
    // Tests: GrammarSystem (Spec 8.4)
    // ========================================================================

    /// GrammarSystem::built_in_document() enthält alle NonTerminals
    #[test]
    fn grammar_system_built_in_document_structure() {
        let gs = GrammarSystem::built_in_document();
        assert_eq!(gs.grammar_type(), GrammarType::Document);
        assert_eq!(gs.start(), NonTerminalId::Document);
        assert!(gs.get(NonTerminalId::Document).is_some());
        assert!(gs.get(NonTerminalId::DocContent).is_some());
        assert!(gs.get(NonTerminalId::DocEnd).is_some());
    }

    /// Spec 8.4.1: Document Grammar - Document NonTerminal
    #[test]
    fn grammar_system_built_in_document_nt_document() {
        let gs = GrammarSystem::built_in_document();
        let document = gs.get(NonTerminalId::Document).unwrap();

        assert_eq!(document.productions().len(), 1);

        // SD → DocContent [0]
        let sd = document.find_by_terminal(&Terminal::StartDocument).unwrap();
        assert_eq!(sd.event_code, Some(EventCode::one(0)));
        assert_eq!(sd.right_hand_side, Some(NonTerminalId::DocContent));
    }

    /// Spec 8.4.1: Document Grammar - DocContent NonTerminal
    #[test]
    fn grammar_system_built_in_document_nt_doc_content() {
        let gs = GrammarSystem::built_in_document();
        let doc_content = gs.get(NonTerminalId::DocContent).unwrap();

        assert_eq!(doc_content.productions().len(), 4);

        // SE(*) → DocEnd [0]
        let se = doc_content.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se.event_code, Some(EventCode::one(0)));
        assert_eq!(se.right_hand_side, Some(NonTerminalId::DocEnd));

        // DT → DocContent [1.0]
        let dt = doc_content.find_by_terminal(&Terminal::DocType).unwrap();
        assert_eq!(dt.event_code, Some(EventCode::two(1, 0)));
        assert_eq!(dt.right_hand_side, Some(NonTerminalId::DocContent));

        // CM → DocContent [1.1.0]
        let cm = doc_content.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::three(1, 1, 0)));
        assert_eq!(cm.right_hand_side, Some(NonTerminalId::DocContent));

        // PI → DocContent [1.1.1]
        let pi = doc_content
            .find_by_terminal(&Terminal::ProcessingInstr)
            .unwrap();
        assert_eq!(pi.event_code, Some(EventCode::three(1, 1, 1)));
        assert_eq!(pi.right_hand_side, Some(NonTerminalId::DocContent));
    }

    /// Spec 8.4.1: Document Grammar - DocEnd NonTerminal
    #[test]
    fn grammar_system_built_in_document_nt_doc_end() {
        let gs = GrammarSystem::built_in_document();
        let doc_end = gs.get(NonTerminalId::DocEnd).unwrap();

        assert_eq!(doc_end.productions().len(), 3);

        // ED [0]
        let ed = doc_end.find_by_terminal(&Terminal::EndDocument).unwrap();
        assert_eq!(ed.event_code, Some(EventCode::one(0)));
        assert!(ed.right_hand_side.is_none()); // Terminal!

        // CM → DocEnd [1.0]
        let cm = doc_end.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::two(1, 0)));
        assert_eq!(cm.right_hand_side, Some(NonTerminalId::DocEnd));

        // PI → DocEnd [1.1]
        let pi = doc_end
            .find_by_terminal(&Terminal::ProcessingInstr)
            .unwrap();
        assert_eq!(pi.event_code, Some(EventCode::two(1, 1)));
        assert_eq!(pi.right_hand_side, Some(NonTerminalId::DocEnd));
    }

    /// GrammarSystem::built_in_fragment() enthält alle NonTerminals
    #[test]
    fn grammar_system_built_in_fragment_structure() {
        let gs = GrammarSystem::built_in_fragment();
        assert_eq!(gs.grammar_type(), GrammarType::Fragment);
        assert_eq!(gs.start(), NonTerminalId::Fragment);
        assert!(gs.get(NonTerminalId::Fragment).is_some());
        assert!(gs.get(NonTerminalId::FragmentContent).is_some());
    }

    /// Spec 8.4.2: Fragment Grammar - Fragment NonTerminal
    #[test]
    fn grammar_system_built_in_fragment_nt_fragment() {
        let gs = GrammarSystem::built_in_fragment();
        let fragment = gs.get(NonTerminalId::Fragment).unwrap();

        assert_eq!(fragment.productions().len(), 1);

        // SD → FragmentContent [0]
        let sd = fragment.find_by_terminal(&Terminal::StartDocument).unwrap();
        assert_eq!(sd.event_code, Some(EventCode::one(0)));
        assert_eq!(sd.right_hand_side, Some(NonTerminalId::FragmentContent));
    }

    /// Spec 8.4.2: Fragment Grammar - FragmentContent NonTerminal
    #[test]
    fn grammar_system_built_in_fragment_nt_fragment_content() {
        let gs = GrammarSystem::built_in_fragment();
        let fc = gs.get(NonTerminalId::FragmentContent).unwrap();

        assert_eq!(fc.productions().len(), 4);

        // SE(*) → FragmentContent [0]
        let se = fc.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se.event_code, Some(EventCode::one(0)));
        assert_eq!(se.right_hand_side, Some(NonTerminalId::FragmentContent));

        // ED [1]
        let ed = fc.find_by_terminal(&Terminal::EndDocument).unwrap();
        assert_eq!(ed.event_code, Some(EventCode::one(1)));
        assert!(ed.right_hand_side.is_none());

        // CM → FragmentContent [2.0]
        let cm = fc.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::two(2, 0)));
        assert_eq!(cm.right_hand_side, Some(NonTerminalId::FragmentContent));

        // PI → FragmentContent [2.1]
        let pi = fc.find_by_terminal(&Terminal::ProcessingInstr).unwrap();
        assert_eq!(pi.event_code, Some(EventCode::two(2, 1)));
        assert_eq!(pi.right_hand_side, Some(NonTerminalId::FragmentContent));
    }

    /// GrammarSystem::built_in_element() mit selfContained=true
    #[test]
    fn grammar_system_built_in_element_self_contained_true() {
        let options = ExiOptions {
            self_contained: true,

            ..Default::default()
        };
        let gs = GrammarSystem::built_in_element(&options);

        assert_eq!(gs.grammar_type(), GrammarType::Element);
        assert_eq!(gs.start(), NonTerminalId::StartTagContent);

        let stc = gs.get(NonTerminalId::StartTagContent).unwrap();

        // EE [0.0], AT(*) [0.1], NS [0.2], SC [0.3], SE(*) [0.4], CH [0.5], ER [0.6], CM [0.7.0], PI [0.7.1]
        assert_eq!(stc.productions().len(), 9);

        // SC → Fragment [0.3]
        let sc = stc.find_by_terminal(&Terminal::SelfContained).unwrap();
        assert_eq!(sc.event_code, Some(EventCode::two(0, 3)));
        assert_eq!(sc.right_hand_side, Some(NonTerminalId::Fragment));

        // SE(*) → ElementContent [0.4]
        let se = stc.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se.event_code, Some(EventCode::two(0, 4)));
    }

    /// GrammarSystem::built_in_element() mit selfContained=false (Spec 8.4.3 Note)
    #[test]
    fn grammar_system_built_in_element_self_contained_false() {
        let options = ExiOptions::default(); // self_contained = false
        let gs = GrammarSystem::built_in_element(&options);

        let stc = gs.get(NonTerminalId::StartTagContent).unwrap();

        // EE [0.0], AT(*) [0.1], NS [0.2], SE(*) [0.3], CH [0.4], ER [0.5], CM [0.6.0], PI [0.6.1]
        // KEIN SC!
        assert_eq!(stc.productions().len(), 8);

        // Kein SC
        assert!(stc.find_by_terminal(&Terminal::SelfContained).is_none());

        // SE(*) → ElementContent [0.3] (nicht 0.4!)
        let se = stc.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se.event_code, Some(EventCode::two(0, 3)));

        // CH → ElementContent [0.4]
        let ch = stc.find_by_terminal(&Terminal::Characters).unwrap();
        assert_eq!(ch.event_code, Some(EventCode::two(0, 4)));

        // CM → ElementContent [0.6.0] (nicht 0.7.0!)
        let cm = stc.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::three(0, 6, 0)));
    }

    /// Spec 8.4.3: Element Grammar - ElementContent NonTerminal
    #[test]
    fn grammar_system_built_in_element_nt_element_content() {
        let options = ExiOptions::default();
        let gs = GrammarSystem::built_in_element(&options);
        let ec = gs.get(NonTerminalId::ElementContent).unwrap();

        // EE [0], SE(*) [1.0], CH [1.1], ER [1.2], CM [1.3.0], PI [1.3.1]
        assert_eq!(ec.productions().len(), 6);

        // EE [0]
        let ee = ec.find_by_terminal(&Terminal::EndElement).unwrap();
        assert_eq!(ee.event_code, Some(EventCode::one(0)));
        assert!(ee.right_hand_side.is_none());

        // SE(*) → ElementContent [1.0]
        let se = ec.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se.event_code, Some(EventCode::two(1, 0)));
        assert_eq!(se.right_hand_side, Some(NonTerminalId::ElementContent));

        // CH → ElementContent [1.1]
        let ch = ec.find_by_terminal(&Terminal::Characters).unwrap();
        assert_eq!(ch.event_code, Some(EventCode::two(1, 1)));

        // ER → ElementContent [1.2]
        let er = ec.find_by_terminal(&Terminal::EntityRef).unwrap();
        assert_eq!(er.event_code, Some(EventCode::two(1, 2)));

        // CM → ElementContent [1.3.0]
        let cm = ec.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::three(1, 3, 0)));

        // PI → ElementContent [1.3.1]
        let pi = ec.find_by_terminal(&Terminal::ProcessingInstr).unwrap();
        assert_eq!(pi.event_code, Some(EventCode::three(1, 3, 1)));
    }

    /// GrammarSystem::event_code_context() delegiert an NonTerminal
    #[test]
    fn grammar_system_event_code_context() {
        let gs = GrammarSystem::built_in_document();
        let ctx = gs.event_code_context(NonTerminalId::DocContent).unwrap();

        // DocContent: [0], [1.0], [1.1.0], [1.1.1]
        // Part 1: max=1, num_values=2
        assert_eq!(ctx.num_values_part1(), 2);
        // Part 2: max=1, num_values=2
        assert_eq!(ctx.max_num_values_part2(), 2);
        // Part 3: max=1, num_values=2
        assert_eq!(ctx.max_num_values_part3(), 2);
    }

    /// GrammarSystem::get() gibt None für nicht existierende NonTerminals
    #[test]
    fn grammar_system_get_nonexistent() {
        let gs = GrammarSystem::built_in_document();
        // Document Grammar hat keine Element-NonTerminals
        assert!(gs.get(NonTerminalId::StartTagContent).is_none());
        assert!(gs.get(NonTerminalId::ElementContent).is_none());
    }

    // ========================================================================
    // Tests: Schema-informed Document Grammar (Spec 8.5.1)
    // ========================================================================

    /// Spec 8.5.1: Leeres Schema → nur SE(*), DT, CM, PI in DocContent
    #[test]
    fn schema_informed_document_empty_schema() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder().build();
        let gs = GrammarSystem::schema_informed_document(&schema, &mut StringInterner::new()).unwrap();

        assert_eq!(gs.grammar_type(), GrammarType::Document);
        assert_eq!(gs.start(), NonTerminalId::Document);

        let doc_content = gs.get(NonTerminalId::DocContent).unwrap();

        // Nur SE(*), DT, CM, PI (keine SE(qname))
        assert_eq!(doc_content.productions().len(), 4);

        // SE(*) → DocEnd [0] (n=0)
        let se = doc_content.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se.event_code, Some(EventCode::one(0)));

        // DT → DocContent [1.0] (n+1=1)
        let dt = doc_content.find_by_terminal(&Terminal::DocType).unwrap();
        assert_eq!(dt.event_code, Some(EventCode::two(1, 0)));

        // CM → DocContent [1.1.0]
        let cm = doc_content.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::three(1, 1, 0)));

        // PI → DocContent [1.1.1]
        let pi = doc_content
            .find_by_terminal(&Terminal::ProcessingInstr)
            .unwrap();
        assert_eq!(pi.event_code, Some(EventCode::three(1, 1, 1)));
    }

    /// Spec 8.5.1: Schema mit 3 globalen Elementen
    #[test]
    fn schema_informed_document_three_elements() {
        use crate::schema::SchemaInfo;

        let mut interner = test_interner();
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "book"))
            .global_element(QName::new("", "author"))
            .global_element(QName::new("", "chapter"))
            .build();

        let gs = GrammarSystem::schema_informed_document(&schema, &mut interner).unwrap();
        let doc_content = gs.get(NonTerminalId::DocContent).unwrap();

        // 3 SE(qname) + SE(*) + DT + CM + PI = 7
        assert_eq!(doc_content.productions().len(), 7);

        // Sortierte QNames: author < book < chapter (local-name first)
        // SE(author) [0]
        let se_author = doc_content.find_by_terminal(&se("", "author", &mut interner)).unwrap();
        assert_eq!(se_author.event_code, Some(EventCode::one(0)));
        assert_eq!(se_author.right_hand_side, Some(NonTerminalId::DocEnd));

        // SE(book) [1]
        let se_book = doc_content.find_by_terminal(&se("", "book", &mut interner)).unwrap();
        assert_eq!(se_book.event_code, Some(EventCode::one(1)));

        // SE(chapter) [2]
        let se_chapter = doc_content.find_by_terminal(&se("", "chapter", &mut interner)).unwrap();
        assert_eq!(se_chapter.event_code, Some(EventCode::one(2)));

        // SE(*) [3] (n=3)
        let se_wildcard = doc_content.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se_wildcard.event_code, Some(EventCode::one(3)));

        // DT [4.0] (n+1=4)
        let dt = doc_content.find_by_terminal(&Terminal::DocType).unwrap();
        assert_eq!(dt.event_code, Some(EventCode::two(4, 0)));

        // CM [4.1.0]
        let cm = doc_content.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::three(4, 1, 0)));

        // PI [4.1.1]
        let pi = doc_content
            .find_by_terminal(&Terminal::ProcessingInstr)
            .unwrap();
        assert_eq!(pi.event_code, Some(EventCode::three(4, 1, 1)));
    }

    /// Spec 8.5.1: DocEnd bleibt gleich (ED, CM, PI)
    #[test]
    fn schema_informed_document_doc_end() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .build();

        let gs = GrammarSystem::schema_informed_document(&schema, &mut StringInterner::new()).unwrap();
        let doc_end = gs.get(NonTerminalId::DocEnd).unwrap();

        // DocEnd ist identisch mit Built-in: ED [0], CM [1.0], PI [1.1]
        assert_eq!(doc_end.productions().len(), 3);

        let ed = doc_end.find_by_terminal(&Terminal::EndDocument).unwrap();
        assert_eq!(ed.event_code, Some(EventCode::one(0)));

        let cm = doc_end.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::two(1, 0)));

        let pi = doc_end
            .find_by_terminal(&Terminal::ProcessingInstr)
            .unwrap();
        assert_eq!(pi.event_code, Some(EventCode::two(1, 1)));
    }

    /// Spec 8.5.1: Pruning funktioniert korrekt
    #[test]
    fn schema_informed_document_prune() {
        use crate::schema::SchemaInfo;

        let mut interner = test_interner();
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .build();

        let mut gs = GrammarSystem::schema_informed_document(&schema, &mut interner).unwrap();
        let options = ExiOptions::default(); // CM, PI, DT werden entfernt

        gs.prune(&options).unwrap();

        let doc_content = gs.get(NonTerminalId::DocContent).unwrap();

        // Nur SE(root), SE(*) übrig
        assert_eq!(doc_content.productions().len(), 2);

        // SE(root) [0]
        let se_root = doc_content.find_by_terminal(&se("", "root", &mut interner)).unwrap();
        assert_eq!(se_root.event_code, Some(EventCode::one(0)));

        // SE(*) [1]
        let se_wildcard = doc_content.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se_wildcard.event_code, Some(EventCode::one(1)));
    }

    /// Spec 8.5.1 + 8.3: DocContent Pruning wenn DT weg aber CM/PI bleiben
    ///
    /// Vor: SE(root)[0], SE(*)[1], DT[2.0], CM[2.1.0], PI[2.1.1]
    /// Nach (dtd=false, comments=true, pis=true): SE(root)[0], SE(*)[1], CM[2.0.0], PI[2.0.1]
    ///
    /// Hierarchische Struktur bleibt erhalten (3-Teil → 3-Teil), aber Werte werden renummeriert.
    #[test]
    fn schema_informed_document_prune_dt_only() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .build();

        let mut gs = GrammarSystem::schema_informed_document(&schema, &mut StringInterner::new()).unwrap();
        let options = ExiOptions {
            preserve: Preserve {
                dtd: false,     // DT wird entfernt
                comments: true, // CM bleibt
                pis: true,      // PI bleibt
                ..Default::default()
            },

            ..Default::default()
        };

        gs.prune(&options).unwrap();

        let doc_content = gs.get(NonTerminalId::DocContent).unwrap();

        // SE(root), SE(*), CM, PI = 4
        assert_eq!(doc_content.productions().len(), 4);

        // CM: [2.1.0] → [2.0.0] (Struktur erhalten, Werte renummeriert)
        let cm = doc_content.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(
            cm.event_code,
            Some(EventCode::three(2, 0, 0)),
            "CM sollte [2.0.0] sein"
        );

        // PI: [2.1.1] → [2.0.1] (Struktur erhalten, Werte renummeriert)
        let pi = doc_content
            .find_by_terminal(&Terminal::ProcessingInstr)
            .unwrap();
        assert_eq!(
            pi.event_code,
            Some(EventCode::three(2, 0, 1)),
            "PI sollte [2.0.1] sein"
        );
    }

    /// Spec 8.5.1 + 8.3: DocEnd Pruning wenn CM weg aber PI bleibt
    ///
    /// Vor: ED[0], CM[1.0], PI[1.1]
    /// Nach (comments=false, pis=true): ED[0], PI[1.0]
    #[test]
    fn schema_informed_document_prune_doc_end_cm_only() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .build();

        let mut gs = GrammarSystem::schema_informed_document(&schema, &mut StringInterner::new()).unwrap();
        let options = ExiOptions {
            preserve: Preserve {
                comments: false, // CM wird entfernt
                pis: true,       // PI bleibt
                ..Default::default()
            },

            ..Default::default()
        };

        gs.prune(&options).unwrap();

        let doc_end = gs.get(NonTerminalId::DocEnd).unwrap();

        // ED, PI = 2
        assert_eq!(doc_end.productions().len(), 2);

        // ED bleibt [0]
        let ed = doc_end.find_by_terminal(&Terminal::EndDocument).unwrap();
        assert_eq!(ed.event_code, Some(EventCode::one(0)));

        // PI muss von [1.1] auf [1.0] renummeriert werden
        let pi = doc_end
            .find_by_terminal(&Terminal::ProcessingInstr)
            .unwrap();
        assert_eq!(pi.event_code, Some(EventCode::two(1, 0)), "PI sollte [1.0] sein");
    }

    // ========================================================================
    // Tests: Schema-informed Fragment Grammar (Spec 8.5.2)
    // ========================================================================

    /// Spec 8.5.2: Leeres Schema → nur SE(*), ED, CM, PI in FragmentContent
    #[test]
    fn schema_informed_fragment_empty_schema() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder().build();
        let gs = GrammarSystem::schema_informed_fragment(&schema, &mut StringInterner::new()).unwrap();

        assert_eq!(gs.grammar_type(), GrammarType::Fragment);
        assert_eq!(gs.start(), NonTerminalId::Fragment);

        let fc = gs.get(NonTerminalId::FragmentContent).unwrap();

        // Nur SE(*), ED, CM, PI (keine SE(qname))
        assert_eq!(fc.productions().len(), 4);

        // SE(*) → FragmentContent [0] (n=0)
        let se = fc.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se.event_code, Some(EventCode::one(0)));
        assert_eq!(se.right_hand_side, Some(NonTerminalId::FragmentContent));

        // ED [1] (n+1=1)
        let ed = fc.find_by_terminal(&Terminal::EndDocument).unwrap();
        assert_eq!(ed.event_code, Some(EventCode::one(1)));

        // CM → FragmentContent [2.0] (n+2=2)
        let cm = fc.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::two(2, 0)));

        // PI → FragmentContent [2.1]
        let pi = fc.find_by_terminal(&Terminal::ProcessingInstr).unwrap();
        assert_eq!(pi.event_code, Some(EventCode::two(2, 1)));
    }

    /// Spec 8.5.2: Schema mit 3 Elementen (verwendet all_elements, nicht global_elements)
    #[test]
    fn schema_informed_fragment_three_elements() {
        use crate::schema::SchemaInfo;

        let mut interner = test_interner();
        let schema = SchemaInfo::builder()
            .all_element(QName::new("", "book"))
            .all_element(QName::new("", "author"))
            .all_element(QName::new("", "chapter"))
            .build();

        let gs = GrammarSystem::schema_informed_fragment(&schema, &mut interner).unwrap();
        let fc = gs.get(NonTerminalId::FragmentContent).unwrap();

        // 3 SE(qname) + SE(*) + ED + CM + PI = 7
        assert_eq!(fc.productions().len(), 7);

        // Sortierte QNames: author < book < chapter
        // SE(author) [0]
        let se_author = fc.find_by_terminal(&se("", "author", &mut interner)).unwrap();
        assert_eq!(se_author.event_code, Some(EventCode::one(0)));
        // FragmentContent verweist auf sich selbst (Loop)!
        assert_eq!(
            se_author.right_hand_side,
            Some(NonTerminalId::FragmentContent)
        );

        // SE(book) [1]
        let se_book = fc.find_by_terminal(&se("", "book", &mut interner)).unwrap();
        assert_eq!(se_book.event_code, Some(EventCode::one(1)));

        // SE(chapter) [2]
        let se_chapter = fc.find_by_terminal(&se("", "chapter", &mut interner)).unwrap();
        assert_eq!(se_chapter.event_code, Some(EventCode::one(2)));

        // SE(*) [3] (n=3)
        let se_wildcard = fc.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se_wildcard.event_code, Some(EventCode::one(3)));

        // ED [4] (n+1=4)
        let ed = fc.find_by_terminal(&Terminal::EndDocument).unwrap();
        assert_eq!(ed.event_code, Some(EventCode::one(4)));

        // CM [5.0] (n+2=5)
        let cm = fc.find_by_terminal(&Terminal::Comment).unwrap();
        assert_eq!(cm.event_code, Some(EventCode::two(5, 0)));

        // PI [5.1]
        let pi = fc.find_by_terminal(&Terminal::ProcessingInstr).unwrap();
        assert_eq!(pi.event_code, Some(EventCode::two(5, 1)));
    }

    /// Spec 8.5.2: Pruning funktioniert korrekt
    #[test]
    fn schema_informed_fragment_prune() {
        use crate::schema::SchemaInfo;

        let mut interner = test_interner();
        let schema = SchemaInfo::builder()
            .all_element(QName::new("", "item"))
            .build();

        let mut gs = GrammarSystem::schema_informed_fragment(&schema, &mut interner).unwrap();
        let options = ExiOptions::default(); // CM, PI werden entfernt

        gs.prune(&options).unwrap();

        let fc = gs.get(NonTerminalId::FragmentContent).unwrap();

        // Nur SE(item), SE(*), ED übrig
        assert_eq!(fc.productions().len(), 3);

        // Event Codes werden neu berechnet: [0], [1], [2]
        let se_item = fc.find_by_terminal(&se("", "item", &mut interner)).unwrap();
        assert_eq!(se_item.event_code, Some(EventCode::one(0)));

        let se_wildcard = fc.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se_wildcard.event_code, Some(EventCode::one(1)));

        let ed = fc.find_by_terminal(&Terminal::EndDocument).unwrap();
        assert_eq!(ed.event_code, Some(EventCode::one(2)));
    }

    // ========================================================================
    // Tests: Schema-informed Element Fragment Grammar (Spec 8.5.3)
    // ========================================================================

    /// Spec 8.5.3: Leeres Schema → nur AT(*), SE(*), EE, CH
    #[test]
    fn schema_informed_element_fragment_empty_schema() {
        use crate::schema::SchemaInfo;

        let schema = SchemaInfo::builder().build();
        let gs = GrammarSystem::schema_informed_element_fragment(&schema, &mut StringInterner::new()).unwrap();

        // GrammarType ist ElementFragment (nicht Element!)
        assert_eq!(gs.grammar_type(), GrammarType::ElementFragment);
        assert_eq!(gs.start(), NonTerminalId::ElementFragment0);

        // Enthält alle 5 NonTerminals
        assert!(gs.get(NonTerminalId::ElementFragment0).is_some());
        assert!(gs.get(NonTerminalId::ElementFragment1).is_some());
        assert!(gs.get(NonTerminalId::ElementFragment2).is_some());
        assert!(gs.get(NonTerminalId::ElementFragmentTypeEmpty0).is_some());
        assert!(gs.get(NonTerminalId::ElementFragmentTypeEmpty1).is_some());

        // ElementFragment_0 mit n=0, m=0:
        // AT(*) [0], SE(*) [1], EE [2], CH [3]
        let ef0 = gs.get(NonTerminalId::ElementFragment0).unwrap();
        assert_eq!(ef0.productions().len(), 4);

        let at_wildcard = ef0
            .find_by_terminal(&Terminal::Attribute(AttributeKind::Wildcard))
            .unwrap();
        assert_eq!(at_wildcard.event_code, Some(EventCode::one(0)));

        let se_wildcard = ef0.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se_wildcard.event_code, Some(EventCode::one(1)));

        let ee = ef0.find_by_terminal(&Terminal::EndElement).unwrap();
        assert_eq!(ee.event_code, Some(EventCode::one(2)));

        let ch = ef0.find_by_terminal(&Terminal::Characters).unwrap();
        assert_eq!(ch.event_code, Some(EventCode::one(3)));
    }

    /// Spec 8.5.3: Schema mit Attributen und Elementen
    #[test]
    fn schema_informed_element_fragment_with_attrs_and_elements() {
        use crate::schema::SchemaInfo;

        let mut interner = test_interner();
        // n=2 Attribute, m=2 Elemente
        let schema = SchemaInfo::builder()
            .attribute(QName::new("", "id"))
            .attribute(QName::new("", "class"))
            .all_element(QName::new("", "item"))
            .all_element(QName::new("", "data"))
            .build();

        let gs = GrammarSystem::schema_informed_element_fragment(&schema, &mut interner).unwrap();

        // ElementFragment_0: n=2 attrs, m=2 elements
        // AT(class) [0], AT(id) [1], AT(*) [2],
        // SE(data) [3], SE(item) [4], SE(*) [5],
        // EE [6], CH [7]
        let ef0 = gs.get(NonTerminalId::ElementFragment0).unwrap();
        assert_eq!(ef0.productions().len(), 8);

        // Attribute sortiert: class < id
        let at_class = ef0.find_by_terminal(&at("", "class", &mut interner)).unwrap();
        assert_eq!(at_class.event_code, Some(EventCode::one(0)));

        let at_id = ef0.find_by_terminal(&at("", "id", &mut interner)).unwrap();
        assert_eq!(at_id.event_code, Some(EventCode::one(1)));

        // AT(*) [2]
        let at_wildcard = ef0
            .find_by_terminal(&Terminal::Attribute(AttributeKind::Wildcard))
            .unwrap();
        assert_eq!(at_wildcard.event_code, Some(EventCode::one(2)));

        // Elemente sortiert: data < item
        // SE(data) [3] (n+1=3)
        let se_data = ef0.find_by_terminal(&se("", "data", &mut interner)).unwrap();
        assert_eq!(se_data.event_code, Some(EventCode::one(3)));

        // SE(item) [4]
        let se_item = ef0.find_by_terminal(&se("", "item", &mut interner)).unwrap();
        assert_eq!(se_item.event_code, Some(EventCode::one(4)));

        // SE(*) [5] (n+m+1=5)
        let se_wildcard = ef0.find_by_terminal(&SE_WILDCARD).unwrap();
        assert_eq!(se_wildcard.event_code, Some(EventCode::one(5)));

        // EE [6] (n+m+2=6)
        let ee = ef0.find_by_terminal(&Terminal::EndElement).unwrap();
        assert_eq!(ee.event_code, Some(EventCode::one(6)));

        // CH [7] (n+m+3=7)
        let ch = ef0.find_by_terminal(&Terminal::Characters).unwrap();
        assert_eq!(ch.event_code, Some(EventCode::one(7)));
    }

    /// Spec 8.5.3: ElementFragment_1 und _2 haben keine Attribute
    #[test]
    fn schema_informed_element_fragment_1_and_2() {
        use crate::schema::SchemaInfo;

        let mut interner = test_interner();
        let schema = SchemaInfo::builder()
            .attribute(QName::new("", "id"))
            .all_element(QName::new("", "item"))
            .build();

        let gs = GrammarSystem::schema_informed_element_fragment(&schema, &mut interner).unwrap();

        // ElementFragment_1: nur SE(item), SE(*), EE, CH
        let ef1 = gs.get(NonTerminalId::ElementFragment1).unwrap();
        assert_eq!(ef1.productions().len(), 4);

        // SE(item) [0]
        let se_item = ef1.find_by_terminal(&se("", "item", &mut interner)).unwrap();
        assert_eq!(se_item.event_code, Some(EventCode::one(0)));

        // Keine Attribute
        assert!(
            ef1.find_by_terminal(&Terminal::Attribute(AttributeKind::Wildcard))
                .is_none()
        );

        // ElementFragment_2 ist identisch mit _1
        let ef2 = gs.get(NonTerminalId::ElementFragment2).unwrap();
        assert_eq!(ef2.productions().len(), 4);
    }

    /// Spec 8.5.3: ElementFragmentTypeEmpty_0 hat nur Attribute und EE
    #[test]
    fn schema_informed_element_fragment_type_empty() {
        use crate::schema::SchemaInfo;

        let mut interner = test_interner();
        let schema = SchemaInfo::builder()
            .attribute(QName::new("", "id"))
            .attribute(QName::new("", "class"))
            .all_element(QName::new("", "item")) // Wird ignoriert
            .build();

        let gs = GrammarSystem::schema_informed_element_fragment(&schema, &mut interner).unwrap();

        // ElementFragmentTypeEmpty_0: AT(class), AT(id), AT(*), EE
        let te0 = gs.get(NonTerminalId::ElementFragmentTypeEmpty0).unwrap();
        assert_eq!(te0.productions().len(), 4);

        // AT(class) [0]
        let at_class = te0.find_by_terminal(&at("", "class", &mut interner)).unwrap();
        assert_eq!(at_class.event_code, Some(EventCode::one(0)));

        // AT(id) [1]
        let at_id = te0.find_by_terminal(&at("", "id", &mut interner)).unwrap();
        assert_eq!(at_id.event_code, Some(EventCode::one(1)));

        // AT(*) [2]
        let at_wildcard = te0
            .find_by_terminal(&Terminal::Attribute(AttributeKind::Wildcard))
            .unwrap();
        assert_eq!(at_wildcard.event_code, Some(EventCode::one(2)));

        // EE [3] (n+1=3)
        let ee = te0.find_by_terminal(&Terminal::EndElement).unwrap();
        assert_eq!(ee.event_code, Some(EventCode::one(3)));

        // Keine SE oder CH
        assert!(te0.find_by_terminal(&SE_WILDCARD).is_none());
        assert!(te0.find_by_terminal(&Terminal::Characters).is_none());

        // ElementFragmentTypeEmpty_1: nur EE [0]
        let te1 = gs.get(NonTerminalId::ElementFragmentTypeEmpty1).unwrap();
        assert_eq!(te1.productions().len(), 1);
        let ee = te1.find_by_terminal(&Terminal::EndElement).unwrap();
        assert_eq!(ee.event_code, Some(EventCode::one(0)));
    }

    // ========================================================================
    // Tests: GrammarSystem Pruning (Spec 8.3)
    // ========================================================================

    /// GrammarSystem::prune() entfernt CM wenn preserve.comments=false
    #[test]
    fn grammar_system_prune_removes_cm() {
        let mut gs = GrammarSystem::built_in_document();
        let options = ExiOptions::default(); // comments=false

        gs.prune(&options).unwrap();

        let doc_content = gs.get(NonTerminalId::DocContent).unwrap();
        assert!(doc_content.find_by_terminal(&Terminal::Comment).is_none());
        assert!(
            doc_content
                .find_by_terminal(&Terminal::ProcessingInstr)
                .is_none()
        );
        // SE und DT sollten noch da sein
        assert!(doc_content.find_by_terminal(&SE_WILDCARD).is_some());
    }

    /// GrammarSystem::prune() entfernt PI wenn preserve.pis=false
    #[test]
    fn grammar_system_prune_removes_pi() {
        let mut gs = GrammarSystem::built_in_document();
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: false,
                ..Default::default()
            },

            ..Default::default()
        };

        gs.prune(&options).unwrap();

        let doc_end = gs.get(NonTerminalId::DocEnd).unwrap();
        assert!(doc_end.find_by_terminal(&Terminal::Comment).is_some());
        assert!(
            doc_end
                .find_by_terminal(&Terminal::ProcessingInstr)
                .is_none()
        );
    }

    /// GrammarSystem::prune() entfernt DT wenn preserve.dtd=false
    #[test]
    fn grammar_system_prune_removes_dt() {
        let mut gs = GrammarSystem::built_in_document();
        let options = ExiOptions::default(); // dtd=false

        gs.prune(&options).unwrap();

        let doc_content = gs.get(NonTerminalId::DocContent).unwrap();
        assert!(doc_content.find_by_terminal(&Terminal::DocType).is_none());
    }

    /// Event Codes sind nach Pruning contiguous
    #[test]
    fn grammar_system_prune_contiguous_codes() {
        let mut gs = GrammarSystem::built_in_document();
        let options = ExiOptions::default(); // Alles false

        gs.prune(&options).unwrap();

        // DocContent: Nach Pruning nur SE → DocEnd
        let doc_content = gs.get(NonTerminalId::DocContent).unwrap();
        assert_eq!(doc_content.productions().len(), 1);
        assert_eq!(doc_content.productions()[0].event_code, Some(EventCode::one(0)));

        // DocEnd: Nach Pruning nur ED
        let doc_end = gs.get(NonTerminalId::DocEnd).unwrap();
        assert_eq!(doc_end.productions().len(), 1);
        assert_eq!(doc_end.productions()[0].event_code, Some(EventCode::one(0)));
    }

    /// GrammarSystem::prune() für Fragment Grammar
    #[test]
    fn grammar_system_prune_fragment() {
        let mut gs = GrammarSystem::built_in_fragment();
        let options = ExiOptions::default();

        gs.prune(&options).unwrap();

        let fc = gs.get(NonTerminalId::FragmentContent).unwrap();
        // Nur SE und ED übrig
        assert_eq!(fc.productions().len(), 2);
        assert!(fc.find_by_terminal(&SE_WILDCARD).is_some());
        assert!(fc.find_by_terminal(&Terminal::EndDocument).is_some());
        assert!(fc.find_by_terminal(&Terminal::Comment).is_none());
        assert!(fc.find_by_terminal(&Terminal::ProcessingInstr).is_none());
    }

    /// GrammarSystem::prune() für Element Grammar mit selfContained=false
    #[test]
    fn grammar_system_prune_element() {
        let options = ExiOptions::default();
        let mut gs = GrammarSystem::built_in_element(&options);

        gs.prune(&options).unwrap();

        let stc = gs.get(NonTerminalId::StartTagContent).unwrap();
        // EE, AT, SE, CH sollten übrig sein (NS und ER sind auch weg wegen prefixes/dtd=false)
        assert!(stc.find_by_terminal(&Terminal::EndElement).is_some());
        assert!(stc.find_by_terminal(&AT_WILDCARD).is_some());
        assert!(stc.find_by_terminal(&SE_WILDCARD).is_some());
        assert!(stc.find_by_terminal(&Terminal::Characters).is_some());
        // Diese sind weg
        assert!(stc.find_by_terminal(&Terminal::NamespaceDecl).is_none()); // prefixes=false
        assert!(stc.find_by_terminal(&Terminal::Comment).is_none());
        assert!(stc.find_by_terminal(&Terminal::ProcessingInstr).is_none());
        assert!(stc.find_by_terminal(&Terminal::EntityRef).is_none()); // dtd=false

        let ec = gs.get(NonTerminalId::ElementContent).unwrap();
        // EE, SE, CH übrig
        assert!(ec.find_by_terminal(&Terminal::EndElement).is_some());
        assert!(ec.find_by_terminal(&SE_WILDCARD).is_some());
        assert!(ec.find_by_terminal(&Terminal::Characters).is_some());
        assert!(ec.find_by_terminal(&Terminal::Comment).is_none());
    }

    /// GrammarSystem::prune() behält alle wenn alles erlaubt
    #[test]
    fn grammar_system_prune_preserves_all_when_allowed() {
        let mut gs = GrammarSystem::built_in_document();
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: true,
                dtd: true,
                prefixes: true,
                ..Default::default()
            },

            ..Default::default()
        };

        // Vor Pruning: DocContent hat 4 Productions
        let before = gs
            .get(NonTerminalId::DocContent)
            .unwrap()
            .productions()
            .len();

        gs.prune(&options).unwrap();

        // Nach Pruning: Immer noch 4 Productions
        let after = gs
            .get(NonTerminalId::DocContent)
            .unwrap()
            .productions()
            .len();
        assert_eq!(before, after);
    }

    /// GrammarSystem::get_mut() ermöglicht Modifikation
    #[test]
    fn grammar_system_get_mut() {
        let mut gs = GrammarSystem::built_in_document();
        let nt = gs.get_mut(NonTerminalId::Document).unwrap();
        // Prüfe dass wir Zugriff haben
        assert_eq!(nt.id(), NonTerminalId::Document);
    }

    /// NonTerminal::event_code_context() für 1-Teil Event Codes
    #[test]
    fn non_terminal_event_code_context_one_part() {
        // Document NonTerminal hat nur 1-Teil Event Codes
        let nt = NonTerminal::new(
            NonTerminalId::Document,
            vec![Production::new(
                Terminal::StartDocument,
                EventCode::one(0),
                Some(NonTerminalId::DocContent),
            )],
        );
        let ctx = nt.event_code_context();
        // Nur Part 1 sollte vorhanden sein
        assert_eq!(ctx.num_values_part1(), 1);
        assert_eq!(ctx.max_num_values_part2(), 0);
        assert_eq!(ctx.max_num_values_part3(), 0);
    }

    /// GrammarSystem::is_terminal_allowed() für SelfContained
    #[test]
    fn grammar_system_prune_self_contained_terminal() {
        let options = ExiOptions {
            self_contained: true,
            preserve: Preserve {
                prefixes: true, // NS bleibt
                ..Default::default()
            },

            ..Default::default()
        };
        let mut gs = GrammarSystem::built_in_element(&options);

        // SC sollte vorhanden sein
        let stc = gs.get(NonTerminalId::StartTagContent).unwrap();
        assert!(stc.find_by_terminal(&Terminal::SelfContained).is_some());

        // Prune mit self_contained=false sollte SC entfernen
        let prune_options = ExiOptions {
            self_contained: false,
            preserve: Preserve {
                prefixes: true,
                ..Default::default()
            },

            ..Default::default()
        };
        gs.prune(&prune_options).unwrap();

        let stc = gs.get(NonTerminalId::StartTagContent).unwrap();
        assert!(stc.find_by_terminal(&Terminal::SelfContained).is_none());
    }

    /// GrammarSystem::prune() gibt Error bei invaliden Options zurück.
    ///
    /// Spec 5.4: strict=true + preserve.comments=true ist invalid.
    #[test]
    fn grammar_system_prune_invalid_options() {
        let mut gs = GrammarSystem::built_in_document();
        let invalid_options = ExiOptions {
            strict: true,

            preserve: Preserve {
                comments: true, // Konflikt mit strict!
                ..Default::default()
            },
            ..Default::default()
        };
        let result = gs.prune(&invalid_options);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), crate::Error::InvalidOptionCombination);
    }

    /// Alte Grammar struct: grammar_type() Accessor
    #[test]
    fn grammar_type_accessor() {
        let grammar = Grammar::new(GrammarType::Document, vec![]);
        assert_eq!(grammar.grammar_type(), GrammarType::Document);

        let grammar = Grammar::new(GrammarType::Fragment, vec![]);
        assert_eq!(grammar.grammar_type(), GrammarType::Fragment);

        let grammar = Grammar::new(GrammarType::Element, vec![]);
        assert_eq!(grammar.grammar_type(), GrammarType::Element);
    }

    // ========================================================================
    // Tests: Production mit right_hand_side (Spec 8.1)
    // ========================================================================

    /// Production::new erstellt Production mit allen Feldern
    #[test]
    fn production_new_with_right_hand_side() {
        let prod = Production::new(
            Terminal::StartElement(StartElementKind::Wildcard),
            EventCode::one(0),
            Some(NonTerminalId::ElementContent),
        );
        assert_eq!(prod.terminal, SE_WILDCARD);
        assert_eq!(prod.event_code, Some(EventCode::one(0)));
        assert_eq!(prod.right_hand_side, Some(NonTerminalId::ElementContent));
    }

    /// Production::terminal erstellt Production ohne Übergang (None)
    #[test]
    fn production_terminal_has_no_right_hand_side() {
        let prod = Production::terminal(Terminal::EndElement, EventCode::one(0));
        assert_eq!(prod.terminal, Terminal::EndElement);
        assert_eq!(prod.right_hand_side, None);
    }

    /// EE und ED sind typische terminale Productions
    #[test]
    fn production_ee_ed_are_terminal() {
        let ee = Production::terminal(Terminal::EndElement, EventCode::one(0));
        let ed = Production::terminal(Terminal::EndDocument, EventCode::one(0));
        assert!(ee.right_hand_side.is_none());
        assert!(ed.right_hand_side.is_none());
    }

    /// SE und CH haben typischerweise ein right_hand_side
    #[test]
    fn production_se_ch_have_right_hand_side() {
        let se = Production::new(
            Terminal::StartElement(StartElementKind::Wildcard),
            EventCode::two(1, 0),
            Some(NonTerminalId::ElementContent),
        );
        let ch = Production::new(
            Terminal::Characters,
            EventCode::two(1, 1),
            Some(NonTerminalId::ElementContent),
        );
        assert!(se.right_hand_side.is_some());
        assert!(ch.right_hand_side.is_some());
    }

    // ========================================================================
    // Hilfsfunktionen für Tests
    // ========================================================================

    /// Erstellt eine Test-Grammar mit allen Terminal-Typen (Element Grammar).
    fn element_grammar_full() -> Grammar {
        Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                ),
                Production::terminal(Terminal::Characters, EventCode::two(1, 1)),
                Production::terminal(Terminal::EntityRef, EventCode::two(1, 2)),
                Production::terminal(Terminal::Comment, EventCode::three(1, 3, 0)),
                Production::terminal(Terminal::ProcessingInstr, EventCode::three(1, 3, 1)),
            ],
        )
    }

    /// Gibt die Terminals einer Grammar als Vec zurück.
    fn terminals(grammar: &Grammar) -> Vec<Terminal> {
        grammar
            .productions()
            .iter()
            .map(|p| p.terminal.clone())
            .collect()
    }

    // ========================================================================
    // Tests: Einzelne Fidelity Options (Spec 6.3, Table 6-3)
    // ========================================================================

    /// Spec 6.3: CM wird entfernt wenn preserve.comments=false
    #[test]
    fn prune_cm_when_comments_false() {
        let mut grammar = element_grammar_full();
        let options = ExiOptions {
            preserve: Preserve {
                comments: false,
                pis: true,
                dtd: true,
                prefixes: true,
                ..Default::default()
            },
            self_contained: true,

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        assert!(!terminals(&grammar).contains(&Terminal::Comment));
        assert!(terminals(&grammar).contains(&Terminal::ProcessingInstr));
    }

    /// Spec 6.3: PI wird entfernt wenn preserve.pis=false
    #[test]
    fn prune_pi_when_pis_false() {
        let mut grammar = element_grammar_full();
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: false,
                dtd: true,
                prefixes: true,
                ..Default::default()
            },
            self_contained: true,

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        assert!(!terminals(&grammar).contains(&Terminal::ProcessingInstr));
        assert!(terminals(&grammar).contains(&Terminal::Comment));
    }

    /// Spec 6.3: DT und ER werden entfernt wenn preserve.dtd=false
    #[test]
    fn prune_dt_er_when_dtd_false() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(Terminal::DocType, EventCode::one(1)),
                Production::terminal(Terminal::EntityRef, EventCode::one(2)),
                Production::terminal(Terminal::Comment, EventCode::one(3)),
            ],
        );
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: true,
                dtd: false,
                prefixes: true,
                ..Default::default()
            },
            self_contained: true,

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        assert!(!terminals(&grammar).contains(&Terminal::DocType));
        assert!(!terminals(&grammar).contains(&Terminal::EntityRef));
        assert!(terminals(&grammar).contains(&Terminal::Comment));
    }

    /// Spec 6.3: NS wird entfernt wenn preserve.prefixes=false
    #[test]
    fn prune_ns_when_prefixes_false() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(Terminal::NamespaceDecl, EventCode::one(1)),
                Production::terminal(Terminal::Comment, EventCode::one(2)),
            ],
        );
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: true,
                dtd: true,
                prefixes: false,
                ..Default::default()
            },
            self_contained: true,

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        assert!(!terminals(&grammar).contains(&Terminal::NamespaceDecl));
        assert!(terminals(&grammar).contains(&Terminal::Comment));
    }

    /// Spec 6.3: SC wird entfernt wenn self_contained=false
    #[test]
    fn prune_sc_when_self_contained_false() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(Terminal::SelfContained, EventCode::one(1)),
                Production::terminal(Terminal::Comment, EventCode::one(2)),
            ],
        );
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                ..Default::default()
            },
            self_contained: false,

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        assert!(!terminals(&grammar).contains(&Terminal::SelfContained));
        assert!(terminals(&grammar).contains(&Terminal::Comment));
    }

    /// SC kommt nur in Element Grammar vor, nicht in Document Grammar
    #[test]
    fn sc_not_in_document_grammar() {
        // Document Grammar hat kein SC - wenn es fälschlich drin wäre,
        // würde es bei self_contained=false entfernt
        let mut grammar = Grammar::new(
            GrammarType::Document,
            vec![
                Production::terminal(Terminal::StartDocument, EventCode::one(0)),
                Production::terminal(Terminal::SelfContained, EventCode::one(1)), // sollte nicht vorkommen
            ],
        );
        grammar.prune(&ExiOptions::default()).unwrap();

        // SC wird entfernt weil self_contained=false (default)
        assert!(!terminals(&grammar).contains(&Terminal::SelfContained));
    }

    // ========================================================================
    // Tests: Kombinationen
    // ========================================================================

    /// CM und PI werden beide entfernt
    #[test]
    fn prune_cm_and_pi() {
        let mut grammar = element_grammar_full();
        let options = ExiOptions {
            preserve: Preserve {
                comments: false,
                pis: false,
                dtd: true,
                prefixes: true,
                ..Default::default()
            },
            self_contained: true,

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        assert!(!terminals(&grammar).contains(&Terminal::Comment));
        assert!(!terminals(&grammar).contains(&Terminal::ProcessingInstr));
        assert!(terminals(&grammar).contains(&Terminal::EntityRef));
    }

    /// DTD entfernt, aber Comments bleiben
    #[test]
    fn prune_dtd_keep_comments() {
        let mut grammar = element_grammar_full();
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: true,
                dtd: false,
                prefixes: true,
                ..Default::default()
            },
            self_contained: true,

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        assert!(!terminals(&grammar).contains(&Terminal::EntityRef));
        assert!(terminals(&grammar).contains(&Terminal::Comment));
        assert!(terminals(&grammar).contains(&Terminal::ProcessingInstr));
    }

    /// Alle Fidelity Options false → nur Basis-Terminals bleiben
    #[test]
    fn all_fidelity_false() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(1),
                ),
                Production::terminal(
                    Terminal::Attribute(AttributeKind::Wildcard),
                    EventCode::one(2),
                ),
                Production::terminal(Terminal::Characters, EventCode::one(3)),
                Production::terminal(Terminal::NamespaceDecl, EventCode::one(4)),
                Production::terminal(Terminal::Comment, EventCode::one(5)),
                Production::terminal(Terminal::ProcessingInstr, EventCode::one(6)),
                Production::terminal(Terminal::DocType, EventCode::one(7)),
                Production::terminal(Terminal::EntityRef, EventCode::one(8)),
                Production::terminal(Terminal::SelfContained, EventCode::one(9)),
            ],
        );
        let options = ExiOptions::default(); // Alles false

        grammar.prune(&options).unwrap();

        let remaining = terminals(&grammar);
        assert!(remaining.contains(&Terminal::EndElement));
        assert!(remaining.contains(&SE_WILDCARD));
        assert!(remaining.contains(&AT_WILDCARD));
        assert!(remaining.contains(&Terminal::Characters));
        // Diese sollten entfernt sein
        assert!(!remaining.contains(&Terminal::NamespaceDecl));
        assert!(!remaining.contains(&Terminal::Comment));
        assert!(!remaining.contains(&Terminal::ProcessingInstr));
        assert!(!remaining.contains(&Terminal::DocType));
        assert!(!remaining.contains(&Terminal::EntityRef));
        assert!(!remaining.contains(&Terminal::SelfContained));
    }

    // ========================================================================
    // Tests: Strict-Modus
    // ========================================================================

    /// Strict-Modus: Fidelity-Events und ungetypte Terminals entfernt
    ///
    /// Spec 5.4 Zeile 715-716: "those productions that have NS, CM, PI, ER,
    /// and SC terminal symbols are omitted from the EXI grammars, and
    /// schema-informed element and type grammars are restricted to only
    /// permit items declared in the schemas."
    ///
    /// SE(*)/AT(*) Wildcards bleiben bei strict erhalten (sie können aus
    /// xs:any-Deklarationen stammen). Ungetypte Terminals (AT(*)[untyped],
    /// CH[untyped]) werden bei strict entfernt.
    #[test]
    fn strict_mode_prunes_all() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(1),
                ),
                Production::terminal(Terminal::NamespaceDecl, EventCode::one(2)),
                Production::terminal(Terminal::Comment, EventCode::one(3)),
                Production::terminal(Terminal::ProcessingInstr, EventCode::one(4)),
                Production::terminal(Terminal::DocType, EventCode::one(5)),
                Production::terminal(Terminal::EntityRef, EventCode::one(6)),
                Production::terminal(Terminal::SelfContained, EventCode::one(7)),
            ],
        );
        // strict=true impliziert alle Preserve-Options sind false
        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        let remaining = terminals(&grammar);
        // EE + SE(*) bleiben: Wildcards werden nicht entfernt (können aus
        // xs:any stammen), nur Fidelity-Events (NS, CM, PI, DT, ER, SC)
        assert_eq!(remaining.len(), 2);
        assert!(remaining.contains(&Terminal::EndElement));
        assert!(remaining.contains(&SE_WILDCARD));
    }

    /// Strict-Modus bei Built-in Element: SE(*) bleibt erhalten
    ///
    /// Built-in Element Grammars brauchen SE(*) als fundamentalen Mechanismus
    /// für Child-Matching. Nur Fidelity-Events (CM, PI, DT, ER, NS, SC)
    /// werden entfernt.
    #[test]
    fn strict_mode_builtin_keeps_wildcards() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(1),
                ),
                Production::terminal(Terminal::Comment, EventCode::one(2)),
                Production::terminal(Terminal::ProcessingInstr, EventCode::one(3)),
            ],
        );
        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        let remaining = terminals(&grammar);
        // EE + SE(*) bleiben: Wildcards bei Built-in nicht entfernt
        assert_eq!(remaining.len(), 2);
        assert!(remaining.contains(&Terminal::EndElement));
        assert!(remaining.contains(&SE_WILDCARD));
    }

    /// strict=true + preserve.comments=true ist ungültig (validate() test)
    #[test]
    fn strict_with_preserve_rejected() {
        let options = ExiOptions {
            strict: true,

            preserve: Preserve {
                comments: true,
                ..Default::default()
            },
            ..Default::default()
        };

        assert!(options.validate().is_err());
    }

    /// Grammar::prune() gibt Error bei invaliden Options zurück.
    #[test]
    fn grammar_prune_invalid_options() {
        let mut grammar = Grammar::new(
            GrammarType::Document,
            vec![Production::terminal(
                Terminal::StartDocument,
                EventCode::one(0),
            )],
        );
        let invalid_options = ExiOptions {
            strict: true,

            preserve: Preserve {
                comments: true, // Konflikt mit strict!
                ..Default::default()
            },
            ..Default::default()
        };
        let result = grammar.prune(&invalid_options);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), crate::Error::InvalidOptionCombination);
    }

    // ========================================================================
    // Tests: Event-Code-Neuberechnung (Spec 8.3)
    // ========================================================================

    /// Spec Example 8-6 → 8-7: Event Codes nach Pruning
    #[test]
    fn example_8_6_to_8_7() {
        // Example 8-6: Volle Grammar
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                ),
                Production::terminal(Terminal::Characters, EventCode::two(1, 1)),
                Production::terminal(Terminal::EntityRef, EventCode::two(1, 2)),
                Production::terminal(Terminal::Comment, EventCode::three(1, 3, 0)),
                Production::terminal(Terminal::ProcessingInstr, EventCode::three(1, 3, 1)),
            ],
        );

        // Prune mit comments=false, pis=false, dtd=false
        let options = ExiOptions::default();
        grammar.prune(&options).unwrap();

        // Example 8-7: Nach Pruning
        let remaining = grammar.productions();
        assert_eq!(remaining.len(), 3);

        assert_eq!(remaining[0].terminal, Terminal::EndElement);
        assert_eq!(remaining[1].terminal, SE_WILDCARD);
        assert_eq!(remaining[2].terminal, Terminal::Characters);
    }

    /// Entfernte Mittel-Production: nachfolgende rücken nach
    #[test]
    fn middle_production_removed() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(1),
                ),
                Production::terminal(Terminal::Comment, EventCode::one(2)), // wird entfernt
                Production::terminal(Terminal::Characters, EventCode::one(3)),
            ],
        );
        let options = ExiOptions::default(); // comments=false

        grammar.prune(&options).unwrap();

        let remaining = grammar.productions();
        assert_eq!(remaining.len(), 3);

        // Event Codes sollten contiguous sein: 0, 1, 2
        assert_eq!(remaining[0].event_code, Some(EventCode::one(0)));
        assert_eq!(remaining[1].event_code, Some(EventCode::one(1)));
        assert_eq!(remaining[2].event_code, Some(EventCode::one(2)));
    }

    /// Ganze Sub-Partition weg
    #[test]
    fn entire_sub_partition_removed() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                ),
                Production::terminal(Terminal::Characters, EventCode::two(1, 1)),
                Production::terminal(Terminal::EntityRef, EventCode::two(1, 2)),
                // 1.3.x Sub-Partition wird komplett entfernt
                Production::terminal(Terminal::Comment, EventCode::three(1, 3, 0)),
                Production::terminal(Terminal::ProcessingInstr, EventCode::three(1, 3, 1)),
            ],
        );

        // Nur CM und PI entfernen, ER behalten
        let options = ExiOptions {
            preserve: Preserve {
                comments: false,
                pis: false,
                dtd: true, // ER bleibt
                ..Default::default()
            },

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        let remaining = grammar.productions();
        assert_eq!(remaining.len(), 4);

        // CM und PI sind weg
        assert!(!terminals(&grammar).contains(&Terminal::Comment));
        assert!(!terminals(&grammar).contains(&Terminal::ProcessingInstr));
        // ER ist noch da
        assert!(terminals(&grammar).contains(&Terminal::EntityRef));
    }

    /// Spec Example 8-6 → 8-7: Event Codes nach Pruning.
    ///
    /// Die Spec zeigt explizit:
    /// - Vor Pruning: EE [0], SE(*) [1.0], CH [1.1], ER [1.2], CM [1.3.0], PI [1.3.1]
    /// - Nach Pruning (ER, CM, PI entfernt): EE [0], SE(*) [1.0], CH [1.1]
    #[test]
    fn spec_example_8_6_to_8_7_event_codes() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                ),
                Production::terminal(Terminal::Characters, EventCode::two(1, 1)),
                Production::terminal(Terminal::EntityRef, EventCode::two(1, 2)),
                Production::terminal(Terminal::Comment, EventCode::three(1, 3, 0)),
                Production::terminal(Terminal::ProcessingInstr, EventCode::three(1, 3, 1)),
            ],
        );

        // Pruning: ER, CM, PI entfernen (dtd=false, comments=false, pis=false)
        let options = ExiOptions {
            preserve: Preserve {
                comments: false,
                pis: false,
                dtd: false, // entfernt ER
                ..Default::default()
            },

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        let prods = grammar.productions();
        assert_eq!(prods.len(), 3, "Sollte 3 Productions haben");

        // Spec Example 8-7: EE [0], SE(*) [1.0], CH [1.1]
        assert_eq!(prods[0].event_code, Some(EventCode::one(0)), "EE sollte [0] sein");
        assert_eq!(
            prods[1].event_code,
            Some(EventCode::two(1, 0)),
            "SE(*) sollte [1.0] sein"
        );
        assert_eq!(
            prods[2].event_code,
            Some(EventCode::two(1, 1)),
            "CH sollte [1.1] sein"
        );
    }

    /// Edge Case: 1-teil Codes nach 2-teil Codes in der Liste.
    ///
    /// Unsere Implementierung iteriert über die Productions in Reihenfolge
    /// und weist neue Codes zu. Wenn 1-teil Codes nach 2-teil stehen,
    /// könnte das zu falschen Codes führen.
    #[test]
    fn recalculate_event_codes_mixed_order() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                // 2-teil zuerst
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                ),
                Production::terminal(Terminal::Characters, EventCode::two(1, 1)),
                // 1-teil danach
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
            ],
        );

        grammar.prune(&ExiOptions::default()).unwrap();

        let prods = grammar.productions();
        assert_eq!(prods.len(), 3);

        // Die ursprüngliche Reihenfolge bleibt erhalten, aber die Event Codes
        // werden neu berechnet: 1-teil Codes bekommen 0,1,2,...
        // dann 2-teil ab dem nächsten Index.
        //
        // Mit 1x 1-teil Code (EE) und 2x 2-teil Codes (SE, CH):
        // - EE bekommt [0] (als einziger 1-teil)
        // - SE bekommt [1.0] (part1 = 1, da 1 1-teil Code)
        // - CH bekommt [1.1]
        //
        // ABER die Reihenfolge im Vec ist SE, CH, EE!
        // Also:
        // - prods[0] = SE → sollte [1.0] sein
        // - prods[1] = CH → sollte [1.1] sein
        // - prods[2] = EE → sollte [0] sein

        assert_eq!(
            prods[0].terminal,
            Terminal::StartElement(StartElementKind::Wildcard)
        );
        assert_eq!(
            prods[0].event_code,
            Some(EventCode::two(1, 0)),
            "SE(*) sollte [1.0] sein"
        );

        assert_eq!(prods[1].terminal, Terminal::Characters);
        assert_eq!(
            prods[1].event_code,
            Some(EventCode::two(1, 1)),
            "CH sollte [1.1] sein"
        );

        assert_eq!(prods[2].terminal, Terminal::EndElement);
        assert_eq!(prods[2].event_code, Some(EventCode::one(0)), "EE sollte [0] sein");
    }

    /// recalculate_event_codes ignoriert None-Event-Codes.
    ///
    /// Productions ohne Event Code bleiben None - sie werden nicht verarbeitet.
    /// Nur Productions mit existierenden Event Codes werden neu berechnet.
    #[test]
    fn recalculate_event_codes_ignores_none_codes() {
        // Gemischte Productions: Some und None
        let mut productions = vec![
            // None: wird ignoriert
            Production::without_event_code(Terminal::EndElement, None),
            // Some: wird neu berechnet
            Production::terminal(Terminal::Characters, EventCode::one(5)),
            // None: wird ignoriert
            Production::without_event_code(
                Terminal::StartElement(StartElementKind::Wildcard),
                None,
            ),
            // Some: wird neu berechnet
            Production::terminal(Terminal::Comment, EventCode::three(10, 20, 30)),
        ];

        recalculate_event_codes(&mut productions);

        // None bleibt None
        assert_eq!(productions[0].event_code, None, "EE ohne Code bleibt None");
        // CH: einzige Level-1 Production → [0]
        assert_eq!(
            productions[1].event_code,
            Some(EventCode::one(0)),
            "CH sollte [0] sein"
        );
        // None bleibt None
        assert_eq!(productions[2].event_code, None, "SE(*) ohne Code bleibt None");
        // CM: Level-3 Production → [1.0.0] (part1=1 weil 1 Level-1, part2=0, part3=0)
        assert_eq!(
            productions[3].event_code,
            Some(EventCode::three(1, 0, 0)),
            "CM sollte [1.0.0] sein"
        );
    }

    /// Ursprüngliche Reihenfolge bleibt erhalten
    #[test]
    fn order_preserved_after_pruning() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::Characters, EventCode::one(0)),
                Production::terminal(Terminal::Comment, EventCode::one(1)),
                Production::terminal(Terminal::EndElement, EventCode::one(2)),
                Production::terminal(Terminal::ProcessingInstr, EventCode::one(3)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(4),
                ),
            ],
        );
        let options = ExiOptions::default();

        grammar.prune(&options).unwrap();

        let remaining = terminals(&grammar);
        // Reihenfolge: CH, EE, SE (wie im Original, minus CM und PI)
        assert_eq!(
            remaining,
            vec![
                Terminal::Characters,
                Terminal::EndElement,
                Terminal::StartElement(StartElementKind::Wildcard),
            ]
        );
    }

    // ========================================================================
    // Tests: EventCodeContext-Ableitung
    // ========================================================================

    /// EventCodeContext hat korrekte Counts nach Pruning
    #[test]
    fn event_code_context_from_grammar() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(1),
                ),
                Production::terminal(Terminal::Characters, EventCode::one(2)),
                Production::terminal(Terminal::Comment, EventCode::one(3)),
            ],
        );
        let options = ExiOptions::default();
        grammar.prune(&options).unwrap();

        let context = grammar.event_code_context();

        // 3 Productions übrig → 3 distinct values für Part 1
        assert_eq!(context.num_values_part1(), 3);
        assert_eq!(context.max_num_values_part2(), 0);
        assert_eq!(context.max_num_values_part3(), 0);
    }

    /// Context counts entsprechen Productions
    #[test]
    fn context_counts_match_productions() {
        let grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                ),
                Production::terminal(Terminal::Characters, EventCode::two(1, 1)),
            ],
        );

        let context = grammar.event_code_context();

        // Part 1: max=1, also 2 distinct values (0, 1)
        assert_eq!(context.num_values_part1(), 2);
        // Part 2: max=1, also 2 distinct values (0, 1)
        assert_eq!(context.max_num_values_part2(), 2);
    }

    /// Context counts für 3-Teil Event Codes (alte Grammar)
    #[test]
    fn context_counts_three_parts() {
        let grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                ),
                Production::terminal(Terminal::Characters, EventCode::two(1, 1)),
                Production::terminal(Terminal::Comment, EventCode::three(1, 2, 0)),
                Production::terminal(Terminal::ProcessingInstr, EventCode::three(1, 2, 1)),
            ],
        );

        let context = grammar.event_code_context();

        // Part 1: max=1, also 2 distinct values (0, 1)
        assert_eq!(context.num_values_part1(), 2);
        // Part 2: max=2, also 3 distinct values (0, 1, 2)
        assert_eq!(context.max_num_values_part2(), 3);
        // Part 3: max=1, also 2 distinct values (0, 1)
        assert_eq!(context.max_num_values_part3(), 2);
    }

    /// Leere Grammar ergibt Context mit 0 values
    #[test]
    fn empty_grammar_context() {
        let grammar = Grammar::new(GrammarType::Element, vec![]);
        let context = grammar.event_code_context();

        assert_eq!(context.num_values_part1(), 0);
    }

    /// Spec 8.3: Wenn ein Code aus der Mitte entfernt wird, werden
    /// die nachfolgenden Codes renummeriert um kontiguös zu bleiben.
    ///
    /// Case: [0], [1.0], [1.1], [1.2] mit [1.1]=CM
    /// Nach Pruning (comments=false): [0], [1.0], [1.1] (nicht [1.2]!)
    #[test]
    fn prune_middle_code_renumbers_following() {
        let mut grammar = Grammar::new(
            GrammarType::Element,
            vec![
                Production::terminal(Terminal::EndElement, EventCode::one(0)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::two(1, 0),
                ),
                Production::terminal(Terminal::Comment, EventCode::two(1, 1)), // wird entfernt
                Production::terminal(Terminal::Characters, EventCode::two(1, 2)),
            ],
        );

        let options = ExiOptions {
            preserve: Preserve {
                comments: false,
                ..Default::default()
            },

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        let prods = grammar.productions();
        assert_eq!(prods.len(), 3, "Sollte 3 Productions haben");
        assert_eq!(prods[0].event_code, Some(EventCode::one(0)), "EE sollte [0] sein");
        assert_eq!(
            prods[1].event_code,
            Some(EventCode::two(1, 0)),
            "SE(*) sollte [1.0] sein"
        );
        assert_eq!(
            prods[2].event_code,
            Some(EventCode::two(1, 1)),
            "CH sollte [1.1] sein (nicht [1.2]!)"
        );
    }

    /// Spec 8.5.2 Schema-informed: Wenn CM entfernt wird aber PI bleibt,
    /// muss PI von [n.1] auf [n.0] renummeriert werden.
    ///
    /// Schema-informed FragmentContent hat: 0..n-1, n, n+1, (n+2).0, (n+2).1
    /// Nach Pruning von CM: PI sollte [n+2.0] werden (nicht [n+2.1] bleiben!)
    #[test]
    fn prune_schema_informed_cm_renumbers_pi() {
        let mut interner = test_interner();
        // Simuliert Schema-informed FragmentContent mit n=2:
        // SE(F0) [0], SE(F1) [1], SE(*) [2], ED [3], CM [4.0], PI [4.1]
        let mut grammar = Grammar::new(
            GrammarType::Fragment,
            vec![
                Production::terminal(se("", "a", &mut interner), EventCode::one(0)),
                Production::terminal(se("", "b", &mut interner), EventCode::one(1)),
                Production::terminal(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(2),
                ),
                Production::terminal(Terminal::EndDocument, EventCode::one(3)),
                Production::terminal(Terminal::Comment, EventCode::two(4, 0)), // wird entfernt
                Production::terminal(Terminal::ProcessingInstr, EventCode::two(4, 1)),
            ],
        );

        let options = ExiOptions {
            preserve: Preserve {
                comments: false,
                pis: true, // PI bleibt
                ..Default::default()
            },

            ..Default::default()
        };

        grammar.prune(&options).unwrap();

        let prods = grammar.productions();
        assert_eq!(prods.len(), 5, "Sollte 5 Productions haben");

        // PI sollte von [4.1] auf [4.0] renummeriert werden
        let pi_prod = prods
            .iter()
            .find(|p| p.terminal == Terminal::ProcessingInstr)
            .unwrap();
        assert_eq!(
            pi_prod.event_code,
            Some(EventCode::two(4, 0)),
            "PI sollte [4.0] sein (nicht [4.1] bleiben!)"
        );
    }

    // ========================================================================
    // Coverage-Tests für bisher ungedeckte Zeilen
    // ========================================================================

    /// Test für Production::terminal_without_event_code (Zeile 479-480)
    #[test]
    fn production_terminal_without_event_code() {
        let prod = Production::terminal_without_event_code(Terminal::EndDocument);
        assert_eq!(prod.terminal, Terminal::EndDocument);
        assert!(prod.event_code.is_none());
        assert!(prod.right_hand_side.is_none());
    }

    /// Test für Production::event_code_unwrap (Zeile 487-488)
    #[test]
    fn production_event_code_unwrap() {
        let prod = Production::terminal(Terminal::EndDocument, EventCode::one(0));
        let ec = prod.event_code_unwrap();
        assert_eq!(ec.part1(), 0);
    }

    /// Test für recalculate_event_codes mit leeren Productions (Zeile 83)
    #[test]
    fn recalculate_event_codes_empty_productions() {
        // Muss nicht paniken bei leeren Productions
        let nt = NonTerminal::new(NonTerminalId::Document, vec![]);
        // Productions leer lassen
        assert!(nt.productions().is_empty());

        // Event Code Context sollte trotzdem funktionieren
        let ctx = nt.event_code_context();
        assert_eq!(ctx.num_values_part1(), 0);
    }

    /// DoS-Schutz: learn_production stoppt bei MAX_LEARNED_PRODUCTIONS.
    #[test]
    fn learn_production_respects_limit() {
        let mut interner = test_interner();
        // Direkt ein NonTerminal mit MAX_LEARNED_PRODUCTIONS Productions erzeugen
        // (ohne prepend_learned_production, das wäre O(n²) und zu langsam).
        let productions: Vec<Production> = (0..MAX_LEARNED_PRODUCTIONS)
            .map(|i| {
                Production::new(
                    se("", &format!("e{i}"), &mut interner),
                    EventCode::one(i as u32),
                    Some(NonTerminalId::ElementContent),
                )
            })
            .collect();
        let mut nt = NonTerminal::new(NonTerminalId::ElementContent, productions);
        assert_eq!(nt.productions().len(), MAX_LEARNED_PRODUCTIONS);

        // Nächstes learn_production sollte false zurückgeben
        let learned = nt.learn_production(
            se("", "extra", &mut interner),
            Some(NonTerminalId::ElementContent),
        );
        assert!(!learned);
        assert_eq!(nt.productions().len(), MAX_LEARNED_PRODUCTIONS);
    }

    /// DoS-Schutz: learn_content_production stoppt bei MAX_LEARNED_PRODUCTIONS.
    #[test]
    fn learn_content_production_respects_limit() {
        let mut interner = test_interner();
        let productions: Vec<Production> = (0..MAX_LEARNED_PRODUCTIONS)
            .map(|i| {
                Production::new(
                    se("", &format!("e{i}"), &mut interner),
                    EventCode::one(i as u32),
                    Some(NonTerminalId::ElementContent),
                )
            })
            .collect();
        let mut nt = NonTerminal::new(NonTerminalId::ElementContent, productions);

        // learn_content_production für CH sollte false zurückgeben
        let learned = nt.learn_content_production(
            Terminal::Characters,
            Some(NonTerminalId::ElementContent),
        );
        assert!(!learned);
        assert_eq!(nt.productions().len(), MAX_LEARNED_PRODUCTIONS);
    }

    // ========================================================================
    // NonTerminalId Ord-Invariante
    // ========================================================================

    /// SchemaType muss in der Ord-Reihenfolge VOR Content2 stehen.
    /// augment_element_grammar erwartet Content2 am Ende sortierter NT-Vecs.
    #[test]
    fn non_terminal_id_schema_type_before_content2() {
        assert!(NonTerminalId::SchemaType(0) < NonTerminalId::Content2(0));
        assert!(NonTerminalId::SchemaType(99) < NonTerminalId::Content2(0));
    }
}
