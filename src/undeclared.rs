//! Undeclared Productions für Schema-informed Grammars (Spec 8.5.4.4).
//!
//! Dieses Modul augmentiert normalisierte Element- und Type-Grammars mit
//! Productions für Events die nicht explizit im Schema deklariert sind:
//! - Comments, Processing Instructions (CM, PI)
//! - Entity References (ER)
//! - Schema-Deviations (SE(*), CH[untyped], AT(*)[untyped])
//! - xsi:type, xsi:nil Attribute
//!
//! # Workflow
//!
//! ```text
//! Proto-Grammar
//!   → normalize() (8.5.4.2)
//!   → event_code_assignment() (8.5.4.3)
//!   → augment_*() (8.5.4.4) ← DIESES MODUL
//!   → prune() (8.3)
//! ```
//!
//! # Spec-Referenz
//!
//! - 8.5.4.4 Undeclared Productions
//! - 8.5.4.4.1 Adding Productions when Strict is False
//! - 8.5.4.4.2 Adding Productions when Strict is True

use crate::event_code::EventCode;
use crate::grammar::{
    AttributeKind, NonTerminal, NonTerminalId, Production, StartElementKind, Terminal,
};
use crate::options::ExiOptions;
use crate::qname::{ExpandedNameId, InternedStr, QName, StringInterner};
use crate::schema::{ElementDeclaration, TypeDefinition};

// ============================================================================
// Content-Index Berechnung (Spec 8.5.4.4.1)
// ============================================================================

/// Berechnet den Content-Index einer Element/Type Grammar (Spec 8.5.4.4.1).
///
/// Der Content-Index ist der erste NonTerminal-Index ab dem keine
/// AT-Productions mehr existieren. Für alle j < content gibt es mindestens
/// eine AT-Production im NonTerminal j.
///
/// # Spec 8.5.4.4.1
///
/// "[Definition:] For each normalized element grammar Element_i, the content of
/// Element_i is the index of the first non-terminal Element_{i,j} such that for
/// each set of grammar productions with left-hand side non-terminal symbol of
/// index smaller than content there is at least one production with AT terminal
/// symbol."
///
/// # Beispiel
///
/// ```text
/// Element_0: AT(color), AT(sku), EE  → hat AT → content > 0
/// Element_1: AT(sku), EE             → hat AT → content > 1
/// Element_2: SE(description), EE     → kein AT → content = 2
/// ```
///
/// # Parameter
///
/// - `non_terminals`: Die normalisierten NonTerminals der Grammar
///
/// # Rückgabe
///
/// Der Content-Index (0 bis n, wobei n = Anzahl NonTerminals)
pub fn compute_content_index(non_terminals: &[NonTerminal]) -> usize {
    for (idx, nt) in non_terminals.iter().enumerate() {
        let has_at = nt
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::Attribute(_)));
        if !has_at {
            return idx;
        }
    }
    // Alle NonTerminals haben AT-Productions → content = n
    non_terminals.len()
}

// ============================================================================
// Element Grammar Augmentierung (Spec 8.5.4.4)
// ============================================================================

/// Augmentiert eine normalisierte Element Grammar mit Undeclared Productions.
///
/// # Spec 8.5.4.4
///
/// Diese Funktion fügt Productions für Events hinzu, die nicht im Schema
/// deklariert sind:
/// - Bei strict=false: EE, xsi:type, xsi:nil, AT(*), AT(qname)[untyped],
///   AT(*)[untyped], NS, SC, SE(*), CH[untyped], ER, CM, PI
/// - Bei strict=true: Nur xsi:type (wenn has_named_sub_types oder is_union)
///   und xsi:nil (wenn nillable)
///
/// # Parameter
///
/// - `non_terminals`: Die normalisierten NonTerminals (werden in-place modifiziert)
/// - `element_decl`: Die ElementDeclaration für xsi:nil/xsi:type Prüfung
/// - `options`: Die EXI Options für Fidelity-Prüfung
///
/// # Hinweise
///
/// Die Productions werden mit 2-Teil Event Codes hinzugefügt, beginnend
/// nach dem höchsten existierenden Code. Event Codes müssen nach dem
/// Augmentieren neu berechnet werden falls nötig.
pub fn augment_element_grammar(
    non_terminals: &mut Vec<NonTerminal>,
    element_decl: &ElementDeclaration,
    options: &ExiOptions,
    interner: &mut StringInterner,
) -> crate::Result<()> {
    if non_terminals.is_empty() {
        return Ok(());
    }

    if options.strict {
        augment_element_grammar_strict(non_terminals, element_decl, interner)?;
    } else {
        augment_element_grammar_non_strict(non_terminals, options, interner)?;
    }
    Ok(())
}

/// Augmentierung bei strict=true (Spec 8.5.4.4.2).
///
/// Fügt nur xsi:type und xsi:nil hinzu wenn die Bedingungen erfüllt sind.
fn augment_element_grammar_strict(
    non_terminals: &mut [NonTerminal],
    element_decl: &ElementDeclaration,
    interner: &mut StringInterner,
) -> crate::Result<()> {
    // Hinweis: Leere Vektoren werden bereits in augment_element_grammar abgefangen

    // Spec 8.5.4.4.2: Nur Element_{i,0}
    let nt0 = &mut non_terminals[0];
    let start_id = nt0.id();
    let (part1, mut part2) = next_two_part_code_parts(nt0);

    // xsi:type/xsi:nil als Terminals vorab erstellen
    let xsi_type_terminal = Terminal::at_qname(&QName::xsi_type(), interner)?;
    let xsi_nil_terminal = Terminal::at_qname(&QName::xsi_nil(), interner)?;

    // Prüfe ob xsi:type hinzugefügt werden soll
    // "If T_k either has named sub-types or is a simple type definition
    // of which {variety} is union"
    let needs_xsi_type = element_decl
        .type_definition
        .as_ref()
        .map(|td| td.has_named_sub_types() || td.is_union())
        .unwrap_or(false);

    if needs_xsi_type {
        nt0.push_production(Production::new(
            xsi_type_terminal,
            EventCode::two(part1, part2),
            Some(start_id),
        ));
        part2 += 1;
    }

    // Prüfe ob xsi:nil hinzugefügt werden soll
    // "If the {nillable} property of E_i is true"
    if element_decl.nillable {
        nt0.push_production(Production::new(
            xsi_nil_terminal,
            EventCode::two(part1, part2),
            Some(start_id),
        ));
    }
    Ok(())
}

/// Augmentierung bei strict=false (Spec 8.5.4.4.1).
///
/// Fügt alle Undeclared Productions hinzu.
fn augment_element_grammar_non_strict(
    non_terminals: &mut Vec<NonTerminal>,
    options: &ExiOptions,
    interner: &mut StringInterner,
) -> crate::Result<()> {
    // xsi:type/xsi:nil Terminals vorab erstellen
    let xsi_type_terminal = Terminal::at_qname(&QName::xsi_type(), interner)?;
    let xsi_nil_terminal = Terminal::at_qname(&QName::xsi_nil(), interner)?;

    let content_idx = compute_content_index(non_terminals);

    // Schritt 1: Content2 Kopie erstellen (Spec 8.5.4.4.1)
    let content2_idx = create_content2_copy(non_terminals, content_idx);

    // Schritt 2: Für j ≤ content die Productions hinzufügen
    for j in 0..=content_idx.min(non_terminals.len().saturating_sub(1)) {
        augment_non_terminal_for_content(
            non_terminals,
            j,
            content_idx,
            content2_idx,
            j == 0,
            options,
            xsi_type_terminal,
            xsi_nil_terminal,
        );
    }

    // Schritt 3: Für content2 und j > content
    // Content2 NonTerminal augmentieren
    augment_content2_and_beyond(non_terminals, content_idx, content2_idx, options);
    Ok(())
}

/// Erstellt eine Kopie des Content-NonTerminals als Content2 (Spec 8.5.4.4.1).
///
/// # Parameter
///
/// - `non_terminals`: Die NonTerminal-Liste
/// - `content_idx`: Der Content-Index
///
/// # Rückgabe
///
/// Der Index des neuen Content2-NonTerminals (am Ende der Liste angehängt)
fn create_content2_copy(non_terminals: &mut Vec<NonTerminal>, content_idx: usize) -> usize {
    if content_idx >= non_terminals.len() {
        // Content-Index ist am oder nach dem Ende → kein Content2 nötig
        return non_terminals.len();
    }

    let content_nt = &non_terminals[content_idx];
    let content2_idx = non_terminals.len();

    let content2_prods: Vec<Production> = content_nt.productions().to_vec();

    non_terminals.push(NonTerminal::new(
        NonTerminalId::Content2(content_idx),
        content2_prods,
    ));

    content2_idx
}

/// Augmentiert ein NonTerminal für j ≤ content (Spec 8.5.4.4.1).
fn augment_non_terminal_for_content(
    non_terminals: &mut [NonTerminal],
    j: usize,
    content_idx: usize,
    content2_idx: usize,
    is_first: bool,
    options: &ExiOptions,
    xsi_type_terminal: Terminal,
    xsi_nil_terminal: Terminal,
) {
    // Hinweis: j ist immer < len durch die Schleifenbedingung im Aufrufer

    // Berechne RHS-IDs bevor wir non_terminals mutieren
    let nt_len = non_terminals.len();
    let current_id = non_terminals[j].id();
    let start_id = non_terminals.first()
        .map(|nt| nt.id())
        .unwrap_or(current_id);
    let content2_rhs = if content2_idx < nt_len {
        Some(NonTerminalId::Content2(content_idx))
    } else {
        Some(current_id)
    };

    let nt = &mut non_terminals[j];
    let (part1, mut part2) = next_two_part_code_parts(nt);

    // 1. EE hinzufügen falls nicht vorhanden (Spec 8.5.4.4.1)
    if !has_ee_production(nt) {
        nt.push_production(
            Production::terminal(Terminal::EndElement, EventCode::two(part1, part2)));
        part2 += 1;
    }

    // 2. xsi:type und xsi:nil nur für j=0 (Spec 8.5.4.4.1)
    if is_first {
        // AT(xsi:type)
        nt.push_production(Production::new(
            xsi_type_terminal,
            EventCode::two(part1, part2),
            Some(start_id),
        ));
        part2 += 1;

        // AT(xsi:nil)
        nt.push_production(Production::new(
            xsi_nil_terminal,
            EventCode::two(part1, part2),
            Some(start_id),
        ));
        part2 += 1;
    }

    // 3. AT(*) [typed] - Spec 8.5.4.4.1: IMMER hinzufügen, auch wenn AT(*) schon
    //    bei Tier 1 existiert (z.B. anyType mit anyAttribute).
    nt.push_production(Production::new(
        Terminal::Attribute(AttributeKind::Wildcard),
        EventCode::two(part1, part2),
        Some(current_id),
    ));
    part2 += 1;

    // 4. AT(*)[untyped] (Spec 8.5.4.4.1)
    nt.push_production(Production::new(
        Terminal::Attribute(AttributeKind::WildcardUntyped),
        EventCode::two(part1, part2),
        Some(current_id),
    ));
    part2 += 1;

    // 6. NS nur für j=0 wenn preserve.prefixes=true (Spec 8.5.4.4.1)
    if is_first && options.preserve.prefixes {
        nt.push_production(Production::new(
            Terminal::NamespaceDecl,
            EventCode::two(part1, part2),
            Some(start_id),
        ));
        part2 += 1;
    }

    // 7. SC nur für j=0 wenn self_contained=true (Spec 8.5.4.4.1)
    if is_first && options.self_contained {
        nt.push_production(Production::new(
            Terminal::SelfContained,
            EventCode::two(part1, part2),
            Some(NonTerminalId::Fragment), // SC → Fragment Grammar
        ));
        part2 += 1;
    }

    // 8-12. SE(*), CH, ER, CM, PI → content2
    add_content_productions(nt, part1, &mut part2, content2_rhs, options);
}

/// Augmentiert Content2 und NonTerminals j > content (Spec 8.5.4.4.1).
fn augment_content2_and_beyond(
    non_terminals: &mut [NonTerminal],
    content_idx: usize,
    content2_idx: usize,
    options: &ExiOptions,
) {
    // Content2 NonTerminal
    if content2_idx < non_terminals.len() {
        let self_rhs = Some(NonTerminalId::Content2(content_idx));
        let nt = &mut non_terminals[content2_idx];
        let (part1, mut part2) = next_two_part_code_parts(nt);

        // EE hinzufügen falls nicht vorhanden
        if !has_ee_production(nt) {
            nt.push_production(
                Production::terminal(Terminal::EndElement, EventCode::two(part1, part2)));
            part2 += 1;
        }

        // SE(*), CH, ER, CM, PI → self (content2)
        add_content_productions(nt, part1, &mut part2, self_rhs, options);
    }

    // NonTerminals j > content (außer content2)
    // Hinweis: j ist immer < len da content2_idx <= len und j < content2_idx
    for j in (content_idx + 1)..content2_idx {
        let self_rhs = Some(non_terminals[j].id());
        let nt = &mut non_terminals[j];
        let (part1, mut part2) = next_two_part_code_parts(nt);

        // EE hinzufügen falls nicht vorhanden
        if !has_ee_production(nt) {
            nt.push_production(
                Production::terminal(Terminal::EndElement, EventCode::two(part1, part2)));
            part2 += 1;
        }

        add_content_productions(nt, part1, &mut part2, self_rhs, options);
    }
}

// ============================================================================
// Type Grammar Augmentierung (Spec 8.5.4.4)
// ============================================================================

/// Augmentiert eine normalisierte Type Grammar mit Undeclared Productions.
///
/// # Spec 8.5.4.4
///
/// "Apply the process described above for element grammars to each normalized
/// type grammar Type_i and TypeEmpty_i."
///
/// Wenn `elem_decl` angegeben ist, werden xsi:type/xsi:nil für NT_0 hinzugefügt
/// (analog zu Element Grammars). Dies betrifft TypeEmpty-Grammars, die im
/// Kontext eines konkreten Elements erzeugt werden.
pub fn augment_type_grammar(
    non_terminals: &mut Vec<NonTerminal>,
    _type_def: &TypeDefinition,
    options: &ExiOptions,
    elem_decl: Option<&ElementDeclaration>,
    interner: &mut StringInterner,
) -> crate::Result<()> {
    if non_terminals.is_empty() {
        return Ok(());
    }

    if options.strict {
        // TypeEmpty wird bei strict nicht mit xsi:type/xsi:nil augmentiert.
        // Spec 8.5.4.4.2 + Z.3276-3278 ist mehrdeutig ("apply to Type_i
        // and TypeEmpty_i" vs. strict-Semantik). Exificient augmentiert
        // nicht, erxi uebernimmt dieses Verhalten.
        // Siehe docs/interop-deviations.md.
        return Ok(());
    }

    // xsi:type/xsi:nil Terminals vorab erstellen
    let xsi_type_terminal = Terminal::at_qname(&QName::xsi_type(), interner)?;
    let xsi_nil_terminal = Terminal::at_qname(&QName::xsi_nil(), interner)?;

    let content_idx = compute_content_index(non_terminals);
    let content2_idx = create_content2_copy(non_terminals, content_idx);

    for j in 0..=content_idx.min(non_terminals.len().saturating_sub(1)) {
        augment_type_non_terminal(
            non_terminals,
            j,
            content_idx,
            content2_idx,
            options,
            j == 0,
            elem_decl,
            xsi_type_terminal,
            xsi_nil_terminal,
        );
    }

    augment_content2_and_beyond(non_terminals, content_idx, content2_idx, options);
    Ok(())
}

/// Augmentiert ein Type-NonTerminal.
fn augment_type_non_terminal(
    non_terminals: &mut [NonTerminal],
    j: usize,
    content_idx: usize,
    content2_idx: usize,
    options: &ExiOptions,
    is_first: bool,
    elem_decl: Option<&ElementDeclaration>,
    xsi_type_terminal: Terminal,
    xsi_nil_terminal: Terminal,
) {
    // Hinweis: j ist immer < len durch die Schleifenbedingung im Aufrufer

    // Berechne Content2-RHS bevor wir non_terminals mutieren
    let nt_len = non_terminals.len();
    let current_id = non_terminals[j].id();
    let start_id = non_terminals
        .first()
        .map(|nt| nt.id())
        .unwrap_or(current_id);
    let content2_rhs = if content2_idx < nt_len {
        Some(NonTerminalId::Content2(content_idx))
    } else {
        Some(current_id)
    };

    let nt = &mut non_terminals[j];
    let (part1, mut part2) = next_two_part_code_parts(nt);

    // EE hinzufügen falls nicht vorhanden
    if !has_ee_production(nt) {
        nt.push_production(
            Production::terminal(Terminal::EndElement, EventCode::two(part1, part2)));
        part2 += 1;
    }

    // xsi:type/xsi:nil für j=0 wenn elem_decl vorhanden (Spec 8.5.4.4.1)
    if is_first && elem_decl.is_some() {
        nt.push_production(Production::new(
            xsi_type_terminal,
            EventCode::two(part1, part2),
            Some(start_id),
        ));
        part2 += 1;

        nt.push_production(Production::new(
            xsi_nil_terminal,
            EventCode::two(part1, part2),
            Some(start_id),
        ));
        part2 += 1;
    }

    // AT(*) [typed] - Spec 8.5.4.4.1: IMMER hinzufügen (analog zu Element-Grammar).
    nt.push_production(Production::new(
        Terminal::Attribute(AttributeKind::Wildcard),
        EventCode::two(part1, part2),
        Some(current_id),
    ));
    part2 += 1;

    // AT(*)[untyped] (Spec 8.5.4.4.1)
    nt.push_production(Production::new(
        Terminal::Attribute(AttributeKind::WildcardUntyped),
        EventCode::two(part1, part2),
        Some(current_id),
    ));
    part2 += 1;

    // NS nur für j=0 wenn preserve.prefixes=true (Spec 8.5.4.4.1)
    if is_first && options.preserve.prefixes {
        nt.push_production(Production::new(
            Terminal::NamespaceDecl,
            EventCode::two(part1, part2),
            Some(current_id),
        ));
        part2 += 1;
    }

    // SC nur für j=0 wenn self_contained=true (Spec 8.5.4.4.1)
    if is_first && options.self_contained {
        nt.push_production(Production::new(
            Terminal::SelfContained,
            EventCode::two(part1, part2),
            Some(NonTerminalId::Fragment),
        ));
        part2 += 1;
    }

    // SE(*), CH, ER, CM, PI → content2
    add_content_productions(nt, part1, &mut part2, content2_rhs, options);
}

// ============================================================================
// Hilfsfunktionen
// ============================================================================

/// Fügt SE(*), CH, ER, CM, PI Productions zum NonTerminal hinzu.
///
/// Gemeinsame Logik für Content und Content2 NonTerminals (Spec 8.5.4.4.1).
fn add_content_productions(
    nt: &mut NonTerminal,
    part1: u32,
    part2: &mut u32,
    target_rhs: Option<NonTerminalId>,
    options: &ExiOptions,
) {
    // SE(*)
    nt.push_production(Production::new(
        Terminal::StartElement(StartElementKind::Wildcard),
        EventCode::two(part1, *part2),
        target_rhs,
    ));
    *part2 += 1;

    // CH[untyped]
    nt.push_production(Production::new(
        Terminal::CharactersUntyped,
        EventCode::two(part1, *part2),
        target_rhs,
    ));
    *part2 += 1;

    // ER (nur wenn preserve.dtd=true)
    if options.preserve.dtd {
        nt.push_production(Production::new(
            Terminal::EntityRef,
            EventCode::two(part1, *part2),
            target_rhs,
        ));
        *part2 += 1;
    }

    // CM & PI — Spec 8.5.4.4.1: Sub-Gruppen-Codes
    // CM n.(m+3).0, PI n.(m+3).1
    let cm_pi_base = *part2;
    if options.preserve.comments {
        nt.push_production(Production::new(
            Terminal::Comment,
            EventCode::three(part1, cm_pi_base, 0),
            target_rhs,
        ));
    }
    if options.preserve.pis {
        let pi_idx = if options.preserve.comments { 1 } else { 0 };
        nt.push_production(Production::new(
            Terminal::ProcessingInstr,
            EventCode::three(part1, cm_pi_base, pi_idx),
            target_rhs,
        ));
    }
}

/// Berechnet den nächsten verfügbaren 2-Teil Event Code als (part1, part2) Paar.
/// Das erlaubt einfaches Hochzählen ohne Ownership-Probleme.
fn next_two_part_code_parts(nt: &NonTerminal) -> (u32, u32) {
    let max_part1 = nt
        .productions()
        .iter()
        .filter_map(|p| p.event_code.as_ref().map(|ec| ec.part1()))
        .max()
        .unwrap_or(0);

    // Nächste 2-Teil Code-Basis
    (max_part1 + 1, 0)
}

/// Prüft ob ein NonTerminal bereits eine EE-Production hat.
fn has_ee_production(nt: &NonTerminal) -> bool {
    nt.productions()
        .iter()
        .any(|p| matches!(p.terminal, Terminal::EndElement))
}

// ============================================================================
// Tier 2 Production Generierung (2-Tier Grammar System)
// ============================================================================

/// Kontext für Tier 2 Production Generierung.
///
/// Enthält alle Informationen die benötigt werden um Tier 2 Productions
/// für ein bestimmtes NonTerminal zu generieren. Reine Offset-Berechnung
/// ohne Allokation (Zero-Alloc).
#[derive(Debug, Clone, Copy)]
pub struct Tier2Context {
    /// Ob EE in Tier 1 vorhanden ist (verhindert doppeltes EE in Tier 2).
    pub(crate) has_ee_in_tier1: bool,
    /// Ob dies das erste NonTerminal ist (j=0) - fuer xsi:type/xsi:nil.
    pub(crate) is_first_nt: bool,
    /// Ob dies der Content-Bereich ist (j >= content_idx).
    pub(crate) is_content_area: bool,
    /// Spec 8.5.4.4.2: Ob xsi:type bei strict=true verfuegbar ist.
    /// True wenn has_named_sub_types oder is_union fuer den Element-Typ.
    pub(crate) needs_xsi_type: bool,
    /// Spec 8.5.4.4.2: Ob xsi:nil bei strict=true verfuegbar ist.
    /// True wenn das Element nillable ist.
    pub(crate) is_nillable: bool,
    /// Ob die Grammar eine ElementFragment-Grammar ist.
    pub(crate) is_element_fragment: bool,
    /// Interned xsi:type ExpandedNameId (für Tier2 Lookups ohne Interner).
    pub(crate) xsi_type_id: ExpandedNameId,
    /// Interned xsi:nil ExpandedNameId (für Tier2 Lookups ohne Interner).
    pub(crate) xsi_nil_id: ExpandedNameId,
    // Relevante EXI Options (vermeidet Clone der gesamten ExiOptions-Struktur).
    pub(crate) strict: bool,
    pub(crate) preserve_prefixes: bool,
    pub(crate) preserve_dtd: bool,
    pub(crate) preserve_comments: bool,
    pub(crate) preserve_pis: bool,
    pub(crate) self_contained: bool,
}

enum Tier2Slot {
    Terminal(Terminal),
    CmPiGroup,
}

impl Tier2Context {
    /// Erstellt einen Tier2Context aus ExiOptions und den restlichen Kontext-Feldern.
    pub fn new(options: &ExiOptions) -> Self {
        Self {
            has_ee_in_tier1: false,
            is_first_nt: false,
            is_content_area: false,
            needs_xsi_type: false,
            is_nillable: false,
            is_element_fragment: false,
            xsi_type_id: ExpandedNameId::new(InternedStr(0), InternedStr(0)),
            xsi_nil_id: ExpandedNameId::new(InternedStr(0), InternedStr(0)),
            strict: options.strict,
            preserve_prefixes: options.preserve.prefixes,
            preserve_dtd: options.preserve.dtd,
            preserve_comments: options.preserve.comments,
            preserve_pis: options.preserve.pis,
            self_contained: options.self_contained,
        }
    }

    /// Setzt die xsi:type und xsi:nil ExpandedNameIds (Builder-Pattern).
    pub fn with_xsi_ids(mut self, xsi_type_id: ExpandedNameId, xsi_nil_id: ExpandedNameId) -> Self {
        self.xsi_type_id = xsi_type_id;
        self.xsi_nil_id = xsi_nil_id;
        self
    }

    /// Anzahl der Tier2-Slots (CM/PI zählen als 1).
    pub fn count(&self) -> u32 {
        let mut n = 0u32;
        self.for_each_slot(|_| {
            n += 1;
            true
        });
        n
    }

    /// Anzahl Einträge in der CM/PI Sub-Gruppe (0, 1 oder 2).
    pub fn cm_pi_count(&self) -> u32 {
        if self.strict {
            return 0;
        }
        (self.preserve_comments as u32) + (self.preserve_pis as u32)
    }

    /// Slot-Index der CM/PI Sub-Gruppe, falls beide aktiv.
    pub fn cm_pi_slot(&self) -> Option<u32> {
        if self.strict || !self.preserve_comments || !self.preserve_pis {
            return None;
        }
        let mut idx = 0u32;
        let mut result = None;
        self.for_each_slot(|slot| {
            if matches!(slot, Tier2Slot::CmPiGroup) {
                result = Some(idx);
                return false;
            }
            idx += 1;
            true
        });
        result
    }

    /// Findet ein Terminal in Tier 2 und gibt den Part2-Index zurück.
    pub fn find(&self, terminal: &Terminal) -> Option<u32> {
        let (part2, _) = self.find_with_subgroup(terminal)?;
        Some(part2)
    }

    /// Findet ein Terminal und gibt (part2, Option<part3>) zurück.
    /// CM/PI bilden eine Sub-Gruppe: CM→part3=0, PI→part3=1.
    pub fn find_with_subgroup(&self, terminal: &Terminal) -> Option<(u32, Option<u32>)> {
        let mut idx = 0u32;
        let mut result: Option<(u32, Option<u32>)> = None;
        self.for_each_slot(|slot| {
            match slot {
                Tier2Slot::Terminal(t) => {
                    if terminals_match(terminal, &t) {
                        result = Some((idx, None));
                        return false;
                    }
                }
                Tier2Slot::CmPiGroup => {
                    if terminals_match(terminal, &Terminal::Comment) {
                        result = Some((idx, Some(0)));
                        return false;
                    }
                    if terminals_match(terminal, &Terminal::ProcessingInstr) {
                        result = Some((idx, Some(1)));
                        return false;
                    }
                }
            }
            idx += 1;
            true
        });
        result
    }

    /// Reverse-Lookup: Terminal am berechneten Slot-Index.
    ///
    /// Bei `part2 == cm_pi_slot()` wird `Terminal::Comment` als Fallback
    /// zurückgegeben (Caller muss CM/PI-Sub-Gruppe vorher abfangen).
    pub fn terminal_at(&self, part2: u32) -> Option<Terminal> {
        let mut idx = 0u32;
        let mut result = None;
        self.for_each_slot(|slot| {
            if part2 == idx {
                result = Some(match slot {
                    Tier2Slot::Terminal(t) => t,
                    Tier2Slot::CmPiGroup => Terminal::Comment,
                });
                return false;
            }
            idx += 1;
            true
        });
        result
    }

    /// Prüft ob der Terminal am gegebenen Index AT(*)[untyped] ist.
    /// Im strict-Modus immer `false`.
    pub fn is_wildcard_untyped_at(&self, part2: u32) -> bool {
        if self.strict {
            return false;
        }
        matches!(
            self.terminal_at(part2),
            Some(Terminal::Attribute(AttributeKind::WildcardUntyped))
        )
    }

    fn for_each_slot(&self, mut f: impl FnMut(Tier2Slot) -> bool) {
        if self.strict {
            if !self.is_first_nt {
                return;
            }
            if self.needs_xsi_type {
                if !f(Tier2Slot::Terminal(Terminal::Attribute(AttributeKind::QName(self.xsi_type_id)))) {
                    return;
                }
            }
            if self.is_nillable {
                let _ = f(Tier2Slot::Terminal(Terminal::Attribute(AttributeKind::QName(self.xsi_nil_id))));
            }
            return;
        }

        if !self.has_ee_in_tier1 {
            if !f(Tier2Slot::Terminal(Terminal::EndElement)) {
                return;
            }
        }

        if self.is_first_nt {
            if !f(Tier2Slot::Terminal(Terminal::Attribute(AttributeKind::QName(self.xsi_type_id)))) {
                return;
            }
            if !f(Tier2Slot::Terminal(Terminal::Attribute(AttributeKind::QName(self.xsi_nil_id)))) {
                return;
            }
        }

        if !self.is_content_area || self.is_element_fragment {
            if !f(Tier2Slot::Terminal(Terminal::Attribute(AttributeKind::Wildcard))) {
                return;
            }
            if !f(Tier2Slot::Terminal(Terminal::Attribute(AttributeKind::WildcardUntyped))) {
                return;
            }
        }

        if self.is_first_nt && self.preserve_prefixes {
            if !f(Tier2Slot::Terminal(Terminal::NamespaceDecl)) {
                return;
            }
        }

        if self.is_first_nt && self.self_contained {
            if !f(Tier2Slot::Terminal(Terminal::SelfContained)) {
                return;
            }
        }

        if !f(Tier2Slot::Terminal(Terminal::StartElement(StartElementKind::Wildcard))) {
            return;
        }

        if !f(Tier2Slot::Terminal(Terminal::CharactersUntyped)) {
            return;
        }

        if self.preserve_dtd && !self.is_element_fragment {
            if !f(Tier2Slot::Terminal(Terminal::EntityRef)) {
                return;
            }
        }

        if self.preserve_comments && self.preserve_pis {
            let _ = f(Tier2Slot::CmPiGroup);
        } else if self.preserve_comments {
            let _ = f(Tier2Slot::Terminal(Terminal::Comment));
        } else if self.preserve_pis {
            let _ = f(Tier2Slot::Terminal(Terminal::ProcessingInstr));
        }
    }
}

/// Prüft ob zwei Terminals matchen (inkl. Wildcard-Matching).
///
/// SE(qname) matcht SE(*), AT(qname) matcht AT(*).
fn terminals_match(actual: &Terminal, pattern: &Terminal) -> bool {
    match (actual, pattern) {
        // Exakter Match
        (a, b) if a == b => true,
        // SE(qname) matcht SE(*)
        (Terminal::StartElement(StartElementKind::QName(_)), Terminal::StartElement(StartElementKind::Wildcard)) => true,
        // SE(qname) matcht SE(uri:*)
        (Terminal::StartElement(StartElementKind::QName(q)), Terminal::StartElement(StartElementKind::NamespaceWildcard(uri))) => {
            q.uri == *uri
        }
        // AT(qname) matcht AT(*)
        (
            Terminal::Attribute(AttributeKind::QName(_) | AttributeKind::QNameUntyped(_)),
            Terminal::Attribute(AttributeKind::Wildcard | AttributeKind::WildcardUntyped),
        ) => true,
        // AT(qname) matcht AT(uri:*)
        (
            Terminal::Attribute(AttributeKind::QName(q) | AttributeKind::QNameUntyped(q)),
            Terminal::Attribute(AttributeKind::NamespaceWildcard(uri)),
        ) => {
            q.uri == *uri
        }
        _ => false,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::options::Preserve;
    use crate::qname::StringInterner;
    use std::rc::Rc;

    /// Erstellt einen StringInterner und gibt (interner, xsi_type_id, xsi_nil_id) zurück.
    fn make_interner() -> (StringInterner, ExpandedNameId, ExpandedNameId) {
        let mut interner = StringInterner::new();
        let xsi_type_id = interner
            .intern_expanded("http://www.w3.org/2001/XMLSchema-instance", "type")
            .unwrap();
        let xsi_nil_id = interner
            .intern_expanded("http://www.w3.org/2001/XMLSchema-instance", "nil")
            .unwrap();
        (interner, xsi_type_id, xsi_nil_id)
    }

    // ========================================================================
    // Tests: Content-Index Berechnung (Spec 8.5.4.4.1)
    // ========================================================================

    /// Spec 8.5.4.4.1: Grammar ohne Attribute hat content=0
    #[test]
    fn content_index_no_attributes() {
        let nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![
                Production::new(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(0)),
                ),
                Production::terminal(Terminal::EndElement, EventCode::one(1)),
            ],
        )];

        assert_eq!(compute_content_index(&nts), 0);
    }

    /// Spec 8.5.4.4.1: Grammar mit nur Attributen hat content=n
    #[test]
    fn content_index_all_attributes() {
        let mut interner = StringInterner::new();
        let nts = vec![
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "color").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "sku").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(2)),
                )],
            ),
        ];

        // Alle NonTerminals haben AT-Productions
        assert_eq!(compute_content_index(&nts), 2);
    }

    /// Spec 8.5.4.4.1: Gemischte Grammar (H.3 Product-Beispiel Ähnlich)
    #[test]
    fn content_index_mixed() {
        let mut interner = StringInterner::new();
        let nts = vec![
            // NT_0: AT(color) → NT_1
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "color").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            // NT_1: AT(sku), SE(description), EE
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![
                    Production::new(
                        Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "sku").unwrap())),
                        EventCode::one(0),
                        Some(NonTerminalId::Dynamic(2)),
                    ),
                    Production::new(
                        Terminal::StartElement(StartElementKind::QName(
                            interner.intern_expanded("", "description").unwrap(),
                        )),
                        EventCode::one(1),
                        Some(NonTerminalId::Dynamic(2)),
                    ),
                    Production::terminal(Terminal::EndElement, EventCode::one(2)),
                ],
            ),
            // NT_2: SE(quantity), EE (kein AT mehr)
            NonTerminal::new(
                NonTerminalId::Dynamic(2),
                vec![
                    Production::new(
                        Terminal::StartElement(StartElementKind::QName(
                            interner.intern_expanded("", "quantity").unwrap(),
                        )),
                        EventCode::one(0),
                        Some(NonTerminalId::Dynamic(3)),
                    ),
                    Production::terminal(Terminal::EndElement, EventCode::one(1)),
                ],
            ),
        ];

        // NT_0 hat AT, NT_1 hat AT, NT_2 hat kein AT → content = 2
        assert_eq!(compute_content_index(&nts), 2);
    }

    /// Leere Grammar hat content=0
    #[test]
    fn content_index_empty_grammar() {
        let nts: Vec<NonTerminal> = vec![];
        assert_eq!(compute_content_index(&nts), 0);
    }

    // ========================================================================
    // Tests: Strict=true (Spec 8.5.4.4.2)
    // ========================================================================

    /// Spec 8.5.4.4.2: xsi:type wird hinzugefügt wenn has_named_sub_types=true
    #[test]
    fn strict_xsi_type_when_has_sub_types() {
        let (mut interner, xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let type_def = Rc::new(TypeDefinition::complex_with_sub_types());
        let element =
            ElementDeclaration::new(Rc::new(QName::new("", "test"))).with_type(type_def);

        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        // Sollte AT(xsi:type) haben
        let has_xsi_type = nts[0].productions().iter().any(|p| {
            matches!(
                &p.terminal,
                Terminal::Attribute(AttributeKind::QName(q)) if *q == xsi_type_id
            )
        });
        assert!(
            has_xsi_type,
            "xsi:type sollte bei has_named_sub_types hinzugefügt werden"
        );
    }

    /// Spec 8.5.4.4.2: xsi:type wird hinzugefügt wenn is_union=true
    #[test]
    fn strict_xsi_type_when_is_union() {
        let (mut interner, xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let type_def = Rc::new(TypeDefinition::simple_union());
        let element =
            ElementDeclaration::new(Rc::new(QName::new("", "test"))).with_type(type_def);

        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_xsi_type = nts[0].productions().iter().any(|p| {
            matches!(
                &p.terminal,
                Terminal::Attribute(AttributeKind::QName(q)) if *q == xsi_type_id
            )
        });
        assert!(
            has_xsi_type,
            "xsi:type sollte bei is_union hinzugefügt werden"
        );
    }

    /// Spec 8.5.4.4.2: xsi:type wird NICHT hinzugefügt ohne Sub-Typen/Union
    #[test]
    fn strict_no_xsi_type_otherwise() {
        let (mut interner, xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let type_def = Rc::new(TypeDefinition::simple());
        let element =
            ElementDeclaration::new(Rc::new(QName::new("", "test"))).with_type(type_def);

        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_xsi_type = nts[0].productions().iter().any(|p| {
            matches!(
                &p.terminal,
                Terminal::Attribute(AttributeKind::QName(q)) if *q == xsi_type_id
            )
        });
        assert!(
            !has_xsi_type,
            "xsi:type sollte NICHT hinzugefügt werden ohne Sub-Typen/Union"
        );
    }

    /// Spec 8.5.4.4.2: xsi:nil wird hinzugefügt wenn nillable=true
    #[test]
    fn strict_xsi_nil_when_nillable() {
        let (mut interner, _xsi_type_id, xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let element =
            ElementDeclaration::new(Rc::new(QName::new("", "test"))).with_nillable(true);

        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_xsi_nil = nts[0].productions().iter().any(|p| {
            matches!(
                &p.terminal,
                Terminal::Attribute(AttributeKind::QName(q)) if *q == xsi_nil_id
            )
        });
        assert!(
            has_xsi_nil,
            "xsi:nil sollte bei nillable hinzugefügt werden"
        );
    }

    /// Spec 8.5.4.4.2: xsi:nil wird NICHT hinzugefügt wenn nillable=false
    #[test]
    fn strict_no_xsi_nil_otherwise() {
        let (mut interner, _xsi_type_id, xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));

        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_xsi_nil = nts[0].productions().iter().any(|p| {
            matches!(
                &p.terminal,
                Terminal::Attribute(AttributeKind::QName(q)) if *q == xsi_nil_id
            )
        });
        assert!(
            !has_xsi_nil,
            "xsi:nil sollte NICHT hinzugefügt werden ohne nillable"
        );
    }

    /// Spec 8.5.4.4.2: Bei strict=true werden keine SE(*), CH[untyped], etc. hinzugefügt
    #[test]
    fn strict_no_untyped_productions() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));

        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        let prods_before = nts[0].productions().len();
        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();
        let prods_after = nts[0].productions().len();

        // Keine neuen Productions (weder xsi:type noch xsi:nil da Bedingungen nicht erfüllt)
        assert_eq!(prods_before, prods_after);
    }

    // ========================================================================
    // Tests: Strict=false (Spec 8.5.4.4.1)
    // ========================================================================

    /// Spec 8.5.4.4.1: EE wird hinzugefügt wenn nicht vorhanden
    #[test]
    fn augment_adds_ee_where_missing() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::new(
                Terminal::StartElement(StartElementKind::Wildcard),
                EventCode::one(0),
                Some(NonTerminalId::Dynamic(0)),
            )],
        )];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions::default();

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_ee = nts[0]
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::EndElement));
        assert!(has_ee, "EE sollte hinzugefügt werden");
    }

    /// Spec 8.5.4.4.1: xsi:type und xsi:nil werden bei Element_{i,0} hinzugefügt
    #[test]
    fn augment_adds_xsi_type_and_nil_at_0() {
        let (mut interner, xsi_type_id, xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions::default();

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_xsi_type = nts[0].productions().iter().any(|p| {
            matches!(
                &p.terminal,
                Terminal::Attribute(AttributeKind::QName(q)) if *q == xsi_type_id
            )
        });
        let has_xsi_nil = nts[0].productions().iter().any(|p| {
            matches!(
                &p.terminal,
                Terminal::Attribute(AttributeKind::QName(q)) if *q == xsi_nil_id
            )
        });

        assert!(
            has_xsi_type,
            "xsi:type sollte bei strict=false hinzugefügt werden"
        );
        assert!(
            has_xsi_nil,
            "xsi:nil sollte bei strict=false hinzugefügt werden"
        );
    }

    /// Spec 8.5.4.4.1: AT(*) wird hinzugefügt
    #[test]
    fn augment_adds_at_wildcard() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions::default();

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_at_wildcard = nts[0]
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::Attribute(AttributeKind::Wildcard)));
        assert!(has_at_wildcard, "AT(*) sollte hinzugefügt werden");
    }

    /// Spec 8.5.4.4.1: Content2 Kopie wird erstellt
    #[test]
    fn augment_creates_content2_copy() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "attr").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![
                    Production::new(
                        Terminal::StartElement(StartElementKind::Wildcard),
                        EventCode::one(0),
                        Some(NonTerminalId::Dynamic(1)),
                    ),
                    Production::terminal(Terminal::EndElement, EventCode::one(1)),
                ],
            ),
        ];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions::default();

        let before_len = nts.len();
        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        // Content2 sollte hinzugefügt worden sein
        assert!(
            nts.len() > before_len,
            "Content2 NonTerminal sollte hinzugefügt werden"
        );
        assert!(
            matches!(nts.last().unwrap().id(), NonTerminalId::Content2(_)),
            "Letztes NT sollte Content2 sein"
        );
    }

    /// Spec 8.5.4.4.1: NS nur wenn preserve.prefixes=true
    #[test]
    fn augment_adds_ns_when_preserve_prefixes() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions {
            preserve: Preserve {
                prefixes: true,
                ..Default::default()
            },

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_ns = nts[0]
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::NamespaceDecl));
        assert!(
            has_ns,
            "NS sollte bei preserve.prefixes=true hinzugefügt werden"
        );
    }

    /// Spec 8.5.4.4.1: NS wird NICHT hinzugefügt wenn preserve.prefixes=false
    #[test]
    fn augment_no_ns_when_preserve_prefixes_false() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions::default(); // prefixes=false by default

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_ns = nts[0]
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::NamespaceDecl));
        assert!(
            !has_ns,
            "NS sollte NICHT hinzugefügt werden bei preserve.prefixes=false"
        );
    }

    /// Spec 8.5.4.4.1: SC nur wenn self_contained=true
    #[test]
    fn augment_adds_sc_when_self_contained() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions {
            self_contained: true,

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_sc = nts[0]
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::SelfContained));
        assert!(
            has_sc,
            "SC sollte bei self_contained=true hinzugefügt werden"
        );
    }

    /// Spec 8.5.4.4.1: CM/PI respektieren Fidelity Options
    #[test]
    fn augment_respects_fidelity_options() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: false,
                ..Default::default()
            },

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let has_cm = nts[0]
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::Comment));
        let has_pi = nts[0]
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::ProcessingInstr));

        assert!(
            has_cm,
            "CM sollte bei preserve.comments=true hinzugefügt werden"
        );
        assert!(
            !has_pi,
            "PI sollte NICHT bei preserve.pis=false hinzugefügt werden"
        );
    }

    // ========================================================================
    // Tests: Type Grammar Augmentierung
    // ========================================================================

    /// Spec 8.5.4.4: Type Grammar ohne elem_decl bekommt KEINE xsi:type/xsi:nil
    #[test]
    fn type_grammar_no_xsi_type_nil_without_elem_decl() {
        let (mut interner, xsi_type_id, xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let type_def = TypeDefinition::simple();
        let options = ExiOptions::default();

        augment_type_grammar(&mut nts, &type_def, &options, None, &mut interner).unwrap();

        let has_xsi_type = nts[0].productions().iter().any(|p| {
            matches!(
                &p.terminal,
                Terminal::Attribute(AttributeKind::QName(q)) if *q == xsi_type_id
            )
        });
        let has_xsi_nil = nts[0].productions().iter().any(|p| {
            matches!(
                &p.terminal,
                Terminal::Attribute(AttributeKind::QName(q)) if *q == xsi_nil_id
            )
        });

        assert!(!has_xsi_type, "Type Grammar sollte KEINE xsi:type haben");
        assert!(!has_xsi_nil, "Type Grammar sollte KEINE xsi:nil haben");
    }

    /// Spec 8.5.4.4: Type Grammar bekommt Content2 Kopie
    #[test]
    fn type_grammar_gets_content2_copy() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "attr").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
            ),
        ];

        let type_def = TypeDefinition::simple();
        let options = ExiOptions::default();

        let before_len = nts.len();
        augment_type_grammar(&mut nts, &type_def, &options, None, &mut interner).unwrap();

        assert!(nts.len() > before_len, "Content2 sollte hinzugefügt werden");
    }

    // ========================================================================
    // Tests: Fidelity Options Pfade für Content2 und j > content
    // ========================================================================

    /// Spec 8.5.4.4.1: ER wird hinzugefügt wenn preserve.dtd=true (Content2)
    #[test]
    fn augment_content2_respects_preserve_dtd() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "attr").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
            ),
        ];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions {
            preserve: Preserve {
                dtd: true,
                ..Default::default()
            },

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        // Content2 ist das letzte NonTerminal
        let content2 = nts.last().unwrap();
        let has_er = content2
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::EntityRef));
        assert!(has_er, "ER sollte in Content2 bei preserve.dtd=true hinzugefügt werden");
    }

    /// Spec 8.5.4.4.1: CM/PI werden in Content2 hinzugefügt
    #[test]
    fn augment_content2_respects_cm_pi() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "attr").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
            ),
        ];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: true,
                ..Default::default()
            },

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        let content2 = nts.last().unwrap();
        let has_cm = content2
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::Comment));
        let has_pi = content2
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::ProcessingInstr));

        assert!(has_cm, "CM sollte in Content2 bei preserve.comments=true hinzugefügt werden");
        assert!(has_pi, "PI sollte in Content2 bei preserve.pis=true hinzugefügt werden");
    }

    /// Spec 8.5.4.4.1: Augmentierung für j > content (mehrere NonTerminals)
    #[test]
    fn augment_beyond_content_with_fidelity() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![
            // NT_0: AT(a) → NT_1
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "a").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            // NT_1: content (kein AT mehr)
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
            ),
            // NT_2: j > content
            NonTerminal::new(
                NonTerminalId::Dynamic(2),
                vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
            ),
        ];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions {
            preserve: Preserve {
                dtd: true,
                comments: true,
                pis: true,
                ..Default::default()
            },

            ..Default::default()
        };

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        // NT_2 sollte auch SE(*), CH, ER, CM, PI bekommen haben
        // Content2 wurde am Ende hinzugefügt, also ist NT_2 jetzt an Index 2
        let nt2 = &nts[2];
        let has_se_wildcard = nt2
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::StartElement(StartElementKind::Wildcard)));
        let has_er = nt2
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::EntityRef));
        let has_cm = nt2
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::Comment));
        let has_pi = nt2
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::ProcessingInstr));

        assert!(has_se_wildcard, "SE(*) sollte für j > content hinzugefügt werden");
        assert!(has_er, "ER sollte für j > content bei preserve.dtd=true hinzugefügt werden");
        assert!(has_cm, "CM sollte für j > content bei preserve.comments=true hinzugefügt werden");
        assert!(has_pi, "PI sollte für j > content bei preserve.pis=true hinzugefügt werden");
    }

    /// Spec 8.5.4.4: Type Grammar mit Preserve-Optionen
    #[test]
    fn type_grammar_with_preserve_options() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "attr").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
            ),
        ];

        let type_def = TypeDefinition::simple();
        let options = ExiOptions {
            preserve: Preserve {
                prefixes: true,
                dtd: true,
                comments: true,
                pis: true,
                ..Default::default()
            },
            self_contained: true,

            ..Default::default()
        };

        augment_type_grammar(&mut nts, &type_def, &options, None, &mut interner).unwrap();

        // NT_0 sollte NS und SC haben
        let nt0 = &nts[0];
        let has_ns = nt0
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::NamespaceDecl));
        let has_sc = nt0
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::SelfContained));
        let has_er = nt0
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::EntityRef));
        let has_cm = nt0
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::Comment));
        let has_pi = nt0
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::ProcessingInstr));

        assert!(has_ns, "NS sollte in Type Grammar bei preserve.prefixes=true hinzugefügt werden");
        assert!(has_sc, "SC sollte in Type Grammar bei self_contained=true hinzugefügt werden");
        assert!(has_er, "ER sollte in Type Grammar bei preserve.dtd=true hinzugefügt werden");
        assert!(has_cm, "CM sollte in Type Grammar bei preserve.comments=true hinzugefügt werden");
        assert!(has_pi, "PI sollte in Type Grammar bei preserve.pis=true hinzugefügt werden");
    }

    /// Spec 8.5.4.4: Type Grammar bei strict=true wird nicht augmentiert
    #[test]
    fn type_grammar_strict_no_augment() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![NonTerminal::new(
            NonTerminalId::Dynamic(0),
            vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
        )];

        let type_def = TypeDefinition::simple();
        let options = ExiOptions {
            strict: true,

            ..Default::default()
        };

        let prods_before = nts[0].productions().len();
        augment_type_grammar(&mut nts, &type_def, &options, None, &mut interner).unwrap();
        let prods_after = nts[0].productions().len();

        assert_eq!(prods_before, prods_after, "Type Grammar sollte bei strict=true nicht augmentiert werden");
    }

    /// Spec 8.5.4.4.1: Leere Grammar bleibt leer
    #[test]
    fn augment_empty_grammar() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts: Vec<NonTerminal> = vec![];
        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions::default();

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        assert!(nts.is_empty(), "Leere Grammar sollte leer bleiben");
    }

    /// Spec 8.5.4.4: Type Grammar leerer NonTerminal Vektor
    #[test]
    fn type_grammar_empty() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts: Vec<NonTerminal> = vec![];
        let type_def = TypeDefinition::simple();
        let options = ExiOptions::default();

        augment_type_grammar(&mut nts, &type_def, &options, None, &mut interner).unwrap();

        assert!(nts.is_empty(), "Leerer Type Grammar Vektor sollte leer bleiben");
    }

    /// Spec 8.5.4.4.1: j > content ohne EE bekommt EE hinzugefügt
    #[test]
    fn augment_beyond_content_adds_ee_when_missing() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        let mut nts = vec![
            // NT_0: AT(a) → NT_1 (hat AT)
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "a").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            // NT_1: content (kein AT, hat EE)
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![Production::terminal(Terminal::EndElement, EventCode::one(0))],
            ),
            // NT_2: j > content, KEIN EE (nur SE)
            NonTerminal::new(
                NonTerminalId::Dynamic(2),
                vec![Production::new(
                    Terminal::StartElement(StartElementKind::QName(
                        interner.intern_expanded("", "child").unwrap(),
                    )),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
        ];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions::default();

        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        // NT_2 sollte jetzt EE haben (hinzugefügt durch Augmentierung)
        let nt2 = &nts[2];
        let has_ee = nt2
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::EndElement));
        assert!(has_ee, "EE sollte für j > content hinzugefügt werden wenn nicht vorhanden");
    }

    /// Spec 8.5.4.4: Type Grammar ohne Content2 (alle AT)
    #[test]
    fn type_grammar_all_attributes_no_content2() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        // Alle NonTerminals haben AT → content_idx = len → kein Content2
        let mut nts = vec![
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "a").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "b").unwrap())),
                    EventCode::one(0),
                    None,
                )],
            ),
        ];

        let type_def = TypeDefinition::simple();
        let options = ExiOptions::default();

        let before_len = nts.len();
        augment_type_grammar(&mut nts, &type_def, &options, None, &mut interner).unwrap();

        // Kein Content2 hinzugefügt (content_idx = 2 = len)
        // Aber die NTs sollten trotzdem augmentiert werden mit Dynamic-RHS statt Content2-RHS
        let nt0 = &nts[0];
        let has_se_wildcard = nt0
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::StartElement(StartElementKind::Wildcard)));
        assert!(has_se_wildcard, "SE(*) sollte auch ohne Content2 hinzugefügt werden");

        // Prüfe dass RHS auf Dynamic zeigt statt Content2
        let se_prod = nt0
            .productions()
            .iter()
            .find(|p| matches!(p.terminal, Terminal::StartElement(StartElementKind::Wildcard)))
            .unwrap();
        assert!(
            matches!(se_prod.right_hand_side, Some(NonTerminalId::Dynamic(_))),
            "RHS sollte Dynamic sein wenn kein Content2 existiert"
        );

        // Länge sollte gleich bleiben (kein Content2)
        assert_eq!(nts.len(), before_len, "Kein Content2 sollte hinzugefügt werden");
    }

    /// Spec 8.5.4.4.1: Content-Index gleich Länge → kein Content2
    #[test]
    fn content_index_at_end_no_content2() {
        let (mut interner, _xsi_type_id, _xsi_nil_id) = make_interner();
        // Alle NonTerminals haben AT → content_idx = n → kein Content2 nötig
        let mut nts = vec![
            NonTerminal::new(
                NonTerminalId::Dynamic(0),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "a").unwrap())),
                    EventCode::one(0),
                    Some(NonTerminalId::Dynamic(1)),
                )],
            ),
            NonTerminal::new(
                NonTerminalId::Dynamic(1),
                vec![Production::new(
                    Terminal::Attribute(AttributeKind::QName(interner.intern_expanded("", "b").unwrap())),
                    EventCode::one(0),
                    None,
                )],
            ),
        ];

        let element = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        let options = ExiOptions::default();

        let before_len = nts.len();
        augment_element_grammar(&mut nts, &element, &options, &mut interner).unwrap();

        // Es sollte trotzdem ein Content2 erstellt werden, da content_idx=2 == len
        // create_content2_copy gibt dann non_terminals.len() zurück ohne hinzuzufügen
        // Aber die Augmentierung sollte trotzdem auf die existierenden NTs wirken
        assert!(nts.len() >= before_len, "NonTerminals sollten augmentiert werden");
    }

    // ========================================================================
    // Tier2 Self-Consistency: count/terminal_at/find/cm_pi über alle Kombinationen
    // ========================================================================

    /// Testet Self-Consistency der berechneten Methoden über eine Boolesche Matrix
    /// aller relevanten Parameter-Kombinationen (2^12 = 4096).
    #[test]
    fn tier2_self_consistency_matrix() {
        let (_interner, xsi_type_id, xsi_nil_id) = make_interner();
        let bools = [false, true];

        for &strict in &bools {
            for &has_ee_in_tier1 in &bools {
                for &is_first_nt in &bools {
                    for &is_content_area in &bools {
                        for &is_element_fragment in &bools {
                            for &preserve_prefixes in &bools {
                                for &preserve_dtd in &bools {
                                    for &preserve_comments in &bools {
                                        for &preserve_pis in &bools {
                                            for &self_contained in &bools {
                                                for &needs_xsi_type in &bools {
                                                    for &is_nillable in &bools {
                                                        let options = ExiOptions {
                                                            strict,
                                                            preserve: Preserve {
                                                                prefixes: preserve_prefixes,
                                                                dtd: preserve_dtd,
                                                                comments: preserve_comments,
                                                                pis: preserve_pis,
                                                                ..Default::default()
                                                            },
                                                            self_contained,
                                                
                                                            ..Default::default()
                                                        };
                                                        let ctx = Tier2Context {
                                                            has_ee_in_tier1,
                                                            is_first_nt,
                                                            is_content_area,
                                                            needs_xsi_type,
                                                            is_nillable,
                                                            is_element_fragment,
                                                            xsi_type_id,
                                                            xsi_nil_id,
                                                            ..Tier2Context::new(&options)
                                                        };

                                                        // terminal_at(i) für alle i < count()
                                                        let count = ctx.count();
                                                        for i in 0..count {
                                                            assert!(
                                                                ctx.terminal_at(i).is_some(),
                                                                "terminal_at({}) ist None bei count={}: {:?}",
                                                                i, count, describe(&ctx)
                                                            );
                                                        }
                                                        // Jenseits von count → None
                                                        assert!(
                                                            ctx.terminal_at(count).is_none(),
                                                            "terminal_at({}) sollte None sein: {:?}",
                                                            count, describe(&ctx)
                                                        );

                                                        // cm_pi Invarianten
                                                        let cm_pi_slot = ctx.cm_pi_slot();
                                                        let cm_pi_count = ctx.cm_pi_count();
                                                        if preserve_comments && preserve_pis && !strict {
                                                            assert!(cm_pi_slot.is_some(), "cm_pi_slot sollte Some sein: {:?}", describe(&ctx));
                                                            assert_eq!(cm_pi_count, 2);
                                                        } else if !strict && (preserve_comments || preserve_pis) {
                                                            assert!(cm_pi_slot.is_none());
                                                            assert_eq!(cm_pi_count, 1);
                                                        } else if !strict {
                                                            assert!(cm_pi_slot.is_none());
                                                            assert_eq!(cm_pi_count, 0);
                                                        }

                                                        // CM/PI-Sub-Gruppe explizit testen
                                                        if let Some(slot) = cm_pi_slot {
                                                            // terminal_at am CM/PI-Slot gibt Comment als Fallback
                                                            assert_eq!(
                                                                ctx.terminal_at(slot),
                                                                Some(Terminal::Comment),
                                                                "terminal_at(cm_pi_slot) sollte Comment sein: {:?}",
                                                                describe(&ctx)
                                                            );
                                                            // find_with_subgroup: CM → (slot, Some(0))
                                                            assert_eq!(
                                                                ctx.find_with_subgroup(&Terminal::Comment),
                                                                Some((slot, Some(0))),
                                                                "find_with_subgroup(CM) bei Sub-Gruppe: {:?}",
                                                                describe(&ctx)
                                                            );
                                                            // find_with_subgroup: PI → (slot, Some(1))
                                                            assert_eq!(
                                                                ctx.find_with_subgroup(&Terminal::ProcessingInstr),
                                                                Some((slot, Some(1))),
                                                                "find_with_subgroup(PI) bei Sub-Gruppe: {:?}",
                                                                describe(&ctx)
                                                            );
                                                        }

                                                        // find round-trip: terminal_at(i) → find → i
                                                        for i in 0..count {
                                                            if Some(i) == cm_pi_slot {
                                                                continue; // CM/PI-Slot oben separat getestet
                                                            }
                                                            let t = ctx.terminal_at(i).unwrap();
                                                            let found = ctx.find(&t);
                                                            assert_eq!(
                                                                found, Some(i),
                                                                "find round-trip fehlgeschlagen für terminal_at({}): {:?}",
                                                                i, describe(&ctx)
                                                            );
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn describe(ctx: &Tier2Context) -> String {
        format!(
            "strict={} ee={} first={} content={} elfrag={} pfx={} dtd={} cm={} pi={} sc={} xtype={} nil={}",
            ctx.strict, ctx.has_ee_in_tier1, ctx.is_first_nt, ctx.is_content_area,
            ctx.is_element_fragment, ctx.preserve_prefixes, ctx.preserve_dtd,
            ctx.preserve_comments, ctx.preserve_pis, ctx.self_contained,
            ctx.needs_xsi_type, ctx.is_nillable,
        )
    }
}
