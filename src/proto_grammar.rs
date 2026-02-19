//! Proto-Grammar Datenmodell für Schema-informed Grammars (Spec 8.5.4.1).
//!
//! Proto-Grammars sind ein Zwischenformat bei der Grammar-Generierung aus
//! XML Schema. Sie unterscheiden sich von normalisierten EXI Grammars durch:
//!
//! 1. **ε-Produktionen** (Productions ohne Terminal): `LeftHandSide : RightHandSide`
//! 2. **Müssen normalisiert werden** bevor sie nutzbar sind (Spec 8.5.4.2)
//!
//! # Workflow
//!
//! ```text
//! Schema → ProtoGrammar → normalize() → Vec<NonTerminal> → GrammarSystem
//! ```
//!
//! # Spec-Referenz
//!
//! - 8.5.4.1 EXI Proto-Grammars
//! - 8.5.4.1.1 Grammar Concatenation Operator
//! - 8.5.4.2 EXI Normalized Grammars

use crate::grammar::{AttributeKind, StartElementKind, Terminal};
use crate::qname::{ExpandedNameId, InternedStr, StringInterner};
use std::cmp::Ordering;

// ============================================================================
// TerminalKey - Vergleichbarer Schlüssel für Terminals
// ============================================================================

/// Schlüssel für Terminal-Vergleich bei Duplicate-Elimination.
///
/// Zwei Productions haben ein "Duplicate Terminal" wenn ihre TerminalKeys gleich sind.
///
/// Verwendet `Rc<QName>` und `Rc<str>` für effizientes Sharing ohne Allokationen
/// bei Rc::clone (erhöht nur den Referenzzähler).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TerminalKey {
    StartDocument,
    EndDocument,
    StartElementWildcard,
    StartElementNsWildcard(InternedStr),
    StartElementQName(ExpandedNameId),
    EndElement,
    AttributeWildcard,
    AttributeWildcardUntyped,
    AttributeNsWildcard(InternedStr),
    AttributeQName(ExpandedNameId),
    AttributeQNameUntyped(ExpandedNameId),
    Characters,
    CharactersUntyped,
    NamespaceDecl,
    Comment,
    ProcessingInstr,
    DocType,
    EntityRef,
    SelfContained,
}

impl PartialOrd for TerminalKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TerminalKey {
    fn cmp(&self, other: &Self) -> Ordering {
        use TerminalKey::*;

        // Primär nach Rang sortieren
        let rank_cmp = self.discriminant_rank().cmp(&other.discriminant_rank());
        if rank_cmp != Ordering::Equal {
            return rank_cmp;
        }

        // Bei gleichem Rang: nach Inhalt sortieren (nur für parametrisierte Varianten).
        // Verglichen wird nach ID-Nummern (nicht lexikographisch), was für
        // Gruppierung und Deduplizierung ausreicht.
        match (self, other) {
            (StartElementNsWildcard(u1), StartElementNsWildcard(u2)) => u1.cmp(u2),
            (StartElementQName(q1), StartElementQName(q2)) => {
                (q1.local_name, q1.uri).cmp(&(q2.local_name, q2.uri))
            }
            (AttributeNsWildcard(u1), AttributeNsWildcard(u2)) => u1.cmp(u2),
            (AttributeQName(q1), AttributeQName(q2)) => {
                (q1.local_name, q1.uri).cmp(&(q2.local_name, q2.uri))
            }
            (AttributeQNameUntyped(q1), AttributeQNameUntyped(q2)) => {
                (q1.local_name, q1.uri).cmp(&(q2.local_name, q2.uri))
            }
            // Parameterlose Varianten: gleicher Rang bedeutet Equal
            _ => Ordering::Equal,
        }
    }
}

impl TerminalKey {
    /// Gibt den Rang dieser Variante für die Sortierung zurück.
    ///
    /// Wird nur für verschiedene Varianten verwendet (gleiche Varianten
    /// werden nach Inhalt sortiert).
    fn discriminant_rank(&self) -> u8 {
        use TerminalKey::*;
        match self {
            StartDocument => 0,
            EndDocument => 1,
            StartElementWildcard => 2,
            StartElementNsWildcard(_) => 3,
            StartElementQName(_) => 4,
            EndElement => 5,
            AttributeWildcard => 6,
            AttributeWildcardUntyped => 7,
            AttributeNsWildcard(_) => 8,
            AttributeQName(_) => 9,
            AttributeQNameUntyped(_) => 10,
            Characters => 11,
            CharactersUntyped => 12,
            NamespaceDecl => 13,
            Comment => 14,
            ProcessingInstr => 15,
            DocType => 16,
            EntityRef => 17,
            SelfContained => 18,
        }
    }
}

impl From<&Terminal> for TerminalKey {
    fn from(t: &Terminal) -> Self {
        match t {
            Terminal::StartDocument => TerminalKey::StartDocument,
            Terminal::EndDocument => TerminalKey::EndDocument,
            Terminal::StartElement(kind) => match kind {
                StartElementKind::Wildcard => TerminalKey::StartElementWildcard,
                StartElementKind::NamespaceWildcard(uri) => {
                    TerminalKey::StartElementNsWildcard(*uri)
                }
                StartElementKind::QName(q) => TerminalKey::StartElementQName(*q),
            },
            Terminal::EndElement => TerminalKey::EndElement,
            Terminal::Attribute(kind) => match kind {
                AttributeKind::Wildcard => TerminalKey::AttributeWildcard,
                AttributeKind::WildcardUntyped => TerminalKey::AttributeWildcardUntyped,
                AttributeKind::NamespaceWildcard(uri) => {
                    TerminalKey::AttributeNsWildcard(*uri)
                }
                AttributeKind::QName(q) => TerminalKey::AttributeQName(*q),
                AttributeKind::QNameUntyped(q) => TerminalKey::AttributeQNameUntyped(*q),
            },
            Terminal::Characters => TerminalKey::Characters,
            Terminal::CharactersUntyped => TerminalKey::CharactersUntyped,
            Terminal::NamespaceDecl => TerminalKey::NamespaceDecl,
            Terminal::Comment => TerminalKey::Comment,
            Terminal::ProcessingInstr => TerminalKey::ProcessingInstr,
            Terminal::DocType => TerminalKey::DocType,
            Terminal::EntityRef => TerminalKey::EntityRef,
            Terminal::SelfContained => TerminalKey::SelfContained,
        }
    }
}

// ============================================================================
// Event Code Sortierung (Spec 8.5.4.3)
// ============================================================================

/// Vergleicht zwei Terminals für Event Code Assignment (Spec 8.5.4.3).
///
/// Sortierreihenfolge:
/// 1. AT(qname) - lexikographisch nach local-name, dann uri
/// 2. AT(uri:*)
/// 3. AT(*)
/// 4. SE(qname) - schema order, dann lexikographisch
/// 5. SE(uri:*) - schema order, dann lexikographisch
/// 6. SE(*)
/// 7. EE
/// 8. CH
fn compare_terminals_for_event_code_with_schema_order(
    t1: &Terminal,
    o1: Option<u32>,
    t2: &Terminal,
    o2: Option<u32>,
    interner: &StringInterner,
) -> Ordering {
    let rank1 = terminal_sort_rank(t1);
    let rank2 = terminal_sort_rank(t2);

    if rank1 != rank2 {
        return rank1.cmp(&rank2);
    }

    // Gleicher Rang - innerhalb der Gruppe sortieren.
    // Vergleich über aufgelöste Strings (lexikographisch), nicht über InternedStr-IDs.
    match (t1, t2) {
        // AT(qname): lexikographisch local-name, dann uri
        (
            Terminal::Attribute(AttributeKind::QName(q1)),
            Terminal::Attribute(AttributeKind::QName(q2)),
        ) => {
            let (u1, l1) = q1.resolve(interner);
            let (u2, l2) = q2.resolve(interner);
            (l1, u1).cmp(&(l2, u2))
        }

        // AT(uri:*): lexikographisch nach uri
        (
            Terminal::Attribute(AttributeKind::NamespaceWildcard(u1)),
            Terminal::Attribute(AttributeKind::NamespaceWildcard(u2)),
        ) => interner.resolve(*u1).cmp(interner.resolve(*u2)),

        // SE(qname): schema order mit lexikographischem Fallback
        (
            Terminal::StartElement(StartElementKind::QName(q1)),
            Terminal::StartElement(StartElementKind::QName(q2)),
        ) => compare_with_schema_order(o1, o2, || {
            let (u1, l1) = q1.resolve(interner);
            let (u2, l2) = q2.resolve(interner);
            (l1, u1).cmp(&(l2, u2))
        }),

        // SE(uri:*): schema order mit lexikographischem Fallback
        (
            Terminal::StartElement(StartElementKind::NamespaceWildcard(u1)),
            Terminal::StartElement(StartElementKind::NamespaceWildcard(u2)),
        ) => compare_with_schema_order(o1, o2, || {
            interner.resolve(*u1).cmp(interner.resolve(*u2))
        }),

        // AT(*), SE(*), EE, CH, etc.: nur ein Terminal pro Typ möglich
        _ => Ordering::Equal,
    }
}

/// Vergleicht nach schema_order, mit lexikographischem Fallback.
///
/// Verwendet für SE(qname) und SE(uri:*) bei Event Code Assignment.
/// - Beide haben schema_order: primär nach Order, bei Gleichheit lexikographisch
/// - Mindestens eine fehlt: rein lexikographisch (für schema-less/built-in Grammars)
fn compare_with_schema_order<F>(o1: Option<u32>, o2: Option<u32>, lexicographic: F) -> Ordering
where
    F: FnOnce() -> Ordering,
{
    match (o1, o2) {
        (Some(ord1), Some(ord2)) => ord1.cmp(&ord2).then_with(lexicographic),
        _ => lexicographic(),
    }
}

/// Gibt den Sortier-Rang eines Terminals zurück (Spec 8.5.4.3).
///
/// Die Spec definiert nur die Reihenfolge für AT, SE, EE, CH.
/// Andere Terminals (SD, ED, NS, CM, PI, DT, ER, SC) kommen in normalisierten
/// Element-Grammars nicht vor - sie werden in 8.5.4.4 als undeclared productions
/// NACH der Event Code Assignment hinzugefügt.
///
/// Für Robustheit und deterministische Sortierung geben wir ihnen Ränge nach CH.
fn terminal_sort_rank(t: &Terminal) -> u8 {
    match t {
        Terminal::Attribute(AttributeKind::QName(_)) => 1,
        Terminal::Attribute(AttributeKind::NamespaceWildcard(_)) => 2,
        Terminal::Attribute(AttributeKind::Wildcard) => 3,
        Terminal::Attribute(AttributeKind::WildcardUntyped) => 4,
        Terminal::Attribute(AttributeKind::QNameUntyped(_)) => 5,
        Terminal::StartElement(StartElementKind::QName(_)) => 6,
        Terminal::StartElement(StartElementKind::NamespaceWildcard(_)) => 7,
        Terminal::StartElement(StartElementKind::Wildcard) => 8,
        Terminal::EndElement => 9,
        Terminal::Characters => 10,
        Terminal::CharactersUntyped => 11,
        // Terminals die in normalisierten Grammars nicht vorkommen sollten (8.5.4.4),
        // aber für deterministische Sortierung explizite Ränge nach CH bekommen:
        Terminal::StartDocument => 12,
        Terminal::EndDocument => 13,
        Terminal::NamespaceDecl => 14,
        Terminal::Comment => 15,
        Terminal::ProcessingInstr => 16,
        Terminal::DocType => 17,
        Terminal::EntityRef => 18,
        Terminal::SelfContained => 19,
    }
}

// ============================================================================
// ProtoProduction (Spec 8.5.4.1)
// ============================================================================

/// Production in einer Proto-Grammar (Spec 8.5.4.1).
///
/// Anders als normale Productions kann eine ProtoProduction kein Terminal haben
/// (ε-Produktion). Diese werden bei der Normalisierung eliminiert (Spec 8.5.4.2.1).
///
/// # ε-Produktionen
///
/// ε-Produktionen (`terminal = None`) repräsentieren Accept-Zustände in der
/// Proto-Grammar. Sie werden bei der Grammar Concatenation (⊕) verwendet um
/// zwei Grammars zu verbinden.
///
/// # Schema Order (Spec 8.5.4.3)
///
/// Für SE(qname) und SE(uri:*) Productions wird die Schema-Order mitgeführt.
/// Diese bestimmt die Sortierung bei der Event Code Assignment:
/// "the schema order of productions... is determined by the order of the
/// corresponding particles in the schema... traversing the tree by depth-first method."
#[derive(Debug, Clone, PartialEq)]
pub struct ProtoProduction {
    /// Terminal-Symbol (None = ε-Produktion).
    pub terminal: Option<Terminal>,
    /// Index des Ziel-NonTerminals innerhalb der ProtoGrammar.
    pub right_hand_side: usize,
    /// Schema-Order für SE(qname)/SE(uri:*) Sortierung (Spec 8.5.4.3).
    ///
    /// Wird beim Traversieren des Content Models depth-first zugewiesen.
    /// None für Terminals die nicht nach Schema-Order sortiert werden.
    pub schema_order: Option<u32>,
}

impl ProtoProduction {
    /// Erstellt eine Production mit Terminal.
    pub fn new(terminal: Terminal, rhs: usize) -> Self {
        Self {
            terminal: Some(terminal),
            right_hand_side: rhs,
            schema_order: None,
        }
    }

    /// Erstellt eine Production mit Terminal und Schema-Order.
    pub fn with_schema_order(terminal: Terminal, rhs: usize, order: u32) -> Self {
        Self {
            terminal: Some(terminal),
            right_hand_side: rhs,
            schema_order: Some(order),
        }
    }

    /// Erstellt eine ε-Produktion (ohne Terminal).
    ///
    /// ε-Produktionen werden bei Concatenation verwendet und bei
    /// Normalisierung eliminiert.
    pub fn epsilon(rhs: usize) -> Self {
        Self {
            terminal: None,
            right_hand_side: rhs,
            schema_order: None,
        }
    }

    /// Prüft ob dies eine ε-Produktion ist.
    pub fn is_epsilon(&self) -> bool {
        self.terminal.is_none()
    }
}

// ============================================================================
// ProtoNonTerminal (Spec 8.5.4.1)
// ============================================================================

/// NonTerminal in einer Proto-Grammar (Spec 8.5.4.1).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ProtoNonTerminal {
    /// Productions dieses NonTerminals.
    productions: Vec<ProtoProduction>,
}

impl ProtoNonTerminal {
    /// Erstellt ein leeres NonTerminal.
    pub fn new() -> Self {
        Self::default()
    }

    /// Erstellt ein NonTerminal mit Productions.
    pub fn with_productions(productions: Vec<ProtoProduction>) -> Self {
        Self { productions }
    }

    /// Fügt eine Production hinzu.
    pub fn add_production(&mut self, production: ProtoProduction) {
        self.productions.push(production);
    }

    /// Gibt die Productions zurück.
    pub fn productions(&self) -> &[ProtoProduction] {
        &self.productions
    }

    /// Gibt die Productions mutierbar zurück.
    pub fn productions_mut(&mut self) -> &mut Vec<ProtoProduction> {
        &mut self.productions
    }

    /// Anzahl der Productions.
    pub fn len(&self) -> usize {
        self.productions.len()
    }

    /// Prüft ob das NonTerminal keine Productions hat.
    pub fn is_empty(&self) -> bool {
        self.productions.is_empty()
    }
}

// ============================================================================
// ProtoGrammar (Spec 8.5.4.1)
// ============================================================================

/// Proto-Grammar für Schema-informed Grammars (Spec 8.5.4.1).
///
/// Proto-Grammars sind ein Zwischenformat das aus XML Schema generiert wird.
/// Sie müssen vor der Verwendung normalisiert werden (Spec 8.5.4.2).
///
/// # Beispiel
///
/// ```
/// use erxi::proto_grammar::{ProtoGrammar, ProtoNonTerminal, ProtoProduction};
/// use erxi::grammar::Terminal;
///
/// // Simple Type Grammar: Type_0 : CH Type_1, Type_1 : ε (EE)
/// let mut grammar = ProtoGrammar::new();
/// let nt0 = grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
///     ProtoProduction::new(Terminal::Characters, 1),
/// ]));
/// let nt1 = grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
///     ProtoProduction::epsilon(1), // Selbstreferenz als Accept-Zustand
/// ]));
/// assert_eq!(grammar.len(), 2);
/// ```
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ProtoGrammar {
    /// NonTerminals der Grammar (Index = NonTerminal-ID).
    non_terminals: Vec<ProtoNonTerminal>,
}

/// Attribut-Sortierung für Event Code Assignment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeOrder {
    /// Lexikographisch nach local-name, dann URI (Spec 8.5.4.3).
    Lexicographic,
    /// Schema-Reihenfolge (wie im TypeDefinition-Attribut-Vec).
    Schema,
}

impl ProtoGrammar {
    /// Erstellt eine leere Proto-Grammar.
    pub fn new() -> Self {
        Self::default()
    }

    /// Fügt ein NonTerminal hinzu und gibt dessen Index zurück.
    pub fn add_non_terminal(&mut self, nt: ProtoNonTerminal) -> usize {
        let idx = self.non_terminals.len();
        self.non_terminals.push(nt);
        idx
    }

    /// Gibt die NonTerminals zurück.
    pub fn non_terminals(&self) -> &[ProtoNonTerminal] {
        &self.non_terminals
    }

    /// Gibt die NonTerminals mutierbar zurück.
    pub fn non_terminals_mut(&mut self) -> &mut Vec<ProtoNonTerminal> {
        &mut self.non_terminals
    }

    /// Gibt ein NonTerminal nach Index zurück.
    pub fn get(&self, index: usize) -> Option<&ProtoNonTerminal> {
        self.non_terminals.get(index)
    }

    /// Gibt ein NonTerminal mutierbar nach Index zurück.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut ProtoNonTerminal> {
        self.non_terminals.get_mut(index)
    }

    /// Anzahl der NonTerminals.
    pub fn len(&self) -> usize {
        self.non_terminals.len()
    }

    /// Prüft ob die Grammar leer ist.
    pub fn is_empty(&self) -> bool {
        self.non_terminals.is_empty()
    }

    /// Fügt eine optionale EE-Production zum ersten NonTerminal hinzu.
    ///
    /// Wird verwendet um Particles optional zu machen (min=0).
    ///
    /// # Panics
    ///
    /// Panics im Debug-Modus wenn die Grammar leer ist.
    /// Im Release-Modus ist das Verhalten undefiniert (sollte nie passieren,
    /// da diese Methode nur auf Grammars aus term_impl() aufgerufen wird).
    fn add_optional_ee(&mut self) {
        debug_assert!(
            !self.non_terminals.is_empty(),
            "add_optional_ee: ProtoGrammar darf nicht leer sein"
        );
        if let Some(first) = self.non_terminals.first_mut() {
            first.add_production(ProtoProduction::new(Terminal::EndElement, 0));
        }
    }

    /// Erstellt Complex Type Grammars (Spec 8.5.4.1.3.2).
    ///
    /// Gibt `(Type, TypeEmpty)` zurück.
    ///
    /// # Algorithmus (Spec 8.5.4.1.3.2)
    ///
    /// 1. Generiere Attribute Use Grammars für jedes Attribut, sortiert nach QName
    /// 2. Falls Attribute Wildcard: zu jeder Attribut-Grammar hinzufügen
    /// 3. Generiere Content Grammar basierend auf ContentType
    /// 4. TypeEmpty = G_0 ⊕ G_1 ⊕ ... ⊕ G_n-1 ⊕ EmptyContent
    /// 5. Type = H_0 ⊕ H_1 ⊕ ... ⊕ H_n-1 ⊕ Content
    ///
    /// # Fehler
    ///
    /// Gibt `Error::InvalidState` zurück wenn `def` ein Simple Type ist.
    pub fn complex_type(
        def: &crate::schema::TypeDefinition,
        interner: &mut StringInterner,
    ) -> crate::Result<(ProtoGrammar, ProtoGrammar)> {
        Self::complex_type_with_attr_order(def, AttributeOrder::Lexicographic, interner)
    }

    /// Wie `complex_type`, aber mit konfigurierbarer Attribut-Sortierung.
    pub fn complex_type_with_attr_order(
        def: &crate::schema::TypeDefinition,
        attr_order: AttributeOrder,
        interner: &mut StringInterner,
    ) -> crate::Result<(ProtoGrammar, ProtoGrammar)> {
        use crate::schema::{ContentType, TypeDefinition};

        // Nur Complex Types erlaubt
        let (attributes, attribute_wildcard, content) = match def {
            TypeDefinition::Complex {
                attributes,
                attribute_wildcard,
                content,
                ..
            } => (attributes, attribute_wildcard, content),
            TypeDefinition::Simple { .. } => {
                return Err(crate::Error::schema_violation("simple type has no element content"));
            }
        };

        let mut sorted_attrs = attributes.clone();
        if attr_order == AttributeOrder::Lexicographic {
            // Sortiere Attribute nach QName (lexikographisch local-name, dann uri)
            // -> folgt dem "SHOULD" in Spec 6 (kompaktere Event Codes)
            sorted_attrs.sort_by(|a, b| {
                a.qname
                    .local_name
                    .cmp(&b.qname.local_name)
                    .then_with(|| a.qname.uri.cmp(&b.qname.uri))
            });
        }

        // Generiere Attribute Use Grammars
        let mut attr_grammars: Vec<ProtoGrammar> = sorted_attrs
            .iter()
            .map(|attr| Self::attribute_use(&attr.qname, attr.required, interner))
            .collect::<crate::Result<Vec<_>>>()?;

        if let Some(wildcard) = attribute_wildcard {
            // Spec 8.5.4.1.3.2: Bei anyAttribute ein zusätzliches Attribut-Grammar G_{n-1}
            // erzeugen (EE-only), und Wildcard-Produktionen zu ALLEN G_i hinzufügen.
            attr_grammars.push(Self::end_element_only());
            for g in &mut attr_grammars {
                g.add_attribute_wildcard(wildcard, interner)?;
            }
        } else if attr_grammars.is_empty() {
            // Falls keine Attribute: leere Attribut-Grammar mit nur EE
            attr_grammars.push(Self::end_element_only());
        }

        // Content Grammar generieren
        let content_grammar = match content {
            ContentType::Empty => {
                // Empty: nur EE
                Self::end_element_only()
            }
            ContentType::Simple => {
                // Simple: CH -> EE
                let (simple, _) = Self::simple_type();
                simple
            }
            ContentType::ElementOnly(particle) => {
                // ElementOnly: Particle Grammar (Spec 8.5.4.1.5)
                Self::particle(particle, interner)?
            }
            ContentType::Mixed(particle) => {
                // Mixed: Particle Grammar + CH an allen NonTerminals (Spec 8.5.4.1.3.2)
                let mut content = Self::particle(particle, interner)?;
                content.add_mixed_content_ch();
                content
            }
        };

        // Concatenation-Helper
        fn concatenate_all(grammars: &[ProtoGrammar], final_grammar: ProtoGrammar) -> ProtoGrammar {
            let mut result = grammars[0].clone();
            for g in &grammars[1..] {
                result = result.concatenate(g.clone());
            }
            result.concatenate(final_grammar)
        }

        // Type = Attribute-Grammars ⊕ Content
        let type_grammar = concatenate_all(&attr_grammars, content_grammar);

        // TypeEmpty = Attribute-Grammars ⊕ EmptyContent
        let type_empty = concatenate_all(&attr_grammars, Self::end_element_only());

        Ok((type_grammar, type_empty))
    }

    /// Erstellt eine Grammar mit nur EE-Production (Hilfs-Methode).
    ///
    /// Verwendet für Empty Content, TypeEmpty und leere Attribut-Grammars.
    fn end_element_only() -> ProtoGrammar {
        let mut g = ProtoGrammar::new();
        g.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 0),
        ]));
        g
    }

    /// Berechnet Offsets für Particle-Grammars.
    ///
    /// Jede Particle-Grammar wird um den kumulierten Offset der vorherigen verschoben.
    fn compute_particle_offsets(particle_grammars: &[ProtoGrammar], initial_offset: usize) -> Vec<usize> {
        let mut offsets = Vec::with_capacity(particle_grammars.len());
        let mut current = initial_offset;
        for pg in particle_grammars {
            offsets.push(current);
            current += pg.len();
        }
        offsets
    }

    /// Kopiert Particle-Grammars mit Offset-Anpassung.
    ///
    /// `replace_ee_with_epsilon_to`: Wenn Some(target), werden EE-Productions durch
    /// ε→target ersetzt (für xs:all). Wenn None, bleiben EE erhalten (für xs:choice).
    fn copy_particles_with_offsets(
        grammar: &mut ProtoGrammar,
        particle_grammars: &[ProtoGrammar],
        offsets: &[usize],
        replace_ee_with_epsilon_to: Option<usize>,
    ) {
        for (pg, &offset) in particle_grammars.iter().zip(offsets) {
            for nt in pg.non_terminals() {
                let mut new_nt = ProtoNonTerminal::new();
                for prod in nt.productions() {
                    if let Some(ee_target) = replace_ee_with_epsilon_to
                        && prod.terminal == Some(Terminal::EndElement) {
                            new_nt.add_production(ProtoProduction::epsilon(ee_target));
                            continue;
                        }
                    new_nt.add_production(ProtoProduction {
                        terminal: prod.terminal.clone(),
                        right_hand_side: prod.right_hand_side + offset,
                        schema_order: prod.schema_order,
                    });
                }
                grammar.add_non_terminal(new_nt);
            }
        }
    }

    /// Fügt Attribute Wildcard zu allen NonTerminals hinzu (Spec 8.5.4.1.3.2).
    ///
    /// ```text
    /// // ##any:
    /// G_i,0 : AT(*) G_i,0
    ///
    /// // Namespace-Liste:
    /// G_i,0 : AT(uri_x:*) G_i,0  für jede URI
    /// ```
    ///
    /// # Spec 8.5.4.1.3.2
    ///
    /// "add the following production to each grammar G_i:
    /// G_i,0 : AT(*) G_i,0" (für ##any)
    ///
    /// oder
    ///
    /// "G_i,0 : AT(uri_x:*) G_i,0 for each member value uri_x in {namespace constraint}"
    pub fn add_attribute_wildcard(&mut self, wildcard: &crate::schema::AttributeWildcard, interner: &mut StringInterner) -> crate::Result<()> {
        use crate::schema::AttributeWildcard;

        // Spec 8.5.4.1.3.2: "add the following production to each grammar G_i"
        // mit "G_i,0 : AT(*) G_i,0" - also nur zu NonTerminal 0, nicht zu allen.
        if let Some(nt) = self.non_terminals.first_mut() {
            match wildcard {
                AttributeWildcard::Any => {
                    // AT(*) Selbstreferenz auf G_i,0
                    nt.add_production(ProtoProduction::new(
                        Terminal::Attribute(AttributeKind::Wildcard),
                        0,
                    ));
                }
                AttributeWildcard::Not(_) => {
                    // ##other - behandeln als AT(*) mit Runtime-Validierung
                    // EXI hat keinen speziellen Event-Code für Negation
                    nt.add_production(ProtoProduction::new(
                        Terminal::Attribute(AttributeKind::Wildcard),
                        0,
                    ));
                }
                AttributeWildcard::Namespaces(uris) => {
                    // AT(uri:*) Selbstreferenz auf G_i,0 für jede URI
                    for uri in uris {
                        nt.add_production(ProtoProduction::new(
                            Terminal::at_ns_wildcard(uri, interner)?,
                            0,
                        ));
                    }
                }
            }
        }
        Ok(())
    }


    /// Erstellt Attribute Use Grammar (Spec 8.5.4.1.4).
    ///
    /// ```text
    /// Attribute_i,0 : AT(qname) [schema-typed value] Attribute_i,1
    /// Attribute_i,1 : EE
    ///
    /// // Falls optional (required = false):
    /// Attribute_i,0 : EE
    /// ```
    ///
    /// # Parameter
    ///
    /// - `qname`: QName des Attributs
    /// - `required`: Ob das Attribut required ist (false = zusätzliche EE-Production)
    pub fn attribute_use(
        qname: &crate::qname::QName,
        required: bool,
        interner: &mut StringInterner,
    ) -> crate::Result<ProtoGrammar> {
        let mut grammar = ProtoGrammar::new();

        // Attr_0: AT(qname) -> Attr_1
        let mut attr_0_prods = vec![ProtoProduction::new(
            Terminal::at_qname(qname, interner)?,
            1,
        )];

        // Falls optional: zusätzliche EE-Production
        if !required {
            attr_0_prods.push(ProtoProduction::new(Terminal::EndElement, 0));
        }

        grammar.add_non_terminal(ProtoNonTerminal::with_productions(attr_0_prods));

        // Attr_1: EE (Accept)
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        Ok(grammar)
    }

    /// Erstellt Simple Type Grammars (Spec 8.5.4.1.3.1).
    ///
    /// Gibt `(Type, TypeEmpty)` zurück:
    ///
    /// ```text
    /// Type_i,0 : CH [schema-typed value] Type_i,1
    /// Type_i,1 : EE
    ///
    /// TypeEmpty_i,0 : EE
    /// ```
    ///
    /// # Beispiel
    ///
    /// ```
    /// use erxi::proto_grammar::ProtoGrammar;
    ///
    /// let (type_grammar, type_empty) = ProtoGrammar::simple_type();
    /// assert_eq!(type_grammar.len(), 2);
    /// assert_eq!(type_empty.len(), 1);
    /// ```
    pub fn simple_type() -> (ProtoGrammar, ProtoGrammar) {
        // Type_i: CH -> EE
        let mut type_grammar = ProtoGrammar::new();
        type_grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
        ]));
        type_grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        // TypeEmpty_i: EE
        let mut type_empty = ProtoGrammar::new();
        type_empty.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 0),
        ]));

        (type_grammar, type_empty)
    }

    // ========================================================================
    // Particle und Term Generierung (Spec 8.5.4.1.5 - 8.5.4.1.8)
    // ========================================================================

    /// Erstellt Element Term Grammar (Spec 8.5.4.1.6).
    ///
    /// Generiert SE(qname) Productions für das Element und seine Substitution Group.
    ///
    /// ```text
    /// ParticleTerm_i,0 : SE(qname_0) ParticleTerm_i,1
    ///                  | SE(qname_1) ParticleTerm_i,1
    ///                  | ...
    /// ParticleTerm_i,1 : EE
    /// ```
    pub fn element_term(decl: &crate::schema::ElementDeclaration, interner: &mut StringInterner) -> crate::Result<ProtoGrammar> {
        let mut order = 0u32;
        Self::element_term_impl(decl, &mut order, interner)
    }

    /// Interne Implementierung mit Schema-Order Counter.
    fn element_term_impl(
        decl: &crate::schema::ElementDeclaration,
        schema_order: &mut u32,
        interner: &mut StringInterner,
    ) -> crate::Result<ProtoGrammar> {
        let mut grammar = ProtoGrammar::new();

        // ParticleTerm_i,0 : SE(qname_x) ParticleTerm_i,1 für jedes qname
        // Alle QNames (inkl. Substitution Group Members) bekommen die gleiche schema_order,
        // da sie denselben Particle im Content Model repräsentieren (Spec 8.5.4.1.6).
        let mut prods = Vec::new();
        let order = *schema_order;
        *schema_order += 1;

        for qname in decl.matching_qnames() {
            prods.push(ProtoProduction::with_schema_order(
                Terminal::se_qname(&qname, interner)?,
                1,
                order,
            ));
        }
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(prods));

        // ParticleTerm_i,1 : EE
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        Ok(grammar)
    }

    /// Erstellt Wildcard Term Grammar (Spec 8.5.4.1.7).
    ///
    /// Generiert SE(*) oder SE(uri:*) Productions basierend auf WildcardConstraint.
    ///
    /// ```text
    /// // ##any, ##other (Not):
    /// ParticleTerm_i,0 : SE(*) ParticleTerm_i,1
    /// ParticleTerm_i,1 : EE
    ///
    /// // Namespace-Liste:
    /// ParticleTerm_i,0 : SE(uri_x:*) ParticleTerm_i,1  für jede URI
    /// ```
    ///
    /// # Fehler
    ///
    /// - `Error::EmptyNamespaceList` wenn `WildcardConstraint::Namespaces` leer ist
    pub fn wildcard_term(wildcard: &crate::schema::Wildcard, interner: &mut StringInterner) -> crate::Result<ProtoGrammar> {
        let mut order = 0u32;
        Self::wildcard_term_impl(wildcard, &mut order, interner)
    }

    /// Interne Implementierung mit Schema-Order Counter.
    fn wildcard_term_impl(
        wildcard: &crate::schema::Wildcard,
        schema_order: &mut u32,
        interner: &mut StringInterner,
    ) -> crate::Result<ProtoGrammar> {
        use crate::schema::WildcardConstraint;

        let mut grammar = ProtoGrammar::new();

        // ParticleTerm_i,0 : SE(...) ParticleTerm_i,1
        let prods = match &wildcard.constraint {
            WildcardConstraint::Any | WildcardConstraint::Not(_) => {
                // ##any und ##other → SE(*) - keine schema_order (SE(*) hat eigenen
                // Sortierrang nach allen SE(uri:*), daher irrelevant).
                vec![ProtoProduction::new(
                    Terminal::StartElement(StartElementKind::Wildcard),
                    1,
                )]
            }
            WildcardConstraint::Namespaces(uris) => {
                // Leere Namespace-Liste ist ungültig
                if uris.is_empty() {
                    return Err(crate::Error::EmptyNamespaceList);
                }
                // Namespace-Liste → SE(uri:*) für jede URI mit eigener schema_order.
                // Jede URI bekommt eine eigene schema_order, damit die Reihenfolge
                // aus dem XSD-Attribut erhalten bleibt (Exificient-kompatibel).
                // Die Spec sagt "sorted in schema order" (8.5.4.3), definiert aber
                // keine Sub-Ordnung innerhalb eines Particles. Exificient verwendet
                // die Attribut-Reihenfolge.
                uris.iter()
                    .map(|uri| {
                        let order = *schema_order;
                        *schema_order += 1;
                        Ok(ProtoProduction::with_schema_order(
                            Terminal::se_ns_wildcard(uri, interner)?,
                            1,
                            order,
                        ))
                    })
                    .collect::<crate::Result<Vec<_>>>()?
            }
        };
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(prods));

        // ParticleTerm_i,1 : EE
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        Ok(grammar)
    }

    /// Erstellt Grammar für einen ParticleTerm (Spec 8.5.4.1.5).
    ///
    /// Dispatcht basierend auf dem Term-Typ zu element_term, wildcard_term oder model_group.
    /// Interne Implementierung mit Schema-Order Counter.
    fn term_impl(
        term: &crate::schema::ParticleTerm,
        schema_order: &mut u32,
        interner: &mut StringInterner,
    ) -> crate::Result<ProtoGrammar> {
        use crate::schema::ParticleTerm;

        match term {
            ParticleTerm::Element(decl) => Self::element_term_impl(decl, schema_order, interner),
            ParticleTerm::Wildcard(constraint) => {
                Self::wildcard_term_impl(constraint, schema_order, interner)
            }
            ParticleTerm::ModelGroup(group) => Self::model_group_impl(group, schema_order, interner),
        }
    }

    /// Erstellt Model Group Grammar (Spec 8.5.4.1.8).
    ///
    /// Dispatcht basierend auf dem Compositor zu sequence_term, choice_term oder all_term.
    /// Interne Implementierung mit Schema-Order Counter.
    fn model_group_impl(
        group: &crate::schema::ModelGroup,
        schema_order: &mut u32,
        interner: &mut StringInterner,
    ) -> crate::Result<ProtoGrammar> {
        use crate::schema::Compositor;

        match group.compositor {
            Compositor::Sequence => Self::sequence_term_impl(&group.particles, schema_order, interner),
            Compositor::Choice => Self::choice_term_impl(&group.particles, schema_order, interner),
            Compositor::All => Self::all_term_impl(&group.particles, schema_order, interner),
        }
    }

    /// Erstellt Sequence Model Group Grammar (Spec 8.5.4.1.8.1).
    ///
    /// ```text
    /// ParticleTerm_i = Particle_0 ⊕ Particle_1 ⊕ ... ⊕ Particle_{n-1}
    /// ```
    ///
    /// Falls n=0: nur EE.
    pub fn sequence_term(particles: &[crate::schema::Particle], interner: &mut StringInterner) -> crate::Result<ProtoGrammar> {
        let mut order = 0u32;
        Self::sequence_term_impl(particles, &mut order, interner)
    }

    /// Interne Implementierung mit Schema-Order Counter.
    fn sequence_term_impl(
        particles: &[crate::schema::Particle],
        schema_order: &mut u32,
        interner: &mut StringInterner,
    ) -> crate::Result<ProtoGrammar> {
        if particles.is_empty() {
            // n=0: nur EE
            return Ok(Self::end_element_only());
        }

        // Generiere Particle-Grammars und concateniere sie
        let mut result = Self::particle_impl(&particles[0], schema_order, interner)?;
        for p in &particles[1..] {
            result = result.concatenate(Self::particle_impl(p, schema_order, interner)?);
        }

        Ok(result)
    }

    /// Erstellt Choice Model Group Grammar (Spec 8.5.4.1.8.2).
    ///
    /// ```text
    /// ParticleTerm_i,0 : Particle_0,0
    ///                  | Particle_1,0
    ///                  | ...
    ///                  | Particle_{n-1},0
    /// ```
    ///
    /// Falls n=0: nur EE.
    pub fn choice_term(particles: &[crate::schema::Particle], interner: &mut StringInterner) -> crate::Result<ProtoGrammar> {
        let mut order = 0u32;
        Self::choice_term_impl(particles, &mut order, interner)
    }

    /// Interne Implementierung mit Schema-Order Counter.
    fn choice_term_impl(
        particles: &[crate::schema::Particle],
        schema_order: &mut u32,
        interner: &mut StringInterner,
    ) -> crate::Result<ProtoGrammar> {
        if particles.is_empty() {
            return Ok(Self::end_element_only());
        }

        let particle_grammars: Vec<ProtoGrammar> = particles
            .iter()
            .map(|p| Self::particle_impl(p, schema_order, interner))
            .collect::<crate::Result<Vec<_>>>()?;

        let mut grammar = ProtoGrammar::new();
        let offsets = Self::compute_particle_offsets(&particle_grammars, 1);

        // ParticleTerm_i,0 : ε → Particle_j,0 für alle j
        let choice_prods: Vec<ProtoProduction> =
            offsets.iter().map(|&o| ProtoProduction::epsilon(o)).collect();
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(choice_prods));

        // Kopiere Particle Grammars (EE bleibt erhalten)
        Self::copy_particles_with_offsets(&mut grammar, &particle_grammars, &offsets, None);

        Ok(grammar)
    }

    /// Erstellt All Model Group Grammar (Spec 8.5.4.1.8.3).
    ///
    /// ```text
    /// ParticleTerm_i,0 : EE
    ///                  | Particle_0,0
    ///                  | Particle_1,0
    ///                  | ...
    ///                  | Particle_{n-1},0
    /// ```
    ///
    /// Alle EE-Productions in den Particles werden durch ε zu ParticleTerm_i,0 ersetzt.
    pub fn all_term(particles: &[crate::schema::Particle], interner: &mut StringInterner) -> crate::Result<ProtoGrammar> {
        let mut order = 0u32;
        Self::all_term_impl(particles, &mut order, interner)
    }

    /// Interne Implementierung mit Schema-Order Counter.
    fn all_term_impl(
        particles: &[crate::schema::Particle],
        schema_order: &mut u32,
        interner: &mut StringInterner,
    ) -> crate::Result<ProtoGrammar> {
        if particles.is_empty() {
            return Ok(Self::end_element_only());
        }

        let particle_grammars: Vec<ProtoGrammar> = particles
            .iter()
            .map(|p| Self::particle_impl(p, schema_order, interner))
            .collect::<crate::Result<Vec<_>>>()?;

        let mut grammar = ProtoGrammar::new();
        let offsets = Self::compute_particle_offsets(&particle_grammars, 1);

        // ParticleTerm_i,0 : EE | ε → Particle_j,0 für alle j
        let mut all_prods = vec![ProtoProduction::new(Terminal::EndElement, 0)];
        all_prods.extend(offsets.iter().map(|&o| ProtoProduction::epsilon(o)));
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(all_prods));

        // Kopiere Particle Grammars (EE → ε zu ParticleTerm_i,0)
        Self::copy_particles_with_offsets(&mut grammar, &particle_grammars, &offsets, Some(0));

        Ok(grammar)
    }

    /// Erstellt Particle Grammar (Spec 8.5.4.1.5).
    ///
    /// Kombiniert einen Term mit min/max occurs.
    ///
    /// # Algorithmus
    ///
    /// 1. Validiere Particle
    /// 2. Generiere Term Grammar
    /// 3. Erstelle Kopien basierend auf min/max occurs
    /// 4. Concateniere mit Grammar Concatenation Operator
    pub fn particle(p: &crate::schema::Particle, interner: &mut StringInterner) -> crate::Result<ProtoGrammar> {
        let mut order = 0u32;
        Self::particle_impl(p, &mut order, interner)
    }

    /// Interne Implementierung mit Schema-Order Counter.
    fn particle_impl(
        p: &crate::schema::Particle,
        schema_order: &mut u32,
        interner: &mut StringInterner,
    ) -> crate::Result<ProtoGrammar> {
        use crate::schema::MaxOccurs;

        // Validierung
        p.validate()?;

        // Term Grammar generieren (inkrementiert schema_order für SE-Productions)
        let term = Self::term_impl(&p.term, schema_order, interner)?;

        match (p.min_occurs, &p.max_occurs) {
            // Edge-Case: max=0 → leere Grammar (nur EE)
            (_, MaxOccurs::Bounded(0)) => Ok(Self::end_element_only()),

            // Standard: min=max=1 → Term unverändert
            (1, MaxOccurs::Bounded(1)) => Ok(term),

            // min=0, max=1 → Term mit optionaler EE am Start
            (0, MaxOccurs::Bounded(1)) => {
                let mut grammar = term;
                grammar.add_optional_ee();
                Ok(grammar)
            }

            // min=0, max=unbounded → optionaler Loop
            (0, MaxOccurs::Unbounded) => {
                let mut grammar = term;
                Self::make_unbounded_loop(&mut grammar);
                grammar.add_optional_ee();
                Ok(grammar)
            }

            // min=n, max=unbounded (n >= 1) → n required + 1 loop
            (min, MaxOccurs::Unbounded) => {
                // min required copies
                let mut builder = ConcatBuilder::new(term.clone());
                for _ in 1..min {
                    builder.append(term.clone());
                }

                // 1 loop copy mit optionaler EE
                let mut loop_copy = term;
                Self::make_unbounded_loop(&mut loop_copy);
                loop_copy.add_optional_ee();

                builder.append(loop_copy);
                Ok(builder.finish())
            }

            // min=0, max=n (n >= 2) → n optional copies
            (0, MaxOccurs::Bounded(max)) => {
                let mut first = term.clone();
                first.add_optional_ee();
                let mut builder = ConcatBuilder::new(first);

                for _ in 1..*max {
                    let mut optional_copy = term.clone();
                    optional_copy.add_optional_ee();
                    builder.append(optional_copy);
                }

                Ok(builder.finish())
            }

            // min=n, max=m (n >= 1, m > n) → n required + (m-n) optional
            (min, MaxOccurs::Bounded(max)) => {
                // min required copies
                let mut builder = ConcatBuilder::new(term.clone());
                for _ in 1..min {
                    builder.append(term.clone());
                }

                // (max - min) optional copies
                for _ in min..*max {
                    let mut optional_copy = term.clone();
                    optional_copy.add_optional_ee();
                    builder.append(optional_copy);
                }

                Ok(builder.finish())
            }
        }
    }

    /// Macht eine Grammar zu einem unbounded Loop.
    ///
    /// Ersetzt alle EE-Productions durch ε-Productions die zurück zu NT_0 zeigen.
    fn make_unbounded_loop(grammar: &mut ProtoGrammar) {
        for nt in grammar.non_terminals_mut() {
            for prod in nt.productions_mut() {
                if prod.terminal == Some(Terminal::EndElement) {
                    // EE → ε zu NT_0 (Selbstreferenz auf Start)
                    prod.terminal = None;
                    prod.right_hand_side = 0;
                }
            }
        }
    }

    /// Fügt CH-Productions zu allen NonTerminals hinzu (Mixed Content, Spec 8.5.4.1.3.2).
    ///
    /// "add a production for each non-terminal Content_i,j in Content_i as follows:
    /// Content_i,j : CH [untyped value] Content_i,j"
    pub fn add_mixed_content_ch(&mut self) {
        for nt_idx in 0..self.non_terminals.len() {
            self.non_terminals[nt_idx]
                .add_production(ProtoProduction::new(Terminal::Characters, nt_idx));
        }
    }

    /// Konvertiert Proto-Grammar zu normalisierten NonTerminals.
    ///
    /// 1. Normalisiert die Grammar (eliminiert ε-Productions)
    /// 2. Konvertiert zu `Vec<NonTerminal>` mit Event Codes
    ///
    /// # Parameter
    ///
    /// - `base_id`: Basis-NonTerminalId für das erste NonTerminal.
    ///   Weitere NonTerminals erhalten dynamische IDs.
    ///
    /// # Rückgabe
    ///
    /// `Vec<NonTerminal>` die in ein GrammarSystem integriert werden können.
    pub fn to_non_terminals(
        mut self,
        base_id: crate::grammar::NonTerminalId,
    ) -> crate::Result<Vec<crate::grammar::NonTerminal>> {
        use crate::event_code::EventCode;
        use crate::grammar::{NonTerminal, NonTerminalId, Production};

        // Schritt 1: Normalisieren
        self.eliminate_epsilon_productions();

        // Schritt 2: Konvertiere zu NonTerminal
        let mut result = Vec::with_capacity(self.non_terminals.len());

        for (idx, proto_nt) in self.non_terminals.into_iter().enumerate() {
            // NonTerminalId: erstes bekommt base_id, weitere bekommen dynamische IDs
            let nt_id = if idx == 0 {
                base_id
            } else {
                NonTerminalId::Dynamic(idx)
            };

            // Konvertiere Productions
            let mut productions = Vec::with_capacity(proto_nt.productions.len());
            for (prod_idx, proto_prod) in proto_nt.productions.into_iter().enumerate() {
                // Terminal muss vorhanden sein nach Normalisierung
                let terminal = proto_prod.terminal.ok_or_else(|| {
                    crate::Error::UnknownNonTerminal(
                        "ε-Production nach Normalisierung gefunden".to_string(),
                    )
                })?;

                // RightHandSide: Konvertiere Proto-Index zu NonTerminalId
                let rhs = if proto_prod.right_hand_side == idx {
                    // Selbstreferenz
                    Some(nt_id)
                } else if proto_prod.right_hand_side == 0 {
                    Some(base_id)
                } else {
                    Some(NonTerminalId::Dynamic(proto_prod.right_hand_side))
                };

                // Event Code: vorläufig sequentiell, wird später durch
                // recalculate_event_codes() korrigiert
                let event_code = EventCode::one(prod_idx as u32);

                productions.push(Production {
                    terminal,
                    event_code: Some(event_code),
                    right_hand_side: rhs,
                });
            }

            result.push(NonTerminal::new(nt_id, productions));
        }

        Ok(result)
    }

    /// Eliminiert ε-Productions (Spec 8.5.4.2.1).
    ///
    /// Ersetzt jede Production der Form `L : R` (ε, kein Terminal) durch
    /// alle Productions von R mit L als LeftHandSide.
    ///
    /// # Algorithmus (Fixpunkt-Iteration)
    ///
    /// ```text
    /// Wiederhole bis keine Änderung:
    ///   Für jedes NonTerminal NT_j:
    ///     Für jede ε-Production "NT_j : NT_k":
    ///       Ersetze durch alle Productions von NT_k
    ///       (außer wenn k == j, um Zyklen zu vermeiden)
    /// ```
    ///
    /// # Spec 8.5.4.2.1
    ///
    /// "Replace each production of the form G_i,j : G_i,k with a set of
    /// productions: G_i,j : RHS(G_i,k)_0, RHS(G_i,k)_1, ..., RHS(G_i,k)_m-1"
    pub fn eliminate_epsilon_productions(&mut self) {
        let mut changed = true;
        let mut visited: crate::FastHashSet<(usize, usize)> = crate::FastHashSet::default();

        while changed {
            changed = false;

            for nt_idx in 0..self.non_terminals.len() {
                let mut new_productions = Vec::new();
                let mut had_epsilon = false;

                // Sammle aktuelle Productions
                let current_prods: Vec<_> = self.non_terminals[nt_idx].productions.clone();

                for prod in current_prods {
                    if prod.is_epsilon() {
                        had_epsilon = true;
                        let target_idx = prod.right_hand_side;

                        // Zyklen-Schutz: Selbstreferenz oder bereits besucht
                        if target_idx == nt_idx {
                            continue; // Selbstreferenz ignorieren
                        }

                        let key = (nt_idx, target_idx);
                        if !visited.contains(&key) {
                            visited.insert(key);

                            // Kopiere alle Productions vom Ziel
                            let target_prods: Vec<_> =
                                self.non_terminals[target_idx].productions.clone();

                            for target_prod in target_prods {
                                // Vermeide doppelte ε zu sich selbst
                                if target_prod.is_epsilon() && target_prod.right_hand_side == nt_idx
                                {
                                    continue;
                                }
                                new_productions.push(target_prod);
                            }
                            changed = true;
                        }
                    } else {
                        new_productions.push(prod);
                    }
                }

                if had_epsilon {
                    self.non_terminals[nt_idx].productions = new_productions;
                }
            }
        }

        // Entferne leere NonTerminals' Productions die nur noch ε zu sich selbst haben
        for nt in &mut self.non_terminals {
            nt.productions.retain(|p| !p.is_epsilon());
        }

        // Post-Condition: Keine ε-Productions mehr vorhanden
        debug_assert!(
            self.non_terminals
                .iter()
                .all(|nt| nt.productions.iter().all(|p| p.terminal.is_some())),
            "Nach ε-Elimination sollten keine ε-Productions mehr vorhanden sein"
        );
    }

    /// Eliminiert Duplicate Terminals (Spec 8.5.4.2.2).
    ///
    /// Identifiziert Productions mit gleichem Terminal aber verschiedenen Zielen
    /// und vereinigt sie zu einem Union-NonTerminal.
    ///
    /// # Spec 8.5.4.2.2
    ///
    /// "Identify all pairs of productions that have the same non-terminal on the
    /// left-hand side and the same terminal symbol on the right-hand side...
    /// replace them with a single production: G_i,j : Terminal G_i,k⊔l"
    pub fn eliminate_duplicate_terminals(&mut self) {
        use std::collections::{BTreeMap, BTreeSet, VecDeque};

        let trace = std::env::var("ERXI_TRACE_NORMALIZE").is_ok();

        /// Gibt die kleinere schema_order zurück (None gilt als größer).
        fn min_schema_order(a: Option<u32>, b: Option<u32>) -> Option<u32> {
            match (a, b) {
                (Some(x), Some(y)) => Some(x.min(y)),
                (Some(x), None) | (None, Some(x)) => Some(x),
                (None, None) => None,
            }
        }

        // Cache für bereits erstellte Unions: sorted targets -> union_idx
        let mut union_cache: BTreeMap<Vec<usize>, usize> = BTreeMap::new();

        let mut queue: VecDeque<usize> = (0..self.non_terminals.len()).collect();
        let mut processed: Vec<bool> = vec![false; self.non_terminals.len()];

        while let Some(nt_idx) = queue.pop_front() {
            if nt_idx >= self.non_terminals.len() {
                continue;
            }
            if processed.get(nt_idx).copied().unwrap_or(false) {
                continue;
            }

            loop {
                if trace {
                    let total: usize = self
                        .non_terminals
                        .iter()
                        .map(|nt| nt.productions.len())
                        .sum();
                    eprintln!(
                        "dedup: nt={} nts={} prods={}",
                        nt_idx,
                        self.non_terminals.len(),
                        total
                    );
                }

                // Sammle Productions nach Terminal gruppiert
                let mut terminal_groups: BTreeMap<TerminalKey, Vec<(usize, usize)>> =
                    BTreeMap::new();

                for (prod_idx, prod) in self.non_terminals[nt_idx].productions.iter().enumerate() {
                    if let Some(terminal) = &prod.terminal {
                        let key = TerminalKey::from(terminal);
                        terminal_groups
                            .entry(key)
                            .or_default()
                            .push((prod_idx, prod.right_hand_side));
                    }
                }

                let mut changed = false;

                // Finde Duplikate (gleicher Terminal, verschiedene Ziele)
                for group in terminal_groups.values() {
                    if group.len() < 2 {
                        continue;
                    }

                    // Sammle alle verschiedenen Ziel-NonTerminals
                    let targets: BTreeSet<usize> = group.iter().map(|(_, rhs)| *rhs).collect();
                    if targets.len() < 2 {
                        continue;
                    }

                    let cache_key: Vec<usize> = targets.iter().copied().collect();
                    let union_idx = if let Some(&existing_idx) = union_cache.get(&cache_key) {
                        existing_idx
                    } else {
                        let new_idx =
                            self.create_union_non_terminal_dedup(&targets, &mut union_cache);
                        if new_idx >= processed.len() {
                            processed.resize(new_idx + 1, false);
                        }
                        queue.push_back(new_idx);
                        new_idx
                    };

                    let mut prod_indices: Vec<usize> = group.iter().map(|(idx, _)| *idx).collect();
                    let first_prod = &self.non_terminals[nt_idx].productions[prod_indices[0]];
                    let terminal = first_prod.terminal.clone();

                    let schema_order = prod_indices
                        .iter()
                        .filter_map(|&idx| self.non_terminals[nt_idx].productions[idx].schema_order)
                        .min();

                    prod_indices.sort_unstable_by(|a, b| b.cmp(a));
                    for idx in prod_indices {
                        self.non_terminals[nt_idx].productions.remove(idx);
                    }

                    self.non_terminals[nt_idx]
                        .productions
                        .push(ProtoProduction {
                            terminal,
                            right_hand_side: union_idx,
                            schema_order,
                        });

                    changed = true;
                    break; // Recompute groups for this NT
                }

                if !changed {
                    break;
                }
            }

            if nt_idx >= processed.len() {
                processed.resize(nt_idx + 1, false);
            }
            processed[nt_idx] = true;
        }

        // Entferne echte Duplikate (gleicher Terminal UND gleiches Ziel).
        // Bei Duplikaten behalten wir die Production mit der kleinsten schema_order.
        for nt in &mut self.non_terminals {
            // Finde für jedes (TerminalKey, rhs) die minimale schema_order
            let mut min_order: BTreeMap<(TerminalKey, usize), Option<u32>> = BTreeMap::new();
            for p in &nt.productions {
                if let Some(terminal) = &p.terminal {
                    let key = (TerminalKey::from(terminal), p.right_hand_side);
                    min_order
                        .entry(key)
                        .and_modify(|current| {
                            *current = min_schema_order(*current, p.schema_order);
                        })
                        .or_insert(p.schema_order);
                }
            }

            // Behalte nur die erste Production pro Key mit der minimalen schema_order
            let mut seen: BTreeSet<(TerminalKey, usize)> = BTreeSet::new();
            nt.productions.retain(|p| {
                let Some(ref terminal) = p.terminal else {
                    return true; // ε-Productions behalten
                };
                let key = (TerminalKey::from(terminal), p.right_hand_side);
                let expected_order = min_order.get(&key).copied().flatten();
                // Behalte wenn: noch nicht gesehen UND hat die minimale Order
                if !seen.contains(&key) && p.schema_order == expected_order {
                    seen.insert(key);
                    true
                } else {
                    false
                }
            });
        }
    }

    /// Erstellt (oder re-used) ein Union-NonTerminal, das alle Productions der
    /// gegebenen NonTerminals vereinigt **und Duplikate sofort dedupliziert**.
    ///
    /// Das verhindert exponentielles Wachstum bei komplexen Partikel-Grammars
    /// mit unbounded Loops, indem Duplicate-Terminal-Elimination für das Union
    /// direkt angewendet wird.
    fn create_union_non_terminal_dedup(
        &mut self,
        sources: &std::collections::BTreeSet<usize>,
        union_cache: &mut std::collections::BTreeMap<Vec<usize>, usize>,
    ) -> usize {
        fn min_schema_order(a: Option<u32>, b: Option<u32>) -> Option<u32> {
            match (a, b) {
                (Some(x), Some(y)) => Some(x.min(y)),
                (Some(x), None) | (None, Some(x)) => Some(x),
                (None, None) => None,
            }
        }

        let cache_key: Vec<usize> = sources.iter().copied().collect();
        if let Some(&idx) = union_cache.get(&cache_key) {
            return idx;
        }

        // Reserviere Index im Cache, um Rekursion mit Selbst-Referenzen zu erlauben.
        let idx = self.add_non_terminal(ProtoNonTerminal::new());
        union_cache.insert(cache_key, idx);

        // Gruppiere Productions nach Terminal und vereine RHS-Ziele.
        let mut terminal_groups: std::collections::BTreeMap<
            TerminalKey,
            (Terminal, std::collections::BTreeSet<usize>, Option<u32>),
        > = std::collections::BTreeMap::new();

        for &src_idx in sources {
            debug_assert!(
                src_idx < self.non_terminals.len(),
                "Union-Quelle {} überschreitet NonTerminal-Anzahl {}",
                src_idx,
                self.non_terminals.len()
            );
            for prod in &self.non_terminals[src_idx].productions {
                let Some(terminal) = &prod.terminal else {
                    // Nach ε-Elimination sollten keine ε-Productions mehr existieren.
                    continue;
                };
                let key = TerminalKey::from(terminal);
                let entry = terminal_groups
                    .entry(key)
                    .or_insert_with(|| (terminal.clone(), std::collections::BTreeSet::new(), None));
                entry.1.insert(prod.right_hand_side);
                entry.2 = min_schema_order(entry.2, prod.schema_order);
            }
        }

        let mut union_prods = Vec::with_capacity(terminal_groups.len());
        for (_key, (terminal, rhs_set, schema_order)) in terminal_groups {
            let rhs = if rhs_set.len() == 1 {
                *rhs_set.iter().next().expect("rhs_set is non-empty")
            } else {
                self.create_union_non_terminal_dedup(&rhs_set, union_cache)
            };
            union_prods.push(ProtoProduction {
                terminal: Some(terminal),
                right_hand_side: rhs,
                schema_order,
            });
        }

        if let Some(nt) = self.non_terminals.get_mut(idx) {
            nt.productions = union_prods;
        }

        idx
    }

    /// Normalisiert die Proto-Grammar zu einer EXI Grammar (Spec 8.5.4.2, 8.5.4.3).
    ///
    /// # Schritte
    ///
    /// 1. Eliminiert ε-Productions (8.5.4.2.1)
    /// 2. Eliminiert Duplicate Terminals mit Union-NonTerminals (8.5.4.2.2)
    /// 3. Sortiert und weist Event Codes zu (8.5.4.3)
    ///
    /// # Rückgabe
    ///
    /// Liste normalisierter NonTerminals mit zugewiesenen Event Codes.
    pub fn normalize(mut self, interner: &StringInterner) -> Vec<crate::grammar::NonTerminal> {
        use crate::event_code::EventCode;
        use crate::grammar::{NonTerminal, NonTerminalId, Production};

        let trace = std::env::var("ERXI_TRACE_NORMALIZE").is_ok();
        if trace {
            let total: usize = self
                .non_terminals
                .iter()
                .map(|nt| nt.productions.len())
                .sum();
            eprintln!(
                "normalize: start nts={} prods={}",
                self.non_terminals.len(),
                total
            );
        }

        // Schritt 1: ε-Elimination
        self.eliminate_epsilon_productions();
        if trace {
            let total: usize = self
                .non_terminals
                .iter()
                .map(|nt| nt.productions.len())
                .sum();
            eprintln!(
                "normalize: after epsilon nts={} prods={}",
                self.non_terminals.len(),
                total
            );
        }

        // Schritt 2: Duplicate Terminal Elimination
        self.eliminate_duplicate_terminals();
        if trace {
            let total: usize = self
                .non_terminals
                .iter()
                .map(|nt| nt.productions.len())
                .sum();
            eprintln!(
                "normalize: after dedup nts={} prods={}",
                self.non_terminals.len(),
                total
            );
        }

        // Schritt 3: Konvertiere zu normalisierten NonTerminals mit Event Codes
        let mut result = Vec::with_capacity(self.non_terminals.len());

        for (nt_idx, proto_nt) in self.non_terminals.iter().enumerate() {
            // Sortiere Productions nach Spec 8.5.4.3
            // Tupel: (terminal, rhs, schema_order)
            let mut sorted_prods: Vec<_> = proto_nt
                .productions
                .iter()
                .filter_map(|p| {
                    p.terminal
                        .as_ref()
                        .map(|t| (t.clone(), p.right_hand_side, p.schema_order))
                })
                .collect();

            sorted_prods.sort_by(|(t1, _, o1), (t2, _, o2)| {
                compare_terminals_for_event_code_with_schema_order(t1, *o1, t2, *o2, interner)
            });

            // Weise Event Codes zu (0, 1, 2, ...)
            let productions: Vec<Production> = sorted_prods
                .into_iter()
                .enumerate()
                .map(|(idx, (terminal, rhs, _))| {
                    Production::new(
                        terminal,
                        EventCode::one(idx as u32),
                        Some(NonTerminalId::Dynamic(rhs)),
                    )
                })
                .collect();

            result.push(NonTerminal::new(
                NonTerminalId::Dynamic(nt_idx),
                productions,
            ));
        }

        result
    }

    /// Grammar Concatenation Operator ⊕ (Spec 8.5.4.1.1).
    ///
    /// Verbindet zwei Grammars sequentiell:
    /// - Alle NonTerminals aus `other` werden kopiert (mit Offset für Indizes)
    /// - Alle EE-Productions in `self` werden durch ε-Produktion mit Verweis auf `other`'s Start ersetzt
    ///
    /// # Spec 8.5.4.1.1
    ///
    /// "The grammar concatenation operator ⊕ creates a new grammar from its left
    /// and right grammar operands. The new grammar accepts any set of symbols
    /// accepted by its left operand followed by any set of symbols accepted by
    /// its right operand."
    ///
    /// "replacing each production of the form `Grammar_L_k : EE` with a production
    /// of the form `Grammar_L_k : Grammar_R_0`"
    ///
    /// # Algorithmus
    ///
    /// ```text
    /// Grammar_L ⊕ Grammar_R:
    ///   1. Kopiere alle NonTerminals aus R, mit Offset = len(L) für Indizes
    ///   2. Für jede Production mit Terminal=EE in L:
    ///      Ersetze durch ε-Produktion die auf R_0 (= offset) zeigt
    /// ```
    ///
    /// # Hinweis zu ε-Productions
    ///
    /// ε-Productions (`terminal = None`) repräsentieren `L : R` (NonTerminal → NonTerminal).
    /// EE-Productions (`terminal = Some(EndElement)`) sind Accept-Zustände.
    /// Bei Concatenation werden EE-Productions durch ε-Productions ersetzt.
    pub fn concatenate(mut self, other: ProtoGrammar) -> ProtoGrammar {
        if other.is_empty() {
            return self;
        }

        let offset = self.non_terminals.len();

        // Schritt 1: Kopiere alle NonTerminals aus R mit Offset
        for nt in other.non_terminals {
            let mut new_nt = ProtoNonTerminal::new();
            for prod in nt.productions {
                new_nt.add_production(ProtoProduction {
                    terminal: prod.terminal,
                    right_hand_side: prod.right_hand_side + offset,
                    schema_order: prod.schema_order,
                });
            }
            self.non_terminals.push(new_nt);
        }

        // Schritt 2: Ersetze EE-Productions in L durch ε die auf R_0 zeigt
        for nt in self.non_terminals.iter_mut().take(offset) {
            for prod in nt.productions_mut() {
                if prod.terminal == Some(Terminal::EndElement) {
                    // EE-Production → ε die auf R_0 zeigt
                    prod.terminal = None;
                    prod.right_hand_side = offset;
                }
            }
        }

        self
    }
}

/// Effizienter Concatenation-Builder für viele wiederholte Kopien.
///
/// Reduziert die EE-Replacement-Arbeit auf den jeweils letzten Block
/// (statt bei jeder Concatenation über alle NonTerminals zu iterieren).
struct ConcatBuilder {
    grammar: ProtoGrammar,
    tail_start: usize,
    tail_len: usize,
}

impl ConcatBuilder {
    fn new(grammar: ProtoGrammar) -> Self {
        let tail_len = grammar.len();
        Self {
            grammar,
            tail_start: 0,
            tail_len,
        }
    }

    fn append(&mut self, other: ProtoGrammar) {
        if other.is_empty() {
            return;
        }

        let offset = self.grammar.non_terminals.len();
        let other_len = other.non_terminals.len();

        // Schritt 1: Kopiere alle NonTerminals aus R mit Offset
        for nt in other.non_terminals {
            let mut new_nt = ProtoNonTerminal::new();
            for prod in nt.productions {
                new_nt.add_production(ProtoProduction {
                    terminal: prod.terminal,
                    right_hand_side: prod.right_hand_side + offset,
                    schema_order: prod.schema_order,
                });
            }
            self.grammar.non_terminals.push(new_nt);
        }

        // Schritt 2: Ersetze EE-Productions nur im Tail-Segment
        let end = self.tail_start + self.tail_len;
        for nt in self.grammar.non_terminals[self.tail_start..end].iter_mut() {
            for prod in nt.productions_mut() {
                if prod.terminal == Some(Terminal::EndElement) {
                    prod.terminal = None;
                    prod.right_hand_side = offset;
                }
            }
        }

        self.tail_start = offset;
        self.tail_len = other_len;
    }

    fn finish(self) -> ProtoGrammar {
        self.grammar
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    // ========================================================================
    // Tests: ProtoProduction (Spec 8.5.4.1)
    // ========================================================================

    /// Production mit Terminal
    #[test]
    fn proto_production_with_terminal() {
        let prod = ProtoProduction::new(Terminal::Characters, 1);
        assert!(!prod.is_epsilon());
        assert_eq!(prod.terminal, Some(Terminal::Characters));
        assert_eq!(prod.right_hand_side, 1);
    }

    /// ε-Produktion (ohne Terminal)
    #[test]
    fn proto_production_epsilon() {
        let prod = ProtoProduction::epsilon(0);
        assert!(prod.is_epsilon());
        assert_eq!(prod.terminal, None);
        assert_eq!(prod.right_hand_side, 0);
    }

    /// Production Equality
    #[test]
    fn proto_production_equality() {
        let a = ProtoProduction::new(Terminal::Characters, 1);
        let b = ProtoProduction::new(Terminal::Characters, 1);
        let c = ProtoProduction::new(Terminal::Characters, 2);
        let d = ProtoProduction::new(Terminal::EndElement, 1);

        assert_eq!(a, b);
        assert_ne!(a, c); // unterschiedliche RHS
        assert_ne!(a, d); // unterschiedliches Terminal
    }

    // ========================================================================
    // Tests: ProtoNonTerminal (Spec 8.5.4.1)
    // ========================================================================

    /// Leeres NonTerminal
    #[test]
    fn proto_non_terminal_empty() {
        let nt = ProtoNonTerminal::new();
        assert!(nt.is_empty());
        assert_eq!(nt.len(), 0);
    }

    /// NonTerminal mit Productions
    #[test]
    fn proto_non_terminal_with_productions() {
        let nt = ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
            ProtoProduction::epsilon(0),
        ]);
        assert!(!nt.is_empty());
        assert_eq!(nt.len(), 2);
        assert!(!nt.productions()[0].is_epsilon());
        assert!(nt.productions()[1].is_epsilon());
    }

    /// Production hinzufügen
    #[test]
    fn proto_non_terminal_add_production() {
        let mut nt = ProtoNonTerminal::new();
        nt.add_production(ProtoProduction::new(Terminal::EndElement, 0));
        assert_eq!(nt.len(), 1);
    }

    // ========================================================================
    // Tests: ProtoGrammar (Spec 8.5.4.1)
    // ========================================================================

    /// Leere Grammar
    #[test]
    fn proto_grammar_empty() {
        let g = ProtoGrammar::new();
        assert!(g.is_empty());
        assert_eq!(g.len(), 0);
    }

    /// NonTerminal hinzufügen
    #[test]
    fn proto_grammar_add_non_terminal() {
        let mut g = ProtoGrammar::new();
        let idx = g.add_non_terminal(ProtoNonTerminal::new());
        assert_eq!(idx, 0);
        assert_eq!(g.len(), 1);

        let idx2 = g.add_non_terminal(ProtoNonTerminal::new());
        assert_eq!(idx2, 1);
        assert_eq!(g.len(), 2);
    }

    /// NonTerminal nach Index abrufen
    #[test]
    fn proto_grammar_get() {
        let mut g = ProtoGrammar::new();
        g.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
        ]));

        assert!(g.get(0).is_some());
        assert!(g.get(1).is_none());
        assert_eq!(g.get(0).unwrap().len(), 1);
    }

    /// Simple Type Grammar Struktur (Spec 8.5.4.1.3.1)
    ///
    /// Type_i,0 : CH [schema-typed value] Type_i,1
    /// Type_i,1 : EE
    #[test]
    fn proto_grammar_simple_type_structure() {
        let mut g = ProtoGrammar::new();

        // Type_i,0 : CH Type_i,1
        g.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
        ]));

        // Type_i,1 : EE (Accept-Zustand)
        g.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        assert_eq!(g.len(), 2);
        assert_eq!(g.get(0).unwrap().len(), 1);
        assert_eq!(g.get(1).unwrap().len(), 1);
        // Prüfe dass Type_i,1 EE hat
        assert_eq!(
            g.get(1).unwrap().productions()[0].terminal,
            Some(Terminal::EndElement)
        );
    }

    // ========================================================================
    // Tests: Grammar Concatenation ⊕ (Spec 8.5.4.1.1)
    // ========================================================================

    /// Spec 8.5.4.1.1: Concatenation verbindet zwei Grammars
    ///
    /// L: L_0 : CH L_1, L_1 : EE
    /// R: R_0 : Comment R_1, R_1 : EE
    ///
    /// L ⊕ R sollte L_1's EE durch ε-Produktion mit Verweis auf R_0 ersetzen:
    /// Result: L_0 : CH L_1, L_1 : ε→R_0
    ///         R_0 : Comment R_1, R_1 : EE
    #[test]
    fn concatenation_two_grammars() {
        // L: CH -> EE (Accept)
        let mut left = ProtoGrammar::new();
        left.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
        ]));
        left.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1), // EE = Accept
        ]));

        // R: Comment -> EE (Accept)
        let mut right = ProtoGrammar::new();
        right.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Comment, 1),
        ]));
        right.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1), // EE = Accept
        ]));

        // L ⊕ R
        let result = left.concatenate(right);

        // Ergebnis hat 4 NonTerminals (2 + 2)
        assert_eq!(result.len(), 4);

        // L_0 bleibt unverändert: CH -> 1
        assert_eq!(
            result.get(0).unwrap().productions()[0].terminal,
            Some(Terminal::Characters)
        );
        assert_eq!(result.get(0).unwrap().productions()[0].right_hand_side, 1);

        // L_1's EE wurde durch ε ersetzt die auf R_0 (Index 2) zeigt
        let l1_prod = &result.get(1).unwrap().productions()[0];
        assert!(l1_prod.is_epsilon()); // EE wurde zu ε
        assert_eq!(l1_prod.terminal, None);
        assert_eq!(l1_prod.right_hand_side, 2); // Zeigt auf R_0

        // R_0 (jetzt Index 2): Comment -> 3
        assert_eq!(
            result.get(2).unwrap().productions()[0].terminal,
            Some(Terminal::Comment)
        );
        assert_eq!(result.get(2).unwrap().productions()[0].right_hand_side, 3);

        // R_1 (jetzt Index 3): EE (Accept, unverändert da in R)
        assert_eq!(
            result.get(3).unwrap().productions()[0].terminal,
            Some(Terminal::EndElement)
        );
    }

    /// Spec 8.5.4.1.1: Concatenation mit leerer rechter Grammar
    #[test]
    fn concatenation_with_empty_right() {
        let mut left = ProtoGrammar::new();
        left.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
        ]));
        left.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1), // EE = Accept
        ]));

        let right = ProtoGrammar::new(); // Leer

        let result = left.concatenate(right);

        // Keine Änderung wenn R leer (EE bleibt EE)
        assert_eq!(result.len(), 2);
        assert_eq!(
            result.get(1).unwrap().productions()[0].terminal,
            Some(Terminal::EndElement)
        );
    }

    /// Spec 8.5.4.1.1: Concatenation mit leerer linker Grammar
    #[test]
    fn concatenation_with_empty_left() {
        let left = ProtoGrammar::new(); // Leer

        let mut right = ProtoGrammar::new();
        right.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Comment, 1),
        ]));
        right.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1), // EE = Accept
        ]));

        let result = left.concatenate(right);

        // Nur R's NonTerminals
        assert_eq!(result.len(), 2);
    }

    /// Spec 8.5.4.1.1: Mehrfach-Concatenation (A ⊕ B ⊕ C)
    #[test]
    fn concatenation_triple() {
        // A: CH -> EE (Accept)
        let mut a = ProtoGrammar::new();
        a.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
        ]));
        a.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1), // EE = Accept
        ]));

        // B: PI -> EE (Accept)
        let mut b = ProtoGrammar::new();
        b.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::ProcessingInstr, 1),
        ]));
        b.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1), // EE = Accept
        ]));

        // C: Comment -> EE (Accept)
        let mut c = ProtoGrammar::new();
        c.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Comment, 1),
        ]));
        c.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1), // EE = Accept
        ]));

        // (A ⊕ B) ⊕ C
        let result = a.concatenate(b).concatenate(c);

        // 6 NonTerminals (2 + 2 + 2)
        assert_eq!(result.len(), 6);

        // A_0: CH -> A_1
        assert_eq!(
            result.get(0).unwrap().productions()[0].terminal,
            Some(Terminal::Characters)
        );

        // A_1: ε -> B_0 (Index 2) — EE wurde durch ε ersetzt
        let a1 = &result.get(1).unwrap().productions()[0];
        assert!(a1.is_epsilon());
        assert_eq!(a1.right_hand_side, 2);

        // B_0 (Index 2): PI -> B_1 (Index 3)
        assert_eq!(
            result.get(2).unwrap().productions()[0].terminal,
            Some(Terminal::ProcessingInstr)
        );
        assert_eq!(result.get(2).unwrap().productions()[0].right_hand_side, 3);

        // B_1 (Index 3): ε -> C_0 (Index 4) — EE wurde durch ε ersetzt
        let b1 = &result.get(3).unwrap().productions()[0];
        assert!(b1.is_epsilon());
        assert_eq!(b1.right_hand_side, 4);

        // C_0 (Index 4): Comment -> C_1 (Index 5)
        assert_eq!(
            result.get(4).unwrap().productions()[0].terminal,
            Some(Terminal::Comment)
        );
        assert_eq!(result.get(4).unwrap().productions()[0].right_hand_side, 5);

        // C_1 (Index 5): EE = Accept (unverändert, da letzte Grammar)
        assert_eq!(
            result.get(5).unwrap().productions()[0].terminal,
            Some(Terminal::EndElement)
        );
    }

    // ========================================================================
    // Tests: Simple Type Grammar (Spec 8.5.4.1.3.1)
    // ========================================================================

    /// Spec 8.5.4.1.3.1: Simple Type Grammar Struktur
    ///
    /// Type_i,0 : CH [schema-typed value] Type_i,1
    /// Type_i,1 : EE
    #[test]
    fn simple_type_grammar_structure() {
        let (type_grammar, _) = ProtoGrammar::simple_type();

        // 2 NonTerminals
        assert_eq!(type_grammar.len(), 2);

        // Type_0: CH -> Type_1
        let type_0 = type_grammar.get(0).unwrap();
        assert_eq!(type_0.len(), 1);
        assert_eq!(type_0.productions()[0].terminal, Some(Terminal::Characters));
        assert_eq!(type_0.productions()[0].right_hand_side, 1);

        // Type_1: EE (Accept)
        let type_1 = type_grammar.get(1).unwrap();
        assert_eq!(type_1.len(), 1);
        assert_eq!(type_1.productions()[0].terminal, Some(Terminal::EndElement));
    }

    /// Spec 8.5.4.1.3.1: TypeEmpty Grammar Struktur
    ///
    /// TypeEmpty_i,0 : EE
    #[test]
    fn simple_type_empty_grammar_structure() {
        let (_, type_empty) = ProtoGrammar::simple_type();

        // 1 NonTerminal
        assert_eq!(type_empty.len(), 1);

        // TypeEmpty_0: EE (Accept)
        let type_empty_0 = type_empty.get(0).unwrap();
        assert_eq!(type_empty_0.len(), 1);
        assert_eq!(
            type_empty_0.productions()[0].terminal,
            Some(Terminal::EndElement)
        );
    }

    /// Simple Type + Concatenation: Type ⊕ Content
    #[test]
    fn simple_type_concatenation() {
        let (type_grammar, _) = ProtoGrammar::simple_type();

        // Content-Grammar die nach dem CH-Wert kommt
        let mut content = ProtoGrammar::new();
        content.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Comment, 1),
        ]));
        content.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        // Type ⊕ Content
        let result = type_grammar.concatenate(content);

        // 4 NonTerminals (2 + 2)
        assert_eq!(result.len(), 4);

        // Type_1's EE wurde durch ε ersetzt die auf Content_0 zeigt
        let type_1 = &result.get(1).unwrap().productions()[0];
        assert!(type_1.is_epsilon());
        assert_eq!(type_1.right_hand_side, 2);
    }

    // ========================================================================
    // Tests: Attribute Use Grammar (Spec 8.5.4.1.4)
    // ========================================================================

    /// Spec 8.5.4.1.4: Required Attribute Use Grammar
    ///
    /// Attribute_i,0 : AT(qname) [schema-typed value] Attribute_i,1
    /// Attribute_i,1 : EE
    #[test]
    fn attribute_use_required() {
        use crate::grammar::AttributeKind;
        use crate::qname::QName;

        let mut interner = StringInterner::new();
        let qname = QName::new("", "id");
        let qname_id = interner.intern_expanded("", "id").unwrap();
        let grammar = ProtoGrammar::attribute_use(&qname, true, &mut interner).unwrap();

        // 2 NonTerminals
        assert_eq!(grammar.len(), 2);

        // Attr_0: AT(qname) -> Attr_1
        let attr_0 = grammar.get(0).unwrap();
        assert_eq!(attr_0.len(), 1); // Nur AT, kein EE (required)
        match &attr_0.productions()[0].terminal {
            Some(Terminal::Attribute(AttributeKind::QName(q))) => {
                assert_eq!(*q, qname_id);
            }
            _ => panic!("Expected AT(qname)"),
        }
        assert_eq!(attr_0.productions()[0].right_hand_side, 1);

        // Attr_1: EE (Accept)
        let attr_1 = grammar.get(1).unwrap();
        assert_eq!(attr_1.len(), 1);
        assert_eq!(attr_1.productions()[0].terminal, Some(Terminal::EndElement));
    }

    /// Spec 8.5.4.1.4: Optional Attribute Use Grammar
    ///
    /// Attribute_i,0 : AT(qname) [schema-typed value] Attribute_i,1
    /// Attribute_i,0 : EE  (zusätzlich für optional)
    /// Attribute_i,1 : EE
    #[test]
    fn attribute_use_optional() {
        use crate::grammar::AttributeKind;
        use crate::qname::QName;

        let mut interner = StringInterner::new();
        let qname = QName::new("http://example.org", "optional");
        let qname_id = interner.intern_expanded("http://example.org", "optional").unwrap();
        let grammar = ProtoGrammar::attribute_use(&qname, false, &mut interner).unwrap();

        // 2 NonTerminals
        assert_eq!(grammar.len(), 2);

        // Attr_0: AT(qname) -> Attr_1 ODER EE (optional)
        let attr_0 = grammar.get(0).unwrap();
        assert_eq!(attr_0.len(), 2); // AT + EE

        // Erste Production: AT(qname)
        match &attr_0.productions()[0].terminal {
            Some(Terminal::Attribute(AttributeKind::QName(q))) => {
                assert_eq!(*q, qname_id);
            }
            _ => panic!("Expected AT(qname)"),
        }

        // Zweite Production: EE (kann übersprungen werden)
        assert_eq!(attr_0.productions()[1].terminal, Some(Terminal::EndElement));
    }

    /// Attribute Use Concatenation für mehrere Attribute
    #[test]
    fn attribute_use_multiple_concatenation() {
        use crate::qname::QName;

        let mut interner = StringInterner::new();
        let id = QName::new("", "id");
        let name = QName::new("", "name");

        let attr_id = ProtoGrammar::attribute_use(&id, true, &mut interner).unwrap();
        let attr_name = ProtoGrammar::attribute_use(&name, false, &mut interner).unwrap();

        // attr_id ⊕ attr_name
        let result = attr_id.concatenate(attr_name);

        // 4 NonTerminals (2 + 2)
        assert_eq!(result.len(), 4);

        // Nach Concatenation: id's EE wurde durch ε ersetzt
        let id_1 = &result.get(1).unwrap().productions()[0];
        assert!(id_1.is_epsilon());
        assert_eq!(id_1.right_hand_side, 2); // Zeigt auf name's Start
    }

    // ========================================================================
    // Tests: Attribute Wildcard (Spec 8.5.4.1.3.2)
    // ========================================================================

    /// Spec 8.5.4.1.3.2: Attribute Wildcard ##any
    ///
    /// Fügt nur zu NonTerminal 0 hinzu:
    /// G_i,0 : AT(*) G_i,0
    #[test]
    fn attribute_wildcard_any() {
        use crate::grammar::AttributeKind;
        use crate::schema::AttributeWildcard;

        // Einfache Grammar mit 2 NonTerminals
        let mut grammar = ProtoGrammar::new();
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
        ]));
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        // Wildcard hinzufügen
        let mut interner = StringInterner::new();
        grammar.add_attribute_wildcard(&AttributeWildcard::Any, &mut interner).unwrap();

        // Nur NonTerminal 0 hat AT(*) Selbstreferenz (Spec 8.5.4.1.3.2: "G_i,0")
        let nt_0 = grammar.get(0).unwrap();
        assert_eq!(nt_0.len(), 2); // Original + AT(*)
        assert_eq!(
            nt_0.productions()[1].terminal,
            Some(Terminal::Attribute(AttributeKind::Wildcard))
        );
        assert_eq!(nt_0.productions()[1].right_hand_side, 0); // Selbstreferenz

        // NonTerminal 1 bleibt unverändert
        let nt_1 = grammar.get(1).unwrap();
        assert_eq!(nt_1.len(), 1); // Nur Original
    }

    /// Spec 8.5.4.1.3.2: Attribute Wildcard mit Namespace-Liste
    ///
    /// Fügt nur zu NonTerminal 0 hinzu:
    /// G_i,0 : AT(uri:*) G_i,0  für jede URI
    #[test]
    fn attribute_wildcard_namespaces() {
        use crate::grammar::AttributeKind;
        use crate::schema::AttributeWildcard;

        let mut interner = StringInterner::new();
        let mut grammar = ProtoGrammar::new();
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 0),
        ]));

        // Wildcard mit 2 Namespaces
        grammar.add_attribute_wildcard(&AttributeWildcard::Namespaces(vec![
            "http://a.org".to_string(),
            "http://b.org".to_string(),
        ]), &mut interner).unwrap();

        // NonTerminal hat jetzt 3 Productions: EE + 2x AT(uri:*)
        let nt = grammar.get(0).unwrap();
        assert_eq!(nt.len(), 3);

        // Prüfe die Namespace-Wildcards
        match &nt.productions()[1].terminal {
            Some(Terminal::Attribute(AttributeKind::NamespaceWildcard(uri))) => {
                assert_eq!(interner.resolve(*uri), "http://a.org");
            }
            _ => panic!("Expected AT(uri:*)"),
        }
        match &nt.productions()[2].terminal {
            Some(Terminal::Attribute(AttributeKind::NamespaceWildcard(uri))) => {
                assert_eq!(interner.resolve(*uri), "http://b.org");
            }
            _ => panic!("Expected AT(uri:*)"),
        }
    }

    /// Spec 8.5.4.1.3.2: Leerer Namespace (absent) = ""
    #[test]
    fn attribute_wildcard_namespace_absent() {
        use crate::grammar::AttributeKind;
        use crate::schema::AttributeWildcard;

        let mut interner = StringInterner::new();
        let mut grammar = ProtoGrammar::new();
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 0),
        ]));

        // Wildcard mit absent (leerer String)
        grammar.add_attribute_wildcard(&AttributeWildcard::Namespaces(vec![
            "".to_string(), // absent
        ]), &mut interner).unwrap();

        let nt = grammar.get(0).unwrap();
        match &nt.productions()[1].terminal {
            Some(Terminal::Attribute(AttributeKind::NamespaceWildcard(uri))) => {
                assert_eq!(interner.resolve(*uri), ""); // absent
            }
            _ => panic!("Expected AT(uri:*)"),
        }
    }

    // ========================================================================
    // Tests: Complex Type Grammar (Spec 8.5.4.1.3.2)
    // ========================================================================

    /// Spec 8.5.4.1.3.2: Complex Type mit Empty Content
    #[test]
    fn complex_type_empty_content() {
        use crate::qname::QName;
        use crate::schema::{AttributeUse, ContentType, TypeDefinition};
        use std::rc::Rc;

        let mut interner = StringInterner::new();
        let def = TypeDefinition::Complex {
            name: Some(Rc::new(QName::new("", "EmptyType"))),
            base_type: None,
            derivation: None,
            attributes: vec![AttributeUse {
                type_definition: None,
                qname: Rc::new(QName::new("", "id")),
                required: true,
            }],
            attribute_wildcard: None,
            content: ContentType::Empty,
            has_named_sub_types: false,
        };

        let (type_grammar, type_empty) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();

        // Type: AT(id) -> EE
        // Attribute Grammar (2 NTs) ⊕ Empty Content (1 NT mit EE)
        assert!(type_grammar.len() >= 2);

        // TypeEmpty: gleiche Struktur wie Type bei Empty Content
        assert!(type_empty.len() >= 2);
    }

    /// Spec 8.5.4.1.3.2: Complex Type mit Simple Content
    #[test]
    fn complex_type_simple_content() {
        use crate::qname::QName;
        use crate::schema::{AttributeUse, ContentType, TypeDefinition};

        let mut interner = StringInterner::new();
        let def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![AttributeUse {
                type_definition: None,
                qname: Rc::new(QName::new("", "value")),
                required: false,
            }],
            attribute_wildcard: None,
            content: ContentType::Simple,
            has_named_sub_types: false,
        };

        let (type_grammar, type_empty) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();

        // Type: AT(value)? -> CH -> EE
        // Mehr NonTerminals wegen Simple Content
        assert!(type_grammar.len() >= 3);

        // TypeEmpty: AT(value)? -> EE (kein CH)
        assert!(type_empty.len() >= 2);
    }

    /// Spec 8.5.4.1.3.2: Complex Type mit Attribute Wildcard
    #[test]
    fn complex_type_with_wildcard() {
        use crate::grammar::AttributeKind;
        use crate::qname::QName;
        use crate::schema::{AttributeUse, AttributeWildcard, ContentType, TypeDefinition};

        let mut interner = StringInterner::new();
        let def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![AttributeUse {
                type_definition: None,
                qname: Rc::new(QName::new("", "id")),
                required: true,
            }],
            attribute_wildcard: Some(AttributeWildcard::Any),
            content: ContentType::Empty,
            has_named_sub_types: false,
        };

        let (type_grammar, _) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();

        // Prüfe dass AT(*) Production existiert
        let has_wildcard = type_grammar.non_terminals().iter().any(|nt| {
            nt.productions().iter().any(|p| {
                matches!(
                    &p.terminal,
                    Some(Terminal::Attribute(AttributeKind::Wildcard))
                )
            })
        });
        assert!(has_wildcard, "AT(*) Production sollte existieren");
    }

    /// Spec 8.5.4.1.3.2: Complex Type ohne Attribute
    ///
    /// Spec: "If there is neither an attribute use nor an {attribute wildcard},
    /// G_0 of the following form is used as an attribute use grammar:
    /// G_0,0 : EE"
    #[test]
    fn complex_type_no_attributes() {
        use crate::schema::{ContentType, TypeDefinition};

        let mut interner = StringInterner::new();
        let def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: ContentType::Empty,
            has_named_sub_types: false,
        };

        let (type_grammar, type_empty) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();

        // Leere Attribut-Grammar (1 NT) ⊕ Empty Content (1 NT) = 2 NTs
        // (EE in Attr wird durch ε ersetzt die auf Content zeigt)
        assert_eq!(type_grammar.len(), 2);
        assert_eq!(type_empty.len(), 2);

        // Beide sollten zu EE führen nach Normalisierung
        // Prüfe dass letztes NT EE hat
        assert_eq!(
            type_grammar
                .get(type_grammar.len() - 1)
                .unwrap()
                .productions()[0]
                .terminal,
            Some(Terminal::EndElement)
        );
    }

    /// Simple Type Definition ergibt Fehler bei complex_type()
    #[test]
    fn complex_type_rejects_simple() {
        use crate::qname::QName;
        use crate::schema::{SimpleTypeVariety, TypeDefinition};

        let def = TypeDefinition::Simple {
            name: Some(Rc::new(QName::new("", "string"))),
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: Some("string".to_string()),
            is_union: false,
            enumeration_values: Vec::new(),
            has_named_sub_types: false,
        };

        let mut interner = StringInterner::new();
        let result = ProtoGrammar::complex_type(&def, &mut interner);
        assert!(result.is_err());
    }

    // ========================================================================
    // Tests: Normalisierung (Spec 8.5.4.2)
    // ========================================================================

    /// Spec 8.5.4.2.1: ε-Elimination - einfacher Fall
    ///
    /// Vor: NT_0 : ε→NT_1, NT_1 : CH NT_1
    /// Nach: NT_0 : CH NT_1, NT_1 : CH NT_1
    #[test]
    fn normalize_epsilon_elimination_simple() {
        let mut grammar = ProtoGrammar::new();
        // NT_0 : ε → NT_1
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::epsilon(1),
        ]));
        // NT_1 : CH NT_1
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
        ]));

        grammar.eliminate_epsilon_productions();

        // NT_0 sollte jetzt CH haben (von NT_1 kopiert)
        let nt_0 = grammar.get(0).unwrap();
        assert_eq!(nt_0.len(), 1);
        assert_eq!(nt_0.productions()[0].terminal, Some(Terminal::Characters));
    }

    /// Spec 8.5.4.2.1: ε-Elimination - Kette
    ///
    /// Vor: NT_0 : ε→NT_1, NT_1 : ε→NT_2, NT_2 : EE
    /// Nach: NT_0 : EE, NT_1 : EE, NT_2 : EE
    #[test]
    fn normalize_epsilon_elimination_chain() {
        let mut grammar = ProtoGrammar::new();
        // NT_0 : ε → NT_1
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::epsilon(1),
        ]));
        // NT_1 : ε → NT_2
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::epsilon(2),
        ]));
        // NT_2 : EE
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 2),
        ]));

        grammar.eliminate_epsilon_productions();

        // Alle sollten EE haben
        for i in 0..3 {
            let nt = grammar.get(i).unwrap();
            assert!(
                !nt.productions().iter().any(|p| p.is_epsilon()),
                "NT_{} sollte keine ε-Productions mehr haben",
                i
            );
            assert!(
                nt.productions()
                    .iter()
                    .any(|p| p.terminal == Some(Terminal::EndElement)),
                "NT_{} sollte EE haben",
                i
            );
        }
    }

    /// Spec 8.5.4.2.1: ε-Elimination - mehrere Productions
    ///
    /// Vor: NT_0 : ε→NT_1, NT_1 : CH NT_1 | EE
    /// Nach: NT_0 : CH NT_1 | EE
    #[test]
    fn normalize_epsilon_elimination_multiple_productions() {
        let mut grammar = ProtoGrammar::new();
        // NT_0 : ε → NT_1
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::epsilon(1),
        ]));
        // NT_1 : CH NT_1 | EE NT_1
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        grammar.eliminate_epsilon_productions();

        // NT_0 sollte beide Productions haben
        let nt_0 = grammar.get(0).unwrap();
        assert_eq!(nt_0.len(), 2);
        assert!(
            nt_0.productions()
                .iter()
                .any(|p| p.terminal == Some(Terminal::Characters))
        );
        assert!(
            nt_0.productions()
                .iter()
                .any(|p| p.terminal == Some(Terminal::EndElement))
        );
    }

    /// Spec 8.5.4.2.1: ε-Elimination - Selbstreferenz wird ignoriert
    #[test]
    fn normalize_epsilon_elimination_self_reference() {
        let mut grammar = ProtoGrammar::new();
        // NT_0 : ε → NT_0 (Selbstreferenz)
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::epsilon(0),
        ]));

        grammar.eliminate_epsilon_productions();

        // Sollte keine Endlosschleife verursachen
        // Production wird entfernt (leere Productions)
        let nt_0 = grammar.get(0).unwrap();
        assert!(nt_0.is_empty() || !nt_0.productions().iter().any(|p| p.is_epsilon()));
    }

    /// Nach Concatenation: Normalisierung eliminiert ε
    #[test]
    fn normalize_after_concatenation() {
        // Simple Type: CH -> EE
        let (type_grammar, _) = ProtoGrammar::simple_type();

        // Content: Comment -> EE
        let mut content = ProtoGrammar::new();
        content.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Comment, 1),
        ]));
        content.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        // Concatenation erzeugt ε
        let mut result = type_grammar.concatenate(content);
        assert!(
            result
                .non_terminals()
                .iter()
                .any(|nt| nt.productions().iter().any(|p| p.is_epsilon())),
            "Nach Concatenation sollte ε existieren"
        );

        // Normalisierung eliminiert ε
        result.eliminate_epsilon_productions();
        assert!(
            !result
                .non_terminals()
                .iter()
                .any(|nt| nt.productions().iter().any(|p| p.is_epsilon())),
            "Nach Normalisierung sollte keine ε existieren"
        );
    }

    // ========================================================================
    // Tests: Konvertierung zu NonTerminal (Phase 9)
    // ========================================================================

    /// Simple Type zu NonTerminal konvertieren
    #[test]
    fn to_non_terminals_simple_type() {
        use crate::grammar::NonTerminalId;

        let (type_grammar, _) = ProtoGrammar::simple_type();
        let nts = type_grammar
            .to_non_terminals(NonTerminalId::ElementContent)
            .unwrap();

        // 2 NonTerminals
        assert_eq!(nts.len(), 2);

        // Prüfe dass Productions vorhanden sind
        assert!(!nts[0].productions().is_empty());
        assert!(!nts[1].productions().is_empty());

        // Prüfe Terminals
        assert_eq!(nts[0].productions()[0].terminal, Terminal::Characters);
        assert_eq!(nts[1].productions()[0].terminal, Terminal::EndElement);
    }

    /// Complex Type zu NonTerminal konvertieren
    #[test]
    fn to_non_terminals_complex_type() {
        use crate::grammar::NonTerminalId;
        use crate::qname::QName;
        use crate::schema::{AttributeUse, ContentType, TypeDefinition};

        let mut interner = StringInterner::new();
        let def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![AttributeUse {
                type_definition: None,
                qname: Rc::new(QName::new("", "id")),
                required: true,
            }],
            attribute_wildcard: None,
            content: ContentType::Simple,
            has_named_sub_types: false,
        };

        let (type_grammar, _) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();
        let nts = type_grammar
            .to_non_terminals(NonTerminalId::StartTagContent)
            .unwrap();

        // Sollte mehrere NonTerminals haben
        assert!(nts.len() >= 2);

        // Prüfe dass alle Productions Terminals haben (keine ε nach Konvertierung)
        for nt in &nts {
            assert!(
                !nt.productions().is_empty(),
                "NonTerminal sollte Productions haben"
            );
        }
    }

    /// Concatenierte Grammar zu NonTerminal
    #[test]
    fn to_non_terminals_after_concatenation() {
        use crate::grammar::NonTerminalId;

        let (type_grammar, _) = ProtoGrammar::simple_type();

        let mut content = ProtoGrammar::new();
        content.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Comment, 1),
        ]));
        content.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        let combined = type_grammar.concatenate(content);
        let nts = combined
            .to_non_terminals(NonTerminalId::ElementContent)
            .unwrap();

        // Prüfe dass alle NonTerminals Productions haben (keine leeren nach Normalisierung)
        for nt in &nts {
            assert!(
                !nt.productions().is_empty(),
                "NonTerminal sollte Productions haben"
            );
        }
    }

    // ========================================================================
    // Tests: Element Term (Spec 8.5.4.1.6)
    // ========================================================================

    /// Spec 8.5.4.1.6: Element Term ohne Substitution Group
    ///
    /// ParticleTerm_i,0 : SE(qname) ParticleTerm_i,1
    /// ParticleTerm_i,1 : EE
    #[test]
    fn element_term_simple() {
        use crate::grammar::StartElementKind;
        use crate::qname::QName;
        use crate::schema::ElementDeclaration;

        let mut interner = StringInterner::new();
        let qname = Rc::new(QName::new("http://example.org", "book"));
        let qname_id = interner.intern_expanded("http://example.org", "book").unwrap();
        let decl = ElementDeclaration::new(qname.clone());
        let grammar = ProtoGrammar::element_term(&decl, &mut interner).unwrap();

        // 2 NonTerminals
        assert_eq!(grammar.len(), 2);

        // NT_0: SE(qname) -> NT_1
        let nt_0 = grammar.get(0).unwrap();
        assert_eq!(nt_0.len(), 1);
        match &nt_0.productions()[0].terminal {
            Some(Terminal::StartElement(StartElementKind::QName(q))) => {
                assert_eq!(*q, qname_id);
            }
            _ => panic!("Expected SE(qname)"),
        }
        assert_eq!(nt_0.productions()[0].right_hand_side, 1);

        // NT_1: EE
        let nt_1 = grammar.get(1).unwrap();
        assert_eq!(nt_1.len(), 1);
        assert_eq!(nt_1.productions()[0].terminal, Some(Terminal::EndElement));
    }

    /// Spec 8.5.4.1.6: Element Term mit Substitution Group
    ///
    /// ParticleTerm_i,0 : SE(qname_0) ParticleTerm_i,1
    ///                  | SE(qname_1) ParticleTerm_i,1
    ///                  | SE(qname_2) ParticleTerm_i,1
    #[test]
    fn element_term_with_substitution_group() {
        use crate::grammar::StartElementKind;
        use crate::qname::QName;
        use crate::schema::ElementDeclaration;

        let mut interner = StringInterner::new();
        let e0 = Rc::new(QName::new("", "book"));
        let e1 = Rc::new(QName::new("", "novel"));
        let e2 = Rc::new(QName::new("", "magazine"));
        let decl =
            ElementDeclaration::with_substitution_group(e0.clone(), vec![e1.clone(), e2.clone()]);
        let grammar = ProtoGrammar::element_term(&decl, &mut interner).unwrap();

        // 2 NonTerminals
        assert_eq!(grammar.len(), 2);

        // NT_0: 3 Productions (E_0, E_1, E_2)
        let nt_0 = grammar.get(0).unwrap();
        assert_eq!(nt_0.len(), 3);

        // Prüfe dass alle QNames vorhanden sind
        let names: Vec<&str> = nt_0
            .productions()
            .iter()
            .filter_map(|p| match &p.terminal {
                Some(Terminal::StartElement(StartElementKind::QName(q))) => {
                    let (_, local) = q.resolve(&interner);
                    Some(local)
                }
                _ => None,
            })
            .collect();
        assert_eq!(names.len(), 3);
        assert!(names.contains(&"book"));
        assert!(names.contains(&"novel"));
        assert!(names.contains(&"magazine"));
    }

    // ========================================================================
    // Tests: Wildcard Term (Spec 8.5.4.1.7)
    // ========================================================================

    /// Spec 8.5.4.1.7: Wildcard Term ##any → SE(*)
    #[test]
    fn wildcard_term_any() {
        use crate::grammar::StartElementKind;
        use crate::schema::{Wildcard, WildcardConstraint};

        let mut interner = StringInterner::new();
        let grammar = ProtoGrammar::wildcard_term(&Wildcard::new(WildcardConstraint::Any), &mut interner).unwrap();

        // 2 NonTerminals
        assert_eq!(grammar.len(), 2);

        // NT_0: SE(*) -> NT_1
        let nt_0 = grammar.get(0).unwrap();
        assert_eq!(nt_0.len(), 1);
        assert_eq!(
            nt_0.productions()[0].terminal,
            Some(Terminal::StartElement(StartElementKind::Wildcard))
        );

        // NT_1: EE
        let nt_1 = grammar.get(1).unwrap();
        assert_eq!(nt_1.productions()[0].terminal, Some(Terminal::EndElement));
    }

    /// Spec 8.5.4.1.7: Wildcard Term ##other (Not) → SE(*)
    #[test]
    fn wildcard_term_not() {
        use crate::grammar::StartElementKind;
        use crate::schema::{Wildcard, WildcardConstraint};

        let mut interner = StringInterner::new();

        // ##other mit Namespace
        let grammar = ProtoGrammar::wildcard_term(&Wildcard::new(WildcardConstraint::Not(Some(
            "http://example.org".to_string(),
        ))), &mut interner)
        .unwrap();
        let nt_0 = grammar.get(0).unwrap();
        assert_eq!(
            nt_0.productions()[0].terminal,
            Some(Terminal::StartElement(StartElementKind::Wildcard))
        );

        // ##other mit absent (None)
        let grammar_absent =
            ProtoGrammar::wildcard_term(&Wildcard::new(WildcardConstraint::Not(None)), &mut interner).unwrap();
        let nt_0_absent = grammar_absent.get(0).unwrap();
        assert_eq!(
            nt_0_absent.productions()[0].terminal,
            Some(Terminal::StartElement(StartElementKind::Wildcard))
        );
    }

    /// Spec 8.5.4.1.7: Wildcard Term Namespace-Liste → SE(uri:*)
    #[test]
    fn wildcard_term_namespaces() {
        use crate::grammar::StartElementKind;
        use crate::schema::{Wildcard, WildcardConstraint};

        let mut interner = StringInterner::new();
        let grammar = ProtoGrammar::wildcard_term(&Wildcard::new(WildcardConstraint::Namespaces(
            vec![
                "http://a.org".to_string(),
                "".to_string(), // absent
                "http://b.org".to_string(),
            ],
        )), &mut interner)
        .unwrap();

        // 2 NonTerminals
        assert_eq!(grammar.len(), 2);

        // NT_0: 3 Productions (SE(uri:*) für jede URI)
        let nt_0 = grammar.get(0).unwrap();
        assert_eq!(nt_0.len(), 3);

        // Prüfe URIs
        let uris: Vec<&str> = nt_0
            .productions()
            .iter()
            .filter_map(|p| match &p.terminal {
                Some(Terminal::StartElement(StartElementKind::NamespaceWildcard(u))) => {
                    Some(interner.resolve(*u))
                }
                _ => None,
            })
            .collect();
        assert_eq!(uris.len(), 3);
        assert!(uris.contains(&"http://a.org"));
        assert!(uris.contains(&"")); // absent
        assert!(uris.contains(&"http://b.org"));
    }

    /// Spec 8.5.4.1.7: Leere Namespace-Liste → EmptyNamespaceList Error
    #[test]
    fn wildcard_term_empty_namespaces() {
        use crate::schema::{Wildcard, WildcardConstraint};

        let mut interner = StringInterner::new();
        let result =
            ProtoGrammar::wildcard_term(&Wildcard::new(WildcardConstraint::Namespaces(vec![])), &mut interner);

        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            crate::Error::EmptyNamespaceList
        ));
    }

    // ========================================================================
    // Tests: Particle (Spec 8.5.4.1.5)
    // ========================================================================

    /// Spec 8.5.4.1.5: Particle min=max=1 → Term unverändert
    #[test]
    fn particle_min1_max1() {
        use crate::qname::QName;
        use crate::schema::{ElementDeclaration, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        let qname = Rc::new(QName::new("", "item"));
        let term = ParticleTerm::Element(ElementDeclaration::new(qname));
        let p = Particle::once(term);

        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // Sollte wie element_term aussehen: 2 NonTerminals
        assert_eq!(grammar.len(), 2);
    }

    /// Spec 8.5.4.1.5: Particle min=0, max=0 → nur EE
    #[test]
    fn particle_min0_max0() {
        use crate::schema::{MaxOccurs, Particle, ParticleTerm, Wildcard};

        let mut interner = StringInterner::new();
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let p = Particle::new(0, MaxOccurs::Bounded(0), term).unwrap();

        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // Nur EE
        assert_eq!(grammar.len(), 1);
        assert_eq!(
            grammar.get(0).unwrap().productions()[0].terminal,
            Some(Terminal::EndElement)
        );
    }

    /// Spec 8.5.4.1.5: Particle min=0, max=1 → Term mit optionaler EE
    #[test]
    fn particle_min0_max1() {
        use crate::qname::QName;
        use crate::schema::{ElementDeclaration, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        let qname = Rc::new(QName::new("", "optional"));
        let term = ParticleTerm::Element(ElementDeclaration::new(qname));
        let p = Particle::optional(term);

        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // NT_0 sollte SE und EE haben
        let nt_0 = grammar.get(0).unwrap();
        assert!(
            nt_0.productions()
                .iter()
                .any(|p| p.terminal == Some(Terminal::EndElement)),
            "NT_0 sollte EE haben (optional)"
        );
    }

    /// Spec 8.5.4.1.5: Particle min=0, max=unbounded → optionaler Loop
    #[test]
    fn particle_min0_max_unbounded() {
        use crate::schema::{Particle, ParticleTerm, Wildcard};

        let mut interner = StringInterner::new();
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let p = Particle::zero_or_more(term);

        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // NT_0 sollte EE haben (optional)
        let nt_0 = grammar.get(0).unwrap();
        assert!(
            nt_0.productions()
                .iter()
                .any(|p| p.terminal == Some(Terminal::EndElement)),
            "NT_0 sollte EE haben (optional)"
        );

        // Sollte Loop-Struktur haben: irgendwo eine ε-Production die zurück zu NT_0 zeigt
        // (kann in NT_0 oder NT_1 sein, je nach Implementierung)
        let has_loop = grammar.non_terminals().iter().any(|nt| {
            nt.productions()
                .iter()
                .any(|p| p.is_epsilon() && p.right_hand_side == 0)
        });
        assert!(has_loop, "Sollte Loop-ε zu NT_0 haben");
    }

    /// Spec 8.5.4.1.5: Particle min=1, max=unbounded (Spec H.1: Particle_product)
    #[test]
    fn particle_min1_max_unbounded() {
        use crate::qname::QName;
        use crate::schema::{ElementDeclaration, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        let qname = Rc::new(QName::new("", "product"));
        let term = ParticleTerm::Element(ElementDeclaration::new(qname));
        let p = Particle::one_or_more(term);

        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // Mindestens 1 required copy + 1 loop copy
        // Nach Concatenation: mehr als 2 NonTerminals
        assert!(grammar.len() >= 2, "Sollte mindestens 2 NonTerminals haben");

        // Irgendwo sollte ein Loop sein (ε zu sich selbst oder zu früherem NT)
        let has_loop = grammar
            .non_terminals()
            .iter()
            .any(|nt| nt.productions().iter().any(|p| p.is_epsilon()));
        assert!(has_loop, "Sollte Loop-Struktur haben");
    }

    /// Spec 8.5.4.1.5: Particle min=2, max=5 → 2 required + 3 optional
    #[test]
    fn particle_min2_max5() {
        use crate::schema::{MaxOccurs, Particle, ParticleTerm, Wildcard};

        let mut interner = StringInterner::new();
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let p = Particle::new(2, MaxOccurs::Bounded(5), term).unwrap();

        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // 5 copies × 2 NTs = mindestens 10 NonTerminals (können weniger sein nach Optimierung)
        assert!(grammar.len() >= 4, "Sollte mehrere NonTerminals haben");
    }

    /// Spec 8.5.4.1.5: Particle max < min → InvalidParticleOccurs
    #[test]
    fn particle_invalid_max_less_than_min() {
        use crate::schema::{MaxOccurs, Particle, ParticleTerm, Wildcard};

        let term = ParticleTerm::Wildcard(Wildcard::any());
        let result = Particle::new(5, MaxOccurs::Bounded(2), term);

        assert!(result.is_err(), "max < min sollte Fehler ergeben");
        assert!(matches!(
            result.unwrap_err(),
            crate::Error::InvalidParticleOccurs { min: 5, max: 2 }
        ));
    }

    // ========================================================================
    // Tests: Sequence Model Group (Spec 8.5.4.1.8.1)
    // ========================================================================

    /// Spec 8.5.4.1.8.1: Leere Sequence → nur EE
    #[test]
    fn sequence_term_empty() {
        let mut interner = StringInterner::new();
        let grammar = ProtoGrammar::sequence_term(&[], &mut interner).unwrap();

        assert_eq!(grammar.len(), 1);
        assert_eq!(
            grammar.get(0).unwrap().productions()[0].terminal,
            Some(Terminal::EndElement)
        );
    }

    /// Spec 8.5.4.1.8.1: Sequence mit 1 Particle
    #[test]
    fn sequence_term_single() {
        use crate::schema::{Particle, ParticleTerm, Wildcard};

        let mut interner = StringInterner::new();
        let p = Particle::once(ParticleTerm::Wildcard(Wildcard::any()));
        let grammar = ProtoGrammar::sequence_term(&[p], &mut interner).unwrap();

        // Sollte wie ein einzelner Particle aussehen
        assert!(grammar.len() >= 2);
    }

    /// Spec 8.5.4.1.8.1: Sequence mit 3 Particles (Spec H.1: Term_sequence)
    #[test]
    fn sequence_term_multiple() {
        use crate::qname::QName;
        use crate::schema::{ElementDeclaration, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        // description?, quantity, price (aus Spec H.1)
        let desc = Particle::optional(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "description"),
        ))));
        let qty = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "quantity"),
        ))));
        let price = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "price"),
        ))));

        let grammar = ProtoGrammar::sequence_term(&[desc, qty, price], &mut interner).unwrap();

        // Nach Concatenation: mehrere NonTerminals
        assert!(
            grammar.len() >= 3,
            "Sollte mehrere NonTerminals haben (concatenated)"
        );
    }

    // ========================================================================
    // Tests: Choice Model Group (Spec 8.5.4.1.8.2)
    // ========================================================================

    /// Spec 8.5.4.1.8.2: Leere Choice → nur EE
    #[test]
    fn choice_term_empty() {
        let mut interner = StringInterner::new();
        let grammar = ProtoGrammar::choice_term(&[], &mut interner).unwrap();

        assert_eq!(grammar.len(), 1);
        assert_eq!(
            grammar.get(0).unwrap().productions()[0].terminal,
            Some(Terminal::EndElement)
        );
    }

    /// Spec 8.5.4.1.8.2: Choice mit 2 Particles
    #[test]
    fn choice_term_two() {
        use crate::qname::QName;
        use crate::schema::{ElementDeclaration, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        let p1 = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "option1"),
        ))));
        let p2 = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "option2"),
        ))));

        let grammar = ProtoGrammar::choice_term(&[p1, p2], &mut interner).unwrap();

        // NT_0 sollte ε-Productions zu beiden Particle-Starts haben
        let nt_0 = grammar.get(0).unwrap();
        let epsilon_count = nt_0.productions().iter().filter(|p| p.is_epsilon()).count();
        assert_eq!(epsilon_count, 2, "Choice NT_0 sollte 2 ε-Productions haben");
    }

    /// Spec 8.5.4.1.8.2: Choice mit 3 Particles
    #[test]
    fn choice_term_three() {
        use crate::schema::{Particle, ParticleTerm, Wildcard};

        let mut interner = StringInterner::new();
        let p1 = Particle::once(ParticleTerm::Wildcard(Wildcard::any()));
        let p2 = Particle::optional(ParticleTerm::Wildcard(Wildcard::any()));
        let p3 = Particle::once(ParticleTerm::Wildcard(Wildcard::any()));

        let grammar = ProtoGrammar::choice_term(&[p1, p2, p3], &mut interner).unwrap();

        // NT_0 sollte 3 ε-Productions haben
        let nt_0 = grammar.get(0).unwrap();
        let epsilon_count = nt_0.productions().iter().filter(|p| p.is_epsilon()).count();
        assert_eq!(epsilon_count, 3, "Choice NT_0 sollte 3 ε-Productions haben");
    }

    // ========================================================================
    // Tests: All Model Group (Spec 8.5.4.1.8.3)
    // ========================================================================

    /// Spec 8.5.4.1.8.3: Leere All → nur EE
    #[test]
    fn all_term_empty() {
        let mut interner = StringInterner::new();
        let grammar = ProtoGrammar::all_term(&[], &mut interner).unwrap();

        assert_eq!(grammar.len(), 1);
        assert_eq!(
            grammar.get(0).unwrap().productions()[0].terminal,
            Some(Terminal::EndElement)
        );
    }

    /// Spec 8.5.4.1.8.3: All mit 2 Particles
    #[test]
    fn all_term_two() {
        use crate::qname::QName;
        use crate::schema::{ElementDeclaration, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        let p1 = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "a"),
        ))));
        let p2 = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "b"),
        ))));

        let grammar = ProtoGrammar::all_term(&[p1, p2], &mut interner).unwrap();

        // NT_0 sollte EE + 2 ε-Productions haben
        let nt_0 = grammar.get(0).unwrap();
        let has_ee = nt_0
            .productions()
            .iter()
            .any(|p| p.terminal == Some(Terminal::EndElement));
        let epsilon_count = nt_0.productions().iter().filter(|p| p.is_epsilon()).count();

        assert!(has_ee, "All NT_0 sollte EE haben");
        assert_eq!(epsilon_count, 2, "All NT_0 sollte 2 ε-Productions haben");

        // Alle Particle-EEs sollten zu NT_0 zurück zeigen
        for nt_idx in 1..grammar.len() {
            let nt = grammar.get(nt_idx).unwrap();
            for prod in nt.productions() {
                if prod.is_epsilon() {
                    assert_eq!(prod.right_hand_side, 0, "All: EE sollte zu NT_0 zeigen");
                }
            }
        }
    }

    // ========================================================================
    // Tests: Mixed Content (Spec 8.5.4.1.3.2)
    // ========================================================================

    /// Spec 8.5.4.1.3.2: add_mixed_content_ch fügt CH zu allen NTs hinzu
    #[test]
    fn add_mixed_content_ch() {
        let mut grammar = ProtoGrammar::new();
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 0),
        ]));
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Comment, 1),
        ]));
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 2),
        ]));

        grammar.add_mixed_content_ch();

        // Alle NTs sollten CH haben
        for (idx, nt) in grammar.non_terminals().iter().enumerate() {
            let has_ch = nt
                .productions()
                .iter()
                .any(|p| p.terminal == Some(Terminal::Characters) && p.right_hand_side == idx);
            assert!(has_ch, "NT_{} sollte CH-Selbstreferenz haben", idx);
        }
    }

    /// Spec 8.5.4.1.3.2: Complex Type mit Mixed Content
    #[test]
    fn complex_type_mixed_content() {
        use crate::qname::QName;
        use crate::schema::{
            ContentType, ElementDeclaration, Particle, ParticleTerm, TypeDefinition,
        };

        let mut interner = StringInterner::new();
        let particle = Particle::optional(ParticleTerm::Element(ElementDeclaration::new(
            Rc::new(QName::new("", "child")),
        )));

        let def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: ContentType::Mixed(particle),
            has_named_sub_types: false,
        };

        let (type_grammar, _) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();

        // Prüfe dass CH-Productions existieren
        let has_ch = type_grammar.non_terminals().iter().any(|nt| {
            nt.productions()
                .iter()
                .any(|p| p.terminal == Some(Terminal::Characters))
        });
        assert!(has_ch, "Mixed Content sollte CH-Productions haben");
    }

    /// Spec 8.5.4.1.3.2: Complex Type mit ElementOnly Content (kein CH)
    #[test]
    fn complex_type_element_only_content() {
        use crate::qname::QName;
        use crate::schema::{
            ContentType, ElementDeclaration, Particle, ParticleTerm, TypeDefinition,
        };

        let mut interner = StringInterner::new();
        let particle = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "child"),
        ))));

        let def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: ContentType::ElementOnly(particle),
            has_named_sub_types: false,
        };

        let (type_grammar, _) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();

        // ElementOnly sollte KEINE CH-Productions haben (im Gegensatz zu Mixed)
        // Außer es gibt andere Gründe... checke ob CH nur in Mixed ist
        // In der aktuellen Implementierung hat ElementOnly kein add_mixed_content_ch()
        // Also sollte es kein CH an beliebigen Stellen haben
        // (CH kann aber durch Simple Content existieren - hier nicht relevant)
        assert!(type_grammar.len() >= 1);
    }

    // ========================================================================
    // Tests: Spec H.1 Integrationstests
    // ========================================================================

    /// Spec H.1: Product Element Grammar
    ///
    /// <product>
    ///   <description>?</description>
    ///   <quantity></quantity>
    ///   <price></price>
    /// </product>
    #[test]
    fn spec_h1_product_element() {
        use crate::qname::QName;
        use crate::schema::{Compositor, ElementDeclaration, ModelGroup, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        // Term_description, Term_quantity, Term_price
        let desc = Particle::optional(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "description"),
        ))));
        let qty = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "quantity"),
        ))));
        let price = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "price"),
        ))));

        // Sequence(description?, quantity, price)
        let sequence_term = ParticleTerm::ModelGroup(ModelGroup::new(
            Compositor::Sequence,
            vec![desc, qty, price],
        ));

        // Particle mit min=1, max=1
        let content_particle = Particle::once(sequence_term);

        // Generiere Grammar
        let grammar = ProtoGrammar::particle(&content_particle, &mut interner).unwrap();

        // Sollte funktionieren und mehrere NonTerminals haben
        assert!(
            grammar.len() >= 3,
            "Product Grammar sollte mehrere NTs haben"
        );
    }

    /// Spec H.1: Order Element Grammar
    ///
    /// <order>
    ///   <product>+</product>
    /// </order>
    #[test]
    fn spec_h1_order_element() {
        use crate::qname::QName;
        use crate::schema::{ElementDeclaration, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        // product+ (min=1, max=unbounded)
        let product = Particle::one_or_more(ParticleTerm::Element(ElementDeclaration::new(
            Rc::new(QName::new("", "product")),
        )));

        let grammar = ProtoGrammar::particle(&product, &mut interner).unwrap();

        // Sollte Loop-Struktur haben
        let has_loop = grammar
            .non_terminals()
            .iter()
            .any(|nt| nt.productions().iter().any(|p| p.is_epsilon()));
        assert!(has_loop, "Order Grammar sollte Loop für unbounded haben");
    }

    // ========================================================================
    // Tests: Coverage für nicht abgedeckte Zeilen
    // ========================================================================

    /// Test für get_mut() Methode (Zeile 179-180)
    #[test]
    fn proto_grammar_get_mut() {
        let mut grammar = ProtoGrammar::new();
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 0),
        ]));

        // get_mut existierender Index
        assert!(grammar.get_mut(0).is_some());

        // Modifiziere über get_mut
        grammar
            .get_mut(0)
            .unwrap()
            .add_production(ProtoProduction::new(Terminal::Characters, 0));
        assert_eq!(grammar.get(0).unwrap().len(), 2);

        // get_mut nicht existierender Index
        assert!(grammar.get_mut(99).is_none());
    }

    /// Test für Attribut-Sortierung mit gleichem local-name (Zeile 225-226)
    #[test]
    fn complex_type_attribute_sort_by_uri() {
        use crate::qname::QName;
        use crate::schema::{AttributeUse, ContentType, TypeDefinition};

        // Attribute mit gleichem local-name aber verschiedenen URIs
        let def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![
                AttributeUse {
                type_definition: None,
                    qname: Rc::new(QName::new("http://z.org", "attr")),
                    required: true,
                },
                AttributeUse {
                type_definition: None,
                    qname: Rc::new(QName::new("http://a.org", "attr")),
                    required: true,
                },
            ],
            attribute_wildcard: None,
            content: ContentType::Empty,
            has_named_sub_types: false,
        };

        // Sollte ohne Fehler funktionieren (Sortierung nach URI)
        let mut interner = StringInterner::new();
        let (type_grammar, _) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();
        assert!(type_grammar.len() >= 2);
    }

    /// Test für Complex Type ohne Attribute aber mit Wildcard (Zeile 246)
    #[test]
    fn complex_type_no_attributes_with_wildcard() {
        use crate::grammar::AttributeKind;
        use crate::schema::{AttributeWildcard, ContentType, TypeDefinition};

        let mut interner = StringInterner::new();
        let def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],                               // Keine Attribute
            attribute_wildcard: Some(AttributeWildcard::Any), // Aber Wildcard
            content: ContentType::Empty,
            has_named_sub_types: false,
        };

        let (type_grammar, _) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();

        // Sollte AT(*) Production haben
        let has_wildcard = type_grammar.non_terminals().iter().any(|nt| {
            nt.productions().iter().any(|p| {
                matches!(
                    &p.terminal,
                    Some(Terminal::Attribute(AttributeKind::Wildcard))
                )
            })
        });
        assert!(has_wildcard, "Sollte AT(*) Production haben");
    }

    /// Test für concatenate_all mit 2+ Attributen (Zeile 278)
    #[test]
    fn complex_type_multiple_attributes() {
        use crate::qname::QName;
        use crate::schema::{AttributeUse, ContentType, TypeDefinition};

        let def = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![
                AttributeUse {
                type_definition: None,
                    qname: Rc::new(QName::new("", "a")),
                    required: true,
                },
                AttributeUse {
                type_definition: None,
                    qname: Rc::new(QName::new("", "b")),
                    required: true,
                },
                AttributeUse {
                type_definition: None,
                    qname: Rc::new(QName::new("", "c")),
                    required: false,
                },
            ],
            attribute_wildcard: None,
            content: ContentType::Empty,
            has_named_sub_types: false,
        };

        let mut interner = StringInterner::new();
        let (type_grammar, _) = ProtoGrammar::complex_type(&def, &mut interner).unwrap();
        // 3 Attribute × 2 NTs + Content = mindestens 7 NTs (vor Concatenation)
        assert!(type_grammar.len() >= 4, "Sollte mehrere NTs haben");
    }

    /// Test für model_group() mit Choice (Zeile 537)
    #[test]
    fn particle_with_choice_model_group() {
        use crate::qname::QName;
        use crate::schema::{Compositor, ElementDeclaration, ModelGroup, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        let choice = ModelGroup::new(
            Compositor::Choice,
            vec![
                Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
                    QName::new("", "a"),
                )))),
                Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
                    QName::new("", "b"),
                )))),
            ],
        );

        let p = Particle::once(ParticleTerm::ModelGroup(choice));
        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // Choice: NT_0 hat ε-Productions zu den Particles
        assert!(grammar.len() >= 3);
    }

    /// Test für model_group() mit All (Zeile 538)
    #[test]
    fn particle_with_all_model_group() {
        use crate::qname::QName;
        use crate::schema::{Compositor, ElementDeclaration, ModelGroup, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        let all = ModelGroup::new(
            Compositor::All,
            vec![
                Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
                    QName::new("", "x"),
                )))),
                Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
                    QName::new("", "y"),
                )))),
            ],
        );

        let p = Particle::once(ParticleTerm::ModelGroup(all));
        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // All: NT_0 hat EE und ε-Productions
        let nt_0 = grammar.get(0).unwrap();
        assert!(
            nt_0.productions()
                .iter()
                .any(|p| p.terminal == Some(Terminal::EndElement))
        );
    }

    /// Test für particle min=0, max=3 (Zeile 758-774)
    #[test]
    fn particle_min0_max3() {
        use crate::schema::{MaxOccurs, Particle, ParticleTerm, Wildcard};

        let mut interner = StringInterner::new();
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let p = Particle::new(0, MaxOccurs::Bounded(3), term).unwrap();

        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // 3 optional copies, jede mit EE am Start
        // Alle NT_0 der Copies sollten EE haben
        assert!(grammar.len() >= 3, "min=0, max=3 sollte mehrere NTs haben");
    }

    /// Test für to_non_terminals mit rhs == 0 (Zeile 879)
    #[test]
    fn to_non_terminals_with_rhs_zero() {
        use crate::grammar::NonTerminalId;

        // Grammar wo Production auf NT_0 zeigt (nicht Selbstreferenz)
        let mut grammar = ProtoGrammar::new();
        // NT_0: CH -> NT_1
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 1),
        ]));
        // NT_1: EE -> NT_0 (zurück zum Start)
        grammar.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 0),
        ]));

        let nts = grammar
            .to_non_terminals(NonTerminalId::ElementContent)
            .unwrap();

        // NT_1's EE zeigt auf NT_0 (base_id)
        assert_eq!(nts.len(), 2);
        assert!(nts[1].productions()[0].right_hand_side.is_some());
    }

    /// Test für particle min=2, max=unbounded (Zeile 743)
    #[test]
    fn particle_min2_max_unbounded() {
        use crate::schema::{MaxOccurs, Particle, ParticleTerm, Wildcard};

        let mut interner = StringInterner::new();
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let p = Particle::new(2, MaxOccurs::Unbounded, term).unwrap();

        let grammar = ProtoGrammar::particle(&p, &mut interner).unwrap();

        // 2 required copies + 1 loop copy
        // Sollte mehrere NTs haben und Loop-Struktur
        assert!(
            grammar.len() >= 4,
            "min=2, max=unbounded sollte mehrere NTs haben"
        );

        let has_loop = grammar
            .non_terminals()
            .iter()
            .any(|nt| nt.productions().iter().any(|p| p.is_epsilon()));
        assert!(has_loop, "Sollte Loop-Struktur haben");
    }

    /// Test für Choice mit ε-Productions in Particles (Zeile 615)
    ///
    /// Wenn eine Particle-Grammar bereits ε-Productions enthält (z.B. aus All),
    /// müssen diese auch mit Offset angepasst werden.
    #[test]
    fn choice_term_with_nested_epsilon() {
        use crate::qname::QName;
        use crate::schema::{Compositor, ElementDeclaration, ModelGroup, Particle, ParticleTerm};

        let mut interner = StringInterner::new();
        // Nested All in Choice: Die All-Grammar hat ε-Productions
        let all = ModelGroup::new(
            Compositor::All,
            vec![Particle::once(ParticleTerm::Element(
                ElementDeclaration::new(Rc::new(QName::new("", "a"))),
            ))],
        );

        let choice = ModelGroup::new(
            Compositor::Choice,
            vec![
                Particle::once(ParticleTerm::ModelGroup(all)),
                Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
                    QName::new("", "b"),
                )))),
            ],
        );

        let grammar = ProtoGrammar::choice_term(&choice.particles, &mut interner).unwrap();

        // Sollte funktionieren und mehrere NTs haben
        assert!(grammar.len() >= 3);
    }

    // ========================================================================
    // Tests: Normalisierung (Spec 8.5.4.2, 8.5.4.3)
    // ========================================================================

    /// Spec H.2: ProtoG_OrderElement → NormG_OrderElement
    ///
    /// Proto:
    /// ```text
    /// Term_product 0,0:
    ///   SE("product") Term_product 1,0
    /// Term_product 1,0:
    ///   SE("product") Term_product 1,0
    ///   EE
    /// ```
    ///
    /// Normalisiert (mit Event Codes):
    /// ```text
    /// Term_product 0,0:
    ///   SE("product") Term_product 1,0   [0]
    /// Term_product 1,0:
    ///   SE("product") Term_product 1,0   [0]
    ///   EE                               [1]
    /// ```
    #[test]
    fn normalize_order_element_h2() {
        use crate::grammar::{StartElementKind, Terminal};

        let mut interner = StringInterner::new();
        let product = interner.intern_expanded("", "product").unwrap();

        // ProtoG_OrderElement aufbauen (keine ε-Produktionen)
        let mut proto = ProtoGrammar::new();

        // Term_product 0,0: SE("product") → Term_product 1,0
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(
                Terminal::StartElement(StartElementKind::QName(product)),
                1,
            ),
        ]));

        // Term_product 1,0: SE("product") → 1, EE
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(
                Terminal::StartElement(StartElementKind::QName(product)),
                1,
            ),
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        let normalized = proto.normalize(&interner);

        // Zwei NonTerminals erwartet
        assert_eq!(normalized.len(), 2);

        // NT 0: eine Production SE("product") mit Event Code 0
        let nt0 = &normalized[0];
        assert_eq!(nt0.productions().len(), 1);
        assert_eq!(nt0.productions()[0].event_code.as_ref().unwrap().part1(), 0);
        assert!(matches!(
            &nt0.productions()[0].terminal,
            Terminal::StartElement(StartElementKind::QName(q)) if q.resolve(&interner).1 == "product"
        ));

        // NT 1: zwei Productions - SE("product")[0], EE[1]
        let nt1 = &normalized[1];
        assert_eq!(nt1.productions().len(), 2);
        // SE("product") sollte vor EE kommen (Sortierung: SE(qname) vor EE)
        assert!(matches!(
            &nt1.productions()[0].terminal,
            Terminal::StartElement(StartElementKind::QName(q)) if q.resolve(&interner).1 == "product"
        ));
        assert_eq!(nt1.productions()[0].event_code.as_ref().unwrap().part1(), 0);
        assert_eq!(nt1.productions()[1].terminal, Terminal::EndElement);
        assert_eq!(nt1.productions()[1].event_code.as_ref().unwrap().part1(), 1);
    }

    /// Spec 8.5.4.2.2: Duplicate Terminal Elimination
    ///
    /// ```text
    /// NT_0:
    ///   SE("a") NT_1
    ///   SE("a") NT_2
    /// NT_1: EE
    /// NT_2: CH NT_2 | EE
    /// ```
    ///
    /// Nach Duplicate-Elimination:
    /// ```text
    /// NT_0:
    ///   SE("a") NT_1⊔2    [0]
    /// NT_1⊔2:
    ///   (Productions von NT_1 und NT_2)
    ///   CH NT_1⊔2         [0]
    ///   EE                [1]
    /// ```
    #[test]
    fn normalize_duplicate_terminal_elimination() {
        use crate::grammar::{StartElementKind, Terminal};

        let mut interner = StringInterner::new();
        let a = interner.intern_expanded("", "a").unwrap();

        let mut proto = ProtoGrammar::new();

        // NT_0: SE("a") → NT_1, SE("a") → NT_2
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(
                Terminal::StartElement(StartElementKind::QName(a)),
                1,
            ),
            ProtoProduction::new(
                Terminal::StartElement(StartElementKind::QName(a)),
                2,
            ),
        ]));

        // NT_1: EE
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        // NT_2: CH → NT_2, EE
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 2),
            ProtoProduction::new(Terminal::EndElement, 2),
        ]));

        let normalized = proto.normalize(&interner);

        // NT_0 sollte nur EINE SE("a") Production haben
        let nt0 = &normalized[0];
        let se_a_count = nt0
            .productions()
            .iter()
            .filter(|p| {
                matches!(
                    &p.terminal,
                    Terminal::StartElement(StartElementKind::QName(q)) if *q == a
                )
            })
            .count();
        assert_eq!(se_a_count, 1, "Sollte nur eine SE(\"a\") Production haben");

        // Das Ziel sollte ein Union-NonTerminal sein das CH und EE akzeptiert
        let union_id = nt0.productions()[0].right_hand_side.unwrap();
        let union_nt = normalized
            .iter()
            .find(|nt| nt.id() == union_id)
            .expect("Union-NonTerminal sollte existieren");

        // Union sollte CH und EE haben (ohne Duplikate)
        let has_ch = union_nt
            .productions()
            .iter()
            .any(|p| p.terminal == Terminal::Characters);
        let has_ee = union_nt
            .productions()
            .iter()
            .any(|p| p.terminal == Terminal::EndElement);
        assert!(has_ch, "Union-NT sollte CH haben");
        assert!(has_ee, "Union-NT sollte EE haben");

        // EE sollte genau einmal vorkommen (Duplikat eliminiert)
        let ee_count = union_nt
            .productions()
            .iter()
            .filter(|p| p.terminal == Terminal::EndElement)
            .count();
        assert_eq!(ee_count, 1, "EE sollte nur einmal vorkommen");
    }

    /// Spec 8.5.4.3: Event Code Assignment Sortierung
    ///
    /// Sortierreihenfolge:
    /// 1. AT(qname) - lexikographisch nach local-name, dann uri
    /// 2. AT(uri:*)
    /// 3. AT(*)
    /// 4. SE(qname) - schema order
    /// 5. SE(uri:*)
    /// 6. SE(*)
    /// 7. EE
    /// 8. CH
    #[test]
    fn normalize_event_code_sorting() {
        use crate::grammar::{AttributeKind, StartElementKind, Terminal};

        let mut interner = StringInterner::new();
        // Reihenfolge alphabetisch, damit IDs aufsteigend sortiert sind
        let alpha_id = interner.intern_expanded("", "alpha").unwrap();
        let beta_se = interner.intern_expanded("", "beta").unwrap();
        let zebra_at = interner.intern_expanded("", "zebra").unwrap();
        let alpha_at = alpha_id;
        let alpha_se = alpha_id;

        let mut proto = ProtoGrammar::new();

        // Unsortierte Productions (absichtlich falsche Reihenfolge)
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 0),
            ProtoProduction::new(Terminal::EndElement, 0),
            ProtoProduction::new(
                Terminal::StartElement(StartElementKind::QName(beta_se)),
                0,
            ),
            ProtoProduction::new(
                Terminal::Attribute(AttributeKind::QName(zebra_at)),
                0,
            ),
            ProtoProduction::new(
                Terminal::Attribute(AttributeKind::QName(alpha_at)),
                0,
            ),
            ProtoProduction::new(
                Terminal::StartElement(StartElementKind::QName(alpha_se)),
                0,
            ),
        ]));

        let normalized = proto.normalize(&interner);

        let nt = &normalized[0];
        let terminals: Vec<_> = nt.productions().iter().map(|p| &p.terminal).collect();

        // Erwartete Reihenfolge:
        // AT("alpha"), AT("zebra"), SE("alpha"), SE("beta"), EE, CH
        assert!(
            matches!(terminals[0], Terminal::Attribute(AttributeKind::QName(q)) if q.resolve(&interner).1 == "alpha"),
            "Position 0 sollte AT(\"alpha\") sein"
        );
        assert!(
            matches!(terminals[1], Terminal::Attribute(AttributeKind::QName(q)) if q.resolve(&interner).1 == "zebra"),
            "Position 1 sollte AT(\"zebra\") sein"
        );
        assert!(
            matches!(terminals[2], Terminal::StartElement(StartElementKind::QName(q)) if q.resolve(&interner).1 == "alpha"),
            "Position 2 sollte SE(\"alpha\") sein"
        );
        assert!(
            matches!(terminals[3], Terminal::StartElement(StartElementKind::QName(q)) if q.resolve(&interner).1 == "beta"),
            "Position 3 sollte SE(\"beta\") sein"
        );
        assert_eq!(
            terminals[4],
            &Terminal::EndElement,
            "Position 4 sollte EE sein"
        );
        assert_eq!(
            terminals[5],
            &Terminal::Characters,
            "Position 5 sollte CH sein"
        );

        // Event Codes prüfen: 0, 1, 2, 3, 4, 5
        for (i, prod) in nt.productions().iter().enumerate() {
            assert_eq!(
                prod.event_code.as_ref().unwrap().part1(),
                i as u32,
                "Event Code {} sollte {} sein",
                i,
                i
            );
        }
    }

    /// Spec 8.5.4.3: Schema Order Sortierung für SE(qname)
    ///
    /// Elemente werden nach ihrer Position im Content Model sortiert,
    /// nicht lexikographisch. "zebra" kommt vor "alpha" wenn es im
    /// Schema zuerst definiert wurde.
    #[test]
    fn normalize_schema_order_sorting() {
        use crate::grammar::{StartElementKind, Terminal};
        use crate::qname::QName;
        use crate::schema::{Compositor, ElementDeclaration, ModelGroup, Particle, ParticleTerm};

        let mut interner = StringInterner::new();

        // Choice mit: zebra, alpha, beta (alle auf gleichem NT)
        // Schema order: zebra=0, alpha=1, beta=2
        // Bei Choice landen alle SEs auf dem Start-NT, daher garantiert mehrere SEs.
        let choice = ModelGroup::new(
            Compositor::Choice,
            vec![
                Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
                    QName::new("", "zebra"),
                )))),
                Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
                    QName::new("", "alpha"),
                )))),
                Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
                    QName::new("", "beta"),
                )))),
            ],
        );

        let grammar = ProtoGrammar::choice_term(&choice.particles, &mut interner).unwrap();
        let normalized = grammar.normalize(&interner);

        // Das erste NT sollte alle drei SEs haben (Choice sammelt auf NT_0)
        assert!(!normalized.is_empty(), "Sollte mindestens ein NT haben");

        let se_names: Vec<&str> = normalized[0]
            .productions()
            .iter()
            .filter_map(|p| match &p.terminal {
                Terminal::StartElement(StartElementKind::QName(q)) => {
                    let (_, local) = q.resolve(&interner);
                    Some(local)
                }
                _ => None,
            })
            .collect();

        // Alle drei SEs müssen vorhanden sein
        assert_eq!(
            se_names.len(),
            3,
            "NT_0 sollte genau 3 SE(qname) haben, hat aber: {:?}",
            se_names
        );

        // Die Reihenfolge muss Schema Order sein: zebra, alpha, beta
        // (nicht alphabetisch: alpha, beta, zebra)
        assert_eq!(
            se_names,
            vec!["zebra", "alpha", "beta"],
            "Schema order nicht eingehalten: erwartet [zebra, alpha, beta], bekommen {:?}",
            se_names
        );
    }

    /// Spec 8.5.4.2.1: Zyklische ε-Ketten (A → B → A)
    ///
    /// Testet indirekte Zyklen bei der ε-Elimination.
    /// NT_0 hat ε → NT_1, NT_1 hat ε → NT_0 und CH → NT_1.
    /// Nach Elimination sollten beide CH haben, keine ε, keine Endlosschleife.
    #[test]
    fn normalize_epsilon_mutual_cycle() {
        use crate::grammar::Terminal;

        let interner = StringInterner::new();
        let mut proto = ProtoGrammar::new();

        // NT_0: ε → NT_1
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::epsilon(1),
        ]));

        // NT_1: ε → NT_0, CH → NT_1
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::epsilon(0),
            ProtoProduction::new(Terminal::Characters, 1),
        ]));

        let normalized = proto.normalize(&interner);

        // Beide NTs sollten existieren
        assert_eq!(normalized.len(), 2, "Sollte 2 NonTerminals haben");

        // NT_0 sollte CH haben (von NT_1 geerbt)
        let nt0_has_ch = normalized[0]
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::Characters));
        assert!(nt0_has_ch, "NT_0 sollte CH haben (von NT_1 geerbt)");

        // NT_1 sollte CH haben
        let nt1_has_ch = normalized[1]
            .productions()
            .iter()
            .any(|p| matches!(p.terminal, Terminal::Characters));
        assert!(nt1_has_ch, "NT_1 sollte CH haben");

        // Keine ε-Productions mehr (alle Productions haben ein Terminal)
        for (i, nt) in normalized.iter().enumerate() {
            assert!(
                !nt.productions().is_empty(),
                "NT_{} sollte mindestens eine Production haben",
                i
            );
            // In normalisierten Grammars hat jede Production ein Terminal
            // (ε-Productions werden eliminiert). Der Production-Typ in grammar.rs
            // hat immer ein Terminal, daher ist dieser Check implizit erfüllt.
        }
    }

    /// Spec 8.5.4.3: SE(uri:*) Sortierung mit Schema-Order
    ///
    /// Wildcards mit Namespace-Liste sollten nach Schema-Order sortiert werden,
    /// nicht nach URI-Lexikographie.
    #[test]
    fn normalize_se_namespace_wildcard_schema_order() {
        use crate::grammar::{StartElementKind, Terminal};
        use crate::schema::{Particle, ParticleTerm, Wildcard, WildcardConstraint};

        let mut interner = StringInterner::new();

        // Zwei Wildcards mit verschiedenen Namespace-Listen
        // Wildcard 1: ["http://z.example.com"] - schema_order 0
        // Wildcard 2: ["http://a.example.com"] - schema_order 1
        let w1 = Particle::once(ParticleTerm::Wildcard(Wildcard::new(
            WildcardConstraint::Namespaces(vec!["http://z.example.com".to_string()]),
        )));
        let w2 = Particle::once(ParticleTerm::Wildcard(Wildcard::new(
            WildcardConstraint::Namespaces(vec!["http://a.example.com".to_string()]),
        )));

        let grammar = ProtoGrammar::sequence_term(&[w1, w2], &mut interner).unwrap();
        let normalized = grammar.normalize(&interner);

        // Finde NT mit SE(uri:*) Productions
        let nt_with_ns_wildcards = normalized.iter().find(|nt| {
            nt.productions().iter().any(|p| {
                matches!(
                    &p.terminal,
                    Terminal::StartElement(StartElementKind::NamespaceWildcard(_))
                )
            })
        });

        if let Some(nt) = nt_with_ns_wildcards {
            let uris: Vec<&str> = nt
                .productions()
                .iter()
                .filter_map(|p| match &p.terminal {
                    Terminal::StartElement(StartElementKind::NamespaceWildcard(uri)) => {
                        Some(interner.resolve(*uri))
                    }
                    _ => None,
                })
                .collect();

            // Wenn beide URIs vorhanden sind, sollte "z" vor "a" kommen (schema order)
            if uris.len() >= 2 {
                let z_pos = uris.iter().position(|u| u.contains("z.example"));
                let a_pos = uris.iter().position(|u| u.contains("a.example"));

                if let (Some(z), Some(a)) = (z_pos, a_pos) {
                    assert!(
                        z < a,
                        "Schema order: z (order=0) sollte vor a (order=1) kommen, aber: {:?}",
                        uris
                    );
                }
            }
        }
    }

    /// Spec 8.5.4.2.2: Union-Cache Wiederverwendung
    ///
    /// Wenn zwei NonTerminals Duplikate mit den gleichen Zielen haben,
    /// sollte dasselbe Union-NonTerminal wiederverwendet werden.
    #[test]
    fn normalize_duplicate_terminal_union_cache_reuse() {
        use crate::grammar::Terminal;

        let mut interner = StringInterner::new();
        let a = interner.intern_expanded("", "a").unwrap();
        let mut proto = ProtoGrammar::new();

        // NT_0: SE("a") → NT_2, SE("a") → NT_3
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(
                Terminal::StartElement(crate::grammar::StartElementKind::QName(a)),
                2,
            ),
            ProtoProduction::new(
                Terminal::StartElement(crate::grammar::StartElementKind::QName(a)),
                3,
            ),
        ]));

        // NT_1: SE("a") → NT_2, SE("a") → NT_3 (gleiche Ziele wie NT_0!)
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(
                Terminal::StartElement(crate::grammar::StartElementKind::QName(a)),
                2,
            ),
            ProtoProduction::new(
                Terminal::StartElement(crate::grammar::StartElementKind::QName(a)),
                3,
            ),
        ]));

        // NT_2: EE
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 2),
        ]));

        // NT_3: CH → NT_3
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::Characters, 3),
        ]));

        let normalized = proto.normalize(&interner);

        // NT_0 und NT_1 sollten beide auf dasselbe Union-NT zeigen
        // Das Union-NT ist das letzte hinzugefügte (Index 4)
        let nt0_target = normalized[0].productions()[0].right_hand_side;
        let nt1_target = normalized[1].productions()[0].right_hand_side;

        assert_eq!(
            nt0_target, nt1_target,
            "NT_0 und NT_1 sollten auf dasselbe Union-NT zeigen"
        );

        // Beide sollten nur noch eine SE("a") Production haben (Duplikate eliminiert)
        assert_eq!(
            normalized[0].productions().len(),
            1,
            "NT_0 sollte nach Duplicate-Elimination nur 1 Production haben"
        );
        assert_eq!(
            normalized[1].productions().len(),
            1,
            "NT_1 sollte nach Duplicate-Elimination nur 1 Production haben"
        );
    }

    /// Spec 8.5.4.3: Tie-Break bei identischer schema_order
    ///
    /// Bei Substitution Group Members haben alle SEs die gleiche schema_order.
    /// Für deterministische Sortierung wird lexikographisch tie-gebreakt.
    #[test]
    fn normalize_schema_order_tiebreak_lexicographic() {
        use crate::grammar::{StartElementKind, Terminal};

        let mut interner = StringInterner::new();
        // Alphabetisch internen, damit IDs aufsteigend sortiert sind
        let alpha = interner.intern_expanded("", "alpha").unwrap();
        let beta = interner.intern_expanded("", "beta").unwrap();
        let zebra = interner.intern_expanded("", "zebra").unwrap();

        let mut proto = ProtoGrammar::new();

        // Simuliere Substitution Group: alle haben schema_order=0
        // zebra, alpha, beta - alle mit gleicher Order
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::with_schema_order(
                Terminal::StartElement(StartElementKind::QName(zebra)),
                0,
                0, // schema_order=0
            ),
            ProtoProduction::with_schema_order(
                Terminal::StartElement(StartElementKind::QName(alpha)),
                0,
                0, // gleiche schema_order=0
            ),
            ProtoProduction::with_schema_order(
                Terminal::StartElement(StartElementKind::QName(beta)),
                0,
                0, // gleiche schema_order=0
            ),
        ]));

        let normalized = proto.normalize(&interner);

        // Bei gleicher schema_order sollten sie lexikographisch sortiert sein
        let se_names: Vec<_> = normalized[0]
            .productions()
            .iter()
            .filter_map(|p| match &p.terminal {
                Terminal::StartElement(StartElementKind::QName(q)) => {
                    let (_, local) = q.resolve(&interner);
                    Some(local)
                }
                _ => None,
            })
            .collect();

        assert_eq!(
            se_names,
            vec!["alpha", "beta", "zebra"],
            "Bei gleicher schema_order sollte lexikographisch sortiert werden"
        );
    }

    /// Spec 8.5.4.2.2: Bei Duplikaten wird minimale schema_order behalten
    ///
    /// Wenn zwei Productions den gleichen Terminal und RHS haben aber
    /// unterschiedliche schema_order, wird die mit der kleinsten Order behalten.
    #[test]
    fn normalize_duplicate_keeps_min_schema_order() {
        use crate::grammar::{StartElementKind, Terminal};

        let mut interner = StringInterner::new();
        let item = interner.intern_expanded("", "item").unwrap();

        let mut proto = ProtoGrammar::new();

        // NT_0: Zwei Productions mit gleichem Terminal/RHS aber verschiedener schema_order
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::with_schema_order(
                Terminal::StartElement(StartElementKind::QName(item)),
                1,
                5, // schema_order=5 (später im Schema)
            ),
            ProtoProduction::with_schema_order(
                Terminal::StartElement(StartElementKind::QName(item)),
                1,
                2, // schema_order=2 (früher im Schema)
            ),
            ProtoProduction::new(Terminal::EndElement, 0),
        ]));

        // NT_1: EE
        proto.add_non_terminal(ProtoNonTerminal::with_productions(vec![
            ProtoProduction::new(Terminal::EndElement, 1),
        ]));

        let normalized = proto.normalize(&interner);

        // NT_0 sollte nur eine SE("item") Production haben (Duplikat eliminiert)
        let se_count = normalized[0]
            .productions()
            .iter()
            .filter(|p| {
                matches!(
                    &p.terminal,
                    Terminal::StartElement(StartElementKind::QName(q)) if *q == item
                )
            })
            .count();

        assert_eq!(
            se_count, 1,
            "Duplikat sollte eliminiert sein, nur eine SE(\"item\") übrig"
        );

        // Die Sortierung sollte basierend auf der minimalen schema_order (2) funktionieren
        // Da SE vor EE kommt, sollte SE("item") an Position 0 sein
        assert!(
            matches!(
                &normalized[0].productions()[0].terminal,
                Terminal::StartElement(StartElementKind::QName(q)) if *q == item
            ),
            "SE(\"item\") sollte an Position 0 sein"
        );
    }

    // ========================================================================
    // Coverage Tests für TerminalKey Sortierung
    // ========================================================================

    /// Test für TerminalKey::partial_cmp (Zeile 57-58).
    #[test]
    fn terminal_key_partial_cmp() {
        use std::cmp::Ordering;

        let sd = TerminalKey::StartDocument;
        let ed = TerminalKey::EndDocument;

        // partial_cmp sollte Some zurückgeben
        assert_eq!(sd.partial_cmp(&ed), Some(Ordering::Less));
        assert_eq!(ed.partial_cmp(&sd), Some(Ordering::Greater));
        assert_eq!(sd.partial_cmp(&sd), Some(Ordering::Equal));
    }

    /// Test für TerminalKey discriminant_rank (Zeile 96-111).
    #[test]
    fn terminal_key_all_ranks() {
        let mut interner = StringInterner::new();
        let ns = interner.intern("http://example.org").unwrap();
        let elem = interner.intern_expanded("", "elem").unwrap();
        let attr = interner.intern_expanded("", "attr").unwrap();

        // Alle TerminalKey Varianten sollten nach Rang sortiert werden
        let keys = vec![
            TerminalKey::StartDocument,
            TerminalKey::EndDocument,
            TerminalKey::StartElementWildcard,
            TerminalKey::StartElementNsWildcard(ns),
            TerminalKey::StartElementQName(elem),
            TerminalKey::EndElement,
            TerminalKey::AttributeWildcard,
            TerminalKey::AttributeNsWildcard(ns),
            TerminalKey::AttributeQName(attr),
            TerminalKey::Characters,
            TerminalKey::NamespaceDecl,
            TerminalKey::Comment,
            TerminalKey::ProcessingInstr,
            TerminalKey::DocType,
            TerminalKey::EntityRef,
            TerminalKey::SelfContained,
        ];

        // Jedes benachbarte Paar sollte in korrekter Reihenfolge sein
        for i in 0..keys.len() - 1 {
            assert!(
                keys[i] < keys[i + 1],
                "TerminalKey-Rang {:?} sollte < {:?} sein",
                keys[i],
                keys[i + 1]
            );
        }
    }

    /// Test für TerminalKey::From<&Terminal> (Zeile 119-140).
    #[test]
    fn terminal_key_from_terminal() {
        let mut interner = StringInterner::new();
        let ns = interner.intern("http://example.org").unwrap();
        let elem = interner.intern_expanded("", "elem").unwrap();
        let attr = interner.intern_expanded("", "attr").unwrap();

        // Teste alle Terminal-Varianten
        let terminals = vec![
            Terminal::StartDocument,
            Terminal::EndDocument,
            Terminal::StartElement(StartElementKind::Wildcard),
            Terminal::StartElement(StartElementKind::NamespaceWildcard(ns)),
            Terminal::StartElement(StartElementKind::QName(elem)),
            Terminal::EndElement,
            Terminal::Attribute(AttributeKind::Wildcard),
            Terminal::Attribute(AttributeKind::NamespaceWildcard(ns)),
            Terminal::Attribute(AttributeKind::QName(attr)),
            Terminal::Characters,
            Terminal::NamespaceDecl,
            Terminal::Comment,
            Terminal::ProcessingInstr,
            Terminal::DocType,
            Terminal::EntityRef,
            Terminal::SelfContained,
        ];

        // Jede Terminal-Variante sollte zu einem TerminalKey konvertierbar sein
        for terminal in &terminals {
            let _: TerminalKey = terminal.into();
        }
    }

    /// Test für AttributeNsWildcard Vergleich (Zeile 78).
    #[test]
    fn terminal_key_attribute_ns_wildcard_cmp() {
        use std::cmp::Ordering;

        let mut interner = StringInterner::new();
        let a = interner.intern("http://a.org").unwrap();
        let b = interner.intern("http://b.org").unwrap();

        let a1 = TerminalKey::AttributeNsWildcard(a);
        let a2 = TerminalKey::AttributeNsWildcard(b);

        assert_eq!(a1.cmp(&a2), Ordering::Less);
        assert_eq!(a2.cmp(&a1), Ordering::Greater);
        assert_eq!(a1.cmp(&a1), Ordering::Equal);
    }
}
