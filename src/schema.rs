//! Minimales Schema-Datenmodell für Grammar-Generierung (Spec 8.5).
//!
//! Dieses Modul stellt `SchemaInfo` bereit, das die QNames aus einem Schema
//! enthält, die für die Generierung von Schema-informed Grammars benötigt werden.
//!
//! Vollständiges XSD-Parsing ist nicht im Scope dieses Moduls (siehe Issue #34).
//!
//! # Spec-Referenz
//!
//! - 8.5.1 Schema-informed Document Grammar
//! - 8.5.2 Schema-informed Fragment Grammar
//! - 8.5.3 Schema-informed Element Fragment Grammar
//! - 8.5.4.1.3 Type Grammars
//! - 8.5.4.1.4 Attribute Uses
//! - 8.5.4.1.5 Particles
//! - 8.5.4.1.6 Element Terms
//! - 8.5.4.1.7 Wildcard Terms
//! - 8.5.4.1.8 Model Group Terms

use std::rc::Rc;

use crate::Result;
use crate::qname::QName;

// ============================================================================
// Derivation Kind (XSD 1.0 Part 1 §3.4.6)
// ============================================================================

/// Art der Typableitung für Complex Types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DerivationKind {
    /// Extension erweitert den Base-Typ um zusätzliche Inhalte/Attribute.
    Extension,
    /// Restriction schränkt den Base-Typ ein.
    Restriction,
}

// ============================================================================
// Simple Type Variety (XSD 1.0 Part 2 §4.1)
// ============================================================================

/// Variety eines Simple Types (XSD 1.0 Part 2 §4.1).
#[derive(Debug, Clone, PartialEq)]
#[derive(Default)]
pub enum SimpleTypeVariety {
    /// Atomic: direkter Wert eines Built-in Typs.
    #[default]
    Atomic,
    /// List: Whitespace-separierte Liste von Werten.
    /// Der itemType ist der Typ der einzelnen Listenelemente.
    List {
        /// QName des Item-Typs.
        item_type: Option<Rc<QName>>,
    },
    /// Union: Wert kann einem von mehreren Member-Typen entsprechen.
    Union {
        /// QNames der Member-Typen.
        member_types: Vec<Rc<QName>>,
    },
}


// ============================================================================
// Type Definition (Spec 8.5.4.1.3)
// ============================================================================

/// Minimale Type-Definition für Proto-Grammar-Generierung (Spec 8.5.4.1.3).
///
/// Dieses Datenmodell ist ein Stub für die Proto-Grammar-Generierung.
/// Vollständiges XSD-Parsing erfolgt in Issue #34.
///
/// # Spec-Referenz
///
/// - 8.5.4.1.3.1 Simple Type Grammars
/// - 8.5.4.1.3.2 Complex Type Grammars
/// - 8.5.4.4.2 Adding Productions when Strict is True
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    /// Simple Type: nur CH-Content (Spec 8.5.4.1.3.1).
    ///
    /// Erzeugt Grammar:
    /// ```text
    /// Type_i,0 : CH [schema-typed value] Type_i,1
    /// Type_i,1 : EE
    /// ```
    Simple {
        /// QName des Typs (Some für globale/benannte Typen, None für anonyme).
        name: Option<Rc<QName>>,
        /// Variety des Simple Types (XSD 1.0 Part 2 §4.1).
        ///
        /// Atomic, List oder Union. List-Typen haben einen itemType,
        /// Union-Typen haben memberTypes.
        variety: SimpleTypeVariety,
        /// Base-Type QName für Typableitung (Finding 5).
        ///
        /// Auch für anonyme Typen gespeichert um transitive Base-Resolution
        /// zu ermöglichen.
        base_type_qname: Option<Rc<QName>>,
        /// Basis-Datentyp für EXI typed-value Encoding (Spec 7.1).
        ///
        /// Enthält den ultimativen XSD built-in type (z.B. "string", "int").
        /// Benötigt für 8.5.4.1.3 Type Grammars um den korrekten Codec zu wählen.
        base_type: Option<String>,
        /// Enumerationswerte fuer Restricted Character Sets (Spec 7.1.10.1).
        enumeration_values: Vec<String>,
        /// Ob dieser Simple Type eine Union ist (Spec 8.5.4.4.2).
        ///
        /// Bei Union-Typen wird xsi:type auch bei strict=true hinzugefügt.
        is_union: bool,
        /// Ob dieser Typ benannte Sub-Typen hat (Spec 8.5.4.4.2).
        ///
        /// Auch SimpleTypes können Sub-Typen haben (durch xs:restriction).
        /// Bei Typen mit benannten Sub-Typen wird xsi:type auch bei strict=true
        /// hinzugefügt.
        has_named_sub_types: bool,
    },
    /// Complex Type: Attribute + Content (Spec 8.5.4.1.3.2).
    Complex {
        /// QName des Typs (Some für globale/benannte Typen, None für anonyme).
        name: Option<Rc<QName>>,
        /// Base-Type QName bei Ableitung (Finding 1 & 2).
        ///
        /// Für Extension/Restriction und simpleContent wird der Base-Type
        /// hier gespeichert, um Content/Attribute zu mergen.
        base_type: Option<Rc<QName>>,
        /// Art der Ableitung (Extension oder Restriction).
        derivation: Option<DerivationKind>,
        /// Attribute Uses, sortiert nach QName (local-name, dann URI).
        attributes: Vec<AttributeUse>,
        /// Attribute Wildcard (None, Any, oder Namespace-Liste).
        attribute_wildcard: Option<AttributeWildcard>,
        /// Content Type.
        content: ContentType,
        /// Ob dieser Typ benannte Sub-Typen hat (Spec 8.5.4.4.2).
        ///
        /// Bei Typen mit benannten Sub-Typen wird xsi:type auch bei strict=true
        /// hinzugefügt.
        has_named_sub_types: bool,
    },
}

impl TypeDefinition {
    /// Gibt den QName des Typs zurück (None für anonyme Typen).
    pub fn name(&self) -> Option<&Rc<QName>> {
        match self {
            TypeDefinition::Simple { name, .. } => name.as_ref(),
            TypeDefinition::Complex { name, .. } => name.as_ref(),
        }
    }

    /// Prüft ob dies ein Simple Type ist.
    pub fn is_simple(&self) -> bool {
        matches!(self, TypeDefinition::Simple { .. })
    }

    /// Prüft ob dies ein Complex Type ist.
    pub fn is_complex(&self) -> bool {
        matches!(self, TypeDefinition::Complex { .. })
    }

    /// Prüft ob dieser Typ eine Union ist (nur bei Simple Types relevant).
    ///
    /// # Spec 8.5.4.4.2
    ///
    /// Bei Union-Typen wird AT(xsi:type) auch bei strict=true hinzugefügt.
    pub fn is_union(&self) -> bool {
        match self {
            TypeDefinition::Simple { is_union, .. } => *is_union,
            TypeDefinition::Complex { .. } => false,
        }
    }

    /// Prüft ob dieser Typ benannte Sub-Typen hat.
    ///
    /// # Spec 8.5.4.4.2
    ///
    /// Bei Typen mit benannten Sub-Typen wird AT(xsi:type) auch bei strict=true
    /// hinzugefügt. Dies gilt sowohl für Complex Types als auch für Simple Types
    /// (z.B. durch xs:restriction abgeleitete Typen).
    pub fn has_named_sub_types(&self) -> bool {
        match self {
            TypeDefinition::Simple { has_named_sub_types, .. }
            | TypeDefinition::Complex { has_named_sub_types, .. } => *has_named_sub_types,
        }
    }

    /// Gibt die Enumeration-Facet-Werte zurück (Spec 7.2).
    pub fn enumeration_values(&self) -> &[String] {
        match self {
            TypeDefinition::Simple { enumeration_values, .. } => enumeration_values,
            TypeDefinition::Complex { .. } => &[],
        }
    }

    /// Klont diesen Typ und setzt `has_named_sub_types` auf `true`.
    pub fn with_named_sub_types(&self) -> Self {
        match self {
            TypeDefinition::Simple {
                name, variety, base_type_qname, base_type,
                enumeration_values, is_union, ..
            } => TypeDefinition::Simple {
                name: name.clone(),
                variety: variety.clone(),
                base_type_qname: base_type_qname.clone(),
                base_type: base_type.clone(),
                enumeration_values: enumeration_values.clone(),
                is_union: *is_union,
                has_named_sub_types: true,
            },
            TypeDefinition::Complex {
                name, base_type, derivation, attributes,
                attribute_wildcard, content, ..
            } => TypeDefinition::Complex {
                name: name.clone(),
                base_type: base_type.clone(),
                derivation: derivation.clone(),
                attributes: attributes.clone(),
                attribute_wildcard: attribute_wildcard.clone(),
                content: content.clone(),
                has_named_sub_types: true,
            },
        }
    }

    /// Gibt den QName des Base-Typs zurück (für DTRM Closest Ancestor Lookup).
    ///
    /// # Spec 7.4
    ///
    /// Für die DTRM-Auflösung muss die base_type-Kette traversiert werden,
    /// um den nächsten Vorfahren mit DTRM-Mapping zu finden.
    pub fn base_type_qname(&self) -> Option<&Rc<QName>> {
        match self {
            TypeDefinition::Simple { base_type_qname, .. } => base_type_qname.as_ref(),
            TypeDefinition::Complex { base_type, .. } => base_type.as_ref(),
        }
    }

    /// Gibt die Attribute Wildcard dieses Typs zurück (nur für Complex Types).
    pub fn attribute_wildcard(&self) -> Option<&AttributeWildcard> {
        match self {
            TypeDefinition::Complex {
                attribute_wildcard, ..
            } => attribute_wildcard.as_ref(),
            _ => None,
        }
    }

    /// Findet den Typ eines Attributs basierend auf QName.
    ///
    /// Nur für Complex Types relevant - Simple Types haben keine Attribute.
    ///
    /// # Spec-Referenz
    /// - Spec 8.5.4.1.4: Attribute Uses
    pub fn get_attribute_type(&self, attr_qname: &QName) -> Option<&Rc<TypeDefinition>> {
        match self {
            TypeDefinition::Complex { attributes, .. } => {
                attributes
                    .iter()
                    .find(|au| au.qname.as_ref() == attr_qname)
                    .and_then(|au| au.type_definition.as_ref())
            }
            TypeDefinition::Simple { .. } => None,
        }
    }

    /// Erstellt einen einfachen Simple Type (nicht-Union).
    pub fn simple() -> Self {
        TypeDefinition::Simple {
            name: None,
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: None,
            enumeration_values: Vec::new(),
            is_union: false,
            has_named_sub_types: false,
        }
    }

    /// Erstellt einen Simple Type mit Basis-Datentyp.
    pub fn simple_with_base(base_type: impl Into<String>) -> Self {
        TypeDefinition::Simple {
            name: None,
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: Some(base_type.into()),
            enumeration_values: Vec::new(),
            is_union: false,
            has_named_sub_types: false,
        }
    }

    /// Erstellt einen Simple Type der eine Union ist.
    pub fn simple_union() -> Self {
        TypeDefinition::Simple {
            name: None,
            variety: SimpleTypeVariety::Union { member_types: Vec::new() },
            base_type_qname: None,
            base_type: None,
            enumeration_values: Vec::new(),
            is_union: true,
            has_named_sub_types: false,
        }
    }

    /// Erstellt einen benannten Simple Type.
    pub fn simple_named(name: Rc<QName>) -> Self {
        TypeDefinition::Simple {
            name: Some(name),
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: None,
            enumeration_values: Vec::new(),
            is_union: false,
            has_named_sub_types: false,
        }
    }

    /// Erstellt einen leeren Complex Type.
    pub fn complex_empty() -> Self {
        TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: Vec::new(),
            attribute_wildcard: None,
            content: ContentType::Empty,
            has_named_sub_types: false,
        }
    }

    /// Erstellt einen Complex Type mit benannten Sub-Typen.
    pub fn complex_with_sub_types() -> Self {
        TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: Vec::new(),
            attribute_wildcard: None,
            content: ContentType::Empty,
            has_named_sub_types: true,
        }
    }
}

// ============================================================================
// Attribute Use (Spec 8.5.4.1.4)
// ============================================================================

/// Attribute Use für Complex Type Grammars (Spec 8.5.4.1.4).
///
/// Erzeugt Grammar:
/// ```text
/// Attribute_i,0 : AT(qname) [schema-typed value] Attribute_i,1
/// Attribute_i,1 : EE
///
/// // Falls optional (required = false):
/// Attribute_i,0 : EE
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct AttributeUse {
    /// QName des Attributs.
    pub qname: Rc<QName>,
    /// Ob das Attribut required ist (Spec 8.5.4.1.4).
    ///
    /// Falls false, wird eine zusätzliche EE-Production erzeugt.
    pub required: bool,
    /// Type Definition des Attributs (für typed-value Encoding).
    ///
    /// None für anySimpleType oder wenn der Typ nicht aufgelöst werden konnte.
    pub type_definition: Option<Rc<TypeDefinition>>,
}

// ============================================================================
// Attribute Wildcard (Spec 8.5.4.1.3.2)
// ============================================================================

/// Attribute Wildcard für Complex Type Grammars (Spec 8.5.4.1.3.2).
///
/// Wird zu bestehenden Attribute-Grammars hinzugefügt:
/// ```text
/// // Any:
/// G_i,0 : AT(*) G_i,0
///
/// // Namespaces:
/// G_i,0 : AT(uri_x:*) G_i,0  // für jede URI
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeWildcard {
    /// ##any - matched jeden Attribut-Namen.
    Any,
    /// ##other - matched Attribute die NICHT im angegebenen Namespace sind.
    ///
    /// Some(ns) = nicht in diesem Namespace (typischerweise targetNamespace).
    Not(Option<String>),
    /// Namespace-Liste - matched Attribute mit einer dieser URIs.
    ///
    /// Leerer String "" repräsentiert "absent" (kein Namespace).
    Namespaces(Vec<String>),
}

// ============================================================================
// Content Type (Spec 8.5.4.1.3.2)
// ============================================================================

/// Content Type für Complex Type Grammars (Spec 8.5.4.1.3.2).
///
/// # Hinweis
///
/// `ElementOnly` und `Mixed` enthalten ein `Particle` für das Content Model.
/// Dadurch ist `ContentType` nicht mehr `Copy`.
#[derive(Debug, Clone, PartialEq)]
pub enum ContentType {
    /// Empty Content - nur EE.
    Empty,
    /// Simple Content - Simple Type Grammar (CH + EE).
    Simple,
    /// Element-Only Content - Particle definiert das Content Model (Spec 8.5.4.1.5).
    ElementOnly(Particle),
    /// Mixed Content - Particle + CH an jeder Stelle (Spec 8.5.4.1.3.2).
    Mixed(Particle),
}

// ============================================================================
// MaxOccurs (Spec 8.5.4.1.5)
// ============================================================================

/// MaxOccurs Constraint für Particles (Spec 8.5.4.1.5).
///
/// Repräsentiert die obere Grenze für Wiederholungen eines Particles.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MaxOccurs {
    /// Endliche Obergrenze.
    Bounded(usize),
    /// Unbegrenzte Wiederholungen ({max occurs} = unbounded).
    Unbounded,
}

// ============================================================================
// WildcardConstraint (Spec 8.5.4.1.7)
// ============================================================================

/// Namespace Constraint für Wildcards (Spec 8.5.4.1.7).
///
/// Bestimmt welche Namespaces ein Wildcard-Term matchen kann.
///
/// processContents Attribut für Wildcards (XSD 1.0 Part 1 §3.10).
///
/// Finding 4: processContents beeinflusst die Validierung und Grammar-Generierung.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ProcessContents {
    /// strict (default): Element/Attribut muss validiert werden
    #[default]
    Strict,
    /// lax: Validierung wenn möglich, sonst skip
    Lax,
    /// skip: Keine Validierung
    Skip,
}

/// # Spec 8.5.4.1.7
///
/// - `Any`: "{namespace constraint} is any" → SE(*)
/// - `Not`: "{namespace constraint} is a pair of not and either a namespace name
///   or the special value absent" → SE(*)
/// - `Namespaces`: "{namespace constraint} is a set of values whose members are
///   namespace names or the special value absent" → SE(uri:*) für jede URI
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WildcardConstraint {
    /// ##any - matched jeden Element-Namen.
    Any,
    /// ##other (not) - matched alle außer dem angegebenen Namespace.
    ///
    /// `None` = absent (kein Namespace).
    /// Erzeugt SE(*) wie `Any`.
    Not(Option<String>),
    /// Explizite Namespace-Liste - matched Elemente mit einer dieser URIs.
    ///
    /// Leerer String "" repräsentiert "absent" (kein Namespace).
    /// Erzeugt SE(uri:*) für jede URI.
    Namespaces(Vec<String>),
}

/// Wildcard mit Namespace-Constraint und processContents (XSD 1.0 Part 1 §3.10).
///
/// Finding 4: processContents wird jetzt geparst und gespeichert.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Wildcard {
    /// Namespace-Constraint (##any, ##other, oder Liste).
    pub constraint: WildcardConstraint,
    /// processContents (strict, lax, skip). Default: strict.
    pub process_contents: ProcessContents,
}

impl Wildcard {
    /// Erstellt eine Wildcard mit gegebener Constraint und default processContents (strict).
    pub fn new(constraint: WildcardConstraint) -> Self {
        Self {
            constraint,
            process_contents: ProcessContents::default(),
        }
    }

    /// Erstellt eine ##any Wildcard (strict).
    pub fn any() -> Self {
        Self::new(WildcardConstraint::Any)
    }
}

// ============================================================================
// ElementDeclaration (Spec 8.5.4.1.6)
// ============================================================================

/// Element Declaration für Element Terms (Spec 8.5.4.1.6).
///
/// Repräsentiert ein Element mit optionaler Substitution Group.
///
/// # Spec 8.5.4.1.6
///
/// "Given a particle {term} PT_i that is an XML Schema element declaration
/// with properties {name}, {substitution group affiliation} and {target namespace}..."
///
/// Die Substitution Group enthält alle Elemente E_0, E_1, ..., E_{n-1} wobei
/// E_0 das Element selbst ist.
///
/// # Spec 8.5.4.4
///
/// Für Undeclared Productions werden zusätzliche Properties benötigt:
/// - `nillable`: Für xsi:nil Production bei strict=true (8.5.4.4.2)
/// - `type_definition`: Für xsi:type Production bei strict=true (8.5.4.4.2)
#[derive(Debug, Clone, PartialEq)]
pub struct ElementDeclaration {
    /// QName des Elements (E_0).
    pub(crate) qname: Rc<QName>,
    /// Substitution Group Members: E_1, ..., E_{n-1} (ohne das Element selbst E_0).
    ///
    /// Wenn leer, wird nur das Element selbst (E_0 = `qname`) verwendet.
    pub(crate) substitution_group: Vec<Rc<QName>>,
    /// Vorsortierte matching QNames (Head + Members), sortiert nach
    /// local-name dann URI (Spec 8.5.4.1.6). Einmal bei Konstruktion
    /// berechnet, danach allokationsfrei über Iterator zugänglich.
    sorted_matching_qnames: Vec<Rc<QName>>,
    /// Ob das Element nillable ist (Spec 8.5.4.4.2).
    ///
    /// Bei nillable=true wird AT(xsi:nil) auch bei strict=true hinzugefügt.
    pub(crate) nillable: bool,
    /// Die Type-Definition des Elements (Spec 8.5.4.4.2).
    ///
    /// Wird benötigt um zu prüfen ob xsi:type bei strict=true hinzugefügt
    /// werden soll (has_named_sub_types oder is_union).
    pub(crate) type_definition: Option<Rc<TypeDefinition>>,
}

impl ElementDeclaration {
    /// Erstellt eine ElementDeclaration ohne Substitution Group.
    pub fn new(qname: Rc<QName>) -> Self {
        let sorted = vec![Rc::clone(&qname)];
        Self {
            qname,
            substitution_group: Vec::new(),
            sorted_matching_qnames: sorted,
            nillable: false,
            type_definition: None,
        }
    }

    /// Erstellt eine ElementDeclaration mit Substitution Group.
    ///
    /// `substitution_group` enthält E_1..E_{n-1} (OHNE das Element selbst).
    /// E_0 (qname) wird automatisch von `matching_qnames()` eingefügt.
    pub fn with_substitution_group(qname: Rc<QName>, substitution_group: Vec<Rc<QName>>) -> Self {
        let mut decl = Self {
            qname,
            substitution_group,
            sorted_matching_qnames: Vec::new(),
            nillable: false,
            type_definition: None,
        };
        decl.finalize_matching_qnames();
        decl
    }

    /// Builder: Setzt nillable.
    pub fn with_nillable(mut self, nillable: bool) -> Self {
        self.nillable = nillable;
        self
    }

    /// Builder: Setzt type_definition.
    pub fn with_type(mut self, type_def: Rc<TypeDefinition>) -> Self {
        self.type_definition = Some(type_def);
        self
    }

    /// Gibt den QName des Elements zurück.
    pub fn qname(&self) -> &Rc<QName> {
        &self.qname
    }

    /// Gibt die Substitution Group zurück.
    pub fn substitution_group(&self) -> &[Rc<QName>] {
        &self.substitution_group
    }

    /// Gibt zurück ob das Element nillable ist.
    pub fn nillable(&self) -> bool {
        self.nillable
    }

    /// Gibt die Type-Definition des Elements zurück.
    pub fn type_definition(&self) -> Option<&Rc<TypeDefinition>> {
        self.type_definition.as_ref()
    }

    /// Gibt alle QNames zurück die dieses Element matchen können.
    ///
    /// Falls Substitution Group leer: nur das Element selbst.
    /// Sonst: das Element selbst plus alle Elemente der Substitution Group,
    /// sortiert nach local-name, dann URI (Spec 8.5.4.1.6).
    ///
    /// Die Liste ist bei Konstruktion vorberechnet, damit `matching_qnames()`
    /// ohne erneute Allokation oder Sortierung auskommt.
    pub fn matching_qnames(&self) -> impl Iterator<Item = &Rc<QName>> + '_ {
        debug_assert_eq!(
            self.sorted_matching_qnames.len(),
            1 + self.substitution_group.len(),
            "sorted_matching_qnames out of sync — finalize_matching_qnames() vergessen"
        );
        self.sorted_matching_qnames.iter()
    }

    /// Berechnet die vorsortierte Liste aller matching QNames.
    ///
    /// Wird nach Änderungen an der Substitution Group aufgerufen
    /// (z.B. in `with_substitution_group()` oder nach XSD-Parsing).
    pub(crate) fn finalize_matching_qnames(&mut self) {
        let mut all: Vec<Rc<QName>> =
            std::iter::once(Rc::clone(&self.qname))
                .chain(self.substitution_group.iter().cloned())
                .collect();
        all.sort_by(|a, b| {
            a.local_name
                .cmp(&b.local_name)
                .then_with(|| a.uri.cmp(&b.uri))
        });
        self.sorted_matching_qnames = all;
    }
}

// ============================================================================
// Compositor (Spec 8.5.4.1.8)
// ============================================================================

/// Model Group Compositor (Spec 8.5.4.1.8).
///
/// Bestimmt wie die Particles einer Model Group kombiniert werden.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Compositor {
    /// Sequence: Particles müssen in Reihenfolge erscheinen (8.5.4.1.8.1).
    Sequence,
    /// Choice: Genau ein Particle wird ausgewählt (8.5.4.1.8.2).
    Choice,
    /// All: Particles können in beliebiger Reihenfolge erscheinen (8.5.4.1.8.3).
    All,
}

// ============================================================================
// ModelGroup (Spec 8.5.4.1.8)
// ============================================================================

/// Model Group für komplexe Content Models (Spec 8.5.4.1.8).
///
/// Kombiniert mehrere Particles mit einem Compositor.
#[derive(Debug, Clone, PartialEq)]
pub struct ModelGroup {
    /// Compositor bestimmt die Kombinationsregel.
    pub compositor: Compositor,
    /// Liste der Particles in dieser Group.
    pub particles: Vec<Particle>,
}

impl ModelGroup {
    /// Erstellt eine neue Model Group.
    pub fn new(compositor: Compositor, particles: Vec<Particle>) -> Self {
        Self {
            compositor,
            particles,
        }
    }

    /// Erstellt eine leere Sequence.
    pub fn sequence(particles: Vec<Particle>) -> Self {
        Self::new(Compositor::Sequence, particles)
    }

    /// Erstellt eine leere Choice.
    pub fn choice(particles: Vec<Particle>) -> Self {
        Self::new(Compositor::Choice, particles)
    }

    /// Erstellt eine leere All Group.
    pub fn all(particles: Vec<Particle>) -> Self {
        Self::new(Compositor::All, particles)
    }
}

// ============================================================================
// ParticleTerm (Spec 8.5.4.1.5)
// ============================================================================

/// Particle Term - der Inhalt eines Particles (Spec 8.5.4.1.5).
///
/// Ein Particle Term kann sein:
/// - Element Declaration (8.5.4.1.6)
/// - Wildcard (8.5.4.1.7)
/// - Model Group (8.5.4.1.8)
#[derive(Debug, Clone, PartialEq)]
pub enum ParticleTerm {
    /// Element Declaration (Spec 8.5.4.1.6).
    Element(ElementDeclaration),
    /// Wildcard (Spec 8.5.4.1.7).
    ///
    /// Finding 4: Enthält jetzt auch processContents.
    Wildcard(Wildcard),
    /// Model Group: Sequence, Choice oder All (Spec 8.5.4.1.8).
    ModelGroup(ModelGroup),
}

// ============================================================================
// Particle (Spec 8.5.4.1.5)
// ============================================================================

/// Particle für Content Models (Spec 8.5.4.1.5).
///
/// Ein Particle kombiniert einen Term mit Wiederholungsgrenzen.
///
/// # Spec 8.5.4.1.5
///
/// "Given an XML Schema particle P_i with {min occurs}, {max occurs} and
/// {term} properties, generate a grammar Particle_i..."
#[derive(Debug, Clone, PartialEq)]
pub struct Particle {
    /// Minimale Anzahl Vorkommen.
    pub min_occurs: usize,
    /// Maximale Anzahl Vorkommen.
    pub max_occurs: MaxOccurs,
    /// Der Term dieses Particles.
    pub term: ParticleTerm,
}

impl Particle {
    /// Erstellt ein neues Particle mit Validierung.
    ///
    /// # Fehler
    ///
    /// - `Error::InvalidParticleOccurs` wenn max < min
    pub fn new(min_occurs: usize, max_occurs: MaxOccurs, term: ParticleTerm) -> Result<Self> {
        let p = Self {
            min_occurs,
            max_occurs,
            term,
        };
        p.validate()?;
        Ok(p)
    }

    /// Erstellt ein neues Particle ohne Validierung.
    ///
    /// # Sicherheit
    ///
    /// Der Aufrufer muss sicherstellen, dass die Invarianten eingehalten werden.
    /// Nutze diese Methode nur wenn die Werte bekannt gültig sind.
    pub fn new_unchecked(min_occurs: usize, max_occurs: MaxOccurs, term: ParticleTerm) -> Self {
        Self {
            min_occurs,
            max_occurs,
            term,
        }
    }

    /// Erstellt ein Particle mit min=max=1 (genau einmal).
    pub fn once(term: ParticleTerm) -> Self {
        Self::new_unchecked(1, MaxOccurs::Bounded(1), term)
    }

    /// Erstellt ein optionales Particle (min=0, max=1).
    pub fn optional(term: ParticleTerm) -> Self {
        Self::new_unchecked(0, MaxOccurs::Bounded(1), term)
    }

    /// Erstellt ein Particle mit min=0, max=unbounded (beliebig oft).
    pub fn zero_or_more(term: ParticleTerm) -> Self {
        Self::new_unchecked(0, MaxOccurs::Unbounded, term)
    }

    /// Erstellt ein Particle mit min=1, max=unbounded (mindestens einmal).
    pub fn one_or_more(term: ParticleTerm) -> Self {
        Self::new_unchecked(1, MaxOccurs::Unbounded, term)
    }

    /// Validiert Particle Constraints.
    ///
    /// # Fehler
    ///
    /// - `Error::InvalidParticleOccurs` wenn max < min
    pub fn validate(&self) -> Result<()> {
        match self.max_occurs {
            MaxOccurs::Bounded(max) if max < self.min_occurs => {
                Err(crate::Error::InvalidParticleOccurs {
                    min: self.min_occurs,
                    max,
                })
            }
            _ => Ok(()),
        }
    }
}

/// Findet eine lokale ElementDeclaration innerhalb eines TypeDefinitions.
///
/// Durchsucht das Content-Model (ElementOnly/Mixed) rekursiv nach einer
/// ElementDeclaration, die den QName (inkl. Substitution Group) matcht.
pub fn find_element_decl_in_type(
    type_def: &TypeDefinition,
    qname: &QName,
) -> Option<ElementDeclaration> {
    fn match_decl(decl: &ElementDeclaration, qname: &QName) -> bool {
        decl.matching_qnames().any(|candidate| candidate.as_ref() == qname)
    }

    fn find_in_particle(particle: &Particle, qname: &QName) -> Option<ElementDeclaration> {
        match &particle.term {
            ParticleTerm::Element(decl) => {
                if match_decl(decl, qname) {
                    Some(decl.clone())
                } else {
                    None
                }
            }
            ParticleTerm::Wildcard(_) => None,
            ParticleTerm::ModelGroup(group) => group
                .particles
                .iter()
                .find_map(|p| find_in_particle(p, qname)),
        }
    }

    match type_def {
        TypeDefinition::Complex {
            content: ContentType::ElementOnly(particle) | ContentType::Mixed(particle),
            ..
        } => find_in_particle(particle, qname),
        _ => None,
    }
}

/// Sammelt alle SE-ElementDeclarations aus einem TypeDefinition.
///
/// Traversiert das Content-Model (ElementOnly/Mixed) rekursiv und baut eine
/// Map von QName → ElementDeclaration auf. Jeder QName (Head + Substitution
/// Group Members) wird auf die zugehörige ElementDeclaration gemappt.
///
/// Wird einmal bei GrammarSystem-Konstruktion aufgerufen für O(1) Lookup
/// in `push_element` statt O(n) Particle-Traversal pro Event.
pub(crate) fn collect_se_element_decls(
    type_def: &TypeDefinition,
    interner: &mut crate::qname::StringInterner,
) -> crate::Result<crate::FastHashMap<crate::qname::ExpandedNameId, ElementDeclaration>> {
    fn collect(
        particle: &Particle,
        map: &mut crate::FastHashMap<crate::qname::ExpandedNameId, ElementDeclaration>,
        interner: &mut crate::qname::StringInterner,
    ) -> crate::Result<()> {
        match &particle.term {
            ParticleTerm::Element(decl) => {
                for qname in decl.matching_qnames() {
                    let expanded = interner.intern_expanded(&qname.uri, &qname.local_name)?;
                    map.entry(expanded).or_insert_with(|| decl.clone());
                }
            }
            ParticleTerm::Wildcard(_) => {}
            ParticleTerm::ModelGroup(group) => {
                for p in &group.particles {
                    collect(p, map, interner)?;
                }
            }
        }
        Ok(())
    }

    let mut map = crate::FastHashMap::default();
    if let TypeDefinition::Complex {
        content: ContentType::ElementOnly(p) | ContentType::Mixed(p),
        ..
    } = type_def
    {
        collect(p, &mut map, interner)?;
    }
    Ok(map)
}

/// Minimales Schema-Datenmodell für Grammar-Generierung.
///
/// Enthält die sortierten und deduplizierten QNames, die für die Generierung
/// von Schema-informed Document, Fragment und Element Fragment Grammars
/// benötigt werden.
///
/// # Sortierung (Spec 8.5.1)
///
/// "G_0, G_1, ... G_{n−1} represent all the qnames of global elements sorted
/// lexicographically, **first by local-name, then by uri**."
///
/// # Beispiel
///
/// ```
/// use erxi::schema::SchemaInfo;
/// use erxi::qname::QName;
///
/// let schema = SchemaInfo::builder()
///     .global_element(QName::new("http://example.org", "book"))
///     .global_element(QName::new("http://example.org", "author"))
///     .all_element(QName::new("", "chapter"))
///     .build();
///
/// // Global elements sind sortiert: author < book (local-name first)
/// assert_eq!(&*schema.global_elements()[0].local_name, "author");
/// assert_eq!(&*schema.global_elements()[1].local_name, "book");
/// ```
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Default)]
pub struct SchemaInfo {
    /// Globale Element-QNames, sortiert (für Document Grammar, Spec 8.5.1).
    /// Arc für effizientes Sharing bei Grammar-Generierung.
    global_elements: Vec<Rc<QName>>,
    /// Alle Element-QNames, sortiert (für Fragment Grammar, Spec 8.5.2).
    all_elements: Vec<Rc<QName>>,
    /// Alle Attribut-QNames, sortiert (für Element Fragment Grammar, Spec 8.5.3).
    all_attributes: Vec<Rc<QName>>,

    // ========================================================================
    // Vollständige Schema-Komponenten (für 8.5.4 Proto-Grammars)
    // ========================================================================

    /// Vollständige Element-Deklarationen, indiziert nach QName.
    ///
    /// Enthält alle Informationen für Proto-Grammar-Generierung:
    /// nillable, substitution_group, type_definition.
    element_declarations: BTreeMap<Rc<QName>, Rc<ElementDeclaration>>,

    /// Vollständige Type-Definitionen, indiziert nach QName.
    ///
    /// Enthält Simple und Complex Types mit allen Details für
    /// Type Grammar Generierung (8.5.4.1.3).
    type_definitions: BTreeMap<Rc<QName>, Rc<TypeDefinition>>,
    /// Globale Attribut-Typen (für AT(*) Typ-Lookup).
    global_attribute_types: BTreeMap<Rc<QName>, Rc<TypeDefinition>>,

    /// Globale Model Groups (xs:group), indiziert nach QName.
    ///
    /// Verwendet für group-ref Auflösung in Particles.
    model_groups: BTreeMap<Rc<QName>, Rc<ModelGroup>>,

    /// QNames mit mehreren Element-Deklarationen, deren Typ/Nillable nicht identisch ist.
    ///
    /// Diese Elemente werden in Fragmenten mit der Element Fragment Grammar verarbeitet
    /// (Spec 8.5.3).
    element_fragment_relaxed_elements: BTreeSet<Rc<QName>>,
    /// QNames mit mehreren Attribut-Deklarationen, deren Typname nicht identisch ist.
    ///
    /// Diese Attribute werden im Element Fragment Grammar als String repräsentiert
    /// (Spec 8.5.3).
    element_fragment_relaxed_attributes: BTreeSet<Rc<QName>>,
    /// Attribute-Typen für Element Fragment Grammar (nur wenn eindeutig).
    element_fragment_attribute_types: BTreeMap<Rc<QName>, Rc<TypeDefinition>>,
}

impl SchemaInfo {
    /// Erstellt einen Builder für SchemaInfo.
    pub fn builder() -> SchemaInfoBuilder {
        SchemaInfoBuilder::default()
    }

    /// Globale Element-QNames, sortiert (Spec 8.5.1).
    /// Arc für effizientes Sharing bei Grammar-Generierung.
    pub fn global_elements(&self) -> &[Rc<QName>] {
        &self.global_elements
    }

    /// Alle Element-QNames, sortiert (Spec 8.5.2).
    pub fn all_elements(&self) -> &[Rc<QName>] {
        &self.all_elements
    }

    /// Alle Attribut-QNames, sortiert (Spec 8.5.3).
    pub fn all_attributes(&self) -> &[Rc<QName>] {
        &self.all_attributes
    }

    /// Vollständige Element-Deklarationen (für 8.5.4 Proto-Grammars).
    pub fn element_declarations(&self) -> &BTreeMap<Rc<QName>, Rc<ElementDeclaration>> {
        &self.element_declarations
    }

    /// Sucht eine Element-Deklaration nach QName.
    pub fn get_element(&self, qname: &QName) -> Option<&Rc<ElementDeclaration>> {
        self.element_declarations
            .iter()
            .find(|(k, _)| k.as_ref() == qname)
            .map(|(_, v)| v)
    }

    /// Vollständige Type-Definitionen (für 8.5.4.1.3 Type Grammars).
    pub fn type_definitions(&self) -> &BTreeMap<Rc<QName>, Rc<TypeDefinition>> {
        &self.type_definitions
    }

    /// Sucht den Typ eines globalen Attributs.
    pub fn get_global_attribute_type(&self, qname: &QName) -> Option<&Rc<TypeDefinition>> {
        let td = self
            .global_attribute_types
            .iter()
            .find(|(k, _)| k.as_ref() == qname)
            .map(|(_, v)| v)?;

        // Falls der gespeicherte Typ nur eine benannte Hülle ist,
        // liefere die vollständige Type-Definition aus type_definitions.
        if let Some(name) = td.name()
            && let Some(full) = self.type_definitions.get(name) {
                return Some(full);
            }
        Some(td)
    }

    /// Globale Attribut-Typen (QNames) für Schema-informed StringTable-Prepopulation.
    pub fn global_attribute_types(&self) -> &BTreeMap<Rc<QName>, Rc<TypeDefinition>> {
        &self.global_attribute_types
    }

    /// Sucht eine Type-Definition nach QName.
    pub fn get_type(&self, qname: &QName) -> Option<&Rc<TypeDefinition>> {
        self.type_definitions
            .iter()
            .find(|(k, _)| k.as_ref() == qname)
            .map(|(_, v)| v)
    }

    /// Prüft ob ein Element im Element Fragment Grammar "relaxed" ist.
    ///
    /// Spec 8.5.3: Bei mehreren Deklarationen mit unterschiedlichen Typen/Nillable
    /// wird das Element mit Element Fragment Grammar verarbeitet.
    pub fn is_element_fragment_relaxed_element(&self, qname: &QName) -> bool {
        self.element_fragment_relaxed_elements
            .iter()
            .any(|k| k.as_ref() == qname)
    }

    /// Prüft ob ein Attribut im Element Fragment Grammar "relaxed" ist.
    ///
    /// Spec 8.5.3: Bei mehreren Deklarationen mit unterschiedlichen Typnamen
    /// wird der Wert als String repräsentiert.
    pub fn is_element_fragment_relaxed_attribute(&self, qname: &QName) -> bool {
        self.element_fragment_relaxed_attributes
            .iter()
            .any(|k| k.as_ref() == qname)
    }

    /// Sucht den eindeutigen Attribut-Typ für Element Fragment Grammar.
    ///
    /// Gibt None zurück wenn der Typ nicht eindeutig ist oder nicht bekannt.
    pub fn element_fragment_attribute_type(&self, qname: &QName) -> Option<&Rc<TypeDefinition>> {
        self.element_fragment_attribute_types
            .iter()
            .find(|(k, _)| k.as_ref() == qname)
            .map(|(_, v)| v)
    }


    /// Globale Model Groups (für group-ref Auflösung).
    pub fn model_groups(&self) -> &BTreeMap<Rc<QName>, Rc<ModelGroup>> {
        &self.model_groups
    }

    /// Sortiert QNames lexikografisch: erst local-name, dann URI (Spec 8.5.1).
    fn sort_qnames(qnames: &mut [Rc<QName>]) {
        qnames.sort_by(|a, b| {
            a.local_name
                .cmp(&b.local_name)
                .then_with(|| a.uri.cmp(&b.uri))
        });
    }

    /// Dedupliziert QNames (behält ersten Eintrag bei Duplikaten).
    ///
    /// Spec 8.5.2: "If there is more than one element declared with the same
    /// qname, the qname is included only once."
    fn dedup_qnames(qnames: &mut Vec<Rc<QName>>) {
        qnames.dedup_by(|a, b| **a == **b);
    }

    /// Normalisiert QName: Entfernt Prefix (setzt auf None).
    ///
    /// Schema-Prefixe sind Artefakte des XSD-Parsings und sollten nicht
    /// in die Grammar-Generierung gelangen. Spec 7.1.7: QName-Equality
    /// ignoriert Prefix ohnehin.
    fn normalize_qname(qname: QName) -> QName {
        QName::new(qname.uri, qname.local_name)
    }
}

/// Builder für SchemaInfo.
#[derive(Debug, Clone)]
pub struct SchemaInfoBuilder {
    global_elements: Vec<QName>,
    all_elements: Vec<QName>,
    all_attributes: Vec<QName>,
    /// Ob Prefixe beim Build normalisiert werden sollen (Standard: true).
    normalize_prefixes: bool,
    /// Vollständige Element-Deklarationen.
    element_declarations: BTreeMap<Rc<QName>, Rc<ElementDeclaration>>,
    /// Vollständige Type-Definitionen.
    type_definitions: BTreeMap<Rc<QName>, Rc<TypeDefinition>>,
    /// Globale Attribut-Typen.
    global_attribute_types: BTreeMap<Rc<QName>, Rc<TypeDefinition>>,
    /// Globale Model Groups.
    model_groups: BTreeMap<Rc<QName>, Rc<ModelGroup>>,
    /// Elemente mit uneinheitlicher Typ/Nillable-Kombination (Element Fragment Grammar).
    element_fragment_relaxed_elements: BTreeSet<Rc<QName>>,
}

impl Default for SchemaInfoBuilder {
    fn default() -> Self {
        Self {
            global_elements: Vec::new(),
            all_elements: Vec::new(),
            all_attributes: Vec::new(),
            normalize_prefixes: true,
            element_declarations: BTreeMap::new(),
            type_definitions: BTreeMap::new(),
            global_attribute_types: BTreeMap::new(),
            model_groups: BTreeMap::new(),
            element_fragment_relaxed_elements: BTreeSet::new(),
        }
    }
}

impl SchemaInfoBuilder {
    /// Fügt ein globales Element hinzu.
    ///
    /// Globale Elemente werden in Document Grammar (8.5.1) verwendet.
    pub fn global_element(mut self, qname: QName) -> Self {
        self.global_elements.push(qname);
        self
    }

    /// Fügt mehrere globale Elemente hinzu.
    pub fn global_elements(mut self, qnames: impl IntoIterator<Item = QName>) -> Self {
        self.global_elements.extend(qnames);
        self
    }

    /// Fügt ein Element (lokal oder global) hinzu.
    ///
    /// Alle Elemente werden in Fragment Grammar (8.5.2) verwendet.
    pub fn all_element(mut self, qname: QName) -> Self {
        self.all_elements.push(qname);
        self
    }

    /// Fügt mehrere Elemente hinzu.
    pub fn all_elements(mut self, qnames: impl IntoIterator<Item = QName>) -> Self {
        self.all_elements.extend(qnames);
        self
    }

    /// Fügt ein Attribut hinzu.
    ///
    /// Attribute werden in Element Fragment Grammar (8.5.3) verwendet.
    pub fn attribute(mut self, qname: QName) -> Self {
        self.all_attributes.push(qname);
        self
    }

    /// Fügt mehrere Attribute hinzu.
    pub fn attributes(mut self, qnames: impl IntoIterator<Item = QName>) -> Self {
        self.all_attributes.extend(qnames);
        self
    }

    /// Setzt ob Prefixe beim Build normalisiert werden sollen.
    ///
    /// Standard ist `true` (Prefixe werden entfernt). Setze auf `false`
    /// wenn Prefixe für andere Zwecke (z.B. String Table Prefix-Partition)
    /// benötigt werden.
    ///
    /// Für Grammar-Generierung (8.5.x) werden Prefixe nicht benötigt,
    /// da QName-Equality sie ignoriert (Spec 7.1.7).
    pub fn normalize_prefixes(mut self, normalize: bool) -> Self {
        self.normalize_prefixes = normalize;
        self
    }

    /// Fügt eine vollständige Element-Deklaration hinzu.
    pub fn element_declaration(
        mut self,
        qname: Rc<QName>,
        decl: Rc<ElementDeclaration>,
    ) -> Self {
        self.element_declarations.insert(qname, decl);
        self
    }

    /// Setzt alle Element-Deklarationen.
    pub fn element_declarations(
        mut self,
        decls: BTreeMap<Rc<QName>, Rc<ElementDeclaration>>,
    ) -> Self {
        self.element_declarations = decls;
        self
    }

    /// Fügt eine vollständige Type-Definition hinzu.
    pub fn type_definition(mut self, qname: Rc<QName>, def: Rc<TypeDefinition>) -> Self {
        self.type_definitions.insert(qname, def);
        self
    }

    /// Setzt alle Type-Definitionen.
    pub fn type_definitions(mut self, defs: BTreeMap<Rc<QName>, Rc<TypeDefinition>>) -> Self {
        self.type_definitions = defs;
        self
    }

    /// Setzt globale Attribut-Typen.
    pub fn global_attribute_types(
        mut self,
        defs: BTreeMap<Rc<QName>, Rc<TypeDefinition>>,
    ) -> Self {
        self.global_attribute_types = defs;
        self
    }

    /// Fügt eine Model Group hinzu.
    pub fn model_group(mut self, qname: Rc<QName>, group: Rc<ModelGroup>) -> Self {
        self.model_groups.insert(qname, group);
        self
    }

    /// Setzt alle Model Groups.
    pub fn model_groups(mut self, groups: BTreeMap<Rc<QName>, Rc<ModelGroup>>) -> Self {
        self.model_groups = groups;
        self
    }

    /// Setzt Elemente die im Element Fragment Grammar "relaxed" sind.
    pub fn element_fragment_relaxed_elements(
        mut self,
        qnames: BTreeSet<Rc<QName>>,
    ) -> Self {
        self.element_fragment_relaxed_elements = qnames;
        self
    }

    /// Baut das SchemaInfo mit sortierten und deduplizierten QNames.
    ///
    /// # Invariante
    ///
    /// `global_elements ⊆ all_elements` - Globale Elemente werden automatisch
    /// zu `all_elements` hinzugefügt, da sie laut Spec 8.5.2 auch "unique element
    /// qnames declared in the schema" sind.
    pub fn build(self) -> SchemaInfo {
        // Konvertiere zu Arc und normalisiere Prefixe falls gewünscht
        let to_arc = |qname: QName| -> Rc<QName> {
            if self.normalize_prefixes {
                Rc::new(SchemaInfo::normalize_qname(qname))
            } else {
                Rc::new(qname)
            }
        };

        let (element_fragment_relaxed_attributes, element_fragment_attribute_types) =
            Self::compute_element_fragment_attribute_info(
                &self.type_definitions,
                &self.global_attribute_types,
            );

        let mut global_elements: Vec<Rc<QName>> =
            self.global_elements.into_iter().map(&to_arc).collect();
        let mut all_elements: Vec<Rc<QName>> =
            self.all_elements.into_iter().map(&to_arc).collect();
        let mut all_attributes: Vec<Rc<QName>> =
            self.all_attributes.into_iter().map(to_arc).collect();

        // Invariante: global_elements ⊆ all_elements (Spec 8.5.2)
        // Globale Elemente sind auch "Elemente die im Schema deklariert sind"
        all_elements.extend(global_elements.iter().cloned());

        // Sortieren: local-name first, dann URI
        SchemaInfo::sort_qnames(&mut global_elements);
        SchemaInfo::sort_qnames(&mut all_elements);
        SchemaInfo::sort_qnames(&mut all_attributes);

        // Deduplizieren (nach Sortierung, damit dedup_by funktioniert)
        SchemaInfo::dedup_qnames(&mut global_elements);
        SchemaInfo::dedup_qnames(&mut all_elements);
        SchemaInfo::dedup_qnames(&mut all_attributes);

        SchemaInfo {
            global_elements,
            all_elements,
            all_attributes,
            element_declarations: self.element_declarations,
            type_definitions: self.type_definitions,
            global_attribute_types: self.global_attribute_types,
            model_groups: self.model_groups,
            element_fragment_relaxed_elements: self.element_fragment_relaxed_elements,
            element_fragment_relaxed_attributes,
            element_fragment_attribute_types,
        }
    }

    fn compute_element_fragment_attribute_info(
        type_definitions: &BTreeMap<Rc<QName>, Rc<TypeDefinition>>,
        global_attribute_types: &BTreeMap<Rc<QName>, Rc<TypeDefinition>>,
    ) -> (BTreeSet<Rc<QName>>, BTreeMap<Rc<QName>, Rc<TypeDefinition>>) {
        let mut sigs: BTreeMap<Rc<QName>, Option<Rc<QName>>> = BTreeMap::new();
        let mut relaxed: BTreeSet<Rc<QName>> = BTreeSet::new();
        let mut attr_types: BTreeMap<Rc<QName>, Rc<TypeDefinition>> = BTreeMap::new();

        let mut track = |qname: &Rc<QName>, type_def: Option<&Rc<TypeDefinition>>| {
            if relaxed.iter().any(|k| k.as_ref() == qname.as_ref()) {
                return;
            }
            let type_name = type_def.and_then(|td| td.name().cloned());
            if let Some(existing) = sigs.get(qname) {
                if existing.is_none()
                    || type_name.is_none()
                    || existing.as_ref() != type_name.as_ref()
                {
                    relaxed.insert(qname.clone());
                    attr_types.remove(qname);
                } else if let Some(td) = type_def {
                    attr_types.entry(qname.clone()).or_insert_with(|| td.clone());
                }
            } else {
                sigs.insert(qname.clone(), type_name);
                if let Some(td) = type_def {
                    attr_types.insert(qname.clone(), td.clone());
                }
            }
        };

        for td in type_definitions.values() {
            if let TypeDefinition::Complex { attributes, .. } = td.as_ref() {
                for attr in attributes {
                    track(&attr.qname, attr.type_definition.as_ref());
                }
            }
        }

        for (qname, td) in global_attribute_types {
            track(qname, Some(td));
        }

        (relaxed, attr_types)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // Tests: QName-Sortierung (Spec 8.5.1)
    // ========================================================================

    /// Spec 8.5.1: "sorted lexicographically, first by local-name, then by uri"
    #[test]
    fn sort_by_local_name_first() {
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "zebra"))
            .global_element(QName::new("", "apple"))
            .global_element(QName::new("", "mango"))
            .build();

        let names: Vec<_> = schema
            .global_elements()
            .iter()
            .map(|q| &*q.local_name)
            .collect();
        assert_eq!(names, vec!["apple", "mango", "zebra"]);
    }

    /// Spec 8.5.1: Bei gleichem local-name wird nach URI sortiert
    #[test]
    fn sort_by_uri_when_local_name_equal() {
        let schema = SchemaInfo::builder()
            .global_element(QName::new("http://z.org", "item"))
            .global_element(QName::new("http://a.org", "item"))
            .global_element(QName::new("", "item"))
            .build();

        let uris: Vec<_> = schema
            .global_elements()
            .iter()
            .map(|q| &*q.uri)
            .collect();
        // Leerer String < "http://a.org" < "http://z.org"
        assert_eq!(uris, vec!["", "http://a.org", "http://z.org"]);
    }

    /// Spec 8.5.1: Gemischte Sortierung (local-name primary, URI secondary)
    #[test]
    fn sort_mixed_local_name_and_uri() {
        let schema = SchemaInfo::builder()
            .global_element(QName::new("http://b.org", "book"))
            .global_element(QName::new("http://a.org", "author"))
            .global_element(QName::new("http://a.org", "book"))
            .build();

        let pairs: Vec<_> = schema
            .global_elements()
            .iter()
            .map(|q| (&*q.local_name, &*q.uri))
            .collect();
        // author < book (local-name), dann book@a.org < book@b.org (URI)
        assert_eq!(
            pairs,
            vec![
                ("author", "http://a.org"),
                ("book", "http://a.org"),
                ("book", "http://b.org"),
            ]
        );
    }

    // ========================================================================
    // Tests: Deduplizierung (Spec 8.5.2)
    // ========================================================================

    /// Spec 8.5.2: "If there is more than one element declared with the same
    /// qname, the qname is included only once."
    #[test]
    fn dedup_same_qname() {
        let schema = SchemaInfo::builder()
            .all_element(QName::new("", "item"))
            .all_element(QName::new("", "item"))
            .all_element(QName::new("", "item"))
            .build();

        assert_eq!(schema.all_elements().len(), 1);
        assert_eq!(&*schema.all_elements()[0].local_name, "item");
    }

    /// Deduplizierung funktioniert nach Sortierung
    #[test]
    fn dedup_after_sort() {
        let schema = SchemaInfo::builder()
            .all_element(QName::new("", "b"))
            .all_element(QName::new("", "a"))
            .all_element(QName::new("", "b")) // Duplikat, nicht benachbart vor Sort
            .build();

        assert_eq!(schema.all_elements().len(), 2);
        let names: Vec<_> = schema
            .all_elements()
            .iter()
            .map(|q| &*q.local_name)
            .collect();
        assert_eq!(names, vec!["a", "b"]);
    }

    /// QNames mit unterschiedlicher URI sind nicht gleich
    #[test]
    fn different_uri_not_deduped() {
        let schema = SchemaInfo::builder()
            .all_element(QName::new("http://a.org", "item"))
            .all_element(QName::new("http://b.org", "item"))
            .build();

        assert_eq!(schema.all_elements().len(), 2);
    }

    /// Prefix wird bei Deduplizierung ignoriert (QName-Equality)
    #[test]
    fn prefix_ignored_for_dedup() {
        let schema = SchemaInfo::builder()
            .all_element(QName::with_prefix("http://example.org", "item", "ex"))
            .all_element(QName::with_prefix("http://example.org", "item", "other"))
            .build();

        // Gleiche URI + local-name → Duplikat, auch wenn Prefix unterschiedlich
        assert_eq!(schema.all_elements().len(), 1);
    }

    /// Prefixe werden bei build() normalisiert (auf None gesetzt)
    ///
    /// Schema-Prefixe sind Artefakte des XSD-Parsings und sollten nicht
    /// in die Grammar-Generierung gelangen.
    #[test]
    fn prefix_normalized_to_none() {
        let schema = SchemaInfo::builder()
            .global_element(QName::with_prefix("http://example.org", "book", "ex"))
            .all_element(QName::with_prefix("http://example.org", "chapter", "ns"))
            .attribute(QName::with_prefix("http://example.org", "id", "attr"))
            .build();

        // Alle Prefixe sollten None sein
        assert!(
            schema.global_elements().iter().all(|q| q.prefix.is_none()),
            "global_elements sollten keine Prefixe haben"
        );
        assert!(
            schema.all_elements().iter().all(|q| q.prefix.is_none()),
            "all_elements sollten keine Prefixe haben"
        );
        assert!(
            schema.all_attributes().iter().all(|q| q.prefix.is_none()),
            "all_attributes sollten keine Prefixe haben"
        );
    }

    /// Prefixe bleiben erhalten wenn normalize_prefixes(false)
    #[test]
    fn prefix_preserved_when_disabled() {
        let schema = SchemaInfo::builder()
            .global_element(QName::with_prefix("http://example.org", "book", "ex"))
            .normalize_prefixes(false)
            .build();

        // Prefix sollte erhalten bleiben
        assert_eq!(
            schema.global_elements()[0].prefix.as_deref(),
            Some("ex"),
            "Prefix sollte erhalten bleiben wenn normalize_prefixes=false"
        );
    }

    // ========================================================================
    // Tests: Builder-API
    // ========================================================================

    /// Builder für leeres Schema
    #[test]
    fn empty_schema() {
        let schema = SchemaInfo::builder().build();

        assert!(schema.global_elements().is_empty());
        assert!(schema.all_elements().is_empty());
        assert!(schema.all_attributes().is_empty());
    }

    /// Builder mit allen drei Listen
    #[test]
    fn builder_all_lists() {
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "root"))
            .all_element(QName::new("", "child"))
            .attribute(QName::new("", "id"))
            .build();

        assert_eq!(schema.global_elements().len(), 1);
        // all_elements = 2 (root wird automatisch hinzugefügt + child)
        assert_eq!(schema.all_elements().len(), 2);
        assert_eq!(schema.all_attributes().len(), 1);
    }

    /// Builder mit Batch-Methoden
    #[test]
    fn builder_batch_methods() {
        let elements = vec![
            QName::new("", "a"),
            QName::new("", "b"),
            QName::new("", "c"),
        ];

        let schema = SchemaInfo::builder()
            .global_elements(elements.clone())
            .all_elements(elements)
            .build();

        assert_eq!(schema.global_elements().len(), 3);
        assert_eq!(schema.all_elements().len(), 3);
    }

    // ========================================================================
    // Tests: Invariante global_elements ⊆ all_elements (Spec 8.5.2)
    // ========================================================================

    /// Globale Elemente werden automatisch zu all_elements hinzugefügt
    #[test]
    fn global_elements_auto_added_to_all_elements() {
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "book"))
            .global_element(QName::new("", "author"))
            // Keine expliziten all_element() Aufrufe!
            .build();

        // global_elements hat 2
        assert_eq!(schema.global_elements().len(), 2);

        // all_elements hat auch 2 (automatisch hinzugefügt)
        assert_eq!(schema.all_elements().len(), 2);
        assert!(schema.all_elements().iter().any(|q| &*q.local_name == "book"));
        assert!(
            schema
                .all_elements()
                .iter()
                .any(|q| &*q.local_name == "author")
        );
    }

    /// Lokale + globale Elemente werden korrekt zusammengeführt
    #[test]
    fn global_and_local_elements_merged() {
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "book"))
            .all_element(QName::new("", "chapter")) // lokal
            .all_element(QName::new("", "paragraph")) // lokal
            .build();

        // global_elements hat nur 1
        assert_eq!(schema.global_elements().len(), 1);

        // all_elements hat 3 (1 global + 2 lokal)
        assert_eq!(schema.all_elements().len(), 3);
    }

    /// Duplikate werden dedupliziert wenn global und all_element gleich
    #[test]
    fn global_and_all_element_deduped() {
        let schema = SchemaInfo::builder()
            .global_element(QName::new("", "book"))
            .all_element(QName::new("", "book")) // Duplikat
            .build();

        assert_eq!(schema.global_elements().len(), 1);
        assert_eq!(schema.all_elements().len(), 1); // Nicht 2!
    }

    // ========================================================================
    // Tests: Attribute (Spec 8.5.3)
    // ========================================================================

    /// Spec 8.5.3: Attribute werden genauso sortiert wie Elemente
    #[test]
    fn attributes_sorted() {
        let schema = SchemaInfo::builder()
            .attribute(QName::new("", "id"))
            .attribute(QName::new("", "class"))
            .attribute(QName::new("", "name"))
            .build();

        let names: Vec<_> = schema
            .all_attributes()
            .iter()
            .map(|q| &*q.local_name)
            .collect();
        assert_eq!(names, vec!["class", "id", "name"]);
    }

    // ========================================================================
    // Tests: TypeDefinition (Spec 8.5.4.1.3)
    // ========================================================================

    /// Spec 8.5.4.1.3.1: Simple Type hat nur CH-Content
    #[test]
    fn type_definition_simple() {
        let simple = TypeDefinition::Simple {
            name: Some(Rc::new(QName::new(
                "http://www.w3.org/2001/XMLSchema",
                "string",
            ))),
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: Some("string".to_string()),
            enumeration_values: Vec::new(),
            is_union: false,
            has_named_sub_types: false,
        };
        assert!(simple.is_simple());
        assert!(!simple.is_complex());
    }

    /// Simple Type ohne Namen (anonymer Typ)
    #[test]
    fn type_definition_simple_anonymous() {
        let simple = TypeDefinition::Simple {
            name: None,
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: None,
            enumeration_values: Vec::new(),
            is_union: false,
            has_named_sub_types: false,
        };
        assert!(simple.is_simple());
        assert!(simple.name().is_none());
    }

    /// Complex Type name() Methode
    #[test]
    fn type_definition_complex_name() {
        let complex = TypeDefinition::Complex {
            name: Some(Rc::new(QName::new("", "TestType"))),
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: ContentType::Empty,
            has_named_sub_types: false,
        };
        assert!(complex.name().is_some());
        assert_eq!(&*complex.name().unwrap().local_name, "TestType");

        // Anonymous Complex Type
        let anon = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: ContentType::Empty,
            has_named_sub_types: false,
        };
        assert!(anon.name().is_none());
    }

    /// Spec 8.5.4.1.3.2: Complex Type mit Attributen und Content
    #[test]
    fn type_definition_complex() {
        let complex = TypeDefinition::Complex {
            name: Some(Rc::new(QName::new("", "PersonType"))),
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
        assert!(complex.is_complex());
        assert!(!complex.is_simple());
    }

    /// Complex Type mit Attribute Wildcard (##any)
    #[test]
    fn type_definition_complex_with_any_wildcard() {
        let complex = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: Some(AttributeWildcard::Any),
            content: ContentType::Empty,
            has_named_sub_types: false,
        };
        match &complex {
            TypeDefinition::Complex {
                attribute_wildcard, ..
            } => {
                assert!(matches!(attribute_wildcard, Some(AttributeWildcard::Any)));
            }
            _ => panic!("Expected Complex"),
        }
    }

    /// Spec 8.5.4.1.3.2: Namespace-Constraint Wildcard
    #[test]
    fn type_definition_complex_with_namespace_wildcard() {
        // Dummy-Particle für Mixed Content
        let dummy_particle = Particle::once(ParticleTerm::Element(ElementDeclaration::new(
            Rc::new(QName::new("", "dummy")),
        )));
        let complex = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: Some(AttributeWildcard::Namespaces(vec![
                "http://a.org".to_string(),
                "http://b.org".to_string(),
            ])),
            content: ContentType::Mixed(dummy_particle),
            has_named_sub_types: false,
        };
        match &complex {
            TypeDefinition::Complex {
                attribute_wildcard,
                content,
                ..
            } => {
                if let Some(AttributeWildcard::Namespaces(uris)) = attribute_wildcard {
                    assert_eq!(uris.len(), 2);
                    assert!(uris.contains(&"http://a.org".to_string()));
                } else {
                    panic!("Expected Namespaces wildcard");
                }
                assert!(matches!(content, ContentType::Mixed(_)));
            }
            _ => panic!("Expected Complex"),
        }
    }

    // ========================================================================
    // Tests: AttributeUse (Spec 8.5.4.1.4)
    // ========================================================================

    /// Spec 8.5.4.1.4: Required Attribute Use
    #[test]
    fn attribute_use_required() {
        let attr = AttributeUse {
                type_definition: None,
            qname: Rc::new(QName::new("", "id")),
            required: true,
        };
        assert!(attr.required);
        assert_eq!(&*attr.qname.local_name, "id");
    }

    /// Spec 8.5.4.1.4: Optional Attribute Use
    #[test]
    fn attribute_use_optional() {
        let attr = AttributeUse {
                type_definition: None,
            qname: Rc::new(QName::new("http://example.org", "optional")),
            required: false,
        };
        assert!(!attr.required);
    }

    // ========================================================================
    // Tests: ContentType (Spec 8.5.4.1.3.2)
    // ========================================================================

    /// ContentType-Varianten sind distinct
    #[test]
    fn content_type_variants() {
        // Dummy-Particle für Tests
        let dummy_particle = || {
            Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
                QName::new("", "dummy"),
            ))))
        };

        let empty = ContentType::Empty;
        let simple = ContentType::Simple;
        let element_only = ContentType::ElementOnly(dummy_particle());
        let mixed = ContentType::Mixed(dummy_particle());

        // Prüfe dass alle unterschiedlich sind
        assert_ne!(
            std::mem::discriminant(&empty),
            std::mem::discriminant(&simple)
        );
        assert_ne!(
            std::mem::discriminant(&simple),
            std::mem::discriminant(&element_only)
        );
        assert_ne!(
            std::mem::discriminant(&element_only),
            std::mem::discriminant(&mixed)
        );
    }

    // ========================================================================
    // Tests: Particle (Spec 8.5.4.1.5)
    // ========================================================================

    /// Spec 8.5.4.1.5: Particle mit min=max=1
    #[test]
    fn particle_once() {
        let qname = Rc::new(QName::new("", "element"));
        let term = ParticleTerm::Element(ElementDeclaration::new(qname));
        let p = Particle::once(term);

        assert_eq!(p.min_occurs, 1);
        assert_eq!(p.max_occurs, MaxOccurs::Bounded(1));
        assert!(p.validate().is_ok());
    }

    /// Spec 8.5.4.1.5: Optionales Particle (min=0, max=1)
    #[test]
    fn particle_optional() {
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let p = Particle::optional(term);

        assert_eq!(p.min_occurs, 0);
        assert_eq!(p.max_occurs, MaxOccurs::Bounded(1));
        assert!(p.validate().is_ok());
    }

    /// Spec 8.5.4.1.5: Unbounded Particle
    #[test]
    fn particle_zero_or_more() {
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let p = Particle::zero_or_more(term);

        assert_eq!(p.min_occurs, 0);
        assert_eq!(p.max_occurs, MaxOccurs::Unbounded);
        assert!(p.validate().is_ok());
    }

    /// Spec 8.5.4.1.5: min=1, max=unbounded
    #[test]
    fn particle_one_or_more() {
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let p = Particle::one_or_more(term);

        assert_eq!(p.min_occurs, 1);
        assert_eq!(p.max_occurs, MaxOccurs::Unbounded);
        assert!(p.validate().is_ok());
    }

    /// Particle Validierung: max < min ist ungültig
    #[test]
    fn particle_validate_max_less_than_min() {
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let result = Particle::new(5, MaxOccurs::Bounded(2), term);

        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            crate::Error::InvalidParticleOccurs { min: 5, max: 2 }
        ));
    }

    /// Particle Validierung: min=0, max=0 ist gültig
    #[test]
    fn particle_validate_zero_zero() {
        let term = ParticleTerm::Wildcard(Wildcard::any());
        let p = Particle::new(0, MaxOccurs::Bounded(0), term).unwrap();

        assert!(p.validate().is_ok());
    }

    // ========================================================================
    // Tests: ElementDeclaration (Spec 8.5.4.1.6)
    // ========================================================================

    /// ElementDeclaration ohne Substitution Group
    #[test]
    fn element_declaration_simple() {
        let qname = Rc::new(QName::new("http://example.org", "book"));
        let decl = ElementDeclaration::new(qname.clone());

        assert_eq!(decl.qname, qname);
        assert!(decl.substitution_group.is_empty());

        let matching: Vec<_> = decl.matching_qnames().collect();
        assert_eq!(matching.len(), 1);
        assert_eq!(matching[0].as_ref(), qname.as_ref());
    }

    /// ElementDeclaration mit Substitution Group
    /// Spec 8.5.4.1.6: Elemente werden lexikographisch sortiert (local-name, dann namespace)
    #[test]
    fn element_declaration_with_substitution_group() {
        let e0 = Rc::new(QName::new("", "book"));
        let e1 = Rc::new(QName::new("", "novel"));
        let e2 = Rc::new(QName::new("", "magazine"));

        let decl =
            ElementDeclaration::with_substitution_group(e0.clone(), vec![e1.clone(), e2.clone()]);

        let matching: Vec<_> = decl.matching_qnames().collect();
        // Spec 8.5.4.1.6: Alle Elemente (head + members) werden zusammen lexikographisch sortiert
        // book < magazine < novel (alphabetisch nach local-name)
        assert_eq!(matching.len(), 3);
        assert_eq!(matching[0].as_ref(), e0.as_ref()); // book
        assert_eq!(matching[1].as_ref(), e2.as_ref()); // magazine
        assert_eq!(matching[2].as_ref(), e1.as_ref()); // novel
    }

    /// Spec 8.5.4.4.2: ElementDeclaration nillable default ist false
    #[test]
    fn element_declaration_nillable_default_false() {
        let decl = ElementDeclaration::new(Rc::new(QName::new("", "test")));
        assert!(!decl.nillable);
        assert!(decl.type_definition.is_none());
    }

    /// Spec 8.5.4.4.2: ElementDeclaration mit nillable=true
    #[test]
    fn element_declaration_with_nillable() {
        let decl = ElementDeclaration::new(Rc::new(QName::new("", "test"))).with_nillable(true);
        assert!(decl.nillable);
    }

    /// Spec 8.5.4.4.2: ElementDeclaration mit type_definition
    #[test]
    fn element_declaration_with_type_definition() {
        let type_def = Rc::new(TypeDefinition::simple());
        let decl =
            ElementDeclaration::new(Rc::new(QName::new("", "test"))).with_type(type_def.clone());
        assert!(decl.type_definition.is_some());
        assert!(decl.type_definition.as_ref().unwrap().is_simple());
    }

    // ========================================================================
    // Tests: TypeDefinition Erweiterungen (Spec 8.5.4.4.2)
    // ========================================================================

    /// Spec 8.5.4.4.2: Simple Type is_union default ist false
    #[test]
    fn type_definition_simple_is_union_default_false() {
        let simple = TypeDefinition::simple();
        assert!(!simple.is_union());
        assert!(!simple.has_named_sub_types());
    }

    /// Spec 8.5.4.4.2: Simple Type mit is_union=true
    #[test]
    fn type_definition_simple_is_union_true() {
        let union_type = TypeDefinition::simple_union();
        assert!(union_type.is_union());
    }

    /// Spec 8.5.4.4.2: Complex Type has_named_sub_types default ist false
    #[test]
    fn type_definition_complex_has_named_sub_types_default_false() {
        let complex = TypeDefinition::complex_empty();
        assert!(!complex.has_named_sub_types());
        assert!(!complex.is_union());
    }

    /// Spec 8.5.4.4.2: Complex Type mit has_named_sub_types=true
    #[test]
    fn type_definition_complex_has_named_sub_types_true() {
        let complex = TypeDefinition::complex_with_sub_types();
        assert!(complex.has_named_sub_types());
    }

    /// TypeDefinition Builder: simple_named
    #[test]
    fn type_definition_simple_named() {
        let name = Rc::new(QName::new("http://www.w3.org/2001/XMLSchema", "string"));
        let simple = TypeDefinition::simple_named(name.clone());
        assert!(simple.is_simple());
        assert_eq!(&*simple.name().unwrap().local_name, "string");
    }

    // ========================================================================
    // Tests: WildcardConstraint (Spec 8.5.4.1.7)
    // ========================================================================

    /// Spec 8.5.4.1.7: ##any Wildcard
    #[test]
    fn wildcard_constraint_any() {
        let w = WildcardConstraint::Any;
        assert!(matches!(w, WildcardConstraint::Any));
    }

    /// Spec 8.5.4.1.7: ##other (not) Wildcard
    #[test]
    fn wildcard_constraint_not() {
        let w = WildcardConstraint::Not(Some("http://example.org".to_string()));
        if let WildcardConstraint::Not(Some(uri)) = &w {
            assert_eq!(uri, "http://example.org");
        } else {
            panic!("Expected Not(Some(...))");
        }

        // absent (kein Namespace)
        let w_absent = WildcardConstraint::Not(None);
        assert!(matches!(w_absent, WildcardConstraint::Not(None)));
    }

    /// Spec 8.5.4.1.7: Namespace-Liste Wildcard
    #[test]
    fn wildcard_constraint_namespaces() {
        let w = WildcardConstraint::Namespaces(vec![
            "http://a.org".to_string(),
            "".to_string(), // absent
            "http://b.org".to_string(),
        ]);

        if let WildcardConstraint::Namespaces(uris) = &w {
            assert_eq!(uris.len(), 3);
            assert!(uris.contains(&"".to_string())); // absent
            assert!(uris.contains(&"http://a.org".to_string()));
        } else {
            panic!("Expected Namespaces");
        }
    }

    // ========================================================================
    // Tests: ModelGroup (Spec 8.5.4.1.8)
    // ========================================================================

    /// Spec 8.5.4.1.8: Sequence Model Group
    #[test]
    fn model_group_sequence() {
        let p1 = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "a"),
        ))));
        let p2 = Particle::once(ParticleTerm::Element(ElementDeclaration::new(Rc::new(
            QName::new("", "b"),
        ))));

        let group = ModelGroup::sequence(vec![p1, p2]);
        assert_eq!(group.compositor, Compositor::Sequence);
        assert_eq!(group.particles.len(), 2);
    }

    /// Spec 8.5.4.1.8: Choice Model Group
    #[test]
    fn model_group_choice() {
        let p1 = Particle::once(ParticleTerm::Wildcard(Wildcard::any()));
        let p2 = Particle::optional(ParticleTerm::Wildcard(Wildcard::any()));

        let group = ModelGroup::choice(vec![p1, p2]);
        assert_eq!(group.compositor, Compositor::Choice);
        assert_eq!(group.particles.len(), 2);
    }

    /// Spec 8.5.4.1.8: All Model Group
    #[test]
    fn model_group_all() {
        let group = ModelGroup::all(vec![]);
        assert_eq!(group.compositor, Compositor::All);
        assert!(group.particles.is_empty());
    }

    // ========================================================================
    // Tests: Coverage für nicht abgedeckte Zeilen
    // ========================================================================

    /// Test für attributes() Batch-Methode (Zeile 549-551)
    #[test]
    fn builder_attributes_batch() {
        let attrs = vec![
            QName::new("", "id"),
            QName::new("", "class"),
            QName::new("", "name"),
        ];

        let schema = SchemaInfo::builder().attributes(attrs).build();

        assert_eq!(schema.all_attributes().len(), 3);
        // Sortiert: class < id < name
        assert_eq!(&*schema.all_attributes()[0].local_name, "class");
        assert_eq!(&*schema.all_attributes()[1].local_name, "id");
        assert_eq!(&*schema.all_attributes()[2].local_name, "name");
    }

    /// Test für SimpleTypeVariety::default() (Zeile 61-62)
    #[test]
    fn simple_type_variety_default() {
        let variety: SimpleTypeVariety = Default::default();
        assert!(matches!(variety, SimpleTypeVariety::Atomic));
    }

    /// Test für get_type() Methode (Zeile 816-820)
    #[test]
    fn schema_info_get_type() {
        let type_qname = Rc::new(QName::new("http://example.org", "MyType"));
        let type_def = Rc::new(TypeDefinition::Simple {
            name: Some(type_qname.clone()),
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: None,
            base_type: Some("string".to_string()),
            is_union: false,
            enumeration_values: Vec::new(),
            has_named_sub_types: false,
        });

        let schema = SchemaInfo::builder()
            .type_definition(type_qname.clone(), type_def.clone())
            .build();

        // Gefunden
        let found = schema.get_type(&type_qname);
        assert!(found.is_some());
        assert_eq!(found.unwrap().name(), Some(&type_qname));

        // Nicht gefunden
        let unknown = QName::new("http://other", "Unknown");
        assert!(schema.get_type(&unknown).is_none());
    }

    /// Test für model_groups() Getter (Zeile 824-825)
    #[test]
    fn schema_info_model_groups_getter() {
        let schema = SchemaInfo::builder().build();
        assert!(schema.model_groups().is_empty());
    }

    /// Test für model_group() Builder (Zeile 976-978)
    #[test]
    fn builder_model_group_single() {
        let group_qname = Rc::new(QName::new("http://example.org", "myGroup"));
        let group = Rc::new(ModelGroup {
            compositor: Compositor::Sequence,
            particles: vec![],
        });

        let schema = SchemaInfo::builder()
            .model_group(group_qname.clone(), group.clone())
            .build();

        assert_eq!(schema.model_groups().len(), 1);
        assert!(schema.model_groups().contains_key(&group_qname));
    }

    // ---------------------------------------------------------------
    // collect_se_element_decls — Side-Table-Tests
    // ---------------------------------------------------------------

    /// Hilfsfunktion: Complex TypeDefinition mit gegebenem Content.
    fn complex_type_with(content: ContentType) -> TypeDefinition {
        TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content,
            has_named_sub_types: false,
        }
    }

    /// Side-Table: ElementOnly mit Sequence enthält alle Child-Elemente.
    #[test]
    fn collect_se_element_decls_elementonly_sequence() {
        let child_a = ElementDeclaration::new(Rc::new(QName::new("", "a")));
        let child_b = ElementDeclaration::new(Rc::new(QName::new("", "b")));

        let seq = Particle::once(ParticleTerm::ModelGroup(ModelGroup::new(
            Compositor::Sequence,
            vec![
                Particle::once(ParticleTerm::Element(child_a.clone())),
                Particle::once(ParticleTerm::Element(child_b.clone())),
            ],
        )));

        let mut interner = crate::qname::StringInterner::new();
        let map = super::collect_se_element_decls(&complex_type_with(ContentType::ElementOnly(seq)), &mut interner).unwrap();
        assert_eq!(map.len(), 2);
        let id_a = interner.intern_expanded("", "a").unwrap();
        let id_b = interner.intern_expanded("", "b").unwrap();
        assert!(map.contains_key(&id_a));
        assert!(map.contains_key(&id_b));
    }

    /// Side-Table: Substitution Group Members werden einzeln gemappt.
    #[test]
    fn collect_se_element_decls_substitution_group() {
        let head = Rc::new(QName::new("", "head"));
        let member = Rc::new(QName::new("", "member"));
        let decl = ElementDeclaration::with_substitution_group(
            head.clone(),
            vec![member.clone()],
        );

        let seq = Particle::once(ParticleTerm::ModelGroup(ModelGroup::new(
            Compositor::Sequence,
            vec![Particle::once(ParticleTerm::Element(decl))],
        )));

        let mut interner = crate::qname::StringInterner::new();
        let map = super::collect_se_element_decls(&complex_type_with(ContentType::ElementOnly(seq)), &mut interner).unwrap();
        // Head + Member = 2 Einträge, beide zeigen auf dieselbe Deklaration
        assert_eq!(map.len(), 2);
        let id_head = interner.intern_expanded("", "head").unwrap();
        let id_member = interner.intern_expanded("", "member").unwrap();
        assert!(map.contains_key(&id_head));
        assert!(map.contains_key(&id_member));
        assert_eq!(map[&id_head].qname, head);
        assert_eq!(map[&id_member].qname, head);
    }

    /// Side-Table: Simple Type → leere Map (kein Content-Model).
    #[test]
    fn collect_se_element_decls_simple_type_empty() {
        let type_def = TypeDefinition::simple_with_base("string");
        let mut interner = crate::qname::StringInterner::new();
        let map = super::collect_se_element_decls(&type_def, &mut interner).unwrap();
        assert!(map.is_empty());
    }

    /// Side-Table: Empty Content → leere Map.
    #[test]
    fn collect_se_element_decls_empty_content() {
        let mut interner = crate::qname::StringInterner::new();
        let map = super::collect_se_element_decls(&complex_type_with(ContentType::Empty), &mut interner).unwrap();
        assert!(map.is_empty());
    }

    /// Side-Table: Mixed Content wird genauso traversiert wie ElementOnly.
    #[test]
    fn collect_se_element_decls_mixed_content() {
        let child = ElementDeclaration::new(Rc::new(QName::new("", "item")));
        let seq = Particle::once(ParticleTerm::ModelGroup(ModelGroup::new(
            Compositor::Sequence,
            vec![Particle::once(ParticleTerm::Element(child))],
        )));

        let mut interner = crate::qname::StringInterner::new();
        let map = super::collect_se_element_decls(&complex_type_with(ContentType::Mixed(seq)), &mut interner).unwrap();
        assert_eq!(map.len(), 1);
        let id_item = interner.intern_expanded("", "item").unwrap();
        assert!(map.contains_key(&id_item));
    }
}
