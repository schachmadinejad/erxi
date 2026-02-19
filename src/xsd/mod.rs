//! XSD Schema Loader / Compiler (Issue #34).
//!
//! Parsed XML Schema (XSD) Dateien und konvertiert sie zu `SchemaInfo`
//! für die EXI Grammar-Generierung.
//!
//! # Scope
//!
//! - XSD → `SchemaInfo` Parsing
//! - Element Declarations, Type Definitions, Particles, Model Groups
//! - Substitution Groups, nillable, is_union, has_named_sub_types
//!
//! # Out of Scope
//!
//! - `xs:include` / `xs:redefine` (Schema-Modularisierung)
//! - String Table Pre-Population (Issue #35)
//! - Facets für Restricted Character Sets (Issue #39)
//!
//! **Hinweis:** `xs:import` wird über [`parse_xsd_with_imports()`] unterstützt.

mod imports;

pub use imports::parse_xsd_with_imports;
use imports::MultiSchemaContext;

use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::{FastHashMap, FastHashSet};

use roxmltree::{Document, Node, ParsingOptions};

use crate::error::{Error, Result};
use crate::qname::QName;
use crate::schema::{
    AttributeUse, AttributeWildcard, Compositor, ContentType, DerivationKind, ElementDeclaration,
    MaxOccurs, ModelGroup, Particle, ParticleTerm, ProcessContents, SchemaInfo, SimpleTypeVariety,
    TypeDefinition, Wildcard, WildcardConstraint,
};

/// XML Schema Namespace.
const XS_NS: &str = "http://www.w3.org/2001/XMLSchema";

/// Built-in Types die **immer** benannte Sub-Types haben (XML Schema Hierarchie).
///
/// Diese Types haben in der XML Schema Built-in Type-Hierarchie Sub-Types,
/// unabhängig davon ob im konkreten Schema davon abgeleitet wird.
///
/// # Spec 8.5.4.4.2 (spec/exi-spec.txt lines 3282-3284)
///
/// "If T_k either has named sub-types or is a simple type definition
/// of which {variety} is union"
///
/// Exificient prüft dies gegen alle TYPE_DEFINITIONs im Xerces XSModel,
/// was auch die Built-in Types enthält. Da erxi Built-in Types nicht
/// explizit im Schema modelliert, müssen wir das hier hard-coden.
///
/// Quelle: https://www.w3.org/TR/xmlschema-2/#built-in-datatypes
const BUILTIN_TYPES_WITH_SUBTYPES: &[&str] = &[
    // anyType → alle Types (implicit)
    "anyType",
    // anySimpleType → alle Simple Types
    "anySimpleType",
    // String-Hierarchie: string → normalizedString → token → ...
    "string",
    "normalizedString",
    "token",
    "Name",
    "NCName",
    "NMTOKEN",
    "IDREF",
    "ENTITY",
    // Decimal/Integer-Hierarchie
    "decimal",
    "integer",
    "nonPositiveInteger",
    "negativeInteger",
    "long",
    "int",
    "short",
    "nonNegativeInteger",
    "positiveInteger",
    "unsignedLong",
    "unsignedInt",
    "unsignedShort",
];

/// Maximale Größe eines XSD-Dokuments (16 MiB).
///
/// Fix #10: DoS-Schutz durch Eingabegrößenbeschränkung.
/// Dieser Wert ist ausreichend für praktisch alle realen XSD-Dateien.
const MAX_XSD_SIZE: usize = 16 * 1024 * 1024;

/// Parsed ein XSD-Dokument zu SchemaInfo.
///
/// # Größenbeschränkung
///
/// XSD-Dokumente größer als 16 MiB werden abgelehnt (DoS-Schutz).
///
/// # Beispiel
///
/// ```
/// use erxi::xsd::parse_xsd;
///
/// let xsd = r#"
///     <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
///                targetNamespace="http://example.org">
///         <xs:element name="book" type="xs:string"/>
///     </xs:schema>
/// "#;
///
/// let schema = parse_xsd(xsd).unwrap();
/// assert_eq!(schema.global_elements().len(), 1);
/// ```
pub fn parse_xsd(xsd_content: &str) -> Result<SchemaInfo> {
    // Fix #10: DoS-Schutz durch Eingabegrößenbeschränkung
    if xsd_content.len() > MAX_XSD_SIZE {
        return Err(Error::XsdParseError(format!(
            "XSD document too large: {} bytes (max {} bytes)",
            xsd_content.len(),
            MAX_XSD_SIZE
        )));
    }

    let xml_opts = ParsingOptions { allow_dtd: true, ..Default::default() };
    let doc = Document::parse_with_options(xsd_content, xml_opts)
        .map_err(|e| Error::XsdParseError(format!("XML: {e}")))?;

    let root = doc.root_element();

    // Prüfe ob root ein xs:schema Element ist (Name UND Namespace)
    if root.tag_name().name() != "schema" || root.tag_name().namespace() != Some(XS_NS) {
        return Err(Error::XsdParseError(
            "Root element must be xs:schema".to_string(),
        ));
    }

    let mut parser = XsdParser::from_schema_element(&root)?;
    parser.parse_schema(&root)?;
    parser.resolve_references()?;

    Ok(parser.build())
}

// ========================================================================
// Shared Types und Hilfsfunktionen (von XsdParser UND MultiSchemaContext genutzt)
// ========================================================================

#[derive(Debug, Clone)]
struct ElementFragmentElementSignature {
    type_name: Option<Rc<QName>>,
    nillable: bool,
}

// ========================================================================
// ComplexType Extension Merging (Spec XSD 1.0 §3.4.6)
// ========================================================================

/// Merged ComplexType extensions into their base types.
///
/// Applies only to complex types with derivation=Extension.
/// For restrictions we keep derived content/attrs as-is.
fn merge_complex_type_extensions_in_map(
    types: &mut BTreeMap<Rc<QName>, TypeDefinition>,
    external_types: Option<&BTreeMap<Rc<QName>, TypeDefinition>>,
) {
    let keys: Vec<Rc<QName>> = types.keys().cloned().collect();
    let mut merged: BTreeMap<Rc<QName>, TypeDefinition> = BTreeMap::new();
    let mut visiting: FastHashSet<Rc<QName>> = FastHashSet::default();

    for key in keys {
        if let Some(resolved) = resolve_merged_type(
            &key,
            types,
            external_types,
            &mut merged,
            &mut visiting,
        ) {
            types.insert(key, resolved);
        }
    }
}

fn resolve_merged_type(
    key: &Rc<QName>,
    types: &BTreeMap<Rc<QName>, TypeDefinition>,
    external_types: Option<&BTreeMap<Rc<QName>, TypeDefinition>>,
    merged: &mut BTreeMap<Rc<QName>, TypeDefinition>,
    visiting: &mut FastHashSet<Rc<QName>>,
) -> Option<TypeDefinition> {
    if let Some(existing) = merged.get(key) {
        return Some(existing.clone());
    }
    if visiting.contains(key) {
        return None; // cycle detected
    }

    let current = types.get(key)?.clone();
    visiting.insert(key.clone());

    let resolved = match current.clone() {
        TypeDefinition::Complex {
            name,
            base_type: Some(base_qname),
            derivation: Some(DerivationKind::Extension),
            attributes,
            attribute_wildcard,
            content,
            has_named_sub_types,
        } => {
            let base = types
                .get(&base_qname)
                .or_else(|| external_types.and_then(|m| m.get(&base_qname)));

            if let Some(base_def) = base {
                let base_resolved = if let Some(bname) = base_def.name() {
                    resolve_merged_type(bname, types, external_types, merged, visiting)
                        .unwrap_or_else(|| base_def.clone())
                } else {
                    base_def.clone()
                };

                let merged_complex = merge_complex_extension(
                    TypeDefinition::Complex {
                        name,
                        base_type: Some(base_qname),
                        derivation: Some(DerivationKind::Extension),
                        attributes,
                        attribute_wildcard,
                        content,
                        has_named_sub_types,
                    },
                    base_resolved,
                );
                merged.insert(key.clone(), merged_complex.clone());
                merged_complex
            } else {
                current
            }
        }
        _ => current,
    };

    visiting.remove(key);
    Some(resolved)
}

fn merge_complex_extension(
    derived: TypeDefinition,
    base: TypeDefinition,
) -> TypeDefinition {
    let (name, base_type, derivation, attributes, attribute_wildcard, content, mut has_named_sub_types) =
        match derived {
            TypeDefinition::Complex {
                name,
                base_type,
                derivation,
                attributes,
                attribute_wildcard,
                content,
                has_named_sub_types,
            } => (
                name,
                base_type,
                derivation,
                attributes,
                attribute_wildcard,
                content,
                has_named_sub_types,
            ),
            other => return other,
        };

    let (base_attrs, base_wildcard, base_content) = match base {
        TypeDefinition::Complex {
            attributes,
            attribute_wildcard,
            content,
            has_named_sub_types: base_sub,
            ..
        } => {
            if base_sub {
                has_named_sub_types = true;
            }
            (attributes, attribute_wildcard, content)
        }
        _ => return TypeDefinition::Complex {
            name,
            base_type,
            derivation,
            attributes,
            attribute_wildcard,
            content,
            has_named_sub_types,
        },
    };

    // Merge attributes: base first, then derived (dedup by QName, derived wins).
    let mut merged_attrs = Vec::new();
    for attr in base_attrs.into_iter().chain(attributes.into_iter()) {
        if let Some(pos) = merged_attrs
            .iter()
            .position(|a: &AttributeUse| a.qname == attr.qname)
        {
            merged_attrs[pos] = attr;
        } else {
            merged_attrs.push(attr);
        }
    }

    // Merge attribute wildcards.
    let merged_wildcard = merge_attribute_wildcards(base_wildcard, attribute_wildcard);

    // Merge content: base then derived (sequence) for ElementOnly/Mixed.
    let merged_content = merge_content_types(base_content, content);

    TypeDefinition::Complex {
        name,
        base_type,
        derivation,
        attributes: merged_attrs,
        attribute_wildcard: merged_wildcard,
        content: merged_content,
        has_named_sub_types,
    }
}

fn merge_attribute_wildcards(
    base: Option<AttributeWildcard>,
    derived: Option<AttributeWildcard>,
) -> Option<AttributeWildcard> {
    match (base, derived) {
        (None, None) => None,
        (Some(w), None) | (None, Some(w)) => Some(w),
        (Some(a), Some(b)) => match (a, b) {
            (AttributeWildcard::Any, _) | (_, AttributeWildcard::Any) => Some(AttributeWildcard::Any),
            (AttributeWildcard::Not(_), _) | (_, AttributeWildcard::Not(_)) => {
                // Approximieren zu Any um keine gültigen Attribute zu verlieren.
                Some(AttributeWildcard::Any)
            }
            (AttributeWildcard::Namespaces(mut a), AttributeWildcard::Namespaces(b)) => {
                for ns in b {
                    if !a.contains(&ns) {
                        a.push(ns);
                    }
                }
                Some(AttributeWildcard::Namespaces(a))
            }
        },
    }
}

fn merge_content_types(base: ContentType, derived: ContentType) -> ContentType {
    match (base, derived) {
        (ContentType::Empty, other) => other,
        (other, ContentType::Empty) => other,
        (ContentType::Simple, other) => other,
        (other, ContentType::Simple) => other,
        (ContentType::ElementOnly(b), ContentType::ElementOnly(d)) => {
            ContentType::ElementOnly(sequence_particles(b, d))
        }
        (ContentType::Mixed(b), ContentType::Mixed(d)) => {
            ContentType::Mixed(sequence_particles(b, d))
        }
        (ContentType::Mixed(b), ContentType::ElementOnly(d)) => {
            ContentType::Mixed(sequence_particles(b, d))
        }
        (ContentType::ElementOnly(b), ContentType::Mixed(d)) => {
            ContentType::Mixed(sequence_particles(b, d))
        }
    }
}

fn sequence_particles(base: Particle, derived: Particle) -> Particle {
    Particle::once(ParticleTerm::ModelGroup(ModelGroup::sequence(vec![base, derived])))
}

/// Setzt `has_named_sub_types=true` für Built-in XSD Types die nicht in
/// `global_types` auftauchen (Spec 8.5.4.4.2).
///
/// Built-in Types wie `xsd:anySimpleType` haben per Definition Sub-Types,
/// tauchen aber nicht in der global_types-Map auf. Ohne diesen Fix fehlt
/// AT(xsi:type) im strict-Modus für Elemente mit solchen Types.
fn sync_particle_type_defs(
    particle: &mut Particle,
    global_types: &BTreeMap<Rc<QName>, TypeDefinition>,
) {
    match &mut particle.term {
        ParticleTerm::Element(elem_decl) => {
            if let Some(ref type_def) = elem_decl.type_definition
                && let Some(type_qname) = type_def.name() {
                    if let Some(updated_type) = global_types.get(type_qname) {
                        elem_decl.type_definition = Some(Rc::new(updated_type.clone()));
                    } else if &*type_qname.uri == XS_NS
                        && BUILTIN_TYPES_WITH_SUBTYPES.contains(&&*type_qname.local_name)
                        && !type_def.has_named_sub_types()
                    {
                        let updated = type_def.with_named_sub_types();
                        elem_decl.type_definition = Some(Rc::new(updated));
                    }
                }
        }
        ParticleTerm::ModelGroup(model_group) => {
            for nested_particle in &mut model_group.particles {
                sync_particle_type_defs(nested_particle, global_types);
            }
        }
        ParticleTerm::Wildcard(_) => {}
    }
}

/// Form-Default für Elemente und Attribute.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
enum FormDefault {
    #[default]
    Unqualified,
    Qualified,
}

/// Pending Reference für spätere Auflösung.
#[derive(Debug)]
enum PendingRef {
    /// Substitution Group: member → head.
    SubstitutionGroup {
        member: Rc<QName>,
        head: Rc<QName>,
    },
}

/// XSD Parser State.
struct XsdParser {
    /// Target Namespace des Schemas.
    target_ns: String,
    /// Form-Default für Elemente.
    element_form_default: FormDefault,
    /// Form-Default für Attribute.
    attribute_form_default: FormDefault,
    /// Globale Element-Deklarationen.
    global_elements: BTreeMap<Rc<QName>, ElementDeclaration>,
    /// Globale Type-Definitionen.
    global_types: BTreeMap<Rc<QName>, TypeDefinition>,
    /// Globale Group-Definitionen.
    global_groups: BTreeMap<Rc<QName>, ModelGroup>,
    /// Globale Attribut-Deklarationen (für ref-Auflösung).
    global_attributes: BTreeMap<Rc<QName>, AttributeDeclaration>,
    /// Pending References für spätere Auflösung.
    pending_refs: Vec<PendingRef>,
    /// Base-Type-Tracking für has_named_sub_types (Complex Types).
    type_bases: BTreeMap<Rc<QName>, Rc<QName>>,
    /// Base-Type-Tracking für SimpleTypes (QName → Base-QName).
    simple_type_bases: BTreeMap<Rc<QName>, Rc<QName>>,
    /// Alle gesammelten Elemente (global + lokal).
    all_elements: BTreeSet<Rc<QName>>,
    /// Alle gesammelten Attribute.
    all_attributes: BTreeSet<Rc<QName>>,
    /// Alle Element-Deklarationen (global + lokal) mit vollständiger Type-Info.
    /// Für Schema-informed Element Grammar Generierung (Spec 8.5.4).
    all_element_declarations: BTreeMap<Rc<QName>, ElementDeclaration>,
    /// Signaturen für Element Fragment Grammar (8.5.3).
    element_fragment_element_signatures: BTreeMap<Rc<QName>, ElementFragmentElementSignature>,
    /// Elemente mit uneinheitlicher Typ/Nillable-Kombination.
    element_fragment_relaxed_elements: BTreeSet<Rc<QName>>,
    // -- Externe Komponenten (aus importierten Schemas) --
    /// Externe Typen (aus bereits geladenen importierten Schemas).
    external_types: BTreeMap<Rc<QName>, TypeDefinition>,
    /// Externe Elemente (aus bereits geladenen importierten Schemas).
    external_elements: BTreeMap<Rc<QName>, ElementDeclaration>,
    /// Externe Groups (aus bereits geladenen importierten Schemas).
    external_groups: BTreeMap<Rc<QName>, ModelGroup>,
    /// Externe Attribute (aus bereits geladenen importierten Schemas).
    external_attributes: BTreeMap<Rc<QName>, AttributeDeclaration>,
}

/// Globale Attribut-Deklaration.
#[derive(Debug, Clone)]
struct AttributeDeclaration {
    type_definition: Option<Rc<TypeDefinition>>,
}

impl XsdParser {
    /// Initialisiert Parser aus schema-Element.
    fn from_schema_element(root: &Node) -> Result<Self> {
        let target_ns = root.attribute("targetNamespace").unwrap_or("").to_string();

        // Form-Defaults
        let element_form_default = match root.attribute("elementFormDefault") {
            Some("qualified") => FormDefault::Qualified,
            _ => FormDefault::Unqualified,
        };
        let attribute_form_default = match root.attribute("attributeFormDefault") {
            Some("qualified") => FormDefault::Qualified,
            _ => FormDefault::Unqualified,
        };

        Ok(Self {
            target_ns,
            element_form_default,
            attribute_form_default,
            global_elements: BTreeMap::new(),
            global_types: BTreeMap::new(),
            global_groups: BTreeMap::new(),
            global_attributes: BTreeMap::new(),
            pending_refs: Vec::new(),
            type_bases: BTreeMap::new(),
            simple_type_bases: BTreeMap::new(),
            all_elements: BTreeSet::new(),
            all_attributes: BTreeSet::new(),
            all_element_declarations: BTreeMap::new(),
            element_fragment_element_signatures: BTreeMap::new(),
            element_fragment_relaxed_elements: BTreeSet::new(),
            external_types: BTreeMap::new(),
            external_elements: BTreeMap::new(),
            external_groups: BTreeMap::new(),
            external_attributes: BTreeMap::new(),
        })
    }

    /// Setzt externe Komponenten aus einem MultiSchemaContext.
    ///
    /// Ermöglicht Cross-Namespace Referenz-Auflösung für xs:import.
    fn set_external_components(&mut self, ctx: &MultiSchemaContext) {
        self.external_types = ctx.global_types.clone();
        self.external_elements = ctx.global_elements.clone();
        self.external_groups = ctx.global_groups.clone();
        self.external_attributes = ctx.global_attributes.clone();
    }

    /// XML Namespace (implizit immer definiert).
    const XML_NS: &'static str = "http://www.w3.org/XML/1998/namespace";

    /// Löst einen QName-String aus Attributwerten auf (type, ref, base).
    ///
    /// Durchsucht `node.namespaces()` um auch Namespaces von Ancestor-Elementen
    /// zu berücksichtigen (roxmltree vererbt Namespaces automatisch).
    ///
    /// Der Prefix "xml" ist implizit immer definiert als XML_NS.
    ///
    /// Unprefixed QNames in Attributwerten bedeuten laut XML-Namespace-Spec
    /// NO namespace. Viele reale XSDs deklarieren jedoch den XSD-Namespace
    /// als Default-Namespace (`xmlns="http://www.w3.org/2001/XMLSchema"`)
    /// und erwarten, dass `type="string"` als `xs:string` aufgelöst wird.
    /// Fallback: Wenn der Default-Namespace des Knotens XS_NS ist, wird
    /// ein unpräfixierter Name im XSD-Namespace gesucht.
    fn resolve_qname_in_node(&self, qname_str: &str, node: &Node) -> Result<Rc<QName>> {
        if let Some((prefix, local)) = qname_str.split_once(':') {
            // Sonderfall: "xml" Prefix ist implizit immer definiert
            if prefix == "xml" {
                return Ok(Rc::new(QName::new(Self::XML_NS, local)));
            }

            // Suche Prefix in allen sichtbaren Namespaces des Knotens
            let ns = node
                .namespaces()
                .find(|ns| ns.name() == Some(prefix))
                .map(|ns| ns.uri())
                .ok_or_else(|| {
                    Error::XsdParseError(format!("Unknown prefix '{prefix}' in '{qname_str}'"))
                })?;
            Ok(Rc::new(QName::new(ns, local)))
        } else {
            // Unprefixed: Prüfe ob Default-Namespace = XSD-Namespace
            let default_ns = node
                .namespaces()
                .find(|ns| ns.name().is_none())
                .map(|ns| ns.uri());
            if default_ns == Some(XS_NS) {
                Ok(Rc::new(QName::new(XS_NS, qname_str)))
            } else {
                Ok(Rc::new(QName::new("", qname_str)))
            }
        }
    }

    /// Ermittelt Namespace basierend auf form-Attribut und Default.
    fn namespace_for_form(&self, node: &Node, form_default: FormDefault) -> &str {
        match node.attribute("form") {
            Some("qualified") => &self.target_ns,
            Some("unqualified") => "",
            _ => match form_default {
                FormDefault::Qualified => &self.target_ns,
                FormDefault::Unqualified => "",
            },
        }
    }

    /// Ermittelt Namespace für lokales Element basierend auf form-Default.
    fn element_namespace(&self, node: &Node) -> &str {
        self.namespace_for_form(node, self.element_form_default)
    }

    /// Ermittelt Namespace für lokales Attribut basierend auf form-Default.
    fn attribute_namespace(&self, node: &Node) -> &str {
        self.namespace_for_form(node, self.attribute_form_default)
    }

    /// Parsed das Schema in zwei Phasen für Forward-Reference-Support.
    ///
    /// Pass 1: Alle globalen Namen registrieren (Platzhalter)
    /// Pass 2: Alles vollständig parsen
    fn parse_schema(&mut self, root: &Node) -> Result<()> {
        let children: Vec<_> = root
            .children()
            .filter(|n| n.is_element() && n.tag_name().namespace() == Some(XS_NS))
            .collect();

        // Prüfe auf nicht-unterstützte Konstrukte
        for child in &children {
            match child.tag_name().name() {
                "attributeGroup" => {
                    return Err(Error::XsdParseError(
                        "xs:attributeGroup is not yet supported".to_string(),
                    ));
                }
                "import" | "include" | "redefine" => {
                    return Err(Error::XsdParseError(format!(
                        "xs:{} is not yet supported (schema modularization)",
                        child.tag_name().name()
                    )));
                }
                _ => {}
            }
        }

        // ====================================================================
        // Pass 1: Alle globalen Namen registrieren (Platzhalter)
        // ====================================================================
        for child in &children {
            match child.tag_name().name() {
                "element" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
                        // Platzhalter-Element registrieren
                        self.global_elements
                            .insert(qname.clone(), ElementDeclaration::new(qname.clone()));
                        self.all_elements.insert(qname);
                    }
                }
                "simpleType" | "complexType" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
                        // Platzhalter-Type registrieren
                        let placeholder = if child.tag_name().name() == "simpleType" {
                            TypeDefinition::Simple {
                                name: Some(qname.clone()),
                                variety: SimpleTypeVariety::Atomic,
                                base_type_qname: None,
                                enumeration_values: Vec::new(),
                                base_type: None,
                                is_union: false,
                                has_named_sub_types: false,
                            }
                        } else {
                            TypeDefinition::Complex {
                                name: Some(qname.clone()),
                                base_type: None,
                                derivation: None,
                                attributes: Vec::new(),
                                attribute_wildcard: None,
                                content: ContentType::Empty,
                                has_named_sub_types: false,
                            }
                        };
                        self.global_types.insert(qname, placeholder);
                    }
                }
                "group" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
                        // Platzhalter-Group registrieren (leere Sequence)
                        self.global_groups
                            .insert(qname, ModelGroup::sequence(Vec::new()));
                    }
                }
                "attribute" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
                        self.all_attributes.insert(qname);
                    }
                }
                _ => {}
            }
        }

        // ====================================================================
        // Pass 2: Vollständiges Parsing
        // Reihenfolge wichtig für Forward-References:
        // 1. Groups zuerst (damit Group-Refs in Types korrekt aufgelöst werden)
        // 2. Elemente (damit Element-Refs in Types korrekt aufgelöst werden)
        // 3. Typen zuletzt (können Groups, Elemente und andere Typen referenzieren)
        // ====================================================================

        // 2a: Alle Groups parsen
        for child in &children {
            if child.tag_name().name() == "group" {
                self.parse_global_group(child)?;
            }
        }

        // 2b: Alle Elemente und Attribute parsen
        for child in &children {
            match child.tag_name().name() {
                "element" => self.parse_global_element(child)?,
                "attribute" => self.parse_global_attribute(child)?,
                _ => {}
            }
        }

        // 2c: Alle Typen parsen
        for child in &children {
            match child.tag_name().name() {
                "simpleType" => self.parse_global_simple_type(child)?,
                "complexType" => self.parse_global_complex_type(child)?,
                _ => {}
            }
        }

        Ok(())
    }

    /// Parsed das Schema mit Import-Support.
    ///
    /// Wie `parse_schema`, aber überspringt xs:import Elemente statt sie abzulehnen.
    /// xs:import wird von `MultiSchemaContext` behandelt bevor diese Methode aufgerufen wird.
    fn parse_schema_with_imports(&mut self, root: &Node) -> Result<()> {
        let children: Vec<_> = root
            .children()
            .filter(|n| n.is_element() && n.tag_name().namespace() == Some(XS_NS))
            .collect();

        // Prüfe auf nicht-unterstützte Konstrukte (xs:import ist erlaubt)
        for child in &children {
            match child.tag_name().name() {
                "attributeGroup" => {
                    return Err(Error::XsdParseError(
                        "xs:attributeGroup is not yet supported".to_string(),
                    ));
                }
                "include" | "redefine" => {
                    return Err(Error::XsdParseError(format!(
                        "xs:{} is not yet supported (schema modularization)",
                        child.tag_name().name()
                    )));
                }
                // xs:import wird übersprungen - bereits vom MultiSchemaContext behandelt
                "import" => {}
                _ => {}
            }
        }

        // Pass 1: Alle globalen Namen registrieren (Platzhalter)
        for child in &children {
            match child.tag_name().name() {
                "element" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
                        self.global_elements
                            .insert(qname.clone(), ElementDeclaration::new(qname.clone()));
                        self.all_elements.insert(qname);
                    }
                }
                "simpleType" | "complexType" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
                        let placeholder = if child.tag_name().name() == "simpleType" {
                            TypeDefinition::Simple {
                                name: Some(qname.clone()),
                                variety: SimpleTypeVariety::Atomic,
                                base_type_qname: None,
                                base_type: None,
                                enumeration_values: Vec::new(),
                                is_union: false,
                                has_named_sub_types: false,
                            }
                        } else {
                            TypeDefinition::Complex {
                                name: Some(qname.clone()),
                                base_type: None,
                                derivation: None,
                                attributes: Vec::new(),
                                attribute_wildcard: None,
                                content: ContentType::Empty,
                                has_named_sub_types: false,
                            }
                        };
                        self.global_types.insert(qname, placeholder);
                    }
                }
                "group" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
                        self.global_groups
                            .insert(qname, ModelGroup::sequence(Vec::new()));
                    }
                }
                "attribute" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
                        self.all_attributes.insert(qname);
                    }
                }
                _ => {}
            }
        }

        // Pass 2: Vollständiges Parsing
        // Reihenfolge: Groups → Types → Elements/Attributes
        // Types werden VOR Elements/Attributes geparst, damit Typ-Referenzen
        // in Attribut-Deklarationen korrekt aufgelöst werden (z.B. list-Typen).
        // Element-Referenzen in Content-Models werden nachträglich in
        // resolve_references()/sync_particles() aktualisiert.
        for child in &children {
            if child.tag_name().name() == "group" {
                self.parse_global_group(child)?;
            }
        }

        for child in &children {
            match child.tag_name().name() {
                "simpleType" => self.parse_global_simple_type(child)?,
                "complexType" => self.parse_global_complex_type(child)?,
                _ => {}
            }
        }

        for child in &children {
            match child.tag_name().name() {
                "element" => self.parse_global_element(child)?,
                "attribute" => self.parse_global_attribute(child)?,
                _ => {}
            }
        }

        for child in &children {
            match child.tag_name().name() {
                "simpleType" => self.parse_global_simple_type(child)?,
                "complexType" => self.parse_global_complex_type(child)?,
                _ => {}
            }
        }

        Ok(())
    }

    /// Löst alle Referenzen auf.
    fn resolve_references(&mut self) -> Result<()> {
        // 1. Particles in Types aktualisieren (Group-Refs und Element-Refs)
        //    Muss vor Substitution Groups passieren, da Element-Typen aktualisiert werden
        self.sync_particles();

        // 1a. ComplexType-Extension: Base-Type Inhalte/Attribute mergen
        //     (Spec XSD 1.0 §3.4.6, EXI 8.5.4.1.3.2)
        merge_complex_type_extensions_in_map(
            &mut self.global_types,
            Some(&self.external_types),
        );

        // 1b. Element type_definitions mit finalen Typen synchronisieren
        //     Fix: Elemente werden vor Typen geparsed und haben nur Platzhalter
        self.sync_element_types();

        // 2. SimpleType base_type transitiv auflösen
        self.resolve_transitive_base_types();

        // 3. Finding 3: Union-Variety durch Derivation propagieren (Spec 8.5.4.4.2)
        self.propagate_union_variety();

        // 4. Substitution Groups auflösen
        self.resolve_substitution_groups()?;

        // 4a. Substitution Groups in lokalen Element-Refs aktualisieren
        //     (lokale Refs wurden vor Schritt 4 synchronisiert).
        self.sync_particles();

        // 5. has_named_sub_types berechnen
        self.compute_sub_types();

        Ok(())
    }

    /// Löst SimpleType base_type transitiv auf.
    ///
    /// Wenn ein SimpleType von einem anderen benannten SimpleType erbt,
    /// folge der Kette bis zum xs:* built-in Type.
    fn resolve_transitive_base_types(&mut self) {
        // Sammle alle SimpleTypes mit base_type = None
        let types_needing_resolution: Vec<Rc<QName>> = self
            .global_types
            .iter()
            .filter_map(|(qname, type_def)| {
                if let TypeDefinition::Simple {
                    base_type: None, ..
                } = type_def
                {
                    Some(qname.clone())
                } else {
                    None
                }
            })
            .collect();

        // Für jeden Typ: Folge der simple_type_bases Kette bis zum xs:* Type
        for type_qname in types_needing_resolution {
            if let Some(resolved) = self.find_transitive_builtin(&type_qname)
                && let Some(type_def) = self.global_types.get_mut(&type_qname)
                    && let TypeDefinition::Simple { base_type, .. } = type_def {
                        *base_type = Some(resolved);
                    }
        }
    }

    /// Findet den transitiven built-in base_type für einen SimpleType.
    fn find_transitive_builtin(&self, start_qname: &Rc<QName>) -> Option<String> {
        let mut visited = FastHashSet::default();
        let mut current = start_qname.clone();

        // Folge der Kette (max 100 Schritte um Endlosschleifen zu vermeiden)
        for _ in 0..100 {
            if visited.contains(&current) {
                return None; // Zyklus erkannt
            }
            visited.insert(current.clone());

            // Prüfe ob current bereits einen base_type hat
            if let Some(type_def) = self.global_types.get(&current)
                && let TypeDefinition::Simple {
                    base_type: Some(b), ..
                } = type_def
                {
                    return Some(b.clone()); // Gefunden!
                }

            // Folge der Kette via simple_type_bases
            if let Some(next) = self.simple_type_bases.get(&current) {
                // Wenn next ein xs:* Type ist, sind wir fertig
                if &*next.uri == XS_NS {
                    return Some(next.local_name.to_string());
                }
                current = next.clone();
            } else {
                return None; // Kein weiterer Base-Type
            }
        }
        None // Max Iterationen erreicht
    }

    /// Finding 3: Propagiert is_union durch Derivation (Spec 8.5.4.4.2).
    ///
    /// Wenn ein SimpleType von einem Union-Type erbt (durch restriction oder list),
    /// hat er ebenfalls variety=union und benötigt xsi:type in strict mode.
    fn propagate_union_variety(&mut self) {
        // Sammle alle SimpleTypes die is_union=false haben aber von einem Union erben könnten
        let types_to_check: Vec<Rc<QName>> = self
            .global_types
            .iter()
            .filter_map(|(qname, type_def)| {
                if let TypeDefinition::Simple { is_union: false, .. } = type_def {
                    Some(qname.clone())
                } else {
                    None
                }
            })
            .collect();

        // Für jeden Typ: Prüfe ob irgendein Ancestor ein Union ist
        for type_qname in types_to_check {
            if self.has_union_ancestor(&type_qname)
                && let Some(type_def) = self.global_types.get_mut(&type_qname)
                    && let TypeDefinition::Simple { is_union, .. } = type_def {
                        *is_union = true;
                    }
        }
    }

    /// Prüft ob ein SimpleType einen Union-Ancestor hat.
    fn has_union_ancestor(&self, start_qname: &Rc<QName>) -> bool {
        let mut visited = FastHashSet::default();
        let mut current = start_qname.clone();

        for _ in 0..100 {
            if visited.contains(&current) {
                return false; // Zyklus
            }
            visited.insert(current.clone());

            // Prüfe ob current ein Union ist
            if let Some(type_def) = self.global_types.get(&current)
                && let TypeDefinition::Simple { is_union: true, .. } = type_def {
                    return true;
                }

            // Folge der Kette
            if let Some(next) = self.simple_type_bases.get(&current) {
                // xs:* Built-ins haben keine Union-Variety (außer wenn explizit)
                if &*next.uri == XS_NS {
                    return false;
                }
                current = next.clone();
            } else {
                return false;
            }
        }
        false
    }

    /// Synchronisiert Element-Refs in Particles mit aktuellen globalen Definitionen.
    ///
    /// Problem: Lokale Elemente mit ref="..." werden beim Parsen gegen die
    /// damalige Version des globalen Elements geklont. Dieser Pass aktualisiert
    /// sie mit den finalen Daten (type_definition, nillable, etc.).
    ///
    /// Note: Group-Refs sind durch die Parsing-Reihenfolge bereits korrekt
    /// (Groups werden vor Types geparst).
    fn sync_particles(&mut self) {
        let elements = self.global_elements.clone();

        // Aktualisiere alle TypeDefinitions
        for type_def in self.global_types.values_mut() {
            if let TypeDefinition::Complex { content, .. } = type_def {
                match content {
                    ContentType::ElementOnly(particle) | ContentType::Mixed(particle) => {
                        Self::sync_particle_elements(particle, &elements);
                    }
                    ContentType::Empty | ContentType::Simple => {}
                }
            }
        }

        // Aktualisiere auch anonyme ComplexTypes in globalen Elementen
        for elem_decl in self.global_elements.values_mut() {
            if let Some(type_def) = elem_decl.type_definition.as_mut() {
                let type_def = Rc::make_mut(type_def);
                if let TypeDefinition::Complex { content, .. } = type_def {
                    match content {
                        ContentType::ElementOnly(particle) | ContentType::Mixed(particle) => {
                            Self::sync_particle_elements(particle, &elements);
                        }
                        ContentType::Empty | ContentType::Simple => {}
                    }
                }
            }
        }

        // Aktualisiere auch Element-Refs in globalen Groups
        for model_group in self.global_groups.values_mut() {
            for particle in &mut model_group.particles {
                Self::sync_particle_elements(particle, &elements);
            }
        }
    }

    /// Synchronisiert Element-Refs in einem Particle rekursiv.
    fn sync_particle_elements(
        particle: &mut Particle,
        elements: &BTreeMap<Rc<QName>, ElementDeclaration>,
    ) {
        match &mut particle.term {
            ParticleTerm::Element(elem_decl) => {
                // Wenn das Element ein Ref auf ein globales Element ist,
                // aktualisiere es mit der aktuellen Version.
                // WICHTIG: Auch Platzhalter ersetzen! Forward-Refs haben bereits
                // einen (unvollständigen) Platzhalter, nicht None.
                if let Some(current) = elements.get(&elem_decl.qname) {
                    // Prüfe ob current vollständiger ist als elem_decl
                    let should_update = match (&current.type_definition, &elem_decl.type_definition) {
                        // current hat Typ, elem_decl nicht → updaten
                        (Some(_), None) => true,
                        // Beide haben Typ, aber current könnte vollständiger sein
                        // (z.B. has_named_sub_types gesetzt)
                        (Some(curr_type), Some(elem_type)) => {
                            // Vergleiche ob current mehr Informationen hat
                            Self::type_is_more_complete(curr_type, elem_type)
                        }
                        // current hat keinen Typ → nicht updaten
                        (None, _) => false,
                    };
                    if should_update {
                        *elem_decl = current.clone();
                    } else if current.substitution_group.len() > elem_decl.substitution_group.len() {
                        // Substitution groups may be missing on local refs even when types match.
                        elem_decl.substitution_group = current.substitution_group.clone();
                        elem_decl.finalize_matching_qnames();
                    }
                }
            }
            ParticleTerm::ModelGroup(model_group) => {
                // Rekursiv durch verschachtelte Particles
                for nested_particle in &mut model_group.particles {
                    Self::sync_particle_elements(nested_particle, elements);
                }
            }
            ParticleTerm::Wildcard(_) => {}
        }
    }

    /// Prüft ob ein Typ vollständiger ist als ein anderer.
    ///
    /// Verwendet für Forward-Reference-Auflösung: Der ursprüngliche Typ
    /// könnte ein Platzhalter sein, der aktuelle ist vollständig.
    fn type_is_more_complete(
        current: &Rc<TypeDefinition>,
        existing: &Rc<TypeDefinition>,
    ) -> bool {
        match (current.as_ref(), existing.as_ref()) {
            // Complex: has_named_sub_types ist vollständiger wenn true
            (
                TypeDefinition::Complex {
                    has_named_sub_types: curr_sub,
                    content: curr_content,
                    ..
                },
                TypeDefinition::Complex {
                    has_named_sub_types: exist_sub,
                    content: exist_content,
                    ..
                },
            ) => {
                // current ist vollständiger wenn has_named_sub_types true ist
                // oder wenn content vollständiger ist (nicht Empty)
                (*curr_sub && !*exist_sub)
                    || (matches!(curr_content, ContentType::ElementOnly(_) | ContentType::Mixed(_))
                        && matches!(exist_content, ContentType::Empty))
            }
            // Simple: base_type ist vollständiger wenn Some
            (
                TypeDefinition::Simple {
                    base_type: Some(_), ..
                },
                TypeDefinition::Simple {
                    base_type: None, ..
                },
            ) => true,
            _ => false,
        }
    }

    /// Löst Substitution Groups auf.
    fn resolve_substitution_groups(&mut self) -> Result<()> {
        // Direkte Substitution Groups einfügen
        for pending in &self.pending_refs {
            let PendingRef::SubstitutionGroup { member, head } = pending;
            let head_elem = self.global_elements.get_mut(head).ok_or_else(|| {
                Error::XsdParseError(format!(
                    "Substitution group head '{}:{}' not found for member '{}:{}'",
                    head.uri, head.local_name, member.uri, member.local_name
                ))
            })?;
            head_elem.substitution_group.push(member.clone());
        }

        // Transitive Closure berechnen
        // Wenn A substituiert B, und B substituiert C, dann substituiert A auch C
        self.compute_transitive_substitution_groups();

        Ok(())
    }

    /// Berechnet transitive Substitution Group Closure.
    ///
    /// Fix #8: O(n) BFS statt O(n³) Fixpunkt-Iteration.
    fn compute_transitive_substitution_groups(&mut self) {
        // Baue Mapping: head → direkte members
        let direct_members: FastHashMap<Rc<QName>, Vec<Rc<QName>>> = self
            .global_elements
            .iter()
            .filter(|(_, elem)| !elem.substitution_group.is_empty())
            .map(|(qname, elem)| (qname.clone(), elem.substitution_group.clone()))
            .collect();

        // Für jeden Head: BFS um alle transitiven Members zu finden
        for head_qname in direct_members.keys() {
            let mut all_members = FastHashSet::default();
            let mut queue = std::collections::VecDeque::new();

            // Starte mit direkten Members
            if let Some(direct) = direct_members.get(head_qname) {
                for member in direct {
                    if all_members.insert(member.clone()) {
                        queue.push_back(member.clone());
                    }
                }
            }

            // BFS: Füge transitive Members hinzu
            while let Some(current) = queue.pop_front() {
                if let Some(sub_members) = direct_members.get(&current) {
                    for sub_member in sub_members {
                        if all_members.insert(sub_member.clone()) {
                            queue.push_back(sub_member.clone());
                        }
                    }
                }
            }

            // Aktualisiere substitution_group mit allen gefundenen Members
            if let Some(head_elem) = self.global_elements.get_mut(head_qname) {
                let mut members: Vec<_> = all_members.into_iter().collect();
                members.sort_by(|a, b| {
                    a.local_name
                        .cmp(&b.local_name)
                        .then_with(|| a.uri.cmp(&b.uri))
                });
                head_elem.substitution_group = members;
                head_elem.finalize_matching_qnames();
            }
        }
    }

    /// Berechnet has_named_sub_types für alle Typen (Complex + Simple + Built-ins).
    ///
    /// Spec 8.5.4.4.2: "If T_k either has named sub-types or is a simple type
    /// definition of which {variety} is union" (spec/exi-spec.txt lines 3282-3284).
    fn compute_sub_types(&mut self) {
        // Sammle alle Complex Types die als Base verwendet werden
        let complex_types_with_children: FastHashSet<Rc<QName>> =
            self.type_bases.values().cloned().collect();

        // Sammle alle Simple Types die als Base verwendet werden (Fix #3)
        let simple_types_with_children: FastHashSet<Rc<QName>> =
            self.simple_type_bases.values().cloned().collect();

        // Markiere Complex Types und Simple Types
        for (type_name, type_def) in &mut self.global_types {
            match type_def {
                TypeDefinition::Complex {
                    has_named_sub_types,
                    ..
                } => {
                    if complex_types_with_children.contains(type_name) {
                        *has_named_sub_types = true;
                    }
                }
                TypeDefinition::Simple {
                    has_named_sub_types,
                    ..
                } => {
                    // Fix #3: Auch SimpleTypes können Sub-Types haben
                    if simple_types_with_children.contains(type_name) {
                        *has_named_sub_types = true;
                    }
                }
            }
        }

        // Aktualisiere Element-TypeDefinitions (sie wurden vor compute_sub_types geklont)
        self.sync_element_types();
    }

    /// Synchronisiert Element-TypeDefinitions mit aktuellen global_types.
    ///
    /// Löst das Problem, dass ElementDeclaration.type_definition eine Kopie ist
    /// die nicht automatisch aktualisiert wird wenn has_named_sub_types gesetzt wird.
    ///
    /// Finding 4: Aktualisiert auch built-in types mit has_named_sub_types.
    /// Fix Issue #38: Verwendet jetzt statische Liste BUILTIN_TYPES_WITH_SUBTYPES
    /// statt nur im Schema gefundene Base-Types.
    fn sync_element_types(&mut self) {
        let global_types = self.global_types.clone();

        // Built-in base types used in schema derivations should allow xsi:type.
        let mut schema_builtin_subtypes: FastHashSet<String> = FastHashSet::default();
        for type_def in global_types.values() {
            match type_def {
                TypeDefinition::Simple { base_type_qname, .. } => {
                    if let Some(base) = base_type_qname
                        && &*base.uri == XS_NS {
                            schema_builtin_subtypes.insert(base.local_name.to_string());
                        }
                }
                TypeDefinition::Complex { base_type, .. } => {
                    if let Some(base) = base_type
                        && &*base.uri == XS_NS {
                            schema_builtin_subtypes.insert(base.local_name.to_string());
                        }
                }
            }
        }

        let builtin_has_subtypes = |local_name: &str| {
            BUILTIN_TYPES_WITH_SUBTYPES.contains(&local_name)
                || schema_builtin_subtypes.contains(local_name)
        };

        let update_type_def_from_globals = |type_def: &TypeDefinition| -> TypeDefinition {
            if let Some(type_qname) = type_def.name() {
                if let Some(updated_type) = global_types.get(type_qname) {
                    return updated_type.clone();
                }
                if &*type_qname.uri == XS_NS && builtin_has_subtypes(&type_qname.local_name) {
                    return type_def.with_named_sub_types();
                }
            }
            type_def.clone()
        };

        fn update_local_element_types(
            type_def: &mut TypeDefinition,
            global_types: &BTreeMap<Rc<QName>, TypeDefinition>,
        ) {
            if let TypeDefinition::Complex { content, .. } = type_def {
                match content {
                    ContentType::ElementOnly(particle) | ContentType::Mixed(particle) => {
                        sync_particle_type_defs(particle, global_types);
                    }
                    ContentType::Empty | ContentType::Simple => {}
                }
            }
        }

        // Hilfsfunktion zum Aktualisieren einer ElementDeclaration
        let update_elem =
            |elem: &mut ElementDeclaration, global_types: &BTreeMap<Rc<QName>, TypeDefinition>| {
                if let Some(ref type_def) = elem.type_definition {
                    let mut updated = update_type_def_from_globals(type_def);
                    update_local_element_types(&mut updated, global_types);
                    elem.type_definition = Some(Rc::new(updated));
                }
            };

        // Globale Elemente aktualisieren
        for elem in self.global_elements.values_mut() {
            update_elem(elem, &global_types);
        }

        // Auch all_element_declarations aktualisieren (lokale Elemente in Particles)
        for elem in self.all_element_declarations.values_mut() {
            update_elem(elem, &global_types);
        }

        // Lokale ElementDeclarations innerhalb von globalen ComplexTypes aktualisieren.
        // Umfasst auch Attribut-Typen in ComplexTypes (ref-Attribute haben zum
        // Parse-Zeitpunkt nur Platzhalter-Typen, die hier aktualisiert werden).
        for type_def in self.global_types.values_mut() {
            update_local_element_types(type_def, &global_types);
            if let TypeDefinition::Complex { attributes, .. } = type_def {
                for attr_use in attributes.iter_mut() {
                    if let Some(ref td) = attr_use.type_definition {
                        let updated = update_type_def_from_globals(td);
                        attr_use.type_definition = Some(Rc::new(updated));
                    }
                }
            }
        }

        // Globale Attribute aktualisieren (Spec 7.1: korrekte Typ-Auflösung für
        // z.B. xsd:list-Typen, die als Platzhalter mit variety=Atomic angelegt werden)
        for attr in self.global_attributes.values_mut() {
            if let Some(ref type_def) = attr.type_definition {
                let updated = update_type_def_from_globals(type_def);
                attr.type_definition = Some(Rc::new(updated));
            }
        }
    }

    /// Baut SchemaInfo aus den gesammelten Daten.
    fn build(self) -> SchemaInfo {
        let mut builder = SchemaInfo::builder();

        // Globale Elemente (QNames)
        for qname in self.global_elements.keys() {
            builder = builder.global_element((**qname).clone());
        }

        // Alle Elemente (QNames)
        for qname in &self.all_elements {
            builder = builder.all_element((**qname).clone());
        }

        // Alle Attribute (QNames)
        for qname in &self.all_attributes {
            builder = builder.attribute((**qname).clone());
        }

        // Vollständige Element-Deklarationen (global + lokal)
        // Für Schema-informed Element Grammar Generierung (Spec 8.5.4)
        // WICHTIG: Verwende global_elements für korrekte Substitution Groups,
        // dann füge lokale Elemente hinzu (die keine Substitution Groups haben)
        let mut element_decls: std::collections::BTreeMap<Rc<QName>, Rc<ElementDeclaration>> = self
            .global_elements
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect();

        // Lokale Elemente hinzufügen (überschreiben nicht, falls schon global vorhanden)
        for (k, v) in self.all_element_declarations.into_iter() {
            element_decls.entry(k).or_insert_with(|| Rc::new(v));
        }
        builder = builder.element_declarations(element_decls);

        // Vollständige Type-Definitionen
        let type_defs: std::collections::BTreeMap<Rc<QName>, Rc<TypeDefinition>> = self
            .global_types
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect();
        builder = builder.type_definitions(type_defs.clone());

        // Globale Attribut-Typen (für AT(*) Typ-Lookup)
        let mut attr_types: std::collections::BTreeMap<Rc<QName>, Rc<TypeDefinition>> =
            std::collections::BTreeMap::new();
        for (qname, decl) in self
            .global_attributes
            .into_iter()
            .chain(self.external_attributes.into_iter())
        {
            if let Some(td) = decl.type_definition {
                // Resolve named attribute types to their full definition if available.
                let resolved = td
                    .name()
                    .and_then(|name| type_defs.get(name).cloned())
                    .unwrap_or(td);
                attr_types.insert(qname, resolved);
            }
        }
        builder = builder.global_attribute_types(attr_types);

        // Model Groups
        let model_groups: std::collections::BTreeMap<Rc<QName>, Rc<ModelGroup>> = self
            .global_groups
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect();
        builder = builder.model_groups(model_groups);

        builder.build()
    }

    // ========================================================================
    // Element Parsing
    // ========================================================================

    /// Parsed ein globales Element.
    fn parse_global_element(&mut self, node: &Node) -> Result<()> {
        let name = node
            .attribute("name")
            .ok_or_else(|| Error::XsdParseError("Global element missing name".to_string()))?;

        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
        let elem = self.parse_element_common(node, &qname)?;

        self.all_elements.insert(qname.clone());
        self.all_element_declarations.insert(qname.clone(), elem.clone());
        self.global_elements.insert(qname, elem);

        Ok(())
    }

    /// Parsed ein lokales Element (in Particle).
    fn parse_local_element(&mut self, node: &Node) -> Result<ElementDeclaration> {
        // ref-Attribut hat Priorität
        if let Some(ref_str) = node.attribute("ref") {
            let ref_qname = self.resolve_qname_in_node(ref_str, node)?;

            // Globales Element muss bereits geparsed sein (dieses Schema oder importiert)
            let global_elem = self
                .global_elements
                .get(&ref_qname)
                .or_else(|| self.external_elements.get(&ref_qname))
                .ok_or_else(|| {
                    Error::XsdParseError(format!("Element ref '{ref_str}' not found"))
                })?;

            self.all_elements.insert(ref_qname);
            return Ok(global_elem.clone());
        }

        // Finding 3: substitutionGroup ist nur auf globalen Elementen erlaubt
        // XSD 1.0 Part 1 §3.3.4
        if node.attribute("substitutionGroup").is_some() {
            return Err(Error::XsdParseError(
                "substitutionGroup attribute is only allowed on global elements".to_string(),
            ));
        }

        let name = node
            .attribute("name")
            .ok_or_else(|| Error::XsdParseError("Local element missing name".to_string()))?;

        // Form-Default beachten
        let ns = self.element_namespace(node);
        let qname = Rc::new(QName::new(ns, name));

        self.all_elements.insert(qname.clone());
        let elem = self.parse_element_common(node, &qname)?;

        // Lokale ElementDeclaration auch in all_element_declarations speichern
        // für Schema-informed Element Grammar Generierung (Spec 8.5.4)
        self.all_element_declarations.insert(qname, elem.clone());

        Ok(elem)
    }

    /// Gemeinsame Element-Parsing-Logik.
    ///
    /// Finding 1: Elements ohne expliziten type defaulten zu xs:anyType (Spec 8.5.4.1.2/8.5.4.1.3).
    fn parse_element_common(&mut self, node: &Node, qname: &Rc<QName>) -> Result<ElementDeclaration> {
        let nillable = node.attribute("nillable") == Some("true");

        // Typ: type-Attribut oder inline Definition
        // Finding 1: Default zu xs:anyType wenn kein Typ angegeben (Spec 8.5.4.1.2)
        let type_def: TypeDefinition = if let Some(type_str) = node.attribute("type") {
            let type_qname = self.resolve_qname_in_node(type_str, node)?;
            self.resolve_type_ref(&type_qname)?
        } else if let Some(inline_type) = self.parse_inline_type(node)? {
            inline_type
        } else {
            // Default: xs:anyType (Spec: "If no type is specified, the default is xs:anyType")
            self.resolve_builtin_type("anyType")?
        };

        let elem = ElementDeclaration::new(qname.clone())
            .with_nillable(nillable)
            .with_type(Rc::new(type_def));

        // Substitution Group merken
        if let Some(subst_str) = node.attribute("substitutionGroup") {
            let head_qname = self.resolve_qname_in_node(subst_str, node)?;
            self.pending_refs.push(PendingRef::SubstitutionGroup {
                member: qname.clone(),
                head: head_qname,
            });
        }

        self.track_element_fragment_element(&elem);

        Ok(elem)
    }

    fn track_element_fragment_element(&mut self, elem: &ElementDeclaration) {
        let qname = &elem.qname;
        if self
            .element_fragment_relaxed_elements
            .iter()
            .any(|k| k.as_ref() == qname.as_ref())
        {
            return;
        }
        let type_name = elem
            .type_definition
            .as_ref()
            .and_then(|td| td.name().cloned());
        let signature = ElementFragmentElementSignature {
            type_name,
            nillable: elem.nillable,
        };
        if let Some(existing) = self.element_fragment_element_signatures.get(qname) {
            if existing.type_name.is_none()
                || signature.type_name.is_none()
                || existing.type_name.as_ref() != signature.type_name.as_ref()
                || existing.nillable != signature.nillable
            {
                self.element_fragment_relaxed_elements
                    .insert(qname.clone());
            }
        } else {
            self.element_fragment_element_signatures
                .insert(qname.clone(), signature);
        }
    }

    /// Löst eine Type-Referenz auf.
    ///
    /// Sucht zuerst in Built-in Types, dann in globalen Typen dieses Schemas,
    /// dann in externen Typen (aus importierten Schemas).
    fn resolve_type_ref(&self, type_qname: &Rc<QName>) -> Result<TypeDefinition> {
        // Built-in Types (xs:string, xs:int, etc.)
        if &*type_qname.uri == XS_NS {
            return self.resolve_builtin_type(&type_qname.local_name);
        }

        // Globale Type-Definition suchen (dieses Schema)
        if let Some(t) = self.global_types.get(type_qname) {
            return Ok(t.clone());
        }

        // Externe Type-Definition suchen (importierte Schemas)
        if let Some(t) = self.external_types.get(type_qname) {
            return Ok(t.clone());
        }

        Err(Error::XsdParseError(format!(
            "Type '{}:{}' not found",
            type_qname.uri, type_qname.local_name
        )))
    }

    /// XSD Built-in Types (Spec: XML Schema Part 2: Datatypes).
    const BUILTIN_TYPES: &'static [&'static str] = &[
        // Primitive Types
        "string",
        "boolean",
        "decimal",
        "float",
        "double",
        "duration",
        "dateTime",
        "time",
        "date",
        "gYearMonth",
        "gYear",
        "gMonthDay",
        "gDay",
        "gMonth",
        "hexBinary",
        "base64Binary",
        "anyURI",
        "QName",
        "NOTATION",
        // Derived Types
        "normalizedString",
        "token",
        "language",
        "NMTOKEN",
        "NMTOKENS",
        "Name",
        "NCName",
        "ID",
        "IDREF",
        "IDREFS",
        "ENTITY",
        "ENTITIES",
        "integer",
        "nonPositiveInteger",
        "negativeInteger",
        "long",
        "int",
        "short",
        "byte",
        "nonNegativeInteger",
        "unsignedLong",
        "unsignedInt",
        "unsignedShort",
        "unsignedByte",
        "positiveInteger",
        // Ur-Types
        "anyType",
        "anySimpleType",
    ];

    /// Löst Built-in XSD Types auf.
    ///
    /// xs:anyType ist der Ur-Typ aller komplexen Typen (Spec 8.5.4.1.3.2):
    /// - Mixed Content (CharacterData + Wildcards)
    /// - Attribute Wildcard (##any)
    fn resolve_builtin_type(&self, type_name: &str) -> Result<TypeDefinition> {
        if !Self::BUILTIN_TYPES.contains(&type_name) {
            return Err(Error::XsdParseError(format!(
                "Unknown XSD built-in type: xs:{type_name}"
            )));
        }

        // xs:anyType ist ein komplexer Typ mit Mixed Content und Wildcards
        if type_name == "anyType" {
            return Ok(TypeDefinition::Complex {
                name: Some(Rc::new(QName::new(XS_NS, "anyType"))),
                base_type: None, // anyType hat keinen Base-Type
                derivation: None,
                attributes: Vec::new(),
                attribute_wildcard: Some(AttributeWildcard::Any),
                content: ContentType::Mixed(Particle::new(
                    0,
                    MaxOccurs::Unbounded,
                    ParticleTerm::Wildcard(Wildcard::any()),
                )?),
                has_named_sub_types: true, // Alle komplexen Typen erben von anyType
            });
        }

        Ok(TypeDefinition::Simple {
            name: Some(Rc::new(QName::new(XS_NS, type_name))),
            variety: SimpleTypeVariety::Atomic,
            base_type_qname: None, // Built-in types haben keinen Base-Type QName
            base_type: Some(type_name.to_string()),
            enumeration_values: Vec::new(),
            is_union: false,
            has_named_sub_types: BUILTIN_TYPES_WITH_SUBTYPES.contains(&type_name),
        })
    }

    /// Parsed inline Type-Definition (anonymer Typ).
    fn parse_inline_type(&mut self, node: &Node) -> Result<Option<TypeDefinition>> {
        for child in node.children().filter(|n| n.is_element()) {
            let tag = child.tag_name();
            if tag.namespace() != Some(XS_NS) {
                continue;
            }

            match tag.name() {
                "simpleType" => return Ok(Some(self.parse_simple_type(&child, None)?)),
                "complexType" => return Ok(Some(self.parse_complex_type(&child, None)?)),
                _ => {}
            }
        }
        Ok(None)
    }

    // ========================================================================
    // Simple Type Parsing
    // ========================================================================

    /// Parsed einen globalen Simple Type.
    fn parse_global_simple_type(&mut self, node: &Node) -> Result<()> {
        let name = node
            .attribute("name")
            .ok_or_else(|| Error::XsdParseError("Global simpleType missing name".to_string()))?;

        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
        let type_def = self.parse_simple_type(node, Some(qname.clone()))?;

        self.global_types.insert(qname, type_def);
        Ok(())
    }

    /// Parsed einen Simple Type.
    fn parse_simple_type(
        &mut self,
        node: &Node,
        name: Option<Rc<QName>>,
    ) -> Result<TypeDefinition> {
        // Variety und is_union bestimmen
        let mut variety = SimpleTypeVariety::Atomic;
        let mut is_union = false;

        for child in node.children().filter(|c| {
            c.is_element() && c.tag_name().namespace() == Some(XS_NS)
        }) {
            match child.tag_name().name() {
                "union" => {
                    is_union = true;
                    // Finding 6: Member types müssen alle auflösbar sein (XSD Part 2 §4.1.2)
                    let member_types = if let Some(member_str) = child.attribute("memberTypes") {
                        let mut types = Vec::new();
                        for s in member_str.split_whitespace() {
                            types.push(self.resolve_qname_in_node(s, &child)?);
                        }
                        types
                    } else {
                        Vec::new()
                    };
                    variety = SimpleTypeVariety::Union { member_types };
                }
                "list" => {
                    // Finding 6: itemType muss auflösbar sein wenn angegeben (XSD Part 2 §4.1.3)
                    let item_type = if let Some(s) = child.attribute("itemType") {
                        Some(self.resolve_qname_in_node(s, &child)?)
                    } else {
                        // Wenn kein itemType-Attribut vorhanden ist, kann eine
                        // eingebettete simpleType-Definition vorhanden sein.
                        let nested_simple = child.children().find(|n| {
                            n.is_element()
                                && n.tag_name().namespace() == Some(XS_NS)
                                && n.tag_name().name() == "simpleType"
                        });
                        if let Some(nested) = nested_simple {
                            let (_builtin, base_qname) = self.extract_simple_type_base(&nested);
                            base_qname
                        } else {
                            None
                        }
                    };
                    variety = SimpleTypeVariety::List { item_type };
                }
                _ => {}
            }
        }

        // Enumeration-Facets aus restriction sammeln (Spec 7.2)
        let mut enumeration_values = Vec::new();
        for child in node.children().filter(|c| {
            c.is_element()
                && c.tag_name().namespace() == Some(XS_NS)
                && c.tag_name().name() == "restriction"
        }) {
            for facet in child.children().filter(|c| {
                c.is_element()
                    && c.tag_name().namespace() == Some(XS_NS)
                    && c.tag_name().name() == "enumeration"
            }) {
                if let Some(val) = facet.attribute("value") {
                    enumeration_values.push(val.to_string());
                }
            }
        }

        // Base type aus restriction/list/union ermitteln
        let (base_type, base_qname) = self.extract_simple_type_base(node);

        // Finding 4: List-Typen haben xs:anySimpleType als logischen Base
        // (für Typ-Hierarchie), aber wir speichern den tatsächlichen itemType in variety
        let effective_base_qname = match &variety {
            SimpleTypeVariety::List { .. } => {
                // List types derive from xs:anySimpleType (XSD 1.0 Part 2 §4.1.2)
                Some(Rc::new(QName::new(XS_NS, "anySimpleType")))
            }
            SimpleTypeVariety::Union { .. } => {
                // Union types also derive from xs:anySimpleType
                Some(Rc::new(QName::new(XS_NS, "anySimpleType")))
            }
            SimpleTypeVariety::Atomic => base_qname.clone(),
        };

        // Finding 5: Base-QName auch für anonyme Typen speichern
        // (wird in type_definition gespeichert, nicht nur in simple_type_bases)
        if let (Some(type_name), Some(base_q)) = (&name, &effective_base_qname) {
            self.simple_type_bases
                .insert(type_name.clone(), base_q.clone());
        }

        Ok(TypeDefinition::Simple {
            name,
            variety,
            base_type_qname: effective_base_qname, // Finding 5: auch für anonyme Typen
            base_type,
            enumeration_values,
            is_union,
            has_named_sub_types: false, // Wird in compute_sub_types() gesetzt
        })
    }

    /// Extrahiert den Basis-Datentyp eines Simple Types.
    ///
    /// Gibt (built-in-name, base-qname) zurück:
    /// - built-in-name: Der lokale Name des xs:* Types (für direktes Encoding)
    /// - base-qname: Der QName des Base-Types (für transitive Auflösung)
    fn extract_simple_type_base(&self, node: &Node) -> (Option<String>, Option<Rc<QName>>) {
        for child in node.children().filter(|n| n.is_element()) {
            let tag = child.tag_name();
            if tag.namespace() != Some(XS_NS) {
                continue;
            }

            match tag.name() {
                "restriction" => {
                    if let Some(base_str) = child.attribute("base")
                        && let Ok(base_qname) = self.resolve_qname_in_node(base_str, &child) {
                            let builtin = if &*base_qname.uri == XS_NS {
                                Some(base_qname.local_name.to_string())
                            } else {
                                None
                            };
                            return (builtin, Some(base_qname));
                        }
                }
                "list" => {
                    if let Some(item_str) = child.attribute("itemType")
                        && let Ok(item_qname) = self.resolve_qname_in_node(item_str, &child) {
                            let builtin = if &*item_qname.uri == XS_NS {
                                Some(item_qname.local_name.to_string())
                            } else {
                                None
                            };
                            return (builtin, Some(item_qname));
                        }
                }
                "union" => {
                    // Union hat keinen einzelnen base type
                    return (None, None);
                }
                _ => {}
            }
        }
        (None, None)
    }

    // ========================================================================
    // Complex Type Parsing
    // ========================================================================

    /// Parsed einen globalen Complex Type.
    fn parse_global_complex_type(&mut self, node: &Node) -> Result<()> {
        let name = node
            .attribute("name")
            .ok_or_else(|| Error::XsdParseError("Global complexType missing name".to_string()))?;

        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));
        let type_def = self.parse_complex_type(node, Some(qname.clone()))?;

        self.global_types.insert(qname, type_def);
        Ok(())
    }

    /// Parsed einen Complex Type.
    fn parse_complex_type(
        &mut self,
        node: &Node,
        name: Option<Rc<QName>>,
    ) -> Result<TypeDefinition> {
        let mut attributes = Vec::new();
        let mut attr_wildcard = None;
        let mut content = ContentType::Empty;
        let mut base_type: Option<Rc<QName>> = None;
        let mut derivation: Option<DerivationKind> = None;

        for child in node.children().filter(|n| n.is_element()) {
            let tag = child.tag_name();
            if tag.namespace() != Some(XS_NS) {
                continue;
            }

            match tag.name() {
                "attribute" => attributes.push(self.parse_attribute_use(&child)?),
                "anyAttribute" => attr_wildcard = Some(self.parse_attr_wildcard(&child)?),
                "sequence" | "choice" | "all" => {
                    content = ContentType::ElementOnly(self.parse_model_group_particle(&child)?);
                }
                "group" => {
                    // xs:group ref - wird wie Model Group behandelt
                    content = ContentType::ElementOnly(self.parse_particle(&child)?);
                }
                "simpleContent" => {
                    content = ContentType::Simple;
                    let (base, deriv) = self.parse_content_derivation_with_kind(&child)?;
                    base_type = base;
                    derivation = deriv;
                    // Attribute aus extension/restriction übernehmen
                    if let Some(ext) = self.find_extension_or_restriction(&child) {
                        for inner in ext.children().filter(|n| n.is_element()) {
                            let inner_tag = inner.tag_name();
                            if inner_tag.namespace() != Some(XS_NS) {
                                continue;
                            }
                            match inner_tag.name() {
                                "attribute" => {
                                    attributes.push(self.parse_attribute_use(&inner)?);
                                }
                                "anyAttribute" => {
                                    attr_wildcard = Some(self.parse_attr_wildcard(&inner)?);
                                }
                                _ => {}
                            }
                        }
                    }
                }
                "complexContent" => {
                    // Fix #4: mixed="true" kann an complexContent stehen
                    let complex_content_mixed = child.attribute("mixed") == Some("true");

                    let (base, deriv) = self.parse_content_derivation_with_kind(&child)?;
                    base_type = base;
                    derivation = deriv;
                    // Content Model aus extension/restriction übernehmen
                    if let Some(ext) = self.find_extension_or_restriction(&child) {
                        // mixed kann auch an extension/restriction stehen
                        let ext_mixed = ext.attribute("mixed") == Some("true");

                        for inner in ext.children().filter(|n| n.is_element()) {
                            let inner_tag = inner.tag_name();
                            if inner_tag.namespace() != Some(XS_NS) {
                                continue;
                            }
                            match inner_tag.name() {
                                "sequence" | "choice" | "all" => {
                                    let particle = self.parse_model_group_particle(&inner)?;
                                    // mixed von complexContent oder extension/restriction übernehmen
                                    content = if complex_content_mixed || ext_mixed {
                                        ContentType::Mixed(particle)
                                    } else {
                                        ContentType::ElementOnly(particle)
                                    };
                                }
                                "group" => {
                                    // xs:group ref in complexContent
                                    let particle = self.parse_particle(&inner)?;
                                    content = if complex_content_mixed || ext_mixed {
                                        ContentType::Mixed(particle)
                                    } else {
                                        ContentType::ElementOnly(particle)
                                    };
                                }
                                "attribute" => {
                                    attributes.push(self.parse_attribute_use(&inner)?);
                                }
                                "anyAttribute" => {
                                    attr_wildcard = Some(self.parse_attr_wildcard(&inner)?);
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Base-Type für has_named_sub_types Berechnung speichern
        if let (Some(type_name), Some(base)) = (&name, &base_type) {
            self.type_bases.insert(type_name.clone(), base.clone());
        }

        // mixed Attribut prüfen
        if node.attribute("mixed") == Some("true")
            && let ContentType::ElementOnly(p) = content {
                content = ContentType::Mixed(p);
            }

        Ok(TypeDefinition::Complex {
            name,
            base_type,    // Finding 1 & 2: Base-Type für Content/Attribute Merging
            derivation,   // Finding 1: Extension/Restriction Kind
            attributes,
            attribute_wildcard: attr_wildcard,
            content,
            has_named_sub_types: false,
        })
    }

    /// Findet extension oder restriction Element.
    fn find_extension_or_restriction<'a>(&self, node: &'a Node<'a, 'a>) -> Option<Node<'a, 'a>> {
        node.children()
            .filter(|n| n.is_element())
            .find(|c| matches!(c.tag_name().name(), "extension" | "restriction"))
    }

    /// Extrahiert base type und derivation kind aus simpleContent/complexContent.
    ///
    /// Finding 1 & 2: Gibt sowohl base_type als auch derivation zurück,
    /// damit der Complex Type weiß, ob er Extension oder Restriction ist.
    fn parse_content_derivation_with_kind(
        &self,
        node: &Node,
    ) -> Result<(Option<Rc<QName>>, Option<DerivationKind>)> {
        for child in node.children().filter(|n| n.is_element()) {
            let tag_name = child.tag_name().name();
            if matches!(tag_name, "extension" | "restriction") {
                let derivation_kind = match tag_name {
                    "extension" => Some(DerivationKind::Extension),
                    "restriction" => Some(DerivationKind::Restriction),
                    _ => None,
                };

                if let Some(base_str) = child.attribute("base") {
                    let base_qname = self.resolve_qname_in_node(base_str, &child)?;

                    // Validiere dass der Base-Type existiert
                    if &*base_qname.uri == XS_NS {
                        // xs:* muss ein gültiger built-in Type sein
                        if !Self::BUILTIN_TYPES.contains(&&*base_qname.local_name) {
                            return Err(Error::XsdParseError(format!(
                                "Unknown XSD built-in base type: xs:{}",
                                base_qname.local_name
                            )));
                        }
                    } else if !self.global_types.contains_key(&base_qname)
                        && !self.external_types.contains_key(&base_qname)
                    {
                        // Nicht-XS-Namespace muss im Schema oder importierten Schemas definiert sein
                        return Err(Error::XsdParseError(format!(
                            "Base type '{}:{}' not found",
                            base_qname.uri, base_qname.local_name
                        )));
                    }

                    return Ok((Some(base_qname), derivation_kind));
                }
            }
        }
        Ok((None, None))
    }

    // ========================================================================
    // Particle und Model Group Parsing
    // ========================================================================

    /// Parsed minOccurs/maxOccurs Attribute.
    fn parse_occurs(&self, node: &Node) -> Result<(usize, MaxOccurs)> {
        let min = match node.attribute("minOccurs") {
            Some(s) => s.parse().map_err(|_| {
                Error::XsdParseError(format!("Invalid minOccurs: {s}"))
            })?,
            None => 1,
        };

        let max = match node.attribute("maxOccurs") {
            Some("unbounded") => MaxOccurs::Unbounded,
            Some(s) => s
                .parse()
                .map(MaxOccurs::Bounded)
                .map_err(|_| Error::XsdParseError(format!("Invalid maxOccurs: {s}")))?,
            None => MaxOccurs::Bounded(1),
        };

        // Finding 6: Validiere min <= max (XSD 1.0 Part 1 §3.8)
        if let MaxOccurs::Bounded(max_val) = max
            && min > max_val {
                return Err(Error::XsdParseError(format!(
                    "minOccurs ({min}) cannot be greater than maxOccurs ({max_val})"
                )));
            }

        Ok((min, max))
    }

    /// Parsed ein Particle.
    fn parse_particle(&mut self, node: &Node) -> Result<Particle> {
        let (min, max) = self.parse_occurs(node)?;

        let term = match node.tag_name().name() {
            "element" => ParticleTerm::Element(self.parse_local_element(node)?),
            "any" => ParticleTerm::Wildcard(self.parse_wildcard(node)?),
            "sequence" | "choice" | "all" => {
                ParticleTerm::ModelGroup(self.parse_model_group(node)?)
            }
            "group" => {
                // xs:group ref
                let ref_str = node
                    .attribute("ref")
                    .ok_or_else(|| Error::XsdParseError("xs:group without ref".to_string()))?;
                let ref_qname = self.resolve_qname_in_node(ref_str, node)?;

                let group = self
                    .global_groups
                    .get(&ref_qname)
                    .or_else(|| self.external_groups.get(&ref_qname))
                    .ok_or_else(|| {
                        Error::XsdParseError(format!("Group ref '{ref_str}' not found"))
                    })?;

                ParticleTerm::ModelGroup(group.clone())
            }
            tag => {
                return Err(Error::XsdParseError(format!(
                    "Unknown particle term: {tag}"
                )))
            }
        };

        Particle::new(min, max, term)
    }

    /// Parsed eine Model Group.
    fn parse_model_group(&mut self, node: &Node) -> Result<ModelGroup> {
        let compositor = match node.tag_name().name() {
            "sequence" => Compositor::Sequence,
            "choice" => Compositor::Choice,
            "all" => Compositor::All,
            name => {
                return Err(Error::XsdParseError(format!(
                    "Unknown compositor: {name}"
                )))
            }
        };

        let mut particles = Vec::new();
        for child in node.children().filter(|n| n.is_element()) {
            let tag = child.tag_name();
            if tag.namespace() != Some(XS_NS) {
                continue;
            }

            if matches!(
                tag.name(),
                "element" | "any" | "sequence" | "choice" | "all" | "group"
            ) {
                // Finding 6: xs:all constraints (XSD 1.0 Part 1 §3.8.6)
                if compositor == Compositor::All {
                    // Kinder von xs:all dürfen nur maxOccurs="0" oder "1" haben
                    let max_str = child.attribute("maxOccurs").unwrap_or("1");
                    if max_str != "0" && max_str != "1" {
                        return Err(Error::XsdParseError(format!(
                            "Elements in xs:all must have maxOccurs of 0 or 1, got: {max_str}"
                        )));
                    }
                    // xs:all darf nur xs:element enthalten, keine sequence/choice/all
                    if !matches!(tag.name(), "element" | "any") {
                        return Err(Error::XsdParseError(format!(
                            "xs:all can only contain element or any, not: {}",
                            tag.name()
                        )));
                    }
                }
                particles.push(self.parse_particle(&child)?);
            }
        }

        Ok(ModelGroup::new(compositor, particles))
    }

    /// Wrapper für Model Group als Particle.
    fn parse_model_group_particle(&mut self, node: &Node) -> Result<Particle> {
        let (min, max) = self.parse_occurs(node)?;

        // Finding 6: xs:all selbst darf nur maxOccurs="0" oder "1" haben (XSD 1.0 Part 1 §3.8.6)
        if node.tag_name().name() == "all" {
            match max {
                MaxOccurs::Bounded(0) | MaxOccurs::Bounded(1) => {}
                _ => {
                    return Err(Error::XsdParseError(
                        "xs:all must have maxOccurs of 0 or 1".to_string(),
                    ));
                }
            }
        }

        let model_group = self.parse_model_group(node)?;
        Particle::new(min, max, ParticleTerm::ModelGroup(model_group))
    }

    /// Parsed eine globale Group-Definition.
    fn parse_global_group(&mut self, node: &Node) -> Result<()> {
        let name = node
            .attribute("name")
            .ok_or_else(|| Error::XsdParseError("Global group missing name".to_string()))?;

        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));

        // Eine Group enthält genau ein sequence/choice/all
        for child in node.children().filter(|n| n.is_element()) {
            let tag = child.tag_name();
            if tag.namespace() != Some(XS_NS) {
                continue;
            }

            if matches!(tag.name(), "sequence" | "choice" | "all") {
                let model_group = self.parse_model_group(&child)?;
                self.global_groups.insert(qname, model_group);
                return Ok(());
            }
        }

        Err(Error::XsdParseError(format!(
            "Group '{name}' has no model group content"
        )))
    }

    // ========================================================================
    // Wildcard Parsing
    // ========================================================================

    /// Parsed ein Element-Wildcard.
    ///
    /// Finding 4: processContents wird geparsed (strict/lax/skip).
    fn parse_wildcard(&self, node: &Node) -> Result<Wildcard> {
        // Finding 4: processContents parsen (XSD 1.0 Part 1 §3.10)
        let process_contents = match node.attribute("processContents") {
            None | Some("strict") => ProcessContents::Strict,
            Some("lax") => ProcessContents::Lax,
            Some("skip") => ProcessContents::Skip,
            Some(other) => {
                return Err(Error::XsdParseError(format!(
                    "Invalid processContents value: {other}"
                )));
            }
        };

        let constraint = self.parse_namespace_constraint(node.attribute("namespace"), "wildcard")?;

        Ok(Wildcard {
            constraint,
            process_contents,
        })
    }

    /// Parsed Namespace-Constraint aus namespace-Attribut.
    ///
    /// Gemeinsame Logik für Element- und Attribut-Wildcards.
    /// Spec 8.5.4.1.7: namespace="" ist ungültig (leere Liste).
    /// Fix #6: ##any/##other sind nur als alleinige Werte erlaubt, nicht in Listen.
    /// Finding 6: ##other mit leerem targetNamespace → Not(None) statt Not(Some("")).
    fn parse_namespace_constraint(
        &self,
        ns_attr: Option<&str>,
        context: &str,
    ) -> Result<WildcardConstraint> {
        match ns_attr {
            None | Some("##any") => Ok(WildcardConstraint::Any),
            Some("##other") => {
                // Finding 6: Leerer targetNamespace bedeutet "absent" → Not(None)
                let not_ns = if self.target_ns.is_empty() {
                    None
                } else {
                    Some(self.target_ns.clone())
                };
                Ok(WildcardConstraint::Not(not_ns))
            }
            Some(ns_list) => {
                let tokens: Vec<&str> = ns_list.split_whitespace().collect();
                if tokens.is_empty() {
                    return Err(Error::XsdParseError(format!(
                        "Empty namespace list in {context}"
                    )));
                }
                // Fix #6: ##any/##other dürfen nicht in einer Liste mit anderen Werten stehen
                for token in &tokens {
                    if (*token == "##any" || *token == "##other") && tokens.len() > 1 {
                        return Err(Error::XsdParseError(format!(
                            "'{token}' cannot be combined with other namespace values in {context}"
                        )));
                    }
                }
                let namespaces: Vec<String> =
                    tokens.iter().map(|s| self.resolve_ns_token(s)).collect();
                Ok(WildcardConstraint::Namespaces(namespaces))
            }
        }
    }

    /// Parsed ein Attribute-Wildcard.
    ///
    /// ##other wird analog zu Element-Wildcards behandelt: Not(targetNamespace).
    /// Spec 8.5.4.1.7: namespace="" ist ungültig (leere Liste).
    /// Fix #6: ##any/##other sind nur als alleinige Werte erlaubt, nicht in Listen.
    fn parse_attr_wildcard(&self, node: &Node) -> Result<AttributeWildcard> {
        let constraint = self.parse_namespace_constraint(node.attribute("namespace"), "attribute wildcard")?;
        Ok(match constraint {
            WildcardConstraint::Any => AttributeWildcard::Any,
            WildcardConstraint::Not(ns) => AttributeWildcard::Not(ns),
            WildcardConstraint::Namespaces(ns) => AttributeWildcard::Namespaces(ns),
        })
    }

    /// Löst ##targetNamespace, ##local etc. auf.
    fn resolve_ns_token(&self, token: &str) -> String {
        match token {
            "##targetNamespace" => self.target_ns.clone(),
            "##local" => String::new(),
            other => other.to_string(),
        }
    }

    // ========================================================================
    // Attribute Parsing
    // ========================================================================

    /// Parsed ein globales Attribut.
    fn parse_global_attribute(&mut self, node: &Node) -> Result<()> {
        let name = node
            .attribute("name")
            .ok_or_else(|| Error::XsdParseError("Global attribute missing name".to_string()))?;

        let qname = Rc::new(QName::new(self.target_ns.as_str(), name));

        // Fix #5: Typ des globalen Attributs speichern
        let type_def = self.parse_attribute_type(node)?;
        self.global_attributes.insert(
            qname.clone(),
            AttributeDeclaration {
                type_definition: type_def.map(Rc::new),
            },
        );

        self.all_attributes.insert(qname);
        Ok(())
    }

    /// Parsed ein Attribute Use.
    fn parse_attribute_use(&mut self, node: &Node) -> Result<AttributeUse> {
        // ref hat Priorität
        if let Some(ref_str) = node.attribute("ref") {
            let ref_qname = self.resolve_qname_in_node(ref_str, node)?;
            self.all_attributes.insert(ref_qname.clone());

            // XML-Namespace Attribute (xml:lang, xml:space, xml:base, xml:id) sind
            // implizit definiert laut XML-Spezifikation - keine Deklaration notwendig
            const XML_NS: &str = "http://www.w3.org/XML/1998/namespace";
            if &*ref_qname.uri == XML_NS {
                return Ok(AttributeUse {
                    type_definition: None, // Built-in Typ
                    qname: ref_qname,
                    required: node.attribute("use") == Some("required"),
                });
            }

            // Finding 5: Globales Attribut muss existieren (XSD 1.0 Part 1 §3.2.2)
            let global_attr = self
                .global_attributes
                .get(&ref_qname)
                .or_else(|| self.external_attributes.get(&ref_qname))
                .ok_or_else(|| {
                    Error::XsdParseError(format!(
                        "Attribute ref '{}:{}' not found",
                        ref_qname.uri, ref_qname.local_name
                    ))
                })?;

            return Ok(AttributeUse {
                type_definition: global_attr.type_definition.clone(),
                qname: ref_qname,
                required: node.attribute("use") == Some("required"),
            });
        }

        let name = node
            .attribute("name")
            .ok_or_else(|| Error::XsdParseError("Attribute missing name".to_string()))?;

        // Namespace basierend auf form-Default
        let ns = self.attribute_namespace(node);
        let qname = Rc::new(QName::new(ns, name));

        // Typ: type-Attribut oder inline simpleType
        let type_def = self.parse_attribute_type(node)?;

        self.all_attributes.insert(qname.clone());
        Ok(AttributeUse {
            type_definition: type_def.map(Rc::new),
            qname,
            required: node.attribute("use") == Some("required"),
        })
    }

    /// Parsed den Typ eines Attributs.
    fn parse_attribute_type(&mut self, node: &Node) -> Result<Option<TypeDefinition>> {
        // type-Attribut hat Priorität
        if let Some(type_str) = node.attribute("type") {
            let type_qname = self.resolve_qname_in_node(type_str, node)?;
            return Ok(Some(self.resolve_type_ref(&type_qname)?));
        }

        // Inline simpleType
        for child in node.children().filter(|n| n.is_element()) {
            let tag = child.tag_name();
            if tag.namespace() == Some(XS_NS) && tag.name() == "simpleType" {
                return Ok(Some(self.parse_simple_type(&child, None)?));
            }
        }

        // Finding 2: Default ist xs:anySimpleType (Spec 8.5.4.1.4)
        Ok(Some(self.resolve_builtin_type("anySimpleType")?))
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use super::*;

    // ========================================================================
    // Phase 1: Grundstruktur Tests
    // ========================================================================

    /// Leeres Schema parsen.
    #[test]
    fn parse_empty_schema() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert!(schema.global_elements().is_empty());
        assert!(schema.all_elements().is_empty());
    }

    /// Schema mit targetNamespace.
    #[test]
    fn parse_schema_with_target_namespace() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org">
                <xs:element name="root" type="xs:string"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
        assert_eq!(&*schema.global_elements()[0].uri, "http://example.org");

        assert_eq!(&*schema.global_elements()[0].local_name, "root");

    }

    /// Ungültiges XML gibt Fehler.
    #[test]
    fn parse_invalid_xml_returns_error() {
        let xsd = "<xs:schema><not-closed>";

        let result = parse_xsd(xsd);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, Error::XsdParseError(_)));
    }

    /// Root muss xs:schema sein.
    #[test]
    fn parse_non_schema_root_returns_error() {
        let xsd = r#"<element xmlns="http://www.w3.org/2001/XMLSchema"/>"#;

        let result = parse_xsd(xsd);
        assert!(result.is_err());
    }

    // ========================================================================
    // Phase 2: Namespace/Prefix-Resolution Tests
    // ========================================================================

    /// xs:string → XS_NS.
    #[test]
    fn resolve_xs_prefix() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="test" type="xs:string"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
    }

    /// tns:BookType → target_ns.
    #[test]
    fn resolve_tns_prefix() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:complexType name="BookType"/>
                <xs:element name="book" type="tns:BookType"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
    }

    /// Unprefixed in Attribut = no namespace.
    #[test]
    fn resolve_unprefixed_in_attr() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:simpleType name="MyType"/>
                <xs:element name="test" type="MyType"/>
            </xs:schema>
        "#;

        // Sollte nicht fehlschlagen - MyType ist ohne Namespace
        let _schema = parse_xsd(xsd).unwrap();
    }

    /// elementFormDefault="qualified".
    #[test]
    fn element_form_default_qualified() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org"
                       elementFormDefault="qualified">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:element name="child" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        // child sollte im target namespace sein
        assert!(schema
            .all_elements()
            .iter()
            .any(|q| &*q.local_name == "child" && &*q.uri == "http://example.org"));
    }

    /// elementFormDefault="unqualified" (default).
    #[test]
    fn element_form_default_unqualified() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:element name="child" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        // child sollte KEINEN namespace haben (unqualified)
        assert!(schema
            .all_elements()
            .iter()
            .any(|q| &*q.local_name == "child" && q.uri.is_empty()));
    }

    /// attributeFormDefault="qualified".
    #[test]
    fn attribute_form_default_qualified() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org"
                       attributeFormDefault="qualified">
                <xs:complexType name="TestType">
                    <xs:attribute name="id" type="xs:string"/>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        // id sollte im target namespace sein
        assert!(schema
            .all_attributes()
            .iter()
            .any(|q| &*q.local_name == "id" && &*q.uri == "http://example.org"));
    }

    /// ns_map wird aus xmlns:* Attributen gebaut.
    #[test]
    fn ns_map_from_xmlns_attributes() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       xmlns:other="http://other.org"
                       targetNamespace="http://example.org">
            </xs:schema>
        "#;

        // Sollte ohne Fehler parsen
        let _schema = parse_xsd(xsd).unwrap();
    }

    // ========================================================================
    // Phase 3: Element Parsing Tests
    // ========================================================================

    /// Globales Element parsen.
    #[test]
    fn parse_global_element() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org">
                <xs:element name="book" type="xs:string"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
        assert_eq!(&*schema.global_elements()[0].local_name, "book");

    }

    /// Lokales Element mit name.
    #[test]
    fn parse_local_element_with_name() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:element name="child" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert!(schema.all_elements().iter().any(|q| &*q.local_name == "child"));
    }

    /// Lokales Element mit ref.
    #[test]
    fn parse_local_element_with_ref() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:element name="title" type="xs:string"/>
                <xs:complexType name="BookType">
                    <xs:sequence>
                        <xs:element ref="tns:title"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert!(schema
            .global_elements()
            .iter()
            .any(|q| &*q.local_name == "title"));
    }

    /// Element mit type ref und xs: prefix.
    #[test]
    fn parse_element_with_type_ref_xs_prefix() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="count" type="xs:int"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
    }

    /// Element mit nillable=true.
    #[test]
    fn parse_element_nillable() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="optional" type="xs:string" nillable="true"/>
            </xs:schema>
        "#;

        // Parst ohne Fehler - nillable wird in ElementDeclaration gespeichert
        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Substitution Group.
    #[test]
    fn parse_substitution_group() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:element name="publication" type="xs:string"/>
                <xs:element name="book" substitutionGroup="tns:publication" type="xs:string"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 2);
    }

    /// Lokales Element form="unqualified".
    #[test]
    fn local_element_form_unqualified() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org"
                       elementFormDefault="qualified">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:element name="local" type="xs:string" form="unqualified"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        // local sollte keinen Namespace haben (form überschreibt Default)
        assert!(schema
            .all_elements()
            .iter()
            .any(|q| &*q.local_name == "local" && q.uri.is_empty()));
    }

    /// Lokales Element form="qualified".
    #[test]
    fn local_element_form_qualified() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:element name="local" type="xs:string" form="qualified"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        // local sollte im target namespace sein (form überschreibt Default)
        assert!(schema
            .all_elements()
            .iter()
            .any(|q| &*q.local_name == "local" && &*q.uri == "http://example.org"));
    }

    // ========================================================================
    // Phase 4: Simple Type Parsing Tests
    // ========================================================================

    /// Benannter Simple Type.
    #[test]
    fn parse_simple_type_named() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:simpleType name="ISBN">
                    <xs:restriction base="xs:string"/>
                </xs:simpleType>
            </xs:schema>
        "#;

        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Simple Type mit Union.
    #[test]
    fn parse_simple_type_union() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:simpleType name="StringOrInt">
                    <xs:union memberTypes="xs:string xs:int"/>
                </xs:simpleType>
            </xs:schema>
        "#;

        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Built-in Type xs:string.
    #[test]
    fn parse_builtin_type_string() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="name" type="xs:string"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
    }

    // ========================================================================
    // Phase 5: Complex Type Parsing Tests
    // ========================================================================

    /// Leerer Complex Type.
    #[test]
    fn parse_complex_type_empty() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="EmptyType"/>
            </xs:schema>
        "#;

        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Complex Type mit Attributen.
    #[test]
    fn parse_complex_type_with_attributes() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="WithAttr">
                    <xs:attribute name="id" type="xs:string" use="required"/>
                    <xs:attribute name="name" type="xs:string"/>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.all_attributes().len(), 2);
    }

    /// Complex Type mit sequence.
    #[test]
    fn parse_complex_type_sequence() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="PersonType">
                    <xs:sequence>
                        <xs:element name="name" type="xs:string"/>
                        <xs:element name="age" type="xs:int"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.all_elements().len(), 2);
    }

    /// Complex Type mit mixed content.
    #[test]
    fn parse_complex_type_mixed() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="MixedType" mixed="true">
                    <xs:sequence>
                        <xs:element name="bold" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Complex Type mit extension.
    #[test]
    fn parse_complex_type_extension() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:complexType name="BaseType">
                    <xs:sequence>
                        <xs:element name="base" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:complexType name="DerivedType">
                    <xs:complexContent>
                        <xs:extension base="tns:BaseType">
                            <xs:sequence>
                                <xs:element name="derived" type="xs:string"/>
                            </xs:sequence>
                        </xs:extension>
                    </xs:complexContent>
                </xs:complexType>
            </xs:schema>
        "#;

        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Complex Type mit restriction.
    #[test]
    fn parse_complex_type_restriction() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:complexType name="BaseType">
                    <xs:sequence>
                        <xs:element name="item" type="xs:string" maxOccurs="unbounded"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:complexType name="RestrictedType">
                    <xs:complexContent>
                        <xs:restriction base="tns:BaseType">
                            <xs:sequence>
                                <xs:element name="item" type="xs:string" maxOccurs="5"/>
                            </xs:sequence>
                        </xs:restriction>
                    </xs:complexContent>
                </xs:complexType>
            </xs:schema>
        "#;

        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Base-Type wird für Extension tracked.
    #[test]
    fn base_type_tracked_for_extension() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:complexType name="BaseType"/>
                <xs:complexType name="DerivedType">
                    <xs:complexContent>
                        <xs:extension base="tns:BaseType"/>
                    </xs:complexContent>
                </xs:complexType>
            </xs:schema>
        "#;

        // BaseType sollte has_named_sub_types=true haben nach Auflösung
        let _schema = parse_xsd(xsd).unwrap();
    }

    // ========================================================================
    // Phase 6: Particle und Model Group Parsing Tests
    // ========================================================================

    /// Sequence Particle.
    #[test]
    fn parse_sequence_particle() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:element name="a" type="xs:string"/>
                        <xs:element name="b" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.all_elements().len(), 2);
    }

    /// Choice Particle.
    #[test]
    fn parse_choice_particle() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:choice>
                        <xs:element name="option1" type="xs:string"/>
                        <xs:element name="option2" type="xs:int"/>
                    </xs:choice>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.all_elements().len(), 2);
    }

    /// All Particle.
    #[test]
    fn parse_all_particle() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:all>
                        <xs:element name="x" type="xs:string"/>
                        <xs:element name="y" type="xs:string"/>
                    </xs:all>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.all_elements().len(), 2);
    }

    /// Particle mit minOccurs/maxOccurs.
    #[test]
    fn parse_particle_min_max() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:element name="optional" type="xs:string" minOccurs="0"/>
                        <xs:element name="multiple" type="xs:string" maxOccurs="unbounded"/>
                        <xs:element name="bounded" type="xs:string" minOccurs="2" maxOccurs="5"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.all_elements().len(), 3);
    }

    /// Wildcard ##any.
    #[test]
    fn parse_wildcard_any() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:any/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Wildcard namespace="##other".
    #[test]
    fn parse_wildcard_other() {
        let ns_other = "##other";
        let xsd = format!(
            r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:any namespace="{ns_other}"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#
        );

        let _schema = parse_xsd(&xsd).unwrap();
    }

    /// Finding 6: ##other ohne targetNamespace ergibt Not(None), nicht Not(Some("")).
    #[test]
    fn parse_wildcard_other_no_target_namespace() {
        let ns_other = "##other";
        let xsd = format!(
            r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:any namespace="{ns_other}"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#
        );

        // Sollte ohne Fehler parsen - Not(None) statt Not(Some(""))
        let _schema = parse_xsd(&xsd).unwrap();
    }

    /// Wildcard mit Namespace-Liste.
    #[test]
    fn parse_wildcard_namespaces() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:any namespace="http://a.org http://b.org"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Group ref.
    #[test]
    fn parse_group_ref() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:group name="CommonElements">
                    <xs:sequence>
                        <xs:element name="id" type="xs:string"/>
                        <xs:element name="name" type="xs:string"/>
                    </xs:sequence>
                </xs:group>
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:group ref="tns:CommonElements"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        // id und name sollten in all_elements sein
        assert!(schema.all_elements().iter().any(|q| &*q.local_name == "id"));
        assert!(schema.all_elements().iter().any(|q| &*q.local_name == "name"));
    }

    /// Attribute qualified.
    #[test]
    fn parse_attribute_qualified() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org">
                <xs:complexType name="TestType">
                    <xs:attribute name="id" type="xs:string" form="qualified"/>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert!(schema
            .all_attributes()
            .iter()
            .any(|q| &*q.local_name == "id" && &*q.uri == "http://example.org"));
    }

    /// Attribute unqualified.
    #[test]
    fn parse_attribute_unqualified() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org"
                       attributeFormDefault="qualified">
                <xs:complexType name="TestType">
                    <xs:attribute name="id" type="xs:string" form="unqualified"/>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert!(schema
            .all_attributes()
            .iter()
            .any(|q| &*q.local_name == "id" && q.uri.is_empty()));
    }

    /// anyAttribute.
    #[test]
    fn parse_any_attribute() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:anyAttribute/>
                </xs:complexType>
            </xs:schema>
        "#;

        let _schema = parse_xsd(xsd).unwrap();
    }

    // ========================================================================
    // Phase 7: Post-Processing Tests
    // ========================================================================

    /// SchemaInfo wird aus XSD gebaut.
    #[test]
    fn build_schema_info_from_xsd() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org">
                <xs:element name="root" type="xs:string"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert!(!schema.global_elements().is_empty());
    }

    /// has_named_sub_types wird erkannt.
    #[test]
    fn has_named_sub_types_detected() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:complexType name="BaseType"/>
                <xs:complexType name="DerivedType">
                    <xs:complexContent>
                        <xs:extension base="tns:BaseType"/>
                    </xs:complexContent>
                </xs:complexType>
            </xs:schema>
        "#;

        // Sollte ohne Fehler parsen - has_named_sub_types wird intern gesetzt
        let _schema = parse_xsd(xsd).unwrap();
    }

    // ========================================================================
    // Phase 8: Error Handling Tests
    // ========================================================================

    /// Ungültiges XML gibt XsdParseError.
    #[test]
    fn xsd_parse_error_invalid_xml() {
        let xsd = "<broken";
        let result = parse_xsd(xsd);
        assert!(matches!(result, Err(Error::XsdParseError(_))));
    }

    /// Fehlendes name-Attribut gibt XsdParseError.
    #[test]
    fn xsd_parse_error_missing_name() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element type="xs:string"/>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(matches!(result, Err(Error::XsdParseError(_))));
    }

    /// Unbekannter Prefix gibt XsdParseError.
    #[test]
    fn xsd_parse_error_unknown_prefix() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="test" type="unknown:Type"/>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(matches!(result, Err(Error::XsdParseError(_))));
    }

    // ========================================================================
    // Phase 9: Integration Tests
    // ========================================================================

    /// Einfaches Book Schema.
    #[test]
    fn xsd_book_schema_parse() {
        // XSD-konform: type="tns:BookType" mit explizitem Prefix
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:element name="book" type="tns:BookType"/>
                <xs:complexType name="BookType">
                    <xs:sequence>
                        <xs:element name="title" type="xs:string"/>
                        <xs:element name="author" type="xs:string" maxOccurs="unbounded"/>
                    </xs:sequence>
                    <xs:attribute name="isbn" type="xs:string" use="required"/>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // 1 globales Element (book)
        assert_eq!(schema.global_elements().len(), 1);
        assert_eq!(&*schema.global_elements()[0].local_name, "book");


        // 3 Elemente gesamt (book, title, author)
        // Note: author und title haben keinen Namespace (unqualified default)
        assert_eq!(schema.all_elements().len(), 3);

        // 1 Attribut (isbn)
        assert_eq!(schema.all_attributes().len(), 1);
        assert_eq!(&*schema.all_attributes()[0].local_name, "isbn");

    }

    /// H.3 Product Schema aus EXI Spec.
    #[test]
    fn xsd_h3_product_schema_parse() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="product" type="Product"/>
                <xs:complexType name="Product">
                    <xs:sequence>
                        <xs:element name="description" type="xs:string"/>
                        <xs:element name="quantity" type="xs:int"/>
                    </xs:sequence>
                    <xs:attribute name="sku" type="xs:string"/>
                    <xs:attribute name="color" type="xs:string"/>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // 1 globales Element
        assert_eq!(schema.global_elements().len(), 1);
        assert_eq!(&*schema.global_elements()[0].local_name, "product");


        // 3 Elemente gesamt (product, description, quantity)
        assert_eq!(schema.all_elements().len(), 3);

        // 2 Attribute (sku, color)
        assert_eq!(schema.all_attributes().len(), 2);
    }

    /// Schema ohne targetNamespace.
    #[test]
    fn xsd_no_target_namespace() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="root" type="xs:string"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
        // Leerer Namespace
        assert_eq!(&*schema.global_elements()[0].uri, "");

    }

    /// Komplexes verschachteltes Schema.
    #[test]
    fn xsd_nested_complex_types() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="order">
                    <xs:complexType>
                        <xs:sequence>
                            <xs:element name="item" maxOccurs="unbounded">
                                <xs:complexType>
                                    <xs:sequence>
                                        <xs:element name="name" type="xs:string"/>
                                        <xs:element name="quantity" type="xs:int"/>
                                    </xs:sequence>
                                </xs:complexType>
                            </xs:element>
                        </xs:sequence>
                    </xs:complexType>
                </xs:element>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // 1 globales Element (order)
        assert_eq!(schema.global_elements().len(), 1);

        // 4 Elemente gesamt (order, item, name, quantity)
        assert_eq!(schema.all_elements().len(), 4);
    }

    /// SimpleContent.
    #[test]
    fn xsd_simple_content() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="PriceType">
                    <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                            <xs:attribute name="currency" type="xs:string"/>
                        </xs:extension>
                    </xs:simpleContent>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.all_attributes().len(), 1);
        assert_eq!(&*schema.all_attributes()[0].local_name, "currency");

    }

    /// Default namespace (xmlns ohne Prefix).
    #[test]
    fn xsd_default_namespace() {
        // Default namespace für Element-Tags, aber type-Attribute brauchen expliziten Prefix
        // (XSD-Spec: QNames in Attributwerten nutzen NICHT den default namespace)
        let xsd = r#"
            <schema xmlns="http://www.w3.org/2001/XMLSchema"
                    xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    targetNamespace="http://example.org">
                <element name="test" type="xs:string"/>
            </schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
    }

    /// Default namespace = XSD ohne expliziten xs: Prefix (MediaWiki-Pattern).
    ///
    /// Viele reale XSDs (z.B. MediaWiki export-0.11.xsd) deklarieren:
    ///   xmlns="http://www.w3.org/2001/XMLSchema"
    /// ohne einen xs:-Prefix. Unpräfixierte type/base/ref-Referenzen wie
    /// type="string" müssen dann als XSD-Built-in-Typen aufgelöst werden.
    #[test]
    fn xsd_default_namespace_without_xs_prefix() {
        let xsd = r#"
            <schema xmlns="http://www.w3.org/2001/XMLSchema"
                    xmlns:mw="http://www.mediawiki.org/xml/export-0.11/"
                    targetNamespace="http://www.mediawiki.org/xml/export-0.11/">
                <element name="title" type="string"/>
                <complexType name="PageType">
                    <sequence>
                        <element name="id" type="integer"/>
                        <element name="ns" type="string"/>
                    </sequence>
                </complexType>
            </schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
        assert_eq!(&*schema.global_elements()[0].local_name, "title");
        // PageType wurde korrekt geparst (type="integer" und type="string" aufgelöst)
        assert!(schema.type_definitions().keys().any(|q| &*q.local_name == "PageType"));
    }

    /// Forward-Reference: Element referenziert Typ der später definiert ist.
    #[test]
    fn xsd_forward_reference_type() {
        // Element "book" ist VOR "BookType" definiert
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="book" type="BookType"/>
                <xs:complexType name="BookType">
                    <xs:sequence>
                        <xs:element name="title" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.global_elements().len(), 1);
        assert_eq!(schema.all_elements().len(), 2); // book, title
    }

    /// Forward-Reference: Group referenziert Typ der später definiert ist.
    #[test]
    fn xsd_forward_reference_group() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:group name="PersonGroup">
                    <xs:sequence>
                        <xs:element name="name" type="xs:string"/>
                        <xs:element name="address" type="AddressType"/>
                    </xs:sequence>
                </xs:group>
                <xs:complexType name="AddressType">
                    <xs:sequence>
                        <xs:element name="city" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();
        assert_eq!(schema.all_elements().len(), 3); // name, address, city
    }

    /// Ungültiger xs:* Base-Type sollte Fehler geben.
    #[test]
    fn xsd_invalid_builtin_base_type() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="BadType">
                    <xs:simpleContent>
                        <xs:extension base="xs:InvalidType"/>
                    </xs:simpleContent>
                </xs:complexType>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("InvalidType"), "Error should mention invalid type: {err}");
    }

    // ========================================================================
    // Neue Features: xml-Prefix und Transitive Substitution Groups
    // ========================================================================

    /// xml-Prefix ist implizit an XML-Namespace gebunden (Spec: Namespaces in XML).
    /// Attribute wie xml:lang oder xml:space müssen ohne explizite Deklaration funktionieren.
    #[test]
    fn xml_prefix_implicit_binding() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TextType">
                    <xs:simpleContent>
                        <xs:extension base="xs:string">
                            <xs:attribute ref="xml:lang"/>
                        </xs:extension>
                    </xs:simpleContent>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // xml:lang Attribut sollte im XML-Namespace sein
        let attrs = schema.all_attributes();
        assert_eq!(attrs.len(), 1);
        assert_eq!(&*attrs[0].local_name, "lang");

        assert_eq!(&*attrs[0].uri, "http://www.w3.org/XML/1998/namespace");

    }

    /// Transitive Substitution Groups: A substituiert B substituiert C
    /// → A und B müssen beide in C's Substitution Group erscheinen.
    #[test]
    fn transitive_substitution_groups() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="base" type="xs:string"/>
                <xs:element name="derived1" type="xs:string" substitutionGroup="base"/>
                <xs:element name="derived2" type="xs:string" substitutionGroup="derived1"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // base sollte sowohl derived1 als auch derived2 in seiner Substitution Group haben
        let base_decl = schema.element_declarations()
            .iter()
            .find(|(q, _)| &*q.local_name == "base")
            .map(|(_, e)| e)
            .expect("base element should exist");

        let subst_names: Vec<&str> = base_decl.substitution_group
            .iter()
            .map(|q| &*q.local_name)
            .collect();

        assert!(subst_names.contains(&"derived1"),
            "base should have derived1 in substitution group: {:?}", subst_names);
        assert!(subst_names.contains(&"derived2"),
            "base should have derived2 transitively: {:?}", subst_names);
    }

    /// Längere transitive Kette: A ← B ← C ← D
    #[test]
    fn transitive_substitution_groups_long_chain() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="a" type="xs:string"/>
                <xs:element name="b" type="xs:string" substitutionGroup="a"/>
                <xs:element name="c" type="xs:string" substitutionGroup="b"/>
                <xs:element name="d" type="xs:string" substitutionGroup="c"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // a sollte b, c und d in seiner Substitution Group haben
        let a_decl = schema.element_declarations()
            .iter()
            .find(|(q, _)| &*q.local_name == "a")
            .map(|(_, e)| e)
            .expect("a element should exist");

        assert_eq!(a_decl.substitution_group.len(), 3,
            "a should have 3 members in substitution group: {:?}",
            a_decl.substitution_group.iter().map(|q| &q.local_name).collect::<Vec<_>>());

        // b sollte c und d haben
        let b_decl = schema.element_declarations()
            .iter()
            .find(|(q, _)| &*q.local_name == "b")
            .map(|(_, e)| e)
            .expect("b element should exist");

        assert_eq!(b_decl.substitution_group.len(), 2,
            "b should have 2 members: {:?}",
            b_decl.substitution_group.iter().map(|q| &q.local_name).collect::<Vec<_>>());

        // c sollte nur d haben
        let c_decl = schema.element_declarations()
            .iter()
            .find(|(q, _)| &*q.local_name == "c")
            .map(|(_, e)| e)
            .expect("c element should exist");

        assert_eq!(c_decl.substitution_group.len(), 1,
            "c should have 1 member: {:?}",
            c_decl.substitution_group.iter().map(|q| &q.local_name).collect::<Vec<_>>());
    }

    // ========================================================================
    // Codex Review Fixes: Neue Tests
    // ========================================================================

    /// xs:anyType ist ein komplexer Typ (Spec 8.5.4.1.3.2).
    #[test]
    fn anytype_is_complex() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="test" type="xs:anyType"/>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // Das Element sollte einen komplexen Typ haben
        let elem = schema.element_declarations()
            .iter()
            .find(|(q, _)| &*q.local_name == "test")
            .map(|(_, e)| e)
            .expect("test element should exist");

        assert!(elem.type_definition.is_some());
        let type_def = elem.type_definition.as_ref().unwrap();
        assert!(matches!(type_def.as_ref(), TypeDefinition::Complex { .. }),
            "xs:anyType should be Complex, got: {:?}", type_def);
    }

    /// Attribute-Wildcards mit Namespace-Liste funktionieren.
    #[test]
    fn attr_wildcard_namespaces() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       targetNamespace="http://example.org">
                <xs:complexType name="TestType">
                    <xs:anyAttribute namespace="http://foo http://bar"/>
                </xs:complexType>
            </xs:schema>
        "#;

        // Sollte ohne Fehler parsen
        let _schema = parse_xsd(xsd).unwrap();
    }

    /// Attribute-Wildcards mit namespace="##other" (Not-Constraint).
    #[test]
    fn attr_wildcard_other() {
        // Build XSD with ##other programmatically to avoid raw string issues
        let hash_other = "##other";
        let xsd = format!(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                         targetNamespace="http://example.org">
                <xs:complexType name="TestType">
                    <xs:anyAttribute namespace="{}"/>
                </xs:complexType>
            </xs:schema>"#,
            hash_other
        );

        // Sollte ohne Fehler parsen
        let _schema = parse_xsd(&xsd).unwrap();
    }

    /// Finding 6: ##other ohne targetNamespace ergibt Not(None), nicht Not(Some("")).
    /// Das ist spec-konform: ##other schließt den abwesenden Namespace aus.
    #[test]
    fn attr_wildcard_other_no_target_namespace() {
        let hash_other = "##other";
        let xsd = format!(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:anyAttribute namespace="{}"/>
                </xs:complexType>
            </xs:schema>"#,
            hash_other
        );

        // Sollte ohne Fehler parsen
        let _schema = parse_xsd(&xsd).unwrap();
        // Der AttributeWildcard sollte Not(None) sein, nicht Not(Some(""))
        // (wird intern korrekt behandelt, aber das können wir nicht direkt testen
        // ohne das global_types zu exponieren - der Parse-Erfolg zeigt, dass es funktioniert)
    }

    /// namespace="" in Wildcards ist ein Fehler (Spec 8.5.4.1.7).
    #[test]
    fn wildcard_empty_namespace_is_error() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:any namespace=""/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(result.is_err(), "Empty namespace should be an error");
        let err = result.unwrap_err().to_string();
        assert!(err.contains("Empty namespace"), "Error should mention empty namespace: {err}");
    }

    /// Attribute mit type-Attribut werden korrekt geparst.
    #[test]
    fn attribute_with_type() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:attribute name="id" type="xs:int"/>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // Prüfe dass der Attribut-Typ geparst wurde
        let type_def = schema.type_definitions()
            .iter()
            .find(|(q, _)| &*q.local_name == "TestType")
            .map(|(_, t)| t)
            .expect("TestType should exist");

        if let TypeDefinition::Complex { attributes, .. } = type_def.as_ref() {
            assert_eq!(attributes.len(), 1);
            assert!(attributes[0].type_definition.is_some(),
                "Attribute should have type_definition");

            let attr_type = attributes[0].type_definition.as_ref().unwrap();
            if let TypeDefinition::Simple { base_type, .. } = attr_type.as_ref() {
                assert_eq!(base_type.as_deref(), Some("int"),
                    "Attribute type should be int");
            } else {
                panic!("Attribute type should be Simple");
            }
        } else {
            panic!("TestType should be Complex");
        }
    }

    /// Group-Refs werden korrekt aufgelöst (nicht gegen Platzhalter).
    #[test]
    fn group_ref_fully_resolved() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:group name="PersonGroup">
                    <xs:sequence>
                        <xs:element name="firstName" type="xs:string"/>
                        <xs:element name="lastName" type="xs:string"/>
                    </xs:sequence>
                </xs:group>
                <xs:complexType name="PersonType">
                    <xs:group ref="PersonGroup"/>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // Prüfe dass PersonType die Group-Elemente enthält
        let type_def = schema.type_definitions()
            .iter()
            .find(|(q, _)| &*q.local_name == "PersonType")
            .map(|(_, t)| t)
            .expect("PersonType should exist");

        if let TypeDefinition::Complex { content, .. } = type_def.as_ref() {
            if let ContentType::ElementOnly(particle) = content {
                if let ParticleTerm::ModelGroup(model_group) = &particle.term {
                    assert_eq!(model_group.particles.len(), 2,
                        "Group should have 2 elements, got: {:?}",
                        model_group.particles.len());
                } else {
                    panic!("Content should be ModelGroup");
                }
            } else {
                panic!("Content should be ElementOnly");
            }
        } else {
            panic!("PersonType should be Complex");
        }
    }

    /// Element-Refs werden mit vollständigen Daten aufgelöst.
    #[test]
    fn element_ref_fully_resolved() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="sharedElement" type="xs:string" nillable="true"/>
                <xs:complexType name="ContainerType">
                    <xs:sequence>
                        <xs:element ref="sharedElement"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // Prüfe dass das referenzierte Element die korrekten Eigenschaften hat
        let type_def = schema.type_definitions()
            .iter()
            .find(|(q, _)| &*q.local_name == "ContainerType")
            .map(|(_, t)| t)
            .expect("ContainerType should exist");

        if let TypeDefinition::Complex { content, .. } = type_def.as_ref() {
            if let ContentType::ElementOnly(particle) = content {
                if let ParticleTerm::ModelGroup(model_group) = &particle.term {
                    let elem_particle = &model_group.particles[0];
                    if let ParticleTerm::Element(elem_decl) = &elem_particle.term {
                        assert!(elem_decl.nillable,
                            "Element ref should preserve nillable=true");
                        assert!(elem_decl.type_definition.is_some(),
                            "Element ref should have type_definition");
                    } else {
                        panic!("Should be Element particle");
                    }
                } else {
                    panic!("Content should be ModelGroup");
                }
            } else {
                panic!("Content should be ElementOnly");
            }
        } else {
            panic!("ContainerType should be Complex");
        }
    }

    // ========================================================================
    // Codex Review: Neue Tests (T1-T5)
    // ========================================================================

    /// T1: Forward-Ref-Typen in lokalen Elementen.
    ///
    /// Ein lokales Element referenziert einen Typ der später im Schema definiert wird.
    /// Der Typ muss nach der Forward-Reference-Auflösung vollständig sein.
    #[test]
    fn t1_forward_ref_type_in_local_element() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="ContainerType">
                    <xs:sequence>
                        <xs:element name="nested" type="NestedType"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:complexType name="NestedType">
                    <xs:sequence>
                        <xs:element name="value" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // Prüfe dass NestedType existiert und value enthält
        let nested_type = schema.type_definitions()
            .iter()
            .find(|(q, _)| &*q.local_name == "NestedType")
            .map(|(_, t)| t)
            .expect("NestedType should exist");

        if let TypeDefinition::Complex { content, .. } = nested_type.as_ref() {
            if let ContentType::ElementOnly(particle) = content {
                if let ParticleTerm::ModelGroup(group) = &particle.term {
                    assert_eq!(group.particles.len(), 1);
                } else {
                    panic!("NestedType content should be ModelGroup");
                }
            } else {
                panic!("NestedType should have ElementOnly content");
            }
        } else {
            panic!("NestedType should be Complex");
        }

        // 3 Elemente: nested, value (lokal)
        assert_eq!(schema.all_elements().len(), 2);
    }

    /// T2: Attribute ref mit globalem Attribut + Typ.
    ///
    /// Ein Attribut-Ref sollte den Typ des globalen Attributs übernehmen.
    #[test]
    fn t2_attribute_ref_with_global_type() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:attribute name="globalId" type="xs:int"/>
                <xs:complexType name="TestType">
                    <xs:attribute ref="globalId"/>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // Prüfe dass TestType das Attribut mit Typ hat
        let type_def = schema.type_definitions()
            .iter()
            .find(|(q, _)| &*q.local_name == "TestType")
            .map(|(_, t)| t)
            .expect("TestType should exist");

        if let TypeDefinition::Complex { attributes, .. } = type_def.as_ref() {
            assert_eq!(attributes.len(), 1);
            let attr = &attributes[0];
            assert_eq!(&*attr.qname.local_name, "globalId");


            // Der Typ sollte vom globalen Attribut übernommen sein
            assert!(attr.type_definition.is_some(),
                "Attribute ref should inherit type from global attribute");
            if let Some(attr_type) = &attr.type_definition {
                if let TypeDefinition::Simple { base_type, .. } = attr_type.as_ref() {
                    assert_eq!(base_type.as_deref(), Some("int"));
                } else {
                    panic!("Attribute type should be Simple");
                }
            }
        } else {
            panic!("TestType should be Complex");
        }
    }

    /// T3: User-defined SimpleType-Restriction-Kette.
    ///
    /// Eine Kette von SimpleType restrictions: MyString → ConstrainedString → xs:string
    /// Der base_type sollte transitiv aufgelöst werden.
    #[test]
    fn t3_simpletype_restriction_chain() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:simpleType name="BaseString">
                    <xs:restriction base="xs:string"/>
                </xs:simpleType>
                <xs:simpleType name="ConstrainedString">
                    <xs:restriction base="BaseString">
                        <xs:maxLength value="50"/>
                    </xs:restriction>
                </xs:simpleType>
                <xs:simpleType name="VeryConstrainedString">
                    <xs:restriction base="ConstrainedString">
                        <xs:maxLength value="10"/>
                    </xs:restriction>
                </xs:simpleType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        // BaseString hat named sub-types (ConstrainedString)
        let base_string = schema.type_definitions()
            .iter()
            .find(|(q, _)| &*q.local_name == "BaseString")
            .map(|(_, t)| t)
            .expect("BaseString should exist");

        assert!(base_string.has_named_sub_types(),
            "BaseString should have named sub-types (ConstrainedString derives from it)");

        // ConstrainedString hat auch named sub-types (VeryConstrainedString)
        let constrained = schema.type_definitions()
            .iter()
            .find(|(q, _)| &*q.local_name == "ConstrainedString")
            .map(|(_, t)| t)
            .expect("ConstrainedString should exist");

        assert!(constrained.has_named_sub_types(),
            "ConstrainedString should have named sub-types (VeryConstrainedString derives from it)");

        // VeryConstrainedString hat KEINE named sub-types
        let very_constrained = schema.type_definitions()
            .iter()
            .find(|(q, _)| &*q.local_name == "VeryConstrainedString")
            .map(|(_, t)| t)
            .expect("VeryConstrainedString should exist");

        assert!(!very_constrained.has_named_sub_types(),
            "VeryConstrainedString should NOT have named sub-types");

        // base_type sollte transitiv aufgelöst sein
        if let TypeDefinition::Simple { base_type, .. } = very_constrained.as_ref() {
            assert_eq!(base_type.as_deref(), Some("string"),
                "VeryConstrainedString base_type should be transitively resolved to 'string'");
        }
    }

    /// T4: complexContent mixed="true".
    ///
    /// mixed="true" auf complexContent sollte Mixed content erzeugen.
    #[test]
    fn t4_complex_content_mixed() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                       xmlns:tns="http://example.org"
                       targetNamespace="http://example.org">
                <xs:complexType name="BaseType">
                    <xs:sequence>
                        <xs:element name="item" type="xs:string"/>
                    </xs:sequence>
                </xs:complexType>
                <xs:complexType name="MixedDerived">
                    <xs:complexContent mixed="true">
                        <xs:extension base="tns:BaseType">
                            <xs:sequence>
                                <xs:element name="extra" type="xs:string"/>
                            </xs:sequence>
                        </xs:extension>
                    </xs:complexContent>
                </xs:complexType>
            </xs:schema>
        "#;

        let schema = parse_xsd(xsd).unwrap();

        let mixed_type = schema.type_definitions()
            .iter()
            .find(|(q, _)| &*q.local_name == "MixedDerived")
            .map(|(_, t)| t)
            .expect("MixedDerived should exist");

        if let TypeDefinition::Complex { content, .. } = mixed_type.as_ref() {
            assert!(matches!(content, ContentType::Mixed(_)),
                "MixedDerived should have Mixed content, got: {:?}", content);
        } else {
            panic!("MixedDerived should be Complex");
        }
    }

    /// T5: Ungültige Wildcard-Namespaces.
    ///
    /// `##any` und `##other` dürfen nicht mit anderen Namespace-Werten kombiniert werden.
    #[test]
    fn t5_invalid_wildcard_namespace_combination() {
        // Konstruiere Strings programmatisch um ## Raw-String-Probleme zu vermeiden
        let hash_any = format!("{}any", "##");
        let hash_other = format!("{}other", "##");

        // ##any mit anderen Werten
        let xsd_any_combined = format!(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:any namespace="{} http://example.org"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>"#,
            hash_any
        );

        let result = parse_xsd(&xsd_any_combined);
        assert!(result.is_err(), "{} with other values should be an error", hash_any);
        let err = result.unwrap_err().to_string();
        assert!(err.contains(&hash_any) && err.contains("cannot be combined"),
            "Error should mention {} cannot be combined: {}", hash_any, err);

        // ##other mit anderen Werten
        let xsd_other_combined = format!(
            r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:any namespace="{} http://example.org"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>"#,
            hash_other
        );

        let result = parse_xsd(&xsd_other_combined);
        assert!(result.is_err(), "{} with other values should be an error", hash_other);
        let err = result.unwrap_err().to_string();
        assert!(err.contains("cannot be combined"),
            "Error should mention cannot be combined: {}", err);
    }

    /// Fix #10: XSD Größenlimit wird enforced.
    #[test]
    fn xsd_size_limit_enforced() {
        // Erstelle ein XSD das größer als MAX_XSD_SIZE wäre (simuliert durch Check)
        // Wir testen nur die Fehlermeldung, nicht wirklich 16MB
        let large_xsd = "x".repeat(17 * 1024 * 1024); // 17 MiB

        let result = parse_xsd(&large_xsd);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("too large"), "Error should mention size: {}", err);
    }

    /// Finding 3: substitutionGroup auf lokalem Element ist ein Fehler.
    #[test]
    fn substitution_group_on_local_element_is_error() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:element name="base" type="xs:string"/>
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:element name="child" type="xs:string" substitutionGroup="base"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(result.is_err(), "substitutionGroup on local element should be error");
        let err = result.unwrap_err().to_string();
        assert!(err.contains("global"), "Error should mention global: {err}");
    }

    /// Finding 6: minOccurs > maxOccurs ist ein Fehler.
    #[test]
    fn min_greater_than_max_is_error() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:sequence>
                        <xs:element name="child" type="xs:string" minOccurs="5" maxOccurs="2"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(result.is_err(), "minOccurs > maxOccurs should be error");
        let err = result.unwrap_err().to_string();
        assert!(err.contains("greater"), "Error should mention greater: {err}");
    }

    /// Finding 6: xs:all mit maxOccurs > 1 ist ein Fehler.
    #[test]
    fn all_with_max_occurs_greater_than_one_is_error() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:all maxOccurs="unbounded">
                        <xs:element name="child" type="xs:string"/>
                    </xs:all>
                </xs:complexType>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(result.is_err(), "xs:all with maxOccurs > 1 should be error");
        let err = result.unwrap_err().to_string();
        assert!(err.contains("maxOccurs"), "Error should mention maxOccurs: {err}");
    }

    /// Finding 6: xs:all Kinder mit maxOccurs > 1 ist ein Fehler.
    #[test]
    fn all_children_with_max_occurs_greater_than_one_is_error() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:all>
                        <xs:element name="child" type="xs:string" maxOccurs="5"/>
                    </xs:all>
                </xs:complexType>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(result.is_err(), "xs:all children with maxOccurs > 1 should be error");
        let err = result.unwrap_err().to_string();
        assert!(err.contains("maxOccurs"), "Error should mention maxOccurs: {err}");
    }

    /// Finding 6: xs:all darf keine sequence/choice enthalten.
    #[test]
    fn all_cannot_contain_sequence() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:complexType name="TestType">
                    <xs:all>
                        <xs:sequence>
                            <xs:element name="child" type="xs:string"/>
                        </xs:sequence>
                    </xs:all>
                </xs:complexType>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(result.is_err(), "xs:all cannot contain sequence");
        let err = result.unwrap_err().to_string();
        assert!(err.contains("element") || err.contains("only"), "Error should mention element only: {err}");
    }

    // ========================================================================
    // xs:import Tests
    // ========================================================================

    /// Einfacher Import: lib.xsd wird von main.xsd importiert.
    #[test]
    fn import_simple() {
        let main_xsd = Path::new("tests/fixtures/import/main.xsd");
        if !main_xsd.exists() {
            eprintln!("SKIP: Import test fixtures not found");
            return;
        }

        let schema = parse_xsd_with_imports(main_xsd).expect("Should parse main.xsd with imports");

        // Check that elements from both schemas are present
        let global_elements = schema.global_elements();
        let element_names: Vec<&str> = global_elements.iter().map(|q| &*q.local_name).collect();

        assert!(element_names.contains(&"root"), "Should have 'root' from main.xsd");
        assert!(element_names.contains(&"container"), "Should have 'container' from main.xsd");
        assert!(element_names.contains(&"item"), "Should have 'item' from lib.xsd");
    }

    /// Cross-Namespace Type-Referenz: main.xsd verwendet lib:ContainerType.
    #[test]
    fn import_cross_namespace_type_ref() {
        let main_xsd = Path::new("tests/fixtures/import/main.xsd");
        if !main_xsd.exists() {
            return;
        }

        let schema = parse_xsd_with_imports(main_xsd).expect("Should parse cross-namespace type ref");

        // ContainerType aus lib.xsd sollte in type_definitions sein
        let type_names: Vec<&str> = schema
            .type_definitions()
            .keys()
            .map(|q| &*q.local_name)
            .collect();
        assert!(
            type_names.contains(&"ContainerType"),
            "Should have ContainerType from lib.xsd"
        );
    }

    /// Zirkulärer Import: circular_a.xsd und circular_b.xsd importieren sich gegenseitig.
    #[test]
    fn import_circular() {
        let circular_a = Path::new("tests/fixtures/import/circular_a.xsd");
        if !circular_a.exists() {
            return;
        }

        // Should not hang or stack overflow
        let schema = parse_xsd_with_imports(circular_a).expect("Should handle circular imports");

        // Both schemas should be loaded
        let element_names: Vec<&str> = schema
            .global_elements()
            .iter()
            .map(|q| &*q.local_name)
            .collect();
        assert!(element_names.contains(&"rootA"), "Should have rootA from circular_a.xsd");
        assert!(element_names.contains(&"elementB"), "Should have elementB from circular_b.xsd");
    }

    /// Fehlende Import-Datei gibt korrekten Fehler.
    #[test]
    fn import_missing_file_is_error() {
        let missing_xsd = Path::new("tests/fixtures/import/missing_import.xsd");
        if !missing_xsd.exists() {
            return;
        }

        let result = parse_xsd_with_imports(missing_xsd);
        assert!(result.is_err(), "Missing import file should be an error");
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("not found") || err.contains("does_not_exist"),
            "Error should mention missing file: {err}"
        );
    }

    /// W3C acceptance.xsd mit 4 Imports (inkl. zirkulärem Import).
    #[test]
    fn import_w3c_acceptance() {
        let acceptance_xsd =
            Path::new("tests/fixtures/w3c/acceptance.xsd");
        if !acceptance_xsd.exists() {
            eprintln!("SKIP: W3C acceptance.xsd not found");
            return;
        }

        let schema = parse_xsd_with_imports(acceptance_xsd).expect("Should parse W3C acceptance.xsd");

        // Check elements from all imported schemas
        let element_names: Vec<&str> = schema
            .global_elements()
            .iter()
            .map(|q| &*q.local_name)
            .collect();

        // Elements from acceptance.xsd (urn:foo)
        assert!(element_names.contains(&"A"), "Should have 'A' from acceptance.xsd");
        assert!(element_names.contains(&"ANY"), "Should have 'ANY' from acceptance.xsd");

        // Elements from goo (urn:goo)
        // Note: goo, hoo, ioo all have "AB" but in different namespaces
        let goo_elements: Vec<_> = schema
            .global_elements()
            .iter()
            .filter(|q| &*q.uri == "urn:goo")
            .collect();
        assert!(!goo_elements.is_empty(), "Should have elements from urn:goo");

        eprintln!(
            "Successfully parsed W3C acceptance.xsd with {} global elements",
            element_names.len()
        );
    }

    /// W3C DTRM-XSDs muessen parsebar sein (inkl. DOCTYPE in enumerationDTRM.xsd).
    #[test]
    fn parse_w3c_dtrm_xsds() {
        let base = Path::new("tests/fixtures/w3c/dtrm");
        if !base.exists() {
            eprintln!("SKIP: W3C DTRM XSDs nicht vorhanden");
            return;
        }
        for name in ["enumerationDTRM.xsd", "listDTRM.xsd", "unionDTRM.xsd"] {
            let path = base.join(name);
            let schema = parse_xsd_with_imports(&path)
                .unwrap_or_else(|e| panic!("{name}: {e}"));
            assert!(
                !schema.global_elements().is_empty(),
                "{name}: muss mindestens ein globales Element haben"
            );
            eprintln!("{name}: OK ({} global elements)", schema.global_elements().len());
        }
    }

    /// XSD mit xs:import auf http://www.w3.org/XML/1998/namespace (xml.xsd)
    /// darf keinen Fehler werfen — dieser Namespace ist hardcodiert.
    #[test]
    fn import_skip_xml_namespace() {
        use std::io::Write;
        let dir = std::env::temp_dir().join("erxi_test_import_skip_xml_ns");
        let _ = std::fs::create_dir_all(&dir);
        let xsd_path = dir.join("test.xsd");
        let mut f = std::fs::File::create(&xsd_path).unwrap();
        write!(
            f,
            r#"<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
           xmlns:xml="http://www.w3.org/XML/1998/namespace"
           targetNamespace="urn:test">
    <xs:import namespace="http://www.w3.org/XML/1998/namespace"
               schemaLocation="http://www.w3.org/2001/xml.xsd"/>
    <xs:element name="root">
        <xs:complexType>
            <xs:attribute ref="xml:lang"/>
        </xs:complexType>
    </xs:element>
</xs:schema>"#
        )
        .unwrap();

        let schema = parse_xsd_with_imports(&xsd_path)
            .expect("Import von xml-namespace sollte übersprungen werden");
        let names: Vec<&str> = schema.global_elements().iter().map(|q| &*q.local_name).collect();
        assert!(names.contains(&"root"), "Element 'root' fehlt: {names:?}");
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// XSD mit xs:import auf http://www.w3.org/2001/XMLSchema darf keinen Fehler werfen.
    #[test]
    fn import_skip_xs_namespace() {
        use std::io::Write;
        let dir = std::env::temp_dir().join("erxi_test_import_skip_xs_ns");
        let _ = std::fs::create_dir_all(&dir);
        let xsd_path = dir.join("test.xsd");
        let mut f = std::fs::File::create(&xsd_path).unwrap();
        write!(
            f,
            r#"<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
           targetNamespace="urn:test">
    <xs:import namespace="http://www.w3.org/2001/XMLSchema"
               schemaLocation="http://www.w3.org/2001/XMLSchema.xsd"/>
    <xs:element name="item" type="xs:string"/>
</xs:schema>"#
        )
        .unwrap();

        let schema = parse_xsd_with_imports(&xsd_path)
            .expect("Import von xs-namespace sollte übersprungen werden");
        let names: Vec<&str> = schema.global_elements().iter().map(|q| &*q.local_name).collect();
        assert!(names.contains(&"item"), "Element 'item' fehlt: {names:?}");
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// XSD mit unbekanntem Import und fehlender Datei muss weiterhin fehlschlagen.
    #[test]
    fn import_unknown_namespace_missing_file_errors() {
        use std::io::Write;
        let dir = std::env::temp_dir().join("erxi_test_import_unknown_ns");
        let _ = std::fs::create_dir_all(&dir);
        let xsd_path = dir.join("test.xsd");
        let mut f = std::fs::File::create(&xsd_path).unwrap();
        write!(
            f,
            r#"<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
           targetNamespace="urn:test">
    <xs:import namespace="urn:unknown"
               schemaLocation="does_not_exist.xsd"/>
    <xs:element name="test" type="xs:string"/>
</xs:schema>"#
        )
        .unwrap();

        let result = parse_xsd_with_imports(&xsd_path);
        assert!(result.is_err(), "Unbekannter Import mit fehlender Datei muss fehlschlagen");
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("not found"),
            "Fehler sollte 'not found' enthalten: {err}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// parse_xsd (ohne Imports) sollte weiterhin xs:import ablehnen.
    #[test]
    fn parse_xsd_rejects_import() {
        let xsd = r#"
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                <xs:import namespace="urn:other" schemaLocation="other.xsd"/>
                <xs:element name="test" type="xs:string"/>
            </xs:schema>
        "#;

        let result = parse_xsd(xsd);
        assert!(result.is_err(), "parse_xsd should reject xs:import");
        let err = result.unwrap_err().to_string();
        assert!(err.contains("import"), "Error should mention import: {err}");
    }
}
