//! Multi-Schema Context für xs:import Support (Spec XSD 1.0).
//!
//! Verwaltet mehrere XSD-Schemas und ermöglicht Cross-Namespace
//! Referenz-Auflösung für Typen, Elemente und Groups.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use roxmltree::{Document, Node, ParsingOptions};

use crate::error::{Error, Result};
use crate::qname::QName;
use crate::schema::{
    ContentType, ElementDeclaration, ModelGroup, SchemaInfo, SimpleTypeVariety, TypeDefinition,
};
use crate::{FastHashMap, FastHashSet};

use super::{
    merge_complex_type_extensions_in_map, sync_particle_type_defs, AttributeDeclaration,
    ElementFragmentElementSignature, PendingRef, XsdParser, BUILTIN_TYPES_WITH_SUBTYPES,
    MAX_XSD_SIZE, XS_NS,
};

/// Parsed ein XSD-Dokument mit Import-Auflösung.
///
/// Relative Pfade in `schemaLocation` werden vom Verzeichnis des Haupt-Schemas
/// aufgelöst. Zirkuläre Imports werden erkannt und übersprungen.
///
/// # Beispiel
///
/// ```no_run
/// use std::path::Path;
/// use erxi::xsd::parse_xsd_with_imports;
///
/// let schema = parse_xsd_with_imports(Path::new("schema.xsd")).unwrap();
/// ```
pub fn parse_xsd_with_imports(xsd_path: &Path) -> Result<SchemaInfo> {
    let mut ctx = MultiSchemaContext::new();
    ctx.load_schema(xsd_path)?;
    ctx.resolve_all_references()?;
    Ok(ctx.build())
}

/// Import-Deklaration aus einem Schema.
#[derive(Debug, Clone)]
struct XsdImport {
    /// Namespace des importierten Schemas.
    namespace: Option<String>,
    /// Relativer Pfad zur Schema-Datei.
    schema_location: String,
}

/// Multi-Schema-Kontext für xs:import Support.
///
/// Verwaltet mehrere XsdParser-Instanzen und ermöglicht Cross-Namespace
/// Referenz-Auflösung für Typen, Elemente und Groups.
pub(super) struct MultiSchemaContext {
    /// Geladene Schemas: canonical_path → (target_ns, parsed).
    /// `parsed` ist true wenn das Schema vollständig geparst wurde.
    loaded: FastHashMap<PathBuf, (String, bool)>,
    /// Globale Elemente aus allen Schemas.
    pub(super) global_elements: BTreeMap<Rc<QName>, ElementDeclaration>,
    /// Globale Typen aus allen Schemas.
    pub(super) global_types: BTreeMap<Rc<QName>, TypeDefinition>,
    /// Globale Groups aus allen Schemas.
    pub(super) global_groups: BTreeMap<Rc<QName>, ModelGroup>,
    /// Globale Attribute aus allen Schemas.
    pub(super) global_attributes: BTreeMap<Rc<QName>, AttributeDeclaration>,
    /// Alle Elemente (global + lokal) aus allen Schemas.
    all_elements: BTreeSet<Rc<QName>>,
    /// Alle Attribute aus allen Schemas.
    all_attributes: BTreeSet<Rc<QName>>,
    /// Alle Element-Deklarationen (global + lokal) aus allen Schemas.
    all_element_declarations: BTreeMap<Rc<QName>, ElementDeclaration>,
    /// Pending References aus allen Schemas.
    pending_refs: Vec<PendingRef>,
    /// Base-Type-Tracking für has_named_sub_types (Complex Types).
    type_bases: BTreeMap<Rc<QName>, Rc<QName>>,
    /// Base-Type-Tracking für SimpleTypes.
    simple_type_bases: BTreeMap<Rc<QName>, Rc<QName>>,
    /// Signaturen für Element Fragment Grammar (8.5.3).
    element_fragment_element_signatures: BTreeMap<Rc<QName>, ElementFragmentElementSignature>,
    /// Elemente mit uneinheitlicher Typ/Nillable-Kombination.
    element_fragment_relaxed_elements: BTreeSet<Rc<QName>>,
}

/// Gespeicherter Schema-Content für 2-Phasen-Parsing.
struct LoadedSchema {
    content: String,
    target_ns: String,
}

impl MultiSchemaContext {
    /// Erstellt einen neuen Context.
    fn new() -> Self {
        Self {
            loaded: FastHashMap::default(),
            global_elements: BTreeMap::new(),
            global_types: BTreeMap::new(),
            global_groups: BTreeMap::new(),
            global_attributes: BTreeMap::new(),
            all_elements: BTreeSet::new(),
            all_attributes: BTreeSet::new(),
            all_element_declarations: BTreeMap::new(),
            pending_refs: Vec::new(),
            type_bases: BTreeMap::new(),
            simple_type_bases: BTreeMap::new(),
            element_fragment_element_signatures: BTreeMap::new(),
            element_fragment_relaxed_elements: BTreeSet::new(),
        }
    }

    /// Lädt ein Schema und alle seine Imports rekursiv.
    ///
    /// 2-Phasen-Ansatz für zirkuläre Imports:
    /// 1. Alle Schemas rekursiv laden, Platzhalter registrieren
    /// 2. Alle Schemas vollständig parsen (mit Zugriff auf alle Platzhalter)
    fn load_schema(&mut self, schema_path: &Path) -> Result<()> {
        // Phase 1: Alle Schemas rekursiv sammeln und Platzhalter registrieren
        let schemas = self.collect_all_schemas(schema_path)?;

        // Phase 2: Alle Schemas vollständig parsen
        for schema in &schemas {
            self.parse_schema_fully(&schema.content, &schema.target_ns)?;
        }

        Ok(())
    }

    /// Phase 1: Sammelt alle Schemas rekursiv und registriert Platzhalter.
    fn collect_all_schemas(&mut self, schema_path: &Path) -> Result<Vec<LoadedSchema>> {
        let mut schemas = Vec::new();
        self.collect_schemas_recursive(schema_path, &mut schemas)?;
        Ok(schemas)
    }

    /// Rekursive Schema-Sammlung mit Platzhalter-Registrierung.
    fn collect_schemas_recursive(
        &mut self,
        schema_path: &Path,
        schemas: &mut Vec<LoadedSchema>,
    ) -> Result<()> {
        let canonical = schema_path.canonicalize().map_err(|e| {
            Error::XsdParseError(format!(
                "Cannot resolve schema path '{}': {}",
                schema_path.display(),
                e
            ))
        })?;

        // Bereits besucht? → Skip
        if self.loaded.contains_key(&canonical) {
            return Ok(());
        }

        // Schema-Datei lesen
        let content = std::fs::read_to_string(&canonical).map_err(|e| {
            Error::XsdParseError(format!(
                "Cannot read schema '{}': {}",
                schema_path.display(),
                e
            ))
        })?;

        // DoS-Schutz
        if content.len() > MAX_XSD_SIZE {
            return Err(Error::XsdParseError(format!(
                "XSD document too large: {} bytes (max {} bytes)",
                content.len(),
                MAX_XSD_SIZE
            )));
        }

        // XML parsen für Namespace und Imports
        let xml_opts = ParsingOptions { allow_dtd: true, ..Default::default() };
        let doc = Document::parse_with_options(&content, xml_opts)
            .map_err(|e| Error::XsdParseError(format!("XML: {e}")))?;
        let root = doc.root_element();

        if root.tag_name().name() != "schema" || root.tag_name().namespace() != Some(XS_NS) {
            return Err(Error::XsdParseError(
                "Root element must be xs:schema".to_string(),
            ));
        }

        let target_ns = root.attribute("targetNamespace").unwrap_or("").to_string();

        // Schema als besucht markieren (verhindert Endlosschleifen)
        self.loaded
            .insert(canonical.clone(), (target_ns.clone(), false));

        // Platzhalter für globale Namen registrieren (ermöglicht Cross-Referenzen)
        self.register_placeholders(&root, &target_ns);

        // Imports sammeln (bevor wir content verschieben)
        let imports = collect_imports(&root);
        let schema_dir = canonical.parent().unwrap_or(Path::new(".")).to_path_buf();

        // Schema für Phase 2 speichern (content wird hier verschoben)
        schemas.push(LoadedSchema {
            content,
            target_ns,
        });

        // Imports rekursiv sammeln (nach dem Verschieben von content)

        for import in imports {
            // Bekannte Namespaces überspringen — deren Typen sind hardcodiert
            if let Some(ref ns) = import.namespace {
                if ns == "http://www.w3.org/XML/1998/namespace"
                    || ns == "http://www.w3.org/2001/XMLSchema"
                {
                    continue;
                }
            }

            let import_path = schema_dir.join(&import.schema_location);
            if !import_path.exists() {
                return Err(Error::XsdParseError(format!(
                    "Imported schema not found: '{}'",
                    import.schema_location
                )));
            }
            self.collect_schemas_recursive(&import_path, schemas)?;
        }

        Ok(())
    }

    /// Registriert Platzhalter für alle globalen Namen in einem Schema.
    fn register_placeholders(&mut self, root: &Node, target_ns: &str) {
        for child in root
            .children()
            .filter(|n| n.is_element() && n.tag_name().namespace() == Some(XS_NS))
        {
            match child.tag_name().name() {
                "element" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(target_ns, name));
                        self.global_elements
                            .entry(qname.clone())
                            .or_insert_with(|| ElementDeclaration::new(qname.clone()));
                        self.all_elements.insert(qname);
                    }
                }
                "simpleType" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(target_ns, name));
                        self.global_types.entry(qname.clone()).or_insert_with(|| {
                            TypeDefinition::Simple {
                                name: Some(qname),
                                variety: SimpleTypeVariety::Atomic,
                                base_type_qname: None,
                                base_type: None,
                                enumeration_values: Vec::new(),
                                is_union: false,
                                has_named_sub_types: false,
                            }
                        });
                    }
                }
                "complexType" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(target_ns, name));
                        self.global_types.entry(qname.clone()).or_insert_with(|| {
                            TypeDefinition::Complex {
                                name: Some(qname),
                                base_type: None,
                                derivation: None,
                                attributes: Vec::new(),
                                attribute_wildcard: None,
                                content: ContentType::Empty,
                                has_named_sub_types: false,
                            }
                        });
                    }
                }
                "group" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(target_ns, name));
                        self.global_groups
                            .entry(qname)
                            .or_insert_with(|| ModelGroup::sequence(Vec::new()));
                    }
                }
                "attribute" => {
                    if let Some(name) = child.attribute("name") {
                        let qname = Rc::new(QName::new(target_ns, name));
                        self.all_attributes.insert(qname.clone());
                        self.global_attributes.entry(qname).or_insert_with(|| {
                            AttributeDeclaration {
                                type_definition: None,
                            }
                        });
                    }
                }
                _ => {}
            }
        }
    }

    /// Phase 2: Parsed ein Schema vollständig (mit Zugriff auf alle Platzhalter).
    fn parse_schema_fully(&mut self, content: &str, _target_ns: &str) -> Result<()> {
        let xml_opts = ParsingOptions { allow_dtd: true, ..Default::default() };
        let doc = Document::parse_with_options(content, xml_opts)
            .map_err(|e| Error::XsdParseError(format!("XML: {e}")))?;
        let root = doc.root_element();

        let mut parser = XsdParser::from_schema_element(&root)?;
        parser.set_external_components(self);
        parser.parse_schema_with_imports(&root)?;

        // Komponenten in den Context übernehmen (überschreibt Platzhalter)
        self.merge_parser(parser);

        Ok(())
    }

    /// Merged die Komponenten eines Parsers in den Context.
    fn merge_parser(&mut self, parser: XsdParser) {
        // Überschreibe Platzhalter mit vollständigen Definitionen
        self.global_elements.extend(parser.global_elements);
        self.global_types.extend(parser.global_types);
        self.global_groups.extend(parser.global_groups);
        self.global_attributes.extend(parser.global_attributes);
        self.all_elements.extend(parser.all_elements);
        self.all_attributes.extend(parser.all_attributes);
        self.element_fragment_relaxed_elements
            .extend(parser.element_fragment_relaxed_elements);
        for (qname, signature) in parser.element_fragment_element_signatures {
            self.merge_element_fragment_signature(qname, signature);
        }
        self.all_element_declarations
            .extend(parser.all_element_declarations);
        self.pending_refs.extend(parser.pending_refs);
        self.type_bases.extend(parser.type_bases);
        self.simple_type_bases.extend(parser.simple_type_bases);
    }

    fn merge_element_fragment_signature(
        &mut self,
        qname: Rc<QName>,
        signature: ElementFragmentElementSignature,
    ) {
        if self
            .element_fragment_relaxed_elements
            .iter()
            .any(|k| k.as_ref() == qname.as_ref())
        {
            return;
        }
        if let Some(existing) = self.element_fragment_element_signatures.get(&qname) {
            if existing.type_name.is_none()
                || signature.type_name.is_none()
                || existing.type_name.as_ref() != signature.type_name.as_ref()
                || existing.nillable != signature.nillable
            {
                self.element_fragment_relaxed_elements.insert(qname);
            }
        } else {
            self.element_fragment_element_signatures
                .insert(qname, signature);
        }
    }

    /// Löst alle Referenzen auf (Substitution Groups, has_named_sub_types, etc.).
    fn resolve_all_references(&mut self) -> Result<()> {
        // Analog zu XsdParser::resolve_references(), aber über alle Schemas

        // 1. Particles synchronisieren (Group-Refs und Element-Refs)
        self.sync_particles();

        // 1a. ComplexType-Extension: Base-Type Inhalte/Attribute mergen
        //     (Spec XSD 1.0 §3.4.6, EXI 8.5.4.1.3.2)
        merge_complex_type_extensions_in_map(&mut self.global_types, None);

        // 2. Element type_definitions mit finalen Typen synchronisieren
        self.sync_element_types();

        // 3. SimpleType base_type transitiv auflösen
        self.resolve_transitive_base_types();

        // 4. Union-Variety propagieren
        self.propagate_union_variety();

        // 5. Substitution Groups auflösen
        self.resolve_substitution_groups()?;

        // 5a. Substitution Groups in lokalen Element-Refs aktualisieren
        self.sync_particles();

        // 6. has_named_sub_types berechnen
        self.compute_sub_types();

        Ok(())
    }

    /// Synchronisiert Particles (adaptiert von XsdParser).
    fn sync_particles(&mut self) {
        let elements = self.global_elements.clone();

        for type_def in self.global_types.values_mut() {
            if let TypeDefinition::Complex { content, .. } = type_def {
                match content {
                    ContentType::ElementOnly(particle) | ContentType::Mixed(particle) => {
                        XsdParser::sync_particle_elements(particle, &elements);
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
                            XsdParser::sync_particle_elements(particle, &elements);
                        }
                        ContentType::Empty | ContentType::Simple => {}
                    }
                }
            }
        }

        for model_group in self.global_groups.values_mut() {
            for particle in &mut model_group.particles {
                XsdParser::sync_particle_elements(particle, &elements);
            }
        }
    }

    /// Synchronisiert Element-TypeDefinitions (adaptiert von XsdParser).
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

        let update_elem = |elem: &mut ElementDeclaration| {
            if let Some(ref type_def) = elem.type_definition {
                let mut updated = update_type_def_from_globals(type_def);
                update_local_element_types(&mut updated, &global_types);
                elem.type_definition = Some(Rc::new(updated));
            }
        };

        for elem in self.global_elements.values_mut() {
            update_elem(elem);
        }

        for elem in self.all_element_declarations.values_mut() {
            update_elem(elem);
        }

        // Lokale ElementDeclarations innerhalb von globalen ComplexTypes aktualisieren.
        // Umfasst auch Attribut-Typen in ComplexTypes.
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

        // Globale Attribute aktualisieren (analog zu XsdParser::sync_element_types)
        for attr in self.global_attributes.values_mut() {
            if let Some(ref type_def) = attr.type_definition {
                let updated = update_type_def_from_globals(type_def);
                attr.type_definition = Some(Rc::new(updated));
            }
        }
    }

    /// Löst SimpleType base_type transitiv auf (adaptiert von XsdParser).
    fn resolve_transitive_base_types(&mut self) {
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

        for type_qname in types_needing_resolution {
            if let Some(resolved) = self.find_transitive_builtin(&type_qname)
                && let Some(type_def) = self.global_types.get_mut(&type_qname)
                    && let TypeDefinition::Simple { base_type, .. } = type_def {
                        *base_type = Some(resolved);
                    }
        }
    }

    /// Findet den transitiven built-in base_type (adaptiert von XsdParser).
    fn find_transitive_builtin(&self, start_qname: &Rc<QName>) -> Option<String> {
        let mut visited = FastHashSet::default();
        let mut current = start_qname.clone();

        for _ in 0..100 {
            if visited.contains(&current) {
                return None;
            }
            visited.insert(current.clone());

            if let Some(type_def) = self.global_types.get(&current)
                && let TypeDefinition::Simple {
                    base_type: Some(b), ..
                } = type_def
                {
                    return Some(b.clone());
                }

            if let Some(next) = self.simple_type_bases.get(&current) {
                if &*next.uri == XS_NS {
                    return Some(next.local_name.to_string());
                }
                current = next.clone();
            } else {
                return None;
            }
        }
        None
    }

    /// Propagiert is_union durch Derivation (adaptiert von XsdParser).
    fn propagate_union_variety(&mut self) {
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
                return false;
            }
            visited.insert(current.clone());

            if let Some(type_def) = self.global_types.get(&current)
                && let TypeDefinition::Simple { is_union: true, .. } = type_def {
                    return true;
                }

            if let Some(next) = self.simple_type_bases.get(&current) {
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

    /// Löst Substitution Groups auf (adaptiert von XsdParser).
    fn resolve_substitution_groups(&mut self) -> Result<()> {
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

        self.compute_transitive_substitution_groups();
        Ok(())
    }

    /// Berechnet transitive Substitution Group Closure.
    fn compute_transitive_substitution_groups(&mut self) {
        let direct_members: FastHashMap<Rc<QName>, Vec<Rc<QName>>> = self
            .global_elements
            .iter()
            .filter(|(_, elem)| !elem.substitution_group.is_empty())
            .map(|(qname, elem)| (qname.clone(), elem.substitution_group.clone()))
            .collect();

        for head_qname in direct_members.keys() {
            let mut all_members = FastHashSet::default();
            let mut queue = std::collections::VecDeque::new();

            if let Some(direct) = direct_members.get(head_qname) {
                for member in direct {
                    if all_members.insert(member.clone()) {
                        queue.push_back(member.clone());
                    }
                }
            }

            while let Some(current) = queue.pop_front() {
                if let Some(sub_members) = direct_members.get(&current) {
                    for sub_member in sub_members {
                        if all_members.insert(sub_member.clone()) {
                            queue.push_back(sub_member.clone());
                        }
                    }
                }
            }

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

    /// Berechnet has_named_sub_types (adaptiert von XsdParser).
    fn compute_sub_types(&mut self) {
        let complex_types_with_children: FastHashSet<Rc<QName>> =
            self.type_bases.values().cloned().collect();
        let simple_types_with_children: FastHashSet<Rc<QName>> =
            self.simple_type_bases.values().cloned().collect();

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
                    if simple_types_with_children.contains(type_name) {
                        *has_named_sub_types = true;
                    }
                }
            }
        }

        self.sync_element_types();
    }

    /// Baut SchemaInfo aus allen gesammelten Komponenten.
    fn build(self) -> SchemaInfo {
        let mut builder = SchemaInfo::builder();

        for qname in self.global_elements.keys() {
            builder = builder.global_element((**qname).clone());
        }

        for qname in &self.all_elements {
            builder = builder.all_element((**qname).clone());
        }

        for qname in &self.all_attributes {
            builder = builder.attribute((**qname).clone());
        }

        builder = builder.element_fragment_relaxed_elements(
            self.element_fragment_relaxed_elements.clone(),
        );

        builder = builder.element_fragment_relaxed_elements(
            self.element_fragment_relaxed_elements.clone(),
        );

        let mut element_decls: std::collections::BTreeMap<Rc<QName>, Rc<ElementDeclaration>> = self
            .global_elements
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect();

        for (k, v) in self.all_element_declarations.into_iter() {
            element_decls.entry(k).or_insert_with(|| Rc::new(v));
        }
        builder = builder.element_declarations(element_decls);

        let type_defs: std::collections::BTreeMap<Rc<QName>, Rc<TypeDefinition>> = self
            .global_types
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect();
        builder = builder.type_definitions(type_defs);

        // Globale Attribut-Typen (für AT(*) Typ-Lookup)
        let mut attr_types: std::collections::BTreeMap<Rc<QName>, Rc<TypeDefinition>> =
            std::collections::BTreeMap::new();
        for (qname, decl) in self.global_attributes.into_iter() {
            if let Some(td) = decl.type_definition {
                attr_types.insert(qname, td);
            }
        }
        builder = builder.global_attribute_types(attr_types);

        let model_groups: std::collections::BTreeMap<Rc<QName>, Rc<ModelGroup>> = self
            .global_groups
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect();
        builder = builder.model_groups(model_groups);

        builder.build()
    }
}

/// Sammelt alle xs:import Elemente aus einem Schema.
fn collect_imports(root: &Node) -> Vec<XsdImport> {
    root.children()
        .filter(|n| {
            n.is_element()
                && n.tag_name().namespace() == Some(XS_NS)
                && n.tag_name().name() == "import"
        })
        .filter_map(|node| {
            // schemaLocation ist optional laut XSD-Spec, aber wir brauchen es
            let schema_location = node.attribute("schemaLocation")?;
            Some(XsdImport {
                namespace: node.attribute("namespace").map(|s| s.to_string()),
                schema_location: schema_location.to_string(),
            })
        })
        .collect()
}
