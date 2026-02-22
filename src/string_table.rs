//! String Table (Spec 7.3, 7.3.1, Appendix D).
//!
//! EXI uses a string table to assign "compact identifiers" to string values.
//! The string table is divided into partitions: URI, Prefix, Local-Name, and Value.
//!
//! Lifecycle: One StringTable per EXI stream, not reusable across streams.

use std::rc::Rc;

use crate::FastHashMap;
use crate::memory_monitor::MemoryMonitor;
use crate::qname::QName;

/// Berechnet die Anzahl Bits fuer einen Wert: ceil(log2(n)).
///
/// Aufrufer uebergibt je nach Partition-Typ:
/// - Spec 7.3.2 (Compact ID): m+1 uebergeben → n = ceil(log2(m+1))
/// - Spec 7.3.3 (String Literal): m uebergeben → n = ceil(log2(m))
///
/// # Beispiele
/// - 0, 1 -> 0 Bits (implizit)
/// - 2 -> 1 Bit
/// - 3, 4 -> 2 Bits
/// - 5..8 -> 3 Bits
pub(crate) fn bits_for_value(n: usize) -> u8 {
    crate::bit_width::for_count(n)
}

/// Schwelle ab der Partition von linearer Suche auf HashMap wechselt.
/// Für ≤64 Einträge ist lineare Suche auf Vec<Rc<str>> schneller als
/// HashMap-Hashing+Probe (passt in ~8 L1-Cache-Lines).
const PARTITION_LINEAR_THRESHOLD: usize = 64;

/// Generische Partition für String→ID Mapping.
/// Verwendet Rc<str> um doppelte String-Allokation zu vermeiden.
///
/// Für kleine Partitionen (<64 Einträge) wird lineare Suche auf dem
/// entries-Vec verwendet. Ab 64 Einträgen wird lazy ein HashMap angelegt.
/// URI-/Prefix-Partitionen haben typischerweise 3-10 Einträge.
#[derive(Clone)]
struct Partition {
    entries: Vec<Rc<str>>,
    lookup: Option<FastHashMap<Rc<str>, usize>>,
}

impl Partition {
    /// Erstellt eine neue leere Partition.
    fn new() -> Self {
        Self {
            entries: Vec::new(),
            lookup: None,
        }
    }

    /// Erstellt eine Partition mit initialen Werten.
    fn with_entries(values: &[&str]) -> Self {
        let mut partition = Self::new();
        for value in values {
            partition.add(value);
        }
        partition
    }

    /// Lookup String → Option<Compact ID>
    #[inline]
    fn lookup(&self, value: &str) -> Option<usize> {
        if let Some(ref map) = self.lookup {
            map.get(value).copied()
        } else {
            self.entries.iter().position(|e| &**e == value)
        }
    }

    /// Add String → Compact ID (idempotent)
    fn add(&mut self, value: &str) -> usize {
        if let Some(existing) = self.lookup(value) {
            return existing;
        }

        let id = self.entries.len();
        let rc: Rc<str> = value.into();

        // HashMap lazy anlegen wenn Threshold erreicht
        if self.lookup.is_none() && id + 1 >= PARTITION_LINEAR_THRESHOLD {
            let mut map = FastHashMap::with_capacity_and_hasher(
                id + 1,
                Default::default(),
            );
            for (i, e) in self.entries.iter().enumerate() {
                map.insert(Rc::clone(e), i);
            }
            map.insert(Rc::clone(&rc), id);
            self.lookup = Some(map);
        } else if let Some(ref mut map) = self.lookup {
            map.insert(Rc::clone(&rc), id);
        }

        self.entries.push(rc);
        id
    }

    /// Get String by Compact ID
    fn get(&self, id: usize) -> Option<&str> {
        self.entries.get(id).map(AsRef::as_ref)
    }

    /// Get Rc<str> by Compact ID (Refcount-Increment statt Kopie).
    fn get_rc(&self, id: usize) -> Option<Rc<str>> {
        self.entries.get(id).cloned()
    }

    /// Anzahl Einträge
    fn len(&self) -> usize {
        self.entries.len()
    }
}

/// Reine Speicher-Partition ohne Lookup-HashMap (Spec 7.3.3).
///
/// Für ID→String Decode und size_for_bits().
/// Der Lookup erfolgt über den zentralen `value_lookup` im StringTable.
#[derive(Clone)]
struct ValueStore {
    entries: Vec<Option<Rc<str>>>,
    next_id: usize,
    count: usize,
    capacity: Option<usize>,
    at_capacity: bool,
}

impl ValueStore {
    /// Erstellt neuen unbounded Store.
    fn new() -> Self {
        Self {
            entries: Vec::new(),
            next_id: 0,
            count: 0,
            capacity: None,
            at_capacity: false,
        }
    }

    /// Erstellt neuen bounded Store mit fester Kapazität.
    ///
    /// Alloziert NICHT vorab -- Einträge wachsen lazy bei add()/set().
    /// Verhindert OOM bei manipulierten Kapazitätswerten aus dem EXI-Header.
    fn with_capacity(cap: usize) -> Self {
        Self {
            entries: Vec::new(),
            next_id: 0,
            count: 0,
            capacity: Some(cap),
            at_capacity: false,
        }
    }

    /// Get String by Compact ID (None wenn evicted oder nicht vorhanden)
    fn get(&self, id: usize) -> Option<&str> {
        self.entries.get(id)?.as_ref().map(AsRef::as_ref)
    }

    /// Get Rc<str> by Compact ID (Refcount-Increment statt Kopie).
    fn get_rc(&self, id: usize) -> Option<Rc<str>> {
        self.entries.get(id)?.as_ref().cloned()
    }

    /// Anzahl für Bitbreiten-Berechnung (Spec 7.3.3).
    /// - Unbounded: next_id
    /// - Bounded vor Wrap: next_id
    /// - Bounded nach Wrap: capacity
    fn size_for_bits(&self) -> usize {
        match self.capacity {
            None => self.next_id,
            Some(cap) if self.at_capacity => cap,
            Some(_) => self.next_id,
        }
    }

    /// Fügt einen Wert hinzu (unbounded: append, bounded: an Position).
    /// Gibt die zugewiesene ID zurück.
    fn add(&mut self, rc: Rc<str>) -> usize {
        let id = self.next_id;
        if self.capacity.is_some() {
            // Bounded: lazy wachsen oder ueberschreiben
            if id < self.entries.len() {
                self.entries[id] = Some(rc);
            } else {
                self.entries.push(Some(rc));
            }
        } else {
            // Unbounded: append
            self.entries.push(Some(rc));
        }
        self.next_id += 1;
        self.count += 1;
        id
    }

    /// Set an spezifischer ID (für bounded global nach Wrap).
    fn set(&mut self, id: usize, rc: Rc<str>) {
        // Lazy wachsen falls noetig (bounded: nicht vorab alloziert)
        if id >= self.entries.len() {
            self.entries.resize(id + 1, None);
        }

        if self.entries[id].is_none() {
            self.count += 1;
        }
        self.entries[id] = Some(rc);

        if id >= self.next_id {
            self.next_id = id + 1;
        }

        if let Some(cap) = self.capacity
            && id + 1 >= cap
            && !self.at_capacity
        {
            self.at_capacity = true;
        }
    }

    /// Entfernt String bei ID (setzt auf None).
    fn remove(&mut self, id: usize) {
        if let Some(entry) = self.entries.get_mut(id) {
            if entry.is_some() {
                self.count -= 1;
            }
            *entry = None;
        }
    }

    /// Lineare Suche nach Value (O(n), nur für Test-API und add_value-Idempotenz).
    fn find(&self, value: &str) -> Option<usize> {
        self.entries.iter().position(|e| e.as_ref().is_some_and(|s| &**s == value))
    }

    /// Anzahl tatsächlich belegter Einträge (ohne Holes). O(1).
    fn len(&self) -> usize {
        self.count
    }
}

/// Eintrag im zentralen Value-Lookup (Exificient-style).
#[derive(Clone)]
struct ValueInfo {
    /// QName-Key des ersten Einfügens (für Local-Hit-Check).
    qname_key: usize,
    /// Compact ID in Global Partition.
    global_id: usize,
    /// Compact ID in Local Partition des qname_key.
    local_id: usize,
}

/// Well-known URIs und IDs (Table D-1, D-2)
const URI_EMPTY: &str = "";
const URI_XML: &str = "http://www.w3.org/XML/1998/namespace";
/// XSI Namespace URI (http://www.w3.org/2001/XMLSchema-instance).
///
/// Wird für xsi:type und xsi:nil Attribute verwendet.
pub const URI_XSI: &str = "http://www.w3.org/2001/XMLSchema-instance";
/// XSD Namespace URI (http://www.w3.org/2001/XMLSchema).
///
/// Table D-2: Wird bei schema-informed Streams hinzugefügt.
pub const URI_XSD: &str = "http://www.w3.org/2001/XMLSchema";

const URI_ID_EMPTY: usize = 0;
const URI_ID_XML: usize = 1;
const URI_ID_XSI: usize = 2;
const URI_ID_XSD: usize = 3;

/// Table D-5: 46 XSD built-in types für XSD-NS Local-Name Partition.
///
/// Alphabetisch sortiert gemäß Spec D.3.
const XSD_BUILTIN_TYPES: [&str; 46] = [
    "ENTITIES",
    "ENTITY",
    "ID",
    "IDREF",
    "IDREFS",
    "NCName",
    "NMTOKEN",
    "NMTOKENS",
    "NOTATION",
    "Name",
    "QName",
    "anySimpleType",
    "anyType",
    "anyURI",
    "base64Binary",
    "boolean",
    "byte",
    "date",
    "dateTime",
    "decimal",
    "double",
    "duration",
    "float",
    "gDay",
    "gMonth",
    "gMonthDay",
    "gYear",
    "gYearMonth",
    "hexBinary",
    "int",
    "integer",
    "language",
    "long",
    "negativeInteger",
    "nonNegativeInteger",
    "nonPositiveInteger",
    "normalizedString",
    "positiveInteger",
    "short",
    "string",
    "time",
    "token",
    "unsignedByte",
    "unsignedInt",
    "unsignedLong",
    "unsignedShort",
];

// === Encoding Result Types ===

/// Ergebnis eines String-Lookups für Compact ID Partitionen (Spec 7.3.2).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompactIdResult {
    /// String gefunden mit Compact ID.
    Hit(usize),
    /// String nicht gefunden, wurde hinzugefügt mit Compact ID.
    Miss(usize),
}

/// Ergebnis eines String-Lookups für String Literal Partitionen (Spec 7.3.3).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringLiteralResult {
    /// String gefunden mit Compact ID.
    Hit(usize),
    /// String nicht gefunden (wurde ggf. hinzugefügt).
    Miss,
}

/// Ergebnis eines Value-Lookups (Spec 7.3.3).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueResult {
    /// String gefunden in Local Partition.
    HitLocal(usize),
    /// String gefunden in Global Partition.
    HitGlobal(usize),
    /// String nicht gefunden.
    Miss,
}

/// Tracking-Info für Eviction (Spec 7.3.3).
#[derive(Clone)]
struct GlobalValueEntry {
    /// Kompakter Key (fortlaufende ID) statt QName — vermeidet Rc-Clones und String-Hashing.
    key: usize,
    local_id: usize,
}

/// String Table (Spec 7.3)
#[derive(Clone)]
pub struct StringTable {
    uri: Partition,
    prefix: FastHashMap<usize, Partition>,
    local_name: FastHashMap<usize, Partition>,
    /// Zentraler Value-Lookup: EIN HashMap für alle Values (Exificient-style).
    /// Reduziert String-Hashing von 2× auf 1× pro encode_value().
    value_lookup: FastHashMap<Rc<str>, ValueInfo>,
    global_value: ValueStore,
    /// Lokale Value-Stores, indiziert durch fortlaufende QName-ID statt QName.
    /// Nur für ID→String Decode und size_for_bits().
    local_value: FastHashMap<usize, ValueStore>,
    /// QName identity_hash → ID Index für local_value. Jeder QName bekommt einmalig eine ID.
    /// Verwendet u64 (vorberechneter Hash) statt QName als Key — eliminiert
    /// String-Equality-Vergleiche und Rc-Clone bei HashMap-Operationen.
    ///
    /// Kollisionsrisiko: Bei 64-Bit-Hash sind ~2^32 verschiedene QNames nötig
    /// für 50% Kollisionswahrscheinlichkeit (Birthday-Paradoxon). In der Praxis
    /// hat ein EXI-Dokument nie so viele verschiedene QNames.
    qname_to_value_id: FastHashMap<u64, usize>,
    /// Nächste verfügbare QName-ID.
    next_qname_id: usize,

    // Value-Encoding Optionen
    global_id: usize,
    value_max_length: Option<usize>,
    value_partition_capacity: Option<usize>,
    global_id_to_entry: Vec<Option<GlobalValueEntry>>,
    /// Decode-Only Modus: value_lookup wird nicht befüllt (Decoder braucht nur
    /// ID→String, nicht String→ID). Spart HashMap-Insert + 1 Rc::clone pro Value.
    decode_only: bool,
    /// Optionaler RAM-Monitor fuer OOM-Vermeidung bei grossen Dateien.
    memory_monitor: Option<MemoryMonitor>,
}

impl StringTable {
    /// Erstellt neue String Table mit Pre-Population (schema-less, Appendix D).
    pub fn new() -> Self {
        Self::with_options(None, None)
    }

    /// Erstellt String Table mit Options (Spec 7.3.3).
    ///
    /// - `value_max_length`: Maximale String-Länge für Value-Hinzufügung (None = unbounded)
    /// - `value_partition_capacity`: Maximale Value-Einträge (None = unbounded)
    pub fn with_options(
        value_max_length: Option<usize>,
        value_partition_capacity: Option<usize>,
    ) -> Self {
        Self::with_full_options(value_max_length, value_partition_capacity, false)
    }

    /// Erstellt String Table mit allen Options (Spec 7.3.1, 7.3.3, Appendix D).
    ///
    /// - `value_max_length`: Maximale String-Länge für Value-Hinzufügung (None = unbounded)
    /// - `value_partition_capacity`: Maximale Value-Einträge (None = unbounded)
    /// - `_preserve_prefixes`: Unused - Pre-Population ist laut Spec D.1-D.3 IMMER gleich.
    ///   Parameter bleibt für API-Kompatibilität, beeinflusst nur QName-Encoding (nicht String Table).
    pub fn with_full_options(
        value_max_length: Option<usize>,
        value_partition_capacity: Option<usize>,
        _preserve_prefixes: bool,
    ) -> Self {
        // Spec Appendix D.1: URI partition ist IMMER mit 3 Einträgen pre-populated
        // (unabhängig von preserve.prefixes - das beeinflusst nur QName-Encoding)
        let uri = Partition::with_entries(&[URI_EMPTY, URI_XML, URI_XSI]);

        // Spec Appendix D.2: Prefix partitions sind IMMER pre-populated
        let prefix = [
            (URI_ID_EMPTY, Partition::with_entries(&[""])),
            (URI_ID_XML, Partition::with_entries(&["xml"])),
            (URI_ID_XSI, Partition::with_entries(&["xsi"])),
        ].into_iter().collect();

        // Spec Appendix D.3: Local-Name partitions sind IMMER pre-populated
        let local_name = [
            (
                URI_ID_XML,
                Partition::with_entries(&["base", "id", "lang", "space"]),
            ),
            (URI_ID_XSI, Partition::with_entries(&["nil", "type"])),
        ].into_iter().collect();

        Self::assemble(uri, prefix, local_name, value_max_length, value_partition_capacity)
    }

    /// Erstellt String Table mit Schema Pre-Population (Spec 7.3.1, Appendix D).
    ///
    /// Bei schema-informed Streams werden zusätzlich pre-populated:
    /// - URI-Partition: XSD-NS + Target-Namespaces + Wildcard-URIs (Table D-2, 7.3.1)
    /// - Local-Name: XSD-NS (46 built-in types) + Schema-Attribute/Elemente/Typen (Table D-5, D.3)
    pub fn from_schema(schema: &crate::schema::SchemaInfo) -> Self {
        Self::from_schema_with_options(schema, None, None)
    }

    /// Erstellt String Table mit Schema Pre-Population und Value Options.
    ///
    /// - `value_max_length`: Maximale String-Länge für Value-Hinzufügung (None = unbounded)
    /// - `value_partition_capacity`: Maximale Value-Einträge (None = unbounded)
    pub fn from_schema_with_options(
        schema: &crate::schema::SchemaInfo,
        value_max_length: Option<usize>,
        value_partition_capacity: Option<usize>,
    ) -> Self {
        // Built-in URIs (Spec Table D-1, D-2)
        const BUILTIN_URIS: [&str; 4] = [URI_EMPTY, URI_XML, URI_XSI, URI_XSD];

        // === URI Partition aufbauen (Spec 7.3.1, Table D-1, D-2) ===
        let schema_uris = Self::collect_schema_uris(schema);

        let mut uri_entries: Vec<&str> = BUILTIN_URIS.to_vec();
        for uri in schema_uris {
            if !BUILTIN_URIS.contains(&uri) {
                uri_entries.push(uri);
            }
        }
        let uri = Partition::with_entries(&uri_entries);

        // === Prefix Partition (Spec D.2) ===
        let prefix = [
            (URI_ID_EMPTY, Partition::with_entries(&[""])),
            (URI_ID_XML, Partition::with_entries(&["xml"])),
            (URI_ID_XSI, Partition::with_entries(&["xsi"])),
        ].into_iter().collect();

        // === Local-Name Partitions aufbauen (Spec D.3, D.4, D.5) ===
        let local_names_by_uri = Self::collect_local_names_by_uri(schema);
        let local_name = Self::build_local_name_partitions(&uri, local_names_by_uri);

        Self::assemble(uri, prefix, local_name, value_max_length, value_partition_capacity)
    }

    /// Gemeinsame Initialisierung: Setzt Value-Strukturen auf und baut StringTable zusammen.
    fn assemble(
        uri: Partition,
        prefix: FastHashMap<usize, Partition>,
        local_name: FastHashMap<usize, Partition>,
        value_max_length: Option<usize>,
        value_partition_capacity: Option<usize>,
    ) -> Self {
        let (global_value, global_id_to_entry) = match value_partition_capacity {
            Some(cap) => (ValueStore::with_capacity(cap), Vec::new()),
            None => (ValueStore::new(), Vec::new()),
        };

        Self {
            uri,
            prefix,
            local_name,
            value_lookup: FastHashMap::with_capacity_and_hasher(1024, Default::default()),
            global_value,
            local_value: FastHashMap::with_capacity_and_hasher(32, Default::default()),
            qname_to_value_id: FastHashMap::with_capacity_and_hasher(32, Default::default()),
            next_qname_id: 0,
            global_id: 0,
            value_max_length,
            value_partition_capacity,
            global_id_to_entry,
            decode_only: false,
            memory_monitor: None,
        }
    }

    /// Aktiviert Decode-Only Modus: value_lookup wird nicht befüllt.
    /// Der Decoder braucht nur ID→String (get_local_value/get_global_value),
    /// nicht String→ID (value_lookup). Spart HashMap-Insert + Rc::clone pro Value.
    pub fn set_decode_only(&mut self, decode_only: bool) {
        self.decode_only = decode_only;
        if decode_only {
            self.value_lookup = FastHashMap::default();
        }
    }

    /// Setzt den MemoryMonitor fuer OOM-Vermeidung bei grossen Dateien.
    pub fn set_memory_monitor(&mut self, monitor: MemoryMonitor) {
        self.memory_monitor = Some(monitor);
    }

    /// Prueft den RAM-Verbrauch via MemoryMonitor (falls gesetzt).
    /// Bei `None` sofort `Ok(())` (Zero Overhead).
    #[inline]
    pub(crate) fn check_memory(&mut self, estimated_bytes: u64) -> crate::Result<()> {
        if let Some(monitor) = &mut self.memory_monitor {
            monitor.on_entry_added(estimated_bytes)?;
        }
        Ok(())
    }

    /// Sammelt alle URIs aus dem Schema (sortiert, unique).
    fn collect_schema_uris(schema: &crate::schema::SchemaInfo) -> std::collections::BTreeSet<&str> {
        use std::collections::BTreeSet;

        let mut uris: BTreeSet<&str> = BTreeSet::new();
        let mut visited = crate::FastHashSet::<*const crate::schema::TypeDefinition>::default();

        // URIs aus Elementen und Attributen
        for qname in schema.all_elements() {
            if !qname.uri.is_empty() {
                uris.insert(&qname.uri);
            }
        }
        for qname in schema.all_attributes() {
            if !qname.uri.is_empty() {
                uris.insert(&qname.uri);
            }
        }

        // URIs aus Type-Definitions (inkl. Wildcard-URIs)
        for type_def in schema.type_definitions().values() {
            Self::collect_wildcard_uris_from_type_def(type_def.as_ref(), &mut uris, &mut visited);
        }

        // URIs aus lokalen (anonymen) Type-Definitions in Element-Deklarationen
        for elem_decl in schema.element_declarations().values() {
            if let Some(type_def) = elem_decl.type_definition.as_ref() {
                Self::collect_wildcard_uris_from_type_def(
                    type_def.as_ref(),
                    &mut uris,
                    &mut visited,
                );
            }
        }

        uris
    }

    /// Sammelt Wildcard-URIs aus einer TypeDefinition.
    fn collect_wildcard_uris_from_type_def<'a>(
        type_def: &'a crate::schema::TypeDefinition,
        uris: &mut std::collections::BTreeSet<&'a str>,
        visited: &mut crate::FastHashSet<*const crate::schema::TypeDefinition>,
    ) {
        use crate::schema::{AttributeWildcard, ContentType};

        let type_ptr = type_def as *const crate::schema::TypeDefinition;
        if !visited.insert(type_ptr) {
            return;
        }

        let (attribute_wildcard, content) = match type_def {
            crate::schema::TypeDefinition::Complex {
                attribute_wildcard,
                content,
                ..
            } => (attribute_wildcard, content),
            crate::schema::TypeDefinition::Simple { .. } => return,
        };

        // Attribute Wildcard URIs
        if let Some(AttributeWildcard::Namespaces(wildcard_uris)) = attribute_wildcard {
            for uri in wildcard_uris {
                if !uri.is_empty() {
                    uris.insert(uri);
                }
            }
        }

        // Content Wildcard URIs
        if let ContentType::ElementOnly(p) | ContentType::Mixed(p) = content {
            Self::collect_wildcard_uris_from_particle(p, uris, visited);
        }
    }

    /// Sammelt Wildcard-URIs rekursiv aus einem Particle.
    fn collect_wildcard_uris_from_particle<'a>(
        particle: &'a crate::schema::Particle,
        uris: &mut std::collections::BTreeSet<&'a str>,
        visited: &mut crate::FastHashSet<*const crate::schema::TypeDefinition>,
    ) {
        use crate::schema::{ParticleTerm, WildcardConstraint};

        match &particle.term {
            ParticleTerm::Wildcard(wildcard) => {
                if let WildcardConstraint::Namespaces(ns_uris) = &wildcard.constraint {
                    for uri in ns_uris {
                        if !uri.is_empty() {
                            uris.insert(uri);
                        }
                    }
                }
            }
            ParticleTerm::ModelGroup(group) => {
                for p in &group.particles {
                    Self::collect_wildcard_uris_from_particle(p, uris, visited);
                }
            }
            ParticleTerm::Element(elem_decl) => {
                if let Some(type_def) = elem_decl.type_definition.as_ref() {
                    Self::collect_wildcard_uris_from_type_def(
                        type_def.as_ref(),
                        uris,
                        visited,
                    );
                }
            }
        }
    }

    /// Sammelt Local-Names pro URI-Namespace (sortiert, dedupliziert).
    fn collect_local_names_by_uri(
        schema: &crate::schema::SchemaInfo,
    ) -> std::collections::BTreeMap<&str, std::collections::BTreeSet<&str>> {
        use std::collections::BTreeMap;
        use std::collections::BTreeSet;

        let mut local_names: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();

        // Elemente (all_elements enthält auch global_elements)
        for qname in schema.all_elements() {
            local_names
                .entry(&qname.uri)
                .or_default()
                .insert(&qname.local_name);
        }

        // Attribute
        for qname in schema.all_attributes() {
            local_names
                .entry(&qname.uri)
                .or_default()
                .insert(&qname.local_name);
        }

        // Type-Namen
        for qname in schema.type_definitions().keys() {
            local_names
                .entry(&qname.uri)
                .or_default()
                .insert(&qname.local_name);
        }

        local_names
    }

    /// Baut Local-Name Partitionen aus gesammelten Namen.
    fn build_local_name_partitions(
        uri_partition: &Partition,
        schema_local_names: std::collections::BTreeMap<&str, std::collections::BTreeSet<&str>>,
    ) -> FastHashMap<usize, Partition> {
        use std::collections::BTreeSet;

        // Built-in Local-Names (Table D-4, D-5)
        let xml_names: BTreeSet<&str> = ["base", "id", "lang", "space"].into_iter().collect();
        let xsi_names: BTreeSet<&str> = ["nil", "type"].into_iter().collect();
        let xsd_names: BTreeSet<&str> = XSD_BUILTIN_TYPES.into_iter().collect();

        let mut local_name: FastHashMap<usize, Partition> = FastHashMap::with_capacity_and_hasher(
            uri_partition.len(),
            Default::default(),
        );

        // Alle URIs durchgehen (Schema + Built-in)
        for uri_id in 0..uri_partition.len() {
            let uri_str = uri_partition.get(uri_id).unwrap_or("");

            // Namen sammeln: Built-in + Schema
            let mut names: BTreeSet<&str> = match uri_id {
                URI_ID_XML => xml_names.clone(),
                URI_ID_XSI => xsi_names.clone(),
                URI_ID_XSD => xsd_names.clone(),
                _ => BTreeSet::new(),
            };

            // Schema-Namen hinzufügen
            if let Some(schema_names) = schema_local_names.get(uri_str) {
                names.extend(schema_names);
            }

            // Partition erstellen falls Namen vorhanden
            if !names.is_empty() {
                let sorted_names: Vec<&str> = names.into_iter().collect();
                local_name.insert(uri_id, Partition::with_entries(&sorted_names));
            }
        }

        local_name
    }

    /// Löst einen Prefix zu seiner URI auf (für xsi:type QName-Encoding).
    ///
    /// Iteriert durch alle URI-Partitionen und sucht den Prefix in deren
    /// Prefix-Partitionen. Gibt die erste URI zurück, die den Prefix hat.
    pub fn resolve_prefix_to_uri(&self, prefix: &str) -> Option<String> {
        for (uri_id, prefix_partition) in &self.prefix {
            for i in 0..prefix_partition.len() {
                if prefix_partition.get(i) == Some(prefix) {
                    return self.uri.get(*uri_id).map(|s| s.to_string());
                }
            }
        }
        None
    }

    /// Lookup URI → Option<Compact ID>
    pub fn lookup_uri(&self, uri: &str) -> Option<usize> {
        self.uri.lookup(uri)
    }

    /// Add URI → Compact ID (idempotent)
    pub fn add_uri(&mut self, uri: &str) -> usize {
        self.uri.add(uri)
    }

    /// Get URI by Compact ID
    pub fn get_uri(&self, id: usize) -> Option<&str> {
        self.uri.get(id)
    }

    /// Get URI als Rc<str> by Compact ID (Refcount-Increment statt Kopie).
    pub fn get_uri_rc(&self, id: usize) -> Option<Rc<str>> {
        self.uri.get_rc(id)
    }

    /// Anzahl Einträge in URI Partition
    pub fn uri_count(&self) -> usize {
        self.uri.len()
    }

    // === Prefix Partition ===

    /// Lookup Prefix in Partition für URI-ID
    pub fn lookup_prefix(&self, uri_id: usize, prefix: &str) -> Option<usize> {
        self.prefix.get(&uri_id)?.lookup(prefix)
    }

    /// Add Prefix zu Partition für URI-ID (idempotent, erstellt Partition falls nötig)
    pub fn add_prefix(&mut self, uri_id: usize, prefix: &str) -> usize {
        self.prefix
            .entry(uri_id)
            .or_insert_with(Partition::new)
            .add(prefix)
    }

    /// Get Prefix by ID in Partition für URI-ID
    pub fn get_prefix(&self, uri_id: usize, id: usize) -> Option<&str> {
        self.prefix.get(&uri_id)?.get(id)
    }

    /// Get Prefix als Rc<str> by ID in Partition für URI-ID.
    pub fn get_prefix_rc(&self, uri_id: usize, id: usize) -> Option<Rc<str>> {
        self.prefix.get(&uri_id)?.get_rc(id)
    }

    /// Anzahl Einträge in Prefix Partition für URI-ID
    pub fn prefix_count(&self, uri_id: usize) -> usize {
        self.prefix.get(&uri_id).map_or(0, |p| p.len())
    }

    // === Local-Name Partition ===

    /// Lookup Local-Name in Partition für URI-ID
    pub fn lookup_local_name(&self, uri_id: usize, local_name: &str) -> Option<usize> {
        self.local_name.get(&uri_id)?.lookup(local_name)
    }

    /// Add Local-Name zu Partition für URI-ID (idempotent, erstellt Partition falls nötig)
    pub fn add_local_name(&mut self, uri_id: usize, local_name: &str) -> usize {
        self.local_name
            .entry(uri_id)
            .or_insert_with(Partition::new)
            .add(local_name)
    }

    /// Get Local-Name by ID in Partition für URI-ID
    pub fn get_local_name(&self, uri_id: usize, id: usize) -> Option<&str> {
        self.local_name.get(&uri_id)?.get(id)
    }

    /// Get Local-Name als Rc<str> by ID in Partition für URI-ID.
    pub fn get_local_name_rc(&self, uri_id: usize, id: usize) -> Option<Rc<str>> {
        self.local_name.get(&uri_id)?.get_rc(id)
    }

    /// Anzahl Einträge in Local-Name Partition für URI-ID
    pub fn local_name_count(&self, uri_id: usize) -> usize {
        self.local_name.get(&uri_id).map_or(0, |p| p.len())
    }

    // === Value Key Resolution ===

    /// Löst QName in kompakte usize-ID für local_value auf.
    /// Gibt None zurück wenn der QName noch nie als Value-Key verwendet wurde.
    fn value_key_ro(&self, qname: &QName) -> Option<usize> {
        self.qname_to_value_id.get(&qname.identity_hash()).copied()
    }

    /// Löst QName in kompakte usize-ID auf, vergibt neue ID bei Bedarf.
    /// Keine Seiteneffekte auf URI/local_name-Partitionen.
    fn value_key_or_insert(&mut self, qname: &QName) -> usize {
        let next = &mut self.next_qname_id;
        *self.qname_to_value_id
            .entry(qname.identity_hash())
            .or_insert_with(|| {
                let id = *next;
                *next += 1;
                id
            })
    }

    // === Value Partitions ===

    /// Lookup Value in Global Partition
    pub fn lookup_global_value(&self, value: &str) -> Option<usize> {
        self.value_lookup.get(value).map(|info| info.global_id)
    }

    /// Lookup Value in Local Partition für QName.
    ///
    /// Scannt ValueStore.entries linear (O(n), nur in Tests genutzt).
    /// Bewahrt API-Kompatibilität für `value_partition_isolation`-Test
    /// (gleicher Value in mehreren QName-Partitionen via `add_value()`).
    pub fn lookup_local_value(&self, qname: &QName, value: &str) -> Option<usize> {
        let key = self.value_key_ro(qname)?;
        self.local_value.get(&key)?.find(value)
    }

    /// Add Value zu Global und Local Partition (Spec 7.3.3).
    /// Returns (global_id, local_id)
    ///
    /// Idempotent: gleicher Value + gleicher QName → gleiche IDs.
    /// Gleicher Value + anderer QName → gleiche Global-ID, neue Local-ID.
    ///
    /// # Wichtig
    ///
    /// Diese Methode beachtet weder `valueMaxLength` noch `valuePartitionCapacity`.
    /// Bei StringTables mit Options stattdessen `encode_value` verwenden.
    pub fn add_value(&mut self, qname: &QName, value: &str) -> (usize, usize) {
        let key = self.value_key_or_insert(qname);
        let rc: Rc<str> = value.into();

        if let Some(info) = self.value_lookup.get(&rc) {
            if info.qname_key == key {
                return (info.global_id, info.local_id);
            }
            // Anderer QName: global idempotent, local ggf. neu
            let global_id = info.global_id;
            let local_store = self.local_value.entry(key).or_insert_with(ValueStore::new);
            if let Some(existing_local) = local_store.find(&rc) {
                return (global_id, existing_local);
            }
            let local_id = local_store.add(Rc::clone(&rc));
            return (global_id, local_id);
        }

        // Neuer Value
        let local_store = self.local_value.entry(key).or_insert_with(ValueStore::new);
        let local_id = local_store.add(Rc::clone(&rc));
        let global_id = self.global_value.add(Rc::clone(&rc));

        self.value_lookup.insert(Rc::clone(&rc), ValueInfo {
            qname_key: key,
            global_id,
            local_id,
        });

        (global_id, local_id)
    }

    /// Get Value by ID from Global Partition
    pub fn get_global_value(&self, id: usize) -> Option<&str> {
        self.global_value.get(id)
    }

    /// Get Value by ID from Local Partition für QName
    pub fn get_local_value(&self, qname: &QName, id: usize) -> Option<&str> {
        let key = self.value_key_ro(qname)?;
        self.local_value.get(&key)?.get(id)
    }

    /// Anzahl Einträge in Global Value Partition
    pub fn global_value_count(&self) -> usize {
        self.global_value.len()
    }

    /// Anzahl Einträge in Local Value Partition für QName
    pub fn local_value_count(&self, qname: &QName) -> usize {
        self.value_key_ro(qname)
            .and_then(|key| self.local_value.get(&key))
            .map_or(0, |p| p.len())
    }

    /// Bitbreiten-Größe für Global Value Partition (für Decoding).
    ///
    /// Bei bounded Partitions mit Eviction ist dies die Capacity,
    /// nicht die aktuelle Anzahl an Einträgen.
    pub fn global_value_size_for_bits(&self) -> usize {
        self.global_value.size_for_bits()
    }

    /// Bitbreiten-Größe für Local Value Partition (für Decoding).
    ///
    /// Bei bounded Partitions mit Eviction ist dies die Capacity,
    /// nicht die aktuelle Anzahl an Einträgen.
    pub fn local_value_size_for_bits(&self, qname: &QName) -> usize {
        self.value_key_ro(qname)
            .and_then(|key| self.local_value.get(&key))
            .map_or(0, |p| p.size_for_bits())
    }

    // === Encoding Methods (Spec 7.3.2, 7.3.3) ===

    /// Encode URI (Spec 7.3.2 - Compact ID Partition).
    ///
    /// Returns (result, partition_size) wobei partition_size für die
    /// Bitbreiten-Berechnung n = ⌈log₂(m+1)⌉ genutzt wird.
    pub fn encode_uri(&mut self, uri: &str) -> (CompactIdResult, usize) {
        let size = self.uri.len();
        match self.uri.lookup(uri) {
            Some(id) => (CompactIdResult::Hit(id), size),
            None => {
                let id = self.uri.add(uri);
                (CompactIdResult::Miss(id), size)
            }
        }
    }

    /// Encode Prefix (Spec 7.3.2 - Compact ID Partition).
    pub fn encode_prefix(&mut self, uri_id: usize, prefix: &str) -> (CompactIdResult, usize) {
        let partition = self.prefix.entry(uri_id).or_insert_with(Partition::new);
        let size = partition.len();
        match partition.lookup(prefix) {
            Some(id) => (CompactIdResult::Hit(id), size),
            None => {
                let id = partition.add(prefix);
                (CompactIdResult::Miss(id), size)
            }
        }
    }

    /// Encode Local-Name (Spec 7.3.3 - String Literal Partition).
    ///
    /// Returns (result, partition_size) wobei partition_size für die
    /// Bitbreiten-Berechnung n = ⌈log₂(m)⌉ genutzt wird.
    pub fn encode_local_name(
        &mut self,
        uri_id: usize,
        local_name: &str,
    ) -> (StringLiteralResult, usize) {
        let partition = self.local_name.entry(uri_id).or_insert_with(Partition::new);
        let size = partition.len();
        match partition.lookup(local_name) {
            Some(id) => (StringLiteralResult::Hit(id), size),
            None => {
                partition.add(local_name);
                (StringLiteralResult::Miss, size)
            }
        }
    }

    /// Encode Value (Spec 7.3.3 - String Literal Partition).
    ///
    /// Returns (result, global_size, local_size) für Bitbreiten-Berechnung.
    /// Beachtet valueMaxLength und valuePartitionCapacity.
    ///
    /// Nutzt pre-computed Hash für single-hash Lookup+Insert (kein doppeltes Hashing bei Miss).
    pub fn encode_value(&mut self, qname: &QName, value: &str) -> crate::Result<(ValueResult, usize, usize)> {
        let key = self.value_key_or_insert(qname);
        let global_size = self.global_value.size_for_bits();
        let local_size = self.local_value.get(&key).map_or(0, |s| s.size_for_bits());

        // Pre-compute Hash einmalig
        let hash = self.value_lookup.hasher().hash_one(value);

        // Lookup mit vorberechnetem Hash (kein erneutes Hashing)
        if let Some(info) = self.value_lookup.raw_entry().from_hash(hash, |k| k.as_ref() == value) {
            let info = info.1;
            if info.qname_key == key {
                return Ok((ValueResult::HitLocal(info.local_id), global_size, local_size));
            }
            return Ok((ValueResult::HitGlobal(info.global_id), global_size, local_size));
        }

        // Miss — Value hinzufügen mit vorberechnetem Hash
        if !self.should_skip_value(value) {
            let estimated_bytes = value.len().max(1) as u64;
            self.check_memory(estimated_bytes)?;

            if self.value_partition_capacity.is_some() {
                self.evict_global_slot(self.global_id);
            }

            let rc: Rc<str> = value.into();
            let local_store = self.local_value.entry(key).or_insert_with(ValueStore::new);
            let local_id = local_store.add(Rc::clone(&rc));

            let global_id = self.global_id;
            if self.value_partition_capacity.is_some() {
                self.global_value.set(global_id, Rc::clone(&rc));
            } else {
                self.global_value.add(Rc::clone(&rc));
            }

            if !self.decode_only {
                // Insert mit vorberechnetem Hash — kein doppeltes Hashing
                self.value_lookup.raw_entry_mut()
                    .from_hash(hash, |k| k.as_ref() == value)
                    .or_insert(rc, ValueInfo { qname_key: key, global_id, local_id });
            }

            let entry = GlobalValueEntry { key, local_id };
            if let Some(cap) = self.value_partition_capacity {
                // Lazy wachsen: Vec erst bei Bedarf vergroessern
                if global_id >= self.global_id_to_entry.len() {
                    self.global_id_to_entry.resize_with(global_id + 1, || None);
                }
                self.global_id_to_entry[global_id] = Some(entry);
                self.global_id = (self.global_id + 1) % cap;
            } else {
                self.global_id_to_entry.push(Some(entry));
                self.global_id += 1;
            }
        }
        Ok((ValueResult::Miss, global_size, local_size))
    }

    /// Prüft ob ein Value gemäß Options nicht gespeichert werden soll.
    fn should_skip_value(&self, value: &str) -> bool {
        self.value_partition_capacity == Some(0)
            || value.is_empty()
            || self.value_max_length.is_some_and(|max| {
                // Fast-Path: byte_len <= max → char_count <= max (UTF-8: bytes >= chars)
                value.len() > max && value.chars().count() > max
            })
    }

    /// Fügt Value hinzu unter Beachtung von valueMaxLength und valuePartitionCapacity.
    fn add_value_with_options(&mut self, qname: &QName, value: &str) -> crate::Result<()> {
        // Skip-Check vor Key-Erzeugung — vermeidet qname_to_value_id-Wachstum
        // wenn Value ohnehin nicht gespeichert wird.
        if self.should_skip_value(value) {
            return Ok(());
        }
        let key = self.value_key_or_insert(qname);
        self.add_value_internal(key, value)
    }

    /// Fügt Value in alle drei Strukturen ein (value_lookup, global, local).
    /// Handhabt Eviction bei bounded Partitionen.
    /// Im decode_only Modus wird value_lookup übersprungen (Decoder braucht nur ID→String).
    fn add_value_internal(&mut self, key: usize, value: &str) -> crate::Result<()> {
        if self.should_skip_value(value) {
            return Ok(());
        }

        let estimated_bytes = value.len().max(1) as u64;
        self.check_memory(estimated_bytes)?;

        // Bounded: Eviction wenn nötig
        if self.value_partition_capacity.is_some() {
            self.evict_global_slot(self.global_id);
        }

        let rc: Rc<str> = value.into();

        let local_store = self.local_value.entry(key).or_insert_with(ValueStore::new);
        let local_id = local_store.add(Rc::clone(&rc));

        let global_id = self.global_id;
        if self.value_partition_capacity.is_some() {
            self.global_value.set(global_id, Rc::clone(&rc));
        } else {
            self.global_value.add(Rc::clone(&rc));
        }

        // Im decode_only Modus: value_lookup überspringen — Decoder braucht nur ID→String.
        // Spart HashMap-Insert + String-Hashing pro Value.
        if !self.decode_only {
            self.value_lookup.insert(rc, ValueInfo {
                qname_key: key,
                global_id,
                local_id,
            });
        }

        // Tracking aktualisieren
        let entry = GlobalValueEntry {
            key,
            local_id,
        };
        if let Some(cap) = self.value_partition_capacity {
            // Lazy wachsen: Vec erst bei Bedarf vergroessern
            if global_id >= self.global_id_to_entry.len() {
                self.global_id_to_entry.resize_with(global_id + 1, || None);
            }
            self.global_id_to_entry[global_id] = Some(entry);
            self.global_id = (self.global_id + 1) % cap;
        } else {
            self.global_id_to_entry.push(Some(entry));
            self.global_id += 1;
        }
        Ok(())
    }

    /// Evicted den Eintrag am gegebenen Global-Slot (bounded).
    fn evict_global_slot(&mut self, slot: usize) {
        if let Some(old_entry) = self.global_id_to_entry.get_mut(slot).and_then(|e| e.take()) {
            // Alten String holen für Lookup-Entfernung (nur wenn value_lookup aktiv)
            if !self.decode_only
                && let Some(old_string) = self.global_value.get_rc(slot)
            {
                self.value_lookup.remove(&old_string);
            }
            // Aus global entfernen
            self.global_value.remove(slot);
            // Aus local entfernen
            if let Some(local_store) = self.local_value.get_mut(&old_entry.key) {
                local_store.remove(old_entry.local_id);
            }
        }
    }

    // === Decoding Methods ===

    /// Decode URI Hit (Spec 7.3.2).
    ///
    /// `index_plus_one` ist der empfangene Wert (i+1). Gibt den String zurück
    /// oder Error::InvalidCompactId wenn ungültig.
    pub fn decode_uri_hit(&self, index_plus_one: usize) -> crate::Result<&str> {
        if index_plus_one == 0 {
            return Err(crate::Error::InvalidCompactId(0));
        }
        let id = index_plus_one - 1;
        self.uri.get(id).ok_or(crate::Error::InvalidCompactId(id))
    }

    /// Decode URI Hit mit Rc<str> (vermeidet to_string + into).
    pub fn decode_uri_hit_rc(&self, index_plus_one: usize) -> crate::Result<Rc<str>> {
        if index_plus_one == 0 {
            return Err(crate::Error::InvalidCompactId(0));
        }
        let id = index_plus_one - 1;
        self.uri.get_rc(id).ok_or(crate::Error::InvalidCompactId(id))
    }

    /// Decode URI Miss - fügt String hinzu und gibt ID zurück.
    pub fn decode_uri_miss(&mut self, uri: &str) -> usize {
        self.uri.add(uri)
    }

    /// Debug helper: returns URI partition entries in order.
    /// Debug: returns the current URI partition entries.
    pub fn debug_uri_entries(&self) -> Vec<String> {
        self.uri
            .entries
            .iter()
            .map(|s| s.as_ref().to_string())
            .collect()
    }

    /// Decode Prefix Hit (Spec 7.3.2).
    pub fn decode_prefix_hit(&self, uri_id: usize, index_plus_one: usize) -> crate::Result<&str> {
        if index_plus_one == 0 {
            return Err(crate::Error::InvalidCompactId(0));
        }
        let id = index_plus_one - 1;
        self.prefix
            .get(&uri_id)
            .and_then(|p| p.get(id))
            .ok_or(crate::Error::InvalidCompactId(id))
    }

    /// Decode Prefix Hit mit Rc<str>.
    pub fn decode_prefix_hit_rc(&self, uri_id: usize, index_plus_one: usize) -> crate::Result<Rc<str>> {
        if index_plus_one == 0 {
            return Err(crate::Error::InvalidCompactId(0));
        }
        let id = index_plus_one - 1;
        self.prefix
            .get(&uri_id)
            .and_then(|p| p.get_rc(id))
            .ok_or(crate::Error::InvalidCompactId(id))
    }

    /// Decode Prefix Miss - fügt String hinzu und gibt ID zurück.
    pub fn decode_prefix_miss(&mut self, uri_id: usize, prefix: &str) -> usize {
        self.prefix
            .entry(uri_id)
            .or_insert_with(Partition::new)
            .add(prefix)
    }

    /// Decode Local-Name Hit (Spec 7.3.3).
    pub fn decode_local_name_hit(&self, uri_id: usize, compact_id: usize) -> crate::Result<&str> {
        self.local_name
            .get(&uri_id)
            .and_then(|p| p.get(compact_id))
            .ok_or(crate::Error::InvalidCompactId(compact_id))
    }

    /// Decode Local-Name Hit mit Rc<str>.
    pub fn decode_local_name_hit_rc(&self, uri_id: usize, compact_id: usize) -> crate::Result<Rc<str>> {
        self.local_name
            .get(&uri_id)
            .and_then(|p| p.get_rc(compact_id))
            .ok_or(crate::Error::InvalidCompactId(compact_id))
    }

    /// Decode Local-Name Miss - fügt String hinzu und gibt ID zurück.
    pub fn decode_local_name_miss(&mut self, uri_id: usize, local_name: &str) -> usize {
        self.local_name
            .entry(uri_id)
            .or_insert_with(Partition::new)
            .add(local_name)
    }

    /// Decode Value Hit from Local Partition (Spec 7.3.3).
    pub fn decode_value_hit_local(&self, qname: &QName, compact_id: usize) -> crate::Result<&str> {
        self.value_key_ro(qname)
            .and_then(|key| self.local_value.get(&key))
            .and_then(|p| p.get(compact_id))
            .ok_or(crate::Error::InvalidCompactId(compact_id))
    }

    /// Decode Value Hit from Local Partition mit Rc<str>.
    pub fn decode_value_hit_local_rc(&self, qname: &QName, compact_id: usize) -> crate::Result<Rc<str>> {
        self.value_key_ro(qname)
            .and_then(|key| self.local_value.get(&key))
            .and_then(|p| p.get_rc(compact_id))
            .ok_or(crate::Error::InvalidCompactId(compact_id))
    }

    /// Decode Value Hit from Global Partition (Spec 7.3.3).
    pub fn decode_value_hit_global(&self, compact_id: usize) -> crate::Result<&str> {
        self.global_value
            .get(compact_id)
            .ok_or(crate::Error::InvalidCompactId(compact_id))
    }

    /// Decode Value Hit from Global Partition mit Rc<str>.
    pub fn decode_value_hit_global_rc(&self, compact_id: usize) -> crate::Result<Rc<str>> {
        self.global_value
            .get_rc(compact_id)
            .ok_or(crate::Error::InvalidCompactId(compact_id))
    }

    /// Decode Value Miss - fügt String hinzu (beachtet Options).
    ///
    /// Fügt zusätzlich in value_lookup ein (gleiche Logik wie Encode-Miss).
    pub fn decode_value_miss(&mut self, qname: &QName, value: &str) -> crate::Result<()> {
        self.add_value_with_options(qname, value)
    }
}

impl Default for StringTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Erstellt StringTable mit voller Pre-Population (preserve.prefixes=true).
    /// Verwendet für Tests die Table D-1 bis D-4 testen.
    fn new_fully_prepopulated() -> StringTable {
        StringTable::with_full_options(None, None, true)
    }

    // === Partition Tests ===

    /// Spec 7.3: Neue Partition ist leer
    #[test]
    fn partition_new_is_empty() {
        let p = Partition::new();
        assert_eq!(p.len(), 0);
    }

    /// Spec 7.3: Add gibt neuen Compact ID zurück
    #[test]
    fn partition_add_returns_id() {
        let mut p = Partition::new();
        assert_eq!(p.add("first"), 0);
        assert_eq!(p.add("second"), 1);
        assert_eq!(p.add("third"), 2);
        assert_eq!(p.len(), 3);
    }

    /// Spec 7.3: Add ist idempotent
    #[test]
    fn partition_add_idempotent() {
        let mut p = Partition::new();
        assert_eq!(p.add("value"), 0);
        assert_eq!(p.add("value"), 0); // gleicher Wert → gleicher ID
        assert_eq!(p.add("value"), 0);
        assert_eq!(p.len(), 1); // nur ein Eintrag
    }

    /// Spec 7.3: Lookup findet existierenden Wert
    #[test]
    fn partition_lookup_found() {
        let mut p = Partition::new();
        p.add("test");
        assert_eq!(p.lookup("test"), Some(0));
    }

    /// Spec 7.3: Lookup gibt None für nicht-existenten Wert
    #[test]
    fn partition_lookup_not_found() {
        let p = Partition::new();
        assert_eq!(p.lookup("missing"), None);
    }

    /// Spec 7.3: Get gibt String für gültigen ID zurück
    #[test]
    fn partition_get_valid_id() {
        let mut p = Partition::new();
        p.add("hello");
        p.add("world");
        assert_eq!(p.get(0), Some("hello"));
        assert_eq!(p.get(1), Some("world"));
    }

    /// Spec 7.3: Get gibt None für ungültigen ID
    #[test]
    fn partition_get_invalid_id() {
        let p = Partition::new();
        assert_eq!(p.get(0), None);
        assert_eq!(p.get(999), None);
    }

    /// Spec 7.3: Leerer String ist gültiger Wert
    #[test]
    fn partition_empty_string() {
        let mut p = Partition::new();
        assert_eq!(p.add(""), 0);
        assert_eq!(p.lookup(""), Some(0));
        assert_eq!(p.get(0), Some(""));
    }

    // === StringTable URI Partition Tests ===

    /// Table D-1: URI Partition hat 3 Pre-Populated Einträge (preserve.prefixes=true)
    #[test]
    fn uri_pre_population_count() {
        let st = new_fully_prepopulated();
        assert_eq!(st.uri_count(), 3);
    }

    /// Table D-1: URI ID 0 = "" (leere URI)
    #[test]
    fn uri_pre_population_empty() {
        let st = new_fully_prepopulated();
        assert_eq!(st.get_uri(0), Some(""));
        assert_eq!(st.lookup_uri(""), Some(0));
    }

    /// Table D-1: URI ID 1 = XML namespace (preserve.prefixes=true)
    #[test]
    fn uri_pre_population_xml() {
        let st = new_fully_prepopulated();
        assert_eq!(st.get_uri(1), Some("http://www.w3.org/XML/1998/namespace"));
        assert_eq!(
            st.lookup_uri("http://www.w3.org/XML/1998/namespace"),
            Some(1)
        );
    }

    /// Table D-1: URI ID 2 = XSI namespace (preserve.prefixes=true)
    #[test]
    fn uri_pre_population_xsi() {
        let st = new_fully_prepopulated();
        assert_eq!(
            st.get_uri(2),
            Some("http://www.w3.org/2001/XMLSchema-instance")
        );
        assert_eq!(
            st.lookup_uri("http://www.w3.org/2001/XMLSchema-instance"),
            Some(2)
        );
    }

    /// Spec 7.3: Lookup nicht-existente URI → None
    #[test]
    fn uri_lookup_not_found() {
        let st = new_fully_prepopulated();
        assert_eq!(st.lookup_uri("http://example.org"), None);
    }

    /// Spec 7.3: Add neue URI → nächster Compact ID (preserve.prefixes=true)
    #[test]
    fn uri_add_new() {
        let mut st = new_fully_prepopulated();
        assert_eq!(st.add_uri("http://example.org"), 3);
        assert_eq!(st.uri_count(), 4);
        assert_eq!(st.lookup_uri("http://example.org"), Some(3));
    }

    /// Spec 7.3: Add existierende URI → gleicher ID (idempotent, preserve.prefixes=true)
    #[test]
    fn uri_add_idempotent() {
        let mut st = new_fully_prepopulated();
        assert_eq!(st.add_uri("http://www.w3.org/XML/1998/namespace"), 1);
        assert_eq!(st.uri_count(), 3); // keine neuen Einträge
    }

    /// Spec 7.3: Get mit ungültigem ID → None
    #[test]
    fn uri_get_invalid_id() {
        let st = StringTable::new();
        assert_eq!(st.get_uri(999), None);
    }

    // === StringTable Prefix Partition Tests ===

    /// Table D-3: Prefix Partition für "" URI hat "" prefix
    #[test]
    fn prefix_pre_population_empty() {
        let st = new_fully_prepopulated();
        assert_eq!(st.prefix_count(0), 1);
        assert_eq!(st.get_prefix(0, 0), Some(""));
        assert_eq!(st.lookup_prefix(0, ""), Some(0));
    }

    /// Table D-3: Prefix Partition für XML URI hat "xml" prefix (preserve.prefixes=true)
    #[test]
    fn prefix_pre_population_xml() {
        let st = new_fully_prepopulated();
        assert_eq!(st.prefix_count(1), 1);
        assert_eq!(st.get_prefix(1, 0), Some("xml"));
        assert_eq!(st.lookup_prefix(1, "xml"), Some(0));
    }

    /// Table D-3: Prefix Partition für XSI URI hat "xsi" prefix (preserve.prefixes=true)
    #[test]
    fn prefix_pre_population_xsi() {
        let st = new_fully_prepopulated();
        assert_eq!(st.prefix_count(2), 1);
        assert_eq!(st.get_prefix(2, 0), Some("xsi"));
        assert_eq!(st.lookup_prefix(2, "xsi"), Some(0));
    }

    /// Spec 7.3: Lookup Prefix in nicht-existenter Partition → None
    #[test]
    fn prefix_lookup_nonexistent_partition() {
        let st = new_fully_prepopulated();
        assert_eq!(st.lookup_prefix(999, "foo"), None);
    }

    /// Spec 7.3: prefix_count für nicht-existente Partition → 0
    #[test]
    fn prefix_count_nonexistent_partition() {
        let st = new_fully_prepopulated();
        assert_eq!(st.prefix_count(999), 0);
    }

    /// Spec 7.3: Add Prefix in existierende Partition (preserve.prefixes=true)
    #[test]
    fn prefix_add_to_existing_partition() {
        let mut st = new_fully_prepopulated();
        assert_eq!(st.add_prefix(1, "x"), 1); // XML Partition, neuer Prefix
        assert_eq!(st.prefix_count(1), 2);
        assert_eq!(st.lookup_prefix(1, "x"), Some(1));
    }

    /// Spec 7.3: Add Prefix erstellt neue Partition (preserve.prefixes=true)
    #[test]
    fn prefix_add_creates_partition() {
        let mut st = new_fully_prepopulated();
        let new_uri_id = st.add_uri("http://example.org");
        assert_eq!(st.prefix_count(new_uri_id), 0);
        assert_eq!(st.add_prefix(new_uri_id, "ex"), 0);
        assert_eq!(st.prefix_count(new_uri_id), 1);
    }

    /// Spec 7.3: Add Prefix ist idempotent (preserve.prefixes=true)
    #[test]
    fn prefix_add_idempotent() {
        let mut st = new_fully_prepopulated();
        assert_eq!(st.add_prefix(1, "xml"), 0); // schon vorhanden
        assert_eq!(st.prefix_count(1), 1);
    }

    /// Spec 7.3: Get Prefix mit ungültigem ID → None
    #[test]
    fn prefix_get_invalid_id() {
        let st = StringTable::new();
        assert_eq!(st.get_prefix(1, 999), None);
    }

    /// Spec 7.3: Partition-Isolation - gleicher Prefix in verschiedenen Partitionen
    #[test]
    fn prefix_partition_isolation() {
        let mut st = StringTable::new();
        let uri1 = st.add_uri("http://a.org");
        let uri2 = st.add_uri("http://b.org");
        assert_eq!(st.add_prefix(uri1, "p"), 0);
        assert_eq!(st.add_prefix(uri2, "p"), 0); // gleicher Prefix, verschiedene Partitionen
        assert_eq!(st.lookup_prefix(uri1, "p"), Some(0));
        assert_eq!(st.lookup_prefix(uri2, "p"), Some(0));
    }

    // === StringTable Local-Name Partition Tests ===

    /// Table D-4: XML-NS hat 4 Local-Names (sortiert)
    #[test]
    fn local_name_pre_population_xml() {
        let st = new_fully_prepopulated();
        assert_eq!(st.local_name_count(1), 4);
        assert_eq!(st.get_local_name(1, 0), Some("base"));
        assert_eq!(st.get_local_name(1, 1), Some("id"));
        assert_eq!(st.get_local_name(1, 2), Some("lang"));
        assert_eq!(st.get_local_name(1, 3), Some("space"));
    }

    /// Table D-4: XML-NS Lookup (preserve.prefixes=true)
    #[test]
    fn local_name_lookup_xml() {
        let st = new_fully_prepopulated();
        assert_eq!(st.lookup_local_name(1, "base"), Some(0));
        assert_eq!(st.lookup_local_name(1, "id"), Some(1));
        assert_eq!(st.lookup_local_name(1, "lang"), Some(2));
        assert_eq!(st.lookup_local_name(1, "space"), Some(3));
    }

    /// Table D-4: XSI-NS hat 2 Local-Names (sortiert, preserve.prefixes=true)
    #[test]
    fn local_name_pre_population_xsi() {
        let st = new_fully_prepopulated();
        assert_eq!(st.local_name_count(2), 2);
        assert_eq!(st.get_local_name(2, 0), Some("nil"));
        assert_eq!(st.get_local_name(2, 1), Some("type"));
    }

    /// Table D-4: XSI-NS Lookup (preserve.prefixes=true)
    #[test]
    fn local_name_lookup_xsi() {
        let st = new_fully_prepopulated();
        assert_eq!(st.lookup_local_name(2, "nil"), Some(0));
        assert_eq!(st.lookup_local_name(2, "type"), Some(1));
    }

    /// Spec 7.3: Leere URI hat keine Pre-Populated Local-Names
    #[test]
    fn local_name_empty_uri_no_pre_population() {
        let st = new_fully_prepopulated();
        assert_eq!(st.local_name_count(0), 0);
    }

    /// Spec 7.3: Lookup in nicht-existenter Partition → None
    #[test]
    fn local_name_lookup_nonexistent_partition() {
        let st = new_fully_prepopulated();
        assert_eq!(st.lookup_local_name(999, "foo"), None);
    }

    /// Spec 7.3: local_name_count für nicht-existente Partition → 0
    #[test]
    fn local_name_count_nonexistent_partition() {
        let st = new_fully_prepopulated();
        assert_eq!(st.local_name_count(999), 0);
    }

    /// Spec 7.3: Add Local-Name in existierende Partition (preserve.prefixes=true)
    #[test]
    fn local_name_add_to_existing_partition() {
        let mut st = new_fully_prepopulated();
        assert_eq!(st.add_local_name(1, "newattr"), 4); // nach "space"
        assert_eq!(st.local_name_count(1), 5);
        assert_eq!(st.lookup_local_name(1, "newattr"), Some(4));
    }

    /// Spec 7.3: Add Local-Name erstellt neue Partition
    #[test]
    fn local_name_add_creates_partition() {
        let mut st = new_fully_prepopulated();
        assert_eq!(st.local_name_count(0), 0); // leere URI hat keine
        assert_eq!(st.add_local_name(0, "elem"), 0);
        assert_eq!(st.local_name_count(0), 1);
    }

    /// Spec 7.3: Add Local-Name ist idempotent (preserve.prefixes=true)
    #[test]
    fn local_name_add_idempotent() {
        let mut st = new_fully_prepopulated();
        assert_eq!(st.add_local_name(1, "base"), 0); // schon vorhanden
        assert_eq!(st.local_name_count(1), 4);
    }

    /// Spec 7.3: Get Local-Name mit ungültigem ID → None
    #[test]
    fn local_name_get_invalid_id() {
        let st = StringTable::new();
        assert_eq!(st.get_local_name(1, 999), None);
    }

    /// Spec 7.3: Get Local-Name in nicht-existenter Partition → None
    #[test]
    fn local_name_get_nonexistent_partition() {
        let st = StringTable::new();
        assert_eq!(st.get_local_name(999, 0), None);
    }

    /// Spec 7.3: Partition-Isolation - gleicher Local-Name in verschiedenen Partitionen
    #[test]
    fn local_name_partition_isolation() {
        let mut st = StringTable::new();
        let uri1 = st.add_uri("http://a.org");
        let uri2 = st.add_uri("http://b.org");
        assert_eq!(st.add_local_name(uri1, "elem"), 0);
        assert_eq!(st.add_local_name(uri2, "elem"), 0);
        assert_eq!(st.lookup_local_name(uri1, "elem"), Some(0));
        assert_eq!(st.lookup_local_name(uri2, "elem"), Some(0));
    }

    // === StringTable Value Partition Tests ===

    /// Spec 7.3: Value Partitions sind anfangs leer
    #[test]
    fn value_partitions_initially_empty() {
        let st = StringTable::new();
        assert_eq!(st.global_value_count(), 0);
    }

    /// Spec 7.3.3: add_value fügt in Global und Local ein
    #[test]
    fn value_add_global_and_local() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        let (global_id, local_id) = st.add_value(&qname, "hello");
        assert_eq!(global_id, 0);
        assert_eq!(local_id, 0);
        assert_eq!(st.global_value_count(), 1);
        assert_eq!(st.local_value_count(&qname), 1);
    }

    /// Spec 7.3.3: add_value ist idempotent (gleicher Wert → gleiche IDs)
    #[test]
    fn value_add_idempotent() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        let (g1, l1) = st.add_value(&qname, "hello");
        let (g2, l2) = st.add_value(&qname, "hello");
        assert_eq!((g1, l1), (g2, l2));
        assert_eq!(st.global_value_count(), 1);
        assert_eq!(st.local_value_count(&qname), 1);
    }

    /// Spec 7.3: Lookup Global Value
    #[test]
    fn value_lookup_global() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        st.add_value(&qname, "test");
        assert_eq!(st.lookup_global_value("test"), Some(0));
        assert_eq!(st.lookup_global_value("missing"), None);
    }

    /// Spec 7.3: Lookup Local Value
    #[test]
    fn value_lookup_local() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        st.add_value(&qname, "test");
        assert_eq!(st.lookup_local_value(&qname, "test"), Some(0));
        assert_eq!(st.lookup_local_value(&qname, "missing"), None);
    }

    /// Spec 7.3: Get Global Value
    #[test]
    fn value_get_global() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        st.add_value(&qname, "hello");
        assert_eq!(st.get_global_value(0), Some("hello"));
        assert_eq!(st.get_global_value(999), None);
    }

    /// Spec 7.3: Get Local Value
    #[test]
    fn value_get_local() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        st.add_value(&qname, "hello");
        assert_eq!(st.get_local_value(&qname, 0), Some("hello"));
        assert_eq!(st.get_local_value(&qname, 999), None);
    }

    /// Spec 7.3: Get Local Value für unbekannten QName → None
    #[test]
    fn value_get_local_unknown_qname() {
        let st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        assert_eq!(st.get_local_value(&qname, 0), None);
    }

    /// Spec 7.3: local_value_count für unbekannten QName → 0
    #[test]
    fn value_local_count_unknown_qname() {
        let st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        assert_eq!(st.local_value_count(&qname), 0);
    }

    /// Spec 7.3: Lookup Local Value für unbekannten QName → None
    #[test]
    fn value_lookup_local_unknown_qname() {
        let st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        assert_eq!(st.lookup_local_value(&qname, "test"), None);
    }

    /// Spec 7.3: Partition-Isolation - gleicher Value in verschiedenen QName-Partitionen
    #[test]
    fn value_partition_isolation() {
        let mut st = StringTable::new();
        let q1 = QName::new("http://example.org", "elem1");
        let q2 = QName::new("http://example.org", "elem2");

        let (g1, l1) = st.add_value(&q1, "shared");
        let (g2, l2) = st.add_value(&q2, "shared");

        // Global: gleicher Wert → gleicher ID (idempotent)
        assert_eq!(g1, g2);
        assert_eq!(g1, 0);

        // Local: verschiedene Partitionen → beide ID 0
        assert_eq!(l1, 0);
        assert_eq!(l2, 0);

        // Aber verschiedene Partitionen
        assert_eq!(st.local_value_count(&q1), 1);
        assert_eq!(st.local_value_count(&q2), 1);
    }

    /// Spec 7.3: Mehrere Werte in Global und Local
    #[test]
    fn value_multiple_values() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");

        let (g1, l1) = st.add_value(&qname, "first");
        let (g2, l2) = st.add_value(&qname, "second");
        let (g3, l3) = st.add_value(&qname, "third");

        assert_eq!((g1, l1), (0, 0));
        assert_eq!((g2, l2), (1, 1));
        assert_eq!((g3, l3), (2, 2));

        assert_eq!(st.global_value_count(), 3);
        assert_eq!(st.local_value_count(&qname), 3);
    }

    /// Spec 7.3: Leerer String als Value
    #[test]
    fn value_empty_string() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        let (g, l) = st.add_value(&qname, "");
        assert_eq!((g, l), (0, 0));
        assert_eq!(st.lookup_global_value(""), Some(0));
        assert_eq!(st.lookup_local_value(&qname, ""), Some(0));
    }

    /// Default Impl delegiert an new() (Appendix D.1: immer 3 URIs)
    #[test]
    fn default_impl() {
        let st = StringTable::default();
        assert_eq!(st.uri_count(), 3);
    }

    /// Spec 7.1.7: QName Equality ignoriert Prefix → gleiche Local Value Partition
    #[test]
    fn value_qname_equality_ignores_prefix() {
        let mut st = StringTable::new();
        // Zwei QNames mit gleicher URI + local_name aber verschiedenem Prefix
        let q1 = QName::new("http://example.org", "elem");
        let q2 = QName::with_prefix("http://example.org", "elem", "ex");

        // Add über q1
        let (g1, l1) = st.add_value(&q1, "value");
        assert_eq!((g1, l1), (0, 0));

        // Lookup über q2 (mit Prefix) sollte gleichen Wert finden
        assert_eq!(st.lookup_local_value(&q2, "value"), Some(0));
        assert_eq!(st.get_local_value(&q2, 0), Some("value"));

        // Add über q2 sollte idempotent sein (gleiche Partition)
        let (g2, l2) = st.add_value(&q2, "value");
        assert_eq!((g2, l2), (0, 0));
        assert_eq!(st.local_value_count(&q1), 1);
        assert_eq!(st.local_value_count(&q2), 1);
    }

    // === ValueStore Tests ===

    /// Spec 7.3.3: Neuer ValueStore ist leer
    #[test]
    fn value_store_new_is_empty() {
        let vs = ValueStore::new();
        assert_eq!(vs.len(), 0);
        assert_eq!(vs.size_for_bits(), 0);
        assert!(!vs.at_capacity);
    }

    /// Spec 7.3.3: Bounded Store mit Kapazität (lazy Allokation)
    #[test]
    fn value_store_with_capacity() {
        let vs = ValueStore::with_capacity(5);
        assert_eq!(vs.len(), 0);
        assert_eq!(vs.entries.len(), 0); // Lazy: nicht vorab alloziert
        assert_eq!(vs.capacity, Some(5));
        assert!(!vs.at_capacity);
    }

    /// Spec 7.3.3: Add in unbounded Store
    #[test]
    fn value_store_add_unbounded() {
        let mut vs = ValueStore::new();
        assert_eq!(vs.add("first".into()), 0);
        assert_eq!(vs.add("second".into()), 1);
        assert_eq!(vs.add("third".into()), 2);
        assert_eq!(vs.len(), 3);
        assert_eq!(vs.size_for_bits(), 3);
    }

    /// Spec 7.3.3: Get by ID
    #[test]
    fn value_store_get() {
        let mut vs = ValueStore::new();
        vs.add("hello".into());
        assert_eq!(vs.get(0), Some("hello"));
        assert_eq!(vs.get(999), None);
    }

    /// Spec 7.3.3: Remove setzt Entry auf None
    #[test]
    fn value_store_remove() {
        let mut vs = ValueStore::new();
        vs.add("value".into());
        assert_eq!(vs.get(0), Some("value"));

        vs.remove(0);
        assert_eq!(vs.get(0), None); // Hole
        assert_eq!(vs.len(), 0);
        assert_eq!(vs.size_for_bits(), 1); // next_id bleibt
    }

    /// Spec 7.3.3: Set für bounded Store
    #[test]
    fn value_store_set() {
        let mut vs = ValueStore::with_capacity(3);
        vs.set(0, "first".into());
        vs.set(1, "second".into());
        assert_eq!(vs.get(0), Some("first"));
        assert_eq!(vs.get(1), Some("second"));
    }

    /// Spec 7.3.3: size_for_bits vor Wrap = next_id
    #[test]
    fn value_store_size_for_bits_before_wrap() {
        let mut vs = ValueStore::with_capacity(5);
        vs.set(0, "a".into());
        assert_eq!(vs.size_for_bits(), 1);
        vs.set(1, "b".into());
        assert_eq!(vs.size_for_bits(), 2);
        assert!(!vs.at_capacity);
    }

    /// Spec 7.3.3: size_for_bits nach Wrap = capacity
    #[test]
    fn value_store_size_for_bits_after_wrap() {
        let mut vs = ValueStore::with_capacity(3);
        vs.set(0, "a".into());
        vs.set(1, "b".into());
        vs.set(2, "c".into()); // Triggert at_capacity
        assert!(vs.at_capacity);
        assert_eq!(vs.size_for_bits(), 3); // capacity, nicht next_id
    }

    /// Spec 7.3.3: Unbounded size_for_bits wächst monoton
    #[test]
    fn value_store_unbounded_size_grows() {
        let mut vs = ValueStore::new();
        vs.add("a".into());
        vs.add("b".into());
        vs.add("c".into());
        assert_eq!(vs.size_for_bits(), 3);
        vs.remove(1); // Hole
        assert_eq!(vs.size_for_bits(), 3); // bleibt bei next_id
    }

    // === Encoding Tests (7.3.2, 7.3.3) ===

    /// Spec 7.3.2: encode_uri Hit (preserve.prefixes=true)
    #[test]
    fn encode_uri_hit() {
        let mut st = new_fully_prepopulated();
        // Pre-populated URI
        let (result, size) = st.encode_uri("");
        assert_eq!(result, CompactIdResult::Hit(0));
        assert_eq!(size, 3);
    }

    /// Spec 7.3.2: encode_uri Miss (preserve.prefixes=true)
    #[test]
    fn encode_uri_miss() {
        let mut st = new_fully_prepopulated();
        let (result, size) = st.encode_uri("http://example.org");
        assert_eq!(result, CompactIdResult::Miss(3));
        assert_eq!(size, 3); // size vor Add
        assert_eq!(st.uri_count(), 4);
    }

    /// Spec 7.3.2: encode_prefix Hit (preserve.prefixes=true)
    #[test]
    fn encode_prefix_hit() {
        let mut st = new_fully_prepopulated();
        let (result, size) = st.encode_prefix(URI_ID_XML, "xml");
        assert_eq!(result, CompactIdResult::Hit(0));
        assert_eq!(size, 1);
    }

    /// Spec 7.3.2: encode_prefix Miss (preserve.prefixes=true)
    #[test]
    fn encode_prefix_miss() {
        let mut st = new_fully_prepopulated();
        let (result, size) = st.encode_prefix(URI_ID_XML, "x");
        assert_eq!(result, CompactIdResult::Miss(1));
        assert_eq!(size, 1);
        assert_eq!(st.prefix_count(URI_ID_XML), 2);
    }

    /// Spec 7.3.3: encode_local_name Hit (preserve.prefixes=true)
    #[test]
    fn encode_local_name_hit() {
        let mut st = new_fully_prepopulated();
        let (result, size) = st.encode_local_name(URI_ID_XML, "base");
        assert_eq!(result, StringLiteralResult::Hit(0));
        assert_eq!(size, 4);
    }

    /// Spec 7.3.3: encode_local_name Miss (preserve.prefixes=true)
    #[test]
    fn encode_local_name_miss() {
        let mut st = new_fully_prepopulated();
        let (result, size) = st.encode_local_name(URI_ID_XML, "newattr");
        assert_eq!(result, StringLiteralResult::Miss);
        assert_eq!(size, 4);
        assert_eq!(st.local_name_count(URI_ID_XML), 5);
    }

    /// Spec 7.3.3: encode_value Miss (unbounded)
    #[test]
    fn encode_value_miss_unbounded() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        let (result, g_size, l_size) = st.encode_value(&qname, "hello").unwrap();
        assert_eq!(result, ValueResult::Miss);
        assert_eq!(g_size, 0);
        assert_eq!(l_size, 0);
        // Wurde hinzugefügt
        assert_eq!(st.global_value_count(), 1);
        assert_eq!(st.local_value_count(&qname), 1);
    }

    /// Spec 7.3.3: encode_value HitLocal
    #[test]
    fn encode_value_hit_local() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        st.add_value(&qname, "hello");
        let (result, g_size, l_size) = st.encode_value(&qname, "hello").unwrap();
        assert_eq!(result, ValueResult::HitLocal(0));
        assert_eq!(g_size, 1);
        assert_eq!(l_size, 1);
    }

    /// Spec 7.3.3: encode_value HitGlobal (in anderem QName)
    #[test]
    fn encode_value_hit_global() {
        let mut st = StringTable::new();
        let q1 = QName::new("http://example.org", "elem1");
        let q2 = QName::new("http://example.org", "elem2");
        st.add_value(&q1, "shared");
        let (result, g_size, l_size) = st.encode_value(&q2, "shared").unwrap();
        assert_eq!(result, ValueResult::HitGlobal(0));
        assert_eq!(g_size, 1);
        assert_eq!(l_size, 0); // q2 hat noch keine lokale Partition
    }

    // === Decoding Tests ===

    /// Spec 7.3.2: decode_uri_hit valid (preserve.prefixes=true)
    #[test]
    fn decode_uri_hit_valid() {
        let st = new_fully_prepopulated();
        assert_eq!(st.decode_uri_hit(1), Ok(""));
        assert_eq!(st.decode_uri_hit(2), Ok(URI_XML));
        assert_eq!(st.decode_uri_hit(3), Ok(URI_XSI));
    }

    /// Spec 7.3.2: decode_uri_hit invalid (preserve.prefixes=true)
    #[test]
    fn decode_uri_hit_invalid() {
        let st = new_fully_prepopulated();
        assert!(st.decode_uri_hit(0).is_err()); // 0 ist Miss-Marker
        assert!(st.decode_uri_hit(999).is_err());
    }

    /// Spec 7.3.2: decode_uri_miss (preserve.prefixes=true)
    #[test]
    fn decode_uri_miss_adds() {
        let mut st = new_fully_prepopulated();
        let id = st.decode_uri_miss("http://new.org");
        assert_eq!(id, 3);
        assert_eq!(st.get_uri(3), Some("http://new.org"));
    }

    /// Spec 7.3.3: decode_local_name_hit valid (preserve.prefixes=true)
    #[test]
    fn decode_local_name_hit_valid() {
        let st = new_fully_prepopulated();
        assert_eq!(st.decode_local_name_hit(URI_ID_XML, 0), Ok("base"));
        assert_eq!(st.decode_local_name_hit(URI_ID_XML, 3), Ok("space"));
    }

    /// Spec 7.3.3: decode_local_name_hit invalid (preserve.prefixes=true)
    #[test]
    fn decode_local_name_hit_invalid() {
        let st = new_fully_prepopulated();
        assert!(st.decode_local_name_hit(URI_ID_XML, 999).is_err());
        assert!(st.decode_local_name_hit(999, 0).is_err());
    }

    /// Spec 7.3.3: decode_value_hit_global valid
    #[test]
    fn decode_value_hit_global_valid() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        st.add_value(&qname, "test");
        assert_eq!(st.decode_value_hit_global(0), Ok("test"));
    }

    /// Spec 7.3.3: decode_value_hit_global invalid
    #[test]
    fn decode_value_hit_global_invalid() {
        let st = StringTable::new();
        assert!(st.decode_value_hit_global(0).is_err());
    }

    /// Spec 7.3.3: decode_value_hit_local valid
    #[test]
    fn decode_value_hit_local_valid() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        st.add_value(&qname, "test");
        assert_eq!(st.decode_value_hit_local(&qname, 0), Ok("test"));
    }

    // === valueMaxLength Tests ===

    /// Spec 7.3.3: Value <= maxLength wird hinzugefügt
    #[test]
    fn value_max_length_within_limit() {
        let mut st = StringTable::with_options(Some(5), None);
        let qname = QName::new("http://example.org", "elem");
        st.encode_value(&qname, "hello").unwrap(); // 5 Zeichen
        assert_eq!(st.global_value_count(), 1);
    }

    /// Spec 7.3.3: Value > maxLength wird NICHT hinzugefügt
    #[test]
    fn value_max_length_exceeds_limit() {
        let mut st = StringTable::with_options(Some(5), None);
        let qname = QName::new("http://example.org", "elem");
        st.encode_value(&qname, "toolong").unwrap(); // 7 Zeichen
        assert_eq!(st.global_value_count(), 0);
    }

    /// Spec 7.3.3: Unicode Codepoints, nicht Bytes
    #[test]
    fn value_max_length_unicode_codepoints() {
        let mut st = StringTable::with_options(Some(3), None);
        let qname = QName::new("http://example.org", "elem");
        // "äöü" = 3 Codepoints (aber 6 UTF-8 Bytes)
        st.encode_value(&qname, "äöü").unwrap();
        assert_eq!(st.global_value_count(), 1);
    }

    /// Spec 7.3.3: Leerer Value wird nie hinzugefügt
    #[test]
    fn value_empty_not_added() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        st.encode_value(&qname, "").unwrap();
        assert_eq!(st.global_value_count(), 0);
    }

    // === valuePartitionCapacity Tests ===

    /// Spec 7.3.3: Capacity 0 - keine Values werden hinzugefügt
    #[test]
    fn value_capacity_zero() {
        let mut st = StringTable::with_options(None, Some(0));
        let qname = QName::new("http://example.org", "elem");
        st.encode_value(&qname, "hello").unwrap();
        assert_eq!(st.global_value_count(), 0);
    }

    /// Spec 7.3.3: Wrap-Around bei Capacity
    #[test]
    fn value_capacity_wrap_around() {
        let mut st = StringTable::with_options(None, Some(3));
        let qname = QName::new("http://example.org", "elem");

        st.encode_value(&qname, "a").unwrap();
        st.encode_value(&qname, "b").unwrap();
        st.encode_value(&qname, "c").unwrap();
        assert_eq!(st.global_id, 0); // Wrapped

        // Vierter Wert überschreibt ersten
        st.encode_value(&qname, "d").unwrap();
        assert_eq!(st.global_id, 1);

        // "a" wurde evicted
        assert!(st.decode_value_hit_global(0).is_ok()); // "d" ist jetzt bei 0
        assert_eq!(st.decode_value_hit_global(0), Ok("d"));
    }

    /// Spec 7.3.3: Eviction entfernt aus global UND local
    #[test]
    fn eviction_removes_from_both() {
        let mut st = StringTable::with_options(None, Some(2));
        let q1 = QName::new("http://example.org", "elem1");
        let q2 = QName::new("http://example.org", "elem2");

        st.encode_value(&q1, "first").unwrap(); // global 0, local q1: 0
        st.encode_value(&q2, "second").unwrap(); // global 1, local q2: 0
        st.encode_value(&q1, "third").unwrap(); // global 0 (wrap), evicts "first"

        // "first" nicht mehr auffindbar
        assert_eq!(st.lookup_global_value("first"), None);
        assert_eq!(st.lookup_local_value(&q1, "first"), None);

        // "third" ist bei global 0
        assert_eq!(st.lookup_global_value("third"), Some(0));
    }

    /// Spec 7.3.3: Eviction + Reinsert — evicted Value wird erneut als Miss eingefügt
    #[test]
    fn eviction_reinsert_same_value() {
        let mut st = StringTable::with_options(None, Some(2));
        let qname = QName::new("http://example.org", "elem");

        st.encode_value(&qname, "alpha").unwrap(); // global 0
        st.encode_value(&qname, "beta").unwrap();  // global 1
        // "alpha" wird evicted:
        st.encode_value(&qname, "gamma").unwrap(); // global 0 (wrap), evicts "alpha"

        assert_eq!(st.lookup_global_value("alpha"), None);

        // "alpha" erneut als Miss einfügen
        let (result, _, _) = st.encode_value(&qname, "alpha").unwrap();
        assert_eq!(result, ValueResult::Miss); // nicht Hit!

        // "alpha" ist jetzt bei global 1 (evicts "beta")
        assert_eq!(st.lookup_global_value("alpha"), Some(1));
        assert_eq!(st.lookup_global_value("beta"), None);
    }

    /// Spec 7.3.3: Partition-Isolation via encode_value —
    /// gleicher Value für verschiedene QNames: erster = Local, zweiter = Global
    #[test]
    fn encode_value_partition_isolation() {
        let mut st = StringTable::new();
        let q1 = QName::new("http://example.org", "elem1");
        let q2 = QName::new("http://example.org", "elem2");

        // Erster Miss fügt "shared" zu q1-Local und Global hinzu
        let (r1, _, _) = st.encode_value(&q1, "shared").unwrap();
        assert_eq!(r1, ValueResult::Miss);

        // Zweiter Aufruf für q1 → HitLocal
        let (r2, _, _) = st.encode_value(&q1, "shared").unwrap();
        assert_eq!(r2, ValueResult::HitLocal(0));

        // Gleicher Value für q2 → HitGlobal (nicht in q2-Local)
        let (r3, _, _) = st.encode_value(&q2, "shared").unwrap();
        assert_eq!(r3, ValueResult::HitGlobal(0));
    }

    /// Spec 7.3.3: size_for_bits nach Wrap = capacity
    #[test]
    fn value_size_for_bits_after_wrap() {
        let mut st = StringTable::with_options(None, Some(3));
        let qname = QName::new("http://example.org", "elem");

        st.encode_value(&qname, "a").unwrap();
        assert_eq!(st.global_value.size_for_bits(), 1);

        st.encode_value(&qname, "b").unwrap();
        st.encode_value(&qname, "c").unwrap(); // Triggert wrap
        assert_eq!(st.global_value.size_for_bits(), 3); // capacity
    }

    // === Round-Trip Tests ===

    /// Round-Trip: URI encode → decode (preserve.prefixes=true)
    #[test]
    fn roundtrip_uri() {
        let mut st = new_fully_prepopulated();

        // Miss
        let (result, _) = st.encode_uri("http://new.org");
        assert!(matches!(result, CompactIdResult::Miss(3)));
        assert_eq!(st.decode_uri_hit(4), Ok("http://new.org")); // id+1

        // Hit
        let (result, _) = st.encode_uri("http://new.org");
        assert!(matches!(result, CompactIdResult::Hit(3)));
    }

    /// Round-Trip: Local-Name encode → decode
    #[test]
    fn roundtrip_local_name() {
        let mut st = StringTable::new();

        // Miss
        let (result, size) = st.encode_local_name(0, "newelem");
        assert!(matches!(result, StringLiteralResult::Miss));
        assert_eq!(size, 0); // leere URI hat initial keine local-names

        // Decode
        assert_eq!(st.decode_local_name_hit(0, 0), Ok("newelem"));
    }

    /// Round-Trip: Value encode → decode
    #[test]
    fn roundtrip_value() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");

        // Miss
        let (result, _, _) = st.encode_value(&qname, "testvalue").unwrap();
        assert!(matches!(result, ValueResult::Miss));

        // Decode global
        assert_eq!(st.decode_value_hit_global(0), Ok("testvalue"));

        // Decode local
        assert_eq!(st.decode_value_hit_local(&qname, 0), Ok("testvalue"));

        // Hit local
        let (result, _, _) = st.encode_value(&qname, "testvalue").unwrap();
        assert!(matches!(result, ValueResult::HitLocal(0)));
    }

    // === Bitbreiten Edge-Cases ===

    /// Spec 7.3.2: m=0 → n=0, nur Miss möglich
    #[test]
    fn encode_prefix_m_zero() {
        let mut st = StringTable::new();
        // Neue URI ohne pre-populated prefix partition
        let uri_id = st.add_uri("http://new.org");
        let (result, size) = st.encode_prefix(uri_id, "p");
        assert_eq!(result, CompactIdResult::Miss(0));
        assert_eq!(size, 0); // m=0 vor Add
    }

    /// Spec 7.3.3: m=0 → n=0, nur Miss möglich (local-name)
    #[test]
    fn encode_local_name_m_zero() {
        let mut st = StringTable::new();
        // Leere URI hat keine pre-populated local-names
        let (result, size) = st.encode_local_name(URI_ID_EMPTY, "elem");
        assert_eq!(result, StringLiteralResult::Miss);
        assert_eq!(size, 0); // m=0 vor Add
    }

    /// Spec 7.3.3: m=1 → n=0, Hit mit 0 Bits (ID implizit 0)
    #[test]
    fn encode_local_name_m_one() {
        let mut st = StringTable::new();
        // Füge einen Eintrag hinzu
        st.add_local_name(URI_ID_EMPTY, "first");
        // Jetzt m=1
        let (result, size) = st.encode_local_name(URI_ID_EMPTY, "first");
        assert_eq!(result, StringLiteralResult::Hit(0));
        assert_eq!(size, 1); // m=1 → n=0 Bits (ID 0 implizit)
    }

    /// Spec 7.3.3: m=1 Value → n=0, Hit mit 0 Bits
    #[test]
    fn encode_value_m_one() {
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");
        st.add_value(&qname, "only");
        // global_size=1, local_size=1
        let (result, g_size, l_size) = st.encode_value(&qname, "only").unwrap();
        assert_eq!(result, ValueResult::HitLocal(0));
        assert_eq!(g_size, 1); // m=1 → n=0
        assert_eq!(l_size, 1); // m=1 → n=0
    }

    // === API Contract Tests ===

    /// add_value() ist nur für unbounded StringTables sicher.
    /// Bei bounded Tables encode_value() verwenden.
    #[test]
    fn add_value_safe_for_unbounded() {
        // Unbounded: add_value funktioniert korrekt
        let mut st = StringTable::new();
        let qname = QName::new("http://example.org", "elem");

        for i in 0..100 {
            let (g, l) = st.add_value(&qname, &format!("value{i}"));
            assert_eq!(g, i);
            assert_eq!(l, i);
        }
        assert_eq!(st.global_value_count(), 100);
    }

    /// Bei bounded StringTables encode_value() für korrekte Eviction nutzen.
    #[test]
    fn bounded_use_encode_value() {
        let mut st = StringTable::with_options(None, Some(3));
        let qname = QName::new("http://example.org", "elem");

        // encode_value handhabt Eviction korrekt
        st.encode_value(&qname, "a").unwrap();
        st.encode_value(&qname, "b").unwrap();
        st.encode_value(&qname, "c").unwrap();
        st.encode_value(&qname, "d").unwrap(); // evicts "a"

        assert_eq!(st.lookup_global_value("a"), None); // evicted
        assert_eq!(st.lookup_global_value("d"), Some(0)); // replaced "a"
    }

    // ==========================================================================
    // Schema-informed Pre-Population Tests (Spec 7.3.1, Appendix D)
    // ==========================================================================

    /// Table D-2: XSD-NS wird als ID 3 hinzugefügt bei schema-informed
    #[test]
    fn schema_informed_uri_xsd_ns_at_id_3() {
        let schema = crate::schema::SchemaInfo::builder().build();
        let st = StringTable::from_schema(&schema);

        assert_eq!(st.uri_count(), 4);
        assert_eq!(st.get_uri(3), Some(URI_XSD));
        assert_eq!(st.lookup_uri(URI_XSD), Some(3));
    }

    /// Spec 7.3.1: Target-Namespaces werden zur URI-Partition hinzugefügt
    #[test]
    fn schema_informed_uri_target_namespaces() {
        let schema = crate::schema::SchemaInfo::builder()
            .global_element(QName::new("http://example.org/b", "elem1"))
            .global_element(QName::new("http://example.org/a", "elem2"))
            .build();
        let st = StringTable::from_schema(&schema);

        // IDs 0-3 sind fixed, danach Target-Namespaces sortiert
        assert!(st.lookup_uri("http://example.org/a").is_some());
        assert!(st.lookup_uri("http://example.org/b").is_some());

        // Sortiert: a < b
        let a_id = st.lookup_uri("http://example.org/a").unwrap();
        let b_id = st.lookup_uri("http://example.org/b").unwrap();
        assert!(a_id < b_id);
    }

    /// Spec 7.3.1: URI-Partition ist unique (keine Duplikate)
    #[test]
    fn schema_informed_uri_unique() {
        let schema = crate::schema::SchemaInfo::builder()
            .global_element(QName::new("http://example.org", "elem1"))
            .global_element(QName::new("http://example.org", "elem2"))
            .all_element(QName::new("http://example.org", "local"))
            .build();
        let st = StringTable::from_schema(&schema);

        // Nur ein Eintrag für http://example.org
        let count = (0..st.uri_count())
            .filter(|&id| st.get_uri(id) == Some("http://example.org"))
            .count();
        assert_eq!(count, 1);
    }

    /// Table D-5: XSD-NS Local-Name Partition hat 46 built-in types
    #[test]
    fn schema_informed_local_name_xsd_ns_46_types() {
        let schema = crate::schema::SchemaInfo::builder().build();
        let st = StringTable::from_schema(&schema);

        let xsd_uri_id = st.lookup_uri(URI_XSD).unwrap();
        assert_eq!(st.local_name_count(xsd_uri_id), 46);

        // Erste und letzte prüfen (alphabetisch sortiert)
        assert_eq!(st.get_local_name(xsd_uri_id, 0), Some("ENTITIES"));
        assert_eq!(st.get_local_name(xsd_uri_id, 45), Some("unsignedShort"));

        // Einige aus der Mitte
        assert!(st.lookup_local_name(xsd_uri_id, "string").is_some());
        assert!(st.lookup_local_name(xsd_uri_id, "int").is_some());
        assert!(st.lookup_local_name(xsd_uri_id, "boolean").is_some());
    }

    /// Spec D.3: Schema-Attribute/Elemente/Typen werden zu Local-Name hinzugefügt
    #[test]
    fn schema_informed_local_name_from_schema() {
        let schema = crate::schema::SchemaInfo::builder()
            .global_element(QName::new("http://example.org", "book"))
            .global_element(QName::new("http://example.org", "author"))
            .attribute(QName::new("http://example.org", "id"))
            .build();
        let st = StringTable::from_schema(&schema);

        let uri_id = st.lookup_uri("http://example.org").unwrap();

        // Alle drei local-names vorhanden
        assert!(st.lookup_local_name(uri_id, "book").is_some());
        assert!(st.lookup_local_name(uri_id, "author").is_some());
        assert!(st.lookup_local_name(uri_id, "id").is_some());

        // Sortiert: author < book < id
        let author_id = st.lookup_local_name(uri_id, "author").unwrap();
        let book_id = st.lookup_local_name(uri_id, "book").unwrap();
        let id_id = st.lookup_local_name(uri_id, "id").unwrap();
        assert!(author_id < book_id);
        assert!(book_id < id_id);
    }

    /// Spec D.3: Local-Names sind unique innerhalb ihrer Partition
    #[test]
    fn schema_informed_local_name_unique() {
        let schema = crate::schema::SchemaInfo::builder()
            .global_element(QName::new("http://example.org", "item"))
            .all_element(QName::new("http://example.org", "item")) // Duplikat
            .attribute(QName::new("http://example.org", "item")) // Duplikat
            .build();
        let st = StringTable::from_schema(&schema);

        let uri_id = st.lookup_uri("http://example.org").unwrap();

        // Nur ein Eintrag für "item"
        assert_eq!(st.local_name_count(uri_id), 1);
        assert_eq!(st.lookup_local_name(uri_id, "item"), Some(0));
    }

    /// Spec 7.3.1: Wildcard-URIs werden zur URI-Partition hinzugefügt
    #[test]
    fn schema_informed_uri_wildcard_namespaces() {
        use crate::schema::*;

        // Schema mit Wildcard (Namespaces-Constraint)
        let complex_type = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: Some(AttributeWildcard::Namespaces(vec![
                "http://wildcard.org/a".to_string(),
                "http://wildcard.org/b".to_string(),
            ])),
            content: ContentType::Empty,
            has_named_sub_types: false,
        };

        let type_qname = Rc::new(QName::new("http://example.org", "MyType"));
        let schema = crate::schema::SchemaInfo::builder()
            .type_definition(type_qname, Rc::new(complex_type))
            .build();
        let st = StringTable::from_schema(&schema);

        // Wildcard-URIs sollten in der URI-Partition sein
        assert!(st.lookup_uri("http://wildcard.org/a").is_some());
        assert!(st.lookup_uri("http://wildcard.org/b").is_some());
    }

    /// from_schema respektiert value Options
    #[test]
    fn schema_informed_with_options() {
        let schema = crate::schema::SchemaInfo::builder().build();
        let qname = QName::new("http://example.org", "elem");

        // valueMaxLength=3: längere Strings werden nicht hinzugefügt
        let mut st = StringTable::from_schema_with_options(&schema, Some(3), None);
        st.encode_value(&qname, "toolong").unwrap(); // 7 chars > 3
        assert_eq!(st.global_value_count(), 0);

        // valuePartitionCapacity=2: Wrap-Around funktioniert
        let mut st = StringTable::from_schema_with_options(&schema, None, Some(2));
        st.encode_value(&qname, "a").unwrap();
        st.encode_value(&qname, "b").unwrap();
        st.encode_value(&qname, "c").unwrap(); // evicts "a"
        assert_eq!(st.lookup_global_value("a"), None);
        assert_eq!(st.lookup_global_value("c"), Some(0));
    }

    /// Pre-populated URIs: leere URI wird nicht dupliziert
    #[test]
    fn schema_informed_empty_uri_not_duplicated() {
        let schema = crate::schema::SchemaInfo::builder()
            .global_element(QName::new("", "root")) // leere URI
            .build();
        let st = StringTable::from_schema(&schema);

        // Leere URI ist schon bei ID 0, sollte nicht dupliziert werden
        assert_eq!(st.lookup_uri(""), Some(0));
        let count = (0..st.uri_count())
            .filter(|&id| st.get_uri(id) == Some(""))
            .count();
        assert_eq!(count, 1);
    }

    /// XML/XSI URIs werden nicht dupliziert
    #[test]
    fn schema_informed_builtin_uris_not_duplicated() {
        let schema = crate::schema::SchemaInfo::builder()
            .global_element(QName::new(URI_XML, "custom"))
            .global_element(QName::new(URI_XSI, "custom2"))
            .build();
        let st = StringTable::from_schema(&schema);

        // XML bei ID 1, XSI bei ID 2 - nicht dupliziert
        assert_eq!(st.lookup_uri(URI_XML), Some(1));
        assert_eq!(st.lookup_uri(URI_XSI), Some(2));
    }

    // ==========================================================================
    // Coverage-Lücken Tests
    // ==========================================================================

    /// Spec 7.3.3: ValueStore::add auf bounded Store
    #[test]
    fn value_store_add_bounded() {
        let mut vs = ValueStore::with_capacity(3);
        assert_eq!(vs.add("first".into()), 0);
        assert_eq!(vs.add("second".into()), 1);
        assert_eq!(vs.get(0), Some("first"));
        assert_eq!(vs.get(1), Some("second"));
        assert_eq!(vs.len(), 2);
    }

    /// Spec 7.3.3: ValueStore::set überschreibt existierenden Wert
    #[test]
    fn value_store_set_overwrite() {
        let mut vs = ValueStore::with_capacity(3);
        vs.set(0, "old_value".into());
        assert_eq!(vs.get(0), Some("old_value"));

        // Überschreiben an gleicher Position
        vs.set(0, "new_value".into());
        assert_eq!(vs.get(0), Some("new_value"));
    }

    /// Spec 7.3.2: decode_prefix_hit mit index_plus_one=0 (Zeile 913)
    #[test]
    fn decode_prefix_hit_zero_invalid() {
        let st = StringTable::new();
        // index_plus_one=0 ist der Miss-Marker, nicht gültig für Hit
        let result = st.decode_prefix_hit(URI_ID_XML, 0);
        assert!(result.is_err());
    }

    /// Spec 7.3.1: Wildcard-URIs aus ElementOnly Content (Zeile 458, 462, 582-590)
    #[test]
    fn schema_informed_uri_wildcard_in_element_only_content() {
        use crate::schema::*;

        // Wildcard-Particle mit Namespaces-Constraint
        let wildcard = Wildcard::new(WildcardConstraint::Namespaces(vec![
            "http://content-wildcard.org".to_string(),
        ]));
        let particle = Particle::once(ParticleTerm::Wildcard(wildcard));

        let complex_type = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: ContentType::ElementOnly(particle),
            has_named_sub_types: false,
        };

        let type_qname = Rc::new(QName::new("http://example.org", "MyType"));
        let schema = crate::schema::SchemaInfo::builder()
            .type_definition(type_qname, Rc::new(complex_type))
            .build();
        let st = StringTable::from_schema(&schema);

        // Wildcard-URI aus Content sollte in der URI-Partition sein
        assert!(st.lookup_uri("http://content-wildcard.org").is_some());
    }

    /// Spec 7.3.1: Wildcard-URIs aus Mixed Content (Zeile 458)
    #[test]
    fn schema_informed_uri_wildcard_in_mixed_content() {
        use crate::schema::*;

        let wildcard = Wildcard::new(WildcardConstraint::Namespaces(vec![
            "http://mixed-wildcard.org".to_string(),
        ]));
        let particle = Particle::once(ParticleTerm::Wildcard(wildcard));

        let complex_type = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: ContentType::Mixed(particle),
            has_named_sub_types: false,
        };

        let type_qname = Rc::new(QName::new("http://example.org", "MixedType"));
        let schema = crate::schema::SchemaInfo::builder()
            .type_definition(type_qname, Rc::new(complex_type))
            .build();
        let st = StringTable::from_schema(&schema);

        assert!(st.lookup_uri("http://mixed-wildcard.org").is_some());
    }

    /// Spec 7.3.1: Wildcard-URIs in ModelGroup (rekursiv) (Zeile 592-594)
    #[test]
    fn schema_informed_uri_wildcard_in_model_group() {
        use crate::schema::*;

        // Wildcard in einer Sequence
        let wildcard = Wildcard::new(WildcardConstraint::Namespaces(vec![
            "http://nested-wildcard.org".to_string(),
        ]));
        let wildcard_particle = Particle::once(ParticleTerm::Wildcard(wildcard));
        let group = ModelGroup::sequence(vec![wildcard_particle]);
        let group_particle = Particle::once(ParticleTerm::ModelGroup(group));

        let complex_type = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: ContentType::ElementOnly(group_particle),
            has_named_sub_types: false,
        };

        let type_qname = Rc::new(QName::new("http://example.org", "GroupType"));
        let schema = crate::schema::SchemaInfo::builder()
            .type_definition(type_qname, Rc::new(complex_type))
            .build();
        let st = StringTable::from_schema(&schema);

        // Wildcard-URI aus verschachtelter ModelGroup
        assert!(st.lookup_uri("http://nested-wildcard.org").is_some());
    }

    /// Spec 7.3.1: Element-Particle wird übersprungen (Zeile 597)
    #[test]
    fn schema_informed_uri_element_particle_no_wildcard() {
        use crate::schema::*;

        // Element-Particle (keine Wildcard-URIs)
        let elem_decl = ElementDeclaration::new(Rc::new(QName::new("http://elem.org", "child")));
        let particle = Particle::once(ParticleTerm::Element(elem_decl));

        let complex_type = TypeDefinition::Complex {
            name: None,
            base_type: None,
            derivation: None,
            attributes: vec![],
            attribute_wildcard: None,
            content: ContentType::ElementOnly(particle),
            has_named_sub_types: false,
        };

        let type_qname = Rc::new(QName::new("http://example.org", "ElemType"));
        let schema = crate::schema::SchemaInfo::builder()
            .type_definition(type_qname, Rc::new(complex_type))
            .build();
        let st = StringTable::from_schema(&schema);

        // Element-URI sollte NICHT durch Wildcard-Sammlung hinzugefügt werden
        // (nur durch normale Target-Namespace Sammlung - hier nicht, da nur in type_definition)
        // Der Test stellt sicher, dass der Element-Branch in collect_wildcard_uris_from_particle_set
        // durchlaufen wird ohne Fehler
        assert!(st.lookup_uri(URI_XSD).is_some()); // XSD immer vorhanden
    }

    /// Spec 7.3.2: decode_prefix_hit Erfolgsfall (Zeilen 901-905)
    #[test]
    fn decode_prefix_hit_success() {
        let st = StringTable::new();
        // XML-Namespace hat pre-populated prefix "xml" bei ID 0
        let result = st.decode_prefix_hit(URI_ID_XML, 1); // index_plus_one=1 → id=0
        assert_eq!(result.unwrap(), "xml");
    }

    /// Spec 7.3.2: decode_prefix_hit ungültiger Index (Zeilen 901-905)
    #[test]
    fn decode_prefix_hit_invalid_index() {
        let st = StringTable::new();
        // index_plus_one=100 → id=99, existiert nicht
        let result = st.decode_prefix_hit(URI_ID_XML, 100);
        assert!(result.is_err());
    }

    /// Spec 7.3.2: decode_prefix_miss fügt Prefix hinzu (Zeilen 909-913)
    #[test]
    fn decode_prefix_miss_adds_prefix() {
        let mut st = StringTable::new();
        // Neues Prefix für existierende URI hinzufügen
        let id = st.decode_prefix_miss(URI_ID_XML, "xmlalias");
        assert_eq!(id, 1); // "xml" ist bereits bei 0

        // Prefix sollte jetzt abrufbar sein
        let result = st.decode_prefix_hit(URI_ID_XML, 2); // index_plus_one=2 → id=1
        assert_eq!(result.unwrap(), "xmlalias");
    }

    /// Spec 7.3.2: decode_prefix_miss für neue URI (Zeilen 909-913)
    #[test]
    fn decode_prefix_miss_new_uri() {
        let mut st = StringTable::new();
        // Neue URI hinzufügen
        let (result, _) = st.encode_uri("http://new.org");
        let uri_id = match result {
            CompactIdResult::Miss(id) => id,
            CompactIdResult::Hit(id) => id,
        };

        // Prefix für neue URI hinzufügen (Partition existiert noch nicht)
        let prefix_id = st.decode_prefix_miss(uri_id, "newprefix");
        assert_eq!(prefix_id, 0); // Erstes Prefix für diese URI

        // Prefix sollte abrufbar sein
        let result = st.decode_prefix_hit(uri_id, 1);
        assert_eq!(result.unwrap(), "newprefix");
    }

    /// Spec 7.3.1: SimpleTypes werden in collect_schema_uris übersprungen (Zeilen 483-484)
    #[test]
    fn schema_informed_simple_type_skipped() {
        use crate::schema::*;

        // Schema mit SimpleType (wird übersprungen, da keine Wildcards möglich)
        let simple_type = TypeDefinition::simple();
        let type_qname = Rc::new(QName::new("http://example.org", "MySimpleType"));
        let schema = crate::schema::SchemaInfo::builder()
            .type_definition(type_qname, Rc::new(simple_type))
            .build();

        // from_schema sollte ohne Fehler funktionieren
        let st = StringTable::from_schema(&schema);

        // XSD-Namespace sollte immer vorhanden sein
        assert!(st.lookup_uri(URI_XSD).is_some());
    }

    /// Regression: Riesige value_partition_capacity darf kein OOM verursachen.
    /// Gefunden durch Fuzzer (decode-Target, Crash-Input oom-72a698...).
    #[test]
    fn huge_value_partition_capacity_no_oom() {
        // 638_354_598 war der Wert aus dem Fuzzer-Crash
        let st = StringTable::with_options(None, Some(638_354_598));
        // Muss ohne OOM erstellt werden (lazy Allokation)
        assert_eq!(st.global_value_count(), 0);
    }

    /// Bounded ValueStore mit lazy Allokation funktioniert korrekt.
    #[test]
    fn bounded_value_store_lazy_growth() {
        let mut store = ValueStore::with_capacity(1000);
        assert!(store.entries.is_empty());

        // add() soll lazy wachsen
        let rc: Rc<str> = "test".into();
        let id = store.add(Rc::clone(&rc));
        assert_eq!(id, 0);
        assert_eq!(store.entries.len(), 1);
        assert_eq!(store.get(0), Some("test"));

        // set() soll ebenfalls lazy wachsen
        let rc2: Rc<str> = "hello".into();
        store.set(5, rc2);
        assert_eq!(store.entries.len(), 6);
        assert_eq!(store.get(5), Some("hello"));
    }
}
