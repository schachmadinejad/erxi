//! QName encoding (Spec 7.1.7).
//!
//! A QName is a sequence of URI, local-name, and optional prefix components.
//! The prefix is present only when `Preserve.prefixes` is true.
//!
//! Encoding modes:
//! - **Full**: URI (String) + local-name (String) + optional prefix (n-bit)
//! - **Implicit URI**: local-name (String) + optional prefix (n-bit)
//! - **Implicit URI and local-name**: optional prefix only (n-bit)
//!
//! String Table integration (Compact IDs) is handled in the string_table module.
//!
//! ## Interning
//!
//! Für Performance-kritische Pfade (Grammar-Lookups, Element-Stack) gibt es
//! interned Varianten: [`InternedStr`], [`ExpandedNameId`], [`QNameId`].
//! Diese sind `Copy`-Typen (nur u32-Indizes) und vermeiden Arc-Overhead.

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};

use ahash::AHasher;
use std::rc::Rc;

use crate::bitstream::{BitReader, BitWriter};
use crate::string_table::URI_XSI;
use crate::{Error, Result, FastHashMap, n_bit_unsigned_integer, string};

// ============================================================================
// Interning: StringInterner, InternedStr, ExpandedNameId, QNameId
// ============================================================================

/// Index in den [`StringInterner`]. `Copy`-Type, kein Heap.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternedStr(pub(crate) u32);

impl fmt::Debug for InternedStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "InternedStr({})", self.0)
    }
}

/// Semantische Identität eines QName: URI + local-name (ohne Prefix).
///
/// Spec 7.1.7: "Two qnames are considered equal if they have the same uri
/// and local-name, regardless of their prefix values."
///
/// Verwendet für Grammar-Keys (GrammarKey in Encoder/Decoder).
/// `Copy`-Type — Vergleich ist 2 × u32-Vergleich statt 2 × String-Vergleich.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpandedNameId {
    pub(crate) uri: InternedStr,
    pub(crate) local_name: InternedStr,
}

impl fmt::Debug for ExpandedNameId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ExpandedNameId({:?}, {:?})", self.uri, self.local_name)
    }
}

impl ExpandedNameId {
    /// Erstellt eine neue ExpandedNameId.
    pub fn new(uri: InternedStr, local_name: InternedStr) -> Self {
        Self { uri, local_name }
    }

    /// Löst die ExpandedNameId zu Strings auf.
    pub fn resolve<'a>(&self, interner: &'a StringInterner) -> (&'a str, &'a str) {
        (interner.resolve(self.uri), interner.resolve(self.local_name))
    }

    /// Konvertiert zu QName (für öffentliche API / Serialisierung).
    pub fn to_qname(&self, interner: &StringInterner) -> QName {
        let (uri, local_name) = self.resolve(interner);
        QName::new(uri, local_name)
    }

    /// Wie `to_qname()`, aber teilt die Rc<str>-Instanzen des Interners.
    /// Keine neue Heap-Allokation — nur 2 Rc-Refcount-Inkremente.
    pub fn to_qname_shared(&self, interner: &StringInterner) -> QName {
        let uri = interner.resolve_rc(self.uri);
        let local_name = interner.resolve_rc(self.local_name);
        let identity = compute_identity(&uri, &local_name);
        QName { uri, local_name, prefix: None, identity }
    }
}

// ============================================================================
// QNamePool: Rc<QName>-Cache für den Decoder
// ============================================================================

/// Pool für `Rc<QName>`: Gibt für dieselbe `ExpandedNameId` immer denselben
/// `Rc<QName>` zurück. Eliminiert wiederholte QName-Konstruktion + Rc-Allokation
/// im Decode-Hot-Path.
///
/// Schema-informierte Dekodierung verwendet typischerweise 20-100 verschiedene
/// QNames, ruft diese aber millionenfach ab. Ohne Pool: QName-Konstruktion
/// (2× Rc-Allokation für uri/local_name + AHash-Berechnung) pro Event.
/// Mit Pool: 1× Rc::clone pro Event (HashMap-Lookup auf Copy-Key).
pub(crate) struct QNamePool {
    cache: FastHashMap<ExpandedNameId, Rc<QName>>,
}

impl QNamePool {
    pub(crate) fn new() -> Self {
        Self {
            cache: FastHashMap::default(),
        }
    }

    /// Gibt den gecachten `Rc<QName>` zurück oder erstellt einen neuen.
    #[inline]
    pub(crate) fn get_or_create(
        &mut self,
        expanded: ExpandedNameId,
        interner: &StringInterner,
    ) -> Rc<QName> {
        Rc::clone(
            self.cache
                .entry(expanded)
                .or_insert_with(|| Rc::new(expanded.to_qname_shared(interner))),
        )
    }

    /// Wie `get_or_create`, aber mit optionalem Prefix.
    /// Bei `Some(prefix)` wird kein Pooling durchgeführt (Prefix variiert je nach NS-Kontext).
    #[inline]
    pub(crate) fn get_or_create_with_prefix(
        &mut self,
        expanded: ExpandedNameId,
        prefix: Option<Rc<str>>,
        interner: &StringInterner,
    ) -> Rc<QName> {
        match prefix {
            None => self.get_or_create(expanded, interner),
            Some(pfx) => {
                let uri = interner.resolve_rc(expanded.uri);
                let local_name = interner.resolve_rc(expanded.local_name);
                Rc::new(QName::with_optional_prefix(uri, local_name, Some(pfx)))
            }
        }
    }
}

/// Vollständiger interned QName mit optionalem Prefix.
///
/// `Eq`/`Hash` basieren nur auf [`ExpandedNameId`] (prefix ignoriert),
/// konsistent mit [`QName`].
#[derive(Clone, Copy, Debug)]
pub struct QNameId {
    pub(crate) expanded: ExpandedNameId,
    pub(crate) prefix: Option<InternedStr>,
}

impl PartialEq for QNameId {
    fn eq(&self, other: &Self) -> bool {
        self.expanded == other.expanded
    }
}
impl Eq for QNameId {}

impl Hash for QNameId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.expanded.hash(state);
    }
}

impl QNameId {
    /// Erstellt eine QNameId ohne Prefix.
    pub fn new(uri: InternedStr, local_name: InternedStr) -> Self {
        Self {
            expanded: ExpandedNameId::new(uri, local_name),
            prefix: None,
        }
    }

    /// Erstellt eine QNameId mit Prefix.
    pub fn with_prefix(uri: InternedStr, local_name: InternedStr, prefix: InternedStr) -> Self {
        Self {
            expanded: ExpandedNameId::new(uri, local_name),
            prefix: Some(prefix),
        }
    }

    /// Die semantische Identität (URI + local-name).
    pub fn expanded(&self) -> ExpandedNameId {
        self.expanded
    }

    /// Konvertiert zurück zu QName (für öffentliche API / Serialisierung).
    pub fn to_qname(&self, interner: &StringInterner) -> QName {
        let uri = interner.resolve(self.expanded.uri);
        let local_name = interner.resolve(self.expanded.local_name);
        match self.prefix.map(|p| interner.resolve(p)) {
            Some(pfx) => QName::with_prefix(uri, local_name, pfx),
            None => QName::new(uri, local_name),
        }
    }

    /// Wie `to_qname()`, aber teilt die Rc<str>-Instanzen des Interners.
    /// Keine neue Heap-Allokation — nur Rc-Refcount-Inkremente.
    pub fn to_qname_shared(&self, interner: &StringInterner) -> QName {
        let uri = interner.resolve_rc(self.expanded.uri);
        let local_name = interner.resolve_rc(self.expanded.local_name);
        let identity = compute_identity(&uri, &local_name);
        let prefix = self.prefix.map(|p| interner.resolve_rc(p));
        QName { uri, local_name, prefix, identity }
    }
}

const INTERN_CACHE_SLOTS: usize = 32;

/// Zentraler String-Pool für QName-Interning.
///
/// Speichert jeden String einmalig als `Rc<str>`. Sowohl `strings` (Index→String)
/// als auch `lookup` (String→Index) zeigen auf denselben Arc — kein doppelter
/// Speicherverbrauch.
///
/// Ein Direct-Mapped Cache (32 Slots) vermeidet HashMap-Lookups für
/// wiederkehrende Strings (URI, local-name) — >99% Hit-Rate bei ~20 unique Strings.
#[derive(Clone)]
pub struct StringInterner {
    strings: Vec<Rc<str>>,
    lookup: FastHashMap<Rc<str>, u32>,
    /// Direct-Mapped Cache: (hash, InternedStr) — vermeidet HashMap-Hash+Probe.
    cache: [(u64, InternedStr); INTERN_CACHE_SLOTS],
}

impl StringInterner {
    /// Erstellt einen neuen, leeren Interner.
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            lookup: FastHashMap::default(),
            cache: [(0, InternedStr(u32::MAX)); INTERN_CACHE_SLOTS],
        }
    }

    /// Erstellt einen Interner mit vorgegebener Kapazität.
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            strings: Vec::with_capacity(cap),
            lookup: FastHashMap::with_capacity_and_hasher(cap, ahash::RandomState::new()),
            cache: [(0, InternedStr(u32::MAX)); INTERN_CACHE_SLOTS],
        }
    }

    /// Internt einen String. Gibt den Index zurück.
    /// Bereits bekannte Strings werden dedupliziert (nur Lookup, keine Allokation).
    /// Nutzt Direct-Mapped Cache für Hot-Path (>99% Hit bei wiederkehrenden Strings).
    pub fn intern(&mut self, s: &str) -> Result<InternedStr> {
        // Fast-Path: Direct-Mapped Cache (kein HashMap-Lookup)
        let hash = {
            use std::hash::{Hash, Hasher};
            let mut hasher = ahash::AHasher::default();
            s.hash(&mut hasher);
            hasher.finish()
        };
        let slot = hash as usize % INTERN_CACHE_SLOTS;
        let (cached_hash, cached_id) = self.cache[slot];
        if cached_hash == hash && cached_id.0 != u32::MAX {
            // Verifiziere: gecachter Index zeigt auf denselben String
            if self.strings.get(cached_id.0 as usize).is_some_and(|v| &**v == s) {
                return Ok(cached_id);
            }
        }

        // Slow-Path: HashMap-Lookup
        if let Some(&idx) = self.lookup.get(s) {
            let id = InternedStr(idx);
            self.cache[slot] = (hash, id);
            return Ok(id);
        }
        let idx = u32::try_from(self.strings.len())
            .map_err(|_| Error::IntegerOverflow)?;
        let arc: Rc<str> = Rc::from(s);
        self.strings.push(Rc::clone(&arc));
        self.lookup.insert(arc, idx);
        let id = InternedStr(idx);
        self.cache[slot] = (hash, id);
        Ok(id)
    }

    /// Löst einen InternedStr zu &str auf.
    #[inline]
    pub fn resolve(&self, id: InternedStr) -> &str {
        &self.strings[id.0 as usize]
    }

    /// Gibt den internierten Rc<str> zurück (Rc::clone = nur Refcount-Inkrement).
    #[inline]
    pub fn resolve_rc(&self, id: InternedStr) -> Rc<str> {
        Rc::clone(&self.strings[id.0 as usize])
    }

    /// Anzahl der internierten Strings.
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Ob der Interner leer ist.
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    /// Internt einen QName und gibt die QNameId zurück.
    pub fn intern_qname(&mut self, qname: &QName) -> Result<QNameId> {
        let uri = self.intern(&qname.uri)?;
        let local_name = self.intern(&qname.local_name)?;
        let prefix = qname.prefix.as_deref().map(|p| self.intern(p)).transpose()?;
        Ok(QNameId {
            expanded: ExpandedNameId::new(uri, local_name),
            prefix,
        })
    }

    /// Internt nur URI + local-name und gibt ExpandedNameId zurück.
    pub fn intern_expanded(&mut self, uri: &str, local_name: &str) -> Result<ExpandedNameId> {
        let uri = self.intern(uri)?;
        let local_name = self.intern(local_name)?;
        Ok(ExpandedNameId::new(uri, local_name))
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for StringInterner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StringInterner({} strings)", self.strings.len())
    }
}

/// A QName value with URI, local-name, and optional prefix.
///
/// Per Spec 7.1.7: "Two qnames are considered equal if they have the same uri
/// and local-name, regardless of their prefix values." Therefore, `PartialEq`,
/// `Eq`, and `Hash` only consider `uri` and `local_name`.
///
/// `identity` ist ein vorberechneter 64-Bit-Hash von (uri, local_name).
/// Vermeidet wiederholtes String-Hashing bei HashMap-Operationen.
/// Wird bei Konstruktion berechnet, nie persistiert.
#[derive(Clone)]
pub struct QName {
    /// The namespace URI. Empty string means no namespace.
    pub uri: Rc<str>,
    /// The local name.
    pub local_name: Rc<str>,
    /// The optional prefix. Only present when `Preserve.prefixes` is true.
    pub prefix: Option<Rc<str>>,
    /// Vorberechneter Hash von (uri, local_name). Vermeidet wiederholtes
    /// String-Hashing bei HashMap-Operationen (O(1) statt O(n) pro Hash-Aufruf).
    identity: u64,
}

impl fmt::Debug for QName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("QName")
            .field("uri", &self.uri)
            .field("local_name", &self.local_name)
            .field("prefix", &self.prefix)
            .finish()
    }
}

/// Berechnet den Identity-Hash für ein QName (uri + local_name).
pub(crate) fn compute_identity(uri: &str, local_name: &str) -> u64 {
    let mut hasher = AHasher::default();
    uri.hash(&mut hasher);
    local_name.hash(&mut hasher);
    hasher.finish()
}

thread_local! {
    static XSI_TYPE_CACHED: QName = QName::with_prefix(URI_XSI, "type", "xsi");
    static XSI_NIL_CACHED: QName = QName::with_prefix(URI_XSI, "nil", "xsi");
}

/// Spec 7.1.7: "Two qnames are considered equal if they have the same uri
/// and local-name, regardless of their prefix values."
impl PartialEq for QName {
    fn eq(&self, other: &Self) -> bool {
        self.identity == other.identity
            && self.uri == other.uri
            && self.local_name == other.local_name
    }
}

impl Eq for QName {}

/// Ermöglicht `arc_qname == qname` Vergleiche (z.B. in Encoder/Decoder).
impl PartialEq<QName> for Rc<QName> {
    fn eq(&self, other: &QName) -> bool {
        **self == *other
    }
}

/// Ermöglicht `qname == arc_qname` Vergleiche.
impl PartialEq<Rc<QName>> for QName {
    fn eq(&self, other: &Rc<QName>) -> bool {
        *self == **other
    }
}

/// Ordering konsistent mit PartialEq: nur uri und local_name, prefix ignoriert.
///
/// Sortierung: erst local_name, dann uri (konsistent mit Spec 8.5.1).
impl PartialOrd for QName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for QName {
    fn cmp(&self, other: &Self) -> Ordering {
        self.local_name
            .cmp(&other.local_name)
            .then_with(|| self.uri.cmp(&other.uri))
    }
}

impl Hash for QName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.identity.hash(state);
    }
}

/// Display: Zeigt `prefix:local_name` wenn Prefix vorhanden, sonst nur `local_name`.
impl fmt::Display for QName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.prefix {
            Some(pfx) if !pfx.is_empty() => write!(f, "{pfx}:{}", self.local_name),
            _ => f.write_str(&self.local_name),
        }
    }
}

impl Default for QName {
    fn default() -> Self {
        Self {
            uri: Rc::from(""),
            local_name: Rc::from(""),
            prefix: None,
            identity: compute_identity("", ""),
        }
    }
}

impl QName {
    /// Creates a new QName with the given URI and local-name, without prefix.
    pub fn new(uri: impl Into<Rc<str>>, local_name: impl Into<Rc<str>>) -> Self {
        let uri = uri.into();
        let local_name = local_name.into();
        let identity = compute_identity(&uri, &local_name);
        Self {
            uri,
            local_name,
            prefix: None,
            identity,
        }
    }

    /// Creates a new QName with URI, local-name, and prefix.
    pub fn with_prefix(
        uri: impl Into<Rc<str>>,
        local_name: impl Into<Rc<str>>,
        prefix: impl Into<Rc<str>>,
    ) -> Self {
        let uri = uri.into();
        let local_name = local_name.into();
        let identity = compute_identity(&uri, &local_name);
        Self {
            uri,
            local_name,
            prefix: Some(prefix.into()),
            identity,
        }
    }

    /// Erstellt einen QName mit optionalem Prefix.
    ///
    /// Interne Hilfsfunktion für decode-Pfade, bei denen der Prefix
    /// als `Option<Rc<str>>` vorliegt.
    pub(crate) fn with_optional_prefix(
        uri: Rc<str>,
        local_name: Rc<str>,
        prefix: Option<Rc<str>>,
    ) -> Self {
        let identity = compute_identity(&uri, &local_name);
        Self { uri, local_name, prefix, identity }
    }

    /// xsi:type QName (Spec 8.4.3).
    pub fn xsi_type() -> Self {
        XSI_TYPE_CACHED.with(|q| q.clone())
    }

    /// xsi:nil QName (Spec 8.4.3).
    pub fn xsi_nil() -> Self {
        XSI_NIL_CACHED.with(|q| q.clone())
    }

    /// Check ob dieser QName xsi:type ist (Spec 8.4.3).
    #[inline]
    pub fn is_xsi_type(&self) -> bool {
        XSI_TYPE_CACHED.with(|q| *self == *q)
    }

    /// Check ob dieser QName xsi:nil ist (Spec 8.4.3).
    #[inline]
    pub fn is_xsi_nil(&self) -> bool {
        XSI_NIL_CACHED.with(|q| *self == *q)
    }

    /// Vorberechneter Identity-Hash von (uri, local_name).
    /// Kann als Key in HashMaps verwendet werden um String-Hashing zu vermeiden.
    pub fn identity_hash(&self) -> u64 {
        self.identity
    }
}

/// Encodes a full QName: URI + local-name + optional prefix (Spec 7.1.7).
///
/// The `prefix_count` parameter indicates how many prefixes are in the prefix
/// string table partition for this URI. When `preserve_prefixes` is true and
/// `prefix_count > 0`, the prefix is encoded as an n-bit unsigned integer index.
/// When `prefix_count` is 1, the prefix uses 0 bits (omitted).
pub fn encode(writer: &mut BitWriter, qname: &QName, preserve_prefixes: bool, prefix_count: usize) {
    string::encode(writer, &qname.uri);
    string::encode(writer, &qname.local_name);
    encode_prefix(writer, qname, preserve_prefixes, prefix_count);
}

/// Encodes a QName with implicit URI: local-name + optional prefix (Spec 7.1.7).
///
/// Used when URI is derived from schema-informed grammar (SE(uri:*) or AT(uri:*)).
pub fn encode_implicit_uri(
    writer: &mut BitWriter,
    qname: &QName,
    preserve_prefixes: bool,
    prefix_count: usize,
) {
    string::encode(writer, &qname.local_name);
    encode_prefix(writer, qname, preserve_prefixes, prefix_count);
}

/// Encodes a QName with implicit URI and local-name: prefix only (Spec 7.1.7).
///
/// Used when both URI and local-name are specified by schema-informed grammar
/// (SE(qname) or AT(qname)).
pub fn encode_implicit_uri_and_local_name(
    writer: &mut BitWriter,
    qname: &QName,
    preserve_prefixes: bool,
    prefix_count: usize,
) {
    encode_prefix(writer, qname, preserve_prefixes, prefix_count);
}

/// Encodes the prefix component if `preserve_prefixes` is true.
///
/// **Note:** This implementation always encodes prefix index 0. Actual prefix-to-index
/// lookup via String Tables is not yet implemented. The `qname.prefix` field is
/// currently not used for index lookup during encoding.
fn encode_prefix(
    writer: &mut BitWriter,
    _qname: &QName,
    preserve_prefixes: bool,
    prefix_count: usize,
) {
    if !preserve_prefixes {
        return;
    }

    // n = ceil(log2(N)) where N = prefix_count (Spec 7.1.7)
    // When N <= 1, prefix uses 0 bits (omitted per Spec Note)
    let n = prefix_bits(prefix_count);
    if n == 0 {
        return;
    }

    // Prefix-Index 0 ist korrekt für den häufigsten Fall (ein Prefix pro NS).
    // Mehrere Prefixes pro Namespace (z.B. xmlns:a="..." xmlns:b="..." gleiche URI)
    // sind selten; für volle Korrektheit müsste der String Table konsultiert werden.
    n_bit_unsigned_integer::encode(writer, 0, n);
}

/// Decodes a full QName: URI + local-name + optional prefix (Spec 7.1.7).
pub fn decode(
    reader: &mut BitReader,
    preserve_prefixes: bool,
    prefix_count: usize,
    prefix_resolver: impl Fn(u64) -> Option<Rc<str>>,
) -> Result<QName> {
    let uri: Rc<str> = string::decode(reader)?.into();
    let local_name: Rc<str> = string::decode(reader)?.into();
    let prefix = decode_prefix(reader, preserve_prefixes, prefix_count, prefix_resolver)?;
    Ok(QName::with_optional_prefix(uri, local_name, prefix))
}

/// Decodes a QName with implicit URI: local-name + optional prefix (Spec 7.1.7).
pub fn decode_implicit_uri(
    reader: &mut BitReader,
    uri: Rc<str>,
    preserve_prefixes: bool,
    prefix_count: usize,
    prefix_resolver: impl Fn(u64) -> Option<Rc<str>>,
) -> Result<QName> {
    let local_name: Rc<str> = string::decode(reader)?.into();
    let prefix = decode_prefix(reader, preserve_prefixes, prefix_count, prefix_resolver)?;
    Ok(QName::with_optional_prefix(uri, local_name, prefix))
}

/// Decodes a QName with implicit URI and local-name: prefix only (Spec 7.1.7).
pub fn decode_implicit_uri_and_local_name(
    reader: &mut BitReader,
    uri: Rc<str>,
    local_name: Rc<str>,
    preserve_prefixes: bool,
    prefix_count: usize,
    prefix_resolver: impl Fn(u64) -> Option<Rc<str>>,
) -> Result<QName> {
    let prefix = decode_prefix(reader, preserve_prefixes, prefix_count, prefix_resolver)?;
    Ok(QName::with_optional_prefix(uri, local_name, prefix))
}

/// Decodes the prefix component if `preserve_prefixes` is true.
fn decode_prefix(
    reader: &mut BitReader,
    preserve_prefixes: bool,
    prefix_count: usize,
    prefix_resolver: impl Fn(u64) -> Option<Rc<str>>,
) -> Result<Option<Rc<str>>> {
    if !preserve_prefixes {
        return Ok(None);
    }

    let n = prefix_bits(prefix_count);
    if n == 0 {
        return Ok(None);
    }

    let prefix_index = n_bit_unsigned_integer::decode(reader, n)?;

    // Range check: n bits can encode values >= prefix_count for non-power-of-2 counts.
    // Spec 7.1.7: "A QName is in error if its prefix cannot be resolved"
    if prefix_index >= prefix_count as u64 {
        return Err(Error::UnresolvedPrefix(prefix_index));
    }

    prefix_resolver(prefix_index)
        .ok_or_else(|| Error::UnresolvedPrefix(prefix_index))
        .map(Some)
}

/// Calculates the number of bits needed for prefix encoding.
/// n = ceil(log2(max(N, 1))) where N is the prefix count.
fn prefix_bits(prefix_count: usize) -> u8 {
    if prefix_count <= 1 {
        0
    } else {
        (usize::BITS - (prefix_count - 1).leading_zeros()) as u8
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper: round-trip for full QName
    fn round_trip_full(
        qname: &QName,
        preserve_prefixes: bool,
        prefix_count: usize,
        prefixes: &[&str],
    ) -> QName {
        let mut w = BitWriter::new();
        encode(&mut w, qname, preserve_prefixes, prefix_count);
        let data = w.into_vec();
        let mut r = BitReader::new(&data);
        decode(&mut r, preserve_prefixes, prefix_count, |idx| {
            prefixes.get(idx as usize).map(|s| Rc::from(*s))
        })
        .unwrap()
    }

    // === Spec 7.1.7: Basic QName encoding ===

    /// Spec 7.1.7: QName with URI and local-name, no prefix
    #[test]
    fn full_qname_no_prefix() {
        let qname = QName::new("http://example.org", "element");
        let result = round_trip_full(&qname, false, 0, &[]);
        assert_eq!(&*result.uri, "http://example.org");
        assert_eq!(&*result.local_name, "element");
        assert_eq!(result.prefix, None);
    }

    /// Spec 7.1.7: QName in no namespace (empty URI)
    #[test]
    fn qname_no_namespace() {
        let qname = QName::new("", "local");
        let result = round_trip_full(&qname, false, 0, &[]);
        assert_eq!(&*result.uri, "");
        assert_eq!(&*result.local_name, "local");
    }

    /// Spec 7.1.7: QName with Unicode characters
    #[test]
    fn qname_unicode() {
        let qname = QName::new("http://例え.jp/名前空間", "要素");
        let result = round_trip_full(&qname, false, 0, &[]);
        assert_eq!(&*result.uri, "http://例え.jp/名前空間");
        assert_eq!(&*result.local_name, "要素");
    }

    // === Spec 7.1.7: Prefix encoding ===

    /// Spec 7.1.7: preserve_prefixes=true, prefix_count=1 → 0 bits (omitted)
    #[test]
    fn prefix_zero_bits_when_count_is_one() {
        let qname = QName::with_prefix("http://example.org", "elem", "ex");
        let mut w = BitWriter::new();
        encode(&mut w, &qname, true, 1);
        let pos_with_prefix = w.bit_position();

        let qname_no_prefix = QName::new("http://example.org", "elem");
        let mut w2 = BitWriter::new();
        encode(&mut w2, &qname_no_prefix, false, 0);
        let pos_without_prefix = w2.bit_position();

        // Same size because prefix uses 0 bits when count=1
        assert_eq!(pos_with_prefix, pos_without_prefix);
    }

    /// Spec 7.1.7: decode with preserve_prefixes=true, prefix_count=1 → prefix omitted
    #[test]
    fn decode_prefix_zero_bits_when_count_is_one() {
        let qname = QName::new("http://example.org", "elem");
        let mut w = BitWriter::new();
        encode(&mut w, &qname, true, 1);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result = decode(&mut r, true, 1, |_| Some(Rc::from("ex"))).unwrap();

        // Prefix is None because 0 bits means omitted
        assert_eq!(result.prefix, None);
    }

    /// Spec 7.1.7: preserve_prefixes=true, prefix_count=2 → 1 bit
    #[test]
    fn prefix_one_bit_when_count_is_two() {
        let qname = QName::with_prefix("http://example.org", "elem", "ex");
        let mut w = BitWriter::new();
        encode(&mut w, &qname, true, 2);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result = decode(&mut r, true, 2, |idx| {
            ["ex", "other"].get(idx as usize).map(|s| Rc::from(*s))
        })
        .unwrap();

        assert_eq!(result.prefix.as_deref(), Some("ex"));
    }

    /// Spec 7.1.7: preserve_prefixes=true, prefix_count=4 → 2 bits
    #[test]
    fn prefix_two_bits_when_count_is_four() {
        assert_eq!(prefix_bits(4), 2);
    }

    /// Spec 7.1.7: preserve_prefixes=true, prefix_count=5 → 3 bits
    #[test]
    fn prefix_three_bits_when_count_is_five() {
        assert_eq!(prefix_bits(5), 3);
    }

    /// Spec 7.1.7: preserve_prefixes=false → no prefix encoded
    #[test]
    fn no_prefix_when_preserve_false() {
        let qname = QName::with_prefix("http://example.org", "elem", "ex");
        let result = round_trip_full(&qname, false, 2, &["ex", "other"]);
        assert_eq!(result.prefix, None);
    }

    // === Spec 7.1.7: Implicit URI encoding ===

    /// Spec 7.1.7: SE(uri:*) / AT(uri:*) → only local-name encoded
    #[test]
    fn implicit_uri_encoding() {
        let qname = QName::new("http://example.org", "element");

        let mut w = BitWriter::new();
        encode_implicit_uri(&mut w, &qname, false, 0);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result =
            decode_implicit_uri(&mut r, Rc::from("http://example.org"), false, 0, |_| None)
                .unwrap();

        assert_eq!(&*result.uri, "http://example.org");
        assert_eq!(&*result.local_name, "element");
    }

    // === Spec 7.1.7: Implicit URI and local-name encoding ===

    /// Spec 7.1.7: SE(qname) / AT(qname) → only prefix encoded (if any)
    #[test]
    fn implicit_uri_and_local_name_no_prefix() {
        let qname = QName::new("http://example.org", "element");

        let mut w = BitWriter::new();
        encode_implicit_uri_and_local_name(&mut w, &qname, false, 0);
        let data = w.into_vec();

        // Should be empty - nothing to encode
        assert!(data.is_empty());

        let mut r = BitReader::new(&data);
        let result = decode_implicit_uri_and_local_name(
            &mut r,
            Rc::from("http://example.org"),
            Rc::from("element"),
            false,
            0,
            |_| None,
        )
        .unwrap();

        assert_eq!(&*result.uri, "http://example.org");
        assert_eq!(&*result.local_name, "element");
    }

    /// Spec 7.1.7: SE(qname) / AT(qname) with prefix
    #[test]
    fn implicit_uri_and_local_name_with_prefix() {
        let qname = QName::with_prefix("http://example.org", "element", "ex");

        let mut w = BitWriter::new();
        encode_implicit_uri_and_local_name(&mut w, &qname, true, 2);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result = decode_implicit_uri_and_local_name(
            &mut r,
            Rc::from("http://example.org"),
            Rc::from("element"),
            true,
            2,
            |idx| ["ex", "other"].get(idx as usize).map(|s| Rc::from(*s)),
        )
        .unwrap();

        assert_eq!(result.prefix.as_deref(), Some("ex"));
    }

    // === Spec 7.1.7: Error cases ===

    /// Spec 7.1.7: Unresolved prefix → Error
    #[test]
    fn unresolved_prefix_error() {
        let qname = QName::with_prefix("http://example.org", "elem", "ex");

        let mut w = BitWriter::new();
        encode(&mut w, &qname, true, 2);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result = decode(&mut r, true, 2, |_| None); // resolver returns None

        assert!(matches!(result, Err(Error::UnresolvedPrefix(0))));
    }

    /// Spec 7.1.7: EOF during URI decode
    #[test]
    fn decode_eof_on_uri() {
        let mut r = BitReader::new(&[]);
        let result = decode(&mut r, false, 0, |_| None);
        assert!(matches!(result, Err(Error::PrematureEndOfStream)));
    }

    /// Spec 7.1.7: EOF during local-name decode
    #[test]
    fn decode_eof_on_local_name() {
        let mut w = BitWriter::new();
        string::encode(&mut w, "http://example.org");
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result = decode(&mut r, false, 0, |_| None);
        assert!(matches!(result, Err(Error::PrematureEndOfStream)));
    }

    /// Spec 7.1.7: EOF during prefix decode
    #[test]
    fn decode_eof_on_prefix() {
        let mut w = BitWriter::new();
        string::encode(&mut w, "http://example.org");
        string::encode(&mut w, "element");
        // No prefix bits written
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result = decode(&mut r, true, 4, |_| None); // expects 2 bits for prefix
        assert!(matches!(result, Err(Error::PrematureEndOfStream)));
    }

    // === prefix_bits helper ===

    #[test]
    fn prefix_bits_calculation() {
        assert_eq!(prefix_bits(0), 0); // N=0 → 0 bits
        assert_eq!(prefix_bits(1), 0); // N=1 → 0 bits (omitted)
        assert_eq!(prefix_bits(2), 1); // ceil(log2(2)) = 1
        assert_eq!(prefix_bits(3), 2); // ceil(log2(3)) = 2
        assert_eq!(prefix_bits(4), 2); // ceil(log2(4)) = 2
        assert_eq!(prefix_bits(5), 3); // ceil(log2(5)) = 3
        assert_eq!(prefix_bits(8), 3); // ceil(log2(8)) = 3
        assert_eq!(prefix_bits(9), 4); // ceil(log2(9)) = 4
        assert_eq!(prefix_bits(256), 8); // ceil(log2(256)) = 8
        assert_eq!(prefix_bits(257), 9); // ceil(log2(257)) = 9
    }

    // === Sequential QNames ===

    #[test]
    fn sequential_qnames() {
        let q1 = QName::new("http://a.org", "first");
        let q2 = QName::new("http://b.org", "second");
        let q3 = QName::new("", "third");

        let mut w = BitWriter::new();
        encode(&mut w, &q1, false, 0);
        encode(&mut w, &q2, false, 0);
        encode(&mut w, &q3, false, 0);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        assert_eq!(decode(&mut r, false, 0, |_| None).unwrap(), q1);
        assert_eq!(decode(&mut r, false, 0, |_| None).unwrap(), q2);
        assert_eq!(decode(&mut r, false, 0, |_| None).unwrap(), q3);
    }

    // === Spec 7.1.7: Equality ignores prefix ===

    /// Spec 7.1.7: "Two qnames are considered equal if they have the same uri
    /// and local-name, regardless of their prefix values."
    #[test]
    fn equality_ignores_prefix() {
        let q1 = QName::new("http://example.org", "elem");
        let q2 = QName::with_prefix("http://example.org", "elem", "ex");
        let q3 = QName::with_prefix("http://example.org", "elem", "other");

        assert_eq!(q1, q2);
        assert_eq!(q2, q3);
        assert_eq!(q1, q3);
    }

    /// Spec 7.1.7: Hash must be consistent with equality (only uri + local-name)
    #[test]
    fn hash_consistent_with_equality() {
        use std::collections::hash_map::DefaultHasher;

        let q1 = QName::new("http://example.org", "elem");
        let q2 = QName::with_prefix("http://example.org", "elem", "ex");

        let hash1 = {
            let mut h = DefaultHasher::new();
            q1.hash(&mut h);
            h.finish()
        };
        let hash2 = {
            let mut h = DefaultHasher::new();
            q2.hash(&mut h);
            h.finish()
        };

        assert_eq!(hash1, hash2);
    }

    // === Edge cases ===

    /// Spec 7.1.10: Empty local-name is valid (unusual but allowed)
    #[test]
    fn qname_empty_local_name() {
        let qname = QName::new("http://example.org", "");
        let result = round_trip_full(&qname, false, 0, &[]);
        assert_eq!(&*result.uri, "http://example.org");
        assert_eq!(&*result.local_name, "");
    }

    /// Spec 7.1.7: Empty prefix string (default namespace prefix)
    #[test]
    fn qname_empty_prefix() {
        let qname = QName::with_prefix("http://example.org", "elem", "");
        let mut w = BitWriter::new();
        encode(&mut w, &qname, true, 2);
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result = decode(&mut r, true, 2, |idx| {
            ["", "ns"].get(idx as usize).map(|s| Rc::from(*s))
        })
        .unwrap();

        assert_eq!(result.prefix.as_deref(), Some(""));
    }

    /// Spec 7.1.7: EOF during implicit_uri local-name decode
    #[test]
    fn decode_implicit_uri_eof_on_local_name() {
        let mut r = BitReader::new(&[]);
        let result =
            decode_implicit_uri(&mut r, Rc::from("http://example.org"), false, 0, |_| None);
        assert!(matches!(result, Err(Error::PrematureEndOfStream)));
    }

    /// Spec 7.1.7: EOF during implicit_uri prefix decode
    #[test]
    fn decode_implicit_uri_eof_on_prefix() {
        let mut w = BitWriter::new();
        string::encode(&mut w, "element");
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result =
            decode_implicit_uri(&mut r, Rc::from("http://example.org"), true, 4, |_| None);
        assert!(matches!(result, Err(Error::PrematureEndOfStream)));
    }

    /// Spec 7.1.7: EOF during implicit_uri_and_local_name prefix decode
    #[test]
    fn decode_implicit_uri_and_local_name_eof_on_prefix() {
        let mut r = BitReader::new(&[]);
        let result = decode_implicit_uri_and_local_name(
            &mut r,
            Rc::from("http://example.org"),
            Rc::from("element"),
            true,
            4, // expects 2 bits
            |_| None,
        );
        assert!(matches!(result, Err(Error::PrematureEndOfStream)));
    }

    /// Spec 7.1.7: prefix_index >= prefix_count is invalid (non-power-of-2 case)
    #[test]
    fn decode_prefix_index_out_of_range() {
        // prefix_count=3 needs 2 bits (ceil(log2(3))=2), but 2 bits can encode 0-3.
        // Index 3 is out of range.
        let mut w = BitWriter::new();
        string::encode(&mut w, "http://example.org");
        string::encode(&mut w, "element");
        n_bit_unsigned_integer::encode(&mut w, 3, 2); // index 3, but only 0-2 valid
        let data = w.into_vec();

        let mut r = BitReader::new(&data);
        let result = decode(&mut r, true, 3, |idx| {
            ["a", "b", "c"].get(idx as usize).map(|s| Rc::from(*s))
        });

        assert!(matches!(result, Err(Error::UnresolvedPrefix(3))));
    }

    // === Spec 8.4.3: xsi:type and xsi:nil constants ===

    /// Spec 8.4.3: xsi:type QName hat korrekte URI, local-name, prefix
    #[test]
    fn xsi_type_qname() {
        let xsi_type = QName::xsi_type();
        assert_eq!(&*xsi_type.uri, "http://www.w3.org/2001/XMLSchema-instance");
        assert_eq!(&*xsi_type.local_name, "type");
        assert_eq!(xsi_type.prefix.as_deref(), Some("xsi"));
    }

    /// Spec 8.4.3: xsi:nil QName hat korrekte URI, local-name, prefix
    #[test]
    fn xsi_nil_qname() {
        let xsi_nil = QName::xsi_nil();
        assert_eq!(&*xsi_nil.uri, "http://www.w3.org/2001/XMLSchema-instance");
        assert_eq!(&*xsi_nil.local_name, "nil");
        assert_eq!(xsi_nil.prefix.as_deref(), Some("xsi"));
    }

    /// Spec 8.4.3: xsi:type und xsi:nil sind verschieden
    #[test]
    fn xsi_type_and_nil_are_different() {
        let xsi_type = QName::xsi_type();
        let xsi_nil = QName::xsi_nil();
        assert_ne!(xsi_type, xsi_nil);
    }

    /// Spec 8.4.3: xsi:type Equality ignoriert Prefix
    #[test]
    fn xsi_type_equality_ignores_prefix() {
        let xsi_type = QName::xsi_type();
        let xsi_type_no_prefix = QName::new("http://www.w3.org/2001/XMLSchema-instance", "type");
        assert_eq!(xsi_type, xsi_type_no_prefix);
    }

    // === StringInterner Tests ===

    #[test]
    fn interner_dedupliziert() {
        let mut interner = StringInterner::new();
        let a1 = interner.intern("hello").unwrap();
        let a2 = interner.intern("hello").unwrap();
        let b = interner.intern("world").unwrap();
        assert_eq!(a1, a2);
        assert_ne!(a1, b);
        assert_eq!(interner.len(), 2);
    }

    #[test]
    fn interner_resolve() {
        let mut interner = StringInterner::new();
        let id = interner.intern("test").unwrap();
        assert_eq!(interner.resolve(id), "test");
    }

    #[test]
    fn expanded_name_id_eq_ignoriert_prefix() {
        let mut interner = StringInterner::new();
        let uri = interner.intern("http://example.org").unwrap();
        let local = interner.intern("elem").unwrap();

        let e1 = ExpandedNameId::new(uri, local);
        let e2 = ExpandedNameId::new(uri, local);
        assert_eq!(e1, e2);
    }

    #[test]
    fn qname_id_eq_ignoriert_prefix() {
        let mut interner = StringInterner::new();
        let uri = interner.intern("http://example.org").unwrap();
        let local = interner.intern("elem").unwrap();
        let pfx1 = interner.intern("ex").unwrap();
        let pfx2 = interner.intern("other").unwrap();

        let q1 = QNameId::with_prefix(uri, local, pfx1);
        let q2 = QNameId::with_prefix(uri, local, pfx2);
        let q3 = QNameId::new(uri, local);

        assert_eq!(q1, q2);
        assert_eq!(q1, q3);
    }

    #[test]
    fn qname_id_hash_ignoriert_prefix() {
        use std::collections::hash_map::DefaultHasher;

        let mut interner = StringInterner::new();
        let uri = interner.intern("http://example.org").unwrap();
        let local = interner.intern("elem").unwrap();
        let pfx = interner.intern("ex").unwrap();

        let q1 = QNameId::new(uri, local);
        let q2 = QNameId::with_prefix(uri, local, pfx);

        let hash = |q: &QNameId| {
            let mut h = DefaultHasher::new();
            q.hash(&mut h);
            h.finish()
        };
        assert_eq!(hash(&q1), hash(&q2));
    }

    #[test]
    fn intern_qname_roundtrip() {
        let mut interner = StringInterner::new();
        let qname = QName::with_prefix("http://example.org", "elem", "ex");
        let id = interner.intern_qname(&qname).unwrap();
        let back = id.to_qname(&interner);
        assert_eq!(qname, back);
        assert_eq!(back.prefix.as_deref(), Some("ex"));
    }

    #[test]
    fn expanded_name_id_resolve() {
        let mut interner = StringInterner::new();
        let id = interner.intern_expanded("http://example.org", "elem").unwrap();
        let (uri, local) = id.resolve(&interner);
        assert_eq!(uri, "http://example.org");
        assert_eq!(local, "elem");
    }

    #[test]
    fn qname_id_is_copy() {
        let mut interner = StringInterner::new();
        let uri = interner.intern("").unwrap();
        let local = interner.intern("root").unwrap();
        let id = QNameId::new(uri, local);
        let id2 = id; // Copy
        let id3 = id; // Kann nochmal kopiert werden
        assert_eq!(id2, id3);
    }

    // ========================================================================
    // QNamePool Tests
    // ========================================================================

    #[test]
    fn qname_pool_cache_hit_returns_same_arc() {
        let mut interner = StringInterner::new();
        let uri = interner.intern("http://example.com").unwrap();
        let local = interner.intern("elem").unwrap();
        let expanded = ExpandedNameId { uri, local_name: local };

        let mut pool = QNamePool::new();
        let arc1 = pool.get_or_create(expanded, &interner);
        let arc2 = pool.get_or_create(expanded, &interner);
        assert!(Rc::ptr_eq(&arc1, &arc2));
    }

    #[test]
    fn qname_pool_with_prefix_bypasses_cache() {
        let mut interner = StringInterner::new();
        let uri = interner.intern("http://example.com").unwrap();
        let local = interner.intern("elem").unwrap();
        let expanded = ExpandedNameId { uri, local_name: local };

        let mut pool = QNamePool::new();
        let arc_no_prefix = pool.get_or_create(expanded, &interner);
        let arc_with_prefix = pool.get_or_create_with_prefix(
            expanded,
            Some("ns".into()),
            &interner,
        );
        // Mit Prefix → andere Instanz (nicht gepoolt)
        assert!(!Rc::ptr_eq(&arc_no_prefix, &arc_with_prefix));
        assert_eq!(arc_with_prefix.prefix, Some(Rc::from("ns")));
    }

    #[test]
    fn qname_pool_prefix_none_delegates_to_cache() {
        let mut interner = StringInterner::new();
        let uri = interner.intern("").unwrap();
        let local = interner.intern("root").unwrap();
        let expanded = ExpandedNameId { uri, local_name: local };

        let mut pool = QNamePool::new();
        let arc1 = pool.get_or_create(expanded, &interner);
        let arc2 = pool.get_or_create_with_prefix(expanded, None, &interner);
        assert!(Rc::ptr_eq(&arc1, &arc2));
    }
}
