//! EXI Stream Encoder – Spec 6, 4, 5
//!
//! Encodiert EXI Events zu einem EXI Stream. Unterstützt sowohl Schema-less
//! als auch Schema-informed Encoding.
//!
//! # Beispiel
//!
//! ```
//! use std::rc::Rc;
//! use erxi::encoder::encode;
//! use erxi::event::ExiEvent;
//! use erxi::options::ExiOptions;
//! use erxi::qname::QName;
//!
//! let events = vec![
//!     ExiEvent::StartDocument,
//!     ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
//!     ExiEvent::EndElement,
//!     ExiEvent::EndDocument,
//! ];
//! let bytes = encode(&events, &ExiOptions::default()).unwrap();
//! assert!(!bytes.is_empty());
//! ```

mod compression;
mod pending;
mod qname;
mod value;

use crate::FastHashMap;
use std::rc::Rc;


mod config;
pub use config::EncoderConfig;
pub(crate) use config::{auto_config, ensure_streaming_compatible};
use crate::bitstream::BitWriter;
use crate::error::{Error, Result};
use crate::event::{ChContent, ExiEvent, NsContent};
use crate::xml::XmlEvent;
use crate::event_code::{
    EventCode, EventCodeContext, encode_bit_packed, encode_byte_aligned,
};
use crate::grammar::{
    AttributeKind, GrammarCache, GrammarKey, GrammarSystem, GrammarType, GrammarTemplateCache,
    NonTerminalId, Production, StartElementKind, Terminal, GrammarEvolution,
    cached_schema_grammar, compact_nt_bits,
};
use crate::undeclared::Tier2Context;
use crate::header::ExiHeader;
use crate::options::{Alignment, ExiOptions};
use crate::qname::{ExpandedNameId, QName, StringInterner};
use crate::schema::{AttributeWildcard, ElementDeclaration, SchemaInfo, TypeDefinition};
use crate::string_table::StringTable;

// ============================================================================
// EventRef — Arc-freie Event-Abstraktion
// ============================================================================

/// Thin Event-Abstraktion für Rc-freien Zugriff im Encoder.
///
/// Ermöglicht den internen Encoder-Methoden, sowohl mit ExiEvent (Batch-API)
/// als auch mit XmlEvent (Streaming-Pfad) zu arbeiten, ohne Code-Duplizierung
/// und ohne Rc-Allokationen auf dem heißen Pfad.
enum EventRef<'a> {
    Exi(&'a ExiEvent),
    Xml(&'a XmlEvent<'a>),
}

impl<'a> EventRef<'a> {
    fn is_start_element(&self) -> bool {
        matches!(
            self,
            Self::Exi(ExiEvent::StartElement(_)) | Self::Xml(XmlEvent::StartElement(_))
        )
    }

    fn is_end_element(&self) -> bool {
        matches!(
            self,
            Self::Exi(ExiEvent::EndElement) | Self::Xml(XmlEvent::EndElement)
        )
    }

    fn is_attribute(&self) -> bool {
        matches!(
            self,
            Self::Exi(ExiEvent::Attribute(_)) | Self::Xml(XmlEvent::Attribute { .. })
        )
    }

    fn is_characters(&self) -> bool {
        matches!(
            self,
            Self::Exi(ExiEvent::Characters(_)) | Self::Xml(XmlEvent::Characters(_))
        )
    }

    fn is_start_element_or_characters(&self) -> bool {
        self.is_start_element() || self.is_characters()
    }

    /// QName für SE und AT Events.
    fn qname(&self) -> Option<&QName> {
        match self {
            Self::Exi(ExiEvent::StartElement(q)) => Some(q),
            Self::Exi(ExiEvent::Attribute(a)) => Some(&a.qname),
            Self::Xml(XmlEvent::StartElement(q)) => Some(q),
            Self::Xml(XmlEvent::Attribute { qname, .. }) => Some(qname),
            _ => None,
        }
    }

    /// Value für AT und CH Events.
    fn value(&self) -> Option<&str> {
        match self {
            Self::Exi(ExiEvent::Attribute(a)) => Some(&a.value),
            Self::Exi(ExiEvent::Characters(c)) => Some(&c.value),
            Self::Xml(XmlEvent::Attribute { value, .. }) => Some(value),
            Self::Xml(XmlEvent::Characters(s)) => Some(s),
            _ => None,
        }
    }

    /// NsContent für NS Events.
    fn ns_content(&self) -> Option<&NsContent> {
        match self {
            Self::Exi(ExiEvent::NamespaceDeclaration(ns)) => Some(ns),
            Self::Xml(XmlEvent::NamespaceDeclaration(ns)) => Some(ns),
            _ => None,
        }
    }

    /// Konvertiert zu ExiEvent. Nur für seltene Events (CM, PI, DT, ER) im
    /// Catch-All von encode_event_content verwendet. Auf dem heißen Pfad
    /// (SE, EE, AT, CH) wird dies nie aufgerufen.
    fn to_exi_event_for_content(&self) -> ExiEvent {
        match self {
            Self::Exi(e) => (*e).clone(),
            Self::Xml(e) => e.to_exi_event(),
        }
    }
}

/// Fidelity-Filter: Prüft ob ein Event encodiert werden soll (Spec 6.3).
fn should_encode_event_ref(event: &EventRef<'_>, options: &ExiOptions) -> Result<bool> {
    // Kern-Events (SD, ED, SE, EE, AT, CH) werden immer encodiert.
    if event.is_start_element()
        || event.is_end_element()
        || event.is_attribute()
        || event.is_characters()
    {
        return Ok(true);
    }
    match event {
        EventRef::Exi(ExiEvent::StartDocument) | EventRef::Xml(XmlEvent::StartDocument)
        | EventRef::Exi(ExiEvent::EndDocument) | EventRef::Xml(XmlEvent::EndDocument) => Ok(true),

        EventRef::Exi(ExiEvent::Comment(_)) | EventRef::Xml(XmlEvent::Comment(_)) => {
            Ok(options.preserve.comments)
        }
        EventRef::Exi(ExiEvent::ProcessingInstruction(_))
        | EventRef::Xml(XmlEvent::ProcessingInstruction { .. }) => Ok(options.preserve.pis),
        EventRef::Exi(ExiEvent::DocType(_)) | EventRef::Xml(XmlEvent::DocType(_)) => {
            Ok(options.preserve.dtd)
        }
        EventRef::Exi(ExiEvent::EntityReference(_))
        | EventRef::Xml(XmlEvent::EntityReference(_)) => Ok(options.preserve.dtd),
        EventRef::Exi(ExiEvent::NamespaceDeclaration(_))
        | EventRef::Xml(XmlEvent::NamespaceDeclaration(_)) => Ok(options.preserve.prefixes),
        EventRef::Exi(ExiEvent::SelfContained) => Ok(false),
        _ => Ok(true),
    }
}

// ============================================================================
// ElementContext
// ============================================================================

/// Kontext pro Element (für Element-Stack).
#[derive(Debug, Clone)]
struct ElementContext {
    /// Interned QName-Identität (Copy, für Grammar-Key Lookups).
    expanded_name: ExpandedNameId,
    /// Element-Typ aus dem Schema (falls verfügbar).
    element_type: Option<Rc<TypeDefinition>>,
    /// Ob das Element nillable ist (Schema-informed).
    is_nillable: bool,
    /// Ob xsi:type in strict-mode verfügbar wäre (Schema-informed).
    needs_xsi_type: bool,
    /// Key für die Element-Grammar (global oder lokal).
    grammar_key: GrammarKey,
    /// Aktuelles NonTerminal in der Element-Grammar.
    current_nt: NonTerminalId,
    /// Ob bereits Content-Events (SE/CH) für dieses Element gesehen wurden.
    in_content: bool,
    /// In-scope Namespace-Bindings (prefix → URI) aus NS-Events.
    /// Wird für xsi:type-Value-Auflösung benötigt (Spec 8.5.4.4).
    /// `None` für den Normalfall (0 Bindings) — kein Heap-Alloc.
    ns_bindings: Option<Vec<(Rc<str>, Rc<str>)>>,
}

// ============================================================================
// Tier2Context Cache (Encoder)
// ============================================================================

const ENCODER_TIER2_CACHE_SLOTS: usize = 64;

/// Direct-Mapped Cache für Tier2Context im Encoder.
struct EncoderTier2Cache {
    grammar_key: GrammarKey,
    current_nt: NonTerminalId,
    in_content: bool,
    needs_xsi_type: bool,
    is_nillable: bool,
    epoch: u64,
    context: Tier2Context,
}

// ============================================================================
// Self-Contained State (Spec 8.5.4.4.1)
// ============================================================================

/// Gespeicherter Encoder-State für Self-Contained Elements (Spec 8.5.4.4.1).
///
/// Bei SC wird der gesamte mutable State gespeichert und nach dem SC-Fragment
/// wiederhergestellt. Verschachtelte SC-Elemente werden über einen Stack verwaltet.
#[derive(Clone)]
struct ScEncoderState {
    string_table: StringTable,
    element_grammars: FastHashMap<GrammarKey, Rc<GrammarSystem>>,
    document_grammar: Rc<GrammarSystem>,
    document_nt: NonTerminalId,
    element_stack: Vec<ElementContext>,
    next_local_grammar_id: u64,
    current_element_type: Option<Rc<TypeDefinition>>,
    xsi_nil_active: bool,
    xsi_type_seen: bool,
    xsi_type_switched_grammar: bool,
    document_in_content: bool,
    pending_ch: Option<crate::event::ChContent>,
}

// ============================================================================
// TraceFlags
// ============================================================================

/// Trace-Flags für Encoder-Debugging (einmalig aus Umgebungsvariablen gelesen).
#[derive(Debug, Clone, Default)]
struct EncoderTraceFlags {
    elem_event: bool,
    event_code: bool,
    lookup: bool,
}

fn env_flag(name: &str) -> bool {
    std::env::var(name).is_ok_and(|v| !v.is_empty() && v != "0" && v != "false")
}

impl EncoderTraceFlags {
    fn from_env() -> Self {
        Self {
            elem_event: env_flag("ERXI_TRACE_ELEM_EVENT"),
            event_code: env_flag("ERXI_TRACE_EVENT_CODE"),
            lookup: env_flag("ERXI_TRACE_LOOKUP"),
        }
    }
}

// ============================================================================
// WhitespaceAction (Whitespace-Klassifizierung)
// ============================================================================

/// Prüft ob ein String ausschließlich aus XML-Whitespace besteht (SP/TAB/CR/LF).
fn is_xml_whitespace(s: &str) -> bool {
    s.bytes().all(|b| matches!(b, b' ' | b'\t' | b'\r' | b'\n'))
}

/// Ergebnis der Whitespace-Klassifizierung für CH-Events.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WhitespaceAction {
    /// Whitespace beibehalten (nicht-WS oder nicht filterbar).
    Keep,
    /// Whitespace überspringen (xsi:nil, strict+element-only, in_content).
    Skip,
    /// Look-Ahead nötig: Batch-Pfad prüft next_event direkt,
    /// Streaming-Pfad puffert CH in `pending_ch`.
    NeedLookAhead,
}

impl WhitespaceAction {
    /// Löst NeedLookAhead anhand des nächsten Events auf.
    /// EE → Keep (simple data), sonst → Skip (complex data).
    fn resolve(self, next_event: Option<&ExiEvent>) -> Self {
        if self != Self::NeedLookAhead {
            return self;
        }
        if matches!(next_event, Some(ExiEvent::EndElement)) {
            Self::Keep
        } else {
            Self::Skip
        }
    }
}

// ============================================================================
// Encoder
// ============================================================================

/// EXI Stream Encoder.
///
/// Unterstützt sowohl Schema-less als auch Schema-informed Encoding.
/// Bei Schema-informed Encoding werden Typed Values verwendet und
/// Schema-informed Grammars statt Built-in Grammars.
///
/// # Spec-Referenz
/// - Spec 6: Encoding EXI Streams
/// - Spec 5: EXI Header
/// - Spec 8.5: Schema-informed Grammars
pub struct Encoder {
    /// BitWriter für Output.
    writer: BitWriter,
    /// EXI Options (Alignment, Fidelity, etc.).
    options: ExiOptions,
    /// Encoder-Konfiguration.
    config: EncoderConfig,
    /// String Table für Compact IDs.
    string_table: StringTable,
    /// Zentrale Grammar-Registry (Element Grammars werden hier geteilt).
    element_grammars: FastHashMap<GrammarKey, Rc<GrammarSystem>>,
    /// Document Grammar (Top-Level).
    document_grammar: Rc<GrammarSystem>,
    /// Aktuelles NonTerminal in Document Grammar.
    document_nt: NonTerminalId,
    /// Stack für Element-Verschachtelung.
    element_stack: Vec<ElementContext>,
    /// Nächste lokale Grammar-ID.
    next_local_grammar_id: u64,
    /// Header bereits geschrieben?
    header_written: bool,
    /// Byte-Position nach dem Header (für Compression).
    /// Bei Compression wird nur der Body komprimiert, nicht der Header.
    header_end_byte: usize,
    /// Gepufferte Values für PreCompression (Spec 9).
    /// Kompakte Darstellung: String-Bytes im `pending_string_buffer`,
    /// TypeDefs dedupliziert in `pending_type_defs`.
    pending_values: Vec<PendingValue>,
    /// Konkatenierter String-Buffer für alle pending Values.
    pending_string_buffer: Vec<u8>,
    /// Deduplizierte TypeDefinitions (Index → TypeDef).
    pending_type_defs: Vec<Rc<TypeDefinition>>,
    /// Lookup: TypeDef-Pointer → Index in pending_type_defs.
    pending_type_ptr_index: crate::FastHashMap<usize, u16>,
    /// Deduplizierte QNames für die Value-Channels (Index → QName).
    pending_channel_qnames: Vec<QName>,
    /// Lookup: (uri, local_name) identity_hash → Channel-Index.
    pending_channel_index: crate::FastHashMap<u64, u16>,
    /// Block-Grenzen für Multi-Block Compression (Spec 9.1).
    /// Jeder Eintrag ist (structure_byte_position, values_start_index).
    /// Der erste Block startet implizit bei (header_end_byte, 0).
    block_boundaries: Vec<(usize, usize)>,
    /// Anzahl Values im aktuellen Block.
    current_block_value_count: usize,

    // ========================================================================
    // Schema-informed Encoding (Issue #37)
    // ========================================================================

    /// Schema für Schema-informed Encoding (None für Schema-less).
    schema: Option<Rc<SchemaInfo>>,
    /// Aktueller Element-Typ für CH Typed Value Encoding.
    /// Wird bei push_element() gesetzt basierend auf Schema-Lookup.
    current_element_type: Option<Rc<TypeDefinition>>,
    /// Flag: xsi:nil="true" wurde im aktuellen Element gesetzt.
    /// Wenn true, darf nur noch EE folgen (Spec 8.5.4.4.2).
    xsi_nil_active: bool,
    /// Flag: xsi:type wurde im aktuellen Element gesetzt.
    /// Zusammen mit xsi:nil nicht erlaubt (Spec 8.5.4.4.2).
    xsi_type_seen: bool,
    /// Flag: xsi:type hat die aktuelle Element-Grammar gewechselt.
    xsi_type_switched_grammar: bool,
    /// Für Element-Fragment-Streams: true sobald Content-Bereich erreicht ist.
    document_in_content: bool,
    /// Gepuffertes Whitespace-CH-Event das Look-Ahead braucht (Streaming-Pfad).
    pending_ch: Option<crate::event::ChContent>,

    // ========================================================================
    // Self-Contained (Spec 8.5.4.4.1)
    // ========================================================================

    /// Stack für verschachtelte SC-States.
    sc_state_stack: Vec<ScEncoderState>,
    /// Anfangszustand der String Table (vor Body-Processing).
    /// Wird bei SC gecloned, um den Reset-State zu erzeugen.
    initial_string_table: StringTable,
    /// Fragment Grammar für SC-Fragments (Some wenn self_contained=true).
    fragment_grammar: Option<GrammarSystem>,
    /// Parallele DEFLATE-Kompression aktiviert.
    parallel_deflate: bool,
    /// Wiederverwendbare Compress-Instanz für sequentielle DEFLATE.
    compressor: Option<flate2::Compress>,
    /// Trace-Flags (einmalig im Konstruktor gelesen, kein Syscall pro Event).
    trace: EncoderTraceFlags,
    /// Grammar-Template-Cache: Pristine Schema-Grammars nach Typ+nillable.
    /// Spart erneutes Erzeugen identischer Grammars bei wiederholten Elementen
    /// gleichen Typs. Learning divergiert per Rc::make_mut() (CoW).
    grammar_template_cache: GrammarTemplateCache,
    /// String-Interner für QName→ExpandedNameId (Copy-Type Grammar-Keys).
    interner: StringInterner,
    /// Vorberechnete xsi:type ExpandedNameId für Tier2Context.
    xsi_type_id: ExpandedNameId,
    /// Vorberechnete xsi:nil ExpandedNameId für Tier2Context.
    xsi_nil_id: ExpandedNameId,

    /// Grammar-Cache: 4-Entry Direct-Mapped Cache für element_grammars-Lookups.
    grammar_cache: GrammarCache,
    /// Epoch-Counter für Grammar-Mutationen (invalidiert Grammar-Cache).
    grammar_epoch: u64,
    /// Direct-Mapped Cache für Tier2Context (vermeidet wiederholte Berechnung).
    tier2_cache: [Option<EncoderTier2Cache>; ENCODER_TIER2_CACHE_SLOTS],
}

impl Encoder {
    /// Cached Grammar-Lookup (Direct-Mapped, 4 Slots).
    fn get_element_grammar(&mut self, key: &GrammarKey) -> Option<Rc<GrammarSystem>> {
        if let Some(grammar) = self.grammar_cache.lookup(key, self.grammar_epoch) {
            return Some(grammar);
        }
        let grammar = self.element_grammars.get(key)?.clone();
        self.grammar_cache.insert(*key, &grammar, self.grammar_epoch);
        Some(grammar)
    }

    #[inline]
    fn bump_grammar_epoch(&mut self) {
        self.grammar_epoch = self.grammar_epoch.wrapping_add(1);
    }

    /// Aktiviert/deaktiviert parallele DEFLATE-Kompression.
    pub fn set_parallel_deflate(&mut self, enabled: bool) {
        self.parallel_deflate = enabled;
    }

    /// Setzt den MemoryMonitor fuer OOM-Vermeidung bei grossen Dateien.
    pub fn set_memory_monitor(&mut self, monitor: crate::memory_monitor::MemoryMonitor) {
        self.string_table.set_memory_monitor(monitor);
    }

    /// Gibt aktuelle Byte-Position (für Debugging).
    pub fn byte_position(&self) -> usize {
        self.writer.bit_position().div_ceil(8)
    }

    /// Gibt die aktuelle Buffer-Größe in Bytes zurück (für Flush-Schwellenwerte).
    pub fn buf_len(&self) -> usize {
        self.writer.buf_len()
    }

    /// Erstellt einen neuen Encoder.
    ///
    /// Gibt Fehler zurück wenn die Options ungültig sind.
    ///
    /// Der Header wird beim ersten `encode_event()` oder bei `finish()` geschrieben.
    pub fn new(options: ExiOptions, config: EncoderConfig) -> crate::Result<Self> {
        if matches!(options.schema_id(), Some(crate::options::SchemaId::Id(_))) {
            return Err(Error::InvalidOptionCombination);
        }
        let mut document_grammar = if options.fragment {
            GrammarSystem::built_in_fragment()
        } else {
            GrammarSystem::built_in_document()
        };
        // Grammar basierend auf Fidelity Options prunen
        document_grammar.prune(&options)?;
        let start_nt = document_grammar.start();

        // String Table mit Options initialisieren (Spec 7.3.3, Appendix D)
        let string_table = StringTable::with_options(
            options.value_max_length.map(|v| v as usize),
            options.value_partition_capacity.map(|v| v as usize),
        );
        // Nur bei Self-Contained nötig — SC setzt String Table beim Fragment-Eintritt zurück
        let initial_string_table = if options.self_contained {
            string_table.clone()
        } else {
            StringTable::new()
        };

        // Fragment Grammar für SC (Spec 8.5.4.4.1)
        let fragment_grammar = if options.self_contained {
            let mut fg = GrammarSystem::built_in_fragment();
            fg.prune(&options)?;
            Some(fg)
        } else {
            None
        };

        let mut interner = StringInterner::with_capacity(64);
        let xsi_type_id = interner.intern_expanded(
            crate::string_table::URI_XSI, "type",
        )?;
        let xsi_nil_id = interner.intern_expanded(
            crate::string_table::URI_XSI, "nil",
        )?;

        Ok(Self {
            writer: BitWriter::new(),
            options,
            config,
            string_table,
            element_grammars: FastHashMap::with_capacity_and_hasher(64, Default::default()),
            document_grammar: Rc::new(document_grammar),
            document_nt: start_nt,
            element_stack: Vec::with_capacity(32),
            next_local_grammar_id: 0,
            header_written: false,
            header_end_byte: 0,
            pending_values: Vec::with_capacity(256),
            pending_string_buffer: Vec::with_capacity(256),
            pending_type_defs: Vec::with_capacity(64),
            pending_type_ptr_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            pending_channel_qnames: Vec::with_capacity(64),
            pending_channel_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            block_boundaries: Vec::new(),
            current_block_value_count: 0,
            schema: None,
            current_element_type: None,
            xsi_nil_active: false,
            xsi_type_seen: false,
            xsi_type_switched_grammar: false,
            document_in_content: false,
            pending_ch: None,
            sc_state_stack: Vec::new(),
            initial_string_table,
            fragment_grammar,
            parallel_deflate: false,
            compressor: None,
            trace: EncoderTraceFlags::from_env(),
            grammar_template_cache: GrammarTemplateCache::default(),
            interner,
            xsi_type_id,
            xsi_nil_id,
            grammar_cache: GrammarCache::new(),
            grammar_epoch: 0,
            tier2_cache: [const { None }; ENCODER_TIER2_CACHE_SLOTS],
        })
    }

    /// Erstellt einen neuen Encoder mit Schema (Schema-informed Encoding).
    ///
    /// Bei Schema-informed Encoding werden:
    /// - Schema-informed Document Grammar verwendet (Spec 8.5.1)
    /// - Typed Values encodiert statt Strings (Spec 7.1)
    /// - xsi:type/xsi:nil Handling aktiviert (Spec 8.5.4.4)
    ///
    /// # Spec-Referenz
    /// - Spec 8.5: Schema-informed Grammars
    /// - Spec 7.1: Built-in EXI Datatype Representations
    pub fn with_schema(
        options: ExiOptions,
        config: EncoderConfig,
        schema: SchemaInfo,
    ) -> crate::Result<Self> {
        if matches!(
            options.schema_id(),
            Some(crate::options::SchemaId::None | crate::options::SchemaId::BuiltinOnly)
        ) {
            return Err(Error::InvalidOptionCombination);
        }
        let mut interner = StringInterner::with_capacity(256);

        // Schema-informed Grammar verwenden (Document oder Fragment)
        let mut document_grammar = if options.fragment {
            GrammarSystem::schema_informed_fragment(&schema, &mut interner)?
        } else {
            GrammarSystem::schema_informed_document(&schema, &mut interner)?
        };
        document_grammar.prune(&options)?;
        let start_nt = document_grammar.start();

        // String Table mit Schema pre-population (Spec 7.3.1, Appendix D)
        let string_table = StringTable::from_schema_with_options(
            &schema,
            options.value_max_length.map(|v| v as usize),
            options.value_partition_capacity.map(|v| v as usize),
        );
        // Nur bei Self-Contained nötig — SC setzt String Table beim Fragment-Eintritt zurück
        let initial_string_table = if options.self_contained {
            string_table.clone()
        } else {
            StringTable::new()
        };

        // Fragment Grammar für SC (Spec 8.5.4.4.1)
        let fragment_grammar = if options.self_contained {
            let mut fg = GrammarSystem::schema_informed_fragment(&schema, &mut interner)?;
            fg.prune(&options)?;
            Some(fg)
        } else {
            None
        };

        let xsi_type_id = interner.intern_expanded(
            crate::string_table::URI_XSI, "type",
        )?;
        let xsi_nil_id = interner.intern_expanded(
            crate::string_table::URI_XSI, "nil",
        )?;

        // Kapazität an Schema-Größe anpassen (vermeidet HashMap-Rehash bei push_element)
        let grammar_cap = schema.element_declarations().len().max(64);

        Ok(Self {
            writer: BitWriter::new(),
            options,
            config,
            string_table,
            element_grammars: FastHashMap::with_capacity_and_hasher(grammar_cap, Default::default()),
            document_grammar: Rc::new(document_grammar),
            document_nt: start_nt,
            element_stack: Vec::with_capacity(32),
            next_local_grammar_id: 0,
            header_written: false,
            header_end_byte: 0,
            pending_values: Vec::with_capacity(256),
            pending_string_buffer: Vec::with_capacity(256),
            pending_type_defs: Vec::with_capacity(64),
            pending_type_ptr_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            pending_channel_qnames: Vec::with_capacity(64),
            pending_channel_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            block_boundaries: Vec::new(),
            current_block_value_count: 0,
            schema: Some(Rc::new(schema)),
            current_element_type: None,
            xsi_nil_active: false,
            xsi_type_seen: false,
            xsi_type_switched_grammar: false,
            document_in_content: false,
            pending_ch: None,
            sc_state_stack: Vec::new(),
            initial_string_table,
            fragment_grammar,
            parallel_deflate: false,
            compressor: None,
            trace: EncoderTraceFlags::from_env(),
            grammar_template_cache: GrammarTemplateCache::default(),
            interner,
            xsi_type_id,
            xsi_nil_id,
            grammar_cache: GrammarCache::new(),
            grammar_epoch: 0,
            tier2_cache: [const { None }; ENCODER_TIER2_CACHE_SLOTS],
        })
    }

    /// Schreibt den EXI Header.
    ///
    /// # Spec-Referenz
    /// - Spec 5.1: EXI Cookie
    /// - Spec 5.2: Distinguishing Bits
    /// - Spec 5.3: EXI Format Version
    /// - Spec 5.4: EXI Options
    fn write_header(&mut self) -> Result<()> {
        if self.header_written {
            return Ok(());
        }

        let mut header = ExiHeader::new();
        if self.config.include_cookie {
            header = header.with_cookie();
        }
        if self.config.include_options {
            header = header.with_options();
        }

        // Header encodieren (ohne Padding - wird nach Header/Options gemacht)
        crate::header::encode(&mut self.writer, &header, false)?;

        // Options encodieren falls gewünscht
        if self.config.include_options {
            crate::options_codec::encode(&mut self.writer, &self.options)?;
        }

        // Padding nach Header/Options (Spec 5.1.1)
        // "When the compression option is true, or the alignment option is
        // byte-alignment or pre-compression, padding bits... are added at
        // the end of the header."
        // WICHTIG: Padding wird IMMER geschrieben wenn ByteAlignment/PreCompression,
        // unabhängig davon ob Options im Header sind (symmetrisch zum Decoder).
        let needs_padding = self.options.compression
            || matches!(
                self.options.alignment,
                Alignment::ByteAlignment | Alignment::PreCompression
            );

        if needs_padding {
            self.writer.align_to_byte();
        }

        self.header_written = true;
        self.header_end_byte = self.writer.bit_position().div_ceil(8);
        Ok(())
    }

    /// Encodiert ein einzelnes Event (Streaming-Pfad).
    ///
    /// Whitespace-CH-Events die Look-Ahead brauchen werden gepuffert (`pending_ch`).
    /// Das nächste Event dient als Look-Ahead: EE → Keep (simple data), sonst Skip.
    /// Bei `finish()` wird ein noch gepuffertes CH verworfen (kein EE → insignifikant).
    ///
    /// # Spec-Referenz
    /// - Spec 6: Encoding EXI Streams (Algorithmus)
    pub fn encode_event(&mut self, event: &ExiEvent) -> Result<()> {
        self.write_header()?;
        self.flush_pending_ch(matches!(event, ExiEvent::EndElement))?;

        if let ExiEvent::Characters(ch) = event {
            match self.classify_whitespace(&ch.value) {
                WhitespaceAction::Skip => return Ok(()),
                WhitespaceAction::NeedLookAhead => {
                    self.pending_ch = Some(ch.clone());
                    return Ok(());
                }
                WhitespaceAction::Keep => {}
            }
        }

        self.encode_event_core_ref(&EventRef::Exi(event))
    }

    /// Encodiert ein XmlEvent (borrowed, kein Arc) -- fuer den Streaming-Pfad.
    ///
    /// Vermeidet Rc-Allokationen auf dem heissen Pfad. Nutzt EventRef::Xml
    /// direkt, ohne Konvertierung zu ExiEvent.
    pub(crate) fn encode_xml_event(&mut self, event: &XmlEvent<'_>) -> Result<()> {
        self.write_header()?;
        self.flush_pending_ch(matches!(event, XmlEvent::EndElement))?;

        if let XmlEvent::Characters(text) = event {
            match self.classify_whitespace(text) {
                WhitespaceAction::Skip => return Ok(()),
                WhitespaceAction::NeedLookAhead => {
                    self.pending_ch = Some(ChContent { value: Rc::from(*text) });
                    return Ok(());
                }
                WhitespaceAction::Keep => {}
            }
        }

        self.encode_event_core_ref(&EventRef::Xml(event))
    }

    /// Flusht ein gepuffertes Whitespace-CH-Event (pending_ch).
    ///
    /// Bei `is_end_element=true` wird es als Characters encodiert (simple data).
    /// Sonst wird es verworfen (complex data).
    fn flush_pending_ch(&mut self, is_end_element: bool) -> Result<()> {
        if let Some(pending) = self.pending_ch.take() {
            if is_end_element {
                let ch_event = ExiEvent::Characters(pending);
                self.encode_event_core_ref(&EventRef::Exi(&ch_event))?;
            } else if self.trace.elem_event {
                eprintln!(
                    "[TRACE:elem_event] WS-CH verworfen (complex data): {:?}",
                    pending.value,
                );
            }
        }
        Ok(())
    }

    /// Innere Encoding-Logik (Fidelity-Filter, Terminal-Lookup, Dispatch).
    fn encode_event_core_ref(&mut self, event: &EventRef<'_>) -> Result<()> {
        // Fidelity-Filter
        if !should_encode_event_ref(event, &self.options)? {
            return Ok(());
        }

        // Event → Terminal
        let terminal = self.event_to_terminal_ref(event)?;

        // Dispatch basierend auf aktuellem Kontext (Document oder Element)
        if self.element_stack.is_empty() {
            self.encode_document_event_ref(event, &terminal)
        } else {
            self.encode_element_event_ref(event, &terminal)
        }
    }

    /// Batch-Encoding: Encodiert alle Events mit direktem Look-Ahead.
    ///
    /// Nutzt `encode_event_core_ref()` direkt (nicht `encode_event()`), da die
    /// Whitespace-Filterung hier über `should_skip_ignorable_characters()`
    /// mit vollständigem Look-Ahead erfolgt — kein Buffering nötig.
    fn encode_events(&mut self, events: &[ExiEvent]) -> Result<()> {
        self.write_header()?;
        let mut i = 0;
        while i < events.len() {
            let event = &events[i];
            if let ExiEvent::Characters(ch) = event
                && self.should_skip_ignorable_characters(ch, events.get(i + 1)) {
                    i += 1;
                    continue;
                }
            if matches!(event, ExiEvent::EndElement) && self.should_inject_empty_simple_content() {
                let ch = ExiEvent::Characters(crate::event::ChContent {
                    value: "".into(),
                });
                self.encode_event_core_ref(&EventRef::Exi(&ch))?;
            }
            if matches!(event, ExiEvent::StartElement(_)) {
                self.encode_event_core_ref(&EventRef::Exi(event))?;
                i += 1;

                let mut ns_events: Vec<&ExiEvent> = Vec::with_capacity(4);
                let mut attr_events: Vec<&ExiEvent> = Vec::with_capacity(8);
                while i < events.len() {
                    match &events[i] {
                        ExiEvent::NamespaceDeclaration(_) => {
                            ns_events.push(&events[i]);
                            i += 1;
                        }
                        ExiEvent::Attribute(_) => {
                            attr_events.push(&events[i]);
                            i += 1;
                        }
                        _ => break,
                    }
                }

                for ns in ns_events {
                    self.encode_event_core_ref(&EventRef::Exi(ns))?;
                }
                if !attr_events.is_empty() {
                    if self.schema.is_some() {
                        self.sort_attribute_events(&mut attr_events);
                    }
                    for attr in &attr_events {
                        self.encode_event_core_ref(&EventRef::Exi(attr))?;
                    }
                }
                continue;
            }

            self.encode_event_core_ref(&EventRef::Exi(event))?;
            i += 1;
        }
        Ok(())
    }

    /// Klassifiziert ein Whitespace-CH-Event: Keep, Skip oder NeedLookAhead.
    /// Nur `!in_content && !strict` braucht Look-Ahead (NeedLookAhead).
    fn classify_whitespace(&self, value: &str) -> WhitespaceAction {
        if !is_xml_whitespace(value) {
            return WhitespaceAction::Keep;
        }
        // xsi:nil="true" → Grammar TypeEmpty (nur EE), CH nicht encodierbar.
        if self.xsi_nil_active {
            return WhitespaceAction::Skip;
        }
        // preserve.whitespace oder preserve.lexical_values → kein WS-Stripping.
        // Schema-less: WS wird durch anyType-Semantik (is_mixed_or_untyped) behandelt,
        // offensichtliche Faelle bereits in xml.rs frueh gefiltert.
        if self.options.preserve.preserves_whitespace() {
            return WhitespaceAction::Keep;
        }
        let (is_element_only, is_mixed_or_untyped) = match self.current_element_type.as_ref() {
            Some(td) => match td.as_ref() {
                crate::schema::TypeDefinition::Complex { content, .. } => (
                    matches!(
                        content,
                        crate::schema::ContentType::ElementOnly(_)
                            | crate::schema::ContentType::Empty
                    ),
                    matches!(content, crate::schema::ContentType::Mixed(_)),
                ),
                crate::schema::TypeDefinition::Simple { .. } => (false, false),
            },
            // Undeclared: anyType-Semantik → complex data WS-Regel.
            None => (false, true),
        };
        if !is_element_only && !is_mixed_or_untyped {
            return WhitespaceAction::Keep;
        }
        // strict + element-only → keine undeclared CH-Production → immer skip.
        if self.options.strict && is_element_only {
            return WhitespaceAction::Skip;
        }
        // in_content: mindestens ein Kind-SE wurde encodiert → complex data → skip.
        if self.element_stack.last().is_some_and(|e| e.in_content) {
            return WhitespaceAction::Skip;
        }
        // Vor Kindelementen: simple data nur wenn naechstes Event EE ist → Look-Ahead.
        WhitespaceAction::NeedLookAhead
    }

    fn should_skip_ignorable_characters(
        &self,
        ch: &crate::event::ChContent,
        next_event: Option<&ExiEvent>,
    ) -> bool {
        self.classify_whitespace(&ch.value).resolve(next_event) == WhitespaceAction::Skip
    }

    fn should_inject_empty_simple_content(&self) -> bool {
        if self.schema.is_none() {
            return false;
        }
        // Non-strict: Kein CH("") fuer leere Simple-Content-Elemente injizieren.
        // EE wird direkt als Tier-2 Event encodiert (wie Exificient).
        // In strict-Modus gibt es keine Tier-2 Productions, CH("") bleibt noetig.
        if !self.options.strict {
            return false;
        }
        if self.xsi_nil_active {
            return false;
        }
        let Some(elem) = self.element_stack.last() else {
            return false;
        };
        if elem.in_content {
            return false;
        }
        let Some(td) = elem.element_type.as_ref() else {
            return false;
        };
        match td.as_ref() {
            // Spec 8.5.4.1.3.1: Simple type grammars require CH before EE,
            // even for empty content (encode empty string CH).
            // spec/exi-spec.txt lines 2407-2424.
            crate::schema::TypeDefinition::Simple { .. } => true,
            crate::schema::TypeDefinition::Complex { content, .. } => {
                // Spec 8.5.4.1.3.1 (simpleContent branch),
                // spec/exi-spec.txt lines 2407-2424.
                matches!(content, crate::schema::ContentType::Simple)
            }
        }
    }

    fn sort_attribute_events(&self, attrs: &mut Vec<&ExiEvent>) {
        // Spec 6, Z.808-809: SHOULD lexicographisch (local-name, dann URI).
        // xsi:type und xsi:nil immer zuerst (Spec 8.5.4.4.1).
        attrs.sort_by(|a, b| {
            let qa = match a {
                ExiEvent::Attribute(content) => &content.qname,
                _ => return std::cmp::Ordering::Equal,
            };
            let qb = match b {
                ExiEvent::Attribute(content) => &content.qname,
                _ => return std::cmp::Ordering::Equal,
            };
            let ka = self.attribute_sort_key(qa);
            let kb = self.attribute_sort_key(qb);
            ka.cmp(&kb)
        });
    }

    fn attribute_sort_key<'a>(&self, qname: &'a QName) -> (u8, u32, &'a str, &'a str) {
        if qname.is_xsi_type() {
            return (0, 0, &qname.local_name, &qname.uri);
        }
        if qname.is_xsi_nil() {
            return (0, 1, &qname.local_name, &qname.uri);
        }

        // Spec 9.1 SHOULD: lexicographic order (local-name, then URI).
        // Applies uniformly to declared and wildcard-matched attributes.
        (1, 0, &qname.local_name, &qname.uri)
    }

    /// Trace-Logging fuer EventRef (Arc-freier Pfad).
    fn log_elem_event_ref(&self, event: &EventRef<'_>, current_elem: Option<ExpandedNameId>) {
        if !self.trace.elem_event {
            return;
        }
        if event.is_start_element() {
            let qname = event.qname().unwrap();
            eprintln!(
                "elem_event: type=SE qname={qname} uri={} local={}",
                qname.uri,
                qname.local_name
            );
        } else if event.is_attribute() {
            let qname = event.qname().unwrap();
            eprintln!(
                "elem_event: type=AT qname={qname} uri={} local={}",
                qname.uri,
                qname.local_name
            );
        } else if event.is_end_element() {
            if let Some(expanded) = current_elem {
                let (uri, local) = expanded.resolve(&self.interner);
                eprintln!(
                    "elem_event: type=EE uri={uri} local={local}",
                );
            }
        } else if event.is_characters() {
            eprintln!("elem_event: type=CH");
        }
    }

    fn log_event_code(
        &self,
        event_code: &EventCode,
        ctx: &EventCodeContext,
        terminal: &Terminal,
        grammar: GrammarType,
    ) {
        if !self.trace.event_code {
            return;
        }

        eprintln!(
            "encode_event_code: part1={} bits={} num_values={} grammar={:?}",
            event_code.part1(),
            ctx.bits_for_part1(),
            ctx.num_values_part1(),
            grammar
        );
        eprintln!("encode_event_code: eventType={}", terminal.debug_event_type());

        if let Some(part2) = event_code.part2() {
            eprintln!(
                "encode_event_code: level=2 part2={} eventType={}",
                part2,
                terminal.debug_event_type()
            );
        }
        if let Some(part3) = event_code.part3() {
            eprintln!(
                "encode_event_code: level=3 part3={} eventType={}",
                part3,
                terminal.debug_event_type()
            );
        }
    }

    /// Encodiert ein Event im Document-Kontext.
    ///
    /// # Spec-Referenz
    /// - Spec 8.5.1: Schema-informed Document Grammar hat SE(qname) Productions
    /// - Bei SE(qname) (nicht SE(*)) ist der QName implizit durch den Event Code
    fn encode_document_event_ref(&mut self, event: &EventRef<'_>, terminal: &Terminal) -> Result<()> {
        // Production-Lookup in Document Grammar (mit Wildcard-Info)
        let (event_code, ctx, next_nt, was_wildcard, matched_terminal) =
            self.lookup_document_event_code(terminal)?;

        self.log_elem_event_ref(event, None);
        self.log_event_code(&event_code, &ctx, terminal, self.document_grammar.grammar_type());

        // Event Code encodieren
        self.encode_event_code(&event_code, &ctx)?;

        // Event Content encodieren
        let content_terminal = matched_terminal.as_ref().unwrap_or(terminal);
        self.encode_event_content_document_ref(event, content_terminal, was_wildcard)?;

        if event.is_start_element_or_characters()
            && self.document_grammar.grammar_type() == GrammarType::ElementFragment
        {
            self.document_in_content = true;
        }

        // Grammar-Transition
        if let Some(nt) = next_nt {
            self.document_nt = nt;
        }

        // Bei SE: Element-Stack pushen
        if event.is_start_element() {
            let qname = event.qname().expect("SE Event hat immer einen QName");
            self.push_element(qname)?;

            if self.fragment_grammar.is_some() && self.should_self_contain(qname) {
                self.start_self_contained(qname)?;
            }
        }

        Ok(())
    }

    /// Encodiert ein Event im Element-Kontext.
    fn encode_element_event_ref(&mut self, event: &EventRef<'_>, terminal: &Terminal) -> Result<()> {
        // Spec 8.5.4.4.2: Bei strict=true ist nach xsi:nil="true" nur EE erlaubt.
        if self.xsi_nil_active
            && self.options.strict
            && !event.is_end_element()
            && !event.is_attribute()
        {
            if event.is_characters() && event.value().is_some_and(|v| v.trim().is_empty()) {
                // Ignorable whitespace erlauben (Exificient-kompatibel)
            } else {
                return Err(Error::XsiNilContentNotEmpty);
            }
        }

        // Production-Lookup in Element Grammar (mit Wildcard-Info)
        let (event_code, ctx, next_nt, was_wildcard, matched_terminal) =
            self.lookup_element_event_code_with_wildcard(terminal)?;

        let current_elem = self.element_stack.last().map(|e| e.expanded_name);
        self.log_elem_event_ref(event, current_elem);
        let grammar = self
            .element_grammars
            .get(
                &self
                    .element_stack
                    .last()
                    .ok_or_else(|| Error::invalid_event_code("", "leerer Element-Stack"))?
                    .grammar_key,
            )
            .map(|g| g.grammar_type())
            .unwrap_or(GrammarType::Element);
        self.log_event_code(&event_code, &ctx, terminal, grammar);

        // Event Code encodieren
        self.encode_event_code(&event_code, &ctx)?;

        // Event Content encodieren
        self.xsi_type_switched_grammar = false;
        let content_terminal = matched_terminal.as_ref().unwrap_or(terminal);
        self.encode_event_content_element_ref(event, content_terminal, was_wildcard)?;

        if event.is_start_element_or_characters()
            && let Some(elem) = self.element_stack.last_mut()
        {
            elem.in_content = true;
        }

        // Grammar Evolution bei Wildcard-Match (Spec 8.4.3)
        if was_wildcard {
            self.evolve_element_grammar_ref(event, terminal)?;
        }

        // Grammar Evolution bei CH/EE mit Event Code Länge > 1 (Spec 8.4.3)
        if matches!(
            terminal,
            Terminal::Characters | Terminal::CharactersUntyped | Terminal::EndElement
        )
            && event_code.length() > 1
        {
            self.evolve_content_grammar(terminal, next_nt)?;
        }

        // Grammar-Transition im Element
        if let (Some(elem), Some(nt)) = (self.element_stack.last_mut(), next_nt) {
            let is_xsi_type = event.is_attribute()
                && event.qname().is_some_and(|q| q.is_xsi_type());
            if !(is_xsi_type && self.xsi_type_switched_grammar) {
                elem.current_nt = nt;
            }
        }

        if event.is_attribute()
            && event.qname().is_some_and(|q| q.is_xsi_nil())
            && event.value().is_some_and(|v| matches!(v, "true" | "1"))
        {
            self.switch_current_element_to_type_empty();
        }

        // Bei SE: Neues Element pushen
        if event.is_start_element() {
            let qname = event.qname().unwrap();
            self.push_element(qname)?;

            if self.fragment_grammar.is_some() && self.should_self_contain(qname) {
                self.start_self_contained(qname)?;
            }
        }

        // Bei EE: Element-Stack poppen, lokale Grammar aufräumen,
        // current_element_type aktualisieren
        if event.is_end_element() {
            if let Some(ctx) = self.element_stack.pop() {
                // Lokale Grammars (local_id != None) werden nach Pop nicht mehr
                // gebraucht — sofort entfernen verhindert HashMap-Akkumulation
                // (~360k Einträge bei 10MB Schema-Encode).
                if ctx.grammar_key.local_id.is_some() {
                    self.element_grammars.remove(&ctx.grammar_key);
                }
            }

            if self.element_stack.is_empty() && !self.sc_state_stack.is_empty() {
                self.end_self_contained()?;
                return Ok(());
            }

            self.xsi_nil_active = false;
            self.xsi_type_seen = false;
            self.current_element_type = self
                .element_stack
                .last()
                .and_then(|elem| elem.element_type.clone());
        }

        Ok(())
    }

    /// Startet ein Self-Contained Fragment (Spec 8.5.4.4.1).
    ///
    /// Prüft ob das Element SC-gewrappt werden soll.
    /// Wenn `self_contained_qnames` leer ist, werden alle Elemente gewrappt.
    /// Sonst nur Elemente mit passendem URI+local_name.
    fn should_self_contain(&self, qname: &QName) -> bool {
        if self.options.self_contained_qnames.is_empty() {
            return true;
        }
        self.options
            .self_contained_qnames
            .iter()
            .any(|q| q.uri == qname.uri && q.local_name == qname.local_name)
    }

    /// 1. SC-Production in der aktuellen Element-Grammar finden und Event-Code encodieren
    /// 2. State speichern
    /// 3. State auf Anfangszustand zurücksetzen
    /// 4. Byte-Alignment
    /// 5. SD auf Fragment Grammar encodieren
    /// 6. SE(qname) auf FragmentContent encodieren
    /// 7. push_element im SC-Fragment-Kontext
    fn start_self_contained(&mut self, qname: &QName) -> Result<()> {
        // SC-Production in der Element Grammar des gerade gepushten Elements finden.
        // SC ist in Element i,0 (StartTagContent) — dem Startzustand des Elements.
        let child_elem = self.element_stack.last().ok_or_else(|| Error::invalid_event_code("SC", "leerer Element-Stack"))?;
        let grammar_key = child_elem.grammar_key;
        let current_nt = child_elem.current_nt;

        let Some(grammar) = self.element_grammars.get(&grammar_key) else {
            return Err(Error::invalid_event_code("SC", "Grammar nicht gefunden für SC-Element"));
        };
        let Some(nt) = grammar.get(current_nt) else {
            return Err(Error::invalid_event_code("SC", "NonTerminal nicht gefunden für SC-Element"));
        };
        let Some((_sc_prod, event_code)) = nt.find_by_terminal_with_code(&Terminal::SelfContained) else {
            // Keine SC-Production → SC ist nicht verfügbar, normal weiter
            return Ok(());
        };
        let ctx = nt.event_code_context().clone();

        // SC Event-Code encodieren (direkt, nicht über encode_event)
        self.encode_event_code(&event_code, &ctx)?;

        // Schritt 1: State speichern
        // Das Kind-Element (gerade gepusht) wird NICHT mitgespeichert —
        // es gehört zum SC-Fragment.
        let child = self.element_stack.pop().expect("child just pushed");
        let saved = ScEncoderState {
            string_table: std::mem::replace(
                &mut self.string_table,
                self.initial_string_table.clone(),
            ),
            element_grammars: std::mem::take(&mut self.element_grammars),
            document_grammar: self.document_grammar.clone(),
            document_nt: self.document_nt,
            element_stack: std::mem::take(&mut self.element_stack),
            next_local_grammar_id: self.next_local_grammar_id,
            current_element_type: self.current_element_type.take(),
            xsi_nil_active: self.xsi_nil_active,
            xsi_type_seen: self.xsi_type_seen,
            xsi_type_switched_grammar: self.xsi_type_switched_grammar,
            document_in_content: self.document_in_content,
            pending_ch: self.pending_ch.take(), // Im Streaming-Pfad hier immer None
        };
        self.sc_state_stack.push(saved);

        // Grammar-Cache invalidieren: element_grammars wurde geleert (take)
        self.bump_grammar_epoch();

        // Schritt 2+3: State zurücksetzen
        self.next_local_grammar_id = 0;
        self.xsi_nil_active = false;
        self.xsi_type_seen = false;
        self.xsi_type_switched_grammar = false;
        self.document_in_content = false;

        // Schritt 4: Byte-Alignment
        self.writer.align_to_byte();

        // Schritt 5: Fragment Grammar setzen und SD encodieren
        let fragment = self
            .fragment_grammar
            .as_ref()
            .expect("fragment_grammar must exist when self_contained=true")
            .clone();
        self.document_grammar = Rc::new(fragment);
        self.document_nt = self.document_grammar.start(); // Fragment
        {
            // SD [0] auf Fragment encodieren
            let sd_code = EventCode::one(0);
            let fragment_nt = self
                .document_grammar
                .get(self.document_nt)
                .expect("Fragment NT must exist");
            let sd_ctx = fragment_nt.event_code_context().clone();
            self.encode_event_code(&sd_code, &sd_ctx)?;
            // Transition: Fragment → FragmentContent
            self.document_nt = NonTerminalId::FragmentContent;
        }

        // Schritt 6: SE(qname) auf FragmentContent encodieren
        {
            let fc_nt = self
                .document_grammar
                .get(self.document_nt)
                .expect("FragmentContent NT must exist");
            let fc_ctx = fc_nt.event_code_context().clone();

            // Suche SE(qname) in FragmentContent
            let se_terminal = Terminal::se_qname(&qname, &mut self.interner)?;
            if let Some((_prod, event_code)) = fc_nt.find_by_terminal_with_code(&se_terminal) {
                // Schema-informed: SE(qname) gefunden → Event-Code encodieren, kein QName-Content
                self.encode_event_code(&event_code, &fc_ctx)?;
            } else {
                // SE(*) verwenden + QName-Content
                let se_wildcard = Terminal::StartElement(StartElementKind::Wildcard);
                let (_prod, event_code) = fc_nt
                    .find_by_terminal_with_code(&se_wildcard)
                    .ok_or_else(|| Error::invalid_event_code("SE(*) nicht gefunden", "FragmentContent"))?;
                self.encode_event_code(&event_code, &fc_ctx)?;
                // QName-Content encodieren
                self.encode_qname(qname)?;
            }
        }

        // Schritt 7: push_element im SC-Fragment-Kontext
        // Das Kind-Element hat bereits seine Grammar, die wir wiederherstellen können.
        // Aber im SC-Fragment ist der State resetted, also push_element neu aufrufen.
        self.push_element(qname)?;

        // Die Grammar-Transition des Kind-Elements aus dem alten Kontext verwerfen
        // (child war mit der alten Grammar gepusht)
        drop(child);

        Ok(())
    }

    /// Beendet ein Self-Contained Fragment (Spec 8.5.4.4.1).
    ///
    /// 1. ED auf FragmentContent encodieren
    /// 2. Byte-Alignment
    /// 3. State aus sc_state_stack wiederherstellen
    fn end_self_contained(&mut self) -> Result<()> {
        // ED auf FragmentContent encodieren
        {
            let fc_nt = self
                .document_grammar
                .get(NonTerminalId::FragmentContent)
                .expect("FragmentContent NT must exist");
            let fc_ctx = fc_nt.event_code_context().clone();
            let ed_terminal = Terminal::EndDocument;
            let (_prod, event_code) = fc_nt
                .find_by_terminal_with_code(&ed_terminal)
                .ok_or_else(|| Error::invalid_event_code("ED nicht gefunden", "FragmentContent"))?;
            self.encode_event_code(&event_code, &fc_ctx)?;
        }

        // Byte-Alignment
        self.writer.align_to_byte();

        // State wiederherstellen
        let saved = self
            .sc_state_stack
            .pop()
            .expect("SC state stack must not be empty");
        self.string_table = saved.string_table;
        self.element_grammars = saved.element_grammars;
        self.document_grammar = saved.document_grammar;
        self.document_nt = saved.document_nt;
        self.element_stack = saved.element_stack;
        self.next_local_grammar_id = saved.next_local_grammar_id;
        self.current_element_type = saved.current_element_type;
        self.xsi_nil_active = saved.xsi_nil_active;
        self.xsi_type_seen = saved.xsi_type_seen;
        self.xsi_type_switched_grammar = saved.xsi_type_switched_grammar;
        self.document_in_content = saved.document_in_content;
        self.pending_ch = saved.pending_ch;

        // Grammar-Cache invalidieren: element_grammars wurde restored
        self.bump_grammar_epoch();

        // Das triggernde Element wurde bereits in start_self_contained vom
        // äußeren Stack entfernt (pop vor save). Kein zusätzlicher Pop nötig.
        // xsi-Flags auf aktuelles Top-Element setzen
        self.xsi_nil_active = false;
        self.xsi_type_seen = false;
        self.current_element_type = self
            .element_stack
            .last()
            .and_then(|elem| elem.element_type.clone());

        Ok(())
    }

    /// Pusht ein neues Element auf den Stack.
    ///
    /// Bei Schema-informed Encoding wird die Schema-informed Element Grammar
    /// verwendet, falls das Element im Schema deklariert ist.
    fn push_element(&mut self, qname: &QName) -> Result<()> {
        #[allow(unused_assignments)]
        let mut local_decl_owned: Option<ElementDeclaration> = None;
        let mut local_decl: Option<&ElementDeclaration> = None;
        let mut global_decl: Option<Rc<ElementDeclaration>> = None;

        // Parent-Grammar 1× via Cache holen (statt 2 rohe HashMap-Lookups).
        // WICHTIG: parent_grammar wird VOR jeder Grammar-Mutation (insert/bump_grammar_epoch)
        // verwendet — die Rc-Snapshot-Semantik ist hier korrekt.
        let parent_key = self.element_stack.last().map(|e| e.grammar_key);
        let parent_grammar = parent_key.and_then(|k| self.get_element_grammar(&k));

        let parent_in_element_fragment = parent_grammar
            .as_ref()
            .is_some_and(|g| g.grammar_type() == GrammarType::ElementFragment);
        let allow_element_fragment =
            self.element_stack.is_empty() || parent_in_element_fragment;

        if allow_element_fragment
            && self.options.fragment
            && let Some(ref schema) = self.schema
            && schema.is_element_fragment_relaxed_element(qname)
        {
            let expanded = self.interner.intern_expanded(&qname.uri, &qname.local_name)?;
            let local_id = self.next_local_grammar_id;
            self.next_local_grammar_id = self.next_local_grammar_id.wrapping_add(1);
            let key = GrammarKey {
                qname: expanded,
                local_id: Some(local_id),
            };
            let mut elem_grammar = GrammarSystem::schema_informed_element_fragment(schema, &mut self.interner)?;
            elem_grammar.augment_element_fragment(&self.options, &mut self.interner)?;
            elem_grammar.prune(&self.options)?;
            let start = elem_grammar.start();
            self.element_grammars.insert(key, Rc::new(elem_grammar));
            self.bump_grammar_epoch();

            self.current_element_type = None;
            self.element_stack.push(ElementContext {
                expanded_name: expanded,
                element_type: None,
                // Spec 8.5.3: Element Fragment Grammar wird wie nillable=true
                // und has_named_sub_types=true behandelt (für Undeclared Productions).
                is_nillable: true,
                needs_xsi_type: true,
                grammar_key: key,
                current_nt: start,
                in_content: false,
                ns_bindings: None,
            });
            return Ok(());
        }

        let expanded = self.interner.intern_expanded(&qname.uri, &qname.local_name)?;

        if let Some(ref schema) = self.schema {
            // O(1) Side-Table Lookup via gecachte Parent-Grammar (kein .cloned()!)
            if let Some(ref pg) = parent_grammar {
                local_decl = pg.se_element_decl(expanded);
            }
            // Fallback auf O(n) Traversal wenn Side-Table leer oder kein Parent-Grammar
            // verfügbar (Built-in Grammars, Simple-Type Content, Root-Element)
            if local_decl.is_none() {
                if let Some(parent_type) = self.current_element_type.as_ref() {
                    local_decl_owned = crate::schema::find_element_decl_in_type(parent_type, qname);
                    local_decl = local_decl_owned.as_ref();
                }
            }
            // If the local declaration has no type (likely a ref placeholder),
            // fall back to the global declaration.
            if local_decl.is_some_and(|d| d.type_definition.is_none()) {
                local_decl = None;
            }
            // Spec 8.5.4.1.8: Substitution Group Members (z.B. SE(Qz) im Slot
            // von SE(Qb)) werden via matching_qnames() gefunden, aber die
            // zurückgegebene Deklaration ist die des Group-Heads (Qb mit Typ tQb).
            // Das Element selbst (Qz) hat seinen eigenen Typ (tQz), daher muss
            // die globale Deklaration verwendet werden, um den korrekten Typ,
            // has_named_sub_types und nillable zu erhalten.
            if local_decl.is_some_and(|d| d.qname.as_ref() != qname) {
                local_decl = None;
            }
            if local_decl.is_none() {
                global_decl = schema.get_element(qname).cloned();
            }
        }
        let (grammar_key, start_nt) = if let Some(local_decl) = local_decl {
            let local_id = self.next_local_grammar_id;
            self.next_local_grammar_id = self.next_local_grammar_id.wrapping_add(1);
            let key = GrammarKey {
                qname: expanded,
                local_id: Some(local_id),
            };
            let template = cached_schema_grammar(&mut self.grammar_template_cache, local_decl, &self.options, &mut self.interner)?;
            let start = template.start();
            self.element_grammars.insert(key, template);
            self.bump_grammar_epoch();
            (key, start)
        } else {
            let key = GrammarKey {
                qname: expanded,
                local_id: None,
            };
            if let Some(existing) = self.element_grammars.get(&key) {
                (key, existing.start())
            } else {
                let grammar_arc = if let Some(elem_decl) = global_decl.as_ref() {
                    cached_schema_grammar(&mut self.grammar_template_cache, elem_decl, &self.options, &mut self.interner)?
                } else {
                    let mut grammar = GrammarSystem::built_in_element(&self.options);
                    grammar.prune(&self.options)?;
                    Rc::new(grammar)
                };
                let start = grammar_arc.start();
                self.element_grammars.insert(key, grammar_arc);
                self.bump_grammar_epoch();
                (key, start)
            }
        };

        // Bei Schema-informed: Element-Typ für Typed Value Encoding holen
        // (nur Rc-Clone für type_definition statt vollem Struct-Clone)
        let (elem_type, is_nillable) = if let Some(decl) = local_decl {
            (decl.type_definition.clone(), decl.nillable)
        } else if let Some(ref gd) = global_decl {
            (gd.type_definition.clone(), gd.nillable)
        } else {
            (None, false)
        };
        let needs_xsi_type = elem_type
            .as_ref()
            .is_some_and(|td| td.has_named_sub_types() || td.is_union());
        self.current_element_type = elem_type.clone();

        self.element_stack.push(ElementContext {
            expanded_name: expanded,
            element_type: elem_type,
            is_nillable,
            needs_xsi_type,
            grammar_key,
            current_nt: start_nt,
            in_content: false,
            ns_bindings: None,
        });

        Ok(())
    }

    /// Gibt den QName des aktuellen Elements zurück (materialisiert aus Interner).
    fn current_element_qname(&self) -> Result<QName> {
        self.element_stack
            .last()
            .map(|e| e.expanded_name.to_qname_shared(&self.interner))
            .ok_or_else(|| Error::ordering_violation("Element auf Stack", "leerer Stack"))
    }

    /// Lookup Production in Document Grammar (mit Wildcard-Info).
    ///
    /// Returns: (event_code, ctx, next_nt, was_wildcard)
    fn lookup_document_event_code(
        &self,
        terminal: &Terminal,
    ) -> Result<(
        EventCode,
        EventCodeContext,
        Option<NonTerminalId>,
        bool,
        Option<Terminal>,
    )> {
        if self.trace.lookup {
            eprintln!(
                "lookup_document: nt={:?} terminal={:?} grammar={:?}",
                self.document_nt,
                terminal,
                self.document_grammar.grammar_type()
            );
        }
        let nt = self
            .document_grammar
            .get(self.document_nt)
            .ok_or_else(|| Error::UnknownNonTerminal(format!("{:?}", self.document_nt)))?;
        let is_schema_informed = self.document_grammar.grammar_type() == GrammarType::ElementFragment;
        let base_ctx = nt.event_code_context();

        let tier1_result =
            self.find_production_with_wildcard(&self.document_grammar, self.document_nt, terminal);

        match tier1_result {
            Ok((production, event_code, was_wildcard)) => {
                let matched_terminal = if was_wildcard {
                    Some(production.terminal.clone())
                } else {
                    None
                };

                let ctx = if is_schema_informed {
                    let tier2_ctx = self.create_document_tier2_context(nt)?;
                    let t2_count = tier2_ctx.count();
                    if t2_count > 0 {
                        let escape_part1 = base_ctx.num_values_part1();
                        base_ctx
                            .clone()
                            .with_second_level(true)
                            .with_part2_count(escape_part1, t2_count)
                    } else {
                        base_ctx.clone()
                    }
                } else {
                    base_ctx.clone()
                };

                Ok((event_code, ctx, production.right_hand_side, was_wildcard, matched_terminal))
            }
            Err(_) if is_schema_informed => {
                let tier2_ctx = self.create_document_tier2_context(nt)?;
                let (part2, part3) = tier2_ctx.find_with_subgroup(terminal)
                    .ok_or_else(|| Error::invalid_event_code("Tier2 nicht gefunden", "Document-Grammar"))?;

                let escape_part1 = base_ctx.num_values_part1();
                let t2_count = tier2_ctx.count();

                // Spec 8.5.4.4.1: CM/PI Sub-Gruppe (3-Teil-Code)
                let (event_code, ctx) = if let Some(p3) = part3 {
                    let cm_pi_count = tier2_ctx.cm_pi_count();
                    let ec = EventCode::three(escape_part1, part2, p3);
                    let c = base_ctx
                        .clone()
                        .with_second_level(true)
                        .with_part2_count(escape_part1, t2_count)
                        .with_part3_count(escape_part1, part2, cm_pi_count);
                    (ec, c)
                } else {
                    let ec = EventCode::two(escape_part1, part2);
                    let c = base_ctx
                        .clone()
                        .with_second_level(true)
                        .with_part2_count(escape_part1, t2_count);
                    (ec, c)
                };

                let rhs = match terminal {
                    Terminal::EndElement => None,
                    Terminal::StartElement(_) | Terminal::Characters | Terminal::CharactersUntyped => {
                        self.document_grammar.find_content_nonterminal(self.document_nt)
                            .or(Some(self.document_nt))
                    }
                    _ => Some(self.document_nt),
                };

                let was_wildcard = matches!(
                    terminal,
                    Terminal::StartElement(StartElementKind::QName(_))
                        | Terminal::Attribute(AttributeKind::QName(_) | AttributeKind::QNameUntyped(_))
                );

                // Tier-2 Wildcard: immer SE(*)/AT(*), nie SE(uri:*)
                Ok((event_code, ctx, rhs, was_wildcard, None))
            }
            Err(e) => Err(e),
        }
    }

    /// Lookup Production in Element Grammar (mit Wildcard-Info und Tier-2 Support).
    ///
    /// Bei Schema-informed Grammars wird das 2-Tier System unterstützt:
    /// - Tier 1: Schema-deklarierte Productions (aus der Grammar)
    /// - Tier 2: Undeclared Productions (dynamisch generiert, Spec 8.5.4.4)
    ///
    /// Returns: (event_code, ctx, next_nt, was_wildcard, matched_terminal)
    /// matched_terminal ist das Terminal der gematchten Production bei Wildcard-Match,
    /// damit encode_event_content weiß ob SE(*) oder SE(uri:*) vorliegt.
    fn lookup_element_event_code_with_wildcard(
        &mut self,
        terminal: &Terminal,
    ) -> Result<(
        EventCode,
        EventCodeContext,
        Option<NonTerminalId>,
        bool,
        Option<Terminal>,
    )> {
        let (grammar_key, current_nt) = {
            let elem = self
                .element_stack
                .last()
                .ok_or_else(|| Error::invalid_event_code("", "leerer Element-Stack"))?;
            if self.trace.lookup {
                eprintln!("lookup_element: nt={:?} terminal={:?}", elem.current_nt, terminal);
            }
            (elem.grammar_key, elem.current_nt)
        };

        let elem_grammar = self
            .get_element_grammar(&grammar_key)
            .ok_or_else(|| Error::invalid_event_code("", "Grammar nicht gefunden"))?;

        // Prüfe ob Schema-informed Grammar (2-Tier Event-Code-System)
        let is_schema_informed =
            elem_grammar.grammar_type() == GrammarType::SchemaInformedElement;

        // Tier 1 Match + Daten extrahieren (Borrow auf elem_grammar freigeben)
        let tier1_owned = {
            let tier1_result =
                self.find_production_with_wildcard(&elem_grammar, current_nt, terminal);
            tier1_result.map(|(prod, ec, ww)| (prod.terminal.clone(), prod.right_hand_side, ec, ww))
        };

        // Basis-Context + Length1-Context vorab extrahieren (declared_attributes lazy)
        let nt_ref = elem_grammar
            .get(current_nt)
            .ok_or_else(|| {
                Error::UnknownNonTerminal(format!("{:?}", current_nt))
            })?;
        let base_ctx = nt_ref.event_code_context();
        let length1_ctx_precomp = nt_ref.event_code_context_length_one();

        match tier1_owned {
            Ok((prod_terminal, right_hand_side, event_code, was_wildcard)) => {
                let matched_terminal = if was_wildcard {
                    Some(prod_terminal.clone())
                } else {
                    None
                };

                if !self.options.strict && is_schema_informed {
                    // 2-Tier Event-Code-System: Part1-Bitbreite nur aus length-1
                    // Productions, Undeclared Productions als Escape+Part2 kodieren.
                    // Erzeugt in der Praxis kleinere Streams als Flat-Grammar,
                    // weil deklarierte Events (der Normalfall) schmalere Codes bekommen.
                    // Im Strict-Modus nicht verwenden: die Grammar hat bereits
                    // alle nötigen Productions (xsi:type/xsi:nil via
                    // augment_element_grammar_strict()), Tier-2 ist überflüssig
                    // und würde die Bitbreite verfälschen.
                    let tier2_ctx = self.cached_element_tier2_context()?;
                    let t2_count = tier2_ctx.count();

                    // Tier2-Terminal: Bei Characters→CharactersUntyped Fallback
                    // (ElementFragment Content2) muss CharactersUntyped gesucht werden.
                    let tier2_terminal = if matches!(terminal, Terminal::Characters)
                        && matches!(prod_terminal, Terminal::CharactersUntyped)
                    {
                        &Terminal::CharactersUntyped
                    } else {
                        terminal
                    };

                    // Undeclared Productions erkennen: entweder über event_code
                    // Länge > 1 ODER über Terminal-Typ. Bei einfachen Typen wie
                    // anySimpleType vergibt die Grammar-Augmentierung 1-Part-Codes
                    // an NS, obwohl NS immer Tier-2 sein muss.
                    let is_undeclared = event_code.length() > 1
                        || matches!(terminal, Terminal::NamespaceDecl);

                    if is_undeclared && t2_count > 0 {
                        // Undeclared Production: als 2-Tier Event-Code neu kodieren
                        let length1_ctx = length1_ctx_precomp.clone();
                        let escape_part1 = length1_ctx.num_values_part1();
                        let (part2, part3) = tier2_ctx.find_with_subgroup(tier2_terminal)
                            .ok_or_else(|| Error::invalid_event_code("Tier2 nicht gefunden", "Element-Undeclared"))?;

                        // Spec 8.5.4.4.1: AT(*)[untyped value] teilt den Tier-2
                        // Slot mit AT(qname)[untyped value]. Bei deklarierten
                        // Attributen wird ein 3-Part-Code erzeugt, wobei
                        // Part3 = declared_attrs_count (Wildcard-Slot am Ende).
                        if matches!(terminal, Terminal::Attribute(AttributeKind::WildcardUntyped)) {
                            let declared_attrs = elem_grammar.get(current_nt).unwrap().declared_attributes(&self.interner);
                            let declared_attrs_count = declared_attrs.len();
                            if declared_attrs_count > 0 {
                                let part3_count = (declared_attrs_count + 1) as u32;
                                let event_code = EventCode::three(
                                    escape_part1, part2, declared_attrs_count as u32,
                                );
                                let ctx = length1_ctx
                                    .with_second_level(true)
                                    .with_part2_count(escape_part1, t2_count)
                                    .with_part3_count(escape_part1, part2, part3_count);
                                return Ok((event_code, ctx, right_hand_side, was_wildcard, matched_terminal));
                            }
                        }

                        // Spec 8.5.4.4.1: CM/PI Sub-Gruppe (3-Teil-Code)
                        if let Some(p3) = part3 {
                            let cm_pi_count = tier2_ctx.cm_pi_count();
                            let event_code = EventCode::three(
                                escape_part1, part2, p3,
                            );
                            let ctx = length1_ctx
                                .with_second_level(true)
                                .with_part2_count(escape_part1, t2_count)
                                .with_part3_count(escape_part1, part2, cm_pi_count);
                            return Ok((event_code, ctx, right_hand_side, was_wildcard, matched_terminal));
                        }

                        let event_code =
                            EventCode::two(escape_part1, part2);
                        let ctx = length1_ctx
                            .with_second_level(true)
                            .with_part2_count(escape_part1, t2_count);
                        Ok((event_code, ctx, right_hand_side, was_wildcard, matched_terminal))
                    } else if t2_count > 0 {
                        // Tier-1 Production mit Tier-2 vorhanden: Context anpassen
                        let length1_ctx = length1_ctx_precomp.clone();
                        let escape_part1 = length1_ctx.num_values_part1();
                        let ctx = length1_ctx
                            .with_second_level(true)
                            .with_part2_count(escape_part1, t2_count);
                        Ok((event_code, ctx, right_hand_side, was_wildcard, matched_terminal))
                    } else {
                        // Kein Tier-2: Flat-Grammar verwenden
                        Ok((event_code, base_ctx.clone(), right_hand_side, was_wildcard, matched_terminal))
                    }
                } else {
                    // Spec-konform: Flat-Grammar Event-Codes (Undeclared Productions
                    // direkt in der Grammar eingebettet, Spec 8.5.4.4).
                    Ok((event_code, base_ctx.clone(), right_hand_side, was_wildcard, matched_terminal))
                }
            }
            Err(_) if is_schema_informed && !self.options.strict => {
                // AT(QNameUntyped) hat keine eigene Production in der Grammar.
                // Stattdessen wird ein 3-Part Event-Code erzeugt:
                //   [escape, tier2_untyped_idx, attr_idx]
                // Spec 8.5.4.4.1: AT(qname)[untyped] n.(m+1).attr_idx
                if let Terminal::Attribute(AttributeKind::QNameUntyped(target_qname)) = terminal {
                    let length1_ctx = length1_ctx_precomp.clone();
                    let escape_part1 = length1_ctx.num_values_part1();

                    let tier2_ctx = self.cached_element_tier2_context()?;
                    let t2_count = tier2_ctx.count();

                    // Tier2-Index von AT(*)[untyped]
                    let tier2_untyped_idx = tier2_ctx.find(
                        &Terminal::Attribute(AttributeKind::WildcardUntyped),
                    ).ok_or_else(|| Error::invalid_event_code("AT(*)[untyped] fehlt", "Tier2"))?;

                    let declared_attrs = elem_grammar.get(current_nt).unwrap().declared_attributes(&self.interner);
                    let attr_idx = declared_attrs.iter().position(|(q, _)| *q == *target_qname)
                        .ok_or_else(|| Error::invalid_event_code("AT(qname) nicht deklariert", "Element-Grammar"))?;

                    let part3_count = (declared_attrs.len() + 1) as u32; // +1 für AT(*)[untyped]
                    let event_code = EventCode::three(
                        escape_part1, tier2_untyped_idx, attr_idx as u32,
                    );
                    let ctx = length1_ctx
                        .with_second_level(true)
                        .with_part2_count(escape_part1, t2_count)
                        .with_part3_count(escape_part1, tier2_untyped_idx, part3_count);
                    let rhs = declared_attrs[attr_idx].1;
                    Ok((event_code, ctx, rhs, false, None))
                } else {
                    Err(Error::invalid_event_code("kein AT(QNameUntyped)", "Schema-Element"))
                }
            }
            Err(_) if is_schema_informed => {
                let detail = format!("Production nicht gefunden ({terminal:?})");
                Err(Error::invalid_event_code(detail, "Schema-Element"))
            }
            Err(e) => Err(e),
        }
    }

    fn create_document_tier2_context(
        &self,
        nt: &crate::grammar::NonTerminal,
    ) -> Result<Tier2Context> {
        let has_ee_in_tier1 = nt.has_ee_length_one();

        let is_first_nt = matches!(
            self.document_nt,
            NonTerminalId::ElementFragment0 | NonTerminalId::ElementFragmentTypeEmpty0
        );

        Ok(Tier2Context {
            has_ee_in_tier1,
            is_first_nt,
            is_content_area: self.document_in_content,
            // Spec 8.5.3: Element Fragment Grammar wird wie nillable=true
            // und has_named_sub_types=true behandelt.
            needs_xsi_type: true,
            is_nillable: true,
            is_element_fragment: self.document_grammar.grammar_type() == GrammarType::ElementFragment,
            xsi_type_id: self.xsi_type_id,
            xsi_nil_id: self.xsi_nil_id,
            ..Tier2Context::new(&self.options)
        })
    }

    /// Cached Version von `create_element_tier2_context()`.
    ///
    /// Prüft den Direct-Mapped Cache auf Hit anhand von
    /// (grammar_key, current_nt, in_content, needs_xsi_type, is_nillable, epoch).
    fn cached_element_tier2_context(&mut self) -> Result<Tier2Context> {
        let elem = self
            .element_stack
            .last()
            .ok_or_else(|| Error::invalid_event_code("", "leerer Element-Stack"))?;
        let slot = {
            let nt_bits = compact_nt_bits(elem.current_nt);
            let h = elem.grammar_key.qname.uri.0
                ^ elem.grammar_key.qname.local_name.0
                ^ nt_bits;
            h as usize % ENCODER_TIER2_CACHE_SLOTS
        };
        if let Some(ref cache) = self.tier2_cache[slot] {
            if cache.grammar_key == elem.grammar_key
                && cache.current_nt == elem.current_nt
                && cache.in_content == elem.in_content
                && cache.needs_xsi_type == elem.needs_xsi_type
                && cache.is_nillable == elem.is_nillable
                && cache.epoch == self.grammar_epoch
            {
                return Ok(cache.context);
            }
        }
        let context = self.create_element_tier2_context()?;
        let elem = self.element_stack.last().unwrap();
        self.tier2_cache[slot] = Some(EncoderTier2Cache {
            grammar_key: elem.grammar_key,
            current_nt: elem.current_nt,
            in_content: elem.in_content,
            needs_xsi_type: elem.needs_xsi_type,
            is_nillable: elem.is_nillable,
            epoch: self.grammar_epoch,
            context,
        });
        Ok(context)
    }

    /// Erzeugt Tier2Context für Element-Grammars (2-Tier Encoding).
    fn create_element_tier2_context(&self) -> Result<Tier2Context> {
        let elem = self
            .element_stack
            .last()
            .ok_or_else(|| Error::invalid_event_code("", "leerer Element-Stack"))?;
        let elem_grammar = self
            .element_grammars
            .get(&elem.grammar_key)
            .ok_or_else(|| Error::invalid_event_code("", "Grammar nicht gefunden"))?;
        let nt = elem_grammar
            .get(elem.current_nt)
            .ok_or_else(|| {
                Error::UnknownNonTerminal(format!("{:?}", elem.current_nt))
            })?;

        let has_ee_in_tier1 = nt.has_ee_length_one();

        let is_element_fragment = elem_grammar.grammar_type() == GrammarType::ElementFragment;
        let is_first_nt = matches!(
            elem.current_nt,
            NonTerminalId::SchemaType(0)
                | NonTerminalId::ElementFragment0
                | NonTerminalId::ElementFragmentTypeEmpty0
        ) || Some(elem.current_nt) == elem_grammar.type_empty_start();

        let is_content_area = nt.is_content_area() || elem.in_content;

        Ok(Tier2Context {
            has_ee_in_tier1,
            is_first_nt,
            is_content_area,
            needs_xsi_type: elem.needs_xsi_type,
            is_nillable: elem.is_nillable,
            is_element_fragment,
            xsi_type_id: self.xsi_type_id,
            xsi_nil_id: self.xsi_nil_id,
            ..Tier2Context::new(&self.options)
        })
    }

    /// Prüft ob das aktuelle Element eine Built-in Element Grammar hat.
    ///
    /// Bei Schema-informed Mode soll Grammar Evolution nur für Built-in Element
    /// Grammars durchgeführt werden (nicht für Schema-informed Grammars).
    /// Gibt `true` zurück wenn Evolution erlaubt ist, `false` wenn nicht.
    fn is_built_in_element_grammar(&self) -> bool {
        if self.schema.is_none() {
            return true;
        }
        let Some(elem) = self.element_stack.last() else {
            return false;
        };
        self.element_grammars
            .get(&elem.grammar_key)
            .is_some_and(|g| g.grammar_type() == crate::grammar::GrammarType::Element)
    }

    /// Grammar Evolution bei CH/EE mit Event Code Länge > 1 (Spec 8.4.3).
    ///
    /// "If the matched terminal symbol is CH [...] or EE and the event code is more than
    /// one part, the associated grammar is augmented as follows:
    /// Add a production for CH [...] or EE with an event code of 0 (zero)..."
    fn evolve_content_grammar(
        &mut self,
        terminal: &Terminal,
        next_nt: Option<NonTerminalId>,
    ) -> Result<()> {
        if !self.is_built_in_element_grammar() {
            return Ok(());
        }

        let elem = self
            .element_stack
            .last()
            .ok_or_else(|| Error::invalid_event_code("", "leerer Element-Stack"))?;

        let elem_grammar_arc = self
            .element_grammars
            .get_mut(&elem.grammar_key)
            .ok_or_else(|| Error::invalid_event_code("", "Grammar nicht gefunden"))?;
        let current_nt = elem.current_nt;

        let nt = Rc::make_mut(elem_grammar_arc)
            .get_mut(current_nt)
            .ok_or_else(|| Error::UnknownNonTerminal(format!("{:?}", current_nt)))?;

        GrammarEvolution::learn_content(nt, terminal.clone(), next_nt);
        self.bump_grammar_epoch();
        Ok(())
    }

    /// Grammar Evolution bei Wildcard-Match (Spec 8.4.3).
    ///
    /// - SE(*) Match → lernt SE(qname) im aktuellen NonTerminal
    /// - AT(*) Match → lernt AT(qname)
    ///
    /// Nutzt `event.qname()` für Arc-freien Zugriff auf den QName.
    fn evolve_element_grammar_ref(&mut self, event: &EventRef<'_>, terminal: &Terminal) -> Result<()> {
        if !self.is_built_in_element_grammar() {
            return Ok(());
        }

        let elem = self
            .element_stack
            .last()
            .ok_or_else(|| Error::invalid_event_code("", "leerer Element-Stack"))?;

        let elem_grammar = Rc::make_mut(
            self.element_grammars
                .get_mut(&elem.grammar_key)
                .ok_or_else(|| Error::invalid_event_code("", "Grammar nicht gefunden"))?,
        );
        let current_nt = elem.current_nt;

        // QName aus Terminal oder Event extrahieren.
        // Bei schema-informed Mode kann terminal=Attribute(Wildcard) sein, obwohl
        // das Event einen konkreten QName hat. In dem Fall den QName aus dem Event nehmen.
        match terminal {
            Terminal::StartElement(StartElementKind::QName(_)) => {
                // SE(*) → SE(qname) nur im aktuellen NonTerminal lernen (Spec 8.4.3)
                let nt = elem_grammar.get_mut(current_nt).ok_or_else(|| {
                    Error::UnknownNonTerminal(format!("{:?}", current_nt))
                })?;
                GrammarEvolution::learn_element_se(nt, *terminal);
            }
            Terminal::Attribute(AttributeKind::QName(qname_id)) => {
                // Spec 8.5.4.4.1 (Z.3134): xsi:type und xsi:nil dürfen in
                // schema-informed Grammars NICHT über AT(*) repräsentiert werden.
                // Learning nach AT(*)-Match wird daher unterdrückt.
                if self.schema.is_some()
                    && (*qname_id == self.xsi_type_id || *qname_id == self.xsi_nil_id)
                {
                    return Ok(());
                }
                // AT(*) → AT(qname) nur in StartTagContent lernen
                // (Attribute kommen nur in StartTagContent vor)
                let stc = elem_grammar
                    .get_mut(NonTerminalId::StartTagContent)
                    .ok_or_else(|| {
                        Error::UnknownNonTerminal("StartTagContent".to_string())
                    })?;
                GrammarEvolution::learn_attribute(stc, *terminal, &mut self.interner);
            }
            Terminal::Attribute(
                AttributeKind::Wildcard
                | AttributeKind::WildcardUntyped
                | AttributeKind::NamespaceWildcard(_),
            ) => {
                // Terminal ist bereits Wildcard (schema-informed: undeclared attribute).
                // QName aus dem Event extrahieren für Grammar Evolution.
                if let Some(qname) = event.qname() {
                    if self.schema.is_some()
                        && (qname.is_xsi_type() || qname.is_xsi_nil())
                    {
                        return Ok(());
                    }
                    let Some(learned_terminal) =
                        terminal.learned_from_wildcard(qname, &mut self.interner)? else {
                            return Ok(());
                        };
                    let stc = elem_grammar
                        .get_mut(NonTerminalId::StartTagContent)
                        .ok_or_else(|| {
                            Error::UnknownNonTerminal(
                                "StartTagContent".to_string(),
                            )
                        })?;
                    GrammarEvolution::learn_attribute(stc, learned_terminal, &mut self.interner);
                }
            }
            Terminal::StartElement(
                StartElementKind::Wildcard | StartElementKind::NamespaceWildcard(_),
            ) => {
                // Terminal ist SE(*)/SE(uri:*): QName aus dem Event extrahieren.
                if let Some(qname) = event.qname() {
                    let Some(learned_terminal) =
                        terminal.learned_from_wildcard(qname, &mut self.interner)? else {
                            return Ok(());
                        };
                    let nt = elem_grammar.get_mut(current_nt).ok_or_else(|| {
                        Error::UnknownNonTerminal(format!("{:?}", current_nt))
                    })?;
                    GrammarEvolution::learn_element_se(nt, learned_terminal);
                }
            }
            _ => {
                // Andere Wildcards (CH, etc.) — kein Learning nötig
            }
        }

        self.bump_grammar_epoch();
        Ok(())
    }

    /// Encodiert einen Event Code basierend auf Alignment-Modus.
    ///
    /// Bei ByteAlignment/PreCompression wird vor dem Event Code auf Byte-Grenze
    /// ausgerichtet (Spec 6.2, symmetrisch zum Decoder).
    fn encode_event_code(
        &mut self,
        event_code: &EventCode,
        ctx: &EventCodeContext,
    ) -> Result<()> {
        // Spec 6.2: Bei compression=true wird byte-aligned encoding verwendet
        match self.options.effective_alignment() {
            Alignment::BitPacked => {
                encode_bit_packed(&mut self.writer, event_code, ctx)?;
            }
            Alignment::ByteAlignment | Alignment::PreCompression => {
                self.writer.align_to_byte();
                encode_byte_aligned(&mut self.writer, event_code, ctx)?;
            }
        }
        Ok(())
    }

    /// Encodiert einen n-bit Wert unter Berücksichtigung des Alignment-Modus.
    ///
    /// Bei BitPacked: n Bits werden geschrieben.
    /// Bei ByteAlignment/PreCompression: ceil(n/8) Bytes werden geschrieben (Spec 7.1.9).
    fn encode_n_bit(&mut self, value: u64, n: u8) {
        // Spec 6.2: Bei compression=true wird byte-aligned encoding verwendet
        match self.options.effective_alignment() {
            Alignment::BitPacked => {
                crate::n_bit_unsigned_integer::encode(&mut self.writer, value, n);
            }
            Alignment::ByteAlignment | Alignment::PreCompression => {
                // Bei byte-alignment: minimale Anzahl Bytes verwenden
                let num_bytes = n.div_ceil(8);
                for i in 0..num_bytes {
                    let byte = ((value >> (i * 8)) & 0xFF) as u8;
                    self.writer.write_byte_aligned(byte);
                }
            }
        }
    }



















    /// Behandelt xsi:nil="true" (Spec 8.5.4.4.2,
    /// spec/exi-spec.txt lines 3315-3337).
    ///
    /// Bei xsi:nil="true" oder xsi:nil="1" wird das Flag gesetzt, das bewirkt
    /// dass nur noch EE (EndElement) erlaubt ist.
    ///
    /// # Errors
    ///
    /// `Error::XsiTypeAndNilTogether` wenn xsi:type bereits auf diesem Element gesetzt wurde.
    fn handle_xsi_nil(&mut self, value: &str) -> Result<()> {
        // Spec 8.5.4.4.2 Note (exi-spec.txt Zeile 3344-3346):
        // xsi:type und xsi:nil zusammen sind nur bei strict=true verboten.
        // Bei strict=false hat die Grammar Productions für beide.
        if self.options.strict && self.xsi_type_seen {
            return Err(Error::XsiTypeAndNilTogether);
        }

        if value == "true" || value == "1" {
            self.xsi_nil_active = true;
        }
        Ok(())
    }

    fn switch_current_element_to_type_empty(&mut self) {
        let (grammar_key, current_nt) = match self.element_stack.last() {
            Some(elem) => (elem.grammar_key, elem.current_nt),
            None => return,
        };
        let (type_empty_start, has_candidate) = match self.element_grammars.get(&grammar_key) {
            Some(grammar) => {
                let start = grammar.type_empty_start();
                let candidate = if let (
                    Some(NonTerminalId::SchemaType(empty_offset)),
                    NonTerminalId::SchemaType(current_idx),
                ) = (start, current_nt)
                {
                    let candidate = NonTerminalId::SchemaType(empty_offset + current_idx);
                    grammar.get(candidate).is_some().then_some(candidate)
                } else {
                    None
                };
                (start, candidate)
            }
            None => (None, None),
        };

        if let Some(elem) = self.element_stack.last_mut() {
            if let Some(candidate) = has_candidate {
                elem.current_nt = candidate;
                return;
            }
            if let Some(start) = type_empty_start {
                elem.current_nt = start;
                return;
            }
            // Simple-type elements don't have a TypeEmpty grammar; jump to EE state.
            // Spec 8.5.4.1.3.1: TypeEmpty_i has only EE (spec/exi-spec.txt lines 2411-2424).
            if matches!(
                elem.element_type.as_ref().map(|t| t.as_ref()),
                Some(crate::schema::TypeDefinition::Simple { .. })
            ) {
                let candidate = NonTerminalId::SchemaType(1);
                if let Some(grammar) = self.element_grammars.get(&grammar_key)
                    && grammar.get(candidate).is_some() {
                        elem.current_nt = candidate;
                    }
            }
        }
    }

    /// Behandelt xsi:type Attribut (Spec 8.5.4.4).
    ///
    /// Sucht den Typ im Schema und wechselt `current_element_type`.
    /// Bei `strict=true` und nicht gefundenem Typ wird ein Fehler zurückgegeben.
    ///
    /// # Errors
    ///
    /// - `Error::XsiTypeAndNilTogether` wenn xsi:nil bereits auf diesem Element gesetzt wurde.
    /// - `Error::XsiTypeNotFound` wenn strict=true und der Typ nicht im Schema ist.
    fn handle_xsi_type(&mut self, value: &str) -> Result<()> {
        // Spec 8.5.4.4.2 Note (exi-spec.txt Zeile 3344-3346):
        // xsi:type und xsi:nil zusammen sind nur bei strict=true verboten.
        if self.options.strict && self.xsi_nil_active {
            return Err(Error::XsiTypeAndNilTogether);
        }

        self.xsi_type_seen = true;

        let schema = match &self.schema {
            Some(s) => s.clone(),
            None => return Ok(()), // Kein Schema, nichts zu tun
        };

        // Parse QName: "{URI}localname", "prefix:localname" oder "localname"
        let (type_uri, type_local) = if value.starts_with('{') {
            if let Some(end) = value.find('}') {
                (Some(&value[1..end]), &value[end + 1..])
            } else {
                (None, value)
            }
        } else if let Some(pos) = value.find(':') {
            (None, &value[pos + 1..])
        } else {
            (None, value)
        };

        // Suche in type_definitions nach passendem Namen (mit URI wenn verfuegbar)
        for (type_qname, type_def) in schema.type_definitions() {
            if &*type_qname.local_name == type_local
                && (type_uri.is_none() || type_uri == Some(&*type_qname.uri))
            {
                self.current_element_type = Some(type_def.clone());
                let prev_key =
                    self.element_stack.last().map(|e| e.grammar_key);
                self.switch_current_element_grammar(type_def.clone())?;
                if let (Some(prev), Some(cur)) =
                    (prev_key, self.element_stack.last().map(|e| e.grammar_key))
                {
                    if self.trace.lookup {
                        eprintln!("xsi:type switch: prev={prev:?} new={cur:?}");
                    }
                    if prev != cur {
                        self.xsi_type_switched_grammar = true;
                    }
                }
                return Ok(());
            }
        }

        // Typ nicht im Schema: XSD-Built-in-Types als Fallback (analog zum Decoder)
        let is_xsd_namespace = type_uri == Some("http://www.w3.org/2001/XMLSchema");
        if is_xsd_namespace {
            // anyType ist ein Complex Type mit Mixed Content und Wildcards (Spec 8.5.4.1.3.2).
            // Alle anderen XSD-Built-in-Types sind Simple Types.
            let type_def = if type_local == "anyType" {
                Rc::new(TypeDefinition::Complex {
                    name: None,
                    base_type: None,
                    derivation: None,
                    attributes: Vec::new(),
                    attribute_wildcard: Some(crate::schema::AttributeWildcard::Any),
                    content: crate::schema::ContentType::Mixed(
                        crate::schema::Particle::new(
                            0,
                            crate::schema::MaxOccurs::Unbounded,
                            crate::schema::ParticleTerm::Wildcard(
                                crate::schema::Wildcard::any(),
                            ),
                        )?,
                    ),
                    has_named_sub_types: true,
                })
            } else {
                Rc::new(TypeDefinition::simple_with_base(type_local))
            };
            self.current_element_type = Some(type_def.clone());
            self.switch_current_element_grammar(type_def)?;
            self.xsi_type_switched_grammar = true;
            return Ok(());
        }

        // Typ nicht gefunden — Grammar bleibt unverändert (Spec 8.5.4.4).
        // Auch bei strict=true kein Fehler: der xsi:type-Wert wird als String
        // encodiert, der Grammar-Switch wird übersprungen. Exificient verhält
        // sich ebenso (complexUrType-04/05 Interop-Tests).
        Ok(())
    }

    /// Wechselt die aktuelle Element-Grammar auf den per xsi:type angegebenen Typ.
    ///
    /// Spec 8.5.4.4: Nach AT(xsi:type) wird die Grammar des angegebenen Typs verwendet.
    fn switch_current_element_grammar(&mut self, type_def: Rc<TypeDefinition>) -> Result<()> {
        let elem = match self.element_stack.last_mut() {
            Some(e) => e,
            None => return Ok(()),
        };

        // Bei schema-informed Streams: auch aus Built-in Element Grammars umschalten,
        // wenn ein Typ via xsi:type gefunden wurde (Spec 8.4.3/8.5.4.4).
        let current_grammar = self.element_grammars.get(&elem.grammar_key);
        let current_type = current_grammar.map(|g| g.grammar_type());
        if matches!(
            current_type,
            Some(GrammarType::SchemaInformedElement)
                | Some(GrammarType::Element)
                | Some(GrammarType::ElementFragment)
        ) && self.schema.is_some()
        {
            // QName einmal materialisieren (shared Arcs, keine Heap-Allokation)
            let elem_qname = elem.expanded_name.to_qname_shared(&self.interner);

            // Beim Switch von ElementFragment: is_nillable aus dem Schema
            // nachschlagen. ElementFragment setzt is_nillable=true für alle
            // Elemente (Spec 8.5.3), aber nach dem xsi:type-Switch brauchen
            // wir die echte Nillability, damit die Grammar korrekt aufgebaut
            // wird (mit/ohne AT(xsi:nil) Production).
            if matches!(current_type, Some(GrammarType::ElementFragment))
                && let Some(ref schema) = self.schema
            {
                elem.is_nillable = schema
                    .get_element(&elem_qname)
                    .map_or(false, |d| d.nillable);
            }
            let elem_decl = crate::schema::ElementDeclaration::new(Rc::new(elem_qname))
                .with_nillable(elem.is_nillable)
                .with_type(type_def.clone());

            let local_id = self.next_local_grammar_id;
            self.next_local_grammar_id = self.next_local_grammar_id.wrapping_add(1);
            let key = GrammarKey {
                qname: elem.expanded_name,
                local_id: Some(local_id),
            };
            let mut grammar_arc = cached_schema_grammar(&mut self.grammar_template_cache, &elem_decl, &self.options, &mut self.interner)?;
            // Spec 8.5.4.4.2: Nach xsi:type-Switch wird ein TYPE-Grammar
            // verwendet. In strict Mode haben Type-Grammars KEINE
            // xsi:type/xsi:nil-Augmentierung (Spec definiert diese nur
            // für "each normalized element grammar Element_i").
            if self.options.strict {
                Rc::make_mut(&mut grammar_arc).strip_xsi_type_nil_from_start(&self.interner);
            }
            let start = grammar_arc.start();

            self.element_grammars.insert(key, grammar_arc);
            // Inline statt self.bump_grammar_epoch() wegen aktiver &mut elem Borrow.
            self.grammar_epoch = self.grammar_epoch.wrapping_add(1);
            elem.grammar_key = key;
            elem.current_nt = start;
            elem.element_type = Some(type_def.clone());
            elem.needs_xsi_type = type_def.has_named_sub_types() || type_def.is_union();

            // current_element_type aktualisieren, damit push_element() für
            // Kind-Elemente die lokalen Deklarationen im neuen Typ findet.
            self.current_element_type = Some(type_def.clone());
        }

        Ok(())
    }

    /// Header schreiben und Stacks validieren (gemeinsame Logik für finish/finish_to).
    fn prepare_finish(&mut self) -> Result<()> {
        self.write_header()?;

        // Pending whitespace am Ende → kein nachfolgendes Event → Skip
        // (Whitespace vor ED ist immer insignifikant — Document Grammar hat kein CH)
        debug_assert!(
            self.pending_ch.as_ref().map_or(true, |ch| is_xml_whitespace(&ch.value)),
            "prepare_finish: pending_ch enthält nicht-Whitespace: {:?}",
            self.pending_ch,
        );
        self.pending_ch = None;

        if !self.element_stack.is_empty() {
            return Err(Error::schema_violation(format!(
                "finish: {} offene Elemente im Stack",
                self.element_stack.len()
            )));
        }
        if !self.sc_state_stack.is_empty() {
            return Err(Error::schema_violation(format!(
                "finish: {} offene Self-Contained Fragments",
                self.sc_state_stack.len()
            )));
        }
        Ok(())
    }

    /// Beendet das Encoding und gibt die Bytes zurück.
    ///
    /// Bei PreCompression (Spec 9): Schreibt die gepufferten Values in
    /// Value Channels am Ende des Streams.
    pub fn finish(mut self) -> Result<Vec<u8>> {
        self.prepare_finish()?;

        // Bei Compression/PreCompression: Compressed Streams schreiben (Spec 9.3)
        if self.options.compression
            || matches!(self.options.alignment, Alignment::PreCompression)
        {
            self.write_compressed_streams()?;
        }

        Ok(self.writer.into_vec())
    }

    /// Draint den internen BitWriter-Buffer in den Writer.
    /// Panikt wenn ein aktiver Checkpoint existiert (typischerweise nur sicher
    /// zwischen encode_event()-Aufrufen).
    ///
    /// Gibt Fehler zurück bei Compression/PreCompression (Spec 9 erfordert Buffering).
    /// Hinweis: `pending_ch` (Whitespace mit NeedLookAhead) wird erst beim
    /// nächsten `encode_event()` oder bei `finish_to()` aufgelöst. `flush_to()`
    /// schreibt nur bereits encodierte Bits.
    pub fn flush_to(&mut self, writer: &mut impl std::io::Write) -> Result<()> {
        ensure_streaming_compatible(&self.options)?;
        self.writer
            .drain_to(writer)
            .map_err(|e| Error::IoError(format!("Flush: {e}")))
    }

    /// Finalisiert das Encoding: schreibt verbleibende Bytes (inkl. Padding) in den Writer.
    /// Vorab per flush_to() geschriebene Bytes werden nicht erneut geschrieben.
    ///
    /// Gibt Fehler zurück bei Compression/PreCompression (Spec 9 erfordert Buffering).
    pub fn finish_to(mut self, writer: &mut impl std::io::Write) -> Result<()> {
        ensure_streaming_compatible(&self.options)?;
        self.prepare_finish()?;

        // Restliche Bytes rausschreiben (inkl. Padding)
        self.writer.align_to_byte();
        self.writer
            .drain_to(writer)
            .map_err(|e| Error::IoError(format!("Finish: {e}")))
    }

}

// ============================================================================
// Fidelity-Filter (Spec 6.3)
// ============================================================================

/// Prüft ob ein Event basierend auf Fidelity Options encodiert werden soll.
///
/// # Spec-Referenz
/// - Spec 6 Schritt 2: "If fidelity options indicate this event type is not processed, go to 1"
/// - Spec 6.3: Fidelity Options Table 6-3
///
/// # Returns
/// - `Ok(true)` wenn das Event encodiert werden soll
/// - `Ok(false)` wenn das Event übersprungen werden soll
/// - `Err(...)` wenn das Event einen Fehler verursacht (z.B. SC)
#[cfg(test)]
fn should_encode_event(event: &ExiEvent, options: &ExiOptions) -> Result<bool> {
    match event {
        // Diese Events werden immer encodiert
        ExiEvent::StartDocument
        | ExiEvent::EndDocument
        | ExiEvent::StartElement(_)
        | ExiEvent::EndElement
        | ExiEvent::Attribute(_)
        | ExiEvent::Characters(_) => Ok(true),

        // Fidelity-abhängige Events
        ExiEvent::Comment(_) => Ok(options.preserve.comments),
        ExiEvent::ProcessingInstruction(_) => Ok(options.preserve.pis),
        ExiEvent::DocType(_) => Ok(options.preserve.dtd),
        ExiEvent::EntityReference(_) => Ok(options.preserve.dtd),
        ExiEvent::NamespaceDeclaration(_) => Ok(options.preserve.prefixes),

        // SelfContained wird transparent behandelt (Encoder fügt SC automatisch ein).
        // Explizite SC-Events vom Caller werden ignoriert.
        ExiEvent::SelfContained => Ok(false),
    }
}

// ============================================================================
// Production-Lookup (Spec 6.1)
// ============================================================================

impl Encoder {
    /// Konvertiert ein ExiEvent zu einem Terminal für Grammar-Lookup.
    ///
    /// # Spec-Referenz
    /// - Spec 6.1: Determining Event Codes
    /// - Spec 8.4: Built-in XML Grammars (Terminal-Definitionen)
    fn event_to_terminal_ref(&mut self, event: &EventRef<'_>) -> Result<Terminal> {
        // Häufige Events zuerst (SE, AT, CH): nutzen Accessor-Methoden
        // statt doppelter Exi/Xml-Pattern-Matches.
        if let Some(q) = event.qname() {
            if event.is_start_element() {
                let id = self.interner.intern_expanded(&q.uri, &q.local_name)?;
                return Ok(Terminal::StartElement(StartElementKind::QName(id)));
            }
            if event.is_attribute() {
                let v = event.value().unwrap();
                return self.attribute_terminal_from_parts(q, v);
            }
        }
        if event.is_characters() {
            let v = event.value().unwrap();
            return self.characters_terminal_from_value(v);
        }
        // Seltene Events: direkter Match
        match event {
            EventRef::Exi(ExiEvent::StartDocument) | EventRef::Xml(XmlEvent::StartDocument) => {
                Ok(Terminal::StartDocument)
            }
            EventRef::Exi(ExiEvent::EndDocument) | EventRef::Xml(XmlEvent::EndDocument) => {
                Ok(Terminal::EndDocument)
            }
            EventRef::Exi(ExiEvent::EndElement) | EventRef::Xml(XmlEvent::EndElement) => {
                Ok(Terminal::EndElement)
            }
            EventRef::Exi(ExiEvent::NamespaceDeclaration(_))
            | EventRef::Xml(XmlEvent::NamespaceDeclaration(_)) => Ok(Terminal::NamespaceDecl),
            EventRef::Exi(ExiEvent::Comment(_)) | EventRef::Xml(XmlEvent::Comment(_)) => {
                Ok(Terminal::Comment)
            }
            EventRef::Exi(ExiEvent::ProcessingInstruction(_))
            | EventRef::Xml(XmlEvent::ProcessingInstruction { .. }) => {
                Ok(Terminal::ProcessingInstr)
            }
            EventRef::Exi(ExiEvent::DocType(_)) | EventRef::Xml(XmlEvent::DocType(_)) => {
                Ok(Terminal::DocType)
            }
            EventRef::Exi(ExiEvent::EntityReference(_))
            | EventRef::Xml(XmlEvent::EntityReference(_)) => Ok(Terminal::EntityRef),
            EventRef::Exi(ExiEvent::SelfContained) => Ok(Terminal::SelfContained),
            _ => unreachable!("event_to_terminal_ref: unerwartetes Event"),
        }
    }

    /// Convenience-Wrapper: ExiEvent → Terminal (delegiert an event_to_terminal_ref).
    #[cfg(test)]
    fn event_to_terminal(&mut self, event: &ExiEvent) -> Result<Terminal> {
        self.event_to_terminal_ref(&EventRef::Exi(event))
    }

    fn attribute_terminal_from_parts(&mut self, qname: &QName, value: &str) -> Result<Terminal> {
        // Default: schema-less => AT(qname)
        if self.schema.is_none() {
            let id = self.interner.intern_expanded(&qname.uri, &qname.local_name)?;
            return Ok(Terminal::Attribute(AttributeKind::QName(id)));
        }

        // xsi:type darf nie über AT(*) repräsentiert werden.
        if qname.is_xsi_type() {
            let id = self.interner.intern_expanded(&qname.uri, &qname.local_name)?;
            return Ok(Terminal::Attribute(AttributeKind::QName(id)));
        }
        // xsi:nil ist ein spezielles Schema-Attribut (Spec 8.5.4.4.2,
        // spec/exi-spec.txt lines 3315-3337).
        // Bei gültigem Wert immer AT(xsi:nil) verwenden; bei ungültigem Wert
        // als AT(*) [untyped] codieren (Spec 8.5.4.4.1,
        // spec/exi-spec.txt lines 3013-3019).
        if qname.is_xsi_nil() {
            let valid = matches!(
                value,
                "true" | "1" | "false" | "0"
            );
            if valid {
                let id = self.interner.intern_expanded(&qname.uri, &qname.local_name)?;
                return Ok(Terminal::Attribute(AttributeKind::QName(id)));
            }
            return Ok(Terminal::Attribute(AttributeKind::WildcardUntyped));
        }

        // Element-Kontext nötig für declared vs wildcard
        let elem_type = self.current_element_type.as_ref();
        let in_element_fragment = self
            .element_stack
            .last()
            .and_then(|elem| self.element_grammars.get(&elem.grammar_key))
            .map(|g| g.grammar_type() == GrammarType::ElementFragment)
            .unwrap_or(false);

        let mut declared = false;
        let mut attr_type: Option<Rc<crate::schema::TypeDefinition>> = None;
        let wildcard_terminal = if in_element_fragment {
            None
        } else if let Some(wc) = elem_type.and_then(|et| et.attribute_wildcard()) {
            match wc {
                AttributeWildcard::Any | AttributeWildcard::Not(_) => {
                    Some(AttributeKind::Wildcard)
                }
                AttributeWildcard::Namespaces(uris) => {
                    if uris.iter().any(|uri| uri.as_str() == &*qname.uri) {
                        Some(AttributeKind::NamespaceWildcard(self.interner.intern(&qname.uri)?))
                    } else {
                        None
                    }
                }
            }
        } else {
            None
        };

        if in_element_fragment {
            if let Some(schema) = self.schema.as_ref() {
                if schema.is_element_fragment_relaxed_attribute(qname) {
                    declared = false;
                    attr_type = None;
                } else if let Some(td) = schema.element_fragment_attribute_type(qname) {
                    declared = true;
                    attr_type = Some(td.clone());
                }
            }
        } else if let Some(et) = elem_type
            && let crate::schema::TypeDefinition::Complex { attributes, .. } = et.as_ref()
                && let Some(attr_use) = attributes.iter().find(|au| au.qname.as_ref() == qname) {
                    declared = true;
                    attr_type = attr_use.type_definition.clone();
                }

        if attr_type.is_none()
            && let Some(schema) = self.schema.as_ref()
                && let Some(td) = schema.get_global_attribute_type(qname) {
                    attr_type = Some(td.clone());
                }

        let value_ok = attr_type
            .as_ref()
            .map(|td| self.value_representable(value, td.as_ref()))
            .unwrap_or(true);

        if declared && value_ok {
            // Deklariertes Attribut mit gültigem Wert: AT(qname) [typed value]
            let id = self.interner.intern_expanded(&qname.uri, &qname.local_name)?;
            Ok(Terminal::Attribute(AttributeKind::QName(id)))
        } else if declared {
            // Deklariertes Attribut mit ungültigem Wert: AT(qname) [untyped value]
            // Spec 8.5.4.4.1: 3-Part Event-Code n.(m+1).attr_idx
            let id = self.interner.intern_expanded(&qname.uri, &qname.local_name)?;
            Ok(Terminal::Attribute(AttributeKind::QNameUntyped(id)))
        } else if qname.is_xsi_nil() && !value_ok {
            // Spec 8.5.4.4.1: invalid xsi:nil uses AT(*) [untyped]
            // (spec/exi-spec.txt lines 3013-3019).
            Ok(Terminal::Attribute(AttributeKind::WildcardUntyped))
        } else if attr_type.is_some() && !value_ok {
            Ok(Terminal::Attribute(AttributeKind::WildcardUntyped))
        } else {
            // AT(*) — typed wenn attr_type vorhanden und value_ok, sonst Wildcard
            Ok(Terminal::Attribute(
                wildcard_terminal.unwrap_or(AttributeKind::Wildcard),
            ))
        }
    }

    fn characters_terminal_from_value(&self, value: &str) -> Result<Terminal> {
        if self.schema.is_none() {
            return Ok(Terminal::Characters);
        }

        let Some(td) = self.current_element_type.as_ref() else {
            return Ok(Terminal::Characters);
        };

        match td.as_ref() {
            crate::schema::TypeDefinition::Simple { .. } => {
                if self.value_representable(value, td.as_ref()) {
                    Ok(Terminal::Characters)
                } else {
                    Ok(Terminal::CharactersUntyped)
                }
            }
            crate::schema::TypeDefinition::Complex { content: ct, .. } => match ct {
                crate::schema::ContentType::Simple => {
                    if self.value_representable(value, td.as_ref()) {
                        Ok(Terminal::Characters)
                    } else {
                        Ok(Terminal::CharactersUntyped)
                    }
                }
                crate::schema::ContentType::Mixed(_) => {
                    // Spec 8.5.4.1.3.2: Mixed content adds CH in the schema grammar
                    // (spec/exi-spec.txt lines 2432-2440).
                    Ok(Terminal::Characters)
                }
                crate::schema::ContentType::ElementOnly(_) | crate::schema::ContentType::Empty => {
                    // Spec 8.5.4.1.3.2: Element-only/empty has no CH in Tier 1.
                    // CH appears only via undeclared productions (Spec 8.5.4.4.1),
                    // spec/exi-spec.txt lines 2432-2463, 2953-2969.
                    Ok(Terminal::CharactersUntyped)
                }
            },
        }
    }

    fn value_representable(&self, value: &str, type_def: &crate::schema::TypeDefinition) -> bool {
        let mut writer = BitWriter::new();
        let alignment = self.options.effective_alignment();
        crate::typed_value::encode_typed_value(&mut writer, value, type_def, true, alignment).is_ok()
    }
    /// Findet die passende Production für ein Terminal in einem NonTerminal.
    ///
    /// Sucht erst nach exaktem Match, dann nach Wildcard-Fallback.
    /// Delegiert an `find_production_with_wildcard` und ignoriert den Wildcard-Flag.
    ///
    /// # Spec-Referenz
    /// - Spec 6.1: "Use the grammars to determine the event code of the event"
    /// - Spec 8.4: Wildcard-Matching (SE(*), AT(*))
    #[cfg(test)]
    fn find_production<'a>(
        &'a self,
        grammar: &'a GrammarSystem,
        nt_id: NonTerminalId,
        terminal: &Terminal,
    ) -> Result<&'a Production> {
        let (prod, _event_code, _was_wildcard) = self.find_production_with_wildcard(grammar, nt_id, terminal)?;
        Ok(prod)
    }

    /// Findet die passende Production mit effektivem Event Code und Wildcard-Information.
    ///
    /// Sucht erst nach exaktem Match, dann nach Wildcard-Fallback.
    /// Returns: (production, effective_event_code, was_wildcard)
    ///
    /// Der effektive Event Code berücksichtigt den `learned_count`-Offset
    /// (Spec 8.4.2: gelernte Productions verschieben bestehende Codes).
    ///
    /// # Spec-Referenz
    /// - Spec 6.1: "Use the grammars to determine the event code of the event"
    /// - Spec 8.4: Wildcard-Matching (SE(*), AT(*))
    fn find_production_with_wildcard<'a>(
        &'a self,
        grammar: &'a GrammarSystem,
        nt_id: NonTerminalId,
        terminal: &Terminal,
    ) -> Result<(&'a Production, EventCode, bool)> {
        let nt = grammar
            .get(nt_id)
            .ok_or_else(|| Error::UnknownNonTerminal(format!("{:?}", nt_id)))?;

        // 1. Exakter Match
        if let Some((prod, effective)) = nt.find_by_terminal_with_code(terminal) {
            // Wenn das Terminal selbst ein Wildcard ist (AT(*), SE(*), AT(uri:*), SE(uri:*)),
            // muss was_wildcard=true sein — der QName wird im Stream encodiert.
            let is_wildcard_terminal = matches!(
                terminal,
                Terminal::Attribute(
                    AttributeKind::Wildcard
                        | AttributeKind::WildcardUntyped
                        | AttributeKind::NamespaceWildcard(_)
                ) | Terminal::StartElement(
                    StartElementKind::Wildcard | StartElementKind::NamespaceWildcard(_)
                )
            );
            return Ok((prod, effective, is_wildcard_terminal));
        }

        // 2. Wildcard-Fallback für SE und AT
        match terminal {
            Terminal::StartElement(StartElementKind::QName(q)) => {
                let target_uri = q.uri;
                if let Some((prod, effective)) = nt.find_by_predicate_with_code(|t| {
                    matches!(
                        t,
                        Terminal::StartElement(StartElementKind::NamespaceWildcard(uri))
                            if *uri == target_uri
                    )
                }) {
                    return Ok((prod, effective, true)); // Namespace-Wildcard-Match
                }
                let wc = Terminal::StartElement(StartElementKind::Wildcard);
                if let Some((prod, effective)) = nt.find_by_terminal_with_code(&wc) {
                    return Ok((prod, effective, true)); // Wildcard-Match
                }
            }
            Terminal::Attribute(AttributeKind::QName(q)) => {
                let target_uri = q.uri;
                if let Some((prod, effective)) = nt.find_by_predicate_with_code(|t| {
                    matches!(
                        t,
                        Terminal::Attribute(AttributeKind::NamespaceWildcard(uri))
                            if *uri == target_uri
                    )
                }) {
                    return Ok((prod, effective, true)); // Namespace-Wildcard-Match
                }
                let wc = Terminal::Attribute(AttributeKind::Wildcard);
                if let Some((prod, effective)) = nt.find_by_terminal_with_code(&wc) {
                    return Ok((prod, effective, true)); // Wildcard-Match
                }
            }
            Terminal::Attribute(AttributeKind::QNameUntyped(_)) => {
                // AT(QNameUntyped): kein Wildcard-Fallback.
                // Der 3-Part Event-Code wird im Err-Zweig von
                // lookup_element_event_code_with_wildcard erzeugt.
            }
            _ => {}
        }

        // Characters → CharactersUntyped Fallback.
        // Im ElementFragment-Kontext (Spec 8.5.3) hat Content2 nur
        // CharactersUntyped. Da ElementFragment eingebettete Undeclared
        // Productions hat (kein 2-Tier), muss der Fallback hier greifen.
        if matches!(terminal, Terminal::Characters)
            && let Some((prod, effective)) = nt.find_by_terminal_with_code(&Terminal::CharactersUntyped) {
                return Ok((prod, effective, false));
            }

        // Kein Match gefunden
        Err(Error::invalid_event_code("kein Match", "Production-Lookup"))
    }
}

// ============================================================================
// High-Level API
// ============================================================================

/// Encodiert EXI Events zu einem EXI Stream.
///
/// Schreibt Options automatisch in den Header wenn sie non-default sind,
/// damit der Stream selbst-beschreibend ist. Für explizite Kontrolle
/// `encode_with_config()` verwenden.
pub fn encode(events: &[ExiEvent], options: &ExiOptions) -> Result<Vec<u8>> {
    encode_with_config(events, options, auto_config(options))
}

/// Encodiert EXI Events mit expliziter Konfiguration.
pub fn encode_with_config(
    events: &[ExiEvent],
    options: &ExiOptions,
    config: EncoderConfig,
) -> Result<Vec<u8>> {
    let mut encoder = Encoder::new(options.clone(), config)?;

    encoder.encode_events(events)?;

    encoder.finish()
}

/// Encodiert EXI Events mit Schema (Schema-informed Encoding).
///
/// Bei Schema-informed Encoding werden:
/// - Schema-informed Document Grammar verwendet (Spec 8.5.1)
/// - Typed Values encodiert statt Strings (Spec 7.1)
/// - String Table mit Schema pre-populated (Spec 7.3.1)
///
/// # Spec-Referenz
/// - Spec 8.5: Schema-informed Grammars
/// - Spec 7.1: Built-in EXI Datatype Representations
pub fn encode_with_schema(
    events: &[ExiEvent],
    options: &ExiOptions,
    schema: &SchemaInfo,
) -> Result<Vec<u8>> {
    encode_with_schema_and_config(events, options, schema, auto_config(options))
}

/// Encodiert EXI Events mit Schema und expliziter Konfiguration.
pub fn encode_with_schema_and_config(
    events: &[ExiEvent],
    options: &ExiOptions,
    schema: &SchemaInfo,
    config: EncoderConfig,
) -> Result<Vec<u8>> {
    let mut encoder = Encoder::with_schema(options.clone(), config, schema.clone())?;

    encoder.encode_events(events)?;

    encoder.finish()
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests;
use pending::PendingValue;
