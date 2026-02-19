//! EXI Stream Decoder – Spec 6, 4, 5
//!
//! Decodiert einen EXI Stream zu einer Sequenz von EXI Events.
//! Unterstützt sowohl Schema-less als auch Schema-informed Decoding.
//!
//! # Beispiel
//!
//! ```
//! use std::rc::Rc;
//! use erxi::decoder::decode;
//! use erxi::encoder::encode;
//! use erxi::event::ExiEvent;
//! use erxi::options::ExiOptions;
//! use erxi::qname::QName;
//!
//! // Encode → Decode Round-Trip
//! let events_in = vec![
//!     ExiEvent::StartDocument,
//!     ExiEvent::StartElement(Rc::new(QName::new("", "root"))),
//!     ExiEvent::EndElement,
//!     ExiEvent::EndDocument,
//! ];
//! let bytes = encode(&events_in, &ExiOptions::default()).unwrap();
//! let (events_out, _options) = decode(&bytes).unwrap();
//! assert_eq!(events_out.len(), 4);
//! ```

mod compression;
mod lazy;
mod api;
mod context;
mod qname;
mod value;
pub use api::{decode, decode_with_options, decode_with_schema, decode_iter_with_options, decode_iter_with_schema, probe_compression};

use std::collections::VecDeque;
use std::rc::Rc;

use crate::bitstream::BitReader;
use crate::error::{Error, Result};
use crate::event::{AtContent, ChContent, ExiEvent, NsContent};
use crate::event_code::{EventCode, EventCodeContext, bits_for_part, bytes_for_part};
use crate::grammar::{
    AttributeKind, GrammarCache, GrammarKey, GrammarSystem, GrammarType, GrammarTemplateCache,
    NonTerminalId, ProductionEntry, ProductionTable, StartElementKind, Terminal, GrammarEvolution,
    cached_schema_grammar, compact_nt_bits,
};
use crate::FastHashMap;
use crate::undeclared::Tier2Context;
use crate::options::{Alignment, ExiOptions};
use crate::qname::{ExpandedNameId, QName, QNamePool, StringInterner};
use crate::schema::{
    AttributeWildcard, ContentType, ElementDeclaration, MaxOccurs, Particle, ParticleTerm,
    SchemaInfo, TypeDefinition, Wildcard,
};
use crate::string_table::{StringTable, bits_for_value};

use context::{AllGroupState, ElementContext, ElementSnapshot, ScDecoderState};

// ============================================================================
// Decoder
// ============================================================================

use compression::{MultiBlockState, PendingValue};
use lazy::{CompressedLazyState, PrecompressionLazyState};

const TIER2_CACHE_SLOTS: usize = 64;

/// Direct-Mapped Cache für Tier2Context (64 Slots).
///
/// Cached das Ergebnis von `create_tier2_context()` für ein bestimmtes
/// (grammar_key, current_nt, in_content, needs_xsi_type, is_nillable)-Tupel.
/// Invalidierung über `grammar_epoch` (bei Grammar-Mutationen).
struct Tier2Cache {
    grammar_key: GrammarKey,
    current_nt: NonTerminalId,
    in_content: bool,
    needs_xsi_type: bool,
    is_nillable: bool,
    epoch: u64,
    context: Tier2Context,
}

/// EXI Stream Decoder.
///
/// Unterstützt sowohl Schema-less als auch Schema-informed Decoding.
/// Bei Schema-informed Decoding werden Typed Values verwendet und
/// Schema-informed Grammars statt Built-in Grammars.
///
/// # Spec-Referenz
/// - Spec 6: Encoding EXI Streams
/// - Spec 5: EXI Header
/// - Spec 8.5: Schema-informed Grammars
pub struct Decoder<'a> {
    /// BitReader für Input.
    reader: BitReader<'a>,
    /// EXI Options (Alignment, Fidelity, etc.).
    options: ExiOptions,
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
    /// Header bereits gelesen?
    header_read: bool,
    /// Decoding beendet (ED erreicht)?
    finished: bool,
    /// Force Tier2 event-code decoding for schema-informed elements.
    force_tier2: bool,
    /// Gepufferte Events für spätere Rückgabe (NS-Events nach SE).
    /// Bei preserve.prefixes werden NS-Events nach SE gepuffert, damit der
    /// Prefix aus NS mit local-element-ns=true dem SE zugewiesen werden kann.
    event_buffer: VecDeque<ExiEvent>,
    /// PreCompression Buffer: Enthält alle Events nach dem vollständigen Decoding.
    /// Bei PreCompression werden erst alle Events dekodiert, dann Value Channels gelesen.
    precompression_buffer: Option<VecDeque<ExiEvent>>,
    /// Flag für PreCompression Structure Channel Phase.
    /// Wenn true, werden AT/CH Values nicht gelesen (sie kommen im Value Channel).
    skip_values: bool,
    /// Pending Values für PreCompression (AT/CH Events warten auf ihre Werte).
    pending_values: Vec<PendingValue>,
    /// Deduplizierte QNames für pending Value-Channels (Index → QName).
    pending_channel_qnames: Vec<QName>,
    /// Lookup: QName identity_hash → Channel-Index.
    pending_channel_index: crate::FastHashMap<u64, u16>,
    /// Deduplizierte TypeDefinitions für pending Values (Index → TypeDef).
    pending_type_defs: Vec<Rc<crate::schema::TypeDefinition>>,
    /// Lookup: TypeDef-Pointer → Index in pending_type_defs.
    pending_type_ptr_index: crate::FastHashMap<usize, u16>,
    /// Event-Zähler für PreCompression (um Event-Indices zu tracken).
    precompression_event_count: usize,
    /// Value-Zähler für PreCompression (Anzahl AT/CH Values, für Multi-Block-Erkennung).
    precompression_value_count: usize,

    // ========================================================================
    // Schema-informed Decoding (Issue #37)
    // ========================================================================

    /// Schema für Schema-informed Decoding (None für Schema-less).
    schema: Option<Rc<SchemaInfo>>,
    /// Aktueller Element-Typ für CH Typed Value Decoding.
    /// Wird bei push_element() gesetzt basierend auf Schema-Lookup.
    current_element_type: Option<Rc<TypeDefinition>>,
    /// Flag: xsi:nil="true" wurde im aktuellen Element gesetzt.
    /// Wenn true, darf nur noch EE folgen (Spec 8.5.4.4.2).
    xsi_nil_active: bool,
    /// Flag: xsi:type wurde im aktuellen Element gesetzt.
    /// Zusammen mit xsi:nil nicht erlaubt (Spec 8.5.4.4.2).
    xsi_type_seen: bool,
    /// Mindestens ein invalides xsi:nil (untyped) wurde decodiert.
    saw_invalid_xsi_nil: bool,
    /// QName des letzten xsi:type (zur Namespace-Auflösung).
    xsi_type_qname: Option<QName>,
    /// Erzwinge Element-Fragment-Grammar (Schema-informed).
    /// Für Element-Fragment-Streams: true sobald Content-Bereich erreicht ist.
    document_in_content: bool,
    /// Debug: Track whether we skipped xsi:type learning in built-in grammar.
    xsi_type_skip_learning_hit: bool,

    // ========================================================================
    // Self-Contained (Spec 8.5.4.4.1)
    // ========================================================================

    /// Stack für verschachtelte SC-States.
    sc_state_stack: Vec<ScDecoderState>,
    /// String-Table-Snapshot für SC-Reset (leer wenn self_contained=false).
    initial_string_table: StringTable,
    /// Fragment Grammar für SC-Fragments (Some wenn self_contained=true).
    fragment_grammar: Option<GrammarSystem>,
    // NOTE: value_max_length limits string table additions, not literal decoding.

    // ========================================================================
    // Trace-Flags (einmalig aus env::var gelesen)
    // ========================================================================
    trace: TraceFlags,

    /// Grammar-Template-Cache: Pristine Schema-Grammars nach Typ+nillable.
    /// Spart erneutes Erzeugen identischer Grammars bei wiederholten Elementen
    /// gleichen Typs. Learning divergiert per Rc::make_mut() (CoW).
    grammar_template_cache: GrammarTemplateCache,

    /// String-Interner für QName-Interning (Performance: Copy statt Rc-Clone).
    interner: StringInterner,
    /// QName-Pool: Cached Rc<QName> pro ExpandedNameId.
    /// Eliminiert wiederholte QName-Konstruktion + Rc-Allokation im Hot-Path.
    qname_pool: QNamePool,
    /// Vorberechnete xsi:type ExpandedNameId für Tier2Context.
    xsi_type_id: ExpandedNameId,
    /// Vorberechnete xsi:nil ExpandedNameId für Tier2Context.
    xsi_nil_id: ExpandedNameId,

    /// Grammar-Cache: 4-Entry Direct-Mapped Cache für element_grammars-Lookups.
    /// Vermeidet wiederholte HashMap-Hash+Probe bei zyklisch auftretenden Elementen.
    grammar_cache: GrammarCache,
    /// Epoch-Counter für Grammar-Mutationen. Wird bei jedem Rc::make_mut
    /// oder element_grammars.insert inkrementiert → invalidiert Grammar-Cache.
    grammar_epoch: u64,

    /// 64-Slot Direct-Mapped Cache für Tier2Context (Hot-Path-Optimierung).
    /// Vermeidet wiederholte create_tier2_context()-Aufrufe für dasselbe
    /// (grammar_key, current_nt, in_content, needs_xsi_type, is_nillable)-Tupel.
    tier2_cache: [Option<Tier2Cache>; TIER2_CACHE_SLOTS],
}

/// Extrahiert die ExpandedNameId aus einem SE(qname)-Terminal (falls vorhanden).
#[inline]
fn expanded_hint_from_terminal(terminal: &Terminal) -> Option<ExpandedNameId> {
    match terminal {
        Terminal::StartElement(StartElementKind::QName(q)) => Some(*q),
        _ => None,
    }
}

/// Prüft ob ein Fehler ein Typed-Value-Decode-Fehler ist, bei dem ein
/// Fallback auf untyped value sinnvoll ist (Spec 8.5.4.1.3.2, 8.5.4.4.1).
fn is_typed_value_error<T>(result: &Result<T>) -> bool {
    matches!(
        result.as_ref().err(),
        Some(
            Error::IntegerOverflow
                | Error::InvalidValue(_)
                | Error::InvalidCodePoint(_)
                | Error::InvalidEnumerationIndex { .. }
                | Error::ListLengthOverflow(_)
                | Error::FloatOutOfRange
                | Error::PrematureEndOfStream
        )
    )
}

/// Trace-Flags für Debug-Ausgabe (einmalig aus Umgebungsvariablen gelesen).
struct TraceFlags {
    precomp: bool,
    doc_event: bool,
    nt_ctx: bool,
    pi_probe: bool,
    nt: bool,
    event_code: bool,
    event_bytes: bool,
    push: bool,
    attr: bool,
    elem_event: bool,
    xsi_nil: bool,
}

impl TraceFlags {
    fn from_env() -> Self {
        Self {
            precomp: std::env::var("ERXI_TRACE_PRECOMP").is_ok(),
            doc_event: std::env::var("ERXI_TRACE_DOC_EVENT").is_ok(),
            nt_ctx: std::env::var("ERXI_TRACE_NT_CTX").is_ok(),
            pi_probe: std::env::var("ERXI_TRACE_PI_PROBE").is_ok(),
            nt: std::env::var("ERXI_TRACE_NT").is_ok(),
            event_code: std::env::var("ERXI_TRACE_EVENT_CODE").is_ok(),
            event_bytes: std::env::var("ERXI_TRACE_EVENT_BYTES").is_ok(),
            push: std::env::var("ERXI_TRACE_PUSH").is_ok(),
            attr: std::env::var("ERXI_TRACE_ATTR").is_ok(),
            elem_event: std::env::var("ERXI_TRACE_ELEM_EVENT").is_ok(),
            xsi_nil: std::env::var("ERXI_TRACE_XSI_NIL").is_ok(),
        }
    }
}

impl<'a> Decoder<'a> {
    /// Debug: Returns true if decoding skipped xsi:type learning in a built-in grammar.
    pub fn xsi_type_skip_learning_hit(&self) -> bool {
        self.xsi_type_skip_learning_hit
    }
    /// Erstellt einen neuen Decoder aus einem Byte-Slice.
    ///
    /// Der Header wird beim ersten `decode_event()` gelesen.
    pub fn new(data: &'a [u8]) -> Result<Self> {
        Self::with_options(data, ExiOptions::default())
    }

    /// Erstellt einen neuen Decoder mit externen Options.
    ///
    /// Diese Methode wird verwendet wenn die EXI Options out-of-band bekannt sind
    /// (z.B. bei Streams ohne Options im Header). Die externen Options werden als
    /// Basis verwendet und nur überschrieben wenn der Header Options enthält.
    ///
    /// # Spec-Referenz
    /// - Spec 5.4: "If the options_present flag is false, the EXI options are
    ///   communicated out-of-band"
    pub fn with_options(data: &'a [u8], options: ExiOptions) -> Result<Self> {
        let document_grammar = if options.fragment {
            GrammarSystem::built_in_fragment()
        } else {
            GrammarSystem::built_in_document()
        };
        let start_nt = document_grammar.start();
        let initial_string_table = StringTable::new();

        // Fragment Grammar für SC (Spec 8.5.4.4.1)
        // Wird erst nach Header-Lesen korrekt gesetzt (options können sich ändern).
        // Hier als Platzhalter, wird in read_header() aktualisiert.
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

        let mut string_table = StringTable::new();
        string_table.set_decode_only(true);
        Ok(Self {
            reader: BitReader::new(data),
            options,
            string_table,
            element_grammars: FastHashMap::with_capacity_and_hasher(64, Default::default()),
            document_grammar: Rc::new(document_grammar),
            document_nt: start_nt,
            element_stack: Vec::with_capacity(32),
            next_local_grammar_id: 0,
            header_read: false,
            finished: false,

            force_tier2: false,
            event_buffer: VecDeque::new(),
            precompression_buffer: None,
            skip_values: false,
            pending_values: Vec::with_capacity(256),
            pending_channel_qnames: Vec::with_capacity(64),
            pending_channel_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            pending_type_defs: Vec::with_capacity(64),
            pending_type_ptr_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            precompression_event_count: 0,
            precompression_value_count: 0,
            schema: None,
            current_element_type: None,
            xsi_nil_active: false,
            xsi_type_seen: false,
            saw_invalid_xsi_nil: false,

            xsi_type_qname: None,
            document_in_content: false,
            xsi_type_skip_learning_hit: false,
            sc_state_stack: Vec::new(),
            initial_string_table,
            fragment_grammar,
            trace: TraceFlags::from_env(),
            grammar_template_cache: GrammarTemplateCache::default(),
            interner,
            qname_pool: QNamePool::new(),
            xsi_type_id,
            xsi_nil_id,
            grammar_cache: GrammarCache::new(),
            grammar_epoch: 0,
            tier2_cache: [const { None }; TIER2_CACHE_SLOTS],
        })
    }

    /// Erstellt einen neuen Decoder mit Schema (Schema-informed Decoding).
    ///
    /// Bei Schema-informed Decoding werden:
    /// - Schema-informed Document Grammar verwendet (Spec 8.5.1)
    /// - Typed Values decodiert statt Strings (Spec 7.1)
    /// - String Table mit Schema pre-populated (Spec 7.3.1)
    ///
    /// # Spec-Referenz
    /// - Spec 8.5: Schema-informed Grammars
    /// - Spec 7.1: Built-in EXI Datatype Representations
    pub fn with_schema(data: &'a [u8], options: ExiOptions, schema: SchemaInfo) -> Result<Self> {
        let mut interner = StringInterner::with_capacity(256);

        // Schema-informed Document/Fragment Grammar verwenden
        let document_grammar = if options.fragment {
            GrammarSystem::schema_informed_fragment(
                &schema,
                &mut interner,
            )?
        } else {
            GrammarSystem::schema_informed_document(&schema, &mut interner)?
        };
        let start_nt = document_grammar.start();

        // String Table mit Schema pre-population (Spec 7.3.1, Appendix D)
        let mut string_table = StringTable::from_schema_with_options(
            &schema,
            options.value_max_length.map(|v| v as usize),
            options.value_partition_capacity.map(|v| v as usize),
        );
        string_table.set_decode_only(true);
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
            reader: BitReader::new(data),
            options,
            string_table,
            element_grammars: FastHashMap::with_capacity_and_hasher(grammar_cap, Default::default()),
            document_grammar: Rc::new(document_grammar),
            document_nt: start_nt,
            element_stack: Vec::with_capacity(32),
            next_local_grammar_id: 0,
            header_read: false,
            finished: false,

            force_tier2: false,
            event_buffer: VecDeque::new(),
            precompression_buffer: None,
            skip_values: false,
            pending_values: Vec::with_capacity(256),
            pending_channel_qnames: Vec::with_capacity(64),
            pending_channel_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            pending_type_defs: Vec::with_capacity(64),
            pending_type_ptr_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            precompression_event_count: 0,
            precompression_value_count: 0,
            schema: Some(Rc::new(schema)),
            current_element_type: None,
            xsi_nil_active: false,
            xsi_type_seen: false,
            saw_invalid_xsi_nil: false,

            xsi_type_qname: None,
            document_in_content: false,
            xsi_type_skip_learning_hit: false,
            sc_state_stack: Vec::new(),
            initial_string_table,
            fragment_grammar,
            trace: TraceFlags::from_env(),
            grammar_template_cache: GrammarTemplateCache::default(),
            interner,
            qname_pool: QNamePool::new(),
            xsi_type_id,
            xsi_nil_id,
            grammar_cache: GrammarCache::new(),
            grammar_epoch: 0,
            tier2_cache: [const { None }; TIER2_CACHE_SLOTS],
        })
    }

    /// Erstellt einen Decoder aus einem MultiBlockState.
    ///
    /// Übernimmt den State per Move (kein Clone). Kein Header-Lesen nötig,
    /// da die Options und der Grammar-State vom vorherigen Block bekannt sind.
    /// Schema wird per Rc::clone geteilt (billig).
    fn from_state(
        data: &'a [u8],
        state: MultiBlockState,
        options: &ExiOptions,
        schema: &Option<Rc<SchemaInfo>>,
    ) -> Self {
        Self {
            reader: BitReader::new(data),
            options: options.clone(),
            string_table: state.string_table,
            element_grammars: state.element_grammars,
            document_grammar: state.document_grammar,
            document_nt: state.document_nt,
            element_stack: state.element_stack,
            next_local_grammar_id: state.next_local_grammar_id,
            header_read: true,
            finished: false,
            force_tier2: false,
            event_buffer: VecDeque::new(),
            precompression_buffer: None,
            skip_values: false,
            pending_values: Vec::with_capacity(256),
            pending_channel_qnames: Vec::with_capacity(64),
            pending_channel_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            pending_type_defs: Vec::with_capacity(64),
            pending_type_ptr_index: crate::FastHashMap::with_capacity_and_hasher(32, Default::default()),
            precompression_event_count: 0,
            precompression_value_count: 0,
            schema: schema.clone(),
            current_element_type: state.current_element_type,
            xsi_nil_active: state.xsi_nil_active,
            xsi_type_seen: state.xsi_type_seen,
            saw_invalid_xsi_nil: state.saw_invalid_xsi_nil,
            xsi_type_qname: state.xsi_type_qname,
            document_in_content: state.document_in_content,
            xsi_type_skip_learning_hit: state.xsi_type_skip_learning_hit,
            sc_state_stack: Vec::new(),
            initial_string_table: StringTable::new(),
            fragment_grammar: None,
            trace: TraceFlags::from_env(),
            grammar_template_cache: state.grammar_template_cache,
            interner: state.interner,
            qname_pool: state.qname_pool,
            xsi_type_id: state.xsi_type_id,
            xsi_nil_id: state.xsi_nil_id,
            grammar_cache: state.grammar_cache,
            grammar_epoch: state.grammar_epoch,
            tier2_cache: [const { None }; TIER2_CACHE_SLOTS],
        }
    }

    /// Gibt die gelesenen EXI Options zurück.
    pub fn options(&self) -> &ExiOptions {
        &self.options
    }

    /// Cached Grammar-Lookup (Direct-Mapped, 4 Slots).
    fn get_element_grammar(&mut self, key: &GrammarKey) -> Option<Rc<GrammarSystem>> {
        if let Some(grammar) = self.grammar_cache.lookup(key, self.grammar_epoch) {
            return Some(grammar);
        }
        let grammar = self.element_grammars.get(key)?.clone();
        self.grammar_cache.insert(*key, &grammar, self.grammar_epoch);
        Some(grammar)
    }

    /// Löst den ExpandedNameId auf: nutzt den Hint aus SE(qname) wenn vorhanden,
    /// sonst intern_expanded. Im Debug-Modus wird die Konsistenz geprüft.
    fn resolve_expanded_hint(&mut self, qname: &QName, hint: Option<ExpandedNameId>) -> Result<ExpandedNameId> {
        match hint {
            Some(id) => {
                debug_assert_eq!(
                    id,
                    self.interner
                        .intern_expanded(&qname.uri, &qname.local_name)
                        .expect("intern_expanded in debug_assert"),
                    "expanded_hint divergiert von QName-Interning"
                );
                Ok(id)
            }
            None => self.interner.intern_expanded(&qname.uri, &qname.local_name),
        }
    }

    /// Inkrementiert die Grammar-Epoch (invalidiert Grammar-Cache).
    /// Aufrufen nach jedem `element_grammars.insert()` oder `Rc::make_mut()`.
    #[inline]
    fn bump_grammar_epoch(&mut self) {
        self.grammar_epoch = self.grammar_epoch.wrapping_add(1);
        self.tier2_cache = [const { None }; TIER2_CACHE_SLOTS];
    }

    /// Poppt das oberste Element vom Stack und entfernt dessen lokale Grammar
    /// (local_id != None) aus der HashMap, da sie nach Pop nicht mehr gebraucht wird.
    #[inline]
    fn pop_element_grammar(&mut self) {
        if let Some(ctx) = self.element_stack.pop() {
            if ctx.grammar_key.local_id.is_some() {
                self.element_grammars.remove(&ctx.grammar_key);
            }
        }
    }

    /// Decode-only: erzwingt Tier2-Decoding für schema-informed Element-Grammars.
    pub fn set_force_tier2(&mut self, enabled: bool) {
        self.force_tier2 = enabled;
    }

    /// Setzt den MemoryMonitor fuer OOM-Vermeidung bei grossen Dateien.
    pub fn set_memory_monitor(&mut self, monitor: crate::memory_monitor::MemoryMonitor) {
        self.string_table.set_memory_monitor(monitor);
    }

    /// Gibt die aktuelle Bit-Position im Stream zurück.
    pub fn bit_position(&self) -> usize {
        self.reader.bit_position()
    }

    /// Gibt die verbleibenden Bits im Stream zurück.
    pub fn remaining_bits(&self) -> usize {
        self.reader.remaining_bits()
    }

    /// Liest den EXI Header.
    ///
    /// # Spec-Referenz
    /// - Spec 5.1: EXI Cookie
    /// - Spec 5.2: Distinguishing Bits
    /// - Spec 5.3: EXI Format Version
    /// - Spec 5.4: EXI Options
    /// - Spec 8.4.2: Built-in Fragment Grammar
    fn read_header(&mut self) -> Result<()> {
        if self.header_read {
            return Ok(());
        }

        // Header decodieren (ohne Padding - wird separat behandelt)
        let header = crate::header::decode(&mut self.reader, false)?;

        // Options decodieren falls vorhanden
        if header.options_present() {
            let sc_qnames = std::mem::take(&mut self.options.self_contained_qnames);
            self.options = crate::options_codec::decode(&mut self.reader)?;
            // SC-QNames sind Application-Level Flags
            // (nicht Teil des EXI Options Documents).
            self.options.self_contained_qnames = sc_qnames;
        }

        // Padding nach Header/Options überspringen (Spec 5)
        // "When the compression option is true, or the alignment option is
        // byte-alignment or pre-compression, padding bits... are added at
        // the end of the header."
        //
        // WICHTIG: Das Padding muss auch bei out-of-band Options angewendet werden,
        // da der Encoder das Alignment kennt wenn er den Header schreibt.
        let needs_padding = self.options.compression
            || matches!(
                self.options.alignment,
                Alignment::ByteAlignment | Alignment::PreCompression
            );

        if needs_padding {
            self.reader.align_to_byte();
        }

        // Grammar basierend auf Options konfigurieren
        if self.options.fragment {
            if let Some(schema) = self.schema.as_ref() {
                // Schema-informed Fragment Grammar (Spec 8.5.2)
                self.document_grammar = Rc::new(GrammarSystem::schema_informed_fragment(
                    schema,
                    &mut self.interner,
                )?);
                self.document_nt = self.document_grammar.start();
            } else {
                // Built-in Fragment Grammar (Spec 8.4.2)
                self.document_grammar = Rc::new(GrammarSystem::built_in_fragment());
                self.document_nt = self.document_grammar.start();
            }
        }

        // Bei Schema-less: String Table basierend auf Options initialisieren
        if self.schema.is_none() {
            // String Table mit Options-Constraints erstellen (Spec 7.3.3, Appendix D)
            self.string_table = StringTable::with_options(
                self.options.value_max_length.map(|v| v as usize),
                self.options.value_partition_capacity.map(|v| v as usize),
            );
        }

        // Fragment Grammar für SC nach Options-Decoding aktualisieren (Spec 8.5.4.4.1)
        if self.options.self_contained && self.fragment_grammar.is_none() {
            let mut fg = if let Some(schema) = self.schema.as_ref() {
                GrammarSystem::schema_informed_fragment(
                    schema,
                    &mut self.interner,
                )?
            } else {
                GrammarSystem::built_in_fragment()
            };
            fg.prune(&self.options)?;
            self.fragment_grammar = Some(fg);
        }

        // Initial String Table nach Options-Decoding aktualisieren (nur bei SC nötig)
        if self.options.self_contained {
            self.initial_string_table = self.string_table.clone();
        }

        // Grammar basierend auf Options prunen (für beide Modi)
        Rc::make_mut(&mut self.document_grammar).prune(&self.options)?;

        self.header_read = true;
        Ok(())
    }

    /// Decodiert ein einzelnes Event.
    ///
    /// Gibt `None` zurück wenn das Ende des Streams erreicht ist (ED wurde decodiert).
    ///
    /// # Spec-Referenz
    /// - Spec 6: Encoding EXI Streams (Algorithmus)
    /// - Spec 9: EXI Compression (für PreCompression)
    pub fn decode_event(&mut self) -> Result<Option<ExiEvent>> {
        // Header lesen falls noch nicht geschehen
        self.read_header()?;

        if self.finished {
            return Ok(None);
        }

        // PreCompression: Alle Events auf einmal dekodieren (Spec 9)
        // Bei PreCompression werden AT/CH Values im Value Channel am Ende des Streams
        // geschrieben, nicht inline nach dem Event Code.
        if self.options.alignment == Alignment::PreCompression {
            // Wenn noch kein Buffer vorhanden, den gesamten Stream dekodieren
            if self.precompression_buffer.is_none() {
                self.decode_precompression_stream()?;
            }

            // Event aus Buffer zurückgeben
            if let Some(ref mut buffer) = self.precompression_buffer
                && let Some(event) = buffer.pop_front()
            {
                if matches!(event, ExiEvent::EndDocument) {
                    self.finished = true;
                }
                return Ok(Some(event));
            }
            return Ok(None);
        }

        // Erst gepufferte Events zurückgeben
        if let Some(event) = self.event_buffer.pop_front() {
            // Prüfe ob ED erreicht
            if matches!(event, ExiEvent::EndDocument) {
                self.finished = true;
            }
            // Bei preserve.prefixes und SE aus Buffer: auch Lookahead machen
            // (für verschachtelte SEs die während eines früheren Lookaheads gepuffert wurden)
            if self.options.preserve.prefixes
                && let ExiEvent::StartElement(qname) = event
            {
                return self.process_se_with_lookahead(Rc::unwrap_or_clone(qname));
            }
            return Ok(Some(event));
        }

        // Dispatch basierend auf aktuellem Kontext (Document oder Element)
        let event = if self.element_stack.is_empty() {
            self.decode_document_event()?
        } else {
            self.decode_element_event()?
        };

        // Bei preserve.prefixes und SE: NS-Events vorauslesen und Prefix zuweisen
        if self.options.preserve.prefixes
            && let ExiEvent::StartElement(qname) = event
        {
            return self.process_se_with_lookahead(Rc::unwrap_or_clone(qname));
        }

        // Prüfe ob ED erreicht
        if matches!(event, ExiEvent::EndDocument) {
            self.finished = true;
        }
        // Element-Fragment: Stream endet nach EE (kein ED)
        if !self.finished
            && self.element_stack.is_empty()
            && self.document_grammar.grammar_type() == GrammarType::ElementFragment
            && matches!(event, ExiEvent::EndElement)
        {
            self.finished = true;
        }

        Ok(Some(event))
    }

    /// Verarbeitet SE-Event mit NS-Lookahead (preserve.prefixes=true).
    ///
    /// Liest NS-Events voraus bis ein anderes Event kommt. Wenn ein NS mit
    /// local-element-ns=true gefunden wird, wird dessen Prefix dem SE zugewiesen.
    /// Alle gelesenen Events (NS und das Nicht-NS-Event) werden gepuffert.
    fn process_se_with_lookahead(&mut self, mut qname: QName) -> Result<Option<ExiEvent>> {
        // NS-Events sammeln bis ein anderes Event kommt
        loop {
            let event = self.decode_element_event()?;

            match event {
                ExiEvent::NamespaceDeclaration(ref ns) => {
                    // Wenn local-element-ns=true und gleicher URI, Prefix zuweisen
                    if ns.local_element_ns && *ns.uri == *qname.uri {
                        qname.prefix = Some(Rc::clone(&ns.prefix));
                    }
                    // NS-Event puffern (wird nach SE ausgegeben)
                    self.event_buffer.push_back(event);
                }
                _ => {
                    // Nicht-NS-Event puffern und Schleife beenden
                    self.event_buffer.push_back(event);
                    break;
                }
            }
        }

        Ok(Some(ExiEvent::StartElement(Rc::new(qname))))
    }

    /// Decodiert ein Event im Document-Kontext.
    fn decode_document_event(&mut self) -> Result<ExiEvent> {
        // Sammle Grammar-Daten für hierarchisches Decoding (ohne self zu borgen)
        let (production_table, tier2_ctx) = {
            let nt = self
                .document_grammar
                .get(self.document_nt)
                .ok_or_else(|| Error::UnknownNonTerminal(format!("{:?}", self.document_nt)))?;
            let production_table = nt.production_table().clone();

            if self.document_grammar.grammar_type() == GrammarType::ElementFragment
                && !self.options.strict
            {
                let t2_ctx = self.create_document_tier2_context()?;
                let t2_count = t2_ctx.count();
                if t2_count > 0 {
                    (production_table, Some(t2_ctx))
                } else {
                    (production_table, None)
                }
            } else {
                (production_table, None)
            }
        };

        let event_code_start = self.bit_position();
        let (event_code, terminal, mut right_hand_side, _was_wildcard) =
            if let Some(ref tier2_ctx) = tier2_ctx {
                self.decode_schema_informed_event_code(
                    &production_table,
                    tier2_ctx,
                    self.document_nt,
                )?
            } else {
                let (event_code, entry) =
                    self.decode_event_code_from_table(&production_table)?;
                let was_wildcard = entry.terminal.is_wildcard();
                (event_code, entry.terminal, entry.rhs, was_wildcard)
            };

        if self.trace.precomp
            && self.document_nt == NonTerminalId::DocEnd
            && matches!(terminal, Terminal::Comment | Terminal::ProcessingInstr)
        {
            eprintln!(
                "docend code bits: start={} after_code={}",
                event_code_start,
                self.bit_position()
            );
        }

        let is_tier2 = tier2_ctx.is_some() && event_code.part2().is_some();
        if is_tier2
            && matches!(
                terminal,
                Terminal::StartElement(_) | Terminal::Characters | Terminal::CharactersUntyped
            )
            && let Some(content_nt) =
                self.document_grammar.find_content_nonterminal(self.document_nt)
        {
            right_hand_side = Some(content_nt);
        }

        if self.trace.doc_event {
            eprintln!(
                "doc_event: nt={:?} code={:?} terminal={:?}",
                self.document_nt, event_code, terminal
            );
        }

        self.log_event_code_from_table(
            &event_code,
            &production_table,
            &terminal,
            self.document_grammar.grammar_type(),
        );

        // Event Content decodieren
        let event = self.decode_event_content(&terminal)?;

        self.log_elem_event(&event, self.element_stack.last().map(|e| e.expanded_name));

        if matches!(event, ExiEvent::StartElement(_) | ExiEvent::Characters(_))
            && self.document_grammar.grammar_type() == GrammarType::ElementFragment
        {
            self.document_in_content = true;
        }

        // Grammar Evolution bei SE(*) in Fragment Grammar (Spec 8.4.3)
        // Bei Fragment können mehrere Top-Level-Elemente vorkommen, daher muss
        // SE(qname) in FragmentContent gelernt werden, wenn SE(*) gematcht wird.
        if matches!(
            &terminal,
            Terminal::StartElement(StartElementKind::Wildcard)
        ) && self.options.fragment
            && self.schema.is_none()
            && let ExiEvent::StartElement(qname) = &event
        {
            // Grammar evolution for built-in fragment grammar only.
            // Schema-informed fragment grammars must keep event-code ordering stable.
            self.evolve_document_grammar(qname)?;
        }

        // Grammar-Transition
        if let Some(next_nt) = right_hand_side {
            self.document_nt = next_nt;
        }

        // Bei SE: Element-Stack pushen
        if let ExiEvent::StartElement(ref qname) = event {
            self.push_element(qname, expanded_hint_from_terminal(&terminal))?;
        }

        Ok(event)
    }

    /// Decodiert ein Event im Element-Kontext.
    fn decode_element_event(&mut self) -> Result<ExiEvent> {
        let snap = self
            .element_stack
            .last()
            .ok_or_else(|| Error::invalid_event_code("", "leerer Element-Stack"))?
            .snapshot();

        let current_nt = snap.current_nt;

        // Sammle Grammar-Daten für hierarchisches Decoding
        let (production_table, mut tier2_ctx, bits_part1_override, bytes_part1_override, grammar_type) = {
            let elem_grammar = self
                .get_element_grammar(&snap.grammar_key)
                .ok_or_else(|| Error::invalid_event_code("", "Grammar nicht gefunden"))?;
            let nt = elem_grammar
                .get(current_nt)
                .ok_or_else(|| Error::UnknownNonTerminal(format!("{:?}", current_nt)))?;

            let production_table = nt.production_table().clone();
            let grammar_type = elem_grammar.grammar_type();

            let bp1 = production_table.bits_part1();
            let byp1 = production_table.bytes_part1();

            (production_table, None, bp1, byp1, grammar_type)
        };

        // Tier2-Decoding für SchemaInformedElement/ElementFragment Grammars:
        // - force_tier2: explizit erzwungen
        // - Wenn der Tier-2 Count von der augmentierten Grammar-Part2
        //   Anzahl abweicht, müssen wir Tier-2 verwenden.
        if (self.force_tier2 || !self.options.strict)
            && grammar_type == GrammarType::SchemaInformedElement
            && tier2_ctx.is_none()
        {
            let aug_part2_count = self.element_grammars.get(&snap.grammar_key)
                .and_then(|eg| eg.get(current_nt))
                .map(|nt| {
                    let mut count = nt.productions()
                        .iter()
                        .filter(|p| p.event_code.as_ref().is_some_and(|ec| ec.length() > 1))
                        .count() as u32;
                    let has_aug_cm = nt.productions().iter().any(|p| matches!(p.terminal, Terminal::Comment)
                        && p.event_code.as_ref().is_some_and(|ec| ec.length() > 1));
                    let has_aug_pi = nt.productions().iter().any(|p| matches!(p.terminal, Terminal::ProcessingInstr)
                        && p.event_code.as_ref().is_some_and(|ec| ec.length() > 1));
                    if has_aug_cm && has_aug_pi {
                        count -= 1;
                    }
                    count
                });

            if let Some(aug_part2_count) = aug_part2_count {
                let t2_ctx = self.cached_tier2_context(&snap)?;
                let t2_count = t2_ctx.count();
                let need_tier2 = self.force_tier2
                    || (t2_count > 0 && t2_count != aug_part2_count);
                if need_tier2 && t2_count > 0 {
                    tier2_ctx = Some(t2_ctx);
                }
            }
        }

        // Event Code lesen und Production bestimmen (Tier 1 + optional Tier 2)
        let decode_result = if let Some(ref tier2_ctx) = tier2_ctx {
            self.decode_schema_informed_event_code(&production_table, tier2_ctx, current_nt)
        } else {
            match self.decode_event_code_from_table_with_bits(
                &production_table, bits_part1_override, bytes_part1_override,
            ) {
                Ok((event_code, entry)) => {
                    let terminal = entry.terminal;
                    let right_hand_side = entry.rhs;
                    // AT(WildcardUntyped) mit 3-Part-Code (Spec 8.5.4.4.1)
                    if matches!(&terminal, Terminal::Attribute(AttributeKind::WildcardUntyped))
                    {
                        let part3_result = self.decode_at_invalid_value_part3(current_nt);
                        if let Ok(Some((term3, rhs3))) = part3_result {
                            let was_wildcard = term3.is_wildcard();
                            Ok((event_code, term3, rhs3, was_wildcard))
                        } else {
                            Ok((event_code, terminal, right_hand_side, true))
                        }
                    } else {
                        let was_wildcard = terminal.is_wildcard();
                        Ok((event_code, terminal, right_hand_side, was_wildcard))
                    }
                }
                Err(err) => Err(err),
            }
        };

        // ProductionTable-Rc freigeben: strong_count wird 1, damit
        // inkrementelle Table-Updates in evolve_*/learn_production() in-place laufen.
        drop(production_table);

        let (event_code, terminal, mut right_hand_side, was_wildcard) =
            decode_result?;
        let is_tier2 = tier2_ctx.is_some() && event_code.part2().is_some();
        if is_tier2
            && matches!(
                terminal,
                Terminal::StartElement(_) | Terminal::Characters | Terminal::CharactersUntyped
            )
            && let Some(content_nt) =
                self.element_grammars.get(&snap.grammar_key)
                    .and_then(|g| g.find_content_nonterminal(current_nt))
        {
            right_hand_side = Some(content_nt);
        }
        if self.trace.event_code {
            eprintln!(
                "decode_event_code: part1={} bits={} grammar={:?}",
                event_code.part1(),
                bits_part1_override,
                grammar_type,
            );
        }
        if self.trace.nt_ctx {
            eprintln!(
                "nt_ctx: nt={:?} part1_bits={} content_area={} tier2={} grammar={:?}",
                current_nt,
                bits_part1_override,
                tier2_ctx.as_ref().map(|t| t.is_content_area).unwrap_or(false),
                is_tier2,
                grammar_type,
            );
        }

        // Event Content decodieren
        // Spec 8.4.3: Bei SE(qname)/AT(qname) (gelerntes Event) ist der QName
        // implizit durch den Event Code bekannt und wird NICHT aus dem Stream gelesen.
        // Nur bei SE(*)/AT(*) wird der QName aus dem Stream decodiert.
        if self.trace.pi_probe
            && matches!(terminal, Terminal::Comment | Terminal::ProcessingInstr) {
                let cp = self.reader.save_checkpoint();
                if matches!(terminal, Terminal::Comment) {
                    let probe = crate::event_content::decode_processing_instruction(&mut self.reader);
                    eprintln!("probe_pi_at_cm: pos={} result={:?}", self.bit_position(), probe);
                } else {
                    let probe = crate::event_content::decode_comment(&mut self.reader);
                    eprintln!("probe_cm_at_pi: pos={} result={:?}", self.bit_position(), probe);
                }
                self.reader.restore_checkpoint(cp);
            }
        let event = self.decode_event_content_element(&terminal, is_tier2)?;

        // Spec 8.5.4.4.1: Self-Contained Element Handling
        // SC ist transparent: Der Caller hat bereits das SE vom äußeren Grammar erhalten.
        // Wir setzen den Fragment-State auf und decodieren das nächste echte Event.
        if matches!(event, ExiEvent::SelfContained) {
            if self.fragment_grammar.is_some() {
                self.handle_self_contained_decode()?;
                return self.decode_element_event();
            }
            return Err(Error::UnsupportedSelfContained);
        }

        let current_elem = self.element_stack.last().map(|e| e.expanded_name);
        self.log_elem_event(&event, current_elem);

        // all_group mark_seen: Nur internieren wenn SE + all_group vorhanden (Hot-Path-Guard)
        let all_group_expanded = if let ExiEvent::StartElement(ref child) = event
            && self.element_stack.last().is_some_and(|e| e.all_group.is_some())
        {
            Some(self.interner.intern_expanded(&child.uri, &child.local_name)?)
        } else {
            None
        };

        if let Some(elem) = self.element_stack.last_mut() {
            if !matches!(
                event,
                ExiEvent::Attribute(_) | ExiEvent::NamespaceDeclaration(_)
            ) {
                elem.in_content = true;
            }
            if let Some(expanded) = all_group_expanded {
                if let Some(all_group) = elem.all_group.as_mut() {
                    all_group.mark_seen(expanded);
                }
            }
        }

        // Grammar Evolution bei Wildcard-Match (Spec 8.4.3)
        let mut skip_wildcard_evolution = false;
        if was_wildcard
            && let ExiEvent::Attribute(content) = &event {
                // Spec 8.5.4.4.1 (Z.3134): In schema-informed Grammars dürfen
                // xsi:type und xsi:nil NICHT über AT(*) repräsentiert werden.
                // Learning nach AT(*)-Match auf xsi:type wird daher unterdrückt.
                if self.schema.is_some()
                    && grammar_type == GrammarType::Element
                    && content.qname.is_xsi_type()
                {
                    skip_wildcard_evolution = true;
                    self.xsi_type_skip_learning_hit = true;
                }
            }
        if was_wildcard && !skip_wildcard_evolution {
            self.evolve_element_grammar(&snap.grammar_key, current_nt, &event)?;
        }

        // Grammar Evolution bei CH/EE mit Event Code Länge > 1 (Spec 8.4.3)
        // "If the matched terminal symbol is CH [...] or EE and the event code is more than
        // one part, the associated grammar is augmented as follows:
        // Add a production for CH [...] or EE with an event code of 0 (zero)..."
        //
        // CH/EE-Learning wird für alle Alignment-Modi durchgeführt (Spec 8.4.3).
        // Frühere Annahme, dass Exificient dies nur bei bit-packed macht, war falsch -
        // Tests mit Exificient 1.0.4 zeigen, dass Learning auch bei byte-aligned erfolgt.
        if matches!(
            terminal,
            Terminal::Characters | Terminal::CharactersUntyped | Terminal::EndElement
        )
            && event_code.length() > 1
            && grammar_type == GrammarType::Element
        {
            self.evolve_content_grammar(
                &snap.grammar_key,
                current_nt,
                &terminal,
                right_hand_side,
            )?;
        }

        // Grammar-Transition
        if let Some(next_nt) = right_hand_side
            && let Some(elem) = self.element_stack.last_mut()
        {
            elem.current_nt = next_nt;
            if self.trace.nt {
                eprintln!("nt transition: {:?}", elem.current_nt);
            }
        }

        // Apply deferred xsi:type grammar switch BEFORE xsi:nil TypeEmpty switch.
        // Der xsi:type-Switch muss zuerst angewendet werden, damit
        // switch_current_element_to_type_empty() die TypeEmpty-Grammar des
        // NEUEN Typs verwendet (nicht des alten). Sonst wird die TypeEmpty-
        // Umschaltung durch den nachfolgenden xsi:type-Switch ueberschrieben.
        let pending = self
            .element_stack
            .last_mut()
            .and_then(|e| e.pending_type_switch.take());
        if let Some(type_def) = pending {
            self.switch_current_element_grammar(type_def)?;
        }

        if let ExiEvent::Attribute(AtContent { qname, value }) = &event
            && qname.is_xsi_nil()
            && matches!(&**value, "true" | "1")
        {
            // Spec 8.5.4.4.2: xsi:nil="true" → TypeEmpty Grammar.
            self.switch_current_element_to_type_empty()?;
        }

        // Bei SE: Neues Element pushen
        if let ExiEvent::StartElement(ref qname) = event {
            self.push_element(qname, expanded_hint_from_terminal(&terminal))?;
        }

        // Spec 8.5.4.4.2: Bei strict=true ist nach xsi:nil="true" nur EE erlaubt.
        if self.options.strict
            && self.xsi_nil_active
            && !matches!(event, ExiEvent::EndElement | ExiEvent::Attribute(_))
        {
            return Err(Error::XsiNilContentNotEmpty);
        }

        // Bei EE: Element-Stack poppen und current_element_type aktualisieren
        if matches!(event, ExiEvent::EndElement) {
            self.pop_element_grammar();

            // Spec 8.5.4.4.1: SC-Fragment beenden wenn Stack leer
            if self.element_stack.is_empty() && !self.sc_state_stack.is_empty() {
                self.end_self_contained_decode()?;
                return Ok(ExiEvent::EndElement);
            }

            // xsi:nil und xsi:type Flags zurücksetzen (gelten nur für das aktuelle Element)
            self.xsi_nil_active = false;
            self.xsi_type_seen = false;
            self.xsi_type_qname = None;
            // current_element_type auf Parent-Element-Typ setzen (oder None)
            self.current_element_type = self
                .element_stack
                .last()
                .and_then(|elem| elem.element_type.clone());
        }

        Ok(event)
    }

    /// Handhabt ein decodiertes Terminal::SelfContained (Spec 8.5.4.4.1).
    ///
    /// 1. State speichern
    /// 2. State auf Anfangszustand zurücksetzen
    /// 3. Byte-Alignment
    /// 4. SD auf Fragment Grammar decodieren
    /// 5. SE(qname) auf FragmentContent decodieren (intern, nicht an Caller)
    /// 6. push_element
    ///
    /// Das SE aus dem Fragment wird NICHT an den Caller zurückgegeben, da dieser
    /// bereits ein SE vom äußeren Grammar erhalten hat. SC ist transparent —
    /// der Caller sieht nur das äußere SE und die Inhalte aus dem Fragment.
    fn handle_self_contained_decode(&mut self) -> Result<()> {
        // Schritt 1: State speichern
        let saved = ScDecoderState {
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
            xsi_type_qname: self.xsi_type_qname.take(),
            saw_invalid_xsi_nil: self.saw_invalid_xsi_nil,
            xsi_type_skip_learning_hit: self.xsi_type_skip_learning_hit,
            document_in_content: self.document_in_content,
            event_buffer: std::mem::take(&mut self.event_buffer),
            finished: self.finished,
        };
        self.sc_state_stack.push(saved);

        // Grammar-Cache invalidieren: element_grammars wurde geleert (take)
        self.bump_grammar_epoch();

        // Schritt 2: State zurücksetzen
        self.next_local_grammar_id = 0;
        self.xsi_nil_active = false;
        self.xsi_type_seen = false;
        self.saw_invalid_xsi_nil = false;
        self.xsi_type_skip_learning_hit = false;
        self.document_in_content = false;
        self.finished = false;

        // Schritt 3: Byte-Alignment
        self.reader.align_to_byte();

        // Schritt 4: Fragment Grammar setzen
        let fragment = self
            .fragment_grammar
            .as_ref()
            .expect("fragment_grammar must exist when self_contained=true")
            .clone();
        self.document_grammar = Rc::new(fragment);
        self.document_nt = self.document_grammar.start(); // Fragment

        // SD [0] auf Fragment decodieren (intern)
        {
            let fragment_nt = self
                .document_grammar
                .get(self.document_nt)
                .expect("Fragment NT must exist");
            let sd_ctx = fragment_nt.event_code_context().clone();
            // SD Event-Code lesen (sollte [0] sein)
            let _sd_code = self.decode_event_code_simple(&sd_ctx)?;
            // Transition: Fragment → FragmentContent
            self.document_nt = NonTerminalId::FragmentContent;
        }

        // Schritt 5: SE(qname) auf FragmentContent decodieren
        let (qname, expanded_hint) = {
            let fc_nt = self
                .document_grammar
                .get(self.document_nt)
                .expect("FragmentContent NT must exist");
            let fc_table = fc_nt.production_table().clone();

            let (_event_code, entry) =
                self.decode_event_code_from_table(&fc_table)?;

            match entry.terminal {
                Terminal::StartElement(StartElementKind::QName(q)) => {
                    let qname = self.qname_pool.get_or_create(q, &self.interner);
                    (qname, Some(q))
                }
                Terminal::StartElement(StartElementKind::Wildcard | StartElementKind::NamespaceWildcard(_)) => {
                    (Rc::new(self.decode_qname()?), None)
                }
                _ => return Err(Error::invalid_event_code("kein SE nach SC-Fragment", "FragmentContent")),
            }
        };

        // Schritt 6: push_element
        self.push_element(&qname, expanded_hint)?;

        Ok(())
    }

    /// Beendet ein Self-Contained Fragment (Spec 8.5.4.4.1).
    ///
    /// 1. ED auf FragmentContent decodieren (intern)
    /// 2. Byte-Alignment
    /// 3. State aus sc_state_stack wiederherstellen
    fn end_self_contained_decode(&mut self) -> Result<()> {
        // ED auf FragmentContent decodieren (intern, nicht an Caller)
        {
            let fc_nt = self
                .document_grammar
                .get(NonTerminalId::FragmentContent)
                .expect("FragmentContent NT must exist");
            let fc_table = fc_nt.production_table().clone();

            let (_event_code, entry) =
                self.decode_event_code_from_table(&fc_table)?;
            if !matches!(entry.terminal, Terminal::EndDocument) {
                return Err(Error::invalid_event_code("kein ED erwartet", "SC-Fragment Ende"));
            }
        }

        // Byte-Alignment
        self.reader.align_to_byte();

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
        self.xsi_type_qname = saved.xsi_type_qname;
        self.saw_invalid_xsi_nil = saved.saw_invalid_xsi_nil;
        self.xsi_type_skip_learning_hit = saved.xsi_type_skip_learning_hit;
        self.document_in_content = saved.document_in_content;
        self.event_buffer = saved.event_buffer;
        self.finished = saved.finished;

        // Grammar-Cache invalidieren: element_grammars wurde restored
        self.bump_grammar_epoch();

        // Das äußere Element poppen (das SE das den SC getriggert hat)
        self.pop_element_grammar();
        // xsi-Flags auf Parent setzen
        self.xsi_nil_active = false;
        self.xsi_type_seen = false;
        self.xsi_type_qname = None;
        self.current_element_type = self
            .element_stack
            .last()
            .and_then(|elem| elem.element_type.clone());

        Ok(())
    }

    /// Decodiert einen einfachen Event-Code (ohne Production-Map).
    fn decode_event_code_simple(
        &mut self,
        ctx: &EventCodeContext,
    ) -> Result<EventCode> {
        let bits = ctx.bits_for_part1();
        if bits == 0 {
            return Ok(EventCode::one(0));
        }
        match self.options.effective_alignment() {
            Alignment::BitPacked => {
                let value = self.reader.read_bits(bits)?;
                Ok(EventCode::one(value as u32))
            }
            Alignment::ByteAlignment | Alignment::PreCompression => {
                self.reader.align_to_byte();
                let value = self.read_aligned_part(bits.div_ceil(8))?;
                Ok(EventCode::one(value))
            }
        }
    }

    /// Liest einen byte-aligned Event Code Teil.
    ///
    /// Spec 7.1.9: "Bytes are ordered with the least significant byte first."
    /// (Little-Endian für byte-aligned)
    fn read_aligned_part(&mut self, num_bytes: u8) -> Result<u32> {
        if num_bytes == 0 {
            return Ok(0);
        }
        let mut value = 0u32;
        for i in 0..num_bytes {
            let byte = self.reader.read_byte_aligned()?;
            value |= (byte as u32) << (i * 8);
        }
        Ok(value)
    }

    /// Decodiert einen Event Code anhand der bekannten Event Codes.
    ///
    /// Diese Methode vermeidet Borrow-Probleme, indem sie die Event Codes
    /// als Parameter erhält statt das NonTerminal zu borgen.
    ///
    /// # Spec 6.2: Per-Sibling Bitbreiten
    ///
    /// Decodiert einen Event Code für Schema-informed Grammars (inkl. Tier 2).
    ///
    /// Verwendet ProductionTable für Level-1 Lookup (O(1) Array-Zugriff).
    /// Part2-Bitbreiten kommen aus dem Tier2Context.
    fn decode_schema_informed_event_code(
        &mut self,
        production_table: &ProductionTable,
        tier2_ctx: &Tier2Context,
        current_nt: NonTerminalId,
    ) -> Result<(EventCode, Terminal, Option<NonTerminalId>, bool)> {

        // Byte-Alignment: Auf Byte-Grenze ausrichten
        if matches!(
            self.options.effective_alignment(),
            Alignment::ByteAlignment | Alignment::PreCompression
        ) {
            self.reader.align_to_byte();
        }

        let trace_ec = self.trace.event_code;
        let trace_bytes = self.trace.event_bytes;

        // Part1: Level-1 Einträge + 1 Escape-Code zu Tier 2
        let escape_part1 = production_table.level1_count();
        let part1_count = escape_part1 + 1;
        let bp1 = bits_for_part(part1_count);
        let byp1 = bytes_for_part(part1_count);

        let part1 = self.decode_event_code_part(bp1, byp1)?;
        if trace_ec {
            eprintln!(
                "decode_event_code: part1={} escape={} has_second_level=true bit_pos={}",
                part1,
                escape_part1,
                self.bit_position()
            );
        }
        if trace_bytes {
            eprintln!(
                "event_code_bits: part1 bits={} bytes={} value={}",
                bp1, byp1, part1
            );
        }

        if part1 < escape_part1 {
            let entry = production_table.get_level1(part1)
                .ok_or_else(|| Error::invalid_event_code("Tier1-Production nicht gefunden", "schema_informed_ec"))?;
            let was_wildcard = entry.terminal.is_wildcard();
            return Ok((EventCode::one(part1), entry.terminal, entry.rhs, was_wildcard));
        }

        if part1 != escape_part1 {
            return Err(Error::invalid_event_code("Part1 ohne Second Level", "schema_informed_ec"));
        }

        // Part2: Bitbreite aus Tier2-Count
        let t2_count = tier2_ctx.count();
        let bits_part2 = bits_for_part(t2_count);
        let bytes_part2 = if matches!(
            self.options.effective_alignment(),
            Alignment::ByteAlignment | Alignment::PreCompression
        ) {
            bytes_for_part(t2_count)
        } else {
            0
        };
        let part2 = self.decode_event_code_part(bits_part2, bytes_part2)?;
        if trace_ec {
            eprintln!("decode_event_code: part2={} bit_pos={}", part2, self.bit_position());
        }
        if trace_bytes {
            eprintln!(
                "event_code_bits: part2 bits={} bytes={} value={}",
                bits_part2,
                bytes_part2,
                part2
            );
        }

        // Spec 8.5.4.4.1: CM/PI Sub-Gruppe — part2 zeigt auf den gemeinsamen Slot,
        // part3 bestimmt CM (0) oder PI (1).
        let cm_pi_slot = tier2_ctx.cm_pi_slot();
        let terminal = if cm_pi_slot == Some(part2) {
            let cm_pi_count = tier2_ctx.cm_pi_count();
            let bits_part3 = bits_for_part(cm_pi_count);
            let bytes_part3 = if matches!(
                self.options.effective_alignment(),
                Alignment::ByteAlignment | Alignment::PreCompression
            ) {
                bytes_for_part(cm_pi_count)
            } else {
                0
            };
            let part3 = self.decode_event_code_part(bits_part3, bytes_part3)?;
            if trace_ec {
                eprintln!("decoder: CM/PI sub-group part2={} part3={} cm_pi_count={} bit_pos={}", part2, part3, cm_pi_count, self.bit_position());
            }
            match part3 {
                0 => Some(Terminal::Comment),
                1 => Some(Terminal::ProcessingInstr),
                _ => None,
            }
        } else {
            tier2_ctx.terminal_at(part2)
        };
        let terminal = terminal.ok_or_else(|| Error::invalid_event_code("Tier2-Index ungueltig", "schema_informed_ec"))?;
        let right_hand_side = match terminal {
            Terminal::EndElement => None,
            _ => Some(current_nt),
        };
        let was_wildcard = terminal.is_wildcard();
        Ok((EventCode::two(part1, part2), terminal, right_hand_side, was_wildcard))
    }

    /// Liest einen Event Code Teil (bit-packed oder byte-aligned).
    /// Spec 6.2: Bei compression=true wird byte-aligned encoding verwendet.
    fn decode_event_code_part(&mut self, bits: u8, bytes: u8) -> Result<u32> {
        match self.options.effective_alignment() {
            Alignment::BitPacked => {
                if bits > 0 {
                    Ok(crate::n_bit_unsigned_integer::decode(&mut self.reader, bits)? as u32)
                } else {
                    Ok(0)
                }
            }
            Alignment::ByteAlignment | Alignment::PreCompression => {
                let value = self.read_aligned_part(bytes)?;
                Ok(value)
            }
        }
    }

    /// Decodiert einen Event Code mittels ProductionTable (O(1) Array-Zugriff).
    ///
    /// Verwendet die in der Table vorberechneten Bit-/Byte-Breiten.
    fn decode_event_code_from_table(
        &mut self,
        table: &ProductionTable,
    ) -> Result<(EventCode, ProductionEntry)> {
        self.decode_event_code_from_table_with_bits(
            table,
            table.bits_part1(),
            table.bytes_part1(),
        )
    }

    /// Decodiert einen Event Code mittels ProductionTable mit überschriebenen Part1-Breiten.
    ///
    /// Für Override-Fälle (Tier2)
    /// wo Part1 mit anderen Bitbreiten gelesen werden muss als in der Table vorberechnet.
    fn decode_event_code_from_table_with_bits(
        &mut self,
        table: &ProductionTable,
        bits_part1: u8,
        bytes_part1: u8,
    ) -> Result<(EventCode, ProductionEntry)> {
        // Byte-Alignment
        if matches!(
            self.options.effective_alignment(),
            Alignment::ByteAlignment | Alignment::PreCompression
        ) {
            self.reader.align_to_byte();
        }

        // Teil 1 lesen
        let part1 = self.decode_event_code_part(bits_part1, bytes_part1)?;
        if let Some(entry) = table.get_level1(part1) {
            return Ok((EventCode::one(part1), entry));
        }

        // Teil 2 lesen (Bit-Breite abhängig von part1, Spec 6.2 Sibling-Regel)
        let part2 = self.decode_event_code_part(
            table.bits_for_part2(part1), table.bytes_for_part2(part1),
        )?;
        if let Some(entry) = table.get_level2(part1, part2) {
            return Ok((EventCode::two(part1, part2), entry));
        }

        // Teil 3 lesen (Bit-Breite abhängig von (part1, part2), Spec 6.2)
        let part3 = self.decode_event_code_part(
            table.bits_for_part3(part1, part2), table.bytes_for_part3(part1, part2),
        )?;
        if let Some(entry) = table.get_level3(part1, part2, part3) {
            return Ok((EventCode::three(part1, part2, part3), entry));
        }

        Err(Error::invalid_event_code("3-Teil-Code ungueltig", "decode_event_code_from_table"))
    }

    /// Liest den Part3-Index für AT(WildcardUntyped).
    ///
    /// Wenn das aktuelle NT deklarierte Attribute hat, wird ein 3. Level-Index
    /// gelesen, der das konkrete Attribut bestimmt (Spec 8.5.4.4.1).
    fn decode_at_invalid_value_part3(
        &mut self,
        current_nt: NonTerminalId,
    ) -> Result<Option<(Terminal, Option<NonTerminalId>)>> {
        let (grammar_key, _) = self
            .element_stack
            .last()
            .map(|e| (e.grammar_key, e.current_nt))
            .ok_or_else(|| Error::invalid_event_code("", "leerer Element-Stack (AT-Part3)"))?;
        let grammar = self
            .element_grammars
            .get(&grammar_key)
            .ok_or_else(|| Error::invalid_event_code("", "Grammar nicht gefunden (AT-Part3)"))?;
        let nt_ref = grammar
            .get(current_nt)
            .ok_or_else(|| Error::UnknownNonTerminal(format!("{:?}", current_nt)))?;

        let declared_attrs = nt_ref.declared_attributes(&self.interner);
        let declared_len = declared_attrs.len();
        if declared_len == 0 {
            return Ok(None);
        }
        let bits = bits_for_value(declared_len + 1);
        let idx = self.decode_n_bit(bits)? as usize;
        if idx < declared_len {
            let qname = declared_attrs[idx].0;
            let rhs = declared_attrs[idx].1;
            Ok(Some((
                Terminal::Attribute(AttributeKind::QNameUntyped(qname)),
                rhs.or(Some(current_nt)),
            )))
        } else {
            Ok(Some((
                Terminal::Attribute(AttributeKind::WildcardUntyped),
                Some(current_nt),
            )))
        }
    }

    /// Decodiert einen n-bit Wert unter Berücksichtigung des Alignment-Modus.
    ///
    /// Bei BitPacked: n Bits werden gelesen.
    /// Bei ByteAlignment/PreCompression: ceil(n/8) Bytes werden gelesen (Spec 7.1.9).
    /// Spec 6.2: Bei compression=true wird byte-aligned encoding verwendet.
    fn decode_n_bit(&mut self, n: u8) -> Result<u64> {
        match self.options.effective_alignment() {
            Alignment::BitPacked => crate::n_bit_unsigned_integer::decode(&mut self.reader, n),
            Alignment::ByteAlignment | Alignment::PreCompression => {
                if n == 0 {
                    return Ok(0);
                }
                let num_bytes = n.div_ceil(8);
                let mut value = 0u64;
                for i in 0..num_bytes {
                    let byte = self.reader.read_byte_aligned()?;
                    value |= (byte as u64) << (i * 8);
                }
                Ok(value)
            }
        }
    }

    /// Pusht ein neues Element auf den Stack.
    ///
    /// Bei Schema-informed Mode (Schema vorhanden) wird eine Schema-informed
    /// Element Grammar basierend auf dem Element-Typ verwendet (Spec 8.5.4).
    /// Ansonsten wird die Built-in Element Grammar verwendet (Spec 8.4.3).
    fn push_element(&mut self, qname: &QName, expanded_hint: Option<ExpandedNameId>) -> Result<()> {
        let mut grammar_source = "built-in";
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
        let allow_element_fragment = self.element_stack.is_empty() || parent_in_element_fragment;
        let expanded = self.resolve_expanded_hint(qname, expanded_hint)?;

        if allow_element_fragment
            && self.options.fragment
            && let Some(ref schema) = self.schema
            && schema.is_element_fragment_relaxed_element(qname)
        {
            let local_id = self.next_local_grammar_id;
            self.next_local_grammar_id = self.next_local_grammar_id.wrapping_add(1);
            let key = GrammarKey {
                qname: expanded,
                local_id: Some(local_id),
            };
            let mut elem_grammar = GrammarSystem::schema_informed_element_fragment(
                schema,
                &mut self.interner,
            )?;
            elem_grammar.augment_element_fragment(&self.options, &mut self.interner)?;
            elem_grammar.prune(&self.options)?;
            let start = elem_grammar.start();
            self.element_grammars.insert(key, Rc::new(elem_grammar));
            self.bump_grammar_epoch();

            if self.trace.push {
                eprintln!("push_element: qname={:?} source=element-fragment", qname);
            }

            self.current_element_type = None;
            self.element_stack.push(ElementContext {
                expanded_name: expanded,
                element_type: None,
                // Spec 8.5.3: Element Fragment Grammar wird wie nillable=true
                // und has_named_sub_types=true behandelt (für Undeclared Productions).
                is_nillable: true,
                needs_xsi_type: true,
                pending_type_switch: None,
                grammar_key: key,
                current_nt: start,
                in_content: false,
                all_group: None,
            });
            return Ok(());
        }

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
            // Substitution Group: find_element_decl_in_type kann den Head
            // der Substitution Group zurückgeben (z.B. Qb statt Qz). Der
            // Head hat einen anderen Typ → falsche Grammar. Bei QName-
            // Mismatch die globale Declaration des tatsächlichen Elements
            // bevorzugen.
            if let Some(decl) = local_decl
                && decl.qname.as_ref() != qname
                && let Some(g) = schema.get_element(qname) {
                    global_decl = Some(g.clone());
                    local_decl = None;
                }
            if local_decl.is_none() && global_decl.is_none() {
                global_decl = schema.get_element(qname).cloned();
            }
        }
        let (grammar_key, start_nt) = if let Some(local_decl) = local_decl {
            grammar_source = "schema-local";
            let local_id = self.next_local_grammar_id;
            self.next_local_grammar_id = self.next_local_grammar_id.wrapping_add(1);
            let key = GrammarKey {
                qname: expanded,
                local_id: Some(local_id),
            };
            if self.trace.push {
                eprintln!("push_element: building local grammar for {:?}", qname);
            }
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
                    grammar_source = "schema-global";
                    if self.trace.push {
                        eprintln!("push_element: building global grammar for {:?}", qname);
                    }
                    cached_schema_grammar(&mut self.grammar_template_cache, elem_decl, &self.options, &mut self.interner)?
                } else {
                    let mut g = GrammarSystem::built_in_element(&self.options);
                    g.prune(&self.options)?;
                    Rc::new(g)
                };
                let start = grammar_arc.start();
                self.element_grammars.insert(key, grammar_arc);
                self.bump_grammar_epoch();
                (key, start)
            }
        };

        if self.trace.push {
            eprintln!("push_element: qname={:?} source={}", qname, grammar_source);
        }

        // Bei Schema-informed: Element-Typ und Flags aus der Declaration ableiten
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

        let all_group = if let Some(td) = elem_type.as_ref() {
            Self::all_group_state_from_type(td.as_ref(), &mut self.interner)?
        } else {
            None
        };

        self.element_stack.push(ElementContext {
            expanded_name: expanded,
            element_type: elem_type,
            is_nillable,
            needs_xsi_type,
            pending_type_switch: None,
            grammar_key,
            current_nt: start_nt,
            in_content: false,
            all_group,
        });

        Ok(())
    }

    /// Grammar Evolution für SE(*) in Document Grammar (Spec 8.4.3).
    ///
    /// Bei Fragment Grammar wird SE(qname) in FragmentContent gelernt, wenn SE(*)
    /// gematcht wird. Dies ermöglicht, dass wiederholte Top-Level-Elemente mit
    /// kürzeren Event Codes encodiert werden.
    fn evolve_document_grammar(&mut self, qname: &QName) -> Result<()> {
        let expanded = self.interner.intern_expanded(&qname.uri, &qname.local_name)?;
        let learned_terminal =
            Terminal::StartElement(StartElementKind::QName(expanded));

        let nt = Rc::make_mut(&mut self.document_grammar).get_mut(self.document_nt)
            .ok_or_else(|| Error::invalid_event_code("", "Document-Grammar NT nicht gefunden"))?;
        // SE(qname) mit Event Code [0] lernen, alle anderen inkrementieren
        GrammarEvolution::learn_fragment_se(nt, learned_terminal);

        Ok(())
    }

    /// Prüft ob die Grammar für den gegebenen Key eine Built-in Element Grammar ist.
    ///
    /// Bei Schema-informed Mode soll Grammar Evolution nur für Built-in Element
    /// Grammars durchgeführt werden (nicht für Schema-informed Grammars).
    /// Gibt `true` zurück wenn Evolution erlaubt ist, `false` wenn nicht.
    fn is_built_in_element_grammar(&self, grammar_key: &GrammarKey) -> bool {
        if self.schema.is_none() {
            return true;
        }
        self.element_grammars
            .get(grammar_key)
            .is_some_and(|g| g.grammar_type() == crate::grammar::GrammarType::Element)
    }

    /// Grammar Evolution bei CH/EE mit Event Code Länge > 1 (Spec 8.4.3).
    ///
    /// "If the matched terminal symbol is CH [...] or EE and the event code is more than
    /// one part, the associated grammar is augmented as follows:
    /// Add a production for CH [...] or EE with an event code of 0 (zero)..."
    fn evolve_content_grammar(
        &mut self,
        grammar_key: &GrammarKey,
        current_nt: NonTerminalId,
        terminal: &Terminal,
        next_nt: Option<NonTerminalId>,
    ) -> Result<()> {
        if !self.is_built_in_element_grammar(grammar_key) {
            return Ok(());
        }

        let elem_grammar = self
            .element_grammars
            .get_mut(grammar_key)
            .ok_or_else(|| Error::invalid_event_code("", "Grammar nicht gefunden (Content-Evolution)"))?;

        let nt = Rc::make_mut(elem_grammar)
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
    /// # Spec-Referenz
    ///
    /// Spec 8.4.3: "Create a production of the form LeftHandSide : SE (qname) RightHandSide
    /// with an event code 0. Increment the first part of the event code of each production
    /// in the current grammar with the non-terminal LeftHandSide on the left-hand side."
    ///
    /// Das bedeutet: SE(qname) wird nur im aktuellen NonTerminal gelernt (LeftHandSide),
    /// nicht in allen NonTerminals.
    fn evolve_element_grammar(
        &mut self,
        grammar_key: &GrammarKey,
        current_nt: NonTerminalId,
        event: &ExiEvent,
    ) -> Result<()> {
        if !self.is_built_in_element_grammar(grammar_key) {
            return Ok(());
        }

        let elem_grammar = self
            .element_grammars
            .get_mut(grammar_key)
            .ok_or_else(|| Error::invalid_event_code("", "Grammar nicht gefunden (Element-Evolution)"))?;

        match event {
            ExiEvent::StartElement(qname) => {
                // SE(*) → SE(qname) nur im aktuellen NonTerminal lernen (Spec 8.4.3)
                let expanded = self.interner.intern_expanded(&qname.uri, &qname.local_name)?;
                let learned_terminal =
                    Terminal::StartElement(StartElementKind::QName(expanded));

                let nt = Rc::make_mut(elem_grammar)
                    .get_mut(current_nt)
                    .ok_or_else(|| Error::UnknownNonTerminal(format!("{:?}", current_nt)))?;
                GrammarEvolution::learn_element_se(nt, learned_terminal);
            }
            ExiEvent::Attribute(content) => {
                // AT(*) → AT(qname) nur in StartTagContent lernen
                // (Attribute kommen nur in StartTagContent vor)
                let expanded = self.interner.intern_expanded(&content.qname.uri, &content.qname.local_name)?;
                // Spec 8.5.4.4.1 (Z.3134): xsi:type und xsi:nil dürfen in
                // schema-informed Grammars NICHT über AT(*) repräsentiert werden.
                // Learning nach AT(*)-Match wird daher unterdrückt.
                if self.schema.is_some()
                    && (expanded == self.xsi_type_id || expanded == self.xsi_nil_id)
                {
                    return Ok(());
                }
                let learned_terminal = Terminal::at_expanded(expanded);
                let stc = Rc::make_mut(elem_grammar)
                    .get_mut(NonTerminalId::StartTagContent)
                    .ok_or_else(|| Error::UnknownNonTerminal("StartTagContent".to_string()))?;
                GrammarEvolution::learn_attribute(stc, learned_terminal, &mut self.interner);
            }
            _ => {
                // Wildcard-Match mit unerwartetem Event - sollte nie auftreten
                return Err(Error::invalid_event_code("kein SE/AT nach Wildcard", "evolve_element_grammar"));
            }
        }

        self.bump_grammar_epoch();
        Ok(())
    }

    /// Decodiert den Content eines Events basierend auf dem Terminal (Document-Kontext).
    fn decode_event_content(&mut self, terminal: &Terminal) -> Result<ExiEvent> {
        match terminal {
            Terminal::StartDocument => {
                crate::event_content::decode_start_document(&mut self.reader)?;
                Ok(ExiEvent::StartDocument)
            }
            Terminal::EndDocument => {
                crate::event_content::decode_end_document(&mut self.reader)?;
                Ok(ExiEvent::EndDocument)
            }
            Terminal::StartElement(kind) => {
                // Bei SE(qname) ist der QName implizit durch Event Code bekannt.
                // Bei SE(*) wird der QName aus dem Stream gelesen.
                // Bei SE(uri:*) ist die URI implizit, nur LocalName (und ggf. Prefix) wird gelesen.
                let qname = match kind {
                    StartElementKind::QName(q) => {
                        let prefix = self.resolve_prefix_for_known_qname(*q)?;
                        self.qname_pool.get_or_create_with_prefix(*q, prefix, &self.interner)
                    }
                    StartElementKind::Wildcard => Rc::new(self.decode_qname()?),
                    StartElementKind::NamespaceWildcard(uri) => {
                        Rc::new(self.decode_qname_with_implied_uri(*uri)?)
                    }
                };
                Ok(ExiEvent::StartElement(qname))
            }
            Terminal::EndElement => {
                crate::event_content::decode_end_element(&mut self.reader)?;
                Ok(ExiEvent::EndElement)
            }
            Terminal::Attribute(kind) => {
                // QName + Value aus Stream decodieren (Spec 7.3.3)
                let trace_attr = self.trace.attr;
                let before_qname = if trace_attr {
                    Some(self.bit_position())
                } else {
                    None
                };
                let qname: Rc<QName> = match kind {
                    AttributeKind::QName(q) | AttributeKind::QNameUntyped(q) => {
                        let prefix = self.resolve_prefix_for_known_qname(*q)?;
                        self.qname_pool.get_or_create_with_prefix(*q, prefix, &self.interner)
                    }
                    AttributeKind::Wildcard | AttributeKind::WildcardUntyped => Rc::new(self.decode_qname()?),
                    AttributeKind::NamespaceWildcard(uri) => {
                        Rc::new(self.decode_qname_with_implied_uri(*uri)?)
                    }
                };
                if let Some(before) = before_qname {
                    eprintln!(
                        "decode_attr: qname={:?} bits {} -> {}",
                        qname,
                        before,
                        self.bit_position()
                    );
                }

                // Spec 8.5.4.4: xsi:type Wert ist IMMER QName (7.1.7), auch bei built-in grammar.
                // Spec 8.5.4.4: "In a schema-informed grammar, [...] AT(xsi:nil) [...] assign
                // it the Boolean datatype representation" - bei built-in grammar wird xsi:nil
                // über AT(*) als untyped value (String) repräsentiert.
                let before_value = if trace_attr {
                    Some(self.bit_position())
                } else {
                    None
                };
                let value = if matches!(kind, AttributeKind::QNameUntyped(_)) {
                    self.decode_or_skip_value(&qname, None)?
                } else if qname.is_xsi_type() {
                    self.decode_xsi_type_value()?
                } else if qname.is_xsi_nil()
                    && self.schema.is_some()
                    && !Self::attr_kind_is_untyped(kind)
                {
                    self.decode_xsi_nil_value()?
                } else {
                    let in_element_fragment =
                        self.document_grammar.grammar_type() == GrammarType::ElementFragment;
                    if trace_attr {
                        eprintln!(
                            "decode_attr(elem): current_element_type_present={} qname={:?}",
                            self.current_element_type.is_some(),
                            qname
                        );
                    }
                    let mut attr_type = if matches!(
                        kind,
                        AttributeKind::WildcardUntyped | AttributeKind::QNameUntyped(_)
                    ) {
                        None
                    } else {
                        self.current_element_type
                            .as_ref()
                            .and_then(|et| et.get_attribute_type(&qname).cloned())
                    };
                    if let (Some(schema), Some(td)) = (self.schema.as_ref(), attr_type.as_ref())
                        && let Some(name) = td.name()
                            && let Some(full) = schema.get_type(name) {
                                attr_type = Some(full.clone());
                            }
                    let mut attr_type_from_wildcard = false;
                    if attr_type.is_none()
                        && !matches!(
                            kind,
                            AttributeKind::WildcardUntyped | AttributeKind::QNameUntyped(_)
                        )
                        && let Some(schema) = self.schema.as_ref()
                            && let Some(td) = schema.get_global_attribute_type(&qname) {
                                attr_type = Some(td.clone());
                                if matches!(
                                    kind,
                                    AttributeKind::Wildcard | AttributeKind::NamespaceWildcard(_)
                                ) {
                                    attr_type_from_wildcard = true;
                                }
                            }
                    if in_element_fragment
                        && let Some(schema) = self.schema.as_ref() {
                            if schema.is_element_fragment_relaxed_attribute(&qname) {
                                attr_type = None;
                            } else if let Some(td) =
                                schema.element_fragment_attribute_type(&qname)
                            {
                                attr_type = Some(td.clone());
                            }
                        }
                    if trace_attr {
                        let base_type = attr_type
                            .as_ref()
                            .and_then(|td| crate::typed_value::resolve_base_type(td));
                        eprintln!(
                            "decode_attr(elem): attr_type_present={} base_type={:?} qname={:?}",
                            attr_type.is_some(),
                            base_type,
                            qname
                        );
                    }
                    self.decode_attr_value_with_fallback(
                        &qname, attr_type.as_deref(), attr_type_from_wildcard, kind,
                    )?
                };
                if let Some(before) = before_value {
                    eprintln!(
                        "decode_attr: value bits {} -> {}",
                        before,
                        self.bit_position()
                    );
                }

                // Spec 8.5.4.4.2: xsi:nil Handling (nur bei Schema-informed)
                if self.schema.is_some() && qname.is_xsi_nil() {
                    self.handle_xsi_nil(&value)?;
                }

                // Spec 8.5.4.4: xsi:type Handling (nur bei Schema-informed)
                if self.schema.is_some() && qname.is_xsi_type() {
                    self.handle_xsi_type(&value)?;
                }

                Ok(ExiEvent::Attribute(AtContent { qname, value }))
            }
            Terminal::Characters => {
                // Value mit String Table (Spec 7.3.3)
                // Der assoziierte QName ist das aktuelle Element
                let elem_qname = self.current_element_qname()?;
                // CH: Typed Value wenn current_element_type vorhanden
                let type_def = self.current_element_type.clone();
                let value = self.decode_or_skip_value(&elem_qname, type_def.as_deref())?;
                Ok(ExiEvent::Characters(ChContent { value }))
            }
            Terminal::CharactersUntyped => {
                let elem_qname = self.current_element_qname()?;
                let value = self.decode_or_skip_value(&elem_qname, None)?;
                Ok(ExiEvent::Characters(ChContent { value }))
            }
            Terminal::NamespaceDecl => {
                // NS Content: uri, prefix, local-element-ns (Table 4-2, Spec 7.3.2)
                let content = self.decode_namespace_declaration()?;
                Ok(ExiEvent::NamespaceDeclaration(content))
            }
            Terminal::Comment => {
                let content = crate::event_content::decode_comment(&mut self.reader)?;
                Ok(ExiEvent::Comment(content))
            }
            Terminal::ProcessingInstr => {
                let content =
                    crate::event_content::decode_processing_instruction(&mut self.reader)?;
                Ok(ExiEvent::ProcessingInstruction(content))
            }
            Terminal::DocType => {
                let content = crate::event_content::decode_doctype(&mut self.reader)?;
                Ok(ExiEvent::DocType(content))
            }
            Terminal::EntityRef => {
                let content = crate::event_content::decode_entity_reference(&mut self.reader)?;
                Ok(ExiEvent::EntityReference(content))
            }
            Terminal::SelfContained => Ok(ExiEvent::SelfContained),
        }
    }

    /// Decodiert den Content eines Events im Element-Kontext.
    ///
    /// Spec 8.4.3: Bei SE(qname)/AT(qname) ist der QName implizit durch den Event Code bekannt.
    /// Nur bei SE(*)/AT(*) wird der QName aus dem Stream gelesen.
    fn decode_event_content_element(
        &mut self,
        terminal: &Terminal,
        is_tier2: bool,
    ) -> Result<ExiEvent> {
        match terminal {
            Terminal::StartElement(kind) => {
                let qname = match kind {
                    StartElementKind::QName(q) => {
                        let prefix = self.resolve_prefix_for_known_qname(*q)?;
                        self.qname_pool.get_or_create_with_prefix(*q, prefix, &self.interner)
                    }
                    StartElementKind::Wildcard => {
                        if matches!(self.options.effective_alignment(), Alignment::ByteAlignment) {
                            self.reader.align_to_byte();
                        }
                        Rc::new(self.decode_qname()?)
                    }
                    StartElementKind::NamespaceWildcard(uri) => {
                        if matches!(self.options.effective_alignment(), Alignment::ByteAlignment) {
                            self.reader.align_to_byte();
                        }
                        Rc::new(self.decode_qname_with_implied_uri(*uri)?)
                    }
                };
                Ok(ExiEvent::StartElement(qname))
            }
            Terminal::Attribute(kind) => {
                let trace_attr = self.trace.attr;
                let before_qname = if trace_attr {
                    Some(self.bit_position())
                } else {
                    None
                };
                let qname: Rc<QName> = match kind {
                    AttributeKind::QName(q) | AttributeKind::QNameUntyped(q) => {
                        let prefix = self.resolve_prefix_for_known_qname(*q)?;
                        self.qname_pool.get_or_create_with_prefix(*q, prefix, &self.interner)
                    }
                    AttributeKind::Wildcard | AttributeKind::WildcardUntyped => {
                        if matches!(self.options.effective_alignment(), Alignment::ByteAlignment) {
                            self.reader.align_to_byte();
                        }
                        Rc::new(self.decode_qname()?)
                    }
                    AttributeKind::NamespaceWildcard(uri) => {
                        if matches!(self.options.effective_alignment(), Alignment::ByteAlignment) {
                            self.reader.align_to_byte();
                        }
                        Rc::new(self.decode_qname_with_implied_uri(*uri)?)
                    }
                };
                if let Some(before) = before_qname {
                    eprintln!(
                        "decode_attr(elem): qname={:?} bits {} -> {}",
                        qname,
                        before,
                        self.bit_position()
                    );
                }
                // Spec 8.5.4.4: xsi:type Wert ist IMMER QName (7.1.7), auch bei built-in grammar.
                // Spec 8.5.4.4: "In a schema-informed grammar, [...] AT(xsi:nil) [...] assign
                // it the Boolean datatype representation" - bei built-in grammar wird xsi:nil
                // über AT(*) als untyped value (String) repräsentiert.
                let before_value = if trace_attr {
                    Some(self.bit_position())
                } else {
                    None
                };
                let value = if matches!(kind, AttributeKind::QNameUntyped(_)) {
                    self.decode_or_skip_value(&qname, None)?
                } else if qname.is_xsi_type() {
                    self.decode_xsi_type_value()?
                } else if qname.is_xsi_nil()
                    && self.schema.is_some()
                    && !Self::attr_kind_is_untyped(kind)
                {
                    self.decode_xsi_nil_value()?
                } else {
                    let in_element_fragment = self
                        .element_stack
                        .last()
                        .and_then(|elem| self.element_grammars.get(&elem.grammar_key))
                        .map(|g| g.grammar_type() == GrammarType::ElementFragment)
                        .unwrap_or(false);
                    let mut attr_type = if matches!(
                        kind,
                        AttributeKind::WildcardUntyped | AttributeKind::QNameUntyped(_)
                    ) {
                        None
                    } else {
                        self.current_element_type
                            .as_ref()
                            .and_then(|et| et.get_attribute_type(&qname).cloned())
                    };
                    if let (Some(schema), Some(td)) = (self.schema.as_ref(), attr_type.as_ref())
                        && let Some(name) = td.name()
                            && let Some(full) = schema.get_type(name) {
                                attr_type = Some(full.clone());
                            }
                    let mut attr_type_from_wildcard = false;
                    if attr_type.is_none()
                        && !matches!(
                            kind,
                            AttributeKind::WildcardUntyped | AttributeKind::QNameUntyped(_)
                        )
                        && let Some(schema) = self.schema.as_ref()
                            && let Some(td) = schema.get_global_attribute_type(&qname) {
                                attr_type = Some(td.clone());
                                if matches!(
                                    kind,
                                    AttributeKind::Wildcard | AttributeKind::NamespaceWildcard(_)
                                ) {
                                    attr_type_from_wildcard = true;
                                }
                            }
                    if in_element_fragment
                        && let Some(schema) = self.schema.as_ref() {
                            if schema.is_element_fragment_relaxed_attribute(&qname) {
                                attr_type = None;
                            } else if let Some(td) =
                                schema.element_fragment_attribute_type(&qname)
                            {
                                attr_type = Some(td.clone());
                            }
                        }
                    if trace_attr {
                        let base_type = attr_type
                            .as_ref()
                            .and_then(|td| crate::typed_value::resolve_base_type(td));
                        eprintln!(
                            "decode_attr(elem): attr_type_present={} base_type={:?} qname={:?}",
                            attr_type.is_some(),
                            base_type,
                            qname
                        );
                    }
                    self.decode_attr_value_with_fallback(
                        &qname, attr_type.as_deref(), attr_type_from_wildcard, kind,
                    )?
                };
                if let Some(before) = before_value {
                    eprintln!(
                        "decode_attr(elem): value bits {} -> {}",
                        before,
                        self.bit_position()
                    );
                }

                // Spec 8.5.4.4.2: xsi:nil Handling (nur bei Schema-informed)
                if self.schema.is_some() && qname.is_xsi_nil() {
                    self.handle_xsi_nil(&value)?;
                }

                // Spec 8.5.4.4: xsi:type Handling (nur bei Schema-informed)
                if self.schema.is_some() && qname.is_xsi_type() {
                    self.handle_xsi_type(&value)?;
                }

                Ok(ExiEvent::Attribute(AtContent { qname, value }))
            }
            Terminal::Characters => {
                let elem_qname = self.current_element_qname()?;
                // CH: Bei Tier2 (undeclared) immer untyped, sonst typed falls verfügbar.
                let type_def = if is_tier2 {
                    None
                } else {
                    self.current_element_type.clone()
                };
                let value = self.decode_or_skip_value(
                    &elem_qname,
                    type_def.as_deref(),
                )?;
                Ok(ExiEvent::Characters(ChContent { value }))
            }
            Terminal::CharactersUntyped => {
                let elem_qname = self.current_element_qname()?;
                let value = self.decode_or_skip_value(&elem_qname, None)?;
                Ok(ExiEvent::Characters(ChContent { value }))
            }
            Terminal::EndElement => {
                crate::event_content::decode_end_element(&mut self.reader)?;
                Ok(ExiEvent::EndElement)
            }
            _ => self.decode_event_content(terminal),
        }
    }

    fn log_elem_event(&self, event: &ExiEvent, current_elem: Option<ExpandedNameId>) {
        if !self.trace.elem_event {
            return;
        }
        match event {
            ExiEvent::StartElement(qname) => {
                eprintln!(
                    "elem_event: type=SE qname={qname} uri={} local={}",
                    qname.uri,
                    qname.local_name
                );
            }
            ExiEvent::Attribute(crate::event::AtContent { qname, .. }) => {
                eprintln!(
                    "elem_event: type=AT qname={qname} uri={} local={}",
                    qname.uri,
                    qname.local_name
                );
            }
            ExiEvent::EndElement => {
                if let Some(expanded) = current_elem {
                    let (uri, local) = expanded.resolve(&self.interner);
                    eprintln!(
                        "elem_event: type=EE uri={uri} local={local}",
                    );
                }
            }
            ExiEvent::Characters(_) => {
                eprintln!("elem_event: type=CH");
            }
            _ => {}
        }
    }

    fn log_event_code_from_table(
        &self,
        event_code: &EventCode,
        table: &ProductionTable,
        terminal: &Terminal,
        grammar: GrammarType,
    ) {
        if !self.trace.event_code {
            return;
        }

        eprintln!(
            "decode_event_code: part1={} bits={} num_values={} grammar={:?}",
            event_code.part1(),
            table.bits_part1(),
            table.level1_count(),
            grammar
        );

        if let Some(part2) = event_code.part2() {
            eprintln!(
                "decode_event_code: level=2 part2={} eventType={}",
                part2,
                terminal.debug_event_type()
            );
        }
        if let Some(part3) = event_code.part3() {
            eprintln!(
                "decode_event_code: level=3 part3={} eventType={}",
                part3,
                terminal.debug_event_type()
            );
        }
    }

    /// Gibt den QName des aktuellen Elements zurück (materialisiert aus Pool/Interner).
    fn current_element_qname(&mut self) -> Result<Rc<QName>> {
        let expanded = self.element_stack
            .last()
            .map(|e| e.expanded_name)
            .ok_or_else(|| Error::ordering_violation("Element auf Stack", "leerer Stack"))?;
        Ok(self.qname_pool.get_or_create(expanded, &self.interner))
    }

    /// Bestimmt die xs:all Members (falls direktes all-Group Content).
    fn all_group_state_from_type(td: &TypeDefinition, interner: &mut StringInterner) -> Result<Option<AllGroupState>> {
        let particle = match td {
            TypeDefinition::Complex {
                content: ContentType::ElementOnly(p) | ContentType::Mixed(p),
                ..
            } => p,
            _ => return Ok(None),
        };

        let ParticleTerm::ModelGroup(group) = &particle.term else {
            return Ok(None);
        };
        if group.compositor != crate::schema::Compositor::All {
            return Ok(None);
        }

        let mut members = Vec::new();
        for p in &group.particles {
            match &p.term {
                ParticleTerm::Element(decl) => {
                    for q in decl.matching_qnames() {
                        let expanded = interner.intern_expanded(&q.uri, &q.local_name)?;
                        members.push(expanded);
                    }
                }
                // Wildcards oder verschachtelte Gruppen machen das Mapping unsicher.
                _ => return Ok(None),
            }
        }

        if members.is_empty() {
            return Ok(None);
        }

        Ok(Some(AllGroupState {
            members,
            seen: Vec::new(),
        }))
    }



    /// Decodiert eine URI aus dem Stream (Spec 7.3.2).
    ///
    /// Spec 7.3.2 - Compact Identifier Partition:
    /// - 0: Miss - String Literal folgt
    /// - i+1: Hit - URI mit ID i
    ///
    /// Returns: (uri, uri_id)
    fn decode_uri(&mut self) -> Result<(Rc<str>, usize)> {
        let uri_count = self.string_table.uri_count();
        let n = bits_for_value(uri_count + 1);

        let index = self.decode_n_bit(n)? as usize;
        if self.trace.attr {
            eprintln!("decode_uri: uri_count={} index={}", uri_count, index);
            eprintln!(
                "decode_uri: entries={:?}",
                self.string_table.debug_uri_entries()
            );
        }

        if index == 0 {
            // Miss: String Literal lesen
            let uri = self.decode_limited_string()?;
            let uri_id = self.string_table.decode_uri_miss(&uri);
            Ok((uri.into(), uri_id))
        } else if index > uri_count {
            Err(Error::InvalidCompactId(index - 1))
        } else {
            // Hit: Rc<str> direkt aus String Table
            let uri = self.string_table.decode_uri_hit_rc(index)?;
            Ok((uri, index - 1))
        }
    }

    /// Decodiert einen LocalName aus dem Stream (Spec 7.3.3).
    ///
    /// Spec 7.3.3 - Partitions Optimized for Frequent use of String Literals:
    /// - 0: Hit - Compact ID folgt
    /// - length+1: Miss - String mit `length` Zeichen folgt
    fn decode_local_name(&mut self, uri_id: usize) -> Result<Rc<str>> {
        let length_or_hit = crate::unsigned_integer::decode(&mut self.reader)?;
        if self.trace.attr {
            eprintln!(
                "decode_local_name: uri_id={} length_or_hit={}",
                uri_id, length_or_hit
            );
        }

        if length_or_hit == 0 {
            // Hit: Compact ID als n-Bit lesen
            let local_count = self.string_table.local_name_count(uri_id);
            let n = bits_for_value(local_count);
            let compact_id = self.decode_n_bit(n)? as usize;
            if self.trace.attr {
                eprintln!(
                    "decode_local_name: HIT uri_id={} local_count={} bits={} compact_id={}",
                    uri_id, local_count, n, compact_id
                );
            }
            let local_name = self
                .string_table
                .decode_local_name_hit_rc(uri_id, compact_id)?;
            Ok(local_name)
        } else {
            // Miss: String mit length = length_or_hit - 1 lesen
            let length = (length_or_hit - 1) as usize;
            match self.decode_string_of_length(length) {
                Ok(local_name) => {
                    self.string_table
                        .decode_local_name_miss(uri_id, &local_name);
                    Ok(local_name.into())
                }
                Err(err @ Error::InvalidCodePoint(_)) => {
                    Err(err)
                }
                Err(err) => Err(err),
            }
        }
    }


    /// Löst den Prefix für einen bekannten QName (ExpandedNameId) auf (Spec 7.1.7).
    ///
    /// Gibt `None` zurück wenn `preserve.prefixes` aus ist oder keine Prefixe
    /// für die URI in der String Table vorhanden sind.
    fn resolve_prefix_for_known_qname(
        &mut self,
        expanded: ExpandedNameId,
    ) -> Result<Option<Rc<str>>> {
        if !self.options.preserve.prefixes {
            return Ok(None);
        }
        let uri_str = self.interner.resolve(expanded.uri);
        let Some(uri_id) = self.string_table.lookup_uri(uri_str) else {
            return Ok(None);
        };
        if self.string_table.prefix_count(uri_id) == 0 {
            return Ok(None);
        }
        self.decode_prefix(uri_id).map(Some)
    }

    /// Decodiert einen Prefix aus dem Stream (Spec 7.1.7).
    ///
    /// Anders als LocalName hat Prefix KEINEN "Miss" Fall. Wenn prefix_count=0,
    /// wird der Prefix elided (nicht im Stream). Wenn prefix_count>0, ist es
    /// ein direkter Compact ID ohne Miss-Option.
    fn decode_prefix(&mut self, uri_id: usize) -> Result<Rc<str>> {
        let prefix_count = self.string_table.prefix_count(uri_id);
        debug_assert!(
            prefix_count > 0,
            "decode_prefix sollte nur aufgerufen werden wenn Prefixe existieren"
        );

        // Spec 7.1.7: n = ceiling(log_2(m)) where m is number of entries
        // Bei prefix_count=1: n=0 (nur 1 möglicher Wert)
        // Bei prefix_count=2: n=1 (2 mögliche Werte: 0, 1)
        let n = bits_for_value(prefix_count);

        let compact_id = self.decode_n_bit(n)? as usize;

        // Direkt aus String Table (kein Miss-Fall bei Prefix)
        // decode_prefix_hit erwartet index_plus_one, also compact_id + 1
        self.string_table
            .decode_prefix_hit_rc(uri_id, compact_id + 1)
    }

    /// Decodiert NS (Namespace Declaration) Content.
    ///
    /// # Spec-Referenz
    /// - Table 4-2: NS Content Items (uri, prefix, local-element-ns)
    /// - Spec 7.3.2: URI/Prefix Compact IDs
    /// - Spec 7.1.7: QName prefix encoding
    ///
    /// WICHTIG: Bei NS-Events hat Prefix eine Miss-Option (0=Miss, 1+=Hit), anders als
    /// bei QName-Prefix (wo es keinen Miss gibt, wenn prefix_count > 0).
    fn decode_namespace_declaration(&mut self) -> Result<NsContent> {
        // 1. URI decodieren (wie in QName)
        let (uri, uri_id) = self.decode_uri()?;

        // 2. Prefix decodieren (MIT Miss-Option)
        // Spec: 0 = Miss (String Literal folgt), 1+ = Hit
        let prefix = self.decode_prefix_with_miss(uri_id)?;

        // 3. local-element-ns decodieren (alignment-aware)
        let local_element_ns = self.decode_n_bit(1)? == 1;

        Ok(NsContent {
            uri,
            prefix,
            local_element_ns,
        })
    }

    fn attr_kind_is_untyped(kind: &AttributeKind) -> bool {
        matches!(kind, AttributeKind::QNameUntyped(_) | AttributeKind::WildcardUntyped)
    }

    /// Decodiert einen Prefix mit Miss-Option (für NS-Events).
    ///
    /// Anders als `decode_prefix` (für QName), hat dieser Modus eine Miss-Option:
    /// - 0: Miss - String Literal folgt
    /// - 1+: Hit - Prefix aus String Table
    fn decode_prefix_with_miss(&mut self, uri_id: usize) -> Result<Rc<str>> {
        let prefix_count = self.string_table.prefix_count(uri_id);
        let n = bits_for_value(prefix_count + 1);

        let index = self.decode_n_bit(n)? as usize;

        if index == 0 {
            // Miss: String Literal lesen
            let prefix = self.decode_limited_string()?;
            self.string_table.decode_prefix_miss(uri_id, &prefix);
            Ok(prefix.into())
        } else {
            // Hit: Rc<str> direkt aus String Table
            self.string_table.decode_prefix_hit_rc(uri_id, index)
        }
    }

    /// Decodiert einen längenpräfixierten String mit optionalem Längenlimit (DoS-Schutz).
    fn decode_limited_string(&mut self) -> Result<String> {
        let len = crate::unsigned_integer::decode(&mut self.reader)?;
        self.decode_string_of_length(len as usize)
    }


    /// Decodiert einen Value oder speichert einen Platzhalter (für PreCompression).
    ///
    /// Bei PreCompression (skip_values=true) wird kein Value gelesen, sondern ein
    /// PendingValue gespeichert. Der Value wird später aus dem Value Channel gelesen.
    ///
    /// Decodiert einen Attribut-Wert mit Typed-Value-Fallback (Spec 8.5.4.1.3.2, 8.5.4.4.1).
    ///
    /// Bei fehlgeschlagenem Typed-Value-Decode wird per Checkpoint-Rollback
    /// auf untyped value zurückgefallen, wenn:
    /// - der Typ von einem Wildcard stammt (Spec 8.5.4.1.3.2), oder
    /// - AT(qname) bei strict=false (Spec 8.5.4.4.1).
    fn decode_attr_value_with_fallback(
        &mut self,
        qname: &QName,
        attr_type: Option<&TypeDefinition>,
        attr_type_from_wildcard: bool,
        kind: &AttributeKind,
    ) -> Result<Rc<str>> {
        let value_cp = self.reader.save_checkpoint();
        let mut result = self.decode_or_skip_value(qname, attr_type);
        if !self.skip_values && is_typed_value_error(&result) {
            if self.trace.attr {
                if let Err(err) = &result {
                    eprintln!(
                        "decode_attr(elem): typed decode failed for {:?}: {:?}",
                        qname, err
                    );
                }
            }
            let should_retry = if attr_type_from_wildcard
                && matches!(
                    kind,
                    AttributeKind::Wildcard | AttributeKind::NamespaceWildcard(_)
                ) {
                // Spec 8.5.4.1.3.2 (Attribute Wildcards):
                // Wenn der Wert nicht mit dem globalen Typ darstellbar ist,
                // wird er als untyped value repräsentiert.
                true
            } else {
                // Spec 8.5.4.4.1: AT(qname) [untyped] bei strict=false zulassen,
                // wenn der typed value nicht decodierbar ist.
                !self.options.strict
                    && matches!(kind, AttributeKind::QName(_))
                    && attr_type.is_some()
            };
            if should_retry {
                self.reader.restore_checkpoint(value_cp);
                result = self.decode_or_skip_value(qname, None);
            }
        }
        result
    }

    /// # Spec-Referenz
    /// - Spec 9.2.1: Structure Channel (AT/CH Values nicht enthalten)
    /// - Spec 9.2.2: Value Channels (AT/CH Values hier)
    fn decode_or_skip_value(&mut self, qname: &QName, type_def: Option<&TypeDefinition>) -> Result<Rc<str>> {
        if self.skip_values {
            // Channel-Index: QName deduplizieren (wie Encoder)
            let channel_idx = match self.pending_channel_index.get(&qname.identity_hash()) {
                Some(&idx) => idx,
                None => {
                    let idx = self.pending_channel_qnames.len() as u16;
                    self.pending_channel_qnames.push(qname.clone());
                    self.pending_channel_index.insert(qname.identity_hash(), idx);
                    idx
                }
            };
            // Type-Index: TypeDef deduplizieren
            // Dedup auf dem originalen &TypeDefinition-Pointer (stabil, da aus Arc im Grammar).
            // resolve_type_for_precompression wird nur beim ersten Vorkommen aufgerufen.
            let type_idx = if let Some(td) = type_def {
                let ptr = td as *const _ as usize;
                if let Some(&idx) = self.pending_type_ptr_index.get(&ptr) {
                    idx
                } else {
                    let resolved = self.resolve_type_for_precompression(Some(td))
                        .expect("type_def ist Some, resolve muss auch Some liefern");
                    let idx = (self.pending_type_defs.len() + 1) as u16;
                    self.pending_type_defs.push(resolved);
                    self.pending_type_ptr_index.insert(ptr, idx);
                    idx
                }
            } else {
                0
            };
            self.pending_values.push(PendingValue {
                event_index: self.precompression_event_count as u32,
                data_idx: 0, // Platzhalter, wird in decode_structure_phase_compact nach push() gesetzt
                channel_idx,
                type_idx,
            });
            // Platzhalter (wird später ersetzt)
            Ok(Rc::from(""))
        } else {
            // Normaler Pfad: type_def direkt durchreichen, kein Clone/Arc nötig
            self.decode_value_typed_or_string(qname, type_def)
        }
    }

    /// Resolved type_def in owned Arc für PreCompression.
    fn resolve_type_for_precompression(&self, type_def: Option<&TypeDefinition>) -> Option<Rc<TypeDefinition>> {
        let td = type_def?;
        if let Some(schema) = self.schema.as_ref()
            && let Some(name) = td.name()
            && let Some(arc_td) = schema.get_type(name)
        {
            return Some(arc_td.clone());
        }
        Some(Rc::new(td.clone()))
    }


    /// Erstellt den Tier2Context für Undeclared Productions (Spec 8.5.4.4).
    fn create_tier2_context(&self, snap: &ElementSnapshot) -> Result<Tier2Context> {
        let elem_grammar = self
            .element_grammars
            .get(&snap.grammar_key)
            .ok_or_else(|| Error::invalid_event_code("", "Grammar nicht gefunden (Tier2-Kontext)"))?;
        let nt = elem_grammar.get(snap.current_nt).ok_or_else(|| {
            Error::UnknownNonTerminal(format!("{:?}", snap.current_nt))
        })?;

        let has_ee_in_tier1 = nt.has_ee_length_one();
        let is_content_area = nt.is_content_area() || snap.in_content;

        let is_first_nt = matches!(
                snap.current_nt,
                NonTerminalId::SchemaType(0)
                    | NonTerminalId::ElementFragment0
                    | NonTerminalId::ElementFragmentTypeEmpty0
            ) || Some(snap.current_nt) == elem_grammar.type_empty_start();
        let is_element_fragment = elem_grammar.grammar_type() == GrammarType::ElementFragment;

        Ok(Tier2Context {
            has_ee_in_tier1,
            is_first_nt,
            is_content_area,
            needs_xsi_type: snap.needs_xsi_type,
            is_nillable: snap.is_nillable,
            is_element_fragment,
            xsi_type_id: self.xsi_type_id,
            xsi_nil_id: self.xsi_nil_id,
            ..Tier2Context::new(&self.options)
        })
    }

    /// Cached Version von `create_tier2_context()`.
    ///
    /// Prüft den 64-Slot Direct-Mapped Cache auf Hit anhand von
    /// (grammar_key, current_nt, in_content, needs_xsi_type, is_nillable, epoch).
    /// Bei Miss wird `create_tier2_context()` aufgerufen und das Ergebnis gecached.
    fn cached_tier2_context(&mut self, snap: &ElementSnapshot) -> Result<Tier2Context> {
        let slot = {
            let nt_bits = compact_nt_bits(snap.current_nt);
            let h = snap.grammar_key.qname.uri.0
                ^ snap.grammar_key.qname.local_name.0
                ^ nt_bits;
            h as usize % TIER2_CACHE_SLOTS
        };
        if let Some(ref cache) = self.tier2_cache[slot] {
            if cache.grammar_key == snap.grammar_key
                && cache.current_nt == snap.current_nt
                && cache.in_content == snap.in_content
                && cache.needs_xsi_type == snap.needs_xsi_type
                && cache.is_nillable == snap.is_nillable
                && cache.epoch == self.grammar_epoch
            {
                return Ok(cache.context);
            }
        }
        let context = self.create_tier2_context(snap)?;
        self.tier2_cache[slot] = Some(Tier2Cache {
            grammar_key: snap.grammar_key,
            current_nt: snap.current_nt,
            in_content: snap.in_content,
            needs_xsi_type: snap.needs_xsi_type,
            is_nillable: snap.is_nillable,
            epoch: self.grammar_epoch,
            context,
        });
        Ok(context)
    }

    /// Erzeugt Tier2Context für die Document-Grammar (ElementFragment).
    /// Analog zu Encoder::create_document_tier2_context().
    fn create_document_tier2_context(&self) -> Result<Tier2Context> {
        let nt = self
            .document_grammar
            .get(self.document_nt)
            .ok_or_else(|| Error::UnknownNonTerminal(format!("{:?}", self.document_nt)))?;

        let has_ee_in_tier1 = nt.has_ee_length_one();

        let is_first_nt = matches!(
            self.document_nt,
            NonTerminalId::ElementFragment0 | NonTerminalId::ElementFragmentTypeEmpty0
        );

        Ok(Tier2Context {
            has_ee_in_tier1,
            is_first_nt,
            is_content_area: self.document_in_content,
            needs_xsi_type: true,
            is_nillable: true,
            is_element_fragment: true,
            xsi_type_id: self.xsi_type_id,
            xsi_nil_id: self.xsi_nil_id,
            ..Tier2Context::new(&self.options)
        })
    }




    /// Behandelt xsi:nil="true" (Spec 8.5.4.4.2).
    ///
    /// Bei xsi:nil="true" oder xsi:nil="1" wird das Flag gesetzt, das bewirkt
    /// dass nur noch EE (EndElement) erlaubt ist.
    ///
    /// # Errors
    ///
    /// `Error::XsiTypeAndNilTogether` wenn xsi:type bereits auf diesem Element gesetzt wurde.
    fn handle_xsi_nil(&mut self, value: &str) -> Result<()> {
        // Spec 8.5.4.4.2: xsi:type und xsi:nil dürfen nicht zusammen verwendet werden
        if self.xsi_type_seen && self.options.strict {
            return Err(Error::XsiTypeAndNilTogether);
        }

        if value == "true" || value == "1" {
            self.xsi_nil_active = true;
        }
        Ok(())
    }

    fn switch_current_element_to_type_empty(&mut self) -> Result<()> {
        let (grammar_key, current_nt) = match self.element_stack.last() {
            Some(elem) => (elem.grammar_key, elem.current_nt),
            None => return Err(Error::invalid_event_code("", "leerer Element-Stack (TypeEmpty)")),
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
            } else if let Some(start) = type_empty_start {
                elem.current_nt = start;
            }
        }
        Ok(())
    }

    /// Behandelt xsi:type Attribut (Spec 8.5.4.4).
    ///
    /// Sucht den Typ im Schema und wechselt `current_element_type`.
    /// Bei `strict=true` und nicht gefundenem Typ wird ein Fehler zurückgegeben.
    ///
    /// # Errors
    ///
    /// - `Error::XsiTypeAndNilTogether` wenn xsi:nil bereits auf diesem Element gesetzt wurde.
    /// Bei nicht-gefundenem Typ wird die aktuelle Grammar beibehalten.
    fn handle_xsi_type(&mut self, value: &str) -> Result<()> {
        // Spec 8.5.4.4.2: xsi:type und xsi:nil dürfen nicht zusammen verwendet werden
        if self.xsi_nil_active && self.options.strict {
            return Err(Error::XsiTypeAndNilTogether);
        }

        self.xsi_type_seen = true;

        let schema = match &self.schema {
            Some(s) => s.clone(),
            None => return Ok(()), // Kein Schema, nichts zu tun
        };

        // Prefer the decoded QName (includes namespace URI) when available.
        if let Some(decoded_qname) = self.xsi_type_qname.clone() {
            for (type_qname, type_def) in schema.type_definitions() {
                if type_qname.local_name == decoded_qname.local_name
                    && type_qname.uri == decoded_qname.uri
                {
                    if let Some(elem) = self.element_stack.last_mut() {
                        elem.pending_type_switch = Some(type_def.clone());
                    }
                    self.current_element_type = Some(type_def.clone());
                    return Ok(());
                }
            }
        }

        // Fallback: parse lexical QName and match by local name only.
        let (type_prefix, type_local) = if let Some(pos) = value.find(':') {
            (&value[..pos], &value[pos + 1..])
        } else {
            ("", value)
        };

        for (type_qname, type_def) in schema.type_definitions() {
            if *type_qname.local_name == *type_local {
                if let Some(elem) = self.element_stack.last_mut() {
                    elem.pending_type_switch = Some(type_def.clone());
                }
                self.current_element_type = Some(type_def.clone());
                return Ok(());
            }
        }

        // Typ nicht gefunden: xs:* built-in Types akzeptieren
        let is_xs_namespace = self
            .xsi_type_qname
            .as_ref()
            .map(|q| &*q.uri == "http://www.w3.org/2001/XMLSchema")
            .unwrap_or(false);

        if type_prefix == "xsd" || type_prefix == "xs" || is_xs_namespace {
            let type_def = if type_local == "anyType" {
                TypeDefinition::Complex {
                    name: None,
                    base_type: None,
                    derivation: None,
                    attributes: Vec::new(),
                    attribute_wildcard: Some(AttributeWildcard::Any),
                    content: ContentType::Mixed(Particle::new(
                        0,
                        MaxOccurs::Unbounded,
                        ParticleTerm::Wildcard(Wildcard::any()),
                    )?),
                    has_named_sub_types: true,
                }
            } else {
                TypeDefinition::simple_with_base(type_local)
            };
            let type_def = Rc::new(type_def);
            if let Some(elem) = self.element_stack.last_mut() {
                elem.pending_type_switch = Some(type_def.clone());
            }
            self.current_element_type = Some(type_def);
            return Ok(());
        }

        // Typ nicht gefunden — aktuelle Grammar beibehalten (Exificient-Verhalten).
        // Die QName-Dekodierung hat den Wert bereits gelesen; nur die Grammar-
        // Umschaltung entfällt.
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
            // Beim Switch von ElementFragment: is_nillable aus dem Schema
            // nachschlagen. ElementFragment setzt is_nillable=true für alle
            // Elemente (Spec 8.5.3), aber nach dem xsi:type-Switch brauchen
            // wir die echte Nillability, damit die spekulative xsi:nil-Remap-
            // Logik nicht fälschlicherweise auslöst.
            let elem_qname = self.qname_pool.get_or_create(elem.expanded_name, &self.interner);

            if matches!(current_type, Some(GrammarType::ElementFragment))
                && let Some(ref schema) = self.schema
            {
                elem.is_nillable = schema
                    .get_element(&elem_qname)
                    .map_or(false, |d| d.nillable);
            }
            let elem_decl = crate::schema::ElementDeclaration::new(elem_qname)
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
            // verwendet, kein Element-Grammar. In strict Mode haben Type-
            // Grammars KEINE xsi:type/xsi:nil-Augmentierung (die Spec
            // definiert diese nur für "each normalized element grammar
            // Element_i"). Die von schema_informed_element hinzugefügten
            // xsi:type/xsi:nil-Productions müssen entfernt werden.
            if self.options.strict {
                Rc::make_mut(&mut grammar_arc).strip_xsi_type_nil_from_start(&self.interner);
            }

            let start = grammar_arc.start();
            let current_nt = elem.current_nt;
            let new_nt = if grammar_arc.get(current_nt).is_some() {
                current_nt
            } else {
                start
            };

            self.element_grammars.insert(key, grammar_arc);
            // Inline statt self.bump_grammar_epoch() wegen aktiver &mut elem Borrow.
            self.grammar_epoch = self.grammar_epoch.wrapping_add(1);
            self.tier2_cache = [const { None }; TIER2_CACHE_SLOTS];
            elem.grammar_key = key;
            elem.current_nt = new_nt;
            elem.element_type = Some(type_def.clone());
            elem.needs_xsi_type = type_def.has_named_sub_types() || type_def.is_union();
        }

        Ok(())
    }

    // decode_precompression_stream → compression.rs

    /// Beendet das Decoding und prüft ob der Stream vollständig gelesen wurde.
    ///
    /// Gibt einen Fehler zurück wenn mehr als 7 Bits ungelesen bleiben
    /// (bis zu 7 Bits Padding sind erlaubt bei Byte-Alignment).
    pub fn finish(self) -> Result<()> {
        // Validierung: EndDocument muss erreicht sein
        if !self.finished {
            return Err(Error::schema_violation(
                "finish(): EndDocument wurde nicht erreicht",
            ));
        }

        // Validierung: Element-Stack muss leer sein
        if !self.element_stack.is_empty() {
            return Err(Error::schema_violation(format!(
                "finish(): {} offene Elemente im Stack",
                self.element_stack.len()
            )));
        }

        // Validierung: SC-Stack muss leer sein (alle SC-Fragments abgeschlossen)
        if !self.sc_state_stack.is_empty() {
            return Err(Error::schema_violation(format!(
                "finish(): {} offene Self-Contained Fragments",
                self.sc_state_stack.len()
            )));
        }

        let remaining = self.reader.remaining_bits();
        // Bis zu 7 Bits Padding sind erlaubt (Byte-Alignment)
        if remaining > 7 {
            return Err(Error::DecompressionError(format!(
                "Stream incomplete: {remaining} bits remaining after EndDocument"
            )));
        }
        Ok(())
    }
}

// ============================================================================
// Iterator API
// ============================================================================

/// Streaming-Decoder Iterator.
///
/// Liefert Events einzeln per `next()`. Am Stream-Ende wird `finish()`
/// automatisch aufgerufen; Integritätsfehler werden als letztes `Err` geliefert.
///
/// Kann auch manuell via `finish()` abgeschlossen werden wenn die Iteration
/// vorzeitig abgebrochen wird.
pub struct DecodeIter<'a> {
    inner: DecodeIterInner<'a>,
}

enum DecodeIterInner<'a> {
    Stream {
        decoder: Option<Decoder<'a>>,
    },
    CompressedLazy {
        state: Box<CompressedLazyState<'a>>,
    },
    PrecompressionLazy {
        state: Box<PrecompressionLazyState<'a>>,
    },
}

impl<'a> Iterator for DecodeIter<'a> {
    type Item = Result<ExiEvent>;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.inner {
            DecodeIterInner::Stream { decoder } => {
                let dec = decoder.as_mut()?;
                match dec.decode_event() {
                    Ok(Some(event)) => Some(Ok(event)),
                    Ok(None) => {
                        // Automatisch finish() aufrufen — Fehler als letztes Err liefern
                        match decoder.take().unwrap().finish() {
                            Ok(()) => None,
                            Err(e) => Some(Err(e)),
                        }
                    }
                    Err(e) => {
                        decoder.take();
                        Some(Err(e))
                    }
                }
            }
            DecodeIterInner::CompressedLazy { state } => state.next_event(),
            DecodeIterInner::PrecompressionLazy { state } => state.next_event(),
        }
    }
}

impl<'a> DecodeIter<'a> {
    /// Prüft Stream-Integrität (Trailing Bits, offene Elemente).
    ///
    /// Wird automatisch am Ende der Iteration aufgerufen. Kann auch manuell
    /// aufgerufen werden wenn die Iteration vorzeitig abgebrochen wurde.
    pub fn finish(self) -> Result<()> {
        match self.inner {
            DecodeIterInner::Stream { decoder: Some(decoder) } => decoder.finish(),
            DecodeIterInner::CompressedLazy { state } => state.finish(),
            DecodeIterInner::PrecompressionLazy { mut state } => state.finish(),
            DecodeIterInner::Stream { decoder: None } => Ok(()),
        }
    }

    /// Setzt den MemoryMonitor fuer OOM-Vermeidung bei grossen Dateien.
    pub fn set_memory_monitor(&mut self, monitor: crate::memory_monitor::MemoryMonitor) {
        match &mut self.inner {
            DecodeIterInner::Stream { decoder: Some(dec) } => {
                dec.set_memory_monitor(monitor);
            }
            DecodeIterInner::CompressedLazy { state } => {
                state.set_memory_monitor(monitor);
            }
            DecodeIterInner::PrecompressionLazy { state } => {
                state.set_memory_monitor(monitor);
            }
            _ => {}
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests;
