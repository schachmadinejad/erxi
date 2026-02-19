//! Decoder Compression / PreCompression (Spec 9).
//!
//! Enthält die Block-Dekodierung für Compression und PreCompression:
//! Structure/Value-Phase-Trennung (Spec 9.2), Channel-Ordering (Spec 9.3),
//! Multi-Block State (Spec 9.1) und CompactBlockBuffer (SoA-Optimierung).

use std::rc::Rc;

use crate::event::{AtContent, ChContent, ExiEvent, NsContent};
use crate::grammar::{GrammarCache, GrammarKey, GrammarSystem, GrammarTemplateCache, NonTerminalId};
use crate::options::ExiOptions;
use crate::precompression::{ChannelKey, order_channels};
use crate::qname::{ExpandedNameId, QName, QNamePool, StringInterner};
use crate::schema::TypeDefinition;
use crate::string_table::StringTable;
use crate::{Error, FastHashMap, FastIndexMap, Result};

use super::Decoder;

// ============================================================================
// CompactBlockBuffer (SoA-Layout für Block-Dekodierung)
// ============================================================================

/// Event-Typ Diskriminante (1 Byte) für CompactBlockBuffer.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum EventKind {
    StartDocument = 0,
    EndDocument = 1,
    StartElement = 2,
    EndElement = 3,
    Attribute = 4,
    Characters = 5,
    NamespaceDeclaration = 6,
    Comment = 7,
    ProcessingInstruction = 8,
    DocType = 9,
    EntityReference = 10,
    SelfContained = 11,
}

/// Kompakter Event-Deskriptor: Typ + Index in typspezifisches Array.
/// 8 Bytes pro Event (statt 80 Bytes für ExiEvent).
pub(super) struct CompactEvent {
    pub(super) kind: EventKind,
    pub(super) _pad: [u8; 3],
    pub(super) data_idx: u32,
}

/// SoA-basierter Block-Buffer für Compression-Decode.
///
/// Speichert Events kompakt als Structure-of-Arrays statt Vec<ExiEvent>.
/// Spart ~77 MB bei 1.55M Events (8B/Event statt 80B/Event für Deskriptoren,
/// plus typspezifische Arrays nur für Events die diese Daten tragen).
pub(super) struct CompactBlockBuffer {
    pub(super) events: Vec<CompactEvent>,
    pub(super) se_expanded: Vec<ExpandedNameId>,
    pub(super) se_prefixes: Vec<Option<Rc<str>>>,
    pub(super) at_expanded: Vec<ExpandedNameId>,
    pub(super) at_prefixes: Vec<Option<Rc<str>>>,
    pub(super) at_values: Vec<Rc<str>>,
    pub(super) ch_values: Vec<Rc<str>>,
    ns_contents: Vec<NsContent>,
    cm_contents: Vec<crate::event::CmContent>,
    pi_contents: Vec<crate::event::PiContent>,
    dt_contents: Vec<crate::event::DtContent>,
    er_contents: Vec<crate::event::ErContent>,
}

impl CompactBlockBuffer {
    pub(super) fn with_capacity(event_hint: usize) -> Self {
        // Typische Verteilung: ~30% SE, ~20% AT, ~20% CH, Rest EE/SD/ED etc.
        let se_cap = event_hint / 3;
        let at_cap = event_hint / 5;
        let ch_cap = event_hint / 5;
        Self {
            events: Vec::with_capacity(event_hint),
            se_expanded: Vec::with_capacity(se_cap),
            se_prefixes: Vec::with_capacity(se_cap),
            at_expanded: Vec::with_capacity(at_cap),
            at_prefixes: Vec::with_capacity(at_cap),
            at_values: Vec::with_capacity(at_cap),
            ch_values: Vec::with_capacity(ch_cap),
            ns_contents: Vec::new(),
            cm_contents: Vec::new(),
            pi_contents: Vec::new(),
            dt_contents: Vec::new(),
            er_contents: Vec::new(),
        }
    }

    /// Fügt ein ExiEvent in den kompakten Buffer ein.
    /// Gibt den data_idx des typspezifischen Arrays zurück (für PendingValue).
    pub(super) fn push(&mut self, event: ExiEvent, interner: &mut StringInterner) -> Result<u32> {
        let (kind, data_idx) = match event {
            ExiEvent::StartDocument => (EventKind::StartDocument, 0),
            ExiEvent::EndDocument => (EventKind::EndDocument, 0),
            ExiEvent::EndElement => (EventKind::EndElement, 0),
            ExiEvent::SelfContained => (EventKind::SelfContained, 0),
            ExiEvent::StartElement(qname) => {
                let idx = self.se_expanded.len() as u32;
                let expanded = interner.intern_expanded(&qname.uri, &qname.local_name)?;
                self.se_expanded.push(expanded);
                self.se_prefixes.push(qname.prefix.clone());
                (EventKind::StartElement, idx)
            }
            ExiEvent::Attribute(at) => {
                let idx = self.at_expanded.len() as u32;
                let expanded = interner.intern_expanded(&at.qname.uri, &at.qname.local_name)?;
                self.at_expanded.push(expanded);
                self.at_prefixes.push(at.qname.prefix.clone());
                self.at_values.push(at.value);
                (EventKind::Attribute, idx)
            }
            ExiEvent::Characters(ch) => {
                let idx = self.ch_values.len() as u32;
                self.ch_values.push(ch.value);
                (EventKind::Characters, idx)
            }
            ExiEvent::NamespaceDeclaration(ns) => {
                let idx = self.ns_contents.len() as u32;
                self.ns_contents.push(ns);
                (EventKind::NamespaceDeclaration, idx)
            }
            ExiEvent::Comment(cm) => {
                let idx = self.cm_contents.len() as u32;
                self.cm_contents.push(cm);
                (EventKind::Comment, idx)
            }
            ExiEvent::ProcessingInstruction(pi) => {
                let idx = self.pi_contents.len() as u32;
                self.pi_contents.push(pi);
                (EventKind::ProcessingInstruction, idx)
            }
            ExiEvent::DocType(dt) => {
                let idx = self.dt_contents.len() as u32;
                self.dt_contents.push(dt);
                (EventKind::DocType, idx)
            }
            ExiEvent::EntityReference(er) => {
                let idx = self.er_contents.len() as u32;
                self.er_contents.push(er);
                (EventKind::EntityReference, idx)
            }
        };
        self.events.push(CompactEvent { kind, _pad: [0; 3], data_idx });
        Ok(data_idx)
    }

    /// Rekonstruiert das ExiEvent an Position `pos` (konsumierend via take/move).
    pub(super) fn take_event(
        &mut self,
        pos: usize,
        qname_pool: &mut QNamePool,
        interner: &StringInterner,
    ) -> Option<ExiEvent> {
        let ce = self.events.get(pos)?;
        let di = ce.data_idx as usize;
        Some(match ce.kind {
            EventKind::StartDocument => ExiEvent::StartDocument,
            EventKind::EndDocument => ExiEvent::EndDocument,
            EventKind::EndElement => ExiEvent::EndElement,
            EventKind::SelfContained => ExiEvent::SelfContained,
            EventKind::StartElement => {
                let expanded = self.se_expanded[di];
                let prefix = self.se_prefixes[di].take();
                ExiEvent::StartElement(
                    qname_pool.get_or_create_with_prefix(expanded, prefix, interner),
                )
            }
            EventKind::Attribute => {
                let expanded = self.at_expanded[di];
                let prefix = self.at_prefixes[di].take();
                ExiEvent::Attribute(AtContent {
                    qname: qname_pool.get_or_create_with_prefix(expanded, prefix, interner),
                    value: std::mem::take(&mut self.at_values[di]),
                })
            }
            EventKind::Characters => ExiEvent::Characters(ChContent {
                value: std::mem::take(&mut self.ch_values[di]),
            }),
            EventKind::NamespaceDeclaration => ExiEvent::NamespaceDeclaration(
                std::mem::take(&mut self.ns_contents[di]),
            ),
            EventKind::Comment => ExiEvent::Comment(
                std::mem::take(&mut self.cm_contents[di]),
            ),
            EventKind::ProcessingInstruction => ExiEvent::ProcessingInstruction(
                std::mem::take(&mut self.pi_contents[di]),
            ),
            EventKind::DocType => ExiEvent::DocType(
                std::mem::take(&mut self.dt_contents[di]),
            ),
            EventKind::EntityReference => ExiEvent::EntityReference(
                std::mem::take(&mut self.er_contents[di]),
            ),
        })
    }
}

// ============================================================================
// PendingValue
// ============================================================================

/// Pending Value für PreCompression-Decoding (kompakt, 12 Bytes).
///
/// QNames und TypeDefs werden dedupliziert in `pending_channel_qnames`
/// und `pending_type_defs` gespeichert, hier nur Indizes.
pub(super) struct PendingValue {
    /// Index des Events im Buffer.
    pub(super) event_index: u32,
    /// Index in typspezifisches Array (at_values/ch_values im CompactBlockBuffer).
    pub(super) data_idx: u32,
    /// Channel-Index (verweist auf `pending_channel_qnames`).
    pub(super) channel_idx: u16,
    /// Type-Index: 0 = None, 1..=N = pending_type_defs[idx-1].
    pub(super) type_idx: u16,
}

// ============================================================================
// MultiBlockState
// ============================================================================

/// State-Snapshot für Multi-Block-Decoding (Spec 9.1).
///
/// Analogie zu ScDecoderState — alle Decoder-Felder die zwischen
/// Blöcken erhalten bleiben müssen (Grammars, String Table, Element
/// Stack, xsi:type/nil State, etc.).
pub(super) struct MultiBlockState {
    pub(super) string_table: StringTable,
    pub(super) element_grammars: FastHashMap<GrammarKey, Rc<GrammarSystem>>,
    pub(super) document_grammar: Rc<GrammarSystem>,
    pub(super) document_nt: NonTerminalId,
    pub(super) element_stack: Vec<super::ElementContext>,
    pub(super) next_local_grammar_id: u64,
    pub(super) current_element_type: Option<Rc<TypeDefinition>>,
    pub(super) xsi_nil_active: bool,
    pub(super) xsi_type_seen: bool,
    pub(super) xsi_type_qname: Option<QName>,
    pub(super) saw_invalid_xsi_nil: bool,
    pub(super) xsi_type_skip_learning_hit: bool,
    pub(super) document_in_content: bool,
    pub(super) grammar_template_cache: GrammarTemplateCache,
    pub(super) interner: StringInterner,
    pub(super) qname_pool: QNamePool,
    pub(super) xsi_type_id: ExpandedNameId,
    pub(super) xsi_nil_id: ExpandedNameId,
    pub(super) grammar_cache: GrammarCache,
    pub(super) grammar_epoch: u64,
}

impl MultiBlockState {
    /// Extrahiert den State aus einem Decoder.
    pub(super) fn extract_from(decoder: Decoder<'_>) -> Self {
        Self {
            string_table: decoder.string_table,
            element_grammars: decoder.element_grammars,
            document_grammar: decoder.document_grammar,
            document_nt: decoder.document_nt,
            element_stack: decoder.element_stack,
            next_local_grammar_id: decoder.next_local_grammar_id,
            current_element_type: decoder.current_element_type,
            xsi_nil_active: decoder.xsi_nil_active,
            xsi_type_seen: decoder.xsi_type_seen,
            xsi_type_qname: decoder.xsi_type_qname,
            saw_invalid_xsi_nil: decoder.saw_invalid_xsi_nil,
            xsi_type_skip_learning_hit: decoder.xsi_type_skip_learning_hit,
            document_in_content: decoder.document_in_content,
            grammar_template_cache: decoder.grammar_template_cache,
            interner: decoder.interner,
            qname_pool: decoder.qname_pool,
            xsi_type_id: decoder.xsi_type_id,
            xsi_nil_id: decoder.xsi_nil_id,
            grammar_cache: decoder.grammar_cache,
            grammar_epoch: decoder.grammar_epoch,
        }
    }
}

// ============================================================================
// Decoder Compression-Methoden
// ============================================================================

impl<'a> Decoder<'a> {
    /// Dekodiert einen PreCompression-Stream vollständig (Spec 9).
    ///
    /// Bei PreCompression (und Compression) werden AT/CH Values nicht inline
    /// nach dem Event Code geschrieben, sondern im Value Channel am Ende des
    /// Blocks. Diese Methode:
    /// 1. Dekodiert den Structure Channel (Events mit Platzhaltern für Values)
    /// 2. Liest die Value Channels
    /// 3. Fügt die Values in die Events ein
    ///
    /// # Spec-Referenz
    /// - Spec 9.2.1: Structure Channel
    /// - Spec 9.2.2: Value Channels
    /// - Spec 9.3: Compressed Streams (≤100 Values: ein kombinierter Stream)
    pub(super) fn decode_precompression_stream(&mut self) -> Result<()> {
        let mut events = Vec::new();
        self.pending_values.clear();
        self.pending_channel_qnames.clear();
        self.pending_channel_index.clear();
        self.pending_type_defs.clear();
        self.pending_type_ptr_index.clear();
        self.skip_values = true;
        self.precompression_event_count = 0;
        let mut stall_count = 0u32;
        let mut last_snapshot: Option<(usize, usize, NonTerminalId, bool)> = None;
        let max_stall = 32u32;
        let block_size = self.options.block_size as usize;

        // Multi-Block Pre-Compression (Spec 9.1):
        // Jeder Block enthält bis zu block_size Values. Nach dem Structure Channel
        // eines Blocks folgen die Value Channels für diesen Block, dann der nächste Block.
        // Der letzte Block endet mit ED und hat weniger als block_size Values.

        // NS-Lookahead (preserve.prefixes=true): Wenn ein NS-Event mit local_element_ns=true
        // decodiert wird, suchen wir rückwärts das letzte SE mit passendem URI und setzen
        // dessen Prefix. Das ist O(k) wobei k typisch 0-3 ist.
        let preserve_prefixes = self.options.preserve.prefixes;

        loop {
            let before_bit_pos = self.bit_position();
            let event = if self.element_stack.is_empty() {
                match self.decode_document_event() {
                    Ok(ev) => ev,
                    Err(Error::PrematureEndOfStream)
                        if self.reader.remaining_bits() == 0 =>
                    {
                        // Toleranz: PreCompression Streams die ohne ED enden.
                        ExiEvent::EndDocument
                    }
                    Err(e) => return Err(e),
                }
            } else {
                match self.decode_element_event() {
                    Ok(ev) => ev,
                    Err(e) => return Err(e),
                }
            };
            let after_bit_pos = self.bit_position();
            let snapshot = (
                after_bit_pos,
                self.element_stack.len(),
                self.document_nt,
                self.document_in_content,
            );
            if after_bit_pos == before_bit_pos && last_snapshot == Some(snapshot) {
                stall_count += 1;
                if stall_count >= max_stall {
                    return Err(Error::DecoderStalled);
                }
            } else {
                stall_count = 0;
            }
            last_snapshot = Some(snapshot);

            let is_end_doc = matches!(event, ExiEvent::EndDocument);

            // NS-Lookahead: Bei NS mit local_element_ns=true das letzte SE anpassen
            if preserve_prefixes
                && let ExiEvent::NamespaceDeclaration(ref ns) = event
                && ns.local_element_ns
            {
                for e in events.iter_mut().rev() {
                    if let ExiEvent::StartElement(qname) = e
                        && qname.uri == ns.uri
                    {
                        Rc::make_mut(qname).prefix = Some(Rc::clone(&ns.prefix));
                        break;
                    }
                }
            }

            events.push(event);
            self.precompression_event_count += 1;

            if is_end_doc {
                // Letzter Block: Value Channels lesen und zuweisen
                self.skip_values = false;
                self.read_block_value_channels(&mut events)?;
                break;
            }

            // Spec 9.1: Block-Grenze nach block_size Values
            if block_size > 0 && self.pending_values.len() >= block_size {
                self.skip_values = false;
                self.read_block_value_channels(&mut events)?;
                self.skip_values = true;
                // pending_values wurden konsumiert, per-block State zurücksetzen
                self.pending_values.clear();
                self.pending_channel_qnames.clear();
                self.pending_channel_index.clear();
                self.pending_type_defs.clear();
                self.pending_type_ptr_index.clear();
            }
        }

        self.precompression_buffer = Some(events.into_iter().collect());
        Ok(())
    }

    /// Liest Value Channels für den aktuellen Block und weist die Werte den Events zu.
    ///
    /// Konsumiert `self.pending_values` und liest Values aus dem aktuellen
    /// Reader-Position (direkt nach dem Structure Channel des Blocks).
    ///
    /// # Spec-Referenz
    /// - Spec 9.2.2: Value Channels
    /// - Spec 9.3: Channel-Reihenfolge (kleine zuerst, dann große)
    fn read_block_value_channels(&mut self, events: &mut [ExiEvent]) -> Result<()> {
        let pending = std::mem::take(&mut self.pending_values);
        let channel_qnames = std::mem::take(&mut self.pending_channel_qnames);
        let type_defs = std::mem::take(&mut self.pending_type_defs);

        self.precompression_value_count += pending.len();

        // Channels gruppieren: channel_idx → Vec<(event_index, type_idx)>
        let mut channels: FastIndexMap<ChannelKey, (QName, Vec<(usize, u16)>)> = FastIndexMap::default();
        for pv in &pending {
            let qname = &channel_qnames[pv.channel_idx as usize];
            let key = ChannelKey::new(qname.uri.clone(), qname.local_name.clone());
            channels
                .entry(key)
                .or_insert_with(|| (qname.clone(), Vec::new()))
                .1
                .push((pv.event_index as usize, pv.type_idx));
        }

        // Channel-Counts für Ordering
        let channel_counts: FastIndexMap<ChannelKey, usize> = channels
            .iter()
            .map(|(k, (_, v))| (k.clone(), v.len()))
            .collect();
        let total_values = pending.len();

        // Spec 9.3: Channel-Reihenfolge
        let read_order = order_channels(&channel_counts, total_values);

        if self.trace.precomp {
            eprintln!(
                "precomp: block values={} channels={} bit_pos={}",
                total_values,
                read_order.len(),
                self.bit_position()
            );
        }

        // Lies Values für jeden Channel in der richtigen Reihenfolge
        for channel_key in &read_order {
            let (channel_qname, indices) = channels
                .get(channel_key)
                .ok_or_else(|| Error::ordering_violation("Channel in Map", "Channel nicht gefunden"))?;
            for &(event_index, type_idx) in indices {
                let type_def = (type_idx > 0)
                    .then(|| &*type_defs[(type_idx - 1) as usize]);
                let value = self.decode_value_typed_or_string(channel_qname, type_def)?;

                match &mut events[event_index] {
                    ExiEvent::Characters(ch) => ch.value = value,
                    ExiEvent::Attribute(at) => at.value = value,
                    _ => return Err(Error::ordering_violation("CH oder AT Event", "anderer Event-Typ")),
                }
            }
        }

        // Byte-Alignment nach Value Channels sicherstellen,
        // damit der nächste Block (Structure) byte-aligned startet.
        self.reader.align_to_byte();

        Ok(())
    }

    /// Structure-Phase: Parst Events aus Structure-Stream in CompactBlockBuffer.
    ///
    /// Gibt CompactBlockBuffer (8B/Event statt 80B), PendingValues und
    /// has_end_document zurück. Spart ~77 MB bei 1.55M Events.
    pub(super) fn decode_structure_phase_compact(
        &mut self,
        max_values: usize,
    ) -> Result<(CompactBlockBuffer, Vec<PendingValue>, bool)> {
        self.pending_values.clear();
        self.pending_channel_qnames.clear();
        self.pending_channel_index.clear();
        self.pending_type_defs.clear();
        self.pending_type_ptr_index.clear();
        self.skip_values = true;
        self.precompression_event_count = 0;

        let preserve_prefixes = self.options.preserve.prefixes;
        // Events pro Block ≈ 3× Values (SE+AT+CH+EE etc.)
        let mut buffer = CompactBlockBuffer::with_capacity(max_values.min(100_000) * 3);
        let mut has_end_document = false;

        loop {
            // pv_count VOR decode erfassen (decode_or_skip_value fügt PendingValue hinzu)
            let pv_count_before = self.pending_values.len();

            let event = if self.element_stack.is_empty() {
                match self.decode_document_event() {
                    Ok(ev) => ev,
                    Err(Error::PrematureEndOfStream)
                        if self.reader.remaining_bits() == 0 =>
                    {
                        ExiEvent::EndDocument
                    }
                    Err(e) => return Err(e),
                }
            } else {
                self.decode_element_event()?
            };

            let is_end_doc = matches!(event, ExiEvent::EndDocument);

            // NS-Lookahead: Rückwärtssuche im kompakten Buffer
            if preserve_prefixes
                && let ExiEvent::NamespaceDeclaration(ref ns) = event
                && ns.local_element_ns
            {
                let ns_uri = &ns.uri;
                let ns_prefix = &ns.prefix;
                for i in (0..buffer.events.len()).rev() {
                    if buffer.events[i].kind == EventKind::StartElement {
                        let di = buffer.events[i].data_idx as usize;
                        let expanded = buffer.se_expanded[di];
                        if self.interner.resolve(expanded.uri) == &**ns_uri {
                            buffer.se_prefixes[di] = Some(Rc::clone(ns_prefix));
                            break;
                        }
                    }
                }
            }

            let data_idx = buffer.push(event, &mut self.interner)?;

            // data_idx in PendingValue nachpflegen (wurde in decode_or_skip_value
            // als 0 gesetzt, da data_idx erst nach push() bekannt)
            if self.pending_values.len() > pv_count_before {
                self.pending_values.last_mut().unwrap().data_idx = data_idx;
            }

            self.precompression_event_count += 1;

            if is_end_doc {
                self.finished = true;
                has_end_document = true;
                break;
            }

            if self.pending_values.len() >= max_values {
                break;
            }
        }

        self.skip_values = false;
        let pending = std::mem::take(&mut self.pending_values);
        self.precompression_value_count = pending.len();

        Ok((buffer, pending, has_end_document))
    }

    /// Value-Phase: Liest Values und schreibt direkt in CompactBlockBuffer.
    ///
    /// Nutzt data_idx für direkten Zugriff auf at_values/ch_values,
    /// ohne pending_by_event_index Lookup.
    pub(super) fn decode_value_phase_compact(
        &mut self,
        buffer: &mut CompactBlockBuffer,
        pending: Vec<PendingValue>,
    ) -> Result<()> {
        let mut channels: FastIndexMap<ChannelKey, (QName, Vec<&PendingValue>)> = FastIndexMap::default();
        for pv in &pending {
            let qname = &self.pending_channel_qnames[pv.channel_idx as usize];
            let key = ChannelKey::new(qname.uri.clone(), qname.local_name.clone());
            channels
                .entry(key)
                .or_insert_with(|| (qname.clone(), Vec::new()))
                .1
                .push(pv);
        }

        let channel_counts: FastIndexMap<ChannelKey, usize> = channels
            .iter()
            .map(|(k, (_, v))| (k.clone(), v.len()))
            .collect();
        let total_values = pending.len();
        let read_order = order_channels(&channel_counts, total_values);

        for channel_key in &read_order {
            let (channel_qname, pvs) = channels
                .get(channel_key)
                .ok_or_else(|| Error::ordering_violation("Channel in Map", "Channel nicht gefunden"))?;
            for pv in pvs {
                let type_def_arc = (pv.type_idx > 0)
                    .then(|| self.pending_type_defs[(pv.type_idx - 1) as usize].clone());
                let value = self.decode_value_typed_or_string(channel_qname, type_def_arc.as_deref())?;
                let di = pv.data_idx as usize;
                match buffer.events[pv.event_index as usize].kind {
                    EventKind::Characters => buffer.ch_values[di] = value,
                    EventKind::Attribute => buffer.at_values[di] = value,
                    _ => return Err(Error::ordering_violation("CH oder AT Event", "anderer Event-Typ")),
                }
            }
        }

        Ok(())
    }
}

// ============================================================================
// Hilfsfunktionen
// ============================================================================

/// Generiert einen PreCompression-Header für interne Verarbeitung.
pub(super) fn generate_precompression_header(precomp_options: &ExiOptions) -> Result<Vec<u8>> {
    let mut header_writer = crate::bitstream::BitWriter::new();
    let header = crate::header::ExiHeader::new().with_options();
    crate::header::encode(&mut header_writer, &header, false)?;
    crate::options_codec::encode(&mut header_writer, precomp_options)?;
    header_writer.align_to_byte();
    Ok(header_writer.into_vec())
}

/// Berechnet Channel-Counts aus PendingValues (für Spec 9.3 Stream-Layout).
pub(super) fn compute_channel_counts(pending: &[PendingValue], channel_qnames: &[QName]) -> FastIndexMap<ChannelKey, usize> {
    let mut counts: FastIndexMap<ChannelKey, usize> = FastIndexMap::default();
    for pv in pending {
        let qname = &channel_qnames[pv.channel_idx as usize];
        let key = ChannelKey::new(qname.uri.clone(), qname.local_name.clone());
        *counts.entry(key).or_insert(0) += 1;
    }
    counts
}
