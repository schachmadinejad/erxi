//! Lazy Compression-Decode (Block-by-Block DEFLATE).
//!
//! Dekomprimiert DEFLATE-Streams on-demand und dekodiert Blöcke einzeln,
//! statt alle Streams und Events im Speicher zu materialisieren.

use std::rc::Rc;

use crate::event::ExiEvent;
use crate::memory_monitor::MemoryMonitor;
use crate::options::ExiOptions;
use crate::schema::SchemaInfo;
use crate::{Error, Result};

use super::Decoder;
use super::compression::{
    CompactBlockBuffer, MultiBlockState, compute_channel_counts, generate_precompression_header,
};

/// Lazy Compression-Decode State.
///
/// Dekomprimiert DEFLATE-Streams on-demand und dekodiert Blöcke einzeln,
/// statt alle Streams und Events im Speicher zu materialisieren.
pub(super) struct CompressedLazyState<'a> {
    /// Roh-Daten des komprimierten Body (Referenz auf Input-Slice).
    compressed_body: &'a [u8],
    /// Aktueller Offset in compressed_body (nächster DEFLATE-Stream).
    deflate_offset: usize,
    /// PreCompression-Options für Block-Decoding.
    precomp_options: ExiOptions,
    /// Vorberechneter Header für Block-Decoder.
    header_data: Vec<u8>,
    /// Schema (optional).
    schema: Option<Rc<SchemaInfo>>,
    /// Block-Größe (Spec 9.1).
    block_size: usize,
    /// State zwischen Blöcken (Grammar, String Table, etc.).
    block_state: Option<MultiBlockState>,
    /// Kompakter Block-Buffer (statt Vec<ExiEvent>, spart ~77 MB).
    current_block: Option<CompactBlockBuffer>,
    /// Position im aktuellen Block-Buffer.
    current_pos: usize,
    /// Ob EndDocument bereits erreicht wurde.
    finished: bool,
    /// Terminal: Nach Fehler keine weiteren Events.
    failed: bool,
    /// Block-Nummer (für Fehlermeldungen).
    block_number: usize,
    /// Wiederverwendbare Decompress-Instanz (spart ~44 KB Allokation pro Stream).
    decompressor: flate2::Decompress,
    /// Optionaler RAM-Monitor (wird in jeden Block-Decoder injiziert).
    memory_monitor: Option<MemoryMonitor>,
}

impl<'a> CompressedLazyState<'a> {
    pub(super) fn new(
        compressed_body: &'a [u8],
        precomp_options: ExiOptions,
        schema: Option<Rc<SchemaInfo>>,
        block_size: usize,
    ) -> Result<Self> {
        let header_data = generate_precompression_header(&precomp_options)?;
        Ok(Self {
            compressed_body,
            deflate_offset: 0,
            precomp_options,
            header_data,
            schema,
            block_size,
            block_state: None,
            current_block: None,
            current_pos: 0,
            finished: false,
            failed: false,
            block_number: 0,
            decompressor: flate2::Decompress::new(false),
            memory_monitor: None,
        })
    }

    /// Setzt den MemoryMonitor (wird in jeden Block-Decoder injiziert).
    pub(super) fn set_memory_monitor(&mut self, monitor: MemoryMonitor) {
        if let Some(state) = &mut self.block_state {
            state.string_table.set_memory_monitor(monitor.clone());
        }
        self.memory_monitor = Some(monitor);
    }

    pub(super) fn next_event(&mut self) -> Option<Result<ExiEvent>> {
        // Terminal-State: Nach Fehler keine weiteren Events
        if self.failed {
            return None;
        }

        loop {
            // 1. Aus aktuellem CompactBlockBuffer yielden
            if let Some(ref mut buffer) = self.current_block {
                // block_state ist immer Some wenn current_block Some ist
                let state = self.block_state.as_mut().unwrap();
                if let Some(event) = buffer.take_event(
                    self.current_pos,
                    &mut state.qname_pool,
                    &state.interner,
                ) {
                    self.current_pos += 1;
                    if matches!(event, ExiEvent::EndDocument) {
                        self.finished = true;
                    }
                    return Some(Ok(event));
                }
                // Buffer erschöpft → freigeben
                self.current_block = None;
                self.current_pos = 0;
            }

            // 2. Kein Block mehr → Auto-finish
            if self.finished || self.deflate_offset >= self.compressed_body.len() {
                return match self.finish() {
                    Ok(()) => None,
                    Err(e) => {
                        self.failed = true;
                        Some(Err(e))
                    }
                };
            }

            // 3. Nächsten Block dekodieren
            match self.decode_next_block() {
                Ok(()) => continue, // current_block jetzt gefüllt
                Err(e) => {
                    self.failed = true;
                    return Some(Err(e));
                }
            }
        }
    }

    /// Prüft Stream-Integrität nach Abschluss.
    pub(super) fn finish(&self) -> Result<()> {
        if !self.finished {
            return Err(Error::schema_violation(
                "finish(): EndDocument wurde nicht erreicht",
            ));
        }
        Ok(())
    }

    /// Dekomprimiert den nächsten DEFLATE-Stream mit Decompressor-Reuse.
    fn decompress_next(&mut self) -> Result<Option<(Vec<u8>, usize)>> {
        if self.deflate_offset >= self.compressed_body.len() {
            return Ok(None);
        }
        let (decompressed, consumed) = crate::compression::deflate_decompress_reuse(
            &self.compressed_body[self.deflate_offset..],
            &mut self.decompressor,
        )?;
        if consumed == 0 {
            return Err(Error::DecompressionError(format!(
                "DEFLATE stream made no progress at offset {}, {} bytes remaining",
                self.deflate_offset,
                self.compressed_body.len() - self.deflate_offset
            )));
        }
        Ok(Some((decompressed, consumed)))
    }

    /// Erstellt den initialen MultiBlockState (einmalig beim ersten Block).
    ///
    /// SchemaInfo wird hier einmalig geklont (via with_schema), danach nur
    /// noch per Rc::clone geteilt.
    fn ensure_initial_state(&mut self) -> Result<()> {
        if self.block_state.is_some() {
            return Ok(());
        }
        let mut decoder = if let Some(schema) = &self.schema {
            Decoder::with_schema(
                &self.header_data,
                self.precomp_options.clone(),
                (**schema).clone(),
            )?
        } else {
            Decoder::with_options(&self.header_data, self.precomp_options.clone())?
        };
        decoder.read_header()?;
        if let Some(monitor) = &self.memory_monitor {
            decoder.set_memory_monitor(monitor.clone());
        }
        self.block_state = Some(MultiBlockState::extract_from(decoder));
        Ok(())
    }

    /// Single-Pass Block-Decode: Kein Probe-Pass, kein State-Clone.
    ///
    /// Ablauf:
    /// 1. Structure-Stream dekomprimieren
    /// 2. Decoder from_state (kein Header-Parse, kein Schema-Clone — Schema
    ///    wurde einmalig in `ensure_initial_state` geklont und wird per Arc geteilt)
    /// 3. Structure-Phase: Events parsen, PendingValues sammeln
    /// 4. Channel-Counts aus PendingValues → Extra-Streams bestimmen
    /// 5. Value-Phase: Values lesen (selber Reader oder neuer Reader)
    fn decode_next_block(&mut self) -> Result<()> {
        self.block_number += 1;
        self.ensure_initial_state()?;

        // 1. Structure-Stream dekomprimieren (Decompressor-Reuse)
        let (first_stream, consumed) = self.decompress_next()?
            .ok_or_else(|| {
                Error::DecompressionError(format!(
                    "Block {}: no stream available", self.block_number
                ))
            })?;
        self.deflate_offset += consumed;

        // 2. Decoder from State (kein Header, kein SchemaInfo-Clone)
        // INVARIANT: block_state ist nach take() None. Bei Fehler in nachfolgenden
        // Schritten bleibt es None — sicher, da next_event() bei Err self.failed setzt
        // und keine weiteren decode_next_block()-Aufrufe zulässt.
        let state = self.block_state.take()
            .ok_or_else(|| Error::DecompressionError(format!(
                "Block {}: internal error — block_state not initialized", self.block_number
            )))?;
        let mut decoder = Decoder::from_state(
            &first_stream, state, &self.precomp_options, &self.schema,
        );

        // 3. Structure-Phase (kompakt)
        let (mut buffer, pending, has_end_document) =
            decoder.decode_structure_phase_compact(self.block_size)?;
        let value_count = pending.len();

        // Spec 9.1: Jeder Block vor dem letzten hat genau block_size Values
        if !has_end_document && value_count != self.block_size {
            return Err(Error::DecompressionError(format!(
                "Block {} has {} values, expected {} (Spec 9.1)",
                self.block_number, value_count, self.block_size
            )));
        }

        // 4+5. Value-Phase (Pfad abhängig von Spec 9.3 Stream-Layout)
        if value_count <= 100 {
            // ≤100: Values im selben Stream, Reader steht nach Structure
            decoder.decode_value_phase_compact(&mut buffer, pending)?;
            self.block_state = Some(MultiBlockState::extract_from(decoder));
        } else {
            // >100: Extra-Streams dekomprimieren (Spec 9.3)
            // Dedup-Tabellen aus Structure-Decoder extrahieren (vor extract_from)
            let channel_qnames = std::mem::take(&mut decoder.pending_channel_qnames);
            let type_defs = std::mem::take(&mut decoder.pending_type_defs);
            let channel_counts = compute_channel_counts(&pending, &channel_qnames);
            let has_small = channel_counts.values().any(|&c| c <= 100);
            let large_count = channel_counts.values().filter(|&&c| c > 100).count();
            let extra_count = usize::from(has_small) + large_count;

            // State aus Structure-Decoder extrahieren (Move, kein Clone)
            let mid_state = MultiBlockState::extract_from(decoder);

            // Extra-Streams dekomprimieren (Decompressor-Reuse, jeder Stream
            // wird genau einmal dekomprimiert). Kein Parallel-Pfad: die
            // Stream-Grenzen lassen sich nur durch Dekomprimieren bestimmen
            // (deflate_stream_size), d.h. ein Probe-Pass wäre genauso teuer
            // wie die eigentliche Dekomprimierung — doppelte Arbeit ohne Gewinn.
            let mut value_data = Vec::with_capacity(first_stream.len());
            for i in 0..extra_count {
                let (stream, consumed) = self.decompress_next()?
                    .ok_or_else(|| {
                        Error::DecompressionError(format!(
                            "Block {}: missing value channel stream {}/{}", self.block_number, i+1, extra_count
                        ))
                    })?;
                self.deflate_offset += consumed;
                value_data.extend(stream);
            }

            // Neuer Decoder auf Value-Daten (from_state, kein Clone)
            let mut value_decoder = Decoder::from_state(
                &value_data, mid_state, &self.precomp_options, &self.schema,
            );
            // Dedup-Tabellen an Value-Decoder übertragen
            value_decoder.pending_channel_qnames = channel_qnames;
            value_decoder.pending_type_defs = type_defs;
            value_decoder.decode_value_phase_compact(&mut buffer, pending)?;
            self.block_state = Some(MultiBlockState::extract_from(value_decoder));
        }

        self.current_block = Some(buffer);
        self.current_pos = 0;

        Ok(())
    }
}

// ============================================================================
// Lazy PreCompression-Decode (block-by-block, ohne DEFLATE)
// ============================================================================

/// Lazy PreCompression-Decode State.
///
/// Dekodiert PreCompression-Streams blockweise und liefert Events aus
/// CompactBlockBuffer, statt den gesamten Stream zu puffern.
pub(super) struct PrecompressionLazyState<'a> {
    decoder: Option<Decoder<'a>>,
    current_block: Option<CompactBlockBuffer>,
    current_pos: usize,
    finished: bool,
    failed: bool,
    block_number: usize,
}

impl<'a> PrecompressionLazyState<'a> {
    pub(super) fn new(decoder: Decoder<'a>) -> Self {
        Self {
            decoder: Some(decoder),
            current_block: None,
            current_pos: 0,
            finished: false,
            failed: false,
            block_number: 0,
        }
    }

    pub(super) fn set_memory_monitor(&mut self, monitor: MemoryMonitor) {
        if let Some(decoder) = &mut self.decoder {
            decoder.set_memory_monitor(monitor);
        }
    }

    pub(super) fn next_event(&mut self) -> Option<Result<ExiEvent>> {
        if self.failed {
            return None;
        }
        if self.decoder.is_none() {
            return None;
        }

        loop {
            // 1. Aus aktuellem CompactBlockBuffer yielden
            if let Some(ref mut buffer) = self.current_block {
                let decoder = self.decoder.as_mut().unwrap();
                let (qname_pool, interner) = (&mut decoder.qname_pool, &decoder.interner);
                if let Some(event) = buffer.take_event(self.current_pos, qname_pool, interner) {
                    self.current_pos += 1;
                    if matches!(event, ExiEvent::EndDocument) {
                        self.finished = true;
                    }
                    return Some(Ok(event));
                }
                // Buffer erschöpft → freigeben
                self.current_block = None;
                self.current_pos = 0;
            }

            // 2. Kein Block mehr → Auto-finish
            if self.finished || self.decoder.as_ref().unwrap().reader.remaining_bits() == 0 {
                return match self.finish() {
                    Ok(()) => None,
                    Err(e) => {
                        self.failed = true;
                        Some(Err(e))
                    }
                };
            }

            // 3. Nächsten Block dekodieren
            match self.decode_next_block() {
                Ok(()) => continue,
                Err(e) => {
                    self.failed = true;
                    return Some(Err(e));
                }
            }
        }
    }

    pub(super) fn finish(&mut self) -> Result<()> {
        let decoder = self.decoder.take()
            .ok_or_else(|| Error::schema_violation("finish(): decoder missing"))?;
        decoder.finish()
    }

    fn decode_next_block(&mut self) -> Result<()> {
        self.block_number += 1;
        let decoder = self.decoder.as_mut()
            .ok_or_else(|| Error::schema_violation("decode_next_block(): decoder missing"))?;
        let mut block_size = decoder.options.block_size as usize;
        if block_size == 0 {
            block_size = usize::MAX;
        }

        let (mut buffer, pending, _has_end_document) =
            decoder.decode_structure_phase_compact(block_size)?;
        decoder.decode_value_phase_compact(&mut buffer, pending)?;
        // Byte-Alignment nach Value Channels (Spec 9.2.2)
        decoder.reader.align_to_byte();

        // Per-Block Tabellen aufraeumen, um unbounded growth zu vermeiden
        decoder.pending_channel_qnames.clear();
        decoder.pending_channel_index.clear();
        decoder.pending_type_defs.clear();
        decoder.pending_type_ptr_index.clear();

        self.current_block = Some(buffer);
        self.current_pos = 0;
        Ok(())
    }
}
