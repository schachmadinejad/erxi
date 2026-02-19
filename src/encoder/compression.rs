//! Encoder Compression / PreCompression (Spec 9).
//!
//! Enthält die Stream-Schreiblogik für Compression und PreCompression:
//! Block-Aufteilung (Spec 9.1), Channel-Ordering (Spec 9.3),
//! DEFLATE-Kompression mit optionaler Parallelisierung.

use std::rc::Rc;

use crate::bitstream::BitWriter;
use crate::precompression::{ChannelKey, order_channels};
use crate::qname::QName;
use crate::schema::TypeDefinition;
use crate::{Error, FastIndexMap, Result};

use super::{Encoder, PendingValue};

impl Encoder {
    /// Schreibt Compressed Streams für Compression/PreCompression (Spec 9.1, 9.3).
    ///
    /// # Spec 9.1 (Blocks):
    /// - Jeder Block (außer der letzte) enthält genau `block_size` Values
    /// - Der letzte Block enthält weniger als `block_size` Values
    ///
    /// # Spec 9.3 (Streams pro Block):
    /// - ≤100 Values: 1 Stream (Structure + alle Value Channels)
    /// - >100 Values:
    ///   - Stream 1: nur Structure Channel
    ///   - Stream 2: Value Channels mit ≤100 Values (kombiniert)
    ///   - Streams 3+: je ein Channel mit >100 Values
    pub(super) fn write_compressed_streams(&mut self) -> Result<()> {
        // Ownership-Transfer via into_vec() + split_off(): vermeidet Kopien.
        let mut all_data = std::mem::replace(&mut self.writer, BitWriter::new()).into_vec();
        if self.header_end_byte > all_data.len() {
            return Err(Error::CompressionError(format!(
                "header_end_byte ({}) exceeds data length ({})",
                self.header_end_byte, all_data.len()
            )));
        }
        let structure_data = all_data.split_off(self.header_end_byte);
        let header_data = all_data;
        self.writer.write_bytes_aligned(&header_data);

        let pending_values = std::mem::take(&mut self.pending_values);
        let string_buffer = std::mem::take(&mut self.pending_string_buffer);
        let type_defs = std::mem::take(&mut self.pending_type_defs);
        let block_boundaries = std::mem::take(&mut self.block_boundaries);
        let use_deflate = self.options.compression;

        // Spec 9.1: Multi-Block wenn Block-Grenzen existieren
        if block_boundaries.is_empty() {
            // Single-Block: alle Structure-Daten und alle Values
            self.write_block(&structure_data, &pending_values, &string_buffer, &type_defs, use_deflate)?;
        } else {
            // Multi-Block: Blöcke einzeln verarbeiten
            self.write_multi_blocks(&structure_data, &pending_values, &string_buffer, &type_defs, &block_boundaries, use_deflate)?;
        }
        Ok(())
    }

    /// Schreibt mehrere Blöcke (Multi-Block Mode, Spec 9.1).
    fn write_multi_blocks(
        &mut self,
        structure_data: &[u8],
        pending_values: &[PendingValue],
        string_buffer: &[u8],
        type_defs: &[Rc<TypeDefinition>],
        block_boundaries: &[(usize, usize)],
        use_deflate: bool,
    ) -> Result<()> {
        let mut structure_start = 0usize;
        let mut values_start = 0usize;

        for &(structure_end, next_values_start) in block_boundaries {
            let block_structure = &structure_data[structure_start..structure_end];

            let drain_count = next_values_start.checked_sub(values_start)
                .ok_or_else(|| Error::CompressionError(format!(
                    "block boundary values_start {} < values_start {} (Spec 9.1)",
                    next_values_start, values_start
                )))?;
            let block_values = &pending_values[values_start..values_start + drain_count];

            self.write_block(block_structure, block_values, string_buffer, type_defs, use_deflate)?;

            structure_start = structure_end;
            values_start = next_values_start;
        }

        // Letzter Block: Rest der Values
        let final_structure = &structure_data[structure_start..];
        self.write_block(final_structure, &pending_values[values_start..], string_buffer, type_defs, use_deflate)
    }

    /// Schreibt einen einzelnen Block.
    fn write_block<'a>(
        &mut self,
        structure_data: &[u8],
        values: &[PendingValue],
        string_buffer: &'a [u8],
        type_defs: &'a [Rc<TypeDefinition>],
        use_deflate: bool,
    ) -> Result<()> {
        // Values nach Channel-Index gruppieren, QName aus Lookup-Table auflösen.
        // String-Bytes und TypeDefs werden aus den Shared-Buffern referenziert.
        let mut channels: FastIndexMap<
            ChannelKey,
            (QName, Vec<(&'a str, Option<&'a TypeDefinition>)>),
        > = FastIndexMap::default();
        for pv in values {
            let value_str = std::str::from_utf8(
                &string_buffer[pv.str_offset..pv.str_offset + pv.str_len as usize]
            ).map_err(|e| Error::CompressionError(format!("invalid UTF-8 in pending value: {e}")))?;
            let type_def = (pv.type_idx > 0)
                .then(|| &*type_defs[(pv.type_idx - 1) as usize]);
            let qname = &self.pending_channel_qnames[pv.channel_idx as usize];
            let key = ChannelKey::new(qname.uri.clone(), qname.local_name.clone());
            channels
                .entry(key)
                .or_insert_with(|| (qname.clone(), Vec::new()))
                .1
                .push((value_str, type_def));
        }

        self.write_block_streams(structure_data, &mut channels, use_deflate)
    }

    /// Schreibt die Streams für einen Block (Spec 9.3).
    ///
    /// Stream-Layout bei >100 Values (Spec 9.3):
    /// - Stream 1: Structure Channel
    /// - Stream 2: Kombinierte kleine Value Channels (≤100 Values) — auch wenn 0 Bytes
    /// - Streams 3+: Je ein großer Value Channel (>100 Values)
    ///
    /// Jeder Stream wird als DEFLATE-Block geschrieben (bei Compression), auch wenn
    /// die unkomprimierten Daten leer sind (z.B. bei Typen mit 0-Bit-Kodierung).
    /// Der Decoder erwartet exakt so viele Streams wie Channels vorhanden sind.
    fn write_block_streams(
        &mut self,
        structure_data: &[u8],
        channels: &mut FastIndexMap<ChannelKey, (QName, Vec<(&str, Option<&TypeDefinition>)>)>,
        use_deflate: bool,
    ) -> Result<()> {
        let channel_counts: FastIndexMap<ChannelKey, usize> = channels
            .iter()
            .map(|(k, (_, v))| (k.clone(), v.len()))
            .collect();
        let total_values: usize = channel_counts.values().sum();
        let write_order = order_channels(&channel_counts, total_values);

        if total_values <= 100 {
            // ≤100 Values: Ein kombinierter Stream
            if use_deflate {
                let mut combined = structure_data.to_vec();
                let value_data = self.encode_value_channels_to_buffer(&write_order, channels)?;
                combined.extend(value_data);

                let compressed = self.deflate_compress_data(&combined)?;
                self.writer.write_bytes_aligned(&compressed);
            } else {
                // PreCompression: Structure + Value Channels direkt schreiben
                self.writer.write_bytes_aligned(&structure_data);
                self.write_channels_inline(&write_order, channels)?;
                // Byte-Alignment nach Value Channels sicherstellen,
                // damit der nächste Block (Structure) byte-aligned startet.
                self.writer.align_to_byte();
            }
        } else {
            // >100 Values: Mehrere Streams
            let has_small = channel_counts.values().any(|&c| c <= 100);
            let mut small_channels: Vec<ChannelKey> = Vec::with_capacity(write_order.len());
            let mut large_channels: Vec<ChannelKey> = Vec::with_capacity(write_order.len());

            for key in &write_order {
                // Invariante: write_order Keys kommen aus channel_counts (via order_channels)
                let count = channel_counts.get(key).copied()
                    .expect("write_order key must exist in channel_counts");
                if count <= 100 {
                    small_channels.push(key.clone());
                } else {
                    large_channels.push(key.clone());
                }
            }

            if use_deflate {
                // Alle Rohpuffer sammeln (sequentiell)
                let small_data = self.encode_value_channels_to_buffer(&small_channels, channels)?;
                let mut large_data: Vec<Vec<u8>> = Vec::with_capacity(large_channels.len());
                for key in &large_channels {
                    large_data.push(self.encode_single_channel_to_buffer(key, channels)?);
                }

                // Stream-Anzahl: 1 (structure) + has_small? 1 : 0 + large_count
                // Jeder Stream wird geschrieben, auch wenn leer (0-Bit-Typen).
                // Der Decoder erwartet exakt diese Anzahl.
                let stream_count = 1 + usize::from(has_small) + large_data.len();

                // DEFLATE komprimieren — parallel wenn aktiviert und genug Streams
                const MIN_PARALLEL_SIZE: usize = 10 * 1024; // 10 KiB
                if self.parallel_deflate && stream_count > 1 {
                    let thread_count = std::thread::available_parallelism()
                        .map(|n| n.get())
                        .unwrap_or(1);

                    // Alle Streams mit Index sammeln (auch leere!)
                    let mut all_streams: Vec<(usize, &[u8])> = Vec::with_capacity(stream_count);
                    all_streams.push((0, structure_data));
                    if has_small {
                        all_streams.push((1, &small_data));
                    }
                    for (i, buf) in large_data.iter().enumerate() {
                        let idx = if has_small { 2 + i } else { 1 + i };
                        all_streams.push((idx, buf));
                    }

                    // Nach Größe partitionieren: >= 10 KiB parallel, Rest sequentiell
                    let (parallel_buffers, seq_buffers): (Vec<_>, Vec<_>) =
                        all_streams.into_iter().partition(|(_, buf)| buf.len() >= MIN_PARALLEL_SIZE);

                    // Große Streams parallel komprimieren
                    let mut compressed_results: Vec<(usize, Vec<u8>)> = if !parallel_buffers.is_empty() {
                        crate::compression::bounded_parallel_map(
                            &parallel_buffers,
                            thread_count,
                            |&(idx, buf)| {
                                let compressed = crate::compression::deflate_compress(buf)?;
                                Ok((idx, compressed))
                            },
                            Error::CompressionError,
                        )?
                    } else {
                        Vec::new()
                    };

                    // Kleine Streams sequentiell komprimieren (mit Compressor-Reuse)
                    for (idx, buf) in &seq_buffers {
                        let compressed = self.deflate_compress_data(buf)?;
                        compressed_results.push((*idx, compressed));
                    }

                    // Nach Index sortieren und sequentiell schreiben
                    compressed_results.sort_by_key(|(idx, _)| *idx);
                    for (_, compressed) in compressed_results {
                        self.writer.write_bytes_aligned(&compressed);
                    }
                } else {
                    // Sequentiell komprimieren mit Compressor-Reuse
                    let compressed = self.deflate_compress_data(structure_data)?;
                    self.writer.write_bytes_aligned(&compressed);

                    if has_small {
                        let compressed = self.deflate_compress_data(&small_data)?;
                        self.writer.write_bytes_aligned(&compressed);
                    }

                    for buf in &large_data {
                        let compressed = self.deflate_compress_data(buf)?;
                        self.writer.write_bytes_aligned(&compressed);
                    }
                }
            } else {
                // PreCompression ohne DEFLATE
                self.writer.write_bytes_aligned(&structure_data);
                // Kleine Channels zuerst, dann große
                self.write_channels_inline(&small_channels, channels)?;
                self.write_channels_inline(&large_channels, channels)?;
                // Byte-Alignment nach Value Channels sicherstellen,
                // damit der nächste Block (Structure) byte-aligned startet.
                self.writer.align_to_byte();
            }
        }
        Ok(())
    }

    /// DEFLATE-Kompression mit Compressor-Reuse (sequentieller Pfad).
    fn deflate_compress_data(&mut self, data: &[u8]) -> Result<Vec<u8>> {
        let compressor = self.compressor.get_or_insert_with(|| {
            flate2::Compress::new(flate2::Compression::default(), false)
        });
        crate::compression::deflate_compress_reuse(data, compressor)
    }

    /// Schreibt Channels direkt in den Writer (ohne Buffer).
    fn write_channels_inline(
        &mut self,
        keys: &[ChannelKey],
        channels: &mut FastIndexMap<ChannelKey, (QName, Vec<(&str, Option<&TypeDefinition>)>)>,
    ) -> Result<()> {
        for key in keys {
            if let Some((channel_qname, values)) = channels.shift_remove(key) {
                for (value, type_def) in values {
                    self.encode_value_typed_or_string(&channel_qname, value, type_def)?;
                }
            }
        }
        Ok(())
    }

    /// Encodiert Value Channels in einen Buffer (für DEFLATE).
    fn encode_value_channels_to_buffer(
        &mut self,
        keys: &[ChannelKey],
        channels: &mut FastIndexMap<ChannelKey, (QName, Vec<(&str, Option<&TypeDefinition>)>)>,
    ) -> Result<Vec<u8>> {
        let mut temp_writer = BitWriter::new();
        std::mem::swap(&mut self.writer, &mut temp_writer);
        self.write_channels_inline(keys, channels)?;
        std::mem::swap(&mut self.writer, &mut temp_writer);
        Ok(temp_writer.into_vec())
    }

    /// Encodiert einen einzelnen Channel in einen Buffer (für DEFLATE).
    fn encode_single_channel_to_buffer(
        &mut self,
        key: &ChannelKey,
        channels: &mut FastIndexMap<ChannelKey, (QName, Vec<(&str, Option<&TypeDefinition>)>)>,
    ) -> Result<Vec<u8>> {
        self.encode_value_channels_to_buffer(std::slice::from_ref(key), channels)
    }
}
