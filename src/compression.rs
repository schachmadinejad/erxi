//! EXI Compression (Spec 9, 9.1, 9.2, 9.3).
//!
//! Dieses Modul implementiert:
//! - Blocks: Partitionierung von Events in Blöcke (Spec 9.1)
//! - Channels: Structure Channel + Value Channels (Spec 9.2)
//! - Compressed Streams: Kombination/Splitting basierend auf Value-Anzahl (Spec 9.3)
//! - DEFLATE: RFC 1951 Kompression (flate2 crate)
//!
//! # Architektur
//!
//! ```text
//! Events → [Block 1] → [Block 2] → ... → [Block N]
//!            ↓
//!          [Structure Channel] + [Value Channels nach QName]
//!            ↓
//!          [Compressed Streams] (kombiniert oder einzeln, je nach Value-Anzahl)
//!            ↓
//!          [DEFLATE] (wenn compression=true) oder [direkt] (pre-compression)
//! ```

use std::io::Read;

use flate2::read::DeflateDecoder;
use flate2::{Compress, Compression, Decompress, FlushCompress, FlushDecompress, Status};
use crate::precompression::ChannelKey;
use crate::FastIndexMap;

/// Structure Channel: Event Codes + Content (ohne AT/CH Values).
///
/// Spec 9.2.1: Enthält alle Event Codes und Content, außer AT/CH Values.
/// Ausnahme: xsi:type und xsi:nil Values bleiben im Structure Channel.
#[derive(Debug, Default)]
pub struct StructureChannel {
    data: Vec<u8>,
}

impl StructureChannel {
    /// Erstellt einen neuen, leeren Structure Channel.
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    /// Schreibt Bytes in den Channel.
    pub fn write(&mut self, bytes: &[u8]) {
        self.data.extend_from_slice(bytes);
    }

    /// Gibt die Daten als Slice zurück.
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    /// Gibt die Länge in Bytes zurück.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Prüft ob der Channel leer ist.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

/// Value Channel: AT/CH Values für einen QName.
///
/// Spec 9.2.2: Jeder Channel enthält Values für genau einen QName (URI, local-name).
#[derive(Debug)]
pub struct ValueChannel {
    qname: ChannelKey,
    data: Vec<u8>,
    value_count: usize,
}

impl ValueChannel {
    /// Erstellt einen neuen Value Channel für den gegebenen QName.
    pub fn new(qname: ChannelKey) -> Self {
        Self {
            qname,
            data: Vec::new(),
            value_count: 0,
        }
    }

    /// Schreibt einen Value in den Channel.
    pub fn write_value(&mut self, bytes: &[u8]) {
        self.data.extend_from_slice(bytes);
        self.value_count += 1;
    }

    /// Gibt den QName zurück.
    pub fn qname(&self) -> &ChannelKey {
        &self.qname
    }

    /// Gibt die Daten als Slice zurück.
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    /// Gibt die Anzahl der Values zurück.
    pub fn value_count(&self) -> usize {
        self.value_count
    }

    /// Gibt die Länge in Bytes zurück.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Prüft ob der Channel leer ist.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

/// Ein Block mit Structure Channel + Value Channels.
///
/// Spec 9.1: Jeder Block (außer der letzte) enthält genau `block_size` Values.
/// IndexMap behält automatisch die Insertion-Order.
#[derive(Debug, Default)]
pub struct Block {
    structure: StructureChannel,
    value_channels: FastIndexMap<ChannelKey, ValueChannel>,
    total_values: usize,
}

impl Block {
    /// Erstellt einen neuen, leeren Block.
    pub fn new() -> Self {
        Self {
            structure: StructureChannel::new(),
            value_channels: FastIndexMap::default(),
            total_values: 0,
        }
    }

    /// Schreibt Bytes in den Structure Channel.
    pub fn write_structure(&mut self, bytes: &[u8]) {
        self.structure.write(bytes);
    }

    /// Schreibt einen Value in den Value Channel für den gegebenen QName.
    pub fn write_value(&mut self, qname: ChannelKey, bytes: &[u8]) {
        let channel = self
            .value_channels
            .entry(qname.clone())
            .or_insert_with(|| ValueChannel::new(qname));
        channel.write_value(bytes);
        self.total_values += 1;
    }

    /// Gibt die Gesamtzahl der Values im Block zurück.
    pub fn total_values(&self) -> usize {
        self.total_values
    }

    /// Gibt den Structure Channel zurück.
    pub fn structure(&self) -> &StructureChannel {
        &self.structure
    }

    /// Gibt die Value Channels in Reihenfolge des ersten Auftretens zurück.
    pub fn value_channels_ordered(&self) -> Vec<&ValueChannel> {
        self.value_channels.values().collect()
    }

    /// Prüft ob der Block voll ist (>= block_size Values).
    pub fn is_full(&self, block_size: u32) -> bool {
        self.total_values >= block_size as usize
    }
}

/// Komprimiert Daten mit DEFLATE (RFC 1951).
///
/// Spec 9.3: "each compressed stream in a block is stored using the standard
/// DEFLATE Compressed Data Format defined by RFC 1951"
pub fn deflate_compress(data: &[u8]) -> crate::Result<Vec<u8>> {
    let mut compressor = Compress::new(Compression::default(), false);
    deflate_compress_reuse(data, &mut compressor)
}

/// Komprimiert Daten mit DEFLATE unter Wiederverwendung einer Compress-Instanz.
///
/// Spart die Allokation interner zlib-Strukturen (~256 KB) pro Aufruf.
/// Nur im sequentiellen Pfad sinnvoll, da bei paralleler
/// DEFLATE-Kompression jeder Thread seine eigene Compress-Instanz benötigt.
pub fn deflate_compress_reuse(data: &[u8], compressor: &mut Compress) -> crate::Result<Vec<u8>> {
    compressor.reset();
    let max_len = data.len() + 64;
    let mut output = Vec::with_capacity(max_len);
    let mut offset = 0;
    loop {
        let mut out_buf = [0u8; 8192];
        let before_in = compressor.total_in() as usize;
        let before_out = compressor.total_out() as usize;

        let flush = if offset >= data.len() {
            FlushCompress::Finish
        } else {
            FlushCompress::None
        };

        let status = compressor
            .compress(&data[offset..], &mut out_buf, flush)
            .map_err(|e| {
                crate::Error::CompressionError(format!("DEFLATE compress failed: {e}"))
            })?;

        let consumed = (compressor.total_in() as usize).saturating_sub(before_in);
        let produced = (compressor.total_out() as usize).saturating_sub(before_out);
        offset += consumed;

        if produced > 0 {
            output.extend_from_slice(&out_buf[..produced]);
        }

        match status {
            Status::StreamEnd => return Ok(output),
            Status::Ok | Status::BufError => {
                if consumed == 0 && produced == 0 && offset >= data.len() {
                    return Err(crate::Error::CompressionError(
                        "DEFLATE compress stalled: all input consumed but no StreamEnd".to_string(),
                    ));
                }
            }
        }
    }
}

/// Dekomprimiert DEFLATE-Daten unter Wiederverwendung einer Decompress-Instanz.
///
/// Wie `deflate_decompress_stream`, aber mit Reuse der internen zlib-Strukturen.
/// `reset(false)` = Raw DEFLATE (RFC 1951), kein zlib-Header.
/// Gibt (decompressed_data, bytes_consumed) zurück.
pub fn deflate_decompress_reuse(
    data: &[u8],
    decompressor: &mut Decompress,
) -> crate::Result<(Vec<u8>, usize)> {
    decompressor.reset(false);
    let mut result = Vec::new();
    let mut offset = 0usize;

    loop {
        let mut out_buf = [0u8; 8192];
        let before_in = decompressor.total_in() as usize;
        let before_out = decompressor.total_out() as usize;

        let flush = if offset >= data.len() {
            FlushDecompress::Finish
        } else {
            FlushDecompress::None
        };

        let status = decompressor
            .decompress(&data[offset..], &mut out_buf, flush)
            .map_err(|e| {
                crate::Error::DecompressionError(format!("DEFLATE decompression failed: {e}"))
            })?;

        let consumed = (decompressor.total_in() as usize).saturating_sub(before_in);
        let produced = (decompressor.total_out() as usize).saturating_sub(before_out);
        offset += consumed;

        if produced > 0 {
            result.extend_from_slice(&out_buf[..produced]);
        }

        match status {
            Status::StreamEnd => break,
            Status::Ok | Status::BufError => {
                if consumed == 0 && produced == 0 {
                    return Err(crate::Error::DecompressionError(
                        "DEFLATE decompression failed: PrematureEndOfStream".to_string(),
                    ));
                }
            }
        }
    }

    Ok((result, offset))
}

/// Dekomprimiert DEFLATE-komprimierte Daten.
pub fn deflate_decompress(data: &[u8]) -> crate::Result<Vec<u8>> {
    let mut decoder = DeflateDecoder::new(data);
    let mut result = Vec::new();

    let mut buffer = [0u8; 8192];
    loop {
        let bytes_read = decoder
            .read(&mut buffer)
            .map_err(|e| crate::Error::DecompressionError(format!("DEFLATE decompression failed: {e}")))?;

        if bytes_read == 0 {
            break;
        }

        result.extend_from_slice(&buffer[..bytes_read]);
    }

    Ok(result)
}

/// Dekomprimiert einen DEFLATE-Stream und gibt die dekomprimierten Daten
/// sowie die Anzahl der gelesenen komprimierten Bytes zurück.
///
/// Diese Funktion ist nützlich für Multi-Stream Dekomprimierung (Spec 9.3),
/// wo mehrere DEFLATE-Streams hintereinander gespeichert sind.
///
/// # Returns
/// `(decompressed_data, bytes_consumed)` - Die dekomprimierten Daten und
/// die Anzahl der gelesenen Bytes aus dem Input.
pub fn deflate_decompress_stream(data: &[u8]) -> crate::Result<(Vec<u8>, usize)> {
    let mut decompressor = Decompress::new(false);
    deflate_decompress_reuse(data, &mut decompressor)
}

/// Ermittelt die Größe eines DEFLATE-Streams in Bytes ohne Daten-Allokation.
///
/// Wie `deflate_decompress_stream`, aber ohne Ergebnis-Vec — dekomprimiert
/// in einen Fixpuffer der immer überschrieben wird. Gibt nur `bytes_consumed`
/// zurück. Nützlich für Stream-Grenzen-Ermittlung ohne Speicherallokation.
pub fn deflate_stream_size(data: &[u8]) -> crate::Result<usize> {
    let mut decoder = Decompress::new(false);
    let mut offset = 0usize;
    let mut out_buf = [0u8; 8192];

    loop {
        let before_in = decoder.total_in() as usize;
        let before_out = decoder.total_out() as usize;

        // Wie deflate_decompress_reuse: wenn alle Input-Bytes verbraucht sind,
        // FlushDecompress::Finish senden damit der Decompressor StreamEnd
        // emittieren kann (noetig wenn der letzte decompress-Aufruf alle Bytes
        // verbraucht hat aber noch Status::Ok zurueckgab).
        let flush = if offset >= data.len() {
            FlushDecompress::Finish
        } else {
            FlushDecompress::None
        };

        let status = decoder
            .decompress(&data[offset..], &mut out_buf, flush)
            .map_err(|e| {
                crate::Error::DecompressionError(format!("DEFLATE stream size failed: {e}"))
            })?;

        let consumed = (decoder.total_in() as usize).saturating_sub(before_in);
        let produced = (decoder.total_out() as usize).saturating_sub(before_out);
        offset = offset.saturating_add(consumed);

        match status {
            Status::StreamEnd => return Ok(offset),
            Status::Ok | Status::BufError => {
                if consumed == 0 && produced == 0 {
                    return Err(crate::Error::DecompressionError(
                        "DEFLATE stream size: PrematureEndOfStream".to_string(),
                    ));
                }
            }
        }
    }
}

/// Schreibt einen Block als Compressed Streams.
///
/// Spec 9.3:
/// - ≤100 Values: 1 Stream (Structure + alle Value Channels)
/// - >100 Values:
///   - Stream 1: nur Structure
///   - Stream 2: Value Channels mit ≤100 Values (kombiniert)
///   - Streams 3+: je ein Channel mit >100 Values
///
/// # Parameter
/// - `block`: Der zu schreibende Block
/// - `use_deflate`: true für DEFLATE, false für Pre-Compression
///
/// # Returns
/// Vec von Compressed Streams (jeweils als Vec<u8>)
pub fn write_compressed_streams(block: &Block, use_deflate: bool) -> crate::Result<Vec<Vec<u8>>> {
    let mut streams = Vec::new();

    // Channel-Counts für Sortierung sammeln (IndexMap behält Insertion-Order)
    let channel_counts: FastIndexMap<ChannelKey, usize> = block
        .value_channels
        .iter()
        .map(|(k, v)| (k.clone(), v.value_count()))
        .collect();

    // Channels nach Spec 9.3 sortieren
    let ordered_channels = crate::precompression::order_channels(&channel_counts, block.total_values);

    if block.total_values <= 100 {
        // ≤100 Values: Ein kombinierter Stream
        let mut combined = Vec::new();
        combined.extend_from_slice(block.structure.data());
        for key in &ordered_channels {
            if let Some(channel) = block.value_channels.get(key) {
                combined.extend_from_slice(channel.data());
            }
        }
        streams.push(compress_if_needed(&combined, use_deflate)?);
    } else {
        // >100 Values: Mehrere Streams
        // Stream 1: nur Structure
        streams.push(compress_if_needed(block.structure.data(), use_deflate)?);

        // Stream 2: kleine Channels (≤100 Values) kombiniert
        let mut small_combined = Vec::new();
        let mut large_channels = Vec::new();

        for key in &ordered_channels {
            if let Some(channel) = block.value_channels.get(key) {
                if channel.value_count() <= 100 {
                    small_combined.extend_from_slice(channel.data());
                } else {
                    large_channels.push(channel);
                }
            }
        }

        // Stream 2 nur schreiben wenn kleine Channels existieren
        if !small_combined.is_empty() {
            streams.push(compress_if_needed(&small_combined, use_deflate)?);
        }

        // Streams 3+: große Channels einzeln
        for channel in large_channels {
            streams.push(compress_if_needed(channel.data(), use_deflate)?);
        }
    }

    // Spec 9.3 Note: Leere Streams vermeiden
    streams.retain(|s| !s.is_empty());

    Ok(streams)
}

/// Komprimiert Daten wenn use_deflate true ist, sonst direkt zurückgeben.
fn compress_if_needed(data: &[u8], use_deflate: bool) -> crate::Result<Vec<u8>> {
    if data.is_empty() {
        return Ok(Vec::new());
    }
    if use_deflate {
        deflate_compress(data)
    } else {
        Ok(data.to_vec())
    }
}

/// Extrahiert eine Fehlermeldung aus einem Thread-Panic-Payload.
///
/// `std::thread::JoinHandle::join()` gibt bei Panic ein `Box<dyn Any>` zurueck,
/// das typischerweise `&str` oder `String` enthaelt. Diese Funktion extrahiert
/// die Nachricht und formatiert sie als DEFLATE-Thread-Panic-Meldung.
pub(crate) fn thread_panic_message(payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(s) = payload.downcast_ref::<&str>() {
        format!("DEFLATE thread panicked: {s}")
    } else if let Some(s) = payload.downcast_ref::<String>() {
        format!("DEFLATE thread panicked: {s}")
    } else {
        "DEFLATE thread panicked".into()
    }
}

/// Fuehrt `work` parallel auf `items` aus mit maximal `thread_count` gleichzeitigen Threads.
///
/// Ergebnisse werden in Input-Reihenfolge zurueckgegeben. Bei Panic eines Threads
/// wird `wrap_panic` aufgerufen um den Panic-Payload in einen `Error` zu konvertieren.
pub(crate) fn bounded_parallel_map<I, T, F>(
    items: &[I],
    thread_count: usize,
    work: F,
    wrap_panic: fn(String) -> crate::Error,
) -> crate::Result<Vec<T>>
where
    I: Sync,
    T: Send,
    F: Fn(&I) -> crate::Result<T> + Send + Sync + Copy,
{
    let mut results = Vec::with_capacity(items.len());
    std::thread::scope(|s| -> crate::Result<()> {
        let mut handles = std::collections::VecDeque::with_capacity(items.len());
        for item in items {
            handles.push_back(s.spawn(move || work(item)));
            if handles.len() >= thread_count {
                let h = handles.pop_front().unwrap();
                results.push(
                    h.join()
                        .unwrap_or_else(|e| Err(wrap_panic(thread_panic_message(e))))?,
                );
            }
        }
        for h in handles {
            results.push(
                h.join()
                    .unwrap_or_else(|e| Err(wrap_panic(thread_panic_message(e))))?,
            );
        }
        Ok(())
    })?;
    Ok(results)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    fn key(local: &str) -> ChannelKey {
        ChannelKey::new(Rc::from(""), Rc::from(local))
    }

    // ==================== StructureChannel Tests ====================

    /// Structure Channel speichert Bytes korrekt.
    #[test]
    fn structure_channel_write_and_read() {
        let mut ch = StructureChannel::new();
        assert!(ch.is_empty());

        ch.write(&[1, 2, 3]);
        ch.write(&[4, 5]);

        assert_eq!(ch.len(), 5);
        assert_eq!(ch.data(), &[1, 2, 3, 4, 5]);
        assert!(!ch.is_empty());
    }

    // ==================== ValueChannel Tests ====================

    /// Value Channel speichert QName und Values.
    #[test]
    fn value_channel_basic() {
        let qname = key("attr");
        let mut ch = ValueChannel::new(qname.clone());

        assert_eq!(ch.qname(), &qname);
        assert_eq!(ch.value_count(), 0);
        assert!(ch.is_empty());

        ch.write_value(&[10, 20]);
        ch.write_value(&[30]);

        assert_eq!(ch.value_count(), 2);
        assert_eq!(ch.len(), 3);
        assert_eq!(ch.data(), &[10, 20, 30]);
    }

    // ==================== Block Tests ====================

    /// Block sammelt Structure und Value Daten.
    #[test]
    fn block_basic() {
        let mut block = Block::new();

        block.write_structure(&[1, 2]);
        block.write_value(key("a"), &[10]);
        block.write_value(key("b"), &[20]);
        block.write_value(key("a"), &[11]);

        assert_eq!(block.total_values(), 3);
        assert_eq!(block.structure().data(), &[1, 2]);

        let channels = block.value_channels_ordered();
        assert_eq!(channels.len(), 2);
        assert_eq!(channels[0].qname(), &key("a"));
        assert_eq!(channels[0].value_count(), 2);
        assert_eq!(channels[1].qname(), &key("b"));
        assert_eq!(channels[1].value_count(), 1);
    }

    /// Block erkennt wenn er voll ist.
    #[test]
    fn block_is_full() {
        let mut block = Block::new();
        assert!(!block.is_full(3));

        block.write_value(key("a"), &[1]);
        block.write_value(key("a"), &[2]);
        assert!(!block.is_full(3));

        block.write_value(key("a"), &[3]);
        assert!(block.is_full(3));
    }

    // ==================== DEFLATE Tests ====================

    /// DEFLATE Roundtrip.
    #[test]
    fn deflate_roundtrip() {
        let original = b"Hello, EXI Compression! This is a test of DEFLATE.";
        let compressed = deflate_compress(original).unwrap();
        let decompressed = deflate_decompress(&compressed).unwrap();

        assert_eq!(decompressed, original);
    }

    /// DEFLATE mit leerem Input.
    #[test]
    fn deflate_empty() {
        let compressed = deflate_compress(&[]).unwrap();
        let decompressed = deflate_decompress(&compressed).unwrap();

        assert!(decompressed.is_empty());
    }

    // ==================== Compressed Streams Tests ====================

    /// Spec 9.3: ≤100 Values → 1 kombinierter Stream.
    #[test]
    fn compressed_streams_small_block() {
        let mut block = Block::new();
        block.write_structure(&[1, 2, 3]);

        for i in 0..50 {
            block.write_value(key("a"), &[i]);
        }

        let streams = write_compressed_streams(&block, false).unwrap();

        assert_eq!(streams.len(), 1);
        // Stream enthält Structure (3 Bytes) + Values (50 Bytes)
        assert_eq!(streams[0].len(), 53);
    }

    /// Spec 9.3: >100 Values → mehrere Streams.
    #[test]
    fn compressed_streams_large_block() {
        let mut block = Block::new();
        block.write_structure(&[1, 2, 3]);

        // 150 Values in Channel "big"
        for i in 0..150 {
            block.write_value(key("big"), &[i as u8]);
        }

        let streams = write_compressed_streams(&block, false).unwrap();

        // Stream 1: Structure (3 Bytes)
        // Stream 2: keine kleinen Channels
        // Stream 3: großer Channel (150 Bytes)
        assert_eq!(streams.len(), 2);
        assert_eq!(streams[0].len(), 3); // Structure
        assert_eq!(streams[1].len(), 150); // big channel
    }

    /// Spec 9.3: >100 Values mit gemischten Channels.
    #[test]
    fn compressed_streams_mixed_channels() {
        let mut block = Block::new();
        block.write_structure(&[1]);

        // 50 Values in "small"
        for _ in 0..50 {
            block.write_value(key("small"), &[1]);
        }
        // 60 Values in "big" (>100 total, aber channel selbst ≤100)
        for _ in 0..60 {
            block.write_value(key("medium"), &[2]);
        }

        // Total: 110 Values (>100)
        let streams = write_compressed_streams(&block, false).unwrap();

        // Stream 1: Structure (1 Byte)
        // Stream 2: small + medium kombiniert (50+60 = 110 Bytes)
        assert_eq!(streams.len(), 2);
        assert_eq!(streams[0].len(), 1);
        assert_eq!(streams[1].len(), 110);
    }

    /// Spec 9.3: DEFLATE wird angewendet wenn use_deflate=true.
    #[test]
    fn compressed_streams_with_deflate() {
        let mut block = Block::new();
        // Wiederholende Daten für bessere Kompression
        block.write_structure(&[0; 100]);

        let streams_raw = write_compressed_streams(&block, false).unwrap();
        let streams_deflate = write_compressed_streams(&block, true).unwrap();

        assert_eq!(streams_raw[0].len(), 100);
        // DEFLATE sollte besser komprimieren (wiederholende Nullen)
        assert!(streams_deflate[0].len() < 100);
    }

    /// Spec 9.3 Note: Leere Streams werden vermieden.
    #[test]
    fn compressed_streams_no_empty() {
        let block = Block::new(); // Komplett leerer Block

        let streams = write_compressed_streams(&block, false).unwrap();

        assert!(streams.is_empty());
    }
}
