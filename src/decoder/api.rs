use crate::error::{Error, Result};
use crate::options::{Alignment, ExiOptions};
use crate::schema::SchemaInfo;

use super::{DecodeIter, DecodeIterInner, Decoder};

/// Erstellt einen Decode-Iterator mit externen Options.
///
/// Returns: (DecodeIter, ExiOptions)
pub fn decode_iter_with_options(
    data: &[u8],
    options: ExiOptions,
) -> Result<(DecodeIter<'_>, ExiOptions)> {
    let decoder = Decoder::with_options(data, options)?;
    build_decode_iter(data, decoder)
}

/// Erstellt einen Decode-Iterator mit Schema.
///
/// Returns: (DecodeIter, ExiOptions)
pub fn decode_iter_with_schema<'a>(
    data: &'a [u8],
    options: ExiOptions,
    schema: &SchemaInfo,
) -> Result<(DecodeIter<'a>, ExiOptions)> {
    let decoder = Decoder::with_schema(data, options, schema.clone())?;
    build_decode_iter(data, decoder)
}

/// Gemeinsame Logik: Header lesen, ggf. Compression-State aufbauen.
fn build_decode_iter<'a>(
    data: &'a [u8],
    mut decoder: Decoder<'a>,
) -> Result<(DecodeIter<'a>, ExiOptions)> {
    decoder.read_header()?;
    if let Some(schema_id) = decoder.options().schema_id() {
        let has_schema = decoder.schema.is_some();
        match schema_id {
            crate::options::SchemaId::Id(_) if !has_schema => {
                return Err(Error::InvalidOptionCombination);
            }
            crate::options::SchemaId::None | crate::options::SchemaId::BuiltinOnly if has_schema => {
                return Err(Error::InvalidOptionCombination);
            }
            _ => {}
        }
    }

    if decoder.options().compression {
        let header_end = decoder.reader.bit_position().div_ceil(8);
        let compressed_body = &data[header_end..];
        let block_size = decoder.options().block_size as usize;

        let mut precomp_options = decoder.options().clone();
        precomp_options.compression = false;
        precomp_options.alignment = Alignment::PreCompression;

        let schema = decoder.schema.clone();

        let mut final_options = decoder.options().clone();
        final_options.compression = true;
        final_options.alignment = Alignment::BitPacked;

        let state = crate::decoder::lazy::CompressedLazyState::new(
            compressed_body,
            precomp_options,
            schema,
            block_size,
        )?;

        return Ok((
            DecodeIter { inner: DecodeIterInner::CompressedLazy { state: Box::new(state) } },
            final_options,
        ));
    }

    if decoder.options().alignment == Alignment::PreCompression {
        let opts = decoder.options().clone();
        let state = crate::decoder::lazy::PrecompressionLazyState::new(decoder);
        return Ok((
            DecodeIter { inner: DecodeIterInner::PrecompressionLazy { state: Box::new(state) } },
            opts,
        ));
    }

    let opts = decoder.options().clone();
    Ok((
        DecodeIter { inner: DecodeIterInner::Stream { decoder: Some(decoder) } },
        opts,
    ))
}

/// Prüft ob ein EXI-Stream Compression verwendet (Header-Probe).
///
/// Liest den Header und gibt `true` zurück wenn die effektiven Options
/// `compression=true` haben.
pub fn probe_compression(data: &[u8], options: ExiOptions) -> bool {
    let Ok(mut decoder) = Decoder::with_options(data, options) else {
        return false;
    };
    if decoder.read_header().is_err() {
        return false;
    }
    decoder.options().compression
}

// ============================================================================
// High-Level API
// ============================================================================

/// Decodiert einen EXI Stream zu einer Event-Sequenz.
///
/// Returns: (events, options)
pub fn decode(data: &[u8]) -> Result<(Vec<crate::event::ExiEvent>, ExiOptions)> {
    decode_with_options(data, ExiOptions::default())
}

/// Decodiert einen EXI Stream mit externen Options.
///
/// Diese Funktion wird verwendet wenn die EXI Options out-of-band bekannt sind
/// (z.B. bei Streams ohne Options im Header). Die externen Options werden als
/// Basis verwendet und nur überschrieben wenn der Header Options enthält.
///
/// # Spec-Referenz
/// - Spec 5.4: "If the options_present flag is false, the EXI options are
///   communicated out-of-band"
///
/// Returns: (events, options)
pub fn decode_with_options(
    data: &[u8],
    options: ExiOptions,
) -> Result<(Vec<crate::event::ExiEvent>, ExiOptions)> {
    let (iter, opts) = decode_iter_with_options(data, options)?;
    let events: Result<Vec<crate::event::ExiEvent>> = iter.collect();
    Ok((events?, opts))
}

/// Decodiert einen EXI Stream mit Schema (Schema-informed Decoding).
///
/// Bei Schema-informed Decoding werden:
/// - Schema-informed Document Grammar verwendet (Spec 8.5.1)
/// - Typed Values decodiert statt Strings (Spec 7.1)
/// - String Table mit Schema pre-populated (Spec 7.3.1)
///
/// # Spec-Referenz
/// - Spec 8.5: Schema-informed Grammars
/// - Spec 7.1: Built-in EXI Datatype Representations
///
/// Returns: (events, options)
pub fn decode_with_schema(
    data: &[u8],
    options: ExiOptions,
    schema: &SchemaInfo,
) -> Result<(Vec<crate::event::ExiEvent>, ExiOptions)> {
    let (iter, opts) = decode_iter_with_schema(data, options, schema)?;
    let events: Result<Vec<crate::event::ExiEvent>> = iter.collect();
    Ok((events?, opts))
}
