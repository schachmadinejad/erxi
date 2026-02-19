use crate::options::{Alignment, ExiOptions};
use crate::{Error, Result};

/// Encoder-Konfiguration (Header-Optionen).
#[derive(Debug, Clone, Default)]
pub struct EncoderConfig {
    /// "$EXI" Cookie am Anfang schreiben.
    pub include_cookie: bool,
    /// Options-Header immer schreiben (auch bei Default-Options).
    pub include_options: bool,
}

impl EncoderConfig {
    /// Konfiguration mit Cookie.
    pub fn with_cookie() -> Self {
        Self {
            include_cookie: true,
            include_options: false,
        }
    }
}

pub(crate) fn ensure_streaming_compatible(options: &ExiOptions) -> Result<()> {
    if options.compression {
        return Err(Error::SchemaViolation(
            "Streaming ist mit Compression nicht kompatibel (Spec 9)".into(),
        ));
    }
    if matches!(options.alignment(), Alignment::PreCompression) {
        return Err(Error::SchemaViolation(
            "Streaming ist mit PreCompression nicht kompatibel (Spec 9)".into(),
        ));
    }
    Ok(())
}

/// Erzeugt EncoderConfig mit `include_options=true` wenn Options non-default.
pub(crate) fn auto_config(options: &ExiOptions) -> EncoderConfig {
    if options.compression
        || matches!(options.alignment(), crate::options::Alignment::PreCompression | crate::options::Alignment::ByteAlignment)
        || options.strict
        || options.fragment
        || options.preserve.has_header_relevant_flags()
        || options.block_size != 1_000_000
        || options.value_max_length.is_some()
        || options.value_partition_capacity.is_some()
        || options.self_contained
        || options.schema_id.is_some()
        || !options.datatype_representation_map.is_empty()
    {
        EncoderConfig {
            include_options: true,
            ..EncoderConfig::default()
        }
    } else {
        EncoderConfig::default()
    }
}
