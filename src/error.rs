//! Central error types for the EXI 1.0 implementation.
//!
//! Each variant references the relevant W3C EXI 1.0 Second Edition spec section.

use core::fmt;
use std::borrow::Cow;

/// All error types defined by the EXI 1.0 specification.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum Error {
    /// EXI version number is not supported (Spec 5.3).
    UnsupportedVersion,
    /// EXI header is malformed (Spec 5).
    MalformedHeader,
    /// An event code does not match any production in the current grammar (Spec 6.1, 6.2).
    InvalidEventCode {
        /// Der Event Code der nicht passte (leer wenn nicht verfügbar).
        event_code: Cow<'static, str>,
        /// Der Grammar-Zustand in dem der Fehler auftrat (leer wenn nicht verfügbar).
        grammar_state: Cow<'static, str>,
    },
    /// The EXI stream ended before a complete structure was decoded (Spec 6).
    PrematureEndOfStream,
    /// A float value exceeds the representable range (Spec 7.1.4 MUST NOT).
    FloatOutOfRange,
    /// A namespace prefix could not be resolved to a URI (Spec 7.1.7).
    UnresolvablePrefix,
    /// A namespace URI does not match the expected value (Spec 7.1.7).
    UriMismatch,
    /// An invalid combination of EXI options was specified (Spec 5.4).
    InvalidOptionCombination,
    /// A datatype representation is not supported (Spec 7.4).
    UnsupportedDatatypeRepresentation(String),
    /// The encoded data violates the schema constraints (Spec 8.5).
    SchemaViolation(Cow<'static, str>),
    /// A Particle has invalid occurs constraints: max < min (Spec 8.5.4.1.5).
    InvalidParticleOccurs { min: usize, max: usize },
    /// A WildcardConstraint::Namespaces has an empty namespace list (Spec 8.5.4.1.7).
    EmptyNamespaceList,
    /// Events appear in an order that violates the grammar (Spec 8).
    OrderingViolation {
        /// Was erwartet wurde (leer wenn nicht verfügbar).
        expected: Cow<'static, str>,
        /// Was gefunden wurde (leer wenn nicht verfügbar).
        found: Cow<'static, str>,
    },
    /// An integer value exceeds the representable range (Spec 7.1.5, 7.1.6).
    IntegerOverflow,
    /// A Unicode code point is invalid: surrogate (U+D800..U+DFFF) or > U+10FFFF (Spec 7.1.10).
    InvalidCodePoint(u64),
    /// A QName prefix index could not be resolved (Spec 7.1.7).
    UnresolvedPrefix(u64),
    /// An enumeration index exceeds the valid range (Spec 7.2).
    InvalidEnumerationIndex { index: usize, enum_count: usize },
    /// A list length exceeds the maximum allowed size (Spec 7.1.11).
    ListLengthOverflow(u64),
    /// A compact identifier is invalid or was evicted (Spec 7.3).
    InvalidCompactId(usize),
    /// The EXI distinguishing bits are not `10` (Spec 5.2).
    InvalidDistinguishingBits(u8),
    /// An unknown NonTerminal was requested (Spec 8.4).
    UnknownNonTerminal(String),
    /// Self-Contained (SC) processing is not yet supported (Spec 8.4.3).
    ///
    /// SC erfordert State-Management (String Table, Grammars speichern/wiederherstellen),
    /// Byte-Alignment und Fragment Grammar Integration. Diese Features werden in einem
    /// späteren Issue implementiert.
    UnsupportedSelfContained,
    /// A Production is missing its event code (internal invariant violation).
    ///
    /// Event codes must be assigned via `recalculate_event_codes()` before
    /// encoding/decoding. This error indicates a bug in the grammar construction.
    MissingEventCode,
    /// XSD parsing failed.
    XsdParseError(String),
    /// XML parsing failed.
    XmlParseError(String),
    /// Block size must be greater than zero (Spec 9.1).
    InvalidBlockSize,
    /// DEFLATE compression failed.
    ///
    /// EXI Spec 9.3: "Each compressed stream in a block is stored using the
    /// standard DEFLATE Compressed Data Format defined by RFC 1951."
    CompressionError(String),
    /// DEFLATE decompression failed.
    ///
    /// EXI Spec 9.3: "Each compressed stream in a block is stored using the
    /// standard DEFLATE Compressed Data Format defined by RFC 1951."
    DecompressionError(String),
    /// A typed value could not be parsed or is invalid (Spec 7.1).
    InvalidValue(String),
    /// A decoded string exceeds the configured maximum length.
    ///
    /// Ausgelöst wenn `value_max_length` in den EXI Options gesetzt ist und ein
    /// decodierter String diese Grenze überschreitet.
    StringLengthExceeded { length: u64, max: u32 },
    /// Decoder made no progress (internal guard against infinite loops).
    DecoderStalled,
    /// Content after xsi:nil="true" is not empty (Spec 8.5.4.4.2).
    ///
    /// Nach xsi:nil="true" darf nur EE (EndElement) folgen.
    XsiNilContentNotEmpty,
    /// Schema-informed Compression is not yet supported (Spec 9, 8.5).
    ///
    /// Schema-informed Encoding mit Compression erfordert Typed Value Encoding
    /// in Value Channels. Dieses Feature wird in einem späteren Issue implementiert.
    SchemaCompressionNotSupported,
    /// xsi:type references a type not found in the schema (Spec 8.5.4.4).
    ///
    /// Bei strict=true muss der xsi:type Wert auf einen im Schema definierten
    /// Typ verweisen.
    XsiTypeNotFound(String),
    /// xsi:type and xsi:nil cannot be used together on the same element (Spec 8.5.4.4.2).
    ///
    /// "It is not possible to use xsi:type and xsi:nil attributes together on the same element.
    /// This is due to the fact that xsi:type specifies a target type definition, but xsi:nil
    /// is only permitted on nillable elements, not type definitions."
    XsiTypeAndNilTogether,
    /// DOCTYPE erfordert die Batch-API (Streaming kann DTD-Entities nicht auflösen).
    DtdRequiresBatchApi,
    /// Ein IO-Fehler beim Schreiben des EXI-Streams.
    IoError(String),
    /// Speicherlimit erreicht: RSS >= 95% des physischen RAM.
    ///
    /// Wird vom MemoryMonitor ausgeloest, bevor Linux durch Swap-Thrashing
    /// unbrauchbar wird.
    MemoryLimitExceeded { rss_bytes: u64, total_bytes: u64 },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnsupportedVersion => write!(f, "unsupported EXI version (Spec 5.3)"),
            Self::MalformedHeader => write!(f, "malformed EXI header (Spec 5)"),
            Self::InvalidEventCode { event_code, grammar_state } => {
                if event_code.is_empty() && grammar_state.is_empty() {
                    write!(f, "invalid event code (Spec 6.1, 6.2)")
                } else if grammar_state.is_empty() {
                    write!(f, "invalid event code '{event_code}' (Spec 6.1, 6.2)")
                } else {
                    write!(f, "invalid event code '{event_code}' in state '{grammar_state}' (Spec 6.1, 6.2)")
                }
            }
            Self::PrematureEndOfStream => write!(f, "premature end of EXI stream (Spec 6)"),
            Self::FloatOutOfRange => write!(f, "float value out of range (Spec 7.1.4)"),
            Self::UnresolvablePrefix => write!(f, "unresolvable namespace prefix (Spec 7.1.7)"),
            Self::UriMismatch => write!(f, "namespace URI mismatch (Spec 7.1.7)"),
            Self::InvalidOptionCombination => write!(f, "invalid EXI option combination (Spec 5.4)"),
            Self::UnsupportedDatatypeRepresentation(qname) => write!(f, "unsupported datatype representation '{qname}' (Spec 7.4)"),
            Self::SchemaViolation(msg) => {
                if msg.is_empty() {
                    write!(f, "schema violation (Spec 8.5)")
                } else {
                    write!(f, "schema violation: {msg} (Spec 8.5)")
                }
            }
            Self::InvalidParticleOccurs { min, max } => write!(f, "invalid particle occurs: max {max} < min {min} (Spec 8.5.4.1.5)"),
            Self::EmptyNamespaceList => write!(f, "empty namespace list in WildcardConstraint (Spec 8.5.4.1.7)"),
            Self::OrderingViolation { expected, found } => {
                if expected.is_empty() && found.is_empty() {
                    write!(f, "event ordering violation (Spec 8)")
                } else {
                    write!(f, "event ordering violation: expected '{expected}', found '{found}' (Spec 8)")
                }
            }
            Self::IntegerOverflow => write!(f, "integer overflow (Spec 7.1.5, 7.1.6)"),
            Self::InvalidCodePoint(cp) => write!(f, "invalid Unicode code point U+{cp:X} (Spec 7.1.10)"),
            Self::UnresolvedPrefix(idx) => write!(f, "unresolved QName prefix index {idx} (Spec 7.1.7)"),
            Self::InvalidEnumerationIndex { index, enum_count } => write!(f, "enum index {index} exceeds valid range 0..{enum_count} (Spec 7.2)"),
            Self::ListLengthOverflow(len) => write!(f, "list length {len} exceeds max allowed size (Spec 7.1.11)"),
            Self::InvalidCompactId(id) => write!(f, "invalid or evicted compact identifier {id} (Spec 7.3)"),
            Self::InvalidDistinguishingBits(bits) => write!(f, "invalid distinguishing bits {bits:02b}, expected 10 (Spec 5.2)"),
            Self::UnknownNonTerminal(id) => write!(f, "unknown NonTerminal '{id}' requested (Spec 8.4)"),
            Self::UnsupportedSelfContained => write!(f, "self-contained (SC) processing not yet supported (Spec 8.4.3)"),
            Self::MissingEventCode => write!(f, "production missing event code - call recalculate_event_codes() first"),
            Self::XsdParseError(msg) => write!(f, "XSD parse error: {msg}"),
            Self::XmlParseError(msg) => write!(f, "XML parse error: {msg}"),
            Self::InvalidBlockSize => write!(f, "block size must be greater than zero (Spec 9.1)"),
            Self::CompressionError(msg) => write!(f, "DEFLATE compression failed (RFC 1951, Spec 9.3): {msg}"),
            Self::DecompressionError(msg) => write!(f, "DEFLATE decompression failed (RFC 1951, Spec 9.3): {msg}"),
            Self::InvalidValue(msg) => write!(f, "invalid typed value (Spec 7.1): {msg}"),
            Self::StringLengthExceeded { length, max } => write!(f, "string length {length} exceeds maximum {max}"),
            Self::DecoderStalled => write!(f, "decoder stalled (no progress)"),
            Self::XsiNilContentNotEmpty => write!(f, "content after xsi:nil=\"true\" is not empty (Spec 8.5.4.4.2)"),
            Self::SchemaCompressionNotSupported => write!(f, "schema-informed compression not yet supported (Spec 9, 8.5)"),
            Self::XsiTypeNotFound(type_name) => write!(f, "xsi:type '{type_name}' not found in schema (Spec 8.5.4.4)"),
            Self::XsiTypeAndNilTogether => write!(f, "xsi:type and xsi:nil cannot be used together on the same element (Spec 8.5.4.4.2)"),
            Self::DtdRequiresBatchApi => write!(f, "DOCTYPE erfordert Batch-API (Streaming kann DTD-Entities nicht aufloesen)"),
            Self::IoError(msg) => write!(f, "IO error: {msg}"),
            Self::MemoryLimitExceeded { rss_bytes, total_bytes } => {
                let rss_gb = *rss_bytes as f64 / (1024.0 * 1024.0 * 1024.0);
                let total_gb = *total_bytes as f64 / (1024.0 * 1024.0 * 1024.0);
                write!(
                    f,
                    "Speicherlimit: {rss_gb:.1}/{total_gb:.1} GB (>=95%). \
                     Tipp: --value-capacity oder --value-max-length"
                )
            }
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    /// Erstellt einen `InvalidEventCode` Fehler mit Kontext.
    pub fn invalid_event_code(event_code: impl Into<Cow<'static, str>>, grammar_state: impl Into<Cow<'static, str>>) -> Self {
        Self::InvalidEventCode {
            event_code: event_code.into(),
            grammar_state: grammar_state.into(),
        }
    }

    /// Erstellt einen `OrderingViolation` Fehler mit Kontext.
    pub fn ordering_violation(expected: impl Into<Cow<'static, str>>, found: impl Into<Cow<'static, str>>) -> Self {
        Self::OrderingViolation {
            expected: expected.into(),
            found: found.into(),
        }
    }

    /// Erstellt einen `SchemaViolation` Fehler mit Nachricht.
    pub fn schema_violation(msg: impl Into<Cow<'static, str>>) -> Self {
        Self::SchemaViolation(msg.into())
    }
}

/// A convenience `Result` type alias using [`Error`].
pub type Result<T> = core::result::Result<T, Error>;

#[cfg(test)]
mod tests {
    use super::*;

    /// Every error variant must be constructable and produce a non-empty Display string
    /// that contains the spec section reference.
    /// Spec: diverse MUST/MUST NOT rules across the specification.

    #[test]
    fn unsupported_version_display() {
        let e = Error::UnsupportedVersion;
        let msg = e.to_string();
        assert!(msg.contains("version"), "{msg}");
        assert!(msg.contains("5.3"), "{msg}");
    }

    #[test]
    fn malformed_header_display() {
        let e = Error::MalformedHeader;
        let msg = e.to_string();
        assert!(msg.contains("header"), "{msg}");
        assert!(msg.contains("Spec 5"), "{msg}");
    }

    #[test]
    fn invalid_event_code_display() {
        let e = Error::invalid_event_code("", "");
        let msg = e.to_string();
        assert!(msg.contains("event code"), "{msg}");
        assert!(msg.contains("6.1"), "{msg}");
    }

    #[test]
    fn invalid_event_code_with_context_display() {
        let e = Error::invalid_event_code("1.0", "ElementContent");
        let msg = e.to_string();
        assert!(msg.contains("1.0"), "{msg}");
        assert!(msg.contains("ElementContent"), "{msg}");
        assert!(msg.contains("6.1"), "{msg}");
    }

    #[test]
    fn premature_end_of_stream_display() {
        let e = Error::PrematureEndOfStream;
        let msg = e.to_string();
        assert!(msg.contains("premature"), "{msg}");
        assert!(msg.contains("Spec 6"), "{msg}");
    }

    #[test]
    fn float_out_of_range_display() {
        let e = Error::FloatOutOfRange;
        let msg = e.to_string();
        assert!(msg.contains("float"), "{msg}");
        assert!(msg.contains("7.1.4"), "{msg}");
    }

    #[test]
    fn unresolvable_prefix_display() {
        let e = Error::UnresolvablePrefix;
        let msg = e.to_string();
        assert!(msg.contains("prefix"), "{msg}");
        assert!(msg.contains("7.1.7"), "{msg}");
    }

    #[test]
    fn uri_mismatch_display() {
        let e = Error::UriMismatch;
        let msg = e.to_string();
        assert!(msg.contains("URI"), "{msg}");
        assert!(msg.contains("7.1.7"), "{msg}");
    }

    #[test]
    fn invalid_option_combination_display() {
        let e = Error::InvalidOptionCombination;
        let msg = e.to_string();
        assert!(msg.contains("option"), "{msg}");
        assert!(msg.contains("5.4"), "{msg}");
    }

    #[test]
    fn unsupported_datatype_representation_display() {
        let e = Error::UnsupportedDatatypeRepresentation("{http://example.org}myCodec".to_string());
        let msg = e.to_string();
        assert!(msg.contains("datatype"), "{msg}");
        assert!(msg.contains("7.4"), "{msg}");
        assert!(msg.contains("myCodec"), "{msg}");
    }

    #[test]
    fn schema_violation_display() {
        let e = Error::schema_violation("");
        let msg = e.to_string();
        assert!(msg.contains("schema"), "{msg}");
        assert!(msg.contains("8.5"), "{msg}");
    }

    #[test]
    fn schema_violation_with_msg_display() {
        let e = Error::schema_violation("unexpected element");
        let msg = e.to_string();
        assert!(msg.contains("unexpected element"), "{msg}");
        assert!(msg.contains("8.5"), "{msg}");
    }

    #[test]
    fn ordering_violation_display() {
        let e = Error::ordering_violation("", "");
        let msg = e.to_string();
        assert!(msg.contains("ordering"), "{msg}");
        assert!(msg.contains("Spec 8"), "{msg}");
    }

    #[test]
    fn ordering_violation_with_context_display() {
        let e = Error::ordering_violation("AT", "CH");
        let msg = e.to_string();
        assert!(msg.contains("AT"), "{msg}");
        assert!(msg.contains("CH"), "{msg}");
        assert!(msg.contains("Spec 8"), "{msg}");
    }

    #[test]
    fn integer_overflow_display() {
        let e = Error::IntegerOverflow;
        let msg = e.to_string();
        assert!(msg.contains("overflow"), "{msg}");
        assert!(msg.contains("7.1.5"), "{msg}");
        assert!(msg.contains("7.1.6"), "{msg}");
    }

    #[test]
    fn invalid_code_point_display() {
        let e = Error::InvalidCodePoint(0xD800);
        let msg = e.to_string();
        assert!(msg.contains("code point"), "{msg}");
        assert!(msg.contains("7.1.10"), "{msg}");
        assert!(msg.contains("D800"), "{msg}");
    }

    #[test]
    fn unresolved_prefix_display() {
        let e = Error::UnresolvedPrefix(42);
        let msg = e.to_string();
        assert!(msg.contains("prefix"), "{msg}");
        assert!(msg.contains("7.1.7"), "{msg}");
        assert!(msg.contains("42"), "{msg}");
    }

    #[test]
    fn error_implements_std_error() {
        let e: Box<dyn std::error::Error> = Box::new(Error::UnsupportedVersion);
        assert!(!e.to_string().is_empty());
    }

    #[test]
    fn error_is_clone_and_eq() {
        let e1 = Error::FloatOutOfRange;
        let e2 = e1.clone();
        assert_eq!(e1, e2);
    }

    #[test]
    fn error_debug_format() {
        let e = Error::UnsupportedVersion;
        let debug = format!("{e:?}");
        assert!(debug.contains("UnsupportedVersion"), "{debug}");
    }

    #[test]
    fn result_type_alias_works() {
        let ok: Result<u32> = Ok(42);
        assert_eq!(ok.unwrap(), 42);

        let err: Result<u32> = Err(Error::MalformedHeader);
        assert!(err.is_err());
    }

    #[test]
    fn invalid_enumeration_index_display() {
        let e = Error::InvalidEnumerationIndex {
            index: 5,
            enum_count: 3,
        };
        let msg = e.to_string();
        assert!(msg.contains("enum"), "{msg}");
        assert!(msg.contains("5"), "{msg}");
        assert!(msg.contains("3"), "{msg}");
        assert!(msg.contains("7.2"), "{msg}");
    }

    #[test]
    fn list_length_overflow_display() {
        let e = Error::ListLengthOverflow(999_999_999);
        let msg = e.to_string();
        assert!(msg.contains("list"), "{msg}");
        assert!(msg.contains("999999999"), "{msg}");
        assert!(msg.contains("7.1.11"), "{msg}");
    }

    /// Spec 7.3: InvalidCompactId für ungültige oder evicted IDs
    #[test]
    fn invalid_compact_id_display() {
        let e = Error::InvalidCompactId(42);
        let msg = e.to_string();
        assert!(msg.contains("compact"), "{msg}");
        assert!(msg.contains("42"), "{msg}");
        assert!(msg.contains("7.3"), "{msg}");
    }

    /// Spec 5.2: InvalidDistinguishingBits für ungültige Distinguishing Bits
    #[test]
    fn invalid_distinguishing_bits_display() {
        let e = Error::InvalidDistinguishingBits(0b01);
        let msg = e.to_string();
        assert!(msg.contains("distinguishing"), "{msg}");
        assert!(msg.contains("10"), "{msg}");
        assert!(msg.contains("5.2"), "{msg}");
    }

    /// Spec 8.4: UnknownNonTerminal für unbekannte NonTerminal-IDs
    #[test]
    fn unknown_non_terminal_display() {
        let e = Error::UnknownNonTerminal("DocContent".to_string());
        let msg = e.to_string();
        assert!(msg.contains("NonTerminal"), "{msg}");
        assert!(msg.contains("DocContent"), "{msg}");
        assert!(msg.contains("8.4"), "{msg}");
    }

    /// Spec 8.4.3: UnsupportedSelfContained für SC-Verarbeitung
    #[test]
    fn unsupported_self_contained_display() {
        let e = Error::UnsupportedSelfContained;
        let msg = e.to_string();
        assert!(msg.contains("self-contained"), "{msg}");
        assert!(msg.contains("SC"), "{msg}");
        assert!(msg.contains("8.4.3"), "{msg}");
    }

    /// Spec 8.5.4.1.5: InvalidParticleOccurs für max < min
    #[test]
    fn invalid_particle_occurs_display() {
        let e = Error::InvalidParticleOccurs { min: 5, max: 2 };
        let msg = e.to_string();
        assert!(msg.contains("particle"), "{msg}");
        assert!(msg.contains("5"), "{msg}");
        assert!(msg.contains("2"), "{msg}");
        assert!(msg.contains("8.5.4.1.5"), "{msg}");
    }

    /// Spec 8.5.4.1.7: EmptyNamespaceList für leere Namespace-Liste
    #[test]
    fn empty_namespace_list_display() {
        let e = Error::EmptyNamespaceList;
        let msg = e.to_string();
        assert!(msg.contains("namespace"), "{msg}");
        assert!(msg.contains("empty"), "{msg}");
        assert!(msg.contains("8.5.4.1.7"), "{msg}");
    }

    /// MissingEventCode für Productions ohne zugewiesenen Event Code
    #[test]
    fn missing_event_code_display() {
        let e = Error::MissingEventCode;
        let msg = e.to_string();
        assert!(msg.contains("event code"), "{msg}");
        assert!(msg.contains("recalculate_event_codes"), "{msg}");
    }

    /// XsdParseError für XSD-Parsing-Fehler
    #[test]
    fn xsd_parse_error_display() {
        let e = Error::XsdParseError("missing required attribute".to_string());
        let msg = e.to_string();
        assert!(msg.contains("XSD"), "{msg}");
        assert!(msg.contains("missing required attribute"), "{msg}");
    }

    /// Spec 9.1: InvalidBlockSize für block_size=0
    #[test]
    fn invalid_block_size_display() {
        let e = Error::InvalidBlockSize;
        let msg = e.to_string();
        assert!(msg.contains("block size"), "{msg}");
        assert!(msg.contains("9.1"), "{msg}");
    }

    /// Spec 9.3: CompressionError für DEFLATE-Komprimierungsfehler
    #[test]
    fn compression_error_display() {
        let e = Error::CompressionError("write failed".to_string());
        let msg = e.to_string();
        assert!(msg.contains("DEFLATE"), "{msg}");
        assert!(msg.contains("compression"), "{msg}");
        assert!(msg.contains("write failed"), "{msg}");
        assert!(msg.contains("9.3"), "{msg}");
    }

    /// Spec 9.3: DecompressionError für DEFLATE-Dekomprimierungsfehler
    #[test]
    fn decompression_error_display() {
        let e = Error::DecompressionError("invalid block type".to_string());
        let msg = e.to_string();
        assert!(msg.contains("DEFLATE"), "{msg}");
        assert!(msg.contains("decompression"), "{msg}");
        assert!(msg.contains("invalid block type"), "{msg}");
        assert!(msg.contains("9.3"), "{msg}");
    }

    /// Spec 8.5.4.4.2: XsiNilContentNotEmpty
    #[test]
    fn xsi_nil_content_not_empty_display() {
        let e = Error::XsiNilContentNotEmpty;
        let msg = e.to_string();
        assert!(msg.contains("xsi:nil"), "{msg}");
        assert!(msg.contains("8.5.4.4.2"), "{msg}");
    }

    /// Spec 9, 8.5: SchemaCompressionNotSupported
    #[test]
    fn schema_compression_not_supported_display() {
        let e = Error::SchemaCompressionNotSupported;
        let msg = e.to_string();
        assert!(msg.contains("schema"), "{msg}");
        assert!(msg.contains("compression"), "{msg}");
        assert!(msg.contains("Spec 9"), "{msg}");
    }

    /// Spec 8.5.4.4: XsiTypeNotFound
    #[test]
    fn xsi_type_not_found_display() {
        let e = Error::XsiTypeNotFound("MyCustomType".to_string());
        let msg = e.to_string();
        assert!(msg.contains("xsi:type"), "{msg}");
        assert!(msg.contains("MyCustomType"), "{msg}");
        assert!(msg.contains("8.5.4.4"), "{msg}");
    }

    /// Spec 8.5.4.4.2: XsiTypeAndNilTogether
    #[test]
    fn xsi_type_and_nil_together_display() {
        let e = Error::XsiTypeAndNilTogether;
        let msg = e.to_string();
        assert!(msg.contains("xsi:type"), "{msg}");
        assert!(msg.contains("xsi:nil"), "{msg}");
        assert!(msg.contains("8.5.4.4.2"), "{msg}");
    }

    #[test]
    fn dtd_requires_batch_api_display() {
        let e = Error::DtdRequiresBatchApi;
        let msg = e.to_string();
        assert!(msg.contains("DOCTYPE"), "{msg}");
        assert!(msg.contains("Batch"), "{msg}");
    }

    #[test]
    fn io_error_display() {
        let e = Error::IoError("disk full".to_string());
        let msg = e.to_string();
        assert!(msg.contains("IO"), "{msg}");
        assert!(msg.contains("disk full"), "{msg}");
    }

    /// Spec 7.1: InvalidValue für ungültige typisierte Werte
    #[test]
    fn invalid_value_display() {
        let e = Error::InvalidValue("not a number".to_string());
        let msg = e.to_string();
        assert!(msg.contains("invalid"), "{msg}");
        assert!(msg.contains("typed value"), "{msg}");
        assert!(msg.contains("not a number"), "{msg}");
        assert!(msg.contains("7.1"), "{msg}");
    }

    /// DoS-Schutz: StringLengthExceeded
    #[test]
    fn string_length_exceeded_display() {
        let e = Error::StringLengthExceeded { length: 1_000_000, max: 1024 };
        let msg = e.to_string();
        assert!(msg.contains("1000000"), "{msg}");
        assert!(msg.contains("1024"), "{msg}");
    }
}
