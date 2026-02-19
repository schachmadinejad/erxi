//! erxi – EXI 1.0 (W3C Second Edition) Rust library
//!
//! # Beispiel
//!
//! ```
//! use std::rc::Rc;
//! use erxi::{ExiEvent, ExiOptions, QName};
//! use erxi::encoder::encode;
//! use erxi::decoder::decode;
//!
//! // Encode
//! let events = vec![
//!     ExiEvent::StartDocument,
//!     ExiEvent::StartElement(Rc::new(QName::new("", "greeting"))),
//!     ExiEvent::Characters(erxi::ChContent { value: "Hello".into() }),
//!     ExiEvent::EndElement,
//!     ExiEvent::EndDocument,
//! ];
//! let bytes = encode(&events, &ExiOptions::default()).unwrap();
//!
//! // Decode
//! let (decoded, _opts) = decode(&bytes).unwrap();
//! assert_eq!(decoded.len(), 5);
//! ```

pub mod binary;
pub mod bit_width;
pub mod bitstream;
pub mod boolean;
pub mod compression;
pub mod datetime;
pub mod decimal;
pub mod decoder;
pub mod encoder;
pub mod enumeration;
pub mod error;
pub mod event;
pub mod event_code;
pub mod event_content;
pub mod float;
pub mod grammar;
pub mod header;
pub mod integer;
pub mod list;
pub mod media_type;
pub mod memory_monitor;
pub mod n_bit_unsigned_integer;
pub mod options;
pub mod options_codec;
pub mod precompression;
pub mod proto_grammar;
pub mod qname;
pub mod rcs;
pub mod schema;
pub mod streaming;
pub mod string;
pub mod string_table;
pub mod typed_value;
pub mod undeclared;
pub mod unsigned_integer;
pub mod xml;
pub mod xml_serializer;
pub mod xsd;

pub use error::{Error, Result};

/// HashMap mit ahash (schneller, nicht DoS-resistent — für interne Datenstrukturen).
/// Nutzt hashbrown direkt für entry_ref() und raw_entry API.
pub(crate) type FastHashMap<K, V> = hashbrown::HashMap<K, V, ahash::RandomState>;

/// HashSet mit ahash.
pub(crate) type FastHashSet<K> = hashbrown::HashSet<K, ahash::RandomState>;

/// IndexMap mit ahash (deterministische Iteration + schnelles Hashing).
pub(crate) type FastIndexMap<K, V> = indexmap::IndexMap<K, V, ahash::RandomState>;

// Public API: Events
pub use event::{AtContent, ChContent, DtContent, ErContent, ExiEvent, NsContent};

// Public API: Options
pub use options::{Alignment, ExiOptions, Preserve, SchemaId};

// Public API: Header
pub use header::ExiHeader;

// Public API: Encoder/Decoder
pub use decoder::{
    decode, decode_with_options, decode_with_schema,
    decode_iter_with_options, decode_iter_with_schema,
    probe_compression, DecodeIter,
};
pub use encoder::{EncoderConfig, encode_with_schema, encode_with_schema_and_config};

// Public API: Types
pub use qname::QName;
pub use schema::SchemaInfo;
pub use datetime::DateTime;
pub use typed_value::{
    get_user_defined_representation, register_user_defined_representation,
    unregister_user_defined_representation, UserDefinedDecode, UserDefinedEncode,
};

// Public API: XSD/XML
pub use xsd::parse_xsd_with_imports;
pub use xml::{parse_xml_events, parse_xml_events_from_str, parse_xml_events_with_options};
pub use xml_serializer::{
    events_to_xml, events_to_xml_iter, events_to_xml_iter_fallible, events_to_xml_writer,
};

// Public API: Streaming
pub use memory_monitor::MemoryMonitor;
pub use streaming::{encode_xml_stream, encode_xml_file};
