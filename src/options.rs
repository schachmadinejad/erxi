//! EXI Options data model (Spec 5.4, Table 5-1).
//!
//! Represents all options that control how an EXI stream is encoded/decoded.
//!
//! # Beispiel
//!
//! ```
//! use erxi::options::{Alignment, ExiOptions, Preserve};
//!
//! let opts = ExiOptions::default()
//!     .with_alignment(Alignment::ByteAlignment)
//!     .with_preserve(Preserve { comments: true, ..Preserve::default() })
//!     .with_value_max_length(1024);
//!
//! assert_eq!(opts.alignment(), Alignment::ByteAlignment);
//! assert!(opts.preserve().comments);
//! assert_eq!(opts.value_max_length(), Some(1024));
//! ```

use crate::qname::QName;
use crate::{Error, Result};

/// Alignment of event codes and content items (Spec 5.4).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Alignment {
    /// Event codes and content are packed in bits without padding (default).
    #[default]
    BitPacked,
    /// Event codes and content are aligned on byte boundaries.
    ByteAlignment,
    /// All compression steps except DEFLATE are applied.
    PreCompression,
}

/// Schema identification for the EXI body (Spec 5.4).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SchemaId {
    /// No schema information is used (xsi:nil="true").
    None,
    /// No user-defined schema; only built-in XML Schema types available.
    BuiltinOnly,
    /// A user-defined schema identifier.
    Id(String),
}

/// Fidelity options controlling preservation of information items (Spec 5.4, 6.3).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Preserve {
    /// CM events can be preserved.
    pub comments: bool,
    /// PI events can be preserved.
    pub pis: bool,
    /// DT and ER events can be preserved.
    pub dtd: bool,
    /// NS events and namespace prefixes can be preserved.
    pub prefixes: bool,
    /// Lexical form of element and attribute values can be preserved.
    pub lexical_values: bool,
    /// Insignifikanten Whitespace beibehalten (Default: false = wird gestrippt).
    ///
    /// Lokales Steuerungsfeld -- nicht Teil der EXI-Spezifikation und wird
    /// nicht im EXI-Header codiert. `lexical_values` impliziert ebenfalls
    /// Whitespace-Erhalt.
    pub whitespace: bool,
}

impl Preserve {
    /// Prueft ob Header-relevante Preserve-Flags gesetzt sind.
    ///
    /// `whitespace` ist kein EXI-Spec-Feld und wird nicht im Header codiert.
    pub fn has_header_relevant_flags(&self) -> bool {
        self.comments || self.pis || self.dtd || self.prefixes || self.lexical_values
    }

    /// Prueft ob Whitespace erhalten werden soll.
    ///
    /// Wahr wenn `whitespace` explizit gesetzt ist oder `lexical_values`
    /// aktiviert ist (impliziert WS-Erhalt).
    pub fn preserves_whitespace(&self) -> bool {
        self.whitespace || self.lexical_values
    }
}

/// A datatype representation mapping entry (Spec 5.4, 7.4).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DatatypeRepresentationMapping {
    /// QName des XML Schema Datentyps (z.B. {XSD}decimal).
    pub type_qname: QName,
    /// QName der Representation (z.B. {EXI}string).
    pub representation_qname: QName,
}

/// EXI Options controlling stream encoding/decoding (Spec 5.4, Table 5-1).
#[derive(Debug, Clone, PartialEq)]
pub struct ExiOptions {
    pub(crate) alignment: Alignment,
    pub(crate) compression: bool,
    pub(crate) strict: bool,
    pub(crate) fragment: bool,
    pub(crate) preserve: Preserve,
    pub(crate) self_contained: bool,
    pub(crate) self_contained_qnames: Vec<QName>,
    pub(crate) schema_id: Option<SchemaId>,
    pub(crate) datatype_representation_map: Vec<DatatypeRepresentationMapping>,
    pub(crate) block_size: u32,
    pub(crate) value_max_length: Option<u32>,
    pub(crate) value_partition_capacity: Option<u32>,
}

impl Default for ExiOptions {
    /// Creates ExiOptions with all default values as specified in Table 5-1.
    fn default() -> Self {
        Self {
            alignment: Alignment::BitPacked,
            compression: false,
            strict: false,
            fragment: false,
            preserve: Preserve::default(),
            self_contained: false,
            self_contained_qnames: Vec::new(),
            schema_id: None,
            datatype_representation_map: Vec::new(),
            block_size: 1_000_000,
            value_max_length: None,
            value_partition_capacity: None,
        }
    }
}

impl ExiOptions {
    // --- Getter ---

    /// Alignment of event codes and content items (Spec 5.4).
    pub fn alignment(&self) -> Alignment { self.alignment }
    /// EXI compression is used (Spec 5.4).
    pub fn compression(&self) -> bool { self.compression }
    /// Strict interpretation of schemas (Spec 5.4).
    pub fn strict(&self) -> bool { self.strict }
    /// Body is encoded as an EXI fragment (Spec 5.4).
    pub fn fragment(&self) -> bool { self.fragment }
    /// Fidelity options for preservation (Spec 5.4, 6.3).
    pub fn preserve(&self) -> &Preserve { &self.preserve }
    /// Self-contained elements enabled (Spec 5.4).
    pub fn self_contained(&self) -> bool { self.self_contained }
    /// QNames der SC-gewrappten Elemente.
    pub fn self_contained_qnames(&self) -> &[QName] { &self.self_contained_qnames }
    /// Schema identification for the EXI body (Spec 5.4).
    pub fn schema_id(&self) -> Option<&SchemaId> { self.schema_id.as_ref() }
    /// Alternate datatype representations (Spec 5.4, 7.4).
    pub fn datatype_representation_map(&self) -> &[DatatypeRepresentationMapping] { &self.datatype_representation_map }
    /// Block size for EXI compression (Spec 9.1).
    pub fn block_size(&self) -> u32 { self.block_size }
    /// Maximum string length for string table addition.
    pub fn value_max_length(&self) -> Option<u32> { self.value_max_length }
    /// Total capacity of value partitions in string table.
    pub fn value_partition_capacity(&self) -> Option<u32> { self.value_partition_capacity }

    // --- Builder-Setter (Fluent API) ---

    /// Setzt das Alignment.
    pub fn with_alignment(mut self, alignment: Alignment) -> Self { self.alignment = alignment; self }
    /// Aktiviert Compression.
    pub fn with_compression(mut self) -> Self { self.compression = true; self }
    /// Aktiviert Strict-Modus.
    pub fn with_strict(mut self) -> Self { self.strict = true; self }
    /// Aktiviert Fragment-Modus.
    pub fn with_fragment(mut self) -> Self { self.fragment = true; self }
    /// Setzt die Preserve-Optionen.
    pub fn with_preserve(mut self, preserve: Preserve) -> Self { self.preserve = preserve; self }
    /// Aktiviert Self-Contained.
    pub fn with_self_contained(mut self) -> Self { self.self_contained = true; self }
    /// Setzt die SC-QNames.
    pub fn with_self_contained_qnames(mut self, qnames: Vec<QName>) -> Self { self.self_contained_qnames = qnames; self }
    /// Setzt die Schema-ID.
    pub fn with_schema_id(mut self, schema_id: SchemaId) -> Self { self.schema_id = Some(schema_id); self }
    /// Setzt die DatatypeRepresentationMap.
    pub fn with_datatype_representation_map(mut self, map: Vec<DatatypeRepresentationMapping>) -> Self { self.datatype_representation_map = map; self }
    /// Setzt die Block-Groesse.
    pub fn with_block_size(mut self, size: u32) -> Self { self.block_size = size; self }
    /// Setzt das Value-Max-Length-Limit.
    pub fn with_value_max_length(mut self, len: u32) -> Self { self.value_max_length = Some(len); self }
    /// Setzt die Value-Partition-Capacity.
    pub fn with_value_partition_capacity(mut self, cap: u32) -> Self { self.value_partition_capacity = Some(cap); self }

    // --- Mutable Setter (fuer nachtraegliche Aenderungen) ---

    /// Setzt das Alignment.
    pub fn set_alignment(&mut self, alignment: Alignment) { self.alignment = alignment; }
    /// Setzt Compression.
    pub fn set_compression(&mut self, val: bool) { self.compression = val; }
    /// Setzt Strict-Modus.
    pub fn set_strict(&mut self, val: bool) { self.strict = val; }
    /// Setzt Fragment-Modus.
    pub fn set_fragment(&mut self, val: bool) { self.fragment = val; }
    /// Setzt die Preserve-Optionen.
    pub fn set_preserve(&mut self, preserve: Preserve) { self.preserve = preserve; }
    /// Setzt Self-Contained.
    pub fn set_self_contained(&mut self, val: bool) { self.self_contained = val; }
    /// Setzt die SC-QNames.
    pub fn set_self_contained_qnames(&mut self, qnames: Vec<QName>) { self.self_contained_qnames = qnames; }
    /// Setzt die Schema-ID.
    pub fn set_schema_id(&mut self, schema_id: Option<SchemaId>) { self.schema_id = schema_id; }
    /// Setzt die DatatypeRepresentationMap.
    pub fn set_datatype_representation_map(&mut self, map: Vec<DatatypeRepresentationMapping>) { self.datatype_representation_map = map; }
    /// Setzt die Block-Groesse.
    pub fn set_block_size(&mut self, size: u32) { self.block_size = size; }
    /// Setzt das Value-Max-Length-Limit.
    pub fn set_value_max_length(&mut self, len: Option<u32>) { self.value_max_length = len; }
    /// Setzt die Value-Partition-Capacity.
    pub fn set_value_partition_capacity(&mut self, cap: Option<u32>) { self.value_partition_capacity = cap; }

    /// Validates the option combination according to Spec 5.4 constraints.
    ///
    /// # Errors
    ///
    /// Returns `Error::InvalidOptionCombination` if:
    /// - `alignment` is `ByteAlignment` or `PreCompression` and `compression` is true (Spec 5.4)
    /// - `strict` is true and any of `preserve.comments`, `preserve.pis`,
    ///   `preserve.dtd`, `preserve.prefixes`, or `self_contained` is true (Spec 5.4)
    /// - `self_contained` is true and `compression` is true or `alignment` is
    ///   `PreCompression` or `strict` is true (Spec 5.4)
    pub fn validate(&self) -> Result<()> {
        // Spec 9.1: block_size muss > 0 sein (sonst Division-by-Zero bei Compression)
        if self.block_size == 0 {
            return Err(Error::InvalidBlockSize);
        }

        // SchemaId::Id("") ist ungueltig — leerer String wird beim Decoder
        // bereits zu BuiltinOnly normalisiert, aber manuell konstruierte
        // Options sollen abgelehnt werden.
        if let Some(SchemaId::Id(ref id)) = self.schema_id {
            if id.is_empty() {
                return Err(Error::InvalidOptionCombination);
            }
        }

        // Spec 5.4: "alignment" element MUST NOT appear when "compression" element is present
        // and vice versa
        if self.compression && self.alignment != Alignment::BitPacked {
            return Err(Error::InvalidOptionCombination);
        }

        // Spec 5.4: "strict" MUST NOT appear when one of "dtd", "prefixes", "comments", "pis"
        // or "selfContained" element is present
        let incompatible_with_strict = self.preserve.comments
            || self.preserve.pis
            || self.preserve.dtd
            || self.preserve.prefixes
            || self.self_contained;

        if self.strict && incompatible_with_strict {
            return Err(Error::InvalidOptionCombination);
        }

        // Spec 5.4: "selfContained" MUST NOT appear when one of "compression", "pre-compression"
        // or "strict" elements are present
        // Note: self.strict check is redundant here (already caught above), but kept for clarity
        let incompatible_with_self_contained =
            self.compression || self.alignment == Alignment::PreCompression;

        if self.self_contained && incompatible_with_self_contained {
            return Err(Error::InvalidOptionCombination);
        }

        Ok(())
    }

    /// Gibt das effektive Encoding-Alignment zurück (Spec 6.2, 9).
    ///
    /// Bei `compression=true` wird PreCompression-Style Encoding verwendet
    /// (byte-aligned Event Codes + Value Channels), nur dass am Ende
    /// DEFLATE angewendet wird.
    ///
    /// # Spec-Referenz
    /// - Spec 6.2: "When the value of compression option is true, or either
    ///   byte-alignment or pre-compression alignment option is used, n-bit
    ///   unsigned integers are represented using the minimum number of bytes
    ///   required to store n bits."
    pub fn effective_alignment(&self) -> Alignment {
        if self.compression {
            Alignment::PreCompression
        } else {
            self.alignment
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ==================== Default-Tests (Spec 5.4, Table 5-1) ====================

    /// Spec 5.4, Table 5-1: Default alignment is bit-packed.
    #[test]
    fn default_alignment_is_bit_packed() {
        let opts = ExiOptions::default();
        assert_eq!(opts.alignment, Alignment::BitPacked);
    }

    /// Spec 5.4, Table 5-1: Default compression is false.
    #[test]
    fn default_compression_is_false() {
        let opts = ExiOptions::default();
        assert!(!opts.compression);
    }

    /// Spec 5.4, Table 5-1: Default strict is false.
    #[test]
    fn default_strict_is_false() {
        let opts = ExiOptions::default();
        assert!(!opts.strict);
    }

    /// Spec 5.4, Table 5-1: Default fragment is false.
    #[test]
    fn default_fragment_is_false() {
        let opts = ExiOptions::default();
        assert!(!opts.fragment);
    }

    /// Spec 5.4, Table 5-1: Default preserve options are all false.
    #[test]
    fn default_preserve_all_false() {
        let opts = ExiOptions::default();
        assert!(!opts.preserve.comments);
        assert!(!opts.preserve.pis);
        assert!(!opts.preserve.dtd);
        assert!(!opts.preserve.prefixes);
        assert!(!opts.preserve.lexical_values);
    }

    /// Spec 5.4, Table 5-1: Default selfContained is false.
    #[test]
    fn default_self_contained_is_false() {
        let opts = ExiOptions::default();
        assert!(!opts.self_contained);
    }

    /// Spec 5.4, Table 5-1: Default schemaId has no default value (None).
    #[test]
    fn default_schema_id_is_none() {
        let opts = ExiOptions::default();
        assert!(opts.schema_id.is_none());
    }

    /// Spec 5.4, Table 5-1: Default datatypeRepresentationMap has no default value (empty).
    #[test]
    fn default_datatype_representation_map_is_empty() {
        let opts = ExiOptions::default();
        assert!(opts.datatype_representation_map.is_empty());
    }

    /// Spec 5.4, Table 5-1: Default blockSize is 1,000,000.
    #[test]
    fn default_block_size_is_one_million() {
        let opts = ExiOptions::default();
        assert_eq!(opts.block_size, 1_000_000);
    }

    /// Spec 5.4, Table 5-1: Default valueMaxLength is unbounded (None).
    #[test]
    fn default_value_max_length_is_unbounded() {
        let opts = ExiOptions::default();
        assert!(opts.value_max_length.is_none());
    }

    /// Spec 5.4, Table 5-1: Default valuePartitionCapacity is unbounded (None).
    #[test]
    fn default_value_partition_capacity_is_unbounded() {
        let opts = ExiOptions::default();
        assert!(opts.value_partition_capacity.is_none());
    }

    // ==================== Gültige Kombinationen ====================

    /// Spec 5.4: Default options are valid.
    #[test]
    fn default_options_are_valid() {
        let opts = ExiOptions::default();
        assert!(opts.validate().is_ok());
    }

    /// Spec 5.4: compression with default alignment (bit-packed) is valid.
    #[test]
    fn compression_with_default_alignment_is_valid() {
        let opts = ExiOptions {
            compression: true,

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
    }

    /// Spec 5.4: byte-alignment without compression is valid.
    #[test]
    fn byte_alignment_without_compression_is_valid() {
        let opts = ExiOptions {
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
    }

    /// Spec 5.4: pre-compression without compression is valid.
    #[test]
    fn pre_compression_without_compression_is_valid() {
        let opts = ExiOptions {
            alignment: Alignment::PreCompression,

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
    }

    /// Spec 5.4: strict with lexicalValues is valid (explicitly allowed).
    #[test]
    fn strict_with_lexical_values_is_valid() {
        let opts = ExiOptions {
            strict: true,

            preserve: Preserve {
                lexical_values: true,
                ..Default::default()
            },
            ..Default::default()
        };
        assert!(opts.validate().is_ok());
    }

    /// Spec 5.4: selfContained without compression/pre-compression/strict is valid.
    #[test]
    fn self_contained_alone_is_valid() {
        let opts = ExiOptions {
            self_contained: true,

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
    }

    /// Spec 5.4: selfContained with byte-alignment is valid.
    #[test]
    fn self_contained_with_byte_alignment_is_valid() {
        let opts = ExiOptions {
            self_contained: true,
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
    }

    /// Spec 5.4: All preserve options enabled is valid (without strict).
    #[test]
    fn all_preserve_options_without_strict_is_valid() {
        let opts = ExiOptions {
            preserve: Preserve {
                comments: true,
                pis: true,
                dtd: true,
                prefixes: true,
                lexical_values: true,
                ..Default::default()
            },

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
    }

    // ==================== Ungültige Kombinationen ====================

    /// Spec 5.4: "alignment" MUST NOT appear when "compression" is present.
    #[test]
    fn compression_with_byte_alignment_is_invalid() {
        let opts = ExiOptions {
            compression: true,

            alignment: Alignment::ByteAlignment,
            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "alignment" MUST NOT appear when "compression" is present.
    #[test]
    fn compression_with_pre_compression_is_invalid() {
        let opts = ExiOptions {
            compression: true,

            alignment: Alignment::PreCompression,
            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "strict" MUST NOT appear when "comments" is present.
    #[test]
    fn strict_with_comments_is_invalid() {
        let opts = ExiOptions {
            strict: true,

            preserve: Preserve {
                comments: true,
                ..Default::default()
            },
            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "strict" MUST NOT appear when "pis" is present.
    #[test]
    fn strict_with_pis_is_invalid() {
        let opts = ExiOptions {
            strict: true,

            preserve: Preserve {
                pis: true,
                ..Default::default()
            },
            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "strict" MUST NOT appear when "dtd" is present.
    #[test]
    fn strict_with_dtd_is_invalid() {
        let opts = ExiOptions {
            strict: true,

            preserve: Preserve {
                dtd: true,
                ..Default::default()
            },
            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "strict" MUST NOT appear when "prefixes" is present.
    #[test]
    fn strict_with_prefixes_is_invalid() {
        let opts = ExiOptions {
            strict: true,

            preserve: Preserve {
                prefixes: true,
                ..Default::default()
            },
            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "strict" MUST NOT appear when multiple preserve options are present.
    #[test]
    fn strict_with_multiple_preserve_options_is_invalid() {
        let opts = ExiOptions {
            strict: true,

            preserve: Preserve {
                comments: true,
                pis: true,
                dtd: true,
                prefixes: true,
                ..Default::default()
            },
            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "strict" MUST NOT appear when "selfContained" is present.
    #[test]
    fn strict_with_self_contained_is_invalid() {
        let opts = ExiOptions {
            strict: true,

            self_contained: true,
            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "selfContained" MUST NOT appear when "compression" is present.
    #[test]
    fn self_contained_with_compression_is_invalid() {
        let opts = ExiOptions {
            self_contained: true,
            compression: true,

            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "selfContained" MUST NOT appear when "pre-compression" is present.
    #[test]
    fn self_contained_with_pre_compression_is_invalid() {
        let opts = ExiOptions {
            self_contained: true,
            alignment: Alignment::PreCompression,

            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    /// Spec 5.4: "selfContained" MUST NOT appear when "strict" is present.
    #[test]
    fn self_contained_with_strict_is_invalid() {
        let opts = ExiOptions {
            self_contained: true,
            strict: true,

            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidOptionCombination));
    }

    // ==================== SchemaId Tests ====================

    /// Spec 5.4: SchemaId::None represents xsi:nil="true" (schema-less).
    #[test]
    fn schema_id_none_is_schemaless() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::None),

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
        assert_eq!(opts.schema_id, Some(SchemaId::None));
    }

    /// Spec 5.4: SchemaId::BuiltinOnly means empty schemaId (built-in types only).
    #[test]
    fn schema_id_builtin_only() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::BuiltinOnly),

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
        assert_eq!(opts.schema_id, Some(SchemaId::BuiltinOnly));
    }

    /// Spec 5.4: SchemaId::Id contains a user-defined schema identifier.
    #[test]
    fn schema_id_with_identifier() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::Id("http://example.org/schema".to_string())),

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
    }

    /// Spec 5.4: SchemaId::Id("") ist ungueltig (leerer String → BuiltinOnly verwenden).
    #[test]
    fn schema_id_empty_string_rejected() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::Id(String::new())),

            ..Default::default()
        };
        assert_eq!(opts.validate().unwrap_err(), Error::InvalidOptionCombination);
    }

    // ==================== DatatypeRepresentationMap Tests ====================

    /// Spec 5.4, 7.4: datatypeRepresentationMap can contain mappings.
    #[test]
    fn datatype_representation_map_with_entries() {
        let opts = ExiOptions {
            datatype_representation_map: vec![DatatypeRepresentationMapping {
                type_qname: QName::new("http://www.w3.org/2001/XMLSchema", "decimal"),
                representation_qname: QName::new("http://www.w3.org/2009/exi", "string"),
            }],

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
        assert_eq!(opts.datatype_representation_map.len(), 1);
    }

    // ==================== Trait Tests ====================

    /// ExiOptions implements Clone.
    #[test]
    fn exi_options_is_clone() {
        let opts = ExiOptions::default();
        let cloned = opts.clone();
        assert_eq!(opts, cloned);
    }

    /// ExiOptions implements Debug.
    #[test]
    fn exi_options_is_debug() {
        let opts = ExiOptions::default();
        let debug = format!("{:?}", opts);
        assert!(debug.contains("ExiOptions"));
    }

    /// Alignment implements Default (BitPacked).
    #[test]
    fn alignment_default_is_bit_packed() {
        assert_eq!(Alignment::default(), Alignment::BitPacked);
    }

    /// Preserve implements Default (all false).
    #[test]
    fn preserve_default_all_false() {
        let p = Preserve::default();
        assert!(!p.comments);
        assert!(!p.pis);
        assert!(!p.dtd);
        assert!(!p.prefixes);
        assert!(!p.lexical_values);
        assert!(!p.whitespace);
    }

    // ==================== Preserve-Methoden ====================

    /// preserves_whitespace() ist false bei Default-Preserve.
    #[test]
    fn preserves_whitespace_default_false() {
        assert!(!Preserve::default().preserves_whitespace());
    }

    /// preserves_whitespace() ist true bei whitespace=true.
    #[test]
    fn preserves_whitespace_explicit() {
        let p = Preserve { whitespace: true, ..Default::default() };
        assert!(p.preserves_whitespace());
    }

    /// preserves_whitespace() ist true bei lexical_values=true (impliziert WS-Erhalt).
    #[test]
    fn preserves_whitespace_implied_by_lexical_values() {
        let p = Preserve { lexical_values: true, ..Default::default() };
        assert!(p.preserves_whitespace());
    }

    /// has_header_relevant_flags() ignoriert whitespace (kein Spec-Feld).
    #[test]
    fn has_header_relevant_flags_ignores_whitespace() {
        let p = Preserve { whitespace: true, ..Default::default() };
        assert!(!p.has_header_relevant_flags());
    }

    // ==================== block_size Validierung (Spec 9.1) ====================

    /// Spec 9.1: block_size=0 ist ungültig (Division-by-Zero bei Compression).
    #[test]
    fn block_size_zero_is_invalid() {
        let opts = ExiOptions {
            block_size: 0,

            ..Default::default()
        };
        assert_eq!(opts.validate(), Err(Error::InvalidBlockSize));
    }

    /// Spec 9.1: block_size=1 ist gültig (minimaler Wert).
    #[test]
    fn block_size_one_is_valid() {
        let opts = ExiOptions {
            block_size: 1,

            ..Default::default()
        };
        assert!(opts.validate().is_ok());
    }

    // ==================== effective_alignment Tests (Spec 6.2) ====================

    /// Spec 6.2: Default Options haben effective_alignment = BitPacked.
    #[test]
    fn effective_alignment_default_is_bit_packed() {
        let opts = ExiOptions::default();
        assert_eq!(opts.effective_alignment(), Alignment::BitPacked);
    }

    /// Spec 6.2: compression=true hat effective_alignment = PreCompression.
    #[test]
    fn effective_alignment_compression_is_precompression() {
        let opts = ExiOptions {
            compression: true,

            ..Default::default()
        };
        assert_eq!(opts.effective_alignment(), Alignment::PreCompression);
    }

    /// Spec 6.2: alignment=ByteAlignment hat effective_alignment = ByteAlignment.
    #[test]
    fn effective_alignment_byte_alignment() {
        let opts = ExiOptions {
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };
        assert_eq!(opts.effective_alignment(), Alignment::ByteAlignment);
    }

    /// Spec 6.2: alignment=PreCompression hat effective_alignment = PreCompression.
    #[test]
    fn effective_alignment_precompression() {
        let opts = ExiOptions {
            alignment: Alignment::PreCompression,

            ..Default::default()
        };
        assert_eq!(opts.effective_alignment(), Alignment::PreCompression);
    }
}
