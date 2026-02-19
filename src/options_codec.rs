//! EXI Options Header Encoding/Decoding (Spec 5.4, Appendix C).
//!
//! EXI Options werden als EXI Body (ohne Header!) encodiert, informiert durch
//! das Options-Schema aus Appendix C mit `strict=true`.
//!
//! Bei `strict=true` sind SD/ED/EE implizit (0 bits). SE(header) braucht 1 bit
//! (DocContent: SE(header)=0, SE(*)=1, Spec 8.5.1). Die Dokument-Sequenz ist:
//! ```text
//! [SD] → SE(header)[1 bit] → Header-Content → [EE(header)] → [ED]
//! ```

use crate::bitstream::{BitReader, BitWriter};
use crate::options::{Alignment, DatatypeRepresentationMapping, ExiOptions, SchemaId};
use crate::qname::QName;
use crate::{Error, Result, boolean, n_bit_unsigned_integer, string, unsigned_integer};

/// Maximale Länge eines schemaId-Strings (64 KiB, DoS-Schutz).
const MAX_SCHEMA_ID_LENGTH: usize = 65_536;

// ============================================================================
// Mini-String-Table für Options-Header QName-Encoding (Spec D.1, D.3)
// ============================================================================

/// Mini-String-Table für DTRM QName-Encoding im Options-Header.
///
/// Vorpopuliert nach Spec D.1/D.3 für schema-informed Streams.
/// Das Options-Schema (Appendix C) hat targetNamespace=EXI_NS,
/// daher ist EXI_NS als URI 4 in der URI-Partition.
struct OptionsStringTable {
    /// URI-Partition (Spec D.1 + EXI-NS).
    uris: Vec<String>,
    /// Local-Name-Partitionen pro URI (Spec D.3).
    local_names: Vec<Vec<String>>,
}

use crate::typed_value::{EXI_NS, XSD_NS};

/// XSD Built-in Typnamen, sortiert (Spec D.3, Table D-5).
const XSD_LOCAL_NAMES: &[&str] = &[
    "ENTITIES", "ENTITY", "ID", "IDREF", "IDREFS", "NCName", "NMTOKEN", "NMTOKENS",
    "NOTATION", "Name", "QName", "anySimpleType", "anyType", "anyURI", "base64Binary",
    "boolean", "byte", "date", "dateTime", "decimal", "double", "duration", "float",
    "gDay", "gMonth", "gMonthDay", "gYear", "gYearMonth", "hexBinary", "int", "integer",
    "language", "long", "negativeInteger", "nonNegativeInteger", "nonPositiveInteger",
    "normalizedString", "positiveInteger", "short", "string", "time", "token",
    "unsignedByte", "unsignedInt", "unsignedLong", "unsignedShort",
];

/// EXI Options-Element-Namen, sortiert (Spec Appendix C).
const EXI_LOCAL_NAMES: &[&str] = &[
    "alignment", "blockSize", "byte", "comments", "common", "compression",
    "datatypeRepresentationMap", "dtd", "fragment", "header", "lesscommon",
    "lexicalValues", "pis", "pre-compress", "preserve", "prefixes",
    "schemaId", "selfContained", "strict", "uncommon", "valueMaxLength",
    "valuePartitionCapacity",
];

impl OptionsStringTable {
    /// Erstellt eine neue Options-String-Table mit vorpopulierten Einträgen.
    fn new() -> Self {
        let uris = vec![
            String::new(),                                           // 0: ""
            "http://www.w3.org/XML/1998/namespace".to_string(),      // 1: XML-NS
            "http://www.w3.org/2001/XMLSchema-instance".to_string(), // 2: XSI
            XSD_NS.to_string(),                                      // 3: XSD (schema-informed)
            EXI_NS.to_string(),                                      // 4: EXI (targetNamespace)
        ];
        let local_names = vec![
            vec![],                                                   // 0: "" (keine)
            vec!["base".into(), "id".into(), "lang".into(), "space".into()], // 1: XML-NS
            vec!["nil".into(), "type".into()],                        // 2: XSI
            XSD_LOCAL_NAMES.iter().map(|s| s.to_string()).collect(),   // 3: XSD
            EXI_LOCAL_NAMES.iter().map(|s| s.to_string()).collect(),   // 4: EXI
        ];
        Self { uris, local_names }
    }

    /// Encodiert eine URI (Spec 7.3.2): n-bit Index oder 0 + String Literal.
    fn encode_uri(&mut self, writer: &mut BitWriter, uri: &str) -> usize {
        let count = self.uris.len();
        let bits = bit_width(count + 1);
        // Suche bekannte URI
        if let Some(idx) = self.uris.iter().position(|u| u == uri) {
            n_bit_unsigned_integer::encode(writer, (idx + 1) as u64, bits);
            idx
        } else {
            // Neue URI: 0 + String Literal
            n_bit_unsigned_integer::encode(writer, 0, bits);
            string::encode(writer, uri);
            let idx = self.uris.len();
            self.uris.push(uri.to_string());
            self.local_names.push(Vec::new());
            idx
        }
    }

    /// Decodiert eine URI (Spec 7.3.2): n-bit Index oder 0 + String Literal.
    fn decode_uri(&mut self, reader: &mut BitReader) -> Result<(usize, String)> {
        let count = self.uris.len();
        let bits = bit_width(count + 1);
        let code = n_bit_unsigned_integer::decode(reader, bits)?;
        if code == 0 {
            // Neue URI
            let uri = string::decode(reader)?;
            let idx = self.uris.len();
            self.uris.push(uri.clone());
            self.local_names.push(Vec::new());
            Ok((idx, uri))
        } else {
            let idx = (code - 1) as usize;
            if idx >= self.uris.len() {
                return Err(Error::invalid_event_code("Index ausserhalb", "URI-Tabelle"));
            }
            Ok((idx, self.uris[idx].clone()))
        }
    }

    /// Encodiert einen Local-Name (Spec 7.3.2): n-bit Index oder 0 + String Literal.
    fn encode_local_name(&mut self, writer: &mut BitWriter, uri_idx: usize, local_name: &str) {
        let partition = &self.local_names[uri_idx];
        let count = partition.len();
        let bits = bit_width(count + 1);
        if let Some(idx) = partition.iter().position(|n| n == local_name) {
            n_bit_unsigned_integer::encode(writer, (idx + 1) as u64, bits);
        } else {
            // Neuer Local-Name: 0 + String Literal
            n_bit_unsigned_integer::encode(writer, 0, bits);
            string::encode(writer, local_name);
            self.local_names[uri_idx].push(local_name.to_string());
        }
    }

    /// Decodiert einen Local-Name (Spec 7.3.2): n-bit Index oder 0 + String Literal.
    fn decode_local_name(&mut self, reader: &mut BitReader, uri_idx: usize) -> Result<String> {
        let partition = &self.local_names[uri_idx];
        let count = partition.len();
        let bits = bit_width(count + 1);
        let code = n_bit_unsigned_integer::decode(reader, bits)?;
        if code == 0 {
            // Neuer Local-Name
            let name = string::decode(reader)?;
            self.local_names[uri_idx].push(name.clone());
            Ok(name)
        } else {
            let idx = (code - 1) as usize;
            if idx >= self.local_names[uri_idx].len() {
                return Err(Error::invalid_event_code("Index ausserhalb", "LocalName-Tabelle"));
            }
            Ok(self.local_names[uri_idx][idx].clone())
        }
    }

    /// Encodiert einen QName (URI + Local-Name).
    fn encode_qname(&mut self, writer: &mut BitWriter, qname: &QName) {
        let uri_idx = self.encode_uri(writer, &qname.uri);
        self.encode_local_name(writer, uri_idx, &qname.local_name);
    }

    /// Decodiert einen QName (URI + Local-Name).
    fn decode_qname(&mut self, reader: &mut BitReader) -> Result<QName> {
        let (uri_idx, uri) = self.decode_uri(reader)?;
        let local_name = self.decode_local_name(reader, uri_idx)?;
        Ok(QName::new(uri, local_name))
    }
}

/// Berechnet ceil(log2(n)) für n > 0, sonst 0.
fn bit_width(n: usize) -> u8 {
    crate::bit_width::for_count(n)
}

// ============================================================================
// Helper-Funktionen (Spec 5.4: Default-Werte werden nicht encodiert)
// ============================================================================

/// Prüft ob lesscommon-Element benötigt wird.
/// True wenn: uncommon nötig ODER preserve nötig ODER blockSize != 1_000_000
fn needs_lesscommon(opts: &ExiOptions) -> bool {
    needs_uncommon(opts) || needs_preserve(opts) || opts.block_size != 1_000_000
}

/// Prüft ob uncommon-Element benötigt wird.
/// True wenn: alignment != BitPacked ODER selfContained ODER valueMaxLength.is_some()
///            ODER valuePartitionCapacity.is_some() ODER !datatype_representation_map.is_empty()
fn needs_uncommon(opts: &ExiOptions) -> bool {
    opts.alignment != Alignment::BitPacked
        || opts.self_contained
        || opts.value_max_length.is_some()
        || opts.value_partition_capacity.is_some()
        || !opts.datatype_representation_map.is_empty()
}

/// Prüft ob preserve-Element benötigt wird.
/// True wenn: dtd ODER prefixes ODER lexical_values ODER comments ODER pis
fn needs_preserve(opts: &ExiOptions) -> bool {
    opts.preserve.dtd
        || opts.preserve.prefixes
        || opts.preserve.lexical_values
        || opts.preserve.comments
        || opts.preserve.pis
}

/// Prüft ob common-Element benötigt wird.
/// True wenn: compression ODER fragment ODER schema_id.is_some()
fn needs_common(opts: &ExiOptions) -> bool {
    opts.compression || opts.fragment || opts.schema_id.is_some()
}

// ============================================================================
// Encoding (Spec 5.4, Appendix C)
// ============================================================================

/// Encodiert ExiOptions als EXI Options Document (Spec 5.4).
///
/// Das Options Document besteht NUR aus einem EXI Body (kein Header!).
/// Bei `strict=true` sind SD/ED/EE(header) implizit (0 bits).
/// SE(header) wird mit 1 bit encodiert (DocContent: SE(header)=0, SE(*)=1).
///
/// # Errors
///
/// - `InvalidOptionCombination`: Ungültige Options-Kombination (Spec 5.4)
pub fn encode(writer: &mut BitWriter, options: &ExiOptions) -> Result<()> {
    options.validate()?;

    // SD implicit (0 bits bei strict — einzige Produktion: SD DocContent)
    // SE(header) = Code 0 (1 bit)
    // DocContent hat 2 Produktionen: SE(header)=0, SE(*)=1
    // SE(*) bleibt auch in strict (Spec 8.5.1 — strict pruned nur CM/PI/NS/ER/SC)
    n_bit_unsigned_integer::encode(writer, 0, 1);
    encode_header_content(writer, options)?;
    // EE(header) implicit (0 bits — einzige Produktion im letzten Zustand)
    // ED implicit (0 bits bei strict — DocEnd hat nur ED nach Pruning von CM/PI)
    Ok(())
}

/// Encodiert AfterCommon-State: SE(strict)=0 oder EE=1 (1 bit).
fn encode_after_common(writer: &mut BitWriter, has_strict: bool) {
    if has_strict {
        n_bit_unsigned_integer::encode(writer, 0, 1);
    } else {
        n_bit_unsigned_integer::encode(writer, 1, 1);
    }
}

/// Encodiert Header-Content (Spec Appendix C).
/// HeaderContent: SE(lesscommon)=0, SE(common)=1, SE(strict)=2, EE=3 (2 bits)
fn encode_header_content(writer: &mut BitWriter, opts: &ExiOptions) -> Result<()> {
    let has_lesscommon = needs_lesscommon(opts);
    let has_common = needs_common(opts);
    let has_strict = opts.strict;

    if has_lesscommon {
        n_bit_unsigned_integer::encode(writer, 0, 2);
        encode_lesscommon_content(writer, opts)?;
        // AfterLesscommon: SE(common)=0, SE(strict)=1, EE=2 (2 bits)
        if has_common {
            n_bit_unsigned_integer::encode(writer, 0, 2);
            encode_common_content(writer, opts)?;
            encode_after_common(writer, has_strict);
        } else if has_strict {
            n_bit_unsigned_integer::encode(writer, 1, 2);
        } else {
            n_bit_unsigned_integer::encode(writer, 2, 2);
        }
    } else if has_common {
        n_bit_unsigned_integer::encode(writer, 1, 2);
        encode_common_content(writer, opts)?;
        encode_after_common(writer, has_strict);
    } else if has_strict {
        n_bit_unsigned_integer::encode(writer, 2, 2);
    } else {
        n_bit_unsigned_integer::encode(writer, 3, 2);
    }

    Ok(())
}

/// Encodiert lesscommon-Content.
///
/// LesscommonContent: SE(uncommon), SE(preserve), SE(blockSize), EE → Codes 0,1,2,3 (2 bits)
fn encode_lesscommon_content(writer: &mut BitWriter, opts: &ExiOptions) -> Result<()> {
    let has_uncommon = needs_uncommon(opts);
    let has_preserve = needs_preserve(opts);
    let has_blocksize = opts.block_size != 1_000_000;

    if has_uncommon {
        // SE(uncommon) = 0 (2 bits)
        n_bit_unsigned_integer::encode(writer, 0, 2);
        encode_uncommon_content(writer, opts)?;
        // AfterUncommon: SE(preserve), SE(blockSize), EE → Codes 0,1,2 (2 bits)
        if has_preserve {
            n_bit_unsigned_integer::encode(writer, 0, 2);
            encode_preserve_content(writer, opts)?;
            // AfterPreserve: SE(blockSize), EE → Codes 0,1 (1 bit)
            if has_blocksize {
                n_bit_unsigned_integer::encode(writer, 0, 1);
                encode_unsigned_int_content(writer, opts.block_size as u64);
            } else {
                n_bit_unsigned_integer::encode(writer, 1, 1);
            }
        } else if has_blocksize {
            n_bit_unsigned_integer::encode(writer, 1, 2);
            encode_unsigned_int_content(writer, opts.block_size as u64);
            // AfterBlockSize: EE = 0 (0 bits)
        } else {
            n_bit_unsigned_integer::encode(writer, 2, 2);
        }
    } else if has_preserve {
        // SE(preserve) = 1 (2 bits)
        n_bit_unsigned_integer::encode(writer, 1, 2);
        encode_preserve_content(writer, opts)?;
        // AfterPreserve: SE(blockSize), EE → Codes 0,1 (1 bit)
        if has_blocksize {
            n_bit_unsigned_integer::encode(writer, 0, 1);
            encode_unsigned_int_content(writer, opts.block_size as u64);
        } else {
            n_bit_unsigned_integer::encode(writer, 1, 1);
        }
    } else if has_blocksize {
        // SE(blockSize) = 2 (2 bits)
        n_bit_unsigned_integer::encode(writer, 2, 2);
        encode_unsigned_int_content(writer, opts.block_size as u64);
    } else {
        // needs_lesscommon ist true, also muss mindestens eine der Bedingungen erfüllt sein
        unreachable!("encode_lesscommon_content called but no lesscommon options set");
    }

    Ok(())
}

/// Encodiert uncommon-Content.
///
/// UncommonContent: SE(alignment), SE(selfContained), SE(valueMaxLength),
///                  SE(valuePartitionCapacity), SE(datatypeRepresentationMap), EE
///                  → Codes 0,1,2,3,4,5 (3 bits)
fn encode_uncommon_content(writer: &mut BitWriter, opts: &ExiOptions) -> Result<()> {
    let has_alignment = opts.alignment != Alignment::BitPacked;
    let has_self_contained = opts.self_contained;
    let has_value_max_length = opts.value_max_length.is_some();
    let has_value_partition_capacity = opts.value_partition_capacity.is_some();
    let has_dtrm = !opts.datatype_representation_map.is_empty();

    // State machine für uncommon-Element
    let mut state = UncommonState::Start;

    if has_alignment {
        emit_uncommon_event(writer, &mut state, UncommonEvent::Alignment);
        encode_alignment_content(writer, opts.alignment);
    }

    if has_self_contained {
        emit_uncommon_event(writer, &mut state, UncommonEvent::SelfContained);
        // EmptyContent: EE = 0 (0 bits)
    }

    if has_value_max_length {
        emit_uncommon_event(writer, &mut state, UncommonEvent::ValueMaxLength);
        encode_unsigned_int_content(writer, opts.value_max_length.unwrap() as u64);
    }

    if has_value_partition_capacity {
        emit_uncommon_event(writer, &mut state, UncommonEvent::ValuePartitionCapacity);
        encode_unsigned_int_content(writer, opts.value_partition_capacity.unwrap() as u64);
    }

    if has_dtrm {
        let mut st = OptionsStringTable::new();
        for entry in &opts.datatype_representation_map {
            // SE(datatypeRepresentationMap)
            emit_uncommon_event(writer, &mut state, UncommonEvent::DatatypeRepMap);
            // Encode Child-1 QName (Schema-Typ) als SE(*) mit QName-Encoding
            st.encode_qname(writer, &entry.type_qname);
            // EE für Child-1 (implizit, 0 bits bei strict)
            // Encode Child-2 QName (Representation) als SE(*) mit QName-Encoding
            st.encode_qname(writer, &entry.representation_qname);
            // EE für Child-2 (implizit, 0 bits bei strict)
            // EE für datatypeRepresentationMap (implizit, 0 bits bei strict)
        }
    }

    // EE emittieren
    emit_uncommon_event(writer, &mut state, UncommonEvent::EE);

    Ok(())
}

/// State für uncommon-Element (Encoding und Decoding).
/// Die Bitbreite pro State: Start=3, AfterAlignment=3, AfterSelfContained=2,
/// AfterValueMaxLength=2, AfterValuePartitionCapacity=1,
/// AfterDatatypeRepMap=1
#[derive(Clone, Copy)]
enum UncommonState {
    Start,
    AfterAlignment,
    AfterSelfContained,
    AfterValueMaxLength,
    AfterValuePartitionCapacity,
    AfterDatatypeRepMap,
}

impl UncommonState {
    /// Anzahl Bits für Event-Code in diesem State.
    fn bit_width(self) -> u8 {
        match self {
            Self::Start | Self::AfterAlignment => 3,
            Self::AfterSelfContained | Self::AfterValueMaxLength => 2,
            Self::AfterValuePartitionCapacity | Self::AfterDatatypeRepMap => 1,
        }
    }

    /// Offset in der Event-Reihenfolge: Erster verfügbare Event-Index in diesem State.
    fn offset(self) -> usize {
        match self {
            Self::Start => 0,
            Self::AfterAlignment => 1,
            Self::AfterSelfContained => 2,
            Self::AfterValueMaxLength => 3,
            Self::AfterValuePartitionCapacity | Self::AfterDatatypeRepMap => 4,
        }
    }
}

#[derive(Clone, Copy)]
enum UncommonEvent {
    Alignment,
    SelfContained,
    ValueMaxLength,
    ValuePartitionCapacity,
    DatatypeRepMap,
    EE,
}

impl UncommonEvent {
    /// Index in der geordneten Event-Reihenfolge.
    fn ordinal(self) -> usize {
        match self {
            Self::Alignment => 0,
            Self::SelfContained => 1,
            Self::ValueMaxLength => 2,
            Self::ValuePartitionCapacity => 3,
            Self::DatatypeRepMap => 4,
            Self::EE => 5,
        }
    }

    /// Nächster State nach Emission dieses Events.
    fn next_state(self) -> UncommonState {
        match self {
            Self::Alignment => UncommonState::AfterAlignment,
            Self::SelfContained => UncommonState::AfterSelfContained,
            Self::ValueMaxLength => UncommonState::AfterValueMaxLength,
            Self::ValuePartitionCapacity => UncommonState::AfterValuePartitionCapacity,
            Self::DatatypeRepMap | Self::EE => UncommonState::AfterDatatypeRepMap,
        }
    }
}

/// Emittiert ein Event im uncommon-Element.
///
/// Events sind geordnet: Alignment(0), SelfContained(1), ValueMaxLength(2),
/// ValuePartitionCapacity(3), DatatypeRepMap(4), EE(5). In jedem State sind
/// nur Events mit Index >= state_offset verfügbar, und der Event-Code ist
/// `event_index - state_offset` (Schema-informed Grammar-Prinzip).
fn emit_uncommon_event(writer: &mut BitWriter, state: &mut UncommonState, event: UncommonEvent) {
    let code = event.ordinal() - state.offset();
    n_bit_unsigned_integer::encode(writer, code as u64, state.bit_width());
    *state = event.next_state();
}

/// Encodiert alignment-Content (choice: byte=0 | pre-compress=1, jeweils 1 bit).
fn encode_alignment_content(writer: &mut BitWriter, alignment: Alignment) {
    let code = match alignment {
        Alignment::ByteAlignment => 0,
        Alignment::PreCompression => 1,
        Alignment::BitPacked => unreachable!("BitPacked is default and not encoded"),
    };
    n_bit_unsigned_integer::encode(writer, code, 1);
}

/// Encodiert preserve-Content.
///
/// PreserveContent: SE(dtd), SE(prefixes), SE(lexicalValues), SE(comments), SE(pis), EE
///                  → Codes 0,1,2,3,4,5 (3 bits)
fn encode_preserve_content(writer: &mut BitWriter, opts: &ExiOptions) -> Result<()> {
    let mut state = PreserveState::Start;

    if opts.preserve.dtd {
        emit_preserve_event(writer, &mut state, PreserveEvent::Dtd);
    }
    if opts.preserve.prefixes {
        emit_preserve_event(writer, &mut state, PreserveEvent::Prefixes);
    }
    if opts.preserve.lexical_values {
        emit_preserve_event(writer, &mut state, PreserveEvent::LexicalValues);
    }
    if opts.preserve.comments {
        emit_preserve_event(writer, &mut state, PreserveEvent::Comments);
    }
    if opts.preserve.pis {
        emit_preserve_event(writer, &mut state, PreserveEvent::Pis);
    }

    emit_preserve_event(writer, &mut state, PreserveEvent::EE);

    Ok(())
}

/// State für preserve-Element (Encoding und Decoding).
///
/// Gleiche Architektur wie `UncommonState`: Offset-basierte Event-Codes.
/// Events: Dtd(0), Prefixes(1), LexicalValues(2), Comments(3), Pis(4), EE(5).
/// Bitbreite pro State: Start=3, AfterDtd=3, AfterPrefixes=2,
/// AfterLexicalValues=2, AfterComments=1, AfterPis=0 (implizites EE).
#[derive(Clone, Copy)]
enum PreserveState {
    Start,
    AfterDtd,
    AfterPrefixes,
    AfterLexicalValues,
    AfterComments,
    AfterPis,
}

impl PreserveState {
    /// Anzahl Bits für Event-Code in diesem State (None = implizites EE).
    fn bit_width(self) -> Option<u8> {
        match self {
            Self::Start | Self::AfterDtd => Some(3),
            Self::AfterPrefixes | Self::AfterLexicalValues => Some(2),
            Self::AfterComments => Some(1),
            Self::AfterPis => None,
        }
    }

    /// Offset in der Event-Reihenfolge: Erster verfügbarer Event-Index in diesem State.
    fn offset(self) -> usize {
        match self {
            Self::Start => 0,
            Self::AfterDtd => 1,
            Self::AfterPrefixes => 2,
            Self::AfterLexicalValues => 3,
            Self::AfterComments => 4,
            Self::AfterPis => 5,
        }
    }
}

#[derive(Clone, Copy)]
enum PreserveEvent {
    Dtd,
    Prefixes,
    LexicalValues,
    Comments,
    Pis,
    EE,
}

impl PreserveEvent {
    /// Index in der geordneten Event-Reihenfolge.
    fn ordinal(self) -> usize {
        match self {
            Self::Dtd => 0,
            Self::Prefixes => 1,
            Self::LexicalValues => 2,
            Self::Comments => 3,
            Self::Pis => 4,
            Self::EE => 5,
        }
    }

    /// Nächster State nach Emission dieses Events.
    fn next_state(self) -> PreserveState {
        match self {
            Self::Dtd => PreserveState::AfterDtd,
            Self::Prefixes => PreserveState::AfterPrefixes,
            Self::LexicalValues => PreserveState::AfterLexicalValues,
            Self::Comments => PreserveState::AfterComments,
            Self::Pis | Self::EE => PreserveState::AfterPis,
        }
    }
}

/// Emittiert ein Event im preserve-Element (Offset-basiert, analog zu emit_uncommon_event).
fn emit_preserve_event(writer: &mut BitWriter, state: &mut PreserveState, event: PreserveEvent) {
    let Some(bits) = state.bit_width() else {
        // AfterPis: 0 bits - implizites EE
        return;
    };
    let code = event.ordinal() - state.offset();
    n_bit_unsigned_integer::encode(writer, code as u64, bits);
    *state = event.next_state();
}

/// Encodiert AfterFragment-State: SE(schemaId)=0 oder EE=1 (1 bit).
fn encode_after_fragment(writer: &mut BitWriter, schema_id: Option<&SchemaId>) {
    if let Some(id) = schema_id {
        n_bit_unsigned_integer::encode(writer, 0, 1);
        encode_schema_id_content(writer, id);
    } else {
        n_bit_unsigned_integer::encode(writer, 1, 1);
    }
}

/// Encodiert common-Content.
/// CommonContent: SE(compression)=0, SE(fragment)=1, SE(schemaId)=2, EE=3 (2 bits)
fn encode_common_content(writer: &mut BitWriter, opts: &ExiOptions) -> Result<()> {
    let has_compression = opts.compression;
    let has_fragment = opts.fragment;
    let schema_id = opts.schema_id.as_ref();

    if has_compression {
        n_bit_unsigned_integer::encode(writer, 0, 2);
        // AfterCompression: SE(fragment)=0, SE(schemaId)=1, EE=2 (2 bits)
        if has_fragment {
            n_bit_unsigned_integer::encode(writer, 0, 2);
            encode_after_fragment(writer, schema_id);
        } else if let Some(id) = schema_id {
            n_bit_unsigned_integer::encode(writer, 1, 2);
            encode_schema_id_content(writer, id);
        } else {
            n_bit_unsigned_integer::encode(writer, 2, 2);
        }
    } else if has_fragment {
        n_bit_unsigned_integer::encode(writer, 1, 2);
        encode_after_fragment(writer, schema_id);
    } else if let Some(id) = schema_id {
        n_bit_unsigned_integer::encode(writer, 2, 2);
        encode_schema_id_content(writer, id);
    } else {
        n_bit_unsigned_integer::encode(writer, 3, 2);
    }

    Ok(())
}

/// Encodiert schemaId-Content (nillable string).
///
/// Event Codes (variable length, EXIficient-kompatibel):
/// - CH = `0` (1 bit) → String-Value (Spec 7.3.3, length+2)
/// - AT(xsi:nil) = `10` (2 bits) → Bool + AfterNilAttr (CH/EE, 1 bit)
/// - EE = `11` (2 bits) → leeres Element (BuiltinOnly)
fn encode_schema_id_content(writer: &mut BitWriter, schema_id: &SchemaId) {
    match schema_id {
        SchemaId::None => {
            // AT(xsi:nil)=true, dann EE → `10` + true + `1`
            n_bit_unsigned_integer::encode(writer, 1, 1); // nicht CH (→ AT oder EE)
            n_bit_unsigned_integer::encode(writer, 0, 1); // AT(xsi:nil)
            boolean::encode(writer, true);
            n_bit_unsigned_integer::encode(writer, 1, 1); // EE (AfterNilAttr)
        }
        SchemaId::BuiltinOnly => {
            // EE ohne Content → `11`
            n_bit_unsigned_integer::encode(writer, 1, 1); // nicht CH (→ AT oder EE)
            n_bit_unsigned_integer::encode(writer, 1, 1); // EE
        }
        SchemaId::Id(s) => {
            // CH mit Schema-ID String (Value-Encoding length+2)
            n_bit_unsigned_integer::encode(writer, 0, 1); // CH
            encode_schema_id_value(writer, s);
        }
    }
}

/// Encodiert schemaId-String als EXI Value (Spec 7.3.3, length+2).
fn encode_schema_id_value(writer: &mut BitWriter, value: &str) {
    if value.is_ascii() {
        unsigned_integer::encode(writer, value.len() as u64 + 2);
        writer.write_bytes_aligned(value.as_bytes());
    } else {
        string::encode_non_ascii(writer, value, 2);
    }
}

/// Encodiert unsignedInt-Content (CH und EE sind implizit, 0 bits).
fn encode_unsigned_int_content(writer: &mut BitWriter, value: u64) {
    unsigned_integer::encode(writer, value);
}

// ============================================================================
// Decoding (Spec 5.4, Appendix C)
// ============================================================================

/// Decodiert ExiOptions aus einem EXI Options Document.
///
/// # Errors
///
/// - `PrematureEndOfStream`: Stream endet vorzeitig
/// - `InvalidEventCode`: Ungültiger Event-Code
/// - `InvalidOptionCombination`: Decodierte Options sind ungültig
/// - `SchemaViolation`: schemaId-Element verletzt xsi:nil-Semantik
/// - `IntegerOverflow`: unsignedInt-Wert überschreitet u32::MAX
/// - `InvalidCodePoint`: schemaId-String enthält ungültigen Unicode-Codepoint
pub fn decode(reader: &mut BitReader) -> Result<ExiOptions> {
    let mut options = ExiOptions::default();
    // SD implicit (0 bits bei strict — einzige Produktion: SD DocContent)
    // SE(header) = Code 0 (1 bit)
    // DocContent hat 2 Produktionen: SE(header)=0, SE(*)=1 (Spec 8.5.1)
    let se_code = n_bit_unsigned_integer::decode(reader, 1)?;
    if se_code != 0 {
        return Err(Error::invalid_event_code(
            "SE(header) erwartet (Code 0)",
            "DocContent",
        ));
    }
    decode_header_content(reader, &mut options)?;
    // EE(header) implicit (0 bits — einzige Produktion im letzten Zustand)
    // ED implicit (0 bits bei strict — DocEnd hat nur ED nach Pruning von CM/PI)
    options.validate()?;
    Ok(options)
}

/// Decodiert Header-Content.
fn decode_header_content(reader: &mut BitReader, opts: &mut ExiOptions) -> Result<()> {
    // HeaderContent: SE(lesscommon), SE(common), SE(strict), EE → Codes 0,1,2,3 (2 bits)
    let code = n_bit_unsigned_integer::decode(reader, 2)?;

    match code {
        0 => {
            // SE(lesscommon)
            decode_lesscommon_content(reader, opts)?;
            // AfterLesscommon: SE(common), SE(strict), EE → Codes 0,1,2 (2 bits)
            let code2 = n_bit_unsigned_integer::decode(reader, 2)?;
            match code2 {
                0 => {
                    // SE(common)
                    decode_common_content(reader, opts)?;
                    // AfterCommon: SE(strict), EE → Codes 0,1 (1 bit)
                    let code3 = n_bit_unsigned_integer::decode(reader, 1)?;
                    if code3 == 0 {
                        opts.strict = true;
                    }
                }
                1 => {
                    // SE(strict)
                    opts.strict = true;
                }
                2 => {
                    // EE
                }
                _ => return Err(Error::invalid_event_code("unerwarteter Code", "header/nachLesscommon")),
            }
        }
        1 => {
            // SE(common)
            decode_common_content(reader, opts)?;
            // AfterCommon: SE(strict), EE → Codes 0,1 (1 bit)
            let code2 = n_bit_unsigned_integer::decode(reader, 1)?;
            if code2 == 0 {
                opts.strict = true;
            }
        }
        2 => {
            // SE(strict)
            opts.strict = true;
        }
        3 => {
            // EE - Default options
        }
        _ => return Err(Error::invalid_event_code("unerwarteter Code", "header")),
    }

    Ok(())
}

/// Decodiert lesscommon-Content.
fn decode_lesscommon_content(reader: &mut BitReader, opts: &mut ExiOptions) -> Result<()> {
    // LesscommonContent: SE(uncommon), SE(preserve), SE(blockSize), EE → Codes 0,1,2,3 (2 bits)
    let code = n_bit_unsigned_integer::decode(reader, 2)?;

    match code {
        0 => {
            // SE(uncommon)
            decode_uncommon_content(reader, opts)?;
            // AfterUncommon: SE(preserve), SE(blockSize), EE → Codes 0,1,2 (2 bits)
            let code2 = n_bit_unsigned_integer::decode(reader, 2)?;
            match code2 {
                0 => {
                    decode_preserve_content(reader, opts)?;
                    // AfterPreserve: SE(blockSize), EE → Codes 0,1 (1 bit)
                    let code3 = n_bit_unsigned_integer::decode(reader, 1)?;
                    if code3 == 0 {
                        opts.block_size = decode_unsigned_int_content_u32(reader)?;
                    }
                }
                1 => {
                    opts.block_size = decode_unsigned_int_content_u32(reader)?;
                }
                2 => {
                    // EE
                }
                _ => return Err(Error::invalid_event_code("unerwarteter Code", "lesscommon/nachUncommon")),
            }
        }
        1 => {
            // SE(preserve)
            decode_preserve_content(reader, opts)?;
            // AfterPreserve: SE(blockSize), EE → Codes 0,1 (1 bit)
            let code2 = n_bit_unsigned_integer::decode(reader, 1)?;
            if code2 == 0 {
                opts.block_size = decode_unsigned_int_content_u32(reader)?;
            }
        }
        2 => {
            // SE(blockSize)
            opts.block_size = decode_unsigned_int_content_u32(reader)?;
        }
        3 => {
            // EE
        }
        _ => return Err(Error::invalid_event_code("unerwarteter Code", "lesscommon")),
    }

    Ok(())
}

/// Decodiert uncommon-Content mit Offset-basierter Event-Auflösung.
///
/// Gleiche Logik wie emit_uncommon_event: `event_ordinal = code + state.offset()`.
fn decode_uncommon_content(reader: &mut BitReader, opts: &mut ExiOptions) -> Result<()> {
    let mut state = UncommonState::Start;
    let mut st: Option<OptionsStringTable> = None;

    loop {
        let code = n_bit_unsigned_integer::decode(reader, state.bit_width())? as usize;
        let event_ordinal = code + state.offset();

        match event_ordinal {
            0 => {
                opts.alignment = decode_alignment_content(reader)?;
                state = UncommonState::AfterAlignment;
            }
            1 => {
                opts.self_contained = true;
                state = UncommonState::AfterSelfContained;
            }
            2 => {
                opts.value_max_length = Some(decode_unsigned_int_content_u32(reader)?);
                state = UncommonState::AfterValueMaxLength;
            }
            3 => {
                opts.value_partition_capacity = Some(decode_unsigned_int_content_u32(reader)?);
                state = UncommonState::AfterValuePartitionCapacity;
            }
            4 => {
                decode_dtrm_entry(reader, opts, &mut st)?;
                state = UncommonState::AfterDatatypeRepMap;
            }
            5 => return Ok(()),
            _ => return Err(Error::invalid_event_code("unerwarteter Code", "uncommon")),
        }
    }
}

/// Decodiert ein einzelnes datatypeRepresentationMap-Entry (zwei QNames).
fn decode_dtrm_entry(
    reader: &mut BitReader,
    opts: &mut ExiOptions,
    st: &mut Option<OptionsStringTable>,
) -> Result<()> {
    let table = st.get_or_insert_with(OptionsStringTable::new);
    let type_qname = table.decode_qname(reader)?;
    let representation_qname = table.decode_qname(reader)?;
    opts.datatype_representation_map
        .push(DatatypeRepresentationMapping {
            type_qname,
            representation_qname,
        });
    Ok(())
}

/// Decodiert alignment-Content.
fn decode_alignment_content(reader: &mut BitReader) -> Result<Alignment> {
    // AlignmentContent: SE(byte)=0, SE(pre-compress)=1 (1 bit)
    let code = n_bit_unsigned_integer::decode(reader, 1)?;
    match code {
        0 => Ok(Alignment::ByteAlignment),
        1 => Ok(Alignment::PreCompression),
        _ => Err(Error::invalid_event_code("unerwarteter Code", "alignment")),
    }
}

/// Decodiert preserve-Content mit Offset-basierter Event-Auflösung.
///
/// Gleiche Logik wie emit_preserve_event: `event_ordinal = code + state.offset()`.
fn decode_preserve_content(reader: &mut BitReader, opts: &mut ExiOptions) -> Result<()> {
    let mut state = PreserveState::Start;

    loop {
        let Some(bits) = state.bit_width() else {
            // AfterPis: 0 bits - implizites EE
            return Ok(());
        };
        let code = n_bit_unsigned_integer::decode(reader, bits)? as usize;
        let event_ordinal = code + state.offset();

        match event_ordinal {
            0 => {
                opts.preserve.dtd = true;
                state = PreserveState::AfterDtd;
            }
            1 => {
                opts.preserve.prefixes = true;
                state = PreserveState::AfterPrefixes;
            }
            2 => {
                opts.preserve.lexical_values = true;
                state = PreserveState::AfterLexicalValues;
            }
            3 => {
                opts.preserve.comments = true;
                state = PreserveState::AfterComments;
            }
            4 => {
                opts.preserve.pis = true;
                state = PreserveState::AfterPis;
            }
            5 => return Ok(()),
            _ => return Err(Error::invalid_event_code("unerwarteter Code", "preserve")),
        }
    }
}

/// Decodiert common-Content.
fn decode_common_content(reader: &mut BitReader, opts: &mut ExiOptions) -> Result<()> {
    // CommonContent: SE(compression)=0, SE(fragment)=1, SE(schemaId)=2, EE=3 (2 bits)
    let code = n_bit_unsigned_integer::decode(reader, 2)?;

    match code {
        0 => {
            // SE(compression)
            opts.compression = true;
            // AfterCompression: SE(fragment), SE(schemaId), EE → Codes 0,1,2 (2 bits)
            let code2 = n_bit_unsigned_integer::decode(reader, 2)?;
            match code2 {
                0 => {
                    opts.fragment = true;
                    // AfterFragment: SE(schemaId), EE → Codes 0,1 (1 bit)
                    let code3 = n_bit_unsigned_integer::decode(reader, 1)?;
                    if code3 == 0 {
                        opts.schema_id = Some(decode_schema_id_content(reader)?);
                    }
                }
                1 => {
                    opts.schema_id = Some(decode_schema_id_content(reader)?);
                }
                2 => {
                    // EE
                }
                _ => return Err(Error::invalid_event_code("unerwarteter Code", "common/nachCompression")),
            }
        }
        1 => {
            // SE(fragment)
            opts.fragment = true;
            // AfterFragment: SE(schemaId), EE → Codes 0,1 (1 bit)
            let code2 = n_bit_unsigned_integer::decode(reader, 1)?;
            if code2 == 0 {
                opts.schema_id = Some(decode_schema_id_content(reader)?);
            }
        }
        2 => {
            // SE(schemaId)
            opts.schema_id = Some(decode_schema_id_content(reader)?);
        }
        3 => {
            // EE
        }
        _ => return Err(Error::invalid_event_code("unerwarteter Code", "common")),
    }

    Ok(())
}

/// Decodiert schemaId-Content.
/// Konvertiert String zu SchemaId (leerer String = BuiltinOnly).
fn schema_id_from_string(s: String) -> SchemaId {
    if s.is_empty() {
        SchemaId::BuiltinOnly
    } else {
        SchemaId::Id(s)
    }
}

fn decode_schema_id_content(reader: &mut BitReader) -> Result<SchemaId> {
    // SchemaIdContent (variable length):
    // - CH = 0 (1 bit) → String-Value (length+2)
    // - AT(xsi:nil) = 10 (2 bits) → Bool + AfterNilAttr (CH/EE, 1 bit)
    // - EE = 11 (2 bits) → BuiltinOnly
    let first = n_bit_unsigned_integer::decode(reader, 1)?;
    if first == 0 {
        let s = decode_schema_id_value(reader)?;
        return Ok(schema_id_from_string(s));
    }

    let second = n_bit_unsigned_integer::decode(reader, 1)?;
    if second == 1 {
        return Ok(SchemaId::BuiltinOnly);
    }

    // AT(xsi:nil)
    let nil_value = boolean::decode(reader)?;
    let code = n_bit_unsigned_integer::decode(reader, 1)?; // AfterNilAttr: CH=0, EE=1
    if nil_value {
        if code == 0 {
            return Err(Error::schema_violation("schemaId: xsi:nil=true verlangt EE, aber CH gefunden"));
        }
        return Ok(SchemaId::None);
    }

    if code == 1 {
        return Err(Error::schema_violation("schemaId: xsi:nil=false verlangt CH, aber EE gefunden"));
    }
    let s = decode_schema_id_value(reader)?;
    Ok(schema_id_from_string(s))
}

fn decode_schema_id_value(reader: &mut BitReader) -> Result<String> {
    let indicator = unsigned_integer::decode(reader)?;
    if indicator < 2 {
        return Err(Error::invalid_event_code("schemaId: string table hits nicht unterstützt", "schemaId"));
    }
    let len = indicator - 2;
    if len > MAX_SCHEMA_ID_LENGTH as u64 {
        return Err(Error::StringLengthExceeded { length: len, max: MAX_SCHEMA_ID_LENGTH as u32 });
    }
    let mut s = String::with_capacity(len as usize);
    for _ in 0..len {
        let cp = unsigned_integer::decode(reader)?;
        let ch = u32::try_from(cp)
            .ok()
            .and_then(char::from_u32)
            .ok_or_else(|| Error::InvalidCodePoint(cp))?;
        s.push(ch);
    }
    Ok(s)
}

/// Decodiert unsignedInt-Content als u64.
fn decode_unsigned_int_content(reader: &mut BitReader) -> Result<u64> {
    // CH implizit (einzige Content-Production, 0 bits)
    unsigned_integer::decode(reader)
    // EE implizit (einzige Production nach CH, 0 bits)
}

/// Decodiert unsignedInt-Content als u32 mit Overflow-Prüfung.
///
/// Die Spec definiert valueMaxLength, valuePartitionCapacity und blockSize
/// als xsd:unsignedInt (32-bit). Diese Funktion prüft, dass der decodierte
/// Wert in u32 passt.
fn decode_unsigned_int_content_u32(reader: &mut BitReader) -> Result<u32> {
    let value = decode_unsigned_int_content(reader)?;
    u32::try_from(value).map_err(|_| Error::IntegerOverflow)
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Encodiert ExiOptions in einen Byte-Vektor.
pub fn encode_to_vec(options: &ExiOptions) -> Result<Vec<u8>> {
    let mut writer = BitWriter::new();
    encode(&mut writer, options)?;
    Ok(writer.into_vec())
}

/// Decodiert ExiOptions aus einem Byte-Slice.
///
/// Diese Funktion prüft, dass das gesamte Slice konsumiert wurde (keine
/// trailing Bytes nach dem Options Document). Für Low-Level-Kontrolle
/// verwende [`decode`] direkt.
///
/// # Errors
///
/// Zusätzlich zu den Fehlern von [`decode`]:
/// - `MalformedHeader`: Trailing Bytes oder non-zero Padding-Bits
pub fn decode_from_slice(data: &[u8]) -> Result<ExiOptions> {
    let mut reader = BitReader::new(data);
    let options = decode(&mut reader)?;

    // Prüfe Padding-Bits (müssen 0 sein, Spec 5.2)
    let bit_offset = reader.bit_position() % 8;
    if bit_offset != 0 {
        let padding_bits = 8 - bit_offset;
        let padding = n_bit_unsigned_integer::decode(&mut reader, padding_bits as u8)?;
        if padding != 0 {
            return Err(Error::MalformedHeader);
        }
    }

    // Prüfe auf trailing Bytes nach dem Options Document
    let consumed_bytes = reader.bit_position() / 8;
    if consumed_bytes < data.len() {
        return Err(Error::MalformedHeader);
    }

    Ok(options)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::options::{DatatypeRepresentationMapping, Preserve};

    // ==================== Round-Trip Tests ====================

    /// Spec 5.4: Default options (minimales Encoding).
    #[test]
    fn round_trip_default_options() {
        let opts = ExiOptions::default();
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: strict=true round-trip.
    #[test]
    fn round_trip_strict() {
        let opts = ExiOptions {
            strict: true,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: compression=true round-trip.
    #[test]
    fn round_trip_compression() {
        let opts = ExiOptions {
            compression: true,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: fragment=true round-trip.
    #[test]
    fn round_trip_fragment() {
        let opts = ExiOptions {
            fragment: true,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: alignment=ByteAlignment round-trip.
    #[test]
    fn round_trip_byte_alignment() {
        let opts = ExiOptions {
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: alignment=PreCompression round-trip.
    #[test]
    fn round_trip_pre_compression() {
        let opts = ExiOptions {
            alignment: Alignment::PreCompression,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: selfContained=true round-trip.
    #[test]
    fn round_trip_self_contained() {
        let opts = ExiOptions {
            self_contained: true,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: valueMaxLength round-trip.
    #[test]
    fn round_trip_value_max_length() {
        let opts = ExiOptions {
            value_max_length: Some(1024),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: valuePartitionCapacity round-trip.
    #[test]
    fn round_trip_value_partition_capacity() {
        let opts = ExiOptions {
            value_partition_capacity: Some(4096),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: blockSize round-trip (nicht Default).
    #[test]
    fn round_trip_block_size() {
        let opts = ExiOptions {
            block_size: 500_000,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: schemaId=None (xsi:nil=true) round-trip.
    #[test]
    fn round_trip_schema_id_none() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::None),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: schemaId=BuiltinOnly (leerer String) round-trip.
    #[test]
    fn round_trip_schema_id_builtin_only() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::BuiltinOnly),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: schemaId=Id round-trip.
    #[test]
    fn round_trip_schema_id_custom() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::Id("http://example.org/schema".to_string())),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: preserve.dtd round-trip.
    #[test]
    fn round_trip_preserve_dtd() {
        let opts = ExiOptions {
            preserve: Preserve {
                dtd: true,
                ..Default::default()
            },

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: preserve.prefixes round-trip.
    #[test]
    fn round_trip_preserve_prefixes() {
        let opts = ExiOptions {
            preserve: Preserve {
                prefixes: true,
                ..Default::default()
            },

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: preserve.lexical_values round-trip.
    #[test]
    fn round_trip_preserve_lexical_values() {
        let opts = ExiOptions {
            preserve: Preserve {
                lexical_values: true,
                ..Default::default()
            },

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: preserve.comments round-trip.
    #[test]
    fn round_trip_preserve_comments() {
        let opts = ExiOptions {
            preserve: Preserve {
                comments: true,
                ..Default::default()
            },

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: preserve.pis round-trip.
    #[test]
    fn round_trip_preserve_pis() {
        let opts = ExiOptions {
            preserve: Preserve {
                pis: true,
                ..Default::default()
            },

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: alle preserve-Optionen zusammen.
    #[test]
    fn round_trip_all_preserve_options() {
        let opts = ExiOptions {
            preserve: Preserve {
                dtd: true,
                prefixes: true,
                lexical_values: true,
                comments: true,
                pis: true,
                ..Default::default()
            },

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: Kombination lesscommon + common + strict.
    #[test]
    fn round_trip_all_sections() {
        let opts = ExiOptions {
            self_contained: true,
            value_max_length: Some(2048),
            fragment: true,
            schema_id: Some(SchemaId::Id("test".to_string())),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: Kombination mit strict und lexical_values (erlaubt).
    #[test]
    fn round_trip_strict_with_lexical_values() {
        let opts = ExiOptions {
            strict: true,

            preserve: Preserve {
                lexical_values: true,
                ..Default::default()
            },
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    // ==================== Golden Vector Tests ====================

    /// Spec 5.4: Default options encodiert als SE(header) + EE(header).
    #[test]
    fn golden_vector_default() {
        let opts = ExiOptions::default();
        let encoded = encode_to_vec(&opts).unwrap();
        // SE(header) = 0 → `0` (1 bit)
        // HeaderContent: EE = 3 → `11` (2 bits)
        // Total: `0_11` = 3 bits → padded to 0b0110_0000
        assert_eq!(encoded, vec![0b0110_0000]);
    }

    /// Spec 5.4: strict=true encodiert als SE(header) + SE(strict) + EE.
    #[test]
    fn golden_vector_strict() {
        let opts = ExiOptions {
            strict: true,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        // SE(header) = 0 → `0` (1 bit)
        // HeaderContent: SE(strict) = 2 → `10` (2 bits)
        // Total: `0_10` = 3 bits → padded to 0b0100_0000
        assert_eq!(encoded, vec![0b0100_0000]);
    }

    /// Spec 5.4: compression=true encodiert.
    #[test]
    fn golden_vector_compression() {
        let opts = ExiOptions {
            compression: true,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        // SE(header) = 0 → `0` (1 bit)
        // HeaderContent: SE(common) = 1 → `01` (2 bits)
        // CommonContent: SE(compression) = 0 → `00` (2 bits)
        // AfterCompression: EE = 2 → `10` (2 bits)
        // AfterCommon: EE = 1 → `1` (1 bit)
        // Total: `0_01_00_10_1` = 8 bits → 0b0010_0101
        assert_eq!(encoded, vec![0b0010_0101]);
    }

    /// Spec 5.4: fragment=true encodiert.
    #[test]
    fn golden_vector_fragment() {
        let opts = ExiOptions {
            fragment: true,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        // SE(header) = 0 → `0` (1 bit)
        // HeaderContent: SE(common) = 1 → `01` (2 bits)
        // CommonContent: SE(fragment) = 1 → `01` (2 bits)
        // AfterFragment: EE = 1 → `1` (1 bit)
        // AfterCommon: EE = 1 → `1` (1 bit)
        // Total: `0_01_01_1_1` = 7 bits → 0b0010_1110
        assert_eq!(encoded, vec![0b0010_1110]);
    }

    /// Spec 5.4: schemaId=None (xsi:nil=true) encodiert.
    #[test]
    fn golden_vector_schema_id_none() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::None),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        // SE(header) = 0 → `0` (1 bit)
        // HeaderContent: SE(common) = 1 → `01` (2 bits)
        // CommonContent: SE(schemaId) = 2 → `10` (2 bits)
        // SchemaIdContent: AT(xsi:nil) = `10` (2 bits)
        // xsi:nil value: true → `1` (1 bit)
        // AfterNilAttr: EE = 1 → `1` (1 bit)
        // AfterCommon: EE = 1 → `1` (1 bit)
        // Total: `0_01_10_10_1_1_1` = 10 bits → 2 bytes
        assert_eq!(encoded, vec![0x35, 0xC0]);
    }

    /// Spec 5.4: schemaId=BuiltinOnly (leerer String) encodiert.
    #[test]
    fn golden_vector_schema_id_builtin_only() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::BuiltinOnly),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        // SE(header) = 0 → `0` (1 bit)
        // HeaderContent: SE(common) = 1 → `01` (2 bits)
        // CommonContent: SE(schemaId) = 2 → `10` (2 bits)
        // SchemaIdContent: EE = `11` (2 bits)
        // AfterCommon: EE = 1 → `1` (1 bit)
        // Total: `0_01_10_11_1` = 8 bits → 0b0011_0111
        assert_eq!(encoded, vec![0b0011_0111]);
    }

    /// Spec 5.4: alignment=ByteAlignment encodiert.
    #[test]
    fn golden_vector_byte_alignment() {
        let opts = ExiOptions {
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        // SE(header) = 0 → `0` (1 bit)
        // HeaderContent: SE(lesscommon) = 0 → `00` (2 bits)
        // LesscommonContent: SE(uncommon) = 0 → `00` (2 bits)
        // UncommonContent: SE(alignment) = 0 → `000` (3 bits)
        // AlignmentContent: SE(byte) = 0 → `0` (1 bit)
        // AfterAlignment: EE = 4 → `100` (3 bits)
        // AfterUncommon: EE = 2 → `10` (2 bits)
        // AfterLesscommon: EE = 2 → `10` (2 bits)
        // Total: `0_00_00_000_0_100_10_10` = 16 bits
        // Byte 1: 0000_0000 = 0x00
        // Byte 2: 0100_1010 = 0x4A
        assert_eq!(encoded, vec![0x00, 0x4A]);
    }

    // ==================== Error Cases ====================

    /// Spec 5.4, 7.4: datatypeRepresentationMap mit einem Mapping encodiert und decodiert.
    #[test]
    fn round_trip_datatype_representation_map_single() {
        use crate::qname::QName;
        let opts = ExiOptions {
            datatype_representation_map: vec![DatatypeRepresentationMapping {
                type_qname: QName::new("http://www.w3.org/2001/XMLSchema", "decimal"),
                representation_qname: QName::new("http://www.w3.org/2009/exi", "string"),
            }],

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 5.4: ungültige Option-Kombination.
    #[test]
    fn encode_invalid_option_combination() {
        let opts = ExiOptions {
            compression: true,

            alignment: Alignment::ByteAlignment,
            ..Default::default()
        };
        let result = encode_to_vec(&opts);
        assert_eq!(result.unwrap_err(), Error::InvalidOptionCombination);
    }

    /// Spec 5.4: truncated stream → PrematureEndOfStream.
    #[test]
    fn decode_truncated_stream() {
        let result = decode_from_slice(&[]);
        assert_eq!(result.unwrap_err(), Error::PrematureEndOfStream);
    }

    /// Trailing bytes nach Options Document → MalformedHeader.
    #[test]
    fn decode_trailing_bytes() {
        // SE(header)=0 (1 bit) + EE=3 (2 bits) + padding + trailing garbage byte
        let data = vec![0b0110_0000, 0xFF];
        let result = decode_from_slice(&data);
        assert_eq!(result.unwrap_err(), Error::MalformedHeader);
    }

    /// Trailing bytes: valides Options Document ohne Garbage wird akzeptiert.
    #[test]
    fn decode_no_trailing_bytes() {
        // SE(header)=0 (1 bit) + EE=3 (2 bits) + zero padding
        let data = vec![0b0110_0000];
        let result = decode_from_slice(&data);
        assert!(result.is_ok());
    }

    /// Spec 5.2: Non-zero Padding-Bits → MalformedHeader.
    #[test]
    fn decode_nonzero_padding_bits() {
        // SE(header)=0 (1 bit) + EE=3 (2 bits) + non-zero padding (5 bits)
        // 0b011_11111 - padding sollte 0 sein
        let data = vec![0b0111_1111];
        let result = decode_from_slice(&data);
        assert_eq!(result.unwrap_err(), Error::MalformedHeader);
    }

    /// Spec 5.2: Zero Padding-Bits werden akzeptiert.
    #[test]
    fn decode_zero_padding_bits() {
        // SE(header)=0 (1 bit) + EE=3 (2 bits) + zero padding (5 bits)
        let data = vec![0b0110_0000];
        let result = decode_from_slice(&data);
        assert!(result.is_ok());
    }

    /// Spec 5.4: Decode stream mit strict=true + comments=true → InvalidOptionCombination.
    #[test]
    fn decode_invalid_option_combination_strict_comments() {
        // Manuell konstruierter Stream: SE(header) + SE(lesscommon) + SE(preserve) + SE(comments) + EE + EE + SE(strict)
        let mut writer = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // SE(header)
        n_bit_unsigned_integer::encode(&mut writer, 0, 2); // SE(lesscommon)
        n_bit_unsigned_integer::encode(&mut writer, 1, 2); // SE(preserve)
        n_bit_unsigned_integer::encode(&mut writer, 3, 3); // SE(comments)
        // AfterComments: EE = 1 (1 bit)
        n_bit_unsigned_integer::encode(&mut writer, 1, 1); // EE(preserve)
        // AfterPreserve: EE = 1 (1 bit)
        n_bit_unsigned_integer::encode(&mut writer, 1, 1); // EE(lesscommon)
        // AfterLesscommon: SE(strict) = 1 (2 bits)
        n_bit_unsigned_integer::encode(&mut writer, 1, 2); // SE(strict)
        // AfterStrict: EE = 0 (0 bits)
        let data = writer.into_vec();
        let result = decode_from_slice(&data);
        assert_eq!(result.unwrap_err(), Error::InvalidOptionCombination);
    }

    /// Spec 5.4: Decode stream mit compression + byte-alignment → InvalidOptionCombination.
    #[test]
    fn decode_invalid_option_combination_compression_alignment() {
        // Manuell konstruierter Stream: SE(header) + SE(lesscommon) + SE(uncommon) + SE(alignment) + SE(byte) + EE + EE + SE(common) + SE(compression) + EE + EE
        let mut writer = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // SE(header)
        n_bit_unsigned_integer::encode(&mut writer, 0, 2); // SE(lesscommon)
        n_bit_unsigned_integer::encode(&mut writer, 0, 2); // SE(uncommon)
        n_bit_unsigned_integer::encode(&mut writer, 0, 3); // SE(alignment)
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // SE(byte)
        // AfterAlignment: EE = 4 (3 bits)
        n_bit_unsigned_integer::encode(&mut writer, 4, 3); // EE(uncommon)
        // AfterUncommon: EE = 2 (2 bits)
        n_bit_unsigned_integer::encode(&mut writer, 2, 2); // EE(lesscommon)
        // AfterLesscommon: SE(common) = 0 (2 bits)
        n_bit_unsigned_integer::encode(&mut writer, 0, 2); // SE(common)
        // CommonContent: SE(compression) = 0 (2 bits)
        n_bit_unsigned_integer::encode(&mut writer, 0, 2); // SE(compression)
        // AfterCompression: EE = 2 (2 bits)
        n_bit_unsigned_integer::encode(&mut writer, 2, 2); // EE(common)
        // AfterCommon: EE = 1 (1 bit)
        n_bit_unsigned_integer::encode(&mut writer, 1, 1); // EE(header)
        let data = writer.into_vec();
        let result = decode_from_slice(&data);
        assert_eq!(result.unwrap_err(), Error::InvalidOptionCombination);
    }

    /// Spec 5.4: schemaId mit EE ohne CH/AT → BuiltinOnly (leerer String).
    #[test]
    fn decode_schema_id_ee_without_content() {
        // Manuell konstruierter Stream: SE(header) + SE(common) + SE(schemaId) + EE + EE
        let mut writer = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // SE(header)
        n_bit_unsigned_integer::encode(&mut writer, 1, 2); // SE(common)
        n_bit_unsigned_integer::encode(&mut writer, 2, 2); // SE(schemaId)
        n_bit_unsigned_integer::encode(&mut writer, 1, 1); // prefix: not CH
        n_bit_unsigned_integer::encode(&mut writer, 1, 1); // EE (ohne CH/AT)
        // AfterCommon: EE = 1 (1 bit)
        n_bit_unsigned_integer::encode(&mut writer, 1, 1); // EE(common)
        let data = writer.into_vec();
        let result = decode_from_slice(&data).unwrap();
        assert_eq!(result.schema_id, Some(SchemaId::BuiltinOnly));
    }

    /// Spec 5.4: schemaId mit xsi:nil=true + CH → SchemaViolation.
    #[test]
    fn decode_schema_id_nil_true_with_ch() {
        let mut writer = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // SE(header)
        n_bit_unsigned_integer::encode(&mut writer, 1, 2); // SE(common)
        n_bit_unsigned_integer::encode(&mut writer, 2, 2); // SE(schemaId)
        n_bit_unsigned_integer::encode(&mut writer, 1, 1); // prefix: not CH
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // AT(xsi:nil)
        boolean::encode(&mut writer, true); // nil=true
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // CH (ungültig nach nil=true)
        let data = writer.into_vec();
        let result = decode_from_slice(&data);
        assert!(matches!(result.unwrap_err(), Error::SchemaViolation(_)));
    }

    /// Spec 5.4: schemaId mit xsi:nil=false + EE (ohne CH) → SchemaViolation.
    #[test]
    fn decode_schema_id_nil_false_without_ch() {
        let mut writer = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // SE(header)
        n_bit_unsigned_integer::encode(&mut writer, 1, 2); // SE(common)
        n_bit_unsigned_integer::encode(&mut writer, 2, 2); // SE(schemaId)
        n_bit_unsigned_integer::encode(&mut writer, 1, 1); // prefix: not CH
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // AT(xsi:nil)
        boolean::encode(&mut writer, false); // nil=false
        n_bit_unsigned_integer::encode(&mut writer, 1, 1); // EE (ohne CH, ungültig)
        let data = writer.into_vec();
        let result = decode_from_slice(&data);
        assert!(matches!(result.unwrap_err(), Error::SchemaViolation(_)));
    }

    /// Spec 7.1.6: unsignedInt > u32::MAX → IntegerOverflow.
    #[test]
    fn decode_unsigned_int_overflow() {
        // Manuell konstruierter Stream: SE(header) + SE(lesscommon) + SE(blockSize) + CH(u64::MAX)
        let mut writer = BitWriter::new();
        n_bit_unsigned_integer::encode(&mut writer, 0, 1); // SE(header)
        n_bit_unsigned_integer::encode(&mut writer, 0, 2); // SE(lesscommon)
        n_bit_unsigned_integer::encode(&mut writer, 2, 2); // SE(blockSize)
        // blockSize-Wert > u32::MAX (Unsigned Integer encoding für 0x1_0000_0000)
        unsigned_integer::encode(&mut writer, 0x1_0000_0000_u64);
        // Rest des Streams (EE für lesscommon und header)
        n_bit_unsigned_integer::encode(&mut writer, 2, 2); // AfterLesscommon: EE
        let data = writer.into_vec();
        let result = decode_from_slice(&data);
        assert_eq!(result.unwrap_err(), Error::IntegerOverflow);
    }

    /// Spec 7.4: DTRM mit mehreren Entries (xsd:decimal→exi:string, xsd:float→exi:integer).
    #[test]
    fn round_trip_datatype_representation_map_multiple() {
        use crate::qname::QName;
        let opts = ExiOptions {
            datatype_representation_map: vec![
                DatatypeRepresentationMapping {
                    type_qname: QName::new(XSD_NS, "decimal"),
                    representation_qname: QName::new(EXI_NS, "string"),
                },
                DatatypeRepresentationMapping {
                    type_qname: QName::new(XSD_NS, "float"),
                    representation_qname: QName::new(EXI_NS, "integer"),
                },
            ],

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 7.4: DTRM mit Custom-Namespace (nicht XSD/EXI).
    #[test]
    fn round_trip_datatype_representation_map_custom_ns() {
        use crate::qname::QName;
        let opts = ExiOptions {
            datatype_representation_map: vec![DatatypeRepresentationMapping {
                type_qname: QName::new("http://example.org/types", "myType"),
                representation_qname: QName::new(EXI_NS, "string"),
            }],

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 7.4: DTRM kombiniert mit alignment + valueMaxLength.
    #[test]
    fn round_trip_dtrm_with_alignment_and_value_max_length() {
        use crate::qname::QName;
        let opts = ExiOptions {
            alignment: Alignment::ByteAlignment,
            value_max_length: Some(1024),
            datatype_representation_map: vec![DatatypeRepresentationMapping {
                type_qname: QName::new(XSD_NS, "boolean"),
                representation_qname: QName::new(EXI_NS, "string"),
            }],

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 7.4: DTRM mit selfContained + valuePartitionCapacity.
    #[test]
    fn round_trip_dtrm_with_self_contained_and_vpc() {
        use crate::qname::QName;
        let opts = ExiOptions {
            self_contained: true,
            value_partition_capacity: Some(64),
            datatype_representation_map: vec![DatatypeRepresentationMapping {
                type_qname: QName::new(XSD_NS, "integer"),
                representation_qname: QName::new(EXI_NS, "string"),
            }],

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Spec 7.4: DTRM String-Table-Wiederverwendung (gleiche URI in mehreren Entries).
    #[test]
    fn round_trip_dtrm_string_table_reuse() {
        use crate::qname::QName;
        let opts = ExiOptions {
            datatype_representation_map: vec![
                DatatypeRepresentationMapping {
                    type_qname: QName::new(XSD_NS, "decimal"),
                    representation_qname: QName::new(EXI_NS, "string"),
                },
                DatatypeRepresentationMapping {
                    type_qname: QName::new(XSD_NS, "integer"),
                    representation_qname: QName::new(EXI_NS, "string"),
                },
                DatatypeRepresentationMapping {
                    type_qname: QName::new(XSD_NS, "boolean"),
                    representation_qname: QName::new(EXI_NS, "integer"),
                },
            ],

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    // ==================== Simple-Content Tests ====================

    /// Spec 7.1.6: valueMaxLength=1024 korrekt encodiert.
    #[test]
    fn value_max_length_encoding() {
        let opts = ExiOptions {
            value_max_length: Some(1024),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded.value_max_length, Some(1024));
    }

    /// Spec 7.1.6: blockSize=4096 korrekt encodiert.
    #[test]
    fn block_size_encoding() {
        let opts = ExiOptions {
            block_size: 4096,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded.block_size, 4096);
    }

    // ==================== Branch Coverage Tests ====================

    /// Nur lesscommon gesetzt (blockSize != default).
    #[test]
    fn only_lesscommon_block_size() {
        let opts = ExiOptions {
            block_size: 123456,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Nur uncommon gesetzt (selfContained).
    #[test]
    fn only_uncommon_self_contained() {
        let opts = ExiOptions {
            self_contained: true,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: alignment + selfContained.
    #[test]
    fn uncommon_alignment_and_self_contained() {
        let opts = ExiOptions {
            alignment: Alignment::ByteAlignment,

            self_contained: true,
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: alignment + valueMaxLength.
    #[test]
    fn uncommon_alignment_and_value_max_length() {
        let opts = ExiOptions {
            alignment: Alignment::PreCompression,

            value_max_length: Some(512),
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: selfContained + valueMaxLength + valuePartitionCapacity.
    #[test]
    fn uncommon_multiple_options() {
        let opts = ExiOptions {
            self_contained: true,
            value_max_length: Some(256),
            value_partition_capacity: Some(128),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: preserve mit blockSize.
    #[test]
    fn preserve_with_block_size() {
        let opts = ExiOptions {
            preserve: Preserve {
                comments: true,
                ..Default::default()
            },
            block_size: 999999,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: uncommon + preserve + blockSize.
    #[test]
    fn uncommon_preserve_and_block_size() {
        let opts = ExiOptions {
            alignment: Alignment::ByteAlignment,

            preserve: Preserve {
                dtd: true,
                pis: true,
                ..Default::default()
            },
            block_size: 50000,
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: compression + fragment.
    #[test]
    fn common_compression_and_fragment() {
        let opts = ExiOptions {
            compression: true,

            fragment: true,
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: compression + fragment + schemaId.
    #[test]
    fn common_all_options() {
        let opts = ExiOptions {
            compression: true,

            fragment: true,
            schema_id: Some(SchemaId::Id("urn:test".to_string())),
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: compression + schemaId (ohne fragment).
    #[test]
    fn common_compression_and_schema_id() {
        let opts = ExiOptions {
            compression: true,

            schema_id: Some(SchemaId::BuiltinOnly),
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: fragment + schemaId (ohne compression).
    #[test]
    fn common_fragment_and_schema_id() {
        let opts = ExiOptions {
            fragment: true,
            schema_id: Some(SchemaId::None),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: lesscommon + common.
    #[test]
    fn lesscommon_and_common() {
        let opts = ExiOptions {
            alignment: Alignment::ByteAlignment,

            fragment: true,
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: lesscommon + common + strict.
    #[test]
    fn lesscommon_common_and_strict() {
        let opts = ExiOptions {
            preserve: Preserve {
                lexical_values: true,
                ..Default::default()
            },
            compression: true,
            strict: true,
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: lesscommon + strict (ohne common).
    #[test]
    fn lesscommon_and_strict() {
        let opts = ExiOptions {
            preserve: Preserve {
                lexical_values: true,
                ..Default::default()
            },
            strict: true,
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Kombination: common + strict.
    #[test]
    fn common_and_strict() {
        let opts = ExiOptions {
            compression: true,
            strict: true,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    // ==================== Helper Function Tests ====================

    #[test]
    fn test_needs_lesscommon() {
        let default = ExiOptions::default();
        assert!(!needs_lesscommon(&default));

        let with_uncommon = ExiOptions {
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };
        assert!(needs_lesscommon(&with_uncommon));

        let with_preserve = ExiOptions {
            preserve: Preserve {
                comments: true,
                ..Default::default()
            },

            ..Default::default()
        };
        assert!(needs_lesscommon(&with_preserve));

        let with_blocksize = ExiOptions {
            block_size: 500_000,

            ..Default::default()
        };
        assert!(needs_lesscommon(&with_blocksize));
    }

    #[test]
    fn test_needs_uncommon() {
        let default = ExiOptions::default();
        assert!(!needs_uncommon(&default));

        let with_alignment = ExiOptions {
            alignment: Alignment::ByteAlignment,

            ..Default::default()
        };
        assert!(needs_uncommon(&with_alignment));

        let with_self_contained = ExiOptions {
            self_contained: true,

            ..Default::default()
        };
        assert!(needs_uncommon(&with_self_contained));

        let with_value_max = ExiOptions {
            value_max_length: Some(100),

            ..Default::default()
        };
        assert!(needs_uncommon(&with_value_max));

        let with_value_part = ExiOptions {
            value_partition_capacity: Some(100),

            ..Default::default()
        };
        assert!(needs_uncommon(&with_value_part));
    }

    #[test]
    fn test_needs_preserve() {
        let default = ExiOptions::default();
        assert!(!needs_preserve(&default));

        for test_case in [
            Preserve {
                dtd: true,
                ..Default::default()
            },
            Preserve {
                prefixes: true,
                ..Default::default()
            },
            Preserve {
                lexical_values: true,
                ..Default::default()
            },
            Preserve {
                comments: true,
                ..Default::default()
            },
            Preserve {
                pis: true,
                ..Default::default()
            },
        ] {
            let opts = ExiOptions {
                preserve: test_case,
    
                ..Default::default()
            };
            assert!(needs_preserve(&opts));
        }
    }

    #[test]
    fn test_needs_common() {
        let default = ExiOptions::default();
        assert!(!needs_common(&default));

        let with_compression = ExiOptions {
            compression: true,

            ..Default::default()
        };
        assert!(needs_common(&with_compression));

        let with_fragment = ExiOptions {
            fragment: true,

            ..Default::default()
        };
        assert!(needs_common(&with_fragment));

        let with_schema_id = ExiOptions {
            schema_id: Some(SchemaId::None),

            ..Default::default()
        };
        assert!(needs_common(&with_schema_id));
    }

    // ==================== Komplexe Kombinationen ====================

    /// Alle Optionen (außer strict-inkompatible) aktiviert.
    #[test]
    fn round_trip_many_options() {
        let opts = ExiOptions {
            alignment: Alignment::ByteAlignment,

            self_contained: true,
            value_max_length: Some(2048),
            value_partition_capacity: Some(1024),
            preserve: Preserve {
                dtd: true,
                prefixes: true,
                lexical_values: true,
                comments: true,
                pis: true,
                ..Default::default()
            },
            block_size: 100_000,
            fragment: true,
            schema_id: Some(SchemaId::Id("urn:complex:test".to_string())),
            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Große Werte für numerische Optionen.
    #[test]
    fn round_trip_large_values() {
        let opts = ExiOptions {
            value_max_length: Some(u32::MAX),
            value_partition_capacity: Some(u32::MAX - 1),
            block_size: u32::MAX / 2,

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// Unicode schemaId.
    #[test]
    fn round_trip_unicode_schema_id() {
        let opts = ExiOptions {
            schema_id: Some(SchemaId::Id("urn:日本語:スキーマ".to_string())),

            ..Default::default()
        };
        let encoded = encode_to_vec(&opts).unwrap();
        let decoded = decode_from_slice(&encoded).unwrap();
        assert_eq!(decoded, opts);
    }

    /// DoS-Schutz: schemaId exakt am Limit (MAX_SCHEMA_ID_LENGTH) wird akzeptiert.
    #[test]
    fn schema_id_exact_limit_ok() {
        let mut writer = BitWriter::new();
        // CommonContent: SE(schemaId) = 2 (2 bits)
        n_bit_unsigned_integer::encode(&mut writer, 2, 2);
        // SchemaIdContent: CH = 0 (1 bit)
        n_bit_unsigned_integer::encode(&mut writer, 0, 1);
        // String mit exakt MAX_SCHEMA_ID_LENGTH Zeichen (Value-Encoding: length+2)
        unsigned_integer::encode(&mut writer, MAX_SCHEMA_ID_LENGTH as u64 + 2);
        for _ in 0..MAX_SCHEMA_ID_LENGTH {
            unsigned_integer::encode(&mut writer, 'A' as u64);
        }

        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);
        let mut opts = ExiOptions::default();
        let result = decode_common_content(&mut reader, &mut opts);
        assert!(result.is_ok(), "Exakt am Limit sollte OK sein: {:?}", result);
        assert!(matches!(opts.schema_id(), Some(SchemaId::Id(_))));
    }

    /// DoS-Schutz: schemaId-String darf MAX_SCHEMA_ID_LENGTH nicht überschreiten.
    #[test]
    fn schema_id_length_limit() {
        // Erstelle einen Stream mit schemaId-String > 64 KiB
        let mut writer = BitWriter::new();
        // CommonContent: SE(schemaId) = 2 (2 bits)
        n_bit_unsigned_integer::encode(&mut writer, 2, 2);
        // SchemaIdContent: CH = 0 (1 bit)
        n_bit_unsigned_integer::encode(&mut writer, 0, 1);
        // String mit 65537 'A'-Zeichen (> MAX_SCHEMA_ID_LENGTH)
        let huge_len = MAX_SCHEMA_ID_LENGTH + 1;
        unsigned_integer::encode(&mut writer, huge_len as u64 + 2);
        for _ in 0..huge_len {
            unsigned_integer::encode(&mut writer, 'A' as u64);
        }

        let data = writer.into_vec();
        let mut reader = BitReader::new(&data);
        // Decode CommonContent
        let mut opts = ExiOptions::default();
        let result = decode_common_content(&mut reader, &mut opts);
        assert!(
            matches!(result, Err(Error::StringLengthExceeded { .. })),
            "Erwartete StringLengthExceeded, bekam: {:?}",
            result
        );
    }
}
