# Changelog

## 0.1.0 — Erstveroeffentlichung

Vollstaendige EXI 1.0 (W3C Second Edition) Implementierung in Rust.

### Encoder / Decoder

- Schema-less und schema-informed Encoding/Decoding
- Alle Alignment-Modi: BitPacked, ByteAlignment, PreCompression, Compression (DEFLATE)
- Strict-Modus und Fragment-Modus
- Fidelity-Optionen: Comments, PIs, DTD, Prefixes, Lexical Values
- String Table mit konfigurierbaren Partitionen (valueMaxLength, valuePartitionCapacity)
- Self-Contained Elements
- Datatype Representation Mapping (DTRM)
- Streaming-Encode fuer grosse Dateien (ohne Zwischen-Vec)
- Iterator-basiertes Decoding
- Parallele DEFLATE-Kompression fuer grosse Streams

### Datentypen (Spec 7)

- Alle Built-in EXI Datatype Representations: Boolean, Integer, Unsigned Integer,
  n-Bit Integer, Float, Decimal, String, Binary, DateTime (8 Varianten),
  QName, List, Enumeration
- Restricted Character Sets (Appendix E)

### Grammar-System (Spec 8)

- Built-in XML Grammars (Document, Fragment, Element, StartTagContent, etc.)
- Schema-informed Grammars mit Pruning und Augmentierung
- Undeclared Productions (Spec 8.5.4.4)
- Event-Code-Berechnung und -Encoding

### Header und Options (Spec 5)

- EXI Header (Cookie, Distinguishing Bits, Version)
- Options Encoding/Decoding (Appendix C Schema)
- Initial String Table Entries (Appendix D)

### Schema-Parser

- XSD-Parser (XSD → SchemaInfo) mit xs:import-Unterstuetzung
- Unterstuetzte XSD-Konstrukte: complexType, simpleType, element, attribute,
  group, attributeGroup, sequence, choice, all, extension, restriction,
  substitutionGroup, enumeration, pattern, list, union

### CLI

- `erxi encode` / `erxi decode` mit allen Optionen
- stdin/stdout-Unterstuetzung
- Optionales zlib-ng Backend und mimalloc Allocator (Feature `cli-fast`)

### Tests

- Cross-Implementierungs-Tests gegen Exificient (8030 Kombinationen)
- W3C EXI Test Suite Interop-Tests
- Conformance-Tests (Spec Section 10)
- Infoset Round-Trip-Tests (Appendix B)
- DTRM Round-Trip-Tests
