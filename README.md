# erxi

EXI 1.0 ([W3C Second Edition](https://www.w3.org/TR/exi/)) implementation in Rust.
Complete spec coverage as both a library and a CLI tool.

EXI (Efficient XML Interchange) is a binary XML format that encodes XML documents
compactly and fast--typically smaller than gzip and faster to parse than text XML.

## Features

- Complete EXI 1.0 Second Edition implementation
- Schema-less and schema-informed encoding/decoding
- All alignment modes: BitPacked, ByteAlignment, PreCompression, Compression (DEFLATE)
- Strict mode and Fragment mode
- Fidelity options: Comments, PIs, DTD, Prefixes, Lexical Values
- String Table with configurable partitions
- Self-Contained elements
- Datatype Representation Mapping
- Streaming encode for large files (no intermediate Vec)
- Iterator-based decoding
- XSD schema parser with xs:import support
- Parallel DEFLATE compression for large streams
- High test coverage (unit + interop + cross-RTT)

## Installation

```bash
# Build from the repository
cargo build --release

# Library-only (no CLI)
cargo build --release --no-default-features
```

### Cargo Features

| Feature | Default | Description |
|---------|---------|-------------|
| `cli` | yes | CLI binary (`erxi encode` / `erxi decode`) |
| `cli-fast` | yes | Faster CLI: zlib-ng + mimalloc |
| `fast-deflate` | yes | zlib-ng backend for DEFLATE |
| `fast-alloc` | yes | mimalloc allocator |

## CLI

### Encode

```bash
# Simple encoding (BitPacked, default)
erxi encode -i document.xml -o document.exi

# With schema
erxi encode -i document.xml -o document.exi -s schema.xsd

# With compression
erxi encode -i document.xml -o document.exi --compression

# Byte-aligned with options in header
erxi encode -i document.xml -o document.exi --byte-aligned --include-options

# All fidelity options
erxi encode -i document.xml -o document.exi \
  --preserve-comments --preserve-pis --preserve-dtd --preserve-prefixes

# Large files: parallel DEFLATE
 erxi encode -i large.xml -o large.exi --compression --parallel-deflate

# Read from stdin
cat document.xml | erxi encode -i - -o document.exi

# Auto output (document.xml -> document.exi)
erxi encode -i document.xml

# To stdout
erxi encode -i document.xml -o -
```

### Decode

```bash
# Auto output (document.exi -> document.xml)
erxi decode -i document.exi

# To file
erxi decode -i document.exi -o document.xml

# To stdout
erxi decode -i document.exi -o -

# With schema (when options are not in the header)
erxi decode -i document.exi -o document.xml -s schema.xsd

# Pretty-printed XML output
erxi decode -i document.exi --pretty
```

### JSON (EXI4JSON)

```bash
# Encode JSON -> EXI4JSON (output auto: input.json -> input.exi)
erxi json encode -i data.json

# Encode JSON -> EXI4JSON to stdout
erxi json encode -i data.json -o -

# Decode EXI4JSON -> JSON (output auto: input.exi -> input.json)
erxi json decode -i data.exi

# Decode EXI4JSON -> JSON to stdout
erxi json decode -i data.exi -o -

# Pretty-printed JSON output
erxi json decode -i data.exi --pretty

# Enable EXI4JSON <other> heuristics (date/time/base64/integer/decimal)
erxi json encode -i data.json --exi4json-other
```

### All CLI Options

| Option | Description |
|--------|-------------|
| `-i, --input <FILE>` | Input file (`-` for stdin) |
| `-o, --output <FILE>` | Output file (optional; without `-o` auto-derived, `-o -` = stdout) |
| `-s, --schema <FILE>` | XSD schema file |
| `--schema-id <ID>` | Schema ID in EXI header |
| `--pretty` | Pretty-printed Ausgabe (nur `decode` / `json decode`) |
| `--schema-id-none` | Schema ID = None (xsi:nil=true) |
| `--schema-id-builtin` | Schema ID = BuiltinOnly (empty string) |
| `--byte-aligned` | Byte alignment |
| `--pre-compression` | Pre-compression alignment |
| `--compression` | DEFLATE compression |
| `--strict` | Strict mode |
| `--fragment` | Fragment mode |
| `--preserve-comments` | Preserve comment events |
| `--preserve-pis` | Preserve processing instructions |
| `--preserve-dtd` | Preserve DOCTYPE / entity reference events |
| `--preserve-prefixes` | Preserve namespace prefixes |
| `--preserve-lexical` | Preserve lexical values |
| `--preserve-whitespace` | Preserve insignificant whitespace |
| `--self-contained` | Enable self-contained fragments |
| `--self-contained-qname <URI> <LOCAL>` | Self-contained only for specific elements (repeatable) |
| `--include-options` | Write options in EXI header |
| `--include-cookie` | Write "$EXI" cookie |
| `--parallel-deflate` | Parallel DEFLATE compression |
| `--block-size <N>` | Compression block size (default: 1,000,000) |
| `--value-max-length <N>` | String Table max value length |
| `--value-capacity <N>` | String Table partition capacity |
| `--dtrm <TYPE_URI> <TYPE_LOCAL> <REP_URI> <REP_LOCAL>` | Datatype Representation Map entry (repeatable) |

## Large Files

- Encode without Compression/PreCompression writes streaming output and flushes periodically (no growing output buffer).
- Decode from files uses memory mapping (feature `mmap`); stdin is read fully.

## Spec Coverage

erxi implements the full EXI 1.0 Second Edition specification:

| Spec Section | Module | Description |
|-------------|--------|-------------|
| 4 | event.rs | EXI event model (12 event types) |
| 5 | header.rs, options.rs, options_codec.rs | EXI header and options |
| 6 | encoder/, decoder/ | Encoding/decoding EXI streams |
| 7.1 | bitstream.rs, string.rs, integer.rs, ... | Built-in EXI datatypes |
| 7.2 | enumeration.rs | Enumerations |
| 7.3 | string_table.rs | String Table |
| 8.1-8.4 | grammar.rs, event_code.rs | Built-in XML grammars |
| 8.5 | grammar.rs, proto_grammar.rs, xsd/ | Schema-informed grammars |
| 9 | encoder/compression.rs, decoder/compression.rs | EXI compression |
| 10 | tests/conformance.rs | Conformance |
| Appendix B | tests/infoset_rtt.rs | Infoset mapping |
| Appendix C | options_codec.rs | Options header schema |
| Appendix D | string_table.rs | Initial String Table entries |
| Appendix E | rcs.rs | Restricted Character Sets |

Known deviations: [docs/interop-deviations.md](docs/interop-deviations.md)

## Tests

```bash
# Unit and integration tests
cargo test

# Regression test against cross-matrix expectations
./scripts/run_cross_matrix_test.sh

# Individual test suites
cargo test --test conformance       # Spec 10 conformance
cargo test --test w3c_interop       # W3C Test Suite
cargo test --test cross_rtt         # Cross-RTT with Exificient fixtures
cargo test --test infoset_rtt       # Infoset round-trip (Appendix B)
cargo test --test dtrm_rtt          # Datatype Representation Mapping
```

### Test Prerequisites

- **W3C EXI Test Suite**: `EXI_TESTSUITE_DIR` must be set (or placed under
  `/tmp/exi-testsuite/ttfms-interop-18122013`).
- **Exificient (Java)**: set `EXIFICIENT_JAR` or place JARs under
  `tests/fixtures/exificient/`. Java is required.

### Cross-Implementation Tests

erxi is tested against [Exificient](https://exificient.github.io/) (Java reference implementation).
The cross-matrix checks 8030 combinations of fixtures, alignments, and encode/decode directions.

Expected results: [tests/cross_matrix_expectations.tsv](tests/cross_matrix_expectations.tsv)

## Benchmarks

Benchmarks and profiling live in the separate `erxi-benchmark` repo.

## Documentation

| Document | Description |
|----------|-------------|
| [docs/architecture.md](docs/architecture.md) | Layer model, pipeline, module overview |
| [docs/interop-deviations.md](docs/interop-deviations.md) | Intentional spec deviations for Exificient interop |
| [docs/infoset-mapping.md](docs/infoset-mapping.md) | XML infoset mapping (Appendix B) |
| [docs/byte-diff-analyse.md](docs/byte-diff-analyse.md) | Byte-difference analysis vs Exificient |

## License

[PolyForm Noncommercial 1.0.0](LICENSE) -- free for non-commercial use
(research, education, hobby, public institutions). Commercial use
requires a separate license.
