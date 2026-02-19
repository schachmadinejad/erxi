# Architecture

EXI 1.0 (W3C Second Edition) Rust library. ~59k lines in `src/`.

## Pipeline Overview

**Encode**

1. `xml.rs` parses XML into EXI events (`ExiEvent`).
2. `encoder/mod.rs` turns events into event codes and serializes to the bitstream.
3. `string_table.rs` manages compact string IDs (URI/local/prefix/value tables).
4. `bitstream.rs` writes aligned bits/bytes to the output.
5. With compression: `encoder/compression.rs` splits channels and applies DEFLATE.

**Decode**

1. `decoder/mod.rs` reads the EXI header and options.
2. Event codes are read and mapped to grammar productions.
3. String Table lookups reconstruct QNames and values.
4. `xml_serializer.rs` produces XML from events.
5. With compression: `decoder/compression.rs` inflates and decodes channels lazily.

## Layers

```
Layer 5  Encoder/Decoder    encoder/, decoder/
Layer 4  Grammar System     grammar.rs, proto_grammar.rs, undeclared.rs
Layer 3  Schema + Strings   schema.rs, xsd/, string_table.rs, typed_value.rs
Layer 2  Events + Options   event.rs, options.rs, options_codec.rs, header.rs
Layer 1  Datatypes          datetime.rs, qname.rs, float.rs, decimal.rs, ...
Layer 0  Bit I/O            bitstream.rs
```

## Tests and Scripts

| File | Purpose |
|------|---------|
| `examples/full_cross_test.rs` | Cross-RTT fixture test (10 tests per fixture) |
| `examples/dtrm_cross_test.rs` | DTRM (Datatype Representation Map) fixture test |
| `scripts/run_cross_matrix_test.sh` | Regression test against expected results |
| `scripts/run_full_cross_matrix.sh` | Raw cross-RTT matrix (data collection only) |
| `scripts/run_dtrm_cross_test.sh` | DTRM cross-RTT tests |
| `tests/cross_matrix_expectations.tsv` | Expected test results (non-OK entries; see file) |

## Interop Architecture

Earlier `ExiOptions.interop` switches were removed. Exificient compatibility is now
**always on**: relevant tolerances and workarounds are permanently enabled
(`decoder/mod.rs`, `encoder/mod.rs`, `grammar.rs`, `undeclared.rs`).

Examples of permanently enabled interop behavior:
- Tier-2 event code system
- AT(*)[untyped]
- Wildcard injection
- Premature EOS tolerance
