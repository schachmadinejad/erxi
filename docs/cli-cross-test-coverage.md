# CLI Cross-Test Coverage

This document shows which `erxi` CLI flags are covered by the full cross suite
and where gaps remain.

References:
- Full cross runner: `examples/full_cross_test.rs`
- Cross script: `scripts/run.sh`
- CLI: `src/bin/erxi.rs`

## Important Difference

The full cross suite primarily uses library APIs (`encode_with_schema`/`decode_with_schema`),
not the CLI parser.

That means:
- Good coverage for format/interop behavior.
- No complete coverage for CLI flag parsing and CLI-specific guards.

Additionally there is a dedicated CLI E2E suite:
- `tests/cli_e2e.rs`

CLI cross-interop checks are part of the full cross matrix (CLI fixtures in
`examples/full_cross_test.rs`).

## Coverage by CLI Flag

- `--byte-aligned`: Indirectly covered (alignment `bytealigned` in full cross).
- `--pre-compression`: Indirectly covered (alignment `precompression` in full cross).
- `--compression`: Indirectly covered (alignment `compression` in full cross).
- `--strict`: Indirectly covered (alignment `strict` in full cross).
- `--fragment`: Indirectly covered (fixture-based in full cross).
- `--preserve-comments`: Indirectly covered (fixture-based in full cross).
- `--preserve-pis`: Indirectly covered (fixture-based in full cross).
- `--preserve-dtd`: Indirectly covered (fixture-based in full cross).
- `--preserve-prefixes`: Indirectly covered (fixture-based in full cross).
- `--self-contained`: Indirectly covered (sc fixtures in full cross).
- `--self-contained-qname`: Indirectly covered (fixed QName in full cross).
- `--dtrm`: Indirectly covered (DTRM suite in full cross).

- `--schema-id`: Not used in full cross; tested in CLI unit/E2E.
- `--schema-id-none`: Not used in full cross; tested in CLI E2E.
- `--schema-id-builtin`: Not used in full cross; tested in CLI E2E.
- `--preserve-lexical`: Not used in full cross; tested in CLI unit/E2E.
- `--preserve-whitespace`: Not used in full cross; tested in CLI unit/E2E.
- `--block-size`: Not parameterized in full cross; tested in CLI unit/E2E.
- `--value-max-length`: Not parameterized in full cross; tested in CLI unit/E2E.
- `--value-capacity`: Not parameterized in full cross; tested in CLI unit/E2E.
- `--parallel-deflate`: Not in full cross; tested in CLI E2E.
- `--include-options`: Not in full cross; tested in CLI E2E.
- `--no-include-options`: Covered via CLI fixture in full cross (out-of-band options).
- `--include-cookie`: Not in full cross; tested in CLI E2E.
- `--no-memory-monitor`: Not in full cross; tested in CLI E2E.

## Fixture Coverage (XML/XSD)

- For schema-informed grammar/interop behavior: high (W3C declared/undeclared + DTRM).
- For full CLI options: incomplete.

Gaps remain for deep combinations (cross-product of flags with large/W3C fixtures),
but the core paths are now covered by CLI unit and CLI E2E tests.

## Existing CLI Tests

Unit tests in `src/bin/erxi.rs` cover:
- Flag conflicts (`schema-id` variants),
- Mapping `self-contained-qname`,
- Mapping `schema-id` + `dtrm`,
- Mapping `block-size`, `value-max-length`, `value-capacity`,
- Mapping `preserve-lexical` + `preserve-whitespace`,
- Guards for `schema-id`/`dtrm` without `--schema` in `encode`/`decode`.

E2E tests in `tests/cli_e2e.rs` cover:
- `--include-cookie` + decode roundtrip,
- Auto header options for `--byte-aligned`,
- `--no-include-options` including out-of-band decode requirement,
- `--parallel-deflate` constraint (`requires --compression`),
- `--parallel-deflate` + `--no-memory-monitor` roundtrip,
- `--schema-id-none`/`--schema-id-builtin` incompatibility with `--schema`,
- Roundtrip with `--preserve-lexical`, `--preserve-whitespace`, `--block-size`,
  `--value-max-length`, `--value-capacity`.

Cross checks in the full cross matrix cover:
- `erxi` CLI encode (`--byte-aligned --no-include-options`) -> Exificient decode (out-of-band options),
- Exificient encode (schema + compression + prefixes) -> `erxi` CLI decode.

## Next Reasonable Steps

- If desired: matrix-style CLI E2E tests over multiple fixture classes
  (small, large, schema-informed, precompression/compression) to catch
  interaction bugs between many flags early.
