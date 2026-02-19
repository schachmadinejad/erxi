# EXI for JSON (EXI4JSON) -- Implementation Analysis

Spec: https://www.w3.org/TR/exi-for-json/ (local: `spec/exi-for-json.txt`)

Status: W3C Working Group Note (July 26, 2018) -- not a Recommendation.

## Concept

EXI4JSON is a thin layer on top of schema-informed EXI:

1. JSON is mapped to a **fixed XML schema** (namespace `http://www.w3.org/2015/EXI/json`).
2. That XML is encoded using **schema-informed EXI** with `strict=true`.
3. Lossless roundtrip JSON -> EXI -> JSON.

## JSON-to-EXI Mapping

| JSON Type | EXI Event Sequence |
|----------|---------------------|
| Object `{}` | `SE(j:map) ... EE` |
| Key/Value | `SE(j:key) ... EE` (key = element name) |
| Array `[]` | `SE(j:array) ... EE` |
| String | `SE(j:string) CH(value) EE` |
| Number | `SE(j:number) CH(value) EE` |
| true | `SE(j:boolean) CH("true") EE` |
| false | `SE(j:boolean) CH("false") EE` |
| null | `SE(j:null) EE` |

### Optional Type Optimizations

Strings and numbers can be mapped to more specific types for better compression:

- String -> `SE(j:other) SE(j:base64Binary|j:dateTime|j:time|j:date) CH(value) EE EE`
- Number -> `SE(j:other) SE(j:integer|j:decimal) CH(value) EE EE`

### Key Name Escaping

Two cases:

1. **Non-NCName characters:** `_` + decimal code point + `.` (e.g. `"1 key"` -> `_49._32.key`)
2. **Reserved names** (map, array, string, number, boolean, null, other): prefix `_.` (e.g. `"map"` -> `_.map`)

## Fixed EXI Options

| Option | Value | Note |
|--------|-------|------|
| strict | true | Not changeable |
| schemaId | "exi4json" | Not changeable |

## What erxi already provides

- Schema-informed encoding/decoding with `strict=true`
- All required EXI datatypes
- XSD parsing
- String table, event codes, grammars

## What would need to be built

| Component | Estimated size | Description |
|-----------|----------------|-------------|
| Embed EXI4JSON XSD | ~50 lines | Schema as constant in erxi |
| JSON -> EXI events (encoder) | ~200-300 lines | JSON parser -> event stream |
| EXI events -> JSON (decoder) | ~200-300 lines | Event stream -> JSON output |
| Key name escaping | ~50 lines | NCName escaping + reserved names |
| Key name unescaping | ~50 lines | Reverse |
| Tests | ~200 lines | Roundtrip tests with spec examples |

**Total effort: ~1-2 days**, since the EXI infrastructure already exists.

## Dependencies

- JSON parsing: `serde_json` or manual parser
- No new EXI features required--only an adapter layer

## Spec Examples (for tests)

The spec contains three examples in Appendix D:

1. Simple object with string/number/boolean
2. Nested object with array
3. Key name escaping with special characters
