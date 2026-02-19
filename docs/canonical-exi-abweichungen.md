# Deviation Report: erxi vs Canonical EXI (W3C Recommendation, June 7, 2018)

Spec: https://www.w3.org/TR/exi-c14n/ (local: `spec/exi-c14n.txt`)

## Summary

erxi is an EXI 1.0 library, not a dedicated Canonical EXI processor.
Most normative Canonical EXI rules are therefore **not implemented**,
because erxi does not provide a Canonical EXI mode.

| Status | Count |
|--------|-------|
| CONFORMANT | 14 |
| DEVIATION | 4 |
| NOT IMPLEMENTED | 17 |
| NOT APPLICABLE | 4 |

## Deviations (action required)

### 1. blockSize without compression (Sec. 3, item 2)

> "The element blockSize MUST be omitted if neither compression nor pre-compress is present."

`options_codec.rs` encodes blockSize even when neither compression nor pre-compress is enabled,
as long as the value differs from the default (1000000).

### 2. Decimal -0 (Sec. 4.5.3)

> "The sign value MUST be zero (0) if both the integral portion and the fractional portion
> of the Decimal value are 0 (zero)."

`decimal.rs` allows `negative=true` for integral=0, fractional=0.
The low-level API does not enforce canonicalization.

### 3. Hour 24 (Sec. 4.5.5, item 1)

> "The Hour value used to compute the Time component MUST NOT be 24."

`datetime.rs` allows hour=24 (when minute=0, second=0).
C14N requires normalization to 00:00:00 on the following day.

### 4. Attribute ordering in schema-less mode (Sec. 4.4)

> "Attributes [...] MUST be sorted lexicographically, first by qname local-name then by qname uri."

Attributes are only sorted in schema-informed encoding.
In schema-less encoding they remain in XML document order.

## Conformant Rules (14)

| Rule | Spec | Description |
|------|------|-------------|
| C-HDR-2 | Sec. 3 | Padding bits are always 0 (`BitWriter::align_to_byte`) |
| C-HDR-6 | Sec. 3, item 1 | Default blockSize (1000000) is omitted |
| C-HDR-14 | Sec. 3, item 6 | Empty container elements are omitted |
| C-ALIGN-2 | Sec. 4.1 | Padding bits in streams are 0 |
| C-WS-1 | Sec. 4.3.2 | XML information items are not modified |
| C-WS-4 | Sec. 4.3.2.1 | Whitespace in schema-less is preserved |
| C-ORD-1 | Sec. 4.4 | Input order is not modified |
| C-ORD-3/4 | Sec. 4.4 | xsi:type/xsi:nil before other AT (schema-informed) |
| C-UINT-1 | Sec. 4.5.1 | Unsigned Integer up to u64::MAX supported |
| C-FLT-1/2 | Sec. 4.5.4 | No -0 for mantissa/exponent (i64 has no -0) |
| C-FLT-5 | Sec. 4.5.4 | NaN: mantissa=0 for special exponent |
| C-STR-1 | Sec. 4.5.6 | Code points are not modified |
| C-STR-2/3 | Sec. 4.5.6 | Compact Identifier: local before global |
| C-RCS-1/2 | Sec. 4.5.7 | Restricted Character Sets encoded correctly |

## Not Implemented Rules (17)

### Canonical EXI Options System

| Rule | Spec | Description |
|------|------|-------------|
| C-HDR-1 | Sec. 3 | Cookie must be omitted |
| C-HDR-3 | Sec. 3 | omitOptionsDocument=true -> presence bit=0 |
| C-HDR-4 | Sec. 3 | omitOptionsDocument=false -> presence bit=1 |
| C-HDR-5 | Sec. 3 | Options as Canonical EXI body |
| C-HDR-8 | Sec. 3, item 3 | compression -> pre-compress substitution |
| C-HDR-9 | Sec. 3, item 4 | lexicalValues=true -> DTRM omitted |
| C-HDR-10 | Sec. 3, item 4 | DTRM tuples sorted lexicographically |
| C-HDR-11 | Sec. 3, item 4 | Default DTRM mappings omitted |

### Event Selection and Content Handling

| Rule | Spec | Description |
|------|------|-------------|
| C-EVT-1 | Sec. 4.2 | Two-stage event selection algorithm |
| C-EVT-3 | Sec. 4.2.2 | Choose most precise event (SE/CH/AT hierarchy) |
| C-CONT-1 | Sec. 4.3.1 | Filter extraneous CH("") events |
| C-CONT-2 | Sec. 4.3.1 | Merge consecutive CH events |
| C-WS-2 | Sec. 4.3.2 | Respect xml:space="preserve" |
| C-WS-5 | Sec. 4.3.2.2 | Remove whitespace-only nodes in complex data |
| C-ORD-2 | Sec. 4.4 | Sort NS declarations by prefix |

### Datatype Canonicalization

| Rule | Spec | Description |
|------|------|-------------|
| C-FLT-3 | Sec. 4.5.4 | mantissa=0 -> exponent must be 0 |
| C-FLT-4 | Sec. 4.5.4 | No trailing zeros in mantissa |
| C-ENUM-1 | Sec. 4.5.2 | First index for duplicates |
| C-DT-2 | Sec. 4.5.5 | FractionalSecs=0 -> omitted (not enforced) |
| C-DT-3 | Sec. 4.5.5 | utcTime mode for UTC normalization |
| C-STR-4 | Sec. 4.5.6 | whiteSpace facet for canonical purposes |

## Not Applicable Rules (4)

| Rule | Spec | Reason |
|------|------|--------|
| C-HDR-12 | Sec. 3, item 5 | No user-defined metadata |
| C-HDR-13 | Sec. 3, item 5 | EXI profile not supported |
| C-EVT-2 | Sec. 4.2.1, Note | EXI profile not supported |
| C-DTRM-1 | Sec. 4.5.8 | No rules defined for alternative representations |
