# Byte-Difference Analysis: erxi vs Exificient

## Results

| Suite | Self-RTT | Exif-decode | Byte-identical |
|-------|----------|-------------|----------------|
| Declared | 347/347 OK | 347/347 OK | 277/277 (100%) |
| Undeclared | 230/230 OK | 230/230 OK | 116/176 (66%) |

All fixtures are functionally correct (Self-RTT + Exificient-decode OK).
The 60 remaining byte differences fall into four categories.
In no case is there a functional bug--tests produce semantically identical XML.

---

## Remaining Byte Differences (60 fixture/alignment variants)

### 1. Whitespace filtering (27 variants, erxi smaller)

**Fixtures:** cm-01/02/03, pi-01/02/03, er-01/02/03 (each 3 alignments)

**Cause:** erxi filters insignificant whitespace in element-only content per spec
(Spec 8.5.4.1.3.2). Exificient preserves it due to a stale `lastEvent` bug.
Thus erxi is consistently smaller.

With `interop=true` the behavior is mirrored (WS preserved), but byte sizes
still differ--the WS positions do not fully match.

### 2. ElementFragment QName order (23 variants, mixed)

**Fixtures:** elementFragment-01..06 (3-4 alignments each incl. strict)

**Cause:** Exificient sorts fragment QNames as (uri, local-name), while
Spec 8.5.4.1.6 requires (local-name, uri). Additionally, Tier-2 ordering
differences for ElementFragment grammars. Mixed size outcomes.

### 3. AT(*) in content (6 variants, erxi smaller)

**Fixtures:** complexType-04, complexType-26 (3 alignments each)

**Cause:** Exificient encodes post-xsi:nil attributes in the content area
(AT(*) in content). erxi encodes correctly in the StartTag. erxi is smaller.

### 4. Self-contained grammar evolution (4 variants, erxi smaller)

**Fixtures:** sc-01, sc-02 (bitpacked + bytealigned)

**Cause:** Grammar evolution in SC fragments (Spec 8.5.4.4.1 + 8.4).
Exificient's two-tier event code system computes escape codes after evolution
differently than erxi's flat grammar system. erxi is smaller.

| Fixture | erxi | exif | Diff |
|---------|------|------|------|
| sc-01_bitpacked | 260 | 268 | -8 |
| sc-01_bytealigned | 350 | 356 | -6 |
| sc-02_bitpacked | 351 | 373 | -22 |
| sc-02_bytealigned | 511 | 552 | -41 |

---

## Assessment

All 60 remaining byte differences are due to known implementation differences.
Full functional interop is preserved. The expected byte sizes are recorded in
`tests/cross_matrix_expectations.tsv` as `DIFF:erxi:exif`.
