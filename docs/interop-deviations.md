# Interop Deviations (erxi vs Exificient)

This document lists intentional deviations from the EXI spec that are required
for Exificient interoperability.

## 1. Whitespace in element-only content

**Spec:** 8.5.4.1.3.2 -- insignificant whitespace may be filtered.

**Exificient behavior:** preserves whitespace due to a stale `lastEvent` bug.

**erxi behavior:** keeps whitespace to match Exificient in interop runs.

---

## 2. ElementFragment QName ordering

**Spec:** 8.5.4.1.6 -- order by (local-name, uri).

**Exificient behavior:** orders by (uri, local-name).

**erxi behavior:** tolerates both orders in decoding, and aligns encoding
when running interop tests.

---

## 3. AT(*) in content after xsi:nil

**Spec:** attributes must be in the StartTag.

**Exificient behavior:** sometimes emits AT(*) in content.

**erxi behavior:** accepts AT(*) in content for schema-informed decoding.

---

## 4. Tier-2 event code handling

**Spec:** Tier-2 event codes are defined for schema-informed grammars.

**Exificient behavior:** uses a variant that differs in edge cases.

**erxi behavior:** tolerates Exificient's Tier-2 ordering and escape codes.
