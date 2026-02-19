#!/usr/bin/env bash
# Full cross RTT/interop matrix runner.
# Output: target/cross_rtt_full/results.tsv
# Usage: ./scripts/run.sh [--skip-build]

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

: "${EXI_TESTSUITE_DIR:?EXI_TESTSUITE_DIR must be set (run scripts/setup.sh)}"
: "${EXIFICIENT_JAR:?EXIFICIENT_JAR must be set (run scripts/setup.sh)}"

DECLARED_DIR="$EXI_TESTSUITE_DIR/data/interop/schemaInformedGrammar/declaredProductions"
UNDECLARED_DIR="$EXI_TESTSUITE_DIR/data/interop/schemaInformedGrammar/undeclaredProductions"
ALIGNMENTS="bitpacked bytealigned precompression compression strict"
INF_ALIGNMENTS="bitpacked bytealigned precompression compression"
DTRM_ALIGNMENTS="bitpacked bytealigned precompression compression"
RESULTS_DIR="$ROOT/target/cross_rtt_full"
RESULTS_FILE="$RESULTS_DIR/results.tsv"
BATCH_FILE="$RESULTS_DIR/batch.txt"
BINARY="$ROOT/target/debug/examples/full_cross_test"
STDERR_LOG="$RESULTS_DIR/exificient_stderr.log"

if [[ "${1:-}" != "--skip-build" ]]; then
  echo "Building full_cross_test + erxi..."
  build_start=$SECONDS
  cargo build --quiet --example full_cross_test --bin erxi
  echo "  Build: $((SECONDS - build_start))s"
fi

mkdir -p "$RESULTS_DIR"
if [[ "${EXI_LOG_STDERR:-1}" == "1" ]]; then
  : > "$STDERR_LOG"
fi

if [[ -f "$RESULTS_FILE" ]]; then
  ts=$(date -r "$RESULTS_FILE" +%Y%m%d_%H%M%S)
  mv "$RESULTS_FILE" "${RESULTS_FILE%.tsv}_${ts}.tsv"
fi

printf "fixture\ttest\tresult\tdetail\n" > "$RESULTS_FILE"
: > "$BATCH_FILE"

# Declared Fixtures
for xml in "$DECLARED_DIR"/*.xml; do
  bname=$(basename "$xml" .xml)
  for alignment in $ALIGNMENTS; do
    printf "declared %s %s\n" "$bname" "$alignment" >> "$BATCH_FILE"
  done
done

# Undeclared Fixtures
for xml in "$UNDECLARED_DIR"/*.xml; do
  bname=$(basename "$xml" .xml)
  [[ "$bname" == "er-entity" ]] && continue
  for alignment in $ALIGNMENTS; do
    printf "undeclared %s %s\n" "$bname" "$alignment" >> "$BATCH_FILE"
  done
done

# DTRM Fixtures (no strict)
for xml in "$ROOT"/tests/fixtures/dtrm/*.xml; do
  bname=$(basename "$xml" .xml)
  for alignment in $DTRM_ALIGNMENTS; do
    printf "dtrm %s %s\n" "$bname" "$alignment" >> "$BATCH_FILE"
  done
done

# EXI4JSON Fixtures (strict)
for json in "$ROOT"/tests/fixtures/json/*.json; do
  bname=$(basename "$json" .json)
  printf "exi4json %s strict\n" "$bname" >> "$BATCH_FILE"
done

# Infoset Fixtures (no strict)
for name in b1_document b2_element b3_attribute b4_pi b5_entity b6_character b7_comment b8_doctype b9_unparsed b10_notation b11_namespace; do
  for alignment in $INF_ALIGNMENTS; do
    printf "infoset %s %s\n" "$name" "$alignment" >> "$BATCH_FILE"
  done
done

# Coverage Fixtures (strict)
for name in schema_id_none schema_id_builtin schema_id_id alignment_matrix value_limits preserve_prefixes dtrm_schema_id; do
  printf "coverage %s strict\n" "$name" >> "$BATCH_FILE"
done

# CLI Fixtures (strict)
printf "cli no-include-options strict\n" >> "$BATCH_FILE"
printf "cli exif-to-erxi strict\n" >> "$BATCH_FILE"

# Execute batch
if [[ "${EXI_LOG_STDERR:-1}" == "1" ]]; then
  if [[ "${EXI_PROGRESS:-1}" == "1" ]]; then
    "$BINARY" --batch < "$BATCH_FILE" 2>>"$STDERR_LOG" | tee -a "$RESULTS_FILE"
  else
    "$BINARY" --batch < "$BATCH_FILE" 2>>"$STDERR_LOG" >> "$RESULTS_FILE"
  fi
else
  if [[ "${EXI_PROGRESS:-1}" == "1" ]]; then
    "$BINARY" --batch < "$BATCH_FILE" 2>/dev/null | tee -a "$RESULTS_FILE"
  else
    "$BINARY" --batch < "$BATCH_FILE" 2>/dev/null >> "$RESULTS_FILE"
  fi
fi

# Summary
ok_count=$(grep -c $'\tOK\t' "$RESULTS_FILE" || true)
fail_count=$(grep -c $'\tFAIL\t' "$RESULTS_FILE" || true)
skip_count=$(grep -c $'\tSKIP\t' "$RESULTS_FILE" || true)
error_count=$(grep -c $'\tERROR\t' "$RESULTS_FILE" || true)

cat <<SUMMARY

=== Result ===
File:   $RESULTS_FILE
OK:     $ok_count
FAIL:   $fail_count
SKIP:   $skip_count
ERROR:  $error_count
SUMMARY
