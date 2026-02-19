#!/usr/bin/env bash
# Vollstaendige Cross-RTT-Matrix: Alle Fixtures, alle Alignments, alle 10 Tests.
#
# Ausgabe: target/cross_rtt_full/results.tsv
# Nutzung: ./scripts/run_full_cross_matrix.sh [--skip-build]

set -euo pipefail

if [[ -z "${EXI_TESTSUITE_DIR:-}" ]]; then
    if [[ -d "/tmp/exi-testsuite/ttfms-interop-18122013" ]]; then
        EXI_TESTSUITE_DIR="/tmp/exi-testsuite/ttfms-interop-18122013"
    else
        echo "EXI_TESTSUITE_DIR fehlt. Erwartet W3C EXI Test Suite unter /tmp/exi-testsuite/ttfms-interop-18122013" >&2
        echo "Alternativ: EXI_TESTSUITE_DIR setzen (Pfad zur W3C EXI Test Suite)." >&2
        exit 1
    fi
fi
DECLARED_DIR="$EXI_TESTSUITE_DIR/data/interop/schemaInformedGrammar/declaredProductions"
UNDECLARED_DIR="$EXI_TESTSUITE_DIR/data/interop/schemaInformedGrammar/undeclaredProductions"
ALIGNMENTS="bitpacked bytealigned precompression compression strict"
RESULTS_DIR="target/cross_rtt_full"
RESULTS_FILE="$RESULTS_DIR/results.tsv"
BATCH_FILE="$RESULTS_DIR/batch.txt"
BINARY="./target/debug/examples/full_cross_test"
STDERR_LOG="$RESULTS_DIR/exificient_stderr.log"

total_start=$SECONDS

# Build (sofern nicht --skip-build)
if [[ "${1:-}" != "--skip-build" ]]; then
    echo "Baue full_cross_test..."
    build_start=$SECONDS
    cargo build --quiet --example full_cross_test
    echo "  Build: $((SECONDS - build_start))s"
fi

mkdir -p "$RESULTS_DIR"
if [[ "${EXI_LOG_STDERR:-1}" == "1" ]]; then
    : > "$STDERR_LOG"
fi

# Alten Lauf sichern
if [[ -f "$RESULTS_FILE" ]]; then
    ts=$(date -r "$RESULTS_FILE" +%Y%m%d_%H%M%S)
    mv "$RESULTS_FILE" "${RESULTS_FILE%.tsv}_${ts}.tsv"
fi

# Header
printf "fixture\ttest\tresult\tdetail\n" > "$RESULTS_FILE"

# Batch-Datei vorbereiten
: > "$BATCH_FILE"

# Declared Fixtures
echo "Declared Fixtures..."
declared_start=$SECONDS
for xml in "$DECLARED_DIR"/*.xml; do
    basename=$(basename "$xml" .xml)
    for alignment in $ALIGNMENTS; do
        printf "declared %s %s\n" "$basename" "$alignment" >> "$BATCH_FILE"
    done
done
declared_time=$((SECONDS - declared_start))
echo "  Declared: ${declared_time}s"

# Undeclared Fixtures
echo "Undeclared Fixtures..."
undeclared_start=$SECONDS
for xml in "$UNDECLARED_DIR"/*.xml; do
    basename=$(basename "$xml" .xml)
    # Datendateien (keine eigenstaendigen Fixtures) ueberspringen
    [[ "$basename" == "er-entity" ]] && continue
    for alignment in $ALIGNMENTS; do
        printf "undeclared %s %s\n" "$basename" "$alignment" >> "$BATCH_FILE"
    done
done
undeclared_time=$((SECONDS - undeclared_start))
echo "  Undeclared: ${undeclared_time}s"

# Batch ausfuehren (eine JVM pro Run)
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

total_time=$((SECONDS - total_start))

# Zusammenfassung
total_lines=$(wc -l < "$RESULTS_FILE")
total_lines=$((total_lines - 1))  # Header abziehen

ok_count=$(grep -c $'\tOK\t' "$RESULTS_FILE" || true)
fail_count=$(grep -c $'\tFAIL\t' "$RESULTS_FILE" || true)
skip_count=$(grep -c $'\tSKIP\t' "$RESULTS_FILE" || true)
error_count=$(grep -c $'\tERROR\t' "$RESULTS_FILE" || true)

echo ""
echo "=== Ergebnis ==="
echo "Datei:   $RESULTS_FILE"
echo "Zeilen:  $total_lines"
echo "OK:      $ok_count"
echo "FAIL:    $fail_count"
echo "SKIP:    $skip_count"
echo "ERROR:   $error_count"
echo ""
echo "=== Zeitmessung ==="
echo "Declared:   ${declared_time}s"
echo "Undeclared: ${undeclared_time}s"
echo "Gesamt:     ${total_time}s"
echo ""

# Top-Failures
if [[ $fail_count -gt 0 ]]; then
    echo "=== Top FAIL-Tests ==="
    grep $'\tFAIL\t' "$RESULTS_FILE" | cut -f2 | sort | uniq -c | sort -rn | head -10
    echo ""
fi
