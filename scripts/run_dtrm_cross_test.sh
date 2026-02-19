#!/usr/bin/env bash
# DTRM Cross-RTT-Test: erxi vs Exificient
#
# Fuer jede DTRM-Fixture × Alignment:
# 1. erxi encode → erxi decode (Self-RTT)
# 2. Exificient encode → Exificient decode (Self-RTT)
# 3. erxi encode → Exificient decode (Cross-decode)
# 4. Byte-Vergleich erxi vs Exificient

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

: "${EXIFICIENT_JAR:?EXIFICIENT_JAR muss gesetzt sein (Pfad zur Exificient JAR)}"
DTRM_DIR="$REPO_DIR/tests/fixtures/dtrm"
SCHEMA="$DTRM_DIR/dtrm.xsd"
WORK_DIR="$REPO_DIR/target/dtrm_cross"
BINARY="$REPO_DIR/target/debug/examples/dtrm_cross_test"

mkdir -p "$WORK_DIR"

# Build
echo "Baue dtrm_cross_test..."
cargo build --example dtrm_cross_test 2>/dev/null
echo "  OK"

FIXTURES="dtrm-01 dtrm-02 dtrm-03 dtrm-04 dtrm-05 dtrm-06 dtrm-07 dtrm-08 dtrm-09 dtrm-10"
ALIGNMENTS="bitpacked bytealigned precompression compression"

total=0
ok=0
fail=0
skip=0

for fixture in $FIXTURES; do
    for alignment in $ALIGNMENTS; do
        tag="${fixture}_${alignment}"
        "$BINARY" "$fixture" "$alignment" 2>/dev/null
        rc=$?
        total=$((total + 1))
        if [ $rc -eq 0 ]; then
            ok=$((ok + 1))
        else
            fail=$((fail + 1))
        fi
    done
done

echo ""
echo "=== DTRM Cross-RTT Ergebnis ==="
echo "Total: $total  OK: $ok  FAIL: $fail  SKIP: $skip"
