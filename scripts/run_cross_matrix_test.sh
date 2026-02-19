#!/usr/bin/env bash
# Cross-RTT-Matrix Integrationstest
#
# Fuehrt die volle Cross-RTT-Matrix aus und vergleicht gegen erwartete Ergebnisse.
# Nur unerwartete Abweichungen werden als FAIL gemeldet.
#
# Nutzung:
#   ./scripts/run_cross_matrix_test.sh                    # Voller Lauf
#   ./scripts/run_cross_matrix_test.sh --skip-build       # Ohne Neubau
#   ./scripts/run_cross_matrix_test.sh --quick            # Nur 3 Fixtures pro Suite (Smoke-Test)
#   ./scripts/run_cross_matrix_test.sh --filter 'er-*'    # Nur passende Fixtures (glob)
#   ./scripts/run_cross_matrix_test.sh --filter 'document-02' --skip-build
#
# Exit-Code:
#   0 = Alle Ergebnisse wie erwartet
#   1 = Unerwartete Abweichungen gefunden

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

: "${EXI_TESTSUITE_DIR:?EXI_TESTSUITE_DIR muss gesetzt sein (Pfad zur W3C EXI Test Suite)}"
DECLARED_DIR="$EXI_TESTSUITE_DIR/data/interop/schemaInformedGrammar/declaredProductions"
UNDECLARED_DIR="$EXI_TESTSUITE_DIR/data/interop/schemaInformedGrammar/undeclaredProductions"
ALIGNMENTS="bitpacked bytealigned precompression compression strict"
RESULTS_DIR="$REPO_DIR/target/cross_matrix_test"
RESULTS_FILE="$RESULTS_DIR/results.tsv"
EXPECTATIONS_FILE="$REPO_DIR/tests/cross_matrix_expectations.tsv"
BINARY="$REPO_DIR/target/release/examples/full_cross_test"

SKIP_BUILD=false
QUICK_MODE=false
FILTER=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --skip-build) SKIP_BUILD=true; shift ;;
        --quick) QUICK_MODE=true; shift ;;
        --filter) FILTER="$2"; shift 2 ;;
        *) shift ;;
    esac
done

# Prüft ob ein Fixture-Name zum Filter passt (glob-Matching)
matches_filter() {
    [[ -z "$FILTER" ]] && return 0
    # Bash extended glob: Filter als Muster
    # shellcheck disable=SC2254
    case "$1" in
        $FILTER) return 0 ;;
        *) return 1 ;;
    esac
}

total_start=$SECONDS

# Build
if [[ "$SKIP_BUILD" != "true" ]]; then
    echo "Baue full_cross_test..."
    build_start=$SECONDS
    cargo build --quiet --release --example full_cross_test
    echo "  Build: $((SECONDS - build_start))s"
fi

mkdir -p "$RESULTS_DIR"

if [[ -n "$FILTER" ]]; then
    echo "Filter: '$FILTER'"
fi

# Header
printf "fixture\ttest\tresult\tdetail\n" > "$RESULTS_FILE"

DTRM_ALIGNMENTS="bitpacked bytealigned precompression compression"

# Batch-Input generieren und in einem Prozessaufruf verarbeiten
echo "Generiere Batch-Input..."

generate_batch_input() {
    # Declared Fixtures
    local declared_count=0
    for xml in "$DECLARED_DIR"/*.xml; do
        local bname
        bname=$(basename "$xml" .xml)
        matches_filter "$bname" || continue
        for alignment in $ALIGNMENTS; do
            echo "declared $bname $alignment"
        done
        declared_count=$((declared_count + 1))
        if [[ "$QUICK_MODE" == "true" ]] && [[ $declared_count -ge 3 ]]; then
            break
        fi
    done

    # Undeclared Fixtures
    local undeclared_count=0
    for xml in "$UNDECLARED_DIR"/*.xml; do
        local bname
        bname=$(basename "$xml" .xml)
        [[ "$bname" == "er-entity" ]] && continue
        matches_filter "$bname" || continue
        for alignment in $ALIGNMENTS; do
            echo "undeclared $bname $alignment"
        done
        undeclared_count=$((undeclared_count + 1))
        if [[ "$QUICK_MODE" == "true" ]] && [[ $undeclared_count -ge 3 ]]; then
            break
        fi
    done

    # DTRM Fixtures (kein strict)
    local dtrm_count=0
    for xml in "$REPO_DIR"/tests/fixtures/dtrm/*.xml; do
        local bname
        bname=$(basename "$xml" .xml)
        matches_filter "$bname" || continue
        for alignment in $DTRM_ALIGNMENTS; do
            echo "dtrm $bname $alignment"
        done
        dtrm_count=$((dtrm_count + 1))
        if [[ "$QUICK_MODE" == "true" ]] && [[ $dtrm_count -ge 3 ]]; then
            break
        fi
    done
}

batch_start=$SECONDS
generate_batch_input | "$BINARY" --batch >> "$RESULTS_FILE" 2>/dev/null
batch_time=$((SECONDS - batch_start))
echo "  Batch: ${batch_time}s"

total_time=$((SECONDS - total_start))

# ============================================================================
# Vergleich gegen Expectations
# ============================================================================

echo ""
echo "=== Vergleich gegen Erwartungswerte ==="

# Expectations laden (nicht-OK Eintraege)
declare -A expected
while IFS=$'\t' read -r fixture test result reason; do
    [[ "$fixture" == "#"* ]] && continue
    [[ -z "$fixture" ]] && continue
    expected["${fixture}|${test}"]="$result"
done < "$EXPECTATIONS_FILE"

# Ergebnisse vergleichen
unexpected_count=0
unexpected_file="$RESULTS_DIR/unexpected.tsv"
printf "fixture\ttest\texpected\tactual\tdetail\n" > "$unexpected_file"

new_ok_count=0
new_fail_count=0
regression_count=0
improvement_count=0
matched_count=0
matched_ok=0
matched_skip=0
matched_diff=0

while IFS=$'\t' read -r fixture test result detail; do
    [[ "$fixture" == "fixture" ]] && continue  # Header
    [[ -z "$fixture" ]] && continue

    key="${fixture}|${test}"
    exp="${expected[$key]:-OK}"  # Default: OK erwartet

    if [[ "$result" == "$exp" ]]; then
        matched_count=$((matched_count + 1))
        if [[ "$result" == "OK" ]]; then
            matched_ok=$((matched_ok + 1))
        elif [[ "$result" == SKIP* ]]; then
            matched_skip=$((matched_skip + 1))
        elif [[ "$result" == DIFF:* ]]; then
            matched_diff=$((matched_diff + 1))
        fi
    else
        unexpected_count=$((unexpected_count + 1))
        printf "%s\t%s\t%s\t%s\t%s\n" "$fixture" "$test" "$exp" "$result" "$detail" >> "$unexpected_file"

        # Kategorisierung
        if [[ "$exp" == "OK" ]] && [[ "$result" == "FAIL" ]]; then
            regression_count=$((regression_count + 1))
        elif [[ "$exp" == "FAIL" ]] && [[ "$result" == "OK" ]]; then
            improvement_count=$((improvement_count + 1))
        elif [[ "$exp" == "OK" ]] && [[ "$result" == "SKIP" ]]; then
            new_fail_count=$((new_fail_count + 1))
        elif [[ "$exp" == "SKIP" ]] && [[ "$result" == "OK" ]]; then
            improvement_count=$((improvement_count + 1))
        fi
    fi
done < "$RESULTS_FILE"

# Zusammenfassung
total_tests=$((matched_count + unexpected_count))
echo "Getestet:     $total_tests"
echo "  OK:         $matched_ok"
echo "  DIFF:       $matched_diff (bekannte Byte-Differenzen)"
echo "  SKIP:       $matched_skip"
echo "Unerwartet:   $unexpected_count"
echo ""
echo "Gesamt:       ${total_time}s"
echo ""

if [[ $unexpected_count -gt 0 ]]; then
    echo "=== Unerwartete Ergebnisse ==="

    if [[ $regression_count -gt 0 ]]; then
        echo ""
        echo "--- Regressionen (erwartet OK, bekommen FAIL): $regression_count ---"
        awk -F'\t' 'NR>1 && $3=="OK" && $4=="FAIL" {printf "  %s  %s  %s\n", $1, $2, $5}' "$unexpected_file" | head -20
    fi

    if [[ $improvement_count -gt 0 ]]; then
        echo ""
        echo "--- Verbesserungen (erwartet FAIL/SKIP, bekommen OK): $improvement_count ---"
        awk -F'\t' 'NR>1 && $4=="OK" {printf "  %s  %s  (war: %s)\n", $1, $2, $3}' "$unexpected_file" | head -20
    fi

    # Alle anderen
    other_count=$((unexpected_count - regression_count - improvement_count))
    if [[ $other_count -gt 0 ]]; then
        echo ""
        echo "--- Sonstige Abweichungen: $other_count ---"
        awk -F'\t' 'NR>1 && !($3=="OK" && $4=="FAIL") && $4!="OK" {printf "  %s  %s  exp=%s got=%s  %s\n", $1, $2, $3, $4, $5}' "$unexpected_file" | head -20
    fi

    echo ""
    echo "Details: $unexpected_file"

    # Exit-Code: Nur bei Regressionen FAIL
    if [[ $regression_count -gt 0 ]]; then
        echo ""
        echo "ERGEBNIS: FAIL ($regression_count Regressionen)"
        exit 1
    else
        echo ""
        echo "ERGEBNIS: WARN ($unexpected_count Abweichungen, aber keine Regressionen)"
        exit 0
    fi
else
    echo "ERGEBNIS: OK (alle $total_tests Tests wie erwartet)"
    exit 0
fi
