#!/usr/bin/env bash
# Verify full matrix results against expectations.
# Usage: ./scripts/verify.sh [--skip-build]

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

: "${EXI_TESTSUITE_DIR:?EXI_TESTSUITE_DIR must be set (run scripts/setup.sh)}"
: "${EXIFICIENT_JAR:?EXIFICIENT_JAR must be set (run scripts/setup.sh)}"

RESULTS_DIR="$ROOT/target/cross_rtt_full"
RESULTS_FILE="$RESULTS_DIR/results.tsv"
EXPECTATIONS_FILE="$ROOT/tests/cross_matrix_expectations.tsv"

SKIP_BUILD=false
if [[ "${1:-}" == "--skip-build" ]]; then
  SKIP_BUILD=true
fi

if [[ ! -f "$RESULTS_FILE" ]] || [[ "$SKIP_BUILD" != "true" ]]; then
  "$ROOT/scripts/run.sh" ${SKIP_BUILD:+--skip-build}
fi

if [[ ! -f "$RESULTS_FILE" ]]; then
  echo "ERROR: missing results file $RESULTS_FILE" >&2
  exit 1
fi

# Load expectations
declare -A expected
while IFS=$'\t' read -r fixture test result reason; do
  [[ "$fixture" == "#"* ]] && continue
  [[ -z "$fixture" ]] && continue
  expected["${fixture}|${test}"]="$result"
done < "$EXPECTATIONS_FILE"

unexpected_count=0
unexpected_file="$RESULTS_DIR/unexpected.tsv"
printf "fixture\ttest\texpected\tactual\tdetail\n" > "$unexpected_file"

while IFS=$'\t' read -r fixture test result detail; do
  [[ "$fixture" == "fixture" ]] && continue
  [[ -z "$fixture" ]] && continue

  key="${fixture}|${test}"
  if [[ -n "${expected[$key]+x}" ]]; then
    exp="${expected[$key]}"
  else
    # Allow new tests without expectations: only fail on FAIL/ERROR.
    if [[ "$result" == "FAIL" || "$result" == "ERROR" ]]; then
      exp="OK"
    else
      continue
    fi
  fi

  if [[ "$result" != "$exp" ]]; then
    unexpected_count=$((unexpected_count + 1))
    printf "%s\t%s\t%s\t%s\t%s\n" "$fixture" "$test" "$exp" "$result" "$detail" >> "$unexpected_file"
  fi
done < "$RESULTS_FILE"

if [[ $unexpected_count -gt 0 ]]; then
  echo "Unexpected results: $unexpected_count"
  echo "See: $unexpected_file"
  exit 1
fi

echo "All results match expectations."
