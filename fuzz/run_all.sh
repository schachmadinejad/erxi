#!/usr/bin/env bash
set -euo pipefail

DURATION="${1:-600}"
TARGETS=(decode decode_json header roundtrip)

for target in "${TARGETS[@]}"; do
    echo "=== $target ($DURATION s) ==="
    cargo +nightly fuzz run "$target" -- -max_total_time="$DURATION"
    echo
done

echo "Fertig."
