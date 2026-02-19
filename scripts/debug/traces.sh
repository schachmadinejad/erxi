#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

: "${EXI_TESTSUITE_DIR:?EXI_TESTSUITE_DIR must be set (run scripts/setup.sh)}"

DECLARED_DIR="$EXI_TESTSUITE_DIR/data/interop/schemaInformedGrammar/declaredProductions"

cargo build --quiet --example full_cross_test

# Generates debug traces by running a small subset with EXI_TRACE=1
EXI_TRACE=1 "$ROOT/target/debug/examples/full_cross_test" declared "$(basename "$DECLARED_DIR"/complexType-01.xml .xml)" bitpacked
