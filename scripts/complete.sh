#!/usr/bin/env bash
# Complete setup + run + verify

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

source "$ROOT/scripts/setup.sh" --export

cargo test --quiet --test full_cross_matrix

source "$ROOT/target/cross_env.sh"
"$ROOT/scripts/verify.sh" --skip-build
