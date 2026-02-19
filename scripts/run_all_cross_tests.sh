#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

TESTSUITE_DIR_DEFAULT="$HOME/Downloads/exi_testsuite/ttfms-interop-18122013"
TESTSUITE_ZIP_URL="https://www.w3.org/XML/EXI/interop-framework/ttfms-interop-18122013.zip"
TESTSUITE_ZIP_DIR="$HOME/Downloads/exi_testsuite"
TESTSUITE_ZIP_PATH="$TESTSUITE_ZIP_DIR/exi-interop.zip"

EXI_JAR_DIR="$ROOT/tests/fixtures/exificient"
EXI_FAT_JAR="$EXI_JAR_DIR/exificient-1.0.4-fat.jar"

EXI_JARS=(
  "$EXI_JAR_DIR/exificient-1.0.4.jar"
  "$EXI_JAR_DIR/exificient-core-1.0.4.jar"
  "$EXI_JAR_DIR/exificient-grammars-1.0.4.jar"
  "$EXI_JAR_DIR/xercesImpl-2.12.0.jar"
)

ensure_testsuite() {
  if [[ -z "${EXI_TESTSUITE_DIR:-}" ]]; then
    EXI_TESTSUITE_DIR="$TESTSUITE_DIR_DEFAULT"
    export EXI_TESTSUITE_DIR
  fi

  if [[ -d "$EXI_TESTSUITE_DIR" ]]; then
    return
  fi

  echo "EXI_TESTSUITE_DIR not found at '$EXI_TESTSUITE_DIR'. Downloading testsuite..."
  mkdir -p "$TESTSUITE_ZIP_DIR"
  curl -L -o "$TESTSUITE_ZIP_PATH" "$TESTSUITE_ZIP_URL"
  (cd "$TESTSUITE_ZIP_DIR" && unzip -q -o "$TESTSUITE_ZIP_PATH")

  if [[ -d "$TESTSUITE_DIR_DEFAULT" ]]; then
    EXI_TESTSUITE_DIR="$TESTSUITE_DIR_DEFAULT"
    export EXI_TESTSUITE_DIR
    return
  fi

  echo "ERROR: testsuite missing after download. Expected: $TESTSUITE_DIR_DEFAULT" >&2
  exit 1
}

ensure_exificient_fat_jar() {
  if [[ -f "$EXI_FAT_JAR" ]]; then
    export EXIFICIENT_JAR="$EXI_FAT_JAR"
    return
  fi

  for jar in "${EXI_JARS[@]}"; do
    if [[ ! -f "$jar" ]]; then
      echo "ERROR: missing Exificient JAR: $jar" >&2
      exit 1
    fi
  done

  echo "Building Exificient fat JAR: $EXI_FAT_JAR"
  python3 - <<'PY'
import zipfile
from pathlib import Path

root = Path(__file__).resolve().parent.parent
jar_dir = root / "tests" / "fixtures" / "exificient"
jar_paths = [
    jar_dir / "exificient-1.0.4.jar",
    jar_dir / "exificient-core-1.0.4.jar",
    jar_dir / "exificient-grammars-1.0.4.jar",
    jar_dir / "xercesImpl-2.12.0.jar",
]
out_path = jar_dir / "exificient-1.0.4-fat.jar"
if out_path.exists():
    out_path.unlink()

with zipfile.ZipFile(out_path, "w", compression=zipfile.ZIP_STORED) as out_zip:
    for jar in jar_paths:
        with zipfile.ZipFile(jar, "r") as z:
            for info in z.infolist():
                if info.filename.endswith("/"):
                    continue
                data = z.read(info.filename)
                zi = zipfile.ZipInfo(info.filename)
                zi.date_time = info.date_time
                zi.compress_type = zipfile.ZIP_STORED
                zi.external_attr = info.external_attr
                out_zip.writestr(zi, data)

print(f"Wrote {out_path} ({out_path.stat().st_size} bytes)")
PY

  export EXIFICIENT_JAR="$EXI_FAT_JAR"
}

main() {
  ensure_testsuite
  ensure_exificient_fat_jar

  cd "$ROOT"

  echo "Running cross/interop tests..."
  cargo test --quiet --test coverage_matrix -- --nocapture
  cargo test --quiet --test cli_cross_exificient -- --nocapture
  cargo test --quiet --test exi4json_cross_exificient -- --nocapture
  cargo test --quiet --test cross_rtt -- --nocapture
}

main "$@"
