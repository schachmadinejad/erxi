#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
: "${EXIFICIENT_JAR:?EXIFICIENT_JAR muss gesetzt sein (Pfad zur Exificient JAR)}"
EXI_JAR="$EXIFICIENT_JAR"
: "${EXI_TESTSUITE_DIR:?EXI_TESTSUITE_DIR muss gesetzt sein (Pfad zur W3C EXI Test Suite)}"
XML_ROOT="$EXI_TESTSUITE_DIR/data/interop"
SCHEMA_ROOT="$XML_ROOT/schemaInformedGrammar"

# Optional filtering:
#   - Pass one or more substrings as args, or
#   - Set ERXI_FILTER to a comma-separated list of substrings.
#
# Examples:
#   ERXI_FILTER="schema_declared/eventCodeAssignment-01,complexType-24" ./scripts/run_w3c_debug_traces.sh
#   ./scripts/run_w3c_debug_traces.sh schema_declared/eventCodeAssignment-01 schema_undeclared/er-
#
# If filters are used and ERXI_RUN_TAG is not set, we create a per-run output
# directory to avoid overwriting prior traces.
FILTERS=()
if [[ $# -gt 0 ]]; then
  FILTERS=("$@")
elif [[ -n "${ERXI_FILTER:-}" ]]; then
  IFS=',' read -r -a FILTERS <<< "${ERXI_FILTER}"
fi

if [[ ${#FILTERS[@]} -gt 0 && -z "${ERXI_RUN_TAG:-}" ]]; then
  ERXI_RUN_TAG="partial-$(date +%Y%m%d-%H%M%S)"
fi

OUT_ROOT="$ROOT/target/w3c_debug_traces${ERXI_RUN_TAG:+/$ERXI_RUN_TAG}"

mkdir -p "$OUT_ROOT"

log() { echo "[trace] $*"; }

if [[ "${ERXI_SKIP_BUILD:-}" != "1" ]]; then
  log "building erxi examples (once)"
  (cd "$ROOT" && cargo build --quiet --examples)
fi

ERXI_DECODE_BIN="$ROOT/target/debug/examples/decode_one"
ERXI_ENCODE_BIN="$ROOT/target/debug/examples/xml_to_exi"
EXIFICIENT_ENABLED=1
if [[ "${ERXI_ONLY:-}" == "1" ]]; then
  EXIFICIENT_ENABLED=0
fi

if [[ ! -x "$ERXI_DECODE_BIN" || ! -x "$ERXI_ENCODE_BIN" ]]; then
  echo "missing erxi example binaries; run with ERXI_SKIP_BUILD=0" >&2
  exit 1
fi

fixture_options() {
  local name="$1"
  local opts=()
  if [[ "$name" == *"_bytealigned"* ]]; then
    opts+=("-bytePacked")
  elif [[ "$name" == *"_precompression"* ]]; then
    opts+=("-preCompression")
  elif [[ "$name" == *"_compression"* ]]; then
    opts+=("-compression")
  fi
  if [[ "$name" == *"_strict"* ]]; then
    opts+=("-strict")
  fi
  if [[ "$name" == *"schema_undeclared/cm-"* ]]; then
    opts+=("-preserveComments" "-preservePIs")
  fi
  if [[ "$name" == *"schema_undeclared/pi-"* ]]; then
    opts+=("-preservePIs")
  fi
  if [[ "$name" == *"schema_undeclared/er-"* ]]; then
    opts+=("-preserveDTDs")
  fi
  if [[ "$name" == *"schema_undeclared/namespaceDecl-"* ]]; then
    opts+=("-preservePrefixes")
  fi
  if [[ "$name" == *"schema_declared/document-01"* ]]; then
    opts+=("-preservePrefixes")
  elif [[ "$name" == *"schema_declared/document-02"* ]] || [[ "$name" == *"schema_declared/document-03"* ]] || [[ "$name" == *"schema_declared/document-04"* ]] || [[ "$name" == *"schema_declared/document-05"* ]] || [[ "$name" == *"schema_declared/document-06"* ]] || [[ "$name" == *"schema_declared/document-07"* ]]; then
    opts+=("-preserveDTDs" "-preservePIs" "-preserveComments")
  fi
  if [[ "$name" == *"schema_declared/fragment-01"* ]]; then
    opts+=("-preserveDTDs" "-preservePIs" "-preserveComments")
  elif [[ "$name" == *"schema_declared/fragment-02"* ]]; then
    opts+=("-preserveDTDs")
  elif [[ "$name" == *"schema_declared/fragment-03"* ]] || [[ "$name" == *"schema_declared/fragment-04"* ]] || [[ "$name" == *"schema_declared/fragment-05"* ]] || [[ "$name" == *"schema_declared/fragment-06"* ]]; then
    opts+=("-preservePIs")
  fi
  if [[ "$name" == *"elementFragment"* ]] || [[ "$name" == *"fragment-"* ]] || [[ "$name" == *"_fragment"* ]]; then
    opts+=("-fragment")
  fi
  echo "${opts[@]}"
}

fixture_schema() {
  local name="$1"
  if [[ "$name" == *"schema_undeclared/"* ]]; then
    echo "$SCHEMA_ROOT/acceptance.xsd"
    return
  fi
  if [[ "$name" != *"schema_declared/"* ]]; then
    echo ""
    return
  fi
  local base
  base="$(basename "$name")"
  base="${base%%_*}"
  case "$base" in
    elementFragment-*) echo "$SCHEMA_ROOT/declaredProductions/elementFragment.xsd" ;;
    document-*) echo "$SCHEMA_ROOT/declaredProductions/document.xsd" ;;
    duplicateTerminals-01*) echo "$SCHEMA_ROOT/declaredProductions/duplicateTerminals-01.xsd" ;;
    duplicateTerminals-02*) echo "$SCHEMA_ROOT/declaredProductions/duplicateTerminals-02.xsd" ;;
    substitutionGroup-*) echo "$SCHEMA_ROOT/declaredProductions/substitutionGroup.xsd" ;;
    particle-*) echo "$SCHEMA_ROOT/declaredProductions/particle.xsd" ;;
    fragment-a-*) echo "$SCHEMA_ROOT/declaredProductions/fragment-a.xsd" ;;
    fragment-b-*) echo "$SCHEMA_ROOT/declaredProductions/fragment-b.xsd" ;;
    fragment-c-*) echo "$SCHEMA_ROOT/declaredProductions/fragment-c.xsd" ;;
    fragment-d-*) echo "$SCHEMA_ROOT/declaredProductions/fragment-d.xsd" ;;
    fragment-e-*) echo "$SCHEMA_ROOT/declaredProductions/fragment-e.xsd" ;;
    *) echo "$SCHEMA_ROOT/acceptance.xsd" ;;
  esac
}

fixture_xml() {
  local name="$1"
  local base
  base="$(basename "$name")"
  base="${base%%_*}"
  local direct=""
  if [[ "$name" == *"schema_undeclared/"* ]]; then
    direct="$SCHEMA_ROOT/undeclaredProductions/$base.xml"
  elif [[ "$name" == *"schema_declared/"* ]]; then
    direct="$SCHEMA_ROOT/declaredProductions/$base.xml"
  elif [[ "$name" == *"builtin_element/"* ]]; then
    direct="$XML_ROOT/preserve/element/$base.xml"
  elif [[ "$name" == *"builtin_attribute/"* ]]; then
    direct="$XML_ROOT/preserve/attribute/$base.xml"
  elif [[ "$name" == *"builtin_character/"* ]]; then
    direct="$XML_ROOT/preserve/character/$base.xml"
  elif [[ "$name" == *"datatypes_"* ]]; then
    direct="$XML_ROOT/preserve/datatypes/$base.xml"
  elif [[ "$name" == *"header/"* ]]; then
    direct="$XML_ROOT/preserve/header/$base.xml"
  elif [[ "$name" == *"compression/"* ]]; then
    direct="$XML_ROOT/preserve/compression/$base.xml"
  fi
  if [[ -n "$direct" && -f "$direct" ]]; then
    echo "$direct"
    return
  fi
  find "$XML_ROOT" -name "$base.xml" | head -n 1
}

decode_exificient() {
  local fixture="$1"
  local out_log="$2"
  local schema="$3"
  shift 3
  local opts=("$@")
  local cmd=(java -jar "$EXI_JAR" -decode -debugOnly -i "$fixture")
  if [[ -n "$schema" ]]; then
    cmd+=(-schema "$schema")
  fi
  cmd+=("${opts[@]}")
  ERXI_TRACE_EVENT_CODE=1 ERXI_TRACE_ELEM_EVENT=1 ERXI_TRACE_NT_CTX=1 "${cmd[@]}" >"$out_log" 2>&1 || true
}

decode_erxi() {
  local fixture="$1"
  local out_log="$2"
  local rel="$3"
  local interop_flag="${ERXI_INTEROP:-0}"
  if [[ "$rel" == *"schema_declared/"* || "$rel" == *"schema_undeclared/"* ]]; then
    ERXI_INTEROP="$interop_flag" ERXI_TRACE_EVENT_CODE=1 ERXI_TRACE_ELEM_EVENT=1 ERXI_TRACE_NT_CTX=1 \
      "$ERXI_DECODE_BIN" "$fixture" >"$out_log" 2>&1 || true
  else
    ERXI_NO_SCHEMA=1 ERXI_INTEROP="$interop_flag" ERXI_TRACE_EVENT_CODE=1 ERXI_TRACE_ELEM_EVENT=1 ERXI_TRACE_NT_CTX=1 \
      "$ERXI_DECODE_BIN" "$fixture" >"$out_log" 2>&1 || true
  fi
}

encode_exificient() {
  local xml="$1"
  local out_exi="$2"
  local out_log="$3"
  local schema="$4"
  shift 4
  local opts=("$@")
  local cmd=(java -jar "$EXI_JAR" -encode -i "$xml" -o "$out_exi")
  if [[ -n "$schema" ]]; then
    cmd+=(-schema "$schema")
  fi
  cmd+=("${opts[@]}")
  ERXI_TRACE_EVENT_CODE=1 ERXI_TRACE_ELEM_EVENT=1 ERXI_TRACE_NT_CTX=1 "${cmd[@]}" >"$out_log" 2>&1 || true
}

encode_erxi() {
  local xml="$1"
  local out_exi="$2"
  local out_log="$3"
  local schema="$4"
  local interop_flag="${ERXI_INTEROP:-0}"
  if [[ -n "$schema" ]]; then
    ERXI_INTEROP="$interop_flag" ERXI_TRACE_EVENT_CODE=1 ERXI_TRACE_ELEM_EVENT=1 ERXI_TRACE_NT_CTX=1 \
      "$ERXI_ENCODE_BIN" "$xml" "$out_exi" --schema "$schema" \
      >"$out_log" 2>&1 || true
  else
    ERXI_INTEROP="$interop_flag" ERXI_TRACE_EVENT_CODE=1 ERXI_TRACE_ELEM_EVENT=1 ERXI_TRACE_NT_CTX=1 \
      "$ERXI_ENCODE_BIN" "$xml" "$out_exi" \
      >"$out_log" 2>&1 || true
  fi
}

fixtures=$(find "$ROOT/tests/fixtures/w3c" -type f -name '*.exi' | sort)
for fixture in $fixtures; do
  rel="${fixture#$ROOT/tests/fixtures/w3c/}"
  if [[ ${#FILTERS[@]} -gt 0 ]]; then
    match=0
    for f in "${FILTERS[@]}"; do
      if [[ "$rel" == *"$f"* ]]; then
        match=1
        break
      fi
    done
    if [[ "$match" -eq 0 ]]; then
      continue
    fi
  fi
  base="${rel%.exi}"
  base_name="$(basename "$rel")"
  out_dir="$OUT_ROOT/$base"
  mkdir -p "$out_dir/decode" "$out_dir/encode"

  schema="$(fixture_schema "$rel")"
  opts=($(fixture_options "$rel"))

  log "decode: $rel"
  if [[ "$EXIFICIENT_ENABLED" == "1" ]]; then
    decode_exificient "$fixture" "$out_dir/decode/exificient.log" "$schema" "${opts[@]}"
  fi
  decode_erxi "$fixture" "$out_dir/decode/erxi.log" "$rel"

  xml="$(fixture_xml "$rel" || true)"
  if [[ -n "${xml:-}" && -f "$xml" ]]; then
    log "encode: $rel -> $(basename "$xml")"
    if [[ "$EXIFICIENT_ENABLED" == "1" ]]; then
      encode_exificient "$xml" "$out_dir/encode/exificient.exi" "$out_dir/encode/exificient.log" "$schema" "${opts[@]}"
    fi
    encode_erxi "$xml" "$out_dir/encode/erxi_${base_name}" "$out_dir/encode/erxi.log" "$schema"
  else
    log "encode: $rel -> XML not found, skipping"
  fi
done

log "done: traces in $OUT_ROOT"
