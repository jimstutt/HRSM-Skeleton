#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
OUT=build/reactor.wasm
LINKER=${LINKER:-wasm-ld}
OBJS=$(ls build/*.o dist-reactor/*.o 2>/dev/null || true)
if [ -z "$OBJS" ]; then
  echo "No object files found in build/ or dist-reactor/"
  exit 2
fi
echo "Linking: $OBJS"
$LINKER --no-entry --export=call_reactor --allow-undefined --strip-debug -o "$OUT" $OBJS
echo "WASM: $OUT"
