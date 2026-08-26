#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"
mkdir -p "$DIR/dist-wasm"

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with absolute isolation..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"
unset GHC_PACKAGE_PATH

# CRITICAL: Temporarily rename root cabal.project to prevent parent directory traversal
if [ -f "$DIR/cabal.project" ]; then
  mv "$DIR/cabal.project" "$DIR/cabal.project.bak"
  echo "[HRSM] Temporarily renamed root cabal.project to avoid workspace contamination."
fi

# Clear stale cabal resolution cache
rm -rf "$DIR/dist-newstyle/cache/plan.json"
rm -rf "$DIR/frontend-wasm/dist-newstyle"

wasm32-wasi-cabal update

# CRITICAL: Change directory INTO frontend-wasm so cabal cannot traverse up to the root
cd "$DIR/frontend-wasm"
wasm32-wasi-cabal build frontend-wasm-exe \
  --ghc-options="-fexternal-interpreter -pgmi $DIR/scripts/wasm-iserv.sh"

# Restore root cabal.project
cd "$DIR"
if [ -f "$DIR/cabal.project.bak" ]; then
  mv "$DIR/cabal.project.bak" "$DIR/cabal.project"
  echo "[HRSM] Restored root cabal.project."
fi

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
[ -z "$OBJ_FILE" ] && { echo "Error: Could not find compiled Main.o"; exit 1; }

wasm32-wasi-ghc \
  -O2 \
  -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--allow-undefined \
  -optl-Wl,--export=start_reactor \
  -optl-Wl,--export=reactor_stop \
  -optl-Wl,--export-all \
  "$OBJ_FILE" \
  "$DIR/dist-wasm/stubs.o" \
  -o "$DIR/dist-wasm/reactor.wasm"

echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"
