#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Switching to wasm32-wasi-cabal to resolve version mismatches..."

cat > "$DIR/scripts/build-wasm.sh" << 'BUILD_END'
#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with wasm32-wasi-cabal..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"
unset GHC_PACKAGE_PATH

# Use the wasm-specific cabal provided by ghc-wasm-meta
wasm32-wasi-cabal update

wasm32-wasi-cabal build frontend-wasm-exe \
  --project-dir="$DIR/frontend-wasm" \
  --ghc-options="-fexternal-interpreter"

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
if [ -z "$OBJ_FILE" ]; then 
  echo "Error: Could not find compiled Main.o"
  exit 1
fi

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

echo "[HRSM] Done: $DIR/dist-wasm/react
