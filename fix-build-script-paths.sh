#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing compiler paths and cabal flags in build-wasm.sh..."

cat > "$DIR/scripts/build-wasm.sh" << 'BUILD'
#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"

echo "[1/3] Compiling C stubs..."
clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with cabal..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"

cabal update

# Use --project-dir and standard wasm32-wasi-ghc executable names
cabal build frontend-wasm-exe \
  --project-dir="$DIR/frontend-wasm" \
  --with-compiler=wasm32-wasi-ghc \
  --with-ghc-pkg=wasm32-wasi-ghc-pkg \
  --with-hsc2hs=wasm32-wasi-hsc2hs

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

echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"
BUILD

chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] build-wasm.sh updated. Running build..."
"$DIR/scripts/build-wasm.sh"
