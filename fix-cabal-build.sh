#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Updating build script to run cabal update and target only frontend-wasm..."

cat << 'EOF' > "$DIR/scripts/build-wasm.sh"
#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist-wasm

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c frontend-wasm/stubs.c -o dist-wasm/stubs.o

echo "[2/3] Updating cabal package index and building frontend-wasm..."
export HOME=$PWD
export CABAL_DIR=$PWD/.cabal

# Update the package index (required if missing)
echo "Running 'cabal update' (this may take a minute on first run)..."
cabal update

# Build ONLY the frontend-wasm package to avoid resolving backend dependencies
cabal build frontend-wasm:frontend-wasm-exe \
  --with-compiler=wasm32-wasi-ghc \
  --with-hc-pkg=wasm32-wasi-ghc-pkg \
  --with-hsc2hs=wasm32-wasi-hsc2hs

echo "[3/3] Linking with stubs..."
# Find the compiled object files and link them with our C stubs
OBJ_FILE=$(find dist-newstyle -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
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
  dist-wasm/stubs.o \
  -o dist-wasm/reactor.wasm

echo "[HRSM] Done: dist-wasm/reactor.wasm"
EOF
chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] build-wasm.sh updated successfully."
echo "Next step: Run './scripts/build-wasm.sh' again."
