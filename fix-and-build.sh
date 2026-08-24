#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Ensuring all configuration files exist and running build..."

# 1. Clean up conflicting cabal configs to silence warnings
rm -rf "$DIR/.cabal"
rm -rf "$HOME/.config/cabal"

# 2. Ensure frontend-wasm directory and cabal.project exist
mkdir -p "$DIR/frontend-wasm"
cat << 'EOF' > "$DIR/frontend-wasm/cabal.project"
packages:
  .
EOF

# 3. Ensure frontend-wasm.cabal exists with correct dependencies
cat << 'EOF' > "$DIR/frontend-wasm/frontend-wasm.cabal"
cabal-version:      3.0
name:               frontend-wasm
version:            0.1.0.0
build-type:         Simple

executable frontend-wasm-exe
  main-is:          Main.hs
  hs-source-dirs:   .
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , reflex-dom >= 0.6
    , reflex >= 0.9
    , text
    , containers
  ghc-options:
    -O2
    -no-hs-main
    -optl-mexec-model=reactor
    -optl-Wl,--allow-undefined
    -optl-Wl,--export=start_reactor
    -optl-Wl,--export=reactor_stop
    -optl-Wl,--export-all
EOF

# 4. Update build-wasm.sh to be fully self-contained
cat << 'EOF' > "$DIR/scripts/build-wasm.sh"
#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

mkdir -p "$DIR/dist-wasm"
mkdir -p "$DIR/frontend-wasm"

# Ensure frontend-wasm/cabal.project exists (failsafe)
if [ ! -f "$DIR/frontend-wasm/cabal.project" ]; then
  echo "[HRSM] Creating missing frontend-wasm/cabal.project..."
  echo "packages:" > "$DIR/frontend-wasm/cabal.project"
  echo "  ." >> "$DIR/frontend-wasm/cabal.project"
fi

echo "[1/3] Compiling C stubs..."
clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with cabal (GHC 9.8)..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"

# Clean up old config to avoid warnings
rm -rf "$HOME/.config/cabal"

cabal update

cabal build frontend-wasm:frontend-wasm-exe \
  --project-file="$DIR/frontend-wasm/cabal.project" \
  --with-compiler=wasm32-wasi-ghc-9.8 \
  --with-ghc-pkg=wasm32-wasi-ghc-pkg-9.8 \
  --with-hsc2hs=wasm32-wasi-hsc2hs-9.8

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
if [
