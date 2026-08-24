#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing cabal configuration and running build..."

# 1. Clean up conflicting cabal configs
rm -rf "$DIR/.cabal"
rm -rf "$HOME/.config/cabal"

# 2. Ensure frontend-wasm directory exists
mkdir -p "$DIR/frontend-wasm"

# 3. Create frontend-wasm/cabal.project
echo "packages:" > "$DIR/frontend-wasm/cabal.project"
echo "  ." >> "$DIR/frontend-wasm/cabal.project"

# 4. Create frontend-wasm.cabal
cat > "$DIR/frontend-wasm/frontend-wasm.cabal" << 'CABAL_EOF'
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
CABAL_EOF

# 5. Create build-wasm.sh
cat > "$DIR/scripts/build-wasm.sh" << 'BUILD_EOF'
#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"

echo "[1/3] Compiling C stubs..."
clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with cabal (GHC 9.8)..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"

cabal update

cabal build frontend-wasm:frontend-wasm-exe \
  --project-file="$DIR/frontend-wasm/cabal.project" \
  --with-compiler=wasm32-wasi-ghc-9.8 \
  --with-ghc-pkg=wasm32-wasi-ghc-pkg-9.8 \
  --with-hsc2hs=wasm32-wasi-hsc2hs-9.8

