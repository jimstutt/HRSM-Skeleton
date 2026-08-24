#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Ensuring frontend-wasm cabal configuration exists and cleaning up warnings..."

# 1. Clean up conflicting cabal configs to silence warnings
rm -rf "$DIR/.cabal"
rm -rf "$HOME/.config/cabal"

# 2. Ensure frontend-wasm directory exists
mkdir -p "$DIR/frontend-wasm"

# 3. Create frontend-only cabal.project to isolate dependencies
cat << 'EOF' > "$DIR/frontend-wasm/cabal.project"
packages:
  .
EOF

# 4. Ensure frontend-wasm.cabal exists with correct dependencies
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

echo "[HRSM] Configuration files created successfully."
echo "Next step: Run './scripts/build-wasm.sh'"
