#!/usr/bin/env bash
set -euo pipefail

echo "🏗️  Building NGOLogisticsCG to WebAssembly..."

# Clean old builds
cabal clean

# Ensure we’re inside the nix develop shell with ghc-wasm
if ! command -v cabal &>/dev/null; then
  echo "❌ Error: Run this inside 'nix develop' (WASM toolchain shell)."
  exit 1
fi

# Build for WASM target (uses ghc-wasm in nix shell)
cabal build all

# Find the wasm output
WASM_FILE=$(find dist-newstyle/build -type f -name "*.wasm" | head -n 1 || true)
if [[ -z "$WASM_FILE" ]]; then
  echo "❌ No .wasm output found in dist-newstyle/build/"
  exit 1
fi

# Prepare output dir
mkdir -p dist/wasm
cp "$WASM_FILE" dist/wasm/app.wasm
echo "✅ Copied to dist/wasm/app.wasm"

# Optimize binary (Binaryen)
if command -v wasm-opt &>/dev/null; then
  echo "⚙️  Optimizing with wasm-opt -Oz..."
  wasm-opt -Oz dist/wasm/app.wasm -o dist/wasm/app.opt.wasm
  echo "✅ Optimized: dist/wasm/app.opt.wasm"
else
  echo "⚠️  wasm-opt not found — skipping optimization."
fi

echo "🎉 Build complete! Serve with:"
echo "   python3 -m http.server 8080 --directory dist"
