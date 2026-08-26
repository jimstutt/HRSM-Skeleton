#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Forcing -fexternal-interpreter for all packages via cabal.project..."

# Update cabal.project to apply the flag globally
cat > "$DIR/frontend-wasm/cabal.project" << 'PROJECT_END'
packages:
  .

-- Force external interpreter for all packages (required for TH in Wasm cross-compilation)
package *
  ghc-options: -fexternal-interpreter
PROJECT_END

echo "[HRSM] cabal.project updated. Please run: ./scripts/build-wasm.sh"
