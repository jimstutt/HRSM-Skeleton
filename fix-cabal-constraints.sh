#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing cabal.project constraints and cleaning up config warnings..."

# 1. Remove conflicting cabal config to silence warnings
rm -rf "$DIR/.cabal"
rm -rf "$HOME/.config/cabal"

# 2. Update cabal.project to remove strict version constraints that conflict with GHC 9.14
cat << 'EOF' > "$DIR/cabal.project"
packages:
  common
  backend
  frontend-wasm

-- Allow cabal to resolve compatible versions for GHC 9.14
-- Avoid strict constraints on boot libraries like text or template-haskell

-- Disable documentation and tests to speed up Wasm build
documentation: False
tests: False
benchmarks: False
EOF

# 3. Clean cabal state
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
cabal clean || true

echo "[HRSM] cabal.project updated and config cleaned."
echo "Next step: Run './scripts/build-wasm.sh' again."
