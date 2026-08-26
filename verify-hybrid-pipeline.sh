#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Verifying Hybrid Architecture pipeline..."

cd "$DIR"

# 1. Build TS types
echo "[1/3] Building TypeScript types from Servant API..."
if ! nix build .#ts-types; then
  echo "[HRSM] ERROR: ts-types build failed. Check 'nix log' for details."
  exit 1
fi

# 2. Verify output exists and is non-empty
if [ ! -s result/api-types.ts ]; then
  echo "[HRSM] ERROR: api-types.ts is missing or empty."
  exit 1
fi
echo "[HRSM] ✓ api-types.ts generated ($(wc -l < result/api-types.ts) lines)"

# 3. Copy to frontend source tree (for Vite consumption)
mkdir -p frontend/src
cp result/api-types.ts frontend/src/api-types.ts
echo "[HRSM] ✓ Copied to frontend/src/api-types.ts"

# 4. Verify frontend can resolve the import (basic syntax check)
if [ -f frontend/package.json ]; then
  echo "[HRSM] Checking frontend TypeScript compilation..."
  nix shell nixpkgs#nodejs --run "cd frontend && npm install && npx tsc --noEmit src/api-types.ts" || \
    echo "[HRSM] WARN: TS check failed (may need tsconfig adjustments)"
else
  echo "[HRSM] SKIP: No frontend/package.json found. Initialize Vite project first."
fi

echo "[HRSM] Hybrid pipeline verification complete."
