#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Verifying openapi.json and continuing TS generation..."

# 1. Check if file actually exists (it should from previous run)
if [ -f "$DIR/frontend/openapi.json" ]; then
  echo "[HRSM] ✓ openapi.json exists"
  echo "--- schemas ---"
  jq '.components.schemas | keys' "$DIR/frontend/openapi.json"
else
  echo "[HRSM] ✗ File missing. Regenerating..."
  mkdir -p "$DIR/frontend"
  nix-shell -p haskellPackages.ghc haskellPackages.cabal-install zlib.dev --run "
    cd $DIR/common && cabal run generate-openapi -- --output=$DIR/frontend/openapi.json
  "
fi

# 2. Commit the working generator changes
cd "$DIR"
git add common/app/GenerateOpenAPI.hs common/common.cabal frontend/openapi.json
git commit -m "[HRSM] Auto-create output dirs in GenerateOpenAPI.hs" || true

# 3. Build ts-types derivation
echo "[HRSM] Building ts-types..."
nix build .#ts-types

# 4. Verify final output
if [ -f "$DIR/result/api-types.ts" ]; then
  echo "[HRSM] ✓ api-types.ts generated"
  grep -A5 "export interface User" "$DIR/result/api-types.ts" || echo "[HRSM] ⚠ No User interface found"
else
  echo "[HRSM] ✗ api-types.ts not generated"
fi
