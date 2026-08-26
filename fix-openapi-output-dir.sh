#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Ensuring frontend/ directory exists before generating openapi.json..."

# 1. Create output directory and generate openapi.json
mkdir -p "$DIR/frontend"
nix-shell -p haskellPackages.ghc haskellPackages.cabal-install zlib.dev --run "
  cd $DIR/common && \
  cabal run generate-openapi -- --output=$DIR/frontend/openapi.json
"

# 2. Verify generation succeeded
if [ ! -f "$DIR/frontend/openapi.json" ]; then
  echo "[HRSM] ✗ ERROR: openapi.json was not generated"
  exit 1
fi

echo "--- components.schemas from openapi.json ---"
jq '.components.schemas' "$DIR/frontend/openapi.json"
echo "--- end ---"

USER_SCHEMA=$(jq '.components.schemas.User // empty' "$DIR/frontend/openapi.json")
if [ -z "$USER_SCHEMA" ]; then
  echo "[HRSM] ✗ ERROR: No 'User' schema found"
  jq '.components.schemas | keys' "$DIR/frontend/openapi.json"
  exit 1
fi
echo "[HRSM] ✓ User schema found. Proceeding with TS generation..."

# 3. Rebuild ts-types derivation (now that openapi.json exists for inspection)
cd "$DIR"
nix build .#ts-types

echo "[HRSM] ✓ Build complete. Verify with: cat result/api-types.ts"
