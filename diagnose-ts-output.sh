#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Diagnosing api-types.ts generation..."

# Show first 50 lines of generated file
echo "--- api-types.ts (first 50 lines) ---"
head -50 "$DIR/result/api-types.ts"
echo "--- end ---"

# Check if schemas.json was extracted correctly during build
if [ -f "$DIR/frontend/schemas.json" ]; then
  echo ""
  echo "--- schemas.json content ---"
  cat "$DIR/frontend/schemas.json"
  echo "--- end ---"
else
  echo ""
  echo "[HRSM] ⚠ frontend/schemas.json not found (may be in Nix store only)"
fi

# Show raw openapi.json schemas section
if [ -f "$DIR/frontend/openapi.json" ]; then
  echo ""
  echo "--- openapi.json components.schemas ---"
  jq '.components.schemas' "$DIR/frontend/openapi.json" 2>/dev/null || echo "jq failed or key missing"
  echo "--- end ---"
else
  echo ""
  echo "[HRSM] ⚠ frontend/openapi.json not found"
fi
