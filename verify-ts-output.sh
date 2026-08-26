#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Verifying generated TypeScript types..."

# Check if result symlink exists
if [ ! -L "$DIR/result" ]; then
  echo "[HRSM] ERROR: No 'result' symlink found. Run 'nix build .#ts-types' first."
  exit 1
fi

# Display generated types
echo "--- api-types.ts contents ---"
cat "$DIR/result/api-types.ts"
echo "--- end ---"

# Copy to frontend source tree for Vite consumption
mkdir -p "$DIR/frontend/src"
cp "$DIR/result/api-types.ts" "$DIR/frontend/src/api-types.ts"
echo "[HRSM] ✓ Copied api-types.ts to frontend/src/"

# Verify frontend can see the file
if [ -f "$DIR/frontend/src/api-types.ts" ]; then
  echo "[HRSM] ✓ Frontend integration ready"
else
  echo "[HRSM] ERROR: Failed to copy to frontend"
  exit 1
fi
