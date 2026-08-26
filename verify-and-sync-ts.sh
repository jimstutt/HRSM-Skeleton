#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Verifying ts-types build and syncing to frontend..."

cd "$DIR"

# 1. Check if the build succeeded and result symlink exists
if [ -L "result" ] && [ -f "result/api-types.ts" ]; then
  echo "[HRSM] ✓ Build successful. Generated api-types.ts:"
  echo "---"
  cat "result/api-types.ts"
  echo "---"
  
  # 2. Sync to frontend
  mkdir -p frontend/src
  cp "result/api-types.ts" frontend/src/api-types.ts
  echo "[HRSM] ✓ Synced to frontend/src/api-types.ts (Vite HMR will auto-reload)"
  
  # 3. Commit the synced file and clean up the dirty tree warning
  git add frontend/src/api-types.ts frontend/openapi.json .gitignore 2>/dev/null || true
  git commit -m "[HRSM] Sync generated api-types.ts to frontend and clean tree" || true
else
  echo "[HRSM] ✗ Build failed or result symlink missing."
  echo "[HRSM] Re-running nix build to see errors..."
  nix build .#ts-types 2>&1 | tail -n 30
fi
