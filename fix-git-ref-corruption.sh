#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Recovering corrupted Git refs after system crash..."

cd "$DIR"

# 1. Check if refs/heads/main exists and is valid
if [ ! -s .git/refs/heads/main ]; then
  echo "[HRSM] refs/heads/main is missing or empty. Attempting recovery..."
  
  # Try to recover from packed-refs first
  if [ -f .git/packed-refs ] && grep -q "refs/heads/main" .git/packed-refs; then
    COMMIT=$(grep "refs/heads/main" .git/packed-refs | awk '{print $1}')
    echo "$COMMIT" > .git/refs/heads/main
    echo "[HRSM] ✓ Recovered main ref from packed-refs: $COMMIT"
  else
    # Fall back to HEAD
    if [ -f .git/HEAD ]; then
      HEAD_REF=$(cat .git/HEAD)
      if [[ "$HEAD_REF" == ref:* ]]; then
        REF_PATH=".git/${HEAD_REF#ref: }"
        if [ -s "$REF_PATH" ]; then
          cp "$REF_PATH" .git/refs/heads/main
          echo "[HRSM] ✓ Recovered main ref from HEAD"
        fi
      elif [[ "$HEAD_REF" =~ ^[0-9a-f]{40}$ ]]; then
        echo "$HEAD_REF" > .git/refs/heads/main
        echo "[HRSM] ✓ Recovered main ref from detached HEAD"
      fi
    fi
  fi
fi

# 2. Verify the ref is now valid
if [ -s .git/refs/heads/main ]; then
  COMMIT=$(cat .git/refs/heads/main)
  if git cat-file -t "$COMMIT" &>/dev/null; then
    echo "[HRSM] ✓ Git ref is valid: $COMMIT"
  else
    echo "[HRSM] ✗ Ref points to invalid object. Running git fsck..."
    git fsck --full 2>&1 | head -20
    echo "[HRSM] Attempting to reset to last known good commit..."
    # Find most recent valid commit
    VALID_COMMIT=$(git reflog --format="%H" | head -1)
    if [ -n "$VALID_COMMIT" ]; then
      echo "$VALID_COMMIT" > .git/refs/heads/main
      echo "[HRSM] ✓ Reset main to reflog entry: $VALID_COMMIT"
    fi
  fi
else
  echo "[HRSM] ✗ Could not recover refs/heads/main automatically."
  echo "[HRSM] Manual intervention required. Check .git/reflog for valid commits."
  exit 1
fi

# 3. Test that nix develop works
echo "[HRSM] Testing nix develop..."
if nix flake metadata . &>/dev/null; then
  echo "[HRSM] ✓ Nix flake metadata OK. You can now run 'nix develop'"
else
  echo "[HRSM] ✗ Nix still cannot read flake. Further recovery needed."
  exit 1
fi
