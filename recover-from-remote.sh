#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Recovering repository from GitHub remote..."

cd "$DIR"

# 1. Backup current working tree (excluding .git)
BACKUP_DIR="$DIR/../HRSM-Skeleton-backup-$(date +%s)"
mkdir -p "$BACKUP_DIR"
rsync -a --exclude='.git' --exclude='node_modules' --exclude='.cabal-wasm' --exclude='dist-newstyle' --exclude='result' ./ "$BACKUP_DIR/"
echo "[HRSM] ✓ Working tree backed up to $BACKUP_DIR"

# 2. Remove corrupted .git directory
rm -rf .git
echo "[HRSM] ✓ Corrupted .git removed"

# 3. Re-clone from GitHub
git clone https://github.com/jimstutt/HRSM-Skeleton.git temp-clone
mv temp-clone/.git .
rm -rf temp-clone
echo "[HRSM] ✓ Fresh .git restored from remote"

# 4. Reset working tree to match remote main
git reset --hard origin/main
echo "[HRSM] ✓ Working tree reset to origin/main"

# 5. Restore any uncommitted changes from backup
# (Only files that exist in backup but not in repo, or are newer)
rsync -av --update --exclude='.git' --exclude='node_modules' --exclude='.cabal-wasm' --exclude='dist-newstyle' --exclude='result' "$BACKUP_DIR/" ./
echo "[HRSM] ✓ Uncommitted changes restored from backup"

# 6. Verify Nix flake works
if nix flake metadata . &>/dev/null; then
  echo "[HRSM] ✓ Repository recovered successfully. 'nix develop' should now work."
else
  echo "[HRSM] ✗ Nix flake still broken. Manual inspection required."
  exit 1
fi

echo "[HRSM] Backup retained at: $BACKUP_DIR (safe to delete after verification)"
