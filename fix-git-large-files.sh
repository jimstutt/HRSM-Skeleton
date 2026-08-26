#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Removing .cabal/ from Git history and preventing future commits..."

cd "$DIR"

# 1. Ensure .cabal/ and .cabal-wasm/ are in .gitignore
if ! grep -q "^\.cabal/" .gitignore 2>/dev/null; then
  echo ".cabal/" >> .gitignore
fi
if ! grep -q "^\.cabal-wasm/" .gitignore 2>/dev/null; then
  echo ".cabal-wasm/" >> .gitignore
fi

# 2. Remove .cabal/ from Git tracking (keeps local files intact)
git rm -r --cached .cabal/ 2>/dev/null || true
git rm -r --cached .cabal-wasm/ 2>/dev/null || true

# 3. Permanently purge .cabal/ from ALL Git history using filter-repo
# This rewrites history to eliminate the 984MB files entirely
if ! command -v git-filter-repo &> /dev/null; then
  echo "[HRSM] Installing git-filter-repo via nix..."
  nix profile install nixpkgs#git-filter-repo
fi

git filter-repo --path .cabal/ --invert-paths --force
git filter-repo --path .cabal-wasm/ --invert-paths --force

# 4. Re-add remote (filter-repo removes it as safety measure)
git remote add origin https://github.com/jimstutt/HRSM-Skeleton.git 2>/dev/null || true

# 5. Commit the .gitignore update
git add .gitignore
git commit -m "[HRSM] Prevent .cabal/ and .cabal-wasm/ from being tracked" || true

echo "[HRSM] History cleaned. Now run: git push --force-with-lease origin main"
