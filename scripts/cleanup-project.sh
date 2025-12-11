#!/usr/bin/env bash
#
# cleanup-project.sh
#
# Safely remove files and directories that are NOT needed for the
# GHC + Reflex + Servant + sqlite-simple + WASI stack (C).
#
# Supports:
#   ./scripts/cleanup-project.sh
#   ./scripts/cleanup-project.sh --force
#
# ALWAYS review the list before confirming deletions.
#

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

FORCE=0
if [ "${1:-}" = "--force" ]; then
  FORCE=1
fi

cd "$REPO_ROOT"

echo "--------------------------------------------------------"
echo " NGOLogisticsCG Cleanup Script - stack (C)"
echo "--------------------------------------------------------"
echo "Repository root: $REPO_ROOT"
echo

TO_DELETE=()

### ---------------------------------------------------------
### 1. Remove vendor directories
### ---------------------------------------------------------
if [ -d vendor ]; then
  TO_DELETE+=("vendor")
fi

### ---------------------------------------------------------
### 2. Remove Obelisk remnants
### ---------------------------------------------------------
OBELISK_PATTERNS=(
  ".obelisk"
  "obelisk"
  "frontend/.obelisk"
  "backend/.obelisk"
  "common/.obelisk"
  "logistics-common"
  "ob-command"
  "obelisk-command"
)

for p in "${OBELISK_PATTERNS[@]}"; do
  if [ -e "$p" ]; then
    TO_DELETE+=("$p")
  fi
done

### ---------------------------------------------------------
### 3. Remove WASM stubs not part of the new (C) structure
### ---------------------------------------------------------
# These are files created earlier that are now obsolete.
UNNEEDED_WASM_FILES=(
  "scripts/serve-wasm.js"
  "frontend/loader.ts"
  "static/frontend"
  "dist-reactor"
  "build/reactor.wasm"
)

for p in "${UNNEEDED_WASM_FILES[@]}"; do
  if [ -e "$p" ]; then
    TO_DELETE+=("$p")
  fi
done

### ---------------------------------------------------------
### 4. Remove Node-only runtime files you no longer need
###    (because NGOLogisticsCG uses Servant + sqlite, not JS backend)
### ---------------------------------------------------------
NODE_UNUSED=(
  "package.json"
  "package-lock.json"
  "node_modules"
  "scripts/fix-esm-lie?.sh"
  "scripts/fix-esm-lies.sh"
  "*.mjs"
  "*.cjs"
)

for p in "${NODE_UNUSED[@]}"; do
  for f in $(find . -maxdepth 1 -type f -name "$p" 2>/dev/null || true); do
    TO_DELETE+=("$f")
  done
  if [ -d "$p" ]; then
    TO_DELETE+=("$p")
  fi
done

### ---------------------------------------------------------
### 5. Remove unused backend/frontend directories from prior layouts
### ---------------------------------------------------------
REDUNDANT_DIRS=(
  "backend-old"
  "frontend-old"
  "wasm-old"
  "dummy"
  "tmp"
)

for p in "${REDUNDANT_DIRS[@]}"; do
  if [ -e "$p" ]; then
    TO_DELETE+=("$p")
  fi
done

### ---------------------------------------------------------
### 6. Deduplicate list
### ---------------------------------------------------------
unique() {
  printf "%s\n" "$@" | awk '!seen[$0]++'
}

TO_DELETE=( $(unique "${TO_DELETE[@]}") )

### ---------------------------------------------------------
### 7. Print items to delete
### ---------------------------------------------------------
if [ ${#TO_DELETE[@]} -eq 0 ]; then
  echo "No unnecessary files found. Your project is clean."
  exit 0
fi

echo "The script will remove the following files/directories:"
printf "  - %s\n" "${TO_DELETE[@]}"
echo

### ---------------------------------------------------------
### 8. Confirm unless --force
### ---------------------------------------------------------
if [ "$FORCE" -eq 0 ]; then
  read -r -p "Proceed with deletion? (y/N) " ans
  case "$ans" in
    y|Y) ;;
    *) echo "Aborting."; exit 1 ;;
  esac
fi

### ---------------------------------------------------------
### 9. Delete
### ---------------------------------------------------------
echo
echo "Deleting..."
for p in "${TO_DELETE[@]}"; do
  echo "  removing $p"
  rm -rf "$p"
done

echo
echo "Cleanup complete."
