#!/usr/bin/env bash
set -euo pipefail

VENDOR_DIR="vendor"

echo "Pruning vendored libraries under $VENDOR_DIR ..."

# Loop through each vendored package
for pkg in "$VENDOR_DIR"/*; do
  [ -d "$pkg" ] || continue

  echo "→ Cleaning $pkg"

  # Delete known junk dirs
  rm -rf \
    "$pkg"/bench \
    "$pkg"/benchmarks \
    "$pkg"/examples \
    "$pkg"/example \
    "$pkg"/tests \
    "$pkg"/test \
    "$pkg"/test-suite \
    "$pkg"/scripts \
    "$pkg"/json-data

  # Delete known junk files
  find "$pkg" -maxdepth 1 -type f \( \
      -name "README*" -o \
      -name "CONTRIBUTING*" -o \
      -name "changelog*" -o \
      -name "CHANGELOG*" -o \
      -name "stack*.yaml" -o \
      -name "cabal.project*" -o \
      -name "cabal.haskell-ci" -o \
      -name "Makefile*" -o \
      -name "Setup.hs" -o \
      -name "Setup.lhs" \
    \) -print -delete
done

echo "Prune complete. Keeping only:"
echo "  - *.cabal"
echo "  - src/"
echo "  - LICENSE"
