#!/usr/bin/env bash
set -euo pipefail

echo "Cleaning NGOL-CG bootstrap structure..."

# --- Top-level files ---
rm -f \
  ci.yml.txt \
  ghcid-output.txt \
  home.nix

# --- Directories to remove completely ---
rm -rf \
  src \
  static \
  test \
  wasm \
  wasm-modules

# --- Backend: remove SQLite-specific artifacts ---
rm -rf backend/sql

# --- Remove obsolete WASM/reactor scripts ---
rm -f \
  scripts/build-reactor-wasm.nix \
  scripts/build-reacto-wasi.sh \
  scripts/link-reactor.sh

echo "Cleanup complete."
echo "Review with: git status"
