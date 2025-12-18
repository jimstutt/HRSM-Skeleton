#!/usr/bin/env bash
set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"

mkdir -p "$REPO/dist-wasm"
mkdir -p "$REPO/frontend-wasm"
mkdir -p "$REPO/backend"
mkdir -p "$REPO/scripts"
mkdir -p "$REPO/pkgs"

echo "Project tree initialised."
