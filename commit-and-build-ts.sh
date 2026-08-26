#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Committing new OpenAPI generator files to make them visible to Nix..."

cd "$DIR"
git add common/app/GenerateOpenAPI.hs common/common.cabal nix/generate-ts.nix
git commit -m "[HRSM] Add OpenAPI + quicktype TS generation pipeline"

echo "[HRSM] Files committed. Now building ts-types..."
nix build .#ts-types
