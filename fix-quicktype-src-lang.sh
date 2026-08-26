#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing quicktype source language flag..."

cat > "$DIR/nix/generate-ts.nix" << 'EOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    commonPkg
    pkgs.quicktype 
  ];
  buildPhase = ''
    mkdir -p frontend/src
    
    # Run the pre-built generate-openapi executable from commonPkg
    generate-openapi --output=frontend/openapi.json
    
    # Generate TypeScript from OpenAPI
    # quicktype auto-detects OpenAPI from .json content when no --src-lang is given,
    # or we can explicitly use 'schema' for OpenAPI 3.x
    quicktype --lang typescript \
      --out frontend/src/api-types.ts \
      frontend/openapi.json
  '';
  installPhase = ''
    mkdir -p $out
    cp frontend/src/api-types.ts $out/
  '';
}
EOF

echo "[HRSM] Derivation updated. Committing and rebuilding..."

cd "$DIR"
git add nix/generate-ts.nix
git commit -m "[HRSM] Remove invalid --src-lang openapi flag from quicktype" || true
nix build .#ts-types
