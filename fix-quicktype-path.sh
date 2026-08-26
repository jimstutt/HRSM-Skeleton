#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing quicktype path in nix/generate-ts.nix..."

cat > "$DIR/nix/generate-ts.nix" << 'EOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ commonPkg pkgs.quicktype ];
  buildPhase = ''
    mkdir -p frontend/src
    # Generate OpenAPI spec
    runhaskell common/app/GenerateOpenAPI.hs
    # Generate TypeScript from OpenAPI
    quicktype --src-lang openapi --lang typescript \
      --out frontend/src/api-types.ts \
      frontend/openapi.json
  '';
  installPhase = ''
    mkdir -p $out
    cp frontend/src/api-types.ts $out/
  '';
}
EOF

echo "[HRSM] Fixed quicktype path. Run: nix build .#ts-types"
