#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing quicktype package path to top-level..."

cat > "$DIR/nix/generate-ts.nix" << 'EOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    commonPkg
    pkgs.haskellPackages.ghc 
    pkgs.jq
    pkgs.quicktype
  ];
  buildPhase = ''
    mkdir -p frontend/src
    
    # Generate full OpenAPI spec
    generate-openapi --output=frontend/openapi.json
    
    # Extract only component schemas as standalone JSON Schema
    jq '.components.schemas' frontend/openapi.json > frontend/schemas.json
    
    # Generate TypeScript from extracted schemas
    quicktype --src-lang schema --lang typescript \
      --out frontend/src/api-types.ts \
      frontend/schemas.json
  '';
  installPhase = ''
    mkdir -p $out
    cp frontend/src/api-types.ts $out/
  '';
}
EOF

echo "[HRSM] Derivation fixed with top-level quicktype. Committing and rebuilding..."

cd "$DIR"
git add nix/generate-ts.nix
git commit -m "[HRSM] Use top-level pkgs.quicktype (nodePackages removed)" || true
nix build .#ts-types
