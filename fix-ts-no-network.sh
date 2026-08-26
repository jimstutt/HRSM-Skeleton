#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Rewriting ts-types derivation to use pre-built common package (no network)..."

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
    # No cabal update needed - all deps resolved by Nix
    generate-openapi --output=frontend/openapi.json
    
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

echo "[HRSM] Derivation rewritten. Committing and rebuilding..."

cd "$DIR"
git add nix/generate-ts.nix
git commit -m "[HRSM] Remove network-dependent cabal update from ts-types derivation" || true
nix build .#ts-types
