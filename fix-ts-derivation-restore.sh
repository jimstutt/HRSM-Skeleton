#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Restoring generate-ts.nix with GHC and quicktype..."

cat > "$DIR/nix/generate-ts.nix" << 'EOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    commonPkg
    pkgs.haskellPackages.ghc 
    pkgs.quicktype 
  ];
  buildPhase = ''
    mkdir -p frontend/src
    
    # Run the pre-built generate-openapi executable from commonPkg
    generate-openapi --output=frontend/openapi.json
    
    # Generate TypeScript from OpenAPI (auto-detects format)
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

echo "[HRSM] generate-ts.nix restored. Committing and rebuilding..."

cd "$DIR"
git add nix/generate-ts.nix
git commit -m "[HRSM] Restore generate-ts.nix with GHC after git recovery" || true
nix build .#ts-types
