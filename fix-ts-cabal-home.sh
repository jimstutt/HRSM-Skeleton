#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Setting writable HOME/CABAL_DIR in ts-types derivation..."

cat > "$DIR/nix/generate-ts.nix" << 'EOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.ghc 
    pkgs.quicktype 
  ];
  buildPhase = ''
    # Set up writable cabal environment for sandboxed build
    export HOME="$TMPDIR/home"
    export CABAL_DIR="$TMPDIR/cabal"
    mkdir -p "$HOME" "$CABAL_DIR"
    
    mkdir -p frontend/src
    
    # Use cabal run to properly resolve deps and source paths
    cd common
    cabal update
    cabal run generate-openapi -- --output=../frontend/openapi.json
    
    cd ..
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

echo "[HRSM] Derivation updated with writable HOME/CABAL_DIR. Committing and rebuilding..."

cd "$DIR"
git add nix/generate-ts.nix
git commit -m "[HRSM] Set writable HOME/CABAL_DIR in ts-types derivation" || true
nix build .#ts-types
