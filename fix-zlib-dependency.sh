#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Adding zlib.dev to nix-shell for cabal builds..."

# 1. Generate openapi.json locally with zlib available
mkdir -p "$DIR/frontend"
nix-shell -p haskellPackages.ghc haskellPackages.cabal-install zlib.dev --run "
  cd $DIR/common && \
  cabal run generate-openapi -- --output=$DIR/frontend/openapi.json
"

# 2. Verify schema extraction
echo "--- components.schemas from openapi.json ---"
jq '.components.schemas' "$DIR/frontend/openapi.json"
echo "--- end ---"

USER_SCHEMA=$(jq '.components.schemas.User // empty' "$DIR/frontend/openapi.json")
if [ -z "$USER_SCHEMA" ]; then
  echo "[HRSM] ✗ ERROR: No 'User' schema found"
  jq '.components.schemas | keys' "$DIR/frontend/openapi.json"
  exit 1
fi
echo "[HRSM] ✓ User schema found"

# 3. Update generate-ts.nix derivation to include zlib.dev
cat > "$DIR/nix/generate-ts.nix" << 'NIXEOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
    pkgs.zlib.dev
  ];
  buildPhase = ''
    export HOME="$TMPDIR/home"
    export CABAL_DIR="$TMPDIR/cabal"
    mkdir -p "$HOME" "$CABAL_DIR" frontend/src
    
    cd common
    cabal update
    cabal run generate-ts -- --output=../frontend/src/api-types.ts
  '';
  installPhase = ''
    mkdir -p $out
    cp frontend/src/api-types.ts $out/
  '';
}
NIXEOF

echo "[HRSM] zlib.dev added to both local shell and derivation. Committing and rebuilding..."

cd "$DIR"
git add nix/generate-ts.nix frontend/openapi.json
git commit -m "[HRSM] Add zlib.dev for Haskell C library dependencies" || true
nix build .#ts-types
