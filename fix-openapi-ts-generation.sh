#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Diagnosing and fixing OpenAPI → TypeScript generation..."

# Check if quicktype supports openapi src-lang
if nix shell nixpkgs#quicktype --run "quicktype --help" 2>/dev/null | grep -q "openapi"; then
  echo "[HRSM] ✓ quicktype supports --src-lang openapi. Restoring flag..."
  SRC_LANG_FLAG="--src-lang openapi"
else
  echo "[HRSM] ⚠ quicktype lacks openapi support. Switching to openapi-typescript..."
  # Add openapi-typescript to derivation and use it instead
  cat > "$DIR/nix/generate-ts.nix" << 'EOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    commonPkg
    pkgs.haskellPackages.ghc 
    pkgs.nodePackages.openapi-typescript
  ];
  buildPhase = ''
    mkdir -p frontend/src
    
    # Generate OpenAPI spec
    generate-openapi --output=frontend/openapi.json
    
    # Generate TypeScript using openapi-typescript (proper OpenAPI support)
    openapi-typescript frontend/openapi.json -o frontend/src/api-types.ts
  '';
  installPhase = ''
    mkdir -p $out
    cp frontend/src/api-types.ts $out/
  '';
}
EOF
  cd "$DIR"
  git add nix/generate-ts.nix
  git commit -m "[HRSM] Switch to openapi-typescript for proper OpenAPI→TS generation" || true
  nix build .#ts-types
  exit 0
fi

# If quicktype supports openapi, restore the flag in existing derivation
cat > "$DIR/nix/generate-ts.nix" << EOF
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
    
    # Generate OpenAPI spec
    generate-openapi --output=frontend/openapi.json
    
    # Generate TypeScript with explicit openapi source language
    quicktype $SRC_LANG_FLAG --lang typescript \\
      --out frontend/src/api-types.ts \\
      frontend/openapi.json
  '';
  installPhase = ''
    mkdir -p \$out
    cp frontend/src/api-types.ts \$out/
  '';
}
EOF

cd "$DIR"
git add nix/generate-ts.nix
git commit -m "[HRSM] Restore --src-lang openapi flag for quicktype" || true
nix build .#ts-types
