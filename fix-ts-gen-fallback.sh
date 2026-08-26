#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Finding working OpenAPI→TS toolchain in nixpkgs..."

# Check available packages
if nix eval --impure --expr 'builtins.hasAttr "openapi-typescript" pkgs.nodePackages' 2>/dev/null | grep -q true; then
  PKG="pkgs.nodePackages.openapi-typescript"
  CMD="openapi-typescript"
elif nix eval --impure --expr 'builtins.hasAttr "openapi-typescript-codegen" pkgs.python3Packages' 2>/dev/null | grep -q true; then
  PKG="pkgs.python3Packages.openapi-typescript-codegen"
  CMD="openapi-typescript-codegen"
else
  echo "[HRSM] No dedicated OpenAPI TS generator found. Using jq + quicktype on schemas only..."
  # Extract just the component schemas from openapi.json and feed to quicktype as JSON Schema
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
    pkgs.nodePackages.quicktype
  ];
  buildPhase = ''
    mkdir -p frontend/src
    
    # Generate full OpenAPI spec
    generate-openapi --output=frontend/openapi.json
    
    # Extract only component schemas as standalone JSON Schema
    jq '.components.schemas' frontend/openapi.json > frontend/schemas.json
    
    # Generate TypeScript from extracted schemas (quicktype handles JSON Schema natively)
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
  cd "$DIR"
  git add nix/generate-ts.nix
  git commit -m "[HRSM] Fallback: extract schemas with jq for quicktype compatibility" || true
  nix build .#ts-types
  exit 0
fi

# If a dedicated tool was found, use it
cat > "$DIR/nix/generate-ts.nix" << EOF
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    commonPkg
    pkgs.haskellPackages.ghc 
    $PKG
  ];
  buildPhase = ''
    mkdir -p frontend/src
    generate-openapi --output=frontend/openapi.json
    $CMD frontend/openapi.json -o frontend/src/api-types.ts
  '';
  installPhase = ''
    mkdir -p \$out
    cp frontend/src/api-types.ts \$out/
  '';
}
EOF

cd "$DIR"
git add nix/generate-ts.nix
git commit -m "[HRSM] Use discovered OpenAPI TS generator: $CMD" || true
nix build .#ts-types
