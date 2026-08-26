#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Setting up Hybrid Architecture: Servant -> TypeScript type generation..."

# 1. Add servant-typescript to common/common.cabal
sed -i '/build-depends:/a\    , servant-typescript' "$DIR/common/common.cabal"

# 2. Create the TS generator executable in common/
mkdir -p "$DIR/common/app"
cat > "$DIR/common/app/GenerateTS.hs" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (api)
import Servant.TypeScript (generateTypeScript)
import System.IO (writeFile)

main :: IO ()
main = do
  let tsCode = generateTypeScript api
  writeFile "frontend/src/api-types.ts" tsCode
  putStrLn "[HRSM] Generated frontend/src/api-types.ts"
EOF

# 3. Update common/common.cabal to include the generator executable
cat >> "$DIR/common/common.cabal" << 'EOF'

executable generate-ts
  main-is: GenerateTS.hs
  hs-source-dirs: app, src
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , common
    , servant-typescript
EOF

# 4. Create Nix derivation to auto-generate TS types on build
cat > "$DIR/nix/generate-ts.nix" << 'EOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ commonPkg ];
  buildPhase = ''
    mkdir -p frontend/src
    runhaskell common/app/GenerateTS.hs
  '';
  installPhase = ''
    mkdir -p $out
    cp frontend/src/api-types.ts $out/
  '';
}
EOF

# 5. Update flake.nix to expose the TS generator
sed -i '/packages = {/a\          ts-types = pkgs.callPackage ./nix/generate-ts.nix { inherit commonPkg; };' "$DIR/flake.nix"

echo "[HRSM] Hybrid setup complete. Run 'nix build .#ts-types' to generate API types."
