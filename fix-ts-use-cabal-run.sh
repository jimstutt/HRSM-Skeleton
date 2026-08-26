#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Switching ts-types derivation to use cabal run..."

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

# Update GenerateOpenAPI.hs to accept output path as argument
cat > "$DIR/common/app/GenerateOpenAPI.hs" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (API)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let outputPath = case args of
        ("--output":path:_) -> path
        _ -> "frontend/openapi.json"
  let spec = toOpenApi (Proxy :: Proxy API)
  BL.writeFile outputPath (encode spec)
  putStrLn $ "[HRSM] Generated " ++ outputPath
EOF

echo "[HRSM] Derivation and generator updated. Committing and rebuilding..."

cd "$DIR"
git add nix/generate-ts.nix common/app/GenerateOpenAPI.hs
git commit -m "[HRSM] Use cabal run for proper dep resolution in ts-types" || true
nix build .#ts-types
