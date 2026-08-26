#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Switching TS generation to OpenAPI + quicktype (stable approach)..."

# 1. Update common.cabal to use openapi3 instead of servant-typescript
cat > "$DIR/common/common.cabal" << 'EOF'
cabal-version: 3.0
name: common
version: 0.1.0.0
build-type: Simple

library
  exposed-modules:
      Common.Api
    , Common.Types
  hs-source-dirs: src
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , servant
    , servant-openapi3
    , openapi3
    , text
    , aeson

executable generate-openapi
  main-is: GenerateOpenAPI.hs
  other-modules:
      Common.Api
    , Common.Types
  hs-source-dirs: app, src
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , common
    , servant-openapi3
    , openapi3
    , aeson
    , bytestring
    , text
EOF

# 2. Create the OpenAPI generator
mkdir -p "$DIR/common/app"
cat > "$DIR/common/app/GenerateOpenAPI.hs" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (api)
import Data.OpenApi (toOpenApi)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))

main :: IO ()
main = do
  let spec = toOpenApi (Proxy :: Proxy (Common.Api.API))
  BL.writeFile "frontend/openapi.json" (encode spec)
  putStrLn "[HRSM] Generated frontend/openapi.json"
EOF

# 3. Update nix/generate-ts.nix to use quicktype
cat > "$DIR/nix/generate-ts.nix" << 'EOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ commonPkg pkgs.nodePackages.quicktype ];
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

echo "[HRSM] Switched to OpenAPI + quicktype. Run: nix build .#ts-types"
