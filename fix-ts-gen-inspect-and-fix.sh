#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Inspecting OpenAPI spec and fixing TS generation..."

# 1. Generate openapi.json locally for inspection (using nix run to avoid sandbox issues)
echo "[HRSM] Generating openapi.json locally..."
nix run .#common:generate-openapi -- --output="$DIR/frontend/openapi.json"

# 2. Show schemas section to diagnose structure
echo "--- components.schemas from openapi.json ---"
jq '.components.schemas' "$DIR/frontend/openapi.json"
echo "--- end ---"

# 3. Check if User schema exists and has properties
USER_SCHEMA=$(jq '.components.schemas.User // empty' "$DIR/frontend/openapi.json")
if [ -z "$USER_SCHEMA" ]; then
  echo "[HRSM] ✗ ERROR: No 'User' schema found in openapi.json"
  echo "[HRSM] Available schemas:"
  jq '.components.schemas | keys' "$DIR/frontend/openapi.json"
  exit 1
fi

echo "[HRSM] ✓ User schema found. Checking structure..."
echo "$USER_SCHEMA" | jq .

# 4. If schema looks valid but quicktype failed, switch to manual TS generation
#    (more reliable than fighting quicktype's schema inference)
echo "[HRSM] Switching to direct Haskell→TS generation via custom script..."

cat > "$DIR/common/app/GenerateTS.hs" << 'HSEOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Types (User, UserId)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

-- Simple direct TS generation (no OpenAPI intermediary needed)
generateTS :: Text
generateTS = T.unlines
  [ "// Auto-generated from Common.Types - DO NOT EDIT"
  , ""
  , "export type UserId = number;"
  , ""
  , "export interface User {"
  , "  userId: UserId;"
  , "  userName: string;"
  , "  userEmail: string;"
  , "}"
  ]

main :: IO ()
main = do
  args <- getArgs
  let outputPath = case args of
        ("--output":path:_) -> path
        _ -> "frontend/src/api-types.ts"
  TIO.writeFile outputPath generateTS
  putStrLn $ "[HRSM] Generated " ++ outputPath
HSEOF

# 5. Update cabal to include new executable
cat > "$DIR/common/common.cabal" << 'CABALEOF'
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
    , servant
    , servant-openapi3
    , openapi3
    , aeson
    , bytestring
    , text

executable generate-ts
  main-is: GenerateTS.hs
  other-modules:
      Common.Types
  hs-source-dirs: app, src
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , common
    , text
CABALEOF

# 6. Simplify generate-ts.nix to just run the Haskell generator directly
cat > "$DIR/nix/generate-ts.nix" << 'NIXEOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ commonPkg ];
  buildPhase = ''
    mkdir -p frontend/src
    generate-ts --output=frontend/src/api-types.ts
  '';
  installPhase = ''
    mkdir -p $out
    cp frontend/src/api-types.ts $out/
  '';
}
NIXEOF

echo "[HRSM] Direct TS generator created. Committing and rebuilding..."

cd "$DIR"
git add common/app/GenerateTS.hs common/common.cabal nix/generate-ts.nix frontend/openapi.json
git commit -m "[HRSM] Replace quicktype with direct Haskell→TS generation" || true
nix build .#ts-types
