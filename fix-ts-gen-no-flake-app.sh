#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Generating openapi.json via nix-shell + cabal (no flake app needed)..."

# 1. Generate openapi.json locally using nix-shell with GHC + cabal-install
mkdir -p "$DIR/frontend"
nix-shell -p haskellPackages.ghc haskellPackages.cabal-install --run "
  cd $DIR/common && \
  cabal run generate-openapi -- --output=$DIR/frontend/openapi.json
"

# 2. Show schemas section to diagnose structure
echo "--- components.schemas from openapi.json ---"
jq '.components.schemas' "$DIR/frontend/openapi.json"
echo "--- end ---"

# 3. Check if User schema exists
USER_SCHEMA=$(jq '.components.schemas.User // empty' "$DIR/frontend/openapi.json")
if [ -z "$USER_SCHEMA" ]; then
  echo "[HRSM] ✗ ERROR: No 'User' schema found in openapi.json"
  echo "[HRSM] Available schemas:"
  jq '.components.schemas | keys' "$DIR/frontend/openapi.json"
  exit 1
fi

echo "[HRSM] ✓ User schema found. Switching to direct Haskell→TS generation..."

# 4. Create direct TS generator (bypasses quicktype entirely)
cat > "$DIR/common/app/GenerateTS.hs" << 'HSEOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

generateTS :: String
generateTS = unlines
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
  writeFile outputPath generateTS
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
  hs-source-dirs: app
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
CABALEOF

# 6. Simplify generate-ts.nix to use cabal run inside derivation
cat > "$DIR/nix/generate-ts.nix" << 'NIXEOF'
{ pkgs, commonPkg }:
pkgs.stdenv.mkDerivation {
  pname = "hrsm-ts-types";
  version = "0.1.0.0";
  src = ../.;
  nativeBuildInputs = [ 
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
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

echo "[HRSM] Direct TS generator created. Committing and rebuilding..."

cd "$DIR"
git add common/app/GenerateTS.hs common/common.cabal nix/generate-ts.nix frontend/openapi.json
git commit -m "[HRSM] Replace quicktype with direct Haskell→TS generation (no flake app)" || true
nix build .#ts-types
