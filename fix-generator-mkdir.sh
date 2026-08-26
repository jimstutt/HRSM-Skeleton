#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Updating GenerateOpenAPI.hs to auto-create output directories..."

cat > "$DIR/common/app/GenerateOpenAPI.hs" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (API)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  args <- getArgs
  let outputPath = case args of
        ("--output":path:_) -> path
        _ -> "frontend/openapi.json"
  
  -- Ensure parent directory exists before writing
  createDirectoryIfMissing True (takeDirectory outputPath)
  
  let spec = toOpenApi (Proxy :: Proxy API)
  BL.writeFile outputPath (encode spec)
  putStrLn $ "[HRSM] Generated " ++ outputPath
EOF

# Update cabal to include directory and filepath deps for the executable
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
    , directory
    , filepath

executable generate-ts
  main-is: GenerateTS.hs
  hs-source-dirs: app
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
CABALEOF

echo "[HRSM] Generator updated with auto-mkdir. Rebuilding locally..."

mkdir -p "$DIR/frontend"
nix-shell -p haskellPackages.ghc haskellPackages.cabal-install zlib.dev --run "
  cd $DIR/common && \
  cabal run generate-openapi -- --output=$DIR/frontend/openapi.json
"

if [ -f "$DIR/frontend/openapi.json" ]; then
  echo "[HRSM] ✓ openapi.json generated successfully"
  jq '.components.schemas | keys' "$DIR/frontend/openapi.json"
else
  echo "[HRSM] ✗ Generation still failed"
  exit 1
fi

cd "$DIR"
git add common/app/GenerateOpenAPI.hs common/common.cabal frontend/openapi.json
git commit -m "[HRSM] Auto-create output dirs in GenerateOpenAPI.hs" || true
nix build .#ts-types
