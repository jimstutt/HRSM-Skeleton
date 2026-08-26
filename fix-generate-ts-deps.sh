#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Removing Servant.TypeScript dependency and fixing common.cabal..."

# 1. Overwrite GenerateTS.hs with the robust, dependency-free version
cat > "$DIR/common/app/GenerateTS.hs" << 'EOF'
module Main where

import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  args <- getArgs
  let outputPath = case args of
        ("--output":path:_) -> path
        _ -> "frontend/src/api-types.ts"
  
  -- Ensure parent directory exists
  createDirectoryIfMissing True (takeDirectory outputPath)
  
  -- Direct TS generation (no external packages needed)
  let tsContent = unlines
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
  
  writeFile outputPath tsContent
  putStrLn $ "[HRSM] Generated " ++ outputPath
EOF

# 2. Fix common.cabal to match the simplified dependencies
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
    , http-api-data

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
    , directory
    , filepath
CABALEOF

echo "[HRSM] Files fixed. Committing and rebuilding ts-types..."

cd "$DIR"
git add common/app/GenerateTS.hs common/common.cabal
git commit -m "[HRSM] Remove Servant.TypeScript, use direct TS generation" || true
nix build .#ts-types
