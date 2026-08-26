#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing GenerateOpenAPI.hs argument parsing and finalizing TS generation..."

# 1. Clean up the accidental directory created by previous runs
rm -rf "$DIR/common/frontend"

# 2. Fix GenerateOpenAPI.hs to handle both `--output PATH` and `--output=PATH`
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
import Data.List (isPrefixOf)

main :: IO ()
main = do
  args <- getArgs
  let outputPath = case args of
        ("--output":path:_) -> path
        (x:_) | "--output=" `isPrefixOf` x -> drop 9 x
        _ -> "frontend/openapi.json"
  
  -- Ensure parent directory exists before writing
  createDirectoryIfMissing True (takeDirectory outputPath)
  
  let spec = toOpenApi (Proxy :: Proxy API)
  BL.writeFile outputPath (encode spec)
  putStrLn $ "[HRSM] Generated " ++ outputPath
EOF

# 3. Ensure root frontend directory exists
mkdir -p "$DIR/frontend"

# 4. Run the generator with absolute path
echo "[HRSM] Running OpenAPI generator..."
nix-shell -p haskellPackages.ghc haskellPackages.cabal-install zlib.dev --run "
  cd $DIR/common && cabal run generate-openapi -- --output=$DIR/frontend/openapi.json
"

if [ -f "$DIR/frontend/openapi.json" ]; then
  echo "[HRSM] ✓ openapi.json successfully generated at root frontend/"
else
  echo "[HRSM] ✗ Failed to generate openapi.json in the correct location"
  exit 1
fi

# 5. Build the ts-types derivation
echo "[HRSM] Building ts-types derivation..."
cd "$DIR"
nix build .#ts-types

# 6. Copy generated TS types to frontend source tree
if [ -f "$DIR/result/api-types.ts" ]; then
  mkdir -p "$DIR/frontend/src"
  cp "$DIR/result/api-types.ts" "$DIR/frontend/src/api-types.ts"
  echo "[HRSM] ✓ api-types.ts copied to frontend/src/"
  
  # Commit the fixes
  git add common/app/GenerateOpenAPI.hs frontend/openapi.json frontend/src/api-types.ts
  git commit -m "[HRSM] Fix OpenAPI generator arg parsing and sync TS types" || true
else
  echo "[HRSM] ⚠ api-types.ts not found in result. The quicktype/jq fallback may need inspection."
fi

echo "[HRSM] ✓ Pipeline complete. Vite HMR will auto-reload if running."
