#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing GenerateOpenAPI.hs with correct API type..."

cat > "$DIR/common/app/GenerateOpenAPI.hs" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (API)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))

main :: IO ()
main = do
  let spec = toOpenApi (Proxy :: Proxy API)
  BL.writeFile "frontend/openapi.json" (encode spec)
  putStrLn "[HRSM] Generated frontend/openapi.json"
EOF

echo "[HRSM] GenerateOpenAPI.hs fixed. Committing and rebuilding..."

cd "$DIR"
git add common/app/GenerateOpenAPI.hs
git commit -m "[HRSM] Use correct API type for OpenAPI generation" || true
nix build .#ts-types
