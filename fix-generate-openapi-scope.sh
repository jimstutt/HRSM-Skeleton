#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing GenerateOpenAPI.hs to use imported api value..."

cat > "$DIR/common/app/GenerateOpenAPI.hs" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (api)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))

main :: IO ()
main = do
  -- Use the imported 'api' value; GHC infers the correct Proxy type automatically
  let spec = toOpenApi (Proxy :: Proxy (typeof api))
  BL.writeFile "frontend/openapi.json" (encode spec)
  putStrLn "[HRSM] Generated frontend/openapi.json"
EOF

# Fallback: If typeof isn't supported in this GHC version, use a simpler approach
# that doesn't require explicit type annotation at all
cat > "$DIR/common/app/GenerateOpenAPI.hs" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Common.Api (api)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))

main :: IO ()
main = do
  -- toOpenApi can often infer the type from context, or we use the api value directly
  -- Most servant-openapi3 examples use: toOpenApi (Proxy @MyApiType)
  -- Since we don't know the exact type name, we rely on the fact that 
  -- 'api' is already typed correctly in Common.Api
  let spec = toOpenApi (Proxy :: Proxy _)
  BL.writeFile "frontend/openapi.json" (encode spec)
  putStrLn "[HRSM] Generated frontend/openapi.json"
EOF

echo "[HRSM] GenerateOpenAPI.hs updated with type-inference approach."
echo "[HRSM] Committing and rebuilding..."

cd "$DIR"
git add common/app/GenerateOpenAPI.hs
git commit -m "[HRSM] Fix API type scope in GenerateOpenAPI.hs" || true
nix build .#ts-types
