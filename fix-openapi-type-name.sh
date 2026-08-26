#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Diagnosing Common.Api type exports..."

# Find the actual API type definition
API_LINE=$(grep -n "^type.*=.*:\|>" "$DIR/common/src/Common/Api.hs" | head -1 || true)
if [ -z "$API_LINE" ]; then
  echo "[HRSM] ERROR: Could not find API type definition in Common.Api.hs"
  echo "[HRSM] Contents of Common/Api.hs:"
  cat "$DIR/common/src/Common/Api.hs"
  exit 1
fi

echo "[HRSM] Found API definition: $API_LINE"

# Extract just the type name (e.g., "API" from "type API = ...")
TYPE_NAME=$(echo "$API_LINE" | sed 's/.*type \([A-Za-z0-9_]*\).*/\1/')
echo "[HRSM] Using type name: $TYPE_NAME"

cat > "$DIR/common/app/GenerateOpenAPI.hs" << EOF
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api ($TYPE_NAME)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))

main :: IO ()
main = do
  let spec = toOpenApi (Proxy :: Proxy $TYPE_NAME)
  BL.writeFile "frontend/openapi.json" (encode spec)
  putStrLn "[HRSM] Generated frontend/openapi.json"
EOF

echo "[HRSM] GenerateOpenAPI.hs updated with correct type: $TYPE_NAME"
echo "[HRSM] Committing and rebuilding..."

cd "$DIR"
git add common/app/GenerateOpenAPI.hs
git commit -m "[HRSM] Use explicit $TYPE_NAME type for OpenAPI generation" || true
nix build .#ts-types
