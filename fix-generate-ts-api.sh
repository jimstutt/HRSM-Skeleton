#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing GenerateTS.hs to use correct servant-typescript API..."

cat > "$DIR/common/app/GenerateTS.hs" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (api)
import Servant.TypeScript (getTypeScript)
import System.IO (writeFile)

main :: IO ()
main = do
  let tsCode = getTypeScript api
  writeFile "frontend/src/api-types.ts" tsCode
  putStrLn "[HRSM] Generated frontend/src/api-types.ts"
EOF

echo "[HRSM] GenerateTS.hs fixed. Rebuilding ts-types..."
