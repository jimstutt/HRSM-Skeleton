#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Restoring GenerateOpenAPI.hs and fixing Types.hs deriving..."

# 1. Fix GenerateOpenAPI.hs import (was reverted by git recovery)
cat > "$DIR/common/app/GenerateOpenAPI.hs" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (API)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let outputPath = case args of
        ("--output":path:_) -> path
        _ -> "frontend/openapi.json"
  let spec = toOpenApi (Proxy :: Proxy API)
  BL.writeFile outputPath (encode spec)
  putStrLn $ "[HRSM] Generated " ++ outputPath
EOF

# 2. Fix Common.Types.hs with correct deriving strategies + userEmail field
cat > "$DIR/common/src/Common/Types.hs" << 'EOF'
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Common.Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.OpenApi (ToSchema, ToParamSchema)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype UserId = UserId Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, ToSchema, ToParamSchema)

data User = User
  { userId    :: UserId
  , userName  :: String
  , userEmail :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
EOF

echo "[HRSM] Files restored. Committing and rebuilding ts-types..."

cd "$DIR"
git add common/app/GenerateOpenAPI.hs common/src/Common/Types.hs
git commit -m "[HRSM] Restore generator import and fix Types deriving after git recovery" || true
nix build .#ts-types
