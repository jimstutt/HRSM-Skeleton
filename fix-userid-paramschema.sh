#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Adding ToParamSchema instance for UserId..."

cat > "$DIR/common/src/Common/Types.hs" << 'EOF'
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Common.Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.OpenApi (ToSchema, ToParamSchema)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype UserId = UserId Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, ToSchema, ToParamSchema)

data User = User
  { userId :: UserId
  , userName :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
EOF

echo "[HRSM] Types.hs updated with ToParamSchema for UserId."
echo "[HRSM] Committing and rebuilding..."

cd "$DIR"
git add common/src/Common/Types.hs
git commit -m "[HRSM] Add ToParamSchema instance for UserId capture param" || true
nix build .#ts-types
