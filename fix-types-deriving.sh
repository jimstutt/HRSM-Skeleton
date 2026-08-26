#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing deriving strategies in Types.hs..."

cat > "$DIR/common/src/Common/Types.hs" << 'EOF'
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Common.Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype UserId = UserId Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, ToSchema)

data User = User
  { userId :: UserId
  , userName :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)
  deriving (ToJSON, FromJSON) via (Generic User)
EOF

echo "[HRSM] Types.hs fixed with correct deriving strategies."
echo "[HRSM] Committing and rebuilding..."

cd "$DIR"
git add common/src/Common/Types.hs
git commit -m "[HRSM] Fix deriving strategies for ToSchema compatibility" || true
nix build .#ts-types
