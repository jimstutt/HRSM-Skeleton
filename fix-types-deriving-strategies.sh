#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Adding DerivingStrategies extension to Common.Types.hs..."

cat > "$DIR/common/src/Common/Types.hs" << 'EOF'
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

echo "[HRSM] Types.hs fixed with DerivingStrategies. Committing and rebuilding..."

cd "$DIR"
git add common/src/Common/Types.hs
git commit -m "[HRSM] Add DerivingStrategies extension for multiple deriving clauses" || true
nix build .#ts-types
