#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Adding ToSchema instance for User type..."

cat > "$DIR/common/src/Common/Types.hs" << 'EOF'
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
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)
  deriving anyclass (ToSchema)

data User = User
  { userId :: UserId
  , userName :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
EOF

echo "[HRSM] Types.hs updated with ToSchema instances."
echo "[HRSM] Committing and rebuilding..."

cd "$DIR"
git add common/src/Common/Types.hs
git commit -m "[HRSM] Add ToSchema instances for OpenAPI generation" || true
nix build .#ts-types
