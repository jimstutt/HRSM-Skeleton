#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Simplifying Types.hs deriving to use only Generic + DeriveAnyClass..."

cat > "$DIR/common/src/Common/Types.hs" << 'EOF'
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Common.Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype UserId = UserId Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, ToSchema)

data User = User
  { userId :: UserId
  , userName :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
EOF

echo "[HRSM] Types.hs simplified. Committing and rebuilding..."

cd "$DIR"
git add common/src/Common/Types.hs
git commit -m "[HRSM] Simplify deriving to Generic + DeriveAnyClass only" || true
nix build .#ts-types
