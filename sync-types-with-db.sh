#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Synchronizing Common.Types.User with MariaDB schema..."

# 1. Update Common.Types to match DB schema (id, name, email)
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
  { userId    :: UserId
  , userName  :: String
  , userEmail :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
EOF

# 2. Regenerate TypeScript types from updated API
echo "[HRSM] Rebuilding ts-types derivation..."
cd "$DIR"
nix build .#ts-types

# 3. Copy regenerated types to frontend
mkdir -p "$DIR/frontend/src"
cp result/api-types.ts "$DIR/frontend/src/api-types.ts"
echo "[HRSM] ✓ api-types.ts regenerated and copied to frontend/"

# 4. Commit all changes
git add common/src/Common/Types.hs frontend/src/api-types.ts
git commit -m "[HRSM] Sync User type with MariaDB schema (add userEmail field)" || true

echo "[HRSM] ✓ Types synchronized. Vite HMR will auto-reload frontend."
echo "[HRSM] ⚠ Backend may need restart to pick up new field serialization."
