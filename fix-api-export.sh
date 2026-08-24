#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

cat << 'EOF' > "$DIR/common/src/Common/Api.hs"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Common.Api
  ( API
  , api
  ) where

import Data.Proxy (Proxy(..))
import Servant.API ((:>), (:<|>), Get, JSON, ReqBody, Post)
import Common.Types (User, UserId)

type API = "api" :> "users" :> Get '[JSON] [User]
      :<|> "api" :> "users" :> ReqBody '[JSON] User :> Post '[JSON] UserId

api :: Proxy API
api = Proxy
EOF

echo "[HRSM] Fixed Common.Api to export API (uppercase)"
