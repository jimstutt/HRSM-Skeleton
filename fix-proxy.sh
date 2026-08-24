#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Adding explicit Proxy import to common/src/Common/Api.hs..."

cat << 'EOF' > "$DIR/common/src/Common/Api.hs"
module Common.Api
  ( Api
  , api
  ) where

import Data.Proxy (Proxy(..))
import Servant.API ((:>), (:<|>), Get, JSON, ReqBody, Post)
import Common.Types (Task, TaskId)

-- | Shared Servant API definition
type Api = "api" :> "tasks" :> Get '[JSON] [Task]
      :<|> "api" :> "tasks" :> ReqBody '[JSON] Task :> Post '[JSON] TaskId

api :: Proxy Api
api = Proxy
EOF

echo "[HRSM] Common.Api fixed successfully."
echo "Next step: Run 'nix build .#backend' to verify."
