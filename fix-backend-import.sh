#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

cat << 'EOF' > "$DIR/backend/src/Backend.hs"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Backend
  ( server
  ) where

import Control.Monad.IO.Class (liftIO)
import Servant ((:<|>)(..), Server, Handler)
import Common.Types (User, UserId)
import Common.Api (API)
import qualified DB

server :: DB.DBConn -> Server API
server conn = getUsersHandler :<|> createUserHandler
  where
    getUsersHandler :: Handler [User]
    getUsersHandler = liftIO $ DB.getUsers conn

    createUserHandler :: User -> Handler UserId
    createUserHandler user = liftIO $ DB.createUser conn user
EOF

echo "[HRSM] Fixed Backend.hs to import API (uppercase)"
