#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Wiring up Servant API in backend..."

# 1. Full replacement of backend/backend.cabal
cat << 'EOF' > "$DIR/backend/backend.cabal"
cabal-version:      3.0
name:               backend
version:            0.1.0.0
build-type:         Simple

library
  exposed-modules:
    Backend,
    DB
  build-depends:
    base >=4.14 && <5,
    common,
    text,
    mysql-simple,
    mtl,
    transformers
  hs-source-dirs:   src
  default-language: Haskell2010
  default-extensions:
    OverloadedStrings
    RecordWildCards

executable backend-exe
  main-is:          Main.hs
  other-modules:      Backend, DB
  build-depends:
    base >=4.14 && <5,
    backend,
    common,
    servant,
    servant-server,
    warp,
    wai
  hs-source-dirs:   app
  default-language: Haskell2010
  default-extensions:
    OverloadedStrings
    RecordWildCards
EOF

# 2. Full replacement of backend/src/Backend.hs
cat << 'EOF' > "$DIR/backend/src/Backend.hs"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Backend
  ( server
  ) where

import Control.Monad.IO.Class (liftIO)
import Servant ((:<|>)(..), Server, Handler)
import Common.Types (Task(..), TaskId)
import Common.Api (API)
import qualified DB

-- | Servant server implementation wiring API to DB functions
server :: DB.DBConn -> Server API
server conn = getTasksHandler :<|> createTaskHandler
  where
    getTasksHandler :: Handler [Task]
    getTasksHandler = liftIO $ DB.getTasks conn

    createTaskHandler :: Task -> Handler TaskId
    createTaskHandler task = liftIO $ DB.createTask conn task
EOF

# 3. Full replacement of backend/app/Main.hs
cat << 'EOF' > "$DIR/backend/app/Main.hs"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Data.Proxy (Proxy(..))
import Common.Api (API)
import Backend (server)
import qualified DB

main :: IO ()
main = do
  putStrLn "[HRSM] Starting backend on port 8080..."
  conn <- DB.initDB
  putStrLn "[HRSM] Database connection established."
  run 8080 (serve (Proxy :: Proxy API) (server conn))
EOF

echo "[HRSM] Servant API wired successfully."
echo "Next step: Run 'nix build .#backend' to verify compilation."
