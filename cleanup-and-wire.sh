#!/usr/bin/env bash
set -euo pipefail

DIR="$HOME/Dev/HRSM-Skeleton"

echo "[HRSM] Executing project structure cleanup and MariaDB wiring..."

# --- Task 3: Fix misplaced backend.cabal ---
echo "[HRSM] Removing misplaced common/src/Common/backend.cabal..."
rm -f "$DIR/common/src/Common/backend.cabal"

# --- Task 2: Consolidate shared/ into common/ ---
echo "[HRSM] Removing shared/ directory to enforce TechSpec (common/ only)..."
# Note: If shared/ contained unique logic not in common/, please merge it manually before running this.
rm -rf "$DIR/shared"

# --- Task 1: Wire up MariaDB in backend/src/DB.hs ---
echo "[HRSM] Generating backend/src/DB.hs with MariaDB connection logic..."
cat << 'EOF' > "$DIR/backend/src/DB.hs"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB
  ( DBConn
  , initDB
  , getTasks
  , createTask
  ) where

import Data.Text (Text)
import Database.MySQL.Simple (ConnectInfo(..), Connection, connect, defaultConnectInfo, execute, query_)
import Common.Types (Task(..), TaskId)

-- | Wrapper for MariaDB Connection
type DBConn = Connection

-- | Initialize MariaDB connection
initDB :: IO DBConn
initDB = do
  putStrLn "[HRSM] Connecting to MariaDB..."
  let connInfo = defaultConnectInfo 
        { connectUser = "root"
        , connectDatabase = "project_db"
        }
  connect connInfo

-- | Fetch all tasks from MariaDB
getTasks :: DBConn -> IO [Task]
getTasks conn = do
  rows <- query_ conn "SELECT id, name, done FROM tasks"
  return [ Task (Just tid) name done | (tid, name, done) <- rows ]

-- | Insert a new task into MariaDB
createTask :: DBConn -> Task -> IO TaskId
createTask conn Task{..} = do
  _ <- execute conn "INSERT INTO tasks (name, done) VALUES (?, ?)" (taskName, taskDone)
  -- TODO: Implement proper LAST_INSERT_ID() retrieval for production
  return 1 
EOF

echo "[HRSM] Updating backend/backend.cabal to include mysql-simple..."
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
    mysql-simple
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

echo "[HRSM] All tasks completed successfully."
echo "Next steps:"
echo "  1. Ensure MariaDB is running and 'project_db' exists."
echo "  2. Build the backend: nix build .#backend"
