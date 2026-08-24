#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Creating dedicated database user with password authentication..."

# Create user and grant permissions via Unix socket (where root auth works)
nix shell nixpkgs#mariadb -c sudo mariadb -u root -e "
CREATE USER IF NOT EXISTS 'hrsm_user'@'localhost' IDENTIFIED BY 'hrsm_password';
GRANT ALL PRIVILEGES ON project_db.* TO 'hrsm_user'@'localhost';
FLUSH PRIVILEGES;
"

echo "[HRSM] Updating backend DB.hs to use new credentials..."

cat << 'EOF' > "$DIR/backend/src/DB.hs"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB
  ( DBConn
  , initDB
  , getUsers
  , createUser
  ) where

import Data.Text (Text)
import Database.MySQL.Simple (ConnectInfo(..), Connection, connect, defaultConnectInfo, execute, query_)
import Common.Types (User(..), UserId)

type DBConn = Connection

initDB :: IO DBConn
initDB = do
  putStrLn "[HRSM] Connecting to MariaDB..."
  let connInfo = defaultConnectInfo 
        { connectUser = "hrsm_user"
        , connectPassword = "hrsm_password"
        , connectDatabase = "project_db"
        }
  connect connInfo

getUsers :: DBConn -> IO [User]
getUsers conn = do
  rows <- query_ conn "SELECT id, name, email FROM users"
  return [ User (Just uid) name email | (uid, name, email) <- rows ]

createUser :: DBConn -> User -> IO UserId
createUser conn User{..} = do
  _ <- execute conn "INSERT INTO users (name, email) VALUES (?, ?)" (userName, userEmail)
  return 1 
EOF

echo "[HRSM] Database user created and backend updated."
echo "Next step: Run 'nix build .#backend' and then './result/bin/backend-exe'"
