#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Aligning all modules to use 'User' instead of 'Task'..."

# 1. Fix Common.Types
cat << 'EOF' > "$DIR/common/src/Common/Types.hs"
{-# LANGUAGE DeriveGeneric #-}
module Common.Types
  ( User(..)
  , UserId
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type UserId = Int

data User = User
  { userId    :: Maybe UserId
  , userName  :: Text
  , userEmail :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
EOF

# 2. Fix Common.Api
cat << 'EOF' > "$DIR/common/src/Common/Api.hs"
module Common.Api
  ( Api
  , api
  ) where

import Data.Proxy (Proxy(..))
import Servant.API ((:>), (:<|>), Get, JSON, ReqBody, Post)
import Common.Types (User, UserId)

type Api = "api" :> "users" :> Get '[JSON] [User]
      :<|> "api" :> "users" :> ReqBody '[JSON] User :> Post '[JSON] UserId

api :: Proxy Api
api = Proxy
EOF

# 3. Fix backend/sql/schema.sql
mkdir -p "$DIR/backend/sql"
cat << 'EOF' > "$DIR/backend/sql/schema.sql"
-- Clean up old tasks table if it exists
DROP TABLE IF EXISTS tasks;

-- Create the users table
CREATE TABLE IF NOT EXISTS users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL
);
EOF

# 4. Fix backend/src/DB.hs
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
        { connectUser = "root"
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

# 5. Fix backend/src/Backend.hs
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
import Common.Api (Api)
import qualified DB

server :: DB.DBConn -> Server Api
server conn = getUsersHandler :<|> createUserHandler
  where
    getUsersHandler :: Handler [User]
    getUsersHandler = liftIO $ DB.getUsers conn

    createUserHandler :: User -> Handler UserId
    createUserHandler user = liftIO $ DB.createUser conn user
EOF

echo "[HRSM] All modules aligned to 'User' successfully."
