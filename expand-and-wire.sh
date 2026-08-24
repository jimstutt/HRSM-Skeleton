#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Expanding Backend API and wiring Reflex-DOM Frontend..."

# 1. Update Common.Api to include PUT and DELETE
cat << 'EOF' > "$DIR/common/src/Common/Api.hs"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Common.Api
  ( API
  , api
  ) where

import Data.Proxy (Proxy(..))
import Servant.API ((:>), (:<|>), Get, JSON, ReqBody, Post, Delete, Put, Capture)
import Common.Types (User, UserId)

type API = "api" :> "users" :> Get '[JSON] [User]
      :<|> "api" :> "users" :> ReqBody '[JSON] User :> Post '[JSON] UserId
      :<|> "api" :> "users" :> Capture "id" UserId :> Delete '[JSON] ()
      :<|> "api" :> "users" :> Capture "id" UserId :> ReqBody '[JSON] User :> Put '[JSON] ()

api :: Proxy API
api = Proxy
EOF

# 2. Update backend/src/DB.hs with deleteUser and updateUser
cat << 'EOF' > "$DIR/backend/src/DB.hs"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB
  ( DBConn
  , initDB
  , getUsers
  , createUser
  , deleteUser
  , updateUser
  ) where

import Data.Text (Text)
import Database.MySQL.Simple (ConnectInfo(..), Connection, connect, defaultConnectInfo, execute, query_, Only)
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

deleteUser :: DBConn -> UserId -> IO ()
deleteUser conn uid = do
  _ <- execute conn "DELETE FROM users WHERE id = ?" (Only uid)
  return ()

updateUser :: DBConn -> UserId -> User -> IO ()
updateUser conn uid User{..} = do
  _ <- execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (userName, userEmail, uid)
  return ()
EOF

# 3. Update backend/src/Backend.hs to wire the new handlers
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
server conn = getUsersHandler 
         :<|> createUserHandler 
         :<|> deleteUserHandler 
         :<|> updateUserHandler
  where
    getUsersHandler :: Handler [User]
    getUsersHandler = liftIO $ DB.getUsers conn

    createUserHandler :: User -> Handler UserId
    createUserHandler user = liftIO $ DB.createUser conn user

    deleteUserHandler :: UserId -> Handler ()
    deleteUserHandler uid = liftIO $ DB.deleteUser conn uid

    updateUserHandler :: UserId -> User -> Handler ()
    updateUserHandler uid user = liftIO $ DB.updateUser conn uid user
EOF

# 4. Update frontend-wasm/NGOWidgets.hs with HTTP fetching logic
cat << 'EOF' > "$DIR/frontend-wasm/NGOWidgets.hs"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NGOWidgets
  ( mainWidget
  ) where

import Reflex.Dom
import Data.Aeson (eitherDecode, encode)
import Data.Text (Text, pack, unpack)
import Common.Types (User(..))

mainWidget :: MonadWidget t m => m ()
mainWidget = do
  el "h1" $ text "HRSM User Management"
  
  (refreshBtn, _) <- el' "button" $ text "Load Users"
  let refreshEv = domEvent Click refreshBtn
  
  fetchResp <- performEvent $ ffor refreshEv $ \_ -> liftIO fetchUsers
  
  (addBtn, _) <- el' "button" $ text "Add Test User (Bob)"
  let addEv = domEvent Click addBtn
  
  addResp <- performEvent $ ffor addEv $ \_ -> do
    liftIO $ createUserReq "Bob" "bob@example.com"
    liftIO fetchUsers
    
  usersDyn <- holdDyn [] (leftmost [fetchResp, addResp])
  
  el "h2" $ text "Users:"
  el "ul" $ simpleList usersDyn $ \userDyn -> do
    el "li" $ do
      dynText $ fmap (\u -> "Name: " <> userName u <> ", Email: " <> userEmail u) userDyn

  return ()

fetchUsers :: IO [User]
fetchUsers = do
  resp <- xhrRequest "GET" "http://localhost:8080/api/users" def
  case xhrResponseData resp of
    Right body -> case eitherDecode (pack body) of
      Right users -> return users
      Left err -> do
        putStrLn $ "Decode error: " ++ err
        return []
    Left err -> do
        putStrLn $ "XHR error: " ++ show err
        return []

createUserReq :: Text -> Text -> IO ()
createUserReq name email = do
  let user = User Nothing name email
  let body = unpack $ encode user
  resp <- xhrRequest "POST" "http://localhost:8080/api/users" $ 
    def { xhrRequestContentType = Just "application/json"
        , xhrRequestData = XhrRequestData_Text body
        }
  putStrLn $ "Add user response: " ++ show (xhrResponseData resp)
EOF

# 5. Ensure frontend-wasm.cabal has the correct dependencies
cat << 'EOF' > "$DIR/frontend-wasm/frontend-wasm.cabal"
cabal-version:      3.0
name:               frontend-wasm
version:            0.1.0.0
build-type:         Simple

executable frontend-wasm-exe
  main-is:          Main.hs
  other-modules:
    NGOWidgets
  build-depends:
    base >=4.14 && <5,
    common,
    reflex-dom,
    aeson,
    text
  hs-source-dirs:   .
  default-language: Haskell2010
  default-extensions:
    OverloadedStrings
    RecordWildCards
EOF

echo "[HRSM] Backend API expanded and Frontend wired successfully."
echo "Next steps:"
echo "  1. Rebuild backend: nix build .#backend"
echo "  2. Rebuild frontend: nix build .#frontend-wasm"
