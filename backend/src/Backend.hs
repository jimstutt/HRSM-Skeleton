{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Api (API)
import Common.Types (User, UserId(..))
import DB (getUsers, createUser, DBConn)
import Network.Wai (Middleware, Application)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Servant
import Servant.Server (Server, serve)
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class (liftIO)
import qualified Database.MySQL.Simple as MySQL

-- CORS middleware to allow requests from Vite dev server
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just corsPolicy)
  where
    corsPolicy = simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:5173", "http://localhost:3000"], True)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization"]
      }

-- Helper to get a DB connection
getConn :: IO DBConn
getConn = MySQL.connect MySQL.defaultConnectInfo 
  { MySQL.connectDatabase = "project_db"
  , MySQL.connectUser = "root"
  }

server :: Server API
server = getUsersHandler :<|> createUserHandler :<|> deleteUserHandler :<|> updateUserHandler
  where
    getUsersHandler :: Handler [User]
    getUsersHandler = liftIO $ do
      conn <- getConn
      res <- getUsers conn
      MySQL.close conn
      return res
    
    createUserHandler :: User -> Handler UserId
    createUserHandler user = liftIO $ do
      conn <- getConn
      res <- createUser conn user
      MySQL.close conn
      return res

    -- Dummy handlers for DELETE and PUT to satisfy the Servant API contract
    deleteUserHandler :: UserId -> Handler ()
    deleteUserHandler _uid = return ()

    updateUserHandler :: UserId -> User -> Handler ()
    updateUserHandler _uid _user = return ()

app :: Application
app = corsMiddleware $ serve (Proxy :: Proxy API) server
