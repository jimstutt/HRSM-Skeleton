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

-- CORS middleware to allow requests from Vite dev server
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just corsPolicy)
  where
    corsPolicy = simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:5173", "http://localhost:3000"], True)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization"]
      }

-- Server now takes a DBConn and returns the handlers
server :: DBConn -> Server API
server conn = getUsersHandler :<|> createUserHandler :<|> deleteUserHandler :<|> updateUserHandler
  where
    getUsersHandler :: Handler [User]
    getUsersHandler = liftIO $ getUsers conn
    
    createUserHandler :: User -> Handler UserId
    createUserHandler user = liftIO $ createUser conn user

    deleteUserHandler :: UserId -> Handler ()
    deleteUserHandler _uid = return ()

    updateUserHandler :: UserId -> User -> Handler ()
    updateUserHandler _uid _user = return ()

-- App takes the connection and applies it to the server
app :: DBConn -> Application
app conn = corsMiddleware $ serve (Proxy :: Proxy API) (server conn)
