{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Api (API)
import Common.Types (User, UserId)
import DB (getUsers, createUser, deleteUser, updateUser, DBConn)
import Network.Wai (Middleware, Application)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Servant
import Servant.Server (Server, serve)
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class (liftIO)

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just corsPolicy)
  where
    corsPolicy = simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:5173", "http://localhost:3000"], True)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization"]
      }

server :: DBConn -> Server API
server conn = getUsersHandler :<|> createUserHandler :<|> deleteUserHandler :<|> updateUserHandler
  where
    getUsersHandler :: Handler [User]
    getUsersHandler = liftIO $ getUsers conn
    
    createUserHandler :: User -> Handler UserId
    createUserHandler user = liftIO $ createUser conn user

    deleteUserHandler :: UserId -> Handler ()
    deleteUserHandler uid = liftIO $ deleteUser conn uid

    updateUserHandler :: UserId -> User -> Handler ()
    updateUserHandler uid user = liftIO $ updateUser conn uid user

app :: DBConn -> Application
app conn = corsMiddleware $ serve (Proxy :: Proxy API) (server conn)
