{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Api (API)
import Common.Types (User, UserId(..))
import DB (getUsers, createUser)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (simpleCors, cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Servant
import Servant.Server (Server)

-- CORS middleware to allow requests from Vite dev server
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just corsPolicy)
  where
    corsPolicy = simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:5173", "http://localhost:3000"], True)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization"]
      }

server :: Server API
server = getUsersHandler :<|> createUserHandler
  where
    getUsersHandler :: Handler [User]
    getUsersHandler = liftIO getUsers
    
    createUserHandler :: User -> Handler UserId
    createUserHandler user = liftIO (createUser user)

app :: Application
app = corsMiddleware $ serve (Proxy :: Proxy API) server
