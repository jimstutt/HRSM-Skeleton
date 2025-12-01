{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Api (Api, api)
import Common.Types (User(..))
import DB (Pool, getAllUsers)
import Data.Text (Text)
import Servant.API
import Servant.Server

-- | The type representing the combined environment and application state.
newtype AppState = AppState { appDbPool :: Pool Connection }

type AppM = ReaderT AppState Handler

-- | Handler implementation for the API endpoints.
server :: Pool Connection -> Server Api
server dbPool =
      handleGetAllUsers dbPool
 :<|> handleGetUser dbPool
 :<|> handleRegisterUser

-- | GET /users
handleGetAllUsers :: Pool Connection -> Handler [User]
handleGetAllUsers pool = liftIO $ getAllUsers pool
  -- In a real app, the DB query will be here, using the Pool.
  -- Simulating a result for this example:
  -- liftIO $ pure [User 1 "Alice" "alice@example.com", User 2 "Bob" "bob@example.com"]

-- | GET /user/:id
handleGetUser :: Pool Connection -> Int -> Handler User
handleGetUser _ userId = throwError $ err404 { errBody = "User not found." }

-- | POST /register
handleRegisterUser :: User -> Handler ()
handleRegisterUser newUser = liftIO $ print newUser -- In a real app, this would insert the user into MariaDB
