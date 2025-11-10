{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Types
import Servant
import Servant.Auth.Server

-- Public endpoints: login, list items, get item
-- Protected endpoints: create/update/delete require a valid JWT

type PublicAPI =
       "items" :> Get '[JSON] [Item]
  :<|> "items" :> Capture "id" Int :> Get '[JSON] Item
  :<|> "auth" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

type ProtectedAPI =
       "items" :> ReqBody '[JSON] Item :> PostCreated '[JSON] Item
  :<|> "items" :> Capture "id" Int :> ReqBody '[JSON] Item :> Put '[JSON] NoContent
  :<|> "items" :> Capture "id" Int :> Delete '[JSON] NoContent

type API auths = (Auth auths User :> ProtectedAPI) :<|> PublicAPI

api :: Proxy (API '[JWT])
api = Proxy

-- Simple auth DTOs

data LoginRequest = LoginRequest
  { username :: String
  , password :: String
  } deriving (Show)

data LoginResponse = LoginResponse
  { token :: String
  } deriving (Show)

instance ToJSON LoginResponse where
  toJSON (LoginResponse t) = object ["token" .= t]

-- `User` would be the authenticated user type used with servant-auth-server
newtype User = User { userId :: Int }

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \o -> LoginRequest
    <$> o .: "username"
    <*> o .: "password"
    
