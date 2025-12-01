{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Common.Api where

import Servant.API
import Common.Types (User)

-- | Define the full API for the backend server.
type Api = "users" :> Get '[JSON] [User] -- GET /users -> Returns a list of all Users
        :<|> "user" :> Capture "user_id" Int :> Get '[JSON] User -- GET /user/123 -> Returns a specific User
        :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] () -- POST /register -> Registers a new User

-- | A value-level proxy for the API type.
api :: Proxy Api
api = Proxy
