{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Common.Api
import Common.Types (User(..))
import Reflex.Dom

import qualified Data.Text as T
import Servant.API
import Servant.Common.Req (defaultClientOptions)
import Servant.Reflex (client, Client)

-- | Frontend type to hold API client functions
data AppClient t = AppClient
  { _client_getAllUsers :: Event t () -> Requester t Client (ClientM [User])
  , _client_getUserById :: Event t Int -> Requester t Client (ClientM User)
  , _client_registerUser :: Event t User -> Requester t Client (ClientM ())
  }

-- | Generate the client functions from the shared API type
appClient :: forall t m. (Reflex t, MonadWidget t m) => AppClient t
appClient = AppClient
  { _client_getAllUsers = fst (client (Proxy @("users" :> Get '[JSON] [User])) (Proxy @m))
  , _client_getUserById = fst (client (Proxy @("user" :> Capture "user_id" Int :> Get '[JSON] User)) (Proxy @m))
  , _client_registerUser = fst (client (Proxy @("register" :> ReqBody '[JSON] User :> Post '[JSON] ())) (Proxy @m))
  }

-- | The main Reflex-DOM widget
frontend :: MonadWidget t m => m ()
frontend = do
  el "h1" $ text "NGO Logistics App (Reflex Frontend)"
  el "p" $ text "Communicating with Haskell/MariaDB Backend..."

  -- 1. Setup Requester for API calls
  let baseUrl = BaseUrl Http "localhost" 8000 ""
  requester <- newRequester defaultClientOptions (constDyn baseUrl)

  -- 2. Define click event to fetch users
  (btn, _) <- el' "button" $ text "Fetch Users"
  let fetchEvent = domEvent Click btn

  -- 3. Call the backend API
  let clientFunc = _client_getAllUsers appClient fetchEvent requester
  responseEvent <- performRequest clientFunc

  -- 4. Display result or error
  usersD <- holdDyn [] (fmapMaybe (either (const Nothing) Just) (responseEvent))

  _ <- el "h3" $ text "Users from Backend:"
  _ <- simpleList usersD $ \userD -> el "li" $ do
    text $ T.pack $ show $ current userD
    return ()

  return ()
  
