{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Common.Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.OpenApi (ToSchema, ToParamSchema)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype UserId = UserId Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, ToSchema, ToParamSchema)

data User = User
  { userId    :: UserId
  , userName  :: String
  , userEmail :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
