{-# LANGUAGE DeriveGeneric #-}
module Common.Types
  ( User(..)
  , UserId
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type UserId = Int

data User = User
  { userId    :: Maybe UserId
  , userName  :: Text
  , userEmail :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
