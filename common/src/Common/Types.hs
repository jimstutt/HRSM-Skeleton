module Common.Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | User data structure shared between frontend, backend, and database.
data User = User
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
