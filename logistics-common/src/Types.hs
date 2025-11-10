module Types
  ( Item(..)
  , User(..)
  ) where

-- Dummy data types for demo purposes
data Item = Item
  { itemName     :: String
  , itemQuantity :: Int
  , itemLocation :: String
  } deriving (Eq, Show)

data User = User
  { userName :: String
  , userPass :: String
  } deriving (Eq, Show)
