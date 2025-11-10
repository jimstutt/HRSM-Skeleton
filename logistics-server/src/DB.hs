{-# LANGUAGE OverloadedStrings #-}
module DB
  ( DBPool
  , withPool
  , createPoolFromConnectInfo
  , getAllItems
  , getItemById
  , createItem
  , updateItem
  , deleteItem
  ) where

import Types
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults (FromRow(..), field)
import Database.MySQL.Simple.ToRow (ToRow(..))
import Data.Pool (Pool, createPool, withResource, destroyAllResources)
import Data.Text (Text)
import Control.Exception (bracket)

-- Type alias for pool
type DBPool = Pool Connection

-- Create a pool
createPoolFromConnectInfo :: ConnectInfo -> IO DBPool
createPoolFromConnectInfo ci = createPool (connect ci) close 1 60 10
-- createPool create close stripes idleTime sizePerStripe

withPool :: DBPool -> (Connection -> IO a) -> IO a
withPool pool action = withResource pool action

-- FromRow/ToRow instances
instance FromRow Item where
  fromRow = do
    cid <- field
    nm  <- field
    qty <- field
    loc <- field
    return $ Item (Just cid) nm qty loc

instance ToRow Item where
  toRow (Item _ nm qty loc) = toRow (nm, qty, loc)

getAllItems :: DBPool -> IO [Item]
getAllItems pool = withResource pool $ \conn -> query_ conn "SELECT id, name, quantity, location FROM items ORDER BY id"

getItemById :: DBPool -> Int -> IO (Maybe Item)
getItemById pool iid = withResource pool $ \conn -> do
  r <- query conn "SELECT id, name, quantity, location FROM items WHERE id = ?" (Only iid)
  case r of
    [x] -> return (Just x)
    _   -> return Nothing

createItem :: DBPool -> Item -> IO Int
createItem pool (Item _ nm qty loc) = withResource pool $ \conn -> do
  execute conn "INSERT INTO items (name, quantity, location) VALUES (?,?,?)" (nm, qty, loc)
  -- fetch last inserted id; use SELECT LAST_INSERT_ID()
  [Only newId] <- query_ conn "SELECT LAST_INSERT_ID()"
  return newId

updateItem :: DBPool -> Int -> Item -> IO Bool
updateItem pool iid (Item _ nm qty loc) = withResource pool $ \conn -> do
  res <- execute conn "UPDATE items SET name = ?, quantity = ?, location = ? WHERE id = ?" (nm, qty, loc, iid)
  return (res > 0)

deleteItem :: DBPool -> Int -> IO Bool
deleteItem pool iid = withResource pool $ \conn -> do
  res <- execute conn "DELETE FROM items WHERE id = ?" (Only iid)
  return (res > 0)

  
