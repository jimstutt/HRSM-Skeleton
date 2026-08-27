{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB where

import Common.Types (User(..), UserId(..))
import Database.MySQL.Simple
import Data.Int (Int64)

type DBConn = Connection

unUserId :: UserId -> Int
unUserId (UserId i) = i

getUsers :: DBConn -> IO [User]
getUsers conn = do
  rows <- query_ conn "SELECT id, name, email FROM users"
  return [ User (UserId uid) name email | (uid, name, email) <- rows ]

createUser :: DBConn -> User -> IO UserId
createUser conn User{..} = do
  _ <- execute conn "INSERT INTO users (name, email) VALUES (?, ?)" (userName, userEmail)
  -- Fix: Move type signature to the expression level to avoid ScopedTypeVariables requirement
  [Only newId] <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int64]
  return (UserId (fromIntegral newId))

deleteUser :: DBConn -> UserId -> IO ()
deleteUser conn uid = do
  _ <- execute conn "DELETE FROM users WHERE id = ?" (Only (unUserId uid))
  return ()

updateUser :: DBConn -> UserId -> User -> IO ()
updateUser conn uid User{..} = do
  _ <- execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" 
                   (userName, userEmail, unUserId uid)
  return ()
