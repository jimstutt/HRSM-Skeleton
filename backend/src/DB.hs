{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB
  ( DBConn
  , initDB
  , getUsers
  , createUser
  , deleteUser
  , updateUser
  ) where

import Data.Text (Text)
import Database.MySQL.Simple (ConnectInfo(..), Connection, connect, defaultConnectInfo, execute, query_)
import Database.MySQL.Simple.Types (Only(..))
import Common.Types (User(..), UserId)

unUserId :: UserId -> Int
unUserId (UserId i) = i

type DBConn = Connection

initDB :: IO DBConn
initDB = do
  putStrLn "[HRSM] Connecting to MariaDB..."
  let connInfo = defaultConnectInfo 
        { connectUser = "hrsm_user"
        , connectPassword = "hrsm_password"
        , connectDatabase = "project_db"
        }
  connect connInfo

getUsers :: DBConn -> IO [User]
getUsers conn = do
  rows <- query_ conn "SELECT id, name, email FROM users"
  return [ User (UserId uid) name email | (uid, name, email) <- rows ]

createUser :: DBConn -> User -> IO UserId
createUser conn User{..} = do
  _ <- execute conn "INSERT INTO users (name, email) VALUES (?, ?)" (userName, userEmail)
  return 1 

deleteUser :: DBConn -> UserId -> IO ()
deleteUser conn uid = do
  _ <- execute conn "DELETE FROM users WHERE id = ?" (Only (unUserId uid))
  return ()

updateUser :: DBConn -> UserId -> User -> IO ()
updateUser conn uid User{..} = do
  _ <- execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (userName, userEmail, unUserId uid)
  return ()
