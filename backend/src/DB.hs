{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB
  ( DBConn
  , initDB
  , getTasks
  , createTask
  ) where

import Data.Text (Text)
import Database.MySQL.Simple (ConnectInfo(..), Connection, connect, defaultConnectInfo, execute, query_)
import Common.Types (Task(..), TaskId)

-- | Wrapper for MariaDB Connection
type DBConn = Connection

-- | Initialize MariaDB connection
initDB :: IO DBConn
initDB = do
  putStrLn "[HRSM] Connecting to MariaDB..."
  let connInfo = defaultConnectInfo 
        { connectUser = "root"
        , connectDatabase = "project_db"
        }
  connect connInfo

-- | Fetch all tasks from MariaDB
getTasks :: DBConn -> IO [Task]
getTasks conn = do
  rows <- query_ conn "SELECT id, name, done FROM tasks"
  return [ Task (Just tid) name done | (tid, name, done) <- rows ]

-- | Insert a new task into MariaDB
createTask :: DBConn -> Task -> IO TaskId
createTask conn Task{..} = do
  _ <- execute conn "INSERT INTO tasks (name, done) VALUES (?, ?)" (taskName, taskDone)
  -- TODO: Implement proper LAST_INSERT_ID() retrieval for production
  return 1 
