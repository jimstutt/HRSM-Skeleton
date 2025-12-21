{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.FilePath

import Shared.API

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

dbPath :: FilePath
dbPath = "ngo-logistics.sqlite"

migrationsDir :: FilePath
migrationsDir = "backend/sql"

--------------------------------------------------------------------------------
-- Migrations
--------------------------------------------------------------------------------

runMigrations :: Connection -> IO ()
runMigrations conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS schema_migrations (filename TEXT PRIMARY KEY)"

  files <- listDirectory migrationsDir
  let sqlFiles = filter (".sql" `isExtensionOf`) files
  mapM_ (applyIfNeeded conn) (sorted sqlFiles)
  where
    sorted = Prelude.foldr insertSorted []
    insertSorted f [] = [f]
    insertSorted f (x:xs)
      | f < x     = f : x : xs
      | otherwise = x : insertSorted f xs

applyIfNeeded :: Connection -> FilePath -> IO ()
applyIfNeeded conn file = do
  rows <- query conn
    "SELECT filename FROM schema_migrations WHERE filename = ?"
    (Only file) :: IO [Only Text]

  case rows of
    [] -> do
      putStrLn $ "Applying migration: " <> file
      sql <- readFile (migrationsDir </> file)
      execute_ conn (Query (T.pack sql))
      execute conn
        "INSERT INTO schema_migrations (filename) VALUES (?)"
        (Only file)
    _ ->
      pure ()

--------------------------------------------------------------------------------
-- Servant
--------------------------------------------------------------------------------

server :: Connection -> Server API
server _conn =
  pure "ok"

app :: Connection -> Application
app conn =
  serve api (server conn)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Starting NGOLogisticsCG backend"

  bracket (open dbPath) close $ \conn -> do
    runMigrations conn
    putStrLn "Database ready"
    run 8080 (app conn)
