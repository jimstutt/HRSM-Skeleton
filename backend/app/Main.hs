module Main where

import Backend (app)
import Network.Wai.Handler.Warp (run)
import qualified Database.MySQL.Simple as MySQL

main :: IO ()
main = do
  putStrLn "[HRSM] Backend connecting to MariaDB (project_db)..."
  conn <- MySQL.connect MySQL.defaultConnectInfo 
    { MySQL.connectDatabase = "project_db"
    , MySQL.connectUser = "hrsm_user"
    , MySQL.connectPassword = "hrsm_password"
    , MySQL.connectHost = "127.0.0.1" -- Force TCP to bypass unix_socket auth
    }
  
  putStrLn "[HRSM] Backend starting on port 8080..."
  run 8080 (app conn)
