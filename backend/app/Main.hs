module Main where

import Backend (app)
import Network.Wai.Handler.Warp (run)
import qualified Database.MySQL.Simple as MySQL
import DB (DBConn)

main :: IO ()
main = do
  putStrLn "[HRSM] Backend connecting to MariaDB (project_db)..."
  conn <- MySQL.connect MySQL.defaultConnectInfo 
    { MySQL.connectDatabase = "project_db"
    , MySQL.connectUser = "root"
    }
  
  putStrLn "[HRSM] Backend starting on port 8080..."
  run 8080 (app conn)
