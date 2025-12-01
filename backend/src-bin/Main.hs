module Main where

import Backend (server)
import Common.Api (api)
import DB (withConnPool)
import Network.Wai.Handler.Warp (run)
import Servant.Server

main :: IO ()
main = withConnPool $ \dbPool -> do
  let port = 8000
  putStrLn $ "Starting MariaDB-backed server on http://localhost:" ++ show port
  -- | Wrap the server with the Servant 'serve' function and run with Warp.
  run port (serve api (server dbPool))
  
