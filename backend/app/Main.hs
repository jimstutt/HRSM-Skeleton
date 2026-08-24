{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Data.Proxy (Proxy(..))
import Common.Api (API)
import Backend (server)
import qualified DB

main :: IO ()
main = do
  putStrLn "[HRSM] Starting backend on port 8080..."
  conn <- DB.initDB
  putStrLn "[HRSM] Database connection established."
  run 8080 (serve (Proxy :: Proxy API) (server conn))
