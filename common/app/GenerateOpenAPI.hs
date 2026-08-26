{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (api)
import Data.OpenApi (toOpenApi)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))

main :: IO ()
main = do
  let spec = toOpenApi (Proxy :: Proxy (Common.Api.API))
  BL.writeFile "frontend/openapi.json" (encode spec)
  putStrLn "[HRSM] Generated frontend/openapi.json"
