{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (API)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Servant.OpenApi (toOpenApi)
import Data.Proxy (Proxy(..))
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  args <- getArgs
  let outputPath = case args of
        ("--output":path:_) -> path
        (x:_) | "--output=" `isPrefixOf` x -> drop 9 x
        _ -> "frontend/openapi.json"
  
  -- Ensure parent directory exists before writing
  createDirectoryIfMissing True (takeDirectory outputPath)
  
  let spec = toOpenApi (Proxy :: Proxy API)
  BL.writeFile outputPath (encode spec)
  putStrLn $ "[HRSM] Generated " ++ outputPath
