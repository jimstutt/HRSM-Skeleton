{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common.Api (api)
import Servant.TypeScript (generateTypeScript)
import System.IO (writeFile)

main :: IO ()
main = do
  let tsCode = generateTypeScript api
  writeFile "frontend/src/api-types.ts" tsCode
  putStrLn "[HRSM] Generated frontend/src/api-types.ts"
