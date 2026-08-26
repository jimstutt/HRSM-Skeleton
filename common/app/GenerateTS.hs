module Main where

import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  args <- getArgs
  let outputPath = case args of
        ("--output":path:_) -> path
        _ -> "frontend/src/api-types.ts"
  
  -- Ensure parent directory exists
  createDirectoryIfMissing True (takeDirectory outputPath)
  
  -- Direct TS generation (no external packages needed)
  let tsContent = unlines
        [ "// Auto-generated from Common.Types - DO NOT EDIT"
        , ""
        , "export type UserId = number;"
        , ""
        , "export interface User {"
        , "  userId: UserId;"
        , "  userName: string;"
        , "  userEmail: string;"
        , "}"
        ]
  
  writeFile outputPath tsContent
  putStrLn $ "[HRSM] Generated " ++ outputPath
