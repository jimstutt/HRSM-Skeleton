{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.IO (hFlush, stdout)

foreign export ccall reactor_start :: IO ()
foreign export ccall reactor_stop  :: IO ()

-- hs_init is already exported by GHC RTS, we just need to make sure it's in the export list

reactor_start :: IO ()
reactor_start = do
  putStrLn "[HRSM] Reactor started"
  hFlush stdout

reactor_stop :: IO ()
reactor_stop = do
  putStrLn "[HRSM] Reactor stopped"
  hFlush stdout

main :: IO ()
main = pure ()
