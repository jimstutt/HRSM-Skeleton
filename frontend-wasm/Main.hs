{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

foreign export ccall reactor_start :: IO ()
foreign export ccall reactor_stop  :: IO ()

reactor_start :: IO ()
reactor_start = putStrLn "Reactor started"

reactor_stop :: IO ()
reactor_stop = putStrLn "Reactor stopped"

main :: IO ()
main = pure ()

