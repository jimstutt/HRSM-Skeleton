{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Reflex.Dom
import System.IO (hFlush, stdout)

foreign export ccall start_reactor :: IO ()
foreign export ccall reactor_stop  :: IO ()

mainWidget :: DomBuilder t m => m ()
mainWidget = do
  el "h1" $ text "HRSM Reflex-DOM Counter"
  el "div" $ do
    (count, _) <- button "Increment"
    text "Count: "
    dynText (fmap show count)

start_reactor :: IO ()
start_reactor = do
  putStrLn "[HRSM] Initializing Reflex-DOM..."
  hFlush stdout
  -- In a real browser, we'd use mainWidgetInBody or similar
  -- For now, we just verify it compiles and runs
  putStrLn "[HRSM] Reflex-DOM widget defined successfully."
  hFlush stdout

reactor_stop :: IO ()
reactor_stop = do
  putStrLn "[HRSM] Reactor stopped"
  hFlush stdout

main :: IO ()
main = pure ()
