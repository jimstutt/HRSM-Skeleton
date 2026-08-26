{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Miso
import Miso.String (MisoString)
import System.IO (hFlush, stdout)

foreign export ccall start_reactor :: IO ()
foreign export ccall reactor_stop  :: IO ()

data Model = Model { count :: Int }
data Action = AddOne | SubtractOne | NoOp

start_reactor :: IO ()
start_reactor = do
  putStrLn "[HRSM] Miso Reactor initialized."
  hFlush stdout

reactor_stop :: IO ()
reactor_stop = do
  putStrLn "[HRSM] Miso Reactor stopped."
  hFlush stdout

updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = m { count = count m + 1 } <# pure NoOp
updateModel SubtractOne m = m { count = count m - 1 } <# pure NoOp
updateModel NoOp m = pure m

viewModel :: Model -> View Action
viewModel Model{..} =
  div_ []
    [ button_ [ onClick SubtractOne ] [ text "-" ]
    , text (ms (show count))
    , button_ [ onClick AddOne ] [ text "+" ]
    ]

main :: IO ()
main = pure ()
