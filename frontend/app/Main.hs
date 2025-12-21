{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom.Core

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "NGOLogisticsCG"
  el "p"  $ text "Reflex frontend (WASI reactor target)"
