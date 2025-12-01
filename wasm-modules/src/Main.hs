{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ text "Hello from Reflex-DOM (WASM)"
