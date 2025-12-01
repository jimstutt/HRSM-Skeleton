module Main where

import Reflex.Dom
import Frontend (frontend)

main :: IO ()
main = mainWidget frontend
