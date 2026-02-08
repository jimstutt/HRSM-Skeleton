{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

#ifdef __GHCJS__
-- GHCJS-specific imports
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
#else
-- WASM/Native-specific: Simple HTML output
main :: IO ()
main = do
  putStrLn "<!DOCTYPE html>"
  putStrLn "<html>"
  putStrLn "<head><title>NGO Logistics CG</title></head>"
  putStrLn "<body>"
  putStrLn "  <div id='app'>"
  putStrLn "    <h1>NGO Logistics CG</h1>"
  putStrLn "    <p>Haskell Full-Stack Application</p>"
  putStrLn "    <div class='status'>"
  putStrLn "      <p>Status: Ready</p>"
  putStrLn "      <button onclick='checkHealth()'>Check Backend Health</button>"
  putStrLn "      <p id='health-result'></p>"
  putStrLn "    </div>"
  putStrLn "    <hr>"
  putStrLn "    <div class='info'>"
  putStrLn "      <h2>Stack Info</h2>"
  putStrLn "      <ul>"
  putStrLn "        <li>Frontend: Haskell → WASM</li>"
  putStrLn "        <li>Backend: Servant (Haskell)</li>"
  putStrLn "        <li>Database: SQLite</li>"
  putStrLn "      </ul>"
  putStrLn "    </div>"
  putStrLn "  </div>"
  putStrLn "  <script>"
  putStrLn "    async function checkHealth() {"
  putStrLn "      try {"
  putStrLn "        const response = await fetch('http://localhost:8080/health');"
  putStrLn "        const text = await response.text();"
  putStrLn "        document.getElementById('health-result').textContent = 'Health: ' + text;"
  putStrLn "      } catch (e) {"
  putStrLn "        document.getElementById('health-result').textContent = 'Error: ' + e.message;"
  putStrLn "      }"
  putStrLn "    }"
  putStrLn "  </script>"
  putStrLn "</body>"
  putStrLn "</html>"
#endif
