{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module Main where

import GHC.Wasm.Prim (JSString)
import System.IO (hPutStrLn, stderr)

-- Foreign import of JS fetch (defined in client-boot.js)
foreign import javascript unsafe
  "fetch($1).then(r => r.text()).then(txt => console.log('Response:', txt)).catch(err => console.error(err));"
  js_fetch :: JSString -> IO ()

-- Dummy helper: turn String into JSString
foreign import javascript unsafe "$r = $1"
  toJSString :: String -> JSString

main :: IO ()
main = do
  hPutStrLn stderr "🚀 logistics-client starting (dummy mode, no aeson)..."
  js_fetch (toJSString "https://localhost:8443/api/items")
