{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | JS/WASM exports for the browser client.
-- Compiles as a no-op under normal GHC, and enables FFI under ghcjs/wasm.
module WASMExports where

import Data.Aeson (encode, object, (.=))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE

-- Shared helper: convert lazy ByteString → String
lazyByteStringToString :: BL.ByteString -> String
lazyByteStringToString = T.unpack . TE.decodeUtf8 . BL.toStrict

--------------------------------------------------------------------------------
-- Native GHC build (Linux, macOS, etc.)
--------------------------------------------------------------------------------
#if !(defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH))

-- Stubbed exports so cabal build works under normal GHC.
wasm_login :: String -> String -> IO String
wasm_login _ _ = pure "{\"error\":\"wasm_login not available in native build\"}"

wasm_listItems :: IO String
wasm_listItems = pure "[]"

wasm_createItem :: String -> String -> String -> IO String
wasm_createItem _ _ _ = pure "{\"error\":\"wasm_createItem not available in native build\"}"

--------------------------------------------------------------------------------
-- JS/WASM build (via ghcjs or ghc-wasm-meta)
--------------------------------------------------------------------------------
#else

{-# LANGUAGE JavaScriptFFI #-}
import GHCJS.Types (JSString)

foreign import javascript interruptible
  "fetch($1, {method:'POST', headers:{'Content-Type':'application/json'}, body: $2})\
  \  .then(r => r.text()).then(txt => $c(txt)).catch(e => $c('ERR:' + e))"
  js_fetchPostJSON :: JSString -> JSString -> IO JSString

foreign import javascript interruptible
  "fetch($1)\
  \  .then(r => r.text()).then(txt => $c(txt)).catch(e => $c('ERR:' + e))"
  js_fetchGet :: JSString -> IO JSString

foreign import javascript unsafe "$r = $1;" toJSString :: String -> JSString
foreign import javascript unsafe "$r = $1;" fromJSString :: JSString -> String

serverBase :: String
serverBase = "https://localhost:8443"

foreign export javascript interruptible "wasm_login"
  wasm_login :: JSString -> JSString -> IO JSString

wasm_login :: JSString -> JSString -> IO JSString
wasm_login user pass = do
  let url  = serverBase ++ "/auth/login"
      body = encode $ object [ "username" .= T.pack (fromJSString user)
                             , "password" .= T.pack (fromJSString pass) ]
  js_fetchPostJSON (toJSString url) (toJSString (lazyByteStringToString body))

foreign export javascript interruptible "wasm_listItems"
  wasm_listItems :: IO JSString

wasm_listItems = js_fetchGet (toJSString (serverBase ++ "/items"))

foreign export javascript interruptible "wasm_createItem"
  wasm_createItem :: JSString -> JSString -> JSString -> IO JSString

wasm_createItem n q l = do
  let url  = serverBase ++ "/items"
      body = encode $ object [ "name"     .= T.pack (fromJSString n)
                             , "quantity" .= (read (fromJSString q) :: Int)
                             , "location" .= T.pack (fromJSString l) ]
  js_fetchPostJSON (toJSString url) (toJSString (lazyByteStringToString body))

#endif
