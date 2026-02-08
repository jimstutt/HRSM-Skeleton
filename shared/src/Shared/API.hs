{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Shared.API where

import Servant

type API =
       "health" :> Get '[PlainText] String

api :: Proxy API
api = Proxy
