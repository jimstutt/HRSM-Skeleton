{-# LANGUAGE OverloadedStrings #-}
module Middleware.RateLimit (rateLimitMiddleware) where

import Network.Wai
import Network.HTTP.Types (status429)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.ByteString.Lazy.Char8 (pack)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T

-- Very small in-memory token bucket. For production prefer an external rate limiter (redis, etc.).

type Ip = Text

data Bucket = Bucket
  { tokens :: TVar Double
  , lastRefill :: TVar Double
  }

type Buckets = TVar (M.Map Ip Bucket)

newBuckets :: IO Buckets
newBuckets = newTVarIO M.empty

-- refill rate: tokens per second
refillRate :: Double
refillRate = 1.0 -- 1 request per second

capacity :: Double
capacity = 5.0 -- burst capacity

getIp :: Request -> Ip
getIp req = case lookup "X-Forwarded-For" (requestHeaders req) of
  Just bs -> T.pack (show bs)
  Nothing -> T.pack (show $ remoteHost req)

allowRequest :: Buckets -> Ip -> IO Bool
allowRequest buckets ip = atomically $ do
  m <- readTVar buckets
  case M.lookup ip m of
    Just bucket -> do
      now <- unsafeIOToSTM (fmap realToFrac getPOSIXTime)
      tks <- readTVar (tokens bucket)
      lr  <- readTVar (lastRefill bucket)
      let elapsed = now - lr
          added = elapsed * refillRate
          newTokens = min capacity (tks + added)
      if newTokens >= 1
        then do
          writeTVar (tokens bucket) (newTokens - 1)
          writeTVar (lastRefill bucket) now
          return True
        else return False
    Nothing -> do
      t1 <- newTVar capacity
      t2 <- newTVar =<< unsafeIOToSTM (fmap realToFrac getPOSIXTime)
      let bucket = Bucket t1 t2
      writeTVar buckets (M.insert ip bucket m)
      return True

rateLimitMiddleware :: Buckets -> Middleware
rateLimitMiddleware buckets app req respond = do
  let ip = getIp req
  ok <- allowRequest buckets ip
  if ok
    then app req respond
    else respond $ responseLBS status429 [("Content-Type","text/plain")] "Too many requests"
    
