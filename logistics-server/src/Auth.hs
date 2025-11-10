{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Auth (makeJWTSettingsFromSecret, jwtCfgFromEnv, AuthResult(..)) where

import Servant.Auth.Server
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.Environment (lookupEnv)

-- Create JWTSettings using a secret from env
makeJWTSettingsFromSecret :: ByteString -> JWTSettings
makeJWTSettingsFromSecret secret = defaultJWTSettings (fromJust $ hmacKey secret)
  where
    -- helper: create an `JWK` from secret using `hmacKey`
    hmacKey s = Just (fromOctets s)

-- helper to read secret from env
jwtCfgFromEnv :: IO JWTSettings
jwtCfgFromEnv = do
  m <- lookupEnv "JWT_SECRET"
  let secret = maybe "dev-secret-please-change" BS.pack m
  return $ makeJWTSettingsFromSecret secret
  
