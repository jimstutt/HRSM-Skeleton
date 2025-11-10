{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import API
import Types
import DB
import Validate
import Auth
import Middleware.RateLimit

import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Servant
import Servant.Auth.Server
import System.Environment (lookupEnv)
import Data.Pool (Pool)
import Database.MySQL.Simple (defaultConnectInfo, ConnectInfo(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)

main :: IO ()
main = do
  putStrLn "Starting logistics server..."

  -- Read config from env
  host <- fmap (fromMaybe "127.0.0.1") (lookupEnv "LISTEN_HOST")
  port <- fmap (read . fromMaybe "8443") (lookupEnv "LISTEN_PORT") :: IO Int
  certPath <- fmap (fromMaybe "certs/server.crt") (lookupEnv "TLS_CERT")
  keyPath <- fmap (fromMaybe "certs/server.key") (lookupEnv "TLS_KEY")
  allowedOrigin <- fmap (fromMaybe "http://localhost:3000") (lookupEnv "CORS_ORIGIN")

  -- MySQL conn info from env
  dbHost <- fmap (fromMaybe "127.0.0.1") (lookupEnv "DB_HOST")
  dbUser <- fmap (fromMaybe "user") (lookupEnv "DB_USER")
  dbPass <- fmap (fromMaybe "password") (lookupEnv "DB_PASS")
  dbName <- fmap (fromMaybe "logistics") (lookupEnv "DB_NAME")
  dbPort <- fmap (read . fromMaybe "3306") (lookupEnv "DB_PORT") :: IO Int

  let connectInfo = defaultConnectInfo { connectHost = dbHost
                                       , connectUser = dbUser
                                       , connectPassword = dbPass
                                       , connectDatabase = dbName
                                       , connectPort = dbPort
                                       }

  pool <- createPoolFromConnectInfo connectInfo

  -- JWT settings
  jwtSettings <- jwtCfgFromEnv
  let cookieSettings = defaultCookieSettings

  buckets <- newBuckets

  let ctx = jwtSettings :. cookieSettings :. EmptyContext
      serverApp = serveWithContext api ctx (hoistServerWithContext api (Proxy '[JWTSettings, CookieSettings]) (nt pool jwtSettings) (server pool jwtSettings))
      -- Tighten CORS to configured origin only
      corsPolicy = cors (const $ Just policy)
        where
          policy = simpleCorsResourcePolicy { corsOrigins = Just ([allowedOrigin], True)
                                           , corsMethods = ["GET","POST","PUT","DELETE","OPTIONS"]
                                           , corsRequestHeaders = ["Content-Type","Authorization"] }

      app = logStdoutDev $ rateLimitMiddleware buckets $ corsPolicy serverApp

  -- Use TLS with provided cert/key
  runTLS (tlsSettings certPath keyPath) (setPort port defaultSettings) app

-- natural transformation to run Handler actions in IO with access to pool & JWT
nt :: DBPool -> JWTSettings -> Handler a -> Handler a
nt _ _ = id

-- server implementation
server :: DBPool -> JWTSettings -> Server (API '[JWT])
server pool jwt = protectedHandlers pool :<|> publicHandlers pool jwt

protectedHandlers :: DBPool -> AuthResult User -> Server ProtectedAPI
protectedHandlers pool (Authenticated user) = createH :<|> updateH :<|> deleteH
  where
    createH item = case validateItem item of
      Left err -> throwError err400 { errBody = fromString err }
      Right v -> liftIO $ do
        nid <- createItem pool v
        return $ v { itemId = Just (fromIntegral nid) }

    updateH iid item = case validateItem item of
      Left err -> throwError err400 { errBody = fromString err }
      Right v -> do
        ok <- liftIO $ updateItem pool iid v
        if ok then return NoContent else throwError err404 { errBody = "Item not found" }

    deleteH iid = do
      ok <- liftIO $ deleteItem pool iid
      if ok then return NoContent else throwError err404 { errBody = "Item not found" }

protectedHandlers _ _ = \_ -> throwError err401

publicHandlers :: DBPool -> JWTSettings -> Server PublicAPI
publicHandlers pool jwt = listH :<|> getH :<|> loginH
  where
    listH = liftIO $ getAllItems pool
    getH iid = do
      m <- liftIO $ getItemById pool iid
      case m of
        Just it -> return it
        Nothing -> throwError err404 { errBody = "Item not found" }

    loginH (LoginRequest username password) = do
      -- Replace with real authentication (DB lookup + bcrypt) in production
      if username == "admin" && password == "password"
        then do
          let usr = User 1
          mtoken <- liftIO $ makeJWT jwt usr
          case mtoken of
            Right t -> return $ LoginResponse { token = t }
            Left e -> throwError err500 { errBody = "JWT generation failed" }
        else throwError err401 { errBody = "Invalid credentials" }

-- helper for jwt generation
makeJWT :: JWTSettings -> User -> IO (Either String String)
makeJWT jwt u = do
  ej <- makeJWT' u jwt
  case ej of
    Left e -> return $ Left (show e)
    Right bs -> return $ Right (BS.unpack (toStrict bs))

-- Note: `makeJWT'`/`toStrict` are placeholders; refer to servant-auth-server docs for exact functions.
