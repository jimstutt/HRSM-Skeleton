{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NGOWidgets
  ( mainWidget
  ) where

import Reflex.Dom
import Data.Aeson (eitherDecode, encode)
import Data.Text (Text, pack, unpack)
import Common.Types (User(..))

mainWidget :: MonadWidget t m => m ()
mainWidget = do
  el "h1" $ text "HRSM User Management"
  
  (refreshBtn, _) <- el' "button" $ text "Load Users"
  let refreshEv = domEvent Click refreshBtn
  
  fetchResp <- performEvent $ ffor refreshEv $ \_ -> liftIO fetchUsers
  
  (addBtn, _) <- el' "button" $ text "Add Test User (Bob)"
  let addEv = domEvent Click addBtn
  
  addResp <- performEvent $ ffor addEv $ \_ -> do
    liftIO $ createUserReq "Bob" "bob@example.com"
    liftIO fetchUsers
    
  usersDyn <- holdDyn [] (leftmost [fetchResp, addResp])
  
  el "h2" $ text "Users:"
  el "ul" $ simpleList usersDyn $ \userDyn -> do
    el "li" $ do
      dynText $ fmap (\u -> "Name: " <> userName u <> ", Email: " <> userEmail u) userDyn

  return ()

fetchUsers :: IO [User]
fetchUsers = do
  resp <- xhrRequest "GET" "http://localhost:8080/api/users" def
  case xhrResponseData resp of
    Right body -> case eitherDecode (pack body) of
      Right users -> return users
      Left err -> do
        putStrLn $ "Decode error: " ++ err
        return []
    Left err -> do
        putStrLn $ "XHR error: " ++ show err
        return []

createUserReq :: Text -> Text -> IO ()
createUserReq name email = do
  let user = User Nothing name email
  let body = unpack $ encode user
  resp <- xhrRequest "POST" "http://localhost:8080/api/users" $ 
    def { xhrRequestContentType = Just "application/json"
        , xhrRequestData = XhrRequestData_Text body
        }
  putStrLn $ "Add user response: " ++ show (xhrResponseData resp)
