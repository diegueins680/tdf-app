{-# LANGUAGE OverloadedStrings #-}
module TDF.WhatsApp.Client (sendText) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAuthorization)
import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy as LBS

sendText :: Manager -> Text -> Text -> Text -> Text -> IO (Either String Value)
sendText mgr token phoneId to body = do
  initReq <- parseRequest $ "https://graph.facebook.com/v20.0/" <> T.unpack phoneId <> "/messages"
  let payload = object
        [ "messaging_product" .= ("whatsapp" :: Text)
        , "to"   .= to
        , "type" .= ("text" :: Text)
        , "text" .= object [ "body" .= body ]
        ]
      req = initReq
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , (hAuthorization, BS.pack $ "Bearer " <> T.unpack token)
              ]
          , requestBody = RequestBodyLBS (encode payload)
          }
  res <- try $ httpLbs req mgr :: IO (Either SomeException (Response LBS.ByteString))
  pure $ case res of
    Left e   -> Left (show e)
    Right ok -> eitherDecode' (responseBody ok)
