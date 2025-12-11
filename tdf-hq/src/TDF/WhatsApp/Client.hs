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

sendText :: Manager -> Text -> Text -> Text -> Text -> Text -> IO (Either String Value)
sendText mgr apiVersion token phoneId to body = do
  let version =
        let trimmed = T.strip apiVersion
        in if T.null trimmed then "v20.0" else trimmed
  initReq <- parseRequest $ "https://graph.facebook.com/" <> T.unpack version <> "/" <> T.unpack phoneId <> "/messages"
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
