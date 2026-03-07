{-# LANGUAGE OverloadedStrings #-}
module TDF.WhatsApp.Client
  ( SendTextResult(..)
  , sendText
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Types.Status (statusCode)
import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy as LBS

data SendTextResult = SendTextResult
  { sendTextPayload   :: Value
  , sendTextMessageId :: Maybe Text
  } deriving (Show)

sendText :: Manager -> Text -> Text -> Text -> Text -> Text -> IO (Either String SendTextResult)
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
    Right ok ->
      let status = statusCode (responseStatus ok)
          rawBody = responseBody ok
      in case eitherDecode' rawBody of
           Left err ->
             let rendered = TE.decodeUtf8With lenientDecode (LBS.toStrict rawBody)
             in Left ("Failed to decode WhatsApp API response (" <> show status <> "): " <> err <> " | " <> T.unpack rendered)
           Right parsed ->
             if status >= 200 && status < 300
               then Right SendTextResult
                 { sendTextPayload = parsed
                 , sendTextMessageId = extractMessageId parsed
                 }
               else
                 Left ("HTTP " <> show status <> ": " <> T.unpack (renderValue parsed))

extractMessageId :: Value -> Maybe Text
extractMessageId =
  parseMaybe $
    withObject "SendTextResult" $ \o -> do
      msgs <- o .:? "messages" .!= ([] :: [Value])
      case mapMaybe pullId msgs of
        msgId : _ -> pure msgId
        [] -> fail "Missing WhatsApp message id"
  where
    pullId = parseMaybe (withObject "WhatsAppMessageId" (\msg -> msg .: "id"))

renderValue :: Value -> Text
renderValue = TE.decodeUtf8 . LBS.toStrict . encode
