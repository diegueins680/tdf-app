{-# LANGUAGE OverloadedStrings #-}
module TDF.Services.InstagramMessaging
  ( sendInstagramText
  ) where

import           Control.Exception (SomeException, try)
import           Data.Aeson (encode, object, (.=))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Client (Request(..), RequestBody(..), Response, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Header (hAuthorization)
import           Network.HTTP.Types.Status (statusCode)

import           TDF.Config (AppConfig(..))

sendInstagramText :: AppConfig -> Text -> Text -> IO (Either Text Text)
sendInstagramText cfg recipientId body =
  case (instagramMessagingToken cfg, instagramMessagingAccountId cfg) of
    (Nothing, _) -> pure (Left "INSTAGRAM_MESSAGING_TOKEN no configurado")
    (_, Nothing) -> pure (Left "INSTAGRAM_MESSAGING_ACCOUNT_ID no configurado")
    (Just token, Just accountId) -> do
      manager <- newManager tlsManagerSettings
      let base = T.dropWhileEnd (== '/') (instagramMessagingApiBase cfg)
          urlTxt = base <> "/" <> accountId <> "/messages"
      reqE <- try (parseRequest (T.unpack urlTxt)) :: IO (Either SomeException Request)
      case reqE of
        Left err -> pure (Left (T.pack (show err)))
        Right req0 -> do
          let payload = object
                [ "recipient" .= object [ "id" .= recipientId ]
                , "message" .= object [ "text" .= body ]
                , "messaging_type" .= ("RESPONSE" :: Text)
                ]
              req = req0
                { method = "POST"
                , requestHeaders =
                    [ ("Content-Type", "application/json")
                    , (hAuthorization, BS.pack ("Bearer " <> T.unpack token))
                    ]
                , requestBody = RequestBodyLBS (encode payload)
                }
          respE <- try (httpLbs req manager) :: IO (Either SomeException (Response BL.ByteString))
          pure $ case respE of
            Left err -> Left (T.pack (show err))
            Right resp ->
              let status = statusCode (responseStatus resp)
                  bodyTxt = TE.decodeUtf8 (BL.toStrict (responseBody resp))
              in if status >= 200 && status < 300
                   then Right bodyTxt
                   else Left ("HTTP " <> T.pack (show status) <> ": " <> bodyTxt)
