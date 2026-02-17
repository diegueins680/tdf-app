{-# LANGUAGE OverloadedStrings #-}
module TDF.Services.InstagramMessaging
  ( sendInstagramText
  ) where

import           Control.Exception (SomeException, try)
import           Data.Aeson (encode, object, (.=))
import           Data.List (nub)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Client (Manager, Request(..), RequestBody(..), Response, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Header (hAuthorization)
import           Network.HTTP.Types.Status (statusCode)

import           TDF.Config (AppConfig(..))

sendInstagramText :: AppConfig -> Text -> Text -> IO (Either Text Text)
sendInstagramText cfg recipientId body =
  case instagramMessagingToken cfg >>= nonEmptyText of
    Nothing -> pure (Left "INSTAGRAM_MESSAGING_TOKEN no configurado")
    Just token -> do
      manager <- newManager tlsManagerSettings
      let base = T.dropWhileEnd (== '/') (instagramMessagingApiBase cfg)
          targetUrls = nub (catMaybes
            [ (\accountId -> base <> "/" <> accountId <> "/messages")
                <$> (instagramMessagingAccountId cfg >>= nonEmptyText)
            , Just (base <> "/me/messages")
            ])
      attempts <- mapM (sendToEndpoint manager token recipientId body) targetUrls
      pure (pickFirstSuccess targetUrls attempts)

nonEmptyText :: Text -> Maybe Text
nonEmptyText raw =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

pickFirstSuccess :: [Text] -> [Either Text Text] -> Either Text Text
pickFirstSuccess [] _ = Left "No Instagram messaging endpoint configured"
pickFirstSuccess _ [] = Left "No Instagram messaging attempt executed"
pickFirstSuccess urls results =
  case [payload | Right payload <- results] of
    payload : _ -> Right payload
    [] ->
      let errors = ["Send failed via " <> url <> ": " <> err | (url, Left err) <- zip urls results]
      in if null errors
        then Left "Instagram messaging failed without details"
        else Left (T.intercalate " | " errors)

sendToEndpoint
  :: Manager
  -> Text
  -> Text
  -> Text
  -> Text
  -> IO (Either Text Text)
sendToEndpoint manager token recipientId body urlTxt = do
  reqE <- try (parseRequest (T.unpack urlTxt)) :: IO (Either SomeException Request)
  case reqE of
    Left err -> pure (Left (T.pack (show err)))
    Right req0 -> do
      let req = req0
            { method = "POST"
            , requestHeaders =
                [ ("Content-Type", "application/json")
                , (hAuthorization, BS.pack ("Bearer " <> T.unpack token))
                ]
            , requestBody = RequestBodyLBS (encode payload)
            }
          payload = object
            [ "recipient" .= object [ "id" .= recipientId ]
            , "message" .= object [ "text" .= body ]
            , "messaging_type" .= ("RESPONSE" :: Text)
            ]
      respE <- try (httpLbs req manager) :: IO (Either SomeException (Response BL.ByteString))
      pure $ case respE of
        Left err -> Left (T.pack (show err))
        Right resp ->
          let status = statusCode (responseStatus resp)
              bodyTxt = TE.decodeUtf8 (BL.toStrict (responseBody resp))
          in if status >= 200 && status < 300
               then Right bodyTxt
               else Left ("HTTP " <> T.pack (show status) <> ": " <> bodyTxt)
