{-# LANGUAGE OverloadedStrings #-}
module TDF.Services.FacebookMessaging
  ( sendFacebookText
  ) where

import           Control.Exception (SomeException, try)
import           Data.Aeson (encode, object, (.=))
import           Data.Char (isControl, isSpace)
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

sendFacebookText :: AppConfig -> Text -> Text -> IO (Either Text Text)
sendFacebookText cfg recipientId body =
  case validateFacebookMessagePayload recipientId body of
    Left err -> pure (Left err)
    Right (cleanRecipientId, cleanBody) ->
      case validateFacebookMessagingContext cfg of
        Left err -> pure (Left err)
        Right (token, pageId) -> do
          manager <- newManager tlsManagerSettings
          let base = T.dropWhileEnd (== '/') (facebookMessagingApiBase cfg)
              urlTxt = base <> "/" <> pageId <> "/messages"
          reqE <- try (parseRequest (T.unpack urlTxt)) :: IO (Either SomeException Request)
          case reqE of
            Left err -> pure (Left (T.pack (show err)))
            Right req0 -> do
              let payload = object
                    [ "recipient" .= object [ "id" .= cleanRecipientId ]
                    , "message" .= object [ "text" .= cleanBody ]
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

nonEmptyText :: Text -> Maybe Text
nonEmptyText raw =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

validateFacebookMessagingContext :: AppConfig -> Either Text (Text, Text)
validateFacebookMessagingContext cfg = do
  token <- validateFacebookBearerToken (facebookMessagingToken cfg)
  pageId <- validateFacebookPageId (facebookMessagingPageId cfg)
  pure (token, pageId)

validateFacebookBearerToken :: Maybe Text -> Either Text Text
validateFacebookBearerToken rawToken =
  case rawToken >>= nonEmptyText of
    Nothing ->
      Left "FACEBOOK_MESSAGING_TOKEN no configurado"
    Just token
      | T.any invalidHeaderValueChar token ->
          Left "FACEBOOK_MESSAGING_TOKEN must not contain whitespace or control characters"
      | otherwise ->
          Right token

validateFacebookPageId :: Maybe Text -> Either Text Text
validateFacebookPageId rawPageId =
  case rawPageId >>= nonEmptyText of
    Nothing ->
      Left "FACEBOOK_MESSAGING_PAGE_ID no configurado"
    Just pageId
      | T.any invalidGraphPathSegmentChar pageId ->
          Left "FACEBOOK_MESSAGING_PAGE_ID must be a single Graph path segment"
      | otherwise ->
          Right pageId

validateFacebookMessagePayload :: Text -> Text -> Either Text (Text, Text)
validateFacebookMessagePayload rawRecipientId rawBody = do
  cleanRecipientId <- validateFacebookRecipientId rawRecipientId
  cleanBody <- validateFacebookMessageBody rawBody
  pure (cleanRecipientId, cleanBody)

validateFacebookRecipientId :: Text -> Either Text Text
validateFacebookRecipientId rawRecipientId =
  case nonEmptyText rawRecipientId of
    Nothing ->
      Left "Facebook recipient id requerido"
    Just recipientId
      | T.any invalidFacebookRecipientIdChar recipientId ->
          Left "Facebook recipient id must not contain whitespace or control characters"
      | T.length recipientId > maxFacebookRecipientIdChars ->
          Left "Facebook recipient id must be 256 characters or fewer"
      | otherwise ->
          Right recipientId

validateFacebookMessageBody :: Text -> Either Text Text
validateFacebookMessageBody rawBody =
  case nonEmptyText rawBody of
    Nothing ->
      Left "Facebook message body requerido"
    Just messageBody
      | T.length messageBody > maxFacebookMessageBodyChars ->
          Left "Facebook message body must be 5000 characters or fewer"
      | T.any invalidMessageBodyControlChar messageBody ->
          Left "Facebook message body must not contain control characters"
      | otherwise ->
          Right messageBody

invalidHeaderValueChar :: Char -> Bool
invalidHeaderValueChar ch = isSpace ch || isControl ch

invalidGraphPathSegmentChar :: Char -> Bool
invalidGraphPathSegmentChar ch =
  isSpace ch || isControl ch || ch == '/' || ch == '?' || ch == '#'

invalidFacebookRecipientIdChar :: Char -> Bool
invalidFacebookRecipientIdChar ch = isSpace ch || isControl ch

invalidMessageBodyControlChar :: Char -> Bool
invalidMessageBodyControlChar ch =
  isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t'

maxFacebookRecipientIdChars :: Int
maxFacebookRecipientIdChars = 256

maxFacebookMessageBodyChars :: Int
maxFacebookMessageBodyChars = 5000
