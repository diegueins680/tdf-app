{-# LANGUAGE OverloadedStrings #-}
module TDF.WhatsApp.Client
  ( SendTextResult(..)
  , extractMessageId
  , normalizeGraphApiVersion
  , normalizeWhatsAppAccessToken
  , normalizeWhatsAppMessageBody
  , normalizeWhatsAppPhoneNumberId
  , normalizeWhatsAppRecipientPhone
  , sendText
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Char (isControl, isSpace)
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
  case normalizeGraphApiVersion apiVersion of
    Left err -> pure (Left err)
    Right version -> do
      case normalizeWhatsAppAccessToken token of
        Left err -> pure (Left err)
        Right accessToken ->
          case normalizeWhatsAppPhoneNumberId phoneId of
            Left err -> pure (Left err)
            Right normalizedPhoneId -> do
              case normalizeWhatsAppRecipientPhone to of
                Left err -> pure (Left err)
                Right recipientPhone ->
                  case normalizeWhatsAppMessageBody body of
                    Left err -> pure (Left err)
                    Right messageBody -> do
                      initReq <-
                        parseRequest $
                          "https://graph.facebook.com/"
                            <> T.unpack version
                            <> "/"
                            <> T.unpack normalizedPhoneId
                            <> "/messages"
                      let payload = object
                            [ "messaging_product" .= ("whatsapp" :: Text)
                            , "to"   .= recipientPhone
                            , "type" .= ("text" :: Text)
                            , "text" .= object [ "body" .= messageBody ]
                            ]
                          req = initReq
                              { method = "POST"
                              , requestHeaders =
                                  [ ("Content-Type", "application/json")
                                  , (hAuthorization, BS.pack $ "Bearer " <> T.unpack accessToken)
                                  ]
                              , requestBody = RequestBodyLBS (encode payload)
                              }
                      res <-
                        (try (httpLbs req mgr) ::
                          IO (Either SomeException (Response LBS.ByteString)))
                      pure $ case res of
                        Left e   -> Left (show e)
                        Right ok ->
                          let status = statusCode (responseStatus ok)
                              rawBody = responseBody ok
                          in case eitherDecode' rawBody of
                               Left err ->
                                 let rendered =
                                       TE.decodeUtf8With lenientDecode (LBS.toStrict rawBody)
                                 in Left
                                      ( "Failed to decode WhatsApp API response ("
                                          <> show status
                                          <> "): "
                                          <> err
                                          <> " | "
                                          <> T.unpack rendered
                                      )
                               Right parsed ->
                                 if status >= 200 && status < 300
                                   then Right SendTextResult
                                     { sendTextPayload = parsed
                                     , sendTextMessageId = extractMessageId parsed
                                     }
                                   else
                                     Left
                                       ( "HTTP "
                                           <> show status
                                           <> ": "
                                           <> T.unpack (renderValue parsed)
                                       )

normalizeGraphApiVersion :: Text -> Either String Text
normalizeGraphApiVersion rawVersion
  | T.null version = Right "v20.0"
  | isValidVersion version = Right version
  | otherwise =
      Left "Invalid WhatsApp Graph API version: expected vMAJOR or vMAJOR.MINOR"
  where
    version = T.toLower (T.strip rawVersion)

    isValidVersion value =
      case T.uncons value of
        Just ('v', rest) ->
          case T.splitOn "." rest of
            [major] -> isPositiveVersionSegment major
            [major, minor] ->
              isPositiveVersionSegment major && isCanonicalVersionSegment minor
            _ -> False
        _ -> False

    isPositiveVersionSegment value =
      isCanonicalVersionSegment value && value /= "0"

    isCanonicalVersionSegment value =
      not (T.null value)
        && T.all (\ch -> ch >= '0' && ch <= '9') value
        && (value == "0" || not ("0" `T.isPrefixOf` value))

normalizeWhatsAppAccessToken :: Text -> Either String Text
normalizeWhatsAppAccessToken rawToken
  | T.null token =
      Left "Invalid WhatsApp access token: token is required"
  | T.any isUnsafeHeaderChar token =
      Left "Invalid WhatsApp access token: must not contain whitespace or control characters"
  | otherwise =
      Right token
  where
    token = T.strip rawToken

normalizeWhatsAppPhoneNumberId :: Text -> Either String Text
normalizeWhatsAppPhoneNumberId rawPhoneId
  | T.null phoneId =
      Left "Invalid WhatsApp phone number id: id is required"
  | T.all isAsciiDigit phoneId =
      Right phoneId
  | otherwise =
      Left "Invalid WhatsApp phone number id: expected digits only"
  where
    phoneId = T.strip rawPhoneId

normalizeWhatsAppRecipientPhone :: Text -> Either String Text
normalizeWhatsAppRecipientPhone rawPhone =
  let trimmed = T.strip rawPhone
      onlyDigits = T.filter isAsciiDigit trimmed
      digitCount = T.length onlyDigits
      plusCount = T.count "+" trimmed
      plusIndex = T.findIndex (== '+') trimmed
      firstDigitIndex = T.findIndex isAsciiDigit trimmed
      allowedPhoneChar ch =
        isAsciiDigit ch || isSpace ch || ch `elem` ("+-()." :: String)
      hasInvalidChars = T.any (not . allowedPhoneChar) trimmed
      plusIsValid =
        case plusIndex of
          Nothing -> True
          Just idx ->
            case firstDigitIndex of
              Nothing -> False
              Just digitIdx -> plusCount == 1 && idx < digitIdx
  in if T.null trimmed || T.null onlyDigits
       then Left "Invalid WhatsApp recipient phone: phone is required"
       else if digitCount < 8
            || digitCount > 15
            || hasInvalidChars
            || not plusIsValid
         then
           Left
             invalidWhatsAppRecipientPhoneShapeMessage
         else Right ("+" <> onlyDigits)

invalidWhatsAppRecipientPhoneShapeMessage :: String
invalidWhatsAppRecipientPhoneShapeMessage =
  "Invalid WhatsApp recipient phone: expected 8-15 digits with optional "
    <> "leading + and phone separators"

normalizeWhatsAppMessageBody :: Text -> Either String Text
normalizeWhatsAppMessageBody rawBody
  | T.null body =
      Left "Invalid WhatsApp message body: message is required"
  | T.length body > maxWhatsAppMessageBodyChars =
      Left "Invalid WhatsApp message body: message must be 4096 characters or fewer"
  | T.any invalidMessageBodyControlChar body =
      Left invalidWhatsAppMessageBodyControlMessage
  | otherwise =
      Right body
  where
    body = T.strip rawBody

maxWhatsAppMessageBodyChars :: Int
maxWhatsAppMessageBodyChars = 4096

invalidMessageBodyControlChar :: Char -> Bool
invalidMessageBodyControlChar ch =
  isControl ch && ch `notElem` ("\n\r\t" :: String)

invalidWhatsAppMessageBodyControlMessage :: String
invalidWhatsAppMessageBodyControlMessage =
  "Invalid WhatsApp message body: message must not contain "
    <> "unsupported control characters"

isAsciiDigit :: Char -> Bool
isAsciiDigit ch = ch >= '0' && ch <= '9'

isUnsafeHeaderChar :: Char -> Bool
isUnsafeHeaderChar ch =
  isControl ch || isSpace ch

extractMessageId :: Value -> Maybe Text
extractMessageId =
  parseMaybe $
    withObject "SendTextResult" $ \o -> do
      msgs <- o .:? "messages" .!= ([] :: [Value])
      case mapMaybe pullId msgs of
        [msgId] -> pure msgId
        [] -> fail "Missing WhatsApp message id"
        _ -> fail "Multiple WhatsApp message ids"
  where
    pullId =
      parseMaybe $
        withObject "WhatsAppMessageId" $ \msg -> do
          rawId <- msg .: "id"
          maybe (fail "Invalid WhatsApp message id") pure (normalizeMessageId rawId)

normalizeMessageId :: Text -> Maybe Text
normalizeMessageId rawId =
  let trimmed = T.strip rawId
  in if T.null trimmed || T.any isUnsafeHeaderChar trimmed
       then Nothing
       else Just trimmed

renderValue :: Value -> Text
renderValue = TE.decodeUtf8 . LBS.toStrict . encode
