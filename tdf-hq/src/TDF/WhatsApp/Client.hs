{-# LANGUAGE OverloadedStrings #-}
module TDF.WhatsApp.Client
  ( SendTextResult(..)
  , extractMessageId
  , normalizeGraphApiVersion
  , normalizeWhatsAppAccessToken
  , normalizeWhatsAppMessageBody
  , normalizeWhatsAppPhoneNumberId
  , normalizeWhatsAppRecipientPhone
  , normalizeWhatsAppTemplateLanguage
  , normalizeWhatsAppTemplateName
  , normalizeWhatsAppVerifyToken
  , sendTemplate
  , sendText
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Char
  ( GeneralCategory (Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isControl
  , isSpace
  )
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
                    Right messageBody ->
                      sendMessagePayload
                        mgr
                        version
                        accessToken
                        normalizedPhoneId
                        ( object
                            [ "messaging_product" .= ("whatsapp" :: Text)
                            , "to" .= recipientPhone
                            , "type" .= ("text" :: Text)
                            , "text" .= object ["body" .= messageBody]
                            ]
                        )

sendTemplate
  :: Manager
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> [Text]
  -> IO (Either String SendTextResult)
sendTemplate mgr apiVersion token phoneId to templateName languageCode parameters =
  case normalizeGraphApiVersion apiVersion of
    Left err -> pure (Left err)
    Right version ->
      case normalizeWhatsAppAccessToken token of
        Left err -> pure (Left err)
        Right accessToken ->
          case normalizeWhatsAppPhoneNumberId phoneId of
            Left err -> pure (Left err)
            Right normalizedPhoneId ->
              case normalizeWhatsAppRecipientPhone to of
                Left err -> pure (Left err)
                Right recipientPhone ->
                  case normalizeWhatsAppTemplateName templateName of
                    Left err -> pure (Left err)
                    Right normalizedTemplateName ->
                      case normalizeWhatsAppTemplateLanguage languageCode of
                        Left err -> pure (Left err)
                        Right normalizedLanguage ->
                          case traverse normalizeWhatsAppTemplateParameter parameters of
                            Left err -> pure (Left err)
                            Right normalizedParameters ->
                              sendMessagePayload
                                mgr
                                version
                                accessToken
                                normalizedPhoneId
                                ( templatePayload
                                    recipientPhone
                                    normalizedTemplateName
                                    normalizedLanguage
                                    normalizedParameters
                                )

templatePayload :: Text -> Text -> Text -> [Text] -> Value
templatePayload recipientPhone templateName languageCode parameters =
  object
    [ "messaging_product" .= ("whatsapp" :: Text)
    , "to" .= recipientPhone
    , "type" .= ("template" :: Text)
    , "template" .=
        object
          ( [ "name" .= templateName
            , "language" .= object ["code" .= languageCode]
            ]
              <> if null parameters
                   then []
                   else
                     [ "components" .=
                         [ object
                             [ "type" .= ("body" :: Text)
                             , "parameters" .=
                                 map
                                   (\value ->
                                     object
                                       [ "type" .= ("text" :: Text)
                                       , "text" .= value
                                       ]
                                   )
                                   parameters
                             ]
                         ]
                     ]
          )
    ]

sendMessagePayload
  :: Manager
  -> Text
  -> Text
  -> Text
  -> Value
  -> IO (Either String SendTextResult)
sendMessagePayload mgr version accessToken normalizedPhoneId payload = do
  initReq <-
    parseRequest $
      "https://graph.facebook.com/"
        <> T.unpack version
        <> "/"
        <> T.unpack normalizedPhoneId
        <> "/messages"
  let req =
        initReq
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
    Left err -> Left (show err)
    Right ok ->
      let status = statusCode (responseStatus ok)
          rawBody = responseBody ok
      in case eitherDecode' rawBody of
           Left decodeError ->
             let rendered =
                   TE.decodeUtf8With lenientDecode (LBS.toStrict rawBody)
             in Left
                  ( "Failed to decode WhatsApp API response ("
                      <> show status
                      <> "): "
                      <> decodeError
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
  | T.length token > maxWhatsAppAccessTokenChars =
      Left "Invalid WhatsApp access token: token must be 4096 characters or fewer"
  | T.any isWhitespaceOrControlChar token =
      Left "Invalid WhatsApp access token: must not contain whitespace or control characters"
  | T.any isHiddenFormattingChar token =
      Left "Invalid WhatsApp access token: must not contain hidden formatting characters"
  | T.any (not . isVisibleAsciiHeaderChar) token =
      Left "Invalid WhatsApp access token: must contain visible ASCII characters only"
  | otherwise =
      Right token
  where
    token = T.strip rawToken

maxWhatsAppAccessTokenChars :: Int
maxWhatsAppAccessTokenChars = 4096

normalizeWhatsAppVerifyToken :: Text -> Either String Text
normalizeWhatsAppVerifyToken rawToken
  | T.null token =
      Left "Invalid WhatsApp verify token: token is required"
  | T.length token > maxWhatsAppVerifyTokenChars =
      Left "Invalid WhatsApp verify token: token must be 512 characters or fewer"
  | T.any isWhitespaceOrControlChar token =
      Left "Invalid WhatsApp verify token: must not contain whitespace or control characters"
  | T.any isHiddenFormattingChar token =
      Left "Invalid WhatsApp verify token: must not contain hidden formatting characters"
  | T.any (not . isVisibleAsciiHeaderChar) token =
      Left "Invalid WhatsApp verify token: must contain visible ASCII characters only"
  | otherwise =
      Right token
  where
    token = T.strip rawToken

maxWhatsAppVerifyTokenChars :: Int
maxWhatsAppVerifyTokenChars = 512

normalizeWhatsAppPhoneNumberId :: Text -> Either String Text
normalizeWhatsAppPhoneNumberId rawPhoneId
  | T.null phoneId =
      Left "Invalid WhatsApp phone number id: id is required"
  | T.length phoneId > maxWhatsAppPhoneNumberIdDigits =
      Left "Invalid WhatsApp phone number id: id must be 64 digits or fewer"
  | T.all isAsciiDigit phoneId =
      Right phoneId
  | otherwise =
      Left "Invalid WhatsApp phone number id: expected digits only"
  where
    phoneId = T.strip rawPhoneId

maxWhatsAppPhoneNumberIdDigits :: Int
maxWhatsAppPhoneNumberIdDigits = 64

normalizeWhatsAppRecipientPhone :: Text -> Either String Text
normalizeWhatsAppRecipientPhone rawPhone =
  let trimmed = T.strip rawPhone
      onlyDigits = T.filter isAsciiDigit trimmed
      digitCount = T.length onlyDigits
      plusCount = T.count "+" trimmed
      plusIndex = T.findIndex (== '+') trimmed
      firstDigitIndex = T.findIndex isAsciiDigit trimmed
      allowedPhoneChar ch =
        isAsciiDigit ch || ch == ' ' || ch `elem` ("+-()." :: String)
      hasInvalidChars = T.any (not . allowedPhoneChar) trimmed
      plusIsValid =
        case plusIndex of
          Nothing -> True
          Just idx ->
            case firstDigitIndex of
              Nothing -> False
              Just digitIdx -> plusCount == 1 && idx == 0 && digitIdx == 1
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
  | T.any invalidMessageBodyFormatChar body =
      Left invalidWhatsAppMessageBodyFormatMessage
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

invalidMessageBodyFormatChar :: Char -> Bool
invalidMessageBodyFormatChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

invalidWhatsAppMessageBodyFormatMessage :: String
invalidWhatsAppMessageBodyFormatMessage =
  "Invalid WhatsApp message body: message must not contain "
    <> "hidden formatting or separator characters"

normalizeWhatsAppTemplateName :: Text -> Either String Text
normalizeWhatsAppTemplateName rawName
  | T.null templateName =
      Left "Invalid WhatsApp template name: name is required"
  | T.length templateName > 512 =
      Left "Invalid WhatsApp template name: name must be 512 characters or fewer"
  | T.all isTemplateNameChar templateName =
      Right templateName
  | otherwise =
      Left
        "Invalid WhatsApp template name: use lowercase ASCII letters, numbers, and underscores"
  where
    templateName = T.strip rawName
    isTemplateNameChar ch =
      (ch >= 'a' && ch <= 'z') || isAsciiDigit ch || ch == '_'

normalizeWhatsAppTemplateLanguage :: Text -> Either String Text
normalizeWhatsAppTemplateLanguage rawLanguage
  | validLanguage languageCode =
      Right languageCode
  | otherwise =
      Left
        "Invalid WhatsApp template language: expected a code such as es, en, or es_MX"
  where
    languageCode = T.strip rawLanguage
    validLanguage value =
      case T.splitOn "_" value of
        [language] -> validLanguagePart language
        [language, region] ->
          validLanguagePart language
            && T.length region == 2
            && T.all (\ch -> ch >= 'A' && ch <= 'Z') region
        _ -> False
    validLanguagePart value =
      T.length value >= 2
        && T.length value <= 3
        && T.all (\ch -> ch >= 'a' && ch <= 'z') value

normalizeWhatsAppTemplateParameter :: Text -> Either String Text
normalizeWhatsAppTemplateParameter rawValue
  | T.null value =
      Left "Invalid WhatsApp template parameter: value is required"
  | T.length value > 1024 =
      Left "Invalid WhatsApp template parameter: value must be 1024 characters or fewer"
  | T.any invalidMessageBodyControlChar value =
      Left "Invalid WhatsApp template parameter: unsupported control character"
  | T.any invalidMessageBodyFormatChar value =
      Left "Invalid WhatsApp template parameter: hidden formatting character"
  | otherwise =
      Right value
  where
    value = T.strip rawValue

isAsciiDigit :: Char -> Bool
isAsciiDigit ch = ch >= '0' && ch <= '9'

isUnsafeHeaderChar :: Char -> Bool
isUnsafeHeaderChar ch =
  isWhitespaceOrControlChar ch || isHiddenFormattingChar ch

isWhitespaceOrControlChar :: Char -> Bool
isWhitespaceOrControlChar ch =
  isControl ch || isSpace ch

isHiddenFormattingChar :: Char -> Bool
isHiddenFormattingChar ch =
  generalCategory ch == Format

isVisibleAsciiHeaderChar :: Char -> Bool
isVisibleAsciiHeaderChar ch =
  ch >= '!' && ch <= '~'

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
