{-# LANGUAGE OverloadedStrings #-}
module TDF.Services.FacebookMessaging
  ( sendFacebookText
  , formatFacebookGraphHttpError
  ) where

import           Control.Exception (SomeException, try)
import           Data.Aeson (encode, object, (.=))
import           Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isAlphaNum
  , isControl
  , isSpace
  )
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Client (Request(..), RequestBody(..), Response, httpLbs, parseRequest, responseBody, responseStatus)
import           TDF.DB (sharedTlsManager)
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
          manager <- pure sharedTlsManager
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
                      bodyTxt = decodeFacebookGraphBody (responseBody resp)
                  in if status >= 200 && status < 300
                       then Right bodyTxt
                       else Left (formatFacebookGraphHttpError status (responseBody resp))

formatFacebookGraphHttpError :: Int -> BL.ByteString -> Text
formatFacebookGraphHttpError status bodyBytes =
  "HTTP " <> T.pack (show status) <> suffix
  where
    bodyTxt = sanitizeFacebookGraphBody bodyBytes
    suffix =
      if T.null bodyTxt
        then ""
        else ": " <> bodyTxt

sanitizeFacebookGraphBody :: BL.ByteString -> Text
sanitizeFacebookGraphBody bodyBytes =
  truncateGraphErrorBody
    . redactFacebookGraphSecrets
    . T.strip
    . T.map sanitizeGraphErrorChar $
      decodeFacebookGraphBody bodyBytes

decodeFacebookGraphBody :: BL.ByteString -> Text
decodeFacebookGraphBody =
  TE.decodeUtf8With TEE.lenientDecode . BL.toStrict

truncateGraphErrorBody :: Text -> Text
truncateGraphErrorBody bodyTxt
  | T.length bodyTxt <= maxFacebookGraphErrorBodyChars = bodyTxt
  | otherwise = T.take maxFacebookGraphErrorBodyChars bodyTxt <> "..."

redactFacebookGraphSecrets :: Text -> Text
redactFacebookGraphSecrets = go Nothing
  where
    go _ textValue
      | T.null textValue = ""
    go previous textValue =
      case matchSensitiveFacebookGraphField previous textValue of
        Just (prefix, rest) ->
          prefix <> "[redacted]" <> go Nothing rest
        Nothing ->
          let ch = T.head textValue
          in T.singleton ch <> go (Just ch) (T.tail textValue)

matchSensitiveFacebookGraphField :: Maybe Char -> Text -> Maybe (Text, Text)
matchSensitiveFacebookGraphField previous textValue
  | not (isFacebookSecretFieldBoundary previous) = Nothing
  | otherwise = firstMatch sensitiveFacebookGraphFields
  where
    lowered = T.toLower textValue

    firstMatch [] = Nothing
    firstMatch (fieldName:rest) =
      case parseSensitiveFacebookGraphField fieldName lowered textValue of
        Just match -> Just match
        Nothing -> firstMatch rest

isFacebookSecretFieldBoundary :: Maybe Char -> Bool
isFacebookSecretFieldBoundary Nothing = True
isFacebookSecretFieldBoundary (Just ch) =
  not (isAlphaNum ch || ch == '_' || ch == '-')

sensitiveFacebookGraphFields :: [Text]
sensitiveFacebookGraphFields =
  [ "access_token"
  , "fb_exchange_token"
  , "client_secret"
  , "appsecret_proof"
  , "code"
  ]

parseSensitiveFacebookGraphField :: Text -> Text -> Text -> Maybe (Text, Text)
parseSensitiveFacebookGraphField fieldName lowered textValue
  | not (fieldName `T.isPrefixOf` lowered) = Nothing
  | otherwise = do
      let fieldLength = T.length fieldName
          fieldText = T.take fieldLength textValue
          afterField = T.drop fieldLength textValue
          (closingQuote, afterClosingQuote) = consumeOptionalQuote afterField
          (beforeSeparator, separatorCandidate) = T.span isSpace afterClosingQuote
      (separator, afterSeparator) <- T.uncons separatorCandidate
      if separator == '=' || (separator == ':' && fieldName /= "code")
        then
          let (afterSeparatorSpace, valueStart) = T.span isSpace afterSeparator
              (openingQuote, valueText, isValueEnd) = consumeValueOpeningQuote valueStart
              (_, rest) = T.break isValueEnd valueText
              prefix =
                fieldText
                  <> closingQuote
                  <> beforeSeparator
                  <> T.singleton separator
                  <> afterSeparatorSpace
                  <> openingQuote
          in Just (prefix, rest)
        else Nothing

consumeOptionalQuote :: Text -> (Text, Text)
consumeOptionalQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest)
    Just ('\'', rest) -> ("'", rest)
    _ -> ("", textValue)

consumeValueOpeningQuote :: Text -> (Text, Text, Char -> Bool)
consumeValueOpeningQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest, (== '"'))
    Just ('\'', rest) -> ("'", rest, (== '\''))
    _ -> ("", textValue, isUnquotedSecretValueEnd)

isUnquotedSecretValueEnd :: Char -> Bool
isUnquotedSecretValueEnd ch =
  isSpace ch || ch `elem` ("&,}]" :: String)

sanitizeGraphErrorChar :: Char -> Char
sanitizeGraphErrorChar ch
  | isControl ch = ' '
  | generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator] = ' '
  | otherwise = ch

maxFacebookGraphErrorBodyChars :: Int
maxFacebookGraphErrorBodyChars = 1000

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
      | T.length token > maxFacebookBearerTokenChars ->
          Left "FACEBOOK_MESSAGING_TOKEN must be 4096 characters or fewer"
      | T.any invalidHeaderValueChar token ->
          Left "FACEBOOK_MESSAGING_TOKEN must not contain whitespace or control characters"
      | T.any invalidHiddenHeaderValueChar token ->
          Left "FACEBOOK_MESSAGING_TOKEN must not contain hidden formatting characters"
      | T.any invalidHeaderTextChar token ->
          Left "FACEBOOK_MESSAGING_TOKEN must contain visible ASCII characters only"
      | otherwise ->
          Right token

maxFacebookBearerTokenChars :: Int
maxFacebookBearerTokenChars = 4096

validateFacebookPageId :: Maybe Text -> Either Text Text
validateFacebookPageId rawPageId =
  case rawPageId >>= nonEmptyText of
    Nothing ->
      Left "FACEBOOK_MESSAGING_PAGE_ID no configurado"
    Just pageId
      | T.length pageId > maxFacebookPageIdChars
          || not (T.any isGraphNodeIdAtom pageId)
          || T.any (not . isGraphNodeIdChar) pageId ->
          Left
            ( "FACEBOOK_MESSAGING_PAGE_ID must be a Graph node id using only "
                <> "ASCII letters, numbers, '.', '_' or '-' with at least one "
                <> "letter or number (128 chars max)"
            )
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
      | not (T.any isGraphNodeIdAtom recipientId)
          || T.any (not . isGraphNodeIdChar) recipientId ->
          Left
            ( "Facebook recipient id must be a Graph node id using only "
                <> "ASCII letters, numbers, '.', '_' or '-' with at least one "
                <> "letter or number (256 chars max)"
            )
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
      | T.any invalidMessageBodyFormatChar messageBody ->
          Left "Facebook message body must not contain hidden formatting or separator characters"
      | otherwise ->
          Right messageBody

invalidHeaderValueChar :: Char -> Bool
invalidHeaderValueChar ch = isSpace ch || isControl ch

invalidHiddenHeaderValueChar :: Char -> Bool
invalidHiddenHeaderValueChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

invalidHeaderTextChar :: Char -> Bool
invalidHeaderTextChar ch = ch < '!' || ch > '~'

invalidFacebookRecipientIdChar :: Char -> Bool
invalidFacebookRecipientIdChar ch = isSpace ch || isControl ch

invalidMessageBodyControlChar :: Char -> Bool
invalidMessageBodyControlChar ch =
  isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t'

invalidMessageBodyFormatChar :: Char -> Bool
invalidMessageBodyFormatChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

isGraphNodeIdAtom :: Char -> Bool
isGraphNodeIdAtom ch =
  (ch >= 'a' && ch <= 'z')
    || (ch >= 'A' && ch <= 'Z')
    || (ch >= '0' && ch <= '9')

isGraphNodeIdChar :: Char -> Bool
isGraphNodeIdChar ch =
  isGraphNodeIdAtom ch || ch `elem` ("._-" :: String)

maxFacebookRecipientIdChars :: Int
maxFacebookRecipientIdChars = 256

maxFacebookMessageBodyChars :: Int
maxFacebookMessageBodyChars = 5000

maxFacebookPageIdChars :: Int
maxFacebookPageIdChars = 128
