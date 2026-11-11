{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | Chat-completion client for the marketing assistant: prompt assembly from
-- retrieved context and reviewed ad examples, model fallback, and the
-- sanitizing of provider replies and errors. Shared by HTTP handlers and
-- background workers; it must not depend on either.
module TDF.Services.LlmChat
  ( OpenAIChatMessage(..)
  , ChatCompletionReq(..)
  , ChatChoice(..)
  , ChatCompletionResp(..)
  , loadAdExamples
  , runRagChat
  , runRagChatWithStatus
  , buildRagMessages
  , callOpenAIChat
  , requestOpenAIChat
  , openAIChatRequestErrorMessage
  , shouldRetryWithFallbackModel
  , extractApiErrorMessage
  , sanitizeApiErrorMessage
  , extractModelReplyText
  , nonEmptyText
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (SomeException, displayException, try)
import           Control.Monad (join)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), defaultOptions, eitherDecode, encode, genericParseJSON, genericToJSON)
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKeyMap
import           Data.Aeson.Types (camelTo2, fieldLabelModifier, parseMaybe, withObject, (.:?), (.!=))
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Database.Persist
import           Database.Persist.Sql (SqlPersistT)
import           GHC.Generics (Generic)
import           Network.HTTP.Client (Manager, Request(..), RequestBody(..), Response, httpLbs, parseRequest, responseBody, responseStatus)
import           Network.HTTP.Types.Status (statusCode)
import           System.IO (hPutStrLn, stderr)

import           TDF.Config (AppConfig(..), llmChatModelCandidates, normalizeLlmApiBase)
import           TDF.DB (sharedTlsManager)
import qualified TDF.ModelsExtra as ME

data OpenAIChatMessage = OpenAIChatMessage
  { role :: Text
  , content :: Text
  } deriving (Show, Generic)

instance ToJSON OpenAIChatMessage where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON OpenAIChatMessage where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data ChatCompletionReq = ChatCompletionReq
  { model :: Text
  , messages :: [OpenAIChatMessage]
  , temperature :: Maybe Double
  } deriving (Show, Generic)

instance ToJSON ChatCompletionReq where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data ChatChoice = ChatChoice
  { message :: OpenAIChatMessage
  } deriving (Show, Generic)
instance FromJSON ChatChoice where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data ChatCompletionResp = ChatCompletionResp
  { choices :: [ChatChoice]
  } deriving (Show, Generic)
instance FromJSON ChatCompletionResp where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runRagChat :: AppConfig -> [Text] -> [Entity ME.AdConversationExample] -> Text -> Maybe Text -> IO Text
runRagChat cfg kb examples userMsg mChannel = do
  res <- runRagChatWithStatus cfg kb examples userMsg mChannel
  pure $
    case res of
      Right txt | not (T.null (T.strip txt)) -> T.strip txt
      _ -> "No pude generar una respuesta automática en este momento."

runRagChatWithStatus
  :: AppConfig
  -> [Text]
  -> [Entity ME.AdConversationExample]
  -> Text
  -> Maybe Text
  -> IO (Either Text Text)
runRagChatWithStatus cfg kb examples userMsg mChannel =
  callOpenAIChat cfg (buildRagMessages kb examples userMsg mChannel)

buildRagMessages
  :: [Text]
  -> [Entity ME.AdConversationExample]
  -> Text
  -> Maybe Text
  -> [OpenAIChatMessage]
buildRagMessages kb examples userMsg mChannel =
  let contextBlock = if null kb then "No hay contexto" else T.intercalate "\n\n" kb
      exampleMsgs = concatMap exampleToMessages examples
      channelNote = maybe "" (\ch -> "[Canal: " <> T.strip ch <> "] ") mChannel
      systemIntro = T.intercalate "\n"
        [ "Eres un asistente de marketing de TDF Records."
        , "Responde en el idioma del usuario con un tono cálido y conciso."
        , "Tu objetivo es ayudar al usuario y, cuando sea relevante, promocionar el Curso de Producción Musical (presencial, cuatro sábados, 16 horas en total, $150 USD, cupos limitados) con link: https://www.tdfrecords.net/curso/produccion-musical-jun-2026"
        , "REGLA CRÍTICA DE FORMATO: responde SOLO con una de estas dos formas:"
        , "1) SEND: <tu respuesta final para enviar al usuario>"
        , "2) HOLD: <por qué no puedes responder todavía>\\nNEED: <lista corta de datos que faltan>"
        , "No incluyas nada fuera de ese formato."
        ]
  in [ mkMsg "system" systemIntro
     , mkMsg "system" ("Contexto de negocio:\n" <> contextBlock)
     ] ++ exampleMsgs ++ [mkMsg "user" (channelNote <> userMsg)]
  where
    mkMsg r c = OpenAIChatMessage { role = r, content = c }
    exampleToMessages (Entity _ ex) =
      [ mkMsg "user" (ME.adConversationExampleUserMessage ex)
      , mkMsg "assistant" (ME.adConversationExampleAssistantMessage ex)
      ]

callOpenAIChat :: AppConfig -> [OpenAIChatMessage] -> IO (Either Text Text)
callOpenAIChat cfg messages =
  case openAiApiKey cfg of
    Nothing -> pure (Left "OPENAI_API_KEY no configurada")
    Just key -> do
      manager <- pure sharedTlsManager
      let apiBase = normalizeLlmApiBase (chatKitApiBase cfg)
      reqBase <- parseRequest (T.unpack (apiBase <> "/v1/chat/completions"))
      let models = llmChatModelCandidates (openAiModel cfg)
      tryModels manager reqBase key models Nothing
  where
    tryModels :: Manager -> Request -> Text -> [Text] -> Maybe Text -> IO (Either Text Text)
    tryModels _ _ _ [] lastErr =
      pure (Left (fromMaybe "No pude generar una respuesta automática en este momento." lastErr))
    tryModels manager reqBase key (modelName:remaining) _ = do
      attempt <- requestOpenAIChat manager reqBase key modelName messages
      case attempt of
        Right reply -> pure (Right reply)
        Left (status, msg)
          | shouldRetryWithFallbackModel status msg && not (null remaining) -> do
              hPutStrLn stderr
                ("[openai] Modelo " <> T.unpack modelName <> " no disponible (" <> show status <> "). Intentando fallback.")
              tryModels manager reqBase key remaining (Just msg)
          | otherwise ->
              pure (Left msg)

requestOpenAIChat
  :: Manager
  -> Request
  -> Text
  -> Text
  -> [OpenAIChatMessage]
  -> IO (Either (Int, Text) Text)
requestOpenAIChat manager reqBase key modelName messages = do
  let body = encode ChatCompletionReq
        { model = modelName
        , messages = messages
        , temperature = Just 1.0
        }
      req =
        reqBase
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", "Bearer " <> TE.encodeUtf8 key)
              ]
          , requestBody = RequestBodyLBS body
          }
  respResult <- try (httpLbs req manager) :: IO (Either SomeException (Response BL.ByteString))
  case respResult of
    Left err ->
      pure (Left (0, openAIChatRequestErrorMessage err))
    Right resp -> do
      let status = statusCode (responseStatus resp)
          raw = responseBody resp
      if status >= 200 && status < 300
        then case (eitherDecode raw :: Either String Value) of
          Left err ->
            pure (Left (status, T.pack err))
          Right payload ->
            case extractModelReplyText payload of
              Just reply -> pure (Right reply)
              Nothing ->
                pure (Left (status, fromMaybe "Sin respuesta de modelo" (extractApiErrorMessage payload)))
        else do
          let baseMsg = "Error al generar respuesta (HTTP " <> T.pack (show status) <> ")"
              msg = case (eitherDecode raw :: Either String Value) of
                Right payload -> fromMaybe baseMsg (extractApiErrorMessage payload)
                Left _ -> baseMsg
          pure (Left (status, msg))

openAIChatRequestErrorMessage :: SomeException -> Text
openAIChatRequestErrorMessage err =
  fromMaybe "OpenAI chat request failed" $
    sanitizeApiErrorMessage ("OpenAI chat request failed: " <> T.pack (displayException err))

shouldRetryWithFallbackModel :: Int -> Text -> Bool
shouldRetryWithFallbackModel status rawMessage =
  statusAllowsFallback && hasMarker && not hasNonFallbackMarker
  where
    msg = T.toLower (T.strip rawMessage)
    statusAllowsFallback = status == 400 || status == 403 || status == 404
    hasMarker = any (`T.isInfixOf` msg) markers || hasMissingModelMarker
    hasNonFallbackMarker = any (`T.isInfixOf` msg) nonFallbackMarkers
    hasMissingModelMarker =
      "does not exist" `T.isInfixOf` msg
        && any (`T.isInfixOf` msg)
          [ "the requested model "
          , "requested model "
          , "the model `"
          , "the model '"
          , "the model \""
          ]
    markers =
      [ "does not have access to model"
      , "doesn't have access to model"
      , "do not have access to model"
      , "don't have access to model"
      , "no access to model"
      , "does not exist or you do not have access"
      , "model not found"
      , "unknown model"
      , "invalid model id"
      , "invalid model name"
      , "not a valid model"
      , "model_not_found"
      , "temperature is not supported"
      , "invalid temperature"
      ]
    nonFallbackMarkers =
      [ "rate limit"
      , "too many requests"
      , "quota"
      , "connection"
      , "connecting"
      , "network"
      , "timeout"
      , "timed out"
      , "dns"
      , "tls"
      , "certificate"
      , "model response"
      , "response format"
      , "authentication_error"
      , "invalid_api_key"
      , "invalid api key"
      , "incorrect api key"
      , "unauthorized"
      , "unauthorised"
      , "permission_denied"
      , "permission denied"
      , "forbidden"
      , "billing"
      , "billing_hard_limit"
      , "billing issue"
      , "payment_required"
      , "payment required"
      , "payment method"
      ]

loadAdExamples :: Bool -> [ME.AdCreativeId] -> SqlPersistT IO [Entity ME.AdConversationExample]
loadAdExamples hasScope adIds
  | hasScope && null adIds = pure []
  | null adIds = selectList [] [Desc ME.AdConversationExampleUpdatedAt, LimitTo 6]
  | otherwise = selectList [ME.AdConversationExampleAdId <-. adIds] [Desc ME.AdConversationExampleUpdatedAt, LimitTo 6]

extractApiErrorMessage :: Value -> Maybe Text
extractApiErrorMessage = parseMaybe $ withObject "ApiError" $ \o -> do
  mErr <- o .:? "error"
  candidates <- case mErr of
    Just (String msg) ->
      pure [msg]
    Just (Object errObj) -> do
      let mType = lookupText "type" errObj
          mCode = lookupText "code" errObj
          mStatus = lookupText "status" errObj
          mMessage = lookupText "message" errObj
      pure (catMaybes [mType, mCode, mStatus, mMessage])
    _ -> do
      let mType = lookupText "type" o
          mCode = lookupText "code" o
          mStatus = lookupText "status" o
          mMessage = lookupText "message" o
      pure (catMaybes [mType, mCode, mStatus, mMessage])
  requireSanitizedText candidates
  where
    lookupText fieldName obj =
      case AKeyMap.lookup (AKey.fromText fieldName) obj of
        Just (String txt) -> Just txt
        _ -> Nothing

    requireSanitizedText values =
      case nonEmptyText (T.intercalate ": " (map T.strip values)) of
        Just txt ->
          maybe (fail "message missing") pure (sanitizeApiErrorMessage txt)
        Nothing -> fail "message missing"

maxApiErrorMessageChars :: Int
maxApiErrorMessageChars = 500

sanitizeApiErrorMessage :: Text -> Maybe Text
sanitizeApiErrorMessage raw =
  nonEmptyText $
    if T.length redacted > maxApiErrorMessageChars
      then T.take maxApiErrorMessageChars redacted <> " [truncated]"
      else redacted
  where
    redacted =
      redactApiErrorSecrets $
        T.unwords $
          T.words $
            T.map normalizeErrorChar raw
    normalizeErrorChar ch
      | isControl ch = ' '
      | generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator] = ' '
      | otherwise = ch

redactApiErrorSecrets :: Text -> Text
redactApiErrorSecrets =
  redactSensitiveApiErrorFields . redactBearerTokens

redactBearerTokens :: Text -> Text
redactBearerTokens = go Nothing
  where
    go previous textValue =
      case matchBearerToken previous textValue of
        Just (prefix, rest) ->
          prefix <> "[redacted]" <> go Nothing rest
        Nothing ->
          case T.uncons textValue of
            Nothing -> ""
            Just (ch, rest) -> T.singleton ch <> go (Just ch) rest

matchBearerToken :: Maybe Char -> Text -> Maybe (Text, Text)
matchBearerToken previous textValue
  | not (isApiErrorSecretBoundary previous) = Nothing
  | not ("bearer" `T.isPrefixOf` T.toLower textValue) = Nothing
  | otherwise =
      let bearerText = T.take 6 textValue
          afterBearer = T.drop 6 textValue
          (between, tokenStart) = T.span isSpace afterBearer
          (openingQuote, tokenText, isValueEnd) = consumeApiErrorValueOpeningQuote tokenStart
          (tokenValue, rest) = T.break isValueEnd tokenText
      in if T.null between || T.null tokenValue || not (T.any isApiErrorSecretAtom tokenValue)
           then Nothing
           else Just (bearerText <> between <> openingQuote, rest)

redactSensitiveApiErrorFields :: Text -> Text
redactSensitiveApiErrorFields = go Nothing
  where
    go previous textValue =
      case matchSensitiveApiErrorField previous textValue of
        Just (prefix, rest) ->
          prefix <> "[redacted]" <> go Nothing rest
        Nothing ->
          case T.uncons textValue of
            Nothing -> ""
            Just (ch, rest) -> T.singleton ch <> go (Just ch) rest

matchSensitiveApiErrorField :: Maybe Char -> Text -> Maybe (Text, Text)
matchSensitiveApiErrorField previous textValue
  | not (isApiErrorSecretBoundary previous) = Nothing
  | otherwise = firstMatch sensitiveApiErrorFields
  where
    lowered = T.toLower textValue

    firstMatch [] = Nothing
    firstMatch (fieldName:rest) =
      case parseSensitiveApiErrorField fieldName lowered textValue of
        Just match -> Just match
        Nothing -> firstMatch rest

sensitiveApiErrorFields :: [Text]
sensitiveApiErrorFields =
  [ "access_token"
  , "accesstoken"
  , "api_key"
  , "api-key"
  , "apikey"
  , "authorization"
  , "bearertoken"
  , "client_secret"
  , "clientsecret"
  , "id_token"
  , "refresh_token"
  , "refreshtoken"
  , "x-api-key"
  ]

parseSensitiveApiErrorField :: Text -> Text -> Text -> Maybe (Text, Text)
parseSensitiveApiErrorField fieldName lowered textValue
  | not (fieldName `T.isPrefixOf` lowered) = Nothing
  | otherwise = do
      let fieldLength = T.length fieldName
          fieldText = T.take fieldLength textValue
          afterField = T.drop fieldLength textValue
          (closingQuote, afterClosingQuote) = consumeApiErrorOptionalQuote afterField
          (beforeSeparator, separatorCandidate) = T.span isSpace afterClosingQuote
      (separator, afterSeparator) <- T.uncons separatorCandidate
      if separator == '=' || separator == ':'
        then
          let (afterSeparatorSpace, valueStart) = T.span isSpace afterSeparator
              (openingQuote, valueText, isValueEnd) = consumeApiErrorValueOpeningQuote valueStart
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

isApiErrorSecretBoundary :: Maybe Char -> Bool
isApiErrorSecretBoundary Nothing = True
isApiErrorSecretBoundary (Just ch) =
  not (isAlphaNum ch || ch == '_' || ch == '-')

isApiErrorSecretAtom :: Char -> Bool
isApiErrorSecretAtom ch =
  isAlphaNum ch || ch `elem` (".-_~+/=" :: String)

consumeApiErrorOptionalQuote :: Text -> (Text, Text)
consumeApiErrorOptionalQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest)
    Just ('\'', rest) -> ("'", rest)
    _ -> ("", textValue)

consumeApiErrorValueOpeningQuote :: Text -> (Text, Text, Char -> Bool)
consumeApiErrorValueOpeningQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest, (== '"'))
    Just ('\'', rest) -> ("'", rest, (== '\''))
    _ -> ("", textValue, isUnquotedApiErrorSecretValueEnd)

isUnquotedApiErrorSecretValueEnd :: Char -> Bool
isUnquotedApiErrorSecretValueEnd ch =
  isSpace ch || ch `elem` ("&,;}]" :: String)

extractModelReplyText :: Value -> Maybe Text
extractModelReplyText payload =
  extractChatCompletionText payload
    <|> extractResponsesOutputText payload

extractChatCompletionText :: Value -> Maybe Text
extractChatCompletionText payload = do
  ChatCompletionResp
    { choices = (ChatChoice OpenAIChatMessage{role = replyRole, content = reply} : _)
    } <- parseMaybe parseJSON payload
  if replyRole == "assistant"
    then validateModelReplyText reply
    else Nothing

extractResponsesOutputText :: Value -> Maybe Text
extractResponsesOutputText payload = parseMaybe parsePayload payload
  where
    parsePayload = withObject "ResponsesPayload" $ \o -> do
      mOutputText <- o .:? "output_text"
      case mOutputText >>= validateModelReplyText of
        Just txt -> pure txt
        Nothing -> do
          outputs <- o .:? "output" .!= ([] :: [Value])
          let fragments = concatMap extractOutputFragments outputs
          case validateModelReplyText (T.intercalate "\n" fragments) of
            Just txt -> pure txt
            Nothing -> fail "output text missing"

maxModelReplyTextChars :: Int
maxModelReplyTextChars = 8192

validateModelReplyText :: Text -> Maybe Text
validateModelReplyText rawReply = do
  reply <- nonEmptyText rawReply
  if T.length reply <= maxModelReplyTextChars
      && not (T.any invalidModelReplyChar reply)
    then Just reply
    else Nothing

invalidModelReplyChar :: Char -> Bool
invalidModelReplyChar ch =
  (isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t')
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

extractOutputFragments :: Value -> [Text]
extractOutputFragments value =
  let directText =
        catMaybes
          [ join (parseMaybe (withObject "OutputItemText" (\o -> o .:? "text")) value) >>= nonEmptyText
          , join (parseMaybe (withObject "OutputItemContentText" (\o -> o .:? "content")) value) >>= nonEmptyText
          ]
      contentParts =
        fromMaybe []
          (parseMaybe (withObject "OutputItemContent" (\o -> o .:? "content" .!= ([] :: [Value]))) value)
      partText =
        concatMap
          (\part ->
            catMaybes
              [ join (parseMaybe (withObject "OutputPartText" (\o -> o .:? "text")) part) >>= nonEmptyText
              , join (parseMaybe (withObject "OutputPartContentText" (\o -> o .:? "content")) part) >>= nonEmptyText
              ]
          )
          contentParts
  in directText <> partText

nonEmptyText :: Text -> Maybe Text
nonEmptyText txt =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just trimmed
