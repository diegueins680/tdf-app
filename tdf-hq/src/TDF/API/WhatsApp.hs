{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.WhatsApp
  ( WhatsAppApi
  , whatsappServer
  , LeadsCompleteApi
  , leadsCompleteServer
  , validateHookVerifyRequest
  , validateLeadCompletionRequest
  , validateLeadCompletionId
  , validateLeadCompletionLookup
  , ensureLeadCompletionUpdated
  , leadCompletionConsumedToken
  , extractFirstWebhookMessage
  , extractFirstEnrollmentWebhookMessage
  , PreviewReq(..)
  , CompleteReq(..)
  ) where

import Servant
import GHC.Generics (Generic)
import Data.Aeson
  ( FromJSON(..)
  , Value
  , object
  , (.=)
  , defaultOptions
  , genericParseJSON
  , rejectUnknownFields
  )
import Data.Aeson.Types (Parser)
import Control.Monad (unless)
import Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator, Space)
  , generalCategory
  , isAlphaNum
  , isAscii
  , isAsciiLower
  , isControl
  , isDigit
  , isSpace
  )
import Data.Int (Int64)
import Data.Maybe (isNothing, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection, Only(..), execute, query)

import TDF.WhatsApp.Types
import TDF.WhatsApp.History (normalizeWhatsAppPhone)
import TDF.WhatsApp.Service

-- GET verification + POST inbound + preview
type WhatsAppApi =
       "hooks" :> "whatsapp"
         :> QueryParam "hub.mode" Text
         :> QueryParam "hub.challenge" Text
         :> QueryParam "hub.verify_token" Text
         :> Get '[PlainText] Text
  :<|> "hooks" :> "whatsapp" :> ReqBody '[JSON] WAMetaWebhook :> Post '[JSON] Value
  :<|> "api"   :> "leads" :> "preview-link" :> ReqBody '[JSON] PreviewReq :> Post '[JSON] Value

data PreviewReq = PreviewReq { phone :: Text } deriving (Show, Generic)
instance FromJSON PreviewReq where
  parseJSON rawValue = do
    req <- genericParseJSON defaultOptions
      { rejectUnknownFields = True
      } rawValue
    phoneValue <- normalizePreviewPhone (phone req)
    pure req { phone = phoneValue }

whatsappServer :: Connection -> Server WhatsAppApi
whatsappServer conn =
       hookVerifyH
  :<|> hookReceiveH conn
  :<|> previewH conn

hookVerifyH :: Maybe Text -> Maybe Text -> Maybe Text -> Handler Text
hookVerifyH mmode mchall mtoken = do
  svc <- liftIO mkWhatsAppService
  case validateHookVerifyRequest mmode mchall mtoken (waVerifyToken (waConfig svc)) of
    Right challenge -> pure challenge
    Left err -> throwError err

hookReceiveH :: Connection -> WAMetaWebhook -> Handler Value
hookReceiveH conn payload = do
  svc <- liftIO mkWhatsAppService
  case extractFirstEnrollmentWebhookMessage payload of
    Nothing ->
      case extractFirstWebhookMessage payload of
        Nothing -> pure $ object ["ok" .= True, "reason" .= ("no-message" :: Text)]
        Just _ -> pure $ object ["ignored" .= True]
    Just msg ->
      let fromPhone = from msg
      in do
        unless (isValidE164 fromPhone) $
          throwError err400 { errBody = "Invalid phone number format" }
        (resp, _) <- liftIO $ enrollPhone svc conn fromPhone
        pure resp

previewH :: Connection -> PreviewReq -> Handler Value
previewH conn (PreviewReq p) = do
  svc <- liftIO mkWhatsAppService
  unless (isValidE164 p) $
    throwError err400 { errBody = "Invalid phone number format" }
  liftIO $ previewEnrollment svc conn p

extractFirstWebhookMessage :: WAMetaWebhook -> Maybe WAMessage
extractFirstWebhookMessage =
  listToMaybe . extractWebhookTextMessages

extractFirstEnrollmentWebhookMessage :: WAMetaWebhook -> Maybe WAMessage
extractFirstEnrollmentWebhookMessage =
  listToMaybe . filter isEnrollmentWebhookMessage . extractWebhookTextMessages

extractWebhookTextMessages :: WAMetaWebhook -> [WAMessage]
extractWebhookTextMessages payload =
  [ msg
      { from = senderId
      }
  | ent <- entry payload
  , chg <- changes ent
  , msgs <- maybeToList (messages (value chg))
  , msg <- msgs
  , waType msg == "text"
  , Just senderId <- [normalizeWhatsAppPhone (from msg)]
  , isValidE164 senderId
  , Just txt <- [text msg]
  , not (T.null (T.strip (body txt)))
  ]

isEnrollmentWebhookMessage :: WAMessage -> Bool
isEnrollmentWebhookMessage msg =
  case dropWhile (not . isEnrollmentCommandToken) tokens of
    [] -> False
    commandAndAfter ->
      let beforeCommand = take (length tokens - length commandAndAfter) tokens
      in not (any (`elem` negativeEnrollmentTokens) tokens)
           && (null beforeCommand || any (`elem` affirmativeEnrollmentTokens) beforeCommand)
  where
    tokens = T.words (normalizeEnrollmentCommandText (maybe "" body (text msg)))

normalizeEnrollmentCommandText :: Text -> Text
normalizeEnrollmentCommandText =
  T.unwords . T.words . T.map normalizeChar . T.toUpper
  where
    normalizeChar ch
      | isAlphaNum ch = ch
      | otherwise = ' '

isEnrollmentCommandToken :: Text -> Bool
isEnrollmentCommandToken tokenValue =
  tokenValue == "INSCRIBIRME" || tokenValue == "INSCRIBIR"

affirmativeEnrollmentTokens :: [Text]
affirmativeEnrollmentTokens =
  [ "QUIERO"
  , "DESEO"
  , "ACEPTO"
  , "CONFIRMO"
  , "SI"
  , "SÍ"
  , "OK"
  ]

negativeEnrollmentTokens :: [Text]
negativeEnrollmentTokens =
  [ "NO"
  , "NUNCA"
  , "TAMPOCO"
  , "CANCELAR"
  , "CANCELO"
  , "CANCELEN"
  , "BAJA"
  ]

-- Link minting & sender -------------------------------------------------------

-- Lead completion -------------------------------------------------------------

type LeadsCompleteApi =
  "api" :> "leads" :> Capture "id" Int :> "complete" :>
  ReqBody '[JSON] CompleteReq :> Post '[JSON] Value

data CompleteReq = CompleteReq { token :: Text, name :: Text, email :: Text }
  deriving (Eq, Show, Generic)
instance FromJSON CompleteReq where
  parseJSON = genericParseJSON defaultOptions
    { rejectUnknownFields = True
    }

leadsCompleteServer :: Connection -> Server LeadsCompleteApi
leadsCompleteServer conn lid rawReq = do
  leadId <- either throwError pure (validateLeadCompletionId lid)
  CompleteReq tok nm em <- either throwError pure (validateLeadCompletionRequest rawReq)
  existing <- liftIO $ query conn
      "SELECT status, token FROM lead WHERE id = ? LIMIT 1"
      (Only leadId)
  either throwError pure (validateLeadCompletionLookup tok (listToMaybe existing))

  let consumedToken = leadCompletionConsumedToken leadId
  n <- liftIO $ execute conn
       "UPDATE lead SET display_name=?, email=?, status='COMPLETED', token=? WHERE id=? AND token=? AND status IN ('NEW','LINK_SENT')"
       (nm, em, consumedToken, leadId, tok)
  either throwError pure (ensureLeadCompletionUpdated n)
  pure $ object ["ok" .= True]

-- Validation helpers ----------------------------------------------------------

isValidE164 :: Text -> Bool
isValidE164 t =
  case T.uncons t of
    Just ('+', rest) ->
      case T.uncons rest of
        Just (countryCodeStart, _) ->
          countryCodeStart >= '1'
            && countryCodeStart <= '9'
            && T.all isDigit rest
            && T.length rest >= 7
            && T.length rest <= 15
        Nothing -> False
    _ -> False

normalizePreviewPhone :: Text -> Parser Text
normalizePreviewPhone rawPhone =
  case normalizeWhatsAppPhone rawPhone of
    Just phoneValue
      | isValidE164 phoneValue -> pure phoneValue
    _ -> fail "phone must be a valid E.164 phone number"

isValidEmail :: Text -> Bool
isValidEmail candidate =
  case T.split (== '@') candidate of
    [localPart, domain] ->
      T.length candidate <= maxLeadCompletionEmailChars &&
      isValidEmailLocalPart localPart &&
      not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate) &&
      isValidEmailDomain domain
    _ -> False

isValidEmailDomain :: Text -> Bool
isValidEmailDomain domain =
  not (T.null domain) &&
  T.isInfixOf "." domain &&
  all isValidDomainLabel labels &&
  T.length topLevelLabel >= 2 &&
  T.any isAsciiLower topLevelLabel
  where
    labels = T.splitOn "." domain
    topLevelLabel =
      case reverse labels of
        label : _ -> label
        [] -> ""

isValidEmailLocalPart :: Text -> Bool
isValidEmailLocalPart localPart =
  not (T.null localPart) &&
  T.length localPart <= maxLeadCompletionEmailLocalPartChars &&
  not (T.isPrefixOf "." localPart) &&
  not (T.isSuffixOf "." localPart) &&
  not (".." `T.isInfixOf` localPart) &&
  T.all isValidEmailLocalChar localPart

isValidEmailLocalChar :: Char -> Bool
isValidEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidDomainLabel :: Text -> Bool
isValidDomainLabel label =
  not (T.null label) &&
  T.length label <= maxLeadCompletionEmailDomainLabelChars &&
  not (T.isPrefixOf "-" label) &&
  not (T.isSuffixOf "-" label) &&
  T.all isValidDomainChar label

isValidDomainChar :: Char -> Bool
isValidDomainChar c = (isAscii c && isAlphaNum c) || c == '-'

validateLeadCompletionRequest :: CompleteReq -> Either ServerError CompleteReq
validateLeadCompletionRequest (CompleteReq rawToken rawName rawEmail)
  | T.null tokenValue =
      Left err400 { errBody = "Completion token is required" }
  | not (isValidLeadCompletionToken tokenValue) =
      Left err400 { errBody = "Completion token format is invalid" }
  | T.null nameValue || T.length nameValue > 200 =
      Left err400 { errBody = "Invalid name: must be 1-200 characters" }
  | T.any isUnsafeLeadCompletionNameChar nameValue =
      Left err400
        { errBody =
            "Invalid name: must not contain control or hidden formatting characters"
              <> ", or non-ASCII spaces"
        }
  | not (T.any isAlphaNum nameValue) =
      Left err400 { errBody = "Invalid name: must include letters or numbers" }
  | T.length emailValue > maxLeadCompletionEmailChars =
      Left err400 { errBody = "Invalid email: must be 254 characters or fewer" }
  | not (isValidEmail emailValue) =
      Left err400 { errBody = "Invalid email format" }
  | otherwise =
      Right (CompleteReq tokenValue nameValue emailValue)
  where
    tokenValue = T.strip rawToken
    nameValue = T.strip rawName
    emailValue = T.toLower (T.strip rawEmail)

isValidLeadCompletionToken :: Text -> Bool
isValidLeadCompletionToken tokenValue =
  T.length tokenValue == leadCompletionTokenLength
    && T.all isValidLeadCompletionTokenChar tokenValue

leadCompletionTokenLength :: Int
leadCompletionTokenLength = 20

maxLeadCompletionEmailChars :: Int
maxLeadCompletionEmailChars = 254

maxLeadCompletionEmailLocalPartChars :: Int
maxLeadCompletionEmailLocalPartChars = 64

maxLeadCompletionEmailDomainLabelChars :: Int
maxLeadCompletionEmailDomainLabelChars = 63

isValidLeadCompletionTokenChar :: Char -> Bool
isValidLeadCompletionTokenChar c =
  isAscii c && isAlphaNum c

isUnsafeLeadCompletionNameChar :: Char -> Bool
isUnsafeLeadCompletionNameChar ch =
  isControl ch || isHiddenFormattingChar ch || isNonAsciiSpace ch

isNonAsciiSpace :: Char -> Bool
isNonAsciiSpace ch =
  generalCategory ch == Space && ch /= ' '

isHiddenFormattingChar :: Char -> Bool
isHiddenFormattingChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateLeadCompletionId :: Int -> Either ServerError Int
validateLeadCompletionId leadId
  | leadId > 0 =
      Right leadId
  | otherwise =
      Left err400 { errBody = "leadId must be a positive integer" }

validateLeadCompletionLookup :: Text -> Maybe (Text, Maybe Text) -> Either ServerError ()
validateLeadCompletionLookup _ Nothing =
  Left err404 { errBody = "Lead not found" }
validateLeadCompletionLookup suppliedToken (Just (status, mStoredToken))
  | T.toUpper (T.strip status) == "COMPLETED" =
      Left err409 { errBody = "Lead already completed" }
  | not (isCompletableLeadStatus status) =
      Left err409 { errBody = "Lead completion is not available" }
  | isNothing storedTokenValue =
      Left err409 { errBody = "Lead completion is not available" }
  | storedTokenValue /= Just suppliedToken =
      Left err403 { errBody = "Invalid completion token" }
  | otherwise =
      Right ()
  where
    storedTokenValue = canonicalStoredToken mStoredToken

    canonicalStoredToken :: Maybe Text -> Maybe Text
    canonicalStoredToken mTxt =
      case mTxt of
        Just txt
          | txt == T.strip txt && isValidLeadCompletionToken txt -> Just txt
        _ -> Nothing

isCompletableLeadStatus :: Text -> Bool
isCompletableLeadStatus rawStatus =
  let statusValue = T.strip rawStatus
  in rawStatus == statusValue && statusValue `elem` ["NEW", "LINK_SENT"]

ensureLeadCompletionUpdated :: Int64 -> Either ServerError ()
ensureLeadCompletionUpdated updatedRows
  | updatedRows == 1 =
      Right ()
  | otherwise =
      Left err409 { errBody = "Lead completion could not be applied" }

leadCompletionConsumedToken :: Int -> Text
leadCompletionConsumedToken leadId =
  -- Preserve the legacy NOT NULL token column while replacing the reusable
  -- credential with a value clients cannot submit through completion validation.
  "completed:" <> T.pack (show leadId)

validateHookVerifyRequest :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Either ServerError Text
validateHookVerifyRequest mmode mchall mtoken mExpected =
  case validateConfiguredVerifyToken mExpected of
    Left err -> Left err
    Right expected ->
      case fmap T.toLower (nonBlank mmode) of
        Nothing ->
          Left err400 { errBody = "hub.mode is required" }
        Just "subscribe" ->
          case nonBlankRaw mchall of
            Nothing ->
              Left err400 { errBody = "hub.challenge is required" }
            Just challenge ->
              case validateHookChallenge challenge of
                Left err -> Left err
                Right challengeVal ->
                  case nonBlankRaw mtoken of
                    Nothing ->
                      Left err400 { errBody = "hub.verify_token is required" }
                    Just verifyToken
                      | T.any isUnsafeVerifyTokenChar verifyToken ->
                          Left err400
                            { errBody =
                                "hub.verify_token must not contain control characters or whitespace; hidden formatting characters are not allowed"
                            }
                      | verifyToken == expected -> Right challengeVal
                      | otherwise -> Left err403 { errBody = "hub.verify_token mismatch" }
        Just _ ->
          Left err400 { errBody = "hub.mode must be subscribe" }
  where
    validateConfiguredVerifyToken :: Maybe Text -> Either ServerError Text
    validateConfiguredVerifyToken mTxt =
      case nonBlank mTxt of
        Nothing ->
          Left err503 { errBody = "WhatsApp verify token not configured" }
        Just txt
          | T.any isUnsafeVerifyTokenChar txt ->
              Left err503 { errBody = "WhatsApp verify token is misconfigured" }
          | otherwise ->
              Right txt

    nonBlank :: Maybe Text -> Maybe Text
    nonBlank mTxt =
      case fmap T.strip mTxt of
        Just txt | not (T.null txt) -> Just txt
        _ -> Nothing

    nonBlankRaw :: Maybe Text -> Maybe Text
    nonBlankRaw mTxt =
      case mTxt of
        Just txt | not (T.null (T.strip txt)) -> Just txt
        _ -> Nothing

    validateHookChallenge :: Text -> Either ServerError Text
    validateHookChallenge challenge
      | T.length challenge > 512 =
          Left err400 { errBody = "hub.challenge must be 512 characters or fewer" }
      | T.any isControl challenge =
          Left err400 { errBody = "hub.challenge must not contain control characters" }
      | T.any isSpace challenge =
          Left err400 { errBody = "hub.challenge must not contain whitespace" }
      | T.any isHiddenFormattingChar challenge =
          Left err400
            { errBody = "hub.challenge must not contain hidden formatting characters" }
      | otherwise =
          Right challenge

    isUnsafeVerifyTokenChar ch =
      isControl ch || isSpace ch || isHiddenFormattingChar ch
