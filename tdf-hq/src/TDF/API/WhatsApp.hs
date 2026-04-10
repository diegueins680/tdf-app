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
  , CompleteReq(..)
  ) where

import Servant
import GHC.Generics (Generic)
import Data.Aeson
import Control.Monad (unless)
import Data.Char (isAlphaNum, isAscii, isDigit)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection, Only(..), execute, query)

import TDF.WhatsApp.Types
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
instance FromJSON PreviewReq

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
  let mMsg = do
        ent <- listToMaybe (entry payload)
        chg <- listToMaybe (changes ent)
        msgs <- messages (value chg)
        listToMaybe msgs
  case mMsg of
    Nothing -> pure $ object ["ok" .= True, "reason" .= ("no-message" :: Text)]
    Just msg ->
      let normText = T.toUpper . T.strip $ maybe "" body (text msg)
          fromPhone = from msg
          looksEnroll = ("INSCRIBIRME" `T.isInfixOf` normText) || ("INSCRIBIR" `T.isInfixOf` normText)
      in if looksEnroll
         then do
           unless (isValidE164 fromPhone) $
             throwError err400 { errBody = "Invalid phone number format" }
           (resp, _) <- liftIO $ enrollPhone svc conn fromPhone
           pure resp
         else pure $ object ["ignored" .= True]

previewH :: Connection -> PreviewReq -> Handler Value
previewH conn (PreviewReq p) = do
  svc <- liftIO mkWhatsAppService
  unless (isValidE164 p) $
    throwError err400 { errBody = "Invalid phone number format" }
  liftIO $ previewEnrollment svc conn p

-- Link minting & sender -------------------------------------------------------

-- Lead completion -------------------------------------------------------------

type LeadsCompleteApi =
  "api" :> "leads" :> Capture "id" Int :> "complete" :>
  ReqBody '[JSON] CompleteReq :> Post '[JSON] Value

data CompleteReq = CompleteReq { token :: Text, name :: Text, email :: Text }
  deriving (Eq, Show, Generic)
instance FromJSON CompleteReq

leadsCompleteServer :: Connection -> Server LeadsCompleteApi
leadsCompleteServer conn lid rawReq = do
  leadId <- either throwError pure (validateLeadCompletionId lid)
  CompleteReq tok nm em <- either throwError pure (validateLeadCompletionRequest rawReq)
  existing <- liftIO $ query conn
      "SELECT status, token FROM lead WHERE id = ? LIMIT 1"
      (Only leadId)
  either throwError pure (validateLeadCompletionLookup tok (listToMaybe existing))

  n <- liftIO $ execute conn
       "UPDATE lead SET display_name=?, email=?, status='COMPLETED', token=NULL WHERE id=? AND token=? AND status != 'COMPLETED'"
       (nm, em, leadId, tok)
  either throwError pure (ensureLeadCompletionUpdated n)
  pure $ object ["ok" .= True]

-- Validation helpers ----------------------------------------------------------

isValidE164 :: Text -> Bool
isValidE164 t =
  case T.uncons t of
    Just ('+', rest) -> not (T.null rest) && T.all isDigit rest && T.length rest >= 7 && T.length rest <= 15
    _ -> False

isValidEmail :: Text -> Bool
isValidEmail candidate =
  case T.split (== '@') candidate of
    [localPart, domain] ->
      not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate) &&
      not (T.null localPart) &&
      not (T.null domain) &&
      T.isInfixOf "." domain &&
      all isValidDomainLabel (T.splitOn "." domain)
    _ -> False

isValidDomainLabel :: Text -> Bool
isValidDomainLabel label =
  not (T.null label) &&
  not (T.isPrefixOf "-" label) &&
  not (T.isSuffixOf "-" label) &&
  T.all isValidDomainChar label

isValidDomainChar :: Char -> Bool
isValidDomainChar c = (isAscii c && isAlphaNum c) || c == '-'

validateLeadCompletionRequest :: CompleteReq -> Either ServerError CompleteReq
validateLeadCompletionRequest (CompleteReq rawToken rawName rawEmail)
  | T.null tokenValue =
      Left err400 { errBody = "Completion token is required" }
  | T.null nameValue || T.length nameValue > 200 =
      Left err400 { errBody = "Invalid name: must be 1-200 characters" }
  | not (isValidEmail emailValue) =
      Left err400 { errBody = "Invalid email format" }
  | otherwise =
      Right (CompleteReq tokenValue nameValue emailValue)
  where
    tokenValue = T.strip rawToken
    nameValue = T.strip rawName
    emailValue = T.strip rawEmail

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
  | mStoredToken /= Just suppliedToken =
      Left err403 { errBody = "Invalid completion token" }
  | otherwise =
      Right ()

ensureLeadCompletionUpdated :: Int64 -> Either ServerError ()
ensureLeadCompletionUpdated updatedRows
  | updatedRows == 1 =
      Right ()
  | otherwise =
      Left err409 { errBody = "Lead completion could not be applied" }

validateHookVerifyRequest :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Either ServerError Text
validateHookVerifyRequest mmode mchall mtoken mExpected =
  case nonBlank mExpected of
    Nothing ->
      Left err503 { errBody = "WhatsApp verify token not configured" }
    Just expected ->
      case fmap T.toLower (nonBlank mmode) of
        Nothing ->
          Left err400 { errBody = "hub.mode is required" }
        Just "subscribe" ->
          case nonBlank mchall of
            Nothing ->
              Left err400 { errBody = "hub.challenge is required" }
            Just challenge ->
              case nonBlank mtoken of
                Nothing ->
                  Left err400 { errBody = "hub.verify_token is required" }
                Just verifyToken
                  | verifyToken == expected -> Right challenge
                  | otherwise -> Left err403 { errBody = "hub.verify_token mismatch" }
        Just _ ->
          Left err400 { errBody = "hub.mode must be subscribe" }
  where
    nonBlank :: Maybe Text -> Maybe Text
    nonBlank mTxt =
      case fmap T.strip mTxt of
        Just txt | not (T.null txt) -> Just txt
        _ -> Nothing
