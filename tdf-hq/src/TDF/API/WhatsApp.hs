{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.WhatsApp (WhatsAppApi, whatsappServer, LeadsCompleteApi, leadsCompleteServer) where

import Servant
import GHC.Generics (Generic)
import Data.Aeson
import Control.Monad (unless, when)
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection, execute)

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
  let modeOk = maybe True (\m -> T.toLower m == "subscribe") mmode
  case (modeOk, mchall, mtoken, waVerifyToken (waConfig svc)) of
    (True, Just c, Just t, Just e) | t == e -> pure c
    _ -> throwError err403

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
  deriving (Show, Generic)
instance FromJSON CompleteReq

leadsCompleteServer :: Connection -> Server LeadsCompleteApi
leadsCompleteServer conn lid (CompleteReq tok nm em) = do
  when (T.null nm || T.length nm > 200) $
    throwError err400 { errBody = "Invalid name: must be 1-200 characters" }
  unless (isValidEmail em) $
    throwError err400 { errBody = "Invalid email format" }

  n <- liftIO $ execute conn
       "UPDATE lead SET display_name=?, email=?, status='COMPLETED', token=NULL WHERE id=? AND token=? AND status != 'COMPLETED'"
       (nm, em, lid, tok)
  pure $ object ["ok" .= (n == 1)]

-- Validation helpers ----------------------------------------------------------

isValidE164 :: Text -> Bool
isValidE164 t =
  case T.uncons t of
    Just ('+', rest) -> not (T.null rest) && T.all isDigit rest && T.length rest >= 7 && T.length rest <= 15
    _ -> False

isValidEmail :: Text -> Bool
isValidEmail email =
  case T.split (== '@') email of
    [localPart, domain] ->
      not (T.null localPart) &&
      not (T.null domain) &&
      not (T.isPrefixOf "." domain) &&
      not (T.isSuffixOf "." domain) &&
      T.isInfixOf "." domain &&
      T.length domain >= 3
    _ -> False
