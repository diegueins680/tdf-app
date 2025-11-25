{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.WhatsApp (WhatsAppApi, whatsappServer, LeadsCompleteApi, leadsCompleteServer) where

import Servant
import GHC.Generics (Generic)
import Data.Aeson
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
hookVerifyH _ mchall mtoken = do
  svc <- liftIO mkWhatsAppService
  case (mchall, mtoken, waVerifyToken (waConfig svc)) of
    (Just c, Just t, Just e) | t == e -> pure c
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
           (resp, _) <- liftIO $ enrollPhone svc conn fromPhone
           pure resp
         else pure $ object ["ignored" .= True]

previewH :: Connection -> PreviewReq -> Handler Value
previewH conn (PreviewReq p) = do
  svc <- liftIO mkWhatsAppService
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
  n <- liftIO $ execute conn
       "UPDATE lead SET display_name=?, email=?, status='COMPLETED' WHERE id=? AND token=?"
       (nm, em, lid, tok)
  pure $ object ["ok" .= (n == 1)]
