{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.WhatsApp (WhatsAppApi, whatsappServer, LeadsCompleteApi, leadsCompleteServer) where

import Servant
import GHC.Generics (Generic)
import Data.Aeson
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Database.PostgreSQL.Simple (Connection, execute, Only(..))

import TDF.WhatsApp.Types
import TDF.WhatsApp.Client (sendText)
import TDF.Leads.Model

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
  expected <- liftIO $ fmap T.pack <$> lookupEnv "WA_VERIFY_TOKEN"
  case (mchall, mtoken, expected) of
    (Just c, Just t, Just e) | t == e -> pure c
    _ -> throwError err403

hookReceiveH :: Connection -> WAMetaWebhook -> Handler Value
hookReceiveH conn payload = do
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
           (url, lid) <- liftIO $ mintLink conn fromPhone
           _ <- liftIO $ sendViaWA fromPhone url
           _ <- liftIO $ execute conn "UPDATE lead SET status='LINK_SENT' WHERE id=?" (Only lid)
           pure $ object ["sent" .= True, "leadId" .= lid, "url" .= url]
         else pure $ object ["ignored" .= True]

previewH :: Connection -> PreviewReq -> Handler Value
previewH conn (PreviewReq p) = do
  (url, lid) <- liftIO $ mintLink conn p
  _ <- liftIO $ sendViaWA p url
  pure $ object ["sent" .= True, "leadId" .= lid, "url" .= url]

-- Link minting & sender -------------------------------------------------------

mintLink :: Connection -> Text -> IO (Text, Int)
mintLink conn phone = do
  mslug <- lookupEnv "COURSE_EDITION_SLUG"
  let slug = maybe "produccion-musical-dic-2025" T.pack mslug
  mCeId <- lookupCourseIdBySlug conn slug
  ceId <- maybe (fail "course_edition not found") pure mCeId
  (lid, tok) <- ensureLead conn phone ceId
  mHard <- lookupEnv "COURSE_REG_URL"
  case mHard of
    Just hard -> pure (T.pack hard, lid)
    Nothing   -> do
      mBase <- lookupEnv "HQ_APP_URL"
      let base = maybe "http://localhost:5173" id mBase
      pure ( T.pack base <> "/inscripcion/" <> slug
             <> "?lead=" <> T.pack (show lid) <> "&t=" <> tok
           , lid)

sendViaWA :: Text -> Text -> IO Value
sendViaWA to url = do
  tok <- fmap (maybe "" T.pack) (lookupEnv "WA_TOKEN")
  pid <- fmap (maybe "" T.pack) (lookupEnv "WA_PHONE_ID")
  mgr <- newManager tlsManagerSettings
  let msg = "¡Gracias por tu interés en el Curso de Producción Musical! Aquí está tu enlace de inscripción: "
            <> url <> "\nCupos limitados (10)."
  _ <- sendText mgr tok pid to msg
  pure (object ["ok" .= True])

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
