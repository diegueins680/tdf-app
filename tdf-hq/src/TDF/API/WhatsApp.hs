{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.WhatsApp (WhatsAppApi, whatsappServer, LeadsCompleteApi, leadsCompleteServer) where

import Servant
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)
import Network.HTTP.Client (Manager)
import Database.PostgreSQL.Simple (Connection, execute, Only(..))
import Data.Char (isDigit)

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

whatsappServer :: Manager -> Connection -> Server WhatsAppApi
whatsappServer mgr conn =
       hookVerifyH
  :<|> hookReceiveH mgr conn
  :<|> previewH mgr conn

-- Webhook verification with hub.mode validation
hookVerifyH :: Maybe Text -> Maybe Text -> Maybe Text -> Handler Text
hookVerifyH mmode mchall mtoken = do
  expected <- liftIO $ fmap T.pack <$> lookupEnv "WA_VERIFY_TOKEN"
  case (mmode, mchall, mtoken, expected) of
    (Just mode, Just c, Just t, Just e) | T.toLower mode == "subscribe" && t == e -> pure c
    _ -> throwError err403

hookReceiveH :: Manager -> Connection -> WAMetaWebhook -> Handler Value
hookReceiveH mgr conn payload = do
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
           -- Validate phone format
           unless (isValidE164 fromPhone) $
             throwError err400 { errBody = "Invalid phone number format" }
           mRes <- liftIO $ mintLink conn fromPhone
           case mRes of
             Nothing -> throwError err404 { errBody = "Course edition not found" }
             Just (url, lid) -> do
               waResult <- liftIO $ sendViaWA mgr fromPhone url
               case waResult of
                 Left errMsg -> pure $ object ["sent" .= False, "error" .= errMsg, "leadId" .= lid]
                 Right () -> do
                   _ <- liftIO $ execute conn "UPDATE lead SET status='LINK_SENT' WHERE id=?" (Only lid)
                   pure $ object ["sent" .= True, "leadId" .= lid, "url" .= url]
         else pure $ object ["ignored" .= True]

previewH :: Manager -> Connection -> PreviewReq -> Handler Value
previewH mgr conn (PreviewReq p) = do
  -- Validate phone format
  unless (isValidE164 p) $
    throwError err400 { errBody = "Invalid phone number format" }
  mRes <- liftIO $ mintLink conn p
  case mRes of
    Nothing -> throwError err404 { errBody = "Course edition not found" }
    Just (url, lid) -> do
      waResult <- liftIO $ sendViaWA mgr p url
      case waResult of
        Left errMsg -> pure $ object ["sent" .= False, "error" .= errMsg, "leadId" .= lid]
        Right () -> pure $ object ["sent" .= True, "leadId" .= lid, "url" .= url]

-- Link minting & sender -------------------------------------------------------

mintLink :: Connection -> Text -> IO (Maybe (Text, Int))
mintLink conn phone = do
  mslug <- lookupEnv "COURSE_EDITION_SLUG"
  let slug = maybe "produccion-musical-dic-2025" T.pack mslug
  mCeId <- lookupCourseIdBySlug conn slug
  case mCeId of
    Nothing -> pure Nothing
    Just ceId -> do
      (lid, tok) <- ensureLead conn phone ceId
      mHard <- lookupEnv "COURSE_REG_URL"
      case mHard of
        Just hard -> pure $ Just (T.pack hard, lid)
        Nothing   -> do
          mBase <- lookupEnv "HQ_APP_URL"
          let base = maybe "http://localhost:5173" id mBase
          pure $ Just ( T.pack base <> "/inscripcion/" <> slug
                         <> "?lead=" <> T.pack (show lid) <> "&t=" <> tok
                       , lid)

sendViaWA :: Manager -> Text -> Text -> IO (Either Text ())
sendViaWA mgr to url = do
  tok <- fmap (maybe "" T.pack) (lookupEnv "WA_TOKEN")
  pid <- fmap (maybe "" T.pack) (lookupEnv "WA_PHONE_ID")
  -- Use configurable message template with URL placeholder
  mTemplate <- lookupEnv "WA_MSG_TEMPLATE"
  let defaultMsg = "¡Gracias por tu interés en el Curso de Producción Musical! Aquí está tu enlace de inscripción: {url}\nCupos limitados (10)."
      template = maybe defaultMsg T.pack mTemplate
      msg = T.replace "{url}" url template
  result <- sendText mgr tok pid to msg
  -- Check if send was successful (basic validation)
  case result of
    Object obj -> 
      case KeyMap.lookup (Key.fromString "error") obj of
        Just _ -> pure $ Left "WhatsApp API error"
        Nothing -> pure $ Right ()
    _ -> pure $ Right ()

-- Validation helpers ----------------------------------------------------------

-- Validate E.164 phone format: starts with + and contains only digits after
isValidE164 :: Text -> Bool
isValidE164 t =
  case T.uncons t of
    Just ('+', rest) -> not (T.null rest) && T.all isDigit rest && T.length rest >= 7 && T.length rest <= 15
    _ -> False

-- Validate email format (basic validation)
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

-- Lead completion -------------------------------------------------------------

type LeadsCompleteApi =
  "api" :> "leads" :> Capture "id" Int :> "complete" :>
  ReqBody '[JSON] CompleteReq :> Post '[JSON] Value

data CompleteReq = CompleteReq { token :: Text, name :: Text, email :: Text }
  deriving (Show, Generic)
instance FromJSON CompleteReq

leadsCompleteServer :: Connection -> Server LeadsCompleteApi
leadsCompleteServer conn lid (CompleteReq tok nm em) = do
  -- Validate input
  when (T.null nm || T.length nm > 200) $
    throwError err400 { errBody = "Invalid name: must be 1-200 characters" }
  unless (isValidEmail em) $
    throwError err400 { errBody = "Invalid email format" }
  
  -- Update lead and invalidate token to prevent reuse
  n <- liftIO $ execute conn
       "UPDATE lead SET display_name=?, email=?, status='COMPLETED', token=NULL WHERE id=? AND token=? AND status != 'COMPLETED'"
       (nm, em, lid, tok)
  pure $ object ["ok" .= (n == 1)]
