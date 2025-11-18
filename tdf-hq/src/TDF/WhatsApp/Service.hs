{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module TDF.WhatsApp.Service
  ( WhatsAppService(..)
  , WhatsAppConfig(..)
  , mkWhatsAppService
  , enrollPhone
  , previewEnrollment
  , verifyTokenMatches
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import System.Environment (lookupEnv)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Database.PostgreSQL.Simple (Connection, execute, Only(..))

import TDF.Leads.Model (ensureLead, lookupCourseIdBySlug)
import TDF.WhatsApp.Client (sendText)

data WhatsAppConfig = WhatsAppConfig
  { waToken       :: Text
  , waPhoneId     :: Text
  , waVerifyToken :: Maybe Text
  , courseSlug    :: Text
  , courseRegUrl  :: Maybe Text
  , appBaseUrl    :: Text
  }

data WhatsAppService = WhatsAppService
  { waManager :: Manager
  , waConfig  :: WhatsAppConfig
  }

mkWhatsAppService :: IO WhatsAppService
mkWhatsAppService = do
  mgr <- newManager tlsManagerSettings
  cfg <- loadConfig
  pure $ WhatsAppService mgr cfg

loadConfig :: IO WhatsAppConfig
loadConfig = do
  tok <- fmap (maybe "" T.pack) (lookupEnv "WA_TOKEN")
  pid <- fmap (maybe "" T.pack) (lookupEnv "WA_PHONE_ID")
  ver <- fmap (fmap T.pack) (lookupEnv "WA_VERIFY_TOKEN")
  mSlug <- lookupEnv "COURSE_EDITION_SLUG"
  mReg  <- lookupEnv "COURSE_REG_URL"
  mBase <- lookupEnv "HQ_APP_URL"
  let cfg = WhatsAppConfig
        { waToken       = tok
        , waPhoneId     = pid
        , waVerifyToken = ver
        , courseSlug    = maybe "produccion-musical-dic-2025" T.pack mSlug
        , courseRegUrl  = fmap T.pack mReg
        , appBaseUrl    = maybe "http://localhost:5173" T.pack mBase
        }
  pure cfg

verifyTokenMatches :: WhatsAppService -> Text -> Bool
verifyTokenMatches svc token = case waVerifyToken (waConfig svc) of
  Nothing -> False
  Just expected -> expected == token

enrollPhone :: WhatsAppService -> Connection -> Text -> IO (Value, Int)
enrollPhone svc conn phone = do
  (url, lid) <- mintLink svc conn phone
  _ <- sendViaWA svc phone url
  _ <- execute conn "UPDATE lead SET status='LINK_SENT' WHERE id=?" (Only lid)
  pure (object ["sent" .= True, "leadId" .= lid, "url" .= url], lid)

previewEnrollment :: WhatsAppService -> Connection -> Text -> IO Value
previewEnrollment svc conn phone = do
  (url, lid) <- mintLink svc conn phone
  _ <- sendViaWA svc phone url
  pure $ object ["sent" .= True, "leadId" .= lid, "url" .= url]

sendViaWA :: WhatsAppService -> Text -> Text -> IO Value
sendViaWA WhatsAppService{waManager, waConfig} to url = do
  let msg = "¡Gracias por tu interés en el Curso de Producción Musical! Aquí está tu enlace de inscripción: "
            <> url <> "\nCupos limitados (10)."
  _ <- sendText waManager (waToken waConfig) (waPhoneId waConfig) to msg
  pure (object ["ok" .= True])

mintLink :: WhatsAppService -> Connection -> Text -> IO (Text, Int)
mintLink WhatsAppService{waConfig} conn phone = do
  mCeId <- lookupCourseIdBySlug conn (courseSlug waConfig)
  ceId <- maybe (fail "course_edition not found") pure mCeId
  (lid, tok) <- ensureLead conn phone ceId
  case courseRegUrl waConfig of
    Just hard -> pure (hard, lid)
    Nothing   -> do
      let base = T.dropWhileEnd (== '/') (appBaseUrl waConfig)
          cleanBase = if T.null base then appBaseUrl waConfig else base
      pure ( cleanBase <> "/inscripcion/" <> courseSlug waConfig
             <> "?lead=" <> T.pack (show lid) <> "&t=" <> tok
           , lid)
