{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module TDF.WhatsApp.Service
  ( WhatsAppService(..)
  , WhatsAppConfig(..)
  , loadWhatsAppConfig
  , mkWhatsAppService
  , enrollPhone
  , previewEnrollment
  , verifyTokenMatches
  , requireWhatsAppSendSuccess
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Database.PostgreSQL.Simple (Connection, execute, Only(..))

import qualified TDF.Config as Config
import TDF.Leads.Model (ensureLead, lookupCourseIdBySlug)
import TDF.WhatsApp.Client (SendTextResult (..), sendText)

data WhatsAppConfig = WhatsAppConfig
  { waToken       :: Text
  , waPhoneId     :: Text
  , waVerifyToken :: Maybe Text
  , courseSlug    :: Text
  , courseRegUrl  :: Maybe Text
  , appBaseUrl    :: Text
  , waApiVersion  :: Text
  }

data WhatsAppService = WhatsAppService
  { waManager :: Manager
  , waConfig  :: WhatsAppConfig
  }

mkWhatsAppService :: IO WhatsAppService
mkWhatsAppService = do
  mgr <- newManager tlsManagerSettings
  cfg <- loadWhatsAppConfig
  pure $ WhatsAppService mgr cfg

loadWhatsAppConfig :: IO WhatsAppConfig
loadWhatsAppConfig = do
  tok <- fmap (maybe "" T.pack) (lookupEnv "WA_TOKEN")
  pid <- fmap (maybe "" T.pack) (lookupEnv "WA_PHONE_ID")
  ver <- fmap (fmap T.pack) (lookupEnv "WA_VERIFY_TOKEN")
  mSlug <- lookupFirstNonEmptyEnv ["COURSE_EDITION_SLUG", "COURSE_DEFAULT_SLUG"]
  mReg  <- lookupEnv "COURSE_REG_URL"
  mBase <- lookupEnv "HQ_APP_URL"
  mVersion <- lookupFirstNonEmptyEnv ["WA_GRAPH_API_VERSION", "WHATSAPP_API_VERSION"]
  slugVal <- either fail pure (Config.normalizeConfiguredCourseSlug mSlug)
  regUrl <- either fail pure (normalizeWhatsAppRegistrationUrl mReg)
  baseUrl <- either fail pure (normalizeWhatsAppAppBaseUrl mBase)
  version <- either fail pure (normalizeWhatsAppApiVersion mVersion)
  let cfg = WhatsAppConfig
        { waToken       = tok
        , waPhoneId     = pid
        , waVerifyToken = ver
        , courseSlug    = slugVal
        , courseRegUrl  = regUrl
        , appBaseUrl    = baseUrl
        , waApiVersion  = version
        }
  pure cfg

lookupFirstNonEmptyEnv :: [String] -> IO (Maybe String)
lookupFirstNonEmptyEnv [] = pure Nothing
lookupFirstNonEmptyEnv (key:rest) = do
  value <- lookupEnv key
  case value >>= nonEmptyString of
    Just normalized -> pure (Just normalized)
    Nothing -> lookupFirstNonEmptyEnv rest

nonEmptyString :: String -> Maybe String
nonEmptyString raw =
  let trimmed = T.unpack (T.strip (T.pack raw))
  in if null trimmed then Nothing else Just trimmed

normalizeWhatsAppRegistrationUrl :: Maybe String -> Either String (Maybe Text)
normalizeWhatsAppRegistrationUrl Nothing = Right Nothing
normalizeWhatsAppRegistrationUrl (Just rawUrl) =
  Config.normalizeConfiguredBaseUrl "COURSE_REG_URL" rawUrl

normalizeWhatsAppAppBaseUrl :: Maybe String -> Either String Text
normalizeWhatsAppAppBaseUrl Nothing = Right "http://localhost:5173"
normalizeWhatsAppAppBaseUrl (Just rawUrl) =
  case Config.normalizeConfiguredBaseUrl "HQ_APP_URL" rawUrl of
    Left msg -> Left msg
    Right Nothing -> Right "http://localhost:5173"
    Right (Just urlVal) -> Right urlVal

normalizeWhatsAppApiVersion :: Maybe String -> Either String Text
normalizeWhatsAppApiVersion Nothing = Right "v20.0"
normalizeWhatsAppApiVersion (Just rawVersion)
  | T.null version = Right "v20.0"
  | isValidVersion version = Right version
  | otherwise =
      Left "WhatsApp API version must look like v20.0"
  where
    version = T.toLower (T.strip (T.pack rawVersion))
    isValidVersion value =
      case T.uncons value of
        Just ('v', rest) ->
          case T.splitOn "." rest of
            [major] -> allAsciiDigits major
            [major, minor] -> allAsciiDigits major && allAsciiDigits minor
            _ -> False
        _ -> False
    allAsciiDigits value =
      not (T.null value) && T.all (\ch -> ch >= '0' && ch <= '9') value

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
      version = waApiVersion waConfig
  result <- sendText waManager version (waToken waConfig) (waPhoneId waConfig) to msg
  either fail pure (requireWhatsAppSendSuccess result)

requireWhatsAppSendSuccess :: Either String SendTextResult -> Either String Value
requireWhatsAppSendSuccess (Left err) =
  Left ("WhatsApp send failed: " <> err)
requireWhatsAppSendSuccess (Right SendTextResult{sendTextMessageId = Nothing}) =
  Left "WhatsApp send failed: provider response did not include a message id"
requireWhatsAppSendSuccess (Right SendTextResult{sendTextMessageId = Just rawId})
  | T.null (T.strip rawId) =
      Left "WhatsApp send failed: provider response did not include a message id"
  | otherwise =
      Right (object ["ok" .= True])

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
