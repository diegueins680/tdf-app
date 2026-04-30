{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.WhatsApp.Transport
  ( WhatsAppEnv(..)
  , loadWhatsAppEnv
  , sendWhatsAppTextIO
  ) where

import           Data.Maybe (fromMaybe, isNothing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Environment (lookupEnv)

import           TDF.WhatsApp.Client
  ( SendTextResult
  , normalizeGraphApiVersion
  , normalizeWhatsAppAccessToken
  , normalizeWhatsAppMessageBody
  , normalizeWhatsAppPhoneNumberId
  , normalizeWhatsAppRecipientPhone
  , normalizeWhatsAppVerifyToken
  , sendText
  )

data WhatsAppEnv = WhatsAppEnv
  { waManager       :: Manager
  , waToken         :: Maybe Text
  , waPhoneId       :: Maybe Text
  , waVerifyToken   :: Maybe Text
  , waContactNumber :: Maybe Text
  , waApiVersion    :: Maybe Text
  }

loadWhatsAppEnv :: IO WhatsAppEnv
loadWhatsAppEnv = do
  manager <- newManager tlsManagerSettings
  token <-
    validateOptionalEnvText normalizeWhatsAppAccessToken
      =<< firstNonEmptyText ["WHATSAPP_TOKEN", "WA_TOKEN"]
  phoneId <-
    validateOptionalEnvText normalizeWhatsAppPhoneNumberId
      =<< firstNonEmptyText ["WHATSAPP_PHONE_NUMBER_ID", "WA_PHONE_ID"]
  verify <-
    validateOptionalEnvText normalizeWhatsAppVerifyToken
      =<< firstNonEmptyText ["WHATSAPP_VERIFY_TOKEN", "WA_VERIFY_TOKEN"]
  contact <- firstNonEmptyText ["COURSE_WHATSAPP_NUMBER", "WHATSAPP_CONTACT_NUMBER", "WA_CONTACT_NUMBER"]
  apiVersion <-
    validateOptionalEnvText normalizeGraphApiVersion
      =<< firstNonEmptyText ["WHATSAPP_API_VERSION", "WA_GRAPH_API_VERSION", "WA_API_VERSION"]
  pure WhatsAppEnv
    { waManager = manager
    , waToken = token
    , waPhoneId = phoneId
    , waVerifyToken = verify
    , waContactNumber = contact
    , waApiVersion = apiVersion
    }

validateOptionalEnvText :: (Text -> Either String Text) -> Maybe Text -> IO (Maybe Text)
validateOptionalEnvText _ Nothing = pure Nothing
validateOptionalEnvText normalizeValue (Just rawValue) =
  either fail (pure . Just) (normalizeValue rawValue)

sendWhatsAppTextIO :: WhatsAppEnv -> Text -> Text -> IO (Either Text SendTextResult)
sendWhatsAppTextIO env@WhatsAppEnv{waManager, waToken, waPhoneId, waApiVersion} phone msg =
  case normalizeWhatsAppRecipientPhone phone of
    Left err -> pure (Left (T.pack err))
    Right recipientPhone ->
      case normalizeWhatsAppMessageBody msg of
        Left err -> pure (Left (T.pack err))
        Right messageBody ->
          case (waToken, waPhoneId) of
            (Just tok, Just pid) -> do
              let version = fromMaybe "v20.0" waApiVersion
              result <- sendText waManager version tok pid recipientPhone messageBody
              pure (either (Left . T.pack) Right result)
            _ ->
              pure (Left (missingConfigMessage env))

missingConfigMessage :: WhatsAppEnv -> Text
missingConfigMessage WhatsAppEnv{waToken, waPhoneId} =
  let missingPieces =
        [ ("token",   ["WHATSAPP_TOKEN", "WA_TOKEN"])              | isNothing waToken   ] ++
        [ ("phoneId", ["WHATSAPP_PHONE_NUMBER_ID", "WA_PHONE_ID"]) | isNothing waPhoneId ]
      pieceToText (name, envs) =
        T.concat
          [ name
          , " (expected env vars: "
          , T.intercalate ", " (map T.pack envs)
          , ")"
          ]
      details =
        if null missingPieces
          then "unknown WhatsApp configuration problem"
          else T.intercalate "; " (map pieceToText missingPieces)
  in T.concat ["WhatsApp configuration not available: ", details]

firstNonEmptyText :: [String] -> IO (Maybe Text)
firstNonEmptyText names = go names
  where
    go [] = pure Nothing
    go (name:rest) = do
      val <- lookupEnv name
      case fmap (T.strip . T.pack) val of
        Just txt | not (T.null txt) -> pure (Just txt)
        _ -> go rest
