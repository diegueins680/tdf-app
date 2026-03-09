{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.WhatsApp.Transport
  ( WhatsAppEnv(..)
  , loadWhatsAppEnv
  , sendWhatsAppTextIO
  ) where

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Environment (lookupEnv)

import           TDF.WhatsApp.Client (SendTextResult, sendText)

data WhatsAppEnv = WhatsAppEnv
  { waToken         :: Maybe Text
  , waPhoneId       :: Maybe Text
  , waVerifyToken   :: Maybe Text
  , waContactNumber :: Maybe Text
  , waApiVersion    :: Maybe Text
  } deriving (Show)

loadWhatsAppEnv :: IO WhatsAppEnv
loadWhatsAppEnv = do
  token <- firstNonEmptyText ["WHATSAPP_TOKEN", "WA_TOKEN"]
  phoneId <- firstNonEmptyText ["WHATSAPP_PHONE_NUMBER_ID", "WA_PHONE_ID"]
  verify <- firstNonEmptyText ["WHATSAPP_VERIFY_TOKEN", "WA_VERIFY_TOKEN"]
  contact <- firstNonEmptyText ["COURSE_WHATSAPP_NUMBER", "WHATSAPP_CONTACT_NUMBER", "WA_CONTACT_NUMBER"]
  apiVersion <- firstNonEmptyText ["WHATSAPP_API_VERSION", "WA_GRAPH_API_VERSION", "WA_API_VERSION"]
  pure WhatsAppEnv
    { waToken = token
    , waPhoneId = phoneId
    , waVerifyToken = verify
    , waContactNumber = contact
    , waApiVersion = apiVersion
    }

sendWhatsAppTextIO :: WhatsAppEnv -> Text -> Text -> IO (Either Text SendTextResult)
sendWhatsAppTextIO WhatsAppEnv{waToken = Just tok, waPhoneId = Just pid, waApiVersion = mVersion} phone msg = do
  manager <- newManager tlsManagerSettings
  let version = fromMaybe "v20.0" mVersion
  result <- sendText manager version tok pid phone msg
  pure (either (Left . T.pack) Right result)
sendWhatsAppTextIO _ _ _ = pure (Left "WhatsApp config not available")

firstNonEmptyText :: [String] -> IO (Maybe Text)
firstNonEmptyText names = go names
  where
    go [] = pure Nothing
    go (name:rest) = do
      val <- lookupEnv name
      case fmap (T.strip . T.pack) val of
        Just txt | not (T.null txt) -> pure (Just txt)
        _ -> go rest
