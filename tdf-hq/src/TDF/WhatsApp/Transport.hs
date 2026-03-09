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
sendWhatsAppTextIO env@WhatsAppEnv{waToken, waPhoneId, waApiVersion} phone msg =
  case (waToken, waPhoneId) of
    (Just tok, Just pid) -> do
      manager <- newManager tlsManagerSettings
      let version = fromMaybe "v20.0" waApiVersion
      result <- sendText manager version tok pid phone msg
      pure (either (Left . T.pack) Right result)
    _ ->
      pure (Left (missingConfigMessage env))

missingConfigMessage :: WhatsAppEnv -> Text
missingConfigMessage WhatsAppEnv{waToken, waPhoneId} =
  let missingPieces =
        [ ("token", ["WHATSAPP_TOKEN", "WA_TOKEN"])
        | waToken == Nothing ] ++
        [ ("phoneId", ["WHATSAPP_PHONE_NUMBER_ID", "WA_PHONE_ID"])
        | waPhoneId == Nothing ]
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
