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
import           Network.HTTP.Client (Manager)
import           TDF.DB (sharedTlsManager)
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
  manager <- pure sharedTlsManager
  token <-
    validateOptionalEnvText normalizeWhatsAppAccessToken
      =<< firstNonEmptyAliasText "WhatsApp access token" ["WHATSAPP_TOKEN", "WA_TOKEN"]
  phoneId <-
    validateOptionalEnvText normalizeWhatsAppPhoneNumberId
      =<< firstNonEmptyAliasText
        "WhatsApp phone number id"
        ["WHATSAPP_PHONE_NUMBER_ID", "WA_PHONE_ID"]
  verify <-
    validateOptionalEnvText normalizeWhatsAppVerifyToken
      =<< firstNonEmptyAliasText
        "WhatsApp verify token"
        ["WHATSAPP_VERIFY_TOKEN", "WA_VERIFY_TOKEN"]
  contact <-
    firstNormalizedAliasText
      "WhatsApp contact number"
      normalizeWhatsAppContactNumber
      ["COURSE_WHATSAPP_NUMBER", "WHATSAPP_CONTACT_NUMBER", "WA_CONTACT_NUMBER"]
  apiVersion <-
    validateOptionalEnvText normalizeGraphApiVersion
      =<< firstNonEmptyAliasText
        "WhatsApp API version"
        ["WHATSAPP_API_VERSION", "WA_GRAPH_API_VERSION", "WA_API_VERSION"]
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

normalizeWhatsAppContactNumber :: Text -> Either String Text
normalizeWhatsAppContactNumber rawPhone =
  case normalizeWhatsAppRecipientPhone rawPhone of
    Left _ -> Left contactNumberMessage
    Right phone ->
      let digitsOnly = T.filter (\ch -> ch >= '0' && ch <= '9') phone
      in case T.uncons digitsOnly of
           Just (firstDigit, _)
             | firstDigit /= '0' -> Right phone
           _ -> Left contactNumberMessage
  where
    contactNumberMessage =
      "Invalid WhatsApp contact number: expected international 8-15 digits with country code"

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

firstNonEmptyAliasText :: String -> [String] -> IO (Maybe Text)
firstNonEmptyAliasText label names = do
  values <- traverse lookupAlias names
  case [entry | Just entry <- values] of
    [] -> pure Nothing
    (firstName, firstValue):rest ->
      case filter ((/= firstValue) . snd) rest of
        [] -> pure (Just firstValue)
        (conflictName, _):_ ->
          fail $
            label <> " aliases conflict: "
              <> firstName <> " and " <> conflictName
              <> " are both set with different values"
  where
    lookupAlias name = do
      val <- lookupEnv name
      pure $ do
        txt <- T.strip . T.pack <$> val
        if T.null txt
          then Nothing
          else Just (name, txt)

firstNormalizedAliasText :: String -> (Text -> Either String Text) -> [String] -> IO (Maybe Text)
firstNormalizedAliasText label normalizeValue names = do
  values <- traverse lookupAlias names
  case [entry | Just entry <- values] of
    [] -> pure Nothing
    (firstName, firstValue):rest ->
      case filter ((/= firstValue) . snd) rest of
        [] -> pure (Just firstValue)
        (conflictName, _):_ ->
          fail $
            label <> " aliases conflict: "
              <> firstName <> " and " <> conflictName
              <> " are both set with different values"
  where
    lookupAlias name = do
      val <- lookupEnv name
      case fmap (T.strip . T.pack) val of
        Just txt | not (T.null txt) ->
          case normalizeValue txt of
            Left err -> fail err
            Right normalized -> pure (Just (name, normalized))
        _ -> pure Nothing
