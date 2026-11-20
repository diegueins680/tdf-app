{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | MSG-DELIVERY-001: email and device push reminders for chat notices.
--
-- The database writes one delivery intent per notice and channel in the same
-- transaction as the message (see sql/2026-11-20_chat_external_delivery.sql).
-- This worker leases due intents, rechecks read state, consent, block, closure,
-- opt-out, freshness and coalescing at dispatch, and completes each lease with a
-- fenced update. Channels stay off unless explicitly enabled by environment.
-- Provider acceptance (SMTP or Expo ticket) is recorded; it is not a claim that
-- the message reached the inbox or the device.
module TDF.Chat.Delivery
  ( notificationDeliveryServer
  , startChatDeliveryWorker
    -- * Pure logic (exported for tests)
  , DeliverySettings(..)
  , deliverySettingsFromEnv
  , enabledChannels
  , retryDelaySeconds
  , validExpoPushToken
  , ExpoTicket(..)
  , ExpoOutcome(..)
  , parseExpoTickets
  , classifyExpoTickets
  , expoPushMessages
  , chatEmailContent
  , chatThreadUrl
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (Value(..), object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, toLower)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Foldable (toList)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Network.HTTP.Client
  ( Manager, RequestBody(..), httpLbs, method, parseRequest, requestBody, requestHeaders
  , responseBody, responseStatus, responseTimeout, responseTimeoutMicro )
import Network.HTTP.Types.Status (statusCode)
import Servant
import System.Environment (lookupEnv)

import qualified TDF.API.NotificationDelivery as API
import TDF.Auth (AuthedUser(..))
import TDF.Config (AppConfig(..))
import TDF.DB (ConnectionPool, Env(..), sharedTlsManager)
import qualified TDF.Email as Email
import qualified TDF.LogBuffer as Log

type AppM = ReaderT Env Handler

-- Settings ------------------------------------------------------------------

data DeliverySettings = DeliverySettings
  { dsEmailEnabled :: Bool
  , dsPushEnabled :: Bool
  , dsPushTokenKey :: Maybe Text
  , dsExpoAccessToken :: Maybe Text
  , dsExpoPushUrl :: String
  } deriving (Eq, Show)

-- | Pure interpretation of the environment, so defaults are testable: every
-- channel is off and push has no encryption key unless configured.
deliverySettingsFromEnv :: (String -> Maybe String) -> DeliverySettings
deliverySettingsFromEnv env = DeliverySettings
  { dsEmailEnabled = flag "CHAT_EMAIL_DELIVERY_ENABLED"
  , dsPushEnabled = flag "CHAT_PUSH_DELIVERY_ENABLED" && isJust key
  , dsPushTokenKey = key
  , dsExpoAccessToken = nonEmpty "EXPO_ACCESS_TOKEN"
  , dsExpoPushUrl = maybe "https://exp.host/--/api/v2/push/send" T.unpack (nonEmpty "EXPO_PUSH_URL")
  }
  where
    flag name = maybe False ((`elem` ["1", "true", "yes", "on"]) . map toLower) (env name)
    nonEmpty name = case T.strip . T.pack <$> env name of
      Just value | not (T.null value) -> Just value
      _ -> Nothing
    -- pgp_sym_encrypt key for stored device tokens; short keys are rejected.
    key = case nonEmpty "NOTIFICATION_PUSH_TOKEN_KEY" of
      Just value | T.length value >= 32 -> Just value
      _ -> Nothing

readDeliverySettings :: IO DeliverySettings
readDeliverySettings = do
  let names =
        [ "CHAT_EMAIL_DELIVERY_ENABLED", "CHAT_PUSH_DELIVERY_ENABLED"
        , "NOTIFICATION_PUSH_TOKEN_KEY", "EXPO_ACCESS_TOKEN", "EXPO_PUSH_URL" ]
  values <- mapM (\name -> (,) name <$> lookupEnv name) names
  pure (deliverySettingsFromEnv (\name -> lookup name values >>= id))

enabledChannels :: DeliverySettings -> [Text]
enabledChannels settings =
  ["email" | dsEmailEnabled settings] <> ["push" | dsPushEnabled settings]

-- | Backoff after the given (1-based) attempt: 1, 2, 4, 8 minutes, capped at one hour.
retryDelaySeconds :: Int -> Int
retryDelaySeconds attempt = min 3600 (60 * 2 ^ max 0 (min 6 (attempt - 1)))

-- HTTP handlers --------------------------------------------------------------

notificationDeliveryServer :: AuthedUser -> ServerT API.NotificationDeliveryAPI AppM
notificationDeliveryServer user =
       registerPush user
  :<|> revokePush user
  :<|> getChannels user
  :<|> putChannels user
  :<|> markThreadRead user

runDB :: SqlPersistT IO a -> AppM a
runDB action = asks envPool >>= liftIO . runSqlPool action

actor :: AuthedUser -> PersistValue
actor = PersistInt64 . fromSqlKey . auPartyId

-- | Expo tokens look like @ExponentPushToken[...]@ (or the newer @ExpoPushToken[...]@).
validExpoPushToken :: Text -> Bool
validExpoPushToken raw =
  T.length raw <= 255 && any matches ["ExponentPushToken[", "ExpoPushToken["]
  where
    matches prefix = case T.stripPrefix prefix raw >>= T.stripSuffix "]" of
      Just inner -> not (T.null inner) && T.all (\c -> isAlphaNum c || c `elem` ("-_" :: String)) inner
      Nothing -> False

registerPush :: AuthedUser -> API.PushSubscriptionRegister -> AppM NoContent
registerPush user request = do
  let tokenValue = T.strip (API.token request)
      platformValue = T.toLower (T.strip (API.platform request))
  unless (T.toLower (T.strip (API.provider request)) == "expo") $
    throwError err422 {errBody = "Unsupported push provider"}
  unless (platformValue `elem` ["ios", "android"]) $
    throwError err422 {errBody = "Unsupported push platform"}
  unless (validExpoPushToken tokenValue) $
    throwError err422 {errBody = "Invalid push token"}
  settings <- liftIO readDeliverySettings
  key <- maybe (throwError err503 {errBody = "Push notifications are not configured"}) pure
    (dsPushTokenKey settings)
  runDB $ rawExecute
    "INSERT INTO notification_push_subscription(party_id,provider,platform,token_digest,encrypted_token) \
    \VALUES (?, 'expo', ?, encode(digest(?::text,'sha256'),'hex'), pgp_sym_encrypt(?::text, ?::text)) \
    \ON CONFLICT (provider, token_digest) DO UPDATE SET party_id=EXCLUDED.party_id, \
    \platform=EXCLUDED.platform, encrypted_token=EXCLUDED.encrypted_token, active=true, \
    \last_failure_code=NULL, updated_at=now()"
    [actor user, PersistText platformValue, PersistText tokenValue, PersistText tokenValue, PersistText key]
  pure NoContent

revokePush :: AuthedUser -> API.PushSubscriptionRevoke -> AppM NoContent
revokePush user request = do
  let tokenValue = T.strip (API.revokeToken request)
  unless (validExpoPushToken tokenValue) $
    throwError err422 {errBody = "Invalid push token"}
  runDB $ rawExecute
    "UPDATE notification_push_subscription SET active=false, updated_at=now() \
    \WHERE provider='expo' AND token_digest=encode(digest(?::text,'sha256'),'hex') AND party_id=?"
    [PersistText tokenValue, actor user]
  pure NoContent

getChannels :: AuthedUser -> AppM API.NotificationChannelsDTO
getChannels user = runDB (loadChannels user)

loadChannels :: AuthedUser -> SqlPersistT IO API.NotificationChannelsDTO
loadChannels user = do
  rows <- rawSql
    "SELECT channel, enabled FROM notification_channel_preference WHERE party_id=? AND category='chat'"
    [actor user] :: SqlPersistT IO [(Single Text, Single Bool)]
  let enabled channel = fromMaybe True (lookup channel [(c, e) | (Single c, Single e) <- rows])
  pure API.NotificationChannelsDTO {API.email = enabled "email", API.push = enabled "push"}

putChannels :: AuthedUser -> API.NotificationChannelsDTO -> AppM API.NotificationChannelsDTO
putChannels user request = runDB $ do
  forM_ [("email", API.email request), ("push", API.push request)] $ \(channel, enabled) ->
    rawExecute
      "INSERT INTO notification_channel_preference(party_id,category,channel,enabled,updated_at) \
      \VALUES (?, 'chat', ?, ?, now()) ON CONFLICT (party_id,category,channel) \
      \DO UPDATE SET enabled=EXCLUDED.enabled, updated_at=now()"
      [actor user, PersistText channel, PersistBool enabled]
  loadChannels user

markThreadRead :: AuthedUser -> Int64 -> API.ChatThreadReadRequest -> AppM NoContent
markThreadRead user threadId request = do
  when (threadId <= 0) $ throwError err400 {errBody = "threadId must be a positive integer"}
  when (API.lastMessageId request <= 0) $ throwError err400 {errBody = "lastMessageId must be a positive integer"}
  rows <- runDB $ rawSql "SELECT chat_mark_thread_read(?, ?, ?)"
    [actor user, PersistInt64 threadId, PersistInt64 (API.lastMessageId request)]
  case rows of
    [Single True] -> pure NoContent
    _ -> throwError err404

-- Content ------------------------------------------------------------------------

chatThreadUrl :: Maybe Text -> Int64 -> Maybe Text
chatThreadUrl mBase threadId = do
  base <- T.dropWhileEnd (== '/') . T.strip <$> mBase
  if T.null base then Nothing else Just (base <> "/chat?threadId=" <> T.pack (show threadId))

-- | Subject, preheader and body. Like the in-app notice, it carries no message
-- text, sender name or sender address.
chatEmailContent :: (Text, Text, [Text])
chatEmailContent =
  ( "Tienes un mensaje nuevo en TDF"
  , "Alguien te escribió por el chat de TDF Records."
  , [ "Tienes un mensaje nuevo en el chat de TDF Records."
    , "Abre la conversación para leerlo y responder."
    , "Puedes desactivar estos avisos en el chat de TDF, en el botón «Avisos»."
    ]
  )

-- | One Expo message per device token. Metadata is only the notice id, which the
-- app resolves through the authenticated notification screen.
expoPushMessages :: Int64 -> [Text] -> Value
expoPushMessages notificationId tokens = toJSON
  [ object
      [ "to" .= tokenValue
      , "title" .= ("Nuevo mensaje" :: Text)
      , "body" .= ("Tienes un mensaje nuevo en TDF." :: Text)
      , "sound" .= ("default" :: Text)
      , "priority" .= ("high" :: Text)
      , "data" .= object ["notificationId" .= notificationId]
      ]
  | tokenValue <- tokens ]

-- Expo push tickets --------------------------------------------------------------

data ExpoTicket = TicketOk | TicketDeviceNotRegistered | TicketError Text
  deriving (Eq, Show)

data ExpoOutcome
  = ExpoAccepted
  | ExpoNoDevice
  | ExpoRetry Text
  deriving (Eq, Show)

parseExpoTickets :: BL.ByteString -> Maybe [ExpoTicket]
parseExpoTickets raw = do
  Object root <- Aeson.decode raw
  Array tickets <- KeyMap.lookup "data" root
  traverse ticket (toList tickets)
  where
    ticket (Object item) = case KeyMap.lookup "status" item of
      Just (String "ok") -> Just TicketOk
      Just (String "error") ->
        let detail = case KeyMap.lookup "details" item of
              Just (Object details) -> case KeyMap.lookup "error" details of
                Just (String code) -> code
                _ -> "unknown"
              _ -> "unknown"
        in Just (if detail == "DeviceNotRegistered" then TicketDeviceNotRegistered else TicketError detail)
      _ -> Nothing
    ticket _ = Nothing

classifyExpoTickets :: [ExpoTicket] -> ExpoOutcome
classifyExpoTickets tickets
  | TicketOk `elem` tickets = ExpoAccepted
  | not (null tickets) && all (== TicketDeviceNotRegistered) tickets = ExpoNoDevice
  | otherwise = ExpoRetry "provider_error"

-- Worker ----------------------------------------------------------------------

startChatDeliveryWorker :: Env -> IO ()
startChatDeliveryWorker env = do
  settings <- readDeliverySettings
  let channels = enabledChannels settings
  if null channels
    then Log.addLog Log.LogInfo "[ChatDelivery] email and push reminders are disabled"
    else do
      Log.addLog Log.LogInfo ("[ChatDelivery] enabled channels: " <> T.intercalate "," channels)
      void . forkIO . forever $ do
        outcome <- try (runDeliveryBatch env settings channels)
        case outcome of
          Right _ -> pure ()
          Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
            Just _ -> throwIO err
            Nothing -> Log.addLog Log.LogError ("[ChatDelivery] batch failed: " <> T.pack (displayException err))
        threadDelay 15000000

deliveryAvailable :: ConnectionPool -> IO Bool
deliveryAvailable pool = do
  rows <- runSqlPool
    (rawSql "SELECT to_regprocedure('chat_delivery_lease(text[],integer,integer)') IS NOT NULL" [])
    pool :: IO [Single Bool]
  pure (rows == [Single True])

runDeliveryBatch :: Env -> DeliverySettings -> [Text] -> IO Int
runDeliveryBatch env settings channels = do
  let pool = envPool env
  ready <- deliveryAvailable pool
  if not ready then pure 0 else do
    leased <- runSqlPool
      (rawSql
        "SELECT outbox_id, outbox_channel, outbox_lease::text, outbox_attempts FROM chat_delivery_lease(?::text[], 20, 120)"
        [PersistText ("{" <> T.intercalate "," channels <> "}")])
      pool :: IO [(Single Int64, Single Text, Single Text, Single Int)]
    forM_ leased $ \(Single outboxId, Single channel, Single lease, Single attempts) ->
      deliverOne env settings outboxId channel lease attempts
    pure (length leased)

deliverOne :: Env -> DeliverySettings -> Int64 -> Text -> Text -> Int -> IO ()
deliverOne env settings outboxId channel lease attempts = do
  let pool = envPool env
  decisionRows <- runSqlPool
    (rawSql "SELECT chat_delivery_decision(?, ?::uuid)" [PersistInt64 outboxId, PersistText lease])
    pool :: IO [Single Text]
  case decisionRows of
    [Single "send"] -> do
      result <- try (if channel == "email" then sendEmail env outboxId else sendPush env settings outboxId)
      case result of
        Right (state, reason) -> complete state reason
        Left (err :: SomeException) -> do
          Log.addLog Log.LogError ("[ChatDelivery] " <> channel <> " delivery failed: " <> T.pack (displayException err))
          complete "retry" "provider_error"
    [Single "lease_lost"] -> pure ()
    [Single reason] -> complete "skipped" reason
    _ -> complete "retry" "decision_unavailable"
  where
    complete :: Text -> Text -> IO ()
    complete state reason = void (runSqlPool
      (rawSql "SELECT chat_delivery_complete(?, ?::uuid, ?, ?, ?)"
        [ PersistInt64 outboxId, PersistText lease, PersistText state, PersistText reason
        , PersistInt64 (fromIntegral (retryDelaySeconds attempts)) ])
      (envPool env) :: IO [Single Bool])

-- Returns the completion state and minimized outcome code.
sendEmail :: Env -> Int64 -> IO (Text, Text)
sendEmail env outboxId = case emailConfig (envConfig env) of
  Nothing -> pure ("skipped", "provider_unconfigured")
  Just cfg -> do
    rows <- runSqlPool
      (rawSql
        "SELECT p.display_name, p.primary_email, o.thread_id FROM chat_delivery_outbox o \
        \JOIN party p ON p.id=o.recipient_party_id WHERE o.id=?"
        [PersistInt64 outboxId])
      (envPool env) :: IO [(Single Text, Single (Maybe Text), Single Int64)]
    case rows of
      [(Single name, Single (Just address), Single threadId)] | not (T.null (T.strip address)) -> do
        let (subject, preheader, bodyLines) = chatEmailContent
        Email.sendTransactionalEmail cfg name (T.strip address) subject preheader bodyLines
          (chatThreadUrl (appBaseUrl (envConfig env)) threadId)
        pure ("accepted", "provider_accepted")
      _ -> pure ("skipped", "no_address")

sendPush :: Env -> DeliverySettings -> Int64 -> IO (Text, Text)
sendPush env settings outboxId = case dsPushTokenKey settings of
  Nothing -> pure ("skipped", "provider_unconfigured")
  Just key -> do
    rows <- runSqlPool
      (rawSql
        "SELECT s.id, pgp_sym_decrypt(s.encrypted_token, ?::text), o.notification_id \
        \FROM chat_delivery_outbox o JOIN notification_push_subscription s \
        \ON s.party_id=o.recipient_party_id AND s.active WHERE o.id=? ORDER BY s.id LIMIT 20"
        [PersistText key, PersistInt64 outboxId])
      (envPool env) :: IO [(Single Int64, Single Text, Single Int64)]
    case rows of
      [] -> pure ("skipped", "no_subscription")
      (_, _, Single notificationId) : _ -> do
        let subscriptionIds = [sid | (Single sid, _, _) <- rows]
            tokens = [tokenValue | (_, Single tokenValue, _) <- rows]
        initial <- parseRequest (dsExpoPushUrl settings)
        let request = initial
              { method = "POST"
              , requestHeaders =
                  [ ("Accept", "application/json")
                  , ("Content-Type", "application/json") ]
                  <> maybe [] (\accessToken -> [("Authorization", "Bearer " <> TE.encodeUtf8 accessToken)])
                       (dsExpoAccessToken settings)
              , requestBody = RequestBodyLBS (Aeson.encode (expoPushMessages notificationId tokens))
              , responseTimeout = responseTimeoutMicro 15000000
              }
        response <- httpLbs request sharedTlsManager
        let status = statusCode (responseStatus response)
        if status /= 200
          then pure ("retry", if status == 429 then "provider_rate_limited" else "provider_error")
          else case parseExpoTickets (responseBody response) of
            Just tickets | length tickets == length subscriptionIds -> do
              forM_ (zip subscriptionIds tickets) $ \(sid, ticket) -> case ticket of
                TicketOk -> runSqlPool (rawExecute
                  "UPDATE notification_push_subscription SET last_success_at=now(), last_failure_code=NULL WHERE id=?"
                  [PersistInt64 sid]) (envPool env)
                TicketDeviceNotRegistered -> runSqlPool (rawExecute
                  "UPDATE notification_push_subscription SET active=false, last_failure_code='device_not_registered', updated_at=now() WHERE id=?"
                  [PersistInt64 sid]) (envPool env)
                TicketError code -> runSqlPool (rawExecute
                  "UPDATE notification_push_subscription SET last_failure_code=? WHERE id=?"
                  [PersistText (T.take 64 (sanitizeCode code)), PersistInt64 sid]) (envPool env)
              pure $ case classifyExpoTickets tickets of
                ExpoAccepted -> ("accepted", "provider_accepted")
                ExpoNoDevice -> ("skipped", "no_subscription")
                ExpoRetry reason -> ("retry", reason)
            _ -> pure ("retry", "provider_error")
  where
    sanitizeCode = T.map (\c -> if isAlphaNum c then toLower c else '_')
