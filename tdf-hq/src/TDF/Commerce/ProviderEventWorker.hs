{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Commerce.ProviderEventWorker
  ( ProviderEventWorkerStats(..)
  , providerEventWorkerTick
  , startProviderEventWorker
  , validateStoredPaypalEvent
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception.Safe (displayException, tryAny)
import           Control.Monad (foldM, forever, void)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import           Database.Persist.Sql
  ( ConnectionPool, Single(..), SqlPersistT, rawSql, runSqlPool )
import           System.Environment (lookupEnv)
import           System.IO (hPutStrLn, stderr)

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.ProviderEventStore as ProviderEvent
import           TDF.DB (Env(..))
import           TDF.Server.ServiceStorefront
  ( PaypalEventProcessResult(..)
  , PaypalWebhookEnvelope(..)
  , parsePaypalWebhookEnvelope
  , paypalWebhookResourceId
  , processPaypalWebhookEventIO
  )

data ProviderEventWorkerStats = ProviderEventWorkerStats
  { pewClaimed      :: Int
  , pewProcessed    :: Int
  , pewIgnored      :: Int
  , pewRetried      :: Int
  , pewDeadLettered :: Int
  } deriving (Eq, Show)

emptyStats :: ProviderEventWorkerStats
emptyStats = ProviderEventWorkerStats 0 0 0 0 0

startProviderEventWorker :: Env -> IO ()
startProviderEventWorker env = do
  rawKey <- lookupEnv "COMMERCE_EVENT_ENCRYPTION_KEY"
  case validateEncryptionKey (T.pack <$> rawKey) of
    Left message ->
      hPutStrLn stderr
        ("{\"component\":\"provider-event-worker\",\"level\":\"warning\",\"message\":\""
          <> redactLogValue (T.unpack message) <> "\"}")
    Right encryptionKey -> void (forkIO (workerLoop env encryptionKey))

workerLoop :: Env -> Text -> IO ()
workerLoop env encryptionKey = forever $ do
  result <- tryAny (providerEventWorkerTick env encryptionKey)
  case result of
    Left err ->
      hPutStrLn stderr
        ("{\"component\":\"provider-event-worker\",\"level\":\"error\",\"message\":\"tick failed\",\"error\":\""
          <> redactLogValue (displayException err) <> "\"}")
    Right stats
      | stats /= emptyStats ->
          putStrLn
            ("{\"component\":\"provider-event-worker\",\"level\":\"info\",\"claimed\":"
              <> show (pewClaimed stats)
              <> ",\"processed\":" <> show (pewProcessed stats)
              <> ",\"ignored\":" <> show (pewIgnored stats)
              <> ",\"retried\":" <> show (pewRetried stats)
              <> ",\"deadLettered\":" <> show (pewDeadLettered stats) <> "}")
      | otherwise -> pure ()
  threadDelay (5 * 1000000)

providerEventWorkerTick :: Env -> Text -> IO ProviderEventWorkerStats
providerEventWorkerTick env@Env{envPool} encryptionKey = do
  installed <- runSqlPool providerEventInboxInstalled envPool
  if not installed
    then pure emptyStats
    else do
      now <- getCurrentTime
      references <- runSqlPool
        (ProviderEvent.listDueProviderEventReferences now 50) envPool
      foldM (processReference env encryptionKey) emptyStats references

processReference
  :: Env
  -> Text
  -> ProviderEventWorkerStats
  -> ProviderEvent.ProviderEventReference
  -> IO ProviderEventWorkerStats
processReference env@Env{envPool} encryptionKey stats eventRef = do
  now <- getCurrentTime
  claim <- runSqlPool (ProviderEvent.claimProviderEvent eventRef now) envPool
  case claim of
    ProviderEvent.ProviderEventAlreadyHandled _ -> pure stats
    ProviderEvent.ProviderEventBusy -> pure stats
    ProviderEvent.ProviderEventClaimed attemptCount -> do
      let claimedStats = stats { pewClaimed = pewClaimed stats + 1 }
      loaded <- tryAny $ runSqlPool
        (ProviderEvent.loadProviderEventPayload eventRef encryptionKey) envPool
      case loaded of
        Left _ -> markRetry envPool eventRef attemptCount
          "Provider event payload decryption failed" now claimedStats
        Right (Left summary) -> do
          runSqlPool
            (ProviderEvent.markProviderEventDeadLetter
              eventRef Nothing Nothing Nothing summary now) envPool
          pure claimedStats
            { pewDeadLettered = pewDeadLettered claimedStats + 1 }
        Right (Right payload) ->
          case validateStoredPaypalEvent payload of
            Left summary -> do
              runSqlPool
                (ProviderEvent.markProviderEventDeadLetter
                  eventRef Nothing Nothing Nothing summary now) envPool
              pure claimedStats
                { pewDeadLettered = pewDeadLettered claimedStats + 1 }
            Right (environment, envelope) -> do
              processed <- tryAny $ processPaypalWebhookEventIO
                env environment (ProviderEvent.pepMerchantRef payload) envelope now
              case processed of
                Left _ -> markRetry envPool eventRef attemptCount
                  "Provider event processing failed" now claimedStats
                Right outcome ->
                  applyOutcome envPool eventRef attemptCount now claimedStats outcome

applyOutcome
  :: ConnectionPool
  -> ProviderEvent.ProviderEventReference
  -> Int
  -> UTCTime
  -> ProviderEventWorkerStats
  -> PaypalEventProcessResult
  -> IO ProviderEventWorkerStats
applyOutcome envPool eventRef attemptCount now stats outcome =
  case outcome of
    PaypalEventProcessed checkoutId attemptId refundId -> do
      runSqlPool
        (ProviderEvent.markProviderEventProcessed
          eventRef checkoutId attemptId refundId now) envPool
      pure stats { pewProcessed = pewProcessed stats + 1 }
    PaypalEventIgnored -> do
      runSqlPool
        (ProviderEvent.markProviderEventIgnored
          eventRef Nothing Nothing Nothing now) envPool
      pure stats { pewIgnored = pewIgnored stats + 1 }
    PaypalEventPermanentFailure summary checkoutId attemptId refundId -> do
      runSqlPool
        (ProviderEvent.markProviderEventDeadLetter
          eventRef checkoutId attemptId refundId summary now) envPool
      pure stats { pewDeadLettered = pewDeadLettered stats + 1 }
    PaypalEventRetry summary ->
      markRetry envPool eventRef attemptCount summary now stats

markRetry
  :: ConnectionPool
  -> ProviderEvent.ProviderEventReference
  -> Int
  -> Text
  -> UTCTime
  -> ProviderEventWorkerStats
  -> IO ProviderEventWorkerStats
markRetry envPool eventRef attemptCount summary now stats = do
  exhausted <- runSqlPool
    (ProviderEvent.markProviderEventRetry eventRef attemptCount summary now) envPool
  pure $ if exhausted
    then stats { pewDeadLettered = pewDeadLettered stats + 1 }
    else stats { pewRetried = pewRetried stats + 1 }

validateStoredPaypalEvent
  :: ProviderEvent.ProviderEventPayload
  -> Either Text (Checkout.CheckoutEnvironment, PaypalWebhookEnvelope)
validateStoredPaypalEvent ProviderEvent.ProviderEventPayload{..} = do
  if pepProvider == "paypal"
    then pure ()
    else Left "Unsupported provider event was routed to the PayPal worker"
  environment <- case pepEnvironment of
    "sandbox" -> Right Checkout.CheckoutSandbox
    "production" -> Right Checkout.CheckoutProduction
    _ -> Left "Stored provider event environment is invalid"
  envelope <- parsePaypalWebhookEnvelope (BL.fromStrict pepRawPayload)
  if pweEventId envelope == pepProviderEventId
      && pweEventType envelope == pepEventType
      && Just (pweCreatedAt envelope) == pepProviderCreatedAt
      && paypalWebhookResourceId envelope == pepProviderResourceId
    then Right (environment, envelope)
    else Left "Stored provider event metadata does not match its immutable payload"

providerEventInboxInstalled :: SqlPersistT IO Bool
providerEventInboxInstalled = do
  rows <- rawSql
    "SELECT to_regclass('commerce_provider_event_inbox') IS NOT NULL" []
      :: SqlPersistT IO [Single Bool]
  pure (rows == [Single True])

validateEncryptionKey :: Maybe Text -> Either Text Text
validateEncryptionKey mRawKey = do
  key <- maybe (Left "worker disabled: COMMERCE_EVENT_ENCRYPTION_KEY is not configured")
    (Right . T.strip) mRawKey
  if T.length key >= 32 && T.length key <= 256
      && T.all (\character -> character >= '!' && character <= '~') key
    then Right key
    else Left "worker disabled: COMMERCE_EVENT_ENCRYPTION_KEY is invalid"

redactLogValue :: String -> String
redactLogValue = take 500 . map replaceUnsafe
  where
    replaceUnsafe character
      | character `elem` ['\n', '\r', '\t', '"'] = ' '
      | otherwise = character
