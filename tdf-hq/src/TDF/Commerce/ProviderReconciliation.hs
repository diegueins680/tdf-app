{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Authoritative reconciliation for hosted providers. Incoming callbacks
-- only wake this processor; all financial state changes depend on an
-- authenticated provider query matching the immutable local binding.
module TDF.Commerce.ProviderReconciliation
  ( ReconciliationDisposition(..)
  , ProviderReconciliationResult(..)
  , processProviderEventIO
  , processProviderEventWith
  , applyQueryResult
  , startProviderQueryWorker
  , providerQueryWorkerTickWith
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception.Safe (tryAny)
import           Control.Monad (forever, void, when)
import qualified Data.Aeson as A
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import           Database.Persist.Sql
  ( SqlPersistT, rawExecute, runSqlPool )
import           System.Entropy (getEntropy)
import           System.Environment (lookupEnv)
import           System.IO (hPutStrLn, stderr)

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentIntentStore as Intent
import           TDF.Commerce.ProviderAdapter
import           TDF.Commerce.ProviderAdapter.Http
  ( AdapterTransportError(..), executeAdapterRequest, sharedProviderManager )
import qualified TDF.Commerce.ProviderEventStore as ProviderEvent
import qualified TDF.Commerce.ProviderExecutionStore as Execution
import           TDF.Commerce.ProviderCapabilities (ProviderOutcomeCertainty(..))
import           TDF.Commerce.ProviderRuntimeConfig
  ( RuntimeProviderAdapter(..), loadRuntimeProviderAdapter, loadConfiguredCheckoutEnvironment )
import           TDF.Commerce.StateMachine (PaymentEvent(..))
import           TDF.DB (Env(..))

data ReconciliationDisposition
  = ReconciliationProcessed
  | ReconciliationRetry
  | ReconciliationDeadLetter
  deriving (Eq, Show)

data ProviderReconciliationResult = ProviderReconciliationResult
  { prrDisposition :: ReconciliationDisposition
  , prrSummary     :: Text
  , prrCheckoutId  :: Maybe Text
  , prrAttemptId   :: Maybe Text
  } deriving (Eq, Show)

processProviderEventIO
  :: Env
  -> ProviderEvent.ProviderEventPayload
  -> UTCTime
  -> IO ProviderReconciliationResult
processProviderEventIO = processProviderEventWith
  (executeAdapterRequest sharedProviderManager) getCurrentTime

-- | Injectable transport/clock for contract tests. Production always uses the
-- bounded, no-implicit-retry shared transport above. This does not bypass
-- callback trust validation, immutable binding lookup or adapter parsing.
processProviderEventWith
  :: (AdapterRequest -> IO (Either AdapterTransportError A.Value))
  -> IO UTCTime
  -> Env
  -> ProviderEvent.ProviderEventPayload
  -> UTCTime
  -> IO ProviderReconciliationResult
processProviderEventWith fetch appliedAt env@Env{envPool} payload now =
  case validateStoredEventEnvelope payload of
    Left problem -> pure (deadLetter problem Nothing)
    Right (provider, environment, rawValue) -> do
      runtimeResult <- loadRuntimeProviderAdapter environment provider
      case runtimeResult of
        Left _ -> pure (retry "Provider runtime configuration is unavailable" Nothing)
        Right RuntimeProviderAdapter{rpaAdapter} ->
          case adapterAssessNotification rpaAdapter rawValue of
            Left _ -> pure (deadLetter "Stored provider callback is invalid" Nothing)
            Right assessment ->
              case validateAssessment payload provider assessment of
                Left problem -> pure (deadLetter problem Nothing)
                Right () -> do
                  boundResult <- runSqlPool
                    (Execution.loadBoundProviderPayment
                      provider environment (ProviderEvent.pepMerchantRef payload)
                      (notificationExternalId assessment)
                      (notificationMerchantReference assessment))
                    envPool
                  case boundResult of
                    Left _ -> pure (retry
                      "Provider callback has no available immutable binding yet" Nothing)
                    Right payment -> queryAndApply fetch appliedAt env rpaAdapter payload payment now

queryAndApply
  :: (AdapterRequest -> IO (Either AdapterTransportError A.Value))
  -> IO UTCTime
  -> Env
  -> ProviderAdapter
  -> ProviderEvent.ProviderEventPayload
  -> Execution.BoundProviderPayment
  -> UTCTime
  -> IO ProviderReconciliationResult
queryAndApply fetch appliedAt Env{envPool} adapter payload payment now = do
  granted <- runSqlPool (Execution.reserveProviderQueryBudget
    (Execution.bppProvider payment) (Execution.bppEnvironment payment)) envPool
  if not granted
    then pure (retry "Authoritative query budget is temporarily unavailable" (Just payment))
    else do
      queried <- queryProviderWith fetch adapter payment now
      observedAt <- appliedAt
      case queried of
        Left QueryBindingMismatch -> do
          runSqlPool (recordQueryMismatch payment observedAt) envPool
          pure (deadLetter "Authoritative query did not match its immutable binding" (Just payment))
        Left QueryUnsupported -> pure (deadLetter "Stored binding cannot form a safe query" (Just payment))
        Left QueryUnavailable -> pure (retry "Authoritative provider query is temporarily unavailable" (Just payment))
        Right result -> do
          applied <- runSqlPool (applyQueryResult payment result
            (ProviderEvent.providerEventReferenceId (ProviderEvent.pepReference payload)) observedAt) envPool
          pure $ either (\problem -> retry problem (Just payment))
            (\disposition -> resultFor disposition (Just payment)) applied

data QueryFailure = QueryUnsupported | QueryUnavailable | QueryBindingMismatch

queryProviderWith
  :: (AdapterRequest -> IO (Either AdapterTransportError A.Value))
  -> ProviderAdapter -> Execution.BoundProviderPayment -> UTCTime
  -> IO (Either QueryFailure AdapterResult)
queryProviderWith fetch adapter payment now = do
  nonce <- getEntropy 32
  let locator = PaymentLocator
        { plExternalId = Execution.bppProviderResourceId payment
        , plExpected = ExpectedPayment
            { epReference = Execution.bppProviderReference payment
            , epAmountMinor = Execution.bppAmountMinor payment
            , epCurrency = Execution.bppCurrency payment
            }
        }
      context = AdapterContext now nonce
  case adapterBuildQuery adapter context locator of
    Left _ -> pure (Left QueryUnsupported)
    Right request -> do
      response <- fetch request
      pure $ case response of
        Left AdapterTransportError{} -> Left QueryUnavailable
        Right value -> either (const (Left QueryBindingMismatch)) Right
          (adapterParseResponse adapter AdapterQuery locator value)

-- No thread is started by default. The process switch is also reread each
-- tick; the database gate and account qualification remain independently required.
startProviderQueryWorker :: Env -> IO ()
startProviderQueryWorker env = do
  enabled <- queryWorkerEnabled
  when enabled $ void $ forkIO $ forever $ do
    outcome <- tryAny (providerQueryWorkerTickWith
      (executeAdapterRequest sharedProviderManager) env)
    case outcome of
      Left _ -> hPutStrLn stderr
        "{\"component\":\"provider-query-worker\",\"level\":\"error\",\"message\":\"tick failed; lease recovery required\"}"
      Right _ -> pure ()
    threadDelay (5 * 1000000)

queryWorkerEnabled :: IO Bool
queryWorkerEnabled = (== Just "true") <$> lookupEnv "COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED"

-- One job per provider per tick, never a batch of leases aging during HTTP.
-- Tests inject only transport; runtime, flag, account, binding and lease gates
-- are the same as production. Return the number of claimed jobs, not payments.
providerQueryWorkerTickWith
  :: (AdapterRequest -> IO (Either AdapterTransportError A.Value)) -> Env -> IO Int
providerQueryWorkerTickWith fetch env@Env{envPool} = do
  enabled <- queryWorkerEnabled
  configuredEnvironment <- loadConfiguredCheckoutEnvironment
  if not enabled then pure 0 else case configuredEnvironment of
    Left _ -> pure 0
    Right environment -> do
      installed <- runSqlPool Execution.providerQueryRecoveryInstalled envPool
      if not installed then pure 0 else do
        placeToPay <- tickProvider environment Checkout.ProviderPlaceToPay
        payPhone <- tickProvider environment Checkout.ProviderPayPhone
        pure (placeToPay + payPhone)
  where
    tickProvider environment provider = do
      runtime <- loadRuntimeProviderAdapter environment provider
      case runtime of
        Left _ -> pure 0
        Right RuntimeProviderAdapter{rpaAdapter} -> do
          _ <- runSqlPool (Execution.enqueueProviderQueries provider environment) envPool
          claimed <- runSqlPool (Execution.claimProviderQuery provider environment) envPool
          case claimed of
            Nothing -> pure 0
            Just claim -> processQueryClaimWith fetch env rpaAdapter claim >> pure 1

processQueryClaimWith
  :: (AdapterRequest -> IO (Either AdapterTransportError A.Value)) -> Env
  -> ProviderAdapter -> Execution.ProviderQueryClaim -> IO ()
processQueryClaimWith fetch Env{envPool} adapter claim = do
  prepared <- runSqlPool (validateQueryClaim claim True) envPool
  case prepared of
    Nothing -> pure ()
    Just payment -> do
      now <- getCurrentTime
      queried <- queryProviderWith fetch adapter payment now
      observedAt <- getCurrentTime
      processEnabled <- queryWorkerEnabled
      runSqlPool (do
        current <- validateQueryClaim claim False
        case current of
          Nothing -> pure ()
          Just bound
            | not processEnabled -> finish "retry" "process_switch_disabled"
            | bound /= payment -> finish "dead_letter" "binding_changed"
            | otherwise -> case queried of
                Left QueryUnavailable -> finish "retry" "query_unavailable"
                Left QueryUnsupported -> finish "dead_letter" "query_unsupported"
                Left QueryBindingMismatch -> finish "dead_letter" "query_binding_mismatch"
                Right result -> do
                  applied <- applyQueryResultCorrelated payment result
                    ("provider-query-job:" <> Execution.pqcOperationId claim) observedAt
                  case applied of
                    Left _ -> finish "dead_letter" "query_application_rejected"
                    Right ReconciliationProcessed -> finish "completed" "query_applied"
                    Right ReconciliationRetry -> finish "retry" "provider_nonterminal"
                    Right ReconciliationDeadLetter -> finish "dead_letter" "provider_requires_review"
        ) envPool
  where finish = Execution.finishProviderQuery claim

validateQueryClaim
  :: Execution.ProviderQueryClaim -> Bool
  -> SqlPersistT IO (Maybe Execution.BoundProviderPayment)
validateQueryClaim claim closeTerminal = do
  live <- Execution.lockLiveProviderQuery claim
  if not live then pure Nothing else do
    ready <- Execution.queryRecoveryReady (Execution.pqcProvider claim)
      (Execution.pqcEnvironment claim) (Execution.pqcMerchantRef claim)
    terminal <- Execution.providerQueryOperationTerminal claim
    if not ready then finish "retry" "configuration_revoked"
    else if closeTerminal && terminal then finish "completed" "operation_already_terminal"
    else do
      payment <- Execution.loadQueryClaimPayment claim
      case payment of
        Left _ -> finish "dead_letter" "immutable_binding_unavailable"
        Right bound -> pure (Just bound)
  where finish status code = Execution.finishProviderQuery claim status code >> pure Nothing

-- | Apply only an authenticated query parsed by the provider adapter against
-- a database-loaded immutable binding. The caller owns the transaction (and
-- any worker lease lock). A domain rejection rolls back just this application;
-- SQL exceptions escape so the outer transaction can roll back in full.
-- Never use this with callback-supplied financial fields.
applyQueryResult
  :: Execution.BoundProviderPayment
  -> AdapterResult
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text ReconciliationDisposition)
applyQueryResult payment result eventId =
  applyQueryResultCorrelated payment result (correlationId eventId)

applyQueryResultCorrelated
  :: Execution.BoundProviderPayment -> AdapterResult -> Text -> UTCTime
  -> SqlPersistT IO (Either Text ReconciliationDisposition)
applyQueryResultCorrelated payment result eventId now = do
  rebound <- Execution.loadBoundProviderPayment
    (Execution.bppProvider payment) (Execution.bppEnvironment payment)
    (Execution.bppMerchantRef payment) (Execution.bppProviderResourceId payment)
    (Just (Execution.bppProviderReference payment))
  if rebound /= Right payment || not (queryResultMatches payment result)
    then pure (Left "Provider query result does not match its immutable binding")
    else do
      rawExecute "SAVEPOINT tdf_provider_query_apply" []
      applied <- applyQueryResultInTransaction payment result eventId now
      case applied of
        Left _ -> rawExecute "ROLLBACK TO SAVEPOINT tdf_provider_query_apply" []
        Right _ -> pure ()
      rawExecute "RELEASE SAVEPOINT tdf_provider_query_apply" []
      pure applied

-- Both supported query parsers return exact money for every status. A create
-- response (which lacks verified money), or a wrongly assembled typed result,
-- must not accidentally become authoritative evidence at this boundary.
queryResultMatches :: Execution.BoundProviderPayment -> AdapterResult -> Bool
queryResultMatches payment result =
  adapterResultExternalId result == Execution.bppProviderResourceId payment
    && adapterResultAmountMinor result == Just (Execution.bppAmountMinor payment)
    && adapterResultCurrency result == Just (Execution.bppCurrency payment)
    && adapterResultCertainty result == expectedCertainty (adapterResultState result)
  where
    expectedCertainty AdapterSucceeded = ProviderSucceeded
    expectedCertainty AdapterDeclined = ProviderConfirmedNoCharge
    expectedCertainty AdapterCancelled = ProviderConfirmedNoCharge
    expectedCertainty _ = ProviderAmbiguous

applyQueryResultInTransaction
  :: Execution.BoundProviderPayment
  -> AdapterResult
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text ReconciliationDisposition)
applyQueryResultInTransaction payment result eventId now = do
  operationResult <- Execution.recordReconciledCreateResult payment result now
  case operationResult of
    Left problem -> pure (Left problem)
    Right () -> case adapterResultState result of
      AdapterSucceeded -> do
        verified <- Checkout.recordVerifiedPayment Checkout.VerifiedPayment
          { Checkout.vpAttempt = Execution.bppAttempt payment
          , Checkout.vpCheckout = Execution.bppCheckout payment
          , Checkout.vpProvider = Execution.bppProvider payment
          , Checkout.vpEnvironment = Execution.bppEnvironment payment
          , Checkout.vpMerchantRef = Execution.bppMerchantRef payment
          , Checkout.vpResourceType = Execution.bppResourceType payment
          , Checkout.vpProviderResource = Execution.bppProviderResourceId payment
          , Checkout.vpProviderResourcePath = Execution.bppProviderResourcePath payment
          , Checkout.vpOrderReference = Execution.bppDomainOrderId payment
          , Checkout.vpProviderReference = Execution.bppProviderReference payment
          , Checkout.vpAmountMinor = Execution.bppAmountMinor payment
          , Checkout.vpCurrency = Execution.bppCurrency payment
          , Checkout.vpEvidence = "server_to_server"
          , Checkout.vpOccurredAt = now
          , Checkout.vpCorrelationId = eventId
          }
        case verified of
          Left problem -> pure (Left problem)
          Right _ -> pure (Right ReconciliationProcessed)
      AdapterDeclined -> confirmNoCharge payment PaymentFailureConfirmed
        "provider_declined" eventId now
      AdapterCancelled -> confirmNoCharge payment PaymentCancellationRequested
        "provider_cancelled" eventId now
      AdapterPending -> markStillProcessing payment eventId now
      AdapterRequiresCustomerAction -> markStillProcessing payment eventId now
      AdapterUnknown -> do
        Checkout.recordReconciliationException
          (Execution.bppProvider payment)
          (Execution.bppEnvironment payment)
          (Execution.bppMerchantRef payment)
          "provider_status_unknown"
          (Checkout.checkoutReferenceId (Execution.bppCheckout payment))
          (Execution.bppProviderResourceId payment)
          (Execution.bppAmountMinor payment)
          (adapterResultAmountMinor result)
          (Execution.bppCurrency payment)
          now
        pure (Right ReconciliationRetry)
      AdapterAuthorized ->
        pure (Left "Unsupported authorization state requires operator review")
      AdapterReversed ->
        pure (Left "Unexpected reversal state requires operator review")

confirmNoCharge
  :: Execution.BoundProviderPayment
  -> PaymentEvent
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text ReconciliationDisposition)
confirmNoCharge payment event failureCode eventId now = do
  observed <- Execution.validateNoChargeObservation payment
  case observed of
    Left problem -> pure (Left problem)
    Right True -> pure (Right ReconciliationProcessed)
    Right False -> do
      transitioned <- Intent.transitionPaymentIntent
        (Intent.PaymentIntentReference (Execution.bppPaymentIntentId payment))
        event "provider" eventId now
      case transitioned of
        Left problem -> pure (Left problem)
        Right _ -> do
          if event == PaymentCancellationRequested
            then Checkout.recordPaymentCancellation
              (Execution.bppCheckout payment) (Execution.bppAttempt payment)
              (Execution.bppProvider payment) eventId now
            else Checkout.recordPaymentFailure
              (Execution.bppCheckout payment) (Execution.bppAttempt payment)
              (Execution.bppProvider payment) failureCode eventId now
          pure (Right ReconciliationProcessed)

markStillProcessing
  :: Execution.BoundProviderPayment
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text ReconciliationDisposition)
markStillProcessing payment eventId now = do
  Checkout.recordPaymentProcessing
    (Execution.bppCheckout payment)
    (Execution.bppAttempt payment)
    (Execution.bppProvider payment)
    eventId
    now
  pure (Right ReconciliationRetry)

recordQueryMismatch
  :: Execution.BoundProviderPayment
  -> UTCTime
  -> SqlPersistT IO ()
recordQueryMismatch payment now =
  Checkout.recordReconciliationException
    (Execution.bppProvider payment)
    (Execution.bppEnvironment payment)
    (Execution.bppMerchantRef payment)
    "provider_query_binding_mismatch"
    (Checkout.checkoutReferenceId (Execution.bppCheckout payment))
    (Execution.bppProviderResourceId payment)
    (Execution.bppAmountMinor payment)
    Nothing
    (Execution.bppCurrency payment)
    now

validateStoredEventEnvelope
  :: ProviderEvent.ProviderEventPayload
  -> Either Text (Checkout.PaymentProvider, Checkout.CheckoutEnvironment, A.Value)
validateStoredEventEnvelope payload = do
  provider <- case ProviderEvent.pepProvider payload of
    "placetopay" -> Right Checkout.ProviderPlaceToPay
    "payphone" -> Right Checkout.ProviderPayPhone
    _ -> Left "Unsupported provider event was routed to hosted-provider reconciliation"
  environment <- case ProviderEvent.pepEnvironment payload of
    "sandbox" -> Right Checkout.CheckoutSandbox
    "production" -> Right Checkout.CheckoutProduction
    _ -> Left "Stored provider event environment is invalid"
  rawValue <- either (const (Left "Stored provider event is not valid JSON")) Right
    (A.eitherDecodeStrict' (ProviderEvent.pepRawPayload payload))
  pure (provider, environment, rawValue)

validateAssessment
  :: ProviderEvent.ProviderEventPayload
  -> Checkout.PaymentProvider
  -> NotificationAssessment
  -> Either Text ()
validateAssessment payload provider assessment
  | Just (notificationExternalId assessment) /=
      ProviderEvent.pepProviderResourceId payload =
      Left "Stored callback resource does not match its immutable metadata"
  | not (notificationRequiresQuery assessment) =
      Left "Provider callback must require authoritative reconciliation"
  | provider == Checkout.ProviderPlaceToPay
      && (ProviderEvent.pepEvidenceType payload /= "signature_verified"
        || not (ProviderEvent.pepSignatureVerified payload)
        || not (notificationAuthenticated assessment)) =
      Left "PlaceToPay callback lacks valid signed evidence"
  | provider == Checkout.ProviderPayPhone
      && (ProviderEvent.pepEvidenceType payload /= "untrusted_callback"
        || ProviderEvent.pepSignatureVerified payload
        || notificationAuthenticated assessment) =
      Left "PayPhone callback trust classification is invalid"
  | otherwise = Right ()

resultFor
  :: ReconciliationDisposition
  -> Maybe Execution.BoundProviderPayment
  -> ProviderReconciliationResult
resultFor disposition payment = ProviderReconciliationResult
  { prrDisposition = disposition
  , prrSummary = case disposition of
      ReconciliationProcessed -> "Authoritative provider state was applied"
      ReconciliationRetry -> "Provider payment remains non-terminal"
      ReconciliationDeadLetter -> "Provider event requires operator review"
  , prrCheckoutId = Checkout.checkoutReferenceId . Execution.bppCheckout <$> payment
  , prrAttemptId = Checkout.paymentAttemptReferenceId . Execution.bppAttempt <$> payment
  }

retry :: Text -> Maybe Execution.BoundProviderPayment -> ProviderReconciliationResult
retry summary payment = (resultFor ReconciliationRetry payment) { prrSummary = summary }

deadLetter :: Text -> Maybe Execution.BoundProviderPayment -> ProviderReconciliationResult
deadLetter summary payment =
  (resultFor ReconciliationDeadLetter payment) { prrSummary = summary }

correlationId :: Text -> Text
correlationId eventId = "provider-event:" <> T.take 220 eventId
