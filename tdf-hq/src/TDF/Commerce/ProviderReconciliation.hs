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
  ) where

import qualified Data.Aeson as A
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.Persist.Sql
  ( SqlPersistT, runSqlPool, transactionSave, transactionUndo )
import           System.Entropy (getEntropy)

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentIntentStore as Intent
import           TDF.Commerce.ProviderAdapter
import           TDF.Commerce.ProviderAdapter.Http
  ( AdapterTransportError(..), executeAdapterRequest, sharedProviderManager )
import qualified TDF.Commerce.ProviderEventStore as ProviderEvent
import qualified TDF.Commerce.ProviderExecutionStore as Execution
import           TDF.Commerce.ProviderRuntimeConfig
  ( RuntimeProviderAdapter(..), loadRuntimeProviderAdapter )
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
processProviderEventIO env@Env{envPool} payload now =
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
                    Right payment -> queryAndApply env rpaAdapter payload payment now

queryAndApply
  :: Env
  -> ProviderAdapter
  -> ProviderEvent.ProviderEventPayload
  -> Execution.BoundProviderPayment
  -> UTCTime
  -> IO ProviderReconciliationResult
queryAndApply Env{envPool} adapter payload payment now = do
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
      ids = Just payment
  case adapterBuildQuery adapter context locator of
    Left _ -> pure (deadLetter "Stored provider binding cannot form a safe query" ids)
    Right request -> do
      response <- executeAdapterRequest sharedProviderManager request
      case response of
        Left AdapterTransportError{} ->
          pure (retry "Authoritative provider query is temporarily unavailable" ids)
        Right providerValue ->
          case adapterParseResponse adapter AdapterQuery locator providerValue of
            Left _ -> do
              runSqlPool
                (recordQueryMismatch payment now)
                envPool
              pure (deadLetter
                "Authoritative provider query did not match the immutable payment binding"
                ids)
            Right result -> do
              applied <- runSqlPool
                (applyQueryResult payment result
                  (ProviderEvent.providerEventReferenceId
                    (ProviderEvent.pepReference payload)) now)
                envPool
              pure $ case applied of
                Left problem -> retry problem ids
                Right disposition -> resultFor disposition ids

applyQueryResult
  :: Execution.BoundProviderPayment
  -> AdapterResult
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text ReconciliationDisposition)
applyQueryResult payment result eventId now = do
  transactionSave
  operationResult <- Execution.recordReconciledCreateResult payment result now
  case operationResult of
    Left problem -> transactionUndo >> pure (Left problem)
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
          , Checkout.vpCorrelationId = correlationId eventId
          }
        case verified of
          Left problem -> transactionUndo >> pure (Left problem)
          Right _ -> transactionSave >> pure (Right ReconciliationProcessed)
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
        transactionSave
        pure (Right ReconciliationRetry)
      AdapterAuthorized -> transactionUndo >>
        pure (Left "Unsupported authorization state requires operator review")
      AdapterReversed -> transactionUndo >>
        pure (Left "Unexpected reversal state requires operator review")

confirmNoCharge
  :: Execution.BoundProviderPayment
  -> PaymentEvent
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text ReconciliationDisposition)
confirmNoCharge payment event failureCode eventId now = do
  transitioned <- Intent.transitionPaymentIntent
    (Intent.PaymentIntentReference (Execution.bppPaymentIntentId payment))
    event "provider" (correlationId eventId) now
  case transitioned of
    Left problem -> transactionUndo >> pure (Left problem)
    Right _ -> do
      Checkout.recordPaymentFailure
        (Execution.bppCheckout payment)
        (Execution.bppAttempt payment)
        (Execution.bppProvider payment)
        failureCode
        (correlationId eventId)
        now
      transactionSave
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
    (correlationId eventId)
    now
  transactionSave
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
