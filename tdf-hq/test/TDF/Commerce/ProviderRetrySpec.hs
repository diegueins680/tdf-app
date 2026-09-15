{-# LANGUAGE OverloadedStrings #-}

-- Real PostgreSQL persistence/concurrency tests; no provider HTTP or credentials.
module TDF.Commerce.ProviderRetrySpec (spec) where

import           Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (throwIO)
import           Control.Monad (forM, forM_, unless)
import           Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import           Data.Either (isLeft, isRight, rights)
import           Data.Int (Int64)
import           Data.Pool (destroyAllResources)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (addUTCTime, getCurrentTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool)
import           System.Environment (lookupEnv)
import           Test.Hspec

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentIntentStore as Intent
import qualified TDF.Commerce.PaymentRuntimeStore as Runtime
import           TDF.Commerce.ProviderAdapter (AdapterOperation(..))
import           TDF.Commerce.ProviderCapabilities (PaymentMethod(..))
import qualified TDF.Commerce.ProviderExecutionStore as Execution
import           TDF.Commerce.StateMachine (PaymentEvent(..))
import           TDF.Server.ProviderExecution (providerReference)

spec :: Spec
spec = do
  configured <- runIO (lookupEnv "TDF_PROVIDER_RETRY_DATABASE_URL")
  case configured of
    Nothing -> pure ()
    Just url -> beforeAll (openDatabase url) $ afterAll destroyAllResources $
      describe "provider-retry-runtime PostgreSQL" $ do
        it "serializes different keys and permits only one active attempt" $ \pool -> do
          creation <- newCheckout pool
          results <- concurrently
            [ begin pool creation
            , begin pool creation { Checkout.pacIdempotencyKey = "concurrent-different-key" }
            ]
          length (rights results) `shouldBe` 1
          assertCounts pool creation 1 1

        it "replays the same key concurrently without another attempt or intent" $ \pool -> do
          creation <- newCheckout pool
          results <- concurrently [begin pool creation, begin pool creation]
          results `shouldSatisfy` all isRight
          case results of
            [first, second] -> first `shouldBe` second
            _ -> expectationFailure "Expected both concurrent requests to finish"
          assertCounts pool creation 1 1

        it "blocks every alternate rail and method while the result is ambiguous" $ \pool -> do
          creation <- newCheckout pool
          attempt <- begin pool creation >>= requireRight
          now <- getCurrentTime
          runSqlPool (Checkout.recordPaymentFailure
            (Checkout.pacCheckout creation) attempt Checkout.ProviderPlaceToPay
            "provider_transport_ambiguous" "provider-retry-test" now) pool
          forM_ [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone,
              Checkout.ProviderPayPal, Checkout.ProviderDatafast] $ \provider -> do
            let method = case provider of
                  Checkout.ProviderPayPhone -> MethodPayPhoneWallet
                  Checkout.ProviderPayPal -> MethodPayPalWallet
                  _ -> MethodCard
            result <- runSqlPool (Runtime.beginPaymentAttemptForMethod method creation
              { Checkout.pacProvider = provider
              , Checkout.pacIdempotencyKey = "another-key-after-timeout"
              }) pool
            result `shouldSatisfy` isLeft
          result <- runSqlPool (Runtime.beginPaymentAttemptForMethod MethodBankRedirect creation
            { Checkout.pacIdempotencyKey = "another-method-after-timeout" }) pool
          result `shouldSatisfy` isLeft
          assertCounts pool creation 1 1

        it "creates a fresh intent and provider reference only after confirmed no-charge" $ \pool -> do
          creation <- newCheckout pool
          first <- begin pool creation >>= requireRight
          firstIntent <- intentFor pool first
          now <- getCurrentTime
          _ <- runSqlPool (Intent.transitionPaymentIntent (Intent.PaymentIntentReference firstIntent)
            PaymentFailureConfirmed "provider" "verified-test-decline" now) pool >>= requireRight
          second <- begin pool creation { Checkout.pacIdempotencyKey = "retry-after-verified-decline" }
            >>= requireRight
          second `shouldNotBe` first
          secondIntent <- intentFor pool second
          secondIntent `shouldNotBe` firstIntent
          providerReference Checkout.ProviderPlaceToPay (Checkout.paymentAttemptReferenceId first)
            `shouldNotBe` providerReference Checkout.ProviderPlaceToPay
              (Checkout.paymentAttemptReferenceId second)
          -- A stale replay recovers the old attempt; it cannot create a third one.
          begin pool creation `shouldReturn` Right first
          assertCounts pool creation 2 2

        it "preserves legacy intent keys, external references and idempotent bindings" $ \pool -> do
          creation <- newCheckout pool
          now <- getCurrentTime
          legacy <- runSqlPool (Intent.createPaymentIntent Intent.PaymentIntentCreation
            { Intent.picCheckout = Checkout.pacCheckout creation
            , Intent.picEnvironment = Checkout.CheckoutSandbox
            , Intent.picProvider = Checkout.ProviderPlaceToPay
            , Intent.picPaymentMethod = MethodCard
            , Intent.picCaptureMethod = Intent.CaptureAutomatic
            , Intent.picAmountMinor = 12515
            , Intent.picCurrency = "USD"
            , Intent.picIdempotencyKey = "payment-intent:legacy-provider-key"
            , Intent.picOccurredAt = now
            , Intent.picCorrelationId = "legacy-test"
            }) pool >>= requireRight
          attempt <- runSqlPool (Checkout.beginPaymentAttempt creation) pool >>= requireRight
          _ <- runSqlPool (Intent.bindPaymentAttemptToIntent (Intent.pisReference legacy) attempt)
            pool >>= requireRight
          let reference = providerReference Checkout.ProviderPlaceToPay
                (Checkout.checkoutReferenceId (Checkout.pacCheckout creation))
          operation <- runSqlPool (Execution.prepareProviderOperation Execution.ProviderOperationPreparation
            { Execution.popAttempt = attempt
            , Execution.popProvider = Checkout.ProviderPlaceToPay
            , Execution.popEnvironment = Checkout.CheckoutSandbox
            , Execution.popMerchantRef = "synthetic-provider-retry-merchant"
            , Execution.popProviderReference = reference
            , Execution.popOperation = AdapterCreate
            , Execution.popIdempotencyKey = Checkout.pacIdempotencyKey creation
            , Execution.popRequestSha256 = T.replicate 64 "a"
            , Execution.popOccurredAt = now
            }) pool >>= requireRight
          begin pool creation `shouldReturn` Right attempt
          runSqlPool (Execution.loadAttemptProviderReference attempt) pool
            `shouldReturn` Right (Just reference)
          claims <- concurrently $ replicate 2 $ runSqlPool
            (Execution.claimProviderOperation (Execution.porReference operation) now
              "synthetic-provider-retry-encryption-key") pool
          length [() | Right (Execution.ProviderOperationClaimed _) <- claims] `shouldBe` 1
          length [() | Right Execution.ProviderOperationBusy <- claims] `shouldBe` 1
          assertCounts pool creation 1 1

        it "rejects changed immutable fields and cross-checkout idempotency reuse" $ \pool -> do
          creation <- newCheckout pool
          _ <- begin pool creation >>= requireRight
          begin pool creation { Checkout.pacAmountMinor = 1 } >>= (`shouldSatisfy` isLeft)
          other <- newCheckout pool
          begin pool other { Checkout.pacIdempotencyKey = Checkout.pacIdempotencyKey creation }
            >>= (`shouldSatisfy` isLeft)
          runSqlPool (Runtime.beginPaymentAttemptForMethod MethodBankRedirect creation) pool
            >>= (`shouldSatisfy` isLeft)
          assertCounts pool creation 1 1
          assertCounts pool other 0 0

        it "does not silently backfill or reinterpret unbound historical attempts" $ \pool -> do
          creation <- newCheckout pool
          _ <- runSqlPool (Checkout.beginPaymentAttempt creation) pool >>= requireRight
          begin pool creation >>= (`shouldSatisfy` isLeft)
          assertCounts pool creation 1 0

        it "replays Datafast status verification on the original debit-sale attempt" $ \pool -> do
          seed <- newCheckout pool
          let creation = seed { Checkout.pacProvider = Checkout.ProviderDatafast }
              start value = runSqlPool (Runtime.beginPaymentAttempt value) pool
          origin <- start creation >>= requireRight
          now <- getCurrentTime
          runSqlPool (Checkout.recordPaymentProcessing (Checkout.pacCheckout creation) origin
            Checkout.ProviderDatafast "synthetic-datafast-status" now) pool
          start creation `shouldReturn` Right origin
          start creation { Checkout.pacIdempotencyKey = "datafast-different-key-while-pending" }
            >>= (`shouldSatisfy` isLeft)
          assertCounts pool creation 1 1

        it "keeps PayPal create and capture on one intent without allowing a second capture key" $ \pool -> do
          seed <- newCheckout pool
          let creation = seed { Checkout.pacProvider = Checkout.ProviderPayPal }
              start value = runSqlPool (Runtime.beginPaymentAttempt value) pool
          origin <- start creation >>= requireRight
          now <- getCurrentTime
          _ <- runSqlPool (Checkout.bindProviderResource Checkout.ProviderBindingCreation
            { Checkout.pbcAttempt = origin
            , Checkout.pbcCheckout = Checkout.pacCheckout creation
            , Checkout.pbcProvider = Checkout.ProviderPayPal
            , Checkout.pbcEnvironment = Checkout.CheckoutSandbox
            , Checkout.pbcMerchantRef = Checkout.pacMerchantRef creation
            , Checkout.pbcResourceType = "order"
            , Checkout.pbcProviderResource = "synthetic-" <> Checkout.paymentAttemptReferenceId origin
            , Checkout.pbcResourcePath = Nothing
            , Checkout.pbcOrderReference = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
            , Checkout.pbcAmountMinor = 12515
            , Checkout.pbcCurrency = "USD"
            , Checkout.pbcStage = Checkout.AttemptRequiresCustomerAction
            , Checkout.pbcOccurredAt = now
            , Checkout.pbcCorrelationId = "synthetic-paypal-create"
            }) pool >>= requireRight
          let capture = creation
                { Checkout.pacOperation = Checkout.OperationCapture
                , Checkout.pacIdempotencyKey = "paypal-capture-original-key"
                }
          captured <- start capture >>= requireRight
          firstIntent <- intentFor pool origin
          intentFor pool captured `shouldReturn` firstIntent
          start capture `shouldReturn` Right captured
          start capture { Checkout.pacIdempotencyKey = "paypal-capture-different-key" }
            >>= (`shouldSatisfy` isLeft)
          assertCounts pool creation 2 1

openDatabase :: String -> IO ConnectionPool
openDatabase url = do
  pool <- runNoLoggingT (createPostgresqlPool (BS.pack url) 4)
  names <- runSqlPool (rawSql "SELECT current_database()" []) pool :: IO [Single Text]
  unless (names == [Single "tdf_provider_retry_test"]) $ do
    destroyAllResources pool
    fail "Provider retry tests require the dedicated disposable database"
  runSqlPool (do
    rawExecute
      "UPDATE commerce_provider_account SET status='ready',contract_status='approved',\
      \ credential_status='validated',enabled=true,verified_at=NOW(),verified_by=1,\
      \ merchant_account_ref='synthetic-provider-retry-merchant' WHERE environment='sandbox'" []
    rawExecute
      "UPDATE commerce_provider_capability SET verification_status='sandbox_verified',verified_at=NOW()\
      \ WHERE provider_account_id IN (SELECT id FROM commerce_provider_account WHERE environment='sandbox')" []
    ) pool
  pure pool

newCheckout :: ConnectionPool -> IO Checkout.PaymentAttemptCreation
newCheckout pool = do
  checkoutId <- toText <$> nextRandom
  now <- getCurrentTime
  runSqlPool (rawExecute
    "INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,environment,\
    \ currency,subtotal_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at)\
    \ VALUES (?::uuid,'event_ticket_order',?,'awaiting_payment','sandbox','USD',12515,12515,\
    \ 'synthetic@example.test',?,?,?)"
    [PersistText checkoutId, PersistText checkoutId, PersistText checkoutId, PersistText checkoutId,
      PersistUTCTime (addUTCTime 1800 now)]) pool
  pure Checkout.PaymentAttemptCreation
    { Checkout.pacCheckout = Checkout.CheckoutReference checkoutId
    , Checkout.pacProvider = Checkout.ProviderPlaceToPay
    , Checkout.pacEnvironment = Checkout.CheckoutSandbox
    , Checkout.pacOperation = Checkout.OperationCreate
    , Checkout.pacAmountMinor = 12515
    , Checkout.pacCurrency = "USD"
    , Checkout.pacMerchantRef = "synthetic-provider-retry-merchant"
    , Checkout.pacIdempotencyKey = "attempt-" <> checkoutId
    , Checkout.pacCreatedAt = now
    , Checkout.pacCorrelationId = "provider-retry-test-" <> checkoutId
    }

begin :: ConnectionPool -> Checkout.PaymentAttemptCreation
  -> IO (Either Text Checkout.PaymentAttemptReference)
begin pool creation = runSqlPool (Runtime.beginPaymentAttemptForMethod MethodCard creation) pool

intentFor :: ConnectionPool -> Checkout.PaymentAttemptReference -> IO Text
intentFor pool attempt = do
  rows <- runSqlPool (rawSql "SELECT payment_intent_id::text FROM commerce_payment_attempt WHERE id=?::uuid"
    [PersistText (Checkout.paymentAttemptReferenceId attempt)]) pool :: IO [Single Text]
  case rows of
    [Single intent] -> pure intent
    _ -> fail "Expected exactly one bound canonical intent"

assertCounts :: ConnectionPool -> Checkout.PaymentAttemptCreation -> Int64 -> Int64 -> Expectation
assertCounts pool creation attempts intents = do
  rows <- runSqlPool (rawSql
    "SELECT (SELECT COUNT(*) FROM commerce_payment_attempt WHERE checkout_id=?::uuid),\
    \ (SELECT COUNT(*) FROM commerce_payment_intent WHERE checkout_id=?::uuid)"
    (replicate 2 (PersistText (Checkout.checkoutReferenceId (Checkout.pacCheckout creation))))) pool
  (rows :: [(Single Int64, Single Int64)]) `shouldBe` [(Single attempts, Single intents)]

requireRight :: Show error => Either error value -> IO value
requireRight = either (fail . show) pure

concurrently :: [IO value] -> IO [value]
concurrently actions = do
  completions <- forM actions $ \action -> do
    completion <- newEmptyMVar
    _ <- forkFinally action (putMVar completion)
    pure completion
  mapM (\completion -> takeMVar completion >>= either throwIO pure) completions
