{-# LANGUAGE OverloadedStrings #-}

-- Real PostgreSQL persistence/concurrency tests; no provider HTTP or credentials.
module TDF.Commerce.ProviderRetrySpec (spec) where

import           Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (bracket, throwIO)
import           Control.Monad (forM, forM_, unless)
import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.Reader (runReaderT)
import           Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as A
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Either (isLeft, isRight, rights)
import           Data.Int (Int64)
import           Data.Pool (destroyAllResources)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime(..), addUTCTime, fromGregorian, getCurrentTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import           Network.Socket (SockAddr(..))
import           Servant (ServerError, errHTTPCode, runHandler, (:<|>)(..))
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import           Test.Hspec

import qualified TDF.Commerce.CheckoutStore as Checkout
import           TDF.API.ProviderExecution (PaymentSessionCreateDTO(..), PaymentSessionDTO(..))
import           TDF.DB (Env(..))
import qualified TDF.Commerce.PaymentIntentStore as Intent
import qualified TDF.Commerce.PaymentRuntimeStore as Runtime
import           TDF.Commerce.ProviderAdapter (AdapterOperation(..))
import qualified TDF.Commerce.ProviderAdapter as Adapter
import qualified TDF.Commerce.ProviderAdapter.PayPhone as PayPhone
import qualified TDF.Commerce.ProviderAdapter.PlaceToPay as PlaceToPay
import           TDF.Commerce.ProviderCapabilities (PaymentMethod(..))
import qualified TDF.Commerce.ProviderExecutionStore as Execution
import qualified TDF.Commerce.ProviderEventStore as Event
import qualified TDF.Commerce.ProviderEventWorker as EventWorker
import           TDF.Commerce.StateMachine (PaymentEvent(..))
import           TDF.Server.ProviderExecution (providerReference, providerExecutionServer)
import qualified TDF.Server.ServiceStorefront as Storefront

spec :: Spec
spec = do
  notificationMinimizationSpec
  configured <- runIO (lookupEnv "TDF_PROVIDER_RETRY_DATABASE_URL")
  case configured of
    Nothing -> pure ()
    Just url -> beforeAll (openDatabase url) $ afterAll destroyAllResources $
      describe "provider-retry-runtime PostgreSQL" $ do
        notificationInboxSpec
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

        describe "exact create response recovery" $ do
          forM_ [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone] $ \provider ->
            forM_ ["paid", "expired", "cancelled", "refunded", "disputed"] $ \checkoutStatus ->
              it ("recovers " <> show provider <> " after checkout " <> T.unpack checkoutStatus) $
                \pool -> withRecoveryEnvironment $ do
                  (creation, operation, request) <- replayFixture pool provider
                  closeCheckout pool creation checkoutStatus
                  setOperationState pool operation "succeeded" "succeeded"
                  recovered <- replayHandler pool creation request >>= requireRight
                  pssAttemptId recovered `shouldBe`
                    Checkout.paymentAttemptReferenceId (Execution.porAttempt operation)
                  pssState recovered `shouldBe` "succeeded"
                  pssCanRetryOrFallback recovered `shouldBe` False
                  assertCounts pool creation 1 1

          it "recovers with suspended accounts and absent provider credentials" $ \pool ->
            withRecoveryEnvironment $ do
              (creation, operation, request) <- replayFixture pool Checkout.ProviderPlaceToPay
              setOperationState pool operation "ambiguous" "ambiguous"
              let suspend enabled status = runSqlPool (rawExecute
                    "UPDATE commerce_provider_account SET enabled=?,status=?\
                    \ WHERE provider='placetopay' AND environment='sandbox'"
                    [PersistBool enabled, PersistText status]) pool
              bracket (suspend False "suspended") (const (suspend True "ready")) $ \_ -> do
                recovered <- replayHandler pool creation request >>= requireRight
                pssState recovered `shouldBe` "ambiguous"
                pssCanRetryOrFallback recovered `shouldBe` False
              assertCounts pool creation 1 1

          it "normalizes the exact contact replay without requiring an unexpired payable state" $ \pool ->
            withRecoveryEnvironment $ do
              (creation, operation, request) <- replayFixture pool Checkout.ProviderPayPhone
              runSqlPool (rawExecute
                "UPDATE commerce_checkout_session SET status='paid' WHERE id=?::uuid"
                [PersistText (Checkout.checkoutReferenceId (Checkout.pacCheckout creation))]) pool
              setOperationState pool operation "succeeded" "succeeded"
              recovered <- replayHandler pool creation request
                { pscBuyerPhone = Just " 991234567 ", pscBuyerCountryCode = Just " 593 " }
                >>= requireRight
              pssState recovered `shouldBe` "succeeded"
              assertCounts pool creation 1 1

          it "requires the operation decryption key even when provider secrets are absent" $ \pool ->
            withRecoveryEnvironment $ do
              (creation, operation, request) <- replayFixture pool Checkout.ProviderPlaceToPay
              setOperationState pool operation "processing" "ambiguous"
              setEnv "COMMERCE_EVENT_ENCRYPTION_KEY" ""
              replayHandler pool creation request >>= assertHttpError 503
              assertCounts pool creation 1 1

          it "returns every contacted operation state without claiming it again" $ \pool ->
            withRecoveryEnvironment $ forM_
              [("in_flight", "ambiguous"), ("requires_customer_action", "ambiguous")
              , ("processing", "ambiguous"), ("confirmed_no_charge", "confirmed_no_charge")
              , ("failed", "rejected_before_creation")] $ \(status, certainty) -> do
                (creation, operation, request) <- replayFixture pool Checkout.ProviderPlaceToPay
                closeCheckout pool creation "expired"
                setOperationState pool operation status certainty
                recovered <- replayHandler pool creation request >>= requireRight
                pssState recovered `shouldBe` status
                if status == "requires_customer_action"
                  then pssRedirectUrl recovered `shouldBe`
                    Just "https://checkout-test.placetopay.ec/session/synthetic-recovery"
                  else pure ()
                assertCounts pool creation 1 1

          it "rejects changed method, contact, provider, key and lookup authorization" $ \pool ->
            withRecoveryEnvironment $ do
              (creation, operation, request) <- replayFixture pool Checkout.ProviderPayPhone
              closeCheckout pool creation "expired"
              setOperationState pool operation "processing" "ambiguous"
              forM_ [request { pscPaymentMethod = "card" }
                    , request { pscBuyerPhone = Just "991234568" }
                    , request { pscBuyerCountryCode = Just "1" }
                    , request { pscBuyerPhone = Nothing }] $ \changed ->
                replayHandler pool creation changed >>= assertHttpError 409
              replayHandler pool creation request { pscProvider = "placetopay" }
                >>= assertHttpError 404
              replayHandler pool creation { Checkout.pacIdempotencyKey = "changed-recovery-key" }
                request >>= assertHttpError 404
              let checkoutId = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
              runSqlPool (rawExecute
                "UPDATE commerce_checkout_session SET lookup_token_hash=? WHERE id=?::uuid"
                [PersistText "revoked-synthetic-token", PersistText checkoutId]) pool
              replayHandler pool creation request >>= assertHttpError 404
              assertCounts pool creation 1 1

          it "does not reveal another checkout operation using its request key" $ \pool ->
            withRecoveryEnvironment $ do
              (creation, operation, request) <- replayFixture pool Checkout.ProviderPlaceToPay
              setOperationState pool operation "processing" "ambiguous"
              (other, _, _) <- replayFixture pool Checkout.ProviderPlaceToPay
              closeCheckout pool other "expired"
              replayHandler pool other
                { Checkout.pacIdempotencyKey = Checkout.pacIdempotencyKey creation } request
                >>= assertHttpError 404
              assertCounts pool other 1 1

          it "does not contact a merely prepared operation on an expired checkout" $ \pool ->
            withRecoveryEnvironment $ do
              (creation, operation, request) <- replayFixture pool Checkout.ProviderPlaceToPay
              closeCheckout pool creation "expired"
              replayHandler pool creation request >>= assertHttpError 404
              let checkoutId = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
              stored <- runSqlPool (Execution.loadAuthorizedCreateOperation checkoutId
                (Checkout.paymentAttemptReferenceId (Execution.porAttempt operation))
                (digestText checkoutId) recoveryEncryptionKey) pool >>= requireRight
              Execution.porStatus stored `shouldBe` "prepared"
              assertCounts pool creation 1 1

          it "replays concurrently without additional operations or intent mutations" $ \pool ->
            withRecoveryEnvironment $ do
              (creation, operation, request) <- replayFixture pool Checkout.ProviderPlaceToPay
              closeCheckout pool creation "paid"
              setOperationState pool operation "succeeded" "succeeded"
              results <- concurrently (replicate 4 (replayHandler pool creation request))
              results `shouldSatisfy` all isRight
              length (rights results) `shouldBe` 4
              map (fmap pssOperationId) results `shouldBe`
                replicate 4 (Right (Execution.providerOperationReferenceId
                  (Execution.porReference operation)))
              assertCounts pool creation 1 1

notificationMinimizationSpec :: Spec
notificationMinimizationSpec = describe "provider notification evidence minimization" $ do
  it "drops arbitrary PayPal fields recursively without changing capture evidence" $ do
    let original = paypalNotification "WH-SYNTHETIC" "125.15" privateMarker
    retained <- requireRight (Event.minimizeProviderEventPayload Checkout.ProviderPayPal original)
    BS.isInfixOf (TE.encodeUtf8 privateMarker) retained `shouldBe` False
    originalEnvelope <- requireRight (Storefront.parsePaypalWebhookEnvelope (BL.fromStrict original))
    retainedEnvelope <- requireRight (Storefront.parsePaypalWebhookEnvelope (BL.fromStrict retained))
    Storefront.parsePaypalWebhookCapture retainedEnvelope
      `shouldBe` Storefront.parsePaypalWebhookCapture originalEnvelope
    forM_ ["PAYMENT.CAPTURE.REFUNDED", "PAYMENT.CAPTURE.REVERSED"] $ \eventType -> do
      let envelope = A.object
            [ "id" A..= ("WH-SYNTHETIC" :: Text), "event_type" A..= (eventType :: Text)
            , "create_time" A..= notificationTime
            , "resource" A..= Storefront.pweResource originalEnvelope ]
      minimized <- requireRight (Event.minimizeProviderEventPayload
        Checkout.ProviderPayPal (encodeStrict envelope))
      parsed <- requireRight (Storefront.parsePaypalWebhookEnvelope (BL.fromStrict minimized))
      Storefront.parsePaypalWebhookCapture parsed
        `shouldBe` Storefront.parsePaypalWebhookCapture originalEnvelope

  it "retains no unused resource fields for unsupported PayPal events" $ do
    let payload = encodeStrict $ A.object
          [ "id" A..= ("WH-IGNORED" :: Text), "event_type" A..= ("CUSTOMER.DISPUTE.CREATED" :: Text)
          , "create_time" A..= notificationTime
          , "resource" A..= A.object ["id" A..= ("CASE-1" :: Text), "evidence" A..= privateMarker] ]
    retained <- requireRight (Event.minimizeProviderEventPayload Checkout.ProviderPayPal payload)
    BS.isInfixOf (TE.encodeUtf8 privateMarker) retained `shouldBe` False
    parsed <- requireRight (Storefront.parsePaypalWebhookEnvelope (BL.fromStrict retained))
    Storefront.pweResource parsed `shouldBe` A.object ["id" A..= ("CASE-1" :: Text)]

  it "preserves PlaceToPay signed fields and still rejects a wrong signing secret" $ do
    let original = placeToPayNotification privateMarker
        config = PlaceToPay.PlaceToPayConfig Checkout.CheckoutSandbox
          "synthetic-login" "synthetic-secret" []
    retained <- requireRight (Event.minimizeProviderEventPayload Checkout.ProviderPlaceToPay original)
    BS.isInfixOf (TE.encodeUtf8 privateMarker) retained `shouldBe` False
    originalValue <- requireRight (A.eitherDecodeStrict' original)
    retainedValue <- requireRight (A.eitherDecodeStrict' retained)
    let assessed = PlaceToPay.verifyPlaceToPayNotification config retainedValue
    assessed `shouldSatisfy` isRight
    assessed `shouldBe` PlaceToPay.verifyPlaceToPayNotification config originalValue
    PlaceToPay.verifyPlaceToPayNotification config { PlaceToPay.ptpSecretKey = "wrong-secret" }
      retainedValue `shouldSatisfy` isLeft

  it "canonicalizes PayPhone aliases and drops untrusted status/customer data" $ do
    let alias = encodeStrict $ A.object
          [ "id" A..= (1234 :: Int), "clientTransactionID" A..= ("CLIENT-1" :: Text)
          , "StoreId" A..= ("synthetic-store" :: Text), "StatusCode" A..= (3 :: Int)
          , "card" A..= privateMarker ]
    retained <- requireRight (Event.minimizeProviderEventPayload Checkout.ProviderPayPhone alias)
    retained `shouldBe` encodeStrict (A.object
      [ "TransactionId" A..= (1234 :: Int), "ClientTransactionId" A..= ("CLIENT-1" :: Text)
      , "StoreId" A..= ("synthetic-store" :: Text) ])
    adapter <- requireRight (PayPhone.payPhoneAdapter
      (PayPhone.PayPhoneConfig "synthetic-token" "synthetic-store"))
    value <- requireRight (A.eitherDecodeStrict' retained)
    assessment <- requireRight (Adapter.adapterAssessNotification adapter value)
    Adapter.notificationExternalId assessment `shouldBe` "1234"
    Adapter.notificationMerchantReference assessment `shouldBe` Just "CLIENT-1"
    Adapter.notificationAuthenticated assessment `shouldBe` False
    Adapter.notificationRequiresQuery assessment `shouldBe` True

  it "is idempotent for every supported evidence schema" $
    forM_ notificationProviders $ \provider -> do
      let original = notificationPayload provider "EVENT-1" privateMarker
      retained <- requireRight (Event.minimizeProviderEventPayload provider original)
      Event.minimizeProviderEventPayload provider retained `shouldBe` Right retained

  it "rejects malformed, oversized, wrong-type and unsupported evidence without echoing input" $ do
    forM_ ["{\"SYNTHETIC-PRIVATE\":", "[]", "{}", BS.replicate (1024 * 1024 + 1) 'x'
      , "{\"TransactionId\":1.5,\"ClientTransactionId\":\"CLIENT-1\"}"
      , "{\"TransactionId\":-1,\"ClientTransactionId\":\"CLIENT-1\"}"
      , "{\"TransactionId\":1,\"ClientTransactionId\":{\"card\":\"SYNTHETIC-PRIVATE\"}}"] $ \raw ->
        forM_ notificationProviders $ \provider -> do
          let result = Event.minimizeProviderEventPayload provider raw
          result `shouldSatisfy` isLeft
          show result `shouldNotContain` "SYNTHETIC-PRIVATE"
    Event.minimizeProviderEventPayload Checkout.ProviderDatafast
      (paypalNotification "WH-1" "125.15" privateMarker) `shouldSatisfy` isLeft

  it "does not echo malformed PayPal JSON in public parser errors" $ do
    let result = Storefront.parsePaypalWebhookEnvelope "{\"SYNTHETIC-PRIVATE\":"
    result `shouldSatisfy` isLeft
    show result `shouldNotContain` "SYNTHETIC-PRIVATE"

  it "rejects nested retained-field objects, excessive field lengths and invalid timestamps" $ do
    let invalidPayPal = encodeStrict $ A.object
          [ "id" A..= ("WH-INVALID" :: Text), "event_type" A..= ("PAYMENT.CAPTURE.COMPLETED" :: Text)
          , "create_time" A..= notificationTime
          , "resource" A..= A.object ["amount" A..= A.object
              ["value" A..= A.object ["card" A..= privateMarker]]] ]
        invalidPlaceToPay = encodeStrict $ A.object
          [ "requestId" A..= (1234 :: Int)
          , "status" A..= A.object ["status" A..= ("APPROVED" :: Text), "date" A..= privateMarker]
          , "signature" A..= ("sha256:" <> T.replicate 64 "a") ]
        invalidPayPhone = encodeStrict $ A.object
          [ "TransactionId" A..= (1234 :: Int), "ClientTransactionId" A..= T.replicate 129 "x" ]
    forM_ [(Checkout.ProviderPayPal, invalidPayPal), (Checkout.ProviderPlaceToPay, invalidPlaceToPay)
      , (Checkout.ProviderPayPhone, invalidPayPhone)] $ \(provider, raw) -> do
        let result = Event.minimizeProviderEventPayload provider raw
        result `shouldSatisfy` isLeft
        show result `shouldNotContain` T.unpack privateMarker

notificationInboxSpec :: SpecWith ConnectionPool
notificationInboxSpec = describe "minimized notification inbox" $ do
  forM_ notificationProviders $ \provider ->
    it ("encrypts only retained " <> T.unpack (Checkout.paymentProviderText provider) <> " fields") $ \pool -> do
      creation <- newNotification provider
      stored <- storeNotification pool creation >>= requireRight
      Event.pesInserted stored `shouldBe` True
      retained <- requireRight (Event.minimizeProviderEventPayload provider (Event.pecRawPayload creation))
      readStoredNotification pool stored `shouldReturn` retained
      BS.isInfixOf (TE.encodeUtf8 privateMarker) retained `shouldBe` False
      claim <- runSqlPool (Event.claimProviderEvent (Event.pesReference stored) notificationTime) pool
      claim `shouldBe` Event.ProviderEventClaimed 1
      loaded <- runSqlPool (Event.loadProviderEventPayload (Event.pesReference stored) recoveryEncryptionKey)
        pool >>= requireRight
      Event.pepRawPayload loaded `shouldBe` retained
      show loaded `shouldBe` "ProviderEventPayload {payload = <redacted>}"
      if provider == Checkout.ProviderPayPal
        then EventWorker.validateStoredPaypalEvent loaded `shouldSatisfy` isRight
        else pure ()

  it "deduplicates ignored-field variations concurrently on one immutable event" $ \pool -> do
    creation <- newNotification Checkout.ProviderPayPal
    let variant = creation { Event.pecRawPayload = paypalNotification
          (Event.pecProviderEventId creation) "125.15" "DIFFERENT-SYNTHETIC-PRIVATE" }
    stored <- concurrently [storeNotification pool creation, storeNotification pool variant]
    results <- mapM requireRight stored
    length [() | result <- results, Event.pesInserted result] `shouldBe` 1
    map Event.pesReference results `shouldSatisfy` (\refs -> head refs == last refs)

  it "rejects changed monetary evidence, metadata, resource and trust on replay" $ \pool -> do
    creation <- newNotification Checkout.ProviderPayPal
    _ <- storeNotification pool creation >>= requireRight
    let changedAmount = creation { Event.pecRawPayload = paypalNotification
          (Event.pecProviderEventId creation) "0.01" privateMarker }
    forM_ [changedAmount, creation { Event.pecProviderResource = Just "OTHER-CAPTURE" }
      , creation { Event.pecEventType = "PAYMENT.CAPTURE.REVERSED" }
      , creation { Event.pecProviderCreatedAt = Just (addUTCTime 1 notificationTime) }] $ \changed ->
        storeNotification pool changed >>= (`shouldSatisfy` isLeft)
    runSqlPool (Event.storeUntrustedProviderEvent creation) pool >>= (`shouldSatisfy` isLeft)

  forM_ notificationProviders $ \provider ->
    it ("preserves exact historical raw " <> T.unpack (Checkout.paymentProviderText provider) <> " redelivery") $ \pool -> do
      creation <- newNotification provider
      historical <- insertHistoricalNotification pool creation
      replay <- storeNotification pool creation >>= requireRight
      Event.pesReference replay `shouldBe` historical
      Event.pesInserted replay `shouldBe` False
      readStoredNotification pool replay `shouldReturn` Event.pecRawPayload creation

  it "rejects malformed evidence before creating an inbox row" $ \pool -> do
    creation <- newNotification Checkout.ProviderPayPal
    result <- storeNotification pool creation { Event.pecRawPayload = "{\"card\":\"SYNTHETIC-PRIVATE\"}" }
    result `shouldSatisfy` isLeft
    counts <- runSqlPool (rawSql
      "SELECT count(*) FROM commerce_provider_event_inbox WHERE provider_event_id=?"
      [PersistText (Event.pecProviderEventId creation)] :: SqlPersistT IO [Single Int64]) pool
    counts `shouldBe` [Single 0]

notificationProviders :: [Checkout.PaymentProvider]
notificationProviders = [Checkout.ProviderPayPal, Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone]

privateMarker :: Text
privateMarker = "SYNTHETIC-PRIVATE-CARD-CUSTOMER-TOKEN"

notificationTime :: UTCTime
notificationTime = UTCTime (fromGregorian 2026 9 14) (12 * 60 * 60)

encodeStrict :: A.Value -> BS.ByteString
encodeStrict = BL.toStrict . A.encode

paypalNotification :: Text -> Text -> Text -> BS.ByteString
paypalNotification eventId amount privateValue = encodeStrict $ A.object
  [ "id" A..= eventId, "event_type" A..= ("PAYMENT.CAPTURE.COMPLETED" :: Text)
  , "create_time" A..= notificationTime, "payer" A..= privateValue
  , "resource" A..= A.object
      [ "id" A..= ("CAPTURE-1" :: Text), "status" A..= ("COMPLETED" :: Text)
      , "amount" A..= A.object ["value" A..= amount, "currency_code" A..= ("USD" :: Text)
          , "card" A..= privateValue]
      , "payee" A..= A.object ["merchant_id" A..= ("MERCHANT-1" :: Text), "email" A..= privateValue]
      , "supplementary_data" A..= A.object ["related_ids" A..= A.object
          ["order_id" A..= ("ORDER-1" :: Text), "token" A..= privateValue], "payer" A..= privateValue]
      , "payment_source" A..= A.object ["card" A..= privateValue]
      , "links" A..= [privateValue], "description" A..= privateValue ] ]

placeToPayNotification :: Text -> BS.ByteString
placeToPayNotification privateValue = encodeStrict $ A.object
  [ "requestId" A..= (1234 :: Int), "reference" A..= privateValue
  , "signature" A..= ("sha256:" <> digestText "1234APPROVED2026-09-14T12:00:00Zsynthetic-secret")
  , "status" A..= A.object ["status" A..= ("APPROVED" :: Text)
      , "date" A..= ("2026-09-14T12:00:00Z" :: Text), "message" A..= privateValue]
  , "card" A..= privateValue ]

notificationPayload :: Checkout.PaymentProvider -> Text -> Text -> BS.ByteString
notificationPayload provider eventId privateValue = case provider of
  Checkout.ProviderPayPal -> paypalNotification eventId "125.15" privateValue
  Checkout.ProviderPlaceToPay -> placeToPayNotification privateValue
  _ -> encodeStrict $ A.object
    [ "TransactionId" A..= (1234 :: Int), "ClientTransactionId" A..= ("CLIENT-1" :: Text)
    , "StoreId" A..= ("synthetic-store" :: Text), "StatusCode" A..= (3 :: Int)
    , "customer" A..= privateValue ]

newNotification :: Checkout.PaymentProvider -> IO Event.ProviderEventCreation
newNotification provider = do
  eventId <- ("EVENT-" <>) . toText <$> nextRandom
  pure Event.ProviderEventCreation
    { Event.pecProvider = provider, Event.pecEnvironment = Checkout.CheckoutSandbox
    , Event.pecMerchantRef = "synthetic-inbox-merchant", Event.pecProviderEventId = eventId
    , Event.pecEventType = if provider == Checkout.ProviderPayPal
        then "PAYMENT.CAPTURE.COMPLETED" else "PAYMENT_NOTIFICATION"
    , Event.pecProviderCreatedAt = if provider == Checkout.ProviderPayPal then Just notificationTime else Nothing
    , Event.pecProviderResource = Just (if provider == Checkout.ProviderPayPal then "CAPTURE-1" else "1234")
    , Event.pecRawPayload = notificationPayload provider eventId privateMarker
    , Event.pecEncryptionKey = recoveryEncryptionKey, Event.pecReceivedAt = notificationTime }

storeNotification :: ConnectionPool -> Event.ProviderEventCreation -> IO (Either Text Event.ProviderEventStored)
storeNotification pool creation = runSqlPool
  ((if Event.pecProvider creation == Checkout.ProviderPayPhone
      then Event.storeUntrustedProviderEvent else Event.storeVerifiedProviderEvent) creation) pool

readStoredNotification :: ConnectionPool -> Event.ProviderEventStored -> IO BS.ByteString
readStoredNotification pool stored = do
  rows <- runSqlPool (rawSql
    "SELECT pgp_sym_decrypt_bytea(payload_ciphertext, ?) FROM commerce_provider_event_inbox WHERE id=?::uuid"
    [PersistText recoveryEncryptionKey, PersistText (Event.providerEventReferenceId (Event.pesReference stored))]
    :: SqlPersistT IO [Single BS.ByteString]) pool
  case rows of
    [Single value] -> pure value
    _ -> expectationFailure "Expected one encrypted inbox row" >> pure BS.empty

insertHistoricalNotification :: ConnectionPool -> Event.ProviderEventCreation -> IO Event.ProviderEventReference
insertHistoricalNotification pool creation = do
  eventId <- toText <$> nextRandom
  let verified = Event.pecProvider creation /= Checkout.ProviderPayPhone
  runSqlPool (rawExecute
    "INSERT INTO commerce_provider_event_inbox (id,provider,environment,merchant_account_ref,\
    \provider_event_id,event_type,signature_verified,evidence_type,provider_created_at,provider_resource_id,\
    \payload_ciphertext,payload_sha256) VALUES (?::uuid,?,'sandbox',?,?,?,?,?,?,?,\
    \pgp_sym_encrypt_bytea(?::bytea,?,'cipher-algo=aes256,compress-algo=1'),?)"
    [ PersistText eventId, PersistText (Checkout.paymentProviderText (Event.pecProvider creation))
    , PersistText (Event.pecMerchantRef creation), PersistText (Event.pecProviderEventId creation)
    , PersistText (Event.pecEventType creation), PersistBool verified
    , PersistText (if verified then "signature_verified" else "untrusted_callback")
    , maybe PersistNull PersistUTCTime (Event.pecProviderCreatedAt creation)
    , maybe PersistNull PersistText (Event.pecProviderResource creation)
    , PersistByteString (Event.pecRawPayload creation), PersistText recoveryEncryptionKey
    , PersistText (digestText (TE.decodeUtf8 (Event.pecRawPayload creation))) ]) pool
  pure (Event.ProviderEventReference eventId)

recoveryEncryptionKey :: Text
recoveryEncryptionKey = "synthetic-provider-recovery-encryption-key"

-- Clear provider authentication before invoking handlers. Restore caller
-- configuration without printing it, even when an assertion fails.
withRecoveryEnvironment :: IO a -> IO a
withRecoveryEnvironment action = bracket
  (forM names $ \name -> (,) name <$> lookupEnv name)
  (mapM_ (\(name, value) -> maybe (unsetEnv name) (setEnv name) value)) $ \_ -> do
    setEnv "COMMERCE_EVENT_ENCRYPTION_KEY" (T.unpack recoveryEncryptionKey)
    setEnv "PLACETOPAY_LOGIN" ""
    setEnv "PAYPHONE_TOKEN" ""
    action
  where names = ["COMMERCE_EVENT_ENCRYPTION_KEY", "PLACETOPAY_LOGIN", "PAYPHONE_TOKEN"]

digestText :: Text -> Text
digestText value = TE.decodeUtf8
  (BAE.convertToBase BAE.Base16 (hash (TE.encodeUtf8 value) :: Digest SHA256))

replayFixture :: ConnectionPool -> Checkout.PaymentProvider
  -> IO (Checkout.PaymentAttemptCreation, Execution.ProviderOperationRecord, PaymentSessionCreateDTO)
replayFixture pool provider = do
  seed <- newCheckout pool
  let creation = seed { Checkout.pacProvider = provider }
      checkoutId = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
      method = if provider == Checkout.ProviderPayPhone then MethodPayPhoneWallet else MethodCard
      phone = if provider == Checkout.ProviderPayPhone then Just "991234567" else Nothing
      country = if provider == Checkout.ProviderPayPhone then Just "593" else Nothing
      request = PaymentSessionCreateDTO (Checkout.paymentProviderText provider)
        (if provider == Checkout.ProviderPayPhone then "payphone_wallet" else "card") phone country
      -- Exercise pre-upgrade checkout-derived references, not only new UUID refs.
      reference = providerReference provider checkoutId
  runSqlPool (rawExecute
    "UPDATE commerce_checkout_session SET lookup_token_hash=? WHERE id=?::uuid"
    [PersistText (digestText checkoutId), PersistText checkoutId]) pool
  attempt <- runSqlPool (Runtime.beginPaymentAttemptForMethod method creation) pool >>= requireRight
  operation <- runSqlPool (Execution.prepareProviderOperation Execution.ProviderOperationPreparation
    { Execution.popAttempt = attempt, Execution.popProvider = provider
    , Execution.popEnvironment = Checkout.CheckoutSandbox
    , Execution.popMerchantRef = Checkout.pacMerchantRef creation
    , Execution.popProviderReference = reference, Execution.popOperation = AdapterCreate
    , Execution.popIdempotencyKey = Checkout.pacIdempotencyKey creation
    -- Reconstruct the OLD persisted byte format independently of the new helper.
    , Execution.popRequestSha256 = digestText (T.intercalate "|"
        [checkoutId, Checkout.paymentProviderText provider, pscPaymentMethod request
        , pscPaymentMethod request, reference, "12515", "USD"
        , maybe "" id phone, maybe "" id country])
    , Execution.popOccurredAt = Checkout.pacCreatedAt creation
    }) pool >>= requireRight
  pure (creation, operation, request)

closeCheckout :: ConnectionPool -> Checkout.PaymentAttemptCreation -> Text -> IO ()
closeCheckout pool creation status = runSqlPool (rawExecute
  "UPDATE commerce_checkout_session SET status=?,expires_at=NOW()-INTERVAL '1 hour'\
  \ WHERE id=?::uuid"
  [PersistText status, PersistText (Checkout.checkoutReferenceId (Checkout.pacCheckout creation))]) pool

-- Synthetic stored provider outcomes, not remote sandbox results.
setOperationState :: ConnectionPool -> Execution.ProviderOperationRecord -> Text -> Text -> IO ()
setOperationState pool operation status certainty = runSqlPool (rawExecute
  "UPDATE commerce_provider_operation SET status=?,outcome_certainty=?,started_at=NOW(),\
  \ provider_resource_id='synthetic-recovery',redirect_url_ciphertext=pgp_sym_encrypt(?,?)\
  \ WHERE id=?::uuid"
  [PersistText status, PersistText certainty
  , PersistText "https://checkout-test.placetopay.ec/session/synthetic-recovery"
  , PersistText recoveryEncryptionKey
  , PersistText (Execution.providerOperationReferenceId (Execution.porReference operation))]) pool

replayHandler :: ConnectionPool -> Checkout.PaymentAttemptCreation -> PaymentSessionCreateDTO
  -> IO (Either ServerError PaymentSessionDTO)
replayHandler pool creation request = do
  let create :<|> _ = providerExecutionServer
      checkoutId = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
      env = Env pool (error "Recovery must not use AppConfig")
  runHandler (runReaderT (create checkoutId (Just checkoutId)
    (Just (Checkout.pacIdempotencyKey creation)) (Just "Synthetic provider recovery test")
    (SockAddrInet 0 0) request) env)

assertHttpError :: Int -> Either ServerError PaymentSessionDTO -> Expectation
assertHttpError status = either ((`shouldBe` status) . errHTTPCode)
  (const (expectationFailure "Expected payment recovery to fail closed"))

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
