{-# LANGUAGE OverloadedStrings #-}

-- Real PostgreSQL persistence/concurrency tests; no provider HTTP or credentials.
module TDF.Commerce.ProviderRetrySpec (spec) where

import           Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar, threadDelay)
import           Control.Exception (AsyncException(..), IOException, bracket, throwIO, toException, try)
import           Control.Monad (forM, forM_, unless)
import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT)
import           Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Either (isLeft, isRight, rights)
import           Data.Int (Int64)
import           Data.IORef (newIORef, readIORef, atomicModifyIORef', modifyIORef')
import           Data.Pool (destroyAllResources)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime(..), addUTCTime, diffUTCTime, fromGregorian, getCurrentTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool, toSqlKey)
import           Database.PostgreSQL.Simple (SqlError(..))
import           Network.Socket (SockAddr(..))
import           Numeric (readHex)
import qualified Network.HTTP.Client as HC
import           Servant (NoContent, ServerError, errHTTPCode, errBody, errHeaders, runHandler, getResponse, getHeaders, (:<|>)(..))
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import qualified System.Timeout as Timeout
import           Test.Hspec

import qualified TDF.Commerce.CheckoutStore as Checkout
import           TDF.API.CommerceOperations
import           TDF.Auth (AuthedUser(..))
import           TDF.Models (RoleEnum(..))
import qualified TDF.Server.CommerceOperations as Operations
import           TDF.API.ProviderExecution (PaymentSessionCreateDTO(..), PaymentSessionDTO(..))
import           TDF.DB (Env(..))
import qualified TDF.Commerce.PaymentIntentStore as Intent
import qualified TDF.Commerce.PaymentRuntimeStore as Runtime
import           TDF.Commerce.ProviderAdapter (AdapterOperation(..))
import qualified TDF.Commerce.ProviderAdapter as Adapter
import qualified TDF.Commerce.ProviderAdapter.Http as ProviderHttp
import qualified TDF.Commerce.ProviderAdapter.PayPhone as PayPhone
import qualified TDF.Commerce.ProviderAdapter.PlaceToPay as PlaceToPay
import           TDF.Commerce.ProviderCapabilities (PaymentMethod(..), ProviderOutcomeCertainty(..))
import qualified TDF.Commerce.ProviderExecutionStore as Execution
import qualified TDF.Commerce.ProviderEventStore as Event
import qualified TDF.Commerce.ProviderEventWorker as EventWorker
import qualified TDF.Commerce.ProviderReconciliation as Reconciliation
import           TDF.Commerce.StateMachine (PaymentEvent(..))
import           TDF.Server.ProviderExecution (providerReference, providerExecutionServer)
import qualified TDF.Server.ServiceStorefront as Storefront

-- In-memory HTTP connection fixtures. These exercise the real HTTP executor,
-- not a real provider, TLS handshake, socket, merchant account or sandbox.
providerTransportSpec :: Spec
providerTransportSpec = describe "provider HTTP transport boundary" $ do
  it "allows configured provider destinations and pins redirects/header timeout" $ do
    forM_ transportOrigins $ \(provider, origin) -> do
      request <- ProviderHttp.parseProviderRequest provider (origin <> "/api/test") >>= requireRight
      HC.redirectCount request `shouldBe` 0
      HC.responseTimeout request `shouldBe` HC.responseTimeoutMicro 15000000
      HC.cookieJar request `shouldSatisfy` maybe True (const False)

  it "rejects spoofed hosts, URL credentials, explicit ports and unsafe URLs" $ do
    forM_ ["http://api-m.sandbox.paypal.com/v2/orders"
      , "https://api-m.sandbox.paypal.com.attacker.invalid/v2/orders"
      , "https://user:synthetic-private@api-m.sandbox.paypal.com/v2/orders"
      , "https://api-m.sandbox.paypal.com:443/v2/orders"
      , "https://api-m.sandbox.paypal.com/v2/orders#synthetic-private"
      , "https://api-m.sandbox.paypal.com\\@attacker.invalid/v2/orders"
      , "https://api-m.sandbox.paypal.com/v2/\nsynthetic-private"
      , "https://127.0.0.1/v2/orders", "not a URL synthetic-private"] $ \url -> do
        result <- ProviderHttp.parseProviderRequest Checkout.ProviderPayPal url
        result `shouldSatisfy` isLeft
        either show (const "unexpected accepted request") result
          `shouldNotContain` "synthetic-private"
    ProviderHttp.parseProviderRequest Checkout.ProviderDatafast
      "https://test.oppwa.com.attacker.invalid/v1/checkouts" >>= (`shouldSatisfy` isLeft)
    ProviderHttp.parseProviderRequest Checkout.ProviderPayPhone
      "https://api-m.sandbox.paypal.com/v2/orders" >>= (`shouldSatisfy` isLeft)

  it "revalidates the parsed destination and rejects Host overrides or invalid UTF8" $ do
    request <- paypalTransportRequest
    forM_ [request { HC.secure = False }, request { HC.port = 8443 }
      , request { HC.host = "attacker.invalid" }, request { HC.host = BS.pack ['\255'] }
      , request { HC.requestHeaders = [("Host", "attacker.invalid")] }] $ \altered ->
        ProviderHttp.prepareProviderRequest Checkout.ProviderPayPal altered `shouldSatisfy` isLeft

  it "preserves form/JSON bytes, authorization and stable idempotency headers" $ do
    request <- paypalTransportRequest
    let original = request { HC.method = "POST"
          , HC.requestHeaders = [("Authorization", "Bearer synthetic-token")
              , ("PayPal-Request-Id", "synthetic-stable-request")]
          , HC.requestBody = HC.RequestBodyBS "grant_type=client_credentials"
          , HC.redirectCount = 10, HC.responseTimeout = HC.responseTimeoutNone }
    prepared <- requireRight (ProviderHttp.prepareProviderRequest Checkout.ProviderPayPal original)
    HC.method prepared `shouldBe` "POST"
    HC.requestHeaders prepared `shouldBe` HC.requestHeaders original
    case HC.requestBody prepared of
      HC.RequestBodyBS bytes -> bytes `shouldBe` "grant_type=client_credentials"
      _ -> expectationFailure "Form encoding changed"
    HC.redirectCount prepared `shouldBe` 0

  it "accepts exactly one MiB of JSON without rounding or altering decimal strings" $ do
    let json = "\"" <> BS.replicate (1024 * 1024 - 2) 'a' <> "\""
    reader <- chunkReader [BS.take 1024 json, BS.drop 1024 json]
    value <- ProviderHttp.readProviderResponse 200 reader :: IO (Either ProviderHttp.AdapterTransportError A.Value)
    value `shouldBe` Right (A.String (T.replicate (1024 * 1024 - 2) "a"))
    reader2 <- chunkReader ["{\"amount\":{\"value\":\"125.15\",\"currency_code\":\"USD\"}}"]
    result <- ProviderHttp.readProviderResponse 201 reader2 :: IO (Either ProviderHttp.AdapterTransportError A.Value)
    result `shouldBe` Right (A.object ["amount" A..= A.object
      ["value" A..= ("125.15" :: Text), "currency_code" A..= ("USD" :: Text)]])

  it "stops reading at the first oversized chunk" $ do
    readCount <- newIORef (0 :: Int)
    reader <- chunkReader [BS.replicate (1024 * 1024) ' ', "x", "must-not-be-read"]
    result <- ProviderHttp.readProviderResponse 200 (modifyIORef' readCount (+1) >> reader)
      :: IO (Either ProviderHttp.AdapterTransportError A.Value)
    result `shouldSatisfy` isLeft
    readIORef readCount `shouldReturn` 2

  it "does not consume redirect, rejection or server-error response bodies" $ do
    forM_ [301, 302, 307, 308, 400, 401, 409, 422, 429, 500, 503] $ \code -> do
      result <- ProviderHttp.readProviderResponse code
        (fail "Non-2xx response body must never be consumed")
        :: IO (Either ProviderHttp.AdapterTransportError A.Value)
      result `shouldSatisfy` isLeft

  it "redacts malformed JSON and typed parser errors" $ do
    forM_ ["{synthetic-private-invalid-json", "\"synthetic-private-token\""] $ \body -> do
      reader <- chunkReader [body]
      result <- ProviderHttp.readProviderResponse 200 reader
        :: IO (Either ProviderHttp.AdapterTransportError Storefront.ServiceDatafastPaymentStatus)
      result `shouldSatisfy` isLeft
      show result `shouldNotContain` "synthetic-private"

  it "uses the same real executor for all four provider destinations" $ do
    forM_ transportOrigins $ \(provider, origin) -> do
      reader <- chunkReader [jsonWire "{\"ok\":true}"]
      withProviderWire reader $ \manager connections _ _ -> do
        request <- ProviderHttp.parseProviderRequest provider (origin <> "/api/test") >>= requireRight
        result <- ProviderHttp.executeProviderRequest manager provider request
          :: IO (Either ProviderHttp.AdapterTransportError A.Value)
        result `shouldBe` Right (A.object ["ok" A..= True])
        connections `shouldReturn` 1

  it "rejects a changed destination before opening any connection" $ do
    withProviderWire (fail "No network allowed") $ \manager connections _ _ -> do
      request <- paypalTransportRequest
      result <- ProviderHttp.executeProviderRequest manager Checkout.ProviderPayPal
        request { HC.host = "attacker.invalid" }
        :: IO (Either ProviderHttp.AdapterTransportError A.Value)
      result `shouldSatisfy` isLeft
      connections `shouldReturn` 0

  it "does not follow a credential-bearing POST redirect" $ do
    reader <- chunkReader ["HTTP/1.1 307 Temporary Redirect\r\nLocation: https://attacker.invalid/collect\r\nContent-Length: 0\r\n\r\n"]
    withProviderWire reader $ \manager connections writes _ -> do
      request <- paypalTransportRequest
      result <- ProviderHttp.executeProviderRequest manager Checkout.ProviderPayPal request
        { HC.method = "POST", HC.requestHeaders = [("Authorization", "Bearer synthetic-token")]
        , HC.requestBody = HC.RequestBodyBS "{}" }
        :: IO (Either ProviderHttp.AdapterTransportError A.Value)
      result `shouldSatisfy` isLeft
      connections `shouldReturn` 1
      sent <- writes
      sent `shouldNotSatisfy` BS.isInfixOf "attacker.invalid"

  it "redacts network parser exceptions and closes the failed response" $ do
    reader <- chunkReader ["synthetic-private-invalid-http\r\n\r\n"]
    withProviderWire reader $ \manager _ _ closes -> do
      request <- paypalTransportRequest
      result <- ProviderHttp.executeProviderRequest manager Checkout.ProviderPayPal request
        :: IO (Either ProviderHttp.AdapterTransportError A.Value)
      result `shouldSatisfy` isLeft
      show result `shouldNotContain` "synthetic-private"
      closes >>= (`shouldSatisfy` (> 0))

  it "disables stale-connection retries even for ambiguous POST results" $ do
    HC.managerRetryableException ProviderHttp.providerManagerSettings
      (toException (userError "synthetic-private-error")) `shouldBe` False
    reader <- chunkReader [jsonWire "{}"]
    withProviderWire reader $ \manager connections writes _ -> do
      request <- paypalTransportRequest
      _ <- (ProviderHttp.executeProviderRequest manager Checkout.ProviderPayPal request
        :: IO (Either ProviderHttp.AdapterTransportError A.Value)) >>= requireRight
      result <- ProviderHttp.executeProviderRequest manager Checkout.ProviderPayPal request
        { HC.method = "POST", HC.requestBody = HC.RequestBodyBS "{}" }
        :: IO (Either ProviderHttp.AdapterTransportError A.Value)
      result `shouldSatisfy` isLeft
      connections `shouldReturn` 1
      sent <- writes
      length (filter (BS.isPrefixOf "POST ") (BS.lines sent)) `shouldBe` 1

  it "bounds a body stalled after valid headers and closes its connection" $ do
    first <- newIORef True
    let reader = do
          initial <- atomicModifyIORef' first (\value -> (False, value))
          if initial then pure "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n"
            else threadDelay 30000000 >> pure "{}"
    withProviderWire reader $ \manager _ _ closes -> do
      request <- paypalTransportRequest
      started <- getCurrentTime
      result <- Timeout.timeout 20000000
        (ProviderHttp.executeProviderRequest manager Checkout.ProviderPayPal request
          :: IO (Either ProviderHttp.AdapterTransportError A.Value))
      result `shouldSatisfy` maybe False isLeft
      finished <- getCurrentTime
      diffUTCTime finished started `shouldSatisfy` (>= 14)
      closes >>= (`shouldSatisfy` (> 0))

  it "redacts decompression failures without throwing provider bytes" $ do
    let body = "synthetic-private-invalid-gzip"
    reader <- chunkReader ["HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\nContent-Length: "
      <> BS.pack (show (BS.length body)) <> "\r\n\r\n" <> body]
    withProviderWire reader $ \manager _ _ _ -> do
      request <- paypalTransportRequest
      result <- ProviderHttp.executeProviderRequest manager Checkout.ProviderPayPal request
        :: IO (Either ProviderHttp.AdapterTransportError A.Value)
      result `shouldBe` Left (ProviderHttp.AdapterTransportError
        "Payment provider is temporarily unavailable; reconcile before retrying.")
      show result `shouldNotContain` "synthetic-private"

  it "routes legacy PayPal order creation through the executor with stable request IDs" $ do
    let created = "{\"id\":\"SYNTHETIC-ORDER\",\"links\":[{\"rel\":\"approve\",\"href\":\"https://www.sandbox.paypal.com/checkoutnow?token=SYNTHETIC-ORDER\"}]}"
    reader <- chunkReader [jsonWire oauthFixture, jsonWire created, jsonWire oauthFixture, jsonWire created]
    withProviderWire reader $ \manager _ writes _ -> do
      let create = runHandler $ runReaderT
            (Storefront.createPaypalOrderRemoteForService manager "synthetic-client" "synthetic-secret"
              "https://api-m.sandbox.paypal.com" "synthetic-internal-order" 12515 "USD"
              "Synthetic Buyer" "buyer@example.invalid")
            (error "Remote helper must not access the database")
      first <- create
      second <- create
      first `shouldSatisfy` isRight
      second `shouldBe` first
      sent <- writes
      sent `shouldSatisfy` BS.isInfixOf "\"value\":\"125.15\""
      let keys = filter (BS.isPrefixOf "PayPal-Request-Id:") (BS.lines sent)
      length keys `shouldBe` 2
      case keys of
        [one, two] -> one `shouldBe` two
        _ -> expectationFailure "Missing stable PayPal request IDs"

  it "rejects unsafe PayPal OAuth tokens before sending a financial request" $ do
    forM_ ["{\"access_token\":\"synthetic-private\\r\\nHeader: x\",\"token_type\":\"Bearer\"}"
      , "{\"access_token\":\"synthetic-private\",\"token_type\":\"Basic\"}"] $ \oauth -> do
        reader <- chunkReader [jsonWire oauth]
        withProviderWire reader $ \manager _ writes _ -> do
          result <- runHandler $ runReaderT
            (Storefront.capturePaypalOrderRemoteForService manager "synthetic-client" "synthetic-secret"
              "https://api-m.sandbox.paypal.com" "SYNTHETIC-ORDER")
            (error "Remote helper must not access the database")
          result `shouldSatisfy` isLeft
          either (BL.toStrict . errBody) (const "unexpected success") result
            `shouldNotSatisfy` BS.isInfixOf "synthetic-private"
          sent <- writes
          sent `shouldNotSatisfy` BS.isInfixOf "/capture"

  it "preserves capture binding fields and the existing capture idempotency header" $ do
    let captured = BL.toStrict $ A.encode $ A.object
          [ "purchase_units" A..= [A.object
              [ "custom_id" A..= ("synthetic-internal-order" :: Text)
              , "payee" A..= A.object ["merchant_id" A..= ("synthetic-merchant" :: Text)]
              , "payments" A..= A.object ["captures" A..= [A.object
                  [ "id" A..= ("SYNTHETIC-CAPTURE" :: Text)
                  , "status" A..= ("COMPLETED" :: Text)
                  , "amount" A..= A.object ["value" A..= ("125.15" :: Text)
                      , "currency_code" A..= ("USD" :: Text)] ]]] ]]]
    reader <- chunkReader [jsonWire oauthFixture, jsonWire captured]
    withProviderWire reader $ \manager _ writes _ -> do
      outcome <- runHandler (runReaderT
        (Storefront.capturePaypalOrderRemoteForService manager "synthetic-client" "synthetic-secret"
          "https://api-m.sandbox.paypal.com" "SYNTHETIC-ORDER")
        (error "Remote helper must not access the database")) >>= requireRight
      Storefront.validatePaypalSuccessfulCapture "synthetic-internal-order" 12515 "USD"
        "synthetic-merchant" outcome `shouldBe` Right ()
      sent <- writes
      sent `shouldSatisfy` BS.isInfixOf "POST /v2/checkout/orders/SYNTHETIC-ORDER/capture"
      sent `shouldSatisfy` BS.isInfixOf "PayPal-Request-Id: capture-"

  it "keeps generic provider response errors redacted at the legacy API boundary" $ do
    reader <- chunkReader [jsonWire "synthetic-private-invalid-json"]
    withProviderWire reader $ \manager _ _ _ -> do
      request <- paypalTransportRequest
      result <- runHandler $ runReaderT
        (Storefront.providerResponse manager Checkout.ProviderPayPal request)
        (error "Transport helper must not access the database")
        :: IO (Either ServerError A.Value)
      result `shouldSatisfy` isLeft
      either errHTTPCode (const 0) result `shouldBe` 502
      either (BL.toStrict . errBody) (const "unexpected success") result
        `shouldNotSatisfy` BS.isInfixOf "synthetic-private"

transportOrigins :: [(Checkout.PaymentProvider, String)]
transportOrigins =
  [ (Checkout.ProviderPayPal, "https://api-m.sandbox.paypal.com")
  , (Checkout.ProviderPayPal, "https://api-m.paypal.com")
  , (Checkout.ProviderDatafast, "https://test.oppwa.com")
  , (Checkout.ProviderDatafast, "https://eu-prod.oppwa.com")
  , (Checkout.ProviderPlaceToPay, "https://checkout-test.placetopay.ec")
  , (Checkout.ProviderPlaceToPay, "https://checkout.placetopay.ec")
  , (Checkout.ProviderPayPhone, "https://pay.payphonetodoesposible.com")
  ]

paypalTransportRequest :: IO HC.Request
paypalTransportRequest = ProviderHttp.parseProviderRequest Checkout.ProviderPayPal
  "https://api-m.sandbox.paypal.com/v2/checkout/orders" >>= requireRight

chunkReader :: [BS.ByteString] -> IO HC.BodyReader
chunkReader chunks = do
  remaining <- newIORef chunks
  pure $ atomicModifyIORef' remaining $ \current -> case current of
    [] -> ([], BS.empty)
    part : rest -> (rest, part)

jsonWire :: BS.ByteString -> BS.ByteString
jsonWire body = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: "
  <> BS.pack (show (BS.length body)) <> "\r\n\r\n" <> body

oauthFixture :: BS.ByteString
oauthFixture = "{\"access_token\":\"synthetic-access-token\",\"token_type\":\"Bearer\"}"

withProviderWire
  :: HC.BodyReader
  -> (HC.Manager -> IO Int -> IO BS.ByteString -> IO Int -> IO a)
  -> IO a
withProviderWire reader action = do
  connections <- newIORef (0 :: Int)
  writes <- newIORef []
  closes <- newIORef (0 :: Int)
  let connect _ _ _ = do
        modifyIORef' connections (+1)
        HC.makeConnection reader (\bytes -> modifyIORef' writes (bytes :))
          (modifyIORef' closes (+1))
      settings = HC.managerSetProxy HC.noProxy ProviderHttp.providerManagerSettings
        { HC.managerTlsConnection = pure connect
        , HC.managerRawConnection = pure connect }
  bracket (HC.newManager settings) HC.closeManager $ \manager ->
    action manager (readIORef connections) (BS.concat . reverse <$> readIORef writes) (readIORef closes)

spec :: Spec
spec = do
  reconciliationReportValidationSpec
  providerQueryReportValidationSpec
  providerTransportSpec
  notificationMinimizationSpec
  notificationIdentitySpec
  configured <- runIO (lookupEnv "TDF_PROVIDER_RETRY_DATABASE_URL")
  case configured of
    Nothing -> pure ()
    Just url -> beforeAll (openDatabase url) $ afterAll destroyAllResources $
      describe "provider-retry-runtime PostgreSQL" $ do
        reconciliationReportSpec
        providerQueryReportSpec
        queryRecoverySpec
        noChargeReplaySpec
        notificationInboxSpec
        notificationIdentityInboxSpec
        reconciliationTransactionSpec
        closedCheckoutEvidenceSpec
        captureReplaySpec
        manualCaptureReplaySpec
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

notificationIdentitySpec :: Spec
notificationIdentitySpec = describe "provider PlaceToPay signed notification identity" $ do
  it "pins the v2 array encoding with an independently calculated SHA-256 golden" $
    Event.placeToPayNotificationEventId (placeToPayNotification privateMarker)
      `shouldBe` Right "ptp-v2-59b4f46382c7696e27c2062275a40c2ae1970a83c035853876fb48b1f436c35d"

  it "ignores unsigned fields, whitespace, numeric spelling, escaped keys and signature casing" $ do
    let expected = Event.placeToPayNotificationEventId (placeToPayNotification privateMarker)
    forM_ placeToPayReplayVariants $ \raw -> do
      Event.placeToPayNotificationEventId raw `shouldBe` expected
      value <- requireRight (A.eitherDecodeStrict' raw)
      PlaceToPay.verifyPlaceToPayNotification notificationConfig value `shouldSatisfy` isRight

  it "keeps different signed request IDs, statuses, dates and signatures distinct" $ do
    let original = Event.placeToPayNotificationEventId (placeToPayNotification privateMarker)
    forM_ [signedPlaceToPayNotification 1235 "APPROVED" "2026-09-14T12:00:00Z" "synthetic-secret" ""
      , signedPlaceToPayNotification 1234 "REJECTED" "2026-09-14T12:00:00Z" "synthetic-secret" ""
      , signedPlaceToPayNotification 1234 "APPROVED" "2026-09-14T12:00:01Z" "synthetic-secret" ""
      , signedPlaceToPayNotification 1234 "APPROVED" "2026-09-14T12:00:00Z" "rotated-synthetic-secret" ""] $ \raw -> do
        Event.placeToPayNotificationEventId raw `shouldSatisfy` isRight
        Event.placeToPayNotificationEventId raw `shouldNotBe` original

  it "does not merge different field tuples even if provider concatenation is identical" $ do
    let first = signedPlaceToPayNotification 12 "34APPROVED" "2026-09-14T12:00:00Z" "synthetic-secret" ""
    Event.placeToPayNotificationEventId first
      `shouldNotBe` Event.placeToPayNotificationEventId (placeToPayNotification "")

  it "rejects invalid identity shapes and redacts payload details" $
    forM_ ["[]", "{}", "{\"SYNTHETIC-PRIVATE\":", BS.replicate (1024 * 1024 + 1) 'x'
      , signedPlaceToPayNotification (-1) "APPROVED" "2026-09-14T12:00:00Z" "synthetic-secret" ""
      , signedPlaceToPayNotification 1234 "APPROVED" "not-a-date" "synthetic-secret" ""] $ \raw -> do
        let result = Event.placeToPayNotificationEventId raw
        result `shouldSatisfy` isLeft
        show result `shouldNotContain` "SYNTHETIC-PRIVATE"

  it "never treats successful identity derivation as authentication" $ do
    let raw = signedPlaceToPayNotification 1234 "APPROVED" "2026-09-14T12:00:00Z" "wrong-secret" ""
    Event.placeToPayNotificationEventId raw `shouldSatisfy` isRight
    value <- requireRight (A.eitherDecodeStrict' raw)
    PlaceToPay.verifyPlaceToPayNotification notificationConfig value `shouldSatisfy` isLeft

notificationIdentityInboxSpec :: SpecWith ConnectionPool
notificationIdentityInboxSpec = describe "PlaceToPay signed identity inbox" $ do
  it "refuses to bypass signed deduplication through the untrusted store" $ \pool -> do
    creation <- newNotification Checkout.ProviderPlaceToPay
    runSqlPool (Event.storeUntrustedProviderEvent creation) pool >>= (`shouldSatisfy` isLeft)
    notificationRowCount pool creation `shouldReturn` 0

  it "converges concurrent raw-body and caller-ID variations on one row and one claim" $ \pool -> do
    creation <- newNotification Checkout.ProviderPlaceToPay
    results <- concurrently
      [storeNotification pool creation { Event.pecRawPayload = raw
        , Event.pecProviderEventId = "CALLER-" <> T.pack (show n) }
      | (n, raw) <- zip [1 :: Int ..] placeToPayReplayVariants] >>= mapM requireRight
    length (filter Event.pesInserted results) `shouldBe` 1
    first <- case results of
      result : _ -> pure result
      [] -> fail "Expected concurrent notification results"
    map Event.pesReference results `shouldSatisfy` all (== Event.pesReference first)
    notificationRowCount pool creation `shouldReturn` 1
    claims <- concurrently (replicate 4 (runSqlPool
      (Event.claimProviderEvent (Event.pesReference first) notificationTime) pool))
    length [() | Event.ProviderEventClaimed _ <- claims] `shouldBe` 1

  it "does not requeue a processed canonical notification" $ \pool -> do
    creation <- newNotification Checkout.ProviderPlaceToPay
    stored <- storeNotification pool creation >>= requireRight
    _ <- runSqlPool (Event.claimProviderEvent (Event.pesReference stored) notificationTime) pool
    runSqlPool (Event.markProviderEventProcessed (Event.pesReference stored)
      Nothing Nothing Nothing notificationTime) pool
    replay <- storeNotification pool creation { Event.pecRawPayload = last placeToPayReplayVariants }
      >>= requireRight
    Event.pesReference replay `shouldBe` Event.pesReference stored
    Event.pesInserted replay `shouldBe` False
    runSqlPool (Event.claimProviderEvent (Event.pesReference replay) notificationTime) pool
      `shouldReturn` Event.ProviderEventAlreadyHandled "processed"

  it "scopes deduplication by merchant and environment" $ \pool -> do
    creation <- newNotification Checkout.ProviderPlaceToPay
    results <- mapM (storeNotification pool)
      [creation, creation { Event.pecMerchantRef = Event.pecMerchantRef creation <> "-other" }
      , creation { Event.pecEnvironment = Checkout.CheckoutProduction }] >>= mapM requireRight
    map Event.pesInserted results `shouldBe` [True, True, True]

  it "retains separate out-of-order signed states for authoritative reconciliation" $ \pool -> do
    creation <- newNotification Checkout.ProviderPlaceToPay
    let changed = creation { Event.pecRawPayload = signedPlaceToPayNotification
          1234 "PENDING" "2026-09-14T11:00:00Z" "synthetic-secret" "" }
    results <- mapM (storeNotification pool) [creation, changed, changed, creation] >>= mapM requireRight
    map Event.pesInserted results `shouldBe` [True, True, False, False]
    notificationRowCount pool creation `shouldReturn` 2

  it "rejects immutable metadata changes on canonical and historical replay" $ \pool -> do
    forM_ [False, True] $ \historical -> do
      creation <- newNotification Checkout.ProviderPlaceToPay
      if historical then insertHistoricalNotification pool creation >> pure ()
        else storeNotification pool creation >>= requireRight >> pure ()
      forM_ [creation { Event.pecProviderResource = Just "OTHER-RESOURCE" }
        , creation { Event.pecEventType = "OTHER_EVENT" }
        , creation { Event.pecProviderCreatedAt = Just notificationTime }] $ \changed ->
          storeNotification pool changed >>= (`shouldSatisfy` isLeft)
      notificationRowCount pool creation `shouldReturn` 1

  it "preserves exact historical uppercase minimized evidence without rewriting it" $ \pool -> do
    creation <- newNotification Checkout.ProviderPlaceToPay
    let raw = uppercasePlaceToPaySignature (Event.pecRawPayload creation)
    -- Independently reconstruct the prior projection, which retained hex casing.
    value <- requireRight (A.eitherDecodeStrict' raw)
    let oldProjection = case value of
          A.Object fields -> encodeStrict (A.Object (KM.filterWithKey
            (\key _ -> key == "requestId" || key == "signature" || key == "status")
            (KM.mapWithKey (\key status -> if key /= "status" then status else case status of
              A.Object details -> A.Object (KM.delete "message" details)
              _ -> status) fields)))
          _ -> error "Expected synthetic object"
        replayCreation = creation { Event.pecRawPayload = raw, Event.pecProviderEventId = legacyPlaceToPayId raw }
    historical <- insertHistoricalNotification pool replayCreation { Event.pecRawPayload = oldProjection }
    replay <- storeNotification pool replayCreation >>= requireRight
    Event.pesReference replay `shouldBe` historical
    Event.pesInserted replay `shouldBe` False
    readStoredNotification pool replay `shouldReturn` oldProjection

  it "bounds reformatted pre-upgrade delivery to one canonical row and preserves the old row" $ \pool -> do
    creation <- newNotification Checkout.ProviderPlaceToPay
    historical <- insertHistoricalNotification pool creation
    results <- concurrently
      [storeNotification pool creation { Event.pecRawPayload = raw }
      | raw <- drop 1 placeToPayReplayVariants] >>= mapM requireRight
    length (filter Event.pesInserted results) `shouldBe` 1
    notificationRowCount pool creation `shouldReturn` 2
    replay <- storeNotification pool creation >>= requireRight
    Event.pesReference replay `shouldBe` historical
    readStoredNotification pool replay `shouldReturn` Event.pecRawPayload creation

  it "authenticates the original callback before identity persistence at the Servant handler" $ \pool ->
    withNotificationEnvironment $ do
      let bad = signedPlaceToPayNotification 991238 "APPROVED" "2026-09-14T12:00:00Z" "wrong-secret" privateMarker
      countBefore <- handlerNotificationCount pool
      failed <- notificationHandler pool bad
      either (\err -> do
        errHTTPCode err `shouldBe` 401
        BL.toStrict (errBody err) `shouldSatisfy` (not . BS.isInfixOf (TE.encodeUtf8 privateMarker)))
        (const (expectationFailure "Forged callback was accepted")) failed
      handlerNotificationCount pool `shouldReturn` countBefore
      let good = signedPlaceToPayNotification 991238 "APPROVED" "2026-09-14T12:00:00Z" "synthetic-secret" privateMarker
      notificationHandler pool good >>= (`shouldSatisfy` isRight)
      notificationHandler pool ("\n " <> uppercasePlaceToPaySignature good <> "\n") >>= (`shouldSatisfy` isRight)
      handlerNotificationCount pool `shouldReturn` (countBefore + 1)

notificationConfig :: PlaceToPay.PlaceToPayConfig
notificationConfig = PlaceToPay.PlaceToPayConfig Checkout.CheckoutSandbox "synthetic-login" "synthetic-secret" []

legacyPlaceToPayId :: BS.ByteString -> Text
legacyPlaceToPayId raw = "ptp-" <> digestText (TE.decodeUtf8 raw)

uppercasePlaceToPaySignature :: BS.ByteString -> BS.ByteString
uppercasePlaceToPaySignature raw = case A.eitherDecodeStrict' raw of
  Right (A.Object fields) -> encodeStrict $ A.Object $ KM.mapWithKey
    (\key value -> if key /= "signature" then value else case value of
      A.String signature -> A.String (T.toUpper signature); _ -> value) fields
  _ -> error "Expected synthetic PlaceToPay object"

placeToPayReplayVariants :: [BS.ByteString]
placeToPayReplayVariants =
  [ placeToPayNotification privateMarker
  , placeToPayNotification "DIFFERENT-SYNTHETIC-PRIVATE"
  , " \n" <> placeToPayNotification "" <> "\n "
  , uppercasePlaceToPaySignature (placeToPayNotification "")
  , TE.encodeUtf8 (T.replace "1234" "1234.0" (TE.decodeUtf8 (placeToPayNotification "")))
  , TE.encodeUtf8 (T.replace "requestId" "request\\u0049d" (TE.decodeUtf8 (placeToPayNotification "")))
  , "{\"status\":{\"date\":\"2026-09-14T12:00:00Z\",\"status\":\"APPROV\\u0045D\"},"
      <> "\"signature\":\"sha256:" <> TE.encodeUtf8 (digestText "1234APPROVED2026-09-14T12:00:00Zsynthetic-secret")
      <> "\",\"requestId\":1234,\"reference\":\"\"}"
  ]

notificationRowCount :: ConnectionPool -> Event.ProviderEventCreation -> IO Int64
notificationRowCount pool creation = do
  rows <- runSqlPool (rawSql
    "SELECT count(*) FROM commerce_provider_event_inbox WHERE provider='placetopay'\
    \ AND merchant_account_ref=?"
    [PersistText (Event.pecMerchantRef creation)] :: SqlPersistT IO [Single Int64]) pool
  case rows of [Single count] -> pure count; _ -> fail "Expected one count"

notificationHandler :: ConnectionPool -> BS.ByteString -> IO (Either ServerError NoContent)
notificationHandler pool raw = do
  let _ :<|> _ :<|> receive :<|> _ = providerExecutionServer
  runHandler (runReaderT (receive (BL.fromStrict raw)) (Env pool (error "Notification must not use AppConfig")))

handlerNotificationCount :: ConnectionPool -> IO Int64
handlerNotificationCount pool = do
  creation <- newNotification Checkout.ProviderPlaceToPay
  notificationRowCount pool creation { Event.pecMerchantRef = "synthetic-provider-retry-merchant" }

withNotificationEnvironment :: IO a -> IO a
withNotificationEnvironment action = bracket
  (forM values $ \(name, _) -> (,) name <$> lookupEnv name)
  (mapM_ (\(name, value) -> maybe (unsetEnv name) (setEnv name) value)) $ \_ -> do
    forM_ values (uncurry setEnv)
    action
  where values =
          [("COMMERCE_CHECKOUT_ENV", "sandbox"), ("COMMERCE_EVENT_ENCRYPTION_KEY", T.unpack recoveryEncryptionKey)
          , ("PLACETOPAY_LOGIN", "synthetic-login"), ("PLACETOPAY_SECRET_KEY", "synthetic-secret")
          , ("PLACETOPAY_RETURN_URL", "https://example.invalid/return")
          , ("PLACETOPAY_NOTIFICATION_URL", "https://example.invalid/notify")
          , ("PLACETOPAY_CARD_PAYMENT_METHODS", ""), ("PLACETOPAY_BANK_PAYMENT_METHODS", "")
          , ("PLACETOPAY_DEUNA_PAYMENT_METHODS", ""), ("PAYPHONE_TOKEN", "synthetic-token")
          , ("PAYPHONE_STORE_ID", "synthetic-store"), ("PAYPHONE_RESPONSE_URL", "https://example.invalid/return")]

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
placeToPayNotification = signedPlaceToPayNotification 1234 "APPROVED" "2026-09-14T12:00:00Z" "synthetic-secret"

signedPlaceToPayNotification :: Int64 -> Text -> Text -> Text -> Text -> BS.ByteString
signedPlaceToPayNotification requestId status date secret privateValue = encodeStrict $ A.object
  [ "requestId" A..= requestId, "reference" A..= privateValue
  , "signature" A..= ("sha256:" <> digestText (T.pack (show requestId) <> status <> date <> secret))
  , "status" A..= A.object ["status" A..= status
      , "date" A..= date, "message" A..= privateValue]
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
  let rawPayload = notificationPayload provider eventId privateMarker
  pure Event.ProviderEventCreation
    { Event.pecProvider = provider, Event.pecEnvironment = Checkout.CheckoutSandbox
    , Event.pecMerchantRef = "synthetic-inbox-merchant-" <> eventId
    , Event.pecProviderEventId = if provider == Checkout.ProviderPlaceToPay
        then legacyPlaceToPayId rawPayload else eventId
    , Event.pecEventType = if provider == Checkout.ProviderPayPal
        then "PAYMENT.CAPTURE.COMPLETED" else "PAYMENT_NOTIFICATION"
    , Event.pecProviderCreatedAt = if provider == Checkout.ProviderPayPal then Just notificationTime else Nothing
    , Event.pecProviderResource = Just (if provider == Checkout.ProviderPayPal then "CAPTURE-1" else "1234")
    , Event.pecRawPayload = rawPayload
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

-- These tests parse synthetic official-contract-shaped query responses and
-- exercise the actual PostgreSQL financial path. They are not sandbox tests.
reconciliationReportValidationSpec :: Spec
reconciliationReportValidationSpec = describe "payment reconciliation report boundary" $ do
  it "defaults to sandbox, bounds pages and normalizes exact UUID filters" $ do
    Operations.validateReconciliationFilters Nothing Nothing Nothing Nothing Nothing
      `shouldBe` Right ("sandbox",Nothing,Nothing,25,0)
    Operations.validateReconciliationFilters (Just " production ") (Just "OPEN")
      (Just "AAAAAAAA-AAAA-4AAA-8AAA-AAAAAAAAAAAA") (Just 100) (Just 10000)
      `shouldBe` Right ("production",Just "open",Just "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa",100,10000)

  it "rejects invalid filters before reading the database or echoing their contents" $ do
    forM_ [(Just "synthetic_private",Nothing,Nothing,Nothing,Nothing),
      (Nothing,Just "synthetic_private",Nothing,Nothing,Nothing),
      (Nothing,Nothing,Just "synthetic_private",Nothing,Nothing),
      (Nothing,Nothing,Nothing,Just 0,Nothing),(Nothing,Nothing,Nothing,Just 101,Nothing),
      (Nothing,Nothing,Nothing,Nothing,Just (-1)),(Nothing,Nothing,Nothing,Nothing,Just 10001)] $
      \(environment,status,checkout,limit,offset) -> do
        result <- reconciliationReport [Admin] (error "Invalid filter accessed database")
          environment status checkout limit offset
        either errHTTPCode (const 200) result `shouldBe` 400
        show result `shouldNotContain` "synthetic_private"

  it "enforces strict-admin authorization before filters or database access" $ do
    forM_ [[Customer],[Fan],[Webmaster],[StudioManager],[Admin,Webmaster],[Admin,Manager]] $ \roles -> do
      result <- reconciliationReport roles (error "Unauthorized report accessed database")
        (Just "invalid") Nothing Nothing Nothing Nothing
      either errHTTPCode (const 200) result `shouldBe` 403

reconciliationReportSpec :: SpecWith ConnectionPool
reconciliationReportSpec = describe "read-only reconciliation evidence" $ do
  it "distinguishes an empty installed report from missing schema" $ \pool -> do
    emptyReport <- reconciliationReport [Admin] pool Nothing Nothing Nothing Nothing Nothing >>= requireRight
    crrSchemaReady emptyReport `shouldBe` True
    crrEnvironment emptyReport `shouldBe` "sandbox"
    crrEntries emptyReport `shouldBe` []
    let rename fromName toName = runSqlPool (rawExecute
          ("ALTER TABLE " <> fromName <> " RENAME TO " <> toName) []) pool
    bracket (rename "commerce_provider_binding" "synthetic_hidden_review_binding")
      (const (rename "synthetic_hidden_review_binding" "commerce_provider_binding")) $ \_ -> do
        report <- reconciliationReport [Admin] pool Nothing Nothing Nothing Nothing Nothing >>= requireRight
        crrSchemaReady report `shouldBe` False
        crrEntries report `shouldBe` []

  it "redacts database failures and never reports them as empty success" $ \pool -> do
    let rename fromName toName = runSqlPool (rawExecute
          ("ALTER TABLE commerce_reconciliation_exception RENAME COLUMN " <> fromName <> " TO " <> toName) []) pool
    bracket (rename "actual_amount_minor" "synthetic_private_column")
      (const (rename "synthetic_private_column" "actual_amount_minor")) $ \_ -> do
        result <- reconciliationReport [Admin] pool Nothing Nothing Nothing Nothing Nothing
        either errHTTPCode (const 200) result `shouldBe` 503
        show result `shouldNotContain` "synthetic_private_column"
        show result `shouldNotContain` "SELECT"
        either (\problem -> errHeaders problem `shouldContain` [("Cache-Control","no-store")])
          (const (expectationFailure "Expected unavailable response")) result

  forM_ [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone] $ \provider ->
    it ("links exact held evidence without changing " <> T.unpack (Checkout.paymentProviderText provider)) $ \pool -> do
      payment <- closedPaymentFixture pool provider "expired"
      approved <- parsedQuery payment Adapter.AdapterSucceeded
      runSqlPool (Reconciliation.applyQueryResult payment approved "review-report-fixture" notificationTime)
        pool `shouldReturn` Right Reconciliation.ReconciliationDeadLetter
      snapshot <- runSqlPool (paymentSnapshot payment) pool
      review <- runSqlPool (closedPaymentReviewSnapshot payment) pool
      let checkoutId = Checkout.checkoutReferenceId (Execution.bppCheckout payment)
          readReport = reconciliationReport [Admin,Fan,Customer] pool Nothing (Just "open")
            (Just checkoutId) Nothing Nothing >>= requireRight
      reports <- concurrently (replicate 4 readReport)
      forM_ reports $ \report -> do
        crrCheckoutId report `shouldBe` Just checkoutId
        crrStatus report `shouldBe` Just "open"
        case crrEntries report of
          [entry] -> do
            creProvider entry `shouldBe` Checkout.paymentProviderText provider
            creReason entry `shouldBe` "closed_checkout_approval"
            creCheckoutId entry `shouldBe` Just checkoutId
            crePaymentAttemptId entry `shouldBe`
              Just (Checkout.paymentAttemptReferenceId (Execution.bppAttempt payment))
            creExpectedMinor entry `shouldBe` Just "12515"
            creActualMinor entry `shouldBe` Just "12515"
          _ -> expectationFailure "Expected one exact linked review"
        let encoded = BL.toStrict (A.encode report)
        forM_ [TE.encodeUtf8 (Execution.bppMerchantRef payment),
          TE.encodeUtf8 (Execution.bppProviderResourceId payment),"merchant_account_ref",
          "provider_reference","resolution_notes","redirect","lease_token"] $ \private ->
            encoded `shouldNotSatisfy` BS.isInfixOf private
      runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot
      runSqlPool (closedPaymentReviewSnapshot payment) pool `shouldReturn` review

  it "preserves Int64 extrema as strings and redacts arbitrary legacy values" $ \pool -> do
    ident <- toText <$> nextRandom
    runSqlPool (rawExecute
      "INSERT INTO commerce_reconciliation_exception(id,provider,environment,merchant_account_ref,\
      \ exception_type,internal_reference,provider_reference,expected_amount_minor,actual_amount_minor,\
      \ currency,status,resolution_notes) VALUES (?::uuid,'synthetic_private_provider','sandbox',\
      \ 'synthetic_private_merchant','synthetic_private_type','synthetic_private_reference',\
      \ 'synthetic_private_resource',-9223372036854775808,9223372036854775807,\
      \ 'synthetic_private_currency','open','synthetic_private_notes')" [PersistText ident]) pool
    report <- reconciliationReport [Admin] pool Nothing Nothing Nothing Nothing Nothing >>= requireRight
    case filter ((== ident) . creId) (crrEntries report) of
      [entry] -> do
        creProvider entry `shouldBe` "unrecognized"
        creReason entry `shouldBe` "unrecognized"
        creCheckoutId entry `shouldBe` Nothing
        crePaymentAttemptId entry `shouldBe` Nothing
        creCurrency entry `shouldBe` Nothing
        creExpectedMinor entry `shouldBe` Just "-9223372036854775808"
        creActualMinor entry `shouldBe` Just "9223372036854775807"
      _ -> expectationFailure "Expected the redacted legacy review"
    let encoded = BL.toStrict (A.encode report)
    encoded `shouldNotSatisfy` BS.isInfixOf "synthetic_private"
    encoded `shouldSatisfy` BS.isInfixOf "\"9223372036854775807\""

  it "does not infer a binding from a checkout UUID with the wrong merchant account" $ \pool -> do
    payment <- closedPaymentFixture pool Checkout.ProviderPayPhone "cancelled"
    approved <- parsedQuery payment Adapter.AdapterSucceeded
    runSqlPool (Reconciliation.applyQueryResult payment approved "wrong-merchant-report" notificationTime)
      pool `shouldReturn` Right Reconciliation.ReconciliationDeadLetter
    let checkoutId = Checkout.checkoutReferenceId (Execution.bppCheckout payment)
    runSqlPool (rawExecute "UPDATE commerce_reconciliation_exception\
      \ SET merchant_account_ref='synthetic_private_wrong_account' WHERE internal_reference=?"
      [PersistText checkoutId]) pool
    report <- reconciliationReport [Admin] pool Nothing Nothing (Just checkoutId) Nothing Nothing >>= requireRight
    map creCheckoutId (crrEntries report) `shouldBe` [Nothing]
    map crePaymentAttemptId (crrEntries report) `shouldBe` [Nothing]

  it "separates environment and workflow filters with bounded deterministic pages" $ \pool -> do
    checkoutId <- toText <$> nextRandom
    forM_ [("sandbox","open"),("sandbox","open"),("sandbox","assigned"),("production","open")] $
      \(environment,status) -> runSqlPool (rawExecute
        "INSERT INTO commerce_reconciliation_exception(provider,environment,merchant_account_ref,\
        \ exception_type,internal_reference,expected_amount_minor,actual_amount_minor,currency,status)\
        \ VALUES ('paypal',?,'synthetic-account','provider_status_unknown',?,0,NULL,'USD',?)"
        [PersistText environment,PersistText checkoutId,PersistText status]) pool
    let page environment status offset = reconciliationReport [Admin] pool (Just environment)
          (Just status) (Just checkoutId) (Just 1) (Just offset) >>= requireRight
    first <- page "sandbox" "open" 0
    second <- page "sandbox" "open" 1
    crrHasMore first `shouldBe` True
    crrHasMore second `shouldBe` False
    map creId (crrEntries first) `shouldNotBe` map creId (crrEntries second)
    map creExpectedMinor (crrEntries first) `shouldBe` [Just "0"]
    map creActualMinor (crrEntries first) `shouldBe` [Nothing]
    assigned <- page "sandbox" "assigned" 0
    map creStatus (crrEntries assigned) `shouldBe` ["assigned"]
    production <- page "production" "open" 0
    crrEnvironment production `shouldBe` "production"
    length (crrEntries production) `shouldBe` 1
    crrHasMore production `shouldBe` False

  it "bounds a locked reporting read without changing the exception" $ \pool -> do
    locked <- newEmptyMVar
    release <- newEmptyMVar
    finished <- newEmptyMVar
    let hold = runSqlPool (do
          rawExecute "LOCK TABLE commerce_reconciliation_exception IN ACCESS EXCLUSIVE MODE" []
          liftIO (putMVar locked ())
          liftIO (takeMVar release)) pool
        cleanupLock _ = putMVar release () >> takeMVar finished >>= either throwIO pure
    bracket (forkFinally hold (putMVar finished)) cleanupLock $ \_ -> do
      Timeout.timeout 5000000 (takeMVar locked) `shouldReturn` Just ()
      result <- Timeout.timeout 8000000 (reconciliationReport [Admin] pool
        Nothing Nothing Nothing Nothing Nothing)
      fmap (either errHTTPCode (const 200)) result `shouldBe` Just 503

reconciliationReport
  :: [RoleEnum] -> ConnectionPool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int
  -> IO (Either ServerError CommerceReconciliationReportDTO)
reconciliationReport roles pool environment status checkout limit offset = do
  let user = AuthedUser (toSqlKey 1) roles mempty
      _ :<|> _ :<|> _ :<|> _ :<|> listReviews = Operations.commerceOperationsServer user
  result <- runHandler (runReaderT (listReviews environment status checkout limit offset) (queryEnv pool))
  case result of
    Left problem -> pure (Left problem)
    Right headers -> do
      getHeaders headers `shouldContain` [("Cache-Control","no-store")]
      pure (Right (getResponse headers))

providerQueryReportValidationSpec :: Spec
providerQueryReportValidationSpec = describe "provider query report boundary" $ do
  it "defaults to sandbox and validates bounded filters without echoing invalid input" $ do
    Operations.validateProviderQueryFilters Nothing Nothing Nothing Nothing
      `shouldBe` Right ("sandbox",Nothing,25,0)
    Operations.validateProviderQueryFilters (Just " production ") (Just "RETRY") (Just 100) (Just 10000)
      `shouldBe` Right ("production",Just "retry",100,10000)
    forM_ [(Just "synthetic_private",Nothing,Nothing,Nothing),
      (Nothing,Just "synthetic_private",Nothing,Nothing),
      (Nothing,Nothing,Just 0,Nothing),(Nothing,Nothing,Just 101,Nothing),
      (Nothing,Nothing,Nothing,Just (-1)),(Nothing,Nothing,Nothing,Just 10001)] $
      \(environment,status,limit,offset) -> do
        let result = Operations.validateProviderQueryFilters environment status limit offset
        result `shouldSatisfy` isLeft
        show result `shouldNotContain` "synthetic_private"

  it "projects only recognized server outcomes and never passes arbitrary diagnostics through" $ do
    Operations.providerQueryOutcome "query_binding_mismatch" `shouldBe` "query_binding_mismatch"
    Operations.providerQueryOutcome "immutable_binding_unavailable" `shouldBe` "immutable_binding_unavailable"
    Operations.providerQueryOutcome "synthetic_private_token" `shouldBe` "unrecognized"

  it "rejects invalid admin pagination before acquiring a database connection" $ do
    result <- queryReport [Admin] (error "Invalid pagination accessed the database")
      Nothing Nothing (Just 101) Nothing
    either errHTTPCode (const 200) result `shouldBe` 400

  it "denies non-strict admins before parsing filters or touching the database" $ do
    forM_ [[Customer],[Fan],[Webmaster],[StudioManager],[Admin,Webmaster],[Admin,Manager]] $ \roles -> do
      result <- queryReport roles (error "Unauthorized report accessed the database")
        (Just "invalid") Nothing Nothing Nothing
      either errHTTPCode (const 200) result `shouldBe` 403

providerQueryReportSpec :: SpecWith ConnectionPool
providerQueryReportSpec = describe "read-only provider query operations" $ do
  it "distinguishes installed empty queues from missing recovery schema" $ \pool -> do
    emptyReport <- queryReport [Admin] pool Nothing Nothing Nothing Nothing >>= requireRight
    cpqsSchemaReady emptyReport `shouldBe` True
    cpqsRecoveryFlagEnabled emptyReport `shouldBe` False
    cpqsEnvironment emptyReport `shouldBe` "sandbox"
    cpqsJobs emptyReport `shouldBe` []
    let rename fromName toName = runSqlPool (rawExecute
          ("ALTER TABLE " <> fromName <> " RENAME TO " <> toName) []) pool
    bracket (rename "commerce_provider_query_job" "synthetic_hidden_query_job")
      (const (rename "synthetic_hidden_query_job" "commerce_provider_query_job")) $ \_ -> do
        report <- queryReport [Admin] pool Nothing Nothing Nothing Nothing >>= requireRight
        cpqsSchemaReady report `shouldBe` False
        cpqsJobs report `shouldBe` []

  it "redacts SQL failures instead of returning a misleading empty report" $ \pool -> do
    let rename fromName toName = runSqlPool (rawExecute
          ("ALTER TABLE commerce_provider_query_budget RENAME COLUMN " <> fromName <> " TO " <> toName) []) pool
    bracket (rename "next_query_at" "synthetic_private_column")
      (const (rename "synthetic_private_column" "next_query_at")) $ \_ -> do
        report <- queryReport [Admin] pool Nothing Nothing Nothing Nothing
        either errHTTPCode (const 200) report `shouldBe` 503
        show report `shouldNotContain` "synthetic_private_column"
        show report `shouldNotContain` "SELECT"

  it "separates environments, bounds pages, and preserves jobs, budgets and financial history" $ \pool ->
    withQueryRecovery pool Checkout.ProviderPayPhone $ \merchant -> do
      payments <- forM [1..3 :: Int] $ \_ -> do
        payment <- reconciliationFixtureWithMerchant merchant pool Checkout.ProviderPayPhone
        prepareQueryJob pool payment
        pure payment
      beforeSnapshot <- mapM (\payment -> runSqlPool (paymentSnapshot payment) pool) payments
      first <- queryReport [Admin] pool Nothing (Just "pending") (Just 1) Nothing >>= requireRight
      second <- queryReport [Admin] pool Nothing (Just "pending") (Just 1) (Just 1) >>= requireRight
      cpqsHasMore first `shouldBe` True
      length (cpqsJobs first) `shouldBe` 1
      map cpqOperationId (cpqsJobs first) `shouldNotBe` map cpqOperationId (cpqsJobs second)
      cpqsOffset second `shouldBe` 1
      production <- queryReport [Admin] pool (Just "production") Nothing Nothing Nothing >>= requireRight
      cpqsEnvironment production `shouldBe` "production"
      cpqsJobs production `shouldBe` []
      cpqsBudgets production `shouldBe` []
      cpqsRecoveryFlagEnabled production `shouldBe` False
      cpqsRecoveryFlagEnabled first `shouldBe` True
      forM_ payments $ \payment -> queryJobState pool payment `shouldReturn` ("pending",0)
      mapM (\payment -> runSqlPool (paymentSnapshot payment) pool) payments `shouldReturn` beforeSnapshot
      -- Reading a pending queue must not reserve any remote-query budget.
      runSqlPool (rawSql "SELECT count(*) FROM commerce_provider_query_budget" []) pool
        `shouldReturn` [Single (0 :: Int64)]

  it "bounds a blocked reporting read and returns a redacted unavailable response" $ \pool -> do
    locked <- newEmptyMVar
    release <- newEmptyMVar
    finished <- newEmptyMVar
    let hold = runSqlPool (do
          rawExecute "LOCK TABLE commerce_provider_query_job IN ACCESS EXCLUSIVE MODE" []
          liftIO (putMVar locked ())
          liftIO (takeMVar release)) pool
        cleanupLock _ = do
          putMVar release ()
          takeMVar finished >>= either throwIO pure
    bracket (forkFinally hold (putMVar finished)) cleanupLock $ \_ -> do
      Timeout.timeout 5000000 (takeMVar locked) `shouldReturn` Just ()
      result <- Timeout.timeout 8000000 (queryReport [Admin] pool Nothing Nothing Nothing Nothing)
      fmap (either errHTTPCode (const 200)) result `shouldBe` Just 503

  it "redacts stored diagnostics and never exposes merchant, lease or provider references" $ \pool ->
    withQueryRecovery pool Checkout.ProviderPlaceToPay $ \merchant -> do
      payment <- reconciliationFixtureWithMerchant merchant pool Checkout.ProviderPlaceToPay
      prepareQueryJob pool payment
      runSqlPool (rawExecute
        "UPDATE commerce_provider_query_job SET last_error_code='synthetic_private_token'\
        \ WHERE operation_id IN (SELECT id FROM commerce_provider_operation WHERE payment_attempt_id=?::uuid)"
        [paymentAttemptParameter payment]) pool
      report <- queryReport [Admin,Fan,Customer] pool Nothing Nothing Nothing Nothing >>= requireRight
      let jobs = filter ((== Checkout.paymentAttemptReferenceId (Execution.bppAttempt payment))
            . cpqPaymentAttemptId) (cpqsJobs report)
          encoded = BL.toStrict (A.encode report)
      map cpqLastOutcome jobs `shouldBe` [Just "unrecognized"]
      forM_ ["synthetic_private_token",TE.encodeUtf8 merchant,"lease_token",
        "merchant_account_ref","provider_reference","provider_resource_id","redirect"] $ \forbidden ->
        encoded `shouldNotSatisfy` BS.isInfixOf forbidden

  it "shows completed checks independently from confirmed no-charge payment outcomes" $ \pool ->
    withQueryRecovery pool Checkout.ProviderPayPhone $ \merchant -> do
      payment <- reconciliationFixtureWithMerchant merchant pool Checkout.ProviderPayPhone
      prepareQueryJob pool payment
      runQueryTick pool (\_ -> pure (Right (queryValue payment Adapter.AdapterCancelled))) `shouldReturn` 1
      report <- queryReport [Admin] pool Nothing (Just "completed") Nothing Nothing >>= requireRight
      let jobs = filter ((== Checkout.paymentAttemptReferenceId (Execution.bppAttempt payment))
            . cpqPaymentAttemptId) (cpqsJobs report)
      map cpqStatus jobs `shouldBe` ["completed"]
      map cpqOperationStatus jobs `shouldBe` ["confirmed_no_charge"]
      map cpqOutcomeCertainty jobs `shouldBe` ["confirmed_no_charge"]
      noChargeStates pool payment `shouldReturn` ("cancelled","cancelled","provider_cancelled")

queryReport
  :: [RoleEnum] -> ConnectionPool -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int
  -> IO (Either ServerError CommerceProviderQueriesDTO)
queryReport roles pool environment status limit offset = do
  let user = AuthedUser (toSqlKey 1) roles mempty
      _ :<|> _ :<|> _ :<|> listQueries :<|> _ = Operations.commerceOperationsServer user
  result <- runHandler (runReaderT (listQueries environment status limit offset) (queryEnv pool))
  case result of
    Left problem -> pure (Left problem)
    Right headers -> do
      getHeaders headers `shouldContain` [("Cache-Control", "no-store")]
      pure (Right (getResponse headers))

noChargeReplaySpec :: SpecWith ConnectionPool
noChargeReplaySpec = describe "history-preserving no-charge reconciliation" $
  forM_ [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone] $ \provider ->
    describe (T.unpack (Checkout.paymentProviderText provider)) $ do
      it "records a new cancellation once and replays it concurrently without financial rewrites" $ \pool -> do
        payment <- reconciliationFixture pool provider
        cancelled <- parsedQuery payment Adapter.AdapterCancelled
        cancelledResult pool payment cancelled
        noChargeStates pool payment `shouldReturn` ("cancelled","cancelled","provider_cancelled")
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        results <- concurrently (replicate 4 (runSqlPool
          (Reconciliation.applyQueryResult payment cancelled "duplicate-cancel" notificationTime) pool))
        results `shouldBe` replicate 4 (Right Reconciliation.ReconciliationProcessed)
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "preserves failed legacy intent, attempt, error code and audit after reclassified cancellation" $ \pool -> do
        payment <- reconciliationFixture pool provider
        legacy <- historicalDecline payment
        cancelledResult pool payment legacy
        noChargeStates pool payment `shouldReturn` ("failed","failed","provider_declined")
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        cancelled <- parsedQuery payment Adapter.AdapterCancelled
        results <- concurrently (replicate 4 (runSqlPool
          (Reconciliation.applyQueryResult payment cancelled "legacy-cancel-replay" notificationTime) pool))
        results `shouldBe` replicate 4 (Right Reconciliation.ReconciliationProcessed)
        noChargeStates pool payment `shouldReturn` ("failed","failed","provider_declined")
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "does not downgrade a new cancellation when an older declined classification arrives" $ \pool -> do
        payment <- reconciliationFixture pool provider
        parsedQuery payment Adapter.AdapterCancelled >>= cancelledResult pool payment
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        historicalDecline payment >>= cancelledResult pool payment
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      forM_ [False, True] $ \paid ->
        it ("preserves a replacement attempt after old no-charge replay, paid=" <> show paid) $ \pool -> do
          payment <- reconciliationFixture pool provider
          historicalDecline payment >>= cancelledResult pool payment
          let replacementProvider = if provider == Checkout.ProviderPayPhone
                then Checkout.ProviderPlaceToPay else Checkout.ProviderPayPhone
          replacement <- replacementPayment pool payment replacementProvider
          now <- getCurrentTime
          if paid then do
            succeeded <- parsedQuery replacement Adapter.AdapterSucceeded
            runSqlPool (Reconciliation.applyQueryResult replacement succeeded "replacement-success" now) pool
              `shouldReturn` Right Reconciliation.ReconciliationProcessed
            assertPaymentPosted pool replacement
          else runSqlPool (Checkout.recordPaymentProcessing (Execution.bppCheckout replacement)
            (Execution.bppAttempt replacement) replacementProvider "replacement-processing" now) pool
          originalSnapshot <- runSqlPool (paymentSnapshot payment) pool
          replacementSnapshot <- runSqlPool (paymentSnapshot replacement) pool
          parsedQuery payment Adapter.AdapterCancelled >>= cancelledResult pool payment
          runSqlPool (paymentSnapshot payment) pool `shouldReturn` originalSnapshot
          runSqlPool (paymentSnapshot replacement) pool `shouldReturn` replacementSnapshot

      it "keeps historical no-charge labels unchanged through the actual callback query pipeline" $ \pool ->
        withNotificationEnvironment $ do
          payment <- reconciliationFixture pool provider
          historicalDecline payment >>= cancelledResult pool payment
          payload <- reconciliationNotification pool payment
          snapshot <- runSqlPool (paymentSnapshot payment) pool
          outcome <- Reconciliation.processProviderEventWith
            (\_ -> pure (Right (queryValue payment Adapter.AdapterCancelled))) getCurrentTime
            (queryEnv pool) payload notificationTime
          Reconciliation.prrDisposition outcome `shouldBe` Reconciliation.ReconciliationProcessed
          runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "applies cancellation through the fenced missed-callback worker without a capture" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runQueryTick pool (\_ -> pure (Right (queryValue payment Adapter.AdapterCancelled)))
            `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("completed",1)
          noChargeStates pool payment `shouldReturn` ("cancelled","cancelled","provider_cancelled")

      it "rejects inconsistent terminal intent/attempt history without repairing it silently" $ \pool -> do
        payment <- reconciliationFixture pool provider
        historicalDecline payment >>= cancelledResult pool payment
        runSqlPool (rawExecute "UPDATE commerce_payment_attempt SET status='processing' WHERE id=?::uuid"
          [paymentAttemptParameter payment]) pool
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        cancelled <- parsedQuery payment Adapter.AdapterCancelled
        runSqlPool (Reconciliation.applyQueryResult payment cancelled "inconsistent-no-charge" notificationTime)
          pool >>= (`shouldSatisfy` isLeft)
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "refuses no-charge replay with authorized, captured or refunded money" $ \pool ->
        forM_ [(1,0,0),(1,1,0),(1,1,1)] $ \(authorized,captured,refunded) -> do
          payment <- reconciliationFixture pool provider
          historicalDecline payment >>= cancelledResult pool payment
          runSqlPool (rawExecute
            "UPDATE commerce_payment_intent SET authorized_minor=?,captured_minor=?,refunded_minor=?\
            \ WHERE id=?::uuid" [PersistInt64 authorized,PersistInt64 captured,PersistInt64 refunded,
              PersistText (Execution.bppPaymentIntentId payment)]) pool
          snapshot <- runSqlPool (paymentSnapshot payment) pool
          cancelled <- parsedQuery payment Adapter.AdapterCancelled
          runSqlPool (Reconciliation.applyQueryResult payment cancelled "money-conflict" notificationTime)
            pool >>= (`shouldSatisfy` isLeft)
          runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "refuses posted capture evidence even if a corrupt repair cleared intent counters" $ \pool -> do
        payment <- reconciliationFixture pool provider
        succeeded <- parsedQuery payment Adapter.AdapterSucceeded
        runSqlPool (Reconciliation.applyQueryResult payment succeeded "ledger-guard-capture" notificationTime)
          pool `shouldReturn` Right Reconciliation.ReconciliationProcessed
        -- Deliberately incoherent local fixture: no guard or ledger row is
        -- disabled/deleted. Verify the store's defense in depth directly;
        -- the public reconciler also rejects the successful operation conflict.
        runSqlPool (do
          rawExecute "UPDATE commerce_payment_attempt SET status='failed' WHERE id=?::uuid"
            [paymentAttemptParameter payment]
          rawExecute "UPDATE commerce_payment_intent SET status='failed',authorized_minor=0,\
            \ captured_minor=0,refunded_minor=0 WHERE id=?::uuid"
            [PersistText (Execution.bppPaymentIntentId payment)]) pool
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        runSqlPool (Execution.validateNoChargeObservation payment) pool >>= (`shouldSatisfy` isLeft)
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

-- Reconstruct the old typed result only for historical compatibility fixtures.
-- This is not presented as a result from the current PayPhone query parser.
historicalDecline :: Execution.BoundProviderPayment -> IO Adapter.AdapterResult
historicalDecline payment = do
  result <- parsedQuery payment Adapter.AdapterCancelled
  pure result { Adapter.adapterResultState = Adapter.AdapterDeclined }

cancelledResult :: ConnectionPool -> Execution.BoundProviderPayment -> Adapter.AdapterResult -> Expectation
cancelledResult pool payment result = runSqlPool
  (Reconciliation.applyQueryResult payment result "no-charge-test" notificationTime) pool
  `shouldReturn` Right Reconciliation.ReconciliationProcessed

noChargeStates :: ConnectionPool -> Execution.BoundProviderPayment -> IO (Text,Text,Text)
noChargeStates pool payment = do
  rows <- runSqlPool (rawSql
    "SELECT intent.status,attempt.status,attempt.failure_code FROM commerce_payment_attempt attempt\
    \ JOIN commerce_payment_intent intent ON intent.id=attempt.payment_intent_id\
    \ WHERE attempt.id=?::uuid AND intent.authorized_minor=0 AND intent.captured_minor=0\
    \ AND intent.refunded_minor=0 AND NOT EXISTS (SELECT 1 FROM commerce_ledger_transaction txn\
    \ WHERE txn.source_id=attempt.id::text)" [paymentAttemptParameter payment]) pool
    :: IO [(Single Text,Single Text,Single Text)]
  case rows of
    [(Single intentStatus,Single attemptStatus,Single code)] -> pure (intentStatus,attemptStatus,code)
    _ -> fail "Expected one no-charge payment without financial entries"

replacementPayment
  :: ConnectionPool -> Execution.BoundProviderPayment -> Checkout.PaymentProvider
  -> IO Execution.BoundProviderPayment
replacementPayment pool original provider = do
  now <- getCurrentTime
  key <- toText <$> nextRandom
  let creation = Checkout.PaymentAttemptCreation
        { Checkout.pacCheckout = Execution.bppCheckout original
        , Checkout.pacProvider = provider, Checkout.pacEnvironment = Checkout.CheckoutSandbox
        , Checkout.pacOperation = Checkout.OperationCreate, Checkout.pacAmountMinor = 12515
        , Checkout.pacCurrency = "USD", Checkout.pacMerchantRef = Execution.bppMerchantRef original
        , Checkout.pacIdempotencyKey = key, Checkout.pacCreatedAt = now
        , Checkout.pacCorrelationId = "synthetic-replacement"
        }
      method = if provider == Checkout.ProviderPayPhone then MethodPayPhoneWallet else MethodCard
  attempt <- runSqlPool (Runtime.beginPaymentAttemptForMethod method creation) pool >>= requireRight
  let reference = providerReference provider (Checkout.paymentAttemptReferenceId attempt)
  operation <- runSqlPool (Execution.prepareProviderOperation Execution.ProviderOperationPreparation
    { Execution.popAttempt = attempt, Execution.popProvider = provider
    , Execution.popEnvironment = Checkout.CheckoutSandbox
    , Execution.popMerchantRef = Checkout.pacMerchantRef creation
    , Execution.popProviderReference = reference, Execution.popOperation = AdapterCreate
    , Execution.popIdempotencyKey = key, Execution.popRequestSha256 = digestText key
    , Execution.popOccurredAt = now }) pool >>= requireRight
  bindReconciliationFixture pool creation operation reference

queryRecoverySpec :: SpecWith ConnectionPool
queryRecoverySpec = describe "durable missed-callback recovery" $ do
  it "installs both recovery flags disabled and enforces a shared concurrent query budget" $ \pool -> do
    flags <- runSqlPool (rawSql
      "SELECT enabled FROM revenue_feature_flag WHERE flag_key='checkout.provider_query_recovery' ORDER BY environment" [])
      pool :: IO [Single Bool]
    flags `shouldBe` [Single False, Single False]
    resetQueryBudget pool Checkout.ProviderPayPhone
    grants <- concurrently (replicate 8 (runSqlPool
      (Execution.reserveProviderQueryBudget Checkout.ProviderPayPhone Checkout.CheckoutSandbox) pool))
    length (filter id grants) `shouldBe` 1
    runSqlPool (Execution.reserveProviderQueryBudget Checkout.ProviderPlaceToPay Checkout.CheckoutProduction)
      pool `shouldReturn` True

  forM_ [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone] $ \provider ->
    describe (T.unpack (Checkout.paymentProviderText provider)) $ do
      it "recovers a missed callback with one query and no fabricated inbox evidence" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runQueryTick pool (successfulQuery payment) `shouldReturn` 1
          assertPaymentPosted pool payment
          queryJobState pool payment `shouldReturn` ("completed",1)
          rows <- runSqlPool (rawSql
            "SELECT count(*) FROM commerce_provider_event_inbox WHERE merchant_account_ref=?"
            [PersistText merchant]) pool :: IO [Single Int64]
          rows `shouldBe` [Single 0]

      it "honors the process switch, exact sandbox flag and runtime configuration" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          setEnv "COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED" "false"
          runQueryTick pool noQuery `shouldReturn` 0
          setEnv "COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED" "true"
          setRecoveryFlag pool False
          runQueryTick pool noQuery `shouldReturn` 0
          setRecoveryFlag pool True
          setEnv (if provider == Checkout.ProviderPlaceToPay then "PLACETOPAY_LOGIN" else "PAYPHONE_TOKEN") ""
          runQueryTick pool noQuery `shouldReturn` 0
          queryJobState pool payment `shouldReturn` ("pending",0)
          assertQueryUnpaid pool payment

      it "ignores known resources without qualified account authority" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runSqlPool (rawExecute
            "UPDATE commerce_provider_account SET enabled=false WHERE provider=? AND environment='sandbox'"
            [PersistText (Checkout.paymentProviderText provider)]) pool
          runQueryTick pool noQuery `shouldReturn` 0
          queryJobState pool payment `shouldReturn` ("pending",0)

      it "does not enqueue or recreate an ambiguous operation without a bound resource" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          _ <- replayFixtureWithMerchant "service_booking" merchant pool provider
          runQueryTick pool noQuery `shouldReturn` 0
          rows <- runSqlPool (rawSql
            "SELECT count(*) FROM commerce_provider_query_job job\
            \ JOIN commerce_provider_operation operation ON operation.id=job.operation_id\
            \ WHERE operation.merchant_account_ref=?" [PersistText merchant]) pool :: IO [Single Int64]
          rows `shouldBe` [Single 0]

      it "serializes replica claims and posts only one payment" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          calls <- newIORef (0 :: Int)
          claimed <- concurrently (replicate 4 (runQueryTick pool
            (\request -> atomicModifyIORef' calls (\n -> (n+1,())) >> successfulQuery payment request)))
          sum claimed `shouldBe` 1
          readIORef calls `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("completed",1)
          assertPaymentPosted pool payment

      it "shares the callback query budget without burning job attempts" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          payload <- reconciliationNotification pool payment
          outcome <- Reconciliation.processProviderEventWith
            (\_ -> pure (Right (queryValue payment Adapter.AdapterPending))) getCurrentTime
            (queryEnv pool) payload notificationTime
          Reconciliation.prrDisposition outcome `shouldBe` Reconciliation.ReconciliationRetry
          runQueryTick pool noQuery `shouldReturn` 0
          queryJobState pool payment `shouldReturn` ("pending",0)

      it "retries transport failures with backoff and never infers no charge" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runQueryTick pool (\_ -> pure (Left (ProviderHttp.AdapterTransportError "SYNTHETIC-PRIVATE")))
            `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("retry",1)
          assertQueryUnpaid pool payment
          rows <- runSqlPool (rawSql
            "SELECT job.next_attempt_at>clock_timestamp(),job.last_error_code FROM commerce_provider_query_job job\
            \ JOIN commerce_provider_operation operation ON operation.id=job.operation_id\
            \ WHERE operation.payment_attempt_id=?::uuid" [paymentAttemptParameter payment]) pool
            :: IO [(Single Bool,Single Text)]
          rows `shouldBe` [(Single True,Single "query_unavailable")]

      it "discards an expired response and recovers the original attempt on a fresh lease" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runQueryTick pool (\request -> expireQueryLease pool payment >> successfulQuery payment request)
            `shouldReturn` 1
          assertQueryUnpaid pool payment
          queryJobState pool payment `shouldReturn` ("processing",1)
          resetQueryBudget pool provider
          runQueryTick pool (successfulQuery payment) `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("completed",2)
          assertPaymentPosted pool payment

      it "cannot apply a response after another worker replaces its lease token" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          replacement <- newIORef Nothing
          runQueryTick pool (\request -> do
            expireQueryLease pool payment
            resetQueryBudget pool provider
            claimed <- runSqlPool (Execution.claimProviderQuery provider Checkout.CheckoutSandbox) pool
            claimed `shouldSatisfy` maybe False (const True)
            modifyIORef' replacement (const (Execution.pqcLeaseToken <$> claimed))
            successfulQuery payment request) `shouldReturn` 1
          assertQueryUnpaid pool payment
          queryJobState pool payment `shouldReturn` ("processing",2)
          expected <- readIORef replacement
          tokens <- runSqlPool (rawSql
            "SELECT lease_token::text FROM commerce_provider_query_job job\
            \ JOIN commerce_provider_operation operation ON operation.id=job.operation_id\
            \ WHERE operation.payment_attempt_id=?::uuid" [paymentAttemptParameter payment]) pool
            :: IO [Single (Maybe Text)]
          tokens `shouldBe` [Single expected]

      it "rechecks the kill switch after the provider responds" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runQueryTick pool (\request -> setRecoveryFlag pool False >> successfulQuery payment request)
            `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("retry",1)
          assertQueryUnpaid pool payment

      it "rechecks the process switch after the provider responds" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runQueryTick pool (\request -> do
            setEnv "COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED" "false"
            successfulQuery payment request) `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("retry",1)
          assertQueryUnpaid pool payment

      it "rolls back financial effects if the atomic job completion fails" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          let install = runSqlPool (rawExecute
                "ALTER TABLE commerce_provider_query_job ADD CONSTRAINT synthetic_job_completion_failure\
                \ CHECK (status <> 'completed') NOT VALID" []) pool
              remove () = runSqlPool (rawExecute
                "ALTER TABLE commerce_provider_query_job DROP CONSTRAINT synthetic_job_completion_failure" []) pool
          failed <- bracket install remove (\_ -> try (runQueryTick pool (successfulQuery payment)))
            :: IO (Either SqlError Int)
          failed `shouldSatisfy` isLeft
          assertQueryUnpaid pool payment
          queryJobState pool payment `shouldReturn` ("processing",1)
          expireQueryLease pool payment
          resetQueryBudget pool provider
          runQueryTick pool (successfulQuery payment) `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("completed",2)
          assertPaymentPosted pool payment

      it "does not downgrade a callback success arriving during a pending query" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runQueryTick pool (\_ -> do
            succeeded <- parsedQuery payment Adapter.AdapterSucceeded
            runSqlPool (Reconciliation.applyQueryResult payment succeeded
              "synthetic-concurrent-callback" notificationTime) pool
              `shouldReturn` Right Reconciliation.ReconciliationProcessed
            pure (Right (queryValue payment Adapter.AdapterPending))) `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("dead_letter",1)
          assertPaymentPosted pool payment

      it "dead-letters a crashed final lease without another remote query" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          _ <- runSqlPool (Execution.claimProviderQuery provider Checkout.CheckoutSandbox) pool
          runSqlPool (rawExecute
            "UPDATE commerce_provider_query_job SET attempt_count=24,\
            \ lease_expires_at=clock_timestamp()-INTERVAL '1 second' WHERE operation_id IN\
            \ (SELECT id FROM commerce_provider_operation WHERE payment_attempt_id=?::uuid)"
            [paymentAttemptParameter payment]) pool
          runQueryTick pool noQuery `shouldReturn` 0
          queryJobState pool payment `shouldReturn` ("dead_letter",24)
          assertQueryUnpaid pool payment

      it "dead-letters mismatched money and late payment on an expired checkout" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runQueryTick pool (\_ -> pure (Right (queryValue payment { Execution.bppAmountMinor = 1 }
            Adapter.AdapterSucceeded))) `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("dead_letter",1)
          assertQueryUnpaid pool payment
          other <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool other
          runSqlPool (rawExecute "UPDATE commerce_checkout_session SET status='expired' WHERE id=?::uuid"
            [PersistText (Checkout.checkoutReferenceId (Execution.bppCheckout other))]) pool
          resetQueryBudget pool provider
          runQueryTick pool (successfulQuery other) `shouldReturn` 1
          queryJobState pool other `shouldReturn` ("dead_letter",1)
          exceptions <- runSqlPool (rawSql
            "SELECT count(*) FROM commerce_reconciliation_exception\
            \ WHERE merchant_account_ref=? AND exception_type='scheduled_query_requires_review'"
            [PersistText merchant]) pool :: IO [Single Int64]
          exceptions `shouldBe` [Single 1]
          observed <- runSqlPool (rawSql
            "SELECT actual_amount_minor FROM commerce_reconciliation_exception\
            \ WHERE internal_reference=? AND exception_type='verified_payment_on_closed_checkout'"
            [PersistText (Checkout.checkoutReferenceId (Execution.bppCheckout other))]) pool
            :: IO [Single Int64]
          observed `shouldBe` [Single (Execution.bppAmountMinor other)]

      it "stops at the retry bound without converting pending into no charge" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          runSqlPool (rawExecute
            "UPDATE commerce_provider_query_job SET attempt_count=23 WHERE operation_id IN\
            \ (SELECT id FROM commerce_provider_operation WHERE payment_attempt_id=?::uuid)"
            [paymentAttemptParameter payment]) pool
          runQueryTick pool (\_ -> pure (Right (queryValue payment Adapter.AdapterPending))) `shouldReturn` 1
          queryJobState pool payment `shouldReturn` ("dead_letter",24)
          runQueryTick pool noQuery `shouldReturn` 0
          assertQueryUnpaid pool payment

      it "preserves terminal jobs and rejects invalid state or live-lease changes" $ \pool ->
        withQueryRecovery pool provider $ \merchant -> do
          payment <- reconciliationFixtureWithMerchant merchant pool provider
          prepareQueryJob pool payment
          let attemptParam = [paymentAttemptParameter payment]
              mutate suffix = try (runSqlPool (rawExecute
                ("UPDATE commerce_provider_query_job SET " <> suffix <>
                  " WHERE operation_id IN (SELECT id FROM commerce_provider_operation WHERE payment_attempt_id=?::uuid)")
                attemptParam) pool) :: IO (Either SqlError ())
          mutate "status='completed',completed_at=clock_timestamp()" >>= (`shouldSatisfy` isLeft)
          _ <- runSqlPool (Execution.claimProviderQuery provider Checkout.CheckoutSandbox) pool
          mutate "lease_token=gen_random_uuid()" >>= (`shouldSatisfy` isLeft)
          expireQueryLease pool payment
          resetQueryBudget pool provider
          runQueryTick pool (successfulQuery payment) `shouldReturn` 1
          mutate "last_error_code='rewritten'" >>= (`shouldSatisfy` isLeft)
          deleted <- try (runSqlPool (rawExecute
            "DELETE FROM commerce_provider_query_job WHERE operation_id IN\
            \ (SELECT id FROM commerce_provider_operation WHERE payment_attempt_id=?::uuid)" attemptParam) pool)
            :: IO (Either SqlError ())
          deleted `shouldSatisfy` isLeft
          queryJobState pool payment `shouldReturn` ("completed",2)

withQueryRecovery :: ConnectionPool -> Checkout.PaymentProvider -> (Text -> IO value) -> IO value
withQueryRecovery pool provider action = withNotificationEnvironment $
  bracket (lookupEnv "COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED")
    (maybe (unsetEnv "COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED")
      (setEnv "COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED")) $ \_ -> do
    merchant <- ("synthetic-query-" <>) . toText <$> nextRandom
    let restore () = do
          setRecoveryFlag pool False
          runSqlPool (rawExecute
            "UPDATE commerce_provider_account SET merchant_account_ref='synthetic-provider-retry-merchant',enabled=true\
            \ WHERE provider=? AND environment='sandbox'" [PersistText (Checkout.paymentProviderText provider)]) pool
        setup = do
          setEnv "COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED" "true"
          setEnv (if provider == Checkout.ProviderPlaceToPay then "PAYPHONE_TOKEN" else "PLACETOPAY_LOGIN") ""
          setRecoveryFlag pool True
          runSqlPool (rawExecute "UPDATE commerce_provider_account SET merchant_account_ref=?\
            \ WHERE provider=? AND environment='sandbox'"
            [PersistText merchant,PersistText (Checkout.paymentProviderText provider)]) pool
          resetQueryBudget pool provider
    bracket setup restore (const (action merchant))

setRecoveryFlag :: ConnectionPool -> Bool -> IO ()
setRecoveryFlag pool enabled = runSqlPool (rawExecute
  "UPDATE revenue_feature_flag SET enabled=? WHERE flag_key='checkout.provider_query_recovery' AND environment='sandbox'"
  [PersistBool enabled]) pool

resetQueryBudget :: ConnectionPool -> Checkout.PaymentProvider -> IO ()
resetQueryBudget pool provider = runSqlPool (rawExecute
  "UPDATE commerce_provider_query_budget SET next_query_at=clock_timestamp()-INTERVAL '1 second'\
  \ WHERE provider=? AND environment='sandbox'" [PersistText (Checkout.paymentProviderText provider)]) pool

queryEnv :: ConnectionPool -> Env
queryEnv pool = Env pool (error "Synthetic query worker must not use AppConfig")

runQueryTick :: ConnectionPool -> (Adapter.AdapterRequest -> IO (Either ProviderHttp.AdapterTransportError A.Value)) -> IO Int
runQueryTick pool fetch = Reconciliation.providerQueryWorkerTickWith fetch (queryEnv pool)

noQuery :: Adapter.AdapterRequest -> IO (Either ProviderHttp.AdapterTransportError A.Value)
noQuery _ = fail "No remote query is authorized in this test"

successfulQuery :: Execution.BoundProviderPayment -> Adapter.AdapterRequest
  -> IO (Either ProviderHttp.AdapterTransportError A.Value)
successfulQuery payment request = do
  Adapter.arOperation request `shouldBe` AdapterQuery
  Adapter.arProvider request `shouldBe` Execution.bppProvider payment
  pure (Right (queryValue payment Adapter.AdapterSucceeded))

prepareQueryJob :: ConnectionPool -> Execution.BoundProviderPayment -> IO ()
prepareQueryJob pool payment = do
  _ <- runSqlPool (Execution.enqueueProviderQueries (Execution.bppProvider payment) Checkout.CheckoutSandbox) pool
  runSqlPool (rawExecute
    "UPDATE commerce_provider_query_job SET next_attempt_at=clock_timestamp()-INTERVAL '1 day'\
    \ WHERE operation_id IN (SELECT id FROM commerce_provider_operation WHERE payment_attempt_id=?::uuid)"
    [paymentAttemptParameter payment]) pool
  queryJobState pool payment `shouldReturn` ("pending",0)

queryJobState :: ConnectionPool -> Execution.BoundProviderPayment -> IO (Text,Int)
queryJobState pool payment = do
  rows <- runSqlPool (rawSql
    "SELECT job.status,job.attempt_count FROM commerce_provider_query_job job\
    \ JOIN commerce_provider_operation operation ON operation.id=job.operation_id\
    \ WHERE operation.payment_attempt_id=?::uuid" [paymentAttemptParameter payment]) pool
    :: IO [(Single Text,Single Int)]
  case rows of [(Single status,Single count)] -> pure (status,count); _ -> fail "Expected one query job"

expireQueryLease :: ConnectionPool -> Execution.BoundProviderPayment -> IO ()
expireQueryLease pool payment = runSqlPool (rawExecute
  "UPDATE commerce_provider_query_job SET lease_expires_at=clock_timestamp()-INTERVAL '1 second'\
  \ WHERE operation_id IN (SELECT id FROM commerce_provider_operation WHERE payment_attempt_id=?::uuid)"
  [paymentAttemptParameter payment]) pool

assertQueryUnpaid :: ConnectionPool -> Execution.BoundProviderPayment -> Expectation
assertQueryUnpaid pool payment = do
  rows <- runSqlPool (rawSql
    "SELECT checkout.paid_minor,intent.captured_minor,operation.outcome_certainty,\
    \ (SELECT count(*) FROM commerce_ledger_transaction WHERE source_id=attempt.id::text),\
    \ (SELECT count(*) FROM commerce_receipt WHERE checkout_id=checkout.id)\
    \ FROM commerce_payment_attempt attempt\
    \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id\
    \ JOIN commerce_payment_intent intent ON intent.id=attempt.payment_intent_id\
    \ JOIN commerce_provider_operation operation ON operation.payment_attempt_id=attempt.id\
    \ WHERE attempt.id=?::uuid" [paymentAttemptParameter payment]) pool
    :: IO [(Single Int64,Single Int64,Single Text,Single Int64,Single Int64)]
  rows `shouldBe` [(Single 0,Single 0,Single "ambiguous",Single 0,Single 0)]

-- Shared CheckoutStore boundary: real PostgreSQL, synthetic verified evidence.
-- These tests do not exercise provider HTTP or actual refunds/disputes.
closedCheckoutEvidenceSpec :: SpecWith ConnectionPool
closedCheckoutEvidenceSpec = describe "closed checkout approval evidence" $
  forM_ [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone] $ \provider ->
    describe (T.unpack (Checkout.paymentProviderText provider)) $
      forM_ ["expired", "cancelled"] $ \closedStatus ->
        describe (T.unpack closedStatus) $ do
          it "retains exact observed money once without applying a late capture" $ \pool -> do
            payment <- closedPaymentFixture pool provider closedStatus
            result <- parsedQuery payment Adapter.AdapterSucceeded
            financialBefore <- runSqlPool (closedPaymentFinancialSnapshot payment) pool
            outcomes <- concurrently (replicate 4 (runSqlPool
              (Reconciliation.applyQueryResult payment result "late-approval" notificationTime) pool))
            outcomes `shouldBe` replicate 4 (Right Reconciliation.ReconciliationDeadLetter)
            assertClosedPaymentEvidence pool payment
            runSqlPool (closedPaymentFinancialSnapshot payment) pool `shouldReturn` financialBefore
            snapshot <- runSqlPool (paymentSnapshot payment) pool
            runSqlPool (Reconciliation.applyQueryResult payment result "later-replay"
              (addUTCTime 3600 notificationTime)) pool
              `shouldReturn` Right Reconciliation.ReconciliationDeadLetter
            runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

          it "rolls back review evidence and its audit with the caller savepoint" $ \pool -> do
            payment <- closedPaymentFixture pool provider closedStatus
            result <- parsedQuery payment Adapter.AdapterSucceeded
            snapshotBefore <- runSqlPool (paymentSnapshot payment) pool
            runSqlPool (do
              rawExecute "SAVEPOINT caller_owned" []
              applied <- Reconciliation.applyQueryResult payment result "late-rollback" notificationTime
              liftIO (applied `shouldBe` Right Reconciliation.ReconciliationDeadLetter)
              rawExecute "ROLLBACK TO SAVEPOINT caller_owned" []
              rawExecute "RELEASE SAVEPOINT caller_owned" []) pool
            runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshotBefore

          it "holds reordered outcomes and never releases review from an exception status edit" $ \pool -> do
            payment <- closedPaymentFixture pool provider closedStatus
            succeeded <- parsedQuery payment Adapter.AdapterSucceeded
            runSqlPool (Reconciliation.applyQueryResult payment succeeded "late-first" notificationTime)
              pool `shouldReturn` Right Reconciliation.ReconciliationDeadLetter
            forM_ ["assigned", "resolved", "ignored"] $ \reviewStatus -> do
              runSqlPool (rawExecute
                "UPDATE commerce_reconciliation_exception SET status=?,resolution_notes='synthetic review'\
                \ WHERE internal_reference=? AND exception_type='verified_payment_on_closed_checkout'"
                [PersistText reviewStatus,
                  PersistText (Checkout.checkoutReferenceId (Execution.bppCheckout payment))]) pool
              snapshot <- runSqlPool (paymentSnapshot payment) pool
              review <- runSqlPool (closedPaymentReviewSnapshot payment) pool
              forM_ [Adapter.AdapterSucceeded, Adapter.AdapterPending, Adapter.AdapterCancelled,
                  Adapter.AdapterUnknown] $ \state -> do
                result <- parsedQuery payment state
                runSqlPool (Reconciliation.applyQueryResult payment result "reordered-after-review"
                  (addUTCTime 3600 notificationTime)) pool
                  `shouldReturn` Right Reconciliation.ReconciliationDeadLetter
                runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot
                runSqlPool (closedPaymentReviewSnapshot payment) pool `shouldReturn` review

          it "does not create or acknowledge review from mismatched typed evidence" $ \pool -> do
            payment <- closedPaymentFixture pool provider closedStatus
            result <- parsedQuery payment Adapter.AdapterSucceeded
            let reject = do
                  snapshot <- runSqlPool (paymentSnapshot payment) pool
                  forM_ [result { Adapter.adapterResultAmountMinor = Nothing }
                    , result { Adapter.adapterResultAmountMinor = Just 1 }
                    , result { Adapter.adapterResultCurrency = Just "EUR" }
                    , result { Adapter.adapterResultExternalId = "SYNTHETIC-PRIVATE" }
                    , result { Adapter.adapterResultCertainty = ProviderAmbiguous }] $ \invalid -> do
                      outcome <- runSqlPool (Reconciliation.applyQueryResult payment invalid
                        "invalid-closed-approval" notificationTime) pool
                      outcome `shouldSatisfy` isLeft
                      show outcome `shouldNotContain` "SYNTHETIC-PRIVATE"
                  runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot
            reject
            runSqlPool (Reconciliation.applyQueryResult payment result "valid-closed-approval" notificationTime)
              pool `shouldReturn` Right Reconciliation.ReconciliationDeadLetter
            reject

          it "rolls back the exception if its immutable audit cannot be written" $ \pool -> do
            payment <- closedPaymentFixture pool provider closedStatus
            result <- parsedQuery payment Adapter.AdapterSucceeded
            snapshot <- runSqlPool (paymentSnapshot payment) pool
            let install = runSqlPool (rawExecute
                  "ALTER TABLE commerce_checkout_audit_event ADD CONSTRAINT synthetic_late_audit_failure\
                  \ CHECK (event_type<>'verified_payment_on_closed_checkout') NOT VALID" []) pool
                remove _ = runSqlPool (rawExecute
                  "ALTER TABLE commerce_checkout_audit_event DROP CONSTRAINT synthetic_late_audit_failure" []) pool
            failed <- bracket install remove (\_ -> try (runSqlPool
              (Reconciliation.applyQueryResult payment result "failed-review-audit" notificationTime) pool))
              :: IO (Either SqlError (Either Text Reconciliation.ReconciliationDisposition))
            failed `shouldSatisfy` isLeft
            runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

          it "hides a retained redirect and never advertises no-charge fallback through recovery" $ \pool -> do
            payment <- closedPaymentFixture pool provider closedStatus
            let checkoutId = Checkout.checkoutReferenceId (Execution.bppCheckout payment)
                attemptId = Checkout.paymentAttemptReferenceId (Execution.bppAttempt payment)
                load token = runSqlPool (Execution.loadAuthorizedCreateOperation checkoutId attemptId
                  (digestText token) recoveryEncryptionKey) pool
            runSqlPool (rawExecute
              "UPDATE commerce_provider_operation SET redirect_url_ciphertext=pgp_sym_encrypt(?,?)\
              \ WHERE payment_attempt_id=?::uuid"
              [PersistText "https://checkout-test.placetopay.com/session/synthetic-private",
                PersistText recoveryEncryptionKey,paymentAttemptParameter payment]) pool
            original <- load checkoutId >>= requireRight
            Execution.porRedirectUrl original `shouldSatisfy` (/= Nothing)
            result <- parsedQuery payment Adapter.AdapterSucceeded
            runSqlPool (Reconciliation.applyQueryResult payment result "held-recovery" notificationTime)
              pool `shouldReturn` Right Reconciliation.ReconciliationDeadLetter
            held <- load checkoutId >>= requireRight
            Execution.porStatus held `shouldBe` "ambiguous"
            Execution.porOutcomeCertainty held `shouldBe` ProviderAmbiguous
            Execution.porRedirectUrl held `shouldBe` Nothing
            Execution.porReference held `shouldBe` Execution.porReference original
            Execution.porProviderResourceId held `shouldBe` Execution.porProviderResourceId original
            load "wrong-synthetic-token" >>= (`shouldSatisfy` isLeft)
            withRecoveryEnvironment $ do
              let _ :<|> getSession :<|> _ = providerExecutionServer
                  request token = runHandler (runReaderT (getSession checkoutId attemptId (Just token))
                    (queryEnv pool))
              dto <- request checkoutId >>= requireRight
              pssState dto `shouldBe` "ambiguous"
              pssOutcomeCertainty dto `shouldBe` "ambiguous"
              pssRedirectUrl dto `shouldBe` Nothing
              pssCanRetryOrFallback dto `shouldBe` False
              request "wrong-synthetic-token" >>= assertHttpError 404

          it "fails closed if a retained review record has conflicting money" $ \pool -> do
            payment <- closedPaymentFixture pool provider closedStatus
            result <- parsedQuery payment Adapter.AdapterSucceeded
            runSqlPool (Reconciliation.applyQueryResult payment result "first-review" notificationTime)
              pool `shouldReturn` Right Reconciliation.ReconciliationDeadLetter
            runSqlPool (rawExecute
              "UPDATE commerce_reconciliation_exception SET actual_amount_minor=1\
              \ WHERE internal_reference=? AND exception_type='verified_payment_on_closed_checkout'"
              [PersistText (Checkout.checkoutReferenceId (Execution.bppCheckout payment))]) pool
            snapshot <- runSqlPool (paymentSnapshot payment) pool
            review <- runSqlPool (closedPaymentReviewSnapshot payment) pool
            runSqlPool (Reconciliation.applyQueryResult payment result "conflicting-review" notificationTime)
              pool >>= (`shouldSatisfy` isLeft)
            runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot
            runSqlPool (closedPaymentReviewSnapshot payment) pool `shouldReturn` review

closedPaymentFixture
  :: ConnectionPool -> Checkout.PaymentProvider -> Text -> IO Execution.BoundProviderPayment
closedPaymentFixture pool provider status = do
  payment <- reconciliationFixture pool provider
  runSqlPool (rawExecute "UPDATE commerce_checkout_session SET status=? WHERE id=?::uuid"
    [PersistText status, PersistText (Checkout.checkoutReferenceId (Execution.bppCheckout payment))]) pool
  pure payment

closedPaymentFinancialSnapshot :: Execution.BoundProviderPayment -> SqlPersistT IO [Single Text]
closedPaymentFinancialSnapshot payment = rawSql
  "SELECT jsonb_build_object(\
  \ 'checkout',to_jsonb(checkout),'attempt',to_jsonb(attempt),'intent',to_jsonb(intent),\
  \ 'operation',to_jsonb(operation.*),\
  \ 'ledger',(SELECT jsonb_agg(to_jsonb(txn) ORDER BY txn.id) FROM commerce_ledger_transaction txn\
  \ WHERE source_id=attempt.id::text),\
  \ 'receipts',(SELECT jsonb_agg(to_jsonb(receipt) ORDER BY receipt.id) FROM commerce_receipt receipt\
  \ WHERE checkout_id=checkout.id),\
  \ 'history',(SELECT jsonb_agg(to_jsonb(history) ORDER BY history.id) FROM commerce_payment_state_history history\
  \ WHERE payment_intent_id=intent.id))::text\
  \ FROM commerce_payment_attempt attempt\
  \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id\
  \ JOIN commerce_payment_intent intent ON intent.id=attempt.payment_intent_id\
  \ JOIN commerce_provider_operation operation ON operation.payment_attempt_id=attempt.id\
  \ WHERE attempt.id=?::uuid"
  [paymentAttemptParameter payment]

closedPaymentReviewSnapshot :: Execution.BoundProviderPayment -> SqlPersistT IO [Single Text]
closedPaymentReviewSnapshot payment = rawSql
  "SELECT to_jsonb(review)::text FROM commerce_reconciliation_exception review\
  \ WHERE internal_reference=? AND exception_type='verified_payment_on_closed_checkout' ORDER BY id"
  [PersistText (Checkout.checkoutReferenceId (Execution.bppCheckout payment))]

assertClosedPaymentEvidence :: ConnectionPool -> Execution.BoundProviderPayment -> Expectation
assertClosedPaymentEvidence pool payment = do
  rows <- runSqlPool (rawSql
    "SELECT exception.expected_amount_minor,exception.actual_amount_minor,exception.currency,\
    \ exception.provider,exception.environment,exception.merchant_account_ref,exception.detected_at,\
    \ (SELECT count(*) FROM commerce_checkout_audit_event audit\
    \ WHERE audit.checkout_id=?::uuid AND audit.event_type='verified_payment_on_closed_checkout'\
    \ AND audit.metadata->>'exception_id'=exception.id::text\
    \ AND audit.metadata->>'attempt_id'=?)\
    \ FROM commerce_reconciliation_exception exception WHERE exception.internal_reference=?\
    \ AND exception.provider_reference=? AND exception.exception_type='verified_payment_on_closed_checkout'"
    [ PersistText checkoutId, paymentAttemptParameter payment, PersistText checkoutId
    , PersistText (Execution.bppProviderResourceId payment)]) pool
    :: IO [(Single Int64, Single Int64, Single Text, Single Text, Single Text,
      Single Text, Single UTCTime, Single Int64)]
  rows `shouldBe` [(Single (Execution.bppAmountMinor payment),Single (Execution.bppAmountMinor payment),
    Single (Execution.bppCurrency payment),Single (Checkout.paymentProviderText (Execution.bppProvider payment)),
    Single "sandbox",Single (Execution.bppMerchantRef payment),Single notificationTime,Single 1)]
  where checkoutId = Checkout.checkoutReferenceId (Execution.bppCheckout payment)

captureReplaySpec :: SpecWith ConnectionPool
captureReplaySpec = describe "verified capture replay integrity" $
  forM_ [Checkout.ProviderDatafast, Checkout.ProviderPayPal,
      Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone] $ \provider ->
    describe (T.unpack (Checkout.paymentProviderText provider)) $ do
      it "preserves the complete financial snapshot on later and concurrent replays" $ \pool -> do
        payment <- captureFixture pool provider
        runSqlPool (Checkout.recordVerifiedPayment payment) pool `shouldReturn` Right True
        snapshot <- runSqlPool (captureSnapshot payment) pool
        let replay = payment { Checkout.vpOccurredAt = addUTCTime 3600 (Checkout.vpOccurredAt payment)
                             , Checkout.vpCorrelationId = "synthetic-later-replay" }
        outcomes <- concurrently (replicate 4 (runSqlPool (Checkout.recordVerifiedPayment replay) pool))
        outcomes `shouldBe` replicate 4 (Right False)
        runSqlPool (captureSnapshot payment) pool `shouldReturn` snapshot

      it "rejects same-amount receipts belonging to another provider or external reference" $ \pool ->
        forM_ ["adapter='synthetic-other-provider'", "external_reference='synthetic-other-reference'"
          , "external_reference=NULL", "receipt_number=receipt_number||'-other'"
          , "amount_minor=1", "currency='EUR'"] $ \change -> do
          payment <- captureFixture pool provider
          runSqlPool (Checkout.recordVerifiedPayment payment) pool `shouldReturn` Right True
          runSqlPool (rawExecute ("UPDATE commerce_receipt SET " <> change <> " WHERE checkout_id=?::uuid")
            [PersistText (Checkout.checkoutReferenceId (Checkout.vpCheckout payment))]) pool
          snapshot <- runSqlPool (captureSnapshot payment) pool
          runSqlPool (Checkout.recordVerifiedPayment payment) pool >>= (`shouldSatisfy` isLeft)
          runSqlPool (captureSnapshot payment) pool `shouldReturn` snapshot

      it "acknowledges original capture after refund or dispute without reopening checkout" $ \pool ->
        forM_ [("partially_refunded", 100), ("refunded", 12515), ("disputed", 0), ("chargeback", 0)] $ \(status, refunded) -> do
          payment <- captureFixture pool provider
          runSqlPool (Checkout.recordVerifiedPayment payment) pool `shouldReturn` Right True
          runSqlPool (do
            rawExecute "UPDATE commerce_checkout_session SET status=?,refunded_minor=? WHERE id=?::uuid"
              [PersistText status,PersistInt64 refunded,captureCheckoutParameter payment]
            rawExecute "UPDATE commerce_payment_intent SET status=?,refunded_minor=? WHERE checkout_id=?::uuid"
              [PersistText status,PersistInt64 refunded,captureCheckoutParameter payment]
            rawExecute "UPDATE commerce_receipt SET voided_at=NOW() WHERE checkout_id=?::uuid"
              [captureCheckoutParameter payment]) pool
          snapshot <- runSqlPool (captureSnapshot payment) pool
          runSqlPool (Checkout.recordVerifiedPayment payment
            { Checkout.vpOccurredAt = addUTCTime 3600 (Checkout.vpOccurredAt payment) }) pool
            `shouldReturn` Right False
          runSqlPool (captureSnapshot payment) pool `shouldReturn` snapshot

      it "does not rebuild missing receipts or accept duplicate historical receipts" $ \pool ->
        forM_ ["DELETE FROM commerce_receipt WHERE checkout_id=?::uuid"
          , "INSERT INTO commerce_receipt(checkout_id,receipt_number,kind,adapter,external_reference,amount_minor,currency,issued_at) SELECT checkout_id,receipt_number||'-duplicate',kind,adapter,external_reference,amount_minor,currency,issued_at FROM commerce_receipt WHERE checkout_id=?::uuid"] $ \corrupt -> do
          payment <- captureFixture pool provider
          runSqlPool (Checkout.recordVerifiedPayment payment) pool `shouldReturn` Right True
          runSqlPool (rawExecute corrupt [captureCheckoutParameter payment]) pool
          snapshot <- runSqlPool (captureSnapshot payment) pool
          runSqlPool (Checkout.recordVerifiedPayment payment) pool >>= (`shouldSatisfy` isLeft)
          runSqlPool (captureSnapshot payment) pool `shouldReturn` snapshot

      it "rejects incomplete historical money instead of silently completing it" $ \pool ->
        forM_ ["UPDATE commerce_checkout_session SET paid_minor=1 WHERE id=?::uuid"
          , "UPDATE commerce_checkout_session SET status='paid',paid_minor=total_minor WHERE id=?::uuid"
          , "UPDATE commerce_payment_attempt SET status='succeeded' WHERE checkout_id=?::uuid"] $ \corrupt -> do
          payment <- captureFixture pool provider
          runSqlPool (rawExecute corrupt [captureCheckoutParameter payment]) pool
          snapshot <- runSqlPool (captureSnapshot payment) pool
          runSqlPool (Checkout.recordVerifiedPayment payment) pool >>= (`shouldSatisfy` isLeft)
          runSqlPool (captureSnapshot payment) pool `shouldReturn` snapshot

      it "preserves successful and closed attempt evidence during binding replay" $ \pool -> do
        payment <- captureFixture pool provider
        runSqlPool (Checkout.recordVerifiedPayment payment) pool `shouldReturn` Right True
        snapshot <- runSqlPool (captureSnapshot payment) pool
        let binding = (captureBinding payment)
              { Checkout.pbcOccurredAt = addUTCTime 3600 (Checkout.vpOccurredAt payment) }
        outcomes <- concurrently (replicate 4 (runSqlPool (Checkout.bindProviderResource binding) pool))
        outcomes `shouldBe` replicate 4 (Right ())
        runSqlPool (captureSnapshot payment) pool `shouldReturn` snapshot
        forM_ ["failed", "cancelled", "expired", "requires_review"] $ \status -> do
          other <- captureFixture pool provider
          runSqlPool (rawExecute "UPDATE commerce_payment_attempt SET status=? WHERE id=?::uuid"
            [PersistText status,PersistText (Checkout.paymentAttemptReferenceId (Checkout.vpAttempt other))]) pool
          closed <- runSqlPool (captureSnapshot other) pool
          runSqlPool (Checkout.bindProviderResource (captureBinding other)) pool `shouldReturn` Right ()
          runSqlPool (captureSnapshot other) pool `shouldReturn` closed

      it "advances customer action once and never regresses processing during binding replay" $ \pool -> do
        payment <- captureFixtureWithStage Checkout.AttemptRequiresCustomerAction pool provider
        runSqlPool (Checkout.bindProviderResource (captureBinding payment)) pool `shouldReturn` Right ()
        snapshot <- runSqlPool (captureSnapshot payment) pool
        runSqlPool (Checkout.bindProviderResource (captureBinding payment)
          { Checkout.pbcStage = Checkout.AttemptRequiresCustomerAction
          , Checkout.pbcOccurredAt = addUTCTime 3600 (Checkout.vpOccurredAt payment) }) pool `shouldReturn` Right ()
        runSqlPool (captureSnapshot payment) pool `shouldReturn` snapshot
        runSqlPool (Checkout.recordVerifiedPayment payment) pool `shouldReturn` Right True

      it "rejects cross-checkout bindings before inserting another resource or audit" $ \pool -> do
        payment <- captureFixture pool provider
        other <- captureFixture pool provider
        snapshots <- mapM (\value -> runSqlPool (captureSnapshot value) pool) [payment,other]
        countBefore <- runSqlPool (rawSql "SELECT count(*) FROM commerce_provider_binding" []) pool :: IO [Single Int64]
        runSqlPool (Checkout.bindProviderResource (captureBinding payment)
          { Checkout.pbcCheckout = Checkout.vpCheckout other
          , Checkout.pbcProviderResource = "synthetic-unbound-new-resource"
          , Checkout.pbcResourceType = "synthetic-other-resource" }) pool >>= (`shouldSatisfy` isLeft)
        mapM (\value -> runSqlPool (captureSnapshot value) pool) [payment,other] `shouldReturn` snapshots
        runSqlPool (rawSql "SELECT count(*) FROM commerce_provider_binding" []) pool `shouldReturn` countBefore

      it "still rejects first capture on expired or cancelled checkouts" $ \pool ->
        forM_ ["expired", "cancelled"] $ \status -> do
          payment <- captureFixture pool provider
          runSqlPool (rawExecute "UPDATE commerce_checkout_session SET status=? WHERE id=?::uuid"
            [PersistText status,captureCheckoutParameter payment]) pool
          snapshot <- runSqlPool (captureSnapshot payment) pool
          runSqlPool (Checkout.recordVerifiedPayment payment) pool >>= (`shouldSatisfy` isLeft)
          runSqlPool (captureSnapshot payment) pool `shouldReturn` snapshot

captureFixture :: ConnectionPool -> Checkout.PaymentProvider -> IO Checkout.VerifiedPayment
captureFixture = captureFixtureWithStage Checkout.AttemptProcessing

captureFixtureWithStage
  :: Checkout.PaymentAttemptStage -> ConnectionPool -> Checkout.PaymentProvider -> IO Checkout.VerifiedPayment
captureFixtureWithStage stage pool provider = do
  seed <- newCheckoutForDomain "service_booking" pool
  let creation = seed { Checkout.pacProvider = provider }
      method = case provider of
        Checkout.ProviderPayPal -> MethodPayPalWallet
        Checkout.ProviderPayPhone -> MethodPayPhoneWallet
        _ -> MethodCard
      checkoutId = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
  attempt <- runSqlPool (Runtime.beginPaymentAttemptForMethod method creation) pool >>= requireRight
  let resource = "synthetic-capture-" <> Checkout.paymentAttemptReferenceId attempt
  _ <- runSqlPool (Checkout.bindProviderResource Checkout.ProviderBindingCreation
    { Checkout.pbcAttempt = attempt, Checkout.pbcCheckout = Checkout.pacCheckout creation
    , Checkout.pbcProvider = provider, Checkout.pbcEnvironment = Checkout.CheckoutSandbox
    , Checkout.pbcMerchantRef = Checkout.pacMerchantRef creation, Checkout.pbcResourceType = "payment"
    , Checkout.pbcProviderResource = resource, Checkout.pbcResourcePath = Nothing
    , Checkout.pbcOrderReference = checkoutId, Checkout.pbcAmountMinor = 12515
    , Checkout.pbcCurrency = "USD", Checkout.pbcStage = stage
    , Checkout.pbcOccurredAt = Checkout.pacCreatedAt creation
    , Checkout.pbcCorrelationId = "synthetic-capture-binding" }) pool >>= requireRight
  pure Checkout.VerifiedPayment
    { Checkout.vpAttempt = attempt, Checkout.vpCheckout = Checkout.pacCheckout creation
    , Checkout.vpProvider = provider, Checkout.vpEnvironment = Checkout.CheckoutSandbox
    , Checkout.vpMerchantRef = Checkout.pacMerchantRef creation, Checkout.vpResourceType = "payment"
    , Checkout.vpProviderResource = resource, Checkout.vpProviderResourcePath = Nothing
    , Checkout.vpOrderReference = checkoutId, Checkout.vpProviderReference = checkoutId
    , Checkout.vpAmountMinor = 12515, Checkout.vpCurrency = "USD"
    , Checkout.vpEvidence = "server_to_server", Checkout.vpOccurredAt = Checkout.pacCreatedAt creation
    , Checkout.vpCorrelationId = "synthetic-capture-verification" }

captureBinding :: Checkout.VerifiedPayment -> Checkout.ProviderBindingCreation
captureBinding payment = Checkout.ProviderBindingCreation
  { Checkout.pbcAttempt = Checkout.vpAttempt payment, Checkout.pbcCheckout = Checkout.vpCheckout payment
  , Checkout.pbcProvider = Checkout.vpProvider payment, Checkout.pbcEnvironment = Checkout.vpEnvironment payment
  , Checkout.pbcMerchantRef = Checkout.vpMerchantRef payment, Checkout.pbcResourceType = Checkout.vpResourceType payment
  , Checkout.pbcProviderResource = Checkout.vpProviderResource payment, Checkout.pbcResourcePath = Checkout.vpProviderResourcePath payment
  , Checkout.pbcOrderReference = Checkout.vpProviderReference payment, Checkout.pbcAmountMinor = Checkout.vpAmountMinor payment
  , Checkout.pbcCurrency = Checkout.vpCurrency payment, Checkout.pbcStage = Checkout.AttemptProcessing
  , Checkout.pbcOccurredAt = Checkout.vpOccurredAt payment, Checkout.pbcCorrelationId = "synthetic-binding-replay" }

captureCheckoutParameter :: Checkout.VerifiedPayment -> PersistValue
captureCheckoutParameter = PersistText . Checkout.checkoutReferenceId . Checkout.vpCheckout

manualCaptureReplaySpec :: SpecWith ConnectionPool
manualCaptureReplaySpec = describe "approved bank transfer replay integrity" $ do
  it "requires independent approval and preserves evidence on later settlement replay" $ \pool -> do
    seed <- newCheckoutForDomain "service_booking" pool
    let creation = seed { Checkout.pacProvider = Checkout.ProviderBankTransfer
                        , Checkout.pacOperation = Checkout.OperationManualVerify }
    attempt <- runSqlPool (Runtime.beginPaymentAttempt creation) pool >>= requireRight
    evidence <- toText <$> nextRandom
    let payment = Checkout.VerifiedPayment
          { Checkout.vpAttempt = attempt, Checkout.vpCheckout = Checkout.pacCheckout creation
          , Checkout.vpProvider = Checkout.ProviderBankTransfer
          , Checkout.vpEnvironment = Checkout.CheckoutSandbox
          , Checkout.vpMerchantRef = Checkout.pacMerchantRef creation
          , Checkout.vpResourceType = "manual_evidence", Checkout.vpProviderResource = evidence
          , Checkout.vpProviderResourcePath = Nothing
          , Checkout.vpOrderReference = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
          , Checkout.vpProviderReference = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
          , Checkout.vpAmountMinor = 12515, Checkout.vpCurrency = "USD"
          , Checkout.vpEvidence = "staff_verified_manual"
          , Checkout.vpOccurredAt = Checkout.pacCreatedAt creation
          , Checkout.vpCorrelationId = "synthetic-bank-approval" }
    runSqlPool (do
      rawExecute "INSERT INTO commerce_manual_payment_evidence(id,checkout_id,payment_attempt_id,status)\
        \ VALUES (?::uuid,?::uuid,?::uuid,'awaiting_evidence')"
        [PersistText evidence,captureCheckoutParameter payment,
          PersistText (Checkout.paymentAttemptReferenceId attempt)]
      rawExecute "UPDATE commerce_manual_payment_evidence\
        \ SET status='submitted',customer_reference='synthetic-bank-reference',submitted_amount_minor=12515,\
        \ currency='USD',submitted_at=NOW(),submitted_by=1 WHERE id=?::uuid" [PersistText evidence]) pool
    _ <- runSqlPool (Checkout.bindProviderResource (captureBinding payment)) pool >>= requireRight
    snapshot <- runSqlPool (captureSnapshot payment) pool
    runSqlPool (Checkout.recordApprovedManualPayment payment) pool >>= (`shouldSatisfy` isLeft)
    runSqlPool (captureSnapshot payment) pool `shouldReturn` snapshot
    rejected <- try (runSqlPool (rawExecute "UPDATE commerce_manual_payment_evidence\
      \ SET status='under_review',reviewed_by=1 WHERE id=?::uuid" [PersistText evidence]) pool)
      :: IO (Either SqlError ())
    rejected `shouldSatisfy` isLeft
    runSqlPool (do
      rawExecute "UPDATE commerce_manual_payment_evidence SET status='under_review',reviewed_by=2 WHERE id=?::uuid"
        [PersistText evidence]
      rawExecute "UPDATE commerce_manual_payment_evidence\
        \ SET status='approved',reviewed_at=NOW(),review_notes='Synthetic independently approved evidence' WHERE id=?::uuid"
        [PersistText evidence]) pool
    runSqlPool (Checkout.recordApprovedManualPayment payment) pool `shouldReturn` Right True
    paid <- runSqlPool (captureSnapshot payment) pool
    outcomes <- concurrently (replicate 4 (runSqlPool (Checkout.recordApprovedManualPayment payment
      { Checkout.vpOccurredAt = addUTCTime 3600 (Checkout.vpOccurredAt payment) }) pool))
    outcomes `shouldBe` replicate 4 (Right False)
    runSqlPool (captureSnapshot payment) pool `shouldReturn` paid

captureSnapshot :: Checkout.VerifiedPayment -> SqlPersistT IO [Single Text]
captureSnapshot payment = rawSql
  "SELECT jsonb_build_object('checkout',to_jsonb(checkout),'attempt',to_jsonb(attempt),\
  \ 'intent',to_jsonb(intent),\
  \ 'manual',(SELECT jsonb_agg(to_jsonb(evidence) ORDER BY evidence.id) FROM commerce_manual_payment_evidence evidence WHERE checkout_id=checkout.id),\
  \ 'receipts',(SELECT jsonb_agg(to_jsonb(receipt) ORDER BY receipt.id) FROM commerce_receipt receipt WHERE checkout_id=checkout.id),\
  \ 'ledger',(SELECT jsonb_agg(to_jsonb(txn) ORDER BY txn.id) FROM commerce_ledger_transaction txn WHERE source_id=attempt.id::text),\
  \ 'entries',(SELECT jsonb_agg(to_jsonb(entry) ORDER BY entry.id) FROM commerce_ledger_entry entry JOIN commerce_ledger_transaction txn ON txn.id=entry.transaction_id WHERE txn.source_id=attempt.id::text),\
  \ 'history',(SELECT jsonb_agg(to_jsonb(history) ORDER BY history.id) FROM commerce_payment_state_history history WHERE payment_intent_id=intent.id),\
  \ 'audit',(SELECT jsonb_agg(to_jsonb(audit) ORDER BY audit.id) FROM commerce_checkout_audit_event audit WHERE checkout_id=checkout.id)\
  \ )::text FROM commerce_payment_attempt attempt\
  \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id\
  \ JOIN commerce_payment_intent intent ON intent.id=attempt.payment_intent_id\
  \ WHERE attempt.id=?::uuid"
  [PersistText (Checkout.paymentAttemptReferenceId (Checkout.vpAttempt payment))]

reconciliationTransactionSpec :: SpecWith ConnectionPool
reconciliationTransactionSpec = describe "authoritative query transaction ownership" $ do
  reconciliationPipelineSpec
  forM_ [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone] $ \provider ->
    describe (T.unpack (Checkout.paymentProviderText provider)) $ do
      it "rolls back payment, intent, ledger, receipt and audit when its caller fails" $ \pool -> do
        payment <- reconciliationFixture pool provider
        result <- parsedQuery payment Adapter.AdapterSucceeded
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        failed <- try (runSqlPool (do
          applied <- Reconciliation.applyQueryResult payment result "outer-failure" notificationTime
          liftIO (applied `shouldBe` Right Reconciliation.ReconciliationProcessed)
          liftIO (throwIO (userError "synthetic caller failure"))) pool) :: IO (Either IOException ())
        failed `shouldSatisfy` isLeft
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "preserves the caller savepoint around closed-checkout review evidence" $ \pool -> do
        payment <- reconciliationFixture pool provider
        result <- parsedQuery payment Adapter.AdapterSucceeded
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        runSqlPool (do
          rawExecute "SAVEPOINT caller_owned" []
          rawExecute "UPDATE commerce_checkout_session SET status='expired' WHERE id=?::uuid"
            [PersistText (Checkout.checkoutReferenceId (Execution.bppCheckout payment))]
          expired <- closedPaymentFinancialSnapshot payment
          applied <- Reconciliation.applyQueryResult payment result "expired-query" notificationTime
          liftIO (applied `shouldBe` Right Reconciliation.ReconciliationDeadLetter)
          closedPaymentFinancialSnapshot payment >>= liftIO . (`shouldBe` expired)
          rawExecute "ROLLBACK TO SAVEPOINT caller_owned" []
          rawExecute "RELEASE SAVEPOINT caller_owned" []
          paymentSnapshot payment >>= liftIO . (`shouldBe` snapshot)) pool
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "retains pre-existing row locks until the caller completes" $ \pool -> do
        payment <- reconciliationFixture pool provider
        result <- parsedQuery payment Adapter.AdapterPending
        ready <- newEmptyMVar
        checked <- newEmptyMVar
        outcomes <- concurrently
          [ runSqlPool (do
              _ <- lockOperation payment ""
              applied <- Reconciliation.applyQueryResult payment result "lock-owner" notificationTime
              liftIO (applied `shouldBe` Right Reconciliation.ReconciliationRetry)
              liftIO (putMVar ready ())
              liftIO (takeMVar checked)) pool
          , do
              takeMVar ready
              locked <- try (runSqlPool (lockOperation payment " NOWAIT") pool)
                :: IO (Either SqlError [Single Text])
              let retained = either ((== "55P03") . sqlState) (const False) locked
              putMVar checked retained
              pure retained
          ]
        outcomes `shouldBe` [True, True]

      it "serializes concurrent successes into one capture ledger, receipt and paid audit" $ \pool -> do
        payment <- reconciliationFixture pool provider
        result <- parsedQuery payment Adapter.AdapterSucceeded
        outcomes <- concurrently (replicate 4 (runSqlPool
          (Reconciliation.applyQueryResult payment result "duplicate-query" notificationTime) pool))
        outcomes `shouldBe` replicate 4 (Right Reconciliation.ReconciliationProcessed)
        assertPaymentPosted pool payment

      it "never downgrades a terminal successful operation with reordered results" $ \pool -> do
        payment <- reconciliationFixture pool provider
        succeeded <- parsedQuery payment Adapter.AdapterSucceeded
        runSqlPool (Reconciliation.applyQueryResult payment succeeded "first-success" notificationTime)
          pool `shouldReturn` Right Reconciliation.ReconciliationProcessed
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        forM_ [Adapter.AdapterPending, Adapter.AdapterDeclined, Adapter.AdapterUnknown] $ \state -> do
          stale <- parsedQuery payment state
          runSqlPool (Reconciliation.applyQueryResult payment stale "stale-query" notificationTime)
            pool >>= (`shouldSatisfy` isLeft)
          runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "preserves confirmed no-charge evidence if a later success conflicts" $ \pool -> do
        payment <- reconciliationFixture pool provider
        declined <- parsedQuery payment Adapter.AdapterDeclined
        runSqlPool (Reconciliation.applyQueryResult payment declined "first-decline" notificationTime)
          pool `shouldReturn` Right Reconciliation.ReconciliationProcessed
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        succeeded <- parsedQuery payment Adapter.AdapterSucceeded
        runSqlPool (Reconciliation.applyQueryResult payment succeeded "conflicting-query" notificationTime)
          pool >>= (`shouldSatisfy` isLeft)
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "rejects malformed typed evidence and altered bound identifiers without side effects" $ \pool -> do
        payment <- reconciliationFixture pool provider
        result <- parsedQuery payment Adapter.AdapterSucceeded
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        forM_ [result { Adapter.adapterResultExternalId = "SYNTHETIC-PRIVATE" }
          , result { Adapter.adapterResultAmountMinor = Just 12514 }
          , result { Adapter.adapterResultAmountMinor = Nothing }
          , result { Adapter.adapterResultCurrency = Just "EUR" }
          , result { Adapter.adapterResultCurrency = Nothing }
          , result { Adapter.adapterResultCertainty = ProviderAmbiguous }] $ \invalid -> do
            rejected <- runSqlPool (Reconciliation.applyQueryResult payment invalid
              "invalid-evidence" notificationTime) pool
            rejected `shouldSatisfy` isLeft
            show rejected `shouldNotContain` "SYNTHETIC-PRIVATE"
        forM_ [payment { Execution.bppDomainOrderId = "SYNTHETIC-PRIVATE" }
          , payment { Execution.bppMerchantRef = "SYNTHETIC-PRIVATE" }
          , payment { Execution.bppProviderReference = "SYNTHETIC-PRIVATE" }
          , payment { Execution.bppProviderResourcePath = Just "SYNTHETIC-PRIVATE" }] $ \invalid ->
            runSqlPool (Reconciliation.applyQueryResult invalid result "invalid-binding" notificationTime)
              pool >>= (`shouldSatisfy` isLeft)
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "rolls back the whole transaction on a database error or thread interruption" $ \pool -> do
        payment <- reconciliationFixture pool provider
        result <- parsedQuery payment Adapter.AdapterSucceeded
        snapshot <- runSqlPool (paymentSnapshot payment) pool
        failed <- try (runSqlPool (do
          _ <- Reconciliation.applyQueryResult payment result "sql-error" notificationTime
          _ <- rawSql "SELECT 1/0" [] :: SqlPersistT IO [Single Int]
          pure ()) pool) :: IO (Either SqlError ())
        either ((`shouldBe` "22012") . sqlState) (const (expectationFailure "Expected SQL failure")) failed
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot
        interrupted <- try (runSqlPool (do
          _ <- Reconciliation.applyQueryResult payment result "interrupted" notificationTime
          liftIO (throwIO ThreadKilled)) pool) :: IO (Either AsyncException ())
        interrupted `shouldBe` Left ThreadKilled
        runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

  it "keeps unknown states retryable and rolls back their exception with the caller" $ \pool -> do
    payment <- reconciliationFixture pool Checkout.ProviderPlaceToPay
    result <- parsedQuery payment Adapter.AdapterUnknown
    snapshot <- runSqlPool (paymentSnapshot payment) pool
    runSqlPool (do
      rawExecute "SAVEPOINT caller_owned" []
      applied <- Reconciliation.applyQueryResult payment result "unknown-query" notificationTime
      liftIO (applied `shouldBe` Right Reconciliation.ReconciliationRetry)
      rawExecute "ROLLBACK TO SAVEPOINT caller_owned" []
      rawExecute "RELEASE SAVEPOINT caller_owned" []) pool
    runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

  it "applies confirmed provider cancellation atomically without creating a capture" $ \pool -> do
    payment <- reconciliationFixture pool Checkout.ProviderPlaceToPay
    result <- parsedQuery payment Adapter.AdapterCancelled
    runSqlPool (Reconciliation.applyQueryResult payment result "cancel-query" notificationTime) pool
      `shouldReturn` Right Reconciliation.ReconciliationProcessed
    rows <- runSqlPool (rawSql
      "SELECT operation.status,intent.status FROM commerce_provider_operation operation\
      \ JOIN commerce_payment_attempt attempt ON attempt.id=operation.payment_attempt_id\
      \ JOIN commerce_payment_intent intent ON intent.id=attempt.payment_intent_id\
      \ WHERE attempt.id=?::uuid" [paymentAttemptParameter payment]) pool
      :: IO [(Single Text, Single Text)]
    rows `shouldBe` [(Single "confirmed_no_charge", Single "cancelled")]

  it "rejects unsupported authorization and reversal results without changing history" $ \pool -> do
    payment <- reconciliationFixture pool Checkout.ProviderPlaceToPay
    pendingResult <- parsedQuery payment Adapter.AdapterPending
    snapshot <- runSqlPool (paymentSnapshot payment) pool
    forM_ [Adapter.AdapterAuthorized, Adapter.AdapterReversed] $ \state ->
      runSqlPool (Reconciliation.applyQueryResult payment pendingResult { Adapter.adapterResultState = state }
        "unsupported-query" notificationTime) pool >>= (`shouldSatisfy` isLeft)
    runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

reconciliationPipelineSpec :: SpecWith ConnectionPool
reconciliationPipelineSpec = describe "persisted callback to authoritative query" $
  forM_ [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone] $ \provider ->
    describe (T.unpack (Checkout.paymentProviderText provider)) $ do
      it "uses query status, not callback status, and records response observation time" $ \pool ->
        withNotificationEnvironment $ do
          payment <- reconciliationFixture pool provider
          payload <- reconciliationNotification pool payment
          let observedAt = addUTCTime 15 notificationTime
          calls <- newIORef (0 :: Int)
          let fetch request = do
                modifyIORef' calls (+1)
                Adapter.arOperation request `shouldBe` AdapterQuery
                Adapter.arProvider request `shouldBe` provider
                Adapter.arRetryPolicy request `shouldBe` Adapter.SafeReadRetry
                Adapter.arMethod request `shouldBe` (if provider == Checkout.ProviderPlaceToPay
                  then Adapter.AdapterPost else Adapter.AdapterGet)
                pure (Right (queryValue payment Adapter.AdapterSucceeded))
          outcome <- Reconciliation.processProviderEventWith fetch (pure observedAt)
            (Env pool (error "Synthetic reconciliation must not use AppConfig")) payload notificationTime
          Reconciliation.prrDisposition outcome `shouldBe` Reconciliation.ReconciliationProcessed
          readIORef calls `shouldReturn` 1
          assertPaymentPosted pool payment
          rows <- runSqlPool (rawSql "SELECT paid_at FROM commerce_checkout_session WHERE id=?::uuid"
            [PersistText (Checkout.checkoutReferenceId (Execution.bppCheckout payment))]) pool
            :: IO [Single (Maybe UTCTime)]
          rows `shouldBe` [Single (Just observedAt)]

      it "keeps a timed-out query ambiguous and redacts transport details" $ \pool ->
        withNotificationEnvironment $ do
          payment <- reconciliationFixture pool provider
          payload <- reconciliationNotification pool payment
          snapshot <- runSqlPool (paymentSnapshot payment) pool
          calls <- newIORef (0 :: Int)
          outcome <- Reconciliation.processProviderEventWith
            (\_ -> modifyIORef' calls (+1) >> pure (Left
              (ProviderHttp.AdapterTransportError "SYNTHETIC-PRIVATE timeout"))) getCurrentTime
            (Env pool (error "Synthetic reconciliation must not use AppConfig")) payload notificationTime
          Reconciliation.prrDisposition outcome `shouldBe` Reconciliation.ReconciliationRetry
          show outcome `shouldNotContain` "SYNTHETIC-PRIVATE"
          readIORef calls `shouldReturn` 1
          runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

      it "dead-letters mismatched query money and records a redacted reconciliation exception" $ \pool ->
        withNotificationEnvironment $ do
          payment <- reconciliationFixture pool provider
          payload <- reconciliationNotification pool payment
          outcome <- Reconciliation.processProviderEventWith
            (\_ -> pure (Right (queryValue payment { Execution.bppAmountMinor = 1 }
              Adapter.AdapterSucceeded))) getCurrentTime
            (Env pool (error "Synthetic reconciliation must not use AppConfig")) payload notificationTime
          Reconciliation.prrDisposition outcome `shouldBe` Reconciliation.ReconciliationDeadLetter
          rows <- runSqlPool (rawSql
            "SELECT checkout.status,operation.status,\
            \ (SELECT count(*) FROM commerce_reconciliation_exception WHERE internal_reference=checkout.id::text\
            \   AND exception_type='provider_query_binding_mismatch' AND status='open'),\
            \ (SELECT count(*) FROM commerce_ledger_transaction WHERE source_id=attempt.id::text)\
            \ FROM commerce_payment_attempt attempt\
            \ JOIN commerce_provider_operation operation ON operation.payment_attempt_id=attempt.id\
            \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id WHERE attempt.id=?::uuid"
            [paymentAttemptParameter payment]) pool :: IO [(Single Text, Single Text, Single Int64, Single Int64)]
          rows `shouldBe` [(Single (if provider == Checkout.ProviderPlaceToPay then "awaiting_payment" else "processing"), Single (if provider == Checkout.ProviderPlaceToPay
            then "requires_customer_action" else "processing"), Single 1, Single 0)]

      it "rejects tampered stored callback trust before invoking the query transport" $ \pool ->
        withNotificationEnvironment $ do
          payment <- reconciliationFixture pool provider
          payload <- reconciliationNotification pool payment
          snapshot <- runSqlPool (paymentSnapshot payment) pool
          forM_ [payload { Event.pepSignatureVerified = not (Event.pepSignatureVerified payload) }
            , payload { Event.pepEvidenceType = "SYNTHETIC-PRIVATE" }
            , payload { Event.pepProviderResourceId = Just "different-resource" }
            , payload { Event.pepRawPayload = "{\"SYNTHETIC-PRIVATE\":" }] $ \tampered -> do
              outcome <- Reconciliation.processProviderEventWith
                (\_ -> fail "Invalid callback must not reach a remote query") getCurrentTime
                (Env pool (error "Synthetic reconciliation must not use AppConfig")) tampered notificationTime
              Reconciliation.prrDisposition outcome `shouldBe` Reconciliation.ReconciliationDeadLetter
              show outcome `shouldNotContain` "SYNTHETIC-PRIVATE"
          runSqlPool (paymentSnapshot payment) pool `shouldReturn` snapshot

reconciliationNotification :: ConnectionPool -> Execution.BoundProviderPayment -> IO Event.ProviderEventPayload
reconciliationNotification pool payment = do
  resetQueryBudget pool (Execution.bppProvider payment)
  creation <- newNotification (Execution.bppProvider payment)
  let resource = read (T.unpack (Execution.bppProviderResourceId payment)) :: Int64
      -- Intentionally report failure in the callback; only the subsequent
      -- authenticated query is allowed to establish the financial outcome.
      raw = if Execution.bppProvider payment == Checkout.ProviderPlaceToPay
        then signedPlaceToPayNotification resource "REJECTED" "2026-09-14T12:00:00Z" "synthetic-secret" ""
        else encodeStrict (A.object ["TransactionId" A..= resource
          , "ClientTransactionId" A..= Execution.bppProviderReference payment
          , "StoreId" A..= ("synthetic-store" :: Text), "StatusCode" A..= (2 :: Int)])
  stored <- storeNotification pool creation
    { Event.pecMerchantRef = Execution.bppMerchantRef payment
    , Event.pecProviderResource = Just (Execution.bppProviderResourceId payment)
    , Event.pecRawPayload = raw } >>= requireRight
  claim <- runSqlPool (Event.claimProviderEvent (Event.pesReference stored) notificationTime) pool
  claim `shouldBe` Event.ProviderEventClaimed 1
  runSqlPool (Event.loadProviderEventPayload (Event.pesReference stored) recoveryEncryptionKey)
    pool >>= requireRight

reconciliationFixture :: ConnectionPool -> Checkout.PaymentProvider -> IO Execution.BoundProviderPayment
reconciliationFixture = reconciliationFixtureWithMerchant "synthetic-provider-retry-merchant"

reconciliationFixtureWithMerchant
  :: Text -> ConnectionPool -> Checkout.PaymentProvider -> IO Execution.BoundProviderPayment
reconciliationFixtureWithMerchant merchant pool provider = do
  -- Exercise TDF service revenue, not a ticket order without its fee/seat
  -- snapshot. Ticket fulfillment is covered by its separate runtime harness.
  (creation, operation, _) <- replayFixtureWithMerchant "service_booking" merchant pool provider
  bindReconciliationFixture pool creation operation
    (providerReference provider (Checkout.checkoutReferenceId (Checkout.pacCheckout creation)))

bindReconciliationFixture
  :: ConnectionPool -> Checkout.PaymentAttemptCreation -> Execution.ProviderOperationRecord
  -> Text -> IO Execution.BoundProviderPayment
bindReconciliationFixture pool creation operation reference = do
  let checkoutId = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
      attempt = Execution.porAttempt operation
      provider = Checkout.pacProvider creation
      state = if provider == Checkout.ProviderPlaceToPay
        then Adapter.AdapterRequiresCustomerAction else Adapter.AdapterPending
  resourceUuid <- toText <$> nextRandom
  resource <- case readHex (T.unpack (T.take 12 (T.filter (/= '-') resourceUuid))) of
    [(number, "")] -> pure (T.pack (show (number + 1 :: Int64)))
    _ -> fail "Expected synthetic numeric provider resource"
  checkout <- runSqlPool (Execution.loadAuthorizedCheckout checkoutId (digestText checkoutId)
    (Checkout.pacCreatedAt creation)) pool >>= requireRight
  _ <- runSqlPool (Execution.claimProviderOperation (Execution.porReference operation)
    (Checkout.pacCreatedAt creation) recoveryEncryptionKey) pool >>= requireRight
  _ <- runSqlPool (Execution.recordCreateResult (Execution.porReference operation) checkout attempt
    provider (Checkout.pacMerchantRef creation) reference recoveryEncryptionKey
    (Adapter.AdapterResult state resource Nothing Nothing Nothing ProviderAmbiguous)
    "synthetic-query-binding" (Checkout.pacCreatedAt creation)) pool >>= requireRight
  runSqlPool (Execution.loadBoundProviderPayment provider Checkout.CheckoutSandbox
    (Checkout.pacMerchantRef creation) resource (Just reference)) pool >>= requireRight

parsedQuery :: Execution.BoundProviderPayment -> Adapter.AdapterResultState -> IO Adapter.AdapterResult
parsedQuery payment state = do
  adapter <- if Execution.bppProvider payment == Checkout.ProviderPlaceToPay
    then requireRight (PlaceToPay.placeToPayAdapter notificationConfig)
    else requireRight (PayPhone.payPhoneAdapter (PayPhone.PayPhoneConfig "synthetic-token" "synthetic-store"))
  requireRight (Adapter.adapterParseResponse adapter AdapterQuery
    (Adapter.PaymentLocator (Execution.bppProviderResourceId payment)
      (Adapter.ExpectedPayment (Execution.bppProviderReference payment)
        (Execution.bppAmountMinor payment) (Execution.bppCurrency payment)))
    (queryValue payment state))

queryValue :: Execution.BoundProviderPayment -> Adapter.AdapterResultState -> A.Value
queryValue payment state
  | Execution.bppProvider payment == Checkout.ProviderPlaceToPay = A.object
      [ "requestId" A..= resource, "status" A..= status
      , "request" A..= A.object ["payment" A..= A.object
          ["reference" A..= reference, "amount" A..= amount]]
      , "payment" A..= if state == Adapter.AdapterSucceeded then
          [A.object ["status" A..= status, "refunded" A..= False
            , "internalReference" A..= (1 :: Int), "reference" A..= reference
            , "amount" A..= A.object ["from" A..= amount, "to" A..= amount]]]
          else ([] :: [A.Value]) ]
  | otherwise = A.object
      ["transactionId" A..= resource, "clientTransactionId" A..= reference
      , "amount" A..= Execution.bppAmountMinor payment, "currency" A..= Execution.bppCurrency payment
      , "statusCode" A..= (case state of
          Adapter.AdapterSucceeded -> 3; Adapter.AdapterDeclined -> 2; Adapter.AdapterCancelled -> 2
          Adapter.AdapterPending -> 1; _ -> 99 :: Int)]
  where
    resource = read (T.unpack (Execution.bppProviderResourceId payment)) :: Int64
    reference = Execution.bppProviderReference payment
    amount = A.object ["currency" A..= Execution.bppCurrency payment
      , "total" A..= Adapter.minorToDecimal (Execution.bppAmountMinor payment)]
    status = A.object ["status" A..= (case state of
      Adapter.AdapterSucceeded -> "APPROVED"; Adapter.AdapterDeclined -> "REJECTED"
      Adapter.AdapterCancelled -> "CANCELLED"; Adapter.AdapterPending -> "PENDING"
      _ -> "UNKNOWN" :: Text)]

paymentAttemptParameter :: Execution.BoundProviderPayment -> PersistValue
paymentAttemptParameter = PersistText . Checkout.paymentAttemptReferenceId . Execution.bppAttempt

lockOperation :: Execution.BoundProviderPayment -> Text -> SqlPersistT IO [Single Text]
lockOperation payment lockSuffix = rawSql
  ("SELECT id::text FROM commerce_provider_operation WHERE payment_attempt_id=?::uuid FOR UPDATE" <> lockSuffix)
  [paymentAttemptParameter payment]

paymentSnapshot :: Execution.BoundProviderPayment -> SqlPersistT IO [Single Text]
paymentSnapshot payment = rawSql
  "SELECT jsonb_build_object(\
  \ 'operation',jsonb_build_array(operation.status,operation.outcome_certainty,operation.updated_at,operation.completed_at),\
  \ 'checkout',jsonb_build_array(checkout.status,checkout.paid_minor,checkout.paid_at,checkout.updated_at),\
  \ 'attempt',jsonb_build_array(attempt.status,attempt.updated_at,attempt.failure_code,attempt.failure_summary),\
  \ 'intent',jsonb_build_array(intent.status,intent.authorized_minor,intent.captured_minor,intent.updated_at),\
  \ 'ledger',(SELECT count(*) FROM commerce_ledger_transaction WHERE source_id=attempt.id::text),\
  \ 'entries',(SELECT count(*) FROM commerce_ledger_entry entry JOIN commerce_ledger_transaction txn\
  \   ON txn.id=entry.transaction_id WHERE txn.source_id=attempt.id::text),\
  \ 'receipts',(SELECT count(*) FROM commerce_receipt WHERE checkout_id=checkout.id),\
  \ 'history',(SELECT count(*) FROM commerce_payment_state_history WHERE payment_intent_id=intent.id),\
  \ 'audit',(SELECT count(*) FROM commerce_checkout_audit_event WHERE checkout_id=checkout.id),\
  \ 'exceptions',(SELECT count(*) FROM commerce_reconciliation_exception WHERE internal_reference=checkout.id::text)\
  \ )::text FROM commerce_provider_operation operation\
  \ JOIN commerce_payment_attempt attempt ON attempt.id=operation.payment_attempt_id\
  \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id\
  \ JOIN commerce_payment_intent intent ON intent.id=attempt.payment_intent_id\
  \ WHERE attempt.id=?::uuid"
  [paymentAttemptParameter payment]

assertPaymentPosted :: ConnectionPool -> Execution.BoundProviderPayment -> Expectation
assertPaymentPosted pool payment = do
  rows <- runSqlPool (rawSql
    "SELECT checkout.status,attempt.status,intent.status,checkout.paid_minor,\
    \ (SELECT count(*) FROM commerce_ledger_transaction WHERE source_id=attempt.id::text AND status='posted'),\
    \ (SELECT count(*) FROM commerce_receipt WHERE checkout_id=checkout.id AND kind='payment_receipt'),\
    \ (SELECT count(*) FROM commerce_checkout_audit_event WHERE checkout_id=checkout.id AND event_type='payment_verified')\
    \ FROM commerce_payment_attempt attempt\
    \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id\
    \ JOIN commerce_payment_intent intent ON intent.id=attempt.payment_intent_id WHERE attempt.id=?::uuid"
    [paymentAttemptParameter payment]) pool
    :: IO [(Single Text, Single Text, Single Text, Single Int64, Single Int64, Single Int64, Single Int64)]
  rows `shouldBe` [(Single "paid", Single "succeeded", Single "captured", Single 12515, Single 1, Single 1, Single 1)]

digestText :: Text -> Text
digestText value = TE.decodeUtf8
  (BAE.convertToBase BAE.Base16 (hash (TE.encodeUtf8 value) :: Digest SHA256))

replayFixture :: ConnectionPool -> Checkout.PaymentProvider
  -> IO (Checkout.PaymentAttemptCreation, Execution.ProviderOperationRecord, PaymentSessionCreateDTO)
replayFixture = replayFixtureForDomain "event_ticket_order"

replayFixtureForDomain :: Text -> ConnectionPool -> Checkout.PaymentProvider
  -> IO (Checkout.PaymentAttemptCreation, Execution.ProviderOperationRecord, PaymentSessionCreateDTO)
replayFixtureForDomain domain = replayFixtureWithMerchant domain "synthetic-provider-retry-merchant"

replayFixtureWithMerchant :: Text -> Text -> ConnectionPool -> Checkout.PaymentProvider
  -> IO (Checkout.PaymentAttemptCreation, Execution.ProviderOperationRecord, PaymentSessionCreateDTO)
replayFixtureWithMerchant domain merchant pool provider = do
  seed <- newCheckoutForDomain domain pool
  let creation = seed { Checkout.pacProvider = provider, Checkout.pacMerchantRef = merchant }
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
newCheckout = newCheckoutForDomain "event_ticket_order"

newCheckoutForDomain :: Text -> ConnectionPool -> IO Checkout.PaymentAttemptCreation
newCheckoutForDomain domain pool = do
  checkoutId <- toText <$> nextRandom
  now <- getCurrentTime
  runSqlPool (rawExecute
    "INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,environment,\
    \ currency,subtotal_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at)\
    \ VALUES (?::uuid,?,?,'awaiting_payment','sandbox','USD',12515,12515,\
    \ 'synthetic@example.test',?,?,?)"
    [PersistText checkoutId, PersistText domain, PersistText checkoutId, PersistText checkoutId, PersistText checkoutId,
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
