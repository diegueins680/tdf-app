{-# LANGUAGE OverloadedStrings #-}

-- | PAY-DOMO-CANCEL-001 against the complete reviewed PostgreSQL schema:
-- cancellation records, refund obligations and the money invariants of
-- TDF.Commerce.DomoCancellationStore. Runs inside the provider-retry-runtime
-- gate (TDF_PROVIDER_RETRY_DATABASE_URL).
module TDF.Commerce.DomoCancellationRuntimeSpec (spec, databaseSpec) where

import           Control.Exception (SomeException, try, bracket, bracket_)
import           Control.Monad (void, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import           Control.Concurrent.Async (mapConcurrently, concurrently, withAsync, wait)
import           System.Timeout (timeout)
import           Data.Either (isLeft)
import           Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import qualified TDF.Commerce.CheckoutStore as C
import qualified TDF.Commerce.PaymentRuntimeStore as PaymentRuntime
import qualified TDF.Commerce.DomoRefundStore as D
import qualified TDF.Commerce.RefundStore as R
import qualified TDF.Commerce.RefundReconciliation as Recovery
import qualified TDF.Commerce.ProviderAdapter.PayPalRefund as Q
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Crypto.Hash (Digest, SHA256, hash)
import           Data.Time (UTCTime, addUTCTime, getCurrentTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue (..))
import           Database.Persist.Sql (ConnectionPool, Single (..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import           Test.Hspec

import           TDF.Commerce.DomoCancellation (Canceller (..))
import           TDF.Commerce.DomoCancellationStore
import           TDF.Commerce.RefundStore (RefundReference (..))
import           TDF.Server.ServiceStorefront (ServicePaidCheckout (..), loadDomoCancellationRefund)

import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Client as HC
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types.Status (statusCode)
import Data.Proxy (Proxy(..))
import TDF.Config (loadConfig)
import qualified TDF.Routes.DomoQuotes as PublicRoutes
import qualified TDF.Server.DomoQuoteCheckout as PublicDomo
import qualified TDF.Commerce.ProviderAdapter.Http as Http
import qualified TDF.Server.DomoRefunds as API
import TDF.Routes.DomoRefunds (ManualRefundEvidence(..), RefundProviderReference(..))
import TDF.Auth (AuthedUser(..), loadAuthedUser)
import TDF.Models (RoleEnum(..))
import TDF.DB (Env(..))
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Servant ( errHTTPCode, runHandler, serve, hoistServer, (:<|>)(..))
import System.Environment (getEnvironment, setEnv, unsetEnv)

spec :: Spec
spec = describe "Domo refund authorization" $ do
  forM_ [[],[Customer],[Fan],[Accounting],[Webmaster],[StudioManager],[Admin,Manager]] $ \roles ->
    it ("refuses every refund action before database/provider access for "<>show roles) $ do
      let readStatus :<|> approve :<|> execute :<|> reconcile :<|> manual :<|> confirm :<|> bindReference =
            API.domoRefundAdminServer (AuthedUser (toSqlKey 2) roles mempty Nothing Nothing) "invalid-id"
      now <- getCurrentTime
      forM_ [readStatus,approve,execute,reconcile,manual (ManualRefundEvidence "receipt" "evidence-reference" 100 "USD" now),confirm,bindReference (RefundProviderReference "synthetic-refund")] $ \action -> do
        result <- runHandler (runReaderT action (error "Unauthorized caller accessed Env"))
        either errHTTPCode (const 200) result `shouldBe` 403

data Fixture = Fixture
  { fxQuoteId    :: Text
  , fxCheckoutId :: Text
  , fxStartsAt   :: UTCTime
  }

depositMinor :: Int64
depositMinor = 200704

-- | A Domo quote priced like the migration rehearsal, starting @daysAhead@ days
-- from now, optionally with its deposit verified through a PayPal capture.
-- Immutable lookup snapshots must contain a real hash of a known synthetic
-- token from creation; an HTTP fixture must never rewrite the bound hash.
-- Full UUID entropy preserves the original collision fix (the historical
-- first-byte construction provided only 256 distinct fixture values).
fixtureLookupToken :: Text -> Text
fixtureLookupToken identifier = "synthetic-lookup:" <> identifier

tokenHash :: Text -> Text
tokenHash identifier = T.pack (show (hash (TE.encodeUtf8 (fixtureLookupToken identifier)) :: Digest SHA256))

seedQuote :: ConnectionPool -> Int -> Bool -> IO Fixture
seedQuote = seedQuoteForProvider "paypal"

seedQuoteForProvider :: Text -> ConnectionPool -> Int -> Bool -> IO Fixture
seedQuoteForProvider provider pool daysAhead paid = do
  now <- getCurrentTime
  quoteId <- toText <$> nextRandom
  commerceQuoteId <- toText <$> nextRandom
  checkoutId <- toText <$> nextRandom
  attemptId <- toText <$> nextRandom
  bindingId <- toText <$> nextRandom
  let startsAt = addUTCTime (fromIntegral daysAhead * 86400 + 3600) now
      endsAt = addUTCTime (8 * 3600) startsAt
      setupStartsAt = addUTCTime (-2 * 3600) startsAt
      holdExpiresAt = addUTCTime 900 now
      rulesHashSql = "(SELECT encode(digest(convert_to(pricing_rules::text,'UTF8'),'sha256'),'hex')\
                     \ FROM commerce_product_version WHERE id='81000000-0000-0000-0000-000000000001')"
  runSqlPool (do
    -- The runtime insert trigger requires the exact approved active rate card.
    rawExecute
      "UPDATE commerce_rate_card_review SET status='approved', reviewed_by=1, reviewed_at=NOW()\
      \ WHERE product_version_id='81000000-0000-0000-0000-000000000001' AND status<>'approved'" []
    rawExecute
      "UPDATE commerce_product_version SET status='active', approved_by=1, approved_at=NOW()\
      \ WHERE id='81000000-0000-0000-0000-000000000001' AND status<>'active'" []
    rawExecute
      ("INSERT INTO commerce_quote(id, domain_type, domain_subject_id, version, status, currency,\
       \ subtotal_minor, tax_minor, total_minor, deposit_minor, expires_at)\
       \ VALUES (?::uuid, 'domo_event_quote', ?, 1, 'sent', 'USD', 448000, 53760, 501760, ?, ?)")
      [PersistText commerceQuoteId, PersistText quoteId, PersistInt64 depositMinor, PersistUTCTime holdExpiresAt]
    rawExecute
      "INSERT INTO commerce_quote_line(quote_id, line_number, product_type, product_id, product_version,\
      \ description, quantity, unit_amount_minor, subtotal_minor) VALUES\
      \ (?::uuid,1,'domo_rate_component','venue_base','legacy_public_formula-v1','Domo event base',1,180000,180000),\
      \ (?::uuid,2,'domo_rate_component','venue_hours','legacy_public_formula-v1','Domo venue hours',8,18000,144000),\
      \ (?::uuid,3,'domo_rate_component','setup_hours','legacy_public_formula-v1','Setup and teardown hours',2,7000,14000),\
      \ (?::uuid,4,'domo_rate_component','additional_guests','legacy_public_formula-v1','Additional guests',20,800,16000),\
      \ (?::uuid,5,'domo_rate_component','catering','legacy_public_formula-v1','Catering and bar',1,52000,52000),\
      \ (?::uuid,6,'domo_rate_component','production','legacy_public_formula-v1','Sound and lighting',1,42000,42000)"
      (replicate 6 (PersistText commerceQuoteId))
    rawExecute
      "INSERT INTO commerce_checkout_session(id, domain_type, domain_order_id, quote_id, status, environment,\
      \ currency, subtotal_minor, total_minor, customer_email, lookup_token_hash, idempotency_key, expires_at)\
      \ VALUES (?::uuid, 'domo_event_quote', ?, ?::uuid, 'holding', 'sandbox', 'USD', ?, ?, 'domo@example.com',\
      \ ?, ?, ?)"
      [ PersistText checkoutId, PersistText ("domo-quote:" <> quoteId), PersistText commerceQuoteId
      , PersistInt64 depositMinor, PersistInt64 depositMinor
      , PersistText (tokenHash quoteId)
      , PersistText ("domo-cancel-fixture:" <> quoteId), PersistUTCTime holdExpiresAt ]
    rawExecute
      "INSERT INTO commerce_checkout_line_item(checkout_id, line_number, product_type, product_id,\
      \ product_version, description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot)\
      \ VALUES (?::uuid,1,'domo_initial_deposit','legacy_public_formula','legacy_public_formula-v1',\
      \ 'Domo initial deposit',1,?,?,?,'{}'::jsonb)"
      [PersistText checkoutId, PersistInt64 depositMinor, PersistInt64 depositMinor, PersistInt64 depositMinor]
    rawExecute
      ("INSERT INTO domo_event_quote_runtime(id, quote_id, checkout_id, product_version_id, lookup_token_hash,\
       \ create_idempotency_key, create_request_sha256, customer_name, customer_email, event_type, guests,\
       \ starts_at, ends_at, setup_starts_at, duration_hours, setup_hours, catering, production, transport,\
       \ quote_status, fulfillment_status, currency, subtotal_minor, tax_minor, total_minor, deposit_minor,\
       \ balance_minor, tax_basis_points, deposit_basis_points, rate_card_version, rate_card_rules_sha256,\
       \ timezone, terms_version, hold_expires_at, venue_key)\
       \ VALUES (?::uuid, ?::uuid, ?::uuid, '81000000-0000-0000-0000-000000000001', ?, ?, ?,\
       \ 'Domo Customer', 'domo@example.com', 'wedding', 80, ?, ?, ?, 8, 2, TRUE, TRUE, FALSE,\
       \ 'sent', 'date_held', 'USD', 448000, 53760, 501760, ?, ?, 1200, 4000, 'legacy_public_formula-v1',\
       \ " <> rulesHashSql <> ", 'America/Guayaquil', 'domo-terms-legacy-draft-v1', ?, ?)")
      [ PersistText quoteId, PersistText commerceQuoteId, PersistText checkoutId
      , PersistText (tokenHash quoteId)
      , PersistText ("domo-cancel-runtime:" <> quoteId), PersistText (T.replicate 64 "3")
      , PersistUTCTime startsAt, PersistUTCTime endsAt, PersistUTCTime setupStartsAt
      , PersistInt64 depositMinor, PersistInt64 (501760 - depositMinor), PersistUTCTime holdExpiresAt
      -- A venue key per fixture keeps the active-window exclusion constraint from
      -- coupling fixtures that share a date, including leftovers of earlier runs.
      , PersistText ("domo-cancel-venue:" <> quoteId) ]
    rawExecute
      "INSERT INTO commerce_reservation_hold(checkout_id, resource_type, resource_id, starts_at, ends_at,\
      \ quantity, status, expires_at) VALUES (?::uuid, 'domo_venue', ?, ?, ?, 1, 'active', ?)"
      [ PersistText checkoutId, PersistText ("domo-cancel-venue:" <> quoteId)
      , PersistUTCTime setupStartsAt, PersistUTCTime endsAt, PersistUTCTime holdExpiresAt ]
    ) pool
  if not paid then pure () else runSqlPool (do
    rawExecute
      "UPDATE domo_event_quote_runtime SET quote_status='accepted', terms_accepted_at=NOW() WHERE id=?::uuid"
      [PersistText quoteId]
    rawExecute
      "UPDATE commerce_quote SET status='accepted', accepted_at=NOW(),\
      \ accepted_terms_version='domo-terms-legacy-draft-v1' WHERE id=?::uuid"
      [PersistText commerceQuoteId]
    rawExecute "UPDATE domo_event_quote_runtime SET quote_status='deposit_due' WHERE id=?::uuid" [PersistText quoteId]
    rawExecute "UPDATE commerce_checkout_session SET status='awaiting_payment' WHERE id=?::uuid" [PersistText checkoutId]
    rawExecute
      "INSERT INTO commerce_payment_attempt(id, checkout_id, provider, environment, operation, status,\
      \ amount_minor, currency, merchant_account_ref, idempotency_key)\
      \ VALUES (?::uuid, ?::uuid, ?, 'sandbox', 'capture', 'succeeded', ?, 'USD', 'PAYPAL-SANDBOX', ?)"
      [PersistText attemptId, PersistText checkoutId, PersistText provider, PersistInt64 depositMinor, PersistText ("domo-cancel-capture:" <> quoteId)]
    rawExecute
      "INSERT INTO commerce_provider_binding(id, payment_attempt_id, provider, environment, merchant_account_ref,\
      \ resource_type, provider_resource_id, provider_resource_path, merchant_reference, amount_minor, currency)\
      \ VALUES (?::uuid, ?::uuid, ?, 'sandbox', 'PAYPAL-SANDBOX', ?, ?, ?, ?, ?, 'USD')"
      [ PersistText bindingId, PersistText attemptId, PersistText provider
      , PersistText (if provider=="datafast" then "checkout" else "capture")
      , PersistText ("CAPTURE-" <> quoteId)
      , PersistText (if provider=="datafast" then "/v1/checkouts/" <> quoteId <> "/payment" else "/v2/checkout/orders/" <> quoteId <> "/capture"), PersistText ("domo-quote:" <> quoteId)
      , PersistInt64 depositMinor ]
    rawExecute
      "UPDATE commerce_checkout_session SET status='paid', paid_minor=?, paid_at=NOW() WHERE id=?::uuid"
      [PersistInt64 depositMinor, PersistText checkoutId]
    ) pool
  pure Fixture { fxQuoteId = quoteId, fxCheckoutId = checkoutId, fxStartsAt = startsAt }

operatorParty :: ConnectionPool -> IO Int64
operatorParty pool = do
  rows <- runSqlPool (rawSql "SELECT id FROM party ORDER BY id LIMIT 1" []) pool :: IO [Single Int64]
  case rows of
    [Single party] -> pure party
    _ -> fail "The payment test schema has no party row for the operator"

cancel :: ConnectionPool -> Fixture -> CancellationActor -> Canceller -> IO (Either CancellationError DomoCancellationRecord)
cancel pool fixture actor canceller = do
  now <- getCurrentTime
  runSqlPool (cancelDomoQuote CancellationRequest
    { crQuoteId = fxQuoteId fixture, crActor = actor, crCanceller = canceller
    , crNote = Just "runtime spec", crNow = now }) pool

scalar :: ConnectionPool -> Text -> [PersistValue] -> IO Text
scalar pool sql params = do
  rows <- runSqlPool (rawSql sql params) pool :: IO [Single Text]
  case rows of
    [Single value] -> pure value
    other -> fail ("Expected one scalar, got " <> show (length other))

requireRight :: Show e => Either e a -> IO a
requireRight = either (\e -> fail ("Unexpected Left: " <> show e)) pure

payableFixture :: ConnectionPool -> IO Fixture
payableFixture pool = do
  fixture <- seedQuote pool 40 False
  now <- getCurrentTime
  runSqlPool (do
    rawExecute "UPDATE domo_event_quote_runtime SET quote_status='accepted',terms_accepted_at=? WHERE id=?::uuid"
      [PersistUTCTime now,PersistText (fxQuoteId fixture)]
    rawExecute "UPDATE commerce_quote SET status='accepted',accepted_at=?,accepted_terms_version='domo-terms-legacy-draft-v1' WHERE id=(SELECT quote_id FROM domo_event_quote_runtime WHERE id=?::uuid)"
      [PersistUTCTime now,PersistText (fxQuoteId fixture)]
    rawExecute "UPDATE domo_event_quote_runtime SET quote_status='deposit_due' WHERE id=?::uuid"
      [PersistText (fxQuoteId fixture)]
    rawExecute "UPDATE commerce_checkout_session SET status='awaiting_payment' WHERE id=?::uuid"
      [PersistText (fxCheckoutId fixture)]) pool
  pure fixture

captureCreation :: Fixture -> IO C.PaymentAttemptCreation
captureCreation fixture = do
  now <- getCurrentTime
  identity <- toText <$> nextRandom
  pure C.PaymentAttemptCreation
    { C.pacCheckout = C.CheckoutReference (fxCheckoutId fixture)
    , C.pacProvider = C.ProviderPayPal, C.pacEnvironment = C.CheckoutSandbox
    , C.pacOperation = C.OperationCapture, C.pacAmountMinor = depositMinor
    , C.pacCurrency = "USD", C.pacMerchantRef = "PAYPAL-SANDBOX"
    , C.pacIdempotencyKey = "domo-bound-capture:" <> identity
    , C.pacCreatedAt = now, C.pacCorrelationId = "synthetic-bound-capture:" <> identity
    }

bounded :: IO a -> IO a
bounded action = timeout 5000000 action >>= maybe (fail "Domo admission barrier timed out") pure

backendPid :: SqlPersistT IO Int
backendPid = do
  rows <- rawSql "SELECT pg_backend_pid()" []
  case rows of
    [Single identifier] -> pure identifier
    _ -> fail "Expected one PostgreSQL backend identity"

awaitBlockedBy :: ConnectionPool -> Int -> Int -> IO ()
awaitBlockedBy pool owner waiter = bounded loop
  where
    loop = do
      rows <- runSqlPool (rawSql "SELECT ?::integer=ANY(pg_blocking_pids(?::integer))"
        [PersistInt64 (fromIntegral owner),PersistInt64 (fromIntegral waiter)]) pool :: IO [Single Bool]
      if rows == [Single True] then pure () else threadDelay 10000 >> loop

databaseSpec :: SpecWith ConnectionPool
databaseSpec = after restoreRecoveryAuthority $ describe "Domo cancellation (PAY-DOMO-CANCEL-001)" $ do
  it "binds the post-save admission to its exact checkout without orphan intents or attempts" $ \pool -> do
    configureRecovery pool
    first <- payableFixture pool
    other <- payableFixture pool
    creation <- captureCreation other
    entered <- newIORef False
    refused <- runSqlPool (PaymentRuntime.beginPaymentAttemptGuarded
      (liftIO (writeIORef entered True) >> domoPaymentAttemptAdmission
        (fxQuoteId first) (fxCheckoutId other) C.OperationCapture) creation) pool
    readIORef entered `shouldReturn` True
    refused `shouldSatisfy` isLeft
    scalar pool "SELECT count(*)::text FROM commerce_payment_attempt WHERE checkout_id IN (?::uuid,?::uuid)"
      [PersistText (fxCheckoutId first),PersistText (fxCheckoutId other)] `shouldReturn` "0"
    scalar pool "SELECT count(*)::text FROM commerce_payment_intent WHERE checkout_id IN (?::uuid,?::uuid)"
      [PersistText (fxCheckoutId first),PersistText (fxCheckoutId other)] `shouldReturn` "0"

  it "preserves expiry refusal and admits only one capture through the real payment bridge" $ \pool -> do
    configureRecovery pool
    expired <- payableFixture pool
    runSqlPool (rawExecute "UPDATE commerce_checkout_session SET status='expired',expires_at=clock_timestamp()-interval '1 second' WHERE id=?::uuid"
      [PersistText (fxCheckoutId expired)]) pool
    expiredCreation <- captureCreation expired
    entered <- newIORef False
    refused <- runSqlPool (PaymentRuntime.beginPaymentAttemptGuarded
      (liftIO (writeIORef entered True) >> domoPaymentAttemptAdmission
        (fxQuoteId expired) (fxCheckoutId expired) C.OperationCapture) expiredCreation) pool
    readIORef entered `shouldReturn` True
    refused `shouldSatisfy` isLeft
    scalar pool "SELECT count(*)::text FROM commerce_payment_attempt WHERE checkout_id=?::uuid"
      [PersistText (fxCheckoutId expired)] `shouldReturn` "0"
    scalar pool "SELECT count(*)::text FROM commerce_payment_intent WHERE checkout_id=?::uuid"
      [PersistText (fxCheckoutId expired)] `shouldReturn` "0"
    payable <- payableFixture pool
    original <- captureCreation payable
    let guard = domoPaymentAttemptAdmission (fxQuoteId payable) (fxCheckoutId payable) C.OperationCapture
    admitted <- runSqlPool (PaymentRuntime.beginPaymentAttemptGuarded guard original) pool
    void (requireRight admitted)
    duplicate <- captureCreation payable
    runSqlPool (PaymentRuntime.beginPaymentAttemptGuarded guard duplicate) pool >>= (`shouldSatisfy` isLeft)
    scalar pool "SELECT count(*)::text FROM commerce_payment_attempt WHERE checkout_id=?::uuid"
      [PersistText (fxCheckoutId payable)] `shouldReturn` "1"
    scalar pool "SELECT count(*)::text FROM commerce_payment_intent WHERE checkout_id=?::uuid"
      [PersistText (fxCheckoutId payable)] `shouldReturn` "1"

  forM_ [False,True] $ \captureFirst ->
    it ("orders real cancellation/capture transactions with observed lock waits; captureFirst=" <> show captureFirst) $ \pool -> do
      configureRecovery pool
      fixture <- payableFixture pool
      creation <- captureCreation fixture
      now <- getCurrentTime
      ready <- newEmptyMVar
      release <- newEmptyMVar
      waiting <- newEmptyMVar
      let guard = domoPaymentAttemptAdmission (fxQuoteId fixture) (fxCheckoutId fixture) C.OperationCapture
          cancellation = cancelDomoQuote CancellationRequest
            { crQuoteId=fxQuoteId fixture, crActor=CustomerActor, crCanceller=CancelledByCustomer
            , crNote=Just "synthetic ordered admission", crNow=now }
          classifyCapture = either (const ("refused" :: Text)) (const "admitted")
          classifyCancel = either (const ("refused" :: Text)) (const "cancelled")
          ownerAction = runSqlPool (do
            pid <- backendPid
            if captureFirst
              then classifyCapture <$> PaymentRuntime.beginPaymentAttemptGuarded (do
                refusal <- guard
                liftIO (putMVar ready pid >> takeMVar release)
                pure refusal) creation
              else do
                result <- cancellation
                liftIO (putMVar ready pid >> takeMVar release)
                pure (classifyCancel result)) pool
          waiterAction = runSqlPool (do
            pid <- backendPid
            liftIO (putMVar waiting pid)
            if captureFirst then classifyCancel <$> cancellation
              else classifyCapture <$> PaymentRuntime.beginPaymentAttemptGuarded guard creation) pool
      withAsync ownerAction $ \owner -> do
        ownerPid <- bounded (takeMVar ready)
        withAsync waiterAction $ \waiter -> do
          waiterPid <- bounded (takeMVar waiting)
          ownerPid `shouldNotBe` waiterPid
          awaitBlockedBy pool ownerPid waiterPid
          putMVar release ()
          outcome <- (,) <$> bounded (wait owner) <*> bounded (wait waiter)
          outcome `shouldBe` (if captureFirst then ("admitted","refused") else ("cancelled","refused"))
      scalar pool "SELECT count(*)::text FROM commerce_payment_attempt WHERE checkout_id=?::uuid"
        [PersistText (fxCheckoutId fixture)] `shouldReturn` (if captureFirst then "1" else "0")
      scalar pool "SELECT count(*)::text FROM commerce_payment_intent WHERE checkout_id=?::uuid"
        [PersistText (fxCheckoutId fixture)] `shouldReturn` (if captureFirst then "1" else "0")
      scalar pool "SELECT count(*)::text FROM domo_quote_cancellation WHERE domo_quote_id=?::uuid"
        [PersistText (fxQuoteId fixture)] `shouldReturn` (if captureFirst then "0" else "1")

  it "cancels an unpaid quote without a policy: hold released, checkout cancelled, zero obligation" $ \pool -> do
    fixture <- seedQuote pool 40 False
    record <- cancel pool fixture CustomerActor CancelledByCustomer >>= requireRight
    dcrQuoteStatus record `shouldBe` "cancelled"
    dcrRefundDueMinor record `shouldBe` 0
    dcrRefundStatus record `shouldBe` "none"
    dcrPolicyVersion record `shouldBe` Nothing
    dcrReplayed record `shouldBe` False
    status <- scalar pool "SELECT status FROM commerce_checkout_session WHERE id=?::uuid" [PersistText (fxCheckoutId fixture)]
    status `shouldBe` "cancelled"
    hold <- scalar pool "SELECT status FROM commerce_reservation_hold WHERE checkout_id=?::uuid" [PersistText (fxCheckoutId fixture)]
    hold `shouldBe` "released"
    fulfillment <- scalar pool "SELECT fulfillment_status FROM domo_event_quote_runtime WHERE id=?::uuid" [PersistText (fxQuoteId fixture)]
    fulfillment `shouldBe` "cancelled"
    replay <- cancel pool fixture CustomerActor CancelledByCustomer >>= requireRight
    dcrReplayed replay `shouldBe` True
    count <- scalar pool "SELECT count(*)::text FROM domo_quote_cancellation WHERE domo_quote_id=?::uuid" [PersistText (fxQuoteId fixture)]
    count `shouldBe` "1"

  it "refuses in-flight and raw FAILED captures; a separate no-attempt quote remains cancellable" $ \pool -> do
    fixture <- seedQuote pool 40 False
    attemptId <- toText <$> nextRandom
    runSqlPool (do
      rawExecute "UPDATE commerce_checkout_session SET status='awaiting_payment' WHERE id=?::uuid"
        [PersistText (fxCheckoutId fixture)]
      -- A PayPal capture admitted just before the provider call: nothing is
      -- collected yet, but the provider may still complete it.
      rawExecute
        "INSERT INTO commerce_payment_attempt(id, checkout_id, provider, environment, operation, status,\
        \ amount_minor, currency, merchant_account_ref, idempotency_key)\
        \ VALUES (?::uuid, ?::uuid, 'paypal', 'sandbox', 'capture', 'created', ?, 'USD', 'PAYPAL-SANDBOX', ?)"
        [ PersistText attemptId, PersistText (fxCheckoutId fixture), PersistInt64 depositMinor
        , PersistText ("domo-cancel-inflight:" <> fxQuoteId fixture) ]
      ) pool
    refused <- cancel pool fixture CustomerActor CancelledByCustomer
    case refused of
      Left (CancellationNotAllowed _) -> pure ()
      other -> expectationFailure ("expected refusal while a capture is in flight, got " <> show other)
    quoteStatus <- scalar pool "SELECT quote_status FROM domo_event_quote_runtime WHERE id=?::uuid" [PersistText (fxQuoteId fixture)]
    quoteStatus `shouldNotBe` "cancelled"
    -- A raw FAILED status is not an authoritative no-charge outcome. Preserve
    -- that uncertainty through the actual public HTTP cancellation boundary.
    runSqlPool (rawExecute "UPDATE commerce_payment_attempt SET status='failed' WHERE id=?::uuid"
      [PersistText attemptId]) pool
    before <- cancellationFinancialState pool fixture
    withPublicCancellation pool fixture $ \request manager -> do
      denied <- HC.httpLbs request { HC.requestHeaders = [("Content-Type","application/json"),
        ("X-Order-Lookup-Token","incorrect-synthetic-token")] } manager
      statusCode (HC.responseStatus denied) `shouldBe` 404
      forM_ [1..3 :: Int] $ \_ -> do
        response <- HC.httpLbs request manager
        statusCode (HC.responseStatus response) `shouldBe` 409
        show (HC.responseBody response) `shouldContain`
          "A deposit payment is being verified; retry once it settles"
        cancellationFinancialState pool fixture `shouldReturn` before
    -- The public cancellation path is database-only. Independently exercise
    -- the real refund executor with an injected transport: absent a legitimate
    -- cancellation/debt it must refuse before even writing a provider request.
    user <- authenticatedAdmin pool
    withPaypalConfig $ withWire [] $ \manager written -> do
      let _read :<|> _approve :<|> execute :<|> _reconcile :<|> _manual :<|> _confirm :<|> _bind =
            API.domoRefundAdminServerWithManager manager user (fxQuoteId fixture)
      result <- runHandler (runReaderT execute (Env pool (error "No unrelated config needed")))
      either errHTTPCode (const 200) result `shouldBe` 409
      written `shouldReturn` ""
      cancellationFinancialState pool fixture `shouldReturn` before
    -- There is no canonical pre-provider abandonment operation; do not forge
    -- a provider-confirmed cancellation. A distinct quote with NO attempt
    -- establishes the legitimate unpaid-success precondition instead.
    unpaid <- seedQuote pool 40 False
    scalar pool "SELECT count(*)::text FROM commerce_payment_attempt WHERE checkout_id=?::uuid"
      [PersistText (fxCheckoutId unpaid)] `shouldReturn` "0"
    admittedBefore <- runSqlPool (domoAttemptAdmission (fxCheckoutId unpaid)) pool
    admittedBefore `shouldBe` Nothing
    record <- cancel pool unpaid CustomerActor CancelledByCustomer >>= requireRight
    dcrQuoteStatus record `shouldBe` "cancelled"
    dcrRefundDueMinor record `shouldBe` 0
    -- The payment side of the fence: a capture that read the quote before the
    -- cancellation committed is refused a new attempt afterwards.
    admittedAfter <- runSqlPool (domoAttemptAdmission (fxCheckoutId unpaid)) pool
    admittedAfter `shouldBe` Just "This Domo quote is no longer payable"

  it "records a customer cancellation's full-refund obligation and lets an operator settle it exactly once" $ \pool -> do
    fixture <- seedQuote pool 40 True
    party <- operatorParty pool
    record <- cancel pool fixture CustomerActor CancelledByCustomer >>= requireRight
    dcrRefundDueMinor record `shouldBe` depositMinor
    dcrRetainedMinor record `shouldBe` 0
    dcrRefundStatus record `shouldBe` "awaiting_operator"
    dcrPolicyVersion record `shouldBe` Just 1
    open <- scalar pool "SELECT count(*)::text FROM domo_quote_cancellation_open_refund WHERE domo_quote_id=?::uuid" [PersistText (fxQuoteId fixture)]
    open `shouldBe` "1"
    exceptions <- scalar pool
      "SELECT count(*)::text FROM commerce_reconciliation_exception WHERE exception_type=? AND internal_reference=? AND status='open'"
      [PersistText refundObligationExceptionType, PersistText ("domo-quote:" <> fxQuoteId fixture)]
    exceptions `shouldBe` "1"
    -- The checkout stays paid; the quote and the venue are released.
    checkoutStatus <- scalar pool "SELECT status FROM commerce_checkout_session WHERE id=?::uuid" [PersistText (fxCheckoutId fixture)]
    checkoutStatus `shouldBe` "paid"
    settled <- cancel pool fixture (OperatorActor party) CancelledByCustomer >>= requireRight
    dcrRefundStatus settled `shouldBe` "requested"
    dcrRefundDueMinor settled `shouldBe` depositMinor
    refundId <- maybe (fail "refund not linked") pure (dcrRefundId settled)
    amount <- scalar pool "SELECT amount_minor::text || ':' || status || ':' || requested_by::text || ':' || reason_code FROM commerce_refund WHERE id=?::uuid" [PersistText refundId]
    amount `shouldBe` (T.pack (show depositMinor) <> ":requested:" <> T.pack (show party) <> ":customer_request")
    openAfter <- scalar pool "SELECT count(*)::text FROM domo_quote_cancellation_open_refund WHERE domo_quote_id=?::uuid" [PersistText (fxQuoteId fixture)]
    openAfter `shouldBe` "1"
    resolved <- scalar pool
      "SELECT status FROM commerce_reconciliation_exception WHERE exception_type=? AND internal_reference=?"
      [PersistText refundObligationExceptionType, PersistText ("domo-quote:" <> fxQuoteId fixture)]
    resolved `shouldBe` "open"
    again <- cancel pool fixture (OperatorActor party) CancelledByCustomer >>= requireRight
    dcrReplayed again `shouldBe` True
    dcrRefundId again `shouldBe` Just refundId
    refunds <- scalar pool "SELECT count(*)::text FROM commerce_refund WHERE checkout_id=?::uuid" [PersistText (fxCheckoutId fixture)]
    refunds `shouldBe` "1"

  it "an operator cancellation 10 days ahead retains half the deposit and requests the other half" $ \pool -> do
    fixture <- seedQuote pool 10 True
    party <- operatorParty pool
    record <- cancel pool fixture (OperatorActor party) CancelledByCustomer >>= requireRight
    dcrLeadDays record `shouldBe` 10
    dcrRetainedBasisPoints record `shouldBe` 5000
    dcrRetainedMinor record + dcrRefundDueMinor record `shouldBe` depositMinor
    dcrRefundDueMinor record `shouldBe` 100352
    dcrRefundStatus record `shouldBe` "requested"
    dcrActorType record `shouldBe` "operator"
    refundId <- maybe (fail "refund not linked") pure (dcrRefundId record)
    amount <- scalar pool "SELECT amount_minor::text FROM commerce_refund WHERE id=?::uuid" [PersistText refundId]
    amount `shouldBe` "100352"
    -- The shared refund approval handler resolves this refund to its Domo quote
    -- and binds it to the verified PayPal capture of the deposit checkout.
    resolved <- runSqlPool (loadDomoCancellationRefund (RefundReference refundId)) pool
    case resolved of
      Just (quoteId, Right paid) -> do
        quoteId `shouldBe` fxQuoteId fixture
        spcCheckoutId paid `shouldBe` fxCheckoutId fixture
        spcProvider paid `shouldBe` "paypal"
        spcProviderResource paid `shouldBe` ("CAPTURE-" <> fxQuoteId fixture)
        spcPaidMinor paid `shouldBe` depositMinor
        spcReservedMinor paid `shouldBe` 100352
      other -> expectationFailure ("expected the Domo refund to resolve, got " <> show (fmap (fmap (fmap spcAttemptId)) other))
    unrelated <- runSqlPool (loadDomoCancellationRefund (RefundReference "00000000-0000-0000-0000-000000000000")) pool
    fmap fst unrelated `shouldBe` Nothing

  it "a TDF cancellation two days ahead refunds the whole deposit" $ \pool -> do
    fixture <- seedQuote pool 2 True
    party <- operatorParty pool
    record <- cancel pool fixture (OperatorActor party) CancelledByTdf >>= requireRight
    dcrCanceller record `shouldBe` "tdf"
    dcrRetainedMinor record `shouldBe` 0
    dcrRefundDueMinor record `shouldBe` depositMinor
    dcrRefundStatus record `shouldBe` "requested"
    reason <- scalar pool "SELECT reason_code FROM commerce_refund WHERE checkout_id=?::uuid" [PersistText (fxCheckoutId fixture)]
    reason `shouldBe` "service_cancelled"

  it "a customer cancellation two days ahead retains the whole deposit and owes nothing" $ \pool -> do
    fixture <- seedQuote pool 2 True
    record <- cancel pool fixture CustomerActor CancelledByCustomer >>= requireRight
    dcrRetainedMinor record `shouldBe` depositMinor
    dcrRefundDueMinor record `shouldBe` 0
    dcrRefundStatus record `shouldBe` "none"
    refunds <- scalar pool "SELECT count(*)::text FROM commerce_refund WHERE checkout_id=?::uuid" [PersistText (fxCheckoutId fixture)]
    refunds `shouldBe` "0"

  it "refuses unknown and terminal quotes" $ \pool -> do
    missing <- toText <$> nextRandom
    now <- getCurrentTime
    unknown <- runSqlPool (cancelDomoQuote CancellationRequest
      { crQuoteId = missing, crActor = CustomerActor, crCanceller = CancelledByCustomer
      , crNote = Nothing, crNow = now }) pool
    unknown `shouldBe` Left CancellationNotFound
    fixture <- seedQuote pool 40 False
    void $ runSqlPool (rawSql "SELECT domo_quote_expire_holds(NOW() + INTERVAL '1 hour', ?::uuid)"
      [PersistText (fxQuoteId fixture)] :: SqlPersistT IO [Single Int]) pool
    expired <- cancel pool fixture CustomerActor CancelledByCustomer
    case expired of
      Left (CancellationNotAllowed _) -> pure ()
      other -> expectationFailure ("Expected NotAllowed for an expired quote, got " <> show other)

  it "the schema refuses cancelling a paid quote without its cancellation record" $ \pool -> do
    fixture <- seedQuote pool 40 True
    outcome <- try (runSqlPool (rawExecute
      "UPDATE domo_event_quote_runtime SET quote_status='cancelled', fulfillment_status='cancelled' WHERE id=?::uuid"
      [PersistText (fxQuoteId fixture)]) pool) :: IO (Either SomeException ())
    case outcome of
      Left _ -> pure ()
      Right () -> expectationFailure "A paid Domo quote was cancelled without a cancellation record"
    status <- scalar pool "SELECT quote_status FROM domo_event_quote_runtime WHERE id=?::uuid" [PersistText (fxQuoteId fixture)]
    status `shouldBe` "deposit_paid"

  forM_ [10,40] $ \days ->
    it ("independently approves, executes once and reconciles the exact "<>show days<>"-day refund") $ \pool -> do
      fixture <- seedQuote pool days True
      record <- cancel pool fixture (OperatorActor 1) CancelledByCustomer >>= requireRight
      now <- getCurrentTime
      expectFailure $ runSqlPool (D.claimDomoRefund (fxQuoteId fixture) 2 now) pool
      expectFailure $ runSqlPool (D.approveDomoRefund (fxQuoteId fixture) 1 now) pool
      approved <- runSqlPool (D.approveDomoRefund (fxQuoteId fixture) 2 now) pool
      R.rrStatus approved `shouldBe` "approved"
      approvals <- mapConcurrently (\_ -> runSqlPool (D.approveDomoRefund (fxQuoteId fixture) 2 now) pool) [1..4::Int]
      map R.rrApprovedBy approvals `shouldBe` replicate 4 (Just 2)
      claims <- mapConcurrently (\_ -> runSqlPool (D.claimDomoRefund (fxQuoteId fixture) 2 now) pool) [1..8::Int]
      length (filter snd claims) `shouldBe` 1
      let refund = fst (head claims)
          ref = R.rrReference refund
      runSqlPool (R.recordRefundPending ref ("REFUND-"<>R.refundReferenceId ref) now) pool `shouldReturn` Right ()
      configureRecovery pool
      held <- Recovery.reconcileKnownRefund pool ref 3 (const (pure True))
        (const (pure (Right Q.RefundQueryHeld))) >>= requireRight
      R.rrStatus (Recovery.rrvRefund held) `shouldBe` "processing"
      outstanding pool fixture `shouldReturn` "1:0"
      configureRecovery pool
      complete <- Recovery.reconcileKnownRefund pool ref 3 (const (pure True))
        (const (pure (Right Q.RefundQueryCompleted))) >>= requireRight
      R.rrStatus (Recovery.rrvRefund complete) `shouldBe` "succeeded"
      outstanding pool fixture `shouldReturn` ("0:"<>T.pack (show (dcrRefundDueMinor record)))
      scalar pool "SELECT count(*)::text FROM commerce_reconciliation_exception WHERE internal_reference=? AND exception_type=? AND status='resolved'"
        [PersistText ("domo-quote:"<>fxQuoteId fixture),PersistText refundObligationExceptionType] `shouldReturn` "1"
      scalar pool "SELECT count(*)::text FROM commerce_receipt WHERE refund_id=?::uuid AND kind='credit_note'"
        [PersistText (R.refundReferenceId ref)] `shouldReturn` "1"
      scalar pool "SELECT count(*)::text FROM commerce_ledger_transaction WHERE source_type='refund' AND source_id=? AND status='posted'"
        [PersistText (R.refundReferenceId ref)] `shouldReturn` "1"
      scalar pool "SELECT sum(e.amount_minor)::text FROM commerce_ledger_entry e JOIN commerce_ledger_transaction t ON t.id=e.transaction_id WHERE t.source_type='refund' AND t.source_id=?"
        [PersistText (R.refundReferenceId ref)] `shouldReturn` "0"
      scalar pool "SELECT e.amount_minor::text FROM commerce_ledger_entry e JOIN commerce_ledger_transaction t ON t.id=e.transaction_id WHERE t.source_type='refund' AND t.source_id=? AND e.account_code='revenue.domo_event_quote'"
        [PersistText (R.refundReferenceId ref)] `shouldReturn` T.pack (show (dcrRefundDueMinor record))
      replay <- runSqlPool (D.claimDomoRefund (fxQuoteId fixture) 2 now) pool
      snd replay `shouldBe` False
      reloaded <- runSqlPool (loadRecord (fxQuoteId fixture)) pool
      fmap dcrRefundStatus reloaded `shouldBe` Just "succeeded"

  it "keeps provider rejection, unknown responses and revoked authority outstanding without another execution" $ \pool -> do
    fixture <- seedQuote pool 40 True
    _ <- cancel pool fixture (OperatorActor 1) CancelledByCustomer >>= requireRight
    now <- getCurrentTime
    _ <- runSqlPool (D.approveDomoRefund (fxQuoteId fixture) 2 now) pool
    (refund,True) <- runSqlPool (D.claimDomoRefund (fxQuoteId fixture) 2 now) pool
    runSqlPool (R.recordRefundFailure (R.rrReference refund) "synthetic_provider_rejection" now) pool
    forM_ [1..4::Int] $ \_ -> do
      (_,permit) <- runSqlPool (D.claimDomoRefund (fxQuoteId fixture) 2 now) pool
      permit `shouldBe` False
    result <- Recovery.reconcileKnownRefund pool (R.rrReference refund) 3 (const (pure True))
      (\_ -> fail "An unknown provider refund ID must never cause discovery or another refund")
    result `shouldSatisfy` isLeft
    expectFailure $ runSqlPool (D.bindDomoRefundReference (fxQuoteId fixture) 1 now "synthetic-refund") pool
    runSqlPool (D.bindDomoRefundReference (fxQuoteId fixture) 2 now ("REFUND-"<>R.refundReferenceId (R.rrReference refund))) pool
    expectFailure $ runSqlPool (D.bindDomoRefundReference (fxQuoteId fixture) 2 now "CONFLICTING-REFUND") pool
    configureRecovery pool
    failed <- Recovery.reconcileKnownRefund pool (R.rrReference refund) 3 (const (pure True))
      (const (pure (Left Recovery.RefundRecoveryQueryFailed)))
    failed `shouldSatisfy` isLeft
    configureRecovery pool
    authority <- newIORef True
    denied <- Recovery.reconcileKnownRefundGuarded (liftIO (readIORef authority)) pool (R.rrReference refund) 3 (const (pure True))
      (\_ -> writeIORef authority False >> pure (Right Q.RefundQueryCompleted))
    denied `shouldSatisfy` isLeft
    outstanding pool fixture `shouldReturn` "1:0"

  it "rolls back reconciliation closure with the ledger and credit note when accounting fails" $ \pool -> do
    fixture <- seedQuote pool 40 True
    _ <- cancel pool fixture (OperatorActor 1) CancelledByTdf >>= requireRight
    now <- getCurrentTime
    _ <- runSqlPool (D.approveDomoRefund (fxQuoteId fixture) 2 now) pool
    (refund,True) <- runSqlPool (D.claimDomoRefund (fxQuoteId fixture) 2 now) pool
    runSqlPool (R.recordRefundPending (R.rrReference refund) ("REFUND-"<>fxQuoteId fixture) now) pool `shouldReturn` Right ()
    configureRecovery pool
    let install=rawExecute "ALTER TABLE commerce_receipt ADD CONSTRAINT synthetic_domo_credit_note_failure CHECK(kind<>'credit_note') NOT VALID" []
        remove=rawExecute "ALTER TABLE commerce_receipt DROP CONSTRAINT synthetic_domo_credit_note_failure" []
    bracket_ (runSqlPool install pool) (runSqlPool remove pool) $ do
      result <- Recovery.reconcileKnownRefund pool (R.rrReference refund) 3 (const (pure True))
        (const (pure (Right Q.RefundQueryCompleted)))
      result `shouldSatisfy` isLeft
    outstanding pool fixture `shouldReturn` "1:0"
    scalar pool "SELECT status FROM commerce_refund WHERE id=?::uuid" [PersistText (R.refundReferenceId (R.rrReference refund))] `shouldReturn` "processing"
    scalar pool "SELECT status FROM commerce_reconciliation_exception WHERE exception_type=? AND internal_reference=?"
      [PersistText refundObligationExceptionType,PersistText ("domo-quote:"<>fxQuoteId fixture)] `shouldReturn` "open"

  it "requires exact manual evidence and a different confirmer; concurrent confirmations post once" $ \pool -> do
    fixture <- seedQuoteForProvider "datafast" pool 40 True
    _ <- cancel pool fixture (OperatorActor 1) CancelledByTdf >>= requireRight
    now <- getCurrentTime
    _ <- runSqlPool (D.approveDomoRefund (fxQuoteId fixture) 2 now) pool
    (_,True) <- runSqlPool (D.claimDomoRefund (fxQuoteId fixture) 2 now) pool
    let evidence = D.ManualEvidence (T.justifyLeft 256 'M' ("MANUAL-"<>fxQuoteId fixture)) ("secure-receipt:"<>fxQuoteId fixture) depositMinor "USD" now
    expectFailure $ runSqlPool (D.recordManualEvidence (fxQuoteId fixture) 2 now evidence {D.meAmountMinor=depositMinor-1}) pool
    expectFailure $ runSqlPool (D.recordManualEvidence (fxQuoteId fixture) 2 now evidence {D.meExternalReference=T.replicate 257 "M"}) pool
    expectFailure $ runSqlPool (D.recordManualEvidence (fxQuoteId fixture) 2 now evidence {D.meExternalReference="INVALID/REFERENCE"}) pool
    expectFailure $ runSqlPool (D.confirmManualEvidence (fxQuoteId fixture) 3 now) pool
    runSqlPool (D.recordManualEvidence (fxQuoteId fixture) 2 now evidence) pool
    runSqlPool (D.recordManualEvidence (fxQuoteId fixture) 2 now evidence) pool
    expectFailure $ runSqlPool (D.recordManualEvidence (fxQuoteId fixture) 2 now evidence {D.meExternalReference="OTHER"}) pool
    expectFailure $ runSqlPool (D.confirmManualEvidence (fxQuoteId fixture) 2 now) pool
    outstanding pool fixture `shouldReturn` "1:0"
    records <- mapConcurrently (\_ -> runSqlPool (D.confirmManualEvidence (fxQuoteId fixture) 3 now) pool) [1..4::Int]
    map R.rrStatus records `shouldBe` replicate 4 "succeeded"
    outstanding pool fixture `shouldReturn` ("0:"<>T.pack (show depositMinor))

  it "fences cancellation during an admitted capture and refuses capture after cancellation" $ \pool -> do
    fixture <- seedQuote pool 40 False
    now <- getCurrentTime
    runSqlPool (do
      rawExecute "UPDATE domo_event_quote_runtime SET quote_status='accepted',terms_accepted_at=? WHERE id=?::uuid" [PersistUTCTime now,PersistText (fxQuoteId fixture)]
      rawExecute "UPDATE domo_event_quote_runtime SET quote_status='deposit_due' WHERE id=?::uuid" [PersistText (fxQuoteId fixture)]
      rawExecute "UPDATE commerce_checkout_session SET status='awaiting_payment' WHERE id=?::uuid" [PersistText (fxCheckoutId fixture)]) pool
    runSqlPool (domoPaymentAdmission (fxQuoteId fixture) C.OperationCapture) pool `shouldReturn` Nothing
    attempt <- toText <$> nextRandom
    -- The durable admission exists before any HTTP response/processing update.
    runSqlPool (rawExecute "INSERT INTO commerce_payment_attempt(id,checkout_id,provider,environment,operation,status,amount_minor,currency,merchant_account_ref,idempotency_key) VALUES (?::uuid,?::uuid,'paypal','sandbox','capture','created',?,'USD','PAYPAL-SANDBOX',?)"
      [PersistText attempt,PersistText (fxCheckoutId fixture),PersistInt64 depositMinor,PersistText ("domo-inflight:"<>attempt)]) pool
    refused <- cancel pool fixture CustomerActor CancelledByCustomer
    refused `shouldSatisfy` isLeft
    scalar pool "SELECT count(*)::text FROM domo_quote_cancellation WHERE domo_quote_id=?::uuid" [PersistText (fxQuoteId fixture)] `shouldReturn` "0"
    other <- seedQuote pool 40 False
    _ <- cancel pool other CustomerActor CancelledByCustomer >>= requireRight
    admission <- runSqlPool (domoPaymentAdmission (fxQuoteId other) C.OperationCapture) pool
    admission `shouldSatisfy` (/=Nothing)


  it "serializes competing capture admission and cancellation under the same locks" $ \pool ->
    forM_ [1..8::Int] $ \_ -> do
      fixture <- seedQuote pool 40 False
      now <- getCurrentTime
      runSqlPool (do
        rawExecute "UPDATE domo_event_quote_runtime SET quote_status='accepted',terms_accepted_at=? WHERE id=?::uuid" [PersistUTCTime now,PersistText (fxQuoteId fixture)]
        rawExecute "UPDATE domo_event_quote_runtime SET quote_status='deposit_due' WHERE id=?::uuid" [PersistText (fxQuoteId fixture)]
        rawExecute "UPDATE commerce_checkout_session SET status='awaiting_payment' WHERE id=?::uuid" [PersistText (fxCheckoutId fixture)]) pool
      attempt <- toText <$> nextRandom
      let capture = runSqlPool (do
            refusal <- domoPaymentAdmission (fxQuoteId fixture) C.OperationCapture
            case refusal of
              Just _ -> pure "refused"
              Nothing -> do
                rawExecute "INSERT INTO commerce_payment_attempt(id,checkout_id,provider,environment,operation,status,amount_minor,currency,merchant_account_ref,idempotency_key) VALUES (?::uuid,?::uuid,'paypal','sandbox','capture','created',?,'USD','PAYPAL-SANDBOX',?)"
                  [PersistText attempt,PersistText (fxCheckoutId fixture),PersistInt64 depositMinor,PersistText ("domo-race:"<>attempt)]
                pure "admitted") pool
          cancellation = either (const "refused") (const "cancelled") <$> cancel pool fixture CustomerActor CancelledByCustomer
      outcome <- concurrently capture cancellation
      outcome `shouldSatisfy` (`elem` ([("admitted","refused"),("refused","cancelled")] :: [(Text,Text)]))
  forM_ ["COMPLETED","PENDING","transport_error","provider_rejection"] $ \outcome ->
    it ("the real handler keeps "<>T.unpack outcome<>" outstanding and never repeats a refund POST") $ \pool -> do
      fixture <- seedQuote pool 40 True
      _ <- cancel pool fixture (OperatorActor 1) CancelledByTdf >>= requireRight
      user <- authenticatedAdmin pool
      now <- getCurrentTime
      _ <- runSqlPool (D.approveDomoRefund (fxQuoteId fixture) (fromIntegral (fromSqlKey (auPartyId user))) now) pool
      configureRecovery pool
      runSqlPool (rawExecute "INSERT INTO revenue_feature_flag(flag_key,enabled,environment,reason) VALUES ('checkout.paypal.refunds',true,'sandbox','synthetic handler fixture') ON CONFLICT(flag_key,environment) DO UPDATE SET enabled=true" []) pool
      let rejected = outcome `elem` ["transport_error","provider_rejection"]
          response = case outcome of
            "transport_error" -> "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 2\r\n\r\n{}"
            "provider_rejection" -> "HTTP/1.1 422 Unprocessable Entity\r\nContent-Length: 2\r\n\r\n{}"
            _ -> wire ("{\"id\":\"REFUND-"<>fxQuoteId fixture<>"\",\"status\":\""<>outcome<>"\",\"amount\":{\"value\":\"2007.04\",\"currency_code\":\"USD\"}}")
      withPaypalConfig $ withWire [wire "{\"access_token\":\"synthetic-token\",\"token_type\":\"Bearer\"}",response] $ \manager written -> do
        let _read :<|> _approve :<|> execute :<|> _reconcile :<|> _manual :<|> _confirm :<|> _bind =
              API.domoRefundAdminServerWithManager manager user (fxQuoteId fixture)
            env=Env pool (error "No unrelated config needed")
        result <- runHandler (runReaderT execute env)
        either errHTTPCode (const 200) result `shouldBe` (if rejected then 502 else 200)
        forM_ [1..4::Int] $ \_ -> do
          replay <- runHandler (runReaderT execute env)
          either errHTTPCode (const 200) replay `shouldBe` 200
        requests <- written
        T.count "/refund HTTP" requests `shouldBe` 1
        T.count "GET /v2/payments/refunds/" requests `shouldBe` 0
      outstanding pool fixture `shouldReturn` "1:0"

-- Complete per-fixture observations before and after a refused cancellation.
-- No aggregate global ledger count can be disturbed by unrelated fixtures.
cancellationFinancialState :: ConnectionPool -> Fixture -> IO [Text]
cancellationFinancialState pool fixture = sequence
  [ scalar pool "SELECT quote_status||':'||fulfillment_status FROM domo_event_quote_runtime WHERE id=?::uuid" [quote]
  , scalar pool "SELECT status||':'||refunded_minor::text FROM commerce_checkout_session WHERE id=?::uuid" [checkout]
  , scalar pool "SELECT status FROM commerce_reservation_hold WHERE checkout_id=?::uuid" [checkout]
  , scalar pool "SELECT count(*)::text FROM domo_quote_cancellation WHERE domo_quote_id=?::uuid" [quote]
  , scalar pool "SELECT count(*)::text FROM commerce_refund WHERE checkout_id=?::uuid" [checkout]
  , scalar pool "SELECT count(*)::text FROM commerce_ledger_transaction WHERE source_id IN (?,?,?) OR source_id IN (SELECT id::text FROM commerce_refund WHERE checkout_id=?::uuid)"
      [PersistText (fxQuoteId fixture),PersistText (fxCheckoutId fixture),
       PersistText ("domo-quote:" <> fxQuoteId fixture),PersistText (fxCheckoutId fixture)]
  ]
  where
    quote = PersistText (fxQuoteId fixture)
    checkout = PersistText (fxCheckoutId fixture)

withPublicCancellation :: ConnectionPool -> Fixture -> (HC.Request -> HC.Manager -> IO a) -> IO a
withPublicCancellation pool fixture action = do
  let token = fixtureLookupToken (fxQuoteId fixture)
  cfg <- loadConfig
  let env = Env pool cfg
      api = Proxy :: Proxy PublicRoutes.PublicDomoQuotesAPI
      app = serve api (hoistServer api (flip runReaderT env) PublicDomo.publicDomoQuotesServer)
  Warp.testWithApplicationSettings (Warp.setHost "127.0.0.1" Warp.defaultSettings) (pure app) $ \port ->
    bracket (HC.newManager HC.defaultManagerSettings) HC.closeManager $ \manager -> do
      request <- HC.parseRequest ("http://127.0.0.1:" <> show port <>
        "/public/domo/quotes/" <> T.unpack (fxQuoteId fixture) <> "/cancel")
      action request { HC.method="POST", HC.requestHeaders=[("Content-Type","application/json"),
        ("X-Order-Lookup-Token",BS.pack (T.unpack token))],
        HC.requestBody=HC.RequestBodyBS "{\"confirmCancellation\":true}",
        HC.checkResponse = \_ _ -> pure () } manager

expectFailure :: IO a -> IO ()
expectFailure action = do
  result <- try (void action) :: IO (Either SomeException ())
  result `shouldSatisfy` isLeft

outstanding :: ConnectionPool -> Fixture -> IO Text
outstanding pool fixture = scalar pool
  "SELECT (SELECT count(*) FROM domo_quote_cancellation_open_refund WHERE domo_quote_id=?::uuid)::text||':'||refunded_minor::text FROM commerce_checkout_session WHERE id=?::uuid"
  [PersistText (fxQuoteId fixture),PersistText (fxCheckoutId fixture)]

configureRecovery :: ConnectionPool -> IO ()
configureRecovery pool = runSqlPool (do
  rawExecute "UPDATE commerce_provider_account SET enabled=true,status='ready',contract_status='approved',credential_status='validated',merchant_account_ref='PAYPAL-SANDBOX' WHERE provider='paypal' AND environment='sandbox'" []
  rawExecute "UPDATE commerce_provider_capability SET verification_status='sandbox_verified' WHERE provider_account_id=(SELECT id FROM commerce_provider_account WHERE provider='paypal' AND environment='sandbox') AND payment_method='paypal_wallet'" []
  rawExecute "INSERT INTO revenue_feature_flag(flag_key,enabled,environment,reason) VALUES ('checkout.paypal.refund_reconciliation',true,'sandbox','synthetic Domo fixture') ON CONFLICT(flag_key,environment) DO UPDATE SET enabled=true" []
  rawExecute "DELETE FROM commerce_provider_query_budget WHERE provider='paypal' AND environment='sandbox'" []) pool

restoreRecoveryAuthority :: ConnectionPool -> IO ()
restoreRecoveryAuthority pool = runSqlPool (do
  rawExecute "UPDATE commerce_provider_account SET merchant_account_ref='synthetic-provider-retry-merchant' WHERE provider='paypal' AND environment='sandbox'" []
  rawExecute "UPDATE revenue_feature_flag SET enabled=false WHERE flag_key='checkout.paypal.refund_reconciliation' AND environment='sandbox'" []) pool

authenticatedAdmin :: ConnectionPool -> IO AuthedUser
authenticatedAdmin pool = do
  token <- ("synthetic-domo-admin:"<>) . toText <$> nextRandom
  runSqlPool (do
    actors <- rawSql "INSERT INTO party(display_name,is_org,created_at) VALUES ('Synthetic Domo refund approver',false,NOW()) RETURNING id" [] :: SqlPersistT IO [Single Int64]
    actor <- case actors of [Single value] -> pure value; _ -> fail "Synthetic admin missing"
    rawExecute "INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version) SELECT ?,id,'bootstrap',true,NOW(),1 FROM security_role WHERE code='admin'" [PersistInt64 actor]
    rawExecute "INSERT INTO api_token(party_id,token,active) VALUES (?,?,true)" [PersistInt64 actor,PersistText token]
    loadAuthedUser token >>= maybe (fail "Synthetic authenticated admin missing") pure) pool

wire :: Text -> BS.ByteString
wire value = let body=BS.pack (T.unpack value) in
  "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: "<>
    BS.pack (show (BS.length body))<>"\r\n\r\n"<>body

withWire :: [BS.ByteString] -> (HC.Manager -> IO Text -> IO a) -> IO a
withWire chunks action = do
  remaining <- newIORef chunks
  writes <- newIORef ([] :: [BS.ByteString])
  let reader = atomicModifyIORef' remaining $ \current -> case current of
        [] -> ([],BS.empty)
        part:rest -> (rest,part)
      connect _ _ _ = HC.makeConnection reader
        (\bytes -> atomicModifyIORef' writes (\old -> (bytes:old,()))) (pure ())
      settings=HC.managerSetProxy HC.noProxy Http.providerManagerSettings
        { HC.managerTlsConnection=pure connect,HC.managerRawConnection=pure connect }
  bracket (HC.newManager settings) HC.closeManager $ \manager ->
    action manager (T.pack . BS.unpack . BS.concat . reverse <$> readIORef writes)

withPaypalConfig :: IO a -> IO a
withPaypalConfig action = do
  prior <- getEnvironment
  let settings=[("PAYPAL_ENV","sandbox"),("PAYPAL_MERCHANT_ID","PAYPAL-SANDBOX"),
        ("PAYPAL_CLIENT_ID","synthetic-client"),("PAYPAL_CLIENT_SECRET","synthetic-secret"),
        ("PAYPAL_REFUND_RECONCILIATION_ENABLED","true")]
      restore (key,_) = maybe (unsetEnv key) (setEnv key) (lookup key prior)
  bracket_ (mapM_ (uncurry setEnv) settings) (mapM_ restore settings) action
