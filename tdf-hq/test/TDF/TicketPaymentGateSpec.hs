{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.TicketPaymentGateSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, toSqlKey, rawExecute, rawSql, runSqlPool)
import Network.Socket (SockAddr(SockAddrInet), tupleToHostAddress)
import Servant (Header, Headers, getResponse, getHeaders, Handler, ServerError, errBody, errHTTPCode, runHandler, (:<|>)(..))
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec
import TDF.Test.DisposablePostgres (isDisposablePostgresUrl)

import TDF.API.ProviderExecution (PaymentSessionCreateDTO(..))
import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentRuntimeStore as PaymentRuntime
import TDF.Auth (AuthedUser(..), modulesForRoles)
import TDF.Models (RoleEnum(..))
import TDF.Config (loadConfig)
import TDF.DB (Env(..))
import qualified TDF.Routes.EventTickets as Routes
import TDF.Server.EventTicketCheckout
  ( lockTicketOrderSalesOpen, paypalCaptureFailureCode, publicEventTicketsServer, ticketCaptureUnverified
  , ticketOrderSalesOpen, ticketSalesClosedRefusal, ticketUnverifiedCaptureRefusal )
import qualified TDF.Server.TicketPaymentReceipt as Receipt
import TDF.Server.SocialEventsHandlers (socialEventsServer)
import TDF.Server.ProviderExecution (providerExecutionServer)

-- | A held, unpaid ticket order must not be payable once its event stops selling
-- tickets, through any endpoint that starts or takes a payment. Orders are
-- created through the real checkout handler against the production schema.
spec :: Spec
spec = describe "ticket-payment-gate-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_TICKET_PAYMENT_GATE_TEST_DB")
  ci <- runIO ((== Just "true") <$> lookupEnv "CI")
  overrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated runner" $ pendingWith "Run scripts/test-ticket-payment-gate.sh"
    Just _ | any (maybe False (not . null)) overrides ->
      it "refuses inherited database routing overrides" $ expectationFailure "Unset libpq routing overrides"
    Just connection | not (isDisposablePostgresUrl ci connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 4)) $ do
      forM_ [False,True] $ \promotion -> forM_ [False,True] $ \elapsed ->
        it ("ticket-reservation-clock enforces canonical deadline: promo=" <>
            show promotion <> ", elapsed=" <> show elapsed) $ \pool -> do
          let label = if promotion then "clock-promo" else "clock-tier"
          (eventId,tierId) <- newOrderFixture pool label
          [Single deadline] <- runSqlPool (rawSql
            "SELECT clock_timestamp()+(?::integer*interval '1 second')"
              [PersistInt64 (if elapsed then 5 else 600)]) pool :: IO [Single UTCTime]
          let code = "CLOCK-" <> T.pack (show eventId)
              request = Routes.PublicEventTicketCheckoutRequest tierId 1 "Synthetic Buyer"
                ("clock-" <> T.pack (show eventId) <> "@example.test") Nothing
                (if promotion then Just code else Nothing) True Nothing Nothing Nothing
              releaseBarrier release = void (tryPutMVar release ())
          runSqlPool (if promotion then rawExecute
            "INSERT INTO promo_code(event_id,code,discount_type,discount_value,currency,valid_until,\
            \ max_redemptions,current_redemptions,is_active) VALUES (?,?,'fixed_amount',100,'USD',?,10,0,true)"
            [PersistInt64 eventId,PersistText code,PersistUTCTime deadline]
            else rawExecute "UPDATE event_ticket_tier SET sales_end=? WHERE id=?"
              [PersistUTCTime deadline,PersistInt64 tierId]) pool
          locked <- newEmptyMVar
          release <- newEmptyMVar
          let holdBarrier = runSqlPool (do
                _ <- if promotion then rawSql
                  "SELECT id FROM promo_code WHERE event_id=? FOR UPDATE" [PersistInt64 eventId]
                  else rawSql "SELECT id FROM event_ticket_tier WHERE id=? FOR UPDATE" [PersistInt64 tierId]
                  :: SqlPersistT IO [Single Int64]
                liftIO $ putMVar locked ()
                liftIO $ takeMVar release) pool
              buy = run pool (createOrder eventId
                (Just ("ticket-reservation-clock-" <> T.pack (show eventId))) request)
          withAsync holdBarrier $ \holding ->
            (do
              timeout 5000000 (takeMVar locked) `shouldReturn` Just ()
              withAsync buy $ \buying ->
                (do
                  let table = if promotion then "promo_code" else "event_ticket_tier"
                  timeout 5000000 (waitForReservationClockLock pool table 200) `shouldReturn` Just True
                  observed <- if elapsed
                    then timeout 15000000 (waitForReservationDeadline pool deadline 600)
                    else do
                      [Single current] <- runSqlPool (rawSql "SELECT clock_timestamp()" []) pool
                      pure (Just current)
                  observed `shouldSatisfy` maybe False (\current ->
                    if elapsed then current > deadline else current <= deadline)
                  putStrLn ("CANONICAL_CLOCK_WAIT event=" <> show eventId <> " end=" <>
                    show deadline <> " releasedAfter=" <> show observed)
                  releaseBarrier release
                  timeout 5000000 (wait holding) `shouldReturn` Just ()
                  result <- timeout 5000000 (wait buying)
                  status <- case result of
                    Just value -> pure (either errHTTPCode (const 200) value)
                    Nothing -> fail "Canonical checkout did not finish after deadline barrier"
                  effects <- runSqlPool (rawSql
                    "SELECT quantity_sold,(SELECT count(*)::bigint FROM event_ticket_order WHERE event_id=?),\
                    \ coalesce((SELECT sum(current_redemptions)::bigint FROM promo_code WHERE event_id=?),0)::bigint\
                    \ FROM event_ticket_tier WHERE id=?"
                    [PersistInt64 eventId,PersistInt64 eventId,PersistInt64 tierId]) pool
                    :: IO [(Single Int64,Single Int64,Single Int64)]
                  let n = if elapsed then 0 else 1 :: Int64
                      r = if not elapsed && promotion then 1 else 0 :: Int64
                  (status,effects) `shouldBe` (if elapsed then 409 else 200,
                    [(Single n,Single n,Single r)])
                ) `finally` releaseBarrier release
            ) `finally` releaseBarrier release

      it "lets a held order start payment while the event sells tickets" $ \pool -> do
        order <- newOrder pool "open"
        view <- run pool (getOrder (oEvent order) (oOrder order) (Just (oToken order)))
        fmap Routes.paymentStatus view `shouldSatisfy` either (const False) (`elem` ["holding", "awaiting_payment"])
        ticketSalesOpenFor pool order `shouldReturn` True
        run pool (datafastCreate (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldNotSatisfy` salesClosed)
        run pool (paypalCreate (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldNotSatisfy` salesClosed)
        run pool (bankSelect (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldNotSatisfy` salesClosed)
        hostedSession pool order >>= (`shouldNotSatisfy` salesClosed)

      it "offers no payment method and refuses every payment start once sales close" $ \pool -> do
        order <- newOrder pool "closed"
        closeSales pool order "postponed"
        view <- run pool (getOrder (oEvent order) (oOrder order) (Just (oToken order)))
        fmap Routes.paymentMethods view `shouldBe` Right []
        fmap Routes.orderId view `shouldBe` Right (oOrder order)
        ticketSalesOpenFor pool order `shouldReturn` False
        run pool (datafastCreate (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldSatisfy` salesClosed)
        run pool (paypalCreate (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldSatisfy` salesClosed)
        run pool (bankSelect (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldSatisfy` salesClosed)
        run pool (paypalCapture (oEvent order) (oOrder order) (Just (oToken order)) captureRequest)
          >>= (`shouldSatisfy` salesClosed)
        hostedSession pool order >>= (`shouldSatisfy` salesClosed)
        attempts <- scalar pool "SELECT count(*) FROM commerce_payment_attempt WHERE checkout_id=?::uuid" [PersistText (oCheckout order)]
        attempts `shouldBe` 0

      it "applies the same refusal to a cancelled event" $ \pool -> do
        order <- newOrder pool "cancelled"
        closeSales pool order "cancelled"
        run pool (datafastCreate (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldSatisfy` salesClosed)
        hostedSession pool order >>= (`shouldSatisfy` salesClosed)

      it "sends no new PayPal capture after sales close, even when an earlier attempt was recorded" $ \pool -> do
        -- A recorded attempt does not prove the capture reached PayPal (it is
        -- stored before the access token is fetched). Only PayPal reporting the
        -- order completed may be reconciled; without that proof it is refused.
        order <- newOrder pool "capture-retry"
        runSqlPool (rawExecute
          "INSERT INTO commerce_payment_attempt (id, checkout_id, provider, environment, operation, status, amount_minor,\
          \ currency, merchant_account_ref, idempotency_key, created_at, updated_at)\
          \ SELECT gen_random_uuid(), id, 'paypal', environment, 'capture', 'failed', total_minor, currency,\
          \ 'synthetic-merchant', 'synthetic-capture-retry', now(), now() FROM commerce_checkout_session WHERE id=?::uuid"
          [PersistText (oCheckout order)]) pool
        closeSales pool order "postponed"
        run pool (paypalCapture (oEvent order) (oOrder order) (Just (oToken order)) captureRequest)
          >>= (`shouldSatisfy` salesClosed)
        attempts <- scalar pool "SELECT count(*) FROM commerce_payment_attempt WHERE checkout_id=?::uuid" [PersistText (oCheckout order)]
        attempts `shouldBe` 1

      it "accepts evidence for a transfer the buyer selected before sales closed" $ \pool -> do
        activateBankTransfer pool
        order <- newOrder pool "evidence"
        selected <- run pool (bankSelect (oEvent order) (oOrder order) (Just (oToken order)))
        fmap (fmap Routes.evidenceStatus . Routes.bankTransfer) selected `shouldBe` Right (Just "awaiting_evidence")
        closeSales pool order "postponed"
        submitted <- run pool (bankEvidence (oEvent order) (oOrder order) (Just (oToken order))
          (Routes.PublicEventTicketBankTransferEvidenceRequest "SYNTHETIC-REF-1" Nothing))
        fmap (fmap Routes.evidenceStatus . Routes.bankTransfer) submitted `shouldBe` Right (Just "submitted")
        stored <- runSqlPool (rawSql
          "SELECT count(*) FROM commerce_manual_payment_evidence WHERE customer_reference='SYNTHETIC-REF-1'" []) pool
        stored `shouldBe` [Single (1 :: Int64)]

      it "binds private receipt evidence and refuses a different-file replay" $ \pool ->
        withSystemTempDirectory "tdf-ticket-receipt" $ \root ->
          bracket (lookupEnv "TICKET_PAYMENT_EVIDENCE_DIR")
            (maybe (unsetEnv "TICKET_PAYMENT_EVIDENCE_DIR") (setEnv "TICKET_PAYMENT_EVIDENCE_DIR")) $ \_ -> do
            setEnv "TICKET_PAYMENT_EVIDENCE_DIR" root
            activateBankTransfer pool
            order <- newOrder pool "receipt"
            _ <- run pool (bankSelect (oEvent order) (oOrder order) (Just (oToken order)))
            let file = Receipt.TicketPaymentReceipt "application/pdf" "JVBERi0xLjc="
                request = Routes.PublicEventTicketBankTransferEvidenceRequest "SYNTHETIC-RECEIPT" (Just file)
                submit token payload = run pool (bankEvidence (oEvent order) (oOrder order) token payload)
            missingCapability <- submit (Just "wrong-token") request
            missingCapability `shouldSatisfy` isLeft
            before <- scalar pool "SELECT count(*) FROM commerce_manual_payment_evidence WHERE checkout_id=?::uuid AND evidence_object_key IS NOT NULL" [PersistText (oCheckout order)]
            before `shouldBe` 0
            submitted <- submit (Just (oToken order)) request
            fmap (fmap Routes.receiptAttached . Routes.bankTransfer) submitted `shouldBe` Right (Just True)
            replay <- submit (Just (oToken order)) request
            fmap (fmap Routes.receiptAttached . Routes.bankTransfer) replay `shouldBe` Right (Just True)
            let changed = Routes.PublicEventTicketBankTransferEvidenceRequest "SYNTHETIC-RECEIPT"
                  (Just (Receipt.TicketPaymentReceipt "application/pdf" "JVBERi0yLjA="))
            conflict <- submit (Just (oToken order)) changed
            fmap errHTTPCode (either Just (const Nothing) conflict) `shouldBe` Just 409
            keys <- runSqlPool (rawSql "SELECT evidence_object_key FROM commerce_manual_payment_evidence WHERE checkout_id=?::uuid"
              [PersistText (oCheckout order)] :: SqlPersistT IO [Single (Maybe Text)]) pool
            case keys of
              [Single (Just key)] -> run pool (Receipt.loadReceipt (oOrder order) key) `shouldReturn` Right file
              _ -> expectationFailure "Expected one private receipt"
            let user role party = AuthedUser (toSqlKey party) [role] (modulesForRoles [role]) Nothing Nothing
                readAs actor event = run pool (manualReceiptFor actor (T.pack (show event)) (T.pack (show (oOrder order))))
            adminView <- readAs (user Admin 1) (oEvent order)
            fmap getResponse adminView `shouldBe` Right file
            fmap (lookup "Cache-Control" . getHeaders) adminView `shouldBe` Right (Just "no-store")
            runSqlPool (rawExecute "UPDATE social_event SET organizer_party_id='2' WHERE id=?" [PersistInt64 (oEvent order)]) pool
            organizerView <- readAs (user Fan 2) (oEvent order)
            fmap getResponse organizerView `shouldBe` Right file
            outsiderView <- readAs (user Fan 3) (oEvent order)
            fmap errHTTPCode (either Just (const Nothing) outsiderView) `shouldSatisfy` (`elem` [Just 403, Just 404])
            other <- newOrder pool "other-receipt-event"
            wrongEvent <- readAs (user Admin 1) (oEvent other)
            fmap errHTTPCode (either Just (const Nothing) wrongEvent) `shouldBe` Just 404

      it "does not gate a checkout that is already processing" $ \pool -> do
        order <- newOrder pool "processing"
        runSqlPool (rawExecute "UPDATE commerce_checkout_session SET status='processing' WHERE id=?::uuid"
          [PersistText (oCheckout order)]) pool
        closeSales pool order "postponed"
        run pool (datafastCreate (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldNotSatisfy` salesClosed)

      it "makes a closure wait for a payment authorization that already holds the event" $ \pool -> do
        order <- newOrder pool "ordering"
        held <- newEmptyMVar
        release <- newEmptyMVar
        finished <- newEmptyMVar
        void $ forkIO $ do
          open <- runSqlPool (do
            open <- lockTicketOrderSalesOpen (T.pack (show (oOrder order)))
            liftIO (putMVar held ())
            liftIO (takeMVar release)
            pure open) pool
          putMVar finished open
        takeMVar held
        blocked <- try (runSqlPool (do
          rawExecute "SET LOCAL lock_timeout = '300ms'" []
          rawExecute "UPDATE social_event SET updated_at = now() WHERE id=?" [PersistInt64 (oEvent order)]) pool)
          :: IO (Either SomeException ())
        blocked `shouldSatisfy` isLeft
        putMVar release ()
        takeMVar finished `shouldReturn` True
        closeSales pool order "postponed"
        runSqlPool (lockTicketOrderSalesOpen (T.pack (show (oOrder order)))) pool `shouldReturn` False

      it "keeps the event locked from the sales check until the payment attempt is committed" $ \pool -> do
        -- Through the real attempt path, which commits once before it inserts:
        -- a closure that arrives after the sales check must find the attempt
        -- already recorded when it finally gets the event row.
        order <- newOrder pool "attempt-order"
        activateBankTransfer pool
        totals <- runSqlPool (rawSql "SELECT total_minor, currency FROM commerce_checkout_session WHERE id=?::uuid"
          [PersistText (oCheckout order)]) pool
        (total, currency) <- case totals of
          [(Single amount, Single code)] -> pure (amount :: Int64, code :: Text)
          _ -> fail "Expected one checkout"
        now <- getCurrentTime
        let orderText = T.pack (show (oOrder order))
            creation = Checkout.PaymentAttemptCreation
              { Checkout.pacCheckout = Checkout.CheckoutReference (oCheckout order)
              , Checkout.pacProvider = Checkout.ProviderBankTransfer
              , Checkout.pacEnvironment = Checkout.CheckoutSandbox
              , Checkout.pacOperation = Checkout.OperationManualVerify
              , Checkout.pacAmountMinor = total
              , Checkout.pacCurrency = currency
              , Checkout.pacMerchantRef = "tdf-manual-settlement"
              , Checkout.pacIdempotencyKey = "synthetic-attempt-order"
              , Checkout.pacCreatedAt = now
              , Checkout.pacCorrelationId = "synthetic-attempt-order"
              }
        checked <- newEmptyMVar
        release <- newEmptyMVar
        attempted <- newEmptyMVar
        closureSaw <- newEmptyMVar
        let salesGuard = do
              refusal <- ticketSalesClosedRefusal <$> lockTicketOrderSalesOpen orderText
              liftIO (putMVar checked ())
              liftIO (takeMVar release)
              pure refusal
        void $ forkIO $ do
          outcome <- try (runSqlPool (PaymentRuntime.beginPaymentAttemptGuarded salesGuard creation) pool)
          putMVar attempted (either (\err -> Left (T.pack (show (err :: SomeException)))) id outcome)
        takeMVar checked
        void $ forkIO $ do
          seen <- runSqlPool (do
            rawExecute
              "UPDATE social_event SET workflow_state_id = (SELECT s.id FROM workflow_state s JOIN workflow_definition d ON d.id=s.workflow_id\
              \ WHERE d.code='social-event-lifecycle' AND s.code='postponed') WHERE id=?"
              [PersistInt64 (oEvent order)]
            rawSql "SELECT count(*) FROM commerce_payment_attempt WHERE checkout_id=?::uuid" [PersistText (oCheckout order)]) pool
          putMVar closureSaw (seen :: [Single Int64])
        threadDelay 400000
        putMVar release ()
        outcome <- takeMVar attempted
        fmap (const ()) outcome `shouldBe` Right ()
        takeMVar closureSaw `shouldReturn` [Single 1]

      it "keeps other rails closed while a PayPal capture is unverified, until it is settled" $ \pool -> do
        -- A capture whose request and readback both failed may have charged the
        -- buyer. The order must not offer or start another rail until PayPal
        -- gives a definite answer for that same attempt.
        activateBankTransfer pool
        order <- newOrder pool "unverified"
        let checkout = Checkout.CheckoutReference (oCheckout order)
            offersTransfer = either (const False) (elem "bank_transfer" . Routes.paymentMethods)
            view = run pool (getOrder (oEvent order) (oOrder order) (Just (oToken order)))
            setFailureCode code = runSqlPool (rawExecute
              "UPDATE commerce_payment_attempt SET failure_code=? WHERE checkout_id=?::uuid AND provider='paypal'"
              [PersistText code, PersistText (oCheckout order)]) pool
        view >>= (`shouldSatisfy` offersTransfer)
        runSqlPool (do
          rawExecute
            "INSERT INTO commerce_payment_attempt (id, checkout_id, provider, environment, operation, status, amount_minor,\
            \ currency, merchant_account_ref, idempotency_key, created_at, updated_at)\
            \ SELECT gen_random_uuid(), id, 'paypal', environment, 'capture', 'failed', total_minor, currency,\
            \ 'synthetic-merchant', 'synthetic-capture-unverified', now(), now() FROM commerce_checkout_session WHERE id=?::uuid"
            [PersistText (oCheckout order)]
          rawExecute "UPDATE commerce_checkout_session SET status='failed' WHERE id=?::uuid"
            [PersistText (oCheckout order)]) pool
        -- A failed capture alone is not the unverified state.
        runSqlPool (ticketCaptureUnverified checkout) pool `shouldReturn` False
        view >>= (`shouldSatisfy` offersTransfer)

        setFailureCode (paypalCaptureFailureCode Nothing)
        runSqlPool (ticketCaptureUnverified checkout) pool `shouldReturn` True
        view >>= (`shouldNotSatisfy` offersTransfer)
        fmap Routes.orderId <$> view `shouldReturn` Right (oOrder order)
        run pool (bankSelect (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldSatisfy` captureUnverified)
        transfers <- scalar pool
          "SELECT count(*) FROM commerce_payment_attempt WHERE checkout_id=?::uuid AND provider='bank_transfer'"
          [PersistText (oCheckout order)]
        transfers `shouldBe` 0

        -- Missing/ambiguous captures and refund evidence never mean no charge.
        forM_ ["UNKNOWN", "PARTIALLY_REFUNDED", "REFUNDED", "PENDING", "VOIDED"] $ \status -> do
          setFailureCode (paypalCaptureFailureCode (Just status))
          runSqlPool (ticketCaptureUnverified checkout) pool `shouldReturn` True
          view >>= (`shouldNotSatisfy` offersTransfer)
          run pool (bankSelect (oEvent order) (oOrder order) (Just (oToken order)))
            >>= (`shouldSatisfy` captureUnverified)
        -- COMPLETED with the wrong merchant/order/amount is not proof that no
        -- charge occurred. Reconciliation must retain the same cross-rail fence.
        setFailureCode "provider_binding_mismatch"
        runSqlPool (ticketCaptureUnverified checkout) pool `shouldReturn` True
        view >>= (`shouldNotSatisfy` offersTransfer)
        run pool (bankSelect (oEvent order) (oOrder order) (Just (oToken order)))
          >>= (`shouldSatisfy` captureUnverified)
        -- DECLINED is a nested capture status; VOIDED is an order status.
        setFailureCode (paypalCaptureFailureCode (Just "DECLINED"))
        runSqlPool (ticketUnverifiedCaptureRefusal checkout) pool `shouldReturn` Nothing
        view >>= (`shouldSatisfy` offersTransfer)
        selected <- run pool (bankSelect (oEvent order) (oOrder order) (Just (oToken order)))
        fmap (fmap Routes.evidenceStatus . Routes.bankTransfer) selected `shouldBe` Right (Just "awaiting_evidence")

      it "rechecks capture uncertainty after a competing failure writer releases the checkout" $ \pool -> do
        activateBankTransfer pool
        order <- newOrder pool "capture-failure-race"
        let checkout = Checkout.CheckoutReference (oCheckout order)
        [(Single attemptId, Single total, Single currency)] <- runSqlPool (rawSql
          "INSERT INTO commerce_payment_attempt (checkout_id,provider,environment,operation,status,amount_minor,\
          \ currency,merchant_account_ref,idempotency_key,created_at,updated_at)\
          \ SELECT id,'paypal',environment,'capture','failed',total_minor,currency,\
          \ 'synthetic-merchant','synthetic-capture-failure-race',now(),now()\
          \ FROM commerce_checkout_session WHERE id=?::uuid RETURNING id::text,amount_minor,currency"
          [PersistText (oCheckout order)]) pool
        now <- getCurrentTime
        let creation = Checkout.PaymentAttemptCreation checkout Checkout.ProviderBankTransfer
              Checkout.CheckoutSandbox Checkout.OperationManualVerify total currency
              "tdf-manual-settlement" "synthetic-fenced-bank-race" now "synthetic-fenced-bank-race"
        held <- newEmptyMVar
        release <- newEmptyMVar
        entering <- newEmptyMVar
        let writer = runSqlPool (do
              _ <- rawSql "SELECT id::text FROM commerce_checkout_session WHERE id=?::uuid FOR UPDATE"
                [PersistText (oCheckout order)] :: SqlPersistT IO [Single Text]
              [Single pid] <- rawSql "SELECT pg_backend_pid()" []
              liftIO (putMVar held (pid :: Int64))
              liftIO (takeMVar release)
              Checkout.recordPaymentFailure checkout (Checkout.PaymentAttemptReference attemptId)
                Checkout.ProviderPayPal "paypal_capture_unverified" "synthetic-failure-race" now) pool
            guard = do
              [Single pid] <- rawSql "SELECT pg_backend_pid()" []
              liftIO (putMVar entering (pid :: Int64))
              ticketUnverifiedCaptureRefusal checkout
            reader = runSqlPool (PaymentRuntime.beginPaymentAttemptGuarded guard creation) pool
            observeBlocked holder waiter = do
              rows <- runSqlPool (rawSql "SELECT ?::integer = ANY(pg_blocking_pids(?::integer))"
                [PersistInt64 holder, PersistInt64 waiter]) pool
              if rows == [Single True] then pure ()
                else threadDelay 10000 >> observeBlocked holder waiter
        withAsync writer $ \writing ->
          (do
            holder <- timeout 5000000 (takeMVar held)
              >>= maybe (fail "Failure writer did not acquire the checkout") pure
            withAsync reader $ \reading ->
              (do
                waiter <- timeout 5000000 (takeMVar entering)
                  >>= maybe (fail "Payment admission did not reach its guard") pure
                timeout 5000000 (observeBlocked holder waiter) `shouldReturn` Just ()
                void (tryPutMVar release ())
                timeout 5000000 (wait writing) `shouldReturn` Just ()
                result <- timeout 5000000 (wait reading)
                result `shouldSatisfy` maybe False isLeft
                scalar pool "SELECT count(*) FROM commerce_payment_attempt WHERE checkout_id=?::uuid AND provider='bank_transfer'"
                  [PersistText (oCheckout order)] `shouldReturn` 0
              ) `finally` void (tryPutMVar release ())
          ) `finally` void (tryPutMVar release ())

      it "treats an unknown or malformed ticket order as not on sale" $ \pool -> do
        runSqlPool (ticketOrderSalesOpen "999999999") pool `shouldReturn` False
        runSqlPool (ticketOrderSalesOpen "not-a-number") pool `shouldReturn` False

-- | Mark the sandbox bank-transfer account ready and its capabilities verified,
-- so the manual rail and the canonical attempt path are reachable.
activateBankTransfer :: ConnectionPool -> IO ()
activateBankTransfer pool =
  runSqlPool (do
    rawExecute "UPDATE commerce_provider_account SET enabled=TRUE, status='ready', credential_status='validated',\
            \ contract_status='approved', merchant_account_ref='tdf-manual-settlement', verified_at=now(), verified_by=1\
            \ WHERE provider='bank_transfer' AND environment='sandbox'" []
    rawExecute "UPDATE commerce_provider_capability SET verification_status='sandbox_verified', verified_at=now()\
            \ WHERE provider_account_id IN (SELECT id FROM commerce_provider_account\
            \ WHERE provider='bank_transfer' AND environment='sandbox')" []) pool

data Order = Order
  { oEvent    :: Int64
  , oOrder    :: Int64
  , oToken    :: Text
  , oCheckout :: Text
  }

type App = ReaderT Env Handler

getOrder :: Int64 -> Int64 -> Maybe Text -> App Routes.PublicEventTicketCheckoutResponse
createOrder :: Int64 -> Maybe Text -> Routes.PublicEventTicketCheckoutRequest -> App Routes.PublicEventTicketCheckoutResponse
_ :<|> createOrder :<|> getOrder :<|> datafastCreate :<|> _ :<|> paypalCreate :<|> paypalCapture
  :<|> bankSelect :<|> bankEvidence = publicEventTicketsServer

manualReceiptFor :: AuthedUser -> Text -> Text -> App (Headers '[Header "Cache-Control" Text] Receipt.TicketPaymentReceipt)
manualReceiptFor user = case socialEventsServer user of
  _events :<|> _cities :<|> _sources :<|> _research :<|> _venues :<|> _artists
    :<|> _rsvps :<|> _invitations :<|> _moments :<|> _live :<|> tickets :<|> _ -> case tickets of
      _tiers :<|> _createTier :<|> _updateTier :<|> _myOrders :<|> _orders
        :<|> _createOrder :<|> _updateOrder :<|> _tickets :<|> _checkIn
        :<|> _promos :<|> _createPromo :<|> _updatePromo :<|> _validatePromo
        :<|> _stripe :<|> _requestRefund :<|> _refunds :<|> _approveRefund :<|> _rejectRefund
        :<|> _manualPayments :<|> _review :<|> receipt :<|> _ -> receipt

captureRequest :: Routes.PublicEventTicketPaypalCaptureRequest
captureRequest = Routes.PublicEventTicketPaypalCaptureRequest "SYNTHETIC-PAYPAL-ORDER"

run :: ConnectionPool -> App a -> IO (Either ServerError a)
run pool action = do
  cfg <- loadConfig
  runHandler (runReaderT action (Env pool cfg))

salesClosed :: Either ServerError a -> Bool
salesClosed (Left err) = errHTTPCode err == 409 && "no longer on sale" `isInfixOf` BL.unpack (errBody err)
salesClosed (Right _) = False

captureUnverified :: Either ServerError a -> Bool
captureUnverified (Left err) = errHTTPCode err == 409 && "could not be verified" `isInfixOf` BL.unpack (errBody err)
captureUnverified (Right _) = False

hostedSession :: ConnectionPool -> Order -> IO (Either ServerError ())
hostedSession pool order = do
  let createSession :<|> _ = providerExecutionServer
  fmap (const ()) <$> run pool (createSession (oCheckout order) (Just (oToken order))
    (Just ("synthetic-hosted-session-" <> T.pack (show (oOrder order)))) (Just "ticket-payment-gate-spec")
    (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    (PaymentSessionCreateDTO "placetopay" "card" Nothing Nothing))

ticketSalesOpenFor :: ConnectionPool -> Order -> IO Bool
ticketSalesOpenFor pool order = runSqlPool (ticketOrderSalesOpen (T.pack (show (oOrder order)))) pool

closeSales :: ConnectionPool -> Order -> Text -> IO ()
closeSales pool order stateCode = runSqlPool (rawExecute
  "UPDATE social_event SET workflow_state_id = (SELECT s.id FROM workflow_state s JOIN workflow_definition d ON d.id=s.workflow_id\
  \ WHERE d.code='social-event-lifecycle' AND s.code=?) WHERE id=?"
  [PersistText stateCode, PersistInt64 (oEvent order)]) pool

scalar :: ConnectionPool -> Text -> [PersistValue] -> IO Int64
scalar pool sql params = do
  rows <- runSqlPool (rawSql sql params) pool
  case rows of
    [Single value] -> pure value
    _ -> fail "Expected one scalar"

-- | A fresh on-sale event with one tier and an approved policy, and a held order
-- for it created through the public checkout handler.
newOrder :: ConnectionPool -> Text -> IO Order
newOrder pool label = do
  (eventId,tierId) <- newOrderFixture pool label
  created <- run pool (createOrder eventId (Just ("ticket-payment-gate-" <> label <> "-0001"))
    (Routes.PublicEventTicketCheckoutRequest tierId 1 "Synthetic Buyer"
      ("gate-" <> label <> "@example.test") Nothing Nothing True Nothing Nothing Nothing))
  case created of
    Left err -> fail ("Ticket order was not created: " <> show (errHTTPCode err) <> " " <> BL.unpack (errBody err))
    Right response -> case Routes.lookupToken response of
      Nothing -> fail "Created ticket order returned no lookup token"
      Just token -> pure (Order eventId (Routes.orderId response) token (Routes.checkoutId response))

-- A fresh valid checkout fixture, before a new request reserves any stock.
newOrderFixture :: ConnectionPool -> Text -> IO (Int64,Int64)
newOrderFixture pool label = do
  events <- runSqlPool (rawSql
    "INSERT INTO social_event(title,start_time,end_time,workflow_state_id,event_type_id,created_at,updated_at)\
    \ SELECT ?, now()+interval '30 days', now()+interval '30 days 3 hours', s.id,\
    \ (SELECT id FROM event_type ORDER BY id LIMIT 1), now(), now()\
    \ FROM workflow_state s JOIN workflow_definition d ON d.id=s.workflow_id\
    \ WHERE d.code='social-event-lifecycle' AND s.code='on_sale' RETURNING id"
    [PersistText ("Synthetic gate event " <> label)]) pool
  eventId <- case events of
    [Single key] -> pure (key :: Int64)
    _ -> fail "Expected one event"
  tiers <- runSqlPool (rawSql
    "INSERT INTO event_ticket_tier(event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active,\
    \ created_at,updated_at,enable_waitlist,allow_transfers,refund_policy)\
    \ VALUES (?, 'general', 'General', 1000, 'USD', 50, 0, TRUE, now(), now(), FALSE, FALSE, 'none') RETURNING id"
    [PersistInt64 eventId]) pool
  tierId <- case tiers of
    [Single key] -> pure (key :: Int64)
    _ -> fail "Expected one tier"
  runSqlPool (rawExecute
    "INSERT INTO event_ticket_checkout_policy(event_id,policy_version,currency,terms_version,terms_summary,refund_policy,\
    \ approval_status,active,approved_at,approved_by,created_at,updated_at,\
    \ manual_transfer_hold_minutes,manual_transfer_cutoff_at)\
    \ VALUES (?, 'gate-v1', 'USD', 'gate-terms-v1', 'Synthetic terms', 'Synthetic refund policy',\
    \ 'approved', TRUE, now(), 'synthetic-test', now(), now(), 120, now()+interval '20 days')"
    [PersistInt64 eventId]) pool
  pure (eventId,tierId)

waitForReservationClockLock :: ConnectionPool -> Text -> Int -> IO Bool
waitForReservationClockLock _ _ 0 = pure False
waitForReservationClockLock pool table remaining = do
  rows <- runSqlPool (rawSql
    "SELECT count(*)::bigint FROM pg_stat_activity WHERE datname=current_database()\
    \ AND pid<>pg_backend_pid() AND wait_event_type='Lock' AND lower(query) LIKE ?"
    [PersistText ("%" <> table <> "%")]) pool :: IO [Single Int64]
  if rows == [Single 1] then pure True else threadDelay 25000
    >> waitForReservationClockLock pool table (remaining-1)

waitForReservationDeadline :: ConnectionPool -> UTCTime -> Int -> IO UTCTime
waitForReservationDeadline _ _ 0 = fail "Database clock never passed the ticket deadline"
waitForReservationDeadline pool deadline remaining = do
  [Single observed] <- runSqlPool (rawSql "SELECT clock_timestamp()" []) pool
  if observed > deadline then pure observed else threadDelay 25000
    >> waitForReservationDeadline pool deadline (remaining-1)
