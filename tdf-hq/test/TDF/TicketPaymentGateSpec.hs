{-# LANGUAGE OverloadedStrings #-}
module TDF.TicketPaymentGateSpec (spec) where

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool)
import Network.Socket (SockAddr(SockAddrInet), tupleToHostAddress)
import Servant (Handler, ServerError, errBody, errHTTPCode, runHandler, (:<|>)(..))
import System.Environment (lookupEnv)
import Test.Hspec

import TDF.API.ProviderExecution (PaymentSessionCreateDTO(..))
import TDF.Config (loadConfig)
import TDF.DB (Env(..))
import qualified TDF.Routes.EventTickets as Routes
import TDF.Server.EventTicketCheckout (publicEventTicketsServer, ticketOrderSalesOpen)
import TDF.Server.ProviderExecution (providerExecutionServer)

-- | A held, unpaid ticket order must not be payable once its event stops selling
-- tickets, through any endpoint that starts or takes a payment. Orders are
-- created through the real checkout handler against the production schema.
spec :: Spec
spec = describe "ticket-payment-gate-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_TICKET_PAYMENT_GATE_TEST_DB")
  case configured of
    Nothing -> it "requires the isolated runner" $ pendingWith "Run scripts/test-ticket-payment-gate.sh"
    Just connection | not ("dbname=tdf_ticket_payment_gate_test_" `isInfixOf` connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 4)) $ do
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

      it "still lets a repeated PayPal capture reconcile after sales close" $ \pool -> do
        order <- newOrder pool "capture-retry"
        runSqlPool (rawExecute
          "INSERT INTO commerce_payment_attempt (id, checkout_id, provider, environment, operation, status, amount_minor,\
          \ currency, merchant_account_ref, idempotency_key, created_at, updated_at)\
          \ SELECT gen_random_uuid(), id, 'paypal', environment, 'capture', 'failed', total_minor, currency,\
          \ 'synthetic-merchant', 'synthetic-capture-retry', now(), now() FROM commerce_checkout_session WHERE id=?::uuid"
          [PersistText (oCheckout order)]) pool
        closeSales pool order "postponed"
        run pool (paypalCapture (oEvent order) (oOrder order) (Just (oToken order)) captureRequest)
          >>= (`shouldNotSatisfy` salesClosed)

      it "does not gate evidence for a transfer the buyer already made" $ \pool -> do
        order <- newOrder pool "evidence"
        closeSales pool order "postponed"
        run pool (bankEvidence (oEvent order) (oOrder order) (Just (oToken order))
            (Routes.PublicEventTicketBankTransferEvidenceRequest "SYNTHETIC-REF-1"))
          >>= (`shouldNotSatisfy` salesClosed)

      it "does not gate a checkout that is already processing" $ \pool -> do
        order <- newOrder pool "processing"
        runSqlPool (rawExecute "UPDATE commerce_checkout_session SET status='processing' WHERE id=?::uuid"
          [PersistText (oCheckout order)]) pool
        closeSales pool order "postponed"
        run pool (datafastCreate (oEvent order) (oOrder order) (Just (oToken order))) >>= (`shouldNotSatisfy` salesClosed)

      it "treats an unknown or malformed ticket order as not on sale" $ \pool -> do
        runSqlPool (ticketOrderSalesOpen "999999999") pool `shouldReturn` False
        runSqlPool (ticketOrderSalesOpen "not-a-number") pool `shouldReturn` False

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

captureRequest :: Routes.PublicEventTicketPaypalCaptureRequest
captureRequest = Routes.PublicEventTicketPaypalCaptureRequest "SYNTHETIC-PAYPAL-ORDER"

run :: ConnectionPool -> App a -> IO (Either ServerError a)
run pool action = do
  cfg <- loadConfig
  runHandler (runReaderT action (Env pool cfg))

salesClosed :: Either ServerError a -> Bool
salesClosed (Left err) = errHTTPCode err == 409 && "no longer on sale" `isInfixOf` BL.unpack (errBody err)
salesClosed (Right _) = False

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
    \ approval_status,active,approved_at,approved_by,created_at,updated_at)\
    \ VALUES (?, 'gate-v1', 'USD', 'gate-terms-v1', 'Synthetic terms', 'Synthetic refund policy',\
    \ 'approved', TRUE, now(), 'synthetic-test', now(), now())"
    [PersistInt64 eventId]) pool
  created <- run pool (createOrder eventId (Just ("ticket-payment-gate-" <> label <> "-0001"))
    (Routes.PublicEventTicketCheckoutRequest tierId 1 "Synthetic Buyer"
      ("gate-" <> label <> "@example.test") Nothing Nothing True Nothing Nothing Nothing))
  case created of
    Left err -> fail ("Ticket order was not created: " <> show (errHTTPCode err) <> " " <> BL.unpack (errBody err))
    Right response -> case Routes.lookupToken response of
      Nothing -> fail "Created ticket order returned no lookup token"
      Just token -> pure (Order eventId (Routes.orderId response) token (Routes.checkoutId response))
