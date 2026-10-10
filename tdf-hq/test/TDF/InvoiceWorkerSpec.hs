{-# LANGUAGE OverloadedStrings #-}
module TDF.InvoiceWorkerSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (mapConcurrently, withAsync, wait)
import Control.Exception (ErrorCall(..), finally, throwIO)
import Control.Monad (void)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (toPersistValue)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool, toSqlKey)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Test.Hspec
import TDF.Invoice.Datil
  ( DatilConfig(..), DatilTransport, TransportResult(..), processNextTaxDocumentWith )
import TDF.Server.TicketTaxDocuments (retryFailedTicketTaxDocument)

-- EVT-TICKET-INVOICE-001: the worker's claim, submission mark and lease fencing
-- against PostgreSQL. The provider is replaced by a recording transport; no
-- request leaves the process. Every case owns one document in the reserved
-- 999-999 series of the sandbox environment.
spec :: Spec
spec = describe "invoice-worker-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_INVOICE_WORKER_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-invoice-worker.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 6)) $
      afterAll destroyAllResources $ do
        it "submits once and records the provider authorization" $ \pool -> do
          document <- fixture pool
          calls <- newIORef []
          tick pool (recording calls (const (pure authorized))) `shouldReturn` True
          stateOf pool document `shouldReturn` ("authorized", True)
          tick pool (recording calls (const (pure authorized))) `shouldReturn` False
          methods calls `shouldReturn` ["POST"]

        it "never resends a submission whose outcome is unknown" $ \pool -> do
          document <- fixture pool
          calls <- newIORef []
          tick pool (recording calls (const (pure TransportUnknown))) `shouldReturn` True
          stateOf pool document `shouldReturn` ("uncertain", True)
          makeDue pool document
          tick pool (recording calls (const (pure authorized))) `shouldReturn` False
          runSqlPool (retryFailedTicketTaxDocument (toSqlKey (docEvent document)) (docId document)) pool
            `shouldReturn` False
          stateOf pool document `shouldReturn` ("uncertain", True)
          methods calls `shouldReturn` ["POST"]

        it "treats a submission interrupted after it started as unknown, not as unsent" $ \pool -> do
          document <- fixture pool
          calls <- newIORef []
          tick pool (recording calls (const (throwIO (ErrorCall "worker stopped")))) `shouldReturn` True
          stateOf pool document `shouldReturn` ("pending", True)
          makeDue pool document
          tick pool (recording calls (const (pure authorized))) `shouldReturn` True
          stateOf pool document `shouldReturn` ("uncertain", True)
          methods calls `shouldReturn` ["POST"]

        it "discards the result of a worker whose lease was taken over" $ \pool -> do
          document <- fixture pool
          slowCalls <- newIORef []
          takeoverCalls <- newIORef []
          entered <- newEmptyMVar
          release <- newEmptyMVar
          let slow = recording slowCalls $ const $ do
                putMVar entered ()
                takeMVar release
                pure authorized
          withAsync (tick pool slow) $ \first ->
            (do
              within (takeMVar entered)
              runSqlPool (rawExecute
                "UPDATE commerce_tax_document SET lease_expires_at = clock_timestamp() - INTERVAL '1 second' WHERE id = ?::uuid"
                [toPersistValue (docId document)]) pool
              tick pool (recording takeoverCalls (const (pure authorized))) `shouldReturn` True
              stateOf pool document `shouldReturn` ("uncertain", True)
              putMVar release ()
              void (within (wait first))
              stateOf pool document `shouldReturn` ("uncertain", True)
              methods slowCalls `shouldReturn` ["POST"]
              methods takeoverCalls `shouldReturn` []
            ) `finally` void (tryPutMVar release ())

        it "polls a document the provider is still processing instead of resending it" $ \pool -> do
          document <- fixture pool
          calls <- newIORef []
          tick pool (recording calls (const (pure processing))) `shouldReturn` True
          stateOf pool document `shouldReturn` ("submitted", True)
          makeDue pool document
          tick pool (recording calls (const (pure TransportUnknown))) `shouldReturn` True
          stateOf pool document `shouldReturn` ("submitted", True)
          makeDue pool document
          tick pool (recording calls (const (pure authorized))) `shouldReturn` True
          stateOf pool document `shouldReturn` ("authorized", True)
          logged <- readIORef calls
          [(method, path) | (method, path, _) <- logged]
            `shouldBe` [("POST", "/invoices/issue"), ("GET", "/invoices/prov-1"), ("GET", "/invoices/prov-1")]

        it "resends a refused document only after an administrator asks, with the same identity" $ \pool -> do
          document <- fixture pool
          calls <- newIORef []
          tick pool (recording calls (const (pure (TransportRejected 400 "invalid")))) `shouldReturn` True
          stateOf pool document `shouldReturn` ("failed", True)
          makeDue pool document
          tick pool (recording calls (const (pure authorized))) `shouldReturn` False
          runSqlPool (retryFailedTicketTaxDocument (toSqlKey (docEvent document + 1)) (docId document)) pool
            `shouldReturn` False
          runSqlPool (retryFailedTicketTaxDocument (toSqlKey (docEvent document)) (docId document)) pool
            `shouldReturn` True
          tick pool (recording calls (const (pure authorized))) `shouldReturn` True
          stateOf pool document `shouldReturn` ("authorized", True)
          logged <- readIORef calls
          case logged of
            [("POST", _, firstBody), ("POST", _, secondBody)] -> do
              firstBody `shouldSatisfy` (/= Nothing)
              secondBody `shouldBe` firstBody
            other -> expectationFailure ("Expected exactly two submissions, saw " <> show (length other))

        it "lets only one of several simultaneous workers submit a document" $ \pool -> do
          document <- fixture pool
          calls <- newIORef []
          claims <- within (mapConcurrently
            (const (tick pool (recording calls (const (pure authorized))))) [(), (), (), ()])
          length (filter id claims) `shouldBe` 1
          stateOf pool document `shouldReturn` ("authorized", True)
          methods calls `shouldReturn` ["POST"]

data Document = Document { docId :: Text, docEvent :: Int64 }

type Call = (BS.ByteString, String, Maybe Value)

recording :: IORef [Call] -> (Call -> IO TransportResult) -> DatilTransport
recording calls respond method path body = do
  atomicModifyIORef' calls (\logged -> (logged <> [(method, path, body)], ()))
  respond (method, path, body)

methods :: IORef [Call] -> IO [BS.ByteString]
methods calls = map (\(method, _, _) -> method) <$> readIORef calls

authorized :: TransportResult
authorized = TransportOk $ object
  [ "id" .= ("prov-1" :: Text), "estado" .= ("AUTORIZADO" :: Text)
  , "autorizacion" .= object
      [ "numero" .= ("1010202601179321509200110019990000000011234567813" :: Text)
      , "fecha" .= ("2026-10-10T12:00:00" :: Text) ] ]

processing :: TransportResult
processing = TransportOk (object ["id" .= ("prov-1" :: Text), "estado" .= ("RECIBIDO" :: Text)])

tick :: ConnectionPool -> DatilTransport -> IO Bool
tick pool transport = within (processNextTaxDocumentWith transport pool config)

config :: DatilConfig
config = DatilConfig
  { dcApiKey = "key", dcCertificatePassword = "secret", dcEnvironment = 1
  , dcRuc = "1793215092001", dcLegalName = "TDF RECORDS", dcTradeName = "TDF Records"
  , dcAddress = "Quito", dcEstablishmentAddress = "Quito", dcAccountingRequired = False
  , dcSpecialTaxpayer = Nothing }

stateOf :: ConnectionPool -> Document -> IO (Text, Bool)
stateOf pool document = do
  rows <- runSqlPool (rawSql
    "SELECT status, submitted_at IS NOT NULL FROM commerce_tax_document WHERE id = ?::uuid"
    [toPersistValue (docId document)] :: SqlPersistT IO [(Single Text, Single Bool)]) pool
  case rows of
    [(Single status, Single started)] -> pure (status, started)
    _ -> fail "Expected one tax document"

makeDue :: ConnectionPool -> Document -> IO ()
makeDue pool document = runSqlPool (rawExecute
  "UPDATE commerce_tax_document SET next_attempt_at = clock_timestamp() - INTERVAL '1 second' WHERE id = ?::uuid"
  [toPersistValue (docId document)]) pool

-- One unpaid-fixture order with an enqueued invoice. Earlier cases' documents in
-- the reserved series are closed first so the worker can only claim this one.
fixture :: ConnectionPool -> IO Document
fixture pool = do
  n <- randomRIO (10000000, 900000000 :: Int64)
  let key = toPersistValue n
      text = toPersistValue (T.pack (show n))
      checkout = toPersistValue (checkoutId n)
  rows <- runSqlPool (do
    rawExecute
      "UPDATE commerce_tax_document SET status = 'rejected', lease_token = NULL, lease_expires_at = NULL \
      \WHERE environment = 'sandbox' AND establishment = '999' AND emission_point = '999' \
      \AND status IN ('pending','submitted')" []
    claimable <- rawSql
      "SELECT count(*) FROM commerce_tax_document WHERE environment = 'sandbox' AND status IN ('pending','submitted')"
      [] :: SqlPersistT IO [Single Int64]
    if claimable /= [Single 0] then pure [] else do
      rawExecute
        "INSERT INTO social_event(id, title, start_time, end_time, event_type_id, workflow_state_id) \
        \SELECT ?, 'Invoice worker fixture', NOW() + INTERVAL '30 days', NOW() + INTERVAL '31 days', id, \
        \'00000000-0000-4000-8000-000000000232' FROM event_type WHERE code = 'concert'" [key]
      rawExecute
        "INSERT INTO event_ticket_tier(id, event_id, code, name, price_cents, currency, quantity_total, \
        \quantity_sold, is_active, allow_transfers) VALUES (?, ?, 'general', 'General', 2000, 'USD', 20, 0, TRUE, TRUE)"
        [key, key]
      rawExecute
        "INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency, buyer_fee_bps, \
        \organizer_fee_bps, tax_bps, hold_minutes, terms_version, terms_summary, refund_policy, \
        \max_tickets_per_order, tax_invoice_required) VALUES (?, 'invoiced-v1', 'USD', 0, 0, 0, 10, \
        \'invoiced-terms-v1', 'Invoiced terms.', 'Invoiced refund policy.', 4, TRUE)" [key]
      rawExecute
        "UPDATE event_ticket_checkout_policy SET approval_status = 'approved', active = TRUE, \
        \approved_at = NOW(), approved_by = 'invoice-worker-test' WHERE event_id = ?" [key]
      rawExecute
        "INSERT INTO event_ticket_order(id, event_id, tier_id, buyer_name, buyer_email, quantity, amount_cents, \
        \currency, status, original_amount_cents, payment_method, purchased_at) VALUES (?, ?, ?, 'Invoice buyer', \
        \'invoice-worker@example.invalid', 1, 2000, 'USD', 'pending', 2000, 'bank_transfer', NOW())"
        [key, key, key]
      rawExecute
        "INSERT INTO commerce_checkout_session(id, domain_type, domain_order_id, status, environment, currency, \
        \subtotal_minor, tax_minor, total_minor, customer_email, lookup_token_hash, idempotency_key, expires_at) \
        \VALUES (?::uuid, 'event_ticket_order', ?, 'awaiting_payment', 'sandbox', 'USD', 2000, 0, 2000, \
        \'invoice-worker@example.invalid', md5('wa' || ?) || md5('wb' || ?), \
        \'invoice-worker-checkout-idempotency-' || ?, NOW() + INTERVAL '1 hour')"
        [checkout, text, text, text, text]
      rawExecute
        "INSERT INTO commerce_checkout_line_item(checkout_id, line_number, product_type, product_id, \
        \product_version, description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot) \
        \VALUES (?::uuid, 1, 'ticket', ?, '1', 'Invoice worker fixture', 1, 2000, 2000, 2000, '{}')"
        [checkout, text]
      rawExecute
        "INSERT INTO event_ticket_checkout_runtime(order_id, event_id, tier_id, checkout_id, policy_id, \
        \policy_version, lookup_token_hash, create_idempotency_key, create_request_sha256, quantity, currency, \
        \unit_price_minor, gross_face_value_minor, discount_minor, net_face_value_minor, buyer_fee_bps, \
        \buyer_fee_minor, organizer_fee_bps, organizer_fee_minor, tax_bps, tax_minor, checkout_total_minor, \
        \organizer_payable_minor, platform_fee_minor, terms_version, terms_accepted_at, hold_expires_at) \
        \SELECT ?, ?, ?, ?::uuid, p.id, p.policy_version, md5('wc' || ?) || md5('wd' || ?), \
        \'invoice-worker-runtime-idempotency-' || ?, md5('we' || ?) || md5('wf' || ?), 1, 'USD', 2000, 2000, 0, \
        \2000, 0, 0, 0, 0, 0, 0, 2000, 2000, 0, 'invoiced-terms-v1', NOW(), NOW() + INTERVAL '1 hour' \
        \FROM event_ticket_checkout_policy p WHERE p.event_id = ?"
        [key, key, key, checkout, text, text, text, text, text, key]
      rawSql
        "INSERT INTO commerce_tax_document(kind, domain_type, domain_order_id, checkout_id, environment, \
        \provider, establishment, emission_point, sequential, amount_minor, currency) \
        \VALUES ('invoice', 'event_ticket_order', ?, ?::uuid, 'sandbox', 'datil', '999', '999', ?, 2000, 'USD') \
        \RETURNING id::text" [text, checkout, key] :: SqlPersistT IO [Single Text]) pool
  case rows of
    [Single documentId] -> pure Document { docId = documentId, docEvent = n }
    _ -> fail "Another sandbox tax document is claimable; the worker cases need an otherwise idle queue"

checkoutId :: Int64 -> Text
checkoutId n = "e7100000-0000-4000-8000-" <> T.justifyRight 12 '0' (T.pack (show n))

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres:postgres@postgres:5432/"]

within :: IO a -> IO a
within action = timeout 30000000 action >>= maybe (fail "Invoice worker case timed out") pure
