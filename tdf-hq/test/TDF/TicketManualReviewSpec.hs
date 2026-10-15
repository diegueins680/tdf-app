{-# LANGUAGE OverloadedStrings #-}

module TDF.TicketManualReviewSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (
  ConnectionPool,
  PersistValue (..),
  Single (..),
  SqlPersistT,
  rawExecute,
  rawSql,
  runSqlPool,
  toSqlKey,
 )
import System.Environment (lookupEnv)
import System.IO.Error (ioeGetErrorString)
import System.Timeout (timeout)
import TDF.Server.TicketManualPayments (
  ManualReviewAction (..),
  ManualReviewer (..),
  reviewTicketManualPayment,
 )
import Test.Hspec

-- Each case builds its own event, ticket order, checkout and submitted
-- transfer reference, and reviews it through the function the handler calls.
-- The review commits, so what a case created is removed afterwards by id.
spec :: Spec
spec = describe "ticket-manual-review-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_TICKET_MANUAL_REVIEW_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  made <- runIO (newIORef [])
  case configured of
    Nothing ->
      it "requires the isolated fully migrated integration runner" $
        pendingWith "Run scripts/test-ticket-manual-review-concurrency.sh"
    Just _
      | any (maybe False (not . null)) routingOverrides ->
          it "refuses inherited libpq routing overrides" $
            expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated\
              \ runner"
    Just connection
      | not (safeConnection connection) ->
          it "refuses a non-disposable database" $ expectationFailure "Use the isolated\
            \ integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 4)) $
      afterAll (\pool -> forget made pool >> destroyAllResources pool) $ do
        it "records one payment when the organizer approves a submitted transfer" $ \pool -> do
          c <- transfer made pool
          review pool c (organizer c) ManualApprove `shouldReturn` Right True
          state pool c `shouldReturn` ("approved", "succeeded", "paid", 2000)
          payments pool c `shouldReturn` 1

        it "answers a repeated approval without a second payment" $ \pool -> do
          c <- transfer made pool
          review pool c (organizer c) ManualApprove `shouldReturn` Right True
          review pool c (organizer c) ManualApprove `shouldReturn` Right True
          state pool c `shouldReturn` ("approved", "succeeded", "paid", 2000)
          payments pool c `shouldReturn` 1

        it "lets an administrator who is not the organizer approve" $ \pool -> do
          c <- transfer made pool
          review pool c (ManualReviewer (outsider c) True) ManualApprove `shouldReturn` Right True
          state pool c `shouldReturn` ("approved", "succeeded", "paid", 2000)

        it "refuses a reviewer who is neither the organizer nor an administrator" $ \pool -> do
          c <- transfer made pool
          review pool c (ManualReviewer (outsider c) False) ManualApprove
            `shouldReturn` Left "Only the event organizer or an administrator may review this\
              \ payment"
          state pool c `shouldReturn` ("submitted", "requires_review", "awaiting_payment", 0)

        it "refuses the person who submitted the reference, even as administrator" $ \pool -> do
          c <- transfer made pool
          review pool c (ManualReviewer (submitter c) True) ManualApprove
            `shouldReturn` Left "Bank transfer evidence requires an independent reviewer"
          state pool c `shouldReturn` ("submitted", "requires_review", "awaiting_payment", 0)

        it "refuses a reviewer whose email is the buyer's" $ \pool -> do
          c <- transfer made pool
          runSqlPool
            ( rawExecute
                "UPDATE party SET primary_email=? WHERE id=?"
                [PersistText (T.toUpper (buyerEmail c)), PersistInt64 (outsider c)]
            )
            pool
          review pool c (ManualReviewer (outsider c) True) ManualApprove
            `shouldReturn` Left "Staff cannot review a transfer for an order bought with their\
              \ own email"
          state pool c `shouldReturn` ("submitted", "requires_review", "awaiting_payment", 0)

        it "rejects without a payment, repeats a rejection, and does not approve a rejected\
          \ reference" $ \pool -> do
          c <- transfer made pool
          review pool c (organizer c) ManualReject `shouldReturn` Right False
          state pool c `shouldReturn` ("rejected", "failed", "failed", 0)
          review pool c (organizer c) ManualReject `shouldReturn` Right False
          review pool c (organizer c) ManualApprove
            `shouldReturn` Left "Rejected evidence must be resubmitted by the buyer before approval"
          state pool c `shouldReturn` ("rejected", "failed", "failed", 0)
          payments pool c `shouldReturn` 0

        it "does not change an approved transfer when a rejection follows" $ \pool -> do
          c <- transfer made pool
          review pool c (organizer c) ManualApprove `shouldReturn` Right True
          review pool c (organizer c) ManualReject
            `shouldReturn` Left "Approved bank transfer evidence cannot be changed"
          state pool c `shouldReturn` ("approved", "succeeded", "paid", 2000)

        it "refuses approval after the seat hold has passed and records it for\
          \ reconciliation" $ \pool -> do
          c <- transfer made pool
          now <- getCurrentTime
          reviewAt pool c (organizer c) ManualApprove (addUTCTime 3600 now)
            `shouldReturn` Left
              "The seat hold expired; the transfer requires reconciliation or refund and no\
                \ ticket was issued"
          state pool c `shouldReturn` ("submitted", "requires_review", "awaiting_payment", 0)
          exceptions pool c `shouldReturn` ["manual_payment_after_ticket_hold_expiry"]

        it "refuses a review for another event's order" $ \pool -> do
          c <- transfer made pool
          other <- transfer made pool
          now <- getCurrentTime
          outcome <-
            runSqlPool
              ( reviewTicketManualPayment
                  (organizer other)
                  (toSqlKey (eventId other))
                  (toSqlKey (orderId c))
                  ManualApprove
                  "Deposito verificado"
                  now
                  (pure ())
              )
              pool
          outcome `shouldBe` Left "No bank transfer evidence exists for this ticket order"
          state pool c `shouldReturn` ("submitted", "requires_review", "awaiting_payment", 0)

        it "answers both reviewers when approval and rejection overlap" $ \pool -> do
          forM_ [1 :: Int .. 4] $ \_ -> do
            c <- transfer made pool
            (approval, rejection) <-
              overlapping
                pool
                c
                (review pool c (organizer c) ManualApprove)
                (review pool c (ManualReviewer (outsider c) True) ManualReject)
            settled <- state pool c
            (fmap decided approval, fmap decided rejection, settled)
              `shouldSatisfy` ( `elem`
                  [ (Right (Just True), Right Nothing, ("approved", "succeeded", "paid", 2000))
                  , (Right Nothing, Right (Just False), ("rejected", "failed", "failed", 0))
                  ]
                )

        it "answers both reviewers with success when two approvals overlap" $ \pool -> do
          forM_ [1 :: Int .. 4] $ \_ -> do
            c <- transfer made pool
            (first, second) <-
              overlapping
                pool
                c
                (review pool c (organizer c) ManualApprove)
                (review pool c (ManualReviewer (outsider c) True) ManualApprove)
            (fmap decided first, fmap decided second)
              `shouldBe` (Right (Just True), Right (Just True))
            state pool c `shouldReturn` ("approved", "succeeded", "paid", 2000)
            payments pool c `shouldReturn` 1

        it "releases the holder before cancelling reviewers after an observation failure" $
          \pool -> do
            c <- transfer made pool
            let approve = review pool c (organizer c) ManualApprove
            overlappingWithObservation (fail "Synthetic observation failure") pool c approve approve
              `shouldThrow` ((== "Synthetic observation failure") . ioeGetErrorString)
            review pool c (organizer c) ManualApprove `shouldReturn` Right True
            payments pool c `shouldReturn` 1

-- A decision (Just) or a refusal (Nothing).
decided :: Either Text Bool -> Maybe Bool
decided = either (const Nothing) Just

-- Runs two reviews so that they overlap: the checkout row is held until both
-- are waiting for it, then released. A database error in either is kept.
withinDeadline :: IO a -> IO a
withinDeadline action = do
  result <- timeout 30000000 action
  maybe (fail "Ticket manual review overlap timed out waiting for both transactions") pure result

overlapping :: ConnectionPool -> Case -> IO a -> IO b -> IO (Either String a, Either String b)
overlapping = overlappingWithObservation (pure ())

overlappingWithObservation ::
  IO () -> ConnectionPool -> Case -> IO a -> IO b -> IO (Either String a, Either String b)
overlappingWithObservation observed pool c one' two = withinDeadline $ do
  held <- newEmptyMVar
  release <- newEmptyMVar
  withAsync
    ( runSqlPool
        ( do
            _ <-
              rawSql
                "SELECT status FROM commerce_checkout_session WHERE id=?::uuid FOR UPDATE"
                [PersistText (checkout c)] ::
                SqlPersistT IO [Single Text]
            liftIO (putMVar held () >> takeMVar release)
        )
        pool
    )
    $ \holder -> do
      takeMVar held
      -- A PostgreSQL query can defer cancellation while waiting on the holder.
      -- Release that lock before waiting for either reviewer to acknowledge cancellation.
      let withReviewer action = bracket (async (attempt action)) $ \worker -> do
            void (tryPutMVar release ())
            cancel worker
      outcome <- withReviewer one' $ \first -> withReviewer two $ \second -> do
        bothWaiting
        observed
        putMVar release ()
        (,) <$> wait first <*> wait second
      wait holder
      pure outcome
 where
  attempt :: IO x -> IO (Either String x)
  attempt action = either (\e -> Left (show (e :: SomeException))) Right <$> try action
  bothWaiting = do
    n <-
      one
        pool
        "SELECT count(*) FROM pg_stat_activity WHERE datname = current_database()\
        \ AND wait_event_type = 'Lock' AND query LIKE 'SELECT checkout.id::text, checkout.status%'"
        []
    if n >= 2 then pure () else threadDelay 20000 >> bothWaiting

data Case = Case
  { eventId :: Int64
  , orderId :: Int64
  , checkout :: Text
  , organizer :: ManualReviewer
  , submitter :: Int64
  , outsider :: Int64
  , buyerEmail :: Text
  }

safeConnection :: String -> Bool
safeConnection value =
  not (any (`elem` value) ['?', '#'])
    && "_test" `isSuffixOf` value
    && any
      (`isPrefixOf` value)
      [ "postgresql://127.0.0.1/"
      , "postgresql://127.0.0.1:"
      , "postgresql://localhost/"
      , "postgresql://localhost:"
      , "postgresql://postgres:postgres@postgres:5432/"
      ]

review :: ConnectionPool -> Case -> ManualReviewer -> ManualReviewAction -> IO (Either Text Bool)
review pool c reviewer action = getCurrentTime >>= reviewAt pool c reviewer action

reviewAt ::
  ConnectionPool -> Case -> ManualReviewer -> ManualReviewAction -> UTCTime -> IO (Either Text Bool)
reviewAt pool c reviewer action now =
  runSqlPool
    ( reviewTicketManualPayment
        reviewer
        (toSqlKey (eventId c))
        (toSqlKey (orderId c))
        action
        "Deposito verificado"
        now
        (pure ())
    )
    pool

-- Evidence, attempt and checkout status, and the amount recorded as paid.
state :: ConnectionPool -> Case -> IO (Text, Text, Text, Int64)
state pool c = do
  rows <-
    runSqlPool
      ( rawSql
          "SELECT evidence.status, attempt.status, session.status, session.paid_minor\
          \ FROM commerce_checkout_session session\
          \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id = session.id\
          \ JOIN commerce_manual_payment_evidence evidence ON evidence.payment_attempt_id =\
            \ attempt.id\
          \ WHERE session.id = ?::uuid"
          [PersistText (checkout c)]
      )
      pool
  case rows of
    [(Single evidence, Single attempt, Single session, Single paid)] ->
      pure (evidence, attempt, session, paid)
    _ -> fail "the case has no single checkout"

-- Payment captures posted to the ledger for the checkout's attempts.
payments :: ConnectionPool -> Case -> IO Int
payments pool c =
  one
    pool
    "SELECT count(*) FROM commerce_ledger_transaction posted\
    \ JOIN commerce_payment_attempt attempt ON attempt.id::text = posted.source_id\
    \ WHERE posted.source_type = 'payment_attempt' AND posted.transaction_type = 'payment_capture'\
    \ AND attempt.checkout_id = ?::uuid"
    [PersistText (checkout c)]

exceptions :: ConnectionPool -> Case -> IO [Text]
exceptions pool c = do
  rows <-
    runSqlPool
      ( rawSql
          "SELECT exception_type FROM commerce_reconciliation_exception WHERE provider =\
            \ 'bank_transfer' AND internal_reference = ?"
          [PersistText (T.pack (show (orderId c)))]
      )
      pool
  pure (sort [code | Single code <- rows])

one :: ConnectionPool -> Text -> [PersistValue] -> IO Int
one pool sql args = do
  rows <- runSqlPool (rawSql sql args) pool
  case rows of
    [Single n] -> pure n
    _ -> fail "the count returned no row"

key :: Text -> [PersistValue] -> SqlPersistT IO Int64
key sql args = do
  rows <- rawSql sql args
  case rows of
    [Single k] -> pure k
    _ -> error "no key returned"

text :: Text -> [PersistValue] -> SqlPersistT IO Text
text sql args = do
  rows <- rawSql sql args
  case rows of
    [Single k] -> pure k
    _ -> error "no value returned"

-- An event with an approved bank transfer policy, one ticket order with a
-- ten-minute hold, its checkout and attempt, and a reference the buyer submitted.
transfer :: IORef [Case] -> ConnectionPool -> IO Case
transfer made pool = do
  c <- runSqlPool build pool
  modifyIORef' made (c :)
  pure c
 where
  person =
    key
      "INSERT INTO party (display_name, is_org, created_at) VALUES ('manual-review-spec',\
        \ false, now()) RETURNING id"
      []
  build = do
    organizerId <- person
    submitterId <- person
    outsiderId <- person
    event <-
      key
        "INSERT INTO social_event (title, start_time, end_time, organizer_party_id,\
          \ event_type_id, workflow_state_id)\
        \ SELECT 'manual-review-spec', now() + interval '30 days', now() + interval '31 days',\
          \ ?, id,\
        \ '00000000-0000-4000-8000-000000000232' FROM event_type WHERE code = 'concert'\
          \ RETURNING id"
        [PersistText (T.pack (show organizerId))]
    tier <-
      key
        "INSERT INTO event_ticket_tier (event_id, code, name, price_cents, currency,\
          \ quantity_total, quantity_sold, is_active)\
        \ VALUES (?, 'general', 'General', 2000, 'USD', 20, 1, true) RETURNING id"
        [PersistInt64 event]
    policy <-
      text
        "INSERT INTO event_ticket_checkout_policy (event_id, policy_version, currency,\
          \ buyer_fee_bps, organizer_fee_bps,\
        \ tax_bps, hold_minutes, terms_version, terms_summary, refund_policy,\
          \ max_tickets_per_order,\
        \ manual_transfer_hold_minutes, manual_transfer_cutoff_at)\
        \ VALUES (?, 'manual-v1', 'USD', 0, 0, 0, 10, 'manual-terms-v1', 'Terms.', 'Refunds.',\
          \ 4, 1440, now() + interval '2 days')\
        \ RETURNING id::text"
        [PersistInt64 event]
    rawExecute
      "UPDATE event_ticket_checkout_policy SET approval_status='approved', active=true,\
        \ approved_at=now(),\
      \ approved_by='manual-review-spec' WHERE id=?::uuid"
      [PersistText policy]
    order <-
      key
        "INSERT INTO event_ticket_order (event_id, tier_id, buyer_name, buyer_email, quantity,\
          \ amount_cents, currency,\
        \ status, original_amount_cents, payment_method, purchased_at)\
        \ VALUES (?, ?, 'Transfer buyer', 'pending@example.invalid', 1, 2000, 'USD', 'pending',\
          \ 2000, 'bank_transfer', now())\
        \ RETURNING id"
        [PersistInt64 event, PersistInt64 tier]
    let email = "buyer" <> T.pack (show order) <> "@example.invalid"
        orderText = T.pack (show order)
    rawExecute
      "UPDATE event_ticket_order SET buyer_email=? WHERE id=?"
      [PersistText email, PersistInt64 order]
    session <-
      text
        "INSERT INTO commerce_checkout_session (domain_type, domain_order_id, status,\
          \ environment, currency,\
        \ subtotal_minor, tax_minor, total_minor, customer_email, lookup_token_hash,\
          \ idempotency_key, expires_at)\
        \ VALUES ('event_ticket_order', ?, 'awaiting_payment', 'sandbox', 'USD', 2000, 0, 2000, ?,\
        \ md5('lookup' || ?) || md5('lookup2' || ?), 'manual-review-spec-checkout-' || ?, now()\
          \ + interval '10 minutes')\
        \ RETURNING id::text"
        [ PersistText orderText
        , PersistText email
        , PersistText orderText
        , PersistText orderText
        , PersistText orderText
        ]
    rawExecute
      "INSERT INTO commerce_checkout_line_item (checkout_id, line_number, product_type,\
        \ product_id, product_version,\
      \ description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot)\
      \ VALUES (?::uuid, 1, 'event_ticket', ?, 'manual-v1', 'General', 1, 2000, 2000, 2000,\
        \ '{}'::jsonb)"
      [PersistText session, PersistText (T.pack (show tier))]
    rawExecute
      "INSERT INTO event_ticket_checkout_runtime (order_id, event_id, tier_id, checkout_id,\
        \ policy_id, policy_version,\
      \ lookup_token_hash, create_idempotency_key, create_request_sha256, quantity, currency,\
        \ unit_price_minor,\
      \ gross_face_value_minor, discount_minor, net_face_value_minor, buyer_fee_bps,\
        \ buyer_fee_minor,\
      \ organizer_fee_bps, organizer_fee_minor, tax_bps, tax_minor, checkout_total_minor,\
        \ organizer_payable_minor,\
      \ platform_fee_minor, terms_version, terms_accepted_at, hold_expires_at)\
      \ VALUES (?, ?, ?, ?::uuid, ?::uuid, 'manual-v1', md5('runtime' || ?) || md5('runtime2'\
        \ || ?),\
      \ 'manual-review-spec-runtime-' || ?, md5('request' || ?) || md5('request2' || ?), 1,\
        \ 'USD', 2000, 2000, 0, 2000,\
      \ 0, 0, 0, 0, 0, 0, 2000, 2000, 0, 'manual-terms-v1', now(), now() + interval '10 minutes')"
      [ PersistInt64 order
      , PersistInt64 event
      , PersistInt64 tier
      , PersistText session
      , PersistText policy
      , PersistText orderText
      , PersistText orderText
      , PersistText orderText
      , PersistText orderText
      , PersistText orderText
      ]
    attempt <-
      text
        "INSERT INTO commerce_payment_attempt (checkout_id, provider, environment, operation,\
          \ status, amount_minor,\
        \ currency, merchant_account_ref, idempotency_key)\
        \ VALUES (?::uuid, 'bank_transfer', 'sandbox', 'manual_verify', 'requires_review',\
          \ 2000, 'USD',\
        \ 'tdf-manual-settlement', 'manual-review-spec-attempt-' || ?) RETURNING id::text"
        [PersistText session, PersistText orderText]
    rawExecute
      "INSERT INTO commerce_manual_payment_evidence (checkout_id, payment_attempt_id, status)\
      \ VALUES (?::uuid, ?::uuid, 'awaiting_evidence')"
      [PersistText session, PersistText attempt]
    rawExecute
      "UPDATE commerce_manual_payment_evidence SET status='submitted',\
        \ customer_reference='BANK-REF',\
      \ submitted_amount_minor=2000, currency='USD', submitted_at=now(), submitted_by=?\
      \ WHERE payment_attempt_id=?::uuid"
      [PersistInt64 submitterId, PersistText attempt]
    pure
      Case
        { eventId = event
        , orderId = order
        , checkout = session
        , organizer = ManualReviewer organizerId False
        , submitter = submitterId
        , outsider = outsiderId
        , buyerEmail = email
        }

-- Removes what the cases created, including what the schema derived from it
-- (ledger postings, receipts, Operations events, the directory entry).
-- Commerce records are immutable by trigger, so the removal runs with
-- triggers off, in its own transaction.
forget :: IORef [Case] -> ConnectionPool -> IO ()
forget made pool = do
  cases <- readIORef made
  runSqlPool
    ( do
        rawExecute "SET LOCAL session_replication_role = replica" []
        byAttempt <- tablesWith "payment_attempt_id" "commerce_manual_payment_evidence"
        byCheckout <- tablesWith "checkout_id" "commerce_payment_attempt"
        forM_ cases $ \c -> do
          let session = [PersistText (checkout c)]
              order = [PersistInt64 (orderId c)]
              event = [PersistInt64 (eventId c)]
              eventText = [PersistText (T.pack (show (eventId c)))]
              attempts = "(SELECT id FROM commerce_payment_attempt WHERE checkout_id=?::uuid)"
          rawExecute
            ( "DELETE FROM commerce_ledger_entry WHERE transaction_id IN (SELECT id FROM\
              \ commerce_ledger_transaction\
              \ WHERE source_type='payment_attempt' AND source_id IN (SELECT id::text FROM\
                \ commerce_payment_attempt WHERE checkout_id=?::uuid))"
            )
            session
          rawExecute
            "DELETE FROM commerce_ledger_transaction WHERE source_type='payment_attempt'\
            \ AND source_id IN (SELECT id::text FROM commerce_payment_attempt WHERE\
              \ checkout_id=?::uuid)"
            session
          forM_ byAttempt $ \t -> rawExecute ("DELETE FROM " <> t <> " WHERE payment_attempt_id\
            \ IN " <> attempts) session
          forM_ byCheckout $ \t -> rawExecute ("DELETE FROM " <> t <> " WHERE\
            \ checkout_id=?::uuid") session
          rawExecute
            "DELETE FROM commerce_reconciliation_exception WHERE provider='bank_transfer' AND\
              \ internal_reference=?"
            [PersistText (T.pack (show (orderId c)))]
          rawExecute "DELETE FROM commerce_payment_attempt WHERE checkout_id=?::uuid" session
          rawExecute "DELETE FROM commerce_checkout_session WHERE id=?::uuid" session
          rawExecute "DELETE FROM event_ticket_order WHERE id=?" order
          rawExecute "DELETE FROM event_ticket_checkout_policy_history WHERE event_id=?" event
          rawExecute "DELETE FROM event_ticket_checkout_policy WHERE event_id=?" event
          rawExecute "DELETE FROM event_ticket_tier WHERE event_id=?" event
          rawExecute
            "DELETE FROM operations_work_item_event WHERE domain_event_id IN (SELECT id FROM\
              \ operations_domain_event\
            \ WHERE aggregate_type='social_event' AND aggregate_id=?)"
            eventText
          rawExecute
            "DELETE FROM operations_outbox WHERE aggregate_type='social_event' AND aggregate_id=?"
            eventText
          rawExecute
            "DELETE FROM operations_domain_event WHERE aggregate_type='social_event' AND\
              \ aggregate_id=?"
            eventText
          rawExecute
            "DELETE FROM operations_aggregate_sequence WHERE aggregate_type='social_event' AND\
              \ aggregate_id=?"
            eventText
          rawExecute
            "DELETE FROM directory_search_document WHERE entity_kind='event' AND entity_id=?"
            eventText
          rawExecute "DELETE FROM social_event WHERE id=?" event
          forM_ [mrPartyId (organizer c), submitter c, outsider c] $ \p ->
            rawExecute "DELETE FROM party WHERE id=?" [PersistInt64 p]
    )
    pool
 where
  tablesWith :: Text -> Text -> SqlPersistT IO [Text]
  tablesWith column except = do
    rows <-
      rawSql
        "SELECT c.table_name::text FROM information_schema.columns c\
        \ JOIN information_schema.tables t ON t.table_schema = c.table_schema AND t.table_name\
          \ = c.table_name\
        \ WHERE c.table_schema='public' AND t.table_type='BASE TABLE' AND c.column_name=? AND\
          \ c.data_type='uuid'\
        \ AND c.table_name <> ?"
        [PersistText column, PersistText except]
    pure [t | Single t <- rows]
