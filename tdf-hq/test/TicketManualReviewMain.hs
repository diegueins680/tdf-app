{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- EVT-TICKET-MANUAL-001: the staff review transaction of a bank-transfer ticket
-- order against the production schema snapshot plus the production migration
-- batch. Run only against the disposable tdf_ticket_manual_review_test
-- database prepared by scripts/test-ticket-manual-review.sh.
module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql
import System.Environment (getEnv, lookupEnv)
import TDF.DisposableTicketDatabase (safeManualReviewDatabase)
import Test.Hspec

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentRuntimeStore as Runtime
import TDF.Server.TicketManualPayments

organizer, admin, buyerEmailAdmin, receptionist :: ManualReviewer
organizer = ManualReviewer 7101 False
admin = ManualReviewer 7102 True
buyerEmailAdmin = ManualReviewer 7103 True
receptionist = ManualReviewer 7104 False

-- | One order, prepared the way the public handlers do it: canonical checkout
-- and runtime snapshot, bank transfer selected through the payment runtime, a
-- new guest submitter party, the extended hold and the reported reference.
data Order = Order
  { oId :: Int64
  , oCheckout :: Text
  , oAttempt :: Text
  , oSubmitter :: Int64
  }

newOrder :: ConnectionPool -> IO Order
newOrder pool = do
  now <- getCurrentTime
  -- 64 hex characters, the shape of a SHA-256 lookup token digest.
  halves <- mapM (const (toText <$> nextRandom)) [1, 2 :: Int]
  let lookupHash = T.filter (/= '-') (T.concat halves)
  flip runSqlPool pool $ do
    [Single orderId] <- rawSql
      "INSERT INTO event_ticket_order(event_id, tier_id, buyer_name, buyer_email, quantity,\
      \ amount_cents, currency, status, purchased_at, original_amount_cents, payment_method)\
      \ VALUES (7001, 7001, 'Comprador', 'comprador@example.invalid', 1, 2000, 'USD',\
      \ 'pending', NOW(), 2000, 'bank_transfer') RETURNING id" []
    let ref = T.pack (show (orderId :: Int64))
    rawExecute "UPDATE event_ticket_tier SET quantity_sold = quantity_sold + 1 WHERE id = 7001" []
    -- The same canonical writer as the public checkout: header and line snapshot.
    checkout <- Checkout.createCheckout Checkout.CheckoutCreation
      { Checkout.ccDomainType = "event_ticket_order"
      , Checkout.ccDomainOrderId = ref
      , Checkout.ccEnvironment = Checkout.CheckoutSandbox
      , Checkout.ccCurrency = "USD"
      , Checkout.ccAmountMinor = 2000
      , Checkout.ccCustomerEmail = "comprador@example.invalid"
      , Checkout.ccLookupTokenHash = lookupHash
      , Checkout.ccIdempotencyKey = "manual-review-checkout-" <> ref
      , Checkout.ccExpiresAt = addUTCTime 600 now
      , Checkout.ccProductType = "event_ticket_tier"
      , Checkout.ccProductId = "7001"
      , Checkout.ccProductVersion = "manual-review-v1"
      , Checkout.ccDescription = "General"
      , Checkout.ccSnapshot = A.object []
      , Checkout.ccCorrelationId = "event-ticket-create:" <> ref
      }
    let checkoutId = Checkout.checkoutReferenceId checkout
    rawExecute
      "INSERT INTO event_ticket_checkout_runtime(order_id, event_id, tier_id, checkout_id,\
      \ policy_id, policy_version, lookup_token_hash, create_idempotency_key,\
      \ create_request_sha256, quantity, currency, unit_price_minor, gross_face_value_minor,\
      \ discount_minor, net_face_value_minor, buyer_fee_bps, buyer_fee_minor,\
      \ organizer_fee_bps, organizer_fee_minor, tax_bps, tax_minor, checkout_total_minor,\
      \ organizer_payable_minor, platform_fee_minor, terms_version, terms_accepted_at,\
      \ hold_expires_at)\
      \ SELECT ?, 7001, 7001, ?::uuid, policy.id, policy.policy_version,\
      \ md5('runtime' || ?) || md5('r2' || ?), 'manual-review-runtime-' || ?,\
      \ md5('request' || ?) || md5('q2' || ?), 1, 'USD', 2000, 2000, 0, 2000, 0, 0, 0, 0,\
      \ 0, 0, 2000, 2000, 0, policy.terms_version, NOW(), NOW() + INTERVAL '10 minutes'\
      \ FROM event_ticket_checkout_policy policy WHERE policy.event_id = 7001"
      [ PersistInt64 orderId, PersistText checkoutId, PersistText ref, PersistText ref
      , PersistText ref, PersistText ref, PersistText ref ]
    attempt <- Runtime.beginPaymentAttempt Checkout.PaymentAttemptCreation
      { Checkout.pacCheckout = checkout
      , Checkout.pacProvider = Checkout.ProviderBankTransfer
      , Checkout.pacEnvironment = Checkout.CheckoutSandbox
      , Checkout.pacOperation = Checkout.OperationManualVerify
      , Checkout.pacAmountMinor = 2000
      , Checkout.pacCurrency = "USD"
      , Checkout.pacMerchantRef = "tdf-manual-settlement"
      , Checkout.pacIdempotencyKey = "manual-review-attempt-" <> ref
      , Checkout.pacCreatedAt = now
      , Checkout.pacCorrelationId = "event-ticket:" <> ref <> ":bank_transfer:manual-select"
      } >>= either (liftIO . fail . T.unpack) pure
    Checkout.recordManualPaymentSelection checkout attempt Checkout.ProviderBankTransfer
      ("event-ticket:" <> ref <> ":bank_transfer:manual-select") now
    [Single guest] <- rawSql
      "INSERT INTO party(display_name, is_org, primary_email, notes, created_at)\
      \ VALUES ('Comprador', FALSE, 'comprador@example.invalid',\
      \ 'Unverified guest ticket buyer', NOW()) RETURNING id" []
    rawExecute
      "UPDATE event_ticket_checkout_runtime SET manual_submitter_party_id = ?,\
      \ manual_hold_expires_at = NOW() + INTERVAL '1 hour' WHERE order_id = ?"
      [PersistInt64 guest, PersistInt64 orderId]
    let order = Order orderId checkoutId (Checkout.paymentAttemptReferenceId attempt) guest
    submitEvidence order
    pure order

-- Mirrors submitPublicEventTicketBankTransferEvidence for a first or repeated report.
submitEvidence :: Order -> SqlPersistT IO ()
submitEvidence order = rawExecute
  "UPDATE commerce_manual_payment_evidence SET customer_reference = ?,\
  \ submitted_amount_minor = 2000, currency = 'USD', submitted_at = NOW(), submitted_by = ?,\
  \ status = 'submitted', reviewed_by = NULL, reviewed_at = NULL, review_notes = NULL\
  \ WHERE payment_attempt_id = ?::uuid"
  [ PersistText ("BANCO-" <> T.pack (show (oId order)))
  , PersistInt64 (oSubmitter order), PersistText (oAttempt order) ]

review :: ConnectionPool -> ManualReviewer -> Order -> ManualReviewAction -> UTCTime
       -> IO (Either Text Bool)
review pool reviewer order action now = runSqlPool
  (reviewTicketManualPayment reviewer (toSqlKey 7001) (toSqlKey (oId order)) action
    "Revisado contra el estado de cuenta" now) pool

-- | Authoritative rows: evidence status/reviewer, checkout status and paid
-- amount, attempt and intent status, bindings, approval audits, exceptions.
data Snapshot = Snapshot
  { evidenceStatus :: Text
  , reviewedBy :: Maybe Int64
  , checkoutStatus :: Text
  , paidMinor :: Int64
  , attemptStatus :: Text
  , intentStatus :: Text
  , bindings :: Int64
  , approvals :: Int64
  , exceptions :: Int64
  } deriving (Eq, Show)

snapshot :: ConnectionPool -> Order -> IO Snapshot
snapshot pool order = do
  rows <- runSqlPool (rawSql
    "SELECT evidence.status, evidence.reviewed_by, checkout.status, checkout.paid_minor,\
    \ attempt.status, intent.status,\
    \ (SELECT count(*) FROM commerce_provider_binding binding\
    \   WHERE binding.payment_attempt_id = attempt.id),\
    \ (SELECT count(*) FROM commerce_checkout_audit_event audit\
    \   WHERE audit.checkout_id = checkout.id AND audit.event_type = 'manual_payment_approved'),\
    \ (SELECT count(*) FROM commerce_reconciliation_exception exception\
    \   WHERE exception.provider = 'bank_transfer' AND exception.internal_reference = ?)\
    \ FROM commerce_manual_payment_evidence evidence\
    \ JOIN commerce_payment_attempt attempt ON attempt.id = evidence.payment_attempt_id\
    \ JOIN commerce_payment_intent intent ON intent.id = attempt.payment_intent_id\
    \ JOIN commerce_checkout_session checkout ON checkout.id = evidence.checkout_id\
    \ WHERE evidence.payment_attempt_id = ?::uuid"
    [PersistText (T.pack (show (oId order))), PersistText (oAttempt order)]) pool
  case rows of
    [(Single a, Single b, Single c, Single d, Single e, Single f, Single g, Single h, Single i)] ->
      pure (Snapshot a b c d e f g h i)
    _ -> fail "Expected exactly one bank transfer evidence row"

-- Approval outcomes of one order, in request order, without the Left texts.
chunksOf3 :: [Either Text Bool] -> [[Either Text Bool]]
chunksOf3 xs = case splitAt 3 xs of
  ([], _) -> []
  (chunk, rest) -> filter isRight chunk : chunksOf3 rest

shouldBeLeftWith :: Either Text Bool -> Text -> Expectation
shouldBeLeftWith result fragment = case result of
  Left message | fragment `T.isInfixOf` message -> pure ()
  other -> expectationFailure ("expected Left containing " <> show fragment <> ", got " <> show other)

main :: IO ()
main = do
  dsn <- getEnv "TICKET_MANUAL_REVIEW_TEST_DSN"
  ci <- (== Just "true") <$> lookupEnv "CI"
  overrides <- traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"]
  unless (safeManualReviewDatabase ci overrides dsn) $
    fail "Refusing non-local manual review fixture routing or libpq overrides"
  runNoLoggingT $ withPostgresqlPool (BS.pack dsn) 8 $ \pool -> liftIO $ do
    names <- runSqlPool (rawSql "SELECT current_database()" []) pool
    unless (names == [Single ("tdf_ticket_manual_review_test" :: Text)]) $
      fail "Refusing fixtures outside tdf_ticket_manual_review_test"
    hspec $ describe "EVT-TICKET-MANUAL-001 staff review (PostgreSQL)" $ do
      it "denies reviewers without authority or independence and leaves every row unchanged" $ do
        order <- newOrder pool
        initial <- snapshot pool order
        now <- getCurrentTime
        review pool receptionist order ManualApprove now
          >>= (`shouldBeLeftWith` "Only the event organizer or an administrator")
        review pool buyerEmailAdmin order ManualApprove now
          >>= (`shouldBeLeftWith` "their own email")
        review pool (ManualReviewer (oSubmitter order) True) order ManualApprove now
          >>= (`shouldBeLeftWith` "independent reviewer")
        review pool receptionist order ManualReject now
          >>= (`shouldBeLeftWith` "Only the event organizer or an administrator")
        snapshot pool order `shouldReturn` initial
        evidenceStatus initial `shouldBe` "submitted"
        checkoutStatus initial `shouldBe` "awaiting_payment"

      it "settles once on organizer approval and replays without another payment" $ do
        order <- newOrder pool
        now <- getCurrentTime
        review pool organizer order ManualApprove now `shouldReturn` Right True
        paid <- snapshot pool order
        paid `shouldBe` Snapshot "approved" (Just 7101) "paid" 2000 "succeeded" "captured" 1 1 0
        review pool admin order ManualApprove now `shouldReturn` Right True
        review pool organizer order ManualReject now
          >>= (`shouldBeLeftWith` "cannot be changed")
        snapshot pool order `shouldReturn` paid

      it "refuses approval after the effective hold and records a reconciliation exception" $ do
        order <- newOrder pool
        now <- getCurrentTime
        review pool admin order ManualApprove (addUTCTime 7200 now)
          >>= (`shouldBeLeftWith` "seat hold expired")
        refused <- snapshot pool order
        (evidenceStatus refused, checkoutStatus refused, paidMinor refused, exceptions refused)
          `shouldBe` ("submitted", "awaiting_payment", 0, 1)

      it "accepts approval after the original hold while the transfer extension is alive" $ do
        order <- newOrder pool
        now <- getCurrentTime
        review pool organizer order ManualApprove (addUTCTime 1800 now) `shouldReturn` Right True
        checkoutStatus <$> snapshot pool order `shouldReturn` "paid"

      it "requires a new report after rejection and then settles the same attempt" $ do
        order <- newOrder pool
        now <- getCurrentTime
        review pool organizer order ManualReject now `shouldReturn` Right False
        rejected <- snapshot pool order
        (evidenceStatus rejected, checkoutStatus rejected, attemptStatus rejected, paidMinor rejected)
          `shouldBe` ("rejected", "failed", "failed", 0)
        review pool organizer order ManualApprove now
          >>= (`shouldBeLeftWith` "resubmitted by the buyer")
        flip runSqlPool pool $ do
          let ref = T.pack (show (oId order))
          Checkout.recordManualPaymentSelection (Checkout.CheckoutReference (oCheckout order))
            (Checkout.PaymentAttemptReference (oAttempt order)) Checkout.ProviderBankTransfer
            ("event-ticket:" <> ref <> ":bank_transfer:manual-resubmit") now
          submitEvidence order
        review pool admin order ManualApprove now `shouldReturn` Right True
        settled <- snapshot pool order
        (evidenceStatus settled, checkoutStatus settled, paidMinor settled, intentStatus settled)
          `shouldBe` ("approved", "paid", 2000, "captured")

      -- Regression: the review once committed after its locked reads, so a
      -- concurrent reviewer of the same order failed with a deadlock or an
      -- invalid evidence transition (HTTP 500) instead of a decided answer.
      it "decides concurrent reviews of one order without database errors" $ do
        orders <- mapM (const (newOrder pool)) [1 .. 6 :: Int]
        now <- getCurrentTime
        let contenders = [(organizer, ManualApprove), (admin, ManualApprove), (admin, ManualReject)]
        results <- mapConcurrently
          (\(order, (reviewer, action)) -> try (review pool reviewer order action now))
          [ (order, contender) | order <- orders, contender <- contenders ]
        [show failure | Left (failure :: SomeException) <- results] `shouldBe` []
        let outcomes = [outcome | Right outcome <- results]
        mapM_ (\(order, decided) -> do
          final <- snapshot pool order
          let settled = evidenceStatus final == "approved" && checkoutStatus final == "paid"
                && paidMinor final == 2000 && approvals final == 1 && bindings final == 1
                && decided == [Right True, Right True]
              declined = evidenceStatus final == "rejected" && checkoutStatus final == "failed"
                && paidMinor final == 0 && approvals final == 0
                && length [() | Right False <- decided] == 1
          (settled || declined) `shouldBe` True)
          (zip orders (chunksOf3 outcomes))

      it "rejects only bank-transfer evidence it can bind unambiguously to the order" $ do
        now <- getCurrentTime
        missing <- runSqlPool (reviewTicketManualPayment admin (toSqlKey 7001)
          (toSqlKey 999999999) ManualApprove "Revisado contra el estado de cuenta" now) pool
        missing `shouldBeLeftWith` "No bank transfer evidence"
        order <- newOrder pool
        wrongEvent <- runSqlPool (reviewTicketManualPayment admin (toSqlKey 7999)
          (toSqlKey (oId order)) ManualApprove "Revisado contra el estado de cuenta" now) pool
        wrongEvent `shouldBeLeftWith` "No bank transfer evidence"
        isRight wrongEvent `shouldBe` False
