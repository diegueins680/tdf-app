{-# LANGUAGE OverloadedStrings #-}

-- | PAY-DOMO-CANCEL-001 against the complete reviewed PostgreSQL schema:
-- cancellation records, refund obligations and the money invariants of
-- TDF.Commerce.DomoCancellationStore. Runs inside the provider-retry-runtime
-- gate (TDF_PROVIDER_RETRY_DATABASE_URL).
module TDF.Commerce.DomoCancellationRuntimeSpec (databaseSpec) where

import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, addUTCTime, getCurrentTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue (..))
import           Database.Persist.Sql (ConnectionPool, Single (..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import           Test.Hspec

import           TDF.Commerce.DomoCancellation (Canceller (..))
import           TDF.Commerce.DomoCancellationStore

data Fixture = Fixture
  { fxQuoteId    :: Text
  , fxCheckoutId :: Text
  , fxStartsAt   :: UTCTime
  }

depositMinor :: Int64
depositMinor = 200704

-- | A Domo quote priced like the migration rehearsal, starting @daysAhead@ days
-- from now, optionally with its deposit verified through a PayPal capture.
-- | A 64-hex lookup token hash unique per fixture. Repeating only the first
-- byte of the UUID (the previous construction) left 256 possible values and
-- collided across the suite's fixtures on the unique lookup_token_hash
-- columns roughly one run in six.
tokenHash :: Text -> Text
tokenHash identifier = let hex = T.filter (/= '-') identifier in hex <> hex

seedQuote :: ConnectionPool -> Int -> Bool -> IO Fixture
seedQuote pool daysAhead paid = do
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
      , PersistText (tokenHash checkoutId)
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
      \ VALUES (?::uuid, ?::uuid, 'paypal', 'sandbox', 'capture', 'succeeded', ?, 'USD', 'PAYPAL-SANDBOX', ?)"
      [PersistText attemptId, PersistText checkoutId, PersistInt64 depositMinor, PersistText ("domo-cancel-capture:" <> quoteId)]
    rawExecute
      "INSERT INTO commerce_provider_binding(id, payment_attempt_id, provider, environment, merchant_account_ref,\
      \ resource_type, provider_resource_id, provider_resource_path, merchant_reference, amount_minor, currency)\
      \ VALUES (?::uuid, ?::uuid, 'paypal', 'sandbox', 'PAYPAL-SANDBOX', 'capture', ?, ?, ?, ?, 'USD')"
      [ PersistText bindingId, PersistText attemptId, PersistText ("CAPTURE-" <> quoteId)
      , PersistText ("/v2/checkout/orders/" <> quoteId <> "/capture"), PersistText ("domo-quote:" <> quoteId)
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

databaseSpec :: SpecWith ConnectionPool
databaseSpec = describe "Domo cancellation (PAY-DOMO-CANCEL-001)" $ do
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
    openAfter `shouldBe` "0"
    resolved <- scalar pool
      "SELECT status FROM commerce_reconciliation_exception WHERE exception_type=? AND internal_reference=?"
      [PersistText refundObligationExceptionType, PersistText ("domo-quote:" <> fxQuoteId fixture)]
    resolved `shouldBe` "resolved"
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
