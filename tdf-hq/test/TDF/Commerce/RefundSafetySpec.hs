{-# LANGUAGE OverloadedStrings #-}

-- Synthetic money/property and real PostgreSQL tests; no provider HTTP.
module TDF.Commerce.RefundSafetySpec (spec, databaseSpec) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO, try)
import Control.Monad (forM, forM_)
import Data.Either (isLeft, rights)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (addUTCTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import Database.PostgreSQL.Simple (SqlError)
import Test.Hspec
import qualified Test.QuickCheck as QC

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.RefundStore as Refund
import qualified TDF.Commerce.StateMachine as State

spec :: Spec
spec = describe "refund-safety money invariants" $ do
  it "rejects an overflowing sum of already committed balances" $
    Refund.validateRefundAmount 1 maxBound maxBound 1 "USD" "USD"
      `shouldSatisfy` isLeft

  it "accepts exact remaining balances at the Int64 boundary" $ do
    Refund.validateRefundAmount maxBound 0 0 maxBound "USD" "USD" `shouldBe` Right ()
    Refund.validateRefundAmount maxBound (maxBound - 1) 0 1 "USD" "USD"
      `shouldBe` Right ()
    Refund.validateRefundAmount maxBound 0 maxBound 1 "USD" "USD"
      `shouldSatisfy` isLeft

  it "agrees with unbounded integer arithmetic across the whole Int64 range" $
    QC.forAll (QC.vectorOf 4 (QC.choose (minBound, maxBound) :: QC.Gen Int64)) $ \values ->
      case values of
        [paid, refunded, reserved, requested] ->
          let expected = paid > 0 && refunded >= 0 && reserved >= 0 && requested > 0
                && toInteger refunded + toInteger reserved + toInteger requested <= toInteger paid
          in (Refund.validateRefundAmount paid refunded reserved requested "USD" "usd" == Right ())
               QC.=== expected
        _ -> QC.property False

  it "rejects overflow in the canonical refund transition" $
    State.transitionPayment
      (State.PaymentLifecycle State.PaymentPartiallyRefunded maxBound maxBound maxBound 1)
      (State.PaymentRefundVerified maxBound) `shouldSatisfy` isLeft

  it "matches the Integer refund oracle for every generated Int64 balance" $
    QC.forAll (QC.choose (1, maxBound) :: QC.Gen Int64) $ \captured ->
      QC.forAll (QC.choose (0, captured - 1)) $ \refunded ->
        QC.forAll (QC.choose (1, maxBound) :: QC.Gen Int64) $ \requested ->
          let start = if refunded == 0 then State.PaymentCaptured else State.PaymentPartiallyRefunded
              lifecycle = State.PaymentLifecycle start captured captured captured refunded
              total = toInteger refunded + toInteger requested
              actual = State.transitionPayment lifecycle (State.PaymentRefundVerified requested)
          in if total > toInteger captured
               then QC.property (isLeft actual)
               else actual QC.=== Right lifecycle
                 { State.paymentState = if total == toInteger captured
                     then State.PaymentRefunded else State.PaymentPartiallyRefunded
                 , State.paymentRefundedMinor = fromInteger total
                 }

databaseSpec
  :: (ConnectionPool -> Checkout.PaymentProvider -> IO Checkout.VerifiedPayment)
  -> SpecWith ConnectionPool
databaseSpec captureFixture = describe "refund-safety execution and reservation" $ do
  it "grants one durable claim, including retries long after provider retention" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    (_, first) <- claim pool record >>= requireRight
    first `shouldBe` True
    forM_ [0, 86400 * 366] $ \delay -> do
      result <- runSqlPool (Refund.approveRefundForProcessing (Refund.rrReference record) 2
        (addUTCTime delay (Refund.rcCreatedAt creation))) pool >>= requireRight
      snd result `shouldBe` False
      Refund.rrStatus (fst result) `shouldBe` "processing"

  it "serializes concurrent approvers without a second provider execution permit" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    results <- concurrent (replicate 8 (claim pool record))
    length (rights results) `shouldBe` 8
    length (filter snd (rights results)) `shouldBe` 1

  it "keeps two-person approval and permits a separately approved request once" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    runSqlPool (Refund.approveRefundForProcessing (Refund.rrReference record) 1
      (Refund.rcCreatedAt creation)) pool >>= (`shouldSatisfy` isLeft)
    approved <- runSqlPool (Refund.approveRefundRequest (Refund.rrReference record) 2
      (Refund.rcCreatedAt creation)) pool >>= requireRight
    snd approved `shouldBe` True
    claim pool record >>= requireRight >>= ((`shouldBe` True) . snd)
    claim pool record >>= requireRight >>= ((`shouldBe` False) . snd)

  it "retains ambiguous funds, provider evidence and the original execution fence" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- claim pool record >>= requireRight
    let ref = Refund.rrReference record
        now = Refund.rcCreatedAt creation
    runSqlPool (Refund.recordRefundPending ref "synthetic-pending-refund" now) pool
      `shouldReturn` Right ()
    runSqlPool (Refund.recordRefundFailure ref "provider_verification_mismatch" now) pool
    held <- runSqlPool (Refund.loadRefund ref) pool
    fmap Refund.rrStatus held `shouldBe` Just "processing"
    fmap Refund.rrProviderRefundId held `shouldBe` Just (Just "synthetic-pending-refund")
    runSqlPool (Refund.cancelRefundRequest ref 2 now) pool >>= (`shouldSatisfy` isLeft)
    claim pool record >>= requireRight >>= ((`shouldBe` False) . snd)
    runSqlPool (Refund.requestSingleLineRefund creation
      { Refund.rcIdempotencyKey = "synthetic-replacement-refund" }) pool
      >>= (`shouldSatisfy` isLeft)
    assertFinancialCount pool creation 0

  it "quarantines legacy failed requests without reissuing, cancelling or release" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- claim pool record >>= requireRight
    -- Historical rows may mean verification mismatch, not confirmed no-refund.
    runSqlPool (rawExecute
      "UPDATE commerce_refund SET status='failed',failure_code='provider_verification_mismatch'\
      \ WHERE id=?::uuid" [PersistText (Refund.refundReferenceId (Refund.rrReference record))]) pool
    claim pool record >>= (`shouldSatisfy` isLeft)
    runSqlPool (Refund.cancelRefundRequest (Refund.rrReference record) 2
      (Refund.rcCreatedAt creation)) pool >>= (`shouldSatisfy` isLeft)
    runSqlPool (Refund.requestSingleLineRefund creation
      { Refund.rcIdempotencyKey = "synthetic-legacy-replacement" }) pool
      >>= (`shouldSatisfy` isLeft)
    runSqlPool (Refund.loadRefund (Refund.rrReference record)) pool
      >>= ((`shouldBe` Just "failed") . fmap Refund.rrStatus)

  it "keeps a cancelled request terminal when pending provider evidence arrives" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- runSqlPool (Refund.cancelRefundRequest (Refund.rrReference record) 2
      (Refund.rcCreatedAt creation)) pool >>= requireRight
    runSqlPool (Refund.recordRefundPending (Refund.rrReference record)
      "synthetic-late-refund" (Refund.rcCreatedAt creation)) pool
      >>= (`shouldSatisfy` isLeft)

  it "allows verified completion after ambiguity and records a refund only once" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- claim pool record >>= requireRight
    let verified = Refund.VerifiedRefund (Refund.rrReference record)
          ("synthetic-" <> Refund.refundReferenceId (Refund.rrReference record))
          (Refund.rcAmountMinor creation) "USD" (Refund.rcCreatedAt creation) "synthetic-refund"
    runSqlPool (Refund.recordRefundFailure (Refund.rrReference record)
      "paypal_refund_status_unverified" (Refund.rcCreatedAt creation)) pool
    runSqlPool (Refund.recordVerifiedRefund verified) pool `shouldReturn` Right True
    runSqlPool (Refund.recordVerifiedRefund verified) pool `shouldReturn` Right False
    runSqlPool (Refund.recordRefundFailure (Refund.rrReference record)
      "provider_verification_mismatch" (Refund.rcCreatedAt creation)) pool
    claim pool record >>= requireRight >>= ((`shouldBe` False) . snd)
    assertFinancialCount pool creation 1

  it "serializes competing reservations against the captured balance" $ \pool -> do
    creation <- fixture pool
    results <- concurrent
      [runSqlPool (Refund.requestSingleLineRefund value) pool | value <-
        [creation, creation { Refund.rcIdempotencyKey = "synthetic-competing-refund" }]]
    length (rights results) `shouldBe` 1

  it "reserves a failed line even when another line has enough unreserved funds" $ \pool -> do
    creation <- fixtureWithLines pool [7500, 5015]
    lines' <- runSqlPool (rawSql
      "SELECT id::text FROM commerce_checkout_line_item WHERE checkout_id=?::uuid\
      \ ORDER BY line_number"
      [PersistText (Checkout.checkoutReferenceId (Refund.rcCheckout creation))]) pool
    line <- case lines' of
      [Single first, Single _] -> pure first
      _ -> fail "Expected two synthetic immutable lines"
    record <- runSqlPool (Refund.requestAllocatedRefund
      creation { Refund.rcAmountMinor = 7500 } [Refund.RefundAllocation line 7500]) pool
      >>= requireRight
    _ <- claim pool record >>= requireRight
    runSqlPool (rawExecute "UPDATE commerce_refund SET status='failed' WHERE id=?::uuid"
      [PersistText (Refund.refundReferenceId (Refund.rrReference record))]) pool
    runSqlPool (Refund.requestAllocatedRefund creation
      { Refund.rcAmountMinor = 1, Refund.rcIdempotencyKey = "synthetic-same-line-overcommit" }
      [Refund.RefundAllocation line 1]) pool >>= (`shouldSatisfy` isLeft)
    runSqlPool (Refund.recordRefundPending (Refund.rrReference record)
      "synthetic-legacy-late-pending" (Refund.rcCreatedAt creation)) pool
      >>= (`shouldSatisfy` isLeft)
    runSqlPool (Refund.loadRefund (Refund.rrReference record)) pool
      >>= ((`shouldBe` Just "failed") . fmap Refund.rrStatus)

  it "releases a cancelled pre-execution reservation without sending a refund" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- runSqlPool (Refund.approveRefundRequest (Refund.rrReference record) 2
      (Refund.rcCreatedAt creation)) pool >>= requireRight
    _ <- runSqlPool (Refund.cancelRefundRequest (Refund.rrReference record) 2
      (Refund.rcCreatedAt creation)) pool >>= requireRight
    claim pool record >>= (`shouldSatisfy` isLeft)
    replacement <- request pool creation { Refund.rcIdempotencyKey = "synthetic-pre-send-replace" }
    Refund.rrStatus replacement `shouldBe` "requested"
    assertFinancialCount pool creation 0

  it "completes concurrent partial refunds once each without exceeding the capture" $ \pool -> do
    creation <- fixture pool
    first <- request pool creation { Refund.rcAmountMinor = 6000 }
    second <- request pool creation
      { Refund.rcAmountMinor = 6515, Refund.rcIdempotencyKey = "synthetic-second-partial" }
    forM_ [first, second] $ \record -> claim pool record >>= requireRight
      >>= ((`shouldBe` True) . snd)
    let completePartial record = runSqlPool (Refund.recordVerifiedRefund Refund.VerifiedRefund
          { Refund.vrRefund = Refund.rrReference record
          , Refund.vrProviderRefund =
              "synthetic-" <> Refund.refundReferenceId (Refund.rrReference record)
          , Refund.vrAmountMinor = Refund.rrAmountMinor record, Refund.vrCurrency = "USD"
          , Refund.vrOccurredAt = Refund.rcCreatedAt creation
          , Refund.vrCorrelationId = "synthetic-partial-completion"
          }) pool
    concurrent [completePartial first, completePartial second] `shouldReturn` [Right True, Right True]
    concurrent [completePartial first, completePartial second] `shouldReturn` [Right False, Right False]
    rows <- runSqlPool (rawSql
      "SELECT status,refunded_minor FROM commerce_checkout_session WHERE id=?::uuid"
      [PersistText (Checkout.checkoutReferenceId (Refund.rcCheckout creation))]) pool
    rows `shouldBe` [(Single ("refunded" :: Text), Single (12515 :: Int64))]
    assertIntentBalance pool creation "refunded" 12515 2

  it "refuses canonical balance drift without completing or posting a refund" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- claim pool record >>= requireRight
    runSqlPool (rawExecute
      "UPDATE commerce_payment_intent SET status='partially_refunded',refunded_minor=1\
      \ WHERE checkout_id=?::uuid"
      [PersistText (Checkout.checkoutReferenceId (Refund.rcCheckout creation))]) pool
    complete pool record "synthetic-drift-refund" "synthetic-drift"
      >>= (`shouldSatisfy` isLeft)
    assertHeld pool record
    assertIntentBalance pool creation "partially_refunded" 1 0

  it "does not overwrite a canonical dispute while completing a refund" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- claim pool record >>= requireRight
    runSqlPool (rawExecute
      "UPDATE commerce_payment_intent SET status='disputed' WHERE checkout_id=?::uuid"
      [PersistText (Checkout.checkoutReferenceId (Refund.rcCheckout creation))]) pool
    complete pool record "synthetic-disputed-refund" "synthetic-dispute"
      >>= (`shouldSatisfy` isLeft)
    assertHeld pool record
    assertIntentBalance pool creation "disputed" 0 0

  it "rejects an invalid canonical correlation before any financial mutation" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- claim pool record >>= requireRight
    complete pool record "synthetic-invalid-correlation" ""
      >>= (`shouldSatisfy` isLeft)
    assertHeld pool record
    assertIntentBalance pool creation "captured" 0 0

  it "rolls back the intent and refund if a later financial write fails" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- claim pool record >>= requireRight
    -- The invalid integer cast runs AFTER real completion in the same transaction.
    -- No temporary trigger or weakened financial constraint is needed.
    result <- try (runSqlPool (do
      verified <- Refund.recordVerifiedRefund (verifiedCompletion record "synthetic-atomic-refund"
        "synthetic-atomic")
      _ <- rawSql "SELECT 'synthetic-rollback'::bigint" []
        :: SqlPersistT IO [Single Int64]
      pure verified) pool) :: IO (Either SqlError (Either Text Bool))
    result `shouldSatisfy` isLeft
    assertHeld pool record
    assertIntentBalance pool creation "captured" 0 0
    complete pool record "synthetic-atomic-refund" "synthetic-atomic"
      `shouldReturn` Right True
    assertIntentBalance pool creation "refunded" 12515 1

  it "never replaces an immutable pending provider refund reference" $ \pool -> do
    creation <- fixture pool
    record <- request pool creation
    _ <- claim pool record >>= requireRight
    let pending value = runSqlPool (Refund.recordRefundPending (Refund.rrReference record)
          value (Refund.rcCreatedAt creation)) pool
    pending "synthetic-original-refund" `shouldReturn` Right ()
    pending "synthetic-original-refund" `shouldReturn` Right ()
    pending "synthetic-replacement-refund" >>= (`shouldSatisfy` isLeft)
    runSqlPool (Refund.loadRefund (Refund.rrReference record)) pool
      >>= ((`shouldBe` Just (Just "synthetic-original-refund")) . fmap Refund.rrProviderRefundId)

  where
    fixture pool = fixtureWithLines pool [12515]
    fixtureWithLines pool amounts = do
      payment <- captureFixture pool Checkout.ProviderPayPal
      requestId <- toText <$> nextRandom
      forM_ (zip [1..] amounts) $ \(lineNumber, amount) -> do
        lineId <- toText <$> nextRandom
        runSqlPool (rawExecute
          "INSERT INTO commerce_checkout_line_item(id,checkout_id,line_number,product_type,\
          \ product_id,product_version,description,quantity,unit_amount_minor,subtotal_minor,\
          \ total_minor,snapshot) VALUES (?::uuid,?::uuid,?,'service','synthetic','1',\
          \ 'Synthetic refund fixture',1,?,?,?,'{}'::jsonb)"
          ([PersistText lineId,
            PersistText (Checkout.checkoutReferenceId (Checkout.vpCheckout payment)),
            PersistInt64 lineNumber] <> replicate 3 (PersistInt64 amount))) pool
      runSqlPool (Checkout.recordVerifiedPayment payment) pool `shouldReturn` Right True
      pure Refund.RefundCreation
        { Refund.rcCheckout = Checkout.vpCheckout payment
        , Refund.rcPaymentAttempt = Checkout.vpAttempt payment
        , Refund.rcProvider = Checkout.vpProvider payment
        , Refund.rcEnvironment = Checkout.vpEnvironment payment
        , Refund.rcMerchantRef = Checkout.vpMerchantRef payment
        , Refund.rcAmountMinor = Checkout.vpAmountMinor payment
        , Refund.rcCurrency = "USD", Refund.rcReasonCode = "customer_request"
        , Refund.rcIdempotencyKey = "synthetic-refund-" <> requestId
        , Refund.rcRequestedBy = 1, Refund.rcCreatedAt = Checkout.vpOccurredAt payment
        }
    request pool creation =
      runSqlPool (Refund.requestSingleLineRefund creation) pool >>= requireRight
    claim pool record = runSqlPool (Refund.approveRefundForProcessing
      (Refund.rrReference record) 2 (Refund.rrCreatedAt record)) pool
    complete pool record providerRef correlation =
      runSqlPool (Refund.recordVerifiedRefund (verifiedCompletion record providerRef correlation)) pool

verifiedCompletion :: Refund.RefundRecord -> Text -> Text -> Refund.VerifiedRefund
verifiedCompletion record providerRef correlation = Refund.VerifiedRefund
  { Refund.vrRefund = Refund.rrReference record, Refund.vrProviderRefund = providerRef
  , Refund.vrAmountMinor = Refund.rrAmountMinor record, Refund.vrCurrency = Refund.rrCurrency record
  , Refund.vrOccurredAt = Refund.rrCreatedAt record, Refund.vrCorrelationId = correlation
  }

assertHeld :: ConnectionPool -> Refund.RefundRecord -> Expectation
assertHeld pool record = do
  rows <- runSqlPool (rawSql
    "SELECT refund.status,checkout.refunded_minor,\
    \ (SELECT COUNT(*) FROM commerce_ledger_transaction WHERE source_id=refund.id::text\
    \ AND transaction_type='payment_refund'),\
    \ (SELECT COUNT(*) FROM commerce_receipt WHERE checkout_id=checkout.id AND kind='credit_note')\
    \ FROM commerce_refund refund JOIN commerce_checkout_session checkout\
    \ ON checkout.id=refund.checkout_id WHERE refund.id=?::uuid"
    [PersistText (Refund.refundReferenceId (Refund.rrReference record))]) pool
    :: IO [(Single Text, Single Int64, Single Int64, Single Int64)]
  rows `shouldBe` [(Single "processing", Single 0, Single 0, Single 0)]

assertIntentBalance :: ConnectionPool -> Refund.RefundCreation -> Text -> Int64 -> Int64 -> Expectation
assertIntentBalance pool creation status refunded historyCount = do
  rows <- runSqlPool (rawSql
    "SELECT intent.status,intent.refunded_minor,\
    \ (SELECT COUNT(*) FROM commerce_payment_state_history history\
    \ WHERE history.payment_intent_id=intent.id\
    \ AND (history.event_type LIKE 'PaymentRefundVerified %'\
    \ OR history.event_type='refund_completion_verified'))\
    \ FROM commerce_payment_intent intent WHERE intent.checkout_id=?::uuid"
    [PersistText (Checkout.checkoutReferenceId (Refund.rcCheckout creation))]) pool
    :: IO [(Single Text, Single Int64, Single Int64)]
  rows `shouldBe` [(Single status, Single refunded, Single historyCount)]

assertFinancialCount :: ConnectionPool -> Refund.RefundCreation -> Int64 -> Expectation
assertFinancialCount pool creation count = do
  rows <- runSqlPool (rawSql
    "SELECT checkout.refunded_minor,(SELECT COUNT(*) FROM commerce_receipt receipt\
    \ WHERE receipt.checkout_id=checkout.id AND receipt.kind='credit_note'),\
    \ (SELECT COUNT(*) FROM commerce_ledger_transaction txn JOIN commerce_refund refund\
    \ ON txn.source_id=refund.id::text WHERE refund.checkout_id=checkout.id\
    \ AND txn.transaction_type='payment_refund') FROM commerce_checkout_session checkout\
    \ WHERE checkout.id=?::uuid"
    [PersistText (Checkout.checkoutReferenceId (Refund.rcCheckout creation))]) pool
    :: IO [(Single Int64, Single Int64, Single Int64)]
  rows `shouldBe` [(Single (count * Refund.rcAmountMinor creation), Single count, Single count)]
  assertIntentBalance pool creation (if count == 0 then "captured" else "refunded")
    (count * Refund.rcAmountMinor creation) count

requireRight :: Show error => Either error value -> IO value
requireRight = either (fail . show) pure

concurrent :: [IO value] -> IO [value]
concurrent actions = do
  completions <- forM actions $ \action -> do
    completion <- newEmptyMVar
    _ <- forkFinally action (putMVar completion)
    pure completion
  mapM (\completion -> takeMVar completion >>= either throwIO pure) completions
