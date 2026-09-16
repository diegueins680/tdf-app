{-# LANGUAGE OverloadedStrings #-}

-- Synthetic money/property and real PostgreSQL tests; no provider HTTP.
module TDF.Commerce.RefundSafetySpec (spec, databaseSpec) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO)
import Control.Monad (forM, forM_)
import Data.Either (isLeft, rights)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (addUTCTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool)
import Test.Hspec
import qualified Test.QuickCheck as QC

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.RefundStore as Refund

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

databaseSpec
  :: (ConnectionPool -> Checkout.PaymentProvider -> IO Checkout.VerifiedPayment)
  -> SpecWith ConnectionPool
databaseSpec captureFixture = describe "refund-safety execution and reservation" $ do
  it "grants one durable execution claim, including retries long after provider retention" $ \pool -> do
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

  it "keeps two-person approval and permits a separately approved request exactly once" $ \pool -> do
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

  it "quarantines legacy failed requests without reissuing, cancelling or releasing them" $ \pool -> do
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

  where
    fixture pool = do
      payment <- captureFixture pool Checkout.ProviderPayPal
      lineId <- toText <$> nextRandom
      runSqlPool (rawExecute
        "INSERT INTO commerce_checkout_line_item(id,checkout_id,line_number,product_type,\
        \ product_id,product_version,description,quantity,unit_amount_minor,subtotal_minor,\
        \ total_minor,snapshot) VALUES (?::uuid,?::uuid,1,'service','synthetic','1',\
        \ 'Synthetic refund fixture',1,12515,12515,12515,'{}'::jsonb)"
        [PersistText lineId, PersistText (Checkout.checkoutReferenceId (Checkout.vpCheckout payment))])
        pool
      runSqlPool (Checkout.recordVerifiedPayment payment) pool `shouldReturn` Right True
      pure Refund.RefundCreation
        { Refund.rcCheckout = Checkout.vpCheckout payment
        , Refund.rcPaymentAttempt = Checkout.vpAttempt payment
        , Refund.rcProvider = Checkout.vpProvider payment
        , Refund.rcEnvironment = Checkout.vpEnvironment payment
        , Refund.rcMerchantRef = Checkout.vpMerchantRef payment
        , Refund.rcAmountMinor = Checkout.vpAmountMinor payment
        , Refund.rcCurrency = "USD", Refund.rcReasonCode = "customer_request"
        , Refund.rcIdempotencyKey = "synthetic-refund-" <> lineId
        , Refund.rcRequestedBy = 1, Refund.rcCreatedAt = Checkout.vpOccurredAt payment
        }
    request pool creation = runSqlPool (Refund.requestSingleLineRefund creation) pool >>= requireRight
    claim pool record = runSqlPool (Refund.approveRefundForProcessing
      (Refund.rrReference record) 2 (Refund.rrCreatedAt record)) pool

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

requireRight :: Show error => Either error value -> IO value
requireRight = either (fail . show) pure

concurrent :: [IO value] -> IO [value]
concurrent actions = do
  completions <- forM actions $ \action -> do
    completion <- newEmptyMVar
    _ <- forkFinally action (putMVar completion)
    pure completion
  mapM (\completion -> takeMVar completion >>= either throwIO pure) completions
