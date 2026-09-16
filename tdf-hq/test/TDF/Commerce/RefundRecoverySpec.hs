{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.RefundRecoverySpec (spec, databaseSpec) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO)
import Control.Monad (forM, forM_)
import Data.Aeson ((.=), Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool)
import Test.Hspec
import qualified Test.QuickCheck as QC

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.ProviderAdapter as Adapter
import qualified TDF.Commerce.ProviderAdapter.Http as Http
import TDF.Commerce.ProviderAdapter.PayPalRefund
import TDF.Commerce.RefundReconciliation
import qualified TDF.Commerce.RefundStore as Refund

spec :: Spec
spec = describe "held-refund query adapter" $ do
  forM_ [Checkout.CheckoutSandbox, Checkout.CheckoutProduction] $ \environment ->
    it ("builds only an allowlisted GET with a redacted token for " <> show environment) $ do
      request <- requireRight (buildRefundQuery "synthetic-access-token"
        exampleBinding { rqbEnvironment = environment })
      Adapter.arMethod request `shouldBe` Adapter.AdapterGet
      Adapter.arOperation request `shouldBe` Adapter.AdapterQuery
      Adapter.arBody request `shouldBe` Nothing
      Adapter.arRetryPolicy request `shouldBe` Adapter.SafeReadRetry
      Http.adapterRequestDestinationAllowed request `shouldBe` True
      show (Adapter.safeRequestSummary request) `shouldNotContain` "synthetic-access-token"
      show (map snd (Adapter.arHeaders request)) `shouldNotContain` "synthetic-access-token"

  it "validates the sanitized official represented completion example" $
    parseRefundQuery exampleBinding (payload exampleBinding "COMPLETED" "10.99")
      `shouldBe` Right RefundQueryCompleted

  forM_ ["PENDING", "FAILED", "CANCELLED", "DECLINED", "UNKNOWN", "completed", ""] $ \status ->
    it ("never releases a held amount from status " <> show status) $
      parseRefundQuery exampleBinding (payload exampleBinding status "10.99")
        `shouldBe` Right RefundQueryHeld

  forM_ ["10.991", "1e2", "+10.99", "-10.99", " 10.99", "10.99 ", "", "NaN",
          "92233720368547758.08", "10.98", "0"] $ \amount ->
    it ("rejects malformed, overflowing or mismatched decimal " <> show amount) $
      parseRefundQuery exampleBinding (payload exampleBinding "COMPLETED" amount)
        `shouldSatisfy` isLeft

  it "rejects another refund, capture, currency, environment and merchant" $ do
    let original = payload exampleBinding "COMPLETED" "10.99"
    forM_ [ exampleBinding { rqbRefundId = "OTHER-REFUND" }
          , exampleBinding { rqbCaptureId = "OTHER-CAPTURE" }
          , exampleBinding { rqbCurrency = "EUR" }
          , exampleBinding { rqbEnvironment = Checkout.CheckoutSandbox }
          ] $ \binding -> parseRefundQuery binding original `shouldSatisfy` isLeft
    parseRefundQuery exampleBinding (setField "payer"
      (A.object ["merchant_id" .= ("OTHER-MERCHANT" :: Text)]) original) `shouldSatisfy` isLeft

  it "rejects absent, duplicate or attacker-controlled link evidence" $ do
    let original = payload exampleBinding "COMPLETED" "10.99"
        legitimate = linksFor exampleBinding
        attacker = A.object ["rel" .= ("up" :: Text), "method" .= ("GET" :: Text),
          "href" .= ("https://attacker.invalid/capture" :: Text)]
    forM_ [[], legitimate <> legitimate, [attacker], [A.Null]] $ \links ->
      parseRefundQuery exampleBinding (setField "links" (A.toJSON links) original)
        `shouldSatisfy` isLeft

  forM_ ["../other", "R?capture=other", "R/other", "R#fragment", "R\nvalue", ""] $ \ref ->
    it ("rejects unsafe refund identifiers before credentials for " <> show ref) $
      buildRefundQuery "synthetic-token" exampleBinding { rqbRefundId = ref }
        `shouldSatisfy` either (const True) (const False)

  it "rejects a header-shaped token" $
    buildRefundQuery "synthetic\r\nInjected: value" exampleBinding
      `shouldSatisfy` either (const True) (const False)

  it "round-trips every generated positive Int64 minor amount without rounding" $
    QC.forAll (QC.choose (1, maxBound) :: QC.Gen Int64) $ \minor ->
      let binding = exampleBinding { rqbAmountMinor = minor }
          decimal = T.pack (show (minor `div` 100)) <> "."
            <> T.justifyRight 2 '0' (T.pack (show (minor `mod` 100)))
      in parseRefundQuery binding (payload binding "COMPLETED" decimal)
           QC.=== Right RefundQueryCompleted

databaseSpec
  :: (ConnectionPool -> Checkout.PaymentProvider -> IO Checkout.VerifiedPayment)
  -> SpecWith ConnectionPool
databaseSpec captureFixture = after resetAuthority $ describe "held-refund authoritative query" $ do
  it "reads readiness without provider contact or financial/audit mutations" $ \pool -> do
    record <- fixture pool
    before <- snapshot pool record
    view <- readRefundRecovery pool (Refund.rrReference record) (const (pure True)) >>= requireRight
    rrvCanQuery view `shouldBe` True
    rrvOutcome view `shouldBe` "not_queried"
    snapshot pool record `shouldReturn` before

  it "completes the original refund and canonical accounting with one GET callback" $ \pool -> do
    record <- fixture pool
    calls <- newIORef (0 :: Int)
    let query binding = do
          atomicModifyIORef' calls (\n -> (n + 1, ()))
          rqbRefundId binding `shouldBe` providerId record
          pure (Right RefundQueryCompleted)
    view <- run pool record (const (pure True)) query >>= requireRight
    Refund.rrStatus (rrvRefund view) `shouldBe` "succeeded"
    rrvCanQuery view `shouldBe` False
    rrvOutcome view `shouldBe` "completed"
    readIORef calls `shouldReturn` 1
    snapshot pool record `shouldReturn` [Single "succeeded:12515:12515:1:1:2"]
    run pool record (const (pure True)) query >>= (`shouldSatisfy` isLeft)
    readIORef calls `shouldReturn` 1

  it "holds pending and unknown outcomes without granting another refund permit" $ \pool -> do
    record <- fixture pool
    view <- run pool record (const (pure True)) (const (pure (Right RefundQueryHeld)))
      >>= requireRight
    rrvOutcome view `shouldBe` "held"
    snapshot pool record `shouldReturn` [Single "processing:0:0:0:0:2"]
    replay <- runSqlPool (Refund.approveRefundForProcessing (Refund.rrReference record)
      2 (Refund.rrCreatedAt record)) pool >>= requireRight
    snd replay `shouldBe` False

  it "retains only fixed audit codes when provider transport throws sensitive text" $ \pool -> do
    record <- fixture pool
    result <- run pool record (const (pure True)) (\_ -> fail "synthetic-private-provider-payload")
    result `shouldSatisfy` either (== RefundRecoveryQueryFailed) (const False)
    snapshot pool record `shouldReturn` [Single "processing:0:0:0:0:2"]
    rows <- runSqlPool (rawSql
      "SELECT metadata::text FROM commerce_checkout_audit_event\
      \ WHERE checkout_id=?::uuid AND event_type='refund_query'"
      [PersistText (Checkout.checkoutReferenceId (Refund.rrCheckout record))]) pool
      :: IO [Single Text]
    show rows `shouldNotContain` "synthetic-private-provider-payload"

  forM_ ["flag", "account", "capability", "configuration"] $ \gate ->
    it ("fails closed before querying with missing " <> T.unpack gate) $ \pool -> do
      record <- fixture pool
      revoke pool gate
      calls <- newIORef (0 :: Int)
      let query _ = atomicModifyIORef' calls (\n -> (n + 1, Right RefundQueryCompleted))
      result <- run pool record (const (pure (gate /= "configuration"))) query
      result `shouldSatisfy` isLeft
      readIORef calls `shouldReturn` 0
      snapshot pool record `shouldReturn` [Single "processing:0:0:0:0:0"]

  forM_ ["flag", "account", "capability", "configuration"] $ \gate ->
    it ("rechecks " <> T.unpack gate <> " after a successful provider query") $ \pool -> do
      record <- fixture pool
      configured <- newIORef True
      let query _ = do
            revoke pool gate
            atomicModifyIORef' configured (const (gate /= "configuration", ()))
            pure (Right RefundQueryCompleted)
      result <- run pool record (const (readIORef configured)) query
      result `shouldSatisfy` isLeft
      snapshot pool record `shouldReturn` [Single "processing:0:0:0:0:2"]

  it "shares admission quota across concurrent operators without duplicate queries" $ \pool -> do
    record <- fixture pool
    calls <- newIORef (0 :: Int)
    let query _ = atomicModifyIORef' calls (\n -> (n + 1, Right RefundQueryHeld))
    results <- concurrent (replicate 8 (run pool record (const (pure True)) query))
    length (filter (either (const False) (const True)) results) `shouldBe` 1
    readIORef calls `shouldReturn` 1
    snapshot pool record `shouldReturn` [Single "processing:0:0:0:0:2"]

  it "does not regress when completion wins a race with an older pending query" $ \pool -> do
    record <- fixture pool
    let query _ = do
          runSqlPool (Refund.recordVerifiedRefund (verified record)) pool `shouldReturn` Right True
          pure (Right RefundQueryHeld)
    view <- run pool record (const (pure True)) query >>= requireRight
    rrvOutcome view `shouldBe` "already_completed"
    snapshot pool record `shouldReturn` [Single "succeeded:12515:12515:1:1:2"]

  it "keeps canonical drift held after positive provider evidence" $ \pool -> do
    record <- fixture pool
    runSqlPool (rawExecute
      "UPDATE commerce_payment_intent SET status='disputed' WHERE checkout_id=?::uuid"
      [PersistText (Checkout.checkoutReferenceId (Refund.rrCheckout record))]) pool
    result <- run pool record (const (pure True)) (const (pure (Right RefundQueryCompleted)))
    result `shouldSatisfy` either (== RefundRecoveryConflict) (const False)
    snapshot pool record `shouldReturn` [Single "processing:0:0:0:0:2"]

  where
    run pool record = reconcileKnownRefund pool (Refund.rrReference record) 3
    fixture pool = do
      resetAuthority pool
      payment <- captureFixture pool Checkout.ProviderPayPal
      lineId <- toText <$> nextRandom
      runSqlPool (rawExecute
        "INSERT INTO commerce_checkout_line_item(id,checkout_id,line_number,product_type,\
        \ product_id,product_version,description,quantity,unit_amount_minor,subtotal_minor,\
        \ total_minor,snapshot) VALUES (?::uuid,?::uuid,1,'service','synthetic','1',\
        \ 'Synthetic refund recovery',1,12515,12515,12515,'{}'::jsonb)"
        [PersistText lineId, PersistText (Checkout.checkoutReferenceId (Checkout.vpCheckout payment))]) pool
      runSqlPool (Checkout.recordVerifiedPayment payment) pool `shouldReturn` Right True
      record <- runSqlPool (Refund.requestSingleLineRefund Refund.RefundCreation
        { Refund.rcCheckout = Checkout.vpCheckout payment, Refund.rcPaymentAttempt = Checkout.vpAttempt payment
        , Refund.rcProvider = Checkout.ProviderPayPal, Refund.rcEnvironment = Checkout.CheckoutSandbox
        , Refund.rcMerchantRef = Checkout.vpMerchantRef payment, Refund.rcAmountMinor = 12515
        , Refund.rcCurrency = "USD", Refund.rcReasonCode = "customer_request"
        , Refund.rcIdempotencyKey = "synthetic-recovery-" <> lineId
        , Refund.rcRequestedBy = 1, Refund.rcCreatedAt = Checkout.vpOccurredAt payment
        }) pool >>= requireRight
      _ <- runSqlPool (Refund.approveRefundForProcessing (Refund.rrReference record) 2
        (Refund.rrCreatedAt record)) pool >>= requireRight
      runSqlPool (Refund.recordRefundPending (Refund.rrReference record) (providerId record)
        (Refund.rrCreatedAt record)) pool `shouldReturn` Right ()
      runSqlPool (do
        rawExecute "INSERT INTO revenue_feature_flag(flag_key,enabled,environment,reason)\
          \ VALUES ('checkout.paypal.refund_reconciliation',true,'sandbox','synthetic fixture')\
          \ ON CONFLICT(flag_key,environment) DO UPDATE SET enabled=true" []
        rawExecute "UPDATE commerce_provider_account SET enabled=true,status='ready',\
          \ contract_status='approved',credential_status='validated'\
          \ WHERE provider='paypal' AND environment='sandbox'" []
        rawExecute "UPDATE commerce_provider_capability SET verification_status='sandbox_verified'\
          \ WHERE provider_account_id=(SELECT id FROM commerce_provider_account\
          \ WHERE provider='paypal' AND environment='sandbox')\
          \ AND payment_method='paypal_wallet'" []
        rawExecute "DELETE FROM commerce_provider_query_budget\
          \ WHERE provider='paypal' AND environment='sandbox'" []) pool
      current <- runSqlPool (Refund.loadRefund (Refund.rrReference record)) pool
      maybe (fail "Synthetic refund missing") pure current

resetAuthority :: ConnectionPool -> IO ()
resetAuthority pool = runSqlPool (do
  rawExecute "UPDATE commerce_provider_account SET enabled=true,status='ready',\
    \ contract_status='approved',credential_status='validated'\
    \ WHERE provider='paypal' AND environment='sandbox'" []
  rawExecute "UPDATE commerce_provider_capability SET verification_status='sandbox_verified'\
    \ WHERE provider_account_id=(SELECT id FROM commerce_provider_account\
    \ WHERE provider='paypal' AND environment='sandbox')" []
  rawExecute "UPDATE revenue_feature_flag SET enabled=false\
    \ WHERE flag_key='checkout.paypal.refund_reconciliation' AND environment='sandbox'" []) pool

revoke :: ConnectionPool -> Text -> IO ()
revoke pool gate = runSqlPool (case gate of
  "flag" -> rawExecute "UPDATE revenue_feature_flag SET enabled=false\
    \ WHERE flag_key='checkout.paypal.refund_reconciliation' AND environment='sandbox'" []
  "account" -> rawExecute "UPDATE commerce_provider_account SET enabled=false\
    \ WHERE provider='paypal' AND environment='sandbox'" []
  "capability" -> rawExecute "UPDATE commerce_provider_capability SET verification_status='documented'\
    \ WHERE provider_account_id=(SELECT id FROM commerce_provider_account\
    \ WHERE provider='paypal' AND environment='sandbox') AND capability='server_verification'" []
  _ -> pure ()) pool

snapshot :: ConnectionPool -> Refund.RefundRecord -> IO [Single Text]
snapshot pool record = runSqlPool (rawSql
  "SELECT concat_ws(':',refund.status,checkout.refunded_minor,intent.refunded_minor,\
  \ (SELECT COUNT(*) FROM commerce_receipt WHERE refund_id=refund.id AND kind='credit_note'),\
  \ (SELECT COUNT(*) FROM commerce_ledger_transaction WHERE source_id=refund.id::text\
  \ AND transaction_type='payment_refund'),\
  \ (SELECT COUNT(*) FROM commerce_checkout_audit_event\
  \ WHERE checkout_id=checkout.id AND event_type='refund_query'))\
  \ FROM commerce_refund refund JOIN commerce_checkout_session checkout ON checkout.id=refund.checkout_id\
  \ JOIN commerce_payment_attempt attempt ON attempt.id=refund.payment_attempt_id\
  \ JOIN commerce_payment_intent intent ON intent.id=attempt.payment_intent_id WHERE refund.id=?::uuid"
  [PersistText (Refund.refundReferenceId (Refund.rrReference record))]) pool

verified :: Refund.RefundRecord -> Refund.VerifiedRefund
verified record = Refund.VerifiedRefund (Refund.rrReference record) (providerId record)
  12515 "USD" (Refund.rrCreatedAt record) "synthetic-competing-completion"

providerId :: Refund.RefundRecord -> Text
providerId record = "R-" <> Refund.refundReferenceId (Refund.rrReference record)

exampleBinding :: RefundQueryBinding
exampleBinding = RefundQueryBinding Checkout.CheckoutProduction
  "1JU08902781691411" "2GG279541U471931P" "SYNTHETIC-MERCHANT" 1099 "USD"

payload :: RefundQueryBinding -> Text -> Text -> Value
payload binding status amount = A.object
  [ "id" .= rqbRefundId binding, "status" .= status
  , "amount" .= A.object ["value" .= amount, "currency_code" .= rqbCurrency binding]
  , "links" .= linksFor binding
  ]

linksFor :: RefundQueryBinding -> [Value]
linksFor binding =
  [ link "self" ("refunds/" <> rqbRefundId binding)
  , link "up" ("captures/" <> rqbCaptureId binding)
  ]
  where
    base = case rqbEnvironment binding of
      Checkout.CheckoutSandbox -> "https://api-m.sandbox.paypal.com/v2/payments/"
      Checkout.CheckoutProduction -> "https://api-m.paypal.com/v2/payments/"
    link rel path = A.object ["rel" .= (rel :: Text), "method" .= ("GET" :: Text), "href" .= (base <> path)]

setField :: A.Key -> Value -> Value -> Value
setField key value (A.Object obj) = A.Object (KM.insert key value obj)
setField _ _ value = value

requireRight :: Show e => Either e a -> IO a
requireRight = either (fail . show) pure

concurrent :: [IO a] -> IO [a]
concurrent actions = do
  boxes <- forM actions $ \action -> do
    box <- newEmptyMVar
    _ <- forkFinally action (putMVar box)
    pure box
  mapM (\box -> takeMVar box >>= either throwIO pure) boxes
