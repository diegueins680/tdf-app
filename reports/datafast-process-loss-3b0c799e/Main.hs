{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist
import Database.Persist.Postgresql
import System.Environment
import System.IO.Temp
import System.Process
import Test.Hspec
import qualified TDF.Commerce.ProviderExecutionStore as E
import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentRuntimeStore as Runtime
import TDF.Commerce.ProviderCapabilities (PaymentMethod(..))
-- Only used by other provider fixtures. Abort if reached by this test.
providerReference :: Checkout.PaymentProvider -> Text -> Text
providerReference _ _ = error "out-of-scope provider fixture"
main = do
 url <- getEnv "TDF_PROVIDER_RETRY_DATABASE_URL"
 db <- openDatabase url
 args <- getArgs
 case args of
  ["origin",dump] -> do
   seed <- newCheckout db
   payment <- captureFixtureFromCheckout seed "payment" Checkout.AttemptProcessing db Checkout.ProviderDatafast
   callProcess "pg_dump" ["-Fc","-f",dump,url]
   runSqlPool (Checkout.recordVerifiedPaymentOrReconcile payment) db `shouldReturn` Right True
   putStrLn "origin: committed post-snapshot confirmation; exiting"
  ["recover"] -> do
   let query = "SELECT merchant_account_ref, provider_resource_id, merchant_reference FROM commerce_provider_binding WHERE provider='datafast' AND environment='sandbox' AND resource_type='checkout'"
   [(Single merchant,Single resource,Single ref)] <- runSqlPool (rawSql query []) db :: IO [(Single Text,Single Text,Single Text)]
   let load = E.loadBoundProviderPayment Checkout.ProviderDatafast Checkout.CheckoutSandbox merchant resource (Just ref)
   result <- runSqlPool load db
   case result of
    Right _ -> fail "unexpected admission; continue with provider verification"
    Left reason -> putStrLn ("recovery binding lookup: " ++ T.unpack reason)
   runSqlPool (rawExecute "UPDATE revenue_feature_flag SET enabled=true WHERE environment='sandbox' AND flag_key='checkout.provider_query_recovery'" []) db
   runSqlPool (E.queryRecoveryReady Checkout.ProviderDatafast Checkout.CheckoutSandbox merchant) db `shouldReturn` True
   putStrLn "synthetic recovery flag and merchant readiness: true"
   n <- runSqlPool (E.enqueueProviderQueries Checkout.ProviderDatafast Checkout.CheckoutSandbox) db
   n `shouldBe` 0
   putStrLn "durable recovery jobs enqueued: 0"
  _ -> fail "unknown mode"
 destroyAllResources db
captureFixtureFromCheckout :: Checkout.PaymentAttemptCreation -> Text -> Checkout.PaymentAttemptStage
  -> ConnectionPool -> Checkout.PaymentProvider -> IO Checkout.VerifiedPayment
captureFixtureFromCheckout seed requestedResourceType stage pool provider = do
  let creation = seed { Checkout.pacProvider = provider }
      method = case provider of
        Checkout.ProviderPayPal -> MethodPayPalWallet
        Checkout.ProviderPayPhone -> MethodPayPhoneWallet
        _ -> MethodCard
      checkoutId = Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
      resourceType = case (provider, requestedResourceType) of
        (Checkout.ProviderDatafast, "payment") -> "checkout"
        (Checkout.ProviderDatafast, "capture") -> "checkout"
        (Checkout.ProviderPayPal, "payment") -> "capture"
        (Checkout.ProviderPlaceToPay, "payment") -> "session"
        (Checkout.ProviderPayPhone, "payment") -> "sale"
        _ -> requestedResourceType
  [Single domainReference] <- runSqlPool (rawSql
    "SELECT domain_order_id FROM commerce_checkout_session WHERE id=?::uuid"
    [PersistText checkoutId]) pool
  attempt <- runSqlPool (Runtime.beginPaymentAttemptForMethod method creation) pool >>= requireRight
  let resource = "synthetic-capture-" <> Checkout.paymentAttemptReferenceId attempt
      boundReference = if provider `elem` [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone]
        then providerReference provider checkoutId else domainReference
  when (provider `elem` [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone]) $
    runSqlPool (rawExecute
      "INSERT INTO commerce_provider_operation(payment_attempt_id,provider,environment,merchant_account_ref,\
      \ provider_reference,operation,idempotency_key,request_sha256,provider_resource_id,status,outcome_certainty,started_at)\
      \ VALUES (?::uuid,?,'sandbox',?,?,'create',?,repeat('a',64),?,'processing','ambiguous',NOW())"
      [PersistText (Checkout.paymentAttemptReferenceId attempt),PersistText (Checkout.paymentProviderText provider),
       PersistText (Checkout.pacMerchantRef creation),PersistText boundReference,
       PersistText (Checkout.pacIdempotencyKey creation),PersistText resource]) pool
  _ <- runSqlPool (Checkout.bindProviderResource Checkout.ProviderBindingCreation
    { Checkout.pbcAttempt = attempt, Checkout.pbcCheckout = Checkout.pacCheckout creation
    , Checkout.pbcProvider = provider, Checkout.pbcEnvironment = Checkout.CheckoutSandbox
    , Checkout.pbcMerchantRef = Checkout.pacMerchantRef creation, Checkout.pbcResourceType = resourceType
    , Checkout.pbcProviderResource = resource, Checkout.pbcResourcePath = Nothing
    , Checkout.pbcOrderReference = boundReference, Checkout.pbcAmountMinor = 12515
    , Checkout.pbcCurrency = "USD", Checkout.pbcStage = stage
    , Checkout.pbcOccurredAt = Checkout.pacCreatedAt creation
    , Checkout.pbcCorrelationId = "synthetic-capture-binding" }) pool >>= requireRight
  pure Checkout.VerifiedPayment
    { Checkout.vpAttempt = attempt, Checkout.vpCheckout = Checkout.pacCheckout creation
    , Checkout.vpProvider = provider, Checkout.vpEnvironment = Checkout.CheckoutSandbox
    , Checkout.vpMerchantRef = Checkout.pacMerchantRef creation, Checkout.vpResourceType = resourceType
    , Checkout.vpProviderResource = resource, Checkout.vpProviderResourcePath = Nothing
    , Checkout.vpOrderReference = domainReference, Checkout.vpProviderReference = boundReference
    , Checkout.vpAmountMinor = 12515, Checkout.vpCurrency = "USD"
    , Checkout.vpEvidence = "server_to_server", Checkout.vpOccurredAt = Checkout.pacCreatedAt creation
    , Checkout.vpCorrelationId = "synthetic-capture-verification" }


captureSnapshot :: Checkout.VerifiedPayment -> SqlPersistT IO [Single Text]
captureSnapshot payment = rawSql
  "SELECT jsonb_build_object('checkout',to_jsonb(checkout),'attempt',to_jsonb(attempt),\
  \ 'intent',to_jsonb(intent),\
  \ 'manual',(SELECT jsonb_agg(to_jsonb(evidence) ORDER BY evidence.id) FROM commerce_manual_payment_evidence evidence WHERE checkout_id=checkout.id),\
  \ 'receipts',(SELECT jsonb_agg(to_jsonb(receipt) ORDER BY receipt.id) FROM commerce_receipt receipt WHERE checkout_id=checkout.id),\
  \ 'ledger',(SELECT jsonb_agg(to_jsonb(txn) ORDER BY txn.id) FROM commerce_ledger_transaction txn WHERE source_id=attempt.id::text),\
  \ 'entries',(SELECT jsonb_agg(to_jsonb(entry) ORDER BY entry.id) FROM commerce_ledger_entry entry JOIN commerce_ledger_transaction txn ON txn.id=entry.transaction_id WHERE txn.source_id=attempt.id::text),\
  \ 'history',(SELECT jsonb_agg(to_jsonb(history) ORDER BY history.id) FROM commerce_payment_state_history history WHERE payment_intent_id=intent.id),\
  \ 'audit',(SELECT jsonb_agg(to_jsonb(audit) ORDER BY audit.id) FROM commerce_checkout_audit_event audit WHERE checkout_id=checkout.id)\
  \ )::text FROM commerce_payment_attempt attempt\
  \ JOIN commerce_checkout_session checkout ON checkout.id=attempt.checkout_id\
  \ JOIN commerce_payment_intent intent ON intent.id=attempt.payment_intent_id\
  \ WHERE attempt.id=?::uuid"
  [PersistText (Checkout.paymentAttemptReferenceId (Checkout.vpAttempt payment))]


openDatabase :: String -> IO ConnectionPool
openDatabase url = do
  pool <- runNoLoggingT (createPostgresqlPool (BS.pack url) 4)
  names <- runSqlPool (rawSql "SELECT current_database()" []) pool :: IO [Single Text]
  unless (names == [Single "tdf_provider_retry_test"]) $ do
    destroyAllResources pool
    fail "Provider retry tests require the dedicated disposable database"
  runSqlPool (do
    rawExecute
      "UPDATE commerce_provider_account SET status='ready',contract_status='approved',\
      \ credential_status='validated',enabled=true,verified_at=NOW(),verified_by=1,\
      \ merchant_account_ref='synthetic-provider-retry-merchant' WHERE environment='sandbox'" []
    rawExecute
      "UPDATE commerce_provider_capability SET verification_status='sandbox_verified',verified_at=NOW()\
      \ WHERE provider_account_id IN (SELECT id FROM commerce_provider_account WHERE environment='sandbox')" []
    ) pool
  pure pool

newCheckout :: ConnectionPool -> IO Checkout.PaymentAttemptCreation
newCheckout = newCheckoutForDomain "event_ticket_order"

newCheckoutForDomain :: Text -> ConnectionPool -> IO Checkout.PaymentAttemptCreation
newCheckoutForDomain domain = newCheckoutForDomainWithLines domain [12515]

-- Construct the immutable checkout snapshot once, before any payment attempt.
-- Refund fixtures may split its fixed total, but cannot append a second snapshot.
newCheckoutForDomainWithLines :: Text -> [Int64] -> ConnectionPool -> IO Checkout.PaymentAttemptCreation
newCheckoutForDomainWithLines domain amounts = newCheckoutWithOrder domain amounts Nothing

newCheckoutWithOrder :: Text -> [Int64] -> Maybe Text -> ConnectionPool -> IO Checkout.PaymentAttemptCreation
newCheckoutWithOrder domain amounts domainOrder pool = do
  unless (all (> 0) amounts && sum (map toInteger amounts) == 12515) $
    fail "Synthetic checkout lines must sum exactly to 12515"
  checkoutId <- toText <$> nextRandom
  now <- getCurrentTime
  runSqlPool (do
    orderReference <- case (domain, domainOrder) of
      ("event_ticket_order", Nothing) -> do
        [Single orderKey] <- rawSql "SELECT payment_test.ticket_order()" []
        pure (T.pack (show (orderKey :: Int64)))
      _ -> pure (maybe checkoutId id domainOrder)
    gross <- if domain == "event_ticket_order"
      then do
        [Single value] <- rawSql
          "SELECT tier.price_cents*orders.quantity FROM event_ticket_order orders\
          \ JOIN event_ticket_tier tier ON tier.id=orders.tier_id WHERE orders.id=?::bigint"
          [PersistText orderReference]
        pure value
      else pure (12515 :: Int64)
    rawExecute
      "INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,environment,\
    \ currency,subtotal_minor,discount_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at)\
    \ VALUES (?::uuid,?,?,'awaiting_payment','sandbox','USD',?,?,12515,\
    \ 'synthetic@example.test',?,?,?)"
      [PersistText checkoutId, PersistText domain, PersistText orderReference, PersistInt64 gross, PersistInt64 (gross-12515),
        PersistText checkoutId, PersistText checkoutId, PersistUTCTime (addUTCTime 1800 now)]
    forM_ (zip [1..] amounts) $ \(lineNumber, amount) -> rawExecute
      "INSERT INTO commerce_checkout_line_item(checkout_id,line_number,product_type,product_id,\
      \ product_version,description,quantity,unit_amount_minor,subtotal_minor,discount_minor,total_minor,snapshot)\
      \ VALUES (?::uuid,?,?,?,'synthetic-v1','Provider retry fixture',1,?,?,?,?,'{}'::jsonb)"
      ([PersistText checkoutId, PersistInt64 lineNumber, PersistText domain, PersistText checkoutId]
        <> [PersistInt64 (if domain == "event_ticket_order" then gross else amount),
            PersistInt64 (if domain == "event_ticket_order" then gross else amount),
            PersistInt64 (if domain == "event_ticket_order" then gross-amount else 0),PersistInt64 amount])
    when (domain == "event_ticket_order" && domainOrder == Nothing) $
      attachTicketRuntime checkoutId orderReference 0 0
    ) pool
  pure Checkout.PaymentAttemptCreation
    { Checkout.pacCheckout = Checkout.CheckoutReference checkoutId
    , Checkout.pacProvider = Checkout.ProviderPlaceToPay
    , Checkout.pacEnvironment = Checkout.CheckoutSandbox
    , Checkout.pacOperation = Checkout.OperationCreate
    , Checkout.pacAmountMinor = 12515
    , Checkout.pacCurrency = "USD"
    , Checkout.pacMerchantRef = "synthetic-provider-retry-merchant"
    , Checkout.pacIdempotencyKey = "attempt-" <> checkoutId
    , Checkout.pacCreatedAt = now
    , Checkout.pacCorrelationId = "provider-retry-test-" <> checkoutId
    }

attachTicketRuntime :: Text -> Text -> Int64 -> Int64 -> SqlPersistT IO ()
attachTicketRuntime checkoutId orderReference fee tax = do
  attached <- rawSql
    "SELECT 1::bigint FROM payment_test.ticket_runtime(?::uuid,?::bigint,?,?)"
    [PersistText checkoutId,PersistText orderReference,PersistInt64 fee,PersistInt64 tax]
    :: SqlPersistT IO [Single Int64]
  unless (attached == [Single 1]) $ liftIO (fail "Ticket runtime fixture was not attached")


requireRight :: Show error => Either error value -> IO value
requireRight = either (fail . show) pure

concurrently :: [IO value] -> IO [value]
concurrently actions = do
  completions <- forM actions $ \action -> do
    completion <- newEmptyMVar
    _ <- forkFinally action (putMVar completion)
    pure completion
  mapM (\completion -> takeMVar completion >>= either throwIO pure) completions
