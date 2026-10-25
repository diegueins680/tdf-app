{-# LANGUAGE OverloadedStrings #-}

-- Executed by test-provider-retry-runtime.sh on its owned, migrated database.
-- This small entrypoint exercises the real store without compiling the HTTP app.
module Main (main) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (bracket, finally, throwIO)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import System.Environment (getEnv, lookupEnv)
import System.Timeout (timeout)
import Test.Hspec

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentIntentStore as Intent
import TDF.Commerce.ProviderCapabilities (PaymentMethod(..))
import TDF.Test.DisposablePostgres (isDisposablePostgresUrl)

main :: IO ()
main = do
  url <- getEnv "TDF_PROVIDER_RETRY_DATABASE_URL"
  ci <- (== Just "true") <$> lookupEnv "CI"
  overrides <- mapM lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"]
  unless (isDisposablePostgresUrl ci url && all (maybe True null) overrides) $
    fail "Payment expiry probe requires a local disposable URL without routing overrides"
  -- The shell runner owns and validates the database before this entrypoint.
  -- Also refuse accidental direct execution on a different database name.
  bracket (runNoLoggingT (createPostgresqlPool (BS.pack url) 4)) destroyAllResources $ \pool -> do
    database <- runSqlPool (rawSql "SELECT current_database()" []) pool :: IO [Single Text]
    unless (database == [Single "tdf_provider_retry_test"]) $
      fail "Payment expiry probe requires the owned provider-retry fixture"
    hspec $ describe "payment-intent expiry admission" $ do
      forM_ [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone,
             Checkout.ProviderPayPal, Checkout.ProviderBankTransfer] $ \provider -> do
        it ("rejects a stale request timestamp for " <> show provider) $ do
          now <- getCurrentTime
          creation <- fixture pool provider (addUTCTime (-1) now) (addUTCTime (-60) now)
          runSqlPool (Intent.createPaymentIntent creation) pool
            `shouldReturn` Left "Canonical checkout has expired"
          assertEmpty pool creation

      it "uses admission time rather than a future audit timestamp" $ do
        now <- getCurrentTime
        creation <- fixture pool Checkout.ProviderPlaceToPay
          (addUTCTime 3600 now) (addUTCTime 7200 now)
        runSqlPool (Intent.createPaymentIntent creation) pool >>= (`shouldSatisfy` isRight)

      forM_ ["READ COMMITTED", "REPEATABLE READ"] $ \isolation ->
        it ("checks time after a witnessed checkout lock wait at " <> T.unpack isolation) $ do
          now <- getCurrentTime
          creation <- fixture pool Checkout.ProviderPlaceToPay (addUTCTime 5 now) now
          locked <- newEmptyMVar
          release <- newEmptyMVar
          blockerDone <- newEmptyMVar
          _ <- forkFinally (runSqlPool (do
            _ <- rawSql "SELECT id::text FROM commerce_checkout_session WHERE id=?::uuid FOR UPDATE"
              [checkoutParam creation] :: DatabaseAction [Single Text]
            liftIO (putMVar locked () >> takeMVar release)) pool) (putMVar blockerDone)
          takeMVar locked
          waiterPid <- newEmptyMVar
          waiterDone <- newEmptyMVar
          _ <- forkFinally (runSqlPool (do
            rawExecute ("SET TRANSACTION ISOLATION LEVEL " <> isolation) []
            [Single pid] <- rawSql "SELECT pg_backend_pid()::bigint" []
            -- Begin the transaction before waiting; now()/statement-start time
            -- must not be mistaken for time after the checkout lock is obtained.
            liftIO (putMVar waiterPid (pid :: Int64))
            Intent.createPaymentIntent creation) pool) (putMVar waiterDone)
          pid <- takeMVar waiterPid
          (do
            await pool "checkout lock wait" "SELECT cardinality(pg_blocking_pids(?::integer)) > 0"
              [PersistInt64 pid]
            await pool "database expiry" "SELECT expires_at <= clock_timestamp() FROM commerce_checkout_session WHERE id=?::uuid"
              [checkoutParam creation]) `finally` putMVar release ()
          takeMVar blockerDone >>= either throwIO pure
          result <- takeMVar waiterDone >>= either throwIO pure
          result `shouldBe` Left "Canonical checkout has expired"
          assertEmpty pool creation

type DatabaseAction = SqlPersistT IO

checkoutParam :: Intent.PaymentIntentCreation -> PersistValue
checkoutParam = PersistText . Checkout.checkoutReferenceId . Intent.picCheckout

assertEmpty :: ConnectionPool -> Intent.PaymentIntentCreation -> Expectation
assertEmpty pool creation = do
  counts <- runSqlPool (rawSql
    "SELECT (SELECT count(*) FROM commerce_payment_intent WHERE checkout_id=?::uuid),\
    \ (SELECT count(*) FROM commerce_payment_amount_component component\
    \ JOIN commerce_payment_intent intent ON intent.id=component.payment_intent_id\
    \ WHERE intent.checkout_id=?::uuid)"
    [checkoutParam creation, checkoutParam creation]) pool :: IO [(Single Int64, Single Int64)]
  counts `shouldBe` [(Single 0, Single 0)]

await :: ConnectionPool -> String -> Text -> [PersistValue] -> IO ()
await pool label query parameters = do
  completed <- timeout 15000000 loop
  unless (completed == Just ()) (fail ("Did not witness " <> label))
  where
    loop = do
      rows <- runSqlPool (rawSql query parameters) pool :: IO [Single Bool]
      if rows == [Single True] then pure () else threadDelay 10000 >> loop

fixture :: ConnectionPool -> Checkout.PaymentProvider -> UTCTime -> UTCTime
  -> IO Intent.PaymentIntentCreation
fixture pool provider expires occurred = do
  identifier <- toText <$> nextRandom
  let checkout = Checkout.CheckoutReference identifier
  runSqlPool (do
    rawExecute
      "INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,environment,\
      \ currency,subtotal_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at)\
      \ VALUES (?::uuid,'service_booking',?,'awaiting_payment','sandbox','USD',100,100,\
      \ 'expiry@example.test',?,?,?)"
      [PersistText identifier, PersistText identifier, PersistText identifier,
       PersistText identifier, PersistUTCTime expires]
    rawExecute
      "INSERT INTO commerce_checkout_line_item(checkout_id,line_number,product_type,product_id,\
      \ product_version,description,quantity,unit_amount_minor,subtotal_minor,total_minor,snapshot)\
      \ VALUES (?::uuid,1,'service_booking',?,'synthetic-v1','Expiry fixture',1,100,100,100,'{}')"
      [PersistText identifier, PersistText identifier]) pool
  pure Intent.PaymentIntentCreation
    { Intent.picCheckout = checkout
    , Intent.picEnvironment = Checkout.CheckoutSandbox
    , Intent.picProvider = provider
    , Intent.picPaymentMethod = case provider of
        Checkout.ProviderPayPhone -> MethodPayPhoneWallet
        Checkout.ProviderPayPal -> MethodPayPalWallet
        Checkout.ProviderBankTransfer -> MethodManualBankTransfer
        _ -> MethodCard
    , Intent.picCaptureMethod = Intent.CaptureAutomatic
    , Intent.picAmountMinor = 100
    , Intent.picCurrency = "USD"
    , Intent.picIdempotencyKey = "expiry-" <> identifier
    , Intent.picOccurredAt = occurred
    , Intent.picCorrelationId = "expiry-" <> identifier
    }
