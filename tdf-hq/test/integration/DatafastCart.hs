{-# LANGUAGE OverloadedStrings #-}

-- Actual PostgreSQL reader boundary; no HTTP requests, credentials or live data.
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Either (isLeft)
import Data.Text (Text)
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool)
import Servant (runHandler, errHTTPCode)
import System.Environment (getEnv)
import Test.Hspec
import TDF.Commerce.CheckoutStore (CheckoutReference(..), CheckoutEnvironment(..))
import TDF.DB (Env(..))
import TDF.Server.ServiceStorefront
  (loadDatafastCartParameters, validateBoundDatafastSuccessfulPayment,
   ServiceDatafastPaymentStatus(..))

checkoutId :: Text
checkoutId = "01111111-1111-4111-8111-111111111111"

reset :: ConnectionPool -> Int64 -> Text -> IO ()
reset pool amount currency = flip runSqlPool pool $ do
  rawExecute "DELETE FROM commerce_provider_binding" []
  rawExecute "DELETE FROM commerce_payment_attempt" []
  rawExecute "DELETE FROM commerce_checkout_line_item" []
  rawExecute "DELETE FROM commerce_checkout_session" []
  rawExecute
    "INSERT INTO commerce_checkout_session\
    \ (id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,total_minor,\
    \ customer_email,lookup_token_hash,idempotency_key,expires_at)\
    \ VALUES (?::uuid,'service_storefront','synthetic-order','awaiting_payment','sandbox',\
    \ ?,?,?,'synthetic@example.invalid','synthetic-token','synthetic-key',now()+interval '1 hour')"
    [PersistText checkoutId, PersistText currency, PersistInt64 amount, PersistInt64 amount]

line :: ConnectionPool -> Int -> Text -> Int -> Int64 -> Int64 -> Int64 -> Int64 -> IO ()
line pool number description quantity unit discount tax fee = flip runSqlPool pool $
  rawExecute
    "INSERT INTO commerce_checkout_line_item\
    \ (checkout_id,line_number,product_type,product_id,product_version,description,quantity,\
    \ unit_amount_minor,subtotal_minor,discount_minor,tax_minor,fee_minor,total_minor,snapshot)\
    \ VALUES (?::uuid,?,'synthetic','synthetic-product','v1',?,?,?,?,?,?,?,?,'{}'::jsonb)"
    [ PersistText checkoutId, PersistInt64 (fromIntegral number), PersistText description
    , PersistInt64 (fromIntegral quantity), PersistInt64 unit
    , PersistInt64 (fromIntegral quantity * unit), PersistInt64 discount, PersistInt64 tax
    , PersistInt64 fee, PersistInt64 (fromIntegral quantity * unit - discount + tax + fee)
    ]

attemptId :: Text
attemptId = "02222222-2222-4222-8222-222222222222"

otherAttemptId :: Text
otherAttemptId = "03333333-3333-4333-8333-333333333333"

boundCheckout :: ConnectionPool -> IO ()
boundCheckout pool = do
  reset pool 2000 "USD"
  flip runSqlPool pool $ do
    forM_ [attemptId, otherAttemptId] $ \reference -> rawExecute
      "INSERT INTO commerce_payment_attempt\
      \ (id,checkout_id,provider,environment,operation,status,amount_minor,currency,\
      \ merchant_account_ref,idempotency_key)\
      \ VALUES (?::uuid,?::uuid,'datafast','sandbox','create','requires_customer_action',\
      \ 2000,'USD','synthetic-merchant',?)"
      [PersistText reference, PersistText checkoutId, PersistText reference]
    rawExecute
      "INSERT INTO commerce_provider_binding\
      \ (payment_attempt_id,provider,environment,merchant_account_ref,resource_type,\
      \ provider_resource_id,provider_resource_path,merchant_reference,amount_minor,currency)\
      \ VALUES (?::uuid,'datafast','sandbox','synthetic-merchant','checkout',\
      \ 'synthetic-gateway-checkout','/v1/checkouts/synthetic-gateway-checkout/payment',\
      \ 'synthetic-order',2000,'USD')"
      [PersistText attemptId]

approved :: Text -> ServiceDatafastPaymentStatus
approved reference = ServiceDatafastPaymentStatus
  { sdfpsPaymentId = Just "synthetic-payment"
  , sdfpsAmount = Just "20.00"
  , sdfpsCurrency = Just "USD"
  , sdfpsMerchantTransactionId = Just reference
  , sdfpsResultCode = "000.100.112"
  }

main :: IO ()
main = do
  connection <- getEnv "TDF_DATAFAST_CART_TEST_DB"
  runNoLoggingT $ withPostgresqlPool (BS.pack connection) 1 $ \pool -> liftIO $ do
    names <- flip runSqlPool pool (rawSql "SELECT current_database()" [])
      :: IO [Single Text]
    unless (names == [Single "tdf_dataweb_cart_test"]) $
      fail "Dataweb reader tests require their dedicated disposable database"
    let readCart amount currency = runHandler $ runReaderT
          (loadDatafastCartParameters (CheckoutReference checkoutId) amount currency)
          (Env pool (error "Cart must not read unrelated configuration"))
        refused amount currency = do
          result <- readCart amount currency
          case result of
            Left failure -> errHTTPCode failure `shouldBe` 409
            Right _ -> expectationFailure "Invalid canonical Dataweb cart was admitted"
    let verify checkout environment merchant gateway amount currency status = runHandler $
          runReaderT
            (validateBoundDatafastSuccessfulPayment (CheckoutReference checkout)
              environment merchant gateway amount currency status)
            (Env pool (error "Reference verification must not read creation configuration"))
        verifySaved = verify checkoutId CheckoutSandbox "synthetic-merchant"
          "synthetic-gateway-checkout" 2000 "USD"
        rejected action = do
          result <- action
          case result of
            Right validation -> validation `shouldSatisfy` isLeft
            Left _ -> expectationFailure "Reference verification failed before its database assertion"
    hspec $ do
      describe "Dataweb canonical cart PostgreSQL reader" $ do
        it "reads the real total_minor column and immutable lines in line-number order" $ do
          reset pool 2000 "USD"
          line pool 2 "Reserva" 1 1000 0 0 0
          line pool 1 "Entrada" 2 500 0 0 0
          result <- readCart 2000 "USD"
          fmap (lookup "cart.items[0].name") result `shouldBe` Right (Just "Entrada")
          fmap (lookup "cart.items[1].price") result `shouldBe` Right (Just "10.00")
          fmap (lookup "customParameters[SHOPPER_VAL_BASE0]") result `shouldBe` Right Nothing

        it "rejects an expected amount or currency that differs from its saved parent" $ do
          reset pool 2000 "USD"
          line pool 1 "Entrada" 2 1000 0 0 0
          refused 1999 "USD"
          refused 2000 "EUR"

        it "does not synthesize a line when the checkout has no saved cart" $ do
          reset pool 2000 "USD"
          refused 2000 "USD"

        it "rejects a line sum inconsistent with the parent even if each line is valid" $ do
          reset pool 2000 "USD"
          line pool 1 "Entrada" 1 1999 0 0 0
          refused 2000 "USD"

        it "requires a real canonical parent" $ do
          reset pool 2000 "USD"
          flip runSqlPool pool $ rawExecute "DELETE FROM commerce_checkout_session" []
          refused 2000 "USD"

        it "does not reprice or absorb discounts, taxes and fees into a balancing item" $ do
          forM_ [(100,0,0),(0,100,0),(0,0,100),(100,100,0)] $ \(discount,tax,fee) -> do
            let amount = 2000 - discount + tax + fee
            reset pool amount "USD"
            line pool 1 "Entrada" 2 1000 discount tax fee
            refused (fromIntegral amount) "USD"
          forM_ [(100,0,0),(0,100,0),(0,0,100),(100,100,0)] $ \(discount,tax,fee) -> do
            reset pool 2000 "USD"
            line pool 1 "Entrada" 2 1000 0 0 0
            flip runSqlPool pool $ rawExecute
              "UPDATE commerce_checkout_session SET subtotal_minor=?, discount_minor=?,\
              \ tax_minor=?, fee_minor=? WHERE id=?::uuid"
              [ PersistInt64 (2000 + discount - tax - fee), PersistInt64 discount
              , PersistInt64 tax, PersistInt64 fee, PersistText checkoutId ]
            refused 2000 "USD"

        it "refuses an unsupported currency even when parent and request agree" $ do
          reset pool 2000 "EUR"
          line pool 1 "Entrada" 2 1000 0 0 0
          refused 2000 "EUR"

        it "does not rewrite provider-invalid product names from a current catalogue" $ do
          reset pool 2000 "USD"
          line pool 1 "Private & title" 2 1000 0 0 0
          refused 2000 "USD"

      describe "Dataweb bound transaction reference PostgreSQL verification" $ do
        it "accepts the UUID of the exact saved creation attempt" $ do
          boundCheckout pool
          result <- verifySaved (approved attemptId)
          fmap (either (const False) (const True)) result `shouldBe` Right True

        it "preserves the legacy domain reference on the exact same checkout" $ do
          boundCheckout pool
          result <- verifySaved (approved "synthetic-order")
          fmap (either (const False) (const True)) result `shouldBe` Right True

        it "rejects another real attempt on the same order and arbitrary references" $ do
          boundCheckout pool
          forM_ [otherAttemptId, checkoutId, "other-order", "", "unrelated"] $ \reference ->
            rejected (verifySaved (approved reference))
          rejected (verifySaved ((approved attemptId) { sdfpsMerchantTransactionId = Nothing }))

        it "requires the requested checkout, environment, merchant, resource, amount and currency" $ do
          boundCheckout pool
          rejected (verify otherAttemptId CheckoutSandbox "synthetic-merchant"
            "synthetic-gateway-checkout" 2000 "USD" (approved attemptId))
          rejected (verify checkoutId CheckoutProduction "synthetic-merchant"
            "synthetic-gateway-checkout" 2000 "USD" (approved attemptId))
          rejected (verify checkoutId CheckoutSandbox "other-merchant"
            "synthetic-gateway-checkout" 2000 "USD" (approved attemptId))
          rejected (verify checkoutId CheckoutSandbox "synthetic-merchant"
            "another-gateway-checkout" 2000 "USD" (approved attemptId))
          rejected (verify checkoutId CheckoutSandbox "synthetic-merchant"
            "synthetic-gateway-checkout" 1999 "USD" (approved attemptId))
          rejected (verify checkoutId CheckoutSandbox "synthetic-merchant"
            "synthetic-gateway-checkout" 2000 "EUR" (approved attemptId))

        it "refuses inconsistent immutable attempt and binding fields" $ do
          let alterations =
                [ "UPDATE commerce_payment_attempt SET provider='paypal'"
                , "UPDATE commerce_payment_attempt SET operation='capture'"
                , "UPDATE commerce_payment_attempt SET environment='production'"
                , "UPDATE commerce_payment_attempt SET merchant_account_ref='other-merchant'"
                , "UPDATE commerce_payment_attempt SET amount_minor=1999"
                , "UPDATE commerce_payment_attempt SET currency='EUR'"
                , "UPDATE commerce_provider_binding SET provider='paypal'"
                , "UPDATE commerce_provider_binding SET environment='production'"
                , "UPDATE commerce_provider_binding SET merchant_account_ref='other-merchant'"
                , "UPDATE commerce_provider_binding SET resource_type='payment'"
                , "UPDATE commerce_provider_binding SET merchant_reference='other-order'"
                , "UPDATE commerce_provider_binding SET amount_minor=1999"
                , "UPDATE commerce_provider_binding SET currency='EUR'"
                ]
          forM_ alterations $ \statement -> do
            boundCheckout pool
            flip runSqlPool pool $ rawExecute statement []
            rejected (verifySaved (approved attemptId))
            rejected (verifySaved (approved "synthetic-order"))

        it "retains exact provider amount, currency and nonempty payment ID checks" $ do
          boundCheckout pool
          forM_ [ (approved attemptId) { sdfpsAmount = Just "19.99" }
                , (approved attemptId) { sdfpsAmount = Nothing }
                , (approved attemptId) { sdfpsCurrency = Just "EUR" }
                , (approved attemptId) { sdfpsCurrency = Nothing }
                , (approved attemptId) { sdfpsPaymentId = Just "" }
                , (approved attemptId) { sdfpsPaymentId = Nothing }
                ] $ \status -> rejected (verifySaved status)

        it "never substitutes an unbound order reference when its binding is absent" $ do
          boundCheckout pool
          flip runSqlPool pool $ rawExecute "DELETE FROM commerce_provider_binding" []
          rejected (verifySaved (approved attemptId))
          rejected (verifySaved (approved "synthetic-order"))

        it "is read-only on replay and reconciliation and does not require a current cart" $ do
          boundCheckout pool
          flip runSqlPool pool $ rawExecute
            "UPDATE commerce_payment_attempt SET status='succeeded' WHERE id=?::uuid"
            [PersistText attemptId]
          forM_ [attemptId, "synthetic-order", attemptId] $ \reference -> do
            result <- verifySaved (approved reference)
            fmap (either (const False) (const True)) result `shouldBe` Right True
          rows <- flip runSqlPool pool $ rawSql
            "SELECT status FROM commerce_payment_attempt WHERE id=?::uuid"
            [PersistText attemptId]
          rows `shouldBe` [Single ("succeeded" :: Text)]
