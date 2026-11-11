{-# LANGUAGE OverloadedStrings #-}

-- Actual PostgreSQL reader boundary; no HTTP requests, credentials or live data.
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, rawExecute, runSqlPool)
import Servant (runHandler, errHTTPCode)
import System.Environment (getEnv)
import Test.Hspec
import TDF.Commerce.CheckoutStore (CheckoutReference(..))
import TDF.DB (Env(..))
import TDF.Server.ServiceStorefront (loadDatafastCartParameters)

checkoutId :: Text
checkoutId = "01111111-1111-4111-8111-111111111111"

reset :: ConnectionPool -> Int64 -> Text -> IO ()
reset pool amount currency = flip runSqlPool pool $ do
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

main :: IO ()
main = do
  connection <- getEnv "TDF_DATAFAST_CART_TEST_DB"
  runNoLoggingT $ withPostgresqlPool (BS.pack connection) 1 $ \pool -> liftIO $ do
    let readCart amount currency = runHandler $ runReaderT
          (loadDatafastCartParameters (CheckoutReference checkoutId) amount currency)
          (Env pool (error "Cart must not read unrelated configuration"))
        refused amount currency = do
          result <- readCart amount currency
          case result of
            Left failure -> errHTTPCode failure `shouldBe` 409
            Right _ -> expectationFailure "Invalid canonical Dataweb cart was admitted"
    hspec $ describe "Dataweb canonical cart PostgreSQL reader" $ do
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
