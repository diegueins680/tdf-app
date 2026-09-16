{-# LANGUAGE OverloadedStrings #-}
module TDF.Server.PaymentAuditSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), rawExecute, rawSql, runSqlPool)
import Servant (ServerError(..))
import Servant.Server (runHandler)
import System.Environment (lookupEnv)
import Test.Hspec
import TDF.API.Types (MarketplaceCheckoutReq(..), MarketplaceOrderDTO(..))
import TDF.API.CommerceOperations (CommercePaymentIntentSummaryDTO(..))
import TDF.DB (Env(..), makePool)
import TDF.Server (checkoutCart, marketplaceSha256Text)
import TDF.Server.CommerceOperations (loadPaymentIntentSummaries)

spec :: Spec
spec = do
  database <- runIO (lookupEnv "TDF_PAYMENT_AUDIT_DATABASE_URL")
  case database of
    Nothing -> pure ()
    Just url -> describe "payment-audit PostgreSQL boundaries" $ do
      it "keeps sale and rental contact retries unpaid with bank transfers unavailable" $ do
        pool <- makePool (BS.pack url)
        let env = Env pool (error "Contact replay must not send mail or read provider credentials")
            buyer = "Audit Buyer" :: Text
            email = "audit@example.test" :: Text
            payload = MarketplaceCheckoutReq buyer email Nothing (Just "pickup") Nothing Nothing Nothing Nothing
            cart = "a5400000-0000-4000-8000-000000000010" :: Text
            digest = marketplaceSha256Text . TE.decodeUtf8 . BL.toStrict . encode $ object
              [ "cart_id" .= cart, "buyer_name" .= buyer, "buyer_email" .= email
              , "buyer_phone" .= (Nothing :: Maybe Text), "fulfillment_method" .= ("pickup" :: Text)
              , "shipping_address" .= mcrShippingAddress payload
              , "rental_terms_accepted" .= mcrRentalTermsAccepted payload
              , "identity_document_type" .= (Nothing :: Maybe Text)
              , "identity_document_last4" .= (Nothing :: Maybe Text)
              ]
        forM_ [("sale", "a5400000-0000-4000-8000-000000000001"),
               ("rental", "a5400000-0000-4000-8000-000000000002")] $ \(kind, key) -> do
          let idem = "payment-audit-contact-" <> kind
          flip runSqlPool pool $ do
            rawExecute "INSERT INTO marketplace_order(id,cart_id,buyer_name,buyer_email,total_usd_cents,currency,status) VALUES (?::uuid,?::uuid,?,?,2500,'USD','awaiting_payment')"
              [PersistText key,PersistText cart,PersistText buyer,PersistText email]
            rawExecute "INSERT INTO commerce_checkout_session VALUES (?::uuid,?,'awaiting_payment','sandbox')"
              [PersistText key,PersistText ("marketplace_" <> kind)]
            rawExecute "INSERT INTO marketplace_order_checkout_runtime VALUES (?::uuid,?::uuid,?,?,?,'pickup','awaiting_payment',now()+interval '15 minutes',NULL)"
              [PersistText key,PersistText key,PersistText idem,PersistText digest,PersistText kind]
          forM_ [1 :: Int,2] $ \_ -> do
            result <- runHandler (runReaderT (checkoutCart cart (Just idem) payload) env)
            case result of
              Left err -> expectationFailure ("Contact request failed: " <> show (errHTTPCode err))
              Right dto -> do
                moStatus dto `shouldBe` "awaiting_payment"
                moPaymentProvider dto `shouldBe` Nothing
                moManualPaymentStatus dto `shouldBe` Nothing
          counts <- flip runSqlPool pool $ rawSql
            "SELECT (SELECT count(*) FROM commerce_payment_attempt), (SELECT count(*) FROM commerce_payment_intent), (SELECT count(*) FROM commerce_manual_payment_evidence)" []
          (counts :: [(Single Int64,Single Int64,Single Int64)]) `shouldBe` [(Single 0,Single 0,Single 0)]
          flip runSqlPool pool $ rawExecute "INSERT INTO commerce_payment_attempt(id,checkout_id,provider,operation,status,updated_at) VALUES (?::uuid,?::uuid,'paypal','create','processing',now())" [PersistText key,PersistText key]
          blocked <- runHandler (runReaderT (checkoutCart cart (Just idem) payload) env)
          case blocked of
            Left err -> errHTTPCode err `shouldBe` 409
            Right _ -> expectationFailure "Pending online payment must block contact switching"
          flip runSqlPool pool $ rawExecute "DELETE FROM commerce_payment_attempt" []
      it "keeps sandbox and production canonical totals distinct" $ do
        pool <- makePool (BS.pack url)
        flip runSqlPool pool $ do
          rawExecute "INSERT INTO commerce_checkout_session VALUES ('a5400000-0000-4000-8000-000000000003','service_booking','paid','production')" []
          rawExecute "INSERT INTO commerce_payment_intent VALUES ('a5400000-0000-4000-8000-000000000001','captured','USD',5000,5000,5000,500),('a5400000-0000-4000-8000-000000000003','captured','USD',7000,7000,7000,0)" []
        rows <- runSqlPool loadPaymentIntentSummaries pool
        [(cpiEnvironment x,cpiCapturedMinor x,cpiRefundedMinor x) | x <- rows]
          `shouldBe` [("production",7000,0),("sandbox",5000,500)]
