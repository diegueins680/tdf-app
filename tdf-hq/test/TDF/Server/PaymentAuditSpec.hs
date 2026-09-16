{-# LANGUAGE OverloadedStrings #-}
module TDF.Server.PaymentAuditSpec (spec) where

import Control.Monad (forM_)
import Control.Exception (bracket)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Either (isLeft)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
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
import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentRuntimeStore as Runtime

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
  fallbackSpec

-- This database is created only by the owned PostgreSQL test runner and uses
-- the real forward migrations, including immutable evidence and sync triggers.
fallbackSpec :: Spec
fallbackSpec = do
  database <- runIO (lookupEnv "TDF_PAYMENT_FALLBACK_DATABASE_URL")
  case database of
    Nothing -> pure ()
    Just url -> describe "payment-audit canonical decline fallback" $
      forM_
        [ ("declined capture", Checkout.ProviderPayPal, "paypal_declined", Right (), True)
        , ("invalid card", Checkout.ProviderDatafast, "800.100.151", Right (), True)
        , ("timeout", Checkout.ProviderPayPal, "paypal_capture_request", Right (), False)
        , ("unknown status", Checkout.ProviderPayPal, "paypal_unknown", Right (), False)
        , ("mismatched evidence", Checkout.ProviderPayPal, "paypal_declined", Left "order mismatch", False)
        ] $ \(label, provider, code, binding, permitsFallback) ->
        it ("allows switching only after authoritative no-charge evidence: " <> label) $
          bracket (makePool (BS.pack url)) destroyAllResources $ \pool -> do
            checkoutId <- toText <$> nextRandom
            now <- getCurrentTime
            let checkout = Checkout.CheckoutReference checkoutId
                creation selected operation = Checkout.PaymentAttemptCreation
                  { Checkout.pacCheckout = checkout
                  , Checkout.pacProvider = selected
                  , Checkout.pacEnvironment = Checkout.CheckoutSandbox
                  , Checkout.pacOperation = operation
                  , Checkout.pacAmountMinor = 2500
                  , Checkout.pacCurrency = "USD"
                  , Checkout.pacMerchantRef = "payment-audit-fixture"
                  , Checkout.pacIdempotencyKey = checkoutId <> Checkout.paymentProviderText selected
                  , Checkout.pacCreatedAt = now
                  , Checkout.pacCorrelationId = "audit:" <> checkoutId
                  }
                alternate = if provider == Checkout.ProviderPayPal
                  then Checkout.ProviderDatafast else Checkout.ProviderPayPal
            flip runSqlPool pool $ do
              rawExecute "UPDATE commerce_provider_account SET enabled=TRUE,status='ready',contract_status='approved',credential_status='validated',merchant_account_ref='payment-audit-fixture',verified_at=now(),verified_by=1 WHERE environment='sandbox' AND provider IN ('paypal','datafast')" []
              rawExecute "UPDATE commerce_provider_capability SET verification_status='sandbox_verified',verified_at=now() WHERE provider_account_id IN (SELECT id FROM commerce_provider_account WHERE environment='sandbox' AND provider IN ('paypal','datafast'))" []
              rawExecute "INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at) VALUES (?::uuid,'service_booking',?,'awaiting_payment','sandbox','USD',2500,2500,'audit@example.test','fixture-only',?,now()+interval '30 minutes')"
                [PersistText checkoutId, PersistText checkoutId, PersistText checkoutId]
            original <- runSqlPool (Runtime.beginPaymentAttempt (creation provider Checkout.OperationCreate)) pool >>= expectRight
            -- Separate capture attempt reproduces the stale create-attempt gate.
            attempt <- if provider == Checkout.ProviderPayPal
              then runSqlPool (Runtime.beginPaymentAttempt (creation provider Checkout.OperationCapture)) pool >>= expectRight
              else pure original
            runSqlPool (Checkout.recordPaymentProcessing checkout original provider "audit:processing" now) pool
            blockedBefore <- runSqlPool (Runtime.beginPaymentAttempt (creation alternate Checkout.OperationCreate)) pool
            blockedBefore `shouldSatisfy` isLeft
            runSqlPool (Runtime.recordProviderPaymentFailure checkout attempt provider code binding "audit:decline" now) pool
            -- The authoritative transition and history are idempotent.
            runSqlPool (Runtime.recordProviderPaymentFailure checkout attempt provider code binding "audit:decline" now) pool
            state <- flip runSqlPool pool $ rawSql
              "SELECT status FROM commerce_payment_intent WHERE checkout_id=?::uuid"
              [PersistText checkoutId]
            (state :: [Single Text]) `shouldBe` [Single (if permitsFallback then "failed" else "processing")]
            history <- flip runSqlPool pool $ rawSql
              "SELECT count(*) FROM commerce_payment_state_history history JOIN commerce_payment_intent intent ON intent.id=history.payment_intent_id WHERE intent.checkout_id=?::uuid AND history.to_status='failed'"
              [PersistText checkoutId]
            (history :: [Single Int64]) `shouldBe` [Single (if permitsFallback then 1 else 0)]
            switched <- runSqlPool (Runtime.beginPaymentAttempt (creation alternate Checkout.OperationCreate)) pool
            if permitsFallback then do
              nextAttempt <- expectRight switched
              runSqlPool (Checkout.recordPaymentProcessing checkout nextAttempt alternate "audit:fallback" now) pool
              runSqlPool (Runtime.recordProviderPaymentFailure checkout attempt provider code binding "audit:late-replay" now) pool
              checkoutState <- flip runSqlPool pool $ rawSql
                "SELECT status FROM commerce_checkout_session WHERE id=?::uuid" [PersistText checkoutId]
              (checkoutState :: [Single Text]) `shouldBe` [Single "processing"]
              oldAttempts <- flip runSqlPool pool $ rawSql
                "SELECT count(*) FROM commerce_payment_attempt WHERE checkout_id=?::uuid AND provider=? AND status <> 'failed'"
                [PersistText checkoutId, PersistText (Checkout.paymentProviderText provider)]
              (oldAttempts :: [Single Int64]) `shouldBe` [Single 0]
              replay <- runSqlPool (Runtime.beginPaymentAttempt (creation provider Checkout.OperationCreate)) pool
              replay `shouldSatisfy` isLeft
            else switched `shouldSatisfy` isLeft

expectRight :: Show e => Either e a -> IO a
expectRight = either (\problem -> expectationFailure (show problem) >> fail "Expected Right") pure
