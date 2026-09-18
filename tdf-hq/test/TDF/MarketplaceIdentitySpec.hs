{-# LANGUAGE OverloadedStrings #-}
module TDF.MarketplaceIdentitySpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, void)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (ConnectionPool, Single(..), PersistValue(..), rawExecute, rawSql, runSqlPool)
import Database.Persist.Postgresql (createPostgresqlPool)
import System.Environment (lookupEnv)
import Test.Hspec
import Web.PathPieces (fromPathPiece)
import TDF.Server (MarketplacePaymentContext, loadMarketplacePaymentContext, submitMarketplaceManualEvidenceDb)

spec :: Spec
spec = describe "marketplace-contact-identity-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_MARKETPLACE_IDENTITY_TEST_DB")
  case configured of
    Nothing -> it "requires the isolated runner" $ pendingWith "Run scripts/test-marketplace-contact-identity.sh"
    Just connection | not ("dbname=tdf_marketplace_identity_test_" `isInfixOf` connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 6)) $ do
      it "does not create contacts when manual evidence has not been selected" $ \pool -> do
        context <- loadContext pool 1
        before <- contactCount pool
        submit pool context "REFERENCE-1" `shouldReturn` Left "Select bank transfer before submitting evidence"
        contactCount pool `shouldReturn` before
      it "serializes identical submissions and preserves the checkout identity on retries" $ \pool -> do
        context <- loadContext pool 2
        before <- contactCount pool
        boxes <- forM [1..5 :: Int] $ \_ -> do
          box <- newEmptyMVar
          void $ forkIO $ do
            result <- try (submit pool context "REFERENCE-2") :: IO (Either SomeException (Either Text ()))
            putMVar box result
          pure box
        outcomes <- mapM takeMVar boxes
        map (either (const False) (== Right ())) outcomes `shouldBe` replicate 5 True
        contactCount pool `shouldReturn` (before + 1)
        identities <- sqlScalar pool "SELECT count(*) FROM commerce_checkout_session c JOIN commerce_manual_payment_evidence e ON e.checkout_id=c.id JOIN party p ON p.id=c.customer_party_id WHERE c.id=?::uuid AND e.submitted_by=c.customer_party_id AND p.display_name <> 'Established synthetic account'" [PersistText (checkoutId 2)]
        identities `shouldBe` 1
        audits <- auditCount pool 2
        submit pool context "REFERENCE-2" `shouldReturn` Right ()
        contactCount pool `shouldReturn` (before + 1)
        auditCount pool 2 `shouldReturn` audits
        submit pool context "CHANGED-REFERENCE" `shouldReturn` Left "Different manual evidence is already under review"
        contactCount pool `shouldReturn` (before + 1)
      it "keeps the established contact when rejected evidence is corrected" $ \pool -> do
        context <- loadContext pool 3
        submit pool context "REFERENCE-3" `shouldReturn` Right ()
        before <- contactCount pool
        original <- sqlScalar pool "SELECT customer_party_id FROM commerce_checkout_session WHERE id=?::uuid" [PersistText (checkoutId 3)]
        runSqlPool (rawExecute "UPDATE commerce_manual_payment_evidence SET status='under_review',reviewed_by=(SELECT id FROM party WHERE display_name='Synthetic reviewer') WHERE checkout_id=?::uuid" [PersistText (checkoutId 3)]) pool
        runSqlPool (rawExecute "UPDATE commerce_manual_payment_evidence SET status='rejected',reviewed_by=(SELECT id FROM party WHERE display_name='Synthetic reviewer'),reviewed_at=now(),review_notes='Synthetic rejection' WHERE checkout_id=?::uuid" [PersistText (checkoutId 3)]) pool
        submit pool context "CORRECTED-REFERENCE-3" `shouldReturn` Right ()
        contactCount pool `shouldReturn` before
        sqlScalar pool "SELECT customer_party_id FROM commerce_checkout_session WHERE id=?::uuid" [PersistText (checkoutId 3)] `shouldReturn` original
        submit pool context "CORRECTED-REFERENCE-3" `shouldReturn` Right ()
        contactCount pool `shouldReturn` before
      it "rolls back a new contact if the dependent audit write fails" $ \pool -> do
        context <- loadContext pool 4
        before <- contactCount pool
        runSqlPool (rawExecute "CREATE FUNCTION marketplace_identity_fail() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.event_type='manual_payment_evidence_submitted' THEN RAISE EXCEPTION 'synthetic audit failure'; END IF; RETURN NEW; END $$; CREATE TRIGGER marketplace_identity_fail BEFORE INSERT ON commerce_checkout_audit_event FOR EACH ROW EXECUTE FUNCTION marketplace_identity_fail()" []) pool
        result <- try (submit pool context "REFERENCE-4") :: IO (Either SomeException (Either Text ()))
        either (const True) (const False) result `shouldBe` True
        runSqlPool (rawExecute "DROP TRIGGER marketplace_identity_fail ON commerce_checkout_audit_event; DROP FUNCTION marketplace_identity_fail()" []) pool
        contactCount pool `shouldReturn` before
        sqlScalar pool "SELECT count(*) FROM commerce_checkout_session WHERE id=?::uuid AND customer_party_id IS NULL" [PersistText (checkoutId 4)] `shouldReturn` 1
        submit pool context "REFERENCE-4" `shouldReturn` Right ()
        contactCount pool `shouldReturn` (before+1)
        sqlScalar pool "SELECT count(*) FROM commerce_payment_attempt WHERE status='succeeded'" [] `shouldReturn` 0

orderId, checkoutId :: Int -> Text
orderId n = "50000000-0000-4000-8000-" <> T.justifyRight 12 '0' (T.pack (show n))
checkoutId n = "60000000-0000-4000-8000-" <> T.justifyRight 12 '0' (T.pack (show n))

loadContext :: ConnectionPool -> Int -> IO MarketplacePaymentContext
loadContext pool n = do
  key <- maybe (fail "Invalid synthetic key") pure (fromPathPiece (orderId n))
  result <- runSqlPool (loadMarketplacePaymentContext key) pool
  either (fail . T.unpack) pure result

submit :: ConnectionPool -> MarketplacePaymentContext -> Text -> IO (Either Text ())
submit pool context reference = runSqlPool (submitMarketplaceManualEvidenceDb context reference) pool

sqlScalar :: ConnectionPool -> Text -> [PersistValue] -> IO Int64
sqlScalar pool query args = do
  rows <- runSqlPool (rawSql query args) pool
  case rows of
    [Single value] -> pure value
    _ -> fail "Missing scalar result"

contactCount :: ConnectionPool -> IO Int64
contactCount pool = sqlScalar pool "SELECT count(*) FROM party WHERE primary_email='manual-identity@example.test'" []
auditCount :: ConnectionPool -> Int -> IO Int64
auditCount pool n = sqlScalar pool "SELECT count(*) FROM commerce_checkout_audit_event WHERE checkout_id=?::uuid" [PersistText (checkoutId n)]
