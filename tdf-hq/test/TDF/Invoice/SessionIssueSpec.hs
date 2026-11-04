{-# LANGUAGE OverloadedStrings #-}

-- | PAY-INVOICE-ISSUE-001: idempotent session invoice issuance. Pure decisions
-- plus the reservation statements against a real (SQLite) database.
module TDF.Invoice.SessionIssueSpec (spec) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Maybe (isJust)
import qualified Data.Text as T
import           Data.Time (UTCTime (..), fromGregorian)
import qualified Data.UUID as UUID
import           Database.Persist (Entity (..), get, selectList)
import           Database.Persist.Sql (SqlPersistT, rawExecute)
import           Database.Persist.Sqlite (runSqlite)
import           Test.Hspec

import           TDF.DTO (GenerateSessionInvoiceLineReq (..), GenerateSessionInvoiceReq (..))
import           TDF.Invoice.SessionIssue
import qualified TDF.ModelsExtra as ME

type Db = SqlPersistT (NoLoggingT (ResourceT IO))

-- Same columns and unique key as 2026-11-04_session_invoice_issue.sql.
schema :: Db ()
schema = rawExecute
  "CREATE TABLE \"session_invoice_issue\" (\
  \\"id\" INTEGER PRIMARY KEY, \"session_id\" VARCHAR NOT NULL, \"idempotency_key\" VARCHAR NOT NULL,\
  \\"request_sha256\" VARCHAR NOT NULL, \"invoice_id\" INTEGER NULL, \"response\" VARCHAR NULL,\
  \\"created_at\" TIMESTAMP NOT NULL, \"completed_at\" TIMESTAMP NULL,\
  \CONSTRAINT \"unique_session_invoice_issue\" UNIQUE (\"session_id\", \"idempotency_key\"))" []

withDb :: Db a -> IO a
withDb action = runSqlite ":memory:" (schema >> action)

now :: UTCTime
now = UTCTime (fromGregorian 2026 11 4) 0

sessionA, sessionB :: ME.SessionId
sessionA = ME.SessionKey (UUID.fromWords 1 2 3 4)
sessionB = ME.SessionKey (UUID.fromWords 5 6 7 8)

key1, key2 :: T.Text
key1 = "issue-key-0000000000000001"
key2 = "issue-key-0000000000000002"

line :: Int -> GenerateSessionInvoiceLineReq
line cents = GenerateSessionInvoiceLineReq
  { gsilDescription = "Mezcla"
  , gsilQuantity = 1
  , gsilUnitCents = cents
  , gsilTaxBps = Just 1500
  , gsilServiceOrderId = Nothing
  , gsilPackagePurchaseId = Nothing
  , gsilSriCode = Nothing
  , gsilSriAuxiliaryCode = Nothing
  , gsilSriAdditionalInfo = Nothing
  , gsilSriIvaCode = Nothing
  }

request :: Int -> GenerateSessionInvoiceReq
request cents = GenerateSessionInvoiceReq
  { gsiCustomerId = Just 9
  , gsiCurrency = Just "USD"
  , gsiNumber = Nothing
  , gsiNotes = Nothing
  , gsiLineItems = [line cents]
  , gsiGenerateReceipt = Nothing
  , gsiIssueSri = Just True
  , gsiCertificatePassword = Nothing
  , gsiIdempotencyKey = Just key1
  }

expectReserved :: IssueReservation -> Db ME.SessionInvoiceIssueId
expectReserved (IssueReserved reservation) = pure reservation
expectReserved other = liftIO (expectationFailure ("expected a reservation, got " <> show other)) >> fail "stop"

spec :: Spec
spec = describe "Session invoice issuance idempotency (PAY-INVOICE-ISSUE-001)" $ do
  describe "validateIssueKey" $ do
    it "requires a key when an SRI document will be issued" $
      validateIssueKey True Nothing `shouldBe` Left "idempotencyKey is required when issuing an SRI invoice"
    it "keeps the key optional without SRI" $
      validateIssueKey False Nothing `shouldBe` Right Nothing
    it "accepts 16 to 128 visible ASCII characters and trims surrounding space" $ do
      validateIssueKey True (Just (" " <> key1 <> " ")) `shouldBe` Right (Just key1)
      validateIssueKey True (Just (T.replicate 128 "k")) `shouldBe` Right (Just (T.replicate 128 "k"))
    it "rejects short, long and non-visible keys even without SRI" $ do
      validateIssueKey False (Just "short") `shouldSatisfy` either (const True) (const False)
      validateIssueKey True (Just (T.replicate 129 "k")) `shouldSatisfy` either (const True) (const False)
      validateIssueKey True (Just "issue key with spaces") `shouldSatisfy` either (const True) (const False)
      validateIssueKey True (Just "clave-con-ñ-0000000") `shouldSatisfy` either (const True) (const False)

  describe "issueFingerprint" $ do
    it "ignores the key and the certificate password, which a retry may re-enter" $
      issueFingerprint "s" (request 1000)
        `shouldBe` issueFingerprint "s" ((request 1000) { gsiIdempotencyKey = Just key2, gsiCertificatePassword = Just "secret" })
    it "changes with the amount, the SRI choice and the session" $ do
      issueFingerprint "s" (request 1000) `shouldNotBe` issueFingerprint "s" (request 1001)
      issueFingerprint "s" (request 1000) `shouldNotBe` issueFingerprint "s" ((request 1000) { gsiIssueSri = Just False })
      issueFingerprint "s" (request 1000) `shouldNotBe` issueFingerprint "t" (request 1000)
    it "is a lowercase SHA-256 hex digest, as the table CHECK requires" $ do
      let digest = issueFingerprint "s" (request 1000)
      T.length digest `shouldBe` 64
      T.all (`elem` ("0123456789abcdef" :: String)) digest `shouldBe` True

  describe "decideIssueReplay" $ do
    it "is fresh without a reservation" $
      decideIssueReplay "f" Nothing `shouldBe` IssueFresh
    it "replays the stored response for the same request" $
      decideIssueReplay "f" (Just ("f", Just "{}")) `shouldBe` IssueReplayStored "{}"
    it "refuses an unanswered reservation instead of issuing again" $
      decideIssueReplay "f" (Just ("f", Nothing)) `shouldBe` IssueInProgress
    it "refuses a different request with the same key, answered or not" $ do
      decideIssueReplay "g" (Just ("f", Just "{}")) `shouldSatisfy` isConflict
      decideIssueReplay "g" (Just ("f", Nothing)) `shouldSatisfy` isConflict

  describe "reservation statements (SQLite)" $ do
    it "reserves once; a concurrent duplicate sees the reservation, not an error" $ withDb $ do
      _ <- reserveIssue now sessionA key1 "f" >>= expectReserved
      again <- reserveIssue now sessionA key1 "f"
      liftIO $ show again `shouldBe` show (IssueExisting IssueInProgress :: IssueReservation)
      rows <- selectList [] [] :: Db [Entity ME.SessionInvoiceIssue]
      liftIO $ length rows `shouldBe` 1

    it "replays the completed response and refuses a different request with the key" $ withDb $ do
      reservation <- reserveIssue now sessionA key1 "f" >>= expectReserved
      completeIssue now reservation Nothing "{\"ok\":true}"
      replay <- reserveIssue now sessionA key1 "f"
      liftIO $ show replay `shouldBe` show (IssueExisting (IssueReplayStored "{\"ok\":true}"))
      conflict <- reserveIssue now sessionA key1 "g"
      liftIO $ case conflict of
        IssueExisting decision -> decision `shouldSatisfy` isConflict
        other -> expectationFailure ("expected a conflict, got " <> show other)
      stored <- get reservation
      liftIO $ (ME.sessionInvoiceIssueCompletedAt <$> stored) `shouldSatisfy` maybe False isJust

    it "scopes keys per session" $ withDb $ do
      _ <- reserveIssue now sessionA key1 "f" >>= expectReserved
      _ <- reserveIssue now sessionB key1 "f" >>= expectReserved
      pure ()

    it "releases only an unanswered reservation, so a failed request can be retried with its key" $ withDb $ do
      pending <- reserveIssue now sessionA key1 "f" >>= expectReserved
      releaseIssue pending
      _ <- reserveIssue now sessionA key1 "f" >>= expectReserved
      done <- reserveIssue now sessionA key2 "f" >>= expectReserved
      completeIssue now done Nothing "{}"
      releaseIssue done
      kept <- reserveIssue now sessionA key2 "f"
      liftIO $ show kept `shouldBe` show (IssueExisting (IssueReplayStored "{}"))
  where
    isConflict (IssueConflict _) = True
    isConflict _ = False
