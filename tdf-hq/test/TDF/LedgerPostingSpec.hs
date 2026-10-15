{-# LANGUAGE OverloadedStrings #-}
module TDF.LedgerPostingSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (finally, try)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import Database.PostgreSQL.Simple (SqlError(..))
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Test.Hspec

-- Posting rules of commerce_ledger_transaction against the fully migrated
-- synthetic database, exercised through plain SQL, the way every writer
-- (the Haskell stores, the deposit settlement function, an operator) reaches
-- them. Each statement runs in its own transaction unless the case says so.
spec :: Spec
spec = describe "ledger-posting-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_LEDGER_POSTING_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-ledger-posting.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 3)) $
      afterAll (\pool -> runSqlPool forget pool `finally` destroyAllResources pool) $ do

        it "posts a draft whose entries balance in every currency" $ \pool -> do
          txn <- draft pool
          addEntry pool txn "cash.spec" "USD" 500
          addEntry pool txn "revenue.spec" "USD" (-500)
          addEntry pool txn "cash.spec" "EUR" 7
          addEntry pool txn "revenue.spec" "EUR" (-7)
          post pool txn `shouldReturn` Right ()
          statusOf pool txn `shouldReturn` "posted"

        it "refuses to post an unbalanced draft, and one with no entries" $ \pool -> do
          txn <- draft pool
          post pool txn `shouldReturn` Left "has no entries"
          addEntry pool txn "cash.spec" "USD" 500
          addEntry pool txn "revenue.spec" "USD" (-499)
          post pool txn `shouldReturn` Left "is not balanced by currency"
          statusOf pool txn `shouldReturn` "draft"

        it "compares exact sums: amounts that wrap to zero in 64 bits are not balanced" $ \pool -> do
          txn <- draft pool
          addEntry pool txn "cash.spec" "USD" maxBound
          addEntry pool txn "cash.spec" "USD" maxBound
          addEntry pool txn "revenue.spec" "USD" 2
          post pool txn `shouldReturn` Left "is not balanced by currency"

        it "takes no entry once posted, nor an update, a deletion or a move of one" $ \pool -> do
          txn <- posted pool
          addEntryEither pool txn "cash.spec" "USD" 1 `shouldReturn` Left "Posted ledger entries are immutable"
          sqlEither pool "UPDATE commerce_ledger_entry SET amount_minor = amount_minor + 1 WHERE transaction_id = ?::uuid"
            [PersistText txn] `shouldReturn` Left "Posted ledger entries are immutable"
          sqlEither pool "DELETE FROM commerce_ledger_entry WHERE transaction_id = ?::uuid" [PersistText txn]
            `shouldReturn` Left "Posted ledger entries are immutable"
          entriesOf pool txn `shouldReturn` 2
          other <- draft pool
          addEntry pool other "cash.spec" "USD" 999
          sqlEither pool "UPDATE commerce_ledger_entry SET transaction_id = ?::uuid WHERE transaction_id = ?::uuid"
            [PersistText txn, PersistText other] `shouldReturn` Left "cannot move between transactions"
          balanceOf pool txn `shouldReturn` 0

        it "keeps a referenced reversing transaction posted" $ \pool -> do
          original <- posted pool
          reverser <- posted pool
          replacement <- posted pool
          reverseWith pool original reverser `shouldReturn` Right ()
          reverseWith pool reverser replacement
            `shouldReturn` Left "is a posted reverser and cannot be reversed"
          statusOf pool original `shouldReturn` "reversed"
          statusOf pool reverser `shouldReturn` "posted"

        it "does not return a posted transaction to draft" $ \pool -> do
          txn <- posted pool
          sqlEither pool "UPDATE commerce_ledger_transaction SET status = 'draft' WHERE id = ?::uuid" [PersistText txn]
            `shouldReturn` Left "is posted; create a reversing transaction"
          statusOf pool txn `shouldReturn` "posted"

        it "creates transactions only as draft" $ \pool -> do
          other <- posted pool
          insertWithStatus pool "posted" Nothing `shouldReturn` Left "must be created as draft"
          insertWithStatus pool "reversed" (Just other) `shouldReturn` Left "must be created as draft"

        it "reverses a posted transaction only by a posted one, and a reversed one never changes again" $ \pool -> do
          original <- posted pool
          reverser <- draft pool
          reverseWith pool original original `shouldReturn` Left "can only be reversed by a posted transaction"
          reverseWith pool original reverser `shouldReturn` Left "can only be reversed by a posted transaction"
          addEntry pool reverser "cash.spec" "USD" (-500)
          addEntry pool reverser "revenue.spec" "USD" 500
          post pool reverser `shouldReturn` Right ()
          reverseWith pool original reverser `shouldReturn` Right ()
          statusOf pool original `shouldReturn` "reversed"
          sqlEither pool "UPDATE commerce_ledger_transaction SET status = 'posted' WHERE id = ?::uuid" [PersistText original]
            `shouldReturn` Left "is reversed and cannot change status"
          sqlEither pool "UPDATE commerce_ledger_transaction SET reversal_of = ?::uuid WHERE id = ?::uuid" [PersistText original, PersistText original]
            `shouldReturn` Left "is reversed and cannot change status"
          draftOne <- draft pool
          reverseWith pool draftOne reverser `shouldReturn` Left "is a draft and has nothing to reverse"

        it "never leaves a posted transaction unbalanced when an entry and the posting race" $ \pool -> do
          txn <- draft pool
          addEntry pool txn "cash.spec" "USD" 500
          addEntry pool txn "revenue.spec" "USD" (-500)
          (entryOutcome, postOutcome) <- concurrently
            (addEntryEither pool txn "cash.spec" "USD" 1)
            (post pool txn)
          status <- statusOf pool txn
          balance <- balanceOf pool txn
          case (entryOutcome, postOutcome, status) of
            (Right (), Left _, "draft") -> balance `shouldBe` 1
            (Left _, Right (), "posted") -> balance `shouldBe` 0
            other -> expectationFailure ("Unexpected race outcome: " <> show other)

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres:postgres@postgres:5432/"]

sourcePrefix :: Text
sourcePrefix = "ledger-posting-spec."

-- A draft transaction with a synthetic source id, removed by 'forget'.
draft :: ConnectionPool -> IO Text
draft pool = do
  suffix <- randomRIO (100000000, 999999999 :: Int)
  rows <- runSqlPool (rawSql
    "INSERT INTO commerce_ledger_transaction(transaction_type, source_type, source_id, status, effective_at, correlation_id, created_by) \
    \VALUES ('payment_capture', 'ledger_posting_spec', ?, 'draft', now(), ?, 'ledger-posting-spec') RETURNING id::text"
    [PersistText (sourcePrefix <> T.pack (show suffix)), PersistText ("spec-" <> T.pack (show suffix))]) pool
  case rows of
    [Single txn] -> pure txn
    _ -> fail "The draft transaction was not created"

posted :: ConnectionPool -> IO Text
posted pool = do
  txn <- draft pool
  addEntry pool txn "cash.spec" "USD" 500
  addEntry pool txn "revenue.spec" "USD" (-500)
  outcome <- post pool txn
  outcome `shouldBe` Right ()
  pure txn

insertWithStatus :: ConnectionPool -> Text -> Maybe Text -> IO (Either Text ())
insertWithStatus pool status reversalOf = do
  suffix <- randomRIO (100000000, 999999999 :: Int)
  sqlEither pool
    "INSERT INTO commerce_ledger_transaction(transaction_type, source_type, source_id, status, effective_at, correlation_id, created_by, reversal_of) \
    \VALUES ('payment_capture', 'ledger_posting_spec', ?, ?, now(), ?, 'ledger-posting-spec', ?::uuid)"
    [ PersistText (sourcePrefix <> T.pack (show suffix)), PersistText status
    , PersistText ("spec-" <> T.pack (show suffix)), maybe PersistNull PersistText reversalOf ]

addEntry :: ConnectionPool -> Text -> Text -> Text -> Int64 -> IO ()
addEntry pool txn account currency amount = do
  outcome <- addEntryEither pool txn account currency amount
  outcome `shouldBe` Right ()

addEntryEither :: ConnectionPool -> Text -> Text -> Text -> Int64 -> IO (Either Text ())
addEntryEither pool txn account currency amount = sqlEither pool
  "INSERT INTO commerce_ledger_entry(transaction_id, account_code, currency, amount_minor) VALUES (?::uuid, ?, ?, ?)"
  [PersistText txn, PersistText account, PersistText currency, PersistInt64 amount]

post :: ConnectionPool -> Text -> IO (Either Text ())
post pool txn = sqlEither pool
  "UPDATE commerce_ledger_transaction SET status = 'posted' WHERE id = ?::uuid" [PersistText txn]

reverseWith :: ConnectionPool -> Text -> Text -> IO (Either Text ())
reverseWith pool txn reverser = sqlEither pool
  "UPDATE commerce_ledger_transaction SET status = 'reversed', reversal_of = ?::uuid WHERE id = ?::uuid"
  [PersistText reverser, PersistText txn]

statusOf :: ConnectionPool -> Text -> IO Text
statusOf pool txn = do
  rows <- runSqlPool (rawSql "SELECT status FROM commerce_ledger_transaction WHERE id = ?::uuid" [PersistText txn]) pool
  case rows of
    [Single status] -> pure status
    _ -> fail "The transaction was not found"

entriesOf :: ConnectionPool -> Text -> IO Int64
entriesOf pool txn = do
  rows <- runSqlPool (rawSql "SELECT count(*) FROM commerce_ledger_entry WHERE transaction_id = ?::uuid" [PersistText txn]) pool
  case rows of
    [Single n] -> pure n
    _ -> fail "The entry count was not a single row"

balanceOf :: ConnectionPool -> Text -> IO Int64
balanceOf pool txn = do
  rows <- runSqlPool (rawSql
    "SELECT COALESCE(SUM(amount_minor), 0)::bigint FROM commerce_ledger_entry WHERE transaction_id = ?::uuid" [PersistText txn]) pool
  case rows of
    [Single n] -> pure n
    _ -> fail "The balance was not a single row"

-- The statement's outcome: Right, or the server's message without the
-- transaction id, so cases can match the rule that refused it.
sqlEither :: ConnectionPool -> Text -> [PersistValue] -> IO (Either Text ())
sqlEither pool statement params = do
  outcome <- try (runSqlPool (rawExecute statement params :: SqlPersistT IO ()) pool)
  pure $ case outcome of
    Right () -> Right ()
    Left err -> Left (ruleOf (sqlErrorMsg err))
  where
    ruleOf msg =
      let text = T.pack (BS.unpack msg)
      in head ([rule | rule <- rules, rule `T.isInfixOf` text] ++ [text])
    rules =
      [ "has no entries", "is not balanced by currency", "Posted ledger entries are immutable"
      , "is posted; create a reversing transaction", "must be created as draft", "cannot move between transactions"
      , "can only be reversed by a posted transaction", "is reversed and cannot change status"
      , "is a draft and has nothing to reverse", "is a posted reverser and cannot be reversed" ]

-- Removes only the transactions this spec created.
forget :: SqlPersistT IO ()
forget = do
  rawExecute "ALTER TABLE commerce_ledger_entry DISABLE TRIGGER trg_commerce_ledger_entry_immutable" []
  rawExecute "DELETE FROM commerce_ledger_entry WHERE transaction_id IN (SELECT id FROM commerce_ledger_transaction WHERE source_type = 'ledger_posting_spec')" []
  rawExecute "ALTER TABLE commerce_ledger_entry ENABLE TRIGGER trg_commerce_ledger_entry_immutable" []
  -- Reversed rows first: they reference their reverser (ON DELETE RESTRICT)
  -- and their check constraint forbids clearing the reference.
  rawExecute "DELETE FROM commerce_ledger_transaction WHERE source_type = 'ledger_posting_spec' AND reversal_of IS NOT NULL" []
  rawExecute "DELETE FROM commerce_ledger_transaction WHERE source_type = 'ledger_posting_spec'" []
