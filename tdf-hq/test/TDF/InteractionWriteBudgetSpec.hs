{-# LANGUAGE OverloadedStrings #-}
module TDF.InteractionWriteBudgetSpec (spec) where

import Control.Concurrent.Async (forConcurrently)
import Control.Monad (forM, replicateM)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistValue(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool)
import Database.PostgreSQL.Simple (SqlError(..))
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Test.Hspec
import TDF.Interactions.Server (ChargedWrite(..), chargedInteractionWrite)

-- Exercises the charge and the write it guards against the migrated
-- interaction_consume_write_budget function. Accounts are synthetic numbers:
-- the counter is keyed by a hash and has no reference to party.
spec :: Spec
spec = describe "interaction-write-budget-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_INTERACTION_BUDGET_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  used <- runIO (newIORef [])
  let newActor = newActorIn used
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-interaction-write-budget.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 10)) $
      afterAll (\pool -> forget used pool >> destroyAllResources pool) $ do
        it "keeps the charge when the write is refused by the database" $ \pool -> do
          actor <- newActor
          outcome <- runSqlPool (chargedInteractionWrite actor unstorable [nulPayload]) pool
          failureState outcome `shouldBe` Just "22P05"
          charged pool actor `shouldReturn` 1

        it "undoes what a refused write had done and leaves the transaction usable" $ \pool -> do
          actor <- newActor
          marker <- newActor
          (outcome, after) <- runSqlPool (do
            failed <- chargedInteractionWrite actor writesThenFails [PersistText (T.pack (show marker))]
            alive <- rawSql "SELECT 1" []
            pure (failed, alive)) pool
          failureState outcome `shouldBe` Just "22012"
          after `shouldBe` [Single (1 :: Int)]
          charged pool marker `shouldReturn` 0
          charged pool actor `shouldReturn` 1

        it "charges a write that succeeds exactly once" $ \pool -> do
          actor <- newActor
          outcome <- runSqlPool (chargedInteractionWrite actor "SELECT 'ok'::text" []) pool
          answered outcome `shouldBe` Just ["ok"]
          charged pool actor `shouldReturn` 1

        it "runs attempt 90, refuses attempt 91 without running it and still counts it" $ \pool -> do
          actor <- newActor
          marker <- newActor
          -- One transaction keeps every attempt inside one calendar minute.
          (first90, last91) <- runSqlPool (do
            allowed <- replicateM 90 (chargedInteractionWrite actor unstorable [nulPayload])
            refused <- chargedInteractionWrite actor writesMarker [PersistText (T.pack (show marker))]
            pure (allowed, refused)) pool
          map failureState first90 `shouldBe` replicate 90 (Just "22P05")
          limited last91 `shouldBe` True
          charged pool marker `shouldReturn` 0
          charged pool actor `shouldReturn` 91

        it "counts every one of several simultaneous refused writes" $ \pool -> do
          actor <- newActor
          outcomes <- forConcurrently [1 .. 8 :: Int] $ \_ ->
            runSqlPool (chargedInteractionWrite actor unstorable [nulPayload]) pool
          map failureState outcomes `shouldBe` replicate 8 (Just "22P05")
          charged pool actor `shouldReturn` 8

        it "keeps accounts apart" $ \pool -> do
          actors <- replicateM 2 newActor
          _ <- forM actors $ \actor ->
            runSqlPool (chargedInteractionWrite actor unstorable [nulPayload]) pool
          mapM (charged pool) actors `shouldReturn` [1, 1]

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://localhost/", "postgresql://127.0.0.1:",
   "postgresql://postgres:postgres@postgres:5432/"]

actorFloor, actorCeiling :: Int64
actorFloor = 7300000000
actorCeiling = 7399999999

newActorIn :: IORef [Int64] -> IO Int64
newActorIn used = do
  actor <- randomRIO (actorFloor, actorCeiling)
  modifyIORef' used (actor :)
  pure actor

-- A JSON string holding a NUL escape, which jsonb refuses to store.
nulPayload :: PersistValue
nulPayload = PersistText "{\"body\":\"\\u0000\"}"

unstorable :: Text
unstorable = "SELECT (?::jsonb)::text"

markerInsert :: Text
markerInsert = "INSERT INTO directory_rate_limit(scope,subject_hash,window_started_at,count,updated_at) \
  \VALUES('interaction:write',encode(digest(?::text,'sha256'),'hex'),date_trunc('minute',now()),1,now()) RETURNING count"

writesMarker :: Text
writesMarker = "WITH written AS (" <> markerInsert <> ") SELECT count::text FROM written"

writesThenFails :: Text
writesThenFails = "WITH written AS (" <> markerInsert <> ") SELECT (1/(count-1))::text FROM written"

failureState :: ChargedWrite -> Maybe BS.ByteString
failureState (WriteFailed err) = Just (sqlState err)
failureState _ = Nothing

answered :: ChargedWrite -> Maybe [Text]
answered (WriteAnswered rows) = Just [row | Single row <- rows]
answered _ = Nothing

limited :: ChargedWrite -> Bool
limited WriteLimited = True
limited _ = False

charged :: ConnectionPool -> Int64 -> IO Int64
charged pool actor = do
  rows <- runSqlPool (rawSql
    "SELECT coalesce(sum(count),0)::bigint FROM directory_rate_limit \
    \WHERE scope='interaction:write' AND subject_hash=encode(digest(?::text,'sha256'),'hex')"
    [PersistText (T.pack (show actor))]) pool
  case rows of
    [Single total] -> pure total
    _ -> fail "Write budget total was not a single row"

-- Removes only the counters of the synthetic accounts this run created.
forget :: IORef [Int64] -> ConnectionPool -> IO ()
forget used pool = do
  actors <- readIORef used
  runSqlPool (mapM_ (\actor -> rawExecute
    "DELETE FROM directory_rate_limit WHERE scope='interaction:write' \
    \AND subject_hash=encode(digest(?::text,'sha256'),'hex')"
    [PersistText (T.pack (show actor))]) actors) pool
