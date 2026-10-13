{-# LANGUAGE OverloadedStrings #-}
module TDF.ReadinessSpec (spec, main) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Pool (destroyAllResources)
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Control.Exception (AsyncException(..), Exception, SomeException, bracket, fromException, throwIO, try)
import System.Timeout (timeout)
import Test.Hspec
import TDF.App.Readiness (databaseReady, probeUntil, readinessAttempts, withinReadinessDeadline)

data PrivateFailure = PrivateFailure
instance Show PrivateFailure where show _ = error "PRIVATE_EXCEPTION_MUST_NOT_RENDER"
instance Exception PrivateFailure

main :: IO ()
main = hspec spec

-- A probe whose first `stale` calls fail like a pooled connection PostgreSQL
-- has closed; later calls answer from a live connection.
staleThen :: Int -> IO Bool -> IO (IO Bool, IO Int)
staleThen stale live = do
  calls <- newIORef (0 :: Int)
  let probe = do
        call <- atomicModifyIORef' calls (\n -> (n + 1, n + 1))
        if call <= stale then throwIO PrivateFailure else live
  pure (probe, readIORef calls)

spec :: Spec
spec = describe "database readiness boundary" $ do
  it "accepts only a successful probe" $
    withinReadinessDeadline 1000000 (pure True) `shouldReturn` True
  it "rejects a negative probe" $
    withinReadinessDeadline 1000000 (pure False) `shouldReturn` False
  it "rejects synchronous database errors without rendering private detail" $
    withinReadinessDeadline 1000000 (throwIO PrivateFailure) `shouldReturn` False
  it "includes unavailable pool acquisition in the failure boundary" $
    databaseReady (error "PRIVATE_POOL_DETAILS") `shouldReturn` False
  it "budgets eleven attempts (pool size ten plus one; bound to TDF.DB by verify-readiness.py)" $
    readinessAttempts `shouldBe` 11
  it "discards stale pooled connections and answers from the next live one" $ do
    (probe, calls) <- staleThen 2 (pure True)
    probeUntil 3 probe `shouldReturn` True
    calls `shouldReturn` 3
  it "spends at most its attempts on an unavailable database" $ do
    (probe, calls) <- staleThen 10 (pure True)
    probeUntil 3 probe `shouldReturn` False
    calls `shouldReturn` 3
  it "keeps a negative probe result final" $ do
    (probe, calls) <- staleThen 1 (pure False)
    probeUntil 3 probe `shouldReturn` False
    calls `shouldReturn` 2
  it "makes no attempt without an attempt budget" $ do
    (probe, calls) <- staleThen 0 (pure True)
    probeUntil 0 probe `shouldReturn` False
    calls `shouldReturn` 0
  -- Before the real-pool example: a retry that swallowed cancellation would
  -- block that example forever (the verifier runs controls with --fail-fast).
  it "propagates cancellation through a retry instead of spending attempts on it" $ do
    calls <- newIORef (0 :: Int)
    let probe = atomicModifyIORef' calls (\n -> (n + 1, ())) >> throwIO ThreadKilled
    result <- try (probeUntil 3 probe) :: IO (Either SomeException Bool)
    case result of
      Left caught -> fromException caught `shouldBe` Just ThreadKilled
      Right _ -> expectationFailure "Cancellation became a readiness result"
    readIORef calls `shouldReturn` 1
  it "times out an exhausted real pool and recovers after its connection is released" $
    bracket (runNoLoggingT (createSqlitePool ":memory:" 1)) destroyAllResources $ \pool -> do
      acquired <- newEmptyMVar
      release <- newEmptyMVar
      withAsync (runSqlPool (liftIO (putMVar acquired () >> takeMVar release)) pool) $ \holder -> do
        takeMVar acquired
        timeout 5000000 (databaseReady pool) `shouldReturn` Just False
        putMVar release ()
        wait holder
        databaseReady pool `shouldReturn` True
  it "rejects work that exceeds its deadline" $
    withinReadinessDeadline 1000 (threadDelay 100000 >> pure True) `shouldReturn` False
  it "rejects a zero deadline" $
    withinReadinessDeadline 0 (pure True) `shouldReturn` False
  mapM_ (\exception -> it ("propagates " <> show exception) $ do
    result <- try (withinReadinessDeadline 1000000 (throwIO exception)) :: IO (Either SomeException Bool)
    case result of
      Left caught -> fromException caught `shouldBe` Just exception
      Right _ -> expectationFailure "Cancellation became a readiness result") [ThreadKilled, UserInterrupt]
