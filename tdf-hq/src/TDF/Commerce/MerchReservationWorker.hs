{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.MerchReservationWorker
  ( merchReservationWorkerTick
  , startMerchReservationWorker
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception.Safe (displayException, tryAny)
import           Control.Monad (forever, void)
import           Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool)
import           System.IO (hPutStrLn, stderr)

import           TDF.DB (Env(..))

-- The SQL function expires canonical checkout sessions. Its existing trigger
-- then releases all linked inventory reservations and updates the independent
-- payment state in the same transaction. The function is safe across replicas:
-- each eligible checkout can transition away from a nonterminal status once.
merchReservationWorkerTick :: Env -> IO Int
merchReservationWorkerTick Env{envPool} = do
  installed <- runSqlPool merchReservationCleanupInstalled envPool
  if not installed
    then pure 0
    else do
      rows <- runSqlPool
        (rawSql "SELECT merch_release_expired_reservations(now())" []
          :: SqlPersistT IO [Single Int])
        envPool
      case rows of
        [Single released] -> pure released
        _ -> fail "Merch reservation cleanup returned an ambiguous result"

merchReservationCleanupInstalled :: SqlPersistT IO Bool
merchReservationCleanupInstalled = do
  rows <- (rawSql
    "SELECT to_regprocedure('merch_release_expired_reservations(timestamp with time zone)') IS NOT NULL"
    [] :: SqlPersistT IO [Single Bool])
  pure (rows == [Single True])

startMerchReservationWorker :: Env -> IO ()
startMerchReservationWorker env = void (forkIO (workerLoop env))

workerLoop :: Env -> IO ()
workerLoop env = forever $ do
  result <- tryAny (merchReservationWorkerTick env)
  case result of
    Left err ->
      hPutStrLn stderr
        ("{\"component\":\"merch-reservation-worker\",\"level\":\"error\",\"message\":\"tick failed\",\"error\":\""
          <> redactLogValue (displayException err) <> "\"}")
    Right released
      | released > 0 ->
          putStrLn
            ("{\"component\":\"merch-reservation-worker\",\"level\":\"info\",\"expiredCheckouts\":"
              <> show released <> "}")
      | otherwise -> pure ()
  threadDelay (30 * 1000000)

redactLogValue :: String -> String
redactLogValue = take 500 . map replaceUnsafe
  where
    replaceUnsafe character
      | character `elem` ['\n', '\r', '\t', '"'] = ' '
      | otherwise = character
