{-# LANGUAGE OverloadedStrings #-}

-- Focused Stack/runghc harness; optional integration is only against the disposable test DB.
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (runSqlPool)
import System.Environment (lookupEnv)
import Test.Hspec

import TDF.EventOperations.DatabaseBoundary (loadSnapshot)
import qualified TDF.EventOperations.DatabaseBoundarySpec as Boundary
import TDF.EventOperations.Types

main :: IO ()
main = do
  connection <- lookupEnv "EVENT_OPERATIONS_TEST_DSN"
  case connection of
    Nothing -> do
      putStrLn "PostgreSQL adapter integration not selected (EVENT_OPERATIONS_TEST_DSN absent)."
      hspec Boundary.spec
    Just dsn -> runNoLoggingT $ withPostgresqlPool (BS.pack dsn) 1 $ \pool -> liftIO $ hspec $ do
      Boundary.spec
      describe "event operations real PostgreSQL snapshot adapter" $ do
        it "decodes the existing canonical event projection" $ do
          result <- runSqlPool (loadSnapshot 10 1) pool
          fmap eosEventId result `shouldBe` Just 10
          fmap eosCanonicalState result `shouldBe` Just Planning
          fmap eosVersion result `shouldBe` Just 4
        it "does not disclose to an outsider or for a missing event" $ do
          runSqlPool (loadSnapshot 10 3) pool `shouldReturn` Nothing
          runSqlPool (loadSnapshot 999999 1) pool `shouldReturn` Nothing
        it "does not advertise approval to the review requester" $ do
          result <- runSqlPool (loadSnapshot 12 1) pool
          fmap (elem Approved . eosAvailableTransitions) result `shouldBe` Just False
