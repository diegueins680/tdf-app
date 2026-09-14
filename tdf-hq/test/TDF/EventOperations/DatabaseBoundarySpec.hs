{-# LANGUAGE OverloadedStrings #-}

module TDF.EventOperations.DatabaseBoundarySpec (spec) where

import Control.Exception (AsyncException(..), throwIO, toException)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (Single(..))
import Database.PostgreSQL.Simple (SqlError(..))
import Database.PostgreSQL.LibPQ (ExecStatus(FatalError))
import Test.Hspec
import Test.QuickCheck (property)

import TDF.EventOperations.DatabaseBoundary
import TDF.EventOperations.Types

spec :: Spec
spec = describe "event operations database privacy boundary" $ do
  it "does not log arbitrary database messages, details, hints or unknown SQLSTATE bytes" $
    property $ \payload ->
      let bytes = BS.pack (payload :: String)
          exception = toException (SqlError bytes FatalError bytes bytes bytes)
          category = if bytes `elem` ["40001", "40P01"] then "transaction_conflict" else "unavailable"
      in fmap databaseFailureLog (classifyDatabaseFailure exception)
          == Just (object ["event" .= ("event_operations_database_error" :: String),
                           "category" .= (category :: String)])

  it "classifies only allowlisted transaction failures without echoing their payload" $ do
    let failure state = toException (SqlError state FatalError "secret query" "secret contract" "secret token")
    map (classifyDatabaseFailure . failure) ["40001", "40P01"]
      `shouldBe` replicate 2 (Just TransactionConflict)
    databaseFailureLog TransactionConflict `shouldBe`
      object ["event" .= ("event_operations_database_error" :: String),
              "category" .= ("transaction_conflict" :: String)]

  it "normalizes synchronous failures without returning their message" $ do
    result <- tryDatabaseAction (throwIO (userError "password=private") :: IO ())
    result `shouldBe` Left DatabaseUnavailable

  it "preserves asynchronous cancellation" $
    (tryDatabaseAction (throwIO ThreadKilled) :: IO (Either DatabaseFailure ()))
      `shouldThrow` (== ThreadKilled)

  it "decodes exactly one matching, versioned snapshot" $ do
    let snapshot = EventOperationSnapshotDTO 10 Planning 4 Nothing ["event.read"] []
        raw = TE.decodeUtf8 (BL.toStrict (encode snapshot))
    decodeSnapshotRows 10 [Single (Just raw)] `shouldBe` Right (Just snapshot)
    decodeSnapshotRows 11 [Single (Just raw)] `shouldBe` Left SnapshotDecodeError

  it "maps SQL NULL to no snapshot, not an empty successful object" $
    decodeSnapshotRows 10 [Single Nothing] `shouldBe` Right Nothing

  it "rejects malformed, unknown-field, invalid-version and noncanonical responses" $ do
    let invalid =
          [ "{}"
          , "private diagnostic text"
          , "{\"eventId\":10,\"canonicalState\":\"finished\",\"version\":1,\"capabilities\":[],\"availableTransitions\":[]}"
          , "{\"eventId\":10,\"canonicalState\":\"planning\",\"version\":0,\"capabilities\":[],\"availableTransitions\":[]}"
          , "{\"eventId\":10,\"canonicalState\":\"planning\",\"version\":1,\"capabilities\":[],\"availableTransitions\":[],\"secret\":\"private\"}"
          ]
    map (decodeSnapshotRows 10 . pure . Single . Just) invalid
      `shouldBe` replicate (length invalid) (Left SnapshotDecodeError)
    decodeSnapshotRows 10 [] `shouldBe` Left SnapshotDecodeError
    decodeSnapshotRows 10 [Single Nothing, Single Nothing] `shouldBe` Left SnapshotDecodeError
