{-# LANGUAGE OverloadedStrings #-}

module TDF.EventOperations.DatabaseBoundary
  ( DatabaseFailure(..)
  , SnapshotDecodeError(..)
  , classifyDatabaseFailure
  , databaseFailureLog
  , tryDatabaseAction
  , decodeSnapshotRows
  , loadSnapshot
  ) where

import Control.Exception
  ( Exception, SomeAsyncException, SomeException, fromException, throwIO, tryJust )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, eitherDecodeStrict', object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql)
import Database.PostgreSQL.Simple (SqlError(..))

import TDF.EventOperations.Types (EventOperationSnapshotDTO(..))

data DatabaseFailure = TransactionConflict | DatabaseUnavailable
  deriving (Eq, Show)

-- No raw response or decoder diagnostic is retained in this exception.
data SnapshotDecodeError = SnapshotDecodeError deriving (Eq, Show)
instance Exception SnapshotDecodeError

classifyDatabaseFailure :: SomeException -> Maybe DatabaseFailure
classifyDatabaseFailure failure = case fromException failure :: Maybe SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just $ case fromException failure :: Maybe SqlError of
    Just sqlFailure | sqlState sqlFailure `elem` ["40001", "40P01"] -> TransactionConflict
    _ -> DatabaseUnavailable

databaseFailureLog :: DatabaseFailure -> Value
databaseFailureLog failure = object
  [ "event" .= ("event_operations_database_error" :: Text)
  , "category" .= category
  ]
  where
    category :: Text
    category = case failure of
      TransactionConflict -> "transaction_conflict"
      DatabaseUnavailable -> "unavailable"

-- tryJust rethrows asynchronous cancellation, allowing pool/transaction cleanup.
tryDatabaseAction :: IO a -> IO (Either DatabaseFailure a)
tryDatabaseAction = tryJust classifyDatabaseFailure

decodeSnapshotRows
  :: Int64 -> [Single (Maybe Text)] -> Either SnapshotDecodeError (Maybe EventOperationSnapshotDTO)
decodeSnapshotRows _ [Single Nothing] = Right Nothing
decodeSnapshotRows expectedEventId [Single (Just raw)] =
  case eitherDecodeStrict' (TE.encodeUtf8 raw) of
    Right snapshot
      | eosEventId snapshot == expectedEventId && eosVersion snapshot > 0 -> Right (Just snapshot)
    _ -> Left SnapshotDecodeError
decodeSnapshotRows _ _ = Left SnapshotDecodeError

loadSnapshot :: Int64 -> Int64 -> SqlPersistT IO (Maybe EventOperationSnapshotDTO)
loadSnapshot eventId actorPartyId = do
  rows <- rawSql "SELECT event_operation_read_snapshot(?, ?)::text"
    [PersistInt64 eventId, PersistInt64 actorPartyId]
  either (liftIO . throwIO) pure (decodeSnapshotRows eventId rows)
