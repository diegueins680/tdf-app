{-# LANGUAGE OverloadedStrings #-}

module TDF.EventOperations.DatabaseBoundary
  ( DatabaseFailure(..)
  , SnapshotDecodeError(..)
  , classifyDatabaseFailure
  , databaseFailureLog
  , tryDatabaseAction
  , decodeSnapshotRows
  , loadSnapshot
  , decodeTaskRows
  , loadTask
  , decodeTaskWithRevisionRows
  , loadTaskWithRevision
  ) where

import Control.Exception
  ( Exception, SomeAsyncException, SomeException, fromException, throwIO, tryJust )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, eitherDecodeStrict', object, (.=))
import Data.Int (Int64)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql)
import Database.PostgreSQL.Simple (SqlError(..))

import TDF.EventOperations.Types

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

decodeTaskRows
  :: Int64 -> Int64 -> [Single (Maybe Text)] -> Either SnapshotDecodeError (Maybe EventOperationTaskDTO)
decodeTaskRows _ _ [Single Nothing] = Right Nothing
decodeTaskRows eventId activityId [Single (Just raw)] =
  case eitherDecodeStrict' (TE.encodeUtf8 raw) of
    Right task | validTask eventId activityId task -> Right (Just task)
    _ -> Left SnapshotDecodeError
decodeTaskRows _ _ _ = Left SnapshotDecodeError

validTask :: Int64 -> Int64 -> EventOperationTaskDTO -> Bool
validTask eventId activityId task =
      eotEventId task == eventId && eotActivityId task == activityId
      && all isSafePositiveInteger
        ([eotEventId task, eotActivityId task, eotVersion task] <> map eraPartyId (eotRaci task))
      && maybe True (isSafePositiveInteger . etpVersion) (eotPolicy task)
      && Set.size (Set.fromList [(eraPartyId a, eraRole a) | a <- eotRaci task]) == length (eotRaci task)
      && eotAccountabilityNeedsAttention task == needsAttention task
  where
    needsAttention value = maybe False etpRequiresAccountability (eotPolicy value)
      && (length (filter ((== RaciAccountable) . eraRole) (eotRaci value)) /= 1
          || not (any ((== RaciResponsible) . eraRole) (eotRaci value)))

loadTask :: Int64 -> Int64 -> Int64 -> SqlPersistT IO (Maybe EventOperationTaskDTO)
loadTask eventId activityId actorPartyId = do
  rows <- rawSql "SELECT event_operation_read_task(?, ?, ?)::text"
    [PersistInt64 eventId, PersistInt64 activityId, PersistInt64 actorPartyId]
  either (liftIO . throwIO) pure (decodeTaskRows eventId activityId rows)

decodeTaskWithRevisionRows :: Int64 -> Int64 -> [Single (Maybe Text)]
  -> Either SnapshotDecodeError (Maybe EventOperationTaskWithRevisionDTO)
decodeTaskWithRevisionRows _ _ [Single Nothing] = Right Nothing
decodeTaskWithRevisionRows eventId activityId [Single (Just raw)] =
  case eitherDecodeStrict' (TE.encodeUtf8 raw) of
    Right result | validTask eventId activityId (etrTask result) -> Right (Just result)
    _ -> Left SnapshotDecodeError
decodeTaskWithRevisionRows _ _ _ = Left SnapshotDecodeError

loadTaskWithRevision :: Int64 -> Int64 -> Int64 -> SqlPersistT IO (Maybe EventOperationTaskWithRevisionDTO)
loadTaskWithRevision eventId activityId actorPartyId = do
  rows <- rawSql "SELECT event_operation_read_task_with_revision(?, ?, ?)::text"
    [PersistInt64 eventId, PersistInt64 activityId, PersistInt64 actorPartyId]
  either (liftIO . throwIO) pure (decodeTaskWithRevisionRows eventId activityId rows)
