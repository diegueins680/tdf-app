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
  , decodeRaciReassignmentRows
  , reassignRaci
  ) where

import Control.Exception
  ( Exception, SomeAsyncException, SomeException, fromException, throwIO, tryJust )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), Result(..), fromJSON, eitherDecodeStrict', object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
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

decodeRaciReassignmentRows :: Int64 -> Int64 -> UUID.UUID -> EventRaciReassignmentCommand
  -> [Single (Maybe Text)]
  -> Either SnapshotDecodeError (Either Text EventRaciReassignmentOutcomeDTO)
decodeRaciReassignmentRows eventId activityId commandId command [Single (Just raw)] =
  case eitherDecodeStrict' (TE.encodeUtf8 raw) of
    Right value@(Object fields) -> case KM.toList fields of
      [("error", String code)] | code `elem` allowedErrors -> Right (Left code)
      _ -> case fromJSON value of
        Success result | validRaciReassignmentCommand command
          && all isSafePositiveInteger [eventId, activityId]
          && eroEventId result == eventId && eroActivityId result == activityId
          && eroCommandId result == commandId && eroRole result == ercRole command
          && eroFromPartyId result == ercFromPartyId command
          && eroToPartyId result == ercToPartyId command
          && aggregateRevisionInteger (eroAggregateRevision result)
             == aggregateRevisionInteger (ercExpectedRevision command) + 2 -> Right (Right result)
        _ -> Left SnapshotDecodeError
    _ -> Left SnapshotDecodeError
  where
    allowedErrors = ["invalid_request", "feature_disabled", "not_found", "forbidden",
      "version_conflict", "idempotency_conflict", "operation_not_ready", "assignment_not_replaceable",
      "assignee_unavailable", "assignment_conflict", "accountability_not_ready"]
decodeRaciReassignmentRows _ _ _ _ _ = Left SnapshotDecodeError

-- Decoding is deliberately inside SqlPersistT, before runSqlPool can commit.
reassignRaci :: Int64 -> Int64 -> Int64 -> UUID.UUID -> EventRaciReassignmentCommand
  -> SqlPersistT IO (Either Text EventRaciReassignmentOutcomeDTO)
reassignRaci eventId activityId actorPartyId commandId command = do
  rows <- rawSql "SELECT event_operation_reassign_raci(?, ?, ?, ?::uuid, ?::bigint, ?, ?, ?, ?, ?)::text"
    [PersistInt64 eventId, PersistInt64 activityId, PersistInt64 actorPartyId,
     PersistText (UUID.toText commandId),
     PersistText (T.pack (show (aggregateRevisionInteger (ercExpectedRevision command)))),
     PersistText (raciRoleText (ercRole command)), PersistInt64 (ercFromPartyId command),
     PersistInt64 (ercToPartyId command), PersistText (ercReason command), PersistText (ercCorrelationId command)]
  either (liftIO . throwIO) pure (decodeRaciReassignmentRows eventId activityId commandId command rows)
