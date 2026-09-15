{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.EventOperations.Server
  ( eventOperationsServer
  , eventOperationDomainError
  ) where

import Control.Monad (unless)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value(..), decodeStrict', encode, fromJSON, object, Result(..), (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool)
import Servant

import TDF.Auth (AuthedUser(..), withCurrentAuthSession)
import TDF.DB (Env(..))
import TDF.EventOperations.API (EventOperationsAPI)
import TDF.EventOperations.DatabaseBoundary
  ( databaseFailureLog, loadSnapshot, loadTask, loadTaskWithRevision, reassignRaci, tryDatabaseAction )
import qualified TDF.EventOperations.Types as EventOps

type EventOperationsM = ReaderT Env Handler

eventOperationsServer :: AuthedUser -> ServerT EventOperationsAPI EventOperationsM
eventOperationsServer user eventId =
       getEventOperationsSnapshot user eventId
  :<|> applyEventTransition user eventId
  :<|> getEventTask user eventId
  :<|> getEventTaskWithRevision user eventId
  :<|> reassignEventTaskRaci user eventId

reassignEventTaskRaci :: AuthedUser -> Int64 -> Int64 -> UUID.UUID
  -> EventOps.EventRaciReassignmentCommand
  -> EventOperationsM (Headers '[Header "Cache-Control" Text] EventOps.EventRaciReassignmentOutcomeDTO)
reassignEventTaskRaci user eventId activityId commandId command =
  action `catchError` (\failure -> throwError failure
    { errHeaders = ("Cache-Control", "private, no-store") : errHeaders failure })
  where
    action = do
      unless (all EventOps.isSafePositiveInteger [eventId, activityId]
        && EventOps.validRaciReassignmentCommand command) $
        throwError (eventOperationDomainError "invalid_request")
      requireEventOperationsEnabled
      result <- runEventOperationsSessionDb user $
        reassignRaci eventId activityId (fromSqlKey (auPartyId user)) commandId command
      outcome <- either (throwError . eventOperationDomainError) pure result
      pure (addHeader ("private, no-store" :: Text) outcome)

getEventTaskWithRevision :: AuthedUser -> Int64 -> Int64
  -> EventOperationsM (Headers '[Header "Cache-Control" Text] EventOps.EventOperationTaskWithRevisionDTO)
getEventTaskWithRevision user eventId activityId = do
  unless (all EventOps.isSafePositiveInteger [eventId, activityId]) $
    throwError (eventOperationDomainError "invalid_request")
  requireEventOperationsEnabled
  task <- runEventOperationsSessionDb user $
    loadTaskWithRevision eventId activityId (fromSqlKey (auPartyId user))
  value <- maybe (throwError (eventOperationDomainError "not_found")) pure task
  pure (addHeader ("private, no-store" :: Text) value)

getEventTask
  :: AuthedUser -> Int64 -> Int64
  -> EventOperationsM (Headers '[Header "Cache-Control" Text] EventOps.EventOperationTaskDTO)
getEventTask user eventId activityId = do
  unless (all EventOps.isSafePositiveInteger [eventId, activityId]) $
    throwError (eventOperationDomainError "invalid_request")
  requireEventOperationsEnabled
  task <- runEventOperationsSessionDb user $
    loadTask eventId activityId (fromSqlKey (auPartyId user))
  value <- maybe (throwError (eventOperationDomainError "not_found")) pure task
  pure (addHeader ("private, no-store" :: Text) value)

runEventOperationsDb :: SqlPersistT IO a -> EventOperationsM a
runEventOperationsDb action = do
  Env{envPool} <- ask
  result <- liftIO (tryDatabaseAction (runSqlPool action envPool))
  case result of
    Right value -> pure value
    Left failure -> do
      liftIO $ BL8.putStrLn $ encode (databaseFailureLog failure)
      throwError err503
        { errBody = encode (object ["code" .= ("event_operations_unavailable" :: Text)])
        , errHeaders = [("Content-Type", "application/json")]
        }

requireEventOperationsEnabled :: EventOperationsM ()
requireEventOperationsEnabled = do
  tableRows <- runEventOperationsDb $ rawSql
    "SELECT to_regclass('public.event_operation_feature_flag') IS NOT NULL"
    [] :: EventOperationsM [Single Bool]
  case tableRows of
    [Single True] -> pure ()
    _ -> throwError (eventOperationDomainError "feature_disabled")
  enabledRows <- runEventOperationsDb $ rawSql
    "SELECT enabled FROM event_operation_feature_flag WHERE feature_code = 'event.operations.api'"
    [] :: EventOperationsM [Single Bool]
  unless (enabledRows == [Single True]) $
    throwError (eventOperationDomainError "feature_disabled")

-- Authentication and event work use separate pool transactions. Recheck the bound session
-- and retain its row lock in the SAME transaction as the event snapshot/command/replay.
runEventOperationsSessionDb :: AuthedUser -> SqlPersistT IO a -> EventOperationsM a
runEventOperationsSessionDb user action = do
  result <- runEventOperationsDb (withCurrentAuthSession user action)
  maybe (throwError err401 { errBody = "Invalid or inactive token" }) pure result

getEventOperationsSnapshot
  :: AuthedUser
  -> Int64
  -> EventOperationsM EventOps.EventOperationSnapshotDTO
getEventOperationsSnapshot user eventId = do
  requireEventOperationsEnabled
  snapshot <- runEventOperationsSessionDb user (loadSnapshot eventId actorPartyId)
  maybe (throwError (eventOperationDomainError "not_found")) pure snapshot
  where
    actorPartyId = fromSqlKey (auPartyId user)

applyEventTransition
  :: AuthedUser
  -> Int64
  -> UUID.UUID
  -> EventOps.EventTransitionCommand
  -> EventOperationsM EventOps.EventTransitionOutcomeDTO
applyEventTransition user eventId commandId command = do
  requireEventOperationsEnabled
  responseRows <- runEventOperationsSessionDb user $ rawSql
    "SELECT event_operation_apply_transition(?, ?, ?::uuid, ?, ?, ?, ?, ?)::text"
    [ PersistInt64 eventId
    , PersistInt64 actorPartyId
    , PersistText (UUID.toText commandId)
    , PersistInt64 (EventOps.etcExpectedVersion command)
    , PersistText (EventOps.eventLifecycleStateText (EventOps.etcTargetState command))
    , maybe PersistNull PersistText (EventOps.etcReason command)
    , PersistText (EventOps.etcCorrelationId command)
    , PersistText requestHash
    ] :: EventOperationsM [Single Text]
  responseValue <- case responseRows of
    [Single rawResponse] ->
      maybe
        (throwError (eventOperationDomainError "invalid_database_response"))
        pure
        (decodeStrict' (TE.encodeUtf8 rawResponse))
    _ -> throwError (eventOperationDomainError "invalid_database_response")
  case eventOperationErrorCode responseValue of
    Just errorCode -> throwError (eventOperationDomainError errorCode)
    Nothing -> case fromJSON responseValue of
      Success outcome -> pure outcome
      Error _ -> throwError (eventOperationDomainError "invalid_database_response")
  where
    actorPartyId = fromSqlKey (auPartyId user)
    requestHash = T.pack $ show
      (hash (BL.toStrict (encode command)) :: Digest SHA256)

eventOperationErrorCode :: Value -> Maybe Text
eventOperationErrorCode (Object fields) = case KeyMap.lookup "error" fields of
  Just (String errorCode) -> Just errorCode
  _ -> Nothing
eventOperationErrorCode _ = Nothing

eventOperationDomainError :: Text -> ServerError
eventOperationDomainError errorCode =
  (statusFor errorCode)
    { errBody = encode (object ["code" .= errorCode])
    , errHeaders = [("Content-Type", "application/json")]
    }
  where
    statusFor "feature_disabled" = err404
    statusFor "not_found" = err404
    statusFor "forbidden" = err403
    statusFor "invalid_request" = err400
    statusFor "reason_required" = err400
    statusFor "idempotency_conflict" = err409
    statusFor "version_conflict" = err409
    statusFor "transition_invalid" = err409
    statusFor "transition_effects_not_ready" = err409
    statusFor "operation_not_ready" = err409
    statusFor "assignment_not_replaceable" = err409
    statusFor "assignee_unavailable" = err409
    statusFor "assignment_conflict" = err409
    statusFor "accountability_not_ready" = err409
    statusFor "separation_of_duties" = err409
    statusFor _ = err500
