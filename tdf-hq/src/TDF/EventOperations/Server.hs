{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.EventOperations.Server
  ( eventOperationsServer
  , eventOperationDomainError
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
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

import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.EventOperations.API (EventOperationsAPI)
import qualified TDF.EventOperations.Types as EventOps

type EventOperationsM = ReaderT Env Handler

eventOperationsServer :: AuthedUser -> ServerT EventOperationsAPI EventOperationsM
eventOperationsServer user eventId =
       getEventOperationsSnapshot user eventId
  :<|> applyEventTransition user eventId

runEventOperationsDb :: SqlPersistT IO a -> EventOperationsM a
runEventOperationsDb action = do
  Env{envPool} <- ask
  result <- liftIO (try (runSqlPool action envPool))
  case result of
    Right value -> pure value
    Left (failure :: SomeException) -> do
      liftIO $ BL8.putStrLn $ encode $ object
        [ "event" .= ("event_operations_database_error" :: Text)
        , "detail" .= displayException failure
        ]
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

getEventOperationsSnapshot
  :: AuthedUser
  -> Int64
  -> EventOperationsM EventOps.EventOperationSnapshotDTO
getEventOperationsSnapshot user eventId = do
  requireEventOperationsEnabled
  snapshot <- runEventOperationsDb (loadSnapshotDb eventId actorPartyId)
  maybe (throwError (eventOperationDomainError "not_found")) pure snapshot
  where
    actorPartyId = fromSqlKey (auPartyId user)

loadSnapshotDb
  :: Int64
  -> Int64
  -> SqlPersistT IO (Maybe EventOps.EventOperationSnapshotDTO)
loadSnapshotDb eventId actorPartyId = do
  stateRows <- rawSql
    "SELECT state.canonical_state, state.version, state.legacy_state_code \
    \FROM event_operation_event_state state \
    \JOIN event_operation_feature_flag flag \
    \  ON flag.feature_code = 'event.operations.api' AND flag.enabled \
    \WHERE state.event_id = ? \
    \  AND event_operation_actor_can_read(state.event_id, ?) \
    \FOR SHARE OF state"
    [PersistInt64 eventId, PersistInt64 actorPartyId]
      :: SqlPersistT IO [(Single Text, Single Int64, Single (Maybe Text))]
  case stateRows of
    [(Single stateText, Single version, Single legacyStateCode)] -> do
      capabilityRows <- rawSql
        "SELECT capability.scope_code \
        \FROM event_operation_actor_capabilities(?, ?) capability \
        \ORDER BY capability.scope_code"
        [PersistInt64 eventId, PersistInt64 actorPartyId]
          :: SqlPersistT IO [Single Text]
      transitionRows <- rawSql
        "SELECT policy.to_state \
        \FROM event_operation_lifecycle_transition_policy policy \
        \JOIN event_operation_transition_capability capability \
        \  ON capability.from_state = policy.from_state \
        \ AND capability.to_state = policy.to_state \
        \ AND capability.write_enabled \
        \WHERE policy.from_state = ? AND policy.active \
        \  AND event_operation_actor_has_authority(?, ?, policy.required_authority) \
        \ORDER BY policy.to_state"
        [PersistText stateText, PersistInt64 eventId, PersistInt64 actorPartyId]
          :: SqlPersistT IO [Single Text]
      case
        ( EventOps.parseEventLifecycleState stateText
        , traverse (EventOps.parseEventLifecycleState . unSingle) transitionRows
        ) of
          (Just canonicalState, Just availableTransitions) ->
            pure $ Just EventOps.EventOperationSnapshotDTO
              { EventOps.eosEventId = eventId
              , EventOps.eosCanonicalState = canonicalState
              , EventOps.eosVersion = version
              , EventOps.eosLegacyStateCode = legacyStateCode
              , EventOps.eosCapabilities = map unSingle capabilityRows
              , EventOps.eosAvailableTransitions = availableTransitions
              }
          _ -> pure Nothing
    _ -> pure Nothing

applyEventTransition
  :: AuthedUser
  -> Int64
  -> UUID.UUID
  -> EventOps.EventTransitionCommand
  -> EventOperationsM EventOps.EventTransitionOutcomeDTO
applyEventTransition user eventId commandId command = do
  requireEventOperationsEnabled
  responseRows <- runEventOperationsDb $ rawSql
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
    statusFor "separation_of_duties" = err409
    statusFor _ = err500
