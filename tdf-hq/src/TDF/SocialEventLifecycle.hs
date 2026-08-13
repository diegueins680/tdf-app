{-# LANGUAGE OverloadedStrings #-}

module TDF.SocialEventLifecycle
  ( socialEventWorkflowCode
  , recognizedSocialEventStateCodes
  , recognizedSocialEventCapabilityCodes
  , resolveActiveSocialEventStateId
  , resolveInitialSocialEventStateId
  , resolveSocialEventStateCode
  , loadActiveSocialEventState
  , socialEventTransitionAllowed
  , socialEventStateHasCapability
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Database.Persist (PersistValue (PersistText), toPersistValue)
import Database.Persist.Sql (Single (..), SqlPersistT, rawSql)
import TDF.UUIDInstances ()

socialEventWorkflowCode :: Text
socialEventWorkflowCode = "social-event-lifecycle"

-- Stable parser/dispatcher identifiers recognized by the executable. Names,
-- translations, ordering, defaults, transitions, capabilities, and active
-- state remain database-authoritative.
recognizedSocialEventStateCodes :: [Text]
recognizedSocialEventStateCodes =
  [ "planning"
  , "announced"
  , "on_sale"
  , "live"
  , "postponed"
  , "unavailable"
  , "out_of_scope"
  , "completed"
  , "cancelled"
  ]

-- These codes are executable authorization/behaviour boundaries. Their
-- assignment to states remains persisted and may change without a release.
recognizedSocialEventCapabilityCodes :: [Text]
recognizedSocialEventCapabilityCodes =
  [ "public-listable"
  , "ticket-purchase"
  ]

resolveActiveSocialEventStateId :: Text -> SqlPersistT IO UUID
resolveActiveSocialEventStateId rawCode = do
  let stateCode = T.toLower (T.strip rawCode)
  rows <- rawSql
    "SELECT state.id FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code=? AND workflow.active=TRUE AND state.code=? AND state.active=TRUE ORDER BY state.id LIMIT 2"
    [PersistText socialEventWorkflowCode, PersistText stateCode]
  case rows of
    [Single identifier] -> pure identifier
    [] -> lifecycleFailure $ "Unknown or inactive social-event workflow state: " <> stateCode
    _ -> lifecycleFailure $ "Ambiguous social-event workflow state: " <> stateCode

resolveInitialSocialEventStateId :: SqlPersistT IO UUID
resolveInitialSocialEventStateId = do
  rows <- rawSql
    "SELECT state.id FROM workflow_default_state default_state JOIN workflow_definition workflow ON workflow.id=default_state.workflow_id JOIN workflow_state state ON state.id=default_state.state_id AND state.workflow_id=workflow.id WHERE workflow.code=? AND workflow.active=TRUE AND default_state.context='initial' AND default_state.active=TRUE AND state.active=TRUE ORDER BY default_state.id LIMIT 2"
    [PersistText socialEventWorkflowCode]
  case rows of
    [Single identifier] -> pure identifier
    [] -> lifecycleFailure "The social-event workflow has no active initial state"
    _ -> lifecycleFailure "The social-event workflow has multiple active initial states"

resolveSocialEventStateCode :: UUID -> SqlPersistT IO Text
resolveSocialEventStateCode stateId = do
  rows <- rawSql
    "SELECT state.code FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE state.id=? AND workflow.code=? AND workflow.active=TRUE AND state.active=TRUE ORDER BY state.id LIMIT 2"
    [toPersistValue stateId, PersistText socialEventWorkflowCode]
  case rows of
    [Single stateCode] -> pure stateCode
    [] -> lifecycleFailure "Social event references an unknown or inactive workflow state"
    _ -> lifecycleFailure "Social event workflow state identity is ambiguous"

loadActiveSocialEventState :: UUID -> SqlPersistT IO (Maybe (Text, Text, Text))
loadActiveSocialEventState stateId = do
  rows <- rawSql
    "SELECT state.code, state.name_es, state.name_en FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE state.id=? AND workflow.code=? AND workflow.active=TRUE AND state.active=TRUE ORDER BY state.id LIMIT 2"
    [toPersistValue stateId, PersistText socialEventWorkflowCode]
  case rows of
    [(Single stateCode, Single nameEs, Single nameEn)] ->
      pure (Just (stateCode, nameEs, nameEn))
    _ -> pure Nothing

socialEventTransitionAllowed :: UUID -> UUID -> SqlPersistT IO Bool
socialEventTransitionAllowed fromStateId toStateId
  | fromStateId == toStateId = pure True
  | otherwise = do
      rows <- rawSql
        "SELECT COUNT(*) FROM workflow_transition transition JOIN workflow_definition workflow ON workflow.id=transition.workflow_id JOIN workflow_state from_state ON from_state.id=transition.from_state_id AND from_state.workflow_id=workflow.id JOIN workflow_state to_state ON to_state.id=transition.to_state_id AND to_state.workflow_id=workflow.id WHERE workflow.code=? AND workflow.active=TRUE AND transition.from_state_id=? AND transition.to_state_id=? AND transition.active=TRUE AND transition.required_permission_id IS NULL AND NOT transition.requires_review AND NOT transition.requires_distinct_approver AND from_state.active=TRUE AND to_state.active=TRUE AND (transition.effective_from IS NULL OR transition.effective_from<=CURRENT_TIMESTAMP) AND (transition.effective_until IS NULL OR transition.effective_until>CURRENT_TIMESTAMP)"
        [ PersistText socialEventWorkflowCode
        , toPersistValue fromStateId
        , toPersistValue toStateId
        ]
      pure $ rows == [Single (1 :: Int)]

socialEventStateHasCapability :: UUID -> Text -> SqlPersistT IO Bool
socialEventStateHasCapability stateId rawCapability = do
  rows <- rawSql
    "SELECT COUNT(*) FROM workflow_state_capability capability JOIN workflow_state state ON state.id=capability.state_id JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE state.id=? AND workflow.code=? AND workflow.active=TRUE AND state.active=TRUE AND capability.capability_code=? AND capability.enabled=TRUE"
    [ toPersistValue stateId
    , PersistText socialEventWorkflowCode
    , PersistText (T.toLower (T.strip rawCapability))
    ]
  pure $ rows == [Single (1 :: Int)]

lifecycleFailure :: Text -> SqlPersistT IO value
lifecycleFailure = liftIO . ioError . userError . T.unpack
