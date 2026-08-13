{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Catalog.Security
  ( validateSecurityRegistry
  , validateSecurityRegistrySnapshot
  , SecurityRegistrySnapshot (..)
  , expectedSecurityModuleCodes
  , expectedSecurityActionCodes
  , expectedSecurityRoleCodes
  , expectedSecurityPermissionBindings
  , expectedSecurityRoleAssignmentPolicyBindings
  , loadCanonicalPartyRoles
  , loadCanonicalPartyRoleMap
  , selectCanonicalPartyIdsByRole
  , hasCanonicalPartyRole
  , ensureBootstrapSecurityRole
  , applySecurityRoleAssignmentPolicy
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist (Entity (..), SelectOpt (Asc), get, getBy, insert_, selectList, toPersistValue, (==.), (<-.))
import Database.Persist.Sql (Single (..), SqlPersistT, fromSqlKey, rawSql)

import TDF.Auth (moduleRegistryCode)
import qualified TDF.Catalog.Models as Catalog
import TDF.Models (PartyId, RoleEnum, roleFromRegistryCode, roleRegistryCode)

-- These identifiers are an explicit technical-constant allowlist for backend
-- enforcement. User-facing labels, ordering, role assignments and permission
-- grants are persisted and are deliberately absent from this module.
expectedSecurityModuleCodes :: Set Text
expectedSecurityModuleCodes =
  Set.fromList [moduleRegistryCode moduleTag | moduleTag <- allValues]

expectedSecurityActionCodes :: Set Text
expectedSecurityActionCodes = Set.fromList
  [ "access"
  , "read"
  , "create"
  , "update"
  , "delete"
  , "review"
  , "approve"
  , "publish"
  , "import"
  , "export"
  , "merge"
  , "replace"
  , "deactivate"
  , "restore"
  , "assign"
  , "emergency-recover"
  ]

expectedSecurityRoleCodes :: Set Text
expectedSecurityRoleCodes =
  Set.fromList [roleRegistryCode role | role <- (allValues :: [RoleEnum])]

-- policy code, trigger code, role code, requires verified email
expectedSecurityRoleAssignmentPolicyBindings :: Set (Text, Text, Text, Bool)
expectedSecurityRoleAssignmentPolicyBindings = Set.fromList
  [ ("account.signup.customer", "account-signup", "customer", False)
  , ("account.google.customer", "google-account-create", "customer", True)
  , ("artist.verified-claim.artist", "verified-artist-claim", "artist", True)
  , ("account.generated.customer", "generated-account-create", "customer", False)
  , ("course.registration.student", "course-registration", "student", False)
  , ("trial.inquiry.student", "trial-inquiry", "student", False)
  , ("trial.teacher-subject.teacher", "teacher-subject-configured", "teacher", False)
  , ("trial.teacher-student.student", "teacher-student-linked", "student", False)
  , ("trial.student-created.student", "student-created", "student", False)
  , ("live-session.artist-profile.artist", "artist-profile-created", "artist", False)
  ]

-- permission code, module code, action code, resource scope
expectedSecurityPermissionBindings :: Set (Text, Text, Text, Text)
expectedSecurityPermissionBindings =
  Set.fromList
    ( [ (moduleCode <> ".access", moduleCode, "access", "module")
      | moduleCode <- Set.toList expectedSecurityModuleCodes
      ]
        <> [ ("catalog." <> actionCode, "catalog", actionCode, "catalog")
           | actionCode <- catalogActionCodes
           ]
        <> [ ("security." <> actionCode, "admin", actionCode, "security-registry")
           | actionCode <- securityActionCodes
           ]
        <> [ ("pipeline." <> actionCode, "scheduling", actionCode, "pipeline")
           | actionCode <- pipelineActionCodes
           ]
    )

data SecurityRegistrySnapshot = SecurityRegistrySnapshot
  { srsModuleCodes :: Set Text
  , srsActionCodes :: Set Text
  , srsRoleCodes :: Set Text
  , srsPermissionBindings :: Set (Text, Text, Text, Text)
  , srsAutomaticPolicyBindings :: Set (Text, Text, Text, Bool)
  , srsGrantedPermissionCodes :: Set Text
  , srsAssignedRoleCodes :: Set Text
  }
  deriving (Eq, Show)

validateSecurityRegistry :: SqlPersistT IO ()
validateSecurityRegistry = do
  moduleRows <- selectList [Catalog.SecurityModuleActive ==. True] [Asc Catalog.SecurityModuleCode]
  actionRows <- selectList [Catalog.SecurityActionActive ==. True] [Asc Catalog.SecurityActionCode]
  roleRows <- selectList [Catalog.SecurityRoleActive ==. True] [Asc Catalog.SecurityRoleCode]
  permissionRows <- rawSql
    "SELECT p.code, m.code, a.code, p.resource_scope FROM security_permission p JOIN security_module m ON m.id=p.module_id JOIN security_action a ON a.id=p.action_id WHERE p.active=TRUE ORDER BY p.code"
    [] :: SqlPersistT IO [(Single Text, Single Text, Single Text, Single Text)]
  policyRows <- rawSql
    "SELECT policy.code, policy.trigger_code, role.code, policy.requires_verified_email FROM security_role_assignment_policy policy JOIN security_role role ON role.id=policy.role_id WHERE policy.active=TRUE ORDER BY policy.code"
    [] :: SqlPersistT IO [(Single Text, Single Text, Single Text, Single Bool)]
  grantedPermissionRows <- rawSql
    "SELECT DISTINCT p.code FROM role_permission rp JOIN security_role r ON r.id=rp.role_id JOIN security_permission p ON p.id=rp.permission_id WHERE rp.active=TRUE AND r.active=TRUE AND p.active=TRUE ORDER BY p.code"
    []
  assignedRoleRows <- rawSql
    "SELECT DISTINCT r.code FROM party_security_role psr JOIN security_role r ON r.id=psr.role_id WHERE psr.active=TRUE AND r.active=TRUE ORDER BY r.code"
    []

  let snapshot =
        SecurityRegistrySnapshot
          { srsModuleCodes = Set.fromList [Catalog.securityModuleCode row | Entity _ row <- moduleRows]
          , srsActionCodes = Set.fromList [Catalog.securityActionCode row | Entity _ row <- actionRows]
          , srsRoleCodes = Set.fromList [Catalog.securityRoleCode row | Entity _ row <- roleRows]
          , srsPermissionBindings = Set.fromList
              [ (permissionCodeValue, moduleCode, actionCode, resourceScope)
              | (Single permissionCodeValue, Single moduleCode, Single actionCode, Single resourceScope) <- permissionRows
              ]
          , srsAutomaticPolicyBindings = Set.fromList
              [ (policyCode, triggerCode, roleCode, requiresVerifiedEmail)
              | (Single policyCode, Single triggerCode, Single roleCode, Single requiresVerifiedEmail) <- policyRows
              ]
          , srsGrantedPermissionCodes = Set.fromList [code | Single code <- grantedPermissionRows]
          , srsAssignedRoleCodes = Set.fromList [code | Single code <- assignedRoleRows]
          }
  case validateSecurityRegistrySnapshot snapshot of
    Right () -> pure ()
    Left message -> liftIO . ioError . userError $ T.unpack message

validateSecurityRegistrySnapshot :: SecurityRegistrySnapshot -> Either Text ()
validateSecurityRegistrySnapshot SecurityRegistrySnapshot{..} = do
  requireEmpty "missing code-recognized modules" (expectedSecurityModuleCodes Set.\\ srsModuleCodes)
  requireEmpty "missing code-recognized actions" (expectedSecurityActionCodes Set.\\ srsActionCodes)
  requireEmpty "missing code-recognized roles" (expectedSecurityRoleCodes Set.\\ srsRoleCodes)
  requireEmpty "missing code-recognized permissions" (expectedPermissionCodes Set.\\ actualPermissionCodes)
  requireEmpty "unknown active permissions" (actualPermissionCodes Set.\\ expectedPermissionCodes)
  requireEmpty "permissions bound to the wrong module, action, or scope" (srsPermissionBindings Set.\\ expectedSecurityPermissionBindings)
  requireEmpty "expected permission bindings not present" (expectedSecurityPermissionBindings Set.\\ srsPermissionBindings)
  requireEmpty "unknown or altered automatic assignment policies" (srsAutomaticPolicyBindings Set.\\ expectedSecurityRoleAssignmentPolicyBindings)
  requireEmpty "missing automatic assignment policies" (expectedSecurityRoleAssignmentPolicyBindings Set.\\ srsAutomaticPolicyBindings)
  requireEmpty "unknown permission codes with active grants" (srsGrantedPermissionCodes Set.\\ expectedPermissionCodes)
  requireEmpty "unknown role codes with active assignments" (srsAssignedRoleCodes Set.\\ expectedSecurityRoleCodes)
  where
    expectedPermissionCodes = Set.map permissionCode expectedSecurityPermissionBindings
    actualPermissionCodes = Set.map permissionCode srsPermissionBindings
    permissionCode (code, _, _, _) = code

pipelineActionCodes :: [Text]
pipelineActionCodes = ["read", "create", "update", "delete"]

requireEmpty :: Show a => Text -> Set a -> Either Text ()
requireEmpty label values =
  if Set.null values
    then Right ()
    else Left ("Security registry validation failed: " <> label <> ": " <> T.pack (show (Set.toList values)))

-- Runtime role reads are database-authoritative. RoleEnum remains only as the
-- exhaustive set of capability identifiers recognized by compiled backend
-- policy; assignment identity and ordering come from the persisted registry.
loadCanonicalPartyRoles :: PartyId -> SqlPersistT IO (Either Text [RoleEnum])
loadCanonicalPartyRoles partyKey = do
  rows <- rawSql
    "SELECT role.code FROM party_security_role assignment JOIN security_role role ON role.id=assignment.role_id WHERE assignment.party_id=? AND assignment.active=TRUE AND role.active=TRUE ORDER BY role.sort_order, role.code"
    [toPersistValue partyKey] :: SqlPersistT IO [Single Text]
  pure (traverse decodeRole rows)
  where
    decodeRole (Single roleCode) =
      maybe
        (Left ("Unknown active persisted security role: " <> roleCode))
        Right
        (roleFromRegistryCode roleCode)

loadCanonicalPartyRoleMap
  :: [PartyId]
  -> SqlPersistT IO (Either Text (Map PartyId [RoleEnum]))
loadCanonicalPartyRoleMap [] = pure (Right Map.empty)
loadCanonicalPartyRoleMap partyKeys = do
  assignments <- selectList
    [ Catalog.PartySecurityRolePartyId <-. partyKeys
    , Catalog.PartySecurityRoleActive ==. True
    ]
    []
  roleRows <- selectList
    [ Catalog.SecurityRoleId <-.
        Set.toList (Set.fromList [Catalog.partySecurityRoleRoleId value | Entity _ value <- assignments])
    , Catalog.SecurityRoleActive ==. True
    ]
    [Asc Catalog.SecurityRoleSortOrder, Asc Catalog.SecurityRoleCode]
  let roleById = Map.fromList [(roleKey, roleValue) | Entity roleKey roleValue <- roleRows]
      decodeAssignment (Entity _ assignment) = do
        roleValue <- maybe
          (Left "An active party security assignment references an inactive or missing role")
          Right
          (Map.lookup (Catalog.partySecurityRoleRoleId assignment) roleById)
        roleCode <- maybe
          (Left ("Unknown active persisted security role: " <> Catalog.securityRoleCode roleValue))
          Right
          (roleFromRegistryCode (Catalog.securityRoleCode roleValue))
        pure
          ( Catalog.partySecurityRolePartyId assignment
          , Catalog.securityRoleSortOrder roleValue
          , roleCode
          )
  pure $ do
    decoded <- traverse decodeAssignment assignments
    let grouped = Map.fromListWith (<>)
          [(partyKey, [(sortOrder, roleCode)]) | (partyKey, sortOrder, roleCode) <- decoded]
    pure (Map.map (map snd . sortOn fst) grouped)

selectCanonicalPartyIdsByRole :: RoleEnum -> SqlPersistT IO [PartyId]
selectCanonicalPartyIdsByRole roleCode = do
  rows <- rawSql
    "SELECT assignment.party_id FROM party_security_role assignment JOIN security_role role ON role.id=assignment.role_id WHERE role.code=? AND assignment.active=TRUE AND role.active=TRUE ORDER BY assignment.party_id"
    [toPersistValue (roleRegistryCode roleCode)]
  pure [partyKey | Single partyKey <- rows]

hasCanonicalPartyRole :: PartyId -> RoleEnum -> SqlPersistT IO Bool
hasCanonicalPartyRole partyKey roleCode = do
  rows <- rawSql
    "SELECT EXISTS (SELECT 1 FROM party_security_role assignment JOIN security_role role ON role.id=assignment.role_id WHERE assignment.party_id=? AND role.code=? AND assignment.active=TRUE AND role.active=TRUE)"
    [toPersistValue partyKey, toPersistValue (roleRegistryCode roleCode)]
  pure $ case rows of
    [Single exists] -> exists
    _ -> False

-- Bootstrap is reserved for deterministic installation/seed identities. It
-- never reactivates a reviewed revocation and always records provenance.
ensureBootstrapSecurityRole
  :: PartyId
  -> RoleEnum
  -> UTCTime
  -> SqlPersistT IO (Either Text ())
ensureBootstrapSecurityRole partyKey roleCode now = do
  roleEntity <- getBy (Catalog.UniqueSecurityRoleCode (roleRegistryCode roleCode))
  case roleEntity of
    Nothing -> pure (Left ("Persisted bootstrap security role is missing: " <> roleRegistryCode roleCode))
    Just (Entity roleKey roleValue)
      | not (Catalog.securityRoleActive roleValue) ->
          pure (Left ("Persisted bootstrap security role is inactive: " <> roleRegistryCode roleCode))
      | otherwise -> do
          existing <- getBy (Catalog.UniquePartySecurityRole partyKey roleKey)
          case existing of
            Just (Entity _ assignment)
              | Catalog.partySecurityRoleActive assignment -> pure (Right ())
              | otherwise -> pure (Left ("Bootstrap cannot reactivate a revoked security role: " <> roleRegistryCode roleCode))
            Nothing -> do
              insert_ Catalog.PartySecurityRole
                { Catalog.partySecurityRolePartyId = partyKey
                , Catalog.partySecurityRoleRoleId = roleKey
                , Catalog.partySecurityRoleGrantedBy = Nothing
                , Catalog.partySecurityRoleApprovedBy = Nothing
                , Catalog.partySecurityRoleApprovalMode = "bootstrap"
                , Catalog.partySecurityRoleEmergencyReason = Nothing
                , Catalog.partySecurityRoleSourceRevisionId = Nothing
                , Catalog.partySecurityRoleSourcePolicyId = Nothing
                , Catalog.partySecurityRoleActive = True
                , Catalog.partySecurityRoleCreatedAt = now
                , Catalog.partySecurityRoleRevokedAt = Nothing
                , Catalog.partySecurityRoleVersion = 1
                }
              insert_ Catalog.SecurityAuditEvent
                { Catalog.securityAuditEventRevisionId = Nothing
                , Catalog.securityAuditEventSourcePolicyId = Nothing
                , Catalog.securityAuditEventEntityKind = "party-role"
                , Catalog.securityAuditEventPartyId = Just partyKey
                , Catalog.securityAuditEventRoleId = roleKey
                , Catalog.securityAuditEventPermissionId = Nothing
                , Catalog.securityAuditEventOperation = "bootstrap-assigned"
                , Catalog.securityAuditEventPreviousActive = Nothing
                , Catalog.securityAuditEventNewActive = Just True
                , Catalog.securityAuditEventActorId = Nothing
                , Catalog.securityAuditEventReviewerId = Nothing
                , Catalog.securityAuditEventApproverId = Nothing
                , Catalog.securityAuditEventOccurredAt = now
                , Catalog.securityAuditEventSourcePlatform = "seed"
                , Catalog.securityAuditEventReason = Just "Deterministic installation bootstrap"
                , Catalog.securityAuditEventCorrelationId =
                    "bootstrap-role:"
                      <> T.pack (show (fromSqlKey partyKey))
                      <> ":"
                      <> roleRegistryCode roleCode
                , Catalog.securityAuditEventApprovalMode = "bootstrap"
                , Catalog.securityAuditEventResult = "success"
                }
              pure (Right ())

-- Applies only a compile-recognized, persisted automatic assignment policy.
-- It never reactivates a revoked assignment and records immutable provenance.
applySecurityRoleAssignmentPolicy
  :: Text
  -> PartyId
  -> Bool
  -> Maybe PartyId
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text RoleEnum)
applySecurityRoleAssignmentPolicy policyCode partyKey verifiedEmail actorId sourcePlatform correlationId now = do
  policyRow <- getBy (Catalog.UniqueSecurityRoleAssignmentPolicyCode policyCode)
  case policyRow of
    Nothing -> pure (Left ("Automatic security policy is missing: " <> policyCode))
    Just (Entity policyKey policy) -> do
      roleMaybe <- get (Catalog.securityRoleAssignmentPolicyRoleId policy)
      case roleMaybe >>= \role -> (,) role <$> roleFromRegistryCode (Catalog.securityRoleCode role) of
        Nothing -> pure (Left ("Automatic security policy references an unknown role: " <> policyCode))
        Just (role, roleCode)
          | (policyCode, Catalog.securityRoleAssignmentPolicyTriggerCode policy, Catalog.securityRoleCode role, Catalog.securityRoleAssignmentPolicyRequiresVerifiedEmail policy)
              `Set.notMember` expectedSecurityRoleAssignmentPolicyBindings ->
              pure (Left ("Automatic security policy does not match a code-recognized binding: " <> policyCode))
          | not (Catalog.securityRoleAssignmentPolicyActive policy) ->
              pure (Left ("Automatic security policy is inactive: " <> policyCode))
          | not (Catalog.securityRoleActive role) || not (Catalog.securityRoleAutomaticAssignable role) ->
              pure (Left ("Automatic security policy role is inactive or not automatically assignable: " <> policyCode))
          | Catalog.securityRoleAssignmentPolicyRequiresVerifiedEmail policy && not verifiedEmail ->
              pure (Left ("Automatic security policy requires a verified email: " <> policyCode))
          | maybe False (> now) (Catalog.securityRoleAssignmentPolicyEffectiveFrom policy)
              || maybe False (<= now) (Catalog.securityRoleAssignmentPolicyEffectiveTo policy) ->
              pure (Left ("Automatic security policy is outside its effective period: " <> policyCode))
          | otherwise -> do
              existing <- getBy (Catalog.UniquePartySecurityRole partyKey (Catalog.securityRoleAssignmentPolicyRoleId policy))
              case existing of
                Just (Entity _ assignment)
                  | Catalog.partySecurityRoleActive assignment -> pure (Right roleCode)
                  | otherwise -> pure (Left ("Automatic security policy cannot reactivate a revoked assignment: " <> policyCode))
                Nothing -> do
                  insert_ Catalog.PartySecurityRole
                    { Catalog.partySecurityRolePartyId = partyKey
                    , Catalog.partySecurityRoleRoleId = Catalog.securityRoleAssignmentPolicyRoleId policy
                    , Catalog.partySecurityRoleGrantedBy = Nothing
                    , Catalog.partySecurityRoleApprovedBy = Nothing
                    , Catalog.partySecurityRoleApprovalMode = "system-policy"
                    , Catalog.partySecurityRoleEmergencyReason = Nothing
                    , Catalog.partySecurityRoleSourceRevisionId = Nothing
                    , Catalog.partySecurityRoleSourcePolicyId = Just policyKey
                    , Catalog.partySecurityRoleActive = True
                    , Catalog.partySecurityRoleCreatedAt = now
                    , Catalog.partySecurityRoleRevokedAt = Nothing
                    , Catalog.partySecurityRoleVersion = 1
                    }
                  insert_ Catalog.SecurityAuditEvent
                    { Catalog.securityAuditEventRevisionId = Nothing
                    , Catalog.securityAuditEventSourcePolicyId = Just policyKey
                    , Catalog.securityAuditEventEntityKind = "party-role"
                    , Catalog.securityAuditEventPartyId = Just partyKey
                    , Catalog.securityAuditEventRoleId = Catalog.securityRoleAssignmentPolicyRoleId policy
                    , Catalog.securityAuditEventPermissionId = Nothing
                    , Catalog.securityAuditEventOperation = "system-policy-assigned"
                    , Catalog.securityAuditEventPreviousActive = Nothing
                    , Catalog.securityAuditEventNewActive = Just True
                    , Catalog.securityAuditEventActorId = actorId
                    , Catalog.securityAuditEventReviewerId = Nothing
                    , Catalog.securityAuditEventApproverId = Nothing
                    , Catalog.securityAuditEventOccurredAt = now
                    , Catalog.securityAuditEventSourcePlatform = sourcePlatform
                    , Catalog.securityAuditEventReason = Just ("Applied persisted policy " <> policyCode)
                    , Catalog.securityAuditEventCorrelationId = correlationId
                    , Catalog.securityAuditEventApprovalMode = "system-policy"
                    , Catalog.securityAuditEventResult = "success"
                    }
                  pure (Right roleCode)

catalogActionCodes :: [Text]
catalogActionCodes =
  [ "read"
  , "create"
  , "update"
  , "review"
  , "approve"
  , "publish"
  , "import"
  , "export"
  , "merge"
  , "replace"
  , "deactivate"
  , "restore"
  ]

securityActionCodes :: [Text]
securityActionCodes =
  [ "read"
  , "create"
  , "review"
  , "approve"
  , "assign"
  , "emergency-recover"
  ]

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound .. maxBound]
