{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerInternAudit
  ( internAuditServer
  , finalSummarySubmissionIsFresh
  , reportBlocksCompletion
  , reportStateCountsForFailure
  , shouldCompleteProject
  , validateExecutionStatus
  , validateReportableText
  ) where

import           Control.Monad          (forM, forM_, unless, when)
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import qualified Data.Aeson             as Aeson
import           Data.Aeson             (object, (.=))
import qualified Data.ByteString.Lazy   as BL
import           Data.Int               (Int64)
import           Data.List              (nub)
import           Data.Maybe             (catMaybes, fromMaybe, isJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (UTCTime, addDays, getCurrentTime, utctDay)
import           Database.Persist
import           Database.Persist.Sql   (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool, toSqlKey, updateWhereCount)
import           Servant
import           System.Environment      (lookupEnv)
import           Web.PathPieces         (PathPiece, fromPathPiece, toPathPiece)

import qualified TDF.API.InternAudit    as IA
import           TDF.Auth               (AuthedUser(..))
import           TDF.Catalog.Security   (loadCanonicalPartyRoles, selectCanonicalPartyIdsByRole)
import           TDF.DB                 (Env(..))
import qualified TDF.Models             as M
import qualified TDF.ModelsExtra        as ME
import           TDF.UserActivity       (recordUserActivity)

planStatuses :: [Text]
planStatuses = ["draft", "active", "completed", "cancelled"]

executionStatuses :: [Text]
executionStatuses =
  [ "pending"
  , "in_progress"
  , "passed"
  , "failed"
  , "blocked"
  , "not_applicable"
  , "ready_for_retest"
  , "verified"
  ]

terminalExecutionStatuses :: [Text]
terminalExecutionStatuses = ["passed", "failed", "blocked", "not_applicable", "verified"]

criticalCompletionStatuses :: [Text]
criticalCompletionStatuses = ["passed", "not_applicable", "verified"]

validateExecutionStatus :: Text -> Either ServerError Text
validateExecutionStatus raw =
  let normalized = T.toLower (T.strip raw)
  in if normalized `elem` executionStatuses
       then Right normalized
       else Left err400 { errBody = "Invalid test execution status" }

validateReportableText :: Text -> Int -> Text -> Either ServerError Text
validateReportableText fieldName maxLength raw =
  let normalized = T.strip raw
  in if T.null normalized
       then Left err400 { errBody = encoded (fieldName <> " is required") }
       else if T.length normalized > maxLength
         then Left err400 { errBody = encoded (fieldName <> " is too long") }
         else if T.any isUnsafe normalized
           then Left err400 { errBody = encoded (fieldName <> " contains unsupported control characters") }
           else Right normalized
  where
    encoded = BL.fromStrict . TE.encodeUtf8
    isUnsafe ch = ch == '\0' || (ch < ' ' && ch `notElem` ['\n', '\r', '\t'])

internAuditServer
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => AuthedUser
  -> ServerT IA.InternAuditAPI m
internAuditServer user =
       (listPlansH :<|> createPlanH :<|> planByIdH)
  :<|> executionsByCaseH
  :<|> updateExecutionH
  where
    isAdminUser = any (`elem` auRoles user) [M.Admin, M.Manager, M.StudioManager]
    isInternUser = M.Intern `elem` auRoles user

    ensureAdmin = unless isAdminUser $
      throwError err403 { errBody = "Internship administrator access required" }

    ensureInternshipMember = unless (isAdminUser || isInternUser) $
      throwError err403 { errBody = "Internship access required" }

    planByIdH rawPlanId =
      getPlanH rawPlanId
        :<|> updatePlanH rawPlanId
        :<|> activatePlanH rawPlanId
        :<|> casesByPlanH rawPlanId
        :<|> dailySummariesByPlanH rawPlanId
        :<|> finalSummaryByPlanH rawPlanId

    listPlansH = do
      ensureInternshipMember
      plans <- withPool $ selectList [] [Desc ME.InternAuditPlanCreatedAt]
      visible <- filterMPlanAccess plans
      mapM toPlanDTO visible

    createPlanH IA.InternAuditPlanCreate{..} = do
      ensureAdmin
      projectKey <- parseKey @ME.InternProject iapcProjectId
      taskKey <- parseKey @ME.InternTask iapcTaskId
      environment <- validateChoice "environment" ["staging"] iapcEnvironment
      let durationDays = fromMaybe 14 iapcDurationDays
          hoursMin = fromMaybe 20 iapcExpectedHoursMin
          hoursMax = fromMaybe 30 iapcExpectedHoursMax
          midpoint = fromMaybe 50 iapcMidpointPercent
      unless (durationDays >= 1 && durationDays <= 90) $
        throwError err400 { errBody = "durationDays must be between 1 and 90" }
      unless (hoursMin >= 1 && hoursMax >= hoursMin && hoursMax <= 200) $
        throwError err400 { errBody = "Expected hours must be a valid ascending range" }
      unless (midpoint >= 1 && midpoint <= 99) $
        throwError err400 { errBody = "midpointPercent must be between 1 and 99" }
      proposedAssignee <- traverse (validatePartyId "proposedAssignee") iapcProposedAssignee
      forM_ proposedAssignee $ \partyId -> do
        let partyKey = toSqlKey partyId :: M.PartyId
        party <- withPool $ get partyKey
        when (not (isJust party)) $
          throwError err400 { errBody = "proposedAssignee must identify an existing party" }
        rolesResult <- withPool $ loadCanonicalPartyRoles partyKey
        roles <- either (throwError . serverFailure) pure rolesResult
        unless (M.Intern `elem` roles) $
          throwError err400 { errBody = "proposedAssignee must have the Intern role" }
      now <- liftIO getCurrentTime
      entity <- withPool $ do
        _ <- (rawSql "SELECT id::text FROM intern_project WHERE id = ? FOR UPDATE"
          [toPersistValue projectKey] :: SqlPersistT IO [Single Text])
        _ <- (rawSql "SELECT id::text FROM intern_task WHERE id = ? FOR UPDATE"
          [toPersistValue taskKey] :: SqlPersistT IO [Single Text])
        project <- get projectKey
        task <- get taskKey
        case (project, task) of
          (Just projectValue, Just taskValue)
            | ME.internTaskProjectId taskValue == projectKey
              && ME.internProjectActivationStatus projectValue == "draft"
              && not (ME.internProjectNotificationsEnabled projectValue)
              && not (isJust (ME.internProjectActivatedAt projectValue))
              && ME.internTaskActivationStatus taskValue == "draft"
              && not (isJust (ME.internTaskAssignedTo taskValue))
              && ME.internTaskStatus taskValue == "todo"
              && ME.internTaskProgress taskValue == 0 -> do
                existing <- getBy (ME.UniqueInternAuditPlanTask taskKey)
                case existing of
                  Just _ -> pure (Left "An audit plan already exists for this task")
                  Nothing -> do
                    planId <- insert ME.InternAuditPlan
                      { ME.internAuditPlanProjectId = projectKey
                      , ME.internAuditPlanTaskId = taskKey
                      , ME.internAuditPlanEnvironment = environment
                      , ME.internAuditPlanStatus = "draft"
                      , ME.internAuditPlanDurationDays = durationDays
                      , ME.internAuditPlanExpectedHoursMin = hoursMin
                      , ME.internAuditPlanExpectedHoursMax = hoursMax
                      , ME.internAuditPlanMidpointPercent = midpoint
                      , ME.internAuditPlanProposedAssignee = fmap toSqlKey proposedAssignee
                      , ME.internAuditPlanFinalReviewRequired = fromMaybe True iapcFinalReviewRequired
                      , ME.internAuditPlanCompletionJustification = Nothing
                      , ME.internAuditPlanCompletionExceptionApproved = False
                      , ME.internAuditPlanCompletionApprovedBy = Nothing
                      , ME.internAuditPlanCompletionApprovedAt = Nothing
                      , ME.internAuditPlanCreatedBy = auPartyId user
                      , ME.internAuditPlanCreatedAt = now
                      , ME.internAuditPlanUpdatedAt = now
                      }
                    update projectKey
                      [ ME.InternProjectActivationStatus =. "draft"
                      , ME.InternProjectActivatedAt =. Nothing
                      , ME.InternProjectNotificationsEnabled =. False
                      , ME.InternProjectUpdatedAt =. now
                      ]
                    update taskKey
                      [ ME.InternTaskActivationStatus =. "draft"
                      , ME.InternTaskAssignedTo =. Nothing
                      , ME.InternTaskProposedAssignee =. fmap toSqlKey proposedAssignee
                      , ME.InternTaskStatus =. "todo"
                      , ME.InternTaskProgress =. 0
                      , ME.InternTaskUpdatedAt =. now
                      ]
                    Right <$> getJustEntity planId
          (Just _, Just taskValue)
            | ME.internTaskProjectId taskValue == projectKey ->
                pure (Left "Audit plans require an untouched draft project and task")
          _ -> pure (Left "Project and task must exist and belong together")
      ent <- either (throwError . conflict) pure entity
      audit "intern_audit_plan" (toPathPiece (entityKey ent)) "draft_created"
        (Just $ object ["projectId" .= iapcProjectId, "taskId" .= iapcTaskId])
      toPlanDTO ent

    getPlanH rawPlanId = do
      ensureInternshipMember
      ent <- loadPlan rawPlanId
      ensurePlanAccess ent
      toPlanDTO ent

    updatePlanH rawPlanId IA.InternAuditPlanUpdate{..} = do
      ensureAdmin
      ent@(Entity planKey plan) <- loadPlan rawPlanId
      now <- liftIO getCurrentTime
      justification <- case iapuCompletionJustification of
        Nothing -> pure Nothing
        Just Nothing -> pure (Just Nothing)
        Just (Just raw) -> Just . Just <$> either throwError pure (validateReportableText "completionJustification" 5000 raw)
      requestedStatus <- traverse (validateChoice "status" planStatuses) iapuStatus
      when (requestedStatus == Just "active") $
        throwError err400 { errBody = "Use the explicit activation endpoint to activate a draft plan" }
      forM_ requestedStatus $ \nextStatus ->
        unless (validPlanTransition (ME.internAuditPlanStatus plan) nextStatus) $
          throwError err409 { errBody = "Invalid audit-plan state transition" }
      currentDto <- toPlanDTO ent
      let approvesException = iapuApproveException == Just True
          effectiveJustification = case justification of
            Just value -> value
            Nothing -> ME.internAuditPlanCompletionJustification plan
      when (approvesException && not (isJust effectiveJustification)) $
        throwError err400 { errBody = "A completion justification is required before approval" }
      when (approvesException && requestedStatus /= Just "completed") $
        throwError err400 { errBody = "Exception approval is only valid while completing a plan" }
      when (requestedStatus == Just "completed" && not (IA.iapCanComplete currentDto) && not approvesException) $
        throwError err409 { errBody = "The audit plan does not meet completion criteria" }
      let updates = catMaybes
            [ fmap (ME.InternAuditPlanCompletionJustification =.) justification
            , fmap (ME.InternAuditPlanStatus =.) requestedStatus
            ]
          approvalUpdates =
            if requestedStatus == Just "completed"
              then [ ME.InternAuditPlanCompletionApprovedBy =. Just (auPartyId user)
                   , ME.InternAuditPlanCompletionApprovedAt =. Just now
                   , ME.InternAuditPlanCompletionExceptionApproved =. approvesException
                   ]
              else []
      transitioned <- withPool $ do
        changed <- updateWhereCount
          [ ME.InternAuditPlanId ==. planKey
          , ME.InternAuditPlanStatus ==. ME.internAuditPlanStatus plan
          ]
          (updates ++ approvalUpdates ++ [ME.InternAuditPlanUpdatedAt =. now])
        if changed /= 1
          then pure False
          else do
            case requestedStatus of
              Just "completed" -> do
                updateWhere
                  [ ME.InternFinalSummaryPlanId ==. planKey
                  , ME.InternFinalSummarySubmittedAt !=. Nothing
                  ]
                  [ ME.InternFinalSummaryApprovedBy =. Just (auPartyId user)
                  , ME.InternFinalSummaryApprovedAt =. Just now
                  , ME.InternFinalSummaryUpdatedAt =. now
                  ]
                update (ME.internAuditPlanTaskId plan)
                  [ ME.InternTaskStatus =. "done"
                  , ME.InternTaskProgress =. 100
                  , ME.InternTaskUpdatedAt =. now
                  ]
                remainingTasks <- count
                  [ ME.InternTaskProjectId ==. ME.internAuditPlanProjectId plan
                  , ME.InternTaskId !=. ME.internAuditPlanTaskId plan
                  , ME.InternTaskStatus /<-. ["done", "cancelled"]
                  ]
                when (shouldCompleteProject remainingTasks) $
                  update (ME.internAuditPlanProjectId plan)
                    [ ME.InternProjectStatus =. "completed"
                    , ME.InternProjectUpdatedAt =. now
                    ]
              Just "cancelled" -> do
                update (ME.internAuditPlanTaskId plan)
                  [ ME.InternTaskStatus =. "cancelled"
                  , ME.InternTaskUpdatedAt =. now
                  ]
                remainingTasks <- count
                  [ ME.InternTaskProjectId ==. ME.internAuditPlanProjectId plan
                  , ME.InternTaskId !=. ME.internAuditPlanTaskId plan
                  , ME.InternTaskStatus /<-. ["done", "cancelled"]
                  ]
                when (remainingTasks == 0) $
                  update (ME.internAuditPlanProjectId plan)
                    [ ME.InternProjectStatus =. "cancelled"
                    , ME.InternProjectUpdatedAt =. now
                    ]
              _ -> pure ()
            pure True
      unless transitioned $
        throwError err409 { errBody = "Audit plan changed during this update; reload it before retrying" }
      audit "intern_audit_plan" rawPlanId "updated"
        (Just $ object ["status" .= requestedStatus, "exceptionApproved" .= approvesException])
      updated <- withPool $ getJustEntity planKey
      toPlanDTO updated

    activatePlanH rawPlanId = do
      ensureAdmin
      Entity planKey plan <- loadPlan rawPlanId
      unless (ME.internAuditPlanStatus plan == "draft") $
        throwError err409 { errBody = "Only draft audit plans can be activated" }
      assignee <- maybe
        (throwError err409 { errBody = "A verified proposed intern is required before activation" })
        pure
        (ME.internAuditPlanProposedAssignee plan)
      rolesResult <- withPool $ loadCanonicalPartyRoles assignee
      roles <- either (throwError . serverFailure) pure rolesResult
      unless (M.Intern `elem` roles) $
        throwError err409 { errBody = "The proposed assignee does not have the Intern role" }
      caseCount <- withPool $ count [ME.InternTestCasePlanId ==. planKey]
      when (caseCount == 0) $
        throwError err409 { errBody = "At least one test case is required before activation" }
      testTransport <- liftIO isTestRuntime
      now <- liftIO getCurrentTime
      let activationDay = utctDay now
          dueDay = addDays (fromIntegral (ME.internAuditPlanDurationDays plan)) activationDay
      activated <- withPool $ do
        changed <- updateWhereCount
          [ ME.InternAuditPlanId ==. planKey
          , ME.InternAuditPlanStatus ==. "draft"
          ]
          [ ME.InternAuditPlanStatus =. "active"
          , ME.InternAuditPlanUpdatedAt =. now
          ]
        if changed /= 1
          then pure False
          else do
            update (ME.internAuditPlanProjectId plan)
              [ ME.InternProjectActivationStatus =. "active"
              , ME.InternProjectStatus =. "active"
              , ME.InternProjectActivatedAt =. Just now
              , ME.InternProjectNotificationsEnabled =. True
              , ME.InternProjectStartAt =. Just activationDay
              , ME.InternProjectDueAt =. Just dueDay
              , ME.InternProjectUpdatedAt =. now
              ]
            update (ME.internAuditPlanTaskId plan)
              [ ME.InternTaskActivationStatus =. "active"
              , ME.InternTaskAssignedTo =. Just assignee
              , ME.InternTaskProposedAssignee =. Just assignee
              , ME.InternTaskStatus =. "todo"
              , ME.InternTaskDueAt =. Just dueDay
              , ME.InternTaskUpdatedAt =. now
              ]
            insert_ M.Notification
              { M.notificationRecipientPartyId = assignee
              , M.notificationNotifType = "internship_audit_assigned"
              , M.notificationTitle = "Nueva tarea de prácticas"
              , M.notificationBody = "Tu auditoría funcional del estudio está lista. Abre Prácticas para comenzar."
              , M.notificationTargetType = Just "internship_task"
              , M.notificationTargetId = Nothing
              , M.notificationIsRead = False
              , M.notificationCreatedAt = now
              }
            insert_ ME.InternAuditNotificationOutbox
              { ME.internAuditNotificationOutboxRecipientPartyId = assignee
              , ME.internAuditNotificationOutboxReportId = Nothing
              , ME.internAuditNotificationOutboxPlanId = Just planKey
              , ME.internAuditNotificationOutboxTemplateKey = "internship_audit_assigned"
              , ME.internAuditNotificationOutboxDeliveryMode = "immediate"
              , ME.internAuditNotificationOutboxTestTransport = testTransport
              , ME.internAuditNotificationOutboxPayload = notificationPayload testTransport
              , ME.internAuditNotificationOutboxDispatchedAt = Nothing
              , ME.internAuditNotificationOutboxCreatedAt = now
              }
            pure True
      unless activated $
        throwError err409 { errBody = "Audit plan changed before activation; reload it before retrying" }
      audit "intern_audit_plan" rawPlanId "activated" Nothing
      updated <- withPool $ getJustEntity planKey
      toPlanDTO updated

    casesByPlanH rawPlanId = listCasesH rawPlanId :<|> createCaseH rawPlanId

    listCasesH rawPlanId = do
      ensureInternshipMember
      Entity planKey _ <- loadPlan rawPlanId >>= ensurePlanAccessAndReturn
      cases <- withPool $ selectList [ME.InternTestCasePlanId ==. planKey]
        [Asc ME.InternTestCaseSortOrder, Asc ME.InternTestCaseStableId]
      mapM toCaseDTO cases

    createCaseH rawPlanId IA.InternTestCaseCreate{..} = do
      ensureAdmin
      Entity planKey plan <- loadPlan rawPlanId
      unless (ME.internAuditPlanStatus plan == "draft") $
        throwError err409 { errBody = "Test cases can only be added while the plan is a draft" }
      stableId <- validateStableId itccStableId
      moduleName <- required "moduleName" 120 itccModuleName
      featureName <- required "featureName" 160 itccFeatureName
      userRole <- required "userRole" 100 itccUserRole
      objective <- required "objective" 3000 itccObjective
      businessPurpose <- required "businessPurpose" 3000 itccBusinessPurpose
      preconditions <- required "preconditions" 5000 itccPreconditions
      requiredTestData <- required "requiredTestData" 5000 itccRequiredTestData
      environment <- validateChoice "environment" [ME.internAuditPlanEnvironment plan] itccEnvironment
      platform <- required "platform" 120 itccPlatform
      browserOrDevice <- required "browserOrDevice" 200 itccBrowserOrDevice
      language <- validateChoice "language" ["es", "en"] itccLanguage
      detailedSteps <- required "detailedSteps" 12000 itccDetailedSteps
      expectedResult <- required "expectedResult" 5000 itccExpectedResult
      expectedPersistedState <- required "expectedPersistedState" 5000 itccExpectedPersistedState
      expectedSideEffects <- required "expectedSideEffects" 5000 itccExpectedSideEffects
      cleanupInstructions <- required "cleanupInstructions" 5000 itccCleanupInstructions
      criticality <- validateChoice "criticality" ["low", "medium", "high", "critical"] itccCriticality
      evidence <- validateChoice "evidenceRequirement" ["light", "strong"] itccEvidenceRequirement
      exploratoryCharter <- validatedOptional "exploratoryCharter" 5000 itccExploratoryCharter
      let sortOrder = fromMaybe 0 itccSortOrder
      when (sortOrder < 0) $
        throwError err400 { errBody = "sortOrder must not be negative" }
      result <- withPool $ do
        draft <- lockDraftAuditPlan planKey
        if not draft
          then pure (Left ("not_draft" :: Text))
          else do
            duplicate <- getBy (ME.UniqueInternTestCaseStableId planKey stableId)
            case duplicate of
              Just _ -> pure (Left "duplicate")
              Nothing -> do
                now <- liftIO getCurrentTime
                testCaseId <- insert ME.InternTestCase
                  { ME.internTestCasePlanId = planKey
                  , ME.internTestCaseStableId = stableId
                  , ME.internTestCaseModuleName = moduleName
                  , ME.internTestCaseFeatureName = featureName
                  , ME.internTestCaseUserRole = userRole
                  , ME.internTestCaseObjective = objective
                  , ME.internTestCaseBusinessPurpose = businessPurpose
                  , ME.internTestCasePreconditions = preconditions
                  , ME.internTestCaseRequiredTestData = requiredTestData
                  , ME.internTestCaseEnvironment = environment
                  , ME.internTestCasePlatform = platform
                  , ME.internTestCaseBrowserOrDevice = browserOrDevice
                  , ME.internTestCaseLanguage = language
                  , ME.internTestCaseDetailedSteps = detailedSteps
                  , ME.internTestCaseExpectedResult = expectedResult
                  , ME.internTestCaseExpectedPersistedState = expectedPersistedState
                  , ME.internTestCaseExpectedSideEffects = expectedSideEffects
                  , ME.internTestCaseCleanupInstructions = cleanupInstructions
                  , ME.internTestCaseCriticality = criticality
                  , ME.internTestCaseEvidenceRequirement = evidence
                  , ME.internTestCaseExploratoryCharter = exploratoryCharter
                  , ME.internTestCaseApplicable = fromMaybe True itccApplicable
                  , ME.internTestCaseSortOrder = sortOrder
                  , ME.internTestCaseCreatedAt = now
                  , ME.internTestCaseUpdatedAt = now
                  }
                Right <$> getJustEntity testCaseId
      ent <- case result of
        Left "not_draft" -> throwError err409
          { errBody = "Test cases can only be added while the plan is a draft" }
        Left "duplicate" -> throwError $ conflict
          "Stable test-case identifiers must be unique within the plan"
        Left _ -> throwError err500
        Right entity -> pure entity
      audit "intern_test_case" (toPathPiece (entityKey ent)) "created"
        (Just $ object ["planId" .= rawPlanId, "stableId" .= stableId])
      toCaseDTO ent

    executionsByCaseH rawCaseId = listExecutionsH rawCaseId :<|> createExecutionH rawCaseId

    listExecutionsH rawCaseId = do
      ensureInternshipMember
      (caseEnt@(Entity caseKey _), planEnt) <- loadCaseAndPlan rawCaseId
      ensurePlanAccess planEnt
      _ <- pure caseEnt
      rows <- withPool $ selectList [ME.InternTestExecutionTestCaseId ==. caseKey]
        [Desc ME.InternTestExecutionExecutionNumber]
      pure (map toExecutionDTO rows)

    createExecutionH rawCaseId IA.InternTestExecutionCreate{..} = do
      ensureInternshipMember
      (Entity caseKey testCase, planEnt@(Entity planKey _)) <- loadCaseAndPlan rawCaseId
      ensurePlanAccess planEnt
      ensureActivePlanMutation planEnt
      status <- either throwError pure (validateExecutionStatus itecStatus)
      actualResult <- validatedOptional "actualResult" 5000 itecActualResult
      persistedState <- validatedOptional "persistedStateObserved" 5000 itecPersistedStateObserved
      sideEffects <- validatedOptional "sideEffectsObserved" 5000 itecSideEffectsObserved
      blockerReason <- validatedOptional "blockerReason" 5000 itecBlockerReason
      evidenceSummary <- validatedOptional "evidenceSummary" 5000 itecEvidenceSummary
      validateExecutionPayload status actualResult blockerReason
      validateStrongEvidence testCase status evidenceSummary
      now <- liftIO getCurrentTime
      let startedAt = if status == "pending" then Nothing else Just now
          completedAt = if status `elem` terminalExecutionStatuses then Just now else Nothing
      result <- withPool $ do
        active <- lockActiveAuditPlan planKey
        if not active
          then pure Nothing
          else do
            lockInternTestExecutionSequence caseKey
            latest <- selectFirst [ME.InternTestExecutionTestCaseId ==. caseKey]
              [Desc ME.InternTestExecutionExecutionNumber]
            let nextNumber = maybe 1
                  ((+ 1) . ME.internTestExecutionExecutionNumber . entityVal)
                  latest
            executionId <- insert ME.InternTestExecution
              { ME.internTestExecutionTestCaseId = caseKey
              , ME.internTestExecutionExecutionNumber = nextNumber
              , ME.internTestExecutionExecutorPartyId = auPartyId user
              , ME.internTestExecutionStatus = status
              , ME.internTestExecutionActualResult = actualResult
              , ME.internTestExecutionPersistedStateObserved = persistedState
              , ME.internTestExecutionSideEffectsObserved = sideEffects
              , ME.internTestExecutionBlockerReason = blockerReason
              , ME.internTestExecutionEvidenceSummary = evidenceSummary
              , ME.internTestExecutionStartedAt = startedAt
              , ME.internTestExecutionCompletedAt = completedAt
              , ME.internTestExecutionCreatedAt = now
              , ME.internTestExecutionUpdatedAt = now
              }
            Just <$> getJustEntity executionId
      ent <- maybe (throwError finalizedMutationConflict) pure result
      audit "intern_test_execution" (toPathPiece (entityKey ent)) "created"
        (Just $ object ["testCaseId" .= rawCaseId, "status" .= status])
      notifyPlanMilestones planEnt status
      pure (toExecutionDTO ent)

    updateExecutionH rawExecutionId IA.InternTestExecutionUpdate{..} = do
      ensureInternshipMember
      executionKey <- parseKey @ME.InternTestExecution rawExecutionId
      Entity _ execution <- withPool (getEntity executionKey) >>= maybe (throwError err404) pure
      (Entity _ testCase, planEnt@(Entity planKey _)) <- loadCaseAndPlan (toPathPiece (ME.internTestExecutionTestCaseId execution))
      ensurePlanAccess planEnt
      ensureActivePlanMutation planEnt
      unless (isAdminUser || ME.internTestExecutionExecutorPartyId execution == auPartyId user) $
        throwError err403 { errBody = "Only the execution owner or an administrator may update it" }
      when (ME.internTestExecutionStatus execution `elem` terminalExecutionStatuses && not isAdminUser) $
        throwError err409 { errBody = "Completed executions are immutable; create a new execution for retesting" }
      status <- case iteuStatus of
        Nothing -> pure (ME.internTestExecutionStatus execution)
        Just raw -> either throwError pure (validateExecutionStatus raw)
      actualResultUpdate <- validatedNestedOptional "actualResult" 5000 iteuActualResult
      persistedStateUpdate <- validatedNestedOptional "persistedStateObserved" 5000 iteuPersistedStateObserved
      sideEffectsUpdate <- validatedNestedOptional "sideEffectsObserved" 5000 iteuSideEffectsObserved
      blockerReasonUpdate <- validatedNestedOptional "blockerReason" 5000 iteuBlockerReason
      evidenceSummaryUpdate <- validatedNestedOptional "evidenceSummary" 5000 iteuEvidenceSummary
      let actualResult = fromMaybe (ME.internTestExecutionActualResult execution) actualResultUpdate
          blockerReason = fromMaybe (ME.internTestExecutionBlockerReason execution) blockerReasonUpdate
          evidenceSummary = fromMaybe (ME.internTestExecutionEvidenceSummary execution) evidenceSummaryUpdate
      validateExecutionPayload status actualResult blockerReason
      validateStrongEvidence testCase status evidenceSummary
      now <- liftIO getCurrentTime
      let startedAt = ME.internTestExecutionStartedAt execution <|?> if status == "pending" then Nothing else Just now
          completedAt = if status `elem` terminalExecutionStatuses then Just now else Nothing
          updates = catMaybes
            [ fmap (ME.InternTestExecutionStatus =.) iteuStatusCanonical
            , fmap (ME.InternTestExecutionActualResult =.) actualResultUpdate
            , fmap (ME.InternTestExecutionPersistedStateObserved =.) persistedStateUpdate
            , fmap (ME.InternTestExecutionSideEffectsObserved =.) sideEffectsUpdate
            , fmap (ME.InternTestExecutionBlockerReason =.) blockerReasonUpdate
            , fmap (ME.InternTestExecutionEvidenceSummary =.) evidenceSummaryUpdate
            ]
          iteuStatusCanonical = if isJust iteuStatus then Just status else Nothing
      result <- withPool $ do
        active <- lockActiveAuditPlan planKey
        if not active
          then pure (Left ("finalized" :: Text))
          else do
            changed <- updateWhereCount
              [ ME.InternTestExecutionId ==. executionKey
              , ME.InternTestExecutionStatus ==. ME.internTestExecutionStatus execution
              , ME.InternTestExecutionUpdatedAt ==. ME.internTestExecutionUpdatedAt execution
              ]
              (updates ++
                [ ME.InternTestExecutionStartedAt =. startedAt
                , ME.InternTestExecutionCompletedAt =. completedAt
                , ME.InternTestExecutionUpdatedAt =. now
                ])
            if changed == 1
              then Right <$> getJustEntity executionKey
              else pure (Left "changed")
      updated <- case result of
        Left "finalized" -> throwError finalizedMutationConflict
        Left "changed" -> throwError err409
          { errBody = "Execution changed during this update; reload it before retrying" }
        Left _ -> throwError err500
        Right ent -> pure ent
      audit "intern_test_execution" rawExecutionId "updated"
        (Just $ object ["previousStatus" .= ME.internTestExecutionStatus execution, "status" .= status])
      notifyPlanMilestones planEnt status
      pure (toExecutionDTO updated)

    dailySummariesByPlanH rawPlanId = listDailySummariesH rawPlanId :<|> createDailySummaryH rawPlanId

    listDailySummariesH rawPlanId = do
      ensureInternshipMember
      planEnt@(Entity _ plan) <- loadPlan rawPlanId
      ensurePlanAccess planEnt
      rows <- withPool $ selectList [ME.InternDailySummaryTaskId ==. ME.internAuditPlanTaskId plan]
        [Desc ME.InternDailySummaryWorkDate, Desc ME.InternDailySummaryCreatedAt]
      pure (map toDailySummaryDTO rows)

    createDailySummaryH rawPlanId IA.InternDailySummaryCreate{..} = do
      ensureInternshipMember
      planEnt@(Entity planKey plan) <- loadPlan rawPlanId
      ensurePlanAccess planEnt
      ensureActivePlanMutation planEnt
      unless (idscMinutesWorked >= 1 && idscMinutesWorked <= 1440) $
        throwError err400 { errBody = "minutesWorked must be between 1 and 1440" }
      modulesTested <- required "modulesTested" 2000 idscModulesTested
      nextStep <- required "nextStep" 2000 idscNextStep
      blockers <- validatedOptional "blockers" 5000 idscBlockers
      when (idscCasesCompleted < 0 || idscReportsCreated < 0) $
        throwError err400 { errBody = "Summary counts must not be negative" }
      now <- liftIO getCurrentTime
      result <- withPool $ do
        active <- lockActiveAuditPlan planKey
        if not active
          then pure Nothing
          else do
            summaryId <- insert ME.InternDailySummary
              { ME.internDailySummaryTaskId = ME.internAuditPlanTaskId plan
              , ME.internDailySummaryAuthorPartyId = auPartyId user
              , ME.internDailySummaryWorkDate = idscWorkDate
              , ME.internDailySummaryMinutesWorked = idscMinutesWorked
              , ME.internDailySummaryModulesTested = modulesTested
              , ME.internDailySummaryCasesCompleted = idscCasesCompleted
              , ME.internDailySummaryReportsCreated = idscReportsCreated
              , ME.internDailySummaryBlockers = blockers
              , ME.internDailySummaryNextStep = nextStep
              , ME.internDailySummaryCreatedAt = now
              , ME.internDailySummaryUpdatedAt = now
              }
            Just <$> getJustEntity summaryId
      ent <- maybe (throwError finalizedMutationConflict) pure result
      audit "intern_daily_summary" (toPathPiece (entityKey ent)) "created" Nothing
      pure (toDailySummaryDTO ent)

    finalSummaryByPlanH rawPlanId = getFinalSummaryH rawPlanId :<|> upsertFinalSummaryH rawPlanId

    getFinalSummaryH rawPlanId = do
      ensureInternshipMember
      planEnt@(Entity planKey _) <- loadPlan rawPlanId
      ensurePlanAccess planEnt
      row <- withPool $ getBy (ME.UniqueInternFinalSummaryPlan planKey)
      maybe (throwError err404 { errBody = "Final summary has not been prepared" }) (pure . toFinalSummaryDTO) row

    upsertFinalSummaryH rawPlanId IA.InternFinalSummaryUpdate{..} = do
      ensureInternshipMember
      planEnt@(Entity planKey plan) <- loadPlan rawPlanId
      ensurePlanAccess planEnt
      ensureActivePlanMutation planEnt
      conclusionsUpdate <- validatedOptional "conclusions" 12000 ifsuConclusions
      result <- withPool $ do
        active <- lockActiveAuditPlan planKey
        if not active
          then pure (Left ("finalized" :: Text))
          else do
            snapshot <- buildFinalSnapshotSql planEnt
            now <- liftIO getCurrentTime
            existing <- getBy (ME.UniqueInternFinalSummaryPlan planKey)
            authorized <- case existing of
              Nothing -> do
                task <- get (ME.internAuditPlanTaskId plan)
                pure $ case task of
                  Just taskValue -> ME.internTaskAssignedTo taskValue == Just (auPartyId user)
                  Nothing -> False
              Just (Entity _ summary) ->
                pure (ME.internFinalSummaryAuthorPartyId summary == auPartyId user)
            let effectiveConclusions =
                  conclusionsUpdate <|?> (ME.internFinalSummaryConclusions . entityVal =<< existing)
            if not authorized
              then pure (Left "forbidden")
              else if ifsuSubmit == Just True && not (hasText effectiveConclusions)
                then pure (Left "conclusions")
                else Right <$> case existing of
                  Nothing -> do
                    summaryId <- insert ME.InternFinalSummary
                      { ME.internFinalSummaryPlanId = planKey
                      , ME.internFinalSummaryAuthorPartyId = auPartyId user
                      , ME.internFinalSummaryGeneratedSnapshot = snapshot
                      , ME.internFinalSummaryConclusions = conclusionsUpdate
                      , ME.internFinalSummarySubmittedAt = if ifsuSubmit == Just True then Just now else Nothing
                      , ME.internFinalSummaryApprovedBy = Nothing
                      , ME.internFinalSummaryApprovedAt = Nothing
                      , ME.internFinalSummaryCreatedAt = now
                      , ME.internFinalSummaryUpdatedAt = now
                      }
                    getJustEntity summaryId
                  Just (Entity summaryKey _) -> do
                    update summaryKey
                      [ ME.InternFinalSummaryGeneratedSnapshot =. snapshot
                      , ME.InternFinalSummaryConclusions =. effectiveConclusions
                      , ME.InternFinalSummarySubmittedAt =. if ifsuSubmit == Just True then Just now else Nothing
                      , ME.InternFinalSummaryApprovedBy =. Nothing
                      , ME.InternFinalSummaryApprovedAt =. Nothing
                      , ME.InternFinalSummaryUpdatedAt =. now
                      ]
                    getJustEntity summaryKey
      ent <- case result of
        Left "finalized" -> throwError finalizedMutationConflict
        Left "forbidden" -> throwError err403
          { errBody = "Only the assigned summary author may save the final summary" }
        Left "conclusions" -> throwError err400
          { errBody = "Conclusions are required before submitting the final summary" }
        Left _ -> throwError err500
        Right entity -> pure entity
      audit "intern_final_summary" (toPathPiece (entityKey ent))
        (if ifsuSubmit == Just True then "submitted" else "saved") Nothing
      when (ifsuSubmit == Just True) $ enqueueTeamNotification planEnt "internship_final_ready" "immediate"
      pure (toFinalSummaryDTO ent)

    loadPlan rawPlanId = do
      key <- parseKey @ME.InternAuditPlan rawPlanId
      withPool (getEntity key) >>= maybe (throwError err404) pure

    loadCaseAndPlan rawCaseId = do
      caseKey <- parseKey @ME.InternTestCase rawCaseId
      caseEnt@(Entity _ testCase) <- withPool (getEntity caseKey) >>= maybe (throwError err404) pure
      planEnt <- withPool (getEntity (ME.internTestCasePlanId testCase)) >>= maybe (throwError err404) pure
      pure (caseEnt, planEnt)

    ensurePlanAccess (Entity _ plan)
      | isAdminUser = pure ()
      | otherwise = do
          task <- withPool $ get (ME.internAuditPlanTaskId plan)
          case task of
            Just taskValue
              | ME.internTaskActivationStatus taskValue == "active"
                && ME.internTaskAssignedTo taskValue == Just (auPartyId user) -> pure ()
            _ -> throwError err404 { errBody = "Audit plan not found" }

    ensureActivePlanMutation (Entity _ plan) =
      unless (ME.internAuditPlanStatus plan == "active") $
        throwError err409 { errBody = "Finalized audit plans do not accept workflow changes" }

    ensurePlanAccessAndReturn ent = ensurePlanAccess ent >> pure ent

    filterMPlanAccess [] = pure []
    filterMPlanAccess (ent:rest) = do
      allowed <- if isAdminUser then pure True else canAccessPlan ent
      remaining <- filterMPlanAccess rest
      pure (if allowed then ent : remaining else remaining)

    canAccessPlan (Entity _ plan) = do
      task <- withPool $ get (ME.internAuditPlanTaskId plan)
      pure $ case task of
        Just taskValue -> ME.internTaskActivationStatus taskValue == "active"
          && ME.internTaskAssignedTo taskValue == Just (auPartyId user)
        Nothing -> False

    toPlanDTO ent@(Entity key plan) = do
      stats <- calculatePlanStats ent
      pure IA.InternAuditPlanDTO
        { IA.iapId = toPathPiece key
        , IA.iapProjectId = toPathPiece (ME.internAuditPlanProjectId plan)
        , IA.iapTaskId = toPathPiece (ME.internAuditPlanTaskId plan)
        , IA.iapEnvironment = ME.internAuditPlanEnvironment plan
        , IA.iapStatus = ME.internAuditPlanStatus plan
        , IA.iapDurationDays = ME.internAuditPlanDurationDays plan
        , IA.iapExpectedHoursMin = ME.internAuditPlanExpectedHoursMin plan
        , IA.iapExpectedHoursMax = ME.internAuditPlanExpectedHoursMax plan
        , IA.iapMidpointPercent = ME.internAuditPlanMidpointPercent plan
        , IA.iapProposedAssignee = fmap fromSqlKey (ME.internAuditPlanProposedAssignee plan)
        , IA.iapFinalReviewRequired = ME.internAuditPlanFinalReviewRequired plan
        , IA.iapCompletionJustification = ME.internAuditPlanCompletionJustification plan
        , IA.iapCompletionApprovedBy = fmap fromSqlKey (ME.internAuditPlanCompletionApprovedBy plan)
        , IA.iapCompletionApprovedAt = ME.internAuditPlanCompletionApprovedAt plan
        , IA.iapCaseCount = psCaseCount stats
        , IA.iapExecutedCaseCount = psExecutedCount stats
        , IA.iapCriticalRemaining = psCriticalRemaining stats
        , IA.iapOpenBlockerCount = psOpenBlockers stats
        , IA.iapFailedWithoutReport = psFailedWithoutReport stats
        , IA.iapEvidenceMissing = psEvidenceMissing stats
        , IA.iapCalculatedProgress = psProgress stats
        , IA.iapCanComplete = psCanComplete stats
        , IA.iapCreatedAt = ME.internAuditPlanCreatedAt plan
        , IA.iapUpdatedAt = ME.internAuditPlanUpdatedAt plan
        }

    toCaseDTO (Entity key testCase) = do
      latest <- withPool $ selectFirst [ME.InternTestExecutionTestCaseId ==. key]
        [Desc ME.InternTestExecutionExecutionNumber]
      pure IA.InternTestCaseDTO
        { IA.itcId = toPathPiece key
        , IA.itcPlanId = toPathPiece (ME.internTestCasePlanId testCase)
        , IA.itcStableId = ME.internTestCaseStableId testCase
        , IA.itcModuleName = ME.internTestCaseModuleName testCase
        , IA.itcFeatureName = ME.internTestCaseFeatureName testCase
        , IA.itcUserRole = ME.internTestCaseUserRole testCase
        , IA.itcObjective = ME.internTestCaseObjective testCase
        , IA.itcBusinessPurpose = ME.internTestCaseBusinessPurpose testCase
        , IA.itcPreconditions = ME.internTestCasePreconditions testCase
        , IA.itcRequiredTestData = ME.internTestCaseRequiredTestData testCase
        , IA.itcEnvironment = ME.internTestCaseEnvironment testCase
        , IA.itcPlatform = ME.internTestCasePlatform testCase
        , IA.itcBrowserOrDevice = ME.internTestCaseBrowserOrDevice testCase
        , IA.itcLanguage = ME.internTestCaseLanguage testCase
        , IA.itcDetailedSteps = ME.internTestCaseDetailedSteps testCase
        , IA.itcExpectedResult = ME.internTestCaseExpectedResult testCase
        , IA.itcExpectedPersistedState = ME.internTestCaseExpectedPersistedState testCase
        , IA.itcExpectedSideEffects = ME.internTestCaseExpectedSideEffects testCase
        , IA.itcCleanupInstructions = ME.internTestCaseCleanupInstructions testCase
        , IA.itcCriticality = ME.internTestCaseCriticality testCase
        , IA.itcEvidenceRequirement = ME.internTestCaseEvidenceRequirement testCase
        , IA.itcExploratoryCharter = ME.internTestCaseExploratoryCharter testCase
        , IA.itcApplicable = ME.internTestCaseApplicable testCase
        , IA.itcSortOrder = ME.internTestCaseSortOrder testCase
        , IA.itcLatestExecution = toExecutionDTO <$> latest
        }

    toExecutionDTO (Entity key execution) = IA.InternTestExecutionDTO
      { IA.itexId = toPathPiece key
      , IA.itexTestCaseId = toPathPiece (ME.internTestExecutionTestCaseId execution)
      , IA.itexExecutionNumber = ME.internTestExecutionExecutionNumber execution
      , IA.itexExecutorPartyId = fromSqlKey (ME.internTestExecutionExecutorPartyId execution)
      , IA.itexStatus = ME.internTestExecutionStatus execution
      , IA.itexActualResult = ME.internTestExecutionActualResult execution
      , IA.itexPersistedStateObserved = ME.internTestExecutionPersistedStateObserved execution
      , IA.itexSideEffectsObserved = ME.internTestExecutionSideEffectsObserved execution
      , IA.itexBlockerReason = ME.internTestExecutionBlockerReason execution
      , IA.itexEvidenceSummary = ME.internTestExecutionEvidenceSummary execution
      , IA.itexStartedAt = ME.internTestExecutionStartedAt execution
      , IA.itexCompletedAt = ME.internTestExecutionCompletedAt execution
      , IA.itexCreatedAt = ME.internTestExecutionCreatedAt execution
      , IA.itexUpdatedAt = ME.internTestExecutionUpdatedAt execution
      }

    toDailySummaryDTO (Entity key summary) = IA.InternDailySummaryDTO
      { IA.idsId = toPathPiece key
      , IA.idsTaskId = toPathPiece (ME.internDailySummaryTaskId summary)
      , IA.idsAuthorPartyId = fromSqlKey (ME.internDailySummaryAuthorPartyId summary)
      , IA.idsWorkDate = ME.internDailySummaryWorkDate summary
      , IA.idsMinutesWorked = ME.internDailySummaryMinutesWorked summary
      , IA.idsModulesTested = ME.internDailySummaryModulesTested summary
      , IA.idsCasesCompleted = ME.internDailySummaryCasesCompleted summary
      , IA.idsReportsCreated = ME.internDailySummaryReportsCreated summary
      , IA.idsBlockers = ME.internDailySummaryBlockers summary
      , IA.idsNextStep = ME.internDailySummaryNextStep summary
      , IA.idsCreatedAt = ME.internDailySummaryCreatedAt summary
      }

    toFinalSummaryDTO (Entity key summary) = IA.InternFinalSummaryDTO
      { IA.ifsId = toPathPiece key
      , IA.ifsPlanId = toPathPiece (ME.internFinalSummaryPlanId summary)
      , IA.ifsAuthorPartyId = fromSqlKey (ME.internFinalSummaryAuthorPartyId summary)
      , IA.ifsGeneratedSnapshot = ME.internFinalSummaryGeneratedSnapshot summary
      , IA.ifsConclusions = ME.internFinalSummaryConclusions summary
      , IA.ifsSubmittedAt = ME.internFinalSummarySubmittedAt summary
      , IA.ifsApprovedBy = fmap fromSqlKey (ME.internFinalSummaryApprovedBy summary)
      , IA.ifsApprovedAt = ME.internFinalSummaryApprovedAt summary
      , IA.ifsCreatedAt = ME.internFinalSummaryCreatedAt summary
      , IA.ifsUpdatedAt = ME.internFinalSummaryUpdatedAt summary
      }

    calculatePlanStats planEnt = withPool $ calculatePlanStatsSql planEnt

    calculatePlanStatsSql (Entity planKey plan) = do
      cases <- selectList [ME.InternTestCasePlanId ==. planKey] []
      executions <- case map entityKey cases of
        [] -> pure []
        caseKeys -> selectList [ME.InternTestExecutionTestCaseId <-. caseKeys] []
      latestPairs <- forM cases $ \caseEnt@(Entity caseKey _) -> do
        latest <- selectFirst [ME.InternTestExecutionTestCaseId ==. caseKey]
          [Desc ME.InternTestExecutionExecutionNumber]
        pure (caseEnt, latest)
      reports <- selectList
        [ME.InternalFeedbackReportInternshipTaskId ==. Just (ME.internAuditPlanTaskId plan)] []
      evidence <- case map entityKey reports of
        [] -> pure []
        reportKeys -> selectList
          [ME.InternalFeedbackEvidenceReportId <-. reportKeys] []
      daily <- selectFirst [ME.InternDailySummaryTaskId ==. ME.internAuditPlanTaskId plan] []
      final <- getBy (ME.UniqueInternFinalSummaryPlan planKey)
      let applicable = filter (ME.internTestCaseApplicable . entityVal . fst) latestPairs
          latestStatus = fmap (ME.internTestExecutionStatus . entityVal) . snd
          executed = filter (maybe False (`elem` terminalExecutionStatuses) . latestStatus) applicable
          reportMatches (Entity caseKey _) latest (Entity _ report) =
            ME.internalFeedbackReportTestCaseId report == Just caseKey
              || maybe False
                   (\(Entity executionKey _) -> ME.internalFeedbackReportTestExecutionId report == Just executionKey)
                   latest
          hasLinkedReport (caseEnt, latest) = any
            (\reportEnt@(Entity _ report) ->
              reportMatches caseEnt latest reportEnt
                && reportStateCountsForFailure
                     (ME.internalFeedbackReportState report)
                     (ME.internalFeedbackReportSubmittedAt report))
            reports
          hasEvidence (caseEnt, latest) =
            maybe False (hasText . ME.internTestExecutionEvidenceSummary . entityVal) latest
              || any
                   (\reportEnt@(Entity reportKey _) ->
                     reportMatches caseEnt latest reportEnt
                       && any ((== reportKey) . ME.internalFeedbackEvidenceReportId . entityVal) evidence)
                   reports
          criticalRemaining = length
            [ ()
            | (Entity _ testCase, latest) <- applicable
            , ME.internTestCaseCriticality testCase == "critical"
            , maybe True ((`notElem` criticalCompletionStatuses) . ME.internTestExecutionStatus . entityVal) latest
            ]
          failedWithoutReport = length
            [ ()
            | pair <- applicable
            , latestStatus pair == Just "failed"
            , not (hasLinkedReport pair)
            ]
          evidenceMissing = length
            [ ()
            | pair@(Entity _ testCase, _) <- executed
            , ME.internTestCaseEvidenceRequirement testCase == "strong"
            , not (hasEvidence pair)
            ]
          openBlockers = length
            [ ()
            | Entity _ report <- reports
            , reportBlocksCompletion
                (ME.internalFeedbackReportBlocking report)
                (ME.internalFeedbackReportState report)
            ]
          caseCount = length applicable
          executedCount = length executed
          progress = if caseCount == 0 then 0 else (executedCount * 100) `div` caseCount
          sourceUpdates =
            [ ME.internTestExecutionUpdatedAt execution
            | Entity _ execution <- executions
            ] ++
            [ ME.internalFeedbackReportUpdatedAt report
            | Entity _ report <- reports
            ]
          finalReady = not (ME.internAuditPlanFinalReviewRequired plan)
            || maybe False
                 (\(Entity _ summary) -> finalSummarySubmissionIsFresh
                   (ME.internFinalSummarySubmittedAt summary)
                   sourceUpdates)
                 final
          canComplete = caseCount > 0 && executedCount == caseCount && criticalRemaining == 0
            && openBlockers == 0 && failedWithoutReport == 0 && evidenceMissing == 0
            && isJust daily && finalReady
      pure PlanStats
        { psCaseCount = caseCount
        , psExecutedCount = executedCount
        , psCriticalRemaining = criticalRemaining
        , psOpenBlockers = openBlockers
        , psFailedWithoutReport = failedWithoutReport
        , psEvidenceMissing = evidenceMissing
        , psProgress = progress
        , psCanComplete = canComplete
        }

    buildFinalSnapshotSql planEnt = do
      stats <- calculatePlanStatsSql planEnt
      Entity planKey plan <- pure planEnt
      cases <- selectList [ME.InternTestCasePlanId ==. planKey] []
      statuses <- forM cases $ \(Entity caseKey testCase) -> do
        latest <- selectFirst [ME.InternTestExecutionTestCaseId ==. caseKey]
          [Desc ME.InternTestExecutionExecutionNumber]
        pure (ME.internTestCaseModuleName testCase, maybe "pending" (ME.internTestExecutionStatus . entityVal) latest)
      reports <- selectList
        [ME.InternalFeedbackReportInternshipTaskId ==. Just (ME.internAuditPlanTaskId plan)] []
      let countStatus status = length (filter ((== status) . snd) statuses)
          countReportType reportType = length
            [() | Entity _ report <- reports, ME.internalFeedbackReportReportType report == reportType]
          value = object
            [ "modules" .= nub (map fst statuses)
            , "cases" .= object
                [ "total" .= psCaseCount stats
                , "passed" .= countStatus "passed"
                , "failed" .= countStatus "failed"
                , "blocked" .= countStatus "blocked"
                , "notApplicable" .= countStatus "not_applicable"
                , "verified" .= countStatus "verified"
                ]
            , "reports" .= object
                [ "total" .= length reports
                , "errors" .= countReportType "error"
                , "suggestions" .= countReportType "suggestion"
                , "ideas" .= countReportType "idea"
                , "accessibility" .= countReportType "accessibility"
                , "permissions" .= countReportType "permissions"
                ]
            , "remainingRisks" .= object
                [ "criticalCases" .= psCriticalRemaining stats
                , "openBlockers" .= psOpenBlockers stats
                ]
            ]
      pure (TE.decodeUtf8 (BL.toStrict (Aeson.encode value)))

    notifyPlanMilestones planEnt@(Entity planKey plan) executionStatus = do
      stats <- calculatePlanStats planEnt
      when (psProgress stats >= ME.internAuditPlanMidpointPercent plan) $ do
        milestoneExists <- withPool $ selectFirst
          [ ME.InternAuditNotificationOutboxPlanId ==. Just planKey
          , ME.InternAuditNotificationOutboxTemplateKey ==. "internship_midpoint_reached"
          ] []
        unless (isJust milestoneExists) $
          enqueueTeamNotification planEnt "internship_midpoint_reached" "immediate"
      when (executionStatus == "blocked") $
        enqueueTeamNotification planEnt "internship_assignment_blocked" "immediate"

    enqueueTeamNotification (Entity planKey _) template deliveryMode = do
      now <- liftIO getCurrentTime
      testTransport <- liftIO isTestRuntime
      partyGroups <- withPool $ mapM selectCanonicalPartyIdsByRole [M.Admin, M.Manager, M.StudioManager]
      let recipients = nub (concat partyGroups)
          (notificationTitle, notificationBody) = case template of
            "internship_midpoint_reached" -> ("Auditoría del estudio al 50%", "La auditoría de prácticas alcanzó el punto medio y está lista para revisión.")
            "internship_assignment_blocked" -> ("Auditoría del estudio bloqueada", "Un caso bloqueado requiere revisión del equipo autorizado.")
            "internship_final_ready" -> ("Informe final de auditoría listo", "El informe final de la auditoría está listo para revisión.")
            _ -> ("Actualización de auditoría de prácticas", "Hay una actualización que requiere revisión del equipo.")
      withPool $ mapM_ (\recipient -> do
        insert_ ME.InternAuditNotificationOutbox
          { ME.internAuditNotificationOutboxRecipientPartyId = recipient
          , ME.internAuditNotificationOutboxReportId = Nothing
          , ME.internAuditNotificationOutboxPlanId = Just planKey
          , ME.internAuditNotificationOutboxTemplateKey = template
          , ME.internAuditNotificationOutboxDeliveryMode = deliveryMode
          , ME.internAuditNotificationOutboxTestTransport = testTransport
          , ME.internAuditNotificationOutboxPayload = notificationPayload testTransport
          , ME.internAuditNotificationOutboxDispatchedAt = Nothing
          , ME.internAuditNotificationOutboxCreatedAt = now
          }
        when (deliveryMode == "immediate") $ insert_ M.Notification
          { M.notificationRecipientPartyId = recipient
          , M.notificationNotifType = template
          , M.notificationTitle = notificationTitle
          , M.notificationBody = notificationBody
          , M.notificationTargetType = Just "intern_audit_plan"
          , M.notificationTargetId = Nothing
          , M.notificationIsRead = False
          , M.notificationCreatedAt = now
          }) recipients

    audit entity entityId action metadata = do
      pool <- asks envPool
      liftIO $ runSqlPool (recordUserActivity (Just (auPartyId user)) entity entityId action metadata) pool

data PlanStats = PlanStats
  { psCaseCount         :: Int
  , psExecutedCount     :: Int
  , psCriticalRemaining :: Int
  , psOpenBlockers      :: Int
  , psFailedWithoutReport :: Int
  , psEvidenceMissing   :: Int
  , psProgress          :: Int
  , psCanComplete       :: Bool
  }

reportStateCountsForFailure :: Text -> Maybe UTCTime -> Bool
reportStateCountsForFailure state submittedAt =
  state /= "draft" && isJust submittedAt

reportBlocksCompletion :: Bool -> Text -> Bool
reportBlocksCompletion blocking state =
  state == "ready_for_retest"
    || (blocking && state `notElem` ["verified", "closed", "duplicate", "discarded"])

finalSummarySubmissionIsFresh :: Maybe UTCTime -> [UTCTime] -> Bool
finalSummarySubmissionIsFresh Nothing _ = False
finalSummarySubmissionIsFresh (Just submittedAt) sourceUpdates =
  all (<= submittedAt) sourceUpdates

shouldCompleteProject :: Int -> Bool
shouldCompleteProject = (== 0)

validateExecutionPayload :: MonadError ServerError m => Text -> Maybe Text -> Maybe Text -> m ()
validateExecutionPayload status actualResult blockerReason = do
  when (status `elem` ["failed", "passed", "not_applicable", "verified"] && not (hasText actualResult)) $
    throwError err400 { errBody = "A completed execution requires an actual result" }
  when (status == "blocked" && maybe True ((< 5) . T.length . T.strip) blockerReason) $
    throwError err400 { errBody = "A blocked execution requires a clear blocker reason" }

validateStrongEvidence :: MonadError ServerError m => ME.InternTestCase -> Text -> Maybe Text -> m ()
validateStrongEvidence testCase status evidenceSummary =
  when (requiresStrongEvidence && not (hasText evidenceSummary)) $
    throwError err400 { errBody = "This execution requires a concise evidence summary or evidence reference" }
  where
    requiresStrongEvidence = status `elem` terminalExecutionStatuses
      && (ME.internTestCaseEvidenceRequirement testCase == "strong" || status `elem` ["failed", "blocked"])

hasText :: Maybe Text -> Bool
hasText = maybe False (not . T.null . T.strip)

validatedOptional :: MonadError ServerError m => Text -> Int -> Maybe Text -> m (Maybe Text)
validatedOptional _ _ Nothing = pure Nothing
validatedOptional fieldName maxLength (Just raw)
  | T.null (T.strip raw) = pure Nothing
  | otherwise = Just <$> required fieldName maxLength raw

validatedNestedOptional
  :: MonadError ServerError m
  => Text
  -> Int
  -> Maybe (Maybe Text)
  -> m (Maybe (Maybe Text))
validatedNestedOptional _ _ Nothing = pure Nothing
validatedNestedOptional _ _ (Just Nothing) = pure (Just Nothing)
validatedNestedOptional fieldName maxLength (Just (Just raw)) =
  Just <$> validatedOptional fieldName maxLength (Just raw)

required :: MonadError ServerError m => Text -> Int -> Text -> m Text
required fieldName maxLength raw = either throwError pure (validateReportableText fieldName maxLength raw)

validateChoice :: MonadError ServerError m => Text -> [Text] -> Text -> m Text
validateChoice fieldName choices raw =
  let normalized = T.toLower (T.strip raw)
  in if normalized `elem` choices
       then pure normalized
       else throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " has an unsupported value")) }

validateStableId :: MonadError ServerError m => Text -> m Text
validateStableId raw =
  let normalized = T.toUpper (T.strip raw)
      validFirst ch = ch >= 'A' && ch <= 'Z'
      validRest ch = validFirst ch || (ch >= '0' && ch <= '9') || ch == '-'
      validShape = case T.uncons normalized of
        Just (first, rest) -> validFirst first && T.all validRest rest
        Nothing -> False
  in if T.length normalized >= 3 && T.length normalized <= 40 && validShape
       then pure normalized
       else throwError err400 { errBody = "stableId's first character must be an uppercase letter; use 3-40 uppercase letters, numbers, or hyphens" }

validatePartyId :: MonadError ServerError m => Text -> Int64 -> m Int64
validatePartyId fieldName value
  | value > 0 = pure value
  | otherwise = throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be positive")) }

validPlanTransition :: Text -> Text -> Bool
validPlanTransition current next = case (current, next) of
  ("draft", "draft") -> True
  ("draft", "cancelled") -> True
  ("active", "completed") -> True
  ("active", "cancelled") -> True
  _ -> False

withPool
  :: (MonadReader Env m, MonadIO m)
  => SqlPersistT IO a
  -> m a
withPool action = asks envPool >>= liftIO . runSqlPool action

lockInternTestExecutionSequence :: ME.InternTestCaseId -> SqlPersistT IO ()
lockInternTestExecutionSequence caseKey = do
  _ <- (rawSql
    "SELECT 1::bigint FROM (SELECT pg_advisory_xact_lock(hashtextextended(?, 0))) locked"
    [PersistText ("intern-test-execution:" <> toPathPiece caseKey)]
    :: SqlPersistT IO [Single Int64])
  pure ()

lockActiveAuditPlan :: ME.InternAuditPlanId -> SqlPersistT IO Bool
lockActiveAuditPlan planKey = lockAuditPlanInStatus planKey "active"

lockDraftAuditPlan :: ME.InternAuditPlanId -> SqlPersistT IO Bool
lockDraftAuditPlan planKey = lockAuditPlanInStatus planKey "draft"

lockAuditPlanInStatus :: ME.InternAuditPlanId -> Text -> SqlPersistT IO Bool
lockAuditPlanInStatus planKey expectedStatus = do
  rows <- (rawSql
    "SELECT status FROM intern_audit_plan WHERE id = ? FOR UPDATE"
    [toPersistValue planKey]
    :: SqlPersistT IO [Single Text])
  pure $ case rows of
    [Single status] -> status == expectedStatus
    _ -> False

finalizedMutationConflict :: ServerError
finalizedMutationConflict = err409
  { errBody = "Finalized audit plans do not accept workflow changes" }

parseKey
  :: forall record m.
     (MonadError ServerError m, PathPiece (Key record))
  => Text
  -> m (Key record)
parseKey raw =
  let normalized = T.strip raw
  in case fromPathPiece normalized of
       Just key | toPathPiece key == normalized -> pure key
       _ -> throwError err400 { errBody = "Invalid identifier" }

conflict :: Text -> ServerError
conflict message = err409 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

serverFailure :: Text -> ServerError
serverFailure message = err500 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

isTestRuntime :: IO Bool
isTestRuntime = do
  appEnvironment <- fmap (T.toLower . T.strip . T.pack) <$> lookupEnv "APP_ENV"
  pure (appEnvironment `notElem` [Just "production", Just "prod"])

notificationPayload :: Bool -> Text
notificationPayload True = "{\"transport\":\"test\"}"
notificationPayload False = "{\"transport\":\"production-approved-activation\"}"

(<|?>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|?> fallback = fallback
value <|?> _ = value
