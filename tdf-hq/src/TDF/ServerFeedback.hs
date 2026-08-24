{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module TDF.ServerFeedback
  ( feedbackServer
  , internalFeedbackServer
  , normalizeOptionalFeedbackText
  , validateFeedbackDescription
  , validateFeedbackTitle
  , validateFeedbackConsent
  , validateOptionalFeedbackContactEmail
  , validateFeedbackAttachmentSize
  , validateFeedbackAttachmentContentType
  , validateFeedbackAttachmentFileName
  , validateFeedbackAttachmentMetadata
  , sanitizeFeedbackAttachmentFileName
  , validateReportType
  , internalReportTypeForCategoryCode
  , validateInternalReportState
  , validateStateTransition
  , validatePriority
  , validateEnvironment
  , validateExternalEvidenceUrl
  , validateGithubIssueUrl
  , validateVideoLinks
  , csvField
  ) where

import           Control.Exception         (SomeException, displayException, try)
import           Control.Applicative       ((<|>))
import           Control.Monad              (forM, forM_, unless, when)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask, asks)
import qualified Data.Aeson                  as Aeson
import           Data.Aeson                  (object, (.=))
import           Data.Int                    (Int64)
import           Data.List                   (nub, sortOn)
import           Data.Maybe                  (catMaybes, fromMaybe, isJust)
import qualified Data.Set                    as Set
import           Data.Char                  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
                                            , generalCategory
                                            , isAlphaNum
                                            , isAscii
                                            , isAsciiLower
                                            , isControl
                                            , isDigit
                                            )
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql       (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool, toSqlKey, updateWhereCount)
import           Servant
import           Servant.Multipart          (FileData(..), Tmp)
import           System.Directory           (createDirectoryIfMissing, doesFileExist, getFileSize)
import           System.FilePath            ((</>), makeRelative, normalise, takeExtension, takeFileName)
import           System.IO                  (hPutStrLn, stderr)
import           System.Environment         (lookupEnv)
import qualified Data.ByteString.Lazy       as BL
import           Data.UUID.V4               (nextRandom)
import           Data.UUID                  (toText)
import           Web.PathPieces             (PathPiece, fromPathPiece, toPathPiece)

import           TDF.API.Feedback
import           TDF.Auth                   ( AuthedUser(..)
                                            , extractTokenFromHeaders
                                            , loadAuthedUser
                                            )
import           TDF.DB                     (Env(..))
import qualified TDF.Models                  as M
import           TDF.ModelsExtra            (Feedback(..))
import qualified TDF.ModelsExtra             as ME
import qualified TDF.Catalog.Models          as Catalog
import           TDF.Catalog.Security        (selectCanonicalPartyIdsByRole)
import qualified TDF.Email.Service          as EmailSvc
import           TDF.UserActivity            (recordUserActivity)

feedbackServer
  :: forall m.
     ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => ServerT FeedbackAPI m
feedbackServer authorizationHeader cookieHeader = submitFeedback
  where
    submitFeedback :: FeedbackPayload -> m NoContent
    submitFeedback FeedbackPayload{..} = do
      title <- either throwError pure (validateFeedbackTitle fpTitle)
      body <- either throwError pure (validateFeedbackDescription fpDescription)
      (categoryId, categoryLabel) <- resolvePublishedFeedbackCategory fpCategoryId
      (severityId, severityLabel) <- resolvePublishedFeedbackSeverity fpSeverityId
      either throwError pure (validateFeedbackConsent fpConsent)
      contactEmail <- either throwError pure (validateOptionalFeedbackContactEmail fpContactEmail)

      now <- liftIO getCurrentTime
      attachmentPath <- traverse validateAndStoreAttachment fpAttachment

      Env{..} <- ask
      let emailSvc = EmailSvc.mkEmailService envConfig
      creator <- case extractTokenFromHeaders envConfig authorizationHeader cookieHeader of
        Left _ ->
          pure Nothing
        Right token ->
          liftIO $ runSqlPool (loadAuthedUser token) envPool

      _ <- liftIO $ runSqlPool
        (insert Feedback
          { feedbackTitle        = title
          , feedbackDescription  = body
          , feedbackCategory     = Nothing
          , feedbackSeverity     = Nothing
          , feedbackCategoryId   = Just categoryId
          , feedbackSeverityId   = Just severityId
          , feedbackContactEmail = contactEmail
          , feedbackAttachment   = fmap T.pack attachmentPath
          , feedbackConsent      = fpConsent
          , feedbackCreatedBy    = auPartyId <$> creator
          , feedbackCreatedAt    = now
          })
        envPool

      liftIO $ notify emailSvc title body (Just categoryLabel) (Just severityLabel) contactEmail attachmentPath

      pure NoContent

    resolvePublishedFeedbackCategory :: Text -> m (Catalog.FeedbackCategoryId, Text)
    resolvePublishedFeedbackCategory rawId = do
      Env{envPool = pool} <- ask
      categoryKey <- maybe
        (throwError err400 { errBody = "categoryId must be a valid catalog UUID" })
        pure
        (fromPathPiece (T.strip rawId))
      result <- liftIO $ runSqlPool (do
        item <- get categoryKey
        case item of
          Nothing -> pure Nothing
          Just category -> do
            state <- get (Catalog.feedbackCategoryWorkflowStateId category)
            catalog <- get (Catalog.feedbackCategoryCatalogId category)
            pure $
              if Catalog.feedbackCategoryActive category
                && Catalog.feedbackCategoryDeprecatedAt category == Nothing
                && maybe False ((== "published") . Catalog.workflowStateCode) state
                && maybe False (\definition -> Catalog.catalogDefinitionActive definition && Catalog.catalogDefinitionCode definition == "feedback-categories") catalog
                then Just (Catalog.feedbackCategoryNameEs category)
                else Nothing) pool
      label <- maybe
        (throwError err400 { errBody = "categoryId must reference an active published feedback category" })
        pure
        result
      pure (categoryKey, label)

    resolvePublishedFeedbackSeverity :: Text -> m (Catalog.FeedbackSeverityId, Text)
    resolvePublishedFeedbackSeverity rawId = do
      Env{envPool = pool} <- ask
      severityKey <- maybe
        (throwError err400 { errBody = "severityId must be a valid catalog UUID" })
        pure
        (fromPathPiece (T.strip rawId))
      result <- liftIO $ runSqlPool (do
        item <- get severityKey
        case item of
          Nothing -> pure Nothing
          Just severity -> do
            state <- get (Catalog.feedbackSeverityWorkflowStateId severity)
            catalog <- get (Catalog.feedbackSeverityCatalogId severity)
            pure $
              if Catalog.feedbackSeverityActive severity
                && Catalog.feedbackSeverityDeprecatedAt severity == Nothing
                && maybe False ((== "published") . Catalog.workflowStateCode) state
                && maybe False (\definition -> Catalog.catalogDefinitionActive definition && Catalog.catalogDefinitionCode definition == "feedback-severities") catalog
                then Just (Catalog.feedbackSeverityNameEs severity)
                else Nothing) pool
      label <- maybe
        (throwError err400 { errBody = "severityId must reference an active published feedback severity" })
        pure
        result
      pure (severityKey, label)

    validateAndStoreAttachment :: FileData Tmp -> m FilePath
    validateAndStoreAttachment file@FileData{..} = do
      (safeName, _) <-
        either throwError pure (validateFeedbackAttachmentMetadata fdFileName fdFileCType)
      size <- liftIO (getFileSize fdPayload)
      either throwError pure (validateFeedbackAttachmentSize size)
      liftIO (storeAttachment safeName file)

    storeAttachment :: Text -> FileData Tmp -> IO FilePath
    storeAttachment safeName FileData{fdPayload = payload} = do
      token <- toText <$> nextRandom
      let destDir = "uploads/feedback"
      createDirectoryIfMissing True destDir
      let destPath = destDir </> T.unpack token <> "-" <> T.unpack safeName
      BL.readFile payload >>= BL.writeFile destPath
      pure destPath

internalFeedbackServer
  :: forall m.
     ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT InternalFeedbackAPI m
internalFeedbackServer user =
       listReportsH
  :<|> exportCsvH
  :<|> exportJsonH
  :<|> listLegacyFeedbackH
  :<|> createReportH
  :<|> reportByIdH
  where
    isAdminUser = any (`elem` auRoles user) [M.Admin, M.Manager, M.StudioManager]
    isInternUser = M.Intern `elem` auRoles user

    ensureInternalAccess = unless (isAdminUser || isInternUser) $
      throwError err403 { errBody = "Internal testing report access required" }

    ensureAdmin = unless isAdminUser $
      throwError err403 { errBody = "Report administration access required" }

    reportByIdH rawReportId =
      getReportH rawReportId
        :<|> updateReportH rawReportId
        :<|> submitReportH rawReportId
        :<|> createCommentH rawReportId
        :<|> uploadEvidenceH rawReportId
        :<|> createEvidenceLinkH rawReportId
        :<|> downloadEvidenceH rawReportId
        :<|> createRetestH rawReportId

    listReportsH mState mModule mQuery mMine = do
      ensureInternalAccess
      stateFilter <- traverse (validateInternalReportState "state") mState
      reports <- selectVisibleReports mMine
      summaries <- mapM toSummaryDTO reports
      pure $ filter (matchesSummary stateFilter mModule mQuery) summaries

    exportJsonH mState mModule = listReportsH mState mModule Nothing Nothing

    exportCsvH mState mModule = do
      rows <- listReportsH mState mModule Nothing Nothing
      pure $ T.unlines
        ( "id,title,type,state,module,feature,environment,platform,priority,blocking,reporter,created_at"
        : map summaryCsv rows
        )

    listLegacyFeedbackH = do
      ensureAdmin
      rows <- withPool $ selectList [] [Desc ME.FeedbackCreatedAt, LimitTo 1000]
      fmap catMaybes $ forM rows $ \(Entity feedbackKey feedback) -> do
        normalized <- withPool $ getBy (ME.UniqueInternalFeedbackReport feedbackKey)
        pure $ case normalized of
          Just _ -> Nothing
          Nothing -> Just LegacyFeedbackDTO
            { lfdId = toPathPiece feedbackKey
            , lfdTitle = feedbackTitle feedback
            , lfdDescription = feedbackDescription feedback
            , lfdCategoryId = toPathPiece <$> feedbackCategoryId feedback
            , lfdSeverityId = toPathPiece <$> feedbackSeverityId feedback
            , lfdContactEmail = feedbackContactEmail feedback
            , lfdConsent = feedbackConsent feedback
            , lfdCreatedBy = fromSqlKey <$> feedbackCreatedBy feedback
            , lfdHasAttachment = isJust (feedbackAttachment feedback)
            , lfdCreatedAt = feedbackCreatedAt feedback
            }

    createReportH InternalFeedbackCreate{..} = do
      ensureInternalAccess
      title <- either throwError pure (validateFeedbackTitle ifcTitle)
      description <- either throwError pure (validateFeedbackDescription ifcDescription)
      categoryId <- resolvePublishedFeedbackCategoryFor ifcCategoryId
      proposedSeverityId <- resolvePublishedFeedbackSeverityFor ifcProposedSeverityId
      reportType <- validateReportType ifcReportType
      validateInternalReportCategoryType categoryId reportType
      moduleName <- validateInternalText "moduleName" 120 ifcModuleName
      environment <- validateEnvironment ifcEnvironment
      platform <- validateInternalText "platform" 80 ifcPlatform
      language <- validateInternalText "language" 20 ifcLanguage
      accountRole <- validateInternalText "accountRole" 100 ifcAccountRole
      featureName <- validateOptionalInternalText "featureName" 160 ifcFeatureName
      urlOrScreen <- validateOptionalInternalText "urlOrScreen" 2048 ifcUrlOrScreen
      device <- validateOptionalInternalText "device" 160 ifcDevice
      browser <- validateOptionalInternalText "browser" 160 ifcBrowser
      reproductionSteps <- validateOptionalInternalText "reproductionSteps" 10000 ifcReproductionSteps
      expectedResult <- validateOptionalInternalText "expectedResult" 5000 ifcExpectedResult
      actualResult <- validateOptionalInternalText "actualResult" 5000 ifcActualResult
      frequency <- validateOptionalInternalText "frequency" 160 ifcFrequency
      videoLinks <- validateVideoLinks ifcVideoLinks
      trace <- resolveTraceability
        ifcInternshipProjectId
        ifcInternshipTaskId
        ifcTestCaseId
        ifcTestExecutionId
      validateTraceability trace
      now <- liftIO getCurrentTime
      contactEmail <- withPool $ maybe Nothing M.partyPrimaryEmail <$> get (auPartyId user)
      entitiesResult <- withPool $ do
        planActive <- maybe (pure True) lockActiveAuditPlanForTask (traceTaskId trace)
        if not planActive
          then pure Nothing
          else Just <$> do
            feedbackId <- insert Feedback
              { feedbackTitle = title
              , feedbackDescription = description
              , feedbackCategory = Nothing
              , feedbackSeverity = Nothing
              , feedbackCategoryId = Just categoryId
              , feedbackSeverityId = Just proposedSeverityId
              , feedbackContactEmail = contactEmail
              , feedbackAttachment = Nothing
              , feedbackConsent = False
              , feedbackCreatedBy = Just (auPartyId user)
              , feedbackCreatedAt = now
              }
            reportId <- insert ME.InternalFeedbackReport
              { ME.internalFeedbackReportFeedbackId = feedbackId
              , ME.internalFeedbackReportReportType = reportType
              , ME.internalFeedbackReportState = "draft"
              , ME.internalFeedbackReportModuleName = moduleName
              , ME.internalFeedbackReportFeatureName = featureName
              , ME.internalFeedbackReportEnvironment = environment
              , ME.internalFeedbackReportUrlOrScreen = urlOrScreen
              , ME.internalFeedbackReportPlatform = platform
              , ME.internalFeedbackReportDevice = device
              , ME.internalFeedbackReportBrowser = browser
              , ME.internalFeedbackReportLanguage = language
              , ME.internalFeedbackReportAccountRole = accountRole
              , ME.internalFeedbackReportReproductionSteps = reproductionSteps
              , ME.internalFeedbackReportExpectedResult = expectedResult
              , ME.internalFeedbackReportActualResult = actualResult
              , ME.internalFeedbackReportFrequency = frequency
              , ME.internalFeedbackReportProposedSeverityId = Just proposedSeverityId
              , ME.internalFeedbackReportAuthoritativeSeverityId = Nothing
              , ME.internalFeedbackReportPriority = Nothing
              , ME.internalFeedbackReportTestCaseId = traceTestCaseId trace
              , ME.internalFeedbackReportTestExecutionId = traceExecutionId trace
              , ME.internalFeedbackReportInternshipProjectId = traceProjectId trace
              , ME.internalFeedbackReportInternshipTaskId = traceTaskId trace
              , ME.internalFeedbackReportReporterPartyId = auPartyId user
              , ME.internalFeedbackReportBlocking = fromMaybe False ifcBlocking
              , ME.internalFeedbackReportAssignedTo = Nothing
              , ME.internalFeedbackReportDuplicateOf = Nothing
              , ME.internalFeedbackReportResolution = Nothing
              , ME.internalFeedbackReportRetestResult = Nothing
              , ME.internalFeedbackReportClosureReason = Nothing
              , ME.internalFeedbackReportGithubIssueUrl = Nothing
              , ME.internalFeedbackReportVideoLinks = videoLinks
              , ME.internalFeedbackReportSubmittedAt = Nothing
              , ME.internalFeedbackReportClosedAt = Nothing
              , ME.internalFeedbackReportVersion = 1
              , ME.internalFeedbackReportCreatedAt = now
              , ME.internalFeedbackReportUpdatedAt = now
              }
            insert_ ME.InternalFeedbackHistory
              { ME.internalFeedbackHistoryReportId = reportId
              , ME.internalFeedbackHistoryActorPartyId = auPartyId user
              , ME.internalFeedbackHistoryAction = "draft_created"
              , ME.internalFeedbackHistoryPreviousState = Nothing
              , ME.internalFeedbackHistoryNewState = Just "draft"
              , ME.internalFeedbackHistoryMetadata = Nothing
              , ME.internalFeedbackHistoryCreatedAt = now
              }
            report <- getJustEntity reportId
            feedback <- getJustEntity feedbackId
            pure (report, feedback)
      entities <- maybe
        (throwError err409 { errBody = "Finalized audit plans do not accept new reports" })
        pure
        entitiesResult
      let (reportEnt, feedbackEnt) = entities
      recordAudit reportEnt "draft_created" Nothing
      buildReportDTO reportEnt feedbackEnt

    getReportH rawReportId = do
      ensureInternalAccess
      (reportEnt, feedbackEnt) <- loadAccessibleReport rawReportId
      buildReportDTO reportEnt feedbackEnt

    updateReportH rawReportId updateRequest@InternalFeedbackUpdate{..} = do
      ensureInternalAccess
      (reportEnt@(Entity reportKey report), Entity feedbackKey feedback) <- loadAccessibleReport rawReportId
      let owner = ME.internalFeedbackReportReporterPartyId report == auPartyId user
          state = ME.internalFeedbackReportState report
          hasAdminFields = or
            [ isJust ifuState
            , isJust ifuAuthoritativeSeverityId
            , isJust ifuPriority
            , isJust ifuAssignedTo
            , isJust ifuDuplicateOf
            , isJust ifuResolution
            , isJust ifuRetestResult
            , isJust ifuClosureReason
            , isJust ifuGithubIssueUrl
            ]
          hasReporterFields = or
            [ isJust ifuTitle, isJust ifuDescription, isJust ifuCategoryId
            , isJust ifuProposedSeverityId, isJust ifuReportType, isJust ifuModuleName
            , isJust ifuFeatureName, isJust ifuEnvironment, isJust ifuUrlOrScreen
            , isJust ifuPlatform, isJust ifuDevice, isJust ifuBrowser, isJust ifuLanguage
            , isJust ifuAccountRole, isJust ifuReproductionSteps, isJust ifuExpectedResult
            , isJust ifuActualResult, isJust ifuFrequency, isJust ifuBlocking, isJust ifuVideoLinks
            ]
      unless (isAdminUser || owner) $ throwError err404
      when (not isAdminUser && hasAdminFields) $
        throwError err403 { errBody = "Only administrators may set authoritative severity, priority, assignment, resolution, duplicate, state, or closure" }
      when (not isAdminUser && isJust ifuBlocking && state /= "draft") $
        throwError err403 { errBody = "Only administrators may change blocker classification after submission" }
      when (not isAdminUser && hasReporterFields && state `notElem` ["draft", "needs_information"]) $
        throwError err409 { errBody = "Submitted report fields may only be expanded when more information is requested" }
      titleUpdate <- traverse (either throwError pure . validateFeedbackTitle) ifuTitle
      descriptionUpdate <- traverse (either throwError pure . validateFeedbackDescription) ifuDescription
      categoryUpdate <- traverse resolvePublishedFeedbackCategoryFor ifuCategoryId
      proposedSeverityUpdate <- traverse resolvePublishedFeedbackSeverityFor ifuProposedSeverityId
      reportTypeUpdate <- traverse validateReportType ifuReportType
      let effectiveCategoryId = categoryUpdate <|> feedbackCategoryId feedback
          effectiveReportType = fromMaybe (ME.internalFeedbackReportReportType report) reportTypeUpdate
      when (isJust categoryUpdate || isJust reportTypeUpdate) $ case effectiveCategoryId of
        Nothing -> throwError err400 { errBody = "Internal reports require a feedback category" }
        Just categoryId -> validateInternalReportCategoryType categoryId effectiveReportType
      moduleUpdate <- traverse (validateInternalText "moduleName" 120) ifuModuleName
      environmentUpdate <- traverse validateEnvironment ifuEnvironment
      platformUpdate <- traverse (validateInternalText "platform" 80) ifuPlatform
      languageUpdate <- traverse (validateInternalText "language" 20) ifuLanguage
      roleUpdate <- traverse (validateInternalText "accountRole" 100) ifuAccountRole
      featureUpdate <- validateNestedInternalText "featureName" 160 ifuFeatureName
      urlUpdate <- validateNestedInternalText "urlOrScreen" 2048 ifuUrlOrScreen
      deviceUpdate <- validateNestedInternalText "device" 160 ifuDevice
      browserUpdate <- validateNestedInternalText "browser" 160 ifuBrowser
      reproductionUpdate <- validateNestedInternalText "reproductionSteps" 10000 ifuReproductionSteps
      expectedUpdate <- validateNestedInternalText "expectedResult" 5000 ifuExpectedResult
      actualUpdate <- validateNestedInternalText "actualResult" 5000 ifuActualResult
      frequencyUpdate <- validateNestedInternalText "frequency" 160 ifuFrequency
      resolutionUpdate <- validateNestedInternalText "resolution" 10000 ifuResolution
      retestResultUpdate <- validateNestedInternalText "retestResult" 5000 ifuRetestResult
      closureReasonUpdate <- validateNestedInternalText "closureReason" 5000 ifuClosureReason
      stateUpdate <- traverse (validateStateTransition state) ifuState
      authoritativeSeverityUpdate <- traverse (traverse resolvePublishedFeedbackSeverityFor) ifuAuthoritativeSeverityId
      priorityUpdate <- traverse (traverse validatePriority) ifuPriority
      assignedToUpdate <- traverse (traverse (validatePositiveParty "assignedTo")) ifuAssignedTo
      forM_ assignedToUpdate $ mapM_ $ \partyId -> do
        let partyKey = toSqlKey partyId :: M.PartyId
        partyExists <- isJust <$> withPool (get partyKey)
        unless partyExists $
          throwError err400 { errBody = "assignedTo must identify an existing party" }
      duplicateUpdate <- traverse (traverse (resolveDuplicateTarget reportKey)) ifuDuplicateOf
      videoLinksUpdate <- traverse validateVideoLinks ifuVideoLinks
      githubIssueUpdate <- traverse (traverse validateGithubIssueUrl) ifuGithubIssueUrl
      now <- liftIO getCurrentTime
      let effectiveClosureReason = case ifuClosureReason of
            Nothing -> ME.internalFeedbackReportClosureReason report
            Just _ -> fromMaybe Nothing closureReasonUpdate
      when (stateUpdate == Just "closed" && not (hasMeaningful effectiveClosureReason)) $
        throwError err400 { errBody = "Closing a report requires a closure reason" }
      when (stateUpdate == Just "duplicate" && fromMaybe (ME.internalFeedbackReportDuplicateOf report) duplicateUpdate == Nothing) $
        throwError err400 { errBody = "A duplicate report must link to its canonical report" }
      let feedbackUpdates = catMaybes
            [ fmap (ME.FeedbackTitle =.) titleUpdate
            , fmap (ME.FeedbackDescription =.) descriptionUpdate
            , fmap (\value -> ME.FeedbackCategoryId =. Just value) categoryUpdate
            , fmap (\value -> ME.FeedbackSeverityId =. Just value) proposedSeverityUpdate
            ]
          reportUpdates = catMaybes
            [ fmap (ME.InternalFeedbackReportReportType =.) reportTypeUpdate
            , fmap (ME.InternalFeedbackReportModuleName =.) moduleUpdate
            , fmap (ME.InternalFeedbackReportFeatureName =.) featureUpdate
            , fmap (ME.InternalFeedbackReportEnvironment =.) environmentUpdate
            , fmap (ME.InternalFeedbackReportUrlOrScreen =.) urlUpdate
            , fmap (ME.InternalFeedbackReportPlatform =.) platformUpdate
            , fmap (ME.InternalFeedbackReportDevice =.) deviceUpdate
            , fmap (ME.InternalFeedbackReportBrowser =.) browserUpdate
            , fmap (ME.InternalFeedbackReportLanguage =.) languageUpdate
            , fmap (ME.InternalFeedbackReportAccountRole =.) roleUpdate
            , fmap (ME.InternalFeedbackReportReproductionSteps =.) reproductionUpdate
            , fmap (ME.InternalFeedbackReportExpectedResult =.) expectedUpdate
            , fmap (ME.InternalFeedbackReportActualResult =.) actualUpdate
            , fmap (ME.InternalFeedbackReportFrequency =.) frequencyUpdate
            , fmap (ME.InternalFeedbackReportBlocking =.) ifuBlocking
            , fmap (ME.InternalFeedbackReportVideoLinks =.) videoLinksUpdate
            , fmap (ME.InternalFeedbackReportState =.) stateUpdate
            , fmap (ME.InternalFeedbackReportAuthoritativeSeverityId =.) authoritativeSeverityUpdate
            , fmap (ME.InternalFeedbackReportPriority =.) priorityUpdate
            , fmap (ME.InternalFeedbackReportAssignedTo =.) (fmap (fmap toSqlKey) assignedToUpdate)
            , fmap (ME.InternalFeedbackReportDuplicateOf =.) duplicateUpdate
            , fmap (ME.InternalFeedbackReportResolution =.) resolutionUpdate
            , fmap (ME.InternalFeedbackReportRetestResult =.) retestResultUpdate
            , fmap (ME.InternalFeedbackReportClosureReason =.) closureReasonUpdate
            , fmap (ME.InternalFeedbackReportGithubIssueUrl =.) githubIssueUpdate
            ]
          closedUpdate = case stateUpdate of
            Just "closed" -> [ME.InternalFeedbackReportClosedAt =. Just now]
            Just _ | state == "closed" -> [ME.InternalFeedbackReportClosedAt =. Nothing]
            _ -> []
      updateResult <- withPool $ do
        planActive <- lockActiveAuditPlanForReport report
        if not planActive
          then pure (Left ("finalized" :: Text))
          else do
            changed <- updateWhereCount
              [ ME.InternalFeedbackReportId ==. reportKey
              , ME.InternalFeedbackReportVersion ==. ME.internalFeedbackReportVersion report
              ]
              ( reportUpdates ++ closedUpdate ++
                [ ME.InternalFeedbackReportVersion +=. 1
                , ME.InternalFeedbackReportUpdatedAt =. now
                ]
              )
            if changed /= 1
              then pure (Left "changed")
              else do
                unless (null feedbackUpdates) (update feedbackKey feedbackUpdates)
                insert_ ME.InternalFeedbackHistory
                  { ME.internalFeedbackHistoryReportId = reportKey
                  , ME.internalFeedbackHistoryActorPartyId = auPartyId user
                  , ME.internalFeedbackHistoryAction = "updated"
                  , ME.internalFeedbackHistoryPreviousState = Just state
                  , ME.internalFeedbackHistoryNewState = stateUpdate
                  , ME.internalFeedbackHistoryMetadata = Just (changedFieldMetadata updateRequest)
                  , ME.internalFeedbackHistoryCreatedAt = now
                  }
                pure (Right ())
      case updateResult of
        Left "finalized" -> throwError finalizedReportMutationConflict
        Left "changed" -> throwError err409
          { errBody = "Report changed during this update; reload it before retrying" }
        Left _ -> throwError err500
        Right () -> pure ()
      recordAudit reportEnt "updated" (Just $ object ["state" .= stateUpdate])
      forM_ stateUpdate $ \newState -> notifyReporterForState reportEnt newState
      refreshedReport <- withPool $ getJustEntity reportKey
      refreshedFeedback <- withPool $ getJustEntity feedbackKey
      buildReportDTO refreshedReport refreshedFeedback

    submitReportH rawReportId = do
      ensureInternalAccess
      (reportEnt@(Entity reportKey report), Entity feedbackKey feedback) <- loadAccessibleReport rawReportId
      unless (ME.internalFeedbackReportReporterPartyId report == auPartyId user || isAdminUser) $
        throwError err404
      unless (ME.internalFeedbackReportState report == "draft") $
        throwError err409 { errBody = "Only draft reports can be submitted" }
      validateSubmissionCompleteness report feedback
      now <- liftIO getCurrentTime
      submissionResult <- withPool $ do
        planActive <- lockActiveAuditPlanForReport report
        if not planActive
          then pure (Left ("finalized" :: Text))
          else do
            changed <- updateWhereCount
              [ ME.InternalFeedbackReportId ==. reportKey
              , ME.InternalFeedbackReportVersion ==. ME.internalFeedbackReportVersion report
              , ME.InternalFeedbackReportState ==. "draft"
              ]
              [ ME.InternalFeedbackReportState =. "received"
              , ME.InternalFeedbackReportSubmittedAt =. Just now
              , ME.InternalFeedbackReportVersion +=. 1
              , ME.InternalFeedbackReportUpdatedAt =. now
              ]
            if changed /= 1
              then pure (Left "changed")
              else do
                insert_ ME.InternalFeedbackHistory
                  { ME.internalFeedbackHistoryReportId = reportKey
                  , ME.internalFeedbackHistoryActorPartyId = auPartyId user
                  , ME.internalFeedbackHistoryAction = "submitted"
                  , ME.internalFeedbackHistoryPreviousState = Just "draft"
                  , ME.internalFeedbackHistoryNewState = Just "submitted"
                  , ME.internalFeedbackHistoryMetadata = Nothing
                  , ME.internalFeedbackHistoryCreatedAt = now
                  }
                insert_ ME.InternalFeedbackHistory
                  { ME.internalFeedbackHistoryReportId = reportKey
                  , ME.internalFeedbackHistoryActorPartyId = auPartyId user
                  , ME.internalFeedbackHistoryAction = "received"
                  , ME.internalFeedbackHistoryPreviousState = Just "submitted"
                  , ME.internalFeedbackHistoryNewState = Just "received"
                  , ME.internalFeedbackHistoryMetadata = Nothing
                  , ME.internalFeedbackHistoryCreatedAt = now
                  }
                insertReporterNotification report "internal_feedback_received" "Reporte recibido" "Tu reporte fue recibido y quedó disponible para revisión."
                pure (Right ())
      case submissionResult of
        Left "finalized" -> throwError finalizedReportMutationConflict
        Left "changed" -> throwError err409
          { errBody = "Report changed before submission; reload it before retrying" }
        Left _ -> throwError err500
        Right () -> pure ()
      enqueueTeamForReport reportEnt
      recordAudit reportEnt "submitted" Nothing
      refreshed <- withPool $ getJustEntity reportKey
      buildReportDTO refreshed (Entity feedbackKey feedback)

    createCommentH rawReportId InternalFeedbackCommentCreate{..} = do
      ensureInternalAccess
      (reportEnt@(Entity reportKey report), _) <- loadAccessibleReport rawReportId
      body <- validateInternalText "comment" 5000 ifccBody
      kind <- validateCommentKind (fromMaybe "comment" ifccKind)
      when (kind == "information_request" && not isAdminUser) $
        throwError err403 { errBody = "Only administrators may request additional information" }
      when (kind == "information_request" && ME.internalFeedbackReportState report `notElem`
        ["received", "confirmed", "prioritized", "in_progress"]) $
        throwError err409 { errBody = "Additional information cannot be requested from the current state" }
      when (kind == "information_response" && ME.internalFeedbackReportReporterPartyId report /= auPartyId user) $
        throwError err403 { errBody = "Only the reporter may answer an information request" }
      now <- liftIO getCurrentTime
      let nextState = case kind of
            "information_request" -> Just "needs_information"
            "information_response" | ME.internalFeedbackReportState report == "needs_information" -> Just "received"
            _ -> Nothing
      entResult <- withPool $ do
        planActive <- lockActiveAuditPlanForReport report
        if not planActive
          then pure (Left ("finalized" :: Text))
          else do
            stateAvailable <- case nextState of
              Nothing -> pure True
              Just target -> do
                changed <- updateWhereCount
                  [ ME.InternalFeedbackReportId ==. reportKey
                  , ME.InternalFeedbackReportVersion ==. ME.internalFeedbackReportVersion report
                  ]
                  [ ME.InternalFeedbackReportState =. target
                  , ME.InternalFeedbackReportVersion +=. 1
                  , ME.InternalFeedbackReportUpdatedAt =. now
                  ]
                pure (changed == 1)
            if not stateAvailable
              then pure (Left "changed")
              else do
                commentId <- insert ME.InternalFeedbackComment
                  { ME.internalFeedbackCommentReportId = reportKey
                  , ME.internalFeedbackCommentAuthorPartyId = auPartyId user
                  , ME.internalFeedbackCommentKind = kind
                  , ME.internalFeedbackCommentBody = body
                  , ME.internalFeedbackCommentCreatedAt = now
                  }
                insert_ ME.InternalFeedbackHistory
                  { ME.internalFeedbackHistoryReportId = reportKey
                  , ME.internalFeedbackHistoryActorPartyId = auPartyId user
                  , ME.internalFeedbackHistoryAction = kind
                  , ME.internalFeedbackHistoryPreviousState = Just (ME.internalFeedbackReportState report)
                  , ME.internalFeedbackHistoryNewState = nextState
                  , ME.internalFeedbackHistoryMetadata = Nothing
                  , ME.internalFeedbackHistoryCreatedAt = now
                  }
                Right <$> getJustEntity commentId
      ent <- case entResult of
        Left "finalized" -> throwError finalizedReportMutationConflict
        Left "changed" -> throwError err409
          { errBody = "Report changed before the comment transition; reload it before retrying" }
        Left _ -> throwError err500
        Right entity -> pure entity
      if kind == "information_request"
        then withPool $ insertReporterNotification report "internal_feedback_needs_information" "Se necesita más información" "Revisa tu reporte y responde la solicitud del equipo."
        else when (kind == "information_response") (enqueueTeamNotification reportEnt "internal_feedback_information_response" "immediate")
      recordAudit reportEnt kind Nothing
      toCommentDTO ent

    uploadEvidenceH rawReportId InternalFeedbackEvidencePayload{..} = do
      ensureInternalAccess
      (reportEnt@(Entity reportKey _), _) <- loadAccessibleReport rawReportId
      (safeName, mediaType) <- either throwError pure $
        validateFeedbackAttachmentMetadata (fdFileName ifepAttachment) (fdFileCType ifepAttachment)
      size <- liftIO $ getFileSize (fdPayload ifepAttachment)
      either throwError pure (validateFeedbackAttachmentSize size)
      caption <- validateOptionalInternalText "caption" 1000 ifepCaption
      uploadRoot <- liftIO internalFeedbackUploadRoot
      now <- liftIO getCurrentTime
      entResult <- withPool $ do
        planActive <- lockActiveAuditPlanForReport (entityVal reportEnt)
        if not planActive
          then pure Nothing
          else do
            storedPath <- liftIO $ storeInternalAttachment uploadRoot rawReportId safeName ifepAttachment
            evidenceId <- insert ME.InternalFeedbackEvidence
              { ME.internalFeedbackEvidenceReportId = reportKey
              , ME.internalFeedbackEvidenceUploadedBy = auPartyId user
              , ME.internalFeedbackEvidenceKind = "attachment"
              , ME.internalFeedbackEvidenceOriginalFileName = Just safeName
              , ME.internalFeedbackEvidenceStoragePath = Just (T.pack storedPath)
              , ME.internalFeedbackEvidenceContentType = Just mediaType
              , ME.internalFeedbackEvidenceSizeBytes = Just (fromIntegral size)
              , ME.internalFeedbackEvidenceExternalUrl = Nothing
              , ME.internalFeedbackEvidenceCaption = caption
              , ME.internalFeedbackEvidenceCreatedAt = now
              }
            insertHistory reportKey "evidence_added" Nothing Nothing
            Just <$> getJustEntity evidenceId
      ent <- maybe (throwError finalizedReportMutationConflict) pure entResult
      recordAudit reportEnt "evidence_added" Nothing
      pure (toEvidenceDTO ent)

    createEvidenceLinkH rawReportId InternalFeedbackEvidenceLinkCreate{..} = do
      ensureInternalAccess
      (reportEnt@(Entity reportKey _), _) <- loadAccessibleReport rawReportId
      url <- validateExternalEvidenceUrl ifelUrl
      kind <- validateChoice "evidence kind" ["external_link", "video_link", "retest"] (fromMaybe "video_link" ifelKind)
      caption <- validateOptionalInternalText "caption" 1000 ifelCaption
      now <- liftIO getCurrentTime
      entResult <- withPool $ do
        planActive <- lockActiveAuditPlanForReport (entityVal reportEnt)
        if not planActive
          then pure Nothing
          else do
            evidenceId <- insert ME.InternalFeedbackEvidence
              { ME.internalFeedbackEvidenceReportId = reportKey
              , ME.internalFeedbackEvidenceUploadedBy = auPartyId user
              , ME.internalFeedbackEvidenceKind = kind
              , ME.internalFeedbackEvidenceOriginalFileName = Nothing
              , ME.internalFeedbackEvidenceStoragePath = Nothing
              , ME.internalFeedbackEvidenceContentType = Nothing
              , ME.internalFeedbackEvidenceSizeBytes = Nothing
              , ME.internalFeedbackEvidenceExternalUrl = Just url
              , ME.internalFeedbackEvidenceCaption = caption
              , ME.internalFeedbackEvidenceCreatedAt = now
              }
            insertHistory reportKey "evidence_link_added" Nothing Nothing
            Just <$> getJustEntity evidenceId
      ent <- maybe (throwError finalizedReportMutationConflict) pure entResult
      recordAudit reportEnt "evidence_link_added" Nothing
      pure (toEvidenceDTO ent)

    downloadEvidenceH rawReportId rawEvidenceId = do
      ensureInternalAccess
      (Entity reportKey _, _) <- loadAccessibleReport rawReportId
      evidenceKey <- parseInternalKey @ME.InternalFeedbackEvidence rawEvidenceId
      Entity _ evidence <- withPool (getEntity evidenceKey) >>= maybe (throwError err404) pure
      unless (ME.internalFeedbackEvidenceReportId evidence == reportKey) $ throwError err404
      path <- maybe (throwError err404) pure (ME.internalFeedbackEvidenceStoragePath evidence)
      uploadRoot <- liftIO internalFeedbackUploadRoot
      let root = normalise (uploadRoot </> T.unpack rawReportId)
          stored = normalise (T.unpack path)
          relative = makeRelative root stored
      when (relative == ".." || (".." <> [pathSeparator]) `isPrefixText` relative) $
        throwError err403 { errBody = "Attachment storage path is outside the report boundary" }
      fileExists <- liftIO (doesFileExist stored)
      unless fileExists $ throwError err404
      bytes <- liftIO (BL.readFile stored)
      let fileName = fromMaybe "attachment" (ME.internalFeedbackEvidenceOriginalFileName evidence)
      pure (addHeader ("attachment; filename=\"" <> fileName <> "\"") bytes)

    createRetestH rawReportId InternalFeedbackRetestCreate{..} = do
      ensureInternalAccess
      (reportEnt@(Entity reportKey report), _) <- loadAccessibleReport rawReportId
      unless (ME.internalFeedbackReportState report == "ready_for_retest" || isAdminUser) $
        throwError err409 { errBody = "This report is not ready for retesting" }
      result <- validateChoice "retest result" ["passed", "failed", "blocked"] ifrcResult
      explicitExecutionKey <- traverse (parseInternalKey @ME.InternTestExecution) ifrcExecutionId
      validateRetestExecution report explicitExecutionKey
      caseKey <- maybe
        (throwError err400 { errBody = "Retesting requires a linked test case" })
        pure
        (ME.internalFeedbackReportTestCaseId report)
      taskKey <- ensureRetestPlanActive caseKey
      notes <- validateOptionalInternalText "retestNotes" 5000 ifrcNotes
      evidenceSummary <- validateOptionalInternalText "retestEvidenceSummary" 5000 ifrcEvidenceSummary
      unless (hasMeaningful notes && hasMeaningful evidenceSummary) $
        throwError err400 { errBody = "Retesting requires notes and an evidence summary" }
      now <- liftIO getCurrentTime
      entResult <- withPool $ do
        planActive <- lockActiveAuditPlanForTask taskKey
        if not planActive
          then pure Nothing
          else do
            lockInternTestExecutionSequence caseKey
            latest <- selectFirst [ME.InternTestExecutionTestCaseId ==. caseKey]
              [Desc ME.InternTestExecutionExecutionNumber]
            let nextNumber = maybe 1
                  ((+ 1) . ME.internTestExecutionExecutionNumber . entityVal)
                  latest
                executionStatus = case result of
                  "passed" -> "verified"
                  other -> other
            executionKey <- insert ME.InternTestExecution
              { ME.internTestExecutionTestCaseId = caseKey
              , ME.internTestExecutionExecutionNumber = nextNumber
              , ME.internTestExecutionExecutorPartyId = auPartyId user
              , ME.internTestExecutionStatus = executionStatus
              , ME.internTestExecutionActualResult = notes
              , ME.internTestExecutionPersistedStateObserved = Nothing
              , ME.internTestExecutionSideEffectsObserved = Nothing
              , ME.internTestExecutionBlockerReason = if result == "blocked" then notes else Nothing
              , ME.internTestExecutionEvidenceSummary = evidenceSummary
              , ME.internTestExecutionStartedAt = Just now
              , ME.internTestExecutionCompletedAt = Just now
              , ME.internTestExecutionCreatedAt = now
              , ME.internTestExecutionUpdatedAt = now
              }
            retestId <- insert ME.InternalFeedbackRetest
              { ME.internalFeedbackRetestReportId = reportKey
              , ME.internalFeedbackRetestExecutionId = Just executionKey
              , ME.internalFeedbackRetestTesterPartyId = auPartyId user
              , ME.internalFeedbackRetestResult = result
              , ME.internalFeedbackRetestNotes = notes
              , ME.internalFeedbackRetestEvidenceSummary = evidenceSummary
              , ME.internalFeedbackRetestCreatedAt = now
              }
            update reportKey
              [ ME.InternalFeedbackReportRetestResult =. Just result
              , ME.InternalFeedbackReportVersion +=. 1
              , ME.InternalFeedbackReportUpdatedAt =. now
              ]
            insertHistory reportKey "retest_recorded" Nothing (Just result)
            Just <$> getJustEntity retestId
      ent <- maybe (throwError finalizedReportMutationConflict) pure entResult
      enqueueTeamNotification reportEnt "internal_feedback_retest_recorded" "immediate"
      recordAudit reportEnt "retest_recorded" (Just $ object ["result" .= result])
      toRetestDTO ent

    selectVisibleReports mMine = do
      let ownerFilter =
            if isAdminUser && mMine /= Just True
              then []
              else [ME.InternalFeedbackReportReporterPartyId ==. auPartyId user]
      withPool $ selectList ownerFilter [Desc ME.InternalFeedbackReportUpdatedAt, LimitTo 1000]

    loadAccessibleReport rawReportId = do
      reportKey <- parseInternalKey @ME.InternalFeedbackReport rawReportId
      reportEnt@(Entity _ report) <- withPool (getEntity reportKey) >>= maybe (throwError err404) pure
      unless (isAdminUser || ME.internalFeedbackReportReporterPartyId report == auPartyId user) $
        throwError err404
      feedbackEnt <- withPool (getEntity (ME.internalFeedbackReportFeedbackId report)) >>= maybe (throwError err404) pure
      pure (reportEnt, feedbackEnt)

    resolveTraceability mProject mTask mCase mExecution = Traceability
      <$> traverse (parseInternalKey @ME.InternProject) mProject
      <*> traverse (parseInternalKey @ME.InternTask) mTask
      <*> traverse (parseInternalKey @ME.InternTestCase) mCase
      <*> traverse (parseInternalKey @ME.InternTestExecution) mExecution

    validateTraceability Traceability{..} = do
      project <- traverse (withPool . get) traceProjectId
      task <- traverse (withPool . get) traceTaskId
      testCase <- traverse (withPool . get) traceTestCaseId
      execution <- traverse (withPool . get) traceExecutionId
      let missingReference = or
            [ isJust traceProjectId && not (maybe False isJust project)
            , isJust traceTaskId && not (maybe False isJust task)
            , isJust traceTestCaseId && not (maybe False isJust testCase)
            , isJust traceExecutionId && not (maybe False isJust execution)
            ]
      when missingReference $
        throwError err400 { errBody = "One or more traceability references do not exist" }
      testCasePlan <- loadTracePlan testCase
      executionCase <- case execution of
        Just (Just executionValue) ->
          Just <$> loadTraceEntity
            "The execution's test case does not exist"
            (ME.internTestExecutionTestCaseId executionValue)
        _ -> pure Nothing
      executionPlan <- loadTracePlan (Just <$> executionCase)
      case (traceProjectId, traceTaskId, task) of
        (Just projectKey, Just _, Just (Just taskValue))
          | ME.internTaskProjectId taskValue /= projectKey ->
              throwError err400 { errBody = "The task does not belong to the referenced project" }
        _ -> pure ()
      case (testCasePlan, traceTaskId) of
        (Just planValue, Just taskKey)
          | ME.internAuditPlanTaskId planValue /= taskKey ->
              throwError err400 { errBody = "The test case does not belong to the referenced task" }
        _ -> pure ()
      case (testCasePlan, traceProjectId) of
        (Just planValue, Just projectKey)
          | ME.internAuditPlanProjectId planValue /= projectKey ->
              throwError err400 { errBody = "The test case does not belong to the referenced project" }
        _ -> pure ()
      case (traceExecutionId, execution, traceTestCaseId) of
        (Just _, Just (Just executionValue), Just caseKey)
          | ME.internTestExecutionTestCaseId executionValue /= caseKey ->
              throwError err400 { errBody = "The execution does not belong to the referenced test case" }
        _ -> pure ()
      case (executionPlan, traceTaskId) of
        (Just planValue, Just taskKey)
          | ME.internAuditPlanTaskId planValue /= taskKey ->
              throwError err400 { errBody = "The execution does not belong to the referenced task" }
        _ -> pure ()
      case (executionPlan, traceProjectId) of
        (Just planValue, Just projectKey)
          | ME.internAuditPlanProjectId planValue /= projectKey ->
              throwError err400 { errBody = "The execution does not belong to the referenced project" }
        _ -> pure ()
      unless isAdminUser $ case (traceTaskId, task) of
        (Just taskKey, Just (Just taskValue))
          | ME.internTaskActivationStatus taskValue == "active"
            && ME.internTaskAssignedTo taskValue == Just (auPartyId user) -> do
              auditPlan <- withPool $ getBy (ME.UniqueInternAuditPlanTask taskKey)
              forM_ auditPlan $ \(Entity _ plan) ->
                unless (ME.internAuditPlanStatus plan == "active") $
                  throwError err409 { errBody = "Finalized audit plans do not accept new reports" }
        _ -> throwError err403 { errBody = "Intern reports must link to the reporter's active assigned task" }
      where
        loadTracePlan Nothing = pure Nothing
        loadTracePlan (Just Nothing) = pure Nothing
        loadTracePlan (Just (Just caseValue)) =
          Just <$> loadTraceEntity
            "The test case's audit plan does not exist"
            (ME.internTestCasePlanId caseValue)

        loadTraceEntity errorMessage key =
          withPool (get key) >>= maybe
            (throwError err400 { errBody = errorMessage })
            pure

    validateRetestExecution _ Nothing = pure ()
    validateRetestExecution report (Just executionKey) = do
      execution <- withPool (get executionKey) >>= maybe (throwError err400) pure
      case ME.internalFeedbackReportTestCaseId report of
        Just caseKey | ME.internTestExecutionTestCaseId execution == caseKey -> pure ()
        _ -> throwError err400 { errBody = "Retest execution must belong to the report's test case" }

    ensureRetestPlanActive caseKey = do
      testCase <- withPool (get caseKey) >>= maybe (throwError err400) pure
      plan <- withPool (get (ME.internTestCasePlanId testCase)) >>= maybe (throwError err400) pure
      unless (ME.internAuditPlanStatus plan == "active") $
        throwError err409 { errBody = "Finalized audit plans do not accept retests" }
      pure (ME.internAuditPlanTaskId plan)

    validateSubmissionCompleteness report feedback = do
      unless (hasMeaningful (Just (feedbackTitle feedback)) && hasMeaningful (Just (feedbackDescription feedback))) $
        throwError err400 { errBody = "Title and description are required" }
      when (ME.internalFeedbackReportReportType report == "error") $ do
        unless (all hasMeaningful
          [ ME.internalFeedbackReportReproductionSteps report
          , ME.internalFeedbackReportExpectedResult report
          , ME.internalFeedbackReportActualResult report
          ]) $
          throwError err400 { errBody = "Error reports require reproduction steps, expected result, and actual result" }
        unless (isJust (ME.internalFeedbackReportTestCaseId report)) $
          throwError err400 { errBody = "Intern error reports must link to a test case" }

    buildReportDTO reportEnt@(Entity reportKey report) feedbackEnt@(Entity _ feedback) = do
      summary <- toSummaryDTO reportEnt
      evidence <- withPool $ selectList [ME.InternalFeedbackEvidenceReportId ==. reportKey]
        [Asc ME.InternalFeedbackEvidenceCreatedAt]
      comments <- withPool $ selectList [ME.InternalFeedbackCommentReportId ==. reportKey]
        [Asc ME.InternalFeedbackCommentCreatedAt]
      history <- withPool $ selectList [ME.InternalFeedbackHistoryReportId ==. reportKey]
        [Asc ME.InternalFeedbackHistoryCreatedAt]
      retests <- withPool $ selectList [ME.InternalFeedbackRetestReportId ==. reportKey]
        [Desc ME.InternalFeedbackRetestCreatedAt]
      duplicates <- potentialDuplicates reportEnt feedbackEnt
      commentDtos <- mapM toCommentDTO comments
      historyDtos <- mapM toHistoryDTO history
      retestDtos <- mapM toRetestDTO retests
      pure InternalFeedbackDTO
        { ifrSummary = summary
        , ifrDescription = feedbackDescription feedback
        , ifrCategoryId = toPathPiece <$> feedbackCategoryId feedback
        , ifrUrlOrScreen = ME.internalFeedbackReportUrlOrScreen report
        , ifrDevice = ME.internalFeedbackReportDevice report
        , ifrBrowser = ME.internalFeedbackReportBrowser report
        , ifrLanguage = ME.internalFeedbackReportLanguage report
        , ifrAccountRole = ME.internalFeedbackReportAccountRole report
        , ifrReproductionSteps = ME.internalFeedbackReportReproductionSteps report
        , ifrExpectedResult = ME.internalFeedbackReportExpectedResult report
        , ifrActualResult = ME.internalFeedbackReportActualResult report
        , ifrFrequency = ME.internalFeedbackReportFrequency report
        , ifrAssignedTo = fmap fromSqlKey (ME.internalFeedbackReportAssignedTo report)
        , ifrResolution = ME.internalFeedbackReportResolution report
        , ifrRetestResult = ME.internalFeedbackReportRetestResult report
        , ifrClosureReason = ME.internalFeedbackReportClosureReason report
        , ifrGithubIssueUrl = ME.internalFeedbackReportGithubIssueUrl report
        , ifrVideoLinks = ME.internalFeedbackReportVideoLinks report
        , ifrSubmittedAt = ME.internalFeedbackReportSubmittedAt report
        , ifrClosedAt = ME.internalFeedbackReportClosedAt report
        , ifrEvidence = map toEvidenceDTO evidence
        , ifrComments = commentDtos
        , ifrHistory = historyDtos
        , ifrRetests = retestDtos
        , ifrPotentialDuplicates = duplicates
        }

    toSummaryDTO (Entity reportKey report) = do
      feedback <- withPool $ get (ME.internalFeedbackReportFeedbackId report)
      reporter <- withPool $ get (ME.internalFeedbackReportReporterPartyId report)
      canonicalDuplicate <- case ME.internalFeedbackReportDuplicateOf report of
        Nothing -> pure Nothing
        Just feedbackKey -> fmap (toPathPiece . entityKey) <$> withPool (getBy (ME.UniqueInternalFeedbackReport feedbackKey))
      pure InternalFeedbackSummaryDTO
        { ifsId = toPathPiece reportKey
        , ifsTitle = maybe "Reporte" feedbackTitle feedback
        , ifsReportType = ME.internalFeedbackReportReportType report
        , ifsState = ME.internalFeedbackReportState report
        , ifsModuleName = ME.internalFeedbackReportModuleName report
        , ifsFeatureName = ME.internalFeedbackReportFeatureName report
        , ifsEnvironment = ME.internalFeedbackReportEnvironment report
        , ifsPlatform = ME.internalFeedbackReportPlatform report
        , ifsProposedSeverityId = toPathPiece <$> ME.internalFeedbackReportProposedSeverityId report
        , ifsAuthoritativeSeverityId = toPathPiece <$> ME.internalFeedbackReportAuthoritativeSeverityId report
        , ifsPriority = ME.internalFeedbackReportPriority report
        , ifsBlocking = ME.internalFeedbackReportBlocking report
        , ifsReporterPartyId = fromSqlKey (ME.internalFeedbackReportReporterPartyId report)
        , ifsReporterName = maybe "Usuario" M.partyDisplayName reporter
        , ifsInternshipProjectId = toPathPiece <$> ME.internalFeedbackReportInternshipProjectId report
        , ifsInternshipTaskId = toPathPiece <$> ME.internalFeedbackReportInternshipTaskId report
        , ifsTestCaseId = toPathPiece <$> ME.internalFeedbackReportTestCaseId report
        , ifsTestExecutionId = toPathPiece <$> ME.internalFeedbackReportTestExecutionId report
        , ifsDuplicateOf = canonicalDuplicate
        , ifsCreatedAt = ME.internalFeedbackReportCreatedAt report
        , ifsUpdatedAt = ME.internalFeedbackReportUpdatedAt report
        }

    potentialDuplicates (Entity currentKey report) (Entity _ feedback) = do
      let privacyFilters =
            if isAdminUser
              then []
              else [ME.InternalFeedbackReportReporterPartyId ==. auPartyId user]
      candidates <- withPool $ selectList
        ([ ME.InternalFeedbackReportId !=. currentKey
         , ME.InternalFeedbackReportState !=. "draft"
         ] ++ privacyFilters)
        [Desc ME.InternalFeedbackReportUpdatedAt, LimitTo 200]
      scored <- forM candidates $ \candidate@(Entity _ other) -> do
        otherFeedback <- withPool $ get (ME.internalFeedbackReportFeedbackId other)
        let score = maybe 0 (duplicateSimilarity report feedback other) otherFeedback
        summary <- toSummaryDTO candidate
        pure (score, summary)
      pure $ map snd $ take 5 $ reverse $ sortOn fst
        [ pair | pair@(score, _) <- scored, score >= 55 ]

    duplicateSimilarity report feedback other otherFeedback =
      let titleScore = jaccardPercent (feedbackTitle feedback) (feedbackTitle otherFeedback)
          bodyScore = jaccardPercent (feedbackDescription feedback) (feedbackDescription otherFeedback)
          moduleScore = if normalizedEqual (ME.internalFeedbackReportModuleName report) (ME.internalFeedbackReportModuleName other) then 100 else 0
          featureScore = if fmap normalizeWords (ME.internalFeedbackReportFeatureName report)
            == fmap normalizeWords (ME.internalFeedbackReportFeatureName other) then 100 else 0
      in (titleScore * 40 + bodyScore * 40 + moduleScore * 15 + featureScore * 5) `div` 100

    toEvidenceDTO (Entity key evidence) = InternalFeedbackEvidenceDTO
      { ifeId = toPathPiece key
      , ifeKind = ME.internalFeedbackEvidenceKind evidence
      , ifeOriginalFileName = ME.internalFeedbackEvidenceOriginalFileName evidence
      , ifeContentType = ME.internalFeedbackEvidenceContentType evidence
      , ifeSizeBytes = ME.internalFeedbackEvidenceSizeBytes evidence
      , ifeExternalUrl = ME.internalFeedbackEvidenceExternalUrl evidence
      , ifeCaption = ME.internalFeedbackEvidenceCaption evidence
      , ifeUploadedBy = fromSqlKey (ME.internalFeedbackEvidenceUploadedBy evidence)
      , ifeCreatedAt = ME.internalFeedbackEvidenceCreatedAt evidence
      }

    toCommentDTO (Entity key comment) = do
      name <- partyName (ME.internalFeedbackCommentAuthorPartyId comment)
      pure InternalFeedbackCommentDTO
        { ifcmId = toPathPiece key
        , ifcmAuthorPartyId = fromSqlKey (ME.internalFeedbackCommentAuthorPartyId comment)
        , ifcmAuthorName = name
        , ifcmKind = ME.internalFeedbackCommentKind comment
        , ifcmBody = ME.internalFeedbackCommentBody comment
        , ifcmCreatedAt = ME.internalFeedbackCommentCreatedAt comment
        }

    toHistoryDTO (Entity key history) = do
      name <- partyName (ME.internalFeedbackHistoryActorPartyId history)
      pure InternalFeedbackHistoryDTO
        { ifhId = toPathPiece key
        , ifhActorPartyId = fromSqlKey (ME.internalFeedbackHistoryActorPartyId history)
        , ifhActorName = name
        , ifhAction = ME.internalFeedbackHistoryAction history
        , ifhPreviousState = ME.internalFeedbackHistoryPreviousState history
        , ifhNewState = ME.internalFeedbackHistoryNewState history
        , ifhMetadata = ME.internalFeedbackHistoryMetadata history
        , ifhCreatedAt = ME.internalFeedbackHistoryCreatedAt history
        }

    toRetestDTO (Entity key retest) = do
      name <- partyName (ME.internalFeedbackRetestTesterPartyId retest)
      pure InternalFeedbackRetestDTO
        { ifrtId = toPathPiece key
        , ifrtExecutionId = toPathPiece <$> ME.internalFeedbackRetestExecutionId retest
        , ifrtTesterPartyId = fromSqlKey (ME.internalFeedbackRetestTesterPartyId retest)
        , ifrtTesterName = name
        , ifrtResult = ME.internalFeedbackRetestResult retest
        , ifrtNotes = ME.internalFeedbackRetestNotes retest
        , ifrtEvidenceSummary = ME.internalFeedbackRetestEvidenceSummary retest
        , ifrtCreatedAt = ME.internalFeedbackRetestCreatedAt retest
        }

    partyName key = withPool $ maybe "Usuario" M.partyDisplayName <$> get key

    insertHistory reportKey action previous next = do
      now <- liftIO getCurrentTime
      insert_ ME.InternalFeedbackHistory
        { ME.internalFeedbackHistoryReportId = reportKey
        , ME.internalFeedbackHistoryActorPartyId = auPartyId user
        , ME.internalFeedbackHistoryAction = action
        , ME.internalFeedbackHistoryPreviousState = previous
        , ME.internalFeedbackHistoryNewState = next
        , ME.internalFeedbackHistoryMetadata = Nothing
        , ME.internalFeedbackHistoryCreatedAt = now
        }

    insertReporterNotification report notificationType title body = do
      now <- liftIO getCurrentTime
      insert_ M.Notification
        { M.notificationRecipientPartyId = ME.internalFeedbackReportReporterPartyId report
        , M.notificationNotifType = notificationType
        , M.notificationTitle = title
        , M.notificationBody = body
        , M.notificationTargetType = Just "internal_feedback_report"
        , M.notificationTargetId = Nothing
        , M.notificationIsRead = False
        , M.notificationCreatedAt = now
        }

    notifyReporterForState (Entity _ report) newState = do
      let message = case newState of
            "needs_information" -> Just ("internal_feedback_needs_information", "Se necesita más información", "El equipo solicitó más información sobre tu reporte.")
            "ready_for_retest" -> Just ("internal_feedback_ready_for_retest", "Reporte listo para retest", "Ya puedes repetir la prueba y registrar el resultado.")
            "closed" -> Just ("internal_feedback_closed", "Reporte cerrado", "Tu reporte fue cerrado. Revisa el motivo y el historial.")
            "received" -> Just ("internal_feedback_reopened", "Reporte reabierto", "Tu reporte fue reabierto para continuar el seguimiento.")
            _ -> Just ("internal_feedback_state_changed", "Estado del reporte actualizado", "El estado de tu reporte cambió a: " <> newState)
      forM_ message $ \(kind, title, body) -> withPool (insertReporterNotification report kind title body)

    enqueueTeamForReport reportEnt@(Entity _ report) = do
      severityCode <- case ME.internalFeedbackReportProposedSeverityId report of
        Nothing -> pure Nothing
        Just severityKey -> fmap Catalog.feedbackSeverityCode <$> withPool (get severityKey)
      let immediate = ME.internalFeedbackReportBlocking report
            || maybe False ((`elem` ["critical", "blocker"]) . T.toLower) severityCode
      enqueueTeamNotification reportEnt "internal_feedback_received" (if immediate then "immediate" else "digest")

    enqueueTeamNotification (Entity reportKey _) template deliveryMode = do
      now <- liftIO getCurrentTime
      testTransport <- liftIO internalFeedbackTestRuntime
      groups <- withPool $ mapM selectCanonicalPartyIdsByRole [M.Admin, M.Manager, M.StudioManager]
      let recipients = nub (concat groups)
          (notificationTitle, notificationBody) = case template of
            "internal_feedback_received" -> ("Reporte crítico o bloqueante recibido", "Se recibió un reporte que requiere revisión inmediata.")
            "internal_feedback_information_response" -> ("Información adicional recibida", "La persona reportante respondió una solicitud de información.")
            "internal_feedback_retest_recorded" -> ("Retest registrado", "Se registró un nuevo resultado de retest para un reporte interno.")
            _ -> ("Actualización de reporte interno", "Hay una actualización que requiere revisión del equipo.")
      withPool $ forM_ recipients $ \recipient -> do
        insert_ ME.InternAuditNotificationOutbox
          { ME.internAuditNotificationOutboxRecipientPartyId = recipient
          , ME.internAuditNotificationOutboxReportId = Just reportKey
          , ME.internAuditNotificationOutboxPlanId = Nothing
          , ME.internAuditNotificationOutboxTemplateKey = template
          , ME.internAuditNotificationOutboxDeliveryMode = deliveryMode
          , ME.internAuditNotificationOutboxTestTransport = testTransport
          , ME.internAuditNotificationOutboxPayload = if testTransport
              then "{\"transport\":\"test\"}"
              else "{\"transport\":\"production\"}"
          , ME.internAuditNotificationOutboxDispatchedAt = Nothing
          , ME.internAuditNotificationOutboxCreatedAt = now
          }
        when (deliveryMode == "immediate") $ insert_ M.Notification
          { M.notificationRecipientPartyId = recipient
          , M.notificationNotifType = template
          , M.notificationTitle = notificationTitle
          , M.notificationBody = notificationBody
          , M.notificationTargetType = Just "internal_feedback_report"
          , M.notificationTargetId = Nothing
          , M.notificationIsRead = False
          , M.notificationCreatedAt = now
          }

    recordAudit (Entity reportKey _) action metadata = do
      pool <- asks envPool
      liftIO $ runSqlPool
        (recordUserActivity (Just (auPartyId user)) "internal_feedback_report" (toPathPiece reportKey) action metadata)
        pool

    resolveDuplicateTarget currentReportKey rawTarget = do
      targetKey <- parseInternalKey @ME.InternalFeedbackReport rawTarget
      when (targetKey == currentReportKey) $
        throwError err400 { errBody = "A report cannot be a duplicate of itself" }
      target <- withPool (get targetKey) >>= maybe (throwError err400) pure
      pure (ME.internalFeedbackReportFeedbackId target)

    matchesSummary stateFilter mModule mQuery summary =
      maybe True (== ifsState summary) stateFilter
        && maybe True (normalizedEqual (ifsModuleName summary)) mModule
        && maybe True (summaryContains summary) (normalizeInternalOptional 200 mQuery)

    summaryContains summary query =
      let needle = normalizeWords query
          haystack = normalizeWords $ T.intercalate " "
            [ ifsTitle summary
            , ifsModuleName summary
            , fromMaybe "" (ifsFeatureName summary)
            , ifsReportType summary
            , ifsState summary
            ]
      in needle `T.isInfixOf` haystack

    summaryCsv summary = T.intercalate ","
      [ csvField (ifsId summary)
      , csvField (ifsTitle summary)
      , csvField (ifsReportType summary)
      , csvField (ifsState summary)
      , csvField (ifsModuleName summary)
      , csvField (fromMaybe "" (ifsFeatureName summary))
      , csvField (ifsEnvironment summary)
      , csvField (ifsPlatform summary)
      , csvField (fromMaybe "" (ifsPriority summary))
      , csvField (if ifsBlocking summary then "true" else "false")
      , csvField (ifsReporterName summary)
      , csvField (T.pack (show (ifsCreatedAt summary)))
      ]

    storeInternalAttachment uploadRoot rawReportId safeName FileData{fdPayload = payload} = do
      token <- toText <$> nextRandom
      let destDir = uploadRoot </> T.unpack rawReportId
          destPath = destDir </> T.unpack token <> "-" <> T.unpack safeName
      createDirectoryIfMissing True destDir
      BL.readFile payload >>= BL.writeFile destPath
      pure destPath

data Traceability = Traceability
  { traceProjectId   :: Maybe ME.InternProjectId
  , traceTaskId      :: Maybe ME.InternTaskId
  , traceTestCaseId  :: Maybe ME.InternTestCaseId
  , traceExecutionId :: Maybe ME.InternTestExecutionId
  }

lockActiveAuditPlanForTask :: ME.InternTaskId -> SqlPersistT IO Bool
lockActiveAuditPlanForTask taskKey = do
  rows <- (rawSql
    "SELECT status FROM intern_audit_plan WHERE task_id = ? FOR UPDATE"
    [toPersistValue taskKey]
    :: SqlPersistT IO [Single Text])
  pure $ case rows of
    [] -> True
    [Single "active"] -> True
    _ -> False

lockActiveAuditPlanForReport :: ME.InternalFeedbackReport -> SqlPersistT IO Bool
lockActiveAuditPlanForReport report =
  maybe (pure True) lockActiveAuditPlanForTask
    (ME.internalFeedbackReportInternshipTaskId report)

finalizedReportMutationConflict :: ServerError
finalizedReportMutationConflict = err409
  { errBody = "Finalized audit plans do not accept workflow changes" }

internalReportTypes :: [Text]
internalReportTypes =
  [ "error", "suggestion", "idea", "question", "accessibility", "permissions"
  , "performance", "content_translation"
  ]

-- The feedback-category catalog owns the user-visible report taxonomy. This
-- closed mapping is only the wire adapter needed to preserve the requested
-- report-type codes and to reject mismatched category/type submissions.
internalReportTypeForCategoryCode :: Text -> Maybe Text
internalReportTypeForCategoryCode rawCode = case T.toLower (T.strip rawCode) of
  "bug" -> Just "error"
  "suggestion" -> Just "suggestion"
  "idea" -> Just "idea"
  "question" -> Just "question"
  "accessibility" -> Just "accessibility"
  "permissions" -> Just "permissions"
  "performance" -> Just "performance"
  "content_translation" -> Just "content_translation"
  _ -> Nothing

internalReportStates :: [Text]
internalReportStates =
  [ "draft", "submitted", "received", "needs_information", "confirmed", "prioritized"
  , "in_progress", "ready_for_retest", "verified", "closed", "duplicate", "discarded"
  ]

validateReportType :: MonadError ServerError m => Text -> m Text
validateReportType = validateChoice "reportType" internalReportTypes

validateInternalReportState :: MonadError ServerError m => Text -> Text -> m Text
validateInternalReportState fieldName = validateChoice fieldName internalReportStates

validateStateTransition :: MonadError ServerError m => Text -> Text -> m Text
validateStateTransition previous rawNext = do
  next <- validateInternalReportState "state" rawNext
  let allowed = case previous of
        "received" -> ["needs_information", "confirmed", "duplicate", "discarded"]
        "needs_information" -> ["received", "confirmed", "discarded"]
        "confirmed" -> ["prioritized", "in_progress", "duplicate", "discarded"]
        "prioritized" -> ["in_progress", "discarded"]
        "in_progress" -> ["ready_for_retest", "discarded"]
        "ready_for_retest" -> ["verified", "in_progress"]
        "verified" -> ["closed", "in_progress"]
        "closed" -> ["received"]
        "duplicate" -> ["received"]
        "discarded" -> ["received"]
        _ -> []
  if next == previous || next `elem` allowed
    then pure next
    else throwError err409 { errBody = "Unsupported report state transition" }

validatePriority :: MonadError ServerError m => Text -> m Text
validatePriority = validateChoice "priority" ["low", "medium", "high", "urgent"]

validateEnvironment :: MonadError ServerError m => Text -> m Text
validateEnvironment = validateChoice "environment" ["local", "test", "staging", "production-read-only"]

validateCommentKind :: MonadError ServerError m => Text -> m Text
validateCommentKind = validateChoice "comment kind" ["comment", "information_request", "information_response"]

validateChoice :: MonadError ServerError m => Text -> [Text] -> Text -> m Text
validateChoice fieldName allowed raw =
  let normalized = T.toLower (T.strip raw)
  in if normalized `elem` allowed
    then pure normalized
    else throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " has an unsupported value")) }

validateInternalText :: MonadError ServerError m => Text -> Int -> Text -> m Text
validateInternalText fieldName maxLength raw =
  let normalized = T.strip raw
  in if T.null normalized
    then throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " is required")) }
    else if T.length normalized > maxLength || T.any isDisallowedDescriptionControl normalized
      then throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " is invalid")) }
      else pure normalized
  where
    isDisallowedDescriptionControl ch =
      (isControl ch && ch `notElem` ['\n', '\r', '\t']) || isHiddenFormattingChar ch

validateOptionalInternalText
  :: MonadError ServerError m
  => Text
  -> Int
  -> Maybe Text
  -> m (Maybe Text)
validateOptionalInternalText _ _ Nothing = pure Nothing
validateOptionalInternalText fieldName maxLength (Just raw)
  | T.null (T.strip raw) = pure Nothing
  | otherwise = Just <$> validateInternalText fieldName maxLength raw

validateNestedInternalText
  :: MonadError ServerError m
  => Text
  -> Int
  -> Maybe (Maybe Text)
  -> m (Maybe (Maybe Text))
validateNestedInternalText _ _ Nothing = pure Nothing
validateNestedInternalText _ _ (Just Nothing) = pure (Just Nothing)
validateNestedInternalText fieldName maxLength (Just (Just raw)) =
  Just <$> validateOptionalInternalText fieldName maxLength (Just raw)

normalizeInternalOptional :: Int -> Maybe Text -> Maybe Text
normalizeInternalOptional maxLength = (>>= \raw ->
  let normalized = T.take maxLength (T.strip raw)
  in if T.null normalized then Nothing else Just normalized)

hasMeaningful :: Maybe Text -> Bool
hasMeaningful = maybe False (not . T.null . T.strip)

validatePositiveParty :: MonadError ServerError m => Text -> Int64 -> m Int64
validatePositiveParty fieldName value
  | value > 0 = pure value
  | otherwise = throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be positive")) }

validateExternalEvidenceUrl :: MonadError ServerError m => Text -> m Text
validateExternalEvidenceUrl raw =
  let url = T.strip raw
      lowered = T.toLower url
      authority = T.takeWhile (`notElem` ("/?#" :: String)) (T.drop 8 lowered)
      host = fst (T.breakOn ":" authority)
      private172 = case T.splitOn "." host of
        ["172", second, _, _] -> case reads (T.unpack second) of
          [(octet :: Int, "")] -> octet >= 16 && octet <= 31
          _ -> False
        _ -> False
      blockedHost = host == "localhost"
        || ".localhost" `T.isSuffixOf` host
        || host == "0.0.0.0"
        || "127." `T.isPrefixOf` host
        || "10." `T.isPrefixOf` host
        || "192.168." `T.isPrefixOf` host
        || "169.254." `T.isPrefixOf` host
        || private172
        || "[" `T.isPrefixOf` host
  in if T.length url <= 2048 && "https://" `T.isPrefixOf` lowered
      && not (T.null authority)
      && not (T.any (`elem` [' ', '\t', '\n', '\r', '@']) authority)
      && T.isInfixOf "." host
      && not blockedHost
    then pure url
    else throwError err400 { errBody = "Evidence links must be safe public HTTPS URLs" }

validateVideoLinks :: MonadError ServerError m => Maybe Text -> m (Maybe Text)
validateVideoLinks Nothing = pure Nothing
validateVideoLinks (Just raw) = do
  when (T.length raw > 10000) $
    throwError err400 { errBody = "Video links must be 10000 characters or fewer" }
  let links = filter (not . T.null) (map T.strip (T.lines raw))
  validated <- traverse validateExternalEvidenceUrl links
  pure $ if null validated then Nothing else Just (T.intercalate "\n" validated)

validateGithubIssueUrl :: MonadError ServerError m => Text -> m Text
validateGithubIssueUrl raw = do
  url <- validateExternalEvidenceUrl raw
  let prefix = "https://github.com/"
      path = fromMaybe "" (T.stripPrefix prefix url)
      parts = T.splitOn "/" path
      valid = case parts of
        [owner, repository, "issues", issueToken] ->
          not (T.null owner) && not (T.null repository)
            && not (T.null issueToken) && T.all isDigit issueToken
        _ -> False
  if valid
    then pure url
    else throwError err400 { errBody = "githubIssueUrl must be an HTTPS GitHub issue URL" }

resolvePublishedFeedbackCategoryFor
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => Text
  -> m Catalog.FeedbackCategoryId
resolvePublishedFeedbackCategoryFor rawId = do
  pool <- asks envPool
  categoryKey <- maybe
    (throwError err400 { errBody = "categoryId must be a valid catalog UUID" })
    pure
    (fromPathPiece (T.strip rawId))
  valid <- liftIO $ runSqlPool (publishedCategoryExists categoryKey) pool
  unless valid $ throwError err400 { errBody = "categoryId must reference an active published feedback category" }
  pure categoryKey

validateInternalReportCategoryType
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => Catalog.FeedbackCategoryId
  -> Text
  -> m ()
validateInternalReportCategoryType categoryKey reportType = do
  category <- withPool (get categoryKey) >>= maybe
    (throwError err400 { errBody = "categoryId must identify a feedback category" })
    pure
  case internalReportTypeForCategoryCode (Catalog.feedbackCategoryCode category) of
    Just expectedType | expectedType == reportType -> pure ()
    _ -> throwError err400
      { errBody = "reportType must match an internal-report feedback category" }

resolvePublishedFeedbackSeverityFor
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => Text
  -> m Catalog.FeedbackSeverityId
resolvePublishedFeedbackSeverityFor rawId = do
  pool <- asks envPool
  severityKey <- maybe
    (throwError err400 { errBody = "severityId must be a valid catalog UUID" })
    pure
    (fromPathPiece (T.strip rawId))
  valid <- liftIO $ runSqlPool (publishedSeverityExists severityKey) pool
  unless valid $ throwError err400 { errBody = "severityId must reference an active published feedback severity" }
  pure severityKey

publishedCategoryExists :: Catalog.FeedbackCategoryId -> SqlPersistT IO Bool
publishedCategoryExists categoryKey = do
  item <- get categoryKey
  case item of
    Nothing -> pure False
    Just category -> do
      state <- get (Catalog.feedbackCategoryWorkflowStateId category)
      catalog <- get (Catalog.feedbackCategoryCatalogId category)
      pure $ Catalog.feedbackCategoryActive category
        && Catalog.feedbackCategoryDeprecatedAt category == Nothing
        && maybe False ((== "published") . Catalog.workflowStateCode) state
        && maybe False (\definition -> Catalog.catalogDefinitionActive definition
          && Catalog.catalogDefinitionCode definition == "feedback-categories") catalog

publishedSeverityExists :: Catalog.FeedbackSeverityId -> SqlPersistT IO Bool
publishedSeverityExists severityKey = do
  item <- get severityKey
  case item of
    Nothing -> pure False
    Just severity -> do
      state <- get (Catalog.feedbackSeverityWorkflowStateId severity)
      catalog <- get (Catalog.feedbackSeverityCatalogId severity)
      pure $ Catalog.feedbackSeverityActive severity
        && Catalog.feedbackSeverityDeprecatedAt severity == Nothing
        && maybe False ((== "published") . Catalog.workflowStateCode) state
        && maybe False (\definition -> Catalog.catalogDefinitionActive definition
          && Catalog.catalogDefinitionCode definition == "feedback-severities") catalog

parseInternalKey
  :: forall record m.
     (MonadError ServerError m, PathPiece (Key record))
  => Text
  -> m (Key record)
parseInternalKey raw =
  let normalized = T.strip raw
  in case fromPathPiece normalized of
    Just key | toPathPiece key == normalized -> pure key
    _ -> throwError err400 { errBody = "Invalid identifier" }

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

encodeMetadata :: Aeson.ToJSON a => a -> Text
encodeMetadata = TE.decodeUtf8 . BL.toStrict . Aeson.encode

changedFieldMetadata :: InternalFeedbackUpdate -> Text
changedFieldMetadata InternalFeedbackUpdate{..} = encodeMetadata $ object
  [ "fields" .= catMaybes
      [ present "title" ifuTitle
      , present "description" ifuDescription
      , present "categoryId" ifuCategoryId
      , present "proposedSeverityId" ifuProposedSeverityId
      , present "reportType" ifuReportType
      , present "moduleName" ifuModuleName
      , present "featureName" ifuFeatureName
      , present "environment" ifuEnvironment
      , present "urlOrScreen" ifuUrlOrScreen
      , present "platform" ifuPlatform
      , present "device" ifuDevice
      , present "browser" ifuBrowser
      , present "language" ifuLanguage
      , present "accountRole" ifuAccountRole
      , present "reproductionSteps" ifuReproductionSteps
      , present "expectedResult" ifuExpectedResult
      , present "actualResult" ifuActualResult
      , present "frequency" ifuFrequency
      , present "blocking" ifuBlocking
      , present "videoLinks" ifuVideoLinks
      , present "state" ifuState
      , present "authoritativeSeverityId" ifuAuthoritativeSeverityId
      , present "priority" ifuPriority
      , present "assignedTo" ifuAssignedTo
      , present "duplicateOf" ifuDuplicateOf
      , present "resolution" ifuResolution
      , present "retestResult" ifuRetestResult
      , present "closureReason" ifuClosureReason
      , present "githubIssueUrl" ifuGithubIssueUrl
      ]
  ]
  where
    present :: Text -> Maybe a -> Maybe Text
    present name value = if isJust value then Just name else Nothing

normalizeWords :: Text -> Text
normalizeWords = T.unwords . T.words . T.toCaseFold . T.map normalizeChar
  where
    normalizeChar ch | isAlphaNum ch = ch
                     | otherwise = ' '

normalizedEqual :: Text -> Text -> Bool
normalizedEqual left right = normalizeWords left == normalizeWords right

jaccardPercent :: Text -> Text -> Int
jaccardPercent left right =
  let leftSet = Set.fromList (T.words (normalizeWords left))
      rightSet = Set.fromList (T.words (normalizeWords right))
      unionSize = Set.size (Set.union leftSet rightSet)
      intersectionSize = Set.size (Set.intersection leftSet rightSet)
  in if unionSize == 0 then 0 else (intersectionSize * 100) `div` unionSize

csvField :: Text -> Text
csvField value = "\"" <> T.replace "\"" "\"\"" safeValue <> "\""
  where
    firstVisible = T.uncons (T.dropWhile (`elem` [' ', '\t', '\r', '\n']) value)
    safeValue = case firstVisible of
      Just (first, _) | first `elem` ['=', '+', '-', '@'] -> "'" <> value
      _ -> value

pathSeparator :: Char
pathSeparator = '/'

isPrefixText :: String -> String -> Bool
isPrefixText prefix value = prefix == take (length prefix) value

normalizeOptionalFeedbackText :: Maybe Text -> Maybe Text
normalizeOptionalFeedbackText mVal =
  case fmap T.strip mVal of
    Just txt | T.null txt -> Nothing
    other                 -> other

validateOptionalFeedbackContactEmail :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalFeedbackContactEmail Nothing = Right Nothing
validateOptionalFeedbackContactEmail (Just rawEmail) =
  case normalizeOptionalFeedbackText (Just rawEmail) of
    Nothing -> Right Nothing
    Just emailVal ->
      let normalized = T.toLower emailVal
      in if T.length normalized > maxFeedbackContactEmailChars
           then Left err400 { errBody = "contactEmail must be 254 characters or fewer" }
           else
             if isValidFeedbackEmail normalized
               then Right (Just normalized)
               else Left err400 { errBody = "contactEmail must be a valid email address" }

maxFeedbackContactEmailChars :: Int
maxFeedbackContactEmailChars = 254

maxFeedbackTitleChars :: Int
maxFeedbackTitleChars = 160

maxFeedbackDescriptionChars :: Int
maxFeedbackDescriptionChars = 5000

validateFeedbackTitle :: Text -> Either ServerError Text
validateFeedbackTitle rawTitle
  | T.null title =
      Left err400 { errBody = "title is required" }
  | T.length title > maxFeedbackTitleChars =
      Left err400 { errBody = "title must be 160 characters or fewer" }
  | T.any isUnsafeFeedbackSingleLineChar title =
      Left err400
        { errBody =
            "title must not contain control characters or hidden formatting characters"
        }
  | not (T.any isAlphaNum title) =
      Left err400 { errBody = "title must include letters or numbers" }
  | otherwise =
      Right title
  where
    title = T.strip rawTitle

validateFeedbackDescription :: Text -> Either ServerError Text
validateFeedbackDescription rawDescription
  | T.null description =
      Left err400 { errBody = "description is required" }
  | T.length description > maxFeedbackDescriptionChars =
      Left err400 { errBody = "description must be 5000 characters or fewer" }
  | T.any isDisallowedDescriptionControl description =
      Left err400
        { errBody =
            "description must not contain control characters or hidden formatting characters"
        }
  | otherwise =
      Right description
  where
    description = T.strip rawDescription
    isDisallowedDescriptionControl ch =
      (isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t')
        || isHiddenFormattingChar ch

isUnsafeFeedbackSingleLineChar :: Char -> Bool
isUnsafeFeedbackSingleLineChar ch =
  isControl ch || isHiddenFormattingChar ch

isHiddenFormattingChar :: Char -> Bool
isHiddenFormattingChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateFeedbackConsent :: Bool -> Either ServerError ()
validateFeedbackConsent True = Right ()
validateFeedbackConsent False =
  Left err400 { errBody = "consent must be accepted before submitting feedback" }

maxFeedbackAttachmentBytes :: Integer
maxFeedbackAttachmentBytes = 10 * 1024 * 1024

validateFeedbackAttachmentSize :: Integer -> Either ServerError ()
validateFeedbackAttachmentSize size
  | size < 0 =
      Left err400 { errBody = "attachment size is invalid" }
  | size == 0 =
      Left err400 { errBody = "attachment must not be empty" }
  | size > maxFeedbackAttachmentBytes =
      Left err400 { errBody = "attachment must be 10 MB or smaller" }
  | otherwise =
      Right ()

validateFeedbackAttachmentContentType :: Text -> Either ServerError Text
validateFeedbackAttachmentContentType rawContentType
  | T.null cleaned =
      Left err400 { errBody = "attachment content type is required" }
  | T.length cleaned > maxFeedbackAttachmentContentTypeChars =
      Left err400 { errBody = "attachment content type must be 100 characters or fewer" }
  | T.any isUnsafeAttachmentContentTypeChar cleaned =
      Left err400
        { errBody =
            "attachment content type must not contain control characters or hidden formatting characters"
        }
  | hasMalformedAttachmentContentTypeParameter cleaned =
      Left err400
        { errBody =
            "attachment content type parameters must be key=value tokens"
        }
  | hasAttachmentContentTypeNameParameter cleaned =
      Left err400
        { errBody =
            "attachment content type must not include filename parameters"
        }
  | mediaType `elem` allowedFeedbackAttachmentContentTypes =
      Right mediaType
  | otherwise =
      Left err400
        { errBody =
            "attachment content type must be a PDF, image, plain text, or CSV file"
        }
  where
    cleaned = T.strip rawContentType
    mediaType = T.toLower (T.strip (fst (T.breakOn ";" cleaned)))

maxFeedbackAttachmentContentTypeChars :: Int
maxFeedbackAttachmentContentTypeChars = 100

hasMalformedAttachmentContentTypeParameter :: Text -> Bool
hasMalformedAttachmentContentTypeParameter rawContentType =
  any (not . isValidContentTypeParameter) (drop 1 (T.splitOn ";" rawContentType))

isValidContentTypeParameter :: Text -> Bool
isValidContentTypeParameter rawParameter =
  let (rawKey, rawValueWithEquals) = T.breakOn "=" rawParameter
      key = T.toLower (T.strip rawKey)
      value = T.strip (T.drop 1 rawValueWithEquals)
  in not (T.null key)
       && "=" `T.isPrefixOf` rawValueWithEquals
       && T.all isContentTypeParameterKeyChar key
       && not (T.null value)

isContentTypeParameterKeyChar :: Char -> Bool
isContentTypeParameterKeyChar ch =
  isAsciiLower ch || isDigit ch || ch == '-' || ch == '*'

hasAttachmentContentTypeNameParameter :: Text -> Bool
hasAttachmentContentTypeNameParameter rawContentType =
  any isNameParameter (drop 1 (T.splitOn ";" rawContentType))
  where
    isNameParameter rawParameter =
      let key = T.toLower (T.strip (fst (T.breakOn "=" rawParameter)))
      in key `elem` ["name", "name*", "filename", "filename*"]
           || "name*" `T.isPrefixOf` key
           || "filename*" `T.isPrefixOf` key

allowedFeedbackAttachmentContentTypes :: [Text]
allowedFeedbackAttachmentContentTypes =
  [ "application/csv"
  , "application/pdf"
  , "image/gif"
  , "image/jpeg"
  , "image/png"
  , "image/webp"
  , "text/csv"
  , "text/plain"
  ]

isUnsafeAttachmentContentTypeChar :: Char -> Bool
isUnsafeAttachmentContentTypeChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateFeedbackAttachmentMetadata :: Text -> Text -> Either ServerError (Text, Text)
validateFeedbackAttachmentMetadata rawName rawContentType = do
  safeName <- validateFeedbackAttachmentFileName rawName
  mediaType <- validateFeedbackAttachmentContentType rawContentType
  validateFeedbackAttachmentExtension mediaType safeName
  Right (safeName, mediaType)

validateFeedbackAttachmentExtension :: Text -> Text -> Either ServerError ()
validateFeedbackAttachmentExtension mediaType safeName =
  let extension = T.toLower (T.pack (takeExtension (T.unpack safeName)))
  in if extension `elem` allowedFeedbackAttachmentExtensions mediaType
       then Right ()
       else
         Left err400
           { errBody = "attachment file name extension must match its content type" }

allowedFeedbackAttachmentExtensions :: Text -> [Text]
allowedFeedbackAttachmentExtensions mediaType =
  case mediaType of
    "application/csv" -> [".csv"]
    "application/pdf" -> [".pdf"]
    "image/gif" -> [".gif"]
    "image/jpeg" -> [".jpg", ".jpeg"]
    "image/png" -> [".png"]
    "image/webp" -> [".webp"]
    "text/csv" -> [".csv"]
    "text/plain" -> [".txt", ".text", ".log", ".md"]
    _ -> []

validateFeedbackAttachmentFileName :: Text -> Either ServerError Text
validateFeedbackAttachmentFileName rawName
  | T.null trimmed =
      Left err400 { errBody = "attachment file name is required" }
  | T.any isUnsafeAttachmentFileNameChar trimmed =
      Left err400
        { errBody =
            "attachment file name must not contain control characters or hidden formatting characters"
        }
  | T.any isPathSeparator trimmed =
      Left err400 { errBody = "attachment file name must not contain path separators" }
  | T.length trimmed > maxFeedbackAttachmentFileNameChars =
      Left err400
        { errBody =
            BL.fromStrict
              ( TE.encodeUtf8
                  ( "attachment file name must be "
                      <> T.pack (show maxFeedbackAttachmentFileNameChars)
                      <> " characters or fewer"
                  )
              )
        }
  | sanitized == "attachment" && trimmed /= "attachment" =
      Left err400 { errBody = "attachment file name must include a usable name" }
  | hasDisallowedFeedbackAttachmentExtension sanitized =
      Left err400 { errBody = "attachment file name extension is not allowed" }
  | otherwise =
      Right sanitized
  where
    trimmed = T.strip rawName
    sanitized = sanitizeFeedbackAttachmentFileName trimmed

isUnsafeAttachmentFileNameChar :: Char -> Bool
isUnsafeAttachmentFileNameChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

isPathSeparator :: Char -> Bool
isPathSeparator ch = ch == '/' || ch == '\\'

hasDisallowedFeedbackAttachmentExtension :: Text -> Bool
hasDisallowedFeedbackAttachmentExtension name =
  any (`elem` extensionChain) disallowedFeedbackAttachmentExtensions
  where
    loweredName = T.toLower name
    extensionChain = map ("." <>) (drop 1 (T.splitOn "." loweredName))

disallowedFeedbackAttachmentExtensions :: [Text]
disallowedFeedbackAttachmentExtensions =
  [ ".bat"
  , ".cmd"
  , ".com"
  , ".exe"
  , ".htm"
  , ".html"
  , ".jar"
  , ".js"
  , ".mjs"
  , ".php"
  , ".ps1"
  , ".scr"
  , ".sh"
  , ".svg"
  , ".svgz"
  , ".xhtml"
  ]

isValidFeedbackEmail :: Text -> Bool
isValidFeedbackEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      isValidFeedbackEmailLocalPart localPart
        && not (T.null domain)
        && not (T.any (`elem` [' ', '\t', '\n', '\r']) candidate)
        && T.isInfixOf "." domain
        && all isValidDomainLabel (T.splitOn "." domain)
        && isValidFeedbackFinalDomainLabel domain
    _ -> False

isValidFeedbackEmailLocalPart :: Text -> Bool
isValidFeedbackEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= maxFeedbackEmailLocalPartChars
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (".." `T.isInfixOf` localPart)
    && T.all isValidFeedbackEmailLocalChar localPart

isValidFeedbackEmailLocalChar :: Char -> Bool
isValidFeedbackEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidDomainLabel :: Text -> Bool
isValidDomainLabel label =
  not (T.null label)
    && T.length label <= maxFeedbackEmailDomainLabelChars
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidDomainChar label

isValidFeedbackFinalDomainLabel :: Text -> Bool
isValidFeedbackFinalDomainLabel domain =
  case reverse (T.splitOn "." domain) of
    finalLabel : _ ->
      T.length finalLabel >= 2 && T.any isAsciiLower finalLabel
    [] -> False

isValidDomainChar :: Char -> Bool
isValidDomainChar c = isAsciiLower c || isDigit c || c == '-'

maxFeedbackEmailLocalPartChars :: Int
maxFeedbackEmailLocalPartChars = 64

maxFeedbackEmailDomainLabelChars :: Int
maxFeedbackEmailDomainLabelChars = 63

sanitizeFeedbackAttachmentFileName :: Text -> Text
sanitizeFeedbackAttachmentFileName rawName =
  let trimmed = T.strip rawName
      baseName = T.pack (takeFileName (T.unpack trimmed))
      cleaned = T.map normalizeAttachmentChar baseName
      stripped = T.dropWhile (== '-') (T.dropWhileEnd (== '-') cleaned)
  in
    if T.null stripped
        || stripped == "."
        || stripped == ".."
        || not (T.any isAlphaNum stripped)
      then "attachment"
      else truncateAttachmentFileName stripped
  where
    normalizeAttachmentChar ch
      | isAscii ch && isAlphaNum ch = ch
      | ch == '.' || ch == '-' || ch == '_' = ch
      | ch == ' ' = '-'
      | otherwise = '-'

maxFeedbackAttachmentFileNameChars :: Int
maxFeedbackAttachmentFileNameChars = 120

truncateAttachmentFileName :: Text -> Text
truncateAttachmentFileName name
  | T.length name <= maxFeedbackAttachmentFileNameChars = name
  | T.length extension > 20 || T.null stem =
      T.take maxFeedbackAttachmentFileNameChars name
  | T.length extension >= maxFeedbackAttachmentFileNameChars =
      T.take maxFeedbackAttachmentFileNameChars name
  | otherwise =
      T.take stemLimit stem <> extension
  where
    (stemWithDot, ext) = T.breakOnEnd "." name
    (stem, extension) =
      if T.null stemWithDot || T.null ext
        then (name, "")
        else (T.dropEnd 1 stemWithDot, "." <> ext)
    stemLimit = maxFeedbackAttachmentFileNameChars - T.length extension

internalFeedbackTestRuntime :: IO Bool
internalFeedbackTestRuntime = do
  appEnvironment <- fmap (T.toLower . T.strip . T.pack) <$> lookupEnv "APP_ENV"
  pure (appEnvironment `notElem` [Just "production", Just "prod"])

internalFeedbackUploadRoot :: IO FilePath
internalFeedbackUploadRoot = do
  configured <- lookupEnv "TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT"
  pure $ normalise $ case configured of
    Just value | not (null value) -> value
    _ -> "uploads" </> "feedback" </> "internal"

notify :: EmailSvc.EmailService -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe FilePath -> IO ()
notify emailSvc title body mCat mSev mContact attachmentPath = do
  let subject = "[TDF Feedback] " <> title
      catLine = maybe "" (\c -> "Categoría: " <> c) mCat
      sevLine = maybe "" (\s -> "Severidad: " <> s) mSev
      contactLine = maybe "Contacto: (no especificado)" (\c -> "Contacto: " <> c) (normalizeOptionalFeedbackText mContact)
      attachmentLine = maybe "Adjunto: (ninguno)" (\p -> "Adjunto: " <> T.pack p) attachmentPath
      bodyLines =
        filter (not . T.null)
          [ catLine
          , sevLine
          , contactLine
          , attachmentLine
          , ""
          , "Descripción:"
          , body
          ]
      recipients =
        [ ("Diego Saa", "diego@tdfrecords.net")
        , ("Equipo TDF", "info@tdfrecords.net")
        , ("TDF Estudio", "tdfestudiodegrabacion@gmail.com")
        ]
  forM_ recipients $ \(name, email) -> do
    sendResult <- try $
      EmailSvc.sendTestEmail emailSvc name email subject bodyLines Nothing
    case sendResult of
      Left (err :: SomeException) ->
        hPutStrLn stderr ("[Feedback] Failed to email " <> T.unpack email <> ": " <> displayException err)
      Right () -> pure ()
  pure ()
