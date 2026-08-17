{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.EventResearch
    ( eventResearchServer
    , validateEventResearchCandidate
    , eventResearchCandidateContentHash
    ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii, isControl)
import Data.Int (Int64)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import Servant
import Text.Read (readMaybe)

import TDF.API.EventResearchAPI
import TDF.Auth (AuthedUser (..), hasStrictAdminAccess)
import TDF.DB (Env (..))
import TDF.DTO.EventResearchDTO
import TDF.Models.SocialEventsModels hiding (eventResearchCandidateContentHash)
import qualified TDF.Models.SocialEventsModels as SM
import qualified TDF.Trials.Server as TrialsServer

type AppM = ReaderT Env Handler

eventResearchServer :: AuthedUser -> ServerT EventResearchRoutes AppM
eventResearchServer user =
    getPilot
        :<|> approvePilot
        :<|> listRuns
        :<|> createRun
        :<|> updateRun
        :<|> listCandidates
        :<|> upsertCandidate
        :<|> listChanges
  where
    requireAdmin =
        unless (hasStrictAdminAccess user) $
            throwError err403{errBody = "Strict admin access required"}

    partyIdText = renderKey (auPartyId user)

    getPilot = do
        requireAdmin
        Env{..} <- ask
        result <- liftIO $ runSqlPool loadPilotDTO envPool
        either throwError pure result

    approvePilot EventResearchPilotApprovalDTO{..} = do
        requireAdmin
        approvalReference <-
            either (throwError . badRequest) pure $
                normalizeRequired "approval reference" 500 erPilotApprovalReference
        Env{..} <- ask
        now <- liftIO getCurrentTime
        result <- liftIO $ runSqlPool (approvePilotDb partyIdText approvalReference now) envPool
        either throwError pure result

    listRuns mLimit = do
        requireAdmin
        Env{..} <- ask
        limit <- either throwError pure (boundedLimit mLimit)
        rows <- liftIO $ runSqlPool (selectList [] [Desc EventResearchRunStartedAt, LimitTo limit]) envPool
        traverse (either (throwError . storedDataError) pure . runEntityToDTO) rows

    createRun EventResearchRunCreateDTO{..} = do
        requireAdmin
        runKey <- either (throwError . badRequest) pure (normalizeRunKey erRunKey)
        checkpoint <- traverse (either (throwError . badRequest) pure . normalizeRequired "checkpoint" 2000) erRunCheckpoint
        Env{..} <- ask
        now <- liftIO getCurrentTime
        row <- liftIO $ runSqlPool (createRunDb partyIdText runKey erRunReconciliation checkpoint now) envPool
        either (throwError . storedDataError) pure (runEntityToDTO row)

    updateRun rawRunId EventResearchRunUpdateDTO{..} = do
        requireAdmin
        runId <- either (throwError . badRequest) pure (parseKey "run" rawRunId)
        status <- either (throwError . badRequest) pure (normalizeRunStatus erRunStatus)
        checkpoint <- traverse (either (throwError . badRequest) pure . normalizeRequired "checkpoint" 2000) erRunCheckpoint
        errorSummary <- traverse (either (throwError . badRequest) pure . normalizeRequired "error summary" 4000) erRunErrorSummary
        Env{..} <- ask
        now <- liftIO getCurrentTime
        result <- liftIO $ runSqlPool (updateRunDb runId status checkpoint erRunCounters errorSummary now) envPool
        either throwError (either (throwError . storedDataError) pure . runEntityToDTO) result

    listCandidates mProvider mState mLimit = do
        requireAdmin
        provider <- traverse (either (throwError . badRequest) pure . normalizeProvider) mProvider
        state <- traverse (either (throwError . badRequest) pure . normalizeReviewState) mState
        limit <- either throwError pure (boundedLimit mLimit)
        Env{..} <- ask
        let filters =
                maybe [] (\value -> [EventResearchCandidateProvider ==. value]) provider
                    <> maybe [] (\value -> [EventResearchCandidateReviewState ==. value]) state
        rows <- liftIO $ runSqlPool (selectList filters [Desc EventResearchCandidateVerifiedAt, LimitTo limit]) envPool
        traverse (either (throwError . storedDataError) pure . candidateEntityToDTO) rows

    upsertCandidate rawCandidate = do
        requireAdmin
        candidate <- either (throwError . badRequest) pure (validateEventResearchCandidate rawCandidate)
        Env{..} <- ask
        now <- liftIO getCurrentTime
        attempted <- liftIO (try (runSqlPool (upsertCandidateDb candidate now) envPool) :: IO (Either SomeException (Either ServerError (Entity EventResearchCandidate))))
        row <- case attempted of
            Left exc
                | "event research pilot candidate limit reached" `T.isInfixOf` T.toLower (T.pack (displayException exc)) ->
                    throwError err409{errBody = "The unapproved pilot already contains its maximum of 20 active candidates"}
                | otherwise -> throwError err500{errBody = "Event research candidate transaction failed"}
            Right result -> either throwError pure result
        either (throwError . storedDataError) pure (candidateEntityToDTO row)

    listChanges mRunId mLimit = do
        requireAdmin
        runId <- traverse (either (throwError . badRequest) pure . parseKey "run") mRunId
        limit <- either throwError pure (boundedLimit mLimit)
        Env{..} <- ask
        let filters = maybe [] (\value -> [EventResearchChangeRunId ==. value]) runId
        rows <- liftIO $ runSqlPool (selectList filters [Desc EventResearchChangeCreatedAt, LimitTo limit]) envPool
        traverse (either (throwError . storedDataError) pure . changeEntityToDTO) rows

loadPilotDTO :: SqlPersistT IO (Either ServerError EventResearchPilotDTO)
loadPilotDTO = do
    control <- getBy (UniqueEventResearchPilotControl "default")
    case control of
        Nothing -> pure (Left err500{errBody = "Event research pilot control is not initialized"})
        Just (Entity _ row) -> do
            activeCount <- count [EventResearchCandidateIsPilot ==. True, EventResearchCandidateReviewState !=. "discarded"]
            pure . Right $
                EventResearchPilotDTO
                    { erPilotApproved = eventResearchPilotControlApproved row
                    , erPilotApprovedAt = eventResearchPilotControlApprovedAt row
                    , erPilotApprovedByPartyId = eventResearchPilotControlApprovedByPartyId row
                    , erPilotApprovalReference = eventResearchPilotControlApprovalReference row
                    , erPilotMaxActiveCandidates = eventResearchPilotControlMaxActiveCandidates row
                    , erPilotActiveCandidates = activeCount
                    , erPilotUpdatedAt = eventResearchPilotControlUpdatedAt row
                    }

approvePilotDb :: T.Text -> T.Text -> UTCTime -> SqlPersistT IO (Either ServerError EventResearchPilotDTO)
approvePilotDb partyId approvalReference now = do
    control <- getBy (UniqueEventResearchPilotControl "default")
    case control of
        Nothing -> pure (Left err500{errBody = "Event research pilot control is not initialized"})
        Just (Entity controlId row)
            | eventResearchPilotControlApproved row
                && eventResearchPilotControlApprovalReference row /= Just approvalReference ->
                    pure (Left err409{errBody = "The pilot was already approved with a different reference"})
            | eventResearchPilotControlApproved row -> loadPilotDTO
            | otherwise -> do
                update
                    controlId
                    [ EventResearchPilotControlApproved =. True
                    , EventResearchPilotControlApprovedAt =. Just (maybe now id (eventResearchPilotControlApprovedAt row))
                    , EventResearchPilotControlApprovedByPartyId =. Just (maybe partyId id (eventResearchPilotControlApprovedByPartyId row))
                    , EventResearchPilotControlApprovalReference =. Just approvalReference
                    , EventResearchPilotControlUpdatedAt =. now
                    ]
                when (not (eventResearchPilotControlApproved row)) $
                    insert_ (EventResearchPilotAudit controlId True partyId approvalReference now)
                loadPilotDTO

createRunDb :: T.Text -> T.Text -> Bool -> Maybe T.Text -> UTCTime -> SqlPersistT IO (Entity EventResearchRun)
createRunDb partyId runKey reconciliation checkpoint now = do
    existing <- getBy (UniqueEventResearchRun runKey)
    case existing of
        Just row -> pure row
        Nothing -> do
            let row = EventResearchRun runKey "running" reconciliation checkpoint "{}" Nothing now now Nothing partyId
            key <- insert row
            pure (Entity key row)

updateRunDb :: EventResearchRunId -> T.Text -> Maybe T.Text -> Aeson.Value -> Maybe T.Text -> UTCTime -> SqlPersistT IO (Either ServerError (Entity EventResearchRun))
updateRunDb runId status checkpoint counters errorSummary now = do
    existing <- get runId
    case existing of
        Nothing -> pure (Left err404{errBody = "Event research run not found"})
        Just row
            | eventResearchRunStatus row == status
                && eventResearchRunCheckpoint row == checkpoint
                && eventResearchRunCounters row == encodeJson counters
                && eventResearchRunErrorSummary row == errorSummary ->
                    pure (Right (Entity runId row))
            | eventResearchRunStatus row == "completed" && status /= "completed" ->
                pure (Left err409{errBody = "A completed research run cannot be reopened"})
            | otherwise -> do
                let finishedAt =
                        if status `elem` ["completed", "failed"]
                            then Just (maybe now id (eventResearchRunFinishedAt row))
                            else Nothing
                update
                    runId
                    [ EventResearchRunStatus =. status
                    , EventResearchRunCheckpoint =. checkpoint
                    , EventResearchRunCounters =. encodeJson counters
                    , EventResearchRunErrorSummary =. errorSummary
                    , EventResearchRunUpdatedAt =. now
                    , EventResearchRunFinishedAt =. finishedAt
                    ]
                updated <- getJust runId
                pure (Right (Entity runId updated))

upsertCandidateDb :: EventResearchCandidateWriteDTO -> UTCTime -> SqlPersistT IO (Either ServerError (Entity EventResearchCandidate))
upsertCandidateDb candidate now = do
    runId <- pure (parseKeyUnsafe (candidate.erCandidateRunId))
    run <- get runId
    case run of
        Nothing -> pure (Left err404{errBody = "Event research run not found"})
        Just runRow -> do
            sourceId <- traverse (pure . parseKeyUnsafe) (candidate.erCandidateSourceId)
            sourceExists <- maybe (pure True) (fmap isJust . get) sourceId
            if not sourceExists
                then pure (Left err400{errBody = "Configured event source does not exist"})
                else do
                    let provider = candidate.erCandidateProvider
                        externalId = candidate.erCandidateExternalId
                        contentHash = eventResearchCandidateContentHash candidate
                    existing <- getBy (UniqueEventResearchCandidate provider externalId)
                    case existing of
                        Just entity@(Entity candidateId row)
                            | eventResearchRunStatus runRow /= "running"
                                && SM.eventResearchCandidateContentHash row /= contentHash ->
                                    pure (Left err409{errBody = "Only idempotent candidate retries are accepted after a run is closed"})
                            | SM.eventResearchCandidateContentHash row == contentHash
                                && ( eventResearchRunStatus runRow /= "running"
                                        || candidate.erCandidateVerifiedAt <= eventResearchCandidateVerifiedAt row
                                   ) ->
                                    pure (Right entity)
                            | otherwise -> do
                                let action = if SM.eventResearchCandidateContentHash row == contentHash then "verified" else "updated"
                                    beforeValue = encodeJson <$> either (const Nothing) Just (candidateEntityToDTO entity)
                                updateCandidate candidate runId sourceId contentHash now candidateId
                                updated <- getJust candidateId
                                let updatedEntity = Entity candidateId updated
                                    afterValue = encodeJson <$> either (const Nothing) Just (candidateEntityToDTO updatedEntity)
                                insertChange candidate runId (Just candidateId) (eventResearchCandidateEventId updated) action beforeValue afterValue contentHash now
                                pure (Right updatedEntity)
                        Nothing
                            | eventResearchRunStatus runRow /= "running" ->
                                pure (Left err409{errBody = "New candidates require a running research run"})
                            | otherwise -> do
                                pilot <- getBy (UniqueEventResearchPilotControl "default")
                                case pilot of
                                    Nothing -> pure (Left err500{errBody = "Event research pilot control is not initialized"})
                                    Just (Entity _ pilotRow) -> do
                                        let row = candidateRow candidate runId sourceId contentHash (not (eventResearchPilotControlApproved pilotRow)) now
                                        candidateId <- insert row
                                        let entity = Entity candidateId row
                                            afterValue = encodeJson <$> either (const Nothing) Just (candidateEntityToDTO entity)
                                        insertChange candidate runId (Just candidateId) Nothing "created" Nothing afterValue contentHash now
                                        pure (Right entity)

candidateRow :: EventResearchCandidateWriteDTO -> EventResearchRunId -> Maybe EventDiscoverySourceId -> T.Text -> Bool -> UTCTime -> EventResearchCandidate
candidateRow EventResearchCandidateWriteDTO{..} runId sourceId contentHash isPilot now =
    EventResearchCandidate
        erCandidateProvider erCandidateExternalId runId sourceId Nothing erCandidateReviewState
        erCandidateTitle erCandidateStartTime erCandidateEndTime erCandidateTimezone erCandidateVenueName
        erCandidateCity erCandidateProvince erCandidateCountryCode erCandidateSourceUrl erCandidateInfoUrl
        erCandidatePurchaseUrl (encodeJson erCandidatePayload) (encodeJson erCandidateEvidence)
        erCandidateConfidence (encodeJson erCandidateManagedFields) contentHash erCandidateVerifiedAt isPilot now now

updateCandidate :: EventResearchCandidateWriteDTO -> EventResearchRunId -> Maybe EventDiscoverySourceId -> T.Text -> UTCTime -> EventResearchCandidateId -> SqlPersistT IO ()
updateCandidate EventResearchCandidateWriteDTO{..} runId sourceId contentHash now candidateId =
    update
        candidateId
        [ EventResearchCandidateRunId =. runId
        , EventResearchCandidateSourceId =. sourceId
        , EventResearchCandidateReviewState =. erCandidateReviewState
        , EventResearchCandidateTitle =. erCandidateTitle
        , EventResearchCandidateStartTime =. erCandidateStartTime
        , EventResearchCandidateEndTime =. erCandidateEndTime
        , EventResearchCandidateTimezone =. erCandidateTimezone
        , EventResearchCandidateVenueName =. erCandidateVenueName
        , EventResearchCandidateCity =. erCandidateCity
        , EventResearchCandidateProvince =. erCandidateProvince
        , EventResearchCandidateCountryCode =. erCandidateCountryCode
        , EventResearchCandidateSourceUrl =. erCandidateSourceUrl
        , EventResearchCandidateInfoUrl =. erCandidateInfoUrl
        , EventResearchCandidatePurchaseUrl =. erCandidatePurchaseUrl
        , EventResearchCandidatePayload =. encodeJson erCandidatePayload
        , EventResearchCandidateEvidence =. encodeJson erCandidateEvidence
        , EventResearchCandidateConfidence =. erCandidateConfidence
        , EventResearchCandidateManagedFields =. encodeJson erCandidateManagedFields
        , EventResearchCandidateContentHash =. contentHash
        , EventResearchCandidateVerifiedAt =. erCandidateVerifiedAt
        , EventResearchCandidateUpdatedAt =. now
        ]

insertChange :: EventResearchCandidateWriteDTO -> EventResearchRunId -> Maybe EventResearchCandidateId -> Maybe SocialEventId -> T.Text -> Maybe T.Text -> Maybe T.Text -> T.Text -> UTCTime -> SqlPersistT IO ()
insertChange candidate runId candidateId eventId action beforeValue afterValue contentHash now = do
    let dedupeKey = sha256Text . BL.fromStrict . TE.encodeUtf8 $
            T.intercalate ":" [renderKey runId, candidate.erCandidateProvider, candidate.erCandidateExternalId, action, contentHash, T.pack (show (candidate.erCandidateVerifiedAt))]
        row = EventResearchChange runId candidateId eventId action beforeValue afterValue
                (candidate.erCandidateSourceUrl) (candidate.erCandidateConfidence) (candidate.erCandidateVerifiedAt)
                (candidate.erCandidateExternalId) "confirmed" dedupeKey now
    _ <- insertUnique row
    pure ()

validateEventResearchCandidate :: EventResearchCandidateWriteDTO -> Either T.Text EventResearchCandidateWriteDTO
validateEventResearchCandidate candidate = do
    provider <- normalizeProvider (candidate.erCandidateProvider)
    externalId <- normalizeIdentifier "external identifier" 512 (candidate.erCandidateExternalId)
    _ <- parsePositiveId "run" candidate.erCandidateRunId
    _ <- traverse (parsePositiveId "event source") candidate.erCandidateSourceId
    reviewState <- normalizeReviewState (candidate.erCandidateReviewState)
    title <- normalizeRequired "title" 240 (candidate.erCandidateTitle)
    timezone <- normalizeTimeZone (candidate.erCandidateTimezone)
    countryCode <- normalizeCountryCode (candidate.erCandidateCountryCode)
    sourceUrl <- normalizeHttpsUrl "source URL" (candidate.erCandidateSourceUrl)
    infoUrl <- traverse (normalizeHttpsUrl "information URL") (candidate.erCandidateInfoUrl)
    purchaseUrl <- traverse (normalizeHttpsUrl "purchase URL") (candidate.erCandidatePurchaseUrl)
    venueName <- traverse (normalizeRequired "venue name" 240) (candidate.erCandidateVenueName)
    city <- traverse (normalizeRequired "city" 160) (candidate.erCandidateCity)
    province <- traverse (normalizeRequired "province" 160) (candidate.erCandidateProvince)
    confidence <- normalizeConfidence (candidate.erCandidateConfidence)
    unless (not (null (candidate.erCandidateEvidence))) (Left "at least one evidence URL is required")
    evidence <- traverse validateEvidence (candidate.erCandidateEvidence)
    unless (any ((== sourceUrl) . erEvidenceUrl) evidence) (Left "evidence must include the primary source URL")
    case (candidate.erCandidateStartTime, candidate.erCandidateEndTime) of
        (Just startTime, Just endTime) -> unless (startTime < endTime) (Left "start time must be before end time")
        _ -> pure ()
    when (confidence == "high") $ do
        unless (isJust (candidate.erCandidateStartTime)) (Left "high confidence requires a confirmed start time")
        unless (isJust venueName && isJust city) (Left "high confidence requires confirmed venue and city")
        unless (isJust purchaseUrl && any ((== "official_sale") . erEvidenceKind) evidence) (Left "high confidence requires an official sale page and direct purchase URL")
    managedFields <- traverse (normalizeIdentifier "managed field" 120) (candidate.erCandidateManagedFields)
    pure
        EventResearchCandidateWriteDTO
        { erCandidateProvider = provider
        , erCandidateExternalId = externalId
        , erCandidateRunId = candidate.erCandidateRunId
        , erCandidateSourceId = candidate.erCandidateSourceId
        , erCandidateReviewState = reviewState
        , erCandidateTitle = title
        , erCandidateStartTime = candidate.erCandidateStartTime
        , erCandidateEndTime = candidate.erCandidateEndTime
        , erCandidateTimezone = timezone
        , erCandidateVenueName = venueName
        , erCandidateCity = city
        , erCandidateProvince = province
        , erCandidateCountryCode = countryCode
        , erCandidateSourceUrl = sourceUrl
        , erCandidateInfoUrl = infoUrl
        , erCandidatePurchaseUrl = purchaseUrl
        , erCandidatePayload = candidate.erCandidatePayload
        , erCandidateEvidence = evidence
        , erCandidateConfidence = confidence
        , erCandidateManagedFields = managedFields
        , erCandidateVerifiedAt = candidate.erCandidateVerifiedAt
        }

validateEvidence :: EventResearchEvidenceDTO -> Either T.Text EventResearchEvidenceDTO
validateEvidence evidence = do
    url <- normalizeHttpsUrl "evidence URL" (erEvidenceUrl evidence)
    kind <- normalizeIdentifier "evidence kind" 80 (erEvidenceKind evidence)
    notes <- traverse (normalizeRequired "evidence notes" 1000) (erEvidenceNotes evidence)
    pure evidence{erEvidenceUrl = url, erEvidenceKind = kind, erEvidenceNotes = notes}

eventResearchCandidateContentHash :: EventResearchCandidateWriteDTO -> T.Text
eventResearchCandidateContentHash candidate =
    sha256Text . Aeson.encode $
        Aeson.object
            [ "provider" Aeson..= candidate.erCandidateProvider
            , "externalId" Aeson..= candidate.erCandidateExternalId
            , "sourceId" Aeson..= candidate.erCandidateSourceId
            , "reviewState" Aeson..= candidate.erCandidateReviewState
            , "title" Aeson..= candidate.erCandidateTitle
            , "startTime" Aeson..= candidate.erCandidateStartTime
            , "endTime" Aeson..= candidate.erCandidateEndTime
            , "timezone" Aeson..= candidate.erCandidateTimezone
            , "venueName" Aeson..= candidate.erCandidateVenueName
            , "city" Aeson..= candidate.erCandidateCity
            , "province" Aeson..= candidate.erCandidateProvince
            , "countryCode" Aeson..= candidate.erCandidateCountryCode
            , "sourceUrl" Aeson..= candidate.erCandidateSourceUrl
            , "infoUrl" Aeson..= candidate.erCandidateInfoUrl
            , "purchaseUrl" Aeson..= candidate.erCandidatePurchaseUrl
            , "payload" Aeson..= candidate.erCandidatePayload
            , "evidence" Aeson..= map evidenceContent (candidate.erCandidateEvidence)
            , "confidence" Aeson..= candidate.erCandidateConfidence
            , "managedFields" Aeson..= candidate.erCandidateManagedFields
            ]
  where
    evidenceContent evidence =
        Aeson.object
            [ "url" Aeson..= erEvidenceUrl evidence
            , "kind" Aeson..= erEvidenceKind evidence
            , "notes" Aeson..= erEvidenceNotes evidence
            ]

runEntityToDTO :: Entity EventResearchRun -> Either T.Text EventResearchRunDTO
runEntityToDTO (Entity key row) = do
    counters <- decodeJson "run counters" (eventResearchRunCounters row)
    pure EventResearchRunDTO
        { erRunId = renderKey key
        , erRunKey = eventResearchRunRunKey row
        , erRunStatus = eventResearchRunStatus row
        , erRunReconciliation = eventResearchRunReconciliation row
        , erRunCheckpoint = eventResearchRunCheckpoint row
        , erRunCounters = counters
        , erRunErrorSummary = eventResearchRunErrorSummary row
        , erRunStartedAt = eventResearchRunStartedAt row
        , erRunUpdatedAt = eventResearchRunUpdatedAt row
        , erRunFinishedAt = eventResearchRunFinishedAt row
        , erRunCreatedByPartyId = eventResearchRunCreatedByPartyId row
        }

candidateEntityToDTO :: Entity EventResearchCandidate -> Either T.Text EventResearchCandidateDTO
candidateEntityToDTO (Entity key row) = do
    payload <- decodeJson "candidate payload" (eventResearchCandidatePayload row)
    evidence <- decodeJson "candidate evidence" (eventResearchCandidateEvidence row)
    managedFields <- decodeJson "candidate managed fields" (eventResearchCandidateManagedFields row)
    pure EventResearchCandidateDTO
        { erCandidateId = renderKey key
        , erCandidateProvider = eventResearchCandidateProvider row
        , erCandidateExternalId = eventResearchCandidateExternalId row
        , erCandidateRunId = renderKey (eventResearchCandidateRunId row)
        , erCandidateSourceId = renderKey <$> eventResearchCandidateSourceId row
        , erCandidateEventId = renderKey <$> eventResearchCandidateEventId row
        , erCandidateReviewState = eventResearchCandidateReviewState row
        , erCandidateTitle = eventResearchCandidateTitle row
        , erCandidateStartTime = eventResearchCandidateStartTime row
        , erCandidateEndTime = eventResearchCandidateEndTime row
        , erCandidateTimezone = eventResearchCandidateTimezone row
        , erCandidateVenueName = eventResearchCandidateVenueName row
        , erCandidateCity = eventResearchCandidateCity row
        , erCandidateProvince = eventResearchCandidateProvince row
        , erCandidateCountryCode = eventResearchCandidateCountryCode row
        , erCandidateSourceUrl = eventResearchCandidateSourceUrl row
        , erCandidateInfoUrl = eventResearchCandidateInfoUrl row
        , erCandidatePurchaseUrl = eventResearchCandidatePurchaseUrl row
        , erCandidatePayload = payload
        , erCandidateEvidence = evidence
        , erCandidateConfidence = eventResearchCandidateConfidence row
        , erCandidateManagedFields = managedFields
        , erCandidateContentHash = SM.eventResearchCandidateContentHash row
        , erCandidateVerifiedAt = eventResearchCandidateVerifiedAt row
        , erCandidateIsPilot = eventResearchCandidateIsPilot row
        , erCandidateCreatedAt = eventResearchCandidateCreatedAt row
        , erCandidateUpdatedAt = eventResearchCandidateUpdatedAt row
        }

changeEntityToDTO :: Entity EventResearchChange -> Either T.Text EventResearchChangeDTO
changeEntityToDTO (Entity key row) = do
    beforeValue <- traverse (decodeJson "change before value") (eventResearchChangeBeforeValue row)
    afterValue <- traverse (decodeJson "change after value") (eventResearchChangeAfterValue row)
    pure EventResearchChangeDTO
        { erChangeId = renderKey key
        , erChangeRunId = renderKey (eventResearchChangeRunId row)
        , erChangeCandidateId = renderKey <$> eventResearchChangeCandidateId row
        , erChangeEventId = renderKey <$> eventResearchChangeEventId row
        , erChangeAction = eventResearchChangeAction row
        , erChangeBeforeValue = beforeValue
        , erChangeAfterValue = afterValue
        , erChangeSourceUrl = eventResearchChangeSourceUrl row
        , erChangeConfidence = eventResearchChangeConfidence row
        , erChangeConsultedAt = eventResearchChangeConsultedAt row
        , erChangeExternalId = eventResearchChangeExternalId row
        , erChangeResult = eventResearchChangeResult row
        , erChangeCreatedAt = eventResearchChangeCreatedAt row
        }

normalizeRunKey :: T.Text -> Either T.Text T.Text
normalizeRunKey raw = do
    value <- normalizeIdentifier "run key" 160 raw
    unless (T.all (\ch -> isAscii ch && (isAlphaNum ch || ch `elem` ("._:-" :: String))) value) (Left "run key contains unsupported characters")
    pure value

normalizeRunStatus :: T.Text -> Either T.Text T.Text
normalizeRunStatus raw =
    let value = T.toLower (T.strip raw)
     in if value `elem` ["running", "completed", "failed"] then Right value else Left "run status must be running, completed, or failed"

normalizeReviewState :: T.Text -> Either T.Text T.Text
normalizeReviewState raw =
    let value = T.toLower (T.strip raw)
     in if value `elem` ["draft", "review", "discarded"] then Right value else Left "review state must be draft, review, or discarded"

normalizeConfidence :: T.Text -> Either T.Text T.Text
normalizeConfidence raw =
    let value = T.toLower (T.strip raw)
     in if value `elem` ["high", "medium", "low"] then Right value else Left "confidence must be high, medium, or low"

normalizeProvider :: T.Text -> Either T.Text T.Text
normalizeProvider = fmap T.toLower . normalizeIdentifier "provider" 80

normalizeIdentifier :: T.Text -> Int -> T.Text -> Either T.Text T.Text
normalizeIdentifier label maxLength raw = do
    value <- normalizeRequired label maxLength raw
    unless (T.all (\ch -> isAscii ch && not (isControl ch)) value) (Left (label <> " must contain safe ASCII characters"))
    pure value

normalizeRequired :: T.Text -> Int -> T.Text -> Either T.Text T.Text
normalizeRequired label maxLength raw =
    let value = T.unwords (T.words (T.strip raw))
     in if T.null value || T.length value > maxLength || T.any isControl raw
            then Left (label <> " must be nonblank, safe, and at most " <> T.pack (show maxLength) <> " characters")
            else Right value

normalizeTimeZone :: T.Text -> Either T.Text T.Text
normalizeTimeZone raw = do
    value <- normalizeRequired "timezone" 64 raw
    unless (T.all (\ch -> isAlphaNum ch || ch `elem` ("_+-/" :: String)) value && "/" `T.isInfixOf` value) (Left "timezone must be an explicit IANA-style identifier")
    pure value

normalizeCountryCode :: T.Text -> Either T.Text T.Text
normalizeCountryCode raw =
    let value = T.toUpper (T.strip raw)
     in if T.length value == 2 && T.all (\ch -> isAscii ch && ch >= 'A' && ch <= 'Z') value then Right value else Left "country code must be two ASCII letters"

normalizeHttpsUrl :: T.Text -> T.Text -> Either T.Text T.Text
normalizeHttpsUrl label raw = do
    value <- normalizeRequired label 2048 raw
    unless ("https://" `T.isPrefixOf` T.toLower value && TrialsServer.isValidHttpUrl value) (Left (label <> " must be a valid HTTPS URL"))
    pure value

boundedLimit :: Maybe Int -> Either ServerError Int
boundedLimit raw =
    let value = maybe 100 id raw
     in if value >= 1 && value <= 500 then Right value else Left err400{errBody = "limit must be between 1 and 500"}

parseKey :: (ToBackendKey SqlBackend record) => T.Text -> T.Text -> Either T.Text (Key record)
parseKey label raw = toSqlKey <$> parsePositiveId label raw

parsePositiveId :: T.Text -> T.Text -> Either T.Text Int64
parsePositiveId label raw =
    case readMaybe (T.unpack (T.strip raw)) :: Maybe Int64 of
        Just value | value > 0 -> Right value
        _ -> Left (label <> " id must be a positive integer")

parseKeyUnsafe :: (ToBackendKey SqlBackend record) => T.Text -> Key record
parseKeyUnsafe raw = toSqlKey (maybe 0 id (readMaybe (T.unpack raw)))

renderKey :: (ToBackendKey SqlBackend record) => Key record -> T.Text
renderKey = T.pack . show . fromSqlKey

encodeJson :: Aeson.ToJSON value => value -> T.Text
encodeJson = TE.decodeUtf8 . BL.toStrict . Aeson.encode

decodeJson :: Aeson.FromJSON value => T.Text -> T.Text -> Either T.Text value
decodeJson label raw =
    case Aeson.eitherDecodeStrict' (TE.encodeUtf8 raw) of
        Left message -> Left (label <> " is invalid JSON: " <> T.pack message)
        Right value -> Right value

sha256Text :: BL.ByteString -> T.Text
sha256Text bytes =
    TE.decodeUtf8 (BAE.convertToBase BAE.Base16 (hash (BL.toStrict bytes) :: Digest SHA256))

badRequest :: T.Text -> ServerError
badRequest message = err400{errBody = BL.fromStrict (TE.encodeUtf8 message)}

storedDataError :: T.Text -> ServerError
storedDataError message = err500{errBody = BL.fromStrict (TE.encodeUtf8 message)}
