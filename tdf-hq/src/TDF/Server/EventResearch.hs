{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.EventResearch
    ( eventResearchServer
    , validateEventResearchCandidate
    , validateEventResearchMaterialization
    , eventResearchCandidateContentHash
    , eventResearchMaterializationDedupeKey
    ) where

import Control.Exception (SomeException, displayException, try)
import Control.Applicative ((<|>))
import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii, isControl)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List (nubBy)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime, utctDay)
import Data.UUID (UUID)
import Database.Persist
import Database.Persist.Sql
    ( Single (..)
    , SqlBackend
    , SqlPersistT
    , fromSqlKey
    , rawSql
    , runSqlPool
    , toSqlKey
    , transactionSave
    , transactionUndo
    )
import Servant
import Text.Read (readMaybe)

import TDF.API.EventResearchAPI
import TDF.Auth (AuthedUser (..), hasStrictAdminAccess)
import TDF.DB (Env (..))
import TDF.DTO.EventResearchDTO
import TDF.Models.SocialEventsModels hiding (eventResearchCandidateContentHash)
import qualified TDF.Models.SocialEventsModels as SM
import qualified TDF.SocialEventLifecycle as EventLifecycle
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
        :<|> materializeCandidate
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

    materializeCandidate rawCandidateId request = do
        requireAdmin
        candidateId <- either (throwError . badRequest) pure (parseKey "candidate" rawCandidateId)
        materializationRunId <-
            either (throwError . badRequest) pure (parseKey "run" request.erMaterializationRunId)
        Env{..} <- ask
        now <- liftIO getCurrentTime
        attempted <-
            liftIO
                ( try (runSqlPool (materializeCandidateTransaction candidateId materializationRunId request now) envPool)
                    :: IO (Either SomeException (Either ServerError EventResearchMaterializationDTO))
                )
        case attempted of
            Left exc ->
                throwError
                    err500
                        { errBody =
                            if "ambiguous" `T.isInfixOf` T.toLower (T.pack (displayException exc))
                                then "Event research materialization failed because entity resolution is ambiguous"
                                else "Event research materialization transaction failed"
                        }
            Right result -> either throwError pure result

    listChanges mRunId mLimit = do
        requireAdmin
        runId <- traverse (either (throwError . badRequest) pure . parseKey "run") mRunId
        limit <- either throwError pure (boundedLimit mLimit)
        Env{..} <- ask
        let filters = maybe [] (\value -> [EventResearchChangeRunId ==. value]) runId
        rows <- liftIO $ runSqlPool (selectList filters [Desc EventResearchChangeCreatedAt, LimitTo limit]) envPool
        traverse (either (throwError . storedDataError) pure . changeEntityToDTO) rows

data MaterializationCoordinates = MaterializationCoordinates
    { mcLatitude :: Double
    , mcLongitude :: Double
    }
    deriving (Show, Eq)

instance Aeson.FromJSON MaterializationCoordinates where
    parseJSON = Aeson.withObject "MaterializationCoordinates" $ \o ->
        MaterializationCoordinates
            <$> o Aeson..: "latitude"
            <*> o Aeson..: "longitude"

data MaterializationImage = MaterializationImage
    { miUrl :: Maybe T.Text
    , miPermission :: Maybe T.Text
    }
    deriving (Show, Eq)

instance Aeson.FromJSON MaterializationImage where
    parseJSON = Aeson.withObject "MaterializationImage" $ \o ->
        MaterializationImage
            <$> o Aeson..:? "url"
            <*> o Aeson..:? "permission"

newtype MaterializationPrice = MaterializationPrice
    { mpCurrency :: T.Text
    }
    deriving (Show, Eq)

instance Aeson.FromJSON MaterializationPrice where
    parseJSON = Aeson.withObject "MaterializationPrice" $ \o ->
        MaterializationPrice <$> o Aeson..: "currency"

data MaterializationPayload = MaterializationPayload
    { mpDescription :: Maybe T.Text
    , mpEventType :: T.Text
    , mpLineup :: [T.Text]
    , mpAddress :: Maybe T.Text
    , mpCoordinates :: Maybe MaterializationCoordinates
    , mpImage :: Maybe MaterializationImage
    , mpPrices :: [MaterializationPrice]
    , mpAvailability :: Maybe T.Text
    , mpPublicationBlockers :: [T.Text]
    }
    deriving (Show, Eq)

instance Aeson.FromJSON MaterializationPayload where
    parseJSON = Aeson.withObject "MaterializationPayload" $ \o ->
        MaterializationPayload
            <$> o Aeson..:? "description"
            <*> o Aeson..: "eventType"
            <*> (o Aeson..:? "lineup" Aeson..!= [])
            <*> o Aeson..:? "address"
            <*> o Aeson..:? "coordinates"
            <*> o Aeson..:? "image"
            <*> (o Aeson..:? "prices" Aeson..!= [])
            <*> o Aeson..:? "availability"
            <*> (o Aeson..:? "publicationBlockers" Aeson..!= [])

data ValidatedMaterialization = ValidatedMaterialization
    { vmStartTime :: UTCTime
    , vmVenueName :: T.Text
    , vmCity :: T.Text
    , vmDescription :: Maybe T.Text
    , vmEventType :: T.Text
    , vmLineup :: [T.Text]
    , vmAddress :: Maybe T.Text
    , vmCoordinates :: Maybe MaterializationCoordinates
    , vmImageUrl :: Maybe T.Text
    , vmCurrency :: Maybe T.Text
    , vmSourceStatus :: T.Text
    }
    deriving (Show, Eq)

validateEventResearchMaterialization
    :: Bool
    -> EventResearchMaterializationRequestDTO
    -> EventResearchCandidateDTO
    -> Either T.Text ValidatedMaterialization
validateEventResearchMaterialization pilotApproved _request candidate = do
    unless pilotApproved (Left "the event research pilot is not approved")
    validatedCandidate <- validateEventResearchCandidate (candidateDTOToWrite candidate)
    unless (validatedCandidate.erCandidateConfidence == "high")
        (Left "only high-confidence candidates can be materialized")
    unless (validatedCandidate.erCandidateReviewState == "draft")
        (Left "only approved draft candidates can be materialized")
    startTime <- maybe (Left "a confirmed start time is required") Right validatedCandidate.erCandidateStartTime
    venueName <- maybe (Left "a confirmed venue is required") Right validatedCandidate.erCandidateVenueName
    city <- maybe (Left "a confirmed city is required") Right validatedCandidate.erCandidateCity
    payload <- case (Aeson.fromJSON validatedCandidate.erCandidatePayload :: Aeson.Result MaterializationPayload) of
        Aeson.Error message -> Left ("candidate payload cannot be materialized: " <> T.pack message)
        Aeson.Success value -> Right value
    description <- traverse (normalizeRequired "description" 2000) payload.mpDescription
    eventType <- normalizeIdentifier "event type" 80 payload.mpEventType
    lineup <- distinctEntityNames <$> traverse (normalizeRequired "lineup artist" 240) payload.mpLineup
    unless (not (null lineup)) (Left "at least one confirmed lineup artist is required")
    address <- traverse (normalizeRequired "venue address" 500) payload.mpAddress
    traverse_ validateCoordinates payload.mpCoordinates
    let blockers = map (T.toLower . T.strip) payload.mpPublicationBlockers
        unsupportedBlockers = filter (/= "event_end_unconfirmed") blockers
    unless (null unsupportedBlockers)
        (Left ("candidate still has publication blockers: " <> T.intercalate ", " unsupportedBlockers))
    let availability = T.toLower . T.strip <$> payload.mpAvailability
    when (availability `elem` map Just ["cancelled", "canceled", "postponed"])
        (Left "cancelled or postponed candidates require review before materialization")
    currency <- uniquePriceCurrency payload.mpPrices
    imageUrl <- usableMaterializationImage payload.mpImage
    pure
        ValidatedMaterialization
            { vmStartTime = startTime
            , vmVenueName = venueName
            , vmCity = city
            , vmDescription = description
            , vmEventType = eventType
            , vmLineup = lineup
            , vmAddress = address
            , vmCoordinates = payload.mpCoordinates
            , vmImageUrl = imageUrl
            , vmCurrency = currency
            , vmSourceStatus = if availability == Just "sold_out" then "sold_out" else "on_sale"
            }
  where
    validateCoordinates MaterializationCoordinates{..} = do
        unless (mcLatitude >= -90 && mcLatitude <= 90) (Left "venue latitude is out of range")
        unless (mcLongitude >= -180 && mcLongitude <= 180) (Left "venue longitude is out of range")

candidateDTOToWrite :: EventResearchCandidateDTO -> EventResearchCandidateWriteDTO
candidateDTOToWrite EventResearchCandidateDTO{..} =
    EventResearchCandidateWriteDTO
        { erCandidateProvider = erCandidateProvider
        , erCandidateExternalId = erCandidateExternalId
        , erCandidateRunId = erCandidateRunId
        , erCandidateSourceId = erCandidateSourceId
        , erCandidateReviewState = erCandidateReviewState
        , erCandidateTitle = erCandidateTitle
        , erCandidateStartTime = erCandidateStartTime
        , erCandidateEndTime = erCandidateEndTime
        , erCandidateTimezone = erCandidateTimezone
        , erCandidateVenueName = erCandidateVenueName
        , erCandidateCity = erCandidateCity
        , erCandidateProvince = erCandidateProvince
        , erCandidateCountryCode = erCandidateCountryCode
        , erCandidateSourceUrl = erCandidateSourceUrl
        , erCandidateInfoUrl = erCandidateInfoUrl
        , erCandidatePurchaseUrl = erCandidatePurchaseUrl
        , erCandidatePayload = erCandidatePayload
        , erCandidateEvidence = erCandidateEvidence
        , erCandidateConfidence = erCandidateConfidence
        , erCandidateManagedFields = erCandidateManagedFields
        , erCandidateVerifiedAt = erCandidateVerifiedAt
        }

distinctEntityNames :: [T.Text] -> [T.Text]
distinctEntityNames =
    nubBy (\left right -> normalizeEntityText left == normalizeEntityText right)

normalizeEntityText :: T.Text -> T.Text
normalizeEntityText =
    T.unwords
        . T.words
        . T.map (\ch -> if isAlphaNum ch then ch else ' ')
        . T.toCaseFold
        . T.strip

uniquePriceCurrency :: [MaterializationPrice] -> Either T.Text (Maybe T.Text)
uniquePriceCurrency prices = do
    currencies <-
        distinctEntityNames
            <$> traverse (normalizeCurrency . mpCurrency) prices
    case currencies of
        [] -> Right Nothing
        [currency] -> Right (Just currency)
        _ -> Left "candidate prices use more than one currency"
  where
    normalizeCurrency raw =
        let currency = T.toUpper (T.strip raw)
         in if T.length currency == 3 && T.all (\ch -> isAscii ch && ch >= 'A' && ch <= 'Z') currency
                then Right currency
                else Left "candidate price currency must be a three-letter code"

usableMaterializationImage :: Maybe MaterializationImage -> Either T.Text (Maybe T.Text)
usableMaterializationImage Nothing = Right Nothing
usableMaterializationImage (Just MaterializationImage{..})
    | fmap (T.toLower . T.strip) miPermission == Just "confirmed" =
        traverse (normalizeHttpsUrl "event image URL") miUrl
    | otherwise = Right Nothing

materializeCandidateTransaction
    :: EventResearchCandidateId
    -> EventResearchRunId
    -> EventResearchMaterializationRequestDTO
    -> UTCTime
    -> SqlPersistT IO (Either ServerError EventResearchMaterializationDTO)
materializeCandidateTransaction candidateId materializationRunId request now = do
    result <- materializeCandidateDb candidateId materializationRunId request now
    case result of
        Left serverError -> transactionUndo >> pure (Left serverError)
        Right response -> transactionSave >> pure (Right response)

materializeCandidateDb
    :: EventResearchCandidateId
    -> EventResearchRunId
    -> EventResearchMaterializationRequestDTO
    -> UTCTime
    -> SqlPersistT IO (Either ServerError EventResearchMaterializationDTO)
materializeCandidateDb candidateId materializationRunId request now = do
    controls <-
        rawSql
            "SELECT ?? FROM event_research_pilot_control WHERE control_key=? FOR UPDATE"
            [PersistText "default"]
    case controls :: [Entity EventResearchPilotControl] of
        [] -> pure (Left err500{errBody = "Event research pilot control is not initialized"})
        [Entity _ control] -> do
            candidates <-
                rawSql
                    "SELECT ?? FROM event_research_candidate WHERE id=? FOR UPDATE"
                    [toPersistValue candidateId]
            case candidates :: [Entity EventResearchCandidate] of
                [] -> pure (Left err404{errBody = "Event research candidate not found"})
                [candidateEntity@(Entity _ lockedCandidate)] ->
                    case eventResearchCandidateEventId lockedCandidate of
                        Just eventId -> linkCandidateAndRespond candidateEntity materializationRunId eventId False now
                        Nothing -> materializeUnlinkedCandidate (eventResearchPilotControlApproved control) materializationRunId candidateEntity request now
                _ -> pure (Left err500{errBody = "Event research candidate identity is ambiguous"})
        _ -> pure (Left err500{errBody = "Event research pilot control identity is ambiguous"})

materializeUnlinkedCandidate
    :: Bool
    -> EventResearchRunId
    -> Entity EventResearchCandidate
    -> EventResearchMaterializationRequestDTO
    -> UTCTime
    -> SqlPersistT IO (Either ServerError EventResearchMaterializationDTO)
materializeUnlinkedCandidate pilotApproved materializationRunId candidateEntity@(Entity _ candidate) request now = do
    runResult <- materializationRunCanWrite materializationRunId
    case runResult of
        Left serverError -> pure (Left serverError)
        Right () ->
            case candidateEntityToDTO candidateEntity of
                Left message -> pure (Left (storedDataError message))
                Right candidateDTO ->
                    case validateEventResearchMaterialization pilotApproved request candidateDTO of
                        Left message -> pure (Left (conflict message))
                        Right validated -> do
                            existingRef <-
                                getBy
                                    ( UniqueExternalEventRef
                                        (eventResearchCandidateProvider candidate)
                                        (eventResearchCandidateExternalId candidate)
                                    )
                            case existingRef of
                                Just (Entity _ ref) -> do
                                    suitable <- existingEventCanSatisfy request (externalEventRefEventId ref)
                                    if suitable
                                        then linkCandidateAndRespond candidateEntity materializationRunId (externalEventRefEventId ref) False now
                                        else pure (Left (conflict "the existing provider event is not safely publishable"))
                                Nothing -> createOrLinkMaterializedEvent candidateEntity materializationRunId request validated now

materializationRunCanWrite :: EventResearchRunId -> SqlPersistT IO (Either ServerError ())
materializationRunCanWrite runId = do
    runs <-
        rawSql
            "SELECT ?? FROM event_research_run WHERE id=? FOR UPDATE"
            [toPersistValue runId]
    pure $ case runs :: [Entity EventResearchRun] of
        [] -> Left err404{errBody = "Event research materialization run not found"}
        [Entity _ row]
            | eventResearchRunStatus row == "running" -> Right ()
            | otherwise -> Left (conflict "the event research materialization run is not open")
        _ -> Left err500{errBody = "Event research materialization run identity is ambiguous"}

createOrLinkMaterializedEvent
    :: Entity EventResearchCandidate
    -> EventResearchRunId
    -> EventResearchMaterializationRequestDTO
    -> ValidatedMaterialization
    -> UTCTime
    -> SqlPersistT IO (Either ServerError EventResearchMaterializationDTO)
createOrLinkMaterializedEvent candidateEntity@(Entity _ candidate) materializationRunId request validated now = do
    eventTypeResult <- resolveMaterializationEventType now validated.vmEventType
    case eventTypeResult of
        Left serverError -> pure (Left serverError)
        Right eventTypeId -> do
            workflowStateId <-
                EventLifecycle.resolveActiveSocialEventStateId
                    (if request.erMaterializationPublish then "on_sale" else "planning")
            venueResult <- resolveMaterializationVenue candidate validated now
            case venueResult of
                Left serverError -> pure (Left serverError)
                Right venueId -> do
                    duplicateResult <- findMaterializationDuplicate candidate validated venueId
                    case duplicateResult of
                        Left serverError -> pure (Left serverError)
                        Right (Just eventId) -> do
                            suitable <- existingEventCanSatisfy request eventId
                            if not suitable
                                then pure (Left (conflict "a matching event exists but has manual visibility or workflow state"))
                                else do
                                    insertedRef <- insertMaterializationEventRef candidate validated eventId now
                                    if insertedRef
                                        then linkCandidateAndRespond candidateEntity materializationRunId eventId False now
                                        else pure (Left (conflict "the provider event was materialized concurrently"))
                        Right Nothing -> do
                            artistsResult <- resolveMaterializationArtists candidate validated now
                            case artistsResult of
                                Left serverError -> pure (Left serverError)
                                Right artistIds -> do
                                    let metadata = materializationEventMetadata candidate request validated
                                    eventId <-
                                        insert
                                            SocialEvent
                                                { socialEventOrganizerPartyId = Just "system:event-research"
                                                , socialEventTitle = eventResearchCandidateTitle candidate
                                                , socialEventDescription = validated.vmDescription
                                                , socialEventVenueId = Just venueId
                                                , socialEventEventTypeId = Just eventTypeId
                                                , socialEventWorkflowStateId = Just workflowStateId
                                                , socialEventTimezone = Just (eventResearchCandidateTimezone candidate)
                                                , socialEventStartTime = validated.vmStartTime
                                                , socialEventEndTime = eventResearchCandidateEndTime candidate
                                                , socialEventPriceCents = Nothing
                                                , socialEventCurrencyId = Nothing
                                                , socialEventCapacity = Nothing
                                                , socialEventMetadata = Just metadata
                                                , socialEventCreatedAt = now
                                                , socialEventUpdatedAt = now
                                                }
                                    forM_ artistIds $ \artistId -> do
                                        _ <- insertUnique (EventArtist eventId artistId Nothing)
                                        pure ()
                                    insertedRef <- insertMaterializationEventRef candidate validated eventId now
                                    if insertedRef
                                        then linkCandidateAndRespond candidateEntity materializationRunId eventId True now
                                        else pure (Left (conflict "the provider event was materialized concurrently"))

resolveMaterializationEventType :: UTCTime -> T.Text -> SqlPersistT IO (Either ServerError UUID)
resolveMaterializationEventType now eventTypeCode = do
    rows <-
        rawSql
            "SELECT item.id FROM event_type item JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='event-types' AND catalog.active=TRUE JOIN workflow_state state ON state.id=item.workflow_state_id AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active=TRUE WHERE lower(trim(?)) IN (lower(item.code), lower(item.name_es), lower(item.name_en), lower(COALESCE(item.current_slug,''))) AND item.active=TRUE AND item.deprecated_at IS NULL AND (item.effective_from IS NULL OR item.effective_from<=?) AND (item.effective_until IS NULL OR item.effective_until>=?) ORDER BY item.id LIMIT 2"
            [toPersistValue eventTypeCode, toPersistValue (utctDay now), toPersistValue (utctDay now)]
    pure $ case rows :: [Single UUID] of
        [Single eventTypeId] -> Right eventTypeId
        [] -> Left (conflict "candidate event type is not active and published")
        _ -> Left (conflict "candidate event type identity is ambiguous")

resolveMaterializationVenue
    :: EventResearchCandidate
    -> ValidatedMaterialization
    -> UTCTime
    -> SqlPersistT IO (Either ServerError VenueId)
resolveMaterializationVenue candidate validated now = do
    let provider = eventResearchCandidateProvider candidate
        externalId = researchEntityExternalId "venue" [validated.vmVenueName, validated.vmCity, eventResearchCandidateCountryCode candidate]
    existingRef <- getBy (UniqueExternalVenueRef provider externalId)
    case existingRef of
        Just (Entity _ ref) -> do
            venue <- get (externalVenueRefVenueId ref)
            pure $ maybe (Left (storedDataError "stored venue reference is broken")) (const (Right (externalVenueRefVenueId ref))) venue
        Nothing -> do
            venues <- selectList [] []
            let matches =
                    filter
                        (\(Entity _ venue) ->
                            normalizeEntityText (venueName venue) == normalizeEntityText validated.vmVenueName
                                && maybe False ((== normalizeEntityText validated.vmCity) . normalizeEntityText) (venueCity venue)
                                && maybe True ((== eventResearchCandidateCountryCode candidate) . T.toUpper . T.strip) (venueCountryCode venue)
                        )
                        venues
            case matches of
                [Entity venueId _] -> do
                    _ <- insertUnique (ExternalVenueRef provider externalId venueId now)
                    pure (Right venueId)
                [] -> do
                    let (latitude, longitude) =
                            maybe (Nothing, Nothing) (\coords -> (Just coords.mcLatitude, Just coords.mcLongitude)) validated.vmCoordinates
                        contact =
                            fmap
                                (\province -> encodeJson (Aeson.object ["state" Aeson..= province]))
                                (eventResearchCandidateProvince candidate)
                    venueId <-
                        insert
                            Venue
                                { venueName = validated.vmVenueName
                                , venueAddress = validated.vmAddress
                                , venueCity = Just validated.vmCity
                                , venueCountry = Nothing
                                , venueCountryCode = Just (eventResearchCandidateCountryCode candidate)
                                , venueCountryId = Nothing
                                , venueCityId = Nothing
                                , venueTimezone = Just (eventResearchCandidateTimezone candidate)
                                , venueLatitude = latitude
                                , venueLongitude = longitude
                                , venueCapacity = Nothing
                                , venueContact = contact
                                , venueCreatedAt = now
                                , venueUpdatedAt = now
                                }
                    inserted <- insertUnique (ExternalVenueRef provider externalId venueId now)
                    pure $ maybe (Left (conflict "venue identity was created concurrently")) (const (Right venueId)) inserted
                _ -> pure (Left (conflict "venue identity is ambiguous"))

resolveMaterializationArtists
    :: EventResearchCandidate
    -> ValidatedMaterialization
    -> UTCTime
    -> SqlPersistT IO (Either ServerError [ArtistProfileId])
resolveMaterializationArtists candidate validated now = do
    results <- forM validated.vmLineup (resolveMaterializationArtist (eventResearchCandidateProvider candidate) now)
    pure (sequence results)

resolveMaterializationArtist
    :: T.Text
    -> UTCTime
    -> T.Text
    -> SqlPersistT IO (Either ServerError ArtistProfileId)
resolveMaterializationArtist provider now artistName = do
    let externalId = researchEntityExternalId "artist" [artistName]
    existingRef <- getBy (UniqueExternalArtistRef provider externalId)
    case existingRef of
        Just (Entity _ ref) -> do
            artist <- get (externalArtistRefArtistId ref)
            pure $ maybe (Left (storedDataError "stored artist reference is broken")) (const (Right (externalArtistRefArtistId ref))) artist
        Nothing -> do
            artists <- selectList [] []
            let matches = filter ((== normalizeEntityText artistName) . normalizeEntityText . artistProfileName . entityVal) artists
            case matches of
                [Entity artistId _] -> do
                    _ <- insertUnique (ExternalArtistRef provider externalId artistId now)
                    pure (Right artistId)
                [] -> do
                    artistId <-
                        insert
                            ArtistProfile
                                { artistProfilePartyId = Nothing
                                , artistProfileName = artistName
                                , artistProfileBio = Nothing
                                , artistProfileAvatarUrl = Nothing
                                , artistProfileGenres = Nothing
                                , artistProfileSocialLinks = Nothing
                                , artistProfileCountryCode = Nothing
                                , artistProfileCountryId = Nothing
                                , artistProfileCreatedAt = now
                                , artistProfileUpdatedAt = now
                                }
                    inserted <- insertUnique (ExternalArtistRef provider externalId artistId now)
                    pure $ maybe (Left (conflict "artist identity was created concurrently")) (const (Right artistId)) inserted
                _ -> pure (Left (conflict ("artist identity is ambiguous: " <> artistName)))

findMaterializationDuplicate
    :: EventResearchCandidate
    -> ValidatedMaterialization
    -> VenueId
    -> SqlPersistT IO (Either ServerError (Maybe SocialEventId))
findMaterializationDuplicate candidate validated venueId = do
    events <-
        selectList
            [ SocialEventVenueId ==. Just venueId
            , SocialEventStartTime >=. addUTCTime (-900) validated.vmStartTime
            , SocialEventStartTime <=. addUTCTime 900 validated.vmStartTime
            ]
            []
    matches <- filterM matchesCandidate events
    pure $ case matches of
        [] -> Right Nothing
        [Entity eventId _] -> Right (Just eventId)
        _ -> Left (conflict "event identity is ambiguous")
  where
    lineupNames = map normalizeEntityText validated.vmLineup
    matchesCandidate (Entity eventId event) =
        if normalizeEntityText (socialEventTitle event) /= normalizeEntityText (eventResearchCandidateTitle candidate)
            || abs (diffUTCTime (socialEventStartTime event) validated.vmStartTime) > 900
            then pure False
            else do
                links <- selectList [EventArtistEventId ==. eventId] []
                names <-
                    mapMaybeM
                        (\link -> fmap artistProfileName <$> get (eventArtistArtistId (entityVal link)))
                        links
                pure (null names || any (`elem` lineupNames) (map normalizeEntityText names))

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM action values = mapMaybe id <$> traverse action values

insertMaterializationEventRef
    :: EventResearchCandidate
    -> ValidatedMaterialization
    -> SocialEventId
    -> UTCTime
    -> SqlPersistT IO Bool
insertMaterializationEventRef candidate validated eventId now =
    isJust
        <$> insertUnique
            ExternalEventRef
                { externalEventRefProvider = eventResearchCandidateProvider candidate
                , externalEventRefExternalId = eventResearchCandidateExternalId candidate
                , externalEventRefEventId = eventId
                , externalEventRefCity = validated.vmCity
                , externalEventRefCountryCode = Just (eventResearchCandidateCountryCode candidate)
                , externalEventRefSourceUrl = eventResearchCandidatePurchaseUrl candidate <|> Just (eventResearchCandidateSourceUrl candidate)
                , externalEventRefPriceCents = Nothing
                , externalEventRefCurrency = validated.vmCurrency
                , externalEventRefLastSeenAt = now
                , externalEventRefMissingRuns = 0
                , externalEventRefSourceStatus = validated.vmSourceStatus
                }

materializationEventMetadata
    :: EventResearchCandidate
    -> EventResearchMaterializationRequestDTO
    -> ValidatedMaterialization
    -> T.Text
materializationEventMetadata candidate request validated =
    encodeJson . Aeson.object $
        [ "ticketUrl" Aeson..= eventResearchCandidatePurchaseUrl candidate
        , "imageUrl" Aeson..= validated.vmImageUrl
        , "isPublic" Aeson..= request.erMaterializationPublish
        , "currency" Aeson..= validated.vmCurrency
        , "budgetCents" Aeson..= (Nothing :: Maybe Int)
        ]

existingEventCanSatisfy
    :: EventResearchMaterializationRequestDTO
    -> SocialEventId
    -> SqlPersistT IO Bool
existingEventCanSatisfy request eventId
    | not request.erMaterializationPublish = isJust <$> get eventId
    | otherwise = materializedEventIsPublished eventId

materializedEventIsPublished :: SocialEventId -> SqlPersistT IO Bool
materializedEventIsPublished eventId = do
    event <- get eventId
    case event of
        Nothing -> pure False
        Just row -> case socialEventWorkflowStateId row of
            Nothing -> pure False
            Just workflowStateId -> do
                listable <- EventLifecycle.socialEventStateHasCapability workflowStateId "public-listable"
                pure (listable && storedEventIsPublic (socialEventMetadata row))

storedEventIsPublic :: Maybe T.Text -> Bool
storedEventIsPublic Nothing = True
storedEventIsPublic (Just raw) =
    case Aeson.eitherDecodeStrict' (TE.encodeUtf8 raw) of
        Right (Aeson.Object object) ->
            case parseMaybe (\o -> o Aeson..:? "isPublic" Aeson..!= True) object of
                Just value -> value
                Nothing -> False
        _ -> False

linkCandidateAndRespond
    :: Entity EventResearchCandidate
    -> EventResearchRunId
    -> SocialEventId
    -> Bool
    -> UTCTime
    -> SqlPersistT IO (Either ServerError EventResearchMaterializationDTO)
linkCandidateAndRespond beforeEntity@(Entity candidateId _) materializationRunId eventId created now = do
    event <- get eventId
    case event of
        Nothing -> pure (Left (storedDataError "materialized event does not exist"))
        Just eventRow -> case socialEventVenueId eventRow of
            Nothing -> pure (Left (conflict "materialized event has no venue"))
            Just venueId -> do
                when (eventResearchCandidateEventId (entityVal beforeEntity) /= Just eventId) $
                    update candidateId [EventResearchCandidateEventId =. Just eventId, EventResearchCandidateUpdatedAt =. now]
                afterRow <- getJust candidateId
                published <- materializedEventIsPublished eventId
                changeResult <- ensureMaterializationChange beforeEntity (Entity candidateId afterRow) materializationRunId eventId published now
                case changeResult of
                    Left serverError -> pure (Left serverError)
                    Right (changeId, auditRunId) -> do
                        artistLinks <- selectList [EventArtistEventId ==. eventId] [Asc EventArtistArtistId]
                        pure . Right $
                            EventResearchMaterializationDTO
                                { erMaterializationRunId = renderKey auditRunId
                                , erMaterializationCandidateId = renderKey candidateId
                                , erMaterializationEventId = renderKey eventId
                                , erMaterializationVenueId = renderKey venueId
                                , erMaterializationArtistIds = map (renderKey . eventArtistArtistId . entityVal) artistLinks
                                , erMaterializationChangeId = renderKey changeId
                                , erMaterializationCreated = created
                                , erMaterializationPublished = published
                                }

ensureMaterializationChange
    :: Entity EventResearchCandidate
    -> Entity EventResearchCandidate
    -> EventResearchRunId
    -> SocialEventId
    -> Bool
    -> UTCTime
    -> SqlPersistT IO (Either ServerError (EventResearchChangeId, EventResearchRunId))
ensureMaterializationChange beforeEntity@(Entity candidateId candidate) afterEntity materializationRunId eventId published now =
    case (candidateEntityToDTO beforeEntity, candidateEntityToDTO afterEntity) of
        (Right beforeDTO, Right afterDTO) -> do
            let dedupeKey = eventResearchMaterializationDedupeKey candidateId eventId
            existing <- getBy (UniqueEventResearchChange dedupeKey)
            case existing of
                Just (Entity changeId change) ->
                    pure (Right (changeId, eventResearchChangeRunId change))
                Nothing -> do
                    runResult <- materializationRunCanWrite materializationRunId
                    case runResult of
                        Left serverError -> pure (Left serverError)
                        Right () -> do
                            inserted <-
                                insertUnique
                                    EventResearchChange
                                        { eventResearchChangeRunId = materializationRunId
                                        , eventResearchChangeCandidateId = Just candidateId
                                        , eventResearchChangeEventId = Just eventId
                                        , eventResearchChangeAction = "materialized"
                                        , eventResearchChangeBeforeValue = Just (encodeJson beforeDTO)
                                        , eventResearchChangeAfterValue = Just (encodeJson afterDTO)
                                        , eventResearchChangeSourceUrl = eventResearchCandidateSourceUrl candidate
                                        , eventResearchChangeConfidence = eventResearchCandidateConfidence candidate
                                        , eventResearchChangeConsultedAt = eventResearchCandidateVerifiedAt candidate
                                        , eventResearchChangeExternalId = eventResearchCandidateExternalId candidate
                                        , eventResearchChangeResult = if published then "published" else "materialized"
                                        , eventResearchChangeDedupeKey = dedupeKey
                                        , eventResearchChangeCreatedAt = now
                                        }
                            case inserted of
                                Just changeId -> pure (Right (changeId, materializationRunId))
                                Nothing -> do
                                    raced <- getBy (UniqueEventResearchChange dedupeKey)
                                    pure $
                                        maybe
                                            (Left (storedDataError "materialization audit could not be persisted"))
                                            (\(Entity racedId racedChange) -> Right (racedId, eventResearchChangeRunId racedChange))
                                            raced
        (Left message, _) -> pure (Left (storedDataError message))
        (_, Left message) -> pure (Left (storedDataError message))

eventResearchMaterializationDedupeKey :: EventResearchCandidateId -> SocialEventId -> T.Text
eventResearchMaterializationDedupeKey candidateId eventId =
    sha256Text . BL.fromStrict . TE.encodeUtf8 $
        T.intercalate ":" ["materialized", renderKey candidateId, renderKey eventId]

researchEntityExternalId :: T.Text -> [T.Text] -> T.Text
researchEntityExternalId entityType parts =
    "event-research:" <> entityType <> ":"
        <> T.take 40 (sha256Text (BL.fromStrict (TE.encodeUtf8 (T.intercalate "|" (map normalizeEntityText parts)))))

conflict :: T.Text -> ServerError
conflict message = err409{errBody = BL.fromStrict (TE.encodeUtf8 message)}

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
