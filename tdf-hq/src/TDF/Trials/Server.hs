{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module TDF.Trials.Server where

import           Control.Exception      (throwIO)
import           Control.Monad          (forM, unless, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
import           Data.Char              (isAlphaNum, isDigit, isSpace)
import           Data.Maybe             (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import           Data.List              (foldl')
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (UTCTime, diffUTCTime, getCurrentTime)
import           Web.PathPieces         (fromPathPiece, toPathPiece)

import           Network.Wai                     (Request)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler)

import           Database.Persist.Sql hiding (loadConfig)

import           TDF.Auth             (AuthedUser(..), ModuleAccess(..), hasModuleAccess)
import           TDF.Config          (loadConfig)
import           TDF.Models          ( Party(..)
                                      , PartyId
                                      , ResourceId
                                      , partyDisplayName
                                      , RoleEnum(..)
                                      , PartyRole(..)
                                      )
import qualified TDF.Models          as Models
import qualified TDF.Email           as Email
import qualified TDF.Email.Service   as EmailSvc
import           TDF.Trials.API
import           TDF.Trials.DTO
import           TDF.Trials.Models
import qualified TDF.Trials.Models      as Trials
import           Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)

type AppM = SqlPersistT IO

statusRequested, statusAssigned, statusScheduled :: Text
statusRequested = "Requested"
statusAssigned  = "Assigned"
statusScheduled = "Scheduled"

entityKeyInt :: ToBackendKey SqlBackend record => Key record -> Int
entityKeyInt = fromIntegral . fromSqlKey

intKey :: ToBackendKey SqlBackend record => Int -> Key record
intKey i = toSqlKey (fromIntegral i :: Int64)

maybeKey :: ToBackendKey SqlBackend record => Maybe Int -> Maybe (Key record)
maybeKey = fmap intKey

cleanOptional :: Maybe Text -> Maybe Text
cleanOptional = (>>= (\txt -> let t = T.strip txt in if T.null t then Nothing else Just t))

normalizePhone :: Text -> Maybe Text
normalizePhone raw =
  let trimmed = T.filter (not . isSpace) (T.strip raw)
      digits = T.filter (\c -> isDigit c || c == '+') trimmed
      withoutPlus = T.dropWhile (== '+') digits
      onlyDigits = T.filter isDigit withoutPlus
  in if T.null onlyDigits then Nothing else Just ("+" <> onlyDigits)

slugify :: Text -> Text
slugify =
  T.take 60 . T.filter (\c -> isAlphaNum c || c `elem` ("._-" :: String)) . T.toLower . T.strip

deriveBaseUsername :: Maybe Text -> Text -> Text
deriveBaseUsername mName emailAddr =
  let emailLocal = T.takeWhile (/= '@') emailAddr
      candidate = fromMaybe emailLocal (slugify <$> mName)
  in if T.null candidate then emailLocal else candidate

generateUniqueUsername :: Text -> PartyId -> SqlPersistT IO Text
generateUniqueUsername base partyId = go (0 :: Int)
  where
    baseClean = T.take 60 (T.filter (\c -> isAlphaNum c || c `elem` (".-_" :: String)) (T.toLower (T.strip base)))
    fallback = "tdf-user-" <> T.pack (show (fromSqlKey partyId))
    root = if T.null baseClean then fallback else baseClean
    go attempt = do
      let suffix = if attempt == 0 then "" else "-" <> T.pack (show attempt)
          candidate = T.take 60 (root <> suffix)
      conflict <- getBy (Models.UniqueCredentialUsername candidate)
      case conflict of
        Nothing -> pure candidate
        Just _  -> go (attempt + 1)

hashPasswordText :: Text -> IO Text
hashPasswordText pwd = do
  let raw = TE.encodeUtf8 pwd
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
  case mHash of
    Nothing   -> fail "Failed to hash password"
    Just hash -> pure (TE.decodeUtf8 hash)

preferredSlotsFrom :: TrialRequest -> [PreferredSlot]
preferredSlotsFrom req =
  PreferredSlot (trialRequestPref1Start req) (trialRequestPref1End req)
    : catMaybes
        [ mkSlot (trialRequestPref2Start req) (trialRequestPref2End req)
        , mkSlot (trialRequestPref3Start req) (trialRequestPref3End req)
        ]
  where
    mkSlot (Just s) (Just e) = Just (PreferredSlot s e)
    mkSlot _        _        = Nothing

trialRequestToQueueItem
  :: Map.Map SubjectId Text
  -> Map.Map PartyId Party
  -> Entity TrialRequest
  -> TrialQueueItem
trialRequestToQueueItem subjectMap partyMap (Entity rid req) =
  TrialQueueItem
    { requestId   = entityKeyInt rid
    , studentId   = Just (entityKeyInt (trialRequestPartyId req))
    , studentName = fmap partyDisplayName (Map.lookup (trialRequestPartyId req) partyMap)
    , subjectId   = entityKeyInt (trialRequestSubjectId req)
    , subjectName = Map.lookup (trialRequestSubjectId req) subjectMap
    , status      = trialRequestStatus req
    , preferred   = preferredSlotsFrom req
    , createdAt   = Just (trialRequestCreatedAt req)
    , notes       = trialRequestNotes req
    }

trialRequestOut :: Key TrialRequest -> TrialRequest -> TrialRequestOut
trialRequestOut rid req =
  TrialRequestOut
    { requestId = entityKeyInt rid
    , status    = trialRequestStatus req
    }

subjectEntityToDTO :: Map.Map SubjectId [Text] -> Entity Subject -> SubjectDTO
subjectEntityToDTO roomMap (Entity sid subj) =
  SubjectDTO
    { subjectId = entityKeyInt sid
    , name      = Trials.subjectName subj
    , active    = Trials.subjectActive subj
    , roomIds   = Map.findWithDefault [] sid roomMap
    }

listSubjects :: Bool -> AppM [SubjectDTO]
listSubjects includeInactive = do
  let filters = if includeInactive then [] else [SubjectActive ==. True]
  entities <- selectList filters [Asc SubjectName]
  roomMap <- subjectRoomMap (map entityKey entities)
  pure (map (subjectEntityToDTO roomMap) entities)

subjectRoomMap :: [SubjectId] -> AppM (Map.Map SubjectId [Text])
subjectRoomMap [] = pure Map.empty
subjectRoomMap subjectIds = do
  prefs <- selectList [SubjectRoomPreferenceSubjectId <-. subjectIds] [Asc SubjectRoomPreferencePriority, Asc SubjectRoomPreferenceRoomId]
  pure $
    foldl'
      (\acc (Entity _ pref) ->
          Map.insertWith (++) (subjectRoomPreferenceSubjectId pref)
            [toPathPiece (subjectRoomPreferenceRoomId pref)]
            acc
      )
      Map.empty
      prefs

listActiveSubjects :: AppM [SubjectDTO]
listActiveSubjects = listSubjects False

trialSlotsForSubject :: Maybe Int -> AppM [TrialSlotDTO]
trialSlotsForSubject Nothing = pure []
trialSlotsForSubject (Just sidInt) = do
  now <- liftIO getCurrentTime
  let subjectKey = intKey sidInt :: Trials.SubjectId
  availabilities <- selectList
    [ TeacherAvailabilitySubjectId ==. subjectKey
    , TeacherAvailabilityEndAt >=. now
    ] [Asc TeacherAvailabilityStartAt]
  validAvailabilities <- fmap catMaybes $
    forM availabilities $ \entity@(Entity _ availability) -> do
      let teacherKey = Trials.teacherAvailabilityTeacherId availability
          slotStart  = Trials.teacherAvailabilityStartAt availability
          slotEnd    = Trials.teacherAvailabilityEndAt availability
      isFree <- teacherAvailable teacherKey slotStart slotEnd
      pure $ if isFree then Just entity else Nothing
  if null validAvailabilities
    then pure []
    else do
      let grouped = Map.fromListWith (++)
            [ (Trials.teacherAvailabilityTeacherId avail, [PreferredSlot (Trials.teacherAvailabilityStartAt avail) (Trials.teacherAvailabilityEndAt avail)])
            | Entity _ avail <- validAvailabilities
            ]
          teacherIds = Map.keys grouped
      teacherEntities <- selectList [Models.PartyId <-. teacherIds] []
      let nameMap = Map.fromList
            [ (entityKey ent, partyDisplayName (entityVal ent))
            | ent <- teacherEntities
            ]
      pure
        [ TrialSlotDTO
            { subjectId   = sidInt
            , teacherId   = entityKeyInt teacherKey
            , teacherName = fromMaybe "Profesor disponible" (Map.lookup teacherKey nameMap)
            , slots       = slots
            }
        | (teacherKey, slots) <- Map.toList grouped
        ]

teacherAvailable :: PartyId -> UTCTime -> UTCTime -> AppM Bool
teacherAvailable teacherId slotStart slotEnd = do
  hasTrialConflict <- recordExists [ TrialAssignmentTeacherId ==. teacherId
                                   , TrialAssignmentStartAt <. slotEnd
                                   , TrialAssignmentEndAt   >. slotStart
                                   ]
  hasClassConflict <- recordExists [ ClassSessionTeacherId ==. teacherId
                                   , ClassSessionStartAt <. slotEnd
                                   , ClassSessionEndAt   >. slotStart
                                   ]
  pure (not (hasTrialConflict || hasClassConflict))

roomAvailable :: ResourceId -> UTCTime -> UTCTime -> AppM Bool
roomAvailable roomId slotStart slotEnd = do
  hasClassConflict <- recordExists [ ClassSessionRoomId ==. roomId
                                   , ClassSessionStartAt <. slotEnd
                                   , ClassSessionEndAt   >. slotStart
                                   ]
  -- check bookings attached to the room resource
  bookingRes <- selectList [Models.BookingResourceResourceId ==. roomId] []
  let bookingIds = map (Models.bookingResourceBookingId . entityVal) bookingRes
  bookings <- if null bookingIds
    then pure []
    else selectList [ Models.BookingId <-. bookingIds
                    , Models.BookingStartsAt <. slotEnd
                    , Models.BookingEndsAt   >. slotStart
                    , Models.BookingStatus /<-. [Models.Cancelled, Models.NoShow]
                    ] []
  let hasBookingConflict = not (null bookings)
  pure (not (hasClassConflict || hasBookingConflict))

recordExists
  :: ( PersistEntity record
     , PersistEntityBackend record ~ SqlBackend
     )
  => [Filter record]
  -> AppM Bool
recordExists filters = do
  mEntity <- selectFirst filters []
  pure (maybe False (const True) mEntity)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f = fmap or . mapM f

publicTrialsServer :: ServerT PublicTrialsAPI AppM
publicTrialsServer =
  signupH
    :<|> interestH
    :<|> trialRequestCreateH
    :<|> publicSubjectsH
    :<|> publicSlotsH
  where
    signupH :: SignupIn -> AppM SignupOut
    signupH SignupIn{..} = do
      now <- liftIO getCurrentTime
      let partyIdKey = intKey 0 -- placeholder, until a full signup flow is implemented
      _ <- insert $ LeadInterest
        { leadInterestPartyId   = partyIdKey
        , leadInterestInterestType = "signup"
        , leadInterestSubjectId = Nothing
        , leadInterestDetails   = Just (T.intercalate " | " (filter (not . T.null)
            [ firstName <> " " <> lastName
            , email
            , maybe "" id phone
            ]))
        , leadInterestSource    = "public_signup"
        , leadInterestDriveLink = Nothing
        , leadInterestStatus    = "Open"
        , leadInterestCreatedAt = now
        }
      pure (SignupOut True)

    interestH :: InterestIn -> AppM InterestOut
    interestH InterestIn{..} = do
      now <- liftIO getCurrentTime
      let partyIdKey = intKey 0
      let subjectKey = maybeKey subjectId
      key <- insert LeadInterest
        { leadInterestPartyId   = partyIdKey
        , leadInterestInterestType = interestType
        , leadInterestSubjectId = subjectKey
        , leadInterestDetails   = details
        , leadInterestSource    = "public_interest"
        , leadInterestDriveLink = driveLink
        , leadInterestStatus    = "Open"
        , leadInterestCreatedAt = now
        }
      pure (InterestOut (entityKeyInt key))

    trialRequestCreateH :: TrialRequestIn -> AppM TrialRequestOut
    trialRequestCreateH TrialRequestIn{..} = do
      now <- liftIO getCurrentTime
      let nameClean  = cleanOptional fullName
          emailClean = cleanOptional email
          phoneClean = cleanOptional phone

      resolvedPartyId <- case partyId of
        Just pid -> pure (intKey pid)
        Nothing  -> createOrFetchParty nameClean emailClean phoneClean now

      mNewCred <- case emailClean of
        Nothing -> pure Nothing
        Just addr -> ensureUserAccountForParty resolvedPartyId nameClean addr

      -- Send welcome email only when we created a credential.
      case (mNewCred, emailClean) of
        (Just (username, password), Just addr) -> liftIO $ do
          cfg <- loadConfig
          let svc = EmailSvc.mkEmailService cfg
              display = fromMaybe addr nameClean
          EmailSvc.sendWelcome svc display addr username password
        _ -> pure ()
      case preferred of
        [] -> liftIO $ throwIO err400 { errBody = "Need at least one preferred slot" }
        (slot1@(PreferredSlot firstStart firstEnd) : rest) -> do
          let slots = take 3 (slot1 : rest)
              pref2 = listToMaybe rest
              pref3 = listToMaybe (drop 1 rest)
              (pref2Start, pref2End) = slotBounds pref2
              (pref3Start, pref3End) = slotBounds pref3
              partyKey = resolvedPartyId
              subjectKey = intKey subjectId
          ensureSubjectAvailability subjectKey slots
          rid <- insert TrialRequest
            { trialRequestPartyId           = partyKey
            , trialRequestSubjectId         = subjectKey
            , trialRequestPref1Start        = firstStart
            , trialRequestPref1End          = firstEnd
            , trialRequestPref2Start        = pref2Start
            , trialRequestPref2End          = pref2End
            , trialRequestPref3Start        = pref3Start
            , trialRequestPref3End          = pref3End
            , trialRequestNotes             = notes
            , trialRequestStatus            = statusRequested
            , trialRequestAssignedTeacherId = Nothing
            , trialRequestAssignedAt        = Nothing
            , trialRequestCreatedAt         = now
            }
          pure (TrialRequestOut (entityKeyInt rid) statusRequested)

    slotBounds :: Maybe PreferredSlot -> (Maybe UTCTime, Maybe UTCTime)
    slotBounds = maybe (Nothing, Nothing) $ \(PreferredSlot s e) -> (Just s, Just e)

    publicSubjectsH :: AppM [SubjectDTO]
    publicSubjectsH = listActiveSubjects

    publicSlotsH :: Maybe Int -> AppM [TrialSlotDTO]
    publicSlotsH = trialSlotsForSubject

    ensureSubjectAvailability :: Trials.SubjectId -> [PreferredSlot] -> AppM ()
    ensureSubjectAvailability subjectKey slots = do
      teacherLinks <- selectList [TeacherSubjectSubjectId ==. subjectKey] []
      whenNoTeachers teacherLinks
      let teacherIds = map (Trials.teacherSubjectTeacherId . entityVal) teacherLinks
      mapM_ (ensureSlotHasTeacher teacherIds) slots
      where
        whenNoTeachers [] = liftIO $ throwIO err422 { errBody = "No hay profesores disponibles para esta materia" }
        whenNoTeachers _  = pure ()

    ensureSlotHasTeacher teacherIds (PreferredSlot slotStart slotEnd) = do
      available <- anyM (\teacherId -> teacherAvailable teacherId slotStart slotEnd) teacherIds
      unless available $ liftIO $ throwIO err422 { errBody = "No hay profesores disponibles en el horario solicitado" }

createOrFetchParty :: Maybe Text -> Maybe Text -> Maybe Text -> UTCTime -> AppM PartyId
createOrFetchParty mName mEmail mPhone now = do
  emailVal <- case cleanOptional mEmail of
    Nothing -> liftIO $ throwIO err400 { errBody = "Correo requerido para crear la cuenta" }
    Just e  -> pure e
  let phoneVal = mPhone >>= normalizePhone
      display = fromMaybe emailVal mName
  mExisting <- selectFirst [Models.PartyPrimaryEmail ==. Just emailVal] []
  case mExisting of
    Just (Entity pid party) -> do
      let updates = catMaybes
            [ if isJust (partyPrimaryPhone party) || isNothing phoneVal then Nothing else Just (Models.PartyPrimaryPhone =. phoneVal)
            , if isJust (partyWhatsapp party) || isNothing phoneVal then Nothing else Just (Models.PartyWhatsapp =. phoneVal)
            , if T.strip (partyDisplayName party) == "" && not (T.null display) then Just (Models.PartyDisplayName =. display) else Nothing
            ]
      unless (null updates) $
        update pid updates
      pure pid
    Nothing -> insert Party
      { partyLegalName       = Nothing
      , partyDisplayName     = display
      , partyIsOrg           = False
      , partyTaxId           = Nothing
      , partyPrimaryEmail    = Just emailVal
      , partyPrimaryPhone    = phoneVal
      , partyWhatsapp        = phoneVal
      , partyInstagram       = Nothing
      , partyEmergencyContact = Nothing
      , partyNotes           = Nothing
      , partyCreatedAt       = now
      }

ensureUserAccountForParty :: PartyId -> Maybe Text -> Text -> AppM (Maybe (Text, Text))
ensureUserAccountForParty partyId mName emailVal = do
  mCred <- selectFirst [Models.UserCredentialPartyId ==. partyId] []
  case mCred of
    Just _ -> pure Nothing
    Nothing -> do
      username <- generateUniqueUsername (deriveBaseUsername mName emailVal) partyId
      tempPassword <- liftIO Email.generateTempPassword
      hashed <- liftIO (hashPasswordText tempPassword)
      _ <- insert Models.UserCredential
        { Models.userCredentialPartyId = partyId
        , Models.userCredentialUsername = username
        , Models.userCredentialPasswordHash = hashed
        , Models.userCredentialActive = True
        }
      void $ upsert (PartyRole partyId Customer True) [Models.PartyRoleActive =. True]
      void $ upsert (PartyRole partyId Fan True) [Models.PartyRoleActive =. True]
      pure (Just (username, tempPassword))

privateTrialsServer :: AuthedUser -> ServerT PrivateTrialsAPI AppM
privateTrialsServer user@AuthedUser{..} =
  queueH
    :<|> assignH
    :<|> scheduleH
    :<|> availabilityListH
    :<|> availabilityUpsertH
    :<|> availabilityDeleteH
    :<|> subjectsH
    :<|> createSubjectH
    :<|> updateSubjectH
    :<|> deleteSubjectH
    :<|> packagesH
    :<|> purchaseH
    :<|> classSessionsListH
    :<|> createClassH
    :<|> updateClassH
    :<|> attendH
    :<|> commissionsH
    :<|> teachersH
    :<|> teacherClassesH
    :<|> studentsListH
    :<|> studentCreateH
  where
    queueH :: Maybe Int -> Maybe Text -> AppM [TrialQueueItem]
    queueH mSubject mStatus = do
      let filters = catMaybes
            [ (TrialRequestSubjectId ==.) . intKey <$> mSubject
            , (TrialRequestStatus ==.) . T.strip <$> mStatus
            ]
      requests <- selectList filters [Desc TrialRequestCreatedAt]
      let subjectIds = map (trialRequestSubjectId . entityVal) requests
          partyIds   = map (trialRequestPartyId . entityVal) requests
      subjects <- if null subjectIds
        then pure Map.empty
        else do
          entities <- selectList [SubjectId <-. subjectIds] []
          pure $ Map.fromList [ (entityKey e, Trials.subjectName (entityVal e)) | e <- entities ]
      parties <- if null partyIds
        then pure Map.empty
        else do
          entities <- selectList [Models.PartyId <-. partyIds] []
          pure $ Map.fromList [ (entityKey e, entityVal e) | e <- entities ]
      pure (map (trialRequestToQueueItem subjects parties) requests)

    assignH :: Int -> TrialAssignIn -> AppM TrialRequestOut
    assignH requestId TrialAssignIn{..} = do
      let rid = intKey requestId :: Key TrialRequest
          teacherKey = intKey teacherId :: PartyId
      now <- liftIO getCurrentTime
      mReq <- get rid
      case mReq of
        Nothing  -> liftIO $ throwIO err404
        Just req -> do
          update rid
            [ TrialRequestAssignedTeacherId =. Just teacherKey
            , TrialRequestAssignedAt        =. Just now
            , TrialRequestStatus            =. statusAssigned
            ]
          pure (trialRequestOut rid req { trialRequestStatus = statusAssigned })

    scheduleH :: TrialScheduleIn -> AppM TrialRequestOut
    scheduleH TrialScheduleIn{..} = do
      let rid       = intKey requestId :: Key TrialRequest
          teacherK  = intKey teacherId :: PartyId
          roomK     = intKey roomId    :: ResourceId
      now <- liftIO getCurrentTime
      mReq <- get rid
      case mReq of
        Nothing  -> liftIO $ throwIO err404
        Just req -> do
          let assignment = TrialAssignment
                { trialAssignmentRequestId = rid
                , trialAssignmentTeacherId = teacherK
                , trialAssignmentStartAt   = startAt
                , trialAssignmentEndAt     = endAt
                , trialAssignmentRoomId    = roomK
                , trialAssignmentBookingId = Nothing
                , trialAssignmentCreatedAt = now
                }
          _ <- upsert assignment
            [ TrialAssignmentTeacherId =. teacherK
            , TrialAssignmentStartAt   =. startAt
            , TrialAssignmentEndAt     =. endAt
            , TrialAssignmentRoomId    =. roomK
            ]
          update rid
            [ TrialRequestAssignedTeacherId =. Just teacherK
            , TrialRequestAssignedAt        =. Just now
            , TrialRequestStatus            =. statusScheduled
            ]
          deleteWhere
            [ TeacherAvailabilityTeacherId ==. teacherK
            , TeacherAvailabilitySubjectId ==. trialRequestSubjectId req
            , TeacherAvailabilityStartAt ==. startAt
            , TeacherAvailabilityEndAt ==. endAt
            ]
          pure (trialRequestOut rid req { trialRequestStatus = statusScheduled })

    availabilityListH :: Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> AppM [TrialAvailabilitySlotDTO]
    availabilityListH mSubject mFrom mTo = do
      ensureModuleAccess ModuleScheduling
      let teacherKey = auPartyId
          filters =
            [ TeacherAvailabilityTeacherId ==. teacherKey ]
            ++ maybe [] (\sid -> [TeacherAvailabilitySubjectId ==. intKey sid]) mSubject
            ++ maybe [] (\fromTs -> [TeacherAvailabilityEndAt >=. fromTs]) mFrom
            ++ maybe [] (\toTs -> [TeacherAvailabilityStartAt <=. toTs]) mTo
      records <- selectList filters [Asc TeacherAvailabilityStartAt]
      let subjects = map (Trials.teacherAvailabilitySubjectId . entityVal) records
          teachers = map (Trials.teacherAvailabilityTeacherId . entityVal) records
          rooms    = map (Trials.teacherAvailabilityRoomId . entityVal) records
      subjectMap <- loadSubjectNames subjects
      teacherMap <- loadTeacherNames teachers
      roomMap    <- loadRoomNames rooms
      pure (map (availabilityEntityToDTO subjectMap teacherMap roomMap) records)

    availabilityUpsertH :: TrialAvailabilityUpsert -> AppM TrialAvailabilitySlotDTO
    availabilityUpsertH TrialAvailabilityUpsert{..} = do
      ensureModuleAccess ModuleScheduling
      when (startAt >= endAt) $
        liftIO $ throwIO err400 { errBody = "La hora de fin debe ser posterior al inicio." }
      teacherKey <- resolveTeacherKey teacherId
      let isSelf = teacherKey == auPartyId
      subjectKey <- ensureSubjectExists subjectId
      roomKey <- parseRoomKey roomId
      ensureRoomAllowed subjectKey roomKey
      when (isSelf && not (hasModuleAccess ModuleAdmin user)) $
        ensureTeacherSubject teacherKey subjectKey
      isFree <- teacherAvailable teacherKey startAt endAt
      unless isFree $
        liftIO $ throwIO err409 { errBody = "Ya tienes una clase o prueba en ese horario." }
      let overlapFilters =
            [ TeacherAvailabilityTeacherId ==. teacherKey
            , TeacherAvailabilityStartAt <. endAt
            , TeacherAvailabilityEndAt   >. startAt
            ] ++ maybe [] (\aid -> [TeacherAvailabilityId !=. intKey aid]) availabilityId
      hasOverlap <- recordExists overlapFilters
      when hasOverlap $
        liftIO $ throwIO err409 { errBody = "Ya publicaste disponibilidad en ese horario." }
      now <- liftIO getCurrentTime
      entity <- case availabilityId of
        Nothing -> do
          newId <- insert TeacherAvailability
            { teacherAvailabilityTeacherId = teacherKey
            , teacherAvailabilitySubjectId = subjectKey
            , teacherAvailabilityRoomId    = roomKey
            , teacherAvailabilityStartAt   = startAt
            , teacherAvailabilityEndAt     = endAt
            , teacherAvailabilityNotes     = notes
            , teacherAvailabilityCreatedAt = now
            }
          getJustEntity newId
        Just aid -> do
          let availabilityKey = intKey aid :: Key TeacherAvailability
          mExisting <- get availabilityKey
          case mExisting of
            Nothing -> liftIO $ throwIO err404
            Just existing -> do
              unless (teacherAvailabilityTeacherId existing == teacherKey || hasModuleAccess ModuleAdmin user) $
                liftIO $ throwIO err403
              update availabilityKey
                [ TeacherAvailabilityTeacherId =. teacherKey
                , TeacherAvailabilitySubjectId =. subjectKey
                , TeacherAvailabilityRoomId    =. roomKey
                , TeacherAvailabilityStartAt   =. startAt
                , TeacherAvailabilityEndAt     =. endAt
                , TeacherAvailabilityNotes     =. notes
                ]
              getJustEntity availabilityKey
      let subjectIds = [Trials.teacherAvailabilitySubjectId (entityVal entity)]
          teacherIds = [Trials.teacherAvailabilityTeacherId (entityVal entity)]
          roomIds    = [Trials.teacherAvailabilityRoomId (entityVal entity)]
      subjectMap <- loadSubjectNames subjectIds
      teacherMap <- loadTeacherNames teacherIds
      roomMap    <- loadRoomNames roomIds
      pure (availabilityEntityToDTO subjectMap teacherMap roomMap entity)

    availabilityDeleteH :: Int -> AppM NoContent
    availabilityDeleteH availabilityIdInt = do
      ensureModuleAccess ModuleScheduling
      let availabilityKey = intKey availabilityIdInt :: Key TeacherAvailability
      mEntity <- get availabilityKey
      case mEntity of
        Nothing -> liftIO $ throwIO err404
        Just row -> do
          let owner = teacherAvailabilityTeacherId row
          unless (owner == auPartyId || hasModuleAccess ModuleAdmin user) $
            liftIO $ throwIO err403
          delete availabilityKey
          pure NoContent

    subjectsH :: Maybe Bool -> AppM [SubjectDTO]
    subjectsH includeInactive = do
      ensureModuleAccess ModuleScheduling
      listSubjects (fromMaybe False includeInactive)

    createSubjectH :: SubjectCreate -> AppM SubjectDTO
    createSubjectH SubjectCreate{..} = do
      ensureModuleAccess ModuleAdmin
      let trimmed = T.strip name
      when (T.null trimmed) $ liftIO $ throwIO err400 { errBody = "El nombre es obligatorio" }
      let isActive = fromMaybe True active
          entityVal = Subject
            { subjectName   = trimmed
            , subjectActive = isActive
            }
      sid <- insert entityVal
      roomMap <- subjectRoomMap [sid]
      pure (subjectEntityToDTO roomMap (Entity sid entityVal))

    updateSubjectH :: Int -> SubjectUpdate -> AppM SubjectDTO
    updateSubjectH subjectIdInt SubjectUpdate{..} = do
      ensureModuleAccess ModuleAdmin
      let sid = intKey subjectIdInt :: Key Subject
      when (maybe False (T.null . T.strip) name) $ liftIO $ throwIO err400 { errBody = "El nombre es obligatorio" }
      mSubject <- get sid
      case mSubject of
        Nothing -> liftIO $ throwIO err404
        Just _ -> do
          let trimmed = T.strip <$> name
              updates = catMaybes
                [ (SubjectName =.) <$> trimmed
                , (SubjectActive =.) <$> active
                ]
          unless (null updates) $ update sid updates
          fresh <- get sid
          case fresh of
            Nothing       -> liftIO $ throwIO err404
            Just newSubj  -> do
              roomMap <- subjectRoomMap [sid]
              pure (subjectEntityToDTO roomMap (Entity sid newSubj))

    deleteSubjectH :: Int -> AppM NoContent
    deleteSubjectH subjectIdInt = do
      ensureModuleAccess ModuleAdmin
      let sid = intKey subjectIdInt :: Key Subject
      mSubj <- get sid
      case mSubj of
        Nothing -> liftIO $ throwIO err404
        Just _  -> do
          update sid [SubjectActive =. False]
          pure NoContent

    ensureModuleAccess :: ModuleAccess -> AppM ()
    ensureModuleAccess tag =
      unless (hasModuleAccess tag user) $
        liftIO $ throwIO err403

    resolveTeacherKey :: Maybe Int -> AppM PartyId
    resolveTeacherKey Nothing = pure auPartyId
    resolveTeacherKey (Just tid) = do
      let key = intKey tid :: PartyId
      unless (key == auPartyId || hasModuleAccess ModuleAdmin user) $
        liftIO $ throwIO err403
      pure key

    ensureSubjectExists :: Int -> AppM SubjectId
    ensureSubjectExists sidInt = do
      let key = intKey sidInt :: SubjectId
      mSubject <- get key
      case mSubject of
        Nothing -> liftIO $ throwIO err404
        Just _  -> pure key

    parseRoomKey :: Text -> AppM ResourceId
    parseRoomKey raw =
      case fromPathPiece raw of
        Nothing -> liftIO $ throwIO err400 { errBody = "Identificador de sala inválido." }
        Just key -> do
          mRoom <- get key
          case mRoom of
            Nothing -> liftIO $ throwIO err404
            Just _  -> pure key

    ensureRoomAllowed :: SubjectId -> ResourceId -> AppM ()
    ensureRoomAllowed subjectKey roomKey = do
      prefs <- selectList [SubjectRoomPreferenceSubjectId ==. subjectKey] []
      case prefs of
        [] -> pure () -- no restriction configured
        _  -> do
          let allowed = any (\(Entity _ pref) -> subjectRoomPreferenceRoomId pref == roomKey) prefs
          unless allowed $
            liftIO $ throwIO err422 { errBody = "Esta materia no se dicta en la sala seleccionada." }

    ensureTeacherSubject :: PartyId -> SubjectId -> AppM ()
    ensureTeacherSubject teacherKey subjectKey = do
      linked <- recordExists [ TeacherSubjectTeacherId ==. teacherKey
                             , TeacherSubjectSubjectId ==. subjectKey
                             ]
      unless linked $
        liftIO $ throwIO err422 { errBody = "No estás asignado a esta materia." }

    loadSubjectNames :: [SubjectId] -> AppM (Map.Map SubjectId Text)
    loadSubjectNames ids =
      if null ids
        then pure Map.empty
        else do
          entities <- selectList [SubjectId <-. distinct ids] []
          pure $ Map.fromList [ (entityKey e, Trials.subjectName (entityVal e)) | e <- entities ]

    loadTeacherNames :: [PartyId] -> AppM (Map.Map PartyId Text)
    loadTeacherNames ids =
      if null ids
        then pure Map.empty
        else do
          entities <- selectList [Models.PartyId <-. distinct ids] []
          pure $ Map.fromList [ (entityKey e, partyDisplayName (entityVal e)) | e <- entities ]

    loadRoomNames :: [ResourceId] -> AppM (Map.Map ResourceId Text)
    loadRoomNames ids =
      if null ids
        then pure Map.empty
        else do
          entities <- selectList [Models.ResourceId <-. distinct ids] []
          pure $ Map.fromList [ (entityKey e, Models.resourceName (entityVal e)) | e <- entities ]

    availabilityEntityToDTO
      :: Map.Map SubjectId Text
      -> Map.Map PartyId Text
      -> Map.Map ResourceId Text
      -> Entity TeacherAvailability
      -> TrialAvailabilitySlotDTO
    availabilityEntityToDTO subjectMap teacherMap roomMap (Entity aid availability) =
      TrialAvailabilitySlotDTO
        { availabilityId = entityKeyInt aid
        , subjectId      = entityKeyInt (Trials.teacherAvailabilitySubjectId availability)
        , subjectName    = Map.lookup (Trials.teacherAvailabilitySubjectId availability) subjectMap
        , teacherId      = entityKeyInt (Trials.teacherAvailabilityTeacherId availability)
        , teacherName    = Map.lookup (Trials.teacherAvailabilityTeacherId availability) teacherMap
        , roomId         = toPathPiece (Trials.teacherAvailabilityRoomId availability)
        , roomName       = Map.lookup (Trials.teacherAvailabilityRoomId availability) roomMap
        , startAt        = Trials.teacherAvailabilityStartAt availability
        , endAt          = Trials.teacherAvailabilityEndAt availability
        , notes          = Trials.teacherAvailabilityNotes availability
        }

    distinct :: (Ord a) => [a] -> [a]
    distinct = Set.toList . Set.fromList

    packagesH :: Maybe Int -> AppM [PackageDTO]
    packagesH mSubject = do
      let filters = [PackageCatalogActive ==. True] ++ maybe [] (\sid -> [PackageCatalogSubjectId ==. intKey sid]) mSubject
      entities <- selectList filters [Asc PackageCatalogName]
      pure [ PackageDTO
              { packageId   = entityKeyInt pid
              , name        = packageCatalogName pkg
              , hoursQty    = packageCatalogHoursQty pkg
              , priceCents  = packageCatalogPriceCents pkg
              , expiresDays = packageCatalogExpiresDays pkg
              }
           | Entity pid pkg <- entities
           ]

    purchaseH :: PurchaseIn -> AppM PurchaseOut
    purchaseH PurchaseIn{..} = do
      now <- liftIO getCurrentTime
      let studentKey = intKey studentId
          packageKey = intKey packageId
          sellerKey  = maybeKey sellerId
          commissionKey = maybeKey commissionedTeacherId
          trialKey   = maybeKey trialRequestId
          discount   = fromMaybe 0 discountCents
          tax        = fromMaybe 0 taxCents
          total      = priceCents - discount + tax
      pid <- insert ClassPackagePurchase
        { classPackagePurchaseStudentId            = studentKey
        , classPackagePurchasePackageId            = packageKey
        , classPackagePurchasePriceCents           = priceCents
        , classPackagePurchaseDiscountCents        = discount
        , classPackagePurchaseTaxCents             = tax
        , classPackagePurchaseTotalPaidCents       = total
        , classPackagePurchasePurchasedAt          = now
        , classPackagePurchaseSellerId             = sellerKey
        , classPackagePurchaseCommissionedTeacherId = commissionKey
        , classPackagePurchaseTrialRequestId       = trialKey
        , classPackagePurchaseStatus               = "Open"
        }
      pure (PurchaseOut (entityKeyInt pid))

    classSessionsListH :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> AppM [ClassSessionDTO]
    classSessionsListH mSubject mTeacher mStudent mFrom mTo mStatus = do
      let filters =
            maybe [] (\sid -> [ClassSessionSubjectId ==. intKey sid]) mSubject
            ++ maybe [] (\tid -> [ClassSessionTeacherId ==. intKey tid]) mTeacher
            ++ maybe [] (\pid -> [ClassSessionStudentId ==. intKey pid]) mStudent
            ++ maybe [] (\startFrom -> [ClassSessionStartAt >=. startFrom]) mFrom
            ++ maybe [] (\endTo -> [ClassSessionStartAt <=. endTo]) mTo
      sessions <- selectList filters [Asc ClassSessionStartAt]
      dtos <- buildClassSessionDTOs sessions
      let normalized = T.toLower . T.strip
      pure $ maybe dtos (\st -> filter (\ClassSessionDTO{status = s} -> normalized s == normalized st) dtos) mStatus

    createClassH :: ClassSessionIn -> AppM ClassSessionOut
    createClassH ClassSessionIn{..} = do
      when (endAt <= startAt) $
        liftIO $ throwIO err400 { errBody = "La hora de fin debe ser mayor a la de inicio" }
      let studentKey = intKey studentId
          teacherKey = intKey teacherId
          subjectKey = intKey subjectId
          roomKey    = intKey roomId :: ResourceId
          bookingKey = maybeKey bookingId
          durationMinutes = floor (realToFrac (diffUTCTime endAt startAt) / 60 :: Double)
      teacherFree <- teacherAvailable teacherKey startAt endAt
      unless teacherFree $
        liftIO $ throwIO err409 { errBody = "Profesor no disponible en ese horario" }
      roomFree <- roomAvailable roomKey startAt endAt
      unless roomFree $
        liftIO $ throwIO err409 { errBody = "Sala no disponible en ese horario" }
      sid <- insert ClassSession
        { classSessionStudentId       = studentKey
        , classSessionTeacherId       = teacherKey
        , classSessionSubjectId       = subjectKey
        , classSessionStartAt         = startAt
        , classSessionEndAt           = endAt
        , classSessionRoomId          = roomKey
        , classSessionBookingId       = bookingKey
        , classSessionAttended        = False
        , classSessionPurchaseId      = Nothing
        , classSessionConsumedMinutes = max 0 durationMinutes
        , classSessionNotes           = Nothing
        }
      pure (ClassSessionOut (entityKeyInt sid) (max 0 durationMinutes))

    updateClassH :: Int -> ClassSessionUpdate -> AppM ClassSessionDTO
    updateClassH classId ClassSessionUpdate{..} = do
      let cid = intKey classId :: Key ClassSession
      mSession <- get cid
      case mSession of
        Nothing -> liftIO $ throwIO err404
        Just sess -> do
          let newStart   = fromMaybe (Trials.classSessionStartAt sess) startAt
              newEnd     = fromMaybe (Trials.classSessionEndAt sess) endAt
              newTeacher = maybe (Trials.classSessionTeacherId sess) intKey teacherId
              newRoom    = maybe (Trials.classSessionRoomId sess) intKey roomId
          when (newEnd <= newStart) $
            liftIO $ throwIO err400 { errBody = "La hora de fin debe ser mayor a la de inicio" }
          teacherFree <- teacherAvailable newTeacher newStart newEnd
          unless teacherFree $
            liftIO $ throwIO err409 { errBody = "Profesor no disponible en ese horario" }
          roomFree <- roomAvailable newRoom newStart newEnd
          unless roomFree $
            liftIO $ throwIO err409 { errBody = "Sala no disponible en ese horario" }
          let updates = concat
                [ maybe [] (\tid -> [ClassSessionTeacherId =. intKey tid]) teacherId
                , maybe [] (\sid -> [ClassSessionSubjectId =. intKey sid]) subjectId
                , maybe [] (\pid -> [ClassSessionStudentId =. intKey pid]) studentId
                , maybe [] (\v   -> [ClassSessionStartAt   =. v])         startAt
                , maybe [] (\v   -> [ClassSessionEndAt     =. v])         endAt
                , maybe [] (\rid -> [ClassSessionRoomId    =. intKey rid]) roomId
                , maybe [] (\bid -> [ClassSessionBookingId =. maybeKey (Just bid)]) bookingId
                , maybe [] (\txt -> [ClassSessionNotes     =. Just txt])  notes
                ]
          unless (null updates) $
            update cid updates
          ent <- getJustEntity cid
          dtos <- buildClassSessionDTOs [ent]
          pure (head dtos)

    attendH :: Int -> AttendIn -> AppM ClassSessionOut
    attendH classId AttendIn{..} = do
      let cid = intKey classId :: Key ClassSession
      mSession <- get cid
      case mSession of
        Nothing -> liftIO $ throwIO err404
        Just sess -> do
          let duration = classSessionConsumedMinutes sess
          update cid
            [ ClassSessionAttended =. attended
            , ClassSessionNotes    =. notes
            ]
          pure (ClassSessionOut (entityKeyInt cid) duration)

    commissionsH :: Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> AppM [CommissionDTO]
    commissionsH mFrom mTo mTeacher = do
      let baseFilters = catMaybes
            [ (CommissionRecognizedAt >=.) <$> mFrom
            , (CommissionRecognizedAt <=.) <$> mTo
            , (CommissionTeacherId ==.) . intKey <$> mTeacher
            ]
      entities <- selectList baseFilters [Desc CommissionRecognizedAt]
      pure [ CommissionDTO
              { teacherId  = entityKeyInt (commissionTeacherId commission)
              , amountCents = commissionAmountCents commission
              , basisCents  = commissionBasisCents commission
              , percent     = commissionPercent commission
              }
           | Entity _ commission <- entities
           ]

    teachersH :: AppM [TeacherDTO]
    teachersH = do
      teacherRoles <- selectList [Models.PartyRoleRole ==. Teacher, Models.PartyRoleActive ==. True] []
      let teacherIds = map (Models.partyRolePartyId . entityVal) teacherRoles

      parties <- if null teacherIds
        then pure Map.empty
        else do
          ents <- selectList [Models.PartyId <-. teacherIds] []
          pure $ Map.fromList [ (entityKey e, entityVal e) | e <- ents ]

      subjectLinks <- if null teacherIds
        then pure []
        else selectList [TeacherSubjectTeacherId <-. teacherIds] []
      let subjectIds = distinct (map (Trials.teacherSubjectSubjectId . entityVal) subjectLinks)

      subjectEntities <- if null subjectIds
        then pure []
        else selectList [SubjectId <-. subjectIds] []
      let subjectMap = Map.fromList [ (entityKey s, entityVal s) | s <- subjectEntities ]
          subjectsByTeacher = Map.fromListWith (<>) 
            [ ( Trials.teacherSubjectTeacherId (entityVal link)
              , [ Trials.teacherSubjectSubjectId (entityVal link) ]
              )
            | link <- subjectLinks
            ]

      pure
        [ TeacherDTO
            { teacherId   = entityKeyInt tid
            , teacherName = partyDisplayName party
            , subjects    =
                [ SubjectBriefDTO
                    { subjectId = entityKeyInt sid
                    , name      = Trials.subjectName subj
                    }
                | sid <- Map.findWithDefault [] tid subjectsByTeacher
                , subj <- maybeToList (Map.lookup sid subjectMap)
                ]
            }
        | (tid, party) <- Map.toList parties
        ]

    buildClassSessionDTOs :: [Entity ClassSession] -> AppM [ClassSessionDTO]
    buildClassSessionDTOs sessions = do
      now <- liftIO getCurrentTime
      let subjectIds = distinct (map (Trials.classSessionSubjectId . entityVal) sessions)
          teacherIds = distinct (map (Trials.classSessionTeacherId . entityVal) sessions)
          studentIds = distinct (map (Trials.classSessionStudentId . entityVal) sessions)
          bookingIds = catMaybes (map (Trials.classSessionBookingId . entityVal) sessions)
          roomIds    = distinct (map (Trials.classSessionRoomId . entityVal) sessions)

      subjectMap <- if null subjectIds
        then pure Map.empty
        else do
          ents <- selectList [SubjectId <-. subjectIds] []
          pure $ Map.fromList [ (entityKey e, entityVal e) | e <- ents ]

      let partyIds = distinct (teacherIds ++ studentIds)
      partyMap <- if null partyIds
        then pure Map.empty
        else do
          ents <- selectList [Models.PartyId <-. partyIds] []
          pure $ Map.fromList [ (entityKey e, partyDisplayName (entityVal e)) | e <- ents ]

      bookingMap <- if null bookingIds
        then pure Map.empty
        else do
          ents <- selectList [Models.BookingId <-. bookingIds] []
          pure $ Map.fromList [ (entityKey e, entityVal e) | e <- ents ]

      resourceMap <- if null roomIds
        then pure Map.empty
        else do
          ents <- selectList [Models.ResourceId <-. roomIds] []
          pure $ Map.fromList [ (entityKey e, entityVal e) | e <- ents ]

      pure
        [ ClassSessionDTO
            { classSessionId = entityKeyInt sid
            , teacherId      = entityKeyInt (Trials.classSessionTeacherId cs)
            , teacherName    = Map.lookup (Trials.classSessionTeacherId cs) partyMap
            , subjectId      = entityKeyInt (Trials.classSessionSubjectId cs)
            , subjectName    = fmap Trials.subjectName (Map.lookup (Trials.classSessionSubjectId cs) subjectMap)
            , studentId      = entityKeyInt (Trials.classSessionStudentId cs)
            , studentName    = Map.lookup (Trials.classSessionStudentId cs) partyMap
            , startAt        = Trials.classSessionStartAt cs
            , endAt          = Trials.classSessionEndAt cs
            , status         = classStatusLabel now (Trials.classSessionAttended cs) (Trials.classSessionStartAt cs) (Trials.classSessionBookingId cs >>= (`Map.lookup` bookingMap))
            , roomId         = Just (toPathPiece (Trials.classSessionRoomId cs))
            , roomName       = Models.resourceName <$> Map.lookup (Trials.classSessionRoomId cs) resourceMap
            , bookingId      = entityKeyInt <$> Trials.classSessionBookingId cs
            , notes          = Trials.classSessionNotes cs
            }
        | Entity sid cs <- sessions
        ]

    teacherClassesH :: Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> AppM [ClassSessionDTO]
    teacherClassesH teacherId mSubject mFrom mTo = do
      let teacherKey = intKey teacherId :: PartyId
          filters = [ClassSessionTeacherId ==. teacherKey]
                  ++ maybe [] (\sid -> [ClassSessionSubjectId ==. intKey sid]) mSubject
                  ++ maybe [] (\startFrom -> [ClassSessionStartAt >=. startFrom]) mFrom
                  ++ maybe [] (\endTo -> [ClassSessionStartAt <=. endTo]) mTo
      sessions <- selectList filters [Asc ClassSessionStartAt]
      buildClassSessionDTOs sessions

    classStatusLabel :: UTCTime -> Bool -> UTCTime -> Maybe Models.Booking -> Text
    classStatusLabel now attended startAt mBooking =
      case Models.bookingStatus <$> mBooking of
        Just Models.Cancelled  -> "cancelada"
        Just Models.NoShow     -> "cancelada"
        Just Models.Completed  -> "realizada"
        Just Models.Tentative  -> "por-confirmar"
        Just Models.InProgress -> "programada"
        Just Models.Confirmed  -> "programada"
        _ ->
          if attended
            then "realizada"
            else if startAt > now then "programada" else "por-confirmar"

    studentsListH :: AppM [StudentDTO]
    studentsListH = do
      studentRoles <- selectList [Models.PartyRoleRole ==. Student, Models.PartyRoleActive ==. True] []
      let ids = map (Models.partyRolePartyId . entityVal) studentRoles
      parties <- if null ids
        then pure []
        else selectList [Models.PartyId <-. ids] []
      pure
        [ StudentDTO
            { studentId   = entityKeyInt pid
            , displayName = Models.partyDisplayName party
            , email       = Models.partyPrimaryEmail party
            , phone       = Models.partyPrimaryPhone party
            }
        | Entity pid party <- parties
        ]

    studentCreateH :: StudentCreate -> AppM StudentDTO
    studentCreateH StudentCreate{..} = do
      now <- liftIO getCurrentTime
      partyId <- createOrFetchParty (Just fullName) (Just email) phone now
      void $ upsert (PartyRole partyId Student True) [Models.PartyRoleActive =. True]
      when (isJust notes) $
        update partyId [Models.PartyNotes =. fmap T.strip notes]
      Entity _ party <- getJustEntity partyId
      pure StudentDTO
        { studentId   = entityKeyInt partyId
        , displayName = Models.partyDisplayName party
        , email       = Models.partyPrimaryEmail party
        , phone       = Models.partyPrimaryPhone party
        }


trialsServer :: ConnectionPool -> Server TrialsAPI
trialsServer pool =
  let trialsProxy = Proxy :: Proxy TrialsAPI
      ctxProxy    = Proxy :: Proxy '[AuthHandler Request AuthedUser]
      server      = publicTrialsServer :<|> authedPrivateServer
  in hoistServerWithContext trialsProxy ctxProxy nt server
  where
    nt :: AppM a -> Handler a
    nt x = liftIO (runSqlPool x pool)

    authedPrivateServer :: AuthedUser -> ServerT PrivateTrialsAPI AppM
    authedPrivateServer user = privateTrialsServer user
