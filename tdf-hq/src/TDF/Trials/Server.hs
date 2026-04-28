{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module TDF.Trials.Server where

import           Control.Exception      (SomeException, displayException, throwIO, try)
import           Control.Monad          (forM, forM_, unless, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Int               (Int64)
import           Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isAlphaNum
  , isAsciiLower
  , isControl
  , isDigit
  , isSpace
  )
import           Data.Maybe             (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import           Data.List              (foldl', sortOn)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (UTCTime, diffUTCTime, getCurrentTime)
import           System.IO              (hPutStrLn, stderr)
import           Text.Read              (readMaybe)
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

normalizeEmail :: Maybe Text -> Maybe Text
normalizeEmail = fmap T.toLower . cleanOptional

isValidEmail :: Text -> Bool
isValidEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      T.length candidate <= maxPublicEmailChars
        && isValidEmailLocalPart localPart
        && not (T.null domain)
        && not (T.any isSpace candidate)
        && not (T.isPrefixOf "." domain)
        && not (T.isSuffixOf "." domain)
        && T.isInfixOf "." domain
        && all isValidEmailDomainLabel (T.splitOn "." domain)
    _ -> False

maxPublicEmailChars :: Int
maxPublicEmailChars = 254

isValidEmailLocalPart :: Text -> Bool
isValidEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= maxPublicEmailLocalPartChars
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (".." `T.isInfixOf` localPart)
    && T.all isValidEmailLocalChar localPart

maxPublicEmailLocalPartChars :: Int
maxPublicEmailLocalPartChars = 64

isValidEmailLocalChar :: Char -> Bool
isValidEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidEmailDomainLabel :: Text -> Bool
isValidEmailDomainLabel label =
  not (T.null label)
    && T.length label <= maxPublicEmailDomainLabelChars
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidEmailDomainChar label

maxPublicEmailDomainLabelChars :: Int
maxPublicEmailDomainLabelChars = 63

isValidEmailDomainChar :: Char -> Bool
isValidEmailDomainChar c = isAsciiLower c || isDigit c || c == '-'

validateOptionalEmail :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalEmail Nothing = Right Nothing
validateOptionalEmail (Just rawEmail) =
  case normalizeEmail (Just rawEmail) of
    Nothing -> Right Nothing
    Just emailVal
      | emailVal == publicLeadFallbackEmail ->
          Left err400 { errBody = "email is reserved for anonymous public interests" }
      | isValidEmail emailVal ->
          Right (Just emailVal)
      | otherwise ->
          Left err400 { errBody = "email inválido" }

validateEmailUpdate :: Maybe Text -> Either ServerError (Maybe (Maybe Text))
validateEmailUpdate Nothing = Right Nothing
validateEmailUpdate (Just rawEmail) =
  case cleanOptional (Just rawEmail) of
    Nothing -> Right (Just Nothing)
    Just _ -> Just <$> validateOptionalEmail (Just rawEmail)

ensureEmailAvailableForParty :: PartyId -> Maybe (Maybe Text) -> AppM ()
ensureEmailAvailableForParty _ Nothing = pure ()
ensureEmailAvailableForParty _ (Just Nothing) = pure ()
ensureEmailAvailableForParty partyKey (Just (Just emailVal)) = do
  matches <- selectList [Models.PartyPrimaryEmail ==. Just emailVal] []
  when (any ((/= partyKey) . entityKey) matches) $
    liftIO $ throwIO err409 { errBody = "El correo ya está asignado a otra persona." }

validateRequiredEmail :: Maybe Text -> Either ServerError Text
validateRequiredEmail mEmail =
  case validateOptionalEmail mEmail of
    Left err -> Left err
    Right Nothing -> Left err400 { errBody = "Correo requerido para crear la cuenta" }
    Right (Just emailVal)
      | emailVal == publicLeadFallbackEmail ->
          Left err400 { errBody = "email is reserved for anonymous public interests" }
      | otherwise -> Right emailVal

normalizePhone :: Text -> Maybe Text
normalizePhone raw =
  let trimmed = T.strip raw
      onlyDigits = T.filter isDigit trimmed
      digitCount = T.length onlyDigits
      plusCount = T.count "+" trimmed
      plusIndex = T.findIndex (== '+') trimmed
      firstDigitIndex = T.findIndex isDigit trimmed
      allowedPhoneChar ch =
        isDigit ch || isSpace ch || ch `elem` ("+-()." :: String)
      hasInvalidChars = T.any (not . allowedPhoneChar) trimmed
      plusIsValid =
        case plusIndex of
          Nothing -> True
          Just idx ->
            case firstDigitIndex of
              Nothing -> False
              Just digitIdx -> plusCount == 1 && idx < digitIdx
  in
    if T.null onlyDigits
        || digitCount < 8
        || digitCount > 15
        || hasInvalidChars
        || not plusIsValid
      then Nothing
      else Just ("+" <> onlyDigits)

validateOptionalPhone :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalPhone Nothing = Right Nothing
validateOptionalPhone (Just rawPhone) =
  case cleanOptional (Just rawPhone) of
    Nothing -> Right Nothing
    Just _ ->
      case normalizePhone rawPhone of
        Just phoneVal -> Right (Just phoneVal)
        Nothing -> Left err400 { errBody = "phone inválido" }

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
      let candidate = buildTrialUsernameCandidate root attempt
      conflict <- getBy (Models.UniqueCredentialUsername candidate)
      case conflict of
        Nothing -> pure candidate
        Just _  -> go (attempt + 1)

buildTrialUsernameCandidate :: Text -> Int -> Text
buildTrialUsernameCandidate root attempt =
  let suffix = if attempt <= 0 then "" else "-" <> T.pack (show attempt)
      rootBudget = max 0 (60 - T.length suffix)
  in T.take 60 (T.take rootBudget root <> suffix)

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

teacherAvailableExceptClassSession :: PartyId -> UTCTime -> UTCTime -> Key ClassSession -> AppM Bool
teacherAvailableExceptClassSession teacherId slotStart slotEnd classSessionId = do
  hasTrialConflict <- recordExists
    [ TrialAssignmentTeacherId ==. teacherId
    , TrialAssignmentStartAt <. slotEnd
    , TrialAssignmentEndAt   >. slotStart
    ]
  hasClassConflict <- recordExists
    [ ClassSessionTeacherId ==. teacherId
    , ClassSessionId !=. classSessionId
    , ClassSessionStartAt <. slotEnd
    , ClassSessionEndAt   >. slotStart
    ]
  pure (not (hasTrialConflict || hasClassConflict))

teacherAvailableExceptTrialRequest :: PartyId -> UTCTime -> UTCTime -> Key TrialRequest -> AppM Bool
teacherAvailableExceptTrialRequest teacherId slotStart slotEnd requestId = do
  hasTrialConflict <- recordExists
    [ TrialAssignmentTeacherId ==. teacherId
    , TrialAssignmentRequestId !=. requestId
    , TrialAssignmentStartAt <. slotEnd
    , TrialAssignmentEndAt   >. slotStart
    ]
  hasClassConflict <- recordExists
    [ ClassSessionTeacherId ==. teacherId
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

roomAvailableExceptClassSession :: ResourceId -> UTCTime -> UTCTime -> Key ClassSession -> AppM Bool
roomAvailableExceptClassSession roomId slotStart slotEnd classSessionId = do
  hasClassConflict <- recordExists
    [ ClassSessionRoomId ==. roomId
    , ClassSessionId !=. classSessionId
    , ClassSessionStartAt <. slotEnd
    , ClassSessionEndAt   >. slotStart
    ]
  -- check bookings attached to the room resource
  bookingRes <- selectList [Models.BookingResourceResourceId ==. roomId] []
  let bookingIds = map (Models.bookingResourceBookingId . entityVal) bookingRes
  bookings <- if null bookingIds
    then pure []
    else selectList
      [ Models.BookingId <-. bookingIds
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

validatePreferredSlots :: [PreferredSlot] -> Either ServerError [PreferredSlot]
validatePreferredSlots [] =
  Left err400 { errBody = "Need at least one preferred slot" }
validatePreferredSlots slots
  | length slots > 3 =
      Left err400 { errBody = "At most three preferred slots are allowed" }
  | otherwise =
      do
        validated <- traverse validateSlot slots
        if hasOverlappingSlots validated
          then Left err400 { errBody = "Preferred slots must be distinct non-overlapping windows" }
          else Right validated
  where
    validateSlot slot@(PreferredSlot slotStart slotEnd)
      | slotStart >= slotEnd =
          Left err400 { errBody = "Preferred slot endAt must be after startAt" }
      | otherwise =
          Right slot

    hasOverlappingSlots :: [PreferredSlot] -> Bool
    hasOverlappingSlots slotList =
      any overlapsAdjacent (zip orderedSlots (drop 1 orderedSlots))
      where
        orderedSlots = sortOn (\(PreferredSlot slotStart _) -> slotStart) slotList
        overlapsAdjacent (PreferredSlot _ leftEnd, PreferredSlot rightStart _) =
          rightStart < leftEnd

validatePreferredSlotsAt :: UTCTime -> [PreferredSlot] -> Either ServerError [PreferredSlot]
validatePreferredSlotsAt now slots = do
  validated <- validatePreferredSlots slots
  traverse validateFutureSlot validated
  where
    validateFutureSlot slot@(PreferredSlot slotStart _)
      | slotStart <= now =
          Left err400 { errBody = "Preferred slots must start in the future" }
      | otherwise =
          Right slot

validatePublicTrialPartyId :: Maybe Int -> Either ServerError ()
validatePublicTrialPartyId Nothing = Right ()
validatePublicTrialPartyId (Just _) =
  Left err400 { errBody = "partyId is not allowed on public trial requests" }

validatePublicTrialRequestInput :: TrialRequestIn -> Either ServerError TrialRequestIn
validatePublicTrialRequestInput
  (TrialRequestIn rawPartyId rawSubjectId preferredSlots rawNotes rawFullName rawEmail rawPhone) = do
  validatePublicTrialPartyId rawPartyId
  subjectIdVal <- validatePublicSubjectIdInput rawSubjectId
  preferredVal <- validatePreferredSlots preferredSlots
  fullNameVal <- validateOptionalPublicTextField "fullName" 160 rawFullName
  notesVal <- validateOptionalPublicTextField "notes" 2000 rawNotes
  emailVal <- validateRequiredEmail rawEmail
  phoneVal <- validateOptionalPhone rawPhone
  Right (TrialRequestIn Nothing subjectIdVal preferredVal notesVal fullNameVal (Just emailVal) phoneVal)

validateOptionalPublicTextField :: Text -> Int -> Maybe Text -> Either ServerError (Maybe Text)
validateOptionalPublicTextField fieldName maxChars rawValue =
  case cleanOptional rawValue of
    Nothing ->
      Right Nothing
    Just value
      | T.length value > maxChars ->
          Left
            (badRequestText
              (fieldName <> " must be 1-" <> T.pack (show maxChars) <> " characters"))
      | T.any invalidPublicTextChar value ->
          Left (badRequestText (publicTextCharacterMessage fieldName))
      | otherwise ->
          Right (Just value)

invalidPublicTextChar :: Char -> Bool
invalidPublicTextChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

publicTextCharacterMessage :: Text -> Text
publicTextCharacterMessage fieldName =
  fieldName <> " must not contain control characters or hidden formatting characters"

validatePublicSignupInput :: SignupIn -> Either ServerError SignupIn
validatePublicSignupInput
  (SignupIn
    rawFirstName
    rawLastName
    rawEmail
    rawPhone
    passwordVal
    googleIdTokenVal
    marketingOptInVal) = do
  firstNameVal <- validatePublicSignupNamePart "firstName" rawFirstName
  lastNameVal <- validatePublicSignupNamePart "lastName" rawLastName
  emailVal <- validateRequiredEmail (Just rawEmail)
  phoneVal <- validateOptionalPhone rawPhone
  validateUnsupportedPublicSignupCredential "password" passwordVal
  validateUnsupportedPublicSignupCredential "googleIdToken" googleIdTokenVal
  Right
    (SignupIn firstNameVal lastNameVal emailVal phoneVal Nothing Nothing marketingOptInVal)

validateUnsupportedPublicSignupCredential :: Text -> Maybe Text -> Either ServerError ()
validateUnsupportedPublicSignupCredential _ Nothing =
  Right ()
validateUnsupportedPublicSignupCredential fieldName (Just rawValue)
  | T.null (T.strip rawValue) =
      Right ()
  | otherwise =
      Left err400
        { errBody =
            BL8.pack (T.unpack (fieldName <> " is not supported on public signup"))
        }

validatePublicSignupNamePart :: Text -> Text -> Either ServerError Text
validatePublicSignupNamePart fieldName rawName =
  let nameVal = T.strip rawName
      fieldLabel = BL8.pack (T.unpack fieldName)
  in if T.null nameVal
       then Left err400 { errBody = fieldLabel <> " is required" }
       else if T.length nameVal > 120
       then Left err400 { errBody = fieldLabel <> " must be 120 characters or fewer" }
       else
         if T.any invalidPublicTextChar nameVal
           then
             Left err400
               { errBody = BL8.pack (T.unpack (publicTextCharacterMessage fieldName)) }
           else Right nameVal

validatePublicSubjectIdInput :: Int -> Either ServerError Int
validatePublicSubjectIdInput subjectIdInt
  | subjectIdInt <= 0 =
      Left err400 { errBody = "subjectId must be a positive integer" }
  | otherwise =
      Right subjectIdInt

validateTeacherSubjectIdsInput :: [Int] -> Either ServerError [Int]
validateTeacherSubjectIdsInput rawSubjectIds
  | any (<= 0) rawSubjectIds =
      Left err400 { errBody = "subjectIds must contain only positive integers" }
  | length rawSubjectIds /= Set.size (Set.fromList rawSubjectIds) =
      Left err400 { errBody = "subjectIds must not contain duplicates" }
  | otherwise =
      Right rawSubjectIds

validateTrialAssignInput :: Int -> TrialAssignIn -> Either ServerError (Int, TrialAssignIn)
validateTrialAssignInput requestIdInt input@TrialAssignIn{..}
  | requestIdInt <= 0 =
      Left err400 { errBody = "requestId must be a positive integer" }
  | teacherId <= 0 =
      Left err400 { errBody = "teacherId must be a positive integer" }
  | otherwise =
      Right (requestIdInt, input)

validateTrialScheduleInput :: TrialScheduleIn -> Either ServerError TrialScheduleIn
validateTrialScheduleInput input@TrialScheduleIn{..}
  | requestId <= 0 =
      Left err400 { errBody = "requestId must be a positive integer" }
  | teacherId <= 0 =
      Left err400 { errBody = "teacherId must be a positive integer" }
  | roomId <= 0 =
      Left err400 { errBody = "roomId must be a positive integer" }
  | endAt <= startAt =
      Left err400 { errBody = "La hora de fin debe ser mayor a la de inicio" }
  | otherwise =
      Right input

badRequestText :: Text -> ServerError
badRequestText message =
  err400 { errBody = BL8.pack (T.unpack message) }

validatePositiveIntField :: Text -> Int -> Either ServerError Int
validatePositiveIntField fieldName value
  | value <= 0 =
      Left (badRequestText (fieldName <> " must be a positive integer"))
  | otherwise =
      Right value

validateOptionalPositiveIntField :: Text -> Maybe Int -> Either ServerError (Maybe Int)
validateOptionalPositiveIntField fieldName =
  traverse (validatePositiveIntField fieldName)

validateNonNegativeIntField :: Text -> Int -> Either ServerError Int
validateNonNegativeIntField fieldName value
  | value < 0 =
      Left (badRequestText (fieldName <> " must be zero or a positive integer"))
  | otherwise =
      Right value

validatePurchaseInput :: PurchaseIn -> Either ServerError PurchaseIn
validatePurchaseInput input@PurchaseIn{..} = do
  _ <- validatePositiveIntField "studentId" studentId
  _ <- validatePositiveIntField "packageId" packageId
  _ <- validateOptionalPositiveIntField "sellerId" sellerId
  _ <- validateOptionalPositiveIntField "commissionedTeacherId" commissionedTeacherId
  _ <- validateOptionalPositiveIntField "trialRequestId" trialRequestId
  _ <- validateNonNegativeIntField "priceCents" priceCents
  discount <- maybe (Right 0) (validateNonNegativeIntField "discountCents") discountCents
  tax <- maybe (Right 0) (validateNonNegativeIntField "taxCents") taxCents
  when (discount > priceCents) $
    Left (badRequestText "discountCents must not exceed priceCents")
  let total = priceCents - discount + tax
  when (total < 0) $
    Left (badRequestText "purchase total must not be negative")
  Right input

normalizeTrialRequestStatusFilter :: Text -> Maybe Text
normalizeTrialRequestStatusFilter rawStatus =
  case T.toLower (T.strip rawStatus) of
    "requested" -> Just statusRequested
    "assigned" -> Just statusAssigned
    "scheduled" -> Just statusScheduled
    _ -> Nothing

validateOptionalTrialRequestStatusFilter :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalTrialRequestStatusFilter Nothing = Right Nothing
validateOptionalTrialRequestStatusFilter (Just rawStatus) =
  case cleanOptional (Just rawStatus) of
    Nothing -> Right Nothing
    Just statusVal ->
      case normalizeTrialRequestStatusFilter statusVal of
        Just normalized -> Right (Just normalized)
        Nothing ->
          Left err400 { errBody = "status must be one of: Requested, Assigned, Scheduled" }

validateAvailabilityListFilters
  :: Maybe Int
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Either ServerError (Maybe Int, Maybe UTCTime, Maybe UTCTime)
validateAvailabilityListFilters rawSubjectId mFrom mTo = do
  subjectId <- traverse validatePublicSubjectIdInput rawSubjectId
  case (mFrom, mTo) of
    (Just fromTs, Just toTs)
      | fromTs > toTs ->
          Left err400 { errBody = "from must be on or before to" }
    _ -> pure ()
  Right (subjectId, mFrom, mTo)

validateAvailabilityIdInput :: Int -> Either ServerError Int
validateAvailabilityIdInput =
  validatePositiveIntField "availabilityId"

validateClassSessionListFilters
  :: Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Either ServerError (Maybe Int, Maybe Int, Maybe Int, Maybe UTCTime, Maybe UTCTime)
validateClassSessionListFilters rawSubjectId rawTeacherId rawStudentId mFrom mTo = do
  subjectId <- traverse (validatePositiveIntField "subjectId") rawSubjectId
  teacherId <- traverse (validatePositiveIntField "teacherId") rawTeacherId
  studentId <- traverse (validatePositiveIntField "studentId") rawStudentId
  case (mFrom, mTo) of
    (Just fromTs, Just toTs)
      | fromTs > toTs ->
          Left err400 { errBody = "from must be on or before to" }
    _ -> pure ()
  Right (subjectId, teacherId, studentId, mFrom, mTo)

validateTeacherClassesFilters
  :: Int
  -> Maybe Int
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Either ServerError (Int, Maybe Int, Maybe UTCTime, Maybe UTCTime)
validateTeacherClassesFilters rawTeacherId rawSubjectId mFrom mTo = do
  teacherId <- validatePositiveIntField "teacherId" rawTeacherId
  (subjectId, fromTs, toTs) <- validateAvailabilityListFilters rawSubjectId mFrom mTo
  Right (teacherId, subjectId, fromTs, toTs)

validateCommissionListFilters
  :: Maybe UTCTime
  -> Maybe UTCTime
  -> Maybe Int
  -> Either ServerError (Maybe UTCTime, Maybe UTCTime, Maybe Int)
validateCommissionListFilters mFrom mTo rawTeacherId = do
  teacherId <- traverse (validatePositiveIntField "teacherId") rawTeacherId
  case (mFrom, mTo) of
    (Just fromTs, Just toTs)
      | fromTs > toTs ->
          Left err400 { errBody = "from must be on or before to" }
    _ -> pure ()
  Right (mFrom, mTo, teacherId)

validateOptionalDriveLink :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalDriveLink Nothing = Right Nothing
validateOptionalDriveLink (Just rawDriveLink) =
  case cleanOptional (Just rawDriveLink) of
    Nothing -> Right Nothing
    Just driveLinkVal ->
      if "https://" `T.isPrefixOf` T.toLower driveLinkVal && isValidHttpUrl driveLinkVal
        then Right (Just driveLinkVal)
        else Left err400 { errBody = "driveLink must be an absolute https URL" }

isValidHttpUrl :: Text -> Bool
isValidHttpUrl rawUrl
  | T.any invalidUrlChar trimmed = False
  | "http://" `T.isPrefixOf` lowerUrl = hasValidAuthority (T.drop 7 trimmed)
  | "https://" `T.isPrefixOf` lowerUrl = hasValidAuthority (T.drop 8 trimmed)
  | otherwise = False
  where
    trimmed = T.strip rawUrl
    lowerUrl = T.toLower trimmed

    invalidUrlChar ch = isSpace ch || isControl ch

    hasValidAuthority remainder =
      let authority = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') remainder
      in validateAuthority authority

    validateAuthority rawAuthority
      | T.null rawAuthority = False
      | T.any (== '@') rawAuthority = False
      -- Public lead drive links should be DNS-backed URLs, not IP literals.
      | "[" `T.isPrefixOf` rawAuthority =
          False
      | T.count ":" rawAuthority > 1 = False
      | otherwise =
          let (host, portSuffix) = T.breakOn ":" rawAuthority
          in validateHost host && validatePortSuffix portSuffix

    validateHost host =
      let normalizedHost = T.toLower host
      in not (T.null normalizedHost)
        && not (T.isPrefixOf "." normalizedHost)
        && not (T.isSuffixOf "." normalizedHost)
        && not (isAmbiguousNumericHost normalizedHost)
        && all isValidEmailDomainLabel (T.splitOn "." normalizedHost)
        && not (looksLikeInvalidIpv4 normalizedHost)
        && not (requiresExplicitPublicHostname normalizedHost)
        && not (isPrivateHost normalizedHost)

    validatePortSuffix suffix
      | T.null suffix = True
      | ":" `T.isPrefixOf` suffix =
          let port = T.drop 1 suffix
          in not (T.null port)
               && T.all isDigit port
               && maybe False (\portNumber -> portNumber >= (1 :: Int) && portNumber <= 65535)
                    (readMaybe (T.unpack port))
      | otherwise = False

    isPrivateHost host =
      host == "localhost"
        || ".localhost" `T.isSuffixOf` host
        || maybe False isNonPublicIpv4 (parseIpv4Octets host)
        || maybe False isNonPublicIpv4 (trailingIpv4Octets host)
        || isPrivateIpv6 host

    parseIpv4Octets host =
      case T.splitOn "." host of
        [a, b, c, d] -> do
          oa <- parseOctet a
          ob <- parseOctet b
          oc <- parseOctet c
          od <- parseOctet d
          pure (oa, ob, oc, od)
        _ -> Nothing

    -- Reject malformed dotted-quad hosts that would otherwise slip through as DNS labels.
    looksLikeInvalidIpv4 host =
      case T.splitOn "." host of
        [a, b, c, d]
          | all isNumericLabel [a, b, c, d] -> isNothing (parseIpv4Octets host)
        _ -> False

    isAmbiguousNumericHost host =
      T.all (\c -> isDigit c || c == '.') host
        && isNothing (parseIpv4Octets host)

    requiresExplicitPublicHostname host =
      not (T.any (== '.') host)
        && not (T.any (== ':') host)
        && isNothing (parseIpv4Octets host)

    isNumericLabel label =
      not (T.null label) && T.all isDigit label

    parseOctet octet
      | T.null octet || T.any (not . isDigit) octet = Nothing
      | T.length octet > 1 && T.head octet == '0' = Nothing
      | otherwise = do
          value <- readMaybe (T.unpack octet)
          if value >= (0 :: Int) && value <= 255
            then Just value
            else Nothing

    trailingIpv4Octets host =
      let suffix = T.takeWhileEnd (/= ':') host
      in if T.any (== '.') suffix
           then parseIpv4Octets suffix
           else Nothing

    isNonPublicIpv4 (a, b, c, _) =
      a == (0 :: Int)
        || a == 10
        || a == 127
        || (a == 100 && b >= 64 && b <= 127)
        || (a == 169 && b == 254)
        || (a == 172 && b >= 16 && b <= 31)
        || (a == 192 && b == 0 && c == 0)
        || (a == 192 && b == 0 && c == 2)
        || (a == 192 && b == 168)
        || (a == 198 && (b == 18 || b == 19))
        || (a == 198 && b == 51 && c == 100)
        || (a == 203 && b == 0 && c == 113)
        || (a >= 224 && a <= 255)

    isPrivateIpv6 host =
      host == "::"
        || host == "::1"
        || isUniqueLocal firstSegment
        || isLinkLocal firstSegment
      where
        firstSegment = T.takeWhile (/= ':') host

        isUniqueLocal segment =
          "fc" `T.isPrefixOf` segment || "fd" `T.isPrefixOf` segment

        isLinkLocal segment =
          any (`T.isPrefixOf` segment) ["fe8", "fe9", "fea", "feb"]

validatePublicInterestType :: Text -> Either ServerError Text
validatePublicInterestType rawInterestType =
  case cleanOptional (Just rawInterestType) of
    Nothing ->
      Left err400 { errBody = "interestType is required" }
    Just interestTypeVal
      | T.length interestTypeVal > 80 ->
          Left err400 { errBody = "interestType must be 1-80 characters" }
      | T.any invalidPublicTextChar interestTypeVal ->
          Left (badRequestText (publicTextCharacterMessage "interestType"))
      | otherwise ->
          Right interestTypeVal

validatePublicInterestDetails :: Maybe Text -> Either ServerError (Maybe Text)
validatePublicInterestDetails rawDetails =
  case cleanOptional rawDetails of
    Nothing ->
      Right Nothing
    Just detailsVal
      | T.length detailsVal > 2000 ->
          Left err400 { errBody = "details must be 1-2000 characters" }
      | T.any invalidPublicTextChar detailsVal ->
          Left (badRequestText (publicTextCharacterMessage "details"))
      | otherwise ->
          Right (Just detailsVal)

validatePublicInterestInput :: InterestIn -> Either ServerError InterestIn
validatePublicInterestInput (InterestIn rawInterestType rawSubjectId details driveLink) = do
  interestTypeVal <- validatePublicInterestType rawInterestType
  subjectId <- traverse validatePublicSubjectIdInput rawSubjectId
  detailsVal <- validatePublicInterestDetails details
  driveLinkVal <- validateOptionalDriveLink driveLink
  Right (InterestIn interestTypeVal subjectId detailsVal driveLinkVal)

validatePublicSubjectSelection :: Maybe Subject -> Either ServerError ()
validatePublicSubjectSelection (Just subject)
  | subjectActive subject = Right ()
validatePublicSubjectSelection _ =
  Left err422 { errBody = "La materia solicitada no está disponible" }

requirePublicActiveSubject :: Int -> AppM SubjectId
requirePublicActiveSubject subjectIdInt = do
  normalizedSubjectId <- either (liftIO . throwIO) pure (validatePublicSubjectIdInput subjectIdInt)
  let subjectKey = intKey normalizedSubjectId :: SubjectId
  mSubject <- get subjectKey
  either (liftIO . throwIO) pure (validatePublicSubjectSelection mSubject)
  pure subjectKey

publicTrialsServer :: ServerT PublicTrialsAPI AppM
publicTrialsServer =
  signupH
    :<|> interestH
    :<|> trialRequestCreateH
    :<|> publicSubjectsH
    :<|> publicSlotsH
  where
    signupH :: SignupIn -> AppM SignupOut
    signupH rawInput = do
      SignupIn{..} <- either (liftIO . throwIO) pure (validatePublicSignupInput rawInput)
      now <- liftIO getCurrentTime
      let fullName = composeFullName firstName lastName
      partyIdKey <- createOrFetchParty fullName (Just email) phone now
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
    interestH rawInput = do
      InterestIn{..} <- either (liftIO . throwIO) pure (validatePublicInterestInput rawInput)
      now <- liftIO getCurrentTime
      subjectKey <- traverse requirePublicActiveSubject subjectId
      partyIdKey <- ensurePublicLeadParty now
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
    trialRequestCreateH rawInput = do
      now <- liftIO getCurrentTime
      TrialRequestIn{..} <- either (liftIO . throwIO) pure (validatePublicTrialRequestInput rawInput)
      let nameClean  = cleanOptional fullName
          emailClean = cleanOptional email
          phoneClean = cleanOptional phone
      slots <- either (liftIO . throwIO) pure (validatePreferredSlotsAt now preferred)
      subjectKey <- requirePublicActiveSubject subjectId
      resolvedPartyId <- createOrFetchParty nameClean emailClean phoneClean now

      mNewCred <- case emailClean of
        Nothing -> pure Nothing
        Just addr -> ensureUserAccountForParty resolvedPartyId nameClean addr

      -- Send welcome email only when we created a credential.
      case (mNewCred, emailClean) of
        (Just (username, password), Just addr) -> liftIO $ do
          cfg <- loadConfig
          let svc = EmailSvc.mkEmailService cfg
              display = fromMaybe addr nameClean
          welcomeResult <- (try $
            EmailSvc.sendWelcome svc display addr username password) :: IO (Either SomeException ())
          case welcomeResult of
            Left err ->
              hPutStrLn stderr ("[Trials] Failed to send welcome email to " <> T.unpack addr <> ": " <> displayException err)
            Right () -> pure ()
        _ -> pure ()
      case slots of
        [] ->
          liftIO $ throwIO err500 { errBody = "Validated preferred slots were unexpectedly empty" }
        PreferredSlot firstStart firstEnd : rest -> do
          let pref2 = listToMaybe rest
              pref3 = listToMaybe (drop 1 rest)
              (pref2Start, pref2End) = slotBounds pref2
              (pref3Start, pref3End) = slotBounds pref3
              partyKey = resolvedPartyId
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
    publicSlotsH Nothing = pure []
    publicSlotsH (Just subjectIdInt) = do
      _ <- requirePublicActiveSubject subjectIdInt
      trialSlotsForSubject (Just subjectIdInt)

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
  emailVal <- either (liftIO . throwIO) pure (validateRequiredEmail mEmail)
  phoneVal <- either (liftIO . throwIO) pure (validateOptionalPhone mPhone)
  let
      display = fromMaybe emailVal (cleanOptional mName)
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

composeFullName :: Text -> Text -> Maybe Text
composeFullName firstName lastName =
  cleanOptional $
    Just
      (T.intercalate " " (filter (not . T.null) [T.strip firstName, T.strip lastName]))

publicLeadFallbackEmail :: Text
publicLeadFallbackEmail = "public-interest@tdf.local"

ensurePublicLeadParty :: UTCTime -> AppM PartyId
ensurePublicLeadParty now = do
  mExisting <- selectFirst [Models.PartyPrimaryEmail ==. Just publicLeadFallbackEmail] []
  case mExisting of
    Just (Entity partyId _) -> pure partyId
    Nothing ->
      insert Party
        { partyLegalName = Nothing
        , partyDisplayName = "Public Trial Interest"
        , partyIsOrg = False
        , partyTaxId = Nothing
        , partyPrimaryEmail = Just publicLeadFallbackEmail
        , partyPrimaryPhone = Nothing
        , partyWhatsapp = Nothing
        , partyInstagram = Nothing
        , partyEmergencyContact = Nothing
        , partyNotes = Just "System fallback party for anonymous public trial interests."
        , partyCreatedAt = now
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
    :<|> teacherSubjectsUpdateH
    :<|> teacherStudentsListH
    :<|> teacherStudentsAddH
    :<|> teacherStudentsDeleteH
    :<|> studentsListH
    :<|> studentCreateH
    :<|> studentUpdateH
  where
    hasRole :: RoleEnum -> Bool
    hasRole role = role `elem` auRoles

    isSchoolStaff :: Bool
    isSchoolStaff = any hasRole [Admin, Manager, StudioManager, Reception]

    ensureSchoolAccess :: AppM ()
    ensureSchoolAccess = do
      ensureModuleAccess ModuleScheduling
      unless (hasRole Teacher || isSchoolStaff) $
        liftIO $ throwIO err403

    ensureSchoolStaffAccess :: AppM ()
    ensureSchoolStaffAccess = do
      ensureSchoolAccess
      unless isSchoolStaff $
        liftIO $ throwIO err403

    ensureTeacherOrStaff :: PartyId -> AppM ()
    ensureTeacherOrStaff teacherKey = do
      ensureSchoolAccess
      unless (teacherKey == auPartyId || isSchoolStaff) $
        liftIO $ throwIO err403

    teacherOwnsStudent :: PartyId -> PartyId -> AppM Bool
    teacherOwnsStudent teacherKey studentKey = do
      hasExplicit <- recordExists
        [ TeacherStudentTeacherId ==. teacherKey
        , TeacherStudentStudentId ==. studentKey
        , TeacherStudentActive ==. True
        ]
      hasClass <- recordExists
        [ ClassSessionTeacherId ==. teacherKey
        , ClassSessionStudentId ==. studentKey
        ]
      pure (hasExplicit || hasClass)

    queueH :: Maybe Int -> Maybe Text -> AppM [TrialQueueItem]
    queueH mSubject mStatus = do
      ensureSchoolStaffAccess
      normalizedSubjectId <- either (liftIO . throwIO) pure (traverse validatePublicSubjectIdInput mSubject)
      normalizedStatus <- either (liftIO . throwIO) pure (validateOptionalTrialRequestStatusFilter mStatus)
      let filters = catMaybes
            [ (TrialRequestSubjectId ==.) . intKey <$> normalizedSubjectId
            , (TrialRequestStatus ==.) <$> normalizedStatus
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
    assignH rawRequestId rawInput = do
      ensureSchoolStaffAccess
      (requestId, TrialAssignIn{..}) <- either (liftIO . throwIO) pure (validateTrialAssignInput rawRequestId rawInput)
      let rid = intKey requestId :: Key TrialRequest
          teacherKey = intKey teacherId :: PartyId
      now <- liftIO getCurrentTime
      mReq <- get rid
      case mReq of
        Nothing  -> liftIO $ throwIO err404
        Just req -> do
          ensureTeacherSelection teacherKey
          ensureTeacherSubject teacherKey (trialRequestSubjectId req)
          update rid
            [ TrialRequestAssignedTeacherId =. Just teacherKey
            , TrialRequestAssignedAt        =. Just now
            , TrialRequestStatus            =. statusAssigned
            ]
          pure (trialRequestOut rid req { trialRequestStatus = statusAssigned })

    scheduleH :: TrialScheduleIn -> AppM TrialRequestOut
    scheduleH rawInput = do
      ensureSchoolStaffAccess
      TrialScheduleIn{..} <- either (liftIO . throwIO) pure (validateTrialScheduleInput rawInput)
      let rid       = intKey requestId :: Key TrialRequest
          teacherK  = intKey teacherId :: PartyId
          roomK     = intKey roomId    :: ResourceId
      now <- liftIO getCurrentTime
      mReq <- get rid
      case mReq of
        Nothing  -> liftIO $ throwIO err404
        Just req -> do
          ensureTeacherSelection teacherK
          ensureTeacherSubject teacherK (trialRequestSubjectId req)
          ensureSchedulableRoom roomK
          ensureRoomAllowed (trialRequestSubjectId req) roomK
          teacherFree <- teacherAvailableExceptTrialRequest teacherK startAt endAt rid
          unless teacherFree $
            liftIO $ throwIO err409 { errBody = "Profesor no disponible en ese horario" }
          roomFree <- roomAvailable roomK startAt endAt
          unless roomFree $
            liftIO $ throwIO err409 { errBody = "Sala no disponible en ese horario" }
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
      ensureSchoolAccess
      (normalizedSubjectId, normalizedFrom, normalizedTo) <-
        either (liftIO . throwIO) pure (validateAvailabilityListFilters mSubject mFrom mTo)
      let teacherKey = auPartyId
          filters =
            [ TeacherAvailabilityTeacherId ==. teacherKey ]
            ++ maybe [] (\sid -> [TeacherAvailabilitySubjectId ==. intKey sid]) normalizedSubjectId
            ++ maybe [] (\fromTs -> [TeacherAvailabilityEndAt >=. fromTs]) normalizedFrom
            ++ maybe [] (\toTs -> [TeacherAvailabilityStartAt <=. toTs]) normalizedTo
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
      ensureSchoolAccess
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
      ensureSchoolAccess
      availabilityIdValue <-
        either (liftIO . throwIO) pure (validateAvailabilityIdInput availabilityIdInt)
      let availabilityKey = intKey availabilityIdValue :: Key TeacherAvailability
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
      ensureSchoolAccess
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
      unless (key == auPartyId || isSchoolStaff || hasModuleAccess ModuleAdmin user) $
        liftIO $ throwIO err403
      pure key

    ensureSubjectExists :: Int -> AppM SubjectId
    ensureSubjectExists sidInt = do
      let key = intKey sidInt :: SubjectId
      mSubject <- get key
      case mSubject of
        Nothing -> liftIO $ throwIO err404 { errBody = "Materia no encontrada" }
        Just _  -> pure key

    ensureStudentExists :: Int -> AppM PartyId
    ensureStudentExists studentIdInt = do
      let key = intKey studentIdInt :: PartyId
      mStudent <- get key
      case mStudent of
        Nothing -> liftIO $ throwIO err404 { errBody = "Estudiante no encontrado" }
        Just _  -> pure key

    ensureSellerExists :: Int -> AppM PartyId
    ensureSellerExists sellerIdInt = do
      let key = intKey sellerIdInt :: PartyId
      mSeller <- get key
      case mSeller of
        Nothing -> liftIO $ throwIO err404 { errBody = "Vendedor no encontrado" }
        Just _  -> pure key

    ensurePackageExists :: Int -> AppM PackageCatalogId
    ensurePackageExists packageIdInt = do
      let key = intKey packageIdInt :: PackageCatalogId
      mPackage <- get key
      case mPackage of
        Nothing -> liftIO $ throwIO err404 { errBody = "Paquete no encontrado" }
        Just _  -> pure key

    ensurePurchaseTrialRequestExists :: Int -> AppM TrialRequestId
    ensurePurchaseTrialRequestExists trialRequestIdInt = do
      let key = intKey trialRequestIdInt :: TrialRequestId
      mTrialRequest <- get key
      case mTrialRequest of
        Nothing -> liftIO $ throwIO err404 { errBody = "Solicitud de prueba no encontrada" }
        Just _  -> pure key

    ensurePurchaseTrialRequestMatches
      :: PartyId
      -> Entity PackageCatalog
      -> Maybe (Entity TrialRequest)
      -> AppM ()
    ensurePurchaseTrialRequestMatches _ _ Nothing = pure ()
    ensurePurchaseTrialRequestMatches studentKey (Entity _ package) (Just (Entity _ trialRequest)) = do
      when (trialRequestPartyId trialRequest /= studentKey) $
        liftIO $
          throwIO err422
            { errBody = "La solicitud de prueba debe pertenecer al mismo estudiante que la compra" }
      when (trialRequestSubjectId trialRequest /= packageCatalogSubjectId package) $
        liftIO $
          throwIO err422
            { errBody = "La solicitud de prueba debe corresponder a la misma materia del paquete" }

    ensureBookingExists :: Int -> AppM Models.BookingId
    ensureBookingExists bookingIdInt
      | bookingIdInt <= 0 =
          liftIO $ throwIO err400 { errBody = "bookingId debe ser un entero positivo" }
      | otherwise = do
          let key = intKey bookingIdInt :: Models.BookingId
          mBooking <- get key
          case mBooking of
            Nothing -> liftIO $ throwIO err404 { errBody = "Reserva no encontrada" }
            Just _  -> pure key

    ensureTeacherSelection :: PartyId -> AppM ()
    ensureTeacherSelection teacherKey = do
      mTeacher <- get teacherKey
      case mTeacher of
        Nothing -> liftIO $ throwIO err404 { errBody = "Profesor no encontrado" }
        Just _  -> do
          hasTeacherRole <- recordExists
            [ Models.PartyRolePartyId ==. teacherKey
            , Models.PartyRoleRole ==. Teacher
            , Models.PartyRoleActive ==. True
            ]
          unless hasTeacherRole $
            liftIO $ throwIO err422 { errBody = "La persona seleccionada no está registrada como profesor" }

    ensureCommissionTeacherExists :: Int -> AppM PartyId
    ensureCommissionTeacherExists teacherIdInt = do
      let teacherKey = intKey teacherIdInt :: PartyId
      ensureTeacherSelection teacherKey
      pure teacherKey

    ensureSchedulableRoom :: ResourceId -> AppM ()
    ensureSchedulableRoom roomKey = do
      mRoom <- get roomKey
      case mRoom of
        Nothing -> liftIO $ throwIO err404 { errBody = "Sala no encontrada" }
        Just room ->
          when (Models.resourceResourceType room /= Models.Room) $
            liftIO $ throwIO err422 { errBody = "El recurso seleccionado no es una sala" }

    parseRoomKey :: Text -> AppM ResourceId
    parseRoomKey raw =
      case fromPathPiece raw of
        Nothing -> liftIO $ throwIO err400 { errBody = "Identificador de sala inválido." }
        Just key -> do
          mRoom <- get key
          case mRoom of
            Nothing -> liftIO $ throwIO err404
            Just room ->
              if Models.resourceResourceType room == Models.Room
                then pure key
                else liftIO $ throwIO err422 { errBody = "El recurso seleccionado no es una sala" }

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
      ensureSchoolStaffAccess
      normalizedSubjectId <- either (liftIO . throwIO) pure (traverse validatePublicSubjectIdInput mSubject)
      let filters = [PackageCatalogActive ==. True] ++ maybe [] (\sid -> [PackageCatalogSubjectId ==. intKey sid]) normalizedSubjectId
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
    purchaseH rawInput = do
      ensureSchoolStaffAccess
      PurchaseIn{..} <- either (liftIO . throwIO) pure (validatePurchaseInput rawInput)
      studentKey <- ensureStudentExists studentId
      packageKey <- ensurePackageExists packageId
      packageEntity <- getJustEntity packageKey
      sellerKey <- traverse ensureSellerExists sellerId
      commissionKey <- traverse ensureCommissionTeacherExists commissionedTeacherId
      trialKey <- traverse ensurePurchaseTrialRequestExists trialRequestId
      trialEntity <- traverse getJustEntity trialKey
      ensurePurchaseTrialRequestMatches studentKey packageEntity trialEntity
      now <- liftIO getCurrentTime
      let discount   = fromMaybe 0 discountCents
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
      ensureSchoolAccess
      (subjectIdFilter, teacherIdFilter, studentIdFilter, fromFilter, toFilter) <-
        either (liftIO . throwIO) pure (validateClassSessionListFilters mSubject mTeacher mStudent mFrom mTo)
      when (not isSchoolStaff) $
        case teacherIdFilter of
          Just tid | intKey tid /= auPartyId -> liftIO $ throwIO err403
          _ -> pure ()
      let filters =
            maybe [] (\sid -> [ClassSessionSubjectId ==. intKey sid]) subjectIdFilter
            ++ if isSchoolStaff
                then maybe [] (\tid -> [ClassSessionTeacherId ==. intKey tid]) teacherIdFilter
                else [ClassSessionTeacherId ==. auPartyId]
            ++ maybe [] (\pid -> [ClassSessionStudentId ==. intKey pid]) studentIdFilter
            ++ maybe [] (\startFrom -> [ClassSessionStartAt >=. startFrom]) fromFilter
            ++ maybe [] (\endTo -> [ClassSessionStartAt <=. endTo]) toFilter
      sessions <- selectList filters [Asc ClassSessionStartAt]
      dtos <- buildClassSessionDTOs sessions
      let normalized = T.toLower . T.strip
      pure $ maybe dtos (\st -> filter (\ClassSessionDTO{status = s} -> normalized s == normalized st) dtos) mStatus

    createClassH :: ClassSessionIn -> AppM ClassSessionOut
    createClassH ClassSessionIn{..} = do
      ensureSchoolAccess
      when (endAt <= startAt) $
        liftIO $ throwIO err400 { errBody = "La hora de fin debe ser mayor a la de inicio" }
      studentKey <- ensureStudentExists studentId
      subjectKey <- ensureSubjectExists subjectId
      bookingKey <- traverse ensureBookingExists bookingId
      let teacherKey = intKey teacherId
          roomKey    = intKey roomId :: ResourceId
          durationMinutes = floor (realToFrac (diffUTCTime endAt startAt) / 60 :: Double)
          isSelfTeacher = teacherKey == auPartyId
      unless (isSchoolStaff || isSelfTeacher) $
        liftIO $ throwIO err403
      ensureTeacherSelection teacherKey
      ensureSchedulableRoom roomKey
      when (isSelfTeacher && not isSchoolStaff) $ do
        ensureTeacherSubject teacherKey subjectKey
        ownsStudent <- teacherOwnsStudent teacherKey studentKey
        unless ownsStudent $
          liftIO $ throwIO err403
      ensureRoomAllowed subjectKey roomKey
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
      ensureSchoolAccess
      let cid = intKey classId :: Key ClassSession
      mSession <- get cid
      case mSession of
        Nothing -> liftIO $ throwIO err404
        Just sess -> do
          let sessionTeacher = Trials.classSessionTeacherId sess
              isSelfTeacher = sessionTeacher == auPartyId
          unless (isSchoolStaff || isSelfTeacher) $
            liftIO $ throwIO err403
          when (not isSchoolStaff) $
            case teacherId of
              Nothing -> pure ()
              Just tid | intKey tid /= auPartyId -> liftIO $ throwIO err403
              Just _ -> pure ()
          newStudent <- maybe (pure (Trials.classSessionStudentId sess)) ensureStudentExists studentId
          newSubject <- maybe (pure (Trials.classSessionSubjectId sess)) ensureSubjectExists subjectId
          newBooking <- traverse ensureBookingExists bookingId
          let newStart   = fromMaybe (Trials.classSessionStartAt sess) startAt
              newEnd     = fromMaybe (Trials.classSessionEndAt sess) endAt
              newTeacher = maybe (Trials.classSessionTeacherId sess) intKey teacherId
              newRoom    = maybe (Trials.classSessionRoomId sess) intKey roomId
          when (newEnd <= newStart) $
            liftIO $ throwIO err400 { errBody = "La hora de fin debe ser mayor a la de inicio" }
          ensureTeacherSelection newTeacher
          ensureSchedulableRoom newRoom
          when (isSelfTeacher && not isSchoolStaff) $ do
            ensureTeacherSubject newTeacher newSubject
            ownsStudent <- teacherOwnsStudent newTeacher newStudent
            unless ownsStudent $
              liftIO $ throwIO err403
          ensureRoomAllowed newSubject newRoom
          teacherFree <- teacherAvailableExceptClassSession newTeacher newStart newEnd cid
          unless teacherFree $
            liftIO $ throwIO err409 { errBody = "Profesor no disponible en ese horario" }
          roomFree <- roomAvailableExceptClassSession newRoom newStart newEnd cid
          unless roomFree $
            liftIO $ throwIO err409 { errBody = "Sala no disponible en ese horario" }
          let updates = concat
                [ maybe [] (\tid -> [ClassSessionTeacherId =. intKey tid]) teacherId
                , maybe [] (\sid -> [ClassSessionSubjectId =. intKey sid]) subjectId
                , maybe [] (\pid -> [ClassSessionStudentId =. intKey pid]) studentId
                , maybe [] (\v   -> [ClassSessionStartAt   =. v])         startAt
                , maybe [] (\v   -> [ClassSessionEndAt     =. v])         endAt
                , maybe [] (\rid -> [ClassSessionRoomId    =. intKey rid]) roomId
                , maybe [] (\bid -> [ClassSessionBookingId =. Just bid])  newBooking
                , maybe [] (\txt -> [ClassSessionNotes     =. Just txt])  notes
                ]
          unless (null updates) $
            update cid updates
          ent <- getJustEntity cid
          dtos <- buildClassSessionDTOs [ent]
          case listToMaybe dtos of
            Just dto -> pure dto
            Nothing -> liftIO $ throwIO err500

    attendH :: Int -> AttendIn -> AppM ClassSessionOut
    attendH classId AttendIn{..} = do
      ensureSchoolAccess
      let cid = intKey classId :: Key ClassSession
      mSession <- get cid
      case mSession of
        Nothing -> liftIO $ throwIO err404
        Just sess -> do
          unless (isSchoolStaff || Trials.classSessionTeacherId sess == auPartyId) $
            liftIO $ throwIO err403
          let duration = classSessionConsumedMinutes sess
          update cid
            [ ClassSessionAttended =. attended
            , ClassSessionNotes    =. notes
            ]
          pure (ClassSessionOut (entityKeyInt cid) duration)

    commissionsH :: Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> AppM [CommissionDTO]
    commissionsH mFrom mTo mTeacher = do
      ensureSchoolStaffAccess
      (fromFilter, toFilter, teacherFilter) <-
        either (liftIO . throwIO) pure (validateCommissionListFilters mFrom mTo mTeacher)
      let baseFilters = catMaybes
            [ (CommissionRecognizedAt >=.) <$> fromFilter
            , (CommissionRecognizedAt <=.) <$> toFilter
            , (CommissionTeacherId ==.) . intKey <$> teacherFilter
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
      ensureSchoolAccess
      -- Show any party that has the Teacher role, regardless of the active flag on the PartyRole entry.
      teacherRoles <- selectList [Models.PartyRoleRole ==. Teacher] []
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
    teacherClassesH rawTeacherId rawSubjectId rawFrom rawTo = do
      ensureSchoolAccess
      (teacherId, mSubject, mFrom, mTo) <-
        either (liftIO . throwIO) pure (validateTeacherClassesFilters rawTeacherId rawSubjectId rawFrom rawTo)
      let teacherKey = intKey teacherId :: PartyId
          filters =
            [ClassSessionTeacherId ==. teacherKey]
              ++ maybe [] (\sid -> [ClassSessionSubjectId ==. intKey sid]) mSubject
              ++ maybe [] (\startFrom -> [ClassSessionStartAt >=. startFrom]) mFrom
              ++ maybe [] (\endTo -> [ClassSessionStartAt <=. endTo]) mTo
      ensureTeacherOrStaff teacherKey
      sessions <- selectList filters [Asc ClassSessionStartAt]
      buildClassSessionDTOs sessions

    teacherSubjectsUpdateH :: Int -> TeacherSubjectsUpdate -> AppM TeacherDTO
    teacherSubjectsUpdateH teacherId TeacherSubjectsUpdate{..} = do
      ensureSchoolAccess
      validatedSubjectIds <- either (liftIO . throwIO) pure (validateTeacherSubjectIdsInput subjectIds)
      let teacherKey = intKey teacherId :: PartyId
      ensureTeacherOrStaff teacherKey
      mTeacher <- get teacherKey
      case mTeacher of
        Nothing -> liftIO $ throwIO err404
        Just party -> do
          subjectEntities <- if null validatedSubjectIds
            then pure []
            else selectList
              ( [SubjectId <-. map intKey validatedSubjectIds]
                ++ if isSchoolStaff then [] else [SubjectActive ==. True]
              )
              []
          when (not (null validatedSubjectIds) && length subjectEntities /= length validatedSubjectIds) $
            liftIO $ throwIO err404 { errBody = "Una o más materias no existen." }

          existingLinks <- selectList [TeacherSubjectTeacherId ==. teacherKey] []
          let existingIds = map (teacherSubjectSubjectId . entityVal) existingLinks
              desiredKeys = map entityKey subjectEntities
              toAdd = filter (`notElem` existingIds) desiredKeys
              toRemove = filter (`notElem` desiredKeys) existingIds

          unless (null toRemove) $
            deleteWhere [ TeacherSubjectTeacherId ==. teacherKey
                        , TeacherSubjectSubjectId <-. toRemove
                        ]

          forM_ toAdd $ \sid ->
            void $ insertUnique TeacherSubject
              { teacherSubjectTeacherId = teacherKey
              , teacherSubjectSubjectId = sid
              , teacherSubjectLevelMin  = Nothing
              , teacherSubjectLevelMax  = Nothing
              }

          when (not (null desiredKeys) || not (null existingIds)) $
            void $ upsert (PartyRole teacherKey Teacher True) [Models.PartyRoleActive =. True]

          let subjectMap = Map.fromList [ (entityKey s, entityVal s) | s <- subjectEntities ]
              subjectsDTO =
                [ SubjectBriefDTO
                    { subjectId = entityKeyInt sid
                    , name      = Trials.subjectName subj
                    }
                | (sid, subj) <- Map.toList subjectMap
                ]
          pure TeacherDTO
            { teacherId = teacherId
            , teacherName = partyDisplayName party
            , subjects = subjectsDTO
            }

    teacherStudentsListH :: Int -> AppM [StudentDTO]
    teacherStudentsListH teacherId = do
      ensureSchoolAccess
      let teacherKey = intKey teacherId :: PartyId
      ensureTeacherOrStaff teacherKey
      ids <- teacherStudentIdsFor teacherKey
      studentsByIds ids

    teacherStudentsAddH :: Int -> TeacherStudentLinkIn -> AppM NoContent
    teacherStudentsAddH teacherId TeacherStudentLinkIn{..} = do
      ensureSchoolAccess
      now <- liftIO getCurrentTime
      let teacherKey = intKey teacherId :: PartyId
          studentKey = intKey studentId :: PartyId
      ensureTeacherOrStaff teacherKey
      mTeacher <- get teacherKey
      when (isNothing mTeacher) $
        liftIO $ throwIO err404
      mStudent <- get studentKey
      when (isNothing mStudent) $
        liftIO $ throwIO err404
      void $ upsert (PartyRole studentKey Student True) [Models.PartyRoleActive =. True]
      void $ upsert (TeacherStudent teacherKey studentKey True now) [TeacherStudentActive =. True]
      pure NoContent

    teacherStudentsDeleteH :: Int -> Int -> AppM NoContent
    teacherStudentsDeleteH teacherId studentId = do
      ensureSchoolAccess
      let teacherKey = intKey teacherId :: PartyId
          studentKey = intKey studentId :: PartyId
      ensureTeacherOrStaff teacherKey
      mLink <- getBy (UniqueTeacherStudent teacherKey studentKey)
      case mLink of
        Nothing -> liftIO $ throwIO err404
        Just (Entity linkId _) -> do
          update linkId [TeacherStudentActive =. False]
          pure NoContent

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
      ensureSchoolAccess
      if isSchoolStaff
        then do
          studentRoles <- selectList [Models.PartyRoleRole ==. Student, Models.PartyRoleActive ==. True] []
          let ids = map (Models.partyRolePartyId . entityVal) studentRoles
          studentsByIds ids
        else do
          ids <- teacherStudentIdsFor auPartyId
          studentsByIds ids

    studentCreateH :: StudentCreate -> AppM StudentDTO
    studentCreateH StudentCreate{..} = do
      ensureSchoolAccess
      let fullNameVal = T.strip fullName
      when (T.null fullNameVal) $
        liftIO $ throwIO err400 { errBody = "El nombre es obligatorio." }
      now <- liftIO getCurrentTime
      partyId <- createOrFetchParty (Just fullNameVal) (Just email) phone now
      void $ upsert (PartyRole partyId Student True) [Models.PartyRoleActive =. True]
      unless isSchoolStaff $
        void $ upsert (TeacherStudent auPartyId partyId True now) [TeacherStudentActive =. True]
      when (isJust notes) $
        update partyId [Models.PartyNotes =. fmap T.strip notes]
      Entity _ party <- getJustEntity partyId
      pure StudentDTO
        { studentId   = entityKeyInt partyId
        , displayName = Models.partyDisplayName party
        , email       = Models.partyPrimaryEmail party
        , phone       = Models.partyPrimaryPhone party
        }

    studentUpdateH :: Int -> StudentUpdate -> AppM StudentDTO
    studentUpdateH studentIdInt StudentUpdate{..} = do
      ensureSchoolAccess
      let studentKey = intKey studentIdInt :: PartyId
      mParty <- get studentKey
      case mParty of
        Nothing -> liftIO $ throwIO err404
        Just _ -> pure ()

      unless isSchoolStaff $ do
        owns <- teacherOwnsStudent auPartyId studentKey
        unless owns $
          liftIO $ throwIO err403

      let nameUpdate = displayName >>= (\txt -> let t = T.strip txt in if T.null t then Nothing else Just t)
          notesUpdate = case notes of
            Nothing -> Nothing
            Just raw -> Just (cleanOptional (Just raw))
      emailUpdate <- either (liftIO . throwIO) pure (validateEmailUpdate email)
      phoneUpdate <- either (liftIO . throwIO) pure (validateOptionalPhone phone)
      ensureEmailAvailableForParty studentKey emailUpdate

      when (isJust displayName && isNothing nameUpdate) $
        liftIO $ throwIO err400 { errBody = "El nombre es obligatorio." }

      let updates =
            maybe [] (\emailVal -> [Models.PartyPrimaryEmail =. emailVal]) emailUpdate
            <> catMaybes
              [ (Models.PartyDisplayName =.) <$> nameUpdate
              , if isJust phone then Just (Models.PartyPrimaryPhone =. phoneUpdate) else Nothing
              , (Models.PartyNotes =.) <$> notesUpdate
              ]

      unless (null updates) $
        update studentKey updates

      Entity _ fresh <- getJustEntity studentKey
      pure StudentDTO
        { studentId   = studentIdInt
        , displayName = Models.partyDisplayName fresh
        , email       = Models.partyPrimaryEmail fresh
        , phone       = Models.partyPrimaryPhone fresh
        }

    teacherStudentIdsFor :: PartyId -> AppM [PartyId]
    teacherStudentIdsFor teacherKey = do
      explicit <- selectList
        [ TeacherStudentTeacherId ==. teacherKey
        , TeacherStudentActive ==. True
        ] []
      classLinks <- selectList [ClassSessionTeacherId ==. teacherKey] []
      let explicitIds = map (teacherStudentStudentId . entityVal) explicit
          classIds = map (Trials.classSessionStudentId . entityVal) classLinks
      pure (distinct (explicitIds ++ classIds))

    studentsByIds :: [PartyId] -> AppM [StudentDTO]
    studentsByIds ids = do
      parties <- if null ids
        then pure []
        else selectList [Models.PartyId <-. distinct ids] []
      pure
        [ StudentDTO
            { studentId   = entityKeyInt pid
            , displayName = Models.partyDisplayName party
            , email       = Models.partyPrimaryEmail party
            , phone       = Models.partyPrimaryPhone party
            }
        | Entity pid party <- parties
        ]


trialsServer :: ConnectionPool -> Server TrialsAPI
trialsServer pool =
  let trialsProxy = Proxy :: Proxy TrialsAPI
      ctxProxy    = Proxy :: Proxy '[AuthHandler Request AuthedUser]
      server      = publicTrialsServer :<|> authedPrivateServer
  in hoistServerWithContext trialsProxy ctxProxy nt server
  where
    nt :: AppM a -> Handler a
    nt x = do
      result <- liftIO (runTrialsAction x)
      either throwError pure result

    runTrialsAction :: AppM a -> IO (Either ServerError a)
    runTrialsAction action = try (runSqlPool action pool)

    authedPrivateServer :: AuthedUser -> ServerT PrivateTrialsAPI AppM
    authedPrivateServer user = privateTrialsServer user
