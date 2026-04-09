{-# LANGUAGE OverloadedStrings #-}

module TDF.Trials.PublicLeadSpec (spec) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (..), getBy, getJustEntity, insert, selectList, (==.))
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Servant (ServerError (errBody, errHTTPCode), (:<|>) ((:<|>)))
import Test.Hspec

import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.Trials.DTO (PreferredSlot (..), TrialRequestOut (..), TrialScheduleIn (..))
import TDF.Trials.API (InterestIn (..))
import TDF.Trials.Server
  ( createOrFetchParty
  , ensurePublicLeadParty
  , privateTrialsServer
  , validatePreferredSlots
  , validatePreferredSlotsAt
  , validatePublicInterestInput
  , validatePublicSubjectIdInput
  , validatePublicSubjectSelection
  , validatePublicTrialPartyId
  , validateTrialScheduleInput
  )
import qualified TDF.Models as Models
import TDF.Trials.Models (Subject (..))
import qualified TDF.Trials.Models as Trials

spec :: Spec
spec = do
  describe "Public trials lead party resolution" $ do
    it "creates/reuses signup party by email" $ do
      (firstId, secondId, storedEmail, storedName, storedPhone) <- runInMemory $ do
        now <- liftIO getCurrentTime
        firstId <- createOrFetchParty (Just "Test User") (Just " User@Example.com ") (Just "+593 99 123 4567") now
        secondId <- createOrFetchParty (Just "Another Name") (Just "user@example.com") Nothing now
        Entity _ party <- getJustEntity firstId
        pure (firstId, secondId, Models.partyPrimaryEmail party, Models.partyDisplayName party, Models.partyPrimaryPhone party)

      firstId `shouldBe` secondId
      storedEmail `shouldBe` Just "user@example.com"
      storedName `shouldBe` "Test User"
      storedPhone `shouldBe` Just "+593991234567"

    it "falls back to the normalized email when the provided name is blank" $ do
      storedName <- runInMemory $ do
        now <- liftIO getCurrentTime
        partyId <- createOrFetchParty (Just "   ") (Just " Student@Example.com ") Nothing now
        Models.partyDisplayName . entityVal <$> getJustEntity partyId

      storedName `shouldBe` "student@example.com"

    it "rejects explicitly invalid nonblank phones instead of silently discarding them" $ do
      result <- tryCreateOrFetchParty (Just "Test User") (Just "user@example.com") (Just "---")
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "phone"
        Right _ ->
          expectationFailure "Expected invalid phone input to be rejected"

    it "rejects phone numbers that are too short or too long to be actionable contacts" $ do
      let assertRejected rawPhone = do
            result <- tryCreateOrFetchParty (Just "Test User") (Just "user@example.com") (Just rawPhone)
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "phone"
              Right _ ->
                expectationFailure ("Expected implausible phone input to be rejected: " <> show rawPhone)
      assertRejected "12345"
      assertRejected "+1234567890123456"

    it "rejects malformed emails instead of creating unusable parties" $ do
      let assertRejected rawEmail = do
            result <- tryCreateOrFetchParty (Just "Test User") (Just rawEmail) Nothing
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "email"
              Right _ ->
                expectationFailure ("Expected invalid email input to be rejected: " <> show rawEmail)
      assertRejected "not-an-email"
      assertRejected "user@example..com"
      assertRejected "user@-example.com"
      assertRejected "user@example-.com"

    it "rejects free-form text that merely contains digits instead of extracting a misleading partial phone" $ do
      result <- tryCreateOrFetchParty (Just "Test User") (Just "user@example.com") (Just "call me at 099 123 4567")
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "phone"
        Right _ ->
          expectationFailure "Expected mixed text phone input to be rejected"

    it "keeps a single fallback party for anonymous interests" $ do
      (firstId, secondId, total) <- runInMemory $ do
        now <- liftIO getCurrentTime
        firstId <- ensurePublicLeadParty now
        secondId <- ensurePublicLeadParty now
        rows <- selectList [Models.PartyPrimaryEmail ==. Just "public-interest@tdf.local"] []
        pure (firstId, secondId, length rows)

      firstId `shouldBe` secondId
      total `shouldBe` 1

  describe "validatePublicTrialPartyId" $ do
    it "accepts requests that omit partyId" $
      validatePublicTrialPartyId Nothing `shouldBe` Right ()

    it "rejects caller-supplied party ids on the public endpoint" $
      case validatePublicTrialPartyId (Just 42) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "partyId is not allowed on public trial requests"
        Right _ ->
          expectationFailure "Expected public partyId to be rejected"

  describe "validatePublicInterestInput" $ do
    it "rejects blank interest types instead of creating unusable anonymous lead rows" $
      case validatePublicInterestInput (InterestIn "   " Nothing (Just "Looking for info") (Just "https://example.com")) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "interestType is required"
        Right _ ->
          expectationFailure "Expected blank interest type to be rejected"

    it "trims interest types and drops blank optional fields" $
      case validatePublicInterestInput (InterestIn "  workshop  " (Just 7) (Just "   ") (Just "  https://example.com/file  ")) of
        Left err ->
          expectationFailure ("Expected valid interest input to be accepted, got " <> show err)
        Right (InterestIn interestTypeValue subjectIdValue detailsValue driveLinkValue) -> do
          interestTypeValue `shouldBe` "workshop"
          subjectIdValue `shouldBe` Just 7
          detailsValue `shouldBe` Nothing
          driveLinkValue `shouldBe` Just "https://example.com/file"

    it "rejects non-positive subject ids instead of treating them as unavailable subjects" $
      case validatePublicInterestInput (InterestIn "workshop" (Just 0) Nothing Nothing) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "subjectId must be a positive integer"
        Right _ ->
          expectationFailure "Expected invalid subjectId to be rejected"

    it "rejects malformed drive links instead of storing ambiguous free-form text" $ do
      let assertRejected rawDriveLink =
            case validatePublicInterestInput (InterestIn "workshop" Nothing Nothing (Just rawDriveLink)) of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "driveLink must be an absolute http(s) URL"
              Right _ ->
                expectationFailure "Expected invalid driveLink to be rejected"
      assertRejected "folder-123"
      assertRejected "https://example.com/shared file"

  describe "validatePublicSubjectIdInput" $ do
    it "accepts positive subject ids" $
      validatePublicSubjectIdInput 7 `shouldBe` Right 7

    it "rejects zero or negative subject ids before database lookup" $ do
      let assertRejected rawSubjectId =
            case validatePublicSubjectIdInput rawSubjectId of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "subjectId must be a positive integer"
              Right value ->
                expectationFailure ("Expected invalid subjectId to be rejected, got " <> show value)
      assertRejected 0
      assertRejected (-3)

  describe "validatePublicSubjectSelection" $ do
    it "accepts active public subjects" $
      validatePublicSubjectSelection (Just (Subject "Piano" True)) `shouldBe` Right ()

    it "rejects missing or inactive subjects instead of reporting a misleading availability error" $ do
      let assertRejected candidate =
            case validatePublicSubjectSelection candidate of
              Left err -> do
                errHTTPCode err `shouldBe` 422
                BL8.unpack (errBody err) `shouldContain` "La materia solicitada no está disponible"
              Right value ->
                expectationFailure ("Expected unavailable public subject to be rejected, got " <> show value)
      assertRejected Nothing
      assertRejected (Just (Subject "Piano" False))

  describe "validatePreferredSlots" $ do
    it "rejects requests with more than three preferred slots" $ do
      let slots = replicate 4 validSlot
      case validatePreferredSlots slots of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "At most three preferred slots are allowed"
        Right _ ->
          expectationFailure "Expected too many slots to be rejected"

    it "rejects slots whose endAt is not after startAt" $ do
      let reversedSlot = PreferredSlot slotEnd slotStart
      case validatePreferredSlots [reversedSlot] of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Preferred slot endAt must be after startAt"
        Right _ ->
          expectationFailure "Expected reversed slot to be rejected"

    it "rejects overlapping or duplicate preferred slots" $ do
      let overlappingSlot = PreferredSlot (addUTCTime 1800 slotStart) (addUTCTime 5400 slotStart)
          assertRejected slots =
            case validatePreferredSlots slots of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "Preferred slots must be distinct non-overlapping windows"
              Right value ->
                expectationFailure ("Expected overlapping slots to be rejected, got " <> show value)
      assertRejected [validSlot, overlappingSlot]
      assertRejected [validSlot, validSlot]

    it "rejects preferred slots that start in the past before creating unschedulable requests" $ do
      let now = addUTCTime 7200 slotStart
      case validatePreferredSlotsAt now [validSlot] of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Preferred slots must start in the future"
        Right value ->
          expectationFailure ("Expected past preferred slot to be rejected, got " <> show value)

    it "preserves valid slots without truncation or mutation" $
      validatePreferredSlots [validSlot, laterValidSlot] `shouldBe` Right [validSlot, laterValidSlot]

  describe "validateTrialScheduleInput" $ do
    let validSchedule = TrialScheduleIn 1 2 slotStart slotEnd 3

    it "accepts positive identifiers and a strictly increasing schedule window" $
      case validateTrialScheduleInput validSchedule of
        Left err ->
          expectationFailure ("Expected valid schedule input to be accepted, got " <> show err)
        Right (TrialScheduleIn requestIdValue teacherIdValue startValue endValue roomIdValue) -> do
          requestIdValue `shouldBe` 1
          teacherIdValue `shouldBe` 2
          startValue `shouldBe` slotStart
          endValue `shouldBe` slotEnd
          roomIdValue `shouldBe` 3

    it "rejects malformed identifiers and impossible time ranges before scheduling a trial" $ do
      let assertInvalid expectedMessage result =
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected invalid schedule input to be rejected, got " <> show value)
      assertInvalid "requestId must be a positive integer" $
        validateTrialScheduleInput (TrialScheduleIn 0 2 slotStart slotEnd 3)
      assertInvalid "teacherId must be a positive integer" $
        validateTrialScheduleInput (TrialScheduleIn 1 (-1) slotStart slotEnd 3)
      assertInvalid "roomId must be a positive integer" $
        validateTrialScheduleInput (TrialScheduleIn 1 2 slotStart slotEnd 0)
      assertInvalid "La hora de fin debe ser mayor a la de inicio" $
        validateTrialScheduleInput (TrialScheduleIn 1 2 slotStart slotStart 3)

  describe "private trial scheduling" $ do
    it "rejects scheduling a trial when the teacher is already booked in an overlapping class" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        teacherPartyId <- insertPartyFixture "Teacher One" now
        studentPartyId <- insertPartyFixture "Student One" now
        otherStudentPartyId <- insertPartyFixture "Student Two" now
        roomResourceId <- insertRoomFixture "Sala A" "sala-a"
        subjectKey <- insert (Subject "Piano" True)
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        _ <- insert Trials.ClassSession
          { Trials.classSessionStudentId = otherStudentPartyId
          , Trials.classSessionTeacherId = teacherPartyId
          , Trials.classSessionSubjectId = subjectKey
          , Trials.classSessionStartAt = addUTCTime 900 scheduleStart
          , Trials.classSessionEndAt = addUTCTime 900 scheduleEnd
          , Trials.classSessionRoomId = roomResourceId
          , Trials.classSessionBookingId = Nothing
          , Trials.classSessionAttended = False
          , Trials.classSessionPurchaseId = Nothing
          , Trials.classSessionConsumedMinutes = 60
          , Trials.classSessionNotes = Nothing
          }
        privateScheduleHandler
          (TrialScheduleIn
            (fromIntegral (fromSqlKey requestKey))
            (fromIntegral (fromSqlKey teacherPartyId))
            scheduleStart
            scheduleEnd
            (fromIntegral (fromSqlKey roomResourceId)))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Profesor no disponible en ese horario"
        Right _ ->
          expectationFailure "Expected overlapping teacher schedule to be rejected"

    it "allows rescheduling the same trial request without conflicting with its own existing assignment" $ do
      (response, assignment, newStart, newEnd) <- runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let oldStart = addUTCTime 3600 now
            oldEnd = addUTCTime 7200 now
            newStart = addUTCTime 5400 now
            newEnd = addUTCTime 9000 now
        teacherPartyId <- insertPartyFixture "Teacher One" now
        studentPartyId <- insertPartyFixture "Student One" now
        roomResourceId <- insertRoomFixture "Sala A" "sala-a"
        subjectKey <- insert (Subject "Piano" True)
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey oldStart oldEnd now
        _ <- insert Trials.TrialAssignment
          { Trials.trialAssignmentRequestId = requestKey
          , Trials.trialAssignmentTeacherId = teacherPartyId
          , Trials.trialAssignmentStartAt = oldStart
          , Trials.trialAssignmentEndAt = oldEnd
          , Trials.trialAssignmentRoomId = roomResourceId
          , Trials.trialAssignmentBookingId = Nothing
          , Trials.trialAssignmentCreatedAt = now
          }
        response <- privateScheduleHandler
          (TrialScheduleIn
            (fromIntegral (fromSqlKey requestKey))
            (fromIntegral (fromSqlKey teacherPartyId))
            newStart
            newEnd
            (fromIntegral (fromSqlKey roomResourceId)))
        mAssignment <- getBy (Trials.UniqueTrialAssignmentRequest requestKey)
        pure (response, fmap entityVal mAssignment, newStart, newEnd)

      status response `shouldBe` "Scheduled"
      case assignment of
        Nothing ->
          expectationFailure "Expected the trial assignment to remain present after rescheduling"
        Just storedAssignment -> do
          Trials.trialAssignmentStartAt storedAssignment `shouldBe` newStart
          Trials.trialAssignmentEndAt storedAssignment `shouldBe` newEnd

runInMemory :: SqlPersistT IO a -> IO a
runInMemory action =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializePartySchema pool
    liftIO $ runSqlPool action pool

tryCreateOrFetchParty
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> IO (Either ServerError Models.PartyId)
tryCreateOrFetchParty mName mEmail mPhone =
  try $ runInMemory $ do
    now <- liftIO getCurrentTime
    createOrFetchParty mName mEmail mPhone now

initializePartySchema :: (MonadIO m) => SqlPersistT m ()
initializePartySchema = do
  rawExecute "PRAGMA foreign_keys = ON" []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"party\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"legal_name\" VARCHAR NULL,\
    \\"display_name\" VARCHAR NOT NULL,\
    \\"is_org\" BOOLEAN NOT NULL,\
    \\"tax_id\" VARCHAR NULL,\
    \\"primary_email\" VARCHAR NULL,\
    \\"primary_phone\" VARCHAR NULL,\
    \\"whatsapp\" VARCHAR NULL,\
    \\"instagram\" VARCHAR NULL,\
    \\"emergency_contact\" VARCHAR NULL,\
    \\"notes\" VARCHAR NULL,\
    \\"created_at\" TIMESTAMP NOT NULL\
    \)"
    []

runTrialsInMemory :: SqlPersistT IO a -> IO a
runTrialsInMemory action =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeTrialsSchema pool
    liftIO $ runSqlPool action pool

initializeTrialsSchema :: (MonadIO m) => SqlPersistT m ()
initializeTrialsSchema = do
  rawExecute "PRAGMA foreign_keys = ON" []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"party\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"legal_name\" VARCHAR NULL,\
    \\"display_name\" VARCHAR NOT NULL,\
    \\"is_org\" BOOLEAN NOT NULL,\
    \\"tax_id\" VARCHAR NULL,\
    \\"primary_email\" VARCHAR NULL,\
    \\"primary_phone\" VARCHAR NULL,\
    \\"whatsapp\" VARCHAR NULL,\
    \\"instagram\" VARCHAR NULL,\
    \\"emergency_contact\" VARCHAR NULL,\
    \\"notes\" VARCHAR NULL,\
    \\"created_at\" TIMESTAMP NOT NULL\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"resource\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"name\" VARCHAR NOT NULL,\
    \\"slug\" VARCHAR NOT NULL,\
    \\"resource_type\" VARCHAR NOT NULL,\
    \\"capacity\" INTEGER NULL,\
    \\"active\" BOOLEAN NOT NULL,\
    \CONSTRAINT \"unique_resource_slug\" UNIQUE (\"slug\")\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"subject\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"name\" VARCHAR NOT NULL,\
    \\"active\" BOOLEAN NOT NULL,\
    \CONSTRAINT \"unique_subject_name\" UNIQUE (\"name\")\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"trial_request\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"party_id\" INTEGER NOT NULL,\
    \\"subject_id\" INTEGER NOT NULL,\
    \\"pref1_start\" TIMESTAMP NOT NULL,\
    \\"pref1_end\" TIMESTAMP NOT NULL,\
    \\"pref2_start\" TIMESTAMP NULL,\
    \\"pref2_end\" TIMESTAMP NULL,\
    \\"pref3_start\" TIMESTAMP NULL,\
    \\"pref3_end\" TIMESTAMP NULL,\
    \\"notes\" VARCHAR NULL,\
    \\"status\" VARCHAR NOT NULL,\
    \\"assigned_teacher_id\" INTEGER NULL,\
    \\"assigned_at\" TIMESTAMP NULL,\
    \\"created_at\" TIMESTAMP NOT NULL\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"trial_assignment\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"request_id\" INTEGER NOT NULL,\
    \\"teacher_id\" INTEGER NOT NULL,\
    \\"start_at\" TIMESTAMP NOT NULL,\
    \\"end_at\" TIMESTAMP NOT NULL,\
    \\"room_id\" INTEGER NOT NULL,\
    \\"booking_id\" INTEGER NULL,\
    \\"created_at\" TIMESTAMP NOT NULL,\
    \CONSTRAINT \"unique_trial_assignment_request\" UNIQUE (\"request_id\")\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"class_session\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"student_id\" INTEGER NOT NULL,\
    \\"teacher_id\" INTEGER NOT NULL,\
    \\"subject_id\" INTEGER NOT NULL,\
    \\"start_at\" TIMESTAMP NOT NULL,\
    \\"end_at\" TIMESTAMP NOT NULL,\
    \\"room_id\" INTEGER NOT NULL,\
    \\"booking_id\" INTEGER NULL,\
    \\"attended\" BOOLEAN NOT NULL,\
    \\"purchase_id\" INTEGER NULL,\
    \\"consumed_minutes\" INTEGER NOT NULL,\
    \\"notes\" VARCHAR NULL\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"teacher_availability\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"teacher_id\" INTEGER NOT NULL,\
    \\"subject_id\" INTEGER NOT NULL,\
    \\"room_id\" INTEGER NOT NULL,\
    \\"start_at\" TIMESTAMP NOT NULL,\
    \\"end_at\" TIMESTAMP NOT NULL,\
    \\"notes\" VARCHAR NULL,\
    \\"created_at\" TIMESTAMP NOT NULL\
    \)"
    []

adminUser :: AuthedUser
adminUser =
  let roles = [Models.Admin]
  in AuthedUser
      { auPartyId = toSqlKey 999
      , auRoles = roles
      , auModules = modulesForRoles roles
      }

privateScheduleHandler :: TrialScheduleIn -> SqlPersistT IO TrialRequestOut
privateScheduleHandler =
  let _ :<|> _ :<|> scheduleH :<|> _ = privateTrialsServer adminUser
  in scheduleH

insertPartyFixture :: Text -> UTCTime -> SqlPersistT IO Models.PartyId
insertPartyFixture displayName now =
  insert Models.Party
    { Models.partyLegalName = Nothing
    , Models.partyDisplayName = displayName
    , Models.partyIsOrg = False
    , Models.partyTaxId = Nothing
    , Models.partyPrimaryEmail = Nothing
    , Models.partyPrimaryPhone = Nothing
    , Models.partyWhatsapp = Nothing
    , Models.partyInstagram = Nothing
    , Models.partyEmergencyContact = Nothing
    , Models.partyNotes = Nothing
    , Models.partyCreatedAt = now
    }

insertRoomFixture :: Text -> Text -> SqlPersistT IO Models.ResourceId
insertRoomFixture name slug =
  insert Models.Resource
    { Models.resourceName = name
    , Models.resourceSlug = slug
    , Models.resourceResourceType = Models.Room
    , Models.resourceCapacity = Nothing
    , Models.resourceActive = True
    }

insertTrialRequestFixture
  :: Models.PartyId
  -> Trials.SubjectId
  -> UTCTime
  -> UTCTime
  -> UTCTime
  -> SqlPersistT IO Trials.TrialRequestId
insertTrialRequestFixture partyId subjectKey slotStartAt slotEndAt now =
  insert Trials.TrialRequest
    { Trials.trialRequestPartyId = partyId
    , Trials.trialRequestSubjectId = subjectKey
    , Trials.trialRequestPref1Start = slotStartAt
    , Trials.trialRequestPref1End = slotEndAt
    , Trials.trialRequestPref2Start = Nothing
    , Trials.trialRequestPref2End = Nothing
    , Trials.trialRequestPref3Start = Nothing
    , Trials.trialRequestPref3End = Nothing
    , Trials.trialRequestNotes = Nothing
    , Trials.trialRequestStatus = "Requested"
    , Trials.trialRequestAssignedTeacherId = Nothing
    , Trials.trialRequestAssignedAt = Nothing
    , Trials.trialRequestCreatedAt = now
    }

slotStart :: UTCTime
slotStart = UTCTime (fromGregorian 2026 4 1) (secondsToDiffTime 36000)

slotEnd :: UTCTime
slotEnd = addUTCTime 3600 slotStart

validSlot :: PreferredSlot
validSlot = PreferredSlot slotStart slotEnd

laterValidSlot :: PreferredSlot
laterValidSlot =
  let laterStart = addUTCTime 7200 slotStart
  in PreferredSlot laterStart (addUTCTime 3600 laterStart)
