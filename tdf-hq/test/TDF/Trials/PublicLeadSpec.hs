{-# LANGUAGE OverloadedStrings #-}

module TDF.Trials.PublicLeadSpec (spec) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (isLeft)
import Data.Text (Text, pack)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (..), getBy, getJustEntity, insert, selectList, (==.))
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Servant (NoContent, ServerError (errBody, errHTTPCode), (:<|>) ((:<|>)))
import Servant.Server.Internal.Handler (runHandler)
import Test.Hspec

import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.Trials.DTO
  ( ClassSessionDTO
  , ClassSessionUpdate (..)
  , PreferredSlot (..)
  , StudentCreate (StudentCreate)
  , StudentDTO
  , StudentUpdate (StudentUpdate)
  , TeacherStudentLinkIn (..)
  , TeacherSubjectsUpdate (..)
  , TrialRequestIn (TrialRequestIn)
  , TrialAvailabilityUpsert (..)
  , TrialAvailabilitySlotDTO
  , TrialAssignIn (..)
  , TrialRequestOut (..)
  , TrialScheduleIn (..)
  )
import TDF.Trials.API
  ( AttendIn (..)
  , ClassSessionIn (..)
  , ClassSessionOut
  , InterestIn (..)
  , PackageDTO
  , PurchaseIn (..)
  , PurchaseOut
  , SignupIn (..)
  , SubjectCreate (SubjectCreate)
  , SubjectUpdate (..)
  , TrialQueueItem
  )
import TDF.Trials.Server
  ( buildTrialUsernameCandidate
  , createOrFetchParty
  , ensurePublicLeadParty
  , privateTrialsServer
  , trialsServer
  , validateAvailabilityIdInput
  , validateEmailUpdate
  , validateOptionalTrialRequestStatusFilter
  , validatePurchaseInput
  , validatePreferredSlots
  , validatePreferredSlotsAt
  , validatePublicInterestInput
  , validatePublicSignupInput
  , validatePublicSubjectIdInput
  , validatePublicSubjectSelection
  , validatePublicTrialRequestInput
  , validatePublicTrialPartyId
  , validateTeacherSubjectIdsInput
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

    it "accepts common dot-and-plus email aliases while still normalizing casing" $ do
      storedEmail <- runInMemory $ do
        now <- liftIO getCurrentTime
        partyId <- createOrFetchParty (Just "Test User") (Just " User.Name+Trial@Example.com ") Nothing now
        Models.partyPrimaryEmail . entityVal <$> getJustEntity partyId

      storedEmail `shouldBe` Just "user.name+trial@example.com"

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
      assertRejected ".user@example.com"
      assertRejected "user.@example.com"
      assertRejected "user..name@example.com"
      assertRejected "user()@example.com"

    it "rejects the reserved anonymous-interest fallback email for real parties" $ do
      result <- tryCreateOrFetchParty
        (Just "Test User")
        (Just " public-interest@tdf.local ")
        Nothing
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "reserved for anonymous public interests"
        Right _ ->
          expectationFailure "Expected the fallback email to be reserved"

    it "rejects assigning the reserved fallback email through student updates" $
      case validateEmailUpdate (Just " public-interest@tdf.local ") of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "reserved for anonymous public interests"
        Right value ->
          expectationFailure ("Expected fallback email update to be rejected, got " <> show value)

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

    it "preserves collision suffixes inside the username length limit" $ do
      let root = pack (replicate 60 'a')
          candidate = buildTrialUsernameCandidate root 12
      candidate `shouldBe` pack (replicate 57 'a') <> "-12"

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

  describe "validatePublicTrialRequestInput" $ do
    it "normalizes public trial contact fields before party, credential, and request writes" $ do
      case validatePublicTrialRequestInput
        (TrialRequestIn Nothing 7 [validSlot] (Just "  Piano goals  ") (Just "  Ada Lovelace  ") (Just " ADA@Example.com ") (Just " +593 99 123 4567 ")) of
        Left err ->
          expectationFailure ("Expected valid public trial request to normalize, got " <> show err)
        Right (TrialRequestIn partyIdValue subjectIdValue preferredValue notesValue fullNameValue emailValue phoneValue) -> do
          partyIdValue `shouldBe` Nothing
          subjectIdValue `shouldBe` 7
          preferredValue `shouldBe` [validSlot]
          notesValue `shouldBe` Just "Piano goals"
          fullNameValue `shouldBe` Just "Ada Lovelace"
          emailValue `shouldBe` Just "ada@example.com"
          phoneValue `shouldBe` Just "+593991234567"

      case validatePublicTrialRequestInput
        (TrialRequestIn Nothing 7 [validSlot] (Just "   ") (Just "   ") (Just "ada@example.com") Nothing) of
        Left err ->
          expectationFailure ("Expected blank optional public trial fields to be dropped, got " <> show err)
        Right (TrialRequestIn _ _ _ notesValue fullNameValue _ _) -> do
          notesValue `shouldBe` Nothing
          fullNameValue `shouldBe` Nothing

    it "rejects malformed public trial contact fields before persisting the request" $ do
      let mkTrial fullNameValue notesValue emailValue phoneValue =
            TrialRequestIn Nothing 7 [validSlot] notesValue fullNameValue emailValue phoneValue
          assertRejected payload expectedMessage =
            case validatePublicTrialRequestInput payload of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected malformed public trial request to be rejected, got " <> show value)
      assertRejected (mkTrial (Just "Ada\nLovelace") Nothing (Just "ada@example.com") Nothing) "fullName must not contain control characters"
      assertRejected (mkTrial (Just (pack (replicate 161 'a'))) Nothing (Just "ada@example.com") Nothing) "fullName must be 1-160 characters"
      assertRejected (mkTrial (Just "Ada") (Just "first line\nsecond line") (Just "ada@example.com") Nothing) "notes must not contain control characters"
      assertRejected (mkTrial (Just "Ada") (Just (pack (replicate 2001 'a'))) (Just "ada@example.com") Nothing) "notes must be 1-2000 characters"
      assertRejected (mkTrial (Just "Ada") Nothing Nothing Nothing) "Correo requerido"

    it "rejects malformed preferred slots before the handler reaches scheduling logic" $ do
      let overlappingSlot = PreferredSlot (addUTCTime 1800 slotStart) (addUTCTime 5400 slotStart)
          payload = TrialRequestIn Nothing 7 [validSlot, overlappingSlot] Nothing (Just "Ada") (Just "ada@example.com") Nothing
      case validatePublicTrialRequestInput payload of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Preferred slots must be distinct non-overlapping windows"
        Right value ->
          expectationFailure ("Expected malformed public trial preferred slots to be rejected, got " <> show value)

  describe "public trial request handler" $ do
    it "returns invalid public trial input as a 400 response instead of an uncaught exception" $ do
      result <- runPublicTrialRequestHandler
        (TrialRequestIn Nothing 7 [validSlot] Nothing (Just "Ada Lovelace") Nothing Nothing)
      case result of
        Left ex ->
          expectationFailure
            ("Expected invalid public trial request to return a ServerError, got exception: " <> show ex)
        Right (Left err) -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Correo requerido"
        Right (Right value) ->
          expectationFailure
            ("Expected invalid public trial request to be rejected, got " <> show value)

  describe "validateOptionalTrialRequestStatusFilter" $ do
    it "treats omitted or blank filters as absent and canonicalizes supported values" $ do
      validateOptionalTrialRequestStatusFilter Nothing `shouldBe` Right Nothing
      validateOptionalTrialRequestStatusFilter (Just "   ") `shouldBe` Right Nothing
      validateOptionalTrialRequestStatusFilter (Just " requested ") `shouldBe` Right (Just "Requested")
      validateOptionalTrialRequestStatusFilter (Just "ASSIGNED") `shouldBe` Right (Just "Assigned")
      validateOptionalTrialRequestStatusFilter (Just "Scheduled") `shouldBe` Right (Just "Scheduled")

    it "rejects unknown trial request statuses instead of silently returning an empty queue" $
      case validateOptionalTrialRequestStatusFilter (Just "pending") of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "status must be one of: Requested, Assigned, Scheduled"
        Right value ->
          expectationFailure ("Expected invalid trial queue status filter to be rejected, got " <> show value)

  describe "SignupIn request decoding" $ do
    it "accepts canonical public signup payloads" $
      case (A.eitherDecode
        "{\"firstName\":\"Ada\",\"lastName\":\"Lovelace\",\"email\":\"ada@example.com\",\"phone\":\"+593991234567\",\"marketingOptIn\":true}"
          :: Either String SignupIn) of
        Left err ->
          expectationFailure ("Expected canonical public signup payload to decode, got " <> err)
        Right (SignupIn firstNameValue lastNameValue emailValue phoneValue passwordValue googleIdTokenValue marketingOptInValue) -> do
          firstNameValue `shouldBe` "Ada"
          lastNameValue `shouldBe` "Lovelace"
          emailValue `shouldBe` "ada@example.com"
          phoneValue `shouldBe` Just "+593991234567"
          passwordValue `shouldBe` Nothing
          googleIdTokenValue `shouldBe` Nothing
          marketingOptInValue `shouldBe` True

    it "rejects unexpected keys so public signup bodies fail explicitly instead of silently over-posting" $
      isLeft
        (A.eitherDecode
          "{\"firstName\":\"Ada\",\"lastName\":\"Lovelace\",\"email\":\"ada@example.com\",\"marketingOptIn\":true,\"role\":\"Admin\"}"
            :: Either String SignupIn)
        `shouldBe` True

  describe "validatePublicSignupInput" $ do
    it "normalizes public signup contact fields before creating parties and lead details" $
      case validatePublicSignupInput
        ( SignupIn
            " Ada "
            " Lovelace "
            " ADA@Example.com "
            (Just " +593 99 123 4567 ")
            Nothing
            Nothing
            True
        ) of
        Left err ->
          expectationFailure ("Expected valid public signup to normalize, got " <> show err)
        Right (SignupIn firstNameValue lastNameValue emailValue phoneValue passwordValue googleIdTokenValue marketingOptInValue) -> do
          firstNameValue `shouldBe` "Ada"
          lastNameValue `shouldBe` "Lovelace"
          emailValue `shouldBe` "ada@example.com"
          phoneValue `shouldBe` Just "+593991234567"
          passwordValue `shouldBe` Nothing
          googleIdTokenValue `shouldBe` Nothing
          marketingOptInValue `shouldBe` True

    it "rejects malformed signup names before persisting public lead details" $ do
      let baseSignup =
            SignupIn
              "Ada"
              "Lovelace"
              "ada@example.com"
              Nothing
              Nothing
              Nothing
              True
          assertRejected payload expectedMessage =
            case validatePublicSignupInput payload of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected malformed public signup to be rejected"
      assertRejected baseSignup { firstName = "   " } "firstName is required"
      assertRejected baseSignup { lastName = "   " } "lastName is required"
      assertRejected baseSignup { firstName = "Ada\nLovelace" } "firstName must not contain control characters"
      assertRejected baseSignup { lastName = pack (replicate 121 'a') } "lastName must be 120 characters or fewer"

    it "rejects oversized signup emails before persisting unusable contact data" $ do
      let oversizedEmail = pack (replicate 245 'a') <> "@example.com"
      case validatePublicSignupInput
        ( SignupIn
            "Ada"
            "Lovelace"
            oversizedEmail
            Nothing
            Nothing
            Nothing
            True
        ) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "email inválido"
        Right _ ->
          expectationFailure "Expected oversized public signup email to be rejected"

    it "rejects unsupported signup credential fields instead of silently discarding them" $ do
      let baseSignup =
            SignupIn
              "Ada"
              "Lovelace"
              "ada@example.com"
              Nothing
              Nothing
              Nothing
              True
          assertRejected payload expectedMessage =
            case validatePublicSignupInput payload of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected unsupported public signup credential to be rejected"
      assertRejected
        baseSignup { password = Just "secret123" }
        "password is not supported on public signup"
      assertRejected
        baseSignup { googleIdToken = Just "token123" }
        "googleIdToken is not supported on public signup"

      case validatePublicSignupInput
        baseSignup { password = Just "   ", googleIdToken = Just "   " } of
        Left err ->
          expectationFailure
            ("Expected blank credential placeholders to be ignored, got " <> show err)
        Right (SignupIn _ _ _ _ passwordValue googleIdTokenValue _) -> do
          passwordValue `shouldBe` Nothing
          googleIdTokenValue `shouldBe` Nothing

  describe "validatePublicInterestInput" $ do
    it "rejects typoed or unexpected JSON keys so subject selections do not silently disappear" $ do
      isLeft
        (A.eitherDecode
          "{\"interestType\":\"workshop\",\"subjectID\":7}"
            :: Either String InterestIn)
        `shouldBe` True
      isLeft
        (A.eitherDecode
          "{\"interestType\":\"workshop\",\"subjectId\":7,\"unexpected\":true}"
            :: Either String InterestIn)
        `shouldBe` True

    it "rejects blank interest types instead of creating unusable anonymous lead rows" $
      case validatePublicInterestInput (InterestIn "   " Nothing (Just "Looking for info") (Just "https://example.com")) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "interestType is required"
        Right _ ->
          expectationFailure "Expected blank interest type to be rejected"

    it "rejects oversized or control-character interest types before storing fallback lead rows" $ do
      let assertRejected rawInterestType expectedMessage =
            case validatePublicInterestInput (InterestIn rawInterestType Nothing Nothing Nothing) of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected malformed interestType to be rejected"
      assertRejected (pack (replicate 81 'a')) "interestType must be 1-80 characters"
      assertRejected "workshop\nvip" "interestType must not contain control characters"

    it "trims interest types and details, and drops blank optional fields" $ do
      case validatePublicInterestInput (InterestIn "  workshop  " (Just 7) (Just "  Looking for info  ") (Just "  https://example.com/file  ")) of
        Left err ->
          expectationFailure ("Expected valid interest input to be accepted, got " <> show err)
        Right (InterestIn interestTypeValue subjectIdValue detailsValue driveLinkValue) -> do
          interestTypeValue `shouldBe` "workshop"
          subjectIdValue `shouldBe` Just 7
          detailsValue `shouldBe` Just "Looking for info"
          driveLinkValue `shouldBe` Just "https://example.com/file"

      case validatePublicInterestInput (InterestIn "workshop" Nothing (Just "   ") Nothing) of
        Left err ->
          expectationFailure ("Expected blank optional details to be dropped, got " <> show err)
        Right (InterestIn _ _ detailsValue _) ->
          detailsValue `shouldBe` Nothing

    it "rejects oversized or control-character details before storing fallback lead rows" $ do
      let assertRejected rawDetails expectedMessage =
            case validatePublicInterestInput (InterestIn "workshop" Nothing (Just rawDetails) Nothing) of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected malformed details to be rejected"
      assertRejected (pack (replicate 2001 'a')) "details must be 1-2000 characters"
      assertRejected "line one\nline two" "details must not contain control characters"

    it "accepts public drive links with valid explicit ports" $
      case validatePublicInterestInput (InterestIn "workshop" Nothing Nothing (Just "https://example.com:8443/file")) of
        Left err ->
          expectationFailure ("Expected public driveLink with a valid port to be accepted, got " <> show err)
        Right (InterestIn _ _ _ driveLinkValue) ->
          driveLinkValue `shouldBe` Just "https://example.com:8443/file"

    it "rejects non-positive subject ids instead of treating them as unavailable subjects" $
      case validatePublicInterestInput (InterestIn "workshop" (Just 0) Nothing Nothing) of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "subjectId must be a positive integer"
        Right _ ->
          expectationFailure "Expected invalid subjectId to be rejected"

    it "rejects malformed or non-HTTPS drive links instead of storing ambiguous free-form text" $ do
      let assertRejected rawDriveLink =
            case validatePublicInterestInput (InterestIn "workshop" Nothing Nothing (Just rawDriveLink)) of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "driveLink must be an absolute https URL"
              Right _ ->
                expectationFailure "Expected invalid driveLink to be rejected"
      assertRejected "folder-123"
      assertRejected "http://example.com/folder"
      assertRejected "https://example.com/shared file"
      assertRejected "https://drive..example.com/folder"
      assertRejected "https://drive_example.com/folder"
      assertRejected "https://drive/folder"
      assertRejected "https://2130706433/folder"
      assertRejected "https://0177.0.0.1/folder"
      assertRejected "https://192.0.2.10/folder"
      assertRejected "https://198.51.100.24/folder"
      assertRejected "https://203.0.113.77/folder"
      assertRejected "https://224.0.0.1/folder"
      assertRejected "http://localhost/folder"
      assertRejected "http://127.0.0.1/folder"
      assertRejected "https://[::1]/folder"
      assertRejected "https://example.com:70000/folder"

  describe "SubjectUpdate request decoding" $ do
    it "rejects typoed or unexpected JSON keys so subject patches cannot degrade into silent no-ops" $ do
      case (A.eitherDecode "{\"active\":false}" :: Either String SubjectUpdate) of
        Left err ->
          expectationFailure ("Expected canonical subject update payload to decode, got " <> err)
        Right (SubjectUpdate nameValue activeValue) -> do
          nameValue `shouldBe` Nothing
          activeValue `shouldBe` Just False

      isLeft
        (A.eitherDecode
          "{\"activee\":false}"
            :: Either String SubjectUpdate)
        `shouldBe` True
      isLeft
        (A.eitherDecode
          "{\"active\":false,\"unexpected\":true}"
            :: Either String SubjectUpdate)
        `shouldBe` True

  describe "StudentCreate request decoding" $ do
    it "accepts canonical private student create payloads" $ do
      let payload = BL8.pack $ concat
            [ "{\"fullName\":\"Ada Lovelace\""
            , ",\"email\":\"ada@example.com\""
            , ",\"phone\":\"+593991234567\""
            , ",\"notes\":\"trial referral\"}"
            ]
      case (A.eitherDecode payload :: Either String StudentCreate) of
        Left err ->
          expectationFailure ("Expected canonical student create payload to decode, got " <> err)
        Right (StudentCreate fullNameValue emailValue phoneValue notesValue) -> do
          fullNameValue `shouldBe` "Ada Lovelace"
          emailValue `shouldBe` "ada@example.com"
          phoneValue `shouldBe` Just "+593991234567"
          notesValue `shouldBe` Just "trial referral"

    it "rejects typoed or unexpected JSON keys so student creates cannot silently drop fields" $ do
      isLeft
        ( A.eitherDecode "{\"fullNam\":\"Ada\",\"email\":\"ada@example.com\"}"
            :: Either String StudentCreate
        )
        `shouldBe` True
      isLeft
        ( A.eitherDecode
            "{\"fullName\":\"Ada\",\"email\":\"ada@example.com\",\"role\":\"Admin\"}"
            :: Either String StudentCreate
        )
        `shouldBe` True

  describe "StudentUpdate request decoding" $ do
    it "rejects typoed or unexpected JSON keys so student patches cannot degrade into silent no-ops" $ do
      case (A.eitherDecode "{\"displayName\":\"Ada\",\"phone\":\"+593991234567\"}" :: Either String StudentUpdate) of
        Left err ->
          expectationFailure ("Expected canonical student update payload to decode, got " <> err)
        Right (StudentUpdate displayNameValue emailValue phoneValue notesValue) -> do
          displayNameValue `shouldBe` Just "Ada"
          emailValue `shouldBe` Nothing
          phoneValue `shouldBe` Just "+593991234567"
          notesValue `shouldBe` Nothing

      isLeft
        (A.eitherDecode
          "{\"displayNam\":\"Ada\"}"
            :: Either String StudentUpdate)
        `shouldBe` True
      isLeft
        (A.eitherDecode
          "{\"displayName\":\"Ada\",\"unexpected\":true}"
            :: Either String StudentUpdate)
        `shouldBe` True

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

  describe "validateTeacherSubjectIdsInput" $ do
    it "accepts explicit clears and distinct positive ids without rewriting the requested subject set" $ do
      validateTeacherSubjectIdsInput [] `shouldBe` Right []
      validateTeacherSubjectIdsInput [7, 3, 5] `shouldBe` Right [7, 3, 5]

    it "rejects non-positive ids instead of silently treating them as a subject clear or partial update" $ do
      let assertRejected rawSubjectIds =
            case validateTeacherSubjectIdsInput rawSubjectIds of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "subjectIds must contain only positive integers"
              Right value ->
                expectationFailure ("Expected invalid subject ids to be rejected, got " <> show value)
      assertRejected [0]
      assertRejected [4, -1]

    it "rejects duplicate ids instead of silently collapsing an ambiguous teacher subject update" $
      case validateTeacherSubjectIdsInput [7, 3, 7] of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "subjectIds must not contain duplicates"
        Right value ->
          expectationFailure ("Expected duplicate subject ids to be rejected, got " <> show value)

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

  describe "SubjectCreate request decoding" $ do
    it "rejects unexpected create keys so admin subject creation cannot silently default fields" $ do
      case (A.eitherDecode "{\"name\":\"Piano\",\"active\":false}" :: Either String SubjectCreate) of
        Left decodeErr ->
          expectationFailure ("Expected canonical subject create payload to decode, got: " <> decodeErr)
        Right (SubjectCreate nameValue activeValue) -> do
          nameValue `shouldBe` "Piano"
          activeValue `shouldBe` Just False

      isLeft
        (A.eitherDecode "{\"name\":\"Piano\",\"activee\":false}" :: Either String SubjectCreate)
        `shouldBe` True
      isLeft
        (A.eitherDecode "{\"name\":\"Piano\",\"active\":false,\"roomIds\":[1]}" :: Either String SubjectCreate)
        `shouldBe` True

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

  describe "private trial scheduling request decoding" $ do
    it "rejects typoed or unexpected assignment keys so teacher selection cannot be silently ignored" $ do
      case (A.eitherDecode "{\"teacherId\":2}" :: Either String TrialAssignIn) of
        Left decodeErr ->
          expectationFailure ("Expected canonical trial assignment payload to decode, got: " <> decodeErr)
        Right (TrialAssignIn teacherIdValue) ->
          teacherIdValue `shouldBe` 2

      isLeft
        (A.eitherDecode "{\"teacherID\":2}" :: Either String TrialAssignIn)
        `shouldBe` True
      isLeft
        (A.eitherDecode "{\"teacherId\":2,\"status\":\"Scheduled\"}" :: Either String TrialAssignIn)
        `shouldBe` True

    it "rejects typoed or unexpected schedule keys so room and time intent stays explicit" $ do
      let canonicalPayload = BL8.pack $ concat
            [ "{\"requestId\":1"
            , ",\"teacherId\":2"
            , ",\"startAt\":\"2026-04-01T10:00:00Z\""
            , ",\"endAt\":\"2026-04-01T11:00:00Z\""
            , ",\"roomId\":3}"
            ]
      case A.eitherDecode canonicalPayload of
        Left decodeErr ->
          expectationFailure ("Expected canonical trial schedule payload to decode, got: " <> decodeErr)
        Right (TrialScheduleIn requestIdValue teacherIdValue startValue endValue roomIdValue) -> do
          requestIdValue `shouldBe` 1
          teacherIdValue `shouldBe` 2
          startValue `shouldBe` slotStart
          endValue `shouldBe` slotEnd
          roomIdValue `shouldBe` 3

      let assertRejected payload =
            isLeft
              (A.eitherDecode (BL8.pack payload) :: Either String TrialScheduleIn)
              `shouldBe` True
      assertRejected $ concat
        [ "{\"requestId\":1"
        , ",\"teacherId\":2"
        , ",\"startAt\":\"2026-04-01T10:00:00Z\""
        , ",\"endAt\":\"2026-04-01T11:00:00Z\""
        , ",\"roomID\":3}"
        ]
      assertRejected $ concat
        [ "{\"requestId\":1"
        , ",\"teacherId\":2"
        , ",\"startAt\":\"2026-04-01T10:00:00Z\""
        , ",\"endAt\":\"2026-04-01T11:00:00Z\""
        , ",\"roomId\":3"
        , ",\"status\":\"Scheduled\"}"
        ]

  describe "validatePurchaseInput" $ do
    let validPurchase :: PurchaseIn
        validPurchase = PurchaseIn 1 2 12000 (Just 1000) (Just 1440) (Just 3) (Just 4) (Just 5)

    it "accepts positive ids and non-negative purchase totals" $
      case validatePurchaseInput validPurchase of
        Left err ->
          expectationFailure ("Expected valid purchase input to be accepted, got " <> show err)
        Right _ ->
          pure ()

    it "rejects malformed ids and impossible money fields before persisting a purchase" $ do
      let assertInvalid expectedMessage purchase =
            case validatePurchaseInput purchase of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected invalid purchase input to be rejected"
      assertInvalid "studentId must be a positive integer" $
        PurchaseIn 0 2 12000 (Just 1000) (Just 1440) (Just 3) (Just 4) (Just 5)
      assertInvalid "packageId must be a positive integer" $
        PurchaseIn 1 (-1) 12000 (Just 1000) (Just 1440) (Just 3) (Just 4) (Just 5)
      assertInvalid "sellerId must be a positive integer" $
        PurchaseIn 1 2 12000 (Just 1000) (Just 1440) (Just 0) (Just 4) (Just 5)
      assertInvalid "commissionedTeacherId must be a positive integer" $
        PurchaseIn 1 2 12000 (Just 1000) (Just 1440) (Just 3) (Just (-3)) (Just 5)
      assertInvalid "trialRequestId must be a positive integer" $
        PurchaseIn 1 2 12000 (Just 1000) (Just 1440) (Just 3) (Just 4) (Just 0)
      assertInvalid "priceCents must be zero or a positive integer" $
        PurchaseIn 1 2 (-1) (Just 1000) (Just 1440) (Just 3) (Just 4) (Just 5)
      assertInvalid "discountCents must be zero or a positive integer" $
        PurchaseIn 1 2 12000 (Just (-1)) (Just 1440) (Just 3) (Just 4) (Just 5)
      assertInvalid "taxCents must be zero or a positive integer" $
        PurchaseIn 1 2 12000 (Just 1000) (Just (-1)) (Just 3) (Just 4) (Just 5)
      assertInvalid "discountCents must not exceed priceCents" $
        PurchaseIn 1 2 12000 (Just 13000) (Just 1440) (Just 3) (Just 4) (Just 5)

  describe "PurchaseIn FromJSON" $ do
    it "accepts canonical purchase payloads for the private trials purchase endpoint" $
      case A.eitherDecode
        "{\"studentId\":1,\"packageId\":2,\"priceCents\":12000,\"discountCents\":1000,\"taxCents\":1440,\"sellerId\":3,\"commissionedTeacherId\":4,\"trialRequestId\":5}" of
        Left decodeErr ->
          expectationFailure ("Expected canonical purchase payload to decode, got: " <> decodeErr)
        Right (PurchaseIn studentIdValue packageIdValue priceCentsValue discountCentsValue taxCentsValue sellerIdValue commissionedTeacherIdValue trialRequestIdValue) -> do
          studentIdValue `shouldBe` 1
          packageIdValue `shouldBe` 2
          priceCentsValue `shouldBe` 12000
          discountCentsValue `shouldBe` Just 1000
          taxCentsValue `shouldBe` Just 1440
          sellerIdValue `shouldBe` Just 3
          commissionedTeacherIdValue `shouldBe` Just 4
          trialRequestIdValue `shouldBe` Just 5

    it "rejects typoed or unexpected purchase keys so financial writes cannot silently ignore caller intent" $ do
      isLeft
        ( A.eitherDecode
            "{\"studentId\":1,\"packageId\":2,\"priceCents\":12000,\"commissionedTeacherID\":4}"
            :: Either String PurchaseIn
        )
        `shouldBe` True
      isLeft
        ( A.eitherDecode
            "{\"studentId\":1,\"packageId\":2,\"priceCents\":12000,\"status\":\"paid\"}"
            :: Either String PurchaseIn
        )
        `shouldBe` True

  describe "private purchases" $ do
    it "rejects missing referenced rows before persisting an orphan purchase" $ do
      let assertRejected expectedMessage buildPurchase = do
            result <- try $ runTrialsInMemory $ do
              now <- liftIO getCurrentTime
              let scheduleStart = addUTCTime 3600 now
                  scheduleEnd = addUTCTime 7200 now
              studentPartyId <- insertPartyFixture "Student One" now
              sellerPartyId <- insertPartyFixture "Seller One" now
              subjectKey <- insert (Subject "Piano" True)
              packageKey <- insertPackageFixture subjectKey
              requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
              privatePurchaseHandler
                (buildPurchase studentPartyId sellerPartyId packageKey requestKey)
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 404
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected dangling purchase references to be rejected"
      assertRejected "Estudiante no encontrado" $
        \_ sellerPartyId packageKey requestKey ->
          PurchaseIn
            9999
            (fromIntegral (fromSqlKey packageKey))
            12000
            (Just 1000)
            (Just 1440)
            (Just (fromIntegral (fromSqlKey sellerPartyId)))
            Nothing
            (Just (fromIntegral (fromSqlKey requestKey)))
      assertRejected "Paquete no encontrado" $
        \studentPartyId sellerPartyId _ requestKey ->
          PurchaseIn
            (fromIntegral (fromSqlKey studentPartyId))
            9999
            12000
            (Just 1000)
            (Just 1440)
            (Just (fromIntegral (fromSqlKey sellerPartyId)))
            Nothing
            (Just (fromIntegral (fromSqlKey requestKey)))
      assertRejected "Vendedor no encontrado" $
        \studentPartyId _ packageKey requestKey ->
          PurchaseIn
            (fromIntegral (fromSqlKey studentPartyId))
            (fromIntegral (fromSqlKey packageKey))
            12000
            (Just 1000)
            (Just 1440)
            (Just 9999)
            Nothing
            (Just (fromIntegral (fromSqlKey requestKey)))
      assertRejected "Solicitud de prueba no encontrada" $
        \studentPartyId sellerPartyId packageKey _ ->
          PurchaseIn
            (fromIntegral (fromSqlKey studentPartyId))
            (fromIntegral (fromSqlKey packageKey))
            12000
            (Just 1000)
            (Just 1440)
            (Just (fromIntegral (fromSqlKey sellerPartyId)))
            Nothing
            (Just 9999)

    it "rejects non-teacher commissioned sellers instead of storing an invalid commission target" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        studentPartyId <- insertPartyFixture "Student One" now
        sellerPartyId <- insertPartyFixture "Seller One" now
        nonTeacherPartyId <- insertPartyFixture "Studio Assistant" now
        subjectKey <- insert (Subject "Piano" True)
        packageKey <- insertPackageFixture subjectKey
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        privatePurchaseHandler
          (PurchaseIn
            (fromIntegral (fromSqlKey studentPartyId))
            (fromIntegral (fromSqlKey packageKey))
            12000
            (Just 1000)
            (Just 1440)
            (Just (fromIntegral (fromSqlKey sellerPartyId)))
            (Just (fromIntegral (fromSqlKey nonTeacherPartyId)))
            (Just (fromIntegral (fromSqlKey requestKey))))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no está registrada como profesor"
        Right _ ->
          expectationFailure "Expected invalid commissioned teachers to be rejected"

    it "rejects trial requests that do not match the purchase student and package subject" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        studentPartyId <- insertPartyFixture "Student One" now
        otherStudentPartyId <- insertPartyFixture "Student Two" now
        sellerPartyId <- insertPartyFixture "Seller One" now
        packageSubjectKey <- insert (Subject "Piano" True)
        requestSubjectKey <- insert (Subject "Guitar" True)
        packageKey <- insertPackageFixture packageSubjectKey
        requestKey <- insertTrialRequestFixture otherStudentPartyId requestSubjectKey scheduleStart scheduleEnd now
        privatePurchaseHandler
          (PurchaseIn
            (fromIntegral (fromSqlKey studentPartyId))
            (fromIntegral (fromSqlKey packageKey))
            12000
            (Just 1000)
            (Just 1440)
            (Just (fromIntegral (fromSqlKey sellerPartyId)))
            Nothing
            (Just (fromIntegral (fromSqlKey requestKey))))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "mismo estudiante"
        Right _ ->
          expectationFailure "Expected mismatched trial request references to be rejected"

  describe "ClassSessionIn FromJSON" $ do
    it "accepts canonical class-session create payloads" $ do
      let payload = BL8.pack $ concat
            [ "{\"studentId\":1"
            , ",\"teacherId\":2"
            , ",\"subjectId\":3"
            , ",\"startAt\":\"2026-04-01T10:00:00Z\""
            , ",\"endAt\":\"2026-04-01T11:00:00Z\""
            , ",\"roomId\":4"
            , ",\"bookingId\":5}"
            ]
      case A.eitherDecode payload of
        Left decodeErr ->
          expectationFailure ("Expected canonical class session payload to decode, got: " <> decodeErr)
        Right (ClassSessionIn studentIdValue teacherIdValue subjectIdValue startValue endValue roomIdValue bookingIdValue) -> do
          studentIdValue `shouldBe` 1
          teacherIdValue `shouldBe` 2
          subjectIdValue `shouldBe` 3
          startValue `shouldBe` slotStart
          endValue `shouldBe` slotEnd
          roomIdValue `shouldBe` 4
          bookingIdValue `shouldBe` Just 5

    it "rejects typoed class-session keys so booking links cannot be silently dropped" $ do
      let assertRejected payload =
            isLeft
              (A.eitherDecode (BL8.pack payload) :: Either String ClassSessionIn)
              `shouldBe` True
      assertRejected $ concat
        [ "{\"studentId\":1"
        , ",\"teacherId\":2"
        , ",\"subjectId\":3"
        , ",\"startAt\":\"2026-04-01T10:00:00Z\""
        , ",\"endAt\":\"2026-04-01T11:00:00Z\""
        , ",\"roomId\":4"
        , ",\"bookingID\":5}"
        ]
      assertRejected $ concat
        [ "{\"studentId\":1"
        , ",\"teacherId\":2"
        , ",\"subjectId\":3"
        , ",\"startAt\":\"2026-04-01T10:00:00Z\""
        , ",\"endAt\":\"2026-04-01T11:00:00Z\""
        , ",\"roomId\":4"
        , ",\"unexpected\":true}"
        ]

  describe "AttendIn FromJSON" $ do
    it "accepts canonical attendance payloads for private class sessions" $
      case A.eitherDecode "{\"attended\":true,\"notes\":\"Completed exercises\"}" of
        Left decodeErr ->
          expectationFailure ("Expected canonical attendance payload to decode, got: " <> decodeErr)
        Right (AttendIn attendedValue notesValue) -> do
          attendedValue `shouldBe` True
          notesValue `shouldBe` Just "Completed exercises"

    it "rejects typoed attendance keys instead of silently leaving attendance unchanged" $ do
      isLeft
        ( A.eitherDecode
            "{\"attended\":true,\"note\":\"Completed exercises\"}"
            :: Either String AttendIn
        )
        `shouldBe` True
      isLeft
        ( A.eitherDecode
            "{\"attended\":true,\"consumedMinutes\":60}"
            :: Either String AttendIn
        )
        `shouldBe` True

  describe "private trial queue filtering" $ do
    it "rejects non-positive subject filters before querying the queue" $ do
      let assertRejected rawSubjectId = do
            result <- try $ runTrialsInMemory $
              privateQueueHandler (Just rawSubjectId) Nothing
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "subjectId must be a positive integer"
              Right _ ->
                expectationFailure "Expected invalid queue subject filter to be rejected"
      assertRejected 0
      assertRejected (-3)

  describe "private trial availability filtering" $ do
    it "rejects non-positive subject filters before querying availability slots" $ do
      let assertRejected rawSubjectId = do
            result <- try $ runTrialsInMemory $
              privateAvailabilityListHandler (Just rawSubjectId) Nothing Nothing
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "subjectId must be a positive integer"
              Right _ ->
                expectationFailure "Expected invalid availability subject filter to be rejected"
      assertRejected 0
      assertRejected (-3)

    it "rejects inverted availability windows instead of silently returning no slots" $ do
      result <- try $ runTrialsInMemory $
        privateAvailabilityListHandler Nothing (Just slotEnd) (Just slotStart)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "from must be on or before to"
        Right value ->
          expectationFailure ("Expected inverted availability window to be rejected, got " <> show value)

  describe "TrialAvailabilityUpsert request decoding" $ do
    it "rejects typoed teacher keys so teacherId fallback cannot publish slots for the wrong teacher" $ do
      let canonicalPayload = BL8.pack $ concat
            [ "{\"subjectId\":3"
            , ",\"roomId\":\"sala-a\""
            , ",\"startAt\":\"2026-04-01T10:00:00Z\""
            , ",\"endAt\":\"2026-04-01T11:00:00Z\""
            , ",\"teacherId\":2}"
            ]
      case A.eitherDecode canonicalPayload of
        Left decodeErr ->
          expectationFailure ("Expected canonical availability payload to decode, got " <> decodeErr)
        Right (TrialAvailabilityUpsert availabilityIdValue subjectIdValue roomIdValue startValue endValue notesValue teacherIdValue) -> do
          availabilityIdValue `shouldBe` Nothing
          subjectIdValue `shouldBe` 3
          roomIdValue `shouldBe` "sala-a"
          startValue `shouldBe` slotStart
          endValue `shouldBe` slotEnd
          notesValue `shouldBe` Nothing
          teacherIdValue `shouldBe` Just 2

      isLeft
        ( A.eitherDecode
            (BL8.pack $ concat
              [ "{\"subjectId\":3"
              , ",\"roomId\":\"sala-a\""
              , ",\"startAt\":\"2026-04-01T10:00:00Z\""
              , ",\"endAt\":\"2026-04-01T11:00:00Z\""
              , ",\"teacherID\":2}"
              ])
            :: Either String TrialAvailabilityUpsert
        )
        `shouldBe` True

  describe "teacher relationship request decoding" $ do
    it "rejects typoed or unexpected teacher-subject keys before mutating assignments" $ do
      case (A.eitherDecode "{\"subjectIds\":[3,7]}" :: Either String TeacherSubjectsUpdate) of
        Left decodeErr ->
          expectationFailure ("Expected canonical teacher-subject payload to decode, got " <> decodeErr)
        Right (TeacherSubjectsUpdate subjectIdsValue) ->
          subjectIdsValue `shouldBe` [3, 7]

      isLeft
        ( A.eitherDecode "{\"subjectIDs\":[3]}"
            :: Either String TeacherSubjectsUpdate
        )
        `shouldBe` True
      isLeft
        ( A.eitherDecode "{\"subjectIds\":[3],\"teacherId\":2}"
            :: Either String TeacherSubjectsUpdate
        )
        `shouldBe` True

    it "rejects unexpected teacher-student link keys instead of silently ignoring intent" $ do
      case (A.eitherDecode "{\"studentId\":12}" :: Either String TeacherStudentLinkIn) of
        Left decodeErr ->
          expectationFailure ("Expected canonical teacher-student payload to decode, got " <> decodeErr)
        Right (TeacherStudentLinkIn studentIdValue) ->
          studentIdValue `shouldBe` 12

      isLeft
        ( A.eitherDecode "{\"studentID\":12}"
            :: Either String TeacherStudentLinkIn
        )
        `shouldBe` True
      isLeft
        ( A.eitherDecode "{\"studentId\":12,\"teacherId\":2}"
            :: Either String TeacherStudentLinkIn
        )
        `shouldBe` True

  describe "private teacher class filtering" $ do
    it "rejects non-positive teacher or subject filters before querying class history" $ do
      let assertRejected expectedMessage rawTeacherId rawSubjectId = do
            result <- try $ runTrialsInMemory $
              privateTeacherClassesHandler rawTeacherId (Just rawSubjectId) Nothing Nothing
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected invalid teacher class filters to be rejected, got " <> show value)
      assertRejected "teacherId must be a positive integer" 0 1
      assertRejected "teacherId must be a positive integer" (-3) 1
      assertRejected "subjectId must be a positive integer" 1 0
      assertRejected "subjectId must be a positive integer" 1 (-7)

    it "rejects inverted teacher class windows instead of silently returning no classes" $ do
      result <- try $ runTrialsInMemory $
        privateTeacherClassesHandler 1 Nothing (Just slotEnd) (Just slotStart)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "from must be on or before to"
        Right value ->
          expectationFailure ("Expected inverted teacher class window to be rejected, got " <> show value)

  describe "private class session filtering" $ do
    it "rejects non-positive subject, teacher, or student filters before querying class history" $ do
      let assertRejected expectedMessage rawSubjectId rawTeacherId rawStudentId = do
            result <- try $ runTrialsInMemory $
              privateClassSessionsListHandler rawSubjectId rawTeacherId rawStudentId Nothing Nothing Nothing
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected invalid class session filters to be rejected, got " <> show value)
      assertRejected "subjectId must be a positive integer" (Just 0) Nothing Nothing
      assertRejected "teacherId must be a positive integer" Nothing (Just (-3)) Nothing
      assertRejected "studentId must be a positive integer" Nothing Nothing (Just 0)

    it "rejects inverted class session windows instead of silently returning no classes" $ do
      result <- try $ runTrialsInMemory $
        privateClassSessionsListHandler Nothing Nothing Nothing (Just slotEnd) (Just slotStart) Nothing
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "from must be on or before to"
        Right value ->
          expectationFailure ("Expected inverted class session window to be rejected, got " <> show value)

  describe "private package filtering" $ do
    it "rejects non-positive subject filters before querying packages" $ do
      let assertRejected rawSubjectId = do
            result <- try $ runTrialsInMemory $
              privatePackagesHandler (Just rawSubjectId)
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "subjectId must be a positive integer"
              Right _ ->
                expectationFailure "Expected invalid package subject filter to be rejected"
      assertRejected 0
      assertRejected (-3)

  describe "private trial availability upserts" $ do
    it "rejects non-positive availability ids before querying for deletion targets" $ do
      let assertRejected rawAvailabilityId = do
            result <- try $ runTrialsInMemory $
              privateAvailabilityDeleteHandler rawAvailabilityId
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` "availabilityId must be a positive integer"
              Right _ ->
                expectationFailure "Expected invalid availability ids to be rejected"
      validateAvailabilityIdInput 7 `shouldBe` Right 7
      assertRejected 0
      assertRejected (-3)

    it "rejects non-room resources instead of publishing impossible availability slots" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let availabilityStart = addUTCTime 3600 now
            availabilityEnd = addUTCTime 7200 now
        teacherPartyId <- insertTeacherFixture "Teacher One" now
        nonRoomResourceId <- insertResourceFixture "PA Rack" "pa-rack" Models.Equipment
        subjectKey <- insert (Subject "Piano" True)
        privateAvailabilityUpsertHandler
          (TrialAvailabilityUpsert
            Nothing
            (fromIntegral (fromSqlKey subjectKey))
            (pack (show (fromSqlKey nonRoomResourceId)))
            availabilityStart
            availabilityEnd
            Nothing
            (Just (fromIntegral (fromSqlKey teacherPartyId))))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no es una sala"
        Right _ ->
          expectationFailure "Expected non-room resources to be rejected for availability upserts"

  describe "private trial scheduling" $ do
    it "rejects non-positive assignment identifiers before any lookup so malformed requests return 400" $ do
      let assertRejected expectedMessage rawRequestId rawTeacherId = do
            result <- try $ runTrialsInMemory $
              privateAssignHandler rawRequestId (TrialAssignIn rawTeacherId)
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right value ->
                expectationFailure ("Expected malformed assignment identifiers to be rejected, got " <> show value)
      assertRejected "requestId must be a positive integer" 0 42
      assertRejected "teacherId must be a positive integer" 1 0

    it "rejects assigning a trial to a non-teacher party instead of storing an invalid assignedTeacherId" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        nonTeacherPartyId <- insertPartyFixture "Student Helper" now
        studentPartyId <- insertPartyFixture "Student One" now
        subjectKey <- insert (Subject "Piano" True)
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        privateAssignHandler
          (fromIntegral (fromSqlKey requestKey))
          (TrialAssignIn (fromIntegral (fromSqlKey nonTeacherPartyId)))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no está registrada como profesor"
        Right _ ->
          expectationFailure "Expected non-teacher parties to be rejected for trial assignment"

    it "rejects scheduling a trial with a missing teacher id instead of creating an orphan assignment" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
            missingTeacherId = 999999
        studentPartyId <- insertPartyFixture "Student One" now
        roomResourceId <- insertRoomFixture "Sala A" "sala-a"
        subjectKey <- insert (Subject "Piano" True)
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        privateScheduleHandler
          (TrialScheduleIn
            (fromIntegral (fromSqlKey requestKey))
            missingTeacherId
            scheduleStart
            scheduleEnd
            (fromIntegral (fromSqlKey roomResourceId)))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 404
          BL8.unpack (errBody err) `shouldContain` "Profesor no encontrado"
        Right _ ->
          expectationFailure "Expected missing teacher to be rejected"

    it "rejects scheduling a trial with a non-teacher party instead of storing an invalid teacher assignment" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        nonTeacherPartyId <- insertPartyFixture "Student Helper" now
        studentPartyId <- insertPartyFixture "Student One" now
        roomResourceId <- insertRoomFixture "Sala A" "sala-a"
        subjectKey <- insert (Subject "Piano" True)
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        privateScheduleHandler
          (TrialScheduleIn
            (fromIntegral (fromSqlKey requestKey))
            (fromIntegral (fromSqlKey nonTeacherPartyId))
            scheduleStart
            scheduleEnd
            (fromIntegral (fromSqlKey roomResourceId)))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no está registrada como profesor"
        Right _ ->
          expectationFailure "Expected non-teacher parties to be rejected for trial scheduling"

    it "rejects scheduling a trial with an inactive teacher role instead of using stale authorization" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        formerTeacherPartyId <- insertPartyFixture "Former Teacher" now
        _ <- insert Models.PartyRole
          { Models.partyRolePartyId = formerTeacherPartyId
          , Models.partyRoleRole = Models.Teacher
          , Models.partyRoleActive = False
          }
        studentPartyId <- insertPartyFixture "Student One" now
        roomResourceId <- insertRoomFixture "Sala A" "sala-a"
        subjectKey <- insert (Subject "Piano" True)
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        privateScheduleHandler
          (TrialScheduleIn
            (fromIntegral (fromSqlKey requestKey))
            (fromIntegral (fromSqlKey formerTeacherPartyId))
            scheduleStart
            scheduleEnd
            (fromIntegral (fromSqlKey roomResourceId)))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no está registrada como profesor"
        Right _ ->
          expectationFailure "Expected inactive teacher roles to be rejected for trial scheduling"

    it "rejects scheduling a trial with a missing room id instead of accepting a dangling room reference" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
            missingRoomId = 999999
        teacherPartyId <- insertTeacherFixture "Teacher One" now
        studentPartyId <- insertPartyFixture "Student One" now
        subjectKey <- insert (Subject "Piano" True)
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        privateScheduleHandler
          (TrialScheduleIn
            (fromIntegral (fromSqlKey requestKey))
            (fromIntegral (fromSqlKey teacherPartyId))
            scheduleStart
            scheduleEnd
            missingRoomId)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 404
          BL8.unpack (errBody err) `shouldContain` "Sala no encontrada"
        Right _ ->
          expectationFailure "Expected missing room to be rejected"

    it "rejects scheduling a trial into a non-room resource instead of treating any resource id as a sala" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        teacherPartyId <- insertTeacherFixture "Teacher One" now
        studentPartyId <- insertPartyFixture "Student One" now
        nonRoomResourceId <- insertResourceFixture "PA Rack" "pa-rack" Models.Equipment
        subjectKey <- insert (Subject "Piano" True)
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        privateScheduleHandler
          (TrialScheduleIn
            (fromIntegral (fromSqlKey requestKey))
            (fromIntegral (fromSqlKey teacherPartyId))
            scheduleStart
            scheduleEnd
            (fromIntegral (fromSqlKey nonRoomResourceId)))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no es una sala"
        Right _ ->
          expectationFailure "Expected non-room resources to be rejected for trial scheduling"

    it "rejects scheduling a trial into a room that is not allowed for the subject" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        teacherPartyId <- insertTeacherFixture "Teacher One" now
        studentPartyId <- insertPartyFixture "Student One" now
        allowedRoomId <- insertRoomFixture "Sala Piano" "sala-piano"
        blockedRoomId <- insertRoomFixture "Sala Voces" "sala-voces"
        subjectKey <- insert (Subject "Piano" True)
        _ <- insert Trials.SubjectRoomPreference
          { Trials.subjectRoomPreferenceSubjectId = subjectKey
          , Trials.subjectRoomPreferenceRoomId = allowedRoomId
          , Trials.subjectRoomPreferencePriority = 1
          }
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        privateScheduleHandler
          (TrialScheduleIn
            (fromIntegral (fromSqlKey requestKey))
            (fromIntegral (fromSqlKey teacherPartyId))
            scheduleStart
            scheduleEnd
            (fromIntegral (fromSqlKey blockedRoomId)))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "Esta materia no se dicta en la sala seleccionada."
        Right _ ->
          expectationFailure "Expected rooms outside the subject preference list to be rejected"

    it "rejects scheduling a trial when the teacher is already booked in an overlapping class" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        teacherPartyId <- insertTeacherFixture "Teacher One" now
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

    it "rejects scheduling a trial when the room is already occupied by another class" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let scheduleStart = addUTCTime 3600 now
            scheduleEnd = addUTCTime 7200 now
        scheduledTeacherPartyId <- insertTeacherFixture "Teacher One" now
        blockingTeacherPartyId <- insertTeacherFixture "Teacher Two" now
        studentPartyId <- insertPartyFixture "Student One" now
        otherStudentPartyId <- insertPartyFixture "Student Two" now
        roomResourceId <- insertRoomFixture "Sala A" "sala-a"
        subjectKey <- insert (Subject "Piano" True)
        requestKey <- insertTrialRequestFixture studentPartyId subjectKey scheduleStart scheduleEnd now
        _ <- insert Trials.ClassSession
          { Trials.classSessionStudentId = otherStudentPartyId
          , Trials.classSessionTeacherId = blockingTeacherPartyId
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
            (fromIntegral (fromSqlKey scheduledTeacherPartyId))
            scheduleStart
            scheduleEnd
            (fromIntegral (fromSqlKey roomResourceId)))
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "Sala no disponible en ese horario"
        Right _ ->
          expectationFailure "Expected overlapping room schedule to be rejected"

    it "allows rescheduling the same trial request without conflicting with its own existing assignment" $ do
      (response, assignment, newStart, newEnd) <- runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let oldStart = addUTCTime 3600 now
            oldEnd = addUTCTime 7200 now
            newStart = addUTCTime 5400 now
            newEnd = addUTCTime 9000 now
        teacherPartyId <- insertTeacherFixture "Teacher One" now
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

  describe "private class session validation" $ do
    it "rejects creating a class with missing student or subject references before persisting orphaned rows" $ do
      let assertRejected expectedMessage buildInput = do
            result <- try $ runTrialsInMemory $ do
              now <- liftIO getCurrentTime
              let classStart = addUTCTime 3600 now
                  classEnd = addUTCTime 7200 now
              teacherPartyId <- insertTeacherFixture "Teacher One" now
              studentPartyId <- insertPartyFixture "Student One" now
              roomResourceId <- insertRoomFixture "Sala A" "sala-a"
              subjectKey <- insert (Subject "Piano" True)
              privateCreateClassHandler (buildInput studentPartyId subjectKey teacherPartyId roomResourceId classStart classEnd)
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 404
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected missing class references to be rejected"
      assertRejected "Estudiante no encontrado" $
        \_ subjectKey teacherPartyId roomResourceId classStart classEnd ->
          ClassSessionIn
            9999
            (fromIntegral (fromSqlKey teacherPartyId))
            (fromIntegral (fromSqlKey subjectKey))
            classStart
            classEnd
            (fromIntegral (fromSqlKey roomResourceId))
            Nothing
      assertRejected "Materia no encontrada" $
        \studentPartyId _ teacherPartyId roomResourceId classStart classEnd ->
          ClassSessionIn
            (fromIntegral (fromSqlKey studentPartyId))
            (fromIntegral (fromSqlKey teacherPartyId))
            9999
            classStart
            classEnd
            (fromIntegral (fromSqlKey roomResourceId))
            Nothing

    it "rejects creating a class with a non-teacher party instead of storing an invalid teacher assignment" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let classStart = addUTCTime 3600 now
            classEnd = addUTCTime 7200 now
        nonTeacherPartyId <- insertPartyFixture "Student Helper" now
        studentPartyId <- insertPartyFixture "Student One" now
        roomResourceId <- insertRoomFixture "Sala A" "sala-a"
        subjectKey <- insert (Subject "Piano" True)
        privateCreateClassHandler
          (ClassSessionIn
            (fromIntegral (fromSqlKey studentPartyId))
            (fromIntegral (fromSqlKey nonTeacherPartyId))
            (fromIntegral (fromSqlKey subjectKey))
            classStart
            classEnd
            (fromIntegral (fromSqlKey roomResourceId))
            Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no está registrada como profesor"
        Right _ ->
          expectationFailure "Expected non-teacher parties to be rejected for class creation"

    it "rejects creating a class in a non-room resource instead of treating any resource as a sala" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let classStart = addUTCTime 3600 now
            classEnd = addUTCTime 7200 now
        teacherPartyId <- insertTeacherFixture "Teacher One" now
        studentPartyId <- insertPartyFixture "Student One" now
        nonRoomResourceId <- insertResourceFixture "PA Rack" "pa-rack" Models.Equipment
        subjectKey <- insert (Subject "Piano" True)
        privateCreateClassHandler
          (ClassSessionIn
            (fromIntegral (fromSqlKey studentPartyId))
            (fromIntegral (fromSqlKey teacherPartyId))
            (fromIntegral (fromSqlKey subjectKey))
            classStart
            classEnd
            (fromIntegral (fromSqlKey nonRoomResourceId))
            Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no es una sala"
        Right _ ->
          expectationFailure "Expected non-room resources to be rejected for class creation"

    it "rejects creating a class with non-positive or missing booking references instead of storing dangling booking links" $ do
      let assertRejected expectedStatus expectedMessage rawBookingId = do
            result <- try $ runTrialsInMemory $ do
              now <- liftIO getCurrentTime
              let classStart = addUTCTime 3600 now
                  classEnd = addUTCTime 7200 now
              teacherPartyId <- insertTeacherFixture "Teacher One" now
              studentPartyId <- insertPartyFixture "Student One" now
              roomResourceId <- insertRoomFixture "Sala A" "sala-a"
              subjectKey <- insert (Subject "Piano" True)
              privateCreateClassHandler
                (ClassSessionIn
                  (fromIntegral (fromSqlKey studentPartyId))
                  (fromIntegral (fromSqlKey teacherPartyId))
                  (fromIntegral (fromSqlKey subjectKey))
                  classStart
                  classEnd
                  (fromIntegral (fromSqlKey roomResourceId))
                  (Just rawBookingId))
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` expectedStatus
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected invalid class booking references to be rejected on create"
      assertRejected 400 "bookingId" 0
      assertRejected 404 "Reserva no encontrada" 9999

    it "rejects updating a class to a non-teacher party instead of storing an invalid teacher assignment" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let classStart = addUTCTime 3600 now
            classEnd = addUTCTime 7200 now
        teacherPartyId <- insertTeacherFixture "Teacher One" now
        nonTeacherPartyId <- insertPartyFixture "Student Helper" now
        studentPartyId <- insertPartyFixture "Student One" now
        roomResourceId <- insertRoomFixture "Sala A" "sala-a"
        subjectKey <- insert (Subject "Piano" True)
        classSessionKey <- insert Trials.ClassSession
          { Trials.classSessionStudentId = studentPartyId
          , Trials.classSessionTeacherId = teacherPartyId
          , Trials.classSessionSubjectId = subjectKey
          , Trials.classSessionStartAt = classStart
          , Trials.classSessionEndAt = classEnd
          , Trials.classSessionRoomId = roomResourceId
          , Trials.classSessionBookingId = Nothing
          , Trials.classSessionAttended = False
          , Trials.classSessionPurchaseId = Nothing
          , Trials.classSessionConsumedMinutes = 60
          , Trials.classSessionNotes = Nothing
          }
        privateUpdateClassHandler
          (fromIntegral (fromSqlKey classSessionKey))
          (ClassSessionUpdate
            (Just (fromIntegral (fromSqlKey nonTeacherPartyId)))
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no está registrada como profesor"
        Right _ ->
          expectationFailure "Expected non-teacher parties to be rejected for class updates"

    it "rejects updating a class to a non-room resource instead of accepting an impossible room reference" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        let classStart = addUTCTime 3600 now
            classEnd = addUTCTime 7200 now
        teacherPartyId <- insertTeacherFixture "Teacher One" now
        studentPartyId <- insertPartyFixture "Student One" now
        roomResourceId <- insertRoomFixture "Sala A" "sala-a"
        nonRoomResourceId <- insertResourceFixture "PA Rack" "pa-rack" Models.Equipment
        subjectKey <- insert (Subject "Piano" True)
        classSessionKey <- insert Trials.ClassSession
          { Trials.classSessionStudentId = studentPartyId
          , Trials.classSessionTeacherId = teacherPartyId
          , Trials.classSessionSubjectId = subjectKey
          , Trials.classSessionStartAt = classStart
          , Trials.classSessionEndAt = classEnd
          , Trials.classSessionRoomId = roomResourceId
          , Trials.classSessionBookingId = Nothing
          , Trials.classSessionAttended = False
          , Trials.classSessionPurchaseId = Nothing
          , Trials.classSessionConsumedMinutes = 60
          , Trials.classSessionNotes = Nothing
          }
        privateUpdateClassHandler
          (fromIntegral (fromSqlKey classSessionKey))
          (ClassSessionUpdate
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            (Just (fromIntegral (fromSqlKey nonRoomResourceId)))
            Nothing
            Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 422
          BL8.unpack (errBody err) `shouldContain` "no es una sala"
        Right _ ->
          expectationFailure "Expected non-room resources to be rejected for class updates"

    it "rejects updating a class to missing student or subject references before persisting orphaned rows" $ do
      let assertRejected expectedMessage buildUpdate = do
            result <- try $ runTrialsInMemory $ do
              now <- liftIO getCurrentTime
              let classStart = addUTCTime 3600 now
                  classEnd = addUTCTime 7200 now
              teacherPartyId <- insertTeacherFixture "Teacher One" now
              studentPartyId <- insertPartyFixture "Student One" now
              roomResourceId <- insertRoomFixture "Sala A" "sala-a"
              subjectKey <- insert (Subject "Piano" True)
              classSessionKey <- insert Trials.ClassSession
                { Trials.classSessionStudentId = studentPartyId
                , Trials.classSessionTeacherId = teacherPartyId
                , Trials.classSessionSubjectId = subjectKey
                , Trials.classSessionStartAt = classStart
                , Trials.classSessionEndAt = classEnd
                , Trials.classSessionRoomId = roomResourceId
                , Trials.classSessionBookingId = Nothing
                , Trials.classSessionAttended = False
                , Trials.classSessionPurchaseId = Nothing
                , Trials.classSessionConsumedMinutes = 60
                , Trials.classSessionNotes = Nothing
                }
              privateUpdateClassHandler
                (fromIntegral (fromSqlKey classSessionKey))
                (buildUpdate teacherPartyId studentPartyId subjectKey roomResourceId)
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` 404
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected missing class references to be rejected on update"
      assertRejected "Estudiante no encontrado" $
        \_ _ _ _ ->
          ClassSessionUpdate
            Nothing
            Nothing
            (Just 9999)
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
      assertRejected "Materia no encontrada" $
        \_ _ _ _ ->
          ClassSessionUpdate
            Nothing
            (Just 9999)
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing

    it "rejects updating a class with non-positive or missing booking references instead of keeping dangling booking links" $ do
      let assertRejected expectedStatus expectedMessage rawBookingId = do
            result <- try $ runTrialsInMemory $ do
              now <- liftIO getCurrentTime
              let classStart = addUTCTime 3600 now
                  classEnd = addUTCTime 7200 now
              teacherPartyId <- insertTeacherFixture "Teacher One" now
              studentPartyId <- insertPartyFixture "Student One" now
              roomResourceId <- insertRoomFixture "Sala A" "sala-a"
              subjectKey <- insert (Subject "Piano" True)
              classSessionKey <- insert Trials.ClassSession
                { Trials.classSessionStudentId = studentPartyId
                , Trials.classSessionTeacherId = teacherPartyId
                , Trials.classSessionSubjectId = subjectKey
                , Trials.classSessionStartAt = classStart
                , Trials.classSessionEndAt = classEnd
                , Trials.classSessionRoomId = roomResourceId
                , Trials.classSessionBookingId = Nothing
                , Trials.classSessionAttended = False
                , Trials.classSessionPurchaseId = Nothing
                , Trials.classSessionConsumedMinutes = 60
                , Trials.classSessionNotes = Nothing
                }
              privateUpdateClassHandler
                (fromIntegral (fromSqlKey classSessionKey))
                (ClassSessionUpdate
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  (Just rawBookingId)
                  Nothing)
            case result of
              Left err -> do
                errHTTPCode err `shouldBe` expectedStatus
                BL8.unpack (errBody err) `shouldContain` expectedMessage
              Right _ ->
                expectationFailure "Expected invalid class booking references to be rejected on update"
      assertRejected 400 "bookingId" 0
      assertRejected 404 "Reserva no encontrada" 9999

  describe "private student creates" $ do
    it "rejects blank names instead of falling back to email display names" $ do
      result <- try $ runTrialsInMemory $
        privateStudentCreateHandler (StudentCreate "   " "student@example.com" Nothing Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "El nombre es obligatorio"
        Right _ ->
          expectationFailure "Expected blank student names to be rejected"

  describe "private student updates" $ do
    it "rejects duplicate emails instead of letting two parties claim the same contact identity" $ do
      result <- try $ runTrialsInMemory $ do
        now <- liftIO getCurrentTime
        _ <- insert Models.Party
          { Models.partyLegalName = Nothing
          , Models.partyDisplayName = "Existing Student"
          , Models.partyIsOrg = False
          , Models.partyTaxId = Nothing
          , Models.partyPrimaryEmail = Just "taken@example.com"
          , Models.partyPrimaryPhone = Nothing
          , Models.partyWhatsapp = Nothing
          , Models.partyInstagram = Nothing
          , Models.partyEmergencyContact = Nothing
          , Models.partyNotes = Nothing
          , Models.partyCreatedAt = now
          }
        targetStudentId <- insert Models.Party
          { Models.partyLegalName = Nothing
          , Models.partyDisplayName = "Target Student"
          , Models.partyIsOrg = False
          , Models.partyTaxId = Nothing
          , Models.partyPrimaryEmail = Just "target@example.com"
          , Models.partyPrimaryPhone = Nothing
          , Models.partyWhatsapp = Nothing
          , Models.partyInstagram = Nothing
          , Models.partyEmergencyContact = Nothing
          , Models.partyNotes = Nothing
          , Models.partyCreatedAt = now
          }
        privateStudentUpdateHandler
          (fromIntegral (fromSqlKey targetStudentId))
          (StudentUpdate Nothing (Just " Taken@Example.com ") Nothing Nothing)
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 409
          BL8.unpack (errBody err) `shouldContain` "correo ya está asignado"
        Right _ ->
          expectationFailure "Expected duplicate student emails to be rejected"

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
    "CREATE TABLE IF NOT EXISTS \"party_role\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"party_id\" INTEGER NOT NULL,\
    \\"role\" VARCHAR NOT NULL,\
    \\"active\" BOOLEAN NOT NULL,\
    \CONSTRAINT \"unique_party_role\" UNIQUE (\"party_id\", \"role\")\
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
    "CREATE TABLE IF NOT EXISTS \"subject_room_preference\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"subject_id\" INTEGER NOT NULL,\
    \\"room_id\" INTEGER NOT NULL,\
    \\"priority\" INTEGER NOT NULL,\
    \CONSTRAINT \"unique_subject_room\" UNIQUE (\"subject_id\", \"room_id\")\
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
    "CREATE TABLE IF NOT EXISTS \"booking\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"title\" VARCHAR NOT NULL,\
    \\"service_order_id\" INTEGER NULL,\
    \\"party_id\" INTEGER NULL,\
    \\"service_type\" VARCHAR NULL,\
    \\"engineer_party_id\" INTEGER NULL,\
    \\"engineer_name\" VARCHAR NULL,\
    \\"starts_at\" TIMESTAMP NOT NULL,\
    \\"ends_at\" TIMESTAMP NOT NULL,\
    \\"status\" VARCHAR NOT NULL,\
    \\"created_by\" INTEGER NULL,\
    \\"notes\" VARCHAR NULL,\
    \\"created_at\" TIMESTAMP NOT NULL\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"booking_resource\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"booking_id\" INTEGER NOT NULL,\
    \\"resource_id\" INTEGER NOT NULL,\
    \\"role\" VARCHAR NOT NULL,\
    \CONSTRAINT \"unique_booking_res\" UNIQUE (\"booking_id\", \"resource_id\", \"role\")\
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
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"package_catalog\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"subject_id\" INTEGER NOT NULL,\
    \\"name\" VARCHAR NOT NULL,\
    \\"hours_qty\" INTEGER NOT NULL,\
    \\"price_cents\" INTEGER NOT NULL,\
    \\"expires_days\" INTEGER NOT NULL,\
    \\"refund_policy\" VARCHAR NOT NULL,\
    \\"active\" BOOLEAN NOT NULL\
    \)"
    []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"class_package_purchase\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"student_id\" INTEGER NOT NULL,\
    \\"package_id\" INTEGER NOT NULL,\
    \\"price_cents\" INTEGER NOT NULL,\
    \\"discount_cents\" INTEGER NOT NULL,\
    \\"tax_cents\" INTEGER NOT NULL,\
    \\"total_paid_cents\" INTEGER NOT NULL,\
    \\"purchased_at\" TIMESTAMP NOT NULL,\
    \\"seller_id\" INTEGER NULL,\
    \\"commissioned_teacher_id\" INTEGER NULL,\
    \\"trial_request_id\" INTEGER NULL,\
    \\"status\" VARCHAR NOT NULL\
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

runPublicTrialRequestHandler
  :: TrialRequestIn
  -> IO (Either SomeException (Either ServerError TrialRequestOut))
runPublicTrialRequestHandler req =
  runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    liftIO $ runSqlPool initializeTrialsSchema pool
    let handler =
          case trialsServer pool of
            publicServer :<|> _privateServer ->
              case publicServer of
                _signupH :<|> _interestH :<|> trialRequestH :<|> _subjectsH :<|> _trialSlotsH ->
                  trialRequestH req
    liftIO $ try (runHandler handler)

privateQueueHandler :: Maybe Int -> Maybe Text -> SqlPersistT IO [TrialQueueItem]
privateQueueHandler =
  let queueH :<|> _ = privateTrialsServer adminUser
  in queueH

privateAvailabilityListHandler
  :: Maybe Int
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> SqlPersistT IO [TrialAvailabilitySlotDTO]
privateAvailabilityListHandler =
  let _ :<|> _ :<|> _ :<|> availabilityListH :<|> _ = privateTrialsServer adminUser
  in availabilityListH

privateAvailabilityUpsertHandler :: TrialAvailabilityUpsert -> SqlPersistT IO TrialAvailabilitySlotDTO
privateAvailabilityUpsertHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> availabilityUpsertH :<|> _ = privateTrialsServer adminUser
  in availabilityUpsertH

privateAvailabilityDeleteHandler :: Int -> SqlPersistT IO NoContent
privateAvailabilityDeleteHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> availabilityDeleteH :<|> _ = privateTrialsServer adminUser
  in availabilityDeleteH

privateTeacherClassesHandler
  :: Int
  -> Maybe Int
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> SqlPersistT IO [ClassSessionDTO]
privateTeacherClassesHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _
        :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> teacherClassesH :<|> _ =
          privateTrialsServer adminUser
  in teacherClassesH

privateClassSessionsListHandler
  :: Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Maybe Text
  -> SqlPersistT IO [ClassSessionDTO]
privateClassSessionsListHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _
        :<|> classSessionsListH :<|> _ =
          privateTrialsServer adminUser
  in classSessionsListH

privatePackagesHandler :: Maybe Int -> SqlPersistT IO [PackageDTO]
privatePackagesHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> packagesH :<|> _ =
        privateTrialsServer adminUser
  in packagesH

privatePurchaseHandler :: PurchaseIn -> SqlPersistT IO PurchaseOut
privatePurchaseHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> purchaseH :<|> _ =
        privateTrialsServer adminUser
  in purchaseH

privateScheduleHandler :: TrialScheduleIn -> SqlPersistT IO TrialRequestOut
privateScheduleHandler =
  let _ :<|> _ :<|> scheduleH :<|> _ = privateTrialsServer adminUser
  in scheduleH

privateAssignHandler :: Int -> TrialAssignIn -> SqlPersistT IO TrialRequestOut
privateAssignHandler =
  let _ :<|> assignH :<|> _ :<|> _ = privateTrialsServer adminUser
  in assignH

privateCreateClassHandler :: ClassSessionIn -> SqlPersistT IO ClassSessionOut
privateCreateClassHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> createH :<|> _ =
        privateTrialsServer adminUser
  in createH

privateUpdateClassHandler :: Int -> ClassSessionUpdate -> SqlPersistT IO ClassSessionDTO
privateUpdateClassHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> updateH :<|> _ =
        privateTrialsServer adminUser
  in updateH

privateStudentCreateHandler :: StudentCreate -> SqlPersistT IO StudentDTO
privateStudentCreateHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _
        :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _
        :<|> _ :<|> _ :<|> _ :<|> _ :<|> studentsH :<|> createH :<|> _ =
          privateTrialsServer adminUser
  in studentsH `seq` createH

privateStudentUpdateHandler :: Int -> StudentUpdate -> SqlPersistT IO StudentDTO
privateStudentUpdateHandler =
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> studentsH :<|> _ :<|> updateH =
        privateTrialsServer adminUser
  in studentsH `seq` updateH

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

insertTeacherFixture :: Text -> UTCTime -> SqlPersistT IO Models.PartyId
insertTeacherFixture displayName now = do
  partyId <- insertPartyFixture displayName now
  _ <- insert Models.PartyRole
    { Models.partyRolePartyId = partyId
    , Models.partyRoleRole = Models.Teacher
    , Models.partyRoleActive = True
    }
  pure partyId

insertRoomFixture :: Text -> Text -> SqlPersistT IO Models.ResourceId
insertRoomFixture name slug =
  insertResourceFixture name slug Models.Room

insertResourceFixture :: Text -> Text -> Models.ResourceType -> SqlPersistT IO Models.ResourceId
insertResourceFixture name slug resourceType =
  insert Models.Resource
    { Models.resourceName = name
    , Models.resourceSlug = slug
    , Models.resourceResourceType = resourceType
    , Models.resourceCapacity = Nothing
    , Models.resourceActive = True
    }

insertPackageFixture :: Trials.SubjectId -> SqlPersistT IO Trials.PackageCatalogId
insertPackageFixture subjectKey =
  insert Trials.PackageCatalog
    { Trials.packageCatalogSubjectId = subjectKey
    , Trials.packageCatalogName = "Mensual"
    , Trials.packageCatalogHoursQty = 4
    , Trials.packageCatalogPriceCents = 12000
    , Trials.packageCatalogExpiresDays = 30
    , Trials.packageCatalogRefundPolicy = "No reembolsable"
    , Trials.packageCatalogActive = True
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
