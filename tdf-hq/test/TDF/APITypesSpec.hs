{-# LANGUAGE OverloadedStrings #-}

module TDF.APITypesSpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy (Proxy (..))
import Servant.API (MimeUnrender (mimeUnrender))
import Test.Hspec

import TDF.API.Types (LooseJSON, RolePayload (..))
import qualified TDF.Routes.Academy as Academy
import qualified TDF.Routes.Courses as Courses
import TDF.Trials.DTO (TrialRequestIn (..))

spec :: Spec
spec = do
    describe "RolePayload FromJSON" $ do
        it "parses raw string payloads" $
            decodeRole "\"Engineer\"" `shouldBe` Right (RolePayload "Engineer")

        it "parses object payloads that provide the role field" $
            decodeRole "{\"role\":\"Teacher\"}" `shouldBe` Right (RolePayload "Teacher")

        it "parses object payloads that provide a value field" $
            decodeRole "{\"value\":\"Artist\"}" `shouldBe` Right (RolePayload "Artist")

        it "fails when neither role nor value is present" $
            decodeRole "{}" `shouldSatisfy` isLeft

        it "fails when both role and value are present to avoid ambiguous role assignment bodies" $
            decodeRole "{\"role\":\"Teacher\",\"value\":\"Artist\"}" `shouldSatisfy` isLeft

    describe "RolePayload LooseJSON MimeUnrender" $ do
        it "still accepts plain text role bodies sent as application/json" $
            decodeLooseRole "Teacher" `shouldBe` Right (RolePayload "Teacher")

        it "rejects malformed or ambiguous JSON-like bodies instead of treating them as raw role text" $ do
            decodeLooseRole "{\"role\":\"Teacher\",\"value\":\"Artist\"}" `shouldSatisfy` isLeft
            decodeLooseRole "{}" `shouldSatisfy` isLeft

    describe "CourseRegistrationRequest FromJSON" $ do
        it "accepts canonical public course registration payloads" $
            case decodeCourseRegistration
                "{\"fullName\":\"Ada Lovelace\",\"email\":\"ada@example.com\",\"phoneE164\":\"+593991234567\",\"source\":\"landing\",\"howHeard\":\"instagram\",\"utm\":{\"source\":\"ig\",\"medium\":\"social\",\"campaign\":\"launch\",\"content\":\"reel\"}}"
             of
                Left err ->
                    expectationFailure ("Expected canonical course registration payload to decode, got: " <> err)
                Right (Courses.CourseRegistrationRequest fullNameVal emailVal phoneVal sourceVal howHeardVal utmVal) -> do
                        fullNameVal `shouldBe` Just "Ada Lovelace"
                        emailVal `shouldBe` Just "ada@example.com"
                        phoneVal `shouldBe` Just "+593991234567"
                        sourceVal `shouldBe` "landing"
                        howHeardVal `shouldBe` Just "instagram"
                        case utmVal of
                            Nothing ->
                                expectationFailure "Expected canonical course registration payload to preserve utm tags"
                            Just (Courses.UTMTags utmSourceVal utmMediumVal utmCampaignVal utmContentVal) -> do
                                    utmSourceVal `shouldBe` Just "ig"
                                    utmMediumVal `shouldBe` Just "social"
                                    utmCampaignVal `shouldBe` Just "launch"
                                    utmContentVal `shouldBe` Just "reel"

        it "rejects unexpected top-level or nested utm keys so malformed registration bodies fail explicitly" $ do
            decodeCourseRegistration
                "{\"fullName\":\"Ada Lovelace\",\"email\":\"ada@example.com\",\"source\":\"landing\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodeCourseRegistration
                "{\"fullName\":\"Ada Lovelace\",\"email\":\"ada@example.com\",\"source\":\"landing\",\"utm\":{\"source\":\"ig\",\"campaign\":\"launch\",\"extra\":\"typo\"}}"
                `shouldSatisfy` isLeft

    describe "CourseRegistrationFollowUp payload FromJSON" $ do
        it "accepts canonical follow-up create and update payloads" $ do
            case decodeFollowUpCreate
                "{\"entryType\":\"call\",\"subject\":\"Confirm payment\",\"notes\":\"Client asked to pay tomorrow\",\"attachmentUrl\":\"https://files.example.com/receipt.pdf\",\"attachmentName\":\"receipt.pdf\",\"nextFollowUpAt\":\"2026-05-02T15:00:00Z\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical course follow-up create payload to decode, got: " <> err)
                Right (Courses.CourseRegistrationFollowUpCreate entryTypeVal subjectVal notesVal attachmentUrlVal attachmentNameVal nextFollowUpAtVal) -> do
                    entryTypeVal `shouldBe` Just "call"
                    subjectVal `shouldBe` Just "Confirm payment"
                    notesVal `shouldBe` "Client asked to pay tomorrow"
                    attachmentUrlVal `shouldBe` Just "https://files.example.com/receipt.pdf"
                    attachmentNameVal `shouldBe` Just "receipt.pdf"
                    nextFollowUpAtVal `shouldBe` Just "2026-05-02T15:00:00Z"

            case decodeFollowUpUpdate
                "{\"notes\":\"Moved reminder to next week\",\"nextFollowUpAt\":\"2026-05-09T15:00:00Z\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical course follow-up update payload to decode, got: " <> err)
                Right (Courses.CourseRegistrationFollowUpUpdate entryTypeVal subjectVal notesVal attachmentUrlVal attachmentNameVal nextFollowUpAtVal) -> do
                    entryTypeVal `shouldBe` Nothing
                    subjectVal `shouldBe` Nothing
                    notesVal `shouldBe` Just "Moved reminder to next week"
                    attachmentUrlVal `shouldBe` Nothing
                    attachmentNameVal `shouldBe` Nothing
                    nextFollowUpAtVal `shouldBe` Just "2026-05-09T15:00:00Z"

        it "rejects unexpected keys so malformed follow-up writes fail explicitly" $ do
            decodeFollowUpCreate
                "{\"entryType\":\"call\",\"notes\":\"Client asked to pay tomorrow\",\"status\":\"pending\"}"
                `shouldSatisfy` isLeft
            decodeFollowUpUpdate
                "{\"notes\":\"Moved reminder to next week\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "Academy request FromJSON" $ do
        it "accepts canonical academy enroll, progress, and referral-claim payloads" $ do
            case decodeEnroll
                "{\"email\":\"ada@example.com\",\"role\":\"artist\",\"platform\":\"instagram\",\"referralCode\":\"REF-42\"}" of
                Left err ->
                    expectationFailure ("Expected canonical academy enroll payload to decode, got: " <> err)
                Right (Academy.EnrollReq emailVal roleVal platformVal referralCodeVal) -> do
                    emailVal `shouldBe` "ada@example.com"
                    roleVal `shouldBe` "artist"
                    platformVal `shouldBe` Just "instagram"
                    referralCodeVal `shouldBe` Just "REF-42"

            case decodeProgress
                "{\"email\":\"ada@example.com\",\"slug\":\"mixing-basics\",\"day\":3}" of
                Left err ->
                    expectationFailure ("Expected canonical academy progress payload to decode, got: " <> err)
                Right (Academy.ProgressReq emailVal slugVal dayVal) -> do
                    emailVal `shouldBe` "ada@example.com"
                    slugVal `shouldBe` "mixing-basics"
                    dayVal `shouldBe` 3

            case decodeReferralClaim
                "{\"email\":\"ada@example.com\",\"code\":\"REF-42\"}" of
                Left err ->
                    expectationFailure ("Expected canonical academy referral-claim payload to decode, got: " <> err)
                Right (Academy.ReferralClaimReq emailVal codeVal) -> do
                    emailVal `shouldBe` "ada@example.com"
                    codeVal `shouldBe` "REF-42"

        it "rejects unexpected keys so malformed academy bodies fail explicitly" $ do
            decodeEnroll
                "{\"email\":\"ada@example.com\",\"role\":\"artist\",\"platform\":\"instagram\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodeProgress
                "{\"email\":\"ada@example.com\",\"slug\":\"mixing-basics\",\"day\":3,\"completedAt\":\"2026-05-01\"}"
                `shouldSatisfy` isLeft
            decodeReferralClaim
                "{\"email\":\"ada@example.com\",\"code\":\"REF-42\",\"status\":\"claimed\"}"
                `shouldSatisfy` isLeft

    describe "TrialRequestIn FromJSON" $ do
        it "accepts canonical public trial request payloads" $
            case decodeTrialRequest
                "{\"subjectId\":7,\"preferred\":[{\"startAt\":\"2026-05-01T15:00:00Z\",\"endAt\":\"2026-05-01T16:00:00Z\"},{\"startAt\":\"2026-05-02T17:00:00Z\",\"endAt\":\"2026-05-02T18:00:00Z\"}],\"notes\":\"Afternoons preferred\",\"fullName\":\"Ada Lovelace\",\"email\":\"ada@example.com\",\"phone\":\"+593991234567\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical trial request payload to decode, got: " <> err)
                Right (TrialRequestIn partyIdVal subjectIdVal preferredVal notesVal fullNameVal emailVal phoneVal) -> do
                    partyIdVal `shouldBe` Nothing
                    subjectIdVal `shouldBe` 7
                    length preferredVal `shouldBe` 2
                    notesVal `shouldBe` Just "Afternoons preferred"
                    fullNameVal `shouldBe` Just "Ada Lovelace"
                    emailVal `shouldBe` Just "ada@example.com"
                    phoneVal `shouldBe` Just "+593991234567"

        it "rejects unexpected top-level or nested slot keys so malformed trial requests fail explicitly" $ do
            decodeTrialRequest
                "{\"subjectId\":7,\"preferred\":[{\"startAt\":\"2026-05-01T15:00:00Z\",\"endAt\":\"2026-05-01T16:00:00Z\"}],\"status\":\"Requested\"}"
                `shouldSatisfy` isLeft
            decodeTrialRequest
                "{\"subjectId\":7,\"preferred\":[{\"startAt\":\"2026-05-01T15:00:00Z\",\"endAt\":\"2026-05-01T16:00:00Z\",\"label\":\"after work\"}]}"
                `shouldSatisfy` isLeft
  where
    decodeRole = eitherDecode
    decodeLooseRole = mimeUnrender (Proxy :: Proxy LooseJSON)
    decodeCourseRegistration = eitherDecode
    decodeFollowUpCreate :: BL8.ByteString -> Either String Courses.CourseRegistrationFollowUpCreate
    decodeFollowUpCreate = eitherDecode
    decodeFollowUpUpdate :: BL8.ByteString -> Either String Courses.CourseRegistrationFollowUpUpdate
    decodeFollowUpUpdate = eitherDecode
    decodeEnroll :: BL8.ByteString -> Either String Academy.EnrollReq
    decodeEnroll = eitherDecode
    decodeProgress :: BL8.ByteString -> Either String Academy.ProgressReq
    decodeProgress = eitherDecode
    decodeReferralClaim :: BL8.ByteString -> Either String Academy.ReferralClaimReq
    decodeReferralClaim = eitherDecode
    decodeTrialRequest :: BL8.ByteString -> Either String TrialRequestIn
    decodeTrialRequest = eitherDecode
    isLeft (Left _) = True
    isLeft (Right _) = False
