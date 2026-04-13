{-# LANGUAGE OverloadedStrings #-}

module TDF.APITypesSpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy (Proxy (..))
import Servant.API (MimeUnrender (mimeUnrender))
import Test.Hspec

import TDF.API.Types (LooseJSON, RolePayload (..))
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
    decodeTrialRequest :: BL8.ByteString -> Either String TrialRequestIn
    decodeTrialRequest = eitherDecode
    isLeft (Left _) = True
    isLeft (Right _) = False
