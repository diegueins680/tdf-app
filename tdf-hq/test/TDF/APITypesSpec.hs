{-# LANGUAGE OverloadedStrings #-}

module TDF.APITypesSpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy (Proxy (..))
import Data.Time (fromGregorian)
import Servant.API (MimeUnrender (mimeUnrender))
import Test.Hspec

import qualified TDF.API as API
import qualified TDF.API.Proposals as Proposals
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

    describe "ChatKitSessionRequest FromJSON" $ do
        it "accepts canonical top-level and nested workflow selectors" $ do
            case decodeChatKitSession "{\"workflowId\":\" wf_primary \"}" of
                Left err ->
                    expectationFailure ("Expected top-level ChatKit workflow selector to decode, got: " <> err)
                Right (API.ChatKitSessionRequest workflowIdVal) ->
                    workflowIdVal `shouldBe` Just "wf_primary"

            case decodeChatKitSession "{\"workflow\":{\"id\":\"wf_nested\"}}" of
                Left err ->
                    expectationFailure ("Expected nested ChatKit workflow selector to decode, got: " <> err)
                Right (API.ChatKitSessionRequest workflowIdVal) ->
                    workflowIdVal `shouldBe` Just "wf_nested"

            case decodeChatKitSession "{\"workflowId\":\"wf_shared\",\"workflow\":{\"id\":\"wf_shared\"}}" of
                Left err ->
                    expectationFailure ("Expected matching ChatKit workflow selectors to decode, got: " <> err)
                Right (API.ChatKitSessionRequest workflowIdVal) ->
                    workflowIdVal `shouldBe` Just "wf_shared"

        it "rejects blank, conflicting, or malformed workflow selectors instead of silently picking a fallback" $ do
            decodeChatKitSession "{\"workflowId\":\"   \"}" `shouldSatisfy` isLeft
            decodeChatKitSession "{\"workflow\":{\"id\":\"   \"}}" `shouldSatisfy` isLeft
            decodeChatKitSession "{\"workflowId\":\"wf_primary\",\"workflow\":{\"id\":\"wf_nested\"}}" `shouldSatisfy` isLeft
            decodeChatKitSession "{\"workflow\":true}" `shouldSatisfy` isLeft
            decodeChatKitSession "{\"workflow\":{}}" `shouldSatisfy` isLeft
            decodeChatKitSession "{\"workflowId\":\"wf_primary\",\"unexpected\":true}" `shouldSatisfy` isLeft
            decodeChatKitSession "{\"workflow\":{\"id\":\"wf_nested\",\"label\":\"default\"}}" `shouldSatisfy` isLeft

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

    describe "Course admin write payload FromJSON" $ do
        it "accepts canonical status, receipt, and upsert payloads" $ do
            case decodeCourseRegistrationStatusUpdate "{\"status\":\"paid\"}" of
                Left err ->
                    expectationFailure ("Expected canonical course status update payload to decode, got: " <> err)
                Right (Courses.CourseRegistrationStatusUpdate statusVal) ->
                    statusVal `shouldBe` "paid"

            case decodeCourseRegistrationReceiptCreate
                "{\"fileUrl\":\"https://files.example.com/receipt.pdf\",\"fileName\":\"receipt.pdf\",\"mimeType\":\"application/pdf\",\"notes\":\"Banco Pichincha\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical course receipt create payload to decode, got: " <> err)
                Right (Courses.CourseRegistrationReceiptCreate fileUrlVal fileNameVal mimeTypeVal notesVal) -> do
                    fileUrlVal `shouldBe` "https://files.example.com/receipt.pdf"
                    fileNameVal `shouldBe` Just "receipt.pdf"
                    mimeTypeVal `shouldBe` Just "application/pdf"
                    notesVal `shouldBe` Just "Banco Pichincha"

            case decodeCourseUpsert
                "{\"slug\":\"production-bootcamp\",\"title\":\"Production Bootcamp\",\"subtitle\":\"Weekend intensive\",\"format\":\"Hybrid\",\"duration\":\"4 weeks\",\"priceCents\":15000,\"currency\":\"USD\",\"capacity\":16,\"sessionStartHour\":15,\"sessionDurationHours\":4,\"locationLabel\":\"TDF HQ\",\"locationMapUrl\":\"https://maps.example.com/tdf\",\"whatsappCtaUrl\":\"https://wa.me/593991234567\",\"landingUrl\":\"https://tdf.example.com/courses/production-bootcamp\",\"daws\":[\"Ableton\"],\"includes\":[\"Mentoring\"],\"instructorName\":\"Ada\",\"instructorBio\":\"Producer\",\"instructorAvatarUrl\":\"https://cdn.example.com/ada.jpg\",\"sessions\":[{\"label\":\"Kickoff\",\"date\":\"2026-05-02\",\"order\":1}],\"syllabus\":[{\"title\":\"Intro\",\"topics\":[\"Ableton\"],\"order\":1}]}"
             of
                Left err ->
                    expectationFailure ("Expected canonical course upsert payload to decode, got: " <> err)
                Right (Courses.CourseUpsert slugVal titleVal _ _ _ priceCentsVal currencyVal capacityVal _ _ _ _ _ _ dawsVal includesVal instructorNameVal _ _ sessionsVal syllabusVal) -> do
                    slugVal `shouldBe` "production-bootcamp"
                    titleVal `shouldBe` "Production Bootcamp"
                    priceCentsVal `shouldBe` 15000
                    currencyVal `shouldBe` "USD"
                    capacityVal `shouldBe` 16
                    dawsVal `shouldBe` ["Ableton"]
                    includesVal `shouldBe` ["Mentoring"]
                    instructorNameVal `shouldBe` Just "Ada"
                    case sessionsVal of
                        [Courses.CourseSessionIn labelVal dateVal orderVal] -> do
                            labelVal `shouldBe` "Kickoff"
                            dateVal `shouldBe` fromGregorian 2026 5 2
                            orderVal `shouldBe` Just 1
                        _ ->
                            expectationFailure ("Expected a single decoded course session, got: " <> show sessionsVal)
                    case syllabusVal of
                        [Courses.CourseSyllabusIn titleVal topicsVal orderVal] -> do
                            titleVal `shouldBe` "Intro"
                            topicsVal `shouldBe` ["Ableton"]
                            orderVal `shouldBe` Just 1
                        _ ->
                            expectationFailure ("Expected a single decoded course syllabus item, got: " <> show syllabusVal)

        it "rejects unexpected top-level or nested keys so course admin writes fail explicitly" $ do
            decodeCourseRegistrationStatusUpdate
                "{\"status\":\"paid\",\"updatedBy\":\"admin\"}"
                `shouldSatisfy` isLeft
            decodeCourseRegistrationReceiptCreate
                "{\"fileUrl\":\"https://files.example.com/receipt.pdf\",\"status\":\"paid\"}"
                `shouldSatisfy` isLeft
            decodeCourseRegistrationReceiptUpdate
                "{\"notes\":\"Replaced file\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodeCourseUpsert
                "{\"slug\":\"production-bootcamp\",\"title\":\"Production Bootcamp\",\"priceCents\":15000,\"currency\":\"USD\",\"capacity\":16,\"daws\":[],\"includes\":[],\"sessions\":[{\"label\":\"Kickoff\",\"date\":\"2026-05-02\",\"unexpected\":true}],\"syllabus\":[],\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodeCourseUpsert
                "{\"slug\":\"production-bootcamp\",\"title\":\"Production Bootcamp\",\"priceCents\":15000,\"currency\":\"USD\",\"capacity\":16,\"daws\":[],\"includes\":[],\"sessions\":[],\"syllabus\":[{\"title\":\"Intro\",\"topics\":[\"Ableton\"],\"extra\":\"typo\"}]}"
                `shouldSatisfy` isLeft

    describe "Proposal payload FromJSON" $ do
        it "accepts canonical create, update, and version payloads" $ do
            case decodeProposalCreate
                "{\"pcTitle\":\"Live session package\",\"pcStatus\":\"draft\",\"pcContactEmail\":\"sales@example.com\",\"pcTemplateKey\":\"tdf_live_sessions\",\"pcVersionNotes\":\"Initial draft\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical proposal create payload to decode, got: " <> err)
                Right (Proposals.ProposalCreate titleVal statusVal _ _ _ contactEmailVal _ _ _ latexVal templateKeyVal versionNotesVal) -> do
                    titleVal `shouldBe` "Live session package"
                    statusVal `shouldBe` Just "draft"
                    contactEmailVal `shouldBe` Just "sales@example.com"
                    latexVal `shouldBe` Nothing
                    templateKeyVal `shouldBe` Just "tdf_live_sessions"
                    versionNotesVal `shouldBe` Just "Initial draft"

            case decodeProposalUpdate
                "{\"puTitle\":\"Updated package\",\"puStatus\":\"sent\",\"puClientPartyId\":42,\"puNotes\":\"Waiting on signature\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical proposal update payload to decode, got: " <> err)
                Right (Proposals.ProposalUpdate titleVal statusVal _ clientPartyIdVal _ _ _ _ notesVal) -> do
                    titleVal `shouldBe` Just "Updated package"
                    statusVal `shouldBe` Just "sent"
                    clientPartyIdVal `shouldBe` Just (Just 42)
                    notesVal `shouldBe` Just (Just "Waiting on signature")

            case decodeProposalVersionCreate
                "{\"pvcTemplateKey\":\"tdf_live_sessions\",\"pvcNotes\":\"Regenerated PDF\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical proposal version payload to decode, got: " <> err)
                Right (Proposals.ProposalVersionCreate latexVal templateKeyVal notesVal) -> do
                    latexVal `shouldBe` Nothing
                    templateKeyVal `shouldBe` Just "tdf_live_sessions"
                    notesVal `shouldBe` Just "Regenerated PDF"

        it "rejects unexpected keys so malformed proposal writes fail explicitly" $ do
            decodeProposalCreate
                "{\"pcTitle\":\"Live session package\",\"pcTemplateKey\":\"tdf_live_sessions\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodeProposalUpdate
                "{\"puStatus\":\"draft\",\"unknownField\":\"typo\"}"
                `shouldSatisfy` isLeft
            decodeProposalVersionCreate
                "{\"pvcTemplateKey\":\"tdf_live_sessions\",\"renderMode\":\"pdf\"}"
                `shouldSatisfy` isLeft

    describe "ServiceMarketplaceBookingReq FromJSON" $ do
        it "accepts canonical service-marketplace booking payloads" $
            case decodeServiceMarketplaceBooking
                "{\"adId\":42,\"slotId\":84,\"title\":\"Mix review\",\"notes\":\"Need feedback on vocal balance\",\"paymentMethod\":\"bank_transfer\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical service-marketplace booking payload to decode, got: " <> err)
                Right (API.ServiceMarketplaceBookingReq adIdVal slotIdVal titleVal notesVal paymentMethodVal) -> do
                    adIdVal `shouldBe` 42
                    slotIdVal `shouldBe` 84
                    titleVal `shouldBe` Just "Mix review"
                    notesVal `shouldBe` Just "Need feedback on vocal balance"
                    paymentMethodVal `shouldBe` Just "bank_transfer"

        it "rejects unexpected booking keys so typoed marketplace forms cannot create partially-understood bookings" $ do
            decodeServiceMarketplaceBooking
                "{\"adId\":42,\"slotId\":84,\"title\":\"Mix review\",\"status\":\"pending\"}"
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
    decodeChatKitSession :: BL8.ByteString -> Either String API.ChatKitSessionRequest
    decodeChatKitSession = eitherDecode
    decodeCourseRegistration = eitherDecode
    decodeFollowUpCreate :: BL8.ByteString -> Either String Courses.CourseRegistrationFollowUpCreate
    decodeFollowUpCreate = eitherDecode
    decodeFollowUpUpdate :: BL8.ByteString -> Either String Courses.CourseRegistrationFollowUpUpdate
    decodeFollowUpUpdate = eitherDecode
    decodeCourseRegistrationStatusUpdate :: BL8.ByteString -> Either String Courses.CourseRegistrationStatusUpdate
    decodeCourseRegistrationStatusUpdate = eitherDecode
    decodeCourseRegistrationReceiptCreate :: BL8.ByteString -> Either String Courses.CourseRegistrationReceiptCreate
    decodeCourseRegistrationReceiptCreate = eitherDecode
    decodeCourseRegistrationReceiptUpdate :: BL8.ByteString -> Either String Courses.CourseRegistrationReceiptUpdate
    decodeCourseRegistrationReceiptUpdate = eitherDecode
    decodeCourseUpsert :: BL8.ByteString -> Either String Courses.CourseUpsert
    decodeCourseUpsert = eitherDecode
    decodeProposalCreate :: BL8.ByteString -> Either String Proposals.ProposalCreate
    decodeProposalCreate = eitherDecode
    decodeProposalUpdate :: BL8.ByteString -> Either String Proposals.ProposalUpdate
    decodeProposalUpdate = eitherDecode
    decodeProposalVersionCreate :: BL8.ByteString -> Either String Proposals.ProposalVersionCreate
    decodeProposalVersionCreate = eitherDecode
    decodeServiceMarketplaceBooking :: BL8.ByteString -> Either String API.ServiceMarketplaceBookingReq
    decodeServiceMarketplaceBooking = eitherDecode
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
