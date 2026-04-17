{-# LANGUAGE OverloadedStrings #-}

module TDF.APITypesSpec (spec) where

import Data.Aeson (eitherDecode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy (Proxy (..))
import Data.Time (fromGregorian)
import Servant.API (MimeUnrender (mimeUnrender))
import Test.Hspec

import qualified TDF.API as API
import qualified TDF.API.Calendar as Calendar
import qualified TDF.API.Facebook as Facebook
import qualified TDF.API.Instagram as Instagram
import qualified TDF.API.InstagramOAuth as InstagramOAuth
import qualified TDF.API.Proposals as Proposals
import qualified TDF.DTO as DTO
import TDF.API.Types
    ( LooseJSON
    , MarketplaceCheckoutReq (..)
    , MarketplaceCartItemUpdate (..)
    , MarketplaceOrderUpdate (..)
    , PaypalCaptureReq (..)
    , PipelineCardCreate (..)
    , PipelineCardUpdate (..)
    , RolePayload (..)
    , ServiceCatalogCreate (..)
    , ServiceCatalogUpdate (..)
    , LabelTrackCreate (..)
    , LabelTrackUpdate (..)
    , UserRoleUpdatePayload (..)
    )
import qualified TDF.Routes.Academy as Academy
import qualified TDF.Routes.Courses as Courses
import TDF.Models (RoleEnum (Admin, Teacher))
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

    describe "UserRoleUpdatePayload FromJSON" $ do
        it "accepts canonical admin role update payloads" $
            case decodeUserRoleUpdate "{\"roles\":[\"Admin\",\"Teacher\"]}" of
                Left err ->
                    expectationFailure ("Expected canonical user role update payload to decode, got: " <> err)
                Right (UserRoleUpdatePayload rolesVal) ->
                    rolesVal `shouldBe` [Admin, Teacher]

        it "rejects unexpected keys so role updates cannot silently ignore over-posted fields" $
            decodeUserRoleUpdate
                "{\"roles\":[\"Admin\"],\"active\":false}"
                `shouldSatisfy` isLeft

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

    describe "TidalAgentRequest FromJSON" $ do
        it "accepts canonical requests and trims explicit prompt/model selectors" $ do
            case decodeTidalAgentRequest "{\"prompt\":\"  make a mellow bassline  \",\"model\":\" gpt-4o-mini \"}" of
                Left err ->
                    expectationFailure ("Expected canonical tidal-agent payload to decode, got: " <> err)
                Right (API.TidalAgentRequest promptVal modelVal) -> do
                    promptVal `shouldBe` "make a mellow bassline"
                    modelVal `shouldBe` Just "gpt-4o-mini"

            case decodeTidalAgentRequest "{\"prompt\":\"play a broken beat\"}" of
                Left err ->
                    expectationFailure ("Expected tidal-agent payload without model to decode, got: " <> err)
                Right (API.TidalAgentRequest promptVal modelVal) -> do
                    promptVal `shouldBe` "play a broken beat"
                    modelVal `shouldBe` Nothing

        it "rejects blank or unexpected fields instead of silently falling back" $ do
            decodeTidalAgentRequest "{\"prompt\":\"   \"}" `shouldSatisfy` isLeft
            decodeTidalAgentRequest "{\"prompt\":\"play a broken beat\",\"model\":\"   \"}" `shouldSatisfy` isLeft
            decodeTidalAgentRequest "{\"prompt\":\"play a broken beat\",\"unexpected\":true}" `shouldSatisfy` isLeft

    describe "social reply request FromJSON" $ do
        it "accepts canonical manual reply payloads" $ do
            case decodeInstagramReply
                "{\"irSenderId\":\"ig-sender\",\"irMessage\":\"Hola\",\"irExternalId\":\"ig-msg-1\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical Instagram reply payload to decode, got: " <> err)
                Right (Instagram.InstagramReplyReq senderVal messageVal externalIdVal) -> do
                    senderVal `shouldBe` "ig-sender"
                    messageVal `shouldBe` "Hola"
                    externalIdVal `shouldBe` Just "ig-msg-1"

            case decodeFacebookReply
                "{\"frSenderId\":\"fb-sender\",\"frMessage\":\"Hola\",\"frExternalId\":\"fb-msg-1\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical Facebook reply payload to decode, got: " <> err)
                Right (Facebook.FacebookReplyReq senderVal messageVal externalIdVal) -> do
                    senderVal `shouldBe` "fb-sender"
                    messageVal `shouldBe` "Hola"
                    externalIdVal `shouldBe` Just "fb-msg-1"

            case decodeWhatsAppReply
                "{\"wrSenderId\":\"+593991234567\",\"wrMessage\":\"Hola\",\"wrExternalId\":\"wa-msg-1\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical WhatsApp reply payload to decode, got: " <> err)
                Right (API.WhatsAppReplyReq senderVal messageVal externalIdVal) -> do
                    senderVal `shouldBe` "+593991234567"
                    messageVal `shouldBe` "Hola"
                    externalIdVal `shouldBe` Just "wa-msg-1"

        it "rejects unexpected reply keys so over-posted social replies fail before dispatch" $ do
            decodeInstagramReply
                "{\"irSenderId\":\"ig-sender\",\"irMessage\":\"Hola\",\"senderId\":\"other\"}"
                `shouldSatisfy` isLeft
            decodeFacebookReply
                "{\"frSenderId\":\"fb-sender\",\"frMessage\":\"Hola\",\"externalId\":\"fb-msg-1\"}"
                `shouldSatisfy` isLeft
            decodeWhatsAppReply
                "{\"wrSenderId\":\"+593991234567\",\"wrMessage\":\"Hola\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

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

    describe "AdsInquiry FromJSON" $ do
        it "accepts canonical public ads inquiry payloads" $
            case decodeAdsInquiry
                "{\"name\":\"Ada Lovelace\",\"email\":\"ada@example.com\",\"phone\":\"+593991234567\",\"course\":\"Ableton\",\"message\":\"Quiero info\",\"channel\":\"instagram\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical ads inquiry payload to decode, got: " <> err)
                Right (API.AdsInquiry nameVal emailVal phoneVal courseVal messageVal channelVal) -> do
                    nameVal `shouldBe` Just "Ada Lovelace"
                    emailVal `shouldBe` Just "ada@example.com"
                    phoneVal `shouldBe` Just "+593991234567"
                    courseVal `shouldBe` Just "Ableton"
                    messageVal `shouldBe` Just "Quiero info"
                    channelVal `shouldBe` Just "instagram"

        it "rejects unexpected keys so public ads inquiries fail explicitly instead of silently dropping input" $ do
            decodeAdsInquiry
                "{\"name\":\"Ada Lovelace\",\"email\":\"ada@example.com\",\"message\":\"Quiero info\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "AdsAssistRequest FromJSON" $ do
        it "accepts canonical public ads assist payloads" $
            case decodeAdsAssist
                "{\"aarMessage\":\"Responder al lead\",\"aarChannel\":\"whatsapp\",\"aarAdId\":42,\"aarCampaignId\":7}"
             of
                Left err ->
                    expectationFailure ("Expected canonical ads assist payload to decode, got: " <> err)
                Right (DTO.AdsAssistRequest adIdVal campaignIdVal messageVal channelVal partyIdVal) -> do
                    adIdVal `shouldBe` Just 42
                    campaignIdVal `shouldBe` Just 7
                    messageVal `shouldBe` "Responder al lead"
                    channelVal `shouldBe` Just "whatsapp"
                    partyIdVal `shouldBe` Nothing

        it "rejects unexpected keys so typoed public assist requests fail before model fallback handling" $
            decodeAdsAssist
                "{\"aarMessage\":\"Responder al lead\",\"message\":\"typoed duplicate\"}"
                `shouldSatisfy` isLeft

    describe "CmsContentIn FromJSON" $ do
        it "accepts canonical CMS content payloads" $
            case decodeCmsContent
                "{\"slug\":\"homepage-hero\",\"locale\":\"en\",\"title\":\"Hero\",\"status\":\"draft\",\"payload\":{\"headline\":\"Create faster\"}}"
             of
                Left err ->
                    expectationFailure ("Expected canonical CMS content payload to decode, got: " <> err)
                Right (API.CmsContentIn slugVal localeVal titleVal statusVal payloadVal) -> do
                    slugVal `shouldBe` "homepage-hero"
                    localeVal `shouldBe` "en"
                    titleVal `shouldBe` Just "Hero"
                    statusVal `shouldBe` Just "draft"
                    payloadVal `shouldBe` Just (object ["headline" .= ("Create faster" :: String)])

        it "rejects unexpected fields so typoed CMS writes fail explicitly instead of becoming partial updates" $ do
            decodeCmsContent
                "{\"slug\":\"homepage-hero\",\"locale\":\"en\",\"payload\":{\"headline\":\"Create faster\"},\"unexpected\":true}"
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
                    nextFollowUpAtVal `shouldBe` Just (Just "2026-05-09T15:00:00Z")

        it "distinguishes omitted follow-up reminder updates from explicit null clears" $ do
            case decodeFollowUpUpdate "{\"notes\":\"Keep note only\"}" of
                Left err ->
                    expectationFailure ("Expected omitted follow-up reminder update to decode, got: " <> err)
                Right (Courses.CourseRegistrationFollowUpUpdate _ _ _ _ _ nextFollowUpAtVal) ->
                    nextFollowUpAtVal `shouldBe` Nothing

            case decodeFollowUpUpdate "{\"notes\":\"Clear reminder\",\"nextFollowUpAt\":null}" of
                Left err ->
                    expectationFailure ("Expected explicit follow-up reminder clear to decode, got: " <> err)
                Right (Courses.CourseRegistrationFollowUpUpdate _ _ _ _ _ nextFollowUpAtVal) ->
                    nextFollowUpAtVal `shouldBe` Just Nothing

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

    describe "PipelineCard payload FromJSON" $ do
        it "accepts canonical pipeline create and patch payloads" $ do
            case decodePipelineCardCreate
                "{\"title\":\"Demo Lead\",\"artist\":\"Ada\",\"stage\":\"Inquiry\",\"sortOrder\":2,\"notes\":\"Needs quote\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical pipeline card create payload to decode, got: " <> err)
                Right (PipelineCardCreate titleVal artistVal stageVal sortOrderVal notesVal) -> do
                    titleVal `shouldBe` "Demo Lead"
                    artistVal `shouldBe` Just "Ada"
                    stageVal `shouldBe` Just "Inquiry"
                    sortOrderVal `shouldBe` Just 2
                    notesVal `shouldBe` Just "Needs quote"

            case decodePipelineCardUpdate
                "{\"title\":\"Final Quote\",\"artist\":null,\"notes\":null}"
             of
                Left err ->
                    expectationFailure ("Expected canonical pipeline card patch payload to decode, got: " <> err)
                Right (PipelineCardUpdate titleVal artistVal stageVal sortOrderVal notesVal) -> do
                    titleVal `shouldBe` Just "Final Quote"
                    artistVal `shouldBe` Just Nothing
                    stageVal `shouldBe` Nothing
                    sortOrderVal `shouldBe` Nothing
                    notesVal `shouldBe` Just Nothing

        it "rejects unexpected keys so typoed pipeline writes cannot turn into partial creates or silent no-op patches" $ do
            decodePipelineCardCreate
                "{\"title\":\"Demo Lead\",\"artist\":\"Ada\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodePipelineCardUpdate
                "{\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodePipelineCardUpdate
                "{\"title\":\"Final Quote\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "InstagramOAuthExchangeRequest FromJSON" $ do
        it "accepts canonical payloads, trims inputs, and preserves the redirect fallback contract" $ do
            case decodeInstagramOAuthExchange
                "{\"code\":\" oauth-code-123 \",\"redirectUri\":\" https://tdf-app.pages.dev/oauth/instagram/callback \"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical Instagram OAuth exchange payload to decode, got: " <> err)
                Right (InstagramOAuth.InstagramOAuthExchangeRequest codeVal redirectUriVal) -> do
                    codeVal `shouldBe` "oauth-code-123"
                    redirectUriVal `shouldBe` Just "https://tdf-app.pages.dev/oauth/instagram/callback"

            case decodeInstagramOAuthExchange
                "{\"code\":\"oauth-code-123\",\"redirectUri\":\"   \"}"
             of
                Left err ->
                    expectationFailure ("Expected blank Instagram OAuth redirectUri to decode as omitted, got: " <> err)
                Right (InstagramOAuth.InstagramOAuthExchangeRequest _ redirectUriVal) ->
                    redirectUriVal `shouldBe` Nothing

        it "rejects blank or typoed request bodies before the handler reaches Facebook with ambiguous input" $ do
            decodeInstagramOAuthExchange
                "{\"code\":\"   \"}"
                `shouldSatisfy` isLeft
            decodeInstagramOAuthExchange
                "{\"code\":\"oauth-code-123\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "Calendar admin request FromJSON" $ do
        it "normalizes canonical token exchange and sync payloads before handlers call Google" $ do
            case decodeCalendarTokenExchange
                "{\"code\":\" oauth-code-123 \",\"redirectUri\":\"   \",\"calendarId\":\" primary \"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical calendar token payload to decode, got: " <> err)
                Right (Calendar.TokenExchangeIn codeVal redirectUriVal calendarIdVal) -> do
                    codeVal `shouldBe` "oauth-code-123"
                    redirectUriVal `shouldBe` Nothing
                    calendarIdVal `shouldBe` "primary"

            case decodeCalendarSync
                "{\"calendarId\":\" primary \",\"from\":\"2026-05-02T15:00:00Z\",\"to\":\"2026-05-02T16:00:00Z\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical calendar sync payload to decode, got: " <> err)
                Right (Calendar.SyncRequest calendarIdVal fromVal toVal) -> do
                    calendarIdVal `shouldBe` "primary"
                    show <$> fromVal `shouldBe` Just "2026-05-02 15:00:00 UTC"
                    show <$> toVal `shouldBe` Just "2026-05-02 16:00:00 UTC"

        it "rejects blank, typoed, or inverted calendar admin bodies before ambiguous Google calls" $ do
            decodeCalendarTokenExchange
                "{\"code\":\"   \",\"calendarId\":\"primary\"}"
                `shouldSatisfy` isLeft
            decodeCalendarTokenExchange
                "{\"code\":\"oauth-code-123\",\"calendarId\":\"   \"}"
                `shouldSatisfy` isLeft
            decodeCalendarTokenExchange
                "{\"code\":\"oauth-code-123\",\"calendarId\":\"primary\",\"syncCursor\":\"stale\"}"
                `shouldSatisfy` isLeft
            decodeCalendarSync
                "{\"calendarId\":\"   \"}"
                `shouldSatisfy` isLeft
            decodeCalendarSync
                "{\"calendarId\":\"primary\",\"from\":\"2026-05-02T16:00:00Z\",\"to\":\"2026-05-02T15:00:00Z\"}"
                `shouldSatisfy` isLeft
            decodeCalendarSync
                "{\"calendarId\":\"primary\",\"status\":\"confirmed\"}"
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

    describe "Service catalog write payload FromJSON" $ do
        it "accepts canonical service catalog create and update payloads, including explicit clear updates" $ do
            case decodeServiceCatalogCreate
                "{\"sccName\":\"Podcast\",\"sccRateCents\":4500,\"sccCurrency\":\"usd\",\"sccBillingUnit\":\"session\",\"sccActive\":true}"
             of
                Left err ->
                    expectationFailure ("Expected canonical service catalog create payload to decode, got: " <> err)
                Right (ServiceCatalogCreate nameVal _ _ rateCentsVal currencyVal billingUnitVal taxBpsVal activeVal) -> do
                    nameVal `shouldBe` "Podcast"
                    rateCentsVal `shouldBe` Just 4500
                    currencyVal `shouldBe` Just "usd"
                    billingUnitVal `shouldBe` Just "session"
                    taxBpsVal `shouldBe` Nothing
                    activeVal `shouldBe` Just True

            case decodeServiceCatalogUpdate
                "{\"scuName\":\"Podcast Pro\",\"scuRateCents\":null,\"scuBillingUnit\":null,\"scuTaxBps\":1200,\"scuActive\":false}"
             of
                Left err ->
                    expectationFailure ("Expected canonical service catalog update payload to decode, got: " <> err)
                Right (ServiceCatalogUpdate nameVal _ _ rateCentsVal _ billingUnitVal taxBpsVal activeVal) -> do
                    nameVal `shouldBe` Just "Podcast Pro"
                    rateCentsVal `shouldBe` Just Nothing
                    billingUnitVal `shouldBe` Just Nothing
                    taxBpsVal `shouldBe` Just (Just 1200)
                    activeVal `shouldBe` Just False

        it "rejects unexpected service catalog write keys instead of silently ignoring caller intent" $ do
            decodeServiceCatalogCreate
                "{\"sccName\":\"Podcast\",\"scActive\":false}"
                `shouldSatisfy` isLeft
            decodeServiceCatalogUpdate
                "{\"scuName\":\"Podcast Pro\",\"scActive\":false}"
                `shouldSatisfy` isLeft
            decodeServiceCatalogUpdate
                "{\"scuRateCents\":null,\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "Service marketplace ad write payloads FromJSON" $ do
        it "accepts canonical service ad and slot creation payloads" $ do
            case decodeServiceAdCreate
                "{\"serviceCatalogId\":9,\"roleTag\":\"mixing\",\"headline\":\"Mix critique\",\"description\":\"Detailed feedback\",\"feeCents\":5000,\"currency\":\"USD\",\"slotMinutes\":90}"
             of
                Left err ->
                    expectationFailure ("Expected canonical service ad create payload to decode, got: " <> err)
                Right (API.ServiceAdCreateReq serviceCatalogIdVal roleTagVal headlineVal descriptionVal feeCentsVal currencyVal slotMinutesVal) -> do
                    serviceCatalogIdVal `shouldBe` Just 9
                    roleTagVal `shouldBe` "mixing"
                    headlineVal `shouldBe` "Mix critique"
                    descriptionVal `shouldBe` Just "Detailed feedback"
                    feeCentsVal `shouldBe` 5000
                    currencyVal `shouldBe` Just "USD"
                    slotMinutesVal `shouldBe` Just 90

            case decodeServiceAdSlotCreate
                "{\"startsAt\":\"2026-05-01T15:00:00Z\",\"endsAt\":\"2026-05-01T16:30:00Z\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical service ad slot create payload to decode, got: " <> err)
                Right (API.ServiceAdSlotCreateReq startsAtVal endsAtVal) -> do
                    show startsAtVal `shouldBe` "2026-05-01 15:00:00 UTC"
                    show endsAtVal `shouldBe` "2026-05-01 16:30:00 UTC"

        it "rejects unexpected keys so service ad writes cannot silently ignore caller intent" $ do
            decodeServiceAdCreate
                "{\"serviceCatalogId\":9,\"roleTag\":\"mixing\",\"headline\":\"Mix critique\",\"feeCents\":5000,\"active\":false}"
                `shouldSatisfy` isLeft
            decodeServiceAdSlotCreate
                "{\"startsAt\":\"2026-05-01T15:00:00Z\",\"endsAt\":\"2026-05-01T16:30:00Z\",\"status\":\"closed\"}"
                `shouldSatisfy` isLeft

    describe "MarketplaceCheckoutReq FromJSON" $ do
        it "accepts canonical marketplace checkout payloads" $
            case decodeMarketplaceCheckout
                "{\"mcrBuyerName\":\"Ada Lovelace\",\"mcrBuyerEmail\":\"ada@example.com\",\"mcrBuyerPhone\":\"+593991234567\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical marketplace checkout payload to decode, got: " <> err)
                Right payload -> do
                    mcrBuyerName payload `shouldBe` "Ada Lovelace"
                    mcrBuyerEmail payload `shouldBe` "ada@example.com"
                    mcrBuyerPhone payload `shouldBe` Just "+593991234567"

        it "rejects unexpected checkout keys so malformed public checkout bodies fail explicitly" $ do
            decodeMarketplaceCheckout
                "{\"mcrBuyerName\":\"Ada Lovelace\",\"mcrBuyerEmail\":\"ada@example.com\",\"mcrBuyerPhone\":\"+593991234567\",\"status\":\"pending\"}"
                `shouldSatisfy` isLeft

    describe "MarketplaceCartItemUpdate FromJSON" $ do
        it "accepts canonical public cart item payloads" $
            case decodeMarketplaceCartItemUpdate
                "{\"mciuListingId\":\"1\",\"mciuQuantity\":2}"
             of
                Left err ->
                    expectationFailure ("Expected marketplace cart item payload to decode, got: " <> err)
                Right payload -> do
                    mciuListingId payload `shouldBe` "1"
                    mciuQuantity payload `shouldBe` 2

        it "rejects unexpected cart item keys so malformed cart writes fail explicitly" $
            decodeMarketplaceCartItemUpdate
                "{\"mciuListingId\":\"1\",\"mciuQuantity\":2,\"status\":\"pending\"}"
                `shouldSatisfy` isLeft

    describe "MarketplaceOrderUpdate FromJSON" $ do
        it "distinguishes omitted nullable fields from explicit clears for admin order updates" $ do
            case decodeMarketplaceOrderUpdate "{\"mouStatus\":\"paid\"}" of
                Left err ->
                    expectationFailure ("Expected marketplace order status update to decode, got: " <> err)
                Right payload -> do
                    mouStatus payload `shouldBe` Just "paid"
                    mouPaymentProvider payload `shouldBe` Nothing
                    mouPaidAt payload `shouldBe` Nothing

            case decodeMarketplaceOrderUpdate "{\"mouPaymentProvider\":null,\"mouPaidAt\":null}" of
                Left err ->
                    expectationFailure ("Expected marketplace order clear update to decode, got: " <> err)
                Right payload -> do
                    mouStatus payload `shouldBe` Nothing
                    mouPaymentProvider payload `shouldBe` Just Nothing
                    mouPaidAt payload `shouldBe` Just Nothing

        it "rejects unexpected order update keys instead of silently ignoring admin intent" $ do
            decodeMarketplaceOrderUpdate
                "{\"mouStatus\":\"paid\",\"status\":\"cancelled\"}"
                `shouldSatisfy` isLeft

    describe "PaypalCaptureReq FromJSON" $ do
        it "accepts canonical PayPal capture payloads" $
            case decodePaypalCapture
                "{\"pcCaptureOrderId\":\"42\",\"pcCapturePaypalId\":\"PAYPAL-ORDER-123\"}"
             of
                Left err ->
                    expectationFailure ("Expected PayPal capture payload to decode, got: " <> err)
                Right payload -> do
                    pcCaptureOrderId payload `shouldBe` "42"
                    pcCapturePaypalId payload `shouldBe` "PAYPAL-ORDER-123"

        it "rejects unexpected capture keys before payment handler validation runs" $
            decodePaypalCapture
                "{\"pcCaptureOrderId\":\"42\",\"pcCapturePaypalId\":\"PAYPAL-ORDER-123\",\"paypalOrderId\":\"TYPOED-DUPLICATE\"}"
                `shouldSatisfy` isLeft

    describe "LabelTrack write payload FromJSON" $ do
        it "accepts canonical label track create and update payloads" $ do
            case decodeLabelTrackCreate
                "{\"ltcTitle\":\"Master final\",\"ltcNote\":\"Check credits\",\"ltcOwnerId\":8}"
             of
                Left err ->
                    expectationFailure ("Expected label track create payload to decode, got: " <> err)
                Right (LabelTrackCreate titleVal noteVal ownerIdVal) -> do
                    titleVal `shouldBe` "Master final"
                    noteVal `shouldBe` Just "Check credits"
                    ownerIdVal `shouldBe` Just 8

            case decodeLabelTrackUpdate
                "{\"ltuTitle\":\"Master delivered\",\"ltuNote\":\"\",\"ltuStatus\":\"done\"}"
             of
                Left err ->
                    expectationFailure ("Expected label track update payload to decode, got: " <> err)
                Right (LabelTrackUpdate titleVal noteVal statusVal) -> do
                    titleVal `shouldBe` Just "Master delivered"
                    noteVal `shouldBe` Just ""
                    statusVal `shouldBe` Just "done"

        it "rejects unexpected keys so owner typos and over-posted updates fail explicitly" $ do
            decodeLabelTrackCreate
                "{\"ltcTitle\":\"Master final\",\"ownerId\":8}"
                `shouldSatisfy` isLeft
            decodeLabelTrackUpdate
                "{\"ltuStatus\":\"done\",\"ltuOwnerId\":8}"
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
    decodeUserRoleUpdate :: BL8.ByteString -> Either String UserRoleUpdatePayload
    decodeUserRoleUpdate = eitherDecode
    decodeChatKitSession :: BL8.ByteString -> Either String API.ChatKitSessionRequest
    decodeChatKitSession = eitherDecode
    decodeTidalAgentRequest :: BL8.ByteString -> Either String API.TidalAgentRequest
    decodeTidalAgentRequest = eitherDecode
    decodeInstagramReply :: BL8.ByteString -> Either String Instagram.InstagramReplyReq
    decodeInstagramReply = eitherDecode
    decodeFacebookReply :: BL8.ByteString -> Either String Facebook.FacebookReplyReq
    decodeFacebookReply = eitherDecode
    decodeWhatsAppReply :: BL8.ByteString -> Either String API.WhatsAppReplyReq
    decodeWhatsAppReply = eitherDecode
    decodeAdsInquiry :: BL8.ByteString -> Either String API.AdsInquiry
    decodeAdsInquiry = eitherDecode
    decodeAdsAssist :: BL8.ByteString -> Either String DTO.AdsAssistRequest
    decodeAdsAssist = eitherDecode
    decodeCmsContent :: BL8.ByteString -> Either String API.CmsContentIn
    decodeCmsContent = eitherDecode
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
    decodePipelineCardCreate :: BL8.ByteString -> Either String PipelineCardCreate
    decodePipelineCardCreate = eitherDecode
    decodePipelineCardUpdate :: BL8.ByteString -> Either String PipelineCardUpdate
    decodePipelineCardUpdate = eitherDecode
    decodeInstagramOAuthExchange :: BL8.ByteString -> Either String InstagramOAuth.InstagramOAuthExchangeRequest
    decodeInstagramOAuthExchange = eitherDecode
    decodeCalendarTokenExchange :: BL8.ByteString -> Either String Calendar.TokenExchangeIn
    decodeCalendarTokenExchange = eitherDecode
    decodeCalendarSync :: BL8.ByteString -> Either String Calendar.SyncRequest
    decodeCalendarSync = eitherDecode
    decodeServiceCatalogCreate :: BL8.ByteString -> Either String ServiceCatalogCreate
    decodeServiceCatalogCreate = eitherDecode
    decodeServiceCatalogUpdate :: BL8.ByteString -> Either String ServiceCatalogUpdate
    decodeServiceCatalogUpdate = eitherDecode
    decodeServiceAdCreate :: BL8.ByteString -> Either String API.ServiceAdCreateReq
    decodeServiceAdCreate = eitherDecode
    decodeServiceAdSlotCreate :: BL8.ByteString -> Either String API.ServiceAdSlotCreateReq
    decodeServiceAdSlotCreate = eitherDecode
    decodeServiceMarketplaceBooking :: BL8.ByteString -> Either String API.ServiceMarketplaceBookingReq
    decodeServiceMarketplaceBooking = eitherDecode
    decodeMarketplaceCheckout :: BL8.ByteString -> Either String MarketplaceCheckoutReq
    decodeMarketplaceCheckout = eitherDecode
    decodeMarketplaceCartItemUpdate :: BL8.ByteString -> Either String MarketplaceCartItemUpdate
    decodeMarketplaceCartItemUpdate = eitherDecode
    decodeMarketplaceOrderUpdate :: BL8.ByteString -> Either String MarketplaceOrderUpdate
    decodeMarketplaceOrderUpdate = eitherDecode
    decodePaypalCapture :: BL8.ByteString -> Either String PaypalCaptureReq
    decodePaypalCapture = eitherDecode
    decodeLabelTrackCreate :: BL8.ByteString -> Either String LabelTrackCreate
    decodeLabelTrackCreate = eitherDecode
    decodeLabelTrackUpdate :: BL8.ByteString -> Either String LabelTrackUpdate
    decodeLabelTrackUpdate = eitherDecode
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
