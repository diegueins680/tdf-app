{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (eitherDecode, (.=))
import qualified Data.Aeson as A
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text
import Data.Time (UTCTime (..), addDays, addUTCTime, fromGregorian, secondsToDiffTime)
import Database.Persist.Sql (toSqlKey)
import Servant (ServerError (..))
import Servant.Multipart (FromMultipart (fromMultipart), Input (..), MultipartData (..), Tmp)
import Test.Hspec

import TDF.API.LiveSessions (LiveSessionIntakePayload (..))
import TDF.API.WhatsApp
    ( CompleteReq (..),
      ensureLeadCompletionUpdated,
      validateHookVerifyRequest,
      validateLeadCompletionLookup,
      validateLeadCompletionRequest )
import qualified TDF.APITypesSpec as APITypesSpec
import TDF.Cron (Directive (..), parseDirective)
import TDF.DTO.SocialEventsDTO
    ( EventMetadataUpdateDTO (..),
      EventUpdateDTO (..),
      InvitationUpdateDTO (..),
      NullableFieldUpdate (..),
      TicketCheckInRequestDTO (..),
      VenueUpdateDTO (..),
      eudMetadataUpdate,
      emuBudgetCents,
      emuTicketUrl,
      iudMessageUpdate,
      vcuPhone,
      vudContactUpdate )
import TDF.Models.SocialEventsModels (EventInvitationId, SocialEventId)
import qualified TDF.Profiles.ArtistSpec as ArtistSpec
import qualified TDF.ServerAdminSpec as ServerAdminSpec
import TDF.ServerRadio (validateRadioStreamUrl)
import TDF.RagStore (availabilityOverlaps, validateEmbeddingModelDimensions)
import TDF.ServerAdmin (parseSocialErrorsChannel)
import TDF.Contracts.Server (validateContractId, validateContractPayload)
import TDF.ServerInternships
    ( validateInternProjectStatusInput,
      validateInternTaskProgressUpdate,
      validateOptionalInternProjectStatusInput,
      validateOptionalInternTaskStatusInput )
import TDF.ServerProposals
    ( validateOptionalProposalContactEmail,
      validateOptionalProposalStatus,
      validateProposalStatus,
      validateTemplateKey )
import TDF.ServerFeedback (normalizeOptionalFeedbackText, validateOptionalFeedbackContactEmail)
import TDF.Server.SocialEventsHandlers (
    normalizeBudgetLineType,
    normalizeEventStatus,
    normalizeEventType,
    normalizeFinanceDirection,
    normalizeFinanceEntryStatus,
    normalizeFinanceSource,
    normalizeArtistGenres,
    normalizeInvitationStatus,
    normalizeMomentCaption,
    normalizeMomentCommentBody,
    normalizeMomentMediaType,
    normalizeMomentReaction,
    normalizePositivePartyIdText,
    parseFollowerQueryParamEither,
    normalizeTicketOrderStatus,
    normalizeTicketStatus,
    parseNearQueryEither,
    parseInvitationIdsEither,
    TicketCheckInLookup (..),
    validateRsvpStatus,
    validateTicketCheckInLookup,
    validateEventCurrencyInput,
    validateEventCreateTypeStatus,
    validateEventMetadataUpdate,
 )
import qualified TDF.ServerSpec as ServerSpec
import qualified TDF.ServerExtraSpec as ServerExtraSpec
import qualified TDF.Social.FollowHandlerSpec as FollowHandlerSpec
import qualified TDF.Social.FollowSpec as FollowSpec
import qualified TDF.Trials.PublicLeadSpec as PublicLeadSpec

main :: IO ()
main = hspec $ do
    describe "normalizeOptionalFeedbackText" $ do
        it "trims meaningful optional feedback metadata values" $ do
            normalizeOptionalFeedbackText (Just "  bug ") `shouldBe` Just "bug"
            normalizeOptionalFeedbackText (Just " P2 ") `shouldBe` Just "P2"
            normalizeOptionalFeedbackText (Just " user@example.com ") `shouldBe` Just "user@example.com"

        it "drops explicit blank feedback metadata values instead of storing ambiguous empty strings" $ do
            normalizeOptionalFeedbackText Nothing `shouldBe` Nothing
            normalizeOptionalFeedbackText (Just "   ") `shouldBe` Nothing

    describe "validateOptionalFeedbackContactEmail" $ do
        it "normalizes valid optional feedback contact emails and keeps blanks unset" $ do
            validateOptionalFeedbackContactEmail Nothing `shouldBe` Right Nothing
            validateOptionalFeedbackContactEmail (Just "   ") `shouldBe` Right Nothing
            validateOptionalFeedbackContactEmail (Just " User@Example.com ")
                `shouldBe` Right (Just "user@example.com")

        it "rejects malformed feedback contact emails instead of storing unusable contact data" $ do
            let assertInvalid raw = case validateOptionalFeedbackContactEmail (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "contactEmail must be a valid email address"
                    Right value ->
                        expectationFailure ("Expected invalid feedback contact email to be rejected, got " <> show value)
            assertInvalid "not-an-email"
            assertInvalid "user@example..com"
            assertInvalid "user@-example.com"
            assertInvalid "user@example-.com"

    describe "normalizeInvitationStatus" $ do
        it "falls back to pending when missing" $ do
            normalizeInvitationStatus Nothing `shouldBe` "pending"

        it "trims and lowercases non-empty statuses" $ do
            normalizeInvitationStatus (Just "  Accepted ") `shouldBe` "accepted"

        it "treats blank strings as pending" $ do
            normalizeInvitationStatus (Just "   ") `shouldBe` "pending"

    describe "validateRsvpStatus" $ do
        it "trims and canonicalizes supported RSVP states" $ do
            validateRsvpStatus " Accepted " `shouldBe` Right "accepted"
            validateRsvpStatus "DECLINED" `shouldBe` Right "declined"
            validateRsvpStatus "maybe" `shouldBe` Right "maybe"

        it "rejects blank or unknown RSVP states instead of persisting arbitrary labels" $ do
            let assertInvalid raw = case validateRsvpStatus raw of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "accepted, declined, maybe"
                    Right value ->
                        expectationFailure ("Expected invalid RSVP status to be rejected, got " <> show value)
            assertInvalid "   "
            assertInvalid "interested"

    describe "normalizeArtistGenres" $ do
        it "trims genres, drops blanks, and deduplicates case-insensitively" $ do
            normalizeArtistGenres ["  Salsa ", "", "salsa", " Rock ", "ROCK", "Pop"] `shouldBe` ["Salsa", "Rock", "Pop"]

    describe "social events update payload parsing" $ do
        it "distinguishes missing metadata fields from explicit nulls for event updates" $ do
            let payload = "{\"eventTitle\":\"Test\",\"eventStart\":\"2026-01-01T00:00:00Z\",\"eventEnd\":\"2026-01-01T01:00:00Z\",\"eventArtists\":[],\"eventTicketUrl\":null,\"eventBudgetCents\":4500}"
            case eitherDecode payload :: Either String EventUpdateDTO of
                Left err -> expectationFailure err
                Right parsed -> do
                    emuTicketUrl (eudMetadataUpdate parsed) `shouldBe` FieldNull
                    emuBudgetCents (eudMetadataUpdate parsed) `shouldBe` FieldValue 4500

        it "captures venue contact nulls and invitation message nulls in update payloads" $ do
            let venuePayload = "{\"venueName\":\"Sala Uno\",\"venuePhone\":null}"
                invitationPayload = "{\"invitationToPartyId\":\"12\",\"invitationMessage\":null}"
            case eitherDecode venuePayload :: Either String VenueUpdateDTO of
                Left err -> expectationFailure err
                Right parsed ->
                    vcuPhone (vudContactUpdate parsed) `shouldBe` FieldNull
            case eitherDecode invitationPayload :: Either String InvitationUpdateDTO of
                Left err -> expectationFailure err
                Right parsed ->
                    iudMessageUpdate parsed `shouldBe` FieldNull

    describe "validateEventMetadataUpdate" $ do
        let baseUpdate = EventMetadataUpdateDTO
                { emuTicketUrl = FieldMissing
                , emuImageUrl = FieldMissing
                , emuIsPublic = FieldMissing
                , emuType = FieldMissing
                , emuStatus = FieldMissing
                , emuCurrency = FieldMissing
                , emuBudgetCents = FieldMissing
                }

        it "normalizes supported event type/status updates and keeps blank values as explicit clears" $ do
            validateEventMetadataUpdate
                baseUpdate
                    { emuType = FieldValue " FESTIVAL "
                    , emuStatus = FieldValue " canceled "
                    , emuCurrency = FieldValue " usd "
                    }
                `shouldBe` Right
                    baseUpdate
                        { emuType = FieldValue "festival"
                        , emuStatus = FieldValue "cancelled"
                        , emuCurrency = FieldValue "USD"
                        }
            validateEventMetadataUpdate baseUpdate { emuType = FieldValue "   " }
                `shouldBe` Right baseUpdate { emuType = FieldNull }
            validateEventMetadataUpdate baseUpdate { emuCurrency = FieldValue "   " }
                `shouldBe` Right baseUpdate { emuCurrency = FieldNull }

        it "rejects invalid explicit event metadata updates instead of silently ignoring them" $ do
            let assertInvalid updateValue expected =
                    case validateEventMetadataUpdate updateValue of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure ("Expected invalid event metadata update to be rejected, got " <> show value)
            assertInvalid
                baseUpdate { emuType = FieldValue "warehouse" }
                "eventType must be one of: party, concert, festival, conference, showcase, other"
            assertInvalid
                baseUpdate { emuStatus = FieldValue "sold_out" }
                "eventStatus must be one of: planning, announced, on_sale, live, completed, cancelled"
            assertInvalid
                baseUpdate { emuCurrency = FieldValue "usdollars" }
                "eventCurrency must be a 3-letter ISO code"

    describe "validateEventCreateTypeStatus" $ do
        it "defaults omitted or blank create values and normalizes supported explicit values" $ do
            validateEventCreateTypeStatus Nothing Nothing
                `shouldBe` Right ("party", "planning")
            validateEventCreateTypeStatus (Just "   ") (Just " canceled ")
                `shouldBe` Right ("party", "cancelled")
            validateEventCreateTypeStatus (Just " FESTIVAL ") Nothing
                `shouldBe` Right ("festival", "planning")

        it "rejects invalid explicit create values instead of silently falling back" $ do
            let assertInvalid result expected =
                    case result of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure ("Expected invalid event create metadata to be rejected, got " <> show value)
            assertInvalid
                (validateEventCreateTypeStatus (Just "warehouse") Nothing)
                "eventType must be one of: party, concert, festival, conference, showcase, other"
            assertInvalid
                (validateEventCreateTypeStatus Nothing (Just "sold_out"))
                "eventStatus must be one of: planning, announced, on_sale, live, completed, cancelled"

    describe "validateEventCurrencyInput" $ do
        it "defaults omitted or blank create currencies to USD and normalizes explicit ISO codes" $ do
            validateEventCurrencyInput Nothing `shouldBe` Right "USD"
            validateEventCurrencyInput (Just "   ") `shouldBe` Right "USD"
            validateEventCurrencyInput (Just " eur ") `shouldBe` Right "EUR"

        it "rejects malformed explicit event currencies instead of storing opaque metadata" $ do
            case validateEventCurrencyInput (Just "usdollars") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "eventCurrency must be a 3-letter ISO code"
                Right value ->
                    expectationFailure ("Expected invalid event currency to be rejected, got " <> show value)

    describe "normalizePositivePartyIdText" $ do
        it "accepts positive numeric ids and canonicalizes them" $ do
            normalizePositivePartyIdText " 0042 " `shouldBe` Just "42"

        it "rejects blank and non-numeric ids" $ do
            normalizePositivePartyIdText "   " `shouldBe` Nothing
            normalizePositivePartyIdText "abc" `shouldBe` Nothing

    describe "moment normalizers" $ do
        it "normalizes supported media types and reactions" $ do
            normalizeMomentMediaType " PHOTO " `shouldBe` Just "image"
            normalizeMomentMediaType "clip" `shouldBe` Just "video"
            normalizeMomentMediaType "audio" `shouldBe` Nothing
            normalizeMomentReaction "heart" `shouldBe` Just "love"
            normalizeMomentReaction "CLAP" `shouldBe` Just "applause"
            normalizeMomentReaction "wow" `shouldBe` Nothing

        it "accepts blank captions as missing and rejects oversize captions" $ do
            normalizeMomentCaption (Just "   ") `shouldBe` Right Nothing
            normalizeMomentCaption (Just "  aftermovie  ") `shouldBe` Right (Just "aftermovie")
            case normalizeMomentCaption (Just (Data.Text.replicate 281 "a")) of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Moment caption must be 280 characters or less"
                Right value ->
                    expectationFailure ("Expected oversize caption to fail, got " <> show value)

        it "requires non-empty comment bodies and enforces comment length" $ do
            normalizeMomentCommentBody "  impecable set  " `shouldBe` Right "impecable set"
            case normalizeMomentCommentBody "   " of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Moment comment body is required"
                Right value ->
                    expectationFailure ("Expected blank moment comment to fail, got " <> show value)
            case normalizeMomentCommentBody (Data.Text.replicate 501 "b") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Moment comment body must be 500 characters or less"
                Right value ->
                    expectationFailure ("Expected oversize moment comment to fail, got " <> show value)

    describe "parseFollowerQueryParamEither" $ do
        it "canonicalizes numeric follower query params before delete lookups" $ do
            parseFollowerQueryParamEither (Just " 0042 ") `shouldBe` Right "42"

        it "rejects missing or blank follower query params" $ do
            case parseFollowerQueryParamEither Nothing of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "follower query param is required"
                Right _ -> expectationFailure "Expected missing follower query param to be rejected"
            case parseFollowerQueryParamEither (Just "   ") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "follower query param is required"
                Right _ -> expectationFailure "Expected blank follower query param to be rejected"

        it "rejects non-positive or non-numeric follower query params" $ do
            case parseFollowerQueryParamEither (Just "abc") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "follower query param must be a positive integer"
                Right _ -> expectationFailure "Expected invalid follower query param to be rejected"
            case parseFollowerQueryParamEither (Just "0") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "follower query param must be a positive integer"
                Right _ -> expectationFailure "Expected non-positive follower query param to be rejected"

    describe "parseInvitationIdsEither" $ do
        it "parses numeric ids into typed keys" $ do
            let expected :: (SocialEventId, EventInvitationId)
                expected = (toSqlKey 42, toSqlKey 77)
            parseInvitationIdsEither "42" "77" `shouldBe` Right expected

        it "returns a ServerError for invalid ids" $ do
            case parseInvitationIdsEither "x" "1" of
                Left err -> BL.unpack (errBody err) `shouldContain` "Invalid event or invitation id"
                Right _ -> expectationFailure "Expected an error for invalid ids"

        it "rejects non-positive ids" $ do
            case parseInvitationIdsEither "0" "-3" of
                Left err -> BL.unpack (errBody err) `shouldContain` "Invalid event or invitation id"
                Right _ -> expectationFailure "Expected an error for non-positive ids"

    describe "parseNearQueryEither" $ do
        it "parses finite coordinates and default radius" $ do
            parseNearQueryEither "-0.18, -78.48" `shouldBe` Right (-0.18, -78.48, 25)

        it "rejects non-finite coordinates and radius values" $ do
            case parseNearQueryEither "NaN,-78.48" of
                Left err -> BL.unpack (errBody err) `shouldContain` "Invalid near latitude"
                Right _ -> expectationFailure "Expected NaN latitude to be rejected"
            case parseNearQueryEither "-0.18,-78.48,Infinity" of
                Left err -> BL.unpack (errBody err) `shouldContain` "Invalid near radiusKm"
                Right _ -> expectationFailure "Expected Infinity radius to be rejected"

    describe "normalizeTicketOrderStatus" $ do
        it "defaults to pending for empty/unknown values" $ do
            normalizeTicketOrderStatus Nothing `shouldBe` "pending"
            normalizeTicketOrderStatus (Just "  ") `shouldBe` "pending"
            normalizeTicketOrderStatus (Just "unknown") `shouldBe` "pending"

        it "normalizes valid statuses" $ do
            normalizeTicketOrderStatus (Just "PAID") `shouldBe` "paid"
            normalizeTicketOrderStatus (Just "canceled") `shouldBe` "cancelled"

    describe "normalizeTicketStatus" $ do
        it "defaults to issued when missing" $ do
            normalizeTicketStatus Nothing `shouldBe` "issued"

        it "normalizes alternate ticket status spellings" $ do
            normalizeTicketStatus (Just "checkedin") `shouldBe` "checked_in"
            normalizeTicketStatus (Just "CANCELED") `shouldBe` "cancelled"

    describe "validateTicketCheckInLookup" $ do
        it "accepts exactly one lookup field and normalizes ticket codes" $ do
            validateTicketCheckInLookup
                TicketCheckInRequestDTO
                    { ticketCheckInTicketId = Just " 42 "
                    , ticketCheckInTicketCode = Nothing
                    }
                `shouldBe` Right (TicketCheckInLookupById "42")
            validateTicketCheckInLookup
                TicketCheckInRequestDTO
                    { ticketCheckInTicketId = Nothing
                    , ticketCheckInTicketCode = Just " ab-123 "
                    }
                `shouldBe` Right (TicketCheckInLookupByCode "AB-123")

        it "rejects ambiguous check-in payloads that provide both id and code" $ do
            case validateTicketCheckInLookup
                TicketCheckInRequestDTO
                    { ticketCheckInTicketId = Just "42"
                    , ticketCheckInTicketCode = Just "ab-123"
                    } of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Provide exactly one of ticketCheckInTicketId or ticketCheckInTicketCode"
                Right value ->
                    expectationFailure ("Expected ambiguous ticket check-in payload to be rejected, got " <> show value)

    describe "validateRadioStreamUrl" $ do
        it "trims surrounding whitespace and accepts http(s) stream URLs" $
            validateRadioStreamUrl "  HTTPS://radio.example.com/live  "
                `shouldBe` Right "HTTPS://radio.example.com/live"

        it "accepts explicit numeric ports, including bracketed IPv6 hosts" $ do
            validateRadioStreamUrl "https://radio.example.com:8443/live"
                `shouldBe` Right "https://radio.example.com:8443/live"
            validateRadioStreamUrl "https://[2001:db8::1]:8000/live"
                `shouldBe` Right "https://[2001:db8::1]:8000/live"

        it "rejects blank stream URLs with a precise 400" $
            case validateRadioStreamUrl "   " of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl is required"
                Right _ -> expectationFailure "Expected blank streamUrl to be rejected"

        it "rejects malformed absolute URLs that would fail later in the radio pipeline" $ do
            case validateRadioStreamUrl "https://" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must include a host"
                Right _ -> expectationFailure "Expected hostless streamUrl to be rejected"
            case validateRadioStreamUrl "https://radio.example.com/live stream" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must not contain whitespace"
                Right _ -> expectationFailure "Expected whitespace-containing streamUrl to be rejected"

        it "rejects hostless or malformed-port URLs before they can be stored" $ do
            case validateRadioStreamUrl "https://:8443/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must include a host"
                Right _ -> expectationFailure "Expected hostless streamUrl to be rejected"
            case validateRadioStreamUrl "https://radio.example.com:/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl port must be numeric"
                Right _ -> expectationFailure "Expected empty-port streamUrl to be rejected"
            case validateRadioStreamUrl "https://radio.example.com:abc/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl port must be numeric"
                Right _ -> expectationFailure "Expected non-numeric port streamUrl to be rejected"

        it "rejects non-http stream URLs before they can be stored" $
            case validateRadioStreamUrl "ftp://radio.example.com/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must be http(s)"
                Right _ -> expectationFailure "Expected non-http streamUrl to be rejected"

    describe "validateTemplateKey" $ do
        it "trims valid proposal template keys before lookup" $
            validateTemplateKey "  tdf_live_sessions  " `shouldBe` Right "tdf_live_sessions"

        it "rejects blank or unsafe template keys with a 400 instead of a missing-template 404" $ do
            let assertInvalid raw expected = case validateTemplateKey raw of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid templateKey to be rejected, got: " <> show value)
            assertInvalid "   " "templateKey required"
            assertInvalid "../proposal" "ASCII letters, numbers, hyphens, or underscores"

    describe "proposal status validation" $ do
        it "defaults omitted create status to draft and normalizes supported explicit statuses" $ do
            validateProposalStatus Nothing `shouldBe` Right "draft"
            validateProposalStatus (Just " Sent ") `shouldBe` Right "sent"
            validateOptionalProposalStatus Nothing `shouldBe` Right Nothing
            validateOptionalProposalStatus (Just " DRAFT ")
                `shouldBe` Right (Just "draft")

        it "rejects blank or unknown proposal statuses instead of persisting UI-opaque values" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "status must be one of: draft, sent"
                    Right value ->
                        expectationFailure ("Expected invalid proposal status to be rejected, got: " <> show value)
            assertInvalid (validateProposalStatus (Just "   "))
            assertInvalid (validateProposalStatus (Just "archived"))
            assertInvalid (validateOptionalProposalStatus (Just "queued"))

    describe "validateOptionalProposalContactEmail" $ do
        it "normalizes valid proposal contact emails and treats blanks as unset" $ do
            validateOptionalProposalContactEmail Nothing `shouldBe` Right Nothing
            validateOptionalProposalContactEmail (Just "   ") `shouldBe` Right Nothing
            validateOptionalProposalContactEmail (Just " Sales@Example.com ")
                `shouldBe` Right (Just "sales@example.com")

        it "rejects malformed proposal contact emails instead of storing unusable CRM contact data" $ do
            let assertInvalid raw = case validateOptionalProposalContactEmail (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "contactEmail must be a valid email address"
                    Right value ->
                        expectationFailure ("Expected invalid proposal contact email to be rejected, got " <> show value)
            assertInvalid "not-an-email"
            assertInvalid "sales@example..com"
            assertInvalid "sales@-example.com"
            assertInvalid "sales@example-.com"

    describe "validateContractId" $ do
        it "accepts UUID-shaped contract ids and canonicalizes surrounding whitespace" $
            validateContractId " 550e8400-e29b-41d4-a716-446655440000 "
                `shouldBe` Right "550e8400-e29b-41d4-a716-446655440000"

        it "rejects non-UUID ids with a 400 instead of falling through to ambiguous lookups" $ do
            let assertInvalid raw = case validateContractId raw of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "Invalid contract id"
                    Right value ->
                        expectationFailure ("Expected invalid contract id to be rejected, got: " <> show value)
            assertInvalid "contract-123"
            assertInvalid "../contracts/store"

    describe "validateContractPayload" $ do
        it "requires object payloads and normalizes the stored contract kind" $ do
            validateContractPayload
                (A.object
                    [ "kind" .= ("  Event_Vendor_Contract  " :: Text)
                    , "amountCents" .= (25000 :: Int)
                    ]
                )
                `shouldBe`
                Right
                    ( "event_vendor_contract"
                    , A.object
                        [ "kind" .= ("event_vendor_contract" :: Text)
                        , "amountCents" .= (25000 :: Int)
                        ]
                    )
            validateContractPayload (A.object ["amountCents" .= (25000 :: Int)])
                `shouldBe`
                Right
                    ( "generic"
                    , A.object
                        [ "kind" .= ("generic" :: Text)
                        , "amountCents" .= (25000 :: Int)
                        ]
                    )

        it "rejects non-object or malformed kind values instead of silently storing generic contracts" $ do
            let assertInvalid payload expected = case validateContractPayload payload of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid contract payload to be rejected, got: " <> show value)
            assertInvalid (A.String "not-an-object") "Contract payload must be a JSON object"
            assertInvalid (A.object ["kind" .= ("" :: Text)]) "Contract payload kind must be a non-empty slug"
            assertInvalid (A.object ["kind" .= ("event vendor" :: Text)]) "Contract payload kind must be a non-empty slug"
            assertInvalid (A.object ["kind" .= A.Null]) "Contract payload kind must be a non-empty slug"
            assertInvalid (A.object ["kind" .= (42 :: Int)]) "Contract payload kind must be a non-empty slug"

    describe "internship status validation" $ do
        it "defaults omitted project statuses and normalizes supported explicit values" $ do
            validateInternProjectStatusInput Nothing `shouldBe` Right "active"
            validateInternProjectStatusInput (Just " COMPLETED ") `shouldBe` Right "completed"
            validateOptionalInternProjectStatusInput Nothing `shouldBe` Right Nothing
            validateOptionalInternProjectStatusInput (Just " paused ")
                `shouldBe` Right (Just "paused")
            validateOptionalInternTaskStatusInput Nothing `shouldBe` Right Nothing
            validateOptionalInternTaskStatusInput (Just " DOING ")
                `shouldBe` Right (Just "doing")

        it "rejects blank or unknown internship statuses instead of storing values the UI cannot map" $ do
            let assertInvalid result expected = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid internship status to be rejected, got " <> show value)
            assertInvalid
                (validateInternProjectStatusInput (Just "   "))
                "projectStatus must be one of: active, paused, completed"
            assertInvalid
                (validateOptionalInternProjectStatusInput (Just "archived"))
                "projectStatus must be one of: active, paused, completed"
            assertInvalid
                (validateOptionalInternTaskStatusInput (Just "review"))
                "taskStatus must be one of: todo, doing, blocked, done"

    describe "internship task progress validation" $ do
        it "preserves omitted progress and accepts explicit values inside the supported range" $ do
            validateInternTaskProgressUpdate Nothing `shouldBe` Right Nothing
            validateInternTaskProgressUpdate (Just 0) `shouldBe` Right (Just 0)
            validateInternTaskProgressUpdate (Just 67) `shouldBe` Right (Just 67)
            validateInternTaskProgressUpdate (Just 100) `shouldBe` Right (Just 100)

        it "rejects out-of-range progress updates instead of silently clamping them" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "taskProgress must be between 0 and 100"
                    Right value ->
                        expectationFailure ("Expected invalid internship task progress to be rejected, got " <> show value)
            assertInvalid (validateInternTaskProgressUpdate (Just (-1)))
            assertInvalid (validateInternTaskProgressUpdate (Just 101))

    describe "event finance normalizers" $ do
        it "normalizes event type and status with safe fallbacks" $ do
            normalizeEventType (Just " FESTIVAL ") `shouldBe` Just "festival"
            normalizeEventType (Just "unknown-type") `shouldBe` Nothing
            normalizeEventStatus (Just "canceled") `shouldBe` Just "cancelled"
            normalizeEventStatus (Just "not-real") `shouldBe` Nothing

        it "normalizes budget and accounting dimensions" $ do
            normalizeBudgetLineType (Just "INCOME") `shouldBe` "income"
            normalizeBudgetLineType (Just "whatever") `shouldBe` "expense"
            normalizeFinanceDirection (Just "income") `shouldBe` "income"
            normalizeFinanceDirection (Just "invalid") `shouldBe` "expense"
            normalizeFinanceSource (Just "VENDOR_PAYMENT") `shouldBe` "vendor_payment"
            normalizeFinanceSource (Just "nonsense") `shouldBe` "manual"
            normalizeFinanceEntryStatus (Just "draft") `shouldBe` "draft"
            normalizeFinanceEntryStatus (Just "bad") `shouldBe` "posted"

    describe "availabilityOverlaps" $ do
        let day = fromGregorian 2025 1 1
            windowStart = UTCTime day (secondsToDiffTime 0)
            windowEnd = UTCTime (addDays 1 day) (secondsToDiffTime 0)

        it "includes events that overlap the start" $ do
            let eventStart = addUTCTime (-3600) windowStart
                eventEnd = addUTCTime 3600 windowStart
            availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` True

        it "includes events that overlap the end" $ do
            let eventStart = addUTCTime (-3600) windowEnd
                eventEnd = addUTCTime 3600 windowEnd
            availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` True

        it "excludes events entirely before the window" $ do
            let eventStart = addUTCTime (-7200) windowStart
                eventEnd = addUTCTime (-3600) windowStart
            availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` False

        it "excludes events entirely after the window" $ do
            let eventStart = addUTCTime 3600 windowEnd
                eventEnd = addUTCTime 7200 windowEnd
            availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` False

    describe "validateEmbeddingModelDimensions" $ do
        it "accepts known embedding models" $ do
            validateEmbeddingModelDimensions "text-embedding-3-small" `shouldBe` Right 1536
            validateEmbeddingModelDimensions "text-embedding-ada-002" `shouldBe` Right 1536
            validateEmbeddingModelDimensions "TEXT-EMBEDDING-3-LARGE" `shouldBe` Right 3072

        it "rejects unknown embedding models" $ do
            validateEmbeddingModelDimensions "mystery-embedder" `shouldSatisfy` isLeft

    describe "parseDirective" $ do
        it "parses SEND/HOLD directives regardless of casing" $ do
            parseDirective "send: Hola!" `shouldBe` Right (Send "Hola!")
            parseDirective "hold: Confirma nombre\nneed: email" `shouldBe` Right (Hold "Confirma nombre" (Just "email"))

        it "allows leading blank lines in HOLD body and keeps NEED optional" $ do
            parseDirective "HOLD:\n\nFalta el teléfono\nNEED:   telefono  " `shouldBe` Right (Hold "Falta el teléfono" (Just "telefono"))
            parseDirective "HOLD: Falta confirmar datos\nNEED:   " `shouldBe` Right (Hold "Falta confirmar datos" Nothing)

        it "requires HOLD reason even when NEED exists" $ do
            parseDirective "HOLD:\nNEED: email" `shouldBe` Left "HOLD directive empty"

    describe "validateHookVerifyRequest" $ do
        it "accepts subscribe verification requests with a matching token" $ do
            validateHookVerifyRequest (Just "SuBsCrIbE") (Just "challenge-123") (Just "secret") (Just "secret")
                `shouldBe` Right "challenge-123"

        it "rejects missing verification query params with precise 400s" $ do
            case validateHookVerifyRequest Nothing (Just "challenge-123") (Just "secret") (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "hub.mode is required"
                Right _ -> expectationFailure "Expected missing hub.mode to be rejected"
            case validateHookVerifyRequest (Just "subscribe") (Just "   ") (Just "secret") (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "hub.challenge is required"
                Right _ -> expectationFailure "Expected blank hub.challenge to be rejected"
            case validateHookVerifyRequest (Just "subscribe") (Just "challenge-123") Nothing (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "hub.verify_token is required"
                Right _ -> expectationFailure "Expected missing hub.verify_token to be rejected"

        it "distinguishes bad mode, token mismatch, and missing server config" $ do
            case validateHookVerifyRequest (Just "publish") (Just "challenge-123") (Just "secret") (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "hub.mode must be subscribe"
                Right _ -> expectationFailure "Expected unsupported hub.mode to be rejected"
            case validateHookVerifyRequest (Just "subscribe") (Just "challenge-123") (Just "wrong-secret") (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 403
                    BL.unpack (errBody err) `shouldContain` "hub.verify_token mismatch"
                Right _ -> expectationFailure "Expected mismatched verify token to be rejected"
            case validateHookVerifyRequest (Just "subscribe") (Just "challenge-123") (Just "secret") Nothing of
                Left err -> do
                    errHTTPCode err `shouldBe` 503
                    BL.unpack (errBody err) `shouldContain` "WhatsApp verify token not configured"
                Right _ -> expectationFailure "Expected missing verify-token config to be rejected"

    describe "validateLeadCompletionRequest" $ do
        it "trims meaningful lead-completion payload fields before persistence" $ do
            validateLeadCompletionRequest (CompleteReq " token-123 " " Ada Lovelace " " ada@example.com ")
                `shouldBe` Right (CompleteReq "token-123" "Ada Lovelace" "ada@example.com")

        it "rejects blank tokens, blank names, and malformed emails with precise 400s" $ do
            let assertInvalid payload expected = case validateLeadCompletionRequest payload of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid lead completion payload to be rejected, got " <> show value)
            assertInvalid (CompleteReq "   " "Ada Lovelace" "ada@example.com") "Completion token is required"
            assertInvalid (CompleteReq "token-123" "   " "ada@example.com") "Invalid name: must be 1-200 characters"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada @example.com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada@example..com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada@-example.com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada@example-.com") "Invalid email format"

    describe "validateLeadCompletionLookup" $ do
        it "allows only matching non-completed lead records" $ do
            validateLeadCompletionLookup "token-123" (Just ("LINK_SENT", Just "token-123"))
                `shouldBe` Right ()

        it "returns explicit 404/403/409 errors for missing, invalid-token, and completed links" $ do
            let assertLookupFailure result expectedStatus expectedBody = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` expectedStatus
                        BL.unpack (errBody err) `shouldContain` expectedBody
                    Right _ ->
                        expectationFailure ("Expected lookup failure with body containing " <> show expectedBody)
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" Nothing)
                404
                "Lead not found"
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" (Just ("LINK_SENT", Just "other-token")))
                403
                "Invalid completion token"
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" (Just ("completed", Nothing)))
                409
                "Lead already completed"

    describe "ensureLeadCompletionUpdated" $ do
        it "requires the completion update to affect exactly one row" $ do
            ensureLeadCompletionUpdated 1 `shouldBe` Right ()
            case ensureLeadCompletionUpdated 0 of
                Left err -> do
                    errHTTPCode err `shouldBe` 409
                    BL.unpack (errBody err) `shouldContain` "Lead completion could not be applied"
                Right _ -> expectationFailure "Expected zero-row lead completion update to be rejected"

    describe "parseSocialErrorsChannel" $ do
        it "normalizes valid channel values" $ do
            parseSocialErrorsChannel (Just " WhatsApp ") `shouldBe` Right "whatsapp"
            parseSocialErrorsChannel (Just "FACEBOOK") `shouldBe` Right "facebook"

        it "rejects missing or blank channels instead of falling back implicitly" $ do
            case parseSocialErrorsChannel Nothing of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "channel requerido"
                Right _ -> expectationFailure "Expected missing channel to be rejected"
            case parseSocialErrorsChannel (Just "   ") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "channel requerido"
                Right _ -> expectationFailure "Expected blank channel to be rejected"

        it "rejects unknown channel values" $
            case parseSocialErrorsChannel (Just "telegram") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "channel inválido"
                Right _ -> expectationFailure "Expected invalid channel to be rejected"

    describe "live session intake multipart parsing" $ do
        it "normalizes blank optional text fields to Nothing while preserving versioned consent" $ do
            let parsed = fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "  The House Band  ")
                    , ("contactEmail", "   ")
                    , ("acceptedTerms", " yes ")
                    , ("termsVersion", "  TDF Live Sessions v2  ")
                    , ("musicians", "[]")
                    ])
            case parsed :: Either String LiveSessionIntakePayload of
                Left err -> expectationFailure err
                Right payload -> do
                    lsiBandName payload `shouldBe` "The House Band"
                    lsiContactEmail payload `shouldBe` Nothing
                    lsiAcceptedTerms payload `shouldBe` True
                    lsiTermsVersion payload `shouldBe` Just "TDF Live Sessions v2"

        it "rejects accepted terms without a terms version" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("acceptedTerms", "true")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "termsVersion is required when acceptedTerms is true"
                Right payload ->
                    expectationFailure ("Expected missing termsVersion to be rejected, got: " <> show payload)

        it "rejects malformed acceptedTerms values instead of silently coercing them to false" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("acceptedTerms", "maybe")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "acceptedTerms must be a boolean"
                Right payload ->
                    expectationFailure ("Expected invalid acceptedTerms to be rejected, got: " <> show payload)

    APITypesSpec.spec
    ArtistSpec.spec
    ServerSpec.spec
    ServerAdminSpec.spec
    ServerExtraSpec.spec
    FollowSpec.spec
    FollowHandlerSpec.spec
    PublicLeadSpec.spec

mkLiveSessionMultipart :: [(Text, Text)] -> MultipartData Tmp
mkLiveSessionMultipart fields =
    MultipartData
        { inputs = map (uncurry Input) fields
        , files = []
        }
