{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (eitherDecode)
import Data.Either (isLeft)
import Data.Time (UTCTime (..), addDays, addUTCTime, fromGregorian, secondsToDiffTime)
import Database.Persist.Sql (toSqlKey)
import Servant (ServerError (..))
import Test.Hspec

import TDF.API.WhatsApp (validateHookVerifyRequest)
import qualified TDF.APITypesSpec as APITypesSpec
import TDF.Cron (Directive (..), parseDirective)
import TDF.DTO.SocialEventsDTO
    ( EventUpdateDTO (..),
      InvitationUpdateDTO (..),
      NullableFieldUpdate (..),
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
import TDF.Server.SocialEventsHandlers (
    normalizeBudgetLineType,
    normalizeEventStatus,
    normalizeEventType,
    normalizeFinanceDirection,
    normalizeFinanceEntryStatus,
    normalizeFinanceSource,
    normalizeArtistGenres,
    normalizeInvitationStatus,
    normalizePositivePartyIdText,
    parseFollowerQueryParamEither,
    normalizeTicketOrderStatus,
    normalizeTicketStatus,
    parseNearQueryEither,
    parseInvitationIdsEither,
 )
import qualified TDF.ServerSpec as ServerSpec
import qualified TDF.ServerExtraSpec as ServerExtraSpec
import qualified TDF.Social.FollowHandlerSpec as FollowHandlerSpec
import qualified TDF.Social.FollowSpec as FollowSpec
import qualified TDF.Trials.PublicLeadSpec as PublicLeadSpec

main :: IO ()
main = hspec $ do
    describe "normalizeInvitationStatus" $ do
        it "falls back to pending when missing" $ do
            normalizeInvitationStatus Nothing `shouldBe` "pending"

        it "trims and lowercases non-empty statuses" $ do
            normalizeInvitationStatus (Just "  Accepted ") `shouldBe` "accepted"

        it "treats blank strings as pending" $ do
            normalizeInvitationStatus (Just "   ") `shouldBe` "pending"

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

    describe "normalizePositivePartyIdText" $ do
        it "accepts positive numeric ids and canonicalizes them" $ do
            normalizePositivePartyIdText " 0042 " `shouldBe` Just "42"

        it "rejects blank and non-numeric ids" $ do
            normalizePositivePartyIdText "   " `shouldBe` Nothing
            normalizePositivePartyIdText "abc" `shouldBe` Nothing

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

    describe "validateRadioStreamUrl" $ do
        it "trims surrounding whitespace and accepts http(s) stream URLs" $
            validateRadioStreamUrl "  HTTPS://radio.example.com/live  "
                `shouldBe` Right "HTTPS://radio.example.com/live"

        it "rejects blank stream URLs with a precise 400" $
            case validateRadioStreamUrl "   " of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl is required"
                Right _ -> expectationFailure "Expected blank streamUrl to be rejected"

        it "rejects non-http stream URLs before they can be stored" $
            case validateRadioStreamUrl "ftp://radio.example.com/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must be http(s)"
                Right _ -> expectationFailure "Expected non-http streamUrl to be rejected"

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

    APITypesSpec.spec
    ArtistSpec.spec
    ServerSpec.spec
    ServerAdminSpec.spec
    ServerExtraSpec.spec
    FollowSpec.spec
    FollowHandlerSpec.spec
    PublicLeadSpec.spec
