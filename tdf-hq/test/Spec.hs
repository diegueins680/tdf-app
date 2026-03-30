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
    normalizeTicketOrderStatus,
    normalizeTicketStatus,
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

        it "rejects verification requests when hub.mode is missing" $ do
            case validateHookVerifyRequest Nothing (Just "challenge-123") (Just "secret") (Just "secret") of
                Left err -> errHTTPCode err `shouldBe` 403
                Right _ -> expectationFailure "Expected missing hub.mode to be rejected"

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
    ServerExtraSpec.spec
    FollowSpec.spec
    FollowHandlerSpec.spec
    PublicLeadSpec.spec
