{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TDF.Server.EventResearchSpec (spec) where

import Data.Aeson (Value, eitherDecode, object, (.=))
import Data.Either (isLeft, isRight)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Control.Monad.Trans.Reader (runReaderT)
import Database.Persist.Sql (toSqlKey)
import Servant ((:<|>) (..), errHTTPCode)
import Servant.Server.Internal.Handler (runHandler)
import Test.Hspec

import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.DTO.EventResearchDTO
    ( EventResearchCandidateDTO (..)
    , EventResearchCandidateWriteDTO (..)
    , EventResearchEvidenceDTO (..)
    , EventResearchMaterializationRequestDTO (..)
    )
import TDF.DTO.SocialEventsDTO (DiscoverySourceWriteDTO (..))
import TDF.Models.SocialEventsModels
    ( EventResearchCandidateId
    , SocialEventId
    )
import TDF.Models (RoleEnum (Customer))
import TDF.Server.EventResearch
import TDF.Server.SocialEventsHandlers (validateDiscoverySourceWrite)

spec :: Spec
spec = do
    describe "event research candidate validation" $ do
        it "accepts a complete official-sale candidate with an explicit timezone" $ do
            validateEventResearchCandidate completeCandidate `shouldSatisfy` isRight

        it "keeps high confidence stricter than draft review" $ do
            validateEventResearchCandidate completeCandidate{erCandidatePurchaseUrl = Nothing}
                `shouldSatisfy` isLeft
            validateEventResearchCandidate
                completeCandidate
                    { erCandidatePurchaseUrl = Nothing
                    , erCandidateConfidence = "medium"
                    }
                `shouldSatisfy` isRight

        it "rejects a missing IANA timezone and evidence not containing the primary source" $ do
            validateEventResearchCandidate completeCandidate{erCandidateTimezone = "UTC"}
                `shouldSatisfy` isLeft
            validateEventResearchCandidate
                completeCandidate
                    { erCandidateEvidence =
                        [ (head completeCandidate.erCandidateEvidence)
                            { erEvidenceUrl = "https://other.example/event"
                            }
                        ]
                    }
                `shouldSatisfy` isLeft

        it "keeps material hashes stable across a later verification timestamp" $ do
            eventResearchCandidateContentHash completeCandidate
                `shouldBe` eventResearchCandidateContentHash
                    completeCandidate
                        { erCandidateVerifiedAt = fixtureTime 13
                        , erCandidateEvidence =
                            [ (head completeCandidate.erCandidateEvidence)
                                { erEvidenceConsultedAt = fixtureTime 13
                                }
                            ]
                        }

    describe "event research candidate materialization" $ do
        it "accepts an approved high-confidence candidate without a confirmed end" $ do
            validateEventResearchMaterialization True publishRequest materializationCandidate
                `shouldSatisfy` isRight

        it "requires pilot approval, pilot membership, high confidence, draft review, and a lineup" $ do
            validateEventResearchMaterialization False publishRequest materializationCandidate
                `shouldSatisfy` isLeft
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate{erCandidateIsPilot = False}
                `shouldSatisfy` isLeft
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate{erCandidateConfidence = "medium"}
                `shouldSatisfy` isLeft
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate{erCandidateReviewState = "review"}
                `shouldSatisfy` isLeft
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate
                    { erCandidatePayload =
                        object
                            [ "eventType" .= ("concert" :: String)
                            , "publicationBlockers" .= ["event_end_unconfirmed" :: String]
                            ]
                    }
                `shouldSatisfy` isLeft

        it "allows only the explicit unconfirmed-end blocker" $ do
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate
                    { erCandidatePayload = materializationPayload ["venue_unconfirmed"] }
                `shouldSatisfy` isLeft
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate
                    { erCandidatePayload =
                        materializationPayload ["event_end_unconfirmed"]
                    }
                `shouldSatisfy` isRight

        it "rejects cancelled and postponed candidates" $ do
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate
                    { erCandidatePayload =
                        object
                            [ "eventType" .= ("concert" :: String)
                            , "lineup" .= ["Artista oficial" :: String]
                            , "availability" .= ("cancelled" :: String)
                            , "publicationBlockers" .= ["event_end_unconfirmed" :: String]
                            ]
                    }
                `shouldSatisfy` isLeft

        it "accepts only explicit sale availability states" $ do
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate
                    { erCandidatePayload = materializationPayloadWithAvailability "partially_sold_out" }
                `shouldSatisfy` isRight
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate
                    { erCandidatePayload = materializationPayloadWithAvailability "sold_out" }
                `shouldSatisfy` isLeft
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate
                    { erCandidatePayload = materializationPayloadWithAvailability "off_sale" }
                `shouldSatisfy` isLeft
            validateEventResearchMaterialization
                True
                publishRequest
                materializationCandidate
                    { erCandidatePayload = materializationPayloadWithAvailability "unavailable" }
                `shouldSatisfy` isLeft

        it "marks unpublished provider references as drafts" $ do
            materializationEventRefSourceStatus False "on_sale" `shouldBe` "draft:on_sale"
            materializationEventRefSourceStatus True "on_sale" `shouldBe` "on_sale"

        it "uses only valid materialization workflow states" $ do
            materializationWorkflowStateCode True "on_sale" `shouldBe` "on_sale"
            materializationWorkflowStateCode False "on_sale" `shouldBe` "planning"

        it "uses a stable candidate/event audit dedupe key" $ do
            let candidateId = toSqlKey 7 :: EventResearchCandidateId
                eventId = toSqlKey 11 :: SocialEventId
                otherEventId = toSqlKey 12 :: SocialEventId
            eventResearchMaterializationDedupeKey candidateId eventId
                `shouldBe` eventResearchMaterializationDedupeKey candidateId eventId
            eventResearchMaterializationDedupeKey candidateId eventId
                `shouldNotBe` eventResearchMaterializationDedupeKey candidateId otherEventId

        it "rejects unknown request fields" $ do
            (eitherDecode "{\"erMaterializationRunId\":\"3\",\"erMaterializationPublish\":true,\"unexpected\":1}" :: Either String EventResearchMaterializationRequestDTO)
                `shouldSatisfy` isLeft

        it "requires strict administrator access before reading the environment" $ do
            let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> materialize :<|> _ =
                    eventResearchServer nonAdminUser
            result <-
                runHandler $
                    runReaderT
                        (materialize "7" publishRequest)
                        (error "authorization rejection must not read Env")
            case result of
                Left serverError -> errHTTPCode serverError `shouldBe` 403
                Right _ -> expectationFailure "Expected strict administrator rejection"

    describe "manual web discovery source validation" $ do
        it "accepts only disabled HTTPS web sources without a city" $ do
            validateDiscoverySourceWrite webSource `shouldSatisfy` isRight
            validateDiscoverySourceWrite webSource{discoverySourceWriteEnabled = True}
                `shouldSatisfy` isLeft
            validateDiscoverySourceWrite webSource{discoverySourceWriteCityId = Just "1"}
                `shouldSatisfy` isLeft

completeCandidate :: EventResearchCandidateWriteDTO
completeCandidate =
    EventResearchCandidateWriteDTO
        { erCandidateProvider = "fixture"
        , erCandidateExternalId = "official-event-1"
        , erCandidateRunId = "1"
        , erCandidateSourceId = Nothing
        , erCandidateReviewState = "draft"
        , erCandidateTitle = "Festival confirmado"
        , erCandidateStartTime = Just (fixtureTime 10)
        , erCandidateEndTime = Just (fixtureTime 12)
        , erCandidateTimezone = "America/Guayaquil"
        , erCandidateVenueName = Just "Venue oficial"
        , erCandidateCity = Just "Quito"
        , erCandidateProvince = Just "Pichincha"
        , erCandidateCountryCode = "EC"
        , erCandidateSourceUrl = "https://official.example/event"
        , erCandidateInfoUrl = Just "https://official.example/event"
        , erCandidatePurchaseUrl = Just "https://official.example/event/buy"
        , erCandidatePayload = object []
        , erCandidateEvidence =
            [ EventResearchEvidenceDTO
                { erEvidenceUrl = "https://official.example/event"
                , erEvidenceKind = "official_sale"
                , erEvidenceConsultedAt = fixtureTime 9
                , erEvidenceNotes = Nothing
                }
            ]
        , erCandidateConfidence = "high"
        , erCandidateManagedFields = ["title", "startTime", "timezone"]
        , erCandidateVerifiedAt = fixtureTime 9
        }

publishRequest :: EventResearchMaterializationRequestDTO
publishRequest =
    EventResearchMaterializationRequestDTO
        { erMaterializationRunId = "3"
        , erMaterializationPublish = True
        }

nonAdminUser :: AuthedUser
nonAdminUser =
    AuthedUser
        { auPartyId = toSqlKey 99
        , auRoles = [Customer]
        , auModules = modulesForRoles [Customer]
        }

materializationCandidate :: EventResearchCandidateDTO
materializationCandidate =
    EventResearchCandidateDTO
        { erCandidateId = "7"
        , erCandidateProvider = completeCandidate.erCandidateProvider
        , erCandidateExternalId = completeCandidate.erCandidateExternalId
        , erCandidateRunId = completeCandidate.erCandidateRunId
        , erCandidateSourceId = completeCandidate.erCandidateSourceId
        , erCandidateEventId = Nothing
        , erCandidateReviewState = completeCandidate.erCandidateReviewState
        , erCandidateTitle = completeCandidate.erCandidateTitle
        , erCandidateStartTime = completeCandidate.erCandidateStartTime
        , erCandidateEndTime = Nothing
        , erCandidateTimezone = completeCandidate.erCandidateTimezone
        , erCandidateVenueName = completeCandidate.erCandidateVenueName
        , erCandidateCity = completeCandidate.erCandidateCity
        , erCandidateProvince = completeCandidate.erCandidateProvince
        , erCandidateCountryCode = completeCandidate.erCandidateCountryCode
        , erCandidateSourceUrl = completeCandidate.erCandidateSourceUrl
        , erCandidateInfoUrl = completeCandidate.erCandidateInfoUrl
        , erCandidatePurchaseUrl = completeCandidate.erCandidatePurchaseUrl
        , erCandidatePayload = materializationPayload ["event_end_unconfirmed"]
        , erCandidateEvidence = completeCandidate.erCandidateEvidence
        , erCandidateConfidence = completeCandidate.erCandidateConfidence
        , erCandidateManagedFields = completeCandidate.erCandidateManagedFields
        , erCandidateContentHash = "fixture-content-hash"
        , erCandidateVerifiedAt = completeCandidate.erCandidateVerifiedAt
        , erCandidateIsPilot = True
        , erCandidateCreatedAt = fixtureTime 8
        , erCandidateUpdatedAt = fixtureTime 9
        }

materializationPayload :: [String] -> Value
materializationPayload blockers =
    object
        [ "eventType" .= ("concert" :: String)
        , "lineup" .= ["Artista oficial" :: String]
        , "publicationBlockers" .= blockers
        ]

materializationPayloadWithAvailability :: String -> Value
materializationPayloadWithAvailability availability =
    object
        [ "eventType" .= ("concert" :: String)
        , "lineup" .= ["Artista oficial" :: String]
        , "availability" .= availability
        , "publicationBlockers" .= ["event_end_unconfirmed" :: String]
        ]

webSource :: DiscoverySourceWriteDTO
webSource =
    DiscoverySourceWriteDTO
        { discoverySourceWriteKey = "official-web"
        , discoverySourceWriteName = "Official web"
        , discoverySourceWriteType = "web"
        , discoverySourceWriteFeedUrl = Just "https://official.example/"
        , discoverySourceWriteCityId = Nothing
        , discoverySourceWriteEnabled = False
        , discoverySourceWritePriority = 200
        }

fixtureTime :: Integer -> UTCTime
fixtureTime hour = UTCTime (fromGregorian 2026 8 16) (secondsToDiffTime (hour * 3600))
