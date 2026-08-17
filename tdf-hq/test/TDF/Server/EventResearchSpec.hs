{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TDF.Server.EventResearchSpec (spec) where

import Data.Aeson (object)
import Data.Either (isLeft, isRight)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Test.Hspec

import TDF.DTO.EventResearchDTO
    ( EventResearchCandidateWriteDTO (..)
    , EventResearchEvidenceDTO (..)
    )
import TDF.DTO.SocialEventsDTO (DiscoverySourceWriteDTO (..))
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
