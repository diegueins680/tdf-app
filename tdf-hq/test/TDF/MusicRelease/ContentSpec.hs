{-# LANGUAGE OverloadedStrings #-}

module TDF.MusicRelease.ContentSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.Time (UTCTime)
import qualified Data.Text as Text
import Test.Hspec

import TDF.API.MusicRelease
import TDF.MusicRelease.ContentValidation (validateMusicReleaseContent)

spec :: Spec
spec = describe "music release catalog payload" $ do
  it "decodes the documented partyId and partyKind request fields" $
    case eitherDecode "{\"clientRef\":\"external\",\"partyId\":null,\"tdfPartyId\":null,\"displayName\":\"External Artist\",\"legalName\":null,\"partyKind\":\"person\",\"identifiers\":[]}" of
      Left failure -> expectationFailure failure
      Right party -> do
        musicPartyClientRef party `shouldBe` "external"
        musicPartyId party `shouldBe` Nothing
        musicPartyKind party `shouldBe` "person"

  it "decodes resumable part evidence and completion etag fields" $ do
    case eitherDecode "{\"byteSize\":1024,\"etag\":\"synthetic-etag\",\"sha256\":\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"}" of
      Left failure -> expectationFailure failure
      Right part -> do
        musicUploadPartByteSize part `shouldBe` 1024
        musicUploadPartEtag part `shouldBe` "synthetic-etag"
    case eitherDecode "{\"etag\":\"synthetic-final-etag\"}" of
      Left failure -> expectationFailure failure
      Right completion -> musicUploadConfirmEtag completion `shouldBe` "synthetic-final-etag"

  it "decodes the player event schema used by anonymous and authenticated clients" $
    case eitherDecode "{\"eventId\":\"10000000-0000-4000-8000-000000000001\",\"sessionId\":\"20000000-0000-4000-8000-000000000001\",\"sequenceNumber\":1,\"anonymousId\":\"synthetic-listener\",\"releaseVersionId\":\"30000000-0000-4000-8000-000000000001\",\"recordingId\":\"40000000-0000-4000-8000-000000000001\",\"eventType\":\"progress\",\"positionMs\":30000,\"listenedDeltaMs\":30000,\"quality\":\"high\",\"territoryCode\":\"EC\",\"occurredAt\":\"2026-09-13T09:00:00Z\",\"metadata\":{}}" of
      Left failure -> expectationFailure failure
      Right event -> do
        musicEventSequenceNumber event `shouldBe` 1
        musicEventType event `shouldBe` "progress"
        musicEventListenedDeltaMs event `shouldBe` 30000

  it "accepts a complete audio release with a collaborator who has no TDF account" $
    validateMusicReleaseContent validContent `shouldBe` Right ()

  it "accepts an unfinished external collaborator without assigning credits or splits" $ do
    let pending = MusicPartyDraft
          { musicPartyClientRef="pending",musicPartyId=Nothing,musicPartyTdfPartyId=Nothing
          , musicPartyDisplayName="Uncredited collaborator",musicPartyLegalName=Nothing
          , musicPartyKind="person",musicPartyIdentifiers=[]
          }
    validateMusicReleaseContent validContent
      { musicContentParties = musicContentParties validContent ++ [pending] }
      `shouldBe` Right ()

  it "rejects an invalid provided ISRC without issuing a replacement" $ do
    let invalid = validContent
          { musicContentIdentifiers = [MusicIdentifierDraft (Just "track-1") "isrc" "NOT-AN-ISRC"] }
    validateMusicReleaseContent invalid `shouldSatisfy` isLeftContaining "ISRC"

  it "accepts syntactically valid ISNI, IPI, and DPID as provided party identifiers" $ do
    case musicContentParties validContent of
      party : remaining -> do
        let identified = party
              { musicPartyIdentifiers =
                  [ MusicPartyIdentifierDraft "isni" "000000012146438X"
                  , MusicPartyIdentifierDraft "ipi" "00000000000"
                  , MusicPartyIdentifierDraft "dpid" "PADPIDA2026TDFTEST"
                  ]
              }
        validateMusicReleaseContent validContent{ musicContentParties=identified:remaining }
          `shouldBe` Right ()
      [] -> expectationFailure "valid fixture has no parties"

  it "validates master and composition splits within each declaration" $ do
    case musicContentRightsDeclarations validContent of
      firstRights : remaining -> case musicRightsSplits firstRights of
        firstSplit : otherSplits -> do
          let invalid = validContent
                { musicContentRightsDeclarations = firstRights
                    { musicRightsSplits = firstSplit { musicSplitBasisPoints = 9999 } : otherSplits
                    } : remaining
                }
          validateMusicReleaseContent invalid `shouldSatisfy` isLeftContaining "10000"
        [] -> expectationFailure "valid fixture has no splits"
      [] -> expectationFailure "valid fixture has no rights declarations"

  it "requires an exact downloadable asset and integer price for a purchase" $ do
    case musicContentAvailability validContent of
      rule : _ -> do
        let invalidRule = rule
              { musicAvailabilityDownloadPolicy="purchase"
              , musicAvailabilityPurchasable=True
              , musicAvailabilityCurrency=Just "USD"
              , musicAvailabilityPriceMinor=Just 250
              }
        validateMusicReleaseContent validContent{ musicContentAvailability=[invalidRule] }
          `shouldSatisfy` isLeftContaining "downloadable asset"
      [] -> expectationFailure "valid fixture has no availability rule"

  it "rejects dangling request-local references before opening a transaction" $ do
    case musicContentCredits validContent of
      credit : _ -> do
        let invalidCredit = credit{ musicCreditPartyRef="missing-party" }
        validateMusicReleaseContent validContent{ musicContentCredits=[invalidCredit] }
          `shouldSatisfy` isLeftContaining "Unknown credit partyRef"
      [] -> expectationFailure "valid fixture has no credits"

validContent :: MusicReleaseContentRequest
validContent = MusicReleaseContentRequest
  { musicContentExpectedUpdatedAt = read "2026-09-12 12:00:00 UTC" :: UTCTime
  , musicContentTracks =
      [ MusicTrackDraft
          { musicTrackClientRef="track-1",musicTrackRecordingId=Nothing
          , musicTrackTitle="Synthetic Single",musicTrackSubtitle=Nothing,musicTrackVersionTitle=Nothing
          , musicTrackTitleLanguage="es",musicTrackTitleScript=Nothing
          , musicTrackExplicitContent="not_explicit",musicTrackDiscNumber=1,musicTrackTrackNumber=1
          , musicTrackDisplayArtist="Synthetic Artist",musicTrackIsPrimaryResource=True
          , musicTrackPreviewStartMs=Just 30000,musicTrackPreviewDurationMs=Just 30000
          }
      ]
  , musicContentParties =
      [ MusicPartyDraft
          { musicPartyClientRef="artist",musicPartyId=Nothing,musicPartyTdfPartyId=Nothing
          , musicPartyDisplayName="Synthetic Artist",musicPartyLegalName=Nothing,musicPartyKind="person"
          , musicPartyIdentifiers=[]
          }
      ]
  , musicContentCredits =
      [ MusicCreditDraft "artist" (Just "track-1") "main_artist" 0 Nothing
      , MusicCreditDraft "artist" (Just "track-1") "composer" 1 Nothing
      ]
  , musicContentIdentifiers = [MusicIdentifierDraft (Just "track-1") "isrc" "USRC17607839"]
  , musicContentRightsDeclarations =
      [ rights "master" "owned master"
      , rights "composition" "controlled composition"
      ]
  , musicContentAvailability =
      [ MusicAvailabilityDraft
          { musicAvailabilityTrackRef=Nothing,musicAvailabilityTerritoryMode="include"
          , musicAvailabilityTerritories=["Worldwide"],musicAvailabilityStartsAt=Nothing
          , musicAvailabilityEndsAt=Nothing,musicAvailabilityListeningPolicy="full"
          , musicAvailabilityDownloadPolicy="none",musicAvailabilityPurchasable=False
          , musicAvailabilityPriceMinor=Nothing,musicAvailabilityCurrency=Nothing
          , musicAvailabilityDownloadableAssetId=Nothing
          }
      ]
  }
  where
    rights scope basis = MusicRightsDraft
      { musicRightsTrackRef=Just "track-1",musicRightsScope=scope,musicRightsAuthorityBasis=basis
      , musicRightsTerritories=["Worldwide"],musicRightsStartsOn=read "2026-01-01"
      , musicRightsEndsOn=Nothing
      , musicRightsSplits=
          [ MusicSplitDraft "artist" 10000 ["Worldwide"] (read "2026-01-01") Nothing ]
      }

isLeftContaining :: String -> Either Text.Text () -> Bool
isLeftContaining needle (Left message) = Text.pack needle `Text.isInfixOf` message
isLeftContaining _ (Right ()) = False
