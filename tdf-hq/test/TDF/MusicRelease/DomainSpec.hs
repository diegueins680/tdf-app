{-# LANGUAGE OverloadedStrings #-}

module TDF.MusicRelease.DomainSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Time (UTCTime, addUTCTime)
import Test.Hspec
import TDF.MusicRelease.Domain

spec :: Spec
spec = do
  describe "correction error classification" $ do
    it "maps a disconnected/cyclic graph to an actionable asset error" $
      fmap correctionFailureCode (classifyCorrectionFailure "23514"
        "correction asset graph has a cycle or a parent outside the source resources")
        `shouldBe` Just "correction_resource_graph_invalid"
    it "does not echo recording identifiers or SQL details" $ do
      let failure = classifyCorrectionFailure "23514"
            "correction asset private-identifier references a recording outside the source version"
      fmap correctionFailureField failure `shouldBe` Just "assets.recordingId"
      fmap correctionFailureStatus failure `shouldBe` Just 422
      show failure `shouldNotContain` "private-identifier"
    it "reports a concurrently unavailable source as conflict" $
      fmap correctionFailureStatus (classifyCorrectionFailure "23514"
        "source music release version is not an immutable correction source")
        `shouldBe` Just 409
    it "gives bounded concurrency failures safe retry guidance" $
      map (fmap correctionFailureCode . flip classifyCorrectionFailure "private SQL")
        ["40001","40P01"] `shouldBe`
          replicate 2 (Just "correction_retry_required")
    it "does not disguise unrelated database failures as validation errors" $ do
      classifyCorrectionFailure "23514" "unrelated integrity constraint" `shouldBe` Nothing
      classifyCorrectionFailure "23505" "duplicate key" `shouldBe` Nothing
      classifyCorrectionFailure "08006"
        "correction asset graph has a cycle or a parent outside the source resources"
        `shouldBe` Nothing
  describe "release editorial lifecycle" $ do
    it "blocks submission until every editorial gate is satisfied" $ do
      let transitionContext = validContext { transitionGates = validGates { hasCompositionRights = False } }
      transitionRelease transitionContext Draft ReadyForReview
        `shouldBe` Left "Declara por separado los derechos de composición."

    it "requires an authorized TDF reviewer for approval" $ do
      transitionRelease (validContext { reviewerAuthorized = False }) InReview Approved
        `shouldBe` Left "La aprobación final requiere personal autorizado de TDF."

    it "rejects publication before the scheduled instant" $ do
      let transitionContext = validContext
            { scheduledReleaseAt = Just (addUTCTime 60 referenceTime) }
      transitionRelease transitionContext Scheduled Published
        `shouldBe` Left "El embargo sigue vigente; todavía no se puede publicar."

    it "accepts the audited happy path" $ do
      transitionRelease validContext Draft ReadyForReview `shouldBe` Right ReadyForReview
      transitionRelease validContext ReadyForReview InReview `shouldBe` Right InReview
      transitionRelease validContext InReview Approved `shouldBe` Right Approved
      transitionRelease
        (validContext { scheduledReleaseAt = Just (addUTCTime 60 referenceTime) })
        Approved Scheduled `shouldBe` Right Scheduled
      transitionRelease validContext Scheduled Published `shouldBe` Right Published

  describe "rights splits" $ do
    it "validates master and composition independently at 10000 basis points" $ do
      splitTotalValid
        [ (MasterRights, 6000), (MasterRights, 4000)
        , (CompositionRights, 5000), (CompositionRights, 5000)
        ] `shouldBe` True
      splitTotalValid
        [ (MasterRights, 10000), (CompositionRights, 9999) ] `shouldBe` False

  describe "provided identifiers" $ do
    it "normalizes a formatted ISRC but never issues one" $ do
      validateIdentifier ISRC "US-RC1-76-07839" `shouldBe` Right "USRC17607839"

    it "checks GTIN check digits" $ do
      validateIdentifier UPC "012345678905" `shouldBe` Right "012345678905"
      validateIdentifier UPC "012345678901" `shouldSatisfy` isLeft

    it "checks ISNI and DPID syntax" $ do
      validateIdentifier ISNI "000000012146438X" `shouldSatisfy` isRight
      validateIdentifier DPID "PADPIDA2026TDF" `shouldBe` Right "PADPIDA2026TDF"
      validateIdentifier DPID "DPID:TDF" `shouldSatisfy` isLeft

    it "reports all DDEX identifier prerequisites" $ do
      validateDdexExportPrerequisites Nothing Nothing [] `shouldSatisfy` isLeft
      validateDdexExportPrerequisites
        (Just "PADPIDA2026TDF")
        (Just "PADPIDA2026DSP")
        [(ISRC, "USRC17607839"), (UPC, "012345678905")]
        `shouldBe` Right ()

  describe "money and playback access" $ do
    it "uses supported ISO currency and integer minor units" $ do
      validateMoney ["USD"] True "usd" 125 `shouldBe` Right ("USD", 125)
      validateMoney ["USD"] True "USD" 0 `shouldSatisfy` isLeft
      validateMoney ["USD"] True "EUR" 125 `shouldSatisfy` isLeft

    it "applies time and territory rules for visitors as well as users" $ do
      let rule = AccessRule IncludeTerritories ["EC"] Nothing Nothing FullListening
      decidePlaybackAccess referenceTime "EC" rule `shouldBe` AccessFull
      decidePlaybackAccess referenceTime "US" rule
        `shouldBe` AccessDenied "El contenido no está disponible en tu territorio."

  describe "eligible plays and basic anti-fraud" $ do
    it "counts 30 seconds of actual continuous listening" $ do
      eligiblePlay validPlayback `shouldBe` True

    it "does not count seeks or automation as listening" $ do
      eligiblePlay (validPlayback { automationDetected = True }) `shouldBe` False
      basicFraudFlags (validPlayback { cumulativeListenedMs = 60000, wallClockElapsedMs = 1000 })
        `shouldContain` ["impossible_listen_time"]

    it "uses 80 percent for a recording shorter than 30 seconds" $ do
      eligiblePlay validPlayback
        { trackDurationMs = 10000
        , cumulativeListenedMs = 8000
        , maxContinuousListenedMs = 8000
        , wallClockElapsedMs = 8000
        } `shouldBe` True

  describe "DDEX compatibility matrix" $ do
    it "pins ERN 4.3.2 without the discontinued ERN 3 business profile" $ do
      ernVersion supportedDdexCompatibility `shouldBe` "4.3.2"
      releaseProfile supportedDdexCompatibility `shouldBe` "Audio"
      releaseProfileVersion supportedDdexCompatibility `shouldBe` "2.3.1"
      businessProfileVersion supportedDdexCompatibility `shouldBe` Nothing
      allowedValueSetsVersion supportedDdexCompatibility `shouldBe` "011"

referenceTime :: UTCTime
referenceTime = read "2026-09-12 12:00:00 UTC"

validGates :: ReleaseGates
validGates = ReleaseGates
  { metadataValid = True
  , assetsValid = True
  , rightsValid = True
  , accessValid = True
  , hasTracks = True
  , hasMainArtist = True
  , hasMasterRights = True
  , hasCompositionRights = True
  , authorityTermsAccepted = True
  , openChangeRequests = 0
  }

validContext :: TransitionContext
validContext = TransitionContext
  { transitionNow = referenceTime
  , transitionGates = validGates
  , reviewerAuthorized = True
  , scheduledReleaseAt = Just (addUTCTime (-60) referenceTime)
  , originalTimeZone = Just "America/Guayaquil"
  , immutableSnapshotSha256 = Just "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  }

validPlayback :: PlaybackEvidence
validPlayback = PlaybackEvidence
  { trackDurationMs = 180000
  , cumulativeListenedMs = 30000
  , maxContinuousListenedMs = 30000
  , wallClockElapsedMs = 30000
  , playbackRate = 1
  , seekCount = 0
  , startsInWindow = 1
  , automationDetected = False
  }
