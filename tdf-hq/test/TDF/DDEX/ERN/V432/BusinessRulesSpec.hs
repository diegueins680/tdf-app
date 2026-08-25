{-# LANGUAGE OverloadedStrings #-}

module TDF.DDEX.ERN.V432.BusinessRulesSpec (spec) where

import Test.Hspec
import TDF.DDEX.ERN.V432.Normalize
import TDF.DDEX.ERN.V432.BusinessRules
import qualified TDF.Catalog.Types as Catalog

spec :: Spec
spec = do
  describe "validateBusinessRules" $ do
    context "BR-001: Sound recordings must have ISRC" $ do
      it "reports error for sound recording without ISRC" $ do
        let ci = emptyCanonicalImport
              { ciResources = [CanonicalResource
                  { cresTitle = "Test Track"
                  , cresSubTitle = Nothing
                  , cresResourceType = Catalog.SoundRecording
                  , cresDurationMs = Just 210000
                  , cresLanguage = Nothing
                  , cresExplicitContent = False
                  , cresIsrc = Nothing
                  , cresGRid = Nothing
                  , cresSourcePartyRef = "A1"
                  }]
              }
            violations = validateBusinessRules ci
            br001 = filter (\v -> brvRule v == "BR-001") violations
        length br001 `shouldBe` 1
        brvSeverity (head br001) `shouldBe` RuleError

      it "passes for sound recording with ISRC" $ do
        let ci = emptyCanonicalImport
              { ciResources = [CanonicalResource
                  { cresTitle = "Test Track"
                  , cresSubTitle = Nothing
                  , cresResourceType = Catalog.SoundRecording
                  , cresDurationMs = Just 210000
                  , cresLanguage = Nothing
                  , cresExplicitContent = False
                  , cresIsrc = Just "USRC17607839"
                  , cresGRid = Nothing
                  , cresSourcePartyRef = "A1"
                  }]
              }
            violations = validateBusinessRules ci
            br001 = filter (\v -> brvRule v == "BR-001") violations
        length br001 `shouldBe` 0

      it "ignores non-sound-recording resources without ISRC" $ do
        let ci = emptyCanonicalImport
              { ciResources = [CanonicalResource
                  { cresTitle = "Cover Image"
                  , cresSubTitle = Nothing
                  , cresResourceType = Catalog.Image
                  , cresDurationMs = Nothing
                  , cresLanguage = Nothing
                  , cresExplicitContent = False
                  , cresIsrc = Nothing
                  , cresGRid = Nothing
                  , cresSourcePartyRef = "A1"
                  }]
              }
            violations = validateBusinessRules ci
            br001 = filter (\v -> brvRule v == "BR-001") violations
        length br001 `shouldBe` 0

    context "BR-002: ISRC format validation" $ do
      it "warns on invalid ISRC format" $ do
        let ci = emptyCanonicalImport
              { ciResources = [CanonicalResource
                  { cresTitle = "Test Track"
                  , cresSubTitle = Nothing
                  , cresResourceType = Catalog.SoundRecording
                  , cresDurationMs = Just 210000
                  , cresLanguage = Nothing
                  , cresExplicitContent = False
                  , cresIsrc = Just "INVALID"
                  , cresGRid = Nothing
                  , cresSourcePartyRef = "A1"
                  }]
              }
            violations = validateBusinessRules ci
            br002 = filter (\v -> brvRule v == "BR-002") violations
        length br002 `shouldBe` 1
        brvSeverity (head br002) `shouldBe` RuleWarning

      it "passes for valid ISRC format" $ do
        let ci = emptyCanonicalImport
              { ciResources = [CanonicalResource
                  { cresTitle = "Test Track"
                  , cresSubTitle = Nothing
                  , cresResourceType = Catalog.SoundRecording
                  , cresDurationMs = Just 210000
                  , cresLanguage = Nothing
                  , cresExplicitContent = False
                  , cresIsrc = Just "USRC17607839"
                  , cresGRid = Nothing
                  , cresSourcePartyRef = "A1"
                  }]
              }
            violations = validateBusinessRules ci
            br002 = filter (\v -> brvRule v == "BR-002") violations
        length br002 `shouldBe` 0

    context "BR-003: UPC format validation" $ do
      it "warns on invalid UPC format" $ do
        let ci = emptyCanonicalImport
              { ciReleases = [CanonicalRelease
                  { crTitle = "Test Album"
                  , crSubTitle = Nothing
                  , crReleaseType = Catalog.Album
                  , crReleaseDate = Nothing
                  , crOriginalDate = Nothing
                  , crLabel = Nothing
                  , crCopyrightLine = Nothing
                  , crPhonographicCopyrightLine = Nothing
                  , crGenre = Nothing
                  , crUpc = Just "INVALID"
                  , crCatalogNumber = Nothing
                  , crResourceRefs = []
                  , crSourcePartyRef = "R1"
                  }]
              }
            violations = validateBusinessRules ci
            br003 = filter (\v -> brvRule v == "BR-003") violations
        length br003 `shouldBe` 1
        brvSeverity (head br003) `shouldBe` RuleWarning

      it "passes for valid 12-digit UPC" $ do
        let ci = emptyCanonicalImport
              { ciReleases = [CanonicalRelease
                  { crTitle = "Test Album"
                  , crSubTitle = Nothing
                  , crReleaseType = Catalog.Album
                  , crReleaseDate = Nothing
                  , crOriginalDate = Nothing
                  , crLabel = Nothing
                  , crCopyrightLine = Nothing
                  , crPhonographicCopyrightLine = Nothing
                  , crGenre = Nothing
                  , crUpc = Just "012345678901"
                  , crCatalogNumber = Nothing
                  , crResourceRefs = []
                  , crSourcePartyRef = "R1"
                  }]
              }
            violations = validateBusinessRules ci
            br003 = filter (\v -> brvRule v == "BR-003") violations
        length br003 `shouldBe` 0

    context "BR-004: Resource references must exist" $ do
      it "reports error for non-existent resource reference" $ do
        let ci = emptyCanonicalImport
              { ciReleases = [CanonicalRelease
                  { crTitle = "Test Album"
                  , crSubTitle = Nothing
                  , crReleaseType = Catalog.Album
                  , crReleaseDate = Nothing
                  , crOriginalDate = Nothing
                  , crLabel = Nothing
                  , crCopyrightLine = Nothing
                  , crPhonographicCopyrightLine = Nothing
                  , crGenre = Nothing
                  , crUpc = Nothing
                  , crCatalogNumber = Nothing
                  , crResourceRefs = ["A1", "A2"]
                  , crSourcePartyRef = "R1"
                  }]
            , ciResources = [CanonicalResource
                  { cresTitle = "Track 1"
                  , cresSubTitle = Nothing
                  , cresResourceType = Catalog.SoundRecording
                  , cresDurationMs = Just 210000
                  , cresLanguage = Nothing
                  , cresExplicitContent = False
                  , cresIsrc = Just "USRC17607839"
                  , cresGRid = Nothing
                  , cresSourcePartyRef = "A1"
                  }]
              }
            violations = validateBusinessRules ci
            br004 = filter (\v -> brvRule v == "BR-004") violations
        length br004 `shouldBe` 1
        brvSeverity (head br004) `shouldBe` RuleError

    context "BR-005: Territory codes validation" $ do
      it "warns on invalid territory code" $ do
        let ci = emptyCanonicalImport
              { ciDeals = [CanonicalDeal
                  { cdealReleaseRef = Just "R1"
                  , cdealResourceRef = Nothing
                  , cdealModel = Catalog.DistributionAgreement
                  , cdealTerritories = ["INVALID"]
                  , cdealStartDate = read "2026-01-01 00:00:00 UTC"
                  , cdealEndDate = Nothing
                  , cdealPartnerName = "Test Partner"
                  }]
              }
            violations = validateBusinessRules ci
            br005 = filter (\v -> brvRule v == "BR-005") violations
        length br005 `shouldBe` 1
        brvSeverity (head br005) `shouldBe` RuleWarning

      it "passes for valid territory codes" $ do
        let ci = emptyCanonicalImport
              { ciDeals = [CanonicalDeal
                  { cdealReleaseRef = Just "R1"
                  , cdealResourceRef = Nothing
                  , cdealModel = Catalog.DistributionAgreement
                  , cdealTerritories = ["Worldwide", "US", "GB"]
                  , cdealStartDate = read "2026-01-01 00:00:00 UTC"
                  , cdealEndDate = Nothing
                  , cdealPartnerName = "Test Partner"
                  }]
              }
            violations = validateBusinessRules ci
            br005 = filter (\v -> brvRule v == "BR-005") violations
        length br005 `shouldBe` 0

    context "BR-006: Copyright lines" $ do
      it "warns on missing copyright lines" $ do
        let ci = emptyCanonicalImport
              { ciReleases = [CanonicalRelease
                  { crTitle = "Test Album"
                  , crSubTitle = Nothing
                  , crReleaseType = Catalog.Album
                  , crReleaseDate = Nothing
                  , crOriginalDate = Nothing
                  , crLabel = Nothing
                  , crCopyrightLine = Nothing
                  , crPhonographicCopyrightLine = Nothing
                  , crGenre = Nothing
                  , crUpc = Nothing
                  , crCatalogNumber = Nothing
                  , crResourceRefs = []
                  , crSourcePartyRef = "R1"
                  }]
              }
            violations = validateBusinessRules ci
            br006 = filter (\v -> brvRule v == "BR-006") violations
        length br006 `shouldBe` 1
        brvSeverity (head br006) `shouldBe` RuleWarning

-- | Empty canonical import for testing
emptyCanonicalImport :: CanonicalImport
emptyCanonicalImport = CanonicalImport
  { ciReleases = []
  , ciResources = []
  , ciParties = []
  , ciCredits = []
  , ciDeals = []
  , ciSourceDocumentId = 0
  }
