{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.ERN.V432.ParseSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import Data.Either (isRight, isLeft)
import Data.Text (Text)
import TDF.DDEX.ERN.V432.Parse
import TDF.DDEX.ERN.V432.Types

spec :: Spec
spec = do
  describe "parseErnMessage" $ do
    context "with valid single release" $ do
      it "parses successfully" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-valid.xml"
        let result = parseErnMessage content
        result `shouldSatisfy` isRight

      it "extracts message header" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-valid.xml"
        let Right ern = parseErnMessage content
            header = ernMessageHeader ern
        mhMessageId header `shouldBe` "MSG-20260804-001"
        mhMessageThreadId header `shouldBe` Just "THREAD-001"

      it "extracts parties" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-valid.xml"
        let Right ern = parseErnMessage content
            parties = ernPartyList ern
        length parties `shouldBe` 2

      it "extracts resources" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-valid.xml"
        let Right ern = parseErnMessage content
            resources = ernResourceList ern
        length resources `shouldBe` 1
        let res = head resources
        resourceTitle res `shouldBe` "Test Song"
        resourceReference res `shouldBe` ResourceReference "A1"

      it "extracts ISRC" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-valid.xml"
        let Right ern = parseErnMessage content
            resources = ernResourceList ern
            res = head resources
            isrc = extractIsrcFromResource res
        isrc `shouldBe` Just "USTDF2600001"

      it "extracts releases" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-valid.xml"
        let Right ern = parseErnMessage content
            releases = ernReleaseList ern
        length releases `shouldBe` 1
        let rel = head releases
        releaseTitle rel `shouldBe` "Test Single"
        releaseType rel `shouldBe` ReleaseTypeSingle

      it "extracts UPC" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-valid.xml"
        let Right ern = parseErnMessage content
            releases = ernReleaseList ern
            rel = head releases
            upc = extractUpcFromRelease rel
        upc `shouldBe` Just "012345678901"

      it "extracts resource groups" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-valid.xml"
        let Right ern = parseErnMessage content
            groups = ernResourceGroups ern
        length groups `shouldBe` 1

      it "extracts deals" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-valid.xml"
        let Right ern = parseErnMessage content
            deals = ernDealList ern
        length deals `shouldBe` 1

    context "with valid album release" $ do
      it "parses successfully" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/album-valid.xml"
        let result = parseErnMessage content
        result `shouldSatisfy` isRight

      it "extracts multiple resources" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/album-valid.xml"
        let Right ern = parseErnMessage content
            resources = ernResourceList ern
        length resources `shouldBe` 3

      it "extracts album release type" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/album-valid.xml"
        let Right ern = parseErnMessage content
            releases = ernReleaseList ern
            rel = head releases
        releaseType rel `shouldBe` ReleaseTypeAlbum

      it "extracts release subtitle" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/album-valid.xml"
        let Right ern = parseErnMessage content
            releases = ernReleaseList ern
            rel = head releases
        releaseSubTitle rel `shouldBe` Just "Deluxe Edition"

      it "extracts copyright lines" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/album-valid.xml"
        let Right ern = parseErnMessage content
            releases = ernReleaseList ern
            rel = head releases
        releaseCopyrightLine rel `shouldBe` Just "(C) 2026 TDF Records"
        releasePhonographicCopyrightLine rel `shouldBe` Just "(P) 2026 TDF Records"

      it "extracts multiple deals" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/album-valid.xml"
        let Right ern = parseErnMessage content
            deals = ernDealList ern
        length deals `shouldBe` 2

      it "extracts contributors" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/album-valid.xml"
        let Right ern = parseErnMessage content
            resources = ernResourceList ern
            firstResource = head resources
            contributors = resourceContributors firstResource
        length contributors `shouldBe` 3

    context "with invalid business rules" $ do
      it "parses successfully (parsing doesn't validate business rules)" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-invalid-business-rules.xml"
        let result = parseErnMessage content
        result `shouldSatisfy` isRight

      it "extracts resource without ISRC" $ do
        content <- BL.readFile "test/fixtures/ddex/ern-v432/single-invalid-business-rules.xml"
        let Right ern = parseErnMessage content
            resources = ernResourceList ern
            res = head resources
            isrc = extractIsrcFromResource res
        isrc `shouldBe` Nothing

    context "with invalid XML" $ do
      it "fails on malformed XML" $ do
        let content = BL.fromStrict "<?xml version=\"1.0\"?><unclosed>"
            result = parseErnMessage content
        result `shouldSatisfy` isLeft

      it "fails on missing MessageHeader" $ do
        let content = BL.fromStrict "<?xml version=\"1.0\"?><ernNewReleaseMessage xmlns=\"http://ddex.net/xml/ern/432\"></ernNewReleaseMessage>"
            result = parseErnMessage content
        result `shouldSatisfy` isLeft

-- Helper functions to extract identifiers
extractIsrcFromResource :: Resource -> Maybe Text
extractIsrcFromResource Resource{..} =
  case resourceIds of
    (ResourceIdISRC isrc:_) -> Just (formatISRC isrc)
    _ -> Nothing
  where
    formatISRC ISRC{..} = isrcCountryCode <> isrcRegistrant <> isrcYear <> isrcDesignation

extractUpcFromRelease :: Release -> Maybe Text
extractUpcFromRelease Release{..} =
  case releaseIds of
    (ReleaseIdUPC upc:_) -> Just upc
    _ -> Nothing
