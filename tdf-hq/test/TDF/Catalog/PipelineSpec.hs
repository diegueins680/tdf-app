{-# LANGUAGE OverloadedStrings #-}

module TDF.Catalog.PipelineSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (isInfixOf)
import Test.Hspec
import TDF.API.Types

spec :: Spec
spec = describe "canonical pipeline API contract" $ do
  it "round-trips persisted workflow definitions with UUID-backed stages and services" $ do
    let definition = PipelineDefinitionDTO
          { pdWorkflowId = "00000000-0000-4000-8000-000000000106"
          , pdCode = "pipeline-mixing"
          , pdNameEs = "Pipeline de mezcla"
          , pdNameEn = "Mixing pipeline"
          , pdRevision = 18
          , pdServiceOfferings =
              [PipelineServiceOfferingDTO "10000000-0000-4000-8000-000000000001" "mixing" "Mezcla" "Mixing"]
          , pdStages =
              [PipelineStageDTO "00000000-0000-4000-8000-000000000251" "brief" "Brief" "Brief" 10 False]
          }
    eitherDecode (encode definition) `shouldBe` Right definition

  it "round-trips one revisioned snapshot for bounded mobile synchronization" $ do
    let snapshot = PipelineSnapshotDTO 12 [] []
    eitherDecode (encode snapshot) `shouldBe` Right snapshot

  it "does not expose legacy type or stage string fields on cards" $ do
    let card = PipelineCardDTO
          { pcId = "30000000-0000-4000-8000-000000000001"
          , pcTitle = "Single A"
          , pcArtist = Just "Arkabuz"
          , pcServiceOfferingId = "10000000-0000-4000-8000-000000000001"
          , pcServiceOfferingCode = "mixing"
          , pcWorkflowId = "00000000-0000-4000-8000-000000000106"
          , pcWorkflowStateId = "00000000-0000-4000-8000-000000000251"
          , pcWorkflowStateCode = "brief"
          , pcWorkflowStateNameEs = "Brief"
          , pcWorkflowStateNameEn = "Brief"
          , pcSortOrder = 10
          , pcNotes = Nothing
          }
        payload = encode card
    BL8.unpack payload `shouldSatisfy` (not . isInfixOf "\"stage\"")
    BL8.unpack payload `shouldSatisfy` (not . isInfixOf "\"type\"")
    eitherDecode payload `shouldBe` Right card

  it "rejects legacy string create and update writes" $ do
    let legacyCreate = BL8.pack
          "{\"title\":\"Single A\",\"serviceOfferingId\":\"10000000-0000-4000-8000-000000000001\",\"stage\":\"Brief\"}"
        legacyUpdate = BL8.pack "{\"stage\":\"Approved\"}"
    (eitherDecode legacyCreate :: Either String PipelineCardCreate) `shouldSatisfy` isLeft
    (eitherDecode legacyUpdate :: Either String PipelineCardUpdate) `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
