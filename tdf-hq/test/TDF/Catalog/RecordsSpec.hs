{-# LANGUAGE OverloadedStrings #-}

module TDF.Catalog.RecordsSpec (spec) where

import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (isLeft)
import Test.Hspec

import TDF.API.Catalog
  ( AuthoredContentDTO (..)
  , ContentTypeDTO (..)
  , RecordsCollectionDTO (..)
  , RecordsFeedDTO (..)
  , WorkflowStateDTO (..)
  , WorkflowTransitionDTO (..)
  )
import TDF.Seed (durationTextToMilliseconds)

spec :: Spec
spec = do
  describe "Records feed JSON contract" $ do
    it "uses canonical wire field names and UUID identifiers" $ do
      let collection =
            RecordsCollectionDTO
              "4f879db7-3217-4a09-89ce-10afc7a40ec4"
              "tdf-records-releases"
              "release"
              "RELEASES by TDF"
              Nothing
              (Just "/records")
              []
              3
          feed = RecordsFeedDTO "es" 7 [collection] [] [] []
          expected =
            object
              [ "locale" .= ("es" :: String)
              , "revision" .= (7 :: Int)
              , "collections" .=
                  [ object
                      [ "id" .= ("4f879db7-3217-4a09-89ce-10afc7a40ec4" :: String)
                      , "code" .= ("tdf-records-releases" :: String)
                      , "kind" .= ("release" :: String)
                      , "name" .= ("RELEASES by TDF" :: String)
                      , "publicRoute" .= ("/records" :: String)
                      , "resources" .= ([] :: [Value])
                      , "revision" .= (3 :: Int)
                      ]
                  ]
              , "releases" .= ([] :: [Value])
              , "recordings" .= ([] :: [Value])
              , "sessions" .= ([] :: [Value])
              ]
      eitherDecode (encode feed) `shouldBe` Right expected
      eitherDecode (encode feed) `shouldBe` Right feed

    it "rejects the former arbitrary CMS payload shape" $
      ( eitherDecode
          (BL8.pack "{\"locale\":\"es\",\"revision\":1,\"collections\":[],\"releases\":[],\"recordings\":[],\"sessions\":[],\"payload\":{}}")
          :: Either String RecordsFeedDTO
      ) `shouldSatisfy` isLeft

  describe "persisted content type JSON contract" $ do
    it "returns strict schema and route metadata without slug-derived type inference" $ do
      let contentType = ContentTypeDTO
            "8e7382b1-31c4-46e1-9905-40389f7ec2fc"
            "fan-hub-page"
            "authored_page"
            "Página Fan Hub"
            "Página Fan Hub"
            "Fan Hub page"
            Nothing
            Nothing
            Nothing
            (object ["type" .= ("object" :: String), "required" .= (["heroTitle"] :: [String])])
            2
            (Just "/fans")
            (Just "/cms/{id}")
            True
            True
            "published"
            4
          encoded = encode contentType
      eitherDecode encoded `shouldBe` Right contentType
      (eitherDecode encoded :: Either String Value) `shouldBe` Right
        (object
          [ "id" .= ("8e7382b1-31c4-46e1-9905-40389f7ec2fc" :: String)
          , "code" .= ("fan-hub-page" :: String)
          , "entityKind" .= ("authored_page" :: String)
          , "name" .= ("Página Fan Hub" :: String)
          , "nameEs" .= ("Página Fan Hub" :: String)
          , "nameEn" .= ("Fan Hub page" :: String)
          , "schema" .= object ["type" .= ("object" :: String), "required" .= (["heroTitle"] :: [String])]
          , "schemaVersion" .= (2 :: Int)
          , "publicRoutePattern" .= ("/fans" :: String)
          , "adminRoutePattern" .= ("/cms/{id}" :: String)
          , "publicRead" .= True
          , "active" .= True
          , "workflowState" .= ("published" :: String)
          , "version" .= (4 :: Int)
          ])

    it "rejects unknown content type response fields" $
      ( eitherDecode
          (BL8.pack "{\"id\":\"8e7382b1-31c4-46e1-9905-40389f7ec2fc\",\"code\":\"fan-hub-page\",\"entityKind\":\"authored_page\",\"name\":\"Fan Hub page\",\"nameEs\":\"Fan Hub page\",\"nameEn\":\"Fan Hub page\",\"schema\":{},\"schemaVersion\":1,\"publicRead\":true,\"active\":true,\"workflowState\":\"published\",\"version\":1,\"slugPrefix\":\"fan-\"}")
          :: Either String ContentTypeDTO
      ) `shouldSatisfy` isLeft

    it "keeps authored-content identity and routes separate from URL aliases" $ do
      let authored = AuthoredContentDTO
            { acId = "20000000-0000-4000-8000-000000000001"
            , acCode = "fan-hub"
            , acContentTypeId = "30000000-0000-4000-8000-000000000001"
            , acContentTypeCode = "fan-hub-page"
            , acEntityKind = "authored_page"
            , acName = "Fan Hub"
            , acNameEs = "Fan Hub"
            , acNameEn = "Fan Hub"
            , acDescription = Nothing
            , acDescriptionEs = Nothing
            , acDescriptionEn = Nothing
            , acCurrentSlug = "fan-hub"
            , acPublicRoute = Just "/fans"
            , acSchema = object ["type" .= ("object" :: String)]
            , acSchemaVersion = 2
            , acSortOrder = 10
            , acActive = True
            , acWorkflowState = "published"
            , acRevision = 1
            , acVersion = 1
            }
      eitherDecode (encode authored) `shouldBe` Right authored
      (eitherDecode (encode authored) :: Either String Value) `shouldBe` Right
        (object
          [ "id" .= ("20000000-0000-4000-8000-000000000001" :: String)
          , "code" .= ("fan-hub" :: String)
          , "contentTypeId" .= ("30000000-0000-4000-8000-000000000001" :: String)
          , "contentTypeCode" .= ("fan-hub-page" :: String)
          , "entityKind" .= ("authored_page" :: String)
          , "name" .= ("Fan Hub" :: String)
          , "nameEs" .= ("Fan Hub" :: String)
          , "nameEn" .= ("Fan Hub" :: String)
          , "currentSlug" .= ("fan-hub" :: String)
          , "publicRoute" .= ("/fans" :: String)
          , "schema" .= object ["type" .= ("object" :: String)]
          , "schemaVersion" .= (2 :: Int)
          , "sortOrder" .= (10 :: Int)
          , "active" .= True
          , "workflowState" .= ("published" :: String)
          , "revision" .= (1 :: Int)
          , "version" .= (1 :: Int)
          ])

    it "returns localized workflow labels from the persisted security registry" $ do
      let state = WorkflowStateDTO
            "00000000-0000-4000-8000-000000000205"
            "00000000-0000-4000-8000-000000000101"
            "catalog-publication"
            "published"
            "Publicado"
            "Publicado"
            "Published"
            50
            True
            True
            ["initial"]
            ["public-listable"]
            [WorkflowTransitionDTO "00000000-0000-4000-8000-000000000206" True False False Nothing Nothing 1]
            1
      eitherDecode (encode state) `shouldBe` Right state

    it "rejects unknown workflow-state response fields" $
      ( eitherDecode
          (BL8.pack "{\"id\":\"00000000-0000-4000-8000-000000000205\",\"workflowId\":\"00000000-0000-4000-8000-000000000101\",\"workflowCode\":\"catalog-publication\",\"code\":\"published\",\"name\":\"Publicado\",\"nameEs\":\"Publicado\",\"nameEn\":\"Published\",\"sortOrder\":50,\"terminal\":true,\"active\":true,\"version\":1,\"slug\":\"published\"}")
          :: Either String WorkflowStateDTO
      ) `shouldSatisfy` isLeft

  describe "persisted Records duration normalization" $ do
    it "normalizes supported minute and hour forms to milliseconds" $ do
      durationTextToMilliseconds "03:17" `shouldBe` Just 197000
      durationTextToMilliseconds "1:02:03" `shouldBe` Just 3723000

    it "withholds malformed duration values instead of guessing" $ do
      durationTextToMilliseconds "3:75" `shouldBe` Nothing
      durationTextToMilliseconds "unknown" `shouldBe` Nothing
      durationTextToMilliseconds "-1:05" `shouldBe` Nothing
