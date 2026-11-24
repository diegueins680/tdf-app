{-# LANGUAGE OverloadedStrings #-}

-- | TAX-FILTER-001: shared filter input and SQL fragments.
module TDF.Taxonomy.FilterSpec (spec) where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import Data.String (fromString)
import qualified Database.PostgreSQL.Simple as PG
import System.Environment (lookupEnv)
import TDF.ServerRadio (radioFacetsQuery)
import TDF.Test.DisposablePostgres (isDisposablePostgresUrl)
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..))
import Test.Hspec

import TDF.Server.Directory (directoryTaxonomyFacetsSql)
import TDF.Taxonomy.Filter

spec :: Spec
spec = describe "TAX-FILTER-001 shared taxonomy filters" $ do
  let uuid n = fromJust (UUID.fromString ("00000000-0000-4000-8000-00000000000" <> show (n :: Int)))

  it "deduplicates each dimension, keeping first-seen order" $
    mkTaxonomyFilter [uuid 1, uuid 2, uuid 1] [] [uuid 3] [uuid 4, uuid 4]
      `shouldBe` Right (TaxonomyFilter [uuid 1, uuid 2] [] [uuid 3] [uuid 4])

  it "bounds every dimension to 50 values" $ do
    let many = [UUID.fromWords 0 0 0 (fromIntegral n) | n <- [1 .. 51 :: Int]]
    mkTaxonomyFilter many [] [] [] `shouldSatisfy` isLeft
    mkTaxonomyFilter [] [] [] (take 50 many) `shouldSatisfy` either (const False) (const True)

  it "treats a filter with no values as no filter" $ do
    taxonomyFilterIsEmpty emptyTaxonomyFilter `shouldBe` True
    taxonomyFilterIsEmpty (TaxonomyFilter [] [] [] [uuid 1]) `shouldBe` False

  it "passes the four dimensions as PostgreSQL uuid arrays, genres last (expanded in SQL)" $
    taxonomyInputParams (TaxonomyFilter [uuid 1] [] [uuid 2, uuid 3] [])
      `shouldBe`
        [ PersistText "{00000000-0000-4000-8000-000000000001}"
        , PersistText "{}"
        , PersistText "{00000000-0000-4000-8000-000000000002,00000000-0000-4000-8000-000000000003}"
        , PersistText "{}"
        ]

  it "expands the genre selection to its closure once, in the input" $
    taxonomyInputColumnsSql `shouldSatisfy` T.isInfixOf "taxonomy_genre_closure(?::uuid[]) tax_genre_ids"

  it "matches items through the shared location and genre predicates" $ do
    let clause = taxonomyMatchSql "input" "item.city_id" "item.country_id" "item.genre_ids"
    clause `shouldSatisfy` T.isInfixOf "taxonomy_location_matches(item.city_id,item.country_id,input.tax_country_ids,input.tax_region_ids,input.tax_city_ids)"
    clause `shouldSatisfy` T.isInfixOf "taxonomy_genres_match(item.genre_ids,input.tax_genre_ids)"
    clause `shouldSatisfy` T.isPrefixOf "(cardinality(input.tax_country_ids)+cardinality(input.tax_region_ids)+cardinality(input.tax_city_ids)=0 OR "
    clause `shouldSatisfy` T.isInfixOf "(cardinality(input.tax_genre_ids)=0 OR "

  it "counts each dimension without its own selection" $ do
    let facets = taxonomyFacetsSql "base" "input"
    facets `shouldSatisfy` T.isInfixOf "taxonomy_location_matches(b.city_id,b.country_id,NULL::uuid[],input.tax_region_ids,input.tax_city_ids)"
    facets `shouldSatisfy` T.isInfixOf "taxonomy_location_matches(b.city_id,b.country_id,input.tax_country_ids,NULL::uuid[],input.tax_city_ids)"
    facets `shouldSatisfy` T.isInfixOf "taxonomy_location_matches(b.city_id,b.country_id,input.tax_country_ids,input.tax_region_ids,NULL::uuid[])"
    facets `shouldSatisfy` T.isInfixOf "unnest(taxonomy_genre_ancestors(ARRAY[rolled.id]))"
    T.count "?" facets `shouldBe` 0

  it "keeps the directory's literal facet SQL equal to the shared fragment" $
    directoryTaxonomyFacetsSql `shouldBe` taxonomyFacetsSql "taxonomy_base" "input"

  it "counts radio stations by the country of a selected city, retaining the city in country counts" $ do
    connection <- lookupEnv "TDF_DISCOVERY_TEST_DATABASE_URL"
    ci <- lookupEnv "CI"
    case connection of
      Nothing -> pendingWith "Set TDF_DISCOVERY_TEST_DATABASE_URL to the migrated discovery test database"
      Just url | isDisposablePostgresUrl (ci == Just "true") url ->
        bracket (PG.connectPostgreSQL (BS.pack url)) PG.close $ \db -> PG.withTransaction db $ do
          _ <- PG.execute_ db "CREATE TEMP TABLE radio_stream(country_id uuid, genre_id uuid, is_active boolean) ON COMMIT DROP"
          _ <- PG.execute_ db "INSERT INTO radio_stream SELECT id,NULL,true FROM country_reference WHERE alpha2 IN ('EC','US')"
          [PG.Only city] <- PG.query_ db "SELECT id::text FROM city_reference WHERE code='quito-ec-p'"
          let facets = "WITH facets AS (" <> radioFacetsQuery <> " AS counts) SELECT \
                \(counts->'countries') = jsonb_build_array(jsonb_build_object('id', (SELECT id FROM country_reference WHERE alpha2='EC'), 'count', 1)), \
                \(counts->'regions') @> jsonb_build_array(jsonb_build_object('id', (SELECT id FROM subdivision_reference WHERE code='EC-P'), 'count', 1)), \
                \(counts->'cities') @> jsonb_build_array(jsonb_build_object('id', (SELECT id FROM city_reference WHERE code='quito-ec-p'), 'count', 1)) FROM facets"
              empty = "{}" :: String
          result <- PG.query db (fromString (T.unpack facets)) (empty, empty, "{" <> (city :: String) <> "}", empty)
          result `shouldBe` [(True, True, True)]
      Just _ -> expectationFailure "Expected a disposable discovery test database"
