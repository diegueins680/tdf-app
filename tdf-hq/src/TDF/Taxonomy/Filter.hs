{-# LANGUAGE OverloadedStrings #-}

-- | Shared location and genre filters (TAX-FILTER-001).
--
-- Every filterable surface (community search, radio, music, events) accepts
-- the same four dimensions: country, region (subdivision), city and genre.
-- Values inside one dimension are ORed; dimensions are ANDed; a parent genre
-- includes its subgenres; an item without a value for an active dimension is
-- hidden. The rules themselves live in the SQL functions created by
-- @2026-11-21_location_genre_filters.sql@; this module only validates input
-- and builds the SQL fragments and parameters that call them.
module TDF.Taxonomy.Filter
  ( TaxonomyFilter(..)
  , emptyTaxonomyFilter
  , taxonomyFilterIsEmpty
  , mkTaxonomyFilter
  , maxValuesPerDimension
  , taxonomyInputColumnsSql
  , taxonomyInputParams
  , taxonomyMatchSql
  , taxonomyFacetsSql
  , postgresUuidArrayParam
  ) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..))

data TaxonomyFilter = TaxonomyFilter
  { tfCountryIds :: [UUID]
  , tfRegionIds :: [UUID]
  , tfCityIds :: [UUID]
  , tfGenreIds :: [UUID]
  } deriving (Eq, Show)

emptyTaxonomyFilter :: TaxonomyFilter
emptyTaxonomyFilter = TaxonomyFilter [] [] [] []

taxonomyFilterIsEmpty :: TaxonomyFilter -> Bool
taxonomyFilterIsEmpty value = value == emptyTaxonomyFilter

-- | Bound on selected values per dimension; enough for any real selection and
-- small enough to keep the closure and the ANY() checks cheap.
maxValuesPerDimension :: Int
maxValuesPerDimension = 50

-- | Deduplicate (keeping first-seen order) and bound each dimension. A legacy
-- single-value parameter (e.g. @cityId@) is merged into its list.
mkTaxonomyFilter :: [UUID] -> [UUID] -> [UUID] -> [UUID] -> Either Text TaxonomyFilter
mkTaxonomyFilter countries regions cities genres = do
  countryIds <- bounded "countryIds" countries
  regionIds <- bounded "regionIds" regions
  cityIds <- bounded "cityIds" cities
  genreIds <- bounded "genreIds" genres
  pure (TaxonomyFilter countryIds regionIds cityIds genreIds)
  where
    bounded name values =
      let unique = nub values
      in if length unique > maxValuesPerDimension
           then Left (name <> " accepts at most " <> T.pack (show maxValuesPerDimension) <> " values")
           else Right unique

-- | Columns for a parameterised input CTE, in the order of 'taxonomyInputParams'.
-- The genre selection is expanded to its closure once, here.
taxonomyInputColumnsSql :: Text
taxonomyInputColumnsSql =
  "?::uuid[] tax_country_ids,?::uuid[] tax_region_ids,?::uuid[] tax_city_ids,"
    <> "taxonomy_genre_closure(?::uuid[]) tax_genre_ids"

taxonomyInputParams :: TaxonomyFilter -> [PersistValue]
taxonomyInputParams value =
  map postgresUuidArrayParam
    [tfCountryIds value, tfRegionIds value, tfCityIds value, tfGenreIds value]

-- | Predicate over one item. @input@ names the relation that carries the
-- columns of 'taxonomyInputColumnsSql'; the other arguments are SQL
-- expressions for the item's city id, country id and genre id array.
--
-- Each check is guarded by its own emptiness test so an unfiltered query never
-- evaluates the per-item location or genre expressions (function arguments
-- are computed before the call, even when the function would ignore them).
taxonomyMatchSql :: Text -> Text -> Text -> Text -> Text
taxonomyMatchSql input cityExpr countryExpr genresExpr =
  "(cardinality(" <> input <> ".tax_country_ids)+cardinality(" <> input <> ".tax_region_ids)+cardinality(" <> input <> ".tax_city_ids)=0"
    <> " OR taxonomy_location_matches(" <> cityExpr <> "," <> countryExpr <> ","
    <> input <> ".tax_country_ids," <> input <> ".tax_region_ids," <> input <> ".tax_city_ids))"
    <> " AND (cardinality(" <> input <> ".tax_genre_ids)=0 OR taxonomy_genres_match(" <> genresExpr <> "," <> input <> ".tax_genre_ids))"

-- | A jsonb expression with per-value counts for each dimension, computed
-- over @base@ (columns: @item_key@, @city_id@, @country_id@, @genre_ids@) that
-- already satisfies every non-taxonomy filter. Each dimension's counts apply
-- the other three taxonomy dimensions but not its own, so selecting a city
-- still shows how many items the sibling cities have. Genre counts roll up
-- to ancestors and count each item once per genre.
taxonomyFacetsSql :: Text -> Text -> Text
taxonomyFacetsSql base input =
  "jsonb_build_object("
    <> "'genres'," <> counts
         ("SELECT rollup.ancestor_id facet_id,count(DISTINCT b.item_key) total FROM " <> base <> " b CROSS JOIN " <> input
          <> " CROSS JOIN LATERAL unnest(b.genre_ids) item_genre(genre_id)"
          -- The genre -> ancestor rollup is computed once per genre, not per item.
          <> " JOIN (SELECT rolled.id genre_id,ancestor.ancestor_id FROM genre rolled"
          <> " CROSS JOIN LATERAL unnest(taxonomy_genre_ancestors(ARRAY[rolled.id])) ancestor(ancestor_id)) rollup"
          <> " ON rollup.genre_id=item_genre.genre_id WHERE "
          <> location input "tax_country_ids" "tax_region_ids" "tax_city_ids" <> " GROUP BY 1")
    <> ",'countries'," <> counts
         ("SELECT coalesce(b.country_id,city.country_id) facet_id,count(DISTINCT b.item_key) total FROM " <> base <> " b CROSS JOIN " <> input
          <> " LEFT JOIN city_reference city ON city.id=b.city_id WHERE coalesce(b.country_id,city.country_id) IS NOT NULL AND "
          <> location input "NULL" "tax_region_ids" "tax_city_ids" <> " AND " <> genres input <> " GROUP BY 1")
    <> ",'regions'," <> counts
         ("SELECT city.subdivision_id facet_id,count(DISTINCT b.item_key) total FROM " <> base <> " b CROSS JOIN " <> input
          <> " JOIN city_reference city ON city.id=b.city_id WHERE city.subdivision_id IS NOT NULL AND "
          <> location input "tax_country_ids" "NULL" "tax_city_ids" <> " AND " <> genres input <> " GROUP BY 1")
    <> ",'cities'," <> counts
         ("SELECT b.city_id facet_id,count(DISTINCT b.item_key) total FROM " <> base <> " b CROSS JOIN " <> input
          <> " WHERE b.city_id IS NOT NULL AND "
          <> location input "tax_country_ids" "tax_region_ids" "NULL" <> " AND " <> genres input <> " GROUP BY 1")
    <> ")"
  where
    counts query =
      "coalesce((SELECT jsonb_agg(jsonb_build_object('id',facet_id,'count',total) ORDER BY total DESC,facet_id) FROM ("
        <> query <> ") facet),'[]'::jsonb)"
    column relation name = if name == "NULL" then "NULL::uuid[]" else relation <> "." <> name
    location relation countries regions cities =
      "taxonomy_location_matches(b.city_id,b.country_id," <> column relation countries <> ","
        <> column relation regions <> "," <> column relation cities <> ")"
    genres relation = "taxonomy_genres_match(b.genre_ids," <> relation <> ".tax_genre_ids)"

-- | PostgreSQL array literal for a @?::uuid[]@ parameter.
postgresUuidArrayParam :: [UUID] -> PersistValue
postgresUuidArrayParam values =
  PersistText ("{" <> T.intercalate "," (map UUID.toText values) <> "}")
