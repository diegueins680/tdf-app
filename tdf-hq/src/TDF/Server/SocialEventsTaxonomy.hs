{-# LANGUAGE OverloadedStrings #-}
-- | TAX-FILTER-001 for social events. An event's location is its venue's (the
-- city id, or the venue's city text resolved to exactly one reference); its
-- genres are the canonical genres of its lineup.
module TDF.Server.SocialEventsTaxonomy
  ( eventTaxonomyClause
  , eventTaxonomyFacets
  , taxonomyFilterError
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql)
import Servant (ServerError(..), err400)

import qualified TDF.CMS.Models as CMS
import TDF.Taxonomy.Filter (TaxonomyFilter, taxonomyFacetsSql, taxonomyFilterIsEmpty, taxonomyInputColumnsSql, taxonomyInputParams, taxonomyMatchSql)

taxonomyFilterError :: Text -> ServerError
taxonomyFilterError message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

-- | The list's extra WHERE clause (over @social_event@) and its parameters;
-- empty when nothing is selected.
eventTaxonomyClause :: TaxonomyFilter -> (Text, [PersistValue])
eventTaxonomyClause taxonomy
  | taxonomyFilterIsEmpty taxonomy = ("", [])
  | otherwise =
      ( "EXISTS (SELECT 1 FROM (SELECT " <> taxonomyInputColumnsSql <> ") tax_input WHERE "
          <> taxonomyMatchSql "tax_input" eventCitySql eventCountrySql eventGenresSql <> ")"
      , taxonomyInputParams taxonomy
      )

-- | Per-value counts over the given events (see 'taxonomyFacetsSql').
eventTaxonomyFacets :: TaxonomyFilter -> [Int64] -> SqlPersistT IO Aeson.Value
eventTaxonomyFacets taxonomy eventIds = do
  rows <- rawSql
    ( "WITH tax_input AS (SELECT " <> taxonomyInputColumnsSql <> "), "
        <> "event_base AS (SELECT social_event.id::text item_key," <> eventCitySql <> " city_id,"
        <> eventCountrySql <> " country_id," <> eventGenresSql <> " genre_ids "
        <> "FROM social_event WHERE social_event.id=ANY(?::bigint[])) "
        <> "SELECT " <> taxonomyFacetsSql "event_base" "tax_input"
    )
    (taxonomyInputParams taxonomy <> [PersistText ("{" <> T.intercalate "," (map (T.pack . show) eventIds) <> "}")])
  pure $ case rows of
    Single (CMS.AesonValue value) : _ -> value
    [] -> Aeson.object []

eventCitySql :: Text
eventCitySql =
  "(SELECT coalesce(event_venue.city_id,taxonomy_city_from_text(event_venue.city,event_venue.country_id)) FROM venue event_venue WHERE event_venue.id=social_event.venue_id)"

eventCountrySql :: Text
eventCountrySql =
  "(SELECT coalesce(event_venue.country_id,(SELECT country.id FROM country_reference country WHERE country.alpha2=upper(btrim(event_venue.country_code)))) FROM venue event_venue WHERE event_venue.id=social_event.venue_id)"

eventGenresSql :: Text
eventGenresSql =
  "ARRAY(SELECT DISTINCT lineup_genre.genre_id FROM event_artist lineup JOIN artist_genre_membership lineup_genre ON lineup_genre.artist_id=lineup.artist_id WHERE lineup.event_id=social_event.id)"
