{-# LANGUAGE OverloadedStrings #-}

-- | Venue search (EVT-VENUE-SEARCH-001, formal/system/venue-search.md).
--
-- 'venueSearchSpec' is the specification: the filter the venue list has always
-- applied in memory, as a pure, total function. 'loadVenueSearch' evaluates the
-- same filter in PostgreSQL so that no row cap is needed. The two agree on
-- every venue whose distance is not within 'venueSearchToleranceKm' of the
-- radius; the specification is the authority and the SQL is tested against it.
--
-- Text matching compares the case-folded needle with @venue_search_fold@ of
-- each column ("TDF.Venues.SearchFold"). Distance uses 'venueBoundingBox' as a
-- prefilter and then the haversine formula with the operations of
-- 'haversineDistanceKm' in the same order.
module TDF.Venues.Search
  ( VenueSearchFields (..)
  , VenueSearchQuery (..)
  , BoundingBox (..)
  , earthRadiusKm
  , venueSearchToleranceKm
  , haversineDistanceKm
  , matchesVenueText
  , matchesVenueNear
  , venueSearchSpec
  , venueBoundingBox
  , inVenueBoundingBox
  , escapeLikeNeedle
  , venueSearchSelectSql
  , venueSearchCityClauseSql
  , venueSearchTextClauseSql
  , venueSearchNearClauseSql
  , venueSearchPageSql
  , venueSearchStatement
  , loadVenueSearch
  , venueSqlSearchFlag
  , venueSqlSearchEnvironment
  , venueSqlSearchInstalledSql
  , venueSqlSearchEnabledSql
  , venueSqlSearchEnabled
  ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Data.Char                     (toLower)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Database.Persist              (Entity, PersistValue (..))
import           Database.Persist.Sql          (Single (..), SqlPersistT, rawSql)

import           TDF.Models.SocialEventsModels (Venue)

-- | The columns of a venue the search reads.
data VenueSearchFields = VenueSearchFields
  { vsfName      :: !Text
  , vsfCity      :: !(Maybe Text)
  , vsfAddress   :: !(Maybe Text)
  , vsfLatitude  :: !(Maybe Double)
  , vsfLongitude :: !(Maybe Double)
  } deriving (Eq, Show)

-- | A validated venue list request.
data VenueSearchQuery = VenueSearchQuery
  { vsqCity   :: !(Maybe Text)
    -- ^ exact, case-sensitive city
  , vsqNeedle :: !(Maybe Text)
    -- ^ already case-folded; 'Nothing' or empty means no text filter
  , vsqNear   :: !(Maybe (Double, Double, Double))
    -- ^ latitude, longitude, radius in kilometres
  , vsqLimit  :: !Int
  , vsqOffset :: !Int
  } deriving (Eq, Show)

earthRadiusKm :: Double
earthRadiusKm = 6371

-- | The SQL filter and the specification may disagree only on a venue whose
-- specified distance is within this many kilometres (1 mm) of the radius.
venueSearchToleranceKm :: Double
venueSearchToleranceKm = 1.0e-6

haversineDistanceKm :: Double -> Double -> Double -> Double -> Double
haversineDistanceKm lat1 lng1 lat2 lng2 =
  let dLat = degToRad (lat2 - lat1)
      dLng = degToRad (lng2 - lng1)
      lat1Rad = degToRad lat1
      lat2Rad = degToRad lat2
      a =
        (sin (dLat / 2) * sin (dLat / 2))
          + (cos lat1Rad * cos lat2Rad * sin (dLng / 2) * sin (dLng / 2))
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
   in earthRadiusKm * c

degToRad :: Double -> Double
degToRad deg = deg * pi / 180

-- | The folded needle occurs in the folded name, city or address.
matchesVenueText :: Text -> VenueSearchFields -> Bool
matchesVenueText needle venue =
  let nameVal = T.toCaseFold (vsfName venue)
      cityVal = maybe "" T.toCaseFold (vsfCity venue)
      addressVal = maybe "" T.toCaseFold (vsfAddress venue)
   in T.isInfixOf needle nameVal || T.isInfixOf needle cityVal || T.isInfixOf needle addressVal

-- | Both coordinates are present and the venue is within the radius.
matchesVenueNear :: (Double, Double, Double) -> VenueSearchFields -> Bool
matchesVenueNear (lat, lng, radiusKm) venue =
  case (vsfLatitude venue, vsfLongitude venue) of
    (Just venueLat, Just venueLng) ->
      haversineDistanceKm lat lng venueLat venueLng <= radiusKm
    _ -> False

-- | The venue list over rows already ordered by name and then id: keep the
-- rows of the city that match the text and the distance, then take the page.
venueSearchSpec :: (a -> VenueSearchFields) -> VenueSearchQuery -> [a] -> [a]
venueSearchSpec fields query =
  take (max 0 (vsqLimit query)) . drop (max 0 (vsqOffset query)) . filter (keep . fields)
  where
    keep venue = inCity venue && hasText venue && isNear venue
    inCity venue = maybe True (\city -> vsfCity venue == Just city) (vsqCity query)
    hasText venue = case vsqNeedle query of
      Just needle | not (T.null needle) -> matchesVenueText needle venue
      _                                -> True
    isNear venue = maybe True (`matchesVenueNear` venue) (vsqNear query)

-- | Latitude and longitude bounds, in degrees, that contain every venue with
-- coordinates in -90..90 and -180..180 that 'matchesVenueNear' accepts. The
-- longitude bounds are around the query longitude and may leave -180..180; a
-- venue is inside when its longitude, or that longitude plus or minus 360, is.
data BoundingBox = BoundingBox
  { bbLatMin :: !Double
  , bbLatMax :: !Double
  , bbLngMin :: !Double
  , bbLngMax :: !Double
  } deriving (Eq, Show)

-- | With @d = (radiusKm + tolerance) / 6371@ radians, so that the box also
-- holds every venue inside the tolerance band: latitude within @d@ of the query, and
-- longitude within @asin (sin d / cos lat)@ (formal/social/venue_search_bounding_box.py).
-- Where that ratio reaches 0.9999, which includes every circle that touches a
-- pole, all longitudes are kept; below it the arcsine is well conditioned.
-- Each half-width is widened to absorb floating-point rounding.
venueBoundingBox :: Double -> Double -> Double -> BoundingBox
venueBoundingBox lat lng radiusKm =
  let angular = (radiusKm + venueSearchToleranceKm) / earthRadiusKm
      widen degrees = degrees * (1 + 1.0e-9) + 1.0e-9
      latHalf = widen (angular * 180 / pi)
      ratio = sin angular / cos (degToRad lat)
      lngHalf = widen (asin ratio * 180 / pi)
      narrow = angular < pi / 2 && ratio >= 0 && ratio < 0.9999
      (lngMin, lngMax)
        | narrow = (lng - lngHalf, lng + lngHalf)
        | otherwise = (-180, 180)
   in BoundingBox (lat - latHalf) (lat + latHalf) lngMin lngMax

-- | The box test the SQL applies to every venue with both coordinates.
inVenueBoundingBox :: BoundingBox -> Double -> Double -> Bool
inVenueBoundingBox box venueLat venueLng =
  bbLatMin box <= venueLat
    && venueLat <= bbLatMax box
    && any inLng [venueLng, venueLng + 360, venueLng - 360]
  where
    inLng value = bbLngMin box <= value && value <= bbLngMax box

-- | Escape the LIKE wildcards with @!@, the escape character the SQL names.
escapeLikeNeedle :: Text -> Text
escapeLikeNeedle = T.concatMap escape
  where
    escape ch
      | ch == '!' || ch == '%' || ch == '_' = T.pack ['!', ch]
      | otherwise = T.singleton ch

venueSearchSelectSql :: Text
venueSearchSelectSql = "SELECT ?? FROM venue WHERE TRUE"

-- | One parameter: the city.
venueSearchCityClauseSql :: Text
venueSearchCityClauseSql = " AND venue.city = ?"

-- | Three parameters: the escaped needle between @%@, once for each column.
venueSearchTextClauseSql :: Text
venueSearchTextClauseSql =
  " AND (venue_search_fold(venue.name) LIKE ? ESCAPE '!'\
  \ OR venue_search_fold(venue.city) LIKE ? ESCAPE '!'\
  \ OR venue_search_fold(venue.address) LIKE ? ESCAPE '!')"

-- | Twelve parameters: the latitude bounds; the longitude bounds three times;
-- the radius; the query latitude and longitude; the query latitude.
--
-- The constraint @venue_coordinates_in_range@ keeps every stored coordinate
-- NULL or inside -90..90 / -180..180, and 'venueSqlSearchEnabled' requires
-- it, so every row the statement reads is one the bounding box covers. A row
-- without both coordinates fails the box tests (NULL) and never matches.
--
-- PostgreSQL raises on floating-point underflow where Haskell returns zero or
-- a subnormal. A difference below 1e-290 degrees is taken as zero, which gives
-- the value Haskell computes, and a squared sine below 1e-240 is dropped, which
-- moves the distance by far less than the stated tolerance. With in-range
-- coordinates nothing else can raise, so the order in which PostgreSQL
-- evaluates the conditions does not matter.
venueSearchNearClauseSql :: Text
venueSearchNearClauseSql =
  " AND venue.latitude BETWEEN ?::float8 AND ?::float8\
  \ AND (venue.longitude BETWEEN ?::float8 AND ?::float8\
  \ OR venue.longitude + 360 BETWEEN ?::float8 AND ?::float8\
  \ OR venue.longitude - 360 BETWEEN ?::float8 AND ?::float8)\
  \ AND (SELECT CASE WHEN h.a >= 0 AND h.a <= 1\
  \ THEN 6371 * (2 * atan2(sqrt(h.a), sqrt(1 - h.a))) <= ?::float8 ELSE FALSE END\
  \ FROM (SELECT (CASE WHEN abs(s.slat) < 1e-120 THEN 0 ELSE s.slat * s.slat END)\
  \ + (CASE WHEN abs(s.slng) < 1e-120 THEN 0 ELSE s.c1 * s.c2 * s.slng * s.slng END) AS a\
  \ FROM (SELECT sin(r.dlat / 2) AS slat, sin(r.dlng / 2) AS slng,\
  \ cos(r.lat1) AS c1, cos(r.lat2) AS c2\
  \ FROM (SELECT\
  \ CASE WHEN abs(venue.latitude - q.lat) < 1e-290 THEN 0\
  \ ELSE (venue.latitude - q.lat) * pi() / 180 END AS dlat,\
  \ CASE WHEN abs(venue.longitude - q.lng) < 1e-290 THEN 0\
  \ ELSE (venue.longitude - q.lng) * pi() / 180 END AS dlng,\
  \ CASE WHEN abs(q.lat1) < 1e-290 THEN 0 ELSE q.lat1 * pi() / 180 END AS lat1,\
  \ CASE WHEN abs(venue.latitude) < 1e-290 THEN 0\
  \ ELSE venue.latitude * pi() / 180 END AS lat2\
  \ FROM (SELECT ?::float8 AS lat, ?::float8 AS lng, ?::float8 AS lat1) q) r) s) h)"

-- | Two parameters: limit and offset.
venueSearchPageSql :: Text
venueSearchPageSql = " ORDER BY venue.name, venue.id LIMIT ? OFFSET ?"

-- | The statement for a query, and its parameters in order.
venueSearchStatement :: VenueSearchQuery -> (Text, [PersistValue])
venueSearchStatement query =
  ( venueSearchSelectSql <> citySql <> textSql <> nearSql <> venueSearchPageSql
  , cityParams <> textParams <> nearParams <> pageParams
  )
  where
    (citySql, cityParams) = case vsqCity query of
      Just city -> (venueSearchCityClauseSql, [PersistText city])
      Nothing   -> ("", [])
    (textSql, textParams) = case vsqNeedle query of
      Just needle | not (T.null needle) ->
        let pattern = PersistText ("%" <> escapeLikeNeedle needle <> "%")
         in (venueSearchTextClauseSql, [pattern, pattern, pattern])
      _ -> ("", [])
    (nearSql, nearParams) = case vsqNear query of
      Just (lat, lng, radiusKm) ->
        let box = venueBoundingBox lat lng radiusKm
            latBounds = [bbLatMin box, bbLatMax box]
            lngBounds = [bbLngMin box, bbLngMax box]
         in ( venueSearchNearClauseSql
            , map PersistDouble
                (latBounds <> lngBounds <> lngBounds <> lngBounds
                  <> [radiusKm, lat, lng, lat])
            )
      Nothing -> ("", [])
    pageParams =
      [ PersistInt64 (fromIntegral (max 0 (vsqLimit query)))
      , PersistInt64 (fromIntegral (max 0 (vsqOffset query)))
      ]

-- | Read one page of venues. One read-only statement.
loadVenueSearch :: MonadIO m => VenueSearchQuery -> SqlPersistT m [Entity Venue]
loadVenueSearch query = uncurry rawSql (venueSearchStatement query)

-- | The runtime flag that selects the SQL path. Off unless a row enables it.
venueSqlSearchFlag :: Text
venueSqlSearchFlag = "social_venues.sql_search"

-- | The flag environment for a raw @APP_ENV@, as the other runtime flags read it.
venueSqlSearchEnvironment :: Maybe String -> Text
venueSqlSearchEnvironment raw =
  case fmap (T.strip . T.pack . map toLower) raw of
    Just "production" -> "production"
    Just "prod"       -> "production"
    _                 -> "sandbox"

-- | Whether the flag table, the fold function and a validated coordinate
-- constraint on the venue table the statement reads all exist. A database
-- without both migrations keeps the in-memory path instead of failing.
venueSqlSearchInstalledSql :: Text
venueSqlSearchInstalledSql =
  "SELECT to_regclass('revenue_feature_flag') IS NOT NULL\
  \ AND to_regprocedure('venue_search_fold(text)') IS NOT NULL\
  \ AND EXISTS (SELECT 1 FROM pg_constraint\
  \ WHERE conname = 'venue_coordinates_in_range'\
  \ AND conrelid = to_regclass('venue') AND contype = 'c' AND convalidated)"

-- | Two parameters: the flag key and the environment.
venueSqlSearchEnabledSql :: Text
venueSqlSearchEnabledSql =
  "SELECT enabled FROM revenue_feature_flag WHERE flag_key = ? AND environment = ?"

-- | True only when the migration is installed and the flag row is enabled for
-- the environment.
venueSqlSearchEnabled :: MonadIO m => Text -> SqlPersistT m Bool
venueSqlSearchEnabled environment = do
  installed <- rawSql venueSqlSearchInstalledSql []
  if installed /= [Single True]
    then pure False
    else do
      rows <- rawSql venueSqlSearchEnabledSql
        [PersistText venueSqlSearchFlag, PersistText environment]
      pure (rows == [Single True])
