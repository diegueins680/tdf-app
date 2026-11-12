{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.Directory
  ( directoryPublicServer
  , directoryProtectedServer
  , consumeRate
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, Value(..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii, isControl)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import System.Environment (lookupEnv)
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Data.Scientific (base10Exponent, toBoundedInteger, toRealFloat)
import Servant

import TDF.API.Directory
import TDF.Auth (AuthedUser(..), ModuleAccess(..), hasModuleAccess)
import qualified TDF.CMS.Models as CMS
import TDF.DB (Env(..))
import TDF.Directory.Policy
import TDF.Models (RoleEnum(Admin))
import qualified TDF.Models.SocialEventsModels as Social

type AppM = ReaderT Env Handler

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

jsonRows :: Text -> [PersistValue] -> AppM [Value]
jsonRows statement params = do
  rows <- runDB (rawSql statement params :: SqlPersistT IO [Single CMS.AesonValue])
  pure [CMS.unAesonValue value | Single value <- rows]

jsonOne :: ServerError -> Text -> [PersistValue] -> AppM Value
jsonOne missing statement params = do
  rows <- jsonRows statement params
  maybe (throwError missing) pure (listToMaybe rows)

optionalUuid :: Maybe UUID -> PersistValue
optionalUuid = maybe PersistNull toPersistValue

optionalText :: Maybe Text -> PersistValue
optionalText = maybe PersistNull PersistText

optionalDouble :: Maybe Double -> PersistValue
optionalDouble = maybe PersistNull PersistDouble

optionalTime :: Maybe UTCTime -> PersistValue
optionalTime = maybe PersistNull toPersistValue

optionalInt64 :: Maybe Int64 -> PersistValue
optionalInt64 = maybe PersistNull PersistInt64

directoryPublicServer :: ServerT DirectoryPublicAPI AppM
directoryPublicServer =
       searchDirectory
  :<|> suggestDirectory
  :<|> directoryTaxonomies
  :<|> publicProfile
  :<|> publicProfileByParty
  :<|> publicProfileReviews
  :<|> publicClassified
  :<|> publicEvent
  :<|> publicVenue

searchDirectory mQuery mEntityType mCityId mLatitude mLongitude mRadiusKm
  mProfessionId mServiceId mInstrumentId mGenreId mRemote mAvailable
  mDateFrom mDateTo mCursor mLimit mProfileKind mIntent = do
  query <- validateQuery mQuery
  entityType <- validateEntityType mEntityType
  let profileKind = normalizeOptional mProfileKind
      intent = normalizeOptional mIntent
  when (maybe False (`notElem` ["person","artist","band","project"]) profileKind) $
    throwError err400 { errBody = "unsupported profileKind" }
  when (maybe False (`notElem` ["seeking","offering"]) intent) $
    throwError err400 { errBody = "unsupported intent" }
  (latitude, longitude, radiusKm) <- validateGeo mLatitude mLongitude mRadiusKm
  let limit = min 50 (max 1 (fromMaybe 20 mLimit))
      cursor = normalizeOptional mCursor
      commonParams =
        [ PersistText query, optionalText entityType, optionalUuid mCityId
        , optionalDouble latitude, optionalDouble longitude, optionalDouble radiusKm
        , optionalUuid mProfessionId, optionalUuid mServiceId
        , optionalUuid mInstrumentId, optionalUuid mGenreId
        , maybe PersistNull PersistBool mRemote, maybe PersistNull PersistBool mAvailable
        , optionalTime mDateFrom, optionalTime mDateTo, optionalText profileKind
        , optionalText intent
        ]
      eventVisibilityParams = [PersistText Social.externalEventRefSuppressedStatus]
  ranked <- jsonRows (searchSql False) (commonParams <> eventVisibilityParams <> [optionalText cursor, PersistInt64 (fromIntegral (limit + 1))])
  sponsored <- jsonRows (searchSql True) (commonParams <> eventVisibilityParams <> [PersistNull, PersistInt64 3])
  facetRows <- jsonRows facetsSql (commonParams <> eventVisibilityParams)
  let visible = take limit ranked
      next = if length ranked > limit then valueCursor (last visible) else Nothing
      facetValue = fromMaybe (object ["entityTypes" .= object [], "cities" .= ([] :: [Value])]) (listToMaybe facetRows)
  recordSearchAnalytics query entityType mCityId mProfessionId mServiceId mInstrumentId mGenreId mRemote mAvailable (length visible)
  pure DirectorySearchResponse
    { items = visible
    , sponsoredItems = sponsored
    , facets = facetValue
    , nextCursor = next
    }

validateQuery :: Maybe Text -> AppM Text
validateQuery raw = do
  let value = maybe "" T.strip raw
  when (T.length value > 160 || T.any isControl value) $
    throwError err400 { errBody = "q must be at most 160 safe characters" }
  pure value

validateEntityType :: Maybe Text -> AppM (Maybe Text)
validateEntityType raw =
  case normalizeOptional raw of
    Nothing -> pure Nothing
    Just value | value `Set.member` Set.fromList ["profile","artist","classified","event","venue"] -> pure (Just value)
    _ -> throwError err400 { errBody = "unsupported entityType" }

validateGeo :: Maybe Double -> Maybe Double -> Maybe Double -> AppM (Maybe Double,Maybe Double,Maybe Double)
validateGeo latitude longitude radius = do
  when ((latitude == Nothing) /= (longitude == Nothing)) $
    throwError err400 { errBody = "latitude and longitude must be supplied together" }
  when (maybe False (\value -> value < -90 || value > 90) latitude) $
    throwError err400 { errBody = "invalid latitude" }
  when (maybe False (\value -> value < -180 || value > 180) longitude) $
    throwError err400 { errBody = "invalid longitude" }
  when (maybe False (\value -> value <= 0 || value > 500) radius) $
    throwError err400 { errBody = "radiusKm must be greater than 0 and no more than 500" }
  when (radius /= Nothing && latitude == Nothing) $
    throwError err400 { errBody = "radiusKm requires consented coordinates" }
  pure (latitude,longitude,radius)

normalizeOptional :: Maybe Text -> Maybe Text
normalizeOptional value = case T.strip <$> value of
  Just trimmed | not (T.null trimmed) -> Just trimmed
  _ -> Nothing

directoryVisibleEventDocumentClause :: Text
directoryVisibleEventDocumentClause =
  "AND (document.entity_kind<>'event' OR NOT EXISTS (SELECT 1 FROM external_event_ref event_ref WHERE event_ref.event_id::text=document.entity_id AND lower(trim(event_ref.source_status))=?)) "

-- The derivation link lives on the materialized row (the public view keeps its
-- historical column list); read it through the primary key.
directorySourceProfileSql :: Text
directorySourceProfileSql =
  "(SELECT raw.source_profile_id FROM directory_search_document raw WHERE raw.entity_kind=document.entity_kind AND raw.entity_id=document.entity_id)"

-- Both discovery surfaces use this projection. Directory identities win even when
-- private, draft, moderated or merged; fallback must never re-publish them.
searchDocumentsSql :: Text
searchDocumentsSql =
  "documents AS (SELECT document.*,profile.profile_kind,('/directorio/'||document.slug)::text canonical_path FROM directory_public_search_document document LEFT JOIN directory_profile profile ON document.entity_kind='profile' AND profile.id::text=document.entity_id WHERE document.entity_kind<>'profile' OR (profile.profile_status='published' AND profile.visibility='public' AND profile.moderation_status='allowed' AND profile.canonical_profile_id IS NULL) " <>
  "UNION ALL SELECT 'profile','artist-'||artist.artist_party_id::text,artist.artist_party_id::text,party.display_name,NULL,artist.bio,artist.hero_image_url,city.id,artist.city,artist.country_code,NULL::float8,NULL::float8,'city'," <>
  "'{}'::uuid[],'{}'::uuid[],'{}'::uuid[],ARRAY(SELECT genre_id FROM artist_profile_genre_membership WHERE artist_party_id=artist.artist_party_id)," <>
  "text.content,to_tsvector('simple',text.content),0::numeric,0::numeric,0::numeric,artist.created_at,NULL::timestamptz,coalesce(artist.updated_at,artist.created_at),1::bigint,FALSE,NULL,FALSE,FALSE,FALSE,'artist','/artista/'||artist.artist_party_id::text " <>
  "FROM artist_profile artist JOIN party ON party.id=artist.artist_party_id " <>
  "LEFT JOIN LATERAL (SELECT (array_agg(id))[1] id FROM city_reference WHERE active AND directory_normalize_text(name_es)=directory_normalize_text(artist.city) HAVING count(*)=1) city ON TRUE " <>
  "CROSS JOIN LATERAL (SELECT directory_normalize_text(concat_ws(' ',party.display_name,artist.slug,artist.bio,artist.city,artist.country_code,artist.genres,artist.highlights)) content) text " <>
  "WHERE ((SELECT q FROM input)='' OR artist.artist_party_id IN (" <>
  "SELECT id FROM party WHERE directory_normalize_text(display_name) LIKE '%'||directory_normalize_text((SELECT q FROM input))||'%' " <>
  "UNION SELECT artist_party_id FROM artist_profile WHERE directory_normalize_text(coalesce(slug,'')||' '||coalesce(bio,'')||' '||coalesce(city,'')||' '||coalesce(country_code,'')||' '||coalesce(genres,'')||' '||coalesce(highlights,'')) LIKE '%'||directory_normalize_text((SELECT q FROM input))||'%')) " <>
  "AND NOT EXISTS (SELECT 1 FROM directory_profile profile WHERE profile.subject_party_id=artist.artist_party_id) " <>
  "AND NOT EXISTS (SELECT 1 FROM directory_legacy_link link WHERE link.legacy_kind='artist_profile' AND link.legacy_id=artist.id::text)), "

-- The filters every search query shares, after the entity-type and derived
-- listing predicates, which the facet query applies differently. The
-- profession, service, instrument, availability and remote filters describe
-- people and listings; an event or a venue neither carries nor is excluded by
-- them, so "Eventos" with a profession left selected still lists events. The
-- genre filter applies to events as well (their genres are mapped to catalog
-- ids), and the city, text, radius and date filters apply to every kind.
directoryFilterClause :: Text
directoryFilterClause =
  directoryVisibleEventDocumentClause <>
  "AND directory_matches_non_geo_filters(document.entity_kind,document.profile_kind,document.city_id," <>
  "document.profession_ids,document.service_ids,document.instrument_ids,document.genre_ids," <>
  "document.search_text,document.search_vector,document.remote,document.availability_score," <>
  "document.effective_at,document.expires_at,input.q,(SELECT alias_entity_ids FROM directory_query_aliases),input.profile_kind," <>
  "input.city_id,input.profession_id,input.service_id,input.instrument_id,input.genre_id," <>
  "input.remote_only,input.available_only,input.date_from,input.date_to) " <>
  "AND (input.latitude IS NULL OR input.radius_km IS NULL OR directory_distance_km(input.latitude,input.longitude,document.public_latitude,document.public_longitude)<=input.radius_km) " <>
  -- ADR 0134: an intent filter only ever selects classifieds, by their
  -- category intent (busco / ofrezco).
  "AND (input.intent IS NULL OR (document.entity_kind='classified' AND EXISTS (SELECT 1 FROM classified intent_classified JOIN classified_category intent_category ON intent_category.id=intent_classified.category_id WHERE intent_classified.id::text=document.entity_id AND intent_category.intent=input.intent))) "

directoryEntityTypeClause :: Text
directoryEntityTypeClause =
  "AND directory_matches_entity_and_derivation(document.entity_kind,document.profile_kind," <>
  directorySourceProfileSql <> ",input.entity_type) "

-- Typed query parameters and alias resolution are shared by ranking and facets.
-- Materialize only aliases: typed inputs remain foldable for index conditions.
directorySearchInputSql :: Text
directorySearchInputSql =
  "WITH input AS (SELECT ?::text q,?::text entity_type,?::uuid city_id,?::float8 latitude,?::float8 longitude,?::float8 radius_km,?::uuid profession_id,?::uuid service_id,?::uuid instrument_id,?::uuid genre_id,?::boolean remote_only,?::boolean available_only,?::timestamptz date_from,?::timestamptz date_to,?::text profile_kind,?::text intent), " <>
  "directory_query_aliases AS MATERIALIZED (SELECT directory_query_alias_entity_ids((SELECT q FROM input)) alias_entity_ids), "

searchSql :: Bool -> Text
searchSql sponsoredOnly =
  directorySearchInputSql <>
  searchDocumentsSql <>
  "scored AS (SELECT document.*," <> directorySourceProfileSql <> " source_profile_id,directory_distance_km(input.latitude,input.longitude,document.public_latitude,document.public_longitude) distance_km," <>
  "CASE WHEN input.q='' THEN .5 WHEN directory_normalize_text(document.title)=directory_normalize_text(input.q) THEN 10 WHEN directory_normalize_text(document.title) LIKE directory_normalize_text(input.q)||'%' THEN 7 WHEN position(directory_normalize_text(input.q) in directory_normalize_text(document.title))>0 THEN 4 ELSE least(1,greatest(ts_rank_cd(document.search_vector,plainto_tsquery('simple',directory_normalize_text(input.q))),directory_text_similarity(document.title,input.q))) END text_score," <>
  "CASE WHEN input.q='' THEN 0 WHEN EXISTS (SELECT 1 FROM catalog_search_alias alias WHERE alias.entity_id=ANY(document.profession_ids||document.service_ids||document.instrument_ids||document.genre_ids) AND (alias.normalized_term LIKE directory_normalize_text(input.q)||'%' OR directory_normalize_text(input.q) LIKE alias.normalized_term||'%')) THEN 1 ELSE 0 END semantic_score " <>
  "FROM documents document CROSS JOIN input WHERE document.sponsored=" <> (if sponsoredOnly then "TRUE " else "FALSE ") <>
  directoryEntityTypeClause <>
  directoryFilterClause <> "), " <>
  "ranked AS (SELECT scored.*,round(((.40*text_score+.15*semantic_score+.15*CASE WHEN distance_km IS NULL THEN .35 ELSE 1/(1+distance_km/25) END+.10*profile_completeness+.08*least(1,1/(1+extract(epoch from (now()-source_updated_at))/2592000))+.05*availability_score+.07*reputation_score)/5)::numeric,6) organic_score FROM scored), " <>
  "boundary AS (SELECT organic_score,source_updated_at,entity_kind,entity_id FROM ranked WHERE entity_kind||':'||entity_id=?::text), " <>
  "page AS (SELECT ranked.* FROM ranked WHERE NOT EXISTS (SELECT 1 FROM boundary) OR EXISTS (SELECT 1 FROM boundary b WHERE ranked.organic_score<b.organic_score OR (ranked.organic_score=b.organic_score AND (ranked.source_updated_at<b.source_updated_at OR (ranked.source_updated_at=b.source_updated_at AND (ranked.entity_kind>b.entity_kind OR (ranked.entity_kind=b.entity_kind AND ranked.entity_id>b.entity_id)))))) ORDER BY organic_score DESC,source_updated_at DESC,entity_kind,entity_id LIMIT ?) " <>
  "SELECT jsonb_build_object('id',entity_id,'type',entity_kind,'slug',slug,'title',title,'subtitle',subtitle,'summary',summary,'profileKind',profile_kind,'canonicalPath',CASE WHEN entity_kind='profile' THEN canonical_path ELSE NULL END,'imageUrl',image_url,'location',jsonb_build_object('cityId',city_id,'city',city_name,'countryCode',country_code,'latitude',public_latitude,'longitude',public_longitude,'precision',location_precision,'distanceKm',CASE WHEN distance_km IS NULL THEN NULL ELSE round(distance_km::numeric,1) END),'modality',jsonb_build_object('onsite',onsite,'remote',remote,'travel',available_to_travel),'taxonomy',jsonb_build_object('professionIds',profession_ids,'serviceIds',service_ids,'instrumentIds',instrument_ids,'genreIds',genre_ids),'score',organic_score,'scoreBreakdown',jsonb_build_object('text',round(text_score::numeric,4),'taxonomy',semantic_score,'proximity',CASE WHEN distance_km IS NULL THEN NULL ELSE round((1/(1+distance_km/25))::numeric,4) END,'quality',profile_completeness,'activityWeight',.08,'availability',availability_score,'reputation',reputation_score),'sponsored',sponsored,'sponsorDisclosure',sponsor_disclosure,'effectiveAt',effective_at,'expiresAt',expires_at,'sourceProfile',CASE WHEN source_profile_id IS NULL THEN NULL ELSE (SELECT jsonb_build_object('id',profile.id,'slug',profile.slug,'name',profile.public_name,'canonicalUrl','/directorio/'||profile.slug) FROM directory_public_profile profile WHERE profile.id=page.source_profile_id) END,'intent',CASE WHEN entity_kind='classified' THEN (SELECT intent_category.intent FROM classified intent_classified JOIN classified_category intent_category ON intent_category.id=intent_classified.category_id WHERE intent_classified.id::text=page.entity_id) ELSE NULL END,'cursor',entity_kind||':'||entity_id) FROM page"

-- `total` and `cities` describe the current query (its entity type included),
-- so "Resultados orgánicos · N" is the full match count, not the page size.
-- `entityTypes` counts every kind under the same filters but without the
-- entity type, each as its own tab would list it (a derived artist listing
-- counts under "classified"), plus `all` for the mixed tab, where derived
-- listings are folded into their profile. The tab counts therefore do not
-- vanish once a tab is selected.
facetsSql :: Text
facetsSql =
  directorySearchInputSql <>
  searchDocumentsSql <>
  "filtered AS (SELECT document.entity_kind,document.profile_kind,document.city_id,document.city_name," <> directorySourceProfileSql <> " source_profile_id FROM documents document CROSS JOIN input WHERE NOT document.sponsored " <>
  directoryFilterClause <> "), " <>
  "selected AS (SELECT filtered.* FROM filtered CROSS JOIN input WHERE directory_matches_entity_and_derivation(filtered.entity_kind,filtered.profile_kind,filtered.source_profile_id,input.entity_type)) " <>
  "SELECT jsonb_build_object('entityTypes',coalesce((SELECT jsonb_object_agg(entity_kind,total) FROM (SELECT entity_kind,count(*) total FROM filtered GROUP BY entity_kind) counts),'{}'::jsonb)||jsonb_build_object('all',(SELECT count(*) FROM filtered WHERE source_profile_id IS NULL),'artist',(SELECT count(*) FROM filtered WHERE profile_kind IN ('artist','band','project')))," <>
  "'cities',coalesce((SELECT jsonb_agg(jsonb_build_object('id',city_id,'name',city_name,'count',total) ORDER BY total DESC,city_name) FROM (SELECT city_id,max(city_name) city_name,count(*) total FROM selected WHERE city_id IS NOT NULL GROUP BY city_id ORDER BY total DESC LIMIT 20) cities),'[]'::jsonb)," <>
  "'total',(SELECT count(*) FROM selected))"

valueCursor :: Value -> Maybe Text
valueCursor (Object value) = case KeyMap.lookup (Key.fromText "cursor") value of
  Just (String cursor) -> Just cursor
  _ -> Nothing
valueCursor _ = Nothing

valueUuid :: Text -> Value -> Maybe UUID
valueUuid key (Object value) = case KeyMap.lookup (Key.fromText key) value of
  Just (String identifier) -> UUID.fromText identifier
  _ -> Nothing
valueUuid _ _ = Nothing

suggestDirectory mQuery mCityId = do
  query <- validateQuery mQuery
  rows <- jsonRows
    ( "SELECT jsonb_build_object('label',label,'canonicalQuery',canonical_query,'suggestionKind',kind,'entityId',entity_id) FROM (SELECT alias.term label,alias.normalized_term canonical_query,'taxonomy'::text kind,alias.entity_id::text entity_id,1 priority FROM catalog_search_alias alias JOIN catalog_definition catalog ON catalog.id=alias.catalog_id WHERE catalog.public_read AND alias.normalized_term LIKE directory_normalize_text(?)||'%' UNION ALL SELECT document.title,document.title,document.entity_kind,document.entity_id,2 FROM directory_public_search_document document WHERE NOT document.sponsored "
        <> directoryVisibleEventDocumentClause
        -- A derived listing repeats its profile's title; suggest the profile only.
        <> "AND " <> directorySourceProfileSql <> " IS NULL "
        <> "AND (?::uuid IS NULL OR document.city_id=?::uuid) AND document.search_text LIKE directory_normalize_text(?)||'%' ) suggestion ORDER BY priority,label LIMIT 10"
    )
    [ PersistText query
    , PersistText Social.externalEventRefSuppressedStatus
    , optionalUuid mCityId
    , optionalUuid mCityId
    , PersistText query
    ]
  pure [DirectorySuggestion l cq sk eid | Object item <- rows,
    Just (String l) <- [KeyMap.lookup "label" item],Just (String cq) <- [KeyMap.lookup "canonicalQuery" item],
    Just (String sk) <- [KeyMap.lookup "suggestionKind" item],
    let eid=case KeyMap.lookup "entityId" item of Just (String value)->Just value; _->Nothing]

directoryTaxonomies mLocale =
  jsonOne err500
    ( "WITH requested AS (SELECT CASE WHEN ?::text IN ('es','en','pt') "
   <> "THEN ?::text ELSE 'es' END locale) SELECT jsonb_build_object("
   <> "'locale',requested.locale,"
   <> "'professions',(SELECT coalesce(jsonb_agg(jsonb_build_object("
   <> "'id',item.id,'code',item.code,'slug',item.current_slug,"
   <> "'name',coalesce(translation.name,CASE WHEN requested.locale='en' "
   <> "THEN item.name_en ELSE item.name_es END),'parentId',item.parent_id) "
   <> "ORDER BY item.sort_order,item.code),'[]'::jsonb) FROM profession item "
   <> "LEFT JOIN catalog_item_translation translation ON translation.entity_id=item.id "
   <> "AND translation.locale_id=(SELECT id FROM locale_reference "
   <> "WHERE code=requested.locale LIMIT 1) WHERE item.active),"
   <> "'classifiedCategories',(SELECT coalesce(jsonb_agg(jsonb_build_object("
   <> "'id',item.id,'code',item.code,'slug',item.current_slug,"
   <> "'name',CASE WHEN requested.locale='en' THEN item.name_en ELSE item.name_es END,"
   <> "'requirements',item.requirements,'derived',item.requirements->>'derivation' IS NOT NULL,'metadata',jsonb_build_object('intent',item.intent)) ORDER BY item.sort_order),'[]'::jsonb) "
   <> "FROM classified_category item WHERE item.active),"
   <> "'compensationTypes',(SELECT coalesce(jsonb_agg(jsonb_build_object("
   <> "'id',item.id,'code',item.code,'slug',item.current_slug,"
   <> "'name',CASE WHEN requested.locale='en' THEN item.name_en ELSE item.name_es END,"
   <> "'metadata',item.metadata) ORDER BY item.sort_order),'[]'::jsonb) "
   <> "FROM compensation_type item WHERE item.active),"
   <> "'serviceOfferings',(SELECT coalesce(jsonb_agg(jsonb_build_object("
   <> "'id',item.id,'code',item.code,'slug',item.current_slug,"
   <> "'name',CASE WHEN requested.locale='en' THEN item.name_en ELSE item.name_es END,"
   <> "'currencyId',item.currency_id) ORDER BY item.sort_order,item.code),'[]'::jsonb) "
   <> "FROM service_offering item WHERE item.active),"
   <> "'currencies',(SELECT coalesce(jsonb_agg(jsonb_build_object("
   <> "'id',item.id,'code',item.code,"
   <> "'name',CASE WHEN requested.locale='en' THEN item.name_en ELSE item.name_es END,"
   <> "'symbol',item.symbol,'minorUnits',item.minor_units) "
   <> "ORDER BY CASE WHEN item.code='USD' THEN 0 ELSE 1 END,item.sort_order,item.code),"
   <> "'[]'::jsonb) FROM currency_reference item WHERE item.active),"
   <> "'languages',(SELECT coalesce(jsonb_agg(jsonb_build_object("
   <> "'id',item.id,'code',coalesce(item.iso6391,item.iso6392_t),"
   <> "'name',CASE WHEN requested.locale='en' THEN item.name_en ELSE item.name_es END) "
   <> "ORDER BY item.sort_order,item.iso6392_t),'[]'::jsonb) "
   <> "FROM language_reference item WHERE item.active),"
   <> "'instruments',(SELECT coalesce(jsonb_agg(jsonb_build_object("
   <> "'id',id,'code',code,'name',CASE WHEN requested.locale='en' "
   <> "THEN name_en ELSE name_es END) ORDER BY sort_order),'[]'::jsonb) "
   <> "FROM instrument WHERE active),"
   <> "'genres',(SELECT coalesce(jsonb_agg(jsonb_build_object("
   <> "'id',id,'code',code,'name',CASE WHEN requested.locale='en' "
   <> "THEN name_en ELSE name_es END) ORDER BY sort_order),'[]'::jsonb) "
   <> "FROM genre WHERE active),"
   <> "'cities',(SELECT coalesce(jsonb_agg(jsonb_build_object("
   <> "'id',id,'code',code,'name',CASE WHEN requested.locale='en' "
   <> "THEN name_en ELSE name_es END,'countryId',country_id,"
   <> "'latitude',latitude,'longitude',longitude) ORDER BY sort_order),"
   <> "'[]'::jsonb) FROM city_reference WHERE active)) FROM requested" )
    [PersistText (fromMaybe "es" mLocale),PersistText (fromMaybe "es" mLocale)]

publicProfile slugValue =
  jsonOne err404
    ( T.replace "'portfolio',profile.portfolio,'links',profile.links" profileRichMediaProjectionSql
    $ "SELECT jsonb_build_object('id',profile.id,'kind',profile.profile_kind,'name',profile.public_name,"
   <> "'slug',profile.slug,'bio',profile.bio,'experience',profile.experience_summary,"
   <> "'creditsSummary',profile.credits_summary,'portfolio',profile.portfolio,'links',profile.links,"
   <> "'equipment',profile.equipment_summary,'rates',CASE WHEN profile.rate_min_minor IS NULL THEN NULL "
   <> "ELSE jsonb_build_object('minMinor',profile.rate_min_minor,'maxMinor',profile.rate_max_minor,'currencyId',profile.currency_id) END,"
   <> "'availability',jsonb_build_object('status',profile.availability_status,'onsite',profile.onsite,'remote',profile.remote,'travel',profile.available_to_travel,'radiusKm',profile.travel_radius_km),"
   <> "'locations',coalesce((SELECT jsonb_agg(jsonb_build_object('cityId',location.city_id,'city',city.name_es,'countryCode',country.alpha2,'sector',location.sector_label,'latitude',location.public_latitude,'longitude',location.public_longitude,'precision',location.precision) ORDER BY location.primary_location DESC,location.created_at,location.id) FROM directory_profile_location location JOIN country_reference country ON country.id=location.country_id LEFT JOIN city_reference city ON city.id=location.city_id WHERE location.profile_id=profile.id),'[]'::jsonb),"
   <> "'professions',coalesce((SELECT jsonb_agg(jsonb_build_object('id',term.id,'code',term.code,'name',term.name_es,'headline',member.headline,'yearsExperience',member.years_experience,'rateMinMinor',member.rate_min_minor,'rateMaxMinor',member.rate_max_minor,'currencyId',member.currency_id) ORDER BY member.sort_order) FROM directory_profile_profession member JOIN profession term ON term.id=member.profession_id WHERE member.profile_id=profile.id),'[]'::jsonb),"
   <> "'instruments',coalesce((SELECT jsonb_agg(jsonb_build_object('id',term.id,'code',term.code,'name',term.name_es,'proficiency',member.proficiency) ORDER BY member.sort_order) FROM directory_profile_instrument member JOIN instrument term ON term.id=member.instrument_id WHERE member.profile_id=profile.id),'[]'::jsonb),"
   <> "'genres',coalesce((SELECT jsonb_agg(jsonb_build_object('id',term.id,'code',term.code,'name',term.name_es) ORDER BY member.sort_order) FROM directory_profile_genre member JOIN genre term ON term.id=member.genre_id WHERE member.profile_id=profile.id),'[]'::jsonb),"
   <> "'services',coalesce((SELECT jsonb_agg(jsonb_build_object('id',term.id,'code',term.code,'name',term.name_es,'bookable',member.bookable) ORDER BY member.sort_order) FROM directory_profile_service member JOIN service_offering term ON term.id=member.service_offering_id WHERE member.profile_id=profile.id),'[]'::jsonb),"
   <> "'languages',coalesce((SELECT jsonb_agg(jsonb_build_object('id',term.id,'code',coalesce(term.iso6391,term.iso6392_t),'name',term.name_es,'proficiency',member.proficiency) ORDER BY term.sort_order,term.iso6392_t) FROM directory_profile_language member JOIN language_reference term ON term.id=member.language_id WHERE member.profile_id=profile.id),'[]'::jsonb),"
   <> "'verification',coalesce((SELECT jsonb_agg(jsonb_build_object('type',verification.verification_type,'status',verification.status,'verifiedAt',verification.verified_at)) FROM directory_verification verification WHERE verification.profile_id=profile.id AND verification.status='verified'),'[]'::jsonb),"
   <> "'reputation',jsonb_build_object('completeness',profile.completeness_score,'responseRate',profile.response_rate,'medianResponseMinutes',profile.median_response_minutes,'completed',profile.completed_interactions,'reviewAverage',profile.review_average,'reviewCount',profile.review_count),"
   <> "'previewImageUrl',directory_profile_preview_image_url(profile.id),'derivedListing',(SELECT jsonb_build_object('id',item.id,'slug',item.slug,'canonicalUrl','/clasificados/'||item.slug) FROM classified item WHERE item.source_profile_id=profile.id AND item.status='published' AND item.moderation_status='allowed'),"
   <> "'canonicalUrl','/directorio/'||profile.slug) FROM directory_public_profile_resolution profile WHERE profile.requested_slug=?" )
    [PersistText (T.toLower (T.strip slugValue))]

-- Artist detail already exposes its Party identifier. This resolver only
-- returns the same safe public profile projection as the slug route and never
-- adds the Party identifier to that projection.
publicProfileByParty :: Int64 -> AppM Value
publicProfileByParty partyId = do
  canonicalSlug <- jsonOne err404
    ( "SELECT to_jsonb(resolved.slug) FROM directory_profile source "
   <> "JOIN directory_public_profile_resolution resolved "
   <> "ON resolved.requested_slug=source.slug "
   <> "WHERE source.subject_party_id=? "
   <> "ORDER BY (source.id=resolved.id) DESC,source.updated_at DESC,source.id LIMIT 1" )
    [PersistInt64 partyId]
  case canonicalSlug of
    String slugValue -> publicProfile slugValue
    _ -> throwError err500

publicProfileReviews slugValue mCursor mLimit = do
  let limit = min 50 (max 1 (fromMaybe 20 mLimit))
      slug = T.toLower (T.strip slugValue)
  summary <- jsonOne err404
    "SELECT jsonb_build_object('profileId',profile.id,'average',profile.review_average,'count',profile.review_count) FROM directory_public_profile_resolution profile WHERE profile.requested_slug=?"
    [PersistText slug]
  ranked <- jsonRows
    ( "WITH resolved AS (SELECT id FROM directory_public_profile_resolution WHERE requested_slug=?), "
   <> "review_rows AS (SELECT review.id,review.rating,review.body,review.created_at,interaction.interaction_kind,"
   <> "author.id author_id,author.public_name author_name,author.slug author_slug "
   <> "FROM directory_review review JOIN directory_interaction interaction ON interaction.id=review.interaction_id "
   <> "JOIN directory_profile raw_subject ON raw_subject.id=review.subject_profile_id "
   <> "JOIN directory_profile raw_author ON raw_author.id=review.author_profile_id "
   <> "JOIN directory_public_profile author ON author.id=coalesce(raw_author.canonical_profile_id,raw_author.id) "
   <> "CROSS JOIN resolved WHERE coalesce(raw_subject.canonical_profile_id,raw_subject.id)=resolved.id "
   <> "AND author.id<>resolved.id AND review.status='published' AND interaction.status='completed' AND interaction.verified_at IS NOT NULL "
   <> "AND ((interaction.profile_a_id=review.author_profile_id AND interaction.profile_b_id=review.subject_profile_id) OR (interaction.profile_b_id=review.author_profile_id AND interaction.profile_a_id=review.subject_profile_id))), "
   <> "boundary AS (SELECT review.created_at,review.id FROM directory_review review "
   <> "JOIN directory_profile boundary_subject ON boundary_subject.id=review.subject_profile_id CROSS JOIN resolved "
   <> "WHERE review.id=?::uuid AND coalesce(boundary_subject.canonical_profile_id,boundary_subject.id)=resolved.id) "
   <> "SELECT jsonb_build_object('id',review.id,'rating',review.rating,'body',review.body,'createdAt',review.created_at,"
   <> "'verifiedInteractionType',review.interaction_kind,'authorProfile',jsonb_build_object('id',review.author_id,'name',review.author_name,'slug',review.author_slug)) "
   <> "FROM review_rows review WHERE ?::uuid IS NULL OR EXISTS (SELECT 1 FROM boundary WHERE (review.created_at,review.id)<(boundary.created_at,boundary.id)) "
   <> "ORDER BY review.created_at DESC,review.id DESC LIMIT ?" )
    [PersistText slug,optionalUuid mCursor,optionalUuid mCursor,PersistInt64 (fromIntegral (limit+1))]
  let visible = take limit ranked
      next = if length ranked>limit then valueUuid "id" (last visible) else Nothing
  pure DirectoryReviewPage {summary,items=visible,nextCursor=next}

publicClassified slugValue =
  jsonOne err404
    "SELECT jsonb_build_object('id',classified.id,'title',classified.title,'slug',classified.slug,'description',classified.description,'category',jsonb_build_object('id',category.id,'code',category.code,'name',category.name_es),'author',jsonb_build_object('id',profile.id,'name',profile.public_name,'slug',profile.slug),'modality',jsonb_build_object('onsite',classified.onsite,'remote',classified.remote,'travel',classified.available_to_travel),'locations',coalesce((SELECT jsonb_agg(jsonb_build_object('cityId',location.city_id,'city',city.name_es,'countryCode',country.alpha2,'metroId',location.metropolitan_area_id,'radiusKm',location.service_radius_km)) FROM classified_location location JOIN country_reference country ON country.id=location.country_id LEFT JOIN city_reference city ON city.id=location.city_id WHERE location.classified_id=classified.id),'[]'::jsonb),'compensation',CASE WHEN classified.compensation_type_id IS NULL THEN NULL ELSE jsonb_build_object('typeId',classified.compensation_type_id,'minMinor',classified.budget_min_minor,'maxMinor',classified.budget_max_minor,'currencyId',classified.currency_id,'negotiable',classified.budget_negotiable) END,'startsAt',classified.starts_at,'endsAt',classified.ends_at,'expiresAt',NULLIF(classified.expires_at,'infinity'::timestamptz),'intent',category.intent,'priceMode',classified.price_mode,'slots',classified.slots,'slotsRemaining',classified.slots_remaining,'offerDeadlineAt',classified.offer_deadline_at,'terms',classified.terms,'offerStats',(SELECT jsonb_build_object('count',count(*),'minMinor',CASE WHEN count(*)>=3 THEN min(application.current_amount_minor) END,'maxMinor',CASE WHEN count(*)>=3 THEN max(application.current_amount_minor) END,'currencyId',CASE WHEN count(*)>=3 AND count(DISTINCT application.currency_id)=1 THEN min(application.currency_id::text)::uuid END) FROM classified_application application WHERE application.classified_id=classified.id AND application.status NOT IN ('withdrawn','expired')),'imageUrl',CASE WHEN classified.source_profile_id IS NOT NULL THEN directory_profile_preview_image_url(classified.source_profile_id) ELSE (SELECT document.image_url FROM directory_search_document document WHERE document.entity_kind='classified' AND document.entity_id=classified.id::text) END,'sourceProfile',CASE WHEN classified.source_profile_id IS NULL THEN NULL ELSE jsonb_build_object('id',profile.id,'name',profile.public_name,'slug',profile.slug,'kind',profile.profile_kind,'canonicalUrl','/directorio/'||profile.slug) END,'canonicalUrl','/clasificados/'||classified.slug) FROM classified JOIN classified_category category ON category.id=classified.category_id JOIN directory_public_profile profile ON profile.id=classified.author_profile_id WHERE classified.slug=? AND classified.status='published' AND classified.moderation_status='allowed' AND classified.expires_at>now()"
    [PersistText (T.toLower (T.strip slugValue))]

publicEvent eventId = jsonOne err404
  "SELECT jsonb_build_object('id',public_event.id,'title',public_event.title,'description',public_event.description,'startTime',public_event.start_time,'endTime',public_event.end_time,'timezone',public_event.timezone,'priceCents',public_event.price_cents,'currencyId',public_event.currency_id,'capacity',public_event.capacity,'imageUrl',public_event.image_url,'isPublic',public_event.is_public,'workflowStateCode',public_event.workflow_state_code,'rsvpEligible',public_event.rsvp_eligible,'publicShareEligible',public_event.public_share_eligible,'rsvpSummary',jsonb_build_object('acceptedCount',(SELECT count(*) FROM event_rsvp WHERE event_id=public_event.id AND status='accepted'),'maybeCount',(SELECT count(*) FROM event_rsvp WHERE event_id=public_event.id AND status='maybe')),'venue',CASE WHEN public_event.venue_id IS NULL THEN NULL ELSE jsonb_build_object('id',public_event.venue_id,'name',public_event.venue_name) END,'location',jsonb_build_object('cityId',public_event.city_id,'city',public_event.city_name,'countryCode',public_event.country_code,'latitude',public_event.public_latitude,'longitude',public_event.public_longitude,'precision','city'),'canonicalUrl','/eventos/'||public_event.id::text) FROM directory_public_rsvp_event public_event WHERE public_event.id=? AND NOT EXISTS (SELECT 1 FROM external_event_ref event_ref WHERE event_ref.event_id=public_event.id AND lower(trim(event_ref.source_status))=?)"
  [PersistInt64 eventId, PersistText Social.externalEventRefSuppressedStatus]

publicVenue venueId = jsonOne err404
  "SELECT jsonb_build_object('id',id,'name',name,'capacity',capacity,'location',jsonb_build_object('cityId',city_id,'city',city_name,'countryCode',country_code,'latitude',public_latitude,'longitude',public_longitude,'precision','city'),'imageUrl',(SELECT directory_safe_image_url(directory_try_jsonb_object(source.contact)->>'imageUrl') FROM venue source WHERE source.id=directory_public_venue.id),'canonicalUrl','/venues/'||id::text) FROM directory_public_venue WHERE id=?" [PersistInt64 venueId]

directoryProtectedServer :: AuthedUser -> ServerT DirectoryProtectedAPI AppM
directoryProtectedServer user =
       notificationContext user
  :<|> setAgeAssurance user
  :<|> listManagedProfiles user
  :<|> createProfile user
  :<|> updateProfile user
  :<|> changeProfileStatus user
  :<|> listManagedClassifieds user
  :<|> createClassified user
  :<|> changeClassifiedStatus user
  :<|> listApplications user
  :<|> createApplication user
  :<|> listMyApplications user
  :<|> getApplication user
  :<|> counterOffer user
  :<|> changeApplicationStatus user
  :<|> listInvitations user
  :<|> createInvitation user
  :<|> changeInvitationStatus user
  :<|> contactProfile user
  :<|> listReviewEligibility user
  :<|> createReview user
  :<|> listFavorites user
  :<|> addFavorite user
  :<|> removeFavorite user
  :<|> listSavedSearches user
  :<|> createSavedSearch user
  :<|> prepareArtistClaim user
  :<|> createClaim user
  :<|> createVerification user
  :<|> createReport user
  :<|> listAdminClaims user
  :<|> changeClaimStatus user
  :<|> listAdminVerifications user
  :<|> changeVerificationStatus user
  :<|> listModerationQueue user
  :<|> createModerationDecision user
  :<|> mergeProfiles user

setAgeAssurance user AgeAssuranceRequest{adultAttestation,guardianPartyId} = do
  when (not adultAttestation && guardianPartyId == Nothing) $
    throwError err400 { errBody = "guardianPartyId is required for a minor account" }
  when (guardianPartyId == Just (partyNumber user)) $
    throwError err400 { errBody = "guardian must be a different Party" }
  runDB $ rawExecute
    "INSERT INTO directory_age_assurance(account_party_id,assurance_status,guardian_party_id,guardian_consent_status,updated_at) VALUES (?,CASE WHEN ? THEN 'adult_attested' ELSE 'guardian_pending' END,?,CASE WHEN ? THEN NULL ELSE 'pending' END,now()) ON CONFLICT(account_party_id) DO UPDATE SET assurance_status=EXCLUDED.assurance_status,guardian_party_id=EXCLUDED.guardian_party_id,guardian_consent_status=EXCLUDED.guardian_consent_status,evidence_reference=NULL,verified_at=NULL,updated_at=now()"
    [toPersistValue (auPartyId user),PersistBool adultAttestation,optionalInt64 guardianPartyId,PersistBool adultAttestation]
  jsonOne err500 "SELECT jsonb_build_object('status',assurance_status,'guardianConsentStatus',guardian_consent_status) FROM directory_age_assurance WHERE account_party_id=?" [toPersistValue (auPartyId user)]

partyNumber :: AuthedUser -> Int64
partyNumber = fromSqlKey . auPartyId

safeStoredProfileUrlSql :: Text -> Text
safeStoredProfileUrlSql expression =
  "(strpos(" <> expression <> ",chr(92))=0 AND ((" <> expression <> " ~* '^https{0,1}://[^[:space:][:cntrl:]]+$' AND split_part(" <> expression <> ",'/',3) NOT LIKE '%@%') OR "
    <> expression <> " ~ '^/[^/[:space:][:cntrl:]][^[:space:][:cntrl:]]*$'))"

profilePortfolioProjectionSql :: Text
profilePortfolioProjectionSql =
  "(SELECT coalesce(jsonb_agg(jsonb_strip_nulls(jsonb_build_object("
    <> "'itemType',CASE WHEN coalesce(entry.value->>'itemType',entry.value->>'kind') IN ('audio','video','image','release','credit','document','other') THEN coalesce(entry.value->>'itemType',entry.value->>'kind') ELSE 'other' END,"
    <> "'title',left(coalesce(nullif(entry.value->>'title',''),nullif(initcap(replace(entry.value->>'kind','-',' ')),''),'Portfolio'),160),"
    <> "'url',entry.value->>'url','description',left(entry.value->>'description',1000),"
    <> "'thumbnailUrl',CASE WHEN " <> safeStoredProfileUrlSql "entry.value->>'thumbnailUrl'" <> " THEN entry.value->>'thumbnailUrl' END"
    <> ")) ORDER BY entry.ordinality),'[]'::jsonb) FROM jsonb_array_elements(CASE WHEN jsonb_typeof(profile.portfolio)='array' THEN profile.portfolio ELSE '[]'::jsonb END) WITH ORDINALITY entry(value,ordinality) "
    <> "WHERE jsonb_typeof(entry.value)='object' AND " <> safeStoredProfileUrlSql "entry.value->>'url'" <> ")"

profileLinksProjectionSql :: Text
profileLinksProjectionSql =
  "(SELECT coalesce(jsonb_agg(jsonb_build_object("
    <> "'label',left(coalesce(nullif(entry.value->>'label',''),nullif(initcap(replace(entry.value->>'kind','-',' ')),''),'Link'),80),"
    <> "'url',entry.value->>'url') ORDER BY entry.ordinality),'[]'::jsonb) "
    <> "FROM jsonb_array_elements(CASE WHEN jsonb_typeof(profile.links)='array' THEN profile.links ELSE '[]'::jsonb END) WITH ORDINALITY entry(value,ordinality) "
    <> "WHERE jsonb_typeof(entry.value)='object' AND " <> safeStoredProfileUrlSql "entry.value->>'url'" <> ")"

profileRichMediaProjectionSql :: Text
profileRichMediaProjectionSql =
  "'portfolio'," <> profilePortfolioProjectionSql <> ",'links'," <> profileLinksProjectionSql

listManagedProfiles user = jsonRows managedProfileSql [toPersistValue (auPartyId user)]

managedProfileSql =
  T.replace "'portfolio',profile.portfolio,'links',profile.links" profileRichMediaProjectionSql
    "SELECT jsonb_build_object('id',profile.id,'kind',profile.profile_kind,'name',profile.public_name,'slug',profile.slug,'coverImageUrl',profile.cover_image_url,'previewImageUrl',directory_profile_preview_image_url(profile.id),'derivedListing',(SELECT jsonb_build_object('id',item.id,'slug',item.slug,'status',item.status,'canonicalUrl','/clasificados/'||item.slug) FROM classified item WHERE item.source_profile_id=profile.id),'bio',profile.bio,'experienceSummary',profile.experience_summary,'creditsSummary',profile.credits_summary,'portfolio',profile.portfolio,'links',profile.links,'equipmentSummary',profile.equipment_summary,'rates',CASE WHEN profile.rate_min_minor IS NULL THEN NULL ELSE jsonb_build_object('minMinor',profile.rate_min_minor,'maxMinor',profile.rate_max_minor,'currencyId',profile.currency_id) END,'availabilityStatus',profile.availability_status,'onsite',profile.onsite,'remote',profile.remote,'availableToTravel',profile.available_to_travel,'travelRadiusKm',profile.travel_radius_km,'professionIds',coalesce((SELECT jsonb_agg(member.profession_id ORDER BY member.sort_order) FROM directory_profile_profession member WHERE member.profile_id=profile.id),'[]'::jsonb),'professionDetails',coalesce((SELECT jsonb_agg(jsonb_build_object('professionId',member.profession_id,'headline',member.headline,'yearsExperience',member.years_experience,'rateMinMinor',member.rate_min_minor,'rateMaxMinor',member.rate_max_minor,'currencyId',member.currency_id) ORDER BY member.sort_order) FROM directory_profile_profession member WHERE member.profile_id=profile.id),'[]'::jsonb),'instrumentIds',coalesce((SELECT jsonb_agg(member.instrument_id ORDER BY member.sort_order) FROM directory_profile_instrument member WHERE member.profile_id=profile.id),'[]'::jsonb),'instrumentDetails',coalesce((SELECT jsonb_agg(jsonb_build_object('instrumentId',member.instrument_id,'proficiency',member.proficiency) ORDER BY member.sort_order) FROM directory_profile_instrument member WHERE member.profile_id=profile.id),'[]'::jsonb),'genreIds',coalesce((SELECT jsonb_agg(member.genre_id ORDER BY member.sort_order) FROM directory_profile_genre member WHERE member.profile_id=profile.id),'[]'::jsonb),'serviceOfferingIds',coalesce((SELECT jsonb_agg(member.service_offering_id ORDER BY member.sort_order) FROM directory_profile_service member WHERE member.profile_id=profile.id),'[]'::jsonb),'languages',coalesce((SELECT jsonb_agg(jsonb_build_object('languageId',member.language_id,'proficiency',member.proficiency) ORDER BY member.language_id) FROM directory_profile_language member WHERE member.profile_id=profile.id),'[]'::jsonb),'serviceAreas',coalesce((SELECT jsonb_agg(jsonb_build_object('countryId',location.country_id,'subdivisionId',location.subdivision_id,'cityId',location.city_id,'metropolitanAreaId',location.metropolitan_area_id,'sectorLabel',location.sector_label,'serviceRadiusKm',location.service_radius_km,'primaryLocation',location.primary_location,'onsite',location.onsite) ORDER BY location.primary_location DESC,location.created_at,location.id) FROM directory_profile_location location WHERE location.profile_id=profile.id),'[]'::jsonb),'status',profile.profile_status,'visibility',profile.visibility,'moderationStatus',profile.moderation_status,'version',profile.version,'capabilities',jsonb_build_object('viewPrivate',manager.can_view_private,'edit',manager.can_edit,'publish',manager.can_publish,'contact',manager.can_contact,'manage',manager.can_manage)) AS value FROM directory_profile_manager manager JOIN directory_profile profile ON profile.id=manager.profile_id WHERE manager.account_party_id=? AND manager.active ORDER BY profile.updated_at DESC,profile.id"

createProfile user idempotency request@DirectoryProfileUpsert
  { profileKind, publicName, slug, bio, experienceSummary, creditsSummary, portfolio, links
  , equipmentSummary, rateMinMinor, rateMaxMinor, currencyId, availabilityStatus
  , onsite, remote, availableToTravel, travelRadiusKm, coverImageUrl } = do
  validateProfileRequest request
  profileId <- reserveIdempotency user "profile.create" idempotency request "profile"
  existing <- jsonRows "SELECT jsonb_build_object('id',id,'slug',slug,'status',profile_status,'version',version) FROM directory_profile WHERE id=?" [toPersistValue profileId]
  case existing of
    _:_ -> profileSummary user profileId
    [] -> do
      now <- liftIO getCurrentTime
      locationId <- liftIO nextRandom
      let normalizedPortfolio = map normalizePortfolioItem (fromMaybe [] portfolio)
          normalizedLinks = map normalizeProfileLink (fromMaybe [] links)
      runDB $ do
        subjectPartyId <- if profileKind `Set.member` organizationalProfileKinds
          then do
            created <- rawSql
              "INSERT INTO party(display_name,is_org,created_at) VALUES (?,TRUE,?) RETURNING id"
              [PersistText (T.strip publicName),toPersistValue now] :: SqlPersistT IO [Single Int64]
            case created of
              Single newPartyId:_ -> pure newPartyId
              _ -> liftIO (fail "organization Party insert did not return an id")
          else pure (partyNumber user)
        rawExecute "INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,bio,experience_summary,credits_summary,portfolio,links,equipment_summary,rate_min_minor,rate_max_minor,currency_id,availability_status,onsite,remote,available_to_travel,travel_radius_km,cover_image_url,profile_status,visibility,moderation_status,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?::jsonb,?::jsonb,?,?,?,?,?,?,?,?,?,NULLIF(trim(?::text),''),'draft','public','allowed',?,?)"
          [ toPersistValue profileId,PersistInt64 subjectPartyId,PersistText profileKind
          , PersistText (T.strip publicName),PersistText (T.toLower (T.strip slug))
          , optionalText (cleanOptionalText bio),optionalText (cleanOptionalText experienceSummary)
          , optionalText (cleanOptionalText creditsSummary)
          , PersistText (decodeJsonText (Aeson.toJSON normalizedPortfolio))
          , PersistText (decodeJsonText (Aeson.toJSON normalizedLinks))
          , optionalText (cleanOptionalText equipmentSummary),optionalInt64 rateMinMinor
          , optionalInt64 rateMaxMinor,optionalUuid currencyId
          , PersistText (fromMaybe "ask" availabilityStatus),PersistBool onsite,PersistBool remote
          , PersistBool availableToTravel,maybe PersistNull (PersistDouble . realToFrac) travelRadiusKm,optionalText coverImageUrl
          , toPersistValue now,toPersistValue now]
        rawExecute "INSERT INTO directory_profile_manager(profile_id,account_party_id,can_view_private,can_edit,can_publish,can_contact,can_manage,active,granted_by) VALUES (?,?,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,?)" [toPersistValue profileId,toPersistValue (auPartyId user),toPersistValue (auPartyId user)]
        rawExecute "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,new_state,correlation_id,metadata) VALUES (?,'profile.created','profile',?,'draft',?,jsonb_build_object('subjectPartyId',?::bigint,'profileKind',?::text))"
          [toPersistValue (auPartyId user),PersistText (UUID.toText profileId),PersistText ("profile-created-"<>UUID.toText profileId),PersistInt64 subjectPartyId,PersistText profileKind]
        replaceProfileSelectionsDB locationId profileId request
      profileSummary user profileId

organizationalProfileKinds :: Set.Set Text
organizationalProfileKinds = Set.fromList
  ["band","project","organization","company","venue","studio","agency","label","distributor","school"]

updateProfile user profileId request@DirectoryProfileUpsert
  { profileKind, publicName, slug, bio, experienceSummary, creditsSummary, portfolio, links
  , equipmentSummary, rateMinMinor, rateMaxMinor, currencyId, clearRates, availabilityStatus
  , professionDetails, instrumentDetails, languages, serviceAreas
  , onsite, remote, availableToTravel, travelRadiusKm, coverImageUrl } = do
  requireProfileCapability user profileId "edit"
  validateProfileRequest request
  currentKindValue <- jsonOne err404 "SELECT to_jsonb(profile_kind) FROM directory_profile WHERE id=?" [toPersistValue profileId]
  currentKind <- case currentKindValue of String value -> pure value; _ -> throwError err500
  when ((currentKind `Set.member` organizationalProfileKinds) /= (profileKind `Set.member` organizationalProfileKinds)) $
    throwError err409 {errBody="changing between a personal and organizational subject requires an audited reconciliation"}
  let ratesRequested = fromMaybe False clearRates || any isJust [rateMinMinor,rateMaxMinor] || isJust currencyId
      clearedRates = fromMaybe False clearRates
      nextRateMin = if clearedRates then Nothing else rateMinMinor
      nextRateMax = if clearedRates then Nothing else rateMaxMinor
      nextCurrency = if clearedRates then Nothing else currencyId
      normalizedPortfolio = map normalizePortfolioItem (fromMaybe [] portfolio)
      normalizedLinks = map normalizeProfileLink (fromMaybe [] links)
  locationId <- liftIO nextRandom
  runDB $ do
    rawExecute "UPDATE directory_profile SET profile_kind=?,public_name=?,slug=?,bio=?,experience_summary=CASE WHEN ? THEN NULLIF(trim(?::text),'') ELSE experience_summary END,credits_summary=CASE WHEN ? THEN NULLIF(trim(?::text),'') ELSE credits_summary END,portfolio=CASE WHEN ? THEN ?::jsonb ELSE portfolio END,links=CASE WHEN ? THEN ?::jsonb ELSE links END,equipment_summary=CASE WHEN ? THEN NULLIF(trim(?::text),'') ELSE equipment_summary END,rate_min_minor=CASE WHEN ? THEN ?::bigint ELSE rate_min_minor END,rate_max_minor=CASE WHEN ? THEN ?::bigint ELSE rate_max_minor END,currency_id=CASE WHEN ? THEN ?::uuid ELSE currency_id END,availability_status=CASE WHEN ? THEN ?::text ELSE availability_status END,onsite=?,remote=?,available_to_travel=?,travel_radius_km=?,cover_image_url=CASE WHEN ? THEN NULLIF(trim(?::text),'') ELSE cover_image_url END,updated_at=now(),version=version+1 WHERE id=?"
      [ PersistText profileKind,PersistText (T.strip publicName),PersistText (T.toLower (T.strip slug))
      , optionalText (cleanOptionalText bio)
      , PersistBool (isJust experienceSummary),optionalText (cleanOptionalText experienceSummary)
      , PersistBool (isJust creditsSummary),optionalText (cleanOptionalText creditsSummary)
      , PersistBool (isJust portfolio),PersistText (decodeJsonText (Aeson.toJSON normalizedPortfolio))
      , PersistBool (isJust links),PersistText (decodeJsonText (Aeson.toJSON normalizedLinks))
      , PersistBool (isJust equipmentSummary),optionalText (cleanOptionalText equipmentSummary)
      , PersistBool ratesRequested,optionalInt64 nextRateMin,PersistBool ratesRequested,optionalInt64 nextRateMax
      , PersistBool ratesRequested,optionalUuid nextCurrency
      , PersistBool (isJust availabilityStatus),PersistText (fromMaybe "ask" availabilityStatus)
      , PersistBool onsite,PersistBool remote,PersistBool availableToTravel
      , maybe PersistNull (PersistDouble . realToFrac) travelRadiusKm
      , PersistBool (isJust coverImageUrl),optionalText coverImageUrl,toPersistValue profileId]
    replaceProfileSelectionsDB locationId profileId request
    rawExecute "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,correlation_id,metadata) VALUES (?,'profile.updated','profile',?, ?,jsonb_build_object('profileKind',?::text,'experienceRequested',?::boolean,'creditsRequested',?::boolean,'portfolioRequested',?::boolean,'linksRequested',?::boolean,'equipmentRequested',?::boolean,'ratesRequested',?::boolean,'availabilityRequested',?::boolean,'professionDetailsRequested',?::boolean,'instrumentDetailsRequested',?::boolean,'languagesRequested',?::boolean,'serviceAreasRequested',?::boolean))"
      [ toPersistValue (auPartyId user),PersistText (UUID.toText profileId),PersistText ("profile-update-"<>UUID.toText locationId)
      , PersistText profileKind,PersistBool (isJust experienceSummary),PersistBool (isJust creditsSummary)
      , PersistBool (isJust portfolio),PersistBool (isJust links),PersistBool (isJust equipmentSummary)
      , PersistBool ratesRequested,PersistBool (isJust availabilityStatus),PersistBool (isJust professionDetails)
      , PersistBool (isJust instrumentDetails),PersistBool (isJust languages),PersistBool (isJust serviceAreas)]
  profileSummary user profileId

replaceProfileSelectionsDB _locationId profileId DirectoryProfileUpsert
  { professionIds, professionDetails, instrumentIds, instrumentDetails, genreIds
  , serviceOfferingIds, languages, serviceAreas, countryId, cityId
  , metropolitanAreaId, travelRadiusKm, onsite } = do
    let selectedProfessionIds = maybe professionIds (map professionId) professionDetails
        selectedInstrumentIds = maybe instrumentIds (map instrumentId) instrumentDetails
    rawExecute "DELETE FROM directory_profile_profession WHERE profile_id=? AND profession_id<>ALL(?::uuid[])" [toPersistValue profileId,uuidArrayValue selectedProfessionIds]
    case professionDetails of
      Nothing -> forM_ (zip [0::Int ..] professionIds) $ \(position,itemId) ->
        rawExecute "INSERT INTO directory_profile_profession(profile_id,profession_id,sort_order) VALUES (?,?,?) ON CONFLICT(profile_id,profession_id) DO UPDATE SET sort_order=EXCLUDED.sort_order"
          [toPersistValue profileId,toPersistValue itemId,PersistInt64 (fromIntegral position)]
      Just details -> forM_ (zip [0::Int ..] details) $ \(position,DirectoryProfessionInput{professionId,headline,yearsExperience,rateMinMinor,rateMaxMinor,currencyId}) ->
        rawExecute "INSERT INTO directory_profile_profession(profile_id,profession_id,headline,years_experience,rate_min_minor,rate_max_minor,currency_id,sort_order) VALUES (?,?,?,?,?,?,?,?) ON CONFLICT(profile_id,profession_id) DO UPDATE SET headline=EXCLUDED.headline,years_experience=EXCLUDED.years_experience,rate_min_minor=EXCLUDED.rate_min_minor,rate_max_minor=EXCLUDED.rate_max_minor,currency_id=EXCLUDED.currency_id,sort_order=EXCLUDED.sort_order"
          [toPersistValue profileId,toPersistValue professionId,optionalText (cleanOptionalText headline),maybe PersistNull PersistDouble yearsExperience,optionalInt64 rateMinMinor,optionalInt64 rateMaxMinor,optionalUuid currencyId,PersistInt64 (fromIntegral position)]

    rawExecute "DELETE FROM directory_profile_instrument WHERE profile_id=? AND instrument_id<>ALL(?::uuid[])" [toPersistValue profileId,uuidArrayValue selectedInstrumentIds]
    case instrumentDetails of
      Nothing -> forM_ (zip [0::Int ..] instrumentIds) $ \(position,itemId) ->
        rawExecute "INSERT INTO directory_profile_instrument(profile_id,instrument_id,sort_order) VALUES (?,?,?) ON CONFLICT(profile_id,instrument_id) DO UPDATE SET sort_order=EXCLUDED.sort_order"
          [toPersistValue profileId,toPersistValue itemId,PersistInt64 (fromIntegral position)]
      Just details -> forM_ (zip [0::Int ..] details) $ \(position,DirectoryInstrumentInput{instrumentId,proficiency}) ->
        rawExecute "INSERT INTO directory_profile_instrument(profile_id,instrument_id,proficiency,sort_order) VALUES (?,?,?,?) ON CONFLICT(profile_id,instrument_id) DO UPDATE SET proficiency=EXCLUDED.proficiency,sort_order=EXCLUDED.sort_order"
          [toPersistValue profileId,toPersistValue instrumentId,optionalText proficiency,PersistInt64 (fromIntegral position)]

    rawExecute "DELETE FROM directory_profile_genre WHERE profile_id=? AND genre_id<>ALL(?::uuid[])" [toPersistValue profileId,uuidArrayValue genreIds]
    forM_ (zip [0::Int ..] genreIds) $ \(position,itemId) -> rawExecute "INSERT INTO directory_profile_genre(profile_id,genre_id,sort_order) VALUES (?,?,?) ON CONFLICT(profile_id,genre_id) DO UPDATE SET sort_order=EXCLUDED.sort_order" [toPersistValue profileId,toPersistValue itemId,PersistInt64 (fromIntegral position)]
    rawExecute "DELETE FROM directory_profile_service WHERE profile_id=? AND service_offering_id<>ALL(?::uuid[])" [toPersistValue profileId,uuidArrayValue serviceOfferingIds]
    forM_ (zip [0::Int ..] serviceOfferingIds) $ \(position,itemId) -> rawExecute "INSERT INTO directory_profile_service(profile_id,service_offering_id,sort_order) VALUES (?,?,?) ON CONFLICT(profile_id,service_offering_id) DO UPDATE SET sort_order=EXCLUDED.sort_order" [toPersistValue profileId,toPersistValue itemId,PersistInt64 (fromIntegral position)]

    case languages of
      Nothing -> pure ()
      Just details -> do
        rawExecute "DELETE FROM directory_profile_language WHERE profile_id=? AND language_id<>ALL(?::uuid[])" [toPersistValue profileId,uuidArrayValue (map languageId details)]
        forM_ details $ \DirectoryLanguageInput{languageId,proficiency} -> rawExecute "INSERT INTO directory_profile_language(profile_id,language_id,proficiency) VALUES (?,?,?) ON CONFLICT(profile_id,language_id) DO UPDATE SET proficiency=EXCLUDED.proficiency" [toPersistValue profileId,toPersistValue languageId,optionalText proficiency]

    case serviceAreas of
      Just areas -> do
        rawExecute "DELETE FROM directory_profile_location WHERE profile_id=?" [toPersistValue profileId]
        forM_ areas $ \DirectoryServiceAreaInput{countryId=areaCountryId,subdivisionId,cityId=areaCityId,metropolitanAreaId=areaMetropolitanAreaId,sectorLabel,serviceRadiusKm,primaryLocation,onsite=areaOnsite} ->
          rawExecute "INSERT INTO directory_profile_location(profile_id,country_id,subdivision_id,city_id,metropolitan_area_id,sector_label,service_radius_km,public_latitude,public_longitude,precision,primary_location,onsite) SELECT ?,?,?,?,?,?,?,city.latitude,city.longitude,CASE WHEN ?::text IS NOT NULL THEN 'sector' WHEN ?::uuid IS NOT NULL THEN 'city' WHEN ?::uuid IS NOT NULL THEN 'metro' WHEN ?::uuid IS NOT NULL THEN 'region' ELSE 'country' END,?,? FROM (SELECT 1) seed LEFT JOIN city_reference city ON city.id=?::uuid"
            [toPersistValue profileId,toPersistValue areaCountryId,optionalUuid subdivisionId,optionalUuid areaCityId,optionalUuid areaMetropolitanAreaId,optionalText (cleanOptionalText sectorLabel),maybe PersistNull PersistDouble serviceRadiusKm,optionalText (cleanOptionalText sectorLabel),optionalUuid areaCityId,optionalUuid areaMetropolitanAreaId,optionalUuid subdivisionId,PersistBool primaryLocation,PersistBool areaOnsite,optionalUuid areaCityId]
      Nothing -> do
        rawExecute "UPDATE directory_profile_location SET primary_location=FALSE WHERE profile_id=? AND primary_location" [toPersistValue profileId]
        rawExecute "UPDATE directory_profile_location SET primary_location=TRUE,service_radius_km=?,onsite=?,updated_at=now() WHERE id=(SELECT id FROM directory_profile_location WHERE profile_id=? AND country_id=? AND subdivision_id IS NULL AND city_id IS NOT DISTINCT FROM ?::uuid AND metropolitan_area_id IS NOT DISTINCT FROM ?::uuid AND sector_label IS NULL ORDER BY created_at,id LIMIT 1)"
          [maybe PersistNull PersistDouble travelRadiusKm,PersistBool onsite,toPersistValue profileId,toPersistValue countryId,optionalUuid cityId,optionalUuid metropolitanAreaId]
        rawExecute "INSERT INTO directory_profile_location(id,profile_id,country_id,city_id,metropolitan_area_id,service_radius_km,public_latitude,public_longitude,precision,primary_location,onsite) SELECT ?,?,?,?, ?,?,city.latitude,city.longitude,CASE WHEN ?::uuid IS NULL THEN CASE WHEN ?::uuid IS NULL THEN 'country' ELSE 'metro' END ELSE 'city' END,TRUE,? FROM (SELECT 1) seed LEFT JOIN city_reference city ON city.id=?::uuid WHERE NOT EXISTS (SELECT 1 FROM directory_profile_location existing WHERE existing.profile_id=? AND existing.primary_location)"
          [toPersistValue _locationId,toPersistValue profileId,toPersistValue countryId,optionalUuid cityId,optionalUuid metropolitanAreaId,maybe PersistNull PersistDouble travelRadiusKm,optionalUuid cityId,optionalUuid metropolitanAreaId,PersistBool onsite,optionalUuid cityId,toPersistValue profileId]

    rawExecute "UPDATE directory_profile profile SET completeness_score=least(1,.20+CASE WHEN length(trim(coalesce(profile.bio,'')))>=40 THEN .15 ELSE 0 END+CASE WHEN length(trim(coalesce(profile.experience_summary,'')))>=20 OR jsonb_array_length(profile.portfolio)>0 THEN .10 ELSE 0 END+CASE WHEN EXISTS(SELECT 1 FROM directory_profile_profession member WHERE member.profile_id=profile.id) OR EXISTS(SELECT 1 FROM directory_profile_service member WHERE member.profile_id=profile.id) THEN .20 ELSE 0 END+CASE WHEN EXISTS(SELECT 1 FROM directory_profile_location location WHERE location.profile_id=profile.id) OR profile.remote THEN .15 ELSE 0 END+CASE WHEN profile.onsite OR profile.remote OR profile.available_to_travel THEN .10 ELSE 0 END+CASE WHEN EXISTS(SELECT 1 FROM directory_profile_instrument member WHERE member.profile_id=profile.id) OR EXISTS(SELECT 1 FROM directory_profile_genre member WHERE member.profile_id=profile.id) THEN .05 ELSE 0 END+CASE WHEN EXISTS(SELECT 1 FROM directory_profile_language member WHERE member.profile_id=profile.id) OR jsonb_array_length(profile.links)>0 OR profile.equipment_summary IS NOT NULL THEN .05 ELSE 0 END) WHERE profile.id=?" [toPersistValue profileId]
    refreshProfileDB profileId

uuidArrayValue :: [UUID] -> PersistValue
uuidArrayValue values = PersistText ("{" <> T.intercalate "," (map UUID.toText values) <> "}")

cleanOptionalText :: Maybe Text -> Maybe Text
cleanOptionalText value = case T.strip <$> value of
  Just trimmed | not (T.null trimmed) -> Just trimmed
  _ -> Nothing

normalizePortfolioItem :: DirectoryPortfolioItem -> DirectoryPortfolioItem
normalizePortfolioItem item@DirectoryPortfolioItem{title,url,description,thumbnailUrl} =
  item
    { title = T.strip title
    , url = T.strip url
    , description = cleanOptionalText description
    , thumbnailUrl = cleanOptionalText thumbnailUrl
    }

normalizeProfileLink :: DirectoryProfileLink -> DirectoryProfileLink
normalizeProfileLink item@DirectoryProfileLink{label,url} =
  item { label = T.strip label, url = T.strip url }

validateProfileRequest :: DirectoryProfileUpsert -> AppM ()
validateProfileRequest DirectoryProfileUpsert
  { profileKind, publicName, slug, bio, experienceSummary, creditsSummary, portfolio, links
  , equipmentSummary, rateMinMinor, rateMaxMinor, currencyId, clearRates, availabilityStatus
  , professionIds, professionDetails, instrumentIds, instrumentDetails, genreIds
  , serviceOfferingIds, languages, serviceAreas, onsite, remote, availableToTravel
  , countryId, cityId, metropolitanAreaId, travelRadiusKm, coverImageUrl } = do
  validateSlug slug
  forM_ (cleanOptionalText coverImageUrl) $ \coverValue -> do
    validateHttpUrl "coverImageUrl" coverValue
    accepted <- jsonRows "SELECT to_jsonb(directory_safe_image_url(?) IS NOT NULL)" [PersistText coverValue]
    unless (accepted == [Bool True]) $ throwError err400 {errBody="coverImageUrl must be a public image URL"}
  let nameValue=T.strip publicName
  when (T.length nameValue<1 || T.length nameValue>160 || T.any isControl nameValue) $ throwError err400 {errBody="publicName is invalid"}
  unless (profileKind `Set.member` Set.fromList ["person","artist","band","project","organization","company","venue","studio","agency","label","distributor","school"]) $ throwError err400 {errBody="invalid profileKind"}
  unless (onsite || remote || availableToTravel) $ throwError err400 {errBody="at least one work modality is required"}
  when (maybe False (\radius -> radius<0 || radius>20000) travelRadiusKm) $ throwError err400 {errBody="invalid travelRadiusKm"}
  validateOptionalProfileText "bio" 10000 bio
  validateOptionalProfileText "experienceSummary" 5000 experienceSummary
  validateOptionalProfileText "creditsSummary" 5000 creditsSummary
  validateOptionalProfileText "equipmentSummary" 5000 equipmentSummary
  unless (maybe True (`Set.member` Set.fromList ["available","limited","unavailable","ask"]) availabilityStatus) $
    throwError err400 {errBody="invalid availabilityStatus"}
  validateUnique "professionIds" professionIds
  validateUnique "instrumentIds" instrumentIds
  validateUnique "genreIds" genreIds
  validateUnique "serviceOfferingIds" serviceOfferingIds
  case professionDetails of
    Nothing -> pure ()
    Just details -> do
      validateUnique "professionDetails" (map professionId details)
      unless (detailIdsMatch professionIds (map professionId details)) $
        throwError err400 {errBody="professionIds and professionDetails must describe the same set"}
      forM_ details validateProfessionInput
  case instrumentDetails of
    Nothing -> pure ()
    Just details -> do
      validateUnique "instrumentDetails" (map instrumentId details)
      unless (detailIdsMatch instrumentIds (map instrumentId details)) $
        throwError err400 {errBody="instrumentIds and instrumentDetails must describe the same set"}
      forM_ details $ \DirectoryInstrumentInput{proficiency} ->
        unless (maybe True (`Set.member` Set.fromList ["beginner","intermediate","advanced","professional","virtuoso"]) proficiency) $
          throwError err400 {errBody="invalid instrument proficiency"}
  case languages of
    Nothing -> pure ()
    Just details -> do
      when (length details > 20) $ throwError err400 {errBody="languages accepts at most 20 entries"}
      validateUnique "languages" (map languageId details)
      forM_ details $ \DirectoryLanguageInput{proficiency} ->
        unless (maybe True (`Set.member` Set.fromList ["basic","conversational","professional","native"]) proficiency) $
          throwError err400 {errBody="invalid language proficiency"}
  case portfolio of
    Nothing -> pure ()
    Just items -> do
      when (length items > 50) $ throwError err400 {errBody="portfolio accepts at most 50 entries"}
      forM_ items validatePortfolioItem
  case links of
    Nothing -> pure ()
    Just items -> do
      when (length items > 30) $ throwError err400 {errBody="links accepts at most 30 entries"}
      forM_ items validateProfileLink
  validateProfileRates rateMinMinor rateMaxMinor currencyId (fromMaybe False clearRates)
  case serviceAreas of
    Nothing -> validateServiceArea DirectoryServiceAreaInput
      { countryId, subdivisionId=Nothing, cityId, metropolitanAreaId, sectorLabel=Nothing
      , serviceRadiusKm=travelRadiusKm, primaryLocation=True, onsite }
    Just areas -> do
      when (length areas > 20) $ throwError err400 {errBody="serviceAreas accepts at most 20 entries"}
      when (onsite && null areas) $ throwError err400 {errBody="onsite profiles require at least one service area"}
      when (null areas) $ validateServiceArea DirectoryServiceAreaInput
        { countryId, subdivisionId=Nothing, cityId=Nothing, metropolitanAreaId=Nothing, sectorLabel=Nothing
        , serviceRadiusKm=Nothing, primaryLocation=True, onsite=False }
      unless (serviceAreaPrimaryValid (map primaryLocation areas)) $
        throwError err400 {errBody="serviceAreas requires exactly one primary location"}
      unless (null areas) $
        when (listToMaybe [areaCountry | DirectoryServiceAreaInput{countryId=areaCountry,primaryLocation=True} <- areas] /= Just countryId) $
          throwError err400 {errBody="countryId must match the primary service area"}
      let areaKeys = map serviceAreaKey areas
      when (length areaKeys /= Set.size (Set.fromList areaKeys)) $
        throwError err400 {errBody="serviceAreas contains duplicate locations"}
      forM_ areas validateServiceArea

validateOptionalProfileText :: Text -> Int -> Maybe Text -> AppM ()
validateOptionalProfileText fieldName maximumLength value =
  when (maybe False (\textValue -> T.length textValue > maximumLength || T.any isControl textValue) value) $
    throwError err400 {errBody=BL.fromStrict (TE.encodeUtf8 (fieldName <> " is invalid"))}

validateUnique :: Ord a => Text -> [a] -> AppM ()
validateUnique fieldName values =
  when (length values /= Set.size (Set.fromList values)) $
    throwError err400 {errBody=BL.fromStrict (TE.encodeUtf8 (fieldName <> " must not contain duplicates"))}

validateProfessionInput :: DirectoryProfessionInput -> AppM ()
validateProfessionInput DirectoryProfessionInput{headline,yearsExperience,rateMinMinor,rateMaxMinor,currencyId} = do
  validateOptionalProfileText "profession headline" 160 headline
  when (maybe False (\years -> years < 0 || years > 100) yearsExperience) $
    throwError err400 {errBody="yearsExperience must be between 0 and 100"}
  validateProfileRates rateMinMinor rateMaxMinor currencyId False

validateProfileRates :: Maybe Int64 -> Maybe Int64 -> Maybe UUID -> Bool -> AppM ()
validateProfileRates minimumRate maximumRate currency clear = do
  when (clear && any isJust [minimumRate,maximumRate]) $
    throwError err400 {errBody="clearRates cannot be combined with rate amounts"}
  when (clear && isJust currency) $
    throwError err400 {errBody="clearRates cannot be combined with currencyId"}
  when (maybe False (<0) minimumRate || maybe False (<0) maximumRate) $
    throwError err400 {errBody="rates cannot be negative"}
  when (isJust maximumRate && not (isJust minimumRate)) $
    throwError err400 {errBody="rateMaxMinor requires rateMinMinor"}
  when (maybe False (\upper -> maybe False (>upper) minimumRate) maximumRate) $
    throwError err400 {errBody="rateMaxMinor cannot be lower than rateMinMinor"}
  when ((isJust minimumRate || isJust maximumRate) && not (isJust currency)) $
    throwError err400 {errBody="currencyId is required when rates are present"}
  when (isJust currency && not (isJust minimumRate)) $
    throwError err400 {errBody="currencyId requires rateMinMinor"}

validatePortfolioItem :: DirectoryPortfolioItem -> AppM ()
validatePortfolioItem DirectoryPortfolioItem{itemType,title,url,description,thumbnailUrl} = do
  unless (itemType `Set.member` Set.fromList ["audio","video","image","release","credit","document","other"]) $
    throwError err400 {errBody="invalid portfolio itemType"}
  validateRequiredProfileText "portfolio title" 160 title
  validateHttpUrl "portfolio url" url
  validateOptionalProfileText "portfolio description" 1000 description
  forM_ thumbnailUrl (validateHttpUrl "portfolio thumbnailUrl")

validateProfileLink :: DirectoryProfileLink -> AppM ()
validateProfileLink DirectoryProfileLink{label,url} = do
  validateRequiredProfileText "link label" 80 label
  validateHttpUrl "link url" url

validateRequiredProfileText :: Text -> Int -> Text -> AppM ()
validateRequiredProfileText fieldName maximumLength value =
  when (T.null (T.strip value) || T.length value > maximumLength || T.any isControl value) $
    throwError err400 {errBody=BL.fromStrict (TE.encodeUtf8 (fieldName <> " is invalid"))}

validateHttpUrl :: Text -> Text -> AppM ()
validateHttpUrl fieldName raw = do
  let value = T.strip raw
      lower = T.toLower value
      validScheme = "https://" `T.isPrefixOf` lower || "http://" `T.isPrefixOf` lower
      authorityAndPath = fromMaybe "" (T.stripPrefix "https://" lower <|> T.stripPrefix "http://" lower)
      authority = T.takeWhile (\character -> character /= '/' && character /= '?' && character /= '#') authorityAndPath
      validAbsolute = validScheme && not (T.null authority) && not (T.any (=='@') authority)
      validSameOrigin = "/" `T.isPrefixOf` value && not ("//" `T.isPrefixOf` value) && not (T.any (=='\\') value)
  when (T.length value > 2048 || not (validAbsolute || validSameOrigin) || T.any (\character -> isControl character || character == ' ' || character == '\\') value) $
    throwError err400 {errBody=BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be an HTTP(S) or same-origin URL without embedded credentials"))}

serviceAreaKey :: DirectoryServiceAreaInput -> Text
serviceAreaKey DirectoryServiceAreaInput{countryId,subdivisionId,cityId,metropolitanAreaId,sectorLabel} =
  T.intercalate ":" [UUID.toText countryId,maybe "" UUID.toText subdivisionId,maybe "" UUID.toText cityId,maybe "" UUID.toText metropolitanAreaId,maybe "" (T.toLower . T.strip) sectorLabel]

validateServiceArea :: DirectoryServiceAreaInput -> AppM ()
validateServiceArea DirectoryServiceAreaInput{countryId,subdivisionId,cityId,metropolitanAreaId,sectorLabel,serviceRadiusKm} = do
  validateOptionalProfileText "sectorLabel" 80 sectorLabel
  when (maybe False (\radius -> radius < 0 || radius > 20000) serviceRadiusKm) $
    throwError err400 {errBody="invalid serviceRadiusKm"}
  hierarchyMatches <- jsonOne err500
    "WITH requested AS (SELECT ?::uuid country_id,?::uuid subdivision_id,?::uuid city_id,?::uuid metro_id) SELECT to_jsonb(EXISTS (SELECT 1 FROM requested JOIN country_reference country ON country.id=requested.country_id AND country.active WHERE (requested.subdivision_id IS NULL OR EXISTS (SELECT 1 FROM subdivision_reference subdivision WHERE subdivision.id=requested.subdivision_id AND subdivision.country_id=country.id AND subdivision.active)) AND (requested.city_id IS NULL OR EXISTS (SELECT 1 FROM city_reference city WHERE city.id=requested.city_id AND city.country_id=country.id AND city.active AND (requested.subdivision_id IS NULL OR city.subdivision_id=requested.subdivision_id))) AND (requested.metro_id IS NULL OR EXISTS (SELECT 1 FROM metropolitan_area metro WHERE metro.id=requested.metro_id AND metro.country_id=country.id AND metro.active AND (requested.subdivision_id IS NULL OR metro.subdivision_id=requested.subdivision_id) AND (requested.city_id IS NULL OR EXISTS (SELECT 1 FROM metropolitan_area_city membership WHERE membership.metropolitan_area_id=metro.id AND membership.city_id=requested.city_id))))))"
    [toPersistValue countryId,optionalUuid subdivisionId,optionalUuid cityId,optionalUuid metropolitanAreaId]
  unless (hierarchyMatches == Bool True) $
    throwError err400 {errBody="service area references must form one active geographic hierarchy"}

validateSlug :: Text -> AppM ()
validateSlug raw = do
  let value=T.toLower (T.strip raw)
  when (T.length value<2 || T.length value>120 || T.head value=='-' || T.last value=='-' || T.any (\char -> not (isAscii char && (isAlphaNum char || char=='-'))) value) $ throwError err400 {errBody="slug must contain lowercase ASCII letters, numbers, or hyphens"}

changeProfileStatus user profileId DirectoryStatusRequest{status=newStatus,reason=statusReason} = do
  requireProfileCapability user profileId "publish"
  assurance <- ageAssurance user
  when (newStatus=="published" && not (minorMayPublishOrRespond assurance)) $ throwError err403 {errBody="age assurance or approved guardian consent is required"}
  when (newStatus=="published") $ do
    eligible <- jsonRows "SELECT to_jsonb(profile.completeness_score>=.5 AND profile.moderation_status='allowed' AND (profile.remote OR EXISTS(SELECT 1 FROM directory_profile_location location WHERE location.profile_id=profile.id))) FROM directory_profile profile WHERE profile.id=?" [toPersistValue profileId]
    unless (listToMaybe eligible==Just (Bool True)) $ throwError err409 {errBody="profile does not meet the declared publication completeness, location, or moderation preconditions"}
  when (newStatus `elem` ["suspended","merged"]) $ throwError err403 {errBody="this status requires administrative review"}
  current <- jsonOne err404 "SELECT to_jsonb(profile_status) FROM directory_profile WHERE id=?" [toPersistValue profileId]
  oldStatus <- case current of String value -> pure value; _ -> throwError err500
  from <- maybe (throwError err409 {errBody="unknown current profile state"}) pure (parseProfileStatus oldStatus)
  to <- maybe (throwError err400 {errBody="invalid target profile state"}) pure (parseProfileStatus newStatus)
  unless (allowedProfileTransition from to) $ throwError err409 {errBody="undeclared profile transition"}
  runDB $ do
    rawExecute "UPDATE directory_profile SET profile_status=?,published_at=CASE WHEN ?='published' THEN coalesce(published_at,now()) ELSE published_at END,archived_at=CASE WHEN ?='archived' THEN now() ELSE archived_at END,updated_at=now(),version=version+1 WHERE id=?" [PersistText newStatus,PersistText newStatus,PersistText newStatus,toPersistValue profileId]
    rawExecute "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,new_state,correlation_id,metadata) VALUES (?,'profile.status','profile',?,?,?,jsonb_build_object('reason',?::text))" [toPersistValue (auPartyId user),PersistText (UUID.toText profileId),PersistText newStatus,PersistText ("profile-status-"<>UUID.toText profileId),optionalText statusReason]
    refreshProfileDB profileId
  when (newStatus == "published") $
    recordAuthenticatedEvent user "profile_completed" "profile" (UUID.toText profileId)
  profileSummary user profileId

parseProfileStatus value = lookup value
  [("draft",ProfileDraft),("pending_review",ProfilePendingReview),("published",ProfilePublished),("paused",ProfilePaused),("archived",ProfileArchived),("suspended",ProfileSuspended),("merged",ProfileMerged)]

profileSummary user profileId = jsonOne err404
  ("SELECT managed.value FROM (" <> managedProfileSql <> ") managed WHERE managed.value->>'id'=?")
  [toPersistValue (auPartyId user),PersistText (UUID.toText profileId)]

listManagedClassifieds user = jsonRows ("SELECT " <> managedClassifiedProjectionSql <> " FROM classified JOIN classified_category category ON category.id=classified.category_id JOIN directory_profile_manager manager ON manager.profile_id=classified.author_profile_id WHERE manager.account_party_id=? AND manager.active ORDER BY classified.updated_at DESC,classified.id") [toPersistValue (auPartyId user)]

createClassified user idempotency request@ClassifiedCreateRequest
  { authorProfileId, categoryId, title, slug, description, onsite, remote
  , availableToTravel, startsAt, endsAt, experienceLevel, compensationTypeId
  , budgetMinMinor, budgetMaxMinor, currencyId, budgetNegotiable
  , serviceOfferingId, serviceAdId, expiresAt, slots, priceMode, offerDeadlineAt, terms } = do
  requireProfileCapability user authorProfileId "publish"
  requireAdult user
  validateClassified request
  (slotCount, mode, termsValue) <- validateMarketplaceFields slots priceMode terms budgetMinMinor budgetMaxMinor currencyId budgetNegotiable
  classifiedId <- reserveIdempotency user "classified.create" idempotency request "classified"
  existing <- jsonRows "SELECT jsonb_build_object('id',id,'slug',slug,'status',status,'version',version) FROM classified WHERE id=?" [toPersistValue classifiedId]
  case existing of
    value:_ -> pure value
    [] -> do
      now <- liftIO getCurrentTime
      let expiry=fromMaybe (addUTCTime (30*24*60*60) now) expiresAt
      when (expiry<=now || expiry>addUTCTime (90*24*60*60) now) $
        throwError err400 {errBody="expiresAt must be in the future and no more than 90 days away; renew manually after publication"}
      when (maybe False (\deadline -> deadline<=now || deadline>expiry) offerDeadlineAt) $
        throwError err400 {errBody="offerDeadlineAt must be in the future and no later than expiresAt"}
      runDB $ do
        rawExecute "INSERT INTO classified(id,author_profile_id,category_id,title,slug,description,status,moderation_status,onsite,remote,available_to_travel,starts_at,ends_at,experience_level,compensation_type_id,budget_min_minor,budget_max_minor,currency_id,budget_negotiable,service_offering_id,service_ad_id,expires_at,duplicate_fingerprint,slots,slots_remaining,price_mode,offer_deadline_at,terms) VALUES (?,?,?,?,?,?,'draft','allowed',?,?,?,?,?,?,?,?,?,?,?,?,?,?,encode(digest(directory_normalize_text(?)||':'||directory_normalize_text(?),'sha256'),'hex'),?,?,?,?,?::jsonb)"
          [toPersistValue classifiedId,toPersistValue authorProfileId,toPersistValue categoryId,PersistText (T.strip title),PersistText (T.toLower (T.strip slug)),PersistText (T.strip description),PersistBool onsite,PersistBool remote,PersistBool availableToTravel,optionalTime startsAt,optionalTime endsAt,optionalText experienceLevel,optionalUuid compensationTypeId,optionalInt64 budgetMinMinor,optionalInt64 budgetMaxMinor,optionalUuid currencyId,PersistBool budgetNegotiable,optionalUuid serviceOfferingId,optionalInt64 serviceAdId,toPersistValue expiry,PersistText title,PersistText description,PersistInt64 (fromIntegral slotCount),PersistInt64 (fromIntegral slotCount),PersistText (priceModeText mode),optionalTime offerDeadlineAt,PersistText (decodeJsonText termsValue)]
        replaceClassifiedSelectionsDB classifiedId request
      classifiedSummary classifiedId

-- ADR 0134: slots, price mode and structured terms. The price mode defaults
-- from the budget exactly as the migration backfilled existing rows.
validateMarketplaceFields :: Maybe Int -> Maybe Text -> Maybe Value -> Maybe Int64 -> Maybe Int64 -> Maybe UUID -> Bool -> AppM (Int, PriceMode, Value)
validateMarketplaceFields slots priceMode terms budgetMinMinor budgetMaxMinor currencyId budgetNegotiable = do
  let slotCount = fromMaybe 1 slots
  when (slotCount < 1 || slotCount > 100) $ throwError err400 {errBody="slots must be between 1 and 100"}
  mode <- case priceMode of
    Nothing -> pure $ case budgetMaxMinor of
      Nothing -> PriceOpen
      Just maximumValue
        | budgetNegotiable -> PriceSuggested
        | maybe True (== maximumValue) budgetMinMinor -> PriceFixed
        | otherwise -> PriceSuggested
    Just raw -> maybe (throwError err400 {errBody="priceMode must be open, suggested or fixed"}) pure (parsePriceMode raw)
  when (mode /= PriceOpen && (budgetMaxMinor == Nothing || currencyId == Nothing)) $
    throwError err400 {errBody="suggested and fixed prices require budgetMaxMinor and currencyId"}
  when (mode == PriceFixed && maybe False (\minimumValue -> Just minimumValue /= budgetMaxMinor) budgetMinMinor) $
    throwError err400 {errBody="a fixed price requires a single amount"}
  let termsValue = fromMaybe (object []) terms
  requireJsonObject "terms" termsValue
  when (BL.length (encode termsValue) > 20000) $ throwError err400 {errBody="terms is too large"}
  pure (slotCount, mode, termsValue)

validateClassified ClassifiedCreateRequest
  { categoryId, title, slug, description, onsite, remote, availableToTravel
  , professionIds, instrumentIds, genreIds, countryIds, cityIds, metropolitanAreaIds
  , startsAt, endsAt, compensationTypeId, budgetMinMinor, budgetMaxMinor, currencyId
  , serviceOfferingId } = do
  validateSlug slug
  when (T.length (T.strip title)<5 || T.length title>160) $ throwError err400 {errBody="title must contain 5-160 characters"}
  when (T.length (T.strip description)<20 || T.length description>10000) $ throwError err400 {errBody="description must contain 20-10000 characters"}
  unless (onsite || remote || availableToTravel) $ throwError err400 {errBody="at least one modality is required"}
  when (case (budgetMinMinor,budgetMaxMinor) of (Just minimumValue,Just maximumValue)->maximumValue<minimumValue; _->False) $ throwError err400 {errBody="budget range is invalid"}
  when ((budgetMinMinor /= Nothing || budgetMaxMinor /= Nothing) && currencyId==Nothing) $ throwError err400 {errBody="currencyId is required with a budget"}
  requirements <- jsonOne err400 "SELECT requirements FROM classified_category WHERE id=? AND active" [toPersistValue categoryId]
  when (classifiedCategoryIsDerived requirements) $
    throwError err400 {errBody="this category is generated automatically from artist profiles"}
  let required = classifiedRequiredFields requirements
      supported = Set.fromList
        [ "instrumentIds", "genreIds", "professionIds", "locations", "dateRange"
        , "expiresAt", "compensationTypeId", "budget", "serviceOfferingId", "locationsOrRemote"
        ]
      unknown = required `Set.difference` supported
      hasLocation = not (null countryIds && null cityIds && null metropolitanAreaIds)
      requireField field condition message = when (field `Set.member` required && not condition) (throwError err400 {errBody=message})
  unless (Set.null unknown) $ throwError err409 {errBody="classified category has unsupported required fields"}
  requireField "instrumentIds" (not (null instrumentIds)) "instrumentIds is required by this category"
  requireField "genreIds" (not (null genreIds)) "genreIds is required by this category"
  requireField "professionIds" (not (null professionIds)) "professionIds is required by this category"
  requireField "locations" hasLocation "at least one location is required by this category"
  requireField "locationsOrRemote" (hasLocation || remote) "a location or remote modality is required by this category"
  requireField "dateRange" (startsAt /= Nothing && endsAt /= Nothing) "startsAt and endsAt are required by this category"
  requireField "compensationTypeId" (compensationTypeId /= Nothing) "compensationTypeId is required by this category"
  requireField "budget" (budgetMinMinor /= Nothing || budgetMaxMinor /= Nothing) "a budget is required by this category"
  requireField "serviceOfferingId" (serviceOfferingId /= Nothing) "serviceOfferingId is required by this category"
  when (case (startsAt,endsAt) of (Just startValue,Just endValue)->endValue<startValue; _->False) $
    throwError err400 {errBody="endsAt must not precede startsAt"}
  validateCompensation compensationTypeId budgetMinMinor budgetMaxMinor

classifiedCategoryIsDerived :: Value -> Bool
classifiedCategoryIsDerived (Object values) = KeyMap.member "derivation" values
classifiedCategoryIsDerived _ = False

classifiedRequiredFields :: Value -> Set.Set Text
classifiedRequiredFields (Object values) = case KeyMap.lookup "required" values of
  Just (Array items) -> Set.fromList [value | String value <- toList items]
  _ -> Set.empty
classifiedRequiredFields _ = Set.empty

validateCompensation :: Maybe UUID -> Maybe Int64 -> Maybe Int64 -> AppM ()
validateCompensation Nothing _ _ = pure ()
validateCompensation (Just compensationId) minimumValue maximumValue = do
  metadata <- jsonOne err400 "SELECT metadata FROM compensation_type WHERE id=? AND active" [toPersistValue compensationId]
  let budgetMode = case metadata of
        Object values -> case KeyMap.lookup "budget" values of Just (String value) -> Just value; _ -> Nothing
        _ -> Nothing
  case budgetMode of
    Just "exact" -> unless (minimumValue /= Nothing && maybe True (== fromMaybe 0 minimumValue) maximumValue) $
      throwError err400 {errBody="exact compensation requires one amount (or an equal min/max)"}
    Just "range" -> unless (minimumValue /= Nothing && maximumValue /= Nothing) $
      throwError err400 {errBody="range compensation requires budgetMinMinor and budgetMaxMinor"}
    Just "optional" -> pure ()
    Just "forbidden" -> when (minimumValue /= Nothing || maximumValue /= Nothing) $
      throwError err400 {errBody="this compensation type does not permit a monetary budget"}
    _ -> throwError err409 {errBody="compensation type has unsupported budget metadata"}

replaceClassifiedSelectionsDB classifiedId ClassifiedCreateRequest
  { professionIds, instrumentIds, genreIds, countryIds, cityIds, metropolitanAreaIds } = do
  forM_ (Set.toList (Set.fromList professionIds)) $ \itemId -> rawExecute "INSERT INTO classified_profession(classified_id,profession_id) VALUES (?,?) ON CONFLICT DO NOTHING" [toPersistValue classifiedId,toPersistValue itemId]
  forM_ (Set.toList (Set.fromList instrumentIds)) $ \itemId -> rawExecute "INSERT INTO classified_instrument(classified_id,instrument_id) VALUES (?,?) ON CONFLICT DO NOTHING" [toPersistValue classifiedId,toPersistValue itemId]
  forM_ (Set.toList (Set.fromList genreIds)) $ \itemId -> rawExecute "INSERT INTO classified_genre(classified_id,genre_id) VALUES (?,?) ON CONFLICT DO NOTHING" [toPersistValue classifiedId,toPersistValue itemId]
  forM_ (Set.toList (Set.fromList countryIds)) $ \itemId -> rawExecute "INSERT INTO classified_location(classified_id,country_id,service_radius_km) VALUES (?,?,NULL) ON CONFLICT DO NOTHING" [toPersistValue classifiedId,toPersistValue itemId]
  forM_ (Set.toList (Set.fromList cityIds)) $ \itemId -> rawExecute "INSERT INTO classified_location(classified_id,country_id,subdivision_id,city_id) SELECT ?,country_id,subdivision_id,id FROM city_reference WHERE id=? ON CONFLICT DO NOTHING" [toPersistValue classifiedId,toPersistValue itemId]
  forM_ (Set.toList (Set.fromList metropolitanAreaIds)) $ \itemId -> rawExecute "INSERT INTO classified_location(classified_id,country_id,subdivision_id,metropolitan_area_id) SELECT ?,country_id,subdivision_id,id FROM metropolitan_area WHERE id=? ON CONFLICT DO NOTHING" [toPersistValue classifiedId,toPersistValue itemId]
  refreshClassifiedDB classifiedId

changeClassifiedStatus user classifiedId DirectoryStatusRequest{status=newStatus,reason=statusReason} = do
  authorProfile <- requireClassifiedAuthor user classifiedId
  derived <- jsonRows "SELECT to_jsonb(TRUE) FROM classified WHERE id=? AND source_profile_id IS NOT NULL" [toPersistValue classifiedId]
  unless (null derived) $
    throwError err409 {errBody="artist profile listings follow their profile; change the profile publication instead"}
  when (newStatus=="published") (requireAdult user)
  current <- jsonOne err404 "SELECT to_jsonb(status) FROM classified WHERE id=?" [toPersistValue classifiedId]
  oldStatus <- case current of String value -> pure value; _ -> throwError err500
  from <- maybe (throwError err409 {errBody="unknown current classified state"}) pure (parseClassifiedStatus oldStatus)
  to <- maybe (throwError err400 {errBody="invalid target classified state"}) pure (parseClassifiedStatus newStatus)
  unless (allowedClassifiedTransition from to) $ throwError err409 {errBody="undeclared classified transition"}
  runDB $ do
    rawExecute "UPDATE classified SET status=?,closed_at=CASE WHEN ? IN ('filled','withdrawn') THEN now() ELSE closed_at END,published_at=CASE WHEN ?='published' AND published_at IS NULL THEN now() ELSE published_at END,updated_at=now(),version=version+1 WHERE id=?" [PersistText newStatus,PersistText newStatus,PersistText newStatus,toPersistValue classifiedId]
    rawExecute "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,previous_state,new_state,correlation_id,metadata) VALUES (?,'classified.status','classified',?,?,?,?,jsonb_build_object('authorProfileId',?::text,'reason',?::text))" [toPersistValue (auPartyId user),PersistText (UUID.toText classifiedId),PersistText oldStatus,PersistText newStatus,PersistText ("classified-status-"<>UUID.toText classifiedId),PersistText (UUID.toText authorProfile),optionalText statusReason]
    -- ADR 0134: closing a classified rejects every open offer with a reason.
    forM_ (closureRejectionReason to) $ \reason -> closeOpenOffersDB user classifiedId reason
    refreshClassifiedDB classifiedId
  when (newStatus == "published") $
    recordAuthenticatedEvent user "classified_published" "classified" (UUID.toText classifiedId)
  classifiedSummary classifiedId

-- Rejects open offers of a classified, notifies each offerer and audits the
-- bulk rejection. Runs inside the caller's transaction.
closeOpenOffersDB :: AuthedUser -> UUID -> Text -> SqlPersistT IO ()
closeOpenOffersDB user classifiedId reason = do
  rawExecute "INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at) SELECT DISTINCT manager.account_party_id,'marketplace.offer.rejected','Oferta cerrada',CASE WHEN ?='slots_filled' THEN 'El anuncio cubrió sus cupos con otra oferta.' ELSE 'El anuncio se cerró antes de decidir sobre tu oferta.' END,'directory_application',application.id::text,FALSE,now() FROM classified_application application JOIN directory_profile_manager manager ON manager.profile_id=application.applicant_profile_id AND manager.active AND manager.can_contact WHERE application.classified_id=? AND application.status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant')" [PersistText reason,toPersistValue classifiedId]
  rawExecute "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,previous_state,new_state,correlation_id,metadata) SELECT ?,'application.status','application',application.id::text,application.status,'rejected','classified-close-'||application.classified_id::text,jsonb_build_object('reason',?::text) FROM classified_application application WHERE application.classified_id=? AND application.status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant')" [toPersistValue (auPartyId user),PersistText reason,toPersistValue classifiedId]
  rawExecute "UPDATE classified_application SET status='rejected',rejection_reason=?,updated_at=now(),version=version+1 WHERE classified_id=? AND status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant')" [PersistText reason,toPersistValue classifiedId]

parseClassifiedStatus value = lookup value [("draft",Draft),("pending_moderation",PendingModeration),("published",Published),("paused",Paused),("filled",Filled),("expired",Expired),("withdrawn",Withdrawn),("rejected",Rejected),("moderated",Moderated)]

classifiedSummary classifiedId = jsonOne err404 ("SELECT " <> managedClassifiedProjectionSql <> " FROM classified JOIN classified_category category ON category.id=classified.category_id WHERE classified.id=?") [toPersistValue classifiedId]

-- An offer is active while its stored state is open and its expiry has not
-- elapsed; elapsed offers read as expired before any write persists that
-- (ADR 0134). The alias of classified_application is a parameter.
activeOfferSql :: Text -> Text
activeOfferSql alias =
  alias <> ".status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant') AND (" <> alias <> ".offer_expires_at IS NULL OR " <> alias <> ".offer_expires_at>now())"

-- Negotiation rounds of an offer, oldest first.
offerRoundsSql :: Text
offerRoundsSql =
  "(SELECT coalesce(jsonb_agg(jsonb_build_object('roundNumber',round.round_number,'actorRole',round.actor_role,'kind',round.kind,'amountMinor',round.amount_minor,'currencyId',round.currency_id,'terms',round.terms,'message',round.message,'expiresAt',round.expires_at,'createdAt',round.created_at) ORDER BY round.round_number),'[]'::jsonb) FROM classified_offer_round round WHERE round.application_id=application.id)"

-- Shared projection for the author's own listings (ADR 0134 adds intent,
-- slots, price mode, offer deadline and the number of offers received).
managedClassifiedProjectionSql :: Text
managedClassifiedProjectionSql =
  "jsonb_build_object('id',classified.id,'authorProfileId',classified.author_profile_id,'sourceProfileId',classified.source_profile_id,'title',classified.title,'slug',classified.slug,'status',classified.status,'moderationStatus',classified.moderation_status,'expiresAt',NULLIF(classified.expires_at,'infinity'::timestamptz),'version',classified.version,'intent',category.intent,'slots',classified.slots,'slotsRemaining',classified.slots_remaining,'priceMode',classified.price_mode,'offerDeadlineAt',classified.offer_deadline_at,'budget',CASE WHEN classified.budget_max_minor IS NULL AND classified.budget_min_minor IS NULL THEN NULL ELSE jsonb_build_object('minMinor',classified.budget_min_minor,'maxMinor',classified.budget_max_minor,'currencyId',classified.currency_id,'negotiable',classified.budget_negotiable) END,'offerCount',(SELECT count(*) FROM classified_application application WHERE application.classified_id=classified.id),'openOfferCount',(SELECT count(*) FROM classified_application application WHERE application.classified_id=classified.id AND " <> activeOfferSql "application" <> "))"

listApplications user classifiedId mSort = do
  _ <- requireClassifiedAuthor user classifiedId
  orderBy <- case normalizeOptional mSort of
    Nothing -> pure "application.submitted_at DESC,application.id"
    Just "recent" -> pure "application.updated_at DESC,application.id"
    Just "price_asc" -> pure "application.current_amount_minor ASC NULLS LAST,application.submitted_at DESC,application.id"
    Just "price_desc" -> pure "application.current_amount_minor DESC NULLS LAST,application.submitted_at DESC,application.id"
    Just "reputation" -> pure "(reputation.value->>'ratingAvg')::numeric DESC NULLS LAST,(reputation.value->>'completedCount')::integer DESC NULLS LAST,application.submitted_at DESC,application.id"
    Just _ -> throwError err400 {errBody="sort must be recent, price_asc, price_desc or reputation"}
  jsonRows ("SELECT " <> offerProjectionSql "'author'" <> " FROM classified_application application JOIN classified ON classified.id=application.classified_id JOIN directory_profile profile ON profile.id=application.applicant_profile_id CROSS JOIN LATERAL (" <> profileReputationSql "application.applicant_profile_id" <> ") reputation WHERE application.classified_id=? ORDER BY " <> orderBy) [toPersistValue classifiedId]

-- Offers where the caller manages the offering profile (ADR 0134).
listMyApplications user mStatus = do
  let statusFilter = normalizeOptional mStatus
  when (maybe False (`notElem` ["open","accepted","closed"]) statusFilter) $
    throwError err400 {errBody="status must be open, accepted or closed"}
  jsonRows ("SELECT " <> offerProjectionSql "'applicant'" <> " FROM classified_application application JOIN classified ON classified.id=application.classified_id JOIN directory_profile profile ON profile.id=application.applicant_profile_id JOIN directory_profile_manager manager ON manager.profile_id=application.applicant_profile_id AND manager.account_party_id=? AND manager.active CROSS JOIN LATERAL (" <> profileReputationSql "classified.author_profile_id" <> ") reputation WHERE (?::text IS NULL OR (?::text='open' AND " <> activeOfferSql "application" <> ") OR (?::text='accepted' AND application.status IN ('accepted','conversation_open','converted')) OR (?::text='closed' AND (application.status IN ('rejected','withdrawn','expired') OR (application.status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant') AND application.offer_expires_at<=now())))) ORDER BY application.updated_at DESC,application.id")
    [toPersistValue (auPartyId user),optionalText statusFilter,optionalText statusFilter,optionalText statusFilter,optionalText statusFilter]

getApplication user applicationId = do
  role <- applicationParticipantRole user applicationId
  -- The role comes from applicationParticipantRole (a closed set), so it is
  -- safe to interpolate as a SQL literal; the author sees the offerer's
  -- reputation and the offerer sees the author's.
  let roleLiteral = "'" <> role <> "'"
      counterpartProfile = if role == "author" then "application.applicant_profile_id" else "classified.author_profile_id"
  jsonOne err404 ("SELECT " <> offerProjectionSql roleLiteral <> " FROM classified_application application JOIN classified ON classified.id=application.classified_id JOIN directory_profile profile ON profile.id=application.applicant_profile_id CROSS JOIN LATERAL (" <> profileReputationSql counterpartProfile <> ") reputation WHERE application.id=?")
    [toPersistValue applicationId]

-- Typed offer projection shared by every offer endpoint, negotiation rounds
-- included so boards and lists show counter-offer messages before a decision.
-- The participant role is interpolated as a SQL literal or placeholder by the
-- caller.
offerProjectionSql :: Text -> Text
offerProjectionSql roleSql =
  "jsonb_build_object('id',application.id,'classifiedId',application.classified_id,'classified',jsonb_build_object('id',classified.id,'authorProfileId',classified.author_profile_id,'title',classified.title,'slug',classified.slug,'status',classified.status,'priceMode',classified.price_mode,'slots',classified.slots,'slotsRemaining',classified.slots_remaining,'offerDeadlineAt',classified.offer_deadline_at,'budget',CASE WHEN classified.budget_max_minor IS NULL AND classified.budget_min_minor IS NULL THEN NULL ELSE jsonb_build_object('minMinor',classified.budget_min_minor,'maxMinor',classified.budget_max_minor,'currencyId',classified.currency_id,'negotiable',classified.budget_negotiable) END),'applicantProfileId',application.applicant_profile_id,'applicantProfile',jsonb_build_object('id',profile.id,'name',profile.public_name,'slug',profile.slug,'kind',profile.profile_kind),'message',application.message,'portfolio',application.portfolio,'availability',application.availability_text,'currencyId',application.currency_id,'proposedAmountMinor',application.proposed_amount_minor,'currentAmountMinor',application.current_amount_minor,'acceptedAmountMinor',application.accepted_amount_minor,'currentRound',application.current_round,'lastActorRole',application.last_actor_role,'terms',application.terms,'offerExpiresAt',application.offer_expires_at,'status',CASE WHEN application.status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant') AND application.offer_expires_at IS NOT NULL AND application.offer_expires_at<=now() THEN 'expired' ELSE application.status END,'rejectionReason',application.rejection_reason,'participantRole'," <> roleSql <> ",'reputation',reputation.value,'rounds'," <> offerRoundsSql <> ",'submittedAt',application.submitted_at,'updatedAt',application.updated_at,'acceptedAt',application.accepted_at,'version',application.version)"

-- Verified-review reputation of a profile, from directory_review (Phase 3
-- adds response metrics and badges on top of this).
profileReputationSql :: Text -> Text
profileReputationSql profileSql =
  "SELECT jsonb_build_object('ratingAvg',round(avg(review.rating)::numeric,2),'ratingCount',count(review.id),'completedCount',(SELECT count(*) FROM directory_interaction interaction WHERE interaction.status='completed' AND (interaction.profile_a_id=" <> profileSql <> " OR interaction.profile_b_id=" <> profileSql <> "))) value FROM directory_review review WHERE review.subject_profile_id=" <> profileSql <> " AND review.status='published'"

createApplication user classifiedId idempotency request@ApplicationCreateRequest
  { applicantProfileId, message, portfolio, availability, proposedAmountMinor, amountMinor, currencyId, terms, expiresAt } = do
  requireProfileCapability user applicantProfileId "contact"
  requireAdult user
  validatePrivateMessage 10 message
  requireJsonArray "portfolio" portfolio
  let offeredAmount = amountMinor <|> proposedAmountMinor
      termsValue = fromMaybe (object []) terms
  requireJsonObject "terms" termsValue
  when (maybe False (<0) offeredAmount || (offeredAmount/=Nothing && currencyId==Nothing)) $
    throwError err400 {errBody="a non-negative amount requires currencyId"}
  applicationId <- reserveIdempotency user "classified.apply" idempotency request "application"
  existing <- jsonRows "SELECT to_jsonb(TRUE) FROM classified_application WHERE id=?" [toPersistValue applicationId]
  if not (null existing) then applicationSummary user applicationId else do
    allowed <- jsonRows "SELECT to_jsonb(TRUE) FROM classified JOIN directory_profile author ON author.id=classified.author_profile_id JOIN directory_profile applicant ON applicant.id=? LEFT JOIN directory_contact_preference preference ON preference.profile_id=author.id WHERE classified.id=? AND classified.status='published' AND classified.moderation_status='allowed' AND classified.expires_at>now() AND classified.author_profile_id<>applicant.id AND applicant.completeness_score>=coalesce(preference.minimum_profile_completeness,0) AND coalesce(preference.allow_classified_applications,TRUE) AND NOT EXISTS (SELECT 1 FROM directory_profile_block block WHERE (block.blocker_profile_id=classified.author_profile_id AND block.blocked_profile_id=applicant.id) OR (block.blocker_profile_id=applicant.id AND block.blocked_profile_id=classified.author_profile_id))" [toPersistValue applicantProfileId,toPersistValue classifiedId]
    when (null allowed) $ throwError err403 {errBody="this classified does not accept this application"}
    -- ADR 0134: marketplace admission rules.
    listing <- jsonOne err404 "SELECT jsonb_build_object('priceMode',price_mode,'budgetMaxMinor',budget_max_minor,'currencyId',currency_id,'slotsRemaining',slots_remaining,'offerDeadlineAt',offer_deadline_at,'expiresAt',expires_at,'derived',source_profile_id IS NOT NULL) FROM classified WHERE id=?" [toPersistValue classifiedId]
    when (valueBool "derived" listing) $ throwError err409 {errBody="artist profile listings do not take offers; contact the profile or invite it to a classified"}
    when (maybe True (<=0) (valueInt "slotsRemaining" listing)) $ throwError err409 {errBody="this classified has no slot left"}
    now <- liftIO getCurrentTime
    when (maybe False (<=now) (valueTime "offerDeadlineAt" listing)) $ throwError err409 {errBody="the offer deadline has passed"}
    mode <- maybe (throwError err500) pure (parsePriceMode (fromMaybe "open" (valueText "priceMode" listing)))
    let referenceAmount = valueInt64 "budgetMaxMinor" listing
        referenceCurrency = valueUuid "currencyId" listing
    unless (offerAmountAcceptable mode referenceAmount offeredAmount && (mode /= PriceFixed || currencyId == referenceCurrency)) $
      throwError err409 {errBody="this classified has a fixed price; the offer must accept that exact amount and currency"}
    -- An elapsed previous offer is persisted as expired so a replacement is
    -- admitted; accepted and open, unexpired offers still block duplicates.
    runDB $ rawExecute "UPDATE classified_application SET status='expired',updated_at=now(),version=version+1 WHERE classified_id=? AND applicant_profile_id=? AND status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant') AND offer_expires_at<=now()" [toPersistValue classifiedId,toPersistValue applicantProfileId]
    duplicates <- jsonRows ("SELECT to_jsonb(TRUE) FROM classified_application application WHERE application.classified_id=? AND application.applicant_profile_id=? AND (application.status IN ('accepted','conversation_open','converted') OR " <> activeOfferSql "application" <> ")") [toPersistValue classifiedId,toPersistValue applicantProfileId]
    unless (null duplicates) $ throwError err409 {errBody="this profile already has an active offer on this classified"}
    let deadline = valueTime "offerDeadlineAt" listing <|> valueTime "expiresAt" listing
        defaultExpiry = addUTCTime (7*24*60*60) now
        offerExpiry = minimumTime (fromMaybe defaultExpiry expiresAt) deadline
    when (offerExpiry <= now) $ throwError err400 {errBody="expiresAt must be in the future"}
    consumeRate user "application" 20
    let fingerprint = requestFingerprint request
    runDB $ do
      rawExecute "INSERT INTO classified_application(id,classified_id,applicant_profile_id,message,portfolio,availability_text,proposed_amount_minor,current_amount_minor,currency_id,terms,offer_expires_at,status,idempotency_key,request_fingerprint) VALUES (?,?,?,?,?::jsonb,?,?,?,?,?::jsonb,?,'submitted',?,?)"
        [toPersistValue applicationId,toPersistValue classifiedId,toPersistValue applicantProfileId,PersistText (T.strip message),PersistText (decodeJsonText portfolio),optionalText availability,optionalInt64 offeredAmount,optionalInt64 offeredAmount,optionalUuid currencyId,PersistText (decodeJsonText termsValue),toPersistValue offerExpiry,PersistText idempotency,PersistText fingerprint]
      rawExecute "INSERT INTO classified_offer_round(application_id,round_number,actor_role,kind,amount_minor,currency_id,terms,message,expires_at,created_by_party_id) VALUES (?,1,'applicant','initial',?,?,?::jsonb,?,?,?)"
        [toPersistValue applicationId,optionalInt64 offeredAmount,optionalUuid currencyId,PersistText (decodeJsonText termsValue),PersistText (T.strip message),toPersistValue offerExpiry,toPersistValue (auPartyId user)]
    notifyClassifiedAuthor classifiedId applicationId "marketplace.offer.new" "Nueva oferta" "Recibiste una oferta en tu anuncio."
    recordAuthenticatedEvent user "application_submitted" "application" (UUID.toText applicationId)
    applicationSummary user applicationId

-- A counter-offer from either participant (ADR 0134). Alternation and the
-- round limit come from TDF.Directory.Policy; the round and the offer's
-- current state change in one transaction.
counterOffer user applicationId idempotency request@OfferCounterRequest{amountMinor, currencyId, terms, message, expiresAt} = do
  role <- applicationParticipantRole user applicationId
  actor <- case role of
    "author" -> pure ActorAuthor
    "applicant" -> pure ActorApplicant
    _ -> throwError err403 {errBody="only the classified author or the offerer may counter"}
  requireAdult user
  when (amountMinor < 0) $ throwError err400 {errBody="amountMinor must be non-negative"}
  forM_ message (validatePrivateMessage 1)
  let termsValue = fromMaybe (object []) terms
  requireJsonObject "terms" termsValue
  requireMarketplaceOffers
  roundId <- reserveIdempotency user "offer.counter" idempotency request "offer_round"
  replayed <- jsonRows "SELECT to_jsonb(TRUE) FROM classified_offer_round WHERE id=?" [toPersistValue roundId]
  if not (null replayed) then getApplication user applicationId else do
    current <- jsonOne err404 "SELECT jsonb_build_object('status',application.status,'round',application.current_round,'lastActor',application.last_actor_role,'offerExpiresAt',application.offer_expires_at,'priceMode',classified.price_mode,'classifiedStatus',classified.status,'classifiedExpiresAt',NULLIF(classified.expires_at,'infinity'::timestamptz),'offerDeadlineAt',classified.offer_deadline_at,'classifiedId',classified.id,'applicantProfileId',application.applicant_profile_id,'authorProfileId',classified.author_profile_id) FROM classified_application application JOIN classified ON classified.id=application.classified_id WHERE application.id=?" [toPersistValue applicationId]
    status <- maybe (throwError err409 {errBody="unknown current application state"}) pure (valueText "status" current >>= parseApplicationStatus)
    now <- liftIO getCurrentTime
    when (maybe False (<=now) (valueTime "offerExpiresAt" current) && applicationIsOpen status) $ do
      runDB $ rawExecute "UPDATE classified_application SET status='expired',updated_at=now(),version=version+1 WHERE id=? AND status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant')" [toPersistValue applicationId]
      throwError err409 {errBody="the offer expired before this counter"}
    unless (valueText "classifiedStatus" current == Just "published") $ throwError err409 {errBody="the classified is no longer open"}
    when (maybe False (<=now) (valueTime "classifiedExpiresAt" current) || maybe False (<=now) (valueTime "offerDeadlineAt" current)) $
      throwError err409 {errBody="the classified no longer takes offers"}
    when (valueText "priceMode" current == Just "fixed") $ throwError err409 {errBody="fixed-price classifieds do not admit counter-offers"}
    unless (counterAllowedFor status actor) $ throwError err409 {errBody="it is not this participant's turn to counter"}
    let nextRound = fromMaybe 1 (valueInt "round" current) + 1
    -- The final round is reserved for the acceptance (I18).
    unless (counterRoundAllowed nextRound) $ throwError err409 {errBody="negotiation round limit reached; accept, reject or withdraw the current offer"}
    lastActor <- case valueText "lastActor" current of
      Just "author" -> pure ActorAuthor
      Just "applicant" -> pure ActorApplicant
      _ -> throwError err500
    unless (nextRoundActor lastActor == actor) $ throwError err409 {errBody="negotiation must alternate between participants"}
    let defaultExpiry = addUTCTime (7*24*60*60) now
        -- A counter never outlives the classified offer deadline or expiry.
        listingDeadline = case (valueTime "offerDeadlineAt" current, valueTime "classifiedExpiresAt" current) of
          (Just deadline, Just expiry) -> Just (min deadline expiry)
          (deadline, expiry) -> deadline <|> expiry
        roundExpiry = minimumTime (fromMaybe defaultExpiry expiresAt) listingDeadline
        newStatus = if actor == ActorAuthor then "countered_by_author" else "countered_by_applicant"
        actorText = if actor == ActorAuthor then "author" else "applicant"
    when (roundExpiry <= now) $ throwError err400 {errBody="expiresAt must be in the future"}
    consumeRate user "application" 20
    runDB $ do
      rawExecute "INSERT INTO classified_offer_round(id,application_id,round_number,actor_role,kind,amount_minor,currency_id,terms,message,expires_at,created_by_party_id) VALUES (?,?,?,?,'counter',?,?,?::jsonb,?,?,?)"
        [toPersistValue roundId,toPersistValue applicationId,PersistInt64 (fromIntegral nextRound),PersistText actorText,PersistInt64 amountMinor,toPersistValue currencyId,PersistText (decodeJsonText termsValue),optionalText (T.strip <$> message),toPersistValue roundExpiry,toPersistValue (auPartyId user)]
      rawExecute "UPDATE classified_application SET status=?,current_amount_minor=?,currency_id=?,current_round=?,last_actor_role=?,terms=CASE WHEN ?::jsonb='{}'::jsonb THEN terms ELSE ?::jsonb END,offer_expires_at=?,updated_at=now(),version=version+1 WHERE id=?"
        [PersistText newStatus,PersistInt64 amountMinor,toPersistValue currencyId,PersistInt64 (fromIntegral nextRound),PersistText actorText,PersistText (decodeJsonText termsValue),PersistText (decodeJsonText termsValue),toPersistValue roundExpiry,toPersistValue applicationId]
      rawExecute "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,previous_state,new_state,correlation_id,metadata) VALUES (?,'application.counter','application',?,?,?,?,jsonb_build_object('round',?::int,'amountMinor',?::bigint))"
        [toPersistValue (auPartyId user),PersistText (UUID.toText applicationId),PersistText (applicationStatusText status),PersistText newStatus,PersistText ("application-counter-"<>UUID.toText applicationId),PersistInt64 (fromIntegral nextRound),PersistInt64 amountMinor]
    let counterpart = if actor == ActorAuthor then valueUuid "applicantProfileId" current else valueUuid "authorProfileId" current
    forM_ counterpart $ \profileIdValue -> notifyOfferParticipant profileIdValue applicationId "marketplace.offer.countered" "Contraoferta recibida" "La otra parte propuso un nuevo monto para la oferta."
    recordAuthenticatedEvent user "offer_countered" "application" (UUID.toText applicationId)
    getApplication user applicationId

mergeObjects :: Value -> Value -> Value
mergeObjects (Object left) (Object right) = Object (KeyMap.union right left)
mergeObjects left _ = left

minimumTime :: UTCTime -> Maybe UTCTime -> UTCTime
minimumTime value Nothing = value
minimumTime value (Just other) = min value other

valueText :: Text -> Value -> Maybe Text
valueText key (Object values) = case KeyMap.lookup (Key.fromText key) values of
  Just (String value) -> Just value
  _ -> Nothing
valueText _ _ = Nothing

valueBool :: Text -> Value -> Bool
valueBool key (Object values) = case KeyMap.lookup (Key.fromText key) values of
  Just (Bool value) -> value
  _ -> False
valueBool _ _ = False

valueInt :: Text -> Value -> Maybe Int
valueInt key (Object values) = case KeyMap.lookup (Key.fromText key) values of
  Just (Number value) -> toBoundedInteger value
  _ -> Nothing
valueInt _ _ = Nothing

valueInt64 :: Text -> Value -> Maybe Int64
valueInt64 key (Object values) = case KeyMap.lookup (Key.fromText key) values of
  Just (Number value) -> toBoundedInteger value
  _ -> Nothing
valueInt64 _ _ = Nothing

valueTime :: Text -> Value -> Maybe UTCTime
valueTime key (Object values) = case KeyMap.lookup (Key.fromText key) values of
  Just value@(String _) -> case (Aeson.fromJSON value :: Aeson.Result UTCTime) of
    Aeson.Success time -> Just time
    _ -> Nothing
  _ -> Nothing
valueTime _ _ = Nothing

applicationStatusText :: ApplicationStatus -> Text
applicationStatusText status = case status of
  ApplicationSubmitted -> "submitted"
  ApplicationViewed -> "viewed"
  ApplicationShortlisted -> "shortlisted"
  ApplicationCounteredByAuthor -> "countered_by_author"
  ApplicationCounteredByApplicant -> "countered_by_applicant"
  ApplicationAccepted -> "accepted"
  ApplicationRejected -> "rejected"
  ApplicationWithdrawn -> "withdrawn"
  ApplicationExpired -> "expired"
  ApplicationConversationOpen -> "conversation_open"
  ApplicationConverted -> "converted"

changeApplicationStatus user applicationId DirectoryStatusRequest{status=newStatus,reason=statusReason} = do
  visible <- applicationParticipantRole user applicationId
  current <- jsonOne err404 "SELECT jsonb_build_object('status',application.status,'offerExpiresAt',application.offer_expires_at,'classifiedId',classified.id,'classifiedStatus',classified.status,'slots',classified.slots,'slotsRemaining',classified.slots_remaining,'expiresAt',classified.expires_at,'applicantProfileId',application.applicant_profile_id,'authorProfileId',classified.author_profile_id,'currentAmountMinor',application.current_amount_minor) FROM classified_application application JOIN classified ON classified.id=application.classified_id WHERE application.id=?" [toPersistValue applicationId]
  oldStatus <- maybe (throwError err500) pure (valueText "status" current)
  from <- maybe (throwError err409 {errBody="unknown current application state"}) pure (parseApplicationStatus oldStatus)
  to <- maybe (throwError err400 {errBody="invalid target application state"}) pure (parseApplicationStatus newStatus)
  when (to `elem` [ApplicationCounteredByAuthor, ApplicationCounteredByApplicant]) $
    throwError err400 {errBody="counter-offers are recorded through the counter endpoint"}
  now <- liftIO getCurrentTime
  when (applicationIsOpen from && maybe False (<=now) (valueTime "offerExpiresAt" current) && to /= ApplicationExpired && visible /= "admin") $ do
    runDB $ rawExecute "UPDATE classified_application SET status='expired',updated_at=now(),version=version+1 WHERE id=? AND status IN ('submitted','viewed','shortlisted','countered_by_author','countered_by_applicant')" [toPersistValue applicationId]
    throwError err409 {errBody="the offer expired before this transition"}
  unless (participantTransitionAllowed visible from to) $ throwError err403 {errBody="application transition is not allowed for this participant"}
  unless (allowedApplicationTransition from to) $ throwError err409 {errBody="undeclared application transition"}
  classifiedId <- maybe (throwError err500) pure (valueUuid "classifiedId" current)
  let slotsRemaining = fromMaybe 0 (valueInt "slotsRemaining" current)
      slotsTotal = fromMaybe 1 (valueInt "slots" current)
      actorText = if visible == "applicant" then "applicant" else "author"
      consumesSlot = to == ApplicationAccepted && from /= ApplicationAccepted
      releasesSlot = to == ApplicationWithdrawn && from `elem` [ApplicationAccepted, ApplicationConversationOpen]
  when consumesSlot $ do
    unless (valueText "classifiedStatus" current == Just "published") $ throwError err409 {errBody="the classified is no longer open"}
    when (maybe False (<=now) (valueTime "expiresAt" current)) $ throwError err409 {errBody="the classified has expired"}
    when (slotsAfterAccept slotsRemaining == Nothing) $ throwError err409 {errBody="this classified has no slot left"}
  -- The offer and its classified are re-read under row locks inside the
  -- transaction: a concurrent transition or acceptance that won the lock makes
  -- this one fail before any write, so one acceptance consumes one slot
  -- (ADR 0134 I19).
  outcome <- runDB $ do
    locked <- rawSql "SELECT application.status,classified.status,classified.slots_remaining,(classified.expires_at>now()) FROM classified_application application JOIN classified ON classified.id=application.classified_id WHERE application.id=? FOR UPDATE OF application,classified"
      [toPersistValue applicationId] :: SqlPersistT IO [(Single Text, Single Text, Single Int, Single Bool)]
    case locked of
      [(Single lockedStatus, Single lockedClassifiedStatus, Single lockedRemaining, Single lockedLive)]
        | lockedStatus /= oldStatus -> pure (Left err409 {errBody="the offer changed before this transition; reload and retry"})
        | consumesSlot && (lockedClassifiedStatus /= "published" || not lockedLive) -> pure (Left err409 {errBody="the classified is no longer open"})
        | consumesSlot && slotsAfterAccept lockedRemaining == Nothing -> pure (Left err409 {errBody="this classified has no slot left"})
        | otherwise -> do
            rawExecute "UPDATE classified_application SET status=?,withdrawn_at=CASE WHEN ?='withdrawn' THEN now() ELSE withdrawn_at END,accepted_at=CASE WHEN ?='accepted' THEN now() ELSE accepted_at END,accepted_amount_minor=CASE WHEN ?='accepted' THEN current_amount_minor ELSE accepted_amount_minor END,rejection_reason=CASE WHEN ?='rejected' THEN 'author_decision' ELSE rejection_reason END,updated_at=now(),version=version+1 WHERE id=? AND status=?"
              [PersistText newStatus,PersistText newStatus,PersistText newStatus,PersistText newStatus,PersistText newStatus,toPersistValue applicationId,PersistText oldStatus]
            rawExecute "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,previous_state,new_state,correlation_id,metadata) VALUES (?,'application.status','application',?,?,?,?,jsonb_build_object('reason',?::text,'participant',?::text))"
              [toPersistValue (auPartyId user),PersistText (UUID.toText applicationId),PersistText oldStatus,PersistText newStatus,PersistText ("application-status-"<>UUID.toText applicationId),optionalText statusReason,PersistText visible]
            when consumesSlot $ do
              -- ADR 0134 I18/I19: the accept round always closes the chain
              -- (counters stop at round 9) and exactly one slot is consumed.
              rawExecute "INSERT INTO classified_offer_round(application_id,round_number,actor_role,kind,amount_minor,currency_id,terms,created_by_party_id) SELECT id,least(current_round+1,10),?,'accept',current_amount_minor,currency_id,terms,? FROM classified_application WHERE id=? ON CONFLICT DO NOTHING"
                [PersistText actorText,toPersistValue (auPartyId user),toPersistValue applicationId]
              rawExecute "UPDATE classified_application SET current_round=least(current_round+1,10),last_actor_role=? WHERE id=?" [PersistText actorText,toPersistValue applicationId]
              rawExecute "UPDATE classified SET slots_remaining=slots_remaining-1,updated_at=now(),version=version+1 WHERE id=? AND slots_remaining>0" [toPersistValue classifiedId]
              let remaining = fromMaybe 0 (slotsAfterAccept lockedRemaining)
              when (remaining == 0) $ do
                rawExecute "UPDATE classified SET status='filled',closed_at=now() WHERE id=? AND status='published'" [toPersistValue classifiedId]
                rawExecute "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,previous_state,new_state,correlation_id,metadata) VALUES (?,'classified.status','classified',?,'published','filled',?,jsonb_build_object('reason','slots_filled'))" [toPersistValue (auPartyId user),PersistText (UUID.toText classifiedId),PersistText ("classified-status-"<>UUID.toText classifiedId)]
                closeOpenOffersDB user classifiedId "slots_filled"
                rawExecute "INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at) SELECT DISTINCT manager.account_party_id,'marketplace.classified.filled','Anuncio cubierto','Tu anuncio cubrió todos sus cupos.','directory_classified',?::text,FALSE,now() FROM directory_profile_manager manager WHERE manager.profile_id=? AND manager.active AND manager.can_contact" [PersistText (UUID.toText classifiedId),optionalUuid (valueUuid "authorProfileId" current)]
              refreshClassifiedDB classifiedId
            when releasesSlot $ do
              -- I19: the released slot reopens a filled, unexpired classified
              -- (filled -> published is declared in the database relation).
              rawExecute "UPDATE classified SET slots_remaining=?,status=CASE WHEN status='filled' AND expires_at>now() THEN 'published' ELSE status END,closed_at=CASE WHEN status='filled' AND expires_at>now() THEN NULL ELSE closed_at END,updated_at=now(),version=version+1 WHERE id=?"
                [PersistInt64 (fromIntegral (slotsAfterRelease slotsTotal lockedRemaining)),toPersistValue classifiedId]
              refreshClassifiedDB classifiedId
            pure (Right ())
      _ -> pure (Left err404)
  either throwError pure outcome
  let counterpart = if visible == "applicant" then valueUuid "authorProfileId" current else valueUuid "applicantProfileId" current
      notify kind title body = forM_ counterpart $ \profileIdValue -> notifyOfferParticipant profileIdValue applicationId kind title body
  case to of
    ApplicationAccepted | consumesSlot -> notify "marketplace.offer.accepted" "Oferta aceptada" "La oferta fue aceptada. Ya pueden coordinar los detalles."
    ApplicationRejected | from /= ApplicationRejected -> notify "marketplace.offer.rejected" "Oferta rechazada" "El autor del anuncio declinó la oferta."
    ApplicationWithdrawn | from /= ApplicationWithdrawn -> notify "marketplace.offer.withdrawn" "Oferta retirada" "La otra parte retiró su oferta."
    _ -> pure ()
  when (newStatus == "accepted" && consumesSlot) $
    recordAuthenticatedEvent user "offer_accepted" "application" (UUID.toText applicationId)
  when (newStatus == "conversation_open") $
    recordAuthenticatedEvent user "contact_accepted" "application" (UUID.toText applicationId)
  when (newStatus == "converted") $
    recordAuthenticatedEvent user "match_converted" "application" (UUID.toText applicationId)
  applicationSummary user applicationId

parseApplicationStatus :: Text -> Maybe ApplicationStatus
parseApplicationStatus value = lookup value
  [("submitted",ApplicationSubmitted),("viewed",ApplicationViewed),("shortlisted",ApplicationShortlisted),("countered_by_author",ApplicationCounteredByAuthor),("countered_by_applicant",ApplicationCounteredByApplicant),("accepted",ApplicationAccepted),("rejected",ApplicationRejected),("withdrawn",ApplicationWithdrawn),("expired",ApplicationExpired),("conversation_open",ApplicationConversationOpen),("converted",ApplicationConverted)]

applicationSummary user applicationId = getApplication user applicationId

applicationParticipantRole user applicationId = do
  roles <- jsonRows "SELECT to_jsonb(CASE WHEN applicant.account_party_id IS NOT NULL THEN 'applicant' WHEN author.account_party_id IS NOT NULL THEN 'author' ELSE 'none' END::text) FROM classified_application application JOIN classified ON classified.id=application.classified_id LEFT JOIN directory_profile_manager applicant ON applicant.profile_id=application.applicant_profile_id AND applicant.account_party_id=? AND applicant.active LEFT JOIN directory_profile_manager author ON author.profile_id=classified.author_profile_id AND author.account_party_id=? AND author.active WHERE application.id=?" [toPersistValue (auPartyId user),toPersistValue (auPartyId user),toPersistValue applicationId]
  case listToMaybe roles of Just (String role) | role/="none" -> pure role; _ | isDirectoryAdmin user -> pure "admin"; _ -> throwError err404

createInvitation user idempotency request@InvitationCreateRequest
  { senderProfileId, targetProfileId, classifiedId, message } = do
  requireProfileCapability user senderProfileId "contact"
  requireAdult user
  validatePrivateMessage 10 message
  invitationId <- reserveIdempotency user "invitation.create" idempotency request "invitation"
  existing <- jsonRows "SELECT to_jsonb(TRUE) FROM directory_invitation WHERE id=?" [toPersistValue invitationId]
  if not (null existing) then invitationSummary user invitationId else do
    permitted <- jsonRows "SELECT to_jsonb(TRUE) FROM directory_public_profile target JOIN directory_public_profile sender ON sender.id=? LEFT JOIN directory_contact_preference preference ON preference.profile_id=target.id WHERE target.id=? AND target.id<>sender.id AND sender.completeness_score>=coalesce(preference.minimum_profile_completeness,0) AND coalesce(preference.allow_direct_invitations,TRUE) AND NOT EXISTS (SELECT 1 FROM directory_profile_block block WHERE (block.blocker_profile_id=target.id AND block.blocked_profile_id=sender.id) OR (block.blocker_profile_id=sender.id AND block.blocked_profile_id=target.id)) AND (?::uuid IS NULL OR EXISTS(SELECT 1 FROM classified WHERE classified.id=?::uuid AND classified.author_profile_id=sender.id AND classified.status='published' AND classified.moderation_status='allowed' AND classified.expires_at>now()))" [toPersistValue senderProfileId,toPersistValue targetProfileId,optionalUuid classifiedId,optionalUuid classifiedId]
    when (null permitted) $ throwError err403 {errBody="target profile does not accept invitations"}
    consumeRate user "invitation" 10
    now <- liftIO getCurrentTime
    runDB $ rawExecute "INSERT INTO directory_invitation(id,sender_profile_id,target_profile_id,classified_id,message,status,idempotency_key,request_fingerprint,expires_at) VALUES (?,?,?,?,?,'pending',?,?,?)" [toPersistValue invitationId,toPersistValue senderProfileId,toPersistValue targetProfileId,optionalUuid classifiedId,PersistText (T.strip message),PersistText idempotency,PersistText (requestFingerprint request),toPersistValue (addUTCTime (30*24*60*60) now)]
    notifyProfile targetProfileId invitationId "directory.invitation" "Nueva invitación" "Un perfil te invitó a una oportunidad."
    recordAuthenticatedEvent user "invitation_sent" "invitation" (UUID.toText invitationId)
    invitationSummary user invitationId

listInvitations user = jsonRows
  "SELECT jsonb_build_object('id',invitation.id,'senderProfileId',invitation.sender_profile_id,'targetProfileId',invitation.target_profile_id,'classifiedId',invitation.classified_id,'message',invitation.message,'status',CASE WHEN invitation.status='pending' AND invitation.expires_at<=now() THEN 'expired' ELSE invitation.status END,'expiresAt',invitation.expires_at,'version',invitation.version,'participantRole',CASE WHEN sender_manager.account_party_id IS NOT NULL THEN 'sender' ELSE 'target' END,'senderProfile',jsonb_build_object('id',sender_profile.id,'name',sender_profile.public_name,'slug',sender_profile.slug),'targetProfile',jsonb_build_object('id',target_profile.id,'name',target_profile.public_name,'slug',target_profile.slug),'classified',CASE WHEN classified.id IS NULL THEN NULL ELSE jsonb_build_object('id',classified.id,'title',classified.title,'slug',classified.slug,'status',classified.status) END) FROM directory_invitation invitation JOIN directory_profile sender_profile ON sender_profile.id=invitation.sender_profile_id JOIN directory_profile target_profile ON target_profile.id=invitation.target_profile_id LEFT JOIN classified ON classified.id=invitation.classified_id LEFT JOIN directory_profile_manager sender_manager ON sender_manager.profile_id=invitation.sender_profile_id AND sender_manager.account_party_id=? AND sender_manager.active LEFT JOIN directory_profile_manager target_manager ON target_manager.profile_id=invitation.target_profile_id AND target_manager.account_party_id=? AND target_manager.active WHERE sender_manager.account_party_id IS NOT NULL OR target_manager.account_party_id IS NOT NULL ORDER BY invitation.created_at DESC,invitation.id"
  [toPersistValue (auPartyId user),toPersistValue (auPartyId user)]

changeInvitationStatus user invitationId DirectoryStatusRequest{status=newStatus} = do
  role <- invitationParticipantRole user invitationId
  expired <- jsonRows "SELECT to_jsonb(TRUE) FROM directory_invitation WHERE id=? AND status='pending' AND expires_at<=now()" [toPersistValue invitationId]
  unless (null expired) $ do
    runDB $ rawExecute "UPDATE directory_invitation SET status='expired',updated_at=now(),version=version+1 WHERE id=? AND status='pending'" [toPersistValue invitationId]
    throwError err409 {errBody="invitation expired before this transition"}
  current <- jsonOne err404 "SELECT to_jsonb(status) FROM directory_invitation WHERE id=?" [toPersistValue invitationId]
  oldStatus <- case current of String value -> pure value; _ -> throwError err500
  from <- maybe (throwError err409 {errBody="unknown current invitation state"}) pure (parseInvitationStatus oldStatus)
  to <- maybe (throwError err400 {errBody="invalid target invitation state"}) pure (parseInvitationStatus newStatus)
  let senderAllowed=Set.fromList ["withdrawn","conversation_open"]
      targetAllowed=Set.fromList ["accepted","declined","blocked","conversation_open","converted"]
  unless ((role=="sender" && newStatus `Set.member` senderAllowed) || (role=="target" && newStatus `Set.member` targetAllowed) || role=="admin") $ throwError err403 {errBody="invitation transition is not allowed"}
  unless (allowedInvitationTransition from to) $ throwError err409 {errBody="undeclared invitation transition"}
  runDB $ do
    rawExecute "UPDATE directory_invitation SET status=?,updated_at=now(),version=version+1 WHERE id=?" [PersistText newStatus,toPersistValue invitationId]
    when (newStatus=="blocked" && role=="target") $
      rawExecute "INSERT INTO directory_profile_block(blocker_profile_id,blocked_profile_id,created_by,reason) SELECT target_profile_id,sender_profile_id,?,'blocked_from_invitation' FROM directory_invitation WHERE id=? ON CONFLICT DO NOTHING" [toPersistValue (auPartyId user),toPersistValue invitationId]
  when (newStatus `elem` ["accepted","conversation_open"]) $
    recordAuthenticatedEvent user "contact_accepted" "invitation" (UUID.toText invitationId)
  when (newStatus == "converted") $
    recordAuthenticatedEvent user "match_converted" "invitation" (UUID.toText invitationId)
  invitationSummary user invitationId

parseInvitationStatus value = lookup value
  [("pending",InvitationPending),("accepted",InvitationAccepted),("declined",InvitationDeclined),("withdrawn",InvitationWithdrawn),("blocked",InvitationBlocked),("conversation_open",InvitationConversationOpen),("converted",InvitationConverted),("expired",InvitationExpired)]

invitationParticipantRole user invitationId = do
  rows <- jsonRows "SELECT to_jsonb(CASE WHEN sender.account_party_id IS NOT NULL THEN 'sender' WHEN target.account_party_id IS NOT NULL THEN 'target' ELSE 'none' END::text) FROM directory_invitation invitation LEFT JOIN directory_profile_manager sender ON sender.profile_id=invitation.sender_profile_id AND sender.account_party_id=? AND sender.active LEFT JOIN directory_profile_manager target ON target.profile_id=invitation.target_profile_id AND target.account_party_id=? AND target.active WHERE invitation.id=?" [toPersistValue (auPartyId user),toPersistValue (auPartyId user),toPersistValue invitationId]
  case listToMaybe rows of Just (String role) | role/="none" -> pure role; _ | isDirectoryAdmin user -> pure "admin"; _ -> throwError err404

invitationSummary user invitationId = do
  role <- invitationParticipantRole user invitationId
  jsonOne err404 "SELECT jsonb_build_object('id',invitation.id,'senderProfileId',invitation.sender_profile_id,'targetProfileId',invitation.target_profile_id,'classifiedId',invitation.classified_id,'message',invitation.message,'status',CASE WHEN invitation.status='pending' AND invitation.expires_at<=now() THEN 'expired' ELSE invitation.status END,'expiresAt',invitation.expires_at,'version',invitation.version,'participantRole',?::text,'senderProfile',jsonb_build_object('id',sender_profile.id,'name',sender_profile.public_name,'slug',sender_profile.slug),'targetProfile',jsonb_build_object('id',target_profile.id,'name',target_profile.public_name,'slug',target_profile.slug),'classified',CASE WHEN classified.id IS NULL THEN NULL ELSE jsonb_build_object('id',classified.id,'title',classified.title,'slug',classified.slug,'status',classified.status) END) FROM directory_invitation invitation JOIN directory_profile sender_profile ON sender_profile.id=invitation.sender_profile_id JOIN directory_profile target_profile ON target_profile.id=invitation.target_profile_id LEFT JOIN classified ON classified.id=invitation.classified_id WHERE invitation.id=?" [PersistText role,toPersistValue invitationId]

contactProfile user idempotency request@DirectoryContactRequest
  { senderProfileId, targetProfileId, contextKind, contextId, message } = do
  requireProfileCapability user senderProfileId "contact"
  requireAdult user
  validateContactContext user senderProfileId targetProfileId contextKind contextId
  validatePrivateMessage 1 message
  contextResource <- reserveIdempotency user "contact.create" idempotency request "conversation-context"
  existing <- jsonRows "SELECT jsonb_build_object('threadId',context.chat_thread_id,'senderPartyId',?::bigint,'contextId',context.idempotency_resource_id) FROM directory_conversation_context context WHERE context.idempotency_resource_id=?" [toPersistValue (auPartyId user),toPersistValue contextResource]
  case existing of
    prior:_ -> pure prior
    [] -> do
      permitted <- jsonRows "SELECT jsonb_build_object('targetPartyId',manager.account_party_id) FROM directory_public_profile target JOIN directory_profile target_state ON target_state.id=target.id JOIN directory_public_profile sender ON sender.id=? JOIN LATERAL (SELECT candidate.account_party_id FROM directory_profile_manager candidate WHERE candidate.profile_id=target.id AND candidate.active AND candidate.can_contact AND candidate.account_party_id<>? ORDER BY candidate.can_manage DESC,candidate.created_at,candidate.account_party_id LIMIT 1) manager ON TRUE LEFT JOIN directory_contact_preference preference ON preference.profile_id=target.id WHERE target.id=? AND target.id<>sender.id AND (?::text IN ('application','invitation') OR (target_state.public_contact_enabled AND sender.completeness_score>=coalesce(preference.minimum_profile_completeness,0) AND coalesce(preference.allow_profile_contacts,TRUE))) AND NOT EXISTS (SELECT 1 FROM directory_profile_block block WHERE (block.blocker_profile_id=target.id AND block.blocked_profile_id=sender.id) OR (block.blocker_profile_id=sender.id AND block.blocked_profile_id=target.id))" [toPersistValue senderProfileId,toPersistValue (auPartyId user),toPersistValue targetProfileId,PersistText contextKind]
      when (null permitted) $ throwError err403 {errBody="target profile does not accept contact or has no authorized contact manager"}
      consumeRate user "contact" 20
      thread <- jsonOne err500 "WITH recipient AS (SELECT manager.account_party_id target_party FROM directory_profile_manager manager WHERE manager.profile_id=? AND manager.active AND manager.can_contact AND manager.account_party_id<>? ORDER BY manager.can_manage DESC,manager.created_at,manager.account_party_id LIMIT 1), inserted AS (INSERT INTO chat_thread(dm_party_a,dm_party_b,created_at,updated_at) SELECT least(?::bigint,target_party),greatest(?::bigint,target_party),now(),now() FROM recipient ON CONFLICT(dm_party_a,dm_party_b) DO UPDATE SET updated_at=now() RETURNING id) SELECT jsonb_build_object('threadId',id,'senderPartyId',?::bigint,'contextId',?::uuid) FROM inserted" [toPersistValue targetProfileId,toPersistValue (auPartyId user),toPersistValue (auPartyId user),toPersistValue (auPartyId user),toPersistValue (auPartyId user),toPersistValue contextResource]
      threadId <- case thread of
        Object values -> case KeyMap.lookup "threadId" values of
          Just (Aeson.Number value) -> maybe (throwError err500) pure (toBoundedInteger value :: Maybe Int64)
          _ -> throwError err500
        _ -> throwError err500
      runDB $ do
        rawExecute "INSERT INTO directory_conversation_context(chat_thread_id,context_kind,context_id,idempotency_resource_id,created_by) VALUES (?,?,?,?,?)" [PersistInt64 threadId,PersistText contextKind,toPersistValue contextId,toPersistValue contextResource,toPersistValue (auPartyId user)]
        rawExecute "INSERT INTO chat_message(thread_id,sender_party_id,body,created_at) VALUES (?,?,?,now())" [PersistInt64 threadId,toPersistValue (auPartyId user),PersistText (T.strip message)]
      recordAuthenticatedEvent user "contact_started" contextKind (UUID.toText contextId)
      pure thread

listReviewEligibility user mAuthorProfileId =
  jsonRows
    ( "SELECT jsonb_build_object('interactionId',interaction.id,'interactionKind',interaction.interaction_kind,'verifiedAt',interaction.verified_at,"
   <> "'authorProfile',jsonb_build_object('id',author.id,'name',author.public_name,'slug',author.slug),"
   <> "'subjectProfile',jsonb_build_object('id',subject.id,'name',subject.public_name,'slug',subject.slug)) "
   <> "FROM directory_interaction interaction "
   <> "CROSS JOIN LATERAL (VALUES (interaction.profile_a_id,interaction.profile_b_id),(interaction.profile_b_id,interaction.profile_a_id)) pair(author_id,subject_id) "
   <> "JOIN directory_profile_manager manager ON manager.profile_id=pair.author_id AND manager.account_party_id=? AND manager.active AND manager.can_edit "
   <> "JOIN directory_public_profile author ON author.id=pair.author_id "
   <> "JOIN directory_public_profile subject ON subject.id=pair.subject_id "
   <> "WHERE interaction.status='completed' AND interaction.verified_at IS NOT NULL "
   <> "AND (?::uuid IS NULL OR author.id=?::uuid) "
   <> "AND NOT EXISTS (SELECT 1 FROM directory_review review WHERE review.interaction_id=interaction.id AND review.author_profile_id=author.id AND review.subject_profile_id=subject.id) "
   <> "AND NOT EXISTS (SELECT 1 FROM directory_profile_block block WHERE (block.blocker_profile_id=author.id AND block.blocked_profile_id=subject.id) OR (block.blocker_profile_id=subject.id AND block.blocked_profile_id=author.id)) "
   <> "ORDER BY interaction.verified_at DESC,interaction.id,author.id" )
    [toPersistValue (auPartyId user),optionalUuid mAuthorProfileId,optionalUuid mAuthorProfileId]

createReview user idempotency request@DirectoryReviewCreateRequest
  {interactionId,authorProfileId,subjectProfileId,rating,body} = do
  requireProfileCapability user authorProfileId "edit"
  requireAdult user
  when (authorProfileId==subjectProfileId) $
    throwError err400 {errBody="a profile cannot review itself"}
  when (rating<1 || rating>5) $
    throwError err400 {errBody="rating must be between 1 and 5"}
  validateReviewBody body
  reviewId <- reserveIdempotency user "review.create" idempotency request "review"
  existing <- jsonRows reviewSummarySql [toPersistValue reviewId]
  case existing of
    prior:_ -> pure prior
    [] -> do
      eligible <- jsonRows
        ( "SELECT to_jsonb(TRUE) FROM directory_interaction interaction "
       <> "JOIN directory_public_profile author ON author.id=? "
       <> "JOIN directory_public_profile subject ON subject.id=? "
       <> "WHERE interaction.id=? AND interaction.status='completed' AND interaction.verified_at IS NOT NULL "
       <> "AND ((interaction.profile_a_id=author.id AND interaction.profile_b_id=subject.id) OR (interaction.profile_b_id=author.id AND interaction.profile_a_id=subject.id)) "
       <> "AND NOT EXISTS (SELECT 1 FROM directory_profile_block block WHERE (block.blocker_profile_id=author.id AND block.blocked_profile_id=subject.id) OR (block.blocker_profile_id=subject.id AND block.blocked_profile_id=author.id))" )
        [toPersistValue authorProfileId,toPersistValue subjectProfileId,toPersistValue interactionId]
      when (null eligible) $
        throwError err409 {errBody="review requires an eligible verified completed interaction between public profiles"}
      duplicate <- jsonRows
        "SELECT to_jsonb(TRUE) FROM directory_review WHERE interaction_id=? AND author_profile_id=? AND subject_profile_id=?"
        [toPersistValue interactionId,toPersistValue authorProfileId,toPersistValue subjectProfileId]
      unless (null duplicate) $
        throwError err409 {errBody="this interaction has already been reviewed by the author profile"}
      consumeRate user "review" 10
      runDB $ do
        rawExecute
          "INSERT INTO directory_review(id,interaction_id,author_profile_id,subject_profile_id,rating,body,status) VALUES (?,?,?,?,?,?,'published') ON CONFLICT(interaction_id,author_profile_id,subject_profile_id) DO NOTHING"
          [toPersistValue reviewId,toPersistValue interactionId,toPersistValue authorProfileId,toPersistValue subjectProfileId,PersistInt64 (fromIntegral rating),optionalText (T.strip <$> body)]
        rawExecute
          "INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at) SELECT DISTINCT manager.account_party_id,'directory.review-created','Nueva reseña verificada','Un perfil con una interacción completada publicó una reseña.','directory_review',?::text,FALSE,now() FROM directory_profile_manager manager WHERE manager.profile_id=? AND manager.active AND manager.can_manage AND manager.account_party_id<>? AND EXISTS (SELECT 1 FROM directory_review review WHERE review.id=?)"
          [toPersistValue reviewId,toPersistValue subjectProfileId,toPersistValue (auPartyId user),toPersistValue reviewId]
      created <- jsonRows reviewSummarySql [toPersistValue reviewId]
      case created of
        value:_ -> pure value
        [] -> throwError err409 {errBody="the review was concurrently created with another idempotency key"}

reviewSummarySql :: Text
reviewSummarySql =
  "SELECT jsonb_build_object('id',review.id,'interactionId',review.interaction_id,'rating',review.rating,'body',review.body,'status',review.status,'createdAt',review.created_at,'verifiedInteractionType',interaction.interaction_kind,'authorProfile',jsonb_build_object('id',author.id,'name',author.public_name,'slug',author.slug),'subjectProfile',jsonb_build_object('id',subject.id,'name',subject.public_name,'slug',subject.slug)) FROM directory_review review JOIN directory_interaction interaction ON interaction.id=review.interaction_id JOIN directory_profile author ON author.id=review.author_profile_id JOIN directory_profile subject ON subject.id=review.subject_profile_id WHERE review.id=?"

validateReviewBody :: Maybe Text -> AppM ()
validateReviewBody Nothing = pure ()
validateReviewBody (Just value) =
  when (T.length (T.strip value)<10 || T.length value>2000 || T.any unsafeControl value) $
    throwError err400 {errBody="review body must contain 10-2000 safe characters"}
  where
    unsafeControl character = isControl character && character `notElem` ['\n','\r','\t']

listFavorites user mTargetKind = do
  targetKind <- traverse (either favoriteValidationError pure . canonicalFavoriteKind) mTargetKind
  jsonRows
    "SELECT jsonb_build_object('targetKind',favorite.target_kind,'targetId',favorite.target_id,'createdAt',favorite.created_at,'result',CASE WHEN document.entity_id IS NULL THEN NULL ELSE jsonb_build_object('type',document.entity_kind,'id',document.entity_id,'slug',document.slug,'title',document.title,'city',document.city_name) END) FROM directory_favorite favorite LEFT JOIN directory_public_search_document document ON document.entity_kind=favorite.target_kind AND document.entity_id=favorite.target_id WHERE favorite.account_party_id=? AND (CAST(? AS text) IS NULL OR favorite.target_kind=CAST(? AS text)) ORDER BY favorite.created_at DESC"
    [toPersistValue (auPartyId user), optionalText targetKind, optionalText targetKind]

addFavorite user rawTargetKind rawTargetId = do
  (targetKind, targetId) <- either favoriteValidationError pure (canonicalFavoriteTarget rawTargetKind rawTargetId)
  correlationId <- ("favorite-save-" <>) . UUID.toText <$> liftIO nextRandom
  status <- jsonRows
    ( "WITH requested AS (SELECT CAST(? AS text) AS target_kind,CAST(? AS text) AS target_id),"
      <> "eligible AS (SELECT requested.target_kind,requested.target_id FROM requested WHERE "
      <> favoriteTargetEligibilitySql targetKind
      <> "),"
      <> "inserted AS (INSERT INTO directory_favorite(account_party_id,target_kind,target_id) SELECT ?,eligible.target_kind,eligible.target_id FROM eligible WHERE NOT EXISTS (SELECT 1 FROM directory_favorite existing WHERE existing.account_party_id=? AND existing.target_kind=eligible.target_kind AND directory_canonical_favorite_target(existing.target_kind,existing.target_id)=eligible.target_id) ON CONFLICT DO NOTHING RETURNING target_kind,target_id),"
      <> "audited AS (INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,correlation_id,metadata) SELECT ?,'favorite.saved',inserted.target_kind,inserted.target_id,?,jsonb_build_object('source','directory.favorite.put','visibility','public-upcoming') FROM inserted RETURNING id) "
      <> "SELECT jsonb_build_object('targetExists',EXISTS(SELECT 1 FROM eligible),'newlySaved',EXISTS(SELECT 1 FROM inserted),'auditWritten',EXISTS(SELECT 1 FROM audited))"
    )
    [ PersistText targetKind
    , PersistText targetId
    , toPersistValue (auPartyId user)
    , toPersistValue (auPartyId user)
    , toPersistValue (auPartyId user)
    , PersistText correlationId
    ]
  unless (any favoriteTargetWasEligible status) $
    throwError err404 {errBody="Favorite target not found, not public, or no longer upcoming"}
  pure NoContent

removeFavorite user rawTargetKind rawTargetId = do
  targetKind <- either favoriteValidationError pure (canonicalFavoriteKind rawTargetKind)
  when (T.null (T.strip rawTargetId) || T.length rawTargetId > 160) $
    favoriteValidationError "invalid targetId"
  let canonicalTargetId = either (const Nothing) (Just . snd) (canonicalFavoriteTarget targetKind rawTargetId)
  runDB $ rawExecute
    "DELETE FROM directory_favorite WHERE account_party_id=? AND target_kind=? AND (target_id=? OR (CAST(? AS text) IS NOT NULL AND directory_canonical_favorite_target(target_kind,target_id)=CAST(? AS text)))"
    [ toPersistValue (auPartyId user)
    , PersistText targetKind
    , PersistText rawTargetId
    , optionalText canonicalTargetId
    , optionalText canonicalTargetId
    ]
  pure NoContent

favoriteValidationError :: Text -> AppM a
favoriteValidationError message = throwError err400 {errBody=BL.fromStrict (TE.encodeUtf8 message)}

favoriteTargetEligibilitySql :: Text -> Text
favoriteTargetEligibilitySql targetKind = case targetKind of
  "event" ->
    "EXISTS (SELECT 1 FROM directory_public_event event WHERE event.id=CAST(requested.target_id AS bigint) AND event.start_time>=CURRENT_TIMESTAMP)"
  "venue" ->
    "EXISTS (SELECT 1 FROM directory_public_venue venue WHERE venue.id=CAST(requested.target_id AS bigint))"
  "profile" ->
    "EXISTS (SELECT 1 FROM directory_public_profile profile WHERE profile.id=CAST(requested.target_id AS uuid))"
  "classified" ->
    "EXISTS (SELECT 1 FROM classified item JOIN directory_public_profile author ON author.id=item.author_profile_id WHERE item.id=CAST(requested.target_id AS uuid) AND item.status='published' AND item.moderation_status='allowed' AND item.expires_at>CURRENT_TIMESTAMP)"
  _ -> "FALSE"

favoriteTargetWasEligible :: Value -> Bool
favoriteTargetWasEligible (Object status) =
  KeyMap.lookup (Key.fromText "targetExists") status == Just (Bool True)
favoriteTargetWasEligible _ = False

listSavedSearches user = jsonRows "SELECT jsonb_build_object('id',id,'name',name,'canonicalQuery',canonical_query,'alertsEnabled',alerts_enabled,'alertFrequency',alert_frequency,'lastEvaluatedAt',last_evaluated_at,'createdAt',created_at) FROM directory_saved_search WHERE account_party_id=? ORDER BY created_at DESC,id" [toPersistValue (auPartyId user)]

-- Saved searches use the existing DTO, but their filters must have the same
-- types as public search. Do not persist arbitrary nested payloads/coordinates.
validateSavedSearchQuery :: Bool -> Value -> AppM ()
validateSavedSearchQuery alertsEnabled canonicalQuery = do
  let invalid = savedSearchError "La búsqueda contiene filtros no válidos. Revisa los filtros e inténtalo de nuevo."
      supportedFields = Set.fromList
        [ "q", "entityType", "profileKind", "cityId", "professionId", "serviceId"
        , "instrumentId", "genreId", "remote", "available", "dateFrom", "dateTo"
        , "latitude", "longitude", "radiusKm"
        ]
  case canonicalQuery of
    Object fields -> forM_ (KeyMap.toList fields) $ \(key, value) -> do
      let name = Key.toText key
      unless (name `Set.member` supportedFields) invalid
      when (name `elem` ["latitude", "longitude"] && value /= Null) $
        savedSearchError "No se pueden guardar coordenadas exactas. Quita la ubicación de esta búsqueda."
      case value of
        Null -> pure ()
        String text -> do
          if name == "q" then do
            _ <- validateQuery (Just text)
            pure ()
          else when (T.any isControl (if name `elem` ["entityType", "profileKind"] then T.strip text else text)) invalid
          when (name `elem` ["dateFrom", "dateTo"]) $
            case (parseQueryParam text :: Either Text UTCTime) of
              Left _ -> invalid
              Right _ -> pure ()
        Bool _ -> pure ()
        Number number -> do
          let radius = toRealFloat number :: Double
          -- jsonb rejects scales beyond16383 before its decoder can run.
          unless (name == "radiusKm" && not (isInfinite radius || isNaN radius)
            && radius > 0 && radius <= 500 && base10Exponent number >= -16383) invalid
        _ -> invalid
    _ -> invalid
  validation <- jsonOne err500
    "SELECT jsonb_build_object('valid',valid,'reason',reason) FROM directory_parse_saved_search(?::jsonb)"
    [PersistText (decodeJsonText canonicalQuery)]
  case validation of
    Object fields | KeyMap.lookup "valid" fields == Just (Bool True) -> pure ()
    Object fields | KeyMap.lookup "reason" fields == Just (String "unsupported_radius") ->
      when alertsEnabled $
        savedSearchError "Las alertas por radio aún no están disponibles. Quita el radio para guardar esta alerta."
    Object fields | KeyMap.lookup "reason" fields == Just (String "coordinates_not_allowed") ->
      savedSearchError "No se pueden guardar coordenadas exactas. Quita la ubicación de esta búsqueda."
    _ -> invalid
  where
    savedSearchError :: Text -> AppM ()
    savedSearchError message =
      throwError err400 {errBody=BL.fromStrict (TE.encodeUtf8 message)}

createSavedSearch user idempotency request@SavedSearchCreateRequest
  { name, canonicalQuery, alertsEnabled, alertFrequency } = do
  when (T.length (T.strip name)<1 || T.length name>120 || T.any isControl name) $
    throwError err400 {errBody="saved search name must contain 1-120 safe characters"}
  requireJsonObject "canonicalQuery" canonicalQuery
  validateSavedSearchQuery alertsEnabled canonicalQuery
  unless (alertFrequency `Set.member` Set.fromList ["instant","daily","weekly","off"]) $
    throwError err400 {errBody="invalid alertFrequency"}
  savedId <- reserveIdempotency user "saved-search.create" idempotency request "saved-search"
  let queryText=decodeJsonText canonicalQuery
  runDB $ rawExecute "INSERT INTO directory_saved_search(id,account_party_id,name,canonical_query,query_hash,alerts_enabled,alert_frequency) VALUES (?,?,?,?::jsonb,encode(digest(?,'sha256'),'hex'),?,?) ON CONFLICT(account_party_id,query_hash) DO UPDATE SET name=EXCLUDED.name,alerts_enabled=EXCLUDED.alerts_enabled,alert_frequency=EXCLUDED.alert_frequency,updated_at=now()" [toPersistValue savedId,toPersistValue (auPartyId user),PersistText (T.strip name),PersistText queryText,PersistText queryText,PersistBool alertsEnabled,PersistText alertFrequency]
  jsonOne err500 "SELECT jsonb_build_object('id',id,'name',name,'canonicalQuery',canonical_query,'alertsEnabled',alerts_enabled,'alertFrequency',alert_frequency) FROM directory_saved_search WHERE account_party_id=? AND query_hash=encode(digest(?,'sha256'),'hex')" [toPersistValue (auPartyId user),PersistText queryText]

-- Authenticated, naturally idempotent preparation, not a management grant.
-- Serialize on the existing core artist before looking up/inserting the twin.
-- Never publish a draft or disclose its private fields to the claimant.
prepareArtistClaim :: AuthedUser -> Int64 -> AppM Value
prepareArtistClaim _user partyId = do
  result <- runDB $ do
    artists <- rawSql
      "SELECT party.display_name FROM party JOIN artist_profile artist ON artist.artist_party_id=party.id WHERE party.id=? FOR UPDATE OF party"
      [PersistInt64 partyId] :: SqlPersistT IO [Single Text]
    case artists of
      [] -> pure Nothing
      Single artistName : _ -> do
        targets <- rawSql
          ( "SELECT resolved.id::text FROM directory_profile source JOIN directory_profile resolved "
         <> "ON resolved.id=coalesce(source.canonical_profile_id,source.id) "
         <> "WHERE source.subject_party_id=? AND source.profile_kind='artist' ORDER BY (source.id=resolved.id) DESC,source.updated_at DESC,source.id LIMIT 1" )
          [PersistInt64 partyId] :: SqlPersistT IO [Single Text]
        targetId <- case targets of
          Single existing : _ -> pure existing
          [] -> do
            freshId <- liftIO nextRandom
            let freshText = UUID.toText freshId
            rawExecute
              ( "INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,public_contact_enabled) "
             <> "VALUES (?,?,'artist',?,?,'draft','private',false)" )
              [toPersistValue freshId,PersistInt64 partyId,PersistText (T.take 160 artistName),PersistText ("artist-claim-" <> freshText)]
            pure freshText
        -- Existing suspended/archived/moderated records remain blocked; a new
        -- twin is not created as a way around their state.
        allowed <- rawSql
          "SELECT id::text FROM directory_profile WHERE id=?::uuid AND profile_kind='artist' AND profile_status NOT IN ('suspended','archived','merged') AND moderation_status<>'blocked'"
          [PersistText targetId] :: SqlPersistT IO [Single Text]
        pure $ case allowed of
          [] -> Nothing
          _ -> Just (object ["id" .= targetId, "name" .= artistName])
  maybe (throwError err404) pure result

createClaim user idempotency request@ClaimCreateRequest{profileId,claimType,evidence} = do
  unless (claimType `Set.member` Set.fromList ["profile","organization","venue","administration","credit"]) $
    throwError err400 {errBody="invalid claimType"}
  requireJsonArray "evidence" evidence
  claimId <- reserveIdempotency user "claim.create" idempotency request "claim"
  runDB $ rawExecute "INSERT INTO directory_claim(id,profile_id,claimant_party_id,claim_type,status,evidence,submitted_at) VALUES (?,?,?,?,'submitted',?::jsonb,now()) ON CONFLICT(id) DO NOTHING" [toPersistValue claimId,toPersistValue profileId,toPersistValue (auPartyId user),PersistText claimType,PersistText (decodeJsonText evidence)]
  jsonOne err500 "SELECT jsonb_build_object('id',id,'profileId',profile_id,'claimType',claim_type,'status',status,'submittedAt',submitted_at) FROM directory_claim WHERE id=? AND claimant_party_id=?" [toPersistValue claimId,toPersistValue (auPartyId user)]

createVerification user idempotency request@VerificationCreateRequest{profileId,verificationType,evidence} = do
  requireProfileCapability user profileId "edit"
  unless (verificationType `Set.member` Set.fromList ["identity","organization","venue","ownership","administration","professional_credit"]) $
    throwError err400 {errBody="invalid verificationType"}
  requireJsonArray "evidence" evidence
  verificationId <- reserveIdempotency user "verification.create" idempotency request "verification"
  runDB $ rawExecute "INSERT INTO directory_verification(id,profile_id,verification_type,status,evidence) VALUES (?,?,?,'submitted',?::jsonb) ON CONFLICT(id) DO NOTHING" [toPersistValue verificationId,toPersistValue profileId,PersistText verificationType,PersistText (decodeJsonText evidence)]
  jsonOne err500 "SELECT jsonb_build_object('id',id,'profileId',profile_id,'verificationType',verification_type,'status',status,'createdAt',created_at) FROM directory_verification WHERE id=?" [toPersistValue verificationId]

createReport user idempotency request@ReportCreateRequest{targetKind,targetId,reasonCode,details} = do
  validateReportTarget targetKind targetId
  when (T.length (T.strip reasonCode)<2 || T.length reasonCode>120 || T.any isControl reasonCode) $
    throwError err400 {errBody="invalid reasonCode"}
  when (maybe False (\value -> T.length (T.strip value)<10 || T.length value>3000 || T.any isControl value) details) $
    throwError err400 {errBody="report details must contain 10-3000 safe characters"}
  reportId <- reserveIdempotency user "report.create" idempotency request "report"
  runDB $ rawExecute "INSERT INTO directory_moderation_report(id,reporter_party_id,target_kind,target_id,reason_code,details,status) VALUES (?,?,?,?,?,?,'open') ON CONFLICT(id) DO NOTHING" [toPersistValue reportId,toPersistValue (auPartyId user),PersistText targetKind,PersistText targetId,PersistText reasonCode,optionalText details]
  runDB $ rawExecute "INSERT INTO directory_moderation_case(target_kind,target_id,status,priority) VALUES (?,?,'open','normal') ON CONFLICT(target_kind,target_id) WHERE status IN ('open','triaged','under_review','actioned','appealed','appeal_review') DO NOTHING" [PersistText targetKind,PersistText targetId]
  recordAuthenticatedEvent user "report_created" targetKind targetId
  jsonOne err500 "SELECT jsonb_build_object('id',id,'targetKind',target_kind,'targetId',target_id,'reasonCode',reason_code,'status',status,'createdAt',created_at) FROM directory_moderation_report WHERE id=? AND reporter_party_id=?" [toPersistValue reportId,toPersistValue (auPartyId user)]

validateReportTarget :: Text -> Text -> AppM ()
validateReportTarget kind identifier = do
  unless (kind `Set.member` Set.fromList ["profile","classified","application","invitation","event","venue","message","review"]) $
    throwError err400 {errBody="invalid report targetKind"}
  when (T.null (T.strip identifier) || T.length identifier>160) $
    throwError err400 {errBody="invalid report targetId"}

-- ADR 0134: the marketplace offer flow (amounts, counters, slot accounting)
-- ships behind the `marketplace.offers` runtime flag, read from the same
-- revenue_feature_flag table as the other commerce gates. Merging code never
-- enables it.
marketplaceFeatureEnvironment :: IO Text
marketplaceFeatureEnvironment = do
  raw <- lookupEnv "APP_ENV"
  pure $ case fmap (T.toLower . T.strip . T.pack) raw of
    Just "production" -> "production"
    Just "prod" -> "production"
    Just "staging" -> "staging"
    _ -> "sandbox"

marketplaceFeatureEnabled :: Text -> AppM Bool
marketplaceFeatureEnabled flag = do
  environment <- liftIO marketplaceFeatureEnvironment
  rows <- runDB (rawSql
    "SELECT enabled FROM revenue_feature_flag WHERE flag_key=? AND environment=?"
    [PersistText flag, PersistText environment] :: SqlPersistT IO [Single Bool])
  pure (rows == [Single True])

requireMarketplaceOffers :: AppM ()
requireMarketplaceOffers = do
  enabled <- marketplaceFeatureEnabled "marketplace.offers"
  unless enabled $ throwError err503
    { errBody = "Feature marketplace.offers is not enabled in this environment" }

requireDirectoryAdmin :: AuthedUser -> AppM ()
requireDirectoryAdmin user = unless (isDirectoryAdmin user) (throwError err403 {errBody="directory administration requires the Admin role and module"})

listAdminClaims user = do
  requireDirectoryAdmin user
  jsonRows "SELECT jsonb_build_object('id',claim.id,'profileId',claim.profile_id,'profileName',profile.public_name,'claimantPartyId',claim.claimant_party_id,'claimType',claim.claim_type,'status',claim.status,'evidence',claim.evidence,'submittedAt',claim.submitted_at,'reviewerNotes',claim.reviewer_notes) FROM directory_claim claim JOIN directory_profile profile ON profile.id=claim.profile_id ORDER BY claim.created_at DESC,claim.id" []

changeClaimStatus user claimId DirectoryStatusRequest{status=newStatus,reason=statusReason} = do
  requireDirectoryAdmin user
  -- AUTHORITY-097: revoking an approved claim requires a stated reason.
  when (newStatus == "revoked" && maybe True (T.null . T.strip) statusReason) $
    throwError err400 {errBody="claim revocation requires a reason"}
  result <- runDB $ do
    current <- rawSql
      "SELECT status,claimant_party_id FROM directory_claim WHERE id=? FOR UPDATE"
      [toPersistValue claimId] :: SqlPersistT IO [(Single Text, Single Int64)]
    case current of
      [] -> pure (Left err404)
      [(Single oldStatus, Single claimant)]
        | claimant == fromSqlKey (auPartyId user) ->
            pure (Left err403 {errBody="claim review requires a distinct administrator"})
        | not (allowedClaimTransition oldStatus newStatus) ->
            pure (Left err409 {errBody="undeclared claim transition"})
        | otherwise -> do
            -- An identical-state retry is observational: it cannot rewrite the
            -- reviewer or resurrect a manager grant revoked after approval.
            when (oldStatus /= newStatus) $ do
              -- Grants created by the claim are withdrawn before the status
              -- changes: the database refuses a revoked claim with an active grant.
              when (newStatus=="revoked") $ rawExecute "UPDATE directory_profile_manager SET active=FALSE,revoked_at=now(),version=version+1 WHERE source_claim_id=? AND active"
                [toPersistValue claimId]
              rawExecute "UPDATE directory_claim SET status=?,reviewer_party_id=?,reviewer_notes=?,reviewed_at=CASE WHEN ? IN ('approved','rejected','revoked') THEN now() ELSE reviewed_at END,updated_at=now(),version=version+1 WHERE id=?"
                [PersistText newStatus,toPersistValue (auPartyId user),optionalText statusReason,PersistText newStatus,toPersistValue claimId]
              when (newStatus=="approved") $ rawExecute "INSERT INTO directory_profile_manager(profile_id,account_party_id,can_view_private,can_edit,can_publish,can_contact,can_manage,active,granted_by,source_claim_id) SELECT profile_id,claimant_party_id,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,?,id FROM directory_claim WHERE id=? AND status='approved' ON CONFLICT(profile_id,account_party_id) DO UPDATE SET active=TRUE,can_view_private=TRUE,can_edit=TRUE,can_publish=TRUE,can_contact=TRUE,can_manage=TRUE,granted_by=EXCLUDED.granted_by,source_claim_id=EXCLUDED.source_claim_id,revoked_at=NULL,version=directory_profile_manager.version+1"
                [toPersistValue (auPartyId user),toPersistValue claimId]
              rawExecute
                "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,previous_state,new_state,correlation_id,metadata) SELECT ?,?,'claim',id::text,?,?,('claim-review-'||id::text||'-'||version::text),jsonb_build_object('version',version,'reason',?::text) FROM directory_claim WHERE id=?"
                [toPersistValue (auPartyId user),PersistText (if newStatus=="revoked" then "claim.revoked" else "claim.reviewed"),PersistText oldStatus,PersistText newStatus,optionalText statusReason,toPersistValue claimId]
            rows <- rawSql
              "SELECT jsonb_build_object('id',id,'profileId',profile_id,'claimantPartyId',claimant_party_id,'claimType',claim_type,'status',status,'reviewedAt',reviewed_at) FROM directory_claim WHERE id=?"
              [toPersistValue claimId] :: SqlPersistT IO [Single CMS.AesonValue]
            case rows of
              [Single value] -> pure (Right (CMS.unAesonValue value))
              _ -> liftIO (fail "Claim receipt missing inside review transaction")
      _ -> pure (Left err500)
  either throwError pure result

listAdminVerifications user = do
  requireDirectoryAdmin user
  jsonRows "SELECT jsonb_build_object('id',verification.id,'profileId',verification.profile_id,'profileName',profile.public_name,'verificationType',verification.verification_type,'status',verification.status,'evidence',verification.evidence,'createdAt',verification.created_at,'verifiedAt',verification.verified_at) FROM directory_verification verification JOIN directory_profile profile ON profile.id=verification.profile_id ORDER BY verification.created_at DESC,verification.id" []

changeVerificationStatus user verificationId DirectoryStatusRequest{status=newStatus,reason=statusReason} = do
  requireDirectoryAdmin user
  unless (newStatus `Set.member` Set.fromList ["under_review","verified","rejected","expired","revoked"]) $ throwError err400 {errBody="invalid verification status"}
  runDB $ rawExecute "UPDATE directory_verification SET status=?,reviewer_party_id=?,reviewer_notes=?,verified_by=CASE WHEN ?='verified' THEN ? ELSE verified_by END,verified_at=CASE WHEN ?='verified' THEN now() ELSE verified_at END,revoked_at=CASE WHEN ?='revoked' THEN now() ELSE revoked_at END,updated_at=now(),version=version+1 WHERE id=?" [PersistText newStatus,toPersistValue (auPartyId user),optionalText statusReason,PersistText newStatus,toPersistValue (auPartyId user),PersistText newStatus,PersistText newStatus,toPersistValue verificationId]
  jsonOne err404 "SELECT jsonb_build_object('id',id,'profileId',profile_id,'verificationType',verification_type,'status',status,'verifiedAt',verified_at) FROM directory_verification WHERE id=?" [toPersistValue verificationId]

listModerationQueue user = do
  requireDirectoryAdmin user
  jsonRows "SELECT jsonb_build_object('id',case_record.id,'targetKind',case_record.target_kind,'targetId',case_record.target_id,'status',case_record.status,'priority',case_record.priority,'assignedTo',case_record.assigned_to,'reports',coalesce((SELECT jsonb_agg(jsonb_build_object('id',report.id,'reasonCode',report.reason_code,'details',report.details,'createdAt',report.created_at)) FROM directory_moderation_report report WHERE report.target_kind=case_record.target_kind AND report.target_id=case_record.target_id),'[]'::jsonb),'createdAt',case_record.created_at,'updatedAt',case_record.updated_at,'version',case_record.version) FROM directory_moderation_case case_record ORDER BY CASE case_record.priority WHEN 'urgent' THEN 0 WHEN 'high' THEN 1 WHEN 'normal' THEN 2 ELSE 3 END,case_record.created_at,case_record.id" []

createModerationDecision user caseId idempotency request@ModerationDecisionRequest{decision,reasonCode,notes} = do
  requireDirectoryAdmin user
  unless (decision `Set.member` Set.fromList ["dismiss","warn","pause","remove","suspend","close"]) $ throwError err400 {errBody="invalid or unsupported moderation decision"}
  when (T.length (T.strip notes)<10 || T.length notes>5000) $ throwError err400 {errBody="moderation notes must contain 10-5000 characters"}
  decisionId <- reserveIdempotency user "moderation.decision" idempotency request "moderation-decision"
  target <- jsonOne err404 "SELECT jsonb_build_object('kind',target_kind,'id',target_id) FROM directory_moderation_case WHERE id=?" [toPersistValue caseId]
  runDB $ do
    rawExecute "INSERT INTO directory_moderation_decision(id,case_id,decision,reason_code,notes,actor_party_id) VALUES (?,?,?,?,?,?) ON CONFLICT(id) DO NOTHING" [toPersistValue decisionId,toPersistValue caseId,PersistText decision,PersistText reasonCode,PersistText (T.strip notes),toPersistValue (auPartyId user)]
    rawExecute "UPDATE directory_moderation_case SET status=CASE WHEN ? IN ('dismiss','close') THEN 'closed' ELSE 'actioned' END,updated_at=now(),closed_at=CASE WHEN ? IN ('dismiss','close') THEN now() ELSE closed_at END,version=version+1 WHERE id=?" [PersistText decision,PersistText decision,toPersistValue caseId]
    applyModerationTarget target decision
  recordAuthenticatedEvent user "moderation_action" "moderation_case" (UUID.toText caseId)
  jsonOne err500 "SELECT jsonb_build_object('id',id,'caseId',case_id,'decision',decision,'reasonCode',reason_code,'notes',notes,'createdAt',created_at) FROM directory_moderation_decision WHERE id=?" [toPersistValue decisionId]

mergeProfiles user idempotency request@ProfileMergeRequest{sourceProfileId,targetProfileId,reason=mergeReason} = do
  requireDirectoryAdmin user
  when (sourceProfileId == targetProfileId) $ throwError err400 {errBody="source and target profiles must differ"}
  when (T.length (T.strip mergeReason)<10 || T.length mergeReason>2000) $ throwError err400 {errBody="merge reason must contain 10-2000 characters"}
  mergeId <- reserveIdempotency user "profile.merge" idempotency request "profile-merge"
  result <- jsonOne err409
    "SELECT directory_execute_profile_merge(?,?,?,?,?)"
    [toPersistValue mergeId,toPersistValue sourceProfileId,toPersistValue targetProfileId,toPersistValue (auPartyId user),PersistText (T.strip mergeReason)]
  recordAuthenticatedEvent user "moderation_action" "profile_merge" (UUID.toText mergeId)
  pure result

applyModerationTarget :: Value -> Text -> SqlPersistT IO ()
applyModerationTarget (Object values) action = case (KeyMap.lookup "kind" values,KeyMap.lookup "id" values) of
  (Just (String "profile"),Just (String targetId)) | action `elem` ["pause","remove","suspend"] -> do
    rawExecute "UPDATE directory_profile SET profile_status=CASE WHEN ?='pause' THEN 'paused' ELSE 'suspended' END,moderation_status=CASE WHEN ?='pause' THEN moderation_status ELSE 'blocked' END,updated_at=now(),version=version+1 WHERE id::text=?" [PersistText action,PersistText action,PersistText targetId]
    rawExecute "DELETE FROM directory_search_document WHERE entity_kind='profile' AND entity_id=?" [PersistText targetId]
  (Just (String "classified"),Just (String targetId)) | action `elem` ["pause","remove"] -> do
    -- A derived listing's paused state belongs to its source profile (the next
    -- profile sync would republish it), so a moderator pause is recorded as the
    -- durable moderated state instead.
    rawExecute "UPDATE classified SET status=CASE WHEN ?='pause' AND source_profile_id IS NULL THEN 'paused' ELSE 'moderated' END,moderation_status=CASE WHEN ?='pause' THEN moderation_status ELSE 'blocked' END,updated_at=now() WHERE id::text=?" [PersistText action,PersistText action,PersistText targetId]
    rawExecute "DELETE FROM directory_search_document WHERE entity_kind='classified' AND entity_id=?" [PersistText targetId]
  (Just (String "review"),Just (String targetId)) | action `elem` ["pause","remove"] -> do
    rawExecute "UPDATE directory_review SET status=CASE WHEN ?='pause' THEN 'hidden' ELSE 'removed' END,updated_at=now() WHERE id::text=?" [PersistText action,PersistText targetId]
    rawExecute "UPDATE experience_review SET status=CASE WHEN ?='pause' THEN 'hidden' ELSE 'removed' END,updated_at=now() WHERE id::text=?" [PersistText action,PersistText targetId]
  _ -> pure ()
applyModerationTarget _ _ = pure ()

requireProfileCapability :: AuthedUser -> UUID -> Text -> AppM ()
requireProfileCapability user profileId capability = do
  rows <- jsonRows ("SELECT to_jsonb(TRUE) FROM directory_profile_manager WHERE profile_id=? AND account_party_id=? AND active AND "<>column<>"=TRUE") [toPersistValue profileId,toPersistValue (auPartyId user)]
  when (null rows) $ throwError err404 {errBody="profile not found or not authorized"}
  where column=case capability of "edit"->"can_edit";"publish"->"can_publish";"contact"->"can_contact";_ -> "can_manage"

requireClassifiedAuthor :: AuthedUser -> UUID -> AppM UUID
requireClassifiedAuthor user classifiedId = do
  rows <- jsonRows "SELECT to_jsonb(classified.author_profile_id::text) FROM classified JOIN directory_profile_manager manager ON manager.profile_id=classified.author_profile_id WHERE classified.id=? AND manager.account_party_id=? AND manager.active AND manager.can_publish" [toPersistValue classifiedId,toPersistValue (auPartyId user)]
  case listToMaybe rows of Just (String value) -> maybe (throwError err500) pure (UUID.fromText value); _ -> throwError err404 {errBody="classified not found or not authorized"}

ageAssurance user = do
  rows <- jsonRows "SELECT to_jsonb(assurance_status) FROM directory_age_assurance WHERE account_party_id=?" [toPersistValue (auPartyId user)]
  pure $ case listToMaybe rows of Just (String value)->value; _->"unknown"

requireAdult user = ageAssurance user >>= \assurance -> unless (minorMayPublishOrRespond assurance) (throwError err403 {errBody="age assurance or approved guardian consent is required"})

reserveIdempotency :: ToJSON request => AuthedUser -> Text -> Text -> request -> Text -> AppM UUID
reserveIdempotency user operation key request resourceKind = do
  when (T.length key<8 || T.length key>160 || T.any isControl key) $ throwError err400 {errBody="Idempotency-Key must contain 8-160 safe characters"}
  let fingerprint=requestFingerprint request
  candidateId <- liftIO nextRandom
  runDB $ rawExecute "INSERT INTO directory_idempotency(actor_party_id,operation,idempotency_key,request_fingerprint,resource_kind,resource_id,expires_at) VALUES (?,?,?,?,?,?,now()+interval '24 hours') ON CONFLICT(actor_party_id,operation,idempotency_key) DO UPDATE SET request_fingerprint=EXCLUDED.request_fingerprint,resource_kind=EXCLUDED.resource_kind,resource_id=EXCLUDED.resource_id,created_at=now(),expires_at=EXCLUDED.expires_at WHERE directory_idempotency.expires_at<=now()" [toPersistValue (auPartyId user),PersistText operation,PersistText key,PersistText fingerprint,PersistText resourceKind,PersistText (UUID.toText candidateId)]
  stored <- jsonOne err500 "SELECT jsonb_build_object('fingerprint',request_fingerprint,'resourceId',resource_id) FROM directory_idempotency WHERE actor_party_id=? AND operation=? AND idempotency_key=?" [toPersistValue (auPartyId user),PersistText operation,PersistText key]
  case stored of
    Object values -> case (KeyMap.lookup "fingerprint" values,KeyMap.lookup "resourceId" values) of
      (Just (String previous),Just (String resourceId)) | previous==fingerprint ->
        maybe (throwError err500) pure (UUID.fromText resourceId)
      _ -> throwError err409 {errBody="Idempotency-Key was already used with a different request"}
    _ -> throwError err500

requestFingerprint :: ToJSON request => request -> Text
requestFingerprint request = T.pack (show (hash (BL.toStrict (encode request)) :: Digest SHA256))

decodeJsonText :: Value -> Text
decodeJsonText = TE.decodeUtf8 . BL.toStrict . encode

requireJsonArray :: Text -> Value -> AppM ()
requireJsonArray _ (Array _) = pure ()
requireJsonArray field _ = throwError err400 {errBody=BL.fromStrict (TE.encodeUtf8 (field<>" must be an array"))}

requireJsonObject :: Text -> Value -> AppM ()
requireJsonObject _ (Object _) = pure ()
requireJsonObject field _ = throwError err400 {errBody=BL.fromStrict (TE.encodeUtf8 (field<>" must be an object"))}

consumeRate :: AuthedUser -> Text -> Int64 -> AppM ()
consumeRate user action maxCount = do
  rows <- jsonRows "WITH current AS (INSERT INTO directory_rate_limit(scope,subject_hash,window_started_at,count,updated_at) VALUES (?,encode(digest(?::text,'sha256'),'hex'),date_trunc('day',now()),1,now()) ON CONFLICT(scope,subject_hash,window_started_at) DO UPDATE SET count=directory_rate_limit.count+1,updated_at=now() RETURNING count) SELECT jsonb_build_object('allowed',count<=?) FROM current" [PersistText action,PersistText (T.pack (show (partyNumber user))),PersistInt64 maxCount]
  case listToMaybe rows of Just (Object values) | KeyMap.lookup "allowed" values==Just (Bool True) -> pure (); _ -> throwError err429 {errBody="rate limit exceeded"}

notifyClassifiedAuthor classifiedId applicationId notificationType notificationTitle notificationBody = runDB $ rawExecute "INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at) SELECT DISTINCT manager.account_party_id,?,?,?,'directory_application',?::text,FALSE,now() FROM classified JOIN directory_profile_manager manager ON manager.profile_id=classified.author_profile_id AND manager.active AND manager.can_contact WHERE classified.id=?" [PersistText notificationType,PersistText notificationTitle,PersistText notificationBody,toPersistValue applicationId,toPersistValue classifiedId]

notifyOfferParticipant profileIdValue applicationId notificationType notificationTitle notificationBody = runDB $ rawExecute "INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at) SELECT DISTINCT manager.account_party_id,?,?,?,'directory_application',?::text,FALSE,now() FROM directory_profile_manager manager WHERE manager.profile_id=? AND manager.active AND manager.can_contact" [PersistText notificationType,PersistText notificationTitle,PersistText notificationBody,toPersistValue applicationId,toPersistValue profileIdValue]

notifyProfile profileIdValue invitationId notificationType notificationTitle notificationBody = runDB $ rawExecute "INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at) SELECT DISTINCT manager.account_party_id,?,?,?,'directory_invitation',?::text,FALSE,now() FROM directory_profile_manager manager WHERE manager.profile_id=? AND manager.active AND manager.can_contact" [PersistText notificationType,PersistText notificationTitle,PersistText notificationBody,toPersistValue invitationId,toPersistValue profileIdValue]

isDirectoryAdmin user = Admin `elem` auRoles user && hasModuleAccess ModuleAdmin user

refreshProfileDB :: UUID -> SqlPersistT IO ()
refreshProfileDB profileId = do
  _ <- rawSql "SELECT directory_refresh_profile_search(?) IS NULL" [toPersistValue profileId] :: SqlPersistT IO [Single Bool]
  pure ()

refreshClassifiedDB :: UUID -> SqlPersistT IO ()
refreshClassifiedDB classifiedId = do
  _ <- rawSql "SELECT directory_refresh_classified_search(?) IS NULL" [toPersistValue classifiedId] :: SqlPersistT IO [Single Bool]
  pure ()

recordSearchAnalytics
  :: Text -> Maybe Text -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe UUID
  -> Maybe UUID -> Maybe Bool -> Maybe Bool -> Int -> AppM ()
recordSearchAnalytics query entityType cityIdValue professionIdValue serviceIdValue instrumentIdValue genreIdValue remoteValue availableValue resultCount =
  runDB $ do
    rawExecute
      "INSERT INTO directory_analytics_event(event_name,city_id,properties) VALUES ('search_submitted',?,jsonb_build_object('queryHash',encode(digest(directory_normalize_text(?),'sha256'),'hex'),'queryLength',length(?::text),'entityType',?::text,'professionId',?::uuid,'serviceId',?::uuid,'instrumentId',?::uuid,'genreId',?::uuid,'remote',?::boolean,'available',?::boolean,'resultCount',?::bigint))"
      [ optionalUuid cityIdValue, PersistText query, PersistText query, optionalText entityType
      , optionalUuid professionIdValue, optionalUuid serviceIdValue, optionalUuid instrumentIdValue
      , optionalUuid genreIdValue, maybe PersistNull PersistBool remoteValue
      , maybe PersistNull PersistBool availableValue, PersistInt64 (fromIntegral resultCount)
      ]
    when (resultCount == 0) $
      rawExecute
        "INSERT INTO directory_analytics_event(event_name,city_id,properties) VALUES ('search_zero_results',?,jsonb_build_object('queryHash',encode(digest(directory_normalize_text(?),'sha256'),'hex'),'queryLength',length(?::text),'entityType',?::text))"
        [optionalUuid cityIdValue,PersistText query,PersistText query,optionalText entityType]

recordAuthenticatedEvent :: AuthedUser -> Text -> Text -> Text -> AppM ()
recordAuthenticatedEvent user eventName entityKind entityId =
  runDB $ rawExecute
    "INSERT INTO directory_analytics_event(actor_scope_hash,event_name,entity_kind,entity_id_hash,properties) VALUES (encode(digest(?::text,'sha256'),'hex'),?,?,encode(digest(?::text,'sha256'),'hex'),'{}'::jsonb)"
    [PersistText (T.pack (show (partyNumber user))),PersistText eventName,PersistText entityKind,PersistText entityId]

validatePrivateMessage :: Int -> Text -> AppM ()
validatePrivateMessage minimumLength value =
  when (T.length (T.strip value) < minimumLength || T.length value > 5000 || T.any unsafeControl value) $
    throwError err400 {errBody="message length or characters are invalid"}
  where
    unsafeControl character = isControl character && character `notElem` ['\n','\r','\t']

validateContactContext :: AuthedUser -> UUID -> UUID -> Text -> UUID -> AppM ()
validateContactContext user senderProfileIdValue targetProfileIdValue kind identifier = case kind of
  "profile" -> unless (identifier == targetProfileIdValue) $
    throwError err400 {errBody="profile contact context must match targetProfileId"}
  "classified" -> do
    rows <- jsonRows "SELECT to_jsonb(TRUE) FROM classified WHERE id=? AND author_profile_id=? AND author_profile_id<>? AND status='published' AND moderation_status='allowed' AND expires_at>now()" [toPersistValue identifier,toPersistValue targetProfileIdValue,toPersistValue senderProfileIdValue]
    when (null rows) (throwError err404 {errBody="classified contact context does not match the participants"})
  "application" -> do
    _ <- applicationParticipantRole user identifier
    rows <- jsonRows "SELECT to_jsonb(TRUE) FROM classified_application application JOIN classified ON classified.id=application.classified_id WHERE application.id=? AND application.status IN ('accepted','conversation_open') AND ((application.applicant_profile_id=? AND classified.author_profile_id=?) OR (application.applicant_profile_id=? AND classified.author_profile_id=?))" [toPersistValue identifier,toPersistValue senderProfileIdValue,toPersistValue targetProfileIdValue,toPersistValue targetProfileIdValue,toPersistValue senderProfileIdValue]
    when (null rows) (throwError err404 {errBody="application contact context is not accepted or does not match the participants"})
  "invitation" -> do
    _ <- invitationParticipantRole user identifier
    rows <- jsonRows "SELECT to_jsonb(TRUE) FROM directory_invitation WHERE id=? AND status IN ('accepted','conversation_open') AND ((sender_profile_id=? AND target_profile_id=?) OR (sender_profile_id=? AND target_profile_id=?))" [toPersistValue identifier,toPersistValue senderProfileIdValue,toPersistValue targetProfileIdValue,toPersistValue targetProfileIdValue,toPersistValue senderProfileIdValue]
    when (null rows) (throwError err404 {errBody="invitation contact context is not accepted or does not match the participants"})
  _ -> throwError err400 {errBody="unsupported contact context"}

-- Read-only notification destinations recheck the underlying resource scope.
-- Never accept a URL or disclose whether an inaccessible record exists.
notificationContext :: AuthedUser -> Text -> UUID.UUID -> AppM Value
notificationContext user kind ident = case kind of
  "application" -> do
    -- ADR 0134: offer notifications reach both participants (new offer,
    -- counter, acceptance, rejection), so either may open the context.
    detail <- getApplication user ident
    authorProfile <- jsonOne err404 "SELECT jsonb_build_object('authorProfileId',c.author_profile_id,'title',c.title) FROM classified_application a JOIN classified c ON c.id=a.classified_id WHERE a.id=?" [toPersistValue ident]
    pure (mergeObjects detail authorProfile)
  "invitation" -> invitationSummary user ident
  "review" -> do
    visible <- jsonRows "SELECT to_jsonb(TRUE) FROM directory_review r JOIN directory_profile subject ON subject.id=r.subject_profile_id JOIN directory_profile_manager m ON m.profile_id=coalesce(subject.canonical_profile_id,subject.id) WHERE r.id=? AND r.status='published' AND m.account_party_id=? AND m.active AND m.can_manage" [toPersistValue ident,toPersistValue (auPartyId user)]
    when (null visible) $ throwError err404
    jsonOne err404 reviewSummarySql [toPersistValue ident]
  "alert" -> jsonOne err404
    "SELECT jsonb_build_object('id',d.id,'searchName',s.name,'resultType',doc.entity_kind,'resultId',doc.entity_id,'slug',doc.slug,'title',doc.title) FROM directory_alert_delivery d JOIN directory_saved_search s ON s.id=d.saved_search_id LEFT JOIN directory_profile original ON d.result_kind='profile' AND original.id::text=d.result_id JOIN directory_public_search_document doc ON doc.entity_kind=d.result_kind AND doc.entity_id=coalesce(original.canonical_profile_id::text,d.result_id) WHERE d.id=? AND s.account_party_id=? AND (doc.expires_at IS NULL OR doc.expires_at>now()) AND (doc.entity_kind<>'event' OR NOT EXISTS (SELECT 1 FROM external_event_ref ref WHERE ref.event_id::text=doc.entity_id AND lower(trim(ref.source_status))=?))"
    [toPersistValue ident,toPersistValue (auPartyId user),PersistText Social.externalEventRefSuppressedStatus]
  _ -> throwError err404
