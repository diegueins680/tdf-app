{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.Directory
  ( directoryPublicServer
  , directoryProtectedServer
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
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Data.Scientific (toBoundedInteger)
import Servant

import TDF.API.Directory
import TDF.Auth (AuthedUser(..), ModuleAccess(..), hasModuleAccess)
import qualified TDF.CMS.Models as CMS
import TDF.DB (Env(..))
import TDF.Directory.Policy

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
  :<|> publicProfileReviews
  :<|> publicClassified
  :<|> publicEvent
  :<|> publicVenue

searchDirectory mQuery mEntityType mCityId mLatitude mLongitude mRadiusKm
  mProfessionId mServiceId mInstrumentId mGenreId mRemote mAvailable
  mDateFrom mDateTo mCursor mLimit = do
  query <- validateQuery mQuery
  entityType <- validateEntityType mEntityType
  (latitude, longitude, radiusKm) <- validateGeo mLatitude mLongitude mRadiusKm
  let limit = min 50 (max 1 (fromMaybe 20 mLimit))
      cursor = normalizeOptional mCursor
      commonParams =
        [ PersistText query, optionalText entityType, optionalUuid mCityId
        , optionalDouble latitude, optionalDouble longitude, optionalDouble radiusKm
        , optionalUuid mProfessionId, optionalUuid mServiceId
        , optionalUuid mInstrumentId, optionalUuid mGenreId
        , maybe PersistNull PersistBool mRemote, maybe PersistNull PersistBool mAvailable
        , optionalTime mDateFrom, optionalTime mDateTo
        ]
  ranked <- jsonRows (searchSql False) (commonParams <> [optionalText cursor, PersistInt64 (fromIntegral (limit + 1))])
  sponsored <- jsonRows (searchSql True) (commonParams <> [PersistNull, PersistInt64 3])
  facetRows <- jsonRows facetsSql commonParams
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
    Just value | value `Set.member` Set.fromList ["profile","classified","event","venue"] -> pure (Just value)
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

searchSql :: Bool -> Text
searchSql sponsoredOnly =
  "WITH input AS (SELECT ?::text q,?::text entity_type,?::uuid city_id,?::float8 latitude,?::float8 longitude,?::float8 radius_km,?::uuid profession_id,?::uuid service_id,?::uuid instrument_id,?::uuid genre_id,?::boolean remote_only,?::boolean available_only,?::timestamptz date_from,?::timestamptz date_to), " <>
  "scored AS (SELECT document.*,directory_distance_km(input.latitude,input.longitude,document.public_latitude,document.public_longitude) distance_km," <>
  "CASE WHEN input.q='' THEN .5 ELSE greatest(ts_rank_cd(document.search_vector,plainto_tsquery('simple',directory_normalize_text(input.q))),directory_text_similarity(document.title,input.q)) END text_score," <>
  "CASE WHEN input.q='' THEN 0 WHEN EXISTS (SELECT 1 FROM catalog_search_alias alias WHERE alias.entity_id=ANY(document.profession_ids||document.service_ids||document.instrument_ids||document.genre_ids) AND (alias.normalized_term LIKE directory_normalize_text(input.q)||'%' OR directory_normalize_text(input.q) LIKE alias.normalized_term||'%')) THEN 1 ELSE 0 END semantic_score " <>
  "FROM directory_public_search_document document CROSS JOIN input WHERE document.sponsored=" <> (if sponsoredOnly then "TRUE " else "FALSE ") <>
  "AND (input.entity_type IS NULL OR document.entity_kind=input.entity_type) AND (input.city_id IS NULL OR document.city_id=input.city_id) " <>
  "AND (input.profession_id IS NULL OR input.profession_id=ANY(document.profession_ids)) AND (input.service_id IS NULL OR input.service_id=ANY(document.service_ids)) " <>
  "AND (input.instrument_id IS NULL OR input.instrument_id=ANY(document.instrument_ids)) AND (input.genre_id IS NULL OR input.genre_id=ANY(document.genre_ids)) " <>
  "AND (input.q='' OR document.search_vector @@ plainto_tsquery('simple',directory_normalize_text(input.q)) OR directory_text_similarity(document.search_text,input.q)>=.2 OR EXISTS (SELECT 1 FROM catalog_search_alias alias WHERE alias.entity_id=ANY(document.profession_ids||document.service_ids||document.instrument_ids||document.genre_ids) AND (alias.normalized_term LIKE directory_normalize_text(input.q)||'%' OR directory_normalize_text(input.q) LIKE alias.normalized_term||'%'))) " <>
  "AND (input.latitude IS NULL OR input.radius_km IS NULL OR directory_distance_km(input.latitude,input.longitude,document.public_latitude,document.public_longitude)<=input.radius_km) " <>
  "AND (input.remote_only IS DISTINCT FROM TRUE OR document.remote) AND (input.available_only IS DISTINCT FROM TRUE OR document.availability_score>0) " <>
  "AND (input.date_from IS NULL OR document.expires_at IS NULL OR document.expires_at>=input.date_from) AND (input.date_to IS NULL OR document.effective_at IS NULL OR document.effective_at<=input.date_to)), " <>
  "ranked AS (SELECT scored.*,round((.40*text_score+.15*semantic_score+.15*CASE WHEN distance_km IS NULL THEN .35 ELSE 1/(1+distance_km/25) END+.10*profile_completeness+.08*least(1,1/(1+extract(epoch from (now()-source_updated_at))/2592000))+.05*availability_score+.07*reputation_score)::numeric,6) organic_score FROM scored), " <>
  "boundary AS (SELECT organic_score,source_updated_at,entity_kind,entity_id FROM ranked WHERE entity_kind||':'||entity_id=?::text), " <>
  "page AS (SELECT ranked.* FROM ranked WHERE NOT EXISTS (SELECT 1 FROM boundary) OR EXISTS (SELECT 1 FROM boundary b WHERE ranked.organic_score<b.organic_score OR (ranked.organic_score=b.organic_score AND (ranked.source_updated_at<b.source_updated_at OR (ranked.source_updated_at=b.source_updated_at AND (ranked.entity_kind>b.entity_kind OR (ranked.entity_kind=b.entity_kind AND ranked.entity_id>b.entity_id)))))) ORDER BY organic_score DESC,source_updated_at DESC,entity_kind,entity_id LIMIT ?) " <>
  "SELECT jsonb_build_object('id',entity_id,'type',entity_kind,'slug',slug,'title',title,'subtitle',subtitle,'summary',summary,'imageUrl',image_url,'location',jsonb_build_object('cityId',city_id,'city',city_name,'countryCode',country_code,'latitude',public_latitude,'longitude',public_longitude,'precision',location_precision,'distanceKm',CASE WHEN distance_km IS NULL THEN NULL ELSE round(distance_km::numeric,1) END),'modality',jsonb_build_object('onsite',onsite,'remote',remote,'travel',available_to_travel),'taxonomy',jsonb_build_object('professionIds',profession_ids,'serviceIds',service_ids,'instrumentIds',instrument_ids,'genreIds',genre_ids),'score',organic_score,'scoreBreakdown',jsonb_build_object('text',round(text_score::numeric,4),'taxonomy',semantic_score,'proximity',CASE WHEN distance_km IS NULL THEN NULL ELSE round((1/(1+distance_km/25))::numeric,4) END,'quality',profile_completeness,'activityWeight',.08,'availability',availability_score,'reputation',reputation_score),'sponsored',sponsored,'sponsorDisclosure',sponsor_disclosure,'effectiveAt',effective_at,'expiresAt',expires_at,'cursor',entity_kind||':'||entity_id) FROM page"

facetsSql :: Text
facetsSql =
  "WITH input AS (SELECT ?::text q,?::text entity_type,?::uuid city_id,?::float8 latitude,?::float8 longitude,?::float8 radius_km,?::uuid profession_id,?::uuid service_id,?::uuid instrument_id,?::uuid genre_id,?::boolean remote_only,?::boolean available_only,?::timestamptz date_from,?::timestamptz date_to), " <>
  "filtered AS (SELECT document.* FROM directory_public_search_document document CROSS JOIN input WHERE NOT document.sponsored " <>
  "AND (input.q='' OR document.search_vector@@plainto_tsquery('simple',directory_normalize_text(input.q)) OR directory_text_similarity(document.search_text,input.q)>=.2 " <>
  "OR EXISTS (SELECT 1 FROM catalog_search_alias alias WHERE alias.entity_id=ANY(document.profession_ids||document.service_ids||document.instrument_ids||document.genre_ids) " <>
  "AND (alias.normalized_term LIKE directory_normalize_text(input.q)||'%' OR directory_normalize_text(input.q) LIKE alias.normalized_term||'%'))) " <>
  "AND (input.entity_type IS NULL OR document.entity_kind=input.entity_type) AND (input.city_id IS NULL OR document.city_id=input.city_id) " <>
  "AND (input.profession_id IS NULL OR input.profession_id=ANY(document.profession_ids)) AND (input.service_id IS NULL OR input.service_id=ANY(document.service_ids)) " <>
  "AND (input.instrument_id IS NULL OR input.instrument_id=ANY(document.instrument_ids)) AND (input.genre_id IS NULL OR input.genre_id=ANY(document.genre_ids)) " <>
  "AND (input.latitude IS NULL OR input.radius_km IS NULL OR directory_distance_km(input.latitude,input.longitude,document.public_latitude,document.public_longitude)<=input.radius_km) " <>
  "AND (input.remote_only IS DISTINCT FROM TRUE OR document.remote) AND (input.available_only IS DISTINCT FROM TRUE OR document.availability_score>0) " <>
  "AND (input.date_from IS NULL OR document.expires_at IS NULL OR document.expires_at>=input.date_from) " <>
  "AND (input.date_to IS NULL OR document.effective_at IS NULL OR document.effective_at<=input.date_to)) " <>
  "SELECT jsonb_build_object('entityTypes',coalesce((SELECT jsonb_object_agg(entity_kind,total) FROM (SELECT entity_kind,count(*) total FROM filtered GROUP BY entity_kind) counts),'{}'::jsonb)," <>
  "'cities',coalesce((SELECT jsonb_agg(jsonb_build_object('id',city_id,'name',city_name,'count',total) ORDER BY total DESC,city_name) FROM (SELECT city_id,max(city_name) city_name,count(*) total FROM filtered WHERE city_id IS NOT NULL GROUP BY city_id ORDER BY total DESC LIMIT 20) cities),'[]'::jsonb)," <>
  "'total',(SELECT count(*) FROM filtered))"

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
    "SELECT jsonb_build_object('label',label,'canonicalQuery',canonical_query,'suggestionKind',kind,'entityId',entity_id) FROM (SELECT alias.term label,alias.normalized_term canonical_query,'taxonomy'::text kind,alias.entity_id::text entity_id,1 priority FROM catalog_search_alias alias JOIN catalog_definition catalog ON catalog.id=alias.catalog_id WHERE catalog.public_read AND alias.normalized_term LIKE directory_normalize_text(?)||'%' UNION ALL SELECT document.title,document.title,document.entity_kind,document.entity_id,2 FROM directory_public_search_document document WHERE NOT document.sponsored AND (?::uuid IS NULL OR document.city_id=?::uuid) AND document.search_text LIKE directory_normalize_text(?)||'%' ) suggestion ORDER BY priority,label LIMIT 10"
    [PersistText query,optionalUuid mCityId,optionalUuid mCityId,PersistText query]
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
   <> "'requirements',item.requirements) ORDER BY item.sort_order),'[]'::jsonb) "
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
   <> "'canonicalUrl','/directorio/'||profile.slug) FROM directory_public_profile_resolution profile WHERE profile.requested_slug=?" )
    [PersistText (T.toLower (T.strip slugValue))]

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
    "SELECT jsonb_build_object('id',classified.id,'title',classified.title,'slug',classified.slug,'description',classified.description,'category',jsonb_build_object('id',category.id,'code',category.code,'name',category.name_es),'author',jsonb_build_object('id',profile.id,'name',profile.public_name,'slug',profile.slug),'modality',jsonb_build_object('onsite',classified.onsite,'remote',classified.remote,'travel',classified.available_to_travel),'locations',coalesce((SELECT jsonb_agg(jsonb_build_object('cityId',location.city_id,'city',city.name_es,'countryCode',country.alpha2,'metroId',location.metropolitan_area_id,'radiusKm',location.service_radius_km)) FROM classified_location location JOIN country_reference country ON country.id=location.country_id LEFT JOIN city_reference city ON city.id=location.city_id WHERE location.classified_id=classified.id),'[]'::jsonb),'compensation',CASE WHEN classified.compensation_type_id IS NULL THEN NULL ELSE jsonb_build_object('typeId',classified.compensation_type_id,'minMinor',classified.budget_min_minor,'maxMinor',classified.budget_max_minor,'currencyId',classified.currency_id,'negotiable',classified.budget_negotiable) END,'startsAt',classified.starts_at,'endsAt',classified.ends_at,'expiresAt',classified.expires_at,'canonicalUrl','/clasificados/'||classified.slug) FROM classified JOIN classified_category category ON category.id=classified.category_id JOIN directory_public_profile profile ON profile.id=classified.author_profile_id WHERE classified.slug=? AND classified.status='published' AND classified.moderation_status='allowed' AND classified.expires_at>now()"
    [PersistText (T.toLower (T.strip slugValue))]

publicEvent eventId = jsonOne err404
  "SELECT jsonb_build_object('id',id,'title',title,'description',description,'startTime',start_time,'endTime',end_time,'timezone',timezone,'priceCents',price_cents,'currencyId',currency_id,'capacity',capacity,'venue',CASE WHEN venue_id IS NULL THEN NULL ELSE jsonb_build_object('id',venue_id,'name',venue_name) END,'location',jsonb_build_object('cityId',city_id,'city',city_name,'countryCode',country_code,'latitude',public_latitude,'longitude',public_longitude,'precision','city'),'canonicalUrl','/eventos/'||id::text) FROM directory_public_event WHERE id=?" [PersistInt64 eventId]

publicVenue venueId = jsonOne err404
  "SELECT jsonb_build_object('id',id,'name',name,'capacity',capacity,'location',jsonb_build_object('cityId',city_id,'city',city_name,'countryCode',country_code,'latitude',public_latitude,'longitude',public_longitude,'precision','city'),'canonicalUrl','/venues/'||id::text) FROM directory_public_venue WHERE id=?" [PersistInt64 venueId]

directoryProtectedServer :: AuthedUser -> ServerT DirectoryProtectedAPI AppM
directoryProtectedServer user =
       setAgeAssurance user
  :<|> listManagedProfiles user
  :<|> createProfile user
  :<|> updateProfile user
  :<|> changeProfileStatus user
  :<|> listManagedClassifieds user
  :<|> createClassified user
  :<|> changeClassifiedStatus user
  :<|> listApplications user
  :<|> createApplication user
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
    "SELECT jsonb_build_object('id',profile.id,'kind',profile.profile_kind,'name',profile.public_name,'slug',profile.slug,'bio',profile.bio,'experienceSummary',profile.experience_summary,'creditsSummary',profile.credits_summary,'portfolio',profile.portfolio,'links',profile.links,'equipmentSummary',profile.equipment_summary,'rates',CASE WHEN profile.rate_min_minor IS NULL THEN NULL ELSE jsonb_build_object('minMinor',profile.rate_min_minor,'maxMinor',profile.rate_max_minor,'currencyId',profile.currency_id) END,'availabilityStatus',profile.availability_status,'onsite',profile.onsite,'remote',profile.remote,'availableToTravel',profile.available_to_travel,'travelRadiusKm',profile.travel_radius_km,'professionIds',coalesce((SELECT jsonb_agg(member.profession_id ORDER BY member.sort_order) FROM directory_profile_profession member WHERE member.profile_id=profile.id),'[]'::jsonb),'professionDetails',coalesce((SELECT jsonb_agg(jsonb_build_object('professionId',member.profession_id,'headline',member.headline,'yearsExperience',member.years_experience,'rateMinMinor',member.rate_min_minor,'rateMaxMinor',member.rate_max_minor,'currencyId',member.currency_id) ORDER BY member.sort_order) FROM directory_profile_profession member WHERE member.profile_id=profile.id),'[]'::jsonb),'instrumentIds',coalesce((SELECT jsonb_agg(member.instrument_id ORDER BY member.sort_order) FROM directory_profile_instrument member WHERE member.profile_id=profile.id),'[]'::jsonb),'instrumentDetails',coalesce((SELECT jsonb_agg(jsonb_build_object('instrumentId',member.instrument_id,'proficiency',member.proficiency) ORDER BY member.sort_order) FROM directory_profile_instrument member WHERE member.profile_id=profile.id),'[]'::jsonb),'genreIds',coalesce((SELECT jsonb_agg(member.genre_id ORDER BY member.sort_order) FROM directory_profile_genre member WHERE member.profile_id=profile.id),'[]'::jsonb),'serviceOfferingIds',coalesce((SELECT jsonb_agg(member.service_offering_id ORDER BY member.sort_order) FROM directory_profile_service member WHERE member.profile_id=profile.id),'[]'::jsonb),'languages',coalesce((SELECT jsonb_agg(jsonb_build_object('languageId',member.language_id,'proficiency',member.proficiency) ORDER BY member.language_id) FROM directory_profile_language member WHERE member.profile_id=profile.id),'[]'::jsonb),'serviceAreas',coalesce((SELECT jsonb_agg(jsonb_build_object('countryId',location.country_id,'subdivisionId',location.subdivision_id,'cityId',location.city_id,'metropolitanAreaId',location.metropolitan_area_id,'sectorLabel',location.sector_label,'serviceRadiusKm',location.service_radius_km,'primaryLocation',location.primary_location,'onsite',location.onsite) ORDER BY location.primary_location DESC,location.created_at,location.id) FROM directory_profile_location location WHERE location.profile_id=profile.id),'[]'::jsonb),'status',profile.profile_status,'visibility',profile.visibility,'moderationStatus',profile.moderation_status,'version',profile.version,'capabilities',jsonb_build_object('viewPrivate',manager.can_view_private,'edit',manager.can_edit,'publish',manager.can_publish,'contact',manager.can_contact,'manage',manager.can_manage)) AS value FROM directory_profile_manager manager JOIN directory_profile profile ON profile.id=manager.profile_id WHERE manager.account_party_id=? AND manager.active ORDER BY profile.updated_at DESC,profile.id"

createProfile user idempotency request@DirectoryProfileUpsert
  { profileKind, publicName, slug, bio, experienceSummary, creditsSummary, portfolio, links
  , equipmentSummary, rateMinMinor, rateMaxMinor, currencyId, availabilityStatus
  , onsite, remote, availableToTravel, travelRadiusKm } = do
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
        rawExecute "INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,bio,experience_summary,credits_summary,portfolio,links,equipment_summary,rate_min_minor,rate_max_minor,currency_id,availability_status,onsite,remote,available_to_travel,travel_radius_km,profile_status,visibility,moderation_status,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?::jsonb,?::jsonb,?,?,?,?,?,?,?,?,?,'draft','public','allowed',?,?)"
          [ toPersistValue profileId,PersistInt64 subjectPartyId,PersistText profileKind
          , PersistText (T.strip publicName),PersistText (T.toLower (T.strip slug))
          , optionalText (cleanOptionalText bio),optionalText (cleanOptionalText experienceSummary)
          , optionalText (cleanOptionalText creditsSummary)
          , PersistText (decodeJsonText (Aeson.toJSON normalizedPortfolio))
          , PersistText (decodeJsonText (Aeson.toJSON normalizedLinks))
          , optionalText (cleanOptionalText equipmentSummary),optionalInt64 rateMinMinor
          , optionalInt64 rateMaxMinor,optionalUuid currencyId
          , PersistText (fromMaybe "ask" availabilityStatus),PersistBool onsite,PersistBool remote
          , PersistBool availableToTravel,maybe PersistNull (PersistDouble . realToFrac) travelRadiusKm
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
  , onsite, remote, availableToTravel, travelRadiusKm } = do
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
    rawExecute "UPDATE directory_profile SET profile_kind=?,public_name=?,slug=?,bio=?,experience_summary=CASE WHEN ? THEN NULLIF(trim(?::text),'') ELSE experience_summary END,credits_summary=CASE WHEN ? THEN NULLIF(trim(?::text),'') ELSE credits_summary END,portfolio=CASE WHEN ? THEN ?::jsonb ELSE portfolio END,links=CASE WHEN ? THEN ?::jsonb ELSE links END,equipment_summary=CASE WHEN ? THEN NULLIF(trim(?::text),'') ELSE equipment_summary END,rate_min_minor=CASE WHEN ? THEN ?::bigint ELSE rate_min_minor END,rate_max_minor=CASE WHEN ? THEN ?::bigint ELSE rate_max_minor END,currency_id=CASE WHEN ? THEN ?::uuid ELSE currency_id END,availability_status=CASE WHEN ? THEN ?::text ELSE availability_status END,onsite=?,remote=?,available_to_travel=?,travel_radius_km=?,updated_at=now(),version=version+1 WHERE id=?"
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
      , maybe PersistNull (PersistDouble . realToFrac) travelRadiusKm,toPersistValue profileId]
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
  , countryId, cityId, metropolitanAreaId, travelRadiusKm } = do
  validateSlug slug
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

listManagedClassifieds user = jsonRows "SELECT jsonb_build_object('id',classified.id,'authorProfileId',classified.author_profile_id,'title',classified.title,'slug',classified.slug,'status',classified.status,'moderationStatus',classified.moderation_status,'expiresAt',classified.expires_at,'version',classified.version) FROM classified JOIN directory_profile_manager manager ON manager.profile_id=classified.author_profile_id WHERE manager.account_party_id=? AND manager.active ORDER BY classified.updated_at DESC,classified.id" [toPersistValue (auPartyId user)]

createClassified user idempotency request@ClassifiedCreateRequest
  { authorProfileId, categoryId, title, slug, description, onsite, remote
  , availableToTravel, startsAt, endsAt, experienceLevel, compensationTypeId
  , budgetMinMinor, budgetMaxMinor, currencyId, budgetNegotiable
  , serviceOfferingId, serviceAdId, expiresAt } = do
  requireProfileCapability user authorProfileId "publish"
  requireAdult user
  validateClassified request
  classifiedId <- reserveIdempotency user "classified.create" idempotency request "classified"
  existing <- jsonRows "SELECT jsonb_build_object('id',id,'slug',slug,'status',status,'version',version) FROM classified WHERE id=?" [toPersistValue classifiedId]
  case existing of
    value:_ -> pure value
    [] -> do
      now <- liftIO getCurrentTime
      let expiry=fromMaybe (addUTCTime (30*24*60*60) now) expiresAt
      when (expiry<=now || expiry>addUTCTime (90*24*60*60) now) $
        throwError err400 {errBody="expiresAt must be in the future and no more than 90 days away; renew manually after publication"}
      runDB $ do
        rawExecute "INSERT INTO classified(id,author_profile_id,category_id,title,slug,description,status,moderation_status,onsite,remote,available_to_travel,starts_at,ends_at,experience_level,compensation_type_id,budget_min_minor,budget_max_minor,currency_id,budget_negotiable,service_offering_id,service_ad_id,expires_at,duplicate_fingerprint) VALUES (?,?,?,?,?,?,'draft','allowed',?,?,?,?,?,?,?,?,?,?,?,?,?,?,encode(digest(directory_normalize_text(?)||':'||directory_normalize_text(?),'sha256'),'hex'))"
          [toPersistValue classifiedId,toPersistValue authorProfileId,toPersistValue categoryId,PersistText (T.strip title),PersistText (T.toLower (T.strip slug)),PersistText (T.strip description),PersistBool onsite,PersistBool remote,PersistBool availableToTravel,optionalTime startsAt,optionalTime endsAt,optionalText experienceLevel,optionalUuid compensationTypeId,optionalInt64 budgetMinMinor,optionalInt64 budgetMaxMinor,optionalUuid currencyId,PersistBool budgetNegotiable,optionalUuid serviceOfferingId,optionalInt64 serviceAdId,toPersistValue expiry,PersistText title,PersistText description]
        replaceClassifiedSelectionsDB classifiedId request
      classifiedSummary classifiedId

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
  when (newStatus=="published") (requireAdult user)
  current <- jsonOne err404 "SELECT to_jsonb(status) FROM classified WHERE id=?" [toPersistValue classifiedId]
  oldStatus <- case current of String value -> pure value; _ -> throwError err500
  from <- maybe (throwError err409 {errBody="unknown current classified state"}) pure (parseClassifiedStatus oldStatus)
  to <- maybe (throwError err400 {errBody="invalid target classified state"}) pure (parseClassifiedStatus newStatus)
  unless (allowedClassifiedTransition from to) $ throwError err409 {errBody="undeclared classified transition"}
  runDB $ do
    rawExecute "UPDATE classified SET status=? WHERE id=?" [PersistText newStatus,toPersistValue classifiedId]
    rawExecute "INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,previous_state,new_state,correlation_id,metadata) VALUES (?,'classified.status','classified',?,?,?,?,jsonb_build_object('authorProfileId',?::text,'reason',?::text))" [toPersistValue (auPartyId user),PersistText (UUID.toText classifiedId),PersistText oldStatus,PersistText newStatus,PersistText ("classified-status-"<>UUID.toText classifiedId),PersistText (UUID.toText authorProfile),optionalText statusReason]
    refreshClassifiedDB classifiedId
  when (newStatus == "published") $
    recordAuthenticatedEvent user "classified_published" "classified" (UUID.toText classifiedId)
  classifiedSummary classifiedId

parseClassifiedStatus value = lookup value [("draft",Draft),("pending_moderation",PendingModeration),("published",Published),("paused",Paused),("filled",Filled),("expired",Expired),("withdrawn",Withdrawn),("rejected",Rejected),("moderated",Moderated)]

classifiedSummary classifiedId = jsonOne err404 "SELECT jsonb_build_object('id',id,'authorProfileId',author_profile_id,'title',title,'slug',slug,'status',status,'moderationStatus',moderation_status,'expiresAt',expires_at,'version',version) FROM classified WHERE id=?" [toPersistValue classifiedId]

listApplications user classifiedId = do
  _ <- requireClassifiedAuthor user classifiedId
  jsonRows "SELECT jsonb_build_object('id',application.id,'classifiedId',application.classified_id,'applicantProfile',jsonb_build_object('id',profile.id,'name',profile.public_name,'slug',profile.slug),'message',application.message,'portfolio',application.portfolio,'availability',application.availability_text,'proposedAmountMinor',application.proposed_amount_minor,'currencyId',application.currency_id,'status',application.status,'submittedAt',application.submitted_at,'version',application.version) FROM classified_application application JOIN directory_profile profile ON profile.id=application.applicant_profile_id WHERE application.classified_id=? ORDER BY application.submitted_at DESC,application.id" [toPersistValue classifiedId]

createApplication user classifiedId idempotency request@ApplicationCreateRequest
  { applicantProfileId, message, portfolio, availability, proposedAmountMinor, currencyId } = do
  requireProfileCapability user applicantProfileId "contact"
  requireAdult user
  validatePrivateMessage 10 message
  requireJsonArray "portfolio" portfolio
  when (maybe False (<0) proposedAmountMinor || (proposedAmountMinor/=Nothing && currencyId==Nothing)) $
    throwError err400 {errBody="a non-negative proposal requires currencyId"}
  applicationId <- reserveIdempotency user "classified.apply" idempotency request "application"
  existing <- jsonRows "SELECT to_jsonb(TRUE) FROM classified_application WHERE id=?" [toPersistValue applicationId]
  if not (null existing) then applicationSummary user applicationId else do
    allowed <- jsonRows "SELECT to_jsonb(TRUE) FROM classified JOIN directory_profile author ON author.id=classified.author_profile_id JOIN directory_profile applicant ON applicant.id=? LEFT JOIN directory_contact_preference preference ON preference.profile_id=author.id WHERE classified.id=? AND classified.status='published' AND classified.moderation_status='allowed' AND classified.expires_at>now() AND classified.author_profile_id<>applicant.id AND applicant.completeness_score>=coalesce(preference.minimum_profile_completeness,0) AND coalesce(preference.allow_classified_applications,TRUE) AND NOT EXISTS (SELECT 1 FROM directory_profile_block block WHERE (block.blocker_profile_id=classified.author_profile_id AND block.blocked_profile_id=applicant.id) OR (block.blocker_profile_id=applicant.id AND block.blocked_profile_id=classified.author_profile_id))" [toPersistValue applicantProfileId,toPersistValue classifiedId]
    when (null allowed) $ throwError err403 {errBody="this classified does not accept this application"}
    consumeRate user "application" 20
    let fingerprint = requestFingerprint request
    runDB $ rawExecute "INSERT INTO classified_application(id,classified_id,applicant_profile_id,message,portfolio,availability_text,proposed_amount_minor,currency_id,status,idempotency_key,request_fingerprint) VALUES (?,?,?,?,?::jsonb,?,?,?,'submitted',?,?)"
      [toPersistValue applicationId,toPersistValue classifiedId,toPersistValue applicantProfileId,PersistText (T.strip message),PersistText (decodeJsonText portfolio),optionalText availability,optionalInt64 proposedAmountMinor,optionalUuid currencyId,PersistText idempotency,PersistText fingerprint]
    notifyClassifiedAuthor classifiedId "directory.application" "Nueva postulación" "Recibiste una postulación a tu anuncio."
    recordAuthenticatedEvent user "application_submitted" "application" (UUID.toText applicationId)
    applicationSummary user applicationId

changeApplicationStatus user applicationId DirectoryStatusRequest{status=newStatus} = do
  visible <- applicationParticipantRole user applicationId
  current <- jsonOne err404 "SELECT to_jsonb(status) FROM classified_application WHERE id=?" [toPersistValue applicationId]
  oldStatus <- case current of String value -> pure value; _ -> throwError err500
  from <- maybe (throwError err409 {errBody="unknown current application state"}) pure (parseApplicationStatus oldStatus)
  to <- maybe (throwError err400 {errBody="invalid target application state"}) pure (parseApplicationStatus newStatus)
  let applicantAllowed=Set.fromList ["withdrawn","conversation_open"]
      authorAllowed=Set.fromList ["viewed","shortlisted","accepted","rejected","conversation_open","converted"]
  unless ((visible=="applicant" && newStatus `Set.member` applicantAllowed) || (visible=="author" && newStatus `Set.member` authorAllowed) || visible=="admin") $ throwError err403 {errBody="application transition is not allowed for this participant"}
  unless (allowedApplicationTransition from to) $ throwError err409 {errBody="undeclared application transition"}
  runDB $ rawExecute "UPDATE classified_application SET status=?,withdrawn_at=CASE WHEN ?='withdrawn' THEN now() ELSE withdrawn_at END,updated_at=now(),version=version+1 WHERE id=?" [PersistText newStatus,PersistText newStatus,toPersistValue applicationId]
  when (newStatus == "conversation_open") $
    recordAuthenticatedEvent user "contact_accepted" "application" (UUID.toText applicationId)
  when (newStatus == "converted") $
    recordAuthenticatedEvent user "match_converted" "application" (UUID.toText applicationId)
  applicationSummary user applicationId

parseApplicationStatus value = lookup value
  [("submitted",ApplicationSubmitted),("viewed",ApplicationViewed),("shortlisted",ApplicationShortlisted),("accepted",ApplicationAccepted),("rejected",ApplicationRejected),("withdrawn",ApplicationWithdrawn),("conversation_open",ApplicationConversationOpen),("converted",ApplicationConverted)]

applicationSummary user applicationId = do
  _ <- applicationParticipantRole user applicationId
  jsonOne err404 "SELECT jsonb_build_object('id',id,'classifiedId',classified_id,'applicantProfileId',applicant_profile_id,'message',message,'portfolio',portfolio,'availability',availability_text,'proposedAmountMinor',proposed_amount_minor,'currencyId',currency_id,'status',status,'submittedAt',submitted_at,'version',version) FROM classified_application WHERE id=?" [toPersistValue applicationId]

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
    notifyProfile targetProfileId "directory.invitation" "Nueva invitación" "Un perfil te invitó a una oportunidad."
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
  jsonOne err404 "SELECT jsonb_build_object('id',invitation.id,'senderProfileId',invitation.sender_profile_id,'targetProfileId',invitation.target_profile_id,'classifiedId',invitation.classified_id,'message',invitation.message,'status',invitation.status,'expiresAt',invitation.expires_at,'version',invitation.version,'participantRole',?::text,'senderProfile',jsonb_build_object('id',sender_profile.id,'name',sender_profile.public_name,'slug',sender_profile.slug),'targetProfile',jsonb_build_object('id',target_profile.id,'name',target_profile.public_name,'slug',target_profile.slug),'classified',CASE WHEN classified.id IS NULL THEN NULL ELSE jsonb_build_object('id',classified.id,'title',classified.title,'slug',classified.slug,'status',classified.status) END) FROM directory_invitation invitation JOIN directory_profile sender_profile ON sender_profile.id=invitation.sender_profile_id JOIN directory_profile target_profile ON target_profile.id=invitation.target_profile_id LEFT JOIN classified ON classified.id=invitation.classified_id WHERE invitation.id=?" [PersistText role,toPersistValue invitationId]

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
          "INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,is_read,created_at) SELECT DISTINCT manager.account_party_id,'directory.review-created','Nueva reseña verificada','Un perfil con una interacción completada publicó una reseña.','directory_review',FALSE,now() FROM directory_profile_manager manager WHERE manager.profile_id=? AND manager.active AND manager.can_manage AND manager.account_party_id<>? AND EXISTS (SELECT 1 FROM directory_review review WHERE review.id=?)"
          [toPersistValue subjectProfileId,toPersistValue (auPartyId user),toPersistValue reviewId]
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

listFavorites user = jsonRows
  "SELECT jsonb_build_object('targetKind',favorite.target_kind,'targetId',favorite.target_id,'createdAt',favorite.created_at,'result',CASE WHEN document.entity_id IS NULL THEN NULL ELSE jsonb_build_object('type',document.entity_kind,'id',document.entity_id,'slug',document.slug,'title',document.title,'city',document.city_name) END) FROM directory_favorite favorite LEFT JOIN directory_public_search_document document ON document.entity_kind=favorite.target_kind AND document.entity_id=favorite.target_id WHERE favorite.account_party_id=? ORDER BY favorite.created_at DESC"
  [toPersistValue (auPartyId user)]

addFavorite user targetKind targetId = do
  validateTarget targetKind targetId
  runDB $ rawExecute "INSERT INTO directory_favorite(account_party_id,target_kind,target_id) VALUES (?,?,?) ON CONFLICT DO NOTHING" [toPersistValue (auPartyId user),PersistText targetKind,PersistText targetId]
  pure NoContent

removeFavorite user targetKind targetId = do
  validateTarget targetKind targetId
  runDB $ rawExecute "DELETE FROM directory_favorite WHERE account_party_id=? AND target_kind=? AND target_id=?" [toPersistValue (auPartyId user),PersistText targetKind,PersistText targetId]
  pure NoContent

validateTarget :: Text -> Text -> AppM ()
validateTarget kind identifier = do
  unless (kind `Set.member` Set.fromList ["profile","classified","event","venue"]) $ throwError err400 {errBody="invalid targetKind"}
  when (T.null (T.strip identifier) || T.length identifier>160) $ throwError err400 {errBody="invalid targetId"}

listSavedSearches user = jsonRows "SELECT jsonb_build_object('id',id,'name',name,'canonicalQuery',canonical_query,'alertsEnabled',alerts_enabled,'alertFrequency',alert_frequency,'lastEvaluatedAt',last_evaluated_at,'createdAt',created_at) FROM directory_saved_search WHERE account_party_id=? ORDER BY created_at DESC,id" [toPersistValue (auPartyId user)]

createSavedSearch user idempotency request@SavedSearchCreateRequest
  { name, canonicalQuery, alertsEnabled, alertFrequency } = do
  when (T.length (T.strip name)<1 || T.length name>120 || T.any isControl name) $
    throwError err400 {errBody="saved search name must contain 1-120 safe characters"}
  requireJsonObject "canonicalQuery" canonicalQuery
  unless (alertFrequency `Set.member` Set.fromList ["instant","daily","weekly","off"]) $
    throwError err400 {errBody="invalid alertFrequency"}
  savedId <- reserveIdempotency user "saved-search.create" idempotency request "saved-search"
  let queryText=decodeJsonText canonicalQuery
  runDB $ rawExecute "INSERT INTO directory_saved_search(id,account_party_id,name,canonical_query,query_hash,alerts_enabled,alert_frequency) VALUES (?,?,?,?::jsonb,encode(digest(?,'sha256'),'hex'),?,?) ON CONFLICT(account_party_id,query_hash) DO UPDATE SET name=EXCLUDED.name,alerts_enabled=EXCLUDED.alerts_enabled,alert_frequency=EXCLUDED.alert_frequency,updated_at=now()" [toPersistValue savedId,toPersistValue (auPartyId user),PersistText (T.strip name),PersistText queryText,PersistText queryText,PersistBool alertsEnabled,PersistText alertFrequency]
  jsonOne err500 "SELECT jsonb_build_object('id',id,'name',name,'canonicalQuery',canonical_query,'alertsEnabled',alerts_enabled,'alertFrequency',alert_frequency) FROM directory_saved_search WHERE account_party_id=? AND query_hash=encode(digest(?,'sha256'),'hex')" [toPersistValue (auPartyId user),PersistText queryText]

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

requireDirectoryAdmin :: AuthedUser -> AppM ()
requireDirectoryAdmin user = unless (isDirectoryAdmin user) (throwError err403 {errBody="directory administration requires the Admin module"})

listAdminClaims user = do
  requireDirectoryAdmin user
  jsonRows "SELECT jsonb_build_object('id',claim.id,'profileId',claim.profile_id,'profileName',profile.public_name,'claimantPartyId',claim.claimant_party_id,'claimType',claim.claim_type,'status',claim.status,'evidence',claim.evidence,'submittedAt',claim.submitted_at,'reviewerNotes',claim.reviewer_notes) FROM directory_claim claim JOIN directory_profile profile ON profile.id=claim.profile_id ORDER BY claim.created_at DESC,claim.id" []

changeClaimStatus user claimId DirectoryStatusRequest{status=newStatus,reason=statusReason} = do
  requireDirectoryAdmin user
  current <- jsonOne err404 "SELECT to_jsonb(status) FROM directory_claim WHERE id=?" [toPersistValue claimId]
  oldStatus <- case current of String value -> pure value; _ -> throwError err500
  unless (claimTransitionAllowed oldStatus newStatus) $ throwError err409 {errBody="undeclared claim transition"}
  runDB $ do
    rawExecute "UPDATE directory_claim SET status=?,reviewer_party_id=?,reviewer_notes=?,reviewed_at=CASE WHEN ? IN ('approved','rejected') THEN now() ELSE reviewed_at END WHERE id=?" [PersistText newStatus,toPersistValue (auPartyId user),optionalText statusReason,PersistText newStatus,toPersistValue claimId]
    when (newStatus=="approved") $ rawExecute "INSERT INTO directory_profile_manager(profile_id,account_party_id,can_view_private,can_edit,can_publish,can_contact,can_manage,active,granted_by,source_claim_id) SELECT profile_id,claimant_party_id,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,?,id FROM directory_claim WHERE id=? AND status='approved' ON CONFLICT(profile_id,account_party_id) DO UPDATE SET active=TRUE,can_edit=TRUE,can_publish=TRUE,can_contact=TRUE,can_manage=TRUE,granted_by=EXCLUDED.granted_by,source_claim_id=EXCLUDED.source_claim_id,revoked_at=NULL,version=directory_profile_manager.version+1" [toPersistValue (auPartyId user),toPersistValue claimId]
  jsonOne err404 "SELECT jsonb_build_object('id',id,'profileId',profile_id,'claimantPartyId',claimant_party_id,'claimType',claim_type,'status',status,'reviewedAt',reviewed_at) FROM directory_claim WHERE id=?" [toPersistValue claimId]

claimTransitionAllowed from to = from == to || (from,to) `Set.member` Set.fromList
  [ ("submitted","under_review"), ("submitted","withdrawn")
  , ("under_review","approved"), ("under_review","rejected"), ("under_review","withdrawn")
  , ("rejected","submitted")
  ]

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
    rawExecute "UPDATE classified SET status=CASE WHEN ?='pause' THEN 'paused' ELSE 'moderated' END,moderation_status=CASE WHEN ?='pause' THEN moderation_status ELSE 'blocked' END,updated_at=now() WHERE id::text=?" [PersistText action,PersistText action,PersistText targetId]
    rawExecute "DELETE FROM directory_search_document WHERE entity_kind='classified' AND entity_id=?" [PersistText targetId]
  (Just (String "review"),Just (String targetId)) | action `elem` ["pause","remove"] ->
    rawExecute "UPDATE directory_review SET status=CASE WHEN ?='pause' THEN 'hidden' ELSE 'removed' END,updated_at=now() WHERE id::text=?" [PersistText action,PersistText targetId]
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

consumeRate user action maxCount = do
  rows <- jsonRows "WITH current AS (INSERT INTO directory_rate_limit(scope,subject_hash,window_started_at,count,updated_at) VALUES (?,encode(digest(?::text,'sha256'),'hex'),date_trunc('day',now()),1,now()) ON CONFLICT(scope,subject_hash,window_started_at) DO UPDATE SET count=directory_rate_limit.count+1,updated_at=now() RETURNING count) SELECT jsonb_build_object('allowed',count<=?) FROM current" [PersistText action,PersistText (T.pack (show (partyNumber user))),PersistInt64 maxCount]
  case listToMaybe rows of Just (Object values) | KeyMap.lookup "allowed" values==Just (Bool True) -> pure (); _ -> throwError err429 {errBody="rate limit exceeded"}

notifyClassifiedAuthor classifiedId notificationType notificationTitle notificationBody = runDB $ rawExecute "INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,is_read,created_at) SELECT DISTINCT manager.account_party_id,?,?,?,'directory_application',FALSE,now() FROM classified JOIN directory_profile_manager manager ON manager.profile_id=classified.author_profile_id AND manager.active AND manager.can_contact WHERE classified.id=?" [PersistText notificationType,PersistText notificationTitle,PersistText notificationBody,toPersistValue classifiedId]

notifyProfile profileIdValue notificationType notificationTitle notificationBody = runDB $ rawExecute "INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,is_read,created_at) SELECT DISTINCT manager.account_party_id,?,?,?,'directory_invitation',FALSE,now() FROM directory_profile_manager manager WHERE manager.profile_id=? AND manager.active AND manager.can_contact" [PersistText notificationType,PersistText notificationTitle,PersistText notificationBody,toPersistValue profileIdValue]

isDirectoryAdmin user = hasModuleAccess ModuleAdmin user

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
