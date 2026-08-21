{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.Reviews
  ( reviewsPublicServer
  , reviewsProtectedServer
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (ToJSON, Value(..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql
  (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Servant
import Text.Read (readMaybe)

import TDF.API.Reviews
import TDF.Auth (AuthedUser(..))
import qualified TDF.CMS.Models as CMS
import TDF.DB (Env(..))
import TDF.Server.SocialEventsHandlers (postgresVisibleImportedMetadataClause)

type AppM = ReaderT Env Handler

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

jsonRows :: Text -> [PersistValue] -> AppM [Value]
jsonRows statement params = do
  rows <- runDB (rawSql statement params :: SqlPersistT IO [Single CMS.AesonValue])
  pure [CMS.unAesonValue value | Single value <- rows]

reviewsPublicServer :: ServerT ReviewsPublicAPI AppM
reviewsPublicServer = listPublicReviews

reviewsProtectedServer :: AuthedUser -> ServerT ReviewsProtectedAPI AppM
reviewsProtectedServer user =
       listReviewEligibility user
  :<|> createReview user

listPublicReviews :: Text -> Text -> Maybe UUID -> Maybe Int -> AppM ExperienceReviewPage
listPublicReviews rawTargetKind rawTargetId cursor requestedLimit = do
  (targetKind, targetId) <- validateTarget rawTargetKind rawTargetId
  ensurePublicTarget targetKind targetId
  let limit = min 50 (max 1 (fromMaybe 20 requestedLimit))
  summaryRows <- jsonRows
    ( "SELECT jsonb_build_object('targetKind',?::text,'targetId',?::text,"
   <> "'average',round(avg(rating)::numeric,2),'count',count(*)::bigint) "
   <> "FROM experience_review WHERE target_kind=? AND target_id=? AND status='published'" )
    [PersistText targetKind, PersistText targetId, PersistText targetKind, PersistText targetId]
  rows <- jsonRows publicReviewPageSql
    [ PersistText targetKind, PersistText targetId
    , maybe PersistNull toPersistValue cursor
    , PersistInt64 (fromIntegral (limit + 1))
    ]
  let visible = take limit rows
      next = if length rows > limit then visibleReviewId (last visible) else Nothing
  pure ExperienceReviewPage
    { summary = fromMaybe (object ["targetKind" .= targetKind, "targetId" .= targetId, "average" .= Null, "count" .= (0 :: Int)]) (listToMaybe summaryRows)
    , items = visible
    , nextCursor = next
    }

publicReviewPageSql :: Text
publicReviewPageSql =
  "WITH requested AS (SELECT ?::text target_kind,?::text target_id,?::uuid cursor), " <>
  "boundary AS (SELECT review.created_at,review.id FROM experience_review review,requested " <>
  "WHERE review.id=requested.cursor AND review.target_kind=requested.target_kind " <>
  "AND review.target_id=requested.target_id), " <>
  "page AS (SELECT review.* FROM experience_review review,requested " <>
  "WHERE review.target_kind=requested.target_kind AND review.target_id=requested.target_id " <>
  "AND review.status='published' AND (NOT EXISTS (SELECT 1 FROM boundary) OR EXISTS " <>
  "(SELECT 1 FROM boundary WHERE review.created_at<boundary.created_at OR " <>
  "(review.created_at=boundary.created_at AND review.id<boundary.id))) " <>
  "ORDER BY review.created_at DESC,review.id DESC LIMIT ?) " <>
  "SELECT jsonb_build_object('id',page.id,'targetKind',page.target_kind,'targetId',page.target_id," <>
  "'rating',page.rating,'body',page.body,'status',page.status," <>
  "'createdAt',page.created_at,'verified',TRUE," <>
  "'sourceKind',page.source_kind,'author',jsonb_build_object(" <>
  "'name',author.display_name,'avatarUrl',fan.avatar_url)) " <>
  "FROM page JOIN party author ON author.id=page.author_party_id " <>
  "LEFT JOIN fan_profile fan ON fan.fan_party_id=author.id " <>
  "ORDER BY page.created_at DESC,page.id DESC"

visibleReviewId :: Value -> Maybe UUID
visibleReviewId (Object values) = case KeyMap.lookup "id" values of
  Just (String value) -> UUID.fromText value
  _ -> Nothing
visibleReviewId _ = Nothing

ensurePublicTarget :: Text -> Text -> AppM ()
ensurePublicTarget targetKind targetId = do
  rows <- jsonRows statement [PersistText targetId]
  when (null rows) (throwError err404 {errBody = "review target not found"})
  where
    statement = case targetKind of
      "event" ->
        ( "SELECT to_jsonb(TRUE) FROM social_event event WHERE event.id::text=? AND ("
       <> "NOT EXISTS (SELECT 1 FROM external_event_ref source WHERE source.event_id=event.id) OR "
       <> postgresVisibleImportedMetadataClause "event.metadata"
       <> ")" )
      "marketplace_listing" -> "SELECT to_jsonb(TRUE) FROM marketplace_listing WHERE id::text=?"
      "service_offering" ->
        "SELECT to_jsonb(TRUE) FROM service_offering WHERE id::text=?"
      "service_package" ->
        "SELECT to_jsonb(TRUE) FROM service_storefront_package WHERE id::text=?"
      _ -> "SELECT to_jsonb(FALSE) WHERE FALSE"

listReviewEligibility :: AuthedUser -> Maybe Text -> Maybe Text -> AppM [Value]
listReviewEligibility user rawTargetKind rawTargetId = do
  (targetKind, targetId) <- validateEligibilityFilter rawTargetKind rawTargetId
  jsonRows eligibilitySql (eligibilityParams user targetKind targetId)

eligibilitySql :: Text
eligibilitySql =
  "WITH candidates AS (" <>
  "SELECT 'event'::text target_kind,event.id::text target_id,event.title target_title," <>
  "'event_ticket_order'::text source_kind,orders.id::text source_id," <>
  "coalesce(event.end_time,event.start_time) completed_at " <>
  "FROM event_ticket_order orders JOIN social_event event ON event.id=orders.event_id " <>
  "WHERE orders.buyer_party_id=?::bigint AND experience_review_source_is_eligible(" <>
  "'event',event.id::text,'event_ticket_order',orders.id::text,?::bigint) " <>
  "UNION ALL " <>
  "SELECT 'marketplace_listing',listing.id::text,listing.title,'marketplace_order',orders.id::text," <>
  "coalesce(sale.delivered_at,rental.returned_at,sale.updated_at,rental.updated_at,orders.updated_at) " <>
  "FROM marketplace_order orders JOIN marketplace_order_item item ON item.order_id=orders.id " <>
  "JOIN marketplace_listing listing ON listing.id=item.listing_id " <>
  "LEFT JOIN marketplace_sale_order_runtime sale ON sale.order_id=orders.id " <>
  "LEFT JOIN marketplace_rental_order_runtime rental ON rental.order_id=orders.id " <>
  "WHERE experience_review_source_is_eligible('marketplace_listing',listing.id::text," <>
  "'marketplace_order',orders.id::text,?::bigint) " <>
  "UNION ALL " <>
  "SELECT 'service_offering',offering.id::text,offering.name_es,'service_booking',booking.id::text," <>
  "coalesce(runtime.completed_at,booking.ends_at) " <>
  "FROM booking booking JOIN service_offering offering ON offering.id=booking.service_offering_id " <>
  "LEFT JOIN service_booking_checkout_runtime runtime ON runtime.booking_id=booking.id " <>
  "WHERE booking.party_id=?::bigint AND experience_review_source_is_eligible(" <>
  "'service_offering',offering.id::text,'service_booking',booking.id::text,?::bigint) " <>
  "UNION ALL " <>
  "SELECT 'service_package',package.id::text,package.name,'service_storefront_order',orders.id::text," <>
  "orders.updated_at FROM service_storefront_order orders " <>
  "JOIN service_storefront_package package ON package.id=orders.package_id " <>
  "WHERE experience_review_source_is_eligible('service_package',package.id::text," <>
  "'service_storefront_order',orders.id::text,?::bigint)), " <>
  "requested AS (SELECT ?::text target_kind,?::text target_id) " <>
  "SELECT jsonb_build_object('targetKind',candidate.target_kind,'targetId',candidate.target_id," <>
  "'targetTitle',candidate.target_title,'sourceKind',candidate.source_kind," <>
  "'sourceId',candidate.source_id,'completedAt',candidate.completed_at) " <>
  "FROM candidates candidate CROSS JOIN requested WHERE " <>
  "(requested.target_kind IS NULL OR candidate.target_kind=requested.target_kind) AND " <>
  "(requested.target_id IS NULL OR candidate.target_id=requested.target_id) AND NOT EXISTS (" <>
  "SELECT 1 FROM experience_review review WHERE review.source_kind=candidate.source_kind " <>
  "AND review.source_id=candidate.source_id AND review.target_kind=candidate.target_kind " <>
  "AND review.target_id=candidate.target_id AND review.author_party_id=?::bigint) " <>
  "ORDER BY candidate.completed_at DESC,candidate.target_kind,candidate.target_id,candidate.source_id"

-- The eligibility query uses the authenticated party in each source branch,
-- then repeats it for the final duplicate guard. Keeping the placeholders
-- explicit makes accidental cross-account broadening visible in review.
eligibilityParams :: AuthedUser -> Maybe Text -> Maybe Text -> [PersistValue]
eligibilityParams user targetKind targetId =
  replicate 6 (toPersistValue (auPartyId user)) <>
  [maybe PersistNull PersistText targetKind, maybe PersistNull PersistText targetId, toPersistValue (auPartyId user)]

createReview :: AuthedUser -> Text -> ExperienceReviewCreateRequest -> AppM Value
createReview user idempotency request@ExperienceReviewCreateRequest
  { targetKind = rawTargetKind
  , targetId = rawTargetId
  , sourceKind = rawSourceKind
  , sourceId = rawSourceId
  , rating
  , body
  } = do
  (targetKind, targetId) <- validateTarget rawTargetKind rawTargetId
  (sourceKind, sourceId) <- validateSource targetKind rawSourceKind rawSourceId
  when (rating < 1 || rating > 5) $
    throwError err400 {errBody = "rating must be between 1 and 5"}
  validateReviewBody body
  reviewId <- reserveIdempotency user idempotency request
  prior <- reviewById reviewId
  case prior of
    Just value -> pure value
    Nothing -> do
      eligible <- runDB
        (rawSql
          "SELECT experience_review_source_is_eligible(?,?,?,?,?)"
          [ PersistText targetKind, PersistText targetId, PersistText sourceKind
          , PersistText sourceId, toPersistValue (auPartyId user)
          ] :: SqlPersistT IO [Single Bool])
      unless (eligible == [Single True]) $
        throwError err409 {errBody = "review requires an eligible completed interaction owned by this account"}
      consumeRate user
      runDB $ rawExecute
        ( "INSERT INTO experience_review(id,target_kind,target_id,source_kind,source_id,"
       <> "author_party_id,rating,body,status) VALUES (?,?,?,?,?,?,?,?,'published') "
       <> "ON CONFLICT(source_kind,source_id,target_kind,target_id,author_party_id) DO NOTHING" )
        [ toPersistValue reviewId, PersistText targetKind, PersistText targetId
        , PersistText sourceKind, PersistText sourceId, toPersistValue (auPartyId user)
        , PersistInt64 (fromIntegral rating), maybe PersistNull (PersistText . T.strip) body
        ]
      created <- reviewById reviewId
      maybe
        (throwError err409 {errBody = "this completed interaction has already been reviewed"})
        pure
        created

reviewById :: UUID -> AppM (Maybe Value)
reviewById reviewId = listToMaybe <$> jsonRows
  ( "SELECT jsonb_build_object('id',review.id,'targetKind',review.target_kind,"
 <> "'targetId',review.target_id,'rating',review.rating,'body',review.body,"
 <> "'status',review.status,'createdAt',review.created_at,'verified',TRUE,"
 <> "'sourceKind',review.source_kind,'author',jsonb_build_object("
 <> "'name',author.display_name,'avatarUrl',fan.avatar_url)) "
 <> "FROM experience_review review JOIN party author ON author.id=review.author_party_id "
 <> "LEFT JOIN fan_profile fan ON fan.fan_party_id=author.id WHERE review.id=?" )
  [toPersistValue reviewId]

reserveIdempotency
  :: ToJSON request
  => AuthedUser
  -> Text
  -> request
  -> AppM UUID
reserveIdempotency user key request = do
  when (T.length key < 8 || T.length key > 160 || T.any isControl key) $
    throwError err400 {errBody = "Idempotency-Key must contain 8-160 safe characters"}
  let fingerprint = requestFingerprint request
  candidateId <- liftIO nextRandom
  runDB $ rawExecute
    ( "INSERT INTO directory_idempotency(actor_party_id,operation,idempotency_key,"
   <> "request_fingerprint,resource_kind,resource_id,expires_at) "
   <> "VALUES (?,'experience-review.create',?,?, 'experience_review',?,now()+interval '24 hours') "
   <> "ON CONFLICT(actor_party_id,operation,idempotency_key) DO UPDATE SET "
   <> "request_fingerprint=EXCLUDED.request_fingerprint,resource_kind=EXCLUDED.resource_kind,"
   <> "resource_id=EXCLUDED.resource_id,created_at=now(),expires_at=EXCLUDED.expires_at "
   <> "WHERE directory_idempotency.expires_at<=now()" )
    [ toPersistValue (auPartyId user), PersistText key, PersistText fingerprint
    , PersistText (UUID.toText candidateId)
    ]
  stored <- jsonRows
    ( "SELECT jsonb_build_object('fingerprint',request_fingerprint,'resourceId',resource_id) "
   <> "FROM directory_idempotency WHERE actor_party_id=? "
   <> "AND operation='experience-review.create' AND idempotency_key=?" )
    [toPersistValue (auPartyId user), PersistText key]
  case listToMaybe stored of
    Just (Object values) ->
      case (KeyMap.lookup "fingerprint" values, KeyMap.lookup "resourceId" values) of
        (Just (String previous), Just (String resourceId)) | previous == fingerprint ->
          maybe (throwError err500) pure (UUID.fromText resourceId)
        _ -> throwError err409 {errBody = "Idempotency-Key was already used with a different request"}
    _ -> throwError err500

requestFingerprint :: ToJSON request => request -> Text
requestFingerprint request =
  T.pack (show (hash (BL.toStrict (encode request)) :: Digest SHA256))

consumeRate :: AuthedUser -> AppM ()
consumeRate user = do
  allowed <- runDB
    (rawSql
      ( "WITH current AS (INSERT INTO directory_rate_limit("
     <> "scope,subject_hash,window_started_at,count,updated_at) VALUES ("
     <> "'experience-review',encode(digest(?::text,'sha256'),'hex'),date_trunc('day',now()),1,now()) "
     <> "ON CONFLICT(scope,subject_hash,window_started_at) DO UPDATE SET "
     <> "count=directory_rate_limit.count+1,updated_at=now() RETURNING count) "
     <> "SELECT count<=10 FROM current" )
      [PersistText (T.pack (show (fromSqlParty user)))] :: SqlPersistT IO [Single Bool])
  unless (allowed == [Single True]) $
    throwError err429 {errBody = "review rate limit exceeded"}

fromSqlParty :: AuthedUser -> Integer
fromSqlParty = fromIntegral . fromSqlKey . auPartyId

validateEligibilityFilter :: Maybe Text -> Maybe Text -> AppM (Maybe Text, Maybe Text)
validateEligibilityFilter Nothing Nothing = pure (Nothing, Nothing)
validateEligibilityFilter Nothing (Just _) =
  throwError err400 {errBody = "targetId requires targetKind"}
validateEligibilityFilter (Just kind) Nothing = do
  normalized <- validateTargetKind kind
  pure (Just normalized, Nothing)
validateEligibilityFilter (Just kind) (Just identifier) = do
  (normalizedKind, normalizedId) <- validateTarget kind identifier
  pure (Just normalizedKind, Just normalizedId)

validateTarget :: Text -> Text -> AppM (Text, Text)
validateTarget rawKind rawId = do
  targetKind <- validateTargetKind rawKind
  let targetId = T.strip rawId
  validateIdentifier (targetIdType targetKind) "targetId" targetId
  pure (targetKind, targetId)

validateTargetKind :: Text -> AppM Text
validateTargetKind rawKind = do
  let targetKind = T.toLower (T.strip rawKind)
  unless (targetKind `elem` ["event","marketplace_listing","service_offering","service_package"]) $
    throwError err400 {errBody = "unsupported review targetKind"}
  pure targetKind

validateSource :: Text -> Text -> Text -> AppM (Text, Text)
validateSource targetKind rawKind rawId = do
  let sourceKind = T.toLower (T.strip rawKind)
      sourceId = T.strip rawId
      expected = case targetKind of
        "event" -> ("event_ticket_order", DecimalId)
        "marketplace_listing" -> ("marketplace_order", UuidId)
        "service_offering" -> ("service_booking", DecimalId)
        "service_package" -> ("service_storefront_order", UuidId)
        _ -> ("", UuidId)
  unless (sourceKind == fst expected) $
    throwError err400 {errBody = "sourceKind does not match targetKind"}
  validateIdentifier (snd expected) "sourceId" sourceId
  pure (sourceKind, sourceId)

data IdentifierType = DecimalId | UuidId

targetIdType :: Text -> IdentifierType
targetIdType "event" = DecimalId
targetIdType _ = UuidId

validateIdentifier :: IdentifierType -> BL.ByteString -> Text -> AppM ()
validateIdentifier identifierType field value =
  unless valid (throwError err400 {errBody = field <> " is invalid"})
  where
    valid = case identifierType of
      UuidId -> maybe False (const True) (UUID.fromText value)
      DecimalId -> maybe False (> (0 :: Integer)) (readMaybe (T.unpack value))

validateReviewBody :: Maybe Text -> AppM ()
validateReviewBody Nothing = pure ()
validateReviewBody (Just value) =
  when (T.length (T.strip value) < 10 || T.length value > 2000 || T.any unsafeControl value) $
    throwError err400 {errBody = "review body must contain 10-2000 safe characters"}
  where
    unsafeControl character = isControl character && character `notElem` ['\n','\r','\t']
