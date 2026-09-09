{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.MerchReputation
  ( merchReputationPublicServer
  , merchReputationProtectedServer
  ) where

import Control.Exception (SomeException, fromException, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (Value(..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool)
import Database.PostgreSQL.Simple (SqlError(..))
import Servant
import System.Environment (lookupEnv)

import TDF.API.MerchReputation
import TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.CMS.Models as CMS
import TDF.DB (Env(..))

type AppM = ReaderT Env Handler

merchReputationPublicServer :: ServerT MerchReputationPublicAPI AppM
merchReputationPublicServer =
       listArtistStores
  :<|> getStoreReputation
  :<|> listStoreReviews
  :<|> getProductReputation
  :<|> listProductReviews
  :<|> getFormulaExplanation

merchReputationProtectedServer :: AuthedUser -> ServerT MerchReputationProtectedAPI AppM
merchReputationProtectedServer user =
       getOrderEligibility user
  :<|> claimOrderBuyer user
  :<|> submitStoreReview user
  :<|> submitProductReview user
  :<|> submitSellerResponse user
  :<|> reportContent user
  :<|> appealDecision user
  :<|> getPriorities user
  :<|> putPriorities user
  :<|> submitCategorySuggestion user
  :<|> getNotificationPreferences user
  :<|> putNotificationPreferences user
  :<|> getSellerStoreReputation user
  :<|> listModerationCases user
  :<|> listCategorySuggestions user
  :<|> decideCategorySuggestion user
  :<|> transitionModeration user
  :<|> decideModeration user
  :<|> resolveAppeal user

runRows :: Text -> [PersistValue] -> AppM [Value]
runRows statement params = do
  pool <- asks envPool
  rows <- liftIO (runSqlPool
    (rawSql statement params :: SqlPersistT IO [Single CMS.AesonValue]) pool)
  pure [CMS.unAesonValue value | Single value <- rows]

oneRow :: Text -> [PersistValue] -> AppM Value
oneRow statement params = do
  rows <- runRows statement params
  maybe (throwError err404) pure (listToMaybe rows)

runtimeEnvironment :: AppM Text
runtimeEnvironment = liftIO $ do
  configured <- lookupEnv "COMMERCE_CHECKOUT_ENV"
  pure $ case fmap (T.toLower . T.strip . T.pack) configured of
    Just "production" -> "production"
    Just "staging" -> "staging"
    _ -> "development"

requireFlag :: Text -> AppM Text
requireFlag flagKey = do
  environment <- runtimeEnvironment
  enabled <- runRows
    "SELECT to_jsonb(enabled) FROM merch_reputation_feature_flag WHERE flag_key=? AND environment=?"
    [PersistText flagKey,PersistText environment]
  unless (enabled == [Bool True]) $
    throwError err404 {errBody="Merch reputation is unavailable"}
  pure environment

listArtistStores :: Int64 -> AppM [Value]
listArtistStores artistPartyId = do
  _ <- requireFlag "store_reviews"
  runRows artistStoresSql [PersistInt64 artistPartyId]

getStoreReputation :: UUID -> AppM Value
getStoreReputation storeId = do
  _ <- requireFlag "store_reviews"
  oneRow storeReputationSql [toPersistValue storeId]

getProductReputation :: UUID -> AppM Value
getProductReputation productId = do
  _ <- requireFlag "product_reviews"
  oneRow productReputationSql [toPersistValue productId]

listStoreReviews :: UUID -> Maybe UUID -> Maybe Int -> AppM Value
listStoreReviews storeId cursor requestedLimit = do
  _ <- requireFlag "store_reviews"
  listPublicMerchReviews "store" storeId cursor requestedLimit

listProductReviews :: UUID -> Maybe UUID -> Maybe Int -> AppM Value
listProductReviews productId cursor requestedLimit = do
  _ <- requireFlag "product_reviews"
  listPublicMerchReviews "product" productId cursor requestedLimit

listPublicMerchReviews :: Text -> UUID -> Maybe UUID -> Maybe Int -> AppM Value
listPublicMerchReviews kind subjectId cursor requestedLimit = do
  let pageLimit = max 1 (min 50 (fromMaybe 20 requestedLimit))
  items <- runRows publicReviewsSql
    [ PersistText kind,toPersistValue subjectId,maybe PersistNull toPersistValue cursor
    , PersistInt64 (fromIntegral (pageLimit+1))
    ]
  let visible = take pageLimit items
      nextCursor = if length items > pageLimit then reviewIdFromValue (last visible) else Nothing
  pure (object ["items".=visible,"nextCursor".=nextCursor])

reviewIdFromValue :: Value -> Maybe Text
reviewIdFromValue (Object value) =
  case KeyMap.lookup "id" value of
    Just (String reviewId) -> Just reviewId
    _ -> Nothing
reviewIdFromValue _ = Nothing

getFormulaExplanation :: Maybe Text -> AppM Value
getFormulaExplanation locale =
  oneRow
    ("SELECT jsonb_build_object('version',id,'explanation',CASE WHEN ?='en' "
      <> "THEN explanation_en ELSE explanation_es END,'parameters',jsonb_build_object("
      <> "'minimumEvaluableOrders',parameters->'minimumEvaluableOrders',"
      <> "'primaryPeriodDays',parameters->'primaryPeriodDays',"
      <> "'reviewWeight',parameters->'reviewWeight','operationalWeight',parameters->'operationalWeight',"
      <> "'purchaseValueWeighted',false,'rankingContributionCap',parameters->'rankingContributionCap',"
      <> "'limitedEvidenceReviewCount',parameters->'limitedEvidenceReviewCount',"
      <> "'strongEvidenceReviewCount',parameters->'strongEvidenceReviewCount')) "
      <> "FROM merch_reputation_formula_version WHERE status='active' ORDER BY activated_at DESC LIMIT 1")
    [PersistText (if fmap T.toLower locale==Just "en" then "en" else "es")]

getOrderEligibility :: AuthedUser -> UUID -> AppM Value
getOrderEligibility user orderId = do
  _ <- requireFlag "store_reviews"
  oneRow eligibilitySql [actorValue user,toPersistValue orderId,actorValue user]

claimOrderBuyer :: AuthedUser -> UUID -> Text -> AppM Value
claimOrderBuyer user orderId lookupToken = do
  _ <- requireFlag "store_reviews"
  when (T.length lookupToken<32 || T.length lookupToken>300
      || T.any (<' ') lookupToken) $
    throwError err404
  runMutation "SELECT merch_reputation_claim_order_buyer(?,?,?)"
    [toPersistValue orderId,actorValue user,PersistText lookupToken]

submitStoreReview
  :: AuthedUser -> UUID -> Text -> MerchReviewSubmitRequest -> AppM Value
submitStoreReview user orderId idempotencyKey request = do
  environment <- requireFlag "store_reviews"
  validateReviewRequest idempotencyKey request
  runMutation
    "SELECT merch_reputation_submit_review(?::bigint, 'store', ?, NULL::uuid, ?::smallint, ?, ?, ?::jsonb, ?::jsonb, ?::integer, ?, ?)"
    [ actorValue user,toPersistValue orderId
    , PersistInt64 (fromIntegral (overallRating request)),PersistBool (issueOccurred request)
    , maybe PersistNull PersistText (comment request),jsonValue (dimensions request)
    , jsonValue (toJSON (fromMaybe [] (images request)))
    , PersistInt64 (fromIntegral (expectedRevision request)),PersistText (T.strip idempotencyKey)
    , PersistText environment
    ]

submitProductReview
  :: AuthedUser -> UUID -> UUID -> Text -> MerchReviewSubmitRequest -> AppM Value
submitProductReview user orderId lineId idempotencyKey request = do
  environment <- requireFlag "product_reviews"
  validateReviewRequest idempotencyKey request
  runMutation
    "SELECT merch_reputation_submit_review(?::bigint, 'product', ?, ?, ?::smallint, ?, ?, ?::jsonb, ?::jsonb, ?::integer, ?, ?)"
    [ actorValue user,toPersistValue orderId,toPersistValue lineId
    , PersistInt64 (fromIntegral (overallRating request)),PersistBool (issueOccurred request)
    , maybe PersistNull PersistText (comment request),jsonValue (dimensions request)
    , jsonValue (toJSON (fromMaybe [] (images request)))
    , PersistInt64 (fromIntegral (expectedRevision request)),PersistText (T.strip idempotencyKey)
    , PersistText environment
    ]

submitSellerResponse
  :: AuthedUser -> UUID -> Text -> MerchSellerResponseRequest -> AppM Value
submitSellerResponse user reviewId idempotencyKey MerchSellerResponseRequest{responseBody,responseExpectedRevision} = do
  environment <- requireFlag "seller_responses"
  validateIdempotencyKey idempotencyKey
  when (T.length (T.strip responseBody)<2 || T.length responseBody>2000) $
    throwError err400 {errBody="Response must contain 2-2000 characters"}
  runMutation "SELECT merch_reputation_respond(?::bigint,?,?,?::integer,?,?)"
    [ actorValue user,toPersistValue reviewId,PersistText (T.strip responseBody)
    , PersistInt64 (fromIntegral responseExpectedRevision),PersistText (T.strip idempotencyKey)
    , PersistText environment
    ]

reportContent
  :: AuthedUser -> Text -> MerchContentReportRequest -> AppM Value
reportContent user idempotencyKey MerchContentReportRequest
  {reportTargetType,reportTargetId,reportReason,reportDetails,authorizedEvidence} = do
  environment <- requireFlag "moderation"
  validateIdempotencyKey idempotencyKey
  runMutation "SELECT merch_reputation_report_content(?,?,?,?,?,?::jsonb,?,?)"
    [ actorValue user,PersistText reportTargetType,toPersistValue reportTargetId
    , PersistText reportReason,maybe PersistNull PersistText reportDetails
    , jsonValue (toJSON (fromMaybe [] authorizedEvidence)),PersistText (T.strip idempotencyKey)
    , PersistText environment
    ]

appealDecision :: AuthedUser -> UUID -> Text -> MerchAppealRequest -> AppM Value
appealDecision user decisionId idempotencyKey MerchAppealRequest{appealGrounds} = do
  _ <- requireFlag "moderation"
  validateIdempotencyKey idempotencyKey
  when (T.length (T.strip appealGrounds)<20 || T.length appealGrounds>3000) $
    throwError err400 {errBody="Appeal grounds must contain 20-3000 characters"}
  runMutation "SELECT merch_reputation_appeal_decision(?,?,?,?)"
    [actorValue user,toPersistValue decisionId,PersistText appealGrounds,PersistText (T.strip idempotencyKey)]

getPriorities :: AuthedUser -> Text -> AppM Value
getPriorities user requestedKind = do
  let subjectKind = T.toLower (T.strip requestedKind)
  unless (subjectKind `elem` ["store","product"]) $
    throwError err400 {errBody="Subject kind must be store or product"}
  _ <- requireFlag (if subjectKind=="store" then "store_reviews" else "product_reviews")
  oneRow prioritiesSql
    [actorValue user,PersistText subjectKind,PersistText subjectKind,PersistText subjectKind]

putPriorities :: AuthedUser -> Text -> Text -> MerchPriorityRequest -> AppM Value
putPriorities user requestedKind idempotencyKey MerchPriorityRequest
  {orderedDimensionCodes,priorityExpectedRevision} = do
  let subjectKind = T.toLower (T.strip requestedKind)
  unless (subjectKind `elem` ["store","product"]) $
    throwError err400 {errBody="Subject kind must be store or product"}
  _ <- requireFlag (if subjectKind=="store" then "store_reviews" else "product_reviews")
  validateIdempotencyKey idempotencyKey
  when (null orderedDimensionCodes || length orderedDimensionCodes>20
      || any (T.null . T.strip) orderedDimensionCodes || priorityExpectedRevision<0) $
    throwError err400 {errBody="Invalid commercial reputation priorities"}
  runMutation "SELECT merch_reputation_set_priorities(?::bigint,?,?::jsonb,?::integer,?)"
    [ actorValue user,PersistText subjectKind,jsonValue (toJSON orderedDimensionCodes)
    , PersistInt64 (fromIntegral priorityExpectedRevision),PersistText (T.strip idempotencyKey)
    ]

submitCategorySuggestion
  :: AuthedUser -> Text -> MerchCategorySuggestionRequest -> AppM Value
submitCategorySuggestion user idempotencyKey MerchCategorySuggestionRequest
  {suggestionSubjectKind,suggestionLabel,suggestionDefinition} = do
  let subjectKind = T.toLower (T.strip suggestionSubjectKind)
  unless (subjectKind `elem` ["store","product"]) $
    throwError err400 {errBody="Subject kind must be store or product"}
  environment <- requireFlag (if subjectKind=="store" then "store_reviews" else "product_reviews")
  _ <- requireFlag "moderation"
  validateIdempotencyKey idempotencyKey
  when (T.length (T.strip suggestionLabel)<3 || T.length suggestionLabel>80
      || T.length (T.strip suggestionDefinition)<20 || T.length suggestionDefinition>500) $
    throwError err400 {errBody="Category suggestion label or definition is invalid"}
  runMutation "SELECT merch_reputation_submit_category_suggestion(?::bigint,?,?,?,?,?)"
    [ actorValue user,PersistText subjectKind,PersistText suggestionLabel
    , PersistText suggestionDefinition,PersistText (T.strip idempotencyKey),PersistText environment
    ]

getNotificationPreferences :: AuthedUser -> AppM Value
getNotificationPreferences user = oneRow notificationPreferencesSql [actorValue user]

putNotificationPreferences :: AuthedUser -> MerchNotificationPreferenceRequest -> AppM Value
putNotificationPreferences user MerchNotificationPreferenceRequest
  { reviewInvitation,reviewReminder,sellerResponseNotification,moderationChange
  , evidenceRequest,appealResult,badgeChange
  } = runMutation
    ("INSERT INTO merch_reputation_notification_preference(party_id,review_invitation,"
      <> "review_reminder,seller_response,moderation_change,evidence_request,appeal_result,badge_change) "
      <> "VALUES (?::bigint,?,?,?,?,?,?,?) ON CONFLICT (party_id) DO UPDATE SET "
      <> "review_invitation=EXCLUDED.review_invitation,review_reminder=EXCLUDED.review_reminder,"
      <> "seller_response=EXCLUDED.seller_response,moderation_change=EXCLUDED.moderation_change,"
      <> "evidence_request=EXCLUDED.evidence_request,appeal_result=EXCLUDED.appeal_result,"
      <> "badge_change=EXCLUDED.badge_change,updated_at=NOW() RETURNING jsonb_build_object("
      <> "'reviewInvitation',review_invitation,'reviewReminder',review_reminder,"
      <> "'sellerResponseNotification',seller_response,'moderationChange',moderation_change,"
      <> "'evidenceRequest',evidence_request,'appealResult',appeal_result,'badgeChange',badge_change)")
    [ actorValue user,PersistBool reviewInvitation,PersistBool reviewReminder
    , PersistBool sellerResponseNotification,PersistBool moderationChange
    , PersistBool evidenceRequest,PersistBool appealResult,PersistBool badgeChange
    ]

getSellerStoreReputation :: AuthedUser -> UUID -> AppM Value
getSellerStoreReputation user storeId = do
  _ <- requireFlag "store_reviews"
  oneRow sellerStoreSql [toPersistValue storeId,actorValue user]

listModerationCases :: AuthedUser -> Maybe Text -> AppM [Value]
listModerationCases user state = do
  requireAdmin user
  _ <- requireFlag "moderation"
  runRows moderationCasesSql
    [maybe PersistNull PersistText state,maybe PersistNull PersistText state]

listCategorySuggestions :: AuthedUser -> Maybe Text -> AppM [Value]
listCategorySuggestions user status = do
  requireAdmin user
  _ <- requireFlag "moderation"
  runRows categorySuggestionsSql
    [maybe PersistNull PersistText status,maybe PersistNull PersistText status]

decideCategorySuggestion
  :: AuthedUser -> UUID -> Text -> MerchCategorySuggestionDecisionRequest -> AppM Value
decideCategorySuggestion user suggestionId idempotencyKey MerchCategorySuggestionDecisionRequest
  { suggestionStatus,suggestionMinimumSample,suggestionBiasTest,suggestionUtilityTest
  , suggestionDecisionReason
  } = do
  requireAdmin user
  _ <- requireFlag "moderation"
  validateIdempotencyKey idempotencyKey
  when (suggestionStatus `notElem` ["duplicate","testing","approved","rejected"]
      || T.length (T.strip suggestionDecisionReason)<20
      || T.length suggestionDecisionReason>3000) $
    throwError err400 {errBody="Category suggestion decision is invalid"}
  runMutation
    "SELECT merch_reputation_decide_category_suggestion(?::bigint,?,?,?::integer,?::jsonb,?::jsonb,?,?)"
    [ actorValue user,toPersistValue suggestionId,PersistText suggestionStatus
    , maybe PersistNull (PersistInt64 . fromIntegral) suggestionMinimumSample
    , maybe PersistNull jsonValue suggestionBiasTest,maybe PersistNull jsonValue suggestionUtilityTest
    , PersistText suggestionDecisionReason,PersistText (T.strip idempotencyKey)
    ]

transitionModeration
  :: AuthedUser -> UUID -> Text -> MerchModerationWorkflowRequest -> AppM Value
transitionModeration user caseId idempotencyKey MerchModerationWorkflowRequest
  {moderationAction,workflowRationale,workflowEvidence} = do
  requireAdmin user
  _ <- requireFlag "moderation"
  validateIdempotencyKey idempotencyKey
  when (moderationAction `notElem` ["triage","request_evidence","provisionally_hide","resume_review"]
      || T.length (T.strip workflowRationale)<20 || T.length workflowRationale>3000) $
    throwError err400 {errBody="Moderation workflow action or rationale is invalid"}
  runMutation "SELECT merch_reputation_transition_moderation_case(?::bigint,?,?,?,?::jsonb,?)"
    [ actorValue user,toPersistValue caseId,PersistText moderationAction
    , PersistText workflowRationale,jsonValue workflowEvidence,PersistText (T.strip idempotencyKey)
    ]

decideModeration
  :: AuthedUser -> UUID -> Text -> MerchModerationDecisionRequest -> AppM Value
decideModeration user caseId idempotencyKey MerchModerationDecisionRequest
  {moderationDecision,moderationReasonCode,moderationRationale,moderationEvidence} = do
  requireAdmin user
  _ <- requireFlag "moderation"
  validateIdempotencyKey idempotencyKey
  runMutation "SELECT merch_reputation_decide_moderation(?,?,?,?,?::jsonb,?)"
    [actorValue user,toPersistValue caseId,PersistText moderationDecision
    ,PersistText moderationReasonCode,PersistText moderationRationale
    ,jsonValue moderationEvidence,PersistText (T.strip idempotencyKey)]

resolveAppeal
  :: AuthedUser -> UUID -> Text -> MerchAppealDecisionRequest -> AppM Value
resolveAppeal user appealId idempotencyKey MerchAppealDecisionRequest
  {appealOutcome,appealRationale,appealEvidence} = do
  requireAdmin user
  _ <- requireFlag "moderation"
  validateIdempotencyKey idempotencyKey
  when (appealOutcome `notElem` ["upheld","reversed"]
      || T.length (T.strip appealRationale)<20 || T.length appealRationale>3000) $
    throwError err400 {errBody="Appeal outcome or rationale is invalid"}
  runMutation "SELECT merch_reputation_resolve_appeal(?::bigint,?,?,?,?::jsonb,?)"
    [ actorValue user,toPersistValue appealId,PersistText appealOutcome
    , PersistText appealRationale,jsonValue appealEvidence,PersistText (T.strip idempotencyKey)
    ]

requireAdmin :: AuthedUser -> AppM ()
requireAdmin user = unless (hasStrictAdminAccess user) $
  throwError err403 {errBody="Strict Admin access required"}

actorValue :: AuthedUser -> PersistValue
actorValue = PersistInt64 . fromSqlKey . auPartyId

jsonValue :: Value -> PersistValue
jsonValue = toPersistValue . CMS.AesonValue

validateIdempotencyKey :: Text -> AppM ()
validateIdempotencyKey key =
  when (T.length (T.strip key)<8 || T.length (T.strip key)>200
      || T.any (<' ') key) $
    throwError err400 {errBody="Idempotency-Key must contain 8-200 safe characters"}

validateReviewRequest :: Text -> MerchReviewSubmitRequest -> AppM ()
validateReviewRequest key MerchReviewSubmitRequest
  {overallRating,comment,dimensions,images,expectedRevision} = do
  validateIdempotencyKey key
  when (overallRating<1 || overallRating>5 || expectedRevision<0) $
    throwError err400 {errBody="Rating must be 1-5 and revision cannot be negative"}
  case comment of
    Just body | not (T.null (T.strip body))
      && (T.length (T.strip body)<10 || T.length body>3000) ->
        throwError err400 {errBody="Comment must be omitted or contain 10-3000 characters"}
    _ -> pure ()
  case dimensions of
    Object _ -> pure ()
    _ -> throwError err400 {errBody="Dimensions must be an object"}
  when (length (fromMaybe [] images)>4
      || any invalidImage (fromMaybe [] images)) $
    throwError err400 {errBody="At most four images with 3-300 character alt text are allowed"}
  where
    invalidImage MerchReviewImageInput{altText} =
      T.length (T.strip altText)<3 || T.length altText>300

runMutation :: Text -> [PersistValue] -> AppM Value
runMutation statement params = do
  pool <- asks envPool
  result <- liftIO (try (runSqlPool
    (rawSql statement params :: SqlPersistT IO [Single CMS.AesonValue]) pool)
    :: IO (Either SomeException [Single CMS.AesonValue]))
  case result of
    Right [Single value] -> pure (CMS.unAesonValue value)
    Right _ -> throwError err500
    Left exception ->
      case fromException exception :: Maybe SqlError of
        Just sqlError
          | sqlState sqlError=="23505" -> throwError err409 {errBody="Review already exists"}
          | sqlState sqlError=="23514" || sqlState sqlError=="22P02" ->
              throwError err400 {errBody="Invalid merch reputation input"}
          | sqlState sqlError=="P0001" ->
              let message = TE.decodeUtf8With (\_ _ -> Just '\xfffd') (sqlErrorMsg sqlError)
              in if "order claim" `T.isInfixOf` T.toLower message
                   then throwError err404
                   else if "revision conflict" `T.isInfixOf` T.toLower message
                   then throwError err409 {errBody="Content changed elsewhere; reload and retry"}
                   else if "scope" `T.isInfixOf` T.toLower message
                     || "eligible" `T.isInfixOf` T.toLower message
                     || "belongs" `T.isInfixOf` T.toLower message
                     then throwError err403 {errBody="Merch reputation action is not authorized"}
                     else throwError err400 {errBody="Merch reputation request was rejected"}
        _ -> throwError err500

artistStoresSql :: Text
artistStoresSql =
  "SELECT jsonb_build_object('subjectKind','store','id',store.id,'slug',store.slug,'name',store.name,"
  <> "'commercialReputation',true,'reputationLabel','Reputación comercial',"
  <> "'state',coalesce(aggregate.publication_state,'new_store'),"
  <> "'rating',aggregate.public_rating,'verifiedReviewCount',coalesce(aggregate.verified_review_count,0),"
  <> "'confidence',coalesce(aggregate.confidence,'new'),'objectiveSignals',jsonb_build_object("
  <> "'identityVerified',store.identity_verified_at IS NOT NULL,'platformMemberSince',store.created_at)) "
  <> "FROM merch_reputation_store_source store LEFT JOIN LATERAL (SELECT * FROM merch_reputation_aggregate "
  <> "WHERE subject_kind='store' AND subject_id=store.id ORDER BY calculated_through DESC LIMIT 1) aggregate ON true "
  <> "WHERE store.artist_party_id=? AND store.status='published' ORDER BY store.created_at DESC"

storeReputationSql :: Text
storeReputationSql =
  "SELECT jsonb_build_object('subjectKind','store','storeId',store.id,'storeName',store.name,"
  <> "'commercialReputation',true,'reputationLabel','Reputación comercial',"
  <> "'state',coalesce(aggregate.publication_state,'new_store'),'rating',aggregate.public_rating,"
  <> "'verifiedReviewCount',coalesce(aggregate.verified_review_count,0),"
  <> "'historicalReviewCount',coalesce(aggregate.historical_review_count,0),"
  <> "'primaryPeriod',jsonb_build_object('from',aggregate.primary_period_start,'through',aggregate.calculated_through),"
  <> "'confidence',coalesce(aggregate.confidence,'new'),'formulaVersion',aggregate.formula_version_id,"
  <> "'objectiveSignals',jsonb_build_object('identityVerified',store.identity_verified_at IS NOT NULL,"
  <> "'platformMemberSince',store.created_at),"
  <> "'dimensions',coalesce((SELECT jsonb_agg(jsonb_build_object('code',dimension.dimension_code,"
  <> "'average',dimension.public_average,'count',dimension.verified_review_count,'distribution',dimension.distribution) "
  <> "ORDER BY dimension.dimension_code) FROM merch_reputation_dimension_aggregate dimension "
  <> "WHERE dimension.subject_kind='store' AND dimension.subject_id=store.id "
  <> "AND dimension.formula_version_id=aggregate.formula_version_id),'[]'::jsonb),"
  <> "'badges',coalesce((SELECT jsonb_agg(jsonb_build_object('code',award.badge_code,'earnedAt',award.awarded_at,"
  <> "'validUntil',award.valid_until,'nameEs',definition.name_es,'nameEn',definition.name_en,"
  <> "'requirementsEs',definition.requirements_es,'requirementsEn',definition.requirements_en,"
  <> "'minimumSample',definition.minimum_sample,'formulaVersion',award.formula_version_id)) "
  <> "FROM merch_reputation_badge_award award JOIN merch_reputation_badge_definition definition "
  <> "ON definition.code=award.badge_code "
  <> "WHERE award.store_id=store.id AND award.status='active' AND award.valid_until>now()),'[]'::jsonb)) "
  <> "FROM merch_reputation_store_source store LEFT JOIN LATERAL (SELECT * FROM merch_reputation_aggregate "
  <> "WHERE subject_kind='store' AND subject_id=store.id ORDER BY calculated_through DESC LIMIT 1) aggregate ON true "
  <> "WHERE store.id=? AND store.status='published'"

productReputationSql :: Text
productReputationSql =
  "SELECT jsonb_build_object('subjectKind','product','productId',product.id,'productName',product.name,"
  <> "'storeId',product.store_id,'state',coalesce(aggregate.publication_state,'unrated'),"
  <> "'rating',aggregate.public_rating,'verifiedPurchaseReviewCount',coalesce(aggregate.verified_review_count,0),"
  <> "'formulaVersion',aggregate.formula_version_id,'dimensions',coalesce((SELECT jsonb_agg("
  <> "jsonb_build_object('code',dimension.dimension_code,'average',dimension.public_average,"
  <> "'count',dimension.verified_review_count,'distribution',dimension.distribution) ORDER BY dimension.dimension_code) "
  <> "FROM merch_reputation_dimension_aggregate dimension WHERE dimension.subject_kind='product' "
  <> "AND dimension.subject_id=product.id AND dimension.formula_version_id=aggregate.formula_version_id),'[]'::jsonb)) "
  <> "FROM merch_product product LEFT JOIN LATERAL (SELECT * FROM merch_reputation_aggregate "
  <> "WHERE subject_kind='product' AND subject_id=product.id ORDER BY calculated_through DESC LIMIT 1) aggregate ON true "
  <> "WHERE product.id=? AND product.status IN ('published','sold_out','archived')"

publicReviewsSql :: Text
publicReviewsSql =
  "WITH requested AS (SELECT ?::text kind,?::uuid subject_id,?::uuid cursor),"
  <> "boundary AS (SELECT review.created_at,review.id FROM merch_reputation_review review,requested WHERE review.id=requested.cursor),"
  <> "page AS (SELECT review.* FROM merch_reputation_review review JOIN merch_reputation_order_source reviewed_order "
  <> "ON reviewed_order.id=review.order_id,requested WHERE reviewed_order.fraud_state='clear' "
  <> "AND review.review_kind=requested.kind "
  <> "AND (CASE requested.kind WHEN 'store' THEN review.store_id ELSE review.product_id END)=requested.subject_id "
  <> "AND review.status IN ('published','limited') AND (NOT EXISTS (SELECT 1 FROM boundary) "
  <> "OR EXISTS (SELECT 1 FROM boundary WHERE review.created_at<boundary.created_at "
  <> "OR (review.created_at=boundary.created_at AND review.id<boundary.id))) "
  <> "ORDER BY review.created_at DESC,review.id DESC LIMIT ?) "
  <> "SELECT jsonb_build_object('id',page.id,'kind',page.review_kind,'rating',revision.overall_rating,"
  <> "'comment',revision.comment,'createdAt',page.created_at,'updatedAt',revision.submitted_at,"
  <> "'status',page.status,'verifiedPurchase',true,'badge','Compra verificada',"
  <> "'author',CASE WHEN coalesce(privacy.show_public_identity,true) THEN jsonb_build_object("
  <> "'name',author.display_name,'avatarUrl',fan.avatar_url) ELSE jsonb_build_object('name','Comprador verificado') END,"
  <> "'dimensions',(SELECT coalesce(jsonb_object_agg(rating.dimension_code,rating.rating),'{}'::jsonb) "
  <> "FROM merch_review_dimension_rating rating WHERE rating.revision_id=revision.id),"
  <> "'images',(SELECT coalesce(jsonb_agg(jsonb_build_object('assetId',image.media_asset_id,"
  <> "'url','/assets/serve/'||asset.storage_key,'altText',image.alt_text,'position',image.position) "
  <> "ORDER BY image.position),'[]'::jsonb) "
  <> "FROM merch_review_image image JOIN merch_review_media_asset asset ON asset.id=image.media_asset_id "
  <> "WHERE image.revision_id=revision.id AND asset.scan_status='safe' AND asset.moderation_status='published'),"
  <> "'sellerResponse',(SELECT jsonb_build_object('id',response.id,'body',response_revision.body,"
  <> "'updatedAt',response_revision.submitted_at,'status',response.status) FROM merch_seller_response response "
  <> "JOIN merch_seller_response_revision response_revision ON response_revision.response_id=response.id "
  <> "AND response_revision.revision_no=response.current_revision WHERE response.review_id=page.id "
  <> "AND response.status IN ('published','limited'))) "
  <> "FROM page JOIN merch_review_revision revision ON revision.review_id=page.id "
  <> "AND revision.revision_no=page.current_revision "
  <> "JOIN party author ON author.id=page.author_party_id "
  <> "LEFT JOIN merch_review_privacy_preference privacy ON privacy.party_id=author.id "
  <> "LEFT JOIN fan_profile fan ON fan.fan_party_id=author.id "
  <> "ORDER BY page.created_at DESC,page.id DESC"

eligibilitySql :: Text
eligibilitySql =
  "SELECT jsonb_build_object('orderId',orders.id,'storeId',orders.store_id,"
  <> "'orderState',orders.order_state,'fulfillmentState',orders.fulfillment_state,"
  <> "'storeReview',jsonb_build_object('eligible',merch_review_evidence_is_eligible('store',orders.id,?::bigint),"
  <> "'state',CASE WHEN store_review.id IS NULL THEN 'available' "
  <> "WHEN now()<=store_review.edit_deadline THEN 'edit_available' ELSE 'period_expired' END,"
  <> "'reviewId',store_review.id,'currentRevision',coalesce(store_review.current_revision,0),"
  <> "'deadline',coalesce(store_review.edit_deadline,"
  <> "coalesce(orders.delivered_at,orders.pickup_confirmed_at,orders.cancellation_resolved_at,orders.cancelled_at)+interval '30 days')),"
  <> "'productLines',coalesce((SELECT jsonb_agg(jsonb_build_object('lineId',line.id,'productId',line.product_id,"
  <> "'productName',product.name,'fulfillmentState',line.fulfillment_state,"
  <> "'eligible',merch_review_evidence_is_eligible('product',line.id,orders.buyer_party_id),"
  <> "'reviewId',product_review.id,'currentRevision',coalesce(product_review.current_revision,0),"
  <> "'state',CASE WHEN product_review.id IS NULL AND merch_review_evidence_is_eligible('product',line.id,orders.buyer_party_id) "
  <> "THEN 'available' WHEN product_review.id IS NOT NULL AND now()<=product_review.edit_deadline "
  <> "THEN 'edit_available' ELSE 'period_expired' END) ORDER BY line.created_at) "
  <> "FROM merch_reputation_order_line_source line JOIN merch_product product ON product.id=line.product_id "
  <> "LEFT JOIN merch_reputation_review product_review ON product_review.order_line_id=line.id "
  <> "WHERE line.order_id=orders.id),'[]'::jsonb)) "
  <> "FROM merch_reputation_order_source orders LEFT JOIN merch_reputation_review store_review "
  <> "ON store_review.order_id=orders.id AND store_review.review_kind='store' "
  <> "WHERE orders.id=? AND orders.buyer_party_id=?"

sellerStoreSql :: Text
sellerStoreSql =
  "SELECT jsonb_build_object('storeId',store.id,'storeName',store.name,"
  <> "'aggregate',(SELECT to_jsonb(aggregate) FROM merch_reputation_aggregate aggregate "
  <> "WHERE aggregate.subject_kind='store' AND aggregate.subject_id=store.id "
  <> "ORDER BY aggregate.calculated_through DESC LIMIT 1),"
  <> "'reviews',coalesce((SELECT jsonb_agg(jsonb_build_object('reviewId',review.id,"
  <> "'orderId',review.order_id,'status',review.status,'rating',revision.overall_rating,"
  <> "'comment',revision.comment,'currentRevision',review.current_revision,'editDeadline',review.edit_deadline) "
  <> "ORDER BY review.created_at DESC) FROM merch_reputation_review review JOIN merch_review_revision revision "
  <> "ON revision.review_id=review.id AND revision.revision_no=review.current_revision "
  <> "WHERE review.store_id=store.id),'[]'::jsonb)) FROM merch_reputation_store_source store "
  <> "WHERE store.id=? AND EXISTS (SELECT 1 FROM merch_store_member member "
  <> "WHERE member.store_id=store.id AND member.party_id=? AND member.invitation_status='accepted')"

moderationCasesSql :: Text
moderationCasesSql =
  "SELECT jsonb_build_object('caseId',moderation_case.id,'state',moderation_case.state,"
  <> "'openedAt',moderation_case.opened_at,'assignedTo',moderation_case.assigned_to,"
  <> "'report',jsonb_build_object('id',report.id,'targetType',report.target_type,"
  <> "'targetId',report.target_id,'reason',report.reason,'details',report.details,"
  <> "'authorizedEvidence',report.authorized_evidence,'reporterPartyId',report.reporter_party_id),"
  <> "'decisions',coalesce((SELECT jsonb_agg(to_jsonb(decision) ORDER BY decision.decided_at) "
  <> "FROM merch_reputation_moderation_decision decision WHERE decision.case_id=moderation_case.id),'[]'::jsonb),"
  <> "'appeals',coalesce((SELECT jsonb_agg(to_jsonb(appeal) ORDER BY appeal.created_at) "
  <> "FROM merch_reputation_appeal appeal JOIN merch_reputation_moderation_decision decision "
  <> "ON decision.id=appeal.decision_id WHERE decision.case_id=moderation_case.id),'[]'::jsonb)) "
  <> "FROM merch_reputation_moderation_case moderation_case "
  <> "JOIN merch_reputation_report report ON report.id=moderation_case.report_id "
  <> "WHERE (? IS NULL OR moderation_case.state=?) ORDER BY moderation_case.opened_at DESC LIMIT 100"

categorySuggestionsSql :: Text
categorySuggestionsSql =
  "SELECT jsonb_build_object('suggestionId',suggestion.id,'subjectKind',suggestion.subject_kind,"
  <> "'label',suggestion.label,'definition',suggestion.definition,'status',suggestion.status,"
  <> "'minimumSample',suggestion.minimum_sample,'biasTest',suggestion.bias_test,"
  <> "'utilityTest',suggestion.utility_test,'decisionReason',suggestion.decision_reason,"
  <> "'createdAt',suggestion.created_at,'decidedAt',suggestion.decided_at) "
  <> "FROM merch_reputation_category_suggestion suggestion "
  <> "WHERE (? IS NULL OR suggestion.status=?) ORDER BY suggestion.created_at DESC LIMIT 100"

prioritiesSql :: Text
prioritiesSql =
  "WITH profile AS (SELECT * FROM merch_reputation_priority_profile "
  <> "WHERE party_id=? AND subject_kind=?), current_revision AS (SELECT revision.id "
  <> "FROM merch_reputation_priority_revision revision JOIN profile "
  <> "ON profile.party_id=revision.party_id AND profile.subject_kind=revision.subject_kind "
  <> "AND profile.current_revision=revision.revision_no) "
  <> "SELECT jsonb_build_object('subjectKind',?,'revision',coalesce((SELECT current_revision FROM profile),0),"
  <> "'affectsPublicScore',false,'orderedDimensions',coalesce(jsonb_agg(jsonb_build_object("
  <> "'code',dimension.code,'nameEs',dimension.name_es,'nameEn',dimension.name_en,"
  <> "'definitionEs',dimension.definition_es,'definitionEn',dimension.definition_en) "
  <> "ORDER BY coalesce(item.position,100),dimension.code),'[]'::jsonb)) "
  <> "FROM merch_reputation_dimension dimension LEFT JOIN current_revision ON true "
  <> "LEFT JOIN merch_reputation_priority_item item ON item.revision_id=current_revision.id "
  <> "AND item.dimension_code=dimension.code WHERE dimension.subject_kind=? "
  <> "AND dimension.governed AND dimension.status='active'"

notificationPreferencesSql :: Text
notificationPreferencesSql =
  "SELECT jsonb_build_object("
  <> "'reviewInvitation',coalesce(preference.review_invitation,false),"
  <> "'reviewReminder',coalesce(preference.review_reminder,false),"
  <> "'sellerResponseNotification',coalesce(preference.seller_response,false),"
  <> "'moderationChange',coalesce(preference.moderation_change,false),"
  <> "'evidenceRequest',coalesce(preference.evidence_request,false),"
  <> "'appealResult',coalesce(preference.appeal_result,false),"
  <> "'badgeChange',coalesce(preference.badge_change,false)) "
  <> "FROM (SELECT 1) singleton LEFT JOIN merch_reputation_notification_preference preference "
  <> "ON preference.party_id=?"
