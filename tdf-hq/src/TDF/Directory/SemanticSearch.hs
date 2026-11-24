{-# LANGUAGE OverloadedStrings #-}

-- | Semantic term of directory search (DIR-SEMANTIC-SEARCH-001).
--
-- Directory search ranks a document by a weighted sum whose @semantic_score@
-- used to be 1 when the query matched a catalog alias of the document and 0
-- otherwise. Behind @DIRECTORY_SEMANTIC_SEARCH_ENABLED@ the term becomes
--
-- > max (alias, clamp 0 1 (cosine (E query) (E document)))
--
-- for directory profiles and classifieds. Similarity reorders documents that
-- already match the text filter; it never admits one.
--
-- This module is pure. It holds the ranking arithmetic as exact rationals (the
-- reference the SQL is tested and the solver obligations are stated against),
-- the decision between today's SQL and the semantic SQL, the text that may be
-- sent to the embedding provider, and the SQL text itself. The contract is
-- @formal/system/directory-semantic-search.md@.
module TDF.Directory.SemanticSearch
  ( -- * Constants
    semanticSearchFlagName
  , embeddingProviderName
  , embeddingDimension
  , minQueryChars
  , queryTimeoutMicros
  , queryCallsPerMinute
  , sweepSeconds
  , embeddedTextCap
  , queryCacheDays
  , queryCacheMaxRows
    -- * Ranking reference
  , TitleMatch (..)
  , RankInputs (..)
  , weightText
  , weightSemantic
  , weightProximity
  , weightQuality
  , weightActivity
  , weightAvailability
  , weightReputation
  , weightDivisor
  , imageBoost
  , clampUnit
  , textScore
  , semanticScore
  , proximityScore
  , activityScore
  , organicScore
  , rankScore
  , RankedDocument (..)
  , compareRanked
    -- * Plan
  , QueryEmbedding (..)
  , SearchPlan (..)
  , searchPlan
  , plannedSearch
  , queryEligible
    -- * Text sent to the provider
  , normalizeQueryText
  , redactContactDetails
  , embeddingInput
  , queryCacheKeyInput
    -- * Vectors
  , usableVector
  , vectorLiteral
    -- * SQL
  , semanticSearchSql
  , semanticInputAnchor
  , semanticAliasOpenAnchor
  , semanticAliasCloseAnchor
  , similaritySql
  , documentHashSql
  , eligibleDocumentFromSql
  , claimDocumentsSql
  , writeDocumentEmbeddingSql
  , purgeDocumentEmbeddingsSql
  , lookupQueryEmbeddingSql
  , storeQueryEmbeddingSql
  , spendQueryBudgetSql
  , pruneQueryCacheSql
  , pruneQueryBudgetSql
  ) where

import           Data.Char  (isDigit, isSpace)
import           Data.Ord   (Down (..), comparing)
import           Data.Ratio ((%))
import           Data.Text  (Text)
import qualified Data.Text  as T

-- | Environment flag; absent or false keeps today's ranking.
semanticSearchFlagName :: Text
semanticSearchFlagName = "DIRECTORY_SEMANTIC_SEARCH_ENABLED"

-- | Stored with every vector. Vectors of another provider or model are never
-- compared: the local bag-of-words fallback of the RAG index is not used here.
embeddingProviderName :: Text
embeddingProviderName = "openai"

-- | Dimension of the @vector@ columns created by the migration.
embeddingDimension :: Int
embeddingDimension = 1536

-- | Shorter normalised queries keep the alias term only.
minQueryChars :: Int
minQueryChars = 3

-- | Longest wait for a query embedding inside a request (400 ms).
queryTimeoutMicros :: Int
queryTimeoutMicros = 400000

-- | Provider calls the search route may make per minute, across processes.
queryCallsPerMinute :: Int
queryCallsPerMinute = 60

-- | Pause between two background sweeps.
sweepSeconds :: Int
sweepSeconds = 60

-- | Characters of a document sent to the provider, after redaction.
embeddedTextCap :: Int
embeddedTextCap = 6000

-- | A cached query embedding is dropped after this many days.
queryCacheDays :: Int
queryCacheDays = 30

-- | At most this many cached query embeddings are kept (newest first).
queryCacheMaxRows :: Int
queryCacheMaxRows = 5000

--------------------------------------------------------------------------------
-- Ranking reference

-- | How the normalised title relates to the normalised query: the tiers of
-- the @text_score@ expression in @searchSql@.
data TitleMatch
  = NoQuery
    -- ^ empty query (browsing)
  | TitleExact
  | TitlePrefix
  | TitleContains
  | TitleWeak !Rational
    -- ^ full-text rank or trigram similarity; clamped to at most 1
  deriving (Eq, Show)

-- | Everything the ranking formula reads about one document.
data RankInputs = RankInputs
  { riTitle        :: !TitleMatch
  , riAlias        :: !Bool
    -- ^ the query matches a catalog alias of the document's taxonomy
  , riSimilarity   :: !(Maybe Rational)
    -- ^ cosine of a usable embedding pair; 'Nothing' when there is none
  , riDistanceKm   :: !(Maybe Rational)
  , riQuality      :: !Rational
  , riAgeSeconds   :: !Rational
  , riAvailability :: !Rational
  , riReputation   :: !Rational
  , riHasImage     :: !Bool
  } deriving (Eq, Show)

weightText, weightSemantic, weightProximity, weightQuality, weightActivity,
  weightAvailability, weightReputation, weightDivisor, imageBoost :: Rational
weightText = 40 % 100
weightSemantic = 15 % 100
weightProximity = 15 % 100
weightQuality = 10 % 100
weightActivity = 8 % 100
weightAvailability = 5 % 100
weightReputation = 7 % 100
weightDivisor = 5
imageBoost = 10 % 100

clampUnit :: Rational -> Rational
clampUnit = max 0 . min 1

textScore :: TitleMatch -> Rational
textScore NoQuery          = 1 % 2
textScore TitleExact       = 10
textScore TitlePrefix      = 7
textScore TitleContains    = 4
textScore (TitleWeak rank) = min 1 rank

-- | The semantic term. With no usable embedding pair it is exactly the alias
-- term, which is today's value.
semanticScore :: TitleMatch -> Bool -> Maybe Rational -> Rational
semanticScore NoQuery _ _ = 0
semanticScore _ alias similarity =
  max (if alias then 1 else 0) (maybe 0 (roundTo 4 . clampUnit) similarity)

proximityScore :: Maybe Rational -> Rational
proximityScore Nothing         = 35 % 100
proximityScore (Just distance) = 1 / (1 + distance / 25)

-- | Thirty days halve the weight of an update.
activityScore :: Rational -> Rational
activityScore ageSeconds = min 1 (1 / (1 + ageSeconds / 2592000))

-- | The public @score@: the weighted sum divided by five, at six decimals.
organicScore :: RankInputs -> Rational
organicScore inputs =
  roundTo 6
    ( ( weightText * textScore (riTitle inputs)
          + weightSemantic
            * semanticScore (riTitle inputs) (riAlias inputs) (riSimilarity inputs)
          + weightProximity * proximityScore (riDistanceKm inputs)
          + weightQuality * riQuality inputs
          + weightActivity * activityScore (riAgeSeconds inputs)
          + weightAvailability * riAvailability inputs
          + weightReputation * riReputation inputs
      )
        / weightDivisor
    )

-- | The value results are ordered by: a picture adds a fixed boost.
rankScore :: RankInputs -> Rational
rankScore inputs =
  organicScore inputs + (if riHasImage inputs then imageBoost else 0)

-- | The columns of @ORDER BY rank_score DESC, source_updated_at DESC,
-- entity_kind, entity_id@. Kind and id are the primary key of a search
-- document, so two distinct documents never compare equal.
data RankedDocument = RankedDocument
  { rdRankScore :: !Rational
  , rdUpdatedAt :: !Rational
  , rdKind      :: !Text
  , rdId        :: !Text
  } deriving (Eq, Show)

compareRanked :: RankedDocument -> RankedDocument -> Ordering
compareRanked =
  comparing (Down . rdRankScore)
    <> comparing (Down . rdUpdatedAt)
    <> comparing rdKind
    <> comparing rdId

-- | Round half away from zero, as PostgreSQL rounds @numeric@.
roundTo :: Int -> Rational -> Rational
roundTo places value =
  let scale = 10 ^ places :: Integer
      scaled = value * fromIntegral scale
      magnitude = floor (abs scaled + 1 % 2) :: Integer
  in fromIntegral (if scaled < 0 then negate magnitude else magnitude)
       / fromIntegral scale

--------------------------------------------------------------------------------
-- Plan

-- | A query vector with the space it lives in. The vector is carried as the
-- pgvector literal that is bound to the statement.
data QueryEmbedding = QueryEmbedding
  { qeProvider :: !Text
  , qeModel    :: !Text
  , qeVector   :: !Text
  } deriving (Eq, Show)

data SearchPlan
  = LexicalPlan
    -- ^ run today's @searchSql@ text, unchanged
  | SemanticPlan !QueryEmbedding
  deriving (Eq, Show)

-- | The semantic SQL runs only with the flag on and a query embedding in
-- hand. In every other case the request executes today's statement, so its
-- result is today's result.
searchPlan :: Bool -> Maybe QueryEmbedding -> SearchPlan
searchPlan True (Just embedding) = SemanticPlan embedding
searchPlan _ _                   = LexicalPlan

-- | The statement a plan executes and the parameters it adds after the
-- lexical parameters of the @input@ row. A lexical plan, and a semantic plan whose
-- statement cannot be derived, return today's statement and no parameter.
plannedSearch :: SearchPlan -> Text -> (Text, [Text])
plannedSearch LexicalPlan lexical = (lexical, [])
plannedSearch (SemanticPlan embedding) lexical =
  case semanticSearchSql lexical of
    Just semantic -> (semantic, [qeVector embedding, qeProvider embedding, qeModel embedding])
    Nothing       -> (lexical, [])

-- | Whether a query is worth an embedding at all.
queryEligible :: Bool -> Text -> Bool
queryEligible flagOn query =
  flagOn && T.length (normalizeQueryText query) >= minQueryChars

--------------------------------------------------------------------------------
-- Text sent to the provider

-- | Lower case, contact details removed, single spaces.
normalizeQueryText :: Text -> Text
normalizeQueryText = redactContactDetails . T.toLower

-- | Remove what looks like an e-mail address or a telephone number and
-- collapse white space. Total and idempotent; the result holds no @\@@ and no
-- run of digits and telephone punctuation with seven or more digits.
redactContactDetails :: Text -> Text
redactContactDetails =
  T.unwords . T.words . dropPhones . T.unwords . filter (not . T.any (== '@')) . T.words
  where
    dropPhones = T.concat . map dropPhoneRun . T.groupBy sameClass
    sameClass a b = isPhoneChar a == isPhoneChar b
    dropPhoneRun run
      | T.any isPhoneChar run && T.length (T.filter isDigit run) >= 7 = " "
      | otherwise = run

isPhoneChar :: Char -> Bool
isPhoneChar c = isDigit c || isSpace c || c `elem` ("+-().·/" :: String)

-- | The text of one document as it is sent: redacted and capped.
embeddingInput :: Text -> Text
embeddingInput = T.stripEnd . T.take embeddedTextCap . redactContactDetails

-- | The string whose SHA-256 keys the query cache. The query text itself is
-- not stored.
queryCacheKeyInput :: Text -> Text -> Text -> Text
queryCacheKeyInput provider model query =
  T.intercalate "\n" [provider, model, normalizeQueryText query]

--------------------------------------------------------------------------------
-- Vectors

-- | A provider vector is stored or compared only with the expected dimension,
-- finite components and a non-zero norm. pgvector's cosine distance of a zero
-- vector is NaN, which PostgreSQL orders above every number.
usableVector :: Int -> [Double] -> Bool
usableVector dimension values =
  length values == dimension
    && all finite values
    && any (/= 0) values
  where
    finite value = not (isNaN value || isInfinite value)

vectorLiteral :: [Double] -> Text
vectorLiteral values =
  "[" <> T.intercalate "," (map (T.pack . show) values) <> "]"

--------------------------------------------------------------------------------
-- SQL

-- | End of the @input@ row of @searchSql@; the semantic statement appends the
-- query vector and its space here.
semanticInputAnchor :: Text
semanticInputAnchor = "tax_genre_ids), directory_query_aliases AS MATERIALIZED ("

-- | Start and end of the alias term of @searchSql@.
semanticAliasOpenAnchor, semanticAliasCloseAnchor :: Text
semanticAliasOpenAnchor =
  "text_score,CASE WHEN input.q='' THEN 0 WHEN EXISTS (SELECT 1 FROM catalog_search_alias"
semanticAliasCloseAnchor = "THEN 1 ELSE 0 END semantic_score "

-- | SHA-256 of the revision of @document@: its displayed title, subtitle and
-- summary as stored, its search text and its source version. An embedding is
-- used only while it carries the hash of the current revision. The search
-- text alone is not enough: it is a lossy normalisation, so an edit it
-- discards (non-Latin text, for one) would keep a stale embedding. The source
-- version changes with every edit of the profile or classified, which covers
-- the profile fields that are sent but are not columns of the document.
documentHashSql :: Text
documentHashSql =
  "encode(sha256(convert_to(concat_ws(chr(31),document.title,coalesce(document.subtitle,''),coalesce(document.summary,''),document.search_text,document.source_version::text),'UTF8')),'hex')"

-- | Similarity of @document@ to the query vector of @input@: 0 without a
-- usable pair, otherwise the cosine clamped to [0, 1] at four decimals.
similaritySql :: Text
similaritySql =
  "coalesce((SELECT round(least(1,greatest(0,coalesce(nullif(1-(emb.embedding<=>input.q_embedding),'NaN'::float8),0)))::numeric,4) "
    <> "FROM directory_search_embedding emb WHERE input.q<>'' AND emb.entity_kind=document.entity_kind AND emb.entity_id=document.entity_id "
    <> "AND emb.provider=input.q_provider AND emb.model=input.q_model AND emb.content_hash="
    <> documentHashSql
    <> "),0)"

-- | Derive the semantic statement from today's statement. Exactly three
-- spans change: the @input@ row gains the query vector, and the alias term is
-- wrapped in @greatest(alias, similarity)@. 'Nothing' when an anchor is not
-- found exactly once, in which case the caller keeps today's statement.
--
-- The statement takes three more parameters than today's, bound right after
-- the lexical parameters of the @input@ row: vector literal, provider, model.
semanticSearchSql :: Text -> Maybe Text
semanticSearchSql lexical =
  replaceOnce semanticInputAnchor
    "tax_genre_ids,?::vector q_embedding,?::text q_provider,?::text q_model), directory_query_aliases AS MATERIALIZED ("
    lexical
    >>= replaceOnce semanticAliasOpenAnchor
      "text_score,greatest(CASE WHEN input.q='' THEN 0 WHEN EXISTS (SELECT 1 FROM catalog_search_alias"
    >>= replaceOnce semanticAliasCloseAnchor
      ("THEN 1 ELSE 0 END," <> similaritySql <> ") semantic_score ")

replaceOnce :: Text -> Text -> Text -> Maybe Text
replaceOnce needle replacement haystack =
  case T.breakOnAll needle haystack of
    [(before, rest)] -> Just (before <> replacement <> T.drop (T.length needle) rest)
    _                -> Nothing

-- | The documents that may have an embedding: the profiles and classifieds
-- directory search itself returns. The predicate on the document row repeats
-- @directory_public_search_document@, including its effective and expiry
-- times, so the row can be locked; the profile predicate is the one of
-- @searchDocumentsSql@.
eligibleDocumentFromSql :: Text
eligibleDocumentFromSql =
  "FROM directory_search_document document "
    <> "LEFT JOIN directory_profile profile ON document.entity_kind='profile' AND profile.id::text=document.entity_id "
    <> "WHERE document.entity_kind IN ('profile','classified') "
    <> "AND document.source_status='published' AND document.visibility='public' AND document.moderation_status='allowed' "
    <> "AND (document.effective_at IS NULL OR document.effective_at<=now()) AND (document.expires_at IS NULL OR document.expires_at>now()) "
    <> "AND (document.entity_kind<>'profile' OR (profile.profile_status='published' AND profile.visibility='public' AND profile.moderation_status='allowed' AND profile.canonical_profile_id IS NULL)) "

-- | Eligible documents with no embedding of their current text in the
-- current space, oldest change first. Returns kind, id, hash and the public
-- text. Parameters: provider, model, limit, offset. The offset lets one sweep
-- read past documents it cannot send (their text redacts to nothing), which
-- have no embedding and would otherwise be claimed first on every sweep.
claimDocumentsSql :: Text
claimDocumentsSql =
  "SELECT document.entity_kind,document.entity_id,"
    <> documentHashSql
    <> ",CASE WHEN document.entity_kind='profile' THEN concat_ws(E'\\n',profile.public_name,document.subtitle,profile.bio,profile.experience_summary,profile.credits_summary,profile.equipment_summary,"
    <> "(SELECT string_agg(coalesce(term.name_es,term.name_en),', ') FROM directory_profile_genre member JOIN genre term ON term.id=member.genre_id WHERE member.profile_id=profile.id),"
    <> "(SELECT string_agg(coalesce(term.name_es,term.name_en),', ') FROM directory_profile_service member JOIN service_offering term ON term.id=member.service_offering_id WHERE member.profile_id=profile.id)) "
    <> "ELSE concat_ws(E'\\n',document.title,document.subtitle,document.summary) END "
    <> eligibleDocumentFromSql
    <> "AND NOT EXISTS (SELECT 1 FROM directory_search_embedding emb WHERE emb.entity_kind=document.entity_kind AND emb.entity_id=document.entity_id AND emb.provider=? AND emb.model=? AND emb.content_hash="
    <> documentHashSql
    <> ") ORDER BY document.source_updated_at,document.entity_kind,document.entity_id LIMIT ? OFFSET ?"

-- | Store one vector, but only if the document is still eligible and still
-- has the claimed text. The document row is locked, so a concurrent
-- unpublication either precedes this statement (nothing is written) or waits
-- for it (its trigger then deletes the row). Parameters: hash, provider,
-- model, vector literal, kind, id, hash.
writeDocumentEmbeddingSql :: Text
writeDocumentEmbeddingSql =
  "INSERT INTO directory_search_embedding (entity_kind,entity_id,content_hash,provider,model,embedding,embedded_at) "
    <> "SELECT document.entity_kind,document.entity_id,?,?,?,?::vector,now() "
    <> eligibleDocumentFromSql
    <> "AND document.entity_kind=? AND document.entity_id=? AND "
    <> documentHashSql
    <> "=? FOR SHARE OF document "
    <> "ON CONFLICT (entity_kind,entity_id) DO UPDATE SET content_hash=EXCLUDED.content_hash,provider=EXCLUDED.provider,model=EXCLUDED.model,embedding=EXCLUDED.embedding,embedded_at=EXCLUDED.embedded_at"

-- | Backstop for the trigger: drop every embedding whose document is not
-- eligible or has other text.
purgeDocumentEmbeddingsSql :: Text
purgeDocumentEmbeddingsSql =
  "DELETE FROM directory_search_embedding stale WHERE NOT EXISTS (SELECT 1 "
    <> eligibleDocumentFromSql
    <> "AND document.entity_kind=stale.entity_kind AND document.entity_id=stale.entity_id AND "
    <> documentHashSql
    <> "=stale.content_hash)"

-- | Parameters: query hash, provider, model.
lookupQueryEmbeddingSql :: Text
lookupQueryEmbeddingSql =
  "SELECT embedding::text FROM directory_query_embedding WHERE query_hash=? AND provider=? AND model=?"

-- | Parameters: query hash, provider, model, vector literal.
storeQueryEmbeddingSql :: Text
storeQueryEmbeddingSql =
  "INSERT INTO directory_query_embedding (query_hash,provider,model,embedding,created_at) VALUES (?,?,?,?::vector,now()) "
    <> "ON CONFLICT (query_hash) DO UPDATE SET provider=EXCLUDED.provider,model=EXCLUDED.model,embedding=EXCLUDED.embedding,created_at=EXCLUDED.created_at"

-- | Count one provider call in the current minute and say whether it is
-- within the budget. Parameter: calls allowed per minute.
spendQueryBudgetSql :: Text
spendQueryBudgetSql =
  "INSERT INTO directory_query_embedding_budget (window_started_at,calls) VALUES (date_trunc('minute',now()),1) "
    <> "ON CONFLICT (window_started_at) DO UPDATE SET calls=directory_query_embedding_budget.calls+1 RETURNING calls<=?"

-- | Parameters: days kept, rows kept.
pruneQueryCacheSql :: Text
pruneQueryCacheSql =
  "DELETE FROM directory_query_embedding WHERE created_at<now()-make_interval(days => ?::int) "
    <> "OR query_hash IN (SELECT query_hash FROM directory_query_embedding ORDER BY created_at DESC,query_hash OFFSET ?)"

pruneQueryBudgetSql :: Text
pruneQueryBudgetSql =
  "DELETE FROM directory_query_embedding_budget WHERE window_started_at<now()-interval '1 day'"
