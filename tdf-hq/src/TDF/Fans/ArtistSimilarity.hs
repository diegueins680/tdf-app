{-# LANGUAGE OverloadedStrings #-}

-- | Multi-signal artist similarity (COMMUNITY-ARTIST-SIMILARITY-001,
-- formal/system/artist-similarity.md).
--
-- Every fan gives every artist an integer weight from four signals: a follow,
-- a favourited recording, an accepted RSVP to a public event with the artist
-- in the lineup, and recent eligible plays. Two artists are similar by the
-- cosine of their weighted fan vectors, once at least 'minimumSupport'
-- distinct fans back the pair.
--
-- 'similarityFromWeights' is the single definition of that relation. The
-- nightly job stores its integer output ('runSimilarityGeneration'); a request
-- either reads the stored rows or recomputes them from the live signals (the
-- reference). Both feed the same 'rankBySimilarity', which does the division,
-- so the two paths cannot disagree on arithmetic.
module TDF.Fans.ArtistSimilarity
  ( -- * Constants
    followPoints
  , favouritePoints
  , rsvpPoints
  , playPoints
  , maxSignalWeight
  , maxSeedWeight
  , minimumSupport
  , playWindowDays
  , maxGenerationAgeHours
    -- * Pure core
  , SignalInputs (..)
  , PairStat (..)
  , Similarity (..)
  , signalWeights
  , similarityFromWeights
  , similarityValue
  , restrictSimilarity
  , rankBySimilarity
    -- * SQL
  , followSignalsSql
  , favouriteSignalsSql
  , rsvpSignalsSql
  , playSignalsSql
  , favouriteArtistsForFanSql
  , rsvpArtistsForFanSql
  , playCountsForFanSql
  , optedOutFansSql
  , fanPersonalisedSql
  , snapshotClockSql
  , insertGenerationSql
  , insertNormsSql
  , insertPairsSql
  , pruneGenerationsSql
  , generationSimilaritySql
    -- * Loaders
  , SimilaritySource (..)
  , GenerationSummary (..)
  , loadSignalSimilarity
  , runSimilarityGeneration
  , loadSimilarityRecommendations
  ) where

import           Control.Exception             (throwIO)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Int                      (Int64)
import           Data.List                     (sortOn)
import qualified Data.Map.Strict               as Map
import           Data.Ord                      (Down (..))
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime, addUTCTime, nominalDay)
import           Database.Persist              (PersistValue, toPersistValue)
import           Database.Persist.Sql          (Single (..), SqlPersistT, fromSqlKey, rawExecute, rawSql)

import           TDF.Fans.ArtistRecommendations
import           TDF.Models                    (PartyId)
import           TDF.Profiles.ArtistVisibility (loadPubliclyListedArtistParties)

-- | Points of a follow.
followPoints :: Int
followPoints = 8

-- | Points of at least one favourited recording of the artist.
favouritePoints :: Int
favouritePoints = 8

-- | Points of at least one accepted RSVP to a public event of the artist.
rsvpPoints :: Int
rsvpPoints = 4

-- | Points of @n@ eligible plays inside the window: repeated plays saturate
-- at 3, below an RSVP.
playPoints :: Int -> Int
playPoints plays
  | plays >= 10 = 3
  | plays >= 3 = 2
  | plays >= 1 = 1
  | otherwise = 0

-- | Largest weight one fan can give one artist.
maxSignalWeight :: Int
maxSignalWeight = followPoints + favouritePoints + rsvpPoints + playPoints 10

-- | The caller's weight on a seed counts up to this much (a follow).
maxSeedWeight :: Int
maxSeedWeight = followPoints

-- | Distinct shared fans required before two artists count as similar.
minimumSupport :: Int
minimumSupport = 2

-- | Plays older than this many days before the snapshot are not evidence.
playWindowDays :: Int
playWindowDays = 180

-- | A stored generation is served while its snapshot is at most this old:
-- two job periods of 24 hours.
maxGenerationAgeHours :: Int
maxGenerationAgeHours = 48

-- | Signal rows as @(fan, artist)@; plays carry a count.
data SignalInputs = SignalInputs
  { siFollows    :: ![(Int64, Int64)]
  , siFavourites :: ![(Int64, Int64)]
  , siRsvps      :: ![(Int64, Int64)]
  , siPlays      :: ![(Int64, Int64, Int)]
  } deriving (Eq, Show)

-- | The weight of every @(fan, artist)@ with at least one signal.
--
-- Only @listed@ artists have a vector. A party's signals on its own artist
-- profile are ignored. For a fan in @optedOut@ only the follow, which is
-- public, counts. Repeated rows count once; play counts add up and negative
-- counts are ignored. Every weight is in @1..'maxSignalWeight'@.
signalWeights
  :: Set.Set Int64
  -> Set.Set Int64
  -> SignalInputs
  -> Map.Map (Int64, Int64) Int
signalWeights listed optedOut inputs =
  Map.filter (> 0) (Map.unionsWith (+) [follows, favourites, rsvps, plays])
  where
    admitted (fan, artist) = fan /= artist && Set.member artist listed
    private (fan, _) = not (Set.member fan optedOut)
    once points rows = Map.fromList [(row, points) | row <- rows, admitted row]
    follows = once followPoints (siFollows inputs)
    favourites = once favouritePoints (filter private (siFavourites inputs))
    rsvps = once rsvpPoints (filter private (siRsvps inputs))
    plays =
      Map.map playPoints $
        Map.fromListWith (+)
          [ ((fan, artist), max 0 count)
          | (fan, artist, count) <- siPlays inputs
          , admitted (fan, artist)
          , private (fan, artist)
          ]

-- | Shared evidence of one pair of artists.
data PairStat = PairStat
  { psDot     :: !Int64
    -- ^ sum over fans of the product of their two weights
  , psSupport :: !Int
    -- ^ distinct fans with a weight on both
  } deriving (Eq, Show)

-- | The similarity relation in integers. 'simPairs' is keyed by
-- @(smaller id, larger id)@ and holds only pairs with at least
-- 'minimumSupport' fans; 'simNorms' is the squared length of each vector.
data Similarity = Similarity
  { simNorms :: !(Map.Map Int64 Int64)
  , simPairs :: !(Map.Map (Int64, Int64) PairStat)
  } deriving (Eq, Show)

-- | The similarity relation of a weight matrix. Non-positive weights are
-- ignored.
similarityFromWeights :: Map.Map (Int64, Int64) Int -> Similarity
similarityFromWeights weights =
  Similarity
    { simNorms =
        Map.fromListWith (+)
          [(artist, square weight) | ((_, artist), weight) <- positive]
    , simPairs =
        Map.filter ((>= minimumSupport) . psSupport) $
          Map.fromListWith add
            [ ((a, b), PairStat (fromIntegral wa * fromIntegral wb) 1)
            | artists <- Map.elems byFan
            , (a, wa) <- artists
            , (b, wb) <- artists
            , a < b
            ]
    }
  where
    positive = [entry | entry@(_, weight) <- Map.toList weights, weight > 0]
    square weight = fromIntegral weight * fromIntegral weight
    byFan :: Map.Map Int64 [(Int64, Int)]
    byFan =
      Map.fromListWith (++) [(fan, [(artist, weight)]) | ((fan, artist), weight) <- positive]
    add left right =
      PairStat (psDot left + psDot right) (psSupport left + psSupport right)

-- | Cosine similarity of two artists, in @0..1@. Zero for an artist with
-- itself, for a pair below the minimum support and for any row that is not a
-- possible output of 'similarityFromWeights'.
similarityValue :: Similarity -> Int64 -> Int64 -> Double
similarityValue similarity a b
  | a == b = 0
  | otherwise =
      case Map.lookup (min a b, max a b) (simPairs similarity) of
        Just stat
          | psSupport stat >= minimumSupport
          , psDot stat > 0
          , Just normA <- Map.lookup a (simNorms similarity)
          , Just normB <- Map.lookup b (simNorms similarity)
          , normA > 0
          , normB > 0 ->
              min 1 (fromIntegral (psDot stat) / sqrt (fromIntegral normA * fromIntegral normB))
        _ -> 0

-- | The part of a similarity a request with these seeds reads from the table:
-- the pairs that touch a seed and the norms of the artists in those pairs.
restrictSimilarity :: Set.Set Int64 -> Similarity -> Similarity
restrictSimilarity seeds similarity =
  Similarity
    { simNorms = Map.restrictKeys (simNorms similarity) mentioned
    , simPairs = pairs
    }
  where
    pairs =
      Map.filterWithKey
        (\(a, b) _ -> Set.member a seeds || Set.member b seeds)
        (simPairs similarity)
    mentioned = Set.fromList (concat [[a, b] | (a, b) <- Map.keys pairs])

-- | Rank at most @limit@ artists for one fan from a similarity relation.
--
-- @seedWeights@ are the fan's own weights. A seed must be @eligible@; a
-- result must be @eligible@ and not @excluded@. A candidate's score is the
-- sum over seeds of @min weight 'maxSeedWeight' / 'maxSeedWeight'@ times the
-- similarity, so it is at most the number of seeds. Similar artists are
-- ordered by score, then follower count, then id, and are followed by the
-- popularity fallback of 'rankArtistRecommendations'. Without seeds the
-- result is exactly that fallback.
rankBySimilarity
  :: Int
  -> Set.Set Int64
  -> Set.Set Int64
  -> Map.Map Int64 Int
  -> Map.Map Int64 Int
  -> Similarity
  -> [ArtistRecommendation]
rankBySimilarity limit eligible excluded followers seedWeights similarity
  | limit <= 0 = []
  | otherwise = take limit (similar ++ popular)
  where
    followerCount artistId = Map.findWithDefault 0 artistId followers
    recommendable artistId =
      Set.member artistId eligible && not (Set.member artistId excluded)

    strengths :: Map.Map Int64 Double
    strengths =
      Map.fromList
        [ (seed, fromIntegral (min weight maxSeedWeight) / fromIntegral maxSeedWeight)
        | (seed, weight) <- Map.toList seedWeights
        , weight > 0
        , Set.member seed eligible
        ]

    -- Ascending (candidate, seed): the sum below always adds in the same order.
    contributions :: Map.Map Int64 [(Int64, Double)]
    contributions =
      Map.map Map.toAscList $
        Map.fromListWith Map.union
          [ (candidate, Map.singleton seed weight)
          | (a, b) <- Map.keys (simPairs similarity)
          , (seed, candidate) <- [(a, b), (b, a)]
          , Just strength <- [Map.lookup seed strengths]
          , recommendable candidate
          , let weight = strength * similarityValue similarity seed candidate
          , weight > 0
          ]

    similar =
      sortOn rankKey
        [ ArtistRecommendation
            { recArtistId = candidate
            , recReason = SimilarFans
            , recScore = sum (map snd seeds)
            , recSeedArtistIds =
                take maxRecommendationSeeds
                  (map fst (sortOn (\(seed, weight) -> (Down weight, seed)) seeds))
            }
        | (candidate, seeds) <- Map.toList contributions
        ]

    rankKey item =
      (Down (recScore item), Down (followerCount (recArtistId item)), recArtistId item)

    popular =
      rankArtistRecommendations
        limit
        eligible
        (excluded `Set.union` Map.keysSet contributions)
        followers
        []

-- | Every follow as @(fan, artist)@ (no parameters).
followSignalsSql :: Text
followSignalsSql =
  "SELECT fan_party_id, artist_party_id FROM fan_follow"

-- | Every @(fan, artist)@ where the fan favourited a recording released by
-- the artist (no parameters).
favouriteSignalsSql :: Text
favouriteSignalsSql =
  "SELECT DISTINCT favourite.party_id, release.artist_party_id \
  \FROM music_favorite favourite \
  \JOIN music_release_track track ON track.recording_id = favourite.recording_id \
  \JOIN music_release_version version ON version.id = track.release_version_id \
  \JOIN music_release release ON release.id = version.release_id"

-- | Every @(fan, artist)@ where the fan accepted an RSVP to a public event
-- with the artist in the lineup (no parameters). Party ids are stored as text
-- on both sides; a value that is not a canonical number is not evidence.
rsvpSignalsSql :: Text
rsvpSignalsSql =
  "SELECT DISTINCT rsvp.party_id::bigint, artist.party_id::bigint \
  \FROM event_rsvp rsvp \
  \JOIN directory_public_rsvp_event event ON event.id = rsvp.event_id \
  \JOIN event_artist lineup ON lineup.event_id = rsvp.event_id \
  \JOIN social_artist_profile artist ON artist.id = lineup.artist_id \
  \WHERE rsvp.status = 'accepted' \
  \AND rsvp.party_id ~ '^[1-9][0-9]{0,17}$' \
  \AND artist.party_id ~ '^[1-9][0-9]{0,17}$'"

-- | Eligible plays by signed-in fans per @(fan, artist)@ after an instant
-- (one parameter: the start of the window).
playSignalsSql :: Text
playSignalsSql =
  "SELECT play.party_id, release.artist_party_id, count(*)::bigint \
  \FROM music_playback_event play \
  \JOIN music_release_version version ON version.id = play.release_version_id \
  \JOIN music_release release ON release.id = version.release_id \
  \WHERE play.eligible_play AND play.party_id IS NOT NULL AND play.occurred_at > ? \
  \GROUP BY play.party_id, release.artist_party_id"

-- | 'favouriteSignalsSql' for one fan (one parameter: the fan).
favouriteArtistsForFanSql :: Text
favouriteArtistsForFanSql =
  "SELECT DISTINCT release.artist_party_id \
  \FROM music_favorite favourite \
  \JOIN music_release_track track ON track.recording_id = favourite.recording_id \
  \JOIN music_release_version version ON version.id = track.release_version_id \
  \JOIN music_release release ON release.id = version.release_id \
  \WHERE favourite.party_id = ?"

-- | 'rsvpSignalsSql' for one fan (one parameter: the fan's id as text).
rsvpArtistsForFanSql :: Text
rsvpArtistsForFanSql =
  "SELECT DISTINCT artist.party_id::bigint \
  \FROM event_rsvp rsvp \
  \JOIN directory_public_rsvp_event event ON event.id = rsvp.event_id \
  \JOIN event_artist lineup ON lineup.event_id = rsvp.event_id \
  \JOIN social_artist_profile artist ON artist.id = lineup.artist_id \
  \WHERE rsvp.status = 'accepted' \
  \AND rsvp.party_id = ? \
  \AND artist.party_id ~ '^[1-9][0-9]{0,17}$'"

-- | 'playSignalsSql' for one fan (two parameters: the fan, the start of the
-- window).
playCountsForFanSql :: Text
playCountsForFanSql =
  "SELECT release.artist_party_id, count(*)::bigint \
  \FROM music_playback_event play \
  \JOIN music_release_version version ON version.id = play.release_version_id \
  \JOIN music_release release ON release.id = version.release_id \
  \WHERE play.eligible_play AND play.party_id = ? AND play.occurred_at > ? \
  \GROUP BY release.artist_party_id"

-- | Fans who switched personalisation off (no parameters).
optedOutFansSql :: Text
optedOutFansSql =
  "SELECT party_id FROM social_v2_preference WHERE NOT personalized"

-- | The fan's personalisation switch; no row means the default, on (one
-- parameter: the fan).
fanPersonalisedSql :: Text
fanPersonalisedSql =
  "SELECT personalized FROM social_v2_preference WHERE party_id = ?"

-- | The transaction's clock (no parameters).
snapshotClockSql :: Text
snapshotClockSql =
  "SELECT now()"

-- | Open a generation (three parameters: snapshot instant, artist count, pair
-- count). Returns its id.
insertGenerationSql :: Text
insertGenerationSql =
  "INSERT INTO artist_similarity_generation (snapshot_at, artist_count, pair_count) \
  \VALUES (?, ?, ?) RETURNING id"

-- | Store the norms of a generation (three parameters: generation, artists
-- and norms as array literals of equal length).
insertNormsSql :: Text
insertNormsSql =
  "INSERT INTO artist_similarity_norm (generation, artist_party_id, norm2) \
  \SELECT ?, item.artist, item.norm2 \
  \FROM unnest(?::bigint[], ?::bigint[]) AS item(artist, norm2)"

-- | Store the pairs of a generation (five parameters: generation, then
-- smaller ids, larger ids, dot products and supports as array literals of
-- equal length).
insertPairsSql :: Text
insertPairsSql =
  "INSERT INTO artist_similarity_pair (generation, artist_a, artist_b, dot, support) \
  \SELECT ?, item.artist_a, item.artist_b, item.dot, item.support \
  \FROM unnest(?::bigint[], ?::bigint[], ?::bigint[], ?::integer[]) \
  \AS item(artist_a, artist_b, dot, support)"

-- | Keep the two newest generations (no parameters). Their rows go with them.
pruneGenerationsSql :: Text
pruneGenerationsSql =
  "DELETE FROM artist_similarity_generation WHERE id NOT IN \
  \(SELECT id FROM artist_similarity_generation ORDER BY id DESC LIMIT 2)"

-- | Everything a request reads from the table, in one statement and so from
-- one snapshot: the pairs of the newest generation that touch any of the given
-- artists, each with the norms of its two artists. The newest generation is
-- used only while its snapshot is recent enough; otherwise, as when no pair
-- touches the artists, there are no rows (three parameters: the maximum age in
-- hours, and the artists twice as an array literal).
generationSimilaritySql :: Text
generationSimilaritySql =
  "SELECT pair.artist_a, pair.artist_b, pair.dot, pair.support, norm_a.norm2, norm_b.norm2 \
  \FROM (SELECT id, snapshot_at FROM artist_similarity_generation ORDER BY id DESC LIMIT 1) newest \
  \JOIN artist_similarity_pair pair ON pair.generation = newest.id \
  \JOIN artist_similarity_norm norm_a \
  \ON norm_a.generation = newest.id AND norm_a.artist_party_id = pair.artist_a \
  \JOIN artist_similarity_norm norm_b \
  \ON norm_b.generation = newest.id AND norm_b.artist_party_id = pair.artist_b \
  \WHERE newest.snapshot_at >= now() - make_interval(hours => ?::integer) \
  \AND (pair.artist_a = ANY(?::bigint[]) OR pair.artist_b = ANY(?::bigint[]))"

-- | Where a request takes the similarity from.
data SimilaritySource
  = StoredGeneration
    -- ^ the rows of the newest generation; none or too old means no similarity,
    -- which ranks as the popularity fallback
  | LiveSignals
    -- ^ the reference: recompute from the current signal rows
  deriving (Eq, Show)

-- | What one run of the job stored.
data GenerationSummary = GenerationSummary
  { gsGeneration :: !Int64
  , gsSnapshotAt :: !UTCTime
  , gsArtists    :: !Int
  , gsPairs      :: !Int
  } deriving (Eq, Show)

-- | The similarity of the signal rows the transaction sees, with the play
-- window ending at @snapshotAt@. Listing and opt-out are read in the same
-- transaction.
loadSignalSimilarity :: MonadIO m => UTCTime -> SqlPersistT m Similarity
loadSignalSimilarity snapshotAt = do
  listed <- Set.map fromSqlKey <$> loadPubliclyListedArtistParties
  optedOutRows <- rawSql optedOutFansSql []
  followRows <- rawSql followSignalsSql []
  favouriteRows <- rawSql favouriteSignalsSql []
  rsvpRows <- rawSql rsvpSignalsSql []
  playRows <- rawSql playSignalsSql [toPersistValue (playWindowStart snapshotAt)]
  let inputs =
        SignalInputs
          { siFollows = [(fan, artist) | (Single fan, Single artist) <- followRows]
          , siFavourites = [(fan, artist) | (Single fan, Single artist) <- favouriteRows]
          , siRsvps = [(fan, artist) | (Single fan, Single artist) <- rsvpRows]
          , siPlays =
              [ (fan, artist, countToInt plays)
              | (Single fan, Single artist, Single plays) <- playRows
              ]
          }
      optedOut = Set.fromList [fan | Single fan <- optedOutRows]
  pure (similarityFromWeights (signalWeights listed optedOut inputs))

-- | Compute the similarity of the current signals and store it as a new
-- generation. Run it in one REPEATABLE READ transaction: every read then sees
-- one snapshot, and the generation appears to requests complete or not at all.
runSimilarityGeneration :: MonadIO m => SqlPersistT m GenerationSummary
runSimilarityGeneration = do
  snapshotAt <- loadSnapshotClock
  similarity <- loadSignalSimilarity snapshotAt
  let norms = Map.toList (simNorms similarity)
      pairs = Map.toList (simPairs similarity)
  generationRows <-
    rawSql insertGenerationSql
      [ toPersistValue snapshotAt
      , toPersistValue (length norms)
      , toPersistValue (length pairs)
      ]
  generation <- case generationRows of
    [Single generation] -> pure (generation :: Int64)
    _ -> liftIO (throwIO (userError "artist similarity: the generation insert returned no id"))
  rawExecute insertNormsSql
    [ toPersistValue generation
    , arrayParameter (map fst norms)
    , arrayParameter (map snd norms)
    ]
  rawExecute insertPairsSql
    [ toPersistValue generation
    , arrayParameter [a | ((a, _), _) <- pairs]
    , arrayParameter [b | ((_, b), _) <- pairs]
    , arrayParameter [psDot stat | (_, stat) <- pairs]
    , arrayParameter [fromIntegral (psSupport stat) | (_, stat) <- pairs]
    ]
  rawExecute pruneGenerationsSql []
  pure
    GenerationSummary
      { gsGeneration = generation
      , gsSnapshotAt = snapshotAt
      , gsArtists = length norms
      , gsPairs = length pairs
      }

-- | Recommendations for @fan@ from the multi-signal similarity.
--
-- Listing, follows, the follow policy, the fan's own signals and the fan's
-- personalisation switch are always read live. A fan who switched
-- personalisation off, or a request without a usable stored generation, gets
-- exactly the popularity fallback.
loadSimilarityRecommendations
  :: MonadIO m
  => SimilaritySource
  -> PartyId
  -> Int
  -> SqlPersistT m [ArtistRecommendation]
loadSimilarityRecommendations source fan limit = do
  frame <- loadRecommendationFrame fan
  personalisedRows <- rawSql fanPersonalisedSql [fanValue]
  let listed = rfListed frame
      rank = rankBySimilarity limit listed (rfExcluded frame) (rfFollowers frame)
      fallback = rank Map.empty (Similarity Map.empty Map.empty)
  if personalisedRows == [Single False]
    then pure fallback
    else do
      now <- loadSnapshotClock
      favouriteRows <- rawSql favouriteArtistsForFanSql [fanValue]
      rsvpRows <- rawSql rsvpArtistsForFanSql [toPersistValue (T.pack (show fanId))]
      playRows <-
        rawSql playCountsForFanSql [fanValue, toPersistValue (playWindowStart now)]
      let inputs =
            SignalInputs
              { siFollows = [(fanId, artist) | artist <- Set.toList (rfFollowed frame)]
              , siFavourites = [(fanId, artist) | Single artist <- favouriteRows]
              , siRsvps = [(fanId, artist) | Single artist <- rsvpRows]
              , siPlays =
                  [ (fanId, artist, countToInt plays)
                  | (Single artist, Single plays) <- playRows
                  ]
              }
          seedWeights =
            Map.fromList
              [ (artist, weight)
              | ((_, artist), weight) <- Map.toList (signalWeights listed Set.empty inputs)
              ]
      similarity <- case source of
        LiveSignals -> loadSignalSimilarity now
        StoredGeneration -> loadStoredSimilarity (Map.keysSet seedWeights)
      pure (rank seedWeights similarity)
  where
    fanId = fromSqlKey fan
    fanValue = toPersistValue fan

-- | The stored rows a request with these seeds needs. Without a usable
-- generation this is the empty similarity, which ranks as the fallback.
loadStoredSimilarity :: MonadIO m => Set.Set Int64 -> SqlPersistT m Similarity
loadStoredSimilarity seeds = do
  rows <-
    rawSql generationSimilaritySql
      [toPersistValue maxGenerationAgeHours, seedArray, seedArray]
  pure
    Similarity
      { simNorms =
          Map.fromList
            (concat
              [ [(a, normA), (b, normB)]
              | (Single a, Single b, _, _, Single normA, Single normB) <- rows
              ])
      , simPairs =
          Map.fromList
            [ ((a, b), PairStat dot (countToInt support))
            | (Single a, Single b, Single dot, Single support, _, _) <- rows
            ]
      }
  where
    seedArray = arrayParameter (Set.toList seeds)

loadSnapshotClock :: MonadIO m => SqlPersistT m UTCTime
loadSnapshotClock = do
  rows <- rawSql snapshotClockSql []
  case rows of
    [Single now] -> pure now
    _ -> liftIO (throwIO (userError "artist similarity: the database clock returned no row"))

playWindowStart :: UTCTime -> UTCTime
playWindowStart = addUTCTime (negate (fromIntegral playWindowDays * nominalDay))

countToInt :: Int64 -> Int
countToInt = fromIntegral . max 0

-- | A PostgreSQL array literal, cast to its element type by the statement.
arrayParameter :: [Int64] -> PersistValue
arrayParameter ids =
  toPersistValue ("{" <> T.intercalate "," (map (T.pack . show) ids) <> "}" :: Text)
