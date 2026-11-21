{-# LANGUAGE OverloadedStrings #-}

-- | Artist recommendations for a fan (COMMUNITY-FAN-RECOMMENDATION-001).
--
-- Item-to-item collaborative filtering over 'TDF.Models.FanFollow': two
-- artists are similar when the same fans follow both. The similarity of a
-- followed artist @s@ and a candidate @c@ is the cosine of their follower
-- sets,
--
-- > shared(s, c) / sqrt (followers(s) * followers(c))
--
-- and a candidate's score is the sum of its similarities to every artist the
-- fan already follows. Dividing by the follower counts keeps an artist that
-- everybody follows from being recommended to everyone.
--
-- When the fan's follows give fewer candidates than requested, the list is
-- completed with the most followed artists and those entries say so.
--
-- Only publicly listed artists (ART-PUBLIC-VISIBILITY-001) are used, either
-- as evidence or as a recommendation. The ranking is a pure function; the
-- loader reads aggregate counts only and writes nothing.
module TDF.Fans.ArtistRecommendations
  ( CoFollow (..)
  , RecommendationReason (..)
  , ArtistRecommendation (..)
  , recommendationReasonCode
  , defaultRecommendationLimit
  , maxRecommendationLimit
  , maxRecommendationSeeds
  , clampRecommendationLimit
  , rankArtistRecommendations
  , followedArtistsSql
  , coFollowSql
  , followerCountsSql
  , loadArtistRecommendations
  ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Data.Int                      (Int64)
import           Data.List                     (sortOn)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Ord                      (Down (..))
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import           Database.Persist              (toPersistValue)
import           Database.Persist.Sql          (Single (..), SqlPersistT, fromSqlKey, rawSql)

import           TDF.Models                    (PartyId)
import           TDF.Profiles.ArtistVisibility (loadPubliclyListedArtistParties)

-- | @cfShared@ fans follow both @cfSeedArtistId@ and @cfCandidateArtistId@.
data CoFollow = CoFollow
  { cfSeedArtistId      :: !Int64
  , cfCandidateArtistId :: !Int64
  , cfShared            :: !Int
  } deriving (Eq, Show)

data RecommendationReason
  = SimilarFans
    -- ^ fans of artists the fan follows also follow this artist
  | Popular
    -- ^ no shared-fan evidence; one of the most followed artists
  deriving (Eq, Ord, Show)

-- | Stable wire value of a reason.
recommendationReasonCode :: RecommendationReason -> Text
recommendationReasonCode SimilarFans = "similar_fans"
recommendationReasonCode Popular     = "popular"

data ArtistRecommendation = ArtistRecommendation
  { recArtistId      :: !Int64
  , recReason        :: !RecommendationReason
  , recScore         :: !Double
    -- ^ sum of cosine similarities; 0 for 'Popular'
  , recSeedArtistIds :: ![Int64]
    -- ^ followed artists that contributed most, strongest first
  } deriving (Eq, Show)

defaultRecommendationLimit :: Int
defaultRecommendationLimit = 6

maxRecommendationLimit :: Int
maxRecommendationLimit = 24

-- | How many followed artists are named as the reason for one recommendation.
maxRecommendationSeeds :: Int
maxRecommendationSeeds = 3

clampRecommendationLimit :: Maybe Int -> Int
clampRecommendationLimit =
  min maxRecommendationLimit . max 1 . fromMaybe defaultRecommendationLimit

-- | Rank at most @limit@ artists for one fan.
--
-- @eligible@ are the artists that may be shown or used as evidence, @excluded@
-- the ones never recommended (already followed, and the fan's own profile) and
-- @followers@ the follower count of each artist. Rows that mention an
-- ineligible artist, an excluded candidate or an artist without a positive
-- follower count are ignored. A shared count larger than either follower
-- count (counts read at different instants) is clamped, so one followed artist
-- never contributes more than 1.
--
-- The order is total: score, then follower count, then artist id. Shared-fan
-- recommendations always precede the popularity fallback.
rankArtistRecommendations
  :: Int
  -> Set.Set Int64
  -> Set.Set Int64
  -> Map.Map Int64 Int
  -> [CoFollow]
  -> [ArtistRecommendation]
rankArtistRecommendations limit eligible excluded followers rows
  | limit <= 0 = []
  | otherwise = take limit (similar ++ popular)
  where
    followerCount artistId = Map.findWithDefault 0 artistId followers
    recommendable artistId =
      Set.member artistId eligible && not (Set.member artistId excluded)

    sharedByPair :: Map.Map (Int64, Int64) Int
    sharedByPair =
      Map.fromListWith (+)
        [ ((cfCandidateArtistId row, cfSeedArtistId row), cfShared row)
        | row <- rows
        , cfShared row > 0
        , cfSeedArtistId row /= cfCandidateArtistId row
        , Set.member (cfSeedArtistId row) eligible
        , recommendable (cfCandidateArtistId row)
        , followerCount (cfSeedArtistId row) > 0
        , followerCount (cfCandidateArtistId row) > 0
        ]

    contributions :: Map.Map Int64 [(Int64, Double)]
    contributions =
      Map.fromListWith (++)
        [ (candidate, [(seed, similarity seed candidate shared)])
        | ((candidate, seed), shared) <- Map.toList sharedByPair
        ]

    similarity seed candidate shared =
      let seedFollowers = followerCount seed
          candidateFollowers = followerCount candidate
          overlap = min shared (min seedFollowers candidateFollowers)
      in fromIntegral overlap
           / sqrt (fromIntegral seedFollowers * fromIntegral candidateFollowers)

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

    alreadyRanked = Map.keysSet contributions

    popular =
      [ ArtistRecommendation
          { recArtistId = artistId
          , recReason = Popular
          , recScore = 0
          , recSeedArtistIds = []
          }
      | (artistId, _) <-
          sortOn (\(artistId, count) -> (Down count, artistId))
            [ (artistId, count)
            | (artistId, count) <- Map.toList followers
            , count > 0
            , recommendable artistId
            , not (Set.member artistId alreadyRanked)
            ]
      ]

-- | Artists followed by the fan (one parameter: the fan).
followedArtistsSql :: Text
followedArtistsSql =
  "SELECT artist_party_id FROM fan_follow WHERE fan_party_id = ?"

-- | For each artist the fan follows (seed) and each artist the fan does not
-- follow (candidate): how many other fans follow both. Three parameters, all
-- the fan. scripts/test-fan-recommendations-sql.py runs this text on PostgreSQL.
coFollowSql :: Text
coFollowSql =
  "SELECT seed.artist_party_id, other.artist_party_id, count(*)::bigint \
  \FROM fan_follow seed \
  \JOIN fan_follow other ON other.fan_party_id = seed.fan_party_id \
  \AND other.artist_party_id <> seed.artist_party_id \
  \WHERE seed.fan_party_id <> ? \
  \AND seed.artist_party_id IN \
  \(SELECT mine.artist_party_id FROM fan_follow mine WHERE mine.fan_party_id = ?) \
  \AND other.artist_party_id NOT IN \
  \(SELECT mine.artist_party_id FROM fan_follow mine WHERE mine.fan_party_id = ?) \
  \GROUP BY seed.artist_party_id, other.artist_party_id"

-- | Follower count of every followed artist (no parameters).
followerCountsSql :: Text
followerCountsSql =
  "SELECT artist_party_id, count(*)::bigint FROM fan_follow GROUP BY artist_party_id"

-- | Recommendations for @fan@ from the current follow rows.
--
-- Three aggregate reads in the caller's transaction: the fan's own follows,
-- the shared-fan counts between those and every other artist, and the follower
-- count of each artist. No individual follower other than the fan is read.
loadArtistRecommendations
  :: MonadIO m
  => PartyId
  -> Int
  -> SqlPersistT m [ArtistRecommendation]
loadArtistRecommendations fan limit = do
  listed <- Set.map fromSqlKey <$> loadPubliclyListedArtistParties
  followedRows <- rawSql followedArtistsSql [fanValue]
  coFollowRows <- rawSql coFollowSql [fanValue, fanValue, fanValue]
  followerRows <- rawSql followerCountsSql []
  let followed = [artistId | Single artistId <- followedRows]
      excluded = Set.fromList (fromSqlKey fan : followed)
      followers =
        Map.fromList
          [ (artistId, countToInt total)
          | (Single artistId, Single total) <- followerRows
          ]
      coFollows =
        [ CoFollow seed candidate (countToInt shared)
        | (Single seed, Single candidate, Single shared) <- coFollowRows
        ]
  pure (rankArtistRecommendations limit listed excluded followers coFollows)
  where
    fanValue = toPersistValue fan

    countToInt :: Int64 -> Int
    countToInt = fromIntegral . max 0
