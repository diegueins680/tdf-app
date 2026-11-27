{-# LANGUAGE OverloadedStrings #-}

-- | Ordering a page of events by the caller's affinity to the artists in
-- their lineups (EVT-AFFINITY-ORDER-001, formal/system/event-affinity.md).
--
-- The page itself (which events, after all filters, visibility rules, limit
-- and offset) is chosen by the existing date-ordered query. This module only
-- permutes it. The similarity is the stored relation of
-- COMMUNITY-ARTIST-SIMILARITY-001.
module TDF.Fans.EventAffinity
  ( -- * Pure core
    ownSignalBase
  , artistAffinities
  , eventAffinity
  , orderByAffinity
    -- * SQL
  , eventLineupPartiesSql
    -- * Loaders
  , loadFanArtistAffinities
  , loadEventLineupParties
  , orderEventPageForFan
  ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Data.Int                      (Int64)
import           Data.List                     (sortOn)
import qualified Data.Map.Strict               as Map
import           Data.Ord                      (Down (..))
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import           Database.Persist.Sql          (Single (..), SqlPersistT, fromSqlKey, rawSql)

import           TDF.Fans.ArtistRecommendations (RecommendationFrame (..), loadRecommendationFrame)
import           TDF.Fans.ArtistSimilarity
import           TDF.Models                    (PartyId)

-- | Added to the strength of an artist the fan has a signal for, so that such
-- an artist always ranks above one that is only similar.
ownSignalBase :: Double
ownSignalBase = 1

-- | The fan's affinity to each artist, for artists with a positive value.
--
-- @usable@ are the artists that may be evidence: an artist outside it has no
-- affinity and contributes to none. For a usable artist the fan has a signal
-- for, the affinity is @'ownSignalBase' + min weight 'maxSeedWeight' /
-- 'maxSeedWeight'@, in @(1, 2]@. For any other usable artist it is the mean,
-- over the fan's usable seeds, of strength times similarity, in @[0, 1]@.
artistAffinities
  :: Set.Set Int64
  -> Map.Map Int64 Int
  -> Similarity
  -> Map.Map Int64 Double
artistAffinities usable seedWeights similarity =
  Map.union own inferred
  where
    strengths :: Map.Map Int64 Double
    strengths =
      Map.fromList
        [ (seed, fromIntegral (min weight maxSeedWeight) / fromIntegral maxSeedWeight)
        | (seed, weight) <- Map.toList seedWeights
        , weight > 0
        , Set.member seed usable
        ]
    own = Map.map (ownSignalBase +) strengths
    seedCount = fromIntegral (Map.size strengths) :: Double
    -- Ascending (candidate, seed): each sum always adds in the same order.
    contributions :: Map.Map Int64 (Map.Map Int64 Double)
    contributions =
      Map.fromListWith Map.union
        [ (candidate, Map.singleton seed value)
        | (a, b) <- Map.keys (simPairs similarity)
        , (seed, candidate) <- [(a, b), (b, a)]
        , Just strength <- [Map.lookup seed strengths]
        , Set.member candidate usable
        , not (Map.member candidate strengths)
        , let value = strength * similarityValue similarity seed candidate
        , value > 0
        ]
    inferred =
      Map.filter (> 0) (Map.map (\parts -> min 1 (sum (Map.elems parts) / seedCount)) contributions)

-- | The affinity of an event: the largest affinity among its lineup, 0 for an
-- empty or unknown lineup.
eventAffinity :: Map.Map Int64 Double -> [Int64] -> Double
eventAffinity affinities lineup =
  maximum (0 : [Map.findWithDefault 0 artist affinities | artist <- lineup])

-- | Permute a page: larger affinity first, and the given order among equal
-- affinities. Events without affinity therefore keep their date order at the
-- end, and without any affinity the page is returned unchanged.
orderByAffinity :: Map.Map Int64 Double -> (a -> [Int64]) -> [a] -> [a]
orderByAffinity affinities lineupOf page =
  map snd (sortOn fst [((Down (eventAffinity affinities (lineupOf item)), position), item) | (position, item) <- zip [0 :: Int ..] page])

-- | @(event, artist party)@ for the lineups of the given events (one
-- parameter: the event ids as an array literal). A lineup entry whose profile
-- has no canonical party id is not evidence.
eventLineupPartiesSql :: Text
eventLineupPartiesSql =
  "SELECT lineup.event_id, artist.party_id::bigint \
  \FROM event_artist lineup \
  \JOIN social_artist_profile artist ON artist.id = lineup.artist_id \
  \WHERE lineup.event_id = ANY(?::bigint[]) \
  \AND artist.party_id ~ '^[1-9][0-9]{0,17}$'"

-- | The lineups of the given events as artist party ids.
loadEventLineupParties :: MonadIO m => [Int64] -> SqlPersistT m (Map.Map Int64 [Int64])
loadEventLineupParties [] = pure Map.empty
loadEventLineupParties eventIds = do
  rows <- rawSql eventLineupPartiesSql [arrayParameter eventIds]
  pure (Map.fromListWith (flip (++)) [(event, [artist]) | (Single event, Single artist) <- rows])

-- | Order one page for @fan@: 'orderByAffinity' with the fan's affinities and
-- the lineups of the page. Without any affinity the page is returned as given
-- and the lineups are not read.
orderEventPageForFan
  :: MonadIO m
  => SimilaritySource
  -> PartyId
  -> (a -> Int64)
  -> [a]
  -> SqlPersistT m [a]
orderEventPageForFan source fan eventIdOf page = do
  affinities <- loadFanArtistAffinities source fan
  if Map.null affinities
    then pure page
    else do
      lineups <- loadEventLineupParties (map eventIdOf page)
      pure (orderByAffinity affinities (\item -> Map.findWithDefault [] (eventIdOf item) lineups) page)

-- | The caller's affinities, read live except for the stored similarity.
--
-- Usable artists are publicly listed, are not the caller, and are either
-- followed by the caller or followable under the installed policy. A fan who
-- switched personalisation off, or a request without a usable generation and
-- without own signals, has no affinities.
loadFanArtistAffinities
  :: MonadIO m
  => SimilaritySource
  -> PartyId
  -> SqlPersistT m (Map.Map Int64 Double)
loadFanArtistAffinities source fan = do
  frame <- loadRecommendationFrame fan
  personal <- loadFanSimilarity source fan frame
  let usable =
        Set.delete (fromSqlKey fan) $
          Set.filter
            (\artist -> Set.member artist (rfFollowed frame) || not (Set.member artist (rfExcluded frame)))
            (rfListed frame)
  pure $ case personal of
    Nothing -> Map.empty
    Just (seedWeights, similarity) -> artistAffinities usable seedWeights similarity
