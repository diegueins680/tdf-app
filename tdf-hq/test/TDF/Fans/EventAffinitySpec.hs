{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.Fans.EventAffinitySpec (spec) where

import           Data.Int                  (Int64)
import           Data.List                 (sort)
import qualified Data.Map.Strict           as Map
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Fans.ArtistSimilarity
import           TDF.Fans.EventAffinity

genArtist :: Gen Int64
genArtist = choose (1, 6)

-- | A fan's request: usable artists, own weights and a similarity built from
-- random weighted signals of fans 101..106.
data Case = Case
  { cUsable     :: Set.Set Int64
  , cSeeds      :: Map.Map Int64 Int
  , cSimilarity :: Similarity
  } deriving (Show)

instance Arbitrary Case where
  arbitrary = do
    usable <- Set.fromList <$> listOf genArtist
    seeds <- Map.fromList <$> listOf ((,) <$> genArtist <*> choose (-1, 23))
    weights <-
      Map.fromList
        <$> listOf ((,) <$> ((,) <$> choose (101, 106) <*> genArtist) <*> choose (1, 23))
    pure (Case usable seeds (similarityFromWeights weights))

affinitiesOf :: Case -> Map.Map Int64 Double
affinitiesOf c = artistAffinities (cUsable c) (cSeeds c) (cSimilarity c)

-- | A page: events are numbered by position; each has a lineup.
newtype Page = Page [(Int, [Int64])] deriving (Show)

instance Arbitrary Page where
  arbitrary = Page . zip [0 ..] <$> listOf (listOf genArtist)

spec :: Spec
spec = describe "event order by artist affinity (EVT-AFFINITY-ORDER-001)" $ do
  describe "artist affinities" $ do
    it "only usable artists have an affinity, and it lies in (0, 2]" $
      property $ \c ->
        let affinities = affinitiesOf c
        in Map.keysSet affinities `Set.isSubsetOf` cUsable c
             && all (\value -> value > 0 && value <= 2 && not (isNaN value)) (Map.elems affinities)

    it "an artist the fan has a signal for is above 1; any other is at most 1" $
      property $ \c ->
        let own = Map.keysSet (Map.filter (> 0) (cSeeds c)) `Set.intersection` cUsable c
        in and
             [ if Set.member artist own then value > 1 else value <= 1
             | (artist, value) <- Map.toList (affinitiesOf c)
             ]
             && own `Set.isSubsetOf` Map.keysSet (affinitiesOf c)

    it "an artist that is not usable is never evidence" $
      property $ \c ->
        let usable = cUsable c
            visibleSeeds = Map.filterWithKey (\artist _ -> Set.member artist usable) (cSeeds c)
            visiblePairs =
              (cSimilarity c)
                { simPairs =
                    Map.filterWithKey
                      (\(a, b) _ -> Set.member a usable && Set.member b usable)
                      (simPairs (cSimilarity c))
                }
        in affinitiesOf c === artistAffinities usable visibleSeeds visiblePairs

    it "without own signals, or without usable artists, there is no affinity" $
      property $ \c ->
        artistAffinities (cUsable c) Map.empty (cSimilarity c) === Map.empty
          .&&. artistAffinities Set.empty (cSeeds c) (cSimilarity c) === Map.empty

    it "is the documented value on a fixed case" $ do
      -- Fans 101 and 102 weigh artists 1 and 2 with 8: similarity 1. The caller
      -- follows 1 (weight 8) and played 3 once (weight 1); 4 is hidden.
      let similarity =
            similarityFromWeights
              (Map.fromList [((fan, artist), 8) | fan <- [101, 102], artist <- [1, 2, 4]])
          affinities =
            artistAffinities (Set.fromList [1, 2, 3]) (Map.fromList [(1, 8), (3, 1), (4, 8)]) similarity
      Map.keys affinities `shouldBe` [1, 2, 3]
      Map.lookup 1 affinities `shouldBe` Just 2
      Map.lookup 3 affinities `shouldBe` Just 1.125
      -- Two usable seeds; only seed 1 is similar to 2: (1 * 1 + 0) / 2.
      Map.lookup 2 affinities `shouldBe` Just 0.5

  describe "page order" $ do
    it "returns a permutation of the page" $
      property $ \c (Page page) ->
        sort (map fst (orderByAffinity (affinitiesOf c) snd page)) === map fst page

    it "returns the page unchanged when no lineup artist has an affinity" $
      property $ \(Page page) ->
        orderByAffinity Map.empty snd page === page
          .&&. orderByAffinity (Map.fromList [(99, 2)]) snd page === page

    it "orders by affinity, largest first, and keeps the given order among equals" $
      property $ \c (Page page) ->
        let affinities = affinitiesOf c
            ordered = orderByAffinity affinities snd page
            keys = [(eventAffinity affinities lineup, position) | (position, lineup) <- ordered]
        in and
             [ a > b || (a == b && p < q)
             | ((a, p), (b, q)) <- zip keys (drop 1 keys)
             ]

    it "is idempotent and does not depend on the order of a lineup" $
      property $ \c (Page page) ->
        let affinities = affinitiesOf c
            ordered = orderByAffinity affinities snd page
        in orderByAffinity affinities snd ordered === ordered
             .&&. map fst (orderByAffinity affinities (reverse . snd) page) === map fst ordered

    it "the affinity of an event is the largest of its lineup, and 0 for none" $
      property $ \c (lineup :: [Int64]) ->
        let affinities = affinitiesOf c
            value = eventAffinity affinities lineup
        in value >= 0
             && value <= 2
             && all (\artist -> Map.findWithDefault 0 artist affinities <= value) lineup
             && (value == 0 || any (\artist -> Map.lookup artist affinities == Just value) lineup)

  describe "statement" $
    it "reads lineups through the profile's party and ignores non-canonical party ids" $ do
      eventLineupPartiesSql `shouldSatisfy` T.isInfixOf "JOIN social_artist_profile artist ON artist.id = lineup.artist_id"
      eventLineupPartiesSql `shouldSatisfy` T.isInfixOf "lineup.event_id = ANY(?::bigint[])"
      eventLineupPartiesSql `shouldSatisfy` T.isInfixOf "artist.party_id ~ '^[1-9][0-9]{0,17}$'"
