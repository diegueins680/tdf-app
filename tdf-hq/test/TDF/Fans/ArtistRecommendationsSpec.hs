{-# LANGUAGE OverloadedStrings #-}

module TDF.Fans.ArtistRecommendationsSpec (spec) where

import           Data.Int                       (Int64)
import           Data.List                      (nub, sortOn)
import qualified Data.Map.Strict                as Map
import           Data.Ord                       (Down (..))
import qualified Data.Set                       as Set
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Fans.ArtistRecommendations

-- | A small catalog: artists 1..8, any subset eligible or excluded.
data Scenario = Scenario
  { scLimit     :: Int
  , scEligible  :: Set.Set Int64
  , scExcluded  :: Set.Set Int64
  , scFollowers :: Map.Map Int64 Int
  , scRows      :: [CoFollow]
  } deriving (Show)

instance Arbitrary Scenario where
  arbitrary = do
    let artist = choose (1, 8)
    limit <- choose (-1, 10)
    eligible <- Set.fromList <$> listOf artist
    excluded <- Set.fromList <$> listOf artist
    followers <- Map.fromList <$> listOf ((,) <$> artist <*> choose (-1, 9))
    rows <- listOf (CoFollow <$> artist <*> artist <*> choose (-1, 12))
    pure (Scenario limit eligible excluded followers rows)

rank :: Scenario -> [ArtistRecommendation]
rank s =
  rankArtistRecommendations (scLimit s) (scEligible s) (scExcluded s) (scFollowers s) (scRows s)

spec :: Spec
spec = describe "fan artist recommendations (COMMUNITY-FAN-RECOMMENDATION-001)" $ do
  let everyone = Set.fromList [1 .. 5]
      -- The fan follows artist 1. Its 4 followers split: 2 also follow artist 2
      -- (4 followers) and 2 also follow artist 3 (100 followers).
      followers = Map.fromList [(1, 4), (2, 4), (3, 100), (4, 7), (5, 0)]
      rows = [CoFollow 1 2 2, CoFollow 1 3 2]
      ranked = rankArtistRecommendations 10 everyone (Set.fromList [1]) followers rows

  it "prefers the artist whose audience overlaps most, not the most followed one" $
    map recArtistId ranked `shouldBe` [2, 3, 4]

  it "scores by cosine similarity of the follower sets" $ do
    map recScore ranked `shouldSatisfy` \scores -> case scores of
      [a, b, c] -> abs (a - 0.5) < 1e-9 && abs (b - 0.1) < 1e-9 && c == 0
      _ -> False

  it "names the followed artists behind a recommendation and labels the fallback" $ do
    map recReason ranked `shouldBe` [SimilarFans, SimilarFans, Popular]
    map recSeedArtistIds ranked `shouldBe` [[1], [1], []]
    map (recommendationReasonCode . recReason) ranked
      `shouldBe` ["similar_fans", "similar_fans", "popular"]

  it "adds the similarities to every followed artist and lists the strongest first" $ do
    let counts = Map.fromList [(1, 4), (2, 4), (3, 4), (6, 4)]
        out =
          rankArtistRecommendations 5 (Set.fromList [1, 2, 3, 6]) (Set.fromList [1, 2, 6]) counts
            [CoFollow 1 3 1, CoFollow 2 3 3, CoFollow 6 3 2]
    map recArtistId out `shouldBe` [3]
    map recSeedArtistIds out `shouldBe` [[2, 6, 1]]
    map recScore out `shouldSatisfy` all (\score -> abs (score - 1.5) < 1e-9)

  it "names at most maxRecommendationSeeds followed artists" $ do
    let seeds = [1 .. 5]
        counts = Map.fromList [(artist, 2) | artist <- 9 : seeds]
        out =
          rankArtistRecommendations 1 (Set.fromList (9 : seeds)) (Set.fromList seeds) counts
            [CoFollow seed 9 1 | seed <- seeds]
    map (length . recSeedArtistIds) out `shouldBe` [maxRecommendationSeeds]

  it "never uses a hidden artist as a recommendation or as evidence" $ do
    let hiddenCandidate =
          rankArtistRecommendations 10 (Set.fromList [1, 3, 4]) (Set.fromList [1]) followers rows
        hiddenSeed =
          rankArtistRecommendations 10 (Set.fromList [2, 3, 4]) (Set.fromList [1]) followers rows
    map recArtistId hiddenCandidate `shouldBe` [3, 4]
    map recReason hiddenSeed `shouldBe` [Popular, Popular, Popular]
    map recArtistId hiddenSeed `shouldBe` [3, 4, 2]

  it "clamps a shared count that exceeds a follower count read at another instant" $ do
    let out =
          rankArtistRecommendations 1 (Set.fromList [1, 2]) (Set.fromList [1])
            (Map.fromList [(1, 2), (2, 3)]) [CoFollow 1 2 50]
    map recScore out `shouldSatisfy` all (\score -> score > 0 && score <= 1)

  it "falls back to the most followed artists for a fan without follows" $
    map recArtistId (rankArtistRecommendations 2 everyone Set.empty followers [])
      `shouldBe` [3, 4]

  it "bounds the requested size" $ do
    clampRecommendationLimit Nothing `shouldBe` defaultRecommendationLimit
    clampRecommendationLimit (Just 0) `shouldBe` 1
    clampRecommendationLimit (Just (-5)) `shouldBe` 1
    clampRecommendationLimit (Just 1000) `shouldBe` maxRecommendationLimit
    clampRecommendationLimit (Just 9) `shouldBe` 9

  it "returns at most the limit, each artist once" $ property $ \s ->
    let ids = map recArtistId (rank s)
    in length ids <= max 0 (scLimit s) && nub ids == ids

  it "recommends only eligible artists that are not excluded" $ property $ \s ->
    all
      (\item ->
        Set.member (recArtistId item) (scEligible s)
          && not (Set.member (recArtistId item) (scExcluded s)))
      (rank s)

  it "names only eligible followed artists as evidence" $ property $ \s ->
    all
      (\item ->
        all (`Set.member` scEligible s) (recSeedArtistIds item)
          && notElem (recArtistId item) (recSeedArtistIds item)
          && length (recSeedArtistIds item) <= maxRecommendationSeeds)
      (rank s)

  it "puts shared-fan recommendations before the fallback, best score first" $ property $ \s ->
    let out = rank s
        (similar, rest) = span ((== SimilarFans) . recReason) out
    in all ((== Popular) . recReason) rest
         && all (\item -> recScore item > 0 && not (null (recSeedArtistIds item))) similar
         && all (\item -> recScore item == 0 && null (recSeedArtistIds item)) rest
         && map recScore similar == sortOn Down (map recScore similar)

  it "bounds a score by the number of followed artists that contributed" $ property $ \s ->
    let seedCount = Set.size (Set.fromList (map cfSeedArtistId (scRows s)))
    in all (\item -> recScore item <= fromIntegral seedCount + 1e-9) (rank s)

  it "does not depend on the order of the evidence rows" $ property $ \s ->
    forAll (shuffle (scRows s)) $ \shuffled ->
      rank s { scRows = shuffled } === rank s
