module TDF.Social.SuggestionRankSpec (spec) where

import           Data.Int                  (Int64)
import           Data.List                 (sortOn)
import qualified Data.Map.Strict           as Map
import           Data.Ord                  (Down (..))
import qualified Data.Set                  as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck

import           TDF.Social.SuggestionRank

-- | A small world: accounts 1..8, any of them eligible, any follows between
-- accounts 1..10 (so some rows name accounts that are never eligible).
data World = World
  { wViewer   :: Int64
  , wEligible :: Set.Set Int64
  , wBlocked  :: Set.Set Int64
  , wFollows  :: [(Int64, Int64)]
  } deriving (Show)

instance Arbitrary World where
  arbitrary = do
    viewer <- chooseAccount
    others <- sublistOf [1 .. 8]
    blocked <- sublistOf (filter (/= viewer) [1 .. 8])
    follows <- listOf ((,) <$> anyAccount <*> anyAccount)
    keepBlocks <- arbitrary
    pure World
      { wViewer = viewer
      , wEligible = Set.fromList (viewer : others)
      , wBlocked = if keepBlocks then Set.fromList (take 2 blocked) else Set.empty
      , wFollows = follows
      }
    where
      chooseAccount = fromIntegral <$> chooseInt (1, 8)
      anyAccount = fromIntegral <$> chooseInt (1, 10)
  shrink world =
    [ world { wFollows = follows } | follows <- shrinkList (const []) (wFollows world) ]

graphOf :: World -> WalkGraph
graphOf world =
  case buildWalkGraph (wViewer world) (wEligible world) (wBlocked world) (wFollows world) of
    Just graph -> graph
    Nothing -> error "the generator keeps the viewer eligible and unblocked"

exactScores :: Int -> World -> Map.Map Int64 Rational
exactScores steps = walkScores walkDamping steps . graphOf

norm1 :: Map.Map Int64 Rational -> Map.Map Int64 Rational -> Rational
norm1 left right = sum (map abs (Map.elems (Map.unionWith (+) left (Map.map negate right))))

-- | An arbitrary distribution over the nodes of a graph.
distributionOn :: WalkGraph -> Gen (Map.Map Int64 Rational)
distributionOn graph = do
  let nodes = Set.toList (walkNodes graph)
  weights <- vectorOf (length nodes) (chooseInt (0, 5))
  let weightSum = sum weights
  pure $
    if weightSum == 0
      then initialScores graph
      else Map.fromList
             [ (node, fromIntegral weight / fromIntegral weightSum)
             | (node, weight) <- zip nodes weights
             ]

involves :: Set.Set Int64 -> (Int64, Int64) -> Bool
involves accounts (follower, followed) =
  Set.member follower accounts || Set.member followed accounts

spec :: Spec
spec = describe "TDF.Social.SuggestionRank (COMMUNITY-FRIEND-SUGGESTION-001)" $ do
  describe "probability distribution" $ do
    prop "exact scores are non-negative and sum to 1 after every step" $ \world ->
      forAll (chooseInt (0, 12)) $ \steps ->
        let scores = exactScores steps world
        in all (>= 0) (Map.elems scores) && sum (Map.elems scores) == 1

    prop "one step keeps any distribution on the graph a distribution" $ \world ->
      let graph = graphOf world
      in forAll (distributionOn graph) $ \scores ->
           let next = walkStep walkDamping graph scores
           in all (>= 0) (Map.elems next)
                && sum (Map.elems next) == 1
                && Map.keysSet next `Set.isSubsetOf` walkNodes graph

    prop "Double scores after the contract's iterations sum to 1 within 1e-9" $ \world ->
      let scores = walkScores walkDamping walkIterations (graphOf world) :: Map.Map Int64 Double
      in all (>= 0) (Map.elems scores) && abs (sum (Map.elems scores) - 1) <= 1.0e-9

    it "a viewer who follows nobody keeps all the mass" $
      case buildWalkGraph 1 (Set.fromList [1, 2, 3]) Set.empty [(2, 3), (3, 1)] of
        Nothing -> expectationFailure "viewer is eligible"
        Just graph ->
          (walkScores walkDamping 7 graph :: Map.Map Int64 Rational)
            `shouldBe` Map.singleton 1 1

    it "a dead end returns its mass to the viewer" $
      case buildWalkGraph 1 (Set.fromList [1, 2]) Set.empty [(1, 2)] of
        Nothing -> expectationFailure "viewer is eligible"
        Just graph ->
          (walkScores walkDamping 2 graph :: Map.Map Int64 Rational)
            `shouldBe` Map.fromList [(1, 1 - 0.85 * 0.15), (2, 0.85 * 0.15)]

  describe "convergence" $ do
    prop "one step contracts the L1 distance by the damping factor" $ \world ->
      let graph = graphOf world
      in forAll ((,) <$> distributionOn graph <*> distributionOn graph) $ \(p, q) ->
           norm1 (walkStep walkDamping graph p) (walkStep walkDamping graph q)
             <= walkDamping * norm1 p q

    it "2 * damping ^ iterations is below the stated bound" $
      (2 * (walkDamping :: Rational) ^ walkIterations) < walkErrorBound `shouldBe` True

    prop "the stored scores are within the bound of every later iterate" $ \world ->
      forAll (chooseInt (1, 5)) $ \extra ->
        let graph = graphOf world
            stored = walkScores walkDamping walkIterations graph :: Map.Map Int64 Rational
            later = iterate (walkStep walkDamping graph) stored !! extra
        in norm1 stored later <= walkErrorBound

  describe "exclusions" $ do
    prop "only nodes of the viewer's graph hold score" $ \world ->
      scoreSupport (exactScores 10 world)
        `Set.isSubsetOf` (wEligible world `Set.difference` wBlocked world)

    prop "follows of ineligible or blocked accounts do not change any score" $ \world ->
      let outside = Set.fromList [1 .. 10] `Set.difference`
            (wEligible world `Set.difference` wBlocked world)
          without = world { wFollows = filter (not . involves outside) (wFollows world) }
      in exactScores 10 world == exactScores 10 without

    prop "extra follows by accounts outside the graph change nothing" $ \world ->
      forAll (listOf ((,) <$> elements [9, 10] <*> (fromIntegral <$> chooseInt (1, 10)))) $
        \fake ->
          let reciprocal = [(followed, follower) | (follower, followed) <- fake]
              attacked = world { wFollows = wFollows world ++ fake ++ reciprocal }
          in exactScores 10 world == exactScores 10 attacked

    it "a blocked account passes no score on" $ do
      let eligible = Set.fromList [1, 2, 3]
          follows = [(1, 2), (2, 3)]
          open = buildWalkGraph 1 eligible Set.empty follows
          closed = buildWalkGraph 1 eligible (Set.singleton 2) follows
      fmap (Map.member 3 . exact 5) open `shouldBe` Just True
      fmap (exact 5) closed `shouldBe` Just (Map.singleton 1 1)

    it "an ineligible or blocked viewer has no graph" $ do
      buildWalkGraph 1 (Set.fromList [2, 3]) Set.empty [(2, 3)] `shouldBe` Nothing
      buildWalkGraph 1 (Set.fromList [1, 2]) (Set.singleton 1) [] `shouldBe` Nothing

  describe "ranking" $ do
    prop "candidates are scored nodes that are neither the viewer nor connected" $ \world ->
      forAll (sublistOf [1 .. 8]) $ \connectedList ->
        let connected = Set.fromList connectedList
            scores = exactScores 10 world
            ranked = rankSuggestions 50 (wViewer world) connected scores
        in all
             (\(candidate, score) ->
                score > 0
                  && candidate /= wViewer world
                  && not (Set.member candidate connected)
                  && Map.lookup candidate scores == Just score)
             ranked

    prop "the order is score descending, then account id, without repeats" $ \world ->
      let ranked = rankSuggestions 50 (wViewer world) Set.empty (exactScores 10 world)
          keys = [(Down score, candidate) | (candidate, score) <- ranked]
      in keys == sortOn id keys && Set.size (Set.fromList (map fst ranked)) == length ranked

    prop "the limit is respected and a non-positive limit gives nothing" $ \world ->
      forAll (chooseInt (-2, 6)) $ \limit ->
        let ranked = rankSuggestions limit (wViewer world) Set.empty (exactScores 10 world)
        in length ranked <= max 0 limit

    prop "the result does not depend on the order or repetition of follow rows" $ \world ->
      forAll (shuffle (wFollows world ++ wFollows world)) $ \reordered ->
        exactScores 10 world == exactScores 10 world { wFollows = reordered }

  describe "stored result" $ do
    prop "candidates are within the support, the stored limit and the graph" $ \world ->
      let base = followGraph (wEligible world) (wFollows world)
          connected = Set.fromList
            ([followed | (follower, followed) <- wFollows world, follower == wViewer world]
              ++ [follower | (follower, followed) <- wFollows world, followed == wViewer world])
          stored = suggestionsForViewer base (wBlocked world) connected (wViewer world)
            :: Maybe (ViewerSuggestions Double)
      in case stored of
           Nothing -> False
           Just result ->
             let candidates = map fst (vsCandidates result)
             in length candidates <= storedSuggestionLimit
                  && Set.member (wViewer world) (vsSupport result)
                  && all (`Set.member` vsSupport result) candidates
                  && all (`Set.notMember` connected) candidates
                  && vsSupport result
                       `Set.isSubsetOf` (wEligible world `Set.difference` wBlocked world)
  where
    exact :: Int -> WalkGraph -> Map.Map Int64 Rational
    exact = walkScores walkDamping
