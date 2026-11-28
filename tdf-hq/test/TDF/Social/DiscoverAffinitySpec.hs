{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.Social.DiscoverAffinitySpec (spec) where

import           Data.Int                    (Int64)
import           Data.List                   (elemIndex, sort, sortOn)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Ord                    (Down (..))
import           Test.Hspec
import           Test.QuickCheck

import qualified TDF.Fans.FeedAffinity       as Feed
import           TDF.Social.DiscoverAffinity

-- | A request: page size, and the previous order of the candidates with the
-- units of each. Ids are unique.
data Request = Request Int [(Int64, Int)]
  deriving (Show)

instance Arbitrary Request where
  arbitrary = do
    pageSize <- frequency [(1, pure 1), (6, choose (1, 12)), (2, choose (13, 50))]
    count <- frequency [(1, pure 0), (8, choose (1, 30))]
    ids <- shuffle [1 .. fromIntegral (count :: Int)]
    units <- vectorOf count (frequency [(5, pure 0), (4, choose (1, 6)), (1, choose (7, 20))])
    pure (Request pageSize (zip ids units))
  shrink (Request pageSize base) =
    [Request pageSize smaller | smaller <- shrinkList (const []) base]
      ++ [Request smaller base | smaller <- [1 .. pageSize - 1]]

orderOf :: Request -> [(Int64, Int)]
orderOf (Request pageSize base) = discoverOrder pageSize fst snd base

-- | How many candidates lead the order of a request.
leading :: Request -> Int
leading (Request pageSize base) = min (promotedCount pageSize) (length (filter ((> 0) . snd) base))

position :: (Int64, Int) -> [(Int64, Int)] -> Int
position candidate = fromMaybe (error "candidate missing") . elemIndex candidate

-- | Page size, previous order with units, expected page. The same lines are
-- parsed by scripts/test-social-discover-affinity-sql.py, whose oracle the
-- SQL function is compared with; keep one vector per line in this shape.
referenceVectors :: [(Int, [(Int64, Int)], [Int64])]
referenceVectors =
  [ (4, [(1, 0), (2, 5), (3, 0), (4, 20), (5, 7)], [4, 5, 1, 2])
  , (1, [(1, 0), (2, 5), (3, 9)], [1])
  , (2, [(1, 0), (2, 5), (3, 9)], [3, 1])
  , (3, [(1, 0), (2, 5), (3, 9)], [3, 1, 2])
  , (6, [(9, 0), (7, 4), (8, 4), (3, 0), (2, 1)], [7, 8, 2, 9, 3])
  , (6, [(9, 3), (7, 3), (8, 3), (3, 3), (2, 0)], [3, 7, 8, 9, 2])
  , (10, [(5, 0), (4, 0), (3, 0)], [5, 4, 3])
  , (50, [], [])
  , (2, [(4, 20), (3, 20), (2, 20)], [2, 4])
  ]

spec :: Spec
spec = describe "TDF.Social.DiscoverAffinity (SOCIAL-DISCOVER-AFFINITY-001)" $ do
  describe "constants and units" $ do
    it "uses the constants of the club feed affinity" $ do
      affinityWindowDays `shouldBe` Feed.affinityWindowDays
      replyWeight `shouldBe` Feed.replyWeight
      interactionCap `shouldBe` Feed.interactionCap

    it "counts a reaction once, a reply twice, and stops at the cap" $ do
      evidenceUnits 0 0 `shouldBe` 0
      evidenceUnits 3 0 `shouldBe` 3
      evidenceUnits 1 2 `shouldBe` 5
      evidenceUnits 20 0 `shouldBe` 20
      evidenceUnits 19 1 `shouldBe` 20
      evidenceUnits 0 11 `shouldBe` 20

    it "agrees with the club feed units below the cap and never leaves 0..20" $
      property $ \reactions replies ->
        let units = evidenceUnits reactions replies
        in units === min interactionCap (Feed.evidenceUnits (Feed.AuthorEvidence reactions replies))
             .&&. (units >= 0 && units <= interactionCap)

    it "never falls when a reaction or a reply is added" $
      property $ \(NonNegative reactions) (NonNegative replies) ->
        evidenceUnits (reactions + 1) replies >= evidenceUnits reactions replies
          && evidenceUnits reactions (replies + 1) >= evidenceUnits reactions replies

    it "promotes at most half a page" $ do
      map promotedCount [-3, 0, 1, 2, 3, 20, 50] `shouldBe` [0, 0, 0, 1, 1, 10, 25]

  describe "order" $ do
    it "reproduces the reference vectors" $
      mapM_
        (\(pageSize, base, expected) ->
           map fst (discoverPage pageSize fst snd base) `shouldBe` expected)
        referenceVectors

    it "returns every candidate exactly once" $
      property $ \request@(Request _ base) -> sort (orderOf request) === sort base

    it "is the previous order when no candidate has units" $
      property $ \(Request pageSize base) ->
        let plain = [(ident, 0) | (ident, _) <- base]
        in discoverOrder pageSize fst snd plain === plain

    it "is the previous order for a page of one" $
      property $ \(Request _ base) -> discoverOrder 1 fst snd base === base

    it "leads with the candidates of most units, ties by ascending id" $
      property $ \request@(Request _ base) ->
        let promoted = take (leading request) (orderOf request)
            expected =
              take (leading request)
                (sortOn (\(ident, units) -> (Down units, ident)) (filter ((> 0) . snd) base))
        in promoted === expected

    it "never promotes a candidate without units" $
      property $ \request -> all ((> 0) . snd) (take (leading request) (orderOf request))

    it "keeps the previous order among all other candidates" $
      property $ \request@(Request _ base) ->
        let rest = drop (leading request) (orderOf request)
        in rest === filter (`elem` rest) base

    it "moves no other candidate down more than half a page, and none up" $
      property $ \request@(Request pageSize base) ->
        let ordered = orderOf request
            rest = drop (leading request) ordered
            moved candidate = position candidate ordered - position candidate base
        in conjoin
             [ counterexample (show candidate)
                 (moved candidate >= 0 && moved candidate <= promotedCount pageSize)
             | candidate <- rest ]

    it "depends on the units of the listed candidates only" $
      property $ \request@(Request pageSize base) (extra :: [(Int64, Int)]) ->
        -- Units recorded for parties that are not candidates change nothing.
        let table = Map.union (Map.fromList base) (Map.fromList [(ident + 1000, units) | (ident, units) <- extra])
            viaTable = discoverOrder pageSize fst (\(ident, _) -> Map.findWithDefault 0 ident table) base
        in viaTable === orderOf request

    it "returns the first page_size candidates of the order" $
      property $ \request@(Request pageSize base) ->
        discoverPage pageSize fst snd base === take pageSize (orderOf request)

  describe "statement" $ do
    it "runs the affinity form only with the gate on and the function installed" $ do
      [runsAffinityStatement gateOn installed | gateOn <- [False, True], installed <- [False, True]]
        `shouldBe` [False, False, False, True]

    it "keeps the statement every request ran before this change" $
      baseDiscoverSql `shouldBe` "SELECT social_v2_discover(?,?)::text"

    it "asks for the affinity order explicitly" $
      affinityDiscoverSql `shouldBe` "SELECT social_v2_discover(?,?,true)::text"
