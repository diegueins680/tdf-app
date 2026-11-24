{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.Fans.ArtistSimilaritySpec (spec) where

import           Data.Int                       (Int64)
import           Data.List                      (nub, sortOn)
import qualified Data.Map.Strict                as Map
import           Data.Ord                       (Down (..))
import qualified Data.Set                       as Set
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Fans.ArtistRecommendations
import           TDF.Fans.ArtistSimilarity

-- | A small world: fans 101..106 and artists 1..6. Fan ids overlap artist ids
-- on purpose in 'siOwn' rows to exercise the own-profile rule.
data World = World
  { wListed   :: Set.Set Int64
  , wOptedOut :: Set.Set Int64
  , wInputs   :: SignalInputs
  } deriving (Show)

genArtist :: Gen Int64
genArtist = choose (1, 6)

genFan :: Gen Int64
genFan = frequency [(9, choose (101, 106)), (1, genArtist)]

instance Arbitrary World where
  arbitrary = do
    listed <- Set.fromList <$> listOf genArtist
    optedOut <- Set.fromList <$> listOf genFan
    let rows = listOf ((,) <$> genFan <*> genArtist)
    follows <- rows
    favourites <- rows
    rsvps <- rows
    plays <- listOf ((,,) <$> genFan <*> genArtist <*> choose (-2, 14))
    pure (World listed optedOut (SignalInputs follows favourites rsvps plays))

weightsOf :: World -> Map.Map (Int64, Int64) Int
weightsOf w = signalWeights (wListed w) (wOptedOut w) (wInputs w)

similarityOf :: World -> Similarity
similarityOf = similarityFromWeights . weightsOf

-- | One request against a world.
data Request = Request
  { rqLimit     :: Int
  , rqExcluded  :: Set.Set Int64
  , rqFollowers :: Map.Map Int64 Int
  , rqSeeds     :: Map.Map Int64 Int
  } deriving (Show)

instance Arbitrary Request where
  arbitrary =
    Request
      <$> choose (-1, 10)
      <*> (Set.fromList <$> listOf genArtist)
      <*> (Map.fromList <$> listOf ((,) <$> genArtist <*> choose (-1, 9)))
      <*> (Map.fromList <$> listOf ((,) <$> genArtist <*> choose (-1, 23)))

rank :: World -> Request -> [ArtistRecommendation]
rank w r =
  rankBySimilarity (rqLimit r) (wListed w) (rqExcluded r) (rqFollowers r) (rqSeeds r)
    (similarityOf w)

close :: Double -> Double -> Bool
close a b = abs (a - b) < 1e-9

spec :: Spec
spec = describe "multi-signal artist similarity (COMMUNITY-ARTIST-SIMILARITY-001)" $ do
  let listed = Set.fromList [1 .. 5]
      none = Set.empty

  describe "weights" $ do
    it "orders the signals: favourite = follow > RSVP > any number of plays" $ do
      favouritePoints `shouldBe` followPoints
      followPoints `shouldSatisfy` (> rsvpPoints)
      map playPoints [-3, 0, 1, 2, 3, 9, 10, 5000] `shouldBe` [0, 0, 1, 1, 2, 2, 3, 3]
      rsvpPoints `shouldSatisfy` (> playPoints maxBound)
      maxSignalWeight `shouldBe` 23

    it "adds the components of one fan and one artist, each counted once" $ do
      let inputs =
            SignalInputs
              { siFollows = [(101, 1), (101, 1)]
              , siFavourites = [(101, 1), (101, 1), (102, 1)]
              , siRsvps = [(101, 1), (103, 1), (103, 1)]
              , siPlays = [(101, 1, 4), (101, 1, 6), (104, 1, 2), (105, 1, 0), (105, 1, -3)]
              }
      signalWeights listed none inputs
        `shouldBe` Map.fromList
          [((101, 1), 23), ((102, 1), 8), ((103, 1), 4), ((104, 1), 1)]

    it "gives no vector to an unlisted artist and ignores a party's own profile" $ do
      let inputs = SignalInputs [(101, 1), (101, 9), (2, 2)] [(3, 3)] [(4, 4)] [(5, 5, 20)]
      signalWeights listed none inputs `shouldBe` Map.fromList [((101, 1), 8)]

    it "keeps only the public follow of a fan who switched personalisation off" $ do
      let inputs = SignalInputs [(101, 1)] [(101, 1), (101, 2)] [(101, 3)] [(101, 4, 12)]
      signalWeights listed (Set.fromList [101]) inputs `shouldBe` Map.fromList [((101, 1), 8)]

    it "bounds every weight by 1 and maxSignalWeight" $ property $ \w ->
      all (\weight -> weight >= 1 && weight <= maxSignalWeight) (Map.elems (weightsOf w))

    it "does not depend on the order of the signal rows" $ property $ \w ->
      let SignalInputs follows favourites rsvps plays = wInputs w
      in forAll
           (SignalInputs <$> shuffle follows <*> shuffle favourites <*> shuffle rsvps
              <*> shuffle plays)
           (\shuffled -> signalWeights (wListed w) (wOptedOut w) shuffled === weightsOf w)

  describe "similarity" $ do
    it "is the cosine of the weighted fan vectors" $ do
      -- Artist 1: fans 101 (8), 102 (8). Artist 2: fans 101 (8), 102 (4), 103 (8).
      let weights =
            Map.fromList
              [((101, 1), 8), ((102, 1), 8), ((101, 2), 8), ((102, 2), 4), ((103, 2), 8)]
          similarity = similarityFromWeights weights
      simNorms similarity `shouldBe` Map.fromList [(1, 128), (2, 144)]
      simPairs similarity `shouldBe` Map.fromList [((1, 2), PairStat 96 2)]
      similarityValue similarity 1 2 `shouldSatisfy` close (96 / sqrt (128 * 144))
      similarityValue similarity 2 1 `shouldBe` similarityValue similarity 1 2
      similarityValue similarity 1 1 `shouldBe` 0

    it "needs two distinct shared fans" $ do
      let one = similarityFromWeights (Map.fromList [((101, 1), 23), ((101, 2), 23)])
      minimumSupport `shouldBe` 2
      simPairs one `shouldBe` Map.empty
      similarityValue one 1 2 `shouldBe` 0

    it "is symmetric and between 0 and 1" $ property $ \w ->
      let similarity = similarityOf w
      in conjoin
           [ counterexample (show (a, b)) $
               value >= 0 && value <= 1 && value == similarityValue similarity b a
           | a <- [1 .. 6]
           , b <- [1 .. 6]
           , let value = similarityValue similarity a b
           ]

    it "stores a pair only with enough support, smaller id first, and both norms" $
      property $ \w ->
        let similarity = similarityOf w
        in all
             (\((a, b), stat) ->
               a < b && psSupport stat >= minimumSupport && psDot stat > 0
                 && Map.member a (simNorms similarity)
                 && Map.member b (simNorms similarity))
             (Map.toList (simPairs similarity))

    it "treats a row that the job cannot produce as no similarity" $ do
      let broken norms stat = Similarity (Map.fromList norms) (Map.fromList [((1, 2), stat)])
      similarityValue (broken [(1, 4), (2, 4)] (PairStat 4 1)) 1 2 `shouldBe` 0
      similarityValue (broken [(1, 4)] (PairStat 4 2)) 1 2 `shouldBe` 0
      similarityValue (broken [(1, 0), (2, 4)] (PairStat 4 2)) 1 2 `shouldBe` 0
      similarityValue (broken [(1, 1), (2, 1)] (PairStat 400 2)) 1 2 `shouldBe` 1

  describe "monotonicity" $ do
    -- Raw shared evidence before the minimum-support cut.
    let evidence weights a b =
          let fans = Set.toList (Set.map fst (Map.keysSet weights))
              weight fan artist = Map.findWithDefault 0 (fan, artist) weights
          in ( sum [weight fan a * weight fan b | fan <- fans]
             , length [() | fan <- fans, weight fan a > 0, weight fan b > 0]
             )

    it "a new signal never lowers the shared evidence or the support of a pair" $
      property $ \w ->
        forAll ((,) <$> genFan <*> genArtist) $ \(fan, artist) ->
          forAll (elements [0 :: Int, 1, 2, 3]) $ \kind ->
            let inputs = wInputs w
                more = case kind of
                  0 -> inputs { siFollows = (fan, artist) : siFollows inputs }
                  1 -> inputs { siFavourites = (fan, artist) : siFavourites inputs }
                  2 -> inputs { siRsvps = (fan, artist) : siRsvps inputs }
                  _ -> inputs { siPlays = (fan, artist, 1) : siPlays inputs }
                wBefore = weightsOf w
                wAfter = signalWeights (wListed w) (wOptedOut w) more
            in conjoin
                 [ counterexample (show (artist, other)) $
                     fst (evidence wAfter artist other) >= fst (evidence wBefore artist other)
                       && snd (evidence wAfter artist other) >= snd (evidence wBefore artist other)
                 | other <- [1 .. 6]
                 , other /= artist
                 ]

    it "with follows only, a new shared fan never lowers the similarity" $
      property $ \(follows :: [(Int, Int)]) ->
        let everyone = Set.fromList [1 .. 4]
            rows = [(fromIntegral (101 + abs fan `mod` 6), fromIntegral (1 + abs a `mod` 4))
                   | (fan, a) <- follows]
            similarityWith extra =
              similarityFromWeights
                (signalWeights everyone Set.empty (SignalInputs (extra ++ rows) [] [] []))
            newFan = 200
        in similarityValue (similarityWith [(newFan, 1), (newFan, 2)]) 1 2
             >= similarityValue (similarityWith []) 1 2 - 1e-12

    it "with unequal weights a new signal can lower the cosine (documented limit)" $ do
      -- Two fans weigh both artists 1; similarity 1. The first fan's weight on
      -- artist 1 rises to 2: 3 / sqrt 10.
      let wBefore = Map.fromList [((101, 1), 1), ((102, 1), 1), ((101, 2), 1), ((102, 2), 1)]
          wAfter = Map.insert (101, 1) 2 wBefore
      similarityValue (similarityFromWeights wBefore) 1 2 `shouldSatisfy` close 1
      similarityValue (similarityFromWeights wAfter) 1 2 `shouldSatisfy` close (3 / sqrt 10)

  describe "ranking" $ do
    -- Fans 101 and 102 follow artists 1 and 2; fans 103 and 104 follow 1 and
    -- favourite 3; fan 105 follows 4 alone.
    let inputs =
          SignalInputs
            { siFollows = [(101, 1), (101, 2), (102, 1), (102, 2), (103, 1), (104, 1), (105, 4)]
            , siFavourites = [(103, 3), (104, 3)]
            , siRsvps = []
            , siPlays = []
            }
        similarity = similarityFromWeights (signalWeights listed none inputs)
        followers = Map.fromList [(1, 4), (2, 2), (4, 1)]
        ranked seeds = rankBySimilarity 10 listed (Set.fromList [1]) followers seeds similarity

    it "scores by seed strength times similarity and completes with the fallback" $ do
      let out = ranked (Map.fromList [(1, 8)])
          expected = 128 / sqrt (256 * 128)
      map recArtistId out `shouldBe` [2, 3, 4]
      map recReason out `shouldBe` [SimilarFans, SimilarFans, Popular]
      map recSeedArtistIds out `shouldBe` [[1], [1], []]
      map recScore out `shouldSatisfy` \scores -> case scores of
        [a, b, c] -> close a expected && close b expected && c == 0
        _ -> False

    it "breaks a score tie by follower count, then by id" $ do
      let tied = Map.fromList [(1, 4), (2, 2), (3, 2)]
      map recArtistId (ranked (Map.fromList [(1, 8)])) `shouldBe` [2, 3, 4]
      map recArtistId
        (rankBySimilarity 10 listed (Set.fromList [1]) tied (Map.fromList [(1, 8)]) similarity)
        `shouldBe` [2, 3]
      map recArtistId
        (rankBySimilarity 10 listed (Set.fromList [1]) (Map.insert 3 9 tied)
           (Map.fromList [(1, 8)]) similarity)
        `shouldBe` [3, 2]

    it "weighs a weak seed less and caps a strong one at a follow" $ do
      let score seeds = map recScore (take 1 (ranked seeds))
          full = 128 / sqrt (256 * 128)
      score (Map.fromList [(1, 4)]) `shouldSatisfy` all (close (full / 2))
      score (Map.fromList [(1, 23)]) `shouldSatisfy` all (close full)

    it "never uses a hidden artist as a result or as a seed" $ do
      let rankListed visible =
            rankBySimilarity 10 visible (Set.fromList [1]) followers (Map.fromList [(1, 8)])
              similarity
      map recArtistId (rankListed (Set.fromList [1, 3, 4])) `shouldBe` [3, 4]
      map recReason (rankListed (Set.fromList [2, 3, 4])) `shouldBe` [Popular, Popular]

    it "admits as seeds only listed artists that are followed or followable" $ do
      -- Fan 9 follows 1; 3 is blocked (not followable); 9 is the fan's own party.
      let frame =
            RecommendationFrame
              { rfListed = Set.fromList [1, 2, 3, 4, 9]
              , rfFollowed = Set.fromList [1, 5]
              , rfExcluded = Set.fromList [1, 5, 9, 3]
              , rfFollowers = Map.empty
              }
      frameSeedEligible frame `shouldBe` Set.fromList [1, 2, 4]

    it "never names a blocked artist the fan favourited as a seed" $ do
      -- Fan 107 follows nobody and favourites 3, which is similar to 1. Once
      -- 3 is blocked it is excluded, leaves the seeds and explains nothing.
      let frame blocked =
            RecommendationFrame
              { rfListed = listed
              , rfFollowed = Set.empty
              , rfExcluded = Set.fromList [107] `Set.union` blocked
              , rfFollowers = followers
              }
          seeds = Map.fromList [(3, 8)]
          rankFor blocked =
            rankBySimilarity 10 listed (rfExcluded (frame blocked)) followers
              (Map.restrictKeys seeds (frameSeedEligible (frame blocked)))
              similarity
          named = concatMap recSeedArtistIds
      named (rankFor Set.empty) `shouldSatisfy` elem 3
      named (rankFor (Set.fromList [3])) `shouldSatisfy` notElem 3
      map recArtistId (rankFor (Set.fromList [3])) `shouldSatisfy` notElem 3

    it "a seed admitted by the frame is listed, and followed or not excluded" $ property $
      \(w :: World) (followed :: [Int64]) (excluded :: [Int64]) ->
        let frame =
              RecommendationFrame
                { rfListed = wListed w
                , rfFollowed = Set.fromList followed
                , rfExcluded = Set.fromList (followed ++ excluded)
                , rfFollowers = Map.empty
                }
        in all
             (\seed ->
               Set.member seed (wListed w)
                 && (Set.member seed (rfFollowed frame) || not (Set.member seed (rfExcluded frame))))
             (Set.toList (frameSeedEligible frame))

    it "returns at most the limit, each artist once" $ property $ \w r ->
      let ids = map recArtistId (rank w r)
      in length ids <= max 0 (rqLimit r) && nub ids == ids

    it "returns only listed artists that are not excluded" $ property $ \w r ->
      all
        (\item ->
          Set.member (recArtistId item) (wListed w)
            && not (Set.member (recArtistId item) (rqExcluded r)))
        (rank w r)

    it "names only listed seeds of the caller, never the result itself" $ property $ \w r ->
      all
        (\item ->
          all
            (\seed ->
              Set.member seed (wListed w)
                && Map.findWithDefault 0 seed (rqSeeds r) > 0
                && seed /= recArtistId item)
            (recSeedArtistIds item)
            && length (recSeedArtistIds item) <= maxRecommendationSeeds)
        (rank w r)

    it "orders similar artists by score, follower count and id, before the fallback" $
      property $ \w r ->
        let out = rank w { wListed = Set.fromList [1 .. 6] } r { rqLimit = 24 }
            (similar, rest) = span ((== SimilarFans) . recReason) out
            key item =
              ( Down (recScore item)
              , Down (Map.findWithDefault 0 (recArtistId item) (rqFollowers r))
              , recArtistId item
              )
        in all ((== Popular) . recReason) rest
             && all (\item -> recScore item > 0 && not (null (recSeedArtistIds item))) similar
             && all (\item -> recScore item == 0 && null (recSeedArtistIds item)) rest
             && map key similar == sortOn id (map key similar)
             && nub (map key similar) == map key similar

    it "bounds a score by the number of seeds" $ property $ \w r ->
      let seeds = length [() | (seed, weight) <- Map.toList (rqSeeds r), weight > 0
                             , Set.member seed (wListed w)]
      in all (\item -> recScore item >= 0 && recScore item <= fromIntegral seeds + 1e-9)
           (rank w r)

  describe "fallback and refinement" $ do
    it "without seeds the result is exactly the popularity fallback" $ property $ \w r ->
      rank w r { rqSeeds = Map.empty }
        === rankArtistRecommendations (rqLimit r) (wListed w) (rqExcluded r) (rqFollowers r) []

    it "without a similarity the result is exactly the popularity fallback" $ property $ \w r ->
      rankBySimilarity (rqLimit r) (wListed w) (rqExcluded r) (rqFollowers r) (rqSeeds r)
        (Similarity Map.empty Map.empty)
        === rankArtistRecommendations (rqLimit r) (wListed w) (rqExcluded r) (rqFollowers r) []

    it "the rows a request reads from the table rank exactly like the whole relation" $
      property $ \w r ->
        let whole = similarityOf w
            stored = restrictSimilarity (Map.keysSet (rqSeeds r)) whole
        in rankBySimilarity (rqLimit r) (wListed w) (rqExcluded r) (rqFollowers r) (rqSeeds r)
             stored
             === rank w r

    it "the stored part is a subset of the relation with unchanged values" $ property $ \w r ->
      let whole = similarityOf w
          stored = restrictSimilarity (Map.keysSet (rqSeeds r)) whole
      in Map.isSubmapOf (simPairs stored) (simPairs whole)
           && Map.isSubmapOf (simNorms stored) (simNorms whole)
