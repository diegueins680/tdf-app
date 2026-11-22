{-# LANGUAGE OverloadedStrings #-}

module TDF.Fans.FeedAffinitySpec (spec) where

import           Control.Monad.Logger    (runNoLoggingT)
import           Data.Int                (Int64)
import           Data.List               (nubBy, partition, sort, sortOn)
import qualified Data.Map.Strict         as Map
import           Data.Ord                (Down (..))
import qualified Data.Text               as T
import           Data.Time               (UTCTime (..), addUTCTime, diffUTCTime, fromGregorian,
                                          secondsToDiffTime)
import           Database.Persist.Sql    (ConnectionPool, rawExecute, runSqlPool, toSqlKey)
import           Database.Persist.Sqlite (createSqlitePool)
import           System.FilePath         ((</>))
import           System.IO.Temp          (withSystemTempDirectory)
import           Test.Hspec
import           Test.QuickCheck

import           TDF.DTO                 (FanClubFeedItemDTO (..), ReactionSummaryDTO (..))
import           TDF.Fans.FeedAffinity
import           TDF.ServerFanClub       (feedEvidenceTargets, sortFeedItems)

now :: UTCTime
now = UTCTime (fromGregorian 2026 11 21) (secondsToDiffTime 43200)

viewer :: Int64
viewer = 1

-- | Unpinned or pinned feed items with unique @(kind, id)@, in any order.
newtype Feed = Feed [FanClubFeedItemDTO]

instance Show Feed where
  show (Feed items) = show (map describe items)

describe :: FanClubFeedItemDTO -> (T.Text, Int64, Int64, Int, UTCTime, Bool)
describe item =
  ( fcfKind item, fcfId item, fcfAuthorId item, rsTotal (fcfReactions item)
  , fcfCreatedAt item, fcfIsPinned item )

identity :: FanClubFeedItemDTO -> (T.Text, Int64)
identity item = (fcfKind item, fcfId item)

genItem :: Gen FanClubFeedItemDTO
genItem = do
  kind <- elements ["post", "memory"]
  ident <- choose (1, 12)
  author <- choose (1, 6)
  reactions <- frequency [(2, pure 0), (3, choose (0, 4)), (1, choose (0, 60))]
  -- A few distinct ages, so equal timestamps and equal scores do occur.
  ageSeconds <- elements [-600, 0, 1800, 3600, 7200, 86400, 600000, 7000000 :: Integer]
  pinned <- frequency [(5, pure False), (1, pure True)]
  hidden <- frequency [(6, pure False), (1, pure True)]
  pure FanClubFeedItemDTO
    { fcfId = ident
    , fcfKind = kind
    , fcfTitle = Nothing
    , fcfContent = ""
    , fcfAuthorId = author
    , fcfAuthorName = ""
    , fcfAvatarUrl = Nothing
    , fcfMediaUrls = []
    , fcfIsPinned = pinned
    , fcfIsOfficer = False
    , fcfIsHidden = hidden
    , fcfReactions = ReactionSummaryDTO [] reactions Nothing
    , fcfCreatedAt = addUTCTime (negate (fromInteger ageSeconds)) now
    }

instance Arbitrary Feed where
  arbitrary = Feed . nubBy (\a b -> identity a == identity b) <$> listOf genItem
  shrink (Feed items) = Feed <$> shrinkList (const []) items

newtype Evidence = Evidence (Map.Map Int64 AuthorEvidence)
  deriving (Show)

instance Arbitrary Evidence where
  arbitrary = Evidence . Map.fromList <$> listOf
    ((,) <$> choose (1, 6) <*> (AuthorEvidence <$> choose (-1, 30) <*> choose (-1, 15)))

-- | The order the handler produced before this change, copied from
-- @sortFeedItems@ at main 985117b3d.
legacySort :: T.Text -> UTCTime -> [FanClubFeedItemDTO] -> [FanClubFeedItemDTO]
legacySort sortMode n items =
  let (pinned, unpinned) = partition fcfIsPinned items
      sorted = case sortMode of
        "hot" -> sortOn (Down . hotScore) unpinned
        "top" -> sortOn (Down . rsTotal . fcfReactions) unpinned
        _     -> sortOn (Down . fcfCreatedAt) unpinned
  in pinned ++ sorted
  where
    hotScore :: FanClubFeedItemDTO -> Double
    hotScore item =
      let hoursSincePublish = max 1.0 (realToFrac (diffUTCTime n (fcfCreatedAt item)) / 3600.0)
          total = fromIntegral (rsTotal (fcfReactions item))
      in total / (hoursSincePublish ** 0.8)

-- | The order in which the handler hands items to the sort: posts newest
-- first, then memories newest first. Ties are broken the way the new order
-- breaks them, which the database was free to do before.
handlerOrder :: [FanClubFeedItemDTO] -> [FanClubFeedItemDTO]
handlerOrder = sortOn (\item ->
  (feedItemKindFromCode (fcfKind item), Down (fcfCreatedAt item), Down (fcfId item)))

ids :: [FanClubFeedItemDTO] -> [(T.Text, Int64)]
ids = map identity

-- | @(reactions + 1) / decay@: the personalised score at affinity 1.
baseScore :: FanClubFeedItemDTO -> Double
baseScore item =
  personalisedScore now 1 (feedItemKindFromCode (fcfKind item)) (rsTotal (fcfReactions item))
    (fcfCreatedAt item)

personalised :: Map.Map Int64 AuthorEvidence -> FeedMode
personalised = feedMode True True viewer

spec :: Spec
spec = describe "fan-club feed affinity ranking (COMMUNITY-FEED-AFFINITY-001)" $ do
  describe "mode" $ do
    it "is fallback with the gate off, for an opted-out viewer, or without evidence" $
      property $ \(Evidence evidence) gate allowed ->
        let useful = any ((> 0) . evidenceUnits) (Map.elems (Map.delete viewer evidence))
        in isPersonalised (feedMode gate allowed viewer evidence) === (gate && allowed && useful)

    it "never counts the viewer's interactions with their own items" $
      feedMode True True viewer (Map.singleton viewer (AuthorEvidence 9 9)) `shouldBe` fallbackMode

  describe "affinity" $ do
    it "is 1 without evidence, 3 at the cap, and never outside [1, 3]" $ do
      affinityFromEvidence (AuthorEvidence 0 0) `shouldBe` 1
      affinityFromEvidence (AuthorEvidence 20 0) `shouldBe` 3
      affinityFromEvidence (AuthorEvidence 0 10) `shouldBe` 3
      affinityFromEvidence (AuthorEvidence 500 500) `shouldBe` 3

    it "stays in [1, 3] and does not decrease with more interactions" $
      property $ \r p (NonNegative dr) (NonNegative dp) ->
        let low = affinityFromEvidence (AuthorEvidence r p)
            high = affinityFromEvidence (AuthorEvidence (r + dr) (p + dp))
        in low >= minAffinity .&&. high <= maxAffinity .&&. low <= high

    it "is 1 for every author in fallback mode" $
      property $ \author -> modeAffinity fallbackMode author === 1

  describe "score" $ do
    let genAge = choose (-7200, 9000000 :: Integer)
        at seconds = addUTCTime (negate (fromInteger seconds)) now
    it "rises with reactions, and with affinity, and does not rise with age" $
      forAll ((,,,) <$> choose (1, 3 :: Double) <*> choose (0, 500) <*> genAge <*> elements [PostItem, MemoryItem]) $
        \(affinity, reactions, age, kind) ->
          forAll ((,,) <$> choose (1, 100) <*> choose (0.01, 2 :: Double) <*> choose (1, 900000)) $
            \(moreReactions, moreAffinity, older) ->
              let score a r s = personalisedScore now a kind r (at s)
                  base = score affinity reactions age
              in conjoin
                   [ counterexample "reactions" (score affinity (reactions + moreReactions) age > base)
                   , counterexample "affinity" (score (affinity + moreAffinity) reactions age > base)
                   , counterexample "age" (score affinity reactions (age + older) <= base)
                   ]

    it "keeps the previous score rising with reactions and not rising with age" $
      forAll ((,,,) <$> choose (0, 500) <*> genAge <*> choose (1, 100) <*> choose (1, 900000)) $
        \(reactions, age, moreReactions, older) ->
          let base = legacyHotScore now reactions (at age)
          in legacyHotScore now (reactions + moreReactions) (at age) > base
               .&&. legacyHotScore now reactions (at (age + older)) <= base

  describe "order" $ do
    it "returns exactly the items it was given, in every mode and sort" $
      property $ \(Feed items) (Evidence evidence) ->
        forAll (elements ["hot", "top", "new", "other"]) $ \sortMode ->
          forAll (elements [fallbackMode, personalised evidence]) $ \mode ->
            sort (ids (sortFeedItems mode sortMode now items)) === sort (ids items)

    it "keeps pinned items first and in input order" $
      property $ \(Feed items) (Evidence evidence) ->
        let pinned = filter fcfIsPinned items
            sorted = sortFeedItems (personalised evidence) "hot" now items
        in ids (take (length pinned) sorted) === ids pinned

    it "equals the previous hot order in fallback mode" $
      property $ \(Feed items) ->
        let input = handlerOrder items
        in ids (sortFeedItems fallbackMode "hot" now input) === ids (legacySort "hot" now input)

    it "leaves the top and chronological sorts as they were, whatever the mode" $
      property $ \(Feed items) (Evidence evidence) ->
        forAll (elements ["top", "new", "other"]) $ \sortMode ->
          ids (sortFeedItems (personalised evidence) sortMode now items)
            === ids (legacySort sortMode now items)

    it "does not depend on the order of its input" $
      property $ \(Feed items) (Evidence evidence) ->
        forAll (shuffle items) $ \shuffled ->
          forAll (elements [fallbackMode, personalised evidence]) $ \mode ->
            let unpinned = filter (not . fcfIsPinned)
            in ids (unpinned (sortFeedItems mode "hot" now shuffled))
                 === ids (unpinned (sortFeedItems mode "hot" now items))

    it "lets an item be overtaken only by items within a factor of 3 of its base score" $
      property $ \(Feed items) (Evidence evidence) ->
        let sorted = filter (not . fcfIsPinned) (sortFeedItems (personalised evidence) "hot" now items)
            pairs = [ (y, x) | (index, x) <- zip [0 :: Int ..] sorted, y <- take index sorted ]
            within (y, x) = maxAffinity * baseScore y >= baseScore x * (1 - 1e-9)
        in conjoin [ counterexample (show (describe y, describe x)) (within (y, x)) | (y, x) <- pairs ]

    it "lifts a fresh post without reactions from an author the viewer interacts with" $ do
      let item ident author = FanClubFeedItemDTO
            { fcfId = ident, fcfKind = "post", fcfTitle = Nothing, fcfContent = ""
            , fcfAuthorId = author, fcfAuthorName = "", fcfAvatarUrl = Nothing, fcfMediaUrls = []
            , fcfIsPinned = False, fcfIsOfficer = False, fcfIsHidden = False
            , fcfReactions = ReactionSummaryDTO [] 0 Nothing
            , fcfCreatedAt = addUTCTime (-7200) now }
          feed = [item 2 20, item 1 30]
          mode = personalised (Map.singleton 30 (AuthorEvidence 5 0))
      map fcfId (sortFeedItems fallbackMode "hot" now feed) `shouldBe` [2, 1]
      map fcfId (sortFeedItems mode "hot" now feed) `shouldBe` [1, 2]

  describe "evidence" $ do
    let genItemKey = (,) <$> elements [PostItem, MemoryItem] <*> choose (1, 12 :: Int64)
        genEligible = Map.fromList <$> listOf ((,) <$> genItemKey <*> choose (1, 6 :: Int64))
    it "ignores interactions with items outside the eligible set" $
      forAll ((,,,) <$> genEligible <*> listOf genItemKey <*> listOf genItemKey <*> listOf genItemKey) $
        \(eligible, reacted, replied, noise) ->
          let outside = filter (`Map.notMember` eligible) noise
              tally = tallyEvidence eligible reacted replied
          in tallyEvidence eligible (reacted ++ outside) (replied ++ outside) === tally
               .&&. all (`elem` Map.elems eligible) (Map.keys tally)

    it "counts a reacted item once and every reply" $
      tallyEvidence (Map.fromList [((PostItem, 1), 7), ((MemoryItem, 1), 7)])
        [(PostItem, 1), (PostItem, 1), (MemoryItem, 1)] [(PostItem, 1), (PostItem, 1)]
        `shouldBe` Map.singleton 7 (AuthorEvidence 2 2)

    it "never offers a hidden item as evidence" $
      property $ \(Feed items) ->
        let targets = feedEvidenceTargets items
            hidden = [ (feedItemKindFromCode (fcfKind item), fcfId item) | item <- items, fcfIsHidden item ]
        in all (`Map.notMember` targets) hidden

  describe "loader" $ do
    let eligible = Map.fromList [((PostItem, 10), 30), ((MemoryItem, 5), 31), ((PostItem, 11), viewer)]
        load pool gate = runSqlPool (loadFeedMode gate (toSqlKey viewer) now eligible) pool
    it "reads nothing with the gate off" $ withPool $ \pool ->
      -- No table exists: any read would fail.
      load pool False `shouldReturn` fallbackMode

    it "is fallback where the preference table is not installed" $ withPool $ \pool ->
      load pool True `shouldReturn` fallbackMode

    it "is fallback for a viewer who opted out, without reading behaviour" $ withPool $ \pool -> do
      runSqlPool (do
        rawExecute "CREATE TABLE social_v2_preference(party_id INTEGER PRIMARY KEY, personalized BOOLEAN NOT NULL)" []
        rawExecute "INSERT INTO social_v2_preference VALUES(1, 0)" []) pool
      -- The behaviour tables do not exist: reading them would fail.
      load pool True `shouldReturn` fallbackMode

    it "counts recent reactions and replies to eligible items only" $ withPool $ \pool -> do
      runSqlPool (do
        rawExecute "CREATE TABLE social_v2_preference(party_id INTEGER PRIMARY KEY, personalized BOOLEAN NOT NULL)" []
        mapM_ (`rawExecute` []) behaviourTables
        -- Recent reaction to post 10 (author 30); one older than the window to
        -- memory 5; one to post 99, which the viewer cannot see; one by another fan.
        rawExecute "INSERT INTO fan_club_post_reaction VALUES('00000000-0000-4000-8000-00000000000a',10,1,'00000000-0000-4000-8000-0000000000ff','2026-11-01 00:00:00')" []
        rawExecute "INSERT INTO fan_club_post_reaction VALUES('00000000-0000-4000-8000-00000000000b',99,1,'00000000-0000-4000-8000-0000000000ff','2026-11-01 00:00:00')" []
        rawExecute "INSERT INTO fan_club_post_reaction VALUES('00000000-0000-4000-8000-00000000000c',10,2,'00000000-0000-4000-8000-0000000000ff','2026-11-01 00:00:00')" []
        rawExecute "INSERT INTO fan_club_memory_reaction VALUES('00000000-0000-4000-8000-00000000000d',5,1,'00000000-0000-4000-8000-0000000000ff','2026-05-01 00:00:00')" []
        -- Replies by the viewer: to post 10 (counts), a hidden one, one to their own post 11.
        rawExecute "INSERT INTO fan_club_post VALUES(20,1,1,10,NULL,'r',NULL,0,0,'2026-11-02 00:00:00',NULL)" []
        rawExecute "INSERT INTO fan_club_post VALUES(21,1,1,10,NULL,'r',NULL,0,1,'2026-11-02 00:00:00',NULL)" []
        rawExecute "INSERT INTO fan_club_post VALUES(22,1,1,11,NULL,'r',NULL,0,0,'2026-11-02 00:00:00',NULL)" []) pool
      mode <- load pool True
      mode `shouldBe` feedMode True True viewer (Map.singleton 30 (AuthorEvidence 1 1))
      isPersonalised mode `shouldBe` True
      modeAffinity mode 30 `shouldBe` 1.3
      modeAffinity mode 31 `shouldBe` 1

    it "is fallback for a viewer who allows personalisation but has no evidence" $ withPool $ \pool -> do
      runSqlPool (do
        rawExecute "CREATE TABLE social_v2_preference(party_id INTEGER PRIMARY KEY, personalized BOOLEAN NOT NULL)" []
        rawExecute "INSERT INTO social_v2_preference VALUES(1, 1)" []
        mapM_ (`rawExecute` []) behaviourTables) pool
      load pool True `shouldReturn` fallbackMode

behaviourTables :: [T.Text]
behaviourTables =
  [ "CREATE TABLE fan_club_post_reaction(id TEXT PRIMARY KEY, post_id INTEGER, reactor_party_id INTEGER, reaction_type_id TEXT, created_at TIMESTAMP)"
  , "CREATE TABLE fan_club_memory_reaction(id TEXT PRIMARY KEY, memory_id INTEGER, reactor_party_id INTEGER, reaction_type_id TEXT, created_at TIMESTAMP)"
  , "CREATE TABLE fan_club_post(id INTEGER PRIMARY KEY, club_id INTEGER, fan_party_id INTEGER, parent_id INTEGER, title TEXT, content TEXT, media_urls TEXT, is_pinned BOOLEAN, is_hidden BOOLEAN, created_at TIMESTAMP, updated_at TIMESTAMP)"
  ]

withPool :: (ConnectionPool -> IO a) -> IO a
withPool action = withSystemTempDirectory "tdf-feed-affinity" $ \dir -> do
  pool <- runNoLoggingT (createSqlitePool (T.pack (dir </> "feed.sqlite")) 1)
  action pool
