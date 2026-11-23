{-# LANGUAGE OverloadedStrings #-}

-- | Affinity ranking of the fan-club "hot" feed (COMMUNITY-FEED-AFFINITY-001,
-- @formal/system/feed-affinity-ranking.md@).
--
-- Today's order is @reactions / max 1 ageHours ** 0.8@, the same for every
-- viewer. In personalised mode an item is scored
--
-- > affinity viewer author * typeWeight kind * (reactions + 1) / max 1 ageHours ** 0.8
--
-- where the affinity lies in @[1, 3]@ and grows with the viewer's recent
-- reactions and replies to that author. A request is personalised only when
-- the runtime gate is on, the viewer has not opted out and the viewer has
-- evidence for at least one author; otherwise the previous score is used.
--
-- Ranking only reorders its input: it never adds or drops an item, so what a
-- viewer may see is decided entirely by the caller. The ranking is a pure,
-- total function; the loader reads the viewer's own rows and writes nothing.
module TDF.Fans.FeedAffinity
  ( FeedItemKind (..)
  , feedItemKindFromCode
  , RankItem (..)
  , AuthorEvidence (..)
  , FeedMode
  , fallbackMode
  , feedMode
  , isPersonalised
  , affinityWindowDays
  , replyWeight
  , interactionCap
  , minAffinity
  , maxAffinity
  , typeWeight
  , evidenceUnits
  , affinityFromEvidence
  , modeAffinity
  , legacyHotScore
  , personalisedScore
  , itemScore
  , rankHot
  , evidenceCutoff
  , tallyEvidence
  , canonicalReactionEvidenceSql
  , canonicalReplyEvidenceSql
  , personalizationPreferenceSql
  , loadFeedMode
  ) where

import           Data.Int                 (Int64)
import           Data.List                (sortBy)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (mapMaybe)
import           Data.Ord                 (Down (..), comparing)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time                (UTCTime, addUTCTime, diffUTCTime)
import           Database.Persist         (Entity (..), PersistValue (..), selectList, (!=.),
                                           (==.), (>=.))
import           Database.Persist.Sql     (Single (..), SqlPersistT, fromSqlKey, rawSql)
import           Database.Persist.SqlBackend (getRDBMS)
import           Text.Read                (readMaybe)

import qualified TDF.Interactions.Legacy  as Interactions
import           TDF.Models               (PartyId)
import qualified TDF.Models               as M

-- | The two kinds of item a club feed carries. The order is the tie-break
-- order: a post precedes a memory with the same score.
data FeedItemKind = PostItem | MemoryItem
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Kind of a feed item from its wire code. The feed only produces @post@
-- and @memory@; any other code ranks with memories.
feedItemKindFromCode :: Text -> FeedItemKind
feedItemKindFromCode "post" = PostItem
feedItemKindFromCode _      = MemoryItem

-- | What ranking reads of an item. @(riKind, riId)@ identifies it.
data RankItem a = RankItem
  { riKind      :: !FeedItemKind
  , riId        :: !Int64
  , riAuthorId  :: !Int64
  , riCreatedAt :: !UTCTime
  , riReactions :: !Int
  , riPayload   :: a
  } deriving (Eq, Show)

-- | The viewer's recent interactions with one author.
data AuthorEvidence = AuthorEvidence
  { aeReactions :: !Int
    -- ^ items of the author the viewer currently reacts to
  , aeReplies   :: !Int
    -- ^ replies of the viewer to items of the author
  } deriving (Eq, Show)

-- | How a request is ranked. The constructors are private: 'feedMode' is the
-- only way to obtain a personalised mode.
data FeedMode
  = Fallback
  | Personalised !(Map.Map Int64 AuthorEvidence)
  deriving (Eq, Show)

fallbackMode :: FeedMode
fallbackMode = Fallback

isPersonalised :: FeedMode -> Bool
isPersonalised Fallback         = False
isPersonalised (Personalised _) = True

-- | Evidence older than this many days is not read.
affinityWindowDays :: Int
affinityWindowDays = 90

-- | One reply counts as this many reactions.
replyWeight :: Int
replyWeight = 2

-- | Interaction units beyond this add no affinity.
interactionCap :: Int
interactionCap = 20

minAffinity, maxAffinity :: Double
minAffinity = 1
maxAffinity = 3

-- | Weight of an item kind. Both are 1 until the owner sets them apart.
typeWeight :: FeedItemKind -> Double
typeWeight PostItem   = 1
typeWeight MemoryItem = 1

-- | @reactions + replyWeight * replies@, never negative.
evidenceUnits :: AuthorEvidence -> Int
evidenceUnits (AuthorEvidence reactions replies) =
  max 0 reactions + replyWeight * max 0 replies

-- | @1 + min units cap / 10@: 1 without evidence, 3 from 'interactionCap' on.
affinityFromEvidence :: AuthorEvidence -> Double
affinityFromEvidence evidence =
  minAffinity
    + (maxAffinity - minAffinity) * fromIntegral (min interactionCap (evidenceUnits evidence))
        / fromIntegral interactionCap

-- | Affinity of the viewer to an author under a mode; 1 in fallback mode and
-- for an author without evidence.
modeAffinity :: FeedMode -> Int64 -> Double
modeAffinity Fallback _ = minAffinity
modeAffinity (Personalised evidence) author =
  maybe minAffinity affinityFromEvidence (Map.lookup author evidence)

-- | Decide the mode. Personalised only when the gate is on, the viewer allows
-- personalisation and some author other than the viewer has evidence.
feedMode :: Bool -> Bool -> Int64 -> Map.Map Int64 AuthorEvidence -> FeedMode
feedMode gateOn allowed viewer evidence
  | gateOn && allowed && not (Map.null useful) = Personalised useful
  | otherwise = Fallback
  where
    useful = Map.filter ((> 0) . evidenceUnits) (Map.delete viewer evidence)

ageDecay :: UTCTime -> UTCTime -> Double
ageDecay now createdAt =
  let hoursSincePublish = max 1.0 (realToFrac (diffUTCTime now createdAt) / 3600.0)
  in hoursSincePublish ** 0.8

-- | The score every viewer had before this change, kept expression for
-- expression.
legacyHotScore :: UTCTime -> Int -> UTCTime -> Double
legacyHotScore now reactions createdAt =
  let total = fromIntegral reactions
  in total / ageDecay now createdAt

-- | Score of an item for a viewer whose affinity to its author is given.
personalisedScore :: UTCTime -> Double -> FeedItemKind -> Int -> UTCTime -> Double
personalisedScore now affinity kind reactions createdAt =
  affinity * typeWeight kind * fromIntegral (max 0 reactions + 1) / ageDecay now createdAt

itemScore :: FeedMode -> UTCTime -> RankItem a -> Double
itemScore Fallback now item =
  legacyHotScore now (riReactions item) (riCreatedAt item)
itemScore mode@(Personalised _) now item =
  personalisedScore now (modeAffinity mode (riAuthorId item)) (riKind item)
    (riReactions item) (riCreatedAt item)

-- | Order unpinned items for the "hot" sort: score descending, then posts
-- before memories, then newest first, then highest id first. The key is
-- unique per @(riKind, riId)@, so the result does not depend on input order.
-- In fallback mode this is the previous order wherever that was determined
-- (it was a stable sort of posts-then-memories, each newest first).
rankHot :: FeedMode -> UTCTime -> [RankItem a] -> [RankItem a]
rankHot mode now = sortBy (comparing key)
  where
    key item =
      ( Down (itemScore mode now item)
      , riKind item
      , Down (riCreatedAt item)
      , Down (riId item)
      )

-- | Start of the evidence window.
evidenceCutoff :: UTCTime -> UTCTime
evidenceCutoff = addUTCTime (negate (fromIntegral affinityWindowDays * 86400))

-- | Evidence per author from the viewer's reactions and replies. @eligible@
-- maps every item the viewer may see, and that is not hidden, to its author;
-- an interaction with any other item is not evidence. A reacted item counts
-- once however often it is listed; every listed reply counts.
tallyEvidence
  :: Map.Map (FeedItemKind, Int64) Int64
  -> [(FeedItemKind, Int64)]
  -> [(FeedItemKind, Int64)]
  -> Map.Map Int64 AuthorEvidence
tallyEvidence eligible reacted replied =
  Map.unionWith merge
    (count (\n -> AuthorEvidence n 0) (Map.keys (Map.fromList [(item, ()) | item <- reacted])))
    (count (AuthorEvidence 0) replied)
  where
    merge (AuthorEvidence r1 p1) (AuthorEvidence r2 p2) = AuthorEvidence (r1 + r2) (p1 + p2)
    count build items =
      Map.map build (Map.fromListWith (+) [(author, 1 :: Int) | author <- authorsOf items])
    authorsOf = mapMaybe (`Map.lookup` eligible)

-- | The viewer's current reactions to club items since the cutoff, once
-- canonical interactions are the authority. Parameters: viewer, cutoff.
canonicalReactionEvidenceSql :: Text
canonicalReactionEvidenceSql =
  "SELECT t.entity_kind, t.entity_key \
  \FROM interaction_reaction r \
  \JOIN interaction_target t ON t.id = r.target_id \
  \WHERE r.actor_id = ? AND r.created_at >= ? \
  \AND t.entity_kind IN ('club_post', 'club_memory')"

-- | The viewer's visible comments on club items since the cutoff, one row
-- per comment. Parameters: viewer, cutoff.
canonicalReplyEvidenceSql :: Text
canonicalReplyEvidenceSql =
  "SELECT t.entity_kind, t.entity_key \
  \FROM interaction_comment c \
  \JOIN interaction_target t ON t.id = c.target_id \
  \WHERE c.author_id = ? AND c.created_at >= ? AND c.state = 'visible' \
  \AND t.entity_kind IN ('club_post', 'club_memory')"

-- | The viewer's stored personalisation choice. Parameter: viewer.
personalizationPreferenceSql :: Text
personalizationPreferenceSql =
  "SELECT personalized FROM social_v2_preference WHERE party_id = ?"

tableInstalled :: Text -> SqlPersistT IO Bool
tableInstalled table = do
  backend <- getRDBMS
  rows <- case backend of
    "postgresql" -> rawSql "SELECT to_regclass(?) IS NOT NULL" [PersistText table]
    "sqlite" ->
      rawSql "SELECT count(*) > 0 FROM sqlite_master WHERE type = 'table' AND name = ?"
        [PersistText table]
    _ -> pure []
  pure (rows == [Single True])

-- | Whether the viewer may be served a personalised order: the preference
-- table is installed and the viewer has not switched personalisation off.
-- A viewer without a row has the column default, on.
loadPersonalizationAllowed :: PartyId -> SqlPersistT IO Bool
loadPersonalizationAllowed viewer = do
  installed <- tableInstalled "social_v2_preference"
  if not installed then pure False else do
    rows <- rawSql personalizationPreferenceSql [PersistInt64 (fromSqlKey viewer)]
    pure (all (\(Single allowed) -> allowed) (rows :: [Single Bool]))

canonicalItem :: (Single Text, Single Text) -> Maybe (FeedItemKind, Int64)
canonicalItem (Single kind, Single key) = do
  itemKind <- case kind of
    "club_post"   -> Just PostItem
    "club_memory" -> Just MemoryItem
    _             -> Nothing
  ident <- readMaybe (T.unpack key)
  pure (itemKind, ident)

-- | Decide the mode of one request. With the gate off nothing is read; for a
-- viewer who opted out no behaviour is read. @eligible@ is as for
-- 'tallyEvidence'.
loadFeedMode
  :: Bool
  -> PartyId
  -> UTCTime
  -> Map.Map (FeedItemKind, Int64) Int64
  -> SqlPersistT IO FeedMode
loadFeedMode gateOn viewer now eligible
  | not gateOn || Map.null eligible = pure fallbackMode
  | otherwise = do
      allowed <- loadPersonalizationAllowed viewer
      if not allowed then pure fallbackMode else do
        canonical <- Interactions.activated
        (reacted, replied) <- if canonical then canonicalEvidence else legacyEvidence
        pure (feedMode gateOn allowed viewerId (tallyEvidence eligible reacted replied))
  where
    viewerId = fromSqlKey viewer
    cutoff = evidenceCutoff now
    canonicalEvidence = do
      let parameters = [PersistInt64 viewerId, PersistUTCTime cutoff]
      reactions <- rawSql canonicalReactionEvidenceSql parameters
      replies <- rawSql canonicalReplyEvidenceSql parameters
      pure (mapMaybe canonicalItem reactions, mapMaybe canonicalItem replies)
    legacyEvidence = do
      postReactions <- selectList
        [ M.FanClubPostReactionReactorPartyId ==. viewer
        , M.FanClubPostReactionCreatedAt >=. cutoff
        ] []
      memoryReactions <- selectList
        [ M.FanClubMemoryReactionReactorPartyId ==. viewer
        , M.FanClubMemoryReactionCreatedAt >=. cutoff
        ] []
      replies <- selectList
        [ M.FanClubPostFanPartyId ==. viewer
        , M.FanClubPostParentId !=. Nothing
        , M.FanClubPostIsHidden ==. False
        , M.FanClubPostCreatedAt >=. cutoff
        ] []
      pure
        ( [ (PostItem, fromSqlKey (M.fanClubPostReactionPostId reaction))
          | Entity _ reaction <- postReactions ]
          ++ [ (MemoryItem, fromSqlKey (M.fanClubMemoryReactionMemoryId reaction))
             | Entity _ reaction <- memoryReactions ]
        , [ (PostItem, fromSqlKey parent)
          | Entity _ reply <- replies, Just parent <- [M.fanClubPostParentId reply] ]
        )
