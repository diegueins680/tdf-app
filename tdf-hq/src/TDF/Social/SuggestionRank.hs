-- | Friend suggestions by personalized PageRank (COMMUNITY-FRIEND-SUGGESTION-001).
--
-- The ranking of formal/system/friend-suggestions-pagerank.md as pure, total
-- functions. A walker starts at the viewer; at each step it follows one of the
-- current account's follows with probability 'walkDamping' and otherwise
-- returns to the viewer. A walker on an account that follows nobody returns to
-- the viewer too.
--
-- An account that is not eligible, or that has a block with the viewer in
-- either direction, is not a node of the viewer's graph. It therefore holds no
-- score and no edge through which score could reach another account.
--
-- The functions are polymorphic in the score type so that the tests can run
-- them over 'Rational', where every statement is exact, and production over
-- 'Double'.
module TDF.Social.SuggestionRank
  ( walkDamping
  , walkIterations
  , walkErrorBound
  , storedSuggestionLimit
  , servedSuggestionLimit
  , WalkGraph
  , walkViewer
  , walkNodes
  , walkFollows
  , followGraph
  , viewerGraph
  , buildWalkGraph
  , initialScores
  , walkStep
  , walkScores
  , scoreSupport
  , rankSuggestions
  , ViewerSuggestions (..)
  , suggestionsForViewer
  ) where

import           Data.Int        (Int64)
import           Data.List       (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Ord        (Down (..))
import qualified Data.Set        as Set

-- | Probability of following an edge; the walker restarts with the rest.
walkDamping :: Fractional a => a
walkDamping = 0.85

-- | Number of steps computed by the nightly run.
walkIterations :: Int
walkIterations = 100

-- | Upper bound of the L1 distance between the stored scores and the
-- stationary scores: @2 * walkDamping ^ walkIterations@ is below it.
walkErrorBound :: Fractional a => a
walkErrorBound = 2.0e-7

-- | Candidates stored per viewer by a run.
storedSuggestionLimit :: Int
storedSuggestionLimit = 50

-- | Candidates returned by the route, as before this change.
servedSuggestionLimit :: Int
servedSuggestionLimit = 20

-- | The graph one viewer walks. Every node has an entry in 'walkFollows'; the
-- targets of an entry are distinct nodes other than the entry's own.
data WalkGraph = WalkGraph
  { walkViewer  :: !Int64
  , walkFollows :: !(Map.Map Int64 [Int64])
  } deriving (Eq, Show)

walkNodes :: WalkGraph -> Set.Set Int64
walkNodes = Map.keysSet . walkFollows

-- | The follows between eligible accounts, before any viewer's blocks. Every
-- eligible account has an entry. Follows are @(follower, followed)@;
-- self-follows, repeated rows and rows with an ineligible end are dropped.
followGraph :: Set.Set Int64 -> [(Int64, Int64)] -> Map.Map Int64 [Int64]
followGraph eligible follows = Map.map Set.toAscList targets
  where
    isolated = Map.fromSet (const Set.empty) eligible
    targets =
      foldl' addFollow isolated
        [ (follower, followed)
        | (follower, followed) <- follows
        , follower /= followed
        , Set.member follower eligible
        , Set.member followed eligible
        ]
    addFollow acc (follower, followed) =
      Map.adjust (Set.insert followed) follower acc

-- | One viewer's graph: 'followGraph' without the accounts that have a block
-- towards or from the viewer (@blocked@). 'Nothing' when the viewer is not a
-- node.
viewerGraph :: Int64 -> Set.Set Int64 -> Map.Map Int64 [Int64] -> Maybe WalkGraph
viewerGraph viewer blocked base
  | Set.member viewer blocked || not (Map.member viewer base) = Nothing
  | Set.null blocked = Just (WalkGraph viewer base)
  | otherwise =
      Just . WalkGraph viewer $
        Map.map (filter (`Set.notMember` blocked)) (Map.withoutKeys base blocked)

-- | 'viewerGraph' of 'followGraph'.
buildWalkGraph
  :: Int64
  -> Set.Set Int64
  -> Set.Set Int64
  -> [(Int64, Int64)]
  -> Maybe WalkGraph
buildWalkGraph viewer eligible blocked follows =
  viewerGraph viewer blocked (followGraph eligible follows)

-- | All mass on the viewer.
initialScores :: Num a => WalkGraph -> Map.Map Int64 a
initialScores graph = Map.singleton (walkViewer graph) 1

-- | One step of the walk. Accounts absent from the map hold no mass; mass on
-- an account outside the graph is treated as mass on a dead end.
walkStep :: Fractional a => a -> WalkGraph -> Map.Map Int64 a -> Map.Map Int64 a
walkStep damping graph scores =
  Map.insertWith (+) (walkViewer graph) restart forwarded
  where
    total = sum (Map.elems scores)
    (forwarded, moved) = Map.foldlWithKey' forward (Map.empty, 0) scores
    forward (acc, sent) node mass =
      case Map.findWithDefault [] node (walkFollows graph) of
        [] -> (acc, sent)
        followed ->
          let share = damping * mass / fromIntegral (length followed)
          in ( foldl' (\m target -> Map.insertWith (+) target share m) acc followed
             , sent + damping * mass
             )
    -- Everything that did not move along a follow returns to the viewer: the
    -- restart share of every account plus the whole mass of dead ends.
    restart = total - moved

-- | Scores after @steps@ steps from 'initialScores'.
walkScores :: Fractional a => a -> Int -> WalkGraph -> Map.Map Int64 a
walkScores damping steps graph =
  foldl' (\scores _ -> walkStep damping graph scores) (initialScores graph)
    [1 .. max 0 steps]

-- | The accounts that hold score: the evidence behind a viewer's ranking.
scoreSupport :: (Ord a, Num a) => Map.Map Int64 a -> Set.Set Int64
scoreSupport = Map.keysSet . Map.filter (> 0)

-- | At most @limit@ candidates, by score descending and then account id. The
-- viewer, accounts in @connected@ (followed by or following the viewer) and
-- accounts without score are never candidates.
rankSuggestions
  :: (Ord a, Num a)
  => Int
  -> Int64
  -> Set.Set Int64
  -> Map.Map Int64 a
  -> [(Int64, a)]
rankSuggestions limit viewer connected scores =
  take (max 0 limit) $
    sortOn (\(candidate, score) -> (Down score, candidate))
      [ (candidate, score)
      | (candidate, score) <- Map.toList scores
      , score > 0
      , candidate /= viewer
      , not (Set.member candidate connected)
      ]

-- | What a run stores for one viewer.
data ViewerSuggestions a = ViewerSuggestions
  { vsViewer     :: !Int64
  , vsSupport    :: !(Set.Set Int64)
  , vsCandidates :: ![(Int64, a)]
  } deriving (Eq, Show)

-- | The stored result for one viewer, or 'Nothing' when the viewer is not a
-- node. @base@ is 'followGraph'; @connected@ are the accounts the viewer
-- follows or is followed by, eligible or not. Uses the contract's damping,
-- iteration count and stored limit.
suggestionsForViewer
  :: (Ord a, Fractional a)
  => Map.Map Int64 [Int64]
  -> Set.Set Int64
  -> Set.Set Int64
  -> Int64
  -> Maybe (ViewerSuggestions a)
suggestionsForViewer base blocked connected viewer = do
  graph <- viewerGraph viewer blocked base
  let scores = walkScores walkDamping walkIterations graph
  pure ViewerSuggestions
    { vsViewer = viewer
    , vsSupport = scoreSupport scores
    , vsCandidates = rankSuggestions storedSuggestionLimit viewer connected scores
    }
