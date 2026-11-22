{-# LANGUAGE OverloadedStrings #-}

-- | SCHOOL-SLOT-SUGGESTION-001: properties of the pure slot search. These are
-- tests, not proofs: they compare 'suggestSlots' with an independent brute-force
-- reference written in whole minutes on randomly generated small calendars.
module TDF.Trials.SlotSuggestionsSpec (spec) where

import           Data.Either                (isLeft)
import           Data.Int                   (Int64)
import           Data.List                  (sort, sortOn)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (isNothing)
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime, addUTCTime)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Trials.SlotSuggestions

-- | 2026-12-01T00:00:00Z, a Tuesday; 19:00 the previous day in the studio.
baseMinute :: Integer
baseMinute = 29934720

atMinute :: Integer -> UTCTime
atMinute m = posixSecondsToUTCTime (fromInteger (m * 60))

minuteOf :: UTCTime -> Integer
minuteOf t = floor (utcTimeToPOSIXSeconds t) `div` 60

span' :: Integer -> Integer -> Interval
span' s e = Interval (atMinute s) (atMinute e)

data Scenario = Scenario
  { scRequest  :: SlotRequest
  , scSnapshot :: SlotSnapshot
  } deriving (Show)

genInterval :: Integer -> Integer -> Gen Interval
genInterval lo hi = do
  start <- choose (lo, hi)
  len <- elements [15, 30, 45, 60, 90, 120, 200]
  pure (span' start (start + len))

instance Arbitrary Scenario where
  arbitrary = do
    nowOffset <- choose (0, 1439)
    let nowMinute = baseMinute + nowOffset
    fromOffset <- frequency [(3, pure 0), (1, choose (-600, 2000))]
    days <- choose (1, 3)
    duration <- elements [30, 60, 90]
    limit <- choose (1, 6)
    let lo = nowMinute - 1440
        hi = nowMinute + fromIntegral days * 1440 + 3440
    preferred <- frequency [(2, pure []), (1, resize 2 (listOf (genWindow lo hi)))]
    rooms <- sublistOf [1, 2, 3] >>= mapM (\rid -> RoomOption rid (roomLabel rid) <$> choose (1, 3))
    teaches <- frequency [(9, pure True), (1, pure False)]
    teacherBusy <- resize 6 (listOf (genInterval lo hi))
    studentBusy <- resize 4 (listOf (genInterval lo hi))
    resourceBusy <- resize 6 (listOf ((,) <$> choose (1, 3) <*> genInterval lo hi))
    availability <- resize 8 (listOf ((,) <$> choose (1, 3) <*> genWindow lo hi))
    package <- frequency
      [ (6, pure (Just (atMinute (hi + 100000))))
      , (2, Just . atMinute <$> choose (lo, hi))
      , (1, pure Nothing)
      ]
    pure Scenario
      { scRequest = SlotRequest
          { srNow = atMinute nowMinute
          , srFrom = atMinute (nowMinute + fromOffset)
          , srDays = days
          , srDurationMinutes = duration
          , srLimit = limit
          , srPreferred = preferred
          }
      , scSnapshot = SlotSnapshot
          { ssTeacherTeachesSubject = teaches
          , ssRooms = rooms
          , ssTeacherBusy = teacherBusy
          , ssStudentBusy = studentBusy
          , ssResourceBusy = Map.fromListWith (++) [(rid, [iv]) | (rid, iv) <- resourceBusy]
          , ssAvailability = availability
          , ssPackageValidUntil = package
          }
      }
    where
      roomLabel rid = T.pack ("Sala " <> show rid)
      genWindow lo hi = do
        start <- choose (lo, hi)
        len <- elements [120, 300, 600, 1440, 4000]
        pure (span' start (start + len))

-- | Reference: half-open overlap in whole minutes.
refOverlap :: Interval -> Interval -> Bool
refOverlap a b = max (minuteOf (ivStart a)) (minuteOf (ivStart b))
  < min (minuteOf (ivEnd a)) (minuteOf (ivEnd b))

-- | Reference: is every minute of the slot covered by some window of the room?
refCovered :: [Interval] -> Interval -> Bool
refCovered windows slot =
  all (\m -> any (\w -> minuteOf (ivStart w) <= m && m < minuteOf (ivEnd w)) windows)
    [minuteOf (ivStart slot) .. minuteOf (ivEnd slot) - 1]

-- | Reference feasibility, restating the contract's hard constraints.
refFeasible :: Scenario -> Integer -> RoomOption -> Bool
refFeasible (Scenario req snap) start room =
  start `mod` 30 == 0
    && start >= lower
    && end <= upper
    && localMinute >= 480
    && localMinute + duration <= 1320
    && ssTeacherTeachesSubject snap
    && room `elem` ssRooms snap
    && not (any (refOverlap slot) (ssTeacherBusy snap))
    && not (any (refOverlap slot) (ssStudentBusy snap))
    && not (any (refOverlap slot) (Map.findWithDefault [] (roId room) (ssResourceBusy snap)))
    && refCovered [w | (rid, w) <- ssAvailability snap, rid == roId room] slot
    && maybe False (\expiry -> atMinute end <= expiry) (ssPackageValidUntil snap)
  where
    duration = fromIntegral (srDurationMinutes req)
    end = start + duration
    slot = span' start end
    nowMinute = minuteOf (srNow req)
    horizonLo = max nowMinute (minuteOf (srFrom req))
    lower = max horizonLo (nowMinute + 720)
    upper = horizonLo + fromIntegral (srDays req) * 1440
    localMinute = (start - 300) `mod` 1440

-- | Every feasible (start, room) by brute force over the whole horizon.
refFeasibleAll :: Scenario -> [(Integer, RoomOption)]
refFeasibleAll scenario@(Scenario req snap) =
  [ (start, room)
  | start <- [lo, lo + 30 .. hi]
  , room <- ssRooms snap
  , refFeasible scenario start room
  ]
  where
    nowMinute = minuteOf (srNow req)
    lo = (nowMinute `div` 30) * 30
    hi = nowMinute + 2000 + fromIntegral (srDays req) * 1440 + 1440

candidateOf :: Scenario -> (Integer, RoomOption) -> SlotCandidate
candidateOf (Scenario req _) (start, room) =
  SlotCandidate (span' start (start + fromIntegral (srDurationMinutes req))) room

suggestionKey :: SlotSuggestion -> (UTCTime, Int64)
suggestionKey s = (sgStart s, sgRoomId s)

spec :: Spec
spec = describe "TDF.Trials.SlotSuggestions" $ do
  describe "intervals" $ do
    it "treats intervals as half-open: touching intervals do not overlap" $ do
      overlaps (span' 0 60) (span' 60 120) `shouldBe` False
      overlaps (span' 0 61) (span' 60 120) `shouldBe` True
      overlaps (span' 60 120) (span' 0 60) `shouldBe` False
    it "agrees with the minute reference" $ property $ \(a, b, c, d) ->
      let x = span' (min a b) (max a b + 1)
          y = span' (min c d) (max c d + 1)
      in overlaps x y === refOverlap x y
    it "merges touching and overlapping windows into disjoint ones" $ do
      mergeIntervals [span' 60 120, span' 0 60, span' 200 300, span' 250 260]
        `shouldBe` [span' 0 120, span' 200 300]
      mergeIntervals [span' 10 10, span' 20 5] `shouldBe` []

  describe "mkSlotRequest" $ do
    let now = atMinute baseMinute
    it "applies the approved defaults" $
      mkSlotRequest now Nothing Nothing Nothing Nothing [] []
        `shouldBe` Right (SlotRequest now now 14 60 5 [])
    it "refuses out-of-range values instead of clamping" $ do
      mkSlotRequest now Nothing (Just 29) Nothing Nothing [] [] `shouldSatisfy` isLeft
      mkSlotRequest now Nothing (Just 0) Nothing Nothing [] [] `shouldSatisfy` isLeft
      mkSlotRequest now Nothing Nothing (Just 45) Nothing [] [] `shouldSatisfy` isLeft
      mkSlotRequest now Nothing Nothing (Just 210) Nothing [] [] `shouldSatisfy` isLeft
      mkSlotRequest now Nothing Nothing Nothing (Just 21) [] [] `shouldSatisfy` isLeft
      mkSlotRequest now Nothing Nothing Nothing (Just 0) [] [] `shouldSatisfy` isLeft
    it "refuses unpaired, inverted or too many preferred windows" $ do
      mkSlotRequest now Nothing Nothing Nothing Nothing [now] [] `shouldSatisfy` isLeft
      mkSlotRequest now Nothing Nothing Nothing Nothing [now] [now] `shouldSatisfy` isLeft
      let starts = replicate 6 now
          ends = replicate 6 (addUTCTime 3600 now)
      mkSlotRequest now Nothing Nothing Nothing Nothing starts ends `shouldSatisfy` isLeft

  describe "a worked calendar" $ do
    -- Request at 2026-12-01 00:00Z (19:00 studio time on 30 Nov), one day ahead.
    -- Notice ends 12:00Z = 07:00 studio; the studio opens 13:00Z and the last
    -- 60-minute lesson inside the horizon starts 23:00Z.
    let now = atMinute baseMinute
        req = SlotRequest now now 1 60 3 []
        room = RoomOption 7 "Sala A" 1
        snap = SlotSnapshot
          { ssTeacherTeachesSubject = True
          , ssRooms = [room]
          , ssTeacherBusy = [span' (baseMinute + 900) (baseMinute + 960)]
          , ssStudentBusy = []
          , ssResourceBusy = Map.empty
          , ssAvailability = [(7, span' baseMinute (baseMinute + 1440))]
          , ssPackageValidUntil = Just (atMinute (baseMinute + 50000))
          }
    it "prefers slots adjacent to the teacher's existing lesson, earliest first" $
      [ (minuteOf (sgStart s) - baseMinute, sgTeacherGapMinutes s) | s <- suggestSlots req snap ]
        `shouldBe` [(840, Just 0), (960, Just 0), (810, Just 30)]
    it "puts a slot inside a preferred window first and says so" $ do
      let preferred = req { srPreferred = [span' (baseMinute + 1200) (baseMinute + 1300)] }
          top = head (suggestSlots preferred snap)
      (minuteOf (sgStart top) - baseMinute, sgInsidePreferred top) `shouldBe` (1200, True)
    it "suggests nothing outside opening hours or before the notice" $
      all (\s -> let m = minuteOf (sgStart s) - baseMinute in m >= 780 && m + 60 <= 1440)
        (suggestSlots req { srLimit = 20 } snap) `shouldBe` True
    it "suggests nothing without a valid package, availability or subject link" $ do
      suggestSlots req snap { ssPackageValidUntil = Nothing } `shouldBe` []
      suggestSlots req snap { ssAvailability = [] } `shouldBe` []
      suggestSlots req snap { ssTeacherTeachesSubject = False } `shouldBe` []
      suggestSlots req snap { ssRooms = [] } `shouldBe` []

  describe "properties against the brute-force reference" $ do
    it "soundness: every suggestion satisfies every hard constraint" $
      property $ \scenario@(Scenario req snap) ->
        conjoin
          [ counterexample (show s) $
              refFeasible scenario (minuteOf (sgStart s)) room
                && slotFeasible req snap (SlotCandidate (Interval (sgStart s) (sgEnd s)) room)
          | s <- suggestSlots req snap
          , room <- [r | r <- ssRooms snap, roId r == sgRoomId s]
          ]
    it "soundness: a suggestion always names a room of the snapshot" $
      property $ \(Scenario req snap) ->
        all (\s -> sgRoomId s `elem` map roId (ssRooms snap)) (suggestSlots req snap)
    it "completeness: something is suggested exactly when a feasible slot exists" $
      property $ \scenario@(Scenario req snap) ->
        null (suggestSlots req snap) === null (refFeasibleAll scenario)
    it "completeness: the feasible set is the reference feasible set" $
      property $ \scenario@(Scenario req snap) ->
        sort [ (minuteOf (ivStart (scInterval c)), roId (scRoom c))
             | c <- rankedFeasibleSlots req snap ]
          === sort [(start, roId room) | (start, room) <- refFeasibleAll scenario]
    it "optimality: no feasible slot costs less than the first suggestion" $
      property $ \scenario@(Scenario req snap) ->
        case suggestSlots req snap of
          [] -> property True
          (top : _) ->
            let topRoom = head [r | r <- ssRooms snap, roId r == sgRoomId top]
                topCost = slotCost req snap (SlotCandidate (Interval (sgStart top) (sgEnd top)) topRoom)
            in conjoin
                 [ counterexample (show other) (topCost <= slotCost req snap (candidateOf scenario other))
                 | other <- refFeasibleAll scenario
                 ]
    it "order: suggestions are the cheapest distinct starts, at most the limit" $
      property $ \scenario@(Scenario req snap) ->
        let expected =
              take (srLimit req) . nubOn fst . map snd . sortOn fst $
                [ (slotCost req snap (candidateOf scenario pair), (atMinute (fst pair), roId (snd pair)))
                | pair <- refFeasibleAll scenario
                ]
        in map suggestionKey (suggestSlots req snap) === expected
    it "determinism: the order of the snapshot rows does not matter" $
      property $ \(Scenario req snap) ->
        let shuffled = snap
              { ssRooms = reverse (ssRooms snap)
              , ssTeacherBusy = reverse (ssTeacherBusy snap)
              , ssStudentBusy = reverse (ssStudentBusy snap)
              , ssResourceBusy = Map.map reverse (ssResourceBusy snap)
              , ssAvailability = reverse (ssAvailability snap)
              }
        in suggestSlots req shuffled === suggestSlots req snap
    it "fallback: preferred windows reorder but never change what is feasible" $
      property $ \(Scenario req snap) ->
        let plain = req { srPreferred = [] }
        in sort (map show (rankedFeasibleSlots req snap))
             === sort (map show (rankedFeasibleSlots plain snap))
    it "fallback: without preferred windows the order ignores them entirely" $
      property $ \scenario@(Scenario req snap) ->
        let plain = req { srPreferred = [] }
            withoutFirst (_, gap, priority, start, room) = (gap, priority, start, room)
            expected =
              take (srLimit req) . nubOn fst . map snd . sortOn fst $
                [ (withoutFirst (slotCost req snap (candidateOf scenario pair)), (atMinute (fst pair), roId (snd pair)))
                | pair <- refFeasibleAll scenario
                ]
            fallback = suggestSlots plain snap
        in map suggestionKey fallback === expected
             .&&. all (not . sgInsidePreferred) fallback
    it "advice: once a suggested slot is taken it is no longer feasible" $
      property $ \(Scenario req snap) ->
        conjoin
          [ suggestionKey s `notElem` map suggestionKey (suggestSlots req { srLimit = 20 } taken)
          | s <- suggestSlots req snap
          , let iv = Interval (sgStart s) (sgEnd s)
          , taken <-
              [ snap { ssTeacherBusy = iv : ssTeacherBusy snap }
              , snap { ssStudentBusy = iv : ssStudentBusy snap }
              , snap { ssResourceBusy = Map.insertWith (++) (sgRoomId s) [iv] (ssResourceBusy snap) }
              ]
          ]
    it "the gap is absent exactly when the teacher's studio day is otherwise empty" $
      property $ \(Scenario req snap) ->
        all
          (\s -> isNothing (sgTeacherGapMinutes s)
              == not (any (refOverlap (studioDayOf (sgStart s))) (ssTeacherBusy snap)))
          (suggestSlots req snap)

  describe "snapshot query" $ do
    let sqlWords = T.words (T.toUpper (T.map (\c -> if c `elem` ("(),;" :: String) then ' ' else c) slotSnapshotSql))
    it "is one read: no statement that writes or locks" $ do
      head sqlWords `shouldBe` "WITH"
      filter (`elem` ["INSERT", "UPDATE", "DELETE", "MERGE", "TRUNCATE", "LOCK", "CREATE", "ALTER", "DROP", "NEXTVAL", "SETVAL", "COPY", "CALL"])
        sqlWords `shouldBe` []
    it "takes exactly six parameters" $
      T.count "?" slotSnapshotSql `shouldBe` 6
    it "decodes the document the query builds" $
      decodeSlotSnapshot
        "{\"teacherTeachesSubject\":true,\"rooms\":[{\"id\":7,\"name\":\"Sala A\",\"priority\":2}],\
        \\"teacherBusy\":[[60000000,120000000]],\"studentBusy\":[],\
        \\"resourceBusy\":[[7,0,60000000],[7,120000000,180000000]],\
        \\"availability\":[[7,0,600000000]],\"packageValidUntil\":null}"
        `shouldBe` Right SlotSnapshot
          { ssTeacherTeachesSubject = True
          , ssRooms = [RoomOption 7 "Sala A" 2]
          , ssTeacherBusy = [span' 1 2]
          , ssStudentBusy = []
          , ssResourceBusy = Map.fromList [(7, [span' 2 3, span' 0 1])]
          , ssAvailability = [(7, span' 0 10)]
          , ssPackageValidUntil = Nothing
          }
    it "refuses a malformed document" $
      decodeSlotSnapshot "{\"rooms\":[]}" `shouldSatisfy` isLeft
  where
    nubOn f = foldr (\x acc -> x : filter ((/= f x) . f) acc) []
    studioDayOf t =
      let local = minuteOf t - 300
          dayStart = (local `div` 1440) * 1440 + 300
      in span' dayStart (dayStart + 1440)
