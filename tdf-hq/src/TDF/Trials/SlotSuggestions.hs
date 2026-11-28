{-# LANGUAGE OverloadedStrings #-}

-- | Slot suggestions for one new lesson (SCHOOL-SLOT-SUGGESTION-001).
--
-- Given a teacher, a student, a subject and optionally a room, 'suggestSlots'
-- answers the best feasible slots in a bounded horizon. It is an exact search:
-- every candidate on the 30-minute grid is enumerated, the infeasible ones are
-- dropped and the rest are ordered by a lexicographic cost. Nothing here
-- writes; a suggestion is advice and the database decides at insertion.
--
-- The contract, the owner's decisions and the evidence for each property are in
-- formal/system/lesson-slot-suggestions.md.
module TDF.Trials.SlotSuggestions
  ( Interval (..)
  , RoomOption (..)
  , SlotRequest (..)
  , SlotSnapshot (..)
  , SlotCandidate (..)
  , SlotSuggestion (..)
  , SlotCost
  , slotGridMinutes
  , minSlotDurationMinutes
  , maxSlotDurationMinutes
  , defaultSlotDurationMinutes
  , defaultHorizonDays
  , maxHorizonDays
  , defaultSuggestionLimit
  , maxSuggestionLimit
  , maxPreferredWindows
  , minimumNoticeMinutes
  , openingMinuteOfDay
  , closingMinuteOfDay
  , studioUtcOffsetMinutes
  , emptyDayGapMinutes
  , mkSlotRequest
  , overlaps
  , contains
  , mergeIntervals
  , horizonStart
  , horizonEnd
  , snapshotWindow
  , candidateSlots
  , slotFeasible
  , slotCost
  , rankedFeasibleSlots
  , suggestSlots
  , slotSnapshotSql
  , slotSnapshotFromValue
  , decodeSlotSnapshot
  , loadSlotSnapshot
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON (parseJSON), Value, eitherDecodeStrict, withObject, (.:))
import           Data.Aeson.Types       (parseEither)
import           Data.Int               (Int64)
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Time              (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Database.Persist       (PersistValue (PersistNull), toPersistValue)
import           Database.Persist.Sql   (Single (..), SqlPersistT, rawSql)

-- | Half-open @[ivStart, ivEnd)@, the interval of the exclusion constraints.
data Interval = Interval
  { ivStart :: !UTCTime
  , ivEnd   :: !UTCTime
  } deriving (Eq, Ord, Show)

-- | A room a lesson of the subject may use. A lower priority number is preferred.
data RoomOption = RoomOption
  { roId       :: !Int64
  , roName     :: !Text
  , roPriority :: !Int
  } deriving (Eq, Show)

data SlotRequest = SlotRequest
  { srNow             :: !UTCTime
  , srFrom            :: !UTCTime
  , srDays            :: !Int
  , srDurationMinutes :: !Int
  , srLimit           :: !Int
  , srPreferred       :: ![Interval]
    -- ^ windows typed into this request; never stored or inferred
  } deriving (Eq, Show)

-- | What the database held at one instant, already restricted to the request.
data SlotSnapshot = SlotSnapshot
  { ssTeacherTeachesSubject :: !Bool
  , ssRooms                 :: ![RoomOption]
    -- ^ active rooms allowed for the subject (only the requested one if given)
  , ssTeacherBusy           :: ![Interval]
  , ssStudentBusy           :: ![Interval]
  , ssResourceBusy          :: !(Map.Map Int64 [Interval])
  , ssAvailability          :: ![(Int64, Interval)]
    -- ^ the teacher's availability windows for the subject, by room
  , ssPackageValidUntil     :: !(Maybe UTCTime)
    -- ^ latest expiry of the student's open packages for the subject
  } deriving (Eq, Show)

data SlotCandidate = SlotCandidate
  { scInterval :: !Interval
  , scRoom     :: !RoomOption
  } deriving (Eq, Show)

data SlotSuggestion = SlotSuggestion
  { sgStart             :: !UTCTime
  , sgEnd               :: !UTCTime
  , sgRoomId            :: !Int64
  , sgRoomName          :: !Text
  , sgInsidePreferred   :: !Bool
    -- ^ the request named preferred windows and this slot lies inside one
  , sgTeacherGapMinutes :: !(Maybe Int)
    -- ^ minutes to the teacher's nearest commitment that day; 'Nothing' when
    -- the teacher has no other commitment that day
  } deriving (Eq, Show)

-- | Smaller is better, compared left to right: outside the preferred windows,
-- idle minutes added to the teacher's day, room priority, start, room id.
type SlotCost = (Int, Int, Int, UTCTime, Int64)

slotGridMinutes, minSlotDurationMinutes, maxSlotDurationMinutes :: Int
slotGridMinutes = 30
minSlotDurationMinutes = 30
maxSlotDurationMinutes = 180

defaultSlotDurationMinutes, defaultHorizonDays, maxHorizonDays :: Int
defaultSlotDurationMinutes = 60
defaultHorizonDays = 14
maxHorizonDays = 28

defaultSuggestionLimit, maxSuggestionLimit, maxPreferredWindows :: Int
defaultSuggestionLimit = 5
maxSuggestionLimit = 20
maxPreferredWindows = 5

-- | A lesson is never suggested sooner than this after the request.
minimumNoticeMinutes :: Int
minimumNoticeMinutes = 720

-- | Lessons lie inside 08:00-22:00 studio time on one day.
openingMinuteOfDay, closingMinuteOfDay :: Int
openingMinuteOfDay = 480
closingMinuteOfDay = 1320

-- | America/Guayaquil is UTC-05:00 all year (no daylight saving).
studioUtcOffsetMinutes :: Int
studioUtcOffsetMinutes = -300

-- | Cost of a day on which the teacher has no other commitment: the length of
-- the opening day, larger than any gap inside one.
emptyDayGapMinutes :: Int
emptyDayGapMinutes = closingMinuteOfDay - openingMinuteOfDay

-- | Validate the query. Out-of-range values are refused, never clamped.
mkSlotRequest
  :: UTCTime
  -> Maybe UTCTime
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> [UTCTime]
  -> [UTCTime]
  -> Either Text SlotRequest
mkSlotRequest now mFrom mDays mDuration mLimit preferredStarts preferredEnds = do
  let duration = fromMaybe defaultSlotDurationMinutes mDuration
      days = fromMaybe defaultHorizonDays mDays
      limit = fromMaybe defaultSuggestionLimit mLimit
  check
    (duration >= minSlotDurationMinutes
      && duration <= maxSlotDurationMinutes
      && duration `mod` slotGridMinutes == 0)
    "durationMinutes must be 30 to 180 in steps of 30"
  check (days >= 1 && days <= maxHorizonDays) "days must be 1 to 28"
  check (limit >= 1 && limit <= maxSuggestionLimit) "limit must be 1 to 20"
  check
    (length preferredStarts == length preferredEnds)
    "preferredStart and preferredEnd must be given in pairs"
  check
    (length preferredStarts <= maxPreferredWindows)
    "at most 5 preferred windows"
  let preferred = zipWith Interval preferredStarts preferredEnds
  check
    (all (\window -> ivEnd window > ivStart window) preferred)
    "each preferred window must end after it starts"
  pure SlotRequest
    { srNow = now
    , srFrom = fromMaybe now mFrom
    , srDays = days
    , srDurationMinutes = duration
    , srLimit = limit
    , srPreferred = preferred
    }
  where
    check ok message = if ok then Right () else Left message

-- | Two half-open intervals share an instant. This is PostgreSQL's
-- @tstzrange(a, b, '[)') && tstzrange(c, d, '[)')@ for non-empty ranges;
-- formal/system/lesson_slot_overlap.py proves the equivalence with Z3.
overlaps :: Interval -> Interval -> Bool
overlaps a b = ivStart a < ivEnd b && ivStart b < ivEnd a

-- | @contains outer inner@: the inner interval lies inside the outer one.
contains :: Interval -> Interval -> Bool
contains outer inner = ivStart outer <= ivStart inner && ivEnd inner <= ivEnd outer

-- | Union of intervals as disjoint, non-adjacent intervals in start order.
mergeIntervals :: [Interval] -> [Interval]
mergeIntervals = foldr step [] . sortOn ivStart . filter (\iv -> ivEnd iv > ivStart iv)
  where
    -- The accumulator is already disjoint and in start order, but an interval
    -- that absorbs its successor can reach the ones after it, so keep merging.
    step iv (next : rest)
      | ivEnd iv >= ivStart next = step (Interval (ivStart iv) (max (ivEnd iv) (ivEnd next))) rest
    step iv acc = iv : acc

minutes :: Int -> NominalDiffTime
minutes n = fromIntegral n * 60

-- | The horizon never starts in the past.
horizonStart :: SlotRequest -> UTCTime
horizonStart req = max (srFrom req) (srNow req)

horizonEnd :: SlotRequest -> UTCTime
horizonEnd req = addUTCTime (minutes (srDays req * 1440)) (horizonStart req)

earliestStart :: SlotRequest -> UTCTime
earliestStart req =
  max (horizonStart req) (addUTCTime (minutes minimumNoticeMinutes) (srNow req))

-- | The instants the snapshot must cover: the horizon and a day either side,
-- so every commitment on a candidate's studio day is read.
snapshotWindow :: SlotRequest -> (UTCTime, UTCTime)
snapshotWindow req =
  (addUTCTime (minutes (-1440)) (horizonStart req), addUTCTime (minutes 1440) (horizonEnd req))

-- | Whole seconds since the epoch, rounded down.
epochSeconds :: UTCTime -> Integer
epochSeconds = floor . utcTimeToPOSIXSeconds

fromEpochSeconds :: Integer -> UTCTime
fromEpochSeconds = posixSecondsToUTCTime . fromInteger

-- | Studio-local minute of the day of an instant on a whole minute.
studioMinuteOfDay :: UTCTime -> Integer
studioMinuteOfDay t =
  ((epochSeconds t `div` 60) + fromIntegral studioUtcOffsetMinutes) `mod` 1440

-- | The studio-local calendar day that contains an instant.
studioDay :: UTCTime -> Interval
studioDay t = Interval (fromEpochSeconds dayStart) (fromEpochSeconds (dayStart + 86400))
  where
    offsetSeconds = fromIntegral studioUtcOffsetMinutes * 60
    localSeconds = epochSeconds t + offsetSeconds
    dayStart = (localSeconds `div` 86400) * 86400 - offsetSeconds

-- | The request-only constraints on an interval: on the grid, the requested
-- length, after the notice, inside the horizon and inside opening hours.
slotAdmissible :: SlotRequest -> Interval -> Bool
slotAdmissible req iv =
  utcTimeToPOSIXSeconds start == fromInteger startSeconds
    && startSeconds `mod` (fromIntegral slotGridMinutes * 60) == 0
    && diffUTCTime end start == minutes (srDurationMinutes req)
    && start >= earliestStart req
    && end <= horizonEnd req
    && startMinute >= fromIntegral openingMinuteOfDay
    && startMinute + fromIntegral (srDurationMinutes req) <= fromIntegral closingMinuteOfDay
  where
    start = ivStart iv
    end = ivEnd iv
    startSeconds = epochSeconds start
    startMinute = studioMinuteOfDay start

-- | Every admissible interval paired with every room, in start then room order.
candidateSlots :: SlotRequest -> [RoomOption] -> [SlotCandidate]
candidateSlots req rooms =
  [ SlotCandidate iv room
  | startSeconds <- [firstGrid, firstGrid + step .. lastStart]
  , let start = fromEpochSeconds startSeconds
        iv = Interval start (addUTCTime (minutes (srDurationMinutes req)) start)
  , slotAdmissible req iv
  , room <- sortOn roId rooms
  ]
  where
    step = fromIntegral slotGridMinutes * 60 :: Integer
    firstGrid = negate ((negate (ceilingSeconds (earliestStart req))) `div` step) * step
    lastStart = epochSeconds (horizonEnd req) - fromIntegral (srDurationMinutes req) * 60
    ceilingSeconds = ceiling . utcTimeToPOSIXSeconds :: UTCTime -> Integer

-- | All hard constraints. A candidate that passes can be inserted on the
-- snapshot it was computed from without violating an exclusion constraint.
slotFeasible :: SlotRequest -> SlotSnapshot -> SlotCandidate -> Bool
slotFeasible req snapshot (SlotCandidate iv room) =
  slotAdmissible req iv
    && ssTeacherTeachesSubject snapshot
    && any ((== roId room) . roId) (ssRooms snapshot)
    && not (any (overlaps iv) (ssTeacherBusy snapshot))
    && not (any (overlaps iv) (ssStudentBusy snapshot))
    && not (any (overlaps iv) (Map.findWithDefault [] (roId room) (ssResourceBusy snapshot)))
    && any (`contains` iv) roomWindows
    && maybe False (ivEnd iv <=) (ssPackageValidUntil snapshot)
  where
    roomWindows =
      mergeIntervals [window | (roomId, window) <- ssAvailability snapshot, roomId == roId room]

-- | Minutes between a slot and the teacher's nearest commitment on the slot's
-- studio day, or 'Nothing' when that day holds no commitment.
teacherGapMinutes :: SlotSnapshot -> Interval -> Maybe Int
teacherGapMinutes snapshot iv =
  case [gap busy | busy <- ssTeacherBusy snapshot, overlaps busy (studioDay (ivStart iv))] of
    []   -> Nothing
    gaps -> Just (minimum gaps)
  where
    gap busy
      | ivEnd busy <= ivStart iv = wholeMinutes (diffUTCTime (ivStart iv) (ivEnd busy))
      | ivStart busy >= ivEnd iv = wholeMinutes (diffUTCTime (ivStart busy) (ivEnd iv))
      | otherwise = 0
    wholeMinutes :: NominalDiffTime -> Int
    wholeMinutes span' = floor (span' / 60)

insidePreferred :: SlotRequest -> Interval -> Bool
insidePreferred req iv = any (`contains` iv) (srPreferred req)

slotCost :: SlotRequest -> SlotSnapshot -> SlotCandidate -> SlotCost
slotCost req snapshot (SlotCandidate iv room) =
  ( if null (srPreferred req) || insidePreferred req iv then 0 else 1
  , fromMaybe emptyDayGapMinutes (teacherGapMinutes snapshot iv)
  , roPriority room
  , ivStart iv
  , roId room
  )

-- | Every feasible candidate, best first. The order is total.
rankedFeasibleSlots :: SlotRequest -> SlotSnapshot -> [SlotCandidate]
rankedFeasibleSlots req snapshot =
  sortOn (slotCost req snapshot)
    (filter (slotFeasible req snapshot) (candidateSlots req (ssRooms snapshot)))

-- | At most 'srLimit' suggestions, best first, one per start instant (the best
-- room for that start). Empty exactly when no candidate is feasible.
suggestSlots :: SlotRequest -> SlotSnapshot -> [SlotSuggestion]
suggestSlots req snapshot =
  take (srLimit req) (distinctStarts Set.empty (rankedFeasibleSlots req snapshot))
  where
    distinctStarts _ [] = []
    distinctStarts seen (candidate@(SlotCandidate iv _) : rest)
      | Set.member (ivStart iv) seen = distinctStarts seen rest
      | otherwise = toSuggestion candidate : distinctStarts (Set.insert (ivStart iv) seen) rest
    toSuggestion (SlotCandidate iv room) = SlotSuggestion
      { sgStart = ivStart iv
      , sgEnd = ivEnd iv
      , sgRoomId = roId room
      , sgRoomName = roName room
      , sgInsidePreferred = insidePreferred req iv
      , sgTeacherGapMinutes = teacherGapMinutes snapshot iv
      }

-- | The whole snapshot as one JSON document from one statement, so every part
-- is read at the same instant. Six parameters: teacher, student, subject, room
-- (or NULL), window start, window end. Instants are microseconds since the
-- epoch. scripts/test-lesson-slot-suggestions-sql.py runs this text on
-- PostgreSQL, inside a read-only transaction.
slotSnapshotSql :: Text
slotSnapshotSql =
  "WITH q AS (SELECT ?::bigint AS teacher_id, ?::bigint AS student_id, \
  \?::bigint AS subject_id, ?::bigint AS room_id, ?::timestamptz AS lo, ?::timestamptz AS hi) \
  \SELECT jsonb_build_object(\
  \'teacherTeachesSubject', EXISTS (SELECT 1 FROM teacher_subject ts \
  \WHERE ts.teacher_id = q.teacher_id AND ts.subject_id = q.subject_id), \
  \'rooms', COALESCE((SELECT jsonb_agg(jsonb_build_object(\
  \'id', r.id, 'name', r.name, 'priority', COALESCE(p.priority, 1)) ORDER BY r.id) \
  \FROM resource r LEFT JOIN subject_room_preference p \
  \ON p.room_id = r.id AND p.subject_id = q.subject_id \
  \WHERE r.active AND r.resource_type = 'Room' \
  \AND (q.room_id IS NULL OR r.id = q.room_id) \
  \AND (p.id IS NOT NULL OR NOT EXISTS (SELECT 1 FROM subject_room_preference other \
  \WHERE other.subject_id = q.subject_id))), '[]'::jsonb), \
  \'teacherBusy', COALESCE((SELECT jsonb_agg(jsonb_build_array(\
  \(extract(epoch FROM o.starts_at) * 1000000)::bigint, \
  \(extract(epoch FROM o.ends_at) * 1000000)::bigint)) \
  \FROM school_schedule_occupancy o WHERE o.subject_kind = 'teacher' \
  \AND o.subject_id = q.teacher_id AND o.ends_at > q.lo AND o.starts_at < q.hi), '[]'::jsonb), \
  \'studentBusy', COALESCE((SELECT jsonb_agg(jsonb_build_array(\
  \(extract(epoch FROM o.starts_at) * 1000000)::bigint, \
  \(extract(epoch FROM o.ends_at) * 1000000)::bigint)) \
  \FROM school_schedule_occupancy o WHERE o.subject_kind = 'student' \
  \AND o.subject_id = q.student_id AND o.ends_at > q.lo AND o.starts_at < q.hi), '[]'::jsonb), \
  \'resourceBusy', COALESCE((SELECT jsonb_agg(jsonb_build_array(busy.resource_id, \
  \(extract(epoch FROM busy.starts_at) * 1000000)::bigint, \
  \(extract(epoch FROM busy.ends_at) * 1000000)::bigint)) FROM (\
  \SELECT o.subject_id AS resource_id, o.starts_at, o.ends_at \
  \FROM school_schedule_occupancy o WHERE o.subject_kind = 'resource' \
  \AND o.ends_at > q.lo AND o.starts_at < q.hi \
  \UNION ALL SELECT br.resource_id, b.starts_at, b.ends_at \
  \FROM booking b JOIN booking_resource br ON br.booking_id = b.id \
  \WHERE b.status::text NOT IN ('Cancelled','NoShow') \
  \AND b.ends_at > q.lo AND b.starts_at < q.hi) busy), '[]'::jsonb), \
  \'availability', COALESCE((SELECT jsonb_agg(jsonb_build_array(a.room_id, \
  \(extract(epoch FROM a.start_at) * 1000000)::bigint, \
  \(extract(epoch FROM a.end_at) * 1000000)::bigint)) \
  \FROM teacher_availability a WHERE a.teacher_id = q.teacher_id \
  \AND a.subject_id = q.subject_id AND a.end_at > q.lo AND a.start_at < q.hi), '[]'::jsonb), \
  \'packageValidUntil', (SELECT (extract(epoch FROM max(pp.purchased_at \
  \+ make_interval(days => pc.expires_days::int))) * 1000000)::bigint \
  \FROM class_package_purchase pp JOIN package_catalog pc ON pc.id = pp.package_id \
  \WHERE pp.student_id = q.student_id AND pp.status = 'Open' \
  \AND pc.subject_id = q.subject_id))::text FROM q"

newtype SnapshotJson = SnapshotJson SlotSnapshot

instance FromJSON SnapshotJson where
  parseJSON = withObject "SlotSnapshot" $ \o -> do
    teaches <- o .: "teacherTeachesSubject"
    rooms <- o .: "rooms" >>= mapM parseRoom
    teacherBusy <- o .: "teacherBusy"
    studentBusy <- o .: "studentBusy"
    resourceBusy <- o .: "resourceBusy"
    availability <- o .: "availability"
    packageValidUntil <- o .: "packageValidUntil"
    pure $ SnapshotJson SlotSnapshot
      { ssTeacherTeachesSubject = teaches
      , ssRooms = rooms
      , ssTeacherBusy = [fromMicros s e | (s, e) <- teacherBusy]
      , ssStudentBusy = [fromMicros s e | (s, e) <- studentBusy]
      , ssResourceBusy =
          Map.fromListWith (++) [(resourceId, [fromMicros s e]) | (resourceId, s, e) <- resourceBusy]
      , ssAvailability = [(roomId, fromMicros s e) | (roomId, s, e) <- availability]
      , ssPackageValidUntil = microsToUtc <$> packageValidUntil
      }
    where
      parseRoom = withObject "RoomOption" $ \room ->
        RoomOption <$> room .: "id" <*> room .: "name" <*> room .: "priority"
      fromMicros :: Int64 -> Int64 -> Interval
      fromMicros s e = Interval (microsToUtc s) (microsToUtc e)

microsToUtc :: Int64 -> UTCTime
microsToUtc micros = posixSecondsToUTCTime (fromIntegral micros / 1000000)

-- | The snapshot from an already parsed document of the shape 'slotSnapshotSql'
-- builds.
slotSnapshotFromValue :: Value -> Either Text SlotSnapshot
slotSnapshotFromValue value =
  case parseEither parseJSON value of
    Left err                      -> Left (T.pack err)
    Right (SnapshotJson snapshot) -> Right snapshot

decodeSlotSnapshot :: Text -> Either Text SlotSnapshot
decodeSlotSnapshot raw =
  case eitherDecodeStrict (TE.encodeUtf8 raw) of
    Left err                      -> Left (T.pack err)
    Right (SnapshotJson snapshot) -> Right snapshot

-- | Read the snapshot for one request with a single SELECT in the caller's
-- transaction. Reads only.
loadSlotSnapshot
  :: MonadIO m
  => Int64
  -> Int64
  -> Int64
  -> Maybe Int64
  -> SlotRequest
  -> SqlPersistT m (Either Text SlotSnapshot)
loadSlotSnapshot teacherId studentId subjectId mRoomId req = do
  rows <- rawSql slotSnapshotSql
    [ toPersistValue teacherId
    , toPersistValue studentId
    , toPersistValue subjectId
    , maybe PersistNull toPersistValue mRoomId
    , toPersistValue windowStart
    , toPersistValue windowEnd
    ]
  pure $ case rows of
    [Single raw] -> decodeSlotSnapshot raw
    _            -> Left "slot snapshot returned no document"
  where
    (windowStart, windowEnd) = snapshotWindow req
