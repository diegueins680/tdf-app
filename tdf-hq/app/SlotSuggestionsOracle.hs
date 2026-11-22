{-# LANGUAGE OverloadedStrings #-}

-- | Verification oracle for SCHOOL-SLOT-SUGGESTION-001. Not a service and not
-- shipped in the runtime image: it lets the PostgreSQL and Z3 checks ask the
-- compiled 'TDF.Trials.SlotSuggestions' what it decides, instead of restating it.
--
-- Reads one JSON object per line on stdin:
--
-- > {"now":µs,"from":µs|null,"days":n,"durationMinutes":n,"limit":n,
-- >  "preferred":[[µs,µs]],"snapshot":{...as slotSnapshotSql builds...}}
--
-- and writes one JSON object per line: the suggestions, every candidate and
-- every feasible candidate with its cost, or @{"error":...}@.
module Main (main) where

import           Data.Aeson                 (Value, eitherDecodeStrict, encode, object, (.=))
import           Data.Aeson.Types           (Parser, parseEither, withObject, (.:), (.:?))
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           System.IO                  (BufferMode (LineBuffering), hSetBuffering, stdout)

import           TDF.Trials.SlotSuggestions

fromMicros :: Int64 -> UTCTime
fromMicros micros = posixSecondsToUTCTime (fromIntegral micros / 1000000)

toMicros :: UTCTime -> Int64
toMicros t = round (utcTimeToPOSIXSeconds t * 1000000)

parseInstance :: Value -> Parser (Either Text (SlotRequest, SlotSnapshot))
parseInstance = withObject "instance" $ \o -> do
  now <- fromMicros <$> o .: "now"
  mFrom <- fmap fromMicros <$> o .:? "from"
  days <- o .:? "days"
  duration <- o .:? "durationMinutes"
  limit <- o .:? "limit"
  preferred <- o .: "preferred" :: Parser [(Int64, Int64)]
  snapshotValue <- o .: "snapshot"
  pure $ do
    request <- mkSlotRequest now mFrom days duration limit
      (map (fromMicros . fst) preferred) (map (fromMicros . snd) preferred)
    snapshot <- slotSnapshotFromValue snapshotValue
    Right (request, snapshot)

answer :: BS8.ByteString -> Value
answer line =
  case eitherDecodeStrict line >>= parseEither parseInstance of
    Left err -> object ["error" .= err]
    Right (Left err) -> object ["error" .= err]
    Right (Right (request, snapshot)) -> object
      [ "suggestions" .=
          [ object
              [ "start" .= toMicros (sgStart s)
              , "end" .= toMicros (sgEnd s)
              , "roomId" .= sgRoomId s
              , "insidePreferred" .= sgInsidePreferred s
              , "teacherGapMinutes" .= sgTeacherGapMinutes s
              ]
          | s <- suggestSlots request snapshot
          ]
      , "candidates" .=
          [ (toMicros (ivStart (scInterval c)), roId (scRoom c))
          | c <- candidateSlots request (ssRooms snapshot)
          ]
      , "feasible" .=
          [ object
              [ "start" .= toMicros (ivStart (scInterval c))
              , "roomId" .= roId (scRoom c)
              , "cost" .= costList (slotCost request snapshot c)
              ]
          | c <- rankedFeasibleSlots request snapshot
          ]
      ]
  where
    costList :: SlotCost -> [Int64]
    costList (preferred, gap, priority, start, room) =
      [fromIntegral preferred, fromIntegral gap, fromIntegral priority, toMicros start, room]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  input <- BS8.getContents
  mapM_ (BL8.putStrLn . encode . answer) (filter (not . BS8.null) (BS8.lines input))
