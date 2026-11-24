{-# LANGUAGE OverloadedStrings #-}

-- | MEDIA-MUSIC-LOUDNESS-001 (formal/system/music-loudness-check.md).
--
-- Pure core of the loudness check of delivered masters: the measurement
-- lifecycle, the pass/warn decision and the rule that binds a result to one
-- file. Every function here is total. Loudness and true peak are integers in
-- tenths (@-142@ is -14.2 LUFS), the precision the measuring tool prints, so no
-- comparison involves floating point.
module TDF.MusicRelease.Loudness
  ( Tenths
  , LoudnessLimits(..)
  , defaultLoudnessLimits
  , LoudnessFlag(..)
  , loudnessFlagText
  , loudnessFlags
  , MeasurementState(..)
  , measurementStateText
  , parseMeasurementState
  , MeasurementEvent(..)
  , stepMeasurement
  , loudnessFailureReasons
  , MasterFile(..)
  , StoredMeasurement(..)
  , CheckStatus(..)
  , checkStatusText
  , TrackCheck(..)
  , trackCheck
  , trackCheckJSON
  , loudnessLimitsJSON
  , loudnessTracksSql
  ) where

import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)

type Tenths = Int

data LoudnessLimits = LoudnessLimits
  { limitTargetLufs :: Tenths
  , limitTolerance :: Tenths
  , limitTruePeakCeiling :: Tenths
  } deriving (Eq, Show)

-- | Owner decisions of 2026-11-21: -14.0 LUFS, -1.0 dBTP; the +/- 2.0 LU band
-- was proposed by the implementer and continued without change.
defaultLoudnessLimits :: LoudnessLimits
defaultLoudnessLimits = LoudnessLimits
  { limitTargetLufs = -140
  , limitTolerance = 20
  , limitTruePeakCeiling = -10
  }

data LoudnessFlag = TooLoud | TooQuiet | TruePeakOver
  deriving (Eq, Ord, Show, Enum, Bounded)

loudnessFlagText :: LoudnessFlag -> Text
loudnessFlagText flag = case flag of
  TooLoud -> "too_loud"
  TooQuiet -> "too_quiet"
  TruePeakOver -> "true_peak_over"

-- | Flags raised by one measurement, in constructor order. Each flag depends on
-- one value through one strict comparison, which is what makes it monotone.
loudnessFlags :: LoudnessLimits -> Tenths -> Tenths -> [LoudnessFlag]
loudnessFlags limits integrated truePeak =
  [ TooLoud | integrated > limitTargetLufs limits + limitTolerance limits ]
  <> [ TooQuiet | integrated < limitTargetLufs limits - limitTolerance limits ]
  <> [ TruePeakOver | truePeak > limitTruePeakCeiling limits ]

data MeasurementState = Pending | Measured | Failed | Superseded
  deriving (Eq, Ord, Show, Enum, Bounded)

measurementStateText :: MeasurementState -> Text
measurementStateText state = case state of
  Pending -> "pending"
  Measured -> "measured"
  Failed -> "failed"
  Superseded -> "superseded"

parseMeasurementState :: Text -> Maybe MeasurementState
parseMeasurementState raw = lookup raw
  [ (measurementStateText state, state) | state <- [minBound .. maxBound] ]

data MeasurementEvent = RecordMeasured | RecordFailed | Supersede
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The whole lifecycle. 'Nothing' is a refused transition; the database
-- trigger of the migration refuses the same ones.
stepMeasurement :: MeasurementState -> MeasurementEvent -> Maybe MeasurementState
stepMeasurement state event = case (state, event) of
  (Pending, RecordMeasured) -> Just Measured
  (Pending, RecordFailed) -> Just Failed
  (Superseded, _) -> Nothing
  (_, Supersede) -> Just Superseded
  _ -> Nothing

-- | Reasons the wrapper or the worker may record for a failed measurement.
loudnessFailureReasons :: [Text]
loudnessFailureReasons =
  [ "timeout", "decode_error", "no_summary", "unparseable", "out_of_range"
  , "silent", "tool_unavailable", "interrupted" ]

-- | The newest master file of a track in a version, as stored.
data MasterFile = MasterFile
  { masterAssetId :: Text
  , masterSha256 :: Text
  , masterUsable :: Bool
    -- ^ inspected, promoted and immutable
  } deriving (Eq, Show)

-- | The measurement row that is not superseded for some asset.
data StoredMeasurement = StoredMeasurement
  { storedAssetId :: Text
  , storedSourceSha256 :: Text
  , storedState :: MeasurementState
  , storedIntegrated :: Maybe Tenths
  , storedTruePeak :: Maybe Tenths
  , storedFailureReason :: Maybe Text
  } deriving (Eq, Show)

data CheckStatus = CheckPass | CheckWarn | CheckFailed | CheckPending | CheckNotMeasured
  deriving (Eq, Ord, Show, Enum, Bounded)

checkStatusText :: CheckStatus -> Text
checkStatusText status = case status of
  CheckPass -> "pass"
  CheckWarn -> "warn"
  CheckFailed -> "failed"
  CheckPending -> "pending"
  CheckNotMeasured -> "not_measured"

data TrackCheck = TrackCheck
  { checkStatus :: CheckStatus
  , checkChecked :: Bool
  , checkFlags :: [LoudnessFlag]
  , checkIntegrated :: Maybe Tenths
  , checkTruePeak :: Maybe Tenths
  , checkFailureReason :: Maybe Text
  } deriving (Eq, Show)

notMeasured :: TrackCheck
notMeasured = TrackCheck CheckNotMeasured False [] Nothing Nothing Nothing

-- | Invariant L1. A track is checked only from a measured row of the very file
-- that is its newest usable master: same asset and same SHA-256. A row of any
-- other file, a superseded row or a row without both values says nothing about
-- the track, whatever it contains.
trackCheck :: LoudnessLimits -> Maybe MasterFile -> Maybe StoredMeasurement -> TrackCheck
trackCheck limits master measurement = case (master, measurement) of
  (Just file, Just row)
    | storedAssetId row == masterAssetId file
    , storedSourceSha256 row == masterSha256 file -> case storedState row of
        Measured
          | masterUsable file
          , Just integrated <- storedIntegrated row
          , Just truePeak <- storedTruePeak row ->
              let flags = loudnessFlags limits integrated truePeak
              in TrackCheck (if null flags then CheckPass else CheckWarn) True flags
                   (Just integrated) (Just truePeak) Nothing
          | otherwise -> notMeasured
        Failed -> notMeasured
          { checkStatus = CheckFailed, checkFailureReason = storedFailureReason row }
        Pending -> notMeasured { checkStatus = CheckPending }
        Superseded -> notMeasured
  _ -> notMeasured

loudnessLimitsJSON :: LoudnessLimits -> Value
loudnessLimitsJSON limits = object
  [ "integratedLufsTenths" .= limitTargetLufs limits
  , "toleranceTenths" .= limitTolerance limits
  , "truePeakCeilingDbtpTenths" .= limitTruePeakCeiling limits
  ]

trackCheckJSON :: Text -> Maybe Text -> TrackCheck -> Value
trackCheckJSON recordingId assetId check = object
  [ "recordingId" .= recordingId
  , "masterAssetId" .= assetId
  , "status" .= checkStatusText (checkStatus check)
  , "checked" .= checkChecked check
  , "flags" .= map loudnessFlagText (checkFlags check)
  , "integratedLufsTenths" .= checkIntegrated check
  , "truePeakDbtpTenths" .= checkTruePeak check
  , "failureReason" .= checkFailureReason check
  ]

-- | One row per track of the version, in track order: the newest master file
-- of the recording that was not deleted, and the measurement of that asset that
-- is not superseded. Both SHA-256 values are returned so 'trackCheck' decides
-- the binding; the query itself never marks anything as checked.
loudnessTracksSql :: Text
loudnessTracksSql =
  "SELECT track.recording_id::text, master.id::text, master.sha256, \
  \  COALESCE(master.immutable AND master.processing_state IN ('valid','ready'), FALSE), \
  \  measurement.asset_id::text, measurement.source_sha256, measurement.state, \
  \  measurement.integrated_lufs_tenths, measurement.true_peak_dbtp_tenths, \
  \  measurement.failure_reason \
  \FROM music_release_track track \
  \LEFT JOIN LATERAL ( \
  \  SELECT asset.id, asset.sha256, asset.immutable, asset.processing_state \
  \  FROM music_asset asset \
  \  WHERE asset.release_version_id = track.release_version_id \
  \    AND asset.recording_id = track.recording_id \
  \    AND asset.asset_role = 'master_audio' \
  \    AND asset.processing_state <> 'deleted' \
  \  ORDER BY asset.created_at DESC, asset.id DESC LIMIT 1) master ON TRUE \
  \LEFT JOIN music_audio_measurement measurement \
  \  ON measurement.asset_id = master.id \
  \ AND measurement.kind = 'loudness' \
  \ AND measurement.state <> 'superseded' \
  \WHERE track.release_version_id = ?::uuid \
  \ORDER BY track.disc_number, track.track_number, track.id"
