{-# LANGUAGE OverloadedStrings #-}

-- MEDIA-MUSIC-LOUDNESS-001. Properties of the pure core; these are tests, not
-- proofs. The machine-checked counterparts are formal/music/.
module TDF.MusicRelease.LoudnessSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import TDF.MusicRelease.Loudness

genTenths :: Gen Tenths
genTenths = frequency
  [ (3, choose (-200, -80))
  , (2, choose (-30, 10))
  , (1, choose (-1600, 300))
  , (1, elements [-161, -160, -159, -121, -120, -119, -11, -10, -9])
  ]

genLimits :: Gen LoudnessLimits
genLimits = LoudnessLimits <$> choose (-400, 0) <*> choose (0, 100) <*> choose (-100, 50)

genState :: Gen MeasurementState
genState = elements [minBound .. maxBound]

genSha :: Gen Text
genSha = elements ["sha-a", "sha-b", "sha-c"]

genAsset :: Gen Text
genAsset = elements ["asset-1", "asset-2"]

genMaster :: Gen MasterFile
genMaster = MasterFile <$> genAsset <*> genSha <*> arbitrary

genStored :: Gen StoredMeasurement
genStored = StoredMeasurement <$> genAsset <*> genSha <*> genState
  <*> liftArbitrary genTenths <*> liftArbitrary genTenths
  <*> liftArbitrary (elements loudnessFailureReasons)

-- A master and a row that half of the time is a complete measurement of that
-- very file, so the checked case is common rather than a rare coincidence.
genPair :: Gen (Maybe MasterFile, Maybe StoredMeasurement)
genPair = oneof
  [ (,) <$> liftArbitrary genMaster <*> liftArbitrary genStored
  , do
      file <- genMaster
      usable <- frequency [(4, pure True), (1, pure False)]
      integrated <- genTenths
      peak <- genTenths
      pure ( Just file { masterUsable = usable }
           , Just (StoredMeasurement (masterAssetId file) (masterSha256 file) Measured
               (Just integrated) (Just peak) Nothing) )
  ]

spec :: Spec
spec = describe "music loudness check" $ do
  describe "decision" $ do
    it "passes inside the approved band and warns just outside it" $ do
      let flags = loudnessFlags defaultLoudnessLimits
      flags (-140) (-10) `shouldBe` []
      flags (-160) (-10) `shouldBe` []
      flags (-120) (-10) `shouldBe` []
      flags (-161) (-10) `shouldBe` [TooQuiet]
      flags (-119) (-10) `shouldBe` [TooLoud]
      flags (-140) (-9) `shouldBe` [TruePeakOver]
      flags (-80) 3 `shouldBe` [TooLoud, TruePeakOver]

    prop "too_loud never clears as loudness rises" $
      forAll ((,,,) <$> genLimits <*> genTenths <*> genTenths <*> genTenths) $
        \(limits, low, high, peak) -> low <= high ==>
          (TooLoud `elem` loudnessFlags limits low peak)
            <= (TooLoud `elem` loudnessFlags limits high peak)

    prop "too_quiet never clears as loudness falls" $
      forAll ((,,,) <$> genLimits <*> genTenths <*> genTenths <*> genTenths) $
        \(limits, low, high, peak) -> low <= high ==>
          (TooQuiet `elem` loudnessFlags limits high peak)
            <= (TooQuiet `elem` loudnessFlags limits low peak)

    prop "true_peak_over never clears as the peak rises, so pass/warn is monotone in the peak" $
      forAll ((,,,) <$> genLimits <*> genTenths <*> genTenths <*> genTenths) $
        \(limits, integrated, low, high) -> low <= high ==>
          (TruePeakOver `elem` loudnessFlags limits integrated low)
            <= (TruePeakOver `elem` loudnessFlags limits integrated high)
          .&&. null (loudnessFlags limits integrated high)
            <= null (loudnessFlags limits integrated low)

    prop "pass/warn is monotone in loudness on each side of the target" $
      forAll ((,,,) <$> genLimits <*> genTenths <*> genTenths <*> genTenths) $
        \(limits, first, second, peak) ->
          let target = limitTargetLufs limits
              warns value = not (null (loudnessFlags limits value peak))
              (near, far) = if abs (first - target) <= abs (second - target)
                then (first, second) else (second, first)
          in (first >= target) == (second >= target) ==> warns near <= warns far

    prop "loudness flags and the peak flag do not influence each other" $
      forAll ((,,,) <$> genLimits <*> genTenths <*> genTenths <*> genTenths) $
        \(limits, integrated, peak, other) ->
          filter (/= TruePeakOver) (loudnessFlags limits integrated peak)
            == filter (/= TruePeakOver) (loudnessFlags limits integrated other)

    prop "a non-negative tolerance never raises both loudness flags" $
      forAll ((,,) <$> genLimits <*> genTenths <*> genTenths) $ \(limits, integrated, peak) ->
        let flags = loudnessFlags limits integrated peak
        in not (TooLoud `elem` flags && TooQuiet `elem` flags)

  describe "lifecycle" $ do
    it "allows exactly the declared transitions" $
      [ (state, event, next)
      | state <- [minBound .. maxBound], event <- [minBound .. maxBound]
      , Just next <- [stepMeasurement state event] ]
        `shouldBe`
          [ (Pending, RecordMeasured, Measured)
          , (Pending, RecordFailed, Failed)
          , (Pending, Supersede, Superseded)
          , (Measured, Supersede, Superseded)
          , (Failed, Supersede, Superseded)
          ]

    prop "superseded is terminal and a result is never rewritten" $
      forAll (listOf (elements [minBound .. maxBound])) $ \events ->
        let run state [] = [state]
            run state (event : rest) = case stepMeasurement state event of
              Just next -> state : run next rest
              Nothing -> state : run state rest
            trace = run Pending events
            afterFirst reached = dropWhile (/= reached) trace
        in all (== Superseded) (afterFirst Superseded)
             .&&. all (`elem` [Measured, Superseded]) (afterFirst Measured)
             .&&. all (`elem` [Failed, Superseded]) (afterFirst Failed)

    it "round-trips every state name" $
      map (parseMeasurementState . measurementStateText) [minBound .. maxBound]
        `shouldBe` map Just [minBound .. maxBound :: MeasurementState]

  describe "binding to one file (L1)" $ do
    prop "checked exactly when the row is a complete measurement of the same usable asset and SHA-256" $
      forAll ((,) <$> genLimits <*> genPair) $ \(limits, (master, stored)) ->
        let check = trackCheck limits master stored
            bound = case (master, stored) of
              (Just file, Just row) ->
                storedAssetId row == masterAssetId file
                  && storedSourceSha256 row == masterSha256 file
                  && storedState row == Measured && masterUsable file
                  && isJust (storedIntegrated row) && isJust (storedTruePeak row)
              _ -> False
        in checkCoverage $ cover 20 (checkChecked check) "checked" $
             cover 20 (not (checkChecked check)) "not checked" $
               checkChecked check === bound

    prop "a row of another file or asset reveals nothing about the track" $
      forAll ((,,) <$> genLimits <*> genMaster <*> genStored) $ \(limits, file, row) ->
        (storedSourceSha256 row /= masterSha256 file || storedAssetId row /= masterAssetId file)
          ==> trackCheck limits (Just file) (Just row)
                === TrackCheck CheckNotMeasured False [] Nothing Nothing Nothing

    prop "pass and warn are reported only for checked tracks, with both values" $
      forAll ((,) <$> genLimits <*> genPair) $ \(limits, (master, stored)) ->
          let check = trackCheck limits master stored
              decided = checkStatus check `elem` [CheckPass, CheckWarn]
          in decided === checkChecked check
               .&&. (isJust (checkIntegrated check) && isJust (checkTruePeak check)) === decided
               .&&. (checkStatus check == CheckWarn) === not (null (checkFlags check))
               .&&. (isNothing (checkFailureReason check) || checkStatus check == CheckFailed)

    prop "the verdict of a checked track is the decision on its stored values" $
      forAll ((,,,) <$> genLimits <*> genMaster <*> genTenths <*> genTenths) $
        \(limits, file, integrated, peak) ->
          let row = StoredMeasurement (masterAssetId file) (masterSha256 file) Measured
                      (Just integrated) (Just peak) Nothing
              check = trackCheck limits (Just file { masterUsable = True }) (Just row)
          in checkChecked check .&&. checkFlags check === loudnessFlags limits integrated peak

    it "never reports a track without a master as anything but not measured" $
      forAll (liftArbitrary genStored) $ \stored ->
        checkStatus (trackCheck defaultLoudnessLimits Nothing stored) === CheckNotMeasured

  describe "query" $
    it "selects the newest master and leaves the binding to the pure rule" $ do
      loudnessTracksSql `shouldSatisfy` T.isInfixOf "ORDER BY asset.created_at DESC, asset.id DESC LIMIT 1"
      loudnessTracksSql `shouldSatisfy` T.isInfixOf "measurement.state <> 'superseded'"
      loudnessTracksSql `shouldSatisfy` T.isInfixOf "master.sha256"
      loudnessTracksSql `shouldSatisfy` T.isInfixOf "measurement.source_sha256"
