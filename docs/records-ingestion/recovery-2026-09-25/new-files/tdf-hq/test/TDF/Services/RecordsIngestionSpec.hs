{-# LANGUAGE OverloadedStrings #-}
module TDF.Services.RecordsIngestionSpec (spec) where
import Data.Time (UTCTime(..), fromGregorian, diffUTCTime, dayOfWeek, DayOfWeek(Sunday))
import Test.Hspec
import Test.QuickCheck
import TDF.Services.RecordsIngestion (recordsIngestionSlot, recordsReconciliationSlot)

spec :: Spec
spec = describe "Canonical video scheduling" $ do
  it "uses UTC hour slots independently of server-local time" $ do
    recordsIngestionSlot 3600 (UTCTime (fromGregorian 2026 9 21) 3599)
      `shouldBe` UTCTime (fromGregorian 2026 9 21) 0
  it "replays one latest weekly identity after missed executions" $ property $ \(NonNegative seconds) ->
    let now = UTCTime (fromGregorian 2026 9 21) (fromInteger (seconds `mod` 86400))
        slot = recordsReconciliationSlot now
    in dayOfWeek (utctDay slot)==Sunday && slot<=now && diffUTCTime now slot<7*86400
  it "normalizes any configured interval to a bounded idempotent slot" $ property $ \seconds (NonNegative offset) ->
    let now = UTCTime (fromGregorian 2026 9 21) (fromInteger (offset `mod` 86400))
        slot = recordsIngestionSlot seconds now
    in slot<=now && diffUTCTime now slot<86400 && recordsIngestionSlot seconds slot==slot
