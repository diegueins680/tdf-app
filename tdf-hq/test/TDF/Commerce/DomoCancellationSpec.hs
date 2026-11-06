{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PAY-DOMO-CANCEL-001 (AUTHORITY-093): refund policy properties.
module TDF.Commerce.DomoCancellationSpec (spec) where

import           Data.Either (isLeft, isRight)
import           Data.Int (Int64)
import           Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Commerce.DomoCancellation

genMinor :: Gen Int64
genMinor = frequency [(8, fromIntegral <$> chooseInt (0, 5000000)), (1, choose (0, maxBound `div` 4))]

policy :: DomoCancellationPolicy
policy = defaultDomoCancellationPolicy

spec :: Spec
spec = describe "Domo cancellation refunds (PAY-DOMO-CANCEL-001)" $ do
  it "the default policy is valid and encodes AUTHORITY-093" $ do
    validateDomoCancellationPolicy policy `shouldSatisfy` isRight
    depositRetainedBasisPoints policy False 31 `shouldBe` 0
    depositRetainedBasisPoints policy False 30 `shouldBe` 5000
    depositRetainedBasisPoints policy False 7 `shouldBe` 5000
    depositRetainedBasisPoints policy False 6 `shouldBe` 10000
    depositRetainedBasisPoints policy True 90 `shouldBe` 10000
  it "rejects inverted thresholds and out-of-range retention" $ do
    validateDomoCancellationPolicy policy { dcpFullRefundMinDays = 7 } `shouldSatisfy` isLeft
    validateDomoCancellationPolicy policy { dcpPartialRetainedBps = 10001 } `shouldSatisfy` isLeft
    validateDomoCancellationPolicy policy { dcpPartialRefundMinDays = -1 } `shouldSatisfy` isLeft
  it "a TDF cancellation refunds everything collected" $
    property $ forAll genMinor $ \d -> forAll genMinor $ \b -> \inProgress lead ->
      domoCancellationRefund policy CancelledByTdf inProgress lead d b === d + b
  it "a customer refund is within the collected money and always includes the balance" $
    property $ forAll genMinor $ \d -> forAll genMinor $ \b -> \inProgress lead ->
      let r = domoCancellationRefund policy CancelledByCustomer inProgress lead d b
      in r >= b .&&. r <= d + b
  it "cancelling earlier never refunds less" $
    property $ forAll genMinor $ \d -> forAll genMinor $ \b -> \(NonNegative earlier) (NonNegative later) ->
      let early = domoCancellationRefund policy CancelledByCustomer False (earlier + later) d b
          late = domoCancellationRefund policy CancelledByCustomer False later d b
      in early >= late
  it "the three windows refund the deposit in full, half (rounded half up) and nothing" $
    property $ forAll genMinor $ \d -> forAll genMinor $ \b ->
      conjoin
        [ domoCancellationRefund policy CancelledByCustomer False 31 d b === d + b
        , domoCancellationRefund policy CancelledByCustomer False 10 d b
            === d - fromInteger ((toInteger d * 5000 + 5000) `div` 10000) + b
        , domoCancellationRefund policy CancelledByCustomer False 0 d b === b
        , domoCancellationRefund policy CancelledByCustomer True 60 d b === b
        ]
  describe "cancellation assessment" $ do
    let start = UTCTime (fromGregorian 2030 1 10) (15 * 3600)
        at seconds = addUTCTime (fromIntegral (seconds :: Int)) start
    it "counts whole 24-hour periods before the start and goes negative afterwards" $ do
      leadDaysBefore (at (-31 * 86400)) start `shouldBe` 31
      leadDaysBefore (at (-31 * 86400 + 1)) start `shouldBe` 30
      leadDaysBefore (at (-7 * 86400)) start `shouldBe` 7
      leadDaysBefore (at 0) start `shouldBe` 0
      leadDaysBefore (at 1) start `shouldBe` (-1)
    it "conserves collected money and retains at most the deposit, for every input" $
      property $ forAll genMinor $ \d -> forAll genMinor $ \b -> \inProgress (offset :: Int) tdf ->
        let canceller = if tdf then CancelledByTdf else CancelledByCustomer
            now = at (offset * 3600)
            a = assessDomoCancellation policy canceller inProgress now start d b
        in conjoin
             [ caRetainedMinor a + caRefundDueMinor a === d + b
             , property (caRetainedMinor a <= d)
             , property (caRetainedMinor a >= 0 && caRefundDueMinor a >= 0)
             , caInProgress a === (inProgress || now >= start)
             , caLeadDays a === leadDaysBefore now start
             ]
    it "treats a cancellation at or after the start as in progress: deposit retained, balance refunded" $
      property $ forAll genMinor $ \d -> forAll genMinor $ \b -> \(NonNegative late) ->
        let a = assessDomoCancellation policy CancelledByCustomer False (at late) start d b
        in caInProgress a .&&. caRetainedMinor a === d .&&. caRefundDueMinor a === b
    it "a TDF cancellation never retains anything" $
      property $ forAll genMinor $ \d -> forAll genMinor $ \b -> \inProgress (offset :: Int) ->
        let a = assessDomoCancellation policy CancelledByTdf inProgress (at (offset * 3600)) start d b
        in caRetainedMinor a === 0 .&&. caRetainedBasisPoints a === 0 .&&. caRefundDueMinor a === d + b
    it "applies the AUTHORITY-093 ladder to the deposit at 31, 10 and 3 days" $ do
      let a days = assessDomoCancellation policy CancelledByCustomer False (at (-days * 86400)) start 200704 0
      caRefundDueMinor (a 31) `shouldBe` 200704
      caRetainedBasisPoints (a 31) `shouldBe` 0
      caRefundDueMinor (a 10) `shouldBe` 100352
      caRetainedBasisPoints (a 10) `shouldBe` 5000
      caRefundDueMinor (a 3) `shouldBe` 0
      caRetainedMinor (a 3) `shouldBe` 200704
