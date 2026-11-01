{-# LANGUAGE OverloadedStrings #-}

-- | PAY-DOMO-CANCEL-001 (AUTHORITY-093): refund policy properties.
module TDF.Commerce.DomoCancellationSpec (spec) where

import           Data.Either (isLeft, isRight)
import           Data.Int (Int64)
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
