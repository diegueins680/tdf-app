{-# LANGUAGE OverloadedStrings #-}

-- | PAY-DEPOSIT-POLICY-001 (AUTHORITY-095): deposit minimum and full-payment switch.
module TDF.Commerce.DepositPolicySpec (spec) where

import           Data.Either (isLeft, isRight)
import           Data.Int (Int64)
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Commerce.DepositPolicy

genMinor :: Gen Int64
genMinor = frequency [(8, fromIntegral <$> chooseInt (0, 5000000)), (1, choose (0, maxBound `div` 4))]

policy :: DepositPolicy
policy = defaultDepositPolicy

spec :: Spec
spec = describe "Deposit policy (PAY-DEPOSIT-POLICY-001)" $ do
  it "the default policy is valid: minimum 5.00 and full payment above 90%" $ do
    validateDepositPolicy policy `shouldSatisfy` isRight
    dpMinimumDepositMinor policy `shouldBe` 500
    dpFullPaymentAboveBps policy `shouldBe` 9000
    validateDepositPolicy policy { dpFullPaymentAboveBps = 10001 } `shouldSatisfy` isLeft
    validateDepositPolicy policy { dpMinimumDepositMinor = -1 } `shouldSatisfy` isLeft
  it "never rejects: every positive total yields a deposit within 1..total or full payment" $
    property $ forAll (suchThat genMinor (> 0)) $ \total -> forAll genMinor $ \computed ->
      case applyDepositPolicy policy total computed of
        FullPaymentNow -> property True
        DepositOf d -> d >= 1 .&&. d <= total .&&. total - d >= 0
  it "raises a rounded-to-zero or tiny deposit to the minimum" $
    property $ forAll (choose (1000, 5000000)) $ \total -> forAll (choose (0, 499)) $ \computed ->
      applyDepositPolicy policy total computed === DepositOf 500
  it "switches to full payment exactly when the raised deposit exceeds 90% of the total" $
    property $ forAll (suchThat genMinor (> 0)) $ \total -> forAll genMinor $ \computed ->
      let raised = max 1 (max 500 computed)
          overShare = toInteger raised * 10000 > toInteger total * 9000
      in (applyDepositPolicy policy total computed == FullPaymentNow) === (raised >= total || overShare)
  it "a non-positive total is always full payment" $
    property $ \(NonNegative n) computed ->
      applyDepositPolicy policy (negate n) computed === FullPaymentNow
