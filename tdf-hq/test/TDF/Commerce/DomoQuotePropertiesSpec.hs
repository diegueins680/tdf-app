{-# LANGUAGE OverloadedStrings #-}

-- | PAY-DOMO-QUOTE-001 arithmetic properties of 'calculateDomoQuote',
-- PAY-COURSE-PRICE-001 properties of 'calculateCoursePrice', and the
-- PAY-CAPTURE-RECON-001 decision of 'verifiedPaymentNeedsReconciliation'.
module TDF.Commerce.DomoQuotePropertiesSpec (spec) where

import           Data.Int (Int64)
import qualified Data.Map.Strict as Map
import           Data.Time (UTCTime (..), fromGregorian)
import           Test.Hspec
import           Test.QuickCheck

import           TDF.Commerce.CheckoutStore
  ( VerifiedPayment (..)
  , verifiedPaymentNeedsReconciliation
  )
import qualified TDF.Commerce.CheckoutStore as Checkout
import           TDF.Commerce.CourseCheckout
import           TDF.Commerce.DomoQuotes

-- Rate cards within the documented validation envelope, including amounts
-- large enough to reach the overflow guard.
genRateCard :: Gen DomoRateCard
genRateCard = do
  base <- money 1
  perGuest <- money 0
  minimumHours <- chooseInt (1, 12)
  maxGuests <- chooseInt (1, 2000)
  includedGuests <- chooseInt (0, maxGuests)
  maxDuration <- chooseInt (minimumHours, 48)
  hour <- money 1
  setupHour <- money 0
  cateringMinimum <- money 0
  cateringPerGuest <- money 0
  production <- money 0
  transport <- money 0
  tax <- chooseInt (0, 10000)
  deposit <- chooseInt (1, 10000)
  maxSetup <- chooseInt (0, 24)
  pure DomoRateCard
    { drcEventRates = Map.fromList [("wedding", DomoEventRate base perGuest minimumHours includedGuests)]
    , drcHourMinor = hour
    , drcSetupHourMinor = setupHour
    , drcCateringMinimumMinor = cateringMinimum
    , drcCateringPerGuestMinor = cateringPerGuest
    , drcProductionMinor = production
    , drcTransportMinor = transport
    , drcTaxBasisPoints = tax
    , drcDepositBasisPoints = deposit
    , drcMaximumGuests = maxGuests
    , drcMaximumDurationHours = maxDuration
    , drcMaximumSetupHours = maxSetup
    }
  where
    money lower = frequency
      [ (8, fromIntegral <$> chooseInt (fromIntegral lower, 5000000))
      , (1, choose (lower, maxBound `div` 4))
      ]

genInput :: DomoRateCard -> Gen DomoQuoteInput
genInput card = DomoQuoteInput "Wedding"
  <$> chooseInt (1, drcMaximumGuests card)
  <*> chooseInt (1, drcMaximumDurationHours card)
  <*> chooseInt (0, drcMaximumSetupHours card)
  <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "calculateDomoQuote arithmetic (PAY-DOMO-QUOTE-001)" $ do
    it "accepted quotes are internally consistent and the deposit never exceeds the total" $
      property $ forAll genRateCard $ \card -> forAll (genInput card) $ \input ->
        case calculateDomoQuote card input of
          Left _ -> property True
          Right b ->
            let lineSum = sum (map (toInteger . dqlSubtotalMinor) (dqbLines b))
            in conjoin
                 [ lineSum === toInteger (dqbSubtotalMinor b)
                 , toInteger (dqbTotalMinor b) === toInteger (dqbSubtotalMinor b) + toInteger (dqbTaxMinor b)
                 , toInteger (dqbDepositMinor b) + toInteger (dqbBalanceMinor b) === toInteger (dqbTotalMinor b)
                 , property (dqbDepositMinor b > 0)
                 , property (dqbDepositMinor b <= dqbTotalMinor b)
                 , property (dqbBalanceMinor b >= 0)
                 , property (dqbTaxMinor b >= 0)
                 , property (all (\l -> dqlSubtotalMinor l >= 0
                                     && toInteger (dqlSubtotalMinor l)
                                          == toInteger (dqlQuantity l) * toInteger (dqlUnitAmountMinor l))
                                 (dqbLines b))
                 , property (dqbBillableHours b >= dqiDurationHours input)
                 ]
    it "rejects instead of wrapping when amounts exceed Int64" $
      property $ forAll genRateCard $ \card -> forAll (genInput card) $ \input ->
        let huge = card { drcHourMinor = maxBound :: Int64 }
            input' = input { dqiDurationHours = max 2 (dqiDurationHours input) }
            card' = huge { drcMaximumDurationHours = max 2 (drcMaximumDurationHours huge) }
        in calculateDomoQuote card' input' === Left "Domo quote total is too large"

  describe "calculateCoursePrice arithmetic (PAY-COURSE-PRICE-001)" $ do
    let genPrice = frequency [(8, fromIntegral <$> chooseInt (1, 5000000)), (1, choose (1, maxBound `div` 4))] :: Gen Int64
    it "full payment charges exactly the total and leaves no balance" $
      property $ forAll genPrice $ \price -> forAll (chooseInt (0, 10000)) $ \taxBps ->
        case calculateCoursePrice price taxBps CourseFullPayment 10000 of
          Left err -> counterexample (show err) False
          Right b -> conjoin
            [ toInteger (cpbTotalMinor b) === toInteger price + toInteger (cpbTaxMinor b)
            , cpbDueNowMinor b === cpbTotalMinor b
            , cpbBalanceMinor b === 0
            , property (cpbTaxMinor b >= 0 && cpbTaxMinor b <= price)
            ]
    it "deposit mode keeps the deposit within the total and the balance non-negative" $
      property $ forAll genPrice $ \price -> forAll (chooseInt (0, 10000)) $ \taxBps ->
        forAll (chooseInt (1, 9999)) $ \depositBps ->
          case calculateCoursePrice price taxBps CourseDeposit depositBps of
            Left err -> counterexample (show err) False
            Right b -> conjoin
              [ toInteger (cpbDueNowMinor b) + toInteger (cpbBalanceMinor b) === toInteger (cpbTotalMinor b)
              , property (cpbDueNowMinor b >= 0 && cpbDueNowMinor b <= cpbTotalMinor b)
              , property (cpbBalanceMinor b >= 0)
              -- PAY-DEPOSIT-POLICY-001: a zero balance means the policy switched to full payment.
              , property (cpbBalanceMinor b > 0 || cpbDueNowMinor b == cpbTotalMinor b)
              , property (cpbDueNowMinor b >= min 500 (cpbTotalMinor b))
              ]
    it "rejects the deposit rate boundaries and a full-payment rate other than 10000" $ do
      calculateCoursePrice 10000 1200 CourseDeposit 0 `shouldSatisfy` isLeftE
      calculateCoursePrice 10000 1200 CourseDeposit 10000 `shouldSatisfy` isLeftE
      calculateCoursePrice 10000 1200 CourseFullPayment 5000 `shouldSatisfy` isLeftE
      calculateCoursePrice 0 1200 CourseFullPayment 10000 `shouldSatisfy` isLeftE
    it "rejects instead of wrapping beyond Int64" $
      calculateCoursePrice maxBound 10000 CourseFullPayment 10000 === Left "Course total is too large"

  describe "verifiedPaymentNeedsReconciliation (PAY-CAPTURE-RECON-001)" $ do
    let base = VerifiedPayment
          { vpAttempt = Checkout.PaymentAttemptReference "00000000-0000-0000-0000-000000000001"
          , vpCheckout = Checkout.CheckoutReference "00000000-0000-0000-0000-000000000002"
          , vpProvider = Checkout.ProviderPayPal
          , vpEnvironment = Checkout.CheckoutSandbox
          , vpMerchantRef = "merchant"
          , vpResourceType = "capture"
          , vpProviderResource = "CAPTURE-1"
          , vpProviderResourcePath = Nothing
          , vpOrderReference = "order"
          , vpProviderReference = "order"
          , vpAmountMinor = 1000
          , vpCurrency = "USD"
          , vpEvidence = "server_to_server"
          , vpOccurredAt = UTCTime (fromGregorian 2026 11 1) 0
          , vpCorrelationId = "corr"
          }
    it "reconciles an authoritative capture the checkout did not admit" $
      verifiedPaymentNeedsReconciliation base (Left "expired") `shouldBe` True
    it "records nothing for an admitted payment or a replay" $ do
      verifiedPaymentNeedsReconciliation base (Right True) `shouldBe` False
      verifiedPaymentNeedsReconciliation base (Right False) `shouldBe` False
    it "records nothing for evidence that is not money" $ do
      verifiedPaymentNeedsReconciliation base { vpEvidence = "browser_redirect" } (Left "x") `shouldBe` False
      verifiedPaymentNeedsReconciliation base { vpAmountMinor = 0 } (Left "x") `shouldBe` False

isLeftE :: Either a b -> Bool
isLeftE = either (const True) (const False)
