{-# LANGUAGE OverloadedStrings #-}
module TDF.Commerce.PaymentArithmeticSpec (spec) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.Int (Int64)
import Test.Hspec
import qualified TDF.Commerce.StateMachine as C

-- Exercise the actual implementation against an independent Integer oracle.
-- Finite boundary coverage, not a proof of the payment subsystem.
spec :: Spec
spec = describe "payment arithmetic Integer oracle" $ do
  let cap = maxBound :: Int64
      balances = [0, 1, 2, cap - 1, cap]
      increments = [minBound, minBound + 1, -1, 0, 1, 2, cap - 1, cap]
      cases = [(current, limit, amount)
              | current <- balances, limit <- balances, amount <- increments]
  forM_ cases $ \inputs@(current, limit, amount) -> do
    let exact = toInteger current + toInteger amount
        permitted = amount > 0 && exact <= toInteger limit
    it ("capture " <> show inputs) $ do
      let result = C.transitionPayment
            (C.PaymentLifecycle C.PaymentPartiallyCaptured cap limit current 0)
            (C.PaymentCaptureVerified amount)
      isLeft result `shouldBe` not permitted
      case result of
        Right next -> toInteger (C.paymentCapturedMinor next) `shouldBe` exact
        Left _ -> pure ()
    it ("refund " <> show inputs) $ do
      let result = C.transitionPayment
            (C.PaymentLifecycle C.PaymentPartiallyRefunded cap cap limit current)
            (C.PaymentRefundVerified amount)
      isLeft result `shouldBe` (not permitted || current > limit)
      case result of
        Right next -> toInteger (C.paymentRefundedMinor next) `shouldBe` exact
        Left _ -> pure ()
  let entries = [minBound, -cap, -1, 0, 1, 2, cap - 1, cap]
  forM_ [[a,b,c] | a <- entries, b <- entries, c <- entries] $ \values ->
    it ("ledger " <> show values) $
      C.ledgerBalances [("USD", x) | x <- values]
        `shouldBe` (sum (map toInteger values) == 0)
