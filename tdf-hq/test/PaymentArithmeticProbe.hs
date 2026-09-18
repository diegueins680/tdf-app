{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Either (isLeft)
import Data.Int (Int64)
import qualified TDF.Commerce.StateMachine as C

-- Execute the actual module against an independent arbitrary-precision oracle.
-- Boundary regression, not an unrestricted proof of the payment subsystem.
main :: IO ()
main = do
  let cap = maxBound :: Int64
      balances = [0, 1, 2, cap - 1, cap]
      increments = [minBound, minBound + 1, -1, 0, 1, 2, cap - 1, cap]
      check label expected observed = unless (expected == observed) (fail label)
  forM_ [(current, limit, amount) | current <- balances, limit <- balances, amount <- increments] $
    \(current, limit, amount) -> do
      let exact = toInteger current + toInteger amount
          permitted = amount > 0 && exact <= toInteger limit
          capture = C.transitionPayment
            (C.PaymentLifecycle C.PaymentPartiallyCaptured cap limit current 0)
            (C.PaymentCaptureVerified amount)
          refund = C.transitionPayment
            (C.PaymentLifecycle C.PaymentPartiallyRefunded cap cap limit current)
            (C.PaymentRefundVerified amount)
      check "capture admission differs from exact arithmetic" (not permitted) (isLeft capture)
      case capture of
        Right result -> check "capture sum differs from Integer oracle" exact (toInteger (C.paymentCapturedMinor result))
        Left _ -> pure ()
      check "refund admission differs from exact arithmetic"
        (not permitted || current > limit) (isLeft refund)
      case refund of
        Right result -> check "refund sum differs from Integer oracle" exact (toInteger (C.paymentRefundedMinor result))
        Left _ -> pure ()
  let entries = [minBound, -cap, -1, 0, 1, 2, cap - 1, cap]
  forM_ [[a,b,c] | a <- entries, b <- entries, c <- entries] $ \values ->
    check "ledger modular overflow" (sum (map toInteger values) == 0)
      (C.ledgerBalances [("USD", x) | x <- values])
  putStrLn "Payment arithmetic: 400 transition boundary cases and 512 ledger cases passed against the actual Haskell module."
