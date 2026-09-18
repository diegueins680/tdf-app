{-# LANGUAGE OverloadedStrings #-}
import TDF.Commerce.StateMachine
import Data.Int (Int64)
main = do
  let m = maxBound :: Int64
  print $ transitionPayment (PaymentLifecycle PaymentPartiallyCaptured m m 1 0) (PaymentCaptureVerified m)
  print $ transitionPayment (PaymentLifecycle PaymentPartiallyRefunded m m m 1) (PaymentRefundVerified m)
  print $ ledgerBalances [("USD", m), ("USD", m), ("USD", 2)]
