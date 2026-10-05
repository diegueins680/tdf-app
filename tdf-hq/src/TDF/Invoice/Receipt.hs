{-# LANGUAGE OverloadedStrings #-}

module TDF.Invoice.Receipt (validateReceiptSnapshot) where

import Control.Monad (unless, when)
import Data.Text (Text)

-- Quantities, unit minor units, basis points and stored line totals. Convert
-- before every multiplication/addition, including historical stored inputs.
validateReceiptSnapshot
  :: (Int, Int, Int)
  -> [(Int, Int, Int, Int)]
  -> Either Text (Int, Int, Int)
validateReceiptSnapshot header lineInputs = do
  when (null lineInputs) $ Left "Invoice has no line items to receipt"
  checked <- traverse lineTotals lineInputs
  let subtotal = sum [s | (s, _, _) <- checked]
      tax = sum [t | (_, t, _) <- checked]
      total = sum [t | (_, _, t) <- checked]
      actual = (subtotal, tax, total)
      (storedSubtotal, storedTax, storedTotal) = header
      stored = (toInteger storedSubtotal, toInteger storedTax, toInteger storedTotal)
  unless (actual == stored) $ Left "Invoice header and line amounts disagree"
  unless (all (\n -> n >= 0 && n <= toInteger (maxBound :: Int)) [subtotal, tax, total]) $
    Left "Invoice receipt amount is out of range"
  pure (fromInteger subtotal, fromInteger tax, fromInteger total)
  where
    lineTotals (quantity, unit, bps, storedTotal) = do
      unless (quantity > 0 && unit >= 0 && bps >= 0 && bps <= 10000) $
        Left "Invoice line has invalid quantity, amount or tax"
      let subtotal = toInteger quantity * toInteger unit
          tax = subtotal * toInteger bps `div` 10000
          total = subtotal + tax
      unless (total == toInteger storedTotal) $
        Left "Invoice line amount disagrees with its quantity, price or tax"
      pure (subtotal, tax, total)
