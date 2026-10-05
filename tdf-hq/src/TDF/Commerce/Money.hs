{-# LANGUAGE OverloadedStrings #-}
module TDF.Commerce.Money (checkedCheckoutSubtotals, checkedCartSubtotal, checkedCartTotal) where

import Data.Int (Int32, Int64)
import Data.Text (Text)

-- Compute before narrowing: both the per-line product and the complete sum
-- must represent the immutable Int64 parent amount exactly. Quantity is stored
-- in PostgreSQL INTEGER even on hosts whose Haskell Int is 64 bits.
checkedCheckoutSubtotals :: Int64 -> [(Int, Int64)] -> Either Text [Int64]
checkedCheckoutSubtotals expected linesToPrice
  | expected <= 0 = Left "Canonical checkout amount must be positive"
  | null linesToPrice = Left "Canonical checkout requires at least one immutable line item"
  | any invalidLine linesToPrice = Left "Canonical checkout line quantity or unit amount is invalid"
  | any (> toInteger (maxBound :: Int64)) subtotals = Left "Canonical checkout line amount overflows storage"
  | sum subtotals /= toInteger expected = Left "Canonical checkout line totals do not match the checkout total"
  | otherwise = Right (map fromInteger subtotals)
  where
    invalidLine (quantity, unit) =
      quantity <= 0 || toInteger quantity > toInteger (maxBound :: Int32) || unit <= 0
    subtotals = [toInteger quantity * toInteger unit | (quantity, unit) <- linesToPrice]

-- Legacy marketplace DTO/order fields use host Int. Keep the exact computation
-- until storage bounds have been checked; zero is allowed for cart display,
-- while checkout admission separately requires a positive payable total.
checkedCartSubtotal :: Int -> Int -> Either Text Int
checkedCartSubtotal quantity unit
  | quantity <= 0 || unit < 0 = Left "Stored marketplace quantity or price is invalid"
  | otherwise = narrowCartAmount (toInteger quantity * toInteger unit)

checkedCartTotal :: [Int] -> Either Text Int
checkedCartTotal amounts
  | any (< 0) amounts = Left "Stored marketplace subtotal is negative"
  | otherwise = narrowCartAmount (sum (map toInteger amounts))

narrowCartAmount :: Integer -> Either Text Int
narrowCartAmount exact
  | exact > toInteger (maxBound :: Int) = Left "Marketplace amount exceeds supported storage"
  | otherwise = Right (fromInteger exact)
