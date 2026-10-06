{-# LANGUAGE OverloadedStrings #-}
module TDF.Invoice.ReceiptSpec (spec, main) where

import Data.Either (isLeft)
import Test.Hspec
import Test.QuickCheck
import TDF.Invoice.Receipt (validateReceiptSnapshot)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "invoice receipt immutable amount snapshot" $ do
  it "preserves valid minor units using per-line floor rounding" $
    property $ forAll (chooseInt (1, 10000)) $ \quantity ->
      forAll (chooseInt (0, 1000000)) $ \unit ->
      forAll (chooseInt (0, 10000)) $ \bps ->
        let subtotal = quantity * unit
            tax = fromInteger (toInteger subtotal * toInteger bps `div` 10000)
            total = subtotal + tax
        in validateReceiptSnapshot (subtotal, tax, total)
          [(quantity, unit, bps, total)] == Right (subtotal, tax, total)
  it "sums individually rounded lines without changing invoice totals" $
    validateReceiptSnapshot (6, 0, 6) [(1, 3, 1500, 3), (1, 3, 1500, 3)]
      `shouldBe` Right (6, 0, 6)
  it "rejects empty receipts" $
    validateReceiptSnapshot (0, 0, 0) [] `shouldSatisfy` isLeft
  it "rejects inconsistent stored header values" $
    validateReceiptSnapshot (99, 1, 100) [(1, 100, 0, 100)] `shouldSatisfy` isLeft
  it "rejects a stored line that does not match exact price and tax" $
    validateReceiptSnapshot (100, 15, 115) [(1, 100, 1500, 114)] `shouldSatisfy` isLeft
  it "rejects negative quantity, price and tax or excessive tax" $
    mapM_ (\line -> validateReceiptSnapshot (0, 0, 0) [line] `shouldSatisfy` isLeft)
      [(0, 0, 0, 0), (-1, 0, 0, 0), (1, -1, 0, 0), (1, 0, -1, 0), (1, 0, 10001, 0)]
  it "rejects multiplication overflow even when wrapped stored values agree" $
    let unit = maxBound :: Int
        wrapped = 2 * unit
    in validateReceiptSnapshot (wrapped, 0, wrapped) [(2, unit, 0, wrapped)]
         `shouldSatisfy` isLeft
  it "rejects multiplication overflow that wraps back to nonnegative zero" $
    validateReceiptSnapshot (0, 0, 0) [(4, maxBound `div` 2 + 1, 0, 0)]
      `shouldSatisfy` isLeft
  it "rejects aggregate overflow even when each line fits" $
    let unit = maxBound :: Int
        wrapped = 2 * unit
    in validateReceiptSnapshot (wrapped, 0, wrapped) [(1, unit, 0, unit), (1, unit, 0, unit)]
         `shouldSatisfy` isLeft
  it "accepts the exact representable upper boundary with zero tax" $
    validateReceiptSnapshot (maxBound, 0, maxBound) [(1, maxBound, 0, maxBound)]
      `shouldBe` Right (maxBound, 0, maxBound)
