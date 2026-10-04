{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TDF.Commerce.CheckoutMoneySpec (spec) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.Int (Int32, Int64)
import Test.Hspec
import Test.QuickCheck
import TDF.Commerce.Money (checkedCheckoutSubtotals, checkedCartSubtotal, checkedCartTotal)

spec :: Spec
spec = describe "PAY-CHECKOUT-001 exact line arithmetic" $ do
  let cap = maxBound :: Int64
  it "rejects a positive wrapped aggregate even when each line fits" $
    checkedCheckoutSubtotals 1 [(1,cap),(1,cap),(1,3)] `shouldSatisfy` isLeft
  it "rejects a positive wrapped line product" $
    checkedCheckoutSubtotals 2 [(3,6148914691236517206)] `shouldSatisfy` isLeft
  it "accepts an exactly maximal representable checkout" $
    checkedCheckoutSubtotals cap [(1,cap - 1),(1,1)] `shouldBe` Right [cap - 1,1]
  it "rejects quantity outside PostgreSQL INTEGER even when amount fits" $
    checkedCheckoutSubtotals (toIntegerInt32Max + 1)
      [(fromIntegral (maxBound :: Int32) + 1,1)] `shouldSatisfy` isLeft
  it "rejects zero-valued hidden lines" $
    checkedCheckoutSubtotals 1 [(0,7),(1,1)] `shouldSatisfy` isLeft
  it "rejects an empty zero checkout" $
    checkedCheckoutSubtotals 0 [] `shouldSatisfy` isLeft
  forM_ [minBound,-1,0,1,2,cap - 1,cap] $ \unit ->
    forM_ [-1,0,1,2,3,fromIntegral (maxBound :: Int32)] $ \quantity ->
      it ("checks quantity and amount boundaries " <> show (quantity,unit)) $ do
        let exact = toInteger quantity * toInteger unit
            parent = fromInteger exact :: Int64
            valid = quantity > 0 && unit > 0 && exact <= toInteger cap
            result = checkedCheckoutSubtotals parent [(quantity,unit)]
        isLeft result `shouldBe` not valid
        case result of
          Right [amount] -> toInteger amount `shouldBe` exact
          Left _ -> pure ()
          _ -> expectationFailure "One input line must produce one subtotal"
  it "agrees with an unbounded oracle over generated multi-line inputs" $
    withMaxSuccess 1000 $ property $ \(rows :: [(Int, Int64)]) ->
      let clipped = take 8 rows
          exact = sum [toInteger quantity * toInteger unit | (quantity,unit) <- clipped]
          parent = fromInteger exact :: Int64
          permitted = not (null clipped) && exact > 0 && exact <= toInteger cap
            && all (\(quantity,unit) -> quantity > 0
              && toInteger quantity <= toInteger (maxBound :: Int32) && unit > 0) clipped
          result = checkedCheckoutSubtotals parent clipped
      in counterexample (show (clipped,parent,result)) $
          isLeft result === not permitted
  it "accepts generated exact positive multi-line snapshots" $
    withMaxSuccess 1000 $ forAll validLines $ \rows ->
      let subtotals = [toInteger quantity * toInteger unit | (quantity,unit) <- rows]
          exact = sum subtotals
      in checkedCheckoutSubtotals (fromInteger exact) rows === Right (map fromInteger subtotals)
  it "rejects a wrapped legacy marketplace cart total" $
    checkedCartTotal [maxBound,maxBound,3] `shouldSatisfy` isLeft
  it "rejects a wrapped legacy marketplace line product" $
    checkedCartSubtotal 3 6148914691236517206 `shouldSatisfy` isLeft
  it "preserves zero cart previews but rejects negative stored prices" $ do
    checkedCartSubtotal 1 0 `shouldBe` Right 0
    checkedCartTotal [] `shouldBe` Right 0
    checkedCartSubtotal 1 (-1) `shouldSatisfy` isLeft
    checkedCartTotal [-1,2] `shouldSatisfy` isLeft
  where
    validLines = do
      count <- chooseInt (1,8)
      vectorOf count $ do
        quantity <- elements [1,2,3,1024,fromIntegral (maxBound :: Int32)]
        unit <- chooseInteger (1, toInteger (maxBound :: Int64) `div` (toInteger count * toInteger quantity))
        pure (quantity,fromInteger unit)
    toIntegerInt32Max = fromIntegral (maxBound :: Int32) :: Int64
