-- Compatibility entrypoint; the canonical oracle also runs in ordinary stack test.
module Main (main) where
import Test.Hspec (hspec)
import qualified TDF.Commerce.PaymentArithmeticSpec as Arithmetic
main :: IO ()
main = hspec Arithmetic.spec
