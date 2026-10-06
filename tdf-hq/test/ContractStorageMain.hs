module Main where
import Test.Hspec (hspec)
import qualified TDF.ContractStorageSpec as ContractStorage
main :: IO ()
main = hspec ContractStorage.spec
