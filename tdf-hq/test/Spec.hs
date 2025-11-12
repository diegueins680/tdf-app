module Main (main) where

import           Test.Hspec

import qualified TDF.ServerSpec as ServerSpec
import qualified TDF.APITypesSpec as APITypesSpec

main :: IO ()
main = hspec $ do
  ServerSpec.spec
  APITypesSpec.spec
