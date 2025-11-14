module Main (main) where

import           Test.Hspec

import qualified TDF.ServerSpec            as ServerSpec
import qualified TDF.APITypesSpec         as APITypesSpec
import qualified TDF.Profiles.ArtistSpec  as ArtistSpec

main :: IO ()
main = hspec $ do
  ServerSpec.spec
  APITypesSpec.spec
  ArtistSpec.spec
