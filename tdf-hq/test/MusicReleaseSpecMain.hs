module Main (main) where

import Test.Hspec (hspec)
import qualified TDF.MusicRelease.DDEX.ERN432Spec as Ern432Spec
import qualified TDF.MusicRelease.ContentSpec as ContentSpec
import qualified TDF.MusicRelease.DomainSpec as DomainSpec
import qualified TDF.MusicRelease.Storage.S3Spec as S3Spec

main :: IO ()
main = hspec $ do
  DomainSpec.spec
  ContentSpec.spec
  Ern432Spec.spec
  S3Spec.spec
