module Main (main) where

import Test.Hspec (hspec)
import qualified TDF.MusicCredentialCacheSpec as CredentialCache

main :: IO ()
main = hspec CredentialCache.spec
