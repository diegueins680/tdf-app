{-# LANGUAGE OverloadedStrings #-}

-- Local synthetic fixtures only; DPIDs/ISRCs are not official allocations.
module Main (main) where

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import TDF.MusicRelease.DDEX.ERN432
import TDF.MusicRelease.DDEX.ERN432Spec (multiPartyRelease, validErn432AudioRelease, creditSnapshot)

main :: IO ()
main = do
  args <- getArgs
  output <- case args of [path] -> pure path; _ -> die "Usage: MusicDdexCreditsFixtureMain OUTPUT_DIRECTORY"
  graph <- either (die . show) pure (parseErn432Credits creditSnapshot)
  let release = multiPartyRelease {catalogCredits = graph}
      technicalRoles = ["performer","producer","engineer","mixer","mastering_engineer","publisher"]
      complete = release {catalogCredits = graph {canonicalCredits = canonicalCredits graph
        ++ [Ern432Credit "guest" (Just "recording-1") role 4 | role <- technicalRoles]
        ++ [Ern432Credit "unused" Nothing "label" 5]}}
  mapM_ (\(name,value) -> do
    xml <- either (die . show) pure (renderErn432AudioRelease value)
    BL.writeFile (output </> name) xml)
    [("single.xml",validErn432AudioRelease),("ep.xml",release {releaseKind = ErnEP})
    ,("album.xml",release),("update.xml",complete {messagePurpose = ErnUpdate})
    ,("takedown.xml",complete {messagePurpose = ErnTakedown})]
