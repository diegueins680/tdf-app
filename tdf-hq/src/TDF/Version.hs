{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TDF.Version
  ( VersionInfo(..)
  , getVersionInfo
  ) where

import           Data.Aeson                   (ToJSON(..), object, (.=))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Version                 (showVersion)
import qualified Data.Time                    as Time
import           Development.GitRev           (gitHash)
import           Data.Maybe                   (fromMaybe)
import           GHC.Generics                 (Generic)
import           Language.Haskell.TH.Syntax   (runIO)
import           Paths_tdf_hq                 (version)
import           System.Environment           (lookupEnv)

data VersionInfo = VersionInfo
  { name      :: Text
  , appVer    :: Text
  , commit    :: Text
  , buildTime :: Text
  } deriving (Show, Generic)

instance ToJSON VersionInfo where
  toJSON v = object
    [ "name"      .= name v
    , "version"   .= appVer v
    , "commit"    .= commit v
    , "buildTime" .= buildTime v
    ]

getVersionInfo :: IO VersionInfo
getVersionInfo = do
  sha   <- resolveCommit
  btime <- resolveBuildTime
  pure VersionInfo
    { name      = "tdf-hq"
    , appVer    = T.pack (showVersion version)
    , commit    = sha
    , buildTime = btime
    }

resolveCommit :: IO Text
resolveCommit = do
  mEnv <- firstJustM lookupEnv commitEnvVars
  let envCommit       = mEnv >>= canonCommit . T.pack
      fallbackCommit  = fromMaybe "dev" compiledCommit
  pure (fromMaybe fallbackCommit envCommit)
  where
    compiledCommit = canonCommit (T.pack $(gitHash))

canonCommit :: Text -> Maybe Text
canonCommit txt =
  let trimmed = T.strip txt
      upper   = T.toUpper trimmed
  in if T.null trimmed || upper == "UNKNOWN" || upper == "DEV"
       then Nothing
       else Just trimmed

resolveBuildTime :: IO Text
resolveBuildTime = do
  mEnv <- firstJustM lookupEnv buildTimeEnvVars
  pure (maybe compiledBuildTime T.pack mEnv)

compiledBuildTime :: Text
compiledBuildTime = T.pack $(do
  now <- runIO Time.getCurrentTime
  let iso = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  [| iso |])

commitEnvVars :: [String]
commitEnvVars =
  [ "GIT_SHA"
  , "SOURCE_COMMIT"
  , "SOURCE_VERSION"
  , "RENDER_GIT_COMMIT"
  , "RENDER_GIT_COMMIT_SHA"
  , "VERCEL_GIT_COMMIT_SHA"
  ]

buildTimeEnvVars :: [String]
buildTimeEnvVars =
  [ "BUILD_TIME"
  , "SOURCE_BUILD_TIME"
  , "RENDER_BUILD_TIME"
  ]

firstJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x:xs) = do
  res <- f x
  case res of
    Just val -> pure (Just val)
    Nothing  -> firstJustM f xs
