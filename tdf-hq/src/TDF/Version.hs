{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TDF.Version
  ( VersionInfo(..)
  , getVersionInfo
  ) where

import           Data.Aeson                   (ToJSON(..), object, (.=))
import           Data.Char                    (isControl, isSpace)
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
import qualified System.IO.Error              as IOError
import qualified Data.Text.IO                 as TIO
import           Control.Applicative          ((<|>))

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
  envCommit <- firstJustM lookupCommitEnv commitEnvVars
  mFile <- readMaybeFile commitFilePath
  let fileCommit     = mFile >>= canonCommit
      fallbackCommit = fromMaybe "dev" compiledCommit
  pure (fromMaybe fallbackCommit (envCommit <|> fileCommit))
  where
    compiledCommit = canonCommit (T.pack $(gitHash))
    commitFilePath = "/app/COMMIT"
    lookupCommitEnv key = fmap (>>= canonCommit . T.pack) (lookupEnv key)

canonCommit :: Text -> Maybe Text
canonCommit txt = do
  value <- canonRuntimeMetadata txt
  if T.any isSpace value
    then Nothing
    else Just value

canonBuildTime :: Text -> Maybe Text
canonBuildTime = canonRuntimeMetadata

canonRuntimeMetadata :: Text -> Maybe Text
canonRuntimeMetadata txt =
  let trimmed = T.strip txt
      upper   = T.toUpper trimmed
  in if T.null trimmed
        || upper `elem` runtimeMetadataSentinels
        || T.any isControl trimmed
       then Nothing
       else Just trimmed

runtimeMetadataSentinels :: [Text]
runtimeMetadataSentinels =
  [ "UNKNOWN"
  , "DEV"
  , "NULL"
  , "UNDEFINED"
  , "NONE"
  , "N/A"
  ]

resolveBuildTime :: IO Text
resolveBuildTime = do
  envVal <- firstJustM lookupBuildTimeEnv buildTimeEnvVars
  mFile <- readMaybeFile "/app/BUILD_TIME"
  let fileVal = mFile >>= canonBuildTime
  pure (fromMaybe compiledBuildTime (envVal <|> fileVal))
  where
    lookupBuildTimeEnv key = fmap (>>= canonBuildTime . T.pack) (lookupEnv key)

compiledBuildTime :: Text
compiledBuildTime = T.pack $(do
  now <- runIO Time.getCurrentTime
  let iso = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  [| iso |])

commitEnvVars :: [String]
commitEnvVars =
  [ "GIT_SHA"
  , "GIT_COMMIT"
  , "GIT_COMMIT_SHA"
  , "COMMIT_SHA"
  , "SOURCE_COMMIT"
  , "SOURCE_VERSION"
  , "SOURCE_SHA"
  , "GITHUB_SHA"
  , "RENDER_GIT_COMMIT"
  , "RENDER_GIT_COMMIT_SHA"
  , "VERCEL_GIT_COMMIT_SHA"
  , "KOYEB_GIT_SHA"
  , "KOYEB_GIT_COMMIT"
  , "KOYEB_GIT_COMMIT_SHA"
  , "KOYEB_DEPLOYMENT_GIT_SHA"
  , "KOYEB_DEPLOYMENT_GIT_COMMIT"
  , "FLY_GIT_SHA"
  ]

buildTimeEnvVars :: [String]
buildTimeEnvVars =
  [ "BUILD_TIME"
  , "SOURCE_BUILD_TIME"
  , "RENDER_BUILD_TIME"
  , "FLY_BUILD_TIME"
  ]

firstJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x:xs) = do
  res <- f x
  case res of
    Just val -> pure (Just val)
    Nothing  -> firstJustM f xs

readMaybeFile :: FilePath -> IO (Maybe Text)
readMaybeFile path = do
  result <- IOError.tryIOError (TIO.readFile path)
  case result of
    Left _  -> pure Nothing
    Right t -> pure (Just (T.strip t))
