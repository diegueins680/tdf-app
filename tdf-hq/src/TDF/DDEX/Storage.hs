{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.Storage
  ( -- * Storage abstraction
    StorageBackend(..)
  , StorageConfig(..)
  , StoredFile(..)
  , computeSha256
  , generateStoragePath
    -- * Local filesystem backend
  , localStorageBackend
  ) where

import qualified Data.ByteString.Lazy as BL
import Crypto.Hash (hash, SHA256, Digest)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>), isRelative, normalise, splitDirectories)

-- | Configuration for storage backend
data StorageConfig = StorageConfig
  { storageBasePath :: FilePath
  , storageBucket   :: Text
  } deriving (Show, Eq)

-- | Metadata for a stored file
data StoredFile = StoredFile
  { storedFileName     :: Text     -- Original filename
  , storedFilePath     :: FilePath -- Internal storage path
  , storedFileUri      :: Text     -- Private URI for retrieval
  , storedFileSha256   :: Text     -- Content hash
  , storedFileSize     :: Integer  -- Size in bytes
  , storedFileMimeType :: Text     -- MIME type
  , storedFileCreatedAt :: UTCTime
  } deriving (Show, Eq)

-- | Storage backend abstraction
data StorageBackend = StorageBackend
  { storeFile     :: Text -> BL.ByteString -> Text -> IO StoredFile
    -- ^ Store a file: original name, content, MIME type
  , retrieveFile  :: FilePath -> IO (Maybe BL.ByteString)
    -- ^ Retrieve file contents by internal path
  , deleteFile    :: FilePath -> IO Bool
    -- ^ Delete a file, returns True if successful
  , fileExists    :: FilePath -> IO Bool
    -- ^ Check if file exists
  }

-- | Compute SHA-256 hash of lazy ByteString
computeSha256 :: BL.ByteString -> Text
computeSha256 content =
  let digest = hash (BL.toStrict content) :: Digest SHA256
  in T.pack (show digest)

-- | Generate a unique storage path
-- Format: ddex/<uuid>.<ext>
generateStoragePath :: Text -> IO FilePath
generateStoragePath originalName = do
  uuid <- UUID.nextRandom
  let ext = takeExtension' originalName
      uuidStr = UUID.toString uuid
      path = "ddex" </> uuidStr <> ext
  return path
  where
    takeExtension' :: Text -> String
    takeExtension' name =
      case T.splitOn "." name of
        [] -> ""
        parts -> "." ++ T.unpack (last parts)

-- | Local filesystem storage backend
localStorageBackend :: StorageConfig -> StorageBackend
localStorageBackend config = StorageBackend
  { storeFile = storeFileLocal config
  , retrieveFile = retrieveFileLocal config
  , deleteFile = deleteFileLocal config
  , fileExists = \path ->
      if isSafeRelativePath path
        then doesFileExist (storageBasePath config </> normalise path)
        else pure False
  }

storeFileLocal :: StorageConfig -> Text -> BL.ByteString -> Text -> IO StoredFile
storeFileLocal config originalName content mimeType = do
  let sha256 = computeSha256 content
      size = fromIntegral (BL.length content)
  relPath <- generateStoragePath originalName
  let fullPath = storageBasePath config </> relPath
  -- Ensure parent directory exists
  createDirectoryIfMissing True (storageBasePath config </> "ddex")
  -- Write file
  BL.writeFile fullPath content
  now <- getCurrentTime
  return StoredFile
    { storedFileName = originalName
    , storedFilePath = relPath
    , storedFileUri = "local-private://" <> T.pack relPath
    , storedFileSha256 = sha256
    , storedFileSize = size
    , storedFileMimeType = mimeType
    , storedFileCreatedAt = now
    }

retrieveFileLocal :: StorageConfig -> FilePath -> IO (Maybe BL.ByteString)
retrieveFileLocal config relPath = do
  if not (isSafeRelativePath relPath)
    then pure Nothing
    else do
      let fullPath = storageBasePath config </> normalise relPath
      exists <- doesFileExist fullPath
      if exists
        then Just <$> BL.readFile fullPath
        else return Nothing

deleteFileLocal :: StorageConfig -> FilePath -> IO Bool
deleteFileLocal config relPath = do
  if not (isSafeRelativePath relPath)
    then pure False
    else do
      let fullPath = storageBasePath config </> normalise relPath
      exists <- doesFileExist fullPath
      if exists
        then do
          removeFile fullPath
          return True
        else return False

isSafeRelativePath :: FilePath -> Bool
isSafeRelativePath path =
  isRelative path
    && not (null path)
    && all (`notElem` ["..", ".", ""]) (splitDirectories (normalise path))
