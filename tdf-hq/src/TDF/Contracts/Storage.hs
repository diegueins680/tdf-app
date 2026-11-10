{-# LANGUAGE OverloadedStrings #-}
-- | Private immutable contract documents. Directory ancestors are trusted and
-- stable; provider delivery and cross-file/database atomicity are not implied.
module TDF.Contracts.Storage (storeContractBytes, readContractBytes) where

import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files (getSymbolicLinkStatus, isRegularFile, linkCount, setFileMode)
import TDF.Storage.AtomicPublication (publishCompleteFile, synchroniseDirectory, synchroniseFile)

paths :: FilePath -> Text -> IO (FilePath, FilePath)
paths root identifier = do
  case UUID.fromText identifier of
    Just uuid | UUID.toText uuid == identifier && uuid /= UUID.nil -> pure ()
    _ -> ioError (userError "Invalid stored contract identifier")
  let name = T.unpack identifier <> ".json"
  pure (root </> "uploads" </> "contracts" </> name,
        root </> "contracts" </> "store" </> name)

readPrivate :: FilePath -> IO (Maybe BL.ByteString)
readPrivate path = do
  info <- (Just <$> getSymbolicLinkStatus path) `catchIOError` \err ->
    if isDoesNotExistError err then pure Nothing else ioError err
  case info of
    Nothing -> pure Nothing
    Just status -> do
      unless (isRegularFile status && linkCount status == 1) $
        ioError (userError "Unsupported stored contract file")
      Just . BL.fromStrict <$> BS.readFile path

-- Missing new storage alone permits legacy fallback. A malformed new document
-- is returned to the caller's validator, never hidden by a legacy copy.
readContractBytes :: FilePath -> Text -> IO (Maybe BL.ByteString)
readContractBytes root identifier = do
  (current, legacy) <- paths root identifier
  currentBytes <- readPrivate current
  legacyBytes <- readPrivate legacy
  case (currentBytes, legacyBytes) of
    (Just a, Just b) | a /= b -> ioError (userError "Conflicting stored contract copies")
    (Just a, _) -> pure (Just a)
    (_, previous) -> pure previous

-- Creation is not idempotent: UUID collisions fail without replacing either
-- retained document. A lost HTTP response can leave a complete unreferenced file.
storeContractBytes :: FilePath -> Text -> BL.ByteString -> IO ()
storeContractBytes root identifier bytes = do
  (destination, legacy) <- paths root identifier
  previous <- readPrivate legacy
  when (previous /= Nothing) $ ioError (userError "Stored contract identifier already exists")
  let uploads = root </> "uploads"
      directory = uploads </> "contracts"
  createDirectoryIfMissing True directory
  setFileMode directory 0o700
  synchroniseDirectory uploads
  synchroniseDirectory root
  published <- publishCompleteFile destination (`BL.hPut` bytes)
  unless published $ ioError (userError "Stored contract identifier already exists")
  synchroniseFile destination
  synchroniseDirectory directory
