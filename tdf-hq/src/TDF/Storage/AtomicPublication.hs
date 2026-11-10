{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if defined(linux_HOST_OS)
{-# OPTIONS_GHC -optc-D_GNU_SOURCE #-}
#endif

-- | Complete local-file publication into an existing, trusted directory.
-- Unsupported exclusive rename or fsync fails closed; never fall back to replace.
module TDF.Storage.AtomicPublication
  ( publishCompleteFile
  , publishCompleteFileWithSync
  , synchroniseDirectory
  , synchroniseFile
  ) where

import Control.Exception (bracket, finally, throwIO)
import Control.Monad (when)
import Foreign.C.Error (eEXIST, getErrno, throwErrno)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CUInt(..))
import System.Directory (removeFile)
import System.FilePath (takeDirectory)
import System.IO (Handle, hClose, hFlush, openBinaryTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.IO (OpenMode(ReadOnly), closeFd, defaultFileFlags, handleToFd, openFd)
import System.Posix.Types (Fd)
import System.Posix.Unistd (fileSynchronise)

#if defined(darwin_HOST_OS)
foreign import capi safe "stdio.h renamex_np"
  renameExclusive :: CString -> CString -> CUInt -> IO CInt
foreign import capi unsafe "stdio.h value RENAME_EXCL"
  exclusiveFlag :: CUInt
renameNoReplace :: CString -> CString -> IO CInt
renameNoReplace old new = renameExclusive old new exclusiveFlag
#elif defined(linux_HOST_OS)
foreign import capi safe "stdio.h renameat2"
  renameExclusive :: CInt -> CString -> CInt -> CString -> CUInt -> IO CInt
foreign import capi unsafe "fcntl.h value AT_FDCWD"
  currentDirectory :: CInt
foreign import capi unsafe "stdio.h value RENAME_NOREPLACE"
  exclusiveFlag :: CUInt
renameNoReplace :: CString -> CString -> IO CInt
renameNoReplace old new = renameExclusive currentDirectory old currentDirectory new exclusiveFlag
#else
#error Atomic file publication requires a qualified Linux or Darwin exclusive rename
#endif

synchroniseDirectory :: FilePath -> IO ()
synchroniseDirectory = synchronisePathUsing fileSynchronise

synchroniseFile :: FilePath -> IO ()
synchroniseFile = synchronisePathUsing fileSynchronise

synchronisePathUsing :: (Fd -> IO ()) -> FilePath -> IO ()
synchronisePathUsing sync path = bracket (openFd path ReadOnly defaultFileFlags) closeFd sync

-- | The writer must finish before publication. True means newly published;
-- False means an existing name was preserved. The caller validates replay bytes.
-- Cancellation after rename may leave a complete final with no acknowledgement.
-- SIGKILL before rename may leave a private staging file; it never becomes final.
publishCompleteFile :: FilePath -> (Handle -> IO ()) -> IO Bool
publishCompleteFile = publishCompleteFileWithSync fileSynchronise

-- | Fault-injection seam for filesystem tests. Production callers use the fixed
-- fileSynchronise implementation above; custom sync callbacks are not durability.
publishCompleteFileWithSync :: (Fd -> IO ()) -> FilePath -> (Handle -> IO ()) -> IO Bool
publishCompleteFileWithSync sync destination writeContent = do
  when ('\0' `elem` destination) $ throwIO (userError "Invalid publication path")
  let directory = takeDirectory destination
      removeStaging path = (removeFile path >> synchronisePathUsing sync directory) `catchIOError` \err ->
        if isDoesNotExistError err then pure () else throwIO err
      cleanup (path, handle) = hClose handle `finally` removeStaging path
  bracket (openBinaryTempFile directory ".tdf-rider-pending.tmp") cleanup $ \(staging, handle) -> do
    writeContent handle
    hFlush handle
    -- handleToFd transfers ownership and closes the Handle. The bracket owns
    -- the descriptor even when synchronization throws or is cancelled.
    bracket (handleToFd handle) closeFd sync
    created <- withCString staging $ \old -> withCString destination $ \new -> do
      result <- renameNoReplace old new
      if result == 0 then pure True else do
        errno <- getErrno
        if errno == eEXIST then pure False else throwErrno "exclusive file publication"
    -- A failed sync is not success; never remove a potentially published final.
    synchronisePathUsing sync directory
    pure created
