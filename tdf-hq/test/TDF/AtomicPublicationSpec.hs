{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TDF.AtomicPublicationSpec (spec) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (SomeException, bracket, throwIO)
import Control.Monad (void, when)
import qualified Data.ByteString as BS
import System.Directory (doesPathExist, listDirectory)
import System.FilePath ((</>))
import System.IO (hFlush)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, fileMode, getFileStatus, getSymbolicLinkStatus, isSymbolicLink, linkCount)
import System.Timeout (timeout)
import Data.IORef (newIORef, atomicModifyIORef')
import System.Posix.Unistd (fileSynchronise)
import Data.Bits ((.&.))
import Test.Hspec
import TDF.Storage.AtomicPublication (publishCompleteFile, publishCompleteFileWithSync, synchroniseDirectory, synchroniseFile)

spec :: Spec
spec = describe "exclusive complete file publication" $ do
  it "publishes complete private bytes with one link and durable directory sync" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
      publishCompleteFile destination (`BS.hPut` "complete") `shouldReturn` True
      BS.readFile destination `shouldReturn` "complete"
      info <- getFileStatus destination
      fileMode info .&. 0o777 `shouldBe` 0o600
      linkCount info `shouldBe` 1
      listDirectory dir `shouldReturn` ["rider"]
      synchroniseDirectory dir
  it "never overwrites an existing different final" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
      BS.writeFile destination "original"
      publishCompleteFile destination (`BS.hPut` "different") `shouldReturn` False
      BS.readFile destination `shouldReturn` "original"
      listDirectory dir `shouldReturn` ["rider"]
  it "preserves an existing symlink instead of replacing or following it" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
      BS.writeFile (dir </> "other") "original"
      createSymbolicLink "other" destination
      publishCompleteFile destination (`BS.hPut` "different") `shouldReturn` False
      isSymbolicLink <$> getSymbolicLinkStatus destination `shouldReturn` True
      BS.readFile (dir </> "other") `shouldReturn` "original"
  it "never publishes partial bytes after a writer failure" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
      publishCompleteFile destination (\handle -> do
        BS.hPut handle "partial"
        hFlush handle
        throwIO (userError "synthetic interrupted write")) `shouldThrow` anyIOException
      doesPathExist destination `shouldReturn` False
      listDirectory dir `shouldReturn` []
  it "keeps publication absent while writing and after asynchronous cancellation" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
      reached <- newEmptyMVar
      blocked <- newEmptyMVar
      finished <- newEmptyMVar
      let writer = publishCompleteFile destination $ \handle -> do
            BS.hPut handle "partial"
            hFlush handle
            putMVar reached ()
            takeMVar blocked
          cleanup thread = killThread thread >> void (timeout 5000000 (readMVar finished))
      bracket (forkFinally writer (putMVar finished)) cleanup $ \thread -> do
        timeout 5000000 (takeMVar reached) `shouldReturn` Just ()
        doesPathExist destination `shouldReturn` False
        killThread thread
        outcome <- timeout 5000000 (readMVar finished)
        case outcome of
          Just (Left (_ :: SomeException)) -> pure ()
          _ -> expectationFailure "cancelled writer did not terminate with an exception"
        doesPathExist destination `shouldReturn` False
        listDirectory dir `shouldReturn` []
  it "concurrent differing publications produce exactly one complete winner" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
          payloads = [BS.replicate 65536 65, BS.replicate 65536 66]
      start <- newEmptyMVar
      done <- newEmptyMVar
      mapM_ (\bytes -> void $ forkFinally (takeMVar start >> publishCompleteFile destination (`BS.hPut` bytes)) (putMVar done)) payloads
      putMVar start (); putMVar start ()
      results <- sequence [takeMVar done, takeMVar done]
      [created | Right created <- results] `shouldMatchList` [True,False]
      stored <- BS.readFile destination
      stored `shouldSatisfy` (`elem` payloads)
      linkCount <$> getFileStatus destination `shouldReturn` 1
      listDirectory dir `shouldReturn` ["rider"]
  it "identical concurrent publications preserve replay bytes" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
      done <- newEmptyMVar
      mapM_ (\_ -> void $ forkFinally (publishCompleteFile destination (`BS.hPut` "same")) (putMVar done)) [1::Int,2]
      results <- sequence [takeMVar done,takeMVar done]
      [created | Right created <- results] `shouldMatchList` [True,False]
      BS.readFile destination `shouldReturn` "same"
  it "propagates missing parent failures without a final file" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      publishCompleteFile (dir </> "missing" </> "rider") (`BS.hPut` "bytes") `shouldThrow` anyIOException
      listDirectory dir `shouldReturn` []

  it "rejects file fsync failure before publishing a final" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
      publishCompleteFileWithSync (\_ -> throwIO (userError "synthetic fsync failure"))
        destination (`BS.hPut` "complete") `shouldThrow` anyIOException
      doesPathExist destination `shouldReturn` False
      listDirectory dir `shouldReturn` []
  it "retains complete final on publication directory fsync failure and permits replay" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
      calls <- newIORef (0::Int)
      let failSecond fd = do
            n <- atomicModifyIORef' calls (\n -> (n+1,n+1))
            when (n==2) $ throwIO (userError "synthetic directory fsync failure")
            fileSynchronise fd
      publishCompleteFileWithSync failSecond destination (`BS.hPut` "complete") `shouldThrow` anyIOException
      BS.readFile destination `shouldReturn` "complete"
      publishCompleteFile destination (`BS.hPut` "complete") `shouldReturn` False
      synchroniseFile destination
      synchroniseDirectory dir
      listDirectory dir `shouldReturn` ["rider"]
  it "propagates staging cleanup fsync failure without changing an existing final" $
    withSystemTempDirectory "tdf-publication" $ \dir -> do
      let destination = dir </> "rider"
      BS.writeFile destination "original"
      calls <- newIORef (0::Int)
      let failThird fd = do
            n <- atomicModifyIORef' calls (\n -> (n+1,n+1))
            when (n==3) $ throwIO (userError "synthetic cleanup fsync failure")
            fileSynchronise fd
      publishCompleteFileWithSync failThird destination (`BS.hPut` "different") `shouldThrow` anyIOException
      BS.readFile destination `shouldReturn` "original"
      listDirectory dir `shouldReturn` ["rider"]
