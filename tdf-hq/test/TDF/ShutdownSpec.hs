module TDF.ShutdownSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Exception (AsyncException (ThreadKilled), IOException, finally, throwIO, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Test.Hspec
import System.Timeout (timeout)
import TDF.App.Shutdown (runWithInitialization)
import TDF.App.StartupRetry (retryStartup)

spec :: Spec
spec = describe "HTTP startup supervision" $ do
  it "propagates startup failure and cancels the running server" $ do
    entered <- newEmptyMVar
    cancelled <- newEmptyMVar
    never <- newEmptyMVar
    result <- try (runWithInitialization
      (takeMVar entered >> ioError (userError "startup failed"))
      ((putMVar entered () >> takeMVar never) `finally` putMVar cancelled ()))
    (result :: Either IOException ()) `shouldSatisfy` either (const True) (const False)
    timeout 1000000 (takeMVar cancelled) `shouldReturn` Just ()
  it "propagates server failure and cancels unfinished initialization" $ do
    entered <- newEmptyMVar
    cancelled <- newEmptyMVar
    never <- newEmptyMVar
    result <- try (runWithInitialization
      ((putMVar entered () >> takeMVar never) `finally` putMVar cancelled ())
      (takeMVar entered >> ioError (userError "listener failed")))
    (result :: Either IOException ()) `shouldSatisfy` either (const True) (const False)
    timeout 1000000 (takeMVar cancelled) `shouldReturn` Just ()
  it "cancels unfinished initialization when the server stops normally" $ do
    entered <- newEmptyMVar
    cancelled <- newEmptyMVar
    never <- newEmptyMVar
    runWithInitialization
      ((putMVar entered () >> takeMVar never) `finally` putMVar cancelled ())
      (takeMVar entered)
    timeout 1000000 (takeMVar cancelled) `shouldReturn` Just ()
  it "keeps serving after successful initialization" $ do
    initialized <- newEmptyMVar
    release <- newEmptyMVar
    withAsync (runWithInitialization (putMVar initialized ()) (takeMVar release)) $ \running -> do
      takeMVar initialized
      status <- poll running
      status `shouldSatisfy` maybe True (const False)
      putMVar release ()
      timeout 1000000 (wait running) `shouldReturn` Just ()
  it "retries synchronous startup failures and returns the recovered value" $ do
    attempts <- newIORef (0 :: Int)
    pauses <- newIORef []
    let operation = do
          modifyIORef' attempts (+1)
          count <- readIORef attempts
          if count < 3 then ioError (userError "retryable") else pure count
    retryStartup 2 (\remaining -> modifyIORef' pauses (++[remaining])) operation `shouldReturn` 3
    readIORef pauses `shouldReturn` [2, 1]
  it "propagates the final synchronous startup failure" $ do
    attempts <- newIORef (0 :: Int)
    let operation = modifyIORef' attempts (+1) >> ioError (userError "exhausted")
    retryStartup 1 (const $ pure ()) operation `shouldThrow` anyIOException
    readIORef attempts `shouldReturn` 2
  it "never retries cancellation raised inside a startup attempt" $ do
    pauses <- newIORef (0 :: Int)
    result <- try (retryStartup 5 (\_ -> modifyIORef' pauses (+1)) (throwIO ThreadKilled))
    (result :: Either AsyncException ()) `shouldBe` Left ThreadKilled
    readIORef pauses `shouldReturn` 0
  it "never retries cancellation raised while pausing between attempts" $ do
    attempts <- newIORef (0 :: Int)
    let operation = modifyIORef' attempts (+1) >> ioError (userError "retryable")
    result <- try (retryStartup 5 (const $ throwIO ThreadKilled) operation)
    (result :: Either AsyncException ()) `shouldBe` Left ThreadKilled
    readIORef attempts `shouldReturn` 1
