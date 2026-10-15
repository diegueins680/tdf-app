module TDF.ShutdownSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Exception (IOException, finally, try)
import Test.Hspec
import System.Timeout (timeout)
import TDF.App.Shutdown (runWithInitialization)

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
