{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.ShutdownSpec (spec) where

import Control.Concurrent
import Control.Exception
import Control.Monad (replicateM_)
import System.IO.Error (ioeGetErrorString)
import Data.IORef
import System.Timeout (timeout)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status200, hConnection)
import qualified Network.Socket as Socket
import Test.Hspec
import TDF.App.DatabaseRetry (retryDatabaseConnection)
import TDF.App.Shutdown

noSignals :: Shutdown -> IO (IO ())
noSignals _ = pure (pure ())

finish :: IO a -> IO a
finish action = timeout 3000000 action >>= maybe (throwIO (userError "shutdown test hung")) pure

spec :: Spec
spec = describe "supervised shutdown" $ do
  it "cancels blocked preparation before any listener or worker is created" $ finish $ do
    started <- newEmptyMVar
    cleaned <- newEmptyMVar
    done <- newEmptyMVar
    _ <- forkFinally (runShutdown 1000000 noSignals $ \control ->
      (putMVar started control >> threadDelay 10000000 >> pure (pure (),pure ()))
        `finally` putMVar cleaned ()) (putMVar done)
    readMVar started >>= requestShutdown
    readMVar done >>= either throwIO pure
    readMVar cleaned

  it "cancels initialization while awaiting successful HTTP drainage" $ finish $ do
    initialized <- newEmptyMVar
    stopping <- newEmptyMVar
    drained <- newEmptyMVar
    cancelled <- newEmptyMVar
    done <- newEmptyMVar
    _ <- forkFinally (runShutdown 1000000 noSignals $ \control -> pure
      (registerServerStop control (putMVar stopping ()) >> readMVar drained
      ,(putMVar initialized control >> threadDelay 10000000) `finally` putMVar cancelled ())) (putMVar done)
    readMVar initialized >>= requestShutdown
    readMVar stopping
    readMVar cancelled
    isEmptyMVar done `shouldReturn` True
    putMVar drained ()
    readMVar done >>= either throwIO pure

  it "closes a listener registered after stop was accepted" $ finish $ do
    controlBox <- newEmptyMVar
    closed <- newEmptyMVar
    registered <- newEmptyMVar
    cancelled <- newEmptyMVar
    done <- newEmptyMVar
    _ <- forkFinally (runShutdown 1000000 noSignals $ \control -> pure
      (readMVar registered >> registerServerStop control (putMVar closed ()) >> readMVar closed
      ,(putMVar controlBox control >> threadDelay 10000000) `finally` putMVar cancelled ())) (putMVar done)
    control <- readMVar controlBox
    requestShutdown control
    -- Let the supervisor accept stop and cancel initialization before exposing
    -- the delayed listener. Registration itself must then close it.
    readMVar cancelled
    let awaitAccepted = do
          admission <- try (admitStartupEffect control (pure ()))
          case admission of
            Left StartupAdmissionClosed -> pure ()
            Left other -> throwIO other
            Right () -> threadDelay 1000 >> awaitAccepted
    awaitAccepted
    putMVar registered ()
    readMVar done >>= either throwIO pure
    readMVar closed

  it "coalesces repeated stop requests and closes the listener once" $ finish $ do
    controlBox <- newEmptyMVar
    closed <- newEmptyMVar
    calls <- newIORef (0::Int)
    done <- newEmptyMVar
    _ <- forkFinally (runShutdown 1000000 noSignals $ \control -> pure
      (registerServerStop control (modifyIORef' calls (+1) >> putMVar closed ()) >> readMVar closed
      ,putMVar controlBox control)) (putMVar done)
    control <- readMVar controlBox
    replicateM_ 20 (requestShutdown control)
    readMVar done >>= either throwIO pure
    readIORef calls `shouldReturn` 1

  it "reports a drain deadline as failure instead of successful server completion" $ finish $ do
    controlBox <- newEmptyMVar
    closed <- newEmptyMVar
    done <- newEmptyMVar
    _ <- forkFinally (runShutdown 30000 noSignals $ \control -> pure
      (registerServerStop control (putMVar closed ()) >> threadDelay 10000000
      ,putMVar controlBox control)) (putMVar done)
    readMVar controlBox >>= requestShutdown
    readMVar closed
    result <- readMVar done
    case result of
      Left e -> fromException e `shouldBe` Just ShutdownDeadlineExpired
      Right () -> expectationFailure "expired drain reported success"

  it "preserves initialization failure and stops the server" $ finish $ do
    stopped <- newEmptyMVar
    entered <- newEmptyMVar
    (runShutdown 1000000 noSignals $ \_ -> pure
      ((putMVar entered () >> threadDelay 10000000) `finally` putMVar stopped ()
      ,readMVar entered >> throwIO (userError "startup failed")))
      `shouldThrow` (\(e::IOException) -> ioeGetErrorString e=="startup failed")
    readMVar stopped

  it "rejects publication after accepted stop" $ finish $ do
    controlBox <- newEmptyMVar
    closed <- newEmptyMVar
    drain <- newEmptyMVar
    done <- newEmptyMVar
    _ <- forkFinally (runShutdown 1000000 noSignals $ \control -> pure
      (registerServerStop control (putMVar closed ()) >> readMVar drain,putMVar controlBox control)) (putMVar done)
    control <- readMVar controlBox
    requestShutdown control
    readMVar closed
    admitStartupEffect control (expectationFailure "late publication") `shouldThrow` (==StartupAdmissionClosed)
    putMVar drain ()
    readMVar done >>= either throwIO pure

  it "does not swallow cancellation as a synchronous startup retry" $ finish $ do
    started <- newEmptyMVar
    done <- newEmptyMVar
    tid <- forkFinally (retryDatabaseConnection 1 (putMVar started () >> threadDelay 10000000)) (putMVar done)
    readMVar started
    killThread tid
    result <- readMVar done
    case result of
      Left e -> fromException e `shouldBe` Just ThreadKilled
      Right _ -> expectationFailure "cancellation became retryable startup error"

  it "rejects unrequested server return and propagates preparation failure" $ finish $ do
    (runShutdown 1000000 noSignals $ \_ -> pure (pure (),threadDelay 10000000))
      `shouldThrow` (==UnexpectedServerReturn)
    (runShutdown 1000000 noSignals $ \_ -> throwIO (userError "invalid configuration"))
      `shouldThrow` (\(e::IOException) -> ioeGetErrorString e=="invalid configuration")

  it "drains a real accepted Warp request before reporting clean shutdown" $ finish $
    bracket Warp.openFreePort (Socket.close . snd) $ \(port,socket) ->
      bracket (HTTP.newManager HTTP.defaultManagerSettings) HTTP.closeManager $ \manager -> do
        controlBox <- newEmptyMVar
        ready <- newEmptyMVar
        entered <- newEmptyMVar
        release <- newEmptyMVar
        closed <- newEmptyMVar
        serverDone <- newEmptyMVar
        responseDone <- newEmptyMVar
        let app _ respond = do
              putMVar entered ()
              readMVar release
              respond (Wai.responseLBS status200 [(hConnection,"close")] "accepted")
        _ <- forkFinally (runShutdown 1000000 noSignals $ \control -> do
          putMVar controlBox control
          let settings = Warp.setBeforeMainLoop (putMVar ready ()) $
                Warp.setInstallShutdownHandler (\close ->
                  registerServerStop control (close >> putMVar closed ())) $
                Warp.setGracefulShutdownTimeout Nothing Warp.defaultSettings
          pure (Warp.runSettingsSocket settings socket app,pure ())) (putMVar serverDone)
        readMVar ready
        request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/")
        _ <- forkFinally (HTTP.httpLbs request manager) (putMVar responseDone)
        readMVar entered
        readMVar controlBox >>= requestShutdown
        readMVar closed
        isEmptyMVar serverDone `shouldReturn` True
        putMVar release ()
        response <- readMVar responseDone >>= either throwIO pure
        HTTP.responseBody response `shouldBe` "accepted"
        readMVar serverDone >>= either throwIO pure

  it "cancels initialization that is blocked inside worker admission" $ finish $ do
    entered <- newEmptyMVar
    blocked <- newEmptyMVar
    cancelled <- newEmptyMVar
    closed <- newEmptyMVar
    done <- newEmptyMVar
    _ <- forkFinally (runShutdown 100000 noSignals $ \control -> pure
      (registerServerStop control (putMVar closed ()) >> readMVar closed
      ,(admitStartupEffect control (putMVar entered control >> readMVar blocked))
        `finally` putMVar cancelled ())) (putMVar done)
    readMVar entered >>= requestShutdown
    readMVar done >>= either throwIO pure
    readMVar cancelled
