-- | POSIX HTTP shutdown only. Background workers and external effects are not drained here.
module TDF.App.Shutdown
  ( runManagedWarp
  , runWithInitialization
  ) where

import Control.Concurrent.Async (wait, waitEither, withAsync)
import Control.Exception (bracket, mask_)
import Control.Monad (forM_, void, when)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

-- | Server completion cancels unfinished initialization; initialization failure
-- propagates and cancels the server. Successful initialization keeps serving.
runWithInitialization :: IO () -> IO () -> IO ()
runWithInitialization initialize serve =
  withAsync serve $ \server ->
    withAsync initialize $ \startup -> do
      result <- waitEither startup server
      case result of
        Left () -> wait server
        Right () -> pure ()

-- | Once Warp installs its listener shutdown action, TERM and INT request the
-- same five-second graceful HTTP drain. Repeated signals do not close a reused
-- descriptor. SIGKILL remains the operator's force-stop mechanism.
runManagedWarp :: Warp.Settings -> IO () -> Application -> IO ()
runManagedWarp settings initialize application =
  bracket (newIORef []) restoreHandlers $ \previousHandlers -> do
    stopping <- newIORef False
    let install closeListener = mask_ $
          forM_ [sigTERM, sigINT] $ \signal -> do
            previous <- installHandler signal (Catch $ do
              first <- atomicModifyIORef' stopping (\old -> (True, not old))
              when first closeListener) Nothing
            modifyIORef' previousHandlers ((signal, previous) :)
        managed = Warp.setInstallShutdownHandler install $
          Warp.setGracefulShutdownTimeout (Just 5) settings
    runWithInitialization initialize (Warp.runSettings managed application)
  where
    restoreHandlers previousHandlers =
      readIORef previousHandlers >>= mapM_ (\(signal, previous) ->
        void $ installHandler signal previous Nothing)
