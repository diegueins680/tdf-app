{-# LANGUAGE ScopedTypeVariables #-}
module TDF.App.Shutdown
  ( Shutdown, ShutdownFailure(..), runWithUnixShutdown, runShutdown
  , requestShutdown, registerServerStop, admitStartupEffect
  ) where

import Control.Concurrent
  ( Chan, MVar, ThreadId, forkFinally, forkIO, killThread, modifyMVar
  , modifyMVar_, newChan, newEmptyMVar, newMVar, putMVar, readChan
  , readMVar, tryPutMVar, withMVar, writeChan
  )
import Control.Exception
  ( Exception, SomeAsyncException, SomeException, bracket, finally
  , fromException, mask, onException, throwIO
  )
import Control.Monad (forM_, void, when)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT, sigTERM)
import System.Timeout (timeout)

-- Workers launched by existing starters remain process-owned. This supervisor
-- drains HTTP and joins initialization; it does not claim provider/worker drain.
data ShutdownFailure = ShutdownDeadlineExpired | StartupAdmissionClosed
                     | UnexpectedServerReturn | DuplicateServerRegistration
  deriving (Eq, Show)
instance Exception ShutdownFailure

data State = State Bool (Maybe (IO ()))
data Event = Prepared (Either SomeException (IO (), IO ()))
           | StartupDone (Either SomeException ())
           | ServerDone (Either SomeException ())
           | StopRequested
data Shutdown = Shutdown (MVar State) (MVar ()) (Chan Event)
type Task = (ThreadId, MVar (Either SomeException ()))

-- Signal handlers enqueue once; no network, cancellation or drain in the handler.
requestShutdown :: Shutdown -> IO ()
requestShutdown (Shutdown _ latch events) = do
  first <- tryPutMVar latch ()
  when first (writeChan events StopRequested)

-- Registration and accepted stop share a linearization point. If stop was
-- accepted before Warp registered its listener, close that listener immediately.
registerServerStop :: Shutdown -> IO () -> IO ()
registerServerStop (Shutdown state _ _) stop = do
  closeNow <- modifyMVar state $ \(State stopping previous) -> case previous of
    Just _ -> throwIO DuplicateServerRegistration
    Nothing -> pure (State stopping (Just stop), stopping)
  when closeNow stop

-- Only short publication/worker-start admission belongs here, never DB setup.
-- Already admitted workers may run until process termination; no worker join
-- guarantee is inferred from this critical section.
admitStartupEffect :: Shutdown -> IO a -> IO a
admitStartupEffect (Shutdown state _ _) effect =
  withMVar state $ \(State stopping _) ->
    if stopping then throwIO StartupAdmissionClosed else effect

installUnix :: Shutdown -> IO (IO ())
installUnix shutdown = do
  oldTerm <- installHandler sigTERM (Catch (requestShutdown shutdown)) Nothing
  oldInt <- installHandler sigINT (Catch (requestShutdown shutdown)) Nothing
    `onException` void (installHandler sigTERM oldTerm Nothing)
  pure $ do
    void (installHandler sigINT oldInt Nothing)
    void (installHandler sigTERM oldTerm Nothing)

runWithUnixShutdown :: Int -> (Shutdown -> IO (IO (), IO ())) -> IO ()
runWithUnixShutdown micros = runShutdown micros installUnix

-- Preparation (including configuration load), initialization and Warp failures
-- propagate to Main. The deadline bounds accepted-stop admission, initialization
-- cancellation and HTTP drainage together. Warp must use an unbounded internal
-- graceful wait so a timeout cannot masquerade as successful drainage.
runShutdown :: Int -> (Shutdown -> IO (IO ())) -> (Shutdown -> IO (IO (), IO ())) -> IO ()
runShutdown micros installer prepare = do
  when (micros <= 0) (throwIO ShutdownDeadlineExpired)
  state <- newMVar (State False Nothing)
  latch <- newEmptyMVar
  events <- newChan
  tasks <- newMVar []
  let shutdown = Shutdown state latch events
      launch :: IO a -> (Either SomeException a -> Event) -> IO Task
      launch action event = mask $ \restore -> do
        result <- newEmptyMVar
        tid <- forkFinally (restore action) $ \outcome -> do
          putMVar result (fmap (const ()) outcome)
          writeChan events (event outcome)
        let task = (tid,result)
        modifyMVar_ tasks (pure . (task:))
        pure task
      -- throwTo/killThread can block on an uninterruptible foreign call. A
      -- separate sender lets the supervisor classify its own deadline. On a
      -- deadline/fatal error Main exits nonzero, terminating remaining threads.
      cancelTask (tid,_) = void (forkIO (killThread tid))
      awaitCancelled (_,result) = do
        outcome <- readMVar result
        case outcome of
          Right () -> pure ()
          Left exception -> case fromException exception :: Maybe SomeAsyncException of
            Just _ -> pure ()
            Nothing -> case fromException exception :: Maybe ShutdownFailure of
              Just StartupAdmissionClosed -> pure ()
              _ -> throwIO exception
      checkedDeadline action = do
        completed <- timeout micros action
        case completed of
          Just () -> pure ()
          Nothing -> throwIO ShutdownDeadlineExpired
      acceptStop = modifyMVar state $ \(State _ stop) -> pure (State True stop,stop)
      stopPreparing task = checkedDeadline $ do
        cancelTask task
        void acceptStop
        awaitCancelled task
      stopServing startup server = checkedDeadline $ do
        -- Some worker starters do synchronous DB checks under admission.
        -- Cancel first so they can release the gate before we accept stop.
        cancelTask startup
        stop <- acceptStop
        forM_ stop id
        awaitCancelled startup
        readMVar (snd server) >>= either throwIO pure
      awaitServing startup server = do
        event <- readChan events
        case event of
          StopRequested -> stopServing startup server
          StartupDone (Left exception) -> throwIO exception
          StartupDone (Right ()) -> awaitServing startup server
          ServerDone (Left exception) -> throwIO exception
          ServerDone (Right ()) -> throwIO UnexpectedServerReturn
          Prepared _ -> throwIO UnexpectedServerReturn
      run = do
        preparing <- launch (prepare shutdown) Prepared
        first <- readChan events
        case first of
          StopRequested -> stopPreparing preparing
          Prepared (Left exception) -> throwIO exception
          Prepared (Right (serve,initialize)) -> do
            server <- launch serve ServerDone
            startup <- launch initialize StartupDone
            awaitServing startup server
          _ -> throwIO UnexpectedServerReturn
      cleanup = readMVar tasks >>= mapM_ cancelTask
  bracket (installer shutdown) id (const (run `finally` cleanup))
