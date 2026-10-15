{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (unless, void)
import Network.HTTP.Types (status200)
import Network.Wai (pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Posix.Signals (Handler (Ignore), installHandler, sigINT, sigTERM)
import TDF.App.Shutdown (runManagedWarp)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [port, mode] <- getArgs
  let initialize = (case mode of
        "startup-failure" -> threadDelay 100000 >> ioError (userError "synthetic startup failure")
        "startup-wait" -> threadDelay 30000000
        _ -> pure ()) `finally` putStrLn "startup-finished"
      settings = Warp.setBeforeMainLoop (putStrLn "listening") $
        Warp.setPort (read port) $ Warp.setHost "127.0.0.1" Warp.defaultSettings
      application request send = do
        case pathInfo request of
          ["hold"] -> putStrLn "request-started" >> threadDelay 1500000
          ["stuck"] -> putStrLn "request-started" >> threadDelay 30000000
          _ -> pure ()
        send (responseLBS status200 [("Content-Type", "text/plain")] "accepted-completed")
  -- Check scoped restoration too; all signals stay inside this owned child.
  void $ installHandler sigTERM Ignore Nothing
  void $ installHandler sigINT Ignore Nothing
  let restored = do
        term <- installHandler sigTERM Ignore Nothing
        int <- installHandler sigINT Ignore Nothing
        let ignored Ignore = True
            ignored _ = False
        unless (ignored term && ignored int) $ ioError (userError "signal handler restoration failed")
        putStrLn "handlers-restored"
  runManagedWarp settings initialize application `finally` restored
  putStrLn "server-finished"
