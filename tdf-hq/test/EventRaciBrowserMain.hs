{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Database.Persist.Postgresql (withPostgresqlPool)
import Network.Wai (Request)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)
import System.Environment (getEnv)
import System.Exit (exitWith)
import System.Process (rawSystem)
import TDF.Auth (AuthedUser, authContext)
import TDF.DB (Env(..))
import TDF.DTO (SessionResponse)
import TDF.EventOperations.API (EventOperationsAPI)
import TDF.EventOperations.HttpTestConfig (httpTestConfig)
import TDF.EventOperations.Server (eventOperationsServer)
import qualified TDF.ServerAuth as Auth

-- Select the actual session getter, without exposing login/logout/preferences writes.
type BrowserAPI =
  (Header "Authorization" Text :> Header "Cookie" Text :> "session"
    :> Get '[JSON] (Maybe SessionResponse))
  :<|> (AuthProtect "bearer-token" :> EventOperationsAPI)

main :: IO ()
main = do
  guardValue <- getEnv "EVENT_RACI_DISPOSABLE_BROWSER_TEST"
  if guardValue /= "1" then fail "Disposable browser guard required" else pure ()
  dsn <- getEnv "EVENT_OPERATIONS_TEST_DSN"
  runner <- getEnv "EVENT_RACI_BROWSER_RUNNER"
  result <- runNoLoggingT $ withPostgresqlPool (BS.pack dsn) 8 $ \pool -> liftIO $ do
    let env = Env pool httpTestConfig
        api = Proxy :: Proxy BrowserAPI
        ctx = Proxy :: Proxy '[AuthHandler Request AuthedUser]
        sessionGet :<|> _ = Auth.sessionServer
        app = serveWithContext api (authContext env) $
          hoistServerWithContext api ctx (flip runReaderT env)
            (sessionGet :<|> eventOperationsServer)
    Warp.testWithApplicationSettings (Warp.setHost "127.0.0.1" Warp.defaultSettings)
      (pure app) $ \port -> rawSystem "node" [runner, show port]
  exitWith result
