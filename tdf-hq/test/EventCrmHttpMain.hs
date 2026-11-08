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
import TDF.Auth (AuthedUser,authContext)
import TDF.DB (Env(..))
import TDF.DTO (SessionResponse)
import TDF.EventOperations.CrmAPI (EventCrmAPI)
import TDF.EventOperations.CrmServer (eventCrmServer)
import TDF.EventOperations.HttpTestConfig (httpTestConfig)
import TDF.API.SocialEventsAPI (SocialEventsAPI)
import TDF.Server.SocialEventsHandlers (socialEventsServer)
import TDF.Routes.EventTickets (PublicEventTicketsAPI)
import TDF.Server.EventTicketCheckout (publicEventTicketsServer)
import qualified TDF.ServerAuth as Auth

type TestAPI =
  (Header "Authorization" Text :> Header "Cookie" Text :> "session" :> Get '[JSON] (Maybe SessionResponse))
  :<|> (AuthProtect "bearer-token" :> (EventCrmAPI :<|> "social-events" :> SocialEventsAPI))
  :<|> PublicEventTicketsAPI

main :: IO ()
main = do
  guardValue <- getEnv "TDF_EVENT_CRM_DISPOSABLE_HTTP"
  if guardValue/="1" then fail "Disposable event CRM guard required" else pure ()
  dsn <- getEnv "TDF_EVENT_CRM_HTTP_DSN"
  if not ("host=127.0.0.1 " `BS.isPrefixOf` BS.pack dsn)
    || not ("dbname=tdf_event_crm_" `BS.isInfixOf` BS.pack dsn)
    || not ("_test" `BS.isSuffixOf` BS.pack dsn) then fail "Owned test database required" else pure ()
  runner <- getEnv "TDF_EVENT_CRM_HTTP_RUNNER"
  result <- runNoLoggingT $ withPostgresqlPool (BS.pack dsn) 8 $ \pool -> liftIO $ do
    let env=Env pool httpTestConfig
        api=Proxy :: Proxy TestAPI
        ctx=Proxy :: Proxy '[AuthHandler Request AuthedUser]
        sessionGet :<|> _ = Auth.sessionServer
        app=serveWithContext api (authContext env) $
          hoistServerWithContext api ctx (flip runReaderT env)
            (sessionGet :<|> (\user -> eventCrmServer user :<|> socialEventsServer user) :<|> publicEventTicketsServer)
    Warp.testWithApplicationSettings (Warp.setHost "127.0.0.1" Warp.defaultSettings)
      (pure app) $ \port -> rawSystem "node" [runner,show port]
  exitWith result
