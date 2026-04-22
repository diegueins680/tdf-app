{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerFuture where

import           Control.Monad.Except (MonadError)
import           Data.Text            (Text)
import           Servant

import           TDF.API.Future
import           TDF.Auth             (AuthedUser, hasStrictAdminAccess)

-- | Shared helper to quickly craft stub responses.
stub :: Applicative m => Text -> Text -> m StubResponse
stub area endpoint = pure StubResponse
  { stubArea     = area
  , stubEndpoint = endpoint
  , stubStatus   = "planned"
  }

futureServer
  :: MonadError ServerError m
  => AuthedUser
  -> ServerT FutureAPI m
futureServer user = accessStubs
          :<|> crmStubs
          :<|> schedulingStubs
          :<|> packagesStubs
          :<|> invoicingStubs
          :<|> inventoryStubs
          :<|> adminStubs
          :<|> crossCuttingStubs
  where
    adminStub area endpoint =
      requireFutureAdminAccess user *> stub area endpoint

    accessStubs =    adminStub "access" "login-options"
                :<|> adminStub "access" "module-behaviour"
                :<|> adminStub "access" "session-policy"

    crmStubs =       adminStub "crm" "parties/list-columns"
                :<|> adminStub "crm" "parties/filters"
                :<|> adminStub "crm" "parties/detail-tabs"

    schedulingStubs =    adminStub "scheduling" "bookings/views"
                    :<|> adminStub "scheduling" "sessions/creation"
                    :<|> adminStub "scheduling" "rooms/features"

    packagesStubs =  adminStub "packages" "catalog"
                :<|> adminStub "packages" "purchase-flow"

    invoicingStubs = adminStub "invoicing" "composer"
                :<|> adminStub "invoicing" "status-flow"

    inventoryStubs = adminStub "inventory" "assets/metadata"
                :<|> adminStub "inventory" "assets/workflow"
                :<|> adminStub "inventory" "stock"

    adminStubs =     adminStub "admin" "seed"
                :<|> adminConsole

    adminConsole = do
      requireFutureAdminAccess user
      pure $
        AdminConsoleView
          { status = "preview"
          , cards =
              [ AdminConsoleCard
                  { cardId = "user-management"
                  , title  = "Gestión de usuarios"
                  , body   =
                      [ "La asignación de roles se administra desde la pantalla de Parties."
                      , "Próximamente aquí se podrá crear usuarios de servicio y tokens API."
                      ]
                  }
              ]
          }

    crossCuttingStubs = adminStub "experience" "navigation"
                    :<|> adminStub "experience" "feedback"
                    :<|> adminStub "experience" "offline"
                    :<|> adminStub "experience" "design"
                    :<|> adminStub "experience" "auditing"

validateFutureAdminAccess :: AuthedUser -> Either ServerError ()
validateFutureAdminAccess user
  | hasStrictAdminAccess user = Right ()
  | otherwise = Left err403 { errBody = "Admin role required" }

requireFutureAdminAccess :: MonadError ServerError m => AuthedUser -> m ()
requireFutureAdminAccess user =
  either throwError pure (validateFutureAdminAccess user)
