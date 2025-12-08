{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerFuture where

import           Data.Text            (Text)
import           Servant

import           TDF.API.Future

-- | Shared helper to quickly craft stub responses.
stub :: Applicative m => Text -> Text -> m StubResponse
stub area endpoint = pure StubResponse
  { stubArea     = area
  , stubEndpoint = endpoint
  , stubStatus   = "planned"
  }

futureServer
  :: Applicative m
  => ServerT FutureAPI m
futureServer = accessStubs
          :<|> crmStubs
          :<|> schedulingStubs
          :<|> packagesStubs
          :<|> invoicingStubs
          :<|> inventoryStubs
          :<|> adminStubs
          :<|> crossCuttingStubs
  where
    accessStubs =    stub "access" "login-options"
                :<|> stub "access" "module-behaviour"
                :<|> stub "access" "session-policy"

    crmStubs =       stub "crm" "parties/list-columns"
                :<|> stub "crm" "parties/filters"
                :<|> stub "crm" "parties/detail-tabs"

    schedulingStubs =    stub "scheduling" "bookings/views"
                    :<|> stub "scheduling" "sessions/creation"
                    :<|> stub "scheduling" "rooms/features"

    packagesStubs =  stub "packages" "catalog"
                :<|> stub "packages" "purchase-flow"

    invoicingStubs = stub "invoicing" "composer"
                :<|> stub "invoicing" "status-flow"

    inventoryStubs = stub "inventory" "assets/metadata"
                :<|> stub "inventory" "assets/workflow"
                :<|> stub "inventory" "stock"

    adminStubs =     stub "admin" "seed"
                :<|> adminConsole

    adminConsole = pure AdminConsoleView
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

    crossCuttingStubs = stub "experience" "navigation"
                    :<|> stub "experience" "feedback"
                    :<|> stub "experience" "offline"
                    :<|> stub "experience" "design"
                    :<|> stub "experience" "auditing"
