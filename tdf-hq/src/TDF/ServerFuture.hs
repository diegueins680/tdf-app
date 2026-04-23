{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerFuture where

import           Control.Monad.Except (MonadError)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Servant

import           TDF.API.Future
import           TDF.Auth             (AuthedUser, hasStrictAdminAccess)

-- | Shared helper to quickly craft stub responses.
stub
  :: MonadError ServerError m
  => Text
  -> Text
  -> m StubResponse
stub rawArea rawEndpoint = do
  (area, endpoint) <-
    either throwError pure (validateFutureStubMetadata rawArea rawEndpoint)
  pure $
    StubResponse
      { stubArea        = area
      , stubEndpoint    = endpoint
      , stubStatus      = "planned"
      , stubImplemented = False
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

validateFutureStubMetadata :: Text -> Text -> Either ServerError (Text, Text)
validateFutureStubMetadata rawArea rawEndpoint = do
  area <- validateFutureStubArea rawArea
  endpoint <- validateFutureStubEndpoint rawEndpoint
  pure (area, endpoint)

validateFutureStubArea :: Text -> Either ServerError Text
validateFutureStubArea rawArea
  | rawArea /= area = invalidFutureStubMetadata
  | validFutureStubSlug area = Right area
  | otherwise = invalidFutureStubMetadata
  where
    area = T.strip rawArea

validateFutureStubEndpoint :: Text -> Either ServerError Text
validateFutureStubEndpoint rawEndpoint
  | rawEndpoint /= endpoint = invalidFutureStubMetadata
  | validFutureStubPath endpoint = Right endpoint
  | otherwise = invalidFutureStubMetadata
  where
    endpoint = T.strip rawEndpoint

validFutureStubPath :: Text -> Bool
validFutureStubPath endpoint =
  not (T.null endpoint)
    && T.length endpoint <= 128
    && all validFutureStubSlug (T.splitOn "/" endpoint)

validFutureStubSlug :: Text -> Bool
validFutureStubSlug slug =
  not (T.null slug)
    && T.length slug <= 64
    && T.all validFutureStubSlugChar slug

validFutureStubSlugChar :: Char -> Bool
validFutureStubSlugChar ch =
  (ch >= 'a' && ch <= 'z')
    || (ch >= '0' && ch <= '9')
    || ch == '-'

invalidFutureStubMetadata :: Either ServerError a
invalidFutureStubMetadata =
  Left err500 { errBody = "Invalid future stub metadata" }
