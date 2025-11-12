{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Future where

import           Data.Aeson   (ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Servant

-- | Generic stub payload used while endpoints are being planned.
data StubResponse = StubResponse
  { stubArea     :: Text
  , stubEndpoint :: Text
  , stubStatus   :: Text
  } deriving stock (Show, Generic)

instance ToJSON StubResponse

-- | Minimal card metadata for the admin console preview.
data AdminConsoleCard = AdminConsoleCard
  { cardId :: Text
  , title  :: Text
  , body   :: [Text]
  } deriving stock (Show, Generic)

instance ToJSON AdminConsoleCard

-- | Wrapper payload for the admin console endpoint.
data AdminConsoleView = AdminConsoleView
  { status :: Text
  , cards  :: [AdminConsoleCard]
  } deriving stock (Show, Generic)

instance ToJSON AdminConsoleView

-- Access & session management discovery endpoints
type AccessStubAPI =
       "login-options" :> Get '[JSON] StubResponse
  :<|> "module-behaviour" :> Get '[JSON] StubResponse
  :<|> "session-policy" :> Get '[JSON] StubResponse

-- CRM / Parties discovery endpoints
type CrmStubAPI =
       "parties" :> "list-columns" :> Get '[JSON] StubResponse
  :<|> "parties" :> "filters" :> Get '[JSON] StubResponse
  :<|> "parties" :> "detail-tabs" :> Get '[JSON] StubResponse

-- Scheduling discovery endpoints
type SchedulingStubAPI =
       "bookings" :> "views" :> Get '[JSON] StubResponse
  :<|> "sessions" :> "creation" :> Get '[JSON] StubResponse
  :<|> "rooms"    :> "features" :> Get '[JSON] StubResponse

-- Packages discovery endpoints
type PackagesStubAPI =
       "catalog"  :> Get '[JSON] StubResponse
  :<|> "purchase-flow" :> Get '[JSON] StubResponse

-- Invoicing discovery endpoints
type InvoicingStubAPI =
       "composer" :> Get '[JSON] StubResponse
  :<|> "status-flow" :> Get '[JSON] StubResponse

-- Inventory discovery endpoints
type InventoryStubAPI =
       "assets"  :> "metadata" :> Get '[JSON] StubResponse
  :<|> "assets"  :> "workflow" :> Get '[JSON] StubResponse
  :<|> "stock"   :> Get '[JSON] StubResponse

-- Admin and platform discovery endpoints
type AdminStubAPI =
       "seed"    :> Get '[JSON] StubResponse
  :<|> "console" :> Get '[JSON] AdminConsoleView

-- Cross-cutting UI considerations
type CrossCuttingStubAPI =
       "navigation" :> Get '[JSON] StubResponse
  :<|> "feedback"   :> Get '[JSON] StubResponse
  :<|> "offline"    :> Get '[JSON] StubResponse
  :<|> "design"     :> Get '[JSON] StubResponse
  :<|> "auditing"   :> Get '[JSON] StubResponse

-- Aggregate API exposed under /stubs/*
type FutureAPI =
       "access"      :> AccessStubAPI
  :<|> "crm"         :> CrmStubAPI
  :<|> "scheduling"  :> SchedulingStubAPI
  :<|> "packages"    :> PackagesStubAPI
  :<|> "invoicing"   :> InvoicingStubAPI
  :<|> "inventory"   :> InventoryStubAPI
  :<|> "admin"       :> AdminStubAPI
  :<|> "experience"  :> CrossCuttingStubAPI
