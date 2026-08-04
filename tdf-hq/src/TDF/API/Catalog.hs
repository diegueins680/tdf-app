{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Catalog (CatalogAPI, CatalogUpdateRequest(..), ExportConfig(..)) where

import Servant
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import TDF.API.DDEX (CatalogReleaseDTO)

-- | Catalog Management API
type CatalogAPI =
       "releases" :> Get '[JSON] [CatalogReleaseDTO]
  :<|> "releases" :> Capture "id" Int :> Get '[JSON] CatalogReleaseDTO
  :<|> "releases" :> Capture "id" Int :> ReqBody '[JSON] CatalogUpdateRequest :> Patch '[JSON] CatalogReleaseDTO
  :<|> "releases" :> Capture "id" Int :> "ddex-exports" :> ReqBody '[JSON] ExportConfig :> Post '[JSON] Text

data CatalogUpdateRequest = CatalogUpdateRequest
  { updateTitle :: Maybe Text
  , updateGenre :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogUpdateRequest
instance FromJSON CatalogUpdateRequest

data ExportConfig = ExportConfig
  { configPartnerId :: Int
  , configProfile :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ExportConfig
instance FromJSON ExportConfig
