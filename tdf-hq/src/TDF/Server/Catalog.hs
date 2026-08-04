{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.Catalog (catalogServer) where

import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import Servant
import TDF.API.Catalog
import TDF.API.DDEX (CatalogReleaseDTO)
import TDF.Auth (AuthedUser, validateModuleAccess, ModuleAccess(..))
import TDF.DB (Env)

type AppM = ReaderT Env Handler

catalogServer :: AuthedUser -> ServerT CatalogAPI AppM
catalogServer user =
       listReleasesHandler user
  :<|> getReleaseHandler user
  :<|> updateReleaseHandler user
  :<|> createExportForReleaseHandler user

listReleasesHandler :: AuthedUser -> AppM [CatalogReleaseDTO]
listReleasesHandler user = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  pure [] -- TODO: Query DB

getReleaseHandler :: AuthedUser -> Int -> AppM CatalogReleaseDTO
getReleaseHandler user _ = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  throwError err501 { errBody = "Not Implemented: Get Release" }

updateReleaseHandler :: AuthedUser -> Int -> CatalogUpdateRequest -> AppM CatalogReleaseDTO
updateReleaseHandler user _ _ = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  throwError err501 { errBody = "Not Implemented: Update Release" }

createExportForReleaseHandler :: AuthedUser -> Int -> ExportConfig -> AppM Text
createExportForReleaseHandler user _ _ = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  pure "export-id-placeholder"
