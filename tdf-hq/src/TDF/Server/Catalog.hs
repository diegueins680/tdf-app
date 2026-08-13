{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.Catalog (catalogServer) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (runSqlPool)
import Servant
import TDF.API.Catalog
import TDF.API.DDEX (CatalogReleaseDTO)
import TDF.Auth (AuthedUser(..), validateModuleAccess, ModuleAccess(..))
import qualified TDF.Catalog.DB as CatalogDB
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(..))

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
  Env{..} <- ask
  liftIO $ runSqlPool (CatalogDB.listReleaseDTOs Nothing) envPool

getReleaseHandler :: AuthedUser -> Int -> AppM CatalogReleaseDTO
getReleaseHandler user releaseId = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  Env{..} <- ask
  result <- liftIO $ runSqlPool (CatalogDB.getReleaseDTO releaseId) envPool
  maybe (throwError err404 { errBody = "Catalog release not found" }) pure result

updateReleaseHandler :: AuthedUser -> Int -> CatalogUpdateRequest -> AppM CatalogReleaseDTO
updateReleaseHandler user releaseId CatalogUpdateRequest{..} = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  unlessCatalogEditor user
  let cleanTitle = T.strip <$> updateTitle
      cleanGenre = T.strip <$> updateGenre
  when (maybe False (\value -> T.null value || T.length value > 500 || T.any (== '\NUL') value) cleanTitle) $
    throwError err400 { errBody = "Release title must contain 1 to 500 safe characters" }
  when (maybe False (\value -> T.null value || T.length value > 160 || T.any (== '\NUL') value) cleanGenre) $
    throwError err400 { errBody = "Release genre must contain 1 to 160 safe characters" }
  when (cleanTitle == Nothing && cleanGenre == Nothing) $
    throwError err400 { errBody = "No supported catalog fields were provided" }
  Env{..} <- ask
  result <- liftIO $ runSqlPool (CatalogDB.updateReleaseMetadata releaseId cleanTitle cleanGenre) envPool
  maybe (throwError err404 { errBody = "Catalog release not found" }) pure result

createExportForReleaseHandler :: AuthedUser -> Int -> ExportConfig -> AppM Text
createExportForReleaseHandler user _ _ = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  unlessCatalogEditor user
  throwError err503
    { errBody = "DDEX export is disabled until a validated recipient profile and private package store are configured" }

unlessCatalogEditor :: AuthedUser -> AppM ()
unlessCatalogEditor user =
  when (not (any (`elem` auRoles user) [Admin, Manager, StudioManager])) $
    throwError err403 { errBody = "Catalog write access requires an authorized catalog operator" }
