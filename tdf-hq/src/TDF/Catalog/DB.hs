{-# LANGUAGE OverloadedStrings #-}

module TDF.Catalog.DB
  ( listReleaseDTOs
  , getReleaseDTO
  , updateReleaseMetadata
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql)

import TDF.API.DDEX (CatalogReleaseDTO(..))

type ReleaseRow =
  ( Single Int64
  , Single Text
  , Single Text
  , Single (Maybe Text)
  , Single (Maybe UTCTime)
  )

listReleaseDTOs :: Maybe Int -> SqlPersistT IO [CatalogReleaseDTO]
listReleaseDTOs mDocumentId = do
  rows <- case mDocumentId of
    Nothing -> rawSql (baseSelect <> "ORDER BY r.created_at DESC") []
    Just documentId -> rawSql (baseSelect <> documentFilter) [PersistInt64 (fromIntegral documentId)]
  pure (map rowToDTO (rows :: [ReleaseRow]))
  where
    baseSelect =
      "SELECT r.id, r.title, r.release_type, "
        <> "(SELECT ci.value FROM catalog_identifier ci "
        <> " WHERE ci.entity_type = 'Release' AND ci.entity_id = r.id "
        <> " AND ci.scheme IN ('UPC','EAN') ORDER BY ci.id LIMIT 1), "
        <> "r.release_date FROM catalog_release r "
    documentFilter =
      "WHERE EXISTS (SELECT 1 FROM catalog_source_link sl "
        <> "WHERE sl.entity_type = 'Release' AND sl.entity_id = r.id "
        <> "AND sl.ddex_document_id = ?) ORDER BY r.created_at DESC"

getReleaseDTO :: Int -> SqlPersistT IO (Maybe CatalogReleaseDTO)
getReleaseDTO releaseId = do
  rows <- rawSql
    ( "SELECT r.id, r.title, r.release_type, "
      <> "(SELECT ci.value FROM catalog_identifier ci "
      <> " WHERE ci.entity_type = 'Release' AND ci.entity_id = r.id "
      <> " AND ci.scheme IN ('UPC','EAN') ORDER BY ci.id LIMIT 1), "
      <> "r.release_date FROM catalog_release r WHERE r.id = ?"
    )
    [PersistInt64 (fromIntegral releaseId)]
  pure $ case (rows :: [ReleaseRow]) of
    row : _ -> Just (rowToDTO row)
    [] -> Nothing

updateReleaseMetadata :: Int -> Maybe Text -> Maybe Text -> SqlPersistT IO (Maybe CatalogReleaseDTO)
updateReleaseMetadata releaseId mTitle mGenre = do
  case fmap T.strip mTitle of
    Just title -> rawExecute
      "UPDATE catalog_release SET title = ?, updated_at = NOW() WHERE id = ?"
      [PersistText title, PersistInt64 (fromIntegral releaseId)]
    Nothing -> pure ()
  case fmap T.strip mGenre of
    Just genre -> rawExecute
      "UPDATE catalog_release SET genre = ?, updated_at = NOW() WHERE id = ?"
      [PersistText genre, PersistInt64 (fromIntegral releaseId)]
    Nothing -> pure ()
  getReleaseDTO releaseId

rowToDTO :: ReleaseRow -> CatalogReleaseDTO
rowToDTO
  ( Single releaseId
  , Single title
  , Single releaseType
  , Single upc
  , Single releaseDate
  ) = CatalogReleaseDTO
      { catalogReleaseDtoId = fromIntegral releaseId
      , catalogReleaseDtoTitle = title
      , catalogReleaseDtoType = releaseType
      , catalogReleaseDtoUpc = upc
      , catalogReleaseDtoReleaseDate = releaseDate
      }
