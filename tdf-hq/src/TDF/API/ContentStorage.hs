{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module TDF.API.ContentStorage
  ( ContentStorageAPI, StorageAccountDTO(..), StorageDestinationUpdate(..), StorageFileDTO(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON, withObject, (.:))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant

data StorageAccountDTO = StorageAccountDTO
  { allowanceBytes :: Int64
  , usedBytes :: Int64
  , reservedBytes :: Int64
  , defaultDestination :: Maybe Text
  , revision :: Int64
  , legacyUsageVerified :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON StorageAccountDTO

data StorageFileDTO = StorageFileDTO
  { fileId :: UUID
  , fileName :: Text
  , byteSize :: Int64
  , status :: Text
  , visibility :: Text
  } deriving (Eq, Show, Generic)
instance ToJSON StorageFileDTO

data StorageDestinationUpdate = StorageDestinationUpdate
  { destination :: Text
  , expectedRevision :: Int64
  } deriving (Eq, Show)
instance FromJSON StorageDestinationUpdate where
  parseJSON = withObject "StorageDestinationUpdate" $ \value -> do
    if any (`notElem` ["destination", "expectedRevision"]) (KM.keys value)
      then fail "Unexpected storage destination field" else pure ()
    StorageDestinationUpdate <$> value .: "destination" <*> value .: "expectedRevision"

type ContentStorageAPI =
       "storage" :> "me" :> Get '[JSON] StorageAccountDTO
  :<|> "storage" :> "me" :> "destination" :> ReqBody '[JSON] StorageDestinationUpdate
         :> Put '[JSON] StorageAccountDTO
  :<|> "storage" :> "files" :> QueryParam "after" UUID :> QueryParam "limit" Int
         :> Get '[JSON] [StorageFileDTO]
  :<|> "storage" :> "files" :> Capture "fileId" UUID
         :> Get '[OctetStream] (Headers '[Header "Cache-Control" Text, Header "Content-Disposition" Text, Header "X-Content-Type-Options" Text] ByteString)
  :<|> "storage" :> "files" :> Capture "fileId" UUID :> DeleteNoContent
