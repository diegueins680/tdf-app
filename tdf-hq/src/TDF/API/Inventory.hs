{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module TDF.API.Inventory where

import           Data.Text         (Text)
import           Servant
import           Servant.Multipart (FileData, FromMultipart(..), MultipartForm, Tmp, lookupFile, lookupInput)
import qualified Data.Text         as T

import           TDF.API.Types

data AssetUploadForm = AssetUploadForm
  { aufFile :: FileData Tmp
  , aufName :: Maybe Text
  }

instance FromMultipart Tmp AssetUploadForm where
  fromMultipart multipart = do
    file <- lookupFile "file" multipart
    let nameTxt = case lookupInput "name" multipart of
          Right v -> Just (T.strip v)
          _       -> Nothing
    pure AssetUploadForm
      { aufFile = file
      , aufName = nameTxt
      }

type InventoryAPI =
       "assets"
         :> QueryParam "q" Text
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> Get '[JSON] (Page AssetDTO)
  :<|> "assets" :> ReqBody '[JSON] AssetCreate :> PostCreated '[JSON] AssetDTO
  :<|> "assets" :> "upload" :> MultipartForm Tmp AssetUploadForm :> Post '[JSON] AssetUploadDTO
  :<|> "assets" :> Capture "id" Text :> Get '[JSON] AssetDTO
  :<|> "assets" :> Capture "id" Text :> ReqBody '[JSON] AssetUpdate :> Patch '[JSON] AssetDTO
  :<|> "assets" :> Capture "id" Text :> DeleteNoContent
  :<|> "assets" :> Capture "id" Text :> "checkout" :> ReqBody '[JSON] AssetCheckoutRequest :> Post '[JSON] AssetCheckoutDTO
  :<|> "assets" :> Capture "id" Text :> "checkin"  :> ReqBody '[JSON] AssetCheckinRequest  :> Post '[JSON] AssetCheckoutDTO
  :<|> "assets" :> Capture "id" Text :> "history"  :> Get '[JSON] [AssetCheckoutDTO]
  :<|> "assets" :> Capture "id" Text :> "qr"       :> Post '[JSON] AssetQrDTO
  :<|> "assets" :> "qr" :> Capture "token" Text    :> Get '[JSON] AssetDTO
