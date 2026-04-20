{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.Inventory where

import           Data.Text         (Text)
import           Servant
import           Servant.Multipart ( FileData
                                    , FromMultipart(..)
                                    , Input(..)
                                    , MultipartData(inputs, files)
                                    , MultipartForm
                                    , Tmp
                                    , fdInputName
                                    , fdFileName
                                    )
import qualified Data.Text         as T

import           TDF.API.Types

data AssetUploadForm = AssetUploadForm
  { aufFile :: FileData Tmp
  , aufName :: Maybe Text
  }

instance FromMultipart Tmp AssetUploadForm where
  fromMultipart multipart = do
    rejectUnexpectedParts multipart
    file <- lookupSingleFile "file" multipart
    nameTxt <- optionalText "name" multipart
    rejectAmbiguousFileName nameTxt file
    pure AssetUploadForm
      { aufFile = file
      , aufName = nameTxt
      }
    where
      optionalText field mp =
        fmap (>>= normalizeInputText) (lookupSingleInput field mp)

      normalizeInputText (Input _ rawValue) =
        let trimmed = T.strip rawValue
        in if T.null trimmed then Nothing else Just trimmed

      lookupSingleInput field mp =
        case filter (\(Input inputName _) -> inputName == field) (inputs mp) of
          [] -> Right Nothing
          [input] -> Right (Just input)
          _ -> Left ("Duplicate field: " <> T.unpack field)

      lookupSingleFile field mp =
        case [file | file <- files mp, fdInputName file == field] of
          [] -> Left ("Missing file field: " <> T.unpack field)
          [file] -> Right file
          _ -> Left ("Duplicate file field: " <> T.unpack field)

      rejectAmbiguousFileName nameTxt file =
        case (nameTxt, T.strip (fdFileName file)) of
          (Nothing, fileName) | T.null fileName ->
            Left "Either field name or uploaded file name must be provided"
          _ -> Right ()

      rejectUnexpectedParts mp =
        case (unexpectedInputs, unexpectedFiles) of
          (fieldName : _, _) -> Left ("Unexpected field: " <> T.unpack fieldName)
          (_, fileName : _) -> Left ("Unexpected file field: " <> T.unpack fileName)
          _ -> Right ()
        where
          expectedInputs = ["name"]
          expectedFiles = ["file"]
          unexpectedInputs =
            [ inputName
            | Input inputName _ <- inputs mp
            , inputName `notElem` expectedInputs
            ]
          unexpectedFiles =
            [ fdInputName file
            | file <- files mp
            , fdInputName file `notElem` expectedFiles
            ]

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
