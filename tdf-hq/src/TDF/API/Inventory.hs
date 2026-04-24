{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.Inventory where

import           Control.Applicative ((<|>))
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
                                    , fdFileCType
                                    )
import           Data.Char         (isControl)
import qualified Data.Text         as T
import           System.FilePath   (takeExtension, takeFileName)

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
    validateImageUpload nameTxt file
    pure AssetUploadForm
      { aufFile = file
      , aufName = nameTxt
      }
    where
      optionalText field mp =
        do
          mInput <- lookupSingleInput field mp
          case mInput of
            Nothing -> Right Nothing
            Just input -> normalizeInputText input

      normalizeInputText (Input _ rawValue) =
        let trimmed = T.strip rawValue
        in if T.null trimmed
             then Right Nothing
             else validateUploadName trimmed

      validateUploadName rawName
        | T.any isControl rawName =
            Left "Asset upload name must not contain control characters"
        | T.any isPathSeparator rawName =
            Left "Asset upload name must not contain path separators"
        | otherwise =
            Right (Just rawName)

      isPathSeparator ch = ch == '/' || ch == '\\'

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

      validateImageUpload nameTxt file = do
        allowedExts <- allowedImageExtensions (fdFileCType file)
        ext <- resolveImageExtension nameTxt file
        validateResolvedUploadNameLength nameTxt file
        let providedExts =
              filter (not . T.null)
                [ maybe "" imageExtension nameTxt
                , imageExtension (fdFileName file)
                ]
        case filter (`notElem` allowedExts) (ext : providedExts) of
          [] -> Right ()
          badExt : _
            | badExt `elem` allImageExtensions ->
                Left "Asset upload image extension must match its MIME type"
            | otherwise ->
                Left "Asset upload file name must end with .jpg, .jpeg, .png, .webp, or .gif"

      allowedImageExtensions rawContentType =
        case T.toLower (T.strip (fst (T.breakOn ";" rawContentType))) of
          "image/jpeg" -> Right [".jpg", ".jpeg"]
          "image/png"  -> Right [".png"]
          "image/webp" -> Right [".webp"]
          "image/gif"  -> Right [".gif"]
          _ ->
            Left "Asset upload must be a raster image (jpg, png, webp, or gif)"

      resolveImageExtension nameTxt file =
        let requestedExt = maybe "" imageExtension nameTxt
            fallbackExt = imageExtension (fdFileName file)
            ext = if T.null requestedExt then fallbackExt else requestedExt
        in if T.null ext
             then Left "Asset upload file name must include a supported image extension"
             else Right ext

      validateResolvedUploadNameLength nameTxt file =
        let fallbackName = nonEmptyBaseName (fdFileName file)
            nameWithExt = applyExtension (nameTxt <|> fallbackName) fallbackName
        in if T.length nameWithExt > maxAssetUploadFileNameChars
             then Left
               ( "Asset upload file name must be "
                   <> show maxAssetUploadFileNameChars
                   <> " characters or fewer"
               )
             else Right ()

      nonEmptyBaseName rawName =
        let baseName = T.pack (takeFileName (T.unpack (T.strip rawName)))
        in if T.null baseName then Nothing else Just baseName

      applyExtension mName fallback =
        let resolved = maybe "upload" T.strip mName
            extFromFallback = maybe "" imageExtension fallback
            extFromName = imageExtension resolved
        in if T.null extFromName && not (T.null extFromFallback)
             then resolved <> extFromFallback
             else resolved

      imageExtension =
        T.toLower . T.pack . takeExtension . T.unpack . T.strip

      allImageExtensions = [".jpg", ".jpeg", ".png", ".webp", ".gif"]
      maxAssetUploadFileNameChars = 218

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

type InventoryPublicAPI =
       "public" :> "assets" :> "qr" :> Capture "token" Text :> Get '[JSON] AssetDTO
  :<|> "public" :> "assets" :> "qr" :> Capture "token" Text :> "checkout" :> ReqBody '[JSON] AssetCheckoutRequest :> Post '[JSON] AssetCheckoutDTO
  :<|> "public" :> "assets" :> "qr" :> Capture "token" Text :> "checkin"  :> ReqBody '[JSON] AssetCheckinRequest  :> Post '[JSON] AssetCheckoutDTO
  :<|> "public" :> "assets" :> "qr" :> Capture "token" Text :> "upload"   :> MultipartForm Tmp AssetUploadForm :> Post '[JSON] AssetUploadDTO
