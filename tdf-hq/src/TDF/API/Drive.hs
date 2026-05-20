{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module TDF.API.Drive where

import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Servant
import           Servant.Multipart  ( FileData
                                     , FromMultipart(..)
                                     , Input(..)
                                     , MultipartData(inputs, files)
                                     , MultipartForm
                                     , Tmp
                                     , fdInputName
                                     )
import           Data.Char          ( GeneralCategory(Format)
                                     , generalCategory
                                     , isAscii
                                     , isControl
                                     , isSpace
                                     )
import qualified Data.Text         as T

import           TDF.API.Types      ( DriveTokenExchangeRequest
                                     , DriveTokenRefreshRequest
                                     , DriveTokenResponse
                                     , DriveUploadDTO
                                     )

data DriveUploadForm = DriveUploadForm
  { duFile        :: FileData Tmp
  , duFolderId    :: Maybe Text
  , duName        :: Maybe Text
  , duAccessToken :: Maybe Text
  } deriving (Generic)

instance FromMultipart Tmp DriveUploadForm where
  fromMultipart multipart = do
    rejectUnexpectedParts multipart
    file <- lookupSingleFile "file" multipart
    folder <- optionalText "folderId" multipart
    nameTxt <- optionalText "name" multipart
    token <- optionalAccessToken "accessToken" multipart
    pure DriveUploadForm
      { duFile = file
      , duFolderId = folder
      , duName = nameTxt
      , duAccessToken = token
      }
    where
      optionalText name mp =
        case lookupSingleInput name mp of
          Left err -> Left err
          Right Nothing -> Right Nothing
          Right (Just (Input _ value)) ->
            let trimmed = T.strip value
            in if T.null trimmed
                then Left (T.unpack name <> " must not be blank")
                else Right (Just trimmed)

      optionalAccessToken name mp =
        case lookupSingleInput name mp of
          Left err -> Left err
          Right Nothing -> Right Nothing
          Right (Just (Input _ value)) ->
            let trimmed = T.strip value
            in if T.null trimmed
                then Left (T.unpack name <> " must not be blank")
                else Just <$> validateMultipartAccessToken name trimmed

      lookupSingleInput name mp =
        case filter (\(Input nm _) -> nm == name) (inputs mp) of
          [] -> Right Nothing
          [input] -> Right (Just input)
          _ -> Left ("Duplicate field: " <> T.unpack name)

      lookupSingleFile name mp =
        case [file | file <- files mp, fdInputName file == name] of
          [] -> Left ("Missing file field: " <> T.unpack name)
          [file] -> Right file
          _ -> Left ("Duplicate file field: " <> T.unpack name)

      rejectUnexpectedParts mp =
        case (unexpectedInputs, unexpectedFiles) of
          (fieldName : _, _) -> Left ("Unexpected field: " <> T.unpack fieldName)
          (_, fileName : _) -> Left ("Unexpected file field: " <> T.unpack fileName)
          _ -> Right ()
        where
          expectedInputs = ["folderId", "name", "accessToken"]
          expectedFiles = ["file"]
          unexpectedInputs =
            [ name
            | Input name _ <- inputs mp
            , name `notElem` expectedInputs
            ]
          unexpectedFiles =
            [ fdInputName file
            | file <- files mp
            , fdInputName file `notElem` expectedFiles
            ]

validateMultipartAccessToken :: Text -> Text -> Either String Text
validateMultipartAccessToken fieldName token
  | T.length token > 4096 =
      Left (fieldLabel <> " must be 4096 characters or fewer")
  | T.any isSpace token =
      Left (fieldLabel <> " must not contain whitespace")
  | T.any isControl token =
      Left (fieldLabel <> " must not contain control characters")
  | T.any isHiddenFormattingChar token =
      Left (fieldLabel <> " must not contain hidden formatting characters")
  | T.any (not . isAscii) token =
      Left (fieldLabel <> " must contain only ASCII characters")
  | otherwise =
      Right token
  where
    fieldLabel = T.unpack fieldName

isHiddenFormattingChar :: Char -> Bool
isHiddenFormattingChar ch =
  generalCategory ch == Format

type DriveAPI =
  "drive" :> "upload"
    :> Header "X-Goog-Access-Token" Text
    :> MultipartForm Tmp DriveUploadForm
    :> Post '[JSON] DriveUploadDTO
  :<|> "drive" :> "token"
    :> ReqBody '[JSON] DriveTokenExchangeRequest
    :> Post '[JSON] DriveTokenResponse
  :<|> "drive" :> "token" :> "refresh"
    :> ReqBody '[JSON] DriveTokenRefreshRequest
    :> Post '[JSON] DriveTokenResponse
