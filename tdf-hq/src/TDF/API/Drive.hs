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
import           Servant.Multipart  (MultipartForm, Tmp, FromMultipart(..), lookupFile, lookupInput, FileData)
import qualified Data.Text         as T

import           TDF.API.Types      (DriveUploadDTO)

data DriveUploadForm = DriveUploadForm
  { duFile        :: FileData Tmp
  , duFolderId    :: Maybe Text
  , duName        :: Maybe Text
  , duAccessToken :: Maybe Text
  } deriving (Generic)

instance FromMultipart Tmp DriveUploadForm where
  fromMultipart multipart = do
    file <- lookupFile "file" multipart
    let folder = either (const Nothing) (Just . T.strip) (lookupInput "folderId" multipart)
        nameTxt = either (const Nothing) (Just . T.strip) (lookupInput "name" multipart)
        token = either (const Nothing) (Just . T.strip) (lookupInput "accessToken" multipart)
    pure DriveUploadForm
      { duFile = file
      , duFolderId = folder
      , duName = nameTxt
      , duAccessToken = token
      }

type DriveAPI =
  "drive" :> "upload"
    :> Header "X-Goog-Access-Token" Text
    :> MultipartForm Tmp DriveUploadForm
    :> Post '[JSON] DriveUploadDTO
