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
import           Servant.Multipart  (MultipartForm, Tmp, FromMultipart(..), lookupFile, lookupInput, FileData, Input(..))
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
    let folder = case lookupInput "folderId" multipart of
          Right v -> Just (T.strip v)
          _       -> Nothing
        nameTxt = case lookupInput "name" multipart of
          Right v -> Just (T.strip v)
          _       -> Nothing
        token = case lookupInput "accessToken" multipart of
          Right v -> Just (T.strip v)
          _       -> Nothing
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
