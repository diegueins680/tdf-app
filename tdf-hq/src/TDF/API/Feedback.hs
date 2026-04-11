{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Feedback
  ( FeedbackAPI
  , FeedbackPayload(..)
  ) where

import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Generics             (Generic)
import           Servant
import           Servant.Multipart        ( FileData
                                          , FromMultipart(..)
                                          , fdInputName
                                          , MultipartData(inputs, files)
                                          , MultipartForm
                                          , Tmp
                                          , Input(..)
                                          )

type FeedbackAPI =
  "feedback" :>
    ( MultipartForm Tmp FeedbackPayload :> Post '[JSON] NoContent )

data FeedbackPayload = FeedbackPayload
  { fpTitle        :: Text
  , fpDescription  :: Text
  , fpCategory     :: Maybe Text
  , fpSeverity     :: Maybe Text
  , fpContactEmail :: Maybe Text
  , fpConsent      :: Bool
  , fpAttachment   :: Maybe (FileData Tmp)
  } deriving (Show, Generic)

instance FromMultipart Tmp FeedbackPayload where
  fromMultipart multipart = do
    title <- lookupText "title" multipart
    description <- lookupText "description" multipart
    consent <- optionalBool "consent" multipart
    category <- optionalText "category" multipart
    severity <- optionalText "severity" multipart
    contact <- optionalText "contactEmail" multipart
    attachment <- lookupFile "attachment" multipart
    pure FeedbackPayload
      { fpTitle        = T.strip title
      , fpDescription  = T.strip description
      , fpCategory     = fmap T.strip category
      , fpSeverity     = fmap T.strip severity
      , fpContactEmail = fmap T.strip contact
      , fpConsent      = consent
      , fpAttachment   = attachment
      }
    where
      lookupText name mp =
        case lookupSingleInput name mp of
          Left err -> Left err
          Right Nothing -> Left ("Missing field: " <> T.unpack name)
          Right (Just val) ->
            let txt = T.strip (inputValueText val)
            in if T.null txt then Left ("Missing field: " <> T.unpack name) else Right txt

      optionalText name mp =
        fmap (fmap inputValueText) (lookupSingleInput name mp)

      optionalBool name mp =
        case lookupSingleInput name mp of
          Left err -> Left err
          Right Nothing  -> Right False
          Right (Just val) -> parseBoolField name (inputValueText val)

      parseBoolField name raw =
        case T.toLower (T.strip raw) of
          "true" -> Right True
          "1" -> Right True
          "yes" -> Right True
          "on" -> Right True
          "si" -> Right True
          "sí" -> Right True
          "false" -> Right False
          "0" -> Right False
          "no" -> Right False
          "off" -> Right False
          _ -> Left ("Invalid field: " <> T.unpack name <> " must be a boolean")

      lookupFile name mp =
        case [file | file <- files mp, fdInputName file == name] of
          [] -> Right Nothing
          [file] -> Right (Just file)
          _ -> Left ("Duplicate file field: " <> T.unpack name)

      lookupSingleInput name mp =
        case filter (\(Input nm _) -> nm == name) (inputs mp) of
          [] -> Right Nothing
          [x] -> Right (Just x)
          _ -> Left ("Duplicate field: " <> T.unpack name)

      inputValueText (Input _ value) = value
