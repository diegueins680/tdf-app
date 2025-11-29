{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
    let category    = optionalText "category" multipart
        severity    = optionalText "severity" multipart
        contact     = optionalText "contactEmail" multipart
        consent     = parseBool "consent" multipart
        attachment  = lookupFile "attachment" multipart
    pure FeedbackPayload
      { fpTitle        = T.strip title
      , fpDescription  = T.strip description
      , fpCategory     = fmap T.strip =<< category
      , fpSeverity     = fmap T.strip =<< severity
      , fpContactEmail = fmap T.strip =<< contact
      , fpConsent      = consent
      , fpAttachment   = attachment
      }
    where
      lookupText name mp =
        case findInput name mp of
          Nothing -> Left ("Missing field: " <> T.unpack name)
          Just val ->
            let txt = T.strip (inputValueText val)
            in if T.null txt then Left ("Missing field: " <> T.unpack name) else Right txt

      optionalText name mp = inputValueText <$> findInput name mp

      parseBool name mp =
        case findInput name mp of
          Nothing   -> False
          Just val  ->
            let txt = T.toLower (T.strip (inputValueText val))
            in txt `elem` ["true", "1", "yes", "on", "si", "sí"]

      lookupFile name mp = lookup name [(fdInputName f, f) | f <- files mp]

      findInput name mp =
        case filter (\(Input nm _) -> nm == name) (inputs mp) of
          []      -> Nothing
          (x : _) -> Just x

      inputValueText (Input _ value) = value
