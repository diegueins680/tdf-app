{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DTO.SocialDiscoveryDTO where

import           Control.Monad (when)
import           Data.Aeson (FromJSON(..), Options(..), ToJSON(..), defaultOptions, genericParseJSON, genericToJSON)
import           Data.Aeson.Types (Parser)
import           Data.Char (toLower)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)

camelDrop :: Int -> String -> String
camelDrop n xs = case drop n xs of
  (c:cs) -> toLower c : cs
  []     -> []

data SocialDiscoveryReviewIn = SocialDiscoveryReviewIn
  { sdriStatus :: Text
  , sdriNotes  :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON SocialDiscoveryReviewIn where
  parseJSON value = do
    review <- genericParseJSON defaultOptions
      { fieldLabelModifier = camelDrop 4
      , rejectUnknownFields = True
      } value
    let status = T.toLower (T.strip (sdriStatus review))
    when (status `notElem` ["pending", "approved", "dismissed"]) $
      fail "status must be pending, approved, or dismissed"
    notes <- traverse validateSocialDiscoveryReviewNotes (sdriNotes review)
    pure review { sdriStatus = status, sdriNotes = notes }

validateSocialDiscoveryReviewNotes :: Text -> Parser Text
validateSocialDiscoveryReviewNotes raw =
  let trimmed = T.strip raw
  in if T.length trimmed > 2000
       then fail "notes must be 2000 characters or fewer"
       else pure trimmed

data SocialDiscoveryPostDTO = SocialDiscoveryPostDTO
  { sddId             :: Text
  , sddPlatform       :: Text
  , sddSourceHandle   :: Maybe Text
  , sddCaption        :: Maybe Text
  , sddPermalink      :: Maybe Text
  , sddMediaUrls      :: [Text]
  , sddPostedAt       :: Maybe UTCTime
  , sddFetchedAt      :: UTCTime
  , sddDetectedTerms  :: [Text]
  , sddReviewStatus   :: Text
  , sddReviewNotes    :: Maybe Text
  , sddReviewedAt     :: Maybe UTCTime
  } deriving (Show, Generic)

instance ToJSON SocialDiscoveryPostDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3, omitNothingFields = True }
