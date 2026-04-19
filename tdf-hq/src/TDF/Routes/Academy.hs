{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Routes.Academy
  ( AcademyAPI
  , EnrollReq(..)
  , ProgressReq(..)
  , ReferralClaimReq(..)
  , MicrocourseDTO(..)
  , LessonDTO(..)
  , NextCohortDTO(..)
  ) where

import           Data.Aeson (FromJSON(parseJSON), Options(..), ToJSON, defaultOptions, genericParseJSON)
import           Data.Aeson.Types (Parser)
import           Data.Char (isControl, isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Servant

data EnrollReq = EnrollReq
  { email        :: Text
  , role         :: Text
  , platform     :: Maybe Text
  , referralCode :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON EnrollReq
instance FromJSON EnrollReq where
  parseJSON value = do
    EnrollReq rawEmail rawRole rawPlatform rawReferralCode <-
      genericParseJSON strictObjectOptions value
    EnrollReq
      <$> requiredEmail "email" rawEmail
      <*> requiredLowerText "role" rawRole
      <*> pure (optionalNonBlank rawPlatform)
      <*> pure (T.toUpper <$> optionalNonBlank rawReferralCode)

data ProgressReq = ProgressReq
  { email :: Text
  , slug  :: Text
  , day   :: Int
  } deriving (Show, Generic)

instance FromJSON ProgressReq where
  parseJSON value = do
    ProgressReq rawEmail rawSlug rawDay <-
      genericParseJSON strictObjectOptions value
    ProgressReq
      <$> requiredEmail "email" rawEmail
      <*> requiredLowerText "slug" rawSlug
      <*> requiredPositiveDay rawDay
instance ToJSON ProgressReq

data ReferralClaimReq = ReferralClaimReq
  { email :: Text
  , code  :: Text
  } deriving (Show, Generic)

instance FromJSON ReferralClaimReq where
  parseJSON value = do
    ReferralClaimReq rawEmail rawCode <-
      genericParseJSON strictObjectOptions value
    ReferralClaimReq
      <$> requiredEmail "email" rawEmail
      <*> (T.toUpper <$> requiredNonBlank "code" rawCode)
instance ToJSON ReferralClaimReq

data LessonDTO = LessonDTO
  { lessonDay   :: Int
  , lessonTitle :: Text
  , lessonBody  :: Text
  } deriving (Show, Generic)

instance ToJSON LessonDTO

data MicrocourseDTO = MicrocourseDTO
  { mcSlug    :: Text
  , mcTitle   :: Text
  , mcSummary :: Maybe Text
  , lessons   :: [LessonDTO]
  } deriving (Show, Generic)

instance ToJSON MicrocourseDTO

data NextCohortDTO = NextCohortDTO
  { nextSlug     :: Text
  , nextTitle    :: Text
  , nextStartsAt :: UTCTime
  , nextEndsAt   :: UTCTime
  , nextSeatCap  :: Int
  , nextSeatsLeft :: Int
  } deriving (Show, Generic)

instance ToJSON NextCohortDTO

type AcademyAPI =
       "academy" :> "enroll" :> ReqBody '[JSON] EnrollReq :> Post '[JSON] NoContent
  :<|> "academy" :> "microcourse" :> Capture "slug" Text :> Get '[JSON] MicrocourseDTO
  :<|> "academy" :> "progress" :> ReqBody '[JSON] ProgressReq :> Post '[JSON] NoContent
  :<|> "referrals" :> "claim" :> ReqBody '[JSON] ReferralClaimReq :> Post '[JSON] NoContent
  :<|> "cohorts" :> "next" :> Get '[JSON] NextCohortDTO

strictObjectOptions :: Options
strictObjectOptions = defaultOptions { rejectUnknownFields = True }

requiredEmail :: String -> Text -> Parser Text
requiredEmail fieldName raw =
  let normalized = T.toLower (T.strip raw)
  in if T.null normalized
       then fail (fieldName <> " must not be blank")
       else
         if isValidAcademyEmail normalized
           then pure normalized
           else fail (fieldName <> " must be a valid email address")

requiredLowerText :: String -> Text -> Parser Text
requiredLowerText fieldName raw =
  T.toLower <$> requiredNonBlank fieldName raw

requiredNonBlank :: String -> Text -> Parser Text
requiredNonBlank fieldName raw =
  let trimmed = T.strip raw
  in if T.null trimmed
       then fail (fieldName <> " must not be blank")
       else pure trimmed

optionalNonBlank :: Maybe Text -> Maybe Text
optionalNonBlank raw =
  case T.strip <$> raw of
    Just trimmed | not (T.null trimmed) -> Just trimmed
    _ -> Nothing

requiredPositiveDay :: Int -> Parser Int
requiredPositiveDay dayNumber =
  if dayNumber <= 0
    then fail "day must be positive"
    else pure dayNumber

isValidAcademyEmail :: Text -> Bool
isValidAcademyEmail candidate =
  case T.splitOn "@" candidate of
    [local, domain] ->
      not (T.null local)
        && not (T.null domain)
        && T.any (== '.') domain
        && not (T.any (\ch -> isSpace ch || isControl ch) candidate)
    _ -> False
