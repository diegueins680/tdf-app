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

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Servant

data EnrollReq = EnrollReq
  { email        :: Text
  , role         :: Text
  , platform     :: Maybe Text
  , referralCode :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON EnrollReq
instance ToJSON EnrollReq

data ProgressReq = ProgressReq
  { email :: Text
  , slug  :: Text
  , day   :: Int
  } deriving (Show, Generic)

instance FromJSON ProgressReq
instance ToJSON ProgressReq

data ReferralClaimReq = ReferralClaimReq
  { email :: Text
  , code  :: Text
  } deriving (Show, Generic)

instance FromJSON ReferralClaimReq
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
