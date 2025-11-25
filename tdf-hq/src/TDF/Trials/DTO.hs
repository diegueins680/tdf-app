{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TDF.Trials.DTO where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)

data PreferredSlot = PreferredSlot
  { startAt :: UTCTime
  , endAt   :: UTCTime
  } deriving (Show, Generic)
instance ToJSON PreferredSlot
instance FromJSON PreferredSlot

data TrialRequestIn = TrialRequestIn
  { partyId   :: Maybe Int
  , subjectId :: Int
  , preferred :: [PreferredSlot]   -- up to 3
  , notes     :: Maybe Text
  , fullName  :: Maybe Text
  , email     :: Maybe Text
  , phone     :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON TrialRequestIn
instance FromJSON TrialRequestIn

data TrialRequestOut = TrialRequestOut
  { requestId   :: Int
  , status      :: Text
  } deriving (Show, Generic)
instance ToJSON TrialRequestOut
instance FromJSON TrialRequestOut

data TrialSlotDTO = TrialSlotDTO
  { subjectId  :: Int
  , teacherId  :: Int
  , teacherName :: Text
  , slots      :: [PreferredSlot]
  } deriving (Show, Generic)
instance ToJSON TrialSlotDTO
instance FromJSON TrialSlotDTO

data TrialAvailabilitySlotDTO = TrialAvailabilitySlotDTO
  { availabilityId :: Int
  , subjectId      :: Int
  , subjectName    :: Maybe Text
  , teacherId      :: Int
  , teacherName    :: Maybe Text
  , roomId         :: Text
  , roomName       :: Maybe Text
  , startAt        :: UTCTime
  , endAt          :: UTCTime
  , notes          :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON TrialAvailabilitySlotDTO
instance FromJSON TrialAvailabilitySlotDTO

data TrialAvailabilityUpsert = TrialAvailabilityUpsert
  { availabilityId :: Maybe Int
  , subjectId      :: Int
  , roomId         :: Text
  , startAt        :: UTCTime
  , endAt          :: UTCTime
  , notes          :: Maybe Text
  , teacherId      :: Maybe Int
  } deriving (Show, Generic)
instance ToJSON TrialAvailabilityUpsert
instance FromJSON TrialAvailabilityUpsert

data TrialAssignIn = TrialAssignIn
  { teacherId :: Int
  } deriving (Show, Generic)
instance ToJSON TrialAssignIn
instance FromJSON TrialAssignIn

data TrialScheduleIn = TrialScheduleIn
  { requestId :: Int
  , teacherId :: Int
  , startAt   :: UTCTime
  , endAt     :: UTCTime
  , roomId    :: Int
  } deriving (Show, Generic)
instance ToJSON TrialScheduleIn
instance FromJSON TrialScheduleIn

data SubjectBriefDTO = SubjectBriefDTO
  { subjectId :: Int
  , name      :: Text
  } deriving (Show, Generic)
instance ToJSON SubjectBriefDTO
instance FromJSON SubjectBriefDTO

data TeacherDTO = TeacherDTO
  { teacherId   :: Int
  , teacherName :: Text
  , subjects    :: [SubjectBriefDTO]
  } deriving (Show, Generic)
instance ToJSON TeacherDTO
instance FromJSON TeacherDTO

data ClassSessionDTO = ClassSessionDTO
  { classSessionId :: Int
  , teacherId      :: Int
  , teacherName    :: Maybe Text
  , subjectId      :: Int
  , subjectName    :: Maybe Text
  , studentId      :: Int
  , studentName    :: Maybe Text
  , startAt        :: UTCTime
  , endAt          :: UTCTime
  , status         :: Text
  , roomId         :: Maybe Text
  , roomName       :: Maybe Text
  , bookingId      :: Maybe Int
  , notes          :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON ClassSessionDTO
instance FromJSON ClassSessionDTO

data ClassSessionUpdate = ClassSessionUpdate
  { teacherId :: Maybe Int
  , subjectId :: Maybe Int
  , studentId :: Maybe Int
  , startAt   :: Maybe UTCTime
  , endAt     :: Maybe UTCTime
  , roomId    :: Maybe Int
  , bookingId :: Maybe Int
  , notes     :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON ClassSessionUpdate
instance FromJSON ClassSessionUpdate
