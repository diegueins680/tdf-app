{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TDF.Trials.DTO where

import GHC.Generics (Generic)
import Data.Aeson
  ( FromJSON (parseJSON)
  , Options
  , ToJSON
  , Value (Null)
  , defaultOptions
  , genericParseJSON
  , rejectUnknownFields
  , withObject
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)

strictObjectOptions :: Options
strictObjectOptions = defaultOptions { rejectUnknownFields = True }

rejectNullOptionalTrialDtoFields :: String -> [Text] -> Value -> Parser ()
rejectNullOptionalTrialDtoFields objectName fieldNames =
  withObject objectName $ \object ->
    let rejectNullOptionalField fieldName =
          case AesonKeyMap.lookup (AesonKey.fromText fieldName) object of
            Just Null -> fail (T.unpack fieldName <> " must be omitted instead of null")
            _         -> pure ()
    in mapM_ rejectNullOptionalField fieldNames

data PreferredSlot = PreferredSlot
  { startAt :: UTCTime
  , endAt   :: UTCTime
  } deriving (Eq, Show, Generic)
instance ToJSON PreferredSlot
instance FromJSON PreferredSlot where
  parseJSON = genericParseJSON strictObjectOptions

data TrialRequestIn = TrialRequestIn
  { partyId   :: Maybe Int -- must be omitted on the public endpoint
  , subjectId :: Int
  , preferred :: [PreferredSlot]   -- up to 3
  , notes     :: Maybe Text
  , fullName  :: Maybe Text
  , email     :: Maybe Text
  , phone     :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON TrialRequestIn
instance FromJSON TrialRequestIn where
  parseJSON value = do
    withObject "TrialRequestIn" rejectPublicPartyId value
    withObject "TrialRequestIn" rejectNullOptionalTrialFields value
    genericParseJSON strictObjectOptions value
    where
      rejectPublicPartyId object
        | AesonKeyMap.member (AesonKey.fromString "partyId") object =
            fail "TrialRequestIn.partyId must be omitted on the public endpoint"
        | otherwise = pure ()
      rejectNullOptionalTrialFields object =
        mapM_ rejectNullOptionalField ["notes", "fullName", "email", "phone"]
        where
          rejectNullOptionalField fieldName =
            case AesonKeyMap.lookup (AesonKey.fromString fieldName) object of
              Just Null -> fail (fieldName <> " must be omitted instead of null")
              _         -> pure ()

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
instance FromJSON TrialAvailabilityUpsert where
  parseJSON value = do
    withObject "TrialAvailabilityUpsert" rejectNullOptionalAvailabilityFields value
    genericParseJSON strictObjectOptions value
    where
      rejectNullOptionalAvailabilityFields object =
        mapM_ rejectNullOptionalField ["availabilityId", "notes", "teacherId"]
        where
          rejectNullOptionalField fieldName =
            case AesonKeyMap.lookup (AesonKey.fromString fieldName) object of
              Just Null -> fail (fieldName <> " must be omitted instead of null")
              _         -> pure ()

data TrialAssignIn = TrialAssignIn
  { teacherId :: Int
  } deriving (Show, Generic)
instance ToJSON TrialAssignIn
instance FromJSON TrialAssignIn where
  parseJSON = genericParseJSON strictObjectOptions

data TrialScheduleIn = TrialScheduleIn
  { requestId :: Int
  , teacherId :: Int
  , startAt   :: UTCTime
  , endAt     :: UTCTime
  , roomId    :: Int
  } deriving (Show, Generic)
instance ToJSON TrialScheduleIn
instance FromJSON TrialScheduleIn where
  parseJSON = genericParseJSON strictObjectOptions

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

data TeacherSubjectsUpdate = TeacherSubjectsUpdate
  { subjectIds :: [Int]
  } deriving (Show, Generic)
instance ToJSON TeacherSubjectsUpdate
instance FromJSON TeacherSubjectsUpdate where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON ClassSessionUpdate where
  parseJSON = withObject "ClassSessionUpdate" $ \object -> do
    let allowedKeys =
          [ "teacherId"
          , "subjectId"
          , "studentId"
          , "startAt"
          , "endAt"
          , "roomId"
          , "bookingId"
          , "notes"
          ]
        providedKeys =
          map AesonKey.toText (AesonKeyMap.keys object)
        unexpectedKeys =
          filter (`notElem` allowedKeys) providedKeys
        nullKeys =
          [ key
          | key <- allowedKeys
          , AesonKeyMap.lookup (AesonKey.fromText key) object == Just Null
          ]
    case unexpectedKeys of
      key:_ -> fail ("Unknown field in ClassSessionUpdate: " <> T.unpack key)
      [] -> case nullKeys of
        key:_ -> fail (T.unpack key <> " must be omitted instead of null")
        [] ->
          if null providedKeys
            then fail "ClassSessionUpdate must include at least one field"
            else
              ClassSessionUpdate
                <$> object .:? "teacherId"
                <*> object .:? "subjectId"
                <*> object .:? "studentId"
                <*> object .:? "startAt"
                <*> object .:? "endAt"
                <*> object .:? "roomId"
                <*> object .:? "bookingId"
                <*> object .:? "notes"

data StudentCreate = StudentCreate
  { fullName :: Text
  , email    :: Text
  , phone    :: Maybe Text
  , notes    :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON StudentCreate
instance FromJSON StudentCreate where
  parseJSON value = do
    rejectNullOptionalTrialDtoFields "StudentCreate" ["phone", "notes"] value
    genericParseJSON strictObjectOptions value

data StudentDTO = StudentDTO
  { studentId   :: Int
  , displayName :: Text
  , email       :: Maybe Text
  , phone       :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON StudentDTO
instance FromJSON StudentDTO

data StudentUpdate = StudentUpdate
  { displayName :: Maybe Text
  , email       :: Maybe Text
  , phone       :: Maybe Text
  , notes       :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON StudentUpdate
instance FromJSON StudentUpdate where
  parseJSON value = do
    rejectNullOptionalTrialDtoFields
      "StudentUpdate"
      ["displayName", "email", "phone", "notes"]
      value
    payload@(StudentUpdate displayNameValue emailValue phoneValue notesValue) <-
      genericParseJSON strictObjectOptions value
    case (displayNameValue, emailValue, phoneValue, notesValue) of
      (Nothing, Nothing, Nothing, Nothing) ->
        fail "StudentUpdate must include at least one field"
      _ ->
        pure payload

data TeacherStudentLinkIn = TeacherStudentLinkIn
  { studentId :: Int
  } deriving (Show, Generic)
instance ToJSON TeacherStudentLinkIn
instance FromJSON TeacherStudentLinkIn where
  parseJSON = genericParseJSON strictObjectOptions
