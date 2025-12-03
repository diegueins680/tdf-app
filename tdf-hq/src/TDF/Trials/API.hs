{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TDF.Trials.API where

import Servant
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import TDF.Trials.DTO

type PublicTrialsAPI =
       "signup" :> ReqBody '[JSON] SignupIn :> Post '[JSON] SignupOut
  :<|> "interests" :> ReqBody '[JSON] InterestIn :> Post '[JSON] InterestOut
  :<|> "trial-requests" :> ReqBody '[JSON] TrialRequestIn :> PostCreated '[JSON] TrialRequestOut
  :<|> "subjects" :> Get '[JSON] [SubjectDTO]
  :<|> "trial-slots" :> QueryParam "subjectId" Int :> Get '[JSON] [TrialSlotDTO]

type PrivateTrialsAPI =
       "trial-requests" :> QueryParam "subjectId" Int :> QueryParam "status" Text :> Get '[JSON] [TrialQueueItem]
  :<|> "trial-requests" :> Capture "id" Int :> "assign" :> ReqBody '[JSON] TrialAssignIn :> Post '[JSON] TrialRequestOut
  :<|> "trial-assignments" :> ReqBody '[JSON] TrialScheduleIn :> Post '[JSON] TrialRequestOut
  :<|> "trial-availability" :> "slots"
        :> QueryParam "subjectId" Int
        :> QueryParam "from" UTCTime
        :> QueryParam "to" UTCTime
        :> Get '[JSON] [TrialAvailabilitySlotDTO]
  :<|> "trial-availability" :> ReqBody '[JSON] TrialAvailabilityUpsert :> Post '[JSON] TrialAvailabilitySlotDTO
  :<|> "trial-availability" :> Capture "id" Int :> Delete '[JSON] NoContent
  :<|> "subjects" :> QueryParam "includeInactive" Bool :> Get '[JSON] [SubjectDTO]
  :<|> "subjects" :> ReqBody '[JSON] SubjectCreate :> PostCreated '[JSON] SubjectDTO
  :<|> "subjects" :> Capture "id" Int :> ReqBody '[JSON] SubjectUpdate :> Patch '[JSON] SubjectDTO
  :<|> "subjects" :> Capture "id" Int :> Delete '[JSON] NoContent
  :<|> "packages" :> QueryParam "subjectId" Int :> Get '[JSON] [PackageDTO]
  :<|> "purchases" :> ReqBody '[JSON] PurchaseIn :> PostCreated '[JSON] PurchaseOut
  :<|> "class-sessions" :> QueryParam "subjectId" Int :> QueryParam "teacherId" Int :> QueryParam "studentId" Int
        :> QueryParam "from" UTCTime :> QueryParam "to" UTCTime :> QueryParam "status" Text :> Get '[JSON] [ClassSessionDTO]
  :<|> "class-sessions" :> ReqBody '[JSON] ClassSessionIn :> Post '[JSON] ClassSessionOut
  :<|> "class-sessions" :> Capture "id" Int :> ReqBody '[JSON] ClassSessionUpdate :> Patch '[JSON] ClassSessionDTO
  :<|> "class-sessions" :> Capture "id" Int :> "attend" :> ReqBody '[JSON] AttendIn :> Post '[JSON] ClassSessionOut
  :<|> "commissions" :> QueryParam "from" UTCTime :> QueryParam "to" UTCTime :> QueryParam "teacherId" Int :> Get '[JSON] [CommissionDTO]
  :<|> "teachers" :> Get '[JSON] [TeacherDTO]
  :<|> "teachers" :> Capture "id" Int :> "classes"
        :> QueryParam "subjectId" Int
        :> QueryParam "from" UTCTime
        :> QueryParam "to" UTCTime
        :> Get '[JSON] [ClassSessionDTO]
  :<|> "teachers" :> Capture "id" Int :> "subjects" :> ReqBody '[JSON] TeacherSubjectsUpdate :> Put '[JSON] TeacherDTO
  :<|> "students" :> Get '[JSON] [StudentDTO]
  :<|> "students" :> ReqBody '[JSON] StudentCreate :> PostCreated '[JSON] StudentDTO

-- Minimal DTOs for the above (you likely have them elsewhere; these are placeholders)
data SignupIn = SignupIn { firstName :: Text, lastName :: Text, email :: Text, phone :: Maybe Text, password :: Maybe Text, googleIdToken :: Maybe Text, marketingOptIn :: Bool } deriving (Generic)
instance ToJSON SignupIn; instance FromJSON SignupIn
data SignupOut = SignupOut { ok :: Bool } deriving (Generic)
instance ToJSON SignupOut; instance FromJSON SignupOut

data InterestIn = InterestIn { interestType :: Text, subjectId :: Maybe Int, details :: Maybe Text, driveLink :: Maybe Text } deriving (Generic)
instance ToJSON InterestIn; instance FromJSON InterestIn
data InterestOut = InterestOut { leadId :: Int } deriving (Generic)
instance ToJSON InterestOut; instance FromJSON InterestOut

data TrialQueueItem = TrialQueueItem
  { requestId   :: Int
  , studentId   :: Maybe Int
  , studentName :: Maybe Text
  , subjectId   :: Int
  , subjectName :: Maybe Text
  , status      :: Text
  , preferred   :: [PreferredSlot]
  , createdAt   :: Maybe UTCTime
  , notes       :: Maybe Text
  } deriving (Generic)
instance ToJSON TrialQueueItem; instance FromJSON TrialQueueItem

data SubjectDTO = SubjectDTO
  { subjectId :: Int
  , name      :: Text
  , active    :: Bool
  , roomIds   :: [Text]
  } deriving (Generic)
instance ToJSON SubjectDTO; instance FromJSON SubjectDTO

data SubjectCreate = SubjectCreate
  { name   :: Text
  , active :: Maybe Bool
  } deriving (Generic)
instance ToJSON SubjectCreate; instance FromJSON SubjectCreate

data SubjectUpdate = SubjectUpdate
  { name   :: Maybe Text
  , active :: Maybe Bool
  } deriving (Generic)
instance ToJSON SubjectUpdate; instance FromJSON SubjectUpdate

data PackageDTO = PackageDTO { packageId :: Int, name :: Text, hoursQty :: Int, priceCents :: Int, expiresDays :: Int } deriving (Generic)
instance ToJSON PackageDTO; instance FromJSON PackageDTO

data PurchaseIn = PurchaseIn { studentId :: Int, packageId :: Int, priceCents :: Int, discountCents :: Maybe Int, taxCents :: Maybe Int, sellerId :: Maybe Int, commissionedTeacherId :: Maybe Int, trialRequestId :: Maybe Int } deriving (Generic)
instance ToJSON PurchaseIn; instance FromJSON PurchaseIn
data PurchaseOut = PurchaseOut { purchaseId :: Int } deriving (Generic)
instance ToJSON PurchaseOut; instance FromJSON PurchaseOut

data ClassSessionIn = ClassSessionIn { studentId :: Int, teacherId :: Int, subjectId :: Int, startAt :: UTCTime, endAt :: UTCTime, roomId :: Int, bookingId :: Maybe Int } deriving (Generic)
instance ToJSON ClassSessionIn; instance FromJSON ClassSessionIn
data ClassSessionOut = ClassSessionOut { classSessionId :: Int, consumedMinutes :: Int } deriving (Generic)
instance ToJSON ClassSessionOut; instance FromJSON ClassSessionOut

data AttendIn = AttendIn { attended :: Bool, notes :: Maybe Text } deriving (Generic)
instance ToJSON AttendIn; instance FromJSON AttendIn

data CommissionDTO = CommissionDTO { teacherId :: Int, amountCents :: Int, basisCents :: Int, percent :: Double } deriving (Generic)
instance ToJSON CommissionDTO; instance FromJSON CommissionDTO

type TrialsAPI = "trials" :> "v1" :> (PublicTrialsAPI :<|> AuthProtect "bearer-token" :> PrivateTrialsAPI)
