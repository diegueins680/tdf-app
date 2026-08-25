{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Feedback
  ( FeedbackAPI
  , InternalFeedbackAPI
  , FeedbackPayload(..)
  , InternalFeedbackCreate(..)
  , InternalFeedbackUpdate(..)
  , InternalFeedbackDTO(..)
  , InternalFeedbackSummaryDTO(..)
  , InternalFeedbackCommentCreate(..)
  , InternalFeedbackCommentDTO(..)
  , InternalFeedbackEvidenceLinkCreate(..)
  , InternalFeedbackEvidencePayload(..)
  , InternalFeedbackEvidenceDTO(..)
  , InternalFeedbackHistoryDTO(..)
  , InternalFeedbackRetestCreate(..)
  , InternalFeedbackRetestDTO(..)
  , LegacyFeedbackDTO(..)
  ) where

import           Data.Aeson               (FromJSON(..), ToJSON, Object, withObject, (.:?))
import qualified Data.Aeson.Key           as AesonKey
import qualified Data.Aeson.KeyMap        as AesonKeyMap
import           Data.Aeson.Types         (Parser)
import qualified Data.ByteString.Lazy     as BL
import           Data.Int                 (Int64)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time                (UTCTime)
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
  Header "Authorization" Text :>
    Header "Cookie" Text :>
      "feedback" :>
        ( MultipartForm Tmp FeedbackPayload :> Post '[JSON] NoContent )

type InternalFeedbackAPI =
       QueryParam "state" Text
         :> QueryParam "module" Text
         :> QueryParam "q" Text
         :> QueryParam "mine" Bool
         :> Get '[JSON] [InternalFeedbackSummaryDTO]
  :<|> "export.csv" :> QueryParam "state" Text :> QueryParam "module" Text :> Get '[PlainText] Text
  :<|> "export.json" :> QueryParam "state" Text :> QueryParam "module" Text :> Get '[JSON] [InternalFeedbackSummaryDTO]
  :<|> "legacy" :> Get '[JSON] [LegacyFeedbackDTO]
  :<|> ReqBody '[JSON] InternalFeedbackCreate :> PostCreated '[JSON] InternalFeedbackDTO
  :<|> Capture "reportId" Text :>
         (    Get '[JSON] InternalFeedbackDTO
         :<|> ReqBody '[JSON] InternalFeedbackUpdate :> Patch '[JSON] InternalFeedbackDTO
         :<|> "submit" :> Post '[JSON] InternalFeedbackDTO
         :<|> "comments" :> ReqBody '[JSON] InternalFeedbackCommentCreate :> PostCreated '[JSON] InternalFeedbackCommentDTO
         :<|> "evidence" :> MultipartForm Tmp InternalFeedbackEvidencePayload :> PostCreated '[JSON] InternalFeedbackEvidenceDTO
         :<|> "evidence-links" :> ReqBody '[JSON] InternalFeedbackEvidenceLinkCreate :> PostCreated '[JSON] InternalFeedbackEvidenceDTO
         :<|> "evidence" :> Capture "evidenceId" Text :> "file"
                :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BL.ByteString)
         :<|> "retests" :> ReqBody '[JSON] InternalFeedbackRetestCreate :> PostCreated '[JSON] InternalFeedbackRetestDTO
         )

data FeedbackPayload = FeedbackPayload
  { fpTitle        :: Text
  , fpDescription  :: Text
  , fpCategoryId   :: Text
  , fpSeverityId   :: Text
  , fpContactEmail :: Maybe Text
  , fpConsent      :: Bool
  , fpAttachment   :: Maybe (FileData Tmp)
  } deriving (Show, Generic)

data InternalFeedbackCreate = InternalFeedbackCreate
  { ifcTitle              :: Text
  , ifcDescription        :: Text
  , ifcCategoryId         :: Text
  , ifcProposedSeverityId :: Text
  , ifcReportType         :: Text
  , ifcModuleName         :: Text
  , ifcFeatureName        :: Maybe Text
  , ifcEnvironment        :: Text
  , ifcUrlOrScreen        :: Maybe Text
  , ifcPlatform           :: Text
  , ifcDevice             :: Maybe Text
  , ifcBrowser            :: Maybe Text
  , ifcLanguage           :: Text
  , ifcAccountRole        :: Text
  , ifcReproductionSteps  :: Maybe Text
  , ifcExpectedResult     :: Maybe Text
  , ifcActualResult       :: Maybe Text
  , ifcFrequency          :: Maybe Text
  , ifcTestCaseId         :: Maybe Text
  , ifcTestExecutionId    :: Maybe Text
  , ifcInternshipProjectId :: Maybe Text
  , ifcInternshipTaskId   :: Maybe Text
  , ifcBlocking           :: Maybe Bool
  , ifcVideoLinks         :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackCreate
instance FromJSON InternalFeedbackCreate

data InternalFeedbackUpdate = InternalFeedbackUpdate
  { ifuTitle                   :: Maybe Text
  , ifuDescription             :: Maybe Text
  , ifuCategoryId              :: Maybe Text
  , ifuProposedSeverityId      :: Maybe Text
  , ifuReportType              :: Maybe Text
  , ifuModuleName              :: Maybe Text
  , ifuFeatureName             :: Maybe (Maybe Text)
  , ifuEnvironment             :: Maybe Text
  , ifuUrlOrScreen             :: Maybe (Maybe Text)
  , ifuPlatform                :: Maybe Text
  , ifuDevice                  :: Maybe (Maybe Text)
  , ifuBrowser                 :: Maybe (Maybe Text)
  , ifuLanguage                :: Maybe Text
  , ifuAccountRole             :: Maybe Text
  , ifuReproductionSteps       :: Maybe (Maybe Text)
  , ifuExpectedResult          :: Maybe (Maybe Text)
  , ifuActualResult            :: Maybe (Maybe Text)
  , ifuFrequency               :: Maybe (Maybe Text)
  , ifuBlocking                :: Maybe Bool
  , ifuVideoLinks              :: Maybe (Maybe Text)
  , ifuState                   :: Maybe Text
  , ifuAuthoritativeSeverityId :: Maybe (Maybe Text)
  , ifuPriority                :: Maybe (Maybe Text)
  , ifuAssignedTo              :: Maybe (Maybe Int64)
  , ifuDuplicateOf             :: Maybe (Maybe Text)
  , ifuResolution              :: Maybe (Maybe Text)
  , ifuRetestResult            :: Maybe (Maybe Text)
  , ifuClosureReason           :: Maybe (Maybe Text)
  , ifuGithubIssueUrl          :: Maybe (Maybe Text)
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackUpdate
instance FromJSON InternalFeedbackUpdate where
  parseJSON = withObject "InternalFeedbackUpdate" $ \value -> InternalFeedbackUpdate
    <$> value .:? "ifuTitle"
    <*> value .:? "ifuDescription"
    <*> value .:? "ifuCategoryId"
    <*> value .:? "ifuProposedSeverityId"
    <*> value .:? "ifuReportType"
    <*> value .:? "ifuModuleName"
    <*> nestedOptional value "ifuFeatureName"
    <*> value .:? "ifuEnvironment"
    <*> nestedOptional value "ifuUrlOrScreen"
    <*> value .:? "ifuPlatform"
    <*> nestedOptional value "ifuDevice"
    <*> nestedOptional value "ifuBrowser"
    <*> value .:? "ifuLanguage"
    <*> value .:? "ifuAccountRole"
    <*> nestedOptional value "ifuReproductionSteps"
    <*> nestedOptional value "ifuExpectedResult"
    <*> nestedOptional value "ifuActualResult"
    <*> nestedOptional value "ifuFrequency"
    <*> value .:? "ifuBlocking"
    <*> nestedOptional value "ifuVideoLinks"
    <*> value .:? "ifuState"
    <*> nestedOptional value "ifuAuthoritativeSeverityId"
    <*> nestedOptional value "ifuPriority"
    <*> nestedOptional value "ifuAssignedTo"
    <*> nestedOptional value "ifuDuplicateOf"
    <*> nestedOptional value "ifuResolution"
    <*> nestedOptional value "ifuRetestResult"
    <*> nestedOptional value "ifuClosureReason"
    <*> nestedOptional value "ifuGithubIssueUrl"

nestedOptional :: FromJSON a => Object -> AesonKey.Key -> Parser (Maybe (Maybe a))
nestedOptional value key
  | AesonKeyMap.member key value = Just <$> value .:? key
  | otherwise = pure Nothing

data InternalFeedbackSummaryDTO = InternalFeedbackSummaryDTO
  { ifsId                    :: Text
  , ifsTitle                 :: Text
  , ifsReportType            :: Text
  , ifsState                 :: Text
  , ifsModuleName            :: Text
  , ifsFeatureName           :: Maybe Text
  , ifsEnvironment           :: Text
  , ifsPlatform              :: Text
  , ifsProposedSeverityId    :: Maybe Text
  , ifsAuthoritativeSeverityId :: Maybe Text
  , ifsPriority              :: Maybe Text
  , ifsBlocking              :: Bool
  , ifsReporterPartyId       :: Int64
  , ifsReporterName          :: Text
  , ifsInternshipProjectId   :: Maybe Text
  , ifsInternshipTaskId      :: Maybe Text
  , ifsTestCaseId            :: Maybe Text
  , ifsTestExecutionId       :: Maybe Text
  , ifsDuplicateOf           :: Maybe Text
  , ifsCreatedAt             :: UTCTime
  , ifsUpdatedAt             :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackSummaryDTO
instance FromJSON InternalFeedbackSummaryDTO

data InternalFeedbackDTO = InternalFeedbackDTO
  { ifrSummary              :: InternalFeedbackSummaryDTO
  , ifrDescription          :: Text
  , ifrCategoryId           :: Maybe Text
  , ifrUrlOrScreen          :: Maybe Text
  , ifrDevice               :: Maybe Text
  , ifrBrowser              :: Maybe Text
  , ifrLanguage             :: Text
  , ifrAccountRole          :: Text
  , ifrReproductionSteps    :: Maybe Text
  , ifrExpectedResult       :: Maybe Text
  , ifrActualResult         :: Maybe Text
  , ifrFrequency            :: Maybe Text
  , ifrAssignedTo           :: Maybe Int64
  , ifrResolution           :: Maybe Text
  , ifrRetestResult         :: Maybe Text
  , ifrClosureReason        :: Maybe Text
  , ifrGithubIssueUrl       :: Maybe Text
  , ifrVideoLinks           :: Maybe Text
  , ifrSubmittedAt          :: Maybe UTCTime
  , ifrClosedAt             :: Maybe UTCTime
  , ifrAuditPlanMutable     :: Bool
  , ifrEvidence             :: [InternalFeedbackEvidenceDTO]
  , ifrComments             :: [InternalFeedbackCommentDTO]
  , ifrHistory              :: [InternalFeedbackHistoryDTO]
  , ifrRetests              :: [InternalFeedbackRetestDTO]
  , ifrPotentialDuplicates  :: [InternalFeedbackSummaryDTO]
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackDTO
instance FromJSON InternalFeedbackDTO

data InternalFeedbackCommentCreate = InternalFeedbackCommentCreate
  { ifccKind :: Maybe Text
  , ifccBody :: Text
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackCommentCreate
instance FromJSON InternalFeedbackCommentCreate

data InternalFeedbackCommentDTO = InternalFeedbackCommentDTO
  { ifcmId            :: Text
  , ifcmAuthorPartyId :: Int64
  , ifcmAuthorName    :: Text
  , ifcmKind          :: Text
  , ifcmBody          :: Text
  , ifcmCreatedAt     :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackCommentDTO
instance FromJSON InternalFeedbackCommentDTO

data InternalFeedbackEvidencePayload = InternalFeedbackEvidencePayload
  { ifepCaption    :: Maybe Text
  , ifepAttachment :: FileData Tmp
  } deriving (Show, Generic)

data InternalFeedbackEvidenceLinkCreate = InternalFeedbackEvidenceLinkCreate
  { ifelUrl     :: Text
  , ifelCaption :: Maybe Text
  , ifelKind    :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackEvidenceLinkCreate
instance FromJSON InternalFeedbackEvidenceLinkCreate

data InternalFeedbackEvidenceDTO = InternalFeedbackEvidenceDTO
  { ifeId               :: Text
  , ifeKind             :: Text
  , ifeOriginalFileName :: Maybe Text
  , ifeContentType      :: Maybe Text
  , ifeSizeBytes        :: Maybe Int
  , ifeExternalUrl      :: Maybe Text
  , ifeCaption          :: Maybe Text
  , ifeUploadedBy       :: Int64
  , ifeCreatedAt        :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackEvidenceDTO
instance FromJSON InternalFeedbackEvidenceDTO

data InternalFeedbackHistoryDTO = InternalFeedbackHistoryDTO
  { ifhId           :: Text
  , ifhActorPartyId :: Int64
  , ifhActorName    :: Text
  , ifhAction       :: Text
  , ifhPreviousState :: Maybe Text
  , ifhNewState     :: Maybe Text
  , ifhMetadata     :: Maybe Text
  , ifhCreatedAt    :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackHistoryDTO
instance FromJSON InternalFeedbackHistoryDTO

data InternalFeedbackRetestCreate = InternalFeedbackRetestCreate
  { ifrcExecutionId    :: Maybe Text
  , ifrcResult         :: Text
  , ifrcNotes          :: Maybe Text
  , ifrcEvidenceSummary :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackRetestCreate
instance FromJSON InternalFeedbackRetestCreate

data InternalFeedbackRetestDTO = InternalFeedbackRetestDTO
  { ifrtId              :: Text
  , ifrtExecutionId     :: Maybe Text
  , ifrtTesterPartyId   :: Int64
  , ifrtTesterName      :: Text
  , ifrtResult          :: Text
  , ifrtNotes           :: Maybe Text
  , ifrtEvidenceSummary :: Maybe Text
  , ifrtCreatedAt       :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternalFeedbackRetestDTO
instance FromJSON InternalFeedbackRetestDTO

data LegacyFeedbackDTO = LegacyFeedbackDTO
  { lfdId            :: Text
  , lfdTitle         :: Text
  , lfdDescription   :: Text
  , lfdCategoryId    :: Maybe Text
  , lfdSeverityId    :: Maybe Text
  , lfdContactEmail  :: Maybe Text
  , lfdConsent       :: Bool
  , lfdCreatedBy     :: Maybe Int64
  , lfdHasAttachment :: Bool
  , lfdCreatedAt     :: UTCTime
  } deriving (Show, Generic)
instance ToJSON LegacyFeedbackDTO
instance FromJSON LegacyFeedbackDTO

instance FromMultipart Tmp FeedbackPayload where
  fromMultipart multipart = do
    rejectUnexpectedParts multipart
    title <- lookupText "title" multipart
    description <- lookupText "description" multipart
    consent <- optionalBool "consent" multipart
    categoryId <- lookupText "categoryId" multipart
    severityId <- lookupText "severityId" multipart
    contact <- optionalText "contactEmail" multipart
    attachment <- lookupFile "attachment" multipart
    pure FeedbackPayload
      { fpTitle        = T.strip title
      , fpDescription  = T.strip description
      , fpCategoryId   = T.strip categoryId
      , fpSeverityId   = T.strip severityId
      , fpContactEmail = contact
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
        fmap (>>= normalizeOptionalInput) (lookupSingleInput name mp)

      normalizeOptionalInput input =
        let txt = T.strip (inputValueText input)
        in if T.null txt then Nothing else Just txt

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

      rejectUnexpectedParts mp =
        case (unexpectedInputs, unexpectedFiles) of
          (fieldName : _, _) -> Left ("Unexpected field: " <> T.unpack fieldName)
          (_, fileName : _) -> Left ("Unexpected file field: " <> T.unpack fileName)
          _ -> Right ()
        where
          expectedInputs =
            [ "title"
            , "description"
            , "categoryId"
            , "severityId"
            , "contactEmail"
            , "consent"
            ]
          expectedFiles = ["attachment"]
          unexpectedInputs =
            [ name
            | Input name _ <- inputs mp
            , name `notElem` expectedInputs
            ]
          unexpectedFiles =
            [ fdInputName file
            | file <- files mp
            , fdInputName file `notElem` expectedFiles
            ]

      lookupSingleInput name mp =
        case filter (\(Input nm _) -> nm == name) (inputs mp) of
          [] -> Right Nothing
          [x] -> Right (Just x)
          _ -> Left ("Duplicate field: " <> T.unpack name)

      inputValueText (Input _ value) = value

instance FromMultipart Tmp InternalFeedbackEvidencePayload where
  fromMultipart multipart = do
    caption <- case [T.strip value | Input name value <- inputs multipart, name == "caption"] of
      [] -> Right Nothing
      [value] -> Right (if T.null value then Nothing else Just value)
      _ -> Left "Duplicate field: caption"
    attachment <- case [file | file <- files multipart, fdInputName file == "attachment"] of
      [file] -> Right file
      [] -> Left "Missing file field: attachment"
      _ -> Left "Duplicate file field: attachment"
    let unexpectedInputs = [name | Input name _ <- inputs multipart, name /= "caption"]
        unexpectedFiles = [fdInputName file | file <- files multipart, fdInputName file /= "attachment"]
    case (unexpectedInputs, unexpectedFiles) of
      (name:_, _) -> Left ("Unexpected field: " <> T.unpack name)
      (_, name:_) -> Left ("Unexpected file field: " <> T.unpack name)
      _ -> Right InternalFeedbackEvidencePayload
        { ifepCaption = caption
        , ifepAttachment = attachment
        }
