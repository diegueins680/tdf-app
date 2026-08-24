{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.InternAudit where

import           Data.Int   (Int64)
import           Data.Text  (Text)
import           Data.Time  (Day, UTCTime)
import           GHC.Generics (Generic)
import           Data.Aeson (FromJSON, ToJSON)
import           Servant

type InternAuditAPI =
       "audit-plans" :>
         (    Get '[JSON] [InternAuditPlanDTO]
         :<|> ReqBody '[JSON] InternAuditPlanCreate :> PostCreated '[JSON] InternAuditPlanDTO
         :<|> Capture "planId" Text :>
               (    Get '[JSON] InternAuditPlanDTO
               :<|> ReqBody '[JSON] InternAuditPlanUpdate :> Patch '[JSON] InternAuditPlanDTO
               :<|> "activate" :> Post '[JSON] InternAuditPlanDTO
               :<|> "cases" :>
                     (    Get '[JSON] [InternTestCaseDTO]
                     :<|> ReqBody '[JSON] InternTestCaseCreate :> PostCreated '[JSON] InternTestCaseDTO
                     )
               :<|> "daily-summaries" :>
                     (    Get '[JSON] [InternDailySummaryDTO]
                     :<|> ReqBody '[JSON] InternDailySummaryCreate :> PostCreated '[JSON] InternDailySummaryDTO
                     )
               :<|> "final-summary" :>
                     (    Get '[JSON] InternFinalSummaryDTO
                     :<|> ReqBody '[JSON] InternFinalSummaryUpdate :> Put '[JSON] InternFinalSummaryDTO
                     )
               )
         )
  :<|> "test-cases" :> Capture "testCaseId" Text :> "executions" :>
         (    Get '[JSON] [InternTestExecutionDTO]
         :<|> ReqBody '[JSON] InternTestExecutionCreate :> PostCreated '[JSON] InternTestExecutionDTO
         )
  :<|> "test-executions" :> Capture "executionId" Text
         :> ReqBody '[JSON] InternTestExecutionUpdate
         :> Patch '[JSON] InternTestExecutionDTO

data InternAuditPlanDTO = InternAuditPlanDTO
  { iapId                    :: Text
  , iapProjectId             :: Text
  , iapTaskId                :: Text
  , iapEnvironment           :: Text
  , iapStatus                :: Text
  , iapDurationDays          :: Int
  , iapExpectedHoursMin      :: Int
  , iapExpectedHoursMax      :: Int
  , iapMidpointPercent       :: Int
  , iapProposedAssignee      :: Maybe Int64
  , iapFinalReviewRequired   :: Bool
  , iapCompletionJustification :: Maybe Text
  , iapCompletionApprovedBy  :: Maybe Int64
  , iapCompletionApprovedAt  :: Maybe UTCTime
  , iapCaseCount             :: Int
  , iapExecutedCaseCount     :: Int
  , iapCriticalRemaining     :: Int
  , iapOpenBlockerCount      :: Int
  , iapFailedWithoutReport   :: Int
  , iapEvidenceMissing       :: Int
  , iapCalculatedProgress    :: Int
  , iapCanComplete           :: Bool
  , iapCreatedAt             :: UTCTime
  , iapUpdatedAt             :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternAuditPlanDTO
instance FromJSON InternAuditPlanDTO

data InternAuditPlanCreate = InternAuditPlanCreate
  { iapcProjectId           :: Text
  , iapcTaskId              :: Text
  , iapcEnvironment         :: Text
  , iapcDurationDays        :: Maybe Int
  , iapcExpectedHoursMin    :: Maybe Int
  , iapcExpectedHoursMax    :: Maybe Int
  , iapcMidpointPercent     :: Maybe Int
  , iapcProposedAssignee    :: Maybe Int64
  , iapcFinalReviewRequired :: Maybe Bool
  } deriving (Show, Generic)
instance ToJSON InternAuditPlanCreate
instance FromJSON InternAuditPlanCreate

data InternAuditPlanUpdate = InternAuditPlanUpdate
  { iapuCompletionJustification :: Maybe (Maybe Text)
  , iapuApproveException        :: Maybe Bool
  , iapuStatus                  :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON InternAuditPlanUpdate
instance FromJSON InternAuditPlanUpdate

data InternTestCaseDTO = InternTestCaseDTO
  { itcId                    :: Text
  , itcPlanId                :: Text
  , itcStableId              :: Text
  , itcModuleName            :: Text
  , itcFeatureName           :: Text
  , itcUserRole              :: Text
  , itcObjective             :: Text
  , itcBusinessPurpose       :: Text
  , itcPreconditions         :: Text
  , itcRequiredTestData      :: Text
  , itcEnvironment           :: Text
  , itcPlatform              :: Text
  , itcBrowserOrDevice       :: Text
  , itcLanguage              :: Text
  , itcDetailedSteps         :: Text
  , itcExpectedResult        :: Text
  , itcExpectedPersistedState :: Text
  , itcExpectedSideEffects   :: Text
  , itcCleanupInstructions   :: Text
  , itcCriticality           :: Text
  , itcEvidenceRequirement   :: Text
  , itcExploratoryCharter    :: Maybe Text
  , itcApplicable            :: Bool
  , itcSortOrder             :: Int
  , itcLatestExecution       :: Maybe InternTestExecutionDTO
  } deriving (Show, Generic)
instance ToJSON InternTestCaseDTO
instance FromJSON InternTestCaseDTO

data InternTestCaseCreate = InternTestCaseCreate
  { itccStableId               :: Text
  , itccModuleName             :: Text
  , itccFeatureName            :: Text
  , itccUserRole               :: Text
  , itccObjective              :: Text
  , itccBusinessPurpose        :: Text
  , itccPreconditions          :: Text
  , itccRequiredTestData       :: Text
  , itccEnvironment            :: Text
  , itccPlatform               :: Text
  , itccBrowserOrDevice        :: Text
  , itccLanguage               :: Text
  , itccDetailedSteps          :: Text
  , itccExpectedResult         :: Text
  , itccExpectedPersistedState :: Text
  , itccExpectedSideEffects    :: Text
  , itccCleanupInstructions    :: Text
  , itccCriticality            :: Text
  , itccEvidenceRequirement    :: Text
  , itccExploratoryCharter     :: Maybe Text
  , itccApplicable             :: Maybe Bool
  , itccSortOrder              :: Maybe Int
  } deriving (Show, Generic)
instance ToJSON InternTestCaseCreate
instance FromJSON InternTestCaseCreate

data InternTestExecutionDTO = InternTestExecutionDTO
  { itexId                       :: Text
  , itexTestCaseId               :: Text
  , itexExecutionNumber          :: Int
  , itexExecutorPartyId          :: Int64
  , itexStatus                   :: Text
  , itexActualResult             :: Maybe Text
  , itexPersistedStateObserved   :: Maybe Text
  , itexSideEffectsObserved      :: Maybe Text
  , itexBlockerReason            :: Maybe Text
  , itexEvidenceSummary          :: Maybe Text
  , itexStartedAt                :: Maybe UTCTime
  , itexCompletedAt              :: Maybe UTCTime
  , itexCreatedAt                :: UTCTime
  , itexUpdatedAt                :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternTestExecutionDTO
instance FromJSON InternTestExecutionDTO

data InternTestExecutionCreate = InternTestExecutionCreate
  { itecStatus                 :: Text
  , itecActualResult           :: Maybe Text
  , itecPersistedStateObserved :: Maybe Text
  , itecSideEffectsObserved    :: Maybe Text
  , itecBlockerReason          :: Maybe Text
  , itecEvidenceSummary        :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON InternTestExecutionCreate
instance FromJSON InternTestExecutionCreate

data InternTestExecutionUpdate = InternTestExecutionUpdate
  { iteuStatus                 :: Maybe Text
  , iteuActualResult           :: Maybe (Maybe Text)
  , iteuPersistedStateObserved :: Maybe (Maybe Text)
  , iteuSideEffectsObserved    :: Maybe (Maybe Text)
  , iteuBlockerReason          :: Maybe (Maybe Text)
  , iteuEvidenceSummary        :: Maybe (Maybe Text)
  } deriving (Show, Generic)
instance ToJSON InternTestExecutionUpdate
instance FromJSON InternTestExecutionUpdate

data InternDailySummaryDTO = InternDailySummaryDTO
  { idsId              :: Text
  , idsTaskId          :: Text
  , idsAuthorPartyId   :: Int64
  , idsWorkDate        :: Day
  , idsMinutesWorked   :: Int
  , idsModulesTested   :: Text
  , idsCasesCompleted  :: Int
  , idsReportsCreated  :: Int
  , idsBlockers        :: Maybe Text
  , idsNextStep        :: Text
  , idsCreatedAt       :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternDailySummaryDTO
instance FromJSON InternDailySummaryDTO

data InternDailySummaryCreate = InternDailySummaryCreate
  { idscWorkDate        :: Day
  , idscMinutesWorked   :: Int
  , idscModulesTested   :: Text
  , idscCasesCompleted  :: Int
  , idscReportsCreated  :: Int
  , idscBlockers        :: Maybe Text
  , idscNextStep        :: Text
  } deriving (Show, Generic)
instance ToJSON InternDailySummaryCreate
instance FromJSON InternDailySummaryCreate

data InternFinalSummaryDTO = InternFinalSummaryDTO
  { ifsId                :: Text
  , ifsPlanId            :: Text
  , ifsAuthorPartyId     :: Int64
  , ifsGeneratedSnapshot :: Text
  , ifsConclusions       :: Maybe Text
  , ifsSubmittedAt       :: Maybe UTCTime
  , ifsApprovedBy        :: Maybe Int64
  , ifsApprovedAt        :: Maybe UTCTime
  , ifsCreatedAt         :: UTCTime
  , ifsUpdatedAt         :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternFinalSummaryDTO
instance FromJSON InternFinalSummaryDTO

data InternFinalSummaryUpdate = InternFinalSummaryUpdate
  { ifsuConclusions :: Maybe Text
  , ifsuSubmit      :: Maybe Bool
  } deriving (Show, Generic)
instance ToJSON InternFinalSummaryUpdate
instance FromJSON InternFinalSummaryUpdate
