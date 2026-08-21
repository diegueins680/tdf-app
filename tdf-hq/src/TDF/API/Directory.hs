{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Directory where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant

data DirectorySearchResponse = DirectorySearchResponse
  { items :: [Value]
  , sponsoredItems :: [Value]
  , facets :: Value
  , nextCursor :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON DirectorySearchResponse

data DirectorySuggestion = DirectorySuggestion
  { label :: Text
  , canonicalQuery :: Text
  , suggestionKind :: Text
  , entityId :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON DirectorySuggestion

data DirectoryReviewPage = DirectoryReviewPage
  { summary :: Value
  , items :: [Value]
  , nextCursor :: Maybe UUID
  } deriving (Show, Generic)
instance ToJSON DirectoryReviewPage

data DirectoryPortfolioItem = DirectoryPortfolioItem
  { itemType :: Text
  , title :: Text
  , url :: Text
  , description :: Maybe Text
  , thumbnailUrl :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON DirectoryPortfolioItem
instance ToJSON DirectoryPortfolioItem

data DirectoryProfileLink = DirectoryProfileLink
  { label :: Text
  , url :: Text
  } deriving (Show, Generic)
instance FromJSON DirectoryProfileLink
instance ToJSON DirectoryProfileLink

data DirectoryProfessionInput = DirectoryProfessionInput
  { professionId :: UUID
  , headline :: Maybe Text
  , yearsExperience :: Maybe Double
  , rateMinMinor :: Maybe Int64
  , rateMaxMinor :: Maybe Int64
  , currencyId :: Maybe UUID
  } deriving (Show, Generic)
instance FromJSON DirectoryProfessionInput
instance ToJSON DirectoryProfessionInput

data DirectoryInstrumentInput = DirectoryInstrumentInput
  { instrumentId :: UUID
  , proficiency :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON DirectoryInstrumentInput
instance ToJSON DirectoryInstrumentInput

data DirectoryLanguageInput = DirectoryLanguageInput
  { languageId :: UUID
  , proficiency :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON DirectoryLanguageInput
instance ToJSON DirectoryLanguageInput

data DirectoryServiceAreaInput = DirectoryServiceAreaInput
  { countryId :: UUID
  , subdivisionId :: Maybe UUID
  , cityId :: Maybe UUID
  , metropolitanAreaId :: Maybe UUID
  , sectorLabel :: Maybe Text
  , serviceRadiusKm :: Maybe Double
  , primaryLocation :: Bool
  , onsite :: Bool
  } deriving (Show, Generic)
instance FromJSON DirectoryServiceAreaInput
instance ToJSON DirectoryServiceAreaInput

data DirectoryProfileUpsert = DirectoryProfileUpsert
  { profileKind :: Text
  , publicName :: Text
  , slug :: Text
  , bio :: Maybe Text
  , experienceSummary :: Maybe Text
  , creditsSummary :: Maybe Text
  , portfolio :: Maybe [DirectoryPortfolioItem]
  , links :: Maybe [DirectoryProfileLink]
  , equipmentSummary :: Maybe Text
  , rateMinMinor :: Maybe Int64
  , rateMaxMinor :: Maybe Int64
  , currencyId :: Maybe UUID
  , clearRates :: Maybe Bool
  , availabilityStatus :: Maybe Text
  , professionIds :: [UUID]
  , professionDetails :: Maybe [DirectoryProfessionInput]
  , instrumentIds :: [UUID]
  , instrumentDetails :: Maybe [DirectoryInstrumentInput]
  , genreIds :: [UUID]
  , serviceOfferingIds :: [UUID]
  , languages :: Maybe [DirectoryLanguageInput]
  , serviceAreas :: Maybe [DirectoryServiceAreaInput]
  , countryId :: UUID
  , cityId :: Maybe UUID
  , metropolitanAreaId :: Maybe UUID
  , onsite :: Bool
  , remote :: Bool
  , availableToTravel :: Bool
  , travelRadiusKm :: Maybe Double
  } deriving (Show, Generic)
instance FromJSON DirectoryProfileUpsert
instance ToJSON DirectoryProfileUpsert

data DirectoryStatusRequest = DirectoryStatusRequest
  { status :: Text
  , reason :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON DirectoryStatusRequest
instance ToJSON DirectoryStatusRequest

data ClassifiedCreateRequest = ClassifiedCreateRequest
  { authorProfileId :: UUID
  , categoryId :: UUID
  , title :: Text
  , slug :: Text
  , description :: Text
  , professionIds :: [UUID]
  , instrumentIds :: [UUID]
  , genreIds :: [UUID]
  , countryIds :: [UUID]
  , cityIds :: [UUID]
  , metropolitanAreaIds :: [UUID]
  , onsite :: Bool
  , remote :: Bool
  , availableToTravel :: Bool
  , startsAt :: Maybe UTCTime
  , endsAt :: Maybe UTCTime
  , experienceLevel :: Maybe Text
  , compensationTypeId :: Maybe UUID
  , budgetMinMinor :: Maybe Int64
  , budgetMaxMinor :: Maybe Int64
  , currencyId :: Maybe UUID
  , budgetNegotiable :: Bool
  , serviceOfferingId :: Maybe UUID
  , serviceAdId :: Maybe Int64
  , expiresAt :: Maybe UTCTime
  } deriving (Show, Generic)
instance FromJSON ClassifiedCreateRequest
instance ToJSON ClassifiedCreateRequest

data ApplicationCreateRequest = ApplicationCreateRequest
  { applicantProfileId :: UUID
  , message :: Text
  , portfolio :: Value
  , availability :: Maybe Text
  , proposedAmountMinor :: Maybe Int64
  , currencyId :: Maybe UUID
  } deriving (Show, Generic)
instance FromJSON ApplicationCreateRequest
instance ToJSON ApplicationCreateRequest

data InvitationCreateRequest = InvitationCreateRequest
  { senderProfileId :: UUID
  , targetProfileId :: UUID
  , classifiedId :: Maybe UUID
  , message :: Text
  } deriving (Show, Generic)
instance FromJSON InvitationCreateRequest
instance ToJSON InvitationCreateRequest

data DirectoryContactRequest = DirectoryContactRequest
  { senderProfileId :: UUID
  , targetProfileId :: UUID
  , contextKind :: Text
  , contextId :: UUID
  , message :: Text
  } deriving (Show, Generic)
instance FromJSON DirectoryContactRequest
instance ToJSON DirectoryContactRequest

data DirectoryReviewCreateRequest = DirectoryReviewCreateRequest
  { interactionId :: UUID
  , authorProfileId :: UUID
  , subjectProfileId :: UUID
  , rating :: Int
  , body :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON DirectoryReviewCreateRequest
instance ToJSON DirectoryReviewCreateRequest

data SavedSearchCreateRequest = SavedSearchCreateRequest
  { name :: Text
  , canonicalQuery :: Value
  , alertsEnabled :: Bool
  , alertFrequency :: Text
  } deriving (Show, Generic)
instance FromJSON SavedSearchCreateRequest
instance ToJSON SavedSearchCreateRequest

data ClaimCreateRequest = ClaimCreateRequest
  { profileId :: UUID
  , claimType :: Text
  , evidence :: Value
  } deriving (Show, Generic)
instance FromJSON ClaimCreateRequest
instance ToJSON ClaimCreateRequest

data ReportCreateRequest = ReportCreateRequest
  { targetKind :: Text
  , targetId :: Text
  , reasonCode :: Text
  , details :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON ReportCreateRequest
instance ToJSON ReportCreateRequest

data VerificationCreateRequest = VerificationCreateRequest
  { profileId :: UUID
  , verificationType :: Text
  , evidence :: Value
  } deriving (Show, Generic)
instance FromJSON VerificationCreateRequest
instance ToJSON VerificationCreateRequest

data ModerationDecisionRequest = ModerationDecisionRequest
  { decision :: Text
  , reasonCode :: Text
  , notes :: Text
  } deriving (Show, Generic)
instance FromJSON ModerationDecisionRequest
instance ToJSON ModerationDecisionRequest

data ProfileMergeRequest = ProfileMergeRequest
  { sourceProfileId :: UUID
  , targetProfileId :: UUID
  , reason :: Text
  } deriving (Show, Generic)
instance FromJSON ProfileMergeRequest
instance ToJSON ProfileMergeRequest

data AgeAssuranceRequest = AgeAssuranceRequest
  { adultAttestation :: Bool
  , guardianPartyId :: Maybe Int64
  } deriving (Show, Generic)
instance FromJSON AgeAssuranceRequest
instance ToJSON AgeAssuranceRequest

type DirectoryPublicAPI = "directory" :>
       ( "search"
           :> QueryParam "q" Text
           :> QueryParam "entityType" Text
           :> QueryParam "cityId" UUID
           :> QueryParam "latitude" Double
           :> QueryParam "longitude" Double
           :> QueryParam "radiusKm" Double
           :> QueryParam "professionId" UUID
           :> QueryParam "serviceId" UUID
           :> QueryParam "instrumentId" UUID
           :> QueryParam "genreId" UUID
           :> QueryParam "remote" Bool
           :> QueryParam "available" Bool
           :> QueryParam "dateFrom" UTCTime
           :> QueryParam "dateTo" UTCTime
           :> QueryParam "cursor" Text
           :> QueryParam "limit" Int
           :> Get '[JSON] DirectorySearchResponse
    :<|> "suggestions" :> QueryParam "q" Text :> QueryParam "cityId" UUID :> Get '[JSON] [DirectorySuggestion]
    :<|> "taxonomies" :> QueryParam "locale" Text :> Get '[JSON] Value
    :<|> "profiles" :> Capture "slug" Text :> Get '[JSON] Value
    :<|> "party-profiles" :> Capture "partyId" Int64 :> Get '[JSON] Value
    :<|> "profiles" :> Capture "slug" Text :> "reviews" :> QueryParam "cursor" UUID :> QueryParam "limit" Int :> Get '[JSON] DirectoryReviewPage
    :<|> "classifieds" :> Capture "slug" Text :> Get '[JSON] Value
    :<|> "events" :> Capture "eventId" Int64 :> Get '[JSON] Value
    :<|> "venues" :> Capture "venueId" Int64 :> Get '[JSON] Value
       )

type RequiredIdempotency = Header' '[Required, Strict] "Idempotency-Key" Text

type DirectoryProtectedAPI = "directory" :>
       ( "age-assurance" :> ReqBody '[JSON] AgeAssuranceRequest :> Put '[JSON] Value
    :<|> "profiles" :> Get '[JSON] [Value]
    :<|> "profiles" :> RequiredIdempotency :> ReqBody '[JSON] DirectoryProfileUpsert :> PostCreated '[JSON] Value
    :<|> "profiles" :> Capture "profileId" UUID :> ReqBody '[JSON] DirectoryProfileUpsert :> Put '[JSON] Value
    :<|> "profiles" :> Capture "profileId" UUID :> "status" :> ReqBody '[JSON] DirectoryStatusRequest :> Patch '[JSON] Value
    :<|> "classifieds" :> Get '[JSON] [Value]
    :<|> "classifieds" :> RequiredIdempotency :> ReqBody '[JSON] ClassifiedCreateRequest :> PostCreated '[JSON] Value
    :<|> "classifieds" :> Capture "classifiedId" UUID :> "status" :> ReqBody '[JSON] DirectoryStatusRequest :> Patch '[JSON] Value
    :<|> "classifieds" :> Capture "classifiedId" UUID :> "applications" :> Get '[JSON] [Value]
    :<|> "classifieds" :> Capture "classifiedId" UUID :> "applications" :> RequiredIdempotency :> ReqBody '[JSON] ApplicationCreateRequest :> PostCreated '[JSON] Value
    :<|> "applications" :> Capture "applicationId" UUID :> "status" :> ReqBody '[JSON] DirectoryStatusRequest :> Patch '[JSON] Value
    :<|> "invitations" :> Get '[JSON] [Value]
    :<|> "invitations" :> RequiredIdempotency :> ReqBody '[JSON] InvitationCreateRequest :> PostCreated '[JSON] Value
    :<|> "invitations" :> Capture "invitationId" UUID :> "status" :> ReqBody '[JSON] DirectoryStatusRequest :> Patch '[JSON] Value
    :<|> "contact" :> RequiredIdempotency :> ReqBody '[JSON] DirectoryContactRequest :> PostCreated '[JSON] Value
    :<|> "review-eligibility" :> QueryParam "authorProfileId" UUID :> Get '[JSON] [Value]
    :<|> "reviews" :> RequiredIdempotency :> ReqBody '[JSON] DirectoryReviewCreateRequest :> PostCreated '[JSON] Value
    :<|> "favorites" :> Get '[JSON] [Value]
    :<|> "favorites" :> Capture "targetKind" Text :> Capture "targetId" Text :> Put '[JSON] NoContent
    :<|> "favorites" :> Capture "targetKind" Text :> Capture "targetId" Text :> Delete '[JSON] NoContent
    :<|> "saved-searches" :> Get '[JSON] [Value]
    :<|> "saved-searches" :> RequiredIdempotency :> ReqBody '[JSON] SavedSearchCreateRequest :> PostCreated '[JSON] Value
    :<|> "claims" :> RequiredIdempotency :> ReqBody '[JSON] ClaimCreateRequest :> PostCreated '[JSON] Value
    :<|> "verifications" :> RequiredIdempotency :> ReqBody '[JSON] VerificationCreateRequest :> PostCreated '[JSON] Value
    :<|> "reports" :> RequiredIdempotency :> ReqBody '[JSON] ReportCreateRequest :> PostCreated '[JSON] Value
    :<|> "admin" :> "claims" :> Get '[JSON] [Value]
    :<|> "admin" :> "claims" :> Capture "claimId" UUID :> "status" :> ReqBody '[JSON] DirectoryStatusRequest :> Patch '[JSON] Value
    :<|> "admin" :> "verifications" :> Get '[JSON] [Value]
    :<|> "admin" :> "verifications" :> Capture "verificationId" UUID :> "status" :> ReqBody '[JSON] DirectoryStatusRequest :> Patch '[JSON] Value
    :<|> "admin" :> "moderation" :> Get '[JSON] [Value]
    :<|> "admin" :> "moderation" :> Capture "caseId" UUID :> "decisions" :> RequiredIdempotency :> ReqBody '[JSON] ModerationDecisionRequest :> PostCreated '[JSON] Value
    :<|> "admin" :> "merges" :> RequiredIdempotency :> ReqBody '[JSON] ProfileMergeRequest :> PostCreated '[JSON] Value
       )
