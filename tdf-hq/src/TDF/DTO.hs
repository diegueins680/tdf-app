{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.DTO where

import           GHC.Generics (Generic)
import           Data.Aeson (Options, ToJSON(..), FromJSON(..), defaultOptions, genericParseJSON, genericToJSON, rejectUnknownFields)
import           Data.Aeson.Types (fieldLabelModifier, omitNothingFields)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int64)
import           Data.Time (UTCTime, Day)
import           Data.Char (toLower)

import           Database.Persist (Entity(..))
import           Database.Persist.Sql (fromSqlKey)

import           TDF.Models
import qualified TDF.Models as M
import           TDF.API.Types (BandDTO)

strictDecodeOptions :: Options
strictDecodeOptions = defaultOptions { rejectUnknownFields = True }

-- Parties
data PartyDTO = PartyDTO
  { partyId          :: Int64
  , legalName        :: Maybe Text
  , displayName      :: Text
  , isOrg            :: Bool
  , taxId            :: Maybe Text
  , primaryEmail     :: Maybe Text
  , primaryPhone     :: Maybe Text
  , whatsapp         :: Maybe Text
  , instagram        :: Maybe Text
  , emergencyContact :: Maybe Text
  , notes            :: Maybe Text
  , band             :: Maybe BandDTO
  , hasUserAccount   :: Bool
  } deriving (Show, Generic)

instance ToJSON PartyDTO
instance FromJSON PartyDTO

data SocialPartyProfileDTO = SocialPartyProfileDTO
  { sppPartyId     :: Int64
  , sppDisplayName :: Text
  , sppAvatarUrl   :: Maybe Text
  , sppBio         :: Maybe Text
  , sppCity        :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON SocialPartyProfileDTO
instance FromJSON SocialPartyProfileDTO
 
data ArtistProfileDTO = ArtistProfileDTO
  { apArtistId        :: Int64
  , apDisplayName     :: Text
  , apSlug            :: Maybe Text
  , apBio             :: Maybe Text
  , apCity            :: Maybe Text
  , apHeroImageUrl    :: Maybe Text
  , apSpotifyArtistId :: Maybe Text
  , apSpotifyUrl      :: Maybe Text
  , apYoutubeChannelId :: Maybe Text
  , apYoutubeUrl      :: Maybe Text
  , apWebsiteUrl      :: Maybe Text
  , apFeaturedVideoUrl :: Maybe Text
  , apGenres          :: Maybe Text
  , apHighlights      :: Maybe Text
  , apFollowerCount   :: Int
  , apHasUserAccount  :: Bool
  } deriving (Show, Generic)
instance ToJSON ArtistProfileDTO

data ArtistProfileUpsert = ArtistProfileUpsert
  { apuArtistId        :: Int64
  , apuDisplayName     :: Maybe Text
  , apuSlug            :: Maybe Text
  , apuBio             :: Maybe Text
  , apuCity            :: Maybe Text
  , apuHeroImageUrl    :: Maybe Text
  , apuSpotifyArtistId :: Maybe Text
  , apuSpotifyUrl      :: Maybe Text
  , apuYoutubeChannelId :: Maybe Text
  , apuYoutubeUrl      :: Maybe Text
  , apuWebsiteUrl      :: Maybe Text
  , apuFeaturedVideoUrl :: Maybe Text
  , apuGenres          :: Maybe Text
  , apuHighlights      :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON ArtistProfileUpsert where
  parseJSON = genericParseJSON strictDecodeOptions

data ArtistReleaseDTO = ArtistReleaseDTO
  { arArtistId     :: Int64
  , arReleaseId    :: Int64
  , arTitle        :: Text
  , arReleaseDate  :: Maybe Day
  , arDescription  :: Maybe Text
  , arCoverImageUrl :: Maybe Text
  , arSpotifyUrl   :: Maybe Text
  , arYoutubeUrl   :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON ArtistReleaseDTO

data ArtistReleaseUpsert = ArtistReleaseUpsert
  { aruArtistId    :: Int64
  , aruTitle       :: Text
  , aruReleaseDate :: Maybe Day
  , aruDescription :: Maybe Text
  , aruCoverImageUrl :: Maybe Text
  , aruSpotifyUrl  :: Maybe Text
  , aruYoutubeUrl  :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON ArtistReleaseUpsert where
  parseJSON = genericParseJSON strictDecodeOptions

data FanProfileDTO = FanProfileDTO
  { fpArtistId      :: Int64
  , fpDisplayName   :: Maybe Text
  , fpAvatarUrl     :: Maybe Text
  , fpFavoriteGenres :: Maybe Text
  , fpBio           :: Maybe Text
  , fpCity          :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON FanProfileDTO

data FanProfileUpdate = FanProfileUpdate
  { fpuDisplayName   :: Maybe Text
  , fpuAvatarUrl     :: Maybe Text
  , fpuFavoriteGenres :: Maybe Text
  , fpuBio           :: Maybe Text
  , fpuCity          :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON FanProfileUpdate where
  parseJSON = genericParseJSON strictDecodeOptions

data CountryDTO = CountryDTO
  { countryCode :: Text
  , countryName :: Text
  } deriving (Show, Generic)
instance ToJSON CountryDTO
instance FromJSON CountryDTO

toCountryDTO :: Entity Country -> CountryDTO
toCountryDTO (Entity _ Country{..}) = CountryDTO
  { countryCode = countryCode
  , countryName = countryName
  }

data FanFollowDTO = FanFollowDTO
  { ffArtistId      :: Int64
  , ffArtistName    :: Text
  , ffHeroImageUrl  :: Maybe Text
  , ffSpotifyUrl    :: Maybe Text
  , ffYoutubeUrl    :: Maybe Text
  , ffStartedAt     :: Day
  } deriving (Show, Generic)
instance ToJSON FanFollowDTO

-- Social follows between any parties (used for vCard/NFC exchanges)
data PartyFollowDTO = PartyFollowDTO
  { pfFollowerId   :: Int64
  , pfFollowingId  :: Int64
  , pfViaNfc       :: Bool
  , pfStartedAt    :: Day
  } deriving (Show, Generic)
instance ToJSON PartyFollowDTO

data SuggestedFriendDTO = SuggestedFriendDTO
  { sfPartyId       :: Int64
  , sfMutualCount   :: Int
  } deriving (Show, Generic)
instance ToJSON SuggestedFriendDTO

data ChatThreadDTO = ChatThreadDTO
  { ctThreadId         :: Int64
  , ctOtherPartyId     :: Int64
  , ctOtherDisplayName :: Text
  , ctLastMessage      :: Maybe Text
  , ctLastMessageAt    :: Maybe UTCTime
  , ctUpdatedAt        :: UTCTime
  } deriving (Show, Generic)
instance ToJSON ChatThreadDTO

data ChatMessageDTO = ChatMessageDTO
  { cmId            :: Int64
  , cmThreadId      :: Int64
  , cmSenderPartyId :: Int64
  , cmBody          :: Text
  , cmCreatedAt     :: UTCTime
  } deriving (Show, Generic)
instance ToJSON ChatMessageDTO

data ChatSendMessageRequest = ChatSendMessageRequest
  { csmBody :: Text
  } deriving (Show, Generic)
instance FromJSON ChatSendMessageRequest where
  parseJSON = genericParseJSON strictDecodeOptions

data CampaignDTO = CampaignDTO
  { campId        :: Int64
  , campName      :: Text
  , campObjective :: Maybe Text
  , campPlatform  :: Maybe Text
  , campStatus    :: Text
  , campBudgetCents :: Maybe Int
  , campStartDate :: Maybe Day
  , campEndDate   :: Maybe Day
  } deriving (Show, Generic)
instance ToJSON CampaignDTO

data CampaignUpsert = CampaignUpsert
  { cuId          :: Maybe Int64
  , cuName        :: Text
  , cuObjective   :: Maybe Text
  , cuPlatform    :: Maybe Text
  , cuStatus      :: Maybe Text
  , cuBudgetCents :: Maybe Int
  , cuStartDate   :: Maybe Day
  , cuEndDate     :: Maybe Day
  } deriving (Show, Generic)
instance FromJSON CampaignUpsert where
  parseJSON = genericParseJSON strictDecodeOptions

data AdCreativeDTO = AdCreativeDTO
  { adId        :: Int64
  , adCampaignId :: Maybe Int64
  , adExternalId :: Maybe Text
  , adName      :: Text
  , adChannel   :: Maybe Text
  , adAudience  :: Maybe Text
  , adLandingUrl :: Maybe Text
  , adCta       :: Maybe Text
  , adStatus    :: Text
  , adNotes     :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON AdCreativeDTO

data AdCreativeUpsert = AdCreativeUpsert
  { acuId         :: Maybe Int64
  , acuCampaignId :: Maybe Int64
  , acuExternalId :: Maybe Text
  , acuName       :: Text
  , acuChannel    :: Maybe Text
  , acuAudience   :: Maybe Text
  , acuLandingUrl :: Maybe Text
  , acuCta        :: Maybe Text
  , acuStatus     :: Maybe Text
  , acuNotes      :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON AdCreativeUpsert where
  parseJSON = genericParseJSON strictDecodeOptions

data AdConversationExampleDTO = AdConversationExampleDTO
  { aceId              :: Int64
  , aceAdId            :: Int64
  , aceUserMessage     :: Text
  , aceAssistantMessage :: Text
  , aceTags            :: Maybe [Text]
  } deriving (Show, Generic)
instance ToJSON AdConversationExampleDTO

data AdConversationExampleCreate = AdConversationExampleCreate
  { aecUserMessage     :: Text
  , aecAssistantMessage :: Text
  , aecTags            :: Maybe [Text]
  } deriving (Show, Generic)
instance FromJSON AdConversationExampleCreate where
  parseJSON = genericParseJSON strictDecodeOptions

data AdsAssistRequest = AdsAssistRequest
  { aarAdId       :: Maybe Int64
  , aarCampaignId :: Maybe Int64
  , aarMessage    :: Text
  , aarChannel    :: Maybe Text
  , aarPartyId    :: Maybe Int64
  } deriving (Show, Generic)
instance FromJSON AdsAssistRequest where
  parseJSON = genericParseJSON strictDecodeOptions

data AdsAssistResponse = AdsAssistResponse
  { aasReply         :: Text
  , aasUsedExamples  :: [AdConversationExampleDTO]
  , aasKnowledgeUsed :: [Text]
  } deriving (Show, Generic)
instance ToJSON AdsAssistResponse

data RadioPresenceDTO = RadioPresenceDTO
  { rpPartyId     :: Int64
  , rpStreamUrl   :: Text
  , rpStationName :: Maybe Text
  , rpStationId   :: Maybe Text
  , rpUpdatedAt   :: UTCTime
  } deriving (Show, Generic)
instance ToJSON RadioPresenceDTO

data RadioPresenceUpsert = RadioPresenceUpsert
  { rpuStreamUrl   :: Text
  , rpuStationName :: Maybe Text
  , rpuStationId   :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON RadioPresenceUpsert

data VCardExchangeRequest = VCardExchangeRequest
  { vcerPartyId :: Int64
  } deriving (Show, Generic)
instance FromJSON VCardExchangeRequest

-- Course registrations (admin)
data CourseRegistrationDTO = CourseRegistrationDTO
  { crId          :: Int64
  , crCourseSlug  :: Text
  , crPartyId     :: Maybe Int64
  , crFullName    :: Maybe Text
  , crEmail       :: Maybe Text
  , crPhoneE164   :: Maybe Text
  , crSource      :: Text
  , crStatus      :: Text
  , crAdminNotes  :: Maybe Text
  , crHowHeard    :: Maybe Text
  , crUtmSource   :: Maybe Text
  , crUtmMedium   :: Maybe Text
  , crUtmCampaign :: Maybe Text
  , crUtmContent  :: Maybe Text
  , crCreatedAt   :: UTCTime
  , crUpdatedAt   :: UTCTime
  } deriving (Show, Generic)
instance ToJSON CourseRegistrationDTO

data CourseRegistrationReceiptDTO = CourseRegistrationReceiptDTO
  { crrId             :: Int64
  , crrRegistrationId :: Int64
  , crrPartyId        :: Maybe Int64
  , crrFileUrl        :: Text
  , crrFileName       :: Maybe Text
  , crrMimeType       :: Maybe Text
  , crrNotes          :: Maybe Text
  , crrUploadedBy     :: Maybe Int64
  , crrCreatedAt      :: UTCTime
  , crrUpdatedAt      :: UTCTime
  } deriving (Show, Generic)
instance ToJSON CourseRegistrationReceiptDTO

data CourseRegistrationFollowUpDTO = CourseRegistrationFollowUpDTO
  { crfId             :: Int64
  , crfRegistrationId :: Int64
  , crfPartyId        :: Maybe Int64
  , crfEntryType      :: Text
  , crfSubject        :: Maybe Text
  , crfNotes          :: Text
  , crfAttachmentUrl  :: Maybe Text
  , crfAttachmentName :: Maybe Text
  , crfNextFollowUpAt :: Maybe UTCTime
  , crfCreatedBy      :: Maybe Int64
  , crfCreatedAt      :: UTCTime
  , crfUpdatedAt      :: UTCTime
  } deriving (Show, Generic)
instance ToJSON CourseRegistrationFollowUpDTO

data CourseRegistrationDossierDTO = CourseRegistrationDossierDTO
  { crdRegistration :: CourseRegistrationDTO
  , crdReceipts     :: [CourseRegistrationReceiptDTO]
  , crdFollowUps    :: [CourseRegistrationFollowUpDTO]
  , crdCanMarkPaid  :: Bool
  } deriving (Show, Generic)
instance ToJSON CourseRegistrationDossierDTO

data CourseEmailEventDTO = CourseEmailEventDTO
  { ceId             :: Int64
  , ceCourseSlug     :: Text
  , ceRegistrationId :: Maybe Int64
  , ceRecipientEmail :: Text
  , ceRecipientName  :: Maybe Text
  , ceEventType      :: Text
  , ceStatus         :: Text
  , ceMessage        :: Maybe Text
  , ceCreatedAt      :: UTCTime
  } deriving (Show, Generic)
instance ToJSON CourseEmailEventDTO

data CourseCohortOptionDTO = CourseCohortOptionDTO
  { ccSlug  :: Text
  , ccTitle :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON CourseCohortOptionDTO

-- Logs
data LogEntryDTO = LogEntryDTO
  { logTimestamp :: UTCTime
  , logLevel     :: Text
  , logMessage   :: Text
  } deriving (Show, Generic)
instance ToJSON LogEntryDTO

data PartyCreate = PartyCreate
  { cLegalName        :: Maybe Text
  , cDisplayName      :: Text
  , cIsOrg            :: Bool
  , cTaxId            :: Maybe Text
  , cPrimaryEmail     :: Maybe Text
  , cPrimaryPhone     :: Maybe Text
  , cWhatsapp         :: Maybe Text
  , cInstagram        :: Maybe Text
  , cEmergencyContact :: Maybe Text
  , cNotes            :: Maybe Text
  , cRoles            :: Maybe [RoleEnum]
  } deriving (Show, Generic)
instance FromJSON PartyCreate where
  parseJSON = genericParseJSON strictDecodeOptions

data PartyUpdate = PartyUpdate
  { uLegalName        :: Maybe Text
  , uDisplayName      :: Maybe Text
  , uIsOrg            :: Maybe Bool
  , uTaxId            :: Maybe Text
  , uPrimaryEmail     :: Maybe Text
  , uPrimaryPhone     :: Maybe Text
  , uWhatsapp         :: Maybe Text
  , uInstagram        :: Maybe Text
  , uEmergencyContact :: Maybe Text
  , uNotes            :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON PartyUpdate where
  parseJSON = genericParseJSON strictDecodeOptions

toPartyDTO :: Bool -> Entity Party -> PartyDTO
toPartyDTO = toPartyDTOWithBand Nothing

toPartyDTOWithBand :: Maybe BandDTO -> Bool -> Entity Party -> PartyDTO
toPartyDTOWithBand mBand hasAccount (Entity pid p) = PartyDTO
  { partyId          = fromSqlKey pid
  , legalName        = partyLegalName p
  , displayName      = M.partyDisplayName p
  , isOrg            = partyIsOrg p
  , taxId            = partyTaxId p
  , primaryEmail     = partyPrimaryEmail p
  , primaryPhone     = partyPrimaryPhone p
  , whatsapp         = partyWhatsapp p
  , instagram        = partyInstagram p
  , emergencyContact = partyEmergencyContact p
  , notes            = partyNotes p
  , band             = mBand
  , hasUserAccount   = hasAccount
  }

-- Helper
tshow :: Show a => a -> Text
tshow = T.pack . show

-- Bookings
data BookingResourceDTO = BookingResourceDTO
  { brRoomId   :: Text
  , brRoomName :: Text
  , brRole     :: Text
  } deriving (Show, Generic)
instance ToJSON BookingResourceDTO

data BookingDTO = BookingDTO
  { bookingId   :: Int64
  , title       :: Text
  , startsAt    :: UTCTime
  , endsAt      :: UTCTime
  , status      :: Text
  , notes       :: Maybe Text
  , partyId     :: Maybe Int64
  , engineerPartyId :: Maybe Int64
  , engineerName :: Maybe Text
  , serviceType :: Maybe Text
  , serviceOrderId    :: Maybe Int64
  , serviceOrderTitle :: Maybe Text
  , customerName      :: Maybe Text
  , partyDisplayName  :: Maybe Text
  , resources   :: [BookingResourceDTO]
  , courseSlug        :: Maybe Text
  , coursePrice       :: Maybe Double
  , courseCapacity    :: Maybe Int
  , courseRemaining   :: Maybe Int
  , courseLocation    :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON BookingDTO

-- Packages
data PackageProductDTO = PackageProductDTO
  { ppId         :: Int64
  , ppName       :: Text
  , ppService    :: Text
  , ppUnitsKind  :: Text
  , ppUnitsQty   :: Int
  , ppPriceCents :: Int
  } deriving (Show, Generic)
instance ToJSON PackageProductDTO

data PackagePurchaseReq = PackagePurchaseReq
  { buyerId   :: Int64
  , productId :: Int64
  } deriving (Show, Generic)
instance FromJSON PackagePurchaseReq where
  parseJSON = genericParseJSON strictDecodeOptions

-- Invoices
data InvoiceLineDTO = InvoiceLineDTO
  { lineId             :: Int64
  , description        :: Text
  , quantity           :: Int
  , unitCents          :: Int
  , taxBps             :: Int
  , totalCents         :: Int
  , serviceOrderId     :: Maybe Int64
  , packagePurchaseId  :: Maybe Int64
  } deriving (Show, Generic)
instance ToJSON InvoiceLineDTO

data InvoiceDTO = InvoiceDTO
  { invId        :: Int64
  , number       :: Maybe Text
  , statusI      :: Text
  , subtotalC    :: Int
  , taxC         :: Int
  , totalC       :: Int
  , currency     :: Text
  , customerId   :: Maybe Int64
  , sriDocumentId :: Maybe Text
  , notes        :: Maybe Text
  , receiptId    :: Maybe Int64
  , lineItems    :: [InvoiceLineDTO]
  } deriving (Show, Generic)
instance ToJSON InvoiceDTO

data CreateInvoiceLineReq = CreateInvoiceLineReq
  { cilDescription       :: Text
  , cilQuantity          :: Int
  , cilUnitCents         :: Int
  , cilTaxBps            :: Maybe Int
  , cilServiceOrderId    :: Maybe Int64
  , cilPackagePurchaseId :: Maybe Int64
  } deriving (Show, Generic)
instance FromJSON CreateInvoiceLineReq where
  parseJSON = genericParseJSON strictDecodeOptions

data CreateInvoiceReq = CreateInvoiceReq
  { ciCustomerId      :: Int64
  , ciCurrency        :: Maybe Text
  , ciNumber          :: Maybe Text
  , ciNotes           :: Maybe Text
  , ciLineItems       :: [CreateInvoiceLineReq]
  , ciGenerateReceipt :: Maybe Bool
  } deriving (Show, Generic)
instance FromJSON CreateInvoiceReq where
  parseJSON = genericParseJSON strictDecodeOptions

data GenerateSessionInvoiceLineReq = GenerateSessionInvoiceLineReq
  { gsilDescription       :: Text
  , gsilQuantity          :: Int
  , gsilUnitCents         :: Int
  , gsilTaxBps            :: Maybe Int
  , gsilServiceOrderId    :: Maybe Int64
  , gsilPackagePurchaseId :: Maybe Int64
  , gsilSriCode           :: Maybe Text
  , gsilSriAuxiliaryCode  :: Maybe Text
  , gsilSriAdditionalInfo :: Maybe Text
  , gsilSriIvaCode        :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON GenerateSessionInvoiceLineReq where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dtoCamelDrop 4
    , rejectUnknownFields = True
    }

data GenerateSessionInvoiceReq = GenerateSessionInvoiceReq
  { gsiCustomerId          :: Maybe Int64
  , gsiCurrency            :: Maybe Text
  , gsiNumber              :: Maybe Text
  , gsiNotes               :: Maybe Text
  , gsiLineItems           :: [GenerateSessionInvoiceLineReq]
  , gsiGenerateReceipt     :: Maybe Bool
  , gsiIssueSri            :: Maybe Bool
  , gsiCertificatePassword :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON GenerateSessionInvoiceReq where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dtoCamelDrop 3
    , rejectUnknownFields = True
    }

data SriIssueBuyerDTO = SriIssueBuyerDTO
  { sibRuc       :: Text
  , sibLegalName :: Text
  , sibEmail     :: Maybe Text
  , sibPhone     :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON SriIssueBuyerDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dtoCamelDrop 3, omitNothingFields = True }
instance FromJSON SriIssueBuyerDTO where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dtoCamelDrop 3 }

data SriIssueResultDTO = SriIssueResultDTO
  { sirOk                  :: Bool
  , sirStatus              :: Text
  , sirTargetId            :: Maybe Text
  , sirBuyer               :: Maybe SriIssueBuyerDTO
  , sirTotal               :: Maybe Double
  , sirAuthorizationNumber :: Maybe Text
  , sirInvoiceNumber       :: Maybe Text
  , sirBuyerEmail          :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON SriIssueResultDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dtoCamelDrop 3, omitNothingFields = True }
instance FromJSON SriIssueResultDTO where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dtoCamelDrop 3 }

dtoCamelDrop :: Int -> String -> String
dtoCamelDrop n xs = case drop n xs of
  (c:cs) -> toLower c : cs
  []     -> []

-- Receipts
data ReceiptLineDTO = ReceiptLineDTO
  { receiptLineId :: Int64
  , rlDescription :: Text
  , rlQuantity    :: Int
  , rlUnitCents   :: Int
  , rlTaxBps      :: Maybe Int
  , rlTotalCents  :: Int
  } deriving (Show, Generic)
instance ToJSON ReceiptLineDTO

data ReceiptDTO = ReceiptDTO
  { receiptId    :: Int64
  , receiptNumber :: Text
  , issuedAt     :: UTCTime
  , issueDate    :: Day
  , buyerName    :: Text
  , buyerEmail   :: Maybe Text
  , currency     :: Text
  , subtotalCents :: Int
  , taxCents     :: Int
  , totalCents   :: Int
  , notes        :: Maybe Text
  , invoiceId    :: Int64
  , lineItems    :: [ReceiptLineDTO]
  } deriving (Show, Generic)
instance ToJSON ReceiptDTO

data CreateReceiptReq = CreateReceiptReq
  { crInvoiceId  :: Int64
  , crBuyerName  :: Maybe Text
  , crBuyerEmail :: Maybe Text
  , crNotes      :: Maybe Text
  , crCurrency   :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON CreateReceiptReq where
  parseJSON = genericParseJSON strictDecodeOptions

-- Auth
data LoginRequest = LoginRequest
  { username :: Text
  , password :: Text
  } deriving (Show, Generic)
instance FromJSON LoginRequest where
  parseJSON = genericParseJSON strictDecodeOptions

data GoogleLoginRequest = GoogleLoginRequest
  { idToken :: Text
  } deriving (Show, Generic)
instance FromJSON GoogleLoginRequest where
  parseJSON = genericParseJSON strictDecodeOptions

data SignupRequest = SignupRequest
  { firstName       :: Text
  , lastName        :: Text
  , email           :: Text
  , phone           :: Maybe Text
  , password        :: Text
  , googleIdToken   :: Maybe Text
  , marketingOptIn  :: Maybe Bool
  , internshipStartAt :: Maybe Day
  , internshipEndAt   :: Maybe Day
  , internshipRequiredHours :: Maybe Int
  , internshipSkills  :: Maybe Text
  , internshipAreas   :: Maybe Text
  , roles           :: Maybe [RoleEnum]
  , fanArtistIds    :: Maybe [Int64]
  , claimArtistId   :: Maybe Int64
  } deriving (Show, Generic)
instance FromJSON SignupRequest where
  parseJSON = genericParseJSON strictDecodeOptions

data ChangePasswordRequest = ChangePasswordRequest
  { username        :: Maybe Text
  , currentPassword :: Text
  , newPassword     :: Text
  } deriving (Show, Generic)
instance FromJSON ChangePasswordRequest where
  parseJSON = genericParseJSON strictDecodeOptions

data PasswordResetRequest = PasswordResetRequest
  { email :: Text
  } deriving (Show, Generic)
instance FromJSON PasswordResetRequest where
  parseJSON = genericParseJSON strictDecodeOptions

data PasswordResetConfirmRequest = PasswordResetConfirmRequest
  { token       :: Text
  , newPassword :: Text
  } deriving (Show, Generic)
instance FromJSON PasswordResetConfirmRequest where
  parseJSON = genericParseJSON strictDecodeOptions

data LoginResponse = LoginResponse
  { token   :: Text
  , partyId :: Int64
  , roles   :: [RoleEnum]
  , modules :: [Text]
  } deriving (Show, Generic)
instance ToJSON LoginResponse

data SessionResponse = SessionResponse
  { sessionUsername :: Text
  , sessionDisplayName :: Text
  , sessionPartyId :: Int64
  , sessionRoles :: [RoleEnum]
  , sessionModules :: [Text]
  } deriving (Show, Generic)

instance ToJSON SessionResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dtoCamelDrop 7 }
