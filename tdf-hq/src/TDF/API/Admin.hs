{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.Admin where

import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Time     (UTCTime)
import           Servant

import           TDF.API.Types ( DropdownOptionCreate
                                , DropdownOptionDTO
                                , DropdownOptionUpdate
                                , RoleDetailDTO
                                , UserAccountCreate
                                , UserAccountDTO
                                , UserAccountUpdate
                                )
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, rejectUnknownFields, withObject, (.:!), (.:?))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Char      ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
                                , generalCategory
                                , isControl
                                , toLower
                                )
import           GHC.Generics (Generic)
import           Data.Int      (Int64)
import           TDF.DTO       (ArtistProfileDTO, ArtistProfileUpsert, ArtistReleaseDTO, ArtistReleaseUpsert, LogEntryDTO, UserActivityDTO)

type DropdownCategoryAPI =
       QueryParam "includeInactive" Bool :> Get '[JSON] [DropdownOptionDTO]
  :<|> ReqBody '[JSON] DropdownOptionCreate :> PostCreated '[JSON] DropdownOptionDTO
  :<|> Capture "optionId" Text :> ReqBody '[JSON] DropdownOptionUpdate :> Patch '[JSON] DropdownOptionDTO

type UsersAPI =
       QueryParam "includeInactive" Bool :> Get '[JSON] [UserAccountDTO]
  :<|> ReqBody '[JSON] UserAccountCreate :> PostCreated '[JSON] UserAccountDTO
  :<|> Capture "userId" Int64 :>
        ( Get '[JSON] UserAccountDTO
     :<|> ReqBody '[JSON] UserAccountUpdate :> Patch '[JSON] UserAccountDTO
        )

type UserCommunicationsAPI =
       "users" :> Capture "userId" Int64 :> "communications"
         :> QueryParam "limit" Int
         :> Get '[JSON] UserCommunicationHistoryDTO
  :<|> "users" :> Capture "userId" Int64 :> "communications" :> "whatsapp"
         :> ReqBody '[JSON] AdminWhatsAppSendRequest
         :> Post '[JSON] AdminWhatsAppSendResponse
  :<|> "communications" :> "whatsapp" :> Capture "messageId" Int64 :> "resend"
         :> ReqBody '[JSON] AdminWhatsAppResendRequest
         :> Post '[JSON] AdminWhatsAppSendResponse
  :<|> "communications" :> "email" :> "registered-users"
         :> ReqBody '[JSON] AdminEmailBroadcastRequest
         :> Post '[JSON] AdminEmailBroadcastResponse

type RolesAPI = Get '[JSON] [RoleDetailDTO]

type ArtistAdminAPI =
       "profiles" :>
         ( Get '[JSON] [ArtistProfileDTO]
       :<|> ReqBody '[JSON] ArtistProfileUpsert :> Post '[JSON] ArtistProfileDTO
         )
  :<|> "releases" :>
         ( ReqBody '[JSON] ArtistReleaseUpsert :> Post '[JSON] ArtistReleaseDTO
       :<|> Capture "releaseId" Int64 :> ReqBody '[JSON] ArtistReleaseUpsert :> Put '[JSON] ArtistReleaseDTO
         )
  :<|> "profiles" :> Capture "artistProfileId" Int64 :> "connect" :> "onboarding-link"
         :> ReqBody '[JSON] ConnectOnboardingLinkRequest
         :> Post '[JSON] ConnectOnboardingLinkResponse

type LogsAPI =
       QueryParam "limit" Int :> Get '[JSON] [LogEntryDTO]
  :<|> Delete '[JSON] NoContent

type ActivityAPI =
       QueryParam "limit" Int :> Get '[JSON] [UserActivityDTO]

type BrainAdminAPI =
       "brain" :> "entries"
         :> QueryParam "includeInactive" Bool
         :> Get '[JSON] [BrainEntryDTO]
  :<|> "brain" :> "entries"
         :> ReqBody '[JSON] BrainEntryCreate
         :> PostCreated '[JSON] BrainEntryDTO
  :<|> "brain" :> "entries" :> Capture "entryId" Int64
         :> ReqBody '[JSON] BrainEntryUpdate
         :> Patch '[JSON] BrainEntryDTO

type RagAdminAPI =
       "rag" :> "status" :> Get '[JSON] RagIndexStatus
  :<|> "rag" :> "refresh" :> Post '[JSON] RagRefreshResponse

type SocialAdminAPI =
       "social" :> "unhold" :> ReqBody '[JSON] SocialUnholdRequest :> Post '[JSON] Value
  :<|> "social" :> "status" :> Get '[JSON] Value
  :<|> "social" :> "errors" :> QueryParam "channel" Text :> QueryParam "limit" Int :> Get '[JSON] Value

type AdminAPI =
       "seed" :> Header "X-Seed-Token" Text :> Post '[JSON] NoContent
  :<|> "dropdowns" :> Capture "category" Text :> DropdownCategoryAPI
  :<|> "users" :> UsersAPI
  :<|> UserCommunicationsAPI
  :<|> "roles" :> RolesAPI
  :<|> "artists" :> ArtistAdminAPI
  :<|> "logs" :> LogsAPI
  :<|> "activity" :> ActivityAPI
  :<|> "email-test" :> ReqBody '[JSON] EmailTestRequest :> Post '[JSON] EmailTestResponse
  :<|> BrainAdminAPI
  :<|> RagAdminAPI
  :<|> SocialAdminAPI

-- | Unhold a social inbound message so the auto-reply worker can retry.
-- One of externalId/ids should be provided.
data SocialUnholdRequest = SocialUnholdRequest
  { surChannel    :: Text  -- instagram|facebook|whatsapp
  , surExternalId :: Maybe Text
  , surSenderId   :: Maybe Text
  , surNote       :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON SocialUnholdRequest where
  parseJSON value = do
    mapM_
      (\fieldName ->
        rejectNullOptionalField
          "SocialUnholdRequest"
          fieldName
          (T.unpack fieldName <> " must be omitted instead of null")
          value)
      ["externalId", "senderId", "note"]
    request <- genericParseJSON defaultOptions
      { fieldLabelModifier = camelDrop 3
      , rejectUnknownFields = True
      } value
    channel <- normalizeSocialUnholdChannel (surChannel request)
    case
      ( normalizeLookupField (surExternalId request)
      , normalizeLookupField (surSenderId request)
      ) of
      (Just externalId, Nothing) ->
        pure request
          { surChannel = channel
          , surExternalId = Just externalId
          , surSenderId = Nothing
          }
      (Nothing, Just senderId) ->
        pure request
          { surChannel = channel
          , surExternalId = Nothing
          , surSenderId = Just senderId
          }
      (Nothing, Nothing) ->
        fail "SocialUnholdRequest requires externalId or senderId"
      (Just _, Just _) ->
        fail "SocialUnholdRequest requires exactly one of externalId or senderId"
    where
      normalizeLookupField rawValue =
        case T.strip <$> rawValue of
          Just lookupValue | not (T.null lookupValue) -> Just lookupValue
          _ -> Nothing

      normalizeSocialUnholdChannel rawChannel
        | T.null channel =
            fail "SocialUnholdRequest channel is required"
        | T.any isControl rawChannel =
            fail "SocialUnholdRequest channel must not contain control characters"
        | T.any isHiddenChannelChar rawChannel =
            fail "SocialUnholdRequest channel must not contain hidden formatting characters"
        | channel `elem` ["instagram", "facebook", "whatsapp"] =
            pure channel
        | otherwise =
            fail "SocialUnholdRequest channel must be one of: instagram, facebook, whatsapp"
        where
          channel = T.toLower (T.strip rawChannel)

      isHiddenChannelChar ch =
        generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

data AdminWhatsAppSendRequest = AdminWhatsAppSendRequest
  { awsrMessage          :: Text
  , awsrMode             :: Text
  , awsrReplyToMessageId :: Maybe Int64
  } deriving (Show, Generic)

instance FromJSON AdminWhatsAppSendRequest where
  parseJSON value = do
    rejectNullOptionalField
      "AdminWhatsAppSendRequest"
      "replyToMessageId"
      "replyToMessageId must be omitted instead of null"
      value
    genericParseJSON defaultOptions
      { fieldLabelModifier = camelDrop 4
      , rejectUnknownFields = True
      } value

data AdminWhatsAppResendRequest = AdminWhatsAppResendRequest
  { awrrMessage :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON AdminWhatsAppResendRequest where
  parseJSON value = do
    rejectNullOptionalField
      "AdminWhatsAppResendRequest"
      "message"
      "message must be omitted instead of null"
      value
    request <- genericParseJSON defaultOptions
      { fieldLabelModifier = camelDrop 4
      , rejectUnknownFields = True
      } value
    messageValue <- traverse normalizeOverrideMessage (awrrMessage request)
    pure request { awrrMessage = messageValue }
    where
      normalizeOverrideMessage rawMessage =
        let trimmedMessage = T.strip rawMessage
        in if T.null trimmedMessage
             then fail "message cannot be blank; omit message to resend the original text"
             else pure trimmedMessage

rejectNullOptionalField :: String -> Text -> String -> Value -> Parser ()
rejectNullOptionalField objectName fieldName message =
  withObject objectName $ \o ->
    case KeyMap.lookup (Key.fromText fieldName) o of
      Just Null -> fail message
      _         -> pure ()

data WhatsAppMessageAdminDTO = WhatsAppMessageAdminDTO
  { wmdId                :: Int64
  , wmdExternalId        :: Text
  , wmdPartyId           :: Maybe Int64
  , wmdActorPartyId      :: Maybe Int64
  , wmdSenderId          :: Text
  , wmdSenderName        :: Maybe Text
  , wmdPhoneE164         :: Maybe Text
  , wmdContactEmail      :: Maybe Text
  , wmdText              :: Maybe Text
  , wmdDirection         :: Text
  , wmdReplyStatus       :: Text
  , wmdReplyError        :: Maybe Text
  , wmdRepliedAt         :: Maybe UTCTime
  , wmdReplyText         :: Maybe Text
  , wmdDeliveryStatus    :: Text
  , wmdDeliveryUpdatedAt :: Maybe UTCTime
  , wmdDeliveryError     :: Maybe Text
  , wmdSource            :: Maybe Text
  , wmdResendOfMessageId :: Maybe Int64
  , wmdCreatedAt         :: UTCTime
  } deriving (Show, Generic)

instance ToJSON WhatsAppMessageAdminDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data UserCommunicationHistoryDTO = UserCommunicationHistoryDTO
  { uchUserId       :: Int64
  , uchPartyId      :: Int64
  , uchPartyName    :: Text
  , uchUsername     :: Text
  , uchPrimaryEmail :: Maybe Text
  , uchPrimaryPhone :: Maybe Text
  , uchWhatsapp     :: Maybe Text
  , uchMessages     :: [WhatsAppMessageAdminDTO]
  } deriving (Show, Generic)

instance ToJSON UserCommunicationHistoryDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data AdminWhatsAppSendResponse = AdminWhatsAppSendResponse
  { awspStatus         :: Text
  , awspMessageId      :: Maybe Int64
  , awspDeliveryStatus :: Text
  , awspMessage        :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON AdminWhatsAppSendResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 4 }

data AdminEmailBroadcastRequest = AdminEmailBroadcastRequest
  { aebrSubject         :: Text
  , aebrBodyLines       :: [Text]
  , aebrDryRun          :: Maybe Bool
  , aebrLimit           :: Maybe Int
  , aebrIncludeInactive :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON AdminEmailBroadcastRequest where
  parseJSON value = do
    case value of
      Object o ->
        mapM_ (rejectNull o) ["dryRun", "limit", "includeInactive"]
      _ ->
        pure ()
    genericParseJSON broadcastRequestOptions value
    where
      rejectNull o fieldName =
        case KeyMap.lookup (Key.fromText fieldName) o of
          Just Null ->
            fail (T.unpack fieldName <> " must be omitted instead of null")
          _ ->
            pure ()
      broadcastRequestOptions =
        defaultOptions
          { fieldLabelModifier = camelDrop 4
          , rejectUnknownFields = True
          }

data AdminEmailBroadcastRecipientDTO = AdminEmailBroadcastRecipientDTO
  { aerdEmail   :: Text
  , aerdName    :: Text
  , aerdStatus  :: Text
  , aerdMessage :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON AdminEmailBroadcastRecipientDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 4 }

data AdminEmailBroadcastResponse = AdminEmailBroadcastResponse
  { aersStatus             :: Text
  , aersDryRun             :: Bool
  , aersMatchedUsers       :: Int
  , aersUniqueRecipients   :: Int
  , aersProcessedRecipients :: Int
  , aersSentCount          :: Int
  , aersFailedCount        :: Int
  , aersRecipients         :: [AdminEmailBroadcastRecipientDTO]
  } deriving (Show, Generic)

instance ToJSON AdminEmailBroadcastResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 4 }


data EmailTestRequest = EmailTestRequest
  { etrEmail   :: Text
  , etrName    :: Maybe Text
  , etrSubject :: Maybe Text
  , etrBody    :: Maybe Text
  , etrCtaUrl  :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON EmailTestRequest where
  parseJSON value = do
    mapM_
      (\fieldName ->
        rejectNullOptionalField
          "EmailTestRequest"
          fieldName
          (T.unpack fieldName <> " must be omitted instead of null")
          value)
      ["name", "subject", "body", "ctaUrl"]
    genericParseJSON defaultOptions
      { fieldLabelModifier = camelDrop 3
      , rejectUnknownFields = True
      } value

data EmailTestResponse = EmailTestResponse
  { status  :: Text
  , message :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON EmailTestResponse

data BrainEntryDTO = BrainEntryDTO
  { bedId        :: Int64
  , bedTitle     :: Text
  , bedBody      :: Text
  , bedCategory  :: Maybe Text
  , bedTags      :: [Text]
  , bedActive    :: Bool
  , bedUpdatedAt :: UTCTime
  } deriving (Show, Generic)
instance ToJSON BrainEntryDTO

data BrainEntryCreate = BrainEntryCreate
  { becTitle    :: Text
  , becBody     :: Text
  , becCategory :: Maybe Text
  , becTags     :: Maybe [Text]
  , becActive   :: Maybe Bool
  } deriving (Show, Generic)
instance FromJSON BrainEntryCreate where
  parseJSON value = do
    mapM_
      (\fieldName ->
        rejectNullOptionalField
          "BrainEntryCreate"
          fieldName
          (T.unpack fieldName <> " must be omitted instead of null")
          value)
      ["becCategory", "becTags", "becActive"]
    genericParseJSON defaultOptions { rejectUnknownFields = True } value

data BrainEntryUpdate = BrainEntryUpdate
  { beuTitle    :: Maybe Text
  , beuBody     :: Maybe Text
  , beuCategory :: Maybe (Maybe Text)
  , beuTags     :: Maybe [Text]
  , beuActive   :: Maybe Bool
  } deriving (Show, Generic)
instance FromJSON BrainEntryUpdate where
  parseJSON = withObject "BrainEntryUpdate" $ \o -> do
    let providedKeys = map Key.toString (KeyMap.keys o)
        rejectNullNonClearableField fieldName =
          case KeyMap.lookup (Key.fromText (T.pack fieldName)) o of
            Just Null -> fail (fieldName <> " must be omitted instead of null")
            _ -> pure ()
    case filter (`notElem` brainEntryUpdateAllowedKeys) providedKeys of
      [] -> pure ()
      unexpected ->
        fail ("Unexpected BrainEntryUpdate keys: " <> show unexpected)
    mapM_ rejectNullNonClearableField
      [ "beuTitle"
      , "beuBody"
      , "beuTags"
      , "beuActive"
      ]
    if null providedKeys
      then fail "BrainEntryUpdate must include at least one field"
      else pure ()
    titleVal <- o .:? "beuTitle"
    bodyVal <- o .:? "beuBody"
    categoryVal <- o .:! "beuCategory"
    tagsVal <- o .:? "beuTags"
    activeVal <- o .:? "beuActive"
    pure BrainEntryUpdate
      { beuTitle = titleVal
      , beuBody = bodyVal
      , beuCategory = categoryVal
      , beuTags = tagsVal
      , beuActive = activeVal
      }
    where
      brainEntryUpdateAllowedKeys =
        [ "beuTitle"
        , "beuBody"
        , "beuCategory"
        , "beuTags"
        , "beuActive"
        ]

data RagIndexStatus = RagIndexStatus
  { risCount     :: Int
  , risUpdatedAt :: Maybe UTCTime
  , risStale     :: Bool
  } deriving (Show, Generic)
instance ToJSON RagIndexStatus

data RagRefreshResponse = RagRefreshResponse
  { rrrStatus :: Text
  , rrrChunks :: Int
  } deriving (Show, Generic)
instance ToJSON RagRefreshResponse

camelDrop :: Int -> String -> String
camelDrop n xs = case drop n xs of
  (c:cs) -> toLower c : cs
  []     -> []

-- | Body for `POST /admin/artists/profiles/:id/connect/onboarding-link`.
-- Both URLs must be absolute https URLs Stripe can redirect the artist to.
data ConnectOnboardingLinkRequest = ConnectOnboardingLinkRequest
  { colRefreshUrl :: Text
  , colReturnUrl  :: Text
  -- | ISO-3166 alpha-2 country. Used only when this is the artist's first
  -- Connect Express account; ignored on subsequent regenerations of the link.
  , colCountry    :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON ConnectOnboardingLinkRequest where
  parseJSON = withObject "ConnectOnboardingLinkRequest" $ \o -> do
    rRefresh <- o .:! "colRefreshUrl"
    rReturn  <- o .:! "colReturnUrl"
    rCountry <- o .:? "colCountry"
    case (rRefresh, rReturn) of
      (Just rf, Just rt) -> pure (ConnectOnboardingLinkRequest rf rt rCountry)
      _ -> fail "ConnectOnboardingLinkRequest requires colRefreshUrl and colReturnUrl"
instance ToJSON ConnectOnboardingLinkRequest where
  toJSON = genericToJSON defaultOptions { rejectUnknownFields = True }

data ConnectOnboardingLinkResponse = ConnectOnboardingLinkResponse
  { colAccountId     :: Text
  , colOnboardingUrl :: Text
  -- | True when the account was created by this request; false when an
  -- existing @acct_*@ id was reused. Helpful for analytics on onboarding
  -- progress.
  , colAccountCreated :: Bool
  } deriving (Show, Generic)
instance ToJSON ConnectOnboardingLinkResponse
instance FromJSON ConnectOnboardingLinkResponse
