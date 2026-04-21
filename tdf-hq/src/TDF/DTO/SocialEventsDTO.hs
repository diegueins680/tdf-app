{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DTO.SocialEventsDTO
  ( ArtistDTO(..)
  , ArtistSocialLinksDTO(..)
  , ArtistFollowerDTO(..)
  , ArtistFollowRequest(..)
  , NullableFieldUpdate(..)
  , VenueDTO(..)
  , VenueContactUpdateDTO(..)
  , VenueUpdateDTO(..)
  , EventDTO(..)
  , EventMetadataUpdateDTO(..)
  , EventUpdateDTO(..)
  , RsvpDTO(..)
  , InvitationDTO(..)
  , InvitationUpdateDTO(..)
  , EventMomentDTO(..)
  , EventMomentCreateDTO(..)
  , EventMomentReactionDTO(..)
  , EventMomentReactionRequestDTO(..)
  , EventMomentCommentDTO(..)
  , EventMomentCommentCreateDTO(..)
  , TicketTierDTO(..)
  , TicketPurchaseRequestDTO(..)
  , TicketOrderStatusUpdateDTO(..)
  , TicketCheckInRequestDTO(..)
  , TicketDTO(..)
  , TicketOrderDTO(..)
  , EventBudgetLineDTO(..)
  , EventFinanceEntryDTO(..)
  , EventFinanceSummaryDTO(..)
  ) where

import           Data.Aeson (FromJSON, ToJSON, Value(..), withObject, (.:), (.:!), (.:?), (.=), object, toJSON, parseJSON)
import           Data.Aeson.Types (Object, Parser)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Data.Time  (UTCTime)
import           GHC.Generics (Generic)

data NullableFieldUpdate a
  = FieldMissing
  | FieldNull
  | FieldValue a
  deriving (Show, Eq, Generic)

nullableFieldFromParsed :: Maybe (Maybe a) -> NullableFieldUpdate a
nullableFieldFromParsed Nothing = FieldMissing
nullableFieldFromParsed (Just Nothing) = FieldNull
nullableFieldFromParsed (Just (Just value)) = FieldValue value

rejectUnknownObjectFields :: String -> [Text] -> Object -> Parser ()
rejectUnknownObjectFields typeName allowedKeys obj =
  case filter (`notElem` allowedKeys) (map AesonKey.toText (AesonKeyMap.keys obj)) of
    [] -> pure ()
    unexpected ->
      fail (typeName <> " contains unknown fields: " <> T.unpack (T.intercalate ", " unexpected))

data ArtistSocialLinksDTO = ArtistSocialLinksDTO
  { aslSpotify    :: Maybe Text
  , aslInstagram  :: Maybe Text
  , aslTwitter    :: Maybe Text
  , aslYoutube    :: Maybe Text
  , aslSoundcloud :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistSocialLinksDTO where
  toJSON ArtistSocialLinksDTO{..} = object
    [ "spotify"    .= aslSpotify
    , "instagram"  .= aslInstagram
    , "twitter"    .= aslTwitter
    , "youtube"    .= aslYoutube
    , "soundcloud" .= aslSoundcloud
    ]

instance FromJSON ArtistSocialLinksDTO where
  parseJSON = withObject "ArtistSocialLinksDTO" $ \o ->
    ArtistSocialLinksDTO
      <$> o .:? "spotify"
      <*> o .:? "instagram"
      <*> o .:? "twitter"
      <*> o .:? "youtube"
      <*> o .:? "soundcloud"

data ArtistDTO = ArtistDTO
  { artistId       :: Maybe Text
  , artistPartyId  :: Maybe Text
  , artistName     :: Text
  , artistGenres   :: [Text]
  , artistBio      :: Maybe Text
  , artistAvatarUrl :: Maybe Text
  , artistSocialLinks :: Maybe ArtistSocialLinksDTO
  , artistCreatedAt :: Maybe UTCTime
  , artistUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON ArtistDTO
instance FromJSON ArtistDTO where
  parseJSON = withObject "ArtistDTO" $ \o -> do
    artistId <- o .:? "artistId"
    artistPartyId <- o .:? "artistPartyId"
    mName <- o .:? "artistName"
    mGenres <- o .:? "artistGenres"
    artistBio <- o .:? "artistBio"
    artistAvatarUrl <- o .:? "artistAvatarUrl"
    artistSocialLinks <- o .:? "artistSocialLinks"
    artistCreatedAt <- o .:? "artistCreatedAt"
    artistUpdatedAt <- o .:? "artistUpdatedAt"
    pure ArtistDTO
      { artistId = artistId
      , artistPartyId = artistPartyId
      , artistName = maybe "" id mName
      , artistGenres = maybe [] id mGenres
      , artistBio = artistBio
      , artistAvatarUrl = artistAvatarUrl
      , artistSocialLinks = artistSocialLinks
      , artistCreatedAt = artistCreatedAt
      , artistUpdatedAt = artistUpdatedAt
      }

data ArtistFollowerDTO = ArtistFollowerDTO
  { afFollowId         :: Maybe Text
  , afArtistId         :: Maybe Text
  , afFollowerPartyId  :: Text
  , afCreatedAt        :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistFollowerDTO where
  toJSON ArtistFollowerDTO{..} = object
    [ "followId" .= afFollowId
    , "artistId" .= afArtistId
    , "followerPartyId" .= afFollowerPartyId
    , "createdAt" .= afCreatedAt
    ]

instance FromJSON ArtistFollowerDTO where
  parseJSON = withObject "ArtistFollowerDTO" $ \o ->
    ArtistFollowerDTO
      <$> o .:? "followId"
      <*> o .:? "artistId"
      <*> o .:  "followerPartyId"
      <*> o .:? "createdAt"

data ArtistFollowRequest = ArtistFollowRequest
  { afrFollowerPartyId :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistFollowRequest where
  toJSON ArtistFollowRequest{..} = object
    [ "followerPartyId" .= afrFollowerPartyId
    ]

instance FromJSON ArtistFollowRequest where
  parseJSON = withObject "ArtistFollowRequest" $ \o ->
    ArtistFollowRequest
      <$> o .: "followerPartyId"

data VenueDTO = VenueDTO
  { venueId       :: Maybe Text
  , venueName     :: Text
  , venueAddress  :: Maybe Text
  , venueCity     :: Maybe Text
  , venueCountry  :: Maybe Text
  , venueLat      :: Maybe Double
  , venueLng      :: Maybe Double
  , venueCapacity :: Maybe Int
  , venueContact  :: Maybe Text
  , venuePhone    :: Maybe Text
  , venueWebsite  :: Maybe Text
  , venueState    :: Maybe Text
  , venueZipCode  :: Maybe Text
  , venueImageUrl :: Maybe Text
  , venueCreatedAt :: Maybe UTCTime
  , venueUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON VenueDTO
instance FromJSON VenueDTO

data VenueContactUpdateDTO = VenueContactUpdateDTO
  { vcuPhone    :: NullableFieldUpdate Text
  , vcuWebsite  :: NullableFieldUpdate Text
  , vcuState    :: NullableFieldUpdate Text
  , vcuZipCode  :: NullableFieldUpdate Text
  , vcuImageUrl :: NullableFieldUpdate Text
  } deriving (Show, Eq, Generic)

data VenueUpdateDTO = VenueUpdateDTO
  { vudVenue         :: VenueDTO
  , vudContactUpdate :: VenueContactUpdateDTO
  } deriving (Show, Eq, Generic)

instance FromJSON VenueUpdateDTO where
  parseJSON value@(Object o) =
    VenueUpdateDTO
      <$> parseJSON value
      <*> (VenueContactUpdateDTO
            <$> (nullableFieldFromParsed <$> (o .:! "venuePhone"))
            <*> (nullableFieldFromParsed <$> (o .:! "venueWebsite"))
            <*> (nullableFieldFromParsed <$> (o .:! "venueState"))
            <*> (nullableFieldFromParsed <$> (o .:! "venueZipCode"))
            <*> (nullableFieldFromParsed <$> (o .:! "venueImageUrl")))
  parseJSON _ = fail "VenueUpdateDTO must be an object"

data EventDTO = EventDTO
  { eventId          :: Maybe Text
  , eventOrganizerPartyId :: Maybe Text
  , eventTitle       :: Text
  , eventDescription :: Maybe Text
  , eventStart       :: UTCTime
  , eventEnd         :: UTCTime
  , eventVenueId     :: Maybe Text
  , eventPriceCents  :: Maybe Int
  , eventCapacity    :: Maybe Int
  , eventTicketUrl   :: Maybe Text
  , eventImageUrl    :: Maybe Text
  , eventIsPublic    :: Maybe Bool
  , eventType        :: Maybe Text
  , eventStatus      :: Maybe Text
  , eventCurrency    :: Maybe Text
  , eventBudgetCents :: Maybe Int
  , eventCreatedAt   :: Maybe UTCTime
  , eventUpdatedAt   :: Maybe UTCTime
  , eventArtists     :: [ArtistDTO]
  } deriving (Show, Eq, Generic)
instance ToJSON EventDTO
instance FromJSON EventDTO

data EventMetadataUpdateDTO = EventMetadataUpdateDTO
  { emuTicketUrl   :: NullableFieldUpdate Text
  , emuImageUrl    :: NullableFieldUpdate Text
  , emuIsPublic    :: NullableFieldUpdate Bool
  , emuType        :: NullableFieldUpdate Text
  , emuStatus      :: NullableFieldUpdate Text
  , emuCurrency    :: NullableFieldUpdate Text
  , emuBudgetCents :: NullableFieldUpdate Int
  } deriving (Show, Eq, Generic)

data EventUpdateDTO = EventUpdateDTO
  { eudEvent          :: EventDTO
  , eudMetadataUpdate :: EventMetadataUpdateDTO
  } deriving (Show, Eq, Generic)

instance FromJSON EventUpdateDTO where
  parseJSON value@(Object o) = do
    rejectUnknownObjectFields "EventUpdateDTO" eventUpdateAllowedKeys o
    EventUpdateDTO
      <$> parseJSON value
      <*> (EventMetadataUpdateDTO
            <$> (nullableFieldFromParsed <$> (o .:! "eventTicketUrl"))
            <*> (nullableFieldFromParsed <$> (o .:! "eventImageUrl"))
            <*> (nullableFieldFromParsed <$> (o .:! "eventIsPublic"))
            <*> (nullableFieldFromParsed <$> (o .:! "eventType"))
            <*> (nullableFieldFromParsed <$> (o .:! "eventStatus"))
            <*> (nullableFieldFromParsed <$> (o .:! "eventCurrency"))
            <*> (nullableFieldFromParsed <$> (o .:! "eventBudgetCents")))
  parseJSON _ = fail "EventUpdateDTO must be an object"

eventUpdateAllowedKeys :: [Text]
eventUpdateAllowedKeys =
  [ "eventId"
  , "eventOrganizerPartyId"
  , "eventTitle"
  , "eventDescription"
  , "eventStart"
  , "eventEnd"
  , "eventVenueId"
  , "eventPriceCents"
  , "eventCapacity"
  , "eventTicketUrl"
  , "eventImageUrl"
  , "eventIsPublic"
  , "eventType"
  , "eventStatus"
  , "eventCurrency"
  , "eventBudgetCents"
  , "eventCreatedAt"
  , "eventUpdatedAt"
  , "eventArtists"
  ]

data RsvpDTO = RsvpDTO
  { rsvpId        :: Maybe Text
  , rsvpEventId   :: Text
  , rsvpPartyId   :: Text
  , rsvpStatus    :: Text  -- "Accepted", "Declined", "Maybe"
  , rsvpCreatedAt :: Maybe UTCTime
  , rsvpUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON RsvpDTO
instance FromJSON RsvpDTO

data InvitationDTO = InvitationDTO
  { invitationId         :: Maybe Text
  , invitationEventId    :: Maybe Text
  , invitationFromPartyId :: Maybe Text
  , invitationToPartyId  :: Text
  , invitationStatus     :: Maybe Text
  , invitationMessage    :: Maybe Text
  , invitationCreatedAt  :: Maybe UTCTime
  , invitationUpdatedAt  :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON InvitationDTO
instance FromJSON InvitationDTO

data InvitationUpdateDTO = InvitationUpdateDTO
  { iudInvitation    :: InvitationDTO
  , iudMessageUpdate :: NullableFieldUpdate Text
  } deriving (Show, Eq, Generic)

instance FromJSON InvitationUpdateDTO where
  parseJSON value@(Object o) =
    InvitationUpdateDTO
      <$> parseJSON value
      <*> (nullableFieldFromParsed <$> (o .:! "invitationMessage"))
  parseJSON _ = fail "InvitationUpdateDTO must be an object"

data EventMomentReactionDTO = EventMomentReactionDTO
  { emrReaction  :: Text
  , emrPartyId   :: Text
  , emrCreatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON EventMomentReactionDTO
instance FromJSON EventMomentReactionDTO

data EventMomentCommentDTO = EventMomentCommentDTO
  { emcId            :: Maybe Text
  , emcMomentId      :: Maybe Text
  , emcAuthorPartyId :: Maybe Text
  , emcAuthorName    :: Text
  , emcBody          :: Text
  , emcCreatedAt     :: Maybe UTCTime
  , emcUpdatedAt     :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON EventMomentCommentDTO
instance FromJSON EventMomentCommentDTO

data EventMomentDTO = EventMomentDTO
  { emId              :: Maybe Text
  , emEventId         :: Maybe Text
  , emAuthorPartyId   :: Maybe Text
  , emAuthorName      :: Text
  , emCaption         :: Maybe Text
  , emMediaUrl        :: Text
  , emMediaType       :: Text
  , emMediaWidth      :: Maybe Int
  , emMediaHeight     :: Maybe Int
  , emMediaDurationMs :: Maybe Int
  , emCreatedAt       :: Maybe UTCTime
  , emUpdatedAt       :: Maybe UTCTime
  , emReactions       :: [EventMomentReactionDTO]
  , emComments        :: [EventMomentCommentDTO]
  } deriving (Show, Eq, Generic)
instance ToJSON EventMomentDTO
instance FromJSON EventMomentDTO

data EventMomentCreateDTO = EventMomentCreateDTO
  { emCreateAuthorName      :: Maybe Text
  , emCreateCaption         :: Maybe Text
  , emCreateMediaUrl        :: Text
  , emCreateMediaType       :: Text
  , emCreateMediaWidth      :: Maybe Int
  , emCreateMediaHeight     :: Maybe Int
  , emCreateMediaDurationMs :: Maybe Int
  } deriving (Show, Eq, Generic)
instance ToJSON EventMomentCreateDTO
instance FromJSON EventMomentCreateDTO

data EventMomentReactionRequestDTO = EventMomentReactionRequestDTO
  { emrrReaction :: Text
  } deriving (Show, Eq, Generic)
instance ToJSON EventMomentReactionRequestDTO
instance FromJSON EventMomentReactionRequestDTO

data EventMomentCommentCreateDTO = EventMomentCommentCreateDTO
  { emccAuthorName :: Maybe Text
  , emccBody       :: Text
  } deriving (Show, Eq, Generic)
instance ToJSON EventMomentCommentCreateDTO
instance FromJSON EventMomentCommentCreateDTO

data TicketTierDTO = TicketTierDTO
  { ticketTierId            :: Maybe Text
  , ticketTierEventId       :: Maybe Text
  , ticketTierCode          :: Text
  , ticketTierName          :: Text
  , ticketTierDescription   :: Maybe Text
  , ticketTierPriceCents    :: Int
  , ticketTierCurrency      :: Text
  , ticketTierQuantityTotal :: Int
  , ticketTierQuantitySold  :: Int
  , ticketTierSalesStart    :: Maybe UTCTime
  , ticketTierSalesEnd      :: Maybe UTCTime
  , ticketTierActive        :: Bool
  , ticketTierPosition      :: Maybe Int
  } deriving (Show, Eq, Generic)
instance ToJSON TicketTierDTO
instance FromJSON TicketTierDTO

data TicketPurchaseRequestDTO = TicketPurchaseRequestDTO
  { ticketPurchaseTierId    :: Text
  , ticketPurchaseQuantity  :: Int
  , ticketPurchaseBuyerPartyId :: Maybe Text
  , ticketPurchaseBuyerName :: Maybe Text
  , ticketPurchaseBuyerEmail :: Maybe Text
  } deriving (Show, Eq, Generic)
instance ToJSON TicketPurchaseRequestDTO
instance FromJSON TicketPurchaseRequestDTO where
  parseJSON = withObject "TicketPurchaseRequestDTO" $ \o -> do
    rejectUnknownObjectFields
      "TicketPurchaseRequestDTO"
      [ "ticketPurchaseTierId"
      , "ticketPurchaseQuantity"
      , "ticketPurchaseBuyerPartyId"
      , "ticketPurchaseBuyerName"
      , "ticketPurchaseBuyerEmail"
      ]
      o
    TicketPurchaseRequestDTO
      <$> o .: "ticketPurchaseTierId"
      <*> o .: "ticketPurchaseQuantity"
      <*> o .:? "ticketPurchaseBuyerPartyId"
      <*> o .:? "ticketPurchaseBuyerName"
      <*> o .:? "ticketPurchaseBuyerEmail"

data TicketOrderStatusUpdateDTO = TicketOrderStatusUpdateDTO
  { ticketOrderStatus :: Text
  } deriving (Show, Eq, Generic)
instance ToJSON TicketOrderStatusUpdateDTO
instance FromJSON TicketOrderStatusUpdateDTO where
  parseJSON = withObject "TicketOrderStatusUpdateDTO" $ \o -> do
    rejectUnknownObjectFields
      "TicketOrderStatusUpdateDTO"
      [ "ticketOrderStatus"
      ]
      o
    TicketOrderStatusUpdateDTO
      <$> o .: "ticketOrderStatus"

data TicketCheckInRequestDTO = TicketCheckInRequestDTO
  { ticketCheckInTicketId   :: Maybe Text
  , ticketCheckInTicketCode :: Maybe Text
  } deriving (Show, Eq, Generic)
instance ToJSON TicketCheckInRequestDTO
instance FromJSON TicketCheckInRequestDTO where
  parseJSON = withObject "TicketCheckInRequestDTO" $ \o -> do
    rejectUnknownObjectFields
      "TicketCheckInRequestDTO"
      [ "ticketCheckInTicketId"
      , "ticketCheckInTicketCode"
      ]
      o
    TicketCheckInRequestDTO
      <$> o .:? "ticketCheckInTicketId"
      <*> o .:? "ticketCheckInTicketCode"

data TicketDTO = TicketDTO
  { ticketId          :: Maybe Text
  , ticketEventId     :: Maybe Text
  , ticketTierId      :: Maybe Text
  , ticketOrderId     :: Maybe Text
  , ticketCode        :: Text
  , ticketStatus      :: Text
  , ticketHolderName  :: Maybe Text
  , ticketHolderEmail :: Maybe Text
  , ticketCheckedInAt :: Maybe UTCTime
  , ticketCreatedAt   :: Maybe UTCTime
  , ticketUpdatedAt   :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON TicketDTO
instance FromJSON TicketDTO

data TicketOrderDTO = TicketOrderDTO
  { ticketOrderId          :: Maybe Text
  , ticketOrderEventId     :: Maybe Text
  , ticketOrderTierId      :: Maybe Text
  , ticketOrderBuyerPartyId :: Maybe Text
  , ticketOrderBuyerName   :: Maybe Text
  , ticketOrderBuyerEmail  :: Maybe Text
  , ticketOrderQuantity    :: Int
  , ticketOrderAmountCents :: Int
  , ticketOrderCurrency    :: Text
  , ticketOrderStatusValue :: Text
  , ticketOrderPurchasedAt :: Maybe UTCTime
  , ticketOrderCreatedAt   :: Maybe UTCTime
  , ticketOrderUpdatedAt   :: Maybe UTCTime
  , ticketOrderTickets     :: [TicketDTO]
  } deriving (Show, Eq, Generic)
instance ToJSON TicketOrderDTO
instance FromJSON TicketOrderDTO

data EventBudgetLineDTO = EventBudgetLineDTO
  { eblId           :: Maybe Text
  , eblEventId      :: Maybe Text
  , eblCode         :: Text
  , eblName         :: Text
  , eblType         :: Text
  , eblCategory     :: Text
  , eblPlannedCents :: Int
  , eblActualCents  :: Maybe Int
  , eblNotes        :: Maybe Text
  , eblCreatedAt    :: Maybe UTCTime
  , eblUpdatedAt    :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON EventBudgetLineDTO
instance FromJSON EventBudgetLineDTO

data EventFinanceEntryDTO = EventFinanceEntryDTO
  { efeId               :: Maybe Text
  , efeEventId          :: Maybe Text
  , efeBudgetLineId     :: Maybe Text
  , efeDirection        :: Text
  , efeSource           :: Text
  , efeCategory         :: Text
  , efeConcept          :: Text
  , efeAmountCents      :: Int
  , efeCurrency         :: Text
  , efeStatus           :: Text
  , efeExternalRef      :: Maybe Text
  , efeNotes            :: Maybe Text
  , efeOccurredAt       :: UTCTime
  , efeRecordedByPartyId :: Maybe Text
  , efeCreatedAt        :: Maybe UTCTime
  , efeUpdatedAt        :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON EventFinanceEntryDTO
instance FromJSON EventFinanceEntryDTO

data EventFinanceSummaryDTO = EventFinanceSummaryDTO
  { efsEventId                   :: Text
  , efsCurrency                  :: Text
  , efsBudgetCents               :: Maybe Int
  , efsPlannedIncomeCents        :: Int
  , efsPlannedExpenseCents       :: Int
  , efsActualIncomeCents         :: Int
  , efsActualExpenseCents        :: Int
  , efsNetCents                  :: Int
  , efsTicketPaidRevenueCents    :: Int
  , efsTicketRefundedRevenueCents :: Int
  , efsTicketPendingRevenueCents :: Int
  , efsAccountsPayableCents      :: Int
  , efsAccountsReceivableCents   :: Int
  , efsContractCommittedCents    :: Int
  , efsContractPaidCents         :: Int
  , efsProcurementCommittedCents :: Int
  , efsProcurementPaidCents      :: Int
  , efsAssetInvestmentCents      :: Int
  , efsLiabilityBalanceCents     :: Int
  , efsBudgetVarianceCents       :: Maybe Int
  , efsBudgetUtilizationPct      :: Maybe Double
  , efsGeneratedAt               :: UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON EventFinanceSummaryDTO
instance FromJSON EventFinanceSummaryDTO
