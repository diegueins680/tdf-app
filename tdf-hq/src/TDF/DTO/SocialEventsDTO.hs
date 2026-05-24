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
  , RsvpCreateDTO(..)
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
  , maxTicketPurchaseQuantity
  , TicketOrderStatusUpdateDTO(..)
  , TicketCheckInRequestDTO(..)
  , TicketDTO(..)
  , TicketOrderDTO(..)
  , PromoCodeDTO(..)
  , TicketPurchaseWithPromoDTO(..)
  , RefundRequestDTO(..)
  , RefundDTO(..)
  , RejectionReasonDTO(..)
  , TicketTransferCreateDTO(..)
  , TicketTransferDTO(..)
  , WaitlistJoinDTO(..)
  , WaitlistEntryDTO(..)
  , StripePaymentIntentDTO(..)
  , TicketWithQRDTO(..)
  , EventBudgetLineDTO(..)
  , EventFinanceEntryDTO(..)
  , EventFinanceSummaryDTO(..)
  ) where

import           Data.Aeson (FromJSON, ToJSON, Value(..), defaultOptions, genericParseJSON, rejectUnknownFields, withObject, (.:), (.:!), (.:?), (.=), object, toJSON, parseJSON)
import           Data.Aeson.Types (Object, Parser)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import           Data.Int   (Int64)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Data.Time  (UTCTime)
import           GHC.Generics (Generic)
import           Text.Read  (readMaybe)

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

rejectNullObjectFields :: [Text] -> Object -> Parser ()
rejectNullObjectFields fieldNames obj =
  case filter isNullField fieldNames of
    [] -> pure ()
    fieldName : _ ->
      fail (T.unpack fieldName <> " must be omitted instead of null")
  where
    isNullField fieldName =
      AesonKeyMap.lookup (AesonKey.fromText fieldName) obj == Just Null

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
  parseJSON = withObject "ArtistSocialLinksDTO" $ \o -> do
    rejectUnknownObjectFields
      "ArtistSocialLinksDTO"
      artistSocialLinksAllowedKeys
      o
    ArtistSocialLinksDTO
      <$> o .:? "spotify"
      <*> o .:? "instagram"
      <*> o .:? "twitter"
      <*> o .:? "youtube"
      <*> o .:? "soundcloud"

artistSocialLinksAllowedKeys :: [Text]
artistSocialLinksAllowedKeys =
  [ "spotify"
  , "instagram"
  , "twitter"
  , "youtube"
  , "soundcloud"
  ]

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
    rejectUnknownObjectFields
      "ArtistDTO"
      artistAllowedKeys
      o
    rejectNullObjectFields
      [ "artistName"
      , "artistGenres"
      ]
      o
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

artistAllowedKeys :: [Text]
artistAllowedKeys =
  [ "artistId"
  , "artistPartyId"
  , "artistName"
  , "artistGenres"
  , "artistBio"
  , "artistAvatarUrl"
  , "artistSocialLinks"
  , "artistCreatedAt"
  , "artistUpdatedAt"
  ]

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
  parseJSON = withObject "ArtistFollowRequest" $ \o -> do
    rejectUnknownObjectFields
      "ArtistFollowRequest"
      [ "followerPartyId"
      ]
      o
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
instance FromJSON VenueDTO where
  parseJSON = genericParseJSON defaultOptions
    { rejectUnknownFields = True
    }

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
  parseJSON value@(Object o) = do
    rejectUnknownObjectFields "VenueUpdateDTO" venueUpdateAllowedKeys o
    phoneUpdate <- nullableFieldFromParsed <$> (o .:! "venuePhone")
    legacyContactUpdate <- nullableFieldFromParsed <$> (o .:! "venueContact")
    resolvedPhoneUpdate <- resolveVenuePhoneUpdate phoneUpdate legacyContactUpdate
    VenueUpdateDTO
      <$> parseJSON value
      <*> (VenueContactUpdateDTO
            <$> pure resolvedPhoneUpdate
            <*> (nullableFieldFromParsed <$> (o .:! "venueWebsite"))
            <*> (nullableFieldFromParsed <$> (o .:! "venueState"))
            <*> (nullableFieldFromParsed <$> (o .:! "venueZipCode"))
            <*> (nullableFieldFromParsed <$> (o .:! "venueImageUrl")))
  parseJSON _ = fail "VenueUpdateDTO must be an object"

resolveVenuePhoneUpdate
  :: NullableFieldUpdate Text
  -> NullableFieldUpdate Text
  -> Parser (NullableFieldUpdate Text)
resolveVenuePhoneUpdate phoneUpdate legacyContactUpdate = do
  rejectStructuredVenueContact legacyContactUpdate
  case (phoneUpdate, legacyContactUpdate) of
    (FieldMissing, legacyUpdate) -> pure legacyUpdate
    (phoneUpdateValue, FieldMissing) -> pure phoneUpdateValue
    (FieldNull, FieldNull) -> pure FieldNull
    (FieldValue phoneValue, FieldValue contactValue)
      | T.strip phoneValue == T.strip contactValue -> pure (FieldValue phoneValue)
    _ ->
      fail "VenueUpdateDTO contains conflicting venuePhone and venueContact values"

rejectStructuredVenueContact :: NullableFieldUpdate Text -> Parser ()
rejectStructuredVenueContact (FieldValue rawContact)
  | looksStructured (T.strip rawContact) =
      fail "VenueUpdateDTO venueContact updates must use explicit venuePhone fields"
  where
    looksStructured value =
      "{" `T.isPrefixOf` value || "[" `T.isPrefixOf` value
rejectStructuredVenueContact _ = pure ()

venueUpdateAllowedKeys :: [Text]
venueUpdateAllowedKeys =
  [ "venueId"
  , "venueName"
  , "venueAddress"
  , "venueCity"
  , "venueCountry"
  , "venueLat"
  , "venueLng"
  , "venueCapacity"
  , "venueContact"
  , "venuePhone"
  , "venueWebsite"
  , "venueState"
  , "venueZipCode"
  , "venueImageUrl"
  , "venueCreatedAt"
  , "venueUpdatedAt"
  ]

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
instance FromJSON EventDTO where
  parseJSON = genericParseJSON defaultOptions
    { rejectUnknownFields = True
    }

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

data RsvpCreateDTO = RsvpCreateDTO
  { rsvpPartyId   :: Text
  , rsvpStatus    :: Text  -- "Accepted", "Declined", "Maybe"
  } deriving (Show, Eq, Generic)
instance FromJSON RsvpCreateDTO where
  parseJSON = withObject "RsvpCreateDTO" $ \o -> do
    rejectUnknownObjectFields
      "RsvpCreateDTO"
      [ "rsvpPartyId"
      , "rsvpStatus"
      ]
      o
    partyId <- o .: "rsvpPartyId" >>= normalizePositiveIdText "rsvpPartyId"
    status <- o .: "rsvpStatus" >>= normalizeRsvpStatusText
    pure (RsvpCreateDTO partyId status)

instance FromJSON RsvpDTO

normalizeRsvpStatusText :: Text -> Parser Text
normalizeRsvpStatusText rawStatus =
  case T.toLower (T.strip rawStatus) of
    "accepted" -> pure "accepted"
    "declined" -> pure "declined"
    "maybe" -> pure "maybe"
    _ -> fail "rsvpStatus must be one of: accepted, declined, maybe"

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
instance FromJSON InvitationDTO where
  parseJSON = genericParseJSON defaultOptions
    { rejectUnknownFields = True
    }

data InvitationUpdateDTO = InvitationUpdateDTO
  { iudInvitation    :: InvitationDTO
  , iudMessageUpdate :: NullableFieldUpdate Text
  } deriving (Show, Eq, Generic)

instance FromJSON InvitationUpdateDTO where
  parseJSON value@(Object o) = do
    rejectUnknownObjectFields "InvitationUpdateDTO" invitationUpdateAllowedKeys o
    InvitationUpdateDTO
      <$> parseJSON value
      <*> (nullableFieldFromParsed <$> (o .:! "invitationMessage"))
  parseJSON _ = fail "InvitationUpdateDTO must be an object"

invitationUpdateAllowedKeys :: [Text]
invitationUpdateAllowedKeys =
  [ "invitationId"
  , "invitationEventId"
  , "invitationFromPartyId"
  , "invitationToPartyId"
  , "invitationStatus"
  , "invitationMessage"
  , "invitationCreatedAt"
  , "invitationUpdatedAt"
  ]

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
instance FromJSON EventMomentCreateDTO where
  parseJSON = withObject "EventMomentCreateDTO" $ \o -> do
    rejectUnknownObjectFields
      "EventMomentCreateDTO"
      [ "emCreateAuthorName"
      , "emCreateCaption"
      , "emCreateMediaUrl"
      , "emCreateMediaType"
      , "emCreateMediaWidth"
      , "emCreateMediaHeight"
      , "emCreateMediaDurationMs"
      ]
      o
    rejectNullObjectFields
      [ "emCreateAuthorName"
      , "emCreateCaption"
      , "emCreateMediaWidth"
      , "emCreateMediaHeight"
      , "emCreateMediaDurationMs"
      ]
      o
    mediaUrl <-
      o .: "emCreateMediaUrl"
        >>= normalizeRequiredText "emCreateMediaUrl"
    mediaType <-
      o .: "emCreateMediaType"
        >>= normalizeMomentCreateMediaType
    mediaWidth <-
      o .:? "emCreateMediaWidth"
        >>= validateOptionalPositiveInt "emCreateMediaWidth"
    mediaHeight <-
      o .:? "emCreateMediaHeight"
        >>= validateOptionalPositiveInt "emCreateMediaHeight"
    mediaDurationMs <-
      o .:? "emCreateMediaDurationMs"
        >>= validateOptionalNonNegativeInt "emCreateMediaDurationMs"
    EventMomentCreateDTO
      <$> o .:? "emCreateAuthorName"
      <*> o .:? "emCreateCaption"
      <*> pure mediaUrl
      <*> pure mediaType
      <*> pure mediaWidth
      <*> pure mediaHeight
      <*> pure mediaDurationMs

normalizeRequiredText :: String -> Text -> Parser Text
normalizeRequiredText fieldName rawValue =
  let trimmed = T.strip rawValue
  in if T.null trimmed
       then fail (fieldName <> " must not be blank")
       else pure trimmed

normalizeMomentCreateMediaType :: Text -> Parser Text
normalizeMomentCreateMediaType rawValue =
  case T.toLower (T.strip rawValue) of
    "image" -> pure "image"
    "photo" -> pure "image"
    "picture" -> pure "image"
    "video" -> pure "video"
    "clip" -> pure "video"
    _ -> fail "emCreateMediaType must be one of: image, video"

validateOptionalPositiveInt :: String -> Maybe Int -> Parser (Maybe Int)
validateOptionalPositiveInt _ Nothing = pure Nothing
validateOptionalPositiveInt fieldName (Just value)
  | value > 0 = pure (Just value)
  | otherwise = fail (fieldName <> " must be greater than 0")

validateOptionalNonNegativeInt :: String -> Maybe Int -> Parser (Maybe Int)
validateOptionalNonNegativeInt _ Nothing = pure Nothing
validateOptionalNonNegativeInt fieldName (Just value)
  | value >= 0 = pure (Just value)
  | otherwise = fail (fieldName <> " must be greater than or equal to 0")

data EventMomentReactionRequestDTO = EventMomentReactionRequestDTO
  { emrrReaction :: Text
  } deriving (Show, Eq, Generic)
instance ToJSON EventMomentReactionRequestDTO
instance FromJSON EventMomentReactionRequestDTO where
  parseJSON = withObject "EventMomentReactionRequestDTO" $ \o -> do
    rejectUnknownObjectFields
      "EventMomentReactionRequestDTO"
      [ "emrrReaction"
      ]
      o
    EventMomentReactionRequestDTO
      <$> o .: "emrrReaction"

data EventMomentCommentCreateDTO = EventMomentCommentCreateDTO
  { emccAuthorName :: Maybe Text
  , emccBody       :: Text
  } deriving (Show, Eq, Generic)
instance ToJSON EventMomentCommentCreateDTO
instance FromJSON EventMomentCommentCreateDTO where
  parseJSON = withObject "EventMomentCommentCreateDTO" $ \o -> do
    rejectUnknownObjectFields
      "EventMomentCommentCreateDTO"
      [ "emccAuthorName"
      , "emccBody"
      ]
      o
    EventMomentCommentCreateDTO
      <$> o .:? "emccAuthorName"
      <*> o .: "emccBody"

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
instance FromJSON TicketTierDTO where
  parseJSON = genericParseJSON defaultOptions
    { rejectUnknownFields = True
    }

data TicketPurchaseRequestDTO = TicketPurchaseRequestDTO
  { ticketPurchaseTierId    :: Text
  , ticketPurchaseQuantity  :: Int
  , ticketPurchaseBuyerPartyId :: Maybe Text
  , ticketPurchaseBuyerName :: Maybe Text
  , ticketPurchaseBuyerEmail :: Maybe Text
  } deriving (Show, Eq, Generic)

maxTicketPurchaseQuantity :: Int
maxTicketPurchaseQuantity = 100

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
    rejectNullObjectFields
      [ "ticketPurchaseBuyerPartyId"
      , "ticketPurchaseBuyerName"
      , "ticketPurchaseBuyerEmail"
      ]
      o
    tierId <-
      o .: "ticketPurchaseTierId"
        >>= normalizePositiveIdText "ticketPurchaseTierId"
    quantity <- o .: "ticketPurchaseQuantity"
    if quantity <= (0 :: Int)
      then fail "ticketPurchaseQuantity must be greater than 0"
      else
        if quantity > maxTicketPurchaseQuantity
          then fail "ticketPurchaseQuantity exceeds the per-order limit"
          else
            TicketPurchaseRequestDTO tierId quantity
              <$> o .:? "ticketPurchaseBuyerPartyId"
              <*> o .:? "ticketPurchaseBuyerName"
              <*> o .:? "ticketPurchaseBuyerEmail"

normalizePositiveIdText :: String -> Text -> Parser Text
normalizePositiveIdText fieldName rawValue =
  let trimmed = T.strip rawValue
  in if T.null trimmed || not (T.all isAsciiDigitText trimmed)
       then fail (fieldName <> " must be a positive integer")
       else
         case readMaybe (T.unpack trimmed) :: Maybe Int64 of
           Just value | value > 0 -> pure (T.pack (show value))
           _ -> fail (fieldName <> " must be a positive integer")

isAsciiDigitText :: Char -> Bool
isAsciiDigitText ch =
  ch >= '0' && ch <= '9'

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
    rawStatus <- o .: "ticketOrderStatus"
    normalizedStatus <- normalizeTicketOrderStatusUpdate rawStatus
    pure (TicketOrderStatusUpdateDTO normalizedStatus)

normalizeTicketOrderStatusUpdate :: Text -> Parser Text
normalizeTicketOrderStatusUpdate rawStatus =
  case T.toLower (T.strip rawStatus) of
    "paid" -> pure "paid"
    "cancelled" -> pure "cancelled"
    "canceled" -> pure "cancelled"
    "refunded" -> pure "refunded"
    _ -> fail "ticketOrderStatus must be one of: paid, cancelled, refunded"

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
    rejectNullObjectFields
      [ "ticketCheckInTicketId"
      , "ticketCheckInTicketCode"
      ]
      o
    ticketId <-
      o .:? "ticketCheckInTicketId"
        >>= traverse (normalizeTicketCheckInLookupField "ticketCheckInTicketId")
    ticketCode <-
      o .:? "ticketCheckInTicketCode"
        >>= traverse (normalizeTicketCheckInLookupField "ticketCheckInTicketCode")
    case (ticketId, ticketCode) of
      (Just _, Just _) ->
        fail "TicketCheckInRequestDTO requires exactly one ticket lookup field"
      (Nothing, Nothing) ->
        fail "TicketCheckInRequestDTO requires ticketCheckInTicketId or ticketCheckInTicketCode"
      _ ->
        pure (TicketCheckInRequestDTO ticketId ticketCode)

normalizeTicketCheckInLookupField :: String -> Text -> Parser Text
normalizeTicketCheckInLookupField fieldName rawValue =
  let trimmed = T.strip rawValue
  in if T.null trimmed
       then fail (fieldName <> " must not be blank")
       else pure trimmed

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

-- =============================================================================
-- PROMO CODES
-- =============================================================================

data PromoCodeDTO = PromoCodeDTO
  { promoCodeId                      :: Maybe Text
  , promoCodeEventId                 :: Maybe Text
  , promoCodeCode                    :: Text
  , promoCodeDescription             :: Maybe Text
  , promoCodeDiscountType            :: Text
  , promoCodeDiscountValue           :: Int
  , promoCodeCurrency                :: Text
  , promoCodeMaxRedemptions          :: Maybe Int
  , promoCodeCurrentRedemptions      :: Int
  , promoCodeValidFrom               :: Maybe UTCTime
  , promoCodeValidUntil              :: Maybe UTCTime
  , promoCodeTierIds                 :: Maybe [Text]
  , promoCodeMinPurchaseAmountCents  :: Maybe Int
  , promoCodeIsActive                :: Bool
  , promoCodeCreatedAt               :: Maybe UTCTime
  , promoCodeUpdatedAt               :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON PromoCodeDTO
instance FromJSON PromoCodeDTO where
  parseJSON = withObject "PromoCodeDTO" $ \o -> do
    rejectUnknownObjectFields "PromoCodeDTO" promoCodeAllowedKeys o
    code <- o .: "promoCodeCode" >>= validatePromoCode
    discountType <- o .: "promoCodeDiscountType" >>= validateDiscountType
    discountValue <- o .: "promoCodeDiscountValue"
    validatedValue <- validateDiscountValue discountType discountValue
    PromoCodeDTO
      <$> o .:? "promoCodeId"
      <*> o .:? "promoCodeEventId"
      <*> pure code
      <*> o .:? "promoCodeDescription"
      <*> pure discountType
      <*> pure validatedValue
      <*> (o .:? "promoCodeCurrency" .!= "USD")
      <*> o .:? "promoCodeMaxRedemptions"
      <*> (o .:? "promoCodeCurrentRedemptions" .!= 0)
      <*> o .:? "promoCodeValidFrom"
      <*> o .:? "promoCodeValidUntil"
      <*> o .:? "promoCodeTierIds"
      <*> o .:? "promoCodeMinPurchaseAmountCents"
      <*> (o .:? "promoCodeIsActive" .!= True)
      <*> o .:? "promoCodeCreatedAt"
      <*> o .:? "promoCodeUpdatedAt"

promoCodeAllowedKeys :: [Text]
promoCodeAllowedKeys =
  [ "promoCodeId"
  , "promoCodeEventId"
  , "promoCodeCode"
  , "promoCodeDescription"
  , "promoCodeDiscountType"
  , "promoCodeDiscountValue"
  , "promoCodeCurrency"
  , "promoCodeMaxRedemptions"
  , "promoCodeCurrentRedemptions"
  , "promoCodeValidFrom"
  , "promoCodeValidUntil"
  , "promoCodeTierIds"
  , "promoCodeMinPurchaseAmountCents"
  , "promoCodeIsActive"
  , "promoCodeCreatedAt"
  , "promoCodeUpdatedAt"
  ]

validatePromoCode :: Text -> Parser Text
validatePromoCode rawCode =
  let code = T.toUpper (T.strip rawCode)
  in if T.null code
       then fail "Promo code must not be empty"
       else if T.length code > 50
         then fail "Promo code must be 50 characters or fewer"
         else if not (T.all isValidPromoChar code)
           then fail "Promo code must contain only letters, numbers, and hyphens"
           else pure code
  where
    isValidPromoChar c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '-'

validateDiscountType :: Text -> Parser Text
validateDiscountType rawType =
  case T.toLower (T.strip rawType) of
    "percentage" -> pure "percentage"
    "fixed_amount" -> pure "fixed_amount"
    "fixed" -> pure "fixed_amount"
    _ -> fail "Discount type must be 'percentage' or 'fixed_amount'"

validateDiscountValue :: Text -> Int -> Parser Int
validateDiscountValue discountType value
  | value < 0 = fail "Discount value must be non-negative"
  | discountType == "percentage" && value > 10000 =
      fail "Percentage discount cannot exceed 100% (10000 basis points)"
  | otherwise = pure value

data TicketPurchaseWithPromoDTO = TicketPurchaseWithPromoDTO
  { tpwpPurchase   :: TicketPurchaseRequestDTO
  , tpwpPromoCode  :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON TicketPurchaseWithPromoDTO where
  toJSON TicketPurchaseWithPromoDTO{..} = object
    [ "ticketPurchaseTierId"       .= ticketPurchaseTierId tpwpPurchase
    , "ticketPurchaseQuantity"     .= ticketPurchaseQuantity tpwpPurchase
    , "ticketPurchaseBuyerPartyId" .= ticketPurchaseBuyerPartyId tpwpPurchase
    , "ticketPurchaseBuyerName"    .= ticketPurchaseBuyerName tpwpPurchase
    , "ticketPurchaseBuyerEmail"   .= ticketPurchaseBuyerEmail tpwpPurchase
    , "ticketPurchasePromoCode"    .= tpwpPromoCode
    ]

instance FromJSON TicketPurchaseWithPromoDTO where
  parseJSON value@(Object o) = do
    rejectUnknownObjectFields "TicketPurchaseWithPromoDTO" ticketPurchaseWithPromoKeys o
    purchase <- parseJSON value
    promoCode <- o .:? "ticketPurchasePromoCode" >>= traverse validateOptionalPromoCode
    pure $ TicketPurchaseWithPromoDTO purchase promoCode
  parseJSON _ = fail "TicketPurchaseWithPromoDTO must be an object"

ticketPurchaseWithPromoKeys :: [Text]
ticketPurchaseWithPromoKeys =
  [ "ticketPurchaseTierId"
  , "ticketPurchaseQuantity"
  , "ticketPurchaseBuyerPartyId"
  , "ticketPurchaseBuyerName"
  , "ticketPurchaseBuyerEmail"
  , "ticketPurchasePromoCode"
  ]

validateOptionalPromoCode :: Text -> Parser Text
validateOptionalPromoCode code =
  if T.null (T.strip code)
    then fail "Promo code must not be blank if provided"
    else validatePromoCode code

-- =============================================================================
-- REFUNDS
-- =============================================================================

data RefundRequestDTO = RefundRequestDTO
  { refundRequestReason      :: Maybe Text
  , refundRequestAmountCents :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON RefundRequestDTO
instance FromJSON RefundRequestDTO where
  parseJSON = withObject "RefundRequestDTO" $ \o -> do
    rejectUnknownObjectFields "RefundRequestDTO" refundRequestAllowedKeys o
    RefundRequestDTO
      <$> o .:? "refundRequestReason"
      <*> o .:? "refundRequestAmountCents"

refundRequestAllowedKeys :: [Text]
refundRequestAllowedKeys =
  [ "refundRequestReason"
  , "refundRequestAmountCents"
  ]

data RefundDTO = RefundDTO
  { refundId                 :: Maybe Text
  , refundOrderId            :: Text
  , refundRequestedByPartyId :: Maybe Text
  , refundReason             :: Maybe Text
  , refundAmountCents        :: Int
  , refundStatus             :: Text
  , refundApprovedByPartyId  :: Maybe Text
  , refundApprovedAt         :: Maybe UTCTime
  , refundRejectionReason    :: Maybe Text
  , refundStripeRefundId     :: Maybe Text
  , refundProcessedAt        :: Maybe UTCTime
  , refundCreatedAt          :: Maybe UTCTime
  , refundUpdatedAt          :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON RefundDTO
instance FromJSON RefundDTO

data RejectionReasonDTO = RejectionReasonDTO
  { rrReason :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON RejectionReasonDTO
instance FromJSON RejectionReasonDTO where
  parseJSON = withObject "RejectionReasonDTO" $ \o -> do
    rejectUnknownObjectFields "RejectionReasonDTO" ["rrReason"] o
    RejectionReasonDTO <$> o .: "rrReason"

-- =============================================================================
-- TICKET TRANSFERS
-- =============================================================================

data TicketTransferCreateDTO = TicketTransferCreateDTO
  { ttcToEmail  :: Text
  , ttcToName   :: Maybe Text
  , ttcMessage  :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON TicketTransferCreateDTO
instance FromJSON TicketTransferCreateDTO where
  parseJSON = withObject "TicketTransferCreateDTO" $ \o -> do
    rejectUnknownObjectFields "TicketTransferCreateDTO" transferCreateAllowedKeys o
    email <- o .: "ttcToEmail" >>= validateTransferEmail
    TicketTransferCreateDTO email
      <$> o .:? "ttcToName"
      <*> o .:? "ttcMessage"

transferCreateAllowedKeys :: [Text]
transferCreateAllowedKeys =
  [ "ttcToEmail"
  , "ttcToName"
  , "ttcMessage"
  ]

validateTransferEmail :: Text -> Parser Text
validateTransferEmail raw =
  let trimmed = T.strip raw
  in if T.null trimmed
       then fail "Transfer recipient email is required"
       else if not ("@" `T.isInfixOf` trimmed)
         then fail "Invalid email format"
         else pure trimmed

data TicketTransferDTO = TicketTransferDTO
  { ttId           :: Maybe Text
  , ttTicketId     :: Text
  , ttFromPartyId  :: Maybe Text
  , ttToEmail      :: Text
  , ttToName       :: Maybe Text
  , ttStatus       :: Text
  , ttTransferCode :: Text
  , ttMessage      :: Maybe Text
  , ttExpiresAt    :: Maybe UTCTime
  , ttAcceptedAt   :: Maybe UTCTime
  , ttCreatedAt    :: Maybe UTCTime
  , ttUpdatedAt    :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON TicketTransferDTO
instance FromJSON TicketTransferDTO

-- =============================================================================
-- WAITLIST
-- =============================================================================

data WaitlistJoinDTO = WaitlistJoinDTO
  { wjEmail    :: Text
  , wjName     :: Maybe Text
  , wjTierId   :: Maybe Text
  , wjQuantity :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON WaitlistJoinDTO
instance FromJSON WaitlistJoinDTO where
  parseJSON = withObject "WaitlistJoinDTO" $ \o -> do
    rejectUnknownObjectFields "WaitlistJoinDTO" waitlistJoinAllowedKeys o
    email <- o .: "wjEmail" >>= validateWaitlistEmail
    quantity <- o .:? "wjQuantity" .!= 1
    if quantity < 1 || quantity > 10
      then fail "Waitlist quantity must be between 1 and 10"
      else WaitlistJoinDTO email
        <$> o .:? "wjName"
        <*> o .:? "wjTierId"
        <*> pure quantity

waitlistJoinAllowedKeys :: [Text]
waitlistJoinAllowedKeys =
  [ "wjEmail"
  , "wjName"
  , "wjTierId"
  , "wjQuantity"
  ]

validateWaitlistEmail :: Text -> Parser Text
validateWaitlistEmail raw =
  let trimmed = T.strip raw
  in if T.null trimmed
       then fail "Email is required"
       else if not ("@" `T.isInfixOf` trimmed)
         then fail "Invalid email format"
         else pure trimmed

data WaitlistEntryDTO = WaitlistEntryDTO
  { weId                 :: Maybe Text
  , weEventId            :: Text
  , weTierId             :: Maybe Text
  , weEmail              :: Text
  , weName               :: Maybe Text
  , weQuantity           :: Int
  , weStatus             :: Text
  , wePriority           :: Int
  , weNotifiedAt         :: Maybe UTCTime
  , weExpiresAt          :: Maybe UTCTime
  , weConvertedOrderId   :: Maybe Text
  , weCreatedAt          :: Maybe UTCTime
  , weUpdatedAt          :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON WaitlistEntryDTO
instance FromJSON WaitlistEntryDTO

-- =============================================================================
-- STRIPE
-- =============================================================================

data StripePaymentIntentDTO = StripePaymentIntentDTO
  { spiClientSecret :: Text
  , spiOrderId      :: Text
  , spiAmountCents  :: Int
  , spiCurrency     :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON StripePaymentIntentDTO
instance FromJSON StripePaymentIntentDTO

-- =============================================================================
-- QR CODES
-- =============================================================================

data TicketWithQRDTO = TicketWithQRDTO
  { twqTicket      :: TicketDTO
  , twqQRData      :: Text
  , twqQRImageUrl  :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON TicketWithQRDTO
instance FromJSON TicketWithQRDTO

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
instance FromJSON EventBudgetLineDTO where
  parseJSON = genericParseJSON defaultOptions
    { rejectUnknownFields = True
    }

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
instance FromJSON EventFinanceEntryDTO where
  parseJSON = genericParseJSON defaultOptions
    { rejectUnknownFields = True
    }

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
