{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Server.SocialEventsHandlers
  ( socialEventsServer
  , validateRsvpStatus
  , validateInvitationToPartyId
  , validateInvitationStatusInput
  , validateInvitationStatusUpdateInput
  , normalizeInvitationStatus
  , normalizeArtistGenres
  , parseInvitationIdsEither
  , parseVenueIdEither
  , parseFollowerQueryParamEither
  , parseNearQueryEither
  , followArtistDb
  , normalizeTicketOrderStatus
  , normalizeTicketStatus
  , normalizeEventType
  , normalizeEventStatus
  , parseEventTypeQueryParamEither
  , parseEventStatusQueryParamEither
  , validateEventCreateTypeStatus
  , validateEventMetadataUpdate
  , validateBudgetLineTypeInput
  , normalizeBudgetLineType
  , normalizeFinanceDirection
  , normalizeFinanceSource
  , normalizeFinanceEntryStatus
  , validateFinanceEntryCurrencyInput
  , validateStoredFinanceEntryDimensions
  , normalizePositivePartyIdText
  , resolveExistingPartyIdText
  , validateEventArtistIds
  , normalizeMomentMediaType
  , normalizeMomentReaction
  , normalizeMomentCaption
  , normalizeMomentCommentBody
  , validateEventCreateUpdateDimensions
  , validateVenueCreateUpdateFields
  , validateEventCurrencyInput
  , TicketCheckInLookup(..)
  , validateTicketCheckInLookup
  , validateTicketCheckInOrderStatus
  , validateTicketCheckInTicketStatus
  , findTicketForCheckIn
  , validateOptionalTicketBuyerPartyId
  , validateTicketPurchaseBuyerEmail
  , validateTicketTierCurrencyInput
  , isImageUpload
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (forM, forM_, replicateM, unless, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAlphaNum, isAscii, isAsciiUpper, isControl, isHexDigit)
import           Data.Int (Int64)
import           Data.List (foldl', sortOn)
import           Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import           Data.Ord (Down(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import           System.Directory (copyFile, createDirectoryIfMissing)
import           System.FilePath ((</>), takeExtension, takeFileName)
import           Text.Read (readMaybe)

import           Servant
import           Servant.Multipart (FileData(..))

-- Pull in full Persistent surface so TH-generated field constructors
-- (EventRsvpEventId, SocialEventStartTime, etc.) are available.
import           Database.Persist
import           Database.Persist.Sql (ConnectionPool, SqlBackend, SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)

import           TDF.API.SocialEventsAPI
import           TDF.Auth (AuthedUser(..))
import           TDF.Config (assetsRootDir, resolveConfiguredAssetsBase)
import           TDF.DTO.SocialEventsDTO
  ( EventDTO(..)
  , EventUpdateDTO(..)
  , EventMetadataUpdateDTO(..)
  , VenueDTO(..)
  , VenueUpdateDTO(..)
  , VenueContactUpdateDTO(..)
  , ArtistDTO(..)
  , ArtistSocialLinksDTO(..)
  , ArtistFollowerDTO(..)
  , ArtistFollowRequest(..)
  , NullableFieldUpdate(..)
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
  , TicketOrderStatusUpdateDTO(..)
  , TicketCheckInRequestDTO(..)
  , TicketDTO(..)
  , TicketOrderDTO(..)
  , EventBudgetLineDTO(..)
  , EventFinanceEntryDTO(..)
  , EventFinanceSummaryDTO(..)
  )
import           TDF.DB (Env(..))
import           TDF.Models (Party)
import           TDF.Models.SocialEventsModels hiding (venueAddress, venueCapacity, venueCity, venueContact, venueCountry, venueCreatedAt, venueName, venueUpdatedAt)
import qualified TDF.Models.SocialEventsModels as SM

type AppM = ReaderT Env Handler

data TicketCheckInLookup
  = TicketCheckInLookupById T.Text
  | TicketCheckInLookupByCode T.Text
  deriving (Show, Eq)

decodeSocialLinks :: Maybe T.Text -> Maybe ArtistSocialLinksDTO
decodeSocialLinks mTxt = do
  txt <- mTxt
  Aeson.decodeStrict (TE.encodeUtf8 txt)

encodeSocialLinks :: Maybe ArtistSocialLinksDTO -> Maybe T.Text
encodeSocialLinks mLinks =
  fmap (TE.decodeUtf8 . BL.toStrict . Aeson.encode) mLinks

data EventMetadataDTO = EventMetadataDTO
  { emTicketUrl :: Maybe T.Text
  , emImageUrl :: Maybe T.Text
  , emIsPublic :: Maybe Bool
  , emType :: Maybe T.Text
  , emStatus :: Maybe T.Text
  , emCurrency :: Maybe T.Text
  , emBudgetCents :: Maybe Int
  }

emptyEventMetadata :: EventMetadataDTO
emptyEventMetadata = EventMetadataDTO
  { emTicketUrl = Nothing
  , emImageUrl = Nothing
  , emIsPublic = Nothing
  , emType = Nothing
  , emStatus = Nothing
  , emCurrency = Nothing
  , emBudgetCents = Nothing
  }

instance Aeson.ToJSON EventMetadataDTO where
  toJSON EventMetadataDTO{..} = Aeson.object
    [ "ticketUrl" Aeson..= emTicketUrl
    , "imageUrl" Aeson..= emImageUrl
    , "isPublic" Aeson..= emIsPublic
    , "eventType" Aeson..= emType
    , "eventStatus" Aeson..= emStatus
    , "currency" Aeson..= emCurrency
    , "budgetCents" Aeson..= emBudgetCents
    ]

instance Aeson.FromJSON EventMetadataDTO where
  parseJSON = Aeson.withObject "EventMetadataDTO" $ \o ->
    EventMetadataDTO
      <$> o Aeson..:? "ticketUrl"
      <*> o Aeson..:? "imageUrl"
      <*> o Aeson..:? "isPublic"
      <*> o Aeson..:? "eventType"
      <*> o Aeson..:? "eventStatus"
      <*> o Aeson..:? "currency"
      <*> o Aeson..:? "budgetCents"

decodeEventMetadata :: Maybe T.Text -> EventMetadataDTO
decodeEventMetadata mTxt = fromMaybe emptyEventMetadata $ do
  txt <- mTxt
  Aeson.decodeStrict (TE.encodeUtf8 txt)

encodeEventMetadata :: EventMetadataDTO -> Maybe T.Text
encodeEventMetadata EventMetadataDTO{..}
  | isNothing emTicketUrl
    && isNothing emImageUrl
    && isNothing emIsPublic
    && isNothing emType
    && isNothing emStatus
    && isNothing emCurrency
    && isNothing emBudgetCents = Nothing
  | otherwise =
      Just
        (TE.decodeUtf8 . BL.toStrict . Aeson.encode $
          EventMetadataDTO
            { emTicketUrl = emTicketUrl
            , emImageUrl = emImageUrl
            , emIsPublic = emIsPublic
            , emType = emType
            , emStatus = emStatus
            , emCurrency = emCurrency
            , emBudgetCents = emBudgetCents
            })

applyNullableTextUpdate :: NullableFieldUpdate T.Text -> Maybe T.Text -> Maybe T.Text
applyNullableTextUpdate field existing =
  case field of
    FieldMissing -> existing
    FieldNull -> Nothing
    FieldValue value -> cleanMaybeText (Just value)

applyNullableBoolUpdate :: NullableFieldUpdate Bool -> Maybe Bool -> Maybe Bool
applyNullableBoolUpdate field existing =
  case field of
    FieldMissing -> existing
    FieldNull -> Nothing
    FieldValue value -> Just value

applyNullableIntUpdate :: (a -> Maybe b) -> NullableFieldUpdate a -> Maybe b -> Maybe b
applyNullableIntUpdate normalizeValue field existing =
  case field of
    FieldMissing -> existing
    FieldNull -> Nothing
    FieldValue value ->
      case normalizeValue value of
        Just normalized -> Just normalized
        Nothing -> existing

applyNullableNormalizedTextUpdate :: (Maybe T.Text -> Maybe T.Text) -> NullableFieldUpdate T.Text -> Maybe T.Text -> Maybe T.Text
applyNullableNormalizedTextUpdate normalizeValue field existing =
  case field of
    FieldMissing -> existing
    FieldNull -> Nothing
    FieldValue value ->
      case normalizeValue (Just value) of
        Just normalized -> Just normalized
        Nothing
          | T.null (T.strip value) -> Nothing
          | otherwise -> existing

normalizeNullableTextUpdate
  :: BL.ByteString
  -> (Maybe T.Text -> Maybe T.Text)
  -> NullableFieldUpdate T.Text
  -> Either ServerError (NullableFieldUpdate T.Text)
normalizeNullableTextUpdate _ _ FieldMissing = Right FieldMissing
normalizeNullableTextUpdate _ _ FieldNull = Right FieldNull
normalizeNullableTextUpdate invalidMessage normalizeValue (FieldValue value) =
  case cleanMaybeText (Just value) of
    Nothing -> Right FieldNull
    Just cleaned ->
      case normalizeValue (Just cleaned) of
        Just normalized -> Right (FieldValue normalized)
        Nothing -> Left err400 { errBody = invalidMessage }

validateEventMetadataUpdate :: EventMetadataUpdateDTO -> Either ServerError EventMetadataUpdateDTO
validateEventMetadataUpdate EventMetadataUpdateDTO{..} = do
  normalizedType <- normalizeNullableTextUpdate
    "eventType must be one of: party, concert, festival, conference, showcase, other"
    normalizeEventType
    emuType
  normalizedStatus <- normalizeNullableTextUpdate
    "eventStatus must be one of: planning, announced, on_sale, live, completed, cancelled"
    normalizeEventStatus
    emuStatus
  normalizedCurrency <- normalizeNullableTextUpdate
    "eventCurrency must be a 3-letter ISO code"
    normalizeEventCurrencyMaybe
    emuCurrency
  pure EventMetadataUpdateDTO
    { emuTicketUrl = emuTicketUrl
    , emuImageUrl = emuImageUrl
    , emuIsPublic = emuIsPublic
    , emuType = normalizedType
    , emuStatus = normalizedStatus
    , emuCurrency = normalizedCurrency
    , emuBudgetCents = emuBudgetCents
    }

validateCreateNormalizedTextDefault
  :: BL.ByteString
  -> (Maybe T.Text -> Maybe T.Text)
  -> T.Text
  -> Maybe T.Text
  -> Either ServerError T.Text
validateCreateNormalizedTextDefault invalidMessage normalizeValue fallbackValue rawValue =
  case cleanMaybeText rawValue of
    Nothing -> Right fallbackValue
    Just cleaned ->
      case normalizeValue (Just cleaned) of
        Just normalized -> Right normalized
        Nothing -> Left err400 { errBody = invalidMessage }

validateEventCreateTypeStatus :: Maybe T.Text -> Maybe T.Text -> Either ServerError (T.Text, T.Text)
validateEventCreateTypeStatus mType mStatus = do
  normalizedType <- validateCreateNormalizedTextDefault
    "eventType must be one of: party, concert, festival, conference, showcase, other"
    normalizeEventType
    "party"
    mType
  normalizedStatus <- validateCreateNormalizedTextDefault
    "eventStatus must be one of: planning, announced, on_sale, live, completed, cancelled"
    normalizeEventStatus
    "planning"
    mStatus
  pure (normalizedType, normalizedStatus)

validateNormalizedQueryParamEither
  :: BL.ByteString
  -> (Maybe T.Text -> Maybe T.Text)
  -> Maybe T.Text
  -> Either ServerError (Maybe T.Text)
validateNormalizedQueryParamEither invalidMessage normalizeValue rawValue =
  case cleanMaybeText rawValue of
    Nothing -> Right Nothing
    Just cleaned ->
      case normalizeValue (Just cleaned) of
        Just normalized -> Right (Just normalized)
        Nothing -> Left err400 { errBody = invalidMessage }

parseEventTypeQueryParamEither :: Maybe T.Text -> Either ServerError (Maybe T.Text)
parseEventTypeQueryParamEither =
  validateNormalizedQueryParamEither
    "eventType must be one of: party, concert, festival, conference, showcase, other"
    normalizeEventType

parseEventStatusQueryParamEither :: Maybe T.Text -> Either ServerError (Maybe T.Text)
parseEventStatusQueryParamEither =
  validateNormalizedQueryParamEither
    "eventStatus must be one of: planning, announced, on_sale, live, completed, cancelled"
    normalizeEventStatus

applyEventMetadataUpdate :: EventMetadataUpdateDTO -> EventMetadataDTO -> EventMetadataDTO
applyEventMetadataUpdate EventMetadataUpdateDTO{..} existing = EventMetadataDTO
  { emTicketUrl = applyNullableTextUpdate emuTicketUrl (emTicketUrl existing)
  , emImageUrl = applyNullableTextUpdate emuImageUrl (emImageUrl existing)
  , emIsPublic = applyNullableBoolUpdate emuIsPublic (emIsPublic existing)
  , emType = applyNullableNormalizedTextUpdate normalizeEventType emuType (emType existing)
  , emStatus = applyNullableNormalizedTextUpdate normalizeEventStatus emuStatus (emStatus existing)
  , emCurrency = applyNullableNormalizedTextUpdate normalizeCurrencyMaybe emuCurrency (emCurrency existing)
  , emBudgetCents = applyNullableIntUpdate (\value -> normalizeBudgetCentsMaybe (Just value)) emuBudgetCents (emBudgetCents existing)
  }

data VenueContactMetadata = VenueContactMetadata
  { vcmPhone :: Maybe T.Text
  , vcmWebsite :: Maybe T.Text
  , vcmState :: Maybe T.Text
  , vcmZipCode :: Maybe T.Text
  , vcmImageUrl :: Maybe T.Text
  }

emptyVenueContactMetadata :: VenueContactMetadata
emptyVenueContactMetadata = VenueContactMetadata
  { vcmPhone = Nothing
  , vcmWebsite = Nothing
  , vcmState = Nothing
  , vcmZipCode = Nothing
  , vcmImageUrl = Nothing
  }

instance Aeson.ToJSON VenueContactMetadata where
  toJSON VenueContactMetadata{..} = Aeson.object
    [ "phone" Aeson..= vcmPhone
    , "website" Aeson..= vcmWebsite
    , "state" Aeson..= vcmState
    , "zipCode" Aeson..= vcmZipCode
    , "imageUrl" Aeson..= vcmImageUrl
    ]

instance Aeson.FromJSON VenueContactMetadata where
  parseJSON = Aeson.withObject "VenueContactMetadata" $ \o ->
    VenueContactMetadata
      <$> o Aeson..:? "phone"
      <*> o Aeson..:? "website"
      <*> o Aeson..:? "state"
      <*> o Aeson..:? "zipCode"
      <*> o Aeson..:? "imageUrl"

decodeVenueContactMetadata :: Maybe T.Text -> VenueContactMetadata
decodeVenueContactMetadata mTxt =
  case cleanMaybeText mTxt of
    Nothing -> emptyVenueContactMetadata
    Just raw ->
      case Aeson.decodeStrict (TE.encodeUtf8 raw) of
        Just meta -> meta
        Nothing -> emptyVenueContactMetadata { vcmPhone = Just raw }

encodeVenueContactMetadata :: VenueContactMetadata -> Maybe T.Text
encodeVenueContactMetadata VenueContactMetadata{..}
  | isNothing vcmPhone && isNothing vcmWebsite && isNothing vcmState && isNothing vcmZipCode && isNothing vcmImageUrl = Nothing
  | isJust vcmPhone && isNothing vcmWebsite && isNothing vcmState && isNothing vcmZipCode && isNothing vcmImageUrl = vcmPhone
  | otherwise =
      Just
        (TE.decodeUtf8 . BL.toStrict . Aeson.encode $
          VenueContactMetadata
            { vcmPhone = vcmPhone
            , vcmWebsite = vcmWebsite
            , vcmState = vcmState
            , vcmZipCode = vcmZipCode
            , vcmImageUrl = vcmImageUrl
            })

venueContactMetadataFromDTO :: VenueDTO -> VenueContactMetadata
venueContactMetadataFromDTO dto =
  let parsedContact = decodeVenueContactMetadata (venueContact dto)
  in VenueContactMetadata
      { vcmPhone = cleanMaybeText (venuePhone dto) <|> vcmPhone parsedContact
      , vcmWebsite = cleanMaybeText (venueWebsite dto) <|> vcmWebsite parsedContact
      , vcmState = cleanMaybeText (venueState dto) <|> vcmState parsedContact
      , vcmZipCode = cleanMaybeText (venueZipCode dto) <|> vcmZipCode parsedContact
      , vcmImageUrl = cleanMaybeText (venueImageUrl dto) <|> vcmImageUrl parsedContact
      }

applyVenueContactUpdate :: VenueContactUpdateDTO -> VenueContactMetadata -> VenueContactMetadata
applyVenueContactUpdate VenueContactUpdateDTO{..} existing = VenueContactMetadata
  { vcmPhone = applyNullableTextUpdate vcuPhone (vcmPhone existing)
  , vcmWebsite = applyNullableTextUpdate vcuWebsite (vcmWebsite existing)
  , vcmState = applyNullableTextUpdate vcuState (vcmState existing)
  , vcmZipCode = applyNullableTextUpdate vcuZipCode (vcmZipCode existing)
  , vcmImageUrl = applyNullableTextUpdate vcuImageUrl (vcmImageUrl existing)
  }

socialEventsServer :: AuthedUser -> ServerT SocialEventsAPI AppM
socialEventsServer user = eventsServer
               :<|> venuesServer
               :<|> artistsServer
               :<|> rsvpsServer
               :<|> invitationsServer
               :<|> momentsServer
               :<|> ticketsServer
               :<|> budgetServer
               :<|> financeServer
  where
    currentPartyId :: T.Text
    currentPartyId = renderPartyId user

    -- Events
    eventsServer :: ServerT EventsRoutes AppM
    eventsServer = listEvents
               :<|> createEvent
               :<|> getEvent
               :<|> updateEvent
               :<|> uploadEventImage
               :<|> deleteEvent

    listEvents :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> AppM [EventDTO]
    listEvents mCity mStartAfter mType mStatus mArtistId mVenueId mLimit mOffset = do
      Env{..} <- ask
      limit <- resolveLimit 200 500 mLimit
      offset <- resolveOffset mOffset
      typeNeedle <- either throwError pure (parseEventTypeQueryParamEither mType)
      statusNeedle <- either throwError pure (parseEventStatusQueryParamEither mStatus)
      startFilter <- case mStartAfter of
        Nothing -> pure []
        Just raw ->
          case iso8601ParseM (T.unpack raw) of
            Just t  -> pure [SocialEventStartTime >=. t]
            Nothing -> throwError err400 { errBody = "Invalid start_after value (expected ISO-8601 datetime)" }
      cityFilter <- case fmap (T.toCaseFold . T.strip) mCity of
        Nothing -> pure []
        Just "" -> pure []
        Just cityNeedle -> do
          venueRows <- liftIO $ runSqlPool (selectList [] [LimitTo 2000]) envPool
          let ids =
                [ entityKey venueRow
                | venueRow@(Entity _ v) <- venueRows
                , maybe False (\cityVal -> T.isInfixOf cityNeedle (T.toCaseFold cityVal)) (SM.venueCity v)
                ]
          if null ids
            then pure [SocialEventId ==. toSqlKey 0] -- force empty result set
            else pure [SocialEventVenueId <-. map Just ids]
      venueFilter <- case fmap T.strip mVenueId of
        Nothing -> pure []
        Just "" -> pure []
        Just raw -> do
          venueKey <- either throwError pure (parseVenueIdEither raw)
          pure [SocialEventVenueId ==. Just venueKey]
      artistFilter <- case fmap T.strip mArtistId of
        Nothing -> pure []
        Just "" -> pure []
        Just raw -> do
          artistKey <- parseArtistId raw
          artistLinks <- liftIO $ runSqlPool (selectList [EventArtistArtistId ==. artistKey] []) envPool
          let eventIds = map (eventArtistEventId . entityVal) artistLinks
          if null eventIds
            then pure [SocialEventId ==. toSqlKey 0]
            else pure [SocialEventId <-. eventIds]
      let filters = startFilter ++ cityFilter ++ venueFilter ++ artistFilter
      rows <- liftIO $ runSqlPool (selectList filters [Desc SocialEventStartTime, LimitTo limit, OffsetBy offset]) envPool
      let matchesMeta eventRow =
            let meta = decodeEventMetadata (socialEventMetadata eventRow)
                typeOk = maybe True (\t -> emType meta == Just t) typeNeedle
                statusOk = maybe True (\s -> emStatus meta == Just s) statusNeedle
            in typeOk && statusOk
      forM (filter (matchesMeta . entityVal) rows) $ \(Entity eid eventRow) -> do
        artists <- liftIO $ loadEventArtists envPool eid
        pure (eventEntityToDTO eid eventRow artists)

    createEvent :: EventDTO -> AppM EventDTO
    createEvent dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      when (T.null (T.strip (eventTitle dto))) $ throwError err400 { errBody = "title is required" }
      when (eventStart dto >= eventEnd dto) $ throwError err400 { errBody = "start time must be before end time" }
      either throwError pure $
        validateEventCreateUpdateDimensions
          (eventPriceCents dto)
          (eventCapacity dto)
          (eventBudgetCents dto)
      (eventTypeVal, eventStatusVal) <- either throwError pure (validateEventCreateTypeStatus (eventType dto) (eventStatus dto))
      currencyVal <- either throwError pure (validateEventCurrencyInput (eventCurrency dto))
      artistKeys <- either throwError pure (validateEventArtistIds (eventArtists dto))
      let metadataVal =
            encodeEventMetadata
              EventMetadataDTO
                { emTicketUrl = cleanMaybeText (eventTicketUrl dto)
                , emImageUrl = cleanMaybeText (eventImageUrl dto)
                , emIsPublic = eventIsPublic dto <|> Just True
                , emType = Just eventTypeVal
                , emStatus = Just eventStatusVal
                , emCurrency = Just currencyVal
                , emBudgetCents = normalizeBudgetCentsMaybe (eventBudgetCents dto)
                }
          createdMetadata = decodeEventMetadata metadataVal
      mVenueKey <- case eventVenueId dto of
        Nothing -> pure Nothing
        Just txt -> Just <$> either throwError pure (parseVenueIdEither txt)
      key <- liftIO $ runSqlPool (insert SocialEvent
        { socialEventOrganizerPartyId = Just currentPartyId
        , socialEventTitle = eventTitle dto
        , socialEventDescription = eventDescription dto
        , socialEventVenueId = mVenueKey
        , socialEventStartTime = eventStart dto
        , socialEventEndTime = eventEnd dto
        , socialEventPriceCents = eventPriceCents dto
        , socialEventCapacity = eventCapacity dto
        , socialEventMetadata = metadataVal
        , socialEventCreatedAt = now
        , socialEventUpdatedAt = now
        }) envPool
      liftIO $ runSqlPool
        (forM_ artistKeys $ \artistKey ->
           insert_ (EventArtist key artistKey Nothing)
        )
        envPool
      pure dto
        { eventId = Just (renderKeyText key)
        , eventOrganizerPartyId = Just currentPartyId
        , eventTicketUrl = emTicketUrl createdMetadata
        , eventImageUrl = emImageUrl createdMetadata
        , eventIsPublic = emIsPublic createdMetadata
        , eventType = emType createdMetadata
        , eventStatus = emStatus createdMetadata
        , eventCurrency = emCurrency createdMetadata
        , eventBudgetCents = emBudgetCents createdMetadata
        , eventCreatedAt = Just now
        , eventUpdatedAt = Just now
        }

    getEvent :: T.Text -> AppM EventDTO
    getEvent rawId = do
      Env{..} <- ask
      eventKey <- parseKeyOr400 "event" rawId
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      case mEvent of
        Nothing -> throwError err404 { errBody = "Event not found" }
        Just eventRow -> do
          artists <- liftIO $ loadEventArtists envPool eventKey
          pure (eventEntityToDTO eventKey eventRow artists)

    updateEvent :: T.Text -> EventUpdateDTO -> AppM EventDTO
    updateEvent rawId EventUpdateDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" rawId
      mExisting <- liftIO $ runSqlPool (get eventKey) envPool
      existing <- maybe (throwError err404 { errBody = "Event not found" }) pure mExisting
      let dto = eudEvent
      when (T.null (T.strip (eventTitle dto))) $ throwError err400 { errBody = "title is required" }
      when (eventStart dto >= eventEnd dto) $ throwError err400 { errBody = "start time must be before end time" }
      either throwError pure $
        validateEventCreateUpdateDimensions
          (eventPriceCents dto)
          (eventCapacity dto)
          (eventBudgetCents dto)
      validatedMetadataUpdate <- either throwError pure (validateEventMetadataUpdate eudMetadataUpdate)
      artistKeys <- either throwError pure (validateEventArtistIds (eventArtists dto))
      let existingMetadata = decodeEventMetadata (socialEventMetadata existing)
          mergedMetadata = applyEventMetadataUpdate validatedMetadataUpdate existingMetadata
      mVenueKey <- case eventVenueId dto of
        Nothing -> pure Nothing
        Just txt -> Just <$> either throwError pure (parseVenueIdEither txt)
      liftIO $ runSqlPool (update eventKey
        [ SocialEventTitle =. eventTitle dto
        , SocialEventDescription =. eventDescription dto
        , SocialEventVenueId =. mVenueKey
        , SocialEventStartTime =. eventStart dto
        , SocialEventEndTime =. eventEnd dto
        , SocialEventPriceCents =. eventPriceCents dto
        , SocialEventCapacity =. eventCapacity dto
        , SocialEventMetadata =. encodeEventMetadata mergedMetadata
        , SocialEventUpdatedAt =. now
        ]) envPool
      liftIO $ runSqlPool (deleteWhere [EventArtistEventId ==. eventKey]) envPool
      liftIO $ runSqlPool
        (forM_ artistKeys $ \artistKey ->
           insert_ (EventArtist eventKey artistKey Nothing)
        )
        envPool
      pure (dto
        { eventId = Just rawId
        , eventOrganizerPartyId = socialEventOrganizerPartyId existing
        , eventTicketUrl = emTicketUrl mergedMetadata
        , eventImageUrl = emImageUrl mergedMetadata
        , eventIsPublic = emIsPublic mergedMetadata
        , eventType = emType mergedMetadata
        , eventStatus = emStatus mergedMetadata
        , eventCurrency = emCurrency mergedMetadata
        , eventBudgetCents = emBudgetCents mergedMetadata
        , eventCreatedAt = Just (socialEventCreatedAt existing)
        , eventUpdatedAt = Just now
        })

    uploadEventImage :: T.Text -> EventImageUploadForm -> AppM EventImageUploadDTO
    uploadEventImage rawId EventImageUploadForm{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      (eventKey, eventRow) <- requireManagedEvent rawId
      let mimeTypeVal = T.toLower (T.strip (fdFileCType eiuFile))
          fallbackName = nonEmptyText (fdFileName eiuFile)
          requestedName = eiuName >>= nonEmptyText
          nameWithExt = applyUploadExtension (requestedName <|> fallbackName) fallbackName
          safeName = sanitizeUploadFileName nameWithExt
      unless (isImageUpload mimeTypeVal safeName) $
        throwError err400
          { errBody =
              "Only raster image uploads with matching MIME type and extension are allowed"
          }

      uuid <- liftIO UUIDV4.nextRandom
      let eventIdTxt = renderKeyText eventKey
          storedName = UUID.toText uuid <> "-" <> safeName
          relPath = T.intercalate "/" ["social-events", "events", eventIdTxt, storedName]
          targetDir = assetsRootDir envConfig </> "social-events" </> "events" </> T.unpack eventIdTxt
          targetPath = targetDir </> T.unpack storedName
          assetsBase = resolveConfiguredAssetsBase envConfig
          publicUrl = buildUploadAssetUrl assetsBase relPath
          existingMeta = decodeEventMetadata (socialEventMetadata eventRow)
          updatedMeta = existingMeta { emImageUrl = Just publicUrl }
      liftIO $ createDirectoryIfMissing True targetDir
      liftIO $ copyFile (fdPayload eiuFile) targetPath
      liftIO $ runSqlPool
        (update eventKey
          [ SocialEventMetadata =. encodeEventMetadata updatedMeta
          , SocialEventUpdatedAt =. now
          ])
        envPool
      pure EventImageUploadDTO
        { eiuEventId = eventIdTxt
        , eiuFileName = storedName
        , eiuPath = relPath
        , eiuPublicUrl = publicUrl
        , eiuImageUrl = publicUrl
        }

    deleteEvent :: T.Text -> AppM NoContent
    deleteEvent rawId = do
      Env{..} <- ask
      eventKey <- parseKeyOr400 "event" rawId
      mExisting <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mExisting) $ throwError err404 { errBody = "Event not found" }
      liftIO $ runSqlPool
        (do
          deleteWhere [EventArtistEventId ==. eventKey]
          deleteWhere [EventRsvpEventId ==. eventKey]
          deleteWhere [EventInvitationEventId ==. eventKey]
          momentKeys <- selectKeysList [EventMomentEventId ==. eventKey] []
          unless (null momentKeys) $ do
            deleteWhere [EventMomentReactionMomentId <-. momentKeys]
            deleteWhere [EventMomentCommentMomentId <-. momentKeys]
          deleteWhere [EventMomentEventId ==. eventKey]
          deleteWhere [EventTicketEventId ==. eventKey]
          deleteWhere [EventTicketOrderEventId ==. eventKey]
          deleteWhere [EventTicketTierEventId ==. eventKey]
          deleteWhere [EventFinanceEntryEventId ==. eventKey]
          deleteWhere [EventBudgetLineEventId ==. eventKey]
          delete eventKey
        )
        envPool
      pure NoContent

    -- Venues
    venuesServer :: ServerT VenuesRoutes AppM
    venuesServer = listVenues
               :<|> createVenue
               :<|> getVenue
               :<|> updateVenue

    listVenues :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> AppM [VenueDTO]
    listVenues mCity mNear mQuery mLimit mOffset = do
      Env{..} <- ask
      limit <- resolveLimit 200 500 mLimit
      offset <- resolveOffset mOffset
      let searchNeedle = fmap (T.toCaseFold . T.strip) mQuery
      nearFilter <- case mNear of
        Nothing -> pure Nothing
        Just raw ->
          case parseNearQueryEither raw of
            Left e -> throwError e
            Right parsed -> pure (Just parsed)
      let filters = case mCity of
                      Just c | not (T.null (T.strip c)) -> [VenueCity ==. Just (T.strip c)]
                      _ -> []
      let hasTextQuery = maybe False (not . T.null) searchNeedle
          hasNearQuery = isJust nearFilter
          needsInMemoryFilter = hasTextQuery || hasNearQuery
      seeded <- if needsInMemoryFilter
        then liftIO $ runSqlPool (selectList filters [Asc VenueName, LimitTo 2000]) envPool
        else liftIO $ runSqlPool (selectList filters [Asc VenueName, LimitTo limit, OffsetBy offset]) envPool
      let matchesText q (Entity _ v) =
            let nameVal = T.toCaseFold (SM.venueName v)
                cityVal = maybe "" T.toCaseFold (SM.venueCity v)
                addressVal = maybe "" T.toCaseFold (SM.venueAddress v)
            in T.isInfixOf q nameVal || T.isInfixOf q cityVal || T.isInfixOf q addressVal
          matchesNear (lat, lng, radiusKm) (Entity _ v) =
            case (venueLatitude v, venueLongitude v) of
              (Just venueLat, Just venueLng) ->
                haversineDistanceKm lat lng venueLat venueLng <= radiusKm
              _ -> False
          rowsFilteredByText = case searchNeedle of
            Just q | not (T.null q) -> filter (matchesText q) seeded
            _ -> seeded
          rowsFiltered = case nearFilter of
            Just nearSpec -> filter (matchesNear nearSpec) rowsFilteredByText
            Nothing -> rowsFilteredByText
          rows = if needsInMemoryFilter
            then take limit (drop offset rowsFiltered)
            else seeded
      pure $ map (\(Entity vid v) ->
        let contactMeta = decodeVenueContactMetadata (SM.venueContact v)
        in VenueDTO
          { venueId = Just (renderKeyText vid)
          , venueName = SM.venueName v
          , venueAddress = SM.venueAddress v
          , venueCity = SM.venueCity v
          , venueCountry = SM.venueCountry v
          , venueLat = venueLatitude v
          , venueLng = venueLongitude v
          , venueCapacity = SM.venueCapacity v
          , venueContact = vcmPhone contactMeta
          , venuePhone = vcmPhone contactMeta
          , venueWebsite = vcmWebsite contactMeta
          , venueState = vcmState contactMeta
          , venueZipCode = vcmZipCode contactMeta
          , venueImageUrl = vcmImageUrl contactMeta
          , venueCreatedAt = Just (SM.venueCreatedAt v)
          , venueUpdatedAt = Just (SM.venueUpdatedAt v)
          }) rows

    createVenue :: VenueDTO -> AppM VenueDTO
    createVenue dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      either throwError pure $
        validateVenueCreateUpdateFields
          (venueName dto)
          (venueLat dto)
          (venueLng dto)
          (venueCapacity dto)
      let contactMeta = venueContactMetadataFromDTO dto
      key <- liftIO $ runSqlPool (insert Venue
        { venueName = venueName dto
        , venueAddress = venueAddress dto
        , venueCity = venueCity dto
        , venueCountry = venueCountry dto
        , venueLatitude = venueLat dto
        , venueLongitude = venueLng dto
        , venueCapacity = venueCapacity dto
        , venueContact = encodeVenueContactMetadata contactMeta
        , venueCreatedAt = now
        , venueUpdatedAt = now
        }) envPool
      pure (dto
        { venueId = Just (renderKeyText key)
        , venueContact = vcmPhone contactMeta
        , venuePhone = vcmPhone contactMeta
        , venueWebsite = vcmWebsite contactMeta
        , venueState = vcmState contactMeta
        , venueZipCode = vcmZipCode contactMeta
        , venueImageUrl = vcmImageUrl contactMeta
        , venueCreatedAt = Just now
        , venueUpdatedAt = Just now
        })

    getVenue :: T.Text -> AppM VenueDTO
    getVenue rawId = do
      Env{..} <- ask
      venueKey <- parseKeyOr400 "venue" rawId
      mEnt <- liftIO $ runSqlPool (get venueKey) envPool
      case mEnt of
        Nothing -> throwError err404 { errBody = "Venue not found" }
        Just v ->
          let contactMeta = decodeVenueContactMetadata (SM.venueContact v)
          in pure VenueDTO
              { venueId = Just (T.strip rawId)
              , venueName = SM.venueName v
              , venueAddress = SM.venueAddress v
              , venueCity = SM.venueCity v
              , venueCountry = SM.venueCountry v
              , venueLat = venueLatitude v
              , venueLng = venueLongitude v
              , venueCapacity = SM.venueCapacity v
              , venueContact = vcmPhone contactMeta
              , venuePhone = vcmPhone contactMeta
              , venueWebsite = vcmWebsite contactMeta
              , venueState = vcmState contactMeta
              , venueZipCode = vcmZipCode contactMeta
              , venueImageUrl = vcmImageUrl contactMeta
              , venueCreatedAt = Just (SM.venueCreatedAt v)
              , venueUpdatedAt = Just (SM.venueUpdatedAt v)
              }

    updateVenue :: T.Text -> VenueUpdateDTO -> AppM VenueDTO
    updateVenue rawId VenueUpdateDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      venueKey <- parseKeyOr400 "venue" rawId
      mExisting <- liftIO $ runSqlPool (get venueKey) envPool
      existing <- maybe (throwError err404 { errBody = "Venue not found" }) pure mExisting
      let dto = vudVenue
      either throwError pure $
        validateVenueCreateUpdateFields
          (venueName dto)
          (venueLat dto)
          (venueLng dto)
          (venueCapacity dto)
      let existingContactMeta = decodeVenueContactMetadata (SM.venueContact existing)
          mergedContactMeta = applyVenueContactUpdate vudContactUpdate existingContactMeta
      liftIO $ runSqlPool (update venueKey
        [ VenueName =. venueName dto
        , VenueAddress =. venueAddress dto
        , VenueCity =. venueCity dto
        , VenueCountry =. venueCountry dto
        , VenueLatitude =. venueLat dto
        , VenueLongitude =. venueLng dto
        , VenueCapacity =. venueCapacity dto
        , VenueContact =. encodeVenueContactMetadata mergedContactMeta
        , VenueUpdatedAt =. now
        ]) envPool
      pure (dto
        { venueId = Just rawId
        , venueContact = vcmPhone mergedContactMeta
        , venuePhone = vcmPhone mergedContactMeta
        , venueWebsite = vcmWebsite mergedContactMeta
        , venueState = vcmState mergedContactMeta
        , venueZipCode = vcmZipCode mergedContactMeta
        , venueImageUrl = vcmImageUrl mergedContactMeta
        , venueCreatedAt = Just (SM.venueCreatedAt existing)
        , venueUpdatedAt = Just now
        })

    -- Artists
    artistsServer :: ServerT ArtistsRoutes AppM
    artistsServer = listArtists
               :<|> createArtist
               :<|> getArtist
               :<|> updateArtist
               :<|> listArtistFollowers
               :<|> followArtist
               :<|> unfollowArtist

    listArtists :: Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> AppM [ArtistDTO]
    listArtists mNameFilter mGenreFilter mLimit mOffset = do
      Env{..} <- ask
      limit <- resolveLimit 500 1000 mLimit
      offset <- resolveOffset mOffset
      let nameFilter = normalizeFilter mNameFilter
          genreFilter = normalizeFilter mGenreFilter
          hasFilter = isJust nameFilter || isJust genreFilter
      rows <- liftIO $ runSqlPool
        (selectList []
          ([Desc ArtistProfileCreatedAt] ++
            if hasFilter
              then [LimitTo 1000]
              else [LimitTo limit, OffsetBy offset]))
        envPool
      artists <- forM rows $ \(Entity aid a) -> do
        genres <- liftIO $ runSqlPool (selectList [ArtistGenreArtistId ==. aid] []) envPool
        let genreList = artistGenresFromRowsAndFallback genres (artistProfileGenres a)
        let nameMatches = case nameFilter of
              Nothing -> True
              Just name -> T.isInfixOf name (T.toCaseFold (artistProfileName a))
        let genreMatches = case genreFilter of
              Nothing -> True
              Just genre -> any ((== genre) . T.toCaseFold) genreList
        pure $ if nameMatches && genreMatches
          then Just ArtistDTO
            { artistId = Just (renderKeyText aid)
            , artistPartyId = artistProfilePartyId a
            , artistName = artistProfileName a
            , artistGenres = genreList
            , artistBio = artistProfileBio a
            , artistAvatarUrl = artistProfileAvatarUrl a
            , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
            , artistCreatedAt = Just (artistProfileCreatedAt a)
            , artistUpdatedAt = Just (artistProfileUpdatedAt a)
            }
          else Nothing
      let filtered = catMaybes artists
      pure
        (if hasFilter
          then take limit (drop offset filtered)
          else filtered)

    normalizeFilter :: Maybe T.Text -> Maybe T.Text
    normalizeFilter mVal =
      case fmap (T.toCaseFold . T.strip) mVal of
        Nothing -> Nothing
        Just t | T.null t -> Nothing
        Just t -> Just t

    resolveLimit :: Int -> Int -> Maybe Int -> AppM Int
    resolveLimit defaultLimit maxLimit mVal =
      case mVal of
        Nothing -> pure defaultLimit
        Just n
          | n <= 0 -> throwError err400 { errBody = "limit must be greater than 0" }
          | n > maxLimit -> throwError err400 { errBody = "limit exceeds allowed maximum" }
          | otherwise -> pure n

    resolveOffset :: Maybe Int -> AppM Int
    resolveOffset mVal =
      case mVal of
        Nothing -> pure 0
        Just n
          | n < 0 -> throwError err400 { errBody = "offset must be greater than or equal to 0" }
          | otherwise -> pure n

    listArtistFollowers :: T.Text -> AppM [ArtistFollowerDTO]
    listArtistFollowers artistIdStr = do
      Env{..} <- ask
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      rows <- liftIO $ runSqlPool
        (selectList [ArtistFollowArtistId ==. artistKey] [Desc ArtistFollowCreatedAt])
        envPool
      let artistIdTxt = renderKeyText artistKey
      pure $ map (\(Entity _ follow) ->
        ArtistFollowerDTO
          { afFollowId = Just (renderFollowId artistKey (artistFollowFollowerPartyId follow))
          , afArtistId = Just artistIdTxt
          , afFollowerPartyId = artistFollowFollowerPartyId follow
          , afCreatedAt = Just (artistFollowCreatedAt follow)
          }) rows

    createArtist :: ArtistDTO -> AppM ArtistDTO
    createArtist dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      when (T.null (T.strip (artistName dto))) $ throwError err400 { errBody = "artist name is required" }
      let genreList = normalizeArtistGenres (artistGenres dto)
      key <- liftIO $ runSqlPool (insert ArtistProfile
        { artistProfilePartyId = cleanMaybeText (artistPartyId dto)
        , artistProfileName = artistName dto
        , artistProfileBio = artistBio dto
        , artistProfileAvatarUrl = artistAvatarUrl dto
        -- Keep this nullable for compatibility with deployments where the
        -- legacy column type is TEXT instead of TEXT[].
        , artistProfileGenres = Nothing
        , artistProfileSocialLinks = encodeSocialLinks (artistSocialLinks dto)
        , artistProfileCreatedAt = now
        , artistProfileUpdatedAt = now
        }) envPool
      liftIO $ runSqlPool
        (forM_ genreList $ \g ->
           insert_ ArtistGenre
             { artistGenreArtistId = key
             , artistGenreGenre = g
             }
        )
        envPool
      pure ArtistDTO
        { artistId = Just (renderKeyText key)
        , artistPartyId = cleanMaybeText (artistPartyId dto)
        , artistName = artistName dto
        , artistGenres = genreList
        , artistBio = artistBio dto
        , artistAvatarUrl = artistAvatarUrl dto
        , artistSocialLinks = artistSocialLinks dto
        , artistCreatedAt = Just now
        , artistUpdatedAt = Just now
        }

    getArtist :: T.Text -> AppM ArtistDTO
    getArtist idStr = do
      Env{..} <- ask
      artistKey <- parseArtistId idStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      case mArtist of
        Nothing -> throwError err404 { errBody = "Artist not found" }
        Just a -> do
          genres <- liftIO $ runSqlPool (selectList [ArtistGenreArtistId ==. artistKey] []) envPool
          let genreList = artistGenresFromRowsAndFallback genres (artistProfileGenres a)
          pure ArtistDTO
            { artistId = Just (T.strip idStr)
            , artistPartyId = artistProfilePartyId a
            , artistName = artistProfileName a
            , artistGenres = genreList
            , artistBio = artistProfileBio a
            , artistAvatarUrl = artistProfileAvatarUrl a
            , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
            , artistCreatedAt = Just (artistProfileCreatedAt a)
            , artistUpdatedAt = Just (artistProfileUpdatedAt a)
            }

    updateArtist :: T.Text -> ArtistDTO -> AppM ArtistDTO
    updateArtist idStr dto = do
      Env{..} <- ask
      artistKey <- parseArtistId idStr
      now <- liftIO getCurrentTime
      when (T.null (T.strip (artistName dto))) $ throwError err400 { errBody = "artist name is required" }
      mExisting <- liftIO $ runSqlPool (get artistKey) envPool
      existing <- maybe (throwError err404 { errBody = "Artist not found" }) pure mExisting
      let genreList = normalizeArtistGenres (artistGenres dto)
      let nextPartyId = cleanMaybeText (artistPartyId dto) <|> artistProfilePartyId existing
      liftIO $ runSqlPool (update artistKey
        [ ArtistProfilePartyId =. nextPartyId
        , ArtistProfileName =. artistName dto
        , ArtistProfileBio =. artistBio dto
        , ArtistProfileAvatarUrl =. artistAvatarUrl dto
        , ArtistProfileSocialLinks =. encodeSocialLinks (artistSocialLinks dto)
        , ArtistProfileUpdatedAt =. now
        ]) envPool
      liftIO $ runSqlPool (deleteWhere [ArtistGenreArtistId ==. artistKey]) envPool
      liftIO $ runSqlPool
        (forM_ genreList $ \g ->
           insert_ ArtistGenre
             { artistGenreArtistId = artistKey
             , artistGenreGenre = g
             }
        )
        envPool
      pure dto
        { artistId = Just (T.strip idStr)
        , artistGenres = genreList
        , artistPartyId = nextPartyId
        , artistCreatedAt = Just (artistProfileCreatedAt existing)
        , artistUpdatedAt = Just now
        }

    followArtist :: T.Text -> ArtistFollowRequest -> AppM ArtistFollowerDTO
    followArtist artistIdStr ArtistFollowRequest{..} = do
      Env{..} <- ask
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      followerParty <-
        liftIO (resolveExistingPartyIdText envPool "followerPartyId" afrFollowerPartyId)
          >>= either throwError pure
      liftIO $ followArtistDb envPool artistKey followerParty

    unfollowArtist :: T.Text -> Maybe T.Text -> AppM NoContent
    unfollowArtist artistIdStr mFollower = do
      Env{..} <- ask
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      followerParty <- either throwError pure (parseFollowerQueryParamEither mFollower)
      liftIO $ runSqlPool
        (deleteWhere [ArtistFollowArtistId ==. artistKey, ArtistFollowFollowerPartyId ==. followerParty])
        envPool
      pure NoContent

    -- RSVPs
    rsvpsServer :: ServerT RsvpRoutes AppM
    rsvpsServer = listRsvps :<|> createRsvp

    listRsvps :: T.Text -> AppM [RsvpDTO]
    listRsvps eventIdStr = do
      Env{..} <- ask
      eventKey <- parseKeyOr400 "event" eventIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
      rsvpRows <- liftIO $ runSqlPool (selectList [EventRsvpEventId ==. eventKey] []) envPool
      pure $ map (\(Entity rid rsvp) -> RsvpDTO
        { rsvpId = Just (renderKeyText rid)
        , rsvpEventId = eventIdStr
        , rsvpPartyId = eventRsvpPartyId rsvp
        , rsvpStatus = eventRsvpStatus rsvp
        , rsvpCreatedAt = Just (eventRsvpCreatedAt rsvp)
        , rsvpUpdatedAt = Just (eventRsvpUpdatedAt rsvp)
        }) rsvpRows

    createRsvp :: T.Text -> RsvpCreateDTO -> AppM RsvpDTO
    createRsvp eventIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      let eventIdVal = T.strip eventIdStr
          RsvpCreateDTO partyIdInput statusInput = dto
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
      statusVal <- either throwError pure (validateRsvpStatus statusInput)
      partyIdVal <-
        liftIO (resolveExistingPartyIdText envPool "rsvpPartyId" partyIdInput)
          >>= either throwError pure

      existingRsvps <- liftIO $ runSqlPool
        (selectList [EventRsvpEventId ==. eventKey, EventRsvpPartyId ==. partyIdVal] [])
        envPool

      case existingRsvps of
        [] -> do
          key <- liftIO $ runSqlPool (insert EventRsvp
            { eventRsvpEventId = eventKey
            , eventRsvpPartyId = partyIdVal
            , eventRsvpStatus = statusVal
            , eventRsvpMetadata = Nothing
            , eventRsvpCreatedAt = now
            , eventRsvpUpdatedAt = now
            }) envPool
          pure RsvpDTO
            { rsvpId = Just (renderKeyText key)
            , rsvpEventId = eventIdVal
            , rsvpPartyId = partyIdVal
            , rsvpStatus = statusVal
            , rsvpCreatedAt = Just now
            , rsvpUpdatedAt = Just now
            }
        (Entity existingKey existing : _) -> do
          liftIO $ runSqlPool (update existingKey
            [ EventRsvpStatus =. statusVal
            , EventRsvpUpdatedAt =. now
            ]) envPool
          pure RsvpDTO
            { rsvpId = Just (renderKeyText existingKey)
            , rsvpEventId = eventIdVal
            , rsvpPartyId = partyIdVal
            , rsvpStatus = statusVal
            , rsvpCreatedAt = Just (eventRsvpCreatedAt existing)
            , rsvpUpdatedAt = Just now
            }

    -- Invitations
    invitationsServer :: ServerT InvitationsRoutes AppM
    invitationsServer eventIdStr =
      listInvitations eventIdStr
        :<|> createInvitation eventIdStr
        :<|> updateInvitation eventIdStr

    momentsServer :: ServerT MomentsRoutes AppM
    momentsServer = listMoments
               :<|> createMoment
               :<|> reactToMoment
               :<|> commentOnMoment

    listInvitations :: T.Text -> AppM [InvitationDTO]
    listInvitations eventIdStr = do
      Env{..} <- ask
      eventKey <- parseKeyOr400 "event" eventIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
      rows <- liftIO $ runSqlPool (selectList [EventInvitationEventId ==. eventKey] [Desc EventInvitationCreatedAt]) envPool
      pure $ map (\(Entity iid inv) ->
        InvitationDTO
          { invitationId = Just (renderKeyText iid)
          , invitationEventId = Just (T.strip eventIdStr)
          , invitationFromPartyId = eventInvitationFromPartyId inv
          , invitationToPartyId = maybe "" id (eventInvitationToPartyId inv)
          , invitationStatus = eventInvitationStatus inv
          , invitationMessage = eventInvitationMessage inv
          , invitationCreatedAt = Just (eventInvitationCreatedAt inv)
          , invitationUpdatedAt = Just (eventInvitationUpdatedAt inv)
          }
        ) rows

    createInvitation :: T.Text -> InvitationDTO -> AppM InvitationDTO
    createInvitation eventIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
      toParty <- either throwError pure (validateInvitationToPartyId (invitationToPartyId dto))
      statusVal <- either throwError pure (validateInvitationStatusInput (invitationStatus dto))
      key <- liftIO $ runSqlPool (insert EventInvitation
        { eventInvitationEventId = eventKey
        , eventInvitationFromPartyId = cleanMaybeText (invitationFromPartyId dto)
        , eventInvitationToPartyId = Just toParty
        , eventInvitationStatus = Just statusVal
        , eventInvitationMessage = invitationMessage dto
        , eventInvitationCreatedAt = now
        , eventInvitationUpdatedAt = now
        }) envPool
      pure InvitationDTO
        { invitationId = Just (renderKeyText key)
        , invitationEventId = Just (T.strip eventIdStr)
        , invitationFromPartyId = invitationFromPartyId dto
        , invitationToPartyId = toParty
        , invitationStatus = Just statusVal
        , invitationMessage = invitationMessage dto
        , invitationCreatedAt = Just now
        , invitationUpdatedAt = Just now
        }

    updateInvitation :: T.Text -> T.Text -> InvitationUpdateDTO -> AppM InvitationDTO
    updateInvitation eventIdStr invitationIdStr InvitationUpdateDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      (eventKey, invitationKey) <- parseIds eventIdStr invitationIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
      mExisting <- liftIO $ runSqlPool (get invitationKey) envPool
      case mExisting of
        Nothing -> throwError err404 { errBody = "Invitation not found" }
        Just inv -> do
          let dto = iudInvitation
          when (eventInvitationEventId inv /= eventKey) $ throwError err400 { errBody = "Invitation does not belong to this event" }
          mStatusVal <- either throwError pure (validateInvitationStatusUpdateInput (invitationStatus dto))
          let messageVal = applyNullableTextUpdate iudMessageUpdate (eventInvitationMessage inv)
              statusUpdates =
                maybe [] (\statusVal -> [EventInvitationStatus =. Just statusVal]) mStatusVal
              responseStatus = mStatusVal <|> eventInvitationStatus inv
          toPartyVal <- either throwError pure (validateInvitationToPartyId (invitationToPartyId dto))
          liftIO $ runSqlPool
            (update invitationKey
              ( statusUpdates <>
                [ EventInvitationMessage =. messageVal
                , EventInvitationToPartyId =. Just toPartyVal
                , EventInvitationUpdatedAt =. now
                ]
              ))
            envPool
          pure InvitationDTO
            { invitationId = Just (renderKeyText invitationKey)
            , invitationEventId = Just (T.strip eventIdStr)
            , invitationFromPartyId = eventInvitationFromPartyId inv
            , invitationToPartyId = toPartyVal
            , invitationStatus = responseStatus
            , invitationMessage = messageVal
            , invitationCreatedAt = Just (eventInvitationCreatedAt inv)
            , invitationUpdatedAt = Just now
            }

    -- Moments
    listMoments :: T.Text -> AppM [EventMomentDTO]
    listMoments eventIdStr = do
      Env{..} <- ask
      eventKey <- parseKeyOr400 "event" eventIdStr
      _ <- requireExistingEvent envPool eventKey
      liftIO $ loadEventMoments envPool eventKey

    createMoment :: T.Text -> EventMomentCreateDTO -> AppM EventMomentDTO
    createMoment eventIdStr EventMomentCreateDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      _ <- requireExistingEvent envPool eventKey
      mediaUrl <- maybe (throwError err400 { errBody = "Moment media URL is required" }) pure (cleanMaybeText (Just emCreateMediaUrl))
      mediaType <- maybe (throwError err400 { errBody = "Moment media type must be image or video" }) pure (normalizeMomentMediaType emCreateMediaType)
      caption <- either throwError pure (normalizeMomentCaption emCreateCaption)
      let authorName = resolveMomentAuthorName currentPartyId emCreateAuthorName
          mediaWidth = normalizePositiveIntMaybe emCreateMediaWidth
          mediaHeight = normalizePositiveIntMaybe emCreateMediaHeight
          mediaDurationMs = normalizeNonNegativeIntMaybe emCreateMediaDurationMs
      momentKey <- liftIO $ runSqlPool
        (insert EventMoment
          { eventMomentEventId = eventKey
          , eventMomentAuthorPartyId = Just currentPartyId
          , eventMomentAuthorName = authorName
          , eventMomentCaption = caption
          , eventMomentMediaUrl = mediaUrl
          , eventMomentMediaType = mediaType
          , eventMomentMediaWidth = mediaWidth
          , eventMomentMediaHeight = mediaHeight
          , eventMomentMediaDurationMs = mediaDurationMs
          , eventMomentCreatedAt = now
          , eventMomentUpdatedAt = now
          })
        envPool
      liftIO $ loadMomentDTO envPool momentKey

    reactToMoment :: T.Text -> T.Text -> EventMomentReactionRequestDTO -> AppM EventMomentDTO
    reactToMoment eventIdStr momentIdStr EventMomentReactionRequestDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      _ <- requireExistingEvent envPool eventKey
      momentKey <- parseKeyOr400 "moment" momentIdStr
      _ <- requireMomentForEvent envPool eventKey momentKey
      reaction <- maybe (throwError err400 { errBody = "Moment reaction must be fire, love, or applause" }) pure (normalizeMomentReaction emrrReaction)
      let sameReactionKey = EventMomentReactionKey momentKey reaction currentPartyId
      existingSameReaction <- liftIO $ runSqlPool (get sameReactionKey) envPool
      liftIO $ runSqlPool
        (do
          deleteWhere [EventMomentReactionMomentId ==. momentKey, EventMomentReactionReactorPartyId ==. currentPartyId]
          when (isNothing existingSameReaction) $
            insert_ EventMomentReaction
              { eventMomentReactionMomentId = momentKey
              , eventMomentReactionReaction = reaction
              , eventMomentReactionReactorPartyId = currentPartyId
              , eventMomentReactionCreatedAt = now
              })
        envPool
      liftIO $ loadMomentDTO envPool momentKey

    commentOnMoment :: T.Text -> T.Text -> EventMomentCommentCreateDTO -> AppM EventMomentCommentDTO
    commentOnMoment eventIdStr momentIdStr EventMomentCommentCreateDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      _ <- requireExistingEvent envPool eventKey
      momentKey <- parseKeyOr400 "moment" momentIdStr
      _ <- requireMomentForEvent envPool eventKey momentKey
      body <- either throwError pure (normalizeMomentCommentBody emccBody)
      let authorName = resolveMomentAuthorName currentPartyId emccAuthorName
      commentKey <- liftIO $ runSqlPool
        (insert EventMomentComment
          { eventMomentCommentMomentId = momentKey
          , eventMomentCommentAuthorPartyId = Just currentPartyId
          , eventMomentCommentAuthorName = authorName
          , eventMomentCommentBody = body
          , eventMomentCommentCreatedAt = now
          , eventMomentCommentUpdatedAt = now
          })
        envPool
      mComment <- liftIO $ runSqlPool (get commentKey) envPool
      case mComment of
        Nothing -> throwError err500 { errBody = "Moment comment could not be loaded after insert" }
        Just commentRow -> pure (momentCommentEntityToDTO commentKey commentRow)

    -- Tickets
    ticketsServer :: ServerT TicketsRoutes AppM
    ticketsServer = listTicketTiers
               :<|> createTicketTier
               :<|> updateTicketTier
               :<|> listTicketOrders
               :<|> createTicketOrder
               :<|> updateTicketOrderStatus
               :<|> listTickets
               :<|> checkInTicket

    listTicketTiers :: T.Text -> AppM [TicketTierDTO]
    listTicketTiers eventIdStr = do
      Env{..} <- ask
      eventKey <- parseKeyOr400 "event" eventIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
      rows <- liftIO $ runSqlPool
        (selectList [EventTicketTierEventId ==. eventKey] [Asc EventTicketTierPosition, Asc EventTicketTierId])
        envPool
      pure (map (ticketTierEntityToDTO eventKey) rows)

    createTicketTier :: T.Text -> TicketTierDTO -> AppM TicketTierDTO
    createTicketTier eventIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      eventVal <- maybe (throwError err404 { errBody = "Event not found" }) pure mEvent
      _ <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal
      let tierName = T.strip (ticketTierName dto)
      when (T.null tierName) $ throwError err400 { errBody = "ticket tier name is required" }
      when (ticketTierPriceCents dto < 0) $ throwError err400 { errBody = "ticket tier price must be >= 0" }
      when (ticketTierQuantityTotal dto <= 0) $ throwError err400 { errBody = "ticket tier quantity must be > 0" }
      currencyVal <- either throwError pure $
        validateTicketTierCurrencyInput (eventCurrencyFromEvent eventVal) (ticketTierCurrency dto)
      let baseCode = cleanMaybeText (Just (ticketTierCode dto)) <|> Just tierName
          tierCode = normalizeTicketTierCode (fromMaybe tierName baseCode)
          salesStartVal = ticketTierSalesStart dto
          salesEndVal = ticketTierSalesEnd dto
      when (invalidSalesWindow salesStartVal salesEndVal) $ throwError err400 { errBody = "invalid sales window" }
      mInserted <- liftIO $ runSqlPool (insertUnique EventTicketTier
        { eventTicketTierEventId = eventKey
        , eventTicketTierCode = tierCode
        , eventTicketTierName = tierName
        , eventTicketTierDescription = cleanMaybeText (ticketTierDescription dto)
        , eventTicketTierPriceCents = ticketTierPriceCents dto
        , eventTicketTierCurrency = currencyVal
        , eventTicketTierQuantityTotal = ticketTierQuantityTotal dto
        , eventTicketTierQuantitySold = 0
        , eventTicketTierSalesStart = salesStartVal
        , eventTicketTierSalesEnd = salesEndVal
        , eventTicketTierIsActive = ticketTierActive dto
        , eventTicketTierPosition = ticketTierPosition dto
        , eventTicketTierCreatedAt = now
        , eventTicketTierUpdatedAt = now
        }) envPool
      tierKey <- maybe (throwError err409 { errBody = "ticket tier code already exists for this event" }) pure mInserted
      mTier <- liftIO $ runSqlPool (getEntity tierKey) envPool
      maybe (throwError err500 { errBody = "Could not create ticket tier" })
            (pure . ticketTierEntityToDTO eventKey)
            mTier

    updateTicketTier :: T.Text -> T.Text -> TicketTierDTO -> AppM TicketTierDTO
    updateTicketTier eventIdStr tierIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      tierKey <- parseKeyOr400 "ticket tier" tierIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      eventVal <- maybe (throwError err404 { errBody = "Event not found" }) pure mEvent
      _ <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal
      mTier <- liftIO $ runSqlPool (get tierKey) envPool
      tier <- maybe (throwError err404 { errBody = "Ticket tier not found" }) pure mTier
      when (eventTicketTierEventId tier /= eventKey) $ throwError err400 { errBody = "Ticket tier does not belong to this event" }
      let tierName = T.strip (ticketTierName dto)
      when (T.null tierName) $ throwError err400 { errBody = "ticket tier name is required" }
      when (ticketTierPriceCents dto < 0) $ throwError err400 { errBody = "ticket tier price must be >= 0" }
      when (ticketTierQuantityTotal dto < eventTicketTierQuantitySold tier) $ throwError err400 { errBody = "ticket tier quantity cannot be below sold quantity" }
      currencyVal <- either throwError pure $
        validateTicketTierCurrencyInput (eventCurrencyFromEvent eventVal) (ticketTierCurrency dto)
      let baseCode = cleanMaybeText (Just (ticketTierCode dto)) <|> Just tierName
          tierCode = normalizeTicketTierCode (fromMaybe tierName baseCode)
          salesStartVal = ticketTierSalesStart dto
          salesEndVal = ticketTierSalesEnd dto
      when (invalidSalesWindow salesStartVal salesEndVal) $ throwError err400 { errBody = "invalid sales window" }
      mCodeOwner <- liftIO $ runSqlPool (getBy (UniqueEventTicketTierCode eventKey tierCode)) envPool
      case mCodeOwner of
        Just (Entity existingKey _) | existingKey /= tierKey ->
          throwError err409 { errBody = "ticket tier code already exists for this event" }
        _ -> pure ()
      liftIO $ runSqlPool (update tierKey
        [ EventTicketTierCode =. tierCode
        , EventTicketTierName =. tierName
        , EventTicketTierDescription =. cleanMaybeText (ticketTierDescription dto)
        , EventTicketTierPriceCents =. ticketTierPriceCents dto
        , EventTicketTierCurrency =. currencyVal
        , EventTicketTierQuantityTotal =. ticketTierQuantityTotal dto
        , EventTicketTierSalesStart =. salesStartVal
        , EventTicketTierSalesEnd =. salesEndVal
        , EventTicketTierIsActive =. ticketTierActive dto
        , EventTicketTierPosition =. ticketTierPosition dto
        , EventTicketTierUpdatedAt =. now
        ]) envPool
      mUpdated <- liftIO $ runSqlPool (getEntity tierKey) envPool
      maybe (throwError err500 { errBody = "Could not update ticket tier" })
            (pure . ticketTierEntityToDTO eventKey)
            mUpdated

    listTicketOrders :: T.Text -> Maybe T.Text -> Maybe T.Text -> AppM [TicketOrderDTO]
    listTicketOrders eventIdStr mBuyerPartyId mStatus = do
      Env{..} <- ask
      eventKey <- parseKeyOr400 "event" eventIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      eventVal <- maybe (throwError err404 { errBody = "Event not found" }) pure mEvent
      let manager = isEventManager currentPartyId eventVal
      requestedBuyer <- either throwError pure
        (validateOptionalTicketBuyerPartyId "buyerPartyId" mBuyerPartyId)
      buyerFilters <-
        if manager
          then pure $ maybe [] (\buyer -> [EventTicketOrderBuyerPartyId ==. Just buyer]) requestedBuyer
          else case requestedBuyer of
            Nothing -> pure [EventTicketOrderBuyerPartyId ==. Just currentPartyId]
            Just buyer
              | buyer == currentPartyId -> pure [EventTicketOrderBuyerPartyId ==. Just currentPartyId]
              | otherwise -> throwError err403 { errBody = "You can only list your own ticket orders" }
      statusFilters <- case cleanMaybeText mStatus of
        Nothing -> pure []
        Just raw -> case parseTicketOrderStatus raw of
          Nothing -> throwError err400 { errBody = "Invalid ticket order status" }
          Just statusVal -> pure [EventTicketOrderStatus ==. statusVal]
      let filters = [EventTicketOrderEventId ==. eventKey] ++ buyerFilters ++ statusFilters
      rows <- liftIO $ runSqlPool (selectList filters [Desc EventTicketOrderPurchasedAt, LimitTo 200]) envPool
      forM rows $ \orderEnt@(Entity orderKey _) -> do
        tickets <- liftIO $ runSqlPool (selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId]) envPool
        pure (ticketOrderEntityToDTO orderEnt tickets)

    createTicketOrder :: T.Text -> TicketPurchaseRequestDTO -> AppM TicketOrderDTO
    createTicketOrder eventIdStr TicketPurchaseRequestDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      tierKey <- parseKeyOr400 "ticket tier" ticketPurchaseTierId
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      eventVal <- maybe (throwError err404 { errBody = "Event not found" }) pure mEvent
      mTier <- liftIO $ runSqlPool (get tierKey) envPool
      tier <- maybe (throwError err404 { errBody = "Ticket tier not found" }) pure mTier
      when (eventTicketTierEventId tier /= eventKey) $ throwError err400 { errBody = "Ticket tier does not belong to this event" }
      when (ticketPurchaseQuantity <= 0) $ throwError err400 { errBody = "Quantity must be > 0" }
      when (not (isTicketTierSaleOpen now tier)) $ throwError err400 { errBody = "Ticket sales are closed for this tier" }

      let manager = isEventManager currentPartyId eventVal
      requestedBuyer <- either throwError pure
        (validateOptionalTicketBuyerPartyId "ticketPurchaseBuyerPartyId" ticketPurchaseBuyerPartyId)
      buyerParty <- case requestedBuyer of
        Nothing -> pure (Just currentPartyId)
        Just buyer
          | buyer == currentPartyId -> pure (Just currentPartyId)
          | manager -> pure (Just buyer)
          | otherwise -> throwError err403 { errBody = "Cannot assign tickets to another buyer" }

      soldAcross <- liftIO $ runSqlPool
        (selectList [EventTicketTierEventId ==. eventKey] [])
        envPool
      let soldCount = sum (map (eventTicketTierQuantitySold . entityVal) soldAcross)
          availableInTier = ticketTierAvailability tier
      when (ticketPurchaseQuantity > availableInTier) $ throwError err409 { errBody = "Not enough tickets available" }
      case socialEventCapacity eventVal of
        Nothing -> pure ()
        Just cap ->
          when (soldCount + ticketPurchaseQuantity > cap) $ throwError err409 { errBody = "Event capacity reached" }

      let orderAmountCents = ticketPurchaseQuantity * eventTicketTierPriceCents tier
          buyerName = cleanMaybeText ticketPurchaseBuyerName
      buyerEmail <-
        either throwError pure (validateTicketPurchaseBuyerEmail ticketPurchaseBuyerEmail)
      when (orderAmountCents < 0) $ throwError err400 { errBody = "Invalid amount" }

      orderDto <- liftIO $ runSqlPool (do
        update tierKey
          [ EventTicketTierQuantitySold +=. ticketPurchaseQuantity
          , EventTicketTierUpdatedAt =. now
          ]
        let orderRecord = EventTicketOrder
              { eventTicketOrderEventId = eventKey
              , eventTicketOrderTierId = tierKey
              , eventTicketOrderBuyerPartyId = buyerParty
              , eventTicketOrderBuyerName = buyerName
              , eventTicketOrderBuyerEmail = buyerEmail
              , eventTicketOrderQuantity = ticketPurchaseQuantity
              , eventTicketOrderAmountCents = orderAmountCents
              , eventTicketOrderCurrency = eventTicketTierCurrency tier
              , eventTicketOrderStatus = "paid"
              , eventTicketOrderMetadata = Nothing
              , eventTicketOrderPurchasedAt = now
              , eventTicketOrderCreatedAt = now
              , eventTicketOrderUpdatedAt = now
              }
        orderKey <- insert orderRecord
        tickets <- replicateM ticketPurchaseQuantity $ do
          ticketCodeValue <- generateUniqueTicketCode
          ticketKey <- insert EventTicket
            { eventTicketEventId = eventKey
            , eventTicketTierRefId = tierKey
            , eventTicketOrderRefId = orderKey
            , eventTicketHolderName = buyerName
            , eventTicketHolderEmail = buyerEmail
            , eventTicketCode = ticketCodeValue
            , eventTicketStatus = "issued"
            , eventTicketCheckedInAt = Nothing
            , eventTicketCreatedAt = now
            , eventTicketUpdatedAt = now
            }
          getJustEntity ticketKey
        pure (ticketOrderEntityToDTO (Entity orderKey orderRecord) tickets)
        ) envPool

      pure orderDto

    updateTicketOrderStatus :: T.Text -> T.Text -> TicketOrderStatusUpdateDTO -> AppM TicketOrderDTO
    updateTicketOrderStatus eventIdStr orderIdStr TicketOrderStatusUpdateDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      orderKey <- parseKeyOr400 "ticket order" orderIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      eventVal <- maybe (throwError err404 { errBody = "Event not found" }) pure mEvent
      _ <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal
      newStatus <- case parseTicketOrderStatus ticketOrderStatus of
        Nothing -> throwError err400 { errBody = "Invalid ticket order status" }
        Just "pending" -> throwError err400 { errBody = "Use paid, cancelled or refunded" }
        Just s -> pure s

      mOrder <- liftIO $ runSqlPool (get orderKey) envPool
      order <- maybe (throwError err404 { errBody = "Ticket order not found" }) pure mOrder
      when (eventTicketOrderEventId order /= eventKey) $ throwError err400 { errBody = "Ticket order does not belong to this event" }
      let oldStatus = normalizeTicketOrderStatus (Just (eventTicketOrderStatus order))
      when (oldStatus `elem` ["cancelled", "refunded"] && newStatus == "paid") $
        throwError err400 { errBody = "Closed orders cannot be moved back to paid" }

      mTier <- liftIO $ runSqlPool (get (eventTicketOrderTierId order)) envPool
      tier <- maybe (throwError err404 { errBody = "Ticket tier not found" }) pure mTier
      let qty = eventTicketOrderQuantity order
          tierAvailable = ticketTierAvailability tier
          capacity = socialEventCapacity eventVal
          soldAdjust
            | oldStatus == "paid" && newStatus /= "paid" = negate qty
            | oldStatus /= "paid" && newStatus == "paid" = qty
            | otherwise = 0
      when (soldAdjust > 0 && soldAdjust > tierAvailable) $ throwError err409 { errBody = "Not enough ticket inventory to mark as paid" }
      when (soldAdjust > 0) $ do
        soldAcross <- liftIO $ runSqlPool
          (selectList [EventTicketTierEventId ==. eventKey] [])
          envPool
        let soldCount = sum (map (eventTicketTierQuantitySold . entityVal) soldAcross)
        case capacity of
          Nothing -> pure ()
          Just cap ->
            when (soldCount + soldAdjust > cap) $ throwError err409 { errBody = "Event capacity reached" }
      when (eventTicketTierQuantitySold tier + soldAdjust < 0) $ throwError err409 { errBody = "Sold quantity underflow" }

      let nextTicketStatus = case newStatus of
            "paid" -> "issued"
            "cancelled" -> "cancelled"
            "refunded" -> "refunded"
            _ -> "issued"
      orderDto <- liftIO $ runSqlPool (do
        when (soldAdjust /= 0) $
          update (eventTicketOrderTierId order)
            [ EventTicketTierQuantitySold +=. soldAdjust
            , EventTicketTierUpdatedAt =. now
            ]
        update orderKey
          [ EventTicketOrderStatus =. newStatus
          , EventTicketOrderUpdatedAt =. now
          ]
        let ticketUpdates =
              [ EventTicketStatus =. nextTicketStatus
              , EventTicketUpdatedAt =. now
              ] ++
              if nextTicketStatus == "issued"
                then [EventTicketCheckedInAt =. Nothing]
                else []
        updateWhere [EventTicketOrderRefId ==. orderKey] ticketUpdates
        mOrderEnt <- getEntity orderKey
        case mOrderEnt of
          Nothing -> pure Nothing
          Just orderEnt -> do
            tickets <- selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId]
            pure (Just (ticketOrderEntityToDTO orderEnt tickets))
        ) envPool

      maybe (throwError err500 { errBody = "Could not update ticket order" }) pure orderDto

    listTickets :: T.Text -> Maybe T.Text -> Maybe T.Text -> AppM [TicketDTO]
    listTickets eventIdStr mOrderId mStatus = do
      Env{..} <- ask
      eventKey <- parseKeyOr400 "event" eventIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      eventVal <- maybe (throwError err404 { errBody = "Event not found" }) pure mEvent
      let manager = isEventManager currentPartyId eventVal
      orderFilters <- case cleanMaybeText mOrderId of
        Nothing ->
          if manager
            then pure []
            else do
              ownOrders <- liftIO $ runSqlPool
                (selectList [EventTicketOrderEventId ==. eventKey, EventTicketOrderBuyerPartyId ==. Just currentPartyId] [LimitTo 500])
                envPool
              let orderIds = map entityKey ownOrders
              if null orderIds
                then pure [EventTicketId ==. toSqlKey 0]
                else pure [EventTicketOrderRefId <-. orderIds]
        Just rawOrderId -> do
          orderKey <- parseKeyOr400 "ticket order" rawOrderId
          mOrder <- liftIO $ runSqlPool (get orderKey) envPool
          order <- maybe (throwError err404 { errBody = "Ticket order not found" }) pure mOrder
          when (eventTicketOrderEventId order /= eventKey) $ throwError err400 { errBody = "Ticket order does not belong to this event" }
          when (not manager && eventTicketOrderBuyerPartyId order /= Just currentPartyId) $
            throwError err403 { errBody = "You can only list your own tickets" }
          pure [EventTicketOrderRefId ==. orderKey]

      statusFilters <- case cleanMaybeText mStatus of
        Nothing -> pure []
        Just raw -> case parseTicketStatus raw of
          Nothing -> throwError err400 { errBody = "Invalid ticket status" }
          Just statusVal -> pure [EventTicketStatus ==. statusVal]

      let filters = [EventTicketEventId ==. eventKey] ++ orderFilters ++ statusFilters
      rows <- liftIO $ runSqlPool (selectList filters [Asc EventTicketId, LimitTo 400]) envPool
      pure (map ticketEntityToDTO rows)

    checkInTicket :: T.Text -> TicketCheckInRequestDTO -> AppM TicketDTO
    checkInTicket eventIdStr TicketCheckInRequestDTO{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      eventVal <- maybe (throwError err404 { errBody = "Event not found" }) pure mEvent
      _ <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal

      ticketLookup <- either throwError pure (validateTicketCheckInLookup TicketCheckInRequestDTO{..})
      mTicket <- liftIO $ runSqlPool (findTicketForCheckIn eventKey ticketLookup) envPool
      ticketEntity <- maybe (throwError err404 { errBody = "Ticket not found" }) pure mTicket

      let ticketKey = entityKey ticketEntity
          ticketVal = entityVal ticketEntity
      orderRef <- liftIO $ runSqlPool (get (eventTicketOrderRefId ticketVal)) envPool
      orderStatus <- either throwError pure
        (validateTicketCheckInOrderStatus (eventTicketOrderStatus <$> orderRef))
      when (orderStatus /= "paid") $ throwError err400 { errBody = "Only paid tickets can be checked in" }
      ticketStatus <- either throwError pure
        (validateTicketCheckInTicketStatus (eventTicketStatus ticketVal))
      case ticketStatus of
        "cancelled" -> throwError err400 { errBody = "Cancelled tickets cannot be checked in" }
        "refunded" -> throwError err400 { errBody = "Refunded tickets cannot be checked in" }
        "checked_in" -> pure (ticketEntityToDTO ticketEntity)
        _ -> do
          liftIO $ runSqlPool (update ticketKey
            [ EventTicketStatus =. "checked_in"
            , EventTicketCheckedInAt =. Just now
            , EventTicketUpdatedAt =. now
            ]) envPool
          mUpdated <- liftIO $ runSqlPool (getEntity ticketKey) envPool
          maybe (throwError err500 { errBody = "Could not check in ticket" })
                (pure . ticketEntityToDTO)
                mUpdated

    -- Budget
    budgetServer :: ServerT BudgetRoutes AppM
    budgetServer = listBudgetLines
              :<|> createBudgetLine
              :<|> updateBudgetLine

    listBudgetLines :: T.Text -> AppM [EventBudgetLineDTO]
    listBudgetLines eventIdStr = do
      Env{..} <- ask
      (eventKey, _) <- requireManagedEvent eventIdStr
      budgetRows <- liftIO $ runSqlPool
        (selectList [EventBudgetLineEventId ==. eventKey] [Asc EventBudgetLineLineType, Asc EventBudgetLineCategory, Asc EventBudgetLineCode])
        envPool
      postedEntries <- liftIO $ runSqlPool
        (selectList [EventFinanceEntryEventId ==. eventKey, EventFinanceEntryStatus ==. "posted"] [])
        envPool
      pure $
        map
          (\lineEnt@(Entity lineKey lineRec) ->
            let lineTypeVal = normalizeBudgetLineType (Just (eventBudgetLineLineType lineRec))
                actualCents = sum
                  [ eventFinanceEntryAmountCents entry
                  | Entity _ entry <- postedEntries
                  , eventFinanceEntryBudgetLineId entry == Just lineKey
                  , normalizeFinanceDirection (Just (eventFinanceEntryDirection entry)) == lineTypeVal
                  ]
            in budgetLineEntityToDTO eventKey (Just actualCents) lineEnt
          )
          budgetRows

    createBudgetLine :: T.Text -> EventBudgetLineDTO -> AppM EventBudgetLineDTO
    createBudgetLine eventIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      (eventKey, _) <- requireManagedEvent eventIdStr
      let lineName = T.strip (eblName dto)
      when (T.null lineName) $ throwError err400 { errBody = "budget line name is required" }
      when (eblPlannedCents dto < 0) $ throwError err400 { errBody = "planned cents must be >= 0" }
      lineTypeVal <- either throwError pure (validateBudgetLineTypeInput (eblType dto))
      let codeVal = normalizeBudgetLineCode (fromMaybe lineName (cleanMaybeText (Just (eblCode dto))))
          categoryVal = normalizeCategory (Just (eblCategory dto))
      mInserted <- liftIO $ runSqlPool (insertUnique EventBudgetLine
        { eventBudgetLineEventId = eventKey
        , eventBudgetLineCode = codeVal
        , eventBudgetLineName = lineName
        , eventBudgetLineLineType = lineTypeVal
        , eventBudgetLineCategory = categoryVal
        , eventBudgetLinePlannedCents = eblPlannedCents dto
        , eventBudgetLineNotes = cleanMaybeText (eblNotes dto)
        , eventBudgetLineCreatedAt = now
        , eventBudgetLineUpdatedAt = now
        }) envPool
      lineKey <- maybe (throwError err409 { errBody = "budget line code already exists for this event" }) pure mInserted
      mLine <- liftIO $ runSqlPool (getEntity lineKey) envPool
      maybe (throwError err500 { errBody = "Could not create budget line" })
            (pure . budgetLineEntityToDTO eventKey (Just 0))
            mLine

    updateBudgetLine :: T.Text -> T.Text -> EventBudgetLineDTO -> AppM EventBudgetLineDTO
    updateBudgetLine eventIdStr lineIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      (eventKey, _) <- requireManagedEvent eventIdStr
      lineKey <- parseKeyOr400 "budget line" lineIdStr
      mLine <- liftIO $ runSqlPool (get lineKey) envPool
      lineRec <- maybe (throwError err404 { errBody = "Budget line not found" }) pure mLine
      when (eventBudgetLineEventId lineRec /= eventKey) $ throwError err400 { errBody = "Budget line does not belong to this event" }
      let lineName = T.strip (eblName dto)
      when (T.null lineName) $ throwError err400 { errBody = "budget line name is required" }
      when (eblPlannedCents dto < 0) $ throwError err400 { errBody = "planned cents must be >= 0" }
      lineTypeVal <- either throwError pure (validateBudgetLineTypeInput (eblType dto))
      let codeVal = normalizeBudgetLineCode (fromMaybe lineName (cleanMaybeText (Just (eblCode dto))))
          categoryVal = normalizeCategory (Just (eblCategory dto))
      mCodeOwner <- liftIO $ runSqlPool (getBy (UniqueEventBudgetLineCode eventKey codeVal)) envPool
      case mCodeOwner of
        Just (Entity existingKey _) | existingKey /= lineKey ->
          throwError err409 { errBody = "budget line code already exists for this event" }
        _ -> pure ()
      liftIO $ runSqlPool (update lineKey
        [ EventBudgetLineCode =. codeVal
        , EventBudgetLineName =. lineName
        , EventBudgetLineLineType =. lineTypeVal
        , EventBudgetLineCategory =. categoryVal
        , EventBudgetLinePlannedCents =. eblPlannedCents dto
        , EventBudgetLineNotes =. cleanMaybeText (eblNotes dto)
        , EventBudgetLineUpdatedAt =. now
        ]) envPool
      postedEntries <- liftIO $ runSqlPool
        (selectList [ EventFinanceEntryEventId ==. eventKey
                    , EventFinanceEntryBudgetLineId ==. Just lineKey
                    , EventFinanceEntryStatus ==. "posted"
                    ] [])
        envPool
      mUpdated <- liftIO $ runSqlPool (getEntity lineKey) envPool
      let actualCents = sum
            [ eventFinanceEntryAmountCents entry
            | Entity _ entry <- postedEntries
            , normalizeFinanceDirection (Just (eventFinanceEntryDirection entry)) == lineTypeVal
            ]
      maybe (throwError err500 { errBody = "Could not update budget line" })
            (pure . budgetLineEntityToDTO eventKey (Just actualCents))
            mUpdated

    -- Finance
    financeServer :: ServerT FinanceRoutes AppM
    financeServer = listFinanceEntries
               :<|> createFinanceEntry
               :<|> updateFinanceEntry
               :<|> getFinanceSummary

    listFinanceEntries :: T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> AppM [EventFinanceEntryDTO]
    listFinanceEntries eventIdStr mDirection mSource mStatus = do
      Env{..} <- ask
      (eventKey, _) <- requireManagedEvent eventIdStr
      directionFilter <- normalizeFinanceDirectionFilter mDirection
      sourceFilter <- normalizeFinanceSourceFilter mSource
      statusFilter <- normalizeFinanceEntryStatusFilter mStatus
      manualRows <- liftIO $ runSqlPool
        (selectList [EventFinanceEntryEventId ==. eventKey] [Desc EventFinanceEntryOccurredAt, Desc EventFinanceEntryId])
        envPool
      ticketOrders <- liftIO $ runSqlPool
        (selectList [EventTicketOrderEventId ==. eventKey] [Desc EventTicketOrderPurchasedAt, Desc EventTicketOrderId])
        envPool
      manualDtos <- either (throwError . financeInvariantServerError) pure
        (traverse financeEntryEntityToDTOEither manualRows)
      let ticketDtos = concatMap (ticketOrderAccountingEntries eventKey) ticketOrders
          merged = manualDtos ++ ticketDtos
          filtered = filter (matchesFinanceFilters directionFilter sourceFilter statusFilter) merged
      pure (sortOn (Down . efeOccurredAt) filtered)

    createFinanceEntry :: T.Text -> EventFinanceEntryDTO -> AppM EventFinanceEntryDTO
    createFinanceEntry eventIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      (eventKey, eventRec) <- requireManagedEvent eventIdStr
      directionVal <- normalizeFinanceDirectionInput (efeDirection dto)
      sourceVal <- normalizeFinanceSourceInput (efeSource dto)
      statusVal <- normalizeFinanceEntryStatusInput (efeStatus dto)
      when (sourceVal `elem` ["ticket_sale", "ticket_refund"]) $
        throwError err400 { errBody = "ticket_sale and ticket_refund entries are generated from ticket orders" }
      let categoryVal = normalizeCategory (Just (efeCategory dto))
          conceptVal = T.strip (efeConcept dto)
          amountVal = efeAmountCents dto
          defaultCurrency = eventCurrencyFromEvent eventRec
      currencyVal <- either throwError pure
        (validateFinanceEntryCurrencyInput defaultCurrency (efeCurrency dto))
      when (T.null conceptVal) $ throwError err400 { errBody = "concept is required" }
      when (amountVal <= 0) $ throwError err400 { errBody = "amountCents must be greater than 0" }
      budgetLineKey <- resolveBudgetLineKey envPool eventKey (efeBudgetLineId dto)
      entryKey <- liftIO $ runSqlPool (insert EventFinanceEntry
        { eventFinanceEntryEventId = eventKey
        , eventFinanceEntryBudgetLineId = budgetLineKey
        , eventFinanceEntryDirection = directionVal
        , eventFinanceEntrySource = sourceVal
        , eventFinanceEntryCategory = categoryVal
        , eventFinanceEntryConcept = conceptVal
        , eventFinanceEntryAmountCents = amountVal
        , eventFinanceEntryCurrency = currencyVal
        , eventFinanceEntryStatus = statusVal
        , eventFinanceEntryExternalRef = cleanMaybeText (efeExternalRef dto)
        , eventFinanceEntryNotes = cleanMaybeText (efeNotes dto)
        , eventFinanceEntryMetadata = Nothing
        , eventFinanceEntryOccurredAt = efeOccurredAt dto
        , eventFinanceEntryRecordedByPartyId = Just currentPartyId
        , eventFinanceEntryCreatedAt = now
        , eventFinanceEntryUpdatedAt = now
        }) envPool
      mCreated <- liftIO $ runSqlPool (getEntity entryKey) envPool
      created <- maybe (throwError err500 { errBody = "Could not create finance entry" }) pure mCreated
      either (throwError . financeInvariantServerError) pure (financeEntryEntityToDTOEither created)

    updateFinanceEntry :: T.Text -> T.Text -> EventFinanceEntryDTO -> AppM EventFinanceEntryDTO
    updateFinanceEntry eventIdStr entryIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      (eventKey, eventRec) <- requireManagedEvent eventIdStr
      entryKey <- parseKeyOr400 "finance entry" entryIdStr
      mExisting <- liftIO $ runSqlPool (get entryKey) envPool
      existing <- maybe (throwError err404 { errBody = "Finance entry not found" }) pure mExisting
      when (eventFinanceEntryEventId existing /= eventKey) $ throwError err400 { errBody = "Finance entry does not belong to this event" }
      directionVal <- normalizeFinanceDirectionInput (efeDirection dto)
      sourceVal <- normalizeFinanceSourceInput (efeSource dto)
      statusVal <- normalizeFinanceEntryStatusInput (efeStatus dto)
      when (sourceVal `elem` ["ticket_sale", "ticket_refund"]) $
        throwError err400 { errBody = "ticket_sale and ticket_refund entries are generated from ticket orders" }
      let categoryVal = normalizeCategory (Just (efeCategory dto))
          conceptVal = T.strip (efeConcept dto)
          amountVal = efeAmountCents dto
          defaultCurrency = eventCurrencyFromEvent eventRec
      currencyVal <- either throwError pure
        (validateFinanceEntryCurrencyInput defaultCurrency (efeCurrency dto))
      when (T.null conceptVal) $ throwError err400 { errBody = "concept is required" }
      when (amountVal <= 0) $ throwError err400 { errBody = "amountCents must be greater than 0" }
      budgetLineKey <- resolveBudgetLineKey envPool eventKey (efeBudgetLineId dto)
      liftIO $ runSqlPool (update entryKey
        [ EventFinanceEntryBudgetLineId =. budgetLineKey
        , EventFinanceEntryDirection =. directionVal
        , EventFinanceEntrySource =. sourceVal
        , EventFinanceEntryCategory =. categoryVal
        , EventFinanceEntryConcept =. conceptVal
        , EventFinanceEntryAmountCents =. amountVal
        , EventFinanceEntryCurrency =. currencyVal
        , EventFinanceEntryStatus =. statusVal
        , EventFinanceEntryExternalRef =. cleanMaybeText (efeExternalRef dto)
        , EventFinanceEntryNotes =. cleanMaybeText (efeNotes dto)
        , EventFinanceEntryOccurredAt =. efeOccurredAt dto
        , EventFinanceEntryRecordedByPartyId =. Just currentPartyId
        , EventFinanceEntryUpdatedAt =. now
        ]) envPool
      mUpdated <- liftIO $ runSqlPool (getEntity entryKey) envPool
      updated <- maybe (throwError err500 { errBody = "Could not update finance entry" }) pure mUpdated
      either (throwError . financeInvariantServerError) pure (financeEntryEntityToDTOEither updated)

    getFinanceSummary :: T.Text -> AppM EventFinanceSummaryDTO
    getFinanceSummary eventIdStr = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      (eventKey, eventRec) <- requireManagedEvent eventIdStr
      let eventCurrencyVal = eventCurrencyFromEvent eventRec
          budgetOverride = eventBudgetFromEvent eventRec
      budgetRows <- liftIO $ runSqlPool (selectList [EventBudgetLineEventId ==. eventKey] []) envPool
      allFinanceRows <- liftIO $ runSqlPool
        (selectList [EventFinanceEntryEventId ==. eventKey] [])
        envPool
      ticketOrders <- liftIO $ runSqlPool (selectList [EventTicketOrderEventId ==. eventKey] []) envPool
      normalizedFinanceRows <- either (throwError . financeInvariantServerError) pure
        (traverse storedFinanceEntrySummaryFields allFinanceRows)

      let plannedIncomeCents =
            sum
              [ eventBudgetLinePlannedCents lineRec
              | Entity _ lineRec <- budgetRows
              , normalizeBudgetLineType (Just (eventBudgetLineLineType lineRec)) == "income"
              ]
          plannedExpenseCents =
            sum
              [ eventBudgetLinePlannedCents lineRec
              | Entity _ lineRec <- budgetRows
              , normalizeBudgetLineType (Just (eventBudgetLineLineType lineRec)) == "expense"
              ]
          entryAmount (amountCents, _, _, _) = amountCents
          entryDirection (_, directionVal, _, _) = directionVal
          entrySource (_, _, sourceVal, _) = sourceVal
          entryStatus (_, _, _, statusVal) = statusVal
          isPosted entry = entryStatus entry == "posted"
          isPendingLike entry =
            let statusVal = entryStatus entry
            in statusVal == "pending" || statusVal == "draft"
          isNonVoid entry = entryStatus entry /= "void"
          manualIncomeCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isPosted entry
              , entryDirection entry == "income"
              ]
          manualExpenseCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isPosted entry
              , entryDirection entry == "expense"
              ]
          ticketPaidRevenueCents =
            sum
              [ eventTicketOrderAmountCents orderRec
              | Entity _ orderRec <- ticketOrders
              , normalizeTicketOrderStatus (Just (eventTicketOrderStatus orderRec)) == "paid"
              ]
          ticketRefundedRevenueCents =
            sum
              [ eventTicketOrderAmountCents orderRec
              | Entity _ orderRec <- ticketOrders
              , normalizeTicketOrderStatus (Just (eventTicketOrderStatus orderRec)) == "refunded"
              ]
          ticketPendingRevenueCents =
            sum
              [ eventTicketOrderAmountCents orderRec
              | Entity _ orderRec <- ticketOrders
              , normalizeTicketOrderStatus (Just (eventTicketOrderStatus orderRec)) == "pending"
              ]
          accountsPayableCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isPendingLike entry
              , entryDirection entry == "expense"
              ]
          accountsReceivableManualCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isPendingLike entry
              , entryDirection entry == "income"
              ]
          accountsReceivableCents = accountsReceivableManualCents + ticketPendingRevenueCents
          contractCommittedCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isNonVoid entry
              , entrySource entry == "contract_commitment"
              ]
          contractPaidCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isPosted entry
              , entrySource entry == "contract_payment"
              ]
          procurementCommittedCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isNonVoid entry
              , entrySource entry == "purchase_order"
              ]
          procurementPaidCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isPosted entry
              , entrySource entry == "purchase_payment"
              ]
          assetInvestmentCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isPosted entry
              , entrySource entry == "asset_purchase"
              ]
          liabilityIncurredCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isPosted entry
              , entrySource entry == "liability_loan"
              , entryDirection entry == "income"
              ]
          liabilityPaidCents =
            sum
              [ entryAmount entry
              | entry <- normalizedFinanceRows
              , isPosted entry
              , entrySource entry == "liability_payment"
              , entryDirection entry == "expense"
              ]
          liabilityBalanceCents = liabilityIncurredCents - liabilityPaidCents
          actualIncomeCents = manualIncomeCents + ticketPaidRevenueCents
          actualExpenseCents = manualExpenseCents + ticketRefundedRevenueCents
          netCents = actualIncomeCents - actualExpenseCents
          budgetCentsVal = budgetOverride <|> fallbackBudget plannedExpenseCents
          budgetVarianceCents = fmap (\budgetCap -> budgetCap - actualExpenseCents) budgetCentsVal
          budgetUtilizationPct =
            case budgetCentsVal of
              Just budgetCap | budgetCap > 0 ->
                Just ((fromIntegral actualExpenseCents / fromIntegral budgetCap) * 100)
              _ -> Nothing

      pure EventFinanceSummaryDTO
        { efsEventId = renderKeyText eventKey
        , efsCurrency = eventCurrencyVal
        , efsBudgetCents = budgetCentsVal
        , efsPlannedIncomeCents = plannedIncomeCents
        , efsPlannedExpenseCents = plannedExpenseCents
        , efsActualIncomeCents = actualIncomeCents
        , efsActualExpenseCents = actualExpenseCents
        , efsNetCents = netCents
        , efsTicketPaidRevenueCents = ticketPaidRevenueCents
        , efsTicketRefundedRevenueCents = ticketRefundedRevenueCents
        , efsTicketPendingRevenueCents = ticketPendingRevenueCents
        , efsAccountsPayableCents = accountsPayableCents
        , efsAccountsReceivableCents = accountsReceivableCents
        , efsContractCommittedCents = contractCommittedCents
        , efsContractPaidCents = contractPaidCents
        , efsProcurementCommittedCents = procurementCommittedCents
        , efsProcurementPaidCents = procurementPaidCents
        , efsAssetInvestmentCents = assetInvestmentCents
        , efsLiabilityBalanceCents = liabilityBalanceCents
        , efsBudgetVarianceCents = budgetVarianceCents
        , efsBudgetUtilizationPct = budgetUtilizationPct
        , efsGeneratedAt = now
        }

    requireManagedEvent :: T.Text -> AppM (SocialEventId, SocialEvent)
    requireManagedEvent rawEventId = do
      Env{..} <- ask
      eventKey <- parseKeyOr400 "event" rawEventId
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      eventVal <- maybe (throwError err404 { errBody = "Event not found" }) pure mEvent
      claimed <- claimOrRequireEventManager currentPartyId envPool eventKey eventVal
      pure (eventKey, claimed)

    requireExistingEvent :: ConnectionPool -> SocialEventId -> AppM SocialEvent
    requireExistingEvent pool eventKey = do
      mEvent <- liftIO $ runSqlPool (get eventKey) pool
      maybe (throwError err404 { errBody = "Event not found" }) pure mEvent

    requireMomentForEvent :: ConnectionPool -> SocialEventId -> EventMomentId -> AppM EventMoment
    requireMomentForEvent pool eventKey momentKey = do
      mMoment <- liftIO $ runSqlPool (get momentKey) pool
      momentRow <- maybe (throwError err404 { errBody = "Moment not found" }) pure mMoment
      when (eventMomentEventId momentRow /= eventKey) $
        throwError err400 { errBody = "Moment does not belong to this event" }
      pure momentRow

    parseIds :: T.Text -> T.Text -> AppM (SocialEventId, EventInvitationId)
    parseIds eventIdStr invitationIdStr =
      case parseInvitationIdsEither eventIdStr invitationIdStr of
        Right ids -> pure ids
        Left e -> throwError e

    parseArtistId :: T.Text -> AppM ArtistProfileId
    parseArtistId = parseKeyOr400 "artist"

-- | Stable, human-friendly identifier for a follow (artistId + follower id).
renderFollowId :: ArtistProfileId -> T.Text -> T.Text
renderFollowId artistId followerPartyId =
  T.intercalate ":" [renderKeyText artistId, followerPartyId]

-- | Insert or fetch an artist follow while keeping the created timestamp stable.
followArtistDb :: ConnectionPool -> ArtistProfileId -> T.Text -> IO ArtistFollowerDTO
followArtistDb pool artistId followerPartyIdRaw = do
  now <- getCurrentTime
  let followerPartyId = fromMaybe (T.strip followerPartyIdRaw) (normalizePositivePartyIdText followerPartyIdRaw)
  let followKey = ArtistFollowKey artistId followerPartyId
  existing <- runSqlPool (get followKey) pool
  _ <- case existing of
    Just _ -> pure followKey
    Nothing -> do
      mInserted <- runSqlPool (insertUnique (ArtistFollow artistId followerPartyId now)) pool
      pure (fromMaybe followKey mInserted)
  let createdAtVal = maybe now artistFollowCreatedAt existing
  pure ArtistFollowerDTO
    { afFollowId = Just (renderFollowId artistId followerPartyId)
    , afArtistId = Just (renderKeyText artistId)
    , afFollowerPartyId = followerPartyId
    , afCreatedAt = Just createdAtVal
    }

normalizePositivePartyIdText :: T.Text -> Maybe T.Text
normalizePositivePartyIdText rawPartyId =
  normalizePositiveIdentifierText rawPartyId

resolveExistingPartyIdText :: ConnectionPool -> T.Text -> T.Text -> IO (Either ServerError T.Text)
resolveExistingPartyIdText pool fieldName rawPartyId =
  case normalizePositivePartyIdText rawPartyId of
    Nothing ->
      pure (Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a positive integer")) })
    Just normalized ->
      case readMaybe (T.unpack normalized) :: Maybe Int64 of
        Nothing ->
          pure (Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a positive integer")) })
        Just partyIdValue -> do
          mParty <- runSqlPool (get (toSqlKey partyIdValue :: Key Party)) pool
          pure $
            case mParty of
              Just _ -> Right normalized
              Nothing ->
                Left err422
                  { errBody =
                      BL.fromStrict
                        (TE.encodeUtf8 (fieldName <> " references an unknown party"))
                  }

normalizePositiveIdentifierText :: T.Text -> Maybe T.Text
normalizePositiveIdentifierText rawIdentifier =
  case readMaybe (T.unpack (T.strip rawIdentifier)) :: Maybe Int64 of
    Just identifier | identifier > 0 -> Just (T.pack (show identifier))
    _ -> Nothing

parseFollowerQueryParamEither :: Maybe T.Text -> Either ServerError T.Text
parseFollowerQueryParamEither mFollower =
  case cleanMaybeText mFollower of
    Nothing -> Left err400 { errBody = "follower query param is required" }
    Just rawFollower ->
      case normalizePositivePartyIdText rawFollower of
        Nothing -> Left err400 { errBody = "follower query param must be a positive integer" }
        Just normalized -> Right normalized

validateInvitationToPartyId :: T.Text -> Either ServerError T.Text
validateInvitationToPartyId rawInvitationPartyId =
  case cleanMaybeText (Just rawInvitationPartyId) of
    Nothing -> Left err400 { errBody = "invitationToPartyId is required" }
    Just trimmed ->
      case normalizePositivePartyIdText trimmed of
        Nothing -> Left err400 { errBody = "invitationToPartyId must be a positive integer" }
        Just normalized -> Right normalized

validateRsvpStatus :: T.Text -> Either ServerError T.Text
validateRsvpStatus raw =
  case T.toLower (T.strip raw) of
    "accepted" -> Right "accepted"
    "declined" -> Right "declined"
    "maybe" -> Right "maybe"
    _ -> Left err400 { errBody = "rsvpStatus must be one of: accepted, declined, maybe" }

parseInvitationStatus :: T.Text -> Maybe T.Text
parseInvitationStatus raw =
  case T.toLower (T.strip raw) of
    "pending" -> Just "pending"
    "accepted" -> Just "accepted"
    "declined" -> Just "declined"
    _ -> Nothing

validateInvitationStatusInput :: Maybe T.Text -> Either ServerError T.Text
validateInvitationStatusInput Nothing = Right "pending"
validateInvitationStatusInput (Just rawStatus) =
  case T.strip rawStatus of
    "" -> Right "pending"
    _ ->
      case parseInvitationStatus rawStatus of
        Just statusVal -> Right statusVal
        Nothing ->
          Left err400
            { errBody = "invitationStatus must be one of: pending, accepted, declined"
            }

validateInvitationStatusUpdateInput :: Maybe T.Text -> Either ServerError (Maybe T.Text)
validateInvitationStatusUpdateInput Nothing = Right Nothing
validateInvitationStatusUpdateInput (Just rawStatus) =
  case cleanMaybeText (Just rawStatus) of
    Nothing ->
      Left err400
        { errBody = "invitationStatus must be one of: pending, accepted, declined"
        }
    Just _ ->
      Just <$> validateInvitationStatusInput (Just rawStatus)

validateEventArtistIds :: [ArtistDTO] -> Either ServerError [ArtistProfileId]
validateEventArtistIds artists = do
  artistKeys <- fmap catMaybes (traverse validateArtistId artists)
  if Set.size (Set.fromList artistKeys) == length artistKeys
    then Right artistKeys
    else Left err400 { errBody = "eventArtists[].artistId must be unique" }
  where
    validateArtistId artist =
      case artistId artist of
        Nothing -> Right Nothing
        Just rawArtistId ->
          case normalizePositiveIdentifierText rawArtistId of
            Nothing -> Left err400 { errBody = "eventArtists[].artistId must be a positive integer" }
            Just normalizedArtistId ->
              case readMaybe (T.unpack normalizedArtistId) :: Maybe Int64 of
                Just artistIdValue -> Right (Just (toSqlKey artistIdValue))
                Nothing -> Left err400 { errBody = "eventArtists[].artistId must be a positive integer" }

-- | Normalize invitation status to a lowercase, non-empty value.
normalizeInvitationStatus :: Maybe T.Text -> T.Text
normalizeInvitationStatus mStatus =
  case mStatus >>= parseInvitationStatus of
    Nothing -> "pending"
    Just s -> s

normalizeTicketOrderStatus :: Maybe T.Text -> T.Text
normalizeTicketOrderStatus mStatus =
  case mStatus >>= parseTicketOrderStatus of
    Nothing -> "pending"
    Just s -> s

normalizeTicketStatus :: Maybe T.Text -> T.Text
normalizeTicketStatus mStatus =
  case mStatus >>= parseTicketStatus of
    Nothing -> "issued"
    Just s -> s

validateTicketCheckInOrderStatus :: Maybe T.Text -> Either ServerError T.Text
validateTicketCheckInOrderStatus Nothing =
  Left err500 { errBody = "Ticket order could not be loaded" }
validateTicketCheckInOrderStatus (Just rawStatus) =
  case parseTicketOrderStatus rawStatus of
    Just statusVal -> Right statusVal
    Nothing -> Left err500 { errBody = "Stored ticket order status is invalid" }

validateTicketCheckInTicketStatus :: T.Text -> Either ServerError T.Text
validateTicketCheckInTicketStatus rawStatus =
  case parseTicketStatus rawStatus of
    Just statusVal -> Right statusVal
    Nothing -> Left err500 { errBody = "Stored ticket status is invalid" }

findTicketForCheckIn :: SocialEventId -> TicketCheckInLookup -> SqlPersistT IO (Maybe (Entity EventTicket))
findTicketForCheckIn eventKey ticketLookup =
  case ticketLookup of
    TicketCheckInLookupById rawTicketId -> do
      let ticketKey = toSqlKey (read (T.unpack rawTicketId) :: Int64)
      selectFirst [EventTicketId ==. ticketKey, EventTicketEventId ==. eventKey] []
    TicketCheckInLookupByCode codeVal ->
      selectFirst [EventTicketEventId ==. eventKey, EventTicketCode ==. codeVal] []

validateTicketCheckInLookup :: TicketCheckInRequestDTO -> Either ServerError TicketCheckInLookup
validateTicketCheckInLookup TicketCheckInRequestDTO{..} =
  case (cleanMaybeText ticketCheckInTicketId, cleanMaybeText ticketCheckInTicketCode) of
    (Just rawTicketId, Nothing) ->
      case normalizePositiveIdentifierText rawTicketId of
        Just normalizedTicketId -> Right (TicketCheckInLookupById normalizedTicketId)
        Nothing ->
          Left err400 { errBody = "ticketCheckInTicketId must be a positive integer" }
    (Nothing, Just rawCode) ->
      case normalizeTicketCheckInCode rawCode of
        Just codeVal -> Right (TicketCheckInLookupByCode codeVal)
        Nothing ->
          Left err400 { errBody = "ticketCheckInTicketCode must be a generated ticket code" }
    (Just _, Just _) ->
      Left err400 { errBody = "Provide exactly one of ticketCheckInTicketId or ticketCheckInTicketCode" }
    (Nothing, Nothing) ->
      Left err400 { errBody = "Provide ticketCheckInTicketId or ticketCheckInTicketCode" }

validateOptionalTicketBuyerPartyId :: T.Text -> Maybe T.Text -> Either ServerError (Maybe T.Text)
validateOptionalTicketBuyerPartyId fieldName mPartyId =
  case cleanMaybeText mPartyId of
    Nothing -> Right Nothing
    Just rawPartyId ->
      case normalizePositivePartyIdText rawPartyId of
        Nothing ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 (fieldName <> " must be a positive integer"))
            }
        Just normalized -> Right (Just normalized)

validateTicketPurchaseBuyerEmail :: Maybe T.Text -> Either ServerError (Maybe T.Text)
validateTicketPurchaseBuyerEmail rawEmail =
  case cleanMaybeText rawEmail of
    Nothing -> Right Nothing
    Just email
      | T.length normalized > 254 ->
          Left err400
            { errBody = "ticketPurchaseBuyerEmail must be 254 characters or fewer" }
      | isValidSocialEventEmail normalized ->
          Right (Just normalized)
      | otherwise ->
          Left err400
            { errBody = "ticketPurchaseBuyerEmail must be a valid email address" }
      where
        normalized = T.toLower email

isValidSocialEventEmail :: T.Text -> Bool
isValidSocialEventEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      T.length candidate <= 254
        && isValidSocialEventEmailLocalPart localPart
        && not (T.null domain)
        && not (T.any (`elem` (" \t\n\r" :: String)) candidate)
        && T.isInfixOf "." domain
        && all isValidSocialEventEmailDomainLabel (T.splitOn "." domain)
    _ -> False

isValidSocialEventEmailLocalPart :: T.Text -> Bool
isValidSocialEventEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= 64
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (".." `T.isInfixOf` localPart)
    && T.all isValidSocialEventEmailLocalChar localPart

isValidSocialEventEmailLocalChar :: Char -> Bool
isValidSocialEventEmailLocalChar c =
  isAscii c
    && (isAlphaNum c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String))

isValidSocialEventEmailDomainLabel :: T.Text -> Bool
isValidSocialEventEmailDomainLabel label =
  not (T.null label)
    && T.length label <= 63
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidSocialEventEmailDomainChar label

isValidSocialEventEmailDomainChar :: Char -> Bool
isValidSocialEventEmailDomainChar c =
  isAscii c && (isAlphaNum c || c == '-')

normalizeTicketCheckInCode :: T.Text -> Maybe T.Text
normalizeTicketCheckInCode rawCode = do
  suffix <- T.stripPrefix "TDF-" normalized
  if T.length suffix == 12 && T.all isAsciiHexDigit suffix
     then Just normalized
     else Nothing
  where
    normalized = T.toUpper (T.strip rawCode)
    isAsciiHexDigit ch = isAscii ch && isHexDigit ch

normalizeEventType :: Maybe T.Text -> Maybe T.Text
normalizeEventType mType =
  case fmap (T.toLower . T.strip) mType of
    Nothing -> Nothing
    Just "" -> Nothing
    Just "party" -> Just "party"
    Just "concert" -> Just "concert"
    Just "festival" -> Just "festival"
    Just "conference" -> Just "conference"
    Just "showcase" -> Just "showcase"
    Just "other" -> Just "other"
    Just _ -> Nothing

normalizeEventStatus :: Maybe T.Text -> Maybe T.Text
normalizeEventStatus mStatus =
  case fmap (T.toLower . T.strip) mStatus of
    Nothing -> Nothing
    Just "" -> Nothing
    Just "planning" -> Just "planning"
    Just "announced" -> Just "announced"
    Just "on_sale" -> Just "on_sale"
    Just "live" -> Just "live"
    Just "completed" -> Just "completed"
    Just "cancelled" -> Just "cancelled"
    Just "canceled" -> Just "cancelled"
    Just _ -> Nothing

normalizeMomentMediaType :: T.Text -> Maybe T.Text
normalizeMomentMediaType raw =
  case T.toLower (T.strip raw) of
    "image" -> Just "image"
    "photo" -> Just "image"
    "picture" -> Just "image"
    "video" -> Just "video"
    "clip" -> Just "video"
    _ -> Nothing

normalizeMomentReaction :: T.Text -> Maybe T.Text
normalizeMomentReaction raw =
  case T.toLower (T.strip raw) of
    "fire" -> Just "fire"
    "love" -> Just "love"
    "heart" -> Just "love"
    "applause" -> Just "applause"
    "clap" -> Just "applause"
    _ -> Nothing

normalizeMomentCaption :: Maybe T.Text -> Either ServerError (Maybe T.Text)
normalizeMomentCaption mCaption =
  case cleanMaybeText mCaption of
    Nothing -> Right Nothing
    Just captionVal
      | T.length captionVal > 280 ->
          Left err400 { errBody = "Moment caption must be 280 characters or less" }
      | otherwise -> Right (Just captionVal)

normalizeMomentCommentBody :: T.Text -> Either ServerError T.Text
normalizeMomentCommentBody rawBody =
  case nonEmptyText rawBody of
    Nothing ->
      Left err400 { errBody = "Moment comment body is required" }
    Just bodyVal
      | T.length bodyVal > 500 ->
          Left err400 { errBody = "Moment comment body must be 500 characters or less" }
      | otherwise -> Right bodyVal

normalizePositiveIntMaybe :: Maybe Int -> Maybe Int
normalizePositiveIntMaybe mValue =
  case mValue of
    Just value | value > 0 -> Just value
    _ -> Nothing

normalizeNonNegativeIntMaybe :: Maybe Int -> Maybe Int
normalizeNonNegativeIntMaybe mValue =
  case mValue of
    Just value | value >= 0 -> Just value
    _ -> Nothing

resolveMomentAuthorName :: T.Text -> Maybe T.Text -> T.Text
resolveMomentAuthorName currentParty mAuthorName =
  fromMaybe ("Party " <> currentParty) (cleanMaybeText mAuthorName)

normalizeBudgetCentsMaybe :: Maybe Int -> Maybe Int
normalizeBudgetCentsMaybe mBudget =
  case mBudget of
    Just n | n >= 0 -> Just n
    _ -> Nothing

validateEventCreateUpdateDimensions :: Maybe Int -> Maybe Int -> Maybe Int -> Either ServerError ()
validateEventCreateUpdateDimensions mPriceCents mCapacity mBudgetCents
  | maybe False (< 0) mPriceCents =
      Left err400 { errBody = "event price must be >= 0" }
  | maybe False (< 0) mCapacity =
      Left err400 { errBody = "event capacity must be >= 0" }
  | maybe False (< 0) mBudgetCents =
      Left err400 { errBody = "event budget must be >= 0" }
  | otherwise =
      Right ()

validateVenueCreateUpdateFields
  :: T.Text
  -> Maybe Double
  -> Maybe Double
  -> Maybe Int
  -> Either ServerError ()
validateVenueCreateUpdateFields rawName mLat mLng mCapacity
  | T.null (T.strip rawName) =
      Left err400 { errBody = "venue name is required" }
  | T.any isControl rawName =
      Left err400 { errBody = "venue name must not contain control characters" }
  | maybe False (< 0) mCapacity =
      Left err400 { errBody = "venue capacity must be >= 0" }
  | otherwise =
      validateVenueCoordinatePair mLat mLng

validateVenueCoordinatePair :: Maybe Double -> Maybe Double -> Either ServerError ()
validateVenueCoordinatePair Nothing Nothing = Right ()
validateVenueCoordinatePair (Just lat) (Just lng)
  | isNaN lat || isInfinite lat =
      Left err400 { errBody = "Invalid venue latitude" }
  | isNaN lng || isInfinite lng =
      Left err400 { errBody = "Invalid venue longitude" }
  | lat < (-90) || lat > 90 =
      Left err400 { errBody = "venue latitude must be between -90 and 90" }
  | lng < (-180) || lng > 180 =
      Left err400 { errBody = "venue longitude must be between -180 and 180" }
  | otherwise =
      Right ()
validateVenueCoordinatePair _ _ =
  Left err400 { errBody = "venue latitude and longitude must be provided together" }

validateEventCurrencyInput :: Maybe T.Text -> Either ServerError T.Text
validateEventCurrencyInput mCurrency =
  case cleanMaybeText mCurrency of
    Nothing -> Right "USD"
    Just rawCurrency ->
      case normalizeEventCurrencyCode rawCurrency of
        Just currency -> Right currency
        Nothing -> Left err400 { errBody = "eventCurrency must be a 3-letter ISO code" }

normalizeEventCurrencyMaybe :: Maybe T.Text -> Maybe T.Text
normalizeEventCurrencyMaybe = (>>= normalizeEventCurrencyCode) . cleanMaybeText

normalizeEventCurrencyCode :: T.Text -> Maybe T.Text
normalizeEventCurrencyCode rawCurrency =
  let normalized = T.toUpper (T.strip rawCurrency)
  in if T.length normalized == 3 && T.all isAsciiUpper normalized
       then Just normalized
       else Nothing

normalizeCurrencyMaybe :: Maybe T.Text -> Maybe T.Text
normalizeCurrencyMaybe mCurrency = normalizeCurrency <$> cleanMaybeText mCurrency

validateFinanceEntryCurrencyInput :: T.Text -> T.Text -> Either ServerError T.Text
validateFinanceEntryCurrencyInput defaultCurrency rawCurrency =
  case cleanMaybeText (Just rawCurrency) of
    Nothing ->
      case normalizeEventCurrencyCode defaultCurrency of
        Just fallbackCurrency -> Right fallbackCurrency
        Nothing ->
          Left err409
            { errBody =
                "event default currency must be a 3-letter ISO code before finance entries can inherit it"
            }
    Just providedCurrency ->
      case normalizeEventCurrencyCode providedCurrency of
        Just currency -> Right currency
        Nothing ->
          Left err400 { errBody = "finance entry currency must be a 3-letter ISO code" }

validateTicketTierCurrencyInput :: T.Text -> T.Text -> Either ServerError T.Text
validateTicketTierCurrencyInput defaultCurrency rawCurrency =
  case cleanMaybeText (Just rawCurrency) of
    Nothing ->
      case normalizeEventCurrencyCode defaultCurrency of
        Just fallbackCurrency -> Right fallbackCurrency
        Nothing ->
          Left err409
            { errBody =
                "event default currency must be a 3-letter ISO code before ticket tiers can inherit it"
            }
    Just providedCurrency ->
      case normalizeEventCurrencyCode providedCurrency of
        Just currency -> Right currency
        Nothing ->
          Left err400 { errBody = "ticket tier currency must be a 3-letter ISO code" }

normalizeBudgetLineType :: Maybe T.Text -> T.Text
normalizeBudgetLineType mType =
  case mType >>= parseBudgetLineType of
    Just lineTypeVal -> lineTypeVal
    Nothing -> "expense"

parseBudgetLineType :: T.Text -> Maybe T.Text
parseBudgetLineType raw =
  case T.toLower (T.strip raw) of
    "income" -> Just "income"
    "expense" -> Just "expense"
    _ -> Nothing

validateBudgetLineTypeInput :: T.Text -> Either ServerError T.Text
validateBudgetLineTypeInput raw =
  case parseBudgetLineType raw of
    Just lineTypeVal -> Right lineTypeVal
    Nothing -> Left err400 { errBody = "budget line type must be income or expense" }

normalizeBudgetLineCode :: T.Text -> T.Text
normalizeBudgetLineCode raw =
  let upper = T.toUpper (T.strip raw)
      withDash = T.map (\c -> if c == ' ' then '-' else c) upper
      cleaned = T.filter (\c -> isAlphaNum c || c == '-' || c == '_') withDash
      chunks = filter (not . T.null) (T.splitOn "-" cleaned)
      normalized = T.intercalate "-" chunks
  in if T.null normalized then "LINE" else normalized

normalizeCategory :: Maybe T.Text -> T.Text
normalizeCategory mCategory =
  case fmap (T.toLower . T.strip) mCategory of
    Nothing -> "general"
    Just "" -> "general"
    Just v -> v

parseFinanceDirection :: T.Text -> Maybe T.Text
parseFinanceDirection raw =
  case T.toLower (T.strip raw) of
    "income" -> Just "income"
    "expense" -> Just "expense"
    _ -> Nothing

normalizeFinanceDirection :: Maybe T.Text -> T.Text
normalizeFinanceDirection mDirection =
  case mDirection >>= parseFinanceDirection of
    Just directionVal -> directionVal
    Nothing -> "expense"

normalizeFinanceSource :: Maybe T.Text -> T.Text
normalizeFinanceSource mSource =
  case mSource >>= parseFinanceSource of
    Just src -> src
    Nothing -> "manual"

parseFinanceSource :: T.Text -> Maybe T.Text
parseFinanceSource raw =
  case T.toLower (T.strip raw) of
    "ticket_sale" -> Just "ticket_sale"
    "ticket_refund" -> Just "ticket_refund"
    "sponsorship" -> Just "sponsorship"
    "vendor_payment" -> Just "vendor_payment"
    "merchandise" -> Just "merchandise"
    "operations" -> Just "operations"
    "manual" -> Just "manual"
    "other" -> Just "other"
    "contract_commitment" -> Just "contract_commitment"
    "contract_payment" -> Just "contract_payment"
    "purchase_order" -> Just "purchase_order"
    "purchase_payment" -> Just "purchase_payment"
    "asset_purchase" -> Just "asset_purchase"
    "liability_loan" -> Just "liability_loan"
    "liability_payment" -> Just "liability_payment"
    "accounts_receivable" -> Just "accounts_receivable"
    "accounts_receivable_collection" -> Just "accounts_receivable_collection"
    "accounts_receivable_settlement" -> Just "accounts_receivable_collection"
    _ -> Nothing

parseFinanceEntryStatus :: T.Text -> Maybe T.Text
parseFinanceEntryStatus raw =
  case T.toLower (T.strip raw) of
    "draft" -> Just "draft"
    "posted" -> Just "posted"
    "void" -> Just "void"
    "pending" -> Just "pending"
    _ -> Nothing

normalizeFinanceEntryStatus :: Maybe T.Text -> T.Text
normalizeFinanceEntryStatus mStatus =
  case mStatus >>= parseFinanceEntryStatus of
    Just statusVal -> statusVal
    Nothing -> "posted"

validateStoredFinanceEntryDimensions :: EventFinanceEntry -> Either T.Text (T.Text, T.Text, T.Text)
validateStoredFinanceEntryDimensions entry = do
  directionVal <- maybe (Left "Stored finance entry direction is invalid") Right
    (parseFinanceDirection (eventFinanceEntryDirection entry))
  sourceVal <- maybe (Left "Stored finance entry source is invalid") Right
    (parseFinanceSource (eventFinanceEntrySource entry))
  statusVal <- maybe (Left "Stored finance entry status is invalid") Right
    (parseFinanceEntryStatus (eventFinanceEntryStatus entry))
  _currencyVal <- maybe (Left "Stored finance entry currency is invalid") Right
    (normalizeEventCurrencyCode (eventFinanceEntryCurrency entry))
  pure (directionVal, sourceVal, statusVal)

normalizeFinanceDirectionInput :: T.Text -> AppM T.Text
normalizeFinanceDirectionInput raw =
  case parseFinanceDirection raw of
    Just directionVal -> pure directionVal
    Nothing -> throwError err400 { errBody = "direction must be income or expense" }

normalizeFinanceSourceInput :: T.Text -> AppM T.Text
normalizeFinanceSourceInput raw =
  case parseFinanceSource raw of
    Just sourceVal -> pure sourceVal
    Nothing -> throwError err400 { errBody = "Invalid finance source" }

normalizeFinanceEntryStatusInput :: T.Text -> AppM T.Text
normalizeFinanceEntryStatusInput raw =
  case parseFinanceEntryStatus raw of
    Just statusVal -> pure statusVal
    Nothing -> throwError err400 { errBody = "Invalid finance status" }

normalizeFinanceDirectionFilter :: Maybe T.Text -> AppM (Maybe T.Text)
normalizeFinanceDirectionFilter Nothing = pure Nothing
normalizeFinanceDirectionFilter (Just raw) =
  case T.toLower (T.strip raw) of
    "" -> pure Nothing
    _ ->
      case parseFinanceDirection raw of
        Just directionVal -> pure (Just directionVal)
        Nothing -> throwError err400 { errBody = "Invalid direction filter" }

normalizeFinanceSourceFilter :: Maybe T.Text -> AppM (Maybe T.Text)
normalizeFinanceSourceFilter Nothing = pure Nothing
normalizeFinanceSourceFilter (Just raw) =
  case T.strip raw of
    "" -> pure Nothing
    nonEmpty ->
      case parseFinanceSource nonEmpty of
        Just sourceVal -> pure (Just sourceVal)
        Nothing -> throwError err400 { errBody = "Invalid source filter" }

normalizeFinanceEntryStatusFilter :: Maybe T.Text -> AppM (Maybe T.Text)
normalizeFinanceEntryStatusFilter Nothing = pure Nothing
normalizeFinanceEntryStatusFilter (Just raw) =
  case T.toLower (T.strip raw) of
    "" -> pure Nothing
    _ ->
      case parseFinanceEntryStatus raw of
        Just statusVal -> pure (Just statusVal)
        Nothing -> throwError err400 { errBody = "Invalid status filter" }

parseTicketOrderStatus :: T.Text -> Maybe T.Text
parseTicketOrderStatus raw =
  case T.toLower (T.strip raw) of
    "pending" -> Just "pending"
    "paid" -> Just "paid"
    "cancelled" -> Just "cancelled"
    "canceled" -> Just "cancelled"
    "refunded" -> Just "refunded"
    _ -> Nothing

parseTicketStatus :: T.Text -> Maybe T.Text
parseTicketStatus raw =
  case T.toLower (T.strip raw) of
    "issued" -> Just "issued"
    "checked_in" -> Just "checked_in"
    "checkedin" -> Just "checked_in"
    "cancelled" -> Just "cancelled"
    "canceled" -> Just "cancelled"
    "refunded" -> Just "refunded"
    _ -> Nothing

-- | Parse event and invitation ids, returning a typed pair or an HTTP 400 error.
parseInvitationIdsEither :: T.Text -> T.Text -> Either ServerError (SocialEventId, EventInvitationId)
parseInvitationIdsEither eventIdStr invitationIdStr =
  case (parseInt64Either "event" eventIdStr, parseInt64Either "invitation" invitationIdStr) of
    (Right eventNum, Right invitationNum) -> Right (toSqlKey eventNum, toSqlKey invitationNum)
    _ -> Left err400 { errBody = "Invalid event or invitation id" }

parseVenueIdEither :: T.Text -> Either ServerError VenueId
parseVenueIdEither =
  fmap toSqlKey . parseInt64Either "venue"

parseInt64Either :: T.Text -> T.Text -> Either ServerError Int64
parseInt64Either label raw =
  case readMaybe (T.unpack (T.strip raw)) :: Maybe Int64 of
    Just n | n > 0 -> Right n
    _ ->
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid " <> label <> " id"))
        }

parseDoubleEither :: T.Text -> T.Text -> Either ServerError Double
parseDoubleEither label raw =
  case readMaybe (T.unpack (T.strip raw)) :: Maybe Double of
    Just n
      | isNaN n || isInfinite n ->
          Left err400
            { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid " <> label))
            }
      | otherwise -> Right n
    Nothing ->
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid " <> label))
        }

parseNearQueryEither :: T.Text -> Either ServerError (Double, Double, Double)
parseNearQueryEither raw =
  case map T.strip (T.splitOn "," raw) of
    [latRaw, lngRaw] -> do
      lat <- parseDoubleEither "near latitude" latRaw
      lng <- parseDoubleEither "near longitude" lngRaw
      validateCoordinates lat lng
      pure (lat, lng, 25)
    [latRaw, lngRaw, radiusRaw] -> do
      lat <- parseDoubleEither "near latitude" latRaw
      lng <- parseDoubleEither "near longitude" lngRaw
      radiusKm <- parseDoubleEither "near radiusKm" radiusRaw
      validateCoordinates lat lng
      validateRadius radiusKm
      pure (lat, lng, radiusKm)
    _ ->
      Left err400
        { errBody = "near must use format lat,lng or lat,lng,radiusKm"
        }

validateCoordinates :: Double -> Double -> Either ServerError ()
validateCoordinates lat lng
  | lat < (-90) || lat > 90 = Left err400 { errBody = "near latitude must be between -90 and 90" }
  | lng < (-180) || lng > 180 = Left err400 { errBody = "near longitude must be between -180 and 180" }
  | otherwise = Right ()

validateRadius :: Double -> Either ServerError ()
validateRadius radiusKm
  | radiusKm <= 0 = Left err400 { errBody = "near radiusKm must be greater than 0" }
  | radiusKm > 1000 = Left err400 { errBody = "near radiusKm exceeds allowed maximum" }
  | otherwise = Right ()

haversineDistanceKm :: Double -> Double -> Double -> Double -> Double
haversineDistanceKm lat1 lng1 lat2 lng2 =
  let earthRadiusKm = 6371
      dLat = degToRad (lat2 - lat1)
      dLng = degToRad (lng2 - lng1)
      lat1Rad = degToRad lat1
      lat2Rad = degToRad lat2
      a =
        (sin (dLat / 2) * sin (dLat / 2)) +
          (cos lat1Rad * cos lat2Rad * sin (dLng / 2) * sin (dLng / 2))
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
  in earthRadiusKm * c

degToRad :: Double -> Double
degToRad deg = deg * pi / 180

parseKeyOr400 :: ToBackendKey SqlBackend record => T.Text -> T.Text -> AppM (Key record)
parseKeyOr400 label raw =
  case parseInt64Either label raw of
    Left e -> throwError e
    Right n -> pure (toSqlKey n)

renderPartyId :: AuthedUser -> T.Text
renderPartyId = renderKeyText . auPartyId

renderKeyText :: ToBackendKey SqlBackend record => Key record -> T.Text
renderKeyText = T.pack . show . fromSqlKey

cleanMaybeText :: Maybe T.Text -> Maybe T.Text
cleanMaybeText mVal =
  case fmap T.strip mVal of
    Just txt | not (T.null txt) -> Just txt
    _ -> Nothing

normalizeArtistGenres :: [T.Text] -> [T.Text]
normalizeArtistGenres rawGenres =
  reverse normalizedRev
  where
    (_, normalizedRev) = foldl' step (Set.empty, []) rawGenres

    step :: (Set.Set T.Text, [T.Text]) -> T.Text -> (Set.Set T.Text, [T.Text])
    step (seen, acc) rawGenre =
      case nonEmptyText rawGenre of
        Nothing -> (seen, acc)
        Just genreVal ->
          let dedupeKey = T.toCaseFold genreVal
          in if Set.member dedupeKey seen
              then (seen, acc)
              else (Set.insert dedupeKey seen, genreVal : acc)

artistGenresFromRowsAndFallback :: [Entity ArtistGenre] -> Maybe [T.Text] -> [T.Text]
artistGenresFromRowsAndFallback genreRows fallbackGenres =
  let normalizedFromRows = normalizeArtistGenres (map (artistGenreGenre . entityVal) genreRows)
      normalizedFallback = normalizeArtistGenres (fromMaybe [] fallbackGenres)
  in if null normalizedFromRows then normalizedFallback else normalizedFromRows

resolveBudgetLineKey :: ConnectionPool -> SocialEventId -> Maybe T.Text -> AppM (Maybe EventBudgetLineId)
resolveBudgetLineKey _ _ Nothing = pure Nothing
resolveBudgetLineKey _ _ (Just raw) | T.null (T.strip raw) = pure Nothing
resolveBudgetLineKey pool eventKey (Just raw) = do
  lineKey <- parseKeyOr400 "budget line" raw
  mLine <- liftIO $ runSqlPool (get lineKey) pool
  lineRec <- maybe (throwError err404 { errBody = "Budget line not found" }) pure mLine
  when (eventBudgetLineEventId lineRec /= eventKey) $
    throwError err400 { errBody = "Budget line does not belong to this event" }
  pure (Just lineKey)

eventCurrencyFromEvent :: SocialEvent -> T.Text
eventCurrencyFromEvent eventRec =
  fromMaybe "USD" (emCurrency (decodeEventMetadata (socialEventMetadata eventRec)))

eventBudgetFromEvent :: SocialEvent -> Maybe Int
eventBudgetFromEvent eventRec =
  emBudgetCents (decodeEventMetadata (socialEventMetadata eventRec))

fallbackBudget :: Int -> Maybe Int
fallbackBudget plannedExpenseCents
  | plannedExpenseCents > 0 = Just plannedExpenseCents
  | otherwise = Nothing

normalizeCurrency :: T.Text -> T.Text
normalizeCurrency raw =
  let cleaned = T.toUpper (T.strip raw)
  in if T.null cleaned then "USD" else cleaned

nonEmptyText :: T.Text -> Maybe T.Text
nonEmptyText txt =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just trimmed

applyUploadExtension :: Maybe T.Text -> Maybe T.Text -> T.Text
applyUploadExtension name fallback =
  let resolved = fromMaybe "upload" name
      extFromFallback =
        case fallback of
          Nothing -> ""
          Just raw -> T.pack (takeExtension (T.unpack raw))
      extFromName = T.pack (takeExtension (T.unpack resolved))
  in if T.null extFromName && not (T.null extFromFallback)
      then resolved <> extFromFallback
      else resolved

sanitizeUploadFileName :: T.Text -> T.Text
sanitizeUploadFileName raw =
  let trimmed = T.strip raw
      baseName = T.pack (takeFileName (T.unpack trimmed))
      cleaned = T.map normalizeUploadChar baseName
      stripped = T.dropWhile (== '-') (T.dropWhileEnd (== '-') cleaned)
  in if T.null stripped || stripped == "." || stripped == ".."
      then "upload"
      else stripped

normalizeUploadChar :: Char -> Char
normalizeUploadChar ch
  | isAscii ch && isAlphaNum ch = ch
  | ch == '.' || ch == '-' || ch == '_' = ch
  | ch == ' ' = '-'
  | otherwise = '-'

buildUploadAssetUrl :: T.Text -> T.Text -> T.Text
buildUploadAssetUrl assetsBase relPath =
  let base = T.dropWhileEnd (== '/') assetsBase
      path = T.dropWhile (== '/') relPath
  in base <> "/" <> path

isImageUpload :: T.Text -> T.Text -> Bool
isImageUpload mimeType fileName =
  let normalizedMime = normalizeUploadMimeType mimeType
      ext = T.toLower (T.pack (takeExtension (T.unpack fileName)))
  in case normalizedMime of
      "image/jpeg" -> ext `elem` [".jpg", ".jpeg"]
      "image/png" -> ext == ".png"
      "image/webp" -> ext == ".webp"
      "image/gif" -> ext == ".gif"
      "image/bmp" -> ext == ".bmp"
      _ -> False

normalizeUploadMimeType :: T.Text -> T.Text
normalizeUploadMimeType raw =
  T.toLower (T.strip (fst (T.breakOn ";" raw)))

normalizeTicketTierCode :: T.Text -> T.Text
normalizeTicketTierCode raw =
  let upper = T.toUpper (T.strip raw)
      withDash = T.map (\c -> if c == ' ' then '-' else c) upper
      cleaned = T.filter (\c -> isAlphaNum c || c == '-' || c == '_') withDash
      chunks = filter (not . T.null) (T.splitOn "-" cleaned)
      normalized = T.intercalate "-" chunks
  in if T.null normalized then "GENERAL" else normalized

invalidSalesWindow :: Maybe UTCTime -> Maybe UTCTime -> Bool
invalidSalesWindow (Just startAt) (Just endAt) = startAt >= endAt
invalidSalesWindow _ _ = False

ticketTierAvailability :: EventTicketTier -> Int
ticketTierAvailability tier =
  max 0 (eventTicketTierQuantityTotal tier - eventTicketTierQuantitySold tier)

isTicketTierSaleOpen :: UTCTime -> EventTicketTier -> Bool
isTicketTierSaleOpen now tier =
  eventTicketTierIsActive tier
    && maybe True (<= now) (eventTicketTierSalesStart tier)
    && maybe True (>= now) (eventTicketTierSalesEnd tier)

isEventManager :: T.Text -> SocialEvent -> Bool
isEventManager currentParty eventRow =
  case cleanMaybeText (socialEventOrganizerPartyId eventRow) of
    Nothing -> False
    Just owner -> owner == currentParty

claimOrRequireEventManager :: T.Text -> ConnectionPool -> SocialEventId -> SocialEvent -> AppM SocialEvent
claimOrRequireEventManager currentParty pool eventKey eventRow =
  case cleanMaybeText (socialEventOrganizerPartyId eventRow) of
    Just owner | owner == currentParty -> pure eventRow
    Just _ -> throwError err403 { errBody = "Only the event organizer can manage this event" }
    Nothing -> do
      now <- liftIO getCurrentTime
      liftIO $ runSqlPool
        (update eventKey [SocialEventOrganizerPartyId =. Just currentParty, SocialEventUpdatedAt =. now])
        pool
      pure eventRow { socialEventOrganizerPartyId = Just currentParty, socialEventUpdatedAt = now }

loadEventArtists :: ConnectionPool -> SocialEventId -> IO [ArtistDTO]
loadEventArtists pool eventKey = runSqlPool (do
  artistLinks <- selectList [EventArtistEventId ==. eventKey] []
  forM artistLinks $ \(Entity _ link) -> do
    mArtist <- get (eventArtistArtistId link)
    case mArtist of
      Nothing ->
        pure ArtistDTO
          { artistId = Nothing
          , artistPartyId = Nothing
          , artistName = "(unknown)"
          , artistGenres = []
          , artistBio = Nothing
          , artistAvatarUrl = Nothing
          , artistSocialLinks = Nothing
          , artistCreatedAt = Nothing
          , artistUpdatedAt = Nothing
          }
      Just a -> do
        genres <- selectList [ArtistGenreArtistId ==. eventArtistArtistId link] []
        let genreList = artistGenresFromRowsAndFallback genres (artistProfileGenres a)
        pure ArtistDTO
          { artistId = Just (renderKeyText (eventArtistArtistId link))
          , artistPartyId = artistProfilePartyId a
          , artistName = artistProfileName a
          , artistGenres = genreList
          , artistBio = artistProfileBio a
          , artistAvatarUrl = artistProfileAvatarUrl a
          , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
          , artistCreatedAt = Just (artistProfileCreatedAt a)
          , artistUpdatedAt = Just (artistProfileUpdatedAt a)
          }
  ) pool

momentReactionEntityToDTO :: Entity EventMomentReaction -> EventMomentReactionDTO
momentReactionEntityToDTO (Entity _ reactionRow) = EventMomentReactionDTO
  { emrReaction =
      fromMaybe
        (eventMomentReactionReaction reactionRow)
        (normalizeMomentReaction (eventMomentReactionReaction reactionRow))
  , emrPartyId = eventMomentReactionReactorPartyId reactionRow
  , emrCreatedAt = Just (eventMomentReactionCreatedAt reactionRow)
  }

momentCommentEntityToDTO :: EventMomentCommentId -> EventMomentComment -> EventMomentCommentDTO
momentCommentEntityToDTO commentKey commentRow = EventMomentCommentDTO
  { emcId = Just (renderKeyText commentKey)
  , emcMomentId = Just (renderKeyText (eventMomentCommentMomentId commentRow))
  , emcAuthorPartyId = eventMomentCommentAuthorPartyId commentRow
  , emcAuthorName = eventMomentCommentAuthorName commentRow
  , emcBody = eventMomentCommentBody commentRow
  , emcCreatedAt = Just (eventMomentCommentCreatedAt commentRow)
  , emcUpdatedAt = Just (eventMomentCommentUpdatedAt commentRow)
  }

momentEntityToDTO
  :: EventMomentId
  -> EventMoment
  -> [EventMomentReactionDTO]
  -> [EventMomentCommentDTO]
  -> EventMomentDTO
momentEntityToDTO momentKey momentRow reactions comments = EventMomentDTO
  { emId = Just (renderKeyText momentKey)
  , emEventId = Just (renderKeyText (eventMomentEventId momentRow))
  , emAuthorPartyId = eventMomentAuthorPartyId momentRow
  , emAuthorName = eventMomentAuthorName momentRow
  , emCaption = eventMomentCaption momentRow
  , emMediaUrl = eventMomentMediaUrl momentRow
  , emMediaType =
      fromMaybe
        (eventMomentMediaType momentRow)
        (normalizeMomentMediaType (eventMomentMediaType momentRow))
  , emMediaWidth = eventMomentMediaWidth momentRow
  , emMediaHeight = eventMomentMediaHeight momentRow
  , emMediaDurationMs = eventMomentMediaDurationMs momentRow
  , emCreatedAt = Just (eventMomentCreatedAt momentRow)
  , emUpdatedAt = Just (eventMomentUpdatedAt momentRow)
  , emReactions = reactions
  , emComments = comments
  }

loadMomentDTO :: ConnectionPool -> EventMomentId -> IO EventMomentDTO
loadMomentDTO pool momentKey = runSqlPool (do
  mMoment <- get momentKey
  case mMoment of
    Nothing -> liftIO (ioError (userError "Moment not found"))
    Just momentRow -> do
      reactionRows <- selectList [EventMomentReactionMomentId ==. momentKey] [Asc EventMomentReactionCreatedAt]
      commentRows <- selectList [EventMomentCommentMomentId ==. momentKey] [Asc EventMomentCommentCreatedAt]
      let reactions = map momentReactionEntityToDTO reactionRows
          comments = map (\(Entity commentKey commentRow) -> momentCommentEntityToDTO commentKey commentRow) commentRows
      pure (momentEntityToDTO momentKey momentRow reactions comments)
  ) pool

loadEventMoments :: ConnectionPool -> SocialEventId -> IO [EventMomentDTO]
loadEventMoments pool eventKey = runSqlPool (do
  momentRows <- selectList [EventMomentEventId ==. eventKey] [Desc EventMomentCreatedAt]
  forM momentRows $ \(Entity momentKey momentRow) -> do
    reactionRows <- selectList [EventMomentReactionMomentId ==. momentKey] [Asc EventMomentReactionCreatedAt]
    commentRows <- selectList [EventMomentCommentMomentId ==. momentKey] [Asc EventMomentCommentCreatedAt]
    let reactions = map momentReactionEntityToDTO reactionRows
        comments = map (\(Entity commentKey commentRow) -> momentCommentEntityToDTO commentKey commentRow) commentRows
    pure (momentEntityToDTO momentKey momentRow reactions comments)
  ) pool

eventEntityToDTO :: SocialEventId -> SocialEvent -> [ArtistDTO] -> EventDTO
eventEntityToDTO eid eventRow artists =
  let metadata = decodeEventMetadata (socialEventMetadata eventRow)
  in EventDTO
      { eventId = Just (renderKeyText eid)
      , eventOrganizerPartyId = socialEventOrganizerPartyId eventRow
      , eventTitle = socialEventTitle eventRow
      , eventDescription = socialEventDescription eventRow
      , eventStart = socialEventStartTime eventRow
      , eventEnd = socialEventEndTime eventRow
      , eventVenueId = fmap renderKeyText (socialEventVenueId eventRow)
      , eventPriceCents = socialEventPriceCents eventRow
      , eventCapacity = socialEventCapacity eventRow
      , eventTicketUrl = emTicketUrl metadata
      , eventImageUrl = emImageUrl metadata
      , eventIsPublic = emIsPublic metadata <|> Just True
      , eventType = emType metadata <|> Just "party"
      , eventStatus = emStatus metadata <|> Just "planning"
      , eventCurrency = emCurrency metadata <|> Just "USD"
      , eventBudgetCents = emBudgetCents metadata
      , eventCreatedAt = Just (socialEventCreatedAt eventRow)
      , eventUpdatedAt = Just (socialEventUpdatedAt eventRow)
      , eventArtists = artists
      }

ticketTierEntityToDTO :: SocialEventId -> Entity EventTicketTier -> TicketTierDTO
ticketTierEntityToDTO eventKey (Entity tierKey tier) = TicketTierDTO
  { ticketTierId = Just (renderKeyText tierKey)
  , ticketTierEventId = Just (renderKeyText eventKey)
  , ticketTierCode = eventTicketTierCode tier
  , ticketTierName = eventTicketTierName tier
  , ticketTierDescription = eventTicketTierDescription tier
  , ticketTierPriceCents = eventTicketTierPriceCents tier
  , ticketTierCurrency = eventTicketTierCurrency tier
  , ticketTierQuantityTotal = eventTicketTierQuantityTotal tier
  , ticketTierQuantitySold = eventTicketTierQuantitySold tier
  , ticketTierSalesStart = eventTicketTierSalesStart tier
  , ticketTierSalesEnd = eventTicketTierSalesEnd tier
  , ticketTierActive = eventTicketTierIsActive tier
  , ticketTierPosition = eventTicketTierPosition tier
  }

ticketEntityToDTO :: Entity EventTicket -> TicketDTO
ticketEntityToDTO (Entity ticketKey ticketRow) = TicketDTO
  { ticketId = Just (renderKeyText ticketKey)
  , ticketEventId = Just (renderKeyText (eventTicketEventId ticketRow))
  , ticketTierId = Just (renderKeyText (eventTicketTierRefId ticketRow))
  , ticketOrderId = Just (renderKeyText (eventTicketOrderRefId ticketRow))
  , ticketCode = eventTicketCode ticketRow
  , ticketStatus = normalizeTicketStatus (Just (eventTicketStatus ticketRow))
  , ticketHolderName = eventTicketHolderName ticketRow
  , ticketHolderEmail = eventTicketHolderEmail ticketRow
  , ticketCheckedInAt = eventTicketCheckedInAt ticketRow
  , ticketCreatedAt = Just (eventTicketCreatedAt ticketRow)
  , ticketUpdatedAt = Just (eventTicketUpdatedAt ticketRow)
  }

ticketOrderEntityToDTO :: Entity EventTicketOrder -> [Entity EventTicket] -> TicketOrderDTO
ticketOrderEntityToDTO (Entity orderKey orderRow) tickets = TicketOrderDTO
  { ticketOrderId = Just (renderKeyText orderKey)
  , ticketOrderEventId = Just (renderKeyText (eventTicketOrderEventId orderRow))
  , ticketOrderTierId = Just (renderKeyText (eventTicketOrderTierId orderRow))
  , ticketOrderBuyerPartyId = eventTicketOrderBuyerPartyId orderRow
  , ticketOrderBuyerName = eventTicketOrderBuyerName orderRow
  , ticketOrderBuyerEmail = eventTicketOrderBuyerEmail orderRow
  , ticketOrderQuantity = eventTicketOrderQuantity orderRow
  , ticketOrderAmountCents = eventTicketOrderAmountCents orderRow
  , ticketOrderCurrency = eventTicketOrderCurrency orderRow
  , ticketOrderStatusValue = normalizeTicketOrderStatus (Just (eventTicketOrderStatus orderRow))
  , ticketOrderPurchasedAt = Just (eventTicketOrderPurchasedAt orderRow)
  , ticketOrderCreatedAt = Just (eventTicketOrderCreatedAt orderRow)
  , ticketOrderUpdatedAt = Just (eventTicketOrderUpdatedAt orderRow)
  , ticketOrderTickets = map ticketEntityToDTO tickets
  }

budgetLineEntityToDTO :: SocialEventId -> Maybe Int -> Entity EventBudgetLine -> EventBudgetLineDTO
budgetLineEntityToDTO eventKey mActualCents (Entity lineKey lineRec) = EventBudgetLineDTO
  { eblId = Just (renderKeyText lineKey)
  , eblEventId = Just (renderKeyText eventKey)
  , eblCode = eventBudgetLineCode lineRec
  , eblName = eventBudgetLineName lineRec
  , eblType = normalizeBudgetLineType (Just (eventBudgetLineLineType lineRec))
  , eblCategory = normalizeCategory (Just (eventBudgetLineCategory lineRec))
  , eblPlannedCents = eventBudgetLinePlannedCents lineRec
  , eblActualCents = mActualCents
  , eblNotes = eventBudgetLineNotes lineRec
  , eblCreatedAt = Just (eventBudgetLineCreatedAt lineRec)
  , eblUpdatedAt = Just (eventBudgetLineUpdatedAt lineRec)
  }

financeInvariantServerError :: T.Text -> ServerError
financeInvariantServerError message =
  err500 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

financeEntryEntityToDTOEither :: Entity EventFinanceEntry -> Either T.Text EventFinanceEntryDTO
financeEntryEntityToDTOEither (Entity entryKey entryRec) = do
  (directionVal, sourceVal, statusVal) <- validateStoredFinanceEntryDimensions entryRec
  pure EventFinanceEntryDTO
    { efeId = Just (renderKeyText entryKey)
    , efeEventId = Just (renderKeyText (eventFinanceEntryEventId entryRec))
    , efeBudgetLineId = fmap renderKeyText (eventFinanceEntryBudgetLineId entryRec)
    , efeDirection = directionVal
    , efeSource = sourceVal
    , efeCategory = normalizeCategory (Just (eventFinanceEntryCategory entryRec))
    , efeConcept = eventFinanceEntryConcept entryRec
    , efeAmountCents = eventFinanceEntryAmountCents entryRec
    , efeCurrency = normalizeCurrency (eventFinanceEntryCurrency entryRec)
    , efeStatus = statusVal
    , efeExternalRef = eventFinanceEntryExternalRef entryRec
    , efeNotes = eventFinanceEntryNotes entryRec
    , efeOccurredAt = eventFinanceEntryOccurredAt entryRec
    , efeRecordedByPartyId = eventFinanceEntryRecordedByPartyId entryRec
    , efeCreatedAt = Just (eventFinanceEntryCreatedAt entryRec)
    , efeUpdatedAt = Just (eventFinanceEntryUpdatedAt entryRec)
    }

storedFinanceEntrySummaryFields :: Entity EventFinanceEntry -> Either T.Text (Int, T.Text, T.Text, T.Text)
storedFinanceEntrySummaryFields (Entity _ entryRec) = do
  (directionVal, sourceVal, statusVal) <- validateStoredFinanceEntryDimensions entryRec
  pure (eventFinanceEntryAmountCents entryRec, directionVal, sourceVal, statusVal)

ticketOrderAccountingEntries :: SocialEventId -> Entity EventTicketOrder -> [EventFinanceEntryDTO]
ticketOrderAccountingEntries eventKey (Entity orderKey orderRec) =
  case normalizeTicketOrderStatus (Just (eventTicketOrderStatus orderRec)) of
    "paid" ->
      [ mkEntry "paid" "income" "ticket_sale" "posted" "Ticket sale"
      ]
    "refunded" ->
      [ mkEntry "refunded" "expense" "ticket_refund" "posted" "Ticket refund"
      ]
    "pending" ->
      [ mkEntry "pending" "income" "ticket_sale" "pending" "Ticket sale pending"
      ]
    _ -> []
  where
    orderIdTxt = renderKeyText orderKey
    mkEntry suffix direction source statusLabel conceptPrefix =
      EventFinanceEntryDTO
        { efeId = Just ("ticket-order-" <> orderIdTxt <> "-" <> suffix)
        , efeEventId = Just (renderKeyText eventKey)
        , efeBudgetLineId = Nothing
        , efeDirection = direction
        , efeSource = source
        , efeCategory = "tickets"
        , efeConcept = conceptPrefix <> " #" <> orderIdTxt
        , efeAmountCents = eventTicketOrderAmountCents orderRec
        , efeCurrency = normalizeCurrency (eventTicketOrderCurrency orderRec)
        , efeStatus = statusLabel
        , efeExternalRef = Just orderIdTxt
        , efeNotes = Nothing
        , efeOccurredAt = eventTicketOrderPurchasedAt orderRec
        , efeRecordedByPartyId = eventTicketOrderBuyerPartyId orderRec
        , efeCreatedAt = Just (eventTicketOrderCreatedAt orderRec)
        , efeUpdatedAt = Just (eventTicketOrderUpdatedAt orderRec)
        }

matchesFinanceFilters :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> EventFinanceEntryDTO -> Bool
matchesFinanceFilters mDirection mSource mStatus entry =
  directionOk && sourceOk && statusOk
  where
    directionOk = maybe True (== efeDirection entry) mDirection
    sourceOk = maybe True (== efeSource entry) mSource
    statusOk = maybe True (== efeStatus entry) mStatus

generateUniqueTicketCode :: MonadIO m => ReaderT SqlBackend m T.Text
generateUniqueTicketCode = do
  uuidVal <- liftIO UUIDV4.nextRandom
  let baseCode =
        T.toUpper
          (T.take 12 (T.replace "-" "" (UUID.toText uuidVal)))
      code = "TDF-" <> baseCode
  mExisting <- getBy (UniqueEventTicketCode code)
  case mExisting of
    Nothing -> pure code
    Just _ -> generateUniqueTicketCode
