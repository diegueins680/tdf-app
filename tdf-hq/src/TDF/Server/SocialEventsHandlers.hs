{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Server.SocialEventsHandlers
  ( socialEventsServer
  , normalizeInvitationStatus
  , parseInvitationIdsEither
  , followArtistDb
  , normalizeTicketOrderStatus
  , normalizeTicketStatus
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (forM, forM_, replicateM, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAlphaNum)
import           Data.Int (Int64)
import           Data.Maybe (catMaybes, fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import           Text.Read (readMaybe)

import           Servant

-- Pull in full Persistent surface so TH-generated field constructors
-- (EventRsvpEventId, SocialEventStartTime, etc.) are available.
import           Database.Persist
import           Database.Persist.Sql (ConnectionPool, SqlBackend, fromSqlKey, runSqlPool, toSqlKey)

import           TDF.API.SocialEventsAPI
import           TDF.Auth (AuthedUser(..))
import           TDF.DTO.SocialEventsDTO
  ( EventDTO(..)
  , VenueDTO(..)
  , ArtistDTO(..)
  , ArtistSocialLinksDTO(..)
  , ArtistFollowerDTO(..)
  , ArtistFollowRequest(..)
  , RsvpDTO(..)
  , InvitationDTO(..)
  , TicketTierDTO(..)
  , TicketPurchaseRequestDTO(..)
  , TicketOrderStatusUpdateDTO(..)
  , TicketCheckInRequestDTO(..)
  , TicketDTO(..)
  , TicketOrderDTO(..)
  )
import           TDF.DB (Env(..))
import           TDF.Models.SocialEventsModels hiding (venueAddress, venueCapacity, venueCity, venueContact, venueCountry, venueCreatedAt, venueName, venueUpdatedAt)
import qualified TDF.Models.SocialEventsModels as SM

type AppM = ReaderT Env Handler

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
  }

emptyEventMetadata :: EventMetadataDTO
emptyEventMetadata = EventMetadataDTO
  { emTicketUrl = Nothing
  , emImageUrl = Nothing
  , emIsPublic = Nothing
  }

instance Aeson.ToJSON EventMetadataDTO where
  toJSON EventMetadataDTO{..} = Aeson.object
    [ "ticketUrl" Aeson..= emTicketUrl
    , "imageUrl" Aeson..= emImageUrl
    , "isPublic" Aeson..= emIsPublic
    ]

instance Aeson.FromJSON EventMetadataDTO where
  parseJSON = Aeson.withObject "EventMetadataDTO" $ \o ->
    EventMetadataDTO
      <$> o Aeson..:? "ticketUrl"
      <*> o Aeson..:? "imageUrl"
      <*> o Aeson..:? "isPublic"

decodeEventMetadata :: Maybe T.Text -> EventMetadataDTO
decodeEventMetadata mTxt = fromMaybe emptyEventMetadata $ do
  txt <- mTxt
  Aeson.decodeStrict (TE.encodeUtf8 txt)

encodeEventMetadata :: EventMetadataDTO -> Maybe T.Text
encodeEventMetadata EventMetadataDTO{..}
  | isNothing emTicketUrl && isNothing emImageUrl && isNothing emIsPublic = Nothing
  | otherwise =
      Just
        (TE.decodeUtf8 . BL.toStrict . Aeson.encode $
          EventMetadataDTO
            { emTicketUrl = emTicketUrl
            , emImageUrl = emImageUrl
            , emIsPublic = emIsPublic
            })

eventMetadataFromDTO :: EventDTO -> EventMetadataDTO
eventMetadataFromDTO dto = EventMetadataDTO
  { emTicketUrl = cleanMaybeText (eventTicketUrl dto)
  , emImageUrl = cleanMaybeText (eventImageUrl dto)
  , emIsPublic = eventIsPublic dto
  }

mergeEventMetadata :: EventMetadataDTO -> EventMetadataDTO -> EventMetadataDTO
mergeEventMetadata incoming existing = EventMetadataDTO
  { emTicketUrl = emTicketUrl incoming <|> emTicketUrl existing
  , emImageUrl = emImageUrl incoming <|> emImageUrl existing
  , emIsPublic = emIsPublic incoming <|> emIsPublic existing
  }

socialEventsServer :: AuthedUser -> ServerT SocialEventsAPI AppM
socialEventsServer user = eventsServer
               :<|> venuesServer
               :<|> artistsServer
               :<|> rsvpsServer
               :<|> invitationsServer
               :<|> ticketsServer
  where
    currentPartyId :: T.Text
    currentPartyId = renderPartyId user

    -- Events
    eventsServer :: ServerT EventsRoutes AppM
    eventsServer = listEvents
               :<|> createEvent
               :<|> getEvent
               :<|> updateEvent
               :<|> deleteEvent

    listEvents :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> AppM [EventDTO]
    listEvents mCity mStartAfter mArtistId mVenueId mLimit mOffset = do
      Env{..} <- ask
      limit <- resolveLimit 200 500 mLimit
      offset <- resolveOffset mOffset
      let startFilter = case mStartAfter of
            Nothing -> []
            Just raw -> case iso8601ParseM (T.unpack raw) of
              Just t  -> [SocialEventStartTime >=. t]
              Nothing -> []
      cityFilter <- case fmap T.strip mCity of
        Nothing -> pure []
        Just "" -> pure []
        Just cityTxt -> do
          venueRows <- liftIO $ runSqlPool (selectList [VenueCity ==. Just cityTxt] []) envPool
          let ids = map entityKey venueRows
          if null ids
            then pure [SocialEventId ==. toSqlKey 0] -- force empty result set
            else pure [SocialEventVenueId <-. map Just ids]
      venueFilter <- case fmap T.strip mVenueId of
        Nothing -> pure []
        Just "" -> pure []
        Just raw -> case readMaybe (T.unpack raw) :: Maybe Int64 of
          Nothing -> throwError err400 { errBody = "Invalid venue id" }
          Just vnum -> pure [SocialEventVenueId ==. Just (toSqlKey vnum)]
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
      forM rows $ \(Entity eid eventRow) -> do
        artists <- liftIO $ loadEventArtists envPool eid
        pure (eventEntityToDTO eid eventRow artists)

    createEvent :: EventDTO -> AppM EventDTO
    createEvent dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      when (T.null (T.strip (eventTitle dto))) $ throwError err400 { errBody = "title is required" }
      when (eventStart dto >= eventEnd dto) $ throwError err400 { errBody = "start time must be before end time" }
      let metadataVal =
            encodeEventMetadata
              EventMetadataDTO
                { emTicketUrl = cleanMaybeText (eventTicketUrl dto)
                , emImageUrl = cleanMaybeText (eventImageUrl dto)
                , emIsPublic = eventIsPublic dto <|> Just True
                }
          createdMetadata = decodeEventMetadata metadataVal
      mVenueKey <- case eventVenueId dto of
        Nothing -> pure Nothing
        Just txt -> case readMaybe (T.unpack txt) :: Maybe Int64 of
          Nothing -> throwError err400 { errBody = "Invalid venue id" }
          Just vnum -> pure (Just (toSqlKey vnum))
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
      let artists = eventArtists dto
      liftIO $ runSqlPool
        (forM_ artists $ \a ->
           case artistId a of
             Nothing -> pure ()
             Just atxt -> case readMaybe (T.unpack atxt) :: Maybe Int64 of
               Nothing -> pure ()
               Just anum -> insert_ (EventArtist key (toSqlKey anum) Nothing)
        )
        envPool
      pure dto
        { eventId = Just (renderKeyText key)
        , eventOrganizerPartyId = Just currentPartyId
        , eventTicketUrl = emTicketUrl createdMetadata
        , eventImageUrl = emImageUrl createdMetadata
        , eventIsPublic = emIsPublic createdMetadata
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

    updateEvent :: T.Text -> EventDTO -> AppM EventDTO
    updateEvent rawId dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" rawId
      mExisting <- liftIO $ runSqlPool (get eventKey) envPool
      existing <- maybe (throwError err404 { errBody = "Event not found" }) pure mExisting
      when (T.null (T.strip (eventTitle dto))) $ throwError err400 { errBody = "title is required" }
      when (eventStart dto >= eventEnd dto) $ throwError err400 { errBody = "start time must be before end time" }
      let existingMetadata = decodeEventMetadata (socialEventMetadata existing)
          requestedMetadata = eventMetadataFromDTO dto
          mergedMetadata =
            mergeEventMetadata
              requestedMetadata
              existingMetadata
      mVenueKey <- case eventVenueId dto of
        Nothing -> pure Nothing
        Just txt -> case readMaybe (T.unpack txt) :: Maybe Int64 of
          Nothing -> throwError err400 { errBody = "Invalid venue id" }
          Just vnum -> pure (Just (toSqlKey vnum))
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
      let artists = eventArtists dto
      liftIO $ runSqlPool
        (forM_ artists $ \a ->
           case artistId a of
             Nothing -> pure ()
             Just atxt -> case readMaybe (T.unpack atxt) :: Maybe Int64 of
               Nothing -> pure ()
               Just anum -> insert_ (EventArtist eventKey (toSqlKey anum) Nothing)
        )
        envPool
      pure (dto
        { eventId = Just rawId
        , eventOrganizerPartyId = socialEventOrganizerPartyId existing
        , eventTicketUrl = emTicketUrl mergedMetadata
        , eventImageUrl = emImageUrl mergedMetadata
        , eventIsPublic = emIsPublic mergedMetadata
        , eventCreatedAt = Just (socialEventCreatedAt existing)
        , eventUpdatedAt = Just now
        })

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
          deleteWhere [EventTicketEventId ==. eventKey]
          deleteWhere [EventTicketOrderEventId ==. eventKey]
          deleteWhere [EventTicketTierEventId ==. eventKey]
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

    listVenues :: Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> AppM [VenueDTO]
    listVenues mCity _mNear mLimit mOffset = do
      Env{..} <- ask
      limit <- resolveLimit 200 500 mLimit
      offset <- resolveOffset mOffset
      let filters = case mCity of
                      Just c | not (T.null (T.strip c)) -> [VenueCity ==. Just (T.strip c)]
                      _ -> []
      rows <- liftIO $ runSqlPool (selectList filters [Asc VenueName, LimitTo limit, OffsetBy offset]) envPool
      pure $ map (\(Entity vid v) -> VenueDTO
        { venueId = Just (renderKeyText vid)
        , venueName = SM.venueName v
        , venueAddress = SM.venueAddress v
        , venueCity = SM.venueCity v
        , venueCountry = SM.venueCountry v
        , venueLat = venueLatitude v
        , venueLng = venueLongitude v
        , venueCapacity = SM.venueCapacity v
        , venueContact = SM.venueContact v
        , venueCreatedAt = Just (SM.venueCreatedAt v)
        , venueUpdatedAt = Just (SM.venueUpdatedAt v)
        }) rows

    createVenue :: VenueDTO -> AppM VenueDTO
    createVenue dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      key <- liftIO $ runSqlPool (insert Venue
        { venueName = venueName dto
        , venueAddress = venueAddress dto
        , venueCity = venueCity dto
        , venueCountry = venueCountry dto
        , venueLatitude = venueLat dto
        , venueLongitude = venueLng dto
        , venueCapacity = venueCapacity dto
        , venueContact = venueContact dto
        , venueCreatedAt = now
        , venueUpdatedAt = now
        }) envPool
      pure (dto
        { venueId = Just (renderKeyText key)
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
        Just v -> pure VenueDTO
          { venueId = Just (T.strip rawId)
          , venueName = SM.venueName v
          , venueAddress = SM.venueAddress v
          , venueCity = SM.venueCity v
          , venueCountry = SM.venueCountry v
          , venueLat = venueLatitude v
          , venueLng = venueLongitude v
          , venueCapacity = SM.venueCapacity v
          , venueContact = SM.venueContact v
          , venueCreatedAt = Just (SM.venueCreatedAt v)
          , venueUpdatedAt = Just (SM.venueUpdatedAt v)
          }

    updateVenue :: T.Text -> VenueDTO -> AppM VenueDTO
    updateVenue rawId dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      venueKey <- parseKeyOr400 "venue" rawId
      mExisting <- liftIO $ runSqlPool (get venueKey) envPool
      existing <- maybe (throwError err404 { errBody = "Venue not found" }) pure mExisting
      liftIO $ runSqlPool (update venueKey
        [ VenueName =. venueName dto
        , VenueAddress =. venueAddress dto
        , VenueCity =. venueCity dto
        , VenueCountry =. venueCountry dto
        , VenueLatitude =. venueLat dto
        , VenueLongitude =. venueLng dto
        , VenueCapacity =. venueCapacity dto
        , VenueContact =. venueContact dto
        , VenueUpdatedAt =. now
        ]) envPool
      pure (dto
        { venueId = Just rawId
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
      rows <- liftIO $ runSqlPool (selectList [] [Desc ArtistProfileCreatedAt, LimitTo limit, OffsetBy offset]) envPool
      artists <- forM rows $ \(Entity aid a) -> do
        genres <- liftIO $ runSqlPool (selectList [ArtistGenreArtistId ==. aid] []) envPool
        let genreList = map (artistGenreGenre . entityVal) genres
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
      pure (catMaybes artists)

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
      key <- liftIO $ runSqlPool (insert ArtistProfile
        { artistProfilePartyId = cleanMaybeText (artistPartyId dto)
        , artistProfileName = artistName dto
        , artistProfileBio = artistBio dto
        , artistProfileAvatarUrl = artistAvatarUrl dto
        , artistProfileGenres = Just (artistGenres dto)
        , artistProfileSocialLinks = encodeSocialLinks (artistSocialLinks dto)
        , artistProfileCreatedAt = now
        , artistProfileUpdatedAt = now
        }) envPool
      let genreList = artistGenres dto
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
          pure ArtistDTO
            { artistId = Just (T.strip idStr)
            , artistPartyId = artistProfilePartyId a
            , artistName = artistProfileName a
            , artistGenres = map (artistGenreGenre . entityVal) genres
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
      mExisting <- liftIO $ runSqlPool (get artistKey) envPool
      existing <- maybe (throwError err404 { errBody = "Artist not found" }) pure mExisting
      let nextPartyId = cleanMaybeText (artistPartyId dto) <|> artistProfilePartyId existing
      liftIO $ runSqlPool (update artistKey
        [ ArtistProfilePartyId =. nextPartyId
        , ArtistProfileName =. artistName dto
        , ArtistProfileBio =. artistBio dto
        , ArtistProfileAvatarUrl =. artistAvatarUrl dto
        , ArtistProfileGenres =. Just (artistGenres dto)
        , ArtistProfileSocialLinks =. encodeSocialLinks (artistSocialLinks dto)
        , ArtistProfileUpdatedAt =. now
        ]) envPool
      liftIO $ runSqlPool (deleteWhere [ArtistGenreArtistId ==. artistKey]) envPool
      liftIO $ runSqlPool
        (forM_ (artistGenres dto) $ \g ->
           insert_ ArtistGenre
             { artistGenreArtistId = artistKey
             , artistGenreGenre = g
             }
        )
        envPool
      pure dto
        { artistId = Just (T.strip idStr)
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
      let followerParty = T.strip afrFollowerPartyId
      when (T.null followerParty) $ throwError err400 { errBody = "followerPartyId is required" }
      liftIO $ followArtistDb envPool artistKey followerParty

    unfollowArtist :: T.Text -> Maybe T.Text -> AppM NoContent
    unfollowArtist artistIdStr mFollower = do
      Env{..} <- ask
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      followerParty <- case cleanMaybeText mFollower of
        Nothing -> throwError err400 { errBody = "follower query param is required" }
        Just t -> pure t
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
      rsvpRows <- liftIO $ runSqlPool (selectList [EventRsvpEventId ==. eventKey] []) envPool
      pure $ map (\(Entity rid rsvp) -> RsvpDTO
        { rsvpId = Just (renderKeyText rid)
        , rsvpEventId = eventIdStr
        , rsvpPartyId = eventRsvpPartyId rsvp
        , rsvpStatus = eventRsvpStatus rsvp
        , rsvpCreatedAt = Just (eventRsvpCreatedAt rsvp)
        }) rsvpRows

    createRsvp :: T.Text -> RsvpDTO -> AppM RsvpDTO
    createRsvp eventIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      eventKey <- parseKeyOr400 "event" eventIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }

      existingRsvps <- liftIO $ runSqlPool
        (selectList [EventRsvpEventId ==. eventKey, EventRsvpPartyId ==. rsvpPartyId dto] [])
        envPool

      key <- case existingRsvps of
        [] ->
          liftIO $ runSqlPool (insert EventRsvp
            { eventRsvpEventId = eventKey
            , eventRsvpPartyId = rsvpPartyId dto
            , eventRsvpStatus = rsvpStatus dto
            , eventRsvpMetadata = Nothing
            , eventRsvpCreatedAt = now
            , eventRsvpUpdatedAt = now
            }) envPool
        (Entity existingKey _ : _) -> do
          liftIO $ runSqlPool (update existingKey
            [ EventRsvpStatus =. rsvpStatus dto
            , EventRsvpUpdatedAt =. now
            ]) envPool
          pure existingKey

      pure dto { rsvpId = Just (renderKeyText key) }

    -- Invitations
    invitationsServer :: ServerT InvitationsRoutes AppM
    invitationsServer eventIdStr =
      listInvitations eventIdStr
        :<|> createInvitation eventIdStr
        :<|> updateInvitation eventIdStr

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
      let toParty = T.strip (invitationToPartyId dto)
      when (T.null toParty) $ throwError err400 { errBody = "invitationToPartyId is required" }
      let statusVal = normalizeInvitationStatus (invitationStatus dto)
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

    updateInvitation :: T.Text -> T.Text -> InvitationDTO -> AppM InvitationDTO
    updateInvitation eventIdStr invitationIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      (eventKey, invitationKey) <- parseIds eventIdStr invitationIdStr
      mEvent <- liftIO $ runSqlPool (get eventKey) envPool
      when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
      mExisting <- liftIO $ runSqlPool (get invitationKey) envPool
      case mExisting of
        Nothing -> throwError err404 { errBody = "Invitation not found" }
        Just inv -> do
          when (eventInvitationEventId inv /= eventKey) $ throwError err400 { errBody = "Invitation does not belong to this event" }
          let statusVal = normalizeInvitationStatus (invitationStatus dto)
          let messageVal = invitationMessage dto <|> eventInvitationMessage inv
          let newToParty = cleanMaybeText (Just (invitationToPartyId dto))
          let toPartyVal = newToParty <|> eventInvitationToPartyId inv
          liftIO $ runSqlPool (update invitationKey
            [ EventInvitationStatus =. Just statusVal
            , EventInvitationMessage =. messageVal
            , EventInvitationToPartyId =. toPartyVal
            , EventInvitationUpdatedAt =. now
            ]) envPool
          pure InvitationDTO
            { invitationId = Just (renderKeyText invitationKey)
            , invitationEventId = Just (T.strip eventIdStr)
            , invitationFromPartyId = eventInvitationFromPartyId inv
            , invitationToPartyId = maybe "" id toPartyVal
            , invitationStatus = Just statusVal
            , invitationMessage = messageVal
            , invitationCreatedAt = Just (eventInvitationCreatedAt inv)
            , invitationUpdatedAt = Just now
            }

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
      let baseCode = cleanMaybeText (Just (ticketTierCode dto)) <|> Just tierName
          tierCode = normalizeTicketTierCode (fromMaybe tierName baseCode)
          currencyVal = normalizeCurrency (ticketTierCurrency dto)
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
      let baseCode = cleanMaybeText (Just (ticketTierCode dto)) <|> Just tierName
          tierCode = normalizeTicketTierCode (fromMaybe tierName baseCode)
          currencyVal = normalizeCurrency (ticketTierCurrency dto)
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
          requestedBuyer = cleanMaybeText mBuyerPartyId
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
          requestedBuyer = cleanMaybeText ticketPurchaseBuyerPartyId
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
          buyerEmail = cleanMaybeText ticketPurchaseBuyerEmail
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

      ticketEntity <- case (cleanMaybeText ticketCheckInTicketId, cleanMaybeText ticketCheckInTicketCode) of
        (Just rawTicketId, _) -> do
          ticketKey <- parseKeyOr400 "ticket" rawTicketId
          mTicket <- liftIO $ runSqlPool (getEntity ticketKey) envPool
          maybe (throwError err404 { errBody = "Ticket not found" }) pure mTicket
        (Nothing, Just rawCode) -> do
          let codeVal = T.toUpper (T.strip rawCode)
          mTicket <- liftIO $ runSqlPool
            (selectFirst [EventTicketEventId ==. eventKey, EventTicketCode ==. codeVal] [])
            envPool
          maybe (throwError err404 { errBody = "Ticket not found" }) pure mTicket
        _ -> throwError err400 { errBody = "Provide ticketCheckInTicketId or ticketCheckInTicketCode" }

      let ticketKey = entityKey ticketEntity
          ticketVal = entityVal ticketEntity
      when (eventTicketEventId ticketVal /= eventKey) $ throwError err400 { errBody = "Ticket does not belong to this event" }
      orderRef <- liftIO $ runSqlPool (get (eventTicketOrderRefId ticketVal)) envPool
      let orderStatus = maybe "pending" (normalizeTicketOrderStatus . Just . eventTicketOrderStatus) orderRef
      when (orderStatus /= "paid") $ throwError err400 { errBody = "Only paid tickets can be checked in" }
      case normalizeTicketStatus (Just (eventTicketStatus ticketVal)) of
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

    parseIds :: T.Text -> T.Text -> AppM (SocialEventId, EventInvitationId)
    parseIds eventIdStr invitationIdStr =
      case parseInvitationIdsEither eventIdStr invitationIdStr of
        Right ids -> pure ids
        Left e -> throwError e

    parseArtistId :: T.Text -> AppM ArtistProfileId
    parseArtistId artistIdStr =
      case readMaybe (T.unpack (T.strip artistIdStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid artist id" }
        Just num -> pure (toSqlKey num)

-- | Stable, human-friendly identifier for a follow (artistId + follower id).
renderFollowId :: ArtistProfileId -> T.Text -> T.Text
renderFollowId artistId followerPartyId =
  T.intercalate ":" [renderKeyText artistId, followerPartyId]

-- | Insert or fetch an artist follow while keeping the created timestamp stable.
followArtistDb :: ConnectionPool -> ArtistProfileId -> T.Text -> IO ArtistFollowerDTO
followArtistDb pool artistId followerPartyIdRaw = do
  now <- getCurrentTime
  let followerPartyId = T.strip followerPartyIdRaw
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

-- | Normalize invitation status to a lowercase, non-empty value.
normalizeInvitationStatus :: Maybe T.Text -> T.Text
normalizeInvitationStatus mStatus =
  case fmap (T.toLower . T.strip) mStatus of
    Nothing -> "pending"
    Just s | T.null s -> "pending"
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
  case ( readMaybe (T.unpack (T.strip eventIdStr)) :: Maybe Int64
       , readMaybe (T.unpack (T.strip invitationIdStr)) :: Maybe Int64
       ) of
    (Just eventNum, Just invitationNum) -> Right (toSqlKey eventNum, toSqlKey invitationNum)
    _ -> Left err400 { errBody = "Invalid event or invitation id" }

parseInt64Either :: T.Text -> T.Text -> Either ServerError Int64
parseInt64Either label raw =
  case readMaybe (T.unpack (T.strip raw)) :: Maybe Int64 of
    Just n -> Right n
    Nothing ->
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid " <> label <> " id"))
        }

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

normalizeCurrency :: T.Text -> T.Text
normalizeCurrency raw =
  let cleaned = T.toUpper (T.strip raw)
  in if T.null cleaned then "USD" else cleaned

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
    Just _ -> throwError err403 { errBody = "Only the event organizer can manage tickets" }
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
    pure $ case mArtist of
      Nothing -> ArtistDTO
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
      Just a -> ArtistDTO
        { artistId = Just (renderKeyText (eventArtistArtistId link))
        , artistPartyId = artistProfilePartyId a
        , artistName = artistProfileName a
        , artistGenres = fromMaybe [] (artistProfileGenres a)
        , artistBio = artistProfileBio a
        , artistAvatarUrl = artistProfileAvatarUrl a
        , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
        , artistCreatedAt = Just (artistProfileCreatedAt a)
        , artistUpdatedAt = Just (artistProfileUpdatedAt a)
        }
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
