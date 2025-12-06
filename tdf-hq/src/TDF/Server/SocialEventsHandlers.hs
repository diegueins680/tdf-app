{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Server.SocialEventsHandlers
  ( socialEventsServer
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import qualified Data.Text as T
import           Text.Read (readMaybe)
import           Data.Int (Int64)
import           Data.Aeson (Value)
import           Data.Time (getCurrentTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Data.Maybe (isNothing)
import           Control.Monad (forM, forM_, when)

import           Servant

-- Pull in full Persistent surface so TH-generated field constructors (EventRsvpEventId, SocialEventStartTime, etc.)
-- are available for filters/updates.
import           Database.Persist
import           Database.Persist.Sql (runSqlPool, fromSqlKey, toSqlKey)

import           TDF.API.SocialEventsAPI
import           TDF.DTO.SocialEventsDTO (EventDTO(..), VenueDTO(..), ArtistDTO(..), RsvpDTO(..))
import           TDF.DB (Env(..))
import           TDF.Models.SocialEventsModels hiding (venueAddress, venueCapacity, venueCity, venueContact, venueCountry, venueName)
import qualified TDF.Models.SocialEventsModels as SM

type AppM = ReaderT Env Handler

socialEventsServer :: ServerT SocialEventsAPI AppM
socialEventsServer = eventsServer
                 :<|> venuesServer
                 :<|> artistsServer
                 :<|> rsvpsServer
                 :<|> invitationsServer
  where
    -- Events
    eventsServer :: ServerT EventsRoutes AppM
    eventsServer = listEvents
               :<|> createEvent
               :<|> getEvent
               :<|> updateEvent
               :<|> deleteEvent

    listEvents :: Maybe T.Text -> Maybe T.Text -> AppM [EventDTO]
    listEvents mCity mStartAfter = do
      Env{..} <- ask
      let startFilter = case mStartAfter of
            Nothing -> []
            Just raw -> case iso8601ParseM (T.unpack raw) of
              Just t  -> [SocialEventStartTime >=. t]
              Nothing -> []
      venueFilter <- case fmap T.strip mCity of
        Nothing -> pure []
        Just "" -> pure []
        Just cityTxt -> do
          venueRows <- liftIO $ runSqlPool (selectList [VenueCity ==. Just cityTxt] []) envPool
          let ids = map entityKey venueRows
          if null ids
            then pure [SocialEventId ==. toSqlKey 0] -- force empty result set
            else pure [SocialEventVenueId <-. map Just ids]
      rows <- liftIO $ runSqlPool (selectList (startFilter ++ venueFilter) [Desc SocialEventStartTime, LimitTo 200]) envPool
      forM rows $ \(Entity eid e) -> do
        artistLinks <- liftIO $ runSqlPool (selectList [EventArtistEventId ==. eid] []) envPool
        artists <- forM artistLinks $ \(Entity _ link) -> do
          mArtist <- liftIO $ runSqlPool (get (eventArtistArtistId link)) envPool
          pure $ case mArtist of
            Nothing -> ArtistDTO { artistId = Nothing, artistName = "(unknown)", artistGenres = [], artistBio = Nothing, artistAvatarUrl = Nothing }
            Just a -> ArtistDTO 
              { artistId = Just (T.pack (show (fromSqlKey (eventArtistArtistId link))))
              , artistName = artistProfileName a
              , artistGenres = maybe [] id (artistProfileGenres a)
              , artistBio = artistProfileBio a
              , artistAvatarUrl = artistProfileAvatarUrl a
              }
        pure EventDTO
          { eventId = Just (T.pack (show (fromSqlKey eid)))
          , eventTitle = socialEventTitle e
          , eventDescription = socialEventDescription e
          , eventStart = socialEventStartTime e
          , eventEnd = socialEventEndTime e
          , eventVenueId = fmap (T.pack . show . fromSqlKey) (socialEventVenueId e)
          , eventPriceCents = socialEventPriceCents e
          , eventCapacity = socialEventCapacity e
          , eventArtists = artists
          }

    createEvent :: EventDTO -> AppM EventDTO
    createEvent dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      mVenueKey <- case eventVenueId dto of
        Nothing -> pure Nothing
        Just txt -> case readMaybe (T.unpack txt) :: Maybe Int64 of
          Nothing -> throwError err400 { errBody = "Invalid venue id" }
          Just vnum -> pure (Just (toSqlKey vnum))
      key <- liftIO $ runSqlPool (insert SocialEvent
        { socialEventOrganizerPartyId = Nothing
        , socialEventTitle = eventTitle dto
        , socialEventDescription = eventDescription dto
        , socialEventVenueId = mVenueKey
        , socialEventStartTime = eventStart dto
        , socialEventEndTime = eventEnd dto
        , socialEventPriceCents = eventPriceCents dto
        , socialEventCapacity = eventCapacity dto
        , socialEventMetadata = Nothing
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
      let createdDto = dto { eventId = Just (T.pack (show (fromSqlKey key))) }
      pure createdDto

    getEvent :: T.Text -> AppM EventDTO
    getEvent rawId = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let key = toSqlKey num :: SocialEventId
          mEnt <- liftIO $ runSqlPool (get key) envPool
          case mEnt of
            Nothing -> throwError err404 { errBody = "Event not found" }
            Just e  -> do
              artistLinks <- liftIO $ runSqlPool (selectList [EventArtistEventId ==. key] []) envPool
              artists <- forM artistLinks $ \(Entity _ link) -> do
                mArtist <- liftIO $ runSqlPool (get (eventArtistArtistId link)) envPool
                pure $ case mArtist of
                  Nothing -> ArtistDTO { artistId = Nothing, artistName = "(unknown)", artistGenres = [], artistBio = Nothing, artistAvatarUrl = Nothing }
                  Just a -> ArtistDTO 
                    { artistId = Just (T.pack (show (fromSqlKey (eventArtistArtistId link))))
                    , artistName = artistProfileName a
                    , artistGenres = maybe [] id (artistProfileGenres a)
                    , artistBio = artistProfileBio a
                    , artistAvatarUrl = artistProfileAvatarUrl a
                    }
              pure $ EventDTO
                { eventId = Just (T.pack (show num))
                , eventTitle = socialEventTitle e
                , eventDescription = socialEventDescription e
                , eventStart = socialEventStartTime e
                , eventEnd = socialEventEndTime e
                , eventVenueId = fmap (T.pack . show . fromSqlKey) (socialEventVenueId e)
                , eventPriceCents = socialEventPriceCents e
                , eventCapacity = socialEventCapacity e
                , eventArtists = artists
                }

    updateEvent :: T.Text -> EventDTO -> AppM EventDTO
    updateEvent rawId dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let key = toSqlKey num :: SocialEventId
          mExisting <- liftIO $ runSqlPool (get key) envPool
          when (isNothing mExisting) $ throwError err404 { errBody = "Event not found" }
          when (T.null (T.strip (eventTitle dto))) $ throwError err400 { errBody = "title is required" }
          when (eventStart dto >= eventEnd dto) $ throwError err400 { errBody = "start time must be before end time" }
          mVenueKey <- case eventVenueId dto of
            Nothing -> pure Nothing
            Just txt -> case readMaybe (T.unpack txt) :: Maybe Int64 of
              Nothing -> throwError err400 { errBody = "Invalid venue id" }
              Just vnum -> pure (Just (toSqlKey vnum))
          liftIO $ runSqlPool (update key
            [ SocialEventTitle =. eventTitle dto
            , SocialEventDescription =. eventDescription dto
            , SocialEventVenueId =. mVenueKey
            , SocialEventStartTime =. eventStart dto
            , SocialEventEndTime =. eventEnd dto
            , SocialEventPriceCents =. eventPriceCents dto
            , SocialEventCapacity =. eventCapacity dto
            , SocialEventUpdatedAt =. now
            ]) envPool
          pure (dto { eventId = Just rawId })

    deleteEvent :: T.Text -> AppM NoContent
    deleteEvent rawId = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let key = toSqlKey num :: SocialEventId
          mExisting <- liftIO $ runSqlPool (get key) envPool
          when (isNothing mExisting) $ throwError err404 { errBody = "Event not found" }
          liftIO $ runSqlPool (delete key) envPool
          pure NoContent

    -- Venues
    venuesServer :: ServerT VenuesRoutes AppM
    venuesServer = listVenues
               :<|> createVenue
               :<|> getVenue
               :<|> updateVenue

    listVenues :: Maybe T.Text -> Maybe T.Text -> AppM [VenueDTO]
    listVenues mCity _mNear = do
      Env{..} <- ask
      let filters = case mCity of
                      Just c | not (T.null (T.strip c)) -> [VenueCity ==. Just (T.strip c)]
                      _ -> []
      rows <- liftIO $ runSqlPool (selectList filters [Asc VenueName, LimitTo 200]) envPool
      pure $ map (\(Entity vid v) -> VenueDTO
        { venueId = Just (T.pack (show (fromSqlKey vid)))
        , venueName = SM.venueName v
        , venueAddress = SM.venueAddress v
        , venueCity = SM.venueCity v
        , venueCountry = SM.venueCountry v
        , venueLat = venueLatitude v
        , venueLng = venueLongitude v
        , venueCapacity = SM.venueCapacity v
        , venueContact = SM.venueContact v
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
      let created = dto { venueId = Just (T.pack (show (fromSqlKey key))) }
      pure created

    getVenue :: T.Text -> AppM VenueDTO
    getVenue rawId = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid venue id" }
        Just num -> do
          let key = toSqlKey num :: VenueId
          mEnt <- liftIO $ runSqlPool (get key) envPool
          case mEnt of
            Nothing -> throwError err404 { errBody = "Venue not found" }
            Just v -> pure VenueDTO
              { venueId = Just (T.pack (show num))
              , venueName = SM.venueName v
              , venueAddress = SM.venueAddress v
              , venueCity = SM.venueCity v
              , venueCountry = SM.venueCountry v
              , venueLat = venueLatitude v
              , venueLng = venueLongitude v
              , venueCapacity = SM.venueCapacity v
              , venueContact = SM.venueContact v
              }

    updateVenue :: T.Text -> VenueDTO -> AppM VenueDTO
    updateVenue rawId dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      case readMaybe (T.unpack (T.strip rawId)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid venue id" }
        Just num -> do
          let key = toSqlKey num :: VenueId
          mExisting <- liftIO $ runSqlPool (get key) envPool
          when (isNothing mExisting) $ throwError err404 { errBody = "Venue not found" }
          liftIO $ runSqlPool (update key
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
          pure (dto { venueId = Just rawId })

    -- Artists
    artistsServer :: ServerT ArtistsRoutes AppM
    artistsServer = listArtists :<|> createArtist :<|> getArtist :<|> updateArtist

    listArtists :: Maybe T.Text -> Maybe T.Text -> AppM [ArtistDTO]
    listArtists mNameFilter _mGenreFilter = do
      Env{..} <- ask
      rows <- liftIO $ runSqlPool (selectList [] [Desc ArtistProfileCreatedAt, LimitTo 500]) envPool
      let filtered = case mNameFilter of
            Nothing -> rows
            Just name -> filter (\(Entity _ a) -> T.isInfixOf name (artistProfileName a)) rows
      forM filtered $ \(Entity aid a) -> do
        genres <- liftIO $ runSqlPool (selectList [ArtistGenreArtistId ==. aid] []) envPool
        pure ArtistDTO
          { artistId = Just (T.pack (show (fromSqlKey aid)))
          , artistName = artistProfileName a
          , artistGenres = map (artistGenreGenre . entityVal) genres
          , artistBio = artistProfileBio a
          , artistAvatarUrl = artistProfileAvatarUrl a
          }

    createArtist :: ArtistDTO -> AppM ArtistDTO
    createArtist dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      key <- liftIO $ runSqlPool (insert ArtistProfile
        { artistProfilePartyId = Nothing
        , artistProfileName = artistName dto
        , artistProfileBio = artistBio dto
        , artistProfileAvatarUrl = artistAvatarUrl dto
        , artistProfileGenres = Just (artistGenres dto)
        , artistProfileSocialLinks = Nothing
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
        { artistId = Just (T.pack (show (fromSqlKey key)))
        , artistName = artistName dto
        , artistGenres = genreList
        , artistBio = artistBio dto
        , artistAvatarUrl = artistAvatarUrl dto
        }

    getArtist :: T.Text -> AppM ArtistDTO
    getArtist idStr = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip idStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid artist id" }
        Just num -> do
          let key = toSqlKey num :: ArtistProfileId
          mArtist <- liftIO $ runSqlPool (get key) envPool
          case mArtist of
            Nothing -> throwError err404 { errBody = "Artist not found" }
            Just a -> do
              genres <- liftIO $ runSqlPool (selectList [ArtistGenreArtistId ==. key] []) envPool
              pure ArtistDTO
                { artistId = Just (T.strip idStr)
                , artistName = artistProfileName a
                , artistGenres = map (artistGenreGenre . entityVal) genres
                , artistBio = artistProfileBio a
                , artistAvatarUrl = artistProfileAvatarUrl a
                }

    updateArtist :: T.Text -> ArtistDTO -> AppM ArtistDTO
    updateArtist idStr dto = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip idStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid artist id" }
        Just num -> do
          let key = toSqlKey num :: ArtistProfileId
          now <- liftIO getCurrentTime
          liftIO $ runSqlPool (update key [ArtistProfileName =. artistName dto
                                          , ArtistProfileBio =. artistBio dto
                                          , ArtistProfileAvatarUrl =. artistAvatarUrl dto
                                          , ArtistProfileGenres =. Just (artistGenres dto)
                                          , ArtistProfileUpdatedAt =. now
                                          ]) envPool
          -- Update genres
          liftIO $ runSqlPool (deleteWhere [ArtistGenreArtistId ==. key]) envPool
          liftIO $ runSqlPool
            (forM_ (artistGenres dto) $ \g ->
               insert_ ArtistGenre
                 { artistGenreArtistId = key
                 , artistGenreGenre = g
                 }
            )
            envPool
          pure dto { artistId = Just (T.strip idStr) }

    -- RSVPs
    rsvpsServer :: ServerT RsvpRoutes AppM
    rsvpsServer = listRsvps :<|> createRsvp

    listRsvps :: T.Text -> AppM [RsvpDTO]
    listRsvps eventIdStr = do
      Env{..} <- ask
      case readMaybe (T.unpack eventIdStr) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let eventKey = toSqlKey num :: SocialEventId
          rsvpRows <- liftIO $ runSqlPool (selectList [EventRsvpEventId ==. eventKey] []) envPool
          pure $ map (\(Entity rid rsvp) -> RsvpDTO
            { rsvpId = Just (T.pack (show (fromSqlKey rid)))
            , rsvpEventId = eventIdStr
            , rsvpPartyId = eventRsvpPartyId rsvp
            , rsvpStatus = eventRsvpStatus rsvp
            , rsvpCreatedAt = Just (eventRsvpCreatedAt rsvp)
            }) rsvpRows

    createRsvp :: T.Text -> RsvpDTO -> AppM RsvpDTO
    createRsvp eventIdStr dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      case readMaybe (T.unpack eventIdStr) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let eventKey = toSqlKey num :: SocialEventId
          -- Verify event exists
          mEvent <- liftIO $ runSqlPool (get eventKey) envPool
          when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
          
          -- Check if RSVP already exists for this party/event
          existingRsvps <- liftIO $ runSqlPool 
            (selectList [EventRsvpEventId ==. eventKey, EventRsvpPartyId ==. rsvpPartyId dto] [])
            envPool
          
          key <- if null existingRsvps
            then do
              -- Create new RSVP
              liftIO $ runSqlPool (insert EventRsvp
                { eventRsvpEventId = eventKey
                , eventRsvpPartyId = rsvpPartyId dto
                , eventRsvpStatus = rsvpStatus dto
                , eventRsvpMetadata = Nothing
                , eventRsvpCreatedAt = now
                , eventRsvpUpdatedAt = now
                }) envPool
            else do
              -- Update existing RSVP
              let (Entity existingKey _) = head existingRsvps
              liftIO $ runSqlPool (update existingKey 
                [EventRsvpStatus =. rsvpStatus dto
                , EventRsvpUpdatedAt =. now
                ]) envPool
              pure existingKey
          
          pure dto { rsvpId = Just (T.pack (show (fromSqlKey key))) }

    -- Invitations (not implemented)
    invitationsServer :: ServerT InvitationsRoutes AppM
    invitationsServer = createInvitation
    createInvitation :: T.Text -> Value -> AppM Value
    createInvitation _ _ = throwError err501 { errBody = "Not implemented: createInvitation" }
