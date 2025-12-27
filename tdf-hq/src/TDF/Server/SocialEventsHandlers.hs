{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Server.SocialEventsHandlers
  ( socialEventsServer
  , normalizeInvitationStatus
  , parseInvitationIdsEither
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Text.Read (readMaybe)
import           Data.Int (Int64)
import           Data.Time (getCurrentTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Data.Maybe (isNothing, catMaybes)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import           Control.Monad (forM, forM_, when)

import           Servant

-- Pull in full Persistent surface so TH-generated field constructors (EventRsvpEventId, SocialEventStartTime, etc.)
-- are available for filters/updates.
import           Database.Persist
import           Database.Persist.Sql (runSqlPool, fromSqlKey, toSqlKey)

import           TDF.API.SocialEventsAPI
import           TDF.Auth (AuthedUser)
import           TDF.DTO.SocialEventsDTO (EventDTO(..), VenueDTO(..), ArtistDTO(..), ArtistSocialLinksDTO(..), ArtistFollowerDTO(..), ArtistFollowRequest(..), RsvpDTO(..), InvitationDTO(..))
import           TDF.DB (Env(..))
import           TDF.Models.SocialEventsModels hiding (venueAddress, venueCapacity, venueCity, venueContact, venueCountry, venueName)
import qualified TDF.Models.SocialEventsModels as SM

type AppM = ReaderT Env Handler

decodeSocialLinks :: Maybe T.Text -> Maybe ArtistSocialLinksDTO
decodeSocialLinks mTxt = do
  txt <- mTxt
  Aeson.decodeStrict (TE.encodeUtf8 txt)

encodeSocialLinks :: Maybe ArtistSocialLinksDTO -> Maybe T.Text
encodeSocialLinks mLinks =
  fmap (TE.decodeUtf8 . BL.toStrict . Aeson.encode) mLinks

socialEventsServer :: AuthedUser -> ServerT SocialEventsAPI AppM
socialEventsServer _user = eventsServer
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

    listEvents :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> AppM [EventDTO]
    listEvents mCity mStartAfter mArtistId mVenueId = do
      Env{..} <- ask
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
      rows <- liftIO $ runSqlPool (selectList filters [Desc SocialEventStartTime, LimitTo 200]) envPool
      forM rows $ \(Entity eid e) -> do
        artistLinks <- liftIO $ runSqlPool (selectList [EventArtistEventId ==. eid] []) envPool
        artists <- forM artistLinks $ \(Entity _ link) -> do
          mArtist <- liftIO $ runSqlPool (get (eventArtistArtistId link)) envPool
          pure $ case mArtist of
            Nothing -> ArtistDTO
              { artistId = Nothing
              , artistName = "(unknown)"
              , artistGenres = []
              , artistBio = Nothing
              , artistAvatarUrl = Nothing
              , artistSocialLinks = Nothing
              }
            Just a -> ArtistDTO 
              { artistId = Just (T.pack (show (fromSqlKey (eventArtistArtistId link))))
              , artistName = artistProfileName a
              , artistGenres = maybe [] id (artistProfileGenres a)
              , artistBio = artistProfileBio a
              , artistAvatarUrl = artistProfileAvatarUrl a
              , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
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
      when (T.null (T.strip (eventTitle dto))) $ throwError err400 { errBody = "title is required" }
      when (eventStart dto >= eventEnd dto) $ throwError err400 { errBody = "start time must be before end time" }
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
                  Nothing -> ArtistDTO
                    { artistId = Nothing
                    , artistName = "(unknown)"
                    , artistGenres = []
                    , artistBio = Nothing
                    , artistAvatarUrl = Nothing
                    , artistSocialLinks = Nothing
                    }
                  Just a -> ArtistDTO 
                    { artistId = Just (T.pack (show (fromSqlKey (eventArtistArtistId link))))
                    , artistName = artistProfileName a
                    , artistGenres = maybe [] id (artistProfileGenres a)
                    , artistBio = artistProfileBio a
                    , artistAvatarUrl = artistProfileAvatarUrl a
                    , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
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
          liftIO $ runSqlPool (deleteWhere [EventArtistEventId ==. key]) envPool
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
          liftIO $ runSqlPool
            (do
              deleteWhere [EventArtistEventId ==. key]
              deleteWhere [EventRsvpEventId ==. key]
              deleteWhere [EventInvitationEventId ==. key]
              delete key
            )
            envPool
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
    artistsServer = listArtists
               :<|> createArtist
               :<|> getArtist
               :<|> updateArtist
               :<|> listArtistFollowers
               :<|> followArtist
               :<|> unfollowArtist

    listArtists :: Maybe T.Text -> Maybe T.Text -> AppM [ArtistDTO]
    listArtists mNameFilter mGenreFilter = do
      Env{..} <- ask
      let nameFilter = normalizeFilter mNameFilter
          genreFilter = normalizeFilter mGenreFilter
      rows <- liftIO $ runSqlPool (selectList [] [Desc ArtistProfileCreatedAt, LimitTo 500]) envPool
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
            { artistId = Just (T.pack (show (fromSqlKey aid)))
            , artistName = artistProfileName a
            , artistGenres = genreList
            , artistBio = artistProfileBio a
            , artistAvatarUrl = artistProfileAvatarUrl a
            , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
            }
          else Nothing
      pure (catMaybes artists)

    normalizeFilter :: Maybe T.Text -> Maybe T.Text
    normalizeFilter mVal =
      case fmap (T.toCaseFold . T.strip) mVal of
        Nothing -> Nothing
        Just t | T.null t -> Nothing
        Just t -> Just t

    listArtistFollowers :: T.Text -> AppM [ArtistFollowerDTO]
    listArtistFollowers artistIdStr = do
      Env{..} <- ask
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      rows <- liftIO $ runSqlPool
        (selectList [ArtistFollowArtistId ==. artistKey] [Desc ArtistFollowCreatedAt])
        envPool
      let artistIdTxt = T.pack (show (fromSqlKey artistKey))
      pure $ map (\(Entity _ follow) ->
        ArtistFollowerDTO
          { afFollowId = Nothing
          , afArtistId = Just artistIdTxt
          , afFollowerPartyId = artistFollowFollowerPartyId follow
          , afCreatedAt = Just (artistFollowCreatedAt follow)
          }) rows
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
        { artistId = Just (T.pack (show (fromSqlKey key)))
        , artistName = artistName dto
        , artistGenres = genreList
        , artistBio = artistBio dto
        , artistAvatarUrl = artistAvatarUrl dto
        , artistSocialLinks = artistSocialLinks dto
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
                , artistSocialLinks = decodeSocialLinks (artistProfileSocialLinks a)
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
                                          , ArtistProfileSocialLinks =. encodeSocialLinks (artistSocialLinks dto)
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

    followArtist :: T.Text -> ArtistFollowRequest -> AppM ArtistFollowerDTO
    followArtist artistIdStr ArtistFollowRequest{..} = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      let followerParty = T.strip afrFollowerPartyId
      when (T.null followerParty) $ throwError err400 { errBody = "followerPartyId is required" }
      _ <- liftIO $ runSqlPool
        (upsert (ArtistFollow artistKey followerParty now) [ArtistFollowCreatedAt =. now])
        envPool
      pure ArtistFollowerDTO
        { afFollowId = Nothing
        , afArtistId = Just (T.pack (show (fromSqlKey artistKey)))
        , afFollowerPartyId = followerParty
        , afCreatedAt = Just now
        }

    unfollowArtist :: T.Text -> Maybe T.Text -> AppM NoContent
    unfollowArtist artistIdStr mFollower = do
      Env{..} <- ask
      artistKey <- parseArtistId artistIdStr
      mArtist <- liftIO $ runSqlPool (get artistKey) envPool
      when (isNothing mArtist) $ throwError err404 { errBody = "Artist not found" }
      followerParty <- case fmap T.strip mFollower of
        Nothing -> throwError err400 { errBody = "follower query param is required" }
        Just t | T.null t -> throwError err400 { errBody = "follower query param is required" }
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
          
          key <- case existingRsvps of
            [] -> do
              -- Create new RSVP
              liftIO $ runSqlPool (insert EventRsvp
                { eventRsvpEventId = eventKey
                , eventRsvpPartyId = rsvpPartyId dto
                , eventRsvpStatus = rsvpStatus dto
                , eventRsvpMetadata = Nothing
                , eventRsvpCreatedAt = now
                , eventRsvpUpdatedAt = now
                }) envPool
            (Entity existingKey _ : _) -> do
              -- Update existing RSVP
              liftIO $ runSqlPool (update existingKey
                [EventRsvpStatus =. rsvpStatus dto
                , EventRsvpUpdatedAt =. now
                ]) envPool
              pure existingKey
          
          pure dto { rsvpId = Just (T.pack (show (fromSqlKey key))) }

    -- Invitations
    invitationsServer :: ServerT InvitationsRoutes AppM
    invitationsServer eventIdStr =
      listInvitations eventIdStr
        :<|> createInvitation eventIdStr
        :<|> updateInvitation eventIdStr

    listInvitations :: T.Text -> AppM [InvitationDTO]
    listInvitations eventIdStr = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip eventIdStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let eventKey = toSqlKey num :: SocialEventId
          mEvent <- liftIO $ runSqlPool (get eventKey) envPool
          when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
          rows <- liftIO $ runSqlPool (selectList [EventInvitationEventId ==. eventKey] [Desc EventInvitationCreatedAt]) envPool
          pure $ map (\(Entity iid inv) ->
            InvitationDTO
              { invitationId = Just (T.pack (show (fromSqlKey iid)))
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
      case readMaybe (T.unpack (T.strip eventIdStr)) :: Maybe Int64 of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just num -> do
          let eventKey = toSqlKey num :: SocialEventId
          mEvent <- liftIO $ runSqlPool (get eventKey) envPool
          when (isNothing mEvent) $ throwError err404 { errBody = "Event not found" }
          let toParty = T.strip (invitationToPartyId dto)
          when (T.null toParty) $ throwError err400 { errBody = "invitationToPartyId is required" }
          let statusVal = normalizeInvitationStatus (invitationStatus dto)
          key <- liftIO $ runSqlPool (insert EventInvitation
            { eventInvitationEventId = eventKey
            , eventInvitationFromPartyId = fmap T.strip (invitationFromPartyId dto)
            , eventInvitationToPartyId = Just toParty
            , eventInvitationStatus = Just statusVal
            , eventInvitationMessage = invitationMessage dto
            , eventInvitationCreatedAt = now
            , eventInvitationUpdatedAt = now
            }) envPool
          pure InvitationDTO
            { invitationId = Just (T.pack (show (fromSqlKey key)))
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
          let newToParty = T.strip (invitationToPartyId dto)
          let toPartyVal = if T.null newToParty then eventInvitationToPartyId inv else Just newToParty
          liftIO $ runSqlPool (update invitationKey
            [ EventInvitationStatus =. Just statusVal
            , EventInvitationMessage =. messageVal
            , EventInvitationToPartyId =. toPartyVal
            , EventInvitationUpdatedAt =. now
            ]) envPool
          pure InvitationDTO
            { invitationId = Just (T.pack (show (fromSqlKey invitationKey)))
            , invitationEventId = Just (T.strip eventIdStr)
            , invitationFromPartyId = eventInvitationFromPartyId inv
            , invitationToPartyId = maybe "" id toPartyVal
            , invitationStatus = Just statusVal
            , invitationMessage = messageVal
            , invitationCreatedAt = Just (eventInvitationCreatedAt inv)
            , invitationUpdatedAt = Just now
            }

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

-- | Normalize invitation status to a lowercase, non-empty value.
normalizeInvitationStatus :: Maybe T.Text -> T.Text
normalizeInvitationStatus mStatus =
  case fmap (T.toLower . T.strip) mStatus of
    Nothing -> "pending"
    Just s | T.null s -> "pending"
    Just s -> s

-- | Parse event and invitation ids, returning a typed pair or an HTTP 400 error.
parseInvitationIdsEither :: T.Text -> T.Text -> Either ServerError (SocialEventId, EventInvitationId)
parseInvitationIdsEither eventIdStr invitationIdStr =
  case (readMaybe (T.unpack (T.strip eventIdStr)) :: Maybe Int64, readMaybe (T.unpack (T.strip invitationIdStr)) :: Maybe Int64) of
    (Just e, Just i) -> Right (toSqlKey e, toSqlKey i)
    _ -> Left err400 { errBody = "Invalid event or invitation id" }
