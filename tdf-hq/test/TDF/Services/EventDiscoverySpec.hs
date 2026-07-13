{-# LANGUAGE OverloadedStrings #-}

module TDF.Services.EventDiscoverySpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad.Logger (runNoLoggingT)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Database.Persist (Entity(..), Filter, count, get, getBy)
import Database.Persist.Sql (SqlPersistT, rawExecute, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

import TDF.Services.EventDiscovery
  ( DiscoveredArtist(..)
  , DiscoveredEvent(..)
  , DiscoveredVenue(..)
  , DiscoverySyncStats(..)
  , beginEventDiscoveryRun
  , buildTicketmasterRequestUrl
  , normalizeTicketmasterResponse
  , normalizeUserCities
  , failEventDiscoveryRun
  , finishEventDiscoveryRun
  , reconcileImportedEvents
  , syncDiscoveredEvent
  )
import qualified TDF.Models.SocialEventsModels as Social

spec :: Spec
spec = do
  describe "event discovery user-city targeting" $ do
    it "normalizes, deduplicates, and rejects unsafe profile cities" $ do
      normalizeUserCities
        [ " Quito "
        , "quito"
        , "  Guayaquil  "
        , ""
        , "Cuenca\nSur"
        , "Loja" <> "\x202E"
        ]
        `shouldBe` ["Guayaquil", "Quito"]

  describe "Ticketmaster event discovery requests" $ do
    it "targets one encoded user city, a bounded date window, and an optional country" $ do
      let startsAt = fixtureTime 12 0
          endsAt = fixtureTime 15 0
          requestUrl =
            buildTicketmasterRequestUrl
              "https://app.ticketmaster.com/discovery/v2/"
              (Just "EC")
              "test-key"
              "San Cristóbal"
              startsAt
              endsAt
              2
      requestUrl `shouldContain` "https://app.ticketmaster.com/discovery/v2/events.json?"
      requestUrl `shouldContain` "apikey=test-key"
      requestUrl `shouldContain` "city=San%20Crist%C3%B3bal"
      requestUrl `shouldContain` "countryCode=EC"
      requestUrl `shouldContain` "page=2"
      requestUrl `shouldContain` "includeTBA=no"
      requestUrl `shouldContain` "sort=date%2Casc"

    it "omits the country filter for a multi-country user base" $ do
      let requestUrl =
            buildTicketmasterRequestUrl
              "https://app.ticketmaster.com/discovery/v2"
              Nothing
              "test-key"
              "Helsinki"
              (fixtureTime 12 0)
              (fixtureTime 15 0)
              0
      requestUrl `shouldContain` "city=Helsinki"
      requestUrl `shouldNotContain` "countryCode="

  describe "Ticketmaster event normalization" $ do
    it "creates a complete event graph and filters responses outside the requested city" $ do
      case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err)
        Right response -> do
          let now = fixtureTime 10 0
              events = normalizeTicketmasterResponse "Quito" now response
          case events of
            [event] -> do
              discoveredEventExternalId event `shouldBe` "tm-event-1"
              discoveredEventTitle event `shouldBe` "Festival Sonoro"
              discoveredEventType event `shouldBe` "festival"
              discoveredEventStatus event `shouldBe` "on_sale"
              discoveredEventPriceCents event `shouldBe` Just 2550
              discoveredEventCurrency event `shouldBe` "USD"
              discoveredEventTicketUrl event `shouldBe` Just "https://ticketmaster.example/event/1"
              discoveredEventImageUrl event `shouldBe` Just "https://cdn.example/event-large.jpg"
              discoveredVenueExternalId (discoveredEventVenue event) `shouldBe` "tm-venue-1"
              discoveredVenueName (discoveredEventVenue event) `shouldBe` "Teatro Nacional"
              discoveredVenueCity (discoveredEventVenue event) `shouldBe` "Quito"
              discoveredVenueLatitude (discoveredEventVenue event) `shouldBe` Just (-0.1807)
              map discoveredArtistName (discoveredEventArtists event) `shouldBe` ["La Banda"]
              map discoveredArtistGenres (discoveredEventArtists event) `shouldBe` [["Latin", "Latin Pop"]]
            other -> expectationFailure ("Expected one normalized Quito event, got " <> show other)
          normalizeTicketmasterResponse "Guayaquil" now response `shouldBe` []

    it "removes the purchase link when the provider reports that sales are closed" $ do
      case eitherDecode (ticketmasterFixtureWithStatus "offsale") of
        Left err -> expectationFailure ("Fixture did not decode: " <> err)
        Right response ->
          case normalizeTicketmasterResponse "Quito" (fixtureTime 10 0) response of
            [event] -> do
              discoveredEventStatus event `shouldBe` "announced"
              discoveredEventTicketUrl event `shouldBe` Nothing
            other -> expectationFailure ("Expected one normalized Quito event, got " <> show other)

    it "upserts the venue, artists, event, and provider references without duplicates" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            other -> expectationFailure ("Expected one normalized event, got " <> show other) >> fail "invalid normalized fixture"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool

      firstStats <- syncDiscoveredEvent pool (fixtureTime 10 5) event
      secondStats <-
        syncDiscoveredEvent
          pool
          (fixtureTime 10 10)
          event
            { discoveredEventTitle = "Festival Sonoro Actualizado"
            , discoveredEventArtists =
                [artist{discoveredArtistGenres = []} | artist <- discoveredEventArtists event]
            }

      discoveryEventsCreated firstStats `shouldBe` 1
      discoveryVenuesCreated firstStats `shouldBe` 1
      discoveryArtistsCreated firstStats `shouldBe` 1
      discoveryEventsUpdated secondStats `shouldBe` 1
      discoveryVenuesCreated secondStats `shouldBe` 0
      discoveryArtistsCreated secondStats `shouldBe` 0

      (venueCount, artistCount, eventCount, eventRefCount) <-
        runSqlPool
          ( (,,,)
              <$> count ([] :: [Filter Social.Venue])
              <*> count ([] :: [Filter Social.ArtistProfile])
              <*> count ([] :: [Filter Social.SocialEvent])
              <*> count ([] :: [Filter Social.ExternalEventRef])
          )
          pool
      (venueCount, artistCount, eventCount, eventRefCount) `shouldBe` (1, 1, 1, 1)

      importedRef <-
        runSqlPool
          (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1"))
          pool
      importedEvent <- case importedRef of
        Nothing -> expectationFailure "Expected a persisted Ticketmaster event reference" >> pure Nothing
        Just (Entity _ ref) -> do
          runSqlPool (get (Social.externalEventRefEventId ref)) pool
      Social.socialEventTitle <$> importedEvent
        `shouldBe` Just "Festival Sonoro Actualizado"
      Social.socialEventOrganizerPartyId <$> importedEvent
        `shouldBe` Just (Just "system:event-discovery")

      genreCount <- runSqlPool (count ([] :: [Filter Social.ArtistGenre])) pool
      lineupCount <- runSqlPool (count ([] :: [Filter Social.EventArtist])) pool
      genreCount `shouldBe` 0
      lineupCount `shouldBe` 1

      _ <-
        syncDiscoveredEvent
          pool
          (fixtureTime 10 15)
          event{discoveredEventArtists = []}
      runSqlPool (count ([] :: [Filter Social.EventArtist])) pool `shouldReturn` 0

      lifecycleChanges <- reconcileImportedEvents pool (fixtureTime 11 0) []
      lifecycleChanges `shouldBe` 1
      importedEventAfterReconcile <-
        case importedRef of
          Nothing -> pure Nothing
          Just (Entity _ ref) -> runSqlPool (get (Social.externalEventRefEventId ref)) pool
      case Social.socialEventMetadata =<< importedEventAfterReconcile of
        Nothing -> expectationFailure "Expected imported event metadata after reconciliation"
        Just metadata -> do
          metadata `shouldSatisfy` T.isInfixOf "\"eventStatus\":\"cancelled\""
          metadata `shouldSatisfy` T.isInfixOf "\"isPublic\":false"

    it "claims at most one run per local day and permits retry after failure" $ do
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool
      let runDate = fromGregorian 2026 8 1
          nextDate = fromGregorian 2026 8 2
          now = fixtureTime 10 0
      first <- beginEventDiscoveryRun pool runDate now
      duplicate <- beginEventDiscoveryRun pool runDate now
      isJust first `shouldBe` True
      isNothing duplicate `shouldBe` True
      case first of
        Nothing -> expectationFailure "Expected the first daily run claim"
        Just handle ->
          finishEventDiscoveryRun pool handle now 1 (DiscoverySyncStats 2 1 1 1 1)
      completedClaim <- beginEventDiscoveryRun pool runDate now
      isNothing completedClaim `shouldBe` True

      failedClaim <- beginEventDiscoveryRun pool nextDate now
      case failedClaim of
        Nothing -> expectationFailure "Expected the next day's run claim"
        Just handle -> failEventDiscoveryRun pool handle now "temporary provider failure"
      retriedClaim <- beginEventDiscoveryRun pool nextDate now
      isJust retriedClaim `shouldBe` True

fixtureTime :: Integer -> Integer -> UTCTime
fixtureTime hour minute =
  UTCTime
    (fromGregorian 2026 8 1)
    (secondsToDiffTime (hour * 3600 + minute * 60))

ticketmasterFixture :: BL8.ByteString
ticketmasterFixture = ticketmasterFixtureWithStatus "onsale"

ticketmasterFixtureWithStatus :: BL8.ByteString -> BL8.ByteString
ticketmasterFixtureWithStatus sourceStatus =
  "{\"_embedded\":{\"events\":[{"
    <> "\"id\":\"tm-event-1\",\"name\":\"Festival Sonoro\","
    <> "\"url\":\"http://ticketmaster.example/event/1\","
    <> "\"info\":\"Musica en vivo\","
    <> "\"images\":["
    <> "{\"url\":\"https://cdn.example/event-small.jpg\",\"width\":320,\"height\":180,\"fallback\":false},"
    <> "{\"url\":\"https://cdn.example/event-large.jpg\",\"width\":1280,\"height\":720,\"fallback\":false}],"
    <> "\"dates\":{\"start\":{\"dateTime\":\"2026-08-01T20:00:00Z\"},"
    <> "\"end\":{\"dateTime\":\"2026-08-01T23:00:00Z\"},\"status\":{\"code\":\""
    <> sourceStatus
    <> "\"}},"
    <> "\"sales\":{\"public\":{\"startDateTime\":\"2026-07-01T12:00:00Z\"}},"
    <> "\"priceRanges\":[{\"currency\":\"USD\",\"min\":40},{\"currency\":\"USD\",\"min\":25.5}],"
    <> "\"classifications\":[{\"segment\":{\"name\":\"Music\"},\"genre\":{\"name\":\"Latin\"},\"subGenre\":{\"name\":\"Latin Pop\"}}],"
    <> "\"_embedded\":{\"venues\":["
    <> "{\"id\":\"tm-venue-outside\",\"name\":\"Arena Bogota\",\"city\":{\"name\":\"Bogota\"}},"
    <> "{\"id\":\"tm-venue-1\",\"name\":\"Teatro Nacional\","
    <> "\"url\":\"https://ticketmaster.example/venue/1\",\"address\":{\"line1\":\"Av. Patria\"},"
    <> "\"city\":{\"name\":\"Quito\"},\"state\":{\"name\":\"Pichincha\"},"
    <> "\"country\":{\"name\":\"Ecuador\"},\"postalCode\":\"170143\","
    <> "\"location\":{\"longitude\":\"-78.4678\",\"latitude\":\"-0.1807\"}}],"
    <> "\"attractions\":[{\"id\":\"tm-artist-1\",\"name\":\"La Banda\",\"images\":[]}]}}]},"
    <> "\"page\":{\"totalPages\":1}}"

initializeEventDiscoverySchema :: SqlPersistT IO ()
initializeEventDiscoverySchema = do
  rawExecute
    "CREATE TABLE social_artist_profile (id INTEGER PRIMARY KEY, party_id TEXT NULL, name TEXT NOT NULL, bio TEXT NULL, avatar_url TEXT NULL, genres TEXT NULL, social_links TEXT NULL, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"
    []
  rawExecute
    "CREATE TABLE venue (id INTEGER PRIMARY KEY, name TEXT NOT NULL, address TEXT NULL, city TEXT NULL, country TEXT NULL, latitude REAL NULL, longitude REAL NULL, capacity INTEGER NULL, contact TEXT NULL, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"
    []
  rawExecute
    "CREATE TABLE social_event (id INTEGER PRIMARY KEY, organizer_party_id TEXT NULL, title TEXT NOT NULL, description TEXT NULL, venue_id INTEGER NULL, start_time TIMESTAMP NOT NULL, end_time TIMESTAMP NOT NULL, price_cents INTEGER NULL, capacity INTEGER NULL, metadata TEXT NULL, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"
    []
  rawExecute
    "CREATE TABLE external_venue_ref (id INTEGER PRIMARY KEY, provider TEXT NOT NULL, external_id TEXT NOT NULL, venue_id INTEGER NOT NULL, last_seen_at TIMESTAMP NOT NULL, UNIQUE(provider, external_id))"
    []
  rawExecute
    "CREATE TABLE external_artist_ref (id INTEGER PRIMARY KEY, provider TEXT NOT NULL, external_id TEXT NOT NULL, artist_id INTEGER NOT NULL, last_seen_at TIMESTAMP NOT NULL, UNIQUE(provider, external_id))"
    []
  rawExecute
    "CREATE TABLE external_event_ref (id INTEGER PRIMARY KEY, provider TEXT NOT NULL, external_id TEXT NOT NULL, event_id INTEGER NOT NULL UNIQUE, city TEXT NOT NULL, source_url TEXT NULL, last_seen_at TIMESTAMP NOT NULL, UNIQUE(provider, external_id))"
    []
  rawExecute
    "CREATE TABLE external_event_discovery_run (id INTEGER PRIMARY KEY, provider TEXT NOT NULL, run_date DATE NOT NULL, status TEXT NOT NULL, cities_count INTEGER NOT NULL, events_seen INTEGER NOT NULL, events_created INTEGER NOT NULL, events_updated INTEGER NOT NULL, venues_created INTEGER NOT NULL, artists_created INTEGER NOT NULL, error_message TEXT NULL, started_at TIMESTAMP NOT NULL, finished_at TIMESTAMP NULL, UNIQUE(provider, run_date))"
    []
  rawExecute
    "CREATE TABLE event_artist (event_id INTEGER NOT NULL, artist_id INTEGER NOT NULL, role TEXT NULL, PRIMARY KEY(event_id, artist_id))"
    []
  rawExecute
    "CREATE TABLE artist_genre (artist_id INTEGER NOT NULL, genre TEXT NOT NULL, PRIMARY KEY(artist_id, genre))"
    []
