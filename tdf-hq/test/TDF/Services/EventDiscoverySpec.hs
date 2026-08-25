{-# LANGUAGE OverloadedStrings #-}

module TDF.Services.EventDiscoverySpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad.Logger (runNoLoggingT)
import Data.Pool (destroyAllResources)
import Data.Time (UTCTime(..), addUTCTime, fromGregorian, secondsToDiffTime, utctDay)
import Database.Persist (Entity(..), Filter, count, get, getBy, toPersistValue, update, (=.))
import Database.Persist.Sql (SqlPersistT, rawExecute, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import TDF.Services.EventDiscovery
  ( DiscoveredArtist(..)
  , DiscoveredEvent(..)
  , DiscoveredVenue(..)
  , DiscoverySyncStats(..)
  , EventDiscoveryCity(..)
  , beginEventDiscoveryRun
  , buildTicketmasterRequestUrl
  , countImportedDiscoveryEvents
  , decodeBuenPlanResponse
  , discoveredEventFitsPilotLimit
  , normalizeTicketmasterResponse
  , normalizeUserCities
  , publishedEventTypeLookupParams
  , failEventDiscoveryRun
  , finishEventDiscoveryRun
  , isDiscoveredEventKnown
  , reconcileImportedEvents
  , reconcileProviderEvents
  , syncDiscoveredEvent
  , syncDiscoveredEventDraft
  )
import qualified TDF.Models.SocialEventsModels as Social

spec :: Spec
spec = do
  describe "event discovery event-type lookup" $ do
    it "binds both effective-date placeholders for PostgreSQL" $ do
      let now = fixtureTime 10 0
      publishedEventTypeLookupParams now "other"
        `shouldBe`
          [ toPersistValue ("other" :: T.Text)
          , toPersistValue (utctDay now)
          , toPersistValue (utctDay now)
          ]

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

    it "formats fractional UTC values with the second precision required by Ticketmaster" $ do
      let requestUrl =
            buildTicketmasterRequestUrl
              "https://app.ticketmaster.com/discovery/v2"
              (Just "EC")
              "test-key"
              "Quito"
              (addUTCTime 0.987 (fixtureTime 12 0))
              (addUTCTime 0.321 (fixtureTime 15 0))
              0
      requestUrl `shouldContain` "startDateTime=2026-08-01T12%3A00%3A00Z"
      requestUrl `shouldContain` "endDateTime=2026-08-01T15%3A00%3A00Z"
      requestUrl `shouldNotContain` "%2E987"
      requestUrl `shouldNotContain` "%2E321"

  describe "Buen Plan event normalization" $ do
    it "decodes the live response shape and requires explicit Quito title or slug evidence" $ do
      let now = fixtureTime 10 0
          cities = [EventDiscoveryCity "QUITO" "EC" (Just "America/Guayaquil")]
      case
          decodeBuenPlanResponse
            "USD"
            cities
            now
            (addUTCTime (90 * 86400) now)
            buenPlanFixture
        of
          Right [event] -> do
            discoveredEventExternalId event `shouldBe` "bp-quito"
            discoveredEventTitle event `shouldBe` "Festival Sonoro - Quito"
            discoveredVenueCity (discoveredEventVenue event) `shouldBe` "QUITO"
            discoveredEventTicketUrl event
              `shouldBe` Just "https://www.buenplan.com.ec/event/festival-sonoro-quito"
          Right events -> expectationFailure ("Expected one safe Quito event, got " <> show events)
          Left err -> expectationFailure ("Expected Buen Plan fixture to decode, got " <> T.unpack err)

    it "fails loudly when a non-empty provider page contains no decodable records" $ do
      let now = fixtureTime 10 0
          result =
            decodeBuenPlanResponse
              "USD"
              [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")]
              now
              (addUTCTime (90 * 86400) now)
              "{\"data\":[{\"title\":\"Missing required fields\"}],\"meta\":{\"pageCount\":1}}"
      result `shouldBe` Left "Buen Plan returned no usable event records"

    it "keeps an expired targeted page usable so pagination can continue" $ do
      let now = fixtureTime 10 0
          result =
            decodeBuenPlanResponse
              "USD"
              [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")]
              now
              (addUTCTime (90 * 86400) now)
              buenPlanExpiredFixture
      result `shouldBe` Right []

  describe "Ticketmaster event normalization" $ do
    it "creates a complete graph while ignoring malformed provider records and other cities" $ do
      case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err)
        Right response -> do
          let now = fixtureTime 10 0
              events = normalizeTicketmasterResponse "USD" "Quito" now response
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
          normalizeTicketmasterResponse "USD" "Guayaquil" now response `shouldBe` []

    it "removes the purchase link when the provider reports that sales are closed" $ do
      case eitherDecode (ticketmasterFixtureWithStatus "offsale") of
        Left err -> expectationFailure ("Fixture did not decode: " <> err)
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [event] -> do
              discoveredEventStatus event `shouldBe` "announced"
              discoveredEventTicketUrl event `shouldBe` Nothing
            other -> expectationFailure ("Expected one normalized Quito event, got " <> show other)

    it "upserts the venue, artists, event, and provider references without duplicates" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
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
      _ <-
        syncDiscoveredEvent
          pool
          (fixtureTime 10 12)
          event
            { discoveredEventProvider = "buenplan"
            , discoveredEventExternalId = "bp-event-1"
            , discoveredEventTitle = "Festival Sonoro Actualizado"
            , discoveredEventVenue =
                (discoveredEventVenue event)
                  { discoveredVenueExternalId = "bp-venue-1"
                  }
            , discoveredEventArtists = []
            , discoveredEventTicketUrl = Just "https://www.buenplan.com.ec/event/festival-sonoro"
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
      (venueCount, artistCount, eventCount, eventRefCount) `shouldBe` (2, 1, 1, 2)

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
      -- A provider refresh may omit its lineup temporarily. Preserve artists
      -- already linked by this or another source.
      runSqlPool (count ([] :: [Filter Social.EventArtist])) pool `shouldReturn` 1

      -- Missing from Ticketmaster once keeps both the canonical event and its
      -- Buen Plan purchase option alive.
      _ <-
        reconcileProviderEvents
          pool
          (fixtureTime 10 30)
          "ticketmaster"
          [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")]
          []
      importedEventAfterOneMiss <-
        case importedRef of
          Nothing -> pure Nothing
          Just (Entity _ ref) -> runSqlPool (get (Social.externalEventRefEventId ref)) pool
      case Social.socialEventMetadata =<< importedEventAfterOneMiss of
        Nothing -> expectationFailure "Expected metadata after source reconciliation"
        Just metadata -> metadata `shouldSatisfy` T.isInfixOf "\"isPublic\":true"

      lifecycleChanges <- reconcileImportedEvents pool (fixtureTime 11 0) []
      lifecycleChanges `shouldBe` 1
      importedEventAfterReconcile <-
        case importedRef of
          Nothing -> pure Nothing
          Just (Entity _ ref) -> runSqlPool (get (Social.externalEventRefEventId ref)) pool
      case Social.socialEventMetadata =<< importedEventAfterReconcile of
        Nothing -> expectationFailure "Expected imported event metadata after reconciliation"
        Just metadata -> do
          metadata `shouldSatisfy` (not . T.isInfixOf "eventStatus")
          metadata `shouldSatisfy` T.isInfixOf "\"isPublic\":false"
      (UUID.toText <$> (Social.socialEventWorkflowStateId =<< importedEventAfterReconcile))
        `shouldBe` Just "00000000-0000-4000-8000-000000000237"

    it "reconciles materialization synthetic entity refs with real provider IDs" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            other -> expectationFailure ("Expected one normalized event, got " <> show other) >> fail "invalid normalized fixture"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool

      let providerEvent =
            event
              { discoveredEventVenue =
                  (discoveredEventVenue event)
                    { discoveredVenueCountryCode = Just "EC"
                    }
              }
          synthetic =
            providerEvent
              { discoveredEventVenue =
                  (discoveredEventVenue providerEvent)
                    { discoveredVenueExternalId = "event-research:venue:fixture"
                    }
              , discoveredEventArtists =
                  [ artist
                      { discoveredArtistExternalId = "event-research:artist:fixture"
                      }
                  | artist <- discoveredEventArtists event
                  ]
              }
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 4) synthetic
      realStats <- syncDiscoveredEventDraft pool (fixtureTime 10 5) providerEvent

      discoveryVenuesCreated realStats `shouldBe` 0
      discoveryArtistsCreated realStats `shouldBe` 0
      runSqlPool (count ([] :: [Filter Social.Venue])) pool `shouldReturn` 1
      runSqlPool (count ([] :: [Filter Social.ArtistProfile])) pool `shouldReturn` 1
      runSqlPool (count ([] :: [Filter Social.ExternalVenueRef])) pool `shouldReturn` 2
      runSqlPool (count ([] :: [Filter Social.ExternalArtistRef])) pool `shouldReturn` 2

    it "keeps pilot imports private, idempotent, capped-countable, and timezone-explicit" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            other -> expectationFailure ("Expected one normalized event, got " <> show other) >> fail "invalid normalized fixture"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool

      firstStats <- syncDiscoveredEventDraft pool (fixtureTime 10 5) event
      secondStats <- syncDiscoveredEventDraft pool (fixtureTime 10 10) event

      discoveryEventsCreated firstStats `shouldBe` 1
      discoveryEventsUpdated secondStats `shouldBe` 1
      countImportedDiscoveryEvents pool `shouldReturn` 1
      isDiscoveredEventKnown pool "ticketmaster" "tm-event-1" `shouldReturn` True

      importedRef <-
        runSqlPool
          (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1"))
          pool
      case importedRef of
        Nothing -> expectationFailure "Expected a persisted draft source reference"
        Just (Entity _ ref) -> do
          Social.externalEventRefSourceStatus ref `shouldBe` "draft:on_sale"
          importedEvent <- runSqlPool (get (Social.externalEventRefEventId ref)) pool
          case importedEvent of
            Nothing -> expectationFailure "Expected a persisted draft event"
            Just row -> do
              Social.socialEventTimezone row `shouldBe` Just "America/Guayaquil"
              UUID.toText <$> Social.socialEventWorkflowStateId row
                `shouldBe` Just "00000000-0000-4000-8000-000000000231"
              Social.socialEventMetadata row
                `shouldSatisfy` maybe False (T.isInfixOf "\"isPublic\":false")

    it "keeps draft refreshes private without regressing an existing lifecycle state" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            other -> expectationFailure ("Expected one normalized event, got " <> show other) >> fail "invalid normalized fixture"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool

      _ <- syncDiscoveredEvent pool (fixtureTime 10 5) event
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 10) event
      lifecycleChanges <-
        reconcileImportedEvents
          pool
          (fixtureTime 11 0)
          [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")]
      lifecycleChanges `shouldBe` 0

      importedRef <-
        runSqlPool
          (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1"))
          pool
      case importedRef of
        Nothing -> expectationFailure "Expected a persisted source reference"
        Just (Entity _ ref) -> do
          importedEvent <- runSqlPool (get (Social.externalEventRefEventId ref)) pool
          (UUID.toText <$> (Social.socialEventWorkflowStateId =<< importedEvent))
            `shouldBe` Just "00000000-0000-4000-8000-000000000233"
          (Social.socialEventMetadata =<< importedEvent)
            `shouldSatisfy` maybe False (T.isInfixOf "\"isPublic\":false")

    it "preserves materialization publication holds across auto-publish refreshes" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            other -> expectationFailure ("Expected one normalized event, got " <> show other) >> fail "invalid normalized fixture"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool

      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 5) event
      heldRef <-
        runSqlPool
          (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1"))
          pool
      case heldRef of
        Nothing -> expectationFailure "Expected a persisted source reference"
        Just (Entity refId ref) -> do
          runSqlPool
            ( do
                update refId [Social.ExternalEventRefSourceStatus =. "materialization_draft:on_sale"]
                update
                  (Social.externalEventRefEventId ref)
                  [Social.SocialEventTitle =. "Corrección manual"]
            )
            pool
          _ <-
            reconcileProviderEvents
              pool
              (fixtureTime 10 6)
              "ticketmaster"
              [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")]
              []
          _ <-
            reconcileProviderEvents
              pool
              (fixtureTime 10 7)
              "ticketmaster"
              [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")]
              []
          heldAfterMisses <- runSqlPool (get refId) pool
          Social.externalEventRefSourceStatus <$> heldAfterMisses
            `shouldBe` Just "materialization_draft:on_sale"
          _ <-
            syncDiscoveredEvent
              pool
              (fixtureTime 10 10)
              event
                { discoveredEventTitle = "Título nuevo del proveedor"
                , discoveredEventArtists =
                    discoveredEventArtists event
                      <> [DiscoveredArtist "tm-artist-2" "Artista del refresh" [] Nothing]
                }
          refreshedRef <- runSqlPool (get refId) pool
          Social.externalEventRefSourceStatus <$> refreshedRef
            `shouldBe` Just "materialization_draft:on_sale"
          heldEvent <- runSqlPool (get (Social.externalEventRefEventId ref)) pool
          Social.socialEventTitle <$> heldEvent `shouldBe` Just "Corrección manual"
          (UUID.toText <$> (Social.socialEventWorkflowStateId =<< heldEvent))
            `shouldBe` Just "00000000-0000-4000-8000-000000000231"
          (Social.socialEventMetadata =<< heldEvent)
            `shouldSatisfy` maybe False (T.isInfixOf "\"isPublic\":false")
          runSqlPool (count ([] :: [Filter Social.EventArtist])) pool `shouldReturn` 1

    it "allows a new provider reference to merge when the canonical pilot is full" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            other -> expectationFailure ("Expected one normalized event, got " <> show other) >> fail "invalid normalized fixture"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool

      discoveredEventFitsPilotLimit pool 1 event `shouldReturn` True
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 5) event
      let sameCanonicalEvent =
            event
              { discoveredEventProvider = "buenplan"
              , discoveredEventExternalId = "bp-event-1"
              , discoveredEventArtists = []
              , discoveredEventTicketUrl = Just "https://www.buenplan.com.ec/event/festival-sonoro"
              }
          distinctEvent =
            sameCanonicalEvent
              { discoveredEventExternalId = "bp-distinct-event"
              , discoveredEventTitle = "Festival completamente diferente"
              , discoveredEventStart = addUTCTime (4 * 60 * 60) (discoveredEventStart event)
              , discoveredEventEnd = addUTCTime (4 * 60 * 60) (discoveredEventEnd event)
              }

      discoveredEventFitsPilotLimit pool 1 sameCanonicalEvent `shouldReturn` True
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 10) sameCanonicalEvent
      countImportedDiscoveryEvents pool `shouldReturn` 1
      discoveredEventFitsPilotLimit pool 1 distinctEvent `shouldReturn` False

    it "preserves a subscribed city's configured timezone for imported events and venues" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] ->
              pure
                normalized
                  { discoveredEventVenue =
                      (discoveredEventVenue normalized)
                        { discoveredVenueTimeZone = Just "Pacific/Galapagos"
                        }
                  }
            other ->
              expectationFailure ("Expected one normalized event, got " <> show other)
                >> fail "invalid normalized fixture"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool

      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 5) event
      importedRef <-
        runSqlPool
          (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1"))
          pool
      case importedRef of
        Nothing -> expectationFailure "Expected a persisted draft source reference"
        Just (Entity _ ref) -> do
          importedEvent <- runSqlPool (get (Social.externalEventRefEventId ref)) pool
          case importedEvent of
            Nothing -> expectationFailure "Expected a persisted draft event"
            Just row -> do
              Social.socialEventTimezone row `shouldBe` Just "Pacific/Galapagos"
              case Social.socialEventVenueId row of
                Nothing -> expectationFailure "Expected a persisted draft venue"
                Just venueKey -> do
                  importedVenue <- runSqlPool (get venueKey) pool
                  Social.venueTimezone <$> importedVenue
                    `shouldBe` Just (Just "Pacific/Galapagos")

    it "rejects an unknown provider event type before writing any event graph rows" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            other -> expectationFailure ("Expected one normalized event, got " <> show other) >> fail "invalid normalized fixture"
      withSystemTempFile "tdf-event-discovery.sqlite" $ \databasePath handle -> do
        hClose handle
        pool <- runNoLoggingT $ createSqlitePool (T.pack databasePath) 1
        runSqlPool initializeEventDiscoverySchema pool

        syncDiscoveredEvent
          pool
          (fixtureTime 10 5)
          event{discoveredEventType = "unknown-event-kind"}
          `shouldThrow` anyIOException

        counts <-
          runSqlPool
            ( (,,,)
                <$> count ([] :: [Filter Social.Venue])
                <*> count ([] :: [Filter Social.ArtistProfile])
                <*> count ([] :: [Filter Social.SocialEvent])
                <*> count ([] :: [Filter Social.ExternalEventRef])
            )
            pool
        counts `shouldBe` (0, 0, 0, 0)
        destroyAllResources pool

    it "claims at most one run per provider slot and permits retry after failure" $ do
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool
      let runSlot = fixtureTime 6 0
          nextSlot = fixtureTime 12 0
          now = fixtureTime 10 0
      first <- beginEventDiscoveryRun pool "ticketmaster" runSlot now
      duplicate <- beginEventDiscoveryRun pool "ticketmaster" runSlot now
      isJust first `shouldBe` True
      isNothing duplicate `shouldBe` True
      case first of
        Nothing -> expectationFailure "Expected the first daily run claim"
        Just handle ->
          finishEventDiscoveryRun pool handle now 1 (DiscoverySyncStats 2 1 1 1 1)
      completedClaim <- beginEventDiscoveryRun pool "ticketmaster" runSlot now
      isNothing completedClaim `shouldBe` True

      failedClaim <- beginEventDiscoveryRun pool "ticketmaster" nextSlot now
      case failedClaim of
        Nothing -> expectationFailure "Expected the next day's run claim"
        Just handle -> failEventDiscoveryRun pool handle now "temporary provider failure"
      retriedClaim <- beginEventDiscoveryRun pool "ticketmaster" nextSlot now
      isJust retriedClaim `shouldBe` True

fixtureTime :: Integer -> Integer -> UTCTime
fixtureTime hour minute =
  UTCTime
    (fromGregorian 2026 8 1)
    (secondsToDiffTime (hour * 3600 + minute * 60))

ticketmasterFixture :: BL8.ByteString
ticketmasterFixture = ticketmasterFixtureWithStatus "onsale"

buenPlanFixture :: BL8.ByteString
buenPlanFixture =
  "{\"data\":["
    <> "{\"id\":\"bp-quito\",\"title\":\"Festival Sonoro - Quito\","
    <> "\"description\":\"Una fecha confirmada en Quito.\","
    <> "\"url\":\"festival-sonoro-quito\",\"startDate\":\"2026-08-22T00:00:00.000Z\","
    <> "\"timeZone\":\"America/Guayaquil\",\"currency\":\"USD\",\"sellActive\":true},"
    <> "{\"id\":\"bp-guayaquil\",\"title\":\"Festival Sonoro - Guayaquil\","
    <> "\"description\":\"La gira tambien visita Quito.\","
    <> "\"url\":\"festival-sonoro-guayaquil\",\"startDate\":\"2026-08-23T00:00:00.000Z\","
    <> "\"timeZone\":\"America/Guayaquil\",\"currency\":\"USD\",\"sellActive\":true},"
    <> "{\"title\":\"Malformed item\"}],\"meta\":{\"pageCount\":1}}"

buenPlanExpiredFixture :: BL8.ByteString
buenPlanExpiredFixture =
  "{\"data\":[{"
    <> "\"id\":\"bp-expired-quito\",\"title\":\"Festival pasado - Quito\","
    <> "\"description\":\"Una fecha anterior en Quito.\","
    <> "\"url\":\"festival-pasado-quito\",\"startDate\":\"2026-07-22T00:00:00.000Z\","
    <> "\"timeZone\":\"America/Guayaquil\",\"currency\":\"USD\",\"sellActive\":false"
    <> "}],\"meta\":{\"pageCount\":2}}"

ticketmasterFixtureWithStatus :: BL8.ByteString -> BL8.ByteString
ticketmasterFixtureWithStatus sourceStatus =
  "{\"_embedded\":{\"events\":["
    <> "{\"id\":\"tm-malformed-event\",\"name\":\"Malformed venue event\","
    <> "\"dates\":{\"start\":{\"dateTime\":\"2026-08-01T19:00:00Z\"}},"
    <> "\"_embedded\":{\"venues\":[{\"id\":\"tm-malformed-only-venue\",\"city\":{\"name\":\"Quito\"}}]}},"
    <> "{"
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
    <> "{\"id\":\"tm-malformed-extra-venue\",\"city\":{\"name\":\"Quito\"}},"
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
    "CREATE TABLE workflow_definition (id TEXT PRIMARY KEY, code TEXT NOT NULL UNIQUE, active BOOLEAN NOT NULL)"
    []
  rawExecute
    "CREATE TABLE workflow_state (id TEXT PRIMARY KEY, workflow_id TEXT NOT NULL, code TEXT NOT NULL, name_es TEXT NOT NULL, name_en TEXT NOT NULL, active BOOLEAN NOT NULL)"
    []
  rawExecute
    "CREATE TABLE workflow_transition (id INTEGER PRIMARY KEY, workflow_id TEXT NOT NULL, from_state_id TEXT NOT NULL, to_state_id TEXT NOT NULL, active BOOLEAN NOT NULL, required_permission_id TEXT NULL, requires_review BOOLEAN NOT NULL, requires_distinct_approver BOOLEAN NOT NULL, effective_from TIMESTAMP NULL, effective_until TIMESTAMP NULL)"
    []
  rawExecute
    "CREATE TABLE event_type (id TEXT PRIMARY KEY, catalog_id TEXT NOT NULL, code TEXT NOT NULL UNIQUE, name_es TEXT NOT NULL, name_en TEXT NOT NULL, current_slug TEXT NULL, active BOOLEAN NOT NULL, deprecated_at TIMESTAMP NULL, workflow_state_id TEXT NOT NULL, effective_from DATE NULL, effective_until DATE NULL)"
    []
  rawExecute
    "CREATE TABLE catalog_definition (id TEXT PRIMARY KEY, code TEXT NOT NULL UNIQUE, active BOOLEAN NOT NULL, workflow_id TEXT NOT NULL)"
    []
  rawExecute
    "CREATE TABLE genre (id TEXT PRIMARY KEY, catalog_id TEXT NOT NULL, code TEXT NOT NULL UNIQUE, name_es TEXT NOT NULL, name_en TEXT NOT NULL, active BOOLEAN NOT NULL, workflow_state_id TEXT NOT NULL)"
    []
  rawExecute
    "INSERT INTO workflow_definition (id, code, active) VALUES ('51000000-0000-4000-8000-000000000006', 'catalog-publication', 1), ('00000000-0000-4000-8000-000000000104', 'social-event-lifecycle', 1)"
    []
  rawExecute
    "INSERT INTO workflow_state (id, workflow_id, code, name_es, name_en, active) VALUES ('51000000-0000-4000-8000-000000000001', '51000000-0000-4000-8000-000000000006', 'published', 'Publicado', 'Published', 1), ('00000000-0000-4000-8000-000000000231', '00000000-0000-4000-8000-000000000104', 'planning', 'En planificación', 'Planning', 1), ('00000000-0000-4000-8000-000000000232', '00000000-0000-4000-8000-000000000104', 'announced', 'Anunciado', 'Announced', 1), ('00000000-0000-4000-8000-000000000233', '00000000-0000-4000-8000-000000000104', 'on_sale', 'En venta', 'On sale', 1), ('00000000-0000-4000-8000-000000000234', '00000000-0000-4000-8000-000000000104', 'live', 'En vivo', 'Live', 1), ('00000000-0000-4000-8000-000000000235', '00000000-0000-4000-8000-000000000104', 'postponed', 'Pospuesto', 'Postponed', 1), ('00000000-0000-4000-8000-000000000236', '00000000-0000-4000-8000-000000000104', 'unavailable', 'No disponible', 'Unavailable', 1), ('00000000-0000-4000-8000-000000000237', '00000000-0000-4000-8000-000000000104', 'out_of_scope', 'Fuera de cobertura', 'Out of scope', 1), ('00000000-0000-4000-8000-000000000238', '00000000-0000-4000-8000-000000000104', 'completed', 'Completado', 'Completed', 1), ('00000000-0000-4000-8000-000000000239', '00000000-0000-4000-8000-000000000104', 'cancelled', 'Cancelado', 'Cancelled', 1)"
    []
  rawExecute
    "INSERT INTO workflow_transition (id, workflow_id, from_state_id, to_state_id, active, required_permission_id, requires_review, requires_distinct_approver, effective_from, effective_until) VALUES (1, '00000000-0000-4000-8000-000000000104', '00000000-0000-4000-8000-000000000233', '00000000-0000-4000-8000-000000000237', 1, NULL, 0, 0, NULL, NULL)"
    []
  rawExecute
    "INSERT INTO event_type (id, catalog_id, code, name_es, name_en, current_slug, active, deprecated_at, workflow_state_id, effective_from, effective_until) VALUES ('51000000-0000-4000-8000-000000000002', '51000000-0000-4000-8000-000000000007', 'festival', 'Festival', 'Festival', 'festival', 1, NULL, '51000000-0000-4000-8000-000000000001', NULL, NULL)"
    []
  rawExecute
    "INSERT INTO catalog_definition (id, code, active, workflow_id) VALUES ('51000000-0000-4000-8000-000000000003', 'genres', 1, '51000000-0000-4000-8000-000000000006'), ('51000000-0000-4000-8000-000000000007', 'event-types', 1, '51000000-0000-4000-8000-000000000006')"
    []
  rawExecute
    "INSERT INTO genre (id, catalog_id, code, name_es, name_en, active, workflow_state_id) VALUES ('51000000-0000-4000-8000-000000000004', '51000000-0000-4000-8000-000000000003', 'latin', 'Latina', 'Latin', 1, '51000000-0000-4000-8000-000000000001'), ('51000000-0000-4000-8000-000000000005', '51000000-0000-4000-8000-000000000003', 'latin-pop', 'Pop latino', 'Latin Pop', 1, '51000000-0000-4000-8000-000000000001')"
    []
  rawExecute
    "CREATE TABLE social_artist_profile (id INTEGER PRIMARY KEY, party_id TEXT NULL, name TEXT NOT NULL, bio TEXT NULL, avatar_url TEXT NULL, genres TEXT NULL, social_links TEXT NULL, country_code TEXT NULL, country_id TEXT NULL, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"
    []
  rawExecute
    "CREATE TABLE venue (id INTEGER PRIMARY KEY, name TEXT NOT NULL, address TEXT NULL, city TEXT NULL, country TEXT NULL, country_code TEXT NULL, country_id TEXT NULL, city_id TEXT NULL, timezone TEXT NULL, latitude REAL NULL, longitude REAL NULL, capacity INTEGER NULL, contact TEXT NULL, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"
    []
  rawExecute
    "CREATE TABLE social_event (id INTEGER PRIMARY KEY, organizer_party_id TEXT NULL, title TEXT NOT NULL, description TEXT NULL, venue_id INTEGER NULL, event_type_id TEXT NULL, workflow_state_id TEXT NULL, timezone TEXT NULL, start_time TIMESTAMP NOT NULL, end_time TIMESTAMP NOT NULL, price_cents INTEGER NULL, currency_id TEXT NULL, capacity INTEGER NULL, metadata TEXT NULL, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"
    []
  rawExecute
    "CREATE TABLE external_venue_ref (id INTEGER PRIMARY KEY, provider TEXT NOT NULL, external_id TEXT NOT NULL, venue_id INTEGER NOT NULL, last_seen_at TIMESTAMP NOT NULL, UNIQUE(provider, external_id))"
    []
  rawExecute
    "CREATE TABLE external_artist_ref (id INTEGER PRIMARY KEY, provider TEXT NOT NULL, external_id TEXT NOT NULL, artist_id INTEGER NOT NULL, last_seen_at TIMESTAMP NOT NULL, UNIQUE(provider, external_id))"
    []
  rawExecute
    "CREATE TABLE external_event_ref (id INTEGER PRIMARY KEY, provider TEXT NOT NULL, external_id TEXT NOT NULL, event_id INTEGER NOT NULL, city TEXT NOT NULL, country_code TEXT NULL, source_url TEXT NULL, price_cents INTEGER NULL, currency TEXT NULL, last_seen_at TIMESTAMP NOT NULL, missing_runs INTEGER NOT NULL DEFAULT 0, source_status TEXT NOT NULL DEFAULT 'active', UNIQUE(provider, external_id))"
    []
  rawExecute
    "CREATE TABLE external_event_discovery_run (id INTEGER PRIMARY KEY, provider TEXT NOT NULL, run_date DATE NOT NULL, scheduled_for TIMESTAMP NULL, status TEXT NOT NULL, cities_count INTEGER NOT NULL, events_seen INTEGER NOT NULL, events_created INTEGER NOT NULL, events_updated INTEGER NOT NULL, venues_created INTEGER NOT NULL, artists_created INTEGER NOT NULL, error_message TEXT NULL, started_at TIMESTAMP NOT NULL, finished_at TIMESTAMP NULL, UNIQUE(provider, scheduled_for))"
    []
  rawExecute
    "CREATE TABLE event_discovery_source (id INTEGER PRIMARY KEY, source_key TEXT NOT NULL UNIQUE, name TEXT NOT NULL, source_type TEXT NOT NULL, feed_url TEXT NULL, city_id INTEGER NULL, enabled BOOLEAN NOT NULL DEFAULT 1, priority INTEGER NOT NULL DEFAULT 100, configuration TEXT NULL, etag TEXT NULL, last_modified TEXT NULL, consecutive_failures INTEGER NOT NULL DEFAULT 0, last_success_at TIMESTAMP NULL, last_error TEXT NULL, created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP)"
    []
  rawExecute
    "CREATE TABLE event_artist (event_id INTEGER NOT NULL, artist_id INTEGER NOT NULL, role TEXT NULL, PRIMARY KEY(event_id, artist_id))"
    []
  rawExecute
    "CREATE TABLE artist_genre (artist_id INTEGER NOT NULL, genre TEXT NOT NULL, genre_id TEXT NULL, PRIMARY KEY(artist_id, genre))"
    []
  rawExecute
    "CREATE TABLE artist_genre_membership (artist_id INTEGER NOT NULL, genre_id TEXT NOT NULL, sort_order INTEGER NOT NULL, created_at TIMESTAMP NOT NULL, PRIMARY KEY(artist_id, genre_id))"
    []
