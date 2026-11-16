{-# LANGUAGE OverloadedStrings #-}

module TDF.Services.EventDiscoverySpec (spec) where

import Data.Aeson (Value(..), eitherDecode, encode, decodeStrict')
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad (forM_)
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
import Test.QuickCheck (property, forAll, chooseInt, quickCheckResult, isSuccess)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import TDF.Services.EventDiscovery
  ( DiscoveredArtist(..)
  , DiscoveredEvent(..)
  , DiscoveredVenue(..)
  , DiscoverySyncStats(..)
  , EventDiscoveryCity(..)
  , eventDiscoveryDailySlot
  , eventDiscoveryFullReconciliation
  , ticketmasterNextPage
  , buenPlanNextPage
  , beginEventDiscoveryRun
  , buildTicketmasterRequestUrl
  , countImportedDiscoveryEvents
  , decodeBuenPlanResponse
  , discoveredEventFitsPilotLimit
  , normalizeTicketmasterResponse
  , normalizeUserCities
  , loadRegistryDiscoveryCities
  , StructuredFeedEvent(..)
  , classifyDiscoveredEventType
  , parseIcsEvents
  , publishedEventTypeLookupParams
  , failEventDiscoveryRun
  , finishEventDiscoveryRun
  , isDiscoveredEventKnown
  , reconcileImportedEvents
  , reconcileProviderEvents
  , preserveEventOwnership
  , discoveredEventPublicationReady
  , syncDiscoveredEvent
  , syncDiscoveredEventDraft
  )
import TDF.Services.EventDiscoveryLinkedData
  ( LinkedDataSourceConfig(..)
  , linkedDataEventLinks
  , normalizeLinkedDataDocument
  , parseLinkedDataSourceConfig
  , parseLinkedDataTime
  )
import qualified TDF.Models.SocialEventsModels as Social

spec :: Spec
spec = do
  describe "daily Ecuador discovery schedule" $ do
    it "converts 06:00 Guayaquil to 11:00 UTC and catches up only the latest slot" $ do
      let day = fromGregorian 2026 9 20
          at hour = UTCTime day (secondsToDiffTime (hour * 3600))
      eventDiscoveryDailySlot 6 (at 11) `shouldBe` at 11
      eventDiscoveryDailySlot 6 (at 10) `shouldBe` addUTCTime (-86400) (at 11)
      eventDiscoveryDailySlot 6 (at 23) `shouldBe` at 11
      eventDiscoveryFullReconciliation (at 11) `shouldBe` True
      eventDiscoveryFullReconciliation (addUTCTime (-86400) (at 11)) `shouldBe` False
    it "always selects one past due slot less than one day old" $ property $
      forAll (chooseInt (0, 23)) $ \hour ->
      forAll (chooseInt (0, 86400 * 40)) $ \seconds ->
        let now = addUTCTime (fromIntegral seconds) (fixtureTime 0 0)
            slot = eventDiscoveryDailySlot hour now
         in slot <= now && now < addUTCTime 86400 slot
    it "does not mistake a capped page prefix for complete inventory" $ do
      ticketmasterNextPage 5 4 6 `shouldBe` Left "Ticketmaster pagination budget exhausted; inventory incomplete"
      ticketmasterNextPage 5 4 5 `shouldBe` Right Nothing
      ticketmasterNextPage 5 0 5 `shouldBe` Right (Just 1)
    it "distinguishes a complete Buen Plan tenth page from a truncated inventory" $ do
      buenPlanNextPage 10 10 `shouldBe` Right Nothing
      buenPlanNextPage 9 11 `shouldBe` Right (Just 10)
      buenPlanNextPage 10 11 `shouldBe` Left "Buen Plan pagination budget exhausted; inventory incomplete"
      buenPlanNextPage 0 1 `shouldBe` Left "Invalid Buen Plan pagination metadata"
    it "never accepts incomplete inventories at the configured budget" $ property $
      forAll (chooseInt (1, 10)) $ \budget ->
      forAll (chooseInt (1, 500)) $ \remaining ->
        case ticketmasterNextPage budget (budget - 1) (budget + remaining) of
          Left _ -> True
          Right _ -> False

  describe "registry discovery coverage" $ do
    it "includes registered Ecuador and Colombia cities without subscriptions and prioritizes Quito" $
      withSystemTempFile "tdf-discovery-cities.sqlite" $ \dbPath handle -> do
        hClose handle
        pool <- runNoLoggingT $ createSqlitePool (T.pack dbPath) 1
        runSqlPool (do
          rawExecute "CREATE TABLE event_city(name TEXT,normalized_name TEXT,country_code TEXT,time_zone TEXT)" []
          rawExecute "INSERT INTO event_city VALUES('Loja','loja','EC','America/Guayaquil'),('Quito','quito','EC','America/Guayaquil'),('Madrid','madrid','ES','Europe/Madrid'),('Bogotá','bogotá','CO','America/Bogota')" []
          ) pool
        cities <- loadRegistryDiscoveryCities pool
        map eventDiscoveryCityName cities `shouldBe` ["Quito","Loja","Bogotá"]
        destroyAllResources pool

  describe "venue iCalendar feeds" $ do
    let calendar =
          T.unlines
            [ "BEGIN:VCALENDAR"
            , "BEGIN:VEVENT"
            , "DTSTART;TZID=America/Guayaquil:20261130T200000"
            , "DTEND;TZID=America/Guayaquil:20261130T220000"
            , "UID:281552-1@teatrosucre.com"
            , "SUMMARY:Orquesta de Instrumentos Andinos"
            , "URL:https://teatrosucre.com/event/oia/"
            , "CATEGORIES:Danza,Música"
            , "ATTACH;FMTTYPE=image/png:https://cdn.teatrosucre.com/oia.png"
            , "END:VEVENT"
            , "BEGIN:VEVENT"
            , "DTSTART:20261201T193000"
            , "UID:floating-2@teatrosucre.com"
            , "SUMMARY:Lectura dramatizada"
            , "LOCATION:Teatro Variedades\\, Plaza del Teatro\\, Quito"
            , "END:VEVENT"
            , "BEGIN:VEVENT"
            , "DTSTART;TZID=Europe/Madrid:20261201T193000"
            , "UID:unknown-zone-3@teatrosucre.com"
            , "SUMMARY:Zona sin regla fija"
            , "END:VEVENT"
            , "BEGIN:VEVENT"
            , "DTSTART;VALUE=DATE:20261205"
            , "UID:all-day-4@teatrosucre.com"
            , "SUMMARY:Abonos"
            , "END:VEVENT"
            , "END:VCALENDAR"
            ]
        events = parseIcsEvents "Teatro Nacional Sucre" (Just "America/Guayaquil") calendar
    it "resolves TZID and floating local times in the feed city's zone and drops unresolvable ones" $ do
      map structuredEventId events `shouldBe` ["281552-1@teatrosucre.com", "floating-2@teatrosucre.com"]
      map structuredEventStart events
        `shouldBe` [ UTCTime (fromGregorian 2026 12 1) (secondsToDiffTime 3600)
                   , UTCTime (fromGregorian 2026 12 2) (secondsToDiffTime 1800)
                   ]
      map structuredEventEnd events
        `shouldBe` [Just (UTCTime (fromGregorian 2026 12 1) (secondsToDiffTime 10800)), Nothing]
    it "names the venue from the source when LOCATION is absent and maps categories to catalog types" $ do
      map structuredEventVenue events `shouldBe` ["Teatro Nacional Sucre", "Teatro Variedades"]
      map structuredEventType events `shouldBe` [Just "concert", Just "other"]
      map structuredEventImageUrl events `shouldBe` [Just "https://cdn.teatrosucre.com/oia.png", Nothing]
    it "keeps explicit UTC values unchanged" $
      map structuredEventStart
        (parseIcsEvents "Venue" Nothing "BEGIN:VEVENT\nDTSTART:20261201T010000Z\nUID:utc-1\nSUMMARY:UTC\nEND:VEVENT\n")
        `shouldBe` [UTCTime (fromGregorian 2026 12 1) (secondsToDiffTime 3600)]

  describe "schema.org linked-data sources" $ do
    let now = UTCTime (fromGregorian 2026 11 14) 0
        quito = EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")
        cuenca = EventDiscoveryCity "Cuenca" "EC" (Just "America/Guayaquil")
        bogota = EventDiscoveryCity "Bogotá" "CO" (Just "America/Bogota")
        medellin = EventDiscoveryCity "Medellín" "CO" (Just "America/Bogota")
        cities = [quito, cuenca, bogota, medellin]
        platform = LinkedDataSourceConfig Nothing Nothing Nothing 60 Nothing Nothing
        at day seconds = UTCTime day (secondsToDiffTime seconds)
        block json = "<script type=\"application/ld+json\">" <> json <> "</script>"
        normalize config fixedCity isEventPage =
          normalizeLinkedDataDocument 365 "USD" "source" config fixedCity cities now isEventPage
    it "parses offsets with and without a colon, UTC, and venue-local times" $ do
      parseLinkedDataTime Nothing "2026-12-11T20:00:00-05:00" `shouldBe` Just (at (fromGregorian 2026 12 12) 3600)
      parseLinkedDataTime Nothing "2026-12-11T20:00:00-0500" `shouldBe` Just (at (fromGregorian 2026 12 12) 3600)
      parseLinkedDataTime Nothing "2026-11-20T20:00-05:00" `shouldBe` Just (at (fromGregorian 2026 11 21) 3600)
      parseLinkedDataTime Nothing "2026-12-11T20:00:00Z" `shouldBe` Just (at (fromGregorian 2026 12 11) 72000)
      parseLinkedDataTime (Just "America/Guayaquil") "2026-11-14T20:30:00" `shouldBe` Just (at (fromGregorian 2026 11 15) 5400)
    it "rejects date-only values and local times in a zone without a fixed offset" $ do
      parseLinkedDataTime (Just "America/Bogota") "2026-11-28" `shouldBe` Nothing
      parseLinkedDataTime Nothing "2026-11-14T20:30:00" `shouldBe` Nothing
      parseLinkedDataTime (Just "Europe/Madrid") "2026-11-14T20:30:00" `shouldBe` Nothing
    it "validates the source configuration" $ do
      parseLinkedDataSourceConfig Nothing `shouldBe` Right platform
      fmap linkedDataMaxPages (parseLinkedDataSourceConfig (Just "{\"linkPrefix\":\"https://a.example/e/\",\"maxPages\":300}"))
        `shouldBe` Right 300
      parseLinkedDataSourceConfig (Just "{\"maxPages\":401}") `shouldSatisfy` isLeftText
      parseLinkedDataSourceConfig (Just "{\"linkPrefix\":\"http://a.example/e/\"}") `shouldSatisfy` isLeftText
      parseLinkedDataSourceConfig (Just "{\"rewriteFrom\":\"/ev/\"}") `shouldSatisfy` isLeftText
      parseLinkedDataSourceConfig (Just "not json") `shouldSatisfy` isLeftText
    it "follows only sitemap locations under the prefix and applies the page rewrite" $ do
      let config = platform
            { linkedDataLinkPrefix = Just "https://www.meet2go.com/ev/"
            , linkedDataRewriteFrom = Just "https://www.meet2go.com/ev/"
            , linkedDataRewriteTo = Just "https://www.meet2go.com/seo/ev/"
            }
          sitemap =
            "<?xml version=\"1.0\"?><urlset><url><loc>https://www.meet2go.com/</loc></url>"
              <> "<url><loc>https://www.meet2go.com/ev/uno</loc><image:image><image:loc>https://cdn.example/a</image:loc></image:image></url>"
              <> "<url><loc>https://www.meet2go.com/ev/dos</loc></url>"
              <> "<url><loc>https://www.meet2go.com/ev/uno</loc></url>"
              <> "<url><loc>https://www.meet2go.com/organizer/x</loc></url></urlset>"
      linkedDataEventLinks config "https://www.meet2go.com/seo/sitemap.xml" sitemap
        `shouldBe` ["https://www.meet2go.com/seo/ev/uno", "https://www.meet2go.com/seo/ev/dos"]
    it "resolves root-relative listing links against the index origin and ignores other hosts" $ do
      let config = platform { linkedDataLinkPrefix = Just "https://www.tuboleta.com/es/eventos/" }
          listing =
            "<a href=\"/es/eventos/uno-bogota\">1</a><a href=\"#eventos\">x</a>"
              <> "<a href=\"/es/eventos/uno-bogota#boletas\">1</a>"
              <> "<a href='https://www.tuboleta.com/es/eventos/dos'>2</a>"
              <> "<a href=\"https://evil.example/es/eventos/tres\">3</a>"
              <> "<a href=\"/es/venue/otro\">v</a><a href=\"../eventos/relativo\">r</a>"
      linkedDataEventLinks config "https://www.tuboleta.com/es/venue/daviarena?x=1" listing
        `shouldBe` ["https://www.tuboleta.com/es/eventos/uno-bogota", "https://www.tuboleta.com/es/eventos/dos"]
      linkedDataEventLinks platform "https://www.tuboleta.com/es/venue/daviarena" listing `shouldBe` []
    it "imports a platform event only when it names a registered city in the matching country" $ do
      let page locality country =
            block
              ( "{\"@context\":\"https://schema.org\",\"@type\":\"Event\",\"name\":\"Noche de jazz\","
                  <> "\"startDate\":\"2026-11-20T19:30:00-05:00\",\"endDate\":\"2026-11-20T21:30:00-05:00\","
                  <> "\"eventStatus\":\"https://schema.org/EventScheduled\","
                  <> "\"location\":{\"@type\":\"Place\",\"name\":\"MicroTeatro UIO\",\"address\":{\"@type\":\"PostalAddress\","
                  <> "\"streetAddress\":\"Yánez Pinzón N25-106\",\"addressLocality\":\"" <> locality <> "\",\"addressCountry\":\"" <> country <> "\"}},"
                  <> "\"image\":[\"https://cdn.example/jazz.png\"],"
                  <> "\"offers\":[{\"@type\":\"Offer\",\"url\":\"https://www.meet2go.com/ev/jazz\",\"price\":\"12.5\",\"priceCurrency\":\"USD\",\"availability\":\"https://schema.org/InStock\"},"
                  <> "{\"@type\":\"Offer\",\"url\":\"https://www.meet2go.com/ev/jazz\",\"price\":\"6\",\"priceCurrency\":\"USD\",\"availability\":\"https://schema.org/InStock\"}]}"
              )
          url = "https://www.meet2go.com/seo/ev/jazz"
      case normalize platform Nothing True url (page "Quito" "EC") of
        [event] -> do
          discoveredEventExternalId event `shouldBe` "jazz"
          discoveredEventStart event `shouldBe` at (fromGregorian 2026 11 21) 1800
          discoveredEventEnd event `shouldBe` Just (at (fromGregorian 2026 11 21) 9000)
          discoveredVenueName (discoveredEventVenue event) `shouldBe` "MicroTeatro UIO"
          discoveredVenueCity (discoveredEventVenue event) `shouldBe` "Quito"
          discoveredVenueCountryCode (discoveredEventVenue event) `shouldBe` Just "EC"
          discoveredEventPriceCents event `shouldBe` Just 600
          discoveredEventCurrency event `shouldBe` "USD"
          discoveredEventTicketUrl event `shouldBe` Just "https://www.meet2go.com/ev/jazz"
          discoveredEventImageUrl event `shouldBe` Just "https://cdn.example/jazz.png"
          discoveredEventStatus event `shouldBe` "on_sale"
          discoveredEventType event `shouldBe` "other"
        other -> expectationFailure ("expected one event, got " <> show (length other))
      normalize platform Nothing True url (page "Ambato" "EC") `shouldBe` []
      normalize platform Nothing True url (page "Cuenca" "ES") `shouldBe` []
      map (discoveredVenueCity . discoveredEventVenue) (normalize platform Nothing True url (page "Cuenca" "Ecuador"))
        `shouldBe` ["Cuenca"]
    it "matches a platform city from an unaccented street address" $ do
      let page =
            block
              ( "{\"@type\":\"Event\",\"name\":\"Jeff Mills\",\"startDate\":\"2026-11-21T20:00:00-05:00\","
                  <> "\"location\":{\"@type\":\"Place\",\"name\":\"Plaza Mayor\",\"address\":{\"@type\":\"PostalAddress\",\"streetAddress\":\"Cl. 41 #55-80, Medellin\"}},"
                  <> "\"performer\":[{\"@type\":\"PerformingGroup\",\"name\":\"Jeff Mills\"}]}"
              )
      case normalize platform Nothing True "https://latiquetera.com/event/jeff-mills" page of
        [event] -> do
          discoveredVenueCity (discoveredEventVenue event) `shouldBe` "Medellín"
          discoveredEventCurrency event `shouldBe` "COP"
          map discoveredArtistName (discoveredEventArtists event) `shouldBe` ["Jeff Mills"]
          discoveredEventTicketUrl event `shouldBe` Just "https://latiquetera.com/event/jeff-mills"
        other -> expectationFailure ("expected one event, got " <> show (length other))
    it "binds a venue source to its city, names the venue from configuration, and flags date-only pages" $ do
      let config = platform
            { linkedDataVenueName = Just "DAVIarena"
            , linkedDataDateOnlyLocalTime = Just "07:00"
            }
          dateOnlyPage =
            block
              ( "{\"@context\":\"https://schema.org\",\"@graph\":[{\"@type\":\"Event\",\"name\":\"LA MARRANADA DEL SOCIAL\","
                  <> "\"image\":{\"@type\":\"ImageObject\",\"url\":\"http://www.tuboleta.com/files/a.jpg\"},"
                  <> "\"startDate\":\"2026-12-19T07:00:00-0500\",\"location\":{\"@type\":\"City\",\"name\":\"Medellín\"}}]}"
              )
          richPage =
            block
              ( "{\"@type\":\"MusicEvent\",\"name\":\"Alcolirykoz - El Remate 2026\",\"startDate\":\"2026-12-11T20:00:00-05:00\","
                  <> "\"location\":{\"@type\":\"Place\",\"name\":\"DAVIarena\",\"address\":{\"@type\":\"PostalAddress\",\"streetAddress\":\"Cra. 49 #52 Sur-200\",\"addressLocality\":\"Sabaneta\",\"addressCountry\":\"CO\"}},"
                  <> "\"offers\":{\"@type\":\"AggregateOffer\",\"priceCurrency\":\"COP\",\"lowPrice\":\"85400\",\"highPrice\":\"419700\",\"url\":\"https://checkout.tuboleta.com/selection/event/1\"},"
                  <> "\"performer\":{\"@type\":\"MusicGroup\",\"name\":\"Alcolirykoz\"}}"
              )
      case normalize config (Just medellin) True "https://www.tuboleta.com/es/eventos/la-marranada-del-social" dateOnlyPage of
        [event] -> do
          discoveredVenueName (discoveredEventVenue event) `shouldBe` "DAVIarena"
          discoveredVenueCity (discoveredEventVenue event) `shouldBe` "Medellín"
          discoveredEventDescription event `shouldBe` Just "Hora por confirmar: la fuente publica solo la fecha."
          discoveredEventImageUrl event `shouldBe` Just "https://www.tuboleta.com/files/a.jpg"
          discoveredEventStatus event `shouldBe` "announced"
        other -> expectationFailure ("expected one event, got " <> show (length other))
      case normalize config (Just medellin) True "https://www.tuboleta.com/es/eventos/alcolirykoz-el-remate-2026-medellin" richPage of
        [event] -> do
          discoveredEventExternalId event `shouldBe` "alcolirykoz-el-remate-2026-medellin"
          discoveredVenueCity (discoveredEventVenue event) `shouldBe` "Medellín"
          discoveredVenueAddress (discoveredEventVenue event) `shouldBe` Just "Cra. 49 #52 Sur-200"
          discoveredEventPriceCents event `shouldBe` Just 8540000
          discoveredEventCurrency event `shouldBe` "COP"
          discoveredEventDescription event `shouldBe` Nothing
          discoveredEventType event `shouldBe` "concert"
          discoveredEventTicketUrl event `shouldBe` Just "https://checkout.tuboleta.com/selection/event/1"
        other -> expectationFailure ("expected one event, got " <> show (length other))
      normalize platform (Just medellin) True "https://www.tuboleta.com/es/eventos/x" dateOnlyPage `shouldBe` []
    it "reads several events from a listing page with venue-local times and distinct identities" $ do
      let config = platform { linkedDataVenueName = Just "Teatro Centro de Arte" }
          guayaquil = EventDiscoveryCity "Guayaquil" "EC" (Just "America/Guayaquil")
          listing =
            block "{\"@type\":\"NGO\",\"name\":\"Fundación\"}"
              <> block
                ( "[{\"@type\":\"Event\",\"name\":\"Los Panchos\",\"startDate\":\"2026-11-20T20:00:00\",\"location\":{\"@id\":\"https://t.example/#theater\"}},"
                    <> "{\"@type\":\"Event\",\"name\":\"El Cascanueces\",\"startDate\":\"2026-11-22T16:30:00\",\"eventStatus\":\"https://schema.org/EventCancelled\",\"location\":{\"@id\":\"https://t.example/#theater\"}},"
                    <> "{\"@type\":\"Event\",\"name\":\"Función pasada\",\"startDate\":\"2026-11-01T16:30:00\",\"location\":{\"@id\":\"https://t.example/#theater\"}}]"
                )
              <> block "{ not json"
          events = normalize config (Just guayaquil) False "https://www.teatrocentrodearte.org/eventos" listing
      map discoveredEventTitle events `shouldBe` ["Los Panchos", "El Cascanueces"]
      map discoveredEventExternalId events `shouldBe` ["eventos@20261121T0100", "eventos@20261122T2130"]
      map discoveredEventStatus events `shouldBe` ["announced", "cancelled"]
      map (discoveredVenueName . discoveredEventVenue) events
        `shouldBe` ["Teatro Centro de Arte", "Teatro Centro de Arte"]
    it "keeps one identity for a listed event with its own page when it is rescheduled" $ do
      let guayaquil = EventDiscoveryCity "Guayaquil" "EC" (Just "America/Guayaquil")
          config = platform { linkedDataVenueName = Just "Teatro Centro de Arte" }
          listingOn day =
            block
              ( "[{\"@type\":\"Event\",\"name\":\"Los Panchos\",\"url\":\"https://www.teatrocentrodearte.org/eventos/los-panchos\",\"startDate\":\"2026-11-" <> day <> "T20:00:00\"},"
                  <> "{\"@type\":\"Event\",\"name\":\"Temporada A\",\"url\":\"https://www.teatrocentrodearte.org/eventos/temporada\",\"startDate\":\"2026-11-25T20:00:00\"},"
                  <> "{\"@type\":\"Event\",\"name\":\"Temporada B\",\"url\":\"https://www.teatrocentrodearte.org/eventos/temporada\",\"startDate\":\"2026-11-26T20:00:00\"}]"
              )
          identities day =
            map discoveredEventExternalId
              (normalize config (Just guayaquil) False "https://www.teatrocentrodearte.org/eventos" (listingOn day))
      identities "20" `shouldBe` ["los-panchos", "temporada@20261126T0100", "temporada@20261127T0100"]
      identities "27" `shouldBe` ["los-panchos", "temporada@20261126T0100", "temporada@20261127T0100"]
    it "gives same-named venues in different cities of one source different identities" $ do
      let eventIn locality =
            block
              ( "{\"@type\":\"Event\",\"name\":\"Gira\",\"startDate\":\"2026-12-11T20:00:00-05:00\","
                  <> "\"location\":{\"@type\":\"Place\",\"name\":\"Teatro Municipal\",\"address\":{\"addressLocality\":\"" <> locality <> "\"}}}"
              )
          venueIdIn locality =
            map (discoveredVenueExternalId . discoveredEventVenue)
              (normalize platform Nothing True "https://p.example/eventos/gira" (eventIn locality))
      venueIdIn "Quito" `shouldBe` ["source:ec:quito:teatro municipal"]
      venueIdIn "Cuenca" `shouldBe` ["source:ec:cuenca:teatro municipal"]
    it "rejects a registered city name in an explicitly different country" $ do
      let eventIn country =
            block
              ( "{\"@type\":\"Event\",\"name\":\"Gira\",\"startDate\":\"2026-12-11T20:00:00-05:00\","
                  <> "\"location\":{\"@type\":\"Place\",\"name\":\"Teatro Municipal\",\"address\":{\"addressLocality\":\"Cuenca\",\"addressCountry\":\"" <> country <> "\"}}}"
              )
          matched country =
            length (normalize platform Nothing True "https://p.example/eventos/gira" (eventIn country))
      matched "Spain" `shouldBe` 0
      matched "España" `shouldBe` 0
      matched "ES" `shouldBe` 0
      matched "Ecuador" `shouldBe` 1
      matched "República del Ecuador" `shouldBe` 1
      matched "EC" `shouldBe` 1
    it "classifies free-text source categories into catalog event types" $ do
      classifyDiscoveredEventType "Festival de Jazz" [] `shouldBe` "festival"
      classifyDiscoveredEventType "Gala" ["Danza", "Música"] `shouldBe` "concert"
      classifyDiscoveredEventType "Gala" ["MusicEvent"] `shouldBe` "concert"
      classifyDiscoveredEventType "Macbeth" ["Teatro"] `shouldBe` "other"

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
            discoveredEventEnd event `shouldBe` Nothing
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

  describe "event publication validation" $ do
    it "requires verified lineup and sale URL even after source approval" $ do
      response <- either fail pure (eitherDecode ticketmasterFixture)
      event <- case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
        [normalized] -> pure normalized
        other -> fail ("Expected one event, got " <> show other)
      let verified = event { discoveredEventTicketUrl=Just "https://official.example/tickets"
            , discoveredEventVenue=(discoveredEventVenue event) { discoveredVenueCountryCode=Just "EC" } }
      discoveredEventPublicationReady verified `shouldBe` True
      discoveredEventPublicationReady verified { discoveredEventArtists=[] } `shouldBe` False
      discoveredEventPublicationReady verified { discoveredEventTicketUrl=Nothing } `shouldBe` False
      discoveredEventPublicationReady verified { discoveredEventEnd=Nothing } `shouldBe` True

  describe "Ticketmaster event normalization" $ do
    it "keeps an unconfirmed end unknown through draft ingestion and replay" $ do
      let withoutEnd = BL8.pack . T.unpack . T.replace
            "\"end\":{\"dateTime\":\"2026-08-01T23:00:00Z\"}," ""
            . T.pack . BL8.unpack $ ticketmasterFixture
      response <- either fail pure (eitherDecode withoutEnd)
      event <- case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
        [normalized] -> pure normalized
        other -> fail ("Expected one event, got " <> show other)
      discoveredEventEnd event `shouldBe` Nothing
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool
      first <- syncDiscoveredEventDraft pool (fixtureTime 10 5) event
      replay <- syncDiscoveredEventDraft pool (fixtureTime 10 6) event
      discoveryEventsCreated first `shouldBe` 1
      discoveryEventsCreated replay `shouldBe` 0
      Just (Entity _ ref) <- runSqlPool
        (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1")) pool
      Just stored <- runSqlPool (get (Social.externalEventRefEventId ref)) pool
      Social.socialEventEndTime stored `shouldBe` Nothing
      destroyAllResources pool

    it "creates a complete graph while ignoring malformed provider records and other cities" $ do
      case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err)
        Right response -> do
          let now = fixtureTime 10 0
              events = normalizeTicketmasterResponse "USD" "Quito" now response
          case events of
            [event] -> do
              discoveredEventEnd event `shouldBe` Just (fixtureTime 23 0)
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

    it "preserves edited event fields and metadata across repeated source refreshes" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> fail err
        Right response -> case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
          [normalized] -> pure normalized
          _ -> fail "Expected one fixture event"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 5) event
      Just (Entity _ ref) <- runSqlPool (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1")) pool
      let eventKey = Social.externalEventRefEventId ref
      Just original <- runSqlPool (get eventKey) pool
      let decodeMetadata row = Social.socialEventMetadata row >>= decodeStrict' . TE.encodeUtf8
          editedMetadata = case decodeMetadata original of
            Just (Object fields) -> Just . TE.decodeUtf8 . BL.toStrict . encode . Object $
              KM.insert "editorNote" (String "Confirmed by venue") $
              KM.insert "imageUrl" (String "https://editor.example/approved.jpg") fields
            _ -> Nothing
      runSqlPool (update eventKey
        [ Social.SocialEventTitle =. "Editorial title"
        , Social.SocialEventPriceCents =. Just 7777
        , Social.SocialEventMetadata =. editedMetadata
        , Social.SocialEventUpdatedAt =. fixtureTime 10 6
        ]) pool
      let refreshed = event
            { discoveredEventTitle = "Provider title changed"
            , discoveredEventPriceCents = Just 1200
            , discoveredEventStart = addUTCTime 86400 (discoveredEventStart event)
            , discoveredEventDescription = Just "Verified new description"
            , discoveredEventImageUrl = Just "https://provider.example/new.jpg"
            }
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 10) refreshed
      -- Coincidental agreement with the editorial value must not let the source
      -- claim ownership again on its next change.
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 12) refreshed
        {discoveredEventTitle = "Editorial title", discoveredEventPriceCents = Just 7777}
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 15) refreshed
        {discoveredEventDescription = Just "Verified subsequent description"}
      Just final <- runSqlPool (get eventKey) pool
      Social.socialEventTitle final `shouldBe` "Editorial title"
      Social.socialEventPriceCents final `shouldBe` Just 7777
      Social.socialEventStartTime final `shouldBe` discoveredEventStart refreshed
      Social.socialEventDescription final `shouldBe` Just "Verified subsequent description"
      case decodeMetadata final of
        Just (Object fields) -> do
          KM.lookup "editorNote" fields `shouldBe` Just (String "Confirmed by venue")
          KM.lookup "imageUrl" fields `shouldBe` Just (String "https://editor.example/approved.jpg")
          KM.lookup "isPublic" fields `shouldBe` Just (Bool False)
        _ -> expectationFailure "Expected retained metadata"
      runSqlPool (count ([] :: [Filter Social.SocialEvent])) pool `shouldReturn` 1
      destroyAllResources pool

    forM_ [False, True] $ \legacy ->
      it ("keeps editorial visibility and ticket URLs through full provider reconciliation; legacy=" <> show legacy) $ do
        event <- case eitherDecode ticketmasterFixture of
          Left err -> fail err
          Right response -> case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            _ -> fail "Expected one fixture event"
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeEventDiscoverySchema pool
        _ <- syncDiscoveredEvent pool (fixtureTime 10 5) event
        Just (Entity _ ref) <- runSqlPool (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1")) pool
        let eventKey = Social.externalEventRefEventId ref
        Just original <- runSqlPool (get eventKey) pool
        let editedMetadata = case Social.socialEventMetadata original >>= decodeStrict' . TE.encodeUtf8 of
              Just (Object fields) -> Just . TE.decodeUtf8 . BL.toStrict . encode . Object $
                (if legacy then KM.delete "_discoveryOwned" else id) $
                KM.insert "isPublic" (Bool False) $
                KM.insert "ticketUrl" (String "https://editor.example/tickets") fields
              _ -> Nothing
        let editorialState = UUID.fromString "00000000-0000-4000-8000-000000000231"
        runSqlPool (update eventKey
          [Social.SocialEventMetadata =. editedMetadata, Social.SocialEventWorkflowStateId =. editorialState]) pool
        forM_ [10, 12, 15] $ \minute -> do
          _ <- syncDiscoveredEvent pool (fixtureTime 10 minute) event
          _ <- reconcileProviderEvents pool (fixtureTime 10 minute) "ticketmaster"
            [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")] ["tm-event-1"]
          _ <- reconcileImportedEvents pool (fixtureTime 10 minute)
            [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")]
          Just stored <- runSqlPool (get eventKey) pool
          Social.socialEventWorkflowStateId stored `shouldBe` editorialState
          case Social.socialEventMetadata stored >>= decodeStrict' . TE.encodeUtf8 of
            Just (Object fields) -> do
              KM.lookup "isPublic" fields `shouldBe` Just (Bool False)
              KM.lookup "ticketUrl" fields `shouldBe` Just (String "https://editor.example/tickets")
              case KM.lookup "_discoveryOwned" fields of
                Just (Object owned) -> do
                  KM.lookup "isPublic" owned `shouldBe` Nothing
                  KM.lookup "ticketUrl" owned `shouldBe` Nothing
                  KM.lookup "workflowStateId" owned `shouldBe` Nothing
                _ -> expectationFailure "Expected explicit non-adoption evidence"
            _ -> expectationFailure "Expected metadata"
        destroyAllResources pool

    it "relinquishes only changed fields at the editorial write, before provider values can coincide" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> fail err
        Right response -> case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
          [normalized] -> pure normalized
          _ -> fail "Expected one fixture event"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 5) event
      Just (Entity _ ref) <- runSqlPool (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1")) pool
      let eventKey = Social.externalEventRefEventId ref
      Just original <- runSqlPool (get eventKey) pool
      let proposed = original{Social.socialEventTitle = "Editorial title"}
          metadata = preserveEventOwnership original proposed
      runSqlPool (update eventKey [Social.SocialEventTitle =. "Editorial title", Social.SocialEventMetadata =. metadata]) pool
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 10) event{discoveredEventTitle = "Editorial title"}
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 15) event
        {discoveredEventTitle = "Provider changed again", discoveredEventDescription = Just "Fresh provider description"}
      Just final <- runSqlPool (get eventKey) pool
      Social.socialEventTitle final `shouldBe` "Editorial title"
      Social.socialEventDescription final `shouldBe` Just "Fresh provider description"
      destroyAllResources pool

    it "keeps editorial ownership removal monotone and idempotent for arbitrary edits" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> fail err
        Right response -> case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
          [normalized] -> pure normalized
          _ -> fail "Expected one fixture event"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 5) event
      Just (Entity _ ref) <- runSqlPool (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1")) pool
      Just original <- runSqlPool (get (Social.externalEventRefEventId ref)) pool
      result <- quickCheckResult $ property $ \title ->
        let proposed = original{Social.socialEventTitle = T.pack title}
            first = proposed{Social.socialEventMetadata = preserveEventOwnership original proposed}
            repeated = preserveEventOwnership first first
            reverted = first{Social.socialEventTitle = Social.socialEventTitle original}
            final = preserveEventOwnership first reverted
            ownedTitle raw = case raw >>= decodeStrict' . TE.encodeUtf8 of
              Just (Object fields) -> case KM.lookup "_discoveryOwned" fields of
                Just (Object owned) -> KM.lookup "title" owned
                _ -> Nothing
              _ -> Nothing
         in repeated == Social.socialEventMetadata first
            && (T.pack title == Social.socialEventTitle original || ownedTitle final == Nothing)
      result `shouldSatisfy` isSuccess
      destroyAllResources pool

    it "does not claim legacy event fields without provider ownership evidence" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> fail err
        Right response -> case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
          [normalized] -> pure normalized
          _ -> fail "Expected one fixture event"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 5) event
      Just (Entity _ ref) <- runSqlPool (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1")) pool
      let eventKey = Social.externalEventRefEventId ref
      runSqlPool (update eventKey
        [ Social.SocialEventTitle =. "Legacy editorial title"
        , Social.SocialEventMetadata =. Just "{\"isPublic\":false,\"editorNote\":\"Keep\"}"
        ]) pool
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 10) event
      _ <- syncDiscoveredEventDraft pool (fixtureTime 10 15) event
      final <- runSqlPool (get eventKey) pool
      Social.socialEventTitle <$> final `shouldBe` Just "Legacy editorial title"
      (Social.socialEventMetadata =<< final) `shouldSatisfy` maybe False (T.isInfixOf "Keep")
      destroyAllResources pool

    it "preserves an administrator suppression marker across provider refreshes" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            other -> expectationFailure ("Expected one normalized event, got " <> show other) >> fail "invalid normalized fixture"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool

      _ <- syncDiscoveredEvent pool (fixtureTime 10 5) event
      importedRef <-
        runSqlPool
          (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1"))
          pool
      case importedRef of
        Nothing -> expectationFailure "Expected a persisted provider event reference"
        Just (Entity refId ref) -> do
          runSqlPool
            ( do
                update
                  refId
                  [ Social.ExternalEventRefSourceStatus =.
                      Social.externalEventRefSuppressedStatus
                  ]
                update
                  (Social.externalEventRefEventId ref)
                  [ Social.SocialEventMetadata =.
                      Just "{\"ticketUrl\":null,\"isPublic\":false,\"currency\":\"USD\"}"
                  ]
            )
            pool
          countImportedDiscoveryEvents pool `shouldReturn` 0

          _ <-
            reconcileProviderEvents
              pool
              (fixtureTime 10 8)
              "ticketmaster"
              [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")]
              []
          reconciledRef <- runSqlPool (get refId) pool
          Social.externalEventRefSourceStatus <$> reconciledRef
            `shouldBe` Just Social.externalEventRefSuppressedStatus
          _ <-
            reconcileImportedEvents
              pool
              (fixtureTime 10 9)
              [EventDiscoveryCity "Quito" "EC" (Just "America/Guayaquil")]

          refreshStats <-
            syncDiscoveredEvent
              pool
              (fixtureTime 10 10)
              event
                { discoveredEventTitle = "Provider tried to restore this event"
                , discoveredEventVenue =
                    (discoveredEventVenue event)
                      { discoveredVenueName = "Provider tried to overwrite this venue"
                      }
                , discoveredEventArtists =
                    [ artist
                        { discoveredArtistName = "Provider tried to overwrite this artist"
                        }
                    | artist <- discoveredEventArtists event
                    ]
                }
          discoveryEventsCreated refreshStats `shouldBe` 0
          discoveryVenuesCreated refreshStats `shouldBe` 0
          discoveryArtistsCreated refreshStats `shouldBe` 0

          refreshedRef <- runSqlPool (get refId) pool
          Social.externalEventRefSourceStatus <$> refreshedRef
            `shouldBe` Just Social.externalEventRefSuppressedStatus
          refreshedEvent <- runSqlPool (get (Social.externalEventRefEventId ref)) pool
          Social.socialEventTitle <$> refreshedEvent `shouldBe` Just "Festival Sonoro"
          fmap Social.socialEventMetadata refreshedEvent
            `shouldSatisfy`
              maybe False (maybe False (T.isInfixOf "\"isPublic\":false"))
          refreshedVenueRef <-
            runSqlPool
              (getBy (Social.UniqueExternalVenueRef "ticketmaster" "tm-venue-1"))
              pool
          case refreshedVenueRef of
            Nothing -> expectationFailure "Expected the original venue reference"
            Just (Entity _ venueRef) -> do
              refreshedVenue <-
                runSqlPool (get (Social.externalVenueRefVenueId venueRef)) pool
              Social.venueName <$> refreshedVenue `shouldBe` Just "Teatro Nacional"
          refreshedArtistRef <-
            runSqlPool
              (getBy (Social.UniqueExternalArtistRef "ticketmaster" "tm-artist-1"))
              pool
          case refreshedArtistRef of
            Nothing -> expectationFailure "Expected the original artist reference"
            Just (Entity _ artistRef) -> do
              refreshedArtist <-
                runSqlPool (get (Social.externalArtistRefArtistId artistRef)) pool
              Social.artistProfileName <$> refreshedArtist `shouldBe` Just "La Banda"

    it "propagates suppression to a new provider identity for the same event" $ do
      event <- case eitherDecode ticketmasterFixture of
        Left err -> expectationFailure ("Fixture did not decode: " <> err) >> fail "invalid fixture"
        Right response ->
          case normalizeTicketmasterResponse "USD" "Quito" (fixtureTime 10 0) response of
            [normalized] -> pure normalized
            other -> expectationFailure ("Expected one normalized event, got " <> show other) >> fail "invalid normalized fixture"
      pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
      runSqlPool initializeEventDiscoverySchema pool

      let distractorStart = addUTCTime (24 * 60 * 60) (discoveredEventStart event)
          distractorEnd = addUTCTime (2 * 60 * 60) distractorStart
      runSqlPool
        ( do
            rawExecute
              "WITH digits(value) AS (VALUES (0),(1),(2),(3),(4),(5),(6),(7),(8),(9)), numbers(value) AS (SELECT ones.value+10*tens.value+100*hundreds.value+1000*thousands.value+1 FROM digits ones CROSS JOIN digits tens CROSS JOIN digits hundreds CROSS JOIN digits thousands) INSERT INTO social_event(id,organizer_party_id,title,description,venue_id,event_type_id,workflow_state_id,timezone,start_time,end_time,price_cents,currency_id,capacity,metadata,created_at,updated_at) SELECT 10000+value,'system:event-discovery','Canonical-match distractor',NULL,NULL,NULL,NULL,NULL,?,?,NULL,NULL,NULL,NULL,?,? FROM numbers WHERE value<=5001"
              [ toPersistValue distractorStart
              , toPersistValue distractorEnd
              , toPersistValue (fixtureTime 10 1)
              , toPersistValue (fixtureTime 10 1)
              ]
            rawExecute
              "INSERT INTO external_event_ref(provider,external_id,event_id,city,country_code,source_url,price_cents,currency,last_seen_at,missing_runs,source_status) SELECT 'distractor','distractor-'||id,id,'Guayaquil','EC',NULL,NULL,'USD',?,0,'on_sale' FROM social_event WHERE id BETWEEN 10001 AND 15001"
              [toPersistValue (fixtureTime 10 1)]
        )
        pool

      _ <- syncDiscoveredEvent pool (fixtureTime 10 5) event
      importedRef <-
        runSqlPool
          (getBy (Social.UniqueExternalEventRef "ticketmaster" "tm-event-1"))
          pool
      case importedRef of
        Nothing -> expectationFailure "Expected a persisted provider event reference"
        Just (Entity refId ref) -> do
          let suppressedEventId = Social.externalEventRefEventId ref
              replacementIdentity =
                event
                  { discoveredEventProvider = "buenplan"
                  , discoveredEventExternalId = "bp-reissued-event-id"
                  , discoveredEventVenue =
                      (discoveredEventVenue event)
                        { discoveredVenueName = "Ignored reissue venue"
                        }
                  , discoveredEventArtists =
                      [ artist
                          { discoveredArtistExternalId = "bp-suppressed-artist"
                          , discoveredArtistName = "Ignored reissue artist"
                          }
                      | artist <- discoveredEventArtists event
                      ]
                  , discoveredEventTicketUrl =
                      Just "https://www.buenplan.com.ec/event/festival-sonoro-reissued"
                  }
          runSqlPool
            ( do
                update
                  refId
                  [ Social.ExternalEventRefSourceStatus =.
                      Social.externalEventRefSuppressedStatus
                  ]
                update
                  suppressedEventId
                  [ Social.SocialEventMetadata =.
                      Just "{\"ticketUrl\":null,\"isPublic\":false,\"currency\":\"USD\"}"
                  ]
            )
            pool

          replacementStats <-
            syncDiscoveredEvent pool (fixtureTime 10 10) replacementIdentity
          discoveryEventsCreated replacementStats `shouldBe` 0
          discoveryVenuesCreated replacementStats `shouldBe` 0
          discoveryArtistsCreated replacementStats `shouldBe` 0
          runSqlPool (count ([] :: [Filter Social.SocialEvent])) pool `shouldReturn` 5002
          runSqlPool (count ([] :: [Filter Social.ExternalEventRef])) pool `shouldReturn` 5003
          runSqlPool (count ([] :: [Filter Social.Venue])) pool `shouldReturn` 1
          runSqlPool (count ([] :: [Filter Social.ExternalVenueRef])) pool `shouldReturn` 1
          runSqlPool (count ([] :: [Filter Social.ArtistProfile])) pool `shouldReturn` 1
          runSqlPool (count ([] :: [Filter Social.ExternalArtistRef])) pool `shouldReturn` 1

          replacementRef <-
            runSqlPool
              (getBy (Social.UniqueExternalEventRef "buenplan" "bp-reissued-event-id"))
              pool
          case replacementRef of
            Nothing -> expectationFailure "Expected the replacement provider identity to be retained"
            Just (Entity _ replacement) -> do
              Social.externalEventRefEventId replacement `shouldBe` suppressedEventId
              Social.externalEventRefSourceStatus replacement
                `shouldBe` Social.externalEventRefSuppressedStatus

          suppressedEvent <- runSqlPool (get suppressedEventId) pool
          Social.socialEventTitle <$> suppressedEvent `shouldBe` Just "Festival Sonoro"
          fmap Social.socialEventMetadata suppressedEvent
            `shouldSatisfy`
              maybe False (maybe False (T.isInfixOf "\"isPublic\":false"))

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
              , discoveredEventEnd = addUTCTime (4 * 60 * 60) <$> discoveredEventEnd event
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
    "CREATE TABLE social_event (id INTEGER PRIMARY KEY, organizer_party_id TEXT NULL, title TEXT NOT NULL, description TEXT NULL, venue_id INTEGER NULL, event_type_id TEXT NULL, workflow_state_id TEXT NULL, timezone TEXT NULL, start_time TIMESTAMP NOT NULL, end_time TIMESTAMP NULL, price_cents INTEGER NULL, currency_id TEXT NULL, capacity INTEGER NULL, metadata TEXT NULL, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL)"
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

isLeftText :: Either T.Text a -> Bool
isLeftText = either (const True) (const False)
