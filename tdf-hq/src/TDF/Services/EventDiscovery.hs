{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Services.EventDiscovery
  ( DiscoveredArtist(..)
  , DiscoveredEvent(..)
  , DiscoveredVenue(..)
  , DiscoverySyncStats(..)
  , EventDiscoveryCity(..)
  , EventDiscoveryRunHandle
  , beginEventDiscoveryRun
  , buildTicketmasterRequestUrl
  , fetchBuenPlanEvents
  , fetchStructuredFeedEvents
  , fetchTicketmasterEvents
  , fetchTicketmasterEventsForCity
  , failEventDiscoveryRun
  , finishEventDiscoveryRun
  , loadActiveUserCities
  , loadSubscribedDiscoveryCities
  , normalizeTicketmasterResponse
  , normalizeUserCities
  , reconcileImportedEvents
  , reconcileProviderEvents
  , syncDiscoveredEvent
  ) where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Monad (forM, forM_)
import Control.Concurrent (threadDelay)
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , decodeStrict'
  , encode
  , eitherDecode
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  )
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isAlphaNum
  , isControl
  )
import Data.Function (on)
import Data.List (maximumBy, nubBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, diffUTCTime, utctDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Database.Persist
  ( Entity(..)
  , SelectOpt(LimitTo)
  , deleteWhere
  , get
  , getBy
  , insert
  , insertUnique
  , selectList
  , update
  , (=.)
  , (==.)
  )
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool)
import Network.HTTP.Client
  ( HttpException
  , Request
  , Response
  , httpLbs
  , parseRequest
  , redirectCount
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Text.Read (readMaybe)

import TDF.Config
  ( AppConfig(..)
  , normalizeConfiguredHttpsUrl
  )
import TDF.DB (ConnectionPool, sharedTlsManager)
import TDF.Internationalization (currencyDecimalPlaces, currencyDefinition, normalizeCurrencyCode)
import qualified TDF.Models.SocialEventsModels as Social

providerName :: Text
providerName = "ticketmaster"

systemOrganizerId :: Text
systemOrganizerId = "system:event-discovery"

data DiscoveredVenue = DiscoveredVenue
  { discoveredVenueExternalId :: Text
  , discoveredVenueName :: Text
  , discoveredVenueAddress :: Maybe Text
  , discoveredVenueCity :: Text
  , discoveredVenueCountry :: Maybe Text
  , discoveredVenueCountryCode :: Maybe Text
  , discoveredVenueLatitude :: Maybe Double
  , discoveredVenueLongitude :: Maybe Double
  , discoveredVenuePhone :: Maybe Text
  , discoveredVenueWebsite :: Maybe Text
  , discoveredVenueState :: Maybe Text
  , discoveredVenuePostalCode :: Maybe Text
  , discoveredVenueImageUrl :: Maybe Text
  } deriving (Eq, Show)

data DiscoveredArtist = DiscoveredArtist
  { discoveredArtistExternalId :: Text
  , discoveredArtistName :: Text
  , discoveredArtistGenres :: [Text]
  , discoveredArtistImageUrl :: Maybe Text
  } deriving (Eq, Show)

data DiscoveredEvent = DiscoveredEvent
  { discoveredEventProvider :: Text
  , discoveredEventExternalId :: Text
  , discoveredEventTitle :: Text
  , discoveredEventDescription :: Maybe Text
  , discoveredEventStart :: UTCTime
  , discoveredEventEnd :: UTCTime
  , discoveredEventVenue :: DiscoveredVenue
  , discoveredEventArtists :: [DiscoveredArtist]
  , discoveredEventPriceCents :: Maybe Int
  , discoveredEventCurrency :: Text
  , discoveredEventTicketUrl :: Maybe Text
  , discoveredEventImageUrl :: Maybe Text
  , discoveredEventType :: Text
  , discoveredEventStatus :: Text
  } deriving (Eq, Show)

data DiscoverySyncStats = DiscoverySyncStats
  { discoveryEventsSeen :: Int
  , discoveryEventsCreated :: Int
  , discoveryEventsUpdated :: Int
  , discoveryVenuesCreated :: Int
  , discoveryArtistsCreated :: Int
  } deriving (Eq, Show)

data EventDiscoveryCity = EventDiscoveryCity
  { eventDiscoveryCityName :: Text
  , eventDiscoveryCityCountryCode :: Text
  , eventDiscoveryCityTimeZone :: Maybe Text
  } deriving (Eq, Show)

emptyDiscoverySyncStats :: DiscoverySyncStats
emptyDiscoverySyncStats = DiscoverySyncStats 0 0 0 0 0

newtype EventDiscoveryRunHandle =
  EventDiscoveryRunHandle Social.ExternalEventDiscoveryRunId

beginEventDiscoveryRun ::
  ConnectionPool ->
  Text ->
  UTCTime ->
  UTCTime ->
  IO (Maybe EventDiscoveryRunHandle)
beginEventDiscoveryRun pool provider scheduledFor now =
  runSqlPool claim pool
  where
    runDate = utctDay scheduledFor
    claim = do
      existing <-
        getBy
          (Social.UniqueExternalEventDiscoverySlot provider (Just scheduledFor))
      case existing of
        Just (Entity runKey runRow)
          | shouldReclaim runRow -> do
              update
                runKey
                [ Social.ExternalEventDiscoveryRunStatus =. "running"
                , Social.ExternalEventDiscoveryRunScheduledFor =. Just scheduledFor
                , Social.ExternalEventDiscoveryRunCitiesCount =. 0
                , Social.ExternalEventDiscoveryRunEventsSeen =. 0
                , Social.ExternalEventDiscoveryRunEventsCreated =. 0
                , Social.ExternalEventDiscoveryRunEventsUpdated =. 0
                , Social.ExternalEventDiscoveryRunVenuesCreated =. 0
                , Social.ExternalEventDiscoveryRunArtistsCreated =. 0
                , Social.ExternalEventDiscoveryRunErrorMessage =. Nothing
                , Social.ExternalEventDiscoveryRunStartedAt =. now
                , Social.ExternalEventDiscoveryRunFinishedAt =. Nothing
                ]
              pure (Just (EventDiscoveryRunHandle runKey))
        Just _ -> pure Nothing
        Nothing -> do
          inserted <-
            insertUnique
              Social.ExternalEventDiscoveryRun
                { Social.externalEventDiscoveryRunProvider = provider
                , Social.externalEventDiscoveryRunRunDate = runDate
                , Social.externalEventDiscoveryRunScheduledFor = Just scheduledFor
                , Social.externalEventDiscoveryRunStatus = "running"
                , Social.externalEventDiscoveryRunCitiesCount = 0
                , Social.externalEventDiscoveryRunEventsSeen = 0
                , Social.externalEventDiscoveryRunEventsCreated = 0
                , Social.externalEventDiscoveryRunEventsUpdated = 0
                , Social.externalEventDiscoveryRunVenuesCreated = 0
                , Social.externalEventDiscoveryRunArtistsCreated = 0
                , Social.externalEventDiscoveryRunErrorMessage = Nothing
                , Social.externalEventDiscoveryRunStartedAt = now
                , Social.externalEventDiscoveryRunFinishedAt = Nothing
                }
          pure (EventDiscoveryRunHandle <$> inserted)

    shouldReclaim runRow =
      Social.externalEventDiscoveryRunStatus runRow == "failed"
        || ( Social.externalEventDiscoveryRunStatus runRow == "running"
              && Social.externalEventDiscoveryRunStartedAt runRow
                <= addUTCTime (negate (6 * 60 * 60)) now
           )

finishEventDiscoveryRun ::
  ConnectionPool ->
  EventDiscoveryRunHandle ->
  UTCTime ->
  Int ->
  DiscoverySyncStats ->
  IO ()
finishEventDiscoveryRun pool (EventDiscoveryRunHandle runKey) now citiesCount stats =
  runSqlPool
    ( update
        runKey
        [ Social.ExternalEventDiscoveryRunStatus =. "completed"
        , Social.ExternalEventDiscoveryRunCitiesCount =. citiesCount
        , Social.ExternalEventDiscoveryRunEventsSeen =. discoveryEventsSeen stats
        , Social.ExternalEventDiscoveryRunEventsCreated =. discoveryEventsCreated stats
        , Social.ExternalEventDiscoveryRunEventsUpdated =. discoveryEventsUpdated stats
        , Social.ExternalEventDiscoveryRunVenuesCreated =. discoveryVenuesCreated stats
        , Social.ExternalEventDiscoveryRunArtistsCreated =. discoveryArtistsCreated stats
        , Social.ExternalEventDiscoveryRunErrorMessage =. Nothing
        , Social.ExternalEventDiscoveryRunFinishedAt =. Just now
        ]
    )
    pool

failEventDiscoveryRun ::
  ConnectionPool ->
  EventDiscoveryRunHandle ->
  UTCTime ->
  Text ->
  IO ()
failEventDiscoveryRun pool (EventDiscoveryRunHandle runKey) now rawError =
  runSqlPool
    ( update
        runKey
        [ Social.ExternalEventDiscoveryRunStatus =. "failed"
        , Social.ExternalEventDiscoveryRunErrorMessage =.
            Just (T.take 2000 (T.strip rawError))
        , Social.ExternalEventDiscoveryRunFinishedAt =. Just now
        ]
    )
    pool

newtype NamedValue = NamedValue { namedValue :: Text }

instance FromJSON NamedValue where
  parseJSON = withObject "NamedValue" $ \o -> NamedValue <$> o .: "name"

data TicketmasterImage = TicketmasterImage
  { ticketmasterImageUrl :: Text
  , ticketmasterImageWidth :: Int
  , ticketmasterImageHeight :: Int
  , ticketmasterImageFallback :: Bool
  }

instance FromJSON TicketmasterImage where
  parseJSON = withObject "TicketmasterImage" $ \o ->
    TicketmasterImage
      <$> o .: "url"
      <*> (o .:? "width" .!= 0)
      <*> (o .:? "height" .!= 0)
      <*> (o .:? "fallback" .!= False)

data TicketmasterLocation = TicketmasterLocation
  { ticketmasterLongitude :: Maybe Text
  , ticketmasterLatitude :: Maybe Text
  }

instance FromJSON TicketmasterLocation where
  parseJSON = withObject "TicketmasterLocation" $ \o ->
    TicketmasterLocation <$> o .:? "longitude" <*> o .:? "latitude"

newtype TicketmasterAddress = TicketmasterAddress { ticketmasterAddressLine :: Maybe Text }

instance FromJSON TicketmasterAddress where
  parseJSON = withObject "TicketmasterAddress" $ \o ->
    TicketmasterAddress <$> o .:? "line1"

data TicketmasterVenue = TicketmasterVenue
  { ticketmasterVenueId :: Text
  , ticketmasterVenueName :: Text
  , ticketmasterVenueUrl :: Maybe Text
  , ticketmasterVenueAddress :: Maybe TicketmasterAddress
  , ticketmasterVenueCity :: Maybe NamedValue
  , ticketmasterVenueState :: Maybe NamedValue
  , ticketmasterVenueCountry :: Maybe NamedValue
  , ticketmasterVenuePostalCode :: Maybe Text
  , ticketmasterVenueLocation :: Maybe TicketmasterLocation
  , ticketmasterVenueBoxOffice :: Maybe TicketmasterBoxOffice
  , ticketmasterVenueImages :: [TicketmasterImage]
  }

newtype TicketmasterBoxOffice = TicketmasterBoxOffice
  { ticketmasterBoxOfficePhone :: Maybe Text
  }

instance FromJSON TicketmasterBoxOffice where
  parseJSON = withObject "TicketmasterBoxOffice" $ \o ->
    TicketmasterBoxOffice <$> o .:? "phoneNumberDetail"

instance FromJSON TicketmasterVenue where
  parseJSON = withObject "TicketmasterVenue" $ \o ->
    TicketmasterVenue
      <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "url"
      <*> o .:? "address"
      <*> o .:? "city"
      <*> o .:? "state"
      <*> o .:? "country"
      <*> o .:? "postalCode"
      <*> o .:? "location"
      <*> o .:? "boxOfficeInfo"
      <*> (decodeValidProviderItems <$> (o .:? "images" .!= []))

data TicketmasterAttraction = TicketmasterAttraction
  { ticketmasterAttractionId :: Text
  , ticketmasterAttractionName :: Text
  , ticketmasterAttractionImages :: [TicketmasterImage]
  , ticketmasterAttractionClassifications :: [TicketmasterClassification]
  }

instance FromJSON TicketmasterAttraction where
  parseJSON = withObject "TicketmasterAttraction" $ \o ->
    TicketmasterAttraction
      <$> o .: "id"
      <*> o .: "name"
      <*> (decodeValidProviderItems <$> (o .:? "images" .!= []))
      <*> (decodeValidProviderItems <$> (o .:? "classifications" .!= []))

data TicketmasterEmbedded = TicketmasterEmbedded
  { ticketmasterVenues :: [TicketmasterVenue]
  , ticketmasterAttractions :: [TicketmasterAttraction]
  }

instance FromJSON TicketmasterEmbedded where
  parseJSON = withObject "TicketmasterEmbedded" $ \o ->
    TicketmasterEmbedded
      <$> (decodeValidProviderItems <$> (o .:? "venues" .!= []))
      <*> (decodeValidProviderItems <$> (o .:? "attractions" .!= []))

data TicketmasterStart = TicketmasterStart
  { ticketmasterStartDateTime :: Maybe Text
  }

instance FromJSON TicketmasterStart where
  parseJSON = withObject "TicketmasterStart" $ \o ->
    TicketmasterStart <$> o .:? "dateTime"

data TicketmasterEnd = TicketmasterEnd
  { ticketmasterEndDateTime :: Maybe Text
  }

instance FromJSON TicketmasterEnd where
  parseJSON = withObject "TicketmasterEnd" $ \o ->
    TicketmasterEnd <$> o .:? "dateTime"

newtype TicketmasterStatus = TicketmasterStatus { ticketmasterStatusCode :: Text }

instance FromJSON TicketmasterStatus where
  parseJSON = withObject "TicketmasterStatus" $ \o ->
    TicketmasterStatus <$> o .: "code"

data TicketmasterDates = TicketmasterDates
  { ticketmasterDatesStart :: TicketmasterStart
  , ticketmasterDatesEnd :: Maybe TicketmasterEnd
  , ticketmasterDatesStatus :: Maybe TicketmasterStatus
  }

instance FromJSON TicketmasterDates where
  parseJSON = withObject "TicketmasterDates" $ \o ->
    TicketmasterDates <$> o .: "start" <*> o .:? "end" <*> o .:? "status"

data TicketmasterPublicSale = TicketmasterPublicSale
  { ticketmasterPublicSaleStart :: Maybe Text
  , ticketmasterPublicSaleEnd :: Maybe Text
  }

instance FromJSON TicketmasterPublicSale where
  parseJSON = withObject "TicketmasterPublicSale" $ \o ->
    TicketmasterPublicSale <$> o .:? "startDateTime" <*> o .:? "endDateTime"

newtype TicketmasterSales = TicketmasterSales
  { ticketmasterPublicSale :: Maybe TicketmasterPublicSale
  }

instance FromJSON TicketmasterSales where
  parseJSON = withObject "TicketmasterSales" $ \o ->
    TicketmasterSales <$> o .:? "public"

data TicketmasterPriceRange = TicketmasterPriceRange (Maybe Text) (Maybe Double)

instance FromJSON TicketmasterPriceRange where
  parseJSON = withObject "TicketmasterPriceRange" $ \o ->
    TicketmasterPriceRange <$> o .:? "currency" <*> o .:? "min"

data TicketmasterClassification = TicketmasterClassification
  { ticketmasterSegment :: Maybe NamedValue
  , ticketmasterGenre :: Maybe NamedValue
  , ticketmasterSubGenre :: Maybe NamedValue
  }

instance FromJSON TicketmasterClassification where
  parseJSON = withObject "TicketmasterClassification" $ \o ->
    TicketmasterClassification
      <$> o .:? "segment"
      <*> o .:? "genre"
      <*> o .:? "subGenre"

data TicketmasterEvent = TicketmasterEvent
  { ticketmasterEventId :: Text
  , ticketmasterEventName :: Text
  , ticketmasterEventUrl :: Maybe Text
  , ticketmasterEventInfo :: Maybe Text
  , ticketmasterEventNote :: Maybe Text
  , ticketmasterEventImages :: [TicketmasterImage]
  , ticketmasterEventDates :: TicketmasterDates
  , ticketmasterEventSales :: Maybe TicketmasterSales
  , ticketmasterEventPrices :: [TicketmasterPriceRange]
  , ticketmasterEventClassifications :: [TicketmasterClassification]
  , ticketmasterEventEmbedded :: Maybe TicketmasterEmbedded
  }

instance FromJSON TicketmasterEvent where
  parseJSON = withObject "TicketmasterEvent" $ \o ->
    TicketmasterEvent
      <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "url"
      <*> o .:? "info"
      <*> o .:? "pleaseNote"
      <*> (decodeValidProviderItems <$> (o .:? "images" .!= []))
      <*> o .: "dates"
      <*> o .:? "sales"
      <*> (decodeValidProviderItems <$> (o .:? "priceRanges" .!= []))
      <*> (decodeValidProviderItems <$> (o .:? "classifications" .!= []))
      <*> o .:? "_embedded"

data TicketmasterEvents = TicketmasterEvents
  { ticketmasterEvents :: [TicketmasterEvent]
  , ticketmasterRawEventCount :: Int
  }

instance FromJSON TicketmasterEvents where
  parseJSON = withObject "TicketmasterEvents" $ \o -> do
    rawEvents <- o .:? "events" .!= []
    pure
      TicketmasterEvents
        { ticketmasterEvents = decodeValidProviderItems rawEvents
        , ticketmasterRawEventCount = length rawEvents
        }

newtype TicketmasterPage = TicketmasterPage { ticketmasterTotalPages :: Int }

instance FromJSON TicketmasterPage where
  parseJSON = withObject "TicketmasterPage" $ \o ->
    TicketmasterPage <$> (o .:? "totalPages" .!= 0)

data TicketmasterResponse = TicketmasterResponse
  { ticketmasterResponseEvents :: [TicketmasterEvent]
  , ticketmasterResponseTotalPages :: Int
  , ticketmasterResponseRawEventCount :: Int
  }

instance FromJSON TicketmasterResponse where
  parseJSON = withObject "TicketmasterResponse" $ \o -> do
    embedded <- o .:? "_embedded"
    page <- o .:? "page"
    pure
      TicketmasterResponse
        { ticketmasterResponseEvents = maybe [] ticketmasterEvents embedded
        , ticketmasterResponseTotalPages = maybe 0 ticketmasterTotalPages page
        , ticketmasterResponseRawEventCount = maybe 0 ticketmasterRawEventCount embedded
        }

-- Ticketmaster occasionally includes an incomplete item inside an otherwise
-- valid page (for example, a venue without a name). Decode provider arrays
-- item-by-item so one malformed record cannot discard every usable event in
-- the city response.
decodeValidProviderItems :: FromJSON a => [Value] -> [a]
decodeValidProviderItems = mapMaybe (parseMaybe parseJSON)

data BuenPlanImage = BuenPlanImage
  { buenPlanImageUrl :: Text
  }

instance FromJSON BuenPlanImage where
  parseJSON = withObject "BuenPlanImage" $ \o ->
    BuenPlanImage <$> o .: "url"

data BuenPlanEvent = BuenPlanEvent
  { buenPlanEventId :: Text
  , buenPlanEventTitle :: Text
  , buenPlanEventDescription :: Maybe Text
  , buenPlanEventSlug :: Text
  , buenPlanEventStart :: UTCTime
  , buenPlanEventTimeZone :: Maybe Text
  , buenPlanEventCover :: Maybe BuenPlanImage
  , buenPlanEventPoster :: Maybe BuenPlanImage
  , buenPlanEventCurrency :: Maybe Text
  , buenPlanEventSellActive :: Bool
  }

instance FromJSON BuenPlanEvent where
  parseJSON = withObject "BuenPlanEvent" $ \o ->
    BuenPlanEvent
      <$> o .: "id"
      <*> o .: "title"
      <*> o .:? "description"
      <*> o .: "url"
      <*> o .: "startDate"
      <*> o .:? "timeZone"
      <*> o .:? "cover"
      <*> o .:? "poster"
      <*> o .:? "currency"
      <*> (o .:? "sellActive" .!= False)

data BuenPlanMeta = BuenPlanMeta
  { buenPlanPageCount :: Int
  }

instance FromJSON BuenPlanMeta where
  parseJSON = withObject "BuenPlanMeta" $ \o ->
    BuenPlanMeta <$> (o .:? "pageCount" .!= 1)

data BuenPlanResponse = BuenPlanResponse
  { buenPlanEvents :: [BuenPlanEvent]
  , buenPlanMeta :: BuenPlanMeta
  }

instance FromJSON BuenPlanResponse where
  parseJSON = withObject "BuenPlanResponse" $ \o ->
    BuenPlanResponse
      <$> (decodeValidProviderItems <$> (o .:? "data" .!= []))
      <*> o .: "meta"

buenPlanApiBase :: Text
buenPlanApiBase = "https://api.buenplan.com.ec"

buenPlanWebBase :: Text
buenPlanWebBase = "https://www.buenplan.com.ec"

fetchBuenPlanEvents ::
  AppConfig ->
  [EventDiscoveryCity] ->
  UTCTime ->
  IO (Either Text [DiscoveredEvent])
fetchBuenPlanEvents cfg cities now
  | null providerCities = pure (Right [])
  | otherwise = fetchPage 1 []
  where
    providerCities =
      filter ((== "EC") . eventDiscoveryCityCountryCode) cities
    endTime =
      addUTCTime
        (fromIntegral (eventDiscoveryLookaheadDays cfg * 86400))
        now
    fetchPage pageNumber collected
      | pageNumber > 10 = pure (Right collected)
      | otherwise = do
          requestResult <-
            try (parseRequest (buildBuenPlanRequestUrl pageNumber)) ::
              IO (Either HttpException Request)
          case requestResult of
            Left _ -> pure (Left "Could not build the Buen Plan event request")
            Right rawRequest -> do
              let request =
                    rawRequest
                      { requestHeaders =
                          [ ("Accept", "application/json")
                          , ("User-Agent", "TDF-Records-Event-Discovery/1.0")
                          ]
                      , responseTimeout = responseTimeoutMicro (20 * 1000000)
                      }
              threadDelay 1000000
              responseResult <- requestTicketmasterPage request 0
              case responseResult of
                Left _ -> pure (Left "Buen Plan request failed")
                Right response ->
                  let httpStatus = statusCode (responseStatus response)
                      body = responseBody response
                   in if httpStatus < 200 || httpStatus >= 300
                        then pure (Left ("Buen Plan returned HTTP " <> T.pack (show httpStatus)))
                        else
                          if BL.length body > 25 * 1024 * 1024
                            then pure (Left "Buen Plan response exceeded the 25 MB safety limit")
                            else
                              case eitherDecode body of
                                Left _ -> pure (Left "Buen Plan returned an invalid event response")
                                Right decoded -> do
                                  let normalized =
                                        mapMaybe
                                          (normalizeBuenPlanEvent (defaultCurrency cfg) providerCities now endTime)
                                          (buenPlanEvents decoded)
                                      nextCollected = collected ++ normalized
                                      pageCount = buenPlanPageCount (buenPlanMeta decoded)
                                  if pageNumber >= pageCount
                                    then pure (Right nextCollected)
                                    else fetchPage (pageNumber + 1) nextCollected

    buildBuenPlanRequestUrl pageNumber =
      T.unpack
        ( buenPlanApiBase
            <> "/v2/events/search"
            <> TE.decodeUtf8
              ( renderSimpleQuery
                  True
                  [ ("take", "100")
                  , ("page", BS8.pack (show pageNumber))
                  , ("order", "asc")
                  , ("sort", "startDate")
                  ]
              )
        )

normalizeBuenPlanEvent ::
  Text ->
  [EventDiscoveryCity] ->
  UTCTime ->
  UTCTime ->
  BuenPlanEvent ->
  Maybe DiscoveredEvent
normalizeBuenPlanEvent configuredDefault cities now endTime BuenPlanEvent{..} = do
  externalId <- cleanIdentifier buenPlanEventId
  title <- cleanSingleLine 160 buenPlanEventTitle
  whenMaybe (buenPlanEventStart < now || buenPlanEventStart > endTime) Nothing
  city <- matchBuenPlanCity cities title buenPlanEventDescription
  slug <- cleanIdentifier buenPlanEventSlug
  let description = buenPlanEventDescription >>= cleanMultiline 5000
      venueName =
        fromMaybe
          ("Ubicación publicada en Buen Plan · " <> eventDiscoveryCityName city)
          (description >>= extractBuenPlanVenueName)
      venueExternalId =
        eventDiscoveryCityCountryCode city
          <> ":"
          <> normalizeTokenText venueName
      imageUrl =
        (buenPlanEventPoster <|> buenPlanEventCover)
          >>= normalizeHttpsUrl "Buen Plan event image" . buenPlanImageUrl
      currency =
        fromMaybe configuredDefault
          ( buenPlanEventCurrency
              >>= normalizeCurrencyCode
          )
      status = if buenPlanEventSellActive then "on_sale" else "announced"
  pure
    DiscoveredEvent
      { discoveredEventProvider = "buenplan"
      , discoveredEventExternalId = externalId
      , discoveredEventTitle = title
      , discoveredEventDescription = description
      , discoveredEventStart = buenPlanEventStart
      , discoveredEventEnd = addUTCTime (3 * 60 * 60) buenPlanEventStart
      , discoveredEventVenue =
          DiscoveredVenue
            { discoveredVenueExternalId = venueExternalId
            , discoveredVenueName = venueName
            , discoveredVenueAddress = Nothing
            , discoveredVenueCity = eventDiscoveryCityName city
            , discoveredVenueCountry = Nothing
            , discoveredVenueCountryCode =
                Just (eventDiscoveryCityCountryCode city)
            , discoveredVenueLatitude = Nothing
            , discoveredVenueLongitude = Nothing
            , discoveredVenuePhone = Nothing
            , discoveredVenueWebsite = Nothing
            , discoveredVenueState = Nothing
            , discoveredVenuePostalCode = Nothing
            , discoveredVenueImageUrl = Nothing
            }
      , discoveredEventArtists = []
      , discoveredEventPriceCents = Nothing
      , discoveredEventCurrency = currency
      , discoveredEventTicketUrl =
          normalizeHttpsUrl
            "Buen Plan ticket URL"
            (buenPlanWebBase <> "/event/" <> slug)
      , discoveredEventImageUrl = imageUrl
      , discoveredEventType = "event"
      , discoveredEventStatus = status
      }

matchBuenPlanCity ::
  [EventDiscoveryCity] ->
  Text ->
  Maybe Text ->
  Maybe EventDiscoveryCity
matchBuenPlanCity cities title description =
  listToMaybe
    ( sortOn
        (negate . T.length . eventDiscoveryCityName)
        [ city
        | city <- cities
        , normalizeEventText (eventDiscoveryCityName city)
            `T.isInfixOf` searchable
        ]
    )
  where
    searchable = normalizeEventText (title <> " " <> fromMaybe "" description)
    normalizeEventText =
      T.unwords
        . T.words
        . T.map (\ch -> if isAlphaNum ch then ch else ' ')
        . T.toCaseFold

extractBuenPlanVenueName :: Text -> Maybe Text
extractBuenPlanVenueName description =
  listToMaybe
    [ venue
    | rawLine <- T.lines description
    , let (_, suffix) = T.breakOn "📍" rawLine
    , not (T.null suffix)
    , let venue = T.strip (T.drop 1 suffix)
    , not (T.null venue)
    , T.length venue <= 300
    ]

data StructuredFeedEvent = StructuredFeedEvent
  { structuredEventId :: Text
  , structuredEventTitle :: Text
  , structuredEventDescription :: Maybe Text
  , structuredEventStart :: UTCTime
  , structuredEventEnd :: Maybe UTCTime
  , structuredEventVenue :: Text
  , structuredEventAddress :: Maybe Text
  , structuredEventTicketUrl :: Maybe Text
  , structuredEventImageUrl :: Maybe Text
  , structuredEventPriceCents :: Maybe Int
  , structuredEventCurrency :: Maybe Text
  , structuredEventStatus :: Maybe Text
  , structuredEventType :: Maybe Text
  , structuredEventArtists :: [Text]
  }

instance FromJSON StructuredFeedEvent where
  parseJSON = withObject "StructuredFeedEvent" $ \o ->
    StructuredFeedEvent
      <$> o .: "id"
      <*> o .: "title"
      <*> o .:? "description"
      <*> o .: "start"
      <*> o .:? "end"
      <*> o .: "venue"
      <*> o .:? "address"
      <*> o .:? "ticketUrl"
      <*> o .:? "imageUrl"
      <*> o .:? "priceCents"
      <*> o .:? "currency"
      <*> o .:? "status"
      <*> o .:? "type"
      <*> (o .:? "artists" .!= [])

newtype StructuredFeedResponse =
  StructuredFeedResponse { structuredFeedEvents :: [StructuredFeedEvent] }

instance FromJSON StructuredFeedResponse where
  parseJSON value@(Array _) =
    StructuredFeedResponse <$> parseJSON value
  parseJSON value =
    withObject
      "StructuredFeedResponse"
      (\o -> StructuredFeedResponse <$> (o .:? "events" .!= []))
      value

fetchStructuredFeedEvents ::
  AppConfig ->
  Text ->
  Text ->
  Text ->
  EventDiscoveryCity ->
  UTCTime ->
  IO (Either Text [DiscoveredEvent])
fetchStructuredFeedEvents cfg sourceKey sourceType feedUrl city now =
  case validatePublicFeedUrl feedUrl of
    Left err -> pure (Left err)
    Right validatedUrl -> do
      requestResult <- try (parseRequest (T.unpack validatedUrl)) :: IO (Either HttpException Request)
      case requestResult of
        Left _ -> pure (Left "Could not build the venue feed request")
        Right rawRequest -> do
          let request =
                rawRequest
                  { requestHeaders =
                      [ ("Accept", structuredAcceptHeader sourceType)
                      , ("User-Agent", "TDF-Records-Event-Discovery/1.0")
                      ]
                  , redirectCount = 0
                  , responseTimeout = responseTimeoutMicro (20 * 1000000)
                  }
          responseResult <- requestTicketmasterPage request 0
          case responseResult of
            Left _ -> pure (Left "Venue feed request failed")
            Right response ->
              let httpStatus = statusCode (responseStatus response)
                  body = responseBody response
               in if httpStatus < 200 || httpStatus >= 300
                    then pure (Left ("Venue feed returned HTTP " <> T.pack (show httpStatus)))
                    else
                      if BL.length body > 10 * 1024 * 1024
                        then pure (Left "Venue feed exceeded the 10 MB safety limit")
                        else
                          pure
                            ( parseStructuredFeed
                                cfg
                                sourceKey
                                sourceType
                                city
                                now
                                body
                            )

structuredAcceptHeader :: Text -> BS8.ByteString
structuredAcceptHeader sourceType
  | T.toCaseFold sourceType == "ical" = "text/calendar"
  | otherwise = "application/json"

validatePublicFeedUrl :: Text -> Either Text Text
validatePublicFeedUrl rawUrl
  | not ("https://" `T.isPrefixOf` lowerUrl) =
      Left "Venue feed URL must use HTTPS"
  | isForbiddenFeedHost hostPart =
      Left "Venue feed URL must resolve to a public host"
  | otherwise =
      case normalizeConfiguredHttpsUrl "Venue feed URL" (T.unpack stripped) of
        Right (Just normalized) -> Right normalized
        _ -> Left "Venue feed URL is invalid"
  where
    stripped = T.strip rawUrl
    lowerUrl = T.toCaseFold stripped
    hostPart = T.takeWhile (/= '/') (T.drop 8 lowerUrl)

isForbiddenFeedHost :: Text -> Bool
isForbiddenFeedHost rawHost =
  host == "localhost"
    || host == "0.0.0.0"
    || host == "[::1]"
    || ".local" `T.isSuffixOf` host
    || any (`T.isPrefixOf` host) ["127.", "10.", "192.168.", "169.254."]
    || isPrivate172 host
    || any (`T.isPrefixOf` host) ["[fc", "[fd", "[fe8", "[fe9", "[fea", "[feb"]
  where
    host =
      case T.breakOn ":" rawHost of
        (plainHost, portSuffix)
          | not (T.null portSuffix) && not ("[" `T.isPrefixOf` rawHost) ->
              plainHost
        _ -> rawHost
    isPrivate172 value =
      case T.splitOn "." value of
        "172" : secondOctet : _ ->
          maybe False (\octet -> octet >= (16 :: Int) && octet <= 31) $
            readMaybe (T.unpack secondOctet)
        _ -> False

parseStructuredFeed ::
  AppConfig ->
  Text ->
  Text ->
  EventDiscoveryCity ->
  UTCTime ->
  BL.ByteString ->
  Either Text [DiscoveredEvent]
parseStructuredFeed cfg sourceKey sourceType city now body
  | T.toCaseFold sourceType == "json" =
      case eitherDecode body of
        Left _ -> Left "Venue JSON feed is invalid"
        Right response ->
          Right
            ( mapMaybe
                (normalizeStructuredEvent cfg sourceKey city now)
                (structuredFeedEvents response)
            )
  | T.toCaseFold sourceType == "ical" =
      case TE.decodeUtf8' (BL.toStrict body) of
        Left _ -> Left "Venue iCalendar feed is not valid UTF-8"
        Right calendarText ->
          Right
            ( mapMaybe
                (normalizeStructuredEvent cfg sourceKey city now)
                (parseIcsEvents calendarText)
            )
  | otherwise = Left "Unsupported venue feed type"

normalizeStructuredEvent ::
  AppConfig ->
  Text ->
  EventDiscoveryCity ->
  UTCTime ->
  StructuredFeedEvent ->
  Maybe DiscoveredEvent
normalizeStructuredEvent cfg sourceKey city now StructuredFeedEvent{..} = do
  externalId <- cleanIdentifier structuredEventId
  title <- cleanSingleLine 160 structuredEventTitle
  venueName <- cleanSingleLine 300 structuredEventVenue
  let endTime =
        case structuredEventEnd of
          Just candidate | candidate > structuredEventStart -> candidate
          _ -> addUTCTime (3 * 60 * 60) structuredEventStart
      lookaheadEnd =
        addUTCTime
          (fromIntegral (eventDiscoveryLookaheadDays cfg * 86400))
          now
  whenMaybe (structuredEventStart < now || structuredEventStart > lookaheadEnd) Nothing
  let currency =
        fromMaybe (defaultCurrency cfg) (structuredEventCurrency >>= normalizeCurrencyCode)
      status =
        normalizeStructuredStatus now structuredEventStart endTime structuredEventStatus
      artists =
        mapMaybe (normalizeStructuredArtist sourceKey) structuredEventArtists
  pure
    DiscoveredEvent
      { discoveredEventProvider = sourceKey
      , discoveredEventExternalId = externalId
      , discoveredEventTitle = title
      , discoveredEventDescription =
          structuredEventDescription >>= cleanMultiline 5000
      , discoveredEventStart = structuredEventStart
      , discoveredEventEnd = endTime
      , discoveredEventVenue =
          DiscoveredVenue
            { discoveredVenueExternalId =
                sourceKey <> ":" <> normalizeTokenText venueName
            , discoveredVenueName = venueName
            , discoveredVenueAddress =
                structuredEventAddress >>= cleanSingleLine 500
            , discoveredVenueCity = eventDiscoveryCityName city
            , discoveredVenueCountry = Nothing
            , discoveredVenueCountryCode =
                Just (eventDiscoveryCityCountryCode city)
            , discoveredVenueLatitude = Nothing
            , discoveredVenueLongitude = Nothing
            , discoveredVenuePhone = Nothing
            , discoveredVenueWebsite = Nothing
            , discoveredVenueState = Nothing
            , discoveredVenuePostalCode = Nothing
            , discoveredVenueImageUrl = Nothing
            }
      , discoveredEventArtists = artists
      , discoveredEventPriceCents =
          structuredEventPriceCents >>= nonNegativePrice
      , discoveredEventCurrency = currency
      , discoveredEventTicketUrl =
          structuredEventTicketUrl >>= normalizeHttpsUrl "Venue event URL"
      , discoveredEventImageUrl =
          structuredEventImageUrl >>= normalizeHttpsUrl "Venue event image"
      , discoveredEventType =
          fromMaybe "event" (structuredEventType >>= cleanSingleLine 80)
      , discoveredEventStatus = status
      }
  where
    nonNegativePrice price
      | price >= 0 = Just price
      | otherwise = Nothing

normalizeStructuredArtist :: Text -> Text -> Maybe DiscoveredArtist
normalizeStructuredArtist sourceKey rawName = do
  name <- cleanSingleLine 300 rawName
  pure
    DiscoveredArtist
      { discoveredArtistExternalId = sourceKey <> ":" <> normalizeTokenText name
      , discoveredArtistName = name
      , discoveredArtistGenres = []
      , discoveredArtistImageUrl = Nothing
      }

normalizeStructuredStatus ::
  UTCTime ->
  UTCTime ->
  UTCTime ->
  Maybe Text ->
  Text
normalizeStructuredStatus now startsAt endsAt rawStatus
  | endsAt < now = "completed"
  | startsAt <= now && endsAt >= now = "live"
  | normalized `elem` ["cancelled", "canceled"] = "cancelled"
  | normalized `elem` ["on_sale", "onsale", "confirmed"] = "on_sale"
  | otherwise = "announced"
  where
    normalized = maybe "" (T.toCaseFold . T.strip) rawStatus

parseIcsEvents :: Text -> [StructuredFeedEvent]
parseIcsEvents =
  mapMaybe eventFromProperties
    . splitIcsEvents
    . unfoldIcsLines
    . T.lines

unfoldIcsLines :: [Text] -> [Text]
unfoldIcsLines = reverse . foldl step []
  where
    step [] line = [T.dropWhileEnd (== '\r') line]
    step (previous : rest) line
      | " " `T.isPrefixOf` line || "\t" `T.isPrefixOf` line =
          (previous <> T.drop 1 (T.dropWhileEnd (== '\r') line)) : rest
      | otherwise = T.dropWhileEnd (== '\r') line : previous : rest

splitIcsEvents :: [Text] -> [[Text]]
splitIcsEvents = go False [] []
  where
    go _ current acc [] =
      reverse (if null current then acc else reverse current : acc)
    go False _ acc (line : remaining)
      | T.toUpper line == "BEGIN:VEVENT" = go True [] acc remaining
      | otherwise = go False [] acc remaining
    go True current acc (line : remaining)
      | T.toUpper line == "END:VEVENT" =
          go False [] (reverse current : acc) remaining
      | otherwise = go True (line : current) acc remaining

eventFromProperties :: [Text] -> Maybe StructuredFeedEvent
eventFromProperties properties = do
  eventId <- lookupIcsProperty "UID" properties >>= cleanIdentifier
  title <- lookupIcsProperty "SUMMARY" properties
  startsAt <- lookupIcsProperty "DTSTART" properties >>= parseIcsTime
  let endsAt = lookupIcsProperty "DTEND" properties >>= parseIcsTime
      location = fromMaybe "Venue" (lookupIcsProperty "LOCATION" properties)
  pure
    StructuredFeedEvent
      { structuredEventId = eventId
      , structuredEventTitle = unescapeIcsText title
      , structuredEventDescription =
          unescapeIcsText <$> lookupIcsProperty "DESCRIPTION" properties
      , structuredEventStart = startsAt
      , structuredEventEnd = endsAt
      , structuredEventVenue =
          T.strip (listToMaybe (T.splitOn "," (unescapeIcsText location)) `orText` location)
      , structuredEventAddress = Just (unescapeIcsText location)
      , structuredEventTicketUrl = lookupIcsProperty "URL" properties
      , structuredEventImageUrl = lookupIcsProperty "IMAGE" properties
      , structuredEventPriceCents = Nothing
      , structuredEventCurrency = Nothing
      , structuredEventStatus = lookupIcsProperty "STATUS" properties
      , structuredEventType = lookupIcsProperty "CATEGORIES" properties
      , structuredEventArtists = []
      }
  where
    orText (Just value) _ = value
    orText Nothing fallback = fallback

lookupIcsProperty :: Text -> [Text] -> Maybe Text
lookupIcsProperty wanted =
  listToMaybe
    . mapMaybe propertyValue
  where
    propertyValue line =
      let (rawKey, rawValueWithColon) = T.breakOn ":" line
          key = T.toUpper (T.takeWhile (/= ';') rawKey)
       in if key == wanted && not (T.null rawValueWithColon)
            then Just (T.drop 1 rawValueWithColon)
            else Nothing

parseIcsTime :: Text -> Maybe UTCTime
parseIcsTime rawValue =
  iso8601ParseM (T.unpack rawValue)
    <|> parseTimeM True defaultTimeLocale "%Y%m%dT%H%M%SZ" (T.unpack rawValue)

unescapeIcsText :: Text -> Text
unescapeIcsText =
  T.replace "\\n" "\n"
    . T.replace "\\N" "\n"
    . T.replace "\\," ","
    . T.replace "\\;" ";"
    . T.replace "\\\\" "\\"

normalizeUserCities :: [Text] -> [Text]
normalizeUserCities rawCities =
  map snd . sortOn fst . Map.toList $
    foldl addCity Map.empty rawCities
  where
    addCity cities rawCity =
      case cleanSingleLine 120 rawCity of
        Nothing -> cities
        Just city -> Map.insertWith (\_ existing -> existing) (normalizeCityKey city) city cities

normalizeCityKey :: Text -> Text
normalizeCityKey = T.toCaseFold . T.unwords . T.words . T.strip

loadActiveUserCities :: ConnectionPool -> IO [Text]
loadActiveUserCities pool =
  map eventDiscoveryCityName <$> loadSubscribedDiscoveryCities pool

loadSubscribedDiscoveryCities :: ConnectionPool -> IO [EventDiscoveryCity]
loadSubscribedDiscoveryCities pool = do
  rows <-
    runSqlPool
      ( rawSql
          "SELECT DISTINCT city.name, city.country_code, city.time_zone\
          \ FROM event_city_subscription subscription\
          \ INNER JOIN event_city city ON city.id = subscription.city_id\
          \ INNER JOIN user_credential credential\
          \   ON credential.party_id::text = subscription.party_id\
          \ WHERE credential.active = TRUE\
          \ ORDER BY city.country_code, city.name"
          [] ::
          SqlPersistT
            IO
            [(Single Text, Single Text, Single (Maybe Text))]
      )
      pool
  pure
    [ EventDiscoveryCity
        { eventDiscoveryCityName = city
        , eventDiscoveryCityCountryCode = T.toUpper countryCode
        , eventDiscoveryCityTimeZone = timeZone
        }
    | (Single city, Single countryCode, Single timeZone) <- rows
    , city `elem` normalizeUserCities [city]
    ]

buildTicketmasterRequestUrl ::
  Text ->
  Maybe Text ->
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  Int ->
  String
buildTicketmasterRequestUrl apiBase countryCode apiKey city startsAt endsAt pageNumber =
  T.unpack (base <> "/events.json" <> query)
  where
    base = T.dropWhileEnd (== '/') apiBase
    queryPairs =
      [ ("apikey", TE.encodeUtf8 apiKey)
      , ("city", TE.encodeUtf8 city)
      , ("startDateTime", TE.encodeUtf8 (T.pack (iso8601Show startsAt)))
      , ("endDateTime", TE.encodeUtf8 (T.pack (iso8601Show endsAt)))
      , ("includeTBA", "no")
      , ("includeTBD", "no")
      , ("includeTest", "no")
      , ("sort", "date,asc")
      , ("size", "100")
      , ("page", TE.encodeUtf8 (T.pack (show pageNumber)))
      ]
        ++ maybe [] (\country -> [("countryCode", TE.encodeUtf8 country)]) countryCode
    query = TE.decodeUtf8 (renderSimpleQuery True queryPairs)

fetchTicketmasterEvents ::
  AppConfig ->
  Text ->
  Text ->
  UTCTime ->
  IO (Either Text [DiscoveredEvent])
fetchTicketmasterEvents cfg apiKey city now =
  fetchTicketmasterEventsWithCountry
    cfg
    (eventDiscoveryCountryCode cfg)
    apiKey
    city
    now

fetchTicketmasterEventsForCity ::
  AppConfig ->
  Text ->
  EventDiscoveryCity ->
  UTCTime ->
  IO (Either Text [DiscoveredEvent])
fetchTicketmasterEventsForCity cfg apiKey city now =
  fmap
    ( fmap
        ( map
            ( \event ->
                event
                  { discoveredEventVenue =
                      (discoveredEventVenue event)
                        { discoveredVenueCountryCode =
                            Just (eventDiscoveryCityCountryCode city)
                        }
                  }
            )
        )
    )
    ( fetchTicketmasterEventsWithCountry
        cfg
        (Just (eventDiscoveryCityCountryCode city))
        apiKey
        (eventDiscoveryCityName city)
        now
    )

fetchTicketmasterEventsWithCountry ::
  AppConfig ->
  Maybe Text ->
  Text ->
  Text ->
  UTCTime ->
  IO (Either Text [DiscoveredEvent])
fetchTicketmasterEventsWithCountry cfg countryCode apiKey city now =
  fetchPage 0 []
  where
    endTime = addUTCTime (fromIntegral (eventDiscoveryLookaheadDays cfg * 86400)) now
    fetchPage pageNumber collected
      | pageNumber >= eventDiscoveryMaxPagesPerCity cfg = pure (Right collected)
      | otherwise = do
          let url =
                buildTicketmasterRequestUrl
                  (ticketmasterApiBase cfg)
                  countryCode
                  apiKey
                  city
                  now
                  endTime
                  pageNumber
          requestResult <- try (parseRequest url) :: IO (Either HttpException Request)
          case requestResult of
            Left _ -> pure (Left "Could not build the Ticketmaster event request")
            Right rawRequest -> do
              let request =
                    rawRequest
                      { requestHeaders =
                          [ ("Accept", "application/json")
                          , ("User-Agent", "TDF-Records-Event-Discovery/1.0")
                          ]
                      , responseTimeout = responseTimeoutMicro (20 * 1000000)
                      }
              responseResult <- requestTicketmasterPage request 0
              case responseResult of
                Left _ -> pure (Left "Ticketmaster request failed")
                Right response ->
                  let httpStatus = statusCode (responseStatus response)
                      body = responseBody response
                   in if httpStatus < 200 || httpStatus >= 300
                        then pure (Left ("Ticketmaster returned HTTP " <> T.pack (show httpStatus)))
                        else if BL.length body > 25 * 1024 * 1024
                          then pure (Left "Ticketmaster response exceeded the 25 MB safety limit")
                          else case eitherDecode body of
                            Left _ -> pure (Left "Ticketmaster returned an invalid event response")
                            Right decoded -> do
                              let decodedEvents = ticketmasterResponseEvents decoded
                                  normalized = normalizeTicketmasterResponse (defaultCurrency cfg) city now decoded
                                  nextCollected = collected ++ normalized
                                  totalPages = ticketmasterResponseTotalPages decoded
                              if ticketmasterResponseRawEventCount decoded > 0 && null decodedEvents
                                then pure (Left "Ticketmaster returned no usable event records")
                                else
                                  if pageNumber + 1 >= totalPages
                                    then pure (Right nextCollected)
                                    else fetchPage (pageNumber + 1) nextCollected

requestTicketmasterPage ::
  Request ->
  Int ->
  IO (Either HttpException (Response BL.ByteString))
requestTicketmasterPage request retryCount = do
  -- Ticketmaster's default quota is five requests per second. Keeping every
  -- request at least 250ms apart leaves headroom for clock/network variance.
  threadDelay 250000
  responseResult <- try (httpLbs request sharedTlsManager)
  case responseResult of
    Right response
      | statusCode (responseStatus response) == 429 && retryCount < 1 -> do
          threadDelay (ticketmasterRetryDelayMicros response)
          requestTicketmasterPage request (retryCount + 1)
    _ -> pure responseResult

ticketmasterRetryDelayMicros :: Response body -> Int
ticketmasterRetryDelayMicros response =
  case lookup "Retry-After" (responseHeaders response) >>= readMaybe . BS8.unpack of
    Just seconds -> max 1000000 (min (60 * 1000000) (seconds * 1000000))
    Nothing -> 2000000

normalizeTicketmasterResponse :: Text -> Text -> UTCTime -> TicketmasterResponse -> [DiscoveredEvent]
normalizeTicketmasterResponse configuredDefault requestedCity now =
  mapMaybe (normalizeTicketmasterEvent configuredDefault requestedCity now) . ticketmasterResponseEvents

normalizeTicketmasterEvent :: Text -> Text -> UTCTime -> TicketmasterEvent -> Maybe DiscoveredEvent
normalizeTicketmasterEvent configuredDefault requestedCity now TicketmasterEvent{..} = do
  externalId <- cleanIdentifier ticketmasterEventId
  title <- cleanSingleLine 160 ticketmasterEventName
  embedded <- ticketmasterEventEmbedded
  venue <-
    listToMaybe
      (mapMaybe (normalizeTicketmasterVenue requestedCity) (ticketmasterVenues embedded))
  startText <- ticketmasterStartDateTime (ticketmasterDatesStart ticketmasterEventDates)
  start <- iso8601ParseM (T.unpack startText)
  whenMaybe (start < now) Nothing
  let parsedEnd = do
        endData <- ticketmasterDatesEnd ticketmasterEventDates
        endText <- ticketmasterEndDateTime endData
        iso8601ParseM (T.unpack endText)
      end = case parsedEnd of
        Just candidate | candidate > start -> candidate
        _ -> addUTCTime (3 * 60 * 60) start
      classifications = ticketmasterEventClassifications
      segmentName = firstClassificationName ticketmasterSegment classifications
      genreNames = classificationGenreNames classifications
      artists =
        nubBy ((==) `on` discoveredArtistExternalId) $
          mapMaybe (normalizeTicketmasterArtist genreNames) (ticketmasterAttractions embedded)
      sourceStatus = ticketmasterStatusCode <$> ticketmasterDatesStatus ticketmasterEventDates
      publicSaleStart =
        ticketmasterEventSales
          >>= ticketmasterPublicSale
          >>= ticketmasterPublicSaleStart
          >>= iso8601ParseM . T.unpack
      publicSaleEnd =
        ticketmasterEventSales
          >>= ticketmasterPublicSale
          >>= ticketmasterPublicSaleEnd
          >>= iso8601ParseM . T.unpack
      normalizedSource = maybe "" (T.toCaseFold . T.strip) sourceStatus
      saleWindowOpen =
        maybe True (<= now) publicSaleStart
          && maybe True (>= now) publicSaleEnd
      saleOpen =
        normalizedSource `notElem` ["offsale", "canceled", "cancelled", "postponed"]
          && (normalizedSource == "onsale" || normalizedSource == "")
          && saleWindowOpen
      rawTicketUrl = ticketmasterEventUrl >>= normalizeHttpsUrl "Ticketmaster event URL"
      ticketUrl = if saleOpen then rawTicketUrl else Nothing
      eventStatus = normalizeEventStatus now start end sourceStatus saleOpen
      eventType = normalizeEventType title segmentName
      (priceCents, currency) = normalizePrice configuredDefault ticketmasterEventPrices
      description =
        joinDescription
          [ ticketmasterEventInfo >>= cleanMultiline 5000
          , ticketmasterEventNote >>= cleanMultiline 5000
          ]
  pure
    DiscoveredEvent
      { discoveredEventProvider = providerName
      , discoveredEventExternalId = externalId
      , discoveredEventTitle = title
      , discoveredEventDescription = description
      , discoveredEventStart = start
      , discoveredEventEnd = end
      , discoveredEventVenue = venue
      , discoveredEventArtists = artists
      , discoveredEventPriceCents = priceCents
      , discoveredEventCurrency = currency
      , discoveredEventTicketUrl = ticketUrl
      , discoveredEventImageUrl = selectImage ticketmasterEventImages
      , discoveredEventType = eventType
      , discoveredEventStatus = eventStatus
      }

whenMaybe :: Bool -> Maybe a -> Maybe ()
whenMaybe condition fallback = if condition then () <$ fallback else Just ()

normalizeTicketmasterVenue :: Text -> TicketmasterVenue -> Maybe DiscoveredVenue
normalizeTicketmasterVenue requestedCity TicketmasterVenue{..} = do
  externalId <- cleanIdentifier ticketmasterVenueId
  name <- cleanSingleLine 300 ticketmasterVenueName
  city <- ticketmasterVenueCity >>= cleanSingleLine 120 . namedValue
  whenMaybe (normalizeCityKey city /= normalizeCityKey requestedCity) Nothing
  let (latitude, longitude) = normalizeCoordinates ticketmasterVenueLocation
  pure
    DiscoveredVenue
      { discoveredVenueExternalId = externalId
      , discoveredVenueName = name
      , discoveredVenueAddress =
          ticketmasterVenueAddress
            >>= ticketmasterAddressLine
            >>= cleanSingleLine 500
      , discoveredVenueCity = city
      , discoveredVenueCountry =
          ticketmasterVenueCountry >>= cleanSingleLine 120 . namedValue
      , discoveredVenueCountryCode = Nothing
      , discoveredVenueLatitude = latitude
      , discoveredVenueLongitude = longitude
      , discoveredVenuePhone =
          ticketmasterVenueBoxOffice
            >>= ticketmasterBoxOfficePhone
            >>= cleanSingleLine 120
      , discoveredVenueWebsite =
          ticketmasterVenueUrl >>= normalizeHttpsUrl "Ticketmaster venue URL"
      , discoveredVenueState =
          ticketmasterVenueState >>= cleanSingleLine 120 . namedValue
      , discoveredVenuePostalCode =
          ticketmasterVenuePostalCode >>= cleanSingleLine 40
      , discoveredVenueImageUrl = selectImage ticketmasterVenueImages
      }

normalizeTicketmasterArtist :: [Text] -> TicketmasterAttraction -> Maybe DiscoveredArtist
normalizeTicketmasterArtist fallbackGenres TicketmasterAttraction{..} = do
  externalId <- cleanIdentifier ticketmasterAttractionId
  name <- cleanSingleLine 300 ticketmasterAttractionName
  let attractionGenres = classificationGenreNames ticketmasterAttractionClassifications
  pure
    DiscoveredArtist
      { discoveredArtistExternalId = externalId
      , discoveredArtistName = name
      , discoveredArtistGenres =
          if null attractionGenres then fallbackGenres else attractionGenres
      , discoveredArtistImageUrl = selectImage ticketmasterAttractionImages
      }

firstClassificationName ::
  (TicketmasterClassification -> Maybe NamedValue) ->
  [TicketmasterClassification] ->
  Maybe Text
firstClassificationName accessor =
  listToMaybe . mapMaybe (fmap namedValue . accessor)

classificationGenreNames :: [TicketmasterClassification] -> [Text]
classificationGenreNames classifications =
  nubBy ((==) `on` T.toCaseFold) . mapMaybe (cleanSingleLine 120) $
    concatMap
      (\classification ->
        catMaybes
          [ namedValue <$> ticketmasterGenre classification
          , namedValue <$> ticketmasterSubGenre classification
          ]
      )
      classifications

normalizeEventType :: Text -> Maybe Text -> Text
normalizeEventType title segment
  | "festival" `T.isInfixOf` T.toCaseFold title = "festival"
  | maybe False ((== "music") . T.toCaseFold . T.strip) segment = "concert"
  | otherwise = "other"

normalizeEventStatus ::
  UTCTime ->
  UTCTime ->
  UTCTime ->
  Maybe Text ->
  Bool ->
  Text
normalizeEventStatus now startsAt endsAt sourceStatus saleOpen
  | normalizedSource `elem` ["cancelled", "canceled"] = "cancelled"
  | now >= startsAt && now <= endsAt = "live"
  | saleOpen = "on_sale"
  | otherwise = "announced"
  where
    normalizedSource = maybe "" (T.toCaseFold . T.strip) sourceStatus

normalizePrice :: Text -> [TicketmasterPriceRange] -> (Maybe Int, Text)
normalizePrice configuredDefault ranges =
  case
    sortOn fst
      [ (cents, currency)
      | TicketmasterPriceRange (Just rawCurrency) (Just minimumPrice) <- ranges
      , Just currency <- [normalizeCurrencyCode rawCurrency]
      , not (isNaN minimumPrice || isInfinite minimumPrice)
      , minimumPrice >= 0
      , let decimalPlaces = maybe 2 currencyDecimalPlaces (currencyDefinition currency)
      , let minorUnitFactor = 10 ^ decimalPlaces :: Integer
      , minimumPrice <= fromIntegral (maxBound :: Int) / fromIntegral minorUnitFactor
      , let centsInteger = round (minimumPrice * fromIntegral minorUnitFactor) :: Integer
      , centsInteger <= fromIntegral (maxBound :: Int)
      , let cents = fromIntegral centsInteger
      ] of
      firstPrice : _ -> (Just (fst firstPrice), snd firstPrice)
      [] -> (Nothing, configuredDefault)

normalizeCoordinates :: Maybe TicketmasterLocation -> (Maybe Double, Maybe Double)
normalizeCoordinates location =
  case (rawLatitude, rawLongitude) of
    (Just latitude, Just longitude)
      | validLatitude latitude && validLongitude longitude ->
          (Just latitude, Just longitude)
    _ -> (Nothing, Nothing)
  where
    rawLatitude = location >>= ticketmasterLatitude >>= readMaybe . T.unpack
    rawLongitude = location >>= ticketmasterLongitude >>= readMaybe . T.unpack
    finite value = not (isNaN value || isInfinite value)
    validLatitude value = finite value && value >= (-90) && value <= 90
    validLongitude value = finite value && value >= (-180) && value <= 180

selectImage :: [TicketmasterImage] -> Maybe Text
selectImage images =
  ticketmasterImageUrl <$> safeMaximum validImages
  where
    usable image = do
      url <- normalizeHttpsUrl "Ticketmaster image URL" (ticketmasterImageUrl image)
      pure image{ticketmasterImageUrl = url}
    validImages = mapMaybe usable (filter (not . ticketmasterImageFallback) images)
    safeMaximum [] = Nothing
    safeMaximum values =
      Just
        ( maximumBy
            (comparing (\image -> ticketmasterImageWidth image * ticketmasterImageHeight image))
            values
        )

normalizeHttpsUrl :: String -> Text -> Maybe Text
normalizeHttpsUrl fieldName rawUrl =
  case normalizeConfiguredHttpsUrl fieldName (T.unpack upgraded) of
    Right (Just normalized) -> Just normalized
    _ -> Nothing
  where
    stripped = T.strip rawUrl
    upgraded = fromMaybe stripped (T.stripPrefix "http://" stripped >>= (Just . ("https://" <>)))

cleanIdentifier :: Text -> Maybe Text
cleanIdentifier rawValue = do
  value <- cleanSingleLine 256 rawValue
  if T.any (\ch -> ch == '/' || ch == '?' || ch == '#') value
    then Nothing
    else Just value

cleanSingleLine :: Int -> Text -> Maybe Text
cleanSingleLine maximumLength rawValue =
  let value = T.unwords (T.words (T.strip rawValue))
   in if T.null value
        || T.length value > maximumLength
        || T.any isUnsupportedProviderTextChar rawValue
        then Nothing
        else Just value

cleanMultiline :: Int -> Text -> Maybe Text
cleanMultiline maximumLength rawValue =
  let value = T.strip rawValue
   in if T.null value
        || T.length value > maximumLength
        || T.any isUnsupportedProviderMultilineChar rawValue
        then Nothing
        else Just value

isUnsupportedProviderTextChar :: Char -> Bool
isUnsupportedProviderTextChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

isUnsupportedProviderMultilineChar :: Char -> Bool
isUnsupportedProviderMultilineChar ch =
  (isControl ch && ch `notElem` ("\n\r\t" :: String))
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

joinDescription :: [Maybe Text] -> Maybe Text
joinDescription parts =
  case catMaybes parts of
    [] -> Nothing
    values -> Just (T.intercalate "\n\n" values)

syncDiscoveredEvent :: ConnectionPool -> UTCTime -> DiscoveredEvent -> IO DiscoverySyncStats
syncDiscoveredEvent pool now event =
  runSqlPool (syncDiscoveredEventDb now event) pool

-- | Keep imported lifecycle state aligned with the current subscription scope.
-- Past imports complete automatically; future imports leave the public feed as
-- soon as no active subscription covers any of their source references.
reconcileImportedEvents ::
  ConnectionPool ->
  UTCTime ->
  [EventDiscoveryCity] ->
  IO Int
reconcileImportedEvents pool now activeCities =
  runSqlPool reconcile pool
  where
    activeCityKeys =
      Map.fromList
        [ ( ( normalizeCityKey (eventDiscoveryCityName city)
            , T.toUpper (eventDiscoveryCityCountryCode city)
            )
          , ()
          )
        | city <- activeCities
        ]
    reconcile = do
      refs <- selectList [] []
      let eventKeys =
            Map.keys
              ( Map.fromList
                  [ (Social.externalEventRefEventId ref, ())
                  | Entity _ ref <- refs
                  ]
              )
      changes <- forM eventKeys $ \eventKey -> do
        maybeEvent <- get eventKey
        case maybeEvent of
          Nothing -> pure 0
          Just eventRow
            | Social.socialEventEndTime eventRow < now -> do
                updateImportedLifecycle eventKey eventRow "completed" False
                pure 1
            | not (eventCoveredBySubscription eventKey refs) -> do
                updateImportedLifecycle eventKey eventRow "out_of_scope" False
                pure 1
            | otherwise -> do
                refreshCanonicalVisibility now eventKey
                pure 0
      pure (sum changes)

    eventCoveredBySubscription eventKey refs =
      any
        ( \ref ->
            Social.externalEventRefEventId ref == eventKey
              && refCityIsActive ref
        )
        (map entityVal refs)

    refCityIsActive ref =
      let cityKey = normalizeCityKey (Social.externalEventRefCity ref)
       in case Social.externalEventRefCountryCode ref of
            Just country ->
              Map.member (cityKey, T.toUpper country) activeCityKeys
            Nothing ->
              any
                (\((candidateCity, _), _) -> candidateCity == cityKey)
                (Map.toList activeCityKeys)

    updateImportedLifecycle eventKey eventRow status isPublic =
      update
        eventKey
        [ Social.SocialEventMetadata =.
            updateImportedEventMetadata status isPublic (Social.socialEventMetadata eventRow)
        , Social.SocialEventUpdatedAt =. now
        ]

-- | Mark provider records that disappeared from a successful full run. A
-- record remains usable for one missed run to absorb transient upstream
-- omissions; the second consecutive miss removes only that source option.
reconcileProviderEvents ::
  ConnectionPool ->
  UTCTime ->
  Text ->
  [EventDiscoveryCity] ->
  [Text] ->
  IO Int
reconcileProviderEvents pool now provider targetCities seenExternalIds =
  runSqlPool reconcile pool
  where
    seen =
      Map.fromList
        [ (externalId, ())
        | externalId <- seenExternalIds
        ]
    reconcile = do
      allProviderRefs <-
        selectList [Social.ExternalEventRefProvider ==. provider] []
      let refs =
            filter
              (refMatchesTargetCity . entityVal)
              allProviderRefs
      changed <- forM refs $ \(Entity refKey ref) ->
        if Map.member (Social.externalEventRefExternalId ref) seen
          then pure 0
          else do
            let nextMissing = Social.externalEventRefMissingRuns ref + 1
                nextStatus =
                  if nextMissing >= 2
                    then "missing"
                    else Social.externalEventRefSourceStatus ref
            update
              refKey
              [ Social.ExternalEventRefMissingRuns =. nextMissing
              , Social.ExternalEventRefSourceStatus =. nextStatus
              ]
            pure 1
      let touchedEventKeys =
            Map.keys
              ( Map.fromList
                  [ (Social.externalEventRefEventId ref, ())
                  | Entity _ ref <- refs
                  ]
              )
      forM_ touchedEventKeys (refreshCanonicalVisibility now)
      pure (sum changed)

    refMatchesTargetCity ref =
      any
        ( \city ->
            normalizeCityKey (Social.externalEventRefCity ref)
              == normalizeCityKey (eventDiscoveryCityName city)
              && maybe
                True
                ( (== T.toUpper (eventDiscoveryCityCountryCode city))
                    . T.toUpper
                    . T.strip
                )
                (Social.externalEventRefCountryCode ref)
        )
        targetCities

updateImportedEventMetadata :: Text -> Bool -> Maybe Text -> Maybe Text
updateImportedEventMetadata status isPublic rawMetadata =
  updateImportedEventMetadataWithTicket status isPublic Nothing rawMetadata

updateImportedEventMetadataWithTicket ::
  Text ->
  Bool ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text
updateImportedEventMetadataWithTicket status isPublic replacementTicketUrl rawMetadata =
  Just . TE.decodeUtf8 . BL.toStrict . encode $
    Object
      ( AesonKeyMap.insert "ticketUrl" ticketUrlValue
          . AesonKeyMap.insert "isPublic" (Bool isPublic)
          . AesonKeyMap.insert "eventStatus" (String status)
          $ originalObject
      )
  where
    originalObject =
      case rawMetadata >>= decodeStrict' . TE.encodeUtf8 of
        Just (Object value) -> value
        _ -> AesonKeyMap.empty
    ticketUrlValue =
      if isPublic
        then
          maybe
            (fromMaybe Null (AesonKeyMap.lookup "ticketUrl" originalObject))
            String
            replacementTicketUrl
        else Null

refreshCanonicalVisibility ::
  UTCTime ->
  Social.SocialEventId ->
  SqlPersistT IO ()
refreshCanonicalVisibility now eventKey = do
  maybeEvent <- get eventKey
  case maybeEvent of
    Nothing -> pure ()
    Just eventRow -> do
      refs <- selectList [Social.ExternalEventRefEventId ==. eventKey] []
      rankedRefs <-
        forM refs $ \entity@(Entity _ ref) -> do
          priority <- eventSourcePriority (Social.externalEventRefProvider ref)
          pure (priority, entity)
      let activeRefs =
            [ (priority, ref)
            | (priority, Entity _ ref) <- rankedRefs
            , sourceRefIsActive ref
            ]
          bestActiveRef =
            case sortOn (negate . fst) activeRefs of
              (_, ref) : _ -> Just ref
              [] -> Nothing
          ended = Social.socialEventEndTime eventRow < now
          isPublic = not ended && maybe False (const True) bestActiveRef
          status
            | ended = "completed"
            | otherwise =
                maybe
                  "unavailable"
                  (normalizeImportedSourceStatus . Social.externalEventRefSourceStatus)
                  bestActiveRef
          ticketUrl = bestActiveRef >>= Social.externalEventRefSourceUrl
      update
        eventKey
        [ Social.SocialEventMetadata =.
            updateImportedEventMetadataWithTicket
              status
              isPublic
              ticketUrl
              (Social.socialEventMetadata eventRow)
        , Social.SocialEventUpdatedAt =. now
        ]

sourceRefIsActive :: Social.ExternalEventRef -> Bool
sourceRefIsActive ref =
  Social.externalEventRefMissingRuns ref < 2
    && normalizedStatus
      `notElem`
        [ "cancelled"
        , "canceled"
        , "completed"
        , "missing"
        , "removed"
        , "unavailable"
        ]
  where
    normalizedStatus =
      T.toCaseFold (T.strip (Social.externalEventRefSourceStatus ref))

normalizeImportedSourceStatus :: Text -> Text
normalizeImportedSourceStatus rawStatus
  | normalized `elem` ["onsale", "on_sale", "confirmed"] = "on_sale"
  | normalized `elem` ["live", "announced", "postponed"] = normalized
  | otherwise = "announced"
  where
    normalized = T.toCaseFold (T.strip rawStatus)

syncDiscoveredEventDb :: UTCTime -> DiscoveredEvent -> SqlPersistT IO DiscoverySyncStats
syncDiscoveredEventDb now DiscoveredEvent{..} = do
  (venueKey, venueCreated) <-
    upsertDiscoveredVenue discoveredEventProvider now discoveredEventVenue
  artistResults <-
    forM
      discoveredEventArtists
      (upsertDiscoveredArtist discoveredEventProvider now)
  let artistKeys = map fst artistResults
      artistsCreated = length (filter snd artistResults)
      metadata = encodeEventMetadataForImport DiscoveredEvent{..}
  existingRef <-
    getBy
      (Social.UniqueExternalEventRef discoveredEventProvider discoveredEventExternalId)
  (eventKey, eventCreated) <-
    case existingRef of
      Just (Entity refKey ref) -> do
        let existingEventKey = Social.externalEventRefEventId ref
        shouldReplace <-
          providerShouldReplaceCanonical discoveredEventProvider existingEventKey
        if shouldReplace
          then
            update
              existingEventKey
              [ Social.SocialEventTitle =. discoveredEventTitle
              , Social.SocialEventDescription =. discoveredEventDescription
              , Social.SocialEventVenueId =. Just venueKey
              , Social.SocialEventStartTime =. discoveredEventStart
              , Social.SocialEventEndTime =. discoveredEventEnd
              , Social.SocialEventPriceCents =. discoveredEventPriceCents
              , Social.SocialEventMetadata =. metadata
              , Social.SocialEventUpdatedAt =. now
              ]
          else pure ()
        update
          refKey
          [ Social.ExternalEventRefCity =. discoveredVenueCity discoveredEventVenue
          , Social.ExternalEventRefCountryCode =.
              discoveredVenueCountryCode discoveredEventVenue
          , Social.ExternalEventRefSourceUrl =. discoveredEventTicketUrl
          , Social.ExternalEventRefPriceCents =. discoveredEventPriceCents
          , Social.ExternalEventRefCurrency =. Just discoveredEventCurrency
          , Social.ExternalEventRefLastSeenAt =. now
          , Social.ExternalEventRefMissingRuns =. 0
          , Social.ExternalEventRefSourceStatus =. discoveredEventStatus
          ]
        pure (existingEventKey, False)
      Nothing -> do
        mergeCandidate <- findCanonicalEventCandidate DiscoveredEvent{..}
        (newEventKey, created) <-
          case mergeCandidate of
            Just candidateKey -> do
              shouldReplace <- providerShouldReplaceCanonical discoveredEventProvider candidateKey
              if shouldReplace
                then
                  update
                    candidateKey
                    [ Social.SocialEventTitle =. discoveredEventTitle
                    , Social.SocialEventDescription =. discoveredEventDescription
                    , Social.SocialEventVenueId =. Just venueKey
                    , Social.SocialEventStartTime =. discoveredEventStart
                    , Social.SocialEventEndTime =. discoveredEventEnd
                    , Social.SocialEventPriceCents =. discoveredEventPriceCents
                    , Social.SocialEventMetadata =. metadata
                    , Social.SocialEventUpdatedAt =. now
                    ]
                else pure ()
              pure (candidateKey, False)
            Nothing -> do
              inserted <-
                insert
                  Social.SocialEvent
                    { Social.socialEventOrganizerPartyId = Just systemOrganizerId
                    , Social.socialEventTitle = discoveredEventTitle
                    , Social.socialEventDescription = discoveredEventDescription
                    , Social.socialEventVenueId = Just venueKey
                    , Social.socialEventStartTime = discoveredEventStart
                    , Social.socialEventEndTime = discoveredEventEnd
                    , Social.socialEventPriceCents = discoveredEventPriceCents
                    , Social.socialEventCapacity = Nothing
                    , Social.socialEventMetadata = metadata
                    , Social.socialEventCreatedAt = now
                    , Social.socialEventUpdatedAt = now
                    }
              pure (inserted, True)
        _ <-
          insert
            Social.ExternalEventRef
              { Social.externalEventRefProvider = discoveredEventProvider
              , Social.externalEventRefExternalId = discoveredEventExternalId
              , Social.externalEventRefEventId = newEventKey
              , Social.externalEventRefCity = discoveredVenueCity discoveredEventVenue
              , Social.externalEventRefCountryCode =
                  discoveredVenueCountryCode discoveredEventVenue
              , Social.externalEventRefSourceUrl = discoveredEventTicketUrl
              , Social.externalEventRefPriceCents = discoveredEventPriceCents
              , Social.externalEventRefCurrency = Just discoveredEventCurrency
              , Social.externalEventRefLastSeenAt = now
              , Social.externalEventRefMissingRuns = 0
              , Social.externalEventRefSourceStatus = discoveredEventStatus
              }
        pure (newEventKey, created)
  forM_ artistKeys $ \artistKey -> do
    _ <- insertUnique (Social.EventArtist eventKey artistKey Nothing)
    pure ()
  pure
    emptyDiscoverySyncStats
      { discoveryEventsSeen = 1
      , discoveryEventsCreated = if eventCreated then 1 else 0
      , discoveryEventsUpdated = if eventCreated then 0 else 1
      , discoveryVenuesCreated = if venueCreated then 1 else 0
      , discoveryArtistsCreated = artistsCreated
      }

findCanonicalEventCandidate ::
  DiscoveredEvent ->
  SqlPersistT IO (Maybe Social.SocialEventId)
findCanonicalEventCandidate discovered = do
  refs <- selectList [] [LimitTo 5000]
  matches <- fmap catMaybes . forM refs $ \(Entity _ ref) -> do
    eventRow <- get (Social.externalEventRefEventId ref)
    case eventRow of
      Nothing -> pure Nothing
      Just existing
        | normalizeCityKey (Social.externalEventRefCity ref)
            /= normalizeCityKey
              (discoveredVenueCity (discoveredEventVenue discovered)) ->
            pure Nothing
        | abs
            ( diffUTCTime
                (Social.socialEventStartTime existing)
                (discoveredEventStart discovered)
            )
            > 90 * 60 ->
            pure Nothing
        | otherwise -> do
            venueMatches <- canonicalVenueMatches existing
            artistMatches <- canonicalArtistMatches (Social.externalEventRefEventId ref)
            let titleScore =
                  normalizedTokenSimilarity
                    (Social.socialEventTitle existing)
                    (discoveredEventTitle discovered)
                veryClose =
                  abs
                    ( diffUTCTime
                        (Social.socialEventStartTime existing)
                        (discoveredEventStart discovered)
                    )
                    <= 15 * 60
                highConfidence =
                  (titleScore >= 0.92 && veryClose)
                    || (titleScore >= 0.80 && (venueMatches || artistMatches))
            pure
              ( if highConfidence
                  then Just (Social.externalEventRefEventId ref)
                  else Nothing
              )
  pure (listToMaybe matches)
  where
    canonicalVenueMatches existing =
      case Social.socialEventVenueId existing of
        Nothing -> pure False
        Just venueKey -> do
          venue <- get venueKey
          pure $
            maybe
              False
              ( \venueRow ->
                  normalizedTokenSimilarity
                    (Social.venueName venueRow)
                    (discoveredVenueName (discoveredEventVenue discovered))
                    >= 0.85
              )
              venue

    canonicalArtistMatches eventKey = do
      links <- selectList [Social.EventArtistEventId ==. eventKey] []
      names <-
        fmap catMaybes . forM links $ \(Entity _ link) ->
          fmap Social.artistProfileName <$> get (Social.eventArtistArtistId link)
      let importedNames =
            map (normalizeTokenText . discoveredArtistName)
              (discoveredEventArtists discovered)
          existingNames = map normalizeTokenText names
      pure (any (`elem` existingNames) importedNames)

providerShouldReplaceCanonical ::
  Text ->
  Social.SocialEventId ->
  SqlPersistT IO Bool
providerShouldReplaceCanonical provider eventKey = do
  newPriority <- eventSourcePriority provider
  refs <- selectList [Social.ExternalEventRefEventId ==. eventKey] []
  existingPriorities <-
    forM refs (eventSourcePriority . Social.externalEventRefProvider . entityVal)
  pure (null existingPriorities || newPriority >= maximum existingPriorities)

eventSourcePriority :: Text -> SqlPersistT IO Int
eventSourcePriority provider = do
  source <- getBy (Social.UniqueEventDiscoverySource provider)
  pure $
    case source of
      Just (Entity _ row) -> Social.eventDiscoverySourcePriority row
      Nothing
        | provider == "buenplan" -> 200
        | provider == "ticketmaster" -> 300
        | otherwise -> 100

normalizedTokenSimilarity :: Text -> Text -> Double
normalizedTokenSimilarity left right
  | Map.null leftTokens && Map.null rightTokens = 1
  | Map.null unionTokens = 0
  | otherwise =
      fromIntegral (Map.size intersectionTokens)
        / fromIntegral (Map.size unionTokens)
  where
    leftTokens = tokenMap left
    rightTokens = tokenMap right
    intersectionTokens = Map.intersection leftTokens rightTokens
    unionTokens = Map.union leftTokens rightTokens

tokenMap :: Text -> Map.Map Text ()
tokenMap =
  Map.fromList
    . map (\token -> (token, ()))
    . filter (not . T.null)
    . T.words
    . normalizeTokenText

normalizeTokenText :: Text -> Text
normalizeTokenText =
  T.unwords
    . T.words
    . T.map (\ch -> if isAlphaNum ch then ch else ' ')
    . T.toCaseFold
    . T.strip

upsertDiscoveredVenue ::
  Text ->
  UTCTime ->
  DiscoveredVenue ->
  SqlPersistT IO (Social.VenueId, Bool)
upsertDiscoveredVenue provider now DiscoveredVenue{..} = do
  existingRef <-
    getBy
      (Social.UniqueExternalVenueRef provider discoveredVenueExternalId)
  let contact =
        encodeVenueContact
          discoveredVenuePhone
          discoveredVenueWebsite
          discoveredVenueState
          discoveredVenuePostalCode
          discoveredVenueImageUrl
  case existingRef of
    Just (Entity refKey ref) -> do
      let venueKey = Social.externalVenueRefVenueId ref
      update
        venueKey
        [ Social.VenueName =. discoveredVenueName
        , Social.VenueAddress =. discoveredVenueAddress
        , Social.VenueCity =. Just discoveredVenueCity
        , Social.VenueCountry =. discoveredVenueCountry
        , Social.VenueLatitude =. discoveredVenueLatitude
        , Social.VenueLongitude =. discoveredVenueLongitude
        , Social.VenueContact =. contact
        , Social.VenueUpdatedAt =. now
        ]
      update refKey [Social.ExternalVenueRefLastSeenAt =. now]
      pure (venueKey, False)
    Nothing -> do
      venueKey <-
        insert
          Social.Venue
            { Social.venueName = discoveredVenueName
            , Social.venueAddress = discoveredVenueAddress
            , Social.venueCity = Just discoveredVenueCity
            , Social.venueCountry = discoveredVenueCountry
            , Social.venueLatitude = discoveredVenueLatitude
            , Social.venueLongitude = discoveredVenueLongitude
            , Social.venueCapacity = Nothing
            , Social.venueContact = contact
            , Social.venueCreatedAt = now
            , Social.venueUpdatedAt = now
            }
      _ <-
        insert
            Social.ExternalVenueRef
              { Social.externalVenueRefProvider = provider
            , Social.externalVenueRefExternalId = discoveredVenueExternalId
            , Social.externalVenueRefVenueId = venueKey
            , Social.externalVenueRefLastSeenAt = now
            }
      pure (venueKey, True)

upsertDiscoveredArtist ::
  Text ->
  UTCTime ->
  DiscoveredArtist ->
  SqlPersistT IO (Social.ArtistProfileId, Bool)
upsertDiscoveredArtist provider now DiscoveredArtist{..} = do
  existingRef <-
    getBy
      (Social.UniqueExternalArtistRef provider discoveredArtistExternalId)
  (artistKey, created) <-
    case existingRef of
      Just (Entity refKey ref) -> do
        let existingArtistKey = Social.externalArtistRefArtistId ref
            imageUpdate =
              maybe [] (\imageUrl -> [Social.ArtistProfileAvatarUrl =. Just imageUrl])
                discoveredArtistImageUrl
        update
          existingArtistKey
          ( [ Social.ArtistProfileName =. discoveredArtistName
            , Social.ArtistProfileUpdatedAt =. now
            ]
              ++ imageUpdate
          )
        update refKey [Social.ExternalArtistRefLastSeenAt =. now]
        pure (existingArtistKey, False)
      Nothing -> do
        newArtistKey <-
          insert
            Social.ArtistProfile
              { Social.artistProfilePartyId = Nothing
              , Social.artistProfileName = discoveredArtistName
              , Social.artistProfileBio = Nothing
              , Social.artistProfileAvatarUrl = discoveredArtistImageUrl
              , Social.artistProfileGenres = Nothing
              , Social.artistProfileSocialLinks = Nothing
              , Social.artistProfileCreatedAt = now
              , Social.artistProfileUpdatedAt = now
              }
        _ <-
          insert
            Social.ExternalArtistRef
              { Social.externalArtistRefProvider = provider
              , Social.externalArtistRefExternalId = discoveredArtistExternalId
              , Social.externalArtistRefArtistId = newArtistKey
              , Social.externalArtistRefLastSeenAt = now
              }
        pure (newArtistKey, True)
  deleteWhere [Social.ArtistGenreArtistId ==. artistKey]
  forM_ discoveredArtistGenres $ \genre -> do
    _ <- insertUnique (Social.ArtistGenre artistKey genre)
    pure ()
  pure (artistKey, created)

encodeVenueContact :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
encodeVenueContact phone website state postalCode imageUrl
  | all (== Nothing) [phone, website, state, postalCode, imageUrl] = Nothing
  | otherwise =
      Just . TE.decodeUtf8 . BL.toStrict . encode $
        object
          [ "phone" .= phone
          , "website" .= website
          , "state" .= state
          , "zipCode" .= postalCode
          , "imageUrl" .= imageUrl
          ]

encodeEventMetadataForImport :: DiscoveredEvent -> Maybe Text
encodeEventMetadataForImport DiscoveredEvent{..} =
  Just . TE.decodeUtf8 . BL.toStrict . encode $
    object
      [ "ticketUrl" .= discoveredEventTicketUrl
      , "imageUrl" .= discoveredEventImageUrl
      , "isPublic" .=
          ( discoveredEventStatus
              `notElem` ["cancelled", "canceled", "completed", "missing"]
          )
      , "eventType" .= discoveredEventType
      , "eventStatus" .= discoveredEventStatus
      , "currency" .= discoveredEventCurrency
      , "budgetCents" .= (Nothing :: Maybe Int)
      ]
