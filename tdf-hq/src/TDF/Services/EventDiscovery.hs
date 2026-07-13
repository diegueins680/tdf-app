{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Services.EventDiscovery
  ( DiscoveredArtist(..)
  , DiscoveredEvent(..)
  , DiscoveredVenue(..)
  , DiscoverySyncStats(..)
  , EventDiscoveryRunHandle
  , beginEventDiscoveryRun
  , buildTicketmasterRequestUrl
  , fetchTicketmasterEvents
  , failEventDiscoveryRun
  , finishEventDiscoveryRun
  , loadActiveUserCities
  , normalizeTicketmasterResponse
  , normalizeUserCities
  , reconcileImportedEvents
  , syncDiscoveredEvent
  ) where

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
import Data.Char (GeneralCategory(Format, LineSeparator, ParagraphSeparator), generalCategory, isControl)
import Data.Function (on)
import Data.List (maximumBy, nubBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, UTCTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Database.Persist
  ( Entity(..)
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
  { discoveredEventExternalId :: Text
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

emptyDiscoverySyncStats :: DiscoverySyncStats
emptyDiscoverySyncStats = DiscoverySyncStats 0 0 0 0 0

newtype EventDiscoveryRunHandle =
  EventDiscoveryRunHandle Social.ExternalEventDiscoveryRunId

beginEventDiscoveryRun ::
  ConnectionPool ->
  Day ->
  UTCTime ->
  IO (Maybe EventDiscoveryRunHandle)
beginEventDiscoveryRun pool runDate now =
  runSqlPool claim pool
  where
    claim = do
      existing <-
        getBy
          (Social.UniqueExternalEventDiscoveryRun providerName runDate)
      case existing of
        Just (Entity runKey runRow)
          | shouldReclaim runRow -> do
              update
                runKey
                [ Social.ExternalEventDiscoveryRunStatus =. "running"
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
                { Social.externalEventDiscoveryRunProvider = providerName
                , Social.externalEventDiscoveryRunRunDate = runDate
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
loadActiveUserCities pool = do
  rows <-
    runSqlPool
      ( rawSql
          "SELECT city FROM (\
          \ SELECT trim(fp.city) AS city\
          \ FROM fan_profile fp\
          \ INNER JOIN user_credential uc ON uc.party_id = fp.fan_party_id\
          \ WHERE uc.active = TRUE AND fp.city IS NOT NULL AND trim(fp.city) <> ''\
          \ UNION\
          \ SELECT trim(ap.city) AS city\
          \ FROM artist_profile ap\
          \ INNER JOIN user_credential uc ON uc.party_id = ap.artist_party_id\
          \ WHERE uc.active = TRUE AND ap.city IS NOT NULL AND trim(ap.city) <> ''\
          \ ) user_cities"
          [] :: SqlPersistT IO [Single Text]
      )
      pool
  pure (normalizeUserCities [city | Single city <- rows])

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
  fetchPage 0 []
  where
    endTime = addUTCTime (fromIntegral (eventDiscoveryLookaheadDays cfg * 86400)) now
    fetchPage pageNumber collected
      | pageNumber >= eventDiscoveryMaxPagesPerCity cfg = pure (Right collected)
      | otherwise = do
          let url =
                buildTicketmasterRequestUrl
                  (ticketmasterApiBase cfg)
                  (eventDiscoveryCountryCode cfg)
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
                                  normalized = normalizeTicketmasterResponse city now decoded
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

normalizeTicketmasterResponse :: Text -> UTCTime -> TicketmasterResponse -> [DiscoveredEvent]
normalizeTicketmasterResponse requestedCity now =
  mapMaybe (normalizeTicketmasterEvent requestedCity now) . ticketmasterResponseEvents

normalizeTicketmasterEvent :: Text -> UTCTime -> TicketmasterEvent -> Maybe DiscoveredEvent
normalizeTicketmasterEvent requestedCity now TicketmasterEvent{..} = do
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
      (priceCents, currency) = normalizePrice ticketmasterEventPrices
      description =
        joinDescription
          [ ticketmasterEventInfo >>= cleanMultiline 5000
          , ticketmasterEventNote >>= cleanMultiline 5000
          ]
  pure
    DiscoveredEvent
      { discoveredEventExternalId = externalId
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

normalizePrice :: [TicketmasterPriceRange] -> (Maybe Int, Text)
normalizePrice ranges =
  case
    sortOn fst
      [ (cents, currency)
      | TicketmasterPriceRange (Just rawCurrency) (Just minimumPrice) <- ranges
      , let currency = T.toUpper (T.strip rawCurrency)
      , T.length currency == 3
      , T.all (\ch -> ch >= 'A' && ch <= 'Z') currency
      , not (isNaN minimumPrice || isInfinite minimumPrice)
      , minimumPrice >= 0
      , minimumPrice <= fromIntegral (maxBound :: Int) / 100
      , let centsInteger = round (minimumPrice * 100) :: Integer
      , centsInteger <= fromIntegral (maxBound :: Int)
      , let cents = fromIntegral centsInteger
      ] of
      firstPrice : _ -> (Just (fst firstPrice), snd firstPrice)
      [] -> (Nothing, "USD")

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

-- | Keep provider-owned lifecycle state aligned with the current user-city
-- scope. Past imports complete automatically; future imports leave the public
-- feed as soon as their city no longer has an active user.
reconcileImportedEvents :: ConnectionPool -> UTCTime -> [Text] -> IO Int
reconcileImportedEvents pool now activeCities =
  runSqlPool reconcile pool
  where
    activeCityKeys = Map.fromList [(normalizeCityKey city, ()) | city <- activeCities]
    reconcile = do
      refs <- selectList [Social.ExternalEventRefProvider ==. providerName] []
      changes <- forM refs $ \(Entity _ ref) -> do
        maybeEvent <- get (Social.externalEventRefEventId ref)
        case maybeEvent of
          Nothing -> pure 0
          Just eventRow
            | Social.socialEventEndTime eventRow < now -> do
                updateImportedLifecycle eventRow ref "completed" True
                pure 1
            | Map.notMember
                (normalizeCityKey (Social.externalEventRefCity ref))
                activeCityKeys -> do
                updateImportedLifecycle eventRow ref "cancelled" False
                pure 1
            | otherwise -> pure 0
      pure (sum changes)

    updateImportedLifecycle eventRow ref status isPublic =
      update
        (Social.externalEventRefEventId ref)
        [ Social.SocialEventMetadata =.
            updateImportedEventMetadata status isPublic (Social.socialEventMetadata eventRow)
        , Social.SocialEventUpdatedAt =. now
        ]

updateImportedEventMetadata :: Text -> Bool -> Maybe Text -> Maybe Text
updateImportedEventMetadata status isPublic rawMetadata =
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
        then fromMaybe Null (AesonKeyMap.lookup "ticketUrl" originalObject)
        else Null

syncDiscoveredEventDb :: UTCTime -> DiscoveredEvent -> SqlPersistT IO DiscoverySyncStats
syncDiscoveredEventDb now DiscoveredEvent{..} = do
  (venueKey, venueCreated) <- upsertDiscoveredVenue now discoveredEventVenue
  artistResults <- forM discoveredEventArtists (upsertDiscoveredArtist now)
  let artistKeys = map fst artistResults
      artistsCreated = length (filter snd artistResults)
      metadata = encodeEventMetadataForImport DiscoveredEvent{..}
  existingRef <-
    getBy
      (Social.UniqueExternalEventRef providerName discoveredEventExternalId)
  (eventKey, eventCreated) <-
    case existingRef of
      Just (Entity refKey ref) -> do
        let existingEventKey = Social.externalEventRefEventId ref
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
        update
          refKey
          [ Social.ExternalEventRefCity =. discoveredVenueCity discoveredEventVenue
          , Social.ExternalEventRefSourceUrl =. discoveredEventTicketUrl
          , Social.ExternalEventRefLastSeenAt =. now
          ]
        pure (existingEventKey, False)
      Nothing -> do
        newEventKey <-
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
        _ <-
          insert
            Social.ExternalEventRef
              { Social.externalEventRefProvider = providerName
              , Social.externalEventRefExternalId = discoveredEventExternalId
              , Social.externalEventRefEventId = newEventKey
              , Social.externalEventRefCity = discoveredVenueCity discoveredEventVenue
              , Social.externalEventRefSourceUrl = discoveredEventTicketUrl
              , Social.externalEventRefLastSeenAt = now
              }
        pure (newEventKey, True)
  deleteWhere [Social.EventArtistEventId ==. eventKey]
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

upsertDiscoveredVenue ::
  UTCTime ->
  DiscoveredVenue ->
  SqlPersistT IO (Social.VenueId, Bool)
upsertDiscoveredVenue now DiscoveredVenue{..} = do
  existingRef <-
    getBy
      (Social.UniqueExternalVenueRef providerName discoveredVenueExternalId)
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
            { Social.externalVenueRefProvider = providerName
            , Social.externalVenueRefExternalId = discoveredVenueExternalId
            , Social.externalVenueRefVenueId = venueKey
            , Social.externalVenueRefLastSeenAt = now
            }
      pure (venueKey, True)

upsertDiscoveredArtist ::
  UTCTime ->
  DiscoveredArtist ->
  SqlPersistT IO (Social.ArtistProfileId, Bool)
upsertDiscoveredArtist now DiscoveredArtist{..} = do
  existingRef <-
    getBy
      (Social.UniqueExternalArtistRef providerName discoveredArtistExternalId)
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
              { Social.externalArtistRefProvider = providerName
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
      , "isPublic" .= True
      , "eventType" .= discoveredEventType
      , "eventStatus" .= discoveredEventStatus
      , "currency" .= discoveredEventCurrency
      , "budgetCents" .= (Nothing :: Maybe Int)
      ]
