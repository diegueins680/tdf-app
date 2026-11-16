{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | schema.org linked-data sources for event discovery: an index document
-- (XML sitemap or HTML listing) whose event pages publish JSON-LD.
module TDF.Services.EventDiscoveryLinkedData
  ( fetchLinkedDataEvents
  , fetchLinkedDataEventsWith
  , LinkedDataSourceConfig(..)
  , parseLinkedDataSourceConfig
  , linkedDataEventLinks
  , normalizeLinkedDataDocument
  , parseLinkedDataTime
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Aeson (FromJSON(..), Value(..), decodeStrict', withObject, (.:?), (.!=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import Data.List (nub, nubBy, sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime(..), LocalTime(..), addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Client
  ( HttpException
  , Request
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
import Text.Read (readMaybe)

import TDF.Config (AppConfig(..))
import TDF.Internationalization (currencyDecimalPlaces, currencyDefinition, normalizeCurrencyCode)
import TDF.Services.EventDiscovery

-- A linked-data source reads an index document (an XML sitemap or an HTML
-- listing), follows the event links under an explicit prefix, and imports the
-- schema.org Event objects those pages publish as JSON-LD. It reads no page
-- markup other than link targets and the embedded JSON-LD blocks.
data LinkedDataSourceConfig = LinkedDataSourceConfig
  { linkedDataLinkPrefix :: Maybe Text
  , linkedDataRewriteFrom :: Maybe Text
  , linkedDataRewriteTo :: Maybe Text
  , linkedDataMaxPages :: Int
  , linkedDataVenueName :: Maybe Text
  , linkedDataDateOnlyLocalTime :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON LinkedDataSourceConfig where
  parseJSON = withObject "LinkedDataSourceConfig" $ \o ->
    LinkedDataSourceConfig
      <$> o .:? "linkPrefix"
      <*> o .:? "rewriteFrom"
      <*> o .:? "rewriteTo"
      <*> (o .:? "maxPages" .!= defaultLinkedDataMaxPages)
      <*> o .:? "venueName"
      <*> o .:? "dateOnlyLocalTime"

defaultLinkedDataMaxPages :: Int
defaultLinkedDataMaxPages = 60

maximumLinkedDataMaxPages :: Int
maximumLinkedDataMaxPages = 400

parseLinkedDataSourceConfig :: Maybe Text -> Either Text LinkedDataSourceConfig
parseLinkedDataSourceConfig rawConfiguration =
  case T.strip <$> rawConfiguration of
    Nothing -> Right defaults
    Just "" -> Right defaults
    Just raw ->
      case decodeStrict' (TE.encodeUtf8 raw) of
        Nothing -> Left "Linked-data source configuration is not valid JSON"
        Just config
          | linkedDataMaxPages config < 1
              || linkedDataMaxPages config > maximumLinkedDataMaxPages ->
              Left "Linked-data source maxPages must be between 1 and 400"
          | maybe False (not . T.isPrefixOf "https://") (linkedDataLinkPrefix config) ->
              Left "Linked-data source linkPrefix must be an HTTPS URL prefix"
          | isJust (linkedDataRewriteFrom config) /= isJust (linkedDataRewriteTo config) ->
              Left "Linked-data source rewriteFrom and rewriteTo must be set together"
          | otherwise -> Right config
  where
    defaults =
      LinkedDataSourceConfig Nothing Nothing Nothing defaultLinkedDataMaxPages Nothing Nothing

extractBetweenAll :: Text -> Text -> Text -> [Text]
extractBetweenAll opening closing = go
  where
    go document =
      case T.breakOn opening document of
        (_, rest)
          | T.null rest -> []
          | otherwise ->
              let afterOpening = T.drop (T.length opening) rest
                  (value, afterValue) = T.breakOn closing afterOpening
               in if T.null afterValue
                    then []
                    else value : go (T.drop (T.length closing) afterValue)

isSitemapDocument :: Text -> Bool
isSitemapDocument document =
  let prefix = T.take 2000 document
   in "<urlset" `T.isInfixOf` prefix || "<sitemapindex" `T.isInfixOf` prefix

decodeMarkupEntities :: Text -> Text
decodeMarkupEntities =
  T.replace "&amp;" "&"
    . T.replace "&quot;" "\""
    . T.replace "&#39;" "'"
    . T.replace "&#039;" "'"
    . T.replace "&apos;" "'"

-- Resolve a link target against the document it appeared in. Only absolute
-- and root-relative targets are followed; fragments are dropped.
resolveDocumentLink :: Text -> Text -> Maybe Text
resolveDocumentLink baseUrl rawTarget
  | T.null target = Nothing
  | "https://" `T.isPrefixOf` target = Just target
  | "http://" `T.isPrefixOf` target = Just ("https://" <> T.drop 7 target)
  | "//" `T.isPrefixOf` target = Just ("https:" <> target)
  | "/" `T.isPrefixOf` target = Just (origin <> target)
  | otherwise = Nothing
  where
    target = T.takeWhile (/= '#') (decodeMarkupEntities (T.strip rawTarget))
    origin = "https://" <> T.takeWhile (`notElem` ("/?#" :: String)) (T.drop 8 baseUrl)

-- Event page links in an index document, in document order without duplicates.
linkedDataEventLinks :: LinkedDataSourceConfig -> Text -> Text -> [Text]
linkedDataEventLinks config indexUrl document =
  case linkedDataLinkPrefix config of
    Nothing -> []
    Just prefix ->
      nub
        [ rewrite link
        | rawTarget <- rawTargets
        , Just link <- [resolveDocumentLink indexUrl rawTarget]
        , prefix `T.isPrefixOf` link
        , T.length link > T.length prefix
        , T.length link <= 2048
        ]
  where
    rawTargets
      | isSitemapDocument document = extractBetweenAll "<loc>" "</loc>" document
      | otherwise =
          extractBetweenAll "href=\"" "\"" document
            ++ extractBetweenAll "href='" "'" document
    rewrite link =
      case (linkedDataRewriteFrom config, linkedDataRewriteTo config) of
        (Just from, Just to) | not (T.null from) -> T.replace from to link
        _ -> link

-- Every JSON-LD block in a document, decoded; malformed blocks are ignored.
linkedDataBlocks :: Text -> [Value]
linkedDataBlocks document =
  mapMaybe decodeBlock (drop 1 (T.splitOn "application/ld+json" document))
  where
    decodeBlock afterType =
      let afterTag = T.drop 1 (T.dropWhile (/= '>') afterType)
          (rawJson, closing) = T.breakOn "</script" afterTag
       in if T.null closing
            then Nothing
            else decodeStrict' (TE.encodeUtf8 (T.strip rawJson))

linkedDataEventObjects :: Value -> [AesonKeyMap.KeyMap Value]
linkedDataEventObjects value =
  case value of
    Array items -> concatMap linkedDataEventObjects items
    Object o ->
      [o | any ("Event" `T.isSuffixOf`) (linkedDataTypes o)]
        ++ maybe [] linkedDataEventObjects (AesonKeyMap.lookup "@graph" o)
    _ -> []

linkedDataTypes :: AesonKeyMap.KeyMap Value -> [Text]
linkedDataTypes o =
  case AesonKeyMap.lookup "@type" o of
    Just (String value) -> [value]
    Just (Array values) -> [value | String value <- foldr (:) [] values]
    _ -> []

linkedText :: Key -> AesonKeyMap.KeyMap Value -> Maybe Text
linkedText key o =
  case AesonKeyMap.lookup key o of
    Just (String value) | not (T.null (T.strip value)) -> Just (decodeMarkupEntities value)
    _ -> Nothing

linkedObjects :: Key -> AesonKeyMap.KeyMap Value -> [AesonKeyMap.KeyMap Value]
linkedObjects key o =
  case AesonKeyMap.lookup key o of
    Just (Object value) -> [value]
    Just (Array values) -> [value | Object value <- foldr (:) [] values]
    _ -> []

linkedNumber :: Key -> AesonKeyMap.KeyMap Value -> Maybe Double
linkedNumber key o =
  case AesonKeyMap.lookup key o of
    Just (Number value) -> Just (realToFrac value)
    Just (String value) -> readMaybe (T.unpack (T.strip value))
    _ -> Nothing

linkedImageUrl :: AesonKeyMap.KeyMap Value -> Maybe Text
linkedImageUrl o =
  case AesonKeyMap.lookup "image" o of
    Just (String value) -> Just value
    Just (Object image) -> linkedText "url" image
    Just (Array values) ->
      listToMaybe
        ( mapMaybe
            ( \item ->
                case item of
                  String value -> Just value
                  Object image -> linkedText "url" image
                  _ -> Nothing
            )
            (foldr (:) [] values)
        )
    _ -> Nothing

-- schema.org dates carry an explicit offset ("-05:00", "-0500" or "Z") or are
-- local to the venue. Date-only values are rejected: they have no start time.
parseLinkedDataTime :: Maybe Text -> Text -> Maybe UTCTime
parseLinkedDataTime zone rawValue = do
  let (datePart, rest) = T.breakOn "T" (T.strip rawValue)
  timeAndOffset <- T.stripPrefix "T" rest
  let (timePart, offsetPart) = T.break (`elem` ("Z+-" :: String)) timeAndOffset
  day <- parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack datePart)
  timeOfDay <-
    listToMaybe
      ( mapMaybe
          (\format -> parseTimeM True defaultTimeLocale format (T.unpack timePart))
          ["%H:%M:%S%Q", "%H:%M"]
      )
  offsetMinutes <-
    case offsetPart of
      "" -> zone >>= fixedUtcOffsetMinutes
      "Z" -> Just 0
      _ -> parseOffsetMinutes offsetPart
  pure (localTimeInZone offsetMinutes (LocalTime day timeOfDay))
  where
    parseOffsetMinutes rawOffset = do
      (signChar, body) <- T.uncons rawOffset
      let digits = T.filter (/= ':') body
          sign = if signChar == '-' then negate else id
      if T.length digits `elem` [2, 4] && T.all (`elem` ("0123456789" :: String)) digits
        then do
          hours <- readMaybe (T.unpack (T.take 2 digits)) :: Maybe Int
          minutes <-
            if T.length digits == 4
              then readMaybe (T.unpack (T.drop 2 digits))
              else Just 0
          if hours <= 14 && minutes < 60 then Just (sign (hours * 60 + minutes)) else Nothing
        else Nothing

data LinkedPlace = LinkedPlace
  { linkedPlaceName :: Maybe Text
  , linkedPlaceStreet :: Maybe Text
  , linkedPlaceLocality :: Maybe Text
  , linkedPlaceCountry :: Maybe Text
  }

linkedPlace :: AesonKeyMap.KeyMap Value -> LinkedPlace
linkedPlace event =
  case linkedObjects "location" event of
    place : _
      | "City" `elem` linkedTypes ->
          LinkedPlace Nothing Nothing (linkedText "name" place) Nothing
      | otherwise ->
          let addressObject = listToMaybe (linkedObjects "address" place)
              country =
                (addressObject >>= linkedText "addressCountry")
                  <|> ( addressObject
                          >>= listToMaybe . linkedObjects "addressCountry"
                          >>= linkedText "name"
                      )
           in LinkedPlace
                { linkedPlaceName = linkedText "name" place
                , linkedPlaceStreet =
                    (addressObject >>= linkedText "streetAddress")
                      <|> linkedText "address" place
                , linkedPlaceLocality = addressObject >>= linkedText "addressLocality"
                , linkedPlaceCountry = country
                }
      where
        linkedTypes = linkedDataTypes place
    [] -> LinkedPlace Nothing Nothing Nothing Nothing

-- A source bound to one city trusts that binding (a venue's suburb is still
-- that venue). A multi-city platform must name a discovery city explicitly.
matchLinkedDataCity ::
  Maybe EventDiscoveryCity ->
  [EventDiscoveryCity] ->
  LinkedPlace ->
  Maybe EventDiscoveryCity
matchLinkedDataCity (Just fixedCity) _ _ = Just fixedCity
matchLinkedDataCity Nothing cities place =
  listToMaybe
    ( sortOn
        (negate . T.length . eventDiscoveryCityName)
        [ city
        | city <- cities
        , countryAllows city
        , let wanted = " " <> foldCityText (eventDiscoveryCityName city) <> " "
        , wanted `T.isInfixOf` (" " <> searchable <> " ")
        ]
    )
  where
    searchable =
      foldCityText
        (fromMaybe (fromMaybe "" (linkedPlaceStreet place)) (linkedPlaceLocality place))
    countryAllows city =
      case foldCityText <$> linkedPlaceCountry place of
        Nothing -> True
        Just country
          | T.null country -> True
          | otherwise ->
              linkedCountryCode country
                == Just (T.toCaseFold (eventDiscoveryCityCountryCode city))

-- The country a source states, as a case-folded two-letter code. A country
-- written in a form this deployment does not recognise is a different
-- country, never permission to match a registered city of the same name.
linkedCountryCode :: Text -> Maybe Text
linkedCountryCode country
  | T.length country == 2 = Just country
  | country == "ecuador" || country == "republica del ecuador" || country == "ecu" = Just "ec"
  | country == "colombia" || country == "republica de colombia" || country == "col" = Just "co"
  | otherwise = Nothing

lastUrlSegment :: Text -> Maybe Text
lastUrlSegment url =
  case filter (not . T.null) (T.splitOn "/" (T.takeWhile (`notElem` ("?#" :: String)) url)) of
    [] -> Nothing
    segments -> Just (last segments)

-- Normalize every schema.org Event published by one document.
normalizeLinkedDataDocument ::
  Int ->
  Text ->
  Text ->
  LinkedDataSourceConfig ->
  Maybe EventDiscoveryCity ->
  [EventDiscoveryCity] ->
  UTCTime ->
  Bool ->
  Text ->
  Text ->
  [DiscoveredEvent]
normalizeLinkedDataDocument lookaheadDays configuredCurrency sourceKey config fixedCity cities now isEventPage documentUrl document =
  mapMaybe normalizeOne eventObjects
  where
    eventObjects = concatMap linkedDataEventObjects (linkedDataBlocks document)
    singleEvent = isEventPage && length eventObjects == 1
    eventUrlOf event = linkedText "url" event >>= normalizeHttpsUrl "Linked-data event URL"
    eventsSharingUrl url = length (filter ((== Just url) . eventUrlOf) eventObjects)
    lookaheadEnd = addUTCTime (fromIntegral (lookaheadDays * 86400)) now

    normalizeOne event = do
      title <- linkedText "name" event >>= cleanSingleLine 160
      let place = linkedPlace event
      city <- matchLinkedDataCity fixedCity cities place
      rawStart <- linkedText "startDate" event
      startsAt <- parseLinkedDataTime (eventDiscoveryCityTimeZone city) rawStart
      whenMaybe (startsAt < now || startsAt > lookaheadEnd) Nothing
      venueName <-
        (linkedPlaceName place >>= cleanSingleLine 300)
          <|> (linkedDataVenueName config >>= cleanSingleLine 300)
      let eventUrl = eventUrlOf event
          identityUrl = if isEventPage then documentUrl else fromMaybe documentUrl eventUrl
          startStamp = T.pack (formatTime defaultTimeLocale "%Y%m%dT%H%M" startsAt)
          -- An event with a page of its own keeps one identity when it is
          -- rescheduled. Only performances that share a page, or have none,
          -- need their start time to be told apart.
          ownPage =
            not isEventPage
              && maybe False (\url -> url /= documentUrl && eventsSharingUrl url == 1) eventUrl
      slug <- lastUrlSegment identityUrl
      externalId <-
        cleanIdentifier (if singleEvent || ownPage then slug else slug <> "@" <> startStamp)
      let endsAt =
            case linkedText "endDate" event >>= parseLinkedDataTime (eventDiscoveryCityTimeZone city) of
              Just candidate | candidate > startsAt -> Just candidate
              _ -> Nothing
          offers = linkedObjects "offers" event
          saleOpen = any (maybe False ("InStock" `T.isInfixOf`) . linkedText "availability") offers
          sourceStatus =
            case linkedText "eventStatus" event of
              Just status | "Cancelled" `T.isInfixOf` status -> Just "cancelled"
              _ -> Nothing
          offerCurrency =
            listToMaybe (mapMaybe (\offer -> linkedText "priceCurrency" offer >>= normalizeCurrencyCode) offers)
          defaultCurrencyCode
            | eventDiscoveryCityCountryCode city == "CO" = "COP"
            | otherwise = configuredCurrency
          currency = fromMaybe defaultCurrencyCode offerCurrency
          prices =
            [ price
            | offer <- offers
            , Just price <- [linkedNumber "price" offer <|> linkedNumber "lowPrice" offer]
            , not (isNaN price || isInfinite price)
            , price >= 0
            ]
          priceCents =
            case prices of
              [] -> Nothing
              _ ->
                let decimalPlaces = maybe 2 currencyDecimalPlaces (currencyDefinition currency)
                    minorUnits = round (minimum prices * (10 ^ decimalPlaces)) :: Integer
                 in if minorUnits <= fromIntegral (maxBound :: Int)
                      then Just (fromIntegral minorUnits)
                      else Nothing
          ticketUrl =
            listToMaybe
              (mapMaybe (\offer -> linkedText "url" offer >>= normalizeHttpsUrl "Linked-data offer URL") offers)
              <|> normalizeHttpsUrl "Linked-data event URL" identityUrl
          dateOnly =
            case linkedDataDateOnlyLocalTime config of
              Just marker ->
                not (T.null marker)
                  && marker `T.isPrefixOf` T.drop 1 (snd (T.breakOn "T" (T.strip rawStart)))
              Nothing -> False
          description =
            joinDescription
              [ if dateOnly
                  then Just "Hora por confirmar: la fuente publica solo la fecha."
                  else Nothing
              , linkedText "description" event >>= cleanMultiline 5000
              ]
          artists =
            mapMaybe
              (\performer -> linkedText "name" performer >>= normalizeStructuredArtist sourceKey)
              (linkedObjects "performer" event)
      pure
        DiscoveredEvent
          { discoveredEventProvider = sourceKey
          , discoveredEventExternalId = externalId
          , discoveredEventTitle = title
          , discoveredEventDescription = description
          , discoveredEventStart = startsAt
          , discoveredEventEnd = endsAt
          , discoveredEventVenue =
              DiscoveredVenue
                { -- The same venue name can exist in two cities of one
                  -- multi-city source, so the place is part of the identity.
                  discoveredVenueExternalId =
                    T.intercalate
                      ":"
                      [ sourceKey
                      , T.toCaseFold (eventDiscoveryCityCountryCode city)
                      , normalizeTokenText (eventDiscoveryCityName city)
                      , normalizeTokenText venueName
                      ]
                , discoveredVenueName = venueName
                , discoveredVenueAddress = linkedPlaceStreet place >>= cleanSingleLine 500
                , discoveredVenueCity = eventDiscoveryCityName city
                , discoveredVenueCountry = Nothing
                , discoveredVenueCountryCode = Just (eventDiscoveryCityCountryCode city)
                , discoveredVenueTimeZone = eventDiscoveryCityTimeZone city
                , discoveredVenueLatitude = Nothing
                , discoveredVenueLongitude = Nothing
                , discoveredVenuePhone = Nothing
                , discoveredVenueWebsite = Nothing
                , discoveredVenueState = Nothing
                , discoveredVenuePostalCode = Nothing
                , discoveredVenueImageUrl = Nothing
                }
          , discoveredEventArtists = nubBy ((==) `on` discoveredArtistExternalId) artists
          , discoveredEventPriceCents = priceCents
          , discoveredEventCurrency = currency
          , discoveredEventTicketUrl = ticketUrl
          , discoveredEventImageUrl =
              linkedImageUrl event >>= normalizeHttpsUrl "Linked-data event image"
          , discoveredEventType = classifyDiscoveredEventType title (linkedDataTypes event)
          , discoveredEventStatus =
              normalizeEventStatus now startsAt endsAt sourceStatus saleOpen
          }

-- Fetch one public HTTPS document. Redirects are followed manually so every
-- hop passes the same public-host validation as the configured URL.
fetchPublicDocument :: Int -> Text -> IO (Either Text (Int, Text, BL.ByteString))
fetchPublicDocument remainingRedirects rawUrl =
  case validatePublicFeedUrl rawUrl of
    Left err -> pure (Left err)
    Right validatedUrl -> do
      requestResult <- try (parseRequest (T.unpack validatedUrl)) :: IO (Either HttpException Request)
      case requestResult of
        Left _ -> pure (Left "Could not build the linked-data request")
        Right rawRequest -> do
          let request =
                rawRequest
                  { requestHeaders =
                      [ ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.5")
                      , ("User-Agent", "TDF-Records-Event-Discovery/1.0")
                      ]
                  , redirectCount = 0
                  , responseTimeout = responseTimeoutMicro (20 * 1000000)
                  }
          responseResult <- requestTicketmasterPage request 0
          case responseResult of
            Left _ -> pure (Left "Linked-data request failed")
            Right response ->
              let httpStatus = statusCode (responseStatus response)
                  body = responseBody response
                  location =
                    lookup "Location" (responseHeaders response)
                      >>= either (const Nothing) Just . TE.decodeUtf8'
                      >>= resolveDocumentLink validatedUrl
               in if httpStatus `elem` [301, 302, 303, 307, 308]
                    then case location of
                      Just nextUrl | remainingRedirects > 0 ->
                        fetchPublicDocument (remainingRedirects - 1) nextUrl
                      _ -> pure (Left "Linked-data redirect could not be followed safely")
                    else
                      if BL.length body > 10 * 1024 * 1024
                        then pure (Left "Linked-data document exceeded the 10 MB safety limit")
                        else pure (Right (httpStatus, validatedUrl, body))

fetchLinkedDataEvents ::
  AppConfig ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe EventDiscoveryCity ->
  [EventDiscoveryCity] ->
  UTCTime ->
  IO (Either Text [DiscoveredEvent])
fetchLinkedDataEvents cfg =
  fetchLinkedDataEventsWith (eventDiscoveryLookaheadDays cfg) (defaultCurrency cfg)

-- The fetch itself needs only the lookahead window and the fallback currency.
fetchLinkedDataEventsWith ::
  Int ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe EventDiscoveryCity ->
  [EventDiscoveryCity] ->
  UTCTime ->
  IO (Either Text [DiscoveredEvent])
fetchLinkedDataEventsWith lookaheadDays configuredCurrency sourceKey indexUrl rawConfiguration fixedCity cities now =
  case parseLinkedDataSourceConfig rawConfiguration of
    Left err -> pure (Left err)
    Right config -> do
      indexResult <- fetchPublicDocument 3 indexUrl
      case indexResult of
        Left err -> pure (Left err)
        Right (httpStatus, resolvedIndexUrl, body)
          | httpStatus < 200 || httpStatus >= 300 ->
              pure (Left ("Linked-data index returned HTTP " <> T.pack (show httpStatus)))
          | otherwise -> do
              let document = decodeDocument body
                  links = linkedDataEventLinks config resolvedIndexUrl document
                  indexEvents = normalize config False resolvedIndexUrl document
              if length links > linkedDataMaxPages config
                then
                  pure
                    ( Left
                        ( "Linked-data index lists "
                            <> T.pack (show (length links))
                            <> " event pages, above the source budget; inventory incomplete"
                        )
                    )
                else do
                  pagesResult <- fetchPages config links []
                  pure $
                    fmap
                      (nubBy ((==) `on` discoveredEventExternalId) . (indexEvents ++))
                      pagesResult
  where
    decodeDocument = TE.decodeUtf8With lenientDecode . BL.toStrict
    normalize config =
      normalizeLinkedDataDocument
        lookaheadDays
        configuredCurrency
        sourceKey
        config
        fixedCity
        cities
        now
    fetchPages _ [] collected = pure (Right (concat (reverse collected)))
    fetchPages config (link : remaining) collected = do
      -- One request per second per source keeps daily polling polite.
      threadDelay 750000
      firstAttempt <- fetchPublicDocument 3 link
      pageResult <-
        case firstAttempt of
          Right (httpStatus, _, _) | httpStatus < 500 -> pure firstAttempt
          _ -> threadDelay 2000000 >> fetchPublicDocument 3 link
      case pageResult of
        Left err -> pure (Left err)
        Right (httpStatus, _, body)
          | httpStatus `elem` [404, 410] -> fetchPages config remaining collected
          | httpStatus < 200 || httpStatus >= 300 ->
              pure (Left ("Linked-data event page returned HTTP " <> T.pack (show httpStatus)))
          | otherwise ->
              fetchPages
                config
                remaining
                (normalize config True link (decodeDocument body) : collected)
