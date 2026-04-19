{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.ServerRadio
  ( radioServer
  , validateRadioStreamUrl
  , validateRadioTransmissionPublicBase
  , validateRadioTransmissionIngestBase
  , validateRadioTransmissionWhipBase
  , validateRadioImportSources
  , validateRadioImportLimit
  , validateRadioMetadataRefreshLimit
  ) where

import           Control.Applicative    ((<|>))
import           Control.Exception      (SomeException, try, displayException)
import           Control.Monad          (forM, when)
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Char              (isAlphaNum, isDigit, isHexDigit, isSpace)
import           Data.Int               (Int64)
import           Data.List              (foldl', find, findIndex)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes, fromMaybe, isNothing)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS8
import qualified Data.CaseInsensitive   as CI
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Time              (UTCTime, getCurrentTime)
import           System.Environment     (lookupEnv)
import           Text.Read              (readMaybe)
import qualified Data.ByteString.Lazy   as BL
import           Database.Persist       (Entity(..), (=.), (==.), SelectOpt(Desc, LimitTo), deleteBy, getBy, insert,
                                         selectList, selectFirst, update)
import           Database.Persist.Sql   (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant                (NoContent(..), ServerError, ServerT, err400, errBody, throwError, (:<|>)(..))
import           Network.HTTP.Client    (BodyReader, Manager, brRead, httpLbs, newManager, parseRequest, responseBody,
                                         responseHeaders, responseTimeoutMicro, requestHeaders, withResponse, Request(..))
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Data.UUID              (toText)
import           Data.UUID.V4           (nextRandom)

import           TDF.API.Radio          (RadioAPI)
import           TDF.API.Types          (RadioStreamDTO(..), RadioStreamUpsert(..), RadioPresenceDTO(..),
                                         RadioPresenceUpsert(..), RadioImportRequest(..), RadioImportResult(..),
                                         RadioMetadataRefreshRequest(..), RadioMetadataRefreshResult(..),
                                         RadioNowPlayingRequest(..), RadioNowPlayingResult(..),
                                         RadioTransmissionRequest(..), RadioTransmissionInfo(..))
import           TDF.Auth               (AuthedUser(..))
import           TDF.DB                 (Env(..))
import           TDF.Models

data StreamMetadata = StreamMetadata
  { smName  :: Maybe Text
  , smGenre :: Maybe Text
  }

lookupHeader :: BS.ByteString -> [(CI.CI BS.ByteString, BS.ByteString)] -> Maybe BS.ByteString
lookupHeader key hdrs =
  let target = CI.mk key
  in fmap snd (find (\(k, _) -> k == target) hdrs)

validateRadioStreamUrl :: Text -> Either ServerError Text
validateRadioStreamUrl rawUrl
  | T.null streamUrl =
      Left err400 { errBody = "streamUrl is required" }
  | T.any isSpace streamUrl =
      Left err400 { errBody = "streamUrl must not contain whitespace" }
  | Nothing <- mRemainder =
      Left err400 { errBody = "streamUrl must be http(s)" }
  | T.null authority =
      Left err400 { errBody = "streamUrl must include a host" }
  | Left authorityErr <- validateRadioAuthority authority =
      Left authorityErr
  | otherwise =
      Right streamUrl
  where
    streamUrl = T.strip rawUrl
    lowerUrl = T.toLower streamUrl
    mRemainder
      | "http://" `T.isPrefixOf` lowerUrl = Just (T.drop 7 streamUrl)
      | "https://" `T.isPrefixOf` lowerUrl = Just (T.drop 8 streamUrl)
      | otherwise = Nothing
    authority =
      maybe "" (T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#')) mRemainder

validateRadioTransmissionPublicBase :: Text -> Either ServerError Text
validateRadioTransmissionPublicBase rawBase =
  case validateRadioStreamUrl rawBase of
    Left err -> Left (radioFieldError "RADIO_PUBLIC_BASE" err)
    Right publicBase ->
      let normalizedPublicBase = normalizePublicBaseScheme publicBase
      in if T.any (`elem` ("?#" :: String)) normalizedPublicBase
            then Left err400 { errBody = "RADIO_PUBLIC_BASE must not include query or fragment" }
            else Right (T.dropWhileEnd (== '/') normalizedPublicBase)

validateRadioTransmissionIngestBase :: Text -> Either ServerError Text
validateRadioTransmissionIngestBase =
  validateRadioTransmissionEndpointBase "RADIO_INGEST_BASE" "rtmp(s)" ["rtmp", "rtmps"]

validateRadioTransmissionWhipBase :: Text -> Either ServerError Text
validateRadioTransmissionWhipBase =
  validateRadioTransmissionEndpointBase "RADIO_WHIP_BASE" "http(s)" ["http", "https"]

validateRadioTransmissionEndpointBase :: Text -> Text -> [Text] -> Text -> Either ServerError Text
validateRadioTransmissionEndpointBase label allowedSchemesText allowedSchemes rawBase
  | T.null endpointBase =
      Left err400 { errBody = fieldBody " is required" }
  | T.any isSpace endpointBase =
      Left err400 { errBody = fieldBody " must not contain whitespace" }
  | Nothing <- mRemainder =
      Left err400 { errBody = fieldBody (" must be " <> allowedSchemesText) }
  | T.null authority =
      Left err400 { errBody = fieldBody " must include a host" }
  | Left authorityErr <- validateRadioAuthority authority =
      Left (radioFieldError label authorityErr)
  | T.any (`elem` ("?#" :: String)) normalizedEndpointBase =
      Left err400 { errBody = fieldBody " must not include query or fragment" }
  | otherwise =
      Right (T.dropWhileEnd (== '/') normalizedEndpointBase)
  where
    endpointBase = T.strip rawBase
    lowerEndpointBase = T.toLower endpointBase
    matchingScheme =
      find
        (\scheme -> (scheme <> "://") `T.isPrefixOf` lowerEndpointBase)
        allowedSchemes
    mRemainder =
      fmap
        (\scheme -> T.drop (T.length scheme + 3) endpointBase)
        matchingScheme
    normalizedEndpointBase =
      case matchingScheme of
        Nothing -> endpointBase
        Just scheme -> scheme <> "://" <> T.drop (T.length scheme + 3) endpointBase
    authority =
      maybe "" (T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#')) mRemainder
    fieldBody suffix =
      BL.fromStrict (TE.encodeUtf8 (label <> suffix))

radioFieldError :: Text -> ServerError -> ServerError
radioFieldError label err =
  let bodyText =
        TE.decodeUtf8With lenientDecode (BL.toStrict (errBody err))
  in err
      { errBody =
          BL.fromStrict
            (TE.encodeUtf8 (T.replace "streamUrl" label bodyText))
      }

normalizePublicBaseScheme :: Text -> Text
normalizePublicBaseScheme baseUrl
  | "https://" `T.isPrefixOf` lowerUrl = "https://" <> T.drop 8 baseUrl
  | "http://" `T.isPrefixOf` lowerUrl = "http://" <> T.drop 7 baseUrl
  | otherwise = baseUrl
  where
    lowerUrl = T.toLower baseUrl

validateRadioImportSources :: Maybe [Text] -> Either ServerError [Text]
validateRadioImportSources Nothing = Right defaultRadioImportSources
validateRadioImportSources (Just rawSources) =
  case dedupeCanonicalSources (filter (not . T.null) (map (T.strip . canonicalRadioImportSource) rawSources)) of
    [] ->
      Left err400 { errBody = "sources must include at least one public http(s) URL" }
    cleanedSources
      | length cleanedSources > maxRadioImportSources ->
          Left err400 { errBody = "sources must include at most 8 public http(s) URLs" }
      | otherwise ->
          traverse validateExplicitSource cleanedSources
  where
    validateExplicitSource source =
      case validateRadioStreamUrl source of
        Left err -> Left (toRadioImportSourceError err)
        Right validSource -> Right validSource

    toRadioImportSourceError err =
      let bodyText =
            TE.decodeUtf8With lenientDecode (BL.toStrict (errBody err))
      in err
          { errBody =
              BL.fromStrict
                (TE.encodeUtf8 (T.replace "streamUrl" "source" bodyText))
          }

    dedupeCanonicalSources =
      reverse
        . fst
        . foldl'
            (\(deduped, seen) source ->
               let sourceKey = T.toLower source
               in if Map.member sourceKey seen
                    then (deduped, seen)
                    else (source : deduped, Map.insert sourceKey () seen))
            ([], Map.empty)

maxRadioImportSources :: Int
maxRadioImportSources = 8

defaultRadioImportSources :: [Text]
defaultRadioImportSources =
  [ "https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv"
  , "https://www.rcast.net/dir"
  , "https://www.internet-radio.com"
  , "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/master/all.m3u"
  ]

canonicalRadioImportSource :: Text -> Text
canonicalRadioImportSource src
  | Just path <- githubComPath source
  , githubRepoPath "mikepierce/internet-radio-streams" path =
      "https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv"
  | Just path <- githubComPath source
  , githubRepoPath "junguler/m3u-radio-music-playlists" path =
      "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/master/all.m3u"
  | T.isSuffixOf ".csv" source = source
  | otherwise = source
  where
    source = T.strip src
    githubComPath raw =
      T.stripPrefix "https://github.com/" lowerRaw
        <|> T.stripPrefix "http://github.com/" lowerRaw
      where
        lowerRaw = T.toLower raw
    githubRepoPath repo path =
      path == repo || (repo <> "/") `T.isPrefixOf` path

validateRadioAuthority :: Text -> Either ServerError ()
validateRadioAuthority rawAuthority
  | T.any (== '@') rawAuthority =
      Left err400 { errBody = "streamUrl must not include user info" }
  | "[" `T.isPrefixOf` rawAuthority =
      validateBracketedAuthority rawAuthority
  | T.count ":" rawAuthority > 1 =
      Left err400 { errBody = "streamUrl must include a valid host" }
  | otherwise =
      let (host, portSuffix) = T.breakOn ":" rawAuthority
      in do
        validateAuthorityHost host
        validatePortSuffix portSuffix
  where
    validateBracketedAuthority authority = do
      let (hostPart, rest) = T.breakOn "]" authority
          host = T.drop 1 hostPart
      if T.null rest
        then Left err400 { errBody = "streamUrl must include a valid host" }
        else do
          validateBracketedHost host
          validatePortSuffix (T.drop 1 rest)

    validateBracketedHost host
      | T.null host = Left err400 { errBody = "streamUrl must include a host" }
      | not (T.any (== ':') host) = Left err400 { errBody = "streamUrl must include a valid host" }
      | T.isInfixOf ":::" host = Left err400 { errBody = "streamUrl must include a valid host" }
      | T.any (not . isValidBracketedHostChar) host =
          Left err400 { errBody = "streamUrl must include a valid host" }
      | otherwise = validatePublicRadioHost host
      where
        isValidBracketedHostChar c = isHexDigit c || c == ':' || c == '.'

    validateAuthorityHost host
      | T.null host = Left err400 { errBody = "streamUrl must include a host" }
      | T.isPrefixOf "." host || T.isSuffixOf "." host =
          Left err400 { errBody = "streamUrl must include a valid host" }
      | isAmbiguousNumericHost host =
          Left err400 { errBody = "streamUrl must include a valid host" }
      | any invalidHostLabel (T.splitOn "." host) =
          Left err400 { errBody = "streamUrl must include a valid host" }
      | otherwise = validatePublicRadioHost host
      where
        invalidHostLabel label =
          T.null label
            || T.isPrefixOf "-" label
            || T.isSuffixOf "-" label
            || T.any (not . isValidHostChar) label

        isValidHostChar c = isAlphaNum c || c == '-'

    validatePortSuffix suffix
      | T.null suffix = Right ()
      | ":" `T.isPrefixOf` suffix =
          let port = T.drop 1 suffix
          in if T.null port || T.any (not . isDigit) port
               then Left err400 { errBody = "streamUrl port must be numeric" }
               else case readMaybe (T.unpack port) :: Maybe Int of
                 Just portNumber | portNumber >= 1 && portNumber <= 65535 ->
                   Right ()
                 _ ->
                   Left err400 { errBody = "streamUrl port must be between 1 and 65535" }
      | otherwise =
          Left err400 { errBody = "streamUrl must include a valid host" }

validatePublicRadioHost :: Text -> Either ServerError ()
validatePublicRadioHost rawHost
  | normalized == "localhost" || ".localhost" `T.isSuffixOf` normalized =
      Left nonPublicRadioHostError
  | Just octets <- parseIpv4Octets normalized
  , isNonPublicIpv4 octets =
      Left nonPublicRadioHostError
  | hasAmbiguousTrailingIpv4 normalized =
      Left err400 { errBody = "streamUrl must include a valid host" }
  | Just octets <- trailingIpv4Octets normalized
  , isNonPublicIpv4 octets =
      Left nonPublicRadioHostError
  | isNonPublicIpv6 normalized =
      Left nonPublicRadioHostError
  | requiresExplicitPublicHostname normalized =
      Left err400 { errBody = "streamUrl host must be a public hostname or IP address" }
  | otherwise =
      Right ()
  where
    normalized = T.toLower rawHost
    nonPublicRadioHostError =
      err400
        { errBody =
            "streamUrl must not target localhost or private network addresses, including reserved ranges"
        }

    requiresExplicitPublicHostname host =
      not (T.any (== '.') host)
        && not (T.any (== ':') host)
        && isNothing (parseIpv4Octets host)

hasAmbiguousTrailingIpv4 :: Text -> Bool
hasAmbiguousTrailingIpv4 host =
  let suffix = T.takeWhileEnd (/= ':') host
  in T.any (== '.') suffix
      && T.all (\c -> isDigit c || c == '.') suffix
      && isNothing (parseIpv4Octets suffix)

parseIpv4Octets :: Text -> Maybe (Int, Int, Int, Int)
parseIpv4Octets host =
  case T.splitOn "." host of
    [a, b, c, d] -> do
      oa <- parseOctet a
      ob <- parseOctet b
      oc <- parseOctet c
      od <- parseOctet d
      pure (oa, ob, oc, od)
    _ -> Nothing
  where
    parseOctet octet
      | T.null octet || T.any (not . isDigit) octet = Nothing
      | T.length octet > 1 && T.head octet == '0' = Nothing
      | otherwise = do
          value <- readMaybe (T.unpack octet)
          if value >= 0 && value <= 255
            then Just value
            else Nothing

isAmbiguousNumericHost :: Text -> Bool
isAmbiguousNumericHost host =
  T.all (\c -> isDigit c || c == '.') host
    && isNothing (parseIpv4Octets host)

trailingIpv4Octets :: Text -> Maybe (Int, Int, Int, Int)
trailingIpv4Octets host =
  let suffix = T.takeWhileEnd (/= ':') host
  in if T.any (== '.') suffix
       then parseIpv4Octets suffix
       else Nothing

isNonPublicIpv4 :: (Int, Int, Int, Int) -> Bool
isNonPublicIpv4 (a, b, c, _) =
  a == 0
    || a == 10
    || a == 127
    || (a == 100 && b >= 64 && b <= 127)
    || (a == 169 && b == 254)
    || (a == 172 && b >= 16 && b <= 31)
    || (a == 192 && b == 0 && c == 0)
    || (a == 192 && b == 0 && c == 2)
    || (a == 192 && b == 168)
    || (a == 198 && (b == 18 || b == 19))
    || (a == 198 && b == 51 && c == 100)
    || (a == 203 && b == 0 && c == 113)
    || (a >= 224 && a <= 255)

isNonPublicIpv6 :: Text -> Bool
isNonPublicIpv6 host =
  host == "::"
    || host == "::1"
    || isDocumentation firstSegment secondSegment
    || isMulticast firstSegment
    || isUniqueLocal firstSegment
    || isLinkLocal firstSegment
  where
    segments = T.splitOn ":" host
    firstSegment = headOrEmpty segments
    secondSegment =
      case drop 1 segments of
        segment:_ -> segment
        [] -> ""

    headOrEmpty [] = ""
    headOrEmpty (segment:_) = segment

    isDocumentation first second =
      first == "2001" && second `elem` ["db8", "0db8"]

    isMulticast segment =
      "ff" `T.isPrefixOf` segment

    isUniqueLocal segment =
      "fc" `T.isPrefixOf` segment || "fd" `T.isPrefixOf` segment

    isLinkLocal segment =
      any (`T.isPrefixOf` segment) ["fe8", "fe9", "fea", "feb"]

validateRadioImportLimit :: Maybe Int -> Either ServerError Int
validateRadioImportLimit = validateRadioBatchLimit 800

validateRadioMetadataRefreshLimit :: Maybe Int -> Either ServerError Int
validateRadioMetadataRefreshLimit = validateRadioBatchLimit 400

validateRadioBatchLimit :: Int -> Maybe Int -> Either ServerError Int
validateRadioBatchLimit defaultLimit Nothing = Right defaultLimit
validateRadioBatchLimit defaultLimit (Just rawLimit)
  | rawLimit < 1 || rawLimit > defaultLimit =
      Left err400 { errBody = "limit must be between 1 and " <> BL.fromStrict (BS8.pack (show defaultLimit)) }
  | otherwise =
      Right rawLimit

radioServer
  :: forall m.
     ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT RadioAPI m
radioServer user =
       searchStreams
  :<|> upsertActive
  :<|> importStreams
  :<|> refreshMetadata
  :<|> nowPlaying
  :<|> createTransmission
  :<|> getSelfPresence
  :<|> upsertPresence
  :<|> clearPresence
  :<|> getPresenceByParty
  where
    searchStreams :: Maybe Text -> Maybe Text -> m [RadioStreamDTO]
    searchStreams mCountry mGenre = do
      Env{..} <- ask
      rows <- liftIO $ flip runSqlPool envPool $
        selectList [RadioStreamIsActive ==. True] [Desc RadioStreamUpdatedAt, LimitTo 400]
      let cQuery = normalize mCountry
          gQuery = normalize mGenre
          matches (Entity _ RadioStream{..}) =
            matchesField cQuery radioStreamCountry && matchesField gQuery radioStreamGenre
      pure (map toDTO (filter matches rows))

    upsertActive :: RadioStreamUpsert -> m RadioStreamDTO
    upsertActive payload = do
      streamUrl <- either throwError pure (validateRadioStreamUrl (rsuStreamUrl payload))
      now <- liftIO getCurrentTime
      Env{..} <- ask
      let normalizedPayload = payload { rsuStreamUrl = streamUrl }
      (entity, _) <- liftIO $ flip runSqlPool envPool (saveStream now normalizedPayload)
      pure (toDTO entity)

    importStreams :: RadioImportRequest -> m RadioImportResult
    importStreams RadioImportRequest{..} = do
      cleanedSources <- either throwError pure (validateRadioImportSources rirSources)
      cap <- either throwError pure (validateRadioImportLimit rirLimit)
      manager <- liftIO $ newManager tlsManagerSettings
      fetchedResults <- liftIO $
        forM cleanedSources $ \src -> do
          res <- try (fetchSource manager src)
          pure $ case res of
            Left (ex :: SomeException) -> Left (src, T.pack (displayException ex))
            Right items                -> Right (src, items)
      let successes = [ items | Right (_, items) <- fetchedResults ]
          failedSources = [ src | Left (src, _) <- fetchedResults ]
          fetched = concat successes
      let deduped =
            take cap
              . Map.elems
              . foldl' (\m item -> Map.insertWith (const id) (T.toLower (rsuStreamUrl item)) item m) Map.empty
              . catMaybes
              $ map normalizeImportUpsert fetched
      now <- liftIO getCurrentTime
      Env{..} <- ask
      (inserted, updated) <- liftIO $ flip runSqlPool envPool $ do
        results <- forM deduped (saveStream now)
        let insCount = length (filter snd results)
            updCount = length results - insCount
        pure (insCount, updCount)
      pure RadioImportResult
        { rirProcessed = length deduped
        , rirInserted  = inserted
        , rirUpdated   = updated
        , rirSources   = cleanedSources
        , rirFailed    = length failedSources
        , rirFailedSources = failedSources
        }

    refreshMetadata :: RadioMetadataRefreshRequest -> m RadioMetadataRefreshResult
    refreshMetadata RadioMetadataRefreshRequest{..} = do
      cap <- either throwError pure (validateRadioMetadataRefreshLimit rmrLimit)
      let onlyMissing = fromMaybe False rmrOnlyMissing
      now <- liftIO getCurrentTime
      Env{..} <- ask
      rows <- liftIO $ flip runSqlPool envPool $
        selectList [RadioStreamIsActive ==. True] [Desc RadioStreamUpdatedAt]
      let candidates =
            take cap $
              if onlyMissing
                then filter (\(Entity _ RadioStream{..}) -> isNothing radioStreamName || isNothing radioStreamGenre) rows
                else rows
      manager <- liftIO $ newManager tlsManagerSettings
      results <- forM candidates $ \(Entity sid stream) -> do
        metaResult <- liftIO $ fetchStreamMetadata manager (radioStreamStreamUrl stream)
        case metaResult of
          Left _ -> do
            liftIO $ flip runSqlPool envPool $
              update sid [RadioStreamLastCheckedAt =. Just now]
            pure False
          Right StreamMetadata{..} -> do
            liftIO $ flip runSqlPool envPool $
              update sid
                [ RadioStreamName =. (smName <|> radioStreamName stream)
                , RadioStreamGenre =. (smGenre <|> radioStreamGenre stream)
                , RadioStreamLastCheckedAt =. Just now
                , RadioStreamUpdatedAt =. now
                ]
            pure True
      let processed = length candidates
          updated = length (filter id results)
          failed = processed - updated
      pure RadioMetadataRefreshResult
        { rmrProcessed = processed
        , rmrUpdated   = updated
        , rmrFailed    = failed
        }

    nowPlaying :: RadioNowPlayingRequest -> m RadioNowPlayingResult
    nowPlaying RadioNowPlayingRequest{..} = do
      streamUrl <- either throwError pure (validateRadioStreamUrl rnpStreamUrl)
      manager <- liftIO $ newManager tlsManagerSettings
      result <- liftIO $ fetchNowPlaying manager streamUrl
      case result of
        Left _ -> pure (RadioNowPlayingResult Nothing Nothing Nothing)
        Right title ->
          let cleaned = normalizeMaybe title
              (artist, track) = maybe (Nothing, Nothing) splitStreamTitle cleaned
          in pure (RadioNowPlayingResult cleaned artist track)

    saveStream :: UTCTime -> RadioStreamUpsert -> SqlPersistT IO (Entity RadioStream, Bool)
    saveStream now RadioStreamUpsert{..} = do
      let streamUrl = T.strip rsuStreamUrl
          name      = normalizeMaybe rsuName
          country   = normalizeMaybe rsuCountry
          genre     = normalizeMaybe rsuGenre
      mExisting <- getBy (UniqueRadioStreamUrl streamUrl)
      case mExisting of
        Nothing -> do
          let record = RadioStream
                { radioStreamStreamUrl = streamUrl
                , radioStreamName = name
                , radioStreamCountry = country
                , radioStreamGenre = genre
                , radioStreamIsActive = True
                , radioStreamLastCheckedAt = Just now
                , radioStreamCreatedAt = now
                , radioStreamUpdatedAt = now
                }
          newId <- insert record
          pure (Entity newId record, True)
        Just (Entity sid old) -> do
          let merged = old
                { radioStreamName = name <|> radioStreamName old
                , radioStreamCountry = country <|> radioStreamCountry old
                , radioStreamGenre = genre <|> radioStreamGenre old
                , radioStreamIsActive = True
                , radioStreamLastCheckedAt = Just now
                , radioStreamUpdatedAt = now
                }
          update sid
            [ RadioStreamName =. radioStreamName merged
            , RadioStreamCountry =. radioStreamCountry merged
            , RadioStreamGenre =. radioStreamGenre merged
            , RadioStreamIsActive =. True
            , RadioStreamLastCheckedAt =. Just now
            , RadioStreamUpdatedAt =. now
            ]
          pure (Entity sid merged, False)

    matchesField :: Maybe Text -> Maybe Text -> Bool
    matchesField Nothing _ = True
    matchesField (Just query) mVal =
      case mVal of
        Nothing    -> False
        Just value -> query `T.isInfixOf` T.toLower value

    normalize :: Maybe Text -> Maybe Text
    normalize = fmap (T.toLower . T.strip)

    normalizeMaybe :: Maybe Text -> Maybe Text
    normalizeMaybe mTxt =
      case fmap T.strip mTxt of
        Just txt | not (T.null txt) -> Just txt
        _                           -> Nothing

    normalizeUpsert :: RadioStreamUpsert -> RadioStreamUpsert
    normalizeUpsert u =
      u { rsuStreamUrl = T.strip (rsuStreamUrl u)
        , rsuName      = normalizeMaybe (rsuName u)
        , rsuCountry   = normalizeMaybe (rsuCountry u)
        , rsuGenre     = normalizeMaybe (rsuGenre u)
        }

    normalizeImportUpsert :: RadioStreamUpsert -> Maybe RadioStreamUpsert
    normalizeImportUpsert rawUpsert =
      let normalized = normalizeUpsert rawUpsert
      in case validateRadioStreamUrl (rsuStreamUrl normalized) of
          Right validUrl | isStreamUrl validUrl ->
            Just normalized { rsuStreamUrl = validUrl }
          _ ->
            Nothing

    fetchSource :: Manager -> Text -> IO [RadioStreamUpsert]
    fetchSource manager src = do
      req <- parseRequest (T.unpack (canonicalRadioImportSource src))
      resp <- httpLbs req manager
      let bodyTxt = TE.decodeUtf8 (BL.toStrict (responseBody resp))
          csvItems = parseCsvStreams bodyTxt
      if not (null csvItems)
        then pure csvItems
        else pure (map toStreamUpsert (extractUrls bodyTxt))

    parseCsvStreams :: Text -> [RadioStreamUpsert]
    parseCsvStreams body =
      let ls = filter (not . T.null) (T.lines body)
      in case ls of
           [] -> []
           (header:rows) ->
             let cols = map (T.toLower . T.strip) (T.splitOn "," header)
                 mUrlIdx = findIndex (`elem` ["url","stream","stream_url","streamurl"]) cols
                 mNameIdx = findIndex (`elem` ["name","title","station","label"]) cols
                 mCountryIdx = findIndex (`elem` ["country","nation"]) cols
                 mGenreIdx = findIndex (`elem` ["genre","style"]) cols
                 pick idx cells = idx >>= (\i -> if i < length cells then Just (cells !! i) else Nothing)
             in catMaybes $ map (\row ->
                  let cells = map T.strip (T.splitOn "," row)
                      url = pick mUrlIdx cells
                  in case url of
                       Nothing -> Nothing
                       Just u  ->
                         Just RadioStreamUpsert
                           { rsuStreamUrl = u
                           , rsuName      = pick mNameIdx cells
                           , rsuCountry   = pick mCountryIdx cells
                           , rsuGenre     = pick mGenreIdx cells
                           }
                 ) rows

    extractUrls :: Text -> [Text]
    extractUrls body =
      let tokens = concatMap (T.split (`elem` ['"', '\'', '<', '>', '(', ')', '[', ']', ',', ';', '|'])) (T.words body)
          urls = filter isStreamUrl tokens
          dedup = Map.elems (foldl' (\m u -> Map.insertWith (const id) (T.toLower u) u m) Map.empty urls)
      in dedup

    isStreamUrl :: Text -> Bool
    isStreamUrl url =
      let lower = T.toLower url
          hasProto = "http://" `T.isPrefixOf` lower || "https://" `T.isPrefixOf` lower
          hasExt = any (`T.isInfixOf` lower) [".m3u", ".pls", ".aac", ".mp3", ".stream", "/stream"]
      in hasProto && hasExt

    toStreamUpsert :: Text -> RadioStreamUpsert
    toStreamUpsert url =
      RadioStreamUpsert
        { rsuStreamUrl = url
        , rsuName = Just (deriveName url)
        , rsuCountry = Nothing
        , rsuGenre = Nothing
        }

    deriveName :: Text -> Text
    deriveName url =
      let noProto = fromMaybe url (T.stripPrefix "https://" url <|> T.stripPrefix "http://" url)
          host = T.takeWhile (/= '/') noProto
      in if T.null host then "Radio" else host

    fetchStreamMetadata :: Manager -> Text -> IO (Either Text StreamMetadata)
    fetchStreamMetadata manager url = do
      result <- try $ do
        req0 <- parseRequest (T.unpack url)
        let req = req0
              { requestHeaders = ("Icy-MetaData","1") : ("User-Agent","tdf-radio-metadata/1.0") : requestHeaders req0
              , responseTimeout = responseTimeoutMicro 5000000
              }
        httpLbs req manager
      pure $ case result of
        Left (ex :: SomeException) ->
          Left (T.pack (displayException ex))
        Right resp ->
          let hdrs = responseHeaders resp
              lookupTxt nameKey = fmap (TE.decodeUtf8With lenientDecode) (lookupHeader nameKey hdrs)
              metaName = lookupTxt "icy-name"
              metaGenre = lookupTxt "icy-genre"
          in if isNothing metaName && isNothing metaGenre
               then Left "no metadata"
               else Right StreamMetadata { smName = metaName, smGenre = metaGenre }

    fetchNowPlaying :: Manager -> Text -> IO (Either Text (Maybe Text))
    fetchNowPlaying manager url = do
      result <- try $ do
        req0 <- parseRequest (T.unpack url)
        let req = req0
              { requestHeaders = ("Icy-MetaData","1") : ("User-Agent","tdf-radio-now-playing/1.0") : requestHeaders req0
              , responseTimeout = responseTimeoutMicro 5000000
              }
        withResponse req manager $ \resp -> do
          let metaIntLimit = 262144
          case parseIcyMetaInt (responseHeaders resp) of
            Nothing -> pure Nothing
            Just metaInt
              | metaInt <= 0 -> pure Nothing
              | metaInt > metaIntLimit -> pure Nothing
              | otherwise -> readIcyStreamTitle (responseBody resp) metaInt 2
      pure $ case result of
        Left (ex :: SomeException) -> Left (T.pack (displayException ex))
        Right title -> Right title

    parseIcyMetaInt :: [(CI.CI BS.ByteString, BS.ByteString)] -> Maybe Int
    parseIcyMetaInt hdrs = do
      raw <- lookupHeader "icy-metaint" hdrs
      case BS8.readInt raw of
        Just (value, _) | value > 0 -> Just value
        _ -> Nothing

    readIcyStreamTitle :: BodyReader -> Int -> Int -> IO (Maybe Text)
    readIcyStreamTitle body metaInt attempts = go BS.empty attempts
      where
        go _ 0 = pure Nothing
        go leftover remaining = do
          (audioChunk, rest) <- readExact body metaInt leftover
          if BS.length audioChunk < metaInt
            then pure Nothing
            else do
              (lenChunk, _restLen) <- readExact body 1 rest
              case BS.uncons lenChunk of
                Nothing -> pure Nothing
                Just (lenByte, restMetaStart) -> do
                  let metaLen = fromIntegral lenByte * 16
                  if metaLen <= 0
                    then go restMetaStart (remaining - 1)
                    else do
                      (metaChunk, restMeta) <- readExact body metaLen restMetaStart
                      if BS.length metaChunk < metaLen
                        then pure Nothing
                        else case parseStreamTitle metaChunk of
                               Just title -> pure (Just title)
                               Nothing -> go restMeta (remaining - 1)

    readExact :: BodyReader -> Int -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
    readExact _ need leftover | need <= 0 = pure (BS.empty, leftover)
    readExact _ need leftover | BS.length leftover >= need =
      let (wanted, rest) = BS.splitAt need leftover
      in pure (wanted, rest)
    readExact body need leftover = do
      chunk <- brRead body
      if BS.null chunk
        then pure (leftover, BS.empty)
        else readExact body need (BS.append leftover chunk)

    parseStreamTitle :: BS.ByteString -> Maybe Text
    parseStreamTitle raw =
      let decoded = TE.decodeUtf8With lenientDecode raw
          cleaned = T.strip (T.takeWhile (/= '\0') decoded)
      in extractFieldInsensitive "StreamTitle" cleaned

    splitStreamTitle :: Text -> (Maybe Text, Maybe Text)
    splitStreamTitle title =
      let trimmed = T.strip title
          separators = [" - ", " -- ", " / ", " | ", " : "]
          splitOn sep =
            let (left, right) = T.breakOn sep trimmed
            in if T.null right
              then Nothing
              else
                let after = T.drop (T.length sep) right
                    artist = T.strip left
                    track = T.strip after
                in if T.null artist || T.null track
                  then Nothing
                  else Just (artist, track)
      in case catMaybes (map splitOn separators) of
           (artist, track):_ -> (Just artist, Just track)
           [] -> (Nothing, Nothing)

    extractFieldInsensitive :: Text -> Text -> Maybe Text
    extractFieldInsensitive field txt =
      let lowerTxt = T.toLower txt
          singleMarker = T.toLower field <> "='"
          doubleMarker = T.toLower field <> "=\""
          extract marker quoteChar =
            let (before, rest) = T.breakOn marker lowerTxt
            in if T.null rest
              then Nothing
              else
                let idx = T.length before + T.length marker
                    after = T.drop idx txt
                    value = T.takeWhile (/= quoteChar) after
                    trimmed = T.strip value
                in if T.null trimmed then Nothing else Just trimmed
      in extract singleMarker '\'' <|> extract doubleMarker '"'

    toDTO :: Entity RadioStream -> RadioStreamDTO
    toDTO (Entity sid RadioStream{..}) =
      RadioStreamDTO
        { rsId = fromIntegral (fromSqlKey sid) :: Int64
        , rsName = radioStreamName
        , rsStreamUrl = radioStreamStreamUrl
        , rsCountry = radioStreamCountry
        , rsGenre = radioStreamGenre
        , rsActive = radioStreamIsActive
        , rsLastCheckedAt = radioStreamLastCheckedAt
        }

    getSelfPresence :: m (Maybe RadioPresenceDTO)
    getSelfPresence = fetchPresence (auPartyId user)

    getPresenceByParty :: Int64 -> m (Maybe RadioPresenceDTO)
    getPresenceByParty partyId = do
      when (partyId <= 0) $
        throwError err400 { errBody = "Invalid party id" }
      fetchPresence (toSqlKey partyId)

    fetchPresence :: PartyId -> m (Maybe RadioPresenceDTO)
    fetchPresence partyId = do
      Env{..} <- ask
      liftIO $ flip runSqlPool envPool $ do
        mRow <- selectFirst [PartyRadioPresencePartyId ==. partyId] []
        pure (presenceToDTO <$> mRow)

    upsertPresence :: RadioPresenceUpsert -> m RadioPresenceDTO
    upsertPresence RadioPresenceUpsert{..} = do
      streamUrl <- either throwError pure (validateRadioStreamUrl rpuStreamUrl)
      let name      = normalizeMaybe rpuStationName
          stationId = normalizeMaybe rpuStationId
      now <- liftIO getCurrentTime
      Env{..} <- ask
      entity <- liftIO $ flip runSqlPool envPool $ do
        mExisting <- getBy (UniquePartyPresence (auPartyId user))
        case mExisting of
          Nothing -> do
            let rec = PartyRadioPresence
                  { partyRadioPresencePartyId     = auPartyId user
                  , partyRadioPresenceStreamUrl   = streamUrl
                  , partyRadioPresenceStationName = name
                  , partyRadioPresenceStationId   = stationId
                  , partyRadioPresenceUpdatedAt   = now
                  }
            newId <- insert rec
            pure (Entity newId rec)
          Just (Entity pid old) -> do
            let streamChanged = streamUrl /= partyRadioPresenceStreamUrl old
                merged = old
                  { partyRadioPresenceStreamUrl   = streamUrl
                  -- Station metadata must describe the active stream. If the stream
                  -- changes and no fresh label arrives, clear the stale station info.
                  , partyRadioPresenceStationName =
                      if streamChanged
                        then name
                        else name <|> partyRadioPresenceStationName old
                  , partyRadioPresenceStationId   =
                      if streamChanged
                        then stationId
                        else stationId <|> partyRadioPresenceStationId old
                  , partyRadioPresenceUpdatedAt   = now
                  }
            update pid
              [ PartyRadioPresenceStreamUrl   =. partyRadioPresenceStreamUrl merged
              , PartyRadioPresenceStationName =. partyRadioPresenceStationName merged
              , PartyRadioPresenceStationId   =. partyRadioPresenceStationId merged
              , PartyRadioPresenceUpdatedAt   =. now
              ]
            pure (Entity pid merged)
      pure (presenceToDTO entity)

    clearPresence :: m NoContent
    clearPresence = do
      Env{..} <- ask
      liftIO $ flip runSqlPool envPool $
        deleteBy (UniquePartyPresence (auPartyId user))
      pure NoContent

    presenceToDTO :: Entity PartyRadioPresence -> RadioPresenceDTO
    presenceToDTO (Entity _ PartyRadioPresence{..}) =
      RadioPresenceDTO
        { rpPartyId     = fromIntegral (fromSqlKey partyRadioPresencePartyId)
        , rpStreamUrl   = partyRadioPresenceStreamUrl
        , rpStationName = partyRadioPresenceStationName
        , rpStationId   = partyRadioPresenceStationId
        , rpUpdatedAt   = partyRadioPresenceUpdatedAt
        }

    createTransmission :: RadioTransmissionRequest -> m RadioTransmissionInfo
    createTransmission RadioTransmissionRequest{..} = do
      now <- liftIO getCurrentTime
      Env{..} <- ask
      streamKey <- liftIO (toText <$> nextRandom)
      listenBaseRaw <- liftIO (readEnv "RADIO_PUBLIC_BASE" "https://tdf-hq.fly.dev/live")
      listenBase <- either throwError pure (validateRadioTransmissionPublicBase listenBaseRaw)
      let fallbackIngest = deriveBase listenBase "rtmp" "/live"
          fallbackWhip   = deriveBase listenBase "https" "/whip"
      ingestBaseRaw <- liftIO (readEnv "RADIO_INGEST_BASE" fallbackIngest)
      whipBaseRaw <- liftIO (readEnv "RADIO_WHIP_BASE" fallbackWhip)
      ingestBase <- either throwError pure (validateRadioTransmissionIngestBase ingestBaseRaw)
      whipBase <- either throwError pure (validateRadioTransmissionWhipBase whipBaseRaw)
      let publicUrl = appendPath listenBase streamKey
          ingestUrl = appendPath ingestBase streamKey
          whipUrl = appendPath whipBase streamKey
          upsertPayload = RadioStreamUpsert
            { rsuStreamUrl = publicUrl
            , rsuName      = rtrName
            , rsuCountry   = rtrCountry
            , rsuGenre     = rtrGenre
            }
      entity <- liftIO $ flip runSqlPool envPool $ do
        (ent, _) <- saveStream now upsertPayload
        pure ent
      pure RadioTransmissionInfo
        { rtiStreamId  = fromIntegral (fromSqlKey (entityKey entity))
        , rtiStreamUrl = publicUrl
        , rtiIngestUrl = ingestUrl
        , rtiStreamKey = streamKey
        , rtiWhipUrl   = whipUrl
        }

    appendPath base path =
      let trimmed = T.dropWhileEnd (== '/') base
      in trimmed <> "/" <> path

    deriveBase :: Text -> Text -> Text -> Text
    deriveBase baseUrl newScheme newPath =
      let noScheme = fromMaybe baseUrl (T.stripPrefix "https://" baseUrl <|> T.stripPrefix "http://" baseUrl)
          host     = T.takeWhile (/= '/') noScheme
          cleanHost = if T.null host then "localhost" else host
          normalizedPath = if T.isPrefixOf "/" newPath then newPath else "/" <> newPath
      in newScheme <> "://" <> cleanHost <> normalizedPath

    readEnv key def = do
      mVal <- lookupEnv key
      let cleaned = fmap (T.strip . T.pack) mVal
      pure $ case cleaned of
        Nothing   -> def
        Just txt | T.null txt -> def
                 | otherwise -> txt
