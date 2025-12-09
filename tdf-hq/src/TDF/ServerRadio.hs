{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.ServerRadio
  ( radioServer
  ) where

import           Control.Applicative    ((<|>))
import           Control.Exception      (SomeException, try, displayException)
import           Control.Monad          (forM, when)
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Int               (Int64)
import           Data.List              (foldl', find, findIndex)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes, fromMaybe, isNothing)
import qualified Data.ByteString        as BS
import qualified Data.CaseInsensitive   as CI
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Time              (UTCTime, getCurrentTime)
import qualified Data.ByteString.Lazy   as BL
import           Database.Persist       (Entity(..), (=.), (==.), SelectOpt(Desc, LimitTo), deleteBy, getBy, insert,
                                         selectList, selectFirst, update)
import           Database.Persist.Sql   (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant                (NoContent(..), ServerError, ServerT, err400, errBody, throwError, (:<|>)(..))
import           Network.HTTP.Client    (Manager, httpLbs, newManager, parseRequest, responseBody, responseHeaders,
                                         responseTimeoutMicro, requestHeaders, Request(..))
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           TDF.API.Radio          (RadioAPI)
import           TDF.API.Types          (RadioStreamDTO(..), RadioStreamUpsert(..), RadioPresenceDTO(..),
                                         RadioPresenceUpsert(..), RadioImportRequest(..), RadioImportResult(..),
                                         RadioMetadataRefreshRequest(..), RadioMetadataRefreshResult(..))
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
      let streamUrl = T.strip (rsuStreamUrl payload)
      when (T.null streamUrl) $
        throwError err400 { errBody = "streamUrl is required" }
      now <- liftIO getCurrentTime
      Env{..} <- ask
      (entity, _) <- liftIO $ flip runSqlPool envPool (saveStream now payload)
      pure (toDTO entity)

    importStreams :: RadioImportRequest -> m RadioImportResult
    importStreams RadioImportRequest{..} = do
      let sources = fromMaybe defaultSources rirSources
          cap     = max 1 (min 2000 (fromMaybe 800 rirLimit))
      manager <- liftIO $ newManager tlsManagerSettings
      fetched <- liftIO $ fmap concat $
        forM sources $ \src -> do
          res <- try (fetchSource manager src)
          case res of
            Left (_ :: SomeException) -> pure []
            Right items               -> pure items
      let deduped =
            take cap
              . Map.elems
              . foldl' (\m item -> Map.insertWith (const id) (T.toLower (rsuStreamUrl item)) item m) Map.empty
              $ fetched
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
        , rirSources   = sources
        }

    refreshMetadata :: RadioMetadataRefreshRequest -> m RadioMetadataRefreshResult
    refreshMetadata RadioMetadataRefreshRequest{..} = do
      let cap = max 1 (min 2000 (fromMaybe 400 rmrLimit))
          onlyMissing = fromMaybe False rmrOnlyMissing
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

    defaultSources :: [Text]
    defaultSources =
      [ "https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv"
      , "https://www.rcast.net/dir"
      , "https://www.internet-radio.com"
      , "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/master/all.m3u"
      ]

    fetchSource :: Manager -> Text -> IO [RadioStreamUpsert]
    fetchSource manager src = do
      req <- parseRequest (T.unpack (canonicalSource src))
      resp <- httpLbs req manager
      let bodyTxt = TE.decodeUtf8 (BL.toStrict (responseBody resp))
          csvItems = parseCsvStreams bodyTxt
      if not (null csvItems)
        then pure csvItems
        else pure (map toStreamUpsert (extractUrls bodyTxt))

    canonicalSource :: Text -> Text
    canonicalSource src
      | "github.com/mikepierce/internet-radio-streams" `T.isInfixOf` src =
          "https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv"
      | "github.com/junguler/m3u-radio-music-playlists" `T.isInfixOf` src =
          "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/master/all.m3u"
      | T.isSuffixOf ".csv" src = src
      | otherwise = src

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
      let streamUrl = T.strip rpuStreamUrl
          name      = normalizeMaybe rpuStationName
          stationId = normalizeMaybe rpuStationId
      when (T.null streamUrl) $
        throwError err400 { errBody = "streamUrl is required" }
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
            let merged = old
                  { partyRadioPresenceStreamUrl   = streamUrl
                  , partyRadioPresenceStationName = name <|> partyRadioPresenceStationName old
                  , partyRadioPresenceStationId   = stationId <|> partyRadioPresenceStationId old
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
