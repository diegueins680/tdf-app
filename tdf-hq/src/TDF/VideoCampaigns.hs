{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- Draft planning only: this module has no provider client, publishing worker or budget.
module TDF.VideoCampaigns
  ( VideoCampaignAPI, CampaignWrite(..), CampaignDTO(..), campaignServer
  , campaignScope, validateDocument, saveCampaign, listCampaigns
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl)
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Database.Persist
import Database.Persist.Sql (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey, updateWhereCount)
import GHC.Generics (Generic)
import Network.URI (URI(..), URIAuth(..), parseURI)
import Servant
import TDF.Auth
import TDF.DB (Env(..))
import TDF.Models (PartyId, RoleEnum(..), Unique(..), partyCreatedAt, fanFollowCreatedAt)
import qualified TDF.ModelsExtra as M

type VideoCampaignAPI = "video-campaigns" :> QueryParam "artistId" Int64 :>
  ( Get '[JSON] [CampaignDTO]
  :<|> Capture "campaignKey" Text :>
    ( ReqBody '[JSON] CampaignWrite :> Put '[JSON] CampaignDTO
    :<|> "metrics" :> Get '[JSON] CampaignMetrics
    :<|> "conversion" :> ReqBody '[JSON] CampaignConversion :> Post '[JSON] NoContent
    )
  )

data CampaignWrite = CampaignWrite
  { expectedRevision :: Int64, document :: Value } deriving (Show, Generic)
instance FromJSON CampaignWrite where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

data CampaignDTO = CampaignDTO
  { id :: Text, artistId :: Int64, revision :: Int64, content :: Value
  , createdAt :: UTCTime, updatedAt :: UTCTime
  } deriving (Show, Generic)
instance ToJSON CampaignDTO

data CampaignMetrics = CampaignMetrics
  { registrations :: Int, follows :: Int } deriving (Show, Generic)
instance ToJSON CampaignMetrics

data CampaignConversion = CampaignConversion
  { kind :: Text, source :: Text, piece :: Text } deriving (Show, Generic)
instance FromJSON CampaignConversion where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

campaignScope :: AuthedUser -> Maybe Int64 -> Either ServerError PartyId
campaignScope user target
  | fromSqlKey actor <= 0 || length roles /= length (nub roles) = Left err403
  | requested <= 0 = Left err400
  | hasStrictAdminAccess user && hasModuleAccess ModuleAdmin user = Right (toSqlKey requested)
  | requested == fromSqlKey actor && any (`elem` roles) [Artist, Artista] = Right actor
  | otherwise = Left err403
  where
    actor = auPartyId user
    roles = auRoles user
    requested = fromMaybe (fromSqlKey actor) target

-- URLs are stored, never fetched by this API. Embedded private/network URLs are rejected.
safeMediaUrl :: Text -> Bool
safeMediaUrl "" = True
safeMediaUrl value = case parseURI (T.unpack value) of
  Just URI{uriScheme = "https:", uriAuthority = Just URIAuth{..}} ->
    null uriUserInfo && length uriRegName > 3 && '.' `elem` uriRegName
      && all (`notElem` ("[]:" :: String)) uriRegName
      && not (all (`elem` ("0123456789." :: String)) uriRegName)
      && uriRegName /= "localhost" && not (".local" `T.isSuffixOf` T.pack uriRegName)
      && length (T.unpack value) <= 2048 && not (T.any isControl value)
  _ -> False

validateDocument :: Value -> Either Text ()
validateDocument value
  | BL.length (encode value) > 131072 = Left "La campaña supera el tamaño permitido."
  | otherwise = either (const (Left "Revisa los campos de la campaña.")) Right
      (parseEither parseDocument value)
  where
    parseDocument = withObject "CampaignDocument" $ \o -> do
      title <- o .: "title" :: Parser Text
      status <- o .: "status" :: Parser Text
      video <- o .: "videoUrl" :: Parser Text
      cover <- o .: "coverUrl" :: Parser Text
      channels <- o .: "channels" :: Parser [Text]
      duration <- o .: "durationSeconds" :: Parser Double
      pieces <- o .: "pieces" :: Parser [Value]
      unless (not (T.null (T.strip title)) && T.length title <= 200
        && status `elem` ["draft", "ready"] && safeMediaUrl video && safeMediaUrl cover
        && duration >= 0 && duration <= 43200 && not (null channels)
        && channels == nub channels && all (`elem` ["instagram", "tiktok"]) channels
        && length pieces <= 30) (fail "Invalid document")
      keys <- traverse (parsePiece channels duration) pieces
      unless (length keys == length (nub keys)) (fail "Duplicate piece")
      when (status == "ready" && (T.null video || null pieces)) (fail "Incomplete campaign")
      when (status == "ready") $ mapM_ (withObject "ReadyPiece" $ \p -> do
        asset <- p .: "assetUrl" :: Parser Text
        caption <- p .: "caption" :: Parser Text
        unless (not (T.null asset) && not (T.null (T.strip caption)))
          (fail "Incomplete piece")) pieces
    parsePiece channels duration = withObject "CampaignPiece" $ \o -> do
      key <- o .: "id" :: Parser Text
      channel <- o .: "channel" :: Parser Text
      day <- o .: "day" :: Parser Int
      caption <- o .: "caption" :: Parser Text
      asset <- o .: "assetUrl" :: Parser Text
      state <- o .: "status" :: Parser Text
      published <- o .: "publishedUrl" :: Parser Text
      start <- o .: "sourceStart" :: Parser Double
      end <- o .: "sourceEnd" :: Parser Double
      unless (T.length key > 0 && T.length key <= 100 && channel `elem` channels
        && day >= -3 && day <= 13 && T.length caption <= 2200
        && safeMediaUrl asset && safeMediaUrl published
        && state `elem` ["draft", "prepared", "exported", "published_manual"]
        && start >= 0 && end >= start && (duration == 0 || end <= duration)
        && (state /= "published_manual" || validPost channel published)) (fail "Invalid piece")
      pure key
    validPost channel url = case parseURI (T.unpack url) >>= uriAuthority of
      Just authority -> let host = uriRegName authority in
        if channel == "instagram" then host `elem` ["www.instagram.com", "instagram.com"]
        else host `elem` ["www.tiktok.com", "tiktok.com"]
      _ -> False

campaignDTO :: M.VideoCampaign -> Either ServerError CampaignDTO
campaignDTO row = case eitherDecodeStrict' (TE.encodeUtf8 (M.videoCampaignDocument row)) of
  Left _ -> Left err500
  Right value -> Right CampaignDTO
    { id = M.videoCampaignCampaignKey row, artistId = fromSqlKey (M.videoCampaignArtistPartyId row)
    , revision = M.videoCampaignRevision row, content = value
    , createdAt = M.videoCampaignCreatedAt row, updatedAt = M.videoCampaignUpdatedAt row }

listCampaigns :: PartyId -> SqlPersistT IO (Either ServerError [CampaignDTO])
listCampaigns artist = do
  rows <- selectList [M.VideoCampaignArtistPartyId ==. artist]
    [Desc M.VideoCampaignUpdatedAt, LimitTo 100]
  pure (traverse (campaignDTO . entityVal) rows)

-- Compare-and-swap plus exact-body replay recovery. No stale save can overwrite a newer edit.
saveCampaign :: PartyId -> Text -> CampaignWrite -> UTCTime
  -> SqlPersistT IO (Either ServerError CampaignDTO)
saveCampaign artist key CampaignWrite{..} now = do
  let body = TE.decodeUtf8 (BL.toStrict (encode document))
      unique = M.UniqueVideoCampaign artist key
  existing <- getBy unique
  case existing of
    Nothing | expectedRevision == 0 -> do
      inserted <- insertUnique (M.VideoCampaign artist key 1 body now now)
      case inserted of
        Just _ -> pure (campaignDTO (M.VideoCampaign artist key 1 body now now))
        Nothing -> replay body =<< getBy unique
    Just (Entity rowId row)
      | expectedRevision == M.videoCampaignRevision row -> do
          count <- updateWhereCount
            [ M.VideoCampaignId ==. rowId, M.VideoCampaignRevision ==. expectedRevision ]
            [ M.VideoCampaignDocument =. body, M.VideoCampaignRevision +=. 1
            , M.VideoCampaignUpdatedAt =. now ]
          if count == 1 then reload else replay body =<< getBy unique
      | otherwise -> replay body existing
    _ -> pure (Left err409)
  where
    reload = do
      stored <- getBy (M.UniqueVideoCampaign artist key)
      pure $ maybe (Left err409) (campaignDTO . entityVal) stored
    replay body stored = pure $ case stored of
      Just (Entity _ row)
        | M.videoCampaignRevision row == expectedRevision + 1
        , M.videoCampaignDocument row == body -> campaignDTO row
      _ -> Left err409 { errBody = "La campaña cambió. Recarga antes de guardar." }

campaignServer :: AuthedUser -> Maybe Int64 -> ServerT
  (Get '[JSON] [CampaignDTO] :<|> Capture "campaignKey" Text :>
    ( ReqBody '[JSON] CampaignWrite :> Put '[JSON] CampaignDTO
    :<|> "metrics" :> Get '[JSON] CampaignMetrics
    :<|> "conversion" :> ReqBody '[JSON] CampaignConversion :> Post '[JSON] NoContent
    )) (ReaderT Env Handler)
campaignServer user target = list :<|> (\key -> save key :<|> metrics key :<|> convert key)
  where
    query :: (PartyId -> SqlPersistT IO (Either ServerError a)) -> ReaderT Env Handler a
    query action = do
      artist <- either throwError pure (campaignScope user target)
      pool <- asks envPool
      result <- liftIO $ flip runSqlPool pool $
        withCurrentAuthorization (\live -> () <$ campaignScope live target) user $ do
          profile <- getBy (UniqueArtistProfile artist)
          if isJust profile then action artist else pure (Left err404)
      either throwError (either throwError pure) result
    list = query listCampaigns
    save key payload = do
      unless (isJust (UUID.fromText key) && expectedRevision payload >= 0
        && expectedRevision payload < maxBound) $ throwError err400
      either (\msg -> throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }) pure
        (validateDocument (document payload))
      now <- liftIO getCurrentTime
      query (\artist -> saveCampaign artist key payload now)
    metrics key = query $ \artist -> do
      found <- getBy (M.UniqueVideoCampaign artist key)
      case found of
        Nothing -> pure (Left err404)
        Just (Entity campaignId _) -> do
          registered <- count [ M.VideoCampaignConversionCampaignId ==. campaignId
            , M.VideoCampaignConversionKind ==. "registration" ]
          followed <- count [ M.VideoCampaignConversionCampaignId ==. campaignId
            , M.VideoCampaignConversionKind ==. "follow" ]
          pure (Right (CampaignMetrics registered followed))
    convert key CampaignConversion{..} = do
      unless (isJust (UUID.fromText key) && kind `elem` ["registration", "follow"]
        && source `elem` ["instagram", "tiktok"] && T.length piece <= 100) $
        throwError err400
      requested <- maybe (throwError err400) pure target
      when (requested <= 0) $ throwError err400
      pool <- asks envPool
      now <- liftIO getCurrentTime
      result <- liftIO $ flip runSqlPool pool $ withCurrentAuthSession user $ do
        campaign <- getBy (M.UniqueVideoCampaign (toSqlKey requested) key)
        actor <- get (auPartyId user)
        following <- getBy (UniqueFanFollow (auPartyId user) (toSqlKey requested))
        case (campaign, actor) of
          (Just (Entity campaignId row), Just party) -> do
            let check = withObject "Campaign" $ \o -> do
                  state <- o .: "status" :: Parser Text
                  pieces <- o .: "pieces" :: Parser [Value]
                  let match = withObject "Piece" $ \p ->
                        (&&) <$> ((== piece) <$> p .: "id") <*> ((== source) <$> p .: "channel")
                  pure (state == "ready" && any (== Right True) (map (parseEither match) pieces))
                storedDocument = TE.encodeUtf8 (M.videoCampaignDocument row)
                validPiece = case eitherDecodeStrict' storedDocument of
                  Right value -> parseEither check value == Right True
                  Left _ -> False
                eventTime = if kind == "registration" then Just (partyCreatedAt party)
                  else fanFollowCreatedAt . entityVal <$> following
                recent time = time >= M.videoCampaignCreatedAt row
                  && diffUTCTime now time >= 0 && diffUTCTime now time <= 3600
            if validPiece && maybe False recent eventTime then do
              _ <- insertUnique (M.VideoCampaignConversion campaignId (auPartyId user)
                kind source piece now)
              pure (Right NoContent)
            else pure (Left err400)
          _ -> pure (Left err404)
      maybe (throwError err401) (either throwError pure) result
