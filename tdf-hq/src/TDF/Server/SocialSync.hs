{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.SocialSync
  ( socialSyncServer
  ) where

import           Control.Monad              (forM)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Data.Int                   (Int64)
import           Data.List                  (nub)
import           Data.Maybe                 (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           Data.Time                  (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql       (SqlPersistT, runSqlPool, toSqlKey)
import           Servant
import           Web.PathPieces             (toPathPiece)

import           TDF.API.SocialSyncAPI
import           TDF.Auth                   (AuthedUser)
import           TDF.DB                     (Env(..))
import           TDF.DTO.SocialSyncDTO
import           TDF.Models

socialSyncServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT SocialSyncAPI m
socialSyncServer _user =
       ingestHandler
  :<|> listHandler
  where
    ingestHandler
      :: ( MonadReader Env m
         , MonadIO m
         , MonadError ServerError m
         )
      => SocialSyncIngestRequest
      -> m SocialSyncIngestResponse
    ingestHandler SocialSyncIngestRequest{..} = do
      now <- liftIO getCurrentTime
      results <- forM ssirPosts $ \payload -> do
        let platform = normalizePlatform (sspPlatform payload)
        let ingestSrc = fromMaybe "manual" (sspIngestSource payload)
        artistPartyKey <- traverse parsePartyId (sspArtistPartyId payload)
        artistProfileKey <- traverse parseProfileId (sspArtistProfileId payload)
        let tagList = classifyTags (sspCaption payload)
            summaryTxt = buildSummary (sspCaption payload)
            tagsText = nonEmptyText (T.intercalate "," tagList)
            mediaText = nonEmptyText (T.intercalate "\n" (fromMaybe [] (sspMediaUrls payload)))
        existing <- withPool $ getBy (UniqueSocialSyncPost platform (sspExternalPostId payload))
        case existing of
          Just (Entity key _) -> do
            let updates = concat
                  [ setMaybe SocialSyncPostCaption (sspCaption payload)
                  , setMaybe SocialSyncPostPermalink (sspPermalink payload)
                  , setMaybe SocialSyncPostMediaUrls mediaText
                  , setMaybe SocialSyncPostPostedAt (sspPostedAt payload)
                  , setMaybe SocialSyncPostTags tagsText
                  , setMaybe SocialSyncPostSummary summaryTxt
                  , setMaybe SocialSyncPostArtistPartyId artistPartyKey
                  , setMaybe SocialSyncPostArtistProfileId artistProfileKey
                  , [SocialSyncPostIngestSource =. ingestSrc]
                  , setMaybe SocialSyncPostLikeCount (sspLikeCount payload)
                  , setMaybe SocialSyncPostCommentCount (sspCommentCount payload)
                  , setMaybe SocialSyncPostShareCount (sspShareCount payload)
                  , setMaybe SocialSyncPostViewCount (sspViewCount payload)
                  , [ SocialSyncPostUpdatedAt =. now
                    , SocialSyncPostFetchedAt =. now
                    ]
                  ]
            withPool $ update key updates
            pure False
          Nothing -> do
            let record = SocialSyncPost
                  { socialSyncPostAccountId = Nothing
                  , socialSyncPostPlatform = platform
                  , socialSyncPostExternalPostId = sspExternalPostId payload
                  , socialSyncPostArtistPartyId = artistPartyKey
                  , socialSyncPostArtistProfileId = artistProfileKey
                  , socialSyncPostCaption = sspCaption payload
                  , socialSyncPostPermalink = sspPermalink payload
                  , socialSyncPostMediaUrls = mediaText
                  , socialSyncPostPostedAt = sspPostedAt payload
                  , socialSyncPostFetchedAt = now
                  , socialSyncPostTags = tagsText
                  , socialSyncPostSummary = summaryTxt
                  , socialSyncPostIngestSource = ingestSrc
                  , socialSyncPostLikeCount = sspLikeCount payload
                  , socialSyncPostCommentCount = sspCommentCount payload
                  , socialSyncPostShareCount = sspShareCount payload
                  , socialSyncPostViewCount = sspViewCount payload
                  , socialSyncPostCreatedAt = now
                  , socialSyncPostUpdatedAt = now
                  }
            withPool $ insert_ record
            pure True
      let inserted = length (filter id results)
          updated = length results - inserted
      let platformLabel = maybe "mixed" (normalizePlatform . sspPlatform) (listToMaybe ssirPosts)
      let runSource = fromMaybe "manual" (listToMaybe ssirPosts >>= sspIngestSource)
      _ <- withPool $ insert SocialSyncRun
        { socialSyncRunPlatform = platformLabel
        , socialSyncRunIngestSource = runSource
        , socialSyncRunStartedAt = now
        , socialSyncRunEndedAt = Just now
        , socialSyncRunStatus = "ok"
        , socialSyncRunNewPosts = inserted
        , socialSyncRunUpdatedPosts = updated
        , socialSyncRunErrorMessage = Nothing
        }
      pure SocialSyncIngestResponse
        { ssirInserted = inserted
        , ssirUpdated = updated
        , ssirTotal = length results
        }

    listHandler
      :: ( MonadReader Env m
         , MonadIO m
         , MonadError ServerError m
         )
      => Maybe Text
      -> Maybe Text
      -> Maybe Text
      -> Maybe Text
      -> Maybe Int
      -> m [SocialSyncPostDTO]
    listHandler mPlatform mParty mProfile mTag mLimit = do
      partyKey <- traverse parsePartyId mParty
      profileKey <- traverse parseProfileId mProfile
      let filters = catMaybes
            [ (SocialSyncPostPlatform ==.) <$> fmap normalizePlatform mPlatform
            , (SocialSyncPostArtistPartyId ==.) . Just <$> partyKey
            , (SocialSyncPostArtistProfileId ==.) . Just <$> profileKey
            ]
          limitVal = clampLimit (fromMaybe 50 mLimit)
      rows <- withPool $ selectList filters [Desc SocialSyncPostPostedAt, Desc SocialSyncPostCreatedAt, LimitTo limitVal]
      let mapped = map toDTO rows
      pure $ maybe mapped (\tag -> filter (hasTag tag) mapped) mTag
      where
        hasTag tag dto = T.toLower tag `elem` map T.toLower (sspdTags dto)

    withPool :: (MonadReader Env m, MonadIO m) => SqlPersistT IO a -> m a
    withPool action = do
      pool <- asks envPool
      liftIO $ runSqlPool action pool

parsePartyId :: MonadError ServerError m => Text -> m (Key Party)
parsePartyId raw =
  case readMaybeInt64 raw of
    Nothing  -> throwError err400 { errBody = "Invalid partyId" }
    Just val -> pure (toSqlKey val)

parseProfileId :: MonadError ServerError m => Text -> m (Key ArtistProfile)
parseProfileId raw =
  case readMaybeInt64 raw of
    Nothing  -> throwError err400 { errBody = "Invalid artistProfileId" }
    Just val -> pure (toSqlKey val)

readMaybeInt64 :: Text -> Maybe Int64
readMaybeInt64 txt =
  case reads (T.unpack (T.strip txt)) of
    [(n, "")] -> Just n
    _         -> Nothing

normalizePlatform :: Text -> Text
normalizePlatform = T.toLower . T.strip

nonEmptyText :: Text -> Maybe Text
nonEmptyText t =
  let trimmed = T.strip t
  in if T.null trimmed then Nothing else Just trimmed

classifyTags :: Maybe Text -> [Text]
classifyTags mCaption =
  let base = T.toLower (fromMaybe "" mCaption)
      matches xs = any (`T.isInfixOf` base) xs
      tags = catMaybes
        [ if matches ["show", "gig", "live", "tour", "set time", "festival"] then Just "show" else Nothing
        , if matches ["release", "single", "album", "ep", "out now", "streaming"] then Just "release" else Nothing
        , if matches ["merch", "shirt", "hoodie", "drop", "store", "shop"] then Just "merch" else Nothing
        , if matches ["press", "interview", "review", "feature", "coverage"] then Just "press" else Nothing
        ]
  in if null tags then ["general"] else nub tags

buildSummary :: Maybe Text -> Maybe Text
buildSummary Nothing = Nothing
buildSummary (Just txt) =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just (T.take 180 trimmed)

clampLimit :: Int -> Int
clampLimit n
  | n < 1 = 1
  | n > 500 = 500
  | otherwise = n

setMaybe :: PersistField a => EntityField SocialSyncPost (Maybe a) -> Maybe a -> [Update SocialSyncPost]
setMaybe _ Nothing = []
setMaybe field (Just v) = [field =. Just v]

toDTO :: Entity SocialSyncPost -> SocialSyncPostDTO
toDTO (Entity key SocialSyncPost{..}) =
  let mediaList = maybe [] (filter (not . T.null) . map T.strip . T.splitOn "\n") socialSyncPostMediaUrls
      tagList = maybe [] (filter (not . T.null) . map T.strip . T.splitOn ",") socialSyncPostTags
      metrics = SocialSyncMetricsDTO
        { ssmLikes = socialSyncPostLikeCount
        , ssmComments = socialSyncPostCommentCount
        , ssmShares = socialSyncPostShareCount
        , ssmViews = socialSyncPostViewCount
        }
  in SocialSyncPostDTO
    { sspdId = toPathPiece key
    , sspdPlatform = socialSyncPostPlatform
    , sspdExternalPostId = socialSyncPostExternalPostId
    , sspdArtistPartyId = fmap toPathPiece socialSyncPostArtistPartyId
    , sspdArtistProfileId = fmap toPathPiece socialSyncPostArtistProfileId
    , sspdCaption = socialSyncPostCaption
    , sspdPermalink = socialSyncPostPermalink
    , sspdMediaUrls = mediaList
    , sspdPostedAt = socialSyncPostPostedAt
    , sspdFetchedAt = socialSyncPostFetchedAt
    , sspdSummary = socialSyncPostSummary
    , sspdTags = if null tagList then ["general"] else tagList
    , sspdIngestSource = socialSyncPostIngestSource
    , sspdMetrics = metrics
    }
