{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.SocialSync
  ( socialSyncServer
  , validateSocialSyncArtistPartyId
  , validateSocialSyncArtistProfileId
  , validateSocialSyncPlatform
  , validateSocialSyncExternalPostId
  , validateSocialSyncIngestSource
  , validateSocialSyncPermalink
  , validateSocialSyncMediaUrls
  , validateSocialSyncPostsLimit
  ) where

import           Control.Monad              (forM)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (isAsciiLower, isAsciiUpper, isControl, isDigit, isSpace)
import           Data.Int                   (Int64)
import           Data.List                  (nub)
import           Data.Maybe                 (catMaybes, fromMaybe)
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql       (SqlPersistT, runSqlPool, toSqlKey)
import           Servant
import           Web.PathPieces             (toPathPiece)

import           TDF.API.SocialSyncAPI
import           TDF.Auth                   (AuthedUser, hasSocialSyncAccess)
import           TDF.DB                     (Env(..))
import           TDF.DTO.SocialSyncDTO
import           TDF.Models
import qualified TDF.Trials.Server          as TrialsServer (isValidHttpUrl)

maxSocialSyncMediaUrls :: Int
maxSocialSyncMediaUrls = 20

maxSocialSyncUrlChars :: Int
maxSocialSyncUrlChars = 2048

data ValidatedSocialSyncPost = ValidatedSocialSyncPost
  { vsspPayload          :: SocialSyncPostIn
  , vsspPlatform         :: Text
  , vsspExternalPostId   :: Text
  , vsspIngestSource     :: Text
  , vsspPermalink        :: Maybe Text
  , vsspMediaUrls        :: Maybe Text
  , vsspArtistPartyId    :: Maybe (Key Party)
  , vsspArtistProfileId  :: Maybe (Key ArtistProfile)
  }

socialSyncServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT SocialSyncAPI m
socialSyncServer user =
       ingestHandler
  :<|> listHandler
  where
    ensureSocialSyncAccess
      :: (MonadError ServerError m)
      => m ()
    ensureSocialSyncAccess =
      if hasSocialSyncAccess user
        then pure ()
        else throwError err403 { errBody = BL.fromStrict (TE.encodeUtf8 "Admin role required") }

    ingestHandler
      :: ( MonadReader Env m
         , MonadIO m
         , MonadError ServerError m
         )
      => SocialSyncIngestRequest
      -> m SocialSyncIngestResponse
    ingestHandler SocialSyncIngestRequest{..} = do
      ensureSocialSyncAccess
      now <- liftIO getCurrentTime
      validatedPosts <- either throwError pure (traverse validateSocialSyncPostPayload ssirPosts)
      resolvedPosts <- traverse resolveSocialSyncArtistReferences validatedPosts
      results <- forM resolvedPosts $ \ValidatedSocialSyncPost{..} -> do
        let tagList = classifyTags (sspCaption payload)
            summaryTxt = buildSummary (sspCaption payload)
            tagsText = nonEmptyText (T.intercalate "," tagList)
            payload = vsspPayload
        existing <- withPool $ getBy (UniqueSocialSyncPost vsspPlatform vsspExternalPostId)
        case existing of
          Just (Entity key _) -> do
            let updates = concat
                  [ setMaybe SocialSyncPostCaption (sspCaption payload)
                  , setMaybe SocialSyncPostPermalink vsspPermalink
                  , setMaybe SocialSyncPostMediaUrls vsspMediaUrls
                  , setMaybe SocialSyncPostPostedAt (sspPostedAt payload)
                  , setMaybe SocialSyncPostTags tagsText
                  , setMaybe SocialSyncPostSummary summaryTxt
                  , setMaybe SocialSyncPostArtistPartyId vsspArtistPartyId
                  , setMaybe SocialSyncPostArtistProfileId vsspArtistProfileId
                  , [SocialSyncPostIngestSource =. vsspIngestSource]
                  , setMaybe SocialSyncPostLikeCount (sspLikeCount payload)
                  , setMaybe SocialSyncPostCommentCount (sspCommentCount payload)
                  , setMaybe SocialSyncPostShareCount (sspShareCount payload)
                  , setMaybe SocialSyncPostViewCount (sspViewCount payload)
                  , [ SocialSyncPostUpdatedAt =. now
                    , SocialSyncPostFetchedAt =. now
                    ]
                  ]
            withPool $ update key updates
            pure (False, vsspPlatform, vsspIngestSource)
          Nothing -> do
            let record = SocialSyncPost
                  { socialSyncPostAccountId = Nothing
                  , socialSyncPostPlatform = vsspPlatform
                  , socialSyncPostExternalPostId = vsspExternalPostId
                  , socialSyncPostArtistPartyId = vsspArtistPartyId
                  , socialSyncPostArtistProfileId = vsspArtistProfileId
                  , socialSyncPostCaption = sspCaption payload
                  , socialSyncPostPermalink = vsspPermalink
                  , socialSyncPostMediaUrls = vsspMediaUrls
                  , socialSyncPostPostedAt = sspPostedAt payload
                  , socialSyncPostFetchedAt = now
                  , socialSyncPostTags = tagsText
                  , socialSyncPostSummary = summaryTxt
                  , socialSyncPostIngestSource = vsspIngestSource
                  , socialSyncPostLikeCount = sspLikeCount payload
                  , socialSyncPostCommentCount = sspCommentCount payload
                  , socialSyncPostShareCount = sspShareCount payload
                  , socialSyncPostViewCount = sspViewCount payload
                  , socialSyncPostCreatedAt = now
                  , socialSyncPostUpdatedAt = now
                  }
            withPool $ insert_ record
            pure (True, vsspPlatform, vsspIngestSource)
      let inserted = length (filter (\(wasInserted, _, _) -> wasInserted) results)
          updated = length results - inserted
      let platformLabel =
            resolveSocialSyncRunLabel "mixed" [platform | (_, platform, _) <- results]
      let runSource =
            resolveSocialSyncRunLabel "manual" [source | (_, _, source) <- results]
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
      ensureSocialSyncAccess
      platformFilter <- traverse (either throwError pure . validateSocialSyncPlatform) mPlatform
      partyKey <- traverse parsePartyId mParty
      profileKey <- traverse parseProfileId mProfile
      limitVal <- either throwError pure (validateSocialSyncPostsLimit mLimit)
      let normalizedTag = normalizeSocialSyncTagFilter mTag
      let filters = catMaybes
            [ (SocialSyncPostPlatform ==.) <$> platformFilter
            , (SocialSyncPostArtistPartyId ==.) . Just <$> partyKey
            , (SocialSyncPostArtistProfileId ==.) . Just <$> profileKey
            ]
      let selectOpts =
            [Desc SocialSyncPostPostedAt, Desc SocialSyncPostCreatedAt]
              <> maybe [LimitTo limitVal] (const []) normalizedTag
      rows <- withPool $ selectList filters selectOpts
      let visibleRows = filterSocialSyncRows normalizedTag limitVal rows
      pure (map toDTO visibleRows)

    withPool :: (MonadReader Env m, MonadIO m) => SqlPersistT IO a -> m a
    withPool action = do
      pool <- asks envPool
      liftIO $ runSqlPool action pool

    resolveSocialSyncArtistReferences
      :: ( MonadReader Env m
         , MonadIO m
         , MonadError ServerError m
         )
      => ValidatedSocialSyncPost
      -> m ValidatedSocialSyncPost
    resolveSocialSyncArtistReferences post@ValidatedSocialSyncPost{..} = do
      resolvedPartyId <- case vsspArtistPartyId of
        Nothing -> pure Nothing
        Just partyId -> do
          mParty <- withPool (get partyId)
          case mParty of
            Nothing ->
              throwError err404
                { errBody = BL.fromStrict (TE.encodeUtf8 "artistPartyId not found")
                }
            Just _ -> pure (Just partyId)
      case vsspArtistProfileId of
        Nothing ->
          pure post { vsspArtistPartyId = resolvedPartyId }
        Just profileId -> do
          mProfile <- withPool (get profileId)
          profile <- case mProfile of
            Nothing ->
              throwError err404
                { errBody = BL.fromStrict (TE.encodeUtf8 "artistProfileId not found")
                }
            Just value -> pure value
          let profilePartyId = artistProfileArtistPartyId profile
          case resolvedPartyId of
            Nothing ->
              pure post
                { vsspArtistPartyId = Just profilePartyId
                , vsspArtistProfileId = Just profileId
                }
            Just partyId
              | partyId == profilePartyId ->
                  pure post
                    { vsspArtistPartyId = Just partyId
                    , vsspArtistProfileId = Just profileId
                    }
              | otherwise ->
                  throwError err400
                    { errBody =
                        BL.fromStrict
                          (TE.encodeUtf8 "artistProfileId must belong to artistPartyId")
                    }

validateSocialSyncPostPayload :: SocialSyncPostIn -> Either ServerError ValidatedSocialSyncPost
validateSocialSyncPostPayload payload = do
  platform <- validateSocialSyncPlatform (sspPlatform payload)
  externalPostId <- validateSocialSyncExternalPostId (sspExternalPostId payload)
  ingestSrc <- validateSocialSyncIngestSource (sspIngestSource payload)
  permalink <- validateSocialSyncPermalink (sspPermalink payload)
  mediaUrls <- validateSocialSyncMediaUrls (sspMediaUrls payload)
  artistPartyId <- traverse validateSocialSyncArtistPartyKey (sspArtistPartyId payload)
  artistProfileId <- traverse validateSocialSyncArtistProfileKey (sspArtistProfileId payload)
  pure ValidatedSocialSyncPost
    { vsspPayload = payload
    , vsspPlatform = platform
    , vsspExternalPostId = externalPostId
    , vsspIngestSource = ingestSrc
    , vsspPermalink = permalink
    , vsspMediaUrls = mediaUrls
    , vsspArtistPartyId = artistPartyId
    , vsspArtistProfileId = artistProfileId
    }

validateSocialSyncArtistPartyKey :: Text -> Either ServerError (Key Party)
validateSocialSyncArtistPartyKey raw =
  toSqlKey <$> validateSocialSyncArtistPartyId raw

validateSocialSyncArtistProfileKey :: Text -> Either ServerError (Key ArtistProfile)
validateSocialSyncArtistProfileKey raw =
  toSqlKey <$> validateSocialSyncArtistProfileId raw

parsePartyId :: MonadError ServerError m => Text -> m (Key Party)
parsePartyId raw =
  case validateSocialSyncArtistPartyId raw of
    Left err  -> throwError err
    Right val -> pure (toSqlKey val)

parseProfileId :: MonadError ServerError m => Text -> m (Key ArtistProfile)
parseProfileId raw =
  case validateSocialSyncArtistProfileId raw of
    Left err  -> throwError err
    Right val -> pure (toSqlKey val)

validateSocialSyncArtistPartyId :: Text -> Either ServerError Int64
validateSocialSyncArtistPartyId =
  validatePositiveSocialSyncId "artistPartyId"

validateSocialSyncArtistProfileId :: Text -> Either ServerError Int64
validateSocialSyncArtistProfileId =
  validatePositiveSocialSyncId "artistProfileId"

validateSocialSyncPlatform :: Text -> Either ServerError Text
validateSocialSyncPlatform raw =
  case normalizePlatform raw of
    "instagram" -> Right "instagram"
    "facebook" -> Right "facebook"
    _ ->
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "platform must be one of: instagram, facebook")
        }

validateSocialSyncExternalPostId :: Text -> Either ServerError Text
validateSocialSyncExternalPostId raw =
  let trimmed = T.strip raw
  in if T.null trimmed
       then Left err400
         { errBody = BL.fromStrict (TE.encodeUtf8 "externalPostId is required")
         }
       else if T.length trimmed > 256
         then Left err400
           { errBody = BL.fromStrict (TE.encodeUtf8 "externalPostId must be 256 characters or fewer")
           }
       else if T.any isSpace trimmed
         then Left err400
           { errBody = BL.fromStrict (TE.encodeUtf8 "externalPostId must not contain whitespace")
           }
       else if T.any isControl trimmed
         then Left err400
           { errBody = BL.fromStrict (TE.encodeUtf8 "externalPostId must not contain control characters")
           }
       else Right trimmed

validateSocialSyncIngestSource :: Maybe Text -> Either ServerError Text
validateSocialSyncIngestSource Nothing = Right "manual"
validateSocialSyncIngestSource (Just raw) =
  case nonEmptyText raw of
    Nothing ->
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 "ingestSource must be omitted or a non-empty ASCII label")
        }
    Just source
      | T.length source > 64 ->
          Left err400
            { errBody = BL.fromStrict (TE.encodeUtf8 "ingestSource must be 64 characters or fewer")
            }
      | T.all isSocialSyncIngestSourceChar source ->
          Right (T.toLower source)
      | otherwise ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 $
                     "ingestSource must contain only ASCII letters, digits, "
                       <> "hyphen, or underscore")
            }
  where
    isSocialSyncIngestSourceChar c =
      isDigit c || isAsciiLower c || isAsciiUpper c || c == '-' || c == '_'

validateSocialSyncPermalink :: Maybe Text -> Either ServerError (Maybe Text)
validateSocialSyncPermalink Nothing = Right Nothing
validateSocialSyncPermalink (Just rawUrl) =
  case nonEmptyText rawUrl of
    Nothing -> Right Nothing
    Just url
      | T.any isSpace url ->
          Left err400
            { errBody = BL.fromStrict (TE.encodeUtf8 "permalink must not contain whitespace")
            }
      | T.length url > maxSocialSyncUrlChars ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 "permalink must be 2048 characters or fewer")
            }
      | not (TrialsServer.isValidHttpUrl url) ->
          Left err400
            { errBody =
                BL.fromStrict (TE.encodeUtf8 "permalink must be an absolute public http(s) URL")
            }
      | otherwise -> Right (Just url)

validateSocialSyncMediaUrls :: Maybe [Text] -> Either ServerError (Maybe Text)
validateSocialSyncMediaUrls Nothing = Right Nothing
validateSocialSyncMediaUrls (Just rawUrls)
  | length mediaUrls > maxSocialSyncMediaUrls =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "mediaUrls must contain at most 20 entries")
        }
  | any T.null mediaUrls =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "mediaUrls entries must not be blank")
        }
  | any (T.any isSpace) mediaUrls =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "mediaUrls entries must not contain whitespace")
        }
  | any ((> maxSocialSyncUrlChars) . T.length) mediaUrls =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "mediaUrls entries must be 2048 characters or fewer")
        }
  | any (not . TrialsServer.isValidHttpUrl) mediaUrls =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "mediaUrls entries must be absolute public http(s) URLs")
        }
  | length mediaUrls /= length (nub mediaUrls) =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "mediaUrls entries must be unique")
        }
  | otherwise =
      Right (nonEmptyText (T.intercalate "\n" mediaUrls))
  where
    mediaUrls = map T.strip rawUrls

validateSocialSyncPostsLimit :: Maybe Int -> Either ServerError Int
validateSocialSyncPostsLimit Nothing = Right 50
validateSocialSyncPostsLimit (Just n)
  | n >= 1 && n <= 500 = Right n
  | otherwise =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "limit must be between 1 and 500")
        }

validatePositiveSocialSyncId :: Text -> Text -> Either ServerError Int64
validatePositiveSocialSyncId fieldName raw =
  case readMaybeInt64 raw of
    Just val | val > 0 -> Right val
    _ ->
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a positive integer"))
        }

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

resolveSocialSyncRunLabel :: Text -> [Text] -> Text
resolveSocialSyncRunLabel fallback rawValues =
  case nub (catMaybes (map nonEmptyText rawValues)) of
    [] -> fallback
    [label] -> label
    _ -> "mixed"

normalizeSocialSyncTagFilter :: Maybe Text -> Maybe Text
normalizeSocialSyncTagFilter mTag = T.toLower <$> (mTag >>= nonEmptyText)

filterSocialSyncRows :: Maybe Text -> Int -> [Entity SocialSyncPost] -> [Entity SocialSyncPost]
filterSocialSyncRows Nothing _ rows = rows
filterSocialSyncRows (Just tag) limitVal rows =
  take limitVal (filter (socialSyncPostMatchesTag tag . entityVal) rows)

socialSyncPostMatchesTag :: Text -> SocialSyncPost -> Bool
socialSyncPostMatchesTag tag SocialSyncPost{..} =
  tag `elem` map T.toLower (parseSocialSyncTags socialSyncPostTags)

parseSocialSyncTags :: Maybe Text -> [Text]
parseSocialSyncTags rawTags =
  let tagList = maybe [] (filter (not . T.null) . map T.strip . T.splitOn ",") rawTags
  in if null tagList then ["general"] else tagList

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

setMaybe :: PersistField a => EntityField SocialSyncPost (Maybe a) -> Maybe a -> [Update SocialSyncPost]
setMaybe _ Nothing = []
setMaybe field (Just v) = [field =. Just v]

toDTO :: Entity SocialSyncPost -> SocialSyncPostDTO
toDTO (Entity key SocialSyncPost{..}) =
  let mediaList = maybe [] (filter (not . T.null) . map T.strip . T.splitOn "\n") socialSyncPostMediaUrls
      tagList = parseSocialSyncTags socialSyncPostTags
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
    , sspdTags = tagList
    , sspdIngestSource = socialSyncPostIngestSource
    , sspdMetrics = metrics
    }
