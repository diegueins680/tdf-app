{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.SocialDiscovery
  ( socialDiscoveryServer
  , detectEventTerms
  ) where

import           Control.Monad.Except (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, asks)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql (runSqlPool, toSqlKey)
import           Servant
import           Web.PathPieces (toPathPiece)

import           TDF.API.SocialDiscoveryAPI
import           TDF.Auth (AuthedUser(..), hasSocialSyncAccess)
import           TDF.DB (Env(..))
import           TDF.DTO.SocialDiscoveryDTO
import           TDF.Models

maxDiscoveryPosts :: Int
maxDiscoveryPosts = 100

candidateFetchLimit :: Int
candidateFetchLimit = 500

socialDiscoveryServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT SocialDiscoveryAPI m
socialDiscoveryServer user = listHandler :<|> reviewHandler
  where
    ensureAccess =
      if hasSocialSyncAccess user
        then pure ()
        else throwError err403 { errBody = "Admin role required" }

    listHandler mStatus mLimit = do
      ensureAccess
      statusFilter <- validateStatusFilter mStatus
      limit <- validateLimit mLimit
      rows <- withPool $ selectList [SocialSyncPostPlatform ==. "instagram"]
        [Desc SocialSyncPostPostedAt, Desc SocialSyncPostFetchedAt, LimitTo candidateFetchLimit]
      candidates <- traverse toDiscoveryDTO rows
      pure $ take limit $ filter (matchesStatus statusFilter) candidates

    reviewHandler postIdText SocialDiscoveryReviewIn{..} = do
      ensureAccess
      postId <- parsePostId postIdText
      post <- withPool $ getEntity postId
      case post of
        Nothing -> throwError err404 { errBody = "Discovered post not found" }
        Just entity
          | socialSyncPostPlatform (entityVal entity) /= "instagram" ->
              throwError err404 { errBody = "Discovered post not found" }
          | null (detectEventTerms (socialSyncPostCaption (entityVal entity))) ->
              throwError err409 { errBody = "Post is not an event candidate" }
          | otherwise -> do
              now <- liftIO getCurrentTime
              let reviewed = sdriStatus /= "pending"
                  review = SocialDiscoveryReview
                    { socialDiscoveryReviewSocialSyncPostId = postId
                    , socialDiscoveryReviewStatus = sdriStatus
                    , socialDiscoveryReviewReviewNotes = sdriNotes
                    , socialDiscoveryReviewReviewedByPartyId = if reviewed then Just (auPartyId user) else Nothing
                    , socialDiscoveryReviewReviewedAt = if reviewed then Just now else Nothing
                    , socialDiscoveryReviewCreatedAt = now
                    , socialDiscoveryReviewUpdatedAt = now
                    }
              existing <- withPool $ getBy (UniqueSocialDiscoveryReview postId)
              case existing of
                Nothing -> voidWithPool (insert_ review)
                Just (Entity reviewId _) -> voidWithPool $ update reviewId
                  [ SocialDiscoveryReviewStatus =. sdriStatus
                  , SocialDiscoveryReviewReviewNotes =. sdriNotes
                  , SocialDiscoveryReviewReviewedByPartyId =. if reviewed then Just (auPartyId user) else Nothing
                  , SocialDiscoveryReviewReviewedAt =. if reviewed then Just now else Nothing
                  , SocialDiscoveryReviewUpdatedAt =. now
                  ]
              toDiscoveryDTO entity

    toDiscoveryDTO (Entity postId post) = do
      sourceHandle <- case socialSyncPostAccountId post of
        Nothing -> pure Nothing
        Just accountId -> fmap socialSyncAccountHandle <$> withPool (get accountId)
      review <- withPool $ getBy (UniqueSocialDiscoveryReview postId)
      let reviewValue = entityVal <$> review
      pure SocialDiscoveryPostDTO
        { sddId = toPathPiece postId
        , sddPlatform = socialSyncPostPlatform post
        , sddSourceHandle = sourceHandle >>= (>>= nonBlank)
        , sddCaption = socialSyncPostCaption post
        , sddPermalink = socialSyncPostPermalink post
        , sddMediaUrls = maybe [] T.lines (socialSyncPostMediaUrls post)
        , sddPostedAt = socialSyncPostPostedAt post
        , sddFetchedAt = socialSyncPostFetchedAt post
        , sddDetectedTerms = detectEventTerms (socialSyncPostCaption post)
        , sddReviewStatus = maybe "pending" socialDiscoveryReviewStatus reviewValue
        , sddReviewNotes = reviewValue >>= socialDiscoveryReviewReviewNotes
        , sddReviewedAt = reviewValue >>= socialDiscoveryReviewReviewedAt
        }

    nonBlank raw = let trimmed = T.strip raw in if T.null trimmed then Nothing else Just trimmed

    matchesStatus Nothing _ = True
    matchesStatus (Just status) item = sddReviewStatus item == status

    validateStatusFilter Nothing = pure Nothing
    validateStatusFilter (Just raw) =
      let status = T.toLower (T.strip raw)
      in if status `elem` ["pending", "approved", "dismissed"]
           then pure (Just status)
           else throwError err400 { errBody = "status must be pending, approved, or dismissed" }

    validateLimit Nothing = pure 50
    validateLimit (Just value)
      | value < 1 || value > maxDiscoveryPosts =
          throwError err400 { errBody = "limit must be between 1 and 100" }
      | otherwise = pure value

    parsePostId raw =
      case reads (T.unpack raw) of
        [(number, "")] | number > (0 :: Int64) -> pure (toSqlKey number)
        _ -> throwError err400 { errBody = "postId must be a positive integer" }

    withPool action = do
      pool <- asks envPool
      liftIO $ runSqlPool action pool

    voidWithPool action = withPool action >> pure ()

detectEventTerms :: Maybe Text -> [Text]
detectEventTerms caption =
  let haystack = T.toCaseFold (fromMaybe "" caption)
      candidates =
        [ "concierto", "en vivo", "evento", "festival", "presentación", "presentacion"
        , "tocamos", "show", "gig", "live", "tickets", "entradas", "boleto", "boletos"
        ]
  in filter (`T.isInfixOf` haystack) candidates
