{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerFanClub
  ( fanClubPublicGetClub
  , fanClubPublicGetEvents
  , fanClubSecureListMyClubs
  , fanClubSecureArtistHandlers
  , validateFanClubArtistPathId
  , validateFanClubPostPathId
  , validateFanClubPostAccess
  , validateFanClubPostMutationTarget
  , validateFanClubReplyParentTarget
  , validateFanClubElectionPathId
  , validateFanClubElectionMutationTarget
  , validateFanClubCandidacyPathId
  , validateFanClubVoteSelectionIds
  , validateFanClubVoteCandidacyTarget
  , validateFanClubVoteCandidacyTargets
  , validateFanClubOfficerRoleInput
  , validateFanClubInboxSubjectInput
  , validateFanClubInboxBodyInput
  , validateFanClubInboxReplyBodyInput
  , validateFanClubInboxStatusInput
  , validateFanClubPostTitleInput
  , validateFanClubPostContentInput
  , validateFanClubMediaUrlsInput
  , validateFanClubMemoryTitleInput
  , validateFanClubMemoryDescriptionInput
  , validateFanClubMemoryPathId
  , validateFanClubMemoryMutationTarget
  , validateFanClubMemoryReportReason
  , validateFanClubEventTitleInput
  , validateFanClubEventDescriptionInput
  , validateFanClubEventLocationInput
  , validateFanClubEventTimeRange
  ) where

import           Control.Arrow          ((&&&))
import           Control.Monad          (forM, forM_, when, unless, void)
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask)
import           Data.Char              (GeneralCategory (Format, LineSeparator, ParagraphSeparator)
                                         , generalCategory, isControl, isSpace)
import           Data.Int               (Int64)
import           Data.List              (nub, sortOn, partition)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (isJust, mapMaybe, catMaybes, fromMaybe, listToMaybe)
import           Data.Ord               (Down(..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString.Lazy   as BL
import           Data.Time              (UTCTime, getCurrentTime)

import           Database.Persist       (Entity(..), Key, (=.), (==.), (<-.), SelectOpt(Asc, Desc, LimitTo)
                                         , get, getBy, insert, insertUnique, selectFirst, selectList, update, count
                                         , delete, deleteWhere, upsert, insert_)
import           Database.Persist.Sql   (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey, rawSql, Single(..))
import           Servant                (NoContent(..), ServerError, (:<|>)(..), err400, err403, err404, errBody)

import           Control.Monad.Reader   (ReaderT, ask)
import           Servant                (Handler, throwError)
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString.Lazy   as BL

import           TDF.Auth               (AuthedUser(..))
import           TDF.DB                 (Env(..))
import           TDF.DTO
import qualified TDF.DTO                as DTO
import           TDF.Models             (FanClub(..), FanClubId, FanClubOfficer(..), FanClubElection(..)
                                         , FanClubCandidacy(..), FanClubVote(..), FanClubPost(..), FanClubPostId
                                         , FanClubEvent(..), FanClubOfficerRole(..), ElectionStatus(..)
                                         , Party(..), PartyId, FanFollow(..), FanProfile(..)
                                         , FanClubMemberProfile(..), FanClubMemory(..), FanClubMemoryReport(..)
                                         , FanClubInboxMessage(..), FanClubInboxMessageId
                                         , ContentReaction(..)
                                         , Notification(..), BoostedContent(..), CreatorBadge(..)
                                         , RoleEnum(..)
                                         , Unique(..), FanClubElectionId, FanClubCandidacyId, artistProfileHeroImageUrl)
import qualified TDF.Models             as M
import           Data.Time.Clock        (diffUTCTime)

type AppM = ReaderT Env Handler

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  Env{..} <- ask
  liftIO $ runSqlPool action envPool

throwBadRequest :: Text -> AppM a
throwBadRequest msg = throwError Servant.err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

badRequestText :: Text -> ServerError
badRequestText msg =
  err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

-- ============================================================================
-- Public handlers
-- ============================================================================

fanClubPublicGetClub :: Int64 -> AppM FanClubDTO
fanClubPublicGetClub artistId = do
  artistKey <- either throwError pure (validateFanClubArtistPathId artistId)
  mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
  case mClub of
    Nothing -> throwError err404 { errBody = "Club de fans no encontrado" }
    Just (Entity cid club) -> runDB $ do
      officers <- loadOfficersDTO cid
      followerCount <- count [M.FanFollowArtistPartyId ==. artistKey]
      mArtistProfile <- getBy (UniqueArtistProfile artistKey)
      let artistImage = mArtistProfile >>= artistProfileHeroImageUrl . entityVal
      pure FanClubDTO
        { fcId = fromSqlKey cid
        , fcArtistId = fromSqlKey artistKey
        , fcName = fanClubName club
        , fcDescription = fanClubDescription club
        , fcOfficers = officers
        , fcFollowerCount = fromIntegral followerCount
        , fcArtistImageUrl = artistImage
        }

fanClubPublicGetEvents :: Int64 -> AppM [FanClubEventDTO]
fanClubPublicGetEvents artistId = do
  artistKey <- either throwError pure (validateFanClubArtistPathId artistId)
  runDB $ do
    mClub <- getBy (UniqueFanClubArtist artistKey)
    case mClub of
      Nothing -> pure []
      Just (Entity cid _) -> do
        events <- selectList [M.FanClubEventClubId ==. cid] [Asc M.FanClubEventStartsAt]
        pure (map eventToDTO events)

-- ============================================================================
-- Secure handlers
-- ============================================================================

fanClubSecureListMyClubs :: AuthedUser -> AppM [FanClubDTO]
fanClubSecureListMyClubs user = runDB $ do
  follows <- selectList [M.FanFollowFanPartyId ==. auPartyId user] [Desc M.FanFollowCreatedAt]
  clubs <- forM follows $ \(Entity _ follow) -> do
    let artistKey = fanFollowArtistPartyId follow
    mClub <- getBy (UniqueFanClubArtist artistKey)
    case mClub of
      Nothing -> pure Nothing
      Just (Entity cid club) -> do
        officers <- loadOfficersDTO cid
        followerCount <- count [M.FanFollowArtistPartyId ==. artistKey]
        mArtistProfile <- getBy (UniqueArtistProfile artistKey)
        let artistImage = mArtistProfile >>= artistProfileHeroImageUrl . entityVal
        pure $ Just FanClubDTO
          { fcId = fromSqlKey cid
          , fcArtistId = fromSqlKey artistKey
          , fcName = fanClubName club
          , fcDescription = fanClubDescription club
          , fcOfficers = officers
          , fcFollowerCount = fromIntegral followerCount
          , fcArtistImageUrl = artistImage
          }
  pure (mapMaybe id clubs)

fanClubSecureArtistHandlers :: AuthedUser -> Int64 ->
  ( AppM FanClubDTO
  :<|> (Maybe Text -> Maybe Text -> AppM [FanClubFeedItemDTO])
  :<|> AppM [FanClubPostDTO]
  :<|> (FanClubCreatePostReq -> AppM FanClubPostDTO)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> ContentReactionReq -> AppM ReactionSummaryDTO)
  :<|> (Int64 -> ContentReactionReq -> AppM ReactionSummaryDTO)
  :<|> (Maybe Text -> AppM [LeaderboardEntryDTO])
  :<|> AppM (Maybe FanClubFeedItemDTO)
  :<|> AppM [FanClubEventDTO]
  :<|> (FanClubCreateEventReq -> AppM FanClubEventDTO)
  :<|> AppM [FanClubElectionDTO]
  :<|> (FanClubCreateElectionReq -> AppM FanClubElectionDTO)
  :<|> (Int64 -> FanClubCreateCandidacyReq -> AppM FanClubCandidacyDTO)
  :<|> (Int64 -> FanClubVoteReq -> AppM NoContent)
  :<|> AppM [FanClubMemoryDTO]
  :<|> (FanClubCreateMemoryReq -> AppM FanClubMemoryDTO)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> FanClubMemoryReportReq -> AppM FanClubMemoryReportDTO)
  :<|> AppM [FanClubMemberProfileDTO]
  :<|> AppM FanClubMemberProfileDTO
  :<|> (FanClubMemberProfileUpdate -> AppM FanClubMemberProfileDTO)
  :<|> AppM [FanClubInboxMessageDTO]
  :<|> (FanClubInboxSendReq -> AppM FanClubInboxMessageDTO)
  :<|> (Int64 -> AppM FanClubInboxMessageDTO)
  :<|> (Int64 -> FanClubInboxReplyReq -> AppM FanClubInboxMessageDTO)
  :<|> (Int64 -> FanClubInboxStatusReq -> AppM FanClubInboxMessageDTO)
  )
fanClubSecureArtistHandlers user artistId =
       getClubDetail artistId
  :<|> listClubFeed artistId
  :<|> listClubPosts artistId
  :<|> createClubPost artistId
  :<|> pinPost artistId
  :<|> unpinPost artistId
  :<|> hidePost artistId
  :<|> unhidePost artistId
  :<|> reactToPost artistId
  :<|> reactToMemory artistId
  :<|> getLeaderboard artistId
  :<|> getSpotlight artistId
  :<|> listClubEvents artistId
  :<|> createClubEvent artistId
  :<|> listClubElections artistId
  :<|> createClubElection artistId
  :<|> createCandidacy artistId
  :<|> castVote artistId
  :<|> listClubMemories artistId
  :<|> createClubMemory artistId
  :<|> hideMemory artistId
  :<|> unhideMemory artistId
  :<|> deleteMemory artistId
  :<|> reportMemory artistId
  :<|> listMemberProfiles artistId
  :<|> getMyMemberProfile artistId
  :<|> updateMyMemberProfile artistId
  :<|> listInboxMessages artistId
  :<|> sendInboxMessage artistId
  :<|> getInboxMessage artistId
  :<|> replyInboxMessage artistId
  :<|> updateInboxStatus artistId
  where
    getClubDetail aId = do
      artistKey <- requireArtistKey aId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club de fans no encontrado" }
        Just (Entity cid club) -> runDB $ do
          officers <- loadOfficersDTO cid
          followerCount <- count [M.FanFollowArtistPartyId ==. artistKey]
          mArtistProfile <- getBy (UniqueArtistProfile artistKey)
          let artistImage = mArtistProfile >>= artistProfileHeroImageUrl . entityVal
          pure FanClubDTO
            { fcId = fromSqlKey cid
            , fcArtistId = fromSqlKey artistKey
            , fcName = fanClubName club
            , fcDescription = fanClubDescription club
            , fcOfficers = officers
            , fcFollowerCount = fromIntegral followerCount
            , fcArtistImageUrl = artistImage
            }

    listClubFeed aId mSort _mPeriod = do
      artistKey <- requireArtistKey aId
      now <- liftIO getCurrentTime
      runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure []
          Just (Entity cid _) -> do
            officers <- selectList [M.FanClubOfficerClubId ==. cid] []
            let officerIds = map (fromSqlKey . fanClubOfficerFanPartyId . entityVal) officers
            posts <- selectList
              [ M.FanClubPostClubId ==. cid
              , M.FanClubPostParentId ==. Nothing
              ] [Desc M.FanClubPostCreatedAt]
            memories <- selectList
              [ M.FanClubMemoryIsDeleted ==. False
              ] [Desc M.FanClubMemoryCreatedAt]
            let memoryMemberIds = map (fanClubMemoryMemberProfileId . entityVal) memories
            memberProfiles <- selectList [M.FanClubMemberProfileId <-. memoryMemberIds] []
            let memberProfileMap = foldl (\m (Entity mpid mp) -> Map.insert mpid mp m) Map.empty memberProfiles
            let validMemories = filter (\(Entity _ m) ->
                  case Map.lookup (fanClubMemoryMemberProfileId m) memberProfileMap of
                    Just mp -> fanClubMemberProfileClubId mp == cid
                    Nothing -> False
                  ) memories
            postItems <- forM posts $ \(Entity pid p) -> do
              author <- getAuthorDTO (fanClubPostFanPartyId p)
              let isOfficer = fromSqlKey (fanClubPostFanPartyId p) `elem` officerIds
              reactions <- buildReactionSummary "post" (fromIntegral (fromSqlKey pid)) (auPartyId user)
              pure FanClubFeedItemDTO
                { fcfId = fromSqlKey pid
                , fcfKind = "post"
                , fcfTitle = fanClubPostTitle p
                , fcfContent = fanClubPostContent p
                , fcfAuthorId = fromSqlKey (fanClubPostFanPartyId p)
                , fcfAuthorName = sppDisplayName author
                , fcfAvatarUrl = sppAvatarUrl author
                , fcfMediaUrls = maybe [] (T.splitOn ",") (fanClubPostMediaUrls p)
                , fcfIsPinned = fanClubPostIsPinned p
                , fcfIsOfficer = isOfficer
                , fcfIsHidden = fanClubPostIsHidden p
                , fcfReactions = reactions
                , fcfCreatedAt = fanClubPostCreatedAt p
                }
            memoryItems <- forM validMemories $ \(Entity mid m) -> do
              let mprofile = Map.lookup (fanClubMemoryMemberProfileId m) memberProfileMap
              case mprofile of
                Nothing -> pure Nothing
                Just mp -> do
                  author <- getAuthorDTO (fanClubMemberProfilePartyId mp)
                  let isOfficer = fromSqlKey (fanClubMemberProfilePartyId mp) `elem` officerIds
                  reactions <- buildReactionSummary "memory" (fromIntegral (fromSqlKey mid)) (auPartyId user)
                  pure $ Just FanClubFeedItemDTO
                    { fcfId = fromSqlKey mid
                    , fcfKind = "memory"
                    , fcfTitle = Just (fanClubMemoryTitle m)
                    , fcfContent = fromMaybe "" (fanClubMemoryDescription m)
                    , fcfAuthorId = fromSqlKey (fanClubMemberProfilePartyId mp)
                    , fcfAuthorName = sppDisplayName author
                    , fcfAvatarUrl = sppAvatarUrl author
                    , fcfMediaUrls = maybe [] (T.splitOn ",") (fanClubMemoryMediaUrls m)
                    , fcfIsPinned = False
                    , fcfIsOfficer = isOfficer
                    , fcfIsHidden = fanClubMemoryIsHidden m
                    , fcfReactions = reactions
                    , fcfCreatedAt = fanClubMemoryCreatedAt m
                    }
            let allItems = postItems ++ catMaybes memoryItems
            let sortMode = fromMaybe "new" mSort
            pure $ sortFeedItems sortMode now allItems

    listClubPosts aId = do
      artistKey <- requireArtistKey aId
      runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure []
          Just (Entity cid _) -> do
            posts <- selectList
              [ M.FanClubPostClubId ==. cid
              , M.FanClubPostParentId ==. Nothing
              ] [Desc M.FanClubPostIsPinned, Desc M.FanClubPostCreatedAt]
            forM posts $ \(Entity pid p) -> do
              replies <- count [M.FanClubPostParentId ==. Just pid]
              author <- getAuthorDTO (fanClubPostFanPartyId p)
              reactions <- buildReactionSummary "post" (fromIntegral (fromSqlKey pid)) (auPartyId user)
              pure $ postToDTO pid p (fromIntegral replies) author reactions

    createClubPost aId req = do
      artistKey <- requireArtistKey aId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          requireFanClubPostAccess user artistKey
          title <-
            either throwError pure $
              validateFanClubPostTitleInput (fcpReqTitle req)
          content <-
            either throwError pure $
              validateFanClubPostContentInput (fcpReqContent req)
          mediaUrlValues <-
            either throwError pure $
              validateFanClubMediaUrlsInput "mediaUrls" (fcpReqMediaUrls req)
          parentKey <- resolveParentKey cid (fcpReqParentId req)
          let mediaUrls =
                if null mediaUrlValues
                  then Nothing
                  else Just (T.intercalate "," mediaUrlValues)
          runDB $ do
            now <- liftIO getCurrentTime
            pid <- insert FanClubPost
              { fanClubPostClubId = cid
              , fanClubPostFanPartyId = auPartyId user
              , fanClubPostParentId = parentKey
              , fanClubPostTitle = title
              , fanClubPostContent = content
              , fanClubPostMediaUrls = mediaUrls
              , fanClubPostIsPinned = False
              , fanClubPostIsHidden = False
              , fanClubPostCreatedAt = now
              , fanClubPostUpdatedAt = Nothing
              }
            author <- getAuthorDTO (auPartyId user)
            let post =
                  FanClubPost
                    cid
                    (auPartyId user)
                    parentKey
                    title
                    content
                    mediaUrls
                    False
                    False
                    now
                    Nothing
            pure $ postToDTO pid post 0 author emptyReactionSummary

    resolveParentKey :: FanClubId -> Maybe Int64 -> AppM (Maybe FanClubPostId)
    resolveParentKey _ Nothing = pure Nothing
    resolveParentKey cid (Just rawParentId) = do
      parentId <- either throwError pure (validateFanClubPostPathId rawParentId)
      mParent <- runDB $ get parentId
      either throwError (pure . Just) $
        maybe
          (Left fanClubPostNotFound)
          (validateFanClubReplyParentTarget cid . Entity parentId)
          mParent

    pinPost aId postId = do
      postKey <- requirePostOfficerTarget aId postId
      runDB $ update postKey [M.FanClubPostIsPinned =. True]
      pure NoContent

    unpinPost aId postId = do
      postKey <- requirePostOfficerTarget aId postId
      runDB $ update postKey [M.FanClubPostIsPinned =. False]
      pure NoContent

    hidePost aId postId = do
      postKey <- requirePostOfficerTarget aId postId
      runDB $ update postKey [M.FanClubPostIsHidden =. True]
      pure NoContent

    unhidePost aId postId = do
      postKey <- requirePostOfficerTarget aId postId
      runDB $ update postKey [M.FanClubPostIsHidden =. False]
      pure NoContent

    requirePostOfficerTarget aId rawPostId = do
      artistKey <- requireArtistKey aId
      isOfficer <- runDB $ checkIsOfficer user artistKey
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      postKey <- either throwError pure (validateFanClubPostPathId rawPostId)
      target <- runDB $ lookupFanClubPostMutationTarget artistKey postKey
      either throwError pure target

    listClubEvents aId = do
      artistKey <- requireArtistKey aId
      runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure []
          Just (Entity cid _) -> do
            events <- selectList [M.FanClubEventClubId ==. cid] [Asc M.FanClubEventStartsAt]
            pure (map eventToDTO events)

    createClubEvent aId req = do
      artistKey <- requireArtistKey aId
      isOfficer <- runDB $ checkIsOfficer user artistKey
      unless isOfficer $ throwError err403 { errBody = "Solo la directiva puede crear eventos" }
      title <- either throwError pure $
        validateFanClubEventTitleInput (fcevTitle req)
      description <- either throwError pure $
        validateFanClubEventDescriptionInput (fcevDescription req)
      location <- either throwError pure $
        validateFanClubEventLocationInput (fcevLocation req)
      either throwError pure $
        validateFanClubEventTimeRange (fcevStartsAt req) (fcevEndsAt req)
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> runDB $ do
          now <- liftIO getCurrentTime
          eid <- insert FanClubEvent
            { fanClubEventClubId = cid
            , fanClubEventTitle = title
            , fanClubEventDescription = description
            , fanClubEventStartsAt = fcevStartsAt req
            , fanClubEventEndsAt = fcevEndsAt req
            , fanClubEventLocation = location
            , fanClubEventIsArtistConcert = False
            , fanClubEventCreatedByPartyId = Just (auPartyId user)
            , fanClubEventCreatedAt = now
            }
          pure $ FanClubEventDTO
            { fceId = fromSqlKey eid
            , fceTitle = title
            , fceDescription = description
            , fceStartsAt = fcevStartsAt req
            , fceEndsAt = fcevEndsAt req
            , fceLocation = location
            , fceIsArtistConcert = False
            , fceCreatedBy = Just (fromSqlKey (auPartyId user))
            }

    listClubElections aId = do
      artistKey <- requireArtistKey aId
      runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure []
          Just (Entity cid _) -> do
            elections <- selectList [M.FanClubElectionClubId ==. cid] [Desc M.FanClubElectionYear]
            forM elections $ \(Entity eid el) -> do
              allCands <- selectList [] []
              let myCands = filter (\(Entity _ c) -> fanClubCandidacyElectionId c == eid && fanClubCandidacyFanPartyId c == auPartyId user) allCands
              allVotes <- selectList [] []
              let myVotes = filter (\(Entity _ v) -> fanClubVoteElectionId v == eid && fanClubVoteFanPartyId v == auPartyId user) allVotes
              let cands = map candidacyToDTO myCands
              let votes = map voteToDTO myVotes
              pure $ FanClubElectionDTO
                { fceElectionId = fromSqlKey eid
                , fceYear = fanClubElectionYear el
                , fceStatus = T.pack (show (fanClubElectionStatus el))
                , fceCandidacyStartsAt = fanClubElectionCandidacyStartsAt el
                , fceCandidacyEndsAt = fanClubElectionCandidacyEndsAt el
                , fceVotingStartsAt = fanClubElectionVotingStartsAt el
                , fceVotingEndsAt = fanClubElectionVotingEndsAt el
                , fceMyCandidacies = cands
                , fceMyVotes = votes
                }

    createClubElection aId req = do
      artistKey <- requireArtistKey aId
      isOfficer <- runDB $ checkIsOfficer user artistKey
      unless isOfficer $ throwError err403 { errBody = "Solo la directiva puede crear elecciones" }
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          now <- liftIO getCurrentTime
          mEid <- runDB $ insertUnique FanClubElection
            { fanClubElectionClubId = cid
            , fanClubElectionYear = fcelYear req
            , fanClubElectionCandidacyStartsAt = fcelCandidacyStartsAt req
            , fanClubElectionCandidacyEndsAt = fcelCandidacyEndsAt req
            , fanClubElectionVotingStartsAt = fcelVotingStartsAt req
            , fanClubElectionVotingEndsAt = fcelVotingEndsAt req
            , fanClubElectionStatus = Upcoming
            , fanClubElectionCreatedAt = now
            }
          case mEid of
            Nothing -> throwError err400 { errBody = "Ya existe una elección para este año" }
            Just eid' -> do
              let el = FanClubElection cid (fcelYear req) (fcelCandidacyStartsAt req) (fcelCandidacyEndsAt req)
                              (fcelVotingStartsAt req) (fcelVotingEndsAt req) Upcoming now
              pure $ FanClubElectionDTO
                { fceElectionId = fromSqlKey eid'
                , fceYear = fanClubElectionYear el
                , fceStatus = T.pack (show (fanClubElectionStatus el))
                , fceCandidacyStartsAt = fanClubElectionCandidacyStartsAt el
                , fceCandidacyEndsAt = fanClubElectionCandidacyEndsAt el
                , fceVotingStartsAt = fanClubElectionVotingStartsAt el
                , fceVotingEndsAt = fanClubElectionVotingEndsAt el
                , fceMyCandidacies = []
                , fceMyVotes = []
                }

    createCandidacy aId electionId req = do
      artistKey <- requireArtistKey aId
      electionKey <- either throwError pure (validateFanClubElectionPathId electionId)
      targetElection <- runDB $ lookupFanClubElectionMutationTarget artistKey electionKey
      _ <- either throwError pure targetElection
      role <- either throwError pure (validateFanClubOfficerRoleInput (fccrRole req))
      mCid <- runDB $ do
        now <- liftIO getCurrentTime
        insertUnique FanClubCandidacy
          { fanClubCandidacyElectionId = electionKey
          , fanClubCandidacyFanPartyId = auPartyId user
          , fanClubCandidacyRole = role
          , fanClubCandidacyManifesto = fccrManifesto req
          , fanClubCandidacyCreatedAt = now
          }
      case mCid of
        Nothing -> throwError err400 { errBody = "Ya estás postulado para este cargo" }
        Just cid' -> runDB $ do
          author <- getAuthorDTO (auPartyId user)
          pure $ FanClubCandidacyDTO
            { fccCandidacyId = fromSqlKey cid'
            , fccFanId = fromSqlKey (auPartyId user)
            , fccFanName = sppDisplayName author
            , fccAvatarUrl = sppAvatarUrl author
            , fccRole = T.pack (show role)
            , fccManifesto = fccrManifesto req
            , fccVoteCount = 0
            }

    castVote aId electionId req = do
      artistKey <- requireArtistKey aId
      electionKey <- either throwError pure (validateFanClubElectionPathId electionId)
      targetElection <- runDB $ lookupFanClubElectionMutationTarget artistKey electionKey
      _ <- either throwError pure targetElection
      candidacyKeys <- either throwError pure $
        validateFanClubVoteSelectionIds (fcvCandidacyIds req)
      candidacies <- runDB $ traverse (lookupFanClubVoteCandidacyTarget electionKey) candidacyKeys
      resolvedCandidacies <- either throwError pure $
        sequence candidacies >>= validateFanClubVoteCandidacyTargets
      runDB $ do
        now <- liftIO getCurrentTime
        forM_ resolvedCandidacies $ \(cKey, cand) ->
          void $ insertUnique FanClubVote
            { fanClubVoteElectionId = electionKey
            , fanClubVoteFanPartyId = auPartyId user
            , fanClubVoteCandidacyId = cKey
            , fanClubVoteRole = fanClubCandidacyRole cand
            , fanClubVoteCreatedAt = now
            }
      pure NoContent

    listClubMemories aId = do
      artistKey <- requireArtistKey aId
      runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure []
          Just (Entity cid _) -> do
            memories <- selectList [M.FanClubMemoryIsDeleted ==. False] [Desc M.FanClubMemoryCreatedAt]
            let memberIds = map (fanClubMemoryMemberProfileId . entityVal) memories
            memberProfiles <- selectList [M.FanClubMemberProfileId <-. memberIds] []
            let profileMap = foldl (\m (Entity mpid mp) -> Map.insert mpid mp m) Map.empty memberProfiles
            let validMemories = filter (\(Entity _ mem) ->
                  case Map.lookup (fanClubMemoryMemberProfileId mem) profileMap of
                    Just mp -> fanClubMemberProfileClubId mp == cid
                    Nothing -> False
                  ) memories
            forM validMemories $ \(Entity mid m) -> do
              let Just mp = Map.lookup (fanClubMemoryMemberProfileId m) profileMap
              author <- getAuthorDTO (fanClubMemberProfilePartyId mp)
              reactions <- buildReactionSummary "memory" (fromIntegral (fromSqlKey mid)) (auPartyId user)
              pure FanClubMemoryDTO
                { fcmId = fromSqlKey mid
                , fcmMemberProfileId = fromSqlKey (fanClubMemoryMemberProfileId m)
                , fcmMemberName = sppDisplayName author
                , fcmMemberAvatarUrl = sppAvatarUrl author
                , fcmTitle = fanClubMemoryTitle m
                , fcmDescription = fanClubMemoryDescription m
                , fcmMediaUrls = maybe [] (T.splitOn ",") (fanClubMemoryMediaUrls m)
                , fcmIsHidden = fanClubMemoryIsHidden m
                , fcmIsDeleted = fanClubMemoryIsDeleted m
                , fcmReactions = reactions
                , fcmCreatedAt = fanClubMemoryCreatedAt m
                }

    createClubMemory aId req = do
      artistKey <- requireArtistKey aId
      title <- either throwError pure $
        validateFanClubMemoryTitleInput (fcmReqTitle req)
      description <- either throwError pure $
        validateFanClubMemoryDescriptionInput (fcmReqDescription req)
      mediaUrlValues <- either throwError pure $
        validateFanClubMediaUrlsInput "mediaUrls" (fcmReqMediaUrls req)
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> runDB $ do
          now <- liftIO getCurrentTime
          mProfile <- getBy (UniqueFanClubMemberProfile (auPartyId user) cid)
          profileId <- case mProfile of
            Just (Entity pid _) -> pure pid
            Nothing -> do
              mFanProfile <- getBy (UniqueFanProfile (auPartyId user))
              let displayName = case mFanProfile of
                    Just (Entity _ fp) -> fanProfileDisplayName fp
                    Nothing -> Nothing
              let avatarUrl = case mFanProfile of
                    Just (Entity _ fp) -> fanProfileAvatarUrl fp
                    Nothing -> Nothing
              insert FanClubMemberProfile
                { fanClubMemberProfilePartyId = auPartyId user
                , fanClubMemberProfileClubId = cid
                , fanClubMemberProfileHandle = Nothing
                , fanClubMemberProfileBio = Nothing
                , fanClubMemberProfileAvatarUrl = avatarUrl
                , fanClubMemberProfileJoinedAt = now
                }
          mid <- insert FanClubMemory
            { fanClubMemoryMemberProfileId = profileId
            , fanClubMemoryTitle = title
            , fanClubMemoryDescription = description
            , fanClubMemoryMediaUrls =
                if null mediaUrlValues
                  then Nothing
                  else Just (T.intercalate "," mediaUrlValues)
            , fanClubMemoryIsHidden = False
            , fanClubMemoryIsDeleted = False
            , fanClubMemoryCreatedAt = now
            }
          author <- getAuthorDTO (auPartyId user)
          pure FanClubMemoryDTO
            { fcmId = fromSqlKey mid
            , fcmMemberProfileId = fromSqlKey profileId
            , fcmMemberName = sppDisplayName author
            , fcmMemberAvatarUrl = sppAvatarUrl author
            , fcmTitle = title
            , fcmDescription = description
            , fcmMediaUrls = fcmReqMediaUrls req
            , fcmIsHidden = False
            , fcmIsDeleted = False
            , fcmReactions = emptyReactionSummary
            , fcmCreatedAt = now
            }

    hideMemory aId memoryId = do
      artistKey <- requireArtistKey aId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          isOfficer <- runDB $ checkIsOfficer user artistKey
          unless isOfficer $ throwError err403 { errBody = "No autorizado" }
          memoryKey <- either throwError pure (validateFanClubMemoryPathId memoryId)
          targetKey <- either throwError pure =<< runDB (lookupFanClubMemoryMutationTarget cid memoryKey)
          runDB $ update targetKey [M.FanClubMemoryIsHidden =. True]
          pure NoContent

    unhideMemory aId memoryId = do
      artistKey <- requireArtistKey aId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          isOfficer <- runDB $ checkIsOfficer user artistKey
          unless isOfficer $ throwError err403 { errBody = "No autorizado" }
          memoryKey <- either throwError pure (validateFanClubMemoryPathId memoryId)
          targetKey <- either throwError pure =<< runDB (lookupFanClubMemoryMutationTarget cid memoryKey)
          runDB $ update targetKey [M.FanClubMemoryIsHidden =. False]
          pure NoContent

    deleteMemory aId memoryId = do
      artistKey <- requireArtistKey aId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          isOfficer <- runDB $ checkIsOfficer user artistKey
          unless isOfficer $ throwError err403 { errBody = "No autorizado" }
          memoryKey <- either throwError pure (validateFanClubMemoryPathId memoryId)
          targetKey <- either throwError pure =<< runDB (lookupFanClubMemoryMutationTarget cid memoryKey)
          runDB $ update targetKey [M.FanClubMemoryIsDeleted =. True]
          pure NoContent

    reportMemory aId memoryId req = do
      artistKey <- requireArtistKey aId
      reason <- either throwError pure $
        validateFanClubMemoryReportReason (fcmrReqReason req)
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          memoryKey <- either throwError pure (validateFanClubMemoryPathId memoryId)
          mResult <- runDB $ do
            mMem <- get memoryKey
            case mMem of
              Nothing -> pure Nothing
              Just mem -> do
                mProfile <- get (fanClubMemoryMemberProfileId mem)
                case mProfile of
                  Nothing -> pure Nothing
                  Just mp -> do
                    if fanClubMemberProfileClubId mp /= cid
                      then pure Nothing
                      else do
                        now <- liftIO getCurrentTime
                        rid <- insert FanClubMemoryReport
                          { fanClubMemoryReportReporterId = auPartyId user
                          , fanClubMemoryReportMemoryId = memoryKey
                          , fanClubMemoryReportReason = reason
                          , fanClubMemoryReportCreatedAt = now
                          }
                        pure $ Just (rid, now)
          case mResult of
            Nothing -> throwError err404 { errBody = "Memory no encontrado" }
            Just (rid, now) -> pure FanClubMemoryReportDTO
              { fcmrId = fromSqlKey rid
              , fcmrReporterId = fromSqlKey (auPartyId user)
              , fcmrMemoryId = memoryId
              , fcmrReason = reason
              , fcmrCreatedAt = now
              }

    listMemberProfiles aId = do
      artistKey <- requireArtistKey aId
      runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure []
          Just (Entity cid _) -> do
            profiles <- selectList [M.FanClubMemberProfileClubId ==. cid] [Asc M.FanClubMemberProfileJoinedAt]
            forM profiles $ \(Entity pid p) -> do
              author <- getAuthorDTO (fanClubMemberProfilePartyId p)
              pure FanClubMemberProfileDTO
                { fcmpId = fromSqlKey pid
                , fcmpPartyId = fromSqlKey (fanClubMemberProfilePartyId p)
                , fcmpClubId = fromSqlKey cid
                , fcmpHandle = fanClubMemberProfileHandle p
                , fcmpBio = fanClubMemberProfileBio p
                , fcmpAvatarUrl = fanClubMemberProfileAvatarUrl p
                , fcmpDisplayName = sppDisplayName author
                , fcmpJoinedAt = fanClubMemberProfileJoinedAt p
                }

    getMyMemberProfile aId = do
      artistKey <- requireArtistKey aId
      mResult <- runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure Nothing
          Just (Entity cid _) -> do
            mProfile <- getBy (UniqueFanClubMemberProfile (auPartyId user) cid)
            case mProfile of
              Nothing -> pure Nothing
              Just (Entity pid p) -> do
                author <- getAuthorDTO (fanClubMemberProfilePartyId p)
                pure $ Just (cid, pid, p, author)
      case mResult of
        Nothing -> throwError err404 { errBody = "Perfil de miembro no encontrado" }
        Just (cid, pid, p, author) -> pure FanClubMemberProfileDTO
          { fcmpId = fromSqlKey pid
          , fcmpPartyId = fromSqlKey (fanClubMemberProfilePartyId p)
          , fcmpClubId = fromSqlKey cid
          , fcmpHandle = fanClubMemberProfileHandle p
          , fcmpBio = fanClubMemberProfileBio p
          , fcmpAvatarUrl = fanClubMemberProfileAvatarUrl p
          , fcmpDisplayName = sppDisplayName author
          , fcmpJoinedAt = fanClubMemberProfileJoinedAt p
          }

    updateMyMemberProfile aId req = do
      artistKey <- requireArtistKey aId
      mResult <- runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure Nothing
          Just (Entity cid _) -> do
            now <- liftIO getCurrentTime
            mProfile <- getBy (UniqueFanClubMemberProfile (auPartyId user) cid)
            profileId <- case mProfile of
              Just (Entity pid _) -> do
                update pid
                  [ M.FanClubMemberProfileHandle =. fcmpuHandle req
                  , M.FanClubMemberProfileBio =. fcmpuBio req
                  , M.FanClubMemberProfileAvatarUrl =. fcmpuAvatarUrl req
                  ]
                pure pid
              Nothing -> do
                insert FanClubMemberProfile
                  { fanClubMemberProfilePartyId = auPartyId user
                  , fanClubMemberProfileClubId = cid
                  , fanClubMemberProfileHandle = fcmpuHandle req
                  , fanClubMemberProfileBio = fcmpuBio req
                  , fanClubMemberProfileAvatarUrl = fcmpuAvatarUrl req
                  , fanClubMemberProfileJoinedAt = now
                  }
            mUpdated <- get profileId
            case mUpdated of
              Nothing -> pure Nothing
              Just p -> do
                author <- getAuthorDTO (fanClubMemberProfilePartyId p)
                pure $ Just (cid, profileId, p, author)
      case mResult of
        Nothing -> throwError err404 { errBody = "Perfil no encontrado" }
        Just (cid, profileId, p, author) -> pure FanClubMemberProfileDTO
          { fcmpId = fromSqlKey profileId
          , fcmpPartyId = fromSqlKey (fanClubMemberProfilePartyId p)
          , fcmpClubId = fromSqlKey cid
          , fcmpHandle = fanClubMemberProfileHandle p
          , fcmpBio = fanClubMemberProfileBio p
          , fcmpAvatarUrl = fanClubMemberProfileAvatarUrl p
          , fcmpDisplayName = sppDisplayName author
          , fcmpJoinedAt = fanClubMemberProfileJoinedAt p
          }

    listInboxMessages aId = do
      artistKey <- requireArtistKey aId
      isOfficer <- runDB $ checkIsOfficer user artistKey
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure []
          Just (Entity cid _) -> do
            msgs <- selectList [M.FanClubInboxMessageClubId ==. cid] [Desc M.FanClubInboxMessageCreatedAt]
            forM msgs $ \(Entity mid m) -> do
              fanAuthor <- getAuthorDTO (fanClubInboxMessageFanPartyId m)
              officerAuthor <- case fanClubInboxMessageOfficerPartyId m of
                Nothing -> pure Nothing
                Just oid -> Just <$> getAuthorDTO oid
              pure FanClubInboxMessageDTO
                { fcimId = fromSqlKey mid
                , fcimFanId = fromSqlKey (fanClubInboxMessageFanPartyId m)
                , fcimFanName = sppDisplayName fanAuthor
                , fcimFanAvatarUrl = sppAvatarUrl fanAuthor
                , fcimSubject = fanClubInboxMessageSubject m
                , fcimBody = fanClubInboxMessageBody m
                , fcimStatus = fanClubInboxMessageStatus m
                , fcimOfficerId = fmap fromSqlKey (fanClubInboxMessageOfficerPartyId m)
                , fcimOfficerName = fmap sppDisplayName officerAuthor
                , fcimReplyBody = fanClubInboxMessageReplyBody m
                , fcimCreatedAt = fanClubInboxMessageCreatedAt m
                , fcimUpdatedAt = fanClubInboxMessageUpdatedAt m
                }

    sendInboxMessage aId req = do
      artistKey <- requireArtistKey aId
      subject <- either throwError pure (validateFanClubInboxSubjectInput (fcisReqSubject req))
      body <- either throwError pure (validateFanClubInboxBodyInput (fcisReqBody req))
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> runDB $ do
          now <- liftIO getCurrentTime
          mid <- insert FanClubInboxMessage
            { fanClubInboxMessageClubId = cid
            , fanClubInboxMessageFanPartyId = auPartyId user
            , fanClubInboxMessageSubject = subject
            , fanClubInboxMessageBody = body
            , fanClubInboxMessageStatus = "unread"
            , fanClubInboxMessageOfficerPartyId = Nothing
            , fanClubInboxMessageReplyBody = Nothing
            , fanClubInboxMessageCreatedAt = now
            , fanClubInboxMessageUpdatedAt = Nothing
            }
          fanAuthor <- getAuthorDTO (auPartyId user)
          pure FanClubInboxMessageDTO
            { fcimId = fromSqlKey mid
            , fcimFanId = fromSqlKey (auPartyId user)
            , fcimFanName = sppDisplayName fanAuthor
            , fcimFanAvatarUrl = sppAvatarUrl fanAuthor
            , fcimSubject = subject
            , fcimBody = body
            , fcimStatus = "unread"
            , fcimOfficerId = Nothing
            , fcimOfficerName = Nothing
            , fcimReplyBody = Nothing
            , fcimCreatedAt = now
            , fcimUpdatedAt = Nothing
            }

    getInboxMessage aId messageId = do
      artistKey <- requireArtistKey aId
      isOfficer <- runDB $ checkIsOfficer user artistKey
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      msgKey <- either throwError pure (validatePositiveIdField "messageId" messageId)
      let mid = toSqlKey msgKey :: FanClubInboxMessageId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          mDto <- runDB $ do
            mMsg <- get mid
            case mMsg of
              Nothing -> pure Nothing
              Just m -> do
                if fanClubInboxMessageClubId m /= cid
                  then pure Nothing
                  else do
                    now <- liftIO getCurrentTime
                    when (fanClubInboxMessageStatus m == "unread") $ do
                      update mid [M.FanClubInboxMessageStatus =. "opened", M.FanClubInboxMessageUpdatedAt =. Just now]
                    fanAuthor <- getAuthorDTO (fanClubInboxMessageFanPartyId m)
                    officerAuthor <- case fanClubInboxMessageOfficerPartyId m of
                      Nothing -> pure Nothing
                      Just oid -> Just <$> getAuthorDTO oid
                    pure $ Just FanClubInboxMessageDTO
                      { fcimId = fromSqlKey mid
                      , fcimFanId = fromSqlKey (fanClubInboxMessageFanPartyId m)
                      , fcimFanName = sppDisplayName fanAuthor
                      , fcimFanAvatarUrl = sppAvatarUrl fanAuthor
                      , fcimSubject = fanClubInboxMessageSubject m
                      , fcimBody = fanClubInboxMessageBody m
                      , fcimStatus = if fanClubInboxMessageStatus m == "unread" then "opened" else fanClubInboxMessageStatus m
                      , fcimOfficerId = fmap fromSqlKey (fanClubInboxMessageOfficerPartyId m)
                      , fcimOfficerName = fmap sppDisplayName officerAuthor
                      , fcimReplyBody = fanClubInboxMessageReplyBody m
                      , fcimCreatedAt = fanClubInboxMessageCreatedAt m
                      , fcimUpdatedAt = if fanClubInboxMessageStatus m == "unread" then Just now else fanClubInboxMessageUpdatedAt m
                      }
          maybe (throwError err404 { errBody = "Mensaje no encontrado" }) pure mDto

    replyInboxMessage aId messageId req = do
      artistKey <- requireArtistKey aId
      isOfficer <- runDB $ checkIsOfficer user artistKey
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      msgKey <- either throwError pure (validatePositiveIdField "messageId" messageId)
      replyBody <- either throwError pure (validateFanClubInboxReplyBodyInput (fcirReqBody req))
      let mid = toSqlKey msgKey :: FanClubInboxMessageId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          mDto <- runDB $ do
            mMsg <- get mid
            case mMsg of
              Nothing -> pure Nothing
              Just m -> do
                if fanClubInboxMessageClubId m /= cid
                  then pure Nothing
                  else do
                    now <- liftIO getCurrentTime
                    update mid
                      [ M.FanClubInboxMessageOfficerPartyId =. Just (auPartyId user)
                      , M.FanClubInboxMessageReplyBody =. Just replyBody
                      , M.FanClubInboxMessageStatus =. "replied"
                      , M.FanClubInboxMessageUpdatedAt =. Just now
                      ]
                    fanAuthor <- getAuthorDTO (fanClubInboxMessageFanPartyId m)
                    officerAuthor <- Just <$> getAuthorDTO (auPartyId user)
                    pure $ Just FanClubInboxMessageDTO
                      { fcimId = fromSqlKey mid
                      , fcimFanId = fromSqlKey (fanClubInboxMessageFanPartyId m)
                      , fcimFanName = sppDisplayName fanAuthor
                      , fcimFanAvatarUrl = sppAvatarUrl fanAuthor
                      , fcimSubject = fanClubInboxMessageSubject m
                      , fcimBody = fanClubInboxMessageBody m
                      , fcimStatus = "replied"
                      , fcimOfficerId = Just (fromSqlKey (auPartyId user))
                      , fcimOfficerName = fmap sppDisplayName officerAuthor
                      , fcimReplyBody = Just replyBody
                      , fcimCreatedAt = fanClubInboxMessageCreatedAt m
                      , fcimUpdatedAt = Just now
                      }
          maybe (throwError err404 { errBody = "Mensaje no encontrado" }) pure mDto

    updateInboxStatus aId messageId req = do
      artistKey <- requireArtistKey aId
      isOfficer <- runDB $ checkIsOfficer user artistKey
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      msgKey <- either throwError pure (validatePositiveIdField "messageId" messageId)
      status <- either throwError pure (validateFanClubInboxStatusInput (fcistReqStatus req))
      let mid = toSqlKey msgKey :: FanClubInboxMessageId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          mDto <- runDB $ do
            mMsg <- get mid
            case mMsg of
              Nothing -> pure Nothing
              Just m -> do
                if fanClubInboxMessageClubId m /= cid
                  then pure Nothing
                  else do
                    now <- liftIO getCurrentTime
                    update mid
                      [ M.FanClubInboxMessageStatus =. status
                      , M.FanClubInboxMessageUpdatedAt =. Just now
                      ]
                    fanAuthor <- getAuthorDTO (fanClubInboxMessageFanPartyId m)
                    officerAuthor <- case fanClubInboxMessageOfficerPartyId m of
                      Nothing -> pure Nothing
                      Just oid -> Just <$> getAuthorDTO oid
                    pure $ Just FanClubInboxMessageDTO
                      { fcimId = fromSqlKey mid
                      , fcimFanId = fromSqlKey (fanClubInboxMessageFanPartyId m)
                      , fcimFanName = sppDisplayName fanAuthor
                      , fcimFanAvatarUrl = sppAvatarUrl fanAuthor
                      , fcimSubject = fanClubInboxMessageSubject m
                      , fcimBody = fanClubInboxMessageBody m
                      , fcimStatus = status
                      , fcimOfficerId = fmap fromSqlKey (fanClubInboxMessageOfficerPartyId m)
                      , fcimOfficerName = fmap sppDisplayName officerAuthor
                      , fcimReplyBody = fanClubInboxMessageReplyBody m
                      , fcimCreatedAt = fanClubInboxMessageCreatedAt m
                      , fcimUpdatedAt = Just now
                      }
          maybe (throwError err404 { errBody = "Mensaje no encontrado" }) pure mDto

    reactToPost aId postId ContentReactionReq{..} = do
      _ <- requireArtistKey aId
      reaction <- validateReaction crrReaction
      now <- liftIO getCurrentTime
      runDB $ do
        toggleReaction "post" (fromIntegral postId) (auPartyId user) reaction now
        buildReactionSummary "post" (fromIntegral postId) (auPartyId user)

    reactToMemory aId memoryId ContentReactionReq{..} = do
      _ <- requireArtistKey aId
      reaction <- validateReaction crrReaction
      now <- liftIO getCurrentTime
      runDB $ do
        toggleReaction "memory" (fromIntegral memoryId) (auPartyId user) reaction now
        buildReactionSummary "memory" (fromIntegral memoryId) (auPartyId user)

    getLeaderboard aId _mPeriod = do
      artistKey <- requireArtistKey aId
      runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure []
          Just (Entity cid _) -> do
            posts <- selectList [M.FanClubPostClubId ==. cid, M.FanClubPostParentId ==. Nothing] []
            let postIds = map (fromSqlKey . entityKey) posts
            let postAuthorMap = Map.fromList $ map (\(Entity pid p) -> (fromSqlKey pid, fanClubPostFanPartyId p)) posts
            allReactions <- selectList
              [ M.ContentReactionTargetType ==. "post"
              , M.ContentReactionTargetId <-. map fromIntegral postIds
              ] []
            let reactionsByAuthor = foldl (\acc (Entity _ r) ->
                  let targetId = contentReactionTargetId r
                      mAuthor = Map.lookup (fromIntegral targetId) postAuthorMap
                  in case mAuthor of
                       Just authorId -> Map.insertWith (+) authorId (1 :: Int) acc
                       Nothing -> acc
                  ) Map.empty allReactions
            let ranked = zip [1..] $ sortOn (Down . snd) (Map.toList reactionsByAuthor)
            forM (take 10 ranked) $ \(rank, (partyId, totalReactions)) -> do
              author <- getAuthorDTO partyId
              badges <- selectList [M.CreatorBadgePartyId ==. partyId, M.CreatorBadgeClubId ==. cid] []
              let badgeTypes = map (creatorBadgeBadgeType . entityVal) badges
              pure LeaderboardEntryDTO
                { lbPartyId = fromSqlKey partyId
                , lbDisplayName = sppDisplayName author
                , lbAvatarUrl = sppAvatarUrl author
                , lbTotalReactions = totalReactions
                , lbBadges = badgeTypes
                , lbRank = rank
                }

    getSpotlight aId = do
      artistKey <- requireArtistKey aId
      runDB $ do
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure Nothing
          Just (Entity cid _) -> do
            posts <- selectList
              [ M.FanClubPostClubId ==. cid
              , M.FanClubPostParentId ==. Nothing
              , M.FanClubPostIsHidden ==. False
              ] [Desc M.FanClubPostCreatedAt]
            let postIds = map (fromSqlKey . entityKey) posts
            allReactions <- selectList
              [ M.ContentReactionTargetType ==. "post"
              , M.ContentReactionTargetId <-. map fromIntegral postIds
              ] []
            let reactionCounts = foldl (\acc (Entity _ r) ->
                  Map.insertWith (+) (contentReactionTargetId r) (1 :: Int) acc
                  ) Map.empty allReactions
            let topPostId = fst <$> listToMaybe (sortOn (Down . snd) (Map.toList reactionCounts))
            case topPostId of
              Nothing -> pure Nothing
              Just tid -> do
                let mPost = filter (\(Entity pid _) -> fromSqlKey pid == fromIntegral tid) posts
                case mPost of
                  [] -> pure Nothing
                  (Entity pid p : _) -> do
                    officers <- selectList [M.FanClubOfficerClubId ==. cid] []
                    let officerIds = map (fromSqlKey . fanClubOfficerFanPartyId . entityVal) officers
                    author <- getAuthorDTO (fanClubPostFanPartyId p)
                    let isOfficer = fromSqlKey (fanClubPostFanPartyId p) `elem` officerIds
                    reactions <- buildReactionSummary "post" (fromIntegral (fromSqlKey pid)) (auPartyId user)
                    pure $ Just FanClubFeedItemDTO
                      { fcfId = fromSqlKey pid
                      , fcfKind = "post"
                      , fcfTitle = fanClubPostTitle p
                      , fcfContent = fanClubPostContent p
                      , fcfAuthorId = fromSqlKey (fanClubPostFanPartyId p)
                      , fcfAuthorName = sppDisplayName author
                      , fcfAvatarUrl = sppAvatarUrl author
                      , fcfMediaUrls = maybe [] (T.splitOn ",") (fanClubPostMediaUrls p)
                      , fcfIsPinned = fanClubPostIsPinned p
                      , fcfIsOfficer = isOfficer
                      , fcfIsHidden = False
                      , fcfReactions = reactions
                      , fcfCreatedAt = fanClubPostCreatedAt p
                      }

    requireArtistKey :: Int64 -> AppM PartyId
    requireArtistKey =
      either throwError pure . validateFanClubArtistPathId

-- ============================================================================
-- Content Engagement Helpers
-- ============================================================================

emptyReactionSummary :: ReactionSummaryDTO
emptyReactionSummary = ReactionSummaryDTO 0 0 0 0 0 0 Nothing

validReactions :: [Text]
validReactions = ["fire", "heart", "clap", "mic_drop", "skull"]

validateReaction :: Text -> AppM Text
validateReaction r
  | r `elem` validReactions = pure r
  | otherwise = throwBadRequest "Reacción inválida. Opciones: fire, heart, clap, mic_drop, skull"

toggleReaction :: Text -> Int -> PartyId -> Text -> UTCTime -> SqlPersistT IO ()
toggleReaction targetType targetId reactorId reaction now = do
  existing <- selectFirst
    [ M.ContentReactionTargetType ==. targetType
    , M.ContentReactionTargetId ==. targetId
    , M.ContentReactionReactorPartyId ==. reactorId
    ] []
  case existing of
    Just (Entity _ r) | contentReactionReaction r == reaction ->
      deleteWhere
        [ M.ContentReactionTargetType ==. targetType
        , M.ContentReactionTargetId ==. targetId
        , M.ContentReactionReactorPartyId ==. reactorId
        ]
    Just _ -> do
      deleteWhere
        [ M.ContentReactionTargetType ==. targetType
        , M.ContentReactionTargetId ==. targetId
        , M.ContentReactionReactorPartyId ==. reactorId
        ]
      insert_ ContentReaction
        { contentReactionTargetType = targetType
        , contentReactionTargetId = targetId
        , contentReactionReactorPartyId = reactorId
        , contentReactionReaction = reaction
        , contentReactionCreatedAt = now
        }
    Nothing ->
      insert_ ContentReaction
        { contentReactionTargetType = targetType
        , contentReactionTargetId = targetId
        , contentReactionReactorPartyId = reactorId
        , contentReactionReaction = reaction
        , contentReactionCreatedAt = now
        }

buildReactionSummary :: Text -> Int -> PartyId -> SqlPersistT IO ReactionSummaryDTO
buildReactionSummary targetType targetId viewerPartyId = do
  reactions <- selectList
    [ M.ContentReactionTargetType ==. targetType
    , M.ContentReactionTargetId ==. targetId
    ] []
  let countReaction t = length $ filter (\(Entity _ r) -> contentReactionReaction r == t) reactions
      myReaction = case filter (\(Entity _ r) -> contentReactionReactorPartyId r == viewerPartyId) reactions of
        (Entity _ r : _) -> Just (contentReactionReaction r)
        [] -> Nothing
      fire = countReaction "fire"
      heart = countReaction "heart"
      clap = countReaction "clap"
      micDrop = countReaction "mic_drop"
      skull = countReaction "skull"
  pure ReactionSummaryDTO
    { rsFire = fire
    , rsHeart = heart
    , rsClap = clap
    , rsMicDrop = micDrop
    , rsSkull = skull
    , rsTotal = fire + heart + clap + micDrop + skull
    , rsMyReaction = myReaction
    }

sortFeedItems :: Text -> UTCTime -> [FanClubFeedItemDTO] -> [FanClubFeedItemDTO]
sortFeedItems sortMode now items =
  let (pinned, unpinned) = partition fcfIsPinned items
      sorted = case sortMode of
        "hot" -> sortOn (Down . hotScore now) unpinned
        "top" -> sortOn (Down . rsTotal . fcfReactions) unpinned
        _     -> sortOn (Down . fcfCreatedAt) unpinned
  in pinned ++ sorted
  where
    hotScore :: UTCTime -> FanClubFeedItemDTO -> Double
    hotScore n item =
      let hoursSincePublish = max 1.0 (realToFrac (diffUTCTime n (fcfCreatedAt item)) / 3600.0)
          total = fromIntegral (rsTotal (fcfReactions item))
      in total / (hoursSincePublish ** 0.8)

-- ============================================================================
-- DTO Builders
-- ============================================================================

loadOfficersDTO :: FanClubId -> SqlPersistT IO [FanClubOfficerDTO]
loadOfficersDTO cid = do
  officers <- selectList [M.FanClubOfficerClubId ==. cid] [Asc M.FanClubOfficerRole]
  forM officers $ \(Entity _ o) -> do
    mParty <- selectFirst [M.PartyId ==. fanClubOfficerFanPartyId o] []
    mProfile <- selectFirst [M.FanProfileFanPartyId ==. fanClubOfficerFanPartyId o] []
    let name = case mParty of
          Just (Entity _ p) -> M.partyDisplayName p
          Nothing -> "Desconocido"
    let avatar = case mProfile of
          Just (Entity _ fp) -> fanProfileAvatarUrl fp
          Nothing -> Nothing
    pure $ FanClubOfficerDTO
      { fcoPartyId = fromSqlKey (fanClubOfficerFanPartyId o)
      , fcoFanName = name
      , fcoAvatarUrl = avatar
      , fcoRole = T.pack (show (fanClubOfficerRole o))
      , fcoElectedAt = fanClubOfficerElectedAt o
      , fcoTermEndsAt = fanClubOfficerTermEndsAt o
      }

eventToDTO :: Entity FanClubEvent -> FanClubEventDTO
eventToDTO (Entity eid e) = FanClubEventDTO
  { fceId = fromSqlKey eid
  , fceTitle = fanClubEventTitle e
  , fceDescription = fanClubEventDescription e
  , fceStartsAt = fanClubEventStartsAt e
  , fceEndsAt = fanClubEventEndsAt e
  , fceLocation = fanClubEventLocation e
  , fceIsArtistConcert = fanClubEventIsArtistConcert e
  , fceCreatedBy = fmap fromSqlKey (fanClubEventCreatedByPartyId e)
  }

postToDTO :: FanClubPostId -> FanClubPost -> Int -> SocialPartyProfileDTO -> ReactionSummaryDTO -> FanClubPostDTO
postToDTO pid p replies author reactions = FanClubPostDTO
  { fcpId = fromSqlKey pid
  , fcpParentId = fmap fromSqlKey (fanClubPostParentId p)
  , fcpTitle = fanClubPostTitle p
  , fcpContent = fanClubPostContent p
  , fcpMediaUrls = maybe [] (T.splitOn ",") (fanClubPostMediaUrls p)
  , fcpAuthorId = fromSqlKey (fanClubPostFanPartyId p)
  , fcpAuthorName = sppDisplayName author
  , fcpAvatarUrl = sppAvatarUrl author
  , fcpIsPinned = fanClubPostIsPinned p
  , fcpIsHidden = fanClubPostIsHidden p
  , fcpReplies = replies
  , fcpReactions = reactions
  , fcpCreatedAt = fanClubPostCreatedAt p
  , fcpUpdatedAt = fanClubPostUpdatedAt p
  }

candidacyToDTO :: Entity FanClubCandidacy -> FanClubCandidacyDTO
candidacyToDTO (Entity cid c) = FanClubCandidacyDTO
  { fccCandidacyId = fromSqlKey cid
  , fccFanId = fromSqlKey (fanClubCandidacyFanPartyId c)
  , fccFanName = ""
  , fccAvatarUrl = Nothing
  , fccRole = T.pack (show (fanClubCandidacyRole c))
  , fccManifesto = fanClubCandidacyManifesto c
  , fccVoteCount = 0
  }

voteToDTO :: Entity FanClubVote -> FanClubVoteDTO
voteToDTO (Entity _ v) = FanClubVoteDTO
  { fcvCandidacyId = fromSqlKey (fanClubVoteCandidacyId v)
  , fcvRole = T.pack (show (fanClubVoteRole v))
  }

getAuthorDTO :: PartyId -> SqlPersistT IO SocialPartyProfileDTO
getAuthorDTO pid = do
  mParty <- selectFirst [M.PartyId ==. pid] []
  mProfile <- selectFirst [M.FanProfileFanPartyId ==. pid] []
  let name = case mParty of
        Just (Entity _ p) -> M.partyDisplayName p
        Nothing -> "Desconocido"
  let avatar = case mProfile of
        Just (Entity _ fp) -> fanProfileAvatarUrl fp
        Nothing -> Nothing
  pure $ SocialPartyProfileDTO
    { sppPartyId = fromSqlKey pid
    , sppDisplayName = name
    , sppAvatarUrl = avatar
    , sppBio = Nothing
    , sppCity = Nothing
    }

validateFanClubMemoryPathId :: Int64 -> Either ServerError (Key FanClubMemory)
validateFanClubMemoryPathId rawMemoryId
  | rawMemoryId <= 0 = Left err400 { errBody = "Invalid fan club memory id" }
  | otherwise = Right (toSqlKey rawMemoryId)

lookupFanClubMemoryMutationTarget
  :: FanClubId
  -> Key FanClubMemory
  -> SqlPersistT IO (Either ServerError (Key FanClubMemory))
lookupFanClubMemoryMutationTarget clubId memoryId = do
  mMemory <- get memoryId
  case mMemory of
    Nothing ->
      pure (Left fanClubMemoryNotFound)
    Just memory -> do
      let profileId = fanClubMemoryMemberProfileId memory
      mProfile <- get profileId
      pure $
        validateFanClubMemoryMutationTarget
          clubId
          (Entity memoryId memory)
          (Entity profileId <$> mProfile)

validateFanClubMemoryMutationTarget
  :: FanClubId
  -> Entity FanClubMemory
  -> Maybe (Entity FanClubMemberProfile)
  -> Either ServerError (Key FanClubMemory)
validateFanClubMemoryMutationTarget clubId (Entity memoryId memory) mProfile =
  case mProfile of
    Just (Entity profileId profile)
      | profileId == fanClubMemoryMemberProfileId memory
      , fanClubMemberProfileClubId profile == clubId ->
          Right memoryId
    _ ->
      Left fanClubMemoryNotFound

fanClubMemoryNotFound :: ServerError
fanClubMemoryNotFound =
  err404 { errBody = "Fan club memory not found" }

validateFanClubMemoryReportReason :: Text -> Either ServerError Text
validateFanClubMemoryReportReason =
  validateRequiredFanClubInboxText "reason" maxFanClubMemoryReportReasonChars True

maxFanClubMemoryReportReasonChars :: Int
maxFanClubMemoryReportReasonChars = 500

validateFanClubEventTitleInput :: Text -> Either ServerError Text
validateFanClubEventTitleInput =
  validateRequiredFanClubInboxText "title" maxFanClubEventTitleChars False

validateFanClubEventDescriptionInput :: Maybe Text -> Either ServerError (Maybe Text)
validateFanClubEventDescriptionInput Nothing = Right Nothing
validateFanClubEventDescriptionInput (Just rawDescription) =
  validateOptionalFanClubInboxText
    "description"
    maxFanClubEventDescriptionChars
    True
    rawDescription

validateFanClubEventLocationInput :: Maybe Text -> Either ServerError (Maybe Text)
validateFanClubEventLocationInput Nothing = Right Nothing
validateFanClubEventLocationInput (Just rawLocation) =
  validateOptionalFanClubInboxText
    "location"
    maxFanClubEventLocationChars
    False
    rawLocation

validateFanClubEventTimeRange :: Maybe UTCTime -> Maybe UTCTime -> Either ServerError ()
validateFanClubEventTimeRange (Just startsAt) (Just endsAt)
  | endsAt <= startsAt = Left err400 { errBody = "endsAt must be after startsAt" }
validateFanClubEventTimeRange _ _ = Right ()

maxFanClubEventTitleChars :: Int
maxFanClubEventTitleChars = 160

maxFanClubEventDescriptionChars :: Int
maxFanClubEventDescriptionChars = 4096

maxFanClubEventLocationChars :: Int
maxFanClubEventLocationChars = 240

validateFanClubOfficerRoleInput :: Text -> Either ServerError FanClubOfficerRole
validateFanClubOfficerRoleInput rawRole =
  case T.toLower (T.strip rawRole) of
    "presidente" -> Right President
    "vicepresidente" -> Right VicePresident
    "secretario" -> Right Secretary
    "tesorero" -> Right Treasurer
    "coordinador" -> Right Coordinator
    "" -> Left err400 { errBody = "role is required" }
    _ ->
      Left err400
        { errBody =
            "role must be one of: presidente, vicepresidente, secretario, tesorero, coordinador"
        }

validateFanClubInboxSubjectInput :: Maybe Text -> Either ServerError (Maybe Text)
validateFanClubInboxSubjectInput Nothing = Right Nothing
validateFanClubInboxSubjectInput (Just rawSubject) =
  validateOptionalFanClubInboxText "subject" maxFanClubInboxSubjectChars False rawSubject

validateFanClubInboxBodyInput :: Text -> Either ServerError Text
validateFanClubInboxBodyInput =
  validateRequiredFanClubInboxText "body" maxFanClubInboxBodyChars True

validateFanClubInboxReplyBodyInput :: Text -> Either ServerError Text
validateFanClubInboxReplyBodyInput =
  validateRequiredFanClubInboxText "replyBody" maxFanClubInboxBodyChars True

validateFanClubPostTitleInput :: Maybe Text -> Either ServerError (Maybe Text)
validateFanClubPostTitleInput Nothing = Right Nothing
validateFanClubPostTitleInput (Just rawTitle) =
  validateOptionalFanClubInboxText "title" maxFanClubPostTitleChars False rawTitle

validateFanClubPostContentInput :: Text -> Either ServerError Text
validateFanClubPostContentInput =
  validateRequiredFanClubInboxText "content" maxFanClubPostContentChars True

validateFanClubMediaUrlsInput :: Text -> [Text] -> Either ServerError [Text]
validateFanClubMediaUrlsInput fieldName rawUrls
  | length rawUrls > maxFanClubMediaUrls =
      Left (badRequestText (fieldName <> " must include at most 10 URLs"))
  | otherwise =
      traverse (validateFanClubMediaUrlInput fieldName) rawUrls

validateFanClubMediaUrlInput :: Text -> Text -> Either ServerError Text
validateFanClubMediaUrlInput fieldName rawUrl
  | T.null mediaUrl =
      Left (badRequestText (fieldName <> " must not include blank URLs"))
  | T.length mediaUrl > maxFanClubMediaUrlChars =
      Left (badRequestText (fieldName <> " entries must be 2048 characters or fewer"))
  | T.any isSpace mediaUrl =
      Left (badRequestText (fieldName <> " entries must not contain whitespace"))
  | T.any isControl mediaUrl =
      Left (badRequestText (fieldName <> " entries must not contain control characters"))
  | T.any isHiddenFanClubMediaUrlChar mediaUrl =
      Left
        ( badRequestText
            (fieldName <> " entries must not contain hidden formatting characters")
        )
  | T.any (== ',') mediaUrl =
      Left (badRequestText (fieldName <> " entries must not contain commas"))
  | otherwise =
      Right mediaUrl
  where
    mediaUrl = T.strip rawUrl

maxFanClubMediaUrls :: Int
maxFanClubMediaUrls = 10

maxFanClubMediaUrlChars :: Int
maxFanClubMediaUrlChars = 2048

isHiddenFanClubMediaUrlChar :: Char -> Bool
isHiddenFanClubMediaUrlChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateFanClubMemoryTitleInput :: Text -> Either ServerError Text
validateFanClubMemoryTitleInput =
  validateRequiredFanClubInboxText "title" maxFanClubMemoryTitleChars False

validateFanClubMemoryDescriptionInput :: Maybe Text -> Either ServerError (Maybe Text)
validateFanClubMemoryDescriptionInput Nothing = Right Nothing
validateFanClubMemoryDescriptionInput (Just rawDescription) =
  validateOptionalFanClubInboxText
    "description"
    maxFanClubMemoryDescriptionChars
    True
    rawDescription

validateOptionalFanClubInboxText
  :: Text
  -> Int
  -> Bool
  -> Text
  -> Either ServerError (Maybe Text)
validateOptionalFanClubInboxText fieldName maxChars allowMultiline rawValue =
  case validateFanClubInboxText fieldName maxChars False allowMultiline rawValue of
    Right value | T.null value -> Right Nothing
    Right value -> Right (Just value)
    Left err -> Left err

validateRequiredFanClubInboxText
  :: Text
  -> Int
  -> Bool
  -> Text
  -> Either ServerError Text
validateRequiredFanClubInboxText fieldName maxChars allowMultiline rawValue =
  validateFanClubInboxText fieldName maxChars True allowMultiline rawValue

validateFanClubInboxText
  :: Text
  -> Int
  -> Bool
  -> Bool
  -> Text
  -> Either ServerError Text
validateFanClubInboxText fieldName maxChars required allowMultiline rawValue
  | required && T.null value =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " is required")) }
  | T.length value > maxChars =
      Left err400
        { errBody =
            BL.fromStrict $
              TE.encodeUtf8 $
                fieldName <> " must be " <> T.pack (show maxChars) <> " characters or fewer"
        }
  | T.any (invalidFanClubInboxTextChar allowMultiline) value =
      Left err400
        { errBody =
            BL.fromStrict $
              TE.encodeUtf8 $
                fieldName
                  <> " must not contain unsupported control, hidden formatting, "
                  <> "or non-ASCII whitespace characters"
        }
  | otherwise =
      Right value
  where
    value = T.strip rawValue

maxFanClubInboxSubjectChars :: Int
maxFanClubInboxSubjectChars = 160

maxFanClubInboxBodyChars :: Int
maxFanClubInboxBodyChars = 4096

maxFanClubPostTitleChars :: Int
maxFanClubPostTitleChars = 160

maxFanClubPostContentChars :: Int
maxFanClubPostContentChars = 4096

maxFanClubMemoryTitleChars :: Int
maxFanClubMemoryTitleChars = 160

maxFanClubMemoryDescriptionChars :: Int
maxFanClubMemoryDescriptionChars = 4096

invalidFanClubInboxTextChar :: Bool -> Char -> Bool
invalidFanClubInboxTextChar allowMultiline ch =
  (isControl ch && not allowedMultilineWhitespace)
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]
    || (isSpace ch && ch /= ' ' && not allowedMultilineWhitespace)
  where
    allowedMultilineWhitespace =
      allowMultiline && ch `elem` ("\n\r\t" :: String)

validateFanClubInboxStatusInput :: Text -> Either ServerError Text
validateFanClubInboxStatusInput rawStatus
  | T.null status =
      Left err400 { errBody = "status is required" }
  | T.any invalidFanClubInboxStatusChar rawStatus =
      Left err400
        { errBody =
            "status must not contain control, hidden formatting, or non-ASCII whitespace"
        }
  | normalized `elem` allowedFanClubInboxStatuses =
      Right normalized
  | otherwise =
      Left err400
        { errBody =
            "status must be one of: unread, opened, replied, archived"
        }
  where
    status = T.strip rawStatus
    normalized = T.toLower status

allowedFanClubInboxStatuses :: [Text]
allowedFanClubInboxStatuses =
  ["unread", "opened", "replied", "archived"]

invalidFanClubInboxStatusChar :: Char -> Bool
invalidFanClubInboxStatusChar ch =
  isControl ch
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]
    || (isSpace ch && ch /= ' ')

checkIsOfficer :: AuthedUser -> PartyId -> SqlPersistT IO Bool
checkIsOfficer user artistKey = do
  let userRoles = auRoles user
      isAdminOrAgency = M.Admin `elem` userRoles || M.Agency `elem` userRoles
  if isAdminOrAgency
    then pure True
    else do
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> pure False
        Just (Entity cid _) -> do
          mOfficer <- selectFirst
            [M.FanClubOfficerClubId ==. cid, M.FanClubOfficerFanPartyId ==. auPartyId user] []
          pure (isJust mOfficer)

requireFanClubPostAccess :: AuthedUser -> PartyId -> AppM ()
requireFanClubPostAccess user artistKey = do
  (isOfficer, mFollow) <- runDB $ do
    officerAccess <- checkIsOfficer user artistKey
    follow <- getBy (UniqueFanFollow (auPartyId user) artistKey)
    pure (officerAccess, follow)
  either throwError pure $
    validateFanClubPostAccess artistKey user isOfficer mFollow

validateFanClubPostAccess
  :: PartyId
  -> AuthedUser
  -> Bool
  -> Maybe (Entity FanFollow)
  -> Either ServerError ()
validateFanClubPostAccess _ _ True _ =
  Right ()
validateFanClubPostAccess artistKey user False (Just (Entity _ follow))
  | fanFollowFanPartyId follow == auPartyId user
  , fanFollowArtistPartyId follow == artistKey =
      Right ()
validateFanClubPostAccess _ _ False _ =
  Left err403 { errBody = "Debes seguir al artista para publicar en su club de fans" }

validateFanClubArtistPathId :: Int64 -> Either ServerError PartyId
validateFanClubArtistPathId rawArtistId
  | rawArtistId <= 0 = Left err400 { errBody = "Invalid fan club artist id" }
  | otherwise = Right (toSqlKey rawArtistId)

validatePositiveIdField :: Text -> Int64 -> Either ServerError Int64
validatePositiveIdField _ rawId
  | rawId <= 0 = Left err400 { errBody = "Invalid id" }
  | otherwise = Right rawId

validateFanClubPostPathId :: Int64 -> Either ServerError FanClubPostId
validateFanClubPostPathId rawPostId
  | rawPostId <= 0 = Left err400 { errBody = "Invalid fan club post id" }
  | otherwise = Right (toSqlKey rawPostId)

lookupFanClubPostMutationTarget
  :: PartyId
  -> FanClubPostId
  -> SqlPersistT IO (Either ServerError FanClubPostId)
lookupFanClubPostMutationTarget artistKey postId = do
  mClub <- getBy (UniqueFanClubArtist artistKey)
  case mClub of
    Nothing -> pure (Left fanClubPostNotFound)
    Just (Entity clubId _) -> do
      mPost <- get postId
      pure $
        maybe
          (Left fanClubPostNotFound)
          (validateFanClubPostMutationTarget clubId . Entity postId)
          mPost

validateFanClubPostMutationTarget
  :: FanClubId
  -> Entity FanClubPost
  -> Either ServerError FanClubPostId
validateFanClubPostMutationTarget clubId (Entity postId post)
  | fanClubPostClubId post == clubId = Right postId
  | otherwise = Left fanClubPostNotFound

validateFanClubReplyParentTarget
  :: FanClubId
  -> Entity FanClubPost
  -> Either ServerError FanClubPostId
validateFanClubReplyParentTarget clubId entity@(Entity _ post) =
  case validateFanClubPostMutationTarget clubId entity of
    Left err -> Left err
    Right postId
      | isJust (fanClubPostParentId post) ->
          Left err400 { errBody = "Fan club replies must target a top-level post" }
      | otherwise -> Right postId

fanClubPostNotFound :: ServerError
fanClubPostNotFound =
  err404 { errBody = "Fan club post not found" }

validateFanClubElectionPathId :: Int64 -> Either ServerError FanClubElectionId
validateFanClubElectionPathId rawElectionId
  | rawElectionId <= 0 = Left err400 { errBody = "Invalid fan club election id" }
  | otherwise = Right (toSqlKey rawElectionId)

lookupFanClubElectionMutationTarget
  :: PartyId
  -> FanClubElectionId
  -> SqlPersistT IO (Either ServerError FanClubElectionId)
lookupFanClubElectionMutationTarget artistKey electionId = do
  mClub <- getBy (UniqueFanClubArtist artistKey)
  case mClub of
    Nothing -> pure (Left fanClubElectionNotFound)
    Just (Entity clubId _) -> do
      mElection <- get electionId
      pure $
        maybe
          (Left fanClubElectionNotFound)
          (validateFanClubElectionMutationTarget clubId . Entity electionId)
          mElection

validateFanClubElectionMutationTarget
  :: FanClubId
  -> Entity FanClubElection
  -> Either ServerError FanClubElectionId
validateFanClubElectionMutationTarget clubId (Entity electionId election)
  | fanClubElectionClubId election == clubId = Right electionId
  | otherwise = Left fanClubElectionNotFound

fanClubElectionNotFound :: ServerError
fanClubElectionNotFound =
  err404 { errBody = "Fan club election not found" }

validateFanClubCandidacyPathId :: Int64 -> Either ServerError FanClubCandidacyId
validateFanClubCandidacyPathId rawCandidacyId
  | rawCandidacyId <= 0 = Left err400 { errBody = "Invalid fan club candidacy id" }
  | otherwise = Right (toSqlKey rawCandidacyId)

validateFanClubVoteSelectionIds :: [Int64] -> Either ServerError [FanClubCandidacyId]
validateFanClubVoteSelectionIds rawCandidacyIds
  | null rawCandidacyIds =
      Left err400 { errBody = "Fan club vote requires at least one candidacy id" }
  | length rawCandidacyIds > maxFanClubVoteSelections =
      Left err400 { errBody = "Fan club vote can include at most 5 candidacy ids" }
  | length rawCandidacyIds /= length (nub rawCandidacyIds) =
      Left err400 { errBody = "Fan club vote must not include duplicate candidacy ids" }
  | otherwise =
      traverse validateFanClubCandidacyPathId rawCandidacyIds

maxFanClubVoteSelections :: Int
maxFanClubVoteSelections = 5

lookupFanClubVoteCandidacyTarget
  :: FanClubElectionId
  -> FanClubCandidacyId
  -> SqlPersistT IO (Either ServerError (FanClubCandidacyId, FanClubCandidacy))
lookupFanClubVoteCandidacyTarget electionId candidacyId = do
  mCandidacy <- get candidacyId
  pure $
    case mCandidacy of
      Nothing -> Left fanClubCandidacyNotFound
      Just candidacy ->
        case validateFanClubVoteCandidacyTarget electionId (Entity candidacyId candidacy) of
          Left err -> Left err
          Right targetId -> Right (targetId, candidacy)

validateFanClubVoteCandidacyTarget
  :: FanClubElectionId
  -> Entity FanClubCandidacy
  -> Either ServerError FanClubCandidacyId
validateFanClubVoteCandidacyTarget electionId (Entity candidacyId candidacy)
  | fanClubCandidacyElectionId candidacy == electionId = Right candidacyId
  | otherwise = Left fanClubCandidacyNotFound

validateFanClubVoteCandidacyTargets
  :: [(FanClubCandidacyId, FanClubCandidacy)]
  -> Either ServerError [(FanClubCandidacyId, FanClubCandidacy)]
validateFanClubVoteCandidacyTargets targets
  | length roles /= length (nub roles) =
      Left err400
        { errBody = "Fan club vote must select at most one candidacy per officer role" }
  | otherwise =
      Right targets
  where
    roles = map (fanClubCandidacyRole . snd) targets

fanClubCandidacyNotFound :: ServerError
fanClubCandidacyNotFound =
  err404 { errBody = "Fan club candidacy not found" }
