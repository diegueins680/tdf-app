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
  , validateFanClubPostMutationTarget
  , validateFanClubReplyParentTarget
  , validateFanClubElectionPathId
  , validateFanClubElectionMutationTarget
  , validateFanClubCandidacyPathId
  , validateFanClubVoteSelectionIds
  , validateFanClubVoteCandidacyTarget
  , validateFanClubVoteCandidacyTargets
  , validateFanClubOfficerRoleInput
  , validateFanClubMemoryPathId
  ) where

import           Control.Arrow          ((&&&))
import           Control.Monad          (forM, forM_, when, unless, void)
import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask)
import           Data.Int               (Int64)
import           Data.List              (nub, sortOn)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (isJust, mapMaybe, catMaybes, fromMaybe)
import           Data.Ord               (Down(..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString.Lazy   as BL
import           Data.Time              (getCurrentTime)

import           Database.Persist       (Entity(..), Key, (=.), (==.), (<-.), SelectOpt(Asc, Desc)
                                         , get, getBy, insert, insertUnique, selectFirst, selectList, update, count)
import           Database.Persist.Sql   (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
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
                                         , RoleEnum(..)
                                         , Unique(..), FanClubElectionId, FanClubCandidacyId, artistProfileHeroImageUrl)
import qualified TDF.Models             as M

type AppM = ReaderT Env Handler

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  Env{..} <- ask
  liftIO $ runSqlPool action envPool

throwBadRequest :: Text -> AppM a
throwBadRequest msg = throwError Servant.err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

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
  :<|> AppM [FanClubFeedItemDTO]
  :<|> AppM [FanClubPostDTO]
  :<|> (FanClubCreatePostReq -> AppM FanClubPostDTO)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> AppM NoContent)
  :<|> (Int64 -> AppM NoContent)
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

    listClubFeed aId = do
      artistKey <- requireArtistKey aId
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
                , fcfCreatedAt = fanClubPostCreatedAt p
                }
            memoryItems <- forM validMemories $ \(Entity mid m) -> do
              let mprofile = Map.lookup (fanClubMemoryMemberProfileId m) memberProfileMap
              case mprofile of
                Nothing -> pure Nothing
                Just mp -> do
                  author <- getAuthorDTO (fanClubMemberProfilePartyId mp)
                  let isOfficer = fromSqlKey (fanClubMemberProfilePartyId mp) `elem` officerIds
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
                    , fcfCreatedAt = fanClubMemoryCreatedAt m
                    }
            let allItems = postItems ++ catMaybes memoryItems
            pure $ sortOn (Down . fcfIsOfficer &&& Down . fcfCreatedAt) allItems

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
              pure $ postToDTO pid p (fromIntegral replies) author

    createClubPost aId req = do
      artistKey <- requireArtistKey aId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          parentKey <- resolveParentKey cid (fcpReqParentId req)
          runDB $ do
            now <- liftIO getCurrentTime
            pid <- insert FanClubPost
              { fanClubPostClubId = cid
              , fanClubPostFanPartyId = auPartyId user
              , fanClubPostParentId = parentKey
              , fanClubPostTitle = fcpReqTitle req
              , fanClubPostContent = fcpReqContent req
              , fanClubPostMediaUrls = if null (fcpReqMediaUrls req) then Nothing else Just (T.intercalate "," (fcpReqMediaUrls req))
              , fanClubPostIsPinned = False
              , fanClubPostIsHidden = False
              , fanClubPostCreatedAt = now
              , fanClubPostUpdatedAt = Nothing
              }
            author <- getAuthorDTO (auPartyId user)
            pure $ postToDTO pid
              (FanClubPost cid (auPartyId user) parentKey (fcpReqTitle req) (fcpReqContent req) (if null (fcpReqMediaUrls req) then Nothing else Just (T.intercalate "," (fcpReqMediaUrls req))) False False now Nothing)
              0 author

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
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> runDB $ do
          now <- liftIO getCurrentTime
          eid <- insert FanClubEvent
            { fanClubEventClubId = cid
            , fanClubEventTitle = fcevTitle req
            , fanClubEventDescription = fcevDescription req
            , fanClubEventStartsAt = fcevStartsAt req
            , fanClubEventEndsAt = fcevEndsAt req
            , fanClubEventLocation = fcevLocation req
            , fanClubEventIsArtistConcert = False
            , fanClubEventCreatedByPartyId = Just (auPartyId user)
            , fanClubEventCreatedAt = now
            }
          pure $ FanClubEventDTO
            { fceId = fromSqlKey eid
            , fceTitle = fcevTitle req
            , fceDescription = fcevDescription req
            , fceStartsAt = fcevStartsAt req
            , fceEndsAt = fcevEndsAt req
            , fceLocation = fcevLocation req
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
                , fcmCreatedAt = fanClubMemoryCreatedAt m
                }

    createClubMemory aId req = do
      artistKey <- requireArtistKey aId
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
            , fanClubMemoryTitle = fcmReqTitle req
            , fanClubMemoryDescription = fcmReqDescription req
            , fanClubMemoryMediaUrls = if null (fcmReqMediaUrls req) then Nothing else Just (T.intercalate "," (fcmReqMediaUrls req))
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
            , fcmTitle = fcmReqTitle req
            , fcmDescription = fcmReqDescription req
            , fcmMediaUrls = fcmReqMediaUrls req
            , fcmIsHidden = False
            , fcmIsDeleted = False
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
          runDB $ do
            mMem <- get memoryKey
            case mMem of
              Nothing -> pure ()
              Just mem -> do
                mProfile <- get (fanClubMemoryMemberProfileId mem)
                case mProfile of
                  Just mp | fanClubMemberProfileClubId mp == cid -> update memoryKey [M.FanClubMemoryIsHidden =. True]
                  _ -> pure ()
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
          runDB $ do
            mMem <- get memoryKey
            case mMem of
              Nothing -> pure ()
              Just mem -> do
                mProfile <- get (fanClubMemoryMemberProfileId mem)
                case mProfile of
                  Just mp | fanClubMemberProfileClubId mp == cid -> update memoryKey [M.FanClubMemoryIsHidden =. False]
                  _ -> pure ()
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
          runDB $ do
            mMem <- get memoryKey
            case mMem of
              Nothing -> pure ()
              Just mem -> do
                mProfile <- get (fanClubMemoryMemberProfileId mem)
                case mProfile of
                  Just mp | fanClubMemberProfileClubId mp == cid -> update memoryKey [M.FanClubMemoryIsDeleted =. True]
                  _ -> pure ()
          pure NoContent

    reportMemory aId memoryId req = do
      artistKey <- requireArtistKey aId
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
                          , fanClubMemoryReportReason = fcmrReqReason req
                          , fanClubMemoryReportCreatedAt = now
                          }
                        pure $ Just (rid, now)
          case mResult of
            Nothing -> throwError err404 { errBody = "Memory no encontrado" }
            Just (rid, now) -> pure FanClubMemoryReportDTO
              { fcmrId = fromSqlKey rid
              , fcmrReporterId = fromSqlKey (auPartyId user)
              , fcmrMemoryId = memoryId
              , fcmrReason = fcmrReqReason req
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
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> runDB $ do
          now <- liftIO getCurrentTime
          mid <- insert FanClubInboxMessage
            { fanClubInboxMessageClubId = cid
            , fanClubInboxMessageFanPartyId = auPartyId user
            , fanClubInboxMessageSubject = fcisReqSubject req
            , fanClubInboxMessageBody = fcisReqBody req
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
            , fcimSubject = fcisReqSubject req
            , fcimBody = fcisReqBody req
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
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> runDB $ do
          let msgKey = validatePositiveIdField "messageId" messageId
          case msgKey of
            Left _ -> throwError err404 { errBody = "Mensaje no encontrado" }
            Right validMsgKey -> do
              let mid = toSqlKey validMsgKey :: FanClubInboxMessageId
              mMsg <- get mid
              case mMsg of
                Nothing -> throwError err404 { errBody = "Mensaje no encontrado" }
                Just m -> do
                  unless (fanClubInboxMessageClubId m == cid) $ throwError err404 { errBody = "Mensaje no encontrado" }
                  now <- liftIO getCurrentTime
                  when (fanClubInboxMessageStatus m == "unread") $ do
                    update mid [M.FanClubInboxMessageStatus =. "opened", M.FanClubInboxMessageUpdatedAt =. Just now]
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
                    , fcimStatus = if fanClubInboxMessageStatus m == "unread" then "opened" else fanClubInboxMessageStatus m
                    , fcimOfficerId = fmap fromSqlKey (fanClubInboxMessageOfficerPartyId m)
                    , fcimOfficerName = fmap sppDisplayName officerAuthor
                    , fcimReplyBody = fanClubInboxMessageReplyBody m
                    , fcimCreatedAt = fanClubInboxMessageCreatedAt m
                    , fcimUpdatedAt = if fanClubInboxMessageStatus m == "unread" then Just now else fanClubInboxMessageUpdatedAt m
                    }

    replyInboxMessage aId messageId req = do
      artistKey <- requireArtistKey aId
      isOfficer <- runDB $ checkIsOfficer user artistKey
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> runDB $ do
          let msgKey = validatePositiveIdField "messageId" messageId
          case msgKey of
            Left _ -> throwError err404 { errBody = "Mensaje no encontrado" }
            Right validMsgKey -> do
              let mid = toSqlKey validMsgKey :: FanClubInboxMessageId
              mMsg <- get mid
              case mMsg of
                Nothing -> throwError err404 { errBody = "Mensaje no encontrado" }
                Just m -> do
                  unless (fanClubInboxMessageClubId m == cid) $ throwError err404 { errBody = "Mensaje no encontrado" }
                  now <- liftIO getCurrentTime
                  update mid
                    [ M.FanClubInboxMessageOfficerPartyId =. Just (auPartyId user)
                    , M.FanClubInboxMessageReplyBody =. Just (fcirReqBody req)
                    , M.FanClubInboxMessageStatus =. "replied"
                    , M.FanClubInboxMessageUpdatedAt =. Just now
                    ]
                  fanAuthor <- getAuthorDTO (fanClubInboxMessageFanPartyId m)
                  officerAuthor <- Just <$> getAuthorDTO (auPartyId user)
                  pure FanClubInboxMessageDTO
                    { fcimId = fromSqlKey mid
                    , fcimFanId = fromSqlKey (fanClubInboxMessageFanPartyId m)
                    , fcimFanName = sppDisplayName fanAuthor
                    , fcimFanAvatarUrl = sppAvatarUrl fanAuthor
                    , fcimSubject = fanClubInboxMessageSubject m
                    , fcimBody = fanClubInboxMessageBody m
                    , fcimStatus = "replied"
                    , fcimOfficerId = Just (fromSqlKey (auPartyId user))
                    , fcimOfficerName = Just (sppDisplayName officerAuthor)
                    , fcimReplyBody = Just (fcirReqBody req)
                    , fcimCreatedAt = fanClubInboxMessageCreatedAt m
                    , fcimUpdatedAt = Just now
                    }

    updateInboxStatus aId messageId req = do
      artistKey <- requireArtistKey aId
      isOfficer <- runDB $ checkIsOfficer user artistKey
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> runDB $ do
          let msgKey = validatePositiveIdField "messageId" messageId
          case msgKey of
            Left _ -> throwError err404 { errBody = "Mensaje no encontrado" }
            Right validMsgKey -> do
              let mid = toSqlKey validMsgKey :: FanClubInboxMessageId
              mMsg <- get mid
              case mMsg of
                Nothing -> throwError err404 { errBody = "Mensaje no encontrado" }
                Just m -> do
                  unless (fanClubInboxMessageClubId m == cid) $ throwError err404 { errBody = "Mensaje no encontrado" }
                  now <- liftIO getCurrentTime
                  update mid
                    [ M.FanClubInboxMessageStatus =. fcistReqStatus req
                    , M.FanClubInboxMessageUpdatedAt =. Just now
                    ]
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
                    , fcimStatus = fcistReqStatus req
                    , fcimOfficerId = fmap fromSqlKey (fanClubInboxMessageOfficerPartyId m)
                    , fcimOfficerName = fmap sppDisplayName officerAuthor
                    , fcimReplyBody = fanClubInboxMessageReplyBody m
                    , fcimCreatedAt = fanClubInboxMessageCreatedAt m
                    , fcimUpdatedAt = Just now
                    }

    requireArtistKey :: Int64 -> AppM PartyId
    requireArtistKey =
      either throwError pure . validateFanClubArtistPathId

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

postToDTO :: FanClubPostId -> FanClubPost -> Int -> SocialPartyProfileDTO -> FanClubPostDTO
postToDTO pid p replies author = FanClubPostDTO
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
