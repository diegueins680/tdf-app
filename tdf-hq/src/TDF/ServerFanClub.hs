{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerFanClub
  ( fanClubPublicGetClub
  , fanClubPublicGetEvents
  , fanClubSecureListMyClubs
  , fanClubSecureArtistHandlers
  , validateFanClubPostPathId
  , validateFanClubPostMutationTarget
  , validateFanClubReplyParentTarget
  , validateFanClubElectionPathId
  , validateFanClubElectionMutationTarget
  , validateFanClubCandidacyPathId
  , validateFanClubVoteCandidacyTarget
  , validateFanClubOfficerRoleInput
  ) where

import           Control.Monad          (forM, forM_, when, unless, void)
import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask)
import           Data.Int               (Int64)
import           Data.Maybe             (isJust, mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString.Lazy   as BL
import           Data.Time              (getCurrentTime)

import           Database.Persist       (Entity(..), (=.), (==.), SelectOpt(Asc, Desc)
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
                                         , Unique(..), FanClubElectionId, FanClubCandidacyId)
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
  let artistKey = toSqlKey artistId
  mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
  case mClub of
    Nothing -> throwError err404 { errBody = "Club de fans no encontrado" }
    Just (Entity cid club) -> runDB $ do
      officers <- loadOfficersDTO cid
      followerCount <- count [M.FanFollowArtistPartyId ==. artistKey]
      pure FanClubDTO
        { fcId = fromSqlKey cid
        , fcArtistId = artistId
        , fcName = fanClubName club
        , fcDescription = fanClubDescription club
        , fcOfficers = officers
        , fcFollowerCount = fromIntegral followerCount
        }

fanClubPublicGetEvents :: Int64 -> AppM [FanClubEventDTO]
fanClubPublicGetEvents artistId = runDB $ do
  let artistKey = toSqlKey artistId
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
        pure $ Just FanClubDTO
          { fcId = fromSqlKey cid
          , fcArtistId = fromSqlKey artistKey
          , fcName = fanClubName club
          , fcDescription = fanClubDescription club
          , fcOfficers = officers
          , fcFollowerCount = fromIntegral followerCount
          }
  pure (mapMaybe id clubs)

fanClubSecureArtistHandlers :: AuthedUser -> Int64 ->
  ( AppM FanClubDTO
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
  )
fanClubSecureArtistHandlers user artistId =
       getClubDetail artistId
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
  where
    getClubDetail aId = do
      let artistKey = toSqlKey aId
      mClub <- runDB $ getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club de fans no encontrado" }
        Just (Entity cid club) -> runDB $ do
          officers <- loadOfficersDTO cid
          followerCount <- count [M.FanFollowArtistPartyId ==. artistKey]
          pure FanClubDTO
            { fcId = fromSqlKey cid
            , fcArtistId = aId
            , fcName = fanClubName club
            , fcDescription = fanClubDescription club
            , fcOfficers = officers
            , fcFollowerCount = fromIntegral followerCount
            }

    listClubPosts aId = runDB $ do
      let artistKey = toSqlKey aId
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
      let artistKey = toSqlKey aId
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
              , fanClubPostIsPinned = False
              , fanClubPostIsHidden = False
              , fanClubPostCreatedAt = now
              , fanClubPostUpdatedAt = Nothing
              }
            author <- getAuthorDTO (auPartyId user)
            pure $ postToDTO pid
              (FanClubPost cid (auPartyId user) parentKey (fcpReqTitle req) (fcpReqContent req) False False now Nothing)
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
      isOfficer <- runDB $ checkIsOfficer aId (auPartyId user)
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      postKey <- either throwError pure (validateFanClubPostPathId rawPostId)
      target <- runDB $ lookupFanClubPostMutationTarget aId postKey
      either throwError pure target

    listClubEvents aId = runDB $ do
      let artistKey = toSqlKey aId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> pure []
        Just (Entity cid _) -> do
          events <- selectList [M.FanClubEventClubId ==. cid] [Asc M.FanClubEventStartsAt]
          pure (map eventToDTO events)

    createClubEvent aId req = do
      isOfficer <- runDB $ checkIsOfficer aId (auPartyId user)
      unless isOfficer $ throwError err403 { errBody = "Solo la directiva puede crear eventos" }
      let artistKey = toSqlKey aId
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

    listClubElections aId = runDB $ do
      let artistKey = toSqlKey aId
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
      isOfficer <- runDB $ checkIsOfficer aId (auPartyId user)
      unless isOfficer $ throwError err403 { errBody = "Solo la directiva puede crear elecciones" }
      let artistKey = toSqlKey aId
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
      electionKey <- either throwError pure (validateFanClubElectionPathId electionId)
      targetElection <- runDB $ lookupFanClubElectionMutationTarget aId electionKey
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
      electionKey <- either throwError pure (validateFanClubElectionPathId electionId)
      targetElection <- runDB $ lookupFanClubElectionMutationTarget aId electionKey
      _ <- either throwError pure targetElection
      candidacyKeys <- either throwError pure $
        traverse validateFanClubCandidacyPathId (fcvCandidacyIds req)
      candidacies <- runDB $ traverse (lookupFanClubVoteCandidacyTarget electionKey) candidacyKeys
      resolvedCandidacies <- either throwError pure (sequence candidacies)
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

checkIsOfficer :: Int64 -> PartyId -> SqlPersistT IO Bool
checkIsOfficer artistId fanId = do
  let artistKey = toSqlKey artistId
  mClub <- getBy (UniqueFanClubArtist artistKey)
  case mClub of
    Nothing -> pure False
    Just (Entity cid _) -> do
      mOfficer <- selectFirst
        [M.FanClubOfficerClubId ==. cid, M.FanClubOfficerFanPartyId ==. fanId] []
      pure (isJust mOfficer)

validateFanClubPostPathId :: Int64 -> Either ServerError FanClubPostId
validateFanClubPostPathId rawPostId
  | rawPostId <= 0 = Left err400 { errBody = "Invalid fan club post id" }
  | otherwise = Right (toSqlKey rawPostId)

lookupFanClubPostMutationTarget
  :: Int64
  -> FanClubPostId
  -> SqlPersistT IO (Either ServerError FanClubPostId)
lookupFanClubPostMutationTarget artistId postId = do
  let artistKey = toSqlKey artistId
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
  :: Int64
  -> FanClubElectionId
  -> SqlPersistT IO (Either ServerError FanClubElectionId)
lookupFanClubElectionMutationTarget artistId electionId = do
  let artistKey = toSqlKey artistId
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

fanClubCandidacyNotFound :: ServerError
fanClubCandidacyNotFound =
  err404 { errBody = "Fan club candidacy not found" }
