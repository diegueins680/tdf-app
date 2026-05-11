{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.ServerFanClub
  ( fanClubPublicServer
  , fanClubSecureServer
  ) where

import           Control.Monad          (forM, when, unless, void)
import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask, asks)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe, isJust, mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString.Lazy   as BL
import           Data.Time              (UTCTime, getCurrentTime)

import           Database.Persist       (Entity(..), (=.), (==.), SelectOpt(Asc, Desc)
                                         , deleteBy, getBy, insert, insertUnique, selectFirst, selectList, update, count)
import           Database.Persist.Sql   (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import           Servant                (NoContent(..), ServerError, ServerT, err400, err403, err404, errBody, (:<|>)(..))

import           TDF.API                (FanPublicAPI, FanSecureAPI)
import           TDF.Auth               (AuthedUser(..))
import           TDF.DB                 (Env(..))
import           TDF.DTO
import           TDF.Models             (FanClub(..), FanClubOfficer(..), FanClubElection(..)
                                         , FanClubCandidacy(..), FanClubVote(..), FanClubPost(..)
                                         , FanClubEvent(..), FanClubOfficerRole(..), ElectionStatus(..)
                                         , Party(..), FanFollow(..), FanProfile(..)
                                         , UniqueFanClubArtist, UniqueFanClubOfficer, UniqueFanClubElection
                                         , UniqueFanClubCandidacy, UniqueFanClubVote, UniqueFanFollow)
import qualified TDF.Models             as M

type AppM = Servant.Handler

throwBadRequest :: Text -> Servant.Handler a
throwBadRequest msg = throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

runDB :: SqlPersistT IO a -> Servant.Handler a
runDB action = do
  Env{..} <- ask
  liftIO $ runSqlPool action envPool

-- ============================================================================
-- Public Server
-- ============================================================================

fanClubPublicServer :: ServerT FanPublicAPI (Servant.Handler)
fanClubPublicServer =
       undefined
  :<|> undefined
  :<|> undefined
  :<|> getPublicClub
  :<|> getPublicClubEvents
  where
    getPublicClub :: Int64 -> Servant.Handler FanClubDTO
    getPublicClub artistId = runDB $ do
      let artistKey = toSqlKey artistId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club de fans no encontrado" }
        Just (Entity cid club) -> do
          officers <- loadOfficersDTO cid
          followerCount <- count [FanFollowArtistPartyId ==. artistKey]
          pure FanClubDTO
            { fcId = fromSqlKey cid
            , fcArtistId = artistId
            , fcName = fanClubName club
            , fcDescription = fanClubDescription club
            , fcOfficers = officers
            , fcFollowerCount = fromIntegral followerCount
            }

    getPublicClubEvents :: Int64 -> Servant.Handler [FanClubEventDTO]
    getPublicClubEvents artistId = runDB $ do
      let artistKey = toSqlKey artistId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> pure []
        Just (Entity cid _) -> do
          events <- selectList [FanClubEventClubId ==. cid] [Asc FanClubEventStartsAt]
          pure (map eventToDTO events)

-- ============================================================================
-- Secure Server
-- ============================================================================

fanClubSecureServer :: AuthedUser -> ServerT FanSecureAPI (Servant.Handler)
fanClubSecureServer user =
       undefined
  :<|> undefined
  :<|> undefined
  :<|> listMyClubs
  :<|> fanClubArtistServer
  where
    listMyClubs :: Servant.Handler [FanClubDTO]
    listMyClubs = runDB $ do
      follows <- selectList [FanFollowFanPartyId ==. auPartyId user] [Desc FanFollowCreatedAt]
      clubs <- forM follows $ \(Entity _ follow) -> do
        let artistKey = fanFollowArtistPartyId follow
        mClub <- getBy (UniqueFanClubArtist artistKey)
        case mClub of
          Nothing -> pure Nothing
          Just (Entity cid club) -> do
            officers <- loadOfficersDTO cid
            followerCount <- count [FanFollowArtistPartyId ==. artistKey]
            pure $ Just FanClubDTO
              { fcId = fromSqlKey cid
              , fcArtistId = fromSqlKey artistKey
              , fcName = fanClubName club
              , fcDescription = fanClubDescription club
              , fcOfficers = officers
              , fcFollowerCount = fromIntegral followerCount
              }
      pure (mapMaybe id clubs)

    fanClubArtistServer :: Int64 ->
      ( Servant.Handler FanClubDTO
      :<|> Servant.Handler [FanClubPostDTO]
      :<|> (FanClubCreatePostReq -> Servant.Handler FanClubPostDTO)
      :<|> (Int64 -> Servant.Handler NoContent)
      :<|> (Int64 -> Servant.Handler NoContent)
      :<|> (Int64 -> Servant.Handler NoContent)
      :<|> (Int64 -> Servant.Handler NoContent)
      :<|> Servant.Handler [FanClubEventDTO]
      :<|> (FanClubCreateEventReq -> Servant.Handler FanClubEventDTO)
      :<|> Servant.Handler [FanClubElectionDTO]
      :<|> (FanClubCreateElectionReq -> Servant.Handler FanClubElectionDTO)
      :<|> (Int64 -> FanClubCreateCandidacyReq -> Servant.Handler FanClubCandidacyDTO)
      :<|> (Int64 -> FanClubVoteReq -> Servant.Handler NoContent)
      )
    fanClubArtistServer artistId =
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

    getClubDetail :: Int64 -> Servant.Handler FanClubDTO
    getClubDetail artistId = runDB $ do
      let artistKey = toSqlKey artistId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club de fans no encontrado" }
        Just (Entity cid club) -> do
          officers <- loadOfficersDTO cid
          followerCount <- count [FanFollowArtistPartyId ==. artistKey]
          pure FanClubDTO
            { fcId = fromSqlKey cid
            , fcArtistId = artistId
            , fcName = fanClubName club
            , fcDescription = fanClubDescription club
            , fcOfficers = officers
            , fcFollowerCount = fromIntegral followerCount
            }

    listClubPosts :: Int64 -> Servant.Handler [FanClubPostDTO]
    listClubPosts artistId = runDB $ do
      let artistKey = toSqlKey artistId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> pure []
        Just (Entity cid _) -> do
          posts <- selectList
            [ FanClubPostClubId ==. cid
            , FanClubPostParentId ==. Nothing
            ] [Desc FanClubPostIsPinned, Desc FanClubPostCreatedAt]
          forM posts $ \(Entity pid p) -> do
            replies <- count [FanClubPostParentId ==. Just pid]
            author <- getAuthorDTO (fanClubPostFanPartyId p)
            pure $ postToDTO pid p (fromIntegral replies) author

    createClubPost :: Int64 -> FanClubCreatePostReq -> Servant.Handler FanClubPostDTO
    createClubPost artistId req = runDB $ do
      let artistKey = toSqlKey artistId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          now <- liftIO getCurrentTime
          let parentKey = fmap toSqlKey (fcpParentId req)
          pid <- insert FanClubPost
            { fanClubPostClubId = cid
            , fanClubPostFanPartyId = auPartyId user
            , fanClubPostParentId = parentKey
            , fanClubPostTitle = fcpTitle req
            , fanClubPostContent = fcpContent req
            , fanClubPostIsPinned = False
            , fanClubPostIsHidden = False
            , fanClubPostCreatedAt = now
            , fanClubPostUpdatedAt = Nothing
            }
          author <- getAuthorDTO (auPartyId user)
          pure $ postToDTO pid
            (FanClubPost cid (auPartyId user) parentKey (fcpTitle req) (fcpContent req) False False now Nothing)
            0 author

    pinPost :: Int64 -> Int64 -> Servant.Handler NoContent
    pinPost artistId postId = runDB $ do
      isOfficer <- checkIsOfficer artistId (auPartyId user)
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      let postKey = toSqlKey postId
      update postKey [FanClubPostIsPinned =. True]
      pure NoContent

    unpinPost :: Int64 -> Int64 -> Servant.Handler NoContent
    unpinPost artistId postId = runDB $ do
      isOfficer <- checkIsOfficer artistId (auPartyId user)
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      let postKey = toSqlKey postId
      update postKey [FanClubPostIsPinned =. False]
      pure NoContent

    hidePost :: Int64 -> Int64 -> Servant.Handler NoContent
    hidePost artistId postId = runDB $ do
      isOfficer <- checkIsOfficer artistId (auPartyId user)
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      let postKey = toSqlKey postId
      update postKey [FanClubPostIsHidden =. True]
      pure NoContent

    unhidePost :: Int64 -> Int64 -> Servant.Handler NoContent
    unhidePost artistId postId = runDB $ do
      isOfficer <- checkIsOfficer artistId (auPartyId user)
      unless isOfficer $ throwError err403 { errBody = "No autorizado" }
      let postKey = toSqlKey postId
      update postKey [FanClubPostIsHidden =. False]
      pure NoContent

    listClubEvents :: Int64 -> Servant.Handler [FanClubEventDTO]
    listClubEvents artistId = runDB $ do
      let artistKey = toSqlKey artistId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> pure []
        Just (Entity cid _) -> do
          events <- selectList [FanClubEventClubId ==. cid] [Asc FanClubEventStartsAt]
          pure (map eventToDTO events)

    createClubEvent :: Int64 -> FanClubCreateEventReq -> Servant.Handler FanClubEventDTO
    createClubEvent artistId req = runDB $ do
      isOfficer <- checkIsOfficer artistId (auPartyId user)
      unless isOfficer $ throwError err403 { errBody = "Solo la directiva puede crear eventos" }
      let artistKey = toSqlKey artistId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
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

    listClubElections :: Int64 -> Servant.Handler [FanClubElectionDTO]
    listClubElections artistId = runDB $ do
      let artistKey = toSqlKey artistId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> pure []
        Just (Entity cid _) -> do
          elections <- selectList [FanClubElectionClubId ==. cid] [Desc FanClubElectionYear]
          forM elections $ \(Entity eid el) -> do
            myCands <- selectList
              [FanClubCandidacyElectionId ==. eid, FanClubCandidacyFanPartyId ==. auPartyId user] []
            myVotes <- selectList
              [FanClubVoteElectionId ==. eid, FanClubVoteFanPartyId ==. auPartyId user] []
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

    createClubElection :: Int64 -> FanClubCreateElectionReq -> Servant.Handler FanClubElectionDTO
    createClubElection artistId req = runDB $ do
      isOfficer <- checkIsOfficer artistId (auPartyId user)
      unless isOfficer $ throwError err403 { errBody = "Solo la directiva puede crear elecciones" }
      let artistKey = toSqlKey artistId
      mClub <- getBy (UniqueFanClubArtist artistKey)
      case mClub of
        Nothing -> throwError err404 { errBody = "Club no encontrado" }
        Just (Entity cid _) -> do
          now <- liftIO getCurrentTime
          mEid <- insertUnique FanClubElection
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

    createCandidacy :: Int64 -> Int64 -> FanClubCreateCandidacyReq -> Servant.Handler FanClubCandidacyDTO
    createCandidacy artistId electionId req = runDB $ do
      let eid = toSqlKey electionId
      mElection <- selectFirst [FanClubElectionId ==. eid] []
      case mElection of
        Nothing -> throwError err404 { errBody = "Elección no encontrada" }
        Just (Entity _ election) -> do
          now <- liftIO getCurrentTime
          let role = parseOfficerRole (fccrRole req)
          mCid <- insertUnique FanClubCandidacy
            { fanClubCandidacyElectionId = eid
            , fanClubCandidacyFanPartyId = auPartyId user
            , fanClubCandidacyRole = role
            , fanClubCandidacyManifesto = fccrManifesto req
            , fanClubCandidacyCreatedAt = now
            }
          case mCid of
            Nothing -> throwError err400 { errBody = "Ya estás postulado para este cargo" }
            Just cid' -> do
              author <- getAuthorDTO (auPartyId user)
              pure $ FanClubCandidacyDTO
                { fccCandidacyId = fromSqlKey cid'
                , fccFanId = fromSqlKey (auPartyId user)
                , fccFanName = spDisplayName author
                , fccAvatarUrl = spAvatarUrl author
                , fccRole = fccrRole req
                , fccManifesto = fccrManifesto req
                , fccVoteCount = 0
                }

    castVote :: Int64 -> Int64 -> FanClubVoteReq -> Servant.Handler NoContent
    castVote artistId electionId req = runDB $ do
      let eid = toSqlKey electionId
      mElection <- selectFirst [FanClubElectionId ==. eid] []
      case mElection of
        Nothing -> throwError err404 { errBody = "Elección no encontrada" }
        Just (Entity _ election) -> do
          now <- liftIO getCurrentTime
          forM_ (fcvCandidacyIds req) $ \candId -> do
            let cKey = toSqlKey candId
            mCand <- selectFirst [FanClubCandidacyId ==. cKey] []
            case mCand of
              Nothing -> pure ()
              Just (Entity _ cand) -> do
                void $ insertUnique FanClubVote
                  { fanClubVoteElectionId = eid
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
  officers <- selectList [FanClubOfficerClubId ==. cid] [Asc FanClubOfficerRole]
  forM officers $ \(Entity _ o) -> do
    mParty <- selectFirst [M.PartyId ==. fanClubOfficerFanPartyId o] []
    mProfile <- selectFirst [FanProfileFanPartyId ==. fanClubOfficerFanPartyId o] []
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
  mProfile <- selectFirst [FanProfileFanPartyId ==. pid] []
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

parseOfficerRole :: Text -> FanClubOfficerRole
parseOfficerRole t = case T.toLower (T.strip t) of
  "presidente" -> President
  "vicepresidente" -> VicePresident
  "secretario" -> Secretary
  "tesorero" -> Treasurer
  "coordinador" -> Coordinator
  _ -> Coordinator

checkIsOfficer :: Int64 -> PartyId -> SqlPersistT IO Bool
checkIsOfficer artistId fanId = do
  let artistKey = toSqlKey artistId
  mClub <- getBy (UniqueFanClubArtist artistKey)
  case mClub of
    Nothing -> pure False
    Just (Entity cid _) -> do
      mOfficer <- selectFirst
        [FanClubOfficerClubId ==. cid, FanClubOfficerFanPartyId ==. fanId] []
      pure (isJust mOfficer)
