{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TDF.Social.FanEffects (followArtist, unfollowArtist, requireFanAccess) where

import Control.Exception (try)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Database.Persist
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool, toSqlKey)
import Database.Persist.SqlBackend (getRDBMS)
import Database.PostgreSQL.Simple (SqlError(..))
import Servant
import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.DTO (FanFollowDTO(..))
import TDF.Models
import TDF.Social.Session (SessionAccess(..), withCurrentSession)

type FanM = ReaderT Env Handler

-- Same Fan/Customer and coherent-role rule used by the existing fan routes.
-- Role-grant revocation remains the existing auth subsystem's responsibility.
requireFanAccess :: AuthedUser -> FanM ()
requireFanAccess user = do
  unless (any (`elem` auRoles user) [Fan,Customer]) $
    throwError err403 {errBody="Fan access required"}
  unless (fromSqlKey (auPartyId user)>0 && length (auRoles user)==length (nub (auRoles user))) $
    throwError err403 {errBody="Fan access requires coherent role grants"}

followArtist :: AuthedUser -> Int64 -> FanM FanFollowDTO
followArtist user target = do
  validateTarget user target "No puedes seguirte a ti mismo"
  runFanTransaction user artist $ \mode -> case mode of
    Nothing -> subscribe actor artist True
    Just legacy -> do
      [Single eligible] <- rawSql
        "SELECT EXISTS(SELECT 1 FROM social_v2_profile_eligible(?,ARRAY[?::bigint]))"
        [PersistInt64 (fromSqlKey actor),PersistInt64 target]
      if eligible then subscribe actor artist legacy else pure (Left err404)
  where
    actor = auPartyId user
    artist = toSqlKey target

-- Removing one's own subscription needs no target visibility or club consent.
-- Use the same account locks as follow so response construction cannot race delete.
unfollowArtist :: AuthedUser -> Int64 -> FanM NoContent
unfollowArtist user target = do
  validateTarget user target "No puedes dejar de seguirte a ti mismo"
  runFanTransaction user artist $ \_ -> do
    existing <- getBy (UniqueFanFollow actor artist)
    when (isJust existing) $ do
      now <- liftIO getCurrentTime
      deleteBy (UniqueFanFollow actor artist)
      insert_ (EngagementEvent (Just actor) (Just artist) "artist"
        (Just (fromIntegral target)) "unfollow" Nothing now)
    pure (Right NoContent)
  where
    actor = auPartyId user
    artist = toSqlKey target

validateTarget :: AuthedUser -> Int64 -> BL.ByteString -> FanM ()
validateTarget user target selfMessage = do
  requireFanAccess user
  when (target<=0) $ throwError err400 {errBody="Invalid artist id"}
  when (target==fromSqlKey (auPartyId user)) $ throwError err400
    {errBody=selfMessage}

runFanTransaction
  :: AuthedUser -> PartyId -> (Maybe Bool -> SqlPersistT IO (Either ServerError a)) -> FanM a
runFanTransaction user artist action = do
  pool <- asks envPool
  result <- liftIO $ try $ runSqlPool transaction pool
  case result of
    Left (err :: SqlError)
      | sqlState err `elem` ["40001","40P01"] -> throwError err503
          {errHeaders=[("Retry-After","1")]}
      | sqlState err == "0A000" -> throwError err503
      | otherwise -> throwError err500
    Right outcome -> either throwError pure outcome
  where
    transaction = do
      available <- availability
      case available of
        Just False -> action Nothing
        Nothing -> pure (Left err503 {errBody="Seguimientos temporalmente no disponibles."})
        Just True -> do
          [Single legacy] <- rawSql "SELECT social_v2_lock_fan_effects()" []
          checked <- withCurrentSession (WriteSession (Just artist)) user (action (Just legacy))
          pure (checked >>= id)

subscribe :: PartyId -> PartyId -> Bool -> SqlPersistT IO (Either ServerError FanFollowDTO)
subscribe actor artist legacy = do
  rows <- rawSql
    "SELECT p.display_name,a.hero_image_url,a.spotify_url,a.youtube_url FROM artist_profile a JOIN party p ON p.id=a.artist_party_id WHERE a.artist_party_id=?"
    [PersistInt64 (fromSqlKey artist)]
    :: SqlPersistT IO [(Single Text,Single (Maybe Text),Single (Maybe Text),Single (Maybe Text))]
  case rows of
    [(Single name,Single hero,Single spotify,Single youtube)] -> do
      now <- liftIO getCurrentTime
      inserted <- insertUnique (FanFollow actor artist now)
      when (isJust inserted) $ do
        insert_ (EngagementEvent (Just actor) (Just artist) "artist"
          (Just (fromIntegral (fromSqlKey artist))) "follow" Nothing now)
        when legacy $ notifyArtist actor artist now
      when legacy $ populateClub actor artist now
      Just (Entity _ follow) <- getBy (UniqueFanFollow actor artist)
      pure (Right (FanFollowDTO (fromSqlKey artist) name hero spotify youtube
        (utctDay (fanFollowCreatedAt follow))))
    _ -> pure (Left err404 {errBody="Artist profile not found"})

notifyArtist :: PartyId -> PartyId -> UTCTime -> SqlPersistT IO ()
notifyArtist actor artist now = do
  rows <- rawSql "SELECT display_name FROM party WHERE id=?"
    [PersistInt64 (fromSqlKey actor)] :: SqlPersistT IO [Single Text]
  let name = case rows of [Single value] -> value; _ -> "Un fan"
  insert_ (Notification artist "artist_liked" "Nuevo fan"
    (name<>" empezó a seguir tu perfil.") (Just "artist")
    (Just (fromIntegral (fromSqlKey artist))) False now)

-- Called only in the never-activated, no-canonical-state compatibility stage.
-- Do not enumerate members at all once this side effect is retired.
populateClub :: PartyId -> PartyId -> UTCTime -> SqlPersistT IO ()
populateClub actor artist now = do
  clubs <- rawSql "SELECT id FROM fan_club WHERE artist_party_id=?"
    [PersistInt64 (fromSqlKey artist)] :: SqlPersistT IO [Single Int64]
  case clubs of
    [Single clubId] -> do
      members <- rawSql "SELECT party_id FROM fan_club_member_profile WHERE club_id=? AND party_id<>?"
        [PersistInt64 clubId,PersistInt64 (fromSqlKey actor)] :: SqlPersistT IO [Single Int64]
      avatars <- rawSql "SELECT avatar_url FROM fan_profile WHERE fan_party_id=?"
        [PersistInt64 (fromSqlKey actor)] :: SqlPersistT IO [Single (Maybe Text)]
      let avatar = case avatars of [Single value] -> value; _ -> Nothing
      void $ insertUnique (FanClubMemberProfile actor (toSqlKey clubId) Nothing Nothing avatar now)
      forM_ members $ \(Single member) -> do
        void $ insertUnique (PartyFollow actor (toSqlKey member) False now)
        void $ insertUnique (PartyFollow (toSqlKey member) actor False now)
    _ -> pure ()

availability :: SqlPersistT IO (Maybe Bool)
availability = do
  backend <- T.toCaseFold <$> getRDBMS
  if backend /= "postgresql" then pure (Just False) else do
    [Single present] <- rawSql
      "SELECT to_regprocedure('social_v2_lock_fan_effects()') IS NOT NULL AND to_regprocedure('social_v2_legacy_suggestions_enabled()') IS NOT NULL AND to_regprocedure('social_v2_profile_eligible(bigint,bigint[])') IS NOT NULL" []
    if present then pure (Just True) else do
      [Single foundation] <- rawSql "SELECT to_regclass('social_v2_runtime') IS NOT NULL" []
      pure $ if foundation then Nothing else Just False
