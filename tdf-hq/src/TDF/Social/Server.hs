{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.Social.Server (socialV2Server) where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (Value(..), eitherDecodeStrict')
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, toSqlKey, rawSql, runSqlPool)
import Servant
import System.Environment (lookupEnv)
import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.Social.API
import TDF.Social.Session

-- Both gates default closed: process flag plus database runtime switch.
-- Account-only pilot: managed entities require richer principal context first.
socialV2Server
  :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => AuthedUser -> ServerT SocialV2API m
socialV2Server user = me :<|> relationship :<|> mutate :<|> preferences :<|> following :<|> discover
  where
    actor = PersistInt64 (fromSqlKey (auPartyId user))
    gate = do
      enabled <- liftIO $ lookupEnv "SOCIAL_V2_ENABLED"
      unless (enabled == Just "true") $ throwError err404
    query = queryWith ReadSession
    queryWith access sql args = do
      gate
      pool <- asks envPool
      result <- liftIO $ runSqlPool
        (withSocialSession access user (rawSql sql args :: SqlPersistT IO [Single Text])) pool
      rows <- either throwError pure result
      case rows of
        [Single body] -> case eitherDecodeStrict' (TE.encodeUtf8 body) of
          Right result -> checkResult result
          Left _ -> throwError err500 {errBody = "Invalid social response"}
        _ -> throwError err500 {errBody = "Missing social response"}
    checkResult result@(Object value) = case KM.lookup "error" value of
      Nothing -> pure result
      Just (String "disabled") -> throwError err404
      Just (String "invalid") -> throwError err400
      Just (String "unavailable") -> throwError err404
      Just (String "rate_limited") -> throwError err429
      Just _ -> throwError err409
    checkResult _ = throwError err500 {errBody = "Invalid social response"}
    validateTarget target = when (target <= 0 || target == fromSqlKey (auPartyId user)) $
      throwError err400
    limitValue limit = do
      let n = fromMaybe 20 limit
      when (n < 1 || n > 50) $ throwError err400
      pure (PersistInt64 (fromIntegral n))
    me = query "SELECT social_v2_me(?)::text" [actor]
    relationship target = do
      gate
      validateTarget target
      query
        "SELECT social_v2_relationship(?,?)::text"
        [actor,PersistInt64 target]
    mutate target (Command op revision key) = do
      gate
      validateTarget target
      queryWith (WriteSession (Just (toSqlKey target))) "SELECT social_v2_mutate(?,?,?,?,?)::text"
        [actor,PersistInt64 target,PersistText op,PersistInt64 revision,PersistText key]
    preferences (Preferences visible personalized revision) =
      queryWith (WriteSession Nothing) "SELECT social_v2_preferences(?,?,?,?)::text"
        [actor,PersistBool visible,PersistBool personalized,PersistInt64 revision]
    following cursor limit = do
      gate
      size <- limitValue limit
      when (maybe False (<=0) cursor) $ throwError err400
      -- Commit the serialized publication batch BEFORE the read snapshot. IDs
      -- allocated in an uncommitted transaction cannot appear behind its cursor.
      when (cursor == Nothing) $ do
        pool <- asks envPool
        result <- liftIO $ runSqlPool
          (withSocialSession (WriteSession Nothing) user
            (rawSql "SELECT social_v2_publish_batch()" [] :: SqlPersistT IO [Single Int64])) pool
        _ <- either throwError pure result
        pure ()
      query "SELECT social_v2_feed(?,?,?)::text" [actor,maybe PersistNull PersistInt64 cursor,size]
    discover limit = do
      size <- limitValue limit
      query "SELECT social_v2_discover(?,?)::text" [actor,size]
