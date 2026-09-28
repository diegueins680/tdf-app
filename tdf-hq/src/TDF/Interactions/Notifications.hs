{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TDF.Interactions.Notifications
  ( notificationRows, notificationUnreadCount, startInteractionNotifications ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, SomeAsyncException, fromException, throwIO, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Database.Persist (Entity, PersistValue(..), selectList, count, Filter, SelectOpt(..), (==.))
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, fromSqlKey, toSqlKey, runSqlPool)
import Database.Persist.SqlBackend (getRDBMS)
import Servant (ServerError)
import TDF.Auth (AuthedUser(..))
import TDF.DB (ConnectionPool)
import qualified TDF.LogBuffer as Log
import TDF.Models
import TDF.Social.Session (SessionAccess(..), withCurrentSession)

available :: SqlPersistT IO Bool
available = do
  backend <- getRDBMS
  if backend /= "postgresql" then pure False else do
    rows <- rawSql "SELECT to_regclass('interaction_notification') IS NOT NULL, to_regprocedure('interaction_notification_visible(bigint,bigint)') IS NOT NULL" []
      :: SqlPersistT IO [(Single Bool,Single Bool)]
    case rows of
      [(Single _,Single True)] -> pure True
      [(Single True,Single False)] -> liftIO (ioError (userError "Incomplete interaction notification authority"))
      _ -> pure False

notificationRows :: AuthedUser -> Bool -> Maybe Int64 -> SqlPersistT IO (Either ServerError [Entity Notification])
notificationRows user unread ident = withCurrentSession ReadSession user $ do
  canonical <- available
  if canonical then rawSql
    "SELECT ?? FROM notification WHERE recipient_party_id=? AND (NOT ? OR NOT is_read) AND (?::bigint IS NULL OR id=?) AND interaction_notification_visible(id,?) ORDER BY created_at DESC,id DESC LIMIT 50"
    [actor, PersistBool unread, maybe PersistNull PersistInt64 ident, maybe PersistNull PersistInt64 ident, actor]
  else selectList filters [Desc NotificationCreatedAt,LimitTo 50]
  where
    actor = PersistInt64 (fromSqlKey (auPartyId user))
    filters :: [Filter Notification]
    filters = [NotificationRecipientPartyId ==. auPartyId user]
      ++ [NotificationIsRead ==. False | unread]
      ++ maybe [] (\key -> [NotificationId ==. toSqlKey key]) ident

notificationUnreadCount :: AuthedUser -> SqlPersistT IO (Either ServerError Int)
notificationUnreadCount user = withCurrentSession ReadSession user $ do
  canonical <- available
  if canonical then do
    rows <- rawSql "SELECT count(*)::bigint FROM notification WHERE recipient_party_id=? AND NOT is_read AND interaction_notification_visible(id,?)"
      [actor,actor] :: SqlPersistT IO [Single Int64]
    pure $ case rows of [Single n] -> fromIntegral n; _ -> 0
  else count [NotificationRecipientPartyId ==. auPartyId user, NotificationIsRead ==. False]
  where actor = PersistInt64 (fromSqlKey (auPartyId user))

startInteractionNotifications :: ConnectionPool -> IO ()
startInteractionNotifications pool = void . forkIO . forever $ do
  outcome <- try $ runSqlPool (do
    ready <- available
    if ready then void (rawSql "SELECT interaction_dispatch_events(20)" [] :: SqlPersistT IO [Single Int]) else pure ()) pool
  case outcome of
    Right () -> pure ()
    Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> Log.addLog Log.LogError "[Interactions] Notification batch failed; retained for retry"
  threadDelay (10 * 1000000)
