{-# LANGUAGE OverloadedStrings #-}
-- | HTTP handlers for MSG-DELIVERY-001: device push subscriptions, chat reminder
-- channel preferences and recipient read receipts. Delivery itself lives in
-- "TDF.Chat.Delivery".
module TDF.Server.NotificationDelivery
  ( notificationDeliveryServer
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Servant

import qualified TDF.API.NotificationDelivery as API
import TDF.Auth (AuthedUser(..))
import TDF.Chat.Delivery (DeliverySettings(..), readDeliverySettings, validExpoPushToken)
import TDF.DB (Env(..))

type AppM = ReaderT Env Handler

notificationDeliveryServer :: AuthedUser -> ServerT API.NotificationDeliveryAPI AppM
notificationDeliveryServer user =
       registerPush user
  :<|> revokePush user
  :<|> getChannels user
  :<|> putChannels user
  :<|> markThreadRead user

runDB :: SqlPersistT IO a -> AppM a
runDB action = asks envPool >>= liftIO . runSqlPool action

actor :: AuthedUser -> PersistValue
actor = PersistInt64 . fromSqlKey . auPartyId

registerPush :: AuthedUser -> API.PushSubscriptionRegister -> AppM NoContent
registerPush user request = do
  let tokenValue = T.strip (API.token request)
      platformValue = T.toLower (T.strip (API.platform request))
  unless (T.toLower (T.strip (API.provider request)) == "expo") $
    throwError err422 {errBody = "Unsupported push provider"}
  unless (platformValue `elem` ["ios", "android"]) $
    throwError err422 {errBody = "Unsupported push platform"}
  unless (validExpoPushToken tokenValue) $
    throwError err422 {errBody = "Invalid push token"}
  settings <- liftIO readDeliverySettings
  key <- maybe (throwError err503 {errBody = "Push notifications are not configured"}) pure
    (dsPushTokenKey settings)
  runDB $ rawExecute
    "INSERT INTO notification_push_subscription(party_id,provider,platform,token_digest,encrypted_token) \
    \VALUES (?, 'expo', ?, encode(digest(?::text,'sha256'),'hex'), pgp_sym_encrypt(?::text, ?::text)) \
    \ON CONFLICT (provider, token_digest) DO UPDATE SET party_id=EXCLUDED.party_id, \
    \platform=EXCLUDED.platform, encrypted_token=EXCLUDED.encrypted_token, active=true, \
    \last_failure_code=NULL, updated_at=now()"
    [actor user, PersistText platformValue, PersistText tokenValue, PersistText tokenValue, PersistText key]
  pure NoContent

revokePush :: AuthedUser -> API.PushSubscriptionRevoke -> AppM NoContent
revokePush user request = do
  let tokenValue = T.strip (API.revokeToken request)
  unless (validExpoPushToken tokenValue) $
    throwError err422 {errBody = "Invalid push token"}
  runDB $ rawExecute
    "UPDATE notification_push_subscription SET active=false, updated_at=now() \
    \WHERE provider='expo' AND token_digest=encode(digest(?::text,'sha256'),'hex') AND party_id=?"
    [PersistText tokenValue, actor user]
  pure NoContent

getChannels :: AuthedUser -> AppM API.NotificationChannelsDTO
getChannels user = runDB (loadChannels user)

loadChannels :: AuthedUser -> SqlPersistT IO API.NotificationChannelsDTO
loadChannels user = do
  rows <- rawSql
    "SELECT channel, enabled FROM notification_channel_preference WHERE party_id=? AND category='chat'"
    [actor user] :: SqlPersistT IO [(Single Text, Single Bool)]
  let enabled channel = fromMaybe True (lookup channel [(c, e) | (Single c, Single e) <- rows])
  pure API.NotificationChannelsDTO {API.email = enabled "email", API.push = enabled "push"}

putChannels :: AuthedUser -> API.NotificationChannelsDTO -> AppM API.NotificationChannelsDTO
putChannels user request = runDB $ do
  forM_ [("email", API.email request), ("push", API.push request)] $ \(channel, enabled) ->
    rawExecute
      "INSERT INTO notification_channel_preference(party_id,category,channel,enabled,updated_at) \
      \VALUES (?, 'chat', ?, ?, now()) ON CONFLICT (party_id,category,channel) \
      \DO UPDATE SET enabled=EXCLUDED.enabled, updated_at=now()"
      [actor user, PersistText channel, PersistBool enabled]
  loadChannels user

markThreadRead :: AuthedUser -> Int64 -> API.ChatThreadReadRequest -> AppM NoContent
markThreadRead user threadId request = do
  when (threadId <= 0) $ throwError err400 {errBody = "threadId must be a positive integer"}
  when (API.lastMessageId request <= 0) $ throwError err400 {errBody = "lastMessageId must be a positive integer"}
  rows <- runDB $ rawSql "SELECT chat_mark_thread_read(?, ?, ?)"
    [actor user, PersistInt64 threadId, PersistInt64 (API.lastMessageId request)]
  case rows of
    [Single True] -> pure NoContent
    _ -> throwError err404
