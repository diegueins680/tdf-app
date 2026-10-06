{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TDF.Social.RelationshipWrites (addFriend, removeFriend, exchangeVCard) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, utctDay)
import Database.Persist
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool, toSqlKey)
import Database.Persist.SqlBackend (getRDBMS)
import Database.PostgreSQL.Simple (SqlError(..))
import Servant
import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.DTO (PartyFollowDTO(..), VCardExchangeRequest(..))
import TDF.Models
import TDF.Social.Session (SessionAccess(..), withCurrentSession)

type WriteM = ReaderT Env Handler

addFriend :: AuthedUser -> Int64 -> WriteM [PartyFollowDTO]
addFriend user target = add user target False

exchangeVCard :: AuthedUser -> VCardExchangeRequest -> WriteM [PartyFollowDTO]
exchangeVCard user request = add user (vcerPartyId request) True

add :: AuthedUser -> Int64 -> Bool -> WriteM [PartyFollowDTO]
add user target nfc = do
  now <- liftIO getCurrentTime
  write user target $ do
    let actor = auPartyId user
        peer = toSqlKey target
    names <- rawSql "SELECT id,display_name FROM party WHERE id IN (?,?)"
      [PersistInt64 (fromSqlKey actor),PersistInt64 target]
      :: SqlPersistT IO [(Single Int64,Single Text)]
    let name :: PartyId -> Maybe Text
        name key = lookup (fromSqlKey key) [(i,n) | (Single i,Single n) <- names]
    case name peer of
      Nothing -> pure (Left err404 {errBody="Party not found"})
      Just _ -> do
        ab <- upsert (PartyFollow actor peer nfc now) [PartyFollowViaNfc =. nfc]
        ba <- upsert (PartyFollow peer actor nfc now) [PartyFollowViaNfc =. nfc]
        let dto (Entity _ edge) = PartyFollowDTO
              (fromSqlKey (partyFollowFollowerPartyId edge))
              (fromSqlKey (partyFollowFollowingPartyId edge))
              (name (partyFollowFollowerPartyId edge)) (name (partyFollowFollowingPartyId edge))
              (partyFollowViaNfc edge) (utctDay (partyFollowCreatedAt edge))
        pure (Right (map dto (if nfc then [ab,ba] else [ab])))

removeFriend :: AuthedUser -> Int64 -> WriteM NoContent
removeFriend user target = write user target $ do
  let actor = auPartyId user
      peer = toSqlKey target
  deleteBy (UniquePartyFollow actor peer)
  deleteBy (UniquePartyFollow peer actor)
  pure (Right NoContent)

write :: AuthedUser -> Int64 -> SqlPersistT IO (Either ServerError a) -> WriteM a
write user target action = do
  when (target <= 0 || target == fromSqlKey (auPartyId user)) $
    throwError err400 {errBody="Selecciona otra persona para actualizar la relacion."}
  pool <- asks envPool
  outcome <- liftIO $ try $ runSqlPool transaction pool
  case outcome of
    Left (err :: SqlError)
      | sqlState err `elem` ["40001","40P01"] -> throwError err503
          {errHeaders=[("Retry-After","1"),("Cache-Control","no-store")]}
      | sqlState err == "0A000" -> throwError err503
      | otherwise -> throwError err500
    Right result -> either throwError pure result
  where
    transaction = do
      available <- availability
      case available of
        Just False -> action
        Nothing -> pure (Left err503 {errBody="Relaciones temporalmente no disponibles."})
        Just True -> do
          [Single permitted] <- rawSql "SELECT social_v2_lock_legacy_write(?,?)"
            [PersistInt64 (fromSqlKey (auPartyId user)),PersistInt64 target]
          checked <- withCurrentSession (WriteSession (Just (toSqlKey target))) user $
            if permitted then action else pure (Left retired)
          pure (checked >>= id)
    retired = err410
      { errBody="Esta accion de amistad ya no esta disponible. Usa Seguir o Conectar en la nueva vista social."
      , errHeaders=[("Cache-Control","no-store")]
      }

-- Schema availability is not a feature toggle. A partially installed foundation
-- fails closed, and pausing the new UI never reopens a governed legacy writer.
availability :: SqlPersistT IO (Maybe Bool)
availability = do
  backend <- T.toCaseFold <$> getRDBMS
  if backend /= "postgresql" then pure (Just False) else do
    [Single present] <- rawSql
      "SELECT to_regprocedure('social_v2_lock_legacy_write(bigint,bigint)') IS NOT NULL AND to_regprocedure('social_v2_dm_required(bigint,bigint)') IS NOT NULL" []
    if present then pure (Just True) else do
      [Single foundation] <- rawSql "SELECT to_regclass('social_v2_runtime') IS NOT NULL" []
      pure $ if foundation then Nothing else Just False
