{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TDF.Social.RelationshipReads (relationshipList, suggestions) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (RawSql, Single(..), SqlPersistT, fromSqlKey, rawSql, runSqlPool)
import Database.Persist.SqlBackend (getRDBMS)
import Database.PostgreSQL.Simple (SqlError(..))
import Servant
import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.DTO (PartyFollowDTO(..), SuggestedFriendDTO(..))
import TDF.Social.Session (SessionAccess(..), withCurrentSession)

type ReadM = ReaderT Env Handler

type RelationshipRow = (Single Int64,Single Int64,Single Text,Single Text,Single Bool,Single Day)

relationshipList :: AuthedUser -> Text -> ReadM [PartyFollowDTO] -> ReadM [PartyFollowDTO]
relationshipList user kind legacy = choose legacy $ do
  rows <- currentRows user "SELECT * FROM social_v2_relationship_rows(?,?)" [PersistText kind]
    :: ReadM [RelationshipRow]
  pure [PartyFollowDTO a b (Just nameA) (Just nameB) nfc day |
    (Single a,Single b,Single nameA,Single nameB,Single nfc,Single day) <- rows]

suggestions :: AuthedUser -> ReadM [SuggestedFriendDTO] -> ReadM [SuggestedFriendDTO]
suggestions user legacy = choose legacy $ do
  rows <- currentRows user "SELECT * FROM social_v2_legacy_suggestions(?)" []
    :: ReadM [(Single Int64,Single Int)]
  pure [SuggestedFriendDTO target count | (Single target,Single count) <- rows]

currentRows :: RawSql a => AuthedUser -> Text -> [PersistValue] -> ReadM [a]
currentRows user statement params = do
  pool <- asks envPool
  outcome <- liftIO $ try $ runSqlPool (withCurrentSession ReadSession user $
    rawSql statement (PersistInt64 (fromSqlKey (auPartyId user)):params)) pool
  case outcome of
    Left (err :: SqlError)
      | sqlState err `elem` ["40001","40P01"] -> throwError err503 {errHeaders=[("Retry-After","1")]}
      | otherwise -> throwError err500
    Right result -> either throwError pure result

-- Current-session and domain checks remain in one transaction. As with profile
-- reads, never choose an old handler from a temporary absence of authority rows.
choose :: ReadM a -> ReadM a -> ReadM a
choose legacy repaired = do
  pool <- asks envPool
  available <- liftIO $ runSqlPool availability pool
  case available of
    Just True -> repaired
    Just False -> legacy
    Nothing -> throwError err503 {errBody="Relaciones temporalmente no disponibles."}

availability :: SqlPersistT IO (Maybe Bool)
availability = do
  backend <- T.toCaseFold <$> getRDBMS
  if backend /= "postgresql" then pure (Just False) else do
    [Single present] <- rawSql
      "SELECT to_regprocedure('social_v2_relationship_rows(bigint,text)') IS NOT NULL AND to_regprocedure('social_v2_legacy_suggestions(bigint)') IS NOT NULL AND to_regprocedure('social_v2_legacy_suggestions_enabled()') IS NOT NULL AND to_regprocedure('social_v2_profile_eligible(bigint,bigint[])') IS NOT NULL" []
    if present then pure (Just True) else do
      [Single foundation] <- rawSql "SELECT to_regclass('social_v2_runtime') IS NOT NULL" []
      pure $ if foundation then Nothing else Just False
