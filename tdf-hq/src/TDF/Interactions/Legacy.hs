{-# LANGUAGE OverloadedStrings #-}
module TDF.Interactions.Legacy
  ( activated, visible, reactionSummary, momentPreview, momentPreviews, commentCount, postReactionCounts ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeStrict', encode)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, fromSqlKey)
import Database.Persist.SqlBackend (getRDBMS)
import TDF.DTO (ReactionSummaryDTO)
import TDF.DTO.SocialEventsDTO (EventMomentReactionDTO, EventMomentCommentDTO)
import TDF.Models (PartyId)

-- Installation alone is not cutover. Once activated, rollback pauses commands;
-- readers continue enforcing canonical blocking and moderation forever.
activated :: SqlPersistT IO Bool
activated = do
  backend <- getRDBMS
  if backend /= "postgresql" then pure False else do
    installed <- rawSql "SELECT to_regclass('interaction_runtime') IS NOT NULL" [] :: SqlPersistT IO [Single Bool]
    if installed /= [Single True] then pure False else do
      rows <- rawSql "SELECT activated_once FROM interaction_runtime WHERE singleton" [] :: SqlPersistT IO [Single Bool]
      pure (rows == [Single True])

visible :: PartyId -> Text -> Text -> SqlPersistT IO Bool
visible actor kind entity = do
  ready <- activated
  if not ready then pure True else do
    rows <- rawSql "SELECT interaction_resolve(?,?,?) IS NOT NULL" [PersistText kind,PersistText entity,PersistInt64 (fromSqlKey actor)]
      :: SqlPersistT IO [Single Bool]
    pure (rows == [Single True])

readJSON :: FromJSON a => Text -> [PersistValue] -> SqlPersistT IO a
readJSON sql parameters = do
  rows <- rawSql sql parameters :: SqlPersistT IO [Single Text]
  case rows of
    [Single body] -> case eitherDecodeStrict' (TE.encodeUtf8 body) of
      Right value -> pure value
      Left _ -> failure
    _ -> failure
  where failure = liftIO (ioError (userError "Invalid canonical interaction compatibility response"))

reactionSummary :: PartyId -> Text -> Text -> SqlPersistT IO (Maybe ReactionSummaryDTO)
reactionSummary actor kind entity = do
  ready <- activated
  if not ready then pure Nothing else Just <$> readJSON "SELECT interaction_legacy_reaction_summary(?,?,?)::text"
    [PersistInt64 (fromSqlKey actor),PersistText kind,PersistText entity]

momentPreview :: Text -> Text -> SqlPersistT IO (Maybe ([EventMomentReactionDTO],[EventMomentCommentDTO]))
momentPreview actor entity = do
  ready <- activated
  if not ready then pure Nothing else Just <$> readJSON
    "SELECT jsonb_build_array(value->'reactions',value->'comments')::text FROM (SELECT interaction_legacy_moment(?::bigint,?) value) x"
    [PersistText actor,PersistText entity]

-- One database round trip for the legacy array endpoint. Its source list has
-- no pagination contract; only each discussion preview is deliberately bounded.
-- Omit inaccessible moments before returning any preview to the caller.
momentPreviews :: Text -> [Text]
  -> SqlPersistT IO (Map.Map Text ([EventMomentReactionDTO],[EventMomentCommentDTO]))
momentPreviews actor entities = readJSON
  "SELECT coalesce(jsonb_object_agg(entity,jsonb_build_array(value->'reactions',value->'comments')),'{}')::text FROM jsonb_array_elements_text(?::jsonb) candidate(entity) CROSS JOIN LATERAL (SELECT interaction_legacy_moment(?::bigint,entity) value) preview WHERE interaction_resolve('event_moment',entity,?::bigint) IS NOT NULL"
  [PersistText (TE.decodeUtf8 (BL.toStrict (encode entities))),PersistText actor,PersistText actor]

commentCount :: PartyId -> Text -> Text -> SqlPersistT IO (Maybe Int)
commentCount actor kind entity = do
  ready <- activated
  if not ready then pure Nothing else do
    rows <- rawSql "SELECT coalesce((interaction_summary(?,?,?)->>'commentCount')::bigint,0)"
      [PersistInt64 (fromSqlKey actor),PersistText kind,PersistText entity] :: SqlPersistT IO [Single Int64]
    pure (Just (case rows of [Single n] -> fromIntegral n; _ -> 0))

-- Shared by legacy discovery ranking; never count frozen source reaction rows
-- after cutover. Batch the candidate IDs and apply the same viewer block rules.
postReactionCounts :: PartyId -> [Int64] -> SqlPersistT IO (Maybe (Map.Map Int64 Int))
postReactionCounts actor ids = do
  ready <- activated
  if not ready then pure Nothing else do
    rows <- rawSql
      "SELECT t.entity_key::bigint,count(*)::bigint FROM interaction_target t JOIN interaction_reaction r ON r.target_id=t.id JOIN jsonb_array_elements_text(?::jsonb) candidate(id) ON candidate.id=t.entity_key WHERE t.entity_kind='club_post' AND interaction_actor_live(r.actor_id) AND NOT interaction_blocked(?,r.actor_id) AND interaction_target_context(t.id,?) IS NOT NULL GROUP BY t.entity_key"
      [PersistText (TE.decodeUtf8 (BL.toStrict (encode ids))),PersistInt64 (fromSqlKey actor),PersistInt64 (fromSqlKey actor)]
      :: SqlPersistT IO [(Single Int64,Single Int64)]
    pure (Just (Map.fromList [(ident,fromIntegral total) | (Single ident,Single total) <- rows]))
