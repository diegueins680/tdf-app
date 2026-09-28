{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Server.RecordsIngestion (recordsIngestionServer) where

import Control.Exception (try)
import Control.Monad (unless)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (SqlError(..))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime, addUTCTime)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, rawExecute, runSqlPool, fromSqlKey)
import TDF.API.RecordsIngestion
import Servant
import System.Environment (lookupEnv)
import TDF.Auth (AuthedUser(..), hasStrictAdminAccess, hasModuleAccess, ModuleAccess(ModuleAdmin))
import TDF.DB (Env(..), sharedTlsManager)
import TDF.Services.RecordsIngestion (runRecordsIngestion, recordsIngestionSlot)
import qualified TDF.Services.YouTube as Y

recordsIngestionServer :: (MonadReader Env m, MonadIO m, MonadError ServerError m)
  => AuthedUser -> ServerT RecordsIngestionAPI m
recordsIngestionServer user = overview :<|> saveSource :<|> control :<|> runNow
  where
    requireAdmin = unless (hasStrictAdminAccess user && hasModuleAccess ModuleAdmin user) (throwError err403)
    db action = do
      pool <- asks envPool
      result <- liftIO (try (runSqlPool action pool))
      case result of
        Right value -> pure value
        Left (err :: SqlError)
          | sqlState err == "55P03" -> throwError err409{errBody="Source is busy; retry after the current page"}
          | sqlState err `elem` ["23503","22P02"] -> throwError err400{errBody="Invalid linked identity"}
          | otherwise -> throwError err500{errBody="Ingestion configuration could not be saved"}
    actor = fromSqlKey (auPartyId user)
    nullable = maybe PersistNull PersistInt64
    asJson (Single value) = maybe Null id (decodeStrict' (TE.encodeUtf8 value))
    overview = do
      requireAdmin
      sources <- db (rawSql
        "SELECT jsonb_build_object('id',id,'channelId',external_user_id,'partyId',party_id,'artistProfileId',artist_profile_id,'configuration',records_ingestion,'lastSuccessAt',last_synced_at)::text FROM social_sync_account WHERE platform='youtube' ORDER BY id LIMIT 100" [] :: SqlPersistT IO [Single Text])
      runs <- db (rawSql
        "SELECT jsonb_build_object('id',id,'key',run_code,'dryRun',dry_run,'status',status,'startedAt',started_at,'completedAt',completed_at,'report',report::jsonb)::text FROM catalog_backfill_run WHERE candidate_revision='youtube-api-v1' ORDER BY started_at DESC LIMIT 100" [] :: SqlPersistT IO [Single Text])
      controls <- db (rawSql "SELECT enabled,interval_seconds FROM records_ingestion_control WHERE singleton" [] :: SqlPersistT IO [(Single Bool,Single Int)])
      now <- liftIO getCurrentTime
      let (on,seconds) = case controls of (Single value,Single interval):_ -> (value,interval); _ -> (False,3600)
      pure (object ["sources" .= map asJson sources,"runs" .= map asJson runs,"enabled" .= on,
        "intervalSeconds" .= seconds,"nextScheduledAt" .= (if on then Just (addUTCTime (fromIntegral seconds) (recordsIngestionSlot seconds now)) else Nothing),
        "timezone" .= ("UTC" :: Text)])
    saveSource SourceRequest{..} = do
      requireAdmin
      unless (T.length channelId==24 && "UC" `T.isPrefixOf` channelId
        && T.all (\c -> c `elem` (['a'..'z']++['A'..'Z']++['0'..'9']++"_-")) channelId
        && (not enabled || UUID.fromText collectionId /= Nothing)
        && maybe True (\v -> not (T.null (T.strip v)) && T.length v<=500) approvalReference
        && (not enabled || approvalReference/=Nothing)) $ throwError err400
      -- Disabling works during provider outages; adding/enabling must verify.
      if not enabled then do
        rows <- db $ do
          rows <- rawSql
            "INSERT INTO social_sync_account(platform,external_user_id,status,created_at,updated_at,records_ingestion) VALUES('youtube',?,'connected',now(),now(),jsonb_build_object('enabled',false)) ON CONFLICT(platform,external_user_id) DO UPDATE SET records_ingestion=jsonb_set(coalesce(social_sync_account.records_ingestion,'{}'),'{enabled}','false'),updated_at=now() RETURNING id"
            [PersistText channelId] :: SqlPersistT IO [Single Int64]
          auditDb "source_disabled" (object ["channelId" .= channelId])
          pure rows
        pure (object ["disabled" .= length rows])
      else do
        -- Verification also consumes the shared daily budget. Serialize the
        -- short reservation transaction and rate-limit each administrator.
        permitted <- db $ do
          _ <- rawSql "SELECT 1 FROM pg_advisory_xact_lock(20260920,0)" [] :: SqlPersistT IO [Single Int]
          recent <- rawSql
            "SELECT id FROM records_ingestion_admin_audit WHERE actor_id=? AND action='source_verification_requested' AND created_at>now()-interval '60 seconds' LIMIT 1"
            [PersistInt64 actor] :: SqlPersistT IO [Single Int64]
          if not (null recent) then pure False else do
            budget <- rawSql
              "INSERT INTO records_ingestion_quota(day,reserved_units) VALUES((now() AT TIME ZONE 'America/Los_Angeles')::date,3) ON CONFLICT(day) DO UPDATE SET reserved_units=records_ingestion_quota.reserved_units+3 WHERE records_ingestion_quota.reserved_units+3<=9000 RETURNING reserved_units"
              [] :: SqlPersistT IO [Single Int]
            if null budget then pure False else do
              auditDb "source_verification_requested" (object ["channelId" .= channelId])
              pure True
        unless permitted $ throwError err429{errBody="Verification rate or daily quota limit reached"}
        secret <- liftIO (maybe "" T.pack <$> lookupEnv "YOUTUBE_API_KEY")
        key <- either (const (throwError err503{errBody="YouTube credential unavailable"})) pure (Y.youTubeKey secret)
        result <- liftIO (Y.fetchChannel sharedTlsManager key channelId)
        channel <- either (const (throwError err502{errBody="YouTube channel verification failed"})) pure result
        now <- liftIO getCurrentTime
        let config = object ["enabled" .= enabled,"approvalReference" .= approvalReference,
              "approvedAt" .= now,"approvedBy" .= actor,"collectionId" .= collectionId,
              "channelTitle" .= Y.channelTitle channel,"uploadsPlaylistId" .= Y.uploadsPlaylistId channel]
        rows <- db $ do
          valid <- rawSql "SELECT id::text FROM editorial_collection WHERE id::text=? AND active AND collection_type='recording' AND workflow_state_id IN (SELECT id FROM workflow_state WHERE code='published' AND active)" [PersistText collectionId] :: SqlPersistT IO [Single Text]
          identities <- rawSql
            "SELECT true WHERE (?::bigint IS NULL OR EXISTS(SELECT 1 FROM party WHERE id=?)) AND (?::bigint IS NULL OR EXISTS(SELECT 1 FROM artist_profile WHERE id=? AND (?::bigint IS NULL OR artist_party_id=?)))"
            [nullable partyId,nullable partyId,nullable artistProfileId,nullable artistProfileId,nullable partyId,nullable partyId] :: SqlPersistT IO [Single Bool]
          if null valid || null identities then pure [] else do
            rows <- rawSql
              "INSERT INTO social_sync_account(platform,external_user_id,party_id,artist_profile_id,status,created_at,updated_at,records_ingestion) VALUES('youtube',?,?,?,'connected',now(),now(),?::jsonb) ON CONFLICT(platform,external_user_id) DO UPDATE SET records_ingestion=coalesce(social_sync_account.records_ingestion,'{}')||excluded.records_ingestion,party_id=excluded.party_id,artist_profile_id=excluded.artist_profile_id,updated_at=now() RETURNING id"
              [PersistText channelId,nullable partyId,nullable artistProfileId,PersistText (jsonText config)]
            auditDb "source_approved" (object ["channelId" .= channelId,"configuration" .= config])
            pure rows
        case rows of
          [Single ident] -> do
            pure (object ["id" .= (ident :: Int64),"configuration" .= config])
          _ -> throwError err400{errBody="Recording collection unavailable"}
    control ControlRequest{..} = do
      requireAdmin
      unless (intervalSeconds>=300 && intervalSeconds<=86400) $ throwError err400
      db $ do
        rawExecute "UPDATE records_ingestion_control SET enabled=?,interval_seconds=?,updated_at=now() WHERE singleton"
          [PersistBool running,PersistInt64 (fromIntegral intervalSeconds)]
        auditDb "control_changed" (object ["enabled" .= running,"intervalSeconds" .= intervalSeconds])
      overview
    runNow RunRequest{..} = do
      requireAdmin
      unless (sourceAccountId>0 && sourceAccountId<=2147483647 && not (T.null executionKey) && T.length executionKey<=150) $ throwError err400
      audit "manual_run_requested" (object ["sourceAccountId" .= sourceAccountId,"executionKey" .= executionKey,"reconciliation" .= reconciliation,"dryRun" .= dryRun])
      pool <- asks envPool
      liftIO $ runRecordsIngestion pool sourceAccountId executionKey reconciliation dryRun
    audit action value = db (auditDb action value)
    auditDb action value = rawExecute
      "INSERT INTO records_ingestion_admin_audit(actor_id,action,details) VALUES(?,?,?::jsonb)"
      [PersistInt64 actor,PersistText action,PersistText (jsonText value)]

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode
