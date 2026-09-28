-- Stop execution first. Retain metadata and audit history for compatibility;
-- never delete imported catalog content as a schema rollback side effect.
BEGIN;
UPDATE records_ingestion_control SET enabled=false,updated_at=now() WHERE singleton;
DROP FUNCTION IF EXISTS tdf_ingest_public_video(bigint,uuid,jsonb);
DROP FUNCTION IF EXISTS tdf_mark_video_unavailable(bigint,uuid,text,text,timestamptz);
DROP TRIGGER IF EXISTS records_source_configuration_lock ON social_sync_account;
DROP FUNCTION IF EXISTS tdf_lock_records_source_configuration();
DROP FUNCTION IF EXISTS tdf_expire_records_provider_data();
DROP FUNCTION IF EXISTS tdf_clear_owned_video_metadata(uuid);
COMMIT;
