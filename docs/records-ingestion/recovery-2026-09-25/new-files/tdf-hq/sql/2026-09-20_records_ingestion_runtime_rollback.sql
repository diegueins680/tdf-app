-- Stop execution first. Retain metadata and audit history for compatibility;
-- never delete imported catalog content as a schema rollback side effect.
BEGIN;
UPDATE records_ingestion_control SET enabled=false,updated_at=now() WHERE singleton;
DROP FUNCTION IF EXISTS tdf_ingest_public_video(bigint,uuid,jsonb);
COMMIT;
