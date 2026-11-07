-- A re-upload with the same declared SHA-256 binds to the existing asset row
-- (unique on version, role, sha256), but the row kept the first upload's
-- quarantine object, so after a checksum-mismatch dead letter the corrected
-- bytes were never inspected. When a processing job is enqueued for an upload
-- session whose object differs from the asset's and the asset is not
-- promoted, point the asset at the session's object and close the asset's
-- other attempts, running ones included (the lease guard stops a cancelled
-- run before it writes). AFTER INSERT, so a conflicting insert that creates
-- no job (ON CONFLICT DO NOTHING) changes nothing. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE OR REPLACE FUNCTION music_rebind_upload_asset()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE
  session music_upload_session;
BEGIN
  IF NEW.source_asset_id IS NULL
     OR NEW.job_key !~ '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$' THEN
    RETURN NEW;
  END IF;
  SELECT * INTO session FROM music_upload_session
  WHERE id = NEW.job_key::uuid AND completed_asset_id = NEW.source_asset_id;
  IF NOT FOUND THEN
    RETURN NEW;
  END IF;

  -- Same lock order as the worker: version, then job rows, then the asset.
  PERFORM 1 FROM music_release_version WHERE id = session.release_version_id FOR NO KEY UPDATE;

  UPDATE music_asset asset
  SET bucket_name = session.bucket_name,
      object_key = session.quarantine_object_key,
      storage_provider = session.provider,
      storage_class = 'quarantine',
      byte_size = session.expected_size,
      original_filename = session.original_filename,
      media_type = COALESCE(session.expected_media_type, asset.media_type),
      etag = NULL,
      processing_state = 'uploaded',
      provenance = asset.provenance || jsonb_build_object(
        'upload_session_id', session.id,
        'provider_upload_id', session.provider_upload_id,
        'rebound_from_object_key', asset.object_key,
        'rebound_from_object_keys', COALESCE(asset.provenance->'rebound_from_object_keys', '[]'::jsonb) || to_jsonb(asset.object_key))
  WHERE asset.id = NEW.source_asset_id
    AND NOT asset.immutable
    AND asset.processing_state IN ('uploaded', 'inspecting', 'failed', 'invalid')
    AND asset.asset_role = session.asset_role
    AND asset.sha256 = session.expected_sha256
    AND asset.release_version_id = session.release_version_id
    AND (asset.bucket_name, asset.object_key) IS DISTINCT FROM (session.bucket_name, session.quarantine_object_key);

  IF FOUND THEN
    UPDATE music_processing_job
    SET status = 'cancelled', locked_at = NULL, locked_by = NULL, updated_at = NOW()
    WHERE source_asset_id = NEW.source_asset_id
      AND id <> NEW.id
      AND status IN ('queued', 'retry', 'running', 'dead_letter');
  END IF;
  RETURN NEW;
END;
$$;

CREATE OR REPLACE TRIGGER trg_music_rebind_upload_asset
  AFTER INSERT ON music_processing_job
  FOR EACH ROW
  WHEN (NEW.job_kind IN ('inspect_audio', 'inspect_artwork', 'validate_release'))
  EXECUTE FUNCTION music_rebind_upload_asset();

COMMIT;
