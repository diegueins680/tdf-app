-- A re-upload with the same declared SHA-256 binds to the existing asset row
-- (unique on version, role, sha256), but the row kept the first upload's
-- quarantine object, so after a checksum-mismatch dead letter the corrected
-- bytes were never inspected. When a processing job is enqueued for an upload
-- session whose object differs from the asset's and the asset is not
-- promoted, point the asset at the session's object and close the asset's
-- earlier open attempts. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE OR REPLACE FUNCTION music_rebind_upload_asset()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE
  session music_upload_session;
  previous_key TEXT;
BEGIN
  IF NEW.source_asset_id IS NULL OR NEW.job_key !~ '^[0-9a-f-]{36}$' THEN
    RETURN NEW;
  END IF;
  SELECT * INTO session FROM music_upload_session WHERE id = NEW.job_key::uuid;
  IF NOT FOUND THEN
    RETURN NEW;
  END IF;

  UPDATE music_asset asset
  SET bucket_name = session.bucket_name,
      object_key = session.quarantine_object_key,
      storage_provider = session.provider,
      storage_class = 'quarantine',
      byte_size = session.expected_size,
      original_filename = session.original_filename,
      media_type = COALESCE(session.expected_media_type, asset.media_type),
      processing_state = 'uploaded',
      provenance = COALESCE(asset.provenance, '{}'::jsonb) || jsonb_build_object(
        'upload_session_id', session.id,
        'provider_upload_id', session.provider_upload_id,
        'rebound_from_object_key', asset.object_key)
  WHERE asset.id = NEW.source_asset_id
    AND NOT asset.immutable
    AND asset.processing_state IN ('uploaded', 'inspecting', 'failed', 'invalid')
    AND asset.sha256 = session.expected_sha256
    AND asset.release_version_id = session.release_version_id
    AND (asset.bucket_name, asset.object_key) IS DISTINCT FROM (session.bucket_name, session.quarantine_object_key)
  RETURNING asset.provenance->>'rebound_from_object_key' INTO previous_key;

  IF previous_key IS NOT NULL THEN
    UPDATE music_processing_job
    SET status = 'cancelled', locked_at = NULL, locked_by = NULL, updated_at = NOW()
    WHERE source_asset_id = NEW.source_asset_id
      AND id <> NEW.id
      AND status IN ('queued', 'retry', 'dead_letter');
  END IF;
  RETURN NEW;
END;
$$;

CREATE OR REPLACE TRIGGER trg_music_rebind_upload_asset
  BEFORE INSERT ON music_processing_job
  FOR EACH ROW
  WHEN (NEW.job_kind IN ('inspect_audio', 'inspect_artwork', 'validate_release'))
  EXECUTE FUNCTION music_rebind_upload_asset();

COMMIT;
