-- Disable music public/authoring flags and stop all workers before rollback.
-- Retain media/jobs/audit. Old workers cannot execute create_preview jobs.
BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM music_processing_job WHERE job_kind='create_preview'
    AND status IN ('queued','running','retry')) THEN
    RAISE EXCEPTION 'Drain/cancel preview jobs before rollback; do not discard their audit';
  END IF;
END $$;
DROP FUNCTION music_queue_preview_jobs(INTEGER);
DROP FUNCTION music_public_asset_accessible(UUID,TEXT);
ALTER FUNCTION music_public_asset_accessible_before_preview(UUID,TEXT) RENAME TO music_public_asset_accessible;
DROP FUNCTION music_check_submission(UUID);
ALTER FUNCTION music_check_submission_before_preview(UUID) RENAME TO music_check_submission;
DROP FUNCTION music_preview_matches(UUID);
DROP FUNCTION music_preview_spec(BIGINT,BIGINT,BIGINT);
COMMIT;
