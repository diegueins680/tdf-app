-- Removes the rebinding trigger. Assets already re-pointed and jobs already
-- cancelled stay as they are. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP TRIGGER IF EXISTS trg_music_rebind_upload_asset ON music_processing_job;
DROP FUNCTION IF EXISTS music_rebind_upload_asset();
COMMIT;
