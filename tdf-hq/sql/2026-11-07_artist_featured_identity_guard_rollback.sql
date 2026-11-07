-- Preserve every preference and canonical identity helper during rollback.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE music_artist_featured_track IN ACCESS EXCLUSIVE MODE;
DO $$
BEGIN
  IF EXISTS(SELECT 1 FROM music_artist_featured_track) THEN
    RAISE EXCEPTION 'Featured preference evidence exists; use a forward identity repair' USING ERRCODE='55000';
  END IF;
END $$;
DROP TRIGGER IF EXISTS identity_archive_reference_guard ON music_artist_featured_track;
COMMIT;
