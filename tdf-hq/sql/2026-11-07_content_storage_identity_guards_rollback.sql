-- Never remove admission from populated credential/reference histories.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE content_storage_credential, content_storage_reference IN SHARE ROW EXCLUSIVE MODE;
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM content_storage_credential)
     OR EXISTS (SELECT 1 FROM content_storage_reference) THEN
    RAISE EXCEPTION 'Cannot remove storage identity admission while credential/reference history exists' USING ERRCODE='55000';
  END IF;
END $$;
DROP TRIGGER IF EXISTS identity_archive_reference_guard ON content_storage_credential;
DROP TRIGGER IF EXISTS identity_archive_reference_guard ON content_storage_reference;
COMMIT;
