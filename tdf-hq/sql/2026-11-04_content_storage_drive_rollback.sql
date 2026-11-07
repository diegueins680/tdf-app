DO $$ BEGIN
  IF EXISTS(SELECT 1 FROM content_storage_drive_attempt) THEN
    RAISE EXCEPTION 'Refusing rollback while provider upload identities exist';
  END IF;
END $$;
DROP TABLE IF EXISTS content_storage_drive_attempt;
