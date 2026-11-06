DO $$ BEGIN
  IF EXISTS(SELECT 1 FROM content_storage_upload) THEN
    RAISE EXCEPTION 'Refusing rollback while content storage uploads exist';
  END IF;
END $$;
DROP FUNCTION IF EXISTS content_storage_reserve(bigint,text,text,bigint,text,text,text,text,text,uuid,uuid);
DROP TABLE IF EXISTS content_storage_upload;
DROP TABLE IF EXISTS content_storage_account;
