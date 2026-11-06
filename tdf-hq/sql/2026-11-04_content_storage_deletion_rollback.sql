DO $$ BEGIN
  IF EXISTS(SELECT 1 FROM content_storage_upload WHERE status='cancelled') THEN
    RAISE EXCEPTION 'Refusing to discard storage deletion intents';
  END IF;
END $$;
ALTER TABLE content_storage_upload DROP COLUMN IF EXISTS deleted_at;
ALTER TABLE content_storage_upload DROP COLUMN IF EXISTS original_completed_at;
