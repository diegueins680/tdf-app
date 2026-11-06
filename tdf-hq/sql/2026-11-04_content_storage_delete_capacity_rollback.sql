DO $$ BEGIN
  IF EXISTS(SELECT 1 FROM content_storage_upload WHERE status='cancelled' AND deleted_at IS NULL) THEN
    RAISE EXCEPTION 'Unconfirmed deletions prevent rollback of capacity protection';
  END IF;
END $$;
-- Keep the stricter compatible functions until the enclosing accounting rollback.
