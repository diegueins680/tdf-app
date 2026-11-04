ALTER TABLE content_storage_upload ADD COLUMN IF NOT EXISTS deleted_at timestamptz;
ALTER TABLE content_storage_upload ADD COLUMN IF NOT EXISTS original_completed_at timestamptz;
COMMENT ON COLUMN content_storage_upload.deleted_at IS 'Set only after the owner-requested file deletion is confirmed; cancelled rows with NULL retain a cleanup intent.';
