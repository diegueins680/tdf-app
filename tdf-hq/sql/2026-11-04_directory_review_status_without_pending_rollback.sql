-- Rollback of 2026-11-04_directory_review_status_without_pending: restores the
-- original CHECK that also admits the unused 'pending' state.
BEGIN;
SET LOCAL lock_timeout = '10s';
ALTER TABLE public.directory_review DROP CONSTRAINT IF EXISTS directory_review_status_check;
ALTER TABLE public.directory_review ADD CONSTRAINT directory_review_status_check
  CHECK (status IN ('pending', 'published', 'hidden', 'removed'));
COMMIT;
