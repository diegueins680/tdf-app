-- Audit F-09: the directory review machine declared 'pending' but no code path
-- ever creates it (reviews of verified interactions are inserted as
-- 'published'; moderation moves them to 'hidden' or 'removed'). The state is
-- removed from docs/music-directory/formal-model.yaml#review and from the
-- column's CHECK so the model, the database and the code agree. Refuses to run
-- if any review is pending.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM public.directory_review WHERE status = 'pending') THEN
    RAISE EXCEPTION 'Pending directory reviews exist; resolve them before removing the state';
  END IF;
END $$;
ALTER TABLE public.directory_review DROP CONSTRAINT IF EXISTS directory_review_status_check;
ALTER TABLE public.directory_review ADD CONSTRAINT directory_review_status_check
  CHECK (status IN ('published', 'hidden', 'removed'));
COMMIT;
