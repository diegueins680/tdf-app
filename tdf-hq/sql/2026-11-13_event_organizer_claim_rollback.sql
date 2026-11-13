-- Refuses to discard recorded claims: they are the only record of who asked
-- for an event, who decided and why. An operator must export them first.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM public.event_organizer_claim) THEN
    RAISE EXCEPTION 'Event organizer claims exist; rollback requires an explicit data-preservation plan';
  END IF;
END $$;
DROP TABLE public.event_organizer_claim;
COMMIT;
