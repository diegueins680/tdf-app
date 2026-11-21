-- Refuses to drop the flag while any tier still uses it; an operator must
-- turn those tiers back into regular tiers first.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM public.event_ticket_tier WHERE registers_participant) THEN
    RAISE EXCEPTION 'Participant ticket tiers exist; rollback requires converting them first';
  END IF;
END $$;
ALTER TABLE public.event_ticket_tier DROP COLUMN registers_participant;
COMMIT;
