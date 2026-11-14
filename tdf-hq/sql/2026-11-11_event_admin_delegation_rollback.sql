-- Refuses to discard recorded grants or invitations; an operator must export
-- or revoke them explicitly before the tables can be removed.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM public.event_admin_member)
     OR EXISTS (SELECT 1 FROM public.event_admin_invitation) THEN
    RAISE EXCEPTION 'Event administrator grants or invitations exist; rollback requires an explicit data-preservation plan';
  END IF;
END $$;
DROP TABLE public.event_admin_member;
DROP TABLE public.event_admin_invitation;
COMMIT;
