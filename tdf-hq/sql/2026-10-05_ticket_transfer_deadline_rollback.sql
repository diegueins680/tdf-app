BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_ticket_checkout_policy WHERE transfer_deadline IS NOT NULL) THEN
    RAISE EXCEPTION 'Retain approved commercial deadlines; use a forward repair';
  END IF;
END $$;
DROP TRIGGER IF EXISTS trg_event_ticket_transfer_deadline_immutable ON event_ticket_checkout_policy;
DROP FUNCTION IF EXISTS event_ticket_transfer_deadline_immutable();
ALTER TABLE event_ticket_checkout_policy DROP COLUMN transfer_deadline;
COMMIT;
