-- Extend the existing versioned commercial policy; no parallel ticketing policy.
BEGIN;
ALTER TABLE event_ticket_checkout_policy
  ADD COLUMN IF NOT EXISTS transfer_deadline TIMESTAMPTZ;

CREATE OR REPLACE FUNCTION event_ticket_transfer_deadline_immutable()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.approval_status IN ('approved','retired')
     AND NEW.transfer_deadline IS DISTINCT FROM OLD.transfer_deadline THEN
    RAISE EXCEPTION 'Approved transfer deadline is immutable; create a new policy version';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_transfer_deadline_immutable
  ON event_ticket_checkout_policy;
CREATE TRIGGER trg_event_ticket_transfer_deadline_immutable
  BEFORE UPDATE ON event_ticket_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION event_ticket_transfer_deadline_immutable();
COMMIT;
