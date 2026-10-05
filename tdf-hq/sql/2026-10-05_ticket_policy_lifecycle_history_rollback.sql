BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_ticket_checkout_policy WHERE approval_status IN ('approved','retired'))
     OR EXISTS (SELECT 1 FROM event_ticket_checkout_policy_history
                WHERE changed_by='migration:2026-10-05_ticket_policy_lifecycle_history') THEN
    RAISE EXCEPTION 'Cannot remove policy protection or discard historical quantity-limit evidence';
  END IF;
END $$;
DROP TRIGGER IF EXISTS trg_event_ticket_policy_lifecycle_guard ON event_ticket_checkout_policy;
DROP FUNCTION IF EXISTS event_ticket_policy_lifecycle_guard();
COMMIT;
