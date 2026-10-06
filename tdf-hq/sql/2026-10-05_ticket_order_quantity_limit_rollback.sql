BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_ticket_checkout_policy WHERE max_tickets_per_order <> 100) THEN
    RAISE EXCEPTION 'Configured ticket quantity limits must be preserved; recover forward';
  END IF;
END $$;
DROP TRIGGER IF EXISTS trg_event_ticket_checkout_enforce_order_limit ON event_ticket_checkout_runtime;
DROP FUNCTION IF EXISTS event_ticket_checkout_enforce_order_limit();
DROP TRIGGER IF EXISTS trg_event_ticket_order_limit_immutable ON event_ticket_checkout_policy;
DROP FUNCTION IF EXISTS event_ticket_order_limit_immutable();
ALTER TABLE event_ticket_checkout_policy DROP COLUMN IF EXISTS max_tickets_per_order;
COMMIT;
