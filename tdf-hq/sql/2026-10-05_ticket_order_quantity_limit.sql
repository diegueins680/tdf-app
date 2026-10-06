-- Per-order limits belong to the existing versioned commercial policy.
-- Existing policies retain the previous server limit of 100 tickets.
BEGIN;
ALTER TABLE event_ticket_checkout_policy
  ADD COLUMN IF NOT EXISTS max_tickets_per_order INTEGER NOT NULL DEFAULT 100
    CHECK (max_tickets_per_order BETWEEN 1 AND 100);

CREATE OR REPLACE FUNCTION event_ticket_order_limit_immutable()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.approval_status IN ('approved','retired')
     AND NEW.max_tickets_per_order IS DISTINCT FROM OLD.max_tickets_per_order THEN
    RAISE EXCEPTION 'Approved ticket quantity limit is immutable; create a new policy version';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_order_limit_immutable
  ON event_ticket_checkout_policy;
CREATE TRIGGER trg_event_ticket_order_limit_immutable
  BEFORE UPDATE ON event_ticket_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION event_ticket_order_limit_immutable();

CREATE OR REPLACE FUNCTION event_ticket_checkout_enforce_order_limit()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  maximum_quantity INTEGER;
BEGIN
  SELECT max_tickets_per_order INTO maximum_quantity
    FROM event_ticket_checkout_policy WHERE id = NEW.policy_id FOR SHARE;
  IF maximum_quantity IS NULL OR NEW.quantity > maximum_quantity THEN
    RAISE EXCEPTION 'Ticket quantity exceeds the purchased policy limit'
      USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_checkout_enforce_order_limit
  ON event_ticket_checkout_runtime;
CREATE TRIGGER trg_event_ticket_checkout_enforce_order_limit
  BEFORE INSERT ON event_ticket_checkout_runtime
  FOR EACH ROW EXECUTE FUNCTION event_ticket_checkout_enforce_order_limit();
COMMIT;
