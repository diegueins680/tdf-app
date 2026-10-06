-- Additive, per-ticket admission/inventory projection of canonical refunds.
CREATE TABLE IF NOT EXISTS event_ticket_refund_allocation (
  refund_id UUID NOT NULL REFERENCES commerce_refund(id) ON DELETE RESTRICT,
  ticket_id BIGINT NOT NULL REFERENCES event_ticket(id) ON DELETE RESTRICT,
  order_id BIGINT NOT NULL REFERENCES event_ticket_order(id) ON DELETE RESTRICT,
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  state TEXT NOT NULL DEFAULT 'reserved' CHECK (state IN ('reserved','completed','cancelled')),
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(refund_id,ticket_id)
);
CREATE UNIQUE INDEX IF NOT EXISTS event_ticket_refund_active_ticket
  ON event_ticket_refund_allocation(ticket_id) WHERE state IN ('reserved','completed');
CREATE OR REPLACE FUNCTION tdf_ticket_refund_allocation_immutable() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION 'Ticket refund allocation audit cannot be deleted';
  END IF;
  IF ROW(NEW.refund_id,NEW.ticket_id,NEW.order_id,NEW.amount_minor,NEW.created_at)
       IS DISTINCT FROM ROW(OLD.refund_id,OLD.ticket_id,OLD.order_id,OLD.amount_minor,OLD.created_at)
     OR OLD.state <> 'reserved' OR NEW.state NOT IN ('completed','cancelled') THEN
    RAISE EXCEPTION 'Ticket refund allocation evidence is immutable';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM commerce_refund r WHERE r.id=NEW.refund_id
      AND r.status=CASE NEW.state WHEN 'completed' THEN 'succeeded' ELSE 'cancelled' END) THEN
    RAISE EXCEPTION 'Ticket refund allocation requires canonical completion or cancellation';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_ticket_refund_allocation_immutable ON event_ticket_refund_allocation;
CREATE TRIGGER event_ticket_refund_allocation_immutable BEFORE UPDATE OR DELETE
  ON event_ticket_refund_allocation FOR EACH ROW
  EXECUTE FUNCTION tdf_ticket_refund_allocation_immutable();
