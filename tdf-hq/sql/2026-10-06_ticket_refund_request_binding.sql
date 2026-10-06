CREATE TABLE IF NOT EXISTS event_ticket_refund_request_binding (
  request_id BIGINT PRIMARY KEY REFERENCES ticket_refund_request(id) ON DELETE RESTRICT,
  refund_id UUID NOT NULL UNIQUE REFERENCES commerce_refund(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE OR REPLACE FUNCTION tdf_ticket_refund_request_binding_immutable() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  RAISE EXCEPTION 'Ticket refund request binding is immutable';
END $$;
DROP TRIGGER IF EXISTS event_ticket_refund_request_binding_immutable ON event_ticket_refund_request_binding;
CREATE TRIGGER event_ticket_refund_request_binding_immutable BEFORE UPDATE OR DELETE
 ON event_ticket_refund_request_binding FOR EACH ROW
 EXECUTE FUNCTION tdf_ticket_refund_request_binding_immutable();
