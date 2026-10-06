BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_ticket_refund_request_binding) THEN
    RAISE EXCEPTION 'Cannot remove used ticket refund request bindings; use forward recovery';
  END IF;
END $$;
DROP TABLE IF EXISTS event_ticket_refund_request_binding;
DROP FUNCTION IF EXISTS tdf_ticket_refund_request_binding_immutable();
COMMIT;
