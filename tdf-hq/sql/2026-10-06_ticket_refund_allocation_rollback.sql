BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_ticket_refund_allocation) THEN
    RAISE EXCEPTION 'Cannot remove used ticket refund allocations; use forward recovery';
  END IF;
END $$;
DROP TABLE IF EXISTS event_ticket_refund_allocation;
DROP FUNCTION IF EXISTS tdf_ticket_refund_allocation_immutable();
COMMIT;
