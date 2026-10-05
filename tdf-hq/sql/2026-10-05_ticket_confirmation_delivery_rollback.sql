BEGIN;
DO $$ BEGIN
  IF EXISTS(SELECT 1 FROM event_ticket_confirmation_delivery) THEN
    RAISE EXCEPTION 'Confirmation delivery history must be retained; recover forward';
  END IF;
END $$;
DROP FUNCTION IF EXISTS event_ticket_finish_confirmation(BIGINT,UUID,TEXT);
DROP FUNCTION IF EXISTS event_ticket_claim_confirmation(UUID);
DROP FUNCTION IF EXISTS event_ticket_queue_confirmation(BIGINT);
DROP TABLE IF EXISTS event_ticket_confirmation_delivery;
COMMIT;
