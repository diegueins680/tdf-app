-- Fresh leases use real claim time; locked expired rows cannot stall unrelated work.
BEGIN;
CREATE OR REPLACE FUNCTION event_ticket_claim_confirmation(p_lease_id UUID)
RETURNS BIGINT LANGUAGE plpgsql AS $$
DECLARE claimed BIGINT; admitted_at TIMESTAMPTZ;
BEGIN
  IF p_lease_id IS NULL THEN RAISE EXCEPTION 'Confirmation lease is required'; END IF;
  WITH expired AS (
    SELECT order_id FROM event_ticket_confirmation_delivery
      WHERE state='processing' AND lease_expires_at<=clock_timestamp()
      ORDER BY lease_expires_at,order_id FOR UPDATE SKIP LOCKED LIMIT 100
  )
  UPDATE event_ticket_confirmation_delivery d
    SET state=CASE WHEN attempts>=8 THEN 'dead_letter' ELSE 'pending' END,
      lease_id=NULL,lease_expires_at=NULL,last_error_code='lease_expired',updated_at=clock_timestamp()
    FROM expired e WHERE d.order_id=e.order_id;
  SELECT order_id INTO claimed FROM event_ticket_confirmation_delivery
    WHERE state='pending' AND attempts<8 AND next_attempt_at<=clock_timestamp()
    ORDER BY next_attempt_at,order_id FOR UPDATE SKIP LOCKED LIMIT 1;
  IF claimed IS NULL THEN RETURN NULL; END IF;
  admitted_at := clock_timestamp();
  UPDATE event_ticket_confirmation_delivery
    SET state='processing',attempts=attempts+1,lease_id=p_lease_id,
      lease_expires_at=admitted_at+INTERVAL '2 minutes',updated_at=admitted_at
    WHERE order_id=claimed;
  RETURN claimed;
END $$;
COMMIT;
