-- Additive correction; historical queue migration remains immutable.
BEGIN;
CREATE OR REPLACE FUNCTION event_ticket_finish_confirmation(
  p_order_id BIGINT,p_lease_id UUID,p_outcome TEXT
) RETURNS BOOLEAN LANGUAGE plpgsql AS $$
DECLARE admitted_at TIMESTAMPTZ;
BEGIN
  IF p_outcome IS NULL OR p_outcome NOT IN ('accepted','delivery_failed','no_eligible_tickets') THEN
    RAISE EXCEPTION 'Invalid confirmation outcome';
  END IF;
  -- Lock before reading real time: a lease may expire while waiting.
  PERFORM 1 FROM event_ticket_confirmation_delivery WHERE order_id=p_order_id FOR UPDATE;
  admitted_at := clock_timestamp();
  UPDATE event_ticket_confirmation_delivery SET
    state=CASE WHEN p_outcome='accepted' THEN 'accepted'
      WHEN p_outcome='no_eligible_tickets' THEN 'cancelled'
      WHEN attempts>=8 THEN 'dead_letter' ELSE 'pending' END,
    accepted_at=CASE WHEN p_outcome='accepted' THEN admitted_at ELSE NULL END,
    next_attempt_at=admitted_at+make_interval(secs=>LEAST(3600,30*(2^attempts)::INTEGER)),
    last_error_code=CASE WHEN p_outcome='accepted' THEN NULL ELSE p_outcome END,
    lease_id=NULL,lease_expires_at=NULL,updated_at=admitted_at
    WHERE order_id=p_order_id AND state='processing' AND lease_id=p_lease_id
      AND lease_expires_at>admitted_at;
  RETURN FOUND;
END $$;
COMMIT;
