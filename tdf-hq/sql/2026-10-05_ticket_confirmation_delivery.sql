-- Public checkout confirmation is durable, but SMTP acceptance is not inbox delivery.
BEGIN;
CREATE TABLE IF NOT EXISTS event_ticket_confirmation_delivery (
  order_id BIGINT PRIMARY KEY REFERENCES event_ticket_checkout_runtime(order_id) ON DELETE RESTRICT,
  state TEXT NOT NULL DEFAULT 'pending'
    CHECK (state IN ('pending','processing','accepted','cancelled','dead_letter')),
  attempts INTEGER NOT NULL DEFAULT 0 CHECK (attempts BETWEEN 0 AND 8),
  next_attempt_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  lease_id UUID,
  lease_expires_at TIMESTAMPTZ,
  accepted_at TIMESTAMPTZ,
  last_error_code TEXT CHECK (last_error_code IN ('delivery_failed','lease_expired','no_eligible_tickets')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK ((state='processing' AND lease_id IS NOT NULL AND lease_expires_at IS NOT NULL)
    OR (state<>'processing' AND lease_id IS NULL AND lease_expires_at IS NULL)),
  CHECK ((state='accepted') = (accepted_at IS NOT NULL))
);
CREATE INDEX IF NOT EXISTS event_ticket_confirmation_due_idx
  ON event_ticket_confirmation_delivery(next_attempt_at,order_id)
  WHERE state='pending';

-- Called inside the same transaction as canonical ticket issuance. No historical
-- backfill: upgrading must not send receipts for old purchases.
CREATE OR REPLACE FUNCTION event_ticket_queue_confirmation(p_order_id BIGINT)
RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  PERFORM 1 FROM event_ticket_checkout_runtime r
    JOIN event_ticket_order o ON o.id=r.order_id
    WHERE r.order_id=p_order_id AND r.fulfillment_status='issued'
      AND r.payment_status='paid' AND o.status='paid'
    FOR SHARE OF r,o;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Confirmation requires a paid, issued canonical order';
  END IF;
  INSERT INTO event_ticket_confirmation_delivery(order_id) VALUES(p_order_id)
    ON CONFLICT(order_id) DO NOTHING;
END $$;

CREATE OR REPLACE FUNCTION event_ticket_claim_confirmation(p_lease_id UUID)
RETURNS BIGINT LANGUAGE plpgsql AS $$
DECLARE claimed BIGINT;
BEGIN
  IF p_lease_id IS NULL THEN RAISE EXCEPTION 'Confirmation lease is required'; END IF;
  UPDATE event_ticket_confirmation_delivery
    SET state=CASE WHEN attempts>=8 THEN 'dead_letter' ELSE 'pending' END,
      lease_id=NULL,lease_expires_at=NULL,last_error_code='lease_expired',updated_at=NOW()
    WHERE state='processing' AND lease_expires_at<=NOW();
  WITH candidate AS (
    SELECT order_id FROM event_ticket_confirmation_delivery
      WHERE state='pending' AND attempts<8 AND next_attempt_at<=NOW()
      ORDER BY next_attempt_at,order_id FOR UPDATE SKIP LOCKED LIMIT 1
  )
  UPDATE event_ticket_confirmation_delivery d
    SET state='processing',attempts=attempts+1,lease_id=p_lease_id,
      lease_expires_at=NOW()+INTERVAL '2 minutes',updated_at=NOW()
    FROM candidate c WHERE d.order_id=c.order_id RETURNING d.order_id INTO claimed;
  RETURN claimed;
END $$;

CREATE OR REPLACE FUNCTION event_ticket_finish_confirmation(
  p_order_id BIGINT,p_lease_id UUID,p_outcome TEXT
) RETURNS BOOLEAN LANGUAGE plpgsql AS $$
BEGIN
  IF p_outcome IS NULL OR p_outcome NOT IN ('accepted','delivery_failed','no_eligible_tickets') THEN
    RAISE EXCEPTION 'Invalid confirmation outcome';
  END IF;
  UPDATE event_ticket_confirmation_delivery SET
    state=CASE WHEN p_outcome='accepted' THEN 'accepted'
      WHEN p_outcome='no_eligible_tickets' THEN 'cancelled'
      WHEN attempts>=8 THEN 'dead_letter' ELSE 'pending' END,
    accepted_at=CASE WHEN p_outcome='accepted' THEN NOW() ELSE NULL END,
    next_attempt_at=NOW()+make_interval(secs=>LEAST(3600,30*(2^attempts)::INTEGER)),
    last_error_code=CASE WHEN p_outcome='accepted' THEN NULL ELSE p_outcome END,
    lease_id=NULL,lease_expires_at=NULL,updated_at=NOW()
    WHERE order_id=p_order_id AND state='processing' AND lease_id=p_lease_id
      AND lease_expires_at>NOW();
  RETURN FOUND;
END $$;
COMMIT;
