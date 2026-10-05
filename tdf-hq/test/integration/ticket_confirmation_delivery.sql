-- Existing public checkout migration fixture: order 1 is canonically paid and
-- issued; order 2 is expired/unpaid. Everything below rolls back.
BEGIN;
DO $$ DECLARE lease UUID := 'c0100000-0000-4000-8000-000000000001';
  stale UUID := 'c0100000-0000-4000-8000-000000000002';
  rejected BOOLEAN := FALSE; result BIGINT;
BEGIN
  BEGIN
    PERFORM event_ticket_queue_confirmation(2);
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Confirmation requires a paid, issued canonical order' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Unpaid order was queued'; END IF;
  PERFORM event_ticket_queue_confirmation(1);
  PERFORM event_ticket_queue_confirmation(1);
  IF (SELECT COUNT(*) FROM event_ticket_confirmation_delivery)<>1 THEN
    RAISE EXCEPTION 'Duplicate issuance created duplicate confirmations'; END IF;
  result := event_ticket_claim_confirmation(lease);
  IF result IS DISTINCT FROM 1 OR event_ticket_claim_confirmation(stale) IS NOT NULL THEN
    RAISE EXCEPTION 'A leased receipt was claimed twice'; END IF;
  IF event_ticket_finish_confirmation(1,stale,'accepted') THEN
    RAISE EXCEPTION 'Foreign lease acknowledged a receipt'; END IF;
  IF NOT event_ticket_finish_confirmation(1,lease,'delivery_failed') THEN
    RAISE EXCEPTION 'Failed delivery was not retained'; END IF;
  IF event_ticket_claim_confirmation(stale) IS NOT NULL THEN
    RAISE EXCEPTION 'Retry ignored backoff'; END IF;
  UPDATE event_ticket_confirmation_delivery SET next_attempt_at=NOW()-INTERVAL '1 minute';
  IF event_ticket_claim_confirmation(stale) IS DISTINCT FROM 1 THEN
    RAISE EXCEPTION 'Due retry was not claimable'; END IF;
  UPDATE event_ticket_confirmation_delivery SET lease_expires_at=NOW()-INTERVAL '1 second';
  IF event_ticket_finish_confirmation(1,stale,'accepted') THEN
    RAISE EXCEPTION 'Expired worker acknowledged a receipt'; END IF;
  IF event_ticket_claim_confirmation(lease) IS DISTINCT FROM 1 THEN
    RAISE EXCEPTION 'Crashed worker lease was not recovered'; END IF;
  IF NOT event_ticket_finish_confirmation(1,lease,'accepted') THEN
    RAISE EXCEPTION 'Successful SMTP acceptance was not retained'; END IF;
  PERFORM event_ticket_queue_confirmation(1);
  IF event_ticket_claim_confirmation(stale) IS NOT NULL
    OR (SELECT accepted_at IS NULL FROM event_ticket_confirmation_delivery WHERE order_id=1) THEN
    RAISE EXCEPTION 'Accepted confirmation was requeued'; END IF;
END $$;
ROLLBACK;
DO $$ BEGIN
  IF EXISTS(SELECT 1 FROM event_ticket_confirmation_delivery) THEN
    RAISE EXCEPTION 'Rolled-back issuance left delivery intent'; END IF;
END $$;

BEGIN;
SELECT event_ticket_queue_confirmation(1);
UPDATE event_ticket_confirmation_delivery SET attempts=7;
SELECT event_ticket_claim_confirmation('c0100000-0000-4000-8000-000000000001');
SELECT event_ticket_finish_confirmation(1,'c0100000-0000-4000-8000-000000000001','delivery_failed');
DO $$ BEGIN
  IF NOT EXISTS(SELECT 1 FROM event_ticket_confirmation_delivery WHERE state='dead_letter' AND attempts=8)
    OR event_ticket_claim_confirmation('c0100000-0000-4000-8000-000000000002') IS NOT NULL THEN
    RAISE EXCEPTION 'Delivery retry budget was not enforced'; END IF;
END $$;
ROLLBACK;

BEGIN;
SELECT event_ticket_queue_confirmation(1);
SELECT event_ticket_claim_confirmation('c0100000-0000-4000-8000-000000000001');
SELECT event_ticket_finish_confirmation(1,'c0100000-0000-4000-8000-000000000001','no_eligible_tickets');
DO $$ BEGIN
  IF NOT EXISTS(SELECT 1 FROM event_ticket_confirmation_delivery WHERE state='cancelled') THEN
    RAISE EXCEPTION 'Revoked/transferred entitlement did not cancel delivery'; END IF;
END $$;
ROLLBACK;
