-- Keep expired-lease recovery bounded by processing jobs, not retained history.
BEGIN;
CREATE INDEX IF NOT EXISTS event_ticket_confirmation_expired_lease_idx
  ON event_ticket_confirmation_delivery(lease_expires_at,order_id)
  WHERE state='processing';
COMMIT;
