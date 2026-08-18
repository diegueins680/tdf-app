-- Provider-action safety follow-up for canonical service bookings.
-- Failed provider attempts remain retryable only until the immutable hold
-- deadline. Once it passes, the checkout and resource allocation expire.
BEGIN;

CREATE OR REPLACE FUNCTION service_booking_expire_holds(at_time TIMESTAMPTZ DEFAULT NOW())
RETURNS INTEGER LANGUAGE plpgsql AS $$
DECLARE expired_count INTEGER;
BEGIN
  WITH expired AS (
    UPDATE commerce_checkout_session
      SET status = 'expired', updated_at = at_time
      WHERE domain_type = 'service_booking'
        AND status IN ('holding','awaiting_payment','failed')
        AND expires_at <= at_time
      RETURNING id
  ) SELECT count(*) INTO expired_count FROM expired;
  RETURN expired_count;
END $$;

COMMIT;
