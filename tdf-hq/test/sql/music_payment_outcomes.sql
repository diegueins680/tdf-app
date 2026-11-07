-- PAY-MUSIC-OUTCOME-001: real migrated outcome/fulfillment triggers.
-- Provider rows are synthetic evidence; provider transport is outside this test.
BEGIN ISOLATION LEVEL READ COMMITTED;
\ir music_payment_state_fixture.sql

CREATE FUNCTION pg_temp.music_state_assert(condition BOOLEAN, label TEXT)
RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  IF condition IS DISTINCT FROM TRUE THEN
    RAISE EXCEPTION 'music payment outcome assertion failed: %', label;
  END IF;
  RAISE NOTICE 'PASS %', label;
END;
$$;

DO $$
DECLARE
  fixture RECORD;
  closed_state TEXT;
  refused BOOLEAN;
BEGIN
  SELECT * INTO fixture FROM pg_temp.music_payment_state_fixture();
  PERFORM pg_temp.music_state_assert(
    music_checkout_payment_attempt_allowed(fixture.checkout_id),
    'payable checkout admits a provider operation');
  UPDATE commerce_payment_attempt SET status = 'failed', failure_code = 'fixture_failure' WHERE id = fixture.attempt_id;
  UPDATE commerce_checkout_session SET status = 'failed'
    WHERE id = fixture.checkout_id AND status IN ('awaiting_payment','processing');
  PERFORM pg_temp.music_state_assert(
    (SELECT state = 'awaiting_payment' FROM music_purchase_order WHERE id = fixture.order_id),
    'attempt failure does not cancel a payable order');
  PERFORM pg_temp.music_state_assert(
    NOT music_checkout_payment_attempt_allowed(fixture.checkout_id),
    'failed checkout denies a new or replayed provider operation');
  PERFORM pg_temp.music_state_assert(
    NOT EXISTS (SELECT 1 FROM music_entitlement WHERE purchase_order_id = fixture.order_id),
    'failed attempt grants no entitlement');

  -- A late binding from another in-flight create can reopen the checkout.
  UPDATE commerce_checkout_session SET status = 'awaiting_payment' WHERE id = fixture.checkout_id;
  PERFORM pg_temp.music_state_assert(
    NOT music_checkout_payment_attempt_allowed(fixture.checkout_id),
    'late binding cannot clear the failed-attempt quarantine');
  -- recordPaymentProcessing changes status but retains failure_code.
  UPDATE commerce_payment_attempt SET status = 'processing' WHERE id = fixture.attempt_id;
  UPDATE commerce_checkout_session SET status = 'processing' WHERE id = fixture.checkout_id;
  PERFORM pg_temp.music_state_assert(
    NOT music_checkout_payment_attempt_allowed(fixture.checkout_id),
    'late pending response cannot clear the durable failure quarantine');

  UPDATE commerce_payment_attempt SET status = 'succeeded', failure_code = NULL WHERE id = fixture.attempt_id;
  UPDATE commerce_checkout_session SET status = 'paid', paid_minor = total_minor, paid_at = NOW()
    WHERE id = fixture.checkout_id;
  PERFORM pg_temp.music_state_assert(
    (SELECT state = 'paid' FROM music_purchase_order WHERE id = fixture.order_id)
      AND (SELECT count(*) = 1 FROM music_entitlement
        WHERE purchase_order_id = fixture.order_id AND status = 'active'),
    'verified success after failure settles the order and grants once');
  PERFORM pg_temp.music_state_assert(
    NOT music_checkout_payment_attempt_allowed(fixture.checkout_id),
    'paid checkout denies another provider operation');

  -- The same guards used by CheckoutStore.recordPaymentFailure must preserve a
  -- committed success when the failing operation finishes afterwards.
  UPDATE commerce_payment_attempt SET status = 'failed'
    WHERE id = fixture.attempt_id AND status <> 'succeeded';
  UPDATE commerce_checkout_session SET status = 'failed'
    WHERE id = fixture.checkout_id AND status IN ('awaiting_payment','processing');
  UPDATE commerce_checkout_session SET status = 'paid' WHERE id = fixture.checkout_id;
  PERFORM pg_temp.music_state_assert(
    (SELECT status = 'paid' FROM commerce_checkout_session WHERE id = fixture.checkout_id)
      AND (SELECT state = 'paid' FROM music_purchase_order WHERE id = fixture.order_id)
      AND (SELECT count(*) = 1 FROM music_entitlement WHERE purchase_order_id = fixture.order_id),
    'late guarded failure and paid replay preserve the committed grant');

  UPDATE commerce_checkout_session SET status = 'refunded', refunded_minor = paid_minor
    WHERE id = fixture.checkout_id;
  PERFORM pg_temp.music_state_assert(
    (SELECT state = 'refunded' FROM music_purchase_order WHERE id = fixture.order_id)
      AND NOT EXISTS (SELECT 1 FROM music_entitlement
        WHERE purchase_order_id = fixture.order_id AND status = 'active'),
    'refund ends fulfillment');

  SELECT * INTO fixture FROM pg_temp.music_payment_state_fixture();
  UPDATE commerce_payment_attempt SET status = 'succeeded', failure_code = NULL WHERE id = fixture.attempt_id;
  UPDATE commerce_checkout_session SET status = 'paid', paid_minor = total_minor, paid_at = NOW()
    WHERE id = fixture.checkout_id;
  UPDATE commerce_checkout_session SET status = 'disputed' WHERE id = fixture.checkout_id;
  PERFORM pg_temp.music_state_assert(
    (SELECT state = 'chargeback' FROM music_purchase_order WHERE id = fixture.order_id)
      AND NOT EXISTS (SELECT 1 FROM music_entitlement
        WHERE purchase_order_id = fixture.order_id AND status = 'active'),
    'dispute revokes fulfillment');

  -- Legacy failed checkouts may already have a terminal order. Retain verified
  -- money evidence and request reconciliation, without restoring fulfillment.
  FOREACH closed_state IN ARRAY ARRAY['cancelled','refunded','chargeback'] LOOP
    SELECT * INTO fixture FROM pg_temp.music_payment_state_fixture();
    UPDATE music_purchase_order SET state = closed_state WHERE id = fixture.order_id;
    UPDATE commerce_checkout_session SET status = 'failed' WHERE id = fixture.checkout_id;
    UPDATE commerce_payment_attempt SET status = 'succeeded', failure_code = NULL WHERE id = fixture.attempt_id;
    UPDATE commerce_checkout_session SET status = 'paid', paid_minor = total_minor, paid_at = NOW()
      WHERE id = fixture.checkout_id;
    UPDATE commerce_checkout_session SET status = 'paid' WHERE id = fixture.checkout_id;
    PERFORM pg_temp.music_state_assert(
      (SELECT state = closed_state FROM music_purchase_order WHERE id = fixture.order_id)
        AND (SELECT status = 'paid' AND paid_minor = total_minor
          FROM commerce_checkout_session WHERE id = fixture.checkout_id)
        AND NOT EXISTS (SELECT 1 FROM music_entitlement
          WHERE purchase_order_id = fixture.order_id AND status = 'active'),
      'verified money never reopens closed fulfillment: ' || closed_state);
    PERFORM pg_temp.music_state_assert(
      (SELECT count(*) = 1 FROM commerce_reconciliation_exception
        WHERE exception_type = 'music_paid_closed_order'
          AND internal_reference = fixture.checkout_id::TEXT
          AND provider = 'paypal' AND environment = 'sandbox'
          AND merchant_account_ref = 'music-state-fixture'
          AND provider_reference = 'FIXTURE-' || fixture.attempt_id
          AND expected_amount_minor = 250 AND actual_amount_minor = 250
          AND currency = 'USD' AND status = 'open')
        AND (SELECT count(*) = 1 FROM music_release_audit_event
          WHERE idempotency_key = 'music-payment-review:' || fixture.checkout_id),
      'closed-order capture has one observable reconciliation case: ' || closed_state);
  END LOOP;

  SELECT * INTO fixture FROM pg_temp.music_payment_state_fixture();
  refused := FALSE;
  BEGIN
    UPDATE commerce_checkout_session SET status = 'paid', paid_minor = total_minor, paid_at = NOW()
      WHERE id = fixture.checkout_id;
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'music checkout cannot become paid without bound verified payment evidence' THEN
      RAISE;
    END IF;
    refused := TRUE;
  END;
  PERFORM pg_temp.music_state_assert(refused
    AND NOT EXISTS (SELECT 1 FROM music_entitlement WHERE purchase_order_id = fixture.order_id),
    'unverified outcome still rejects without fulfillment');
END;
$$;
ROLLBACK;
