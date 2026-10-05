-- Additive synchronization from verified refund evidence; never infer no-charge
-- outcomes from failed attempts. Existing migration checksums remain unchanged.
\set ON_ERROR_STOP on
BEGIN;
LOCK TABLE commerce_checkout_session, commerce_payment_attempt,
  commerce_refund, commerce_payment_intent IN SHARE ROW EXCLUSIVE MODE;

CREATE OR REPLACE FUNCTION commerce_reconcile_intent_refunds(target_intent UUID, correlation TEXT)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE
  intent commerce_payment_intent%ROWTYPE;
  verified_total BIGINT;
  next_status TEXT;
BEGIN
  SELECT * INTO intent FROM commerce_payment_intent WHERE id = target_intent FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Refund references missing canonical intent' USING ERRCODE = '23514';
  END IF;
  IF EXISTS (
    SELECT 1 FROM commerce_refund refund
    JOIN commerce_payment_attempt attempt ON attempt.id = refund.payment_attempt_id
    JOIN commerce_checkout_session checkout ON checkout.id = intent.checkout_id
    WHERE attempt.payment_intent_id = target_intent AND refund.status = 'succeeded'
      AND (refund.checkout_id IS DISTINCT FROM intent.checkout_id
        OR refund.provider IS DISTINCT FROM intent.provider
        OR refund.environment IS DISTINCT FROM checkout.environment
        OR refund.currency IS DISTINCT FROM intent.currency
        OR refund.merchant_account_ref IS DISTINCT FROM attempt.merchant_account_ref
        OR attempt.status <> 'succeeded'
        OR NULLIF(btrim(refund.provider_refund_id), '') IS NULL
        OR refund.completed_at IS NULL)
  ) THEN
    RAISE EXCEPTION 'Verified refund does not match canonical intent' USING ERRCODE = '23514';
  END IF;
  SELECT COALESCE(sum(refund.amount_minor), 0) INTO verified_total
  FROM commerce_refund refund
  JOIN commerce_payment_attempt attempt ON attempt.id = refund.payment_attempt_id
  WHERE attempt.payment_intent_id = target_intent AND refund.status = 'succeeded';
  IF verified_total = intent.refunded_minor THEN RETURN; END IF;
  IF verified_total < intent.refunded_minor OR verified_total > intent.captured_minor
    OR intent.status NOT IN ('captured','partially_captured','partially_refunded','refunded','disputed','chargeback') THEN
    RAISE EXCEPTION 'Verified refunds conflict with canonical captured balance' USING ERRCODE = '23514';
  END IF;
  -- Do not erase a dispute/chargeback state while recording financial evidence.
  next_status := CASE
    WHEN intent.status IN ('disputed','chargeback') THEN intent.status
    WHEN verified_total = intent.captured_minor THEN 'refunded'
    ELSE 'partially_refunded' END;
  UPDATE commerce_payment_intent SET refunded_minor = verified_total,
    status = next_status, updated_at = clock_timestamp() WHERE id = target_intent;
  INSERT INTO commerce_payment_state_history
    (payment_intent_id, from_status, to_status, event_type, actor_type, correlation_id, occurred_at)
    VALUES (target_intent, intent.status, next_status, 'refund_completion_verified', 'provider', correlation, clock_timestamp());
END
$$;

CREATE OR REPLACE FUNCTION commerce_sync_payment_intent_from_refund()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE target_intent UUID;
BEGIN
  IF NEW.status <> 'succeeded' THEN RETURN NEW; END IF;
  SELECT payment_intent_id INTO target_intent FROM commerce_payment_attempt
    WHERE id = NEW.payment_attempt_id;
  -- Legacy, unlinked attempts retain their existing refund behavior.
  IF target_intent IS NOT NULL THEN
    PERFORM commerce_reconcile_intent_refunds(target_intent, 'payment-refund:' || NEW.id::text);
  END IF;
  RETURN NEW;
END
$$;

DROP TRIGGER IF EXISTS trg_commerce_sync_payment_intent_from_refund ON commerce_refund;
CREATE TRIGGER trg_commerce_sync_payment_intent_from_refund
  AFTER UPDATE OF status ON commerce_refund FOR EACH ROW
  EXECUTE FUNCTION commerce_sync_payment_intent_from_refund();

-- Reconcile only bound, already verified evidence. Reapplication is a no-op;
-- any inconsistent evidence aborts the entire migration rather than guessing.
DO $$
DECLARE target UUID;
BEGIN
  FOR target IN
    SELECT DISTINCT attempt.payment_intent_id FROM commerce_payment_attempt attempt
    JOIN commerce_refund refund ON refund.payment_attempt_id = attempt.id
    WHERE attempt.payment_intent_id IS NOT NULL AND refund.status = 'succeeded'
  LOOP
    PERFORM commerce_reconcile_intent_refunds(target, 'migration:2026-09-16-payment-intent-refund-sync');
  END LOOP;
END
$$;
COMMIT;
