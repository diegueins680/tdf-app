-- Keep the canonical payment intent synchronized with authoritative positive
-- progress already recorded by the legacy checkout runtime. Failure rows are
-- deliberately not synchronized: a transport failure can be ambiguous and
-- must not make cross-provider fallback appear safe.
\set ON_ERROR_STOP on
BEGIN;

CREATE OR REPLACE FUNCTION commerce_sync_payment_intent_from_attempt()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  prior_status TEXT;
  next_status TEXT;
  transition_event TEXT;
BEGIN
  IF NEW.payment_intent_id IS NULL THEN
    RETURN NEW;
  END IF;

  IF NEW.status IS NOT DISTINCT FROM OLD.status
     AND NEW.payment_intent_id IS NOT DISTINCT FROM OLD.payment_intent_id THEN
    RETURN NEW;
  END IF;

  SELECT intent.status
    INTO prior_status
    FROM commerce_payment_intent intent
   WHERE intent.id = NEW.payment_intent_id
   FOR UPDATE;

  IF NOT FOUND THEN
    RAISE EXCEPTION 'Payment attempt % references a missing canonical intent', NEW.id;
  END IF;

  IF NEW.status = 'requires_customer_action'
     AND prior_status = 'requires_payment_method' THEN
    next_status := 'requires_customer_action';
    transition_event := 'attempt_requires_customer_action';
  ELSIF NEW.status = 'processing'
     AND prior_status IN ('requires_payment_method', 'requires_customer_action') THEN
    next_status := 'processing';
    transition_event := 'attempt_processing_observed';
  ELSIF NEW.status = 'succeeded'
     AND prior_status IN (
       'requires_payment_method', 'requires_customer_action', 'processing',
       'authorized', 'partially_captured'
     ) THEN
    next_status := 'captured';
    transition_event := 'attempt_capture_verified';
  ELSE
    RETURN NEW;
  END IF;

  UPDATE commerce_payment_intent intent
     SET status = next_status,
         authorized_minor = CASE
           WHEN next_status = 'captured' THEN GREATEST(intent.authorized_minor, NEW.amount_minor)
           ELSE intent.authorized_minor
         END,
         captured_minor = CASE
           WHEN next_status = 'captured' THEN NEW.amount_minor
           ELSE intent.captured_minor
         END,
         updated_at = NEW.updated_at
   WHERE intent.id = NEW.payment_intent_id
     AND intent.status = prior_status
     AND intent.checkout_id = NEW.checkout_id
     AND intent.provider = NEW.provider
     AND intent.amount_minor = NEW.amount_minor
     AND intent.currency = NEW.currency;

  IF NOT FOUND THEN
    RAISE EXCEPTION 'Payment attempt % does not match canonical intent %',
      NEW.id, NEW.payment_intent_id;
  END IF;

  INSERT INTO commerce_payment_state_history (
    payment_intent_id, from_status, to_status, event_type, actor_type,
    correlation_id, occurred_at
  ) VALUES (
    NEW.payment_intent_id, prior_status, next_status, transition_event,
    'provider', 'payment-attempt:' || NEW.id::text, NEW.updated_at
  );

  RETURN NEW;
END
$$;

DROP TRIGGER IF EXISTS trg_commerce_sync_payment_intent_from_attempt
  ON commerce_payment_attempt;
CREATE TRIGGER trg_commerce_sync_payment_intent_from_attempt
  AFTER UPDATE OF status, payment_intent_id ON commerce_payment_attempt
  FOR EACH ROW
  EXECUTE FUNCTION commerce_sync_payment_intent_from_attempt();

-- Verified refund completion is a positive provider fact, unlike a transport
-- failure. Synchronize in the same transaction, exactly once per refund row.
CREATE OR REPLACE FUNCTION commerce_sync_payment_intent_from_refund()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  target_intent_id UUID;
  prior_status TEXT;
  next_status TEXT;
BEGIN
  IF NEW.status <> 'succeeded' OR OLD.status = 'succeeded' THEN RETURN NEW; END IF;
  SELECT payment_intent_id INTO target_intent_id
    FROM commerce_payment_attempt WHERE id = NEW.payment_attempt_id;
  IF target_intent_id IS NULL THEN RETURN NEW; END IF;
  SELECT status INTO prior_status FROM commerce_payment_intent
    WHERE id = target_intent_id FOR UPDATE;
  IF OLD.status <> 'processing' OR NEW.provider_refund_id IS NULL OR NEW.completed_at IS NULL
    OR prior_status NOT IN ('captured', 'partially_captured', 'partially_refunded')
    OR NOT EXISTS (
      SELECT 1 FROM commerce_payment_attempt attempt
      JOIN commerce_checkout_session checkout ON checkout.id = attempt.checkout_id
      WHERE attempt.id = NEW.payment_attempt_id AND attempt.status = 'succeeded'
        AND attempt.checkout_id = NEW.checkout_id AND attempt.provider = NEW.provider
        AND attempt.environment = NEW.environment AND checkout.environment = NEW.environment
        AND attempt.merchant_account_ref = NEW.merchant_account_ref
        AND attempt.currency = NEW.currency
    ) THEN
    RAISE EXCEPTION 'Verified refund does not match its canonical payment binding' USING ERRCODE = '23514';
  END IF;
  UPDATE commerce_payment_intent
    SET refunded_minor = refunded_minor + NEW.amount_minor,
        status = CASE WHEN refunded_minor + NEW.amount_minor = captured_minor
          THEN 'refunded' ELSE 'partially_refunded' END,
        updated_at = NEW.updated_at
    WHERE id = target_intent_id AND checkout_id = NEW.checkout_id
      AND provider = NEW.provider AND currency = NEW.currency
      AND NEW.amount_minor > 0 AND NEW.amount_minor <= captured_minor - refunded_minor
    RETURNING status INTO next_status;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Verified refund exceeds or mismatches canonical captured balance' USING ERRCODE = '23514';
  END IF;
  INSERT INTO commerce_payment_state_history (
    payment_intent_id, from_status, to_status, event_type, actor_type, correlation_id, occurred_at
  ) VALUES (
    target_intent_id, prior_status, next_status, 'refund_completion_verified', 'provider',
    'refund:' || NEW.id::text, NEW.completed_at
  );
  RETURN NEW;
END
$$;
DROP TRIGGER IF EXISTS trg_commerce_sync_payment_intent_from_refund ON commerce_refund;
CREATE TRIGGER trg_commerce_sync_payment_intent_from_refund
  AFTER UPDATE OF status ON commerce_refund FOR EACH ROW
  EXECUTE FUNCTION commerce_sync_payment_intent_from_refund();

COMMIT;
