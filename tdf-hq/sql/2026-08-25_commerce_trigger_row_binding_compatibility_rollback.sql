-- Emergency rollback for the commerce trigger row-binding compatibility fix.
-- Freeze checkout writes and roll back the application before applying it.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE OR REPLACE FUNCTION commerce_validate_payment_attempt()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout commerce_checkout_session%ROWTYPE;
BEGIN
  SELECT * INTO checkout
    FROM commerce_checkout_session
    WHERE id = NEW.checkout_id
    FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Unknown checkout session %', NEW.checkout_id;
  END IF;
  IF NEW.environment <> checkout.environment
     OR NEW.amount_minor <> checkout.total_minor
     OR NEW.currency <> checkout.currency THEN
    RAISE EXCEPTION 'Payment attempt does not match checkout environment, amount, or currency';
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION commerce_validate_provider_binding()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  attempt commerce_payment_attempt%ROWTYPE;
BEGIN
  SELECT * INTO attempt
    FROM commerce_payment_attempt
    WHERE id = NEW.payment_attempt_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Unknown payment attempt %', NEW.payment_attempt_id;
  END IF;
  IF NEW.provider <> attempt.provider
     OR NEW.environment <> attempt.environment
     OR NEW.merchant_account_ref <> attempt.merchant_account_ref
     OR NEW.amount_minor <> attempt.amount_minor
     OR NEW.currency <> attempt.currency THEN
    RAISE EXCEPTION 'Provider binding does not match its payment attempt';
  END IF;
  RETURN NEW;
END $$;

COMMIT;
