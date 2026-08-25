-- Replace ambiguous composite-row trigger variables without mutating the
-- already-applied 2026-08-13 checkout migration.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE OR REPLACE FUNCTION commerce_validate_payment_attempt()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout_environment commerce_checkout_session.environment%TYPE;
  checkout_total_minor commerce_checkout_session.total_minor%TYPE;
  checkout_currency commerce_checkout_session.currency%TYPE;
BEGIN
  SELECT environment, total_minor, currency
    INTO checkout_environment, checkout_total_minor, checkout_currency
    FROM commerce_checkout_session
    WHERE id = NEW.checkout_id
    FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Unknown checkout session %', NEW.checkout_id;
  END IF;
  IF NEW.environment <> checkout_environment
     OR NEW.amount_minor <> checkout_total_minor
     OR NEW.currency <> checkout_currency THEN
    RAISE EXCEPTION 'Payment attempt does not match checkout environment, amount, or currency';
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION commerce_validate_provider_binding()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  attempt_provider commerce_payment_attempt.provider%TYPE;
  attempt_environment commerce_payment_attempt.environment%TYPE;
  attempt_merchant_account_ref commerce_payment_attempt.merchant_account_ref%TYPE;
  attempt_amount_minor commerce_payment_attempt.amount_minor%TYPE;
  attempt_currency commerce_payment_attempt.currency%TYPE;
BEGIN
  SELECT provider, environment, merchant_account_ref, amount_minor, currency
    INTO attempt_provider, attempt_environment, attempt_merchant_account_ref,
      attempt_amount_minor, attempt_currency
    FROM commerce_payment_attempt
    WHERE id = NEW.payment_attempt_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Unknown payment attempt %', NEW.payment_attempt_id;
  END IF;
  IF NEW.provider <> attempt_provider
     OR NEW.environment <> attempt_environment
     OR NEW.merchant_account_ref <> attempt_merchant_account_ref
     OR NEW.amount_minor <> attempt_amount_minor
     OR NEW.currency <> attempt_currency THEN
    RAISE EXCEPTION 'Provider binding does not match its payment attempt';
  END IF;
  RETURN NEW;
END $$;

COMMIT;
