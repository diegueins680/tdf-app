-- PAY-CHECKOUT-002: the line snapshot of a committed checkout is fixed.
-- The 2026-10-04 rules refuse a positive append (the deferred sum check) and
-- any update or delete of a line, but a line whose total is zero could still
-- be appended to a committed checkout by any later transaction. Lines are now
-- created only in the transaction that creates their checkout: the header
-- insert records the checkout id in a transaction-local setting, and a line
-- is accepted only for a checkout recorded there. The setting is scoped to
-- the transaction (SET LOCAL semantics, rolled back with a subtransaction),
-- so a header committed by any other transaction, a header this transaction
-- merely updated, and a header another session has not committed are all
-- equally "not created here". The deferred sum check keeps validating the
-- snapshot at commit. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE OR REPLACE FUNCTION commerce_register_checkout_creation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM set_config('tdf.commerce_checkouts_created',
    coalesce(current_setting('tdf.commerce_checkouts_created', true), '') || ',' || NEW.id::text || ',',
    true);
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_checkout_created ON commerce_checkout_session;
CREATE TRIGGER trg_commerce_checkout_created
  BEFORE INSERT ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION commerce_register_checkout_creation();

CREATE OR REPLACE FUNCTION commerce_reject_late_checkout_line()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF position(',' || NEW.checkout_id::text || ',' IN
       coalesce(current_setting('tdf.commerce_checkouts_created', true), '')) = 0 THEN
    RAISE EXCEPTION 'Checkout % was not created by this transaction; lines are created with their checkout', NEW.checkout_id
      USING ERRCODE = 'check_violation';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_checkout_line_fixed ON commerce_checkout_line_item;
CREATE TRIGGER trg_commerce_checkout_line_fixed
  BEFORE INSERT ON commerce_checkout_line_item
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_late_checkout_line();

COMMIT;
