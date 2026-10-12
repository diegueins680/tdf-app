-- PAY-CHECKOUT-002: the line snapshot of a committed checkout is fixed.
-- The 2026-10-04 rules refuse a positive append (the deferred sum check) and
-- any update or delete of a line, but a line whose total is zero could still
-- be appended to a committed checkout by any later transaction, and a line
-- could be appended to another transaction's checkout as soon as its header
-- committed. Lines are now created only in the transaction that creates
-- their checkout; the deferred sum check keeps validating that snapshot at
-- commit. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE OR REPLACE FUNCTION commerce_reject_late_checkout_line()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout_age integer;
BEGIN
  -- age(xmin) is positive for a row written by an earlier transaction and
  -- zero or negative for a row written by this transaction or one of its
  -- subtransactions; a frozen row counts as infinitely old.
  SELECT age(xmin) INTO checkout_age
    FROM commerce_checkout_session
    WHERE id = NEW.checkout_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Checkout % is not visible to this transaction; lines are created with their checkout', NEW.checkout_id
      USING ERRCODE = 'check_violation';
  END IF;
  IF checkout_age > 0 THEN
    RAISE EXCEPTION 'Checkout % is committed; its line snapshot is fixed', NEW.checkout_id
      USING ERRCODE = 'check_violation';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_checkout_line_fixed ON commerce_checkout_line_item;
CREATE TRIGGER trg_commerce_checkout_line_fixed
  BEFORE INSERT ON commerce_checkout_line_item
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_late_checkout_line();

COMMIT;
