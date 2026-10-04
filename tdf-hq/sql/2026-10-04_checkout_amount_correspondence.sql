-- PAY-CHECKOUT-002: every committed checkout has immutable monetary terms and
-- a nonempty immutable line snapshot with the same exact payable total.
-- Merchandise represents shipping as a header fee and a separate payable line;
-- component-by-component subtotal equality is therefore NOT the contract.
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';
LOCK TABLE commerce_checkout_session, commerce_checkout_line_item IN SHARE ROW EXCLUSIVE MODE;

-- Never manufacture financial evidence to repair a historical mismatch.
DO $preflight$
BEGIN
  IF EXISTS (
    SELECT 1 FROM commerce_checkout_session c
    LEFT JOIN LATERAL (
      SELECT count(*) AS line_count, sum(total_minor::numeric) AS total
      FROM commerce_checkout_line_item WHERE checkout_id = c.id
    ) lines ON true
    WHERE lines.line_count = 0 OR lines.total IS DISTINCT FROM c.total_minor::numeric
  ) THEN
    RAISE EXCEPTION 'Checkout amount correspondence preflight failed; reconcile existing immutable evidence before applying';
  END IF;
END
$preflight$;

-- An old repeatable-read snapshot may predate the now-valid preflight state.
-- It cannot see this row created by the enforcing migration, so it must retry
-- rather than validate a newly appended line against obsolete financial rows.
CREATE TABLE IF NOT EXISTS commerce_checkout_amount_boundary (
  singleton boolean PRIMARY KEY CHECK (singleton),
  established_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
INSERT INTO commerce_checkout_amount_boundary(singleton) VALUES (true)
ON CONFLICT DO NOTHING;

CREATE OR REPLACE FUNCTION commerce_protect_checkout_money()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF (NEW.currency, NEW.subtotal_minor, NEW.discount_minor, NEW.tax_minor,
      NEW.fee_minor, NEW.total_minor) IS DISTINCT FROM
     (OLD.currency, OLD.subtotal_minor, OLD.discount_minor, OLD.tax_minor,
      OLD.fee_minor, OLD.total_minor) THEN
    RAISE EXCEPTION 'Checkout monetary terms are immutable' USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END $$;
CREATE TRIGGER trg_commerce_checkout_money_immutable
  BEFORE UPDATE ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_checkout_money();

CREATE OR REPLACE FUNCTION commerce_check_checkout_line_total()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  target_id uuid;
  expected_total bigint;
  actual_total numeric;
  line_count bigint;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM commerce_checkout_amount_boundary WHERE singleton) THEN
    RAISE EXCEPTION 'Checkout snapshot predates monetary enforcement; retry transaction'
      USING ERRCODE = '40001';
  END IF;
  IF TG_TABLE_NAME = 'commerce_checkout_session' THEN
    target_id := NEW.id;
  ELSE
    target_id := NEW.checkout_id;
  END IF;
  SELECT total_minor INTO expected_total FROM commerce_checkout_session WHERE id = target_id;
  -- A checkout inserted and deleted within one transaction leaves no snapshot.
  IF NOT FOUND THEN RETURN NULL; END IF;
  SELECT count(*), sum(total_minor::numeric) INTO line_count, actual_total
    FROM commerce_checkout_line_item WHERE checkout_id = target_id;
  IF line_count = 0 OR actual_total IS DISTINCT FROM expected_total::numeric THEN
    RAISE EXCEPTION 'Checkout payable total does not match its nonempty line snapshot'
      USING ERRCODE = '23514';
  END IF;
  RETURN NULL;
END $$;

-- Header and all lines are constructed in one transaction by both current
-- writers. Initial deferral allows that construction, never an incomplete COMMIT.
CREATE CONSTRAINT TRIGGER trg_commerce_checkout_total
  AFTER INSERT ON commerce_checkout_session
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION commerce_check_checkout_line_total();
CREATE CONSTRAINT TRIGGER trg_commerce_checkout_line_total
  AFTER INSERT ON commerce_checkout_line_item
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION commerce_check_checkout_line_total();

-- Concurrency argument: existing line UPDATE/DELETE is prohibited, parent money
-- is immutable, and new line totals are nonnegative. A committed existing parent
-- already sums exactly; a positive append cannot pass alone or in a race. Zero
-- appends preserve the invariant. An uncommitted parent cannot be referenced by
-- another transaction until its foreign-key check sees the committed parent.
-- Pre-enforcement repeatable-read snapshots fail the visibility fence above.
-- Assumes normal triggers/constraints enabled; privileged DDL is outside scope.
-- No down migration: retain constraints for compatible forward recovery.
COMMIT;
