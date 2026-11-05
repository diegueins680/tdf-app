-- PAY-DEPOSIT-POLICY-001 (AUTHORITY-095): the deposit minimum and the
-- full-payment switch become part of each approved course checkout policy
-- instead of code defaults. Existing policies receive the owner-accepted
-- defaults (5.00 minimum, full payment above 90%), so no priced checkout
-- changes.
BEGIN;
SET LOCAL lock_timeout = '10s';

ALTER TABLE public.course_checkout_policy
  ADD COLUMN IF NOT EXISTS minimum_deposit_minor BIGINT NOT NULL DEFAULT 500,
  ADD COLUMN IF NOT EXISTS full_payment_above_bps INTEGER NOT NULL DEFAULT 9000;

ALTER TABLE public.course_checkout_policy
  DROP CONSTRAINT IF EXISTS course_checkout_policy_deposit_policy_shape;
ALTER TABLE public.course_checkout_policy
  ADD CONSTRAINT course_checkout_policy_deposit_policy_shape
  CHECK (minimum_deposit_minor >= 0 AND full_payment_above_bps BETWEEN 0 AND 10000);

-- The deposit policy values share the approved row's immutability.
CREATE OR REPLACE FUNCTION course_checkout_validate_policy()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.active AND NOT EXISTS (
    SELECT 1 FROM course
    WHERE id = NEW.course_id
      AND price_cents = NEW.price_minor
      AND upper(currency) = NEW.currency
      AND capacity > 0
  ) THEN
    RAISE EXCEPTION 'Active course policy must match the authoritative course price and currency';
  END IF;
  IF TG_OP = 'UPDATE' AND OLD.approval_status = 'approved'
     AND ROW(
       NEW.course_id, NEW.policy_version, NEW.currency, NEW.price_minor,
       NEW.tax_bps, NEW.payment_mode, NEW.deposit_bps, NEW.hold_minutes,
       NEW.terms_version, NEW.terms_summary, NEW.cancellation_policy,
       NEW.transfer_allowed, NEW.minimum_deposit_minor, NEW.full_payment_above_bps
     ) IS DISTINCT FROM ROW(
       OLD.course_id, OLD.policy_version, OLD.currency, OLD.price_minor,
       OLD.tax_bps, OLD.payment_mode, OLD.deposit_bps, OLD.hold_minutes,
       OLD.terms_version, OLD.terms_summary, OLD.cancellation_policy,
       OLD.transfer_allowed, OLD.minimum_deposit_minor, OLD.full_payment_above_bps
     ) THEN
    RAISE EXCEPTION 'Approved course checkout policy is immutable; create a new version';
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

COMMIT;
