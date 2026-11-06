-- Rollback of 2026-11-02_course_checkout_deposit_policy. Refuses when any
-- approved policy carries values other than the defaults, since dropping them
-- would silently change how its deposits are priced.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM public.course_checkout_policy
    WHERE approval_status IN ('approved', 'retired')
      AND (minimum_deposit_minor <> 500 OR full_payment_above_bps <> 9000)
  ) THEN
    RAISE EXCEPTION 'Approved course policies carry non-default deposit policy values; rollback refused';
  END IF;
END $$;
-- Restore the 2026-08-17 immutability comparison without the dropped columns.
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
       NEW.transfer_allowed
     ) IS DISTINCT FROM ROW(
       OLD.course_id, OLD.policy_version, OLD.currency, OLD.price_minor,
       OLD.tax_bps, OLD.payment_mode, OLD.deposit_bps, OLD.hold_minutes,
       OLD.terms_version, OLD.terms_summary, OLD.cancellation_policy,
       OLD.transfer_allowed
     ) THEN
    RAISE EXCEPTION 'Approved course checkout policy is immutable; create a new version';
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;
ALTER TABLE public.course_checkout_policy
  DROP CONSTRAINT IF EXISTS course_checkout_policy_deposit_policy_shape;
ALTER TABLE public.course_checkout_policy
  DROP COLUMN IF EXISTS minimum_deposit_minor,
  DROP COLUMN IF EXISTS full_payment_above_bps;
COMMIT;
