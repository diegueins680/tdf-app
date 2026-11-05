-- PAY-DOMO-CANCEL-001 (AUTHORITY-093): versioned, approved refund policy for
-- cancelling a Domo quote after money was collected. The application reads the
-- single approved version; cancellation creates the refund request for the
-- amount the policy computes in the same transaction.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE TABLE IF NOT EXISTS public.domo_cancellation_policy (
  id BIGSERIAL PRIMARY KEY,
  version INTEGER NOT NULL UNIQUE CHECK (version >= 1),
  full_refund_min_days INTEGER NOT NULL CHECK (full_refund_min_days >= 1),
  partial_refund_min_days INTEGER NOT NULL CHECK (partial_refund_min_days >= 0),
  partial_retained_bps INTEGER NOT NULL CHECK (partial_retained_bps BETWEEN 0 AND 10000),
  balance_refunded_in_full BOOLEAN NOT NULL DEFAULT TRUE,
  status TEXT NOT NULL CHECK (status IN ('draft', 'approved', 'retired')),
  approved_by BIGINT REFERENCES public.party(id),
  approved_at TIMESTAMPTZ,
  authority TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (full_refund_min_days > partial_refund_min_days),
  CHECK ((approved_at IS NOT NULL) = (status IN ('approved', 'retired')))
);

-- Exactly one approved policy at a time.
CREATE UNIQUE INDEX IF NOT EXISTS uq_domo_cancellation_policy_approved
  ON public.domo_cancellation_policy((status)) WHERE status = 'approved';

-- Approved and retired rows are immutable except for retirement.
CREATE OR REPLACE FUNCTION public.domo_cancellation_policy_protect()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.status IN ('approved', 'retired') THEN
    IF NOT (OLD.status = 'approved' AND NEW.status = 'retired'
            AND (to_jsonb(NEW) - 'status') = (to_jsonb(OLD) - 'status')) THEN
      RAISE EXCEPTION 'An approved Domo cancellation policy is immutable; create a new version'
        USING ERRCODE = 'check_violation';
    END IF;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_domo_cancellation_policy_protect ON public.domo_cancellation_policy;
CREATE TRIGGER trg_domo_cancellation_policy_protect
  BEFORE UPDATE ON public.domo_cancellation_policy
  FOR EACH ROW EXECUTE FUNCTION public.domo_cancellation_policy_protect();

INSERT INTO public.domo_cancellation_policy
  (version, full_refund_min_days, partial_refund_min_days, partial_retained_bps,
   balance_refunded_in_full, status, approved_at, authority)
VALUES (1, 31, 7, 5000, TRUE, 'approved', now(),
        'AUTHORITY-093: owner accepted the audit recommendation on 2026-11-01')
ON CONFLICT (version) DO NOTHING;

COMMIT;
