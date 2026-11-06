-- PAY-DOMO-CANCEL-001 (AUTHORITY-093): one immutable cancellation record per
-- Domo quote. It fixes the policy version, lead time and collected amounts at
-- the instant of cancellation and carries the refund obligation until a refund
-- request for exactly that amount is linked. Collected money is therefore
-- never left unaccounted: retained + refund_due = collected, and an open
-- obligation (refund_due > 0 without a refund) is visible in
-- domo_quote_cancellation_open_refund.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE TABLE IF NOT EXISTS public.domo_quote_cancellation (
  id BIGSERIAL PRIMARY KEY,
  domo_quote_id UUID NOT NULL UNIQUE REFERENCES public.domo_event_quote_runtime(id) ON DELETE RESTRICT,
  policy_version INTEGER REFERENCES public.domo_cancellation_policy(version),
  canceller TEXT NOT NULL CHECK (canceller IN ('customer', 'tdf')),
  actor_type TEXT NOT NULL CHECK (actor_type IN ('customer', 'operator')),
  actor_party_id BIGINT REFERENCES public.party(id),
  cancelled_at TIMESTAMPTZ NOT NULL,
  event_starts_at TIMESTAMPTZ NOT NULL,
  lead_days INTEGER NOT NULL,
  in_progress BOOLEAN NOT NULL,
  retained_basis_points INTEGER NOT NULL CHECK (retained_basis_points BETWEEN 0 AND 10000),
  deposit_collected_minor BIGINT NOT NULL CHECK (deposit_collected_minor >= 0),
  balance_collected_minor BIGINT NOT NULL CHECK (balance_collected_minor >= 0),
  retained_minor BIGINT NOT NULL CHECK (retained_minor >= 0),
  refund_due_minor BIGINT NOT NULL CHECK (refund_due_minor >= 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  reason_code TEXT NOT NULL,
  note TEXT,
  refund_status TEXT NOT NULL CHECK (refund_status IN ('none', 'awaiting_operator', 'requested')),
  refund_id UUID UNIQUE REFERENCES public.commerce_refund(id),
  refund_linked_at TIMESTAMPTZ,
  -- Money conservation: nothing collected is lost or created.
  CHECK (retained_minor + refund_due_minor = deposit_collected_minor + balance_collected_minor),
  -- Only the deposit may be retained; the collected balance is always refunded.
  CHECK (retained_minor <= deposit_collected_minor),
  -- An operator actor is an authenticated party; a customer actor is not.
  CHECK ((actor_type = 'operator') = (actor_party_id IS NOT NULL)),
  -- Collected money requires the approved policy version that priced the refund.
  CHECK (deposit_collected_minor + balance_collected_minor = 0 OR policy_version IS NOT NULL),
  -- Refund status is a function of the obligation and the linked refund.
  CHECK ((refund_due_minor = 0) = (refund_status = 'none')),
  CHECK ((refund_id IS NOT NULL) = (refund_status = 'requested')),
  CHECK ((refund_id IS NOT NULL) = (refund_linked_at IS NOT NULL))
);

-- The refund linked to a cancellation is a refund of the quote's own checkout
-- for exactly the obligation.
CREATE OR REPLACE FUNCTION public.domo_quote_cancellation_validate()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  runtime_checkout UUID;
  linked_checkout UUID;
  linked_amount BIGINT;
  linked_currency TEXT;
BEGIN
  IF TG_OP = 'UPDATE' THEN
    -- Immutable except for linking the refund exactly once.
    IF (to_jsonb(OLD) - 'refund_id' - 'refund_status' - 'refund_linked_at')
       IS DISTINCT FROM (to_jsonb(NEW) - 'refund_id' - 'refund_status' - 'refund_linked_at') THEN
      RAISE EXCEPTION 'Domo cancellation records are immutable' USING ERRCODE = 'check_violation';
    END IF;
    IF OLD.refund_id IS NOT NULL AND NEW.refund_id IS DISTINCT FROM OLD.refund_id THEN
      RAISE EXCEPTION 'A linked Domo cancellation refund cannot be changed' USING ERRCODE = 'check_violation';
    END IF;
  END IF;
  IF NEW.refund_id IS NOT NULL THEN
    SELECT checkout_id INTO runtime_checkout FROM public.domo_event_quote_runtime WHERE id = NEW.domo_quote_id;
    SELECT checkout_id, amount_minor, currency INTO linked_checkout, linked_amount, linked_currency
      FROM public.commerce_refund WHERE id = NEW.refund_id;
    IF linked_checkout IS DISTINCT FROM runtime_checkout
       OR linked_amount IS DISTINCT FROM NEW.refund_due_minor
       OR linked_currency IS DISTINCT FROM NEW.currency THEN
      RAISE EXCEPTION 'Domo cancellation refund must refund the quote checkout for exactly the obligation'
        USING ERRCODE = 'check_violation';
    END IF;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_domo_quote_cancellation_validate ON public.domo_quote_cancellation;
CREATE TRIGGER trg_domo_quote_cancellation_validate
  BEFORE INSERT OR UPDATE ON public.domo_quote_cancellation
  FOR EACH ROW EXECUTE FUNCTION public.domo_quote_cancellation_validate();

-- A cancelled quote with collected money must carry its cancellation record.
CREATE OR REPLACE FUNCTION public.domo_quote_cancellation_required()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.quote_status = 'cancelled' AND OLD.quote_status <> 'cancelled'
     AND EXISTS (SELECT 1 FROM public.commerce_checkout_session checkout
                 WHERE checkout.id = NEW.checkout_id AND checkout.paid_minor > checkout.refunded_minor)
     AND NOT EXISTS (SELECT 1 FROM public.domo_quote_cancellation c WHERE c.domo_quote_id = NEW.id) THEN
    RAISE EXCEPTION 'Cancelling a paid Domo quote requires its cancellation record (PAY-DOMO-CANCEL-001)'
      USING ERRCODE = 'check_violation';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_domo_quote_cancellation_required ON public.domo_event_quote_runtime;
CREATE CONSTRAINT TRIGGER trg_domo_quote_cancellation_required
  AFTER UPDATE OF quote_status ON public.domo_event_quote_runtime
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION public.domo_quote_cancellation_required();

CREATE OR REPLACE VIEW public.domo_quote_cancellation_open_refund AS
  SELECT c.id, c.domo_quote_id, c.cancelled_at, c.refund_due_minor, c.currency, c.canceller, c.actor_type
  FROM public.domo_quote_cancellation c
  WHERE c.refund_due_minor > 0 AND c.refund_id IS NULL;

COMMIT;
