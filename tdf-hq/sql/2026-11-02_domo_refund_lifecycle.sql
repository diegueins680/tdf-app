-- AUTHORITY-093 / PAY-DOMO-CANCEL-001. Additive; no provider calls or money
-- movement. The approved cancellation policy and immutable pricing stay intact.
BEGIN;
SET LOCAL lock_timeout='10s';

CREATE TABLE IF NOT EXISTS public.domo_manual_refund_evidence (
  refund_id UUID PRIMARY KEY REFERENCES public.commerce_refund(id) ON DELETE RESTRICT,
  provider TEXT NOT NULL,
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  merchant_account_ref TEXT NOT NULL,
  external_reference TEXT NOT NULL,
  evidence_reference TEXT NOT NULL CHECK (length(evidence_reference) BETWEEN 16 AND 256),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_by BIGINT NOT NULL REFERENCES public.party(id),
  recorded_at TIMESTAMPTZ NOT NULL,
  confirmed_by BIGINT REFERENCES public.party(id),
  confirmed_at TIMESTAMPTZ,
  CHECK ((confirmed_by IS NULL) = (confirmed_at IS NULL)),
  CHECK (confirmed_by IS NULL OR confirmed_by <> recorded_by),
  UNIQUE(provider,environment,merchant_account_ref,external_reference)
);

-- PostgreSQL limits regex repetition bounds to 255. Keep the separately
-- reviewed 256-character contract without an invalid regex, also on reapply.
ALTER TABLE public.domo_manual_refund_evidence
 DROP CONSTRAINT IF EXISTS domo_manual_refund_evidence_external_reference_check,
 DROP CONSTRAINT IF EXISTS domo_manual_refund_external_reference_shape;
ALTER TABLE public.domo_manual_refund_evidence
 ADD CONSTRAINT domo_manual_refund_external_reference_shape
 CHECK (length(external_reference) BETWEEN 1 AND 256 AND external_reference ~ '^[A-Za-z0-9_.-]+$');

-- These raw-SQL references already appear through PostgreSQL FK discovery.
-- Install the same archived-identity write guard as the other financial tables.
DROP TRIGGER IF EXISTS identity_archive_reference_guard ON public.domo_manual_refund_evidence;
CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE
 ON public.domo_manual_refund_evidence FOR EACH ROW
 EXECUTE FUNCTION public.identity_reject_archived_reference('recorded_by','confirmed_by');

CREATE OR REPLACE FUNCTION public.domo_manual_refund_validate()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP='DELETE' THEN
    RAISE EXCEPTION 'Manual refund evidence cannot be deleted';
  END IF;
  IF TG_OP='UPDATE' AND (
      (to_jsonb(OLD)-'confirmed_by'-'confirmed_at') IS DISTINCT FROM
      (to_jsonb(NEW)-'confirmed_by'-'confirmed_at') OR
      (OLD.confirmed_by IS NOT NULL AND to_jsonb(OLD) IS DISTINCT FROM to_jsonb(NEW))) THEN
    RAISE EXCEPTION 'Manual refund evidence is immutable';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM public.commerce_refund r
    JOIN public.domo_quote_cancellation c ON c.refund_id=r.id
    JOIN public.domo_event_quote_runtime q ON q.id=c.domo_quote_id
    WHERE r.id=NEW.refund_id AND r.checkout_id=q.checkout_id
      AND r.provider <> 'paypal' AND r.provider=NEW.provider
      AND r.environment=NEW.environment AND r.merchant_account_ref=NEW.merchant_account_ref
      AND r.amount_minor=NEW.amount_minor AND r.currency=NEW.currency
      AND r.amount_minor=c.refund_due_minor AND r.currency=c.currency
      AND r.approved_by IS NOT NULL AND r.approved_by<>r.requested_by
      AND r.status IN ('processing','succeeded')
      AND NEW.recorded_by<>r.requested_by
      AND (NEW.confirmed_by IS NULL OR NEW.confirmed_by<>r.requested_by)
  ) THEN RAISE EXCEPTION 'Manual refund evidence does not match an independently approved Domo refund';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_domo_manual_refund_validate ON public.domo_manual_refund_evidence;
CREATE TRIGGER trg_domo_manual_refund_validate BEFORE INSERT OR UPDATE OR DELETE
  ON public.domo_manual_refund_evidence FOR EACH ROW EXECUTE FUNCTION public.domo_manual_refund_validate();

-- An obligation remains outstanding after request/approval/POST/pending/error.
CREATE OR REPLACE VIEW public.domo_quote_cancellation_open_refund AS
 SELECT c.id,c.domo_quote_id,c.cancelled_at,c.refund_due_minor,c.currency,c.canceller,c.actor_type
 FROM public.domo_quote_cancellation c LEFT JOIN public.commerce_refund r ON r.id=c.refund_id
 WHERE c.refund_due_minor>0 AND (r.id IS NULL OR r.status<>'succeeded');

CREATE OR REPLACE FUNCTION public.domo_refund_close_reconciliation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.status='succeeded' AND OLD.status IS DISTINCT FROM 'succeeded' THEN
    UPDATE public.commerce_reconciliation_exception e
      SET status='resolved',resolved_at=NEW.completed_at,
          resolution_notes='Verified canonical Domo refund: '||NEW.id::text
      FROM public.domo_quote_cancellation c JOIN public.domo_event_quote_runtime q ON q.id=c.domo_quote_id
      WHERE c.refund_id=NEW.id AND q.checkout_id=NEW.checkout_id
        AND c.refund_due_minor=NEW.amount_minor AND c.currency=NEW.currency
        AND e.exception_type='domo_cancellation_refund_due'
        AND e.internal_reference='domo-quote:'||c.domo_quote_id::text
        AND e.provider=NEW.provider AND e.environment=NEW.environment
        AND e.merchant_account_ref=NEW.merchant_account_ref
        AND e.expected_amount_minor=NEW.amount_minor AND e.currency=NEW.currency
        AND e.status IN ('open','assigned');
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_domo_refund_close_reconciliation ON public.commerce_refund;
CREATE TRIGGER trg_domo_refund_close_reconciliation AFTER UPDATE OF status
  ON public.commerce_refund FOR EACH ROW EXECUTE FUNCTION public.domo_refund_close_reconciliation();

-- Operator cancellations written by the earlier proposal linked a request but
-- did not always create a reconciliation queue entry. Surface those obligations
-- without changing the immutable request or its financial state.
INSERT INTO public.commerce_reconciliation_exception
 (provider,environment,merchant_account_ref,exception_type,internal_reference,
  provider_reference,expected_amount_minor,currency,status,detected_at)
 SELECT r.provider,r.environment,r.merchant_account_ref,'domo_cancellation_refund_due',
   'domo-quote:'||c.domo_quote_id::text,r.payment_attempt_id::text,c.refund_due_minor,c.currency,'open',c.cancelled_at
 FROM public.domo_quote_cancellation c
 JOIN public.domo_event_quote_runtime q ON q.id=c.domo_quote_id
 JOIN public.commerce_refund r ON r.id=c.refund_id AND r.checkout_id=q.checkout_id
 WHERE c.refund_due_minor>0 AND r.amount_minor=c.refund_due_minor AND r.currency=c.currency
   AND r.status<>'succeeded' AND NOT EXISTS (
     SELECT 1 FROM public.commerce_reconciliation_exception e
     WHERE e.exception_type='domo_cancellation_refund_due'
       AND e.internal_reference='domo-quote:'||c.domo_quote_id::text
       AND e.provider=r.provider AND e.environment=r.environment AND e.merchant_account_ref=r.merchant_account_ref)
 ON CONFLICT DO NOTHING;

-- Preserve the prior closure and its time in the canonical audit; retain free
-- form notes in their original restricted row, never copy them to audit metadata.
INSERT INTO public.commerce_checkout_audit_event(checkout_id,event_type,actor_type,correlation_id,metadata)
 SELECT q.checkout_id,'domo_refund_reconciliation_reopened','system','domo-refund-migration:'||e.id::text,
   jsonb_build_object('exception_id',e.id::text,'previous_status',e.status,'previous_resolved_at',e.resolved_at)
 FROM public.commerce_reconciliation_exception e
 JOIN public.domo_quote_cancellation c ON e.internal_reference='domo-quote:'||c.domo_quote_id::text
 JOIN public.domo_event_quote_runtime q ON q.id=c.domo_quote_id
 LEFT JOIN public.commerce_refund r ON r.id=c.refund_id
 WHERE e.exception_type='domo_cancellation_refund_due' AND c.refund_due_minor>0
   AND (r.id IS NULL OR r.status<>'succeeded') AND e.status IN ('resolved','ignored');

-- Repair only premature reconciliation labels introduced by the proposal.
-- Never reinterpret a completed refund or alter any amount/provider status.
UPDATE public.commerce_reconciliation_exception e SET status='open',resolved_at=NULL,
  resolution_notes=COALESCE(e.resolution_notes||E'\n','')||'Reopened: a refund request is not evidence of returned money'
 FROM public.domo_quote_cancellation c LEFT JOIN public.commerce_refund r ON r.id=c.refund_id
 WHERE e.exception_type='domo_cancellation_refund_due'
   AND e.internal_reference='domo-quote:'||c.domo_quote_id::text
   AND c.refund_due_minor>0 AND (r.id IS NULL OR r.status<>'succeeded') AND e.status IN ('resolved','ignored');

-- The generic reconciliation administration path must respect the same debt
-- finality rule. A user cannot resolve/ignore this financial obligation merely
-- because its request was approved or accepted by a provider.
CREATE OR REPLACE FUNCTION public.domo_refund_require_verified_closure()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF NEW.exception_type='domo_cancellation_refund_due' AND NEW.status IN ('resolved','ignored')
    AND (TG_OP='INSERT' OR OLD.status IS DISTINCT FROM NEW.status) AND NOT EXISTS (
      SELECT 1 FROM public.domo_quote_cancellation c
      JOIN public.domo_event_quote_runtime q ON q.id=c.domo_quote_id
      JOIN public.commerce_refund r ON r.id=c.refund_id AND r.checkout_id=q.checkout_id
      WHERE NEW.internal_reference='domo-quote:'||c.domo_quote_id::text
        AND r.status='succeeded' AND r.provider=NEW.provider AND r.environment=NEW.environment
        AND r.merchant_account_ref=NEW.merchant_account_ref AND r.amount_minor=c.refund_due_minor
        AND NEW.expected_amount_minor=c.refund_due_minor AND r.currency=c.currency AND NEW.currency=c.currency
    ) THEN
   RAISE EXCEPTION 'Domo reconciliation requires verified refund completion';
 END IF;
 RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_domo_refund_require_verified_closure ON public.commerce_reconciliation_exception;
CREATE TRIGGER trg_domo_refund_require_verified_closure BEFORE INSERT OR UPDATE
 ON public.commerce_reconciliation_exception FOR EACH ROW EXECUTE FUNCTION public.domo_refund_require_verified_closure();

INSERT INTO public.revenue_feature_flag(flag_key,environment,enabled,reason)
 VALUES ('domo.refund_manual','sandbox',FALSE,'Enable only after manual evidence and independent review are staffed'),
        ('domo.refund_manual','production',FALSE,'Disabled pending separate operational approval')
 ON CONFLICT DO NOTHING;
COMMIT;
