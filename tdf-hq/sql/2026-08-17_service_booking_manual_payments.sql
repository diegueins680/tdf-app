-- Audited manual-payment evidence for canonical service bookings.
-- A customer reference is evidence for staff review, never payment success.
BEGIN;

ALTER TABLE commerce_manual_payment_evidence
  ADD COLUMN IF NOT EXISTS submitted_by BIGINT;

-- This canonical index was introduced by the service-storefront rollout. Keep
-- booking-only installations self-contained while preserving the shared name.
CREATE UNIQUE INDEX IF NOT EXISTS uq_commerce_manual_evidence_attempt
  ON commerce_manual_payment_evidence(payment_attempt_id);

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'commerce_manual_payment_evidence'::regclass
      AND conname = 'fk_commerce_manual_evidence_submitted_by'
  ) THEN
    ALTER TABLE commerce_manual_payment_evidence
      ADD CONSTRAINT fk_commerce_manual_evidence_submitted_by
      FOREIGN KEY (submitted_by) REFERENCES party(id) ON DELETE RESTRICT NOT VALID;
  END IF;
END $$;

CREATE OR REPLACE VIEW commerce_manual_payment_evidence_review_report AS
SELECT
  evidence.id AS evidence_id,
  checkout.domain_type,
  checkout.domain_order_id,
  attempt.provider,
  evidence.status,
  evidence.submitted_at,
  evidence.reviewed_at,
  CASE
    WHEN evidence.status = 'awaiting_evidence'
      AND evidence.customer_reference IS NULL
      AND evidence.submitted_amount_minor IS NULL
      AND evidence.currency IS NULL
      AND evidence.submitted_by IS NULL
      THEN 'awaiting_customer_evidence'
    WHEN evidence.status IN ('submitted','under_review','approved','rejected')
      AND evidence.customer_reference IS NOT NULL
      AND evidence.submitted_amount_minor = checkout.total_minor
      AND evidence.currency = checkout.currency
      AND evidence.submitted_by IS NOT NULL
      THEN 'canonical'
    ELSE 'requires_reconciliation'
  END AS classification
FROM commerce_manual_payment_evidence evidence
JOIN commerce_checkout_session checkout ON checkout.id = evidence.checkout_id
JOIN commerce_payment_attempt attempt ON attempt.id = evidence.payment_attempt_id;

CREATE OR REPLACE FUNCTION commerce_validate_manual_payment_evidence()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  attempt commerce_payment_attempt%ROWTYPE;
  checkout commerce_checkout_session%ROWTYPE;
BEGIN
  SELECT * INTO attempt
  FROM commerce_payment_attempt
  WHERE id = NEW.payment_attempt_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Unknown manual payment attempt %', NEW.payment_attempt_id;
  END IF;

  SELECT * INTO checkout
  FROM commerce_checkout_session
  WHERE id = NEW.checkout_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Unknown manual payment checkout %', NEW.checkout_id;
  END IF;

  IF attempt.checkout_id <> NEW.checkout_id
     OR attempt.provider NOT IN ('bank_transfer','cash','pos')
     OR attempt.operation <> 'manual_verify'
     OR attempt.environment <> checkout.environment
     OR attempt.amount_minor <> checkout.total_minor
     OR attempt.currency <> checkout.currency THEN
    RAISE EXCEPTION 'Manual evidence does not match its immutable checkout attempt';
  END IF;

  IF NEW.customer_reference IS NOT NULL AND (
       char_length(btrim(NEW.customer_reference)) < 3
       OR char_length(btrim(NEW.customer_reference)) > 120
       OR NEW.customer_reference ~ '[[:cntrl:]]'
     ) THEN
    RAISE EXCEPTION 'Manual payment customer reference is invalid';
  END IF;

  IF NEW.evidence_object_key IS NOT NULL AND (
       char_length(NEW.evidence_object_key) > 512
       OR position('://' IN NEW.evidence_object_key) > 0
       OR position('..' IN NEW.evidence_object_key) > 0
     ) THEN
    RAISE EXCEPTION 'Manual payment evidence must use a private object key';
  END IF;

  IF TG_OP = 'INSERT' THEN
    IF NEW.status <> 'awaiting_evidence'
       OR NEW.evidence_object_key IS NOT NULL
       OR NEW.customer_reference IS NOT NULL
       OR NEW.submitted_amount_minor IS NOT NULL
       OR NEW.currency IS NOT NULL
       OR NEW.submitted_at IS NOT NULL
       OR NEW.submitted_by IS NOT NULL
       OR NEW.reviewed_by IS NOT NULL
       OR NEW.reviewed_at IS NOT NULL
       OR NEW.review_notes IS NOT NULL THEN
      RAISE EXCEPTION 'Manual evidence must start in awaiting_evidence without fabricated proof';
    END IF;
    RETURN NEW;
  END IF;

  IF NEW.checkout_id <> OLD.checkout_id
     OR NEW.payment_attempt_id <> OLD.payment_attempt_id THEN
    RAISE EXCEPTION 'Manual payment evidence binding is immutable';
  END IF;

  IF NEW.status = OLD.status THEN
    IF to_jsonb(NEW) <> to_jsonb(OLD) THEN
      RAISE EXCEPTION 'Manual evidence cannot mutate without a status transition';
    END IF;
    RETURN NEW;
  END IF;

  IF NOT (
    (OLD.status = 'awaiting_evidence' AND NEW.status = 'submitted')
    OR (OLD.status = 'rejected' AND NEW.status = 'submitted')
    OR (OLD.status = 'submitted' AND NEW.status = 'under_review')
    OR (OLD.status = 'under_review' AND NEW.status IN ('approved','rejected'))
  ) THEN
    RAISE EXCEPTION 'Invalid manual evidence transition: % -> %', OLD.status, NEW.status;
  END IF;

  IF NEW.status = 'submitted' THEN
    IF NEW.customer_reference IS NULL
       OR NEW.submitted_amount_minor <> checkout.total_minor
       OR NEW.currency <> checkout.currency
       OR NEW.submitted_at IS NULL
       OR NEW.submitted_by IS NULL
       OR NEW.reviewed_by IS NOT NULL
       OR NEW.reviewed_at IS NOT NULL
       OR NEW.review_notes IS NOT NULL
       OR NOT EXISTS (SELECT 1 FROM party WHERE id = NEW.submitted_by) THEN
      RAISE EXCEPTION 'Submitted manual evidence is incomplete or mismatched';
    END IF;
  ELSE
    IF NEW.customer_reference IS DISTINCT FROM OLD.customer_reference
       OR NEW.evidence_object_key IS DISTINCT FROM OLD.evidence_object_key
       OR NEW.submitted_amount_minor IS DISTINCT FROM OLD.submitted_amount_minor
       OR NEW.currency IS DISTINCT FROM OLD.currency
       OR NEW.submitted_at IS DISTINCT FROM OLD.submitted_at
       OR NEW.submitted_by IS DISTINCT FROM OLD.submitted_by THEN
      RAISE EXCEPTION 'Submitted manual evidence is immutable during review';
    END IF;

    IF NEW.reviewed_by IS NULL
       OR NEW.reviewed_by = NEW.submitted_by
       OR NOT EXISTS (SELECT 1 FROM party WHERE id = NEW.reviewed_by) THEN
      RAISE EXCEPTION 'Manual evidence requires an independent valid reviewer';
    END IF;

    IF NEW.status = 'under_review' AND NEW.reviewed_at IS NOT NULL THEN
      RAISE EXCEPTION 'Manual evidence cannot have a decision timestamp while under review';
    END IF;

    IF NEW.status IN ('approved','rejected') AND (
         NEW.reviewed_by IS DISTINCT FROM OLD.reviewed_by
         OR NEW.reviewed_at IS NULL
         OR NEW.review_notes IS NULL
         OR char_length(btrim(NEW.review_notes)) < 3
         OR char_length(btrim(NEW.review_notes)) > 2000
         OR NEW.review_notes ~ '[[:cntrl:]]'
       ) THEN
      RAISE EXCEPTION 'Manual evidence decision is incomplete or invalid';
    END IF;
  END IF;

  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_validate_manual_payment_evidence
  ON commerce_manual_payment_evidence;
CREATE TRIGGER trg_commerce_validate_manual_payment_evidence
  BEFORE INSERT OR UPDATE ON commerce_manual_payment_evidence
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_manual_payment_evidence();

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('checkout.bank_transfer', TRUE, 'production',
    'Manual bank transfer requires submitted evidence and independent staff verification'),
  ('checkout.cash', TRUE, 'production',
    'Cash settlement requires independent staff verification before payment confirmation'),
  ('checkout.pos', TRUE, 'production',
    'POS settlement requires independent staff verification before payment confirmation')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
