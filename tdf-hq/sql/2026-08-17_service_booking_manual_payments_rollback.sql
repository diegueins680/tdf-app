BEGIN;

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM commerce_manual_payment_evidence
    WHERE submitted_by IS NOT NULL
       OR status <> 'awaiting_evidence'
  ) THEN
    RAISE EXCEPTION 'Refusing to roll back manual-payment controls after evidence submission or review';
  END IF;
  IF EXISTS (
    SELECT 1 FROM revenue_feature_flag
    WHERE flag_key IN ('checkout.bank_transfer','checkout.cash','checkout.pos')
      AND environment = 'production'
      AND (NOT enabled OR updated_by IS NOT NULL)
  ) THEN
    RAISE EXCEPTION 'Refusing to roll back modified manual-payment capability flags';
  END IF;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_validate_manual_payment_evidence
  ON commerce_manual_payment_evidence;
DROP FUNCTION IF EXISTS commerce_validate_manual_payment_evidence();
DROP VIEW IF EXISTS commerce_manual_payment_evidence_review_report;

ALTER TABLE commerce_manual_payment_evidence
  DROP CONSTRAINT IF EXISTS fk_commerce_manual_evidence_submitted_by;
ALTER TABLE commerce_manual_payment_evidence
  DROP COLUMN IF EXISTS submitted_by;

DELETE FROM revenue_feature_flag
WHERE flag_key IN ('checkout.bank_transfer','checkout.cash','checkout.pos')
  AND environment = 'production'
  AND enabled
  AND updated_by IS NULL;

COMMIT;
