-- Emergency rollback for service-storefront runtime linking.
-- Refuse to destroy a live linkage; disable the storefront and reconcile first.
BEGIN;

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM service_storefront_order WHERE checkout_id IS NOT NULL LIMIT 1
  ) THEN
    RAISE EXCEPTION
      'Refusing service-storefront checkout rollback: canonical checkout links exist';
  END IF;
END $$;

DROP VIEW IF EXISTS service_storefront_checkout_backfill_report;
DROP INDEX IF EXISTS uq_commerce_open_reconciliation_fingerprint;
DROP INDEX IF EXISTS uq_commerce_active_payment_receipt_checkout;
DROP INDEX IF EXISTS uq_commerce_succeeded_attempt_checkout;
DROP INDEX IF EXISTS uq_commerce_manual_evidence_attempt;

DELETE FROM revenue_feature_flag
WHERE flag_key = 'commerce.mixing_mastering'
  AND environment = 'production'
  AND updated_by IS NULL
  AND reason = 'Requires provider sandbox evidence, reconciliation ownership, signed callbacks and production authorization';

ALTER TABLE service_storefront_order
  DROP CONSTRAINT IF EXISTS fk_service_storefront_order_checkout;
DROP INDEX IF EXISTS uq_service_storefront_order_checkout;
ALTER TABLE service_storefront_order
  DROP COLUMN IF EXISTS checkout_id;

COMMIT;
