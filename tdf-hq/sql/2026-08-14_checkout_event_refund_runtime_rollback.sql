-- Revert provider-event/refund runtime metadata only before it contains evidence.
BEGIN;

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM commerce_provider_event_inbox
    WHERE checkout_id IS NOT NULL OR payment_attempt_id IS NOT NULL
       OR refund_id IS NOT NULL OR provider_resource_id IS NOT NULL
  ) OR EXISTS (
    SELECT 1 FROM commerce_refund
    WHERE provider IS NOT NULL OR environment IS NOT NULL OR merchant_account_ref IS NOT NULL
  ) OR EXISTS (
    SELECT 1 FROM commerce_receipt WHERE refund_id IS NOT NULL
  ) OR (
    SELECT COUNT(*) <> 6 FROM commerce_refund_reason_code
  ) OR EXISTS (
    SELECT 1
    FROM commerce_refund_reason_code AS actual
    LEFT JOIN (VALUES
      ('customer_request', 'Solicitud del cliente', 'Customer request', TRUE, 10, FALSE),
      ('duplicate', 'Pago duplicado', 'Duplicate payment', TRUE, 20, FALSE),
      ('fraud', 'Fraude confirmado', 'Confirmed fraud', TRUE, 30, TRUE),
      ('quality_issue', 'Problema de calidad', 'Quality issue', TRUE, 40, TRUE),
      ('service_cancelled', 'Servicio cancelado', 'Service cancelled', TRUE, 50, FALSE),
      ('other', 'Otro motivo documentado', 'Other documented reason', TRUE, 60, TRUE)
    ) AS seeded(reason_code, name_es, name_en, active, sort_order, requires_note)
      ON seeded.reason_code = actual.reason_code
    WHERE seeded.reason_code IS NULL
       OR (actual.name_es, actual.name_en, actual.active, actual.sort_order,
           actual.requires_note) IS DISTINCT FROM
          (seeded.name_es, seeded.name_en, seeded.active, seeded.sort_order,
           seeded.requires_note)
  ) THEN
    RAISE EXCEPTION 'Refusing rollback: provider-event, refund, or configured reason evidence exists';
  END IF;
END $$;

DELETE FROM revenue_feature_flag
WHERE flag_key IN (
  'checkout.paypal.webhooks', 'checkout.paypal.refunds',
  'checkout.datafast.webhooks', 'checkout.datafast.refunds'
);

DROP TRIGGER IF EXISTS trg_commerce_validate_credit_note ON commerce_receipt;
DROP FUNCTION IF EXISTS commerce_validate_credit_note();
DROP TRIGGER IF EXISTS trg_commerce_refund_allocation_immutable ON commerce_refund_allocation;
DROP TRIGGER IF EXISTS trg_commerce_validate_refund_write ON commerce_refund;
DROP FUNCTION IF EXISTS commerce_validate_refund_write();
DROP TABLE IF EXISTS commerce_refund_reason_code;

DROP INDEX IF EXISTS uq_commerce_credit_note_refund;
ALTER TABLE commerce_receipt DROP CONSTRAINT IF EXISTS fk_commerce_receipt_refund;
ALTER TABLE commerce_receipt DROP COLUMN IF EXISTS refund_id;

DROP INDEX IF EXISTS idx_commerce_refund_checkout_status;
ALTER TABLE commerce_refund DROP CONSTRAINT IF EXISTS ck_commerce_refund_environment;
ALTER TABLE commerce_refund DROP CONSTRAINT IF EXISTS ck_commerce_refund_provider;
ALTER TABLE commerce_refund
  DROP COLUMN IF EXISTS updated_at,
  DROP COLUMN IF EXISTS failure_summary,
  DROP COLUMN IF EXISTS failure_code,
  DROP COLUMN IF EXISTS merchant_account_ref,
  DROP COLUMN IF EXISTS environment,
  DROP COLUMN IF EXISTS provider;

DROP INDEX IF EXISTS idx_commerce_provider_event_resource;
DROP INDEX IF EXISTS idx_commerce_provider_event_work;
ALTER TABLE commerce_provider_event_inbox
  DROP CONSTRAINT IF EXISTS fk_commerce_provider_event_refund,
  DROP CONSTRAINT IF EXISTS fk_commerce_provider_event_attempt,
  DROP CONSTRAINT IF EXISTS fk_commerce_provider_event_checkout,
  DROP COLUMN IF EXISTS last_attempt_at,
  DROP COLUMN IF EXISTS processing_started_at,
  DROP COLUMN IF EXISTS provider_resource_id,
  DROP COLUMN IF EXISTS refund_id,
  DROP COLUMN IF EXISTS payment_attempt_id,
  DROP COLUMN IF EXISTS checkout_id;

COMMIT;
