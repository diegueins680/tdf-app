-- Link public mixing/mastering orders to the provider-neutral checkout runtime.
-- Historical rows are deliberately classified for review instead of inferred.
BEGIN;

ALTER TABLE service_storefront_order
  ADD COLUMN IF NOT EXISTS checkout_id UUID;

CREATE UNIQUE INDEX IF NOT EXISTS uq_service_storefront_order_checkout
  ON service_storefront_order(checkout_id)
  WHERE checkout_id IS NOT NULL;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_constraint
    WHERE conname = 'fk_service_storefront_order_checkout'
      AND conrelid = 'service_storefront_order'::regclass
  ) THEN
    ALTER TABLE service_storefront_order
      ADD CONSTRAINT fk_service_storefront_order_checkout
      FOREIGN KEY (checkout_id)
      REFERENCES commerce_checkout_session(id)
      ON DELETE RESTRICT;
  END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_commerce_manual_evidence_attempt
  ON commerce_manual_payment_evidence(payment_attempt_id);

CREATE UNIQUE INDEX IF NOT EXISTS uq_commerce_succeeded_attempt_checkout
  ON commerce_payment_attempt(checkout_id)
  WHERE status = 'succeeded';

CREATE UNIQUE INDEX IF NOT EXISTS uq_commerce_active_payment_receipt_checkout
  ON commerce_receipt(checkout_id)
  WHERE kind = 'payment_receipt' AND voided_at IS NULL;

CREATE UNIQUE INDEX IF NOT EXISTS uq_commerce_open_reconciliation_fingerprint
  ON commerce_reconciliation_exception(
    provider,
    environment,
    merchant_account_ref,
    exception_type,
    COALESCE(internal_reference, ''),
    COALESCE(provider_reference, '')
  )
  WHERE status IN ('open', 'assigned');

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('commerce.mixing_mastering', FALSE, 'production',
    'Requires provider sandbox evidence, reconciliation ownership, signed callbacks and production authorization')
ON CONFLICT (flag_key, environment) DO NOTHING;

CREATE OR REPLACE VIEW service_storefront_checkout_backfill_report AS
SELECT
  service_order.id AS service_order_id,
  service_order.order_number,
  service_order.checkout_id,
  service_order.status AS service_order_status,
  service_order.payment_provider,
  service_order.created_at,
  CASE
    WHEN service_order.checkout_id IS NOT NULL THEN 'linked'
    WHEN service_order.lookup_token_hash IS NOT NULL
      AND service_order.paid_at IS NULL
      AND service_order.datafast_checkout_id IS NULL
      AND service_order.datafast_payment_id IS NULL
      AND service_order.paypal_order_id IS NULL
      AND service_order.paypal_capture_id IS NULL
      AND service_order.stripe_payment_intent_id IS NULL
      AND service_order.status IN (
        'awaiting_payment', 'pending_payment', 'payment_failed',
        'awaiting_manual_confirmation'
      ) THEN 'safe_unpaid_candidate'
    ELSE 'requires_reconciliation'
  END AS classification,
  CASE
    WHEN service_order.checkout_id IS NOT NULL THEN 'Canonical checkout already linked.'
    WHEN service_order.lookup_token_hash IS NULL THEN
      'Historical order has no guest capability; staff identity verification is required.'
    WHEN service_order.paid_at IS NOT NULL OR service_order.status = 'paid' THEN
      'Payment-like state requires provider and ledger reconciliation before linking.'
    WHEN service_order.datafast_checkout_id IS NOT NULL
      OR service_order.datafast_payment_id IS NOT NULL
      OR service_order.paypal_order_id IS NOT NULL
      OR service_order.paypal_capture_id IS NOT NULL
      OR service_order.stripe_payment_intent_id IS NOT NULL THEN
      'Existing provider references require evidence-preserving reconciliation.'
    ELSE 'Eligible for an operator-reviewed unpaid checkout backfill.'
  END AS reason
FROM service_storefront_order service_order;

COMMENT ON COLUMN service_storefront_order.checkout_id IS
  'Canonical provider-neutral checkout. NULL means legacy/unlinked and must not be inferred as paid.';
COMMENT ON VIEW service_storefront_checkout_backfill_report IS
  'Read-only classification for dry-run/backfill planning; never evidence of payment.';

COMMIT;
