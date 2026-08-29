-- Phase 0 hardening for public mixing/mastering commerce.
-- Existing orders deliberately receive no lookup token: staff must reconcile and
-- reissue access instead of making historical PII enumerable.
BEGIN;

ALTER TABLE service_storefront_package
  ADD COLUMN IF NOT EXISTS min_song_count INT NOT NULL DEFAULT 1,
  ADD COLUMN IF NOT EXISTS max_song_count INT NOT NULL DEFAULT 1;

UPDATE service_storefront_package
SET max_song_count = CASE
      WHEN service_kind = 'Mastering' AND tier = 'Pro' THEN 3
      WHEN service_kind = 'Mastering' AND tier = 'Premium' THEN 5
      WHEN service_kind = 'Bundle' AND tier = 'Pro' THEN 3
      WHEN service_kind = 'Bundle' AND tier = 'Premium' THEN 5
    END
WHERE (service_kind, tier) IN (
  ('Mastering', 'Pro'),
  ('Mastering', 'Premium'),
  ('Bundle', 'Pro'),
  ('Bundle', 'Premium')
);

ALTER TABLE service_storefront_package
  DROP CONSTRAINT IF EXISTS service_storefront_package_song_count_check,
  DROP CONSTRAINT IF EXISTS service_storefront_package_price_check,
  ADD CONSTRAINT service_storefront_package_song_count_check
    CHECK (min_song_count >= 1 AND max_song_count >= min_song_count),
  ADD CONSTRAINT service_storefront_package_price_check
    CHECK (price_usd_cents > 0);

ALTER TABLE service_storefront_order
  ADD COLUMN IF NOT EXISTS lookup_token_hash TEXT,
  ADD COLUMN IF NOT EXISTS paypal_capture_id TEXT,
  ADD COLUMN IF NOT EXISTS create_idempotency_key TEXT,
  ADD COLUMN IF NOT EXISTS create_request_sha256 TEXT;

ALTER TABLE service_storefront_order
  DROP CONSTRAINT IF EXISTS service_storefront_order_song_count_check,
  DROP CONSTRAINT IF EXISTS service_storefront_order_price_check,
  ADD CONSTRAINT service_storefront_order_song_count_check CHECK (song_count >= 1),
  ADD CONSTRAINT service_storefront_order_price_check CHECK (price_usd_cents > 0);

CREATE UNIQUE INDEX IF NOT EXISTS uq_service_storefront_order_datafast_checkout
  ON service_storefront_order(datafast_checkout_id)
  WHERE datafast_checkout_id IS NOT NULL;
CREATE UNIQUE INDEX IF NOT EXISTS uq_service_storefront_order_datafast_payment
  ON service_storefront_order(datafast_payment_id)
  WHERE datafast_payment_id IS NOT NULL;
CREATE UNIQUE INDEX IF NOT EXISTS uq_service_storefront_order_paypal
  ON service_storefront_order(paypal_order_id)
  WHERE paypal_order_id IS NOT NULL;
CREATE UNIQUE INDEX IF NOT EXISTS uq_service_storefront_order_paypal_capture
  ON service_storefront_order(paypal_capture_id)
  WHERE paypal_capture_id IS NOT NULL;
CREATE UNIQUE INDEX IF NOT EXISTS uq_service_storefront_order_create_idempotency
  ON service_storefront_order(create_idempotency_key)
  WHERE create_idempotency_key IS NOT NULL;

COMMENT ON COLUMN service_storefront_order.lookup_token_hash IS
  'SHA-256 of the one-time guest lookup capability; the raw token is never persisted.';
COMMENT ON COLUMN service_storefront_order.create_idempotency_key IS
  'Opaque client retry key. Reuse with a different request hash is rejected.';

COMMIT;
