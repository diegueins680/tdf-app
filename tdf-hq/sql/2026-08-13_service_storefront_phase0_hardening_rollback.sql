-- Emergency rollback. This weakens public-order access controls, so disable the
-- public storefront before applying it. Order and provider data are retained.
BEGIN;

DROP INDEX IF EXISTS uq_service_storefront_order_paypal_capture;
DROP INDEX IF EXISTS uq_service_storefront_order_create_idempotency;
DROP INDEX IF EXISTS uq_service_storefront_order_paypal;
DROP INDEX IF EXISTS uq_service_storefront_order_datafast_payment;
DROP INDEX IF EXISTS uq_service_storefront_order_datafast_checkout;

ALTER TABLE service_storefront_order
  DROP CONSTRAINT IF EXISTS service_storefront_order_price_check,
  DROP CONSTRAINT IF EXISTS service_storefront_order_song_count_check,
  DROP COLUMN IF EXISTS paypal_capture_id,
  DROP COLUMN IF EXISTS create_request_sha256,
  DROP COLUMN IF EXISTS create_idempotency_key,
  DROP COLUMN IF EXISTS lookup_token_hash;

ALTER TABLE service_storefront_package
  DROP CONSTRAINT IF EXISTS service_storefront_package_price_check,
  DROP CONSTRAINT IF EXISTS service_storefront_package_song_count_check,
  DROP COLUMN IF EXISTS max_song_count,
  DROP COLUMN IF EXISTS min_song_count;

COMMIT;
