BEGIN;

DO $$
BEGIN
  IF to_regclass('public.marketplace_sale_order_runtime') IS NOT NULL
     AND EXISTS (SELECT 1 FROM marketplace_sale_order_runtime) THEN
    RAISE EXCEPTION 'Marketplace sale runtime contains live links; reconcile and archive them before rollback';
  END IF;
END $$;

DROP VIEW IF EXISTS marketplace_sale_checkout_backfill_report;
DROP FUNCTION IF EXISTS marketplace_expire_sale_holds(TIMESTAMPTZ);
DROP TRIGGER IF EXISTS trg_marketplace_protect_canonical_payment_state ON marketplace_order;
DROP FUNCTION IF EXISTS marketplace_protect_canonical_payment_state();
DROP TRIGGER IF EXISTS trg_marketplace_sync_verified_checkout ON commerce_checkout_session;
DROP FUNCTION IF EXISTS marketplace_sync_verified_checkout();
DROP TRIGGER IF EXISTS trg_marketplace_record_fulfillment_transition ON marketplace_sale_order_runtime;
DROP FUNCTION IF EXISTS marketplace_record_fulfillment_transition();
DROP TRIGGER IF EXISTS trg_marketplace_validate_fulfillment_transition ON marketplace_sale_order_runtime;
DROP FUNCTION IF EXISTS marketplace_validate_fulfillment_transition();
DROP FUNCTION IF EXISTS marketplace_fulfillment_transition_allowed(TEXT, TEXT, TEXT);
DROP TRIGGER IF EXISTS trg_marketplace_validate_sale_runtime ON marketplace_sale_order_runtime;
DROP FUNCTION IF EXISTS marketplace_validate_sale_runtime();
DROP TABLE IF EXISTS marketplace_sale_fulfillment_event;
DROP TABLE IF EXISTS marketplace_sale_order_runtime;

DELETE FROM revenue_feature_flag
WHERE flag_key IN ('commerce.marketplace_sales','commerce.marketplace_rentals')
  AND environment = 'production' AND enabled = FALSE;

COMMIT;
