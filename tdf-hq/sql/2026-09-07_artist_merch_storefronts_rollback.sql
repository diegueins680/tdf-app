BEGIN;

DO $$
BEGIN
  IF to_regclass('public.merch_order') IS NOT NULL AND EXISTS (SELECT 1 FROM merch_order) THEN
    RAISE EXCEPTION 'Refusing destructive merch rollback: commercial orders or evidence exist';
  END IF;
  IF to_regclass('public.merch_settlement') IS NOT NULL AND EXISTS (SELECT 1 FROM merch_settlement) THEN
    RAISE EXCEPTION 'Refusing destructive merch rollback: settlement evidence exists';
  END IF;
END $$;

DROP VIEW IF EXISTS merch_public_product;
DROP VIEW IF EXISTS merch_public_storefront;
DROP TRIGGER IF EXISTS merch_review_verified_purchase_trigger ON merch_review;
DROP TRIGGER IF EXISTS merch_order_snapshot_immutable_trigger ON merch_order;
DROP TRIGGER IF EXISTS merch_apply_dispute_state_trigger ON commerce_dispute;
DROP TRIGGER IF EXISTS merch_apply_refund_state_trigger ON commerce_refund;
DROP TRIGGER IF EXISTS merch_refund_case_immutable_trigger ON merch_refund_case;
DROP TRIGGER IF EXISTS merch_refund_case_validate_trigger ON merch_refund_case;
DROP TRIGGER IF EXISTS merch_apply_checkout_status_trigger ON commerce_checkout_session;
DROP TRIGGER IF EXISTS merch_guard_paid_checkout_trigger ON commerce_checkout_session;
DROP TRIGGER IF EXISTS merch_cart_item_store_trigger ON merch_cart_item;
DROP TRIGGER IF EXISTS merch_variant_store_sku_trigger ON merch_product_variant;
DROP TRIGGER IF EXISTS merch_product_policy_store_trigger ON merch_product;
DROP TRIGGER IF EXISTS merch_store_owner_trigger ON merch_store;
DROP TRIGGER IF EXISTS merch_store_eligibility_trigger ON merch_store;
DROP TRIGGER IF EXISTS merch_fulfillment_event_immutable_trigger ON merch_fulfillment_event;
DROP TRIGGER IF EXISTS merch_settlement_payment_evidence_apply_trigger ON merch_settlement_payment_evidence;
DROP TRIGGER IF EXISTS merch_settlement_payment_evidence_immutable_trigger ON merch_settlement_payment_evidence;
DROP TRIGGER IF EXISTS merch_audit_event_immutable_trigger ON merch_audit_event;
DROP TRIGGER IF EXISTS merch_order_line_immutable_trigger ON merch_order_line;

DROP FUNCTION IF EXISTS merch_validate_review_purchase();
DROP FUNCTION IF EXISTS merch_apply_dispute_state();
DROP FUNCTION IF EXISTS merch_apply_refund_state();
DROP FUNCTION IF EXISTS merch_validate_refund_case();
DROP FUNCTION IF EXISTS merch_apply_settlement_payment_evidence();
DROP FUNCTION IF EXISTS merch_protect_order_snapshots();
DROP FUNCTION IF EXISTS merch_apply_checkout_status();
DROP FUNCTION IF EXISTS merch_guard_paid_checkout();
DROP FUNCTION IF EXISTS merch_release_expired_reservations(TIMESTAMPTZ);
DROP FUNCTION IF EXISTS merch_reserve_stock(UUID,UUID,JSONB,TIMESTAMPTZ);
DROP FUNCTION IF EXISTS merch_calculate_commission_bps(UUID,TIMESTAMPTZ);
DROP FUNCTION IF EXISTS merch_validate_cart_item_store();
DROP FUNCTION IF EXISTS merch_validate_variant_store_sku();
DROP FUNCTION IF EXISTS merch_validate_product_policy_store();
DROP FUNCTION IF EXISTS merch_ensure_owner_membership();
DROP FUNCTION IF EXISTS merch_validate_store_eligibility();
DROP FUNCTION IF EXISTS merch_reject_immutable_mutation();

DROP TABLE IF EXISTS merch_audit_event;
DROP TABLE IF EXISTS merch_notification_outbox;
DROP TABLE IF EXISTS merch_analytics_event;
DROP TABLE IF EXISTS merch_review;
DROP TABLE IF EXISTS merch_favorite;
DROP TABLE IF EXISTS merch_settlement_order;
DROP TABLE IF EXISTS merch_settlement_payment_evidence;
DROP TABLE IF EXISTS merch_settlement;
DROP TABLE IF EXISTS merch_refund_case;
DROP TABLE IF EXISTS merch_order_issue;
DROP TABLE IF EXISTS merch_shipment;
DROP TABLE IF EXISTS merch_fulfillment_event;
DROP TABLE IF EXISTS merch_inventory_reservation;
DROP TABLE IF EXISTS merch_order_line;
DROP TABLE IF EXISTS merch_order;
DROP TABLE IF EXISTS merch_cart_item;
DROP TABLE IF EXISTS merch_cart;
DROP TABLE IF EXISTS merch_product_image;
DROP TABLE IF EXISTS merch_product_variant;
DROP TABLE IF EXISTS merch_product;
DROP TABLE IF EXISTS merch_shipping_zone;
DROP TABLE IF EXISTS merch_store_policy;
DROP TABLE IF EXISTS merch_commission_policy;
DROP TABLE IF EXISTS merch_store_member;
DROP TABLE IF EXISTS merch_store;

DELETE FROM revenue_feature_flag WHERE flag_key LIKE 'merch.%' AND enabled = FALSE;

COMMIT;
