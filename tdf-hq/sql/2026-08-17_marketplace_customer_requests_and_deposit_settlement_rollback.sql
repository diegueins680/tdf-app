BEGIN;

DO $$
BEGIN
  IF to_regclass('public.marketplace_customer_request') IS NOT NULL
     AND EXISTS (SELECT 1 FROM marketplace_customer_request) THEN
    RAISE EXCEPTION 'Refusing rollback: marketplace customer request evidence exists';
  END IF;
  IF to_regclass('public.marketplace_rental_deposit_settlement') IS NOT NULL
     AND EXISTS (SELECT 1 FROM marketplace_rental_deposit_settlement) THEN
    RAISE EXCEPTION 'Refusing rollback: rental deposit settlement evidence exists';
  END IF;
END $$;

DROP VIEW IF EXISTS marketplace_rental_deposit_ledger_backfill_report;
DROP TRIGGER IF EXISTS trg_marketplace_apply_verified_rental_deposit_settlement
  ON marketplace_rental_deposit_settlement;
DROP FUNCTION IF EXISTS marketplace_apply_verified_rental_deposit_settlement();
DROP TRIGGER IF EXISTS trg_marketplace_record_rental_deposit_settlement_event
  ON marketplace_rental_deposit_settlement;
DROP FUNCTION IF EXISTS marketplace_record_rental_deposit_settlement_event();
DROP TRIGGER IF EXISTS trg_marketplace_protect_rental_deposit_settlement
  ON marketplace_rental_deposit_settlement;
DROP FUNCTION IF EXISTS marketplace_protect_rental_deposit_settlement();
DROP TRIGGER IF EXISTS trg_marketplace_validate_rental_deposit_settlement
  ON marketplace_rental_deposit_settlement;
DROP FUNCTION IF EXISTS marketplace_validate_rental_deposit_settlement();
DROP TABLE IF EXISTS marketplace_rental_deposit_settlement_event;
DROP TABLE IF EXISTS marketplace_rental_deposit_settlement;

DROP TRIGGER IF EXISTS trg_marketplace_guard_rental_customer_request
  ON marketplace_rental_order_runtime;
DROP FUNCTION IF EXISTS marketplace_guard_rental_customer_request();
DROP TRIGGER IF EXISTS trg_marketplace_guard_sale_customer_request
  ON marketplace_sale_order_runtime;
DROP FUNCTION IF EXISTS marketplace_guard_sale_customer_request();
DROP TRIGGER IF EXISTS trg_marketplace_apply_approved_customer_request
  ON marketplace_customer_request;
DROP FUNCTION IF EXISTS marketplace_apply_approved_customer_request();
DROP TRIGGER IF EXISTS trg_marketplace_record_customer_request_event
  ON marketplace_customer_request;
DROP FUNCTION IF EXISTS marketplace_record_customer_request_event();
DROP TRIGGER IF EXISTS trg_marketplace_protect_customer_request
  ON marketplace_customer_request;
DROP FUNCTION IF EXISTS marketplace_protect_customer_request();
DROP TRIGGER IF EXISTS trg_marketplace_validate_customer_request
  ON marketplace_customer_request;
DROP FUNCTION IF EXISTS marketplace_validate_customer_request();
DROP TABLE IF EXISTS marketplace_customer_request_event;
DROP TABLE IF EXISTS marketplace_customer_request;

DELETE FROM revenue_feature_flag
WHERE flag_key = 'commerce.marketplace_manual_deposit_settlement'
  AND environment = 'production';

CREATE OR REPLACE FUNCTION marketplace_rental_transition_allowed(from_status TEXT, to_status TEXT)
RETURNS BOOLEAN LANGUAGE sql IMMUTABLE AS $$
  SELECT from_status = to_status OR (from_status, to_status) IN (
    ('on_hold','confirmed'), ('on_hold','cancelled'), ('on_hold','expired'),
    ('confirmed','ready_for_handoff'), ('confirmed','cancellation_requested'), ('confirmed','no_show'),
    ('ready_for_handoff','checked_out'), ('ready_for_handoff','cancellation_requested'),
    ('ready_for_handoff','no_show'), ('checked_out','return_due'),
    ('checked_out','returned_pending_inspection'), ('checked_out','lost'), ('checked_out','disputed'),
    ('return_due','returned_pending_inspection'), ('return_due','lost'), ('return_due','disputed'),
    ('returned_pending_inspection','deposit_refund_due'),
    ('returned_pending_inspection','damage_review'), ('damage_review','deposit_refund_due'),
    ('damage_review','disputed'), ('deposit_refund_due','closed'),
    ('cancellation_requested','cancelled'), ('no_show','cancelled'), ('lost','disputed'),
    ('disputed','damage_review'), ('disputed','deposit_refund_due'), ('disputed','closed')
  );
$$;

COMMIT;
