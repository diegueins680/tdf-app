BEGIN;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM domo_event_quote_runtime) THEN
    RAISE EXCEPTION 'Refusing Domo quote runtime rollback: customer quote or checkout records exist';
  END IF;
END $$;

DELETE FROM revenue_feature_flag
WHERE flag_key = 'commerce.domo_quotes' AND environment = 'production';

DROP FUNCTION domo_quote_expire_holds(TIMESTAMPTZ, UUID);
DROP TRIGGER trg_domo_quote_sync_verified_payment ON commerce_checkout_session;
DROP FUNCTION domo_quote_sync_verified_payment();
DROP TRIGGER trg_domo_quote_require_verified_payment ON commerce_checkout_session;
DROP FUNCTION domo_quote_require_verified_payment();
DROP TRIGGER trg_domo_lock_commerce_quote_line ON commerce_quote_line;
DROP FUNCTION domo_lock_commerce_quote_line();
DROP TRIGGER trg_domo_lock_commerce_quote_snapshot ON commerce_quote;
DROP FUNCTION domo_lock_commerce_quote_snapshot();
DROP TRIGGER trg_domo_validate_quote_runtime_update ON domo_event_quote_runtime;
DROP FUNCTION domo_validate_quote_runtime_update();
DROP TRIGGER trg_domo_validate_quote_runtime_insert ON domo_event_quote_runtime;
DROP FUNCTION domo_validate_quote_runtime_insert();
DROP TABLE domo_quote_state_event;
DROP TABLE domo_quote_rate_limit;
DROP TABLE domo_event_quote_runtime;

UPDATE commerce_product_version
SET pricing_rules = pricing_rules
      - 'max_guests' - 'max_duration_hours' - 'max_setup_hours',
    policy_snapshot = policy_snapshot
      - 'terms_version' - 'quote_expiry_minutes' - 'timezone'
      - 'minimum_lead_hours' - 'maximum_advance_days'
WHERE domain_type = 'domo'
  AND source = 'client_legacy_snapshot'
  AND status = 'pending_approval';

COMMIT;
