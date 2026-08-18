BEGIN;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM service_booking_checkout_runtime) THEN
    RAISE EXCEPTION 'Refusing to roll back service booking checkout runtime while canonical booking records exist';
  END IF;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_sync_checkout ON commerce_checkout_session;
DROP TRIGGER IF EXISTS trg_service_booking_require_verified_payment ON commerce_checkout_session;
DROP FUNCTION IF EXISTS service_booking_sync_verified_checkout();
DROP FUNCTION IF EXISTS service_booking_require_verified_payment();
DROP FUNCTION IF EXISTS service_booking_expire_holds(TIMESTAMPTZ);

DROP TRIGGER IF EXISTS trg_service_booking_record_transition ON service_booking_checkout_runtime;
DROP TRIGGER IF EXISTS trg_service_booking_validate_transition ON service_booking_checkout_runtime;
DROP TRIGGER IF EXISTS trg_service_booking_validate_runtime ON service_booking_checkout_runtime;
DROP FUNCTION IF EXISTS service_booking_record_transition();
DROP FUNCTION IF EXISTS service_booking_validate_transition();
DROP FUNCTION IF EXISTS service_booking_transition_allowed(TEXT, TEXT);
DROP FUNCTION IF EXISTS service_booking_validate_runtime();

DROP TRIGGER IF EXISTS trg_service_booking_sync_legacy_allocation ON booking;
DROP TRIGGER IF EXISTS trg_service_booking_allocate_resource ON booking_resource;
DROP FUNCTION IF EXISTS service_booking_sync_legacy_booking_allocation();
DROP FUNCTION IF EXISTS service_booking_allocate_resource();

DROP TABLE IF EXISTS service_booking_event;
DROP TABLE IF EXISTS service_booking_resource_allocation;
DROP TABLE IF EXISTS service_booking_checkout_runtime;

DROP TRIGGER IF EXISTS trg_service_booking_policy_history ON service_booking_commerce_policy;
DROP TRIGGER IF EXISTS trg_service_booking_validate_policy ON service_booking_commerce_policy;
DROP FUNCTION IF EXISTS service_booking_record_policy_history();
DROP FUNCTION IF EXISTS service_booking_validate_policy();
DROP TABLE IF EXISTS service_booking_commerce_policy_history;
DROP TABLE IF EXISTS service_booking_commerce_policy;

DELETE FROM revenue_feature_flag
WHERE flag_key = 'commerce.service_bookings'
  AND environment = 'production'
  AND enabled = FALSE;

COMMIT;
