BEGIN;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM event_ticket_checkout_runtime)
     OR EXISTS (SELECT 1 FROM event_ticket_fulfillment_event) THEN
    RAISE EXCEPTION 'Event ticket checkout rollback blocked: runtime or fulfillment evidence exists';
  END IF;
  IF EXISTS (
    SELECT 1 FROM event_ticket_checkout_policy
    WHERE approval_status = 'approved' OR active
  ) THEN
    RAISE EXCEPTION 'Event ticket checkout rollback blocked: approved or active policies exist';
  END IF;
END $$;

DELETE FROM revenue_feature_flag
WHERE environment = 'production'
  AND flag_key IN ('commerce.event_tickets','commerce.event_ticket_settlements');

DROP TRIGGER IF EXISTS trg_event_ticket_checkout_sync_payment_and_hold
  ON commerce_checkout_session;
DROP TRIGGER IF EXISTS trg_event_ticket_checkout_require_verified_payment
  ON commerce_checkout_session;
DROP TRIGGER IF EXISTS trg_event_ticket_order_require_canonical_payment
  ON event_ticket_order;
DROP FUNCTION IF EXISTS event_ticket_order_require_canonical_payment();
DROP FUNCTION IF EXISTS event_ticket_checkout_sync_payment_and_hold();
DROP FUNCTION IF EXISTS event_ticket_checkout_require_verified_payment();
DROP FUNCTION IF EXISTS event_ticket_checkout_expire_holds(TIMESTAMPTZ, BIGINT, BIGINT);
DROP FUNCTION IF EXISTS event_ticket_checkout_expire_holds(TIMESTAMPTZ, BIGINT);

DROP TRIGGER IF EXISTS trg_event_ticket_checkout_validate_runtime
  ON event_ticket_checkout_runtime;
DROP FUNCTION IF EXISTS event_ticket_checkout_validate_runtime();
DROP TRIGGER IF EXISTS trg_event_ticket_checkout_policy_history
  ON event_ticket_checkout_policy;
DROP FUNCTION IF EXISTS event_ticket_checkout_policy_history_capture();
DROP TRIGGER IF EXISTS trg_event_ticket_checkout_validate_policy
  ON event_ticket_checkout_policy;
DROP FUNCTION IF EXISTS event_ticket_checkout_validate_policy();

DROP TABLE IF EXISTS event_ticket_fulfillment_event;
DROP TABLE IF EXISTS event_ticket_checkout_rate_limit;
DROP TABLE IF EXISTS event_ticket_checkout_runtime;
DROP TABLE IF EXISTS event_ticket_checkout_policy_history;
DROP TABLE IF EXISTS event_ticket_checkout_policy;

COMMIT;
