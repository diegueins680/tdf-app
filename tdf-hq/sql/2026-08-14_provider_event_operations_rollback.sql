-- Revert provider-event operations only before any operator replay evidence exists.
BEGIN;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM commerce_provider_event_action) THEN
    RAISE EXCEPTION 'Refusing rollback: provider event replay audit evidence exists';
  END IF;
END $$;

DELETE FROM revenue_feature_flag
WHERE flag_key = 'checkout.provider_event_worker';

DROP FUNCTION IF EXISTS commerce_requeue_provider_event(UUID, BIGINT, TEXT, TIMESTAMPTZ);
DROP TRIGGER IF EXISTS trg_commerce_provider_event_transition
  ON commerce_provider_event_inbox;
DROP FUNCTION IF EXISTS commerce_validate_provider_event_transition();
DROP TRIGGER IF EXISTS trg_commerce_provider_event_action_immutable
  ON commerce_provider_event_action;
DROP INDEX IF EXISTS idx_commerce_provider_event_action_event;
DROP TABLE IF EXISTS commerce_provider_event_action;

COMMIT;
