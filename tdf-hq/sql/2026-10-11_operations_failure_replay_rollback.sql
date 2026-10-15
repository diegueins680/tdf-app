-- Removes the triggers that make a failure retry re-queue its projection and
-- follow it. Refuse rollback while an internal projection retry is in flight;
-- let those projections settle before retrying rollback. Roll the server back
-- first: a server built with this migration refuses retries without the triggers.
-- Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';
-- Keep the check and trigger removal atomic with respect to retries and workers.
LOCK TABLE operations_integration_failure, operations_outbox IN SHARE ROW EXCLUSIVE MODE;
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM operations_integration_failure
             WHERE provider = 'internal_outbox' AND status = 'retrying') THEN
    RAISE EXCEPTION 'Cannot roll back failure replay while internal retries are active';
  END IF;
END;
$$;
DROP INDEX IF EXISTS operations_integration_failure_open_uidx;
DROP TRIGGER IF EXISTS operations_integration_failure_resolved ON operations_integration_failure;
DROP TRIGGER IF EXISTS operations_integration_failure_merge ON operations_integration_failure;
DROP TRIGGER IF EXISTS operations_integration_failure_retry ON operations_integration_failure;
DROP TRIGGER IF EXISTS operations_outbox_settle_failure ON operations_outbox;
DROP FUNCTION IF EXISTS operations_integration_failure_resolved_event();
DROP FUNCTION IF EXISTS operations_integration_failure_merge();
DROP FUNCTION IF EXISTS operations_integration_failure_retry();
DROP FUNCTION IF EXISTS operations_outbox_settle_failure();
DROP FUNCTION IF EXISTS operations_integration_failure_announce(operations_integration_failure, BOOLEAN);
-- The capture trigger announces to the bootstrap tenant again, as before.
CREATE OR REPLACE FUNCTION operations_integration_failure_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM operations_record_event(
    'integration.failure', 'integration_failure', NEW.id::text,
    'integration_failure:' || NEW.id::text, 'internal',
    CASE WHEN NEW.status = 'dead_letter' THEN 'urgent' ELSE 'high' END,
    'Fallo de integración requiere atención', 'Integration failure needs attention',
    NEW.redacted_summary, NEW.redacted_summary,
    jsonb_build_object('provider', NEW.provider, 'failureCode', NEW.failure_code,
      'retryable', NEW.retryable, 'terminal', false),
    NEW.created_at, NULL, NEW.status = 'dead_letter'
  );
  RETURN NEW;
END;
$$;
DROP FUNCTION IF EXISTS operations_record_scoped_event(UUID, UUID, TEXT, TEXT, TEXT, TEXT, TEXT, TEXT, TEXT, TEXT, TEXT, TEXT, JSONB, TIMESTAMPTZ, TEXT, BOOLEAN);
COMMIT;
