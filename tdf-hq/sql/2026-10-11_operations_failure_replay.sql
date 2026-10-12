-- A dead-lettered projection blocks every later event of its record, and the
-- manager's retry only relabelled the failure: nothing ran the projection again
-- and nothing ever moved the failure out of 'retrying'. The API now re-queues
-- the projection; these triggers keep the failure row truthful afterwards.
-- Re-appliable: functions are replaced, triggers are dropped and recreated.
BEGIN;
SET LOCAL lock_timeout = '10s';

-- The projection ran: its failure, in whatever open state, is over.
CREATE OR REPLACE FUNCTION operations_outbox_settle_failure()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE operations_integration_failure
  SET status = 'resolved', resolved_at = now(), next_attempt_at = NULL
  WHERE organization_id = NEW.organization_id
    AND provider = 'internal_outbox'
    AND source_record_type = NEW.aggregate_type
    AND source_record_id = NEW.aggregate_id
    AND status <> 'resolved';
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_outbox_settle_failure ON operations_outbox;
CREATE TRIGGER operations_outbox_settle_failure
  AFTER UPDATE OF status ON operations_outbox
  FOR EACH ROW
  WHEN (NEW.status = 'processed' AND OLD.status IS DISTINCT FROM 'processed')
  EXECUTE FUNCTION operations_outbox_settle_failure();

-- The retried projection dead-lettered again: return the existing failure to
-- 'dead_letter' instead of opening a second failure for the same record.
CREATE OR REPLACE FUNCTION operations_integration_failure_merge()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE operations_integration_failure
  SET status = 'dead_letter', failure_code = NEW.failure_code,
      last_attempt_at = COALESCE(NEW.last_attempt_at, now()), next_attempt_at = NULL
  WHERE organization_id = NEW.organization_id
    AND provider = 'internal_outbox'
    AND source_record_type = NEW.source_record_type
    AND source_record_id = NEW.source_record_id
    AND status <> 'resolved';
  IF FOUND THEN
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_integration_failure_merge ON operations_integration_failure;
CREATE TRIGGER operations_integration_failure_merge
  BEFORE INSERT ON operations_integration_failure
  FOR EACH ROW
  WHEN (NEW.provider = 'internal_outbox')
  EXECUTE FUNCTION operations_integration_failure_merge();

-- Close the work item that announced the failure.
CREATE OR REPLACE FUNCTION operations_integration_failure_resolved_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM operations_record_event(
    'integration.failure', 'integration_failure', NEW.id::text,
    'integration_failure:' || NEW.id::text, 'internal', 'high',
    'Fallo de integración resuelto', 'Integration failure resolved',
    NEW.redacted_summary, NEW.redacted_summary,
    jsonb_build_object('provider', NEW.provider, 'failureCode', NEW.failure_code,
      'retryable', NEW.retryable, 'terminal', true),
    COALESCE(NEW.resolved_at, now()), NULL, FALSE
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_integration_failure_resolved ON operations_integration_failure;
CREATE TRIGGER operations_integration_failure_resolved
  AFTER UPDATE OF status ON operations_integration_failure
  FOR EACH ROW
  WHEN (NEW.status = 'resolved' AND OLD.status IS DISTINCT FROM 'resolved')
  EXECUTE FUNCTION operations_integration_failure_resolved_event();

-- Failures already relabelled 'retrying' while their projection is still
-- dead-lettered were never retried. Make them retryable again. After this
-- migration 'retrying' and a dead-lettered projection do not coexist, so a
-- second application finds nothing.
UPDATE operations_integration_failure failure
SET status = 'dead_letter', next_attempt_at = NULL
WHERE failure.status = 'retrying'
  AND failure.provider = 'internal_outbox'
  AND EXISTS (
    SELECT 1 FROM operations_outbox outbox
    WHERE outbox.organization_id = failure.organization_id
      AND outbox.aggregate_type = failure.source_record_type
      AND outbox.aggregate_id = failure.source_record_id
      AND outbox.status = 'dead_letter'
  );

COMMIT;
