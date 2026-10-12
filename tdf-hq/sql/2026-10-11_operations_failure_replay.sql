-- A dead-lettered projection blocks every later event of its record, and the
-- manager's retry only relabelled the failure: nothing ran the projection again
-- and nothing ever moved the failure out of 'retrying'. These triggers make the
-- relabel itself re-queue the projection and keep the failure row truthful
-- afterwards, whichever server version performs the retry.
-- Safe to apply again: functions and triggers are replaced, repairs find nothing.
BEGIN;
SET LOCAL lock_timeout = '10s';

-- Announce or close the work item of a failure without letting bookkeeping
-- break the projection or the retry that caused it.
CREATE OR REPLACE FUNCTION operations_integration_failure_announce(
  p_failure operations_integration_failure, p_terminal BOOLEAN
) RETURNS void LANGUAGE plpgsql AS $$
BEGIN
  PERFORM operations_record_event(
    'integration.failure', 'integration_failure', p_failure.id::text,
    'integration_failure:' || p_failure.id::text, 'internal',
    CASE WHEN p_terminal THEN 'high' ELSE 'urgent' END,
    CASE WHEN p_terminal THEN 'Fallo de integración resuelto' ELSE 'Fallo de integración requiere atención' END,
    CASE WHEN p_terminal THEN 'Integration failure resolved' ELSE 'Integration failure needs attention' END,
    p_failure.redacted_summary, p_failure.redacted_summary,
    jsonb_build_object('provider', p_failure.provider, 'failureCode', p_failure.failure_code,
      'retryable', p_failure.retryable, 'terminal', p_terminal),
    clock_timestamp(), NULL, NOT p_terminal
  );
EXCEPTION WHEN OTHERS THEN
  RAISE WARNING 'operations failure work item not updated: %', SQLSTATE;
END;
$$;

-- A retry: put the record's dead-lettered projection back in the queue. With
-- nothing left unprocessed there is nothing to retry and the failure is over.
CREATE OR REPLACE FUNCTION operations_integration_failure_retry()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE operations_outbox
  SET status = 'pending', attempt_count = 0, next_attempt_at = clock_timestamp(),
      locked_at = NULL, locked_by = NULL, last_error = NULL
  WHERE organization_id = NEW.organization_id
    AND aggregate_type = NEW.source_record_type
    AND aggregate_id = NEW.source_record_id
    AND status = 'dead_letter';
  IF NOT EXISTS (
    SELECT 1 FROM operations_outbox
    WHERE organization_id = NEW.organization_id
      AND aggregate_type = NEW.source_record_type
      AND aggregate_id = NEW.source_record_id
      AND status <> 'processed'
  ) THEN
    NEW.status := 'resolved';
    NEW.resolved_at := clock_timestamp();
    NEW.next_attempt_at := NULL;
  END IF;
  RETURN NEW;
END;
$$;

CREATE OR REPLACE TRIGGER operations_integration_failure_retry
  BEFORE UPDATE OF status ON operations_integration_failure
  FOR EACH ROW
  WHEN (NEW.status = 'retrying' AND OLD.status IS DISTINCT FROM 'retrying'
    AND NEW.provider = 'internal_outbox')
  EXECUTE FUNCTION operations_integration_failure_retry();

-- The projection ran: its failure, in whatever open state, is over.
CREATE OR REPLACE FUNCTION operations_outbox_settle_failure()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE operations_integration_failure
  SET status = 'resolved', resolved_at = clock_timestamp(), next_attempt_at = NULL
  WHERE organization_id = NEW.organization_id
    AND source_record_type = NEW.aggregate_type
    AND source_record_id = NEW.aggregate_id
    AND provider = 'internal_outbox'
    AND status <> 'resolved';
  RETURN NEW;
END;
$$;

CREATE OR REPLACE TRIGGER operations_outbox_settle_failure
  AFTER UPDATE OF status ON operations_outbox
  FOR EACH ROW
  WHEN (NEW.status = 'processed' AND OLD.status IS DISTINCT FROM 'processed')
  EXECUTE FUNCTION operations_outbox_settle_failure();

-- The projection dead-lettered again: return the existing failure to
-- 'dead_letter' and reopen its work item instead of opening a second failure.
CREATE OR REPLACE FUNCTION operations_integration_failure_merge()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  existing operations_integration_failure;
BEGIN
  UPDATE operations_integration_failure
  SET status = 'dead_letter', failure_code = NEW.failure_code,
      last_attempt_at = COALESCE(NEW.last_attempt_at, clock_timestamp()), next_attempt_at = NULL
  WHERE organization_id = NEW.organization_id
    AND source_record_type = NEW.source_record_type
    AND source_record_id = NEW.source_record_id
    AND provider = 'internal_outbox'
    AND status <> 'resolved'
  RETURNING * INTO existing;
  IF FOUND THEN
    PERFORM operations_integration_failure_announce(existing, FALSE);
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$;

CREATE OR REPLACE TRIGGER operations_integration_failure_merge
  BEFORE INSERT ON operations_integration_failure
  FOR EACH ROW
  WHEN (NEW.provider = 'internal_outbox')
  EXECUTE FUNCTION operations_integration_failure_merge();

-- Close the work item that announced the failure.
CREATE OR REPLACE FUNCTION operations_integration_failure_resolved_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM operations_integration_failure_announce(NEW, TRUE);
  RETURN NEW;
END;
$$;

CREATE OR REPLACE TRIGGER operations_integration_failure_resolved
  AFTER UPDATE OF status ON operations_integration_failure
  FOR EACH ROW
  WHEN (NEW.status = 'resolved' AND OLD.status IS DISTINCT FROM 'resolved')
  EXECUTE FUNCTION operations_integration_failure_resolved_event();

-- Repairs of rows written before these triggers existed.
-- 1. Relabelled 'retrying' while the projection stayed dead-lettered: never
--    retried. Make it retryable again.
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

-- 2. Several open failures for one record: keep the oldest.
UPDATE operations_integration_failure failure
SET status = 'resolved', resolved_at = clock_timestamp(), next_attempt_at = NULL
WHERE failure.provider = 'internal_outbox'
  AND failure.status <> 'resolved'
  AND EXISTS (
    SELECT 1 FROM operations_integration_failure older
    WHERE older.organization_id = failure.organization_id
      AND older.source_record_type = failure.source_record_type
      AND older.source_record_id = failure.source_record_id
      AND older.provider = 'internal_outbox'
      AND older.status <> 'resolved'
      AND (older.created_at, older.id) < (failure.created_at, failure.id)
  );

-- 3. Open although every projection of the record has run.
UPDATE operations_integration_failure failure
SET status = 'resolved', resolved_at = clock_timestamp(), next_attempt_at = NULL
WHERE failure.provider = 'internal_outbox'
  AND failure.status <> 'resolved'
  AND NOT EXISTS (
    SELECT 1 FROM operations_outbox outbox
    WHERE outbox.organization_id = failure.organization_id
      AND outbox.aggregate_type = failure.source_record_type
      AND outbox.aggregate_id = failure.source_record_id
      AND outbox.status <> 'processed'
  );

-- One open failure per record, enforced; also the lookup the triggers use.
CREATE UNIQUE INDEX IF NOT EXISTS operations_integration_failure_open_uidx
  ON operations_integration_failure (organization_id, source_record_type, source_record_id)
  WHERE provider = 'internal_outbox' AND status <> 'resolved';

COMMIT;
