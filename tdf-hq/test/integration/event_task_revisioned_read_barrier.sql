-- Test-only instrumentation creates a deterministic gap AFTER revision capture and
-- BEFORE the unmodified canonical projector. Never installed outside this fixture.
ALTER FUNCTION event_operation_read_task(BIGINT,BIGINT,BIGINT) RENAME TO task_revision_read_saved;
CREATE FUNCTION event_operation_read_task(BIGINT,BIGINT,BIGINT) RETURNS JSONB
LANGUAGE plpgsql VOLATILE AS $$ BEGIN
  PERFORM pg_advisory_xact_lock(889,3);
  RETURN task_revision_read_saved($1,$2,$3);
END $$;
