CREATE FUNCTION raci_command_test.fail_write() RETURNS TRIGGER LANGUAGE plpgsql AS $$ BEGIN
  RAISE EXCEPTION 'synthetic persistence failure' USING ERRCODE='ZX001';
END $$;
CREATE TRIGGER raci_command_test_failure BEFORE INSERT ON event_operation_audit_event
  FOR EACH ROW EXECUTE FUNCTION raci_command_test.fail_write();
DO $$ DECLARE before_rows JSONB := raci_command_test.rows(); BEGIN
  BEGIN
    PERFORM raci_command_test.command(302,1,900,raci_command_test.rev(302),'responsible',2,3);
    RAISE EXCEPTION 'audit fault unexpectedly succeeded';
  EXCEPTION WHEN SQLSTATE 'ZX001' THEN NULL; END;
  PERFORM raci_command_test.check_that(raci_command_test.rows()=before_rows, 'audit failure rolls back RACI and revision');
END $$;
DROP TRIGGER raci_command_test_failure ON event_operation_audit_event;
CREATE TRIGGER raci_command_test_failure BEFORE INSERT ON event_operation_command_receipt
  FOR EACH ROW EXECUTE FUNCTION raci_command_test.fail_write();
DO $$ DECLARE before_rows JSONB := raci_command_test.rows(); BEGIN
  BEGIN
    PERFORM raci_command_test.command(302,1,900,raci_command_test.rev(302),'responsible',2,3);
    RAISE EXCEPTION 'receipt fault unexpectedly succeeded';
  EXCEPTION WHEN SQLSTATE 'ZX001' THEN NULL; END;
  PERFORM raci_command_test.check_that(raci_command_test.rows()=before_rows, 'receipt failure rolls back RACI, revision and audit');
END $$;
DROP TRIGGER raci_command_test_failure ON event_operation_command_receipt;

DO $$ DECLARE before_rows JSONB := raci_command_test.rows(); result JSONB; BEGIN
  BEGIN
    result := raci_command_test.command(302,1,900,raci_command_test.rev(302),'responsible',2,3);
    PERFORM raci_command_test.check_that(result->>'replayed'='false','accepted before outer abort');
    RAISE EXCEPTION 'synthetic outer abort' USING ERRCODE='ZX002';
  EXCEPTION WHEN SQLSTATE 'ZX002' THEN NULL; END;
  PERFORM raci_command_test.check_that(raci_command_test.rows()=before_rows, 'outer abort restores all evidence and rows');
  BEGIN
    UPDATE event_operation_task_revision SET revision=9223372036854775806 WHERE activity_id=302;
    PERFORM raci_command_test.command(302,1,900,9223372036854775806,'responsible',2,3);
    RAISE EXCEPTION 'second tracker overflow unexpectedly succeeded';
  EXCEPTION WHEN numeric_value_out_of_range THEN NULL; END;
  PERFORM raci_command_test.check_that(raci_command_test.rows()=before_rows, 'second-write overflow rolls back first write');
  BEGIN
    UPDATE event_operation_task_revision SET revision=9223372036854775805 WHERE activity_id=302;
    result := raci_command_test.command(302,1,900,9223372036854775805,'responsible',2,3);
    PERFORM raci_command_test.check_that(result->'aggregateRevision'='"9223372036854775807"'::JSONB,
      'maximum result revision transported as exact text');
    RAISE EXCEPTION 'restore boundary probe' USING ERRCODE='ZX002';
  EXCEPTION WHEN SQLSTATE 'ZX002' THEN NULL; END;
  PERFORM raci_command_test.check_that(raci_command_test.rows()=before_rows,'boundary probe restores all rows');
  BEGIN
    SET CONSTRAINTS event_operation_raci_assignment_guard, event_operation_task_commit_guard IMMEDIATE;
    PERFORM raci_command_test.command(302,1,900,raci_command_test.rev(302),'accountable',1,3);
    RAISE EXCEPTION 'immediate required A constraint was silently deferred';
  EXCEPTION WHEN check_violation THEN NULL; END;
  PERFORM raci_command_test.check_that(raci_command_test.rows()=before_rows,'immediate constraints fail closed without partial replacement');
  BEGIN
    UPDATE event_operation_audit_event SET reason='tamper' WHERE resource_id='300';
    RAISE EXCEPTION 'audit mutation unexpectedly succeeded';
  EXCEPTION WHEN SQLSTATE '55000' THEN NULL; END;
  BEGIN
    DELETE FROM event_operation_command_receipt WHERE operation_code='event.task.raci.reassign/300';
    RAISE EXCEPTION 'receipt deletion unexpectedly succeeded';
  EXCEPTION WHEN SQLSTATE '55000' THEN NULL; END;
  PERFORM raci_command_test.check_that(raci_command_test.rows()=before_rows,'canonical audit and receipt remain immutable');
END $$;
