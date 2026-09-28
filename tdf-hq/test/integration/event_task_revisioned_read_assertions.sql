CREATE SCHEMA task_revision_read_test;
CREATE FUNCTION task_revision_read_test.check_that(ok BOOLEAN, message TEXT) RETURNS VOID
LANGUAGE plpgsql AS $$ BEGIN
  IF ok IS DISTINCT FROM TRUE THEN RAISE EXCEPTION 'Revision read assertion: %', message; END IF;
END $$;
UPDATE event_operation_feature_flag SET enabled=TRUE,updated_by_party_id=1,
  change_reason='disposable revision read test' WHERE feature_code='event.operations.api';
BEGIN;
INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
  VALUES (100,1,'accountable',1),(100,2,'responsible',1);
INSERT INTO event_operation_task_policy(activity_id) VALUES (100);
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
  VALUES (10,2,'task.read','task','100',1);
COMMIT;

CREATE FUNCTION task_revision_read_test.rows() RETURNS JSONB LANGUAGE sql AS $$
  SELECT jsonb_build_object(
    'activity',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_logistics_activity t),
    'policy',(SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) FROM event_operation_task_policy t),
    'raci',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_raci_assignment t),
    'revision',(SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) FROM event_operation_task_revision t),
    'fence',(SELECT jsonb_agg(to_jsonb(t) ORDER BY event_id) FROM event_operation_task_write_fence t),
    'state',(SELECT jsonb_agg(to_jsonb(t) ORDER BY event_id) FROM event_operation_event_state t),
    'grant',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_grant t),
    'audit',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_audit_event t),
    'receipt',(SELECT jsonb_agg(to_jsonb(t) ORDER BY command_id) FROM event_operation_command_receipt t))
$$;
CREATE TABLE task_revision_read_test.preserved AS SELECT task_revision_read_test.rows() AS snapshot;
DO $$ DECLARE envelope JSONB; BEGIN
  envelope := event_operation_read_task_with_revision(10,100,1);
  PERFORM task_revision_read_test.check_that(envelope = jsonb_build_object(
    'task',event_operation_read_task(10,100,1),'aggregateRevision','4'), 'exact canonical envelope');
  PERFORM task_revision_read_test.check_that(event_operation_read_task_with_revision(10,100,2)=envelope,
    'exact-task grant permits only same representation');
  PERFORM task_revision_read_test.check_that(
    event_operation_read_task_with_revision(11,100,1) IS NULL
    AND event_operation_read_task_with_revision(10,102,1) IS NULL
    AND event_operation_read_task_with_revision(10,999,1) IS NULL
    AND event_operation_read_task_with_revision(10,100,3) IS NULL
    AND event_operation_read_task_with_revision(10,101,2) IS NULL,
    'wrong actor/event/sibling and absent target are opaque');
  PERFORM task_revision_read_test.check_that(
    event_operation_read_task_with_revision(10,101,1)->>'aggregateRevision'='1', 'unconfigured task still revisioned');
END $$;
SELECT task_revision_read_test.check_that(
  (SELECT snapshot=task_revision_read_test.rows() FROM task_revision_read_test.preserved), 'reads change no records');

BEGIN;
UPDATE event_operation_task_revision SET revision=9223372036854775807 WHERE activity_id=100;
SELECT task_revision_read_test.check_that(
  event_operation_read_task_with_revision(10,100,1)->'aggregateRevision'='"9223372036854775807"'::JSONB,
  'maximum BIGINT is text, not a rounded number');
ROLLBACK;
BEGIN;
DELETE FROM event_operation_task_revision WHERE activity_id=100;
SELECT task_revision_read_test.check_that(event_operation_read_task_with_revision(10,100,1) IS NULL
  AND event_operation_read_task(10,100,1) IS NOT NULL
  AND NOT EXISTS(SELECT 1 FROM event_operation_task_revision WHERE activity_id=100), 'missing metadata never repaired by read');
ROLLBACK;
BEGIN;
UPDATE event_operation_grant SET scope_code='event.read',resource_kind='event',resource_id=NULL;
SELECT task_revision_read_test.check_that(event_operation_read_task_with_revision(10,100,2) IS NULL, 'no ambient event.read');
ROLLBACK;
BEGIN;
UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='test only';
SELECT task_revision_read_test.check_that(event_operation_read_task_with_revision(10,100,2) IS NULL, 'revocation denies');
ROLLBACK;
BEGIN;
UPDATE event_operation_feature_flag SET enabled=FALSE;
SELECT task_revision_read_test.check_that(event_operation_read_task_with_revision(10,100,1) IS NULL, 'disabled denies');
ROLLBACK;
SELECT task_revision_read_test.check_that(
  (SELECT snapshot=task_revision_read_test.rows() FROM task_revision_read_test.preserved), 'fault probes roll back exactly');
