SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.legacy_rows() FROM event_rehearsal.expected_legacy),
  'apply/reapply preserves all legacy fixture records and the production ledger');
SELECT event_rehearsal.check_that(
  NOT EXISTS (SELECT * FROM event_rehearsal.expected_columns EXCEPT
    SELECT table_name,column_name,data_type,is_nullable,column_default
    FROM information_schema.columns WHERE table_schema='public'),
  'legacy column contracts remain unchanged');
SELECT event_rehearsal.check_that(
  (SELECT canonical_state='planning' AND version=1 FROM event_operation_event_state WHERE event_id=900010),
  'existing catalog lifecycle maps to planning');
SELECT event_rehearsal.check_that(
  (SELECT count(*)=1 FROM event_operation_relationship
   WHERE event_id=900010 AND party_id=900001 AND relationship_kind='primary_owner'),
  'canonical organization owner is backfilled once');
SELECT event_rehearsal.check_that(
  (SELECT count(*)=1 FROM event_operation_migration_issue
   WHERE event_id=900011 AND issue_code='owner_missing'), 'unresolved owner is explicit and deduplicated');
SELECT event_rehearsal.check_that(
  (SELECT NOT enabled FROM event_operation_feature_flag WHERE feature_code='event.operations.api'),
  'event API is disabled after migration and reapply');

-- Local-only activation is necessary to exercise the real SQL entry point, never a live provider.
UPDATE event_operation_feature_flag SET enabled=TRUE,updated_by_party_id=900001,
  change_reason='disposable schema rehearsal only' WHERE feature_code='event.operations.api';
DO $$ DECLARE outcome JSONB; replay JSONB; BEGIN
  outcome := event_operation_apply_transition(900010,900001,
    '90000000-0000-4000-8000-000000000001',1,'pending_approval',NULL,
    'schema-rehearsal',encode(digest('schema-rehearsal','sha256'),'hex'));
  PERFORM event_rehearsal.check_that(outcome->>'canonicalState'='pending_approval'
    AND outcome->>'version'='2' AND outcome->>'replayed'='false', 'accepted production-schema command');
  replay := event_operation_apply_transition(900010,900001,
    '90000000-0000-4000-8000-000000000001',1,'pending_approval',NULL,
    'schema-rehearsal',encode(digest('schema-rehearsal','sha256'),'hex'));
  PERFORM event_rehearsal.check_that(replay=(outcome || '{"replayed":true}'::jsonb),
    'exact retry changes only replay metadata');
  PERFORM event_rehearsal.check_that(event_operation_read_snapshot(900010,900002) IS NULL,
    'unrelated collaborator cannot read the owned event');
END $$;

BEGIN;
INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
VALUES (900010,900001,'accountable',900001),(900010,900002,'responsible',900001);
INSERT INTO event_operation_task_policy(activity_id) VALUES (900010);
COMMIT;
DO $$ BEGIN
  BEGIN
    UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=900010;
    SET CONSTRAINTS ALL IMMEDIATE;
    RAISE EXCEPTION 'Blocked completion was accepted';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
  BEGIN
    DELETE FROM event_operation_raci_assignment WHERE activity_id=900010 AND raci_role='responsible';
    SET CONSTRAINTS ALL IMMEDIATE;
    RAISE EXCEPTION 'Orphan responsibility was accepted';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
  BEGIN
    INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (900011,900010);
    SET CONSTRAINTS ALL IMMEDIATE;
    RAISE EXCEPTION 'Dependency cycle was accepted';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
END $$;
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.legacy_rows() FROM event_rehearsal.expected_legacy),
  'rejected task writes and command leave legacy rows intact');
SELECT event_rehearsal.check_that(
  (SELECT count(*)=1 FROM event_operation_transition WHERE event_id=900010)
  AND (SELECT count(*)=1 FROM event_operation_command_receipt WHERE event_id=900010)
  AND (SELECT count(*)=1 FROM event_operation_audit_event WHERE event_id=900010),
  'one transition, receipt and audit after retry');

CREATE FUNCTION event_rehearsal.history_rows() RETURNS JSONB LANGUAGE sql AS $$
  SELECT jsonb_build_object(
    'transition',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_transition t),
    'receipt',(SELECT jsonb_agg(to_jsonb(t) ORDER BY command_id) FROM event_operation_command_receipt t),
    'audit',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_audit_event t),
    'raci',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_raci_assignment t))
$$;
CREATE TABLE event_rehearsal.expected_history AS SELECT event_rehearsal.history_rows() AS snapshot;

-- The scoped projection also compiles/runs against the complete canonical schema,
-- not only the reduced concurrency fixture. Assignment alone is not read authority.
SELECT event_rehearsal.check_that(
  event_operation_read_task(900010,900010,900001) = '{
    "eventId":900010,"activityId":900010,"status":"planned","version":1,
    "policy":{"requiresAccountability":true,"dependenciesGateCompletion":true,"version":1},
    "raci":[{"partyId":900001,"role":"accountable"},{"partyId":900002,"role":"responsible"}],
    "accountabilityNeedsAttention":false}'::JSONB,
  'canonical schema task projection is exact and allowlisted');
SELECT event_rehearsal.check_that(event_operation_read_task(900010,900010,900002) IS NULL,
  'RACI alone cannot disclose task state');
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.history_rows() FROM event_rehearsal.expected_history)
  AND (SELECT snapshot=event_rehearsal.legacy_rows() FROM event_rehearsal.expected_legacy),
  'task reads preserve legacy and immutable records');

SELECT event_rehearsal.check_that(
  (SELECT revision=4 FROM event_operation_task_revision WHERE activity_id=900010)
  AND (SELECT revision=1 FROM event_operation_task_revision WHERE activity_id=900011),
  'aggregate revisions reflect two RACI inserts and one policy, with failed writes rolled back');
SELECT event_operation_lock_task_revision(900010,900010,4);
SELECT event_rehearsal.check_that(
  event_operation_read_task_with_revision(900010,900010,900001) = jsonb_build_object(
    'task',event_operation_read_task(900010,900010,900001),'aggregateRevision','4')
  AND event_operation_read_task_with_revision(900010,900010,900002) IS NULL,
  'full-schema opt-in revision envelope reuses scoped canonical projection');
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.history_rows() FROM event_rehearsal.expected_history)
  AND (SELECT snapshot=event_rehearsal.legacy_rows() FROM event_rehearsal.expected_legacy),
  'revisioned read does not mutate legacy or immutable history');
CREATE TABLE event_rehearsal.expected_task_revisions AS
  SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) AS snapshot FROM event_operation_task_revision t;
