-- Canonical complete schema, not a substitute for committed/race tests.
BEGIN;
UPDATE event_operation_event_state SET canonical_state='planning' WHERE event_id=900010;
DO $$ DECLARE result JSONB; replay JSONB; original event_logistics_activity%ROWTYPE; BEGIN
  SELECT * INTO original FROM event_logistics_activity WHERE id=900010;
  result := event_operation_complete_task(900010,900010,900001,
    '30000000-0000-4000-8000-000000000001',4,'Synthetic preparation completed','schema-rehearsal');
  PERFORM event_rehearsal.check_that(result='{"error":"dependencies_not_ready"}'::JSONB,
    'complete-schema command cannot bypass its blocked prerequisite');
  UPDATE event_logistics_activity SET status='completed',version=version+1 WHERE id=900011;
  result := event_operation_complete_task(900010,900010,900001,
    '30000000-0000-4000-8000-000000000001',4,'Synthetic preparation completed','schema-rehearsal');
  PERFORM event_rehearsal.check_that(result->>'status'='completed' AND result->>'activityVersion'='2'
    AND result->>'aggregateRevision'='5' AND result->>'replayed'='false',
    'complete-schema command updates both versions exactly once');
  PERFORM event_rehearsal.check_that((SELECT (to_jsonb(t)-ARRAY['status','version','updated_at'])
      =(to_jsonb(original)-ARRAY['status','version','updated_at'])
    FROM event_logistics_activity t WHERE id=900010),'all other real activity fields preserved');
  replay := event_operation_complete_task(900010,900010,900001,
    '30000000-0000-4000-8000-000000000001',4,'Synthetic preparation completed','schema-rehearsal');
  PERFORM event_rehearsal.check_that(replay=result||'{"replayed":true}'::JSONB,'complete-schema exact retry');
  PERFORM event_rehearsal.check_that((SELECT count(*)=1 FROM event_operation_audit_event
    WHERE operation_code='event.task.complete' AND resource_id='900010'),'one completion audit');
END $$;
SET CONSTRAINTS ALL IMMEDIATE;
ROLLBACK;
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.history_rows() FROM event_rehearsal.expected_history)
  AND (SELECT snapshot=event_rehearsal.legacy_rows() FROM event_rehearsal.expected_legacy),
  'aborted complete-schema command restores legacy and immutable evidence');
