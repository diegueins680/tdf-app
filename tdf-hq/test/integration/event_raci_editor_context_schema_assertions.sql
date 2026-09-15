SELECT event_rehearsal.check_that(event_operation_read_raci_editor_context(900010,900010,900001,0)
  ->>'operationReady'='false', 'pending-approval real event is not actionable');
BEGIN;
UPDATE event_operation_event_state SET canonical_state='planning' WHERE event_id=900010;
DO $$ DECLARE context JSONB; BEGIN
  context := event_operation_read_raci_editor_context(900010,900010,900001,0);
  PERFORM event_rehearsal.check_that(context->>'aggregateRevision'='4'
    AND context->>'canManage'='true' AND context->>'operationReady'='true'
    AND context->'eligiblePartyIds'='[900001]'::JSONB
    AND jsonb_array_length(context->'replaceableAssignments')=2,
    'real schema editor context reuses owner, task and revision without contact disclosure');
END $$;
ROLLBACK;
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.history_rows() FROM event_rehearsal.expected_history)
  AND (SELECT snapshot=event_rehearsal.legacy_rows() FROM event_rehearsal.expected_legacy),
  'context read preserves historical and legacy records');
