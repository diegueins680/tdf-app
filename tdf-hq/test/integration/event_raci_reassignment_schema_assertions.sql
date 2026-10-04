-- Exercise the real schema without changing the shared rollback/reapply snapshots.
BEGIN;
UPDATE event_operation_event_state SET canonical_state='planning' WHERE event_id=900010;
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
VALUES (900010,900002,'task.read','task','900010',900001);
DO $$
DECLARE result JSONB; retry JSONB;
BEGIN
  result := event_operation_reassign_raci(900010,900010,900001,
    '20000000-0000-4000-8000-000000000001',4,'accountable',900001,900002,
    'Synthetic full-schema reassignment','schema-rehearsal');
  PERFORM event_rehearsal.check_that(result->>'aggregateRevision'='6'
    AND result->>'replayed'='false', 'real schema accepts atomic Accountable replacement');
  retry := event_operation_reassign_raci(900010,900010,900001,
    '20000000-0000-4000-8000-000000000001',4,'accountable',900001,900002,
    'Synthetic full-schema reassignment','schema-rehearsal');
  PERFORM event_rehearsal.check_that(retry=result||'{"replayed":true}'::JSONB,
    'real schema replays exact accepted version');
END $$;
SET CONSTRAINTS ALL IMMEDIATE;
ROLLBACK;
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.history_rows() FROM event_rehearsal.expected_history),
  'aborted reassignment preserves existing immutable evidence');
