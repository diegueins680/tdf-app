SELECT event_rehearsal.check_that(
  (SELECT NOT enabled FROM event_operation_feature_flag WHERE feature_code='event.operations.api'),
  'roll-forward does not silently reactivate event operations');
SELECT event_rehearsal.check_that(
  to_regprocedure('event_operation_read_snapshot(bigint,bigint)') IS NOT NULL
  AND to_regprocedure('event_operation_read_task(bigint,bigint,bigint)') IS NOT NULL
  AND to_regprocedure('event_operation_read_task_with_revision(bigint,bigint,bigint)') IS NOT NULL
  AND to_regprocedure('event_operation_apply_transition(bigint,bigint,uuid,bigint,text,text,text,text)') IS NOT NULL,
  'roll-forward restores the entry points');
SELECT event_rehearsal.check_that(event_operation_read_task(900010,900010,900001) IS NULL,
  'restored task read remains disabled');
SELECT event_rehearsal.check_that(event_operation_read_task_with_revision(900010,900010,900001) IS NULL,
  'restored revision envelope remains disabled');
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.history_rows() FROM event_rehearsal.expected_history),
  'roll-forward preserves exact history');
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.legacy_rows() FROM event_rehearsal.expected_legacy),
  'roll-forward preserves legacy fixture rows and production ledger');
SELECT event_rehearsal.check_that(
  (SELECT canonical_state='pending_approval' AND version=2
   FROM event_operation_event_state WHERE event_id=900010),
  'backfill does not overwrite committed canonical state from older legacy state');
DO $$ BEGIN
  BEGIN
    UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=900010;
    SET CONSTRAINTS ALL IMMEDIATE;
    RAISE EXCEPTION 'Roll-forward did not restore completion guard';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
  BEGIN
    UPDATE event_operation_audit_event SET operation_code='tampered' WHERE event_id=900010;
    RAISE EXCEPTION 'Audit update was accepted';
  EXCEPTION WHEN SQLSTATE '55000' THEN NULL;
  END;
END $$;
SELECT event_rehearsal.check_that(
  (SELECT snapshot=(SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) FROM event_operation_task_revision t)
    FROM event_rehearsal.expected_task_revisions), 'roll-forward never resets aggregate counters');
SELECT event_operation_lock_task_revision(900010,900010,4);
