SELECT event_rehearsal.check_that(
  (SELECT NOT enabled FROM event_operation_feature_flag WHERE feature_code='event.operations.api'),
  'rollback disables event operations');
SELECT event_rehearsal.check_that(
  to_regprocedure('event_operation_read_snapshot(bigint,bigint)') IS NULL
  AND to_regprocedure('event_operation_read_task_with_revision(bigint,bigint,bigint)') IS NULL
  AND to_regprocedure('event_operation_apply_transition(bigint,bigint,uuid,bigint,text,text,text,text)') IS NULL,
  'rollback removes the public SQL entry points');
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.history_rows() FROM event_rehearsal.expected_history),
  'rollback retains exact receipt, transition, audit and RACI history');
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.legacy_rows() FROM event_rehearsal.expected_legacy),
  'rollback preserves legacy fixture rows and production ledger');
SELECT event_rehearsal.check_that(
  to_regprocedure('event_operation_lock_task_revision(bigint,bigint,bigint)') IS NULL
  AND (SELECT snapshot=(SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) FROM event_operation_task_revision t)
    FROM event_rehearsal.expected_task_revisions), 'rollback removes revision guard but retains counters');
SELECT event_rehearsal.check_that(
  NOT EXISTS (SELECT 1 FROM pg_proc WHERE pronamespace='public'::regnamespace
    AND proname IN ('event_operation_reassign_raci','event_operation_actor_can_manage_task')),
  'RACI command rollback removes only its entry points');
SELECT event_rehearsal.check_that(
  to_regprocedure('event_operation_read_raci_editor_context(bigint,bigint,bigint,bigint)') IS NULL,
  'editor context rollback removes the read entry point');
