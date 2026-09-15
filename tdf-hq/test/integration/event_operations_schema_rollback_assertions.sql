SELECT event_rehearsal.check_that(
  (SELECT NOT enabled FROM event_operation_feature_flag WHERE feature_code='event.operations.api'),
  'rollback disables event operations');
SELECT event_rehearsal.check_that(
  to_regprocedure('event_operation_read_snapshot(bigint,bigint)') IS NULL
  AND to_regprocedure('event_operation_apply_transition(bigint,bigint,uuid,bigint,text,text,text,text)') IS NULL,
  'rollback removes the public SQL entry points');
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.history_rows() FROM event_rehearsal.expected_history),
  'rollback retains exact receipt, transition, audit and RACI history');
SELECT event_rehearsal.check_that(
  (SELECT snapshot=event_rehearsal.legacy_rows() FROM event_rehearsal.expected_legacy),
  'rollback preserves legacy fixture rows and production ledger');
