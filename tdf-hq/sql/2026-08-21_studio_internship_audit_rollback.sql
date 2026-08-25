-- Roll back the internship studio-audit workflow while preserving legacy feedback rows.
\set ON_ERROR_STOP on

BEGIN;

DROP TRIGGER IF EXISTS trg_enforce_intern_audit_completion ON intern_task;
DROP FUNCTION IF EXISTS enforce_intern_audit_completion();
DROP TRIGGER IF EXISTS trg_intern_test_execution_refresh_progress ON intern_test_execution;
DROP FUNCTION IF EXISTS intern_test_execution_refresh_progress();
DROP FUNCTION IF EXISTS refresh_intern_audit_task_progress(UUID);

DROP TABLE IF EXISTS intern_audit_notification_outbox;
DROP TABLE IF EXISTS internal_feedback_retest;
DROP TABLE IF EXISTS internal_feedback_history;
DROP TABLE IF EXISTS internal_feedback_comment;
DROP TABLE IF EXISTS internal_feedback_evidence;
DROP TABLE IF EXISTS internal_feedback_report;
DROP TABLE IF EXISTS intern_final_summary;
DROP TABLE IF EXISTS intern_daily_summary;
DROP TABLE IF EXISTS intern_test_execution;
DROP TABLE IF EXISTS intern_test_case;
DROP TABLE IF EXISTS intern_audit_plan;

ALTER TABLE intern_task
  DROP CONSTRAINT IF EXISTS intern_task_activation_status_check,
  DROP COLUMN IF EXISTS proposed_assignee,
  DROP COLUMN IF EXISTS activation_status;

ALTER TABLE intern_project
  DROP CONSTRAINT IF EXISTS intern_project_activation_status_check,
  DROP COLUMN IF EXISTS notifications_enabled,
  DROP COLUMN IF EXISTS activated_at,
  DROP COLUMN IF EXISTS activation_status;

COMMIT;
