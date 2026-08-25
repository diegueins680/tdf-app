-- Preserve every failed or blocked execution as a completion dependency, even
-- when a later execution for the same case passes. This is a forward-only
-- companion to the already-applied completion-exception migration.
BEGIN;

CREATE OR REPLACE FUNCTION enforce_intern_audit_historical_failures()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
DECLARE
  target_plan intern_audit_plan%ROWTYPE;
  unreported_failures BIGINT;
BEGIN
  IF NEW.status <> 'done' OR OLD.status = 'done' THEN
    RETURN NEW;
  END IF;

  SELECT * INTO target_plan
  FROM intern_audit_plan
  WHERE task_id = NEW.id AND status IN ('draft', 'active', 'completed')
  LIMIT 1;

  IF NOT FOUND OR target_plan.completion_exception_approved THEN
    RETURN NEW;
  END IF;

  SELECT count(*) INTO unreported_failures
  FROM intern_test_execution execution
  JOIN intern_test_case test_case ON test_case.id = execution.test_case_id
  WHERE test_case.plan_id = target_plan.id
    AND test_case.applicable
    AND execution.status IN ('failed', 'blocked')
    AND NOT EXISTS (
      SELECT 1
      FROM internal_feedback_report report
      WHERE (report.test_execution_id = execution.id
        OR EXISTS (
          SELECT 1
          FROM internal_feedback_retest retest
          WHERE retest.report_id = report.id
            AND retest.execution_id = execution.id
        ))
        AND report.state <> 'draft'
        AND report.submitted_at IS NOT NULL
    );

  IF unreported_failures > 0 THEN
    RAISE EXCEPTION USING
      ERRCODE = 'check_violation',
      MESSAGE = format(
        'Intern audit cannot be completed: historical_failed_or_blocked_without_report=%s',
        unreported_failures
      );
  END IF;

  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_enforce_intern_audit_historical_failures ON intern_task;
CREATE TRIGGER trg_enforce_intern_audit_historical_failures
BEFORE UPDATE OF status ON intern_task
FOR EACH ROW EXECUTE FUNCTION enforce_intern_audit_historical_failures();

COMMIT;
