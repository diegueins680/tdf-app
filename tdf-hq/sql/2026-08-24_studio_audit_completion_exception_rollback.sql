-- Restore the original completion-exception predicate. Refuse to discard an
-- explicit approval so rollback cannot silently change an active decision.
BEGIN;

DO $rollback_guard$
BEGIN
  IF EXISTS (
    SELECT 1 FROM intern_audit_plan WHERE completion_exception_approved
  ) THEN
    RAISE EXCEPTION 'Cannot roll back while an explicit completion exception is approved';
  END IF;
END
$rollback_guard$;

CREATE OR REPLACE FUNCTION enforce_intern_audit_completion()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
DECLARE
  target_plan intern_audit_plan%ROWTYPE;
  missing_cases BIGINT;
  missing_critical BIGINT;
  unresolved_blockers BIGINT;
  failed_without_report BIGINT;
  missing_evidence BIGINT;
  missing_daily BIGINT;
  missing_final BIGINT;
BEGIN
  IF NEW.status <> 'done' OR OLD.status = 'done' THEN
    RETURN NEW;
  END IF;

  SELECT * INTO target_plan
  FROM intern_audit_plan
  WHERE task_id = NEW.id AND status IN ('draft', 'active', 'completed')
  LIMIT 1;

  IF NOT FOUND THEN
    RETURN NEW;
  END IF;

  IF target_plan.completion_justification IS NOT NULL
     AND target_plan.completion_approved_by IS NOT NULL
     AND target_plan.completion_approved_at IS NOT NULL THEN
    RETURN NEW;
  END IF;

  WITH latest AS (
    SELECT DISTINCT ON (execution.test_case_id)
      execution.test_case_id, execution.id AS execution_id, execution.status,
      execution.evidence_summary
    FROM intern_test_execution execution
    JOIN intern_test_case test_case ON test_case.id = execution.test_case_id
    WHERE test_case.plan_id = target_plan.id
    ORDER BY execution.test_case_id, execution.execution_number DESC
  )
  SELECT
    count(*) FILTER (
      WHERE test_case.applicable
        AND COALESCE(latest.status, 'pending') NOT IN ('passed', 'failed', 'blocked', 'not_applicable', 'verified')
    ),
    count(*) FILTER (
      WHERE test_case.applicable AND test_case.criticality = 'critical'
        AND COALESCE(latest.status, 'pending') NOT IN ('passed', 'not_applicable', 'verified')
    ),
    count(*) FILTER (
      WHERE test_case.applicable AND latest.status = 'failed'
        AND NOT EXISTS (
          SELECT 1
          FROM internal_feedback_report report
          WHERE (report.test_case_id = test_case.id
             OR report.test_execution_id = latest.execution_id)
            AND report.state <> 'draft'
            AND report.submitted_at IS NOT NULL
        )
    ),
    count(*) FILTER (
      WHERE test_case.applicable
        AND test_case.evidence_requirement = 'strong'
        AND latest.status IN ('passed', 'failed', 'blocked', 'not_applicable', 'verified')
        AND length(btrim(COALESCE(latest.evidence_summary, ''))) = 0
        AND NOT EXISTS (
          SELECT 1
          FROM internal_feedback_report report
          JOIN internal_feedback_evidence evidence ON evidence.report_id = report.id
          WHERE report.test_case_id = test_case.id
             OR report.test_execution_id = latest.execution_id
        )
    )
  INTO missing_cases, missing_critical, failed_without_report, missing_evidence
  FROM intern_test_case test_case
  LEFT JOIN latest ON latest.test_case_id = test_case.id
  WHERE test_case.plan_id = target_plan.id;

  SELECT count(*) INTO unresolved_blockers
  FROM internal_feedback_report report
  WHERE report.internship_task_id = NEW.id
    AND (
      report.state = 'ready_for_retest'
      OR (
        report.blocking
        AND report.state NOT IN ('verified', 'closed', 'duplicate', 'discarded')
      )
    );

  SELECT CASE WHEN count(*) = 0 THEN 1 ELSE 0 END INTO missing_daily
  FROM intern_daily_summary summary
  WHERE summary.task_id = NEW.id;

  SELECT CASE WHEN EXISTS (
    SELECT 1
    FROM intern_final_summary summary
    WHERE summary.plan_id = target_plan.id
      AND summary.submitted_at IS NOT NULL
      AND NOT EXISTS (
        SELECT 1
        FROM intern_test_execution execution
        JOIN intern_test_case test_case ON test_case.id = execution.test_case_id
        WHERE test_case.plan_id = target_plan.id
          AND execution.updated_at > summary.submitted_at
      )
      AND NOT EXISTS (
        SELECT 1
        FROM internal_feedback_report report
        WHERE report.internship_task_id = NEW.id
          AND report.updated_at > summary.submitted_at
      )
  ) THEN 0 ELSE 1 END INTO missing_final;

  IF missing_cases > 0 OR missing_critical > 0 OR unresolved_blockers > 0
     OR failed_without_report > 0 OR missing_evidence > 0
     OR missing_daily > 0 OR (target_plan.final_review_required AND missing_final > 0) THEN
    RAISE EXCEPTION USING
      ERRCODE = 'check_violation',
      MESSAGE = format(
        'Intern audit cannot be completed: unexecuted=%s critical_unfinished=%s blockers=%s failed_without_report=%s evidence_missing=%s daily_summary_missing=%s final_summary_missing=%s',
        missing_cases, missing_critical, unresolved_blockers, failed_without_report,
        missing_evidence, missing_daily, missing_final
      );
  END IF;

  RETURN NEW;
END $$;

ALTER TABLE intern_audit_plan
  DROP COLUMN completion_exception_approved;

COMMIT;
