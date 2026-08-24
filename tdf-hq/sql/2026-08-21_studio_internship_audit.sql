-- Internship studio-audit execution and internal feedback workflow.
-- Existing public feedback rows remain unchanged and readable.
\set ON_ERROR_STOP on

BEGIN;

ALTER TABLE intern_project
  ADD COLUMN IF NOT EXISTS activation_status TEXT NOT NULL DEFAULT 'active',
  ADD COLUMN IF NOT EXISTS activated_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS notifications_enabled BOOLEAN NOT NULL DEFAULT FALSE;

ALTER TABLE intern_project
  DROP CONSTRAINT IF EXISTS intern_project_activation_status_check;
ALTER TABLE intern_project
  ADD CONSTRAINT intern_project_activation_status_check
  CHECK (activation_status IN ('draft', 'active', 'archived'));

ALTER TABLE intern_task
  ADD COLUMN IF NOT EXISTS activation_status TEXT NOT NULL DEFAULT 'active',
  ADD COLUMN IF NOT EXISTS proposed_assignee BIGINT REFERENCES party(id) ON DELETE RESTRICT;

ALTER TABLE intern_task
  DROP CONSTRAINT IF EXISTS intern_task_activation_status_check;
ALTER TABLE intern_task
  ADD CONSTRAINT intern_task_activation_status_check
  CHECK (activation_status IN ('draft', 'active', 'archived'));

CREATE TABLE IF NOT EXISTS intern_audit_plan (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES intern_project(id) ON DELETE RESTRICT,
  task_id UUID NOT NULL REFERENCES intern_task(id) ON DELETE RESTRICT,
  environment TEXT NOT NULL CHECK (environment IN ('local', 'test', 'staging', 'production-read-only')),
  status TEXT NOT NULL DEFAULT 'draft' CHECK (status IN ('draft', 'active', 'completed', 'cancelled')),
  duration_days BIGINT NOT NULL DEFAULT 14 CHECK (duration_days BETWEEN 1 AND 90),
  expected_hours_min BIGINT NOT NULL DEFAULT 20 CHECK (expected_hours_min BETWEEN 1 AND 200),
  expected_hours_max BIGINT NOT NULL DEFAULT 30 CHECK (expected_hours_max BETWEEN 1 AND 200),
  midpoint_percent BIGINT NOT NULL DEFAULT 50 CHECK (midpoint_percent BETWEEN 1 AND 99),
  proposed_assignee BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  final_review_required BOOLEAN NOT NULL DEFAULT TRUE,
  completion_justification TEXT,
  completion_approved_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  completion_approved_at TIMESTAMPTZ,
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT intern_audit_plan_hours_check CHECK (expected_hours_max >= expected_hours_min),
  CONSTRAINT intern_audit_plan_task_unique UNIQUE (task_id)
);

CREATE INDEX IF NOT EXISTS intern_audit_plan_project_idx
  ON intern_audit_plan(project_id, status, created_at DESC);

CREATE TABLE IF NOT EXISTS intern_test_case (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  plan_id UUID NOT NULL REFERENCES intern_audit_plan(id) ON DELETE CASCADE,
  stable_id TEXT NOT NULL CHECK (stable_id ~ '^[A-Z][A-Z0-9-]{2,39}$'),
  module_name TEXT NOT NULL CHECK (length(btrim(module_name)) BETWEEN 1 AND 120),
  feature_name TEXT NOT NULL CHECK (length(btrim(feature_name)) BETWEEN 1 AND 160),
  user_role TEXT NOT NULL CHECK (length(btrim(user_role)) BETWEEN 1 AND 100),
  objective TEXT NOT NULL,
  business_purpose TEXT NOT NULL,
  preconditions TEXT NOT NULL,
  required_test_data TEXT NOT NULL,
  environment TEXT NOT NULL,
  platform TEXT NOT NULL,
  browser_or_device TEXT NOT NULL,
  language TEXT NOT NULL,
  detailed_steps TEXT NOT NULL,
  expected_result TEXT NOT NULL,
  expected_persisted_state TEXT NOT NULL,
  expected_side_effects TEXT NOT NULL,
  cleanup_instructions TEXT NOT NULL,
  criticality TEXT NOT NULL CHECK (criticality IN ('low', 'medium', 'high', 'critical')),
  evidence_requirement TEXT NOT NULL CHECK (evidence_requirement IN ('light', 'strong')),
  exploratory_charter TEXT,
  applicable BOOLEAN NOT NULL DEFAULT TRUE,
  sort_order BIGINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT intern_test_case_plan_stable_unique UNIQUE (plan_id, stable_id)
);

CREATE INDEX IF NOT EXISTS intern_test_case_plan_order_idx
  ON intern_test_case(plan_id, sort_order, stable_id);

CREATE TABLE IF NOT EXISTS intern_test_execution (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  test_case_id UUID NOT NULL REFERENCES intern_test_case(id) ON DELETE RESTRICT,
  execution_number BIGINT NOT NULL CHECK (execution_number >= 1),
  executor_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN (
    'pending', 'in_progress', 'passed', 'failed', 'blocked', 'not_applicable',
    'ready_for_retest', 'verified'
  )),
  actual_result TEXT,
  persisted_state_observed TEXT,
  side_effects_observed TEXT,
  blocker_reason TEXT,
  evidence_summary TEXT,
  started_at TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT intern_test_execution_case_number_unique UNIQUE (test_case_id, execution_number),
  CONSTRAINT intern_test_execution_blocker_reason_check
    CHECK (status <> 'blocked' OR length(btrim(COALESCE(blocker_reason, ''))) >= 5),
  CONSTRAINT intern_test_execution_completion_check
    CHECK (status IN ('pending', 'in_progress', 'ready_for_retest') OR completed_at IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS intern_test_execution_case_latest_idx
  ON intern_test_execution(test_case_id, execution_number DESC);

CREATE TABLE IF NOT EXISTS intern_daily_summary (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  task_id UUID NOT NULL REFERENCES intern_task(id) ON DELETE RESTRICT,
  author_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  work_date DATE NOT NULL,
  minutes_worked BIGINT NOT NULL CHECK (minutes_worked BETWEEN 1 AND 1440),
  modules_tested TEXT NOT NULL,
  cases_completed BIGINT NOT NULL CHECK (cases_completed >= 0),
  reports_created BIGINT NOT NULL CHECK (reports_created >= 0),
  blockers TEXT,
  next_step TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS intern_daily_summary_task_date_idx
  ON intern_daily_summary(task_id, work_date DESC, created_at DESC);

CREATE TABLE IF NOT EXISTS intern_final_summary (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  plan_id UUID NOT NULL REFERENCES intern_audit_plan(id) ON DELETE RESTRICT,
  author_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  generated_snapshot TEXT NOT NULL,
  conclusions TEXT,
  submitted_at TIMESTAMPTZ,
  approved_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  approved_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT intern_final_summary_plan_unique UNIQUE (plan_id)
);

CREATE TABLE IF NOT EXISTS internal_feedback_report (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  feedback_id UUID NOT NULL REFERENCES feedback(id) ON DELETE RESTRICT,
  report_type TEXT NOT NULL CHECK (report_type IN (
    'error', 'suggestion', 'idea', 'question', 'accessibility', 'permissions',
    'performance', 'content_translation'
  )),
  state TEXT NOT NULL DEFAULT 'draft' CHECK (state IN (
    'draft', 'submitted', 'received', 'needs_information', 'confirmed', 'prioritized',
    'in_progress', 'ready_for_retest', 'verified', 'closed', 'duplicate', 'discarded'
  )),
  module_name TEXT NOT NULL CHECK (length(btrim(module_name)) BETWEEN 1 AND 120),
  feature_name TEXT,
  environment TEXT NOT NULL CHECK (environment IN ('local', 'test', 'staging', 'production-read-only')),
  url_or_screen TEXT,
  platform TEXT NOT NULL,
  device TEXT,
  browser TEXT,
  language TEXT NOT NULL,
  account_role TEXT NOT NULL,
  reproduction_steps TEXT,
  expected_result TEXT,
  actual_result TEXT,
  frequency TEXT,
  proposed_severity_id UUID REFERENCES feedback_severity(id) ON DELETE RESTRICT,
  authoritative_severity_id UUID REFERENCES feedback_severity(id) ON DELETE RESTRICT,
  priority TEXT CHECK (priority IS NULL OR priority IN ('low', 'medium', 'high', 'urgent')),
  test_case_id UUID REFERENCES intern_test_case(id) ON DELETE RESTRICT,
  test_execution_id UUID REFERENCES intern_test_execution(id) ON DELETE RESTRICT,
  internship_project_id UUID REFERENCES intern_project(id) ON DELETE RESTRICT,
  internship_task_id UUID REFERENCES intern_task(id) ON DELETE RESTRICT,
  reporter_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  blocking BOOLEAN NOT NULL DEFAULT FALSE,
  assigned_to BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  duplicate_of UUID REFERENCES feedback(id) ON DELETE RESTRICT,
  resolution TEXT,
  retest_result TEXT,
  closure_reason TEXT,
  github_issue_url TEXT,
  video_links TEXT,
  submitted_at TIMESTAMPTZ,
  closed_at TIMESTAMPTZ,
  version BIGINT NOT NULL DEFAULT 1 CHECK (version >= 1),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT internal_feedback_report_feedback_unique UNIQUE (feedback_id),
  CONSTRAINT internal_feedback_report_submitted_at_check
    CHECK (state = 'draft' OR submitted_at IS NOT NULL),
  CONSTRAINT internal_feedback_report_duplicate_check
    CHECK (state <> 'duplicate' OR duplicate_of IS NOT NULL),
  CONSTRAINT internal_feedback_report_closed_check
    CHECK (state <> 'closed' OR (closed_at IS NOT NULL AND length(btrim(COALESCE(closure_reason, ''))) >= 3))
);

CREATE INDEX IF NOT EXISTS internal_feedback_report_owner_idx
  ON internal_feedback_report(reporter_party_id, updated_at DESC);
CREATE INDEX IF NOT EXISTS internal_feedback_report_admin_queue_idx
  ON internal_feedback_report(state, blocking DESC, priority, updated_at DESC);
CREATE INDEX IF NOT EXISTS internal_feedback_report_trace_idx
  ON internal_feedback_report(internship_task_id, test_case_id, test_execution_id);

CREATE TABLE IF NOT EXISTS internal_feedback_evidence (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  report_id UUID NOT NULL REFERENCES internal_feedback_report(id) ON DELETE CASCADE,
  uploaded_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  kind TEXT NOT NULL DEFAULT 'attachment' CHECK (kind IN ('attachment', 'video_link', 'retest')),
  original_file_name TEXT,
  storage_path TEXT,
  content_type TEXT,
  size_bytes BIGINT,
  external_url TEXT,
  caption TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT internal_feedback_evidence_source_check CHECK (
    (storage_path IS NOT NULL AND external_url IS NULL AND original_file_name IS NOT NULL
      AND content_type IS NOT NULL AND size_bytes BETWEEN 1 AND 10485760)
    OR (storage_path IS NULL AND external_url IS NOT NULL AND length(btrim(external_url)) BETWEEN 8 AND 2048)
  )
);

CREATE INDEX IF NOT EXISTS internal_feedback_evidence_report_idx
  ON internal_feedback_evidence(report_id, created_at);

CREATE TABLE IF NOT EXISTS internal_feedback_comment (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  report_id UUID NOT NULL REFERENCES internal_feedback_report(id) ON DELETE CASCADE,
  author_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  kind TEXT NOT NULL DEFAULT 'comment' CHECK (kind IN ('comment', 'information_request', 'information_response')),
  body TEXT NOT NULL CHECK (length(btrim(body)) BETWEEN 1 AND 5000),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS internal_feedback_comment_report_idx
  ON internal_feedback_comment(report_id, created_at);

CREATE TABLE IF NOT EXISTS internal_feedback_history (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  report_id UUID NOT NULL REFERENCES internal_feedback_report(id) ON DELETE CASCADE,
  actor_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  action TEXT NOT NULL,
  previous_state TEXT,
  new_state TEXT,
  metadata TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS internal_feedback_history_report_idx
  ON internal_feedback_history(report_id, created_at, id);

CREATE TABLE IF NOT EXISTS internal_feedback_retest (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  report_id UUID NOT NULL REFERENCES internal_feedback_report(id) ON DELETE CASCADE,
  execution_id UUID REFERENCES intern_test_execution(id) ON DELETE RESTRICT,
  tester_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  result TEXT NOT NULL CHECK (result IN ('passed', 'failed', 'blocked')),
  notes TEXT,
  evidence_summary TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS internal_feedback_retest_report_idx
  ON internal_feedback_retest(report_id, created_at DESC);

CREATE TABLE IF NOT EXISTS intern_audit_notification_outbox (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  recipient_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  report_id UUID REFERENCES internal_feedback_report(id) ON DELETE CASCADE,
  plan_id UUID REFERENCES intern_audit_plan(id) ON DELETE CASCADE,
  template_key TEXT NOT NULL,
  delivery_mode TEXT NOT NULL CHECK (delivery_mode IN ('immediate', 'digest')),
  test_transport BOOLEAN NOT NULL DEFAULT TRUE,
  payload TEXT NOT NULL,
  dispatched_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT intern_audit_notification_target_check CHECK (report_id IS NOT NULL OR plan_id IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS intern_audit_notification_pending_idx
  ON intern_audit_notification_outbox(delivery_mode, created_at)
  WHERE dispatched_at IS NULL;

CREATE OR REPLACE FUNCTION refresh_intern_audit_task_progress(requested_case_id UUID)
RETURNS VOID
LANGUAGE plpgsql
AS $$
DECLARE
  target_task_id UUID;
  total_cases BIGINT;
  executed_cases BIGINT;
BEGIN
  SELECT plan.task_id INTO target_task_id
  FROM intern_test_case test_case
  JOIN intern_audit_plan plan ON plan.id = test_case.plan_id
  WHERE test_case.id = requested_case_id;

  IF target_task_id IS NULL THEN
    RETURN;
  END IF;

  SELECT
    count(*) FILTER (WHERE test_case.applicable),
    count(*) FILTER (
      WHERE test_case.applicable
        AND latest.status IN ('passed', 'failed', 'blocked', 'not_applicable', 'verified')
    )
  INTO total_cases, executed_cases
  FROM intern_test_case test_case
  JOIN intern_audit_plan plan ON plan.id = test_case.plan_id
  LEFT JOIN LATERAL (
    SELECT execution.status
    FROM intern_test_execution execution
    WHERE execution.test_case_id = test_case.id
    ORDER BY execution.execution_number DESC
    LIMIT 1
  ) latest ON TRUE
  WHERE plan.task_id = target_task_id;

  UPDATE intern_task
  SET progress = CASE WHEN total_cases = 0 THEN 0 ELSE floor((executed_cases * 100.0) / total_cases)::BIGINT END,
      updated_at = NOW()
  WHERE id = target_task_id;
END $$;

CREATE OR REPLACE FUNCTION intern_test_execution_refresh_progress()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  PERFORM refresh_intern_audit_task_progress(NEW.test_case_id);
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_intern_test_execution_refresh_progress ON intern_test_execution;
CREATE TRIGGER trg_intern_test_execution_refresh_progress
  AFTER INSERT OR UPDATE OF status ON intern_test_execution
  FOR EACH ROW EXECUTE FUNCTION intern_test_execution_refresh_progress();

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
  WHERE task_id = NEW.id AND status IN ('draft', 'active')
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
          WHERE report.test_case_id = test_case.id
             OR report.test_execution_id = latest.execution_id
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
    AND report.blocking
    AND report.state NOT IN ('verified', 'closed', 'duplicate', 'discarded');

  SELECT CASE WHEN count(*) = 0 THEN 1 ELSE 0 END INTO missing_daily
  FROM intern_daily_summary summary
  WHERE summary.task_id = NEW.id;

  SELECT CASE WHEN count(*) FILTER (WHERE summary.submitted_at IS NOT NULL) = 0 THEN 1 ELSE 0 END
  INTO missing_final
  FROM intern_final_summary summary
  WHERE summary.plan_id = target_plan.id;

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

DROP TRIGGER IF EXISTS trg_enforce_intern_audit_completion ON intern_task;
CREATE TRIGGER trg_enforce_intern_audit_completion
  BEFORE UPDATE OF status ON intern_task
  FOR EACH ROW EXECUTE FUNCTION enforce_intern_audit_completion();

COMMIT;
