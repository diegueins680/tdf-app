#!/bin/sh
set -eu

TDF_AUDIT_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_AUDIT_CONTAINER="tdf-studio-intern-audit-migration-$$"
TDF_AUDIT_DATABASE="tdf_studio_intern_audit_test"
TDF_AUDIT_USE_LOCAL_POSTGRES=${TDF_AUDIT_USE_LOCAL_POSTGRES:-0}
TDF_AUDIT_LOCAL_DATABASE_CREATED=0

cleanup() {
  if [ "$TDF_AUDIT_USE_LOCAL_POSTGRES" = "1" ]; then
    if [ "$TDF_AUDIT_LOCAL_DATABASE_CREATED" = "1" ]; then
      dropdb --if-exists "$TDF_AUDIT_DATABASE" >/dev/null 2>&1 || true
    fi
  else
    docker rm -f "$TDF_AUDIT_CONTAINER" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ "$TDF_AUDIT_USE_LOCAL_POSTGRES" = "1" ]; then
  if psql -d postgres -Atqc "SELECT 1 FROM pg_database WHERE datname = '$TDF_AUDIT_DATABASE'" | grep -q 1; then
    echo "Refusing to replace existing local database: $TDF_AUDIT_DATABASE" >&2
    exit 1
  fi
  createdb "$TDF_AUDIT_DATABASE"
  TDF_AUDIT_LOCAL_DATABASE_CREATED=1
else
  docker run --rm -d \
    --name "$TDF_AUDIT_CONTAINER" \
    -e POSTGRES_PASSWORD=studio-intern-audit-test \
    -e POSTGRES_DB="$TDF_AUDIT_DATABASE" \
    postgres:17-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_AUDIT_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_AUDIT_DATABASE" -Atqc 'SELECT 1' \
    >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 30 ]; then
      echo "Studio internship audit migration database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done
fi

psql_exec() {
  if [ "$TDF_AUDIT_USE_LOCAL_POSTGRES" = "1" ]; then
    PGOPTIONS='-c statement_timeout=10000' psql -X -v ON_ERROR_STOP=1 -d "$TDF_AUDIT_DATABASE" "$@"
  else
    docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_AUDIT_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_AUDIT_DATABASE" "$@"
  fi
}

apply_file() {
  psql_exec < "$TDF_AUDIT_ROOT/$1" >/dev/null
}

assert_equal() {
  actual=$1
  expected=$2
  label=$3
  if [ "$actual" != "$expected" ]; then
    echo "$label: expected '$expected', got '$actual'" >&2
    exit 1
  fi
}

psql_exec <<'SQL' >/dev/null
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE party (
  id BIGINT PRIMARY KEY,
  display_name TEXT NOT NULL,
  primary_email TEXT
);
CREATE TABLE intern_project (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  title TEXT NOT NULL,
  description TEXT,
  status TEXT NOT NULL DEFAULT 'active',
  start_at DATE,
  due_at DATE,
  created_by BIGINT NOT NULL REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE TABLE intern_task (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES intern_project(id),
  title TEXT NOT NULL,
  description TEXT,
  status TEXT NOT NULL DEFAULT 'todo',
  progress BIGINT NOT NULL DEFAULT 0,
  assigned_to BIGINT REFERENCES party(id),
  due_at DATE,
  created_by BIGINT NOT NULL REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE TABLE feedback_severity (
  id UUID PRIMARY KEY,
  code TEXT NOT NULL
);
CREATE TABLE feedback (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  title TEXT NOT NULL,
  description TEXT NOT NULL,
  created_by BIGINT REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
SQL

apply_file tdf-hq/sql/2026-08-21_studio_internship_audit.sql
apply_file tdf-hq/sql/2026-08-21_studio_internship_audit.sql

assert_equal "$(psql_exec -Atqc "SELECT count(*) FROM information_schema.tables WHERE table_name IN ('intern_audit_plan','intern_test_case','intern_test_execution','internal_feedback_report','internal_feedback_evidence','internal_feedback_history');")" "6" "normalized workflow tables"
assert_equal "$(psql_exec -Atqc "SELECT count(*) FROM information_schema.columns WHERE table_name='intern_task' AND column_name IN ('activation_status','proposed_assignee');")" "2" "draft task columns"

psql_exec <<'SQL' >/dev/null
INSERT INTO party(id,display_name,primary_email) VALUES
  (1,'Audit Manager','manager@persona.test'),
  (2,'Audit Intern','intern@persona.test'),
  (3,'Other Intern','other-intern@persona.test');
INSERT INTO feedback_severity(id,code)
VALUES ('10000000-0000-4000-8000-000000000001','high');

INSERT INTO intern_project(id,title,status,activation_status,notifications_enabled,created_by)
VALUES ('20000000-0000-4000-8000-000000000001','Studio audit','active','active',FALSE,1);
INSERT INTO intern_task(id,project_id,title,status,activation_status,assigned_to,created_by)
VALUES (
  '30000000-0000-4000-8000-000000000001',
  '20000000-0000-4000-8000-000000000001',
  'Principal audit assignment','todo','active',2,1
);
INSERT INTO intern_audit_plan(
  id,project_id,task_id,environment,status,proposed_assignee,created_by
) VALUES (
  '40000000-0000-4000-8000-000000000001',
  '20000000-0000-4000-8000-000000000001',
  '30000000-0000-4000-8000-000000000001',
  'staging','active',2,1
);
INSERT INTO intern_test_case(
  id,plan_id,stable_id,module_name,feature_name,user_role,objective,business_purpose,
  preconditions,required_test_data,environment,platform,browser_or_device,language,
  detailed_steps,expected_result,expected_persisted_state,expected_side_effects,
  cleanup_instructions,criticality,evidence_requirement,sort_order
) VALUES
  (
    '50000000-0000-4000-8000-000000000001','40000000-0000-4000-8000-000000000001',
    'STU-SCH-001','Scheduling','Critical booking','Reception','Validate booking','Protect schedule',
    'Staging','ROOM-AUDIT-A','staging','web','Chrome','es','Create and reload','One booking',
    'One row','Test outbox only','Delete fixture','critical','strong',1
  ),
  (
    '50000000-0000-4000-8000-000000000002','40000000-0000-4000-8000-000000000001',
    'STU-CRM-001','CRM','Customer validation','Reception','Validate customer','Protect customer data',
    'Staging','CUST-STUDIO-001','staging','web','Chrome','es','Create and reload','One customer',
    'One row','None','Delete fixture','high','strong',2
  );
INSERT INTO intern_test_execution(
  id,test_case_id,execution_number,executor_party_id,status,actual_result,
  evidence_summary,started_at,completed_at
) VALUES
  (
    '60000000-0000-4000-8000-000000000001','50000000-0000-4000-8000-000000000001',
    1,2,'passed','Booking persisted once','screenshot://STU-SCH-001',NOW(),NOW()
  ),
  (
    '60000000-0000-4000-8000-000000000002','50000000-0000-4000-8000-000000000002',
    1,2,'failed','Duplicate customer accepted','screenshot://STU-CRM-001',NOW(),NOW()
  );
SQL

assert_equal "$(psql_exec -Atqc "SELECT progress FROM intern_task WHERE id='30000000-0000-4000-8000-000000000001';")" "100" "progress from latest terminal executions"

if psql_exec -c "UPDATE intern_task SET status='done' WHERE id='30000000-0000-4000-8000-000000000001';" >/dev/null 2>&1; then
  echo "Completion accepted a failed case without a linked report" >&2
  exit 1
fi

psql_exec <<'SQL' >/dev/null
INSERT INTO feedback(id,title,description,created_by)
VALUES ('70000000-0000-4000-8000-000000000001','Duplicate customer','Steps and observed result',2);
INSERT INTO internal_feedback_report(
  id,feedback_id,report_type,state,module_name,feature_name,environment,platform,
  language,account_role,proposed_severity_id,test_case_id,test_execution_id,
  internship_project_id,internship_task_id,reporter_party_id,blocking,submitted_at
) VALUES (
  '80000000-0000-4000-8000-000000000001','70000000-0000-4000-8000-000000000001',
  'error','draft','CRM','Customer validation','staging','web','es','Intern',
  '10000000-0000-4000-8000-000000000001','50000000-0000-4000-8000-000000000002',
  '60000000-0000-4000-8000-000000000002','20000000-0000-4000-8000-000000000001',
  '30000000-0000-4000-8000-000000000001',2,FALSE,NULL
);
INSERT INTO internal_feedback_evidence(
  id,report_id,uploaded_by,kind,external_url,caption
) VALUES (
  '90000000-0000-4000-8000-000000000001','80000000-0000-4000-8000-000000000001',
  2,'external_link','https://evidence.invalid/STU-CRM-001','Safe fictional evidence'
);
INSERT INTO intern_daily_summary(
  id,task_id,author_party_id,work_date,minutes_worked,modules_tested,cases_completed,
  reports_created,next_step
) VALUES (
  'a0000000-0000-4000-8000-000000000001','30000000-0000-4000-8000-000000000001',
  2,CURRENT_DATE,120,'Scheduling, CRM',2,1,'Retest customer validation'
);
INSERT INTO intern_final_summary(
  id,plan_id,author_party_id,generated_snapshot,conclusions,submitted_at
) VALUES (
  'b0000000-0000-4000-8000-000000000001','40000000-0000-4000-8000-000000000001',
  2,'{}','Prioritize duplicate prevention',NOW()
);
SQL

psql_exec -c "UPDATE intern_audit_plan SET status='completed' WHERE id='40000000-0000-4000-8000-000000000001';" >/dev/null
if psql_exec -c "UPDATE intern_task SET status='done' WHERE id='30000000-0000-4000-8000-000000000001';" >/dev/null 2>&1; then
  echo "Completion accepted a linked report that was still a draft" >&2
  exit 1
fi
psql_exec -c "UPDATE internal_feedback_report SET state='received', submitted_at=NOW(), updated_at=NOW() WHERE id='80000000-0000-4000-8000-000000000001'; UPDATE intern_final_summary SET submitted_at=NOW() + INTERVAL '1 second' WHERE id='b0000000-0000-4000-8000-000000000001'; UPDATE intern_task SET status='done' WHERE id='30000000-0000-4000-8000-000000000001';" >/dev/null

assert_equal "$(psql_exec -Atqc "SELECT status FROM intern_task WHERE id='30000000-0000-4000-8000-000000000001';")" "done" "qualified completion"
assert_equal "$(psql_exec -Atqc "SELECT kind FROM internal_feedback_evidence WHERE id='90000000-0000-4000-8000-000000000001';")" "external_link" "external evidence kind"

psql_exec <<'SQL' >/dev/null
UPDATE intern_task SET status='todo' WHERE id='30000000-0000-4000-8000-000000000001';
UPDATE intern_audit_plan SET status='active' WHERE id='40000000-0000-4000-8000-000000000001';
UPDATE internal_feedback_report
SET state='ready_for_retest', blocking=FALSE, updated_at=NOW()
WHERE id='80000000-0000-4000-8000-000000000001';
UPDATE intern_final_summary
SET submitted_at=NOW() + INTERVAL '1 second'
WHERE id='b0000000-0000-4000-8000-000000000001';
SQL
if psql_exec -c "UPDATE intern_task SET status='done' WHERE id='30000000-0000-4000-8000-000000000001';" >/dev/null 2>&1; then
  echo "Completion accepted a pending non-blocking retest" >&2
  exit 1
fi

psql_exec <<'SQL' >/dev/null
UPDATE internal_feedback_report
SET blocking=TRUE, state='confirmed'
WHERE id='80000000-0000-4000-8000-000000000001';
SQL
if psql_exec -c "UPDATE intern_task SET status='done' WHERE id='30000000-0000-4000-8000-000000000001';" >/dev/null 2>&1; then
  echo "Completion accepted an unresolved blocking report" >&2
  exit 1
fi
psql_exec -c "UPDATE internal_feedback_report SET state='closed', closure_reason='Verified in retest', closed_at=NOW(), updated_at=NOW() WHERE id='80000000-0000-4000-8000-000000000001'; UPDATE intern_final_summary SET submitted_at=NOW() + INTERVAL '1 second' WHERE id='b0000000-0000-4000-8000-000000000001'; UPDATE intern_task SET status='done' WHERE id='30000000-0000-4000-8000-000000000001';" >/dev/null

psql_exec <<'SQL' >/dev/null
UPDATE intern_task SET status='todo' WHERE id='30000000-0000-4000-8000-000000000001';
UPDATE intern_final_summary
SET submitted_at=NOW() - INTERVAL '2 minutes'
WHERE id='b0000000-0000-4000-8000-000000000001';
UPDATE intern_test_execution
SET updated_at=NOW()
WHERE id='60000000-0000-4000-8000-000000000001';
SQL
if psql_exec -c "UPDATE intern_task SET status='done' WHERE id='30000000-0000-4000-8000-000000000001';" >/dev/null 2>&1; then
  echo "Completion accepted a stale submitted final summary" >&2
  exit 1
fi
psql_exec -c "UPDATE intern_final_summary SET submitted_at=NOW() + INTERVAL '1 second' WHERE id='b0000000-0000-4000-8000-000000000001'; UPDATE intern_task SET status='done' WHERE id='30000000-0000-4000-8000-000000000001';" >/dev/null

psql_exec <<'SQL' >/dev/null
INSERT INTO intern_test_execution(
  id,test_case_id,execution_number,executor_party_id,status,actual_result,
  evidence_summary,started_at,completed_at
) VALUES (
  '60000000-0000-4000-8000-000000000003','50000000-0000-4000-8000-000000000002',
  2,2,'verified','Duplicate rejected on retest','screenshot://STU-CRM-001-retest',NOW(),NOW()
);
SQL
assert_equal "$(psql_exec -Atqc "SELECT count(*) FROM intern_test_execution WHERE test_case_id='50000000-0000-4000-8000-000000000002';")" "2" "retest preserves execution history"

apply_file tdf-hq/sql/2026-08-21_studio_internship_audit_rollback.sql
assert_equal "$(psql_exec -Atqc "SELECT count(*) FROM information_schema.tables WHERE table_name='internal_feedback_report';")" "0" "rollback removes normalized internal workflow"
assert_equal "$(psql_exec -Atqc "SELECT count(*) FROM feedback WHERE id='70000000-0000-4000-8000-000000000001';")" "1" "rollback preserves legacy feedback"

apply_file tdf-hq/sql/2026-08-21_studio_internship_audit.sql
apply_file tdf-hq/sql/2026-08-21_studio_internship_audit.sql

echo "Studio internship audit migration passed retry, progress, traceability, completion-gate, blocker, history, compatibility, and rollback checks"
