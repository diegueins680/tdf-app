#!/usr/bin/env bash
set -euo pipefail

: "${DATABASE_URL:?Set DATABASE_URL to a disposable PostgreSQL database}"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
apply_script="${script_dir}/../../sql/2026-08-11_social_event_workflow_cutover_apply.sql"
fixture_prefix="Workflow negative fixture $$"

cleanup() {
  psql "${DATABASE_URL}" -v fixture_prefix="${fixture_prefix}" -X -q <<'SQL'
DELETE FROM social_event WHERE title LIKE :'fixture_prefix' || '%';
SQL
}
trap cleanup EXIT

insert_fixture() {
  local case_name="$1"
  local legacy_status="$2"
  local existing_state_code="$3"

  psql "${DATABASE_URL}" \
    -v fixture_title="${fixture_prefix} ${case_name}" \
    -v legacy_status="${legacy_status}" \
    -v existing_state_code="${existing_state_code}" \
    -X -q <<'SQL'
BEGIN;
ALTER TABLE social_event DISABLE TRIGGER social_event_workflow_state_integrity;
WITH event_type_value AS (
  SELECT item.id
  FROM event_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
    AND catalog.code='event-types' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id
    AND state.code='published' AND state.active
  WHERE item.active AND item.deprecated_at IS NULL
  ORDER BY item.sort_order, item.id
  LIMIT 1
), existing_state AS (
  SELECT state.id
  FROM workflow_state state
  JOIN workflow_definition workflow ON workflow.id=state.workflow_id
    AND workflow.code='social-event-lifecycle' AND workflow.active
  WHERE state.active AND state.code=NULLIF(:'existing_state_code', '')
)
INSERT INTO social_event (
  title, event_type_id, workflow_state_id, start_time, end_time,
  metadata, created_at, updated_at
)
SELECT :'fixture_title', event_type_value.id, existing_state.id,
  now()+interval '5 days', now()+interval '5 days 2 hours',
  jsonb_build_object('eventStatus', :'legacy_status', 'negativeFixture', :'fixture_title')::text,
  now(), now()
FROM event_type_value
LEFT JOIN existing_state ON TRUE;
ALTER TABLE social_event ENABLE TRIGGER social_event_workflow_state_integrity;
COMMIT;
SQL
}

assert_fixture_unchanged() {
  local case_name="$1"
  local legacy_status="$2"
  local existing_state_code="$3"
  local run_code="$4"

  psql "${DATABASE_URL}" \
    -v fixture_title="${fixture_prefix} ${case_name}" \
    -v legacy_status="${legacy_status}" \
    -v existing_state_code="${existing_state_code}" \
    -v run_code="${run_code}" \
    -X -q <<'SQL'
SELECT count(*)=1 AS fixture_unchanged
FROM social_event event
LEFT JOIN workflow_state state ON state.id=event.workflow_state_id
WHERE event.title=:'fixture_title'
  AND event.metadata::jsonb ->> 'eventStatus'=:'legacy_status'
  AND COALESCE(state.code, '')=:'existing_state_code'
\gset
SELECT count(*)=0 AS no_failed_run
FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision='negative-integration-v1' AND NOT dry_run
\gset
\if :fixture_unchanged
\else
  \echo 'Failed cutover changed its source fixture'
  \quit 1
\endif
\if :no_failed_run
\else
  \echo 'Failed cutover committed a run record'
  \quit 1
\endif
SQL
}

run_negative_case() {
  local case_name="$1"
  local legacy_status="$2"
  local existing_state_code="$3"
  local safety_threshold="$4"
  local run_code="social-event-workflow-negative-$$-${case_name}"

  insert_fixture "${case_name}" "${legacy_status}" "${existing_state_code}"
  if psql "${DATABASE_URL}" \
    -v run_code="${run_code}" \
    -v candidate_revision='negative-integration-v1' \
    -v safety_threshold="${safety_threshold}" \
    -v batch_size=2 \
    -X -f "${apply_script}"; then
    echo "Expected ${case_name} cutover to fail" >&2
    exit 1
  fi
  assert_fixture_unchanged "${case_name}" "${legacy_status}" "${existing_state_code}" "${run_code}"
  psql "${DATABASE_URL}" -v fixture_title="${fixture_prefix} ${case_name}" -X -q <<'SQL'
DELETE FROM social_event WHERE title=:'fixture_title';
SQL
}

run_negative_case threshold planning '' 0
run_negative_case unresolved 'provider-mystery-status' '' 20
run_negative_case conflicting-identity announced planning 20

echo "social-event workflow cutover negative checks passed"
