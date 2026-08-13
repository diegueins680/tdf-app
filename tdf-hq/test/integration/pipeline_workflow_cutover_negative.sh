#!/usr/bin/env bash
set -euo pipefail

: "${DATABASE_URL:?Set DATABASE_URL to a disposable PostgreSQL database}"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
apply_script="${script_dir}/../../sql/2026-08-11_pipeline_workflow_cutover_apply.sql"
fixture_prefix="Pipeline negative fixture $$"

cleanup() {
  psql "${DATABASE_URL}" -v fixture_prefix="${fixture_prefix}" -X -q <<'SQL'
DELETE FROM pipeline_card WHERE title LIKE :'fixture_prefix' || '%';
SQL
}
trap cleanup EXIT

insert_fixture() {
  local case_name="$1"
  local legacy_stage="$2"
  local existing_state_code="$3"

  psql "${DATABASE_URL}" \
    -v fixture_title="${fixture_prefix} ${case_name}" \
    -v legacy_stage="${legacy_stage}" \
    -v existing_state_code="${existing_state_code}" \
    -X -q <<'SQL'
BEGIN;
ALTER TABLE pipeline_card DISABLE TRIGGER catalog_pipeline_card_integrity;
WITH service_value AS (
  SELECT id FROM service_offering WHERE code='mixing' AND active
), existing_state AS (
  SELECT state.id FROM workflow_state state
  JOIN workflow_definition workflow ON workflow.id=state.workflow_id
    AND workflow.code='pipeline-mixing' AND workflow.active
  WHERE state.active AND state.code=NULLIF(:'existing_state_code', '')
)
INSERT INTO pipeline_card (
  service_kind, service_offering_id, title, stage, workflow_state_id,
  sort_order, created_at, updated_at
)
SELECT 'Mixing', service_value.id, :'fixture_title', :'legacy_stage',
  existing_state.id, 0, now(), now()
FROM service_value LEFT JOIN existing_state ON TRUE;
ALTER TABLE pipeline_card ENABLE TRIGGER catalog_pipeline_card_integrity;
COMMIT;
SQL
}

assert_fixture_unchanged() {
  local case_name="$1"
  local legacy_stage="$2"
  local existing_state_code="$3"
  local run_code="$4"

  psql "${DATABASE_URL}" \
    -v fixture_title="${fixture_prefix} ${case_name}" \
    -v legacy_stage="${legacy_stage}" \
    -v existing_state_code="${existing_state_code}" \
    -v run_code="${run_code}" \
    -X -q <<'SQL'
SELECT count(*)=1 AS fixture_unchanged
FROM pipeline_card card
LEFT JOIN workflow_state state ON state.id=card.workflow_state_id
WHERE card.title=:'fixture_title'
  AND card.service_kind='Mixing'
  AND card.stage=:'legacy_stage'
  AND COALESCE(state.code, '')=:'existing_state_code'
\gset
SELECT count(*)=0 AS no_failed_run
FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision='negative-pipeline-v1' AND NOT dry_run
\gset
\if :fixture_unchanged
\else
  \echo 'Failed pipeline cutover changed its source fixture'
  \quit 1
\endif
\if :no_failed_run
\else
  \echo 'Failed pipeline cutover committed a run record'
  \quit 1
\endif
SQL
}

run_negative_case() {
  local case_name="$1"
  local legacy_stage="$2"
  local existing_state_code="$3"
  local safety_threshold="$4"
  local run_code="pipeline-workflow-negative-$$-${case_name}"

  insert_fixture "${case_name}" "${legacy_stage}" "${existing_state_code}"
  if psql "${DATABASE_URL}" \
    -v run_code="${run_code}" \
    -v candidate_revision='negative-pipeline-v1' \
    -v safety_threshold="${safety_threshold}" \
    -v batch_size=2 \
    -X -f "${apply_script}"; then
    echo "Expected ${case_name} pipeline cutover to fail" >&2
    exit 1
  fi
  assert_fixture_unchanged "${case_name}" "${legacy_stage}" "${existing_state_code}" "${run_code}"
  psql "${DATABASE_URL}" -v fixture_title="${fixture_prefix} ${case_name}" -X -q <<'SQL'
DELETE FROM pipeline_card WHERE title=:'fixture_title';
SQL
}

run_negative_case threshold brief '' 0
run_negative_case unresolved provider-mystery-stage '' 20
run_negative_case conflicting-identity approved brief 20

echo "pipeline workflow cutover negative checks passed"
