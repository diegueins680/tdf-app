#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${TDF_CATALOG_TEST_DATABASE_URL:-}" ]]; then
  echo 'TDF_CATALOG_TEST_DATABASE_URL is required' >&2
  exit 2
fi

case "$TDF_CATALOG_TEST_DATABASE_URL" in
  postgresql://*@127.0.0.1:*/*|postgresql://*@localhost:*/*) ;;
  *)
    echo 'Refusing to run catalog cutover integration test outside localhost' >&2
    exit 2
    ;;
esac

TDF_CATALOG_TEST_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TDF_CATALOG_TEST_REVISION="${TDF_CATALOG_TEST_REVISION:-catalog-local-integration-v1}"
TDF_CATALOG_TEST_THRESHOLD="${TDF_CATALOG_TEST_THRESHOLD:-10000}"

cutovers=(
  2026-08-07_catalog_backfill
  2026-08-07_label_project_notes_backfill
  2026-08-07_records_cms_backfill
  2026-08-11_instrument_input_cutover
  2026-08-11_feedback_catalog_cutover
  2026-08-11_pipeline_workflow_cutover
  2026-08-11_social_event_type_cutover
  2026-08-11_social_event_workflow_cutover
  2026-08-12_event_moment_reaction_cutover
  2026-08-12_content_reaction_cutover
  2026-08-12_creator_badge_cutover
  2026-08-12_ddex_reference_cutover
  2026-08-12_ddex_validation_reference_cutover
  2026-08-12_ddex_operational_cutover
)

run_cutover_file() {
  local stem="$1"
  local suffix="$2"
  psql "$TDF_CATALOG_TEST_DATABASE_URL" \
    -X -q -v ON_ERROR_STOP=1 \
    -v "run_code=${stem}-local-integration" \
    -v "candidate_revision=${TDF_CATALOG_TEST_REVISION}" \
    -v "safety_threshold=${TDF_CATALOG_TEST_THRESHOLD}" \
    -v batch_size=500 \
    -f "$TDF_CATALOG_TEST_ROOT/tdf-hq/sql/${stem}_${suffix}.sql" >/dev/null
}

for stem in "${cutovers[@]}"; do
  echo "dry-run ${stem}"
  run_cutover_file "$stem" dry_run
  echo "apply ${stem}"
  run_cutover_file "$stem" apply
done

for stem in "${cutovers[@]}"; do
  echo "idempotency ${stem}"
  run_cutover_file "$stem" apply
done

psql "$TDF_CATALOG_TEST_DATABASE_URL" -X -v ON_ERROR_STOP=1 -Atqc "
  SELECT CASE WHEN count(*)=13 THEN 'ok' ELSE 'unexpected-run-count:' || count(*) END
  FROM catalog_backfill_run
  WHERE candidate_revision='${TDF_CATALOG_TEST_REVISION}' AND NOT dry_run AND status='completed';
" | grep -qx ok
