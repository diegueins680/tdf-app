#!/usr/bin/env bash
set -euo pipefail

diagnostic=false
case "${1:-}" in
  '') test "$#" = 0 || exit 2 ;;
  --diagnostic-missing-merch-prerequisite)
    test "$#" = 1 || exit 2
    diagnostic=true
    echo 'DIAGNOSTIC ONLY: a missing prerequisite may be supplied locally; this is NOT a passing release rehearsal.'
    ;;
  *) echo 'Unknown rehearsal option' >&2; exit 2 ;;
esac

# No caller-supplied DSN, server, port or image: all SQL stays inside this disposable container.
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_container_id=''
cleanup() {
  if [ -n "$test_container_id" ]; then
    docker rm -f "$test_container_id" >/dev/null
  fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
test_container_id=$(docker run --rm -d --network none \
  --label tdf.test=event-operations-schema-rehearsal \
  -e POSTGRES_PASSWORD=event-schema-test-only \
  -e POSTGRES_DB=tdf_event_schema_test pgvector/pgvector:pg17)
psql_exec() {
  docker exec -i -e PGOPTIONS='-c client_min_messages=warning' "$test_container_id" psql -X -v ON_ERROR_STOP=1 \
    -U postgres -d tdf_event_schema_test "$@"
}
attempt=0
until psql_exec -h 127.0.0.1 -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo 'Disposable schema rehearsal database did not become ready' >&2
    exit 1
  fi
  sleep 1
done
apply_sql() {
  echo "Schema rehearsal: $1"
  psql_exec < "$repo_root/$1" >/dev/null
}
apply_event_migrations() {
  apply_sql tdf-hq/sql/2026-09-14_event_operations_foundation.sql
  apply_sql tdf-hq/sql/2026-09-14_event_operations_api.sql
  apply_sql tdf-hq/sql/2026-09-14_event_task_commit.sql
}

test "$(psql_exec -qAtc "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p')")" = 0
apply_sql scripts/__tests__/fixtures/production-schema-20260814.sql
apply_sql scripts/__tests__/fixtures/catalog-production-source-fixture.sql
echo 'Schema rehearsal: authoritative migration batch (no application startup)'
apply_manifest() {
  SOURCE_COMMIT=0000000000000000000000000000000000000000 \
    node "$repo_root/scripts/render-production-migration-batch.mjs" | psql_exec >/dev/null
}
if apply_manifest; then
  if [ "$diagnostic" = true ]; then
    echo 'The authoritative baseline passed; the diagnostic workaround is no longer applicable.' >&2
    exit 2
  fi
else
  baseline_status=$?
  echo 'Authoritative schema baseline FAILED before event migrations.' >&2
  if [ "$diagnostic" = false ]; then exit "$baseline_status"; fi
  test "$(psql_exec -qAtc "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relname IN ('merch_store','merch_product','merch_order','merch_order_line','merch_fulfillment_event','merch_shipment')")" = 0
  echo 'DIAGNOSTIC ONLY: applying the existing omitted storefront prerequisite inside the disposable database.'
  apply_sql tdf-hq/sql/2026-09-07_artist_merch_storefronts.sql
  if apply_manifest; then
    echo 'Supplemented baseline passed its embedded schema contract.'
  else
    echo 'DIAGNOSTIC ONLY: supplemented batch still FAILED; continue only if every manifest entry was recorded.' >&2
  fi
fi
expected_migrations=$(node -p "require('$repo_root/scripts/production-migrations.json').migrations.length")
applied_migrations=$(psql_exec -qAtc 'SELECT count(*) FROM public.tdf_schema_migration')
if [ "$applied_migrations" != "$expected_migrations" ]; then
  echo "Incomplete authoritative ledger: $applied_migrations of $expected_migrations entries; event SQL will not run." >&2
  psql_exec -qAtc 'SELECT migration_id FROM public.tdf_schema_migration ORDER BY migration_id' |
    node --input-type=module -e '
      import { readFileSync } from "node:fs";
      const expected = JSON.parse(readFileSync(process.argv[1], "utf8")).migrations.map(row => row.id);
      const actual = new Set(readFileSync(0, "utf8").trim().split("\n"));
      console.error("Missing ledger IDs:", expected.filter(id => !actual.has(id)).join(", "));
      console.error("Unexpected ledger IDs:", [...actual].filter(id => !expected.includes(id)).join(", "));
    ' "$repo_root/scripts/production-migrations.json"
  exit 1
fi
if node "$repo_root/scripts/render-production-schema-verification.mjs" | psql_exec >/dev/null; then
  echo 'Authoritative full-schema contract passed before event migrations.'
else
  contract_status=$?
  if [ "$diagnostic" = false ]; then exit "$contract_status"; fi
  echo 'DIAGNOSTIC ONLY: full-schema contract FAILED; inspecting event checks separately. Final contract remains mandatory.' >&2
fi

ledger_snapshot=$(psql_exec -qAtc 'SELECT jsonb_agg(to_jsonb(t) ORDER BY migration_id) FROM public.tdf_schema_migration t')
echo 'Schema rehearsal: retry authoritative batch without rewriting ledger evidence'
apply_manifest
test "$(psql_exec -qAtc 'SELECT jsonb_agg(to_jsonb(t) ORDER BY migration_id) FROM public.tdf_schema_migration t')" = "$ledger_snapshot"

apply_sql tdf-hq/test/integration/event_operations_schema_seed.sql
apply_event_migrations
apply_event_migrations
apply_sql tdf-hq/test/integration/event_operations_schema_assertions.sql
apply_sql tdf-hq/sql/2026-09-14_event_task_commit_rollback.sql
apply_sql tdf-hq/sql/2026-09-14_event_operations_api_rollback.sql
apply_sql tdf-hq/sql/2026-09-14_event_operations_foundation_rollback.sql
apply_sql tdf-hq/test/integration/event_operations_schema_rollback_assertions.sql
apply_event_migrations
apply_sql tdf-hq/test/integration/event_operations_schema_reapply_assertions.sql
echo 'Event-specific SQL assertions reached completion; final full-schema verification follows.'
node "$repo_root/scripts/render-production-schema-verification.mjs" | psql_exec >/dev/null
test "$(psql_exec -qAtc 'SELECT count(*) FROM public.tdf_schema_migration')" = "$expected_migrations"
if [ "$diagnostic" = true ]; then
  echo 'DIAGNOSTIC ONLY: event checks passed on a supplemented baseline. The authoritative rehearsal remains BLOCKED.'
else
  echo 'Event operations complete-schema rehearsal passed; production manifest and activation unchanged.'
fi
