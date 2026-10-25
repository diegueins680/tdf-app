#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
. "$repo_root/scripts/lib/postgres-test-database.sh"
tdf_test_db_init tdf_event_memory_source_moderation_test
batch=$(mktemp)
cleanup_source() { rm -f "$batch"; tdf_test_db_cleanup; }
trap cleanup_source EXIT INT TERM
tdf_test_psql -f "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
tdf_test_psql -f "$repo_root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
node "$repo_root/scripts/render-production-migration-batch.mjs" > "$batch"
tdf_test_psql -f "$batch" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_source_moderation.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation_rollback.sql" >/dev/null
tdf_test_psql -qAtc "SELECT bool_and(pg_get_functiondef(signature::regprocedure)=definition) FROM event_memory_moderation_authority" | grep -qx t
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_source_moderation.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_source_moderation.sql" >/dev/null
printf '%s\n' 'Source moderation: full-schema, replay, rollback/reapply and policy assertions passed'
