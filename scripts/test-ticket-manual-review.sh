#!/usr/bin/env bash
# EVT-TICKET-MANUAL-001: run the staff bank-transfer review transaction against
# the production schema snapshot plus the production migration batch. The
# database must be empty, disposable and named tdf_ticket_manual_review_test.
set -euo pipefail
repo_root=$(cd -- "$(dirname -- "$0")/.." && pwd)
: "${TICKET_MANUAL_REVIEW_TEST_DSN:?Set the connection for the disposable tdf_ticket_manual_review_test database}"
test -z "${PGHOSTADDR:-}${PGSERVICE:-}${PGSERVICEFILE:-}" || {
  echo 'Unset libpq routing overrides before manual review fixture setup' >&2; exit 1;
}
node --input-type=module - "$TICKET_MANUAL_REVIEW_TEST_DSN" "$repo_root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import assert from 'node:assert/strict';
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
const url = disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
assert.equal(url.pathname, '/tdf_ticket_manual_review_test', 'Dedicated manual review database required');
assert.ok(/^(postgresql|postgres):\/\/(127\.0\.0\.1|localhost)(:5432)?\/tdf_ticket_manual_review_test$/.test(process.argv[2])
  || (process.env.CI === 'true' && process.argv[2] === 'postgresql://postgres:postgres@postgres:5432/tdf_ticket_manual_review_test'),
'Use the exact local or CI URL supported by the direct ticket harness');
JS
actual_database=$(psql "$TICKET_MANUAL_REVIEW_TEST_DSN" -XAt -v ON_ERROR_STOP=1 -c 'SELECT current_database()')
if [[ "$actual_database" != tdf_ticket_manual_review_test ]]; then
  echo 'Refusing to load fixtures outside tdf_ticket_manual_review_test' >&2
  exit 1
fi
existing_tables=$(psql "$TICKET_MANUAL_REVIEW_TEST_DSN" -XAt -v ON_ERROR_STOP=1 -c \
  "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p')")
if [[ "$existing_tables" != 0 ]]; then
  echo "Manual review harness requires an empty database; found $existing_tables tables" >&2
  exit 1
fi
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/tdf-manual-review.XXXXXX")
trap 'rm -rf -- "$work_dir"' EXIT
migration_sql="$work_dir/production-migrations.sql"
SOURCE_COMMIT="${GITHUB_SHA:-0000000000000000000000000000000000000000}" \
  node "$repo_root/scripts/render-production-migration-batch.mjs" > "$migration_sql"
# One session per file: the pg_dump snapshot clears search_path for its session.
for sql_file in \
  "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" \
  "$repo_root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" \
  "$migration_sql"; do
  psql "$TICKET_MANUAL_REVIEW_TEST_DSN" -X -q -v ON_ERROR_STOP=1 -f "$sql_file" >/dev/null
done
psql "$TICKET_MANUAL_REVIEW_TEST_DSN" -X -q -v ON_ERROR_STOP=1 \
  -f "$repo_root/tdf-hq/test/integration/ticket_manual_review_fixture.sql" >/dev/null
# SYS-MONEY-003 on the same production schema (rolled back, no fixture rows).
psql "$TICKET_MANUAL_REVIEW_TEST_DSN" -X -q -v ON_ERROR_STOP=1 \
  -f "$repo_root/tdf-hq/test/integration/ledger_posting_balance.sql" >/dev/null
cd "$repo_root/tdf-hq"
# Compiled, not runghc: the review module's dependency closure exceeds the
# bytecode interpreter's breakpoint table in GHC 9.10.
stack ghc -- -O0 -Wall -threaded -isrc -itest -outputdir "$work_dir/build" \
  -o "$work_dir/ticket-manual-review" test/TicketManualReviewMain.hs >/dev/null
"$work_dir/ticket-manual-review"
