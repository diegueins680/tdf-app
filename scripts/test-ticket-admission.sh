#!/usr/bin/env bash
set -euo pipefail
repo_root=$(cd -- "$(dirname -- "$0")/.." && pwd)
: "${TICKET_ADMISSION_TEST_DSN:?Set the connection for the disposable tdf_ticket_admission_test database}"
node --input-type=module - "$TICKET_ADMISSION_TEST_DSN" "$repo_root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import assert from 'node:assert/strict';
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
const url = disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
assert.equal(url.pathname, '/tdf_ticket_admission_test', 'Dedicated ticket fixture database required');
assert.ok(/^(postgresql|postgres):\/\/(127\.0\.0\.1|localhost)(:5432)?\/tdf_ticket_admission_test$/.test(process.argv[2])
  || (process.env.CI === 'true' && process.argv[2] === 'postgresql://postgres:postgres@postgres:5432/tdf_ticket_admission_test'),
'Use the exact local or CI URL supported by the direct ticket harness');
JS
actual_database=$(psql "$TICKET_ADMISSION_TEST_DSN" -XAt -v ON_ERROR_STOP=1 -c 'SELECT current_database()')
if [[ "$actual_database" != tdf_ticket_admission_test ]]; then
  echo 'Refusing to load fixtures outside tdf_ticket_admission_test' >&2
  exit 1
fi
psql "$TICKET_ADMISSION_TEST_DSN" -X -v ON_ERROR_STOP=1 \
  -f "$repo_root/tdf-hq/test/integration/ticket_admission_fixture.sql" \
  -f "$repo_root/tdf-hq/sql/2026-10-05_ticket_admission_audit.sql" \
  -f "$repo_root/tdf-hq/sql/2026-10-05_ticket_admission_audit.sql" \
  -f "$repo_root/tdf-hq/sql/2026-10-05_ticket_transfer_deadline.sql" \
  -f "$repo_root/tdf-hq/sql/2026-10-05_ticket_transfer_deadline.sql"
cd "$repo_root/tdf-hq"
stack exec -- runghc -Wall -itest test/TicketDatabaseRoutingMain.hs
stack exec -- runghc -Wall -isrc -itest test/TicketAdmissionMain.hs
