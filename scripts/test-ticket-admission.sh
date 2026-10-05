#!/usr/bin/env bash
set -euo pipefail
repo_root=$(cd -- "$(dirname -- "$0")/.." && pwd)
: "${TICKET_ADMISSION_TEST_DSN:?Set the connection for the disposable tdf_ticket_admission_test database}"
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
stack exec -- runghc -Wall -isrc test/TicketAdmissionMain.hs
