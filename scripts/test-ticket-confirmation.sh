#!/usr/bin/env bash
set -euo pipefail
repo_root=$(cd -- "$(dirname -- "$0")/.." && pwd)
: "${TICKET_CONFIRMATION_TEST_DSN:?Set the disposable tdf_ticket_confirmation_worker_test database}"
actual_database=$(psql "$TICKET_CONFIRMATION_TEST_DSN" -XAt -v ON_ERROR_STOP=1 -c 'SELECT current_database()')
if [[ "$actual_database" != tdf_ticket_confirmation_worker_test ]]; then
  echo 'Refusing fixtures outside the dedicated confirmation database' >&2
  exit 1
fi
psql "$TICKET_CONFIRMATION_TEST_DSN" -X -v ON_ERROR_STOP=1 \
  -f "$repo_root/tdf-hq/test/integration/ticket_admission_fixture.sql" \
  -c "ALTER TABLE event_ticket_checkout_runtime ADD COLUMN fulfillment_status TEXT NOT NULL DEFAULT 'issued';" \
  -f "$repo_root/tdf-hq/sql/2026-10-05_ticket_confirmation_delivery.sql" \
  -f "$repo_root/tdf-hq/test/integration/ticket_confirmation_worker_fixture.sql"
cd "$repo_root/tdf-hq"
stack exec -- runghc -Wall -isrc test/TicketConfirmationMain.hs
