#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_database="tdf_ticket_payment_gate_test_$$_test"
export PGHOST="${PGHOST:-127.0.0.1}"
export TDF_TICKET_PAYMENT_GATE_TEST_DB="postgresql://$PGHOST/$test_database"
node --input-type=module - "$TDF_TICKET_PAYMENT_GATE_TEST_DB" "$repo_root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
createdb "$test_database"
trap cleanup EXIT INT TERM
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
node "$repo_root/scripts/render-production-migration-batch.mjs" | psql -X -v ON_ERROR_STOP=1 -d "$test_database" >/dev/null
# Synthetic, test-only values: the handlers refuse to run without a lookup secret
# and an operation encryption key, and no real provider is contacted.
export COMMERCE_CHECKOUT_ENV=sandbox
export COMMERCE_LOOKUP_TOKEN_SECRET=synthetic-ticket-payment-gate-lookup-secret-000000
export COMMERCE_EVENT_ENCRYPTION_KEY=0000000000000000000000000000000000000000000000000000000000000000
export COMMERCE_BANK_TRANSFER_INSTRUCTIONS='Synthetic test account; no transfer is ever made'
cd "$repo_root/tdf-hq"
stack test tdf-hq:test:tdf-hq-test --jobs 1 --test-arguments='--match ticket-payment-gate-postgresql'
