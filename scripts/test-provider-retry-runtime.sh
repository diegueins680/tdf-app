#!/bin/sh
set -eu

TDF_PROVIDER_RETRY_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_PROVIDER_RETRY_CONTAINER=""
TDF_PROVIDER_RETRY_URL=${TDF_PROVIDER_RETRY_DATABASE_URL:-}

validate_runtime_url() {
  node --input-type=module - "$TDF_PROVIDER_RETRY_URL" "$TDF_PROVIDER_RETRY_ROOT/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
}
test -z "${PGHOSTADDR:-}${PGSERVICE:-}${PGSERVICEFILE:-}" || {
  echo 'Unset libpq routing overrides before provider retry fixture setup' >&2; exit 1;
}

cleanup() {
  if [ -n "$TDF_PROVIDER_RETRY_CONTAINER" ]; then
    docker rm -f "$TDF_PROVIDER_RETRY_CONTAINER" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ -z "$TDF_PROVIDER_RETRY_URL" ]; then
  TDF_PROVIDER_RETRY_CONTAINER="tdf-provider-retry-test-$$"
  docker run --rm -d --name "$TDF_PROVIDER_RETRY_CONTAINER" -p 127.0.0.1::5432 \
    -e POSTGRES_HOST_AUTH_METHOD=trust -e POSTGRES_DB=tdf_provider_retry_test \
    postgres:16-alpine >/dev/null
  attempt=0
  until docker exec "$TDF_PROVIDER_RETRY_CONTAINER" pg_isready -U postgres \
      -d tdf_provider_retry_test >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 45 ]; then
      echo "Provider retry test database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done
  published_address=$(docker port "$TDF_PROVIDER_RETRY_CONTAINER" 5432/tcp | head -1)
  published_port=${published_address##*:}
  TDF_PROVIDER_RETRY_URL="postgresql://postgres@127.0.0.1:$published_port/tdf_provider_retry_test"
fi

# Validate the actual target before any schema write; never clear an existing DB.
validate_runtime_url
attempt=0
until psql "$TDF_PROVIDER_RETRY_URL" -X -qAt -v ON_ERROR_STOP=1 -c 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Provider retry test database connection is unavailable" >&2
    exit 1
  fi
  sleep 1
done
database_name=$(psql "$TDF_PROVIDER_RETRY_URL" -X -qAt -v ON_ERROR_STOP=1 -c 'SELECT current_database()')
if [ "$database_name" != "tdf_provider_retry_test" ]; then
  echo "Provider retry tests require the disposable database tdf_provider_retry_test" >&2
  exit 1
fi
table_count=$(psql "$TDF_PROVIDER_RETRY_URL" -X -qAt -v ON_ERROR_STOP=1 -c "SELECT count(*) FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname NOT IN ('pg_catalog','information_schema') AND n.nspname !~ '^pg_toast' AND c.relkind IN ('r','p','v','m')")
if [ "$table_count" != "0" ]; then
  echo "Provider retry test database must be empty; no existing data was changed" >&2
  exit 1
fi
# Every provider/refund fixture now runs against the complete reviewed schema.
# No reduced domain projections or disabled triggers are permitted in this gate.
sh "$TDF_PROVIDER_RETRY_ROOT/scripts/load-payment-test-schema.sh" "$TDF_PROVIDER_RETRY_URL"
psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
  -f "$TDF_PROVIDER_RETRY_ROOT/scripts/__tests__/fixtures/provider-domain-fixtures.sql" >/dev/null

TDF_PROVIDER_RETRY_DATABASE_URL="$TDF_PROVIDER_RETRY_URL" \
  python3 "$TDF_PROVIDER_RETRY_ROOT/scripts/test-provider-domain-binding.py"

# Exercise repeat-apply and empty rollback with exact operator-seed retention
# before test data is inserted. Never roll back a used queue or financial history.
psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
  -f "$TDF_PROVIDER_RETRY_ROOT/tdf-hq/sql/2026-09-15_provider_query_recovery.sql" >/dev/null
psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 -c \
  "UPDATE revenue_feature_flag SET reason='synthetic operator policy' WHERE flag_key='checkout.provider_query_recovery' AND environment='sandbox'" >/dev/null
psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
  -f "$TDF_PROVIDER_RETRY_ROOT/tdf-hq/sql/2026-09-15_provider_query_recovery_rollback.sql" >/dev/null
preserved=$(psql "$TDF_PROVIDER_RETRY_URL" -X -qAt -v ON_ERROR_STOP=1 -c \
  "SELECT count(*) FROM revenue_feature_flag WHERE flag_key='checkout.provider_query_recovery' AND environment='sandbox' AND reason='synthetic operator policy' AND enabled=false")
if [ "$preserved" != "1" ]; then
  echo "Provider query rollback lost an operator-owned flag" >&2
  exit 1
fi
psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
  -f "$TDF_PROVIDER_RETRY_ROOT/tdf-hq/sql/2026-09-15_provider_query_recovery.sql" >/dev/null

# Match quality-backend.sh optimization settings so its compiled artifacts are
# reused; --fast here invalidates both executable and test builds after that gate.
cd "$TDF_PROVIDER_RETRY_ROOT/tdf-hq"
TDF_PROVIDER_RETRY_DATABASE_URL="$TDF_PROVIDER_RETRY_URL" \
PGOPTIONS='-c statement_timeout=15000 -c lock_timeout=10000' \
  stack test --test-arguments='--match=provider-retry-runtime +RTS -N2 -RTS'

# Nonempty rollback must fail and leave all history readable.
if psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
    -f "$TDF_PROVIDER_RETRY_ROOT/tdf-hq/sql/2026-09-15_provider_query_recovery_rollback.sql" >/dev/null 2>&1; then
  echo "Provider query recovery rollback unexpectedly removed used history" >&2
  exit 1
fi
psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
  -c 'SELECT count(*) FROM commerce_provider_query_job' >/dev/null

if psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
    -f "$TDF_PROVIDER_RETRY_ROOT/tdf-hq/sql/2026-10-06_ticket_refund_allocation_rollback.sql" >/dev/null 2>&1; then
  echo "Ticket refund rollback unexpectedly removed used allocations" >&2
  exit 1
fi

if psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
    -f "$TDF_PROVIDER_RETRY_ROOT/tdf-hq/sql/2026-10-06_ticket_refund_request_binding_rollback.sql" >/dev/null 2>&1; then
  echo "Ticket refund rollback unexpectedly removed used request bindings" >&2
  exit 1
fi
