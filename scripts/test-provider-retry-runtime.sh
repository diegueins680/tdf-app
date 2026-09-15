#!/bin/sh
set -eu

TDF_PROVIDER_RETRY_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_PROVIDER_RETRY_CONTAINER=""
TDF_PROVIDER_RETRY_URL=${TDF_PROVIDER_RETRY_DATABASE_URL:-}

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
psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 -c 'CREATE EXTENSION IF NOT EXISTS pgcrypto' >/dev/null
psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
  -f "$TDF_PROVIDER_RETRY_ROOT/scripts/__tests__/fixtures/provider-retry-ledger-base.sql" >/dev/null

for migration in \
  2026-08-13_unified_checkout_core \
  2026-08-25_commerce_trigger_row_binding_compatibility \
  2026-08-15_marketplace_sale_checkout_runtime \
  2026-08-15_marketplace_rental_checkout_runtime \
  2026-08-14_checkout_event_refund_runtime \
  2026-09-09_canonical_payment_lifecycle \
  2026-09-10_payment_attempt_intent_binding \
  2026-09-11_payment_intent_runtime_sync \
  2026-09-11_provider_capability_catalog \
  2026-09-13_provider_execution_runtime; do
  psql "$TDF_PROVIDER_RETRY_URL" -X -q -v ON_ERROR_STOP=1 \
    -f "$TDF_PROVIDER_RETRY_ROOT/tdf-hq/sql/$migration.sql" >/dev/null
done

cd "$TDF_PROVIDER_RETRY_ROOT/tdf-hq"
TDF_PROVIDER_RETRY_DATABASE_URL="$TDF_PROVIDER_RETRY_URL" \
PGOPTIONS='-c statement_timeout=15000 -c lock_timeout=10000' \
  stack test --fast --test-arguments='--match=provider-retry-runtime +RTS -N2 -RTS'
