#!/bin/sh
set -eu

TDF_MERCH_RUNTIME_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_MERCH_RUNTIME_CONTAINER=""
TDF_MERCH_RUNTIME_DATABASE="tdf_merch_runtime_test"
TDF_MERCH_RUNTIME_URL=${TDF_MERCH_RUNTIME_DATABASE_URL:-}

cleanup() {
  if [ -n "$TDF_MERCH_RUNTIME_CONTAINER" ]; then
    docker rm -f "$TDF_MERCH_RUNTIME_CONTAINER" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ -n "$TDF_MERCH_RUNTIME_URL" ]; then
  runtime_database_name=$(psql "$TDF_MERCH_RUNTIME_URL" -X -qAt -v ON_ERROR_STOP=1 -c "SELECT current_database()")
  case "$runtime_database_name" in
    *merch_runtime*) ;;
    *)
      echo "TDF_MERCH_RUNTIME_DATABASE_URL must connect to a disposable database whose actual name contains merch_runtime" >&2
      exit 1
      ;;
  esac
  if [ "$(psql "$TDF_MERCH_RUNTIME_URL" -X -qAt -v ON_ERROR_STOP=1 -c "SELECT count(*) FROM pg_class AS c JOIN pg_namespace AS n ON n.oid = c.relnamespace WHERE n.nspname NOT IN ('pg_catalog', 'information_schema') AND n.nspname !~ '^pg_toast' AND c.relkind IN ('r','p','v','m')")" != "0" ]; then
    echo "External merch runtime database must be empty and disposable" >&2
    exit 1
  fi
  apply_file() {
    psql "$TDF_MERCH_RUNTIME_URL" -X -q -v ON_ERROR_STOP=1 < "$1" >/dev/null
  }
else
  TDF_MERCH_RUNTIME_CONTAINER="tdf-merch-runtime-test-$$"
  docker run --rm -d \
    --name "$TDF_MERCH_RUNTIME_CONTAINER" \
    -p 127.0.0.1::5432 \
    -e POSTGRES_HOST_AUTH_METHOD=trust \
    -e POSTGRES_DB="$TDF_MERCH_RUNTIME_DATABASE" \
    postgres:16-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_MERCH_RUNTIME_CONTAINER" pg_isready -U postgres -d "$TDF_MERCH_RUNTIME_DATABASE" >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 45 ]; then
      echo "Merch runtime test database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done

  sleep 5
  until docker exec "$TDF_MERCH_RUNTIME_CONTAINER" pg_isready -U postgres -d "$TDF_MERCH_RUNTIME_DATABASE" >/dev/null 2>&1; do
    sleep 1
  done

  apply_file() {
    docker exec -i "$TDF_MERCH_RUNTIME_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_MERCH_RUNTIME_DATABASE" < "$1" >/dev/null
  }

  published_address=$(docker port "$TDF_MERCH_RUNTIME_CONTAINER" 5432/tcp | head -1)
  published_port=${published_address##*:}
  if [ -z "$published_port" ]; then
    echo "Could not resolve the isolated PostgreSQL port" >&2
    exit 1
  fi
  TDF_MERCH_RUNTIME_URL="postgresql://postgres@127.0.0.1:$published_port/$TDF_MERCH_RUNTIME_DATABASE"
fi

apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/sql/init_schema.sql"
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/sql/2026-08-14_catalog_canonical_schema.sql"
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/sql/2026-08-13_unified_checkout_core.sql"
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/sql/2026-08-14_checkout_event_refund_runtime.sql"
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/test/integration/artist_merch_runtime_schema_fixture.sql"
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/sql/2026-09-07_artist_merch_storefronts.sql"
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/test/integration/artist_merch_runtime_fixture.sql"

cd "$TDF_MERCH_RUNTIME_ROOT/tdf-hq"
TDF_MERCH_RUNTIME_DATABASE_URL="$TDF_MERCH_RUNTIME_URL" \
DATABASE_URL="$TDF_MERCH_RUNTIME_URL" \
APP_ENV=sandbox \
RUN_MIGRATIONS=false \
RESET_DB=false \
SEED_DB=false \
MERCH_BANK_TRANSFER_INSTRUCTIONS="Synthetic runtime instructions; no funds or provider are involved." \
  stack test --fast --test-arguments=--match=artist-merch-runtime
