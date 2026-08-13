#!/bin/sh
set -eu

TDF_SERVICE_MIGRATION_CONTAINER="tdf-service-migration-test-$$"
TDF_SERVICE_MIGRATION_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_SERVICE_MIGRATION_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_SERVICE_MIGRATION_CONTAINER" \
  -e POSTGRES_PASSWORD=service-migration-test \
  -e POSTGRES_DB=tdf_service_migration_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_SERVICE_MIGRATION_CONTAINER" pg_isready -U postgres -d tdf_service_migration_test >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Service migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

# Avoid observing the temporary initialization server just before the official
# Postgres image performs its one-time restart.
sleep 5
until docker exec "$TDF_SERVICE_MIGRATION_CONTAINER" pg_isready -U postgres -d tdf_service_migration_test >/dev/null 2>&1; do
  sleep 1
done

psql_exec() {
  docker exec "$TDF_SERVICE_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_migration_test "$@"
}

psql_exec -c 'CREATE EXTENSION IF NOT EXISTS pgcrypto;' >/dev/null
psql_exec -c 'CREATE OR REPLACE FUNCTION trigger_set_timestamp() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN NEW.updated_at = NOW(); RETURN NEW; END; $$;' >/dev/null
docker exec -i "$TDF_SERVICE_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_migration_test \
  < "$TDF_SERVICE_MIGRATION_ROOT/tdf-hq/sql/2026-08-04_service_storefront.sql" >/dev/null
docker exec -i "$TDF_SERVICE_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_migration_test \
  < "$TDF_SERVICE_MIGRATION_ROOT/tdf-hq/sql/2026-08-13_service_storefront_phase0_hardening.sql" >/dev/null

pro_max=$(psql_exec -Atc "SELECT max_song_count FROM service_storefront_package WHERE service_kind = 'Mastering' AND tier = 'Pro';")
test "$pro_max" = "3"

if psql_exec -c "UPDATE service_storefront_package SET min_song_count = 4, max_song_count = 3 WHERE service_kind = 'Mastering' AND tier = 'Pro';" >/dev/null 2>&1; then
  echo "Package quantity constraint accepted an invalid range" >&2
  exit 1
fi

package_id=$(psql_exec -Atc 'SELECT id FROM service_storefront_package ORDER BY sort_order LIMIT 1;')
psql_exec -c "INSERT INTO service_storefront_order (order_number, buyer_name, buyer_email, package_id, service_kind, tier, price_usd_cents, datafast_checkout_id, create_idempotency_key, create_request_sha256) VALUES ('TDF-MIG001', 'Migration Test', 'migration@example.com', '$package_id', 'Mixing', 'Basic', 8000, 'checkout-unique', 'idempotency-unique-1', 'request-sha-1');" >/dev/null
if psql_exec -c "INSERT INTO service_storefront_order (order_number, buyer_name, buyer_email, package_id, service_kind, tier, price_usd_cents, datafast_checkout_id, create_idempotency_key) VALUES ('TDF-MIG002', 'Migration Test', 'migration@example.com', '$package_id', 'Mixing', 'Basic', 8000, 'checkout-unique', 'idempotency-unique-2');" >/dev/null 2>&1; then
  echo "Provider uniqueness constraint accepted a duplicate checkout" >&2
  exit 1
fi
if psql_exec -c "INSERT INTO service_storefront_order (order_number, buyer_name, buyer_email, package_id, service_kind, tier, price_usd_cents, create_idempotency_key, create_request_sha256) VALUES ('TDF-MIG003', 'Migration Test', 'migration@example.com', '$package_id', 'Mixing', 'Basic', 8000, 'idempotency-unique-1', 'different-request-sha');" >/dev/null 2>&1; then
  echo "Order creation idempotency constraint accepted a duplicate retry key" >&2
  exit 1
fi

legacy_token=$(psql_exec -Atc "SELECT COALESCE(lookup_token_hash, 'NULL') FROM service_storefront_order WHERE order_number = 'TDF-MIG001';")
test "$legacy_token" = "NULL"

docker exec -i "$TDF_SERVICE_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_migration_test \
  < "$TDF_SERVICE_MIGRATION_ROOT/tdf-hq/sql/2026-08-13_service_storefront_phase0_hardening_rollback.sql" >/dev/null

remaining_columns=$(psql_exec -Atc "SELECT count(*) FROM information_schema.columns WHERE table_name IN ('service_storefront_package', 'service_storefront_order') AND column_name IN ('min_song_count', 'max_song_count', 'lookup_token_hash', 'paypal_capture_id', 'create_idempotency_key', 'create_request_sha256');")
test "$remaining_columns" = "0"

docker exec -i "$TDF_SERVICE_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_migration_test \
  < "$TDF_SERVICE_MIGRATION_ROOT/tdf-hq/sql/2026-08-13_service_storefront_phase0_hardening.sql" >/dev/null

echo "Service storefront Phase 0 migration passed forward, constraint, rollback, and reapply checks."
