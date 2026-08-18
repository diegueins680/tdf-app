#!/bin/sh
set -eu

TDF_PRICING_CONTAINER="tdf-distribution-pricing-test-$$"
TDF_PRICING_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() { docker rm -f "$TDF_PRICING_CONTAINER" >/dev/null 2>&1 || true; }
trap cleanup EXIT INT TERM

docker run --rm -d --name "$TDF_PRICING_CONTAINER" \
  -e POSTGRES_PASSWORD=pricing-migration-test -e POSTGRES_DB=tdf_pricing_migration_test postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_PRICING_CONTAINER" pg_isready -U postgres -d tdf_pricing_migration_test >/dev/null 2>&1; do
  attempt=$((attempt + 1)); test "$attempt" -lt 30 || exit 1; sleep 1
done
sleep 5

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PRICING_CONTAINER" \
    psql -q -v ON_ERROR_STOP=1 -U postgres -d tdf_pricing_migration_test < "$1" >/dev/null
}
psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PRICING_CONTAINER" \
    psql -q -v ON_ERROR_STOP=1 -U postgres -d tdf_pricing_migration_test "$@"
}

psql_exec -c 'CREATE EXTENSION IF NOT EXISTS pgcrypto;' >/dev/null
apply_file "$TDF_PRICING_ROOT/tdf-hq/sql/2026-08-02_ddex_catalog_core.sql"
apply_file "$TDF_PRICING_ROOT/tdf-hq/sql/2026-08-13_unified_checkout_core.sql"
apply_file "$TDF_PRICING_ROOT/tdf-hq/sql/2026-08-13_distribution_accounting_core.sql"
apply_file "$TDF_PRICING_ROOT/tdf-hq/sql/2026-08-13_distribution_product_seeds.sql"

test "$(psql_exec -Atc "SELECT count(*) FROM distribution_product_version WHERE status='pending_approval';")" = "14"
test "$(psql_exec -Atc "SELECT count(*) FROM distribution_product_review WHERE status='pending';")" = "7"
test "$(psql_exec -Atc "SELECT count(DISTINCT locale) FROM distribution_product_version WHERE product_key='distribution.single.standard';")" = "2"
test "$(psql_exec -Atc "SELECT count(*) FROM distribution_product_version WHERE status='active';")" = "0"

apply_file "$TDF_PRICING_ROOT/tdf-hq/sql/2026-08-13_distribution_product_seeds_rollback.sql"
apply_file "$TDF_PRICING_ROOT/tdf-hq/sql/2026-08-13_distribution_product_seeds.sql"

if psql_exec -c "UPDATE distribution_product_version SET status='active',approved_by=20,approved_at=NOW() WHERE product_key='distribution.single.standard';" >/dev/null 2>&1; then
  echo "Distribution products activated without an approved market and margin review" >&2
  exit 1
fi

psql_exec -c "
  UPDATE distribution_product_review
  SET requested_by=10,status='approved',reviewed_by=20,reviewed_at=NOW()
  WHERE product_key='distribution.single.standard';
  UPDATE distribution_product_version
  SET status='active',approved_by=20,approved_at=NOW()
  WHERE product_key='distribution.single.standard';
" >/dev/null

if psql_exec -c "UPDATE distribution_product_version SET price_minor=1 WHERE product_key='distribution.single.standard';" >/dev/null 2>&1; then
  echo "Active distribution economics accepted an in-place mutation" >&2
  exit 1
fi

if apply_file "$TDF_PRICING_ROOT/tdf-hq/sql/2026-08-13_distribution_product_seeds_rollback.sql" 2>/dev/null; then
  echo "Rollback removed approved distribution products" >&2
  exit 1
fi

echo "Distribution pricing migration kept 14 bilingual seeds inactive, enforced independent approval, and refused destructive rollback."
