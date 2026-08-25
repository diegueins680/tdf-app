#!/bin/sh
set -eu

TDF_PRODUCT_CONTAINER="tdf-product-migration-test-$$"
TDF_PRODUCT_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() { docker rm -f "$TDF_PRODUCT_CONTAINER" >/dev/null 2>&1 || true; }
trap cleanup EXIT INT TERM

docker run --rm -d --name "$TDF_PRODUCT_CONTAINER" \
  -e POSTGRES_PASSWORD=product-migration-test -e POSTGRES_DB=tdf_product_migration_test postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_PRODUCT_CONTAINER" pg_isready -U postgres -d tdf_product_migration_test >/dev/null 2>&1; do
  attempt=$((attempt + 1)); test "$attempt" -lt 30 || exit 1; sleep 1
done
sleep 5

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PRODUCT_CONTAINER" \
    psql -q -v ON_ERROR_STOP=1 -U postgres -d tdf_product_migration_test < "$1" >/dev/null
}
psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PRODUCT_CONTAINER" \
    psql -q -v ON_ERROR_STOP=1 -U postgres -d tdf_product_migration_test "$@"
}

psql_exec -c 'CREATE EXTENSION IF NOT EXISTS pgcrypto;' >/dev/null
apply_file "$TDF_PRODUCT_ROOT/tdf-hq/sql/2026-08-13_unified_checkout_core.sql"
apply_file "$TDF_PRODUCT_ROOT/tdf-hq/sql/2026-08-25_commerce_trigger_row_binding_compatibility.sql"
apply_file "$TDF_PRODUCT_ROOT/tdf-hq/sql/2026-08-13_versioned_revenue_products.sql"

active_count=$(psql_exec -Atc "SELECT count(*) FROM commerce_product_version WHERE domain_type='domo' AND status='active';")
test "$active_count" = "0"

if psql_exec -c "UPDATE commerce_product_version SET status='active',approved_by=2,approved_at=NOW() WHERE id='81000000-0000-0000-0000-000000000001';" >/dev/null 2>&1; then
  echo "Domo legacy rates activated without an approved comparison" >&2
  exit 1
fi

psql_exec -c "
  UPDATE commerce_rate_card_review SET status='approved',reviewed_by=2,reviewed_at=NOW()
  WHERE id='82000000-0000-0000-0000-000000000001';
  UPDATE commerce_product_version SET status='active',approved_by=2,approved_at=NOW()
  WHERE id='81000000-0000-0000-0000-000000000001';
" >/dev/null

if psql_exec -c "UPDATE commerce_product_version SET deposit_basis_points=5000 WHERE id='81000000-0000-0000-0000-000000000001';" >/dev/null 2>&1; then
  echo "Approved Domo economics accepted an in-place mutation" >&2
  exit 1
fi

checkout_enabled=$(psql_exec -Atc "SELECT enabled FROM revenue_feature_flag WHERE flag_key='domo.checkout' AND environment='production';")
test "$checkout_enabled" = "f"

if apply_file "$TDF_PRODUCT_ROOT/tdf-hq/sql/2026-08-13_versioned_revenue_products_rollback.sql" 2>/dev/null; then
  echo "Rollback removed an approved product version" >&2
  exit 1
fi

echo "Versioned revenue product migration preserved legacy Domo rates for approval, blocked unsafe activation, and kept production checkout disabled."
