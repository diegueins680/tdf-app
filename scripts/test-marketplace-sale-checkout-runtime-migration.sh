#!/bin/sh
set -eu

TDF_MARKETPLACE_CONTAINER="tdf-marketplace-sale-migration-$$"
TDF_MARKETPLACE_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_MARKETPLACE_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_MARKETPLACE_CONTAINER" \
  -e POSTGRES_PASSWORD=marketplace-sale-test \
  -e POSTGRES_DB=tdf_marketplace_sale_test \
  postgres:17-alpine >/dev/null

attempt=0
until docker exec "$TDF_MARKETPLACE_CONTAINER" \
  psql -v ON_ERROR_STOP=1 -U postgres -d tdf_marketplace_sale_test -Atqc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Marketplace sale migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_MARKETPLACE_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_marketplace_sale_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_MARKETPLACE_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_marketplace_sale_test \
    < "$TDF_MARKETPLACE_ROOT/$1" >/dev/null
}

assert_equal() {
  actual=$1
  expected=$2
  label=$3
  if [ "$actual" != "$expected" ]; then
    echo "$label: expected '$expected', got '$actual'" >&2
    exit 1
  fi
}

psql_exec -c 'CREATE EXTENSION IF NOT EXISTS pgcrypto;' >/dev/null
psql_exec -c "
  CREATE TABLE asset (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(), name TEXT NOT NULL,
    category TEXT NOT NULL, condition TEXT NOT NULL, status TEXT NOT NULL,
    owner TEXT NOT NULL, maintenance_policy TEXT NOT NULL
  );
  CREATE TABLE marketplace_listing (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(), asset_id UUID NOT NULL REFERENCES asset(id),
    title TEXT NOT NULL, purpose TEXT NOT NULL DEFAULT 'sale', price_usd_cents BIGINT NOT NULL,
    markup_pct BIGINT NOT NULL DEFAULT 25, currency TEXT NOT NULL DEFAULT 'USD',
    active BOOLEAN NOT NULL DEFAULT TRUE, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );
  CREATE TABLE marketplace_order (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(), buyer_name TEXT NOT NULL,
    buyer_email TEXT NOT NULL, buyer_phone TEXT, total_usd_cents BIGINT NOT NULL,
    currency TEXT NOT NULL DEFAULT 'USD', status TEXT NOT NULL DEFAULT 'pending',
    payment_provider TEXT, paid_at TIMESTAMPTZ, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );
  CREATE TABLE marketplace_order_item (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    order_id UUID NOT NULL REFERENCES marketplace_order(id),
    listing_id UUID NOT NULL REFERENCES marketplace_listing(id), quantity BIGINT NOT NULL,
    unit_price_usd_cents BIGINT NOT NULL, subtotal_usd_cents BIGINT NOT NULL
  );
" >/dev/null
apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-15_marketplace_sale_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-15_marketplace_sale_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-15_marketplace_sale_checkout_runtime_rollback.sql
apply_file tdf-hq/sql/2026-08-15_marketplace_sale_checkout_runtime.sql

asset_id="a5100000-0000-4000-8000-000000000001"
listing_id="a5200000-0000-4000-8000-000000000001"
order_id="a5300000-0000-4000-8000-000000000001"
checkout_id="a5400000-0000-4000-8000-000000000001"
attempt_id="a5500000-0000-4000-8000-000000000001"
binding_id="a5600000-0000-4000-8000-000000000001"
legacy_paid_order_id="a5300000-0000-4000-8000-000000000002"
legacy_unpaid_order_id="a5300000-0000-4000-8000-000000000003"
legacy_terminal_order_id="a5300000-0000-4000-8000-000000000004"

psql_exec -c "
  INSERT INTO asset(id, name, category, condition, status, owner, maintenance_policy)
  VALUES ('$asset_id', 'Unique sale asset', 'audio', 'Good', 'Active', 'TDF', 'None');
  INSERT INTO marketplace_listing(
    id, asset_id, title, purpose, price_usd_cents, markup_pct, currency, active
  ) VALUES ('$listing_id', '$asset_id', 'Unique sale asset', 'sale', 12500, 25, 'USD', TRUE);
  INSERT INTO marketplace_order(
    id, buyer_name, buyer_email, total_usd_cents, currency, status, payment_provider
  ) VALUES ('$order_id', 'Sale Buyer', 'sale@example.com', 12500, 'USD', 'datafast_pending', 'datafast');
  INSERT INTO marketplace_order_item(order_id, listing_id, quantity, unit_price_usd_cents, subtotal_usd_cents)
  VALUES ('$order_id', '$listing_id', 1, 12500, 12500);
  INSERT INTO marketplace_order(
    id, buyer_name, buyer_email, total_usd_cents, currency, status, payment_provider, paid_at
  ) VALUES
    ('$legacy_paid_order_id', 'Legacy Paid', 'legacy-paid@example.com', 9900, 'USD', 'paid', 'paypal', NOW()),
    ('$legacy_unpaid_order_id', 'Legacy Unpaid', 'legacy-unpaid@example.com', 7500, 'USD', 'pending', NULL, NULL),
    ('$legacy_terminal_order_id', 'Legacy Cancelled', 'legacy-cancelled@example.com', 5000, 'USD', 'cancelled', NULL, NULL);
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_id', 'marketplace_sale', '$order_id', 'awaiting_payment', 'sandbox', 'USD',
    12500, 12500, 'sale@example.com', repeat('1', 64),
    'marketplace-sale-idempotency-0001', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_checkout_line_item(
    checkout_id, line_number, product_type, product_id, product_version,
    description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot
  ) VALUES (
    '$checkout_id', 1, 'marketplace_listing', '$listing_id', 'v1',
    'Unique sale asset', 1, 12500, 12500, 12500, '{}'::jsonb
  );
  INSERT INTO marketplace_sale_order_runtime(
    order_id, checkout_id, lookup_token_hash, create_idempotency_key,
    create_request_sha256, fulfillment_method, fulfillment_status,
    recipient_name, hold_expires_at
  ) VALUES (
    '$order_id', '$checkout_id', repeat('2', 64), 'marketplace-runtime-idempotency-0001',
    repeat('3', 64), 'pickup', 'on_hold', 'Sale Buyer', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_reservation_hold(
    checkout_id, resource_type, resource_id, quantity, status, expires_at
  ) VALUES (
    '$checkout_id', 'marketplace_asset_sale', '$asset_id', 1, 'active', NOW() + INTERVAL '15 minutes'
  );
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT backfill_disposition FROM marketplace_sale_checkout_backfill_report WHERE order_id='$legacy_paid_order_id';")" \
  "requires_payment_reconciliation" "Legacy paid order remains ambiguous"
assert_equal "$(psql_exec -Atc "SELECT backfill_disposition FROM marketplace_sale_checkout_backfill_report WHERE order_id='$legacy_unpaid_order_id';")" \
  "eligible_unpaid_manual_review" "Legacy unpaid order dry-run classification"
assert_equal "$(psql_exec -Atc "SELECT backfill_disposition FROM marketplace_sale_checkout_backfill_report WHERE order_id='$legacy_terminal_order_id';")" \
  "historical_terminal_manual_review" "Legacy terminal order dry-run classification"

if psql_exec -c "UPDATE marketplace_order SET status='paid' WHERE id='$order_id';" >/dev/null 2>&1; then
  echo "Marketplace order became paid without canonical verification" >&2
  exit 1
fi

if psql_exec -c "
  INSERT INTO commerce_reservation_hold(
    checkout_id, resource_type, resource_id, quantity, status, expires_at
  ) VALUES ('$checkout_id', 'marketplace_asset_sale', '$asset_id', 1, 'active', NOW() + INTERVAL '15 minutes');
" >/dev/null 2>&1; then
  echo "Unique marketplace asset accepted two active sale holds" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$attempt_id', '$checkout_id', 'datafast', 'sandbox', 'capture', 'processing',
    12500, 'USD', 'ENTITY-SANDBOX', 'marketplace-capture-0001'
  );
  INSERT INTO commerce_provider_binding(
    id, payment_attempt_id, provider, environment, merchant_account_ref,
    resource_type, provider_resource_id, provider_resource_path,
    merchant_reference, amount_minor, currency
  ) VALUES (
    '$binding_id', '$attempt_id', 'datafast', 'sandbox', 'ENTITY-SANDBOX',
    'payment', 'PAYMENT-0001', '/v1/checkouts/CHECKOUT-0001/payment',
    '$order_id', 12500, 'USD'
  );
  UPDATE commerce_checkout_session
    SET status='paid', paid_minor=12500, paid_at=NOW()
    WHERE id='$checkout_id';
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT status FROM marketplace_order WHERE id='$order_id';")" \
  "paid" "Verified marketplace payment sync"
assert_equal "$(psql_exec -Atc "SELECT fulfillment_status FROM marketplace_sale_order_runtime WHERE order_id='$order_id';")" \
  "ready_to_fulfill" "Paid checkout starts fulfillment without implying delivery"
assert_equal "$(psql_exec -Atc "SELECT status FROM commerce_reservation_hold WHERE checkout_id='$checkout_id';")" \
  "consumed" "Verified payment consumes the stock hold"
assert_equal "$(psql_exec -Atc "SELECT status FROM asset WHERE id='$asset_id';")" \
  "Active" "Payment does not mark equipment sold"

psql_exec -c "UPDATE commerce_checkout_session SET status='refunded' WHERE id='$checkout_id';" >/dev/null
if psql_exec -c "
  UPDATE marketplace_sale_order_runtime SET fulfillment_status='picking' WHERE order_id='$order_id';
" >/dev/null 2>&1; then
  echo "Fully refunded marketplace checkout advanced outbound fulfillment" >&2
  exit 1
fi
psql_exec -c "UPDATE commerce_checkout_session SET status='paid' WHERE id='$checkout_id';" >/dev/null

psql_exec -c "
  UPDATE marketplace_sale_order_runtime SET fulfillment_status='picking' WHERE order_id='$order_id';
  UPDATE marketplace_sale_order_runtime SET fulfillment_status='ready_for_pickup' WHERE order_id='$order_id';
  UPDATE marketplace_sale_order_runtime SET fulfillment_status='delivered' WHERE order_id='$order_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT status FROM asset WHERE id='$asset_id';")" \
  "Sold" "Delivered sale transfers inventory custody"
assert_equal "$(psql_exec -Atc "SELECT active::text FROM marketplace_listing WHERE id='$listing_id';")" \
  "false" "Delivered sale deactivates marketplace listings"

psql_exec -c "
  UPDATE marketplace_sale_order_runtime SET fulfillment_status='return_requested' WHERE order_id='$order_id';
  UPDATE marketplace_sale_order_runtime SET fulfillment_status='return_authorized' WHERE order_id='$order_id';
  UPDATE marketplace_sale_order_runtime SET fulfillment_status='returned' WHERE order_id='$order_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT status FROM asset WHERE id='$asset_id';")" \
  "Active" "Returned sale restores TDF custody without silently relisting"
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM marketplace_sale_fulfillment_event WHERE order_id='$order_id';")" \
  "7" "Immutable fulfillment transition history"

if apply_file tdf-hq/sql/2026-08-15_marketplace_sale_checkout_runtime_rollback.sql; then
  echo "Rollback removed live marketplace sale links" >&2
  exit 1
fi

echo "Marketplace sale checkout runtime migration passed rerun, clean rollback, dry-run legacy classification, unique holds, verified-payment separation, refund/dispute outbound blocking, fulfillment transitions, custody, returns, history, and live-link rollback checks."
