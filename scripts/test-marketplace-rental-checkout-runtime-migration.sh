#!/bin/sh
set -eu

TDF_RENTAL_CONTAINER="tdf-marketplace-rental-migration-$$"
TDF_RENTAL_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_RENTAL_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_RENTAL_CONTAINER" \
  -e POSTGRES_PASSWORD=marketplace-rental-test \
  -e POSTGRES_DB=tdf_marketplace_rental_test \
  postgres:17-alpine >/dev/null

attempt=0
until docker exec "$TDF_RENTAL_CONTAINER" \
  psql -v ON_ERROR_STOP=1 -U postgres -d tdf_marketplace_rental_test -Atqc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Marketplace rental migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_RENTAL_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_marketplace_rental_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_RENTAL_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_marketplace_rental_test \
    < "$TDF_RENTAL_ROOT/$1" >/dev/null
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
  CREATE TABLE marketplace_cart (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );
  CREATE TABLE marketplace_cart_item (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(), cart_id UUID NOT NULL REFERENCES marketplace_cart(id),
    listing_id UUID NOT NULL REFERENCES marketplace_listing(id), quantity BIGINT NOT NULL DEFAULT 1,
    UNIQUE(cart_id, listing_id)
  );
  CREATE TABLE marketplace_order (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(), cart_id UUID REFERENCES marketplace_cart(id),
    buyer_name TEXT NOT NULL, buyer_email TEXT NOT NULL, buyer_phone TEXT,
    total_usd_cents BIGINT NOT NULL, currency TEXT NOT NULL DEFAULT 'USD',
    status TEXT NOT NULL DEFAULT 'pending', payment_provider TEXT,
    paid_at TIMESTAMPTZ, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );
  CREATE TABLE marketplace_order_item (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(), order_id UUID NOT NULL REFERENCES marketplace_order(id),
    listing_id UUID NOT NULL REFERENCES marketplace_listing(id), quantity BIGINT NOT NULL,
    unit_price_usd_cents BIGINT NOT NULL, subtotal_usd_cents BIGINT NOT NULL
  );
" >/dev/null

legacy_rental_asset_id="b5100000-0000-4000-8000-000000000099"
legacy_rental_listing_id="b5200000-0000-4000-8000-000000000099"
psql_exec -c "
  INSERT INTO asset(id, name, category, condition, status, owner, maintenance_policy)
  VALUES ('$legacy_rental_asset_id', 'Migrated rental console', 'audio', 'Good', 'Active', 'TDF', 'Inspect each return');
  INSERT INTO marketplace_listing(
    id, asset_id, title, purpose, price_usd_cents, markup_pct, currency, active
  ) VALUES (
    '$legacy_rental_listing_id', '$legacy_rental_asset_id', 'Migrated rental console',
    'rent', 1200, 25, 'USD', TRUE
  );
" >/dev/null

apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-25_commerce_trigger_row_binding_compatibility.sql
apply_file tdf-hq/sql/2026-08-15_marketplace_sale_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-15_marketplace_rental_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-15_marketplace_rental_checkout_runtime.sql

asset_id="b5100000-0000-4000-8000-000000000001"
listing_id="b5200000-0000-4000-8000-000000000001"
order_id="b5300000-0000-4000-8000-000000000001"
checkout_id="b5400000-0000-4000-8000-000000000001"
attempt_id="b5500000-0000-4000-8000-000000000001"
binding_id="b5600000-0000-4000-8000-000000000001"
other_order_id="b5300000-0000-4000-8000-000000000002"
other_checkout_id="b5400000-0000-4000-8000-000000000002"

psql_exec -c "
  INSERT INTO asset(id, name, category, condition, status, owner, maintenance_policy)
  VALUES ('$asset_id', 'Rental console', 'audio', 'Good', 'Active', 'TDF', 'Inspect each return');
  INSERT INTO marketplace_listing(
    id, asset_id, title, purpose, price_usd_cents, markup_pct, currency, active
  ) VALUES ('$listing_id', '$asset_id', 'Rental console', 'rent', 1000, 25, 'USD', TRUE);
  INSERT INTO marketplace_rental_listing_terms(
    listing_id, daily_rate_usd_cents, weekly_rate_usd_cents,
    security_deposit_usd_cents, late_fee_usd_cents, min_days, max_days,
    cancellation_window_hours, timezone, terms_version, terms_summary,
    active, approved_at, approved_by
  ) VALUES (
    '$listing_id', 1000, 6000, 500, 1000, 1, 30, 24,
    'America/Guayaquil', 'rental-v1',
    'Return the asset in the documented outbound condition.',
    TRUE, NOW(), 'migration-test'
  );
  INSERT INTO marketplace_order(
    id, buyer_name, buyer_email, buyer_phone, total_usd_cents, currency, status
  ) VALUES ('$order_id', 'Rental Buyer', 'rental@example.com', '+593999999999', 3500, 'USD', 'awaiting_payment');
  INSERT INTO marketplace_order_item(order_id, listing_id, quantity, unit_price_usd_cents, subtotal_usd_cents)
  VALUES ('$order_id', '$listing_id', 1, 1000, 3500);
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_id', 'marketplace_rental', '$order_id', 'awaiting_payment', 'sandbox', 'USD',
    3500, 3500, 'rental@example.com', repeat('1', 64),
    'marketplace-rental-idempotency-0001', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_checkout_line_item(
    checkout_id, line_number, product_type, product_id, product_version,
    description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot
  ) VALUES (
    '$checkout_id', 1, 'marketplace_rental_asset', '$asset_id', 'rental-v1',
    'Rental console', 1, 3500, 3500, 3500, '{}'::jsonb
  );
  INSERT INTO marketplace_rental_order_runtime(
    order_id, checkout_id, listing_id, asset_id, lookup_token_hash,
    create_idempotency_key, create_request_sha256, fulfillment_method,
    rental_status, deposit_status, start_date, end_date, duration_days, timezone,
    daily_rate_usd_cents, weekly_rate_usd_cents, rental_charge_usd_cents,
    security_deposit_usd_cents, late_fee_usd_cents, terms_version, terms_accepted_at,
    identity_document_type, identity_document_last4,
    recipient_name, recipient_phone, hold_expires_at
  ) VALUES (
    '$order_id', '$checkout_id', '$listing_id', '$asset_id', repeat('2',64),
    'marketplace-rental-runtime-0001', repeat('3',64), 'pickup',
    'on_hold', 'awaiting_payment', CURRENT_DATE + 2, CURRENT_DATE + 4, 3, 'America/Guayaquil',
    1000, 6000, 3000, 500, 1000, 'rental-v1', NOW(),
    'cedula', '1234', 'Rental Buyer', '+593999999999', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_reservation_hold(
    checkout_id, resource_type, resource_id, quantity, status, expires_at
  ) VALUES ('$checkout_id', 'marketplace_asset_rental', '$asset_id', 1, 'active', NOW() + INTERVAL '15 minutes');
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='commerce.marketplace_sales' AND environment='production';")" \
  "true" "Marketplace sales feature flag"
assert_equal "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='commerce.marketplace_rentals' AND environment='production';")" \
  "true" "Marketplace rentals feature flag"
assert_equal "$(psql_exec -Atc "SELECT active::text || ':' || terms_version || ':' || security_deposit_usd_cents::text FROM marketplace_rental_listing_terms WHERE listing_id='$legacy_rental_listing_id';")" \
  "true:marketplace-rental-v1:0" "Migrated rental listing active terms"
assert_equal "$(psql_exec -Atc "SELECT count(*)::text FROM marketplace_rental_listing_terms_history WHERE listing_id='$legacy_rental_listing_id';")" \
  "1" "Migrated rental terms audit history"
if psql_exec -c "UPDATE marketplace_rental_listing_terms SET daily_rate_usd_cents=1300 WHERE listing_id='$legacy_rental_listing_id';" >/dev/null 2>&1; then
  echo "Commercial rental terms changed without a new terms version" >&2
  exit 1
fi
assert_equal "$(psql_exec -Atc "SELECT order_kind FROM marketplace_order_checkout_runtime WHERE order_id='$order_id';")" \
  "rental" "Unified marketplace checkout runtime"

if psql_exec -c "UPDATE marketplace_order SET status='paid' WHERE id='$order_id';" >/dev/null 2>&1; then
  echo "Rental order became paid without verified checkout evidence" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$attempt_id', '$checkout_id', 'datafast', 'sandbox', 'capture', 'processing',
    3500, 'USD', 'ENTITY-SANDBOX', 'marketplace-rental-capture-0001'
  );
  INSERT INTO commerce_provider_binding(
    id, payment_attempt_id, provider, environment, merchant_account_ref,
    resource_type, provider_resource_id, provider_resource_path,
    merchant_reference, amount_minor, currency
  ) VALUES (
    '$binding_id', '$attempt_id', 'datafast', 'sandbox', 'ENTITY-SANDBOX',
    'payment', 'RENTAL-PAYMENT-0001', '/v1/checkouts/RENTAL-CHECKOUT/payment',
    '$order_id', 3500, 'USD'
  );
  UPDATE commerce_checkout_session
    SET status='paid', paid_minor=3500, paid_at=NOW() WHERE id='$checkout_id';
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT status FROM marketplace_order WHERE id='$order_id';")" \
  "paid" "Verified rental payment sync"
assert_equal "$(psql_exec -Atc "SELECT rental_status || ':' || deposit_status FROM marketplace_rental_order_runtime WHERE order_id='$order_id';")" \
  "confirmed:collected" "Payment remains separate from handoff and records deposit collection"
assert_equal "$(psql_exec -Atc "SELECT status FROM asset WHERE id='$asset_id';")" \
  "Active" "Payment does not transfer rental custody"

psql_exec -c "UPDATE marketplace_rental_order_runtime SET rental_status='ready_for_handoff' WHERE order_id='$order_id';" >/dev/null
if psql_exec -c "UPDATE marketplace_rental_order_runtime SET rental_status='checked_out' WHERE order_id='$order_id';" >/dev/null 2>&1; then
  echo "Rental custody advanced without an outbound condition report" >&2
  exit 1
fi
psql_exec -c "
  UPDATE marketplace_rental_order_runtime
  SET condition_out='Good; serial verified', checked_out_at=NOW(), rental_status='checked_out'
  WHERE order_id='$order_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT status FROM asset WHERE id='$asset_id';")" \
  "Booked" "Rental handoff changes current custody"

psql_exec -c "UPDATE marketplace_rental_order_runtime SET rental_status='return_due' WHERE order_id='$order_id';" >/dev/null
if psql_exec -c "UPDATE marketplace_rental_order_runtime SET rental_status='returned_pending_inspection' WHERE order_id='$order_id';" >/dev/null 2>&1; then
  echo "Rental return advanced without an inbound condition report" >&2
  exit 1
fi
psql_exec -c "
  UPDATE marketplace_rental_order_runtime
  SET condition_in='Returned with cosmetic scratch', returned_at=NOW(), rental_status='returned_pending_inspection'
  WHERE order_id='$order_id';
  UPDATE marketplace_rental_order_runtime
  SET deposit_deduction_usd_cents=100, rental_status='damage_review'
  WHERE order_id='$order_id';
  UPDATE marketplace_rental_order_runtime SET rental_status='deposit_refund_due' WHERE order_id='$order_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT status FROM asset WHERE id='$asset_id';")" \
  "Active" "Returned rental restores asset availability after custody receipt"
assert_equal "$(psql_exec -Atc "SELECT deposit_status FROM marketplace_rental_order_runtime WHERE order_id='$order_id';")" \
  "partial_refund_due" "Deposit deduction remains due rather than falsely refunded"
if psql_exec -c "UPDATE marketplace_rental_order_runtime SET rental_status='closed' WHERE order_id='$order_id';" >/dev/null 2>&1; then
  echo "Rental closed before deposit settlement evidence" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO marketplace_order(id, buyer_name, buyer_email, total_usd_cents, currency, status)
  VALUES ('$other_order_id', 'Other Buyer', 'other@example.com', 3500, 'USD', 'awaiting_payment');
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash, idempotency_key, expires_at
  ) VALUES (
    '$other_checkout_id', 'marketplace_rental', '$other_order_id', 'awaiting_payment', 'sandbox', 'USD',
    3500, 3500, 'other@example.com', repeat('5',64), 'marketplace-rental-runtime-0002', NOW() + INTERVAL '15 minutes'
  );
" >/dev/null
if psql_exec -c "
  INSERT INTO marketplace_rental_order_runtime(
    order_id, checkout_id, listing_id, asset_id, lookup_token_hash,
    create_idempotency_key, create_request_sha256, fulfillment_method,
    rental_status, deposit_status, start_date, end_date, duration_days, timezone,
    daily_rate_usd_cents, rental_charge_usd_cents, security_deposit_usd_cents,
    late_fee_usd_cents, terms_version, terms_accepted_at,
    identity_document_type, identity_document_last4,
    recipient_name, hold_expires_at
  ) VALUES (
    '$other_order_id', '$other_checkout_id', '$listing_id', '$asset_id', repeat('6',64),
    'marketplace-rental-runtime-0002', repeat('7',64), 'pickup', 'on_hold', 'awaiting_payment',
    CURRENT_DATE + 3, CURRENT_DATE + 5, 3, 'America/Guayaquil', 1000, 3000, 500, 1000,
    'rental-v1', NOW(), 'cedula', '5678', 'Other Buyer', NOW() + INTERVAL '15 minutes'
  );
" >/dev/null 2>&1; then
  echo "Overlapping rental dates bypassed the exclusion constraint" >&2
  exit 1
fi

if apply_file tdf-hq/sql/2026-08-15_marketplace_rental_checkout_runtime_rollback.sql; then
  echo "Rollback removed live marketplace rental links" >&2
  exit 1
fi

echo "Marketplace rental checkout runtime migration passed rerun, enabled gates, immutable pricing linkage, verified-payment separation, date exclusion, custody reports, deposit due truthfulness, and live-link rollback checks."
