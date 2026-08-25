#!/bin/sh
set -eu

TDF_OPERATIONS_CONTAINER="tdf-marketplace-operations-migration-$$"
TDF_OPERATIONS_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_OPERATIONS_PGURL=${TDF_MARKETPLACE_OPERATIONS_PGURL:-}

if [ -n "$TDF_OPERATIONS_PGURL" ]; then
  psql_exec() {
    PGOPTIONS='-c statement_timeout=10000' \
      psql "$TDF_OPERATIONS_PGURL" -v ON_ERROR_STOP=1 "$@"
  }

  apply_file() {
    PGOPTIONS='-c statement_timeout=10000' \
      psql "$TDF_OPERATIONS_PGURL" -v ON_ERROR_STOP=1 \
      < "$TDF_OPERATIONS_ROOT/$1" >/dev/null
  }
else
  cleanup() {
    docker rm -f "$TDF_OPERATIONS_CONTAINER" >/dev/null 2>&1 || true
  }
  trap cleanup EXIT INT TERM

  docker run --rm -d \
    --name "$TDF_OPERATIONS_CONTAINER" \
    -e POSTGRES_PASSWORD=marketplace-operations-test \
    -e POSTGRES_DB=tdf_marketplace_operations_test \
    postgres:17-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_OPERATIONS_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_marketplace_operations_test -Atqc 'SELECT 1' \
    >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 30 ]; then
      echo "Marketplace operations migration database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done

  psql_exec() {
    docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_OPERATIONS_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_marketplace_operations_test "$@"
  }

  apply_file() {
    docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_OPERATIONS_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_marketplace_operations_test \
      < "$TDF_OPERATIONS_ROOT/$1" >/dev/null
  }
fi

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

apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-25_commerce_trigger_row_binding_compatibility.sql
apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime.sql
apply_file tdf-hq/sql/2026-08-15_marketplace_sale_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-15_marketplace_rental_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-17_marketplace_customer_requests_and_deposit_settlement.sql
apply_file tdf-hq/sql/2026-08-17_marketplace_customer_requests_and_deposit_settlement.sql
apply_file tdf-hq/sql/2026-08-17_marketplace_customer_requests_and_deposit_settlement_rollback.sql
apply_file tdf-hq/sql/2026-08-17_marketplace_customer_requests_and_deposit_settlement.sql

sale_asset_id="c5100000-0000-4000-8000-000000000001"
sale_listing_id="c5200000-0000-4000-8000-000000000001"
sale_order_id="c5300000-0000-4000-8000-000000000001"
sale_checkout_id="c5400000-0000-4000-8000-000000000001"
sale_attempt_id="c5500000-0000-4000-8000-000000000001"
sale_request_id="c5700000-0000-4000-8000-000000000001"
rental_asset_id="c5100000-0000-4000-8000-000000000002"
rental_listing_id="c5200000-0000-4000-8000-000000000002"
rental_order_id="c5300000-0000-4000-8000-000000000002"
rental_checkout_id="c5400000-0000-4000-8000-000000000002"
rental_attempt_id="c5500000-0000-4000-8000-000000000002"
extension_request_id="c5700000-0000-4000-8000-000000000002"
dispute_request_id="c5700000-0000-4000-8000-000000000003"
settlement_id="c5800000-0000-4000-8000-000000000001"

psql_exec -c "
  INSERT INTO asset(id, name, category, condition, status, owner, maintenance_policy) VALUES
    ('$sale_asset_id', 'Sale interface', 'audio', 'Good', 'Active', 'TDF', 'Inspect before pickup'),
    ('$rental_asset_id', 'Rental console', 'audio', 'Good', 'Active', 'TDF', 'Inspect each return');
  INSERT INTO marketplace_listing(
    id, asset_id, title, purpose, price_usd_cents, markup_pct, currency, active
  ) VALUES
    ('$sale_listing_id', '$sale_asset_id', 'Sale interface', 'sale', 12500, 25, 'USD', TRUE),
    ('$rental_listing_id', '$rental_asset_id', 'Rental console', 'rent', 1000, 25, 'USD', TRUE);
  INSERT INTO marketplace_rental_listing_terms(
    listing_id, daily_rate_usd_cents, weekly_rate_usd_cents,
    security_deposit_usd_cents, late_fee_usd_cents, min_days, max_days,
    cancellation_window_hours, timezone, terms_version, terms_summary,
    active, approved_at, approved_by
  ) VALUES (
    '$rental_listing_id', 1000, 6000, 500, 1000, 1, 30, 24,
    'America/Guayaquil', 'rental-v1', 'Return in the documented condition.',
    TRUE, NOW(), 'operations-migration-test'
  );
  INSERT INTO marketplace_order(
    id, buyer_name, buyer_email, buyer_phone, total_usd_cents, currency, status
  ) VALUES
    ('$sale_order_id', 'Sale Buyer', 'sale@example.com', '+593999999998', 12500, 'USD', 'awaiting_payment'),
    ('$rental_order_id', 'Rental Buyer', 'rental@example.com', '+593999999999', 3500, 'USD', 'awaiting_payment');
  INSERT INTO marketplace_order_item(
    order_id, listing_id, quantity, unit_price_usd_cents, subtotal_usd_cents
  ) VALUES
    ('$sale_order_id', '$sale_listing_id', 1, 12500, 12500),
    ('$rental_order_id', '$rental_listing_id', 1, 1000, 3500);
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES
    ('$sale_checkout_id', 'marketplace_sale', '$sale_order_id', 'awaiting_payment', 'sandbox', 'USD',
      12500, 12500, 'sale@example.com', repeat('1', 64),
      'marketplace-operations-sale-0001', NOW() + INTERVAL '15 minutes'),
    ('$rental_checkout_id', 'marketplace_rental', '$rental_order_id', 'awaiting_payment', 'sandbox', 'USD',
      3500, 3500, 'rental@example.com', repeat('2', 64),
      'marketplace-operations-rental-0001', NOW() + INTERVAL '15 minutes');
  INSERT INTO commerce_checkout_line_item(
    checkout_id, line_number, product_type, product_id, product_version,
    description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot
  ) VALUES
    ('$sale_checkout_id', 1, 'marketplace_listing', '$sale_listing_id', 'v1',
      'Sale interface', 1, 12500, 12500, 12500, '{}'::jsonb),
    ('$rental_checkout_id', 1, 'marketplace_rental_asset', '$rental_asset_id', 'rental-v1',
      'Rental console', 1, 3500, 3500, 3500, '{}'::jsonb);
  INSERT INTO marketplace_sale_order_runtime(
    order_id, checkout_id, lookup_token_hash, create_idempotency_key,
    create_request_sha256, fulfillment_method, fulfillment_status,
    recipient_name, hold_expires_at
  ) VALUES (
    '$sale_order_id', '$sale_checkout_id', repeat('3', 64),
    'marketplace-operations-sale-runtime-0001', repeat('4', 64),
    'pickup', 'on_hold', 'Sale Buyer', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO marketplace_rental_order_runtime(
    order_id, checkout_id, listing_id, asset_id, lookup_token_hash,
    create_idempotency_key, create_request_sha256, fulfillment_method,
    rental_status, deposit_status, start_date, end_date, duration_days, timezone,
    daily_rate_usd_cents, weekly_rate_usd_cents, rental_charge_usd_cents,
    security_deposit_usd_cents, late_fee_usd_cents, terms_version, terms_accepted_at,
    identity_document_type, identity_document_last4, recipient_name, recipient_phone,
    hold_expires_at
  ) VALUES (
    '$rental_order_id', '$rental_checkout_id', '$rental_listing_id', '$rental_asset_id', repeat('5', 64),
    'marketplace-operations-rental-runtime-0001', repeat('6', 64), 'pickup',
    'on_hold', 'awaiting_payment', CURRENT_DATE + 2, CURRENT_DATE + 4, 3,
    'America/Guayaquil', 1000, 6000, 3000, 500, 1000, 'rental-v1', NOW(),
    'cedula', '1234', 'Rental Buyer', '+593999999999', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_reservation_hold(
    checkout_id, resource_type, resource_id, quantity, status, expires_at
  ) VALUES
    ('$sale_checkout_id', 'marketplace_asset_sale', '$sale_asset_id', 1, 'active', NOW() + INTERVAL '15 minutes'),
    ('$rental_checkout_id', 'marketplace_asset_rental', '$rental_asset_id', 1, 'active', NOW() + INTERVAL '15 minutes');
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES
    ('$sale_attempt_id', '$sale_checkout_id', 'datafast', 'sandbox', 'capture', 'processing',
      12500, 'USD', 'ENTITY-SANDBOX', 'marketplace-operations-sale-payment-0001'),
    ('$rental_attempt_id', '$rental_checkout_id', 'paypal', 'sandbox', 'capture', 'processing',
      3500, 'USD', 'PAYPAL-SANDBOX', 'marketplace-operations-rental-payment-0001');
  INSERT INTO commerce_provider_binding(
    payment_attempt_id, provider, environment, merchant_account_ref,
    resource_type, provider_resource_id, provider_resource_path,
    merchant_reference, amount_minor, currency
  ) VALUES
    ('$sale_attempt_id', 'datafast', 'sandbox', 'ENTITY-SANDBOX',
      'payment', 'SALE-PAYMENT-0001', '/v1/checkouts/SALE/payment', '$sale_order_id', 12500, 'USD'),
    ('$rental_attempt_id', 'paypal', 'sandbox', 'PAYPAL-SANDBOX',
      'capture', 'RENTAL-CAPTURE-0001', NULL, '$rental_order_id', 3500, 'USD');
  UPDATE commerce_checkout_session
    SET status='paid', paid_minor=total_minor, paid_at=NOW()
    WHERE id IN ('$sale_checkout_id', '$rental_checkout_id');
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='commerce.marketplace_manual_deposit_settlement' AND environment='production';")" \
  "true" "Manual deposit settlement feature flag"
assert_equal "$(psql_exec -Atc "SELECT fulfillment_status FROM marketplace_sale_order_runtime WHERE order_id='$sale_order_id';")" \
  "ready_to_fulfill" "Sale starts independently fulfillable after verified payment"
assert_equal "$(psql_exec -Atc "SELECT rental_status || ':' || deposit_status FROM marketplace_rental_order_runtime WHERE order_id='$rental_order_id';")" \
  "confirmed:collected" "Rental starts confirmed with a collected deposit liability"

psql_exec -c "
  SELECT set_config('tdf.actor_type', 'customer', false);
  SELECT set_config('tdf.actor_id', 'lookup:operations-sale', false);
  INSERT INTO marketplace_customer_request(
    id, order_id, order_kind, request_type, reason, idempotency_key, request_sha256
  ) VALUES (
    '$sale_request_id', '$sale_order_id', 'sale', 'sale_cancellation',
    'Customer requested cancellation before pickup.',
    'marketplace-sale-cancellation-0001', repeat('a', 64)
  );
" >/dev/null
if psql_exec -c "UPDATE marketplace_sale_order_runtime SET fulfillment_status='picking' WHERE order_id='$sale_order_id';" >/dev/null 2>&1; then
  echo "Sale fulfillment bypassed a pending customer cancellation request" >&2
  exit 1
fi
psql_exec -c "
  SELECT set_config('tdf.actor_type', 'operator', false);
  SELECT set_config('tdf.actor_id', '101', false);
  UPDATE marketplace_customer_request
  SET status='approved', reviewed_by=101, reviewed_at=NOW(), review_notes='Approved before fulfillment.'
  WHERE id='$sale_request_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT fulfillment_status FROM marketplace_sale_order_runtime WHERE order_id='$sale_order_id';")" \
  "cancellation_requested" "Approved sale cancellation advances only fulfillment state"
assert_equal "$(psql_exec -Atc "SELECT count(*)::text FROM marketplace_customer_request_event WHERE request_id='$sale_request_id';")" \
  "2" "Sale customer request audit chain"

psql_exec -c "
  INSERT INTO marketplace_customer_request(
    id, order_id, order_kind, request_type, reason, requested_end_date,
    idempotency_key, request_sha256
  ) VALUES (
    '$extension_request_id', '$rental_order_id', 'rental', 'rental_extension',
    'Customer needs two additional rental days.', CURRENT_DATE + 6,
    'marketplace-rental-extension-0001', repeat('b', 64)
  );
" >/dev/null
if psql_exec -c "
  UPDATE marketplace_customer_request
  SET status='approved', reviewed_by=102, reviewed_at=NOW(), review_notes='Unsafe direct approval.'
  WHERE id='$extension_request_id';
" >/dev/null 2>&1; then
  echo "Rental extension was approved without availability, quote, and change-order checkout" >&2
  exit 1
fi
psql_exec -c "
  UPDATE marketplace_customer_request
  SET status='needs_quote', reviewed_by=102, reviewed_at=NOW(), review_notes='Availability and price review required.'
  WHERE id='$extension_request_id';
  UPDATE marketplace_customer_request
  SET status='rejected', reviewed_by=103, reviewed_at=NOW(), review_notes='Requested dates are unavailable.'
  WHERE id='$extension_request_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT end_date::text FROM marketplace_rental_order_runtime WHERE order_id='$rental_order_id';")" \
  "$(psql_exec -Atc 'SELECT (CURRENT_DATE + 4)::text;')" "Extension request does not mutate custody dates"

psql_exec -c "
  UPDATE marketplace_rental_order_runtime SET rental_status='ready_for_handoff' WHERE order_id='$rental_order_id';
  UPDATE marketplace_rental_order_runtime
    SET condition_out='Good; serial verified', checked_out_at=NOW(), rental_status='checked_out'
    WHERE order_id='$rental_order_id';
  UPDATE marketplace_rental_order_runtime SET rental_status='return_due' WHERE order_id='$rental_order_id';
  UPDATE marketplace_rental_order_runtime
    SET condition_in='Returned with cosmetic scratch', returned_at=NOW(), rental_status='returned_pending_inspection'
    WHERE order_id='$rental_order_id';
  UPDATE marketplace_rental_order_runtime
    SET deposit_deduction_usd_cents=100, rental_status='damage_review'
    WHERE order_id='$rental_order_id';
  UPDATE marketplace_rental_order_runtime SET rental_status='deposit_refund_due' WHERE order_id='$rental_order_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT backfill_disposition FROM marketplace_rental_deposit_ledger_backfill_report WHERE order_id='$rental_order_id';")" \
  "requires_reclassification" "Historical paid deposit dry-run remains explicit"

psql_exec -c "
  INSERT INTO marketplace_customer_request(
    id, order_id, order_kind, request_type, reason, evidence_url,
    idempotency_key, request_sha256
  ) VALUES (
    '$dispute_request_id', '$rental_order_id', 'rental', 'rental_dispute',
    'Customer disputes the documented damage deduction.', '/assets/disputes/rental-0001',
    'marketplace-rental-dispute-0001', repeat('c', 64)
  );
  UPDATE marketplace_customer_request
  SET status='approved', reviewed_by=104, reviewed_at=NOW(), review_notes='Open operational dispute for review.'
  WHERE id='$dispute_request_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT rental_status FROM marketplace_rental_order_runtime WHERE order_id='$rental_order_id';")" \
  "disputed" "Approved dispute advances rental state without claiming a chargeback"
assert_equal "$(psql_exec -Atc "SELECT status FROM commerce_checkout_session WHERE id='$rental_checkout_id';")" \
  "paid" "Operational dispute remains separate from payment state"
psql_exec -c "UPDATE marketplace_rental_order_runtime SET rental_status='deposit_refund_due' WHERE order_id='$rental_order_id';" >/dev/null

psql_exec -c "
  INSERT INTO marketplace_rental_deposit_settlement(
    id, order_id, checkout_id, currency, deposit_amount_minor,
    deduction_amount_minor, refund_amount_minor, settlement_method,
    external_reference, evidence_url, idempotency_key, request_sha256, submitted_by
  ) VALUES (
    '$settlement_id', '$rental_order_id', '$rental_checkout_id', 'USD', 500,
    100, 400, 'bank_transfer', 'BANK-REFUND-OPERATIONS-0001',
    '/assets/deposit-refunds/rental-0001', 'marketplace-rental-deposit-0001', repeat('d', 64), 200
  );
" >/dev/null
if psql_exec -c "
  UPDATE marketplace_rental_deposit_settlement
  SET status='verified', reviewed_by=200, reviewed_at=NOW(), review_notes='Self-approved refund.'
  WHERE id='$settlement_id';
" >/dev/null 2>&1; then
  echo "Rental deposit settlement allowed self-approval" >&2
  exit 1
fi
psql_exec -c "
  UPDATE marketplace_rental_deposit_settlement
  SET status='verified', reviewed_by=201, reviewed_at=NOW(), review_notes='Independent evidence review completed.'
  WHERE id='$settlement_id';
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT status || ':' || refunded_minor::text FROM commerce_checkout_session WHERE id='$rental_checkout_id';")" \
  "partially_refunded:400" "Verified manual deposit return updates checkout refund totals"
assert_equal "$(psql_exec -Atc "SELECT deposit_status FROM marketplace_rental_order_runtime WHERE order_id='$rental_order_id';")" \
  "partially_refunded" "Verified deposit settlement records the truthful terminal deposit state"
assert_equal "$(psql_exec -Atc "SELECT COALESCE(sum(entry.amount_minor), 0)::text FROM commerce_ledger_entry entry JOIN commerce_ledger_transaction transaction ON transaction.id=entry.transaction_id WHERE transaction.source_type='marketplace_rental_deposit_settlement' AND transaction.source_id='$settlement_id';")" \
  "0" "Rental deposit settlement ledger balances"
assert_equal "$(psql_exec -Atc "SELECT count(*)::text FROM commerce_ledger_entry entry JOIN commerce_ledger_transaction transaction ON transaction.id=entry.transaction_id WHERE transaction.source_type='marketplace_rental_deposit_settlement' AND transaction.source_id='$settlement_id';")" \
  "3" "Rental deposit settlement posts liability, cash, and deduction entries"
assert_equal "$(psql_exec -Atc "SELECT kind || ':' || adapter || ':' || amount_minor::text FROM commerce_receipt WHERE checkout_id='$rental_checkout_id' AND external_reference='BANK-REFUND-OPERATIONS-0001';")" \
  "credit_note:bank_transfer:400" "Manual deposit settlement credit note"
assert_equal "$(psql_exec -Atc "SELECT count(*)::text FROM commerce_refund WHERE checkout_id='$rental_checkout_id';")" \
  "0" "Manual liability settlement does not fabricate a provider refund"
assert_equal "$(psql_exec -Atc "SELECT count(*)::text FROM marketplace_rental_deposit_settlement_event WHERE settlement_id='$settlement_id';")" \
  "2" "Rental deposit settlement audit chain"

if psql_exec -c "UPDATE marketplace_rental_deposit_settlement SET external_reference='REWRITTEN' WHERE id='$settlement_id';" >/dev/null 2>&1; then
  echo "Rental deposit settlement evidence was rewritten" >&2
  exit 1
fi
psql_exec -c "UPDATE marketplace_rental_order_runtime SET rental_status='closed' WHERE order_id='$rental_order_id';" >/dev/null
if apply_file tdf-hq/sql/2026-08-17_marketplace_customer_requests_and_deposit_settlement_rollback.sql; then
  echo "Marketplace operations rollback removed immutable customer or settlement evidence" >&2
  exit 1
fi

echo "Marketplace operations migration passed rerun, clean rollback, customer-request guards, extension quote gating, dispute separation, dual-control deposit settlement, balanced ledger, credit-note, and evidence-preserving rollback checks."
