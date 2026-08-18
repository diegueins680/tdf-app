#!/bin/sh
set -eu

TDF_BOOKING_CONTAINER="tdf-service-booking-migration-$$"
TDF_BOOKING_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_BOOKING_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_BOOKING_CONTAINER" \
  -e POSTGRES_PASSWORD=service-booking-test \
  -e POSTGRES_DB=tdf_service_booking_test \
  postgres:17-alpine >/dev/null

attempt=0
until docker exec "$TDF_BOOKING_CONTAINER" \
  psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_booking_test -Atqc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Service booking migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_BOOKING_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_booking_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_BOOKING_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_booking_test \
    < "$TDF_BOOKING_ROOT/$1" >/dev/null
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

create_base_schema() {
  psql_exec -c "
    CREATE EXTENSION IF NOT EXISTS pgcrypto;
    CREATE TABLE currency_reference (
      id UUID PRIMARY KEY, code TEXT NOT NULL, active BOOLEAN NOT NULL
    );
    CREATE TABLE tax_rate_reference (
      id UUID PRIMARY KEY, rate_bps INTEGER NOT NULL, active BOOLEAN NOT NULL
    );
    CREATE TABLE service_catalog (
      id BIGINT PRIMARY KEY, name TEXT NOT NULL, kind TEXT NOT NULL
    );
    CREATE TABLE party (
      id BIGINT PRIMARY KEY
    );
    CREATE TABLE service_offering (
      id UUID PRIMARY KEY, legacy_service_catalog_id BIGINT,
      code TEXT NOT NULL, default_rate_cents INTEGER, currency_id UUID NOT NULL,
      tax_rate_id UUID, default_duration_minutes INTEGER, active BOOLEAN NOT NULL,
      deprecated_at TIMESTAMPTZ, version INTEGER NOT NULL
    );
    CREATE TABLE service_order (
      id BIGSERIAL PRIMARY KEY, customer_id BIGINT NOT NULL, artist_id BIGINT,
      catalog_id BIGINT NOT NULL, service_offering_id UUID, service_kind TEXT NOT NULL,
      title TEXT, description TEXT, status TEXT NOT NULL, price_quoted_cents INTEGER,
      quote_sent_at TIMESTAMPTZ, scheduled_start TIMESTAMPTZ,
      scheduled_end TIMESTAMPTZ, created_at TIMESTAMPTZ NOT NULL
    );
    CREATE TABLE resource (
      id BIGSERIAL PRIMARY KEY, name TEXT NOT NULL, slug TEXT NOT NULL,
      resource_type TEXT NOT NULL, active BOOLEAN NOT NULL
    );
    CREATE TABLE booking (
      id BIGSERIAL PRIMARY KEY, title TEXT NOT NULL, service_order_id BIGINT,
      party_id BIGINT, service_type TEXT, service_offering_id UUID,
      starts_at TIMESTAMPTZ NOT NULL, ends_at TIMESTAMPTZ NOT NULL,
      status TEXT NOT NULL, created_by BIGINT, notes TEXT,
      created_at TIMESTAMPTZ NOT NULL, engineer_party_id BIGINT, engineer_name TEXT
    );
    CREATE TABLE booking_resource (
      id BIGSERIAL PRIMARY KEY, booking_id BIGINT NOT NULL REFERENCES booking(id),
      resource_id BIGINT NOT NULL REFERENCES resource(id), role TEXT NOT NULL,
      UNIQUE(booking_id, resource_id, role)
    );
  " >/dev/null
}

create_base_schema
apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-16_service_booking_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-16_service_booking_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_provider_actions.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_provider_actions.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments.sql

currency_id="c1000000-0000-4000-8000-000000000001"
tax_id="c2000000-0000-4000-8000-000000000001"
offering_id="c3000000-0000-4000-8000-000000000001"
policy_id="c4000000-0000-4000-8000-000000000001"
checkout_id="c5000000-0000-4000-8000-000000000001"
attempt_id="c6000000-0000-4000-8000-000000000001"
binding_id="c7000000-0000-4000-8000-000000000001"
manual_checkout_id="c5000000-0000-4000-8000-000000000003"
manual_attempt_id="c6000000-0000-4000-8000-000000000003"
manual_evidence_id="c8000000-0000-4000-8000-000000000003"

psql_exec -c "
  INSERT INTO currency_reference VALUES ('$currency_id', 'USD', TRUE);
  INSERT INTO tax_rate_reference VALUES ('$tax_id', 1200, TRUE);
  INSERT INTO party(id) VALUES (101), (102), (103), (201), (202);
  INSERT INTO service_catalog VALUES (1, 'Studio recording', 'Recording');
  INSERT INTO service_offering(
    id, legacy_service_catalog_id, code, default_rate_cents, currency_id,
    tax_rate_id, default_duration_minutes, active, version
  ) VALUES ('$offering_id', 1, 'recording', 2500, '$currency_id', '$tax_id', 60, TRUE, 7);
" >/dev/null

# Re-run once data exists so the migration's catalog-preserving draft seed is exercised.
apply_file tdf-hq/sql/2026-08-16_service_booking_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_provider_actions.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments.sql

assert_equal "$(psql_exec -Atc "SELECT approval_status || ':' || active::text || ':' || rate_minor::text || ':' || tax_bps::text FROM service_booking_commerce_policy WHERE service_offering_id='$offering_id';")" \
  "draft:false:2500:1200" "Catalog-preserving draft policy"
assert_equal "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='commerce.service_bookings' AND environment='production';")" \
  "false" "Production service booking gate"
assert_equal "$(psql_exec -Atc "SELECT string_agg(flag_key || ':' || enabled::text, ',' ORDER BY flag_key) FROM revenue_feature_flag WHERE flag_key IN ('checkout.bank_transfer','checkout.cash','checkout.pos') AND environment='production';")" \
  "checkout.bank_transfer:true,checkout.cash:true,checkout.pos:true" "Manual settlement capability flags"

# Customer evidence is never payment. It must bind an immutable manual attempt,
# then pass an independent two-step staff review.
psql_exec -c "
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$manual_checkout_id', 'service_booking', '4', 'awaiting_payment', 'sandbox', 'USD',
    2800, 2800, 'manual@example.com', repeat('7',64),
    'service-booking-manual-0001', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$manual_attempt_id', '$manual_checkout_id', 'bank_transfer', 'sandbox',
    'manual_verify', 'requires_review', 2800, 'USD', 'tdf-manual-settlement',
    'service-booking-manual-attempt-0001'
  );
  INSERT INTO commerce_manual_payment_evidence(id, checkout_id, payment_attempt_id, status)
    VALUES ('$manual_evidence_id', '$manual_checkout_id', '$manual_attempt_id', 'awaiting_evidence');
" >/dev/null

if psql_exec -c "UPDATE commerce_manual_payment_evidence SET status='approved', submitted_amount_minor=2800, currency='USD', submitted_by=201, reviewed_by=202, reviewed_at=NOW(), review_notes='fabricated direct approval' WHERE id='$manual_evidence_id';" >/dev/null 2>&1; then
  echo "Manual evidence skipped required submission and review transitions" >&2
  exit 1
fi

psql_exec -c "
  UPDATE commerce_manual_payment_evidence
    SET status='submitted', customer_reference='BANK-TEST-001',
        submitted_amount_minor=2800, currency='USD', submitted_by=201, submitted_at=NOW()
    WHERE id='$manual_evidence_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT status FROM commerce_checkout_session WHERE id='$manual_checkout_id';")" \
  "awaiting_payment" "Evidence submission is not payment"

if psql_exec -c "UPDATE commerce_manual_payment_evidence SET status='under_review', reviewed_by=201, review_notes='self review' WHERE id='$manual_evidence_id';" >/dev/null 2>&1; then
  echo "Manual evidence allowed its submitter to review it" >&2
  exit 1
fi

psql_exec -c "
  UPDATE commerce_manual_payment_evidence
    SET status='under_review', reviewed_by=202, review_notes='Reference matched bank statement.'
    WHERE id='$manual_evidence_id';
  UPDATE commerce_manual_payment_evidence
    SET status='approved', reviewed_at=NOW(), review_notes='Reference matched bank statement.'
    WHERE id='$manual_evidence_id';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT status || ':' || classification FROM commerce_manual_payment_evidence_review_report WHERE evidence_id='$manual_evidence_id';")" \
  "approved:canonical" "Independent manual evidence approval"
assert_equal "$(psql_exec -Atc "SELECT status FROM commerce_checkout_session WHERE id='$manual_checkout_id';")" \
  "awaiting_payment" "Database evidence approval alone is not payment verification"

psql_exec -c "
  UPDATE service_booking_commerce_policy
    SET id='$policy_id', policy_version='studio-approved-v1', approval_status='approved',
        active=TRUE, approved_at=NOW(), approved_by='migration-test',
        terms_version='studio-terms-v1', terms_summary='Approved test policy.'
    WHERE service_offering_id='$offering_id';
  INSERT INTO resource(id, name, slug, resource_type, active)
    VALUES (1, 'Studio A', 'studio-a', 'Room', TRUE);
  INSERT INTO service_order(
    id, customer_id, catalog_id, service_offering_id, service_kind, title,
    status, price_quoted_cents, quote_sent_at, scheduled_start, scheduled_end, created_at
  ) VALUES (
    1, 101, 1, '$offering_id', 'Recording', 'Studio session', 'deposit_due', 5600,
    NOW(), '2030-01-10 15:00:00+00', '2030-01-10 17:00:00+00', NOW()
  );
  INSERT INTO booking(
    id, title, service_order_id, party_id, service_offering_id, starts_at,
    ends_at, status, created_at
  ) VALUES (
    1, 'Studio session', 1, 101, '$offering_id', '2030-01-10 15:00:00+00',
    '2030-01-10 17:00:00+00', 'Tentative', NOW()
  );
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_id', 'service_booking', '1', 'awaiting_payment', 'sandbox', 'USD',
    2800, 2800, 'booking@example.com', repeat('1',64),
    'service-booking-idempotency-0001', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_checkout_line_item(
    checkout_id, line_number, product_type, product_id, product_version,
    description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot
  ) VALUES (
    '$checkout_id', 1, 'service_booking_deposit', '$offering_id', 'studio-approved-v1',
    'Studio confirmation deposit', 1, 2800, 2800, 2800, '{}'::jsonb
  );
  INSERT INTO service_booking_checkout_runtime(
    booking_id, service_order_id, checkout_id, service_offering_id, policy_id,
    policy_version, lookup_token_hash, create_idempotency_key, create_request_sha256,
    fulfillment_status, deposit_status, balance_status, starts_at, ends_at, timezone,
    duration_minutes, currency, rate_minor, rate_unit_minutes, tax_bps, deposit_bps,
    subtotal_minor, tax_minor, total_minor, deposit_minor, balance_minor,
    terms_version, terms_accepted_at, hold_expires_at
  ) VALUES (
    1, 1, '$checkout_id', '$offering_id', '$policy_id', 'studio-approved-v1',
    repeat('2',64), 'service-booking-runtime-0001', repeat('3',64),
    'on_hold', 'awaiting_payment', 'not_due', '2030-01-10 15:00:00+00',
    '2030-01-10 17:00:00+00', 'America/Guayaquil', 120, 'USD', 2500, 60,
    1200, 5000, 5000, 600, 5600, 2800, 2800, 'studio-terms-v1', NOW(),
    NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO booking_resource(booking_id, resource_id, role) VALUES (1, 1, 'primary');
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT allocation_status FROM service_booking_resource_allocation WHERE booking_id=1;")" \
  "holding" "Canonical booking allocation hold"

if psql_exec -c "UPDATE commerce_checkout_session SET status='paid', paid_minor=total_minor WHERE id='$checkout_id';" >/dev/null 2>&1; then
  echo "Service booking checkout accepted fabricated paid state" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$attempt_id', '$checkout_id', 'datafast', 'sandbox', 'create', 'succeeded',
    2800, 'USD', 'test-merchant', 'service-booking-capture-0001'
  );
  INSERT INTO commerce_provider_binding(
    id, payment_attempt_id, provider, environment, merchant_account_ref,
    resource_type, provider_resource_id, provider_resource_path,
    merchant_reference, amount_minor, currency
  ) VALUES (
    '$binding_id', '$attempt_id', 'datafast', 'sandbox', 'test-merchant',
    'checkout', 'test-checkout-1', '/v1/checkouts/test-checkout-1/payment', '1', 2800, 'USD'
  );
  UPDATE commerce_checkout_session
    SET status='paid', paid_minor=total_minor, paid_at=NOW()
    WHERE id='$checkout_id';
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT runtime.fulfillment_status || ':' || runtime.deposit_status || ':' || booked.status || ':' || service.status || ':' || allocation.allocation_status FROM service_booking_checkout_runtime runtime JOIN booking booked ON booked.id=runtime.booking_id JOIN service_order service ON service.id=runtime.service_order_id JOIN service_booking_resource_allocation allocation ON allocation.booking_id=runtime.booking_id WHERE runtime.booking_id=1;")" \
  "confirmed:paid:Confirmed:deposit_paid:reserved" "Verified deposit and separate fulfillment transition"

psql_exec -c "
  INSERT INTO booking(
    id, title, party_id, service_offering_id, starts_at, ends_at, status, created_at
  ) VALUES (
    2, 'Conflicting legacy request', 102, '$offering_id',
    '2030-01-10 16:00:00+00', '2030-01-10 18:00:00+00', 'Tentative', NOW()
  );
" >/dev/null
if psql_exec -c "INSERT INTO booking_resource(booking_id, resource_id, role) VALUES (2, 1, 'primary');" >/dev/null 2>&1; then
  echo "Overlapping legacy booking bypassed the atomic resource calendar" >&2
  exit 1
fi

if psql_exec -c "UPDATE service_booking_checkout_runtime SET fulfillment_status='completed' WHERE booking_id=1;" >/dev/null 2>&1; then
  echo "Service booking skipped required fulfillment transitions" >&2
  exit 1
fi

psql_exec -c "
  UPDATE service_booking_checkout_runtime SET fulfillment_status='scheduled' WHERE booking_id=1;
  UPDATE service_booking_checkout_runtime SET fulfillment_status='in_progress' WHERE booking_id=1;
  UPDATE service_booking_checkout_runtime SET fulfillment_status='balance_due', balance_status='due' WHERE booking_id=1;
  UPDATE service_booking_checkout_runtime SET fulfillment_status='completed', balance_status='paid' WHERE booking_id=1;
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT fulfillment_status || ':' || allocation_status FROM service_booking_checkout_runtime JOIN service_booking_resource_allocation USING (booking_id) WHERE booking_id=1;")" \
  "completed:completed" "Service completion and calendar release"

# A failed rail remains retryable during the hold, but must not retain a room
# forever after the immutable deadline.
psql_exec -c "
  INSERT INTO resource(id, name, slug, resource_type, active)
    VALUES (2, 'Studio B', 'studio-b', 'Room', TRUE);
  INSERT INTO service_order(
    id, customer_id, catalog_id, service_offering_id, service_kind, title,
    status, price_quoted_cents, quote_sent_at, scheduled_start, scheduled_end, created_at
  ) VALUES (
    3, 103, 1, '$offering_id', 'Recording', 'Expired failed rail', 'deposit_due', 5600,
    NOW() - INTERVAL '2 hours', '2030-02-10 15:00:00+00', '2030-02-10 17:00:00+00',
    NOW() - INTERVAL '2 hours'
  );
  INSERT INTO booking(
    id, title, service_order_id, party_id, service_offering_id, starts_at,
    ends_at, status, created_at
  ) VALUES (
    3, 'Expired failed rail', 3, 103, '$offering_id', '2030-02-10 15:00:00+00',
    '2030-02-10 17:00:00+00', 'Tentative', NOW() - INTERVAL '2 hours'
  );
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at, created_at, updated_at
  ) VALUES (
    'c5000000-0000-4000-8000-000000000002', 'service_booking', '3', 'failed',
    'sandbox', 'USD', 2800, 2800, 'failed@example.com', repeat('4',64),
    'service-booking-idempotency-0002', NOW() - INTERVAL '1 hour',
    NOW() - INTERVAL '2 hours', NOW() - INTERVAL '1 hour'
  );
  INSERT INTO service_booking_checkout_runtime(
    booking_id, service_order_id, checkout_id, service_offering_id, policy_id,
    policy_version, lookup_token_hash, create_idempotency_key, create_request_sha256,
    fulfillment_status, deposit_status, balance_status, starts_at, ends_at, timezone,
    duration_minutes, currency, rate_minor, rate_unit_minutes, tax_bps, deposit_bps,
    subtotal_minor, tax_minor, total_minor, deposit_minor, balance_minor,
    terms_version, terms_accepted_at, hold_expires_at, created_at, updated_at
  ) VALUES (
    3, 3, 'c5000000-0000-4000-8000-000000000002', '$offering_id', '$policy_id',
    'studio-approved-v1', repeat('5',64), 'service-booking-runtime-0002', repeat('6',64),
    'on_hold', 'awaiting_payment', 'not_due', '2030-02-10 15:00:00+00',
    '2030-02-10 17:00:00+00', 'America/Guayaquil', 120, 'USD', 2500, 60,
    1200, 5000, 5000, 600, 5600, 2800, 2800, 'studio-terms-v1',
    NOW() - INTERVAL '2 hours', NOW() - INTERVAL '1 hour',
    NOW() - INTERVAL '2 hours', NOW() - INTERVAL '1 hour'
  );
  INSERT INTO booking_resource(booking_id, resource_id, role) VALUES (3, 2, 'primary');
  SELECT service_booking_expire_holds(NOW());
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT checkout.status || ':' || runtime.fulfillment_status || ':' || allocation.allocation_status FROM commerce_checkout_session checkout JOIN service_booking_checkout_runtime runtime ON runtime.checkout_id=checkout.id JOIN service_booking_resource_allocation allocation ON allocation.booking_id=runtime.booking_id WHERE runtime.booking_id=3;")" \
  "expired:expired:released" "Failed provider attempt hold expiry"

if apply_file tdf-hq/sql/2026-08-16_service_booking_checkout_runtime_rollback.sql; then
  echo "Rollback removed service booking runtime containing canonical records" >&2
  exit 1
fi
if apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments_rollback.sql; then
  echo "Rollback removed reviewed manual payment evidence" >&2
  exit 1
fi

# Prove the rollback/reapply path on a fresh schema in the same disposable DB.
psql_exec -c 'DROP SCHEMA public CASCADE; CREATE SCHEMA public;' >/dev/null
create_base_schema
apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-16_service_booking_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_provider_actions.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments_rollback.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_provider_actions_rollback.sql
apply_file tdf-hq/sql/2026-08-16_service_booking_checkout_runtime_rollback.sql
assert_equal "$(psql_exec -Atc "SELECT to_regclass('public.service_booking_checkout_runtime') IS NULL;")" \
  "t" "Empty runtime rollback"
apply_file tdf-hq/sql/2026-08-16_service_booking_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_provider_actions.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments.sql

echo "Service booking checkout runtime migration tests passed"
