#!/bin/sh
set -eu

TDF_COURSE_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_COURSE_PGURL=${TDF_COURSE_PGURL:-}
TDF_COURSE_CONTAINER=""

cleanup() {
  if [ -n "$TDF_COURSE_CONTAINER" ]; then
    docker rm -f "$TDF_COURSE_CONTAINER" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ -z "$TDF_COURSE_PGURL" ]; then
  TDF_COURSE_CONTAINER="tdf-course-checkout-migration-$$"
  docker run --rm -d \
    --name "$TDF_COURSE_CONTAINER" \
    -e POSTGRES_PASSWORD=course-checkout-test \
    -e POSTGRES_DB=tdf_course_checkout_test \
    postgres:17-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_COURSE_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_course_checkout_test -Atqc 'SELECT 1' \
    >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 30 ]; then
      echo "Course checkout migration database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done
fi

psql_exec() {
  if [ -n "$TDF_COURSE_PGURL" ]; then
    PGOPTIONS="-c statement_timeout=10000" \
      psql "$TDF_COURSE_PGURL" -v ON_ERROR_STOP=1 "$@"
  else
    docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_COURSE_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_course_checkout_test "$@"
  fi
}

apply_file() {
  if [ -n "$TDF_COURSE_PGURL" ]; then
    PGOPTIONS="-c statement_timeout=10000" \
      psql "$TDF_COURSE_PGURL" -v ON_ERROR_STOP=1 \
      < "$TDF_COURSE_ROOT/$1" >/dev/null
  else
    docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_COURSE_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_course_checkout_test \
      < "$TDF_COURSE_ROOT/$1" >/dev/null
  fi
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

psql_exec -c "
  CREATE EXTENSION IF NOT EXISTS pgcrypto;
  CREATE TABLE party (id BIGINT PRIMARY KEY);
  CREATE TABLE course (
    id BIGSERIAL PRIMARY KEY,
    slug TEXT NOT NULL UNIQUE,
    title TEXT NOT NULL,
    price_cents INTEGER NOT NULL,
    currency TEXT NOT NULL,
    capacity INTEGER NOT NULL
  );
  CREATE TABLE course_registration (
    id BIGSERIAL PRIMARY KEY,
    course_slug TEXT NOT NULL,
    full_name TEXT,
    email TEXT,
    phone_e164 TEXT,
    source TEXT NOT NULL,
    status TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );
  INSERT INTO party(id) VALUES (101), (102);
  INSERT INTO course(id, slug, title, price_cents, currency, capacity) VALUES
    (1, 'course-one', 'Course one', 24000, 'USD', 1),
    (2, 'course-expiry', 'Course expiry', 12000, 'USD', 1);
" >/dev/null

apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-25_commerce_trigger_row_binding_compatibility.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments.sql
apply_file tdf-hq/sql/2026-08-17_course_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-17_course_checkout_runtime.sql

assert_equal "$(psql_exec -Atc "SELECT string_agg(approval_status || ':' || active::text, ',' ORDER BY course_id) FROM course_checkout_policy;")" \
  "draft:false,draft:false" "Course prices migrate only as inactive drafts"
assert_equal "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='commerce.courses' AND environment='production';")" \
  "false" "Production course checkout gate"

# A clean, unapproved installation remains reversible.
apply_file tdf-hq/sql/2026-08-17_course_checkout_runtime_rollback.sql
apply_file tdf-hq/sql/2026-08-17_course_checkout_runtime.sql

if psql_exec -c "
  UPDATE course_checkout_policy
    SET price_minor=23999, approval_status='approved', active=TRUE,
        approved_at=NOW(), approved_by='migration-test'
    WHERE course_id=1;
" >/dev/null 2>&1; then
  echo "A mismatched course price became active" >&2
  exit 1
fi

psql_exec -c "
  UPDATE course_checkout_policy
    SET policy_version='course-one-approved-v1', terms_version='course-terms-v1',
        terms_summary='Approved course test terms.',
        cancellation_policy='Approved course cancellation test policy.',
        approval_status='approved', active=TRUE,
        approved_at=NOW(), approved_by='migration-test'
    WHERE course_id=1;
  UPDATE course_checkout_policy
    SET policy_version='course-expiry-approved-v1', terms_version='course-terms-v1',
        terms_summary='Approved course test terms.',
        cancellation_policy='Approved course cancellation test policy.',
        approval_status='approved', active=TRUE,
        approved_at=NOW(), approved_by='migration-test'
    WHERE course_id=2;
" >/dev/null

policy_one="$(psql_exec -Atc "SELECT id FROM course_checkout_policy WHERE course_id=1;")"
policy_two="$(psql_exec -Atc "SELECT id FROM course_checkout_policy WHERE course_id=2;")"
checkout_one="c5100000-0000-4000-8000-000000000001"
checkout_two="c5100000-0000-4000-8000-000000000002"
checkout_expiry="c5100000-0000-4000-8000-000000000003"
attempt_one="c5200000-0000-4000-8000-000000000001"
binding_one="c5300000-0000-4000-8000-000000000001"

psql_exec -c "
  INSERT INTO course_registration(id, course_slug, full_name, email, source, status)
    VALUES (1, 'course-one', 'Buyer one', 'one@example.com', 'landing', 'pending_payment');
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_one', 'course_registration', '1', 'awaiting_payment', 'sandbox', 'USD',
    24000, 24000, 'one@example.com', repeat('1',64),
    'course-checkout-idempotency-0001', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO course_registration_checkout_runtime(
    registration_id, course_id, checkout_id, policy_id, policy_version,
    lookup_token_hash, create_idempotency_key, create_request_sha256,
    payment_schedule, currency, price_minor, tax_bps, tax_minor,
    total_minor, due_now_minor, balance_minor, terms_version,
    terms_accepted_at, hold_expires_at
  ) VALUES (
    1, 1, '$checkout_one', '$policy_one', 'course-one-approved-v1',
    repeat('2',64), 'course-runtime-idempotency-0001', repeat('3',64),
    'full', 'USD', 24000, 0, 0, 24000, 24000, 0,
    'course-terms-v1', NOW(), NOW() + INTERVAL '15 minutes'
  );
" >/dev/null

psql_exec -c "
  INSERT INTO course_registration(id, course_slug, full_name, email, source, status)
    VALUES (2, 'course-one', 'Buyer two', 'two@example.com', 'landing', 'pending_payment');
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_two', 'course_registration', '2', 'awaiting_payment', 'sandbox', 'USD',
    24000, 24000, 'two@example.com', repeat('4',64),
    'course-checkout-idempotency-0002', NOW() + INTERVAL '15 minutes'
  );
" >/dev/null

if psql_exec -c "
  INSERT INTO course_registration_checkout_runtime(
    registration_id, course_id, checkout_id, policy_id, policy_version,
    lookup_token_hash, create_idempotency_key, create_request_sha256,
    payment_schedule, currency, price_minor, tax_bps, tax_minor,
    total_minor, due_now_minor, balance_minor, terms_version,
    terms_accepted_at, hold_expires_at
  ) VALUES (
    2, 1, '$checkout_two', '$policy_one', 'course-one-approved-v1',
    repeat('5',64), 'course-runtime-idempotency-0002', repeat('6',64),
    'full', 'USD', 24000, 0, 0, 24000, 24000, 0,
    'course-terms-v1', NOW(), NOW() + INTERVAL '15 minutes'
  );
" >/dev/null 2>&1; then
  echo "Course capacity accepted two concurrent seat holds" >&2
  exit 1
fi

if psql_exec -c "UPDATE course_registration SET status='paid' WHERE id=1;" >/dev/null 2>&1; then
  echo "Canonical course registration became paid without checkout verification" >&2
  exit 1
fi
if psql_exec -c "UPDATE commerce_checkout_session SET status='paid', paid_minor=24000 WHERE id='$checkout_one';" >/dev/null 2>&1; then
  echo "Course checkout became paid without provider evidence" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status,
    amount_minor, currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$attempt_one', '$checkout_one', 'paypal', 'sandbox', 'capture', 'succeeded',
    24000, 'USD', 'PAYPAL-SANDBOX', 'course-paypal-capture-0001'
  );
  INSERT INTO commerce_provider_binding(
    id, payment_attempt_id, provider, environment, merchant_account_ref,
    resource_type, provider_resource_id, provider_resource_path,
    merchant_reference, amount_minor, currency
  ) VALUES (
    '$binding_one', '$attempt_one', 'paypal', 'sandbox', 'PAYPAL-SANDBOX',
    'capture', 'CAPTURE-COURSE-0001', '/v2/checkout/orders/ORDER-COURSE-0001/capture',
    '1', 24000, 'USD'
  );
  UPDATE commerce_checkout_session
    SET status='paid', paid_minor=24000, paid_at=NOW()
    WHERE id='$checkout_one';
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT payment_status || ':' || enrollment_status FROM course_registration_checkout_runtime WHERE registration_id=1;")" \
  "paid:enrolled" "Verified payment enrolls the held seat"
assert_equal "$(psql_exec -Atc "SELECT status FROM course_registration WHERE id=1;")" \
  "paid" "Verified checkout synchronizes legacy course status"
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM course_enrollment_event WHERE registration_id=1 AND reason_code='verified_payment';")" \
  "1" "Verified enrollment audit event"

psql_exec -c "
  INSERT INTO course_registration(id, course_slug, full_name, email, source, status)
    VALUES (3, 'course-expiry', 'Expiry buyer', 'expiry@example.com', 'landing', 'pending_payment');
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_expiry', 'course_registration', '3', 'awaiting_payment', 'sandbox', 'USD',
    12000, 12000, 'expiry@example.com', repeat('7',64),
    'course-checkout-idempotency-0003', NOW() + INTERVAL '10 minutes'
  );
  INSERT INTO course_registration_checkout_runtime(
    registration_id, course_id, checkout_id, policy_id, policy_version,
    lookup_token_hash, create_idempotency_key, create_request_sha256,
    payment_schedule, currency, price_minor, tax_bps, tax_minor,
    total_minor, due_now_minor, balance_minor, terms_version,
    terms_accepted_at, hold_expires_at
  ) VALUES (
    3, 2, '$checkout_expiry', '$policy_two', 'course-expiry-approved-v1',
    repeat('8',64), 'course-runtime-idempotency-0003', repeat('9',64),
    'full', 'USD', 12000, 0, 0, 12000, 12000, 0,
    'course-terms-v1', NOW(), NOW() + INTERVAL '10 minutes'
  );
  SELECT course_checkout_expire_holds(NOW() + INTERVAL '11 minutes');
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT status FROM commerce_checkout_session WHERE id='$checkout_expiry';")" \
  "expired" "Expired checkout releases seat hold"
assert_equal "$(psql_exec -Atc "SELECT enrollment_status FROM course_registration_checkout_runtime WHERE registration_id=3;")" \
  "expired" "Expired hold does not become enrollment"
assert_equal "$(psql_exec -Atc "SELECT status FROM course_registration WHERE id=3;")" \
  "cancelled" "Expired canonical hold closes the legacy lead"

if apply_file tdf-hq/sql/2026-08-17_course_checkout_runtime_rollback.sql; then
  echo "Course rollback removed approved policy or payment/enrollment evidence" >&2
  exit 1
fi

echo "Course checkout runtime migration passed rerun, clean rollback, inactive draft preservation, approved policy binding, atomic capacity, verified-payment gating, enrollment separation, hold expiry, audit history, and live-evidence rollback checks."
