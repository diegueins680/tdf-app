#!/bin/sh
set -eu

TDF_SERVICE_CHECKOUT_CONTAINER="tdf-service-checkout-migration-$$"
TDF_SERVICE_CHECKOUT_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_SERVICE_CHECKOUT_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_SERVICE_CHECKOUT_CONTAINER" \
  -e POSTGRES_PASSWORD=service-checkout-test \
  -e POSTGRES_DB=tdf_service_checkout_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_SERVICE_CHECKOUT_CONTAINER" pg_isready -U postgres -d tdf_service_checkout_test >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Service checkout migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

sleep 5
until docker exec "$TDF_SERVICE_CHECKOUT_CONTAINER" pg_isready -U postgres -d tdf_service_checkout_test >/dev/null 2>&1; do
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_SERVICE_CHECKOUT_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_checkout_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_SERVICE_CHECKOUT_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_service_checkout_test \
    < "$TDF_SERVICE_CHECKOUT_ROOT/$1" >/dev/null
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
psql_exec -c "CREATE OR REPLACE FUNCTION trigger_set_timestamp() RETURNS trigger LANGUAGE plpgsql AS \$\$ BEGIN NEW.updated_at = NOW(); RETURN NEW; END \$\$;" >/dev/null

apply_file tdf-hq/sql/2026-08-04_service_storefront.sql
apply_file tdf-hq/sql/2026-08-13_service_storefront_phase0_hardening.sql
apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-25_commerce_trigger_row_binding_compatibility.sql
apply_file tdf-hq/sql/2026-08-14_service_storefront_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-14_service_storefront_checkout_runtime.sql

# The rollback is reversible before runtime links exist.
apply_file tdf-hq/sql/2026-08-14_service_storefront_checkout_runtime_rollback.sql
apply_file tdf-hq/sql/2026-08-14_service_storefront_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-14_service_storefront_checkout_runtime.sql

package_id=$(psql_exec -Atc "SELECT id FROM service_storefront_package ORDER BY sort_order, id LIMIT 1;")
safe_order="91000000-0000-0000-0000-000000000001"
ambiguous_order="91000000-0000-0000-0000-000000000002"
linked_order="91000000-0000-0000-0000-000000000003"
checkout_id="92000000-0000-0000-0000-000000000001"
attempt_id="93000000-0000-0000-0000-000000000001"
second_attempt_id="93000000-0000-0000-0000-000000000002"

psql_exec -c "
  INSERT INTO service_storefront_order (
    id, order_number, buyer_name, buyer_email, package_id, service_kind, tier,
    price_usd_cents, currency, status, lookup_token_hash, create_idempotency_key,
    create_request_sha256
  ) VALUES
    ('$safe_order', 'TDF-SAFE', 'Safe Buyer', 'safe@example.com', '$package_id',
      'Mixing', 'Basic', 8000, 'USD', 'awaiting_payment', 'safe-hash',
      'safe-idempotency-key', 'safe-request-hash'),
    ('$ambiguous_order', 'TDF-PAID', 'Paid Buyer', 'paid@example.com', '$package_id',
      'Mixing', 'Basic', 8000, 'USD', 'paid', 'paid-hash',
      'paid-idempotency-key', 'paid-request-hash'),
    ('$linked_order', 'TDF-LINKED', 'Linked Buyer', 'linked@example.com', '$package_id',
      'Mixing', 'Basic', 8000, 'USD', 'awaiting_payment', 'linked-hash',
      'linked-idempotency-key', 'linked-request-hash');
  UPDATE service_storefront_order SET paid_at = NOW() WHERE id = '$ambiguous_order';
" >/dev/null

assert_equal \
  "$(psql_exec -Atc "SELECT classification FROM service_storefront_checkout_backfill_report WHERE service_order_id='$safe_order';")" \
  "safe_unpaid_candidate" \
  "Safe legacy checkout classification"
assert_equal \
  "$(psql_exec -Atc "SELECT classification FROM service_storefront_checkout_backfill_report WHERE service_order_id='$ambiguous_order';")" \
  "requires_reconciliation" \
  "Ambiguous legacy checkout classification"
assert_equal \
  "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='commerce.mixing_mastering' AND environment='production';")" \
  "false" \
  "Production mixing/mastering kill switch"

psql_exec -c "
  INSERT INTO commerce_checkout_session (
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_id', 'mixing_mastering', '$linked_order', 'awaiting_payment',
    'sandbox', 'USD', 8000, 8000, 'linked@example.com', 'canonical-linked-hash',
    'canonical-linked-idempotency', NOW() + INTERVAL '24 hours'
  );
  UPDATE service_storefront_order SET checkout_id='$checkout_id' WHERE id='$linked_order';
" >/dev/null

assert_equal \
  "$(psql_exec -Atc "SELECT classification FROM service_storefront_checkout_backfill_report WHERE service_order_id='$linked_order';")" \
  "linked" \
  "Linked checkout classification"

if psql_exec -c "UPDATE service_storefront_order SET checkout_id='$checkout_id' WHERE id='$safe_order';" >/dev/null 2>&1; then
  echo "A canonical checkout was linked to two service orders" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_payment_attempt (
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$attempt_id', '$checkout_id', 'bank_transfer', 'sandbox', 'manual_verify',
    'requires_review', 8000, 'USD', 'tdf-manual-settlement', 'manual-attempt-1'
  );
  INSERT INTO commerce_manual_payment_evidence (checkout_id, payment_attempt_id, status)
  VALUES ('$checkout_id', '$attempt_id', 'awaiting_evidence');
" >/dev/null

if psql_exec -c "INSERT INTO commerce_manual_payment_evidence (checkout_id, payment_attempt_id, status) VALUES ('$checkout_id', '$attempt_id', 'awaiting_evidence');" >/dev/null 2>&1; then
  echo "A payment attempt accepted duplicate manual evidence records" >&2
  exit 1
fi

psql_exec -c "UPDATE commerce_payment_attempt SET status='succeeded' WHERE id='$attempt_id';" >/dev/null
psql_exec -c "
  INSERT INTO commerce_payment_attempt (
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$second_attempt_id', '$checkout_id', 'paypal', 'sandbox', 'capture',
    'processing', 8000, 'USD', 'sandbox-merchant', 'second-capture-attempt'
  );
" >/dev/null
if psql_exec -c "UPDATE commerce_payment_attempt SET status='succeeded' WHERE id='$second_attempt_id';" >/dev/null 2>&1; then
  echo "A checkout accepted two succeeded payment attempts" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_reconciliation_exception (
    provider, environment, merchant_account_ref, exception_type,
    internal_reference, provider_reference, expected_amount_minor,
    actual_amount_minor, currency, status
  ) VALUES (
    'paypal', 'sandbox', 'sandbox-merchant', 'provider_verification_mismatch',
    '$linked_order', 'CAPTURE-1', 8000, 7999, 'USD', 'open'
  );
" >/dev/null
if psql_exec -c "
  INSERT INTO commerce_reconciliation_exception (
    provider, environment, merchant_account_ref, exception_type,
    internal_reference, provider_reference, expected_amount_minor,
    actual_amount_minor, currency, status
  ) VALUES (
    'paypal', 'sandbox', 'sandbox-merchant', 'provider_verification_mismatch',
    '$linked_order', 'CAPTURE-1', 8000, 7999, 'USD', 'open'
  );
" >/dev/null 2>&1; then
  echo "A provider mismatch accepted duplicate open reconciliation exceptions" >&2
  exit 1
fi

psql_exec -c "INSERT INTO commerce_receipt (checkout_id, receipt_number, kind, adapter, amount_minor, currency, issued_at) VALUES ('$checkout_id', 'TDF-RCPT-1', 'payment_receipt', 'bank_transfer', 8000, 'USD', NOW());" >/dev/null
if psql_exec -c "INSERT INTO commerce_receipt (checkout_id, receipt_number, kind, adapter, amount_minor, currency, issued_at) VALUES ('$checkout_id', 'TDF-RCPT-2', 'payment_receipt', 'bank_transfer', 8000, 'USD', NOW());" >/dev/null 2>&1; then
  echo "A checkout accepted two active payment receipts" >&2
  exit 1
fi

if apply_file tdf-hq/sql/2026-08-14_service_storefront_checkout_runtime_rollback.sql; then
  echo "Rollback removed live canonical checkout links" >&2
  exit 1
fi

echo "Service storefront checkout runtime migration passed rerun, rollback, legacy classification, link, single-success, reconciliation, evidence, and receipt checks."
