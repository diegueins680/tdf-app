#!/bin/sh
set -eu

TDF_PAYMENT_CONTAINER="tdf-canonical-payment-migration-$$"
TDF_PAYMENT_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_PAYMENT_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_PAYMENT_CONTAINER" \
  -e POSTGRES_PASSWORD=canonical-payment-test \
  -e POSTGRES_DB=tdf_canonical_payment_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_PAYMENT_CONTAINER" \
  psql -v ON_ERROR_STOP=1 -U postgres -d tdf_canonical_payment_test -Atqc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Canonical payment migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PAYMENT_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_canonical_payment_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PAYMENT_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_canonical_payment_test \
    < "$TDF_PAYMENT_ROOT/$1" >/dev/null
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

bootstrap_schema() {
  psql_exec -c 'CREATE EXTENSION IF NOT EXISTS pgcrypto;' >/dev/null
  apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
  apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime.sql
}

bootstrap_schema
apply_file tdf-hq/sql/2026-09-09_canonical_payment_lifecycle.sql
apply_file tdf-hq/sql/2026-09-09_canonical_payment_lifecycle.sql
apply_file tdf-hq/sql/2026-09-10_payment_attempt_intent_binding.sql
apply_file tdf-hq/sql/2026-09-10_payment_attempt_intent_binding.sql

assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM commerce_provider_account WHERE enabled=FALSE AND status='disabled';")" \
  "8" \
  "Provider accounts default disabled"

if psql_exec -c "UPDATE commerce_provider_account SET enabled=TRUE WHERE provider='placetopay' AND environment='production';" >/dev/null 2>&1; then
  echo "Provider account enabled without contract, credentials and verification" >&2
  exit 1
fi

apply_file tdf-hq/sql/2026-09-10_payment_attempt_intent_binding_rollback.sql
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM information_schema.columns WHERE table_schema='public' AND table_name='commerce_payment_attempt' AND column_name='payment_intent_id';")" \
  "0" \
  "Unused payment-attempt binding rollback"
apply_file tdf-hq/sql/2026-09-09_canonical_payment_lifecycle_rollback.sql
assert_equal \
  "$(psql_exec -Atc "SELECT to_regclass('commerce_payment_intent') IS NULL;")" \
  "t" \
  "Unused lifecycle rollback"

apply_file tdf-hq/sql/2026-09-09_canonical_payment_lifecycle.sql
apply_file tdf-hq/sql/2026-09-10_payment_attempt_intent_binding.sql

checkout_id='10000000-0000-4000-8000-000000000001'
attempt_id='10000000-0000-4000-8000-000000000002'
intent_id='10000000-0000-4000-8000-000000000003'
authorization_id='10000000-0000-4000-8000-000000000004'
connected_id='10000000-0000-4000-8000-000000000005'
payout_id='10000000-0000-4000-8000-000000000006'

psql_exec -c "
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_id', 'marketplace_sale', 'sale-100', 'awaiting_payment',
    'sandbox', 'USD', 10000, 10000, 'buyer@example.test', 'lookup-hash',
    'checkout-key', NOW() + interval '30 minutes'
  );
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$attempt_id', '$checkout_id', 'placetopay', 'sandbox', 'authorize',
    'processing', 10000, 'USD', 'merchant-test', 'attempt-key'
  );
  INSERT INTO commerce_payment_intent(
    id, checkout_id, status, capture_method, provider, payment_method,
    amount_minor, currency, idempotency_key
  ) VALUES (
    '$intent_id', '$checkout_id', 'processing', 'manual', 'placetopay',
    'card', 10000, 'USD', 'intent-key'
  );
  INSERT INTO commerce_payment_authorization(
    id, payment_intent_id, payment_attempt_id, provider_authorization_id,
    status, amount_minor, currency, authorized_at
  ) VALUES (
    '$authorization_id', '$intent_id', '$attempt_id', 'AUTH-100',
    'authorized', 10000, 'USD', NOW()
  );
  UPDATE commerce_payment_attempt
  SET payment_intent_id='$intent_id'
  WHERE id='$attempt_id';
" >/dev/null

if psql_exec -c "UPDATE commerce_payment_attempt SET payment_intent_id='20000000-0000-4000-8000-000000000099' WHERE id='$attempt_id';" >/dev/null 2>&1; then
  echo "Payment attempt accepted a nonexistent canonical intent" >&2
  exit 1
fi

if psql_exec -c "UPDATE commerce_payment_authorization SET provider_authorization_id='AUTH-TAMPERED' WHERE id='$authorization_id';" >/dev/null 2>&1; then
  echo "Immutable provider authorization reference was mutable" >&2
  exit 1
fi

if psql_exec -c "UPDATE commerce_payment_intent SET captured_minor=10001 WHERE id='$intent_id';" >/dev/null 2>&1; then
  echo "Payment intent accepted a capture above its immutable total" >&2
  exit 1
fi

if psql_exec -c "
  INSERT INTO commerce_connected_account(
    id, seller_party_id, provider, environment, provider_account_id, status,
    provider_managed_funds
  ) VALUES (
    '$connected_id', 77, 'paypal', 'sandbox', 'SELLER-77', 'onboarding', FALSE
  );
" >/dev/null 2>&1; then
  echo "Connected account accepted a TDF-custodied funds model" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_connected_account(
    id, seller_party_id, provider, environment, provider_account_id, status
  ) VALUES (
    '$connected_id', 77, 'paypal', 'sandbox', 'SELLER-77', 'onboarding'
  );
" >/dev/null

if psql_exec -c "
  INSERT INTO commerce_commission(
    payment_intent_id, connected_account_id, basis_amount_minor,
    commission_minor, provider_fee_minor, tax_minor, seller_net_minor,
    currency, terms_version
  ) VALUES (
    '$intent_id', '$connected_id', 10000, 1000, 500, 0, 8501, 'USD', 'v1'
  );
" >/dev/null 2>&1; then
  echo "Commission accepted an unbalanced seller net" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_commission(
    payment_intent_id, connected_account_id, basis_amount_minor,
    commission_minor, provider_fee_minor, tax_minor, seller_net_minor,
    currency, terms_version
  ) VALUES (
    '$intent_id', '$connected_id', 10000, 1000, 500, 0, 8500, 'USD', 'v1'
  );
" >/dev/null

if psql_exec -c "
  INSERT INTO commerce_payout(
    id, connected_account_id, status, amount_minor, currency,
    idempotency_key, requested_by, approved_by, approved_at
  ) VALUES (
    '$payout_id', '$connected_id', 'approved', 8500, 'USD',
    'payout-key', 77, 77, NOW()
  );
" >/dev/null 2>&1; then
  echo "Payout dual control accepted the requester as approver" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_payout(
    id, connected_account_id, status, amount_minor, currency,
    idempotency_key, requested_by, approved_by, approved_at
  ) VALUES (
    '$payout_id', '$connected_id', 'approved', 8500, 'USD',
    'payout-key', 77, 88, NOW()
  );
  UPDATE commerce_payout
  SET status='submitted', provider_payout_id='PAYOUT-100', submitted_at=NOW()
  WHERE id='$payout_id';
" >/dev/null

if psql_exec -c "UPDATE commerce_payout SET provider_payout_id='PAYOUT-TAMPERED' WHERE id='$payout_id';" >/dev/null 2>&1; then
  echo "Bound provider payout reference was mutable" >&2
  exit 1
fi

if apply_file tdf-hq/sql/2026-09-10_payment_attempt_intent_binding_rollback.sql; then
  echo "Payment-attempt binding rollback erased canonical evidence" >&2
  exit 1
fi

assert_equal \
  "$(psql_exec -Atc "SELECT payment_intent_id FROM commerce_payment_attempt WHERE id='$attempt_id';")" \
  "$intent_id" \
  "Attempt-to-intent evidence survived refused rollback"

if apply_file tdf-hq/sql/2026-09-09_canonical_payment_lifecycle_rollback.sql; then
  echo "Lifecycle rollback erased payment or marketplace evidence" >&2
  exit 1
fi

assert_equal \
  "$(psql_exec -Atc "SELECT provider_authorization_id FROM commerce_payment_authorization WHERE id='$authorization_id';")" \
  "AUTH-100" \
  "Authorization evidence survived refused rollback"

echo "Canonical payment lifecycle migration checks passed"
