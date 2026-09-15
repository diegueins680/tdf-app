#!/bin/sh
set -eu

TDF_PROVIDER_EXECUTION_CONTAINER="tdf-provider-execution-migration-$$"
TDF_PROVIDER_EXECUTION_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_PROVIDER_EXECUTION_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_PROVIDER_EXECUTION_CONTAINER" \
  -e POSTGRES_PASSWORD=provider-execution-test \
  -e POSTGRES_DB=tdf_provider_execution_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_PROVIDER_EXECUTION_CONTAINER" \
  psql -v ON_ERROR_STOP=1 -U postgres -d tdf_provider_execution_test -Atqc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Provider execution migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PROVIDER_EXECUTION_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_provider_execution_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PROVIDER_EXECUTION_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_provider_execution_test \
    < "$TDF_PROVIDER_EXECUTION_ROOT/$1" >/dev/null
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
apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime.sql
apply_file tdf-hq/sql/2026-09-09_canonical_payment_lifecycle.sql
apply_file tdf-hq/sql/2026-09-10_payment_attempt_intent_binding.sql
apply_file tdf-hq/sql/2026-09-13_provider_execution_runtime.sql
apply_file tdf-hq/sql/2026-09-13_provider_execution_runtime.sql

# An operator-owned change must survive rollback; only exact untouched seed
# rows belong to this migration.
psql_exec -c "
  UPDATE revenue_feature_flag
  SET reason='operator-reviewed sandbox notification policy'
  WHERE flag_key='checkout.placetopay.webhooks' AND environment='sandbox';
" >/dev/null

checkout_id='30000000-0000-4000-8000-000000000001'
attempt_id='30000000-0000-4000-8000-000000000002'
operation_id='30000000-0000-4000-8000-000000000003'
event_id='30000000-0000-4000-8000-000000000004'

psql_exec -c "
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_id', 'event_ticket_order', 'ticket-provider-execution',
    'awaiting_payment', 'sandbox', 'USD', 12515, 12515,
    'buyer@example.test', 'lookup-hash', 'checkout-key',
    NOW() + interval '30 minutes'
  );
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$attempt_id', '$checkout_id', 'placetopay', 'sandbox', 'create',
    'created', 12515, 'USD', 'merchant-sandbox', 'attempt-key-0001'
  );
  INSERT INTO commerce_provider_operation(
    id, payment_attempt_id, provider, environment, merchant_account_ref,
    provider_reference, operation, idempotency_key, request_sha256
  ) VALUES (
    '$operation_id', '$attempt_id', 'placetopay', 'sandbox',
    'merchant-sandbox', 'TDF-provider-reference-0001',
    'create', 'operation-key-0001', repeat('a', 64)
  );
" >/dev/null

assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM pg_constraint WHERE conrelid='commerce_provider_operation'::regclass AND conname='fk_commerce_provider_operation_account' AND contype='f' AND convalidated;")" \
  "1" \
  "Provider operation uses the canonical provider account registry"

assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM pg_constraint WHERE conrelid='commerce_provider_operation'::regclass AND conname IN ('commerce_provider_operation_provider_check','commerce_provider_operation_environment_check');")" \
  "0" \
  "Provider operation has no provider or environment string allowlist"

if psql_exec -c "
  INSERT INTO commerce_provider_operation(
    payment_attempt_id, provider, environment, merchant_account_ref,
    provider_reference, operation, idempotency_key, request_sha256
  ) VALUES (
    '$attempt_id', 'unregistered-provider', 'sandbox', 'merchant-sandbox',
    'TDF-provider-reference-invalid', 'query', 'operation-key-invalid', repeat('f', 64)
  );
" >/dev/null 2>&1; then
  echo "Provider execution accepted an account absent from the canonical registry" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_provider_event_inbox(
    id, provider, environment, merchant_account_ref, provider_event_id,
    event_type, signature_verified, evidence_type, payload_ciphertext,
    payload_sha256
  ) VALUES (
    '$event_id', 'payphone', 'production', 'merchant-production',
    'payphone-transaction-9911', 'PAYMENT_APPROVED', FALSE,
    'untrusted_callback',
    pgp_sym_encrypt_bytea(
      convert_to('{\"TransactionId\":9911}', 'UTF8'),
      '0123456789abcdef0123456789abcdef',
      'cipher-algo=aes256,compress-algo=1'
    ),
    repeat('c', 64)
  );
" >/dev/null

assert_equal \
  "$(psql_exec -Atc "SELECT evidence_type || ':' || signature_verified FROM commerce_provider_event_inbox WHERE id='$event_id';")" \
  "untrusted_callback:false" \
  "Unsigned PayPhone callback is retained only as untrusted evidence"

if psql_exec -c "
  UPDATE commerce_provider_event_inbox
  SET evidence_type='signature_verified', signature_verified=TRUE
  WHERE id='$event_id';
" >/dev/null 2>&1; then
  echo "Provider event runtime allowed callback trust evidence tampering" >&2
  exit 1
fi

if psql_exec -c "
  INSERT INTO commerce_provider_event_inbox(
    provider, environment, merchant_account_ref, provider_event_id,
    event_type, signature_verified, evidence_type, payload_ciphertext,
    payload_sha256
  ) VALUES (
    'payphone', 'production', 'merchant-production', 'invalid-trust',
    'PAYMENT_APPROVED', TRUE, 'untrusted_callback',
    decode('00', 'hex'), repeat('d', 64)
  );
" >/dev/null 2>&1; then
  echo "Provider event runtime allowed an untrusted callback to claim signature verification" >&2
  exit 1
fi

if psql_exec -c "
  INSERT INTO commerce_provider_operation(
    payment_attempt_id, provider, environment, merchant_account_ref,
    provider_reference, operation, idempotency_key, request_sha256
  ) VALUES (
    '$attempt_id', 'placetopay', 'sandbox', 'merchant-sandbox',
    'TDF-provider-reference-0001', 'create', 'operation-key-0002', repeat('b', 64)
  );
" >/dev/null 2>&1; then
  echo "Provider execution allowed a second create operation for one attempt" >&2
  exit 1
fi

psql_exec -c "
  UPDATE commerce_provider_operation
  SET status='in_flight', outcome_certainty='ambiguous',
      started_at=NOW(), updated_at=NOW()
  WHERE id='$operation_id';
  UPDATE commerce_provider_operation
  SET status='requires_customer_action', outcome_certainty='ambiguous',
      provider_resource_id='9911',
      redirect_url_ciphertext=pgp_sym_encrypt(
        'https://checkout-test.placetopay.ec/session/9911/secret-token',
        '0123456789abcdef0123456789abcdef',
        'cipher-algo=aes256,compress-algo=1'
      ), completed_at=NOW(), updated_at=NOW()
  WHERE id='$operation_id';
" >/dev/null

assert_equal \
  "$(psql_exec -Atc "SELECT pgp_sym_decrypt(redirect_url_ciphertext, '0123456789abcdef0123456789abcdef') FROM commerce_provider_operation WHERE id='$operation_id';")" \
  "https://checkout-test.placetopay.ec/session/9911/secret-token" \
  "Encrypted hosted checkout redirect round trip"

assert_equal \
  "$(psql_exec -Atc "SELECT position('secret-token'::bytea IN redirect_url_ciphertext)=0 FROM commerce_provider_operation WHERE id='$operation_id';")" \
  "t" \
  "Hosted checkout redirect is not stored in plaintext"

if psql_exec -c "UPDATE commerce_provider_operation SET provider_resource_id='tampered' WHERE id='$operation_id';" >/dev/null 2>&1; then
  echo "Provider operation allowed immutable provider resource tampering" >&2
  exit 1
fi
if psql_exec -c "UPDATE commerce_provider_operation SET provider_reference='tampered' WHERE id='$operation_id';" >/dev/null 2>&1; then
  echo "Provider operation allowed immutable merchant reference tampering" >&2
  exit 1
fi

if apply_file tdf-hq/sql/2026-09-13_provider_execution_runtime_rollback.sql 2>/dev/null; then
  echo "Provider execution rollback removed non-empty financial history" >&2
  exit 1
fi

psql_exec -c "DELETE FROM commerce_provider_operation WHERE id='$operation_id';" >/dev/null
if apply_file tdf-hq/sql/2026-09-13_provider_execution_runtime_rollback.sql 2>/dev/null; then
  echo "Provider execution rollback removed untrusted callback evidence" >&2
  exit 1
fi
# The canonical inbox is intentionally UPDATE/DELETE immutable. This database
# is disposable and isolated, so truncate is the only appropriate way to
# reach the empty rollback fixture without weakening that production guard.
psql_exec -c "TRUNCATE TABLE commerce_payment_state_history, commerce_provider_event_inbox;" >/dev/null
apply_file tdf-hq/sql/2026-09-13_provider_execution_runtime_rollback.sql
assert_equal \
  "$(psql_exec -Atc "SELECT to_regclass('commerce_provider_operation') IS NULL;")" \
  "t" \
  "Empty provider execution runtime rollback"
assert_equal \
  "$(psql_exec -Atc "SELECT reason FROM revenue_feature_flag WHERE flag_key='checkout.placetopay.webhooks' AND environment='sandbox';")" \
  "operator-reviewed sandbox notification policy" \
  "Rollback preserves operator-modified feature flags"

apply_file tdf-hq/sql/2026-09-13_provider_execution_runtime.sql
assert_equal \
  "$(psql_exec -Atc "SELECT to_regclass('commerce_provider_operation') IS NOT NULL;")" \
  "t" \
  "Provider execution runtime reapplies after rollback"

echo "Provider execution migration tests passed"
