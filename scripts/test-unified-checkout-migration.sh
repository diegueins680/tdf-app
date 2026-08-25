#!/bin/sh
set -eu

TDF_CHECKOUT_MIGRATION_CONTAINER="tdf-checkout-migration-test-$$"
TDF_CHECKOUT_MIGRATION_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_CHECKOUT_MIGRATION_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_CHECKOUT_MIGRATION_CONTAINER" \
  -e POSTGRES_PASSWORD=checkout-migration-test \
  -e POSTGRES_DB=tdf_checkout_migration_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_CHECKOUT_MIGRATION_CONTAINER" pg_isready -U postgres -d tdf_checkout_migration_test >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Checkout migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

# pg_isready can observe Postgres's temporary initialization server just before
# the entrypoint restarts it. Wait through that one-time handoff, then require
# readiness from the final server process.
sleep 5
until docker exec "$TDF_CHECKOUT_MIGRATION_CONTAINER" pg_isready -U postgres -d tdf_checkout_migration_test >/dev/null 2>&1; do
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_CHECKOUT_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_checkout_migration_test "$@"
}

apply_migration() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_CHECKOUT_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_checkout_migration_test \
    < "$TDF_CHECKOUT_MIGRATION_ROOT/tdf-hq/sql/2026-08-13_unified_checkout_core.sql" >/dev/null
}

apply_compatibility() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_CHECKOUT_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_checkout_migration_test \
    < "$TDF_CHECKOUT_MIGRATION_ROOT/tdf-hq/sql/2026-08-25_commerce_trigger_row_binding_compatibility.sql" >/dev/null
}

rollback_compatibility() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_CHECKOUT_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_checkout_migration_test \
    < "$TDF_CHECKOUT_MIGRATION_ROOT/tdf-hq/sql/2026-08-25_commerce_trigger_row_binding_compatibility_rollback.sql" >/dev/null
}

rollback_migration() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_CHECKOUT_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_checkout_migration_test \
    < "$TDF_CHECKOUT_MIGRATION_ROOT/tdf-hq/sql/2026-08-13_unified_checkout_core_rollback.sql" >/dev/null
}

psql_exec -c 'CREATE EXTENSION IF NOT EXISTS pgcrypto;' >/dev/null
apply_migration
apply_compatibility
apply_compatibility

table_count=$(psql_exec -Atc "SELECT count(*) FROM information_schema.tables WHERE table_schema = 'public' AND table_name LIKE 'commerce_%';")
test "$table_count" = "18"
rollback_compatibility
legacy_binding_count=$(psql_exec -Atc "SELECT count(*) FROM pg_proc WHERE oid IN (
  'commerce_validate_payment_attempt()'::regprocedure,
  'commerce_validate_provider_binding()'::regprocedure
) AND strpos(pg_get_functiondef(oid), '%ROWTYPE') > 0;")
test "$legacy_binding_count" = "2"
rollback_migration
apply_migration
apply_compatibility

safe_binding_count=$(psql_exec -Atc "SELECT count(*) FROM pg_proc WHERE oid IN (
  'commerce_validate_payment_attempt()'::regprocedure,
  'commerce_validate_provider_binding()'::regprocedure
) AND strpos(pg_get_functiondef(oid), '%ROWTYPE') = 0;")
test "$safe_binding_count" = "2"

checkout_id="10000000-0000-0000-0000-000000000001"
payment_id="20000000-0000-0000-0000-000000000001"
ledger_id="30000000-0000-0000-0000-000000000001"

psql_exec -c "
  INSERT INTO commerce_checkout_session (
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_id', 'mixing_mastering', 'order-123', 'awaiting_payment',
    'production', 'USD', 12500, 12500, 'buyer@example.com',
    'sha256-lookup-1', 'checkout-create-1', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_checkout_line_item (
    checkout_id, line_number, product_type, product_id, product_version,
    description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot
  ) VALUES (
    '$checkout_id', 1, 'service_package', 'mastering-pro', '2026-08-13',
    'Mastering Pro', 1, 12500, 12500, 12500, '{\"song_count\":3}'::jsonb
  );
" >/dev/null

if psql_exec -c "UPDATE commerce_checkout_line_item SET total_minor = 1 WHERE checkout_id = '$checkout_id';" >/dev/null 2>&1; then
  echo "Immutable checkout snapshot accepted a mutation" >&2
  exit 1
fi

if psql_exec -c "INSERT INTO commerce_payment_attempt (checkout_id, provider, environment, operation, status, amount_minor, currency, merchant_account_ref, idempotency_key) VALUES ('$checkout_id', 'datafast', 'production', 'create', 'created', 1, 'USD', 'merchant-1', 'payment-create-bad');" >/dev/null 2>&1; then
  echo "Payment attempt accepted an amount that did not match checkout" >&2
  exit 1
fi

psql_exec -c "INSERT INTO commerce_payment_attempt (id, checkout_id, provider, environment, operation, status, amount_minor, currency, merchant_account_ref, idempotency_key) VALUES ('$payment_id', '$checkout_id', 'datafast', 'production', 'create', 'created', 12500, 'USD', 'merchant-1', 'payment-create-1');" >/dev/null

if psql_exec -c "INSERT INTO commerce_provider_binding (payment_attempt_id, provider, environment, merchant_account_ref, resource_type, provider_resource_id, merchant_reference, amount_minor, currency) VALUES ('$payment_id', 'datafast', 'sandbox', 'merchant-1', 'checkout', 'provider-resource-bad', 'order-123', 12500, 'USD');" >/dev/null 2>&1; then
  echo "Provider binding accepted the wrong environment" >&2
  exit 1
fi

psql_exec -c "INSERT INTO commerce_provider_binding (payment_attempt_id, provider, environment, merchant_account_ref, resource_type, provider_resource_id, provider_resource_path, merchant_reference, amount_minor, currency) VALUES ('$payment_id', 'datafast', 'production', 'merchant-1', 'checkout', 'provider-resource-1', '/v1/checkouts/provider-resource-1/payment', 'order-123', 12500, 'USD');" >/dev/null

if psql_exec -c "INSERT INTO commerce_provider_event_inbox (provider, environment, merchant_account_ref, provider_event_id, event_type, signature_verified, payload_ciphertext, payload_sha256) VALUES ('datafast', 'production', 'merchant-1', 'event-unsigned', 'payment.updated', FALSE, decode('00','hex'), 'sha-unsigned');" >/dev/null 2>&1; then
  echo "Production inbox accepted an unsigned provider event" >&2
  exit 1
fi

psql_exec -c "INSERT INTO commerce_provider_event_inbox (provider, environment, merchant_account_ref, provider_event_id, event_type, signature_verified, payload_ciphertext, payload_sha256) VALUES ('datafast', 'production', 'merchant-1', 'event-1', 'payment.updated', TRUE, decode('00','hex'), 'sha-1');" >/dev/null
if psql_exec -c "UPDATE commerce_provider_event_inbox SET payload_sha256 = 'tampered' WHERE provider_event_id = 'event-1';" >/dev/null 2>&1; then
  echo "Provider inbox accepted evidence mutation" >&2
  exit 1
fi
psql_exec -c "UPDATE commerce_provider_event_inbox SET processing_status = 'processed', processed_at = NOW() WHERE provider_event_id = 'event-1';" >/dev/null
if psql_exec -c "INSERT INTO commerce_provider_event_inbox (provider, environment, merchant_account_ref, provider_event_id, event_type, signature_verified, payload_ciphertext, payload_sha256) VALUES ('datafast', 'production', 'merchant-1', 'event-1', 'payment.updated', TRUE, decode('00','hex'), 'sha-1');" >/dev/null 2>&1; then
  echo "Provider inbox accepted a duplicate event" >&2
  exit 1
fi

psql_exec -c "INSERT INTO commerce_reservation_hold (checkout_id, resource_type, resource_id, starts_at, ends_at, quantity, status, expires_at) VALUES ('$checkout_id', 'studio_room', 'room-a', '2026-08-14 10:00:00+00', '2026-08-14 12:00:00+00', 1, 'active', NOW() + INTERVAL '15 minutes');" >/dev/null
if psql_exec -c "INSERT INTO commerce_reservation_hold (checkout_id, resource_type, resource_id, starts_at, ends_at, quantity, status, expires_at) VALUES ('$checkout_id', 'studio_room', 'room-a', '2026-08-14 11:00:00+00', '2026-08-14 13:00:00+00', 1, 'active', NOW() + INTERVAL '15 minutes');" >/dev/null 2>&1; then
  echo "Atomic reservation accepted an overlapping active hold" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_ledger_transaction (id, transaction_type, source_type, source_id, status, effective_at, correlation_id, created_by)
  VALUES ('$ledger_id', 'payment_capture', 'checkout', '$checkout_id', 'draft', NOW(), 'correlation-1', 'migration-test');
  INSERT INTO commerce_ledger_entry (transaction_id, account_code, currency, amount_minor)
  VALUES ('$ledger_id', 'cash.datafast', 'USD', 12500);
" >/dev/null
if psql_exec -c "UPDATE commerce_ledger_transaction SET status = 'posted' WHERE id = '$ledger_id';" >/dev/null 2>&1; then
  echo "Ledger accepted an unbalanced posting" >&2
  exit 1
fi
psql_exec -c "INSERT INTO commerce_ledger_entry (transaction_id, account_code, currency, amount_minor) VALUES ('$ledger_id', 'revenue.service', 'USD', -12500); UPDATE commerce_ledger_transaction SET status = 'posted' WHERE id = '$ledger_id';" >/dev/null
if psql_exec -c "UPDATE commerce_ledger_entry SET amount_minor = -1 WHERE transaction_id = '$ledger_id' AND account_code = 'revenue.service';" >/dev/null 2>&1; then
  echo "Posted ledger accepted a destructive correction" >&2
  exit 1
fi

if rollback_migration >/dev/null 2>&1; then
  echo "Rollback removed checkout tables after financial records existed" >&2
  exit 1
fi

echo "Unified checkout migration passed immutable checksum restoration, compatibility rollback/reapply, binding, inbox, hold, and ledger constraint checks."
