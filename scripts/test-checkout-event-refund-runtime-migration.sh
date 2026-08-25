#!/bin/sh
set -eu

TDF_EVENT_REFUND_CONTAINER="tdf-event-refund-migration-$$"
TDF_EVENT_REFUND_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_EVENT_REFUND_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_EVENT_REFUND_CONTAINER" \
  -e POSTGRES_PASSWORD=event-refund-test \
  -e POSTGRES_DB=tdf_event_refund_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_EVENT_REFUND_CONTAINER" \
  psql -v ON_ERROR_STOP=1 -U postgres -d tdf_event_refund_test -Atqc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Checkout event/refund migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_EVENT_REFUND_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_event_refund_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_EVENT_REFUND_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_event_refund_test \
    < "$TDF_EVENT_REFUND_ROOT/$1" >/dev/null
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
apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime.sql
apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime.sql

# The runtime-only migration is reversible before it contains provider evidence.
apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime_rollback.sql
apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime.sql
apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime.sql

assert_equal \
  "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='checkout.paypal.webhooks' AND environment='production';")" \
  "false" \
  "Production PayPal webhook gate"
assert_equal \
  "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='checkout.paypal.refunds' AND environment='production';")" \
  "false" \
  "Production PayPal refund gate"
assert_equal \
  "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='checkout.datafast.refunds' AND environment='production';")" \
  "false" \
  "Production Datafast refund gate"
assert_equal \
  "$(psql_exec -Atc "SELECT COUNT(*) FROM commerce_refund_reason_code WHERE active;")" \
  "6" \
  "Configured active refund reasons"

package_id=$(psql_exec -Atc "SELECT id FROM service_storefront_package ORDER BY sort_order, id LIMIT 1;")
order_id="a1000000-0000-4000-8000-000000000001"
checkout_id="a2000000-0000-4000-8000-000000000001"
line_id="a3000000-0000-4000-8000-000000000001"
attempt_id="a4000000-0000-4000-8000-000000000001"
event_id="a5000000-0000-4000-8000-000000000001"
refund_id="a6000000-0000-4000-8000-000000000001"
over_refund_id="a6000000-0000-4000-8000-000000000002"

psql_exec -c "
  INSERT INTO service_storefront_order (
    id, order_number, buyer_name, buyer_email, package_id, service_kind, tier,
    price_usd_cents, currency, status, lookup_token_hash, create_idempotency_key,
    create_request_sha256
  ) VALUES (
    '$order_id', 'TDF-REFUND-1', 'Refund Buyer', 'refund@example.com', '$package_id',
    'Mixing', 'Basic', 8000, 'USD', 'paypal_pending', 'lookup-hash',
    'refund-order-idempotency', 'refund-order-request-hash'
  );
  INSERT INTO commerce_checkout_session (
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, paid_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at, paid_at
  ) VALUES (
    '$checkout_id', 'mixing_mastering', '$order_id', 'processing', 'sandbox', 'USD',
    8000, 8000, 0, 'refund@example.com', 'canonical-lookup-hash',
    'canonical-refund-order', NOW() + INTERVAL '24 hours', NULL
  );
  INSERT INTO commerce_checkout_line_item (
    id, checkout_id, line_number, product_type, product_id, product_version,
    description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot
  ) VALUES (
    '$line_id', '$checkout_id', 1, 'service_storefront_package', '$package_id',
    'migration-test-v1', 'Mixing package', 1, 8000, 8000, 8000, '{}'
  );
  UPDATE service_storefront_order
    SET checkout_id='$checkout_id', paypal_order_id='ORDER-1'
    WHERE id='$order_id';
  INSERT INTO commerce_payment_attempt (
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$attempt_id', '$checkout_id', 'paypal', 'sandbox', 'capture', 'processing',
    8000, 'USD', 'MERCHANT-1', 'paypal-capture-idempotency'
  );
  INSERT INTO commerce_provider_binding (
    payment_attempt_id, provider, environment, merchant_account_ref,
    resource_type, provider_resource_id, provider_resource_path,
    merchant_reference, amount_minor, currency
  ) VALUES
    ('$attempt_id', 'paypal', 'sandbox', 'MERCHANT-1', 'order', 'ORDER-1',
      '/v2/checkout/orders/ORDER-1', '$order_id', 8000, 'USD'),
    ('$attempt_id', 'paypal', 'sandbox', 'MERCHANT-1', 'capture', 'CAPTURE-1',
      '/v2/checkout/orders/ORDER-1/capture', '$order_id', 8000, 'USD');
  UPDATE commerce_payment_attempt SET status='succeeded' WHERE id='$attempt_id';
  UPDATE commerce_checkout_session
    SET status='paid', paid_minor=8000, paid_at=NOW() WHERE id='$checkout_id';
  UPDATE service_storefront_order
    SET status='paid', paypal_capture_id='CAPTURE-1', paid_at=NOW()
    WHERE id='$order_id';
" >/dev/null

if psql_exec -c "
  INSERT INTO commerce_provider_event_inbox (
    provider, environment, merchant_account_ref, provider_event_id, event_type,
    signature_verified, payload_ciphertext, payload_sha256
  ) VALUES (
    'paypal', 'production', 'MERCHANT-1', 'WH-UNVERIFIED',
    'PAYMENT.CAPTURE.COMPLETED', FALSE, decode('00','hex'), repeat('a',64)
  );
" >/dev/null 2>&1; then
  echo "Production accepted an unsigned provider event" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_provider_event_inbox (
    id, provider, environment, merchant_account_ref, provider_event_id, event_type,
    signature_verified, received_at, provider_created_at, provider_resource_id,
    payload_ciphertext, payload_sha256, processing_status
  ) VALUES (
    '$event_id', 'paypal', 'sandbox', 'MERCHANT-1', 'WH-1',
    'PAYMENT.CAPTURE.COMPLETED', TRUE, NOW(), NOW(), 'CAPTURE-1',
    pgp_sym_encrypt_bytea(convert_to('{\"id\":\"WH-1\"}','UTF8'),
      'migration-test-encryption-key-32-bytes', 'cipher-algo=aes256'),
    encode(digest('{\"id\":\"WH-1\"}','sha256'),'hex'), 'pending'
  );
" >/dev/null

if psql_exec -c "
  INSERT INTO commerce_provider_event_inbox (
    provider, environment, merchant_account_ref, provider_event_id, event_type,
    signature_verified, payload_ciphertext, payload_sha256
  ) VALUES (
    'paypal', 'sandbox', 'MERCHANT-1', 'WH-1', 'PAYMENT.CAPTURE.COMPLETED',
    TRUE, decode('00','hex'), repeat('b',64)
  );
" >/dev/null 2>&1; then
  echo "Provider inbox accepted a duplicate event ID" >&2
  exit 1
fi

if psql_exec -c "UPDATE commerce_provider_event_inbox SET payload_sha256=repeat('f',64) WHERE id='$event_id';" >/dev/null 2>&1; then
  echo "Provider inbox allowed immutable evidence mutation" >&2
  exit 1
fi

if psql_exec -c "
  INSERT INTO commerce_refund (
    checkout_id, payment_attempt_id, status, amount_minor, currency,
    reason_code, idempotency_key, requested_by
  ) VALUES (
    '$checkout_id', '$attempt_id', 'requested', 8000, 'USD',
    'service_cancelled', 'refund-idempotency-missing-provider', 101
  );
" >/dev/null 2>&1; then
  echo "Refund without immutable provider binding was accepted" >&2
  exit 1
fi

psql_exec -c "UPDATE commerce_refund_reason_code SET active=FALSE WHERE reason_code='service_cancelled';" >/dev/null
if psql_exec -c "
  INSERT INTO commerce_refund (
    checkout_id, payment_attempt_id, provider, environment, merchant_account_ref,
    status, amount_minor, currency, reason_code, idempotency_key, requested_by
  ) VALUES (
    '$checkout_id', '$attempt_id', 'paypal', 'sandbox', 'MERCHANT-1',
    'requested', 8000, 'USD', 'service_cancelled',
    'refund-idempotency-inactive-reason', 101
  );
" >/dev/null 2>&1; then
  echo "Refund with an inactive configured reason was accepted" >&2
  exit 1
fi
psql_exec -c "UPDATE commerce_refund_reason_code SET active=TRUE WHERE reason_code='service_cancelled';" >/dev/null

psql_exec -c "
  INSERT INTO commerce_refund (
    id, checkout_id, payment_attempt_id, provider, environment,
    merchant_account_ref, status, amount_minor, currency, reason_code,
    idempotency_key, requested_by
  ) VALUES (
    '$refund_id', '$checkout_id', '$attempt_id', 'paypal', 'sandbox',
    'MERCHANT-1', 'requested', 8000, 'USD', 'service_cancelled',
    'refund-idempotency-0001', 101
  );
" >/dev/null

if psql_exec -c "UPDATE commerce_refund SET status='approved', approved_by=101 WHERE id='$refund_id';" >/dev/null 2>&1; then
  echo "Refund requester approved their own request" >&2
  exit 1
fi

psql_exec -c "
  UPDATE commerce_refund SET status='approved', approved_by=202 WHERE id='$refund_id';
  UPDATE commerce_refund SET status='processing' WHERE id='$refund_id';
" >/dev/null

if psql_exec -c "UPDATE commerce_refund SET status='succeeded', provider_refund_id='REFUND-1', completed_at=NOW() WHERE id='$refund_id';" >/dev/null 2>&1; then
  echo "Refund succeeded before its immutable allocations matched" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_refund_allocation (refund_id, line_item_id, amount_minor)
  VALUES ('$refund_id', '$line_id', 8000);
  UPDATE commerce_refund
    SET status='succeeded', provider_refund_id='REFUND-1', completed_at=NOW()
    WHERE id='$refund_id';
  UPDATE commerce_checkout_session
    SET refunded_minor=8000, status='refunded' WHERE id='$checkout_id';
" >/dev/null

if psql_exec -c "UPDATE commerce_refund SET reason_code='other' WHERE id='$refund_id';" >/dev/null 2>&1; then
  echo "Refund request snapshot was mutable" >&2
  exit 1
fi
if psql_exec -c "UPDATE commerce_refund_allocation SET amount_minor=7999 WHERE refund_id='$refund_id';" >/dev/null 2>&1; then
  echo "Verified refund allocation was mutable" >&2
  exit 1
fi

if psql_exec -c "
  INSERT INTO commerce_receipt (
    checkout_id, refund_id, receipt_number, kind, adapter,
    external_reference, amount_minor, currency, issued_at
  ) VALUES (
    '$checkout_id', '$refund_id', 'TDF-CN-BAD', 'credit_note', 'paypal',
    'REFUND-1', 7999, 'USD', NOW()
  );
" >/dev/null 2>&1; then
  echo "Credit note with a mismatched refund amount was accepted" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_receipt (
    checkout_id, refund_id, receipt_number, kind, adapter,
    external_reference, amount_minor, currency, issued_at
  ) VALUES (
    '$checkout_id', '$refund_id', 'TDF-CN-1', 'credit_note', 'paypal',
    'REFUND-1', 8000, 'USD', NOW()
  );
" >/dev/null
if psql_exec -c "
  INSERT INTO commerce_receipt (
    checkout_id, refund_id, receipt_number, kind, adapter,
    external_reference, amount_minor, currency, issued_at
  ) VALUES (
    '$checkout_id', '$refund_id', 'TDF-CN-2', 'credit_note', 'paypal',
    'REFUND-1', 8000, 'USD', NOW()
  );
" >/dev/null 2>&1; then
  echo "A succeeded refund accepted two active credit notes" >&2
  exit 1
fi

psql_exec -c "
  UPDATE commerce_provider_event_inbox
    SET processing_status='processed', checkout_id='$checkout_id',
        payment_attempt_id='$attempt_id', refund_id='$refund_id', processed_at=NOW()
    WHERE id='$event_id';
" >/dev/null

assert_equal \
  "$(psql_exec -Atc "SELECT processing_status || ':' || attempt_count FROM commerce_provider_event_inbox WHERE id='$event_id';")" \
  "processed:0" \
  "Provider event terminal evidence"
assert_equal \
  "$(psql_exec -Atc "SELECT status || ':' || approved_by || ':' || provider_refund_id FROM commerce_refund WHERE id='$refund_id';")" \
  "succeeded:202:REFUND-1" \
  "Two-person verified refund evidence"

if apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime_rollback.sql; then
  echo "Rollback removed live provider/refund evidence" >&2
  exit 1
fi

echo "Checkout event/refund runtime migration passed rerun, clean rollback, production gates, immutable inbox evidence, separation of duties, allocation, credit-note, and live-evidence rollback checks."
