#!/bin/sh
set -eu

TDF_PROVIDER_OPS_CONTAINER="tdf-provider-ops-migration-$$"
TDF_PROVIDER_OPS_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_PROVIDER_OPS_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_PROVIDER_OPS_CONTAINER" \
  -e POSTGRES_PASSWORD=provider-ops-test \
  -e POSTGRES_DB=tdf_provider_ops_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_PROVIDER_OPS_CONTAINER" \
  psql -v ON_ERROR_STOP=1 -U postgres -d tdf_provider_ops_test -Atqc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Provider event operations migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PROVIDER_OPS_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_provider_ops_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_PROVIDER_OPS_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_provider_ops_test \
    < "$TDF_PROVIDER_OPS_ROOT/$1" >/dev/null
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
psql_exec -c 'CREATE TABLE party (id BIGINT PRIMARY KEY);' >/dev/null
psql_exec -c 'INSERT INTO party(id) VALUES (101);' >/dev/null
psql_exec -c "CREATE OR REPLACE FUNCTION trigger_set_timestamp() RETURNS trigger LANGUAGE plpgsql AS \$\$ BEGIN NEW.updated_at = NOW(); RETURN NEW; END \$\$;" >/dev/null

apply_file tdf-hq/sql/2026-08-04_service_storefront.sql
apply_file tdf-hq/sql/2026-08-13_service_storefront_phase0_hardening.sql
apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-14_service_storefront_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-14_checkout_event_refund_runtime.sql
apply_file tdf-hq/sql/2026-08-14_provider_event_operations.sql
apply_file tdf-hq/sql/2026-08-14_provider_event_operations.sql

# Rollback is safe before an operator action exists, and rerun remains idempotent.
apply_file tdf-hq/sql/2026-08-14_provider_event_operations_rollback.sql
apply_file tdf-hq/sql/2026-08-14_provider_event_operations.sql
apply_file tdf-hq/sql/2026-08-14_provider_event_operations.sql

assert_equal \
  "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='checkout.provider_event_worker' AND environment='sandbox';")" \
  "true" \
  "Sandbox provider event worker gate"
assert_equal \
  "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='checkout.provider_event_worker' AND environment='production';")" \
  "false" \
  "Production provider event worker gate"

event_id="b5000000-0000-4000-8000-000000000001"
psql_exec -c "
  INSERT INTO commerce_provider_event_inbox (
    id, provider, environment, merchant_account_ref, provider_event_id, event_type,
    signature_verified, received_at, provider_created_at, provider_resource_id,
    payload_ciphertext, payload_sha256, processing_status
  ) VALUES (
    '$event_id', 'paypal', 'sandbox', 'MERCHANT-OPS', 'WH-OPS-1',
    'PAYMENT.CAPTURE.COMPLETED', TRUE, NOW(), NOW(), 'CAPTURE-OPS-1',
    pgp_sym_encrypt_bytea(convert_to('{\"id\":\"WH-OPS-1\"}','UTF8'),
      'provider-operations-key-32-bytes', 'cipher-algo=aes256'),
    encode(digest('{\"id\":\"WH-OPS-1\"}','sha256'),'hex'), 'pending'
  );
" >/dev/null

if psql_exec -c "UPDATE commerce_provider_event_inbox SET processing_status='processed' WHERE id='$event_id';" >/dev/null 2>&1; then
  echo "Provider event skipped the formal processing transition" >&2
  exit 1
fi

psql_exec -c "
  UPDATE commerce_provider_event_inbox
    SET processing_status='processing', attempt_count=1, processing_started_at=NOW()
    WHERE id='$event_id';
  UPDATE commerce_provider_event_inbox
    SET processing_status='dead_letter', processing_started_at=NULL,
        error_summary='Bounded retries exhausted'
    WHERE id='$event_id';
" >/dev/null

second_event_id="b5000000-0000-4000-8000-000000000002"
psql_exec -c "
  INSERT INTO commerce_provider_event_inbox (
    id, provider, environment, merchant_account_ref, provider_event_id, event_type,
    signature_verified, received_at, provider_created_at, provider_resource_id,
    payload_ciphertext, payload_sha256, processing_status, attempt_count,
    error_summary
  ) VALUES (
    '$second_event_id', 'paypal', 'sandbox', 'MERCHANT-OPS', 'WH-OPS-2',
    'PAYMENT.CAPTURE.COMPLETED', TRUE, NOW(), NOW(), 'CAPTURE-OPS-2',
    pgp_sym_encrypt_bytea(convert_to('{\"id\":\"WH-OPS-2\"}','UTF8'),
      'provider-operations-key-32-bytes', 'cipher-algo=aes256'),
    encode(digest('{\"id\":\"WH-OPS-2\"}','sha256'),'hex'),
    'dead_letter', 1, 'Independent dead letter'
  );
" >/dev/null

if psql_exec -c "UPDATE commerce_provider_event_inbox SET processing_status='retry' WHERE id='$event_id';" >/dev/null 2>&1; then
  echo "Provider event bypassed the audited requeue function" >&2
  exit 1
fi
if psql_exec -c "SELECT commerce_requeue_provider_event(
  '$event_id', 101, 'Provider repair' || chr(10) || 'reason', NOW()
);" >/dev/null 2>&1; then
  echo "Provider event replay accepted a control character in its audit reason" >&2
  exit 1
fi

psql_exec -c "
  DO \$\$
  BEGIN
    PERFORM commerce_requeue_provider_event(
      '$event_id', 101, 'Provider configuration repaired by the operator', NOW()
    );
    BEGIN
      UPDATE commerce_provider_event_inbox
        SET processing_status='retry'
        WHERE id='$second_event_id';
      RAISE EXCEPTION 'Requeue authorization leaked to another event';
    EXCEPTION WHEN OTHERS THEN
      IF SQLERRM NOT LIKE 'Invalid provider event transition %' THEN
        RAISE;
      END IF;
    END;
  END \$\$;
" >/dev/null

assert_equal \
  "$(psql_exec -Atc "SELECT processing_status || ':' || attempt_count FROM commerce_provider_event_inbox WHERE id='$event_id';")" \
  "retry:1" \
  "Audited provider event requeue"
assert_equal \
  "$(psql_exec -Atc "SELECT action || ':' || from_status || ':' || to_status || ':' || actor_party_id FROM commerce_provider_event_action WHERE provider_event_id='$event_id';")" \
  "requeued:dead_letter:retry:101" \
  "Provider event action evidence"

if psql_exec -c "UPDATE commerce_provider_event_action SET reason='Changed later' WHERE provider_event_id='$event_id';" >/dev/null 2>&1; then
  echo "Provider event action audit allowed mutation" >&2
  exit 1
fi
if psql_exec -c "SELECT commerce_requeue_provider_event('$event_id', 101, 'Duplicate replay request is forbidden', NOW());" >/dev/null 2>&1; then
  echo "Provider event accepted a duplicate replay while already queued" >&2
  exit 1
fi
if apply_file tdf-hq/sql/2026-08-14_provider_event_operations_rollback.sql; then
  echo "Rollback removed provider event replay audit evidence" >&2
  exit 1
fi

echo "Provider event operations migration passed rerun, clean rollback, formal transitions, audited replay, duplicate prevention, production gate, immutable action, and live-evidence rollback checks."
