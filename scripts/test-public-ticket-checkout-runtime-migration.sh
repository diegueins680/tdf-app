#!/bin/sh
set -eu

TDF_TICKET_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_TICKET_PGURL=${TDF_TICKET_PGURL:-}
TDF_TICKET_CONTAINER=""

cleanup() {
  if [ -n "$TDF_TICKET_CONTAINER" ]; then
    docker rm -f "$TDF_TICKET_CONTAINER" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ -z "$TDF_TICKET_PGURL" ]; then
  TDF_TICKET_CONTAINER="tdf-ticket-checkout-migration-$$"
  docker run --rm -d \
    --name "$TDF_TICKET_CONTAINER" \
    -e POSTGRES_PASSWORD=ticket-checkout-test \
    -e POSTGRES_DB=tdf_ticket_checkout_test \
    postgres:17-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_TICKET_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_ticket_checkout_test -Atqc 'SELECT 1' \
    >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 30 ]; then
      echo "Ticket checkout migration database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done
fi

psql_exec() {
  if [ -n "$TDF_TICKET_PGURL" ]; then
    PGOPTIONS="-c statement_timeout=10000" \
      psql "$TDF_TICKET_PGURL" -v ON_ERROR_STOP=1 "$@"
  else
    docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_TICKET_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_ticket_checkout_test "$@"
  fi
}

apply_file() {
  if [ -n "$TDF_TICKET_PGURL" ]; then
    PGOPTIONS="-c statement_timeout=10000" \
      psql "$TDF_TICKET_PGURL" -v ON_ERROR_STOP=1 \
      < "$TDF_TICKET_ROOT/$1" >/dev/null
  else
    docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_TICKET_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_ticket_checkout_test \
      < "$TDF_TICKET_ROOT/$1" >/dev/null
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
  CREATE TABLE social_event (
    id BIGSERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    start_time TIMESTAMPTZ NOT NULL,
    end_time TIMESTAMPTZ NOT NULL
  );
  CREATE TABLE event_ticket_tier (
    id BIGSERIAL PRIMARY KEY,
    event_id BIGINT NOT NULL REFERENCES social_event(id),
    code TEXT NOT NULL,
    name TEXT NOT NULL,
    price_cents INTEGER NOT NULL,
    currency TEXT NOT NULL,
    quantity_total INTEGER NOT NULL,
    quantity_sold INTEGER NOT NULL,
    is_active BOOLEAN NOT NULL,
    allow_transfers BOOLEAN NOT NULL DEFAULT TRUE,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );
  CREATE TABLE promo_code (
    id BIGSERIAL PRIMARY KEY,
    current_redemptions INTEGER NOT NULL DEFAULT 0,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );
  CREATE TABLE event_ticket_order (
    id BIGSERIAL PRIMARY KEY,
    event_id BIGINT NOT NULL REFERENCES social_event(id),
    tier_id BIGINT NOT NULL REFERENCES event_ticket_tier(id),
    buyer_name TEXT,
    buyer_email TEXT,
    quantity INTEGER NOT NULL,
    amount_cents INTEGER NOT NULL,
    currency TEXT NOT NULL,
    status TEXT NOT NULL,
    promo_code_id BIGINT REFERENCES promo_code(id),
    original_amount_cents INTEGER,
    payment_method TEXT,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
  );
  INSERT INTO social_event(id, title, start_time, end_time)
    VALUES (1, 'Owned pilot', NOW() + INTERVAL '30 days', NOW() + INTERVAL '31 days');
  INSERT INTO event_ticket_tier(
    id, event_id, code, name, price_cents, currency,
    quantity_total, quantity_sold, is_active, allow_transfers
  ) VALUES (1, 1, 'general', 'General', 2500, 'USD', 3, 0, TRUE, TRUE);
" >/dev/null

apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-25_commerce_trigger_row_binding_compatibility.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments.sql
apply_file tdf-hq/sql/2026-08-18_public_ticket_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-18_public_ticket_checkout_runtime.sql

assert_equal "$(psql_exec -Atc "SELECT approval_status || ':' || active::text FROM event_ticket_checkout_policy WHERE event_id=1;")" \
  "draft:false" "Ticket fee policy migrates only as an inactive draft"
assert_equal "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='commerce.event_tickets' AND environment='production';")" \
  "false" "Production public ticket gate"

apply_file tdf-hq/sql/2026-08-18_public_ticket_checkout_runtime_rollback.sql
apply_file tdf-hq/sql/2026-08-18_public_ticket_checkout_runtime.sql
apply_file tdf-hq/sql/2026-10-05_ticket_order_quantity_limit.sql
apply_file tdf-hq/sql/2026-10-05_ticket_order_quantity_limit.sql
assert_equal "$(psql_exec -Atc "SELECT max_tickets_per_order FROM event_ticket_checkout_policy WHERE event_id=1;")" \
  "100" "Existing policies preserve the previous quantity limit"
apply_file tdf-hq/sql/2026-10-05_ticket_order_quantity_limit_rollback.sql
apply_file tdf-hq/sql/2026-10-05_ticket_order_quantity_limit.sql

# Existing snapshots must remain intact, while the implicit legacy limit gets
# explicit corrective evidence even before any policy is subsequently edited.
legacy_history_hash="$(psql_exec -Atc "SELECT md5(string_agg(row_to_json(h)::text,'' ORDER BY id)) FROM event_ticket_checkout_policy_history h;")"
legacy_history_count="$(psql_exec -Atc "SELECT count(*) FROM event_ticket_checkout_policy_history;")"
apply_file tdf-hq/sql/2026-10-05_ticket_policy_lifecycle_history.sql
apply_file tdf-hq/sql/2026-10-05_ticket_policy_lifecycle_history.sql
assert_equal "$(psql_exec -Atc "SELECT md5(string_agg(row_to_json(h)::text,'' ORDER BY id)) FROM event_ticket_checkout_policy_history h WHERE NOT (snapshot ? 'migration_origin_history_id');")" \
  "$legacy_history_hash" "Migration preserves original history bytes and authors"
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM event_ticket_checkout_policy_history WHERE snapshot->>'max_tickets_per_order'='100' AND snapshot ? 'migration_origin_history_id';")" \
  "$legacy_history_count" "Legacy history has one explicit quantity-limit correction per original row"
if apply_file tdf-hq/sql/2026-10-05_ticket_policy_lifecycle_history_rollback.sql; then
  echo "Policy rollback discarded corrective history evidence" >&2
  exit 1
fi

psql_exec -c "
  UPDATE event_ticket_checkout_policy
    SET policy_version='owned-pilot-v1', terms_version='ticket-terms-v1',
        max_tickets_per_order=4,
        terms_summary='Approved pilot terms.', refund_policy='Approved pilot refund policy.',
        approval_status='approved', active=TRUE,
        approved_at=NOW(), approved_by='migration-test'
    WHERE event_id=1;
" >/dev/null

policy_id="$(psql_exec -Atc "SELECT id FROM event_ticket_checkout_policy WHERE event_id=1;")"
checkout_one="e5100000-0000-4000-8000-000000000001"
checkout_expiry="e5100000-0000-4000-8000-000000000002"
attempt_one="e5200000-0000-4000-8000-000000000001"
binding_one="e5300000-0000-4000-8000-000000000001"

psql_exec -c "
  UPDATE event_ticket_tier SET quantity_sold=2 WHERE id=1 AND quantity_sold <= quantity_total - 2;
  INSERT INTO event_ticket_order(
    id, event_id, tier_id, buyer_name, buyer_email, quantity, amount_cents,
    currency, status, original_amount_cents, payment_method
  ) VALUES (1, 1, 1, 'Buyer one', 'one@example.com', 2, 5100,
    'USD', 'pending', 5000, 'paypal');
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, tax_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_one', 'event_ticket_order', '1', 'awaiting_payment', 'sandbox', 'USD',
    5100, 0, 5100, 'one@example.com', repeat('1',64),
    'ticket-checkout-idempotency-0001', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO event_ticket_checkout_runtime(
    order_id, event_id, tier_id, checkout_id, policy_id, policy_version,
    lookup_token_hash, create_idempotency_key, create_request_sha256,
    quantity, currency, unit_price_minor, gross_face_value_minor,
    discount_minor, net_face_value_minor, buyer_fee_bps, buyer_fee_minor,
    organizer_fee_bps, organizer_fee_minor, tax_bps, tax_minor,
    checkout_total_minor, organizer_payable_minor, platform_fee_minor,
    terms_version, terms_accepted_at, hold_expires_at
  ) VALUES (
    1, 1, 1, '$checkout_one', '$policy_id', 'owned-pilot-v1',
    repeat('2',64), 'ticket-runtime-idempotency-0001', repeat('3',64),
    2, 'USD', 2500, 5000, 0, 5000, 200, 100, 200, 100, 0, 0,
    5100, 4900, 200, 'ticket-terms-v1', NOW(), NOW() + INTERVAL '15 minutes'
  );
" >/dev/null

if psql_exec -c "UPDATE event_ticket_order SET status='paid' WHERE id=1;" >/dev/null 2>&1; then
  echo "Canonical ticket order became paid without verified checkout" >&2
  exit 1
fi
if psql_exec -c "UPDATE commerce_checkout_session SET status='paid', paid_minor=5100 WHERE id='$checkout_one';" >/dev/null 2>&1; then
  echo "Ticket checkout became paid without provider evidence" >&2
  exit 1
fi
if psql_exec -c "UPDATE event_ticket_checkout_runtime SET fulfillment_status='issued' WHERE order_id=1;" >/dev/null 2>&1; then
  echo "Ticket runtime issued admission without verified checkout payment" >&2
  exit 1
fi
if psql_exec -c "UPDATE event_ticket_checkout_runtime SET hold_expires_at=hold_expires_at + INTERVAL '1 hour' WHERE order_id=1;" >/dev/null 2>&1; then
  echo "Ticket runtime mutated its immutable checkout snapshot" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status,
    amount_minor, currency, merchant_account_ref, idempotency_key
  ) VALUES (
    '$attempt_one', '$checkout_one', 'paypal', 'sandbox', 'capture', 'succeeded',
    5100, 'USD', 'PAYPAL-SANDBOX', 'ticket-paypal-capture-0001'
  );
  INSERT INTO commerce_provider_binding(
    id, payment_attempt_id, provider, environment, merchant_account_ref,
    resource_type, provider_resource_id, provider_resource_path,
    merchant_reference, amount_minor, currency
  ) VALUES (
    '$binding_one', '$attempt_one', 'paypal', 'sandbox', 'PAYPAL-SANDBOX',
    'capture', 'CAPTURE-TICKET-0001', '/v2/checkout/orders/ORDER-TICKET-0001/capture',
    '1', 5100, 'USD'
  );
  UPDATE commerce_checkout_session
    SET status='paid', paid_minor=5100, paid_at=NOW() WHERE id='$checkout_one';
  UPDATE event_ticket_order SET status='paid' WHERE id=1;
  UPDATE event_ticket_checkout_runtime
    SET fulfillment_status='issued', issued_at=NOW() WHERE order_id=1;
  INSERT INTO event_ticket_fulfillment_event(
    order_id, from_status, to_status, actor_type, reason_code
  ) VALUES (1, 'seat_held', 'issued', 'provider', 'verified_payment');
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT payment_status || ':' || fulfillment_status FROM event_ticket_checkout_runtime WHERE order_id=1;")" \
  "paid:issued" "Verified payment remains distinct from explicit ticket issuance"

if psql_exec -c "INSERT INTO event_ticket_fulfillment_event(order_id, from_status, to_status, actor_type, reason_code) VALUES (1, 'seat_held', 'issued', 'provider', 'duplicate_callback');" >/dev/null 2>&1; then
  echo "Duplicate provider callback recorded a second ticket issuance" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO event_ticket_checkout_rate_limit(scope, subject_hash, window_started_at)
  VALUES ('event:1:checkout', repeat('a', 64), date_trunc('hour', NOW()))
  ON CONFLICT (scope, subject_hash, window_started_at)
  DO UPDATE SET request_count=event_ticket_checkout_rate_limit.request_count + 1;
  INSERT INTO event_ticket_checkout_rate_limit(scope, subject_hash, window_started_at)
  VALUES ('event:1:checkout', repeat('a', 64), date_trunc('hour', NOW()))
  ON CONFLICT (scope, subject_hash, window_started_at)
  DO UPDATE SET request_count=event_ticket_checkout_rate_limit.request_count + 1;
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT request_count FROM event_ticket_checkout_rate_limit WHERE scope='event:1:checkout';")" \
  "2" "Guest checkout rate limiting is atomic"

psql_exec -c "
  INSERT INTO promo_code(id, current_redemptions) VALUES (1, 1);
  UPDATE event_ticket_tier SET quantity_sold=quantity_sold + 1 WHERE id=1;
  INSERT INTO event_ticket_order(
    id, event_id, tier_id, buyer_name, buyer_email, quantity, amount_cents,
    currency, status, promo_code_id, original_amount_cents, payment_method
  ) VALUES (2, 1, 1, 'Expiry buyer', 'expiry@example.com', 1, 2550,
    'USD', 'pending', 1, 2500, 'datafast');
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, tax_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_expiry', 'event_ticket_order', '2', 'awaiting_payment', 'sandbox', 'USD',
    2550, 0, 2550, 'expiry@example.com', repeat('4',64),
    'ticket-checkout-idempotency-0002', NOW() + INTERVAL '10 minutes'
  );
  INSERT INTO event_ticket_checkout_runtime(
    order_id, event_id, tier_id, checkout_id, policy_id, policy_version,
    lookup_token_hash, create_idempotency_key, create_request_sha256,
    quantity, currency, unit_price_minor, gross_face_value_minor,
    discount_minor, net_face_value_minor, buyer_fee_bps, buyer_fee_minor,
    organizer_fee_bps, organizer_fee_minor, tax_bps, tax_minor,
    checkout_total_minor, organizer_payable_minor, platform_fee_minor,
    promo_code_id, terms_version, terms_accepted_at, hold_expires_at
  ) VALUES (
    2, 1, 1, '$checkout_expiry', '$policy_id', 'owned-pilot-v1',
    repeat('5',64), 'ticket-runtime-idempotency-0002', repeat('6',64),
    1, 'USD', 2500, 2500, 0, 2500, 200, 50, 200, 50, 0, 0,
    2550, 2450, 100, 1, 'ticket-terms-v1', NOW(), NOW() + INTERVAL '10 minutes'
  );
  UPDATE event_ticket_tier SET price_cents=2600 WHERE id=1;
  SELECT event_ticket_checkout_expire_holds(NOW() + INTERVAL '11 minutes');
  SELECT event_ticket_checkout_expire_holds(NOW() + INTERVAL '12 minutes');
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT status FROM commerce_checkout_session WHERE id='$checkout_expiry';")" \
  "expired" "Expired ticket checkout"
assert_equal "$(psql_exec -Atc "SELECT fulfillment_status FROM event_ticket_checkout_runtime WHERE order_id=2;")" \
  "expired" "Expired hold remains truthful fulfillment"
assert_equal "$(psql_exec -Atc "SELECT quantity_sold FROM event_ticket_tier WHERE id=1;")" \
  "2" "Expired hold releases inventory exactly once"
assert_equal "$(psql_exec -Atc "SELECT current_redemptions FROM promo_code WHERE id=1;")" \
  "0" "Expired hold releases promotion exactly once"

apply_file tdf-hq/test/integration/ticket_order_quantity_limit.sql
apply_file tdf-hq/sql/2026-10-05_ticket_confirmation_delivery.sql
apply_file tdf-hq/sql/2026-10-05_ticket_confirmation_delivery.sql
assert_equal "$(psql_exec -Atc 'SELECT COUNT(*) FROM event_ticket_confirmation_delivery')" "0" "No historical confirmation backfill"
apply_file tdf-hq/sql/2026-10-05_ticket_confirmation_delivery_rollback.sql
apply_file tdf-hq/sql/2026-10-05_ticket_confirmation_delivery.sql
apply_file tdf-hq/sql/2026-10-05_ticket_confirmation_lease_index.sql
apply_file tdf-hq/sql/2026-10-05_ticket_confirmation_lease_index.sql
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM pg_indexes WHERE indexname='event_ticket_confirmation_expired_lease_idx' AND indexdef LIKE '%lease_expires_at%' AND indexdef LIKE '%processing%';")" "1" "Expired lease recovery has a partial processing index"
apply_file tdf-hq/sql/2026-10-05_ticket_confirmation_lease_index_rollback.sql
apply_file tdf-hq/sql/2026-10-05_ticket_confirmation_lease_index.sql
apply_file tdf-hq/test/integration/ticket_confirmation_delivery.sql
psql_exec -c 'SELECT event_ticket_queue_confirmation(1)' >/dev/null
if apply_file tdf-hq/sql/2026-10-05_ticket_confirmation_delivery_rollback.sql; then
  echo "Ticket rollback discarded confirmation delivery evidence" >&2
  exit 1
fi
if apply_file tdf-hq/sql/2026-10-05_ticket_order_quantity_limit_rollback.sql; then
  echo "Ticket rollback removed an approved quantity limit" >&2
  exit 1
fi

if apply_file tdf-hq/sql/2026-08-18_public_ticket_checkout_runtime_rollback.sql; then
  echo "Ticket rollback removed approved policy or payment/fulfillment evidence" >&2
  exit 1
fi

echo "Public ticket checkout migration passed rerun, clean rollback, inactive policy preservation, immutable price/fee binding, verified-payment gating, explicit issuance, exact hold expiry, promotion release, and evidence-aware rollback checks."
