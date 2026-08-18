#!/bin/sh
set -eu

TDF_DOMO_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_DOMO_PGURL=${TDF_DOMO_PGURL:-}
TDF_DOMO_CONTAINER=""

cleanup() {
  if [ -n "$TDF_DOMO_CONTAINER" ]; then
    docker rm -f "$TDF_DOMO_CONTAINER" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ -z "$TDF_DOMO_PGURL" ]; then
  TDF_DOMO_CONTAINER="tdf-domo-quote-migration-$$"
  docker run --rm -d \
    --name "$TDF_DOMO_CONTAINER" \
    -e POSTGRES_PASSWORD=domo-quote-test \
    -e POSTGRES_DB=tdf_domo_quote_test \
    postgres:17-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_DOMO_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_domo_quote_test -Atqc 'SELECT 1' \
    >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 30 ]; then
      echo "Domo quote migration database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done
fi

psql_exec() {
  if [ -n "$TDF_DOMO_PGURL" ]; then
    PGOPTIONS="-c statement_timeout=10000" \
      psql "$TDF_DOMO_PGURL" -v ON_ERROR_STOP=1 "$@"
  else
    docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_DOMO_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_domo_quote_test "$@"
  fi
}

apply_file() {
  if [ -n "$TDF_DOMO_PGURL" ]; then
    PGOPTIONS="-c statement_timeout=10000" \
      psql "$TDF_DOMO_PGURL" -v ON_ERROR_STOP=1 \
      < "$TDF_DOMO_ROOT/$1" >/dev/null
  else
    docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_DOMO_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_domo_quote_test \
      < "$TDF_DOMO_ROOT/$1" >/dev/null
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
  INSERT INTO party(id) VALUES (1), (2), (3);
" >/dev/null

apply_file tdf-hq/sql/2026-08-13_unified_checkout_core.sql
apply_file tdf-hq/sql/2026-08-13_versioned_revenue_products.sql
apply_file tdf-hq/sql/2026-08-17_service_booking_manual_payments.sql
apply_file tdf-hq/sql/2026-08-18_domo_quote_checkout_runtime.sql

assert_equal "$(psql_exec -Atc "SELECT status FROM commerce_product_version WHERE domain_type='domo';")" \
  "pending_approval" "Domo rate card remains inactive"
assert_equal "$(psql_exec -Atc "SELECT enabled::text FROM revenue_feature_flag WHERE flag_key='commerce.domo_quotes' AND environment='production';")" \
  "false" "Production Domo quote gate"
assert_equal "$(psql_exec -Atc "SELECT pricing_rules->>'max_guests' FROM commerce_product_version WHERE domain_type='domo';")" \
  "220" "Migrated server guest limit"

apply_file tdf-hq/sql/2026-08-18_domo_quote_checkout_runtime_rollback.sql
apply_file tdf-hq/sql/2026-08-18_domo_quote_checkout_runtime.sql

psql_exec -c "
  UPDATE commerce_rate_card_review
    SET status='approved', reviewed_by=2, reviewed_at=NOW()
    WHERE product_version_id='81000000-0000-0000-0000-000000000001';
  UPDATE commerce_product_version
    SET status='active', approved_by=3, approved_at=NOW()
    WHERE id='81000000-0000-0000-0000-000000000001';
" >/dev/null

domo_id="d1000000-0000-4000-8000-000000000001"
quote_id="d2000000-0000-4000-8000-000000000001"
checkout_id="d3000000-0000-4000-8000-000000000001"
attempt_id="d4000000-0000-4000-8000-000000000001"
binding_id="d5000000-0000-4000-8000-000000000001"
rules_hash="$(psql_exec -Atc "SELECT encode(digest(convert_to(pricing_rules::text,'UTF8'),'sha256'),'hex') FROM commerce_product_version WHERE id='81000000-0000-0000-0000-000000000001';")"

psql_exec -c "
  INSERT INTO commerce_quote(
    id, domain_type, domain_subject_id, version, status, currency,
    subtotal_minor, tax_minor, total_minor, deposit_minor, expires_at
  ) VALUES (
    '$quote_id', 'domo_event_quote', '$domo_id', 1, 'sent', 'USD',
    448000, 53760, 501760, 200704, NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_quote_line(
    quote_id, line_number, product_type, product_id, product_version,
    description, quantity, unit_amount_minor, subtotal_minor
  ) VALUES
    ('$quote_id',1,'domo_rate_component','venue_base','legacy_public_formula-v1','Domo event base',1,180000,180000),
    ('$quote_id',2,'domo_rate_component','venue_hours','legacy_public_formula-v1','Domo venue hours',8,18000,144000),
    ('$quote_id',3,'domo_rate_component','setup_hours','legacy_public_formula-v1','Setup and teardown hours',2,7000,14000),
    ('$quote_id',4,'domo_rate_component','additional_guests','legacy_public_formula-v1','Additional guests',20,800,16000),
    ('$quote_id',5,'domo_rate_component','catering','legacy_public_formula-v1','Catering and bar',1,52000,52000),
    ('$quote_id',6,'domo_rate_component','production','legacy_public_formula-v1','Sound and lighting',1,42000,42000);
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, quote_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, lookup_token_hash,
    idempotency_key, expires_at
  ) VALUES (
    '$checkout_id', 'domo_event_quote', 'domo-quote:$domo_id', '$quote_id', 'holding', 'sandbox', 'USD',
    200704, 200704, 'domo@example.com', repeat('1',64),
    'domo-quote-idempotency-0001', NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_checkout_line_item(
    checkout_id, line_number, product_type, product_id, product_version,
    description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot
  ) VALUES (
    '$checkout_id',1,'domo_initial_deposit','legacy_public_formula',
    'legacy_public_formula-v1','Domo initial deposit',1,200704,200704,200704,'{}'::jsonb
  );
  INSERT INTO domo_event_quote_runtime(
    id, quote_id, checkout_id, product_version_id, lookup_token_hash,
    create_idempotency_key, create_request_sha256, customer_name, customer_email,
    event_type, guests, starts_at, ends_at, setup_starts_at, duration_hours,
    setup_hours, catering, production, transport, quote_status, fulfillment_status,
    currency, subtotal_minor, tax_minor, total_minor, deposit_minor, balance_minor,
    tax_basis_points, deposit_basis_points, rate_card_version,
    rate_card_rules_sha256, timezone, terms_version, hold_expires_at
  ) VALUES (
    '$domo_id','$quote_id','$checkout_id','81000000-0000-0000-0000-000000000001',
    repeat('2',64),'domo-runtime-idempotency-0001',repeat('3',64),
    'Domo Customer','domo@example.com','wedding',80,
    '2030-01-10 15:00:00+00','2030-01-10 23:00:00+00','2030-01-10 13:00:00+00',
    8,2,TRUE,TRUE,FALSE,'sent','date_held','USD',448000,53760,501760,200704,301056,
    1200,4000,'legacy_public_formula-v1','$rules_hash','America/Guayaquil','domo-terms-legacy-draft-v1',
    NOW() + INTERVAL '15 minutes'
  );
  INSERT INTO commerce_reservation_hold(
    checkout_id, resource_type, resource_id, starts_at, ends_at, quantity, status, expires_at
  ) VALUES (
    '$checkout_id','domo_venue','domo-del-pululahua',
    '2030-01-10 13:00:00+00','2030-01-10 23:00:00+00',1,'active',NOW() + INTERVAL '15 minutes'
  );
" >/dev/null

if psql_exec -c "UPDATE commerce_quote SET total_minor=1 WHERE id='$quote_id';" >/dev/null 2>&1; then
  echo "Domo quote accepted an economic mutation" >&2
  exit 1
fi
if psql_exec -c "UPDATE domo_event_quote_runtime SET quote_status='deposit_paid', fulfillment_status='date_reserved', terms_accepted_at=NOW(), deposit_paid_at=NOW() WHERE id='$domo_id';" >/dev/null 2>&1; then
  echo "Domo quote became deposit-paid without verified checkout evidence" >&2
  exit 1
fi

if psql_exec -c "
  BEGIN;
  INSERT INTO commerce_quote(
    id,domain_type,domain_subject_id,version,status,currency,
    subtotal_minor,tax_minor,total_minor,deposit_minor,expires_at
  ) VALUES (
    'd2000000-0000-4000-8000-000000000002','domo_event_quote',
    'd1000000-0000-4000-8000-000000000002',1,'sent','USD',448000,53760,501760,200704,
    NOW()+INTERVAL '15 minutes'
  );
  INSERT INTO commerce_quote_line(
    quote_id,line_number,product_type,product_id,product_version,description,
    quantity,unit_amount_minor,subtotal_minor
  ) VALUES (
    'd2000000-0000-4000-8000-000000000002',1,'domo_rate_component','all',
    'legacy_public_formula-v1','All lines',1,448000,448000
  );
  INSERT INTO commerce_checkout_session(
    id,domain_type,domain_order_id,quote_id,status,environment,currency,
    subtotal_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at
  ) VALUES (
    'd3000000-0000-4000-8000-000000000002','domo_event_quote',
    'domo-quote:d1000000-0000-4000-8000-000000000002','d2000000-0000-4000-8000-000000000002',
    'holding','sandbox','USD',200704,200704,'overlap@example.com',repeat('4',64),
    'domo-overlap-idempotency-0002',NOW()+INTERVAL '15 minutes'
  );
  INSERT INTO domo_event_quote_runtime(
    id,quote_id,checkout_id,product_version_id,lookup_token_hash,
    create_idempotency_key,create_request_sha256,customer_name,customer_email,event_type,
    guests,starts_at,ends_at,setup_starts_at,duration_hours,setup_hours,catering,
    production,transport,quote_status,fulfillment_status,currency,subtotal_minor,tax_minor,
    total_minor,deposit_minor,balance_minor,tax_basis_points,deposit_basis_points,
    rate_card_version,rate_card_rules_sha256,timezone,terms_version,hold_expires_at
  ) VALUES (
    'd1000000-0000-4000-8000-000000000002','d2000000-0000-4000-8000-000000000002',
    'd3000000-0000-4000-8000-000000000002','81000000-0000-0000-0000-000000000001',
    repeat('5',64),'domo-overlap-runtime-0002',repeat('6',64),'Overlap','overlap@example.com',
    'wedding',80,'2030-01-10 16:00:00+00','2030-01-11 00:00:00+00',
    '2030-01-10 14:00:00+00',8,2,TRUE,TRUE,FALSE,'sent','date_held','USD',448000,53760,
    501760,200704,301056,1200,4000,'legacy_public_formula-v1','$rules_hash','America/Guayaquil',
    'domo-terms-legacy-draft-v1',NOW()+INTERVAL '15 minutes'
  );
  COMMIT;
" >/dev/null 2>&1; then
  echo "Domo accepted an overlapping date hold" >&2
  exit 1
fi

psql_exec -c "
  UPDATE domo_event_quote_runtime
    SET quote_status='accepted', terms_accepted_at=NOW() WHERE id='$domo_id';
  UPDATE commerce_quote
    SET status='accepted', accepted_at=NOW(), accepted_terms_version='domo-terms-legacy-draft-v1'
    WHERE id='$quote_id';
  UPDATE domo_event_quote_runtime SET quote_status='deposit_due' WHERE id='$domo_id';
  UPDATE commerce_checkout_session SET status='awaiting_payment' WHERE id='$checkout_id';
" >/dev/null

if psql_exec -c "UPDATE commerce_checkout_session SET status='paid',paid_minor=200704,paid_at=NOW() WHERE id='$checkout_id';" >/dev/null 2>&1; then
  echo "Domo checkout became paid without provider evidence" >&2
  exit 1
fi

psql_exec -c "
  INSERT INTO commerce_payment_attempt(
    id,checkout_id,provider,environment,operation,status,amount_minor,currency,
    merchant_account_ref,idempotency_key
  ) VALUES (
    '$attempt_id','$checkout_id','paypal','sandbox','capture','succeeded',200704,'USD',
    'PAYPAL-SANDBOX','domo-paypal-capture-0001'
  );
  INSERT INTO commerce_provider_binding(
    id,payment_attempt_id,provider,environment,merchant_account_ref,resource_type,
    provider_resource_id,provider_resource_path,merchant_reference,amount_minor,currency
  ) VALUES (
    '$binding_id','$attempt_id','paypal','sandbox','PAYPAL-SANDBOX','capture',
    'CAPTURE-DOMO-0001','/v2/checkout/orders/ORDER-DOMO-0001/capture',
    'domo-quote:$domo_id',200704,'USD'
  );
  UPDATE commerce_checkout_session
    SET status='paid',paid_minor=200704,paid_at=NOW() WHERE id='$checkout_id';
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT quote_status || ':' || fulfillment_status FROM domo_event_quote_runtime WHERE id='$domo_id';")" \
  "deposit_paid:date_reserved" "Verified deposit remains separate from event completion"
assert_equal "$(psql_exec -Atc "SELECT status FROM commerce_reservation_hold WHERE checkout_id='$checkout_id';")" \
  "consumed" "Verified deposit consumes the expiring checkout hold"
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM domo_quote_state_event WHERE domo_quote_id='$domo_id' AND to_status='deposit_paid';")" \
  "1" "Verified deposit event is exact-once"

if apply_file tdf-hq/sql/2026-08-18_domo_quote_checkout_runtime_rollback.sql; then
  echo "Domo rollback removed customer quote and payment evidence" >&2
  exit 1
fi

echo "Domo quote migration passed clean rollback/reapply, inactive-rate preservation, immutable pricing, atomic date exclusion, accepted-terms gating, verified-payment evidence, exact-once deposit state, and evidence-aware rollback checks."
