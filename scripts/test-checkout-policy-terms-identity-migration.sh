#!/bin/sh
# A terms_version names exactly one terms text within its course or event
# (2026-10-13_checkout_policy_terms_identity, issue #564).
set -eu

TDF_TERMS_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_TERMS_PGURL=${TDF_TERMS_PGURL:-}
TDF_TERMS_CONTAINER=""

if [ -n "$TDF_TERMS_PGURL" ]; then
  node --input-type=module - "$TDF_TERMS_PGURL" "$TDF_TERMS_ROOT/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
fi

cleanup() {
  if [ -n "$TDF_TERMS_CONTAINER" ]; then
    docker rm -f "$TDF_TERMS_CONTAINER" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ -z "$TDF_TERMS_PGURL" ]; then
  TDF_TERMS_CONTAINER="tdf-terms-identity-migration-$$"
  docker run --rm -d \
    --name "$TDF_TERMS_CONTAINER" \
    -e POSTGRES_PASSWORD=terms-identity-test \
    -e POSTGRES_DB=tdf_terms_identity_test \
    postgres:17-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_TERMS_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_terms_identity_test -Atqc 'SELECT 1' \
    >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 30 ]; then
      echo "Terms identity migration database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done
fi

psql_exec() {
  if [ -n "$TDF_TERMS_PGURL" ]; then
    PGOPTIONS="-c statement_timeout=10000" \
      psql "$TDF_TERMS_PGURL" -v ON_ERROR_STOP=1 "$@"
  else
    docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_TERMS_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_terms_identity_test "$@"
  fi
}

apply_file() {
  if [ -n "$TDF_TERMS_PGURL" ]; then
    PGOPTIONS="-c statement_timeout=10000" \
      psql "$TDF_TERMS_PGURL" -v ON_ERROR_STOP=1 \
      < "$TDF_TERMS_ROOT/$1" >/dev/null
  else
    docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_TERMS_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d tdf_terms_identity_test \
      < "$TDF_TERMS_ROOT/$1" >/dev/null
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

assert_rejected() {
  sql=$1
  label=$2
  if psql_exec -c "$sql" >/dev/null 2>&1; then
    echo "$label: statement was accepted but must be rejected" >&2
    exit 1
  fi
}

MIGRATION=tdf-hq/sql/2026-10-13_checkout_policy_terms_identity.sql
ROLLBACK=tdf-hq/sql/2026-10-13_checkout_policy_terms_identity_rollback.sql

echo "Terms identity PostgreSQL server: $(psql_exec -Atc 'SHOW server_version')"

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
  INSERT INTO course(id, slug, title, price_cents, currency, capacity)
    VALUES (1, 'course-one', 'Course one', 24000, 'USD', 5);
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
apply_file tdf-hq/sql/2026-08-17_course_checkout_runtime.sql
apply_file tdf-hq/sql/2026-08-18_public_ticket_checkout_runtime.sql
apply_file tdf-hq/sql/2026-10-05_ticket_policy_lifecycle_history.sql

# The base migrations seed one draft policy per course and event.
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM course_checkout_policy WHERE course_id=1;")" "1" "Seeded course draft policy"
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM event_ticket_checkout_policy WHERE event_id=1;")" "1" "Seeded ticket draft policy"

psql_exec -c "
  UPDATE course_checkout_policy
    SET policy_version='course-one-v1', terms_version='course-terms-v1',
        terms_summary='Course terms one.', cancellation_policy='Course cancellation one.'
    WHERE course_id=1;
  UPDATE event_ticket_checkout_policy
    SET policy_version='event-one-v1', terms_version='ticket-terms-v1',
        terms_summary='Ticket terms one.', refund_policy='Ticket refund one.'
    WHERE event_id=1;
" >/dev/null

# Existing data that already violates the invariant blocks the migration.
psql_exec -c "
  INSERT INTO course_checkout_policy(course_id, policy_version, currency, price_minor, terms_version, terms_summary, cancellation_policy)
    VALUES (1, 'course-one-v2-conflict', 'USD', 24000, 'course-terms-v1', 'Course terms one.', 'Course cancellation CHANGED.');
" >/dev/null
if apply_file "$MIGRATION" 2>/dev/null; then
  echo "Migration applied over a course policy that reuses terms_version for different text" >&2
  exit 1
fi
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM pg_trigger WHERE tgname LIKE 'trg_%_terms_identity';")" "0" "Failed precondition installs nothing"
psql_exec -c "DELETE FROM course_checkout_policy WHERE policy_version='course-one-v2-conflict';" >/dev/null

# Idempotent installation.
apply_file "$MIGRATION"
apply_file "$MIGRATION"
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM pg_trigger WHERE tgname IN ('trg_course_checkout_policy_terms_identity','trg_event_ticket_checkout_policy_terms_identity');")" \
  "2" "Both terms identity triggers installed once"

# Same terms_version with identical text (price-only replacement) is allowed.
psql_exec -c "
  INSERT INTO course_checkout_policy(course_id, policy_version, currency, price_minor, terms_version, terms_summary, cancellation_policy)
    VALUES (1, 'course-one-v2', 'USD', 26000, 'course-terms-v1', 'Course terms one.', 'Course cancellation one.');
  INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency, buyer_fee_bps, terms_version, terms_summary, refund_policy)
    VALUES (1, 'event-one-v2', 'USD', 300, 'ticket-terms-v1', 'Ticket terms one.', 'Ticket refund one.');
" >/dev/null

# Same terms_version with different text is rejected on insert and on update.
assert_rejected "
  INSERT INTO course_checkout_policy(course_id, policy_version, currency, price_minor, terms_version, terms_summary, cancellation_policy)
    VALUES (1, 'course-one-v3', 'USD', 24000, 'course-terms-v1', 'Course terms CHANGED.', 'Course cancellation one.');
" "Course policy reusing terms_version with a different summary"
assert_rejected "
  INSERT INTO course_checkout_policy(course_id, policy_version, currency, price_minor, terms_version, terms_summary, cancellation_policy)
    VALUES (1, 'course-one-v3', 'USD', 24000, 'course-terms-v1', 'Course terms one.', 'Course cancellation CHANGED.');
" "Course policy reusing terms_version with a different cancellation policy"
assert_rejected "
  UPDATE course_checkout_policy SET cancellation_policy='Course cancellation CHANGED.' WHERE policy_version='course-one-v2';
" "Draft course policy drifting from the text its terms_version names"
assert_rejected "
  INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency, terms_version, terms_summary, refund_policy)
    VALUES (1, 'event-one-v3', 'USD', 'ticket-terms-v1', 'Ticket terms one.', 'Ticket refund CHANGED.');
" "Ticket policy reusing terms_version with a different refund policy"
assert_rejected "
  UPDATE event_ticket_checkout_policy SET terms_summary='Ticket terms CHANGED.' WHERE policy_version='event-one-v2';
" "Draft ticket policy drifting from the text its terms_version names"

# A new terms_version carries new text; the old version is unaffected.
psql_exec -c "
  INSERT INTO course_checkout_policy(course_id, policy_version, currency, price_minor, terms_version, terms_summary, cancellation_policy)
    VALUES (1, 'course-one-v3', 'USD', 24000, 'course-terms-v2', 'Course terms two.', 'Course cancellation two.');
  INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency, terms_version, terms_summary, refund_policy)
    VALUES (1, 'event-one-v3', 'USD', 'ticket-terms-v2', 'Ticket terms two.', 'Ticket refund two.');
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT string_agg(terms_version, ',' ORDER BY policy_version) FROM course_checkout_policy WHERE course_id=1;")" \
  "course-terms-v1,course-terms-v1,course-terms-v2" "Course terms versions after a legitimate replacement"
assert_equal "$(psql_exec -Atc "SELECT string_agg(terms_version, ',' ORDER BY policy_version) FROM event_ticket_checkout_policy WHERE event_id=1;")" \
  "ticket-terms-v1,ticket-terms-v1,ticket-terms-v2" "Ticket terms versions after a legitimate replacement"

# Moving a row onto a version with different text is also a reuse.
assert_rejected "
  UPDATE course_checkout_policy SET terms_version='course-terms-v1' WHERE policy_version='course-one-v3';
" "Course policy renamed onto a terms_version with different text"

# Rollback removes the triggers and nothing else; the migration reinstalls.
apply_file "$ROLLBACK"
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM pg_trigger WHERE tgname LIKE 'trg_%_terms_identity';")" "0" "Rollback removes both triggers"
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM course_checkout_policy;")" "3" "Rollback keeps course policies"
apply_file "$MIGRATION"
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM pg_trigger WHERE tgname LIKE 'trg_%_terms_identity';")" "2" "Migration reinstalls after rollback"

# Reassigning only the scope must validate the target's terms identity too.
psql_exec -c "
  INSERT INTO course(id, slug, title, price_cents, currency, capacity) VALUES
    (2, 'move-source', 'Move source', 24000, 'USD', 5),
    (3, 'move-matching', 'Move matching', 24000, 'USD', 5),
    (4, 'move-empty', 'Move empty', 24000, 'USD', 5);
  INSERT INTO social_event(id, title, start_time, end_time)
    SELECT id, 'Move scope ' || id, NOW() + INTERVAL '30 days', NOW() + INTERVAL '31 days'
    FROM generate_series(2, 4) id;
  INSERT INTO course_checkout_policy(course_id, policy_version, currency, price_minor,
      terms_version, terms_summary, cancellation_policy) VALUES
    (2, 'course-move-source', 'USD', 24000, 'course-terms-v1', 'Different terms.', 'Different cancellation.'),
    (3, 'course-move-matching', 'USD', 24000, 'course-terms-v1', 'Different terms.', 'Different cancellation.');
  INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency,
      terms_version, terms_summary, refund_policy) VALUES
    (2, 'event-move-source', 'USD', 'ticket-terms-v1', 'Different terms.', 'Different refund.'),
    (3, 'event-move-matching', 'USD', 'ticket-terms-v1', 'Different terms.', 'Different refund.');
" >/dev/null
assert_rejected "
  UPDATE course_checkout_policy SET course_id=1 WHERE policy_version='course-move-source';
" "Moving a draft course policy onto conflicting terms"
assert_rejected "
  UPDATE event_ticket_checkout_policy SET event_id=1 WHERE policy_version='event-move-source';
" "Moving a draft event policy onto conflicting terms"
assert_equal "$(psql_exec -Atc "SELECT course_id FROM course_checkout_policy WHERE policy_version='course-move-source';")" \
  "2" "Rejected course move preserves its source scope"
assert_equal "$(psql_exec -Atc "SELECT event_id FROM event_ticket_checkout_policy WHERE policy_version='event-move-source';")" \
  "2" "Rejected event move preserves its source scope"
psql_exec -c "
  UPDATE course_checkout_policy SET course_id=3 WHERE policy_version='course-move-source';
  UPDATE event_ticket_checkout_policy SET event_id=3 WHERE policy_version='event-move-source';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT course_id FROM course_checkout_policy WHERE policy_version='course-move-source';")" \
  "3" "Course move to matching terms is permitted"
assert_equal "$(psql_exec -Atc "SELECT event_id FROM event_ticket_checkout_policy WHERE policy_version='event-move-source';")" \
  "3" "Event move to matching terms is permitted"
psql_exec -c "
  UPDATE course_checkout_policy SET course_id=4 WHERE policy_version='course-move-source';
  UPDATE event_ticket_checkout_policy SET event_id=4 WHERE policy_version='event-move-source';
" >/dev/null
assert_equal "$(psql_exec -Atc "SELECT course_id FROM course_checkout_policy WHERE policy_version='course-move-source';")" \
  "4" "Course move to an unused terms scope is permitted"
assert_equal "$(psql_exec -Atc "SELECT event_id FROM event_ticket_checkout_policy WHERE policy_version='event-move-source';")" \
  "4" "Event move to an unused terms scope is permitted"

echo "checkout policy terms identity migration test passed"

# LEGAL-CONSENT-001: hold a real writer at the preflight/install boundary.
TDF_TERMS_PGURL="$TDF_TERMS_PGURL" TDF_TERMS_CONTAINER="$TDF_TERMS_CONTAINER" \
  python3 "$TDF_TERMS_ROOT/scripts/test-checkout-terms-upgrade-concurrency.py"
