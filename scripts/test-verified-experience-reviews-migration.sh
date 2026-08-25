#!/bin/sh
set -eu

TDF_REVIEW_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_REVIEW_CONTAINER="tdf-experience-review-migration-$$"
TDF_REVIEW_DATABASE="tdf_experience_review_test"
TDF_REVIEW_USE_DOCKER="false"

cleanup() {
  if [ "$TDF_REVIEW_USE_DOCKER" = "true" ]; then
    docker rm -f "$TDF_REVIEW_CONTAINER" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ -n "${TDF_REVIEW_DATABASE_URL:-}" ]; then
  psql_exec() {
    PGOPTIONS='-c statement_timeout=10000' \
      psql "$TDF_REVIEW_DATABASE_URL" -v ON_ERROR_STOP=1 "$@"
  }
  apply_file() {
    PGOPTIONS='-c statement_timeout=10000' \
      psql "$TDF_REVIEW_DATABASE_URL" -v ON_ERROR_STOP=1 \
      < "$TDF_REVIEW_ROOT/$1" >/dev/null
  }
else
  TDF_REVIEW_USE_DOCKER="true"
  docker run --rm -d \
    --name "$TDF_REVIEW_CONTAINER" \
    -e POSTGRES_PASSWORD=experience-review-test \
    -e POSTGRES_DB="$TDF_REVIEW_DATABASE" \
    postgres:17-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_REVIEW_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_REVIEW_DATABASE" -Atqc 'SELECT 1' \
    >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 30 ]; then
      echo "Experience review migration database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done

  psql_exec() {
    docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_REVIEW_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_REVIEW_DATABASE" "$@"
  }
  apply_file() {
    docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_REVIEW_CONTAINER" \
      psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_REVIEW_DATABASE" \
      < "$TDF_REVIEW_ROOT/$1" >/dev/null
  }
fi

assert_equal() {
  actual=$1
  expected=$2
  label=$3
  if [ "$actual" != "$expected" ]; then
    echo "$label: expected '$expected', got '$actual'" >&2
    exit 1
  fi
}

psql_exec <<'SQL' >/dev/null
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE party (
  id BIGINT PRIMARY KEY,
  display_name TEXT NOT NULL,
  primary_email TEXT
);
CREATE TABLE social_event (
  id BIGINT PRIMARY KEY,
  title TEXT NOT NULL,
  start_time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ
);
CREATE TABLE event_ticket_order (
  id BIGINT PRIMARY KEY,
  event_id BIGINT NOT NULL,
  buyer_party_id BIGINT,
  status TEXT NOT NULL
);
CREATE TABLE event_ticket_checkout_runtime (
  order_id BIGINT PRIMARY KEY,
  payment_status TEXT NOT NULL,
  fulfillment_status TEXT NOT NULL
);

CREATE TABLE marketplace_listing (
  id UUID PRIMARY KEY,
  title TEXT NOT NULL
);
CREATE TABLE marketplace_order (
  id UUID PRIMARY KEY,
  buyer_email TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE TABLE marketplace_order_item (
  id UUID PRIMARY KEY,
  order_id UUID NOT NULL,
  listing_id UUID NOT NULL
);
CREATE TABLE marketplace_sale_order_runtime (
  order_id UUID PRIMARY KEY,
  fulfillment_status TEXT NOT NULL,
  delivered_at TIMESTAMPTZ,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE TABLE marketplace_rental_order_runtime (
  order_id UUID PRIMARY KEY,
  rental_status TEXT NOT NULL,
  returned_at TIMESTAMPTZ,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE service_offering (
  id UUID PRIMARY KEY,
  name_es TEXT NOT NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  deprecated_at TIMESTAMPTZ
);
CREATE TABLE booking (
  id BIGINT PRIMARY KEY,
  party_id BIGINT,
  service_offering_id UUID,
  status TEXT NOT NULL,
  ends_at TIMESTAMPTZ NOT NULL
);
CREATE TABLE service_booking_checkout_runtime (
  booking_id BIGINT PRIMARY KEY,
  fulfillment_status TEXT NOT NULL,
  completed_at TIMESTAMPTZ
);

CREATE TABLE service_storefront_package (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE
);
CREATE TABLE service_storefront_order (
  id UUID PRIMARY KEY,
  package_id UUID NOT NULL,
  buyer_email TEXT NOT NULL,
  status TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE directory_rate_limit (
  scope TEXT NOT NULL,
  subject_hash TEXT NOT NULL,
  window_started_at TIMESTAMPTZ NOT NULL,
  count INTEGER NOT NULL DEFAULT 1,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY(scope,subject_hash,window_started_at),
  CONSTRAINT directory_rate_limit_scope_check CHECK (
    scope IN ('search','profile_create','classified_publish','application','invitation','contact','report','review')
  ),
  CHECK (count > 0)
);
SQL

apply_file tdf-hq/sql/2026-08-20_verified_experience_reviews.sql
apply_file tdf-hq/sql/2026-08-20_verified_experience_reviews.sql

psql_exec <<'SQL' >/dev/null
INSERT INTO party VALUES (1,'Verified reviewer','reviewer@example.test');

INSERT INTO social_event VALUES (101,'Past event',NOW()-INTERVAL '2 days',NOW()-INTERVAL '1 day');
INSERT INTO event_ticket_order VALUES (201,101,1,'paid');
INSERT INTO event_ticket_checkout_runtime VALUES (201,'paid','issued');

INSERT INTO marketplace_listing VALUES ('10000000-0000-4000-8000-000000000001','Verified listing');
INSERT INTO marketplace_order VALUES ('20000000-0000-4000-8000-000000000001','reviewer@example.test',NOW());
INSERT INTO marketplace_order_item VALUES (
  '30000000-0000-4000-8000-000000000001',
  '20000000-0000-4000-8000-000000000001',
  '10000000-0000-4000-8000-000000000001'
);
INSERT INTO marketplace_sale_order_runtime VALUES (
  '20000000-0000-4000-8000-000000000001','delivered',NOW(),NOW()
);

INSERT INTO service_offering VALUES ('40000000-0000-4000-8000-000000000001','Sesión verificada',TRUE,NULL);
INSERT INTO booking VALUES (
  501,1,'40000000-0000-4000-8000-000000000001','Completed',NOW()-INTERVAL '1 day'
);
INSERT INTO service_booking_checkout_runtime VALUES (501,'completed',NOW()-INTERVAL '1 day');

INSERT INTO service_storefront_package VALUES (
  '60000000-0000-4000-8000-000000000001','Paquete verificado',TRUE
);
INSERT INTO service_storefront_order VALUES (
  '70000000-0000-4000-8000-000000000001',
  '60000000-0000-4000-8000-000000000001',
  'reviewer@example.test','completed',NOW()
);

INSERT INTO directory_rate_limit(scope,subject_hash,window_started_at)
VALUES ('experience-review','reviewer',date_trunc('day',NOW()));
SQL

eligible_count=$(psql_exec -Atqc "
  SELECT count(*) FROM (VALUES
    (experience_review_source_is_eligible('event','101','event_ticket_order','201',1)),
    (experience_review_source_is_eligible(
      'marketplace_listing','10000000-0000-4000-8000-000000000001',
      'marketplace_order','20000000-0000-4000-8000-000000000001',1)),
    (experience_review_source_is_eligible(
      'service_offering','40000000-0000-4000-8000-000000000001',
      'service_booking','501',1)),
    (experience_review_source_is_eligible(
      'service_package','60000000-0000-4000-8000-000000000001',
      'service_storefront_order','70000000-0000-4000-8000-000000000001',1))
  ) result(eligible) WHERE eligible;
")
assert_equal "$eligible_count" "4" "all completed interaction kinds are eligible"

psql_exec <<'SQL' >/dev/null
INSERT INTO experience_review(target_kind,target_id,source_kind,source_id,author_party_id,rating,body)
VALUES
  ('event','101','event_ticket_order','201',1,5,'Una experiencia verificada.'),
  ('marketplace_listing','10000000-0000-4000-8000-000000000001','marketplace_order','20000000-0000-4000-8000-000000000001',1,4,NULL),
  ('service_offering','40000000-0000-4000-8000-000000000001','service_booking','501',1,5,NULL),
  ('service_package','60000000-0000-4000-8000-000000000001','service_storefront_order','70000000-0000-4000-8000-000000000001',1,5,NULL);
SQL

review_count=$(psql_exec -Atqc 'SELECT count(*) FROM experience_review;')
assert_equal "$review_count" "4" "eligible reviews inserted"

if psql_exec -c "
  INSERT INTO experience_review(target_kind,target_id,source_kind,source_id,author_party_id,rating)
  VALUES ('event','101','event_ticket_order','999',1,5);
" >/dev/null 2>&1; then
  echo "Ineligible review evidence was accepted" >&2
  exit 1
fi

event_review_id=$(psql_exec -Atqc "SELECT id FROM experience_review WHERE target_kind='event';")
if psql_exec -c "UPDATE experience_review SET rating=1 WHERE id='$event_review_id';" >/dev/null 2>&1; then
  echo "Published review content was mutable" >&2
  exit 1
fi
psql_exec -c "UPDATE experience_review SET status='hidden' WHERE id='$event_review_id';" >/dev/null
hidden_count=$(psql_exec -Atqc "SELECT count(*) FROM experience_review WHERE status='hidden';")
assert_equal "$hidden_count" "1" "moderation status remains mutable"

apply_file tdf-hq/sql/2026-08-20_verified_experience_reviews_rollback.sql
table_count=$(psql_exec -Atqc "SELECT count(*) FROM pg_class WHERE relname='experience_review' AND relkind='r';")
assert_equal "$table_count" "0" "rollback removes review table"
rate_rows=$(psql_exec -Atqc "SELECT count(*) FROM directory_rate_limit WHERE scope='experience-review';")
assert_equal "$rate_rows" "1" "rollback preserves abuse-control evidence"

apply_file tdf-hq/sql/2026-08-20_verified_experience_reviews.sql
apply_file tdf-hq/sql/2026-08-20_verified_experience_reviews.sql

echo "Verified experience review migration passed eligibility, integrity, retry, and rollback checks"
