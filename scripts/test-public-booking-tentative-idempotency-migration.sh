#!/bin/sh
set -eu

TDF_PUBLIC_BOOKING_CONTAINER="tdf-public-booking-idempotency-$$"
TDF_PUBLIC_BOOKING_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_PUBLIC_BOOKING_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_PUBLIC_BOOKING_CONTAINER" \
  -e POSTGRES_PASSWORD=public-booking-test \
  -e POSTGRES_DB=tdf_public_booking_test \
  postgres:17-alpine >/dev/null

attempt=0
until docker exec "$TDF_PUBLIC_BOOKING_CONTAINER" \
  psql -v ON_ERROR_STOP=1 -U postgres -d tdf_public_booking_test -Atqc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Public booking idempotency migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_PUBLIC_BOOKING_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_public_booking_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=10000" "$TDF_PUBLIC_BOOKING_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_public_booking_test \
    < "$TDF_PUBLIC_BOOKING_ROOT/$1" >/dev/null
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

psql_exec -c 'CREATE TABLE booking (id BIGINT PRIMARY KEY);' >/dev/null
apply_file tdf-hq/sql/2026-09-09_public_booking_tentative_idempotency.sql
apply_file tdf-hq/sql/2026-09-09_public_booking_tentative_idempotency.sql

psql_exec -c "
  INSERT INTO booking(id) VALUES (1), (2);
  INSERT INTO service_booking_tentative_request(
    idempotency_key, request_sha256, booking_id
  ) VALUES (
    'service-booking-replay-0001', repeat('a', 64), 1
  );
" >/dev/null

assert_equal "$(psql_exec -Atc "SELECT request_sha256 || ':' || booking_id::text FROM service_booking_tentative_request WHERE idempotency_key='service-booking-replay-0001';")" \
  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1" "Tentative booking replay record"

if psql_exec -c "INSERT INTO service_booking_tentative_request VALUES ('service-booking-replay-0001', repeat('b', 64), 2, NOW());" >/dev/null 2>&1; then
  echo "Duplicate public booking idempotency key was accepted" >&2
  exit 1
fi

if psql_exec -c "INSERT INTO service_booking_tentative_request VALUES ('service-booking-replay-0002', repeat('a', 64), 1, NOW());" >/dev/null 2>&1; then
  echo "One tentative booking was bound to multiple idempotency keys" >&2
  exit 1
fi

if psql_exec -c "INSERT INTO service_booking_tentative_request VALUES ('short', repeat('c', 64), 2, NOW());" >/dev/null 2>&1; then
  echo "Invalid public booking idempotency key was accepted" >&2
  exit 1
fi

apply_file tdf-hq/sql/2026-09-09_public_booking_tentative_idempotency_rollback.sql
assert_equal "$(psql_exec -Atc "SELECT count(*) FROM service_booking_tentative_request;")" \
  "1" "Non-destructive tentative booking rollback"

echo "Public tentative booking idempotency migration tests passed"
