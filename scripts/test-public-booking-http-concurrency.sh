#!/bin/sh
set -eu

TDF_PB_HTTP_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_PB_HTTP_DATABASE_URL=${TDF_PUBLIC_BOOKING_HTTP_DATABASE_URL:-}
TDF_PB_HTTP_SERVER_BIN=${TDF_PUBLIC_BOOKING_HTTP_SERVER_BIN:-}
TDF_PB_HTTP_PORT=${TDF_PUBLIC_BOOKING_HTTP_SERVER_PORT:-$((18100 + ($$ % 700)))}
TDF_PB_HTTP_CONTAINER=""
TDF_PB_HTTP_SERVER_PID=""
TDF_PB_HTTP_RUNTIME_DIR=$(mktemp -d "${TMPDIR:-/tmp}/tdf-public-booking-http.XXXXXX")
TDF_PB_HTTP_SERVER_LOG="$TDF_PB_HTTP_RUNTIME_DIR/backend.log"

stop_server() {
  if [ -z "$TDF_PB_HTTP_SERVER_PID" ]; then
    return
  fi
  kill "$TDF_PB_HTTP_SERVER_PID" >/dev/null 2>&1 || true
  wait "$TDF_PB_HTTP_SERVER_PID" >/dev/null 2>&1 || true
  TDF_PB_HTTP_SERVER_PID=""
}

cleanup() {
  stop_server
  if [ -n "$TDF_PB_HTTP_CONTAINER" ]; then
    docker rm -f "$TDF_PB_HTTP_CONTAINER" >/dev/null 2>&1 || true
  fi
  case "$TDF_PB_HTTP_RUNTIME_DIR" in
    "${TMPDIR:-/tmp}"/tdf-public-booking-http.*)
      rm -rf -- "$TDF_PB_HTTP_RUNTIME_DIR"
      ;;
  esac
}
trap cleanup EXIT INT TERM

assert_equal() {
  actual=$1
  expected=$2
  label=$3
  if [ "$actual" != "$expected" ]; then
    echo "$label: expected '$expected', got '$actual'" >&2
    exit 1
  fi
}

assert_contains() {
  needle=$1
  file_path=$2
  label=$3
  if ! grep -Fq "$needle" "$file_path"; then
    echo "$label: response did not contain '$needle'" >&2
    sed -n '1,20p' "$file_path" >&2
    exit 1
  fi
}

psql_value() {
  psql "$TDF_PB_HTTP_DATABASE_URL" -X -qAt -v ON_ERROR_STOP=1 -c "$1"
}

json_field() {
  node -e '
    const fs = require("node:fs");
    const value = JSON.parse(fs.readFileSync(process.argv[1], "utf8"))[process.argv[2]];
    if (value === undefined || value === null) process.exit(2);
    process.stdout.write(String(value));
  ' "$1" "$2"
}

run_request() {
  request_key=$1
  payload=$2
  body_path=$3
  status_path=$4
  http_status=$(curl -sS --max-time 30 -o "$body_path" -w '%{http_code}' \
    -X POST "http://127.0.0.1:$TDF_PB_HTTP_PORT/bookings/public" \
    -H 'Content-Type: application/json' \
    -H "Idempotency-Key: $request_key" \
    --data "$payload")
  printf '%s' "$http_status" > "$status_path"
}

run_after_gate() {
  gate_path=$1
  shift
  while [ ! -e "$gate_path" ]; do
    sleep 0.05
  done
  run_request "$@"
}

if [ -z "$TDF_PB_HTTP_SERVER_BIN" ]; then
  if ! command -v stack >/dev/null 2>&1; then
    echo "TDF_PUBLIC_BOOKING_HTTP_SERVER_BIN is required when Stack is unavailable" >&2
    exit 1
  fi
  stack_root=$(CDPATH= cd -- "$TDF_PB_HTTP_ROOT/tdf-hq" && stack path --local-install-root 2>/dev/null | tail -1)
  TDF_PB_HTTP_SERVER_BIN="$stack_root/bin/tdf-hq-exe"
fi
if [ ! -x "$TDF_PB_HTTP_SERVER_BIN" ]; then
  echo "Candidate backend executable is missing or not executable: $TDF_PB_HTTP_SERVER_BIN" >&2
  exit 1
fi
case "$TDF_PB_HTTP_PORT" in
  ''|*[!0-9]*) echo "TDF_PUBLIC_BOOKING_HTTP_SERVER_PORT must be numeric" >&2; exit 1 ;;
esac
if curl -fsS "http://127.0.0.1:$TDF_PB_HTTP_PORT/health" >/dev/null 2>&1; then
  echo "Refusing to reuse an occupied HTTP test port: $TDF_PB_HTTP_PORT" >&2
  exit 1
fi

if [ -z "$TDF_PB_HTTP_DATABASE_URL" ]; then
  if ! command -v docker >/dev/null 2>&1; then
    echo "Docker is required when TDF_PUBLIC_BOOKING_HTTP_DATABASE_URL is not supplied" >&2
    exit 1
  fi
  TDF_PB_HTTP_CONTAINER="tdf-public-booking-http-$$"
  docker run --rm -d \
    --name "$TDF_PB_HTTP_CONTAINER" \
    -e POSTGRES_DB=tdf_public_booking_http_test \
    -e POSTGRES_PASSWORD=public-booking-http-test \
    -e POSTGRES_USER=postgres \
    -p 127.0.0.1::5432 \
    pgvector/pgvector:pg17 >/dev/null

  attempt=0
  until docker exec "$TDF_PB_HTTP_CONTAINER" \
    pg_isready -U postgres -d tdf_public_booking_http_test >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 45 ]; then
      echo "Disposable PostgreSQL did not become ready" >&2
      exit 1
    fi
    sleep 1
  done
  database_port=$(docker port "$TDF_PB_HTTP_CONTAINER" 5432/tcp | sed 's/.*://')
  TDF_PB_HTTP_DATABASE_URL="postgresql://postgres:public-booking-http-test@127.0.0.1:$database_port/tdf_public_booking_http_test"

  TDF_AUTOMIG_TEST_DATABASE_URL="$TDF_PB_HTTP_DATABASE_URL" \
  TDF_AUTOMIG_SERVER_BIN="$TDF_PB_HTTP_SERVER_BIN" \
  TDF_AUTOMIG_SERVER_PORT="$TDF_PB_HTTP_PORT" \
    bash "$TDF_PB_HTTP_ROOT/scripts/test-automatic-migrations-production-schema.sh"
fi

case "$TDF_PB_HTTP_DATABASE_URL" in
  postgresql://*@127.0.0.1:*/*|postgres://*@127.0.0.1:*/*|postgresql://*@localhost:*/*|postgres://*@localhost:*/*)
    ;;
  postgresql://*@postgres:*/*|postgres://*@postgres:*/*)
    if [ "${CI:-}" != "true" ]; then
      echo "The postgres service hostname is allowed only in CI" >&2
      exit 1
    fi
    ;;
  *)
    echo "Refusing to run the HTTP write test outside loopback or the CI postgres service" >&2
    exit 1
    ;;
esac

database_name=$(psql_value 'SELECT current_database();')
case "$database_name" in
  *_test) ;;
  *) echo "Refusing to write to database without a _test suffix: $database_name" >&2; exit 1 ;;
esac

assert_equal "$(psql_value "SELECT count(*) FROM tdf_schema_migration WHERE migration_id='2026-08-16_service_booking_checkout_runtime';")" \
  "1" "Service booking runtime migration prerequisite"
assert_equal "$(psql_value "SELECT count(*) FROM tdf_schema_migration WHERE migration_id='2026-09-09_public_booking_tentative_idempotency';")" \
  "1" "Public booking idempotency migration prerequisite"

fixture_collisions=$(psql_value "
  SELECT
    (SELECT count(*) FROM service_booking_tentative_request WHERE idempotency_key LIKE 'public-booking-http-test-%')
    + (SELECT count(*) FROM party WHERE lower(primary_email) LIKE 'public-booking-http-%@persona.test')
    + (SELECT count(*) FROM resource WHERE slug='public-booking-http-room')
    + (SELECT count(*) FROM booking WHERE notes LIKE 'http-concurrency:%');
")
assert_equal "$fixture_collisions" "0" "Pre-existing public booking HTTP fixtures"

mkdir -p "$TDF_PB_HTTP_RUNTIME_DIR/assets"
env -i \
  PATH="$PATH" \
  TMPDIR="$TDF_PB_HTTP_RUNTIME_DIR" \
  APP_ENV=test \
  DATABASE_URL="$TDF_PB_HTTP_DATABASE_URL" \
  APP_PORT="$TDF_PB_HTTP_PORT" \
  RESET_DB=false \
  RUN_MIGRATIONS=false \
  SEED_DB=false \
  DEFAULT_LOCALE=es \
  HQ_ASSETS_DIR="$TDF_PB_HTTP_RUNTIME_DIR/assets" \
  EVENT_DISCOVERY_ENABLED=false \
  ARTIST_ENRICHMENT_ENABLED=false \
  EVENT_LOGISTICS_RECHECK_ENABLED=false \
  "$TDF_PB_HTTP_SERVER_BIN" >"$TDF_PB_HTTP_SERVER_LOG" 2>&1 &
TDF_PB_HTTP_SERVER_PID=$!

attempt=0
until curl -fsS "http://127.0.0.1:$TDF_PB_HTTP_PORT/health" 2>/dev/null | grep -q '"status":"ok"'; do
  if ! kill -0 "$TDF_PB_HTTP_SERVER_PID" >/dev/null 2>&1; then
    echo "Candidate backend stopped before becoming healthy" >&2
    tail -100 "$TDF_PB_HTTP_SERVER_LOG" >&2
    exit 1
  fi
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 120 ]; then
    echo "Candidate backend did not become healthy within 120 seconds" >&2
    tail -100 "$TDF_PB_HTTP_SERVER_LOG" >&2
    exit 1
  fi
  sleep 1
done

offering_id=$(psql_value "
  SELECT offering.id::text
  FROM service_offering offering
  JOIN workflow_state state ON state.id=offering.workflow_state_id
  WHERE offering.active=TRUE
    AND offering.deprecated_at IS NULL
    AND offering.requires_engineer=FALSE
    AND state.code='published'
  ORDER BY offering.code
  LIMIT 1;
")
if [ -z "$offering_id" ]; then
  echo "No active published service offering without a required engineer is available" >&2
  exit 1
fi

psql "$TDF_PB_HTTP_DATABASE_URL" -X -q -v ON_ERROR_STOP=1 <<'SQL' >/dev/null
INSERT INTO resource(name,slug,resource_type,capacity,active)
VALUES ('Public booking HTTP room','public-booking-http-room','Room',1,TRUE);

CREATE OR REPLACE FUNCTION public_booking_http_test_delay()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.notes LIKE 'http-concurrency:%' THEN
    PERFORM pg_sleep(2);
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER public_booking_http_test_delay_trigger
BEFORE INSERT ON booking
FOR EACH ROW EXECUTE FUNCTION public_booking_http_test_delay();
SQL

replay_start=$(psql_value "SELECT to_char(date_trunc('hour', now() AT TIME ZONE 'UTC') + interval '30 days', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"');")
conflict_start=$(psql_value "SELECT to_char(date_trunc('hour', now() AT TIME ZONE 'UTC') + interval '31 days', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"');")
resource_slug=public-booking-http-room
replay_key=public-booking-http-test-replay-0001

replay_payload="{\"pbFullName\":\"Replay Customer\",\"pbEmail\":\"public-booking-http-replay@persona.test\",\"pbServiceOfferingId\":\"$offering_id\",\"pbStartsAt\":\"$replay_start\",\"pbDurationMinutes\":60,\"pbNotes\":\"http-concurrency:replay\",\"pbResourceIds\":[\"$resource_slug\"]}"
replay_gate="$TDF_PB_HTTP_RUNTIME_DIR/replay.start"

run_after_gate "$replay_gate" "$replay_key" "$replay_payload" \
  "$TDF_PB_HTTP_RUNTIME_DIR/replay-a.json" "$TDF_PB_HTTP_RUNTIME_DIR/replay-a.status" &
replay_pid_a=$!
run_after_gate "$replay_gate" "$replay_key" "$replay_payload" \
  "$TDF_PB_HTTP_RUNTIME_DIR/replay-b.json" "$TDF_PB_HTTP_RUNTIME_DIR/replay-b.status" &
replay_pid_b=$!
: > "$replay_gate"

observed_advisory_waiter=0
attempt=0
while [ "$attempt" -lt 80 ]; do
  if [ "$(psql_value "SELECT count(*) FROM pg_locks WHERE locktype='advisory' AND granted=FALSE;")" -ge 1 ]; then
    observed_advisory_waiter=1
    break
  fi
  attempt=$((attempt + 1))
  sleep 0.05
done

wait "$replay_pid_a"
wait "$replay_pid_b"
assert_equal "$observed_advisory_waiter" "1" "Concurrent equal-key advisory lock waiter"
assert_equal "$(cat "$TDF_PB_HTTP_RUNTIME_DIR/replay-a.status")" "200" "First equal-key response"
assert_equal "$(cat "$TDF_PB_HTTP_RUNTIME_DIR/replay-b.status")" "200" "Second equal-key response"
replay_booking_a=$(json_field "$TDF_PB_HTTP_RUNTIME_DIR/replay-a.json" bookingId)
replay_booking_b=$(json_field "$TDF_PB_HTTP_RUNTIME_DIR/replay-b.json" bookingId)
assert_equal "$replay_booking_b" "$replay_booking_a" "Equal-key replay booking identity"

assert_equal "$(psql_value "SELECT count(*) FROM booking WHERE notes='http-concurrency:replay';")" "1" "Equal-key booking rows"
assert_equal "$(psql_value "SELECT count(*) FROM service_booking_tentative_request WHERE idempotency_key='$replay_key';")" "1" "Equal-key receipt rows"
assert_equal "$(psql_value "SELECT count(*) FROM party WHERE lower(primary_email)='public-booking-http-replay@persona.test';")" "1" "Equal-key Party rows"
assert_equal "$(psql_value "SELECT count(*) FROM booking_resource WHERE booking_id=$replay_booking_a;")" "1" "Equal-key resource rows"
assert_equal "$(psql_value "SELECT count(*) FROM service_booking_resource_allocation WHERE booking_id=$replay_booking_a;")" "1" "Equal-key allocation rows"

run_request "$replay_key" "$replay_payload" \
  "$TDF_PB_HTTP_RUNTIME_DIR/replay-after-commit.json" "$TDF_PB_HTTP_RUNTIME_DIR/replay-after-commit.status"
assert_equal "$(cat "$TDF_PB_HTTP_RUNTIME_DIR/replay-after-commit.status")" "200" "Post-commit equal-key replay response"
assert_equal "$(json_field "$TDF_PB_HTTP_RUNTIME_DIR/replay-after-commit.json" bookingId)" "$replay_booking_a" \
  "Post-commit equal-key replay booking identity"

changed_payload="{\"pbFullName\":\"Changed Customer\",\"pbEmail\":\"public-booking-http-changed@persona.test\",\"pbServiceOfferingId\":\"$offering_id\",\"pbStartsAt\":\"$replay_start\",\"pbDurationMinutes\":60,\"pbNotes\":\"http-concurrency:changed\",\"pbResourceIds\":[\"$resource_slug\"]}"
run_request "$replay_key" "$changed_payload" \
  "$TDF_PB_HTTP_RUNTIME_DIR/changed.json" "$TDF_PB_HTTP_RUNTIME_DIR/changed.status"
assert_equal "$(cat "$TDF_PB_HTTP_RUNTIME_DIR/changed.status")" "409" "Changed-payload replay response"
assert_contains 'Idempotency key was already used for a different tentative booking' \
  "$TDF_PB_HTTP_RUNTIME_DIR/changed.json" "Changed-payload replay reason"
assert_equal "$(psql_value "SELECT count(*) FROM party WHERE lower(primary_email)='public-booking-http-changed@persona.test';")" "0" "Changed-payload orphan Party rows"
assert_equal "$(psql_value "SELECT count(*) FROM booking WHERE notes='http-concurrency:changed';")" "0" "Changed-payload orphan booking rows"

conflict_payload_a="{\"pbFullName\":\"Conflict Customer A\",\"pbEmail\":\"public-booking-http-conflict-a@persona.test\",\"pbServiceOfferingId\":\"$offering_id\",\"pbStartsAt\":\"$conflict_start\",\"pbDurationMinutes\":60,\"pbNotes\":\"http-concurrency:conflict-a\",\"pbResourceIds\":[\"$resource_slug\"]}"
conflict_payload_b="{\"pbFullName\":\"Conflict Customer B\",\"pbEmail\":\"public-booking-http-conflict-b@persona.test\",\"pbServiceOfferingId\":\"$offering_id\",\"pbStartsAt\":\"$conflict_start\",\"pbDurationMinutes\":60,\"pbNotes\":\"http-concurrency:conflict-b\",\"pbResourceIds\":[\"$resource_slug\"]}"
conflict_gate="$TDF_PB_HTTP_RUNTIME_DIR/conflict.start"

run_after_gate "$conflict_gate" public-booking-http-test-conflict-a-0001 "$conflict_payload_a" \
  "$TDF_PB_HTTP_RUNTIME_DIR/conflict-a.json" "$TDF_PB_HTTP_RUNTIME_DIR/conflict-a.status" &
conflict_pid_a=$!
run_after_gate "$conflict_gate" public-booking-http-test-conflict-b-0001 "$conflict_payload_b" \
  "$TDF_PB_HTTP_RUNTIME_DIR/conflict-b.json" "$TDF_PB_HTTP_RUNTIME_DIR/conflict-b.status" &
conflict_pid_b=$!
: > "$conflict_gate"
wait "$conflict_pid_a"
wait "$conflict_pid_b"

conflict_statuses=$(printf '%s\n%s\n' \
  "$(cat "$TDF_PB_HTTP_RUNTIME_DIR/conflict-a.status")" \
  "$(cat "$TDF_PB_HTTP_RUNTIME_DIR/conflict-b.status")" | sort | tr '\n' ',')
assert_equal "$conflict_statuses" "200,409," "Concurrent resource-conflict responses"
assert_equal "$(psql_value "SELECT count(*) FROM booking WHERE notes LIKE 'http-concurrency:conflict-%';")" "1" "Conflict booking rows"
assert_equal "$(psql_value "SELECT count(*) FROM service_booking_tentative_request WHERE idempotency_key LIKE 'public-booking-http-test-conflict-%';")" "1" "Conflict receipt rows"
assert_equal "$(psql_value "SELECT count(*) FROM party WHERE lower(primary_email) LIKE 'public-booking-http-conflict-%@persona.test';")" "1" "Conflict Party rows"
assert_equal "$(psql_value "SELECT count(*) FROM booking_resource br JOIN booking b ON b.id=br.booking_id WHERE b.notes LIKE 'http-concurrency:conflict-%';")" "1" "Conflict resource rows"
assert_equal "$(psql_value "SELECT count(*) FROM service_booking_resource_allocation allocation JOIN booking b ON b.id=allocation.booking_id WHERE b.notes LIKE 'http-concurrency:conflict-%';")" "1" "Conflict allocation rows"

test_parties=$(psql_value "SELECT string_agg(id::text, ',') FROM party WHERE lower(primary_email) LIKE 'public-booking-http-%@persona.test';")
assert_equal "$(psql_value "SELECT count(*) FROM user_credential WHERE party_id IN ($test_parties);")" "0" "Synthetic guest credential rows"
assert_equal "$(psql_value "SELECT count(*) FROM party_security_role WHERE party_id IN ($test_parties);")" "0" "Synthetic guest security-role rows"

echo "Public booking HTTP concurrency passed: equal replay=200/200, changed payload=409, overlapping resource=200/409, no orphan Party/receipt/resource rows"
