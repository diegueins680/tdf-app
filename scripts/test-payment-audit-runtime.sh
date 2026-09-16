#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
if [ -n "${TDF_PAYMENT_AUDIT_DATABASE_URL:-}" ]; then
  # CI supplies an explicitly isolated service database; never initialize a
  # populated database or a database with a different name.
  test "$(psql "$TDF_PAYMENT_AUDIT_DATABASE_URL" -X -v ON_ERROR_STOP=1 -qAtc "SELECT current_database() = 'tdf_payment_audit_test' AND NOT EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = 'public');")" = t
  psql "$TDF_PAYMENT_AUDIT_DATABASE_URL" -X -v ON_ERROR_STOP=1 --single-transaction \
    -f "$repo_root/tdf-hq/test/integration/payment_audit_fixture.sql" >/dev/null
else
  test_container="tdf-payment-audit-$$"
  cleanup() { docker rm -f "$test_container" >/dev/null 2>&1 || true; }
  trap cleanup EXIT INT TERM
  docker run --rm -d --name "$test_container" -p 127.0.0.1::5432 -e POSTGRES_PASSWORD=payment-audit-fixture -e POSTGRES_DB=tdf_payment_audit_test postgres:16-alpine >/dev/null
  attempt=0
  until docker exec "$test_container" psql -h 127.0.0.1 -U postgres -d tdf_payment_audit_test -qAtc 'SELECT 1' >/dev/null 2>&1; do
    attempt=$((attempt+1)); test "$attempt" -lt 30; sleep 1
  done
  test_port=$(docker port "$test_container" 5432/tcp | sed 's/^127\.0\.0\.1://')
  case "$test_port" in ''|*[!0-9]*) echo 'Invalid owned PostgreSQL port' >&2; exit 1;; esac
  docker exec -i "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_payment_audit_test < "$repo_root/tdf-hq/test/integration/payment_audit_fixture.sql" >/dev/null
  TDF_PAYMENT_AUDIT_DATABASE_URL="postgresql://postgres:payment-audit-fixture@127.0.0.1:$test_port/tdf_payment_audit_test"
fi
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
test -x "$test_binary"
COMMERCE_CHECKOUT_ENV=sandbox TDF_PAYMENT_AUDIT_DATABASE_URL="$TDF_PAYMENT_AUDIT_DATABASE_URL" \
 "$test_binary" --match=payment-audit --fail-on=empty
echo 'Payment audit PostgreSQL boundaries PASS'
