#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container="tdf-payment-audit-$$"
cleanup() { docker rm -f "$test_container" >/dev/null 2>&1 || true; }
trap cleanup EXIT INT TERM
docker run --rm -d --name "$test_container" -p 127.0.0.1::5432 -e POSTGRES_PASSWORD=payment-audit-fixture -e POSTGRES_DB=tdf_payment_audit_test postgres:16-alpine >/dev/null
attempt=0
until docker exec "$test_container" psql -U postgres -d tdf_payment_audit_test -qAtc 'SELECT 1' >/dev/null 2>&1; do
 attempt=$((attempt+1)); test "$attempt" -lt 30; sleep 1
done
test_port=$(docker port "$test_container" 5432/tcp | sed 's/^127\.0\.0\.1://')
case "$test_port" in ''|*[!0-9]*) echo 'Invalid owned PostgreSQL port' >&2; exit 1;; esac
docker exec -i "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_payment_audit_test < "$repo_root/tdf-hq/test/integration/payment_audit_fixture.sql" >/dev/null
docker exec "$test_container" createdb -U postgres tdf_payment_fallback_test
docker exec "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_payment_fallback_test -c 'CREATE EXTENSION pgcrypto' >/dev/null
for migration in \
  2026-08-13_unified_checkout_core.sql \
  2026-08-14_checkout_event_refund_runtime.sql \
  2026-09-09_canonical_payment_lifecycle.sql \
  2026-09-10_payment_attempt_intent_binding.sql \
  2026-09-11_payment_intent_runtime_sync.sql \
  2026-09-11_provider_capability_catalog.sql; do
  docker exec -i "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_payment_fallback_test < "$repo_root/tdf-hq/sql/$migration" >/dev/null
done
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
test -x "$test_binary"
COMMERCE_CHECKOUT_ENV=sandbox TDF_PAYMENT_AUDIT_DATABASE_URL="postgresql://postgres:payment-audit-fixture@127.0.0.1:$test_port/tdf_payment_audit_test" \
 TDF_PAYMENT_FALLBACK_DATABASE_URL="postgresql://postgres:payment-audit-fixture@127.0.0.1:$test_port/tdf_payment_fallback_test" \
 "$test_binary" --match=payment-audit --fail-on=empty
echo 'Payment audit PostgreSQL boundaries PASS'
