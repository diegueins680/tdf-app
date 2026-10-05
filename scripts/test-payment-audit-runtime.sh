#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
. "$repo_root/scripts/lib/postgres-test-database.sh"
tdf_test_db_init tdf_payment_audit_test
tdf_test_psql < "$repo_root/tdf-hq/test/integration/payment_audit_fixture.sql" >/dev/null
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
test -x "$test_binary"
COMMERCE_CHECKOUT_ENV=sandbox TDF_PAYMENT_AUDIT_DATABASE_URL="$TDF_TEST_DATABASE_URL" \
 "$test_binary" --match=payment-audit --fail-on=empty
echo 'Payment audit PostgreSQL boundaries PASS'
