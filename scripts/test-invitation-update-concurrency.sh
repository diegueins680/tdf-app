#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
. "$repo_root/scripts/lib/postgres-test-database.sh"
tdf_test_db_init tdf_invitation_test
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
test -x "$test_binary"
TDF_INVITATION_TEST_DATABASE_URL="$TDF_TEST_DATABASE_URL" \
 "$test_binary" --match=invitation --fail-on=empty
