#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
. "$repo_root/scripts/lib/postgres-test-database.sh"
tdf_test_db_init tdf_event_relations_test
for fixture in \
  test/integration/event_operations_foundation_fixture.sql \
  sql/2026-09-14_event_operations_foundation.sql \
  test/integration/event_relations_fixture.sql; do
  tdf_test_psql < "$repo_root/tdf-hq/$fixture" >/dev/null
done
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
test -x "$test_binary"
TDF_EVENT_RELATIONS_DATABASE_URL="$TDF_TEST_DATABASE_URL" \
 "$test_binary" --match=event-relations --fail-on=empty
