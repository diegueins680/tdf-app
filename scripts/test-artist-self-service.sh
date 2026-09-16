#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_database="tdf_artist_self_service_test_$$"
export PGHOST="${PGHOST:-127.0.0.1}"
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
createdb "$test_database"
trap cleanup EXIT INT TERM
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/tdf-hq/test/integration/artist_self_service_fixture.sql" >/dev/null
migration="$repo_root/tdf-hq/sql/2026-09-16_artist_self_service.sql"
rollback="$repo_root/tdf-hq/sql/2026-09-16_artist_self_service_rollback.sql"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$migration" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$migration" >/dev/null
test "$(psql -X -At -d "$test_database" -c "SELECT count(*) FROM security_role_assignment_policy WHERE active AND code='artist.self-service.artist'")" = 1
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$rollback" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$rollback" >/dev/null
test "$(psql -X -At -d "$test_database" -c "SELECT count(*) FROM security_role_assignment_policy WHERE active AND code='artist.self-service.artist'")" = 0
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$migration" >/dev/null
export TDF_ARTIST_SELF_SERVICE_TEST_DB="host=$PGHOST dbname=$test_database"
cd "$repo_root/tdf-hq"
# Concurrent libpq calls must not block the entire test runtime.
stack test tdf-hq:test:tdf-hq-test --ghc-options=-threaded --jobs 1 --test-arguments='--match artist-self-service-postgresql'
