#!/bin/sh
# Owned test databases only: existing GitHub service in CI, disposable Docker locally.
tdf_test_db_init() {
  tdf_test_database=$1
  case "$tdf_test_database" in
    ''|*[!a-z_]*) echo 'Invalid test database name' >&2; return 1;;
    *_test) ;;
    *) echo 'Test database must end in _test' >&2; return 1;;
  esac
  tdf_test_db_owned=false
  tdf_test_container=""
  if [ "${GITHUB_ACTIONS:-}" = true ]; then
    test "${TDF_TEST_POSTGRES_HOST:-}" = postgres
    test -n "${PGPASSWORD:-}"
    createdb -h postgres -U postgres "$tdf_test_database"
    tdf_test_db_owned=true
    TDF_TEST_DATABASE_URL="host=postgres port=5432 user=postgres dbname=$tdf_test_database"
  else
    tdf_test_container="tdf-runtime-test-$$"
    docker run --rm -d --name "$tdf_test_container" -p 127.0.0.1::5432 \
      -e POSTGRES_PASSWORD=isolated-runtime-fixture -e "POSTGRES_DB=$tdf_test_database" postgres:16-alpine >/dev/null
    tdf_test_db_owned=true
  fi
  trap tdf_test_db_cleanup EXIT INT TERM
  tdf_test_attempt=0
  until tdf_test_psql -qAtc 'SELECT 1' >/dev/null 2>&1; do
    tdf_test_attempt=$((tdf_test_attempt+1))
    test "$tdf_test_attempt" -lt 30 || return 1
    sleep 1
  done
  if [ -n "$tdf_test_container" ]; then
    tdf_test_port=$(docker port "$tdf_test_container" 5432/tcp | sed 's/^127\.0\.0\.1://')
    case "$tdf_test_port" in ''|*[!0-9]*) echo 'Invalid owned PostgreSQL port' >&2; return 1;; esac
    TDF_TEST_DATABASE_URL="postgresql://postgres:isolated-runtime-fixture@127.0.0.1:$tdf_test_port/$tdf_test_database"
  fi
}
tdf_test_psql() {
  if [ -n "$tdf_test_container" ]; then
    docker exec -i "$tdf_test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$tdf_test_database" "$@"
  else
    psql -X -v ON_ERROR_STOP=1 -h postgres -U postgres -d "$tdf_test_database" "$@"
  fi
}
tdf_test_db_cleanup() {
  if [ "$tdf_test_db_owned" = true ]; then
    if [ -n "$tdf_test_container" ]; then
      docker rm -f "$tdf_test_container" >/dev/null 2>&1 || true
    else
      dropdb -h postgres -U postgres "$tdf_test_database"
    fi
    tdf_test_db_owned=false
  fi
}
