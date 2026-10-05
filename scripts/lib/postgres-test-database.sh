#!/bin/sh
# Owned test databases only: CI service, disposable Docker, or explicit native loopback opt-in.
tdf_test_db_init() {
  tdf_test_database=$1
  case "$tdf_test_database" in
    ''|*[!a-z_]*) echo 'Invalid test database name' >&2; return 1;;
    *_test) ;;
    *) echo 'Test database must end in _test' >&2; return 1;;
  esac
  tdf_test_db_owned=false
  tdf_test_container=""
  tdf_test_native=false
  test -z "${PGHOSTADDR:-}${PGSERVICE:-}${PGSERVICEFILE:-}" || {
    echo 'Unset libpq routing overrides for disposable database runners' >&2; return 1;
  }
  if [ "${GITHUB_ACTIONS:-}" = true ]; then
    test "${TDF_TEST_POSTGRES_HOST:-}" = postgres
    test -n "${TDF_TEST_POSTGRES_PASSWORD:-}"
    PGPASSWORD=$TDF_TEST_POSTGRES_PASSWORD
    export PGPASSWORD
    createdb -h postgres -U postgres "$tdf_test_database" || return $?
    tdf_test_db_owned=true
    TDF_TEST_DATABASE_URL="host=postgres port=5432 user=postgres dbname=$tdf_test_database"
  elif [ "${TDF_TEST_NATIVE_POSTGRES:-}" = 1 ]; then
    # Explicit opt-in to an existing loopback server. Own only the new database:
    # a create failure must never trigger cleanup of a pre-existing database.
    tdf_test_native=true
    tdf_test_native_user=${TDF_TEST_NATIVE_POSTGRES_USER:-$(id -un)}
    tdf_test_native_port=${TDF_TEST_NATIVE_POSTGRES_PORT:-5432}
    case "$tdf_test_native_user" in ''|*[!a-zA-Z0-9_-]*) echo 'Invalid native test role' >&2; return 1;; esac
    case "$tdf_test_native_port" in ''|*[!0-9]*) echo 'Invalid native test port' >&2; return 1;; esac
    test "$tdf_test_native_port" -gt 0 && test "$tdf_test_native_port" -le 65535 || return 1
    createdb -h 127.0.0.1 -p "$tdf_test_native_port" -U "$tdf_test_native_user" "$tdf_test_database" || return $?
    tdf_test_db_owned=true
    TDF_TEST_DATABASE_URL="host=127.0.0.1 port=$tdf_test_native_port user=$tdf_test_native_user dbname=$tdf_test_database"
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
  elif [ "$tdf_test_native" = true ]; then
    psql -X -v ON_ERROR_STOP=1 -h 127.0.0.1 -p "$tdf_test_native_port" -U "$tdf_test_native_user" -d "$tdf_test_database" "$@"
  else
    psql -X -v ON_ERROR_STOP=1 -h postgres -U postgres -d "$tdf_test_database" "$@"
  fi
}
tdf_test_db_cleanup() {
  if [ "$tdf_test_db_owned" = true ]; then
    if [ -n "$tdf_test_container" ]; then
      docker rm -f "$tdf_test_container" >/dev/null 2>&1 || true
    elif [ "$tdf_test_native" = true ]; then
      dropdb -h 127.0.0.1 -p "$tdf_test_native_port" -U "$tdf_test_native_user" "$tdf_test_database"
    else
      dropdb -h postgres -U postgres "$tdf_test_database"
    fi
    tdf_test_db_owned=false
  fi
}
