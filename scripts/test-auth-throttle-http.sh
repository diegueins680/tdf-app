#!/bin/sh
# AUTH-CREDENTIAL-INTAKE-001 / AUTH-PASSWORD-RECOVERY-001: the login lockout,
# the generic disabled-account answer and the signup and reset limits, driven
# over HTTP against the compiled backend and a disposable PostgreSQL database.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
DATABASE=${TDF_AUTH_THROTTLE_DATABASE:-tdf_auth_throttle_http_test}
PORT=${TDF_AUTH_THROTTLE_PORT:-18094}
BACKEND=${TDF_AUTH_THROTTLE_BACKEND_EXE:-}
PGHOST=${PGHOST:-127.0.0.1}
PGPORT=${PGPORT:-5432}
PGUSER=${PGUSER:-$(id -un)}
PGPASSWORD=${PGPASSWORD:-unused-local-test-value}
export PGHOST PGPORT PGUSER PGPASSWORD
case "$DATABASE" in tdf_*_test) ;; *) echo 'Database name must match tdf_*_test' >&2; exit 1 ;; esac
case "$PORT" in ''|*[!0-9]*) echo 'Invalid port' >&2; exit 1 ;; esac
if [ ! -x "$BACKEND" ]; then
  echo 'TDF_AUTH_THROTTLE_BACKEND_EXE must identify the compiled backend executable' >&2
  exit 1
fi
RUNTIME=$(mktemp -d "${TMPDIR:-/tmp}/tdf-auth-throttle.XXXXXX")
PID=""
CREATED=0
cleanup() {
  if [ -n "$PID" ]; then kill "$PID" >/dev/null 2>&1 || true; wait "$PID" >/dev/null 2>&1 || true; fi
  if [ "$CREATED" = "1" ]; then dropdb --if-exists "$DATABASE" >/dev/null 2>&1 || true; fi
  rm -rf -- "$RUNTIME"
}
trap cleanup EXIT INT TERM

if psql -d postgres -Atqc "SELECT 1 FROM pg_database WHERE datname = '$DATABASE'" | grep -q 1; then
  echo "Refusing to replace existing database: $DATABASE" >&2
  exit 1
fi
createdb "$DATABASE"
CREATED=1

env -i PATH="$PATH" HOME="$HOME" LANG="${LANG:-C.UTF-8}" \
  APP_ENV=test DB_HOST="$PGHOST" DB_PORT="$PGPORT" DB_USER="$PGUSER" DB_PASS="$PGPASSWORD" \
  DB_NAME="$DATABASE" APP_PORT="$PORT" RESET_DB=false RUN_MIGRATIONS=true SEED_DB=false \
  HQ_ASSETS_DIR="$ROOT/tdf-hq/assets" EVENT_DISCOVERY_ENABLED=false \
  ARTIST_ENRICHMENT_ENABLED=false EVENT_LOGISTICS_RECHECK_ENABLED=false \
  "$BACKEND" >"$RUNTIME/backend.log" 2>&1 &
PID=$!
attempt=0
until curl -fsS "http://127.0.0.1:$PORT/health" 2>/dev/null | grep -q '"status":"ok"'; do
  if ! kill -0 "$PID" >/dev/null 2>&1; then
    echo 'Backend stopped before becoming healthy' >&2; tail -60 "$RUNTIME/backend.log" >&2; exit 1
  fi
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 120 ]; then
    echo 'Backend did not become healthy within 120 seconds' >&2; tail -60 "$RUNTIME/backend.log" >&2; exit 1
  fi
  sleep 1
done

# Reset issuance writes the recovery challenge, which only this migration creates.
psql -X -v ON_ERROR_STOP=1 -d "$DATABASE" \
  < "$ROOT/tdf-hq/sql/2026-10-04_auth_recovery_expiry.sql" >/dev/null

if ! TDF_AUTH_THROTTLE_BASE="http://127.0.0.1:$PORT" TDF_AUTH_THROTTLE_DATABASE="$DATABASE" \
  python3 "$ROOT/scripts/lib/auth_throttle_http.py"; then
  # Backend diagnostics only; the log carries no request bodies.
  grep -v 'Debug#SQL' "$RUNTIME/backend.log" | tail -25 >&2
  exit 1
fi
