#!/usr/bin/env bash
set -euo pipefail
interaction_repo=$(cd "$(dirname "$0")/../.." && pwd)
interaction_binary=${TDF_INTERACTION_SERVER_BIN:?Pass the built backend executable}
test -x "$interaction_binary" || { echo 'Backend executable is missing' >&2; exit 1; }
interaction_db="tdf_interaction_http_ci_$$"
interaction_port=${TDF_INTERACTION_SERVER_PORT:-18149}
interaction_runtime=$(mktemp -d "${TMPDIR:-/tmp}/tdf-interaction-http.XXXXXX")
interaction_pid=''
interaction_owned=false
cleanup() {
  if test -n "$interaction_pid"; then kill "$interaction_pid" 2>/dev/null || true; wait "$interaction_pid" 2>/dev/null || true; fi
  if test "$interaction_owned" = true; then dropdb --if-exists "$interaction_db"; fi
}
trap cleanup EXIT
if test -n "$(psql -X -Atq -d postgres -c "SELECT 1 FROM pg_database WHERE datname='$interaction_db'")"; then echo 'Refusing existing test database' >&2; exit 1; fi
if curl --silent --max-time 2 "http://127.0.0.1:$interaction_port/health" >/dev/null; then echo 'Refusing occupied server port' >&2; exit 1; fi
interaction_owned=true
bash "$interaction_repo/scripts/interactions/prepare-test-database.sh" "$interaction_db" > "$interaction_runtime/migrations.log" 2>&1 || { tail -n 50 "$interaction_runtime/migrations.log"; exit 1; }
env -i PATH="$PATH" TMPDIR="$interaction_runtime" \
  PGHOST="${PGHOST:-127.0.0.1}" PGPORT="${PGPORT:-5432}" PGUSER="${PGUSER:-$(id -un)}" PGPASSWORD="${PGPASSWORD:-unused-local-test-value}" PGDATABASE="$interaction_db" \
  APP_ENV=test APP_PORT="$interaction_port" DEFAULT_LOCALE=es \
  RESET_DB=false RUN_MIGRATIONS=false SEED_DB=false AUTO_APPLY_PRODUCTION_MIGRATIONS=false \
  HQ_ASSETS_DIR="$interaction_runtime" EVENT_DISCOVERY_ENABLED=false ARTIST_ENRICHMENT_ENABLED=false EVENT_LOGISTICS_RECHECK_ENABLED=false \
  "$interaction_binary" > "$interaction_runtime/backend.log" 2>&1 &
interaction_pid=$!
interaction_healthy=false
for ((interaction_attempt=0; interaction_attempt<90; interaction_attempt++)); do
  if curl --silent --fail "http://127.0.0.1:$interaction_port/health" | python3 -c 'import json,sys; assert json.load(sys.stdin)["db"]=="ok"' 2>/dev/null; then interaction_healthy=true; break; fi
  kill -0 "$interaction_pid" 2>/dev/null || { tail -n 50 "$interaction_runtime/backend.log"; exit 1; }
  sleep 1
done
if test "$interaction_healthy" != true; then tail -n 50 "$interaction_runtime/backend.log"; exit 1; fi
TDF_INTERACTION_TEST_BASE="http://127.0.0.1:$interaction_port" TDF_INTERACTION_TEST_DATABASE="$interaction_db" \
  TDF_INTERACTION_TEST_FIXTURE="$interaction_runtime/fixture.json" python3 "$interaction_repo/scripts/interactions/test-api.py"
if test "${TDF_INTERACTION_BROWSER_E2E:-0}" = 1; then
  cd "$interaction_repo"
  TDF_INTERACTION_TEST_FIXTURE="$interaction_runtime/fixture.json" npm exec -- playwright test --config=playwright.interactions.config.mjs
fi
printf 'PASS isolated interaction HTTP runtime; logs in %s\n' "$interaction_runtime"
