#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
database_url="${TDF_AUTOMIG_TEST_DATABASE_URL:?Set TDF_AUTOMIG_TEST_DATABASE_URL to an isolated empty PostgreSQL database}"
server_bin="${TDF_AUTOMIG_SERVER_BIN:?Set TDF_AUTOMIG_SERVER_BIN to the candidate backend executable}"
server_port="${TDF_AUTOMIG_SERVER_PORT:-18881}"
server_log="${TMPDIR:-/tmp}/tdf-automatic-migrations-${$}.log"
server_pid=""

stop_server() {
  if [ -z "${server_pid}" ]; then
    return
  fi
  kill "${server_pid}" >/dev/null 2>&1 || true
  wait "${server_pid}" >/dev/null 2>&1 || true
  server_pid=""
}
trap stop_server EXIT INT TERM

if [ ! -x "${server_bin}" ]; then
  echo "Candidate backend executable is missing or not executable: ${server_bin}" >&2
  exit 1
fi

existing_tables="$(psql "${database_url}" -X -qAt -v ON_ERROR_STOP=1 -c "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p');")"
if [ "${existing_tables}" != "0" ]; then
  echo "Automatic migration test requires an empty isolated database; found ${existing_tables} tables" >&2
  exit 1
fi

psql "${database_url}" -X -v ON_ERROR_STOP=1 \
  -f "${repo_root}/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
psql "${database_url}" -X -v ON_ERROR_STOP=1 \
  -f "${repo_root}/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
SOURCE_COMMIT="${GITHUB_SHA:-0000000000000000000000000000000000000000}" \
  node "${repo_root}/scripts/render-production-migration-batch.mjs" \
  | psql "${database_url}" -X -v ON_ERROR_STOP=1 >/dev/null

expected_migrations="$(node -p "require('${repo_root}/scripts/production-migrations.json').migrations.length")"
applied_migrations="$(psql "${database_url}" -X -qAt -v ON_ERROR_STOP=1 -c 'SELECT count(*) FROM public.tdf_schema_migration;')"
test "${applied_migrations}" = "${expected_migrations}"

db_host="$(node -e 'const url=new URL(process.argv[1]); process.stdout.write(url.hostname)' "${database_url}")"
db_port="$(node -e 'const url=new URL(process.argv[1]); process.stdout.write(url.port || "5432")' "${database_url}")"
db_user="$(node -e 'const url=new URL(process.argv[1]); process.stdout.write(decodeURIComponent(url.username))' "${database_url}")"
db_pass="$(node -e 'const url=new URL(process.argv[1]); process.stdout.write(decodeURIComponent(url.password))' "${database_url}")"
db_name="$(node -e 'const url=new URL(process.argv[1]); process.stdout.write(decodeURIComponent(url.pathname.slice(1)))' "${database_url}")"

start_and_verify() {
  : > "${server_log}"
  DB_HOST="${db_host}" \
  DB_PORT="${db_port}" \
  DB_USER="${db_user}" \
  DB_PASS="${db_pass}" \
  DB_NAME="${db_name}" \
  APP_PORT="${server_port}" \
  RUN_MIGRATIONS=true \
  RESET_DB=false \
  SEED_DB=false \
  EVENT_DISCOVERY_ENABLED=false \
  "${server_bin}" >"${server_log}" 2>&1 &
  server_pid=$!

  for _ in $(seq 1 180); do
    if curl -fsS "http://127.0.0.1:${server_port}/health" 2>/dev/null | grep -q '"db":"ok"'; then
      stop_server
      return
    fi
    if ! kill -0 "${server_pid}" >/dev/null 2>&1; then
      tail -160 "${server_log}" >&2
      echo "Backend exited during automatic migration verification" >&2
      exit 1
    fi
    sleep 1
  done

  tail -160 "${server_log}" >&2
  echo "Backend did not become healthy after automatic migration preflight" >&2
  exit 1
}

start_and_verify
node "${repo_root}/scripts/render-production-schema-verification.mjs" \
  | psql "${database_url}" -X -v ON_ERROR_STOP=1 >/dev/null

legacy_rows="$(psql "${database_url}" -X -qAt -v ON_ERROR_STOP=1 -c 'SELECT count(*) FROM ddex_partner WHERE cardinality(allowed_versions) <> 0;')"
test "${legacy_rows}" = "0"

schema_before="$(psql "${database_url}" -X -qAt -v ON_ERROR_STOP=1 <<'SQL'
SELECT md5(string_agg(definition, E'\n' ORDER BY definition))
FROM (
  SELECT 'column:' || table_schema || '.' || table_name || '.' || column_name || ':' || data_type || ':' || is_nullable || ':' || coalesce(column_default, '') AS definition
  FROM information_schema.columns
  WHERE table_schema = 'public'
  UNION ALL
  SELECT 'constraint:' || conrelid::regclass::text || '.' || conname || ':' || pg_get_constraintdef(oid, true)
  FROM pg_constraint
  WHERE connamespace = 'public'::regnamespace
  UNION ALL
  SELECT 'index:' || schemaname || '.' || indexname || ':' || indexdef
  FROM pg_indexes
  WHERE schemaname = 'public'
) schema_objects;
SQL
)"

start_and_verify

schema_after="$(psql "${database_url}" -X -qAt -v ON_ERROR_STOP=1 <<'SQL'
SELECT md5(string_agg(definition, E'\n' ORDER BY definition))
FROM (
  SELECT 'column:' || table_schema || '.' || table_name || '.' || column_name || ':' || data_type || ':' || is_nullable || ':' || coalesce(column_default, '') AS definition
  FROM information_schema.columns
  WHERE table_schema = 'public'
  UNION ALL
  SELECT 'constraint:' || conrelid::regclass::text || '.' || conname || ':' || pg_get_constraintdef(oid, true)
  FROM pg_constraint
  WHERE connamespace = 'public'::regnamespace
  UNION ALL
  SELECT 'index:' || schemaname || '.' || indexname || ':' || indexdef
  FROM pg_indexes
  WHERE schemaname = 'public'
) schema_objects;
SQL
)"

test -n "${schema_before}"
test "${schema_after}" = "${schema_before}"
echo "Automatic migrations passed against the fully cut-over production schema and were idempotent"
