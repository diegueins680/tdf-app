#!/usr/bin/env bash
set -euo pipefail

container_name="tdf-ddex-partner-legacy-test-${RANDOM}"
database_name="tdf_ddex_partner_legacy_test"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
up_migration="${repo_root}/tdf-hq/sql/2026-08-18_ddex_partner_legacy_compatibility.sql"
rollback_migration="${repo_root}/tdf-hq/sql/2026-08-18_ddex_partner_legacy_compatibility_rollback.sql"

cleanup() {
  docker rm -f "${container_name}" >/dev/null 2>&1 || true
}
trap cleanup EXIT

docker run --detach --rm \
  --name "${container_name}" \
  --env POSTGRES_PASSWORD=postgres \
  --env POSTGRES_DB="${database_name}" \
  postgres:16-alpine >/dev/null

database_ready=0
for _ in $(seq 1 30); do
  if docker exec "${container_name}" pg_isready -U postgres -d "${database_name}" >/dev/null 2>&1; then
    database_ready=1
    break
  fi
  sleep 1
done
if [ "${database_ready}" -ne 1 ]; then
  docker logs "${container_name}" >&2
  echo "PostgreSQL DDEX compatibility test container did not become ready" >&2
  exit 1
fi

psql_test() {
  docker exec -i "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" "$@"
}

psql_test <<'SQL'
CREATE TABLE ddex_partner (
    id serial PRIMARY KEY,
    name text NOT NULL UNIQUE,
    dpid text,
    allowed_versions text[] NOT NULL DEFAULT ARRAY['4.3.2']::text[],
    rules_json jsonb,
    naming_convention text,
    is_active boolean NOT NULL DEFAULT true
);
SQL

psql_test < "${up_migration}"
psql_test < "${up_migration}"

empty_default="$(psql_test -qAt -c "INSERT INTO ddex_partner (name) VALUES ('canonical-default') RETURNING cardinality(allowed_versions);")"
test "${empty_default//[[:space:]]/}" = "0"

psql_test < "${rollback_migration}"
legacy_default="$(psql_test -qAt -c "INSERT INTO ddex_partner (name) VALUES ('rollback-default') RETURNING array_to_string(allowed_versions, ',');")"
test "${legacy_default//[[:space:]]/}" = "4.3.2"

psql_test -c "UPDATE ddex_partner SET allowed_versions=ARRAY[]::text[];" >/dev/null
psql_test < "${up_migration}"
psql_test -c "UPDATE ddex_partner SET allowed_versions=ARRAY['legacy-conflict']::text[] WHERE name='canonical-default';" >/dev/null
if psql_test < "${up_migration}" >/dev/null 2>&1; then
  echo "expected migration to reject unresolved legacy DDEX values" >&2
  exit 1
fi

echo "DDEX partner legacy compatibility migration test passed"
