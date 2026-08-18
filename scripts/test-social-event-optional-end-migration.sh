#!/usr/bin/env bash
set -euo pipefail

container_name="tdf-social-event-optional-end-test-${RANDOM}"
database_name="tdf_optional_end_test"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
up_migration="${repo_root}/tdf-hq/sql/2026-08-17_social_event_optional_end.sql"
rollback_migration="${repo_root}/tdf-hq/sql/2026-08-17_social_event_optional_end_rollback.sql"

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
  echo "PostgreSQL test container did not become ready" >&2
  exit 1
fi

docker exec -i "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" <<'SQL'
CREATE TABLE social_event (
    id bigserial PRIMARY KEY,
    start_time timestamptz NOT NULL,
    end_time timestamptz NOT NULL
);
INSERT INTO social_event (start_time, end_time)
VALUES ('2026-08-17T20:00:00Z', '2026-08-17T22:00:00Z');
SQL

docker exec -i "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" < "${up_migration}"
docker exec -i "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" < "${up_migration}"

nullable="$({ docker exec "${container_name}" psql -X -qAt -U postgres -d "${database_name}" -c "SELECT is_nullable FROM information_schema.columns WHERE table_schema='public' AND table_name='social_event' AND column_name='end_time';"; } | tr -d '[:space:]')"
test "${nullable}" = "YES"

constraint_state="$({ docker exec "${container_name}" psql -X -qAt -U postgres -d "${database_name}" -c "SELECT contype::text || ':' || convalidated::text FROM pg_constraint WHERE conrelid='public.social_event'::regclass AND conname='social_event_time_order';"; } | tr -d '[:space:]')"
test "${constraint_state}" = "c:true"

docker exec "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" -c \
  "INSERT INTO social_event (start_time, end_time) VALUES ('2026-08-18T20:00:00Z', NULL);" >/dev/null

if docker exec "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" -c \
  "INSERT INTO social_event (start_time, end_time) VALUES ('2026-08-18T20:00:00Z', '2026-08-18T19:00:00Z');" >/dev/null 2>&1; then
  echo "expected invalid event time order to be rejected" >&2
  exit 1
fi

if docker exec -i "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" < "${rollback_migration}" >/dev/null 2>&1; then
  echo "expected rollback to refuse lossy NOT NULL restoration" >&2
  exit 1
fi

docker exec "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" -c \
  "DELETE FROM social_event WHERE end_time IS NULL;" >/dev/null
docker exec -i "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" < "${rollback_migration}"
docker exec -i "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" < "${rollback_migration}"

nullable="$({ docker exec "${container_name}" psql -X -qAt -U postgres -d "${database_name}" -c "SELECT is_nullable FROM information_schema.columns WHERE table_schema='public' AND table_name='social_event' AND column_name='end_time';"; } | tr -d '[:space:]')"
test "${nullable}" = "NO"

docker exec -i "${container_name}" psql -X -v ON_ERROR_STOP=1 -U postgres -d "${database_name}" < "${up_migration}"

echo "social_event optional end migration test passed"
