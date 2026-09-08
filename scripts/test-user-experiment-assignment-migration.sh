#!/bin/sh
set -eu

experiment_container="tdf-experiment-assignment-test-$$"
experiment_database="tdf_experiment_assignment_test"
experiment_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
up_migration="$experiment_root/tdf-hq/sql/2026-09-07_user_experiment_assignment.sql"
rollback_migration="$experiment_root/tdf-hq/sql/2026-09-07_user_experiment_assignment_rollback.sql"

cleanup() {
  docker rm -f "$experiment_container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$experiment_container" \
  -e POSTGRES_PASSWORD=experiment-assignment-test \
  -e POSTGRES_DB="$experiment_database" \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$experiment_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$experiment_database" -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    docker logs "$experiment_container" >&2
    echo "PostgreSQL experiment assignment migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec "$experiment_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$experiment_database" "$@"
}

apply_sql() {
  docker exec -i "$experiment_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$experiment_database" < "$1" >/dev/null
}

psql_exec -c 'CREATE TABLE party (id BIGSERIAL PRIMARY KEY); INSERT INTO party DEFAULT VALUES; INSERT INTO party DEFAULT VALUES;' >/dev/null
apply_sql "$up_migration"
apply_sql "$up_migration"

test "$(psql_exec -qAt -c "SELECT count(*) FROM information_schema.columns WHERE table_schema='public' AND table_name='user_experiment_assignment';")" = "8"
test "$(psql_exec -qAt -c "SELECT count(*) FROM pg_constraint WHERE conrelid='public.user_experiment_assignment'::regclass AND convalidated AND contype IN ('f','u','c');")" = "7"
test "$(psql_exec -qAt -c "SELECT count(*) FROM pg_indexes WHERE schemaname='public' AND indexname='user_experiment_assignment_pending_exposure_idx';")" = "1"

psql_exec -c "INSERT INTO user_experiment_assignment (party_id,experiment_id,experiment_version,variant,assigned_at,eligible_until) VALUES (1,'single-feature-onboarding-v1',1,'control','2026-09-07T10:00:00Z','2026-09-08T10:00:00Z');" >/dev/null
if psql_exec -c "INSERT INTO user_experiment_assignment (party_id,experiment_id,experiment_version,variant,assigned_at,eligible_until) VALUES (1,'single-feature-onboarding-v1',1,'treatment_singlefeature',now(),now());" >/dev/null 2>&1; then
  echo "expected duplicate Party/version assignment to be rejected" >&2
  exit 1
fi
if psql_exec -c "INSERT INTO user_experiment_assignment (party_id,experiment_id,experiment_version,variant,assigned_at,eligible_until) VALUES (2,'unknown-experiment',1,'control',now(),now());" >/dev/null 2>&1; then
  echo "expected unsupported experiment identity to be rejected" >&2
  exit 1
fi
if psql_exec -c "INSERT INTO user_experiment_assignment (party_id,experiment_id,experiment_version,variant,assigned_at,eligible_until) VALUES (2,'single-feature-onboarding-v1',1,'other-arm',now(),now());" >/dev/null 2>&1; then
  echo "expected unsupported experiment variant to be rejected" >&2
  exit 1
fi
if psql_exec -c "UPDATE user_experiment_assignment SET exposed_at='2026-09-09T10:00:00Z' WHERE party_id=1;" >/dev/null 2>&1; then
  echo "expected out-of-window exposure to be rejected" >&2
  exit 1
fi

apply_sql "$rollback_migration"
apply_sql "$rollback_migration"
test "$(psql_exec -qAt -c "SELECT count(*) FROM user_experiment_assignment WHERE party_id=1;")" = "1"
psql_exec -c 'DELETE FROM party WHERE id=1;' >/dev/null
test "$(psql_exec -qAt -c 'SELECT count(*) FROM user_experiment_assignment WHERE party_id=1;')" = "0"
apply_sql "$up_migration"

echo "User experiment assignment migration passed forward, idempotency, constraints, cascade, rollback, and reapply checks."
