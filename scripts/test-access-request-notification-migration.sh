#!/bin/sh
set -eu

test_container="tdf-access-request-notification-test-$$"
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
up_migration="$repo_root/tdf-hq/sql/2026-09-04_access_request_notification_types.sql"
down_migration="$repo_root/tdf-hq/sql/2026-09-04_access_request_notification_types_rollback.sql"
nullability_repair="$repo_root/tdf-hq/sql/2026-09-08_notification_notif_type_not_null_repair.sql"

cleanup() {
  docker rm -f "$test_container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$test_container" \
  -e POSTGRES_PASSWORD=access-request-notification-test \
  -e POSTGRES_DB=access_request_notification_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$test_container" \
  psql -v ON_ERROR_STOP=1 -U postgres -d access_request_notification_test -Atc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Access-request notification migration database did not become queryable" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec "$test_container" \
    psql -v ON_ERROR_STOP=1 -U postgres -d access_request_notification_test "$@"
}

insert_notification() {
  notification_type="$1"
  psql_exec -c "INSERT INTO notification (recipient_party_id, notif_type, title, body)
    VALUES (1, '$notification_type', 'test', 'test');" >/dev/null
}

apply_file() {
  docker exec -i "$test_container" \
    psql -v ON_ERROR_STOP=1 -U postgres -d access_request_notification_test \
    < "$1" >/dev/null
}

psql_exec -c 'CREATE TABLE party (id BIGSERIAL PRIMARY KEY);' >/dev/null
psql_exec -c 'INSERT INTO party DEFAULT VALUES;' >/dev/null
apply_file "$repo_root/tdf-hq/sql/2026-07-12_notification_table.sql"
psql_exec -c 'ALTER TABLE notification ALTER COLUMN notif_type DROP NOT NULL;' >/dev/null
psql_exec -c "INSERT INTO notification (recipient_party_id, notif_type, title, body)
  VALUES (1, NULL, 'preserve', 'preserve');" >/dev/null
if apply_file "$up_migration" 2>/dev/null; then
  echo "Access-request migration accepted a NULL notification type" >&2
  exit 1
fi
if [ "$(psql_exec -Atc 'SELECT count(*) FROM notification WHERE notif_type IS NULL;')" != "1" ]; then
  echo "Access-request migration did not preserve the rejected NULL row" >&2
  exit 1
fi
psql_exec -c 'DELETE FROM notification WHERE notif_type IS NULL;' >/dev/null
apply_file "$up_migration"
apply_file "$up_migration"

notif_type_nullable=$(psql_exec -Atc "SELECT is_nullable FROM information_schema.columns WHERE table_schema='public' AND table_name='notification' AND column_name='notif_type';")
if [ "$notif_type_nullable" != "NO" ]; then
  echo "Access-request migration did not restore notification.notif_type NOT NULL" >&2
  exit 1
fi

psql_exec -c 'ALTER TABLE notification ALTER COLUMN notif_type DROP NOT NULL;' >/dev/null
psql_exec -c "INSERT INTO notification (recipient_party_id, notif_type, title, body)
  VALUES (1, NULL, 'preserve', 'preserve');" >/dev/null
if apply_file "$nullability_repair" 2>/dev/null; then
  echo "Notification nullability repair accepted a NULL notification type" >&2
  exit 1
fi
if [ "$(psql_exec -Atc 'SELECT count(*) FROM notification WHERE notif_type IS NULL;')" != "1" ]; then
  echo "Notification nullability repair did not preserve the rejected NULL row" >&2
  exit 1
fi
psql_exec -c 'DELETE FROM notification WHERE notif_type IS NULL;' >/dev/null
apply_file "$nullability_repair"
apply_file "$nullability_repair"

notif_type_nullable=$(psql_exec -Atc "SELECT is_nullable FROM information_schema.columns WHERE table_schema='public' AND table_name='notification' AND column_name='notif_type';")
if [ "$notif_type_nullable" != "NO" ]; then
  echo "Notification nullability repair did not restore notification.notif_type NOT NULL" >&2
  exit 1
fi

for notification_type in \
  reaction_received post_trending weekly_top artist_liked \
  access_request_submitted access_request_review access_request_decided; do
  insert_notification "$notification_type"
done

if insert_notification unknown_type 2>/dev/null; then
  echo "Expanded notification constraint accepted an unknown type" >&2
  exit 1
fi

if apply_file "$down_migration" 2>/dev/null; then
  echo "Rollback discarded access-request notification compatibility with live rows present" >&2
  exit 1
fi

psql_exec -c "DELETE FROM notification WHERE notif_type LIKE 'access_request_%';" >/dev/null
apply_file "$down_migration"
apply_file "$down_migration"

if insert_notification access_request_submitted 2>/dev/null; then
  echo "Rollback left access-request notification types enabled" >&2
  exit 1
fi

apply_file "$up_migration"
apply_file "$up_migration"
for notification_type in access_request_submitted access_request_review access_request_decided; do
  insert_notification "$notification_type"
done

access_request_rows=$(psql_exec -Atc \
  "SELECT count(*) FROM notification WHERE notif_type LIKE 'access_request_%';")
if [ "$access_request_rows" != "3" ]; then
  echo "Expected all three access-request notification types to persist" >&2
  exit 1
fi

# Reproduce the pre-ledger staging baseline: varchar, a named NOT NULL
# constraint, no type allowlist, and notification types emitted by another
# valid producer. The compatibility path must preserve those rows and widen
# varchar to the runtime's canonical text representation without inventing a
# restrictive check constraint.
psql_exec -c 'DELETE FROM notification;' >/dev/null
psql_exec -c 'ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;' >/dev/null
psql_exec -c 'ALTER TABLE notification ALTER COLUMN notif_type TYPE varchar USING notif_type::varchar;' >/dev/null
psql_exec -c "INSERT INTO notification (recipient_party_id, notif_type, title, body)
  VALUES (1, 'test_sink_captured', 'test', 'test'),
         (1, 'test_sink_failed', 'test', 'test');" >/dev/null
apply_file "$up_migration"
apply_file "$up_migration"
apply_file "$nullability_repair"
apply_file "$nullability_repair"

legacy_notif_type=$(psql_exec -Atc "SELECT data_type || ':' || is_nullable
  FROM information_schema.columns
  WHERE table_schema='public' AND table_name='notification' AND column_name='notif_type';")
if [ "$legacy_notif_type" != "text:NO" ]; then
  echo "Legacy varchar notification baseline was not widened to non-null text" >&2
  exit 1
fi

legacy_type_rows=$(psql_exec -Atc "SELECT count(*) FROM notification
  WHERE notif_type IN ('test_sink_captured', 'test_sink_failed');")
if [ "$legacy_type_rows" != "2" ]; then
  echo "Legacy notification types were not preserved" >&2
  exit 1
fi

legacy_allowlist=$(psql_exec -Atc "SELECT count(*) FROM pg_constraint
  WHERE conrelid='public.notification'::regclass
    AND conname='notification_notif_type_check';")
if [ "$legacy_allowlist" != "0" ]; then
  echo "Legacy unconstrained notification baseline gained an unsafe allowlist" >&2
  exit 1
fi

echo "Access-request notification migration passed forward, idempotency,"
echo "constraint, rollback-safety, reapply, and legacy-varchar compatibility checks."
