#!/usr/bin/env bash
# Complete schema-only baseline plus catalog fixture and all registered migrations.
# No existing database URL or application .env is accepted by this harness.
set -euo pipefail
TDF_SOCIAL_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
TDF_SOCIAL_SCHEMA_DATA=$(mktemp -d)
if [ "${TDF_SOCIAL_SCHEMA_NATIVE:-0}" = 1 ]; then
TDF_SOCIAL_PG_BIN=${TDF_SOCIAL_PG_BIN:-/usr/local/opt/postgresql@16/bin}
TDF_SOCIAL_SCHEMA_PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()')
"$TDF_SOCIAL_PG_BIN/initdb" -D "$TDF_SOCIAL_SCHEMA_DATA/db" -U postgres -A trust --no-locale -E UTF8 >/dev/null
trap '"$TDF_SOCIAL_PG_BIN/pg_ctl" -D "$TDF_SOCIAL_SCHEMA_DATA/db" -m immediate -w stop >/dev/null 2>&1 || true' EXIT
"$TDF_SOCIAL_PG_BIN/pg_ctl" -D "$TDF_SOCIAL_SCHEMA_DATA/db" -l "$TDF_SOCIAL_SCHEMA_DATA/server.log" \
  -o "-h 127.0.0.1 -p $TDF_SOCIAL_SCHEMA_PORT -k $TDF_SOCIAL_SCHEMA_DATA" -w start >/dev/null
"$TDF_SOCIAL_PG_BIN/createdb" -h 127.0.0.1 -p "$TDF_SOCIAL_SCHEMA_PORT" -U postgres social_schema
psql_schema() { "$TDF_SOCIAL_PG_BIN/psql" -h 127.0.0.1 -p "$TDF_SOCIAL_SCHEMA_PORT" -X -v ON_ERROR_STOP=1 -U postgres -d social_schema "$@"; }
else
TDF_SOCIAL_SCHEMA_CONTAINER="tdf-social-full-schema-$$"
trap 'docker rm -f "$TDF_SOCIAL_SCHEMA_CONTAINER" >/dev/null 2>&1 || true' EXIT
docker run --rm -d --name "$TDF_SOCIAL_SCHEMA_CONTAINER" -e POSTGRES_PASSWORD=synthetic-only \
  -e POSTGRES_DB=social_schema pgvector/pgvector:pg17 >/dev/null
# The image starts a temporary socket-only server during initialization.
# Wait for TCP so its shutdown cannot interrupt the first fixture query.
for attempt in $(seq 1 30); do
  if docker exec "$TDF_SOCIAL_SCHEMA_CONTAINER" pg_isready -h 127.0.0.1 -U postgres -d social_schema >/dev/null 2>&1; then break; fi
  sleep 1
done
docker exec "$TDF_SOCIAL_SCHEMA_CONTAINER" pg_isready -h 127.0.0.1 -U postgres -d social_schema
psql_schema() { docker exec -i "$TDF_SOCIAL_SCHEMA_CONTAINER" psql -X -v ON_ERROR_STOP=1 -U postgres -d social_schema "$@"; }
fi
psql_schema -Atc 'SELECT version();' 
psql_schema < "$TDF_SOCIAL_ROOT/scripts/__tests__/fixtures/production-schema-20260814.sql" > "$TDF_SOCIAL_SCHEMA_DATA/schema.txt" 2>&1 || { tail -30 "$TDF_SOCIAL_SCHEMA_DATA/schema.txt"; exit 1; }
psql_schema < "$TDF_SOCIAL_ROOT/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" > "$TDF_SOCIAL_SCHEMA_DATA/catalog.txt" 2>&1 || { tail -30 "$TDF_SOCIAL_SCHEMA_DATA/catalog.txt"; exit 1; }
node "$TDF_SOCIAL_ROOT/scripts/render-production-migration-batch.mjs" > "$TDF_SOCIAL_SCHEMA_DATA/migrations.sql"
psql_schema < "$TDF_SOCIAL_SCHEMA_DATA/migrations.sql" > "$TDF_SOCIAL_SCHEMA_DATA/migrations.txt" 2>&1 || { tail -50 "$TDF_SOCIAL_SCHEMA_DATA/migrations.txt"; exit 1; }
psql_schema -Atc 'SELECT count(*) AS registered_migrations FROM tdf_schema_migration;'
psql_schema < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_foundation.sql"
psql_schema < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_read_models.sql"
psql_schema < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_dm_write_boundary.sql"
psql_schema < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_chat_api.sql"
psql_schema < "$TDF_SOCIAL_ROOT/scripts/social/schema-compatibility.sql"
psql_schema < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_pause.sql"
psql_schema < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_foundation.sql"
psql_schema < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_read_models.sql"
psql_schema < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_dm_write_boundary.sql"
psql_schema < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_chat_api.sql"
psql_schema <<'SQL'
DO $$ BEGIN
  ASSERT NOT (SELECT enabled FROM social_v2_runtime);
  ASSERT (SELECT activated_once FROM social_v2_runtime);
  ASSERT social_v2_chat_threads(900000001)->'result'='[]'::jsonb;
  ASSERT social_v2_chat_messages(900000001,900000001,NULL,NULL,50)->>'error'='unavailable';
  ASSERT social_v2_chat_send(900000001,900000001,'denied',true)->>'error'='forbidden';
  ASSERT (SELECT count(*) FROM chat_message WHERE body='Synthetic legacy DM')=1;
  BEGIN
    INSERT INTO chat_message(thread_id,sender_party_id,body,created_at)
      VALUES(900000001,900000001,'Blocked legacy retry',now());
    RAISE EXCEPTION 'paused legacy DM insert succeeded';
  EXCEPTION WHEN insufficient_privilege THEN NULL; END;
  ASSERT social_v2_mutate(900000001,900000002,'unblock',3,'paused')->>'error'='disabled';
  ASSERT (SELECT block_a FROM social_v2_pair WHERE party_a=900000001 AND party_b=900000002);
  ASSERT (SELECT personalized=false AND discoverable FROM social_v2_preference WHERE party_id=900000001);
  ASSERT (SELECT jsonb_agg(to_jsonb(p) ORDER BY position) FROM social_v2_publication p)=(SELECT publications FROM social_schema_preserved);
  ASSERT (SELECT jsonb_agg(to_jsonb(c) ORDER BY actor,request_key) FROM social_v2_command c)=(SELECT commands FROM social_schema_preserved);
END $$;
SQL
psql_schema < "$TDF_SOCIAL_ROOT/scripts/social/reconcile.sql"
echo "PASS: complete schema fixture, registered migrations, additive reapply, resumable publication, legacy source writes and pause preserve new data"
echo "Fixture logs: $TDF_SOCIAL_SCHEMA_DATA (private database cleanup runs on exit)"
