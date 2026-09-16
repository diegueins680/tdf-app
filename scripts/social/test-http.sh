#!/usr/bin/env bash
set -euo pipefail
TDF_SOCIAL_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
if [ -n "${TDF_SOCIAL_HTTP_EMPTY_DATABASE_URL:-}" ]; then
  # CI's explicitly created throwaway database. Refuse another name or any data.
  psql_http() { psql "$TDF_SOCIAL_HTTP_EMPTY_DATABASE_URL" -X -v ON_ERROR_STOP=1 "$@"; }
  [ "$(psql_http -Atc "SELECT current_database()='tdf_hq_social_session_test' AND NOT EXISTS(SELECT 1 FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p','v','m'))")" = t ] || {
    echo 'Session tests require an empty tdf_hq_social_session_test database' >&2
    exit 1
  }
  TDF_SOCIAL_HTTP_CONNECTION="$TDF_SOCIAL_HTTP_EMPTY_DATABASE_URL"
elif [ "${TDF_SOCIAL_HTTP_NATIVE:-0}" = 1 ]; then
  # Fallback when Docker's shared VM/API is unavailable. Private cluster only.
  TDF_SOCIAL_PG_BIN=${TDF_SOCIAL_PG_BIN:-/usr/local/opt/postgresql@16/bin}
  TDF_SOCIAL_PG_DATA=$(mktemp -d)
  TDF_SOCIAL_PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()')
  "$TDF_SOCIAL_PG_BIN/initdb" -D "$TDF_SOCIAL_PG_DATA" -U postgres -A trust --no-locale -E UTF8 >/dev/null
  trap '"$TDF_SOCIAL_PG_BIN/pg_ctl" -D "$TDF_SOCIAL_PG_DATA" -m immediate -w stop >/dev/null 2>&1 || true' EXIT
  "$TDF_SOCIAL_PG_BIN/pg_ctl" -D "$TDF_SOCIAL_PG_DATA" -l "$TDF_SOCIAL_PG_DATA/server.log" \
    -o "-h 127.0.0.1 -p $TDF_SOCIAL_PORT -k $TDF_SOCIAL_PG_DATA" -w start >/dev/null
  "$TDF_SOCIAL_PG_BIN/createdb" -h 127.0.0.1 -p "$TDF_SOCIAL_PORT" -U postgres social_http
  psql_http() { "$TDF_SOCIAL_PG_BIN/psql" -h 127.0.0.1 -p "$TDF_SOCIAL_PORT" -X -v ON_ERROR_STOP=1 -U postgres -d social_http "$@"; }
else
  TDF_SOCIAL_CONTAINER="tdf-social-http-$$"
  trap 'docker rm -f "$TDF_SOCIAL_CONTAINER" >/dev/null 2>&1 || true' EXIT
  docker run --rm -d --name "$TDF_SOCIAL_CONTAINER" -p 127.0.0.1::5432 \
    -e POSTGRES_PASSWORD=synthetic-only -e POSTGRES_DB=social_http postgres:16-alpine >/dev/null
  for attempt in $(seq 1 30); do
    if docker exec "$TDF_SOCIAL_CONTAINER" pg_isready -h 127.0.0.1 -U postgres -d social_http >/dev/null 2>&1; then break; fi
    sleep 1
  done
  psql_http() { docker exec -i "$TDF_SOCIAL_CONTAINER" psql -X -v ON_ERROR_STOP=1 -U postgres -d social_http "$@"; }
  TDF_SOCIAL_PORT=$(docker port "$TDF_SOCIAL_CONTAINER" 5432/tcp | cut -d: -f2)
fi
psql_http <<'SQL'
CREATE TABLE party(id bigint PRIMARY KEY,display_name text NOT NULL,is_org boolean NOT NULL DEFAULT false);
CREATE TABLE user_credential(id bigint PRIMARY KEY,party_id bigint REFERENCES party(id),active boolean NOT NULL DEFAULT true);
INSERT INTO party SELECT n,'Synthetic '||n,false FROM generate_series(1,5) n;
INSERT INTO user_credential SELECT n,n,true FROM generate_series(1,5) n;
CREATE TABLE fan_profile(id bigserial PRIMARY KEY,fan_party_id bigint UNIQUE REFERENCES party(id),display_name text,avatar_url text,bio text,city text);
INSERT INTO fan_profile(fan_party_id,bio) SELECT n,'profile bio '||n FROM generate_series(1,5) n;
CREATE TABLE api_token(id bigint PRIMARY KEY,token text,party_id bigint,label text,active boolean);
INSERT INTO api_token SELECT n,'synthetic-'||n,n,NULL,true FROM generate_series(1,5) n;
INSERT INTO api_token VALUES(6,'synthetic-alt',1,NULL,true);
CREATE TABLE social_session_effect(id integer PRIMARY KEY);
CREATE TABLE party_follow(id bigserial PRIMARY KEY,follower_party_id bigint,following_party_id bigint,via_nfc boolean NOT NULL DEFAULT false,created_at timestamptz NOT NULL DEFAULT now(),UNIQUE(follower_party_id,following_party_id));
CREATE TABLE chat_thread(id bigserial PRIMARY KEY,dm_party_a bigint NOT NULL REFERENCES party(id),dm_party_b bigint NOT NULL REFERENCES party(id),created_at timestamptz NOT NULL DEFAULT now(),updated_at timestamptz NOT NULL DEFAULT now(),UNIQUE(dm_party_a,dm_party_b));
CREATE TABLE chat_message(id bigserial PRIMARY KEY,thread_id bigint NOT NULL REFERENCES chat_thread(id),sender_party_id bigint NOT NULL REFERENCES party(id),body text NOT NULL,created_at timestamptz NOT NULL DEFAULT now());
INSERT INTO chat_thread(dm_party_a,dm_party_b) VALUES(1,2),(2,3);
INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'model first'),(1,2,'model cursor'),(2,2,'foreign secret');
CREATE TABLE party_security_role(id uuid,party_id bigint,role_id uuid,granted_by bigint,approved_by bigint,
  approval_mode text,emergency_reason text,source_revision_id uuid,source_policy_id uuid,active boolean,
  created_at timestamptz,revoked_at timestamptz,version integer);
CREATE TABLE security_role(id uuid,active boolean);
CREATE TABLE role_permission(role_id uuid,permission_id uuid,active boolean);
CREATE TABLE security_permission(id uuid,action_id uuid,module_id uuid,active boolean,resource_scope text);
CREATE TABLE security_action(id uuid,active boolean,code text);
CREATE TABLE security_module(id uuid,active boolean,code text);
SQL
psql_http < "$TDF_SOCIAL_ROOT/scripts/social/fixture.sql"
psql_http < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_foundation.sql"
psql_http < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_read_models.sql"
psql_http < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_dm_write_boundary.sql"
psql_http < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_chat_api.sql"
psql_http < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_profile_reads.sql"
psql_http < "$TDF_SOCIAL_ROOT/scripts/social/dm-read-refinement.sql"
# psql \ir needs a real local path in Docker too, so concatenate the control and
# generated cases instead of relying on the container seeing the checkout.
TDF_SOCIAL_READ_NEGATIVE=$(mktemp)
if { sed '/^\\ir /,$d' "$TDF_SOCIAL_ROOT/scripts/social/dm-read-negative.sql";
     cat "$TDF_SOCIAL_ROOT/scripts/social/dm-read-model-cases.sql"; echo 'ROLLBACK;'; } |
   psql_http > "$TDF_SOCIAL_READ_NEGATIVE" 2>&1; then
  echo 'FAIL: unsafe membership-only read unexpectedly passed generated cases' >&2
  exit 1
fi
grep 'DmReads observed case .* mismatch' "$TDF_SOCIAL_READ_NEGATIVE"
echo 'PASS: generated cases reject the deliberately unsafe read projection'
psql_http < "$TDF_SOCIAL_ROOT/scripts/social/dm-read-model-cases.sql"
echo 'PASS: 1440 checked model outcomes refine thread preview/message/cursor policy on PostgreSQL'
psql_http < "$TDF_SOCIAL_ROOT/scripts/social/profile-read-refinement.sql"
TDF_SOCIAL_PROFILE_NEGATIVE=$(mktemp)
if { cat "$TDF_SOCIAL_ROOT/scripts/social/profile-read-negative.sql";
     cat "$TDF_SOCIAL_ROOT/scripts/social/profile-read-model-cases.sql"; echo 'ROLLBACK;'; } |
   psql_http > "$TDF_SOCIAL_PROFILE_NEGATIVE" 2>&1; then
  echo 'FAIL: unsafe profile policy unexpectedly passed generated cases' >&2
  exit 1
fi
grep 'ProfileReads observed case .* mismatch' "$TDF_SOCIAL_PROFILE_NEGATIVE"
echo 'PASS: generated cases reject the deliberately unsafe profile policy'
psql_http < "$TDF_SOCIAL_ROOT/scripts/social/profile-read-model-cases.sql"
echo 'PASS: checked profile outcomes refine PostgreSQL policy, ordering and payload'
if [ "${TDF_SOCIAL_CHAT_SQL_ONLY:-0}" = 1 ]; then
  echo 'SQL-only mode: HTTP tests were not run'
  exit 0
fi
if [ -z "${TDF_SOCIAL_HTTP_CONNECTION:-}" ]; then
  TDF_SOCIAL_HTTP_CONNECTION="host=127.0.0.1 port=$TDF_SOCIAL_PORT user=postgres password=synthetic-only dbname=social_http connect_timeout=5"
fi
export TDF_SOCIAL_HTTP_DB="$TDF_SOCIAL_HTTP_CONNECTION"
cd "$TDF_SOCIAL_ROOT"
TDF_SOCIAL_HTTP_BUILD=${TDF_SOCIAL_HTTP_BUILD:-$(mktemp -d)}
# Models.hs exceeds GHC 9.10.3's bytecode breakpoint-index bound. Compile object
# code with the repository's Stack toolchain instead of the interpreter.
stack --stack-yaml tdf-hq/stack.yaml exec -- ghc -O0 -threaded -itdf-hq/src -iscripts/social \
  -outputdir "$TDF_SOCIAL_HTTP_BUILD" scripts/social/HttpSpec.hs -o "$TDF_SOCIAL_HTTP_BUILD/social-http-spec"
"$TDF_SOCIAL_HTTP_BUILD/social-http-spec" --fail-fast
