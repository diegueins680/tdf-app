#!/bin/sh
# AUTHORITY-094 / AUTH-SOCIAL-OAUTH-001: the social sync account delivery
# designation migration on a disposable PostgreSQL. Forward, rollback,
# idempotent re-apply, and constraint probes with the columns the migration
# reads. No production connection input is accepted.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container="tdf-social-designation-test-$$"
owned=false
cleanup() {
  if [ "$owned" = true ]; then
    docker rm -f "$test_container" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM
docker run --rm -d --name "$test_container" -p 127.0.0.1::5432 \
  -e POSTGRES_PASSWORD=social-designation-fixture -e POSTGRES_DB=social_designation_test \
  postgres:16.10-alpine@sha256:029660641a0cfc575b14f336ba448fb8a75fd595d42e1fa316b9fb4378742297 >/dev/null
owned=true
port=$(docker port "$test_container" 5432/tcp | head -n 1 | sed 's/.*://')
url="postgresql://postgres:social-designation-fixture@127.0.0.1:${port}/social_designation_test"
attempt=0
until psql "$url" -X -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  test "$attempt" -lt 30 || { docker logs "$test_container" >&2; exit 1; }
  sleep 1
done
run() { psql "$url" -X -q -v ON_ERROR_STOP=1 "$@"; }
must_fail() { # $1 description, $2 sql
  if psql "$url" -X -qAtc "$2" >/dev/null 2>&1; then
    echo "Expected failure: $1" >&2; exit 1
  fi
}
run <<'SQL'
CREATE TABLE public.party (id BIGSERIAL PRIMARY KEY);
CREATE TABLE public.artist_profile (id BIGSERIAL PRIMARY KEY);
CREATE TABLE public.social_sync_account (
  id BIGSERIAL PRIMARY KEY,
  party_id BIGINT REFERENCES public.party(id),
  artist_profile_id BIGINT REFERENCES public.artist_profile(id),
  platform VARCHAR NOT NULL, external_user_id VARCHAR NOT NULL, handle VARCHAR,
  access_token VARCHAR, token_expires_at TIMESTAMPTZ, status VARCHAR NOT NULL,
  last_synced_at TIMESTAMPTZ, created_at TIMESTAMPTZ NOT NULL, updated_at TIMESTAMPTZ,
  CONSTRAINT unique_social_sync_account UNIQUE (platform, external_user_id));
INSERT INTO public.party DEFAULT VALUES; INSERT INTO public.party DEFAULT VALUES;
INSERT INTO public.social_sync_account (party_id, platform, external_user_id, access_token, status, created_at)
  VALUES (1, 'instagram', 'ig-1', 'token', 'connected', now());
SQL
forward="$repo_root/tdf-hq/sql/2026-11-01_social_sync_account_delivery_designation.sql"
rollback="$repo_root/tdf-hq/sql/2026-11-01_social_sync_account_delivery_designation_rollback.sql"
run -f "$forward" >/dev/null
run -f "$rollback" >/dev/null
run -f "$forward" >/dev/null
run -f "$forward" >/dev/null 2>&1
# Existing accounts are not designated after the migration.
test "$(psql "$url" -X -qAtc "SELECT count(*) FROM social_sync_account WHERE delivery_designated")" = 0 || { echo 'Migration designated an existing account' >&2; exit 1; }
must_fail 'designated without designator' "UPDATE social_sync_account SET delivery_designated = TRUE WHERE id = 1"
must_fail 'designator without designation' "UPDATE social_sync_account SET designated_by_party_id = 2 WHERE id = 1"
must_fail 'unknown event type' "INSERT INTO social_sync_account_event (account_id, event_type) VALUES (1, 'renamed')"
run -c "UPDATE social_sync_account SET delivery_designated = TRUE, designated_by_party_id = 2, designated_at = now() WHERE id = 1" >/dev/null
run -c "INSERT INTO social_sync_account_event (account_id, actor_party_id, event_type, from_party_id, to_party_id) VALUES (1, 2, 'designated', 1, 1), (1, 2, 'rebound', 1, 2), (1, 1, 'rebind_refused', 1, 2)" >/dev/null
test "$(psql "$url" -X -qAtc "SELECT count(*) FROM social_sync_account_event WHERE account_id = 1")" = 3 || { echo 'Events not recorded' >&2; exit 1; }
run -c "UPDATE social_sync_account SET delivery_designated = FALSE, designated_by_party_id = NULL, designated_at = NULL WHERE id = 1" >/dev/null
echo 'Social delivery designation migration checked: forward, rollback, idempotent re-apply, no implicit designation, 3 rejected shapes, audit events recorded'
