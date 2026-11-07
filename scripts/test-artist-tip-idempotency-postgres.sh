#!/bin/sh
# PAY-TIP-001: the artist tip idempotency migration on a disposable PostgreSQL.
# Applies the forward migration, rolls it back, applies it twice (idempotent),
# and probes the constraints with the columns the migration reads. No
# production connection input is accepted.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container="tdf-artist-tip-idempotency-test-$$"
owned=false
cleanup() {
  if [ "$owned" = true ]; then
    docker rm -f "$test_container" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM
docker run --rm -d --name "$test_container" -p 127.0.0.1::5432 \
  -e POSTGRES_PASSWORD=artist-tip-fixture -e POSTGRES_DB=artist_tip_test \
  postgres:16.10-alpine@sha256:029660641a0cfc575b14f336ba448fb8a75fd595d42e1fa316b9fb4378742297 >/dev/null
owned=true
port=$(docker port "$test_container" 5432/tcp | head -n 1 | sed 's/.*://')
url="postgresql://postgres:artist-tip-fixture@127.0.0.1:${port}/artist_tip_test"
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
CREATE TABLE party (id BIGSERIAL PRIMARY KEY);
CREATE TABLE artist_profile (id BIGSERIAL PRIMARY KEY, artist_party_id BIGINT NOT NULL UNIQUE REFERENCES party(id));
CREATE TABLE artist_tip (
  id BIGSERIAL PRIMARY KEY,
  artist_profile_id BIGINT NOT NULL REFERENCES artist_profile(id) ON DELETE CASCADE,
  tipper_party_id BIGINT, tipper_email TEXT, tipper_name TEXT,
  amount_cents INTEGER NOT NULL CHECK (amount_cents > 0),
  currency TEXT NOT NULL,
  platform_fee_cents INTEGER NOT NULL CHECK (platform_fee_cents >= 0),
  stripe_payment_intent_id TEXT,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','paid','failed','refunded')),
  message TEXT,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(), updated_at TIMESTAMP NOT NULL DEFAULT NOW());
INSERT INTO party DEFAULT VALUES;
INSERT INTO artist_profile (artist_party_id) VALUES (1);
INSERT INTO artist_tip (artist_profile_id, amount_cents, currency, platform_fee_cents) VALUES (1, 500, 'usd', 50);
SQL
forward="$repo_root/tdf-hq/sql/2026-11-01_artist_tip_idempotency.sql"
rollback="$repo_root/tdf-hq/sql/2026-11-01_artist_tip_idempotency_rollback.sql"
run -f "$forward" >/dev/null
run -f "$rollback" >/dev/null
run -f "$forward" >/dev/null
run -f "$forward" >/dev/null 2>&1
test "$(psql "$url" -X -qAtc 'SELECT count(*) FROM artist_tip')" = 1 || { echo 'Pre-existing keyless tip lost' >&2; exit 1; }
run -c "INSERT INTO artist_tip (artist_profile_id, amount_cents, currency, platform_fee_cents, idempotency_key, request_sha256) VALUES (1, 100, 'usd', 10, 'k1', repeat('a',64))" >/dev/null
must_fail 'same artist and key' "INSERT INTO artist_tip (artist_profile_id, amount_cents, currency, platform_fee_cents, idempotency_key, request_sha256) VALUES (1, 200, 'usd', 20, 'k1', repeat('b',64))"
must_fail 'malformed key' "INSERT INTO artist_tip (artist_profile_id, amount_cents, currency, platform_fee_cents, idempotency_key, request_sha256) VALUES (1, 200, 'usd', 20, 'k 2', repeat('b',64))"
must_fail 'key without fingerprint' "INSERT INTO artist_tip (artist_profile_id, amount_cents, currency, platform_fee_cents, idempotency_key) VALUES (1, 200, 'usd', 20, 'k3')"
must_fail 'fingerprint without key' "INSERT INTO artist_tip (artist_profile_id, amount_cents, currency, platform_fee_cents, request_sha256) VALUES (1, 200, 'usd', 20, repeat('c',64))"
must_fail 'uppercase fingerprint' "INSERT INTO artist_tip (artist_profile_id, amount_cents, currency, platform_fee_cents, idempotency_key, request_sha256) VALUES (1, 200, 'usd', 20, 'k5', repeat('A',64))"
# The same key for another artist and keyless duplicates stay allowed.
run <<'SQL' >/dev/null
INSERT INTO party DEFAULT VALUES;
INSERT INTO artist_profile (artist_party_id) VALUES (2);
INSERT INTO artist_tip (artist_profile_id, amount_cents, currency, platform_fee_cents, idempotency_key, request_sha256) VALUES (2, 100, 'usd', 10, 'k1', repeat('a',64));
INSERT INTO artist_tip (artist_profile_id, amount_cents, currency, platform_fee_cents) VALUES (1, 300, 'usd', 30), (1, 300, 'usd', 30);
SQL
test "$(psql "$url" -X -qAtc "SELECT count(*) FROM artist_tip WHERE idempotency_key IS NULL")" = 3 || { echo 'Keyless tips were constrained' >&2; exit 1; }
echo 'Artist tip idempotency migration checked: forward, rollback, idempotent re-apply, 5 rejected shapes, cross-artist key reuse allowed'
