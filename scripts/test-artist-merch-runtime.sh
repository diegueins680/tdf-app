#!/bin/sh
set -eu

TDF_MERCH_RUNTIME_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_MERCH_RUNTIME_CONTAINER="tdf-merch-runtime-test-$$"
TDF_MERCH_RUNTIME_DATABASE="tdf_merch_runtime_test"

cleanup() {
  docker rm -f "$TDF_MERCH_RUNTIME_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_MERCH_RUNTIME_CONTAINER" \
  -p 127.0.0.1::5432 \
  -e POSTGRES_HOST_AUTH_METHOD=trust \
  -e POSTGRES_DB="$TDF_MERCH_RUNTIME_DATABASE" \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_MERCH_RUNTIME_CONTAINER" pg_isready -U postgres -d "$TDF_MERCH_RUNTIME_DATABASE" >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 45 ]; then
    echo "Merch runtime test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

sleep 5
until docker exec "$TDF_MERCH_RUNTIME_CONTAINER" pg_isready -U postgres -d "$TDF_MERCH_RUNTIME_DATABASE" >/dev/null 2>&1; do
  sleep 1
done

apply_file() {
  file="$1"
  docker exec -i "$TDF_MERCH_RUNTIME_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_MERCH_RUNTIME_DATABASE" < "$file" >/dev/null
}

apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/sql/init_schema.sql"
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/sql/2026-08-14_catalog_canonical_schema.sql"
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/sql/2026-08-13_unified_checkout_core.sql"
docker exec -i "$TDF_MERCH_RUNTIME_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_MERCH_RUNTIME_DATABASE" <<'SQL' >/dev/null
CREATE TABLE directory_profile (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  subject_party_id BIGINT NOT NULL REFERENCES party(id),
  profile_kind TEXT NOT NULL,
  public_name TEXT NOT NULL,
  slug TEXT NOT NULL UNIQUE,
  profile_status TEXT NOT NULL,
  visibility TEXT NOT NULL,
  moderation_status TEXT NOT NULL
);
CREATE TABLE directory_profile_manager (
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  account_party_id BIGINT NOT NULL REFERENCES party(id),
  active BOOLEAN NOT NULL,
  can_manage BOOLEAN NOT NULL,
  source_claim_id UUID,
  PRIMARY KEY(profile_id,account_party_id)
);
CREATE TABLE directory_verification (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  status TEXT NOT NULL
);
SQL
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/sql/2026-09-07_artist_merch_storefronts.sql"
apply_file "$TDF_MERCH_RUNTIME_ROOT/tdf-hq/test/integration/artist_merch_runtime_fixture.sql"

published_address=$(docker port "$TDF_MERCH_RUNTIME_CONTAINER" 5432/tcp | head -1)
published_port=${published_address##*:}
if [ -z "$published_port" ]; then
  echo "Could not resolve the isolated PostgreSQL port" >&2
  exit 1
fi

cd "$TDF_MERCH_RUNTIME_ROOT/tdf-hq"
TDF_MERCH_RUNTIME_DATABASE_URL="postgresql://postgres@127.0.0.1:$published_port/$TDF_MERCH_RUNTIME_DATABASE" \
DATABASE_URL="postgresql://postgres@127.0.0.1:$published_port/$TDF_MERCH_RUNTIME_DATABASE" \
APP_ENV=sandbox \
RUN_MIGRATIONS=false \
RESET_DB=false \
SEED_DB=false \
MERCH_BANK_TRANSFER_INSTRUCTIONS="Synthetic runtime instructions; no funds or provider are involved." \
  stack test --fast --test-arguments=--match=artist-merch-runtime
