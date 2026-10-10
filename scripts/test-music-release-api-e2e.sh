#!/bin/sh
set -eu

TDF_MUSIC_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_MUSIC_DATABASE=${TDF_MUSIC_API_E2E_DATABASE:-tdf_music_release_api_e2e}
TDF_MUSIC_PORT=${TDF_MUSIC_API_E2E_PORT:-18091}
TDF_MUSIC_BOOT_TIMEOUT=${TDF_MUSIC_API_E2E_BOOT_TIMEOUT_SECONDS:-600}
TDF_MUSIC_BACKEND_EXE=${TDF_MUSIC_API_E2E_BACKEND_EXE:-}
TDF_MUSIC_PASSWORD=${TDF_MUSIC_API_E2E_PASSWORD:-}
TDF_MUSIC_RUNTIME_DIR=$(mktemp -d /private/tmp/tdf-music-api-e2e.XXXXXX)
TDF_MUSIC_LOG="$TDF_MUSIC_RUNTIME_DIR/backend.log"
TDF_MUSIC_BACKEND_PID=""
TDF_MUSIC_DATABASE_CREATED=0

cleanup() {
  if [ -n "$TDF_MUSIC_BACKEND_PID" ]; then
    kill "$TDF_MUSIC_BACKEND_PID" >/dev/null 2>&1 || true
    wait "$TDF_MUSIC_BACKEND_PID" >/dev/null 2>&1 || true
  fi
  if [ "$TDF_MUSIC_DATABASE_CREATED" = "1" ]; then
    dropdb --if-exists "$TDF_MUSIC_DATABASE" >/dev/null 2>&1 || true
  fi
  case "$TDF_MUSIC_RUNTIME_DIR" in
    /private/tmp/tdf-music-api-e2e.*) rm -rf -- "$TDF_MUSIC_RUNTIME_DIR" ;;
  esac
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

# Alternative loopback port is used only by the disposable Docker PostgreSQL
# fixture. These values never permit connecting this migration harness remotely.
case "${PGHOST:-127.0.0.1}" in
  127.0.0.1|localhost) ;;
  *) echo 'Music API E2E requires a loopback database host' >&2; exit 2 ;;
esac
if [ -n "${PGHOSTADDR:-}${PGSERVICE:-}${PGSERVICEFILE:-}" ]; then
  echo 'Music API E2E refuses alternate libpq host/service routing' >&2
  exit 2
fi
case "${PGPORT:-5432}" in
  ''|*[!0-9]*) echo 'Music API E2E requires a numeric PostgreSQL port' >&2; exit 2 ;;
esac
export PGHOST=127.0.0.1

if [ -n "${TDF_MUSIC_API_E2E_S3_ENDPOINT:-}" ]; then
  node -e 'const u = process.env.TDF_MUSIC_API_E2E_S3_ENDPOINT; if (!/^https:\/\/127\.0\.0\.1:\d+$/.test(u)) throw new Error("Real S3 E2E requires a loopback HTTPS endpoint")'
  : "${MUSIC_S3_ACCESS_KEY_ID:?Local S3 credentials required}"
  : "${MUSIC_S3_SECRET_ACCESS_KEY:?Local S3 credentials required}"
  : "${TDF_MUSIC_API_E2E_S3_CA:?Local S3 certificate required}"
  TDF_MUSIC_TEST_S3_ACCESS=$MUSIC_S3_ACCESS_KEY_ID
  TDF_MUSIC_TEST_S3_SECRET=$MUSIC_S3_SECRET_ACCESS_KEY
else
  # The fast fixture mode must never inherit a real provider credential.
  TDF_MUSIC_TEST_S3_ACCESS=synthetic-music-access-key
  TDF_MUSIC_TEST_S3_SECRET=synthetic-music-secret-key-for-local-tests
fi

if [ ! -x "$TDF_MUSIC_BACKEND_EXE" ]; then
  echo "TDF_MUSIC_API_E2E_BACKEND_EXE must identify the compiled backend executable" >&2
  exit 1
fi
if [ ! -x "$(dirname -- "$TDF_MUSIC_BACKEND_EXE")/tdf-ddex-render" ]; then
  echo "Build tdf-hq:exe:tdf-ddex-render alongside the backend before running music API E2E" >&2
  exit 1
fi
if [ "${#TDF_MUSIC_PASSWORD}" -lt 16 ]; then
  echo "TDF_MUSIC_API_E2E_PASSWORD must be a runtime-only value of at least 16 characters" >&2
  exit 1
fi
case "$TDF_MUSIC_PORT" in
  ''|*[!0-9]*) echo "TDF_MUSIC_API_E2E_PORT must be numeric" >&2; exit 1 ;;
esac
case "$TDF_MUSIC_BOOT_TIMEOUT" in
  ''|*[!0-9]*) echo "TDF_MUSIC_API_E2E_BOOT_TIMEOUT_SECONDS must be numeric" >&2; exit 1 ;;
esac
case "$TDF_MUSIC_DATABASE" in
  ''|*[!a-z0-9_]*) echo "TDF_MUSIC_API_E2E_DATABASE must contain only lowercase letters, digits, and underscores" >&2; exit 1 ;;
esac
if curl -fsS "http://127.0.0.1:$TDF_MUSIC_PORT/health" >/dev/null 2>&1; then
  echo "Refusing to reuse an occupied E2E port: $TDF_MUSIC_PORT" >&2
  exit 1
fi
if psql -d postgres -Atqc "SELECT 1 FROM pg_database WHERE datname = '$TDF_MUSIC_DATABASE'" | grep -q 1; then
  echo "Refusing to replace existing database: $TDF_MUSIC_DATABASE" >&2
  exit 1
fi

createdb "$TDF_MUSIC_DATABASE"
TDF_MUSIC_DATABASE_CREATED=1

start_backend() {
  APP_ENV=test \
  DB_HOST=127.0.0.1 \
  DB_PORT="${PGPORT:-5432}" \
  DB_USER="${PGUSER:-$(id -un)}" \
  DB_PASS="${PGPASSWORD:-unused-local-test-value}" \
  DB_NAME="$TDF_MUSIC_DATABASE" \
  APP_PORT="$TDF_MUSIC_PORT" \
  RESET_DB=false \
  RUN_MIGRATIONS="$1" \
  SEED_DB="$2" \
  TDF_ENABLE_SYNTHETIC_PERSONAS=1 \
  TDF_SYNTHETIC_PERSONA_FILE="$TDF_MUSIC_ROOT/test/personas/personas.json" \
  TDF_PERSONA_TEST_PASSWORD="$TDF_MUSIC_PASSWORD" \
  HQ_ASSETS_DIR="$TDF_MUSIC_ROOT/tdf-hq/assets" \
  EVENT_DISCOVERY_ENABLED=false \
  ARTIST_ENRICHMENT_ENABLED=false \
  EVENT_LOGISTICS_RECHECK_ENABLED=false \
  COMMERCE_CHECKOUT_ENV=sandbox \
  MUSIC_TRUST_CF_IPCOUNTRY=true \
  ALLOWED_ORIGINS=http://127.0.0.1:4187 \
  ALLOW_ALL_ORIGINS=false \
  CORS_DISABLE_DEFAULTS=true \
  MUSIC_S3_ENDPOINT="${TDF_MUSIC_API_E2E_S3_ENDPOINT:-https://127.0.0.1:19000}" \
  MUSIC_S3_REGION=us-east-1 \
  MUSIC_S3_ACCESS_KEY_ID="$TDF_MUSIC_TEST_S3_ACCESS" \
  MUSIC_S3_SECRET_ACCESS_KEY="$TDF_MUSIC_TEST_S3_SECRET" \
  MUSIC_S3_QUARANTINE_BUCKET=music-e2e-quarantine \
  MUSIC_S3_MASTER_BUCKET=music-e2e-master \
  MUSIC_S3_DERIVATIVE_BUCKET=music-e2e-derivative \
  MUSIC_S3_DDEX_BUCKET=music-e2e-ddex \
  "$TDF_MUSIC_BACKEND_EXE" +RTS -N2 -RTS >"$TDF_MUSIC_LOG" 2>&1 &
  TDF_MUSIC_BACKEND_PID=$!

  attempt=0
  until curl -fsS "http://127.0.0.1:$TDF_MUSIC_PORT/health" 2>/dev/null | grep -q '"status":"ok"'; do
    if ! kill -0 "$TDF_MUSIC_BACKEND_PID" >/dev/null 2>&1; then
      echo "Backend stopped before becoming healthy" >&2
      tail -100 "$TDF_MUSIC_LOG" >&2
      exit 1
    fi
    attempt=$((attempt + 1))
    if [ "$attempt" -ge "$TDF_MUSIC_BOOT_TIMEOUT" ]; then
      echo "Backend did not become healthy within $TDF_MUSIC_BOOT_TIMEOUT seconds" >&2
      tail -100 "$TDF_MUSIC_LOG" >&2
      exit 1
    fi
    sleep 1
  done

  # Health becomes available before the synthetic seed pass ends. Do not stop
  # that first boot until the canonical roles and personas are visible.
  if [ "$2" = "true" ]; then
    attempt=0
    until psql -X -d "$TDF_MUSIC_DATABASE" -Atqc "
      SELECT
        to_regclass('public.security_role') IS NOT NULL
        AND to_regclass('public.party_security_role') IS NOT NULL
        AND EXISTS (
          SELECT 1 FROM party
          WHERE lower(primary_email)='per-16.irene@persona.test'
        );
    " 2>/dev/null | grep -q '^t$'
    do
      if ! kill -0 "$TDF_MUSIC_BACKEND_PID" >/dev/null 2>&1; then
        echo "Backend stopped before completing migrations and synthetic seed" >&2
        tail -100 "$TDF_MUSIC_LOG" >&2
        exit 1
      fi
      attempt=$((attempt + 1))
      if [ "$attempt" -ge "$TDF_MUSIC_BOOT_TIMEOUT" ]; then
        echo "Backend did not complete migrations and synthetic seed within $TDF_MUSIC_BOOT_TIMEOUT seconds" >&2
        tail -100 "$TDF_MUSIC_LOG" >&2
        exit 1
      fi
      sleep 1
    done
  fi
}

stop_backend() {
  kill "$TDF_MUSIC_BACKEND_PID" >/dev/null 2>&1 || true
  wait "$TDF_MUSIC_BACKEND_PID" >/dev/null 2>&1 || true
  TDF_MUSIC_BACKEND_PID=""
}

psql -X -q -v ON_ERROR_STOP=1 -d "$TDF_MUSIC_DATABASE" \
  < "$TDF_MUSIC_ROOT/tdf-hq/sql/init_schema.sql"

for migration in \
  2026-07-12_notification_table.sql \
  2026-08-05_artist_enrichment.sql \
  2026-08-13_unified_checkout_core.sql \
  2026-09-04_access_request_notification_types.sql \
  2026-09-11_music_release_platform.sql \
  2026-09-15_music_preview_ranges.sql \
  2026-09-15_music_version_parties.sql \
  2026-09-15_music_party_details.sql \
  2026-09-16_music_correction_asset_graph.sql \
  2026-09-16_music_correction_concurrency.sql \
  2026-09-16_music_ddex_operations.sql \
  2026-09-16_music_playback_identity.sql
do
  psql -X -q -v ON_ERROR_STOP=1 -d "$TDF_MUSIC_DATABASE" \
    < "$TDF_MUSIC_ROOT/tdf-hq/sql/$migration"
done

# Persistent owns the remaining runtime tables and canonical role tables; the
# normal test seed creates synthetic users after those tables exist.
start_backend true true
stop_backend

psql -X -q -v ON_ERROR_STOP=1 -d "$TDF_MUSIC_DATABASE" <<'SQL'
WITH source_credential AS (
  SELECT credential.password_hash
  FROM user_credential credential
  JOIN party source ON source.id=credential.party_id
  WHERE lower(source.primary_email)='per-11.martina@persona.test'
    AND credential.active
  LIMIT 1
), artist AS (
  INSERT INTO party(display_name,is_org,primary_email,created_at)
  VALUES ('Artista Música E2E',FALSE,'music.artist@persona.test',NOW())
  RETURNING id
), artist_credential AS (
  INSERT INTO user_credential(party_id,username,password_hash,active)
  SELECT artist.id,'music.artist@persona.test',source_credential.password_hash,TRUE
  FROM artist CROSS JOIN source_credential
  RETURNING party_id
), profile AS (
  INSERT INTO artist_profile(artist_party_id,slug,created_at)
  SELECT party_id,'artista-musica-e2e',NOW() FROM artist_credential
  RETURNING artist_party_id
), verified AS (
  INSERT INTO artist_profile_enrichment(
    artist_party_id,last_verified_at,review_status,created_at,updated_at
  )
  SELECT artist_party_id,NOW(),'verified',NOW(),NOW() FROM profile
  RETURNING artist_party_id
), member AS (
  SELECT id
  FROM party
  WHERE lower(primary_email)='per-12.karla@persona.test'
  LIMIT 1
)
INSERT INTO artist_release_team_member(
  artist_party_id,member_party_id,role_code,permissions,granted_by
)
SELECT
  verified.artist_party_id,
  member.id,
  'admin',
  ARRAY[
    'release.read','release.create','release.edit','release.upload','release.submit',
    'release.schedule','release.analytics','release.downloads','release.team.manage'
  ]::TEXT[],
  verified.artist_party_id
FROM verified CROSS JOIN member;

UPDATE revenue_feature_flag
SET enabled=TRUE,reason='Disposable music API E2E'
WHERE environment='sandbox'
  AND flag_key IN (
    'music_releases.authoring','music_releases.processing',
    'music_releases.public','music_releases.commerce','music_releases.ddex_export'
  );
SQL

start_backend false false

if ! TDF_MUSIC_API_E2E_BASE="http://127.0.0.1:$TDF_MUSIC_PORT" \
  TDF_MUSIC_API_E2E_DATABASE="$TDF_MUSIC_DATABASE" \
  TDF_MUSIC_API_E2E_PASSWORD="$TDF_MUSIC_PASSWORD" \
  node "$TDF_MUSIC_ROOT/scripts/test-music-release-api-e2e.mjs"
then
  echo "Music release API E2E failed; backend log tail follows:" >&2
  tail -150 "$TDF_MUSIC_LOG" >&2
  exit 1
fi

echo "Music release API E2E passed with real HTTP handlers, disposable PostgreSQL, synthetic identities, multipart state, editorial review, territorial publication, player library, purchase entitlement, correction and takedown."
