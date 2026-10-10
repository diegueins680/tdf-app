#!/usr/bin/env bash
# Verifies canonical directory preview images and artist-profile derived
# listings on the production schema: forward migration (twice), behavior
# assertions, real two-session concurrency, backfill (twice), rollback and
# re-apply. Requires an isolated, empty PostgreSQL database.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
database_url="${TDF_ARTIST_LISTINGS_TEST_DATABASE_URL:?Set TDF_ARTIST_LISTINGS_TEST_DATABASE_URL to an isolated empty PostgreSQL database}"
work_dir="$(mktemp -d "${TMPDIR:-/tmp}/tdf-artist-listings.XXXXXX")"
trap 'rm -rf "${work_dir}"' EXIT INT TERM

sql_dir="${repo_root}/tdf-hq/sql"
test_dir="${repo_root}/tdf-hq/test/integration"
migration="${sql_dir}/2026-10-07_directory_artist_derived_listings.sql"
rollback="${sql_dir}/2026-10-07_directory_artist_derived_listings_rollback.sql"
backfill="${sql_dir}/2026-10-07_directory_artist_listing_backfill_apply.sql"
dry_run="${sql_dir}/2026-10-07_directory_artist_listing_backfill_dry_run.sql"

run_sql() {
  psql "${database_url}" -X -q -v ON_ERROR_STOP=1 "$@" >/dev/null
}
query() {
  psql "${database_url}" -X -qAt -v ON_ERROR_STOP=1 -c "$1"
}

existing_tables="$(query "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p');")"
if [ "${existing_tables}" != "0" ]; then
  echo "Artist listing test requires an empty isolated database; found ${existing_tables} tables" >&2
  exit 1
fi

# Production-shaped base: the 2026-08-14 production schema plus every
# registered production migration that precedes this change.
run_sql -f "${repo_root}/scripts/__tests__/fixtures/production-schema-20260814.sql"
run_sql -f "${repo_root}/scripts/__tests__/fixtures/catalog-production-source-fixture.sql"
SOURCE_COMMIT="0000000000000000000000000000000000000000" \
  node "${repo_root}/scripts/render-production-migration-batch.mjs" > "${work_dir}/batch.sql"
run_sql -f "${work_dir}/batch.sql"

# The manifest already includes this change; re-applying proves idempotence.
run_sql -f "${migration}"
run_sql -f "${migration}"
run_sql -f "${test_dir}/directory_artist_derived_listings_postgres.sql"

# Two sessions race to create the same profile's listing.
race_profile="$(query "ALTER TABLE directory_profile DISABLE TRIGGER directory_profile_artist_listing_sync_trigger;
  INSERT INTO directory_profile (subject_party_id, profile_kind, public_name, slug, bio, profile_status, visibility,
    moderation_status, completeness_score, published_at)
  VALUES (910001,'artist','Race Artist','race-artist','Artista para la prueba de concurrencia real.','published',
    'public','allowed',.8,now()) RETURNING id;
  ALTER TABLE directory_profile ENABLE TRIGGER directory_profile_artist_listing_sync_trigger;" | grep -E '^[0-9a-f-]{36}$')"
psql "${database_url}" -X -q -v ON_ERROR_STOP=1 \
  -c "BEGIN; SELECT directory_sync_profile_listing('${race_profile}'); SELECT pg_sleep(2); COMMIT;" \
  >/dev/null &
first_session=$!
sleep 0.5
second_listing="$(query "SELECT directory_sync_profile_listing('${race_profile}');")"
wait "${first_session}"
test "$(query "SELECT count(*) FROM classified WHERE source_profile_id='${race_profile}';")" = "1"
test "$(query "SELECT id FROM classified WHERE source_profile_id='${race_profile}';")" = "${second_listing}"

run_sql -f "${dry_run}"
run_sql -f "${backfill}"
run_sql -f "${backfill}"
run_sql -f "${test_dir}/directory_artist_listing_backfill_assertions_postgres.sql"

# Rollback is non-destructive and repeatable; forward re-apply resumes listings.
published_before="$(query "SELECT count(*) FROM classified WHERE source_profile_id IS NOT NULL AND status='published';")"
run_sql -f "${rollback}"
run_sql -f "${rollback}"
test "$(query "SELECT count(*) FROM classified WHERE source_profile_id IS NOT NULL AND status='published';")" = "0"
run_sql -f "${migration}"
run_sql -f "${backfill}"
test "$(query "SELECT count(*) FROM classified WHERE source_profile_id IS NOT NULL AND status='published';")" = "${published_before}"

# Optional HTTP flow through the real API when a backend binary is supplied.
server_bin="${TDF_ARTIST_LISTINGS_SERVER_BIN:-}"
if [ -n "${server_bin}" ]; then
  server_port="${TDF_ARTIST_LISTINGS_SERVER_PORT:-18979}"
  api="http://127.0.0.1:${server_port}"
  query "INSERT INTO party(id,display_name,is_org,created_at) VALUES (930001,'HTTP Artist Owner',false,now());
    INSERT INTO api_token(token,party_id,label,active) VALUES ('synthetic-artist-listing-token',930001,'artist listing http test',true);
    INSERT INTO directory_age_assurance(account_party_id,assurance_status,verified_at,updated_at) VALUES (930001,'adult_attested',now(),now());" >/dev/null
  DATABASE_URL="${database_url}" APP_PORT="${server_port}" RUN_MIGRATIONS=false AUTO_APPLY_PRODUCTION_MIGRATIONS=false \
    RESET_DB=false SEED_DB=false DEFAULT_LOCALE=es EVENT_DISCOVERY_ENABLED=false \
    "${server_bin}" > "${work_dir}/api.log" 2>&1 &
  server_pid=$!
  trap 'kill "${server_pid}" >/dev/null 2>&1 || true; rm -rf "${work_dir}"' EXIT INT TERM
  for _ in $(seq 1 120); do
    curl -fsS "${api}/health" 2>/dev/null | grep -q '"db":"ok"' && break
    kill -0 "${server_pid}" 2>/dev/null || { tail -40 "${work_dir}/api.log" >&2; exit 1; }
    sleep 1
  done
  auth="Authorization: Bearer synthetic-artist-listing-token"
  json() { curl -fsS -H "${auth}" -H 'Content-Type: application/json' "$@"; }
  field() { node -e 'let s="";process.stdin.on("data",c=>s+=c).on("end",()=>{const v=process.argv[1].split(".").reduce((o,k)=>o==null?o:o[k],JSON.parse(s));console.log(typeof v==="object"?JSON.stringify(v):String(v))})' "$1"; }
  body='{"profileKind":"band","publicName":"HTTP Test Band","slug":"http-test-band","bio":"Banda creada mediante la API para verificar el anuncio derivado automático.","professionIds":["21000000-0000-4000-8000-000000000001"],"instrumentIds":[],"genreIds":["2109e4ab-c2b5-493a-aa5e-97c00dd6fa9c"],"serviceOfferingIds":[],"countryId":"1cb3600a-c7e3-4f5f-8e67-55db001de6d5","cityId":"24000000-0000-4000-8000-000000000002","onsite":true,"remote":false,"availableToTravel":false,"coverImageUrl":"https://cdn.example.test/http-band.jpg"}'
  profile_id="$(json -X POST "${api}/directory/profiles" -H 'Idempotency-Key: http-band-create-0001' -d "${body}" | field id)"
  test "$(json -X POST "${api}/directory/profiles" -H 'Idempotency-Key: http-band-create-0001' -d "${body}" | field id)" = "${profile_id}"
  test "$(json -X PATCH "${api}/directory/profiles/${profile_id}/status" -d '{"status":"published"}' | field derivedListing.status)" = "published"
  listing_id="$(json "${api}/directory/profiles" | node -e 'let s="";process.stdin.on("data",c=>s+=c).on("end",()=>console.log(JSON.parse(s).find(p=>p.slug==="http-test-band").derivedListing.id))')"
  test "$(curl -fsS "${api}/directory/classifieds/http-test-band-perfil" | field imageUrl)" = "https://cdn.example.test/http-band.jpg"
  test "$(curl -fsS "${api}/directory/classifieds/http-test-band-perfil" | field sourceProfile.canonicalUrl)" = "/directorio/http-test-band"
  test "$(curl -fsS "${api}/directory/search?q=http%20test%20band" | node -e 'let s="";process.stdin.on("data",c=>s+=c).on("end",()=>console.log(JSON.parse(s).items.filter(i=>i.type==="classified").length))')" = "0"
  json -X PUT "${api}/directory/profiles/${profile_id}" -d "${body/HTTP Test Band\"/HTTP Test Band Renamed\"}" >/dev/null
  test "$(curl -fsS "${api}/directory/classifieds/http-test-band-perfil" | field title)" = "HTTP Test Band Renamed"
  json -X PATCH "${api}/directory/profiles/${profile_id}/status" -d '{"status":"paused"}' >/dev/null
  test "$(curl -s -o /dev/null -w '%{http_code}' "${api}/directory/classifieds/http-test-band-perfil")" = "404"
  json -X PATCH "${api}/directory/profiles/${profile_id}/status" -d '{"status":"published"}' >/dev/null
  test "$(json "${api}/directory/profiles" | node -e 'let s="";process.stdin.on("data",c=>s+=c).on("end",()=>console.log(JSON.parse(s).find(p=>p.slug==="http-test-band").derivedListing.id))')" = "${listing_id}"
  test "$(curl -s -o /dev/null -w '%{http_code}' -H "${auth}" -H 'Content-Type: application/json' -X PATCH "${api}/directory/classifieds/${listing_id}/status" -d '{"status":"paused"}')" = "409"
  test "$(query "SELECT count(*) FROM classified WHERE source_profile_id='${profile_id}';")" = "1"
  # Autocomplete suggests the artist once (the profile), never its derived listing.
  test "$(curl -fsS "${api}/directory/suggestions?q=http%20test%20band" | node -e 'let s="";process.stdin.on("data",c=>s+=c).on("end",()=>{const i=JSON.parse(s);console.log(i.filter(x=>x.suggestionKind==="profile").length+"/"+i.filter(x=>x.suggestionKind==="classified").length)})')" = "1/0"
  # A cross-origin http cover is refused by the API, not stored and hidden later.
  test "$(curl -s -o /dev/null -w '%{http_code}' -H "${auth}" -H 'Content-Type: application/json' -X PUT "${api}/directory/profiles/${profile_id}" -d "${body/https:\/\/cdn.example.test\/http-band.jpg/http://cdn.example.test/http-band.jpg}")" = "400"
  kill "${server_pid}" >/dev/null 2>&1 || true
fi

echo "Directory artist listings: migration, behavior, concurrency, backfill and rollback verified"
