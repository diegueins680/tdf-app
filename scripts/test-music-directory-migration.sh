#!/bin/sh
set -eu

TDF_DIRECTORY_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_DIRECTORY_CONTAINER="tdf-directory-migration-test-$$"
TDF_DIRECTORY_DATABASE="tdf_directory_test"
TDF_DIRECTORY_API_PORT=$((18000 + ($$ % 1000)))
TDF_DIRECTORY_API_LOG="${TMPDIR:-/tmp}/tdf-directory-api-$$.log"
TDF_DIRECTORY_API_PID=""

stop_api() {
  if [ -z "$TDF_DIRECTORY_API_PID" ]; then
    return
  fi
  kill "$TDF_DIRECTORY_API_PID" >/dev/null 2>&1 || true
  attempt=0
  while kill -0 "$TDF_DIRECTORY_API_PID" >/dev/null 2>&1 && [ "$attempt" -lt 10 ]; do
    attempt=$((attempt + 1))
    sleep 1
  done
  if kill -0 "$TDF_DIRECTORY_API_PID" >/dev/null 2>&1; then
    kill -9 "$TDF_DIRECTORY_API_PID" >/dev/null 2>&1 || true
  fi
  wait "$TDF_DIRECTORY_API_PID" >/dev/null 2>&1 || true
  TDF_DIRECTORY_API_PID=""
}

cleanup() {
  stop_api
  docker rm -f "$TDF_DIRECTORY_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_DIRECTORY_CONTAINER" \
  -e POSTGRES_HOST_AUTH_METHOD=trust \
  -e POSTGRES_DB="$TDF_DIRECTORY_DATABASE" \
  -p 127.0.0.1::5432 \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_DIRECTORY_CONTAINER" pg_isready -U postgres -d "$TDF_DIRECTORY_DATABASE" >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 45 ]; then
    echo "Music directory migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

TDF_DIRECTORY_DB_PORT=$(docker port "$TDF_DIRECTORY_CONTAINER" 5432/tcp | sed 's/.*://')

(
  cd "$TDF_DIRECTORY_ROOT/tdf-hq"
  DB_HOST=127.0.0.1 \
  DB_PORT="$TDF_DIRECTORY_DB_PORT" \
  DB_USER=postgres \
  DB_PASS=unused-local-test-value \
  DB_NAME="$TDF_DIRECTORY_DATABASE" \
  APP_PORT="$TDF_DIRECTORY_API_PORT" \
  RUN_MIGRATIONS=true \
  RESET_DB=false \
  SEED_DB=false \
  EVENT_DISCOVERY_ENABLED=false \
  sh -c 'if [ -n "${TDF_DIRECTORY_SERVER_BIN:-}" ]; then exec "$TDF_DIRECTORY_SERVER_BIN"; else exec stack exec -- tdf-hq-exe; fi'
) >"$TDF_DIRECTORY_API_LOG" 2>&1 &
TDF_DIRECTORY_API_PID=$!

attempt=0
until curl -fsS "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/health" 2>/dev/null | grep -q '"db":"ok"'; do
  if ! kill -0 "$TDF_DIRECTORY_API_PID" >/dev/null 2>&1; then
    tail -80 "$TDF_DIRECTORY_API_LOG" >&2
    echo "Backend exited while preparing the authoritative base schema" >&2
    exit 1
  fi
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 180 ]; then
    tail -80 "$TDF_DIRECTORY_API_LOG" >&2
    echo "Backend did not finish base migrations" >&2
    exit 1
  fi
  sleep 1
done

stop_api

psql_exec() {
  docker exec -i "$TDF_DIRECTORY_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_DIRECTORY_DATABASE" "$@"
}
psql_file() {
  docker exec -i "$TDF_DIRECTORY_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_DIRECTORY_DATABASE" < "$1"
}

psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-14_music_directory_core.sql" >/dev/null
# A restart/retry before the migration ledger is committed must be harmless.
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-14_music_directory_core.sql" >/dev/null

psql_exec <<'SQL' >/dev/null
INSERT INTO party(display_name,is_org,created_at)
VALUES ('Synthetic directory migration fixture',FALSE,now());
INSERT INTO artist_profile(artist_party_id,slug,bio,city,country_code,created_at)
SELECT id,'synthetic-directory-fixture','Synthetic data used only by the isolated migration test.','Quito','EC',now()
FROM party WHERE display_name='Synthetic directory migration fixture';
SQL

psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-14_music_directory_backfill_dry_run.sql" >/dev/null
dry_published=$(psql_exec -Atc "SELECT count(*) FROM directory_profile WHERE slug='synthetic-directory-fixture';")
test "$dry_published" = "0"

psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-14_music_directory_backfill_apply.sql" >/dev/null
backfill_state=$(psql_exec -Atc "SELECT profile_status FROM directory_profile WHERE slug='synthetic-directory-fixture';")
test "$backfill_state" = "published"
manager_count=$(psql_exec -Atc "SELECT count(*) FROM directory_profile_manager manager JOIN directory_profile profile ON profile.id=manager.profile_id WHERE profile.slug='synthetic-directory-fixture';")
test "$manager_count" = "0"
legacy_link_count=$(psql_exec -Atc "SELECT count(*) FROM directory_legacy_link link JOIN directory_profile profile ON profile.id=link.profile_id WHERE profile.slug='synthetic-directory-fixture' AND link.source_url='/artistas/synthetic-directory-fixture';")
test "$legacy_link_count" = "1"

psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-14_music_directory_backfill_rollback.sql" >/dev/null
rolled_back=$(psql_exec -Atc "SELECT count(*) FROM directory_profile WHERE slug='synthetic-directory-fixture';")
legacy_preserved=$(psql_exec -Atc "SELECT count(*) FROM artist_profile WHERE slug='synthetic-directory-fixture';")
test "$rolled_back" = "0"
test "$legacy_preserved" = "1"
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-14_music_directory_backfill_apply.sql" >/dev/null

psql_exec <<'SQL' >/dev/null
DO $$
DECLARE
  fixture_party BIGINT;
  source_id UUID := 'd1000000-0000-4000-8000-000000000001';
  target_id UUID := 'd1000000-0000-4000-8000-000000000002';
  claim_id UUID := 'd1000000-0000-4000-8000-000000000003';
  interaction_id UUID := 'd1000000-0000-4000-8000-000000000004';
  merge_id UUID := 'd1000000-0000-4000-8000-000000000005';
  quito_id UUID;
  ecuador_id UUID;
  category_id UUID;
  location_id UUID := 'd1000000-0000-4000-8000-000000000008';
  classified_id UUID := 'd1000000-0000-4000-8000-000000000006';
  saved_id UUID := 'd1000000-0000-4000-8000-000000000007';
  result JSONB;
BEGIN
  SELECT id INTO fixture_party FROM party WHERE display_name='Synthetic directory migration fixture';
  SELECT city.id,city.country_id INTO quito_id,ecuador_id FROM city_reference city WHERE city.code='quito-ec-p';
  SELECT id INTO category_id FROM classified_category WHERE code='collaboration';
  INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,bio,profile_status,visibility,moderation_status,onsite,remote)
  VALUES
    (source_id,fixture_party,'person','Synthetic source profile','synthetic-source-profile','Isolated migration test source.','published','public','allowed',TRUE,FALSE),
    (target_id,fixture_party,'person','Synthetic canonical profile','synthetic-canonical-profile','Isolated migration test target.','published','public','allowed',TRUE,TRUE);
  INSERT INTO directory_profile_location(id,profile_id,country_id,city_id,public_latitude,public_longitude,precision,primary_location,onsite)
  VALUES (location_id,source_id,ecuador_id,quito_id,-0.180653,-78.467834,'city',TRUE,TRUE);
  INSERT INTO directory_private_location(profile_location_id,exact_address,private_latitude,private_longitude,access_reason,created_by)
  VALUES (location_id,'Never exposed fixture address',-0.181111,-78.468888,'Isolated migration privacy invariant test.',fixture_party);
  INSERT INTO classified(id,author_profile_id,category_id,title,slug,description,status,moderation_status,onsite,remote,expires_at)
  VALUES (classified_id,source_id,category_id,'Synthetic collaboration fixture','synthetic-collaboration-fixture','Synthetic classified used only to test reference-preserving merge behavior.','published','allowed',TRUE,FALSE,now()+interval '30 days');

  INSERT INTO directory_claim(id,profile_id,claimant_party_id,claim_type,status,evidence,submitted_at)
  VALUES (claim_id,source_id,fixture_party,'profile','submitted','[]',now());
  BEGIN
    INSERT INTO directory_profile_manager(profile_id,account_party_id,can_edit,source_claim_id)
    VALUES(source_id,fixture_party,TRUE,claim_id);
    RAISE EXCEPTION 'unapproved claim incorrectly granted profile management';
  EXCEPTION WHEN insufficient_privilege THEN NULL;
  END;
  UPDATE directory_claim SET status='approved',reviewer_party_id=fixture_party,reviewed_at=now() WHERE id=claim_id;
  INSERT INTO directory_profile_manager(profile_id,account_party_id,can_edit,source_claim_id)
  VALUES(source_id,fixture_party,TRUE,claim_id);

  INSERT INTO directory_interaction(id,interaction_kind,external_id,profile_a_id,profile_b_id,status)
  VALUES(interaction_id,'confirmed_collaboration','synthetic-unverified',source_id,target_id,'pending');
  BEGIN
    INSERT INTO directory_review(interaction_id,author_profile_id,subject_profile_id,rating,body)
    VALUES(interaction_id,source_id,target_id,5,'This synthetic review must be rejected by the database.');
    RAISE EXCEPTION 'unverified interaction incorrectly accepted a review';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
  UPDATE directory_interaction SET status='completed',verified_at=now() WHERE id=interaction_id;
  INSERT INTO directory_review(interaction_id,author_profile_id,subject_profile_id,rating,body)
  VALUES(interaction_id,source_id,target_id,5,'Synthetic verified interaction review for migration testing.');

  INSERT INTO directory_saved_search(id,account_party_id,name,canonical_query,query_hash,alerts_enabled,alert_frequency)
  VALUES(saved_id,fixture_party,'Synthetic Quito alerts','{"q":"synthetic","entityType":"profile"}',encode(digest('{"q":"synthetic","entityType":"profile"}','sha256'),'hex'),TRUE,'instant');
  PERFORM directory_refresh_profile_search(source_id);
  PERFORM directory_refresh_profile_search(source_id);
  IF (SELECT count(*) FROM directory_alert_delivery WHERE saved_search_id=saved_id)=0 THEN
    RAISE EXCEPTION 'saved search did not create an alert match';
  END IF;
  IF (SELECT count(*) FROM directory_alert_delivery WHERE saved_search_id=saved_id)>1 THEN
    RAISE EXCEPTION 'saved search emitted a duplicate alert';
  END IF;

  result := directory_execute_profile_merge(merge_id,source_id,target_id,fixture_party,'Confirmed duplicate in isolated migration test.');
  IF result->>'status'<>'executed' THEN RAISE EXCEPTION 'merge did not execute'; END IF;
  IF (SELECT profile_status FROM directory_profile WHERE id=source_id)<>'merged' THEN RAISE EXCEPTION 'merge did not archive source'; END IF;
  IF (SELECT canonical_profile_id FROM directory_profile WHERE id=source_id)<>target_id THEN RAISE EXCEPTION 'merge lost canonical target'; END IF;
  IF (SELECT id FROM directory_public_profile_resolution WHERE requested_slug='synthetic-source-profile')<>target_id THEN RAISE EXCEPTION 'source slug no longer resolves'; END IF;
  IF (SELECT author_profile_id FROM classified WHERE id=classified_id)<>source_id THEN RAISE EXCEPTION 'merge rewrote a historical reference'; END IF;
  IF (SELECT status FROM classified WHERE id=classified_id)<>'withdrawn' THEN RAISE EXCEPTION 'merge left dependent classified public'; END IF;
  IF (SELECT before_counts FROM directory_merge_operation WHERE id=merge_id)<>(SELECT after_counts FROM directory_merge_operation WHERE id=merge_id) THEN RAISE EXCEPTION 'merge reference counts changed'; END IF;
  IF directory_execute_profile_merge(merge_id,source_id,target_id,fixture_party,'Confirmed duplicate in isolated migration test.')->>'id'<>merge_id::text THEN RAISE EXCEPTION 'merge retry was not idempotent'; END IF;
END
$$;

DO $$
DECLARE public_json JSONB;
BEGIN
  SELECT to_jsonb(profile) INTO public_json FROM directory_public_profile_resolution profile WHERE requested_slug='synthetic-source-profile';
  IF public_json ?| ARRAY['exact_address','private_latitude','private_longitude','primary_email','primary_phone','tax_id','api_token'] THEN
    RAISE EXCEPTION 'public profile resolution exposes private fields';
  END IF;
END
$$;

INSERT INTO directory_search_document(entity_kind,entity_id,slug,title,summary,search_text,search_vector,source_status,visibility,moderation_status,source_updated_at,source_version,sponsored)
SELECT 'profile','perf-'||series,'synthetic-performer-'||series,'Synthetic performer '||series,
  'Representative performance-only fixture','synthetic performer bassist productor',
  to_tsvector('simple','synthetic performer bassist productor'),'published','public','allowed',now(),1,FALSE
FROM generate_series(1,10000) series
ON CONFLICT DO NOTHING;

DO $$
DECLARE started TIMESTAMPTZ; elapsed_ms NUMERIC;
BEGIN
  started := clock_timestamp();
  PERFORM entity_id FROM directory_public_search_document
    WHERE search_vector @@ plainto_tsquery('simple','bassist productor')
    ORDER BY source_updated_at DESC,entity_kind,entity_id LIMIT 50;
  elapsed_ms := extract(epoch FROM (clock_timestamp()-started))*1000;
  IF elapsed_ms>750 THEN RAISE EXCEPTION 'representative search exceeded 750ms: % ms',elapsed_ms; END IF;
END
$$;
SQL

private_columns=$(psql_exec -Atc "SELECT count(*) FROM information_schema.columns WHERE table_name IN ('directory_public_profile','directory_public_profile_resolution','directory_public_search_document','directory_public_event','directory_public_venue') AND column_name IN ('exact_address','private_latitude','private_longitude','primary_email','primary_phone','tax_id','api_token');")
test "$private_columns" = "0"
public_profession_security_refs=$(psql_exec -Atc "SELECT count(*) FROM information_schema.table_constraints WHERE table_name='directory_profile_profession' AND constraint_name ILIKE '%role%';")
test "$public_profession_security_refs" = "0"

# Exercise the anonymous taxonomy handler against the migrated database. This catches
# drift between the SQL implementation and the canonical OpenAPI/client projection.
(
  cd "$TDF_DIRECTORY_ROOT/tdf-hq"
  DB_HOST=127.0.0.1 \
  DB_PORT="$TDF_DIRECTORY_DB_PORT" \
  DB_USER=postgres \
  DB_PASS=unused-local-test-value \
  DB_NAME="$TDF_DIRECTORY_DATABASE" \
  APP_PORT="$TDF_DIRECTORY_API_PORT" \
  RUN_MIGRATIONS=false \
  RESET_DB=false \
  SEED_DB=false \
  EVENT_DISCOVERY_ENABLED=false \
  sh -c 'if [ -n "${TDF_DIRECTORY_SERVER_BIN:-}" ]; then exec "$TDF_DIRECTORY_SERVER_BIN"; else exec stack exec -- tdf-hq-exe; fi'
) >"$TDF_DIRECTORY_API_LOG" 2>&1 &
TDF_DIRECTORY_API_PID=$!

attempt=0
until curl -fsS "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/health" 2>/dev/null | grep -q '"db":"ok"'; do
  if ! kill -0 "$TDF_DIRECTORY_API_PID" >/dev/null 2>&1; then
    tail -80 "$TDF_DIRECTORY_API_LOG" >&2
    echo "Backend exited while validating the public directory taxonomy" >&2
    exit 1
  fi
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 60 ]; then
    tail -80 "$TDF_DIRECTORY_API_LOG" >&2
    echo "Backend did not become ready for the public directory taxonomy check" >&2
    exit 1
  fi
  sleep 1
done

curl -fsS "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/taxonomies?locale=es" |
  node -e '
    let raw = "";
    process.stdin.on("data", (chunk) => { raw += chunk; });
    process.stdin.on("end", () => {
      const value = JSON.parse(raw);
      const collections = ["professions", "instruments", "genres", "serviceOfferings", "classifiedCategories", "compensationTypes", "currencies", "cities"];
      for (const key of collections) {
        if (!Array.isArray(value[key])) throw new Error(`${key} is not a public taxonomy array`);
      }
      if (!value.currencies.some((item) => item.code === "USD")) throw new Error("USD currency taxonomy is missing");
      if (!value.classifiedCategories.some((item) => Array.isArray(item.requirements?.required))) throw new Error("classified requirements are missing");
    });
  '
stop_api

echo "Music directory migration passed restart, backfill, rollback/reapply, privacy, claim, review, alert, merge, search-volume, taxonomy, and invariant checks."
