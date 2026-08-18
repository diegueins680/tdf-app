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
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-16_music_directory_verified_reviews.sql" >/dev/null
# Exercise the actual incremental deploy retry and operational rollback path.
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-16_music_directory_verified_reviews.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-16_music_directory_verified_reviews_rollback.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-16_music_directory_verified_reviews.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-16_music_directory_verified_reviews.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_images.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_images.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_images_rollback.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_images.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_images.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_image_host_compatibility.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_image_host_compatibility.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_image_host_compatibility_rollback.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_image_host_compatibility.sql" >/dev/null
psql_file "$TDF_DIRECTORY_ROOT/tdf-hq/sql/2026-08-18_music_directory_profile_image_host_compatibility.sql" >/dev/null

unicode_image_url=$(psql_exec -Atc "SELECT directory_profile_primary_image_url('[{\"kind\":\"image\",\"url\":\"https://música.example/profile.webp\"}]'::jsonb);")
ipv6_image_url=$(psql_exec -Atc "SELECT directory_profile_primary_image_url('[{\"kind\":\"image\",\"url\":\"https://[::1]/profile.webp\"}]'::jsonb);")
malformed_authority_fallback=$(psql_exec -Atc "SELECT directory_profile_primary_image_url('[{\"kind\":\"image\",\"thumbnailUrl\":\"http://%\",\"url\":\"https://images.example.test/fallback.webp\"}]'::jsonb);")
malformed_bracket_fallback=$(psql_exec -Atc "SELECT directory_profile_primary_image_url('[{\"kind\":\"image\",\"thumbnailUrl\":\"http://[\",\"url\":\"/assets/serve/directory/fallback.webp\"}]'::jsonb);")
test "$unicode_image_url" = "https://música.example/profile.webp"
test "$ipv6_image_url" = "https://[::1]/profile.webp"
test "$malformed_authority_fallback" = "https://images.example.test/fallback.webp"
test "$malformed_bracket_fallback" = "/assets/serve/directory/fallback.webp"

psql_exec <<'SQL' >/dev/null
INSERT INTO party(display_name,is_org,created_at)
VALUES ('Synthetic directory migration fixture',FALSE,now());
INSERT INTO artist_profile(artist_party_id,slug,bio,city,country_code,hero_image_url,website_url,created_at)
SELECT id,'synthetic-directory-fixture','Synthetic data used only by the isolated migration test.','Quito','EC','/media/synthetic-directory-fixture.webp','https://example.test/synthetic-directory-fixture',now()
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
profile_image_url=$(psql_exec -Atc "SELECT image_url FROM directory_search_document WHERE entity_kind='profile' AND slug='synthetic-directory-fixture';")
test "$profile_image_url" = "/media/synthetic-directory-fixture.webp"

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

# Runtime fixtures use obviously synthetic tokens and profiles in this isolated database.
# They exercise participant binding without relying on production credentials or people.
psql_exec <<'SQL' >/dev/null
INSERT INTO party(display_name,is_org,created_at) VALUES
  ('Synthetic invitation sender',FALSE,now()),
  ('Synthetic invitation target',FALSE,now()),
  ('Synthetic invitation unrelated',FALSE,now());
INSERT INTO api_token(token,party_id,label,active)
SELECT 'synthetic-directory-sender-token',id,'synthetic-directory-runtime',TRUE FROM party WHERE display_name='Synthetic invitation sender'
UNION ALL
SELECT 'synthetic-directory-target-token',id,'synthetic-directory-runtime',TRUE FROM party WHERE display_name='Synthetic invitation target';
INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,bio,profile_status,visibility,moderation_status,completeness_score,public_contact_enabled,onsite,remote,published_at)
SELECT 'd2000000-0000-4000-8000-000000000001'::uuid,id,'person','Synthetic invitation sender','synthetic-invitation-sender','Isolated runtime authorization fixture.','published','public','allowed',.9,TRUE,FALSE,TRUE,now() FROM party WHERE display_name='Synthetic invitation sender'
UNION ALL
SELECT 'd2000000-0000-4000-8000-000000000002'::uuid,id,'person','Synthetic invitation target','synthetic-invitation-target','Isolated runtime authorization fixture.','published','public','allowed',.9,TRUE,FALSE,TRUE,now() FROM party WHERE display_name='Synthetic invitation target'
UNION ALL
SELECT 'd2000000-0000-4000-8000-000000000003'::uuid,id,'person','Synthetic unrelated profile','synthetic-invitation-unrelated','Isolated runtime authorization fixture.','published','public','allowed',.9,TRUE,FALSE,TRUE,now() FROM party WHERE display_name='Synthetic invitation unrelated';
INSERT INTO directory_profile_manager(profile_id,account_party_id,can_edit,can_publish,can_contact,can_manage,active)
SELECT 'd2000000-0000-4000-8000-000000000001'::uuid,id,TRUE,TRUE,TRUE,TRUE,TRUE FROM party WHERE display_name='Synthetic invitation sender'
UNION ALL
SELECT 'd2000000-0000-4000-8000-000000000002'::uuid,id,TRUE,TRUE,TRUE,TRUE,TRUE FROM party WHERE display_name='Synthetic invitation target';
INSERT INTO directory_age_assurance(account_party_id,assurance_status,verified_at)
SELECT id,'adult_attested',now() FROM party WHERE display_name IN ('Synthetic invitation sender','Synthetic invitation target');
INSERT INTO classified(id,author_profile_id,category_id,title,slug,description,status,moderation_status,onsite,remote,expires_at,published_at)
SELECT 'd2000000-0000-4000-8000-000000000004'::uuid,'d2000000-0000-4000-8000-000000000001'::uuid,id,'Synthetic invitation opportunity','synthetic-invitation-opportunity','Isolated classified used to bind an invitation to its authorized sender.','published','allowed',FALSE,TRUE,now()+interval '30 days',now()
FROM classified_category WHERE code='collaboration';
INSERT INTO directory_invitation(id,sender_profile_id,target_profile_id,classified_id,message,status,idempotency_key,request_fingerprint,created_at,expires_at)
VALUES ('d2000000-0000-4000-8000-000000000005','d2000000-0000-4000-8000-000000000001','d2000000-0000-4000-8000-000000000002',NULL,'Synthetic expired invitation for transition enforcement.','pending','synthetic-expired-invitation','synthetic-expired-fingerprint',now()-interval '31 days',now()-interval '1 day');
INSERT INTO directory_interaction(id,interaction_kind,external_id,profile_a_id,profile_b_id,status,verified_at)
VALUES ('d2000000-0000-4000-8000-000000000006','confirmed_collaboration','synthetic-runtime-review-source','d2000000-0000-4000-8000-000000000001','d2000000-0000-4000-8000-000000000002','completed',now());
SQL

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
      const collections = ["professions", "instruments", "genres", "serviceOfferings", "classifiedCategories", "compensationTypes", "currencies", "languages", "cities"];
      for (const key of collections) {
        if (!Array.isArray(value[key])) throw new Error(`${key} is not a public taxonomy array`);
      }
      if (!value.currencies.some((item) => item.code === "USD")) throw new Error("USD currency taxonomy is missing");
      if (!value.classifiedCategories.some((item) => Array.isArray(item.requirements?.required))) throw new Error("classified requirements are missing");
    });
  '

# Legacy portfolio/link keys are projected through the closed modern DTO without
# mutating the stored historical source/provenance fields.
curl -fsS "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/synthetic-directory-fixture" |
  node -e '
    const value = JSON.parse(require("fs").readFileSync(0, "utf8"));
    const media = value.portfolio?.[0];
    const link = value.links?.[0];
    if (media?.itemType !== "image" || media?.title !== "Image" || media?.url !== "/media/synthetic-directory-fixture.webp") throw new Error("legacy portfolio was not normalized into the closed DTO");
    if (link?.label !== "Website" || link?.url !== "https://example.test/synthetic-directory-fixture") throw new Error("legacy link was not normalized into the closed DTO");
    if ("kind" in media || "source" in media || "kind" in link) throw new Error("legacy internal keys escaped the public DTO");
  '

# Create a second profile through the authenticated API. It deliberately covers
# structured roles, catalog languages, two service areas, safe media and rates.
# All values are synthetic and resolved from the isolated database.
rich_profession_one='21000000-0000-4000-8000-000000000005'
rich_profession_two='21000000-0000-4000-8000-000000000006'
rich_instrument_id=$(psql_exec -Atc "SELECT id FROM instrument WHERE active ORDER BY sort_order,id LIMIT 1;")
rich_genre_id=$(psql_exec -Atc "SELECT id FROM genre WHERE active ORDER BY sort_order,id LIMIT 1;")
rich_service_id=$(psql_exec -Atc "SELECT id FROM service_offering WHERE active ORDER BY sort_order,id LIMIT 1;")
rich_country_id=$(psql_exec -Atc "SELECT id FROM country_reference WHERE alpha2='EC';")
rich_other_country_id=$(psql_exec -Atc "SELECT id FROM country_reference WHERE alpha2<>'EC' AND active ORDER BY alpha2 LIMIT 1;")
rich_city_id=$(psql_exec -Atc "SELECT id FROM city_reference WHERE code='quito-ec-p';")
rich_currency_id=$(psql_exec -Atc "SELECT id FROM currency_reference WHERE code='USD';")
rich_language_id=$(psql_exec -Atc "SELECT id FROM language_reference WHERE iso6391='es';")

rich_profile_payload=$(node -e '
  const [currencyId, professionOne, professionTwo, instrumentId, genreId, serviceId, languageId, countryId, cityId] = process.argv.slice(1);
  process.stdout.write(JSON.stringify({
    profileKind: "person",
    publicName: "Synthetic rich producer",
    slug: "synthetic-rich-producer",
    bio: "Synthetic professional profile used only by the isolated directory API test.",
    experienceSummary: "Ten synthetic years producing and recording test fixtures.",
    creditsSummary: "Synthetic Album One; Synthetic Session Two.",
    portfolio: [{ itemType: "credit", title: " Synthetic production credit ", url: " https://example.test/credits/synthetic " }],
    links: [{ label: " Synthetic portfolio ", url: " https://example.test/synthetic-rich-producer " }],
    equipmentSummary: "Synthetic microphones and monitoring equipment.",
    rateMinMinor: 10000,
    rateMaxMinor: 25000,
    currencyId,
    availabilityStatus: "available",
    professionIds: [professionOne, professionTwo],
    professionDetails: [
      { professionId: professionOne, headline: "Synthetic music producer", yearsExperience: 10, rateMinMinor: 12000, rateMaxMinor: 24000, currencyId },
      { professionId: professionTwo, headline: "Synthetic recording engineer", yearsExperience: 8 },
    ],
    instrumentIds: [instrumentId],
    instrumentDetails: [{ instrumentId, proficiency: "professional" }],
    genreIds: [genreId],
    serviceOfferingIds: [serviceId],
    languages: [{ languageId, proficiency: "native" }],
    serviceAreas: [
      { countryId, cityId, serviceRadiusKm: 35, primaryLocation: true, onsite: true },
      { countryId, serviceRadiusKm: 500, primaryLocation: false, onsite: true },
    ],
    countryId,
    cityId,
    onsite: true,
    remote: true,
    availableToTravel: true,
    travelRadiusKm: 500,
  }));
' "$rich_currency_id" "$rich_profession_one" "$rich_profession_two" "$rich_instrument_id" "$rich_genre_id" "$rich_service_id" "$rich_language_id" "$rich_country_id" "$rich_city_id")

rich_profile_id=$(curl -fsS -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-rich-profile-create-1' \
  -H 'Content-Type: application/json' \
  --data "$rich_profile_payload" |
  node -e '
    let raw = "";
    process.stdin.on("data", (chunk) => { raw += chunk; });
    process.stdin.on("end", () => {
      const value = JSON.parse(raw);
      if (!/^[0-9a-f-]{36}$/.test(value.id ?? "")) throw new Error("rich profile id is missing");
      if (value.experienceSummary !== "Ten synthetic years producing and recording test fixtures.") throw new Error("rich experience was not projected");
      if (value.professionDetails?.length !== 2 || value.instrumentDetails?.[0]?.proficiency !== "professional") throw new Error("structured professional details were not projected");
      if (value.languages?.[0]?.proficiency !== "native" || value.serviceAreas?.length !== 2) throw new Error("languages or multiple service areas were not projected");
      if (value.portfolio?.[0]?.itemType !== "credit" || value.portfolio?.[0]?.title !== "Synthetic production credit" || value.portfolio?.[0]?.url !== "https://example.test/credits/synthetic" || value.links?.[0]?.label !== "Synthetic portfolio" || value.rates?.minMinor !== 10000) throw new Error("normalized portfolio, links or rates were not projected");
      if (value.capabilities?.edit !== true || value.capabilities?.publish !== true) throw new Error("explicit manager capabilities are missing");
      process.stdout.write(value.id);
    });
  ')

rich_retry_id=$(curl -fsS -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-rich-profile-create-1' \
  -H 'Content-Type: application/json' \
  --data "$rich_profile_payload" |
  node -e '
    const value = JSON.parse(require("fs").readFileSync(0, "utf8"));
    if (value.professionDetails?.length !== 2 || value.serviceAreas?.length !== 2 || value.capabilities?.edit !== true) throw new Error("idempotent profile retry returned a divergent partial DTO");
    process.stdout.write(value.id);
  ')
test "$rich_retry_id" = "$rich_profile_id"

test "$(psql_exec -Atc "SELECT count(*) FROM directory_profile_profession WHERE profile_id='$rich_profile_id';")" = "2"
test "$(psql_exec -Atc "SELECT count(*) FROM directory_profile_location WHERE profile_id='$rich_profile_id';")" = "2"
test "$(psql_exec -Atc "SELECT count(*) FROM directory_profile_language WHERE profile_id='$rich_profile_id';")" = "1"

# A legacy/basic PUT must preserve omitted rich fields, structured role details,
# language proficiency and the additional service area.
legacy_profile_payload=$(printf '{"profileKind":"person","publicName":"Synthetic rich producer","slug":"synthetic-rich-producer","bio":"Synthetic professional profile used only by the isolated directory API test.","professionIds":["%s","%s"],"instrumentIds":["%s"],"genreIds":["%s"],"serviceOfferingIds":["%s"],"countryId":"%s","cityId":"%s","onsite":true,"remote":true,"availableToTravel":true,"travelRadiusKm":500}' "$rich_profession_one" "$rich_profession_two" "$rich_instrument_id" "$rich_genre_id" "$rich_service_id" "$rich_country_id" "$rich_city_id")
curl -fsS -X PUT "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/$rich_profile_id" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Content-Type: application/json' \
  --data "$legacy_profile_payload" |
  node -e '
    const value = JSON.parse(require("fs").readFileSync(0, "utf8"));
    if (!value.experienceSummary || value.portfolio?.length !== 1 || value.links?.length !== 1 || value.rates?.minMinor !== 10000) throw new Error("legacy update erased omitted rich profile fields");
    if (value.professionDetails?.[0]?.headline !== "Synthetic music producer" || value.instrumentDetails?.[0]?.proficiency !== "professional") throw new Error("legacy update erased structured membership details");
    if (value.languages?.length !== 1 || value.serviceAreas?.length !== 2) throw new Error("legacy update erased languages or secondary service areas");
  '

curl -fsS -X PATCH "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/$rich_profile_id/status" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Content-Type: application/json' \
  --data '{"status":"published"}' >/dev/null

curl -fsS "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/synthetic-rich-producer" |
  node -e '
    const value = JSON.parse(require("fs").readFileSync(0, "utf8"));
    if (value.professions?.length !== 2 || value.services?.length !== 1 || value.languages?.[0]?.proficiency !== "native") throw new Error("public rich taxonomy projection is incomplete");
    if (value.locations?.length !== 2 || value.portfolio?.[0]?.itemType !== "credit" || value.portfolio?.[0]?.url !== "https://example.test/credits/synthetic" || value.links?.[0]?.label !== "Synthetic portfolio" || value.availability?.status !== "available") throw new Error("public locations, normalized media or availability are incomplete");
    for (const forbidden of ["exactAddress","privateLatitude","privateLongitude","primaryEmail","primaryPhone","partyId","capabilities","professionIds","serviceAreas"]) {
      if (forbidden in value || JSON.stringify(value).includes(String.fromCharCode(34) + forbidden + String.fromCharCode(34))) throw new Error("public rich profile exposed " + forbidden);
    }
  '

duplicate_profession_payload=$(printf '{"profileKind":"person","publicName":"Synthetic rich producer","slug":"synthetic-rich-producer","professionIds":["%s","%s"],"instrumentIds":[],"genreIds":[],"serviceOfferingIds":[],"countryId":"%s","cityId":"%s","onsite":true,"remote":false,"availableToTravel":false}' "$rich_profession_one" "$rich_profession_one" "$rich_country_id" "$rich_city_id")
duplicate_profession_status=$(curl -sS -o /dev/null -w '%{http_code}' -X PUT "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/$rich_profile_id" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' -H 'Content-Type: application/json' \
  --data "$duplicate_profession_payload")
test "$duplicate_profession_status" = "400"

unsafe_link_payload=$(printf '{"profileKind":"person","publicName":"Synthetic rich producer","slug":"synthetic-rich-producer","links":[{"label":"Unsafe","url":"https://user:secret@example.test"}],"professionIds":["%s","%s"],"instrumentIds":["%s"],"genreIds":["%s"],"serviceOfferingIds":["%s"],"countryId":"%s","cityId":"%s","onsite":true,"remote":true,"availableToTravel":true}' "$rich_profession_one" "$rich_profession_two" "$rich_instrument_id" "$rich_genre_id" "$rich_service_id" "$rich_country_id" "$rich_city_id")
unsafe_link_status=$(curl -sS -o /dev/null -w '%{http_code}' -X PUT "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/$rich_profile_id" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' -H 'Content-Type: application/json' \
  --data "$unsafe_link_payload")
test "$unsafe_link_status" = "400"

duplicate_primary_payload=$(printf '{"profileKind":"person","publicName":"Synthetic rich producer","slug":"synthetic-rich-producer","professionIds":["%s","%s"],"instrumentIds":["%s"],"genreIds":["%s"],"serviceOfferingIds":["%s"],"serviceAreas":[{"countryId":"%s","cityId":"%s","primaryLocation":true,"onsite":true},{"countryId":"%s","primaryLocation":true,"onsite":true}],"countryId":"%s","cityId":"%s","onsite":true,"remote":true,"availableToTravel":true}' "$rich_profession_one" "$rich_profession_two" "$rich_instrument_id" "$rich_genre_id" "$rich_service_id" "$rich_country_id" "$rich_city_id" "$rich_country_id" "$rich_country_id" "$rich_city_id")
duplicate_primary_status=$(curl -sS -o /dev/null -w '%{http_code}' -X PUT "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/$rich_profile_id" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' -H 'Content-Type: application/json' \
  --data "$duplicate_primary_payload")
test "$duplicate_primary_status" = "400"

mismatched_geography_payload=$(printf '{"profileKind":"person","publicName":"Synthetic rich producer","slug":"synthetic-rich-producer","professionIds":["%s","%s"],"instrumentIds":["%s"],"genreIds":["%s"],"serviceOfferingIds":["%s"],"serviceAreas":[{"countryId":"%s","cityId":"%s","primaryLocation":true,"onsite":true}],"countryId":"%s","cityId":"%s","onsite":true,"remote":true,"availableToTravel":true}' "$rich_profession_one" "$rich_profession_two" "$rich_instrument_id" "$rich_genre_id" "$rich_service_id" "$rich_other_country_id" "$rich_city_id" "$rich_other_country_id" "$rich_city_id")
mismatched_geography_status=$(curl -sS -o /dev/null -w '%{http_code}' -X PUT "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/$rich_profile_id" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' -H 'Content-Type: application/json' \
  --data "$mismatched_geography_payload")
test "$mismatched_geography_status" = "400"

# Explicit empty collections and clearRates are destructive only by intent.
clear_profile_payload=$(printf '{"profileKind":"person","publicName":"Synthetic rich producer","slug":"synthetic-rich-producer","bio":"Synthetic professional profile used only by the isolated directory API test.","experienceSummary":"","creditsSummary":"","portfolio":[],"links":[],"equipmentSummary":"","clearRates":true,"professionIds":["%s","%s"],"instrumentIds":["%s"],"genreIds":["%s"],"serviceOfferingIds":["%s"],"languages":[],"serviceAreas":[{"countryId":"%s","cityId":"%s","serviceRadiusKm":35,"primaryLocation":true,"onsite":true},{"countryId":"%s","serviceRadiusKm":500,"primaryLocation":false,"onsite":true}],"countryId":"%s","cityId":"%s","onsite":true,"remote":true,"availableToTravel":true,"travelRadiusKm":500}' "$rich_profession_one" "$rich_profession_two" "$rich_instrument_id" "$rich_genre_id" "$rich_service_id" "$rich_country_id" "$rich_city_id" "$rich_country_id" "$rich_country_id" "$rich_city_id")
curl -fsS -X PUT "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/$rich_profile_id" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' -H 'Content-Type: application/json' \
  --data "$clear_profile_payload" |
  node -e '
    const value = JSON.parse(require("fs").readFileSync(0, "utf8"));
    if (value.experienceSummary !== null || value.creditsSummary !== null || value.equipmentSummary !== null || value.rates !== null) throw new Error("explicit rich scalar clearing failed");
    if (value.portfolio?.length !== 0 || value.links?.length !== 0 || value.languages?.length !== 0) throw new Error("explicit rich collection clearing failed");
  '
test "$(psql_exec -Atc "SELECT count(*) FROM directory_audit_event WHERE entity_kind='profile' AND entity_id='$rich_profile_id' AND action='profile.updated';")" = "2"
test "$(psql_exec -Atc "SELECT count(*) FROM directory_audit_event WHERE entity_kind='profile' AND entity_id='$rich_profile_id' AND action='profile.updated' AND metadata ?| ARRAY['bio','experienceSummary','creditsSummary','portfolio','links','equipmentSummary'];")" = "0"

invitation_id=$(curl -fsS -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/invitations" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-invitation-create-1' \
  -H 'Content-Type: application/json' \
  --data '{"senderProfileId":"d2000000-0000-4000-8000-000000000001","targetProfileId":"d2000000-0000-4000-8000-000000000002","classifiedId":"d2000000-0000-4000-8000-000000000004","message":"Synthetic invitation used only for isolated runtime authorization testing."}' |
  node -e '
    let raw = "";
    process.stdin.on("data", (chunk) => { raw += chunk; });
    process.stdin.on("end", () => {
      const value = JSON.parse(raw);
      if (!/^[0-9a-f-]{36}$/.test(value.id ?? "")) throw new Error("invitation id is missing");
      if (value.senderProfile?.name !== "Synthetic invitation sender" || value.targetProfile?.name !== "Synthetic invitation target") throw new Error("participant-safe invitation labels are missing");
      if ("email" in value || "phone" in value || "partyId" in value) throw new Error("invitation response exposed participant PII");
      process.stdout.write(value.id);
    });
  ')

preaccept_status=$(curl -sS -o /dev/null -w '%{http_code}' -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/contact" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-contact-before-accept' \
  -H 'Content-Type: application/json' \
  --data "{\"senderProfileId\":\"d2000000-0000-4000-8000-000000000001\",\"targetProfileId\":\"d2000000-0000-4000-8000-000000000002\",\"contextKind\":\"invitation\",\"contextId\":\"$invitation_id\",\"message\":\"Synthetic pre-acceptance contact must be rejected.\"}")
test "$preaccept_status" = "404"

preaccept_transition_status=$(curl -sS -o /dev/null -w '%{http_code}' -X PATCH "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/invitations/$invitation_id/status" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Content-Type: application/json' \
  --data '{"status":"conversation_open"}')
test "$preaccept_transition_status" = "409"

curl -fsS -X PATCH "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/invitations/$invitation_id/status" \
  -H 'Authorization: Bearer synthetic-directory-target-token' \
  -H 'Content-Type: application/json' \
  --data '{"status":"accepted"}' >/dev/null

psql_exec -c "INSERT INTO directory_contact_preference(profile_id,allow_profile_contacts) VALUES ('d2000000-0000-4000-8000-000000000002',FALSE) ON CONFLICT(profile_id) DO UPDATE SET allow_profile_contacts=FALSE;" >/dev/null
cold_contact_status=$(curl -sS -o /dev/null -w '%{http_code}' -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/contact" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-cold-contact-disabled' \
  -H 'Content-Type: application/json' \
  --data '{"senderProfileId":"d2000000-0000-4000-8000-000000000001","targetProfileId":"d2000000-0000-4000-8000-000000000002","contextKind":"profile","contextId":"d2000000-0000-4000-8000-000000000002","message":"Synthetic cold contact must respect the disabled general-contact preference."}')
test "$cold_contact_status" = "403"

mismatched_status=$(curl -sS -o /dev/null -w '%{http_code}' -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/contact" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-contact-wrong-target' \
  -H 'Content-Type: application/json' \
  --data "{\"senderProfileId\":\"d2000000-0000-4000-8000-000000000001\",\"targetProfileId\":\"d2000000-0000-4000-8000-000000000003\",\"contextKind\":\"invitation\",\"contextId\":\"$invitation_id\",\"message\":\"Synthetic mismatched target contact must be rejected.\"}")
test "$mismatched_status" = "404"

curl -fsS -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/contact" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-contact-after-accept' \
  -H 'Content-Type: application/json' \
  --data "{\"senderProfileId\":\"d2000000-0000-4000-8000-000000000001\",\"targetProfileId\":\"d2000000-0000-4000-8000-000000000002\",\"contextKind\":\"invitation\",\"contextId\":\"$invitation_id\",\"message\":\"Synthetic accepted contact remains participant scoped.\"}" >/dev/null

curl -fsS "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/review-eligibility?authorProfileId=d2000000-0000-4000-8000-000000000001" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' |
  node -e '
    let raw = "";
    process.stdin.on("data", (chunk) => { raw += chunk; });
    process.stdin.on("end", () => {
      const value = JSON.parse(raw);
      if (value.length !== 1 || value[0].interactionId !== "d2000000-0000-4000-8000-000000000006") throw new Error("verified review eligibility is missing");
      if (value[0].authorProfile?.id !== "d2000000-0000-4000-8000-000000000001" || value[0].subjectProfile?.id !== "d2000000-0000-4000-8000-000000000002") throw new Error("review direction is not participant scoped");
      if ("externalId" in value[0] || "partyId" in value[0]) throw new Error("review eligibility exposed an external identifier or Party id");
    });
  '

unauthorized_review_status=$(curl -sS -o /dev/null -w '%{http_code}' -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/reviews" \
  -H 'Authorization: Bearer synthetic-directory-target-token' \
  -H 'Idempotency-Key: synthetic-review-wrong-manager' \
  -H 'Content-Type: application/json' \
  --data '{"interactionId":"d2000000-0000-4000-8000-000000000006","authorProfileId":"d2000000-0000-4000-8000-000000000001","subjectProfileId":"d2000000-0000-4000-8000-000000000002","rating":5,"body":"Synthetic unauthorized review must be rejected."}')
test "$unauthorized_review_status" = "404"

review_id=$(curl -fsS -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/reviews" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-review-create-1' \
  -H 'Content-Type: application/json' \
  --data '{"interactionId":"d2000000-0000-4000-8000-000000000006","authorProfileId":"d2000000-0000-4000-8000-000000000001","subjectProfileId":"d2000000-0000-4000-8000-000000000002","rating":5,"body":"Synthetic verified runtime review for authorization testing."}' |
  node -e '
    let raw = "";
    process.stdin.on("data", (chunk) => { raw += chunk; });
    process.stdin.on("end", () => {
      const value = JSON.parse(raw);
      if (!/^[0-9a-f-]{36}$/.test(value.id ?? "") || value.status !== "published") throw new Error("review response is incomplete");
      if (value.authorProfile?.name !== "Synthetic invitation sender" || value.subjectProfile?.name !== "Synthetic invitation target") throw new Error("review response lacks safe participant labels");
      if ("externalId" in value || "partyId" in value) throw new Error("review response exposed an external identifier or Party id");
      process.stdout.write(value.id);
    });
  ')

retry_review_id=$(curl -fsS -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/reviews" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-review-create-1' \
  -H 'Content-Type: application/json' \
  --data '{"interactionId":"d2000000-0000-4000-8000-000000000006","authorProfileId":"d2000000-0000-4000-8000-000000000001","subjectProfileId":"d2000000-0000-4000-8000-000000000002","rating":5,"body":"Synthetic verified runtime review for authorization testing."}' |
  node -pe 'JSON.parse(require("fs").readFileSync(0,"utf8")).id')
test "$retry_review_id" = "$review_id"

duplicate_review_status=$(curl -sS -o /dev/null -w '%{http_code}' -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/reviews" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-review-create-2' \
  -H 'Content-Type: application/json' \
  --data '{"interactionId":"d2000000-0000-4000-8000-000000000006","authorProfileId":"d2000000-0000-4000-8000-000000000001","subjectProfileId":"d2000000-0000-4000-8000-000000000002","rating":4,"body":"Synthetic duplicate review must not create another record."}')
test "$duplicate_review_status" = "409"

curl -fsS "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/synthetic-invitation-target/reviews?limit=20" |
  node -e '
    let raw = "";
    process.stdin.on("data", (chunk) => { raw += chunk; });
    process.stdin.on("end", () => {
      const value = JSON.parse(raw);
      if (value.summary?.count !== 1 || Number(value.summary?.average) !== 5 || value.items?.length !== 1) throw new Error("public review aggregate is not derived from the verified review");
      if (value.items[0].authorProfile?.name !== "Synthetic invitation sender") throw new Error("public review author projection is missing");
      if ("interactionId" in value.items[0] || "externalId" in value.items[0] || "partyId" in value.items[0]) throw new Error("public review exposed a private interaction or Party identifier");
    });
  '

curl -fsS "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/synthetic-invitation-target" |
  node -e '
    let raw = "";
    process.stdin.on("data", (chunk) => { raw += chunk; });
    process.stdin.on("end", () => {
      const value = JSON.parse(raw);
      if (value.reputation?.reviewCount !== 1 || Number(value.reputation?.reviewAverage) !== 5 || value.reputation?.completed !== 1) throw new Error("profile reputation aggregates are stale");
    });
  '

psql_exec -c "UPDATE directory_interaction SET status='cancelled' WHERE id='d2000000-0000-4000-8000-000000000006';" >/dev/null
cancelled_review_count=$(psql_exec -Atc "SELECT review_count FROM directory_profile WHERE id='d2000000-0000-4000-8000-000000000002';")
test "$cancelled_review_count" = "0"
curl -fsS "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/profiles/synthetic-invitation-target/reviews?limit=20" |
  node -e '
    let raw = "";
    process.stdin.on("data", (chunk) => { raw += chunk; });
    process.stdin.on("end", () => {
      const value = JSON.parse(raw);
      if (value.summary?.count !== 0 || value.items?.length !== 0) throw new Error("cancelled interaction remained in public reputation");
    });
  '
psql_exec -c "UPDATE directory_interaction SET status='completed' WHERE id='d2000000-0000-4000-8000-000000000006';" >/dev/null
restored_review_count=$(psql_exec -Atc "SELECT review_count FROM directory_profile WHERE id='d2000000-0000-4000-8000-000000000002';")
test "$restored_review_count" = "1"

psql_exec -c "UPDATE directory_profile SET profile_status='paused' WHERE id='d2000000-0000-4000-8000-000000000001';" >/dev/null
paused_author_review_count=$(psql_exec -Atc "SELECT review_count FROM directory_profile WHERE id='d2000000-0000-4000-8000-000000000002';")
test "$paused_author_review_count" = "0"
psql_exec -c "UPDATE directory_profile SET profile_status='published' WHERE id='d2000000-0000-4000-8000-000000000001';" >/dev/null
republished_author_review_count=$(psql_exec -Atc "SELECT review_count FROM directory_profile WHERE id='d2000000-0000-4000-8000-000000000002';")
test "$republished_author_review_count" = "1"

curl -fsS -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/reports" \
  -H 'Authorization: Bearer synthetic-directory-target-token' \
  -H 'Idempotency-Key: synthetic-review-report-1' \
  -H 'Content-Type: application/json' \
  --data "{\"targetKind\":\"review\",\"targetId\":\"$review_id\",\"reasonCode\":\"community-report\"}" >/dev/null
psql_exec -c "UPDATE directory_review SET status='hidden' WHERE id='$review_id';" >/dev/null
hidden_review_count=$(psql_exec -Atc "SELECT review_count FROM directory_profile WHERE id='d2000000-0000-4000-8000-000000000002';")
test "$hidden_review_count" = "0"
psql_exec -c "UPDATE directory_review SET status='published' WHERE id='$review_id';" >/dev/null
restored_after_moderation_count=$(psql_exec -Atc "SELECT review_count FROM directory_profile WHERE id='d2000000-0000-4000-8000-000000000002';")
test "$restored_after_moderation_count" = "1"

curl -fsS -X PATCH "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/invitations/$invitation_id/status" \
  -H 'Authorization: Bearer synthetic-directory-target-token' \
  -H 'Content-Type: application/json' \
  --data '{"status":"blocked"}' >/dev/null
block_count=$(psql_exec -Atc "SELECT count(*) FROM directory_profile_block WHERE blocker_profile_id='d2000000-0000-4000-8000-000000000002' AND blocked_profile_id='d2000000-0000-4000-8000-000000000001';")
test "$block_count" = "1"

blocked_contact_status=$(curl -sS -o /dev/null -w '%{http_code}' -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/contact" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-contact-after-block' \
  -H 'Content-Type: application/json' \
  --data '{"senderProfileId":"d2000000-0000-4000-8000-000000000001","targetProfileId":"d2000000-0000-4000-8000-000000000002","contextKind":"profile","contextId":"d2000000-0000-4000-8000-000000000002","message":"Synthetic direct contact after a reverse block must be rejected."}')
test "$blocked_contact_status" = "403"

blocked_application_status=$(curl -sS -o /dev/null -w '%{http_code}' -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/classifieds/d2000000-0000-4000-8000-000000000004/applications" \
  -H 'Authorization: Bearer synthetic-directory-target-token' \
  -H 'Idempotency-Key: synthetic-application-after-block' \
  -H 'Content-Type: application/json' \
  --data '{"applicantProfileId":"d2000000-0000-4000-8000-000000000002","message":"Synthetic application after a reverse block must be rejected.","portfolio":[]}')
test "$blocked_application_status" = "403"

blocked_reinvite_status=$(curl -sS -o /dev/null -w '%{http_code}' -X POST "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/invitations" \
  -H 'Authorization: Bearer synthetic-directory-sender-token' \
  -H 'Idempotency-Key: synthetic-invitation-create-2' \
  -H 'Content-Type: application/json' \
  --data '{"senderProfileId":"d2000000-0000-4000-8000-000000000001","targetProfileId":"d2000000-0000-4000-8000-000000000002","message":"Synthetic reinvitation after blocking must be rejected."}')
test "$blocked_reinvite_status" = "403"

expired_status=$(curl -sS -o /dev/null -w '%{http_code}' -X PATCH "http://127.0.0.1:$TDF_DIRECTORY_API_PORT/directory/invitations/d2000000-0000-4000-8000-000000000005/status" \
  -H 'Authorization: Bearer synthetic-directory-target-token' \
  -H 'Content-Type: application/json' \
  --data '{"status":"accepted"}')
test "$expired_status" = "409"
expired_state=$(psql_exec -Atc "SELECT status FROM directory_invitation WHERE id='d2000000-0000-4000-8000-000000000005';")
test "$expired_state" = "expired"
stop_api

echo "Music directory migration passed restart, backfill, rollback/reapply, rich profile compatibility, privacy, claim, verified-review API and aggregation, alert, merge, search-volume, taxonomy, invitation-participant, blocking, expiry, and invariant checks."
