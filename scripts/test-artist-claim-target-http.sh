#!/usr/bin/env bash
set -euo pipefail
TDF_CLAIM_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TDF_CLAIM_DB="${TDF_CLAIM_TEST_DATABASE_URL:?isolated fully migrated test database required}"
TDF_CLAIM_BIN="${TDF_CLAIM_SERVER_BIN:?tested backend binary required}"
TDF_CLAIM_HTTP_PORT="${TDF_CLAIM_SERVER_PORT:-18629}"
case "$TDF_CLAIM_DB" in
  postgresql://*@127.0.0.1:*/*|postgresql://*@localhost:*/*) ;;
  postgresql://*@postgres:*/*) test "${CI:-}" = true ;;
  *) echo 'Refusing a non-isolated database' >&2; exit 1 ;;
esac
claim_psql() { psql "$TDF_CLAIM_DB" -X -q -v ON_ERROR_STOP=1 "$@"; }
case "$(claim_psql -Atc 'SELECT current_database()')" in *_test) ;; *) exit 1 ;; esac
test "$(claim_psql -Atc "SELECT count(*) FROM party WHERE display_name LIKE 'Claim target % fixture'")" = 0
if curl -fsS "http://127.0.0.1:$TDF_CLAIM_HTTP_PORT/health" >/dev/null 2>&1; then
  echo 'Refusing occupied test port' >&2; exit 1
fi
TDF_CLAIM_RUNTIME=$(mktemp -d "${TMPDIR:-/tmp}/tdf-artist-claim.XXXXXX")
mkdir -p "$TDF_CLAIM_RUNTIME/assets"
env -i PATH="$PATH" TMPDIR="$TDF_CLAIM_RUNTIME" APP_ENV=test \
 DATABASE_URL="$TDF_CLAIM_DB" APP_PORT="$TDF_CLAIM_HTTP_PORT" \
 RESET_DB=false RUN_MIGRATIONS=false SEED_DB=false DEFAULT_LOCALE=es \
 HQ_ASSETS_DIR="$TDF_CLAIM_RUNTIME/assets" EVENT_DISCOVERY_ENABLED=false \
 ARTIST_ENRICHMENT_ENABLED=false EVENT_LOGISTICS_RECHECK_ENABLED=false \
 "$TDF_CLAIM_BIN" > "$TDF_CLAIM_RUNTIME/backend.log" 2>&1 &
TDF_CLAIM_PID=$!
trap 'kill "$TDF_CLAIM_PID" 2>/dev/null || true; wait "$TDF_CLAIM_PID" 2>/dev/null || true' EXIT
for attempt in $(seq 1 90); do
 if curl -fsS "http://127.0.0.1:$TDF_CLAIM_HTTP_PORT/health" 2>/dev/null | grep -q '"status":"ok"'; then break; fi
 if ! kill -0 "$TDF_CLAIM_PID" 2>/dev/null || [ "$attempt" = 90 ]; then
   tail -50 "$TDF_CLAIM_RUNTIME/backend.log"; exit 1
 fi
 sleep 1
done
# Artists inserted after the one-time backfill must still accept reviewed claims.
claim_psql <<'SQL' >/dev/null
INSERT INTO party(display_name,is_org,created_at) VALUES ('Claim target account fixture',false,now());
INSERT INTO api_token(token,party_id,label,active) SELECT 'synthetic-claim-target-token',id,'isolated claim runtime',true FROM party WHERE display_name='Claim target account fixture';
INSERT INTO party(display_name,is_org,created_at) VALUES
 ('Claim target fresh fixture',false,now()),('Claim target draft fixture',false,now()),('Claim target blocked fixture',false,now()),('Claim target canonical band fixture',false,now());
INSERT INTO artist_profile(artist_party_id,slug,created_at)
 SELECT id,'claim-target-'||id,now() FROM party WHERE display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture','Claim target canonical band fixture');
INSERT INTO directory_profile(subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status,bio)
 SELECT id,'artist','PRIVATE DIRECTORY NAME','claim-twin-'||id,'draft','private',
 CASE WHEN display_name='Claim target blocked fixture' THEN 'blocked' ELSE 'allowed' END,'PRIVATE BIO'
 FROM party WHERE display_name IN ('Claim target draft fixture','Claim target blocked fixture','Claim target canonical band fixture');
-- A newer band owned by the same Party must never win artist claim selection.
INSERT INTO directory_profile(subject_party_id,profile_kind,public_name,slug,profile_status,visibility,updated_at)
 SELECT id,'band','PRIVATE BAND','claim-band-'||id,'draft','private',now()+interval '1 minute'
 FROM party WHERE display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture','Claim target canonical band fixture');
UPDATE directory_profile artist SET canonical_profile_id=band.id
 FROM directory_profile band JOIN party p ON p.id=band.subject_party_id
 WHERE p.display_name='Claim target canonical band fixture' AND band.profile_kind='band'
 AND artist.subject_party_id=p.id AND artist.profile_kind='artist';
SQL
claim_psql <<'SQL' >/dev/null
-- A Party may own a person profile alongside a distinct artist or band profile.
INSERT INTO party(display_name,is_org,created_at) VALUES
 ('Claim target person-only fixture',false,now()),('Claim target mixed fixture',false,now()),
 ('Claim target band fixture',false,now()),('Claim target wrong-canonical fixture',false,now());
INSERT INTO artist_profile(artist_party_id,slug,created_at)
 SELECT id,'claim-kind-'||id,now() FROM party WHERE display_name IN
 ('Claim target person-only fixture','Claim target mixed fixture','Claim target band fixture','Claim target wrong-canonical fixture');
INSERT INTO directory_profile(subject_party_id,profile_kind,public_name,slug,profile_status,visibility)
 SELECT id,CASE WHEN display_name='Claim target band fixture' THEN 'band' ELSE 'artist' END,
 'PRIVATE ARTIST NAME','claim-kind-artist-'||id,'draft','private'
 FROM party WHERE display_name IN ('Claim target mixed fixture','Claim target band fixture','Claim target wrong-canonical fixture');
INSERT INTO directory_profile(subject_party_id,profile_kind,public_name,slug,profile_status,visibility,updated_at)
 SELECT id,'person','PRIVATE PERSON NAME','claim-kind-person-'||id,'draft','private',now()+interval '1 minute'
 FROM party WHERE display_name IN ('Claim target person-only fixture','Claim target mixed fixture','Claim target wrong-canonical fixture');
UPDATE directory_profile source SET canonical_profile_id=target.id
 FROM directory_profile target WHERE source.subject_party_id=target.subject_party_id
 AND source.slug LIKE 'claim-kind-artist-%' AND target.slug LIKE 'claim-kind-person-%'
 AND source.subject_party_id=(SELECT id FROM party WHERE display_name='Claim target wrong-canonical fixture');
SQL
TDF_CLAIM_TARGET_EXPECTED=$(claim_psql -Atc "SELECT json_object_agg(p.display_name,d.id) FROM directory_profile d JOIN party p ON p.id=d.subject_party_id WHERE p.display_name='Claim target mixed fixture' AND d.profile_kind='artist';") \
TDF_CLAIM_TARGET_FORBIDDEN=$(claim_psql -Atc "SELECT coalesce(json_agg(id),'[]') FROM directory_profile WHERE profile_kind<>'artist' OR canonical_profile_id IS NOT NULL;") \
TDF_CLAIM_TARGET_IDS=$(claim_psql -Atc "SELECT json_object_agg(display_name,id) FROM party WHERE display_name LIKE 'Claim target % fixture';") \
TDF_CLAIM_TARGET_BASE="http://127.0.0.1:$TDF_CLAIM_HTTP_PORT" \
 node "$TDF_CLAIM_ROOT/scripts/__tests__/artist-claim-target-runtime.mjs"
claim_psql <<'SQL' >/dev/null
DO $$ BEGIN
 ASSERT (SELECT count(*)=8 FROM directory_profile d JOIN party p ON p.id=d.subject_party_id WHERE p.display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture','Claim target canonical band fixture')), 'duplicate or missing directory twins';
 ASSERT NOT EXISTS(SELECT 1 FROM directory_profile d JOIN party p ON p.id=d.subject_party_id WHERE p.display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture','Claim target canonical band fixture') AND (d.profile_status<>'draft' OR d.visibility<>'private')), 'preparation published a profile';
 ASSERT NOT EXISTS(SELECT 1 FROM directory_profile_manager m JOIN directory_profile d ON d.id=m.profile_id JOIN party p ON p.id=d.subject_party_id WHERE p.display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture','Claim target canonical band fixture')), 'preparation granted management';
 ASSERT NOT EXISTS(SELECT 1 FROM user_credential c JOIN party p ON p.id=c.party_id WHERE p.display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture','Claim target canonical band fixture')), 'preparation created artist credentials';
 ASSERT (SELECT count(*)=1 FROM directory_claim c JOIN directory_profile d ON d.id=c.profile_id JOIN party p ON p.id=d.subject_party_id WHERE p.display_name='Claim target fresh fixture' AND c.status='submitted'), 'claim was duplicated or approved';
 ASSERT (SELECT count(*)=3 FROM directory_profile d JOIN party p ON p.id=d.subject_party_id WHERE p.display_name IN ('Claim target person-only fixture','Claim target mixed fixture','Claim target band fixture','Claim target wrong-canonical fixture') AND d.profile_kind='artist' AND d.canonical_profile_id IS NULL AND d.visibility='private' AND d.profile_status='draft'), 'wrong profile kind, canonical target or visibility';
 ASSERT NOT EXISTS(SELECT 1 FROM directory_profile_manager m JOIN directory_profile d ON d.id=m.profile_id WHERE d.slug LIKE 'claim-kind-%'), 'profile preparation changed ownership';
 ASSERT NOT EXISTS(SELECT 1 FROM directory_claim c JOIN directory_profile d ON d.id=c.profile_id JOIN party p ON p.id=d.subject_party_id WHERE p.display_name LIKE 'Claim target % fixture' AND d.profile_kind<>'artist'), 'claim targeted non-artist resource';
END $$;
SQL

echo "Artist claim preparation HTTP/database checks passed; logs: $TDF_CLAIM_RUNTIME"
