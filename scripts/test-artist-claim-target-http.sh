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
 ('Claim target fresh fixture',false,now()),('Claim target draft fixture',false,now()),('Claim target blocked fixture',false,now());
INSERT INTO artist_profile(artist_party_id,slug,created_at)
 SELECT id,'claim-target-'||id,now() FROM party WHERE display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture');
INSERT INTO directory_profile(subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status,bio)
 SELECT id,'artist','PRIVATE DIRECTORY NAME','claim-twin-'||id,'draft','private',
 CASE WHEN display_name='Claim target blocked fixture' THEN 'blocked' ELSE 'allowed' END,'PRIVATE BIO'
 FROM party WHERE display_name IN ('Claim target draft fixture','Claim target blocked fixture');
SQL
TDF_CLAIM_TARGET_IDS=$(claim_psql -Atc "SELECT json_object_agg(display_name,id) FROM party WHERE display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture');") \
TDF_CLAIM_TARGET_BASE="http://127.0.0.1:$TDF_CLAIM_HTTP_PORT" \
 node "$TDF_CLAIM_ROOT/scripts/__tests__/artist-claim-target-runtime.mjs"
claim_psql <<'SQL' >/dev/null
DO $$ BEGIN
 ASSERT (SELECT count(*)=3 FROM directory_profile d JOIN party p ON p.id=d.subject_party_id WHERE p.display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture')), 'duplicate or missing directory twins';
 ASSERT NOT EXISTS(SELECT 1 FROM directory_profile d JOIN party p ON p.id=d.subject_party_id WHERE p.display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture') AND (d.profile_status<>'draft' OR d.visibility<>'private')), 'preparation published a profile';
 ASSERT NOT EXISTS(SELECT 1 FROM directory_profile_manager m JOIN directory_profile d ON d.id=m.profile_id JOIN party p ON p.id=d.subject_party_id WHERE p.display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture')), 'preparation granted management';
 ASSERT NOT EXISTS(SELECT 1 FROM user_credential c JOIN party p ON p.id=c.party_id WHERE p.display_name IN ('Claim target fresh fixture','Claim target draft fixture','Claim target blocked fixture')), 'preparation created artist credentials';
 ASSERT (SELECT count(*)=1 FROM directory_claim c JOIN directory_profile d ON d.id=c.profile_id JOIN party p ON p.id=d.subject_party_id WHERE p.display_name='Claim target fresh fixture' AND c.status='submitted'), 'claim was duplicated or approved';
END $$;
SQL

echo "Artist claim preparation HTTP/database checks passed; logs: $TDF_CLAIM_RUNTIME"
