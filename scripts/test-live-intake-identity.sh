#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_database="tdf_live_intake_identity_test_$$"
export PGHOST="${PGHOST:-127.0.0.1}"
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
createdb "$test_database"
trap cleanup EXIT INT TERM
psql -X -v ON_ERROR_STOP=1 -d "$test_database" <<'SQL'
CREATE EXTENSION pgcrypto;
CREATE TABLE party(id bigserial PRIMARY KEY,legal_name text,display_name text NOT NULL,is_org boolean NOT NULL,tax_id text,primary_email text,primary_phone text,whatsapp text,instagram text,emergency_contact text,notes text,stripe_customer_id text,country_code text,country_id uuid,created_at timestamptz NOT NULL);
CREATE TABLE user_credential(id bigserial PRIMARY KEY,party_id bigint REFERENCES party(id));
CREATE TABLE party_role(id bigserial PRIMARY KEY,party_id bigint REFERENCES party(id));
CREATE TABLE identity_party_archive(party_id bigint PRIMARY KEY REFERENCES party(id));
CREATE TABLE live_session_intake(id uuid PRIMARY KEY DEFAULT gen_random_uuid(),band_name text NOT NULL,band_description text,primary_genre text,primary_genre_id uuid,input_list text,contact_email text,contact_phone text,session_date date,availability text,accepted_terms boolean NOT NULL,terms_version text,rider_path text,created_by bigint REFERENCES party(id),created_at timestamptz NOT NULL);
CREATE TABLE live_session_musician(id uuid PRIMARY KEY DEFAULT gen_random_uuid(),intake_id uuid NOT NULL REFERENCES live_session_intake(id),party_id bigint NOT NULL REFERENCES party(id),name text NOT NULL,email text,instrument text,instrument_id uuid,role text,notes text,is_existing boolean NOT NULL);
CREATE TABLE live_session_song(id uuid PRIMARY KEY DEFAULT gen_random_uuid(),intake_id uuid NOT NULL REFERENCES live_session_intake(id),title text NOT NULL,bpm bigint,song_key text,lyrics text,sort_order bigint NOT NULL);
SQL
# Exercise the actual deployed archive guard rather than a copied test function.
sed -n '/^CREATE OR REPLACE FUNCTION identity_reject_archived_reference()/,/^END \$\$;/p' "$repo_root/tdf-hq/sql/2026-09-17_identity_reconciliation.sql" | psql -X -v ON_ERROR_STOP=1 -d "$test_database"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" <<'SQL'
CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON live_session_musician
FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference('party_id');
CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON live_session_intake
FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference('created_by');
SQL
migration="$repo_root/tdf-hq/sql/2026-09-18_live_intake_idempotency.sql"
rollback="$repo_root/tdf-hq/sql/2026-09-18_live_intake_idempotency_rollback.sql"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$migration"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$migration"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$rollback"
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$migration"
export TDF_LIVE_INTAKE_TEST_DB="host=$PGHOST dbname=$test_database"
cd "$repo_root/tdf-hq"
stack test tdf-hq:test:tdf-hq-test --fast --ghc-options=-O0 --jobs 1 --test-arguments='--match live-intake-identity-postgresql'
# Populate a receipt to verify that schema rollback cannot destroy replay protection.
psql -X -v ON_ERROR_STOP=1 -d "$test_database" <<'SQL'
WITH intake AS (INSERT INTO live_session_intake(band_name,accepted_terms,created_by,created_at) VALUES ('rollback guard',true,1,now()) RETURNING id)
INSERT INTO identity_live_intake_request(actor_party_id,request_key,request_payload,intake_id) SELECT 1,'rollback-test-request','{}'::jsonb,id FROM intake;
SQL
if psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$rollback" >/dev/null 2>&1; then
  echo 'Rollback incorrectly removed accepted intake receipts' >&2
  exit 1
fi
